;; 加速配置。
(require 'init-accelerate)

;; 字体设置
(require 'init-font)

(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; 让窗口启动更平滑
  (setq frame-inhibit-implied-resize t)
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  ;; 定义一些启动目录，方便下次迁移修改
  (defvar lazycat-emacs-root-dir (file-truename "~/lazycat-emacs/site-lisp"))
  (defvar lazycat-emacs-config-dir (concat lazycat-emacs-root-dir "/config"))
  (defvar lazycat-emacs-extension-dir (concat lazycat-emacs-root-dir "/extensions"))

;; -------------------------------------------------
;; Linux 下键盘映射：Menu -> Super, Alt -> Meta
;; -------------------------------------------------
(when (eq system-type 'gnu/linux)
  ;; Menu 键作为 Super
  (setq x-super-keysym 'Super_L)
  ;; Alt 键作为 Meta（默认通常就是 Meta，但显式设置）
  (setq x-alt-keysym 'meta))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))


;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))


;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Bootstrap config


(setq custom-file (locate-user-emacs-file "custom.el"))
;; (require 'init-utils)
;; (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
;; (require 'init-elpa)      ;; Machinery for installing required packages
;; (require 'init-exec-path) ;; Set up $PATH


;; General performance tuning
;; (when (require-package 'gcmh)
;;  (setq gcmh-high-cons-threshold (* 128 1024 1024))
;;  (add-hook 'after-init-hook (lambda ()
;;                               (gcmh-mode)
;;                               (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)
   (setq url-proxy-services
       '(("no_proxy" . "^\\(localhost\\|devzhang.cobocn.net\\|127.0.0.1\\)")
         ("http" . "192.168.1.7:7777")        ;; notice without protocol, do NOT add protocol
         ("https" . "192.168.1.7:7777")))

;; Allow users to provide an optional "init-preload-local.el"
;; (require 'init-preload-local nil t)

;; 最大化打开文件数
;;(setq w32-max-handles 2048)

;;自动加载包下的文件
(defun my/load-package-autoloads (pkg-dir)
  "Load autoloads file from PKG-DIR and add it to `load-path'."
  (let* ((abs-dir (expand-file-name pkg-dir))
         (autoload-file (car (directory-files abs-dir t "-autoloads\\.el$"))))
    (when (file-directory-p abs-dir)
      (add-to-list 'load-path abs-dir)
      (when autoload-file
        (load autoload-file)))))

(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/helm-core-20250923.617")
(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/helm-20250923.601")
(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/consult-20250920.1256")
(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/marginalia-20250920.852")
(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/posframe-20250211.110")
(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/pyim-20250225.650")
(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/pyim-basedict-20240923.739")
;(my/load-package-autoloads "~/lazycat-emacs/site-lisp/extensions/fcitx-20240121.1829")
;(fcitx-aggressive-setup)
;(setq fcitx-use-dbus t)

(require 'project)

(customize-set-variable
 'blink-search-common-directory
 `(("HOME" "/mnt/d/xgzhang/projects/svn/Elafs/branches/asp")
))

(setq warning-minimum-level :error)

(require 'orderless)
;; 设置 completion-styles 支持 orderless
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))
(require 'consult)
;; 让 consult-locate 实际调用 consult-fd
(defun my-consult-locate-fd (&optional dir initial)
  "Use `consult-fd' as a replacement for `consult-locate'."
  (interactive "P")
  (consult-fd dir initial))

(advice-add 'consult-locate :override #'my-consult-locate-fd)
  (setq consult-locate-command
        "fd --type f --hidden --exclude .git --exclude .svn --exclude .idea --exclude build --exclude '*.class' --color=never  %s"
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
(require 'vertico)
(define-key vertico-map (kbd "C-j") 'vertico-next)
(define-key vertico-map (kbd "C-k") 'vertico-previous)
(vertico-mode 1)
(marginalia-mode t)
(setq vertico-cycle t)  ;; 支持循环选择候选项
(setq helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t)


;; 启用中文拼音输入法
;;(setq default-input-method "chinese-py-punct")
;; 禁用 Emacs 内置输入法
;;(setq default-input-method nil)
(setq shell-file-name "bash")
(require 'pyim)
(require 'pyim-basedict)
(pyim-basedict-enable)                ;; 启用简体词库

(setq default-input-method "pyim")    ;; 默认输入法设为 pyim
(setq pyim-default-scheme 'quanpin)   ;; 使用全拼
(setq pyim-page-tooltip 'posframe)    ;; 候选词用 posframe 显示
(setq pyim-page-length 7)             ;; 每页显示 7 个候选

;; 标点：中英文自动切换
(setq pyim-punctuation-translate-p '(yes no auto))

  (with-temp-message ""              ;抹掉插件启动的输出
    ;;(require 'benchmark-init-modes)
    ;;(require 'benchmark-init)
    ;;(benchmark-init/activate)

    (require 'init-fullscreen)

    (require 'init-generic)
    (require 'lazycat-theme)
    (lazycat-theme-load-with-sunrise)
    ;; (lazycat-theme-load-dark)
    ;; (lazycat-theme-load-light)
    (when (featurep 'cocoa)
      (require 'cache-path-from-shell))
    (require 'lazy-load)
    (require 'one-key)
    (require 'fingertip)
    (require 'display-line-numbers)
    (require 'basic-toolkit)
    (require 'redo)

    (require 'init-highlight-parentheses)
    (require 'init-awesome-tray)
    (require 'init-line-number)
    (require 'init-lsp-bridge)
    (require 'init-auto-save)
    (require 'init-mode)
    (require 'init-fingertip)
    (require 'init-indent)
    (require 'init-one-key)
    (require 'init-key)
    (require 'init-vi-navigate)
    (require 'init-isearch-mb)
    (require 'init-performance)
    (require 'init-rime)
    (require 'init-treesit)
    (require 'init-key-echo)
    (require 'init-emigo)

    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
         (require 'pretty-lambdada)
         (require 'browse-kill-ring)
         (require 'elf-mode)

         (require 'init-eldoc)
         (require 'init-yasnippet)
         (require 'init-cursor-chg)
         (require 'init-winpoint)
         (require 'init-info)
         (require 'init-c)
         (require 'init-org)
         (require 'init-idle)
         (require 'init-markdown-mode)
         (require 'init-olivetti)
         ;(require 'init-holo-layer)

         (require 'init-eaf)
         (require 'init-popweb)
         (require 'init-eww)

         (require 'trekker)
         (trekker-enable)

         ;; Restore session at last.
         (require 'init-session)
         (emacs-session-restore)

         (require 'init-sort-tab)
         ))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
