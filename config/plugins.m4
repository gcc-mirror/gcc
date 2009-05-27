AC_DEFUN([AC_PLUGINS],
[
AC_ARG_ENABLE([plugins],
AS_HELP_STRING([--enable-plugins], [Enable support for plugins (defaults no)]),
[case "${enableval}" in
  yes | "") plugins=yes ;;
  no) plugins=no ;;
  *) plugins=yes ;;
 esac],
[plugins=no])
])
