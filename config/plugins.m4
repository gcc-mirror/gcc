AC_DEFUN([AC_PLUGINS],
[
  maybe_plugins=no
  AC_CHECK_HEADERS([dlfcn.h], [maybe_plugins=yes], [], [AC_INCLUDES_DEFAULT])
  AC_CHECK_HEADERS([windows.h], [maybe_plugins=yes], [], [AC_INCLUDES_DEFAULT])

  AC_ARG_ENABLE([plugins],
    AS_HELP_STRING([--enable-plugins], [Enable support for plugins]),
    [case "${enableval}" in
      no) plugins=no ;;
      *) plugins=yes
         if test "$maybe_plugins" != "yes" ; then
	   AC_MSG_ERROR([Building with plugin support requires a host that supports dlopen.])
	 fi ;;
     esac],
    [plugins=$maybe_plugins]
  )
  if test "$plugins" = "yes"; then
    AC_SEARCH_LIBS([dlsym], [dl])
  fi
])
