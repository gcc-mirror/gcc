AC_PREREQ(2.59)

AC_INIT(fixincludes, [ ])
AC_CONFIG_SRCDIR(inclhack.def)
AC_CONFIG_AUX_DIR(..)
AC_CANONICAL_SYSTEM
AC_PROG_CC

# Choose one or two-process fix methodology.  Systems that cannot handle
# bi-directional pipes must use the two process method.
#
case $host in
	i?86-*-msdosdjgpp* | \
	*-*-beos* )
		TARGET=twoprocess
		AC_DEFINE(SEPARATE_FIX_PROC, 1, [Define if testing and fixing are done by separate process])
		;;

	vax-dec-bsd* )
		TARGET=oneprocess
		AC_DEFINE(exit, xexit, [Define to xexit if the host system does not support atexit])
		AC_DEFINE(atexit, xatexit, [Define to xatexit if the host system does not support atexit])
		;;

	* )
		TARGET=oneprocess
		;;
esac
AC_SUBST(TARGET)

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stddef.h stdlib.h strings.h unistd.h fcntl.h sys/file.h \
	sys/stat.h])
AC_CHECK_FUNCS(putc_unlocked fputc_unlocked fputs_unlocked \
        fwrite_unlocked fprintf_unlocked)
AC_CHECK_DECLS([abort, errno, putc_unlocked, fputc_unlocked,
        fputs_unlocked, fwrite_unlocked, fprintf_unlocked])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Checks for library functions.
gcc_AC_FUNC_MMAP_BLACKLIST

AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
AC_MSG_RESULT($USE_MAINTAINER_MODE)
if test "$USE_MAINTAINER_MODE" = yes; then
  MAINT=
else
  MAINT='#'
fi	
AC_SUBST(MAINT)

AC_CONFIG_HEADERS(config.h, [echo timestamp > stamp-h])
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
