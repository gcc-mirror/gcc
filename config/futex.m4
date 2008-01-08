dnl ----------------------------------------------------------------------
dnl This whole bit snagged from libgomp.

dnl
dnl GCC_LINUX_FUTEX
dnl    (SHELL-CODE_HANDLER)
dnl
AC_DEFUN([GCC_LINUX_FUTEX],[dnl
GCC_ENABLE(linux-futex,default, ,[use the Linux futex system call],
	   permit yes|no|default)
case "$target" in
  *-linux*)
    case "$enable_linux_futex" in
      default)
	# If headers don't have gettid/futex syscalls definition, then
	# default to no, otherwise there will be compile time failures.
	# Otherwise, default to yes.  If we don't detect we are
	# compiled/linked against NPTL and not cross-compiling, check
	# if programs are run by default against NPTL and if not, issue
	# a warning.
	enable_linux_futex=no
	AC_LINK_IFELSE(
	 [AC_LANG_PROGRAM(
	  [#include <sys/syscall.h>
	   int lk;],
	  [syscall (SYS_gettid); syscall (SYS_futex, &lk, 0, 0, 0);])],
	  [save_LIBS="$LIBS"
	   LIBS="-lpthread $LIBS"
	   AC_LINK_IFELSE(
	   [AC_LANG_PROGRAM(
	    [#ifndef _GNU_SOURCE
	     #define _GNU_SOURCE 1
	     #endif
	     #include <pthread.h>
	     pthread_t th; void *status;],
	    [pthread_tryjoin_np (th, &status);])],[enable_linux_futex=yes],
	    [if test x$cross_compiling = xno; then
	       if getconf GNU_LIBPTHREAD_VERSION 2>/dev/null \
		  | LC_ALL=C grep -i NPTL > /dev/null 2>/dev/null; then :; else
		 AC_MSG_WARN([The kernel might not support futex or gettid syscalls.
If so, please configure with --disable-linux-futex])
	       fi
	     fi
	     enable_linux_futex=yes])
	   LIBS="$save_LIBS"])
	;;
      yes)
	AC_LINK_IFELSE(
	 [AC_LANG_PROGRAM(
	  [#include <sys/syscall.h>
	   int lk;],
	  [syscall (SYS_gettid); syscall (SYS_futex, &lk, 0, 0, 0);])],[],
	  [AC_MSG_ERROR([SYS_gettid and SYS_futex required for --enable-linux-futex])])
	;;
    esac
    ;;
  *)
    enable_linux_futex=no
    ;;
esac
if test x$enable_linux_futex = xyes; then
  $1
fi
])
