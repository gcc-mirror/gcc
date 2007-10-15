dnl Check whether the target supports TLS.
AC_DEFUN([GCC_CHECK_TLS], [
  GCC_ENABLE(tls, yes, [], [Use thread-local storage])
  AC_CACHE_CHECK([whether the target supports thread-local storage],
		 gcc_cv_have_tls, [
    AC_RUN_IFELSE([__thread int a; int b; int main() { return a = b; }],
      [dnl If the test case passed with dynamic linking, try again with
       dnl static linking, but only if static linking is supported (not
       dnl on Solaris 10).  This fails with some older Red Hat releases.
      chktls_save_LDFLAGS="$LDFLAGS"
      LDFLAGS="-static $LDFLAGS"
      AC_LINK_IFELSE([int main() { return 0; }],
	AC_RUN_IFELSE([__thread int a; int b; int main() { return a = b; }],
		      [gcc_cv_have_tls=yes], [gcc_cv_have_tls=no],[]),
	[gcc_cv_have_tls=yes])
      LDFLAGS="$chktls_save_LDFLAGS"
      if test $gcc_cv_have_tls = yes; then
	dnl So far, the binutils and the compiler support TLS.
	dnl Also check whether the libc supports TLS, i.e. whether a variable
	dnl with __thread linkage has a different address in different threads.
	dnl First, find the thread_CFLAGS necessary for linking a program that
	dnl calls pthread_create.
	chktls_save_CFLAGS="$CFLAGS"
	thread_CFLAGS=failed
	for flag in '' '-pthread' '-lpthread'; do
	  CFLAGS="$flag $chktls_save_CFLAGS"
	  AC_LINK_IFELSE(
	    [AC_LANG_PROGRAM(
	       [#include <pthread.h>
		void *g(void *d) { return NULL; }],
	       [pthread_t t; pthread_create(&t,NULL,g,NULL);])],
	    [thread_CFLAGS="$flag"])
	  if test "X$thread_CFLAGS" != Xfailed; then
	    break
	  fi
	done
	CFLAGS="$chktls_save_CFLAGS"
	if test "X$thread_CFLAGS" != Xfailed; then
	  CFLAGS="$thread_CFLAGS $chktls_save_CFLAGS"
	  AC_RUN_IFELSE(
	    [AC_LANG_PROGRAM(
	       [#include <pthread.h>
		__thread int a;
		static int *a_in_other_thread;
		static void *
		thread_func (void *arg)
		{
		  a_in_other_thread = &a;
		  return (void *)0;
		}],
	       [pthread_t thread;
		void *thread_retval;
		int *a_in_main_thread;
		if (pthread_create (&thread, (pthread_attr_t *)0,
				    thread_func, (void *)0))
		  return 0;
		a_in_main_thread = &a;
		if (pthread_join (thread, &thread_retval))
		  return 0;
		return (a_in_other_thread == a_in_main_thread);])],
	     [gcc_cv_have_tls=yes], [gcc_cv_have_tls=no], [])
	  CFLAGS="$chktls_save_CFLAGS"
	fi
      fi],
      [gcc_cv_have_tls=no],
      [dnl This is the cross-compiling case. Assume libc supports TLS if the
       dnl binutils and the compiler do.
       AC_LINK_IFELSE([__thread int a; int b; int main() { return a = b; }],
		      [gcc_cv_have_tls=yes], [gcc_cv_have_tls=no])
      ]
    )])
  if test "$enable_tls $gcc_cv_have_tls" = "yes yes"; then
    AC_DEFINE(HAVE_TLS, 1,
	      [Define to 1 if the target supports thread-local storage.])
  fi])

dnl Check whether the target assembler supports TLS.
AC_DEFUN([GCC_CHECK_CC_TLS], [
  GCC_ENABLE(tls, yes, [], [Use thread-local storage])
  AC_CACHE_CHECK([whether the target asssembler upports thread-local storage],
		 gcc_cv_have_cc_tls, [
    AC_COMPILE_IFELSE([__thread int a; int b; int main() { return a = b; }],
      [gcc_cv_have_cc_tls=yes], [gcc_cv_have_cc_tls=no])]
    )])
  if test "$enable_tls $gcc_cv_have_cc_tls" = "yes yes"; then
    AC_DEFINE(HAVE_CC_TLS, 1,
	      [Define to 1 if the target assembler supports thread-local storage.])
  fi])
