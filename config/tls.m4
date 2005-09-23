dnl Check whether the target supports TLS.
AC_DEFUN([GCC_CHECK_TLS], [
  GCC_ENABLE(tls, yes, [Use thread-local storage])
  AC_CACHE_CHECK([whether the target supports thread-local storage],
		 have_tls, [
    AC_RUN_IFELSE([__thread int a; int b; int main() { return a = b; }],
      [dnl If the test case passed with dynamic linking, try again with
      dnl static linking.  This fails at least with some older Red Hat
      dnl releases.
      save_LDFLAGS="$LDFLAGS"
      LDFLAGS="-static $LDFLAGS"
      AC_RUN_IFELSE([__thread int a; int b; int main() { return a = b; }],
		    [have_tls=yes], [have_tls=no], [])
      LDFLAGS="$save_LDFLAGS"],
      [have_tls=no],
      [AC_COMPILE_IFELSE([__thread int foo;], [have_tls=yes], [have_tls=no])]
    )])
  if test "$enable_tls $have_tls" = "yes yes"; then
    AC_DEFINE(HAVE_TLS, 1,
	      [Define to 1 if the target supports thread-local storage.])
  fi])
