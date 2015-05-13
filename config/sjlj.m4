dnl Check if the compiler is configured for setjmp/longjmp exceptions.
AC_DEFUN([GCC_CHECK_SJLJ_EXCEPTIONS],
  [AC_CACHE_CHECK([whether the compiler is configured for setjmp/longjmp exceptions],
    ac_cv_sjlj_exceptions,
    [AC_COMPILE_IFELSE(
      [AC_LANG_PROGRAM(
        [[#ifdef __USING_SJLJ_EXCEPTIONS__
          this will fail
          #endif]],
        [[int i;]])],
      [ac_cv_sjlj_exceptions=no],
      [ac_cv_sjlj_exceptions=yes])])])
