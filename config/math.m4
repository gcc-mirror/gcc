dnl GCC_CHECK_LIBM
dnl
dnl Check whether -lm is available.  This is a pre-requisite for
dnl GCC_CHECK_MATH_FUNC so that it will link with -lm.
AC_DEFUN([GCC_CHECK_LIBM],
[AC_CHECK_LIB([m],[sin])])

dnl GCC_CHECK_MATH_HEADERS
dnl
dnl Check for math.h and complex.h.  This is a pre-requisite for
dnl GCC_CHECK_MATH_FUNC so that it includes the right headers.
dnl (Some systems, such as AIX or OpenVMS may define macro for math
dnl  functions).
AC_DEFUN([GCC_CHECK_MATH_HEADERS],
[AC_CHECK_HEADERS_ONCE(math.h complex.h)])

dnl GCC_CHECK_MATH_FUNC([name])
dnl
dnl Check whether math function NAME is available on the system (by compiling
dnl and linking a C program) and run define HAVE_name on success.
dnl
dnl Note that OpenVMS system insists on including complex.h before math.h
AC_DEFUN([GCC_CHECK_MATH_FUNC],
[
  AC_REQUIRE([GCC_CHECK_LIBM])
  AC_REQUIRE([GCC_CHECK_MATH_HEADERS])
  AC_CACHE_CHECK([for $1], [gcc_cv_math_func_$1],
		 [AC_LINK_IFELSE([
#ifdef HAVE_COMPLEX_H
#include <complex.h>
#endif
#ifdef HAVE_MATH_H
#include <math.h>
#endif

int (*ptr)() = (int (*)())$1;

int
main ()
{
  return 0;
}
],
[gcc_cv_math_func_$1=yes],
[gcc_cv_math_func_$1=no])])
  if test $gcc_cv_math_func_$1 = yes; then
    AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_$1),[1],
                       [Define to 1 if you have the `$1' function.])
  fi
])
