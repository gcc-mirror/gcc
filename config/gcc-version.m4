dnl Usage: TL_AC_GCC_VERSION(TOPSRCDIR)
dnl
dnl Set up the variables:
dnl
dnl     gcc_version_trigger: pathname of gcc's version.c, if available
dnl     gcc_version_full: full gcc version string
dnl     gcc_version: the first "word" in $gcc_version_full
dnl
dnl TOPSRCDIR is the top-level source directory.
AC_DEFUN([TL_AC_GCC_VERSION],
[
changequote(,)dnl
if test "${with_gcc_version_trigger+set}" = set; then
  gcc_version_trigger=$with_gcc_version_trigger
else
  gcc_version_trigger=$1/gcc/version.c
fi
if test -f "${gcc_version_trigger}"; then
  gcc_version_full=`grep version_string "${gcc_version_trigger}" | sed -e 's/.*"\([^"]*\)".*/\1/'`
else
  gcc_version_full=`$CC -v 2>&1 | sed -n 's/^gcc version //p'`
fi
gcc_version=`echo ${gcc_version_full} | sed -e 's/\([^ ]*\) .*/\1/'`
changequote([,])dnl
AC_SUBST(gcc_version_trigger)
AC_SUBST(gcc_version_full)
AC_SUBST(gcc_version)
])dnl
