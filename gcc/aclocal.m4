dnl See whether we need a declaration for a function.
AC_DEFUN(GCC_NEED_DECLARATION,
[AC_MSG_CHECKING([whether $1 must be declared])
AC_CACHE_VAL(gcc_cv_decl_needed_$1,
[AC_TRY_COMPILE([
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#else
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif],
[char *(*pfn) = (char *(*)) $1],
gcc_cv_decl_needed_$1=no, gcc_cv_decl_needed_$1=yes)])
AC_MSG_RESULT($gcc_cv_decl_needed_$1)
if test $gcc_cv_decl_needed_$1 = yes; then
  gcc_tr_decl=NEED_DECLARATION_`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
  AC_DEFINE_UNQUOTED($gcc_tr_decl)
fi
])dnl

dnl See if symbolic links work and if not, try to substitute either hard links or simple copy.
AC_DEFUN(GCC_PROG_LN_S,
[AC_MSG_CHECKING(whether ln -s works)
AC_CACHE_VAL(gcc_cv_prog_LN_S,
[rm -f conftestdata_to
echo >conftestdata_from
if ln -s conftestdata_from conftestdata_to 2>/dev/null
then
  gcc_cv_prog_LN_S="ln -s"
else
  if ln conftestdata_from conftestdata_to 2>/dev/null
  then
    gcc_cv_prog_LN_S=ln
  else
    gcc_cv_prog_LN_S=cp
  fi
fi
rm -f conftestdata_from conftestdata_to
])dnl
LN_S="$gcc_cv_prog_LN_S"
if test "$gcc_cv_prog_LN_S" = "ln -s"; then
  AC_MSG_RESULT(yes)
else
  if test "$gcc_cv_prog_LN_S" = "ln"; then
    AC_MSG_RESULT([no, using ln])
  else
    AC_MSG_RESULT([no, and neither does ln, so using cp])
  fi
fi
AC_SUBST(LN_S)dnl
])

dnl See if hard links work and if not, try to substitute either symbolic links or simple copy.
AC_DEFUN(GCC_PROG_LN,
[AC_MSG_CHECKING(whether ln works)
AC_CACHE_VAL(gcc_cv_prog_LN,
[rm -f conftestdata_to
echo >conftestdata_from
if ln conftestdata_from conftestdata_to 2>/dev/null
then
  gcc_cv_prog_LN="ln"
else
  if ln -s conftestdata_from conftestdata_to 2>/dev/null
  then
    gcc_cv_prog_LN="ln -s"
  else
    gcc_cv_prog_LN=cp
  fi
fi
rm -f conftestdata_from conftestdata_to
])dnl
LN="$gcc_cv_prog_LN"
if test "$gcc_cv_prog_LN" = "ln"; then
  AC_MSG_RESULT(yes)
else
  if test "$gcc_cv_prog_LN" = "ln -s"; then
    AC_MSG_RESULT([no, using ln -s])
  else
    AC_MSG_RESULT([no, and neither does ln -s, so using cp])
  fi
fi
AC_SUBST(LN)dnl
])
