
dnl Host type sizes probe.
dnl By Kaveh R. Ghazi.  One typo fixed since.
dnl Modified to return a size of 0 if type doesn't exist
dnl
AC_DEFUN([gcc_AC_COMPILE_CHECK_SIZEOF],
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(sizeof_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(size of $1)
AC_CACHE_VAL(AC_CV_NAME,
[for ac_size in 4 8 1 2 16 $3 ; do # List sizes in rough order of prevalence.
  AC_TRY_COMPILE([#include "confdefs.h"
#include <sys/types.h>
$2
], [switch (0) case 0: case (sizeof ($1) == $ac_size):;], AC_CV_NAME=$ac_size)
  if test x$AC_CV_NAME != x ; then break; fi
done
])
if test x$AC_CV_NAME = x ; then
  AC_CV_NAME=0
fi
AC_MSG_RESULT($AC_CV_NAME)
AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME, [The number of bytes in type $1])
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
])

dnl Utility macro used by next two tests.
dnl AC_EXAMINE_OBJECT(C source code,
dnl	commands examining object file,
dnl	[commands to run if compile failed]):
dnl
dnl Compile the source code to an object file; then convert it into a
dnl printable representation.  All unprintable characters and
dnl asterisks (*) are replaced by dots (.).  All white space is
dnl deleted.  Newlines (ASCII 0x10) in the input are preserved in the
dnl output, but runs of newlines are compressed to a single newline.
dnl Finally, line breaks are forcibly inserted so that no line is
dnl longer than 80 columns and the file ends with a newline.  The
dnl result of all this processing is in the file conftest.dmp, which
dnl may be examined by the commands in the second argument.
dnl
AC_DEFUN([gcc_AC_EXAMINE_OBJECT],
[AC_LANG_SAVE
AC_LANG_C
dnl Next bit cribbed from AC_TRY_COMPILE.
cat > conftest.$ac_ext <<EOF
[#line __oline__ "configure"
#include "confdefs.h"
$1
]EOF
if AC_TRY_EVAL(ac_compile); then
  od -c conftest.o |
    sed ['s/^[0-7]*[ 	]*/ /
	  s/\*/./g
	  s/ \\n/*/g
	  s/ [0-9][0-9][0-9]/./g
	  s/  \\[^ ]/./g'] |
    tr -d '
 ' | tr -s '*' '
' | fold | sed '$a\
' > conftest.dmp
  $2
ifelse($3, , , else
  $3
)dnl
fi
rm -rf conftest*
AC_LANG_RESTORE])

dnl Host endianness probe.
dnl Differs from AC_C_BIGENDIAN in that it does not require
dnl running a program on the host.
dnl
AC_DEFUN([fastjar_AC_COMPILE_C_BIGENDIAN],
[AC_CACHE_CHECK(byte ordering, ac_cv_c_compile_endian,
[ac_cv_c_compile_endian=unknown
gcc_AC_EXAMINE_OBJECT([
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
/* This structure must have no internal padding.  */
  struct {
    char prefix[sizeof "\nendian:" - 1];
    short word;
    char postfix[2];
 } tester = {
    "\nendian:",
#if SIZEOF_SHORT == 4
    ('A' << (CHAR_BIT * 3)) | ('B' << (CHAR_BIT * 2)) |
#endif
    ('A' << CHAR_BIT) | 'B',
    'X', '\n'
};],
 [if   grep 'endian:AB' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_compile_endian=big-endian
  elif grep 'endian:BA' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_compile_endian=little-endian
  fi])
])
if test $ac_cv_c_compile_endian = unknown; then
  AC_MSG_ERROR([*** unable to determine endianness])
elif test $ac_cv_c_compile_endian = big-endian; then
  AC_DEFINE(WORDS_BIG_ENDIAN, 1,
  [Define if the host machine stores words of multi-word integers in
   big-endian order.])
fi
])

dnl Define MKDIR_TAKES_ONE_ARG if mkdir accepts only one argument instead
dnl of the usual 2.
AC_DEFUN(gcc_AC_FUNC_MKDIR_TAKES_ONE_ARG,
[AC_CACHE_CHECK([if mkdir takes one argument], gcc_cv_mkdir_takes_one_arg,
[AC_TRY_COMPILE([
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>
#endif], [mkdir ("foo", 0);],
         gcc_cv_mkdir_takes_one_arg=no, gcc_cv_mkdir_takes_one_arg=yes)])
if test $gcc_cv_mkdir_takes_one_arg = yes ; then
   AC_DEFINE(MKDIR_TAKES_ONE_ARG, 1, [Define if host mkdir takes a
single argument.])
fi
])
