dnl See if stdbool.h properly defines bool and true/false.
AC_DEFUN(gcc_AC_HEADER_STDBOOL,
[AC_CACHE_CHECK([for working stdbool.h],
  ac_cv_header_stdbool_h,
[AC_TRY_COMPILE([#include <stdbool.h>],
[bool foo = false;],
ac_cv_header_stdbool_h=yes, ac_cv_header_stdbool_h=no)])
if test $ac_cv_header_stdbool_h = yes; then
  AC_DEFINE(HAVE_STDBOOL_H, 1,
  [Define if you have a working <stdbool.h> header file.])
fi
])

dnl See whether we can include both string.h and strings.h.
AC_DEFUN(gcc_AC_HEADER_STRING,
[AC_CACHE_CHECK([whether string.h and strings.h may both be included],
  gcc_cv_header_string,
[AC_TRY_COMPILE([#include <string.h>
#include <strings.h>], , gcc_cv_header_string=yes, gcc_cv_header_string=no)])
if test $gcc_cv_header_string = yes; then
  AC_DEFINE(STRING_WITH_STRINGS, 1, [Define if you can safely include both <string.h> and <strings.h>.])
fi
])

dnl See whether we need a declaration for a function.
dnl The result is highly dependent on the INCLUDES passed in, so make sure
dnl to use a different cache variable name in this macro if it is invoked
dnl in a different context somewhere else.
dnl gcc_AC_CHECK_DECL(SYMBOL,
dnl 	[ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, INCLUDES]]])
AC_DEFUN(gcc_AC_CHECK_DECL,
[AC_MSG_CHECKING([whether $1 is declared])
AC_CACHE_VAL(gcc_cv_have_decl_$1,
[AC_TRY_COMPILE([$4],
[#ifndef $1
char *(*pfn) = (char *(*)) $1 ;
#endif], eval "gcc_cv_have_decl_$1=yes", eval "gcc_cv_have_decl_$1=no")])
if eval "test \"`echo '$gcc_cv_have_decl_'$1`\" = yes"; then
  AC_MSG_RESULT(yes) ; ifelse([$2], , :, [$2])
else
  AC_MSG_RESULT(no) ; ifelse([$3], , :, [$3])
fi
])dnl

dnl Check multiple functions to see whether each needs a declaration.
dnl Arrange to define HAVE_DECL_<FUNCTION> to 0 or 1 as appropriate.
dnl gcc_AC_CHECK_DECLS(SYMBOLS,
dnl 	[ACTION-IF-NEEDED [, ACTION-IF-NOT-NEEDED [, INCLUDES]]])
AC_DEFUN(gcc_AC_CHECK_DECLS,
[for ac_func in $1
do
changequote(, )dnl
  ac_tr_decl=HAVE_DECL_`echo $ac_func | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
changequote([, ])dnl
gcc_AC_CHECK_DECL($ac_func,
  [AC_DEFINE_UNQUOTED($ac_tr_decl, 1) $2],
  [AC_DEFINE_UNQUOTED($ac_tr_decl, 0) $3],
dnl It is possible that the include files passed in here are local headers
dnl which supply a backup declaration for the relevant prototype based on
dnl the definition of (or lack of) the HAVE_DECL_ macro.  If so, this test
dnl will always return success.  E.g. see libiberty.h's handling of
dnl `basename'.  To avoid this, we define the relevant HAVE_DECL_ macro to
dnl 1 so that any local headers used do not provide their own prototype
dnl during this test.
#undef $ac_tr_decl
#define $ac_tr_decl 1
  $4
)
done
dnl Automatically generate config.h entries via autoheader.
if test x = y ; then
  patsubst(translit([$1], [a-z], [A-Z]), [\w+],
    [AC_DEFINE([HAVE_DECL_\&], 1,
      [Define to 1 if we found this declaration otherwise define to 0.])])dnl
fi
])

dnl See if the printf functions in libc support %p in format strings.
AC_DEFUN(gcc_AC_FUNC_PRINTF_PTR,
[AC_CACHE_CHECK(whether the printf functions support %p,
  gcc_cv_func_printf_ptr,
[AC_TRY_RUN([#include <stdio.h>

int main()
{
  char buf[64];
  char *p = buf, *q = NULL;
  sprintf(buf, "%p", p);
  sscanf(buf, "%p", &q);
  return (p != q);
}], gcc_cv_func_printf_ptr=yes, gcc_cv_func_printf_ptr=no,
	gcc_cv_func_printf_ptr=no)
rm -f core core.* *.core])
if test $gcc_cv_func_printf_ptr = yes ; then
  AC_DEFINE(HAVE_PRINTF_PTR, 1, [Define if printf supports "%p".])
fi
])

dnl See if symbolic links work and if not, try to substitute either hard links or simple copy.
AC_DEFUN(gcc_AC_PROG_LN_S,
[AC_MSG_CHECKING(whether ln -s works)
AC_CACHE_VAL(gcc_cv_prog_LN_S,
[rm -f conftestdata_t
echo >conftestdata_f
if ln -s conftestdata_f conftestdata_t 2>/dev/null
then
  gcc_cv_prog_LN_S="ln -s"
else
  if ln conftestdata_f conftestdata_t 2>/dev/null
  then
    gcc_cv_prog_LN_S=ln
  else
    gcc_cv_prog_LN_S=cp
  fi
fi
rm -f conftestdata_f conftestdata_t
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
AC_DEFUN(gcc_AC_PROG_LN,
[AC_MSG_CHECKING(whether ln works)
AC_CACHE_VAL(gcc_cv_prog_LN,
[rm -f conftestdata_t
echo >conftestdata_f
if ln conftestdata_f conftestdata_t 2>/dev/null
then
  gcc_cv_prog_LN="ln"
else
  if ln -s conftestdata_f conftestdata_t 2>/dev/null
  then
    gcc_cv_prog_LN="ln -s"
  else
    gcc_cv_prog_LN=cp
  fi
fi
rm -f conftestdata_f conftestdata_t
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

dnl See whether the stage1 host compiler accepts the volatile keyword.
AC_DEFUN(gcc_AC_C_VOLATILE,
[AC_CACHE_CHECK([for volatile], gcc_cv_c_volatile,
[AC_TRY_COMPILE(, [volatile int foo;],
        gcc_cv_c_volatile=yes, gcc_cv_c_volatile=no)])
if test $gcc_cv_c_volatile = yes ; then
  AC_DEFINE(HAVE_VOLATILE, 1, [Define if your compiler understands volatile.])
fi
])

dnl Check whether long double is supported.  This differs from the
dnl built-in autoconf test in that it works for cross compiles.
AC_DEFUN(gcc_AC_C_LONG_DOUBLE,
[AC_CACHE_CHECK(for long double, gcc_cv_c_long_double,
[if test "$GCC" = yes; then
  gcc_cv_c_long_double=yes
else
AC_TRY_COMPILE(,
[/* The Stardent Vistra knows sizeof(long double), but does not support it.  */
long double foo = 0.0;
/* On Ultrix 4.3 cc, long double is 4 and double is 8.  */
switch (0) case 0: case (sizeof(long double) >= sizeof(double)):;],
gcc_cv_c_long_double=yes, gcc_cv_c_long_double=no)
fi])
if test $gcc_cv_c_long_double = yes; then
  AC_DEFINE(HAVE_LONG_DOUBLE, 1, 
      [Define if your compiler supports the \`long double' type.])
fi
])

dnl Check whether _Bool is built-in.
AC_DEFUN(gcc_AC_C__BOOL,
[AC_CACHE_CHECK(for built-in _Bool, gcc_cv_c__bool,
[AC_TRY_COMPILE(,
[_Bool foo;],
gcc_cv_c__bool=yes, gcc_cv_c__bool=no)
])
if test $gcc_cv_c__bool = yes; then
  AC_DEFINE(HAVE__BOOL, 1, [Define if the \`_Bool' type is built-in.])
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
  AC_DEFINE(MKDIR_TAKES_ONE_ARG, 1, [Define if host mkdir takes a single argument.])
fi
])

AC_DEFUN(gcc_AC_PROG_INSTALL,
[AC_REQUIRE([AC_CONFIG_AUX_DIR_DEFAULT])dnl
# Find a good install program.  We prefer a C program (faster),
# so one script is as good as another.  But avoid the broken or
# incompatible versions:
# SysV /etc/install, /usr/sbin/install
# SunOS /usr/etc/install
# IRIX /sbin/install
# AIX /bin/install
# AFS /usr/afsws/bin/install, which mishandles nonexistent args
# SVR4 /usr/ucb/install, which tries to use the nonexistent group "staff"
# ./install, which can be erroneously created by make from ./install.sh.
AC_MSG_CHECKING(for a BSD compatible install)
if test -z "$INSTALL"; then
AC_CACHE_VAL(ac_cv_path_install,
[  IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="${IFS}:"
  for ac_dir in $PATH; do
    # Account for people who put trailing slashes in PATH elements.
    case "$ac_dir/" in
    /|./|.//|/etc/*|/usr/sbin/*|/usr/etc/*|/sbin/*|/usr/afsws/bin/*|/usr/ucb/*) ;;
    *)
      # OSF1 and SCO ODT 3.0 have their own names for install.
      for ac_prog in ginstall scoinst install; do
        if test -f $ac_dir/$ac_prog; then
	  if test $ac_prog = install &&
            grep dspmsg $ac_dir/$ac_prog >/dev/null 2>&1; then
	    # AIX install.  It has an incompatible calling convention.
	    # OSF/1 installbsd also uses dspmsg, but is usable.
	    :
	  else
	    ac_cv_path_install="$ac_dir/$ac_prog -c"
	    break 2
	  fi
	fi
      done
      ;;
    esac
  done
  IFS="$ac_save_IFS"
])dnl
  if test "${ac_cv_path_install+set}" = set; then
    INSTALL="$ac_cv_path_install"
  else
    # As a last resort, use the slow shell script.  We don't cache a
    # path for INSTALL within a source directory, because that will
    # break other packages using the cache if that directory is
    # removed, or if the path is relative.
    INSTALL="$ac_install_sh"
  fi
fi
dnl We do special magic for INSTALL instead of AC_SUBST, to get
dnl relative paths right.
AC_MSG_RESULT($INSTALL)
AC_SUBST(INSTALL)dnl

# Use test -z because SunOS4 sh mishandles braces in ${var-val}.
# It thinks the first close brace ends the variable substitution.
test -z "$INSTALL_PROGRAM" && INSTALL_PROGRAM='${INSTALL}'
AC_SUBST(INSTALL_PROGRAM)dnl

test -z "$INSTALL_DATA" && INSTALL_DATA='${INSTALL} -m 644'
AC_SUBST(INSTALL_DATA)dnl
])

dnl Test for GNAT.
dnl We require the gnatbind program, and a compiler driver that
dnl understands Ada.  The user may set the driver name explicitly
dnl with ADAC; also, the user's CC setting is tried.  Failing that,
dnl we try gcc and cc, then a sampling of names known to be used for
dnl the Ada driver on various systems.
dnl
dnl Sets the shell variable have_gnat to yes or no as appropriate, and
dnl substitutes GNATBIND and ADAC.
AC_DEFUN([gcc_AC_PROG_GNAT],
[AC_REQUIRE([AC_CHECK_TOOL_PREFIX])
AC_CHECK_TOOL(GNATBIND, gnatbind, no)
AC_CACHE_CHECK([for compiler driver that understands Ada],
		 gcc_cv_prog_adac,
[cat >conftest.adb <<EOF
procedure conftest is begin null; end conftest;
EOF
gcc_cv_prog_adac=no
# Have to do ac_tool_prefix and user overrides by hand.
for cand in ${ADAC+"$ADAC"} ${CC+"$CC"}	\
	    ${ac_tool_prefix}gcc	gcc		\
	    ${ac_tool_prefix}cc		cc		\
	    ${ac_tool_prefix}gnatgcc	gnatgcc		\
	    ${ac_tool_prefix}gnatcc	gnatcc		\
	    ${ac_tool_prefix}adagcc	adagcc		\
	    ${ac_tool_prefix}adacc	adacc		; do
  # There is a bug in all released versions of GCC which causes the
  # driver to exit successfully when the appropriate language module
  # has not been installed.  This is fixed in 2.95.4, 3.0.2, and 3.1.
  # Therefore we must check for the error message as well as an
  # unsuccessful exit.
  errors=`($cand -c conftest.adb) 2>&1 || echo failure`
  if test x"$errors" = x; then
    gcc_cv_prog_adac=$cand
    break
  fi
done
rm -f conftest.*])
ADAC=$gcc_cv_prog_adac
AC_SUBST(ADAC)

if test x$GNATBIND != xno && test x$ADAC != xno; then
  have_gnat=yes
else
  have_gnat=no
fi
])

#serial 1
dnl This test replaces the one in autoconf.
dnl Currently this macro should have the same name as the autoconf macro
dnl because gettext's gettext.m4 (distributed in the automake package)
dnl still uses it.  Otherwise, the use in gettext.m4 makes autoheader
dnl give these diagnostics:
dnl   configure.in:556: AC_TRY_COMPILE was called before AC_ISC_POSIX
dnl   configure.in:556: AC_TRY_RUN was called before AC_ISC_POSIX

undefine([AC_ISC_POSIX])
AC_DEFUN(AC_ISC_POSIX,
  [
    dnl This test replaces the obsolescent AC_ISC_POSIX kludge.
    AC_CHECK_LIB(cposix, strerror, [LIBS="$LIBS -lcposix"])
  ]
)


dnl GCC_PATH_PROG(VARIABLE, PROG-TO-CHECK-FOR [, VALUE-IF-NOT-FOUND [, PATH]])
dnl like AC_PATH_PROG but use other cache variables
AC_DEFUN(GCC_PATH_PROG,
[# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL(gcc_cv_path_$1,
[case "[$]$1" in
  /*)
  gcc_cv_path_$1="[$]$1" # Let the user override the test with a path.
  ;;
  ?:/*)			 
  gcc_cv_path_$1="[$]$1" # Let the user override the test with a dos path.
  ;;
  *)
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=":"
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
  ac_dummy="ifelse([$4], , $PATH, [$4])"
  for ac_dir in $ac_dummy; do 
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$ac_word; then
      gcc_cv_path_$1="$ac_dir/$ac_word"
      break
    fi
  done
  IFS="$ac_save_ifs"
dnl If no 3rd arg is given, leave the cache variable unset,
dnl so GCC_PATH_PROGS will keep looking.
ifelse([$3], , , [  test -z "[$]gcc_cv_path_$1" && gcc_cv_path_$1="$3"
])dnl
  ;;
esac])dnl
$1="$gcc_cv_path_$1"
if test -n "[$]$1"; then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])

# mmap(2) blacklisting.  Some platforms provide the mmap library routine
# but don't support all of the features we need from it.
AC_DEFUN([gcc_AC_FUNC_MMAP_BLACKLIST],
[if test $ac_cv_header_sys_mman_h != yes \
 || test $ac_cv_func_mmap != yes; then
   gcc_cv_func_mmap_file=no
   gcc_cv_func_mmap_dev_zero=no
   gcc_cv_func_mmap_anon=no
else
   AC_CACHE_CHECK([whether read-only mmap of a plain file works], 
  gcc_cv_func_mmap_file,
  [# Add a system to this blacklist if 
   # mmap(0, stat_size, PROT_READ, MAP_PRIVATE, fd, 0) doesn't return a
   # memory area containing the same data that you'd get if you applied
   # read() to the same fd.  The only system known to have a problem here
   # is VMS, where text files have record structure.
   case "$host_os" in
     vms*) 
        gcc_cv_func_mmap_file=no ;;
     *)
        gcc_cv_func_mmap_file=yes;;
   esac])
   AC_CACHE_CHECK([whether mmap from /dev/zero works],
  gcc_cv_func_mmap_dev_zero,
  [# Add a system to this blacklist if it has mmap() but /dev/zero
   # does not exist, or if mmapping /dev/zero does not give anonymous
   # zeroed pages with both the following properties:
   # 1. If you map N consecutive pages in with one call, and then
   #    unmap any subset of those pages, the pages that were not
   #    explicitly unmapped remain accessible.
   # 2. If you map two adjacent blocks of memory and then unmap them
   #    both at once, they must both go away.
   # Systems known to be in this category are Windows (all variants),
   # VMS, and Darwin.
   case "$host_os" in
     vms* | cygwin* | pe | mingw* | darwin* | ultrix* | hpux10* | hpux11.00)
        gcc_cv_func_mmap_dev_zero=no ;;
     *)
        gcc_cv_func_mmap_dev_zero=yes;;
   esac])

   # Unlike /dev/zero, the MAP_ANON(YMOUS) defines can be probed for.
   AC_CACHE_CHECK([for MAP_ANON(YMOUS)], gcc_cv_decl_map_anon,
    [AC_TRY_COMPILE(
[#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
],
[int n = MAP_ANONYMOUS;],
    gcc_cv_decl_map_anon=yes,
    gcc_cv_decl_map_anon=no)])

   if test $gcc_cv_decl_map_anon = no; then
     gcc_cv_func_mmap_anon=no
   else
     AC_CACHE_CHECK([whether mmap with MAP_ANON(YMOUS) works],
     gcc_cv_func_mmap_anon,
  [# Add a system to this blacklist if it has mmap() and MAP_ANON or
   # MAP_ANONYMOUS, but using mmap(..., MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)
   # doesn't give anonymous zeroed pages with the same properties listed
   # above for use of /dev/zero.
   # Systems known to be in this category are Windows, VMS, and SCO Unix.
   case "$host_os" in
     vms* | cygwin* | pe | mingw* | sco* | udk* )
        gcc_cv_func_mmap_anon=no ;;
     *)
        gcc_cv_func_mmap_anon=yes;;
   esac])
   fi
fi

if test $gcc_cv_func_mmap_file = yes; then
  AC_DEFINE(HAVE_MMAP_FILE, 1,
	    [Define if read-only mmap of a plain file works.])
fi
if test $gcc_cv_func_mmap_dev_zero = yes; then
  AC_DEFINE(HAVE_MMAP_DEV_ZERO, 1,
	    [Define if mmap of /dev/zero works.])
fi
if test $gcc_cv_func_mmap_anon = yes; then
  AC_DEFINE(HAVE_MMAP_ANON, 1,
	    [Define if mmap with MAP_ANON(YMOUS) works.])
fi
])

dnl Locate a program and check that its version is acceptable.
dnl AC_PROG_CHECK_VER(var, name, version-switch,
dnl                  version-extract-regexp, version-glob)
AC_DEFUN(gcc_AC_CHECK_PROG_VER,
[AC_CHECK_PROG([$1], [$2], [$2])
if test -n "[$]$1"; then
  # Found it, now check the version.
  AC_CACHE_CHECK(for modern $2, gcc_cv_prog_$2_modern,
[changequote(<<,>>)dnl
  ac_prog_version=`<<$>>$1 $3 2>&1 |
                   sed -n 's/^.*patsubst(<<$4>>,/,\/).*$/\1/p'`
changequote([,])dnl
  echo "configure:__oline__: version of $2 is $ac_prog_version" >&AC_FD_CC
changequote(<<,>>)dnl
  case $ac_prog_version in
    '')     gcc_cv_prog_$2_modern=no;;
    <<$5>>)
            gcc_cv_prog_$2_modern=yes;;
    *)      gcc_cv_prog_$2_modern=no;;
  esac
changequote([,])dnl
])
else
  gcc_cv_prog_$2_modern=no
fi
])

dnl Determine if enumerated bitfields are unsigned.   ISO C says they can 
dnl be either signed or unsigned.
dnl
AC_DEFUN(gcc_AC_C_ENUM_BF_UNSIGNED,
[AC_CACHE_CHECK(for unsigned enumerated bitfields, gcc_cv_enum_bf_unsigned,
[AC_TRY_RUN(#include <stdlib.h>
enum t { BLAH = 128 } ;
struct s_t { enum t member : 8; } s ;
int main(void)
{            
        s.member = BLAH;
        if (s.member < 0) exit(1);
        exit(0);

}, gcc_cv_enum_bf_unsigned=yes, gcc_cv_enum_bf_unsigned=no, gcc_cv_enum_bf_unsigned=yes)])
if test $gcc_cv_enum_bf_unsigned = yes; then
  AC_DEFINE(ENUM_BITFIELDS_ARE_UNSIGNED, 1,
    [Define if enumerated bitfields are treated as unsigned values.])
fi])

dnl Host type sizes probe.
dnl By Kaveh R. Ghazi.  One typo fixed since.
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
  AC_MSG_ERROR([cannot determine a size for $1])
fi
AC_MSG_RESULT($AC_CV_NAME)
AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME, [The number of bytes in type $1])
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
])

dnl Probe number of bits in a byte.
dnl Note C89 requires CHAR_BIT >= 8.
dnl
AC_DEFUN(gcc_AC_C_CHAR_BIT,
[AC_CACHE_CHECK(for CHAR_BIT, gcc_cv_decl_char_bit,
[AC_EGREP_CPP(found,
[#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif
#ifdef CHAR_BIT
found
#endif], gcc_cv_decl_char_bit=yes, gcc_cv_decl_char_bit=no)
])
if test $gcc_cv_decl_char_bit = no; then
  AC_CACHE_CHECK(number of bits in a byte, gcc_cv_c_nbby,
[i=8
 gcc_cv_c_nbby=
 while test $i -lt 65; do
   AC_TRY_COMPILE(,
     [switch(0) {
  case (unsigned char)((unsigned long)1 << $i) == ((unsigned long)1 << $i):
  case (unsigned char)((unsigned long)1<<($i-1)) == ((unsigned long)1<<($i-1)):
  ; }], 
     [gcc_cv_c_nbby=$i; break])
   i=`expr $i + 1`
 done
 test -z "$gcc_cv_c_nbby" && gcc_cv_c_nbby=failed
])
if test $gcc_cv_c_nbby = failed; then
  AC_MSG_ERROR(cannot determine number of bits in a byte)
else
  AC_DEFINE_UNQUOTED(CHAR_BIT, $gcc_cv_c_nbby,
  [Define as the number of bits in a byte, if \`limits.h' doesn't.])
fi
fi])

dnl Checking for long long.
dnl By Caolan McNamara <caolan@skynet.ie>
dnl Added check for __int64, Zack Weinberg <zackw@stanford.edu>
dnl
AC_DEFUN([gcc_AC_C_LONG_LONG],
[AC_CACHE_CHECK(for long long int, ac_cv_c_long_long,
  [AC_TRY_COMPILE(,[long long int i;],
         ac_cv_c_long_long=yes,
         ac_cv_c_long_long=no)])
  if test $ac_cv_c_long_long = yes; then
    AC_DEFINE(HAVE_LONG_LONG, 1,
      [Define if your compiler supports the \`long long' type.])
  fi
AC_CACHE_CHECK(for __int64, ac_cv_c___int64,
  [AC_TRY_COMPILE(,[__int64 i;],
	ac_cv_c___int64=yes,
	ac_cv_c___int64=no)])
  if test $ac_cv_c___int64 = yes; then
    AC_DEFINE(HAVE___INT64, 1,
      [Define if your compiler supports the \`__int64' type.])
  fi
])

dnl Host character set probe.
dnl The EBCDIC values match the table in config/i370/i370.c;
dnl there are other versions of EBCDIC but GCC won't work with them.
dnl
AC_DEFUN([gcc_AC_C_CHARSET],
[AC_CACHE_CHECK(execution character set, ac_cv_c_charset,
  [AC_EGREP_CPP(ASCII,
[#if '\n' == 0x0A && ' ' == 0x20 && '0' == 0x30 \
   && 'A' == 0x41 && 'a' == 0x61 && '!' == 0x21
ASCII
#endif], ac_cv_c_charset=ASCII)
  if test x${ac_cv_c_charset+set} != xset; then
    AC_EGREP_CPP(EBCDIC,
[#if '\n' == 0x15 && ' ' == 0x40 && '0' == 0xF0 \
   && 'A' == 0xC1 && 'a' == 0x81 && '!' == 0x5A
EBCDIC
#endif], ac_cv_c_charset=EBCDIC)
  fi
  if test x${ac_cv_c_charset+set} != xset; then
    ac_cv_c_charset=unknown
  fi])
if test $ac_cv_c_charset = unknown; then
  AC_MSG_ERROR([*** Cannot determine host character set.])
elif test $ac_cv_c_charset = EBCDIC; then
  AC_DEFINE(HOST_EBCDIC, 1,
  [Define if the host execution character set is EBCDIC.])
fi])

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
dnl This tests byte-within-word endianness.  GCC actually needs
dnl to know word-within-larger-object endianness.  They are the
dnl same on all presently supported hosts.
dnl Differs from AC_C_BIGENDIAN in that it does not require
dnl running a program on the host, and it defines the macro we
dnl want to see.
dnl
AC_DEFUN([gcc_AC_C_COMPILE_ENDIAN],
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
  AC_DEFINE(HOST_WORDS_BIG_ENDIAN, 1,
  [Define if the host machine stores words of multi-word integers in
   big-endian order.])
fi
])

dnl Floating point format probe.
dnl The basic concept is the same as the above: grep the object
dnl file for an interesting string.  We have to watch out for
dnl rounding changing the values in the object, however; this is
dnl handled by ignoring the least significant byte of the float.
dnl
dnl Does not know about VAX G-float or C4x idiosyncratic format.
dnl It does know about PDP-10 idiosyncratic format, but this is
dnl not presently supported by GCC.  S/390 "binary floating point"
dnl is in fact IEEE (but maybe we should have that in EBCDIC as well
dnl as ASCII?)
dnl
AC_DEFUN([gcc_AC_C_FLOAT_FORMAT],
[AC_CACHE_CHECK(floating point format, ac_cv_c_float_format,
[gcc_AC_EXAMINE_OBJECT(
[/* This will not work unless sizeof(double) == 8.  */
extern char sizeof_double_must_be_8 [sizeof(double) == 8 ? 1 : -1];

/* This structure must have no internal padding.  */
struct possibility {
  char prefix[8];
  double candidate;
  char postfix[8];
};

#define C(cand) { "\nformat:", cand, ":tamrof\n" }
struct possibility table [] =
{
  C( 3.25724264705901305206e+01), /* @@IEEEFP - IEEE 754 */
  C( 3.53802595280598432000e+18), /* D__float - VAX */
  C( 5.32201830133125317057e-19), /* D.PDP-10 - PDP-10 - the dot is 0x13a */
  C( 1.77977764695171661377e+10), /* IBMHEXFP - s/390 format, ascii */
  C(-5.22995989424860458374e+10)  /* IBMHEXFP - s/390 format, EBCDIC */
};],
 [if   grep 'format:.@IEEEF.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='IEEE (big-endian)'
  elif grep 'format:.I@@PFE.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='IEEE (big-endian)'
  elif grep 'format:.FEEEI@.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='IEEE (little-endian)'
  elif grep 'format:.EFP@@I.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='IEEE (little-endian)'
  elif grep 'format:.__floa.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='VAX D-float'
  elif grep 'format:..PDP-1.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='PDP-10'
  elif grep 'format:.BMHEXF.:tamrof' conftest.dmp >/dev/null 2>&1; then
    ac_cv_c_float_format='IBM 370 hex'
  else
    AC_MSG_ERROR(Unknown floating point format)
  fi],
  [AC_MSG_ERROR(compile failed)])
])
# IEEE is the default format.  If the float endianness isn't the same
# as the integer endianness, we have to set FLOAT_WORDS_BIG_ENDIAN
# (which is a tristate: yes, no, default).  This is only an issue with
# IEEE; the other formats are only supported by a few machines each,
# all with the same endianness.
format=
fbigend=
case $ac_cv_c_float_format in
    'IEEE (big-endian)' )
	if test $ac_cv_c_compile_endian = little-endian; then
	    fbigend=1
	fi
	;;
    'IEEE (little-endian)' )
	if test $ac_cv_c_compile_endian = big-endian; then
	    fbigend=0
	fi
	;;
    'VAX D-float' )
	format=VAX_FLOAT_FORMAT
	;;
    'PDP-10' )
	format=PDP10_FLOAT_FORMAT
	;;
    'IBM 370 hex' )
	format=IBM_FLOAT_FORMAT
	;;
esac
if test -n "$format"; then
	AC_DEFINE_UNQUOTED(HOST_FLOAT_FORMAT, $format,
  [Define to the floating point format of the host machine, if not IEEE.])
fi
if test -n "$fbigend"; then
	AC_DEFINE_UNQUOTED(HOST_FLOAT_WORDS_BIG_ENDIAN, $fbigend,
  [Define to 1 if the host machine stores floating point numbers in
   memory with the word containing the sign bit at the lowest address,
   or to 0 if it does it the other way around.

   This macro should not be defined if the ordering is the same as for
   multi-word integers.])
fi
])

#serial AM2

dnl From Bruno Haible.

AC_DEFUN([AM_ICONV],
[
  dnl Some systems have iconv in libc, some have it in libiconv (OSF/1 and
  dnl those with the standalone portable GNU libiconv installed).

  am_cv_lib_iconv_ldpath=
  AC_ARG_WITH([libiconv-prefix],
[  --with-libiconv-prefix=DIR  search for libiconv in DIR/include and DIR/lib], [
    for dir in `echo "$withval" | tr : ' '`; do
      if test -d $dir/include; then CPPFLAGS="$CPPFLAGS -I$dir/include"; fi
      if test -d $dir/lib; then am_cv_lib_iconv_ldpath="-L$dir/lib"; fi
    done
   ])

  AC_CACHE_CHECK(for iconv, am_cv_func_iconv, [
    am_cv_func_iconv="no, consider installing GNU libiconv"
    am_cv_lib_iconv=no
    AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
      [iconv_t cd = iconv_open("","");
       iconv(cd,NULL,NULL,NULL,NULL);
       iconv_close(cd);],
      am_cv_func_iconv=yes)
    if test "$am_cv_func_iconv" != yes; then
      am_save_LIBS="$LIBS"
      LIBS="$LIBS $am_cv_libiconv_ldpath -liconv"
      AC_TRY_LINK([#include <stdlib.h>
#include <iconv.h>],
        [iconv_t cd = iconv_open("","");
         iconv(cd,NULL,NULL,NULL,NULL);
         iconv_close(cd);],
        am_cv_lib_iconv=yes
        am_cv_func_iconv=yes)
      LIBS="$am_save_LIBS"
    fi
  ])
  if test "$am_cv_func_iconv" = yes; then
    AC_DEFINE(HAVE_ICONV, 1, [Define if you have the iconv() function.])
    AC_MSG_CHECKING([for iconv declaration])
    AC_CACHE_VAL(am_cv_proto_iconv, [
      AC_TRY_COMPILE([
#include <stdlib.h>
#include <iconv.h>
extern
#ifdef __cplusplus
"C"
#endif
#if defined(__STDC__) || defined(__cplusplus)
size_t iconv (iconv_t cd, char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);
#else
size_t iconv();
#endif
], [], am_cv_proto_iconv_arg1="", am_cv_proto_iconv_arg1="const")
      am_cv_proto_iconv="extern size_t iconv (iconv_t cd, $am_cv_proto_iconv_arg1 char * *inbuf, size_t *inbytesleft, char * *outbuf, size_t *outbytesleft);"])
    am_cv_proto_iconv=`echo "[$]am_cv_proto_iconv" | tr -s ' ' | sed -e 's/( /(/'`
    AC_MSG_RESULT([$]{ac_t:-
         }[$]am_cv_proto_iconv)
    AC_DEFINE_UNQUOTED(ICONV_CONST, $am_cv_proto_iconv_arg1,
      [Define as const if the declaration of iconv() needs const.])
  fi
  LIBICONV=
  if test "$am_cv_lib_iconv" = yes; then
    LIBICONV="$am_cv_lib_iconv_ldpath -liconv"
  fi
  AC_SUBST(LIBICONV)
])

### Gettext macros begin here.
### Changes for GCC marked by 'dnl GCC LOCAL'.
### Note iconv.m4 appears above, as it's used for other reasons.

#serial AM1

dnl From Bruno Haible.

AC_DEFUN([AM_LANGINFO_CODESET],
[
  AC_CACHE_CHECK([for nl_langinfo and CODESET], am_cv_langinfo_codeset,
    [AC_TRY_LINK([#include <langinfo.h>],
      [char* cs = nl_langinfo(CODESET);],
      am_cv_langinfo_codeset=yes,
      am_cv_langinfo_codeset=no)
    ])
  if test $am_cv_langinfo_codeset = yes; then
    AC_DEFINE(HAVE_LANGINFO_CODESET, 1,
      [Define if you have <langinfo.h> and nl_langinfo(CODESET).])
  fi
])

#serial 1
# This test replaces the one in autoconf.
# Currently this macro should have the same name as the autoconf macro
# because gettext's gettext.m4 (distributed in the automake package)
# still uses it.  Otherwise, the use in gettext.m4 makes autoheader
# give these diagnostics:
#   configure.in:556: AC_TRY_COMPILE was called before AC_ISC_POSIX
#   configure.in:556: AC_TRY_RUN was called before AC_ISC_POSIX

undefine([AC_ISC_POSIX])

AC_DEFUN([AC_ISC_POSIX],
  [
    dnl This test replaces the obsolescent AC_ISC_POSIX kludge.
    dnl GCC LOCAL: Use AC_SEARCH_LIBS.
    AC_SEARCH_LIBS(strerror, cposix)
  ]
)

#serial 2

# Test for the GNU C Library, version 2.1 or newer.
# From Bruno Haible.

AC_DEFUN([jm_GLIBC21],
  [
    AC_CACHE_CHECK(whether we are using the GNU C Library 2.1 or newer,
      ac_cv_gnu_library_2_1,
      [AC_EGREP_CPP([Lucky GNU user],
	[
#include <features.h>
#ifdef __GNU_LIBRARY__
 #if (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 1) || (__GLIBC__ > 2)
  Lucky GNU user
 #endif
#endif
	],
	ac_cv_gnu_library_2_1=yes,
	ac_cv_gnu_library_2_1=no)
      ]
    )
    AC_SUBST(GLIBC21)
    GLIBC21="$ac_cv_gnu_library_2_1"
  ]
)

# Check whether LC_MESSAGES is available in <locale.h>.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU General Public
# License or the GNU Library General Public License but which still want
# to provide support for the GNU gettext functionality.
# Please note that the actual code of the GNU gettext library is covered
# by the GNU Library General Public License, and the rest of the GNU
# gettext package package is covered by the GNU General Public License.
# They are *not* in the public domain.

# serial 2

AC_DEFUN([AM_LC_MESSAGES],
  [if test $ac_cv_header_locale_h = yes; then
    AC_CACHE_CHECK([for LC_MESSAGES], am_cv_val_LC_MESSAGES,
      [AC_TRY_LINK([#include <locale.h>], [return LC_MESSAGES],
       am_cv_val_LC_MESSAGES=yes, am_cv_val_LC_MESSAGES=no)])
    if test $am_cv_val_LC_MESSAGES = yes; then
      AC_DEFINE(HAVE_LC_MESSAGES, 1,
        [Define if your <locale.h> file defines LC_MESSAGES.])
    fi
  fi])

# Search path for a program which passes the given test.
# Ulrich Drepper <drepper@cygnus.com>, 1996.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU General Public
# License or the GNU Library General Public License but which still want
# to provide support for the GNU gettext functionality.
# Please note that the actual code of the GNU gettext library is covered
# by the GNU Library General Public License, and the rest of the GNU
# gettext package package is covered by the GNU General Public License.
# They are *not* in the public domain.

# serial 2

dnl AM_PATH_PROG_WITH_TEST(VARIABLE, PROG-TO-CHECK-FOR,
dnl   TEST-PERFORMED-ON-FOUND_PROGRAM [, VALUE-IF-NOT-FOUND [, PATH]])
AC_DEFUN([AM_PATH_PROG_WITH_TEST],
[# Extract the first word of "$2", so it can be a program name with args.
set dummy $2; ac_word=[$]2
AC_MSG_CHECKING([for $ac_word])
AC_CACHE_VAL(ac_cv_path_$1,
[case "[$]$1" in
  /*)
  ac_cv_path_$1="[$]$1" # Let the user override the test with a path.
  ;;
  *)
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:"
  for ac_dir in ifelse([$5], , $PATH, [$5]); do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$ac_word; then
      if [$3]; then
	ac_cv_path_$1="$ac_dir/$ac_word"
	break
      fi
    fi
  done
  IFS="$ac_save_ifs"
dnl If no 4th arg is given, leave the cache variable unset,
dnl so AC_PATH_PROGS will keep looking.
ifelse([$4], , , [  test -z "[$]ac_cv_path_$1" && ac_cv_path_$1="$4"
])dnl
  ;;
esac])dnl
$1="$ac_cv_path_$1"
if test ifelse([$4], , [-n "[$]$1"], ["[$]$1" != "$4"]); then
  AC_MSG_RESULT([$]$1)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST($1)dnl
])

# Macro to add for using GNU gettext.
# Ulrich Drepper <drepper@cygnus.com>, 1995.
#
# This file can be copied and used freely without restrictions.  It can
# be used in projects which are not available under the GNU General Public
# License or the GNU Library General Public License but which still want
# to provide support for the GNU gettext functionality.
# Please note that the actual code of the GNU gettext library is covered
# by the GNU Library General Public License, and the rest of the GNU
# gettext package package is covered by the GNU General Public License.
# They are *not* in the public domain.

# serial 10

dnl Usage: AM_WITH_NLS([TOOLSYMBOL], [NEEDSYMBOL], [LIBDIR]).
dnl If TOOLSYMBOL is specified and is 'use-libtool', then a libtool library
dnl    $(top_builddir)/intl/libintl.la will be created (shared and/or static,
dnl    depending on --{enable,disable}-{shared,static} and on the presence of
dnl    AM-DISABLE-SHARED). Otherwise, a static library
dnl    $(top_builddir)/intl/libintl.a will be created.
dnl If NEEDSYMBOL is specified and is 'need-ngettext', then GNU gettext
dnl    implementations (in libc or libintl) without the ngettext() function
dnl    will be ignored.
dnl LIBDIR is used to find the intl libraries.  If empty,
dnl    the value `$(top_builddir)/intl/' is used.
dnl
dnl The result of the configuration is one of three cases:
dnl 1) GNU gettext, as included in the intl subdirectory, will be compiled
dnl    and used.
dnl    Catalog format: GNU --> install in $(datadir)
dnl    Catalog extension: .mo after installation, .gmo in source tree
dnl 2) GNU gettext has been found in the system's C library.
dnl    Catalog format: GNU --> install in $(datadir)
dnl    Catalog extension: .mo after installation, .gmo in source tree
dnl 3) No internationalization, always use English msgid.
dnl    Catalog format: none
dnl    Catalog extension: none
dnl The use of .gmo is historical (it was needed to avoid overwriting the
dnl GNU format catalogs when building on a platform with an X/Open gettext),
dnl but we keep it in order not to force irrelevant filename changes on the
dnl maintainers.
dnl
AC_DEFUN([AM_WITH_NLS],
  [AC_MSG_CHECKING([whether NLS is requested])
    dnl Default is enabled NLS
    AC_ARG_ENABLE(nls,
      [  --disable-nls           do not use Native Language Support],
      USE_NLS=$enableval, USE_NLS=yes)
    AC_MSG_RESULT($USE_NLS)
    AC_SUBST(USE_NLS)

    BUILD_INCLUDED_LIBINTL=no
    USE_INCLUDED_LIBINTL=no
dnl GCC LOCAL: Separate concept of link command line from dependencies.
    INTLLIBS=
    INTLDEPS=

    dnl If we use NLS figure out what method
    if test "$USE_NLS" = "yes"; then
      AC_DEFINE(ENABLE_NLS, 1,
        [Define to 1 if translation of program messages to the user's native language
   is requested.])
      AC_MSG_CHECKING([whether included gettext is requested])
      AC_ARG_WITH(included-gettext,
        [  --with-included-gettext use the GNU gettext library included here],
        nls_cv_force_use_gnu_gettext=$withval,
        nls_cv_force_use_gnu_gettext=no)
      AC_MSG_RESULT($nls_cv_force_use_gnu_gettext)

      nls_cv_use_gnu_gettext="$nls_cv_force_use_gnu_gettext"
      if test "$nls_cv_force_use_gnu_gettext" != "yes"; then
        dnl User does not insist on using GNU NLS library.  Figure out what
        dnl to use.  If GNU gettext is available we use this.  Else we have
        dnl to fall back to GNU NLS library.
	CATOBJEXT=NONE

        dnl Add a version number to the cache macros.
        define(gt_cv_func_gnugettext_libc, [gt_cv_func_gnugettext]ifelse([$2], need-ngettext, 2, 1)[_libc])
        define(gt_cv_func_gnugettext_libintl, [gt_cv_func_gnugettext]ifelse([$2], need-ngettext, 2, 1)[_libintl])

dnl GCC LOCAL: Expose presence of libintl.h to C code.
	AC_CHECK_HEADER(libintl.h,
	  [AC_DEFINE([HAVE_LIBINTL_H], 1,
		[Define if you have the <libintl.h> header file.])
           AC_CACHE_CHECK([for GNU gettext in libc], gt_cv_func_gnugettext_libc,
	    [AC_TRY_LINK([#include <libintl.h>
extern int _nl_msg_cat_cntr;],
	       [bindtextdomain ("", "");
return (int) gettext ("")]ifelse([$2], need-ngettext, [ + (int) ngettext ("", "", 0)], [])[ + _nl_msg_cat_cntr],
	       gt_cv_func_gnugettext_libc=yes,
	       gt_cv_func_gnugettext_libc=no)])

	   if test "$gt_cv_func_gnugettext_libc" != "yes"; then
	     AC_CACHE_CHECK([for GNU gettext in libintl],
	       gt_cv_func_gnugettext_libintl,
	       [gt_save_LIBS="$LIBS"
		LIBS="$LIBS -lintl $LIBICONV"
		AC_TRY_LINK([#include <libintl.h>
extern int _nl_msg_cat_cntr;],
		  [bindtextdomain ("", "");
return (int) gettext ("")]ifelse([$2], need-ngettext, [ + (int) ngettext ("", "", 0)], [])[ + _nl_msg_cat_cntr],
		  gt_cv_func_gnugettext_libintl=yes,
		  gt_cv_func_gnugettext_libintl=no)
		LIBS="$gt_save_LIBS"])
	   fi

	   dnl If an already present or preinstalled GNU gettext() is found,
	   dnl use it.  But if this macro is used in GNU gettext, and GNU
	   dnl gettext is already preinstalled in libintl, we update this
	   dnl libintl.  (Cf. the install rule in intl/Makefile.in.)
	   if test "$gt_cv_func_gnugettext_libc" = "yes" \
	      || { test "$gt_cv_func_gnugettext_libintl" = "yes" \
		   && test "$PACKAGE" != gettext; }; then
	     AC_DEFINE(HAVE_GETTEXT, 1,
               [Define if the GNU gettext() function is already present or preinstalled.])

	     if test "$gt_cv_func_gnugettext_libintl" = "yes"; then
	       dnl If iconv() is in a separate libiconv library, then anyone
	       dnl linking with libintl{.a,.so} also needs to link with
	       dnl libiconv.
	       INTLLIBS="-lintl $LIBICONV"
	     fi

	     gt_save_LIBS="$LIBS"
	     LIBS="$LIBS $INTLLIBS"
	     AC_CHECK_FUNCS(dcgettext)
	     LIBS="$gt_save_LIBS"

	     dnl Search for GNU msgfmt in the PATH.
	     AM_PATH_PROG_WITH_TEST(MSGFMT, msgfmt,
	       [$ac_dir/$ac_word --statistics /dev/null >/dev/null 2>&1], :)
	     AC_PATH_PROG(GMSGFMT, gmsgfmt, $MSGFMT)

	     dnl Search for GNU xgettext in the PATH.
	     AM_PATH_PROG_WITH_TEST(XGETTEXT, xgettext,
	       [$ac_dir/$ac_word --omit-header /dev/null >/dev/null 2>&1], :)

	     CATOBJEXT=.gmo
	   fi
	])

        if test "$CATOBJEXT" = "NONE"; then
	  dnl GNU gettext is not found in the C library.
	  dnl Fall back on GNU gettext library.
	  nls_cv_use_gnu_gettext=yes
        fi
      fi

      if test "$nls_cv_use_gnu_gettext" = "yes"; then
        dnl Mark actions used to generate GNU NLS library.
        INTLOBJS="\$(GETTOBJS)"
        AM_PATH_PROG_WITH_TEST(MSGFMT, msgfmt,
	  [$ac_dir/$ac_word --statistics /dev/null >/dev/null 2>&1], :)
        AC_PATH_PROG(GMSGFMT, gmsgfmt, $MSGFMT)
        AM_PATH_PROG_WITH_TEST(XGETTEXT, xgettext,
	  [$ac_dir/$ac_word --omit-header /dev/null >/dev/null 2>&1], :)
        AC_SUBST(MSGFMT)
	BUILD_INCLUDED_LIBINTL=yes
	USE_INCLUDED_LIBINTL=yes
        CATOBJEXT=.gmo
	INTLLIBS="ifelse([$3],[],\$(top_builddir)/intl,[$3])/libintl.ifelse([$1], use-libtool, [l], [])a $LIBICONV"
	INTLDEPS="ifelse([$3],[],\$(top_builddir)/intl,[$3])/libintl.ifelse([$1], use-libtool, [l], [])a"
	LIBS=`echo " $LIBS " | sed -e 's/ -lintl / /' -e 's/^ //' -e 's/ $//'`
      fi

      dnl This could go away some day; the PATH_PROG_WITH_TEST already does it.
      dnl Test whether we really found GNU msgfmt.
      if test "$GMSGFMT" != ":"; then
	dnl If it is no GNU msgfmt we define it as : so that the
	dnl Makefiles still can work.
	if $GMSGFMT --statistics /dev/null >/dev/null 2>&1; then
	  : ;
	else
	  AC_MSG_RESULT(
	    [found msgfmt program is not GNU msgfmt; ignore it])
	  GMSGFMT=":"
	fi
      fi

      dnl This could go away some day; the PATH_PROG_WITH_TEST already does it.
      dnl Test whether we really found GNU xgettext.
      if test "$XGETTEXT" != ":"; then
	dnl If it is no GNU xgettext we define it as : so that the
	dnl Makefiles still can work.
	if $XGETTEXT --omit-header /dev/null >/dev/null 2>&1; then
	  : ;
	else
	  AC_MSG_RESULT(
	    [found xgettext program is not GNU xgettext; ignore it])
	  XGETTEXT=":"
	fi
      fi

      dnl We need to process the po/ directory.
      POSUB=po
    fi
    AC_OUTPUT_COMMANDS(
     [for ac_file in $CONFIG_FILES; do
        # Support "outfile[:infile[:infile...]]"
        case "$ac_file" in
          *:*) ac_file=`echo "$ac_file"|sed 's%:.*%%'` ;;
        esac
        # PO directories have a Makefile.in generated from Makefile.in.in.
        case "$ac_file" in */Makefile.in)
          # Adjust a relative srcdir.
          ac_dir=`echo "$ac_file"|sed 's%/[^/][^/]*$%%'`
          ac_dir_suffix="/`echo "$ac_dir"|sed 's%^\./%%'`"
          ac_dots=`echo "$ac_dir_suffix"|sed 's%/[^/]*%../%g'`
          # In autoconf-2.13 it is called $ac_given_srcdir.
          # In autoconf-2.50 it is called $srcdir.
          test -n "$ac_given_srcdir" || ac_given_srcdir="$srcdir"
          case "$ac_given_srcdir" in
            .)  top_srcdir=`echo $ac_dots|sed 's%/$%%'` ;;
            /*) top_srcdir="$ac_given_srcdir" ;;
            *)  top_srcdir="$ac_dots$ac_given_srcdir" ;;
          esac
          if test -f "$ac_given_srcdir/$ac_dir/POTFILES.in"; then
            rm -f "$ac_dir/POTFILES"
            test -n "$as_me" && echo "$as_me: creating $ac_dir/POTFILES" || echo "creating $ac_dir/POTFILES"
            sed -e "/^#/d" -e "/^[ 	]*\$/d" -e "s,.*,     $top_srcdir/& \\\\," -e "\$s/\(.*\) \\\\/\1/" < "$ac_given_srcdir/$ac_dir/POTFILES.in" > "$ac_dir/POTFILES"
            test -n "$as_me" && echo "$as_me: creating $ac_dir/Makefile" || echo "creating $ac_dir/Makefile"
            sed -e "/POTFILES =/r $ac_dir/POTFILES" "$ac_dir/Makefile.in" > "$ac_dir/Makefile"
          fi
          ;;
        esac
      done])


    dnl If this is used in GNU gettext we have to set BUILD_INCLUDED_LIBINTL
    dnl to 'yes' because some of the testsuite requires it.
    if test "$PACKAGE" = gettext; then
      BUILD_INCLUDED_LIBINTL=yes
    fi

    dnl intl/plural.c is generated from intl/plural.y. It requires bison,
    dnl because plural.y uses bison specific features. It requires at least
    dnl bison-1.26 because earlier versions generate a plural.c that doesn't
    dnl compile.
    dnl bison is only needed for the maintainer (who touches plural.y). But in
    dnl order to avoid separate Makefiles or --enable-maintainer-mode, we put
    dnl the rule in general Makefile. Now, some people carelessly touch the
    dnl files or have a broken "make" program, hence the plural.c rule will
    dnl sometimes fire. To avoid an error, defines BISON to ":" if it is not
    dnl present or too old.
    AC_CHECK_PROGS([INTLBISON], [bison])
    if test -z "$INTLBISON"; then
      ac_verc_fail=yes
    else
      dnl Found it, now check the version.
      AC_MSG_CHECKING([version of bison])
changequote(<<,>>)dnl
      ac_prog_version=`$INTLBISON --version 2>&1 | sed -n 's/^.*GNU Bison.* \([0-9]*\.[0-9.]*\).*$/\1/p'`
      case $ac_prog_version in
        '') ac_prog_version="v. ?.??, bad"; ac_verc_fail=yes;;
        1.2[6-9]* | 1.[3-9][0-9]* | [2-9].*)
changequote([,])dnl
           ac_prog_version="$ac_prog_version, ok"; ac_verc_fail=no;;
        *) ac_prog_version="$ac_prog_version, bad"; ac_verc_fail=yes;;
      esac
      AC_MSG_RESULT([$ac_prog_version])
    fi
    if test $ac_verc_fail = yes; then
      INTLBISON=:
    fi

    dnl GCC LOCAL: GMOFILES/POFILES removed as unnecessary.

    dnl Make all variables we use known to autoconf.
    AC_SUBST(BUILD_INCLUDED_LIBINTL)
    AC_SUBST(USE_INCLUDED_LIBINTL)
    AC_SUBST(CATALOGS)
    AC_SUBST(CATOBJEXT)
    AC_SUBST(INTLLIBS)
    AC_SUBST(INTLDEPS)
    AC_SUBST(INTLOBJS)
    AC_SUBST(POSUB)
dnl GCC LOCAL: Make USE_INCLUDED_LIBINTL visible to C code.
    if test $USE_INCLUDED_LIBINTL = yes; then
      AC_DEFINE([USE_INCLUDED_LIBINTL], 1,
  [Define to use the libintl included with this package instead of any
   version in the system libraries.])
    fi

    dnl For backward compatibility. Some configure.ins may be using this.
    nls_cv_header_intl=
    nls_cv_header_libgt=

    dnl For backward compatibility. Some Makefiles may be using this.
    DATADIRNAME=share
    AC_SUBST(DATADIRNAME)

    dnl For backward compatibility. Some Makefiles may be using this.
    INSTOBJEXT=.mo
    AC_SUBST(INSTOBJEXT)

    dnl For backward compatibility. Some Makefiles may be using this.
    GENCAT=gencat
    AC_SUBST(GENCAT)
  ])

dnl Usage: Just like AM_WITH_NLS, which see.
AC_DEFUN([AM_GNU_GETTEXT],
  [AC_REQUIRE([AC_PROG_MAKE_SET])dnl
   AC_REQUIRE([AC_PROG_CC])dnl
   AC_REQUIRE([AC_CANONICAL_HOST])dnl
   AC_REQUIRE([AC_PROG_RANLIB])dnl
   AC_REQUIRE([AC_ISC_POSIX])dnl
   AC_REQUIRE([AC_HEADER_STDC])dnl
   AC_REQUIRE([AC_C_CONST])dnl
   AC_REQUIRE([AC_C_INLINE])dnl
   AC_REQUIRE([AC_TYPE_OFF_T])dnl
   AC_REQUIRE([AC_TYPE_SIZE_T])dnl
   AC_REQUIRE([AC_FUNC_ALLOCA])dnl
dnl GCC LOCAL: Do not refer to AC_FUNC_MMAP, we have special needs.
dnl   AC_REQUIRE([AC_FUNC_MMAP])dnl
   AC_REQUIRE([jm_GLIBC21])dnl

   AC_CHECK_HEADERS([argz.h limits.h locale.h nl_types.h malloc.h stddef.h \
stdlib.h string.h unistd.h sys/param.h])
   AC_CHECK_FUNCS([feof_unlocked fgets_unlocked getcwd getegid geteuid \
getgid getuid mempcpy munmap putenv setenv setlocale stpcpy strchr strcasecmp \
strdup strtoul tsearch __argz_count __argz_stringify __argz_next])

   AM_ICONV
   AM_LANGINFO_CODESET
   AM_LC_MESSAGES
   AM_WITH_NLS([$1],[$2],[$3])

   dnl GCC LOCAL: The LINGUAS/ALL_LINGUAS/CATALOGS mess that was here
   dnl has been torn out and replaced with this more sensible scheme.
   if test "x$CATOBJEXT" != x; then
     AC_MSG_CHECKING(for catalogs to be installed)
     # Look for .po and .gmo files in the source directory.
     CATALOGS=
     XLINGUAS=
     for cat in $srcdir/po/*$CATOBJEXT $srcdir/po/*.po; do
	# If there aren't any .gmo files the shell will give us the
	# literal string "../path/to/srcdir/po/*.gmo" which has to be
	# weeded out.
	case "$cat" in *\**)
	    continue;;
	esac
	# The quadruple backslash is collapsed to a double backslash
	# by the backticks, then collapsed again by the double quotes,
	# leaving us with one backslash in the sed expression (right
	# before the dot that mustn't act as a wildcard).  The dot to
	# be escaped in the second expression is hiding inside CATOBJEXT.
	cat=`echo $cat | sed -e "s!$srcdir/!!" -e "s!\\\\.po!$CATOBJEXT!"`
	lang=`echo $cat | sed -e 's!po/!!' -e "s!\\\\$CATOBJEXT!!"`
	# The user is allowed to set LINGUAS to a list of languages to
	# install catalogs for.  If it's empty that means "all of them."
	if test "x$LINGUAS" = x; then
	    CATALOGS="$CATALOGS $cat"
	    XLINGUAS="$XLINGUAS $lang"
	else
	  case "$LINGUAS" in *$lang*)
	    CATALOGS="$CATALOGS $cat"
	    XLINGUAS="$XLINGUAS $lang"
	    ;;
	  esac
	fi
     done
     LINGUAS="$XLINGUAS"
     AC_MSG_RESULT($LINGUAS)
   fi

   dnl If the AC_CONFIG_AUX_DIR macro for autoconf is used we possibly
   dnl find the mkinstalldirs script in another subdir but $(top_srcdir).
   dnl Try to locate is.
   MKINSTALLDIRS=
   if test -n "$ac_aux_dir"; then
     MKINSTALLDIRS="$ac_aux_dir/mkinstalldirs"
   fi
   if test -z "$MKINSTALLDIRS"; then
     MKINSTALLDIRS="\$(top_srcdir)/mkinstalldirs"
   fi
   AC_SUBST(MKINSTALLDIRS)

   dnl Enable libtool support if the surrounding package wishes it.
   INTL_LIBTOOL_SUFFIX_PREFIX=ifelse([$1], use-libtool, [l], [])
   AC_SUBST(INTL_LIBTOOL_SUFFIX_PREFIX)
  ])

AC_DEFUN(gcc_AC_INITFINI_ARRAY,
[AC_CACHE_CHECK(for .preinit_array/.init_array/.fini_array support,
		 gcc_cv_initfinit_array, [dnl
  cat > conftest.c <<EOF
static int x = -1;
int main (void) { return x; }
int foo (void) { x = 0; }
int (*fp) (void) __attribute__ ((section (".init_array"))) = foo;
EOF
  if AC_TRY_COMMAND([${CC-cc} -o conftest conftest.c 1>&AS_MESSAGE_LOG_FD])
  then
    if ./conftest; then
      gcc_cv_initfinit_array=yes
    else
      gcc_cv_initfinit_array=no
    fi
  else
    gcc_cv_initfinit_array=no
  fi
  rm -f conftest*])
  AC_SUBST(gcc_cv_initfinit_array)
  if test $gcc_cv_initfinit_array = yes; then
    AC_DEFINE(HAVE_INITFINI_ARRAY, 1,
      [Define .init_array/.fini_array sections are available and working.])
  fi])
