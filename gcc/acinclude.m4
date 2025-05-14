dnl Copyright (C) 2005-2025 Free Software Foundation, Inc.
dnl
dnl This file is part of GCC.
dnl
dnl GCC is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl GCC is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with GCC; see the file COPYING3.  If not see
dnl <http://www.gnu.org/licenses/>.

dnl See whether we need a declaration for a function.
dnl The result is highly dependent on the INCLUDES passed in, so make sure
dnl to use a different cache variable name in this macro if it is invoked
dnl in a different context somewhere else.
dnl gcc_AC_CHECK_DECL(SYMBOL,
dnl 	[ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND [, INCLUDES]]])
AC_DEFUN([gcc_AC_CHECK_DECL],
[AC_MSG_CHECKING([whether $1 is declared])
AC_CACHE_VAL(gcc_cv_have_decl_$1,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([$4],
[#ifndef $1
char *(*pfn) = (char *(*)) $1 ;
#endif])], eval "gcc_cv_have_decl_$1=yes", eval "gcc_cv_have_decl_$1=no")])
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
AC_DEFUN([gcc_AC_CHECK_DECLS],
[AC_FOREACH([gcc_AC_Func], [$1],
  [AH_TEMPLATE(AS_TR_CPP(HAVE_DECL_[]gcc_AC_Func),
  [Define to 1 if we found a declaration for ']gcc_AC_Func[', otherwise
   define to 0.])])dnl
for ac_func in $1
do
  ac_tr_decl=AS_TR_CPP([HAVE_DECL_$ac_func])
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
])

dnl 'make compare' can be significantly faster, if cmp itself can
dnl skip bytes instead of using tail.  The test being performed is
dnl "if cmp --ignore-initial=2 t1 t2 && ! cmp --ignore-initial=1 t1 t2"
dnl but we need to sink errors and handle broken shells.  We also test
dnl for the parameter format "cmp file1 file2 skip1 skip2" which is
dnl accepted by cmp on some systems.
AC_DEFUN([gcc_AC_PROG_CMP_IGNORE_INITIAL],
[AC_CACHE_CHECK([for cmp's capabilities], gcc_cv_prog_cmp_skip,
[ echo abfoo >t1
  echo cdfoo >t2
  gcc_cv_prog_cmp_skip=slowcompare
  if cmp --ignore-initial=2 t1 t2 > /dev/null 2>&1; then
    if cmp --ignore-initial=1 t1 t2 > /dev/null 2>&1; then
      :
    else
      gcc_cv_prog_cmp_skip=gnucompare
    fi
  fi
  if test $gcc_cv_prog_cmp_skip = slowcompare ; then
    if cmp t1 t2 2 2 > /dev/null 2>&1; then
      if cmp t1 t2 1 1 > /dev/null 2>&1; then
        :
      else
        gcc_cv_prog_cmp_skip=fastcompare
      fi
    fi
  fi
  rm t1 t2
])
make_compare_target=$gcc_cv_prog_cmp_skip
AC_SUBST(make_compare_target)
])

dnl See if symbolic links work and if not, try to substitute either hard links or simple copy.
AC_DEFUN([gcc_AC_PROG_LN_S],
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
    if cp -p conftestdata_f conftestdata_t 2>/dev/null
    then
      gcc_cv_prog_LN_S="cp -p"
    else
      gcc_cv_prog_LN_S=cp
    fi
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
    AC_MSG_RESULT([no, and neither does ln, so using $gcc_cv_prog_LN_S])
  fi
fi
AC_SUBST(LN_S)dnl
])

dnl Define MKDIR_TAKES_ONE_ARG if mkdir accepts only one argument instead
dnl of the usual 2.
AC_DEFUN([gcc_AC_FUNC_MKDIR_TAKES_ONE_ARG],
[AC_CACHE_CHECK([if mkdir takes one argument], gcc_cv_mkdir_takes_one_arg,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
#include <sys/types.h>
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>
#endif], [mkdir ("foo", 0);])],
        gcc_cv_mkdir_takes_one_arg=no, gcc_cv_mkdir_takes_one_arg=yes)])
if test $gcc_cv_mkdir_takes_one_arg = yes ; then
  AC_DEFINE(MKDIR_TAKES_ONE_ARG, 1, [Define if host mkdir takes a single argument.])
fi
])

AC_DEFUN([gcc_AC_PROG_INSTALL],
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

dnl Determine if enumerated bitfields are unsigned.   ISO C says they can 
dnl be either signed or unsigned.
dnl
AC_DEFUN([gcc_AC_C_ENUM_BF_UNSIGNED],
[AC_CACHE_CHECK(for unsigned enumerated bitfields, gcc_cv_enum_bf_unsigned,
[AC_RUN_IFELSE([AC_LANG_SOURCE([#include <stdlib.h>
enum t { BLAH = 128 } ;
struct s_t { enum t member : 8; } s ;
int main(void)
{            
        s.member = BLAH;
        if (s.member < 0) exit(1);
        exit(0);

}])], gcc_cv_enum_bf_unsigned=yes, gcc_cv_enum_bf_unsigned=no, gcc_cv_enum_bf_unsigned=yes)])
if test $gcc_cv_enum_bf_unsigned = yes; then
  AC_DEFINE(ENUM_BITFIELDS_ARE_UNSIGNED, 1,
    [Define if enumerated bitfields are treated as unsigned values.])
fi])

dnl Probe number of bits in a byte.
dnl Note C89 requires CHAR_BIT >= 8.
dnl
AC_DEFUN([gcc_AC_C_CHAR_BIT],
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
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,
     [switch(0) {
  case (unsigned char)((unsigned long)1 << $i) == ((unsigned long)1 << $i):
  case (unsigned char)((unsigned long)1<<($i-1)) == ((unsigned long)1<<($i-1)):
  ; }])],
     [gcc_cv_c_nbby=$i; break])
   i=`expr $i + 1`
 done
 test -z "$gcc_cv_c_nbby" && gcc_cv_c_nbby=failed
])
if test $gcc_cv_c_nbby = failed; then
  AC_MSG_ERROR(cannot determine number of bits in a byte)
else
  AC_DEFINE_UNQUOTED(CHAR_BIT, $gcc_cv_c_nbby,
  [Define as the number of bits in a byte, if `limits.h' doesn't.])
fi
fi])

AC_DEFUN([gcc_AC_INITFINI_ARRAY],
[AC_ARG_ENABLE(initfini-array,
	[  --enable-initfini-array	use .init_array/.fini_array sections],
	[], [
AC_CACHE_CHECK(for .preinit_array/.init_array/.fini_array support,
		 gcc_cv_initfini_array, [dnl
  if test "x${build}" = "x${target}" && test "x${build}" = "x${host}"; then
    case "${target}" in
      ia64-*)
	AC_RUN_IFELSE([AC_LANG_SOURCE([
#ifndef __ELF__
#error Not an ELF OS
#endif
/* We turn on .preinit_array/.init_array/.fini_array support for ia64
   if it can be used.  */
static int x = -1;
int main (void) { return x; }
int foo (void) { x = 0; }
int (*fp) (void) __attribute__ ((section (".init_array"))) = foo;
])],
	     [gcc_cv_initfini_array=yes], [gcc_cv_initfini_array=no],
	     [gcc_cv_initfini_array=no]);;
      *)
	gcc_cv_initfini_array=no
	if test $in_tree_ld = yes ; then
	  if test "$gcc_cv_gld_major_version" -eq 2 \
	     -a "$gcc_cv_gld_minor_version" -ge 22 \
	     -o "$gcc_cv_gld_major_version" -gt 2 \
	     && test $in_tree_ld_is_elf = yes; then
	    gcc_cv_initfini_array=yes
	  fi
	elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x -a x$gcc_cv_objdump != x ; then
	  case $target:$gas in
	    *:yes)
	      sh_flags='"a"'
	      sh_type='%progbits'
	      ;;
	    i?86-*-solaris2*:no | x86_64-*-solaris2*:no)
	      sh_flags='"a"'
	      sh_type='@progbits'
	      ;;
	    sparc*-*-solaris2*:no)
	      sh_flags='#alloc'
	      sh_type='#progbits'
	      sh_quote='"'
	      ;;
	  esac
	  case "$target:$gnu_ld" in
	    *:yes)
	      cat > conftest.s <<EOF
.section .dtors,$sh_flags,$sh_type
.balign 4
.byte 'A', 'A', 'A', 'A'
.section .ctors,$sh_flags,$sh_type
.balign 4
.byte 'B', 'B', 'B', 'B'
.section .fini_array.65530,$sh_flags,$sh_type
.balign 4
.byte 'C', 'C', 'C', 'C'
.section .init_array.65530,$sh_flags,$sh_type
.balign 4
.byte 'D', 'D', 'D', 'D'
.section .dtors.64528,$sh_flags,$sh_type
.balign 4
.byte 'E', 'E', 'E', 'E'
.section .ctors.64528,$sh_flags,$sh_type
.balign 4
.byte 'F', 'F', 'F', 'F'
.section .fini_array.01005,$sh_flags,$sh_type
.balign 4
.byte 'G', 'G', 'G', 'G'
.section .init_array.01005,$sh_flags,$sh_type
.balign 4
.byte 'H', 'H', 'H', 'H'
.text
.globl _start
_start:
EOF
	      if $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1 \
	         && $gcc_cv_ld -o conftest conftest.o > /dev/null 2>&1 \
	         && $gcc_cv_objdump -s -j .init_array conftest \
		    | grep HHHHFFFFDDDDBBBB > /dev/null 2>&1 \
	         && $gcc_cv_objdump -s -j .fini_array conftest \
		    | grep GGGGEEEECCCCAAAA > /dev/null 2>&1; then
	        gcc_cv_initfini_array=yes
	      fi
	      ;;
	    *-*-solaris2*:no)
	      # When Solaris ld added constructor priority support, it was
	      # decided to only handle .init_array.N/.fini_array.N since
	      # there was no need for backwards compatibility with
	      # .ctors.N/.dtors.N.  .ctors/.dtors remain as separate
	      # sections with correct execution order resp. to
	      # .init_array/.fini_array, while gld merges them into
	      # .init_array/.fini_array.
	      cat > conftest.s <<EOF
.section $sh_quote.fini_array.65530$sh_quote,$sh_flags,$sh_type
.align 4
.byte 'C', 'C', 'C', 'C', 'C', 'C', 'C', 'C'
.section $sh_quote.init_array.65530$sh_quote,$sh_flags,$sh_type
.align 4
.byte 'D', 'D', 'D', 'D', 'D', 'D', 'D', 'D'
.section $sh_quote.fini_array.01005$sh_quote,$sh_flags,$sh_type
.align 4
.byte 'G', 'G', 'G', 'G', 'G', 'G', 'G', 'G'
.section $sh_quote.init_array.01005$sh_quote,$sh_flags,$sh_type
.align 4
.byte 'H', 'H', 'H', 'H', 'H', 'H', 'H', 'H'
.text
.globl _start
_start:
EOF
	      if $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1 \
	         && $gcc_cv_ld -o conftest conftest.o > /dev/null 2>&1 \
	         && $gcc_cv_objdump -s -j .init_array conftest \
		    | grep HHHHHHHHDDDDDDDD > /dev/null 2>&1 \
	         && $gcc_cv_objdump -s -j .fini_array conftest \
		    | grep GGGGGGGGCCCCCCCC > /dev/null 2>&1; then
	        gcc_cv_initfini_array=yes
	      fi
	      ;;
	    esac
changequote(,)dnl
	  rm -f conftest conftest.*
changequote([,])dnl
	fi
	AC_PREPROC_IFELSE([AC_LANG_SOURCE([
#ifndef __ELF__
# error Not an ELF OS
#endif
#include <stdlib.h>
#if defined __GLIBC_PREREQ
# if __GLIBC_PREREQ (2, 4)
# else
#  error GLIBC 2.4 required
# endif
#else
# if defined __sun__ && defined __svr4__
   /* Solaris ld.so.1 supports .init_array/.fini_array since Solaris 8.  */
# else
#  error The C library not known to support .init_array/.fini_array
# endif
#endif
])],, [gcc_cv_initfini_array=no]);;
    esac
  else
    case "${target}" in
      aarch64*-linux-gnu*)
	# AArch64 postdates glibc support for .init_array/.fini_array,
	# so we don't need the preprocessor test above.
	gcc_cv_initfini_array=yes
	;;

      *)
	AC_MSG_CHECKING(cross compile... guessing)
	gcc_cv_initfini_array=no
	;;
    esac
  fi])
  enable_initfini_array=$gcc_cv_initfini_array
])
AC_DEFINE_UNQUOTED(HAVE_INITFINI_ARRAY_SUPPORT,
  [`if test $enable_initfini_array = yes; then echo 1; else echo 0; fi`],
  [Define 0/1 if .init_array/.fini_array sections are available and working.])
])

dnl Check whether the host supports symbol aliases.
AC_DEFUN([gcc_CHECK_ATTRIBUTE_ALIAS], [
  AC_CACHE_CHECK([whether the host/build supports symbol aliases],
                 gcc_cv_have_attribute_alias, [
  if test "x${build}" = "x${host}"; then
    AC_TRY_LINK([
extern "C" void foo(void) { }
extern void bar(void) __attribute__((alias("foo")));],
    [bar();], gcc_cv_have_attribute_alias=yes, gcc_cv_have_attribute_alias=no)
  else
    gcc_cv_have_attribute_alias=no
  fi])
  if test $gcc_cv_have_attribute_alias = yes; then
    AC_DEFINE(HAVE_ATTRIBUTE_ALIAS, 1,
      [Define to 1 if the host/build supports __attribute__((alias(...))).])
  fi])

dnl # gcc_GAS_FLAGS
dnl # Used by gcc_GAS_CHECK_FEATURE 
dnl #
AC_DEFUN([gcc_GAS_FLAGS],
[AC_CACHE_CHECK([assembler flags], gcc_cv_as_flags,
[ case "$target" in
  i[[34567]]86-*-linux*)
    dnl Override the default, which may be incompatible.
    gcc_cv_as_flags=--32
    ;;
  x86_64-*-linux-gnux32)
    dnl Override the default, which may be incompatible.
    gcc_cv_as_flags=--x32
    ;;
  x86_64-*-linux*)
    dnl Override the default, which may be incompatible.
    gcc_cv_as_flags=--64
    ;;
  powerpc*-*-darwin*)
    dnl Always pass -arch ppc to assembler.
    gcc_cv_as_flags="-arch ppc"
    ;;
  *)
    gcc_cv_as_flags=" "
    ;;
  esac])
])

dnl gcc_GAS_CHECK_FEATURE(description, cv,
dnl [extra switches to as], [assembler input],
dnl [extra testing logic], [command if feature available])
dnl
dnl Checks for an assembler feature.
dnl ASSEMBLER INPUT is fed to the assembler and the feature is available
dnl if assembly succeeds.  If EXTRA TESTING LOGIC is not the empty string,
dnl then it is run instead of simply setting CV to "yes" - it is responsible
dnl for doing so, if appropriate.
AC_DEFUN([gcc_GAS_CHECK_FEATURE],
[AC_REQUIRE([gcc_GAS_FLAGS])dnl
AC_CACHE_CHECK([assembler for $1], [$2],
 [[$2]=no
  if test x$gcc_cv_as != x; then
    AS_ECHO([ifelse(m4_substr([$4],0,1),[$], "[$4]", '[$4]')]) > conftest.s
    if AC_TRY_COMMAND([$gcc_cv_as $gcc_cv_as_flags $3 -o conftest.o conftest.s >&AS_MESSAGE_LOG_FD])
    then
	ifelse([$5],, [$2]=yes, [$5])
    else
      echo "configure: failed program was" >&AS_MESSAGE_LOG_FD
      cat conftest.s >&AS_MESSAGE_LOG_FD
    fi
    rm -f conftest.o conftest.s
  fi])
ifelse([$6],,,[dnl
if test $[$2] = yes; then
  $6
fi])
ifelse([$7],,,[dnl
if test $[$2] != yes; then
  $7
fi])])

dnl GCC_TARGET_TEMPLATE(KEY)
dnl ------------------------
dnl Define KEY as a valid configure key on the target machine.

m4_define([GCC_TARGET_TEMPLATE],
[m4_define([GCC_TARGET_TEMPLATE($1)],[])])

dnl AH_TEMPLATE(KEY, DESCRIPTION)
dnl -----------------------------
dnl Issue an autoheader template for KEY, i.e., a comment composed of
dnl DESCRIPTION (properly wrapped), and then #undef KEY.  Redefinition
dnl of the macro in autoheader.m4, to support definition of only a few
dnl keys while compiling target libraries.

m4_define([AH_TEMPLATE],
[AH_VERBATIM([$1],m4_text_wrap([$2 */], [   ], [/* ])
m4_ifdef([GCC_TARGET_TEMPLATE($1)],[],[#ifndef USED_FOR_TARGET
])[#undef $1]m4_ifdef([GCC_TARGET_TEMPLATE($1)],[],[
#endif
]))])

dnl Make sure that build_exeext is looked for
AC_DEFUN([gcc_AC_BUILD_EXEEXT], [
ac_executable_extensions="$build_exeext"])

dnl GCC_GLIBC_VERSION_GTE_IFELSE(MAJOR, MINOR, IF-TRUE, IF-FALSE)
dnl -------------------------------------------------------------
dnl If the target glibc version ($glibc_version_major.$glibc_version_minor)
dnl is at least MAJOR.MINOR, call IF-TRUE, otherwise call IF-FALSE.
AC_DEFUN([GCC_GLIBC_VERSION_GTE_IFELSE],
[
AS_IF([test $glibc_version_major -gt $1 \
  || ( test $glibc_version_major -eq $1 && test $glibc_version_minor -ge $2 )],
[$3], [$4])])
