# configure.ac for GCC
# Process this file with autoconf to generate a configuration script.

# Copyright 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
# Free Software Foundation, Inc.

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 2, or (at your option) any later
#version.

#GCC is distributed in the hope that it will be useful, but WITHOUT
#ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING.  If not, write to the Free
#Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#02111-1307, USA.

# --------------------------------
# Initialization and sanity checks
# --------------------------------

AC_PREREQ(2.57)
AC_INIT
AC_CONFIG_SRCDIR(tree.c)
AC_CONFIG_HEADER(auto-host.h:config.in)

# Determine the host, build, and target systems
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Determine the noncanonical target name, for directory use.
_GCC_TOPLEV_NONCANONICAL_TARGET

# Determine the target- and build-specific subdirectories
GCC_TOPLEV_SUBDIRS

# Set program_transform_name
AC_ARG_PROGRAM

# Check for bogus environment variables.
# Test if LIBRARY_PATH contains the notation for the current directory
# since this would lead to problems installing/building glibc.
# LIBRARY_PATH contains the current directory if one of the following
# is true:
# - one of the terminals (":" and ";") is the first or last sign
# - two terminals occur directly after each other
# - the path contains an element with a dot in it
AC_MSG_CHECKING(LIBRARY_PATH variable)
changequote(,)dnl
case ${LIBRARY_PATH} in
  [:\;]* | *[:\;] | *[:\;][:\;]* |  *[:\;]. | .[:\;]*| . | *[:\;].[:\;]* )
    library_path_setting="contains current directory"
    ;;
  *)
    library_path_setting="ok"
    ;;
esac
changequote([,])dnl
AC_MSG_RESULT($library_path_setting)
if test "$library_path_setting" != "ok"; then
AC_MSG_ERROR([
*** LIBRARY_PATH shouldn't contain the current directory when
*** building gcc. Please change the environment variable
*** and run configure again.])
fi

# Test if GCC_EXEC_PREFIX contains the notation for the current directory
# since this would lead to problems installing/building glibc.
# GCC_EXEC_PREFIX contains the current directory if one of the following
# is true:
# - one of the terminals (":" and ";") is the first or last sign
# - two terminals occur directly after each other
# - the path contains an element with a dot in it
AC_MSG_CHECKING(GCC_EXEC_PREFIX variable)
changequote(,)dnl
case ${GCC_EXEC_PREFIX} in
  [:\;]* | *[:\;] | *[:\;][:\;]* |  *[:\;]. | .[:\;]*| . | *[:\;].[:\;]* )
    gcc_exec_prefix_setting="contains current directory"
    ;;
  *)
    gcc_exec_prefix_setting="ok"
    ;;
esac
changequote([,])dnl
AC_MSG_RESULT($gcc_exec_prefix_setting)
if test "$gcc_exec_prefix_setting" != "ok"; then
AC_MSG_ERROR([
*** GCC_EXEC_PREFIX shouldn't contain the current directory when
*** building gcc. Please change the environment variable
*** and run configure again.])
fi

# -----------
# Directories
# -----------

# Specify the local prefix
local_prefix=
AC_ARG_WITH(local-prefix,
[  --with-local-prefix=DIR specifies directory to put local include],
[case "${withval}" in
yes)	AC_MSG_ERROR(bad value ${withval} given for local include directory prefix) ;;
no)	;;
*)	local_prefix=$with_local_prefix ;;
esac])

# Default local prefix if it is empty
if test x$local_prefix = x; then
	local_prefix=/usr/local
fi

# Don't set gcc_gxx_include_dir to gxx_include_dir since that's only
# passed in by the toplevel make and thus we'd get different behavior
# depending on where we built the sources.
gcc_gxx_include_dir=
# Specify the g++ header file directory
AC_ARG_WITH(gxx-include-dir,
[  --with-gxx-include-dir=DIR
                          specifies directory to put g++ header files],
[case "${withval}" in
yes)	AC_MSG_ERROR(bad value ${withval} given for g++ include directory) ;;
no)	;;
*)	gcc_gxx_include_dir=$with_gxx_include_dir ;;
esac])

if test x${gcc_gxx_include_dir} = x; then
  if test x${enable_version_specific_runtime_libs} = xyes; then
    gcc_gxx_include_dir='${libsubdir}/include/c++'
  else
    topsrcdir=${srcdir}/.. . ${srcdir}/../config.if
changequote(<<, >>)dnl
    gcc_gxx_include_dir="\$(libsubdir)/\$(unlibsubdir)/..\`echo \$(exec_prefix) | sed -e 's|^\$(prefix)||' -e 's|/[^/]*|/..|g'\`/include/"${libstdcxx_incdir}
changequote([, ])dnl
  fi
fi

AC_ARG_WITH(cpp_install_dir,
[  --with-cpp-install-dir=DIR
                          install the user visible C preprocessor in DIR
                          (relative to PREFIX) as well as PREFIX/bin],
[if test x$withval = xyes; then
  AC_MSG_ERROR([option --with-cpp-install-dir requires an argument])
elif test x$withval != xno; then
  cpp_install_dir=$withval
fi])

# We would like to our source tree to be readonly.  However when releases or
# pre-releases are generated, the flex/bison generated files as well as the 
# various formats of manuals need to be included along with the rest of the
# sources.  Therefore we have --enable-generated-files-in-srcdir to do 
# just that.

AC_MSG_CHECKING([whether to place generated files in the source directory])
  dnl generated-files-in-srcdir is disabled by default
  AC_ARG_ENABLE(generated-files-in-srcdir, 
[  --enable-generated-files-in-srcdir
                          put copies of generated files in source dir
                          intended for creating source tarballs for users
                          without texinfo bison or flex.],
      generated_files_in_srcdir=$enableval,
      generated_files_in_srcdir=no)

AC_MSG_RESULT($generated_files_in_srcdir)

if test "$generated_files_in_srcdir" = "yes"; then
  GENINSRC=''
else
  GENINSRC='#'
fi
AC_SUBST(GENINSRC)

# -------------------
# Find default linker
# -------------------

# With GNU ld
AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           arrange to work with GNU ld.],
gnu_ld_flag="$with_gnu_ld",
gnu_ld_flag=no)

# With pre-defined ld
AC_ARG_WITH(ld,
[  --with-ld               arrange to use the specified ld (full pathname)],
DEFAULT_LINKER="$with_ld")
if test x"${DEFAULT_LINKER+set}" = x"set"; then
  if test ! -x "$DEFAULT_LINKER"; then
    AC_MSG_WARN([cannot execute: $DEFAULT_LINKER: check --with-ld or env. var. DEFAULT_LINKER])
  elif $DEFAULT_LINKER -v < /dev/null 2>&1 | grep GNU > /dev/null; then
    gnu_ld_flag=yes
  fi
  AC_DEFINE_UNQUOTED(DEFAULT_LINKER,"$DEFAULT_LINKER",
	[Define to enable the use of a default linker.])
fi

AC_MSG_CHECKING([whether a default linker was specified])
if test x"${DEFAULT_LINKER+set}" = x"set"; then
  if test x"$gnu_ld_flag" = x"no"; then
    AC_MSG_RESULT([yes ($DEFAULT_LINKER)])
  else
    AC_MSG_RESULT([yes ($DEFAULT_LINKER - GNU ld)])
  fi
else
  AC_MSG_RESULT(no)
fi

# ----------------------
# Find default assembler
# ----------------------

# With GNU as
AC_ARG_WITH(gnu-as,
[  --with-gnu-as           arrange to work with GNU as],
gas_flag="$with_gnu_as",
gas_flag=no)

AC_ARG_WITH(as,
[  --with-as               arrange to use the specified as (full pathname)],
DEFAULT_ASSEMBLER="$with_as")
if test x"${DEFAULT_ASSEMBLER+set}" = x"set"; then
  if test ! -x "$DEFAULT_ASSEMBLER"; then
    AC_MSG_WARN([cannot execute: $DEFAULT_ASSEMBLER: check --with-as or env. var. DEFAULT_ASSEMBLER])
  elif $DEFAULT_ASSEMBLER -v < /dev/null 2>&1 | grep GNU > /dev/null; then
    gas_flag=yes
  fi
  AC_DEFINE_UNQUOTED(DEFAULT_ASSEMBLER,"$DEFAULT_ASSEMBLER",
	[Define to enable the use of a default assembler.])
fi

AC_MSG_CHECKING([whether a default assembler was specified])
if test x"${DEFAULT_ASSEMBLER+set}" = x"set"; then
  if test x"$gas_flag" = x"no"; then
    AC_MSG_RESULT([yes ($DEFAULT_ASSEMBLER)])
  else
    AC_MSG_RESULT([yes ($DEFAULT_ASSEMBLER - GNU as)])
  fi
else
  AC_MSG_RESULT(no)
fi

# ---------------
# Find C compiler
# ---------------

# If a non-executable a.out is present (e.g. created by GNU as above even if
# invoked with -v only), the IRIX 6 native ld just overwrites the existing
# file, even when creating an executable, so an execution test fails.
# Remove possible default executable files to avoid this.
#
# FIXME: This really belongs into AC_PROG_CC and can be removed once
# Autoconf includes it.
rm -f a.out a.exe b.out

# Find the native compiler
AC_PROG_CC
AC_PROG_CC_C_O
# autoconf is lame and doesn't give us any substitution variable for this.
if eval "test \"`echo '$ac_cv_prog_cc_'${ac_cc}_c_o`\" = no"; then
  NO_MINUS_C_MINUS_O=yes
else
  OUTPUT_OPTION='-o $@'
fi
AC_SUBST(NO_MINUS_C_MINUS_O)
AC_SUBST(OUTPUT_OPTION)

# -------------------------
# Check C compiler features
# -------------------------

AC_CACHE_CHECK(whether ${CC-cc} accepts -Wno-long-long,
ac_cv_prog_cc_no_long_long,
[save_CFLAGS="$CFLAGS"
CFLAGS="-Wno-long-long"
AC_TRY_COMPILE(,,ac_cv_prog_cc_no_long_long=yes,
	       ac_cv_prog_cc_no_long_long=no)
CFLAGS="$save_CFLAGS"])

AC_PROG_CPP
AC_C_INLINE

gcc_AC_C_LONG_LONG
gcc_AC_C__BOOL

# sizeof(char) is 1 by definition.
AC_COMPILE_CHECK_SIZEOF(void *)
AC_COMPILE_CHECK_SIZEOF(short)
AC_COMPILE_CHECK_SIZEOF(int)
AC_COMPILE_CHECK_SIZEOF(long)
if test $ac_cv_c_long_long = yes; then
  AC_COMPILE_CHECK_SIZEOF(long long)
fi
if test $ac_cv_c___int64 = yes; then
  AC_COMPILE_CHECK_SIZEOF(__int64)
fi

# -----------------
# Find Ada compiler
# -----------------

# See if GNAT has been installed
gcc_AC_PROG_GNAT

# ---------------------
# Warnings and checking
# ---------------------

strict1_warn=
if test $ac_cv_prog_cc_no_long_long = yes ; then
  strict1_warn="-pedantic -Wno-long-long"
fi
AC_SUBST(strict1_warn)

# If the native compiler is GCC, we can enable warnings even in stage1.  
# That's useful for people building cross-compilers, or just running a
# quick `make'.
warn_cflags=
if test "x$GCC" = "xyes"; then
  warn_cflags='$(GCC_WARN_CFLAGS)'
fi
AC_SUBST(warn_cflags)

# Enable -Werror in bootstrap stage2 and later.
# Change the default to "no" on release branches.
AC_ARG_ENABLE(werror, 
[  --enable-werror         enable -Werror in bootstrap stage2 and later], [],
[enable_werror=no])
if test x$enable_werror = xyes ; then
  WERROR=-Werror
fi
AC_SUBST(WERROR)

# Enable expensive internal checks
AC_ARG_ENABLE(checking,
[  --enable-checking[=LIST]
			  enable expensive run-time checks.  With LIST,
			  enable only specific categories of checks.
			  Categories are: misc,tree,rtl,rtlflag,gc,gcac,fold;
			  default is no checking],
[ac_checking=
ac_tree_checking=
ac_rtl_checking=
ac_rtlflag_checking=
ac_gc_checking=
ac_gc_always_collect=
ac_fold_checking=
case "${enableval}" in
yes)	ac_checking=1 ; ac_tree_checking=1 ; ac_gc_checking=1 ;
	ac_rtlflag_checking=1 ;;
no)	;;
*)	IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
	set fnord $enableval; shift
	IFS="$ac_save_IFS"
	for check
	do
		case $check in
		misc)	ac_checking=1 ;;
		tree)	ac_tree_checking=1 ;;
		rtlflag)	ac_rtlflag_checking=1 ;;
		rtl)	ac_rtl_checking=1 ;;
		gc)	ac_gc_checking=1 ;;
		gcac)	ac_gc_always_collect=1 ;;
		fold)	ac_fold_checking=1 ;;
		valgrind)	ac_checking_valgrind=1 ;;
		*)	AC_MSG_ERROR(unknown check category $check) ;;
		esac
	done
	;;
esac
], 
# By default, disable all checks for release versions of GCC.
[ac_checking=; ac_tree_checking=; ac_gc_checking=; ac_rtlflag_checking=;])
nocommon_flag=""
if test x$ac_checking != x ; then
  AC_DEFINE(ENABLE_CHECKING, 1,
[Define if you want more run-time sanity checks.  This one gets a grab
   bag of miscellaneous but relatively cheap checks.])
  nocommon_flag=-fno-common
fi
AC_SUBST(nocommon_flag)
if test x$ac_tree_checking != x ; then
  AC_DEFINE(ENABLE_TREE_CHECKING, 1,
[Define if you want all operations on trees (the basic data
   structure of the front ends) to be checked for dynamic type safety
   at runtime.  This is moderately expensive.])
fi
if test x$ac_rtl_checking != x ; then
  AC_DEFINE(ENABLE_RTL_CHECKING, 1,
[Define if you want all operations on RTL (the basic data structure
   of the optimizer and back end) to be checked for dynamic type safety
   at runtime.  This is quite expensive.])
fi
if test x$ac_rtlflag_checking != x ; then
  AC_DEFINE(ENABLE_RTL_FLAG_CHECKING, 1,
[Define if you want RTL flag accesses to be checked against the RTL
   codes that are supported for each access macro.  This is relatively
   cheap.])
fi
if test x$ac_gc_checking != x ; then
  AC_DEFINE(ENABLE_GC_CHECKING, 1,
[Define if you want the garbage collector to do object poisoning and
   other memory allocation checks.  This is quite expensive.])
fi
if test x$ac_gc_always_collect != x ; then
  AC_DEFINE(ENABLE_GC_ALWAYS_COLLECT, 1,
[Define if you want the garbage collector to operate in maximally
   paranoid mode, validating the entire heap and collecting garbage at
   every opportunity.  This is extremely expensive.])
fi
if test x$ac_fold_checking != x ; then
  AC_DEFINE(ENABLE_FOLD_CHECKING, 1,
[Define if you want fold checked that it never destructs its argument.
   This is quite expensive.])
fi
valgrind_path_defines=
valgrind_command=

dnl # This check AC_REQUIREs various stuff, so it *must not* be inside
dnl # an if statement.  This was the source of very frustrating bugs
dnl # in converting to autoconf 2.5x!
AC_CHECK_HEADER(valgrind.h, have_valgrind_h=yes, have_valgrind_h=no)

if test x$ac_checking_valgrind != x ; then
  # It is certainly possible that there's valgrind but no valgrind.h.
  # GCC relies on making annotations so we must have both.
  AC_MSG_CHECKING(for VALGRIND_DISCARD in <valgrind/memcheck.h>)
  AC_TRY_CPP(
    [#include <valgrind/memcheck.h>
#ifndef VALGRIND_DISCARD
#error VALGRIND_DISCARD not defined
#endif],
  [gcc_cv_header_valgrind_memcheck_h=yes],
  [gcc_cv_header_valgrind_memcheck_h=no])
  AC_MSG_RESULT($gcc_cv_header_valgrind_memcheck_h)
  AC_MSG_CHECKING(for VALGRIND_DISCARD in <memcheck.h>)
  AC_TRY_CPP(
    [#include <memcheck.h>
#ifndef VALGRIND_DISCARD
#error VALGRIND_DISCARD not defined
#endif],
  [gcc_cv_header_memcheck_h=yes], 
  gcc_cv_header_memcheck_h=no)
  AC_MSG_RESULT($gcc_cv_header_memcheck_h)
  AM_PATH_PROG_WITH_TEST(valgrind_path, valgrind,
	[$ac_dir/$ac_word --version | grep valgrind- >/dev/null 2>&1])
  if test "x$valgrind_path" = "x" \
    || (test $have_valgrind_h = no \
	&& test $gcc_cv_header_memcheck_h = no \
	&& test $gcc_cv_header_valgrind_memcheck_h = no); then
	AC_MSG_ERROR([*** Can't find both valgrind and valgrind/memcheck.h, memcheck.h or valgrind.h])
  fi
  valgrind_path_defines=-DVALGRIND_PATH='\"'$valgrind_path'\"'
  valgrind_command="$valgrind_path -q"
  AC_DEFINE(ENABLE_VALGRIND_CHECKING, 1,
[Define if you want to run subprograms and generated programs
   through valgrind (a memory checker).  This is extremely expensive.])
  if test $gcc_cv_header_valgrind_memcheck_h = yes; then
    AC_DEFINE(HAVE_VALGRIND_MEMCHECK_H, 1,
	[Define if valgrind's valgrind/memcheck.h header is installed.])
  fi
  if test $gcc_cv_header_memcheck_h = yes; then
    AC_DEFINE(HAVE_MEMCHECK_H, 1,
	[Define if valgrind's memcheck.h header is installed.])
  fi
fi
AC_SUBST(valgrind_path_defines)
AC_SUBST(valgrind_command)

# Enable code coverage collection
AC_ARG_ENABLE(coverage,
[  --enable-coverage[=LEVEL]
			  enable compiler\'s code coverage collection.
			  Use to measure compiler performance and locate
			  unused parts of the compiler. With LEVEL, specify
			  optimization. Values are opt, noopt,
			  default is noopt],
[case "${enableval}" in
yes|noopt)
	coverage_flags="-fprofile-arcs -ftest-coverage -frandom-seed=\$@ -O0"
	;;
opt)
	coverage_flags="-fprofile-arcs -ftest-coverage -frandom-seed=\$@ -O2"
	;;
*)
	AC_MSG_ERROR(unknown coverage setting $enableval)
	;;
esac],
[coverage_flags=""])
AC_SUBST(coverage_flags)

AC_ARG_ENABLE(gather-detailed-mem-stats, 
[  --enable-gather-detailed-mem-stats         enable detailed memory allocation stats gathering], [],
[enable_gather_detailed_mem_stats=no])
if test x$enable_gather_detailed_mem_stats = xyes ; then
  AC_DEFINE(GATHER_STATISTICS, 1,
        [Define to enable detailed memory allocation stats gathering.])
fi

# -------------------------------
# Miscenalleous configure options
# -------------------------------

# With stabs
AC_ARG_WITH(stabs,
[  --with-stabs            arrange to use stabs instead of host debug format],
stabs="$with_stabs",
stabs=no)

# Determine whether or not multilibs are enabled.
AC_ARG_ENABLE(multilib,
[  --enable-multilib       enable library support for multiple ABIs],
[], [enable_multilib=yes])
AC_SUBST(enable_multilib)

# Enable __cxa_atexit for C++.
AC_ARG_ENABLE(__cxa_atexit,
[  --enable-__cxa_atexit   enable __cxa_atexit for C++],
[], [])

# Enable threads
# Pass with no value to take the default
# Pass with a value to specify a thread package
AC_ARG_ENABLE(threads,
[  --enable-threads        enable thread usage for target GCC
  --enable-threads=LIB    use LIB thread package for target GCC],,
enable_threads='')
# Save in case it gets overwritten in config.gcc
enable_threads_flag=$enable_threads

AC_ARG_ENABLE(objc-gc,
[  --enable-objc-gc	  enable the use of Boehm's garbage collector with
			  the GNU Objective-C runtime],
if test x$enable_objc_gc = xno; then
	objc_boehm_gc=''
else
	objc_boehm_gc=1
fi,
objc_boehm_gc='')

AC_ARG_WITH(dwarf2,
[  --with-dwarf2           force the default debug format to be DWARF 2],
dwarf2="$with_dwarf2",
dwarf2=no)

AC_ARG_ENABLE(shared,
[  --disable-shared        don't provide a shared libgcc],
[
  case $enable_shared in
  yes | no) ;;
  *)
    enable_shared=no
    IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
    for pkg in $enableval; do
      if test "X$pkg" = "Xgcc" || test "X$pkg" = "Xlibgcc"; then
        enable_shared=yes
      fi
    done
    IFS="$ac_save_ifs"
    ;;
  esac
], [enable_shared=yes])
AC_SUBST(enable_shared)

AC_ARG_WITH(sysroot,
[  --with-sysroot[=DIR] Search for usr/lib, usr/include, et al, within DIR.],
[
 case ${with_sysroot} in
 yes) TARGET_SYSTEM_ROOT='${exec_prefix}/${target_noncanonical}/sys-root' ;;
 *) TARGET_SYSTEM_ROOT=$with_sysroot ;;
 esac
   
 TARGET_SYSTEM_ROOT_DEFINE='-DTARGET_SYSTEM_ROOT=\"$(TARGET_SYSTEM_ROOT)\"'
 CROSS_SYSTEM_HEADER_DIR='$(TARGET_SYSTEM_ROOT)$(NATIVE_SYSTEM_HEADER_DIR)'

 if test "x$exec_prefix" = xNONE; then
  if test "x$prefix" = xNONE; then
   test_prefix=/usr/local
  else
   test_prefix=$prefix
  fi
 else
  test_prefix=$exec_prefix
 fi
 case ${TARGET_SYSTEM_ROOT} in
 "${test_prefix}"|"${test_prefix}/"*|\
 '${exec_prefix}'|'${exec_prefix}/'*)
   t="$TARGET_SYSTEM_ROOT_DEFINE -DTARGET_SYSTEM_ROOT_RELOCATABLE"
   TARGET_SYSTEM_ROOT_DEFINE="$t"
   ;;
 esac
], [
 TARGET_SYSTEM_ROOT=
 TARGET_SYSTEM_ROOT_DEFINE=
 CROSS_SYSTEM_HEADER_DIR='$(gcc_tooldir)/sys-include'
])
AC_SUBST(TARGET_SYSTEM_ROOT)
AC_SUBST(TARGET_SYSTEM_ROOT_DEFINE)
AC_SUBST(CROSS_SYSTEM_HEADER_DIR)

# Build with intermodule optimisations
AC_ARG_ENABLE(intermodule,
[  --enable-intermodule    build the compiler in one step],
[case ${enable_intermodule} in
  yes) onestep="-onestep";;
  *) onestep="";;
esac],
[onestep=""])
AC_SUBST(onestep)

# -------------------------
# Checks for other programs
# -------------------------

AC_PROG_MAKE_SET

# Find some useful tools
AC_PROG_AWK
# We need awk to run opts.sh (to create options.c and options.h).
# Bail out if it's missing.
case ${AWK} in
  "") AC_MSG_ERROR([can't build without awk, bailing out]) ;;
esac

gcc_AC_PROG_LN
gcc_AC_PROG_LN_S
AC_PROG_RANLIB
gcc_AC_PROG_INSTALL

# See if cmp has --ignore-initial.
gcc_AC_PROG_CMP_IGNORE_INITIAL

# See if we have the mktemp command.
AC_CHECK_PROG(have_mktemp_command, mktemp, yes, no)

# Do we have a single-tree copy of texinfo?
if test -f $srcdir/../texinfo/Makefile.in; then
  MAKEINFO='$(objdir)/../texinfo/makeinfo/makeinfo'
  gcc_cv_prog_makeinfo_modern=yes
  AC_MSG_RESULT([Using makeinfo from the unified source tree.])
else
  # See if makeinfo has been installed and is modern enough
  # that we can use it.
  gcc_AC_CHECK_PROG_VER(MAKEINFO, makeinfo, --version,
  [GNU texinfo.* \([0-9][0-9.]*\)],
  [4.[2-9]*])
fi

if test $gcc_cv_prog_makeinfo_modern = no; then
  AC_MSG_WARN([
*** Makeinfo is missing or too old.
*** Info documentation will not be built.])
  BUILD_INFO=
else
  BUILD_INFO=info		AC_SUBST(BUILD_INFO)
fi

# Is pod2man recent enough to regenerate manpages?
AC_MSG_CHECKING([for recent Pod::Man])
if (perl -e 'use 1.10 Pod::Man') >/dev/null 2>&1; then
  AC_MSG_RESULT(yes)
  GENERATED_MANPAGES=generated-manpages		AC_SUBST(GENERATED_MANPAGES)
else
  AC_MSG_RESULT(no)
  GENERATED_MANPAGES=
fi

# How about lex?
dnl Don't use AC_PROG_LEX; we insist on flex.
dnl LEXLIB is not useful in gcc.
if test -f $srcdir/../flex/skel.c; then
  FLEX='$(objdir)/../flex/flex'
else
  AC_CHECK_PROG(FLEX, flex, flex, ${CONFIG_SHELL-/bin/sh} ${srcdir}/../missing flex)
fi

# Bison?
# The -L switch is so bison can find its skeleton file.
if test -f $srcdir/../bison/bison.simple; then
  BISON='$(objdir)/../bison/bison -L $(srcdir)/../bison/'
else
  AC_CHECK_PROG(BISON, bison, bison, ${CONFIG_SHELL-/bin/sh} ${srcdir}/../missing bison)
fi

# --------------------
# Checks for C headers
# --------------------

AC_MSG_CHECKING(for GNU C library)
AC_CACHE_VAL(gcc_cv_glibc,
[AC_TRY_COMPILE(
  [#include <features.h>],[
#if ! (defined __GLIBC__ || defined __GNU_LIBRARY__)
#error Not a GNU C library system
#endif], 
  [gcc_cv_glibc=yes], 
  gcc_cv_glibc=no)])
AC_MSG_RESULT($gcc_cv_glibc)
if test $gcc_cv_glibc = yes; then
  AC_DEFINE(_GNU_SOURCE, 1, [Always define this when using the GNU C Library])
fi

# Need to reject headers which give warnings, so that the -Werror bootstrap
# works later. *sigh*  This needs to come before all header checks.
AC_PROG_CPP_WERROR

AC_HEADER_STDC
AC_HEADER_TIME
gcc_AC_HEADER_STDBOOL
gcc_AC_HEADER_STRING
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(limits.h stddef.h string.h strings.h stdlib.h time.h \
		 fcntl.h unistd.h sys/file.h sys/time.h sys/mman.h \
		 sys/resource.h sys/param.h sys/times.h sys/stat.h \
		 direct.h malloc.h langinfo.h ldfcn.h locale.h wchar.h)

# Check for thread headers.
AC_CHECK_HEADER(thread.h, [have_thread_h=yes], [have_thread_h=])
AC_CHECK_HEADER(pthread.h, [have_pthread_h=yes], [have_pthread_h=])

# These tests can't be done till we know if we have limits.h.
gcc_AC_C_CHAR_BIT
AC_C_BIGENDIAN_CROSS

# --------
# UNSORTED
# --------

# Stage specific cflags for build.
stage1_cflags=
case $build in
vax-*-*)
  if test x$GCC = xyes
  then
    stage1_cflags="-Wa,-J"
  else
    stage1_cflags="-J"
  fi
  ;;
powerpc-*-darwin*)
  # The spiffy cpp-precomp chokes on some legitimate constructs in GCC
  # sources; use -no-cpp-precomp to get to GNU cpp.
  # Apple's GCC has bugs in designated initializer handling, so disable
  # that too.
  stage1_cflags="-no-cpp-precomp -DHAVE_DESIGNATED_INITIALIZERS=0"
  ;;
esac
AC_SUBST(stage1_cflags)

# These libraries may be used by collect2.
# We may need a special search path to get them linked.
AC_CACHE_CHECK(for collect2 libraries, gcc_cv_collect2_libs,
[save_LIBS="$LIBS"
for libs in '' -lld -lmld \
		'-L/usr/lib/cmplrs/cc2.11 -lmld' \
		'-L/usr/lib/cmplrs/cc3.11 -lmld'
do
	LIBS="$libs"
	AC_TRY_LINK_FUNC(ldopen,
		[gcc_cv_collect2_libs="$libs"; break])
done
LIBS="$save_LIBS"
test -z "$gcc_cv_collect2_libs" && gcc_cv_collect2_libs='none required'])
case $gcc_cv_collect2_libs in
	"none required")	;;
	*)	COLLECT2_LIBS=$gcc_cv_collect2_libs ;;
esac
AC_SUBST(COLLECT2_LIBS)

# When building Ada code on Alpha, we need exc_resume which is usually in
# -lexc.  So test for it.
save_LIBS="$LIBS"
LIBS=
AC_SEARCH_LIBS(exc_resume, exc)
GNAT_LIBEXC="$LIBS"
LIBS="$save_LIBS"
AC_SUBST(GNAT_LIBEXC)

# Some systems put ldexp and frexp in libm instead of libc; assume
# they're both in the same place.  jcf-dump needs them.
save_LIBS="$LIBS"
LIBS=
AC_SEARCH_LIBS(ldexp, m)
LDEXP_LIB="$LIBS"
LIBS="$save_LIBS"
AC_SUBST(LDEXP_LIB)

# Use <inttypes.h> only if it exists,
# doesn't clash with <sys/types.h>, and declares intmax_t.
AC_MSG_CHECKING(for inttypes.h)
AC_CACHE_VAL(gcc_cv_header_inttypes_h,
[AC_TRY_COMPILE(
  [#include <sys/types.h>
#include <inttypes.h>],
  [intmax_t i = -1;],
  [gcc_cv_header_inttypes_h=yes],
  gcc_cv_header_inttypes_h=no)])
AC_MSG_RESULT($gcc_cv_header_inttypes_h)
if test $gcc_cv_header_inttypes_h = yes; then
  AC_DEFINE(HAVE_INTTYPES_H, 1,
	[Define if you have a working <inttypes.h> header file.])
fi

dnl Disabled until we have a complete test for buggy enum bitfields.
dnl gcc_AC_C_ENUM_BF_UNSIGNED

AC_CHECK_FUNCS(times clock dup2 kill getrlimit setrlimit atoll atoq \
	sysconf strsignal putc_unlocked fputc_unlocked fputs_unlocked \
	fwrite_unlocked fprintf_unlocked getrusage nl_langinfo \
        scandir alphasort gettimeofday mbstowcs wcswidth mmap mincore \
        setlocale)

if test x$ac_cv_func_mbstowcs = xyes; then
  AC_CACHE_CHECK(whether mbstowcs works, gcc_cv_func_mbstowcs_works,
[    AC_TRY_RUN([#include <stdlib.h>
int main()
{
  mbstowcs(0, "", 0);
  return 0;
}],
    gcc_cv_func_mbstowcs_works=yes,
    gcc_cv_func_mbstowcs_works=no,
    gcc_cv_func_mbstowcs_works=yes)])
  if test x$gcc_cv_func_mbstowcs_works = xyes; then
    AC_DEFINE(HAVE_WORKING_MBSTOWCS, 1,
  [Define this macro if mbstowcs does not crash when its
   first argument is NULL.])
  fi
fi

AC_CHECK_TYPE(ssize_t, int)

# Try to determine the array type of the second argument of getgroups
# for the target system (int or gid_t).
AC_TYPE_GETGROUPS
if test "${target}" = "${build}"; then
  TARGET_GETGROUPS_T=$ac_cv_type_getgroups
else
  case "${target}" in
	# This condition may need some tweaking.  It should include all
	# targets where the array type of the second argument of getgroups
	# is int and the type of gid_t is not equivalent to int.
	*-*-sunos* | *-*-ultrix*)
		TARGET_GETGROUPS_T=int
		;;
	*)
		TARGET_GETGROUPS_T=gid_t
		;;
  esac
fi
AC_SUBST(TARGET_GETGROUPS_T)

gcc_AC_FUNC_PRINTF_PTR
gcc_AC_FUNC_MMAP_BLACKLIST

case "${host}" in
*-*-*vms*)
  # Under VMS, vfork works very differently than on Unix. The standard test 
  # won't work, and it isn't easily adaptable. It makes more sense to
  # just force it.
  ac_cv_func_vfork_works=yes
  ;;
esac
AC_FUNC_VFORK

AM_ICONV
# Until we have in-tree GNU iconv:
LIBICONV_DEP=
AC_SUBST(LIBICONV_DEP)

AM_LC_MESSAGES

# We will need to find libiberty.h and ansidecl.h
saved_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -I${srcdir} -I${srcdir}/../include"
gcc_AC_CHECK_DECLS(getenv atol sbrk abort atof getcwd getwd \
	strsignal putc_unlocked fputs_unlocked fwrite_unlocked \
        fprintf_unlocked strstr errno snprintf vasprintf \
	malloc realloc calloc free basename getopt clock, , ,[
#include "ansidecl.h"
#include "system.h"])

gcc_AC_CHECK_DECLS(getrlimit setrlimit getrusage, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
])

AC_TRY_COMPILE([
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
],[rlim_t l = 0;],,[AC_DEFINE([rlim_t],[long],
[Define to \`long' if <sys/resource.h> doesn't define.])])

gcc_AC_CHECK_DECLS(ldgetname, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_LDFCN_H
#include <ldfcn.h>
#endif
])

gcc_AC_CHECK_DECLS(times, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
])

# More time-related stuff.
AC_CACHE_CHECK(for struct tms, ac_cv_struct_tms, [
AC_TRY_COMPILE([
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
], [struct tms tms;], ac_cv_struct_tms=yes, ac_cv_struct_tms=no)])
if test $ac_cv_struct_tms = yes; then
  AC_DEFINE(HAVE_STRUCT_TMS, 1,
  [Define if <sys/times.h> defines struct tms.])
fi

# use gcc_cv_* here because this doesn't match the behavior of AC_CHECK_TYPE.
# revisit after autoconf 2.50.
AC_CACHE_CHECK(for clock_t, gcc_cv_type_clock_t, [
AC_TRY_COMPILE([
#include "ansidecl.h"
#include "system.h"
], [clock_t x;], gcc_cv_type_clock_t=yes, gcc_cv_type_clock_t=no)])
if test $gcc_cv_type_clock_t = yes; then
  AC_DEFINE(HAVE_CLOCK_T, 1,
  [Define if <time.h> defines clock_t.])
fi

AC_CACHE_CHECK(for uchar, gcc_cv_type_uchar,
[AC_TRY_COMPILE([
#include "ansidecl.h"
#include "system.h"
], 
[if ((uchar *)0) return 0;
 if (sizeof(uchar)) return 0;],
ac_cv_type_uchar=yes, ac_cv_type_uchar=no)])
if test $ac_cv_type_uchar = yes; then
  AC_DEFINE(HAVE_UCHAR, 1,
  [Define if <sys/types.h> defines \`uchar'.])
fi

# Restore CFLAGS from before the gcc_AC_NEED_DECLARATIONS tests.
CFLAGS="$saved_CFLAGS"

gcc_AC_INITFINI_ARRAY

# mkdir takes a single argument on some systems. 
gcc_AC_FUNC_MKDIR_TAKES_ONE_ARG

# File extensions
manext='.1'
objext='.o'
AC_SUBST(manext)
AC_SUBST(objext)

# With Setjmp/Longjmp based exception handling.
AC_ARG_ENABLE(sjlj-exceptions,
[  --enable-sjlj-exceptions
                          arrange to use setjmp/longjmp exception handling],
[sjlj=`if test $enableval = yes; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(CONFIG_SJLJ_EXCEPTIONS, $sjlj,
  [Define 0/1 to force the choice for exception handling model.])])

if test x$host = x$target; then
   AC_CHECK_LIB(unwind, main, use_libunwind_default=yes, use_libunwind_default=no)
else
   use_libunwind_default=no
fi
# Use libunwind based exception handling.
AC_ARG_ENABLE(libunwind-exceptions,
[  --enable-libunwind-exceptions  force use libunwind for exceptions],
use_libunwind_exceptions=$enableval,
use_libunwind_exceptions=$use_libunwind_default)
if test x"$use_libunwind_exceptions" = xyes; then
   AC_DEFINE(USE_LIBUNWIND_EXCEPTIONS, 1,
	[Define if gcc should use -lunwind.])
fi

# --------------------------------------------------------
# Build, host, and target specific configuration fragments
# --------------------------------------------------------

# Collect build-machine-specific information.
. ${srcdir}/config.build

# Collect host-machine-specific information.
. ${srcdir}/config.host

target_gtfiles=

# Collect target-machine-specific information.
. ${srcdir}/config.gcc

extra_objs="${host_extra_objs} ${extra_objs}"
extra_gcc_objs="${host_extra_gcc_objs} ${extra_gcc_objs}"

# Default the target-machine variables that were not explicitly set.
if test x"$tm_file" = x
then tm_file=$cpu_type/$cpu_type.h; fi

if test x"$extra_headers" = x
then extra_headers=; fi

if test x$md_file = x
then md_file=$cpu_type/$cpu_type.md; fi

if test x$out_file = x
then out_file=$cpu_type/$cpu_type.c; fi

if test x"$tmake_file" = x
then tmake_file=$cpu_type/t-$cpu_type
fi

if test x"$dwarf2" = xyes
then tm_file="$tm_file tm-dwarf2.h"
fi

# Say what files are being used for the output code and MD file.
echo "Using \`$srcdir/config/$out_file' for machine-specific logic."
echo "Using \`$srcdir/config/$md_file' as machine description file."

# If any of the xm_file variables contain nonexistent files, warn
# about them and drop them.

bx=
for x in $build_xm_file; do
  if    test -f $srcdir/config/$x
  then      bx="$bx $x"
  else      AC_MSG_WARN($srcdir/config/$x does not exist.)
  fi
done
build_xm_file="$bx"

hx=
for x in $host_xm_file; do
  if    test -f $srcdir/config/$x
  then      hx="$hx $x"
  else      AC_MSG_WARN($srcdir/config/$x does not exist.)
  fi
done
host_xm_file="$hx"

tx=
for x in $xm_file; do
  if    test -f $srcdir/config/$x
  then      tx="$tx $x"
  else      AC_MSG_WARN($srcdir/config/$x does not exist.)
  fi
done
xm_file="$tx"

count=a
for f in $tm_file; do
	count=${count}x
done
if test $count = ax; then
	echo "Using \`$srcdir/config/$tm_file' as target machine macro file."
else
	echo "Using the following target machine macro files:"
	for f in $tm_file; do
		echo "	$srcdir/config/$f"
	done
fi

if test x$need_64bit_hwint = xyes; then
	AC_DEFINE(NEED_64BIT_HOST_WIDE_INT, 1,
[Define to 1 if HOST_WIDE_INT must be 64 bits wide (see hwint.h).])
fi

count=a
for f in $host_xm_file; do
	count=${count}x
done
if test $count = a; then
	:
elif test $count = ax; then
	echo "Using \`$srcdir/config/$host_xm_file' as host machine macro file."
else
	echo "Using the following host machine macro files:"
	for f in $host_xm_file; do
		echo "	$srcdir/config/$f"
	done
fi
echo "Using ${out_host_hook_obj} for host machine hooks."

if test "$host_xm_file" != "$build_xm_file"; then
	count=a
	for f in $build_xm_file; do
		count=${count}x
	done
	if test $count = a; then
		:
	elif test $count = ax; then
		echo "Using \`$srcdir/config/$build_xm_file' as build machine macro file."
	else
		echo "Using the following build machine macro files:"
		for f in $build_xm_file; do
			echo "	$srcdir/config/$f"
		done
	fi
fi

# Check if a valid thread package
case ${enable_threads_flag} in
  "" | no)
    # No threads
    target_thread_file='single'
    ;;
  yes)
    # default
    target_thread_file='single'
    ;;
  aix | dce | gnat | irix | posix | rtems | \
  single | solaris | vxworks | win32 )
    target_thread_file=${enable_threads_flag}
    ;;
  *)
    echo "${enable_threads_flag} is an unknown thread package" 1>&2
    exit 1
    ;;
esac

if test x${thread_file} = x; then
  # No thread file set by target-specific clauses in config.gcc,
  # so use file chosen by default logic above
  thread_file=${target_thread_file}
fi

if test x$enable___cxa_atexit = xyes || \
   test x$enable___cxa_atexit = x -a x$default_use_cxa_atexit = xyes; then
   AC_CHECK_FUNC(__cxa_atexit,
	[AC_DEFINE(DEFAULT_USE_CXA_ATEXIT, 1,
   	[Define if you want to use __cxa_atexit, rather than atexit, to
   	register C++ destructors for local statics and global objects.
   	This is essential for fully standards-compliant handling of
   	destructors, but requires __cxa_atexit in libc.])],
	echo "__cxa_atexit can't be enabled on this target")
fi

# Look for a file containing extra machine modes.
if test -n "$extra_modes" && test -f $srcdir/config/$extra_modes; then
  extra_modes_file='$(srcdir)'/config/${extra_modes}
  AC_SUBST(extra_modes_file)
  AC_DEFINE_UNQUOTED(EXTRA_MODES_FILE, "config/$extra_modes",
  [Define to the name of a file containing a list of extra machine modes
   for this architecture.])
fi

# auto-host.h is the file containing items generated by autoconf and is
# the first file included by config.h.
# If host=build, it is correct to have bconfig include auto-host.h
# as well.  If host!=build, we are in error and need to do more 
# work to find out the build config parameters.
if test x$host = x$build
then
	build_auto=auto-host.h
	FORBUILD=..
else
	# We create a subdir, then run autoconf in the subdir.
	# To prevent recursion we set host and build for the new
	# invocation of configure to the build for this invocation
	# of configure. 
	tempdir=build.$$
	rm -rf $tempdir
	mkdir $tempdir
	cd $tempdir
	case ${srcdir} in
	/* | [A-Za-z]:[\\/]* ) realsrcdir=${srcdir};;
	*) realsrcdir=../${srcdir};;
	esac
	saved_CFLAGS="${CFLAGS}"
	CC="${CC_FOR_BUILD}" CFLAGS="${CFLAGS_FOR_BUILD}" \
	${realsrcdir}/configure \
		--target=$target_alias --host=$build_alias --build=$build_alias
	CFLAGS="${saved_CFLAGS}"

	# We just finished tests for the build machine, so rename
	# the file auto-build.h in the gcc directory.
	mv auto-host.h ../auto-build.h
	cd ..
	rm -rf $tempdir
	build_auto=auto-build.h
	FORBUILD=../${build_subdir}
fi
AC_SUBST(FORBUILD)

tm_file="${tm_file} defaults.h"
tm_p_file="${tm_p_file} tm-preds.h"
host_xm_file="auto-host.h ansidecl.h ${host_xm_file}"
build_xm_file="${build_auto} ansidecl.h ${build_xm_file}"
# We don't want ansidecl.h in target files, write code there in ISO/GNU C.
# put this back in temporarily.
xm_file="ansidecl.h ${xm_file}"

# --------
# UNSORTED
# --------

# Get the version trigger filename from the toplevel
if test "${with_gcc_version_trigger+set}" = set; then
	gcc_version_trigger=$with_gcc_version_trigger
else
	gcc_version_trigger=${srcdir}/version.c
fi
changequote(,)dnl
gcc_version_full=`grep version_string ${gcc_version_trigger} | sed -e 's/.*"\([^"]*\)".*/\1/'`
gcc_version=`echo ${gcc_version_full} | sed -e 's/\([^ ]*\) .*/\1/'`

# Compile in configure arguments.
if test -f configargs.h ; then
	# Being re-configured.
	gcc_config_arguments=`grep configuration_arguments configargs.h | sed -e 's/.*"\([^"]*\)".*/\1/'`
	gcc_config_arguments="$gcc_config_arguments : (reconfigured) $TOPLEVEL_CONFIGURE_ARGUMENTS"
else
	gcc_config_arguments="$TOPLEVEL_CONFIGURE_ARGUMENTS"
fi

# Double all backslashes and backslash all quotes to turn
# gcc_config_arguments into a C string.
sed -e 's/\\/\\\\/g; s/"/\\"/g' <<EOF >conftest.out
$gcc_config_arguments
EOF
gcc_config_arguments_str=`cat conftest.out`
rm -f conftest.out

cat > configargs.h <<EOF
/* Generated automatically. */
static const char configuration_arguments[] = "$gcc_config_arguments_str";
static const char thread_model[] = "$thread_file";

static const struct {
  const char *name, *value;
} configure_default_options[] = $configure_default_options;
EOF
changequote([,])dnl

# Internationalization
PACKAGE=gcc
VERSION="$gcc_version"
AC_SUBST(PACKAGE)
AC_SUBST(VERSION)

ZW_GNU_GETTEXT_SISTER_DIR

# If LIBINTL contains LIBICONV, then clear LIBICONV so we don't get
# -liconv on the link line twice.
case "$LIBINTL" in *$LIBICONV*)
	LIBICONV= ;;
esac

# Windows32 Registry support for specifying GCC installation paths.
AC_ARG_ENABLE(win32-registry,
[  --disable-win32-registry
                          disable lookup of installation paths in the
                          Registry on Windows hosts
  --enable-win32-registry enable registry lookup (default)
  --enable-win32-registry=KEY
                          use KEY instead of GCC version as the last portion
                          of the registry key],,)
case $host_os in
	win32 | pe | cygwin* | mingw32* | uwin*)
AC_MSG_CHECKING(whether windows registry support is requested)
if test "x$enable_win32_registry" != xno; then
  AC_DEFINE(ENABLE_WIN32_REGISTRY, 1,
[Define to 1 if installation paths should be looked up in Windows32
   Registry. Ignored on non windows32 hosts.])
  AC_MSG_RESULT(yes)
  AC_SEARCH_LIBS(RegOpenKeyExA, advapi32)
else
  AC_MSG_RESULT(no)
fi

# Check if user specified a different registry key.
case "x${enable_win32_registry}" in
x | xyes)
  # default.
  gcc_cv_win32_registry_key="$VERSION"
  ;;
xno)
  # no registry lookup.
  gcc_cv_win32_registry_key=''
  ;;
*)
  # user-specified key.
  gcc_cv_win32_registry_key="$enable_win32_registry"
  ;;
esac

if test "x$enable_win32_registry" != xno; then
  AC_MSG_CHECKING(registry key on windows hosts)
  AC_DEFINE_UNQUOTED(WIN32_REGISTRY_KEY, "$gcc_cv_win32_registry_key",
	[Define to be the last portion of registry key on windows hosts.])
  AC_MSG_RESULT($gcc_cv_win32_registry_key)
fi
;;
esac

# Get an absolute path to the GCC top-level source directory
holddir=`${PWDCMD-pwd}`
cd $srcdir
topdir=`${PWDCMD-pwd}`
cd $holddir

# Conditionalize the makefile for this host machine.
xmake_file=
for f in ${host_xmake_file}
do
	if test -f ${srcdir}/config/$f
	then
		xmake_file="${xmake_file} \$(srcdir)/config/$f"
	fi
done

# Conditionalize the makefile for this target machine.
tmake_file_=
for f in ${tmake_file}
do
	if test -f ${srcdir}/config/$f
	then
		tmake_file_="${tmake_file_} \$(srcdir)/config/$f"
	fi
done
tmake_file="${tmake_file_}"

symbolic_link='ln -s'

# If the host doesn't support symlinks, modify CC in
# FLAGS_TO_PASS so CC="stage1/xgcc -Bstage1/" works.
# Otherwise, we can use "CC=$(CC)".
rm -f symtest.tem
if $symbolic_link $srcdir/gcc.c symtest.tem 2>/dev/null
then
	cc_set_by_configure="\$(CC)"
	quoted_cc_set_by_configure="\$(CC)"
	stage_prefix_set_by_configure="\$(STAGE_PREFIX)"
	quoted_stage_prefix_set_by_configure="\$(STAGE_PREFIX)"
else
	rm -f symtest.tem
	if cp -p $srcdir/gcc.c symtest.tem 2>/dev/null
	then
		symbolic_link="cp -p"
	else
		symbolic_link="cp"
	fi
	cc_set_by_configure="\`case '\$(CC)' in stage*) echo '\$(CC)' | sed -e 's|stage|../stage|g';; *) echo '\$(CC)';; esac\`"
	quoted_cc_set_by_configure="\\\`case '\\\$(CC)' in stage*) echo '\\\$(CC)' | sed -e 's|stage|../stage|g';; *) echo '\\\$(CC)';; esac\\\`"
	stage_prefix_set_by_configure="\`case '\$(STAGE_PREFIX)' in stage*) echo '\$(STAGE_PREFIX)' | sed -e 's|stage|../stage|g';; *) echo '\$(STAGE_PREFIX)';; esac\`"
	quoted_stage_prefix_set_by_configure="\\\`case '\\\$(STAGE_PREFIX)' in stage*) echo '\\\$(STAGE_PREFIX)' | sed -e 's|stage|../stage|g';; *) echo '\\\$(STAGE_PREFIX)';; esac\\\`"
fi
rm -f symtest.tem

out_object_file=`basename $out_file .c`.o

tm_file_list=
tm_include_list=
for f in $tm_file; do
  case $f in
    defaults.h )
       tm_file_list="${tm_file_list} \$(srcdir)/$f"
       tm_include_list="${tm_include_list} $f"
       ;;
    * )
       tm_file_list="${tm_file_list} \$(srcdir)/config/$f"
       tm_include_list="${tm_include_list} config/$f"
       ;;
  esac
done

tm_p_file_list=
tm_p_include_list=
for f in $tm_p_file; do
  case $f in
    tm-preds.h )
       tm_p_file_list="${tm_p_file_list} $f"
       tm_p_include_list="${tm_p_include_list} $f"
       ;;
    * )
       tm_p_file_list="${tm_p_file_list} \$(srcdir)/config/$f"
       tm_p_include_list="${tm_p_include_list} config/$f"
  esac
done

xm_file_list=
xm_include_list=
for f in $xm_file; do
  case $f in
    ansidecl.h )
       xm_file_list="${xm_file_list} \$(srcdir)/../include/$f"
       xm_include_list="${xm_include_list} $f"
       ;;
    auto-host.h )
       xm_file_list="${xm_file_list} $f"
       xm_include_list="${xm_include_list} $f"
       ;;
    * )
       xm_file_list="${xm_file_list} \$(srcdir)/config/$f"
       xm_include_list="${xm_include_list} config/$f"
       ;;
  esac
done

host_xm_file_list=
host_xm_include_list=
for f in $host_xm_file; do
  case $f in
    ansidecl.h )
       host_xm_file_list="${host_xm_file_list} \$(srcdir)/../include/$f"
       host_xm_include_list="${host_xm_include_list} $f"
       ;;
    auto-host.h )
       host_xm_file_list="${host_xm_file_list} $f"
       host_xm_include_list="${host_xm_include_list} $f"
       ;;
    * )
       host_xm_file_list="${host_xm_file_list} \$(srcdir)/config/$f"
       host_xm_include_list="${host_xm_include_list} config/$f"
       ;;
  esac
done

build_xm_file_list=
for f in $build_xm_file; do
  case $f in
    ansidecl.h )
       build_xm_file_list="${build_xm_file_list} \$(srcdir)/../include/$f"
       build_xm_include_list="${build_xm_include_list} $f"
       ;;
    auto-build.h | auto-host.h )
       build_xm_file_list="${build_xm_file_list} $f"
       build_xm_include_list="${build_xm_include_list} $f"
       ;;
    * )
       build_xm_file_list="${build_xm_file_list} \$(srcdir)/config/$f"
       build_xm_include_list="${build_xm_include_list} config/$f"
       ;;
  esac
done

# Define macro CROSS_COMPILE in compilation if this is a cross-compiler.
# Also use all.cross instead of all.internal and adjust SYSTEM_HEADER_DIR.
CROSS=						AC_SUBST(CROSS)
ALL=all.internal				AC_SUBST(ALL)
SYSTEM_HEADER_DIR='$(NATIVE_SYSTEM_HEADER_DIR)'	AC_SUBST(SYSTEM_HEADER_DIR)
if test x$host != x$target
then
	CROSS="-DCROSS_COMPILE"
	ALL=all.cross
	SYSTEM_HEADER_DIR='$(CROSS_SYSTEM_HEADER_DIR)'
	case "$host","$target" in
	# Darwin crosses can use the host system's libraries and headers,
	# because of the fat library support.  Of course, it must be the
	# same version of Darwin on both sides.  Allow the user to
	# just say --target=foo-darwin without a version number to mean
	# "the version on this system".
	    *-*-darwin*,*-*-darwin*)
		hostos=`echo $host | sed 's/.*-darwin/darwin/'`
		targetos=`echo $target | sed 's/.*-darwin/darwin/'`
		if test $hostos = $targetos -o $targetos = darwin ; then
		    CROSS=
		    SYSTEM_HEADER_DIR='$(NATIVE_SYSTEM_HEADER_DIR)'
		    with_headers=yes
		fi
		;;

	    i?86-*-*,x86_64-*-* \
	    | powerpc*-*-*,powerpc64*-*-*)
		CROSS="$CROSS -DNATIVE_CROSS" ;;
	esac
elif test "x$TARGET_SYSTEM_ROOT" != x; then
        # This is just $(TARGET_SYSTEM_ROOT)$(NATIVE_SYSTEM_HEADER_DIR)
        SYSTEM_HEADER_DIR='$(CROSS_SYSTEM_HEADER_DIR)'
fi

# If this is a cross-compiler that does not
# have its own set of headers then define
# inhibit_libc

# If this is using newlib, without having the headers available now,
# then define inhibit_libc in LIBGCC2_CFLAGS.
# This prevents libgcc2 from containing any code which requires libc
# support.
inhibit_libc=
if { { test x$host != x$target && test "x$with_sysroot" = x ; } ||
       test x$with_newlib = xyes ; } &&
     { test "x$with_headers" = x || test "x$with_headers" = xno ; } ; then
       inhibit_libc=-Dinhibit_libc
fi
AC_SUBST(inhibit_libc)

# When building gcc with a cross-compiler, we need to adjust things so
# that the generator programs are still built with the native compiler.
# Also, we cannot run fixincludes or fix-header.

# These are the normal (build=host) settings:
BUILD_PREFIX=			AC_SUBST(BUILD_PREFIX)
BUILD_PREFIX_1=ignore-		AC_SUBST(BUILD_PREFIX_1)
CC_FOR_BUILD='$(CC)'		AC_SUBST(CC_FOR_BUILD)
BUILD_CFLAGS='$(ALL_CFLAGS)'	AC_SUBST(BUILD_CFLAGS)

STMP_FIXINC=stmp-fixinc		AC_SUBST(STMP_FIXINC)

# Possibly disable fixproto, on a per-target basis.
case ${use_fixproto} in
  no)
    STMP_FIXPROTO=
    ;;
  yes)
    STMP_FIXPROTO=stmp-fixproto
    ;;
esac
AC_SUBST(STMP_FIXPROTO)

# And these apply if build != host, or we are generating coverage data
if test x$build != x$host || test "x$coverage_flags" != x
then
    BUILD_PREFIX=build-
    BUILD_PREFIX_1=build-
    BUILD_CFLAGS='$(INTERNAL_CFLAGS) $(T_CFLAGS) $(CFLAGS_FOR_BUILD)'

    if test "x$TARGET_SYSTEM_ROOT" = x; then
	STMP_FIXINC=
	STMP_FIXPROTO=
    fi
fi

# Expand extra_headers to include complete path.
# This substitutes for lots of t-* files.
extra_headers_list=
# Prepend $(srcdir)/config/${cpu_type}/ to every entry in extra_headers.
for file in ${extra_headers} ; do
  extra_headers_list="${extra_headers_list} \$(srcdir)/config/${cpu_type}/${file}"
done

# Add a definition of USE_COLLECT2 if system wants one.
case $use_collect2 in
  no) use_collect2= ;;
  "") ;;
  *) 
    host_xm_defines="${host_xm_defines} USE_COLLECT2"
    xm_defines="${xm_defines} USE_COLLECT2"
    ;;
esac

# Identify the assembler which will work hand-in-glove with the newly
# built GCC, so that we can examine its features.  This is the assembler
# which will be driven by the driver program.
#
# If build != host, and we aren't building gas in-tree, we identify a
# build->target assembler and hope that it will have the same features
# as the host->target assembler we'll be using.
AC_MSG_CHECKING(what assembler to use)
in_tree_gas=no
gcc_cv_as=
gcc_cv_gas_major_version=
gcc_cv_gas_minor_version=
gcc_cv_as_gas_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/gas
if test -x "$DEFAULT_ASSEMBLER"; then
	gcc_cv_as="$DEFAULT_ASSEMBLER"
elif test -x "$AS"; then
	gcc_cv_as="$AS"
elif test -x as$host_exeext; then
	# Build using assembler in the current directory.
	gcc_cv_as=./as$host_exeext
elif test -f $gcc_cv_as_gas_srcdir/configure.in \
     && test -f ../gas/Makefile; then
  # Single tree build which includes gas.
  in_tree_gas=yes
  _gcc_COMPUTE_GAS_VERSION
  rm -f as$host_exeext
  $symbolic_link ../gas/as-new$host_exeext as$host_exeext 2>/dev/null
  in_tree_gas_is_elf=no
  if grep 'obj_format = elf' ../gas/Makefile > /dev/null \
     || (grep 'obj_format = multi' ../gas/Makefile \
         && grep 'extra_objects =.* obj-elf' ../gas/Makefile) > /dev/null
  then
    in_tree_gas_is_elf=yes
  fi
fi

if test "x$gcc_cv_as" = x; then
	# Search the same directories that the installed compiler will
	# search.  Else we may find the wrong assembler and lose.  If we
	# do not find a suitable assembler binary, then try the user's
	# path.
	#
	# Also note we have to check MD_EXEC_PREFIX before checking the
	# user's path.  Unfortunately, there is no good way to get at the
	# value of MD_EXEC_PREFIX here.  So we do a brute force search
	# through all the known MD_EXEC_PREFIX values.  Ugh.  This needs
	# to be fixed as part of the make/configure rewrite too.

	if test "x$exec_prefix" = xNONE; then
		if test "x$prefix" = xNONE; then
			test_prefix=/usr/local
		else
			test_prefix=$prefix
		fi
	else
		test_prefix=$exec_prefix
	fi

	# If the loop below does not find an assembler, then use whatever
	# one we can find in the users's path.
	# user's path.
	if test "x$program_prefix" != xNONE; then
		gcc_cv_as=${program_prefix}as$host_exeext
	else
		gcc_cv_as=`echo as | sed ${program_transform_name}`$host_exeext
	fi

	test_dirs="$test_prefix/libexec/gcc/$target_noncanonical/$gcc_version \
		   $test_prefix/libexec/gcc/$target_noncanonical \
		   /usr/lib/gcc/$target_noncanonical/$gcc_version \
		   /usr/lib/gcc/$target_noncanonical \
		   $test_prefix/$target_noncanonical/bin/$target_noncanonical/$gcc_version \
		   $test_prefix/$target_noncanonical/bin"

	if test x$host = x$target; then
	    test_dirs="$test_dirs \
		   /usr/libexec \
		   /usr/ccs/gcc \
		   /usr/ccs/bin \
		   /udk/usr/ccs/bin \
		   /bsd43/usr/lib/cmplrs/cc \
		   /usr/cross64/usr/bin \
		   /usr/lib/cmplrs/cc \
		   /sysv/usr/lib/cmplrs/cc \
		   /svr4/usr/lib/cmplrs/cc \
		   /usr/bin"
	fi

	for dir in $test_dirs; do
		if test -x $dir/as$host_exeext; then
			gcc_cv_as=$dir/as$host_exeext
			break;
		fi
	done
fi
case $in_tree_gas in
  yes)
    AC_MSG_RESULT("newly built gas")
    ;;
  no)
    AC_MSG_RESULT($gcc_cv_as)
    ;;
esac

# Identify the linker which will work hand-in-glove with the newly
# built GCC, so that we can examine its features.  This is the linker
# which will be driven by the driver program.
#
# If build != host, and we aren't building gas in-tree, we identify a
# build->target linker and hope that it will have the same features
# as the host->target linker we'll be using.
AC_MSG_CHECKING(what linker to use)
in_tree_ld=no
gcc_cv_ld=
gcc_cv_gld_major_version=
gcc_cv_gld_minor_version=
gcc_cv_ld_gld_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/ld
gcc_cv_ld_bfd_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/bfd
if test -x "$DEFAULT_LINKER"; then
	gcc_cv_ld="$DEFAULT_LINKER"
elif test -x "$LD"; then
	gcc_cv_ld="$LD"
elif test -x collect-ld$host_exeext; then
	# Build using linker in the current directory.
	gcc_cv_ld=./collect-ld$host_exeext
elif test -f $gcc_cv_ld_gld_srcdir/configure.in \
     && test -f ../ld/Makefile; then
	# Single tree build which includes ld.
	in_tree_ld=yes
	in_tree_ld_is_elf=no
	if (grep 'EMUL = .*elf' ../ld/Makefile \
	    || grep 'EMUL = .*linux' ../ld/Makefile) > /dev/null; then
	  in_tree_ld_is_elf=yes
	fi
	for f in $gcc_cv_ld_bfd_srcdir/configure $gcc_cv_ld_gld_srcdir/configure $gcc_cv_ld_gld_srcdir/configure.in $gcc_cv_ld_gld_srcdir/Makefile.in
	do
changequote(,)dnl
		gcc_cv_gld_version=`grep '^VERSION=[0-9]*\.[0-9]*' $f`
changequote([,])dnl
		if test x$gcc_cv_gld_version != x; then
			break
		fi
	done
changequote(,)dnl
	gcc_cv_gld_major_version=`expr "$gcc_cv_gld_version" : "VERSION=\([0-9]*\)"`
	gcc_cv_gld_minor_version=`expr "$gcc_cv_gld_version" : "VERSION=[0-9]*\.\([0-9]*\)"`
changequote([,])dnl
	rm -f collect-ld$host_exeext
	$symbolic_link ../ld/ld-new$host_exeext collect-ld$host_exeext \
		2>/dev/null
fi

if test "x$gcc_cv_ld" = x; then
	# Search the same directories that the installed compiler will
	# search.  Else we may find the wrong linker and lose.  If we
	# do not find a suitable linker binary, then try the user's
	# path.
	#
	# Also note we have to check MD_EXEC_PREFIX before checking the
	# user's path.  Unfortunately, there is no good way to get at the
	# value of MD_EXEC_PREFIX here.  So we do a brute force search
	# through all the known MD_EXEC_PREFIX values.  Ugh.  This needs
	# to be fixed as part of the make/configure rewrite too.

	if test "x$exec_prefix" = xNONE; then
		if test "x$prefix" = xNONE; then
			test_prefix=/usr/local
		else
			test_prefix=$prefix
		fi
	else
		test_prefix=$exec_prefix
	fi

	# If the loop below does not find an linker, then use whatever
	# one we can find in the users's path.
	# user's path.
	if test "x$program_prefix" != xNONE; then
		gcc_cv_ld=${program_prefix}ld$host_exeext
	else
		gcc_cv_ld=`echo ld | sed ${program_transform_name}`$host_exeext
	fi

	test_dirs="$test_prefix/libexec/gcc/$target_noncanonical/$gcc_version \
		   $test_prefix/libexec/gcc/$target_noncanonical \
		   /usr/lib/gcc/$target_noncanonical/$gcc_version \
		   /usr/lib/gcc/$target_noncanonical \
		   $test_prefix/$target_noncanonical/bin/$target_noncanonical/$gcc_version \
		   $test_prefix/$target_noncanonical/bin"

	if test x$host = x$target; then
	    test_dirs="$test_dirs \
		   /usr/libexec \
		   /usr/ccs/gcc \
		   /usr/ccs/bin \
		   /udk/usr/ccs/bin \
		   /bsd43/usr/lib/cmplrs/cc \
		   /usr/cross64/usr/bin \
		   /usr/lib/cmplrs/cc \
		   /sysv/usr/lib/cmplrs/cc \
		   /svr4/usr/lib/cmplrs/cc \
		   /usr/bin"
	fi

	for dir in $test_dirs; do
		if test -x $dir/ld$host_exeext; then
			gcc_cv_ld=$dir/ld$host_exeext
			break;
		fi
	done
fi
case $in_tree_ld in
  yes)
    AC_MSG_RESULT("newly built ld")
    ;;
  no)
    AC_MSG_RESULT($gcc_cv_ld)
    ;;
esac

# Figure out what nm we will be using.
gcc_cv_binutils_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/binutils
AC_MSG_CHECKING(what nm to use)
in_tree_nm=no
if test -x nm$host_exeext; then
	gcc_cv_nm=./nm$host_exeext
elif test -f $gcc_cv_binutils_srcdir/configure.in \
     && test -f ../binutils/Makefile; then
	# Single tree build which includes binutils.
	in_tree_nm=yes
	gcc_cv_nm=./nm$host_exeext
	rm -f nm$host_exeext
	$symbolic_link ../binutils/nm-new$host_exeext nm$host_exeext 2>/dev/null
elif test "x$program_prefix" != xNONE; then
	gcc_cv_nm=${program_prefix}nm$host_exeext
else
	gcc_cv_nm=`echo nm | sed ${program_transform_name}`$host_exeext
fi
case $in_tree_nm in
  yes) AC_MSG_RESULT("newly built nm") ;;
  no)  AC_MSG_RESULT($gcc_cv_nm) ;;
esac

# Figure out what objdump we will be using.
AC_MSG_CHECKING(what objdump to use)
in_tree_objdump=no
if test -x objdump$host_exeext; then
	gcc_cv_objdump=./objdump$host_exeext
elif test -f $gcc_cv_binutils_srcdir/configure.in \
     && test -f ../binutils/Makefile; then
	# Single tree build which includes binutils.
	in_tree_objdump=yes
	gcc_cv_objdump=./objdump$host_exeext
	rm -f objdump$host_exeext
	$symbolic_link ../binutils/objdump$host_exeext \
		objdump$host_exeext 2>/dev/null
elif test "x$program_prefix" != xNONE; then
	gcc_cv_objdump=${program_prefix}objdump$host_exeext
else
	gcc_cv_objdump=`echo objdump | \
		sed ${program_transform_name}`$host_exeext
fi
case $in_tree_objdump in
  yes) AC_MSG_RESULT("newly built objdump") ;;
  no)  AC_MSG_RESULT($gcc_cv_objdump) ;;
esac

# Figure out what assembler alignment features are present.
gcc_GAS_CHECK_FEATURE([.balign and .p2align], gcc_cv_as_balign_and_p2align,
 [2,6,0],,
[.balign 4
.p2align 2],,
[AC_DEFINE(HAVE_GAS_BALIGN_AND_P2ALIGN, 1,
  [Define if your assembler supports .balign and .p2align.])])

gcc_GAS_CHECK_FEATURE([.p2align with maximum skip], gcc_cv_as_max_skip_p2align,
 [2,8,0],,
 [.p2align 4,,7],,
[AC_DEFINE(HAVE_GAS_MAX_SKIP_P2ALIGN, 1,
  [Define if your assembler supports specifying the maximum number
   of bytes to skip when using the GAS .p2align command.])])

gcc_GAS_CHECK_FEATURE([working .subsection -1], gcc_cv_as_subsection_m1,
 [elf,2,9,0],,
 [conftest_label1: .word 0
.subsection -1
conftest_label2: .word 0
.previous],
 [if test x$gcc_cv_nm != x; then
    $gcc_cv_nm conftest.o | grep conftest_label1 > conftest.nm1
    $gcc_cv_nm conftest.o | grep conftest_label2 | sed -e 's/label2/label1/' > conftest.nm2
    if cmp conftest.nm1 conftest.nm2 > /dev/null 2>&1
    then :
    else gcc_cv_as_subsection_m1=yes
    fi
    rm -f conftest.nm1 conftest.nm2
  fi],
 [AC_DEFINE(HAVE_GAS_SUBSECTION_ORDERING, 1,
  [Define if your assembler supports .subsection and .subsection -1 starts
   emitting at the beginning of your section.])])

gcc_GAS_CHECK_FEATURE([.weak], gcc_cv_as_weak,
 [2,2,0],,
 [	.weak foobar],,
[AC_DEFINE(HAVE_GAS_WEAK, 1, [Define if your assembler supports .weak.])])

# .hidden needs to be supported in both the assembler and the linker,
# because GNU LD versions before 2.12.1 have buggy support for STV_HIDDEN.
# This is irritatingly difficult to feature test for; we have to check the
# date string after the version number.  If we've got an in-tree
# ld, we don't know its patchlevel version, so we set the baseline at 2.13
# to be safe.
# The gcc_GAS_CHECK_FEATURE call just sets a cache variable.
gcc_GAS_CHECK_FEATURE([.hidden], gcc_cv_as_hidden,
 [elf,2,13,0],,
[	.hidden foobar
foobar:])

AC_CACHE_CHECK(linker for .hidden support, gcc_cv_ld_hidden,
[if test $in_tree_ld = yes ; then
  gcc_cv_ld_hidden=no
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 13 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
     gcc_cv_ld_hidden=yes
  fi
else
  gcc_cv_ld_hidden=yes
  ld_ver=`$gcc_cv_ld --version 2>/dev/null | sed 1q`
  if echo "$ld_ver" | grep GNU > /dev/null; then
changequote(,)dnl
    ld_vers=`echo $ld_ver | sed -n \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*\)$,\1,p' \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\)$,\1,p' \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\)$,\1,p' \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*\)[	 ].*$,\1,p' \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\)[	 ].*$,\1,p' \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*\)[	 ].*$,\1,p'`
    ld_date=`echo $ld_ver | sed -n 's,^.*\([2-9][0-9][0-9][0-9]\)[-]*\([01][0-9]\)[-]*\([0-3][0-9]\).*$,\1\2\3,p'`
    if test 0"$ld_date" -lt 20020404; then
      if test -n "$ld_date"; then
	# If there was date string, but was earlier than 2002-04-04, fail
	gcc_cv_ld_hidden=no
      elif test -z "$ld_vers"; then
	# If there was no date string nor ld version number, something is wrong
	gcc_cv_ld_hidden=no
      else
	ld_vers_major=`expr "$ld_vers" : '\([0-9]*\)'`
	ld_vers_minor=`expr "$ld_vers" : '[0-9]*\.\([0-9]*\)'`
	ld_vers_patch=`expr "$ld_vers" : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
	test -z "$ld_vers_patch" && ld_vers_patch=0
	if test "$ld_vers_major" -lt 2; then
	  gcc_cv_ld_hidden=no
	elif test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -lt 12; then
	  gcc_cv_ld_hidden="no"
	elif test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -eq 12 -a "$ld_vers_patch" -eq 0; then
	  gcc_cv_ld_hidden=no
	fi
      fi
changequote([,])dnl
    fi
  else
    case "${target}" in
      hppa64*-*-hpux* | ia64*-*-hpux*)
	gcc_cv_ld_hidden=yes
	;;
      *)
	gcc_cv_ld_hidden=no
	;;
    esac
  fi
fi])
libgcc_visibility=no
AC_SUBST(libgcc_visibility)
if test $gcc_cv_as_hidden = yes && test $gcc_cv_ld_hidden = yes; then
  libgcc_visibility=yes
  AC_DEFINE(HAVE_GAS_HIDDEN, 1,
  [Define if your assembler and linker support .hidden.])
fi

# Check if we have .[us]leb128, and support symbol arithmetic with it.
gcc_GAS_CHECK_FEATURE([.sleb128 and .uleb128], gcc_cv_as_leb128,
  [elf,2,11,0],,
[	.data
	.uleb128 L2 - L1
L1:
	.uleb128 1280
	.sleb128 -1010
L2:],
 [# GAS versions before 2.11 do not support uleb128,
  # despite appearing to.
  # ??? There exists an elf-specific test that will crash
  # the assembler.  Perhaps it's better to figure out whether
  # arbitrary sections are supported and try the test.
  as_ver=`$gcc_cv_as --version 2>/dev/null | sed 1q`
  if echo "$as_ver" | grep GNU > /dev/null; then
changequote(,)dnl
    as_ver=`echo $as_ver | sed -e 's/GNU assembler \([0-9.][0-9.]*\).*/\1/'`
    as_major=`echo $as_ver | sed 's/\..*//'`
    as_minor=`echo $as_ver | sed 's/[^.]*\.\([0-9]*\).*/\1/'`
changequote([,])dnl
    if test $as_major -eq 2 && test $as_minor -lt 11
    then :
    else gcc_cv_as_leb128=yes
    fi
  fi],
  [AC_DEFINE(HAVE_AS_LEB128, 1,
    [Define if your assembler supports .sleb128 and .uleb128.])])

# GAS versions up to and including 2.11.0 may mis-optimize
# .eh_frame data.
gcc_GAS_CHECK_FEATURE(eh_frame optimization, gcc_cv_as_eh_frame,
  [elf,2,12,0],,
[	.text
.LFB1:
	.4byte	0
.L1:
	.4byte	0
.LFE1:
	.section	.eh_frame,"aw",@progbits
__FRAME_BEGIN__:
	.4byte	.LECIE1-.LSCIE1
.LSCIE1:
	.4byte	0x0
	.byte	0x1
	.ascii "z\0"
	.byte	0x1
	.byte	0x78
	.byte	0x1a
	.byte	0x0
	.byte	0x4
	.4byte	1
	.p2align 1
.LECIE1:
.LSFDE1:
	.4byte	.LEFDE1-.LASFDE1
.LASFDE1:
	.4byte	.LASFDE1-__FRAME_BEGIN__
	.4byte	.LFB1
	.4byte	.LFE1-.LFB1
	.byte	0x4
	.4byte	.LFE1-.LFB1
	.byte	0x4
	.4byte	.L1-.LFB1
.LEFDE1:],
[  dnl # For autoconf 2.5x, must protect trailing spaces with @&t@.
cat > conftest.lit <<EOF
 0000 10000000 00000000 017a0001 781a0004  .........z..x...
 0010 01000000 12000000 18000000 00000000  ................
 0020 08000000 04080000 0044               .........D      @&t@
EOF
cat > conftest.big <<EOF
 0000 00000010 00000000 017a0001 781a0004  .........z..x...
 0010 00000001 00000012 00000018 00000000  ................
 0020 00000008 04000000 0844               .........D      @&t@
EOF
  # If the assembler didn't choke, and we can objdump,
  # and we got the correct data, then succeed.
  if test x$gcc_cv_objdump != x \
  && $gcc_cv_objdump -s -j .eh_frame conftest.o 2>/dev/null \
     | tail -3 > conftest.got \
  && { cmp conftest.lit conftest.got > /dev/null 2>&1 \
    || cmp conftest.big conftest.got > /dev/null 2>&1; }
  then
    gcc_cv_as_eh_frame=yes
  elif AC_TRY_COMMAND($gcc_cv_as -o conftest.o --traditional-format /dev/null); then
    gcc_cv_as_eh_frame=buggy
  else
    # Uh oh, what do we do now?
    gcc_cv_as_eh_frame=no
  fi])

if test $gcc_cv_as_eh_frame = buggy; then
  AC_DEFINE(USE_AS_TRADITIONAL_FORMAT, 1,
  [Define if your assembler mis-optimizes .eh_frame data.])
fi

gcc_GAS_CHECK_FEATURE(section merging support, gcc_cv_as_shf_merge,
 [elf,2,12,0], [--fatal-warnings],
 [.section .rodata.str, "aMS", @progbits, 1])
AC_DEFINE_UNQUOTED(HAVE_GAS_SHF_MERGE,
  [`if test $gcc_cv_as_shf_merge = yes; then echo 1; else echo 0; fi`],
[Define 0/1 if your assembler supports marking sections with SHF_MERGE flag.])

# Thread-local storage - the check is heavily parametrized.
conftest_s=
tls_first_major=
tls_first_minor=
tls_as_opt=
case "$target" in
changequote(,)dnl
  alpha*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	ldq	$27,__tls_get_addr($29)		!literal!1
	lda	$16,foo($29)			!tlsgd!1
	jsr	$26,($27),__tls_get_addr	!lituse_tlsgd!1
	ldq	$27,__tls_get_addr($29)		!literal!2
	lda	$16,foo($29)			!tlsldm!2
	jsr	$26,($27),__tls_get_addr	!lituse_tlsldm!2
	ldq	$1,foo($29)			!gotdtprel
	ldah	$2,foo($29)			!dtprelhi
	lda	$3,foo($2)			!dtprello
	lda	$4,foo($29)			!dtprel
	ldq	$1,foo($29)			!gottprel
	ldah	$2,foo($29)			!tprelhi
	lda	$3,foo($2)			!tprello
	lda	$4,foo($29)			!tprel'
	tls_first_major=2
	tls_first_minor=13
	tls_as_opt=--fatal-warnings
	;;
  i[34567]86-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	movl	%gs:0, %eax
	leal	foo@TLSGD(,%ebx,1), %eax
	leal	foo@TLSLDM(%ebx), %eax
	leal	foo@DTPOFF(%eax), %edx
	movl	foo@GOTTPOFF(%ebx), %eax
	subl	foo@GOTTPOFF(%ebx), %eax
	addl	foo@GOTNTPOFF(%ebx), %eax
	movl	foo@INDNTPOFF, %eax
	movl	$foo@TPOFF, %eax
	subl	$foo@TPOFF, %eax
	leal	foo@NTPOFF(%ecx), %eax'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt=--fatal-warnings
	;;
  x86_64-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	movq	%fs:0, %rax
	leaq	foo@TLSGD(%rip), %rdi
	leaq	foo@TLSLD(%rip), %rdi
	leaq	foo@DTPOFF(%rax), %rdx
	movq	foo@GOTTPOFF(%rip), %rax
	movq	$foo@TPOFF, %rax'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt=--fatal-warnings
	;;
  ia64-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	data8	25
	.text
	addl	r16 = @ltoff(@dtpmod(foo#)), gp
	addl	r17 = @ltoff(@dtprel(foo#)), gp
	addl	r18 = @ltoff(@tprel(foo#)), gp
	addl	r19 = @dtprel(foo#), gp
	adds	r21 = @dtprel(foo#), r13
	movl	r23 = @dtprel(foo#)
	addl	r20 = @tprel(foo#), gp
	adds	r22 = @tprel(foo#), r13
	movl	r24 = @tprel(foo#)'
	tls_first_major=2
	tls_first_minor=13
	tls_as_opt=--fatal-warnings
	;;
  powerpc-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
	.align 2
ld0:	.space 4
ld1:	.space 4
x1:	.space 4
x2:	.space 4
x3:	.space 4
	.text
	addi 3,31,ld0@got@tlsgd
	bl __tls_get_addr
	addi 3,31,x1@got@tlsld
	bl __tls_get_addr
	addi 9,3,x1@dtprel
	addis 9,3,x2@dtprel@ha
	addi 9,9,x2@dtprel@l
	lwz 9,x3@got@tprel(31)
	add 9,9,x@tls
	addi 9,2,x1@tprel
	addis 9,2,x2@tprel@ha
	addi 9,9,x2@tprel@l'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt="-a32 --fatal-warnings"
	;;
  powerpc64-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
	.align 3
ld0:	.space 8
ld1:	.space 8
x1:	.space 8
x2:	.space 8
x3:	.space 8
	.text
	addi 3,2,ld0@got@tlsgd
	bl .__tls_get_addr
	nop
	addi 3,2,ld1@toc
	bl .__tls_get_addr
	nop
	addi 3,2,x1@got@tlsld
	bl .__tls_get_addr
	nop
	addi 9,3,x1@dtprel
	bl .__tls_get_addr
	nop
	addis 9,3,x2@dtprel@ha
	addi 9,9,x2@dtprel@l
	bl .__tls_get_addr
	nop
	ld 9,x3@got@dtprel(2)
	add 9,9,3
	bl .__tls_get_addr
	nop'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt="-a64 --fatal-warnings"
	;;
  s390-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	.long	foo@TLSGD
	.long	foo@TLSLDM
	.long	foo@DTPOFF
	.long	foo@NTPOFF
	.long	foo@GOTNTPOFF
	.long	foo@INDNTPOFF
	l	%r1,foo@GOTNTPOFF(%r12)
	l	%r1,0(%r1):tls_load:foo
	bas	%r14,0(%r1,%r13):tls_gdcall:foo
	bas	%r14,0(%r1,%r13):tls_ldcall:foo'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt="-m31 --fatal-warnings"
	;;
  s390x-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	.quad	foo@TLSGD
	.quad	foo@TLSLDM
	.quad	foo@DTPOFF
	.quad	foo@NTPOFF
	.quad	foo@GOTNTPOFF
	lg	%r1,foo@GOTNTPOFF(%r12)
	larl	%r1,foo@INDNTPOFF
	brasl	%r14,__tls_get_offset@PLT:tls_gdcall:foo
	brasl	%r14,__tls_get_offset@PLT:tls_ldcall:foo'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt="-m64 -Aesame --fatal-warnings"
	;;
  sh-*-* | sh[34]-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	.long	foo@TLSGD
	.long	foo@TLSLDM
	.long	foo@DTPOFF
	.long	foo@GOTTPOFF
	.long	foo@TPOFF'
	tls_first_major=2
	tls_first_minor=13
	tls_as_opt=--fatal-warnings
	;;
  sparc*-*-*)
    case "$target" in
      sparc*-sun-solaris2.*)
	on_solaris=yes
	;;
      *)
	on_solaris=no
	;;
    esac
    if test x$on_solaris = xyes && test x$gas_flag = xno; then
      conftest_s='
	.section ".tdata",#alloc,#write,#tls
foo:	.long	25
	.text
	sethi	%tgd_hi22(foo), %o0
	add	%o0, %tgd_lo10(foo), %o1
	add	%l7, %o1, %o0, %tgd_add(foo)
	call	__tls_get_addr, %tgd_call(foo)
	sethi	%tldm_hi22(foo), %l1
	add	%l1, %tldm_lo10(foo), %l2
	add	%l7, %l2, %o0, %tldm_add(foo)
	call	__tls_get_addr, %tldm_call(foo)
	sethi	%tldo_hix22(foo), %l3
	xor	%l3, %tldo_lox10(foo), %l4
	add	%o0, %l4, %l5, %tldo_add(foo)
	sethi	%tie_hi22(foo), %o3
	add	%o3, %tie_lo10(foo), %o3
	ld	[%l7 + %o3], %o2, %tie_ld(foo)
	add	%g7, %o2, %o4, %tie_add(foo)
	sethi	%tle_hix22(foo), %l1
	xor	%l1, %tle_lox10(foo), %o5
	ld	[%g7 + %o5], %o1'
	tls_first_major=0
	tls_first_minor=0
    else
      conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	sethi	%tgd_hi22(foo), %o0
	add	%o0, %tgd_lo10(foo), %o1
	add	%l7, %o1, %o0, %tgd_add(foo)
	call	__tls_get_addr, %tgd_call(foo)
	sethi	%tldm_hi22(foo), %l1
	add	%l1, %tldm_lo10(foo), %l2
	add	%l7, %l2, %o0, %tldm_add(foo)
	call	__tls_get_addr, %tldm_call(foo)
	sethi	%tldo_hix22(foo), %l3
	xor	%l3, %tldo_lox10(foo), %l4
	add	%o0, %l4, %l5, %tldo_add(foo)
	sethi	%tie_hi22(foo), %o3
	add	%o3, %tie_lo10(foo), %o3
	ld	[%l7 + %o3], %o2, %tie_ld(foo)
	add	%g7, %o2, %o4, %tie_add(foo)
	sethi	%tle_hix22(foo), %l1
	xor	%l1, %tle_lox10(foo), %o5
	ld	[%g7 + %o5], %o1'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt="-32 --fatal-warnings"
      fi
	;;
changequote([,])dnl
esac
if test -z "$tls_first_major"; then
  : # If we don't have a check, assume no support.
else
  gcc_GAS_CHECK_FEATURE(thread-local storage support, gcc_cv_as_tls,
  [$tls_first_major,$tls_first_minor,0], [$tls_as_opt], [$conftest_s],,
  [AC_DEFINE(HAVE_AS_TLS, 1,
	    [Define if your assembler supports thread-local storage.])])
fi

# Target-specific assembler checks.

case "$target" in
  # All TARGET_ABI_OSF targets.
  alpha*-*-osf* | alpha*-*-linux* | alpha*-*-*bsd*)
    gcc_GAS_CHECK_FEATURE([explicit relocation support],
	gcc_cv_as_alpha_explicit_relocs, [2,12,0],,
[	.set nomacro
	.text
	extbl	$3, $2, $3	!lituse_bytoff!1
	ldq	$2, a($29)	!literal!1
	ldq	$4, b($29)	!literal!2
	ldq_u	$3, 0($2)	!lituse_base!1
	ldq	$27, f($29)	!literal!5
	jsr	$26, ($27), f	!lituse_jsr!5
	ldah	$29, 0($26)	!gpdisp!3
	lda	$0, c($29)	!gprel
	ldah	$1, d($29)	!gprelhigh
	lda	$1, d($1)	!gprellow
	lda	$29, 0($29)	!gpdisp!3],,
    [AC_DEFINE(HAVE_AS_EXPLICIT_RELOCS, 1,
  [Define if your assembler supports explicit relocations.])])
    ;;

  cris-*-*)
    gcc_GAS_CHECK_FEATURE([-no-mul-bug-abort option],
      gcc_cv_as_cris_no_mul_bug,[2,15,91],
      [-no-mul-bug-abort], [.text],,
      [AC_DEFINE(HAVE_AS_NO_MUL_BUG_ABORT_OPTION, 1,
		[Define if your assembler supports the -no-mul-bug-abort option.])])
    ;;

  sparc*-*-*)
    gcc_GAS_CHECK_FEATURE([.register], gcc_cv_as_sparc_register_op,,,
      [.register %g2, #scratch],,
      [AC_DEFINE(HAVE_AS_REGISTER_PSEUDO_OP, 1,
		[Define if your assembler supports .register.])])

    gcc_GAS_CHECK_FEATURE([-relax option], gcc_cv_as_sparc_relax,,
      [-relax], [.text],,
      [AC_DEFINE(HAVE_AS_RELAX_OPTION, 1,
		[Define if your assembler supports -relax option.])])

    gcc_GAS_CHECK_FEATURE([unaligned pcrel relocs],
      gcc_cv_as_sparc_ua_pcrel,,
      [-K PIC],
[.text
foo:
	nop
.data
.align 4
.byte 0
.uaword %r_disp32(foo)],
      [if test x$gcc_cv_ld != x \
       && $gcc_cv_ld -o conftest conftest.o -G > /dev/null 2>&1; then
	 gcc_cv_as_sparc_ua_pcrel=yes
       fi
       rm -f conftest],
      [AC_DEFINE(HAVE_AS_SPARC_UA_PCREL, 1,
		[Define if your assembler and linker support unaligned PC relative relocs.])

      gcc_GAS_CHECK_FEATURE([unaligned pcrel relocs against hidden symbols],
	gcc_cv_as_sparc_ua_pcrel_hidden,,
	[-K PIC],
[.data
.align 4
.byte 0x31
.uaword %r_disp32(foo)
.byte 0x32, 0x33, 0x34
.global foo
.hidden foo
foo:
.skip 4],
	[if test x$gcc_cv_ld != x && test x$gcc_cv_objdump != x \
	 && $gcc_cv_ld -o conftest conftest.o -G > /dev/null 2>&1 \
	 && $gcc_cv_objdump -s -j .data conftest 2> /dev/null \
	    | grep ' 31000000 07323334' > /dev/null 2>&1; then
	    if $gcc_cv_objdump -R conftest 2> /dev/null \
	       | grep 'DISP32' > /dev/null 2>&1; then
		:
	    else
		gcc_cv_as_sparc_ua_pcrel_hidden=yes
	    fi
	 fi
	 rm -f conftest],
	 [AC_DEFINE(HAVE_AS_SPARC_UA_PCREL_HIDDEN, 1,
		   [Define if your assembler and linker support unaligned PC relative relocs against hidden symbols.])])
    ]) # unaligned pcrel relocs

    gcc_GAS_CHECK_FEATURE([offsetable %lo()],
      gcc_cv_as_sparc_offsetable_lo10,,
      [-xarch=v9],
[.text
	or %g1, %lo(ab) + 12, %g1
	or %g1, %lo(ab + 12), %g1],
      [if test x$gcc_cv_objdump != x \
       && $gcc_cv_objdump -s -j .text conftest.o 2> /dev/null \
          | grep ' 82106000 82106000' > /dev/null 2>&1; then
	 gcc_cv_as_offsetable_lo10=yes
       fi],
       [AC_DEFINE(HAVE_AS_OFFSETABLE_LO10, 1,
	         [Define if your assembler supports offsetable %lo().])])
    ;;

changequote(,)dnl
  i[34567]86-*-* | x86_64-*-*)
changequote([,])dnl
    gcc_GAS_CHECK_FEATURE([filds and fists mnemonics],
       gcc_cv_as_ix86_filds_fists,
      [2,9,0],, [filds mem; fists mem],,
      [AC_DEFINE(HAVE_GAS_FILDS_FISTS, 1,
        [Define if your assembler uses the new HImode fild and fist notation.])])

    gcc_GAS_CHECK_FEATURE([cmov syntax],
      gcc_cv_as_ix86_cmov_sun_syntax,,,
      [cmovl.l %edx, %eax],,
      [AC_DEFINE(HAVE_AS_IX86_CMOV_SUN_SYNTAX, 1,
        [Define if your assembler supports the Sun syntax for cmov.])])

    # This one is used unconditionally by i386.[ch]; it is to be defined
    # to 1 if the feature is present, 0 otherwise.
    gcc_GAS_CHECK_FEATURE([GOTOFF in data],
        gcc_cv_as_ix86_gotoff_in_data, [2,11,0],,
[	.text
.L0:
	nop
	.data
	.long .L0@GOTOFF])
    AC_DEFINE_UNQUOTED(HAVE_AS_GOTOFF_IN_DATA,
      [`if test $gcc_cv_as_ix86_gotoff_in_data = yes; then echo 1; else echo 0; fi`],
      [Define true if the assembler supports '.long foo@GOTOFF'.])
    ;;

  ia64*-*-*)
    gcc_GAS_CHECK_FEATURE([ltoffx and ldxmov relocs],
	gcc_cv_as_ia64_ltoffx_ldxmov_relocs, [2,14,0],,
[	.text
	addl r15 = @ltoffx(x#), gp
	;;
	ld8.mov r16 = [[r15]], x#],,
    [AC_DEFINE(HAVE_AS_LTOFFX_LDXMOV_RELOCS, 1,
	  [Define if your assembler supports ltoffx and ldxmov relocations.])])

    ;;

  powerpc*-*-*)
    case $target in
      *-*-aix*) conftest_s='	.csect .text[[PR]]
	mfcr 3,128';;
      *-*-darwin*) conftest_s='	.text
	mfcr r3,128';;
      *)       conftest_s='	.text
	mfcr 3,128';;
    esac

    gcc_GAS_CHECK_FEATURE([mfcr field support],
      gcc_cv_as_powerpc_mfcrf, [2,14,0],,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_MFCRF, 1,
	  [Define if your assembler supports mfcr field.])])
    ;;

  mips*-*-*)
    gcc_GAS_CHECK_FEATURE([explicit relocation support],
      gcc_cv_as_mips_explicit_relocs, [2,14,0],,
[	lw $4,%gp_rel(foo)($4)],,
      [if test x$target_cpu_default = x
       then target_cpu_default=MASK_EXPLICIT_RELOCS
       else target_cpu_default="($target_cpu_default)|MASK_EXPLICIT_RELOCS"
       fi])

    ;;
esac
# ??? Not all targets support dwarf2 debug_line, even within a version
# of gas.  Moreover, we need to emit a valid instruction to trigger any
# info to the output file.  So, as supported targets are added to gas 2.11,
# add some instruction here to (also) show we expect this might work.
# ??? Once 2.11 is released, probably need to add first known working
# version to the per-target configury.
case "$target" in
  i?86*-*-* | mips*-*-* | alpha*-*-* | powerpc*-*-* | sparc*-*-* | m68*-*-* \
  | x86_64*-*-* | hppa*-*-* | arm*-*-* | strongarm*-*-* | xscale*-*-* \
  | xstormy16*-*-* | cris-*-* | xtensa-*-*)
    insn="nop"
    ;;
  ia64*-*-* | s390*-*-*)
    insn="nop 0"
    ;;
  mmix-*-*)
    insn="swym 0"
    ;;
esac
if test x"$insn" != x; then
 conftest_s="\
	.file 1 \"conftest.s\"
	.loc 1 3 0
	$insn"
 gcc_GAS_CHECK_FEATURE([dwarf2 debug_line support],
  gcc_cv_as_dwarf2_debug_line,
  [elf,2,11,0],, [$conftest_s],
  [# ??? This fails with non-gnu grep.  Maybe use objdump?
   if grep debug_line conftest.o > /dev/null 2>&1; then
     gcc_cv_as_dwarf2_debug_line=yes
   fi])

# The .debug_line file table must be in the exact order that
# we specified the files, since these indices are also used
# by DW_AT_decl_file.  Approximate this test by testing if
# the assembler bitches if the same index is assigned twice.
 gcc_GAS_CHECK_FEATURE([buggy dwarf2 .file directive],
  gcc_cv_as_dwarf2_file_buggy,,,
[	.file 1 "foo.s"
	.file 1 "bar.s"])

 if test $gcc_cv_as_dwarf2_debug_line = yes \
 && test $gcc_cv_as_dwarf2_file_buggy = no; then
	AC_DEFINE(HAVE_AS_DWARF2_DEBUG_LINE, 1,
  [Define if your assembler supports dwarf2 .file/.loc directives,
   and preserves file table indices exactly as given.])
 fi

 gcc_GAS_CHECK_FEATURE([--gdwarf2 option],
  gcc_cv_as_gdwarf2_flag,
  [elf,2,11,0], [--gdwarf2], [$insn],,
  [AC_DEFINE(HAVE_AS_GDWARF2_DEBUG_FLAG, 1,
[Define if your assembler supports the --gdwarf2 option.])])

 gcc_GAS_CHECK_FEATURE([--gstabs option],
  gcc_cv_as_gstabs_flag,
  [elf,2,11,0], [--gstabs], [$insn],
  [# The native Solaris 9/Intel assembler doesn't understand --gstabs
   # and warns about it, but still exits successfully.  So check for
   # this.
   if AC_TRY_COMMAND([$gcc_cv_as --gstabs -o conftest.o conftest.s 2>&1 | grep -i warning > /dev/null])
   then :
   else gcc_cv_as_gstabs_flag=yes
   fi],
  [AC_DEFINE(HAVE_AS_GSTABS_DEBUG_FLAG, 1,
[Define if your assembler supports the --gstabs option.])])
fi

AC_MSG_CHECKING(linker read-only and read-write section mixing)
gcc_cv_ld_ro_rw_mix=unknown
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 10 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_ro_rw_mix=read-write
  fi
elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x -a x$gcc_cv_objdump != x ; then
  echo '.section myfoosect, "a"' > conftest1.s
  echo '.section myfoosect, "aw"' > conftest2.s
  echo '.byte 1' >> conftest2.s
  echo '.section myfoosect, "a"' > conftest3.s
  echo '.byte 0' >> conftest3.s
  if $gcc_cv_as -o conftest1.o conftest1.s > /dev/null 2>&1 \
     && $gcc_cv_as -o conftest2.o conftest2.s > /dev/null 2>&1 \
     && $gcc_cv_as -o conftest3.o conftest3.s > /dev/null 2>&1 \
     && $gcc_cv_ld -shared -o conftest1.so conftest1.o \
	conftest2.o conftest3.o > /dev/null 2>&1; then
    gcc_cv_ld_ro_rw_mix=`$gcc_cv_objdump -h conftest1.so \
			 | sed -e '/myfoosect/!d' -e N`
    if echo "$gcc_cv_ld_ro_rw_mix" | grep CONTENTS > /dev/null; then
      if echo "$gcc_cv_ld_ro_rw_mix" | grep READONLY > /dev/null; then
	gcc_cv_ld_ro_rw_mix=read-only
      else
	gcc_cv_ld_ro_rw_mix=read-write
      fi
    fi
  fi
changequote(,)dnl
  rm -f conftest.* conftest[123].*
changequote([,])dnl
fi
if test x$gcc_cv_ld_ro_rw_mix = xread-write; then
	AC_DEFINE(HAVE_LD_RO_RW_SECTION_MIXING, 1,
  [Define if your linker links a mix of read-only
   and read-write sections into a read-write section.])
fi
AC_MSG_RESULT($gcc_cv_ld_ro_rw_mix)

AC_MSG_CHECKING(linker PT_GNU_EH_FRAME support)
gcc_cv_ld_eh_frame_hdr=no
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 12 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_eh_frame_hdr=yes
  fi
elif test x$gcc_cv_ld != x; then
	# Check if linker supports --eh-frame-hdr option
	if $gcc_cv_ld --help 2>/dev/null | grep eh-frame-hdr > /dev/null; then
		gcc_cv_ld_eh_frame_hdr=yes
	fi
fi
if test x"$gcc_cv_ld_eh_frame_hdr" = xyes; then
	AC_DEFINE(HAVE_LD_EH_FRAME_HDR, 1,
[Define if your linker supports --eh-frame-hdr option.])
fi
AC_MSG_RESULT($gcc_cv_ld_eh_frame_hdr)

AC_MSG_CHECKING(linker position independent executable support)
gcc_cv_ld_pie=no
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 15 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_pie=yes
  fi
elif test x$gcc_cv_ld != x; then
	# Check if linker supports -pie option
	if $gcc_cv_ld --help 2>/dev/null | grep -- -pie > /dev/null; then
		gcc_cv_ld_pie=yes
	fi
fi
if test x"$gcc_cv_ld_pie" = xyes; then
	AC_DEFINE(HAVE_LD_PIE, 1,
[Define if your linker supports -pie option.])
fi
AC_MSG_RESULT($gcc_cv_ld_pie)

case "$target" in
  *-*-linux*)
    AC_CACHE_CHECK(linker --as-needed support,
    gcc_cv_ld_as_needed,
    [gcc_cv_ld_as_needed=no
    if test $in_tree_ld = yes ; then
      if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 16 -o "$gcc_cv_gld_major_version" -gt 2 \
         && test $in_tree_ld_is_elf = yes; then
        gcc_cv_ld_as_needed=yes
      fi
    elif test x$gcc_cv_ld != x; then
      # Check if linker supports --as-needed and --no-as-needed options
      if $gcc_cv_ld --help 2>/dev/null | grep as-needed > /dev/null; then
        gcc_cv_ld_as_needed=yes
      fi
    fi
    ])
    if test x"$gcc_cv_ld_as_needed" = xyes; then
      AC_DEFINE(HAVE_LD_AS_NEEDED, 1,
    [Define if your linker supports --as-needed and --no-as-needed options.])
    fi
    ;;
esac

if test x$with_sysroot = x && test x$host = x$target \
   && test "$prefix" != "/usr" && test "x$prefix" != "x$local_prefix" ; then
  AC_DEFINE_UNQUOTED(PREFIX_INCLUDE_DIR, "$prefix/include",
[Define to PREFIX/include if cpp should also search that directory.])
fi

# Figure out what language subdirectories are present.
# Look if the user specified --enable-languages="..."; if not, use
# the environment variable $LANGUAGES if defined. $LANGUAGES might
# go away some day.
# NB:  embedded tabs in this IF block -- do not untabify
if test x"${enable_languages+set}" != xset; then
	if test x"${LANGUAGES+set}" = xset; then
		enable_languages="${LANGUAGES}"
		AC_MSG_WARN([setting LANGUAGES is deprecated, use --enable-languages instead])

	else
		enable_languages=all
	fi
else
	if test x"${enable_languages}" = x \
        || test x"${enable_languages}" = xyes;
	then
		AC_MSG_ERROR([--enable-languages needs at least one language argument])
	fi
fi
enable_languages=`echo "${enable_languages}" | sed -e 's/[[ 	,]][[ 	,]]*/,/g' -e 's/,$//'`

# First scan to see if an enabled language requires some other language.
# We assume that a given config-lang.in will list all the language
# front ends it requires, even if some are required indirectly.
for lang in ${srcdir}/*/config-lang.in
do
   case $lang in
    # The odd quoting in the next line works around
    # an apparent bug in bash 1.12 on linux.
changequote(,)dnl
    ${srcdir}/[*]/config-lang.in)
       ;;
    *)
       lang_alias=`sed -n -e 's,^language=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^language=\([^ 	]*\).*$,\1,p' $lang`
       this_lang_requires=`sed -n -e 's,^lang_requires=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^lang_requires=\([^ 	]*\).*$,\1,p' $lang`
       for other in $this_lang_requires
       do
	  case ,${enable_languages}, in
	   *,$other,*)
	      ;;
	   *,all,*)
	      ;;
	   *,$lang_alias,*)
	      enable_languages="$enable_languages,$other"
	      ;;
	  esac
       done
       ;;
changequote([,])dnl
   esac
done

expected_languages=`echo ,${enable_languages}, | sed -e 's:,: :g' -e 's:  *: :g' -e 's:  *: :g' -e 's:^ ::' -e 's: $::'`
found_languages=
subdirs=
for lang in ${srcdir}/*/config-lang.in
do
	case $lang in
	# The odd quoting in the next line works around
	# an apparent bug in bash 1.12 on linux.
changequote(,)dnl
	${srcdir}/[*]/config-lang.in) ;;
	*)
	  lang_alias=`sed -n -e 's,^language=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^language=\([^ 	]*\).*$,\1,p' $lang`
	  this_lang_libs=`sed -n -e 's,^target_libs=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^target_libs=\([^ 	]*\).*$,\1,p' $lang`
	  build_by_default=`sed -n -e 's,^build_by_default=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^build_by_default=\([^ 	]*\).*$,\1,p' $lang`
	  if test "x$lang_alias" = x
	  then
		echo "$lang doesn't set \$language." 1>&2
		exit 1
	  fi
	  case ${build_by_default},${enable_languages}, in
	  *,$lang_alias,*) add_this_lang=yes ;;
	  no,*) add_this_lang=no ;;
	  *,all,*) add_this_lang=yes ;;
	  *) add_this_lang=no ;;
	  esac
          found_languages="${found_languages} ${lang_alias}"
	  if test x"${add_this_lang}" = xyes; then
		case $lang in
		    ${srcdir}/ada/config-lang.in)
			if test x$have_gnat = xyes ; then
				subdirs="$subdirs `echo $lang | sed -e 's,^.*/\([^/]*\)/config-lang.in$,\1,'`"
			fi
			;;
		    *)
			subdirs="$subdirs `echo $lang | sed -e 's,^.*/\([^/]*\)/config-lang.in$,\1,'`"
			;;
		esac
	  fi
	  ;;
changequote([,])dnl
	esac
done

missing_languages=
for expected_language in ${expected_languages} ..
do 
    if test "${expected_language}" != ..; then
        missing_language="${expected_language}"
        if test "${expected_language}" = "c" \
           || test "${expected_language}" = "all"; then
                missing_language=
        fi
        for found_language in ${found_languages} ..
        do 
            if test "${found_language}" != ..; then
                if test "${expected_language}" = "${found_language}"; then
                    missing_language=
                fi
            fi
        done
        if test "x${missing_language}" != x; then
           missing_languages="${missing_languages} ${missing_language}"
        fi
    fi
done

if test "x$missing_languages" != x; then
  AC_MSG_ERROR([
The following requested languages were not found:${missing_languages}
The following languages were available: c${found_languages}])
fi

# Make gthr-default.h if we have a thread file.
gthread_flags=
if test $thread_file != single; then
    rm -f gthr-default.h
    echo "#include \"gthr-${thread_file}.h\"" > gthr-default.h
    gthread_flags=-DHAVE_GTHR_DEFAULT
fi
AC_SUBST(gthread_flags)

# Find out what GC implementation we want, or may, use.
AC_ARG_WITH(gc,
[  --with-gc={simple,page,zone} choose the garbage collection mechanism to use
                          with the compiler],
[case "$withval" in
  simple | page | zone)
    GGC=ggc-$withval
    ;;
  *)
    AC_MSG_ERROR([$withval is an invalid option to --with-gc])
    ;;
esac],
[GGC=ggc-page])
AC_SUBST(GGC)
echo "Using $GGC for garbage collection."

# Use the system's zlib library.
zlibdir=-L../zlib
zlibinc="-I\$(srcdir)/../zlib"
AC_ARG_WITH(system-zlib,
[  --with-system-zlib      use installed libz],
zlibdir=
zlibinc=
)
AC_SUBST(zlibdir)
AC_SUBST(zlibinc)

dnl Very limited version of automake's enable-maintainer-mode

AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode
                          enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      maintainer_mode=$enableval,
      maintainer_mode=no)

AC_MSG_RESULT($maintainer_mode)

if test "$maintainer_mode" = "yes"; then
  MAINT=''
else
  MAINT='#'
fi
AC_SUBST(MAINT)dnl

# Make empty files to contain the specs and options for each language.
# Then add #include lines to for a compiler that has specs and/or options.

lang_opt_files=
lang_specs_files=
lang_tree_files=
for subdir in . $subdirs
do
	if test -f $srcdir/$subdir/lang.opt; then
	    lang_opt_files="$lang_opt_files $srcdir/$subdir/lang.opt"
	fi
	if test -f $srcdir/$subdir/lang-specs.h; then
	    lang_specs_files="$lang_specs_files $srcdir/$subdir/lang-specs.h"
	fi
	if test -f $srcdir/$subdir/$subdir-tree.def; then
	    lang_tree_files="$lang_tree_files $srcdir/$subdir/$subdir-tree.def"
	fi
done

# These (without "all_") are set in each config-lang.in.
# `language' must be a single word so is spelled singularly.
all_languages=
all_boot_languages=
all_compilers=
all_stagestuff=
all_outputs='Makefile fixinc/Makefile gccbug mklibgcc mkheaders'
# List of language makefile fragments.
all_lang_makefrags=
# List of language subdirectory makefiles.  Deprecated.
all_lang_makefiles=
# Files for gengtype
all_gtfiles="$target_gtfiles"
# Files for gengtype with language
all_gtfiles_files_langs=
all_gtfiles_files_files=

# Add the language fragments.
# Languages are added via two mechanisms.  Some information must be
# recorded in makefile variables, these are defined in config-lang.in.
# We accumulate them and plug them into the main Makefile.
# The other mechanism is a set of hooks for each of the main targets
# like `clean', `install', etc.

language_hooks="Make-hooks"

for s in $subdirs
do
		language=
		boot_language=
		compilers=
		stagestuff=
		outputs=
		gtfiles=
		. ${srcdir}/$s/config-lang.in
		if test "x$language" = x
		then
			echo "${srcdir}/$s/config-lang.in doesn't set \$language." 1>&2
			exit 1
		fi
		all_lang_makefrags="$all_lang_makefrags \$(srcdir)/$s/Make-lang.in"
		if test -f ${srcdir}/$s/Makefile.in
		then all_lang_makefiles="$s/Makefile"
		fi
		all_languages="$all_languages $language"
		if test "x$boot_language" = xyes
		then
			all_boot_languages="$all_boot_languages $language"
		fi
		all_compilers="$all_compilers $compilers"
		all_stagestuff="$all_stagestuff $stagestuff"
		all_outputs="$all_outputs $outputs"
		all_gtfiles="$all_gtfiles $gtfiles"
                for f in $gtfiles
                do
                         all_gtfiles_files_langs="$all_gtfiles_files_langs ${s} "
                         all_gtfiles_files_files="$all_gtfiles_files_files ${f} "
                done
done

# Pick up gtfiles for c
gtfiles=
s="c"
. ${srcdir}/c-config-lang.in
all_gtfiles="$all_gtfiles $gtfiles"
for f in $gtfiles
do
        all_gtfiles_files_langs="$all_gtfiles_files_langs ${s} "
        all_gtfiles_files_files="$all_gtfiles_files_files ${f} "
done

check_languages=
for language in $all_languages
do
		check_languages="$check_languages check-$language"
done

# We link each language in with a set of hooks, reached indirectly via
# lang.${target}.

rm -f Make-hooks
touch Make-hooks
target_list="all.build all.cross start.encap rest.encap tags \
	install-normal install-common install-man \
	uninstall info man srcextra srcman srcinfo \
	mostlyclean clean distclean maintainer-clean \
	stage1 stage2 stage3 stage4 stageprofile stagefeedback"
for t in $target_list
do
	x=
	for lang in $all_languages
	do
			x="$x $lang.$t"
	done
	echo "lang.$t: $x" >> Make-hooks
done

# Create .gdbinit.

echo "dir ." > .gdbinit
echo "dir ${srcdir}" >> .gdbinit
if test x$gdb_needs_out_file_path = xyes
then
	echo "dir ${srcdir}/config/"`dirname ${out_file}` >> .gdbinit
fi
if test "x$subdirs" != x; then
	for s in $subdirs
	do
		echo "dir ${srcdir}/$s" >> .gdbinit
	done
fi
echo "source ${srcdir}/gdbinit.in" >> .gdbinit

# If $(exec_prefix) exists and is not the same as $(prefix), then compute an
# absolute path for gcc_tooldir based on inserting the number of up-directory
# movements required to get from $(exec_prefix) to $(prefix) into the basic
# $(libsubdir)/@(unlibsubdir) based path.
# Don't set gcc_tooldir to tooldir since that's only passed in by the toplevel
# make and thus we'd get different behavior depending on where we built the
# sources.
if test x$exec_prefix = xNONE -o x$exec_prefix = x$prefix; then
    gcc_tooldir='$(libsubdir)/$(unlibsubdir)/../$(target_noncanonical)'
else
changequote(<<, >>)dnl
# An explanation of the sed strings:
#  -e 's|^\$(prefix)||'   matches and eliminates 'prefix' from 'exec_prefix'
#  -e 's|/$||'            match a trailing forward slash and eliminates it
#  -e 's|^[^/]|/|'        forces the string to start with a forward slash (*)
#  -e 's|/[^/]*|../|g'    replaces each occurrence of /<directory> with ../
#
# (*) Note this pattern overwrites the first character of the string
# with a forward slash if one is not already present.  This is not a
# problem because the exact names of the sub-directories concerned is
# unimportant, just the number of them matters.
#
# The practical upshot of these patterns is like this:
#
#  prefix     exec_prefix        result
#  ------     -----------        ------
#   /foo        /foo/bar          ../
#   /foo/       /foo/bar          ../
#   /foo        /foo/bar/         ../
#   /foo/       /foo/bar/         ../
#   /foo        /foo/bar/ugg      ../../
#
    dollar='$$'
    gcc_tooldir="\$(libsubdir)/\$(unlibsubdir)/\`echo \$(exec_prefix) | sed -e 's|^\$(prefix)||' -e 's|/\$(dollar)||' -e 's|^[^/]|/|' -e 's|/[^/]*|../|g'\`\$(target_noncanonical)"
changequote([, ])dnl
fi
AC_SUBST(gcc_tooldir)
AC_SUBST(dollar)

# Find a directory in which to install a shared libgcc.

AC_ARG_ENABLE(version-specific-runtime-libs,
[  --enable-version-specific-runtime-libs
                          specify that runtime libraries should be
                          installed in a compiler-specific directory])

AC_ARG_WITH(slibdir,
[  --with-slibdir=DIR      shared libraries in DIR [LIBDIR]],
slibdir="$with_slibdir",
if test "${enable_version_specific_runtime_libs+set}" = set; then
  slibdir='$(libsubdir)'
elif test "$host" != "$target"; then
  slibdir='$(build_tooldir)/lib'
else
  slibdir='$(libdir)'
fi)
AC_SUBST(slibdir)

objdir=`${PWDCMD-pwd}`
AC_SUBST(objdir)

# Substitute configuration variables
AC_SUBST(subdirs)
AC_SUBST(srcdir)
AC_SUBST(all_boot_languages)
AC_SUBST(all_compilers)
AC_SUBST(all_gtfiles)
AC_SUBST(all_gtfiles_files_langs)
AC_SUBST(all_gtfiles_files_files)
AC_SUBST(all_lang_makefrags)
AC_SUBST(all_lang_makefiles)
AC_SUBST(all_languages)
AC_SUBST(all_stagestuff)
AC_SUBST(build_exeext)
AC_SUBST(build_install_headers_dir)
AC_SUBST(build_xm_file_list)
AC_SUBST(build_xm_include_list)
AC_SUBST(build_xm_defines)
AC_SUBST(check_languages)
AC_SUBST(cc_set_by_configure)
AC_SUBST(quoted_cc_set_by_configure)
AC_SUBST(cpp_install_dir)
AC_SUBST(xmake_file)
AC_SUBST(tmake_file)
AC_SUBST(extra_gcc_objs)
AC_SUBST(extra_headers_list)
AC_SUBST(extra_objs)
AC_SUBST(extra_parts)
AC_SUBST(extra_passes)
AC_SUBST(extra_programs)
AC_SUBST(float_h_file)
AC_SUBST(gcc_config_arguments)
AC_SUBST(gcc_gxx_include_dir)
AC_SUBST(libstdcxx_incdir)
AC_SUBST(gcc_version)
AC_SUBST(gcc_version_full)
AC_SUBST(gcc_version_trigger)
AC_SUBST(host_exeext)
AC_SUBST(host_xm_file_list)
AC_SUBST(host_xm_include_list)
AC_SUBST(host_xm_defines)
AC_SUBST(out_host_hook_obj)
AC_SUBST(install)
AC_SUBST(lang_opt_files)
AC_SUBST(lang_specs_files)
AC_SUBST(lang_tree_files)
AC_SUBST(local_prefix)
AC_SUBST(md_file)
AC_SUBST(objc_boehm_gc)
AC_SUBST(out_file)
AC_SUBST(out_object_file)
AC_SUBST(stage_prefix_set_by_configure)
AC_SUBST(quoted_stage_prefix_set_by_configure)
AC_SUBST(symbolic_link)
AC_SUBST(thread_file)
AC_SUBST(tm_file_list)
AC_SUBST(tm_include_list)
AC_SUBST(tm_defines)
AC_SUBST(tm_p_file_list)
AC_SUBST(tm_p_include_list)
AC_SUBST(xm_file_list)
AC_SUBST(xm_include_list)
AC_SUBST(xm_defines)
AC_SUBST(target_noncanonical)
AC_SUBST(c_target_objs)
AC_SUBST(cxx_target_objs)
AC_SUBST(target_cpu_default)

AC_SUBST_FILE(language_hooks)

# If it doesn't already exist, create document directory
echo "checking for the document directory." 1>&2
if test -d doc ; then
  true
else
  mkdir doc 
fi

# Echo link setup.
if test x${build} = x${host} ; then
  if test x${host} = x${target} ; then
    echo "Links are now set up to build a native compiler for ${target}." 1>&2
  else
    echo "Links are now set up to build a cross-compiler" 1>&2
    echo " from ${host} to ${target}." 1>&2
  fi
else
  if test x${host} = x${target} ; then
    echo "Links are now set up to build (on ${build}) a native compiler" 1>&2
    echo " for ${target}." 1>&2
  else
    echo "Links are now set up to build (on ${build}) a cross-compiler" 1>&2
    echo " from ${host} to ${target}." 1>&2
  fi
fi

# Configure the subdirectories
# AC_CONFIG_SUBDIRS($subdirs)

# Create the Makefile
# and configure language subdirectories
AC_CONFIG_FILES($all_outputs)

AC_CONFIG_COMMANDS([default],
[
case ${CONFIG_HEADERS} in
  *auto-host.h:config.in*)
  echo > cstamp-h ;;
esac
# Make sure all the subdirs exist.
for d in $subdirs
do
    test -d $d || mkdir $d
done
# If the host supports symlinks, point stage[1234] at ../stage[1234] so
# bootstrapping and the installation procedure can still use
# CC="stage1/xgcc -Bstage1/".  If the host doesn't support symlinks,
# FLAGS_TO_PASS has been modified to solve the problem there.
# This is virtually a duplicate of what happens in configure.lang; we do
# an extra check to make sure this only happens if ln -s can be used.
if test "$symbolic_link" = "ln -s"; then
 for d in ${subdirs} fixinc ; do
	STARTDIR=`${PWDCMD-pwd}`
	cd $d
	for t in stage1 stage2 stage3 stage4 stageprofile stagefeedback include
	do
		rm -f $t
		$symbolic_link ../$t $t 2>/dev/null
	done
	cd $STARTDIR
 done
else true ; fi
], 
[subdirs='$subdirs'
symbolic_link='$symbolic_link'
])
AC_OUTPUT
