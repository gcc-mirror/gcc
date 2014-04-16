# configure.ac for GCC
# Process this file with autoconf to generate a configuration script.

# Copyright (C) 1997-2014 Free Software Foundation, Inc.

#This file is part of GCC.

#GCC is free software; you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free
#Software Foundation; either version 3, or (at your option) any later
#version.

#GCC is distributed in the hope that it will be useful, but WITHOUT
#ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#for more details.

#You should have received a copy of the GNU General Public License
#along with GCC; see the file COPYING3.  If not see
#<http://www.gnu.org/licenses/>.

# --------------------------------
# Initialization and sanity checks
# --------------------------------

AC_PREREQ(2.64)
AC_INIT
AC_CONFIG_SRCDIR(tree.c)
AC_CONFIG_HEADER(auto-host.h:config.in)

gcc_version=`cat $srcdir/BASE-VER`

# Determine the host, build, and target systems
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Determine the noncanonical target name, for directory use.
ACX_NONCANONICAL_TARGET

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
[AS_HELP_STRING([--with-local-prefix=DIR],
		[specifies directory to put local include])],
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
[AS_HELP_STRING([--with-gxx-include-dir=DIR],
                [specifies directory to put g++ header files])],
[case "${withval}" in
yes)	AC_MSG_ERROR(bad value ${withval} given for g++ include directory) ;;
no)	;;
*)	gcc_gxx_include_dir=$with_gxx_include_dir ;;
esac])

# This logic must match libstdc++-v3/acinclude.m4:GLIBCXX_EXPORT_INSTALL_INFO.
if test x${gcc_gxx_include_dir} = x; then
  if test x${enable_version_specific_runtime_libs} = xyes; then
    gcc_gxx_include_dir='${libsubdir}/include/c++'
  else
    libstdcxx_incdir='include/c++/$(version)'
    if test x$host != x$target; then
       libstdcxx_incdir="$target_alias/$libstdcxx_incdir"
    fi
    gcc_gxx_include_dir="\$(libsubdir)/\$(libsubdir_to_prefix)$libstdcxx_incdir"
  fi
fi

gcc_gxx_include_dir_add_sysroot=0
if test "${with_sysroot+set}" = set; then
  gcc_gxx_without_sysroot=`expr "${gcc_gxx_include_dir}" : "${with_sysroot}"'\(.*\)'`
  if test "${gcc_gxx_without_sysroot}"; then
    gcc_gxx_include_dir="${gcc_gxx_without_sysroot}"
    gcc_gxx_include_dir_add_sysroot=1
  fi
fi

AC_ARG_WITH(cpp_install_dir,
[AC_HELP_STRING([--with-cpp-install-dir=DIR],
                [install the user visible C preprocessor in DIR
                 (relative to PREFIX) as well as PREFIX/bin])],
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
    [AS_HELP_STRING([--enable-generated-files-in-srcdir],
		    [put copies of generated files in source dir
		     intended for creating source tarballs for users
		     without texinfo bison or flex])],
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
[AS_HELP_STRING([--with-gnu-ld], [arrange to work with GNU ld])],
gnu_ld_flag="$with_gnu_ld",
gnu_ld_flag=no)

# With pre-defined ld
AC_ARG_WITH(ld,
[AS_HELP_STRING([--with-ld], [arrange to use the specified ld (full pathname)])],
DEFAULT_LINKER="$with_ld")
if test x"${DEFAULT_LINKER+set}" = x"set"; then
  if test ! -x "$DEFAULT_LINKER"; then
    AC_MSG_ERROR([cannot execute: $DEFAULT_LINKER: check --with-ld or env. var. DEFAULT_LINKER])
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

# With demangler in GNU ld
AC_ARG_WITH(demangler-in-ld,
[AS_HELP_STRING([--with-demangler-in-ld], [try to use demangler in GNU ld])],
demangler_in_ld="$with_demangler_in_ld",
demangler_in_ld=yes)

# ----------------------
# Find default assembler
# ----------------------

# With GNU as
AC_ARG_WITH(gnu-as,
[AS_HELP_STRING([--with-gnu-as], [arrange to work with GNU as])],
gas_flag="$with_gnu_as",
gas_flag=no)

AC_ARG_WITH(as,
[AS_HELP_STRING([--with-as], [arrange to use the specified as (full pathname)])],
DEFAULT_ASSEMBLER="$with_as")
if test x"${DEFAULT_ASSEMBLER+set}" = x"set"; then
  if test ! -x "$DEFAULT_ASSEMBLER"; then
    AC_MSG_ERROR([cannot execute: $DEFAULT_ASSEMBLER: check --with-as or env. var. DEFAULT_ASSEMBLER])
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
AC_PROG_CXX
ACX_PROG_GNAT([-I"$srcdir"/ada])

# Remove the -O2: for historical reasons, unless bootstrapping we prefer
# optimizations to be activated explicitly by the toplevel.
case "$CC" in
  */prev-gcc/xgcc*) ;;
  *) CFLAGS=`echo "$CFLAGS " | sed -e "s/-Ofast[[ 	]]//" -e "s/-O[[gs]][[ 	]]//" -e "s/-O[[0-9]]*[[ 	]]//" `
     CXXFLAGS=`echo "$CXXFLAGS " | sed -e "s/-Ofast[[ 	]]//" -e "s/-O[[gs]][[ 	]]//" -e "s/-O[[0-9]]*[[ 	]]//" ` ;;
esac
AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)

# Determine PICFLAG for target gnatlib.
GCC_PICFLAG_FOR_TARGET
AC_SUBST(PICFLAG_FOR_TARGET)

# -------------------------
# Check C compiler features
# -------------------------

AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CPP
AC_C_INLINE

AC_SYS_LARGEFILE

# sizeof(char) is 1 by definition.
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_TYPES([long long], [AC_CHECK_SIZEOF(long long)])
AC_CHECK_TYPES([__int64], [AC_CHECK_SIZEOF(__int64)])
GCC_STDINT_TYPES

# ---------------------
# Warnings and checking
# ---------------------

# Check $CC warning features (if it's GCC).
# We want to use -pedantic, but we don't want warnings about
# * 'long long'
# * variadic macros
# * overlong strings
# * C++11 narrowing conversions in { }
# So, we only use -pedantic if we can disable those warnings.

# In stage 1, disable -Wformat warnings from old GCCs about new % codes
AC_ARG_ENABLE(build-format-warnings,
  AS_HELP_STRING([--disable-build-format-warnings],[don't use -Wformat while building GCC]),
  [],[enable_build_format_warnings=yes])
AS_IF([test $enable_build_format_warnings = no],
      [wf_opt=-Wno-format],[wf_opt=])
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-W -Wall -Wno-narrowing -Wwrite-strings -Wcast-qual $wf_opt])), [loose_warn])
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-Wstrict-prototypes -Wmissing-prototypes])),
	[c_loose_warn])
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-Wmissing-format-attribute])), [strict_warn])
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-Wold-style-definition -Wc++-compat])), [c_strict_warn])
ACX_PROG_CC_WARNING_ALMOST_PEDANTIC(
	m4_quote(m4_do([-Wno-long-long -Wno-variadic-macros ], 
		       [-Wno-overlength-strings])), [strict_warn])
ACX_PROG_CC_WARNINGS_ARE_ERRORS([manual], [strict_warn])

# The above macros do nothing if the compiler is not GCC.  However, the
# Makefile has more goo to add other flags, so these variables are used
# to enable warnings only for GCC.
warn_cflags=
warn_cxxflags=
if test "x$GCC" = "xyes"; then
  warn_cflags='$(GCC_WARN_CFLAGS)'
  warn_cxxflags='$(GCC_WARN_CXXFLAGS)'
fi
AC_SUBST(warn_cflags)
AC_SUBST(warn_cxxflags)

# Disable exceptions and RTTI if building with g++
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-fno-exceptions -fno-rtti -fasynchronous-unwind-tables])),
		       [noexception_flags])
	
# Enable expensive internal checks
is_release=
if test x"`cat $srcdir/DEV-PHASE`" != xexperimental; then
  is_release=yes
fi

AC_ARG_ENABLE(checking,
[AS_HELP_STRING([[--enable-checking[=LIST]]],
		[enable expensive run-time checks.  With LIST,
		 enable only specific categories of checks.
		 Categories are: yes,no,all,none,release.
		 Flags are: assert,df,fold,gc,gcac,gimple,misc,
		 rtlflag,rtl,runtime,tree,valgrind,types])],
[ac_checking_flags="${enableval}"],[
# Determine the default checks.
if test x$is_release = x ; then
  ac_checking_flags=yes
else
  ac_checking_flags=release
fi])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in release $ac_checking_flags
do
	case $check in
	# these set all the flags to specific states
	yes)		ac_assert_checking=1 ; ac_checking=1 ; ac_df_checking= ;
			ac_fold_checking= ; ac_gc_checking=1 ;
			ac_gc_always_collect= ; ac_gimple_checking=1 ; ac_rtl_checking= ;
			ac_rtlflag_checking=1 ; ac_runtime_checking=1 ;
			ac_tree_checking=1 ; ac_valgrind_checking= ;
			ac_types_checking=1 ;;
	no|none)	ac_assert_checking= ; ac_checking= ; ac_df_checking= ;
			ac_fold_checking= ; ac_gc_checking= ;
			ac_gc_always_collect= ; ac_gimple_checking= ; ac_rtl_checking= ;
			ac_rtlflag_checking= ; ac_runtime_checking= ;
			ac_tree_checking= ; ac_valgrind_checking= ;
			ac_types_checking= ;;
	all)		ac_assert_checking=1 ; ac_checking=1 ; ac_df_checking=1 ;
			ac_fold_checking=1 ; ac_gc_checking=1 ;
			ac_gc_always_collect=1 ; ac_gimple_checking=1 ; ac_rtl_checking=1 ;
			ac_rtlflag_checking=1 ; ac_runtime_checking=1 ;
			ac_tree_checking=1 ; ac_valgrind_checking= ;
			ac_types_checking=1 ;;
	release)	ac_assert_checking=1 ; ac_checking= ; ac_df_checking= ;
			ac_fold_checking= ; ac_gc_checking= ;
			ac_gc_always_collect= ; ac_gimple_checking= ; ac_rtl_checking= ;
			ac_rtlflag_checking= ; ac_runtime_checking=1 ;
			ac_tree_checking= ; ac_valgrind_checking= ;
			ac_types_checking= ;;
	# these enable particular checks
	assert) 	ac_assert_checking=1 ;;
	df)	 	ac_df_checking=1 ;;
	fold)		ac_fold_checking=1 ;;
	gc)		ac_gc_checking=1 ;;
	gcac)		ac_gc_always_collect=1 ;;
	gimple)		ac_gimple_checking=1 ;;
	misc)		ac_checking=1 ;;
	rtl)		ac_rtl_checking=1 ;;
	rtlflag)	ac_rtlflag_checking=1 ;;
	runtime)	ac_runtime_checking=1 ;;
	tree)		ac_tree_checking=1 ;;
	types)		ac_types_checking=1 ;;
	valgrind)	ac_valgrind_checking=1 ;;
	*)	AC_MSG_ERROR(unknown check category $check) ;;
	esac
done
IFS="$ac_save_IFS"

nocommon_flag=""
if test x$ac_checking != x ; then
  AC_DEFINE(ENABLE_CHECKING, 1,
[Define if you want more run-time sanity checks.  This one gets a grab
   bag of miscellaneous but relatively cheap checks.])
  nocommon_flag=-fno-common
fi
AC_SUBST(nocommon_flag)
if test x$ac_df_checking != x ; then
  AC_DEFINE(ENABLE_DF_CHECKING, 1,
[Define if you want more run-time sanity checks for dataflow.])
fi
if test x$ac_assert_checking != x ; then
  AC_DEFINE(ENABLE_ASSERT_CHECKING, 1,
[Define if you want assertions enabled.  This is a cheap check.])
fi
if test x$ac_gimple_checking != x ; then
  AC_DEFINE(ENABLE_GIMPLE_CHECKING, 1,
[Define if you want operations on GIMPLE (the basic data structure of
the high-level optimizers) to be checked for dynamic type safety at
runtime.  This is moderately expensive.])
fi
GCC_TARGET_TEMPLATE(ENABLE_RUNTIME_CHECKING)
if test x$ac_runtime_checking != x ; then
  AC_DEFINE(ENABLE_RUNTIME_CHECKING, 1,
[Define if you want runtime assertions enabled.  This is a cheap check.])
fi
if test x$ac_tree_checking != x ; then
  AC_DEFINE(ENABLE_TREE_CHECKING, 1,
[Define if you want all operations on trees (the basic data
   structure of the front ends) to be checked for dynamic type safety
   at runtime.  This is moderately expensive.  The tree browser debugging
   routines will also be enabled by this option.
   ])
  TREEBROWSER=tree-browser.o
  TREECHECKING=yes
fi
if test x$ac_types_checking != x ; then
  AC_DEFINE(ENABLE_TYPES_CHECKING, 1,
[Define if you want all gimple types to be verified after gimplifiation.
   This is cheap.
   ])
fi
AC_SUBST(TREEBROWSER)
AC_SUBST(TREECHECKING)
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

if test x$ac_valgrind_checking != x ; then
  # It is certainly possible that there's valgrind but no valgrind.h.
  # GCC relies on making annotations so we must have both.
  AC_MSG_CHECKING(for VALGRIND_DISCARD in <valgrind/memcheck.h>)
  AC_PREPROC_IFELSE([AC_LANG_SOURCE(
    [[#include <valgrind/memcheck.h>
#ifndef VALGRIND_DISCARD
#error VALGRIND_DISCARD not defined
#endif]])],
  [gcc_cv_header_valgrind_memcheck_h=yes],
  [gcc_cv_header_valgrind_memcheck_h=no])
  AC_MSG_RESULT($gcc_cv_header_valgrind_memcheck_h)
  AC_MSG_CHECKING(for VALGRIND_DISCARD in <memcheck.h>)
  AC_PREPROC_IFELSE([AC_LANG_SOURCE(
    [[#include <memcheck.h>
#ifndef VALGRIND_DISCARD
#error VALGRIND_DISCARD not defined
#endif]])],
  [gcc_cv_header_memcheck_h=yes],
  [gcc_cv_header_memcheck_h=no])
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
[AS_HELP_STRING([[--enable-coverage[=LEVEL]]],
		[enable compiler's code coverage collection.
		 Use to measure compiler performance and locate
		 unused parts of the compiler. With LEVEL, specify
		 optimization. Values are opt, noopt,
		 default is noopt])],
[case "${enableval}" in
  yes|noopt)
    coverage_flags="-fprofile-arcs -ftest-coverage -frandom-seed=\$@ -O0"
    ;;
  opt)
    coverage_flags="-fprofile-arcs -ftest-coverage -frandom-seed=\$@ -O2"
    ;;
  no)
    # a.k.a. --disable-coverage
    coverage_flags=""
    ;;
  *)
    AC_MSG_ERROR(unknown coverage setting $enableval)
    ;;
esac],
[coverage_flags=""])
AC_SUBST(coverage_flags)

AC_ARG_ENABLE(gather-detailed-mem-stats, 
[AS_HELP_STRING([--enable-gather-detailed-mem-stats],
		[enable detailed memory allocation stats gathering])], [],
[enable_gather_detailed_mem_stats=no])
gather_stats=`if test $enable_gather_detailed_mem_stats != no; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(GATHER_STATISTICS, $gather_stats,
[Define to enable detailed memory allocation stats gathering.])

# -------------------------------
# Miscenalleous configure options
# -------------------------------

# With stabs
AC_ARG_WITH(stabs,
[AS_HELP_STRING([--with-stabs],
		[arrange to use stabs instead of host debug format])],
stabs="$with_stabs",
stabs=no)

# Determine whether or not multilibs are enabled.
AC_ARG_ENABLE(multilib,
[AS_HELP_STRING([--enable-multilib],
		[enable library support for multiple ABIs])],
[], [enable_multilib=yes])
AC_SUBST(enable_multilib)

# Determine whether or not multiarch is enabled.
AC_ARG_ENABLE(multiarch,
[AS_HELP_STRING([--enable-multiarch],
		[enable support for multiarch paths])],
[case "${enableval}" in
yes|no|auto) enable_multiarch=$enableval;;
*) AC_MSG_ERROR(bad value ${enableval} given for --enable-multiarch option) ;;
esac], [enable_multiarch=auto])
if test x${enable_multiarch} = xauto; then
  if test x${with_native_system_header_dir} != x; then
    ma_msg_suffix=", disabled auto check (configured with --native-system-header-dir)"
    enable_multiarch=no
  fi
  if test x$host != x$target && test "x$with_sysroot" = x; then
    ma_msg_suffix=", disabled auto check (cross build configured without --with-sysroot)"
    enable_multiarch=no
  fi
fi
AC_MSG_CHECKING(for multiarch configuration)
AC_SUBST(enable_multiarch)
AC_MSG_RESULT($enable_multiarch$ma_msg_suffix)

# needed for setting the multiarch name for soft-float/hard-float ABIs
AC_SUBST(with_cpu)
AC_SUBST(with_float)

# Enable __cxa_atexit for C++.
AC_ARG_ENABLE(__cxa_atexit,
[AS_HELP_STRING([--enable-__cxa_atexit], [enable __cxa_atexit for C++])],
[], [])

# Enable C extension for decimal float if target supports it.
GCC_AC_ENABLE_DECIMAL_FLOAT([$target])

dfp=`if test $enable_decimal_float != no; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(ENABLE_DECIMAL_FLOAT, $dfp,
[Define to 1 to enable decimal float extension to C.])

# Use default_decimal_float for dependency.
enable_decimal_float=$default_decimal_float

bid=`if test $enable_decimal_float = bid; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(ENABLE_DECIMAL_BID_FORMAT, $bid,
[Define to 1 to specify that we are using the BID decimal floating
point format instead of DPD])

# Enable C extension for fixed-point arithmetic.
AC_ARG_ENABLE(fixed-point,
[AS_HELP_STRING([--enable-fixed-point],
		[enable fixed-point arithmetic extension to C])],
[],
[
  case $target in
    arm*)
      enable_fixed_point=yes
      ;;

    mips*-*-*)
      enable_fixed_point=yes
      ;;
    *)
      AC_MSG_WARN([fixed-point is not supported for this target, ignored])
      enable_fixed_point=no
      ;;
  esac
])
AC_SUBST(enable_fixed_point)

fixedpoint=`if test $enable_fixed_point = yes; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(ENABLE_FIXED_POINT, $fixedpoint,
[Define to 1 to enable fixed-point arithmetic extension to C.])

# Enable threads
# Pass with no value to take the default
# Pass with a value to specify a thread package
AC_ARG_ENABLE(threads,
[AS_HELP_STRING([[--enable-threads[=LIB]]],
		[enable thread usage for target GCC,
		 using LIB thread package])],,
[enable_threads=''])

AC_ARG_ENABLE(tls,
[AS_HELP_STRING([--enable-tls],
		[enable or disable generation of tls code
                 overriding the assembler check for tls support])],
[
  case $enable_tls in
    yes | no) ;;
    *) AC_MSG_ERROR(['$enable_tls' is an invalid value for --enable-tls.
Valid choices are 'yes' and 'no'.]) ;;
  esac
], [enable_tls=''])

AC_ARG_ENABLE(objc-gc,
[AS_HELP_STRING([--enable-objc-gc],
		[enable the use of Boehm's garbage collector with
		 the GNU Objective-C runtime])],
if test x$enable_objc_gc = xno; then
	objc_boehm_gc=''
else
	objc_boehm_gc=1
fi,
objc_boehm_gc='')

AC_ARG_WITH(dwarf2,
[AS_HELP_STRING([--with-dwarf2], [force the default debug format to be DWARF 2])],
dwarf2="$with_dwarf2",
dwarf2=no)

AC_ARG_ENABLE(shared,
[AS_HELP_STRING([--disable-shared], [don't provide a shared libgcc])],
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

AC_ARG_WITH([native-system-header-dir],
  [  --with-native-system-header-dir=dir
                          use dir as the directory to look for standard
                          system header files in.  Defaults to /usr/include.],
[
 case ${with_native_system_header_dir} in
 yes|no) AC_MSG_ERROR([bad value ${withval} given for --with-native-system-header-dir]) ;;
 /* | [[A-Za-z]]:[[\\/]]*) ;;
 *) AC_MSG_ERROR([--with-native-system-header-dir argument ${withval} must be an absolute directory]) ;;
 esac
 configured_native_system_header_dir="${withval}"
], [configured_native_system_header_dir=])

AC_ARG_WITH(build-sysroot, 
  [AS_HELP_STRING([--with-build-sysroot=sysroot],
                  [use sysroot as the system root during the build])],
  [if test x"$withval" != x ; then
     SYSROOT_CFLAGS_FOR_TARGET="--sysroot=$withval"
   fi],
  [SYSROOT_CFLAGS_FOR_TARGET=])
AC_SUBST(SYSROOT_CFLAGS_FOR_TARGET)

if test "x$prefix" = xNONE; then
 test_prefix=/usr/local
else
 test_prefix=$prefix
fi
if test "x$exec_prefix" = xNONE; then
 test_exec_prefix=$test_prefix
else
 test_exec_prefix=$exec_prefix
fi

AC_ARG_WITH(sysroot,
[AS_HELP_STRING([[--with-sysroot[=DIR]]],
		[search for usr/lib, usr/include, et al, within DIR])],
[
 case ${with_sysroot} in
 yes) TARGET_SYSTEM_ROOT='${exec_prefix}/${target_noncanonical}/sys-root' ;;
 *) TARGET_SYSTEM_ROOT=$with_sysroot ;;
 esac
   
 TARGET_SYSTEM_ROOT_DEFINE='-DTARGET_SYSTEM_ROOT=\"$(TARGET_SYSTEM_ROOT)\"'
 CROSS_SYSTEM_HEADER_DIR='$(TARGET_SYSTEM_ROOT)$${sysroot_headers_suffix}$(NATIVE_SYSTEM_HEADER_DIR)'
	
 case ${TARGET_SYSTEM_ROOT} in
 "${test_prefix}"|"${test_prefix}/"*|\
 "${test_exec_prefix}"|"${test_exec_prefix}/"*|\
 '${prefix}'|'${prefix}/'*|\
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

AC_ARG_WITH(specs,
  [AS_HELP_STRING([--with-specs=SPECS],
                  [add SPECS to driver command-line processing])],
  [CONFIGURE_SPECS=$withval],
  [CONFIGURE_SPECS=]
)
AC_SUBST(CONFIGURE_SPECS)

ACX_PKGVERSION([GCC])
ACX_BUGURL([http://gcc.gnu.org/bugs.html])

# Sanity check enable_languages in case someone does not run the toplevel
# configure # script.
AC_ARG_ENABLE(languages,
[AS_HELP_STRING([--enable-languages=LIST], [specify which front-ends to build])],
[case ,${enable_languages}, in
       ,,|,yes,)
		# go safe -- we cannot be much sure without the toplevel
		# configure's
		# analysis of which target libs are present and usable
		enable_languages=c
		;;
	 *,all,*)
		AC_MSG_ERROR([only the toplevel supports --enable-languages=all])
		;;
	*,c,*)
		;;
        *)
		enable_languages=c,${enable_languages}
		;;
esac],
[enable_languages=c])

AC_ARG_WITH(multilib-list,
[AS_HELP_STRING([--with-multilib-list], [select multilibs (AArch64, SH and x86-64 only)])],
:,
with_multilib_list=default)

# -------------------------
# Checks for other programs
# -------------------------

AC_PROG_MAKE_SET

# Find some useful tools
AC_PROG_AWK
# We need awk to create options.c and options.h.
# Bail out if it's missing.
case ${AWK} in
  "") AC_MSG_ERROR([can't build without awk, bailing out]) ;;
esac

gcc_AC_PROG_LN_S
ACX_PROG_LN($LN_S)
AC_PROG_RANLIB
ranlib_flags=""
AC_SUBST(ranlib_flags)
     
gcc_AC_PROG_INSTALL

# See if cmp has --ignore-initial.
gcc_AC_PROG_CMP_IGNORE_INITIAL

# See if we have the mktemp command.
AC_CHECK_PROG(have_mktemp_command, mktemp, yes, no)

# See if makeinfo has been installed and is modern enough
# that we can use it.
ACX_CHECK_PROG_VER(MAKEINFO, makeinfo, --version,
  [GNU texinfo.* \([0-9][0-9.]*\)],
  [4.[7-9]*|4.[1-9][0-9]*|[5-9]*|[1-9][0-9]*])
if test $gcc_cv_prog_makeinfo_modern = no; then
  AC_MSG_WARN([
*** Makeinfo is missing or too old.
*** Info documentation will not be built.])
  BUILD_INFO=
else
  BUILD_INFO=info
fi
AC_SUBST(BUILD_INFO)

# Is pod2man recent enough to regenerate manpages?
AC_MSG_CHECKING([for recent Pod::Man])
if (perl -e 'use 1.10 Pod::Man') >/dev/null 2>&1; then
  AC_MSG_RESULT(yes)
  GENERATED_MANPAGES=generated-manpages
else
  AC_MSG_RESULT(no)
  GENERATED_MANPAGES=
fi
AC_SUBST(GENERATED_MANPAGES)

MISSING="${CONFIG_SHELL-/bin/sh} $ac_aux_dir/missing"

# How about lex?
dnl Don't use AC_PROG_LEX; we insist on flex.
dnl LEXLIB is not useful in gcc.
AC_CHECK_PROGS([FLEX], flex, [$MISSING flex])

# Bison?
AC_CHECK_PROGS([BISON], bison, [$MISSING bison])

# Binutils are not build modules, unlike bison/flex/makeinfo.  So we
# check for build == host before using them.

# NM
if test x${build} = x${host} && test -f $srcdir/../binutils/nm.c \
  && test -d ../binutils ; then
  NM='${objdir}/../binutils/nm-new'
else
  AC_CHECK_PROG(NM, nm, nm, ${CONFIG_SHELL-/bin/sh} ${srcdir}/../missing nm)
fi

# AR
if test x${build} = x${host} && test -f $srcdir/../binutils/ar.c \
  && test -d ../binutils ; then
  AR='${objdir}/../binutils/ar'
else
  AC_CHECK_PROG(AR, ar, ar, ${CONFIG_SHELL-/bin/sh} ${srcdir}/../missing ar)
fi


# --------------------
# Checks for C headers
# --------------------

# Need to reject headers which give warnings, so that the -Werror bootstrap
# works later. *sigh*  This needs to come before all header checks.
AC_PROG_CPP_WERROR

AC_HEADER_STDC
AC_HEADER_TIME
ACX_HEADER_STRING
AC_HEADER_SYS_WAIT
AC_HEADER_TIOCGWINSZ
AC_CHECK_HEADERS(limits.h stddef.h string.h strings.h stdlib.h time.h iconv.h \
		 fcntl.h unistd.h sys/file.h sys/time.h sys/mman.h \
		 sys/resource.h sys/param.h sys/times.h sys/stat.h \
		 direct.h malloc.h langinfo.h ldfcn.h locale.h wchar.h)

# Check for thread headers.
AC_CHECK_HEADER(thread.h, [have_thread_h=yes], [have_thread_h=])
AC_CHECK_HEADER(pthread.h, [have_pthread_h=yes], [have_pthread_h=])

# These tests can't be done till we know if we have limits.h.
gcc_AC_C_CHAR_BIT
AC_C_BIGENDIAN

# ----------------------
# Checks for C++ headers
# ----------------------

dnl Autoconf will give an error in the configure script if there is no
dnl C++ preprocessor.  Hack to prevent that.
m4_pushdef([AC_MSG_ERROR], m4_defn([AC_MSG_WARN]))[]dnl
AC_PROG_CXXCPP
m4_popdef([AC_MSG_ERROR])[]dnl

AC_LANG_PUSH(C++)

AC_CHECK_HEADERS(unordered_map)
AC_CHECK_HEADERS(tr1/unordered_map)
AC_CHECK_HEADERS(ext/hash_map)

AC_LANG_POP(C++)

# --------
# Dependency checking.
# --------

AC_LANG_PUSH(C++)
ZW_CREATE_DEPDIR
AC_CONFIG_COMMANDS([gccdepdir],[
  ${CONFIG_SHELL-/bin/sh} $ac_aux_dir/mkinstalldirs build/$DEPDIR
  for lang in $subdirs c-family common
  do
      ${CONFIG_SHELL-/bin/sh} $ac_aux_dir/mkinstalldirs $lang/$DEPDIR
  done], [subdirs="$subdirs" ac_aux_dir=$ac_aux_dir DEPDIR=$DEPDIR])

ZW_PROG_COMPILER_DEPENDENCIES([CXX])
AC_LANG_POP(C++)

# --------
# UNSORTED
# --------


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

# To support -mcpu=native on Solaris/SPARC, we need libkstat.
save_LIBS="$LIBS"
LIBS=
AC_SEARCH_LIBS(kstat_open, kstat)
EXTRA_GCC_LIBS="$LIBS"
LIBS="$save_LIBS"
AC_SUBST(EXTRA_GCC_LIBS)

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
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[#include <sys/types.h>
#include <inttypes.h>]],
  [[intmax_t i = -1;]])],
  [gcc_cv_header_inttypes_h=yes],
  [gcc_cv_header_inttypes_h=no])])
AC_MSG_RESULT($gcc_cv_header_inttypes_h)
if test $gcc_cv_header_inttypes_h = yes; then
  AC_DEFINE(HAVE_INTTYPES_H, 1,
	[Define if you have a working <inttypes.h> header file.])
fi

dnl Disabled until we have a complete test for buggy enum bitfields.
dnl gcc_AC_C_ENUM_BF_UNSIGNED

define(gcc_UNLOCKED_FUNCS, clearerr_unlocked feof_unlocked dnl
  ferror_unlocked fflush_unlocked fgetc_unlocked fgets_unlocked dnl
  fileno_unlocked fprintf_unlocked fputc_unlocked fputs_unlocked dnl
  fread_unlocked fwrite_unlocked getchar_unlocked getc_unlocked dnl
  putchar_unlocked putc_unlocked)
AC_CHECK_FUNCS(times clock kill getrlimit setrlimit atoll atoq \
	sysconf strsignal getrusage nl_langinfo \
	gettimeofday mbstowcs wcswidth mmap setlocale \
	gcc_UNLOCKED_FUNCS madvise)

if test x$ac_cv_func_mbstowcs = xyes; then
  AC_CACHE_CHECK(whether mbstowcs works, gcc_cv_func_mbstowcs_works,
[    AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdlib.h>
int main()
{
  mbstowcs(0, "", 0);
  return 0;
}]])],
    [gcc_cv_func_mbstowcs_works=yes],
    [gcc_cv_func_mbstowcs_works=no],
    [gcc_cv_func_mbstowcs_works=yes])])
  if test x$gcc_cv_func_mbstowcs_works = xyes; then
    AC_DEFINE(HAVE_WORKING_MBSTOWCS, 1,
  [Define this macro if mbstowcs does not crash when its
   first argument is NULL.])
  fi
fi

AC_CHECK_TYPE(ssize_t, int)
AC_CHECK_TYPE(caddr_t, char *)

GCC_AC_FUNC_MMAP_BLACKLIST

case "${host}" in
*-*-*vms*)
  # Under VMS, vfork works very differently than on Unix. The standard test 
  # won't work, and it isn't easily adaptable. It makes more sense to
  # just force it.
  ac_cv_func_vfork_works=yes
  ;;
esac
AC_FUNC_FORK

# g++ on Solaris 10+ defines _XOPEN_SOURCE=600, which exposes a different
# iconv() prototype.
AC_LANG_PUSH([C++])
AM_ICONV
AC_LANG_POP([C++])

# Until we have in-tree GNU iconv:
LIBICONV_DEP=
AC_SUBST(LIBICONV_DEP)

AM_LC_MESSAGES

AM_LANGINFO_CODESET

# We will need to find libiberty.h and ansidecl.h
saved_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -I${srcdir} -I${srcdir}/../include $GMPINC"
saved_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -I${srcdir} -I${srcdir}/../include $GMPINC"
gcc_AC_CHECK_DECLS(getenv atol asprintf sbrk abort atof getcwd getwd \
	strsignal strstr stpcpy strverscmp \
	errno snprintf vsnprintf vasprintf malloc realloc calloc \
	free basename getopt clock getpagesize ffs gcc_UNLOCKED_FUNCS, , ,[
#include "ansidecl.h"
#include "system.h"])

gcc_AC_CHECK_DECLS(getrlimit setrlimit getrusage, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
]], [[rlim_t l = 0;]])],[],[AC_DEFINE([rlim_t],[long],
[Define to `long' if <sys/resource.h> doesn't define.])])

# On AIX 5.2, <ldfcn.h> conflicts with <fcntl.h>, as both define incompatible
# FREAD and FWRITE macros.  Fortunately, for GCC's single usage of ldgetname
# in collect2.c, <fcntl.h> isn't visible, but the configure test below needs
# to undef these macros to get the correct value for HAVE_DECL_LDGETNAME.
gcc_AC_CHECK_DECLS(ldgetname, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_LDFCN_H
#undef FREAD
#undef FWRITE
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

gcc_AC_CHECK_DECLS(sigaltstack, , ,[
#include "ansidecl.h"
#include "system.h"
#include <signal.h>
])

# g++ on Solaris 10+ defines _XOPEN_SOURCE=600, which hides the madvise()
# prototype.
AC_LANG_PUSH([C++])
gcc_AC_CHECK_DECLS(madvise, , ,[
  #include "ansidecl.h"
  #include "system.h"
])
AC_LANG_POP([C++])

# More time-related stuff.
AC_CACHE_CHECK(for struct tms, ac_cv_struct_tms, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
]], [[struct tms tms;]])],[ac_cv_struct_tms=yes],[ac_cv_struct_tms=no])])
if test $ac_cv_struct_tms = yes; then
  AC_DEFINE(HAVE_STRUCT_TMS, 1,
  [Define if <sys/times.h> defines struct tms.])
fi

# use gcc_cv_* here because this doesn't match the behavior of AC_CHECK_TYPE.
# revisit after autoconf 2.50.
AC_CACHE_CHECK(for clock_t, gcc_cv_type_clock_t, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include "ansidecl.h"
#include "system.h"
]], [[clock_t x;]])],[gcc_cv_type_clock_t=yes],[gcc_cv_type_clock_t=no])])
if test $gcc_cv_type_clock_t = yes; then
  AC_DEFINE(HAVE_CLOCK_T, 1,
  [Define if <time.h> defines clock_t.])
fi

# Check if F_SETLKW is supported by fcntl.
AC_CACHE_CHECK(for F_SETLKW, ac_cv_f_setlkw, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <fcntl.h>]], [[
struct flock fl;
fl.l_whence = 0;
fl.l_start = 0;
fl.l_len = 0;
fl.l_pid = 0;
return fcntl (1, F_SETLKW, &fl);]])],
[ac_cv_f_setlkw=yes],[ac_cv_f_setlkw=no])])
if test $ac_cv_f_setlkw = yes; then
  AC_DEFINE(HOST_HAS_F_SETLKW, 1,
  [Define if F_SETLKW supported by fcntl.])
fi

# Restore CFLAGS, CXXFLAGS from before the gcc_AC_NEED_DECLARATIONS tests.
CFLAGS="$saved_CFLAGS"
CXXFLAGS="$saved_CXXFLAGS"

# mkdir takes a single argument on some systems. 
gcc_AC_FUNC_MKDIR_TAKES_ONE_ARG

# File extensions
manext='.1'
objext='.o'
AC_SUBST(manext)
AC_SUBST(objext)

# With Setjmp/Longjmp based exception handling.
AC_ARG_ENABLE(sjlj-exceptions,
[AS_HELP_STRING([--enable-sjlj-exceptions],
                [arrange to use setjmp/longjmp exception handling])],
[case $target in
  *-*-hpux10*)
    if test $enableval != yes; then
      AC_MSG_WARN([dwarf2 exceptions not supported, sjlj exceptions forced])
      enableval=yes
    fi
    ;;
esac
force_sjlj_exceptions=yes],
[case $target in
  *-*-hpux10*)
    force_sjlj_exceptions=yes
    enableval=yes
    ;;
  lm32*-*-*)
     force_sjlj_exceptions=yes
     enableval=yes
     ;;
  *)
    force_sjlj_exceptions=no
    ;;
esac])
if test $force_sjlj_exceptions = yes; then
  sjlj=`if test $enableval = yes; then echo 1; else echo 0; fi`
  AC_DEFINE_UNQUOTED(CONFIG_SJLJ_EXCEPTIONS, $sjlj,
    [Define 0/1 to force the choice for exception handling model.])
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

# Support --enable-initfini-array.
if test x$enable_initfini_array != xno; then
  tm_file="${tm_file} initfini-array.h"
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

if test x$use_long_long_for_widest_fast_int = xyes; then
	AC_DEFINE(USE_LONG_LONG_FOR_WIDEST_FAST_INT, 1,
[Define to 1 if the 'long long' (or '__int64') is wider than 'long' but still
efficiently supported by the host hardware.])
fi

gnu_ld_bool=`if test x"$gnu_ld" = x"yes"; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(HAVE_GNU_LD, $gnu_ld_bool, [Define to 1 if using GNU ld.])

gnu_as_bool=`if test x"$gas" = x"yes"; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(HAVE_GNU_AS, $gnu_as_bool, [Define to 1 if using GNU as.])

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

if test -n "$configured_native_system_header_dir"; then
  native_system_header_dir=$configured_native_system_header_dir
fi
NATIVE_SYSTEM_HEADER_DIR="$native_system_header_dir"
AC_SUBST(NATIVE_SYSTEM_HEADER_DIR)

case ${host} in
  powerpc*-*-darwin*)
    AC_CACHE_CHECK([whether mcontext_t fields have underscores],
      gcc_cv_mcontext_underscores,
      AC_COMPILE_IFELSE([
#include <sys/cdefs.h>
#include <sys/signal.h>
#include <ucontext.h>
int main() { mcontext_t m; if (m->ss.srr0) return 0; return 0; }
],
	gcc_cv_mcontext_underscores=no, gcc_cv_mcontext_underscores=yes))
      if test $gcc_cv_mcontext_underscores = yes; then
        AC_DEFINE(HAS_MCONTEXT_T_UNDERSCORES,,dnl
          [mcontext_t fields start with __])
      fi
    ;;
esac

# ---------
# Threading
# ---------

# Check if a valid thread package
case ${enable_threads} in
  "" | no)
    # No threads
    target_thread_file='single'
    ;;
  yes)
    # default
    target_thread_file='single'
    ;;
  aix | dce | lynx | mipssde | posix | rtems | \
  single | tpf | vxworks | win32)
    target_thread_file=${enable_threads}
    ;;
  *)
    echo "${enable_threads} is an unknown thread package" 1>&2
    exit 1
    ;;
esac

if test x${thread_file} = x; then
  # No thread file set by target-specific clauses in config.gcc,
  # so use file chosen by default logic above
  thread_file=${target_thread_file}
fi

# --------
# UNSORTED
# --------

use_cxa_atexit=no
if test x$enable___cxa_atexit = xyes || \
   test x$enable___cxa_atexit = x -a x$default_use_cxa_atexit = xyes; then
  if test x$host = x$target; then
    case $host in
      # mingw32 doesn't have __cxa_atexit but uses atexit registration
      # keyed to flag_use_cxa_atexit
      *-*-mingw32*)
	use_cxa_atexit=yes
	;;
      powerpc-ibm-aix*)
	use_cxa_atexit=yes
	;;
      *)
	AC_CHECK_FUNC(__cxa_atexit,[use_cxa_atexit=yes],
	  [echo "__cxa_atexit can't be enabled on this target"])
	;;
    esac
  else
    # We can't check for __cxa_atexit when building a cross, so assume
    # it is available 
    use_cxa_atexit=yes
  fi
  if test x$use_cxa_atexit = xyes; then
    AC_DEFINE(DEFAULT_USE_CXA_ATEXIT, 2,
      [Define if you want to use __cxa_atexit, rather than atexit, to
      register C++ destructors for local statics and global objects.
      This is essential for fully standards-compliant handling of
      destructors, but requires __cxa_atexit in libc.])
  fi
fi

# Look for a file containing extra machine modes.
if test -n "$extra_modes" && test -f $srcdir/config/$extra_modes; then
  extra_modes_file='$(srcdir)'/config/${extra_modes}
  AC_SUBST(extra_modes_file)
  AC_DEFINE_UNQUOTED(EXTRA_MODES_FILE, "config/$extra_modes",
  [Define to the name of a file containing a list of extra machine modes
   for this architecture.])
fi

# Convert extra_options into a form suitable for Makefile use.
extra_opt_files=
all_opt_files=
for f in $extra_options; do
  extra_opt_files="$extra_opt_files \$(srcdir)/config/$f"
  all_opt_files="$all_opt_files $srcdir/config/$f"
done
AC_SUBST(extra_opt_files)

# auto-host.h is the file containing items generated by autoconf and is
# the first file included by config.h.
# If host=build, it is correct to have bconfig include auto-host.h
# as well.  If host!=build, we are in error and need to do more 
# work to find out the build config parameters.
if test x$host = x$build
then
	build_auto=auto-host.h
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
	# Clearing GMPINC is necessary to prevent host headers being
	# used by the build compiler.  Defining GENERATOR_FILE stops
	# system.h from including gmp.h.
	CC="${CC_FOR_BUILD}" CFLAGS="${CFLAGS_FOR_BUILD}" \
	CXX="${CXX_FOR_BUILD}" CXXFLAGS="${CXXFLAGS_FOR_BUILD}" \
	LD="${LD_FOR_BUILD}" LDFLAGS="${LDFLAGS_FOR_BUILD}" \
	GMPINC="" CPPFLAGS="${CPPFLAGS} -DGENERATOR_FILE" \
	${realsrcdir}/configure \
		--enable-languages=${enable_languages-all} \
		--target=$target_alias --host=$build_alias --build=$build_alias

	# We just finished tests for the build machine, so rename
	# the file auto-build.h in the gcc directory.
	mv auto-host.h ../auto-build.h
	cd ..
	rm -rf $tempdir
	build_auto=auto-build.h
fi
AC_SUBST(build_subdir)

tm_file="${tm_file} defaults.h"
tm_p_file="${tm_p_file} tm-preds.h"
host_xm_file="auto-host.h ansidecl.h ${host_xm_file}"
build_xm_file="${build_auto} ansidecl.h ${build_xm_file}"
# We don't want ansidecl.h in target files, write code there in ISO/GNU C.
# put this back in temporarily.
xm_file="auto-host.h ansidecl.h ${xm_file}"

# --------
# UNSORTED
# --------

changequote(,)dnl
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

changequote(,)dnl
gcc_BASEVER=`cat $srcdir/BASE-VER`
gcc_DEVPHASE=`cat $srcdir/DEV-PHASE`
gcc_DATESTAMP=`cat $srcdir/DATESTAMP`
if test -f $srcdir/REVISION ; then
	gcc_REVISION=`cat $srcdir/REVISION`
else
        gcc_REVISION=""
fi
cat > plugin-version.h <<EOF
#include "configargs.h"

#define GCCPLUGIN_VERSION_MAJOR   `echo $gcc_BASEVER | sed -e 's/^\([0-9]*\).*$/\1/'`
#define GCCPLUGIN_VERSION_MINOR   `echo $gcc_BASEVER | sed -e 's/^[0-9]*\.\([0-9]*\).*$/\1/'`
#define GCCPLUGIN_VERSION_PATCHLEVEL   `echo $gcc_BASEVER | sed -e 's/^[0-9]*\.[0-9]*\.\([0-9]*\)$/\1/'`
#define GCCPLUGIN_VERSION  (GCCPLUGIN_VERSION_MAJOR*1000 + GCCPLUGIN_VERSION_MINOR)

static char basever[] = "$gcc_BASEVER";
static char datestamp[] = "$gcc_DATESTAMP";
static char devphase[] = "$gcc_DEVPHASE";
static char revision[] = "$gcc_REVISION";

/* FIXME plugins: We should make the version information more precise.
   One way to do is to add a checksum. */

static struct plugin_gcc_version gcc_version = {basever, datestamp,
						devphase, revision,
						configuration_arguments};
EOF
changequote([,])dnl

# Internationalization
ZW_GNU_GETTEXT_SISTER_DIR

# If LIBINTL contains LIBICONV, then clear LIBICONV so we don't get
# -liconv on the link line twice.
case "$LIBINTL" in *$LIBICONV*)
	LIBICONV= ;;
esac

AC_ARG_ENABLE(secureplt,
[AS_HELP_STRING([--enable-secureplt],
		[enable -msecure-plt by default for PowerPC])],
[], [])

AC_ARG_ENABLE(leading-mingw64-underscores,
  AS_HELP_STRING([--enable-leading-mingw64-underscores],
                 [enable leading underscores on 64 bit mingw targets]),
  [],[])
AS_IF([ test x"$enable_leading_mingw64_underscores" = xyes ],
  [AC_DEFINE(USE_MINGW64_LEADING_UNDERSCORES, 1,
    [Define if we should use leading underscore on 64 bit mingw targets])])

AC_ARG_ENABLE(cld,
[AS_HELP_STRING([--enable-cld], [enable -mcld by default for 32bit x86])], [],
[enable_cld=no])

AC_ARG_ENABLE(frame-pointer,
[AS_HELP_STRING([--enable-frame-pointer],
		[enable -fno-omit-frame-pointer by default for 32bit x86])], [],
[
case $target_os in
linux* | darwin[[8912]]*)
  # Enable -fomit-frame-pointer by default for Linux and Darwin with
  # DWARF2.
  enable_frame_pointer=no
  ;;
*)
  enable_frame_pointer=yes
  ;;
esac
])

# Windows32 Registry support for specifying GCC installation paths.
AC_ARG_ENABLE(win32-registry,
[AS_HELP_STRING([--disable-win32-registry],
                [disable lookup of installation paths in the
                 Registry on Windows hosts])
AS_HELP_STRING([--enable-win32-registry], [enable registry lookup (default)])
AS_HELP_STRING([--enable-win32-registry=KEY],
               [use KEY instead of GCC version as the last portion
                of the registry key])],,)

case $host_os in
  win32 | pe | cygwin* | mingw32* | uwin*)
    if test "x$enable_win32_registry" != xno; then
      AC_SEARCH_LIBS(RegOpenKeyExA, advapi32,, [enable_win32_registry=no])
    fi

    if test "x$enable_win32_registry" != xno; then
      AC_DEFINE(ENABLE_WIN32_REGISTRY, 1,
  [Define to 1 if installation paths should be looked up in the Windows
   Registry. Ignored on non-Windows hosts.])

      if test "x$enable_win32_registry" != xyes \
         && test "x$enable_win32_registry" != x; then
	AC_DEFINE_UNQUOTED(WIN32_REGISTRY_KEY, "$enable_win32_registry",
  [Define to be the last component of the Windows registry key under which
   to look for installation paths.  The full key used will be 
   HKEY_LOCAL_MACHINE/SOFTWARE/Free Software Foundation/{WIN32_REGISTRY_KEY}.
   The default is the GCC version number.])
      fi
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

out_object_file=`basename $out_file .c`.o
common_out_object_file=`basename $common_out_file .c`.o

tm_file_list="options.h"
tm_include_list="options.h insn-constants.h"
for f in $tm_file; do
  case $f in
    ./* )
       f=`echo $f | sed 's/^..//'`
       tm_file_list="${tm_file_list} $f"
       tm_include_list="${tm_include_list} $f"
       ;;
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

# Define macro CROSS_DIRECTORY_STRUCTURE in compilation if this is a
# cross-compiler which does not use the native headers and libraries.
# Also use all.cross instead of all.internal and adjust SYSTEM_HEADER_DIR.
CROSS=						AC_SUBST(CROSS)
ALL=all.internal				AC_SUBST(ALL)
SYSTEM_HEADER_DIR='$(NATIVE_SYSTEM_HEADER_DIR)'	AC_SUBST(SYSTEM_HEADER_DIR)

if test "x$with_build_sysroot" != x; then
  build_system_header_dir=$with_build_sysroot'$${sysroot_headers_suffix}$(NATIVE_SYSTEM_HEADER_DIR)'
else
  # This value is used, even on a native system, because 
  # CROSS_SYSTEM_HEADER_DIR is just 
  # $(TARGET_SYSTEM_ROOT)$(NATIVE_SYSTEM_HEADER_DIR).
  build_system_header_dir='$(CROSS_SYSTEM_HEADER_DIR)'
fi

if test x$host != x$target
then
	CROSS="-DCROSS_DIRECTORY_STRUCTURE"
	ALL=all.cross
	SYSTEM_HEADER_DIR=$build_system_header_dir
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

	case $target in
		*-*-mingw*)
			if test "x$with_headers" = x; then
				with_headers=yes
			fi
			;;
		*)
			;;
	esac
elif test "x$TARGET_SYSTEM_ROOT" != x; then
        SYSTEM_HEADER_DIR=$build_system_header_dir 
fi

# If this is a cross-compiler that does not
# have its own set of headers then define
# inhibit_libc

# If this is using newlib, without having the headers available now,
# then define inhibit_libc in LIBGCC2_CFLAGS.
# This prevents libgcc2 from containing any code which requires libc
# support.
: ${inhibit_libc=false}
if { { test x$host != x$target && test "x$with_sysroot" = x ; } ||
       test x$with_newlib = xyes ; } &&
     { test "x$with_headers" = x || test "x$with_headers" = xno ; } ; then
       inhibit_libc=true
fi
AC_SUBST(inhibit_libc)

# When building gcc with a cross-compiler, we need to adjust things so
# that the generator programs are still built with the native compiler.
# Also, we cannot run fixincludes.

# These are the normal (build=host) settings:
CC_FOR_BUILD='$(CC)'		AC_SUBST(CC_FOR_BUILD)
CXX_FOR_BUILD='$(CXX)'		AC_SUBST(CXX_FOR_BUILD)
BUILD_CFLAGS='$(ALL_CFLAGS)'	AC_SUBST(BUILD_CFLAGS)
BUILD_CXXFLAGS='$(ALL_CXXFLAGS)' AC_SUBST(BUILD_CXXFLAGS)
BUILD_LDFLAGS='$(LDFLAGS)'	AC_SUBST(BUILD_LDFLAGS)
STMP_FIXINC=stmp-fixinc		AC_SUBST(STMP_FIXINC)

# And these apply if build != host, or we are generating coverage data
if test x$build != x$host || test "x$coverage_flags" != x
then
    BUILD_CFLAGS='$(INTERNAL_CFLAGS) $(T_CFLAGS) $(CFLAGS_FOR_BUILD)'
    BUILD_CXXFLAGS='$(INTERNAL_CFLAGS) $(T_CFLAGS) $(CXXFLAGS_FOR_BUILD)'
    BUILD_LDFLAGS='$(LDFLAGS_FOR_BUILD)'
fi

# Expand extra_headers to include complete path.
# This substitutes for lots of t-* files.
extra_headers_list=
# Prepend $(srcdir)/config/${cpu_type}/ to every entry in extra_headers.
for file in ${extra_headers} ; do
  extra_headers_list="${extra_headers_list} \$(srcdir)/config/${cpu_type}/${file}"
done

# If use_gcc_tgmath is set, append ginclude/tgmath.h.
if test x"$use_gcc_tgmath" = xyes
then extra_headers_list="${extra_headers_list} \$(srcdir)/ginclude/tgmath.h"
fi

# Define collect2 in Makefile.
case $host_can_use_collect2 in
  no) collect2= ;;
  *) collect2='collect2$(exeext)' ;;
esac
AC_SUBST([collect2])

# Add a definition of USE_COLLECT2 if system wants one.
case $use_collect2 in
  no) use_collect2= ;;
  "") ;;
  *) 
    host_xm_defines="${host_xm_defines} USE_COLLECT2"
    xm_defines="${xm_defines} USE_COLLECT2"
    case $host_can_use_collect2 in
      no)
        AC_MSG_ERROR([collect2 is required but cannot be built on this system])
        ;;
    esac
    ;;
esac

AC_DEFINE_UNQUOTED(LTOPLUGINSONAME,"${host_lto_plugin_soname}",
[Define to the name of the LTO plugin DSO that must be
  passed to the linker's -plugin=LIB option.])

# ---------------------------
# Assembler & linker features
# ---------------------------

# During stage 2, ld is actually gcc/collect-ld, which is a small script to
# discern between when to use prev-ld/ld-new and when to use ld/ld-new.
# However when ld-new is first executed from the build tree, libtool will
# relink it as .libs/lt-ld-new, so that it can give it an RPATH that refers
# to the build tree.  While doing this we need to use the previous-stage
# linker, or we have an infinite loop.  The presence of a shell script as
# ld/ld-new, and the fact that the script *uses ld itself*, is what confuses
# the gcc/collect-ld script.  So we need to know how libtool works, or
# exec-tool will fail.

m4_defun([_LT_CONFIG_COMMANDS], [])
AC_PROG_LIBTOOL
AC_SUBST(objdir)
AC_SUBST(enable_fast_install)

# Identify the assembler which will work hand-in-glove with the newly
# built GCC, so that we can examine its features.  This is the assembler
# which will be driven by the driver program.
#
# If build != host, and we aren't building gas in-tree, we identify a
# build->target assembler and hope that it will have the same features
# as the host->target assembler we'll be using.
gcc_cv_gas_major_version=
gcc_cv_gas_minor_version=
gcc_cv_as_gas_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/gas

m4_pattern_allow([AS_FOR_TARGET])dnl
AS_VAR_SET_IF(gcc_cv_as,, [
if test -x "$DEFAULT_ASSEMBLER"; then
	gcc_cv_as="$DEFAULT_ASSEMBLER"
elif test -f $gcc_cv_as_gas_srcdir/configure.in \
     && test -f ../gas/Makefile \
     && test x$build = x$host; then
	gcc_cv_as=../gas/as-new$build_exeext
elif test -x as$build_exeext; then
	# Build using assembler in the current directory.
	gcc_cv_as=./as$build_exeext
elif ( set dummy $AS_FOR_TARGET; test -x $[2] ); then
        gcc_cv_as="$AS_FOR_TARGET"
else
        AC_PATH_PROG(gcc_cv_as, $AS_FOR_TARGET)
fi])

ORIGINAL_AS_FOR_TARGET=$gcc_cv_as
AC_SUBST(ORIGINAL_AS_FOR_TARGET)
case "$ORIGINAL_AS_FOR_TARGET" in
  ./as | ./as$build_exeext) ;;
  *) AC_CONFIG_FILES(as:exec-tool.in, [chmod +x as]) ;;
esac 

AC_MSG_CHECKING(what assembler to use)
if test "$gcc_cv_as" = ../gas/as-new$build_exeext; then
  # Single tree build which includes gas.  We want to prefer it
  # over whatever linker top-level may have detected, since
  # we'll use what we're building after installation anyway.
  AC_MSG_RESULT(newly built gas)
  in_tree_gas=yes
  _gcc_COMPUTE_GAS_VERSION
  in_tree_gas_is_elf=no
  if grep 'obj_format = elf' ../gas/Makefile > /dev/null \
     || (grep 'obj_format = multi' ../gas/Makefile \
         && grep 'extra_objects =.* obj-elf' ../gas/Makefile) > /dev/null
  then
    in_tree_gas_is_elf=yes
  fi
else
  AC_MSG_RESULT($gcc_cv_as)
  in_tree_gas=no
fi

default_ld=
AC_ARG_ENABLE(ld,
[[  --enable-ld[=ARG]       build ld [ARG={default,yes,no}]]],
[case "${enableval}" in
 no)
   default_ld=ld.gold
   ;;
 esac])

AC_ARG_ENABLE(gold,
[[  --enable-gold[=ARG]     build gold [ARG={default,yes,no}]]],
[case "${enableval}" in
 default)
   install_gold_as_default=yes
   ;;
 yes)
   if test x${default_ld} != x; then
     install_gold_as_default=yes
   fi
   ;;
 no)
   ;;
 *)
   AC_MSG_ERROR([invalid --enable-gold argument])
   ;;
 esac],
[install_gold_as_default=no])

# Identify the linker which will work hand-in-glove with the newly
# built GCC, so that we can examine its features.  This is the linker
# which will be driven by the driver program.
#
# If build != host, and we aren't building gas in-tree, we identify a
# build->target linker and hope that it will have the same features
# as the host->target linker we'll be using.
gcc_cv_gld_major_version=
gcc_cv_gld_minor_version=
gcc_cv_ld_gld_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/ld
gcc_cv_ld_gold_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/gold
gcc_cv_ld_bfd_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/bfd

AS_VAR_SET_IF(gcc_cv_ld,, [
if test -x "$DEFAULT_LINKER"; then
	gcc_cv_ld="$DEFAULT_LINKER"
elif test $install_gold_as_default = yes \
     && test -f $gcc_cv_ld_gold_srcdir/configure.ac \
     && test -f ../gold/Makefile \
     && test x$build = x$host; then
	gcc_cv_ld=../gold/ld-new$build_exeext
elif test -f $gcc_cv_ld_gld_srcdir/configure.in \
     && test -f ../ld/Makefile \
     && test x$build = x$host; then
	gcc_cv_ld=../ld/ld-new$build_exeext
elif test -x collect-ld$build_exeext; then
	# Build using linker in the current directory.
	gcc_cv_ld=./collect-ld$build_exeext
elif ( set dummy $LD_FOR_TARGET; test -x $[2] ); then
        gcc_cv_ld="$LD_FOR_TARGET"
else
        AC_PATH_PROG(gcc_cv_ld, $LD_FOR_TARGET)
fi])

ORIGINAL_PLUGIN_LD_FOR_TARGET=$gcc_cv_ld
PLUGIN_LD_SUFFIX=`basename $gcc_cv_ld | sed -e "s,$target_alias-,,"`
# if the PLUGIN_LD is set ld-new, just have it as ld
# as that is the installed named.
if test x$PLUGIN_LD_SUFFIX = xld-new \
   || test x$PLUGIN_LD_SUFFIX = xcollect-ld ; then
  PLUGIN_LD_SUFFIX=ld
fi
AC_ARG_WITH(plugin-ld,
[AS_HELP_STRING([[--with-plugin-ld=[ARG]]], [specify the plugin linker])],
[if test x"$withval" != x; then
   ORIGINAL_PLUGIN_LD_FOR_TARGET="$withval"
   PLUGIN_LD_SUFFIX=`echo $withval | sed -e "s,$target_alias-,,"`
 fi])
AC_SUBST(ORIGINAL_PLUGIN_LD_FOR_TARGET)
AC_DEFINE_UNQUOTED(PLUGIN_LD_SUFFIX, "$PLUGIN_LD_SUFFIX", [Specify plugin linker])

# Check to see if we are using gold instead of ld
AC_MSG_CHECKING(whether we are using gold)
ld_is_gold=no
if test x$gcc_cv_ld != x; then
  if $gcc_cv_ld --version 2>/dev/null | sed 1q \
     | grep "GNU gold" > /dev/null; then
    ld_is_gold=yes
  fi
fi
AC_MSG_RESULT($ld_is_gold)

ORIGINAL_LD_FOR_TARGET=$gcc_cv_ld
AC_SUBST(ORIGINAL_LD_FOR_TARGET)
case "$ORIGINAL_LD_FOR_TARGET" in
  ./collect-ld | ./collect-ld$build_exeext) ;;
  *) AC_CONFIG_FILES(collect-ld:exec-tool.in, [chmod +x collect-ld]) ;;
esac 

AC_MSG_CHECKING(what linker to use)
if test "$gcc_cv_ld" = ../ld/ld-new$build_exeext \
   || test "$gcc_cv_ld" = ../gold/ld-new$build_exeext; then
	# Single tree build which includes ld.  We want to prefer it
	# over whatever linker top-level may have detected, since
	# we'll use what we're building after installation anyway.
	AC_MSG_RESULT(newly built ld)
	in_tree_ld=yes
	in_tree_ld_is_elf=no
	if (grep 'EMUL = .*elf' ../ld/Makefile \
	    || grep 'EMUL = .*linux' ../ld/Makefile \
	    || grep 'EMUL = .*lynx' ../ld/Makefile) > /dev/null; then
	  in_tree_ld_is_elf=yes
	elif test "$ld_is_gold" = yes; then
	  in_tree_ld_is_elf=yes
	fi
	for f in $gcc_cv_ld_bfd_srcdir/configure $gcc_cv_ld_gld_srcdir/configure $gcc_cv_ld_gld_srcdir/configure.in $gcc_cv_ld_gld_srcdir/Makefile.in
	do
changequote(,)dnl
		gcc_cv_gld_version=`sed -n -e 's/^[ 	]*VERSION=[^0-9A-Za-z_]*\([0-9]*\.[0-9]*.*\)/VERSION=\1/p' < $f`
		if test x$gcc_cv_gld_version != x; then
			break
		fi
	done
	case $gcc_cv_gld_version in
	  VERSION=[0-9]*) ;;
changequote([,])dnl
	  *) AC_MSG_ERROR([[cannot find version of in-tree linker]]) ;;
changequote(,)dnl
	esac
	gcc_cv_gld_major_version=`expr "$gcc_cv_gld_version" : "VERSION=\([0-9]*\)"`
	gcc_cv_gld_minor_version=`expr "$gcc_cv_gld_version" : "VERSION=[0-9]*\.\([0-9]*\)"`
changequote([,])dnl
	ORIGINAL_LD_BFD_FOR_TARGET=../ld/ld-new$build_exeext
	ORIGINAL_LD_GOLD_FOR_TARGET=../gold/ld-new$build_exeext
else
	AC_MSG_RESULT($gcc_cv_ld)
	in_tree_ld=no
	gcc_cvs_ld_program=`dirname $gcc_cv_ld`/`basename $gcc_cv_ld $host_exeext`
	ORIGINAL_LD_BFD_FOR_TARGET=${gcc_cvs_ld_program}.bfd$host_exeext
	ORIGINAL_LD_GOLD_FOR_TARGET=${gcc_cvs_ld_program}.gold$host_exeext
fi

AC_SUBST(ORIGINAL_LD_BFD_FOR_TARGET)
AC_SUBST(ORIGINAL_LD_GOLD_FOR_TARGET)

# Figure out what nm we will be using.
gcc_cv_binutils_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/binutils
AS_VAR_SET_IF(gcc_cv_nm,, [
if test -f $gcc_cv_binutils_srcdir/configure.in \
     && test -f ../binutils/Makefile \
     && test x$build = x$host; then
	gcc_cv_nm=../binutils/nm-new$build_exeext
elif test -x nm$build_exeext; then
	gcc_cv_nm=./nm$build_exeext
elif ( set dummy $NM_FOR_TARGET; test -x $[2] ); then
        gcc_cv_nm="$NM_FOR_TARGET"
else
        AC_PATH_PROG(gcc_cv_nm, $NM_FOR_TARGET)
fi])

AC_MSG_CHECKING(what nm to use)
if test "$gcc_cv_nm" = ../binutils/nm-new$build_exeext; then
	# Single tree build which includes binutils.
	AC_MSG_RESULT(newly built nm)
	in_tree_nm=yes
else
	AC_MSG_RESULT($gcc_cv_nm)
	in_tree_nm=no
fi

ORIGINAL_NM_FOR_TARGET=$gcc_cv_nm
AC_SUBST(ORIGINAL_NM_FOR_TARGET)
case "$ORIGINAL_NM_FOR_TARGET" in
  ./nm | ./nm$build_exeext) ;;
  *) AC_CONFIG_FILES(nm:exec-tool.in, [chmod +x nm]) ;;
esac


# Figure out what objdump we will be using.
AS_VAR_SET_IF(gcc_cv_objdump,, [
if test -f $gcc_cv_binutils_srcdir/configure.in \
     && test -f ../binutils/Makefile \
     && test x$build = x$host; then
	# Single tree build which includes binutils.
	gcc_cv_objdump=../binutils/objdump$build_exeext
elif test -x objdump$build_exeext; then
	gcc_cv_objdump=./objdump$build_exeext
elif ( set dummy $OBJDUMP_FOR_TARGET; test -x $[2] ); then
        gcc_cv_objdump="$OBJDUMP_FOR_TARGET"
else
        AC_PATH_PROG(gcc_cv_objdump, $OBJDUMP_FOR_TARGET)
fi])

AC_MSG_CHECKING(what objdump to use)
if test "$gcc_cv_objdump" = ../binutils/objdump$build_exeext; then
	# Single tree build which includes binutils.
	AC_MSG_RESULT(newly built objdump)
elif test x$gcc_cv_objdump = x; then
	AC_MSG_RESULT(not found)
else
	AC_MSG_RESULT($gcc_cv_objdump)
fi

# Figure out what readelf we will be using.
AS_VAR_SET_IF(gcc_cv_readelf,, [
if test -f $gcc_cv_binutils_srcdir/configure.in \
     && test -f ../binutils/Makefile \
     && test x$build = x$host; then
	# Single tree build which includes binutils.
	gcc_cv_readelf=../binutils/readelf$build_exeext
elif test -x readelf$build_exeext; then
	gcc_cv_readelf=./readelf$build_exeext
else
        AC_PATH_PROG(gcc_cv_readelf, readelf)
fi])

AC_MSG_CHECKING(what readelf to use)
if test "$gcc_cv_readelf" = ../binutils/readelf$build_exeext; then
	# Single tree build which includes binutils.
	AC_MSG_RESULT(newly built readelf)
elif test x$gcc_cv_readelf = x; then
	AC_MSG_RESULT(not found)
else
	AC_MSG_RESULT($gcc_cv_readelf)
fi

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

gcc_GAS_CHECK_FEATURE([.literal16], gcc_cv_as_literal16,
 [2,8,0],,
 [.literal16],,
[AC_DEFINE(HAVE_GAS_LITERAL16, 1,
  [Define if your assembler supports .literal16.])])

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

gcc_GAS_CHECK_FEATURE([.weakref], gcc_cv_as_weakref,
 [2,17,0],,
 [	.weakref foobar, barfnot],,
[AC_DEFINE(HAVE_GAS_WEAKREF, 1, [Define if your assembler supports .weakref.])])

gcc_GAS_CHECK_FEATURE([.nsubspa comdat], gcc_cv_as_nsubspa_comdat,
 [2,15,91],,
 [	.SPACE $TEXT$
	.NSUBSPA $CODE$,COMDAT],,
[AC_DEFINE(HAVE_GAS_NSUBSPA_COMDAT, 1, [Define if your assembler supports .nsubspa comdat option.])])

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
foobar:],[
# Solaris 9/x86 as incorrectly emits an alias for a hidden symbol with
# STV_HIDDEN, so disable .hidden support if so.
case "${target}" in
  i?86-*-solaris2* | x86_64-*-solaris2.1[[0-9]]*)
    if test x$gcc_cv_as != x && test x$gcc_cv_objdump != x; then
      cat > conftest.s <<EOF
.globl hidden
        .hidden hidden
hidden:
.globl default
        .set    default,hidden
EOF
      if $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1 \
        && $gcc_cv_objdump -t conftest.o 2>/dev/null | \
        grep '\.hidden default' > /dev/null; then
        gcc_cv_as_hidden=no
      else
        gcc_cv_as_hidden=yes
      fi
    else
      # Assume bug is present if objdump is missing.
      gcc_cv_as_hidden=no
    fi
    ;;
  *)
    gcc_cv_as_hidden=yes
    ;;
esac])
case "${target}" in
  *-*-darwin*)
    # Darwin as has some visibility support, though with a different syntax.
    gcc_cv_as_hidden=yes
    ;;
esac

# gnu_indirect_function type is an extension proposed at
# http://groups.google/com/group/generic-abi/files. It allows dynamic runtime
# selection of function implementation
AC_ARG_ENABLE(gnu-indirect-function,
 [AS_HELP_STRING([--enable-gnu-indirect-function],
                 [enable the use of the @gnu_indirect_function to glibc systems])],
 [case $enable_gnu_indirect_function in
    yes | no) ;;
    *) AC_MSG_ERROR(['$enable_gnu_indirect_function' is an invalid value for --enable-gnu-indirect-function.
Valid choices are 'yes' and 'no'.]) ;;
  esac],
 [enable_gnu_indirect_function="$default_gnu_indirect_function"])

gif=`if test x$enable_gnu_indirect_function = xyes; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(HAVE_GNU_INDIRECT_FUNCTION, $gif,
[Define if your system supports gnu indirect functions.])


changequote(,)dnl
if test $in_tree_ld != yes ; then
  ld_ver=`$gcc_cv_ld --version 2>/dev/null | sed 1q`
  if echo "$ld_ver" | grep GNU > /dev/null; then
    if test x"$ld_is_gold" = xyes; then
      # GNU gold --version looks like this:
      #
      # GNU gold (GNU Binutils 2.21.51.20110225) 1.11
      #
      # We extract the binutils version which is more familiar and specific
      # than the gold version.
      ld_vers=`echo $ld_ver | sed -n \
	  -e 's,^[^)]*[	 ]\([0-9][0-9]*\.[0-9][0-9]*[^)]*\)) .*$,\1,p'`
    else
      # GNU ld --version looks like this:
      #
      # GNU ld (GNU Binutils) 2.21.51.20110225
      ld_vers=`echo $ld_ver | sed -n \
	  -e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*.*\)$,\1,p'`
    fi
    ld_date=`echo $ld_ver | sed -n 's,^.*\([2-9][0-9][0-9][0-9]\)[-]*\([01][0-9]\)[-]*\([0-3][0-9]\).*$,\1\2\3,p'`
    ld_vers_major=`expr "$ld_vers" : '\([0-9]*\)'`
    ld_vers_minor=`expr "$ld_vers" : '[0-9]*\.\([0-9]*\)'`
    ld_vers_patch=`expr "$ld_vers" : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
  else
    case "${target}" in
      *-*-solaris2*)
	# See acinclude.m4 (gcc_SUN_LD_VERSION) for the version number
	# format.
	#
	# Don't reuse gcc_gv_sun_ld_vers_* in case a linker other than
	# /usr/ccs/bin/ld has been configured.
	ld_ver=`$gcc_cv_ld -V 2>&1`
	if echo "$ld_ver" | grep 'Solaris Link Editors' > /dev/null; then
	  ld_vers=`echo $ld_ver | sed -n \
	    -e 's,^.*: 5\.[0-9][0-9]*-\([0-9]\.[0-9][0-9]*\).*$,\1,p'`
	  ld_vers_major=`expr "$ld_vers" : '\([0-9]*\)'`
	  ld_vers_minor=`expr "$ld_vers" : '[0-9]*\.\([0-9]*\)'`
	fi
	;;
    esac
  fi
fi
changequote([,])dnl

AC_CACHE_CHECK(linker for .hidden support, gcc_cv_ld_hidden,
[[if test $in_tree_ld = yes ; then
  gcc_cv_ld_hidden=no
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 13 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
     gcc_cv_ld_hidden=yes
  fi
else
  gcc_cv_ld_hidden=yes
  if test x"$ld_is_gold" = xyes; then
    :
  elif echo "$ld_ver" | grep GNU > /dev/null; then
    case "${target}" in
      mmix-knuth-mmixware)
        # The linker emits by default mmo, not ELF, so "no" is appropriate.
	gcc_cv_ld_hidden=no
	;;
    esac
    if test 0"$ld_date" -lt 20020404; then
      if test -n "$ld_date"; then
	# If there was date string, but was earlier than 2002-04-04, fail
	gcc_cv_ld_hidden=no
      elif test -z "$ld_vers"; then
	# If there was no date string nor ld version number, something is wrong
	gcc_cv_ld_hidden=no
      else
	test -z "$ld_vers_patch" && ld_vers_patch=0
	if test "$ld_vers_major" -lt 2; then
	  gcc_cv_ld_hidden=no
	elif test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -lt 12; then
	  gcc_cv_ld_hidden="no"
	elif test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -eq 12 -a "$ld_vers_patch" -eq 0; then
	  gcc_cv_ld_hidden=no
	fi
      fi
    fi
  else
    case "${target}" in
      *-*-darwin*)
	# Darwin ld has some visibility support.
	gcc_cv_ld_hidden=yes
        ;;
      hppa64*-*-hpux* | ia64*-*-hpux*)
	gcc_cv_ld_hidden=yes
	;;
      *-*-solaris2.9* | *-*-solaris2.1[0-9]*)
	# Support for .hidden in Sun ld appeared in Solaris 9 FCS, but
	# .symbolic was only added in Solaris 9 12/02.
        gcc_cv_ld_hidden=yes
	;;
      *)
	gcc_cv_ld_hidden=no
	;;
    esac
  fi
fi]])
libgcc_visibility=no
AC_SUBST(libgcc_visibility)
GCC_TARGET_TEMPLATE([HAVE_GAS_HIDDEN])
if test $gcc_cv_as_hidden = yes && test $gcc_cv_ld_hidden = yes; then
  libgcc_visibility=yes
  AC_DEFINE(HAVE_GAS_HIDDEN, 1,
  [Define if your assembler and linker support .hidden.])
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

gcc_AC_INITFINI_ARRAY

# Check if we have .[us]leb128, and support symbol arithmetic with it.
gcc_GAS_CHECK_FEATURE([.sleb128 and .uleb128], gcc_cv_as_leb128,
  [elf,2,11,0],,
[	.data
	.uleb128 L2 - L1
L1:
	.uleb128 1280
	.sleb128 -1010
L2:],
[[# GAS versions before 2.11 do not support uleb128,
  # despite appearing to.
  # ??? There exists an elf-specific test that will crash
  # the assembler.  Perhaps it's better to figure out whether
  # arbitrary sections are supported and try the test.
  as_ver=`$gcc_cv_as --version 2>/dev/null | sed 1q`
  if echo "$as_ver" | grep GNU > /dev/null; then
    as_vers=`echo $as_ver | sed -n \
	-e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*.*\)$,\1,p'`
    as_major=`expr "$as_vers" : '\([0-9]*\)'`
    as_minor=`expr "$as_vers" : '[0-9]*\.\([0-9]*\)'`
    if test $as_major -eq 2 && test $as_minor -lt 11
    then :
    else gcc_cv_as_leb128=yes
    fi
  fi]],
  [AC_DEFINE(HAVE_AS_LEB128, 1,
    [Define if your assembler supports .sleb128 and .uleb128.])])

# Check if we have assembler support for unwind directives.
gcc_GAS_CHECK_FEATURE([cfi directives], gcc_cv_as_cfi_directive,
  ,,
[	.text
	.cfi_startproc
	.cfi_offset 0, 0
	.cfi_same_value 1
	.cfi_def_cfa 1, 2
	.cfi_escape 1, 2, 3, 4, 5
	.cfi_endproc],
[case "$target" in
  *-*-solaris*)
    # If the linker used on Solaris (like Sun ld) isn't capable of merging
    # read-only and read-write sections, we need to make sure that the
    # assembler used emits read-write .eh_frame sections.
    if test "x$gcc_cv_ld_ro_rw_mix" != xread-write; then
      if test "x$gcc_cv_objdump" != x; then
	if $gcc_cv_objdump -h conftest.o 2>/dev/null | \
		sed -e /.eh_frame/!d -e N | grep READONLY > /dev/null; then
	  gcc_cv_as_cfi_directive=no
	else
	  case "$target" in
	    i?86-*-solaris2.1[[0-9]]* | x86_64-*-solaris2.1[[0-9]]*)
	      # On Solaris/x86, make sure that GCC and gas agree on using
	      # read-only .eh_frame sections for 64-bit.
	      if $gcc_cv_as --64 -o conftest.o conftest.s > /dev/null 2>&1 && \
		$gcc_cv_objdump -h conftest.o 2>/dev/null | \
			sed -e /.eh_frame/!d -e N | \
			grep READONLY > /dev/null; then
		gcc_cv_as_cfi_directive=yes
	      else
		gcc_cv_as_cfi_directive=no
	      fi
	      ;;
	    *)
	      gcc_cv_as_cfi_directive=yes
	      ;;
	  esac 
	fi
      else
        # no objdump, err on the side of caution
	gcc_cv_as_cfi_directive=no
      fi
    else
      gcc_cv_as_cfi_directive=yes
    fi
    ;;
  *-*-*)
    gcc_cv_as_cfi_directive=yes
    ;;
esac])
if test $gcc_cv_as_cfi_directive = yes && test x$gcc_cv_objdump != x; then
gcc_GAS_CHECK_FEATURE([working cfi advance], gcc_cv_as_cfi_advance_working,
  ,,
[	.text
	.cfi_startproc
	.cfi_adjust_cfa_offset 64
	.skip 75040, 0
	.cfi_adjust_cfa_offset 128
	.cfi_endproc],
[[
if $gcc_cv_objdump -Wf conftest.o 2>/dev/null \
    | grep 'DW_CFA_advance_loc[24]:[ 	][ 	]*75040[ 	]' >/dev/null; then
   gcc_cv_as_cfi_advance_working=yes
fi
]])
else
  # no objdump, err on the side of caution
  gcc_cv_as_cfi_advance_working=no
fi
GCC_TARGET_TEMPLATE(HAVE_GAS_CFI_DIRECTIVE)
AC_DEFINE_UNQUOTED(HAVE_GAS_CFI_DIRECTIVE,
  [`if test $gcc_cv_as_cfi_directive = yes \
       && test $gcc_cv_as_cfi_advance_working = yes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports CFI directives.])

GCC_TARGET_TEMPLATE(HAVE_GAS_CFI_PERSONALITY_DIRECTIVE)
gcc_GAS_CHECK_FEATURE([cfi personality directive],
  gcc_cv_as_cfi_personality_directive, ,,
[	.text
	.cfi_startproc
	.cfi_personality 0, symbol
	.cfi_endproc])
AC_DEFINE_UNQUOTED(HAVE_GAS_CFI_PERSONALITY_DIRECTIVE,
  [`if test $gcc_cv_as_cfi_personality_directive = yes;
    then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports .cfi_personality.])

gcc_GAS_CHECK_FEATURE([cfi sections directive],
  gcc_cv_as_cfi_sections_directive, ,,
[	.text
	.cfi_sections .debug_frame, .eh_frame
	.cfi_startproc
	.cfi_endproc],
[case $target_os in
  win32 | pe | cygwin* | mingw32* | uwin*)
    # Need to check that we generated the correct relocation for the
    # .debug_frame section.  This was fixed for binutils 2.21.
    gcc_cv_as_cfi_sections_directive=no
    if test "x$gcc_cv_objdump" != x; then
     if $gcc_cv_objdump -j .debug_frame -r conftest.o 2>/dev/null | \
	grep secrel > /dev/null; then
      gcc_cv_as_cfi_sections_directive=yes
     fi
    fi
    ;;
  *)
    gcc_cv_as_cfi_sections_directive=yes
    ;;
esac])
GCC_TARGET_TEMPLATE(HAVE_GAS_CFI_SECTIONS_DIRECTIVE)
AC_DEFINE_UNQUOTED(HAVE_GAS_CFI_SECTIONS_DIRECTIVE,
  [`if test $gcc_cv_as_cfi_sections_directive = yes;
    then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports .cfi_sections.])

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
  # The text in the here-document typically retains its unix-style line
  # endings, while the output of objdump will use host line endings.
  # Therefore, use diff -b for the comparisons.
  if test x$gcc_cv_objdump != x \
  && $gcc_cv_objdump -s -j .eh_frame conftest.o 2>/dev/null \
     | tail -3 > conftest.got \
  && { diff -b conftest.lit conftest.got > /dev/null 2>&1 \
    || diff -b conftest.big conftest.got > /dev/null 2>&1; }
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
if test $gcc_cv_as_shf_merge = no; then
  gcc_GAS_CHECK_FEATURE(section merging support, gcc_cv_as_shf_merge,
    [elf,2,12,0], [--fatal-warnings],
    [.section .rodata.str, "aMS", %progbits, 1])
fi
AC_DEFINE_UNQUOTED(HAVE_GAS_SHF_MERGE,
  [`if test $gcc_cv_as_shf_merge = yes; then echo 1; else echo 0; fi`],
[Define 0/1 if your assembler supports marking sections with SHF_MERGE flag.])

gcc_GAS_CHECK_FEATURE([COMDAT group support (GNU as)],
 gcc_cv_as_comdat_group,
 [elf,2,16,0], [--fatal-warnings],
 [.section .text,"axG",@progbits,.foo,comdat])
if test $gcc_cv_as_comdat_group = yes; then
  gcc_cv_as_comdat_group_percent=no
  gcc_cv_as_comdat_group_group=no
else
 gcc_GAS_CHECK_FEATURE([COMDAT group support (GNU as, %type)],
   gcc_cv_as_comdat_group_percent,
   [elf,2,16,0], [--fatal-warnings],
   [.section .text,"axG",%progbits,.foo,comdat])
 if test $gcc_cv_as_comdat_group_percent = yes; then
   gcc_cv_as_comdat_group_group=no
 else
   if test -z "${gcc_cv_as_comdat_group_group+set}"; then
     gcc_cv_as_comdat_group_group=no
   fi
   case "${target}" in
     # Sun as uses a completely different syntax.
     *-*-solaris2*)
       case "${target}" in
         sparc*-*-solaris2*)
           conftest_s='
               .group foo,".text%foo",#comdat
               .section ".text%foo", #alloc,#execinstr,#progbits
               .globl foo
             foo:
	     '
           ;;
         i?86-*-solaris2* | x86_64-*-solaris2.1[[0-9]]*)
	   conftest_s='
               .group foo,.text%foo,#comdat
               .section .text%foo, "ax", @progbits
               .globl  foo
             foo:
	     '
	   ;;
       esac
       gcc_GAS_CHECK_FEATURE([COMDAT group support (Sun as, .group)],
         gcc_cv_as_comdat_group_group,
         ,, [$conftest_s])
       ;;
   esac
 fi
fi
if test x"$ld_is_gold" = xyes; then
  comdat_group=yes
elif test $in_tree_ld = yes ; then
  comdat_group=no
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 16 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
     comdat_group=yes
  fi
elif echo "$ld_ver" | grep GNU > /dev/null; then
  comdat_group=yes
  if test 0"$ld_date" -lt 20050308; then
    if test -n "$ld_date"; then
      # If there was date string, but was earlier than 2005-03-08, fail
      comdat_group=no
    elif test "$ld_vers_major" -lt 2; then
      comdat_group=no
    elif test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -lt 16; then
      comdat_group=no
    fi
  fi
else
changequote(,)dnl
  case "${target}" in
    *-*-solaris2.1[1-9]*)
      comdat_group=no
      # Sun ld has COMDAT group support since Solaris 9, but it doesn't
      # interoperate with GNU as until Solaris 11 build 130, i.e. ld
      # version 1.688.
      #
      # If using Sun as for COMDAT group as emitted by GCC, one needs at
      # least ld version 1.2267.
      if test "$ld_vers_major" -gt 1; then
        comdat_group=yes
      elif test "x$gas_flag" = xyes && test "$ld_vers_minor" -ge 1688; then
	comdat_group=yes
      elif test "$ld_vers_minor" -ge 2267; then
	comdat_group=yes
      fi
      ;;
    *)
      # Assume linkers other than GNU ld don't support COMDAT group.
      comdat_group=no
      ;;
  esac
changequote([,])dnl
fi
# Allow overriding the automatic COMDAT group tests above.
AC_ARG_ENABLE(comdat,
  [AS_HELP_STRING([--enable-comdat], [enable COMDAT group support])],
  [comdat_group="$enable_comdat"])
if test $comdat_group = no; then
  gcc_cv_as_comdat_group=no
  gcc_cv_as_comdat_group_percent=no
  gcc_cv_as_comdat_group_group=no
fi
AC_DEFINE_UNQUOTED(HAVE_COMDAT_GROUP,
  [`if test $gcc_cv_as_comdat_group = yes \
    || test $gcc_cv_as_comdat_group_percent = yes \
    || test $gcc_cv_as_comdat_group_group = yes; then echo 1; else echo 0; fi`],
[Define 0/1 if your assembler and linker support COMDAT groups.])

gcc_GAS_CHECK_FEATURE([line table discriminator support],
 gcc_cv_as_discriminator,
 [2,19,51],,
[	.text
	.file 1 "conf.c"
	.loc 1 1 0 discriminator 1],,
[AC_DEFINE(HAVE_GAS_DISCRIMINATOR, 1,
  [Define if your assembler supports the .loc discriminator sub-directive.])])

# Thread-local storage - the check is heavily parameterized.
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
  cris-*-*|crisv32-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
x:      .long   25
        .text
	move.d x:IE,$r10
	nop'
	tls_first_major=2
	tls_first_minor=20
	tls_as_opt=--fatal-warnings
	;;
  frv*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
x:      .long   25
        .text
        call    #gettlsoff(x)'
	tls_first_major=2
	tls_first_minor=14
	;;
  hppa*-*-linux*)
    conftest_s='
t1:	.reg	%r20
t2:	.reg	%r21
gp:	.reg	%r19
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	.align	4
	addil LT%foo-$tls_gdidx$,gp
	ldo RT%foo-$tls_gdidx$(%r1),%arg0
	b __tls_get_addr
	nop 		
	addil LT%foo-$tls_ldidx$,gp
	b __tls_get_addr
	ldo RT%foo-$tls_ldidx$(%r1),%arg0
	addil LR%foo-$tls_dtpoff$,%ret0
	ldo RR%foo-$tls_dtpoff$(%r1),%t1
	mfctl %cr27,%t1 		
	addil LT%foo-$tls_ieoff$,gp
	ldw RT%foo-$tls_ieoff$(%r1),%t2
	add %t1,%t2,%t3 		
	mfctl %cr27,%t1 		
	addil LR%foo-$tls_leoff$,%t1
	ldo RR%foo-$tls_leoff$(%r1),%t2'
	tls_first_major=2
	tls_first_minor=15
	tls_as_opt=--fatal-warnings
	;;
  arm*-*-*)
    conftest_s='
	.section ".tdata","awT",%progbits
foo:	.long	25
	.text
.word foo(gottpoff)
.word foo(tpoff)
.word foo(tlsgd)
.word foo(tlsldm)
.word foo(tlsldo)'
	tls_first_major=2
	tls_first_minor=17
	;;
  i[34567]86-*-* | x86_64-*-*)
    case "$target" in
      i[34567]86-*-solaris2.*)
	on_solaris=yes
	tga_func=___tls_get_addr
	;;
      x86_64-*-solaris2.1[0-9]*)
	on_solaris=yes
	tga_func=__tls_get_addr
        ;;
      *)
	on_solaris=no
	;;
    esac
    if test x$on_solaris = xyes && test x$gas_flag = xno; then
      conftest_s='
	.section .tdata,"awt",@progbits'
      tls_first_major=0
      tls_first_minor=0
      tls_section_flag=t
changequote([,])dnl
      AC_DEFINE(TLS_SECTION_ASM_FLAG, 't',
[Define to the flag used to mark TLS sections if the default (`T') doesn't work.])
changequote(,)dnl
    else
      conftest_s='
	.section ".tdata","awT",@progbits'
      tls_first_major=2
      tls_first_minor=14
      tls_section_flag=T
      tls_as_opt="--fatal-warnings"
    fi
    case "$target" in
      i[34567]86-*-*)
	conftest_s="$conftest_s
foo:	.long	25
	.text
	movl	%gs:0, %eax
	leal	foo@tlsgd(,%ebx,1), %eax
	leal	foo@tlsldm(%ebx), %eax
	leal	foo@dtpoff(%eax), %edx
	movl	foo@gottpoff(%ebx), %eax
	subl	foo@gottpoff(%ebx), %eax
	addl	foo@gotntpoff(%ebx), %eax
	movl	foo@indntpoff, %eax
	movl	\$foo@tpoff, %eax
	subl	\$foo@tpoff, %eax
	leal	foo@ntpoff(%ecx), %eax"
	;;
      x86_64-*-*)
	if test x$on_solaris = xyes; then
	  case $gas_flag in
	    yes) tls_as_opt="$tls_as_opt --64" ;;
	    no)	 tls_as_opt="$tls_as_opt -xarch=amd64" ;;
	  esac	  
	fi
	conftest_s="$conftest_s
foo:	.long	25
	.text
	movq	%fs:0, %rax
	leaq	foo@tlsgd(%rip), %rdi
	leaq	foo@tlsld(%rip), %rdi
	leaq	foo@dtpoff(%rax), %rdx
	movq	foo@gottpoff(%rip), %rax
	movq	\$foo@tpoff, %rax"
        ;;
    esac
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
  microblaze*-*-*)
    conftest_s='
	.section .tdata,"awT",@progbits
x:
	.word 2
	.text
	addik r5,r20,x@TLSGD
	addik r5,r20,x@TLSLDM'
	tls_first_major=2
	tls_first_minor=20
	tls_as_opt='--fatal-warnings'
	;;
  mips*-*-*)
    conftest_s='
	.section .tdata,"awT",@progbits
x:
	.word 2
	.text
	addiu $4, $28, %tlsgd(x)
	addiu $4, $28, %tlsldm(x)
	lui $4, %dtprel_hi(x)
	addiu $4, $4, %dtprel_lo(x)
	lw $4, %gottprel(x)($28)
	lui $4, %tprel_hi(x)
	addiu $4, $4, %tprel_lo(x)'
	tls_first_major=2
	tls_first_minor=16
	tls_as_opt='-32 --fatal-warnings'
	;;
  m68k-*-*)
    conftest_s='
	.section .tdata,"awT",@progbits
x:
	.word 2
	.text
foo:
	move.l x@TLSGD(%a5),%a0
	move.l x@TLSLDM(%a5),%a0
	move.l x@TLSLDO(%a5),%a0
	move.l x@TLSIE(%a5),%a0
	move.l x@TLSLE(%a5),%a0'
	tls_first_major=2
	tls_first_minor=19
	tls_as_opt='--fatal-warnings'
	;;
  nios2-*-*)
      conftest_s='
	.section ".tdata","awT",@progbits'
	tls_first_major=2
	tls_first_minor=23
	tls_as_opt="--fatal-warnings"
	;;
  aarch64*-*-*)
    conftest_s='
	.section ".tdata","awT",%progbits
foo:	.long	25
	.text
	adrp  x0, :tlsgd:x
	add   x0, x0, #:tlsgd_lo12:x
        bl    __tls_get_addr
	nop'
	tls_first_major=2
	tls_first_minor=20
	tls_as_opt='--fatal-warnings'
	;;
  powerpc-ibm-aix*)
    conftest_s='
	.extern __get_tpointer
	.toc
LC..1:
	.tc a[TC],a[TL]@le
	.csect .text[PR]
.tlstest:
	lwz 9,LC..1(2)
	bla __get_tpointer
	lwzx 3,9,3
	.globl a
	.csect a[TL],4
a:
	.space 4'
	tls_first_major=0
	tls_first_minor=0
	;;
  powerpc64*-*-*)
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
  powerpc*-*-*)
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
	tga_func=__tls_get_addr
	;;
      *)
	on_solaris=no
	;;
    esac
    if test x$on_solaris = xyes && test x$gas_flag = xno; then
      conftest_s='
	.section ".tdata",#alloc,#write,#tls'
	tls_first_major=0
	tls_first_minor=0
    else
      conftest_s='
	.section ".tdata","awT",@progbits'
	tls_first_major=2
	tls_first_minor=14
	tls_as_opt="-32 --fatal-warnings"
    fi
    conftest_s="$conftest_s
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
	ld	[%g7 + %o5], %o1"
	;;
  tilepro*-*-*)
      conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	addli	r0, zero, tls_gd(foo)
	auli	r0, zero, tls_gd_ha16(foo)
	addli	r0, r0, tls_gd_lo16(foo)
	jal	__tls_get_addr
	addli	r0, zero, tls_ie(foo)
	auli	r0, r0, tls_ie_ha16(foo)
	addli	r0, r0, tls_ie_lo16(foo)'
	tls_first_major=2
	tls_first_minor=22
	tls_as_opt="--fatal-warnings"
	;;
  tilegx*-*-*)
      conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	shl16insli r0, zero, hw0_last_tls_gd(foo)
	shl16insli r0, zero, hw1_last_tls_gd(foo)
	shl16insli r0, r0,   hw0_tls_gd(foo)
	jal	   __tls_get_addr
	shl16insli r0, zero, hw1_last_tls_ie(foo)
	shl16insli r0, r0,   hw0_tls_ie(foo)'
	tls_first_major=2
	tls_first_minor=22
	tls_as_opt="--fatal-warnings"
	;;
  xtensa*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	movi	a8, foo@TLSFUNC
	movi	a10, foo@TLSARG
	callx8.tls a8, foo@TLSCALL'
	tls_first_major=2
	tls_first_minor=19
	;;
changequote([,])dnl
esac
set_have_as_tls=no
if test "x$enable_tls" = xno ; then
  : # TLS explicitly disabled.
elif test "x$enable_tls" = xyes ; then
  set_have_as_tls=yes # TLS explicitly enabled.
elif test -z "$tls_first_major"; then
  : # If we don't have a check, assume no support.
else
  gcc_GAS_CHECK_FEATURE(thread-local storage support, gcc_cv_as_tls,
  [$tls_first_major,$tls_first_minor,0], [$tls_as_opt], [$conftest_s],,
  [set_have_as_tls=yes])
fi
case "$target" in
  # TLS was introduced in the Solaris 9 FCS release.  Support for GNU-style
  # TLS on x86 was only introduced in Solaris 9 4/04, replacing the earlier
  # Sun style that Sun ld and GCC don't support any longer.
  *-*-solaris2.*)
    AC_MSG_CHECKING(linker and ld.so.1 TLS support)
    ld_tls_support=no
    # Check ld and ld.so.1 TLS support.
    if echo "$ld_ver" | grep GNU > /dev/null; then
      # Assume all interesting versions of GNU ld have TLS support.
      # FIXME: still need ld.so.1 support, i.e. ld version checks below.
      ld_tls_support=yes
    else
      case "$target" in
	# Solaris 9/x86 ld has GNU style TLS support since version 1.374.
        i?86-*-solaris2.9)
          min_tls_ld_vers_minor=374
          ;;
	# Solaris 9/SPARC and Solaris 10+ ld have TLS support since FCS.
        sparc*-*-solaris2.9 | *-*-solaris2.1[[0-9]]*)
	  min_tls_ld_vers_minor=343
          ;;
      esac
      if test "$ld_vers_major" -gt 1 || \
        test "$ld_vers_minor" -ge "$min_tls_ld_vers_minor"; then
 	ld_tls_support=yes
      else
        set_have_as_tls=no
      fi
    fi
    AC_MSG_RESULT($ld_tls_support)

    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS=
    LDFLAGS=

    AC_MSG_CHECKING(library containing $tga_func)
    # Before Solaris 10, __tls_get_addr (SPARC/x64) resp. ___tls_get_addr
    # (32-bit x86) only lived in libthread, so check for that.  Keep
    # set_have_as_tls if found, disable if not.
    AC_SEARCH_LIBS([$tga_func], [thread],, [set_have_as_tls=no])
    ld_tls_libs="$LIBS"
    # Clear LIBS if we cannot support TLS.
    if test $set_have_as_tls = no; then
      LIBS=
    fi
    # Always define LIB_TLS_SPEC, even without TLS support.
    AC_DEFINE_UNQUOTED(LIB_TLS_SPEC, "$LIBS",
        [Define to the library containing __tls_get_addr/___tls_get_addr.])
    AC_MSG_RESULT($LIBS)

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    ;;
esac
if test $set_have_as_tls = yes ; then
  AC_DEFINE(HAVE_AS_TLS, 1,
	    [Define if your assembler and linker support thread-local storage.])
fi

# Target-specific assembler checks.

AC_MSG_CHECKING(linker -Bstatic/-Bdynamic option)
gcc_cv_ld_static_dynamic=no
gcc_cv_ld_static_option='-Bstatic'
gcc_cv_ld_dynamic_option='-Bdynamic'
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 10 -o "$gcc_cv_gld_major_version" -gt 2; then
    gcc_cv_ld_static_dynamic=yes
  fi
elif test x$gcc_cv_ld != x; then
  # Check if linker supports -Bstatic/-Bdynamic option
  if $gcc_cv_ld --help 2>/dev/null | grep -- -Bstatic > /dev/null \
     && $gcc_cv_ld --help 2>/dev/null | grep -- -Bdynamic > /dev/null; then
      gcc_cv_ld_static_dynamic=yes
  else
    case "$target" in
      # AIX ld uses -b flags
      *-*-aix4.[[23]]* | *-*-aix[[5-9]]*)
	gcc_cv_ld_static_dynamic=yes
	gcc_cv_ld_static_option="-bstatic"
	gcc_cv_ld_dynamic_option="-bdynamic"
	;;
      # HP-UX ld uses -a flags to select between shared and archive.
      *-*-hpux*)
	if test x"$gnu_ld" = xno; then
	  gcc_cv_ld_static_dynamic=yes
	  gcc_cv_ld_static_option="-aarchive_shared"
	  gcc_cv_ld_dynamic_option="-adefault"
	fi
	;;
      # Solaris 2 ld always supports -Bstatic/-Bdynamic.
      *-*-solaris2*)
        gcc_cv_ld_static_dynamic=yes
        ;;
    esac
  fi
fi
if test x"$gcc_cv_ld_static_dynamic" = xyes; then
	AC_DEFINE(HAVE_LD_STATIC_DYNAMIC, 1,
[Define if your linker supports -Bstatic/-Bdynamic or equivalent options.])
	AC_DEFINE_UNQUOTED(LD_STATIC_OPTION, "$gcc_cv_ld_static_option",
[Define to the linker option to disable use of shared objects.])
	AC_DEFINE_UNQUOTED(LD_DYNAMIC_OPTION, "$gcc_cv_ld_dynamic_option",
[Define to the linker option to enable use of shared objects.])
fi
AC_MSG_RESULT($gcc_cv_ld_static_dynamic)

if test x"$demangler_in_ld" = xyes; then
  AC_MSG_CHECKING(linker --demangle support)
  gcc_cv_ld_demangle=no
  if test $in_tree_ld = yes; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 14 -o "$gcc_cv_gld_major_version" -gt 2; then \
      gcc_cv_ld_demangle=yes
    fi
  elif test x$gcc_cv_ld != x -a x"$gnu_ld" = xyes; then
    # Check if the GNU linker supports --demangle option
    if $gcc_cv_ld --help 2>/dev/null | grep no-demangle > /dev/null; then
      gcc_cv_ld_demangle=yes
    fi
  fi
  if test x"$gcc_cv_ld_demangle" = xyes; then
    AC_DEFINE(HAVE_LD_DEMANGLE, 1,
[Define if your linker supports --demangle option.])
  fi
  AC_MSG_RESULT($gcc_cv_ld_demangle)
fi

AC_MSG_CHECKING(linker plugin support)
gcc_cv_lto_plugin=0
if test -f liblto_plugin.la; then
  save_ld_ver="$ld_ver"
  save_ld_vers_major="$ld_vers_major"
  save_ld_vers_minor="$ld_vers_minor"
  save_ld_is_gold="$ld_is_gold"

  ld_is_gold=no

  if test $in_tree_ld = yes -a x"$ORIGINAL_PLUGIN_LD_FOR_TARGET" = x"$gcc_cv_ld"; then
    ld_ver="GNU ld"
    # FIXME: ld_is_gold?
    ld_vers_major="$gcc_cv_gld_major_version"
    ld_vers_minor="$gcc_cv_gld_minor_version"
  else
    # Determine plugin linker version.
    # FIXME: Partial duplicate from above, generalize.
changequote(,)dnl
    ld_ver=`$ORIGINAL_PLUGIN_LD_FOR_TARGET --version 2>/dev/null | sed 1q`
    if echo "$ld_ver" | grep GNU > /dev/null; then
      if echo "$ld_ver" | grep "GNU gold" > /dev/null; then
        ld_is_gold=yes
        ld_vers=`echo $ld_ver | sed -n \
    	    -e 's,^[^)]*[	 ]\([0-9][0-9]*\.[0-9][0-9]*[^)]*\)) .*$,\1,p'`
      else
        ld_vers=`echo $ld_ver | sed -n \
    	    -e 's,^.*[	 ]\([0-9][0-9]*\.[0-9][0-9]*.*\)$,\1,p'`
      fi
      ld_vers_major=`expr "$ld_vers" : '\([0-9]*\)'`
      ld_vers_minor=`expr "$ld_vers" : '[0-9]*\.\([0-9]*\)'`
    fi
changequote([,])dnl
  fi

  # Determine plugin support.
  if echo "$ld_ver" | grep GNU > /dev/null; then
    # Require GNU ld or gold 2.21+ for plugin support by default.
    if test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -ge 21; then
      gcc_cv_lto_plugin=2
    # Allow -fuse-linker-plugin to enable plugin support in GNU gold 2.20.
    elif test "$ld_is_gold" = yes -a "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -eq 20; then
      gcc_cv_lto_plugin=1
    fi
  fi

  ld_ver="$save_ld_ver"
  ld_vers_major="$save_ld_vers_major"
  ld_vers_minor="$save_ld_vers_minor"
  ld_is_gold="$save_ld_is_gold"
fi
AC_DEFINE_UNQUOTED(HAVE_LTO_PLUGIN, $gcc_cv_lto_plugin,
  [Define to the level of your linker's plugin support.])
AC_MSG_RESULT($gcc_cv_lto_plugin)

case "$target" in
  aarch64*-*-*)
    gcc_GAS_CHECK_FEATURE([-mabi option], gcc_cv_as_aarch64_mabi,,
                          [-mabi=lp64], [.text],,,)
    if test x$gcc_cv_as_aarch64_mabi = xyes; then
      AC_DEFINE(HAVE_AS_MABI_OPTION, 1,
                [Define if your assembler supports the -mabi option.])
    else
      if test x$with_abi = xilp32; then
        AC_MSG_ERROR([Assembler does not support -mabi=ilp32.\
                     Upgrade the Assembler.])
      fi
      if test x"$with_multilib_list" = xdefault; then
        TM_MULTILIB_CONFIG=lp64
      else
        aarch64_multilibs=`echo $with_multilib_list | sed -e 's/,/ /g'`
        for aarch64_multilib in ${aarch64_multilibs}; do
          case ${aarch64_multilib} in
            ilp32)
              AC_MSG_ERROR([Assembler does not support -mabi=ilp32.\
                            Upgrade the Assembler.])
              ;;
            *)
              ;;
          esac
        done
      fi
    fi
    ;;

  # All TARGET_ABI_OSF targets.
  alpha*-*-linux* | alpha*-*-*bsd*)
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
    gcc_GAS_CHECK_FEATURE([jsrdirect relocation support],
	gcc_cv_as_alpha_jsrdirect_relocs, [2,16,90],,
[	.set nomacro
	.text
	ldq	$27, a($29)	!literal!1
	jsr	$26, ($27), a	!lituse_jsrdirect!1],,
    [AC_DEFINE(HAVE_AS_JSRDIRECT_RELOCS, 1,
  [Define if your assembler supports the lituse_jsrdirect relocation.])])
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

    gcc_GAS_CHECK_FEATURE([@%:@nobits], gcc_cv_as_sparc_nobits,,,
      [.section "nobits",#alloc,#write,#nobits
       .section "progbits",#alloc,#write,#progbits])
    AC_DEFINE_UNQUOTED(HAVE_AS_SPARC_NOBITS,
      [`if test $gcc_cv_as_sparc_nobits = yes; then echo 1; else echo 0; fi`],
      [Define to 1 if your assembler supports #nobits, 0 otherwise.])

    gcc_GAS_CHECK_FEATURE([-relax option], gcc_cv_as_sparc_relax,,
      [-relax], [.text],,
      [AC_DEFINE(HAVE_AS_RELAX_OPTION, 1,
		[Define if your assembler supports -relax option.])])

    gcc_GAS_CHECK_FEATURE([GOTDATA_OP relocs],
      gcc_cv_as_sparc_gotdata_op,,
      [-K PIC],
[.text
.align 4
foo:
	nop
bar:
	sethi %gdop_hix22(foo), %g1
	xor    %g1, %gdop_lox10(foo), %g1
	ld    [[%l7 + %g1]], %g2, %gdop(foo)],
      [if test x$gcc_cv_ld != x \
       && $gcc_cv_ld -o conftest conftest.o -G > /dev/null 2>&1; then
         if test x$gcc_cv_objdump != x; then
           if $gcc_cv_objdump -s -j .text conftest 2> /dev/null \
              | grep ' 03000004 82186004 c405c001'> /dev/null 2>&1; then
	       gcc_cv_as_sparc_gotdata_op=no
           else
	       gcc_cv_as_sparc_gotdata_op=yes
           fi
         fi
       fi
       rm -f conftest],
      [AC_DEFINE(HAVE_AS_SPARC_GOTDATA_OP, 1,
		[Define if your assembler and linker support GOTDATA_OP relocs.])])

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
	 gcc_cv_as_sparc_offsetable_lo10=yes
       fi],
       [AC_DEFINE(HAVE_AS_OFFSETABLE_LO10, 1,
	         [Define if your assembler supports offsetable %lo().])])

    gcc_GAS_CHECK_FEATURE([FMAF, HPC, and VIS 3.0 instructions],
      gcc_cv_as_sparc_fmaf,,
      [-xarch=v9d],
      [.text
       .register %g2, #scratch
       .register %g3, #scratch
       .align 4
       fmaddd %f0, %f2, %f4, %f6
       addxccc %g1, %g2, %g3
       fsrl32 %f2, %f4, %f8
       fnaddd %f10, %f12, %f14],,
      [AC_DEFINE(HAVE_AS_FMAF_HPC_VIS3, 1,
                [Define if your assembler supports FMAF, HPC, and VIS 3.0 instructions.])])

    gcc_GAS_CHECK_FEATURE([SPARC4 instructions],
      gcc_cv_as_sparc_sparc4,,
      [-xarch=sparc4],
      [.text
       .register %g2, #scratch
       .register %g3, #scratch
       .align 4
       cxbe %g2, %g3, 1f
1:     cwbneg %g2, %g3, 1f
1:     sha1
       md5
       aes_kexpand0 %f4, %f6, %f8
       des_round %f38, %f40, %f42, %f44
       camellia_f %f54, %f56, %f58, %f60
       kasumi_fi_xor %f46, %f48, %f50, %f52],,
      [AC_DEFINE(HAVE_AS_SPARC4, 1,
                [Define if your assembler supports SPARC4 instructions.])])

    gcc_GAS_CHECK_FEATURE([LEON instructions],
      gcc_cv_as_sparc_leon,,
      [-Aleon],
      [.text
       .register %g2, #scratch
       .register %g3, #scratch
       .align 4
       smac %g2, %g3, %g1
       umac %g2, %g3, %g1
       cas [[%g2]], %g3, %g1],,
      [AC_DEFINE(HAVE_AS_LEON, 1,
                [Define if your assembler supports LEON instructions.])])
    ;;

changequote(,)dnl
  i[34567]86-*-* | x86_64-*-*)
changequote([,])dnl
    case $target_os in
      cygwin*)
	# Full C++ conformance when using a shared libstdc++-v3 requires some
	# support from the Cygwin DLL, which in more recent versions exports
	# wrappers to aid in interposing and redirecting operators new, delete,
	# etc., as per n2800 #17.6.4.6 [replacement.functions].  Check if we
	# are configuring for a version of Cygwin that exports the wrappers.
	if test x$host = x$target && test x$host_cpu = xi686; then
	  AC_CHECK_FUNC([__wrap__Znaj],[gcc_ac_cygwin_dll_wrappers=yes],[gcc_ac_cygwin_dll_wrappers=no])
	else
	  # Can't check presence of libc functions during cross-compile, so
	  # we just have to assume we're building for an up-to-date target.
	  gcc_ac_cygwin_dll_wrappers=yes
	fi
	AC_DEFINE_UNQUOTED(USE_CYGWIN_LIBSTDCXX_WRAPPERS,
	  [`if test $gcc_ac_cygwin_dll_wrappers = yes; then echo 1; else echo 0; fi`],
	  [Define if you want to generate code by default that assumes that the
	   Cygwin DLL exports wrappers to support libstdc++ function replacement.])
    esac
    case $target_os in
      cygwin* | pe | mingw32* | interix*)
	# Recent binutils allows the three-operand form of ".comm" on PE.  This
	# definition is used unconditionally to initialise the default state of
	# the target option variable that governs usage of the feature.
	gcc_GAS_CHECK_FEATURE([.comm with alignment], gcc_cv_as_comm_has_align,
	 [2,19,52],,[.comm foo,1,32])
	AC_DEFINE_UNQUOTED(HAVE_GAS_ALIGNED_COMM,
	  [`if test $gcc_cv_as_comm_has_align = yes; then echo 1; else echo 0; fi`],
	  [Define if your assembler supports specifying the alignment
	   of objects allocated using the GAS .comm command.])
	# Used for DWARF 2 in PE
	gcc_GAS_CHECK_FEATURE([.secrel32 relocs],
	  gcc_cv_as_ix86_pe_secrel32,
	  [2,15,91],,
[.text
foo:	nop
.data
	.secrel32 foo],
	  [if test x$gcc_cv_ld != x \
	   && $gcc_cv_ld -o conftest conftest.o > /dev/null 2>&1; then
	     gcc_cv_as_ix86_pe_secrel32=yes
	   fi
	   rm -f conftest],
	  [AC_DEFINE(HAVE_GAS_PE_SECREL32_RELOC, 1,
	    [Define if your assembler and linker support 32-bit section relative relocs via '.secrel32 label'.])])
	# Test if the assembler supports the extended form of the .section
	# directive that specifies section alignment.  LTO support uses this,
	# but normally only after installation, so we warn but don't fail the
	# configure if LTO is enabled but the assembler does not support it.
	gcc_GAS_CHECK_FEATURE([.section with alignment], gcc_cv_as_section_has_align,
	  [2,20,1],-fatal-warnings,[.section lto_test,"dr0"])
	if test x$gcc_cv_as_section_has_align != xyes; then
	  case ",$enable_languages," in
	    *,lto,*)
	      AC_MSG_WARN([LTO for $target requires binutils >= 2.20.1, but version found appears insufficient; LTO will not work until binutils is upgraded.])
	      ;;
	  esac
	fi
	# Test if the assembler supports the section flag 'e' for specifying
	# an excluded section.
	gcc_GAS_CHECK_FEATURE([.section with e], gcc_cv_as_section_has_e,
	  [2,22,51],,
[.section foo1,"e"
.byte 0,0,0,0])
	AC_DEFINE_UNQUOTED(HAVE_GAS_SECTION_EXCLUDE,
	  [`if test $gcc_cv_as_section_has_e = yes; then echo 1; else echo 0; fi`],
          [Define if your assembler supports specifying the section flag e.])
	;;
    esac

    gcc_GAS_CHECK_FEATURE([filds and fists mnemonics],
       gcc_cv_as_ix86_filds,,,
       [filds mem; fists mem],,
       [AC_DEFINE(HAVE_AS_IX86_FILDS, 1,
         [Define if your assembler uses filds and fists mnemonics.])])

    gcc_GAS_CHECK_FEATURE([fildq and fistpq mnemonics],
       gcc_cv_as_ix86_fildq,,,
       [fildq mem; fistpq mem],,
       [AC_DEFINE(HAVE_AS_IX86_FILDQ, 1,
         [Define if your assembler uses fildq and fistq mnemonics.])])

    gcc_GAS_CHECK_FEATURE([cmov syntax],
      gcc_cv_as_ix86_cmov_sun_syntax,,,
      [cmovl.l %edx, %eax],,
      [AC_DEFINE(HAVE_AS_IX86_CMOV_SUN_SYNTAX, 1,
        [Define if your assembler supports the Sun syntax for cmov.])])

    gcc_GAS_CHECK_FEATURE([ffreep mnemonic],
      gcc_cv_as_ix86_ffreep,,,
      [ffreep %st(1)],,
      [AC_DEFINE(HAVE_AS_IX86_FFREEP, 1,
        [Define if your assembler supports the ffreep mnemonic.])])

    gcc_GAS_CHECK_FEATURE([.quad directive],
      gcc_cv_as_ix86_quad,,,
      [.quad 0],,
      [AC_DEFINE(HAVE_AS_IX86_QUAD, 1,
        [Define if your assembler supports the .quad directive.])])

    gcc_GAS_CHECK_FEATURE([sahf mnemonic],
      gcc_cv_as_ix86_sahf,,,
      [.code64
       sahf],,
      [AC_DEFINE(HAVE_AS_IX86_SAHF, 1,
        [Define if your assembler supports the sahf mnemonic in 64bit mode.])])

    gcc_GAS_CHECK_FEATURE([interunit movq mnemonic],
      gcc_cv_as_ix86_interunit_movq,,,
      [.code64
       movq %mm0, %rax
       movq %rax, %xmm0])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_INTERUNIT_MOVQ,
      [`if test $gcc_cv_as_ix86_interunit_movq = yes; then echo 1; else echo 0; fi`],
      [Define if your assembler supports interunit movq mnemonic.])

    gcc_GAS_CHECK_FEATURE([hle prefixes],
      gcc_cv_as_ix86_hle,,,
      [lock xacquire cmpxchg %esi, (%ecx)],,
      [AC_DEFINE(HAVE_AS_IX86_HLE, 1,
        [Define if your assembler supports HLE prefixes.])])

    gcc_GAS_CHECK_FEATURE([swap suffix],
      gcc_cv_as_ix86_swap,,,
      [movl.s %esp, %ebp],,
      [AC_DEFINE(HAVE_AS_IX86_SWAP, 1,
        [Define if your assembler supports the swap suffix.])])

    gcc_GAS_CHECK_FEATURE([different section symbol subtraction],
      gcc_cv_as_ix86_diff_sect_delta,,,
      [.section .rodata
.L1:
        .long .L2-.L1
        .long .L3-.L1
        .text
.L3:    nop
.L2:    nop],,
      [AC_DEFINE(HAVE_AS_IX86_DIFF_SECT_DELTA, 1,
        [Define if your assembler supports the subtraction of symbols in different sections.])])

    # These two are used unconditionally by i386.[ch]; it is to be defined
    # to 1 if the feature is present, 0 otherwise.
    as_ix86_gotoff_in_data_opt=
    if test x$gas = xyes; then
      as_ix86_gotoff_in_data_opt="--32"
    fi
    gcc_GAS_CHECK_FEATURE([GOTOFF in data],
      gcc_cv_as_ix86_gotoff_in_data, [2,11,0],
      [$as_ix86_gotoff_in_data_opt],
[	.text
.L0:
	nop
	.data
	.long .L0@GOTOFF])
    AC_DEFINE_UNQUOTED(HAVE_AS_GOTOFF_IN_DATA,
      [`if test $gcc_cv_as_ix86_gotoff_in_data = yes; then echo 1; else echo 0; fi`],
      [Define true if the assembler supports '.long foo@GOTOFF'.])

    gcc_GAS_CHECK_FEATURE([rep and lock prefix],
        gcc_cv_as_ix86_rep_lock_prefix,,,
	[rep movsl
	 rep ret
	 rep nop
	 rep bsf %ecx, %eax
	 rep bsr %ecx, %eax
	 lock addl %edi, (%eax,%esi)
	 lock orl $0, (%esp)],,
        [AC_DEFINE(HAVE_AS_IX86_REP_LOCK_PREFIX, 1,
          [Define if the assembler supports 'rep <insn>, lock <insn>'.])])

    gcc_GAS_CHECK_FEATURE([ud2 mnemonic],
	gcc_cv_as_ix86_ud2,,,
	[ud2],,
      [AC_DEFINE(HAVE_AS_IX86_UD2, 1,
	[Define if your assembler supports the 'ud2' mnemonic.])])

    gcc_GAS_CHECK_FEATURE([R_386_TLS_GD_PLT reloc],
        gcc_cv_as_ix86_tlsgdplt,,,
	[call    tls_gd@tlsgdplt],
	[if test x$gcc_cv_ld != x \
	 && $gcc_cv_ld -o conftest conftest.o -G > /dev/null 2>&1; then
	   gcc_cv_as_ix86_tlsgdplt=yes
	 fi
	 rm -f conftest],
      [AC_DEFINE(HAVE_AS_IX86_TLSGDPLT, 1,
        [Define if your assembler and linker support @tlsgdplt.])])

    conftest_s='
	.section .tdata,"aw'$tls_section_flag'",@progbits
tls_ld:
	.section .text,"ax",@progbits
	 call    tls_ld@tlsldmplt'
    gcc_GAS_CHECK_FEATURE([R_386_TLS_LDM_PLT reloc],
        gcc_cv_as_ix86_tlsldmplt,,,
	[$conftest_s],
	[if test x$gcc_cv_ld != x \
	 && $gcc_cv_ld -o conftest conftest.o -G > /dev/null 2>&1; then
	   gcc_cv_as_ix86_tlsldmplt=yes
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_TLSLDMPLT,
      [`if test $gcc_cv_as_ix86_tlsldmplt = yes; then echo 1; else echo 0; fi`],
      [Define to 1 if your assembler and linker support @tlsldmplt.])

    # Enforce 32-bit output with gas and gld.
    if test x$gas = xyes; then
      as_ix86_tls_ldm_opt="--32"
    fi
    if echo "$ld_ver" | grep GNU > /dev/null; then
      if $gcc_cv_ld -V 2>/dev/null | grep elf_i386_sol2 > /dev/null; then
        ld_ix86_tls_ldm_opt="-melf_i386_sol2"
      else
        ld_ix86_tls_ldm_opt="-melf_i386"
      fi
    fi
    conftest_s='
	.section .text,"ax",@progbits
        .globl  _start
        .type   _start, @function
_start:      
	leal	value@tlsldm(%ebx), %eax
	call	___tls_get_addr@plt

        .section .tdata,"aw'$tls_section_flag'",@progbits
        .type	value, @object
value:'
    gcc_GAS_CHECK_FEATURE([R_386_TLS_LDM reloc],
        gcc_cv_as_ix86_tlsldm,,
	[$as_ix86_tls_ldm_opt],
	[$conftest_s],
	[if test x$gcc_cv_ld != x && test x$gcc_cv_objdump != x \
	    && $gcc_cv_ld $ld_ix86_tls_ldm_opt -o conftest conftest.o $ld_tls_libs -lc > /dev/null 2>&1; then
	   if $gcc_cv_objdump -d conftest 2>/dev/null | grep nop > /dev/null \
	      || dis conftest 2>/dev/null | grep nop > /dev/null; then
	     gcc_cv_as_ix86_tlsldm=yes
	   fi
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_TLSLDM,
      [`if test $gcc_cv_as_ix86_tlsldm = yes; then echo 1; else echo 0; fi`],
      [Define to 1 if your assembler and linker support @tlsldm.])

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
      *-*-aix*) conftest_s='	.machine "pwr5"
	.csect .text[[PR]]
	mfcr 3,128';;
      *-*-darwin*)
	gcc_GAS_CHECK_FEATURE([.machine directive support],
	  gcc_cv_as_machine_directive,,,
	  [	.machine ppc7400])
	if test x$gcc_cv_as_machine_directive != xyes; then
	  echo "*** This target requires an assembler supporting \".machine\"" >&2
	  echo you can get it from: ftp://gcc.gnu.org/pub/gcc/infrastructure/cctools-528.5.dmg >&2
	  test x$build = x$target && exit 1
	fi
	conftest_s='	.text
	mfcr r3,128';;
      *) conftest_s='	.machine power4
	.text
	mfcr 3,128';;
    esac

    gcc_GAS_CHECK_FEATURE([mfcr field support],
      gcc_cv_as_powerpc_mfcrf, [2,14,0],,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_MFCRF, 1,
	  [Define if your assembler supports mfcr field.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr5"
	.csect .text[[PR]]
	popcntb 3,3';;
      *) conftest_s='	.machine power5
	.text
	popcntb 3,3';;
    esac

    gcc_GAS_CHECK_FEATURE([popcntb support],
      gcc_cv_as_powerpc_popcntb, [2,17,0],,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_POPCNTB, 1,
	  [Define if your assembler supports popcntb field.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr5x"
	.csect .text[[PR]]
	frin 1,1';;
      *) conftest_s='	.machine power5
	.text
	frin 1,1';;
    esac

    gcc_GAS_CHECK_FEATURE([fp round support],
      gcc_cv_as_powerpc_fprnd, [2,17,0],,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_FPRND, 1,
	  [Define if your assembler supports fprnd.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr6"
	.csect .text[[PR]]
	mffgpr 1,3';;
      *) conftest_s='	.machine power6
	.text
	mffgpr 1,3';;
    esac

    gcc_GAS_CHECK_FEATURE([move fp gpr support],
      gcc_cv_as_powerpc_mfpgpr, [2,19,2],,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_MFPGPR, 1,
	  [Define if your assembler supports mffgpr and mftgpr.])])

    case $target in
      *-*-aix*) conftest_s='	.csect .text[[PR]]
LCF..0:
	addis 11,30,_GLOBAL_OFFSET_TABLE_-LCF..0@ha';;
      *-*-darwin*)
	conftest_s='	.text
LCF0:
	addis r11,r30,_GLOBAL_OFFSET_TABLE_-LCF0@ha';;
      *) conftest_s='	.text
.LCF0:
	addis 11,30,_GLOBAL_OFFSET_TABLE_-.LCF0@ha';;
    esac

    gcc_GAS_CHECK_FEATURE([rel16 relocs],
      gcc_cv_as_powerpc_rel16, [2,17,0], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_REL16, 1,
	  [Define if your assembler supports R_PPC_REL16 relocs.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr6"
	.csect .text[[PR]]
	cmpb 3,4,5';;
      *) conftest_s='	.machine power6
	.text
	cmpb 3,4,5';;
    esac

    gcc_GAS_CHECK_FEATURE([compare bytes support],
      gcc_cv_as_powerpc_cmpb, [2,19,2], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_CMPB, 1,
	  [Define if your assembler supports cmpb.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr6"
	.csect .text[[PR]]
	dadd 1,2,3';;
      *) conftest_s='	.machine power6
	.text
	dadd 1,2,3';;
    esac

    gcc_GAS_CHECK_FEATURE([decimal float support],
      gcc_cv_as_powerpc_dfp, [2,19,2], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_DFP, 1,
	  [Define if your assembler supports DFP instructions.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr7"
	.csect .text[[PR]]
	lxvd2x 1,2,3';;
      *) conftest_s='	.machine power7
	.text
	lxvd2x 1,2,3';;
    esac

    gcc_GAS_CHECK_FEATURE([vector-scalar support],
      gcc_cv_as_powerpc_vsx, [2,19,2], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_VSX, 1,
	  [Define if your assembler supports VSX instructions.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr7"
	.csect .text[[PR]]
	popcntd 3,3';;
      *) conftest_s='	.machine power7
	.text
	popcntd 3,3';;
    esac

    gcc_GAS_CHECK_FEATURE([popcntd support],
      gcc_cv_as_powerpc_popcntd, [2,19,2], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_POPCNTD, 1,
	  [Define if your assembler supports POPCNTD instructions.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr8"
	.csect .text[[PR]]';;
      *) conftest_s='	.machine power8
	.text';;
    esac

    gcc_GAS_CHECK_FEATURE([power8 support],
      gcc_cv_as_powerpc_power8, [2,19,2], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_POWER8, 1,
	  [Define if your assembler supports POWER8 instructions.])])

    case $target in
      *-*-aix*) conftest_s='	.csect .text[[PR]]
	lwsync';;
      *) conftest_s='	.text
	lwsync';;
    esac

    gcc_GAS_CHECK_FEATURE([lwsync support],
      gcc_cv_as_powerpc_lwsync, [2,19,2], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_LWSYNC, 1,
	  [Define if your assembler supports LWSYNC instructions.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "476"
	.csect .text[[PR]]
	dci 0';;
      *) conftest_s='	.machine "476"
	.text
	dci 0';;
    esac

    gcc_GAS_CHECK_FEATURE([data cache invalidate support],
      gcc_cv_as_powerpc_dci, [9,99,0], -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_DCI, 1,
	  [Define if your assembler supports the DCI/ICI instructions.])])

    gcc_GAS_CHECK_FEATURE([.gnu_attribute support],
      gcc_cv_as_powerpc_gnu_attribute, [2,18,0],,
      [.gnu_attribute 4,1],,
      [AC_DEFINE(HAVE_AS_GNU_ATTRIBUTE, 1,
	  [Define if your assembler supports .gnu_attribute.])])

    gcc_GAS_CHECK_FEATURE([tls marker support],
      gcc_cv_as_powerpc_tls_markers, [2,20,0],,
      [ bl __tls_get_addr(x@tlsgd)],,
      [AC_DEFINE(HAVE_AS_TLS_MARKERS, 1,
	  [Define if your assembler supports arg info for __tls_get_addr.])])

    case $target in
      *-*-aix*)
	gcc_GAS_CHECK_FEATURE([.ref support],
	  gcc_cv_as_aix_ref, [2,21,0],,
	  [	.csect stuff[[rw]]
	     stuff:
		.long 1
		.extern sym
		.ref sym
	  ],,
	  [AC_DEFINE(HAVE_AS_REF, 1,
	    [Define if your assembler supports .ref])])
	;;
    esac
    ;;

  mips*-*-*)
    gcc_GAS_CHECK_FEATURE([explicit relocation support],
      gcc_cv_as_mips_explicit_relocs, [2,14,0],,
[	lw $4,%gp_rel(foo)($4)],,
      [if test x$target_cpu_default = x
       then target_cpu_default=MASK_EXPLICIT_RELOCS
       else target_cpu_default="($target_cpu_default)|MASK_EXPLICIT_RELOCS"
       fi])
    gcc_GAS_CHECK_FEATURE([-mno-shared support],
      gcc_cv_as_mips_no_shared, [2,16,0], [-mno-shared], [nop],,
      [AC_DEFINE(HAVE_AS_NO_SHARED, 1,
		 [Define if the assembler understands -mno-shared.])])

    gcc_GAS_CHECK_FEATURE([.gnu_attribute support],
      gcc_cv_as_mips_gnu_attribute, [2,18,0],,
      [.gnu_attribute 4,1],,
      [AC_DEFINE(HAVE_AS_GNU_ATTRIBUTE, 1,
	  [Define if your assembler supports .gnu_attribute.])])

    gcc_GAS_CHECK_FEATURE([.micromips support],
      gcc_cv_as_micromips_support,,[--fatal-warnings],
      [.set micromips],,
      [AC_DEFINE(HAVE_GAS_MICROMIPS, 1,
          [Define if your assembler supports the .set micromips directive])])

    gcc_GAS_CHECK_FEATURE([.dtprelword support],
      gcc_cv_as_mips_dtprelword, [2,18,0],,
      [.section .tdata,"awT",@progbits
x:
	.word 2
	.text
	.dtprelword x+0x8000],,
      [AC_DEFINE(HAVE_AS_DTPRELWORD, 1,
	  [Define if your assembler supports .dtprelword.])])

    gcc_GAS_CHECK_FEATURE([DSPR1 mult with four accumulators support],
      gcc_cv_as_mips_dspr1_mult,,,
[	.set	mips32r2
	.set	nodspr2
	.set	dsp
	madd	$ac3,$4,$5
	maddu	$ac3,$4,$5
	msub	$ac3,$4,$5
	msubu	$ac3,$4,$5
	mult	$ac3,$4,$5
	multu	$ac3,$4,$5],,
      [AC_DEFINE(HAVE_AS_DSPR1_MULT, 1,
	  [Define if your assembler supports DSPR1 mult.])])

    AC_MSG_CHECKING(assembler and linker for explicit JALR relocation)
    gcc_cv_as_ld_jalr_reloc=no
    if test $gcc_cv_as_mips_explicit_relocs = yes; then
      if test $in_tree_ld = yes ; then
        if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 20 -o "$gcc_cv_gld_major_version" -gt 2 \
           && test $in_tree_ld_is_elf = yes; then
          gcc_cv_as_ld_jalr_reloc=yes
        fi
      elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x -a x$gcc_cv_objdump != x; then
        echo '	.ent x' > conftest.s
        echo 'x:	ld $2,%got_disp(y)($3)' >> conftest.s
        echo '	ld $25,%call16(y)($28)' >> conftest.s
        echo '	.reloc	1f,R_MIPS_JALR,y' >> conftest.s
        echo '1:	jalr $25' >> conftest.s
        echo '	.reloc	1f,R_MIPS_JALR,x' >> conftest.s
        echo '1:	jalr $25' >> conftest.s
        echo '	.end x' >> conftest.s
        if $gcc_cv_as -o conftest.o conftest.s >/dev/null 2>&AS_MESSAGE_LOG_FD \
           && $gcc_cv_ld -shared -o conftest.so conftest.o >/dev/null 2>&AS_MESSAGE_LOG_FD; then
	  if $gcc_cv_objdump -d conftest.so | grep jalr >/dev/null 2>&1 \
	     && $gcc_cv_objdump -d conftest.so | grep "bal.*<x>" >/dev/null 2>&1; then
            gcc_cv_as_ld_jalr_reloc=yes
	  fi
        fi
        rm -f conftest.*
      fi
    fi
    if test $gcc_cv_as_ld_jalr_reloc = yes; then
      if test x$target_cpu_default = x; then
        target_cpu_default=MASK_RELAX_PIC_CALLS
      else
        target_cpu_default="($target_cpu_default)|MASK_RELAX_PIC_CALLS"
      fi
    fi
    AC_MSG_RESULT($gcc_cv_as_ld_jalr_reloc)

    AC_CACHE_CHECK([linker for .eh_frame personality relaxation],
      [gcc_cv_ld_mips_personality_relaxation],
      [gcc_cv_ld_mips_personality_relaxation=no
       if test $in_tree_ld = yes ; then
	 if test "$gcc_cv_gld_major_version" -eq 2 \
		 -a "$gcc_cv_gld_minor_version" -ge 21 \
		 -o "$gcc_cv_gld_major_version" -gt 2; then
	   gcc_cv_ld_mips_personality_relaxation=yes
	 fi
       elif test x$gcc_cv_as != x \
       	    	 -a x$gcc_cv_ld != x \
		 -a x$gcc_cv_readelf != x ; then
	 cat > conftest.s <<EOF
	.cfi_startproc
	.cfi_personality 0x80,indirect_ptr
	.ent test
test:
	nop
	.end test
	.cfi_endproc

	.section .data,"aw",@progbits
indirect_ptr:
	.dc.a personality
EOF
	 if $gcc_cv_as -KPIC -o conftest.o conftest.s > /dev/null 2>&1 \
	    && $gcc_cv_ld -o conftest conftest.o -shared > /dev/null 2>&1; then
	   if $gcc_cv_readelf -d conftest 2>&1 \
	      | grep TEXTREL > /dev/null 2>&1; then
	     :
	   elif $gcc_cv_readelf --relocs conftest 2>&1 \
	        | grep 'R_MIPS_REL32 *$' > /dev/null 2>&1; then
	     :
	   else
	     gcc_cv_ld_mips_personality_relaxation=yes
	   fi
	 fi
       fi
       rm -f conftest.s conftest.o conftest])
    if test x$gcc_cv_ld_mips_personality_relaxation = xyes; then
	    AC_DEFINE(HAVE_LD_PERSONALITY_RELAXATION, 1,
      [Define if your linker can relax absolute .eh_frame personality
pointers into PC-relative form.])
    fi

    gcc_GAS_CHECK_FEATURE([-mnan= support],
      gcc_cv_as_mips_nan,,
      [-mnan=2008],,,
      [AC_DEFINE(HAVE_AS_NAN, 1,
		 [Define if the assembler understands -mnan=.])])
    if test x$gcc_cv_as_mips_nan = xno \
       && test x$with_nan != x; then
      AC_MSG_ERROR(
	[Requesting --with-nan= requires assembler support for -mnan=])
    fi
    ;;
esac

# Mips and HP-UX need the GNU assembler.
# Linux on IA64 might be able to use the Intel assembler.

case "$target" in
  mips*-*-* | *-*-hpux* )
    if test x$gas_flag = xyes \
       || test x"$host" != x"$build" \
       || test ! -x "$gcc_cv_as" \
       || "$gcc_cv_as" -v < /dev/null 2>&1 | grep GNU > /dev/null; then
      :
    else
      echo "*** This configuration requires the GNU assembler" >&2
      exit 1
    fi
    ;;
esac

# ??? Not all targets support dwarf2 debug_line, even within a version
# of gas.  Moreover, we need to emit a valid instruction to trigger any
# info to the output file.  So, as supported targets are added to gas 2.11,
# add some instruction here to (also) show we expect this might work.
# ??? Once 2.11 is released, probably need to add first known working
# version to the per-target configury.
case "$cpu_type" in
  aarch64 | alpha | arm | avr | bfin | cris | i386 | m32c | m68k | microblaze \
  | mips | nios2 | pa | rs6000 | score | sparc | spu | tilegx | tilepro \
  | xstormy16 | xtensa)
    insn="nop"
    ;;
  ia64 | s390)
    insn="nop 0"
    ;;
  mmix)
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
  [if test x$gcc_cv_objdump != x \
   && $gcc_cv_objdump -h conftest.o 2> /dev/null \
      | grep debug_line > /dev/null 2>&1; then
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

 gcc_GAS_CHECK_FEATURE([--debug-prefix-map option],
  gcc_cv_as_debug_prefix_map_flag,
  [2,18,0], [--debug-prefix-map /a=/b], [$insn],,
  [AC_DEFINE(HAVE_AS_DEBUG_PREFIX_MAP, 1,
[Define if your assembler supports the --debug-prefix-map option.])])
fi

gcc_GAS_CHECK_FEATURE([.lcomm with alignment], gcc_cv_as_lcomm_with_alignment,
 ,,
[.lcomm bar,4,16],,
[AC_DEFINE(HAVE_GAS_LCOMM_WITH_ALIGNMENT, 1,
  [Define if your assembler supports .lcomm with an alignment field.])])

if test x$with_sysroot = x && test x$host = x$target \
   && test "$prefix" != "/usr" && test "x$prefix" != "x$local_prefix" \
   && test "$prefix" != "NONE"; then
  AC_DEFINE_UNQUOTED(PREFIX_INCLUDE_DIR, "$prefix/include",
[Define to PREFIX/include if cpp should also search that directory.])
fi

if test x$host != x$target || test "x$TARGET_SYSTEM_ROOT" != x; then
  if test "x$with_headers" != x; then
    target_header_dir=$with_headers
  elif test "x$with_sysroot" = x; then
    target_header_dir="${test_exec_prefix}/${target_noncanonical}/sys-include"
  elif test "x$with_build_sysroot" != "x"; then
    target_header_dir="${with_build_sysroot}${native_system_header_dir}"
  elif test "x$with_sysroot" = xyes; then
    target_header_dir="${test_exec_prefix}/${target_noncanonical}/sys-root${native_system_header_dir}"
  else
    target_header_dir="${with_sysroot}${native_system_header_dir}"
  fi
else
  target_header_dir=${native_system_header_dir}
fi

# Determine the version of glibc, if any, used on the target.
AC_MSG_CHECKING([for target glibc version])
AC_ARG_WITH([glibc-version],
  [AS_HELP_STRING([--with-glibc-version=M.N],
    [assume GCC used with glibc version M.N or later])], [
if [echo "$with_glibc_version" | grep '^[0-9][0-9]*\.[0-9][0-9]*$']; then
  glibc_version_major=`echo "$with_glibc_version" | sed -e 's/\..*//'`
  glibc_version_minor=`echo "$with_glibc_version" | sed -e 's/.*\.//'`
else
  AC_MSG_ERROR([option --with-glibc-version requires a version number M.N])
fi], [
glibc_version_major=0
glibc_version_minor=0
[if test -f $target_header_dir/features.h \
  && glibc_version_major_define=`$EGREP '^[ 	]*#[ 	]*define[ 	]+__GLIBC__[ 	]+[0-9]' $target_header_dir/features.h` \
  && glibc_version_minor_define=`$EGREP '^[ 	]*#[ 	]*define[ 	]+__GLIBC_MINOR__[ 	]+[0-9]' $target_header_dir/features.h`; then
  glibc_version_major=`echo "$glibc_version_major_define" | sed -e 's/.*__GLIBC__[ 	]*//'`
  glibc_version_minor=`echo "$glibc_version_minor_define" | sed -e 's/.*__GLIBC_MINOR__[ 	]*//'`
fi]])
AC_MSG_RESULT([$glibc_version_major.$glibc_version_minor])

AC_ARG_ENABLE(gnu-unique-object,
 [AS_HELP_STRING([--enable-gnu-unique-object],
   [enable the use of the @gnu_unique_object ELF extension on glibc systems])],
 [case $enable_gnu_unique_object in
    yes | no) ;;
    *) AC_MSG_ERROR(['$enable_gnu_unique_object' is an invalid value for --enable-gnu-unique-object.
Valid choices are 'yes' and 'no'.]) ;;
  esac],
 [gcc_GAS_CHECK_FEATURE([gnu_unique_object], gcc_cv_as_gnu_unique_object,
   [elf,2,19,52],,
   [.type foo, '$target_type_format_char'gnu_unique_object],,
# We need to unquote above to to use the definition from config.gcc.
# Also check for ld.so support, i.e. glibc 2.11 or higher.
   [GCC_GLIBC_VERSION_GTE_IFELSE([2], [11], [enable_gnu_unique_object=yes], )]
   )])
if test x$enable_gnu_unique_object = xyes; then
  AC_DEFINE(HAVE_GAS_GNU_UNIQUE_OBJECT, 1,
   [Define if your assembler supports @gnu_unique_object.])
fi

AC_CACHE_CHECK([assembler for tolerance to line number 0],
 [gcc_cv_as_line_zero],
 [gcc_cv_as_line_zero=no
  if test $in_tree_gas = yes; then
    gcc_GAS_VERSION_GTE_IFELSE(2, 16, 91, [gcc_cv_as_line_zero=yes])
  elif test "x$gcc_cv_as" != x; then
    { echo '# 1 "test.s" 1'; echo '# 0 "" 2'; } > conftest.s
    if AC_TRY_COMMAND([$gcc_cv_as -o conftest.o conftest.s >&AS_MESSAGE_LOG_FD 2>conftest.out]) &&
       test "x`cat conftest.out`" = x
    then
      gcc_cv_as_line_zero=yes
    else
      echo "configure: failed program was" >&AS_MESSAGE_LOG_FD
      cat conftest.s >&AS_MESSAGE_LOG_FD
      echo "configure: error output was" >&AS_MESSAGE_LOG_FD
      cat conftest.out >&AS_MESSAGE_LOG_FD
    fi
    rm -f conftest.o conftest.s conftest.out
  fi])
if test "x$gcc_cv_as_line_zero" = xyes; then
  AC_DEFINE([HAVE_AS_LINE_ZERO], 1,
[Define if the assembler won't complain about a line such as # 0 "" 2.])
fi

AC_MSG_CHECKING(linker PT_GNU_EH_FRAME support)
gcc_cv_ld_eh_frame_hdr=no
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 12 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_eh_frame_hdr=yes
  fi
elif test x$gcc_cv_ld != x; then
  if echo "$ld_ver" | grep GNU > /dev/null; then
    # Check if linker supports --eh-frame-hdr option
    if $gcc_cv_ld --help 2>/dev/null | grep eh-frame-hdr > /dev/null; then
      gcc_cv_ld_eh_frame_hdr=yes
    fi
  else
    case "$target" in
      *-*-solaris2*)
        # Sun ld has various bugs in .eh_frame_hdr support before version 1.2251.
        if test "$ld_vers_major" -gt 1 || test "$ld_vers_minor" -ge 2251; then
          gcc_cv_ld_eh_frame_hdr=yes
        fi
        ;;
    esac
  fi
fi
GCC_TARGET_TEMPLATE([HAVE_LD_EH_FRAME_HDR])
if test x"$gcc_cv_ld_eh_frame_hdr" = xyes; then
	AC_DEFINE(HAVE_LD_EH_FRAME_HDR, 1,
[Define if your linker supports .eh_frame_hdr.])
fi
AC_MSG_RESULT($gcc_cv_ld_eh_frame_hdr)

AC_MSG_CHECKING(linker CIEv3 in .eh_frame support)
gcc_cv_ld_eh_frame_ciev3=no
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 16 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_eh_frame_ciev3=yes
  fi
elif test x$gcc_cv_ld != x; then
  if echo "$ld_ver" | grep GNU > /dev/null; then
    gcc_cv_ld_eh_frame_ciev3=yes
    if test 0"$ld_date" -lt 20040513; then
      if test -n "$ld_date"; then
	# If there was date string, but was earlier than 2004-05-13, fail
	gcc_cv_ld_eh_frame_ciev3=no
      elif test "$ld_vers_major" -lt 2; then
	gcc_cv_ld_eh_frame_ciev3=no
      elif test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -lt 16; then
	gcc_cv_ld_eh_frame_ciev3=no
      fi
    fi
  else
    case "$target" in
      *-*-solaris2*)
        # Sun ld added support for CIE v3 in .eh_frame in Solaris 11.1.
        if test "$ld_vers_major" -gt 1 || test "$ld_vers_minor" -ge 2324; then
          gcc_cv_ld_eh_frame_ciev3=yes
        fi
        ;;
    esac
  fi
fi
AC_DEFINE_UNQUOTED(HAVE_LD_EH_FRAME_CIEV3,
  [`if test x"$gcc_cv_ld_eh_frame_ciev3" = xyes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your linker supports CIE v3 in .eh_frame.])
AC_MSG_RESULT($gcc_cv_ld_eh_frame_ciev3)

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

AC_MSG_CHECKING(linker EH-compatible garbage collection of sections)
gcc_cv_ld_eh_gc_sections=no
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 17 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_eh_gc_sections=yes
  fi
elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x -a x$gcc_cv_objdump != x ; then
  cat > conftest.s <<EOF
	.section	.text
.globl _start
        .type _start, @function
_start:
	.long foo
	.size _start, .-_start
	.section	.text.foo,"ax",@progbits
	.type foo, @function
foo:
	.long 0
	.size foo, .-foo
	.section	.gcc_except_table.foo,"a",@progbits
.L0:
	.long 0
	.section	.eh_frame,"a",@progbits
	.long .L0
EOF
  if $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1; then
    if $gcc_cv_ld -o conftest conftest.o --entry=_start --gc-sections 2>&1 \
	 | grep "gc-sections option ignored" > /dev/null; then
      gcc_cv_ld_eh_gc_sections=no
    elif $gcc_cv_objdump -h conftest 2> /dev/null \
         | grep gcc_except_table > /dev/null; then
      gcc_cv_ld_eh_gc_sections=yes
      # If no COMDAT groups, the compiler will emit .gnu.linkonce.t. sections.
      if test x$gcc_cv_as_comdat_group != xyes; then
	gcc_cv_ld_eh_gc_sections=no
	cat > conftest.s <<EOF
	.section	.text
.globl _start
        .type _start, @function
_start:
	.long foo
	.size _start, .-_start
	.section	.gnu.linkonce.t.foo,"ax",@progbits
	.type foo, @function
foo:
	.long 0
	.size foo, .-foo
	.section	.gcc_except_table.foo,"a",@progbits
.L0:
	.long 0
	.section	.eh_frame,"a",@progbits
	.long .L0
EOF
	if $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1; then
	  if $gcc_cv_ld -o conftest conftest.o --entry=_start --gc-sections 2>&1 \
	       | grep "gc-sections option ignored" > /dev/null; then
	    gcc_cv_ld_eh_gc_sections=no
	  elif $gcc_cv_objdump -h conftest 2> /dev/null \
	       | grep gcc_except_table > /dev/null; then
	    gcc_cv_ld_eh_gc_sections=yes
	  fi
	fi
      fi
    fi
  fi
  rm -f conftest.s conftest.o conftest
fi
case "$target" in
  hppa*-*-linux*)
    # ??? This apparently exposes a binutils bug with PC-relative relocations.
    gcc_cv_ld_eh_gc_sections=no
    ;;
esac
if test x$gcc_cv_ld_eh_gc_sections = xyes; then
	AC_DEFINE(HAVE_LD_EH_GC_SECTIONS, 1,
  [Define if your linker supports garbage collection of
   sections in presence of EH frames.])
fi
AC_MSG_RESULT($gcc_cv_ld_eh_gc_sections)

AC_MSG_CHECKING(linker EH garbage collection of sections bug)
gcc_cv_ld_eh_gc_sections_bug=no
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -lt 19 -o "$gcc_cv_gld_major_version" -lt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_eh_gc_sections_bug=yes
  fi
elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x -a x$gcc_cv_objdump != x -a x$gcc_cv_as_comdat_group = xyes; then
  gcc_cv_ld_eh_gc_sections_bug=yes
  cat > conftest.s <<EOF
	.section	.text
.globl _start
	.type _start, @function
_start:
	.long foo
	.size _start, .-_start
	.section	.text.startup.foo,"ax",@progbits
	.type foo, @function
foo:
	.long 0
	.size foo, .-foo
	.section	.gcc_except_table.foo,"a",@progbits
.L0:
	.long 0
	.section	.eh_frame,"a",@progbits
	.long .L0
EOF
  if $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1; then
    if $gcc_cv_ld -o conftest conftest.o --entry=_start --gc-sections 2>&1 \
	 | grep "gc-sections option ignored" > /dev/null; then
      :
    elif $gcc_cv_objdump -h conftest 2> /dev/null \
	 | grep gcc_except_table > /dev/null; then
      gcc_cv_ld_eh_gc_sections_bug=no
    fi
  fi
  rm -f conftest.s conftest.o conftest
fi
if test x$gcc_cv_ld_eh_gc_sections_bug = xyes; then
	AC_DEFINE(HAVE_LD_EH_GC_SECTIONS_BUG, 1,
  [Define if your linker has buggy garbage collection of
   sections support when .text.startup.foo like sections are used.])
fi
AC_MSG_RESULT($gcc_cv_ld_eh_gc_sections_bug)

# --------
# UNSORTED
# --------

AC_CACHE_CHECK(linker --as-needed support,
gcc_cv_ld_as_needed,
[gcc_cv_ld_as_needed=no
gcc_cv_ld_as_needed_option='--as-needed'
gcc_cv_ld_no_as_needed_option='--no-as-needed'
if test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 16 -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_as_needed=yes
  fi
elif test x$gcc_cv_ld != x; then
	# Check if linker supports --as-needed and --no-as-needed options
	if $gcc_cv_ld --help 2>/dev/null | grep as-needed > /dev/null; then
		gcc_cv_ld_as_needed=yes
	else
	  case "$target" in
	    # Solaris 2 ld always supports -z ignore/-z record.
	    *-*-solaris2*)
	      gcc_cv_ld_as_needed=yes
	      gcc_cv_ld_as_needed_option="-z ignore"
	      gcc_cv_ld_no_as_needed_option="-z record"
	      ;;
	  esac
	fi
fi
# --as-needed/-z ignore can only be used if libgcc_s.so.1 uses
# dl_iterate_phdr, i.e. since Solaris 11.
case "$target" in
  *-*-solaris2.1[[1-9]]*)
    case "$target" in
    i?86-*-* | x86_64-*-*)
      if echo "$ld_ver" | grep GNU > /dev/null; then
        # Doesn't work with gld on Solaris/x86 due to PR ld/12320.
        gcc_cv_ld_as_needed=no
      fi
      ;;
    esac
    ;;
  *-*-solaris2*)
    gcc_cv_ld_as_needed=no
    ;;
esac
])
if test x"$gcc_cv_ld_as_needed" = xyes; then
	AC_DEFINE(HAVE_LD_AS_NEEDED, 1,
[Define if your linker supports --as-needed/--no-as-needed or equivalent options.])
	AC_DEFINE_UNQUOTED(LD_AS_NEEDED_OPTION, "$gcc_cv_ld_as_needed_option",
[Define to the linker option to ignore unused dependencies.])
	AC_DEFINE_UNQUOTED(LD_NO_AS_NEEDED_OPTION, "$gcc_cv_ld_no_as_needed_option",
[Define to the linker option to keep unused dependencies.])
fi

case "$target:$tm_file" in
  powerpc64-*-freebsd* | powerpc64*-*-linux* | powerpc*-*-linux*rs6000/biarch64.h*)
  case "$target" in
     *le-*-linux*)
     emul_name="-melf64lppc"
      ;;
     *-*-linux*)
     emul_name="-melf64ppc"
      ;;
     *-*-freebsd*)
     emul_name="-melf64ppc_fbsd"
      ;;
  esac
    AC_CACHE_CHECK(linker support for omitting dot symbols,
    gcc_cv_ld_no_dot_syms,
    [gcc_cv_ld_no_dot_syms=no
    if test x"$ld_is_gold" = xyes; then
      gcc_cv_ld_no_dot_syms=yes
    elif test $in_tree_ld = yes ; then
      if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 16 -o "$gcc_cv_gld_major_version" -gt 2; then
        gcc_cv_ld_no_dot_syms=yes
      fi
    elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x ; then
      cat > conftest1.s <<EOF
	.text
	bl .foo
EOF
      cat > conftest2.s <<EOF
	.section ".opd","aw"
	.align 3
	.globl foo
	.type foo,@function
foo:
	.quad .LEfoo,.TOC.@tocbase,0
	.text
.LEfoo:
	blr
	.size foo,.-.LEfoo
EOF
      if $gcc_cv_as -a64 -o conftest1.o conftest1.s > /dev/null 2>&1 \
         && $gcc_cv_as -a64 -o conftest2.o conftest2.s > /dev/null 2>&1 \
         && $gcc_cv_ld $emul_name -o conftest conftest1.o conftest2.o > /dev/null 2>&1; then
        gcc_cv_ld_no_dot_syms=yes
      fi
      rm -f conftest conftest1.o conftest2.o conftest1.s conftest2.s
    fi
    ])
    if test x"$gcc_cv_ld_no_dot_syms" = xyes; then
      AC_DEFINE(HAVE_LD_NO_DOT_SYMS, 1,
    [Define if your PowerPC64 linker only needs function descriptor syms.])
    fi

    AC_CACHE_CHECK(linker large toc support,
    gcc_cv_ld_large_toc,
    [gcc_cv_ld_large_toc=no
    if test x"$ld_is_gold" = xyes; then
      gcc_cv_ld_large_toc=yes
    elif test $in_tree_ld = yes ; then
      if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 21 -o "$gcc_cv_gld_major_version" -gt 2; then
        gcc_cv_ld_large_toc=yes
      fi
    elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x ; then
      cat > conftest.s <<EOF
	.section ".tbss","awT",@nobits
	.align 3
ie0:	.space 8
	.global _start
	.text
_start:
	addis 9,13,ie0@got@tprel@ha
	ld 9,ie0@got@tprel@l(9)
EOF
      if $gcc_cv_as -a64 -o conftest.o conftest.s > /dev/null 2>&1 \
         && $gcc_cv_ld $emul_name --no-toc-sort -o conftest conftest.o > /dev/null 2>&1; then
        gcc_cv_ld_large_toc=yes
      fi
      rm -f conftest conftest.o conftest.s
    fi
    ])
    if test x"$gcc_cv_ld_large_toc" = xyes; then
      AC_DEFINE(HAVE_LD_LARGE_TOC, 1,
    [Define if your PowerPC64 linker supports a large TOC.])
    fi
    ;;
esac

case "$target" in
  *-*-aix*)
    AC_CACHE_CHECK(linker large toc support,
    gcc_cv_ld_large_toc,
    [gcc_cv_ld_large_toc=no
    if test x$gcc_cv_as != x ; then
      cat > conftest.s <<EOF
	.toc
LC..1:
	.tc a[[TC]],a[[RW]]
	.extern a[[RW]]
	.csect .text[[PR]]
.largetoctest:
	addis 9,LC..1@u(2)
	ld 3,LC..1@l(9)
EOF
      if $gcc_cv_as -a64 -o conftest.o conftest.s > /dev/null 2>&1; then
        gcc_cv_ld_large_toc=yes
      fi
      rm -f conftest conftest.o conftest.s
    fi
    ])
    if test x"$gcc_cv_ld_large_toc" = xyes; then
      AC_DEFINE(HAVE_LD_LARGE_TOC, 1,
    [Define if your PowerPC64 linker supports a large TOC.])
    fi
    ;;
esac

AC_CACHE_CHECK(linker --build-id support,
  gcc_cv_ld_buildid,
  [gcc_cv_ld_buildid=no
  if test $in_tree_ld = yes ; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a \
       "$gcc_cv_gld_minor_version" -ge 18 -o \
       "$gcc_cv_gld_major_version" -gt 2 \
       && test $in_tree_ld_is_elf = yes; then
      gcc_cv_ld_buildid=yes
    fi
  elif test x$gcc_cv_ld != x; then
    if $gcc_cv_ld --help 2>/dev/null | grep build-id > /dev/null; then
      gcc_cv_ld_buildid=yes
    fi
  fi])
if test x"$gcc_cv_ld_buildid" = xyes; then
  AC_DEFINE(HAVE_LD_BUILDID, 1,
  [Define if your linker supports --build-id.])
fi

AC_ARG_ENABLE(linker-build-id,
[AS_HELP_STRING([--enable-linker-build-id],
                [compiler will always pass --build-id to linker])],
[],
enable_linker_build_id=no)

if test x"$enable_linker_build_id" = xyes; then
  if test x"$gcc_cv_ld_buildid" = xyes; then
    AC_DEFINE(ENABLE_LD_BUILDID, 1,
    [Define if gcc should always pass --build-id to linker.])
  else
    AC_MSG_WARN(--build-id is not supported by your linker; --enable-linker-build-id ignored)
  fi
fi

# In binutils 2.21, GNU ld gained support for new emulations fully
# supporting the Solaris 2 ABI.  Detect their presence in the linker used.
AC_CACHE_CHECK(linker *_sol2 emulation support,
  gcc_cv_ld_sol2_emulation,
  [gcc_cv_ld_sol2_emulation=no
  if test $in_tree_ld = yes ; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a \
       "$gcc_cv_gld_minor_version" -ge 21 -o \
       "$gcc_cv_gld_major_version" -gt 2 \
       && test $in_tree_ld_is_elf = yes; then
      gcc_cv_ld_sol2_emulation=yes
    fi
  elif test x$gcc_cv_ld != x; then
    if $gcc_cv_ld -V 2>/dev/null | sed -e '1,/Supported emulations/d;q' | \
       grep _sol2 > /dev/null; then
      gcc_cv_ld_sol2_emulation=yes
    fi
  fi])
if test x"$gcc_cv_ld_sol2_emulation" = xyes; then
  AC_DEFINE(HAVE_LD_SOL2_EMULATION, 1,
  [Define if your linker supports the *_sol2 emulations.])
fi

AC_CACHE_CHECK(linker --sysroot support,
  gcc_cv_ld_sysroot,
  [gcc_cv_ld_sysroot=no
  if test $in_tree_ld = yes ; then
      if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 16 -o "$gcc_cv_gld_major_version" -gt 2 ; then
        gcc_cv_ld_sysroot=yes
      fi
  elif test x$gcc_cv_ld != x; then 
    if $gcc_cv_ld --help 2>/dev/null | grep sysroot > /dev/null; then
      gcc_cv_ld_sysroot=yes
    fi
  fi])
if test x"$gcc_cv_ld_sysroot" = xyes; then
  AC_DEFINE(HAVE_LD_SYSROOT, 1,
  [Define if your linker supports --sysroot.])
fi	  

# Test for stack protector support in target C library.
AC_CACHE_CHECK(__stack_chk_fail in target C library,
      gcc_cv_libc_provides_ssp,
      [gcc_cv_libc_provides_ssp=no
    case "$target" in
       *-*-linux* | *-*-kfreebsd*-gnu | *-*-knetbsd*-gnu)
      # glibc 2.4 and later provides __stack_chk_fail and
      # either __stack_chk_guard, or TLS access to stack guard canary.
      GCC_GLIBC_VERSION_GTE_IFELSE([2], [4], [gcc_cv_libc_provides_ssp=yes], [
      [if test -f $target_header_dir/features.h \
	 && $EGREP '^[ 	]*#[ 	]*define[ 	]+__GNU_LIBRARY__[ 	]+([1-9][0-9]|[6-9])' \
	    $target_header_dir/features.h > /dev/null; then
	if $EGREP '^[ 	]*#[ 	]*define[ 	]+__UCLIBC__[ 	]+1' \
	     $target_header_dir/features.h > /dev/null && \
	     test -f $target_header_dir/bits/uClibc_config.h && \
	     $EGREP '^[ 	]*#[ 	]*define[ 	]+__UCLIBC_HAS_SSP__[ 	]+1' \
	     $target_header_dir/bits/uClibc_config.h > /dev/null; then
	  gcc_cv_libc_provides_ssp=yes
	fi
      # all versions of Bionic support stack protector
      elif test -f $target_header_dir/sys/cdefs.h \
        && $EGREP '^[  ]*#[    ]*define[       ]+__BIONIC__[   ]+1' \
           $target_header_dir/sys/cdefs.h > /dev/null; then
         gcc_cv_libc_provides_ssp=yes
      fi]])
	;;
       *-*-gnu*)
	 # Avoid complicated tests (see
	 # <http://gcc.gnu.org/ml/gcc/2008-10/msg00130.html>) and for now
	 # simply assert that glibc does provide this, which is true for all
	 # realistically usable GNU/Hurd configurations.
	 gcc_cv_libc_provides_ssp=yes;;
       *-*-darwin* | *-*-freebsd*)
	 AC_CHECK_FUNC(__stack_chk_fail,[gcc_cv_libc_provides_ssp=yes],
           [echo "no __stack_chk_fail on this target"])
        ;;
  *) gcc_cv_libc_provides_ssp=no ;;
    esac])

if test x$gcc_cv_libc_provides_ssp = xyes; then
  AC_DEFINE(TARGET_LIBC_PROVIDES_SSP, 1,
	    [Define if your target C library provides stack protector support])
fi

# Test for <sys/sdt.h> on the target.
GCC_TARGET_TEMPLATE([HAVE_SYS_SDT_H])
AC_MSG_CHECKING(sys/sdt.h in the target C library)
have_sys_sdt_h=no
if test -f $target_header_dir/sys/sdt.h; then
  have_sys_sdt_h=yes
  AC_DEFINE(HAVE_SYS_SDT_H, 1,
            [Define if your target C library provides sys/sdt.h])
fi
AC_MSG_RESULT($have_sys_sdt_h)

# Check if TFmode long double should be used by default or not.
# Some glibc targets used DFmode long double, but with glibc 2.4
# and later they can use TFmode.
case "$target" in
  powerpc*-*-linux* | \
  sparc*-*-linux* | \
  s390*-*-linux* | \
  alpha*-*-linux*)
    AC_ARG_WITH(long-double-128,
      [AS_HELP_STRING([--with-long-double-128],
		      [use 128-bit long double by default])],
      gcc_cv_target_ldbl128="$with_long_double_128",
      [GCC_GLIBC_VERSION_GTE_IFELSE([2], [4], [gcc_cv_target_ldbl128=yes], [
      [gcc_cv_target_ldbl128=no
      grep '^[ 	]*#[ 	]*define[ 	][ 	]*__LONG_DOUBLE_MATH_OPTIONAL' \
        $target_header_dir/bits/wordsize.h > /dev/null 2>&1 \
      && gcc_cv_target_ldbl128=yes
      ]])])
    ;;
esac
if test x$gcc_cv_target_ldbl128 = xyes; then
  AC_DEFINE(TARGET_DEFAULT_LONG_DOUBLE_128, 1,
	    [Define if TFmode long double should be the default])
fi

AC_MSG_CHECKING(dl_iterate_phdr in target C library)
gcc_cv_target_dl_iterate_phdr=unknown
case "$target" in
  # Restrict to Solaris 11+.  While most of the Solaris 11 linker changes
  # were backported to Solaris 10 Update 10, dl_iterate_phdr only lives in
  # libdl there, both complicating its use and breaking compatibility
  # between Solaris 10 updates.
  *-*-solaris2.1[[1-9]]*)
    # <link.h> needs both a dl_iterate_phdr declaration and support for
    # compilation with largefile support.
    if grep dl_iterate_phdr $target_header_dir/link.h > /dev/null 2>&1 \
      && grep 'large file capable' $target_header_dir/link.h > /dev/null 2>&1; then
      gcc_cv_target_dl_iterate_phdr=yes
    else
      gcc_cv_target_dl_iterate_phdr=no
    fi
    ;;
esac
GCC_TARGET_TEMPLATE([TARGET_DL_ITERATE_PHDR])
if test x$gcc_cv_target_dl_iterate_phdr = xyes; then
   AC_DEFINE(TARGET_DL_ITERATE_PHDR, 1,
[Define if your target C library provides the `dl_iterate_phdr' function.])
fi
AC_MSG_RESULT($gcc_cv_target_dl_iterate_phdr)

# We no longer support different GC mechanisms.  Emit an error if
# the user configures with --with-gc.
AC_ARG_WITH(gc,
[AS_HELP_STRING([--with-gc={page,zone}],
		[this option is not supported anymore.  It used to choose
		 the garbage collection mechanism to use with the compiler])],
[AC_MSG_ERROR([Configure option --with-gc is only supported up to GCC 4.7.x])],
[])

# Libraries to use on the host.  This will normally be set by the top
# level Makefile.  Here we simply capture the value for our Makefile.
if test -z "${HOST_LIBS+set}"; then
  HOST_LIBS=
fi
AC_SUBST(HOST_LIBS)

# Use the system's zlib library.
zlibdir=-L../zlib
zlibinc="-I\$(srcdir)/../zlib"
AC_ARG_WITH(system-zlib,
[AS_HELP_STRING([--with-system-zlib], [use installed libz])],
zlibdir=
zlibinc=
)
AC_SUBST(zlibdir)
AC_SUBST(zlibinc)

dnl Very limited version of automake's enable-maintainer-mode

AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[AS_HELP_STRING([--enable-maintainer-mode],
                [enable make rules and dependencies not useful
                 (and sometimes confusing) to the casual installer])],
      maintainer_mode=$enableval,
      maintainer_mode=no)

AC_MSG_RESULT($maintainer_mode)

if test "$maintainer_mode" = "yes"; then
  MAINT=''
else
  MAINT='#'
fi
AC_SUBST(MAINT)dnl

dnl Whether to prevent multiple front-ends from linking at the same time

AC_MSG_CHECKING([whether to avoid linking multiple front-ends at once])
  AC_ARG_ENABLE(link-mutex,
[AS_HELP_STRING([--enable-link-mutex],
		[avoid linking multiple front-ends at once to avoid thrashing
		 on the build machine])],
      do_link_mutex=$enableval,
      do_link_mutex=no)
AC_MSG_RESULT($do_link_mutex)

if test "$do_link_mutex" = "yes"; then
   DO_LINK_MUTEX=true
else
   DO_LINK_MUTEX=false
fi
AC_SUBST(DO_LINK_MUTEX)

# --------------
# Language hooks
# --------------

# Make empty files to contain the specs and options for each language.
# Then add #include lines to for a compiler that has specs and/or options.

subdirs=
lang_opt_files=
lang_specs_files=
lang_tree_files=
# These (without "all_") are set in each config-lang.in.
# `language' must be a single word so is spelled singularly.
all_languages=
all_compilers=
all_outputs='Makefile'
# List of language makefile fragments.
all_lang_makefrags=
# Additional files for gengtype
all_gtfiles="$target_gtfiles"

# These are the languages that are set in --enable-languages,
# and are available in the GCC tree.
all_selected_languages=

# Add the language fragments.
# Languages are added via two mechanisms.  Some information must be
# recorded in makefile variables, these are defined in config-lang.in.
# We accumulate them and plug them into the main Makefile.
# The other mechanism is a set of hooks for each of the main targets
# like `clean', `install', etc.

language_hooks="Make-hooks"

for lang in ${srcdir}/*/config-lang.in
do
changequote(,)dnl
	test "$lang" = "${srcdir}/*/config-lang.in" && continue

        lang_alias=`sed -n -e 's,^language=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^language=\([^   ]*\).*$,\1,p' $lang`
        if test "x$lang_alias" = x
        then
              echo "$lang doesn't set \$language." 1>&2
              exit 1
        fi
        subdir="`echo $lang | sed -e 's,^.*/\([^/]*\)/config-lang.in$,\1,'`"
        subdirs="$subdirs $subdir"

	# $gcc_subdir is where the gcc integration files are to be found
	# for a language, both for internal compiler purposes (compiler
	# sources implementing front-end to GCC tree converters), and for
	# build infrastructure purposes (Make-lang.in, etc.)
	#
	# This will be <subdir> (relative to $srcdir) if a line like 
	# gcc_subdir="<subdir>" or gcc_subdir=<subdir>
	# is found in <langdir>/config-lang.in, and will remain <langdir>
	# otherwise.
	#
	# Except for the language alias (fetched above), the regular
	# "config-lang.in" contents are always retrieved from $gcc_subdir,
	# so a <langdir>/config-lang.in setting gcc_subdir typically sets
	# only this and the language alias.

        gcc_subdir=`sed -n -e 's,^gcc_subdir=['"'"'"'"]\(.*\)["'"'"'"'].*$,\1,p' -e 's,^gcc_subdir=\([^   ]*\).*$,\1,p' $lang`
        if [ "$gcc_subdir" = "" ]; then
           gcc_subdir="$subdir"
        fi

        case ",$enable_languages," in
        *,$lang_alias,*)
            all_selected_languages="$all_selected_languages $lang_alias"
            if test -f $srcdir/$gcc_subdir/lang-specs.h; then
                lang_specs_files="$lang_specs_files $srcdir/$gcc_subdir/lang-specs.h"
	    fi
	    ;;
        esac
changequote([,])dnl

	language=
	boot_language=
	compilers=
	outputs=
	gtfiles=
	subdir_requires=
	. ${srcdir}/$gcc_subdir/config-lang.in
	if test "x$language" = x
	then
		echo "${srcdir}/$gcc_subdir/config-lang.in doesn't set \$language." 1>&2
		exit 1
	fi

	ok=:
        case ",$enable_languages," in
        	*,$lang_alias,*) ;;
		*)
			for i in $subdir_requires; do
				test -f "${srcdir}/$i/config-lang.in" && continue
				ok=false
				break
			done
		;;
	esac
	$ok || continue

	all_lang_makefrags="$all_lang_makefrags \$(srcdir)/$gcc_subdir/Make-lang.in"
	if test -f $srcdir/$gcc_subdir/lang.opt; then
	    lang_opt_files="$lang_opt_files $srcdir/$gcc_subdir/lang.opt"
	    all_opt_files="$all_opt_files $srcdir/$gcc_subdir/lang.opt"
	fi
	if test -f $srcdir/$gcc_subdir/$subdir-tree.def; then
	    lang_tree_files="$lang_tree_files $srcdir/$gcc_subdir/$subdir-tree.def"
	fi
	all_languages="$all_languages $language"
	all_compilers="$all_compilers $compilers"
	all_outputs="$all_outputs $outputs"
	all_gtfiles="$all_gtfiles [[$subdir]] $gtfiles"
        case ",$enable_languages," in
        	*,lto,*)
		    AC_DEFINE(ENABLE_LTO, 1, [Define to enable LTO support.])
		    enable_lto=yes
		    AC_SUBST(enable_lto)
		    ;;
		*) ;;
	esac
done

check_languages=
for language in $all_selected_languages
do
	check_languages="$check_languages check-$language"
done

# We link each language in with a set of hooks, reached indirectly via
# lang.${target}.  Only do so for selected languages.

rm -f Make-hooks
touch Make-hooks
target_list="all.cross start.encap rest.encap tags \
	install-common install-man install-info install-pdf install-html dvi \
	pdf html uninstall info man srcextra srcman srcinfo \
	mostlyclean clean distclean maintainer-clean install-plugin"

for t in $target_list
do
	x=
	for lang in $all_selected_languages
	do
		x="$x $lang.$t"
	done
	echo "lang.$t: $x" >> Make-hooks
done

# --------
# Option include files
# --------

${AWK} -f $srcdir/opt-include.awk $all_opt_files > option-includes.mk
option_includes="option-includes.mk"
AC_SUBST_FILE(option_includes)

# --------
# UNSORTED
# --------

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
echo "python import sys; sys.path.append('${srcdir}'); import gdbhooks" >> .gdbinit

# Put a breakpoint on __asan_report_error to help with debugging buffer
# overflow.
case "$CFLAGS" in
*-fsanitize=address*)
  echo "source ${srcdir}/gdbasan.in" >> .gdbinit
  ;;
esac

gcc_tooldir='$(libsubdir)/$(libsubdir_to_prefix)$(target_noncanonical)'
AC_SUBST(gcc_tooldir)
AC_SUBST(dollar)

# Find a directory in which to install a shared libgcc.

AC_ARG_ENABLE(version-specific-runtime-libs,
[AS_HELP_STRING([--enable-version-specific-runtime-libs],
                [specify that runtime libraries should be
                 installed in a compiler-specific directory])])

# Substitute configuration variables
AC_SUBST(subdirs)
AC_SUBST(srcdir)
AC_SUBST(all_compilers)
AC_SUBST(all_gtfiles)
AC_SUBST(all_lang_makefrags)
AC_SUBST(all_languages)
AC_SUBST(all_selected_languages)
AC_SUBST(build_exeext)
AC_SUBST(build_install_headers_dir)
AC_SUBST(build_xm_file_list)
AC_SUBST(build_xm_include_list)
AC_SUBST(build_xm_defines)
AC_SUBST(build_file_translate)
AC_SUBST(check_languages)
AC_SUBST(cpp_install_dir)
AC_SUBST(xmake_file)
AC_SUBST(tmake_file)
AC_SUBST(TM_ENDIAN_CONFIG)
AC_SUBST(TM_MULTILIB_CONFIG)
AC_SUBST(TM_MULTILIB_EXCEPTIONS_CONFIG)
AC_SUBST(extra_gcc_objs)
AC_SUBST(user_headers_inc_next_pre)
AC_SUBST(user_headers_inc_next_post)
AC_SUBST(extra_headers_list)
AC_SUBST(extra_objs)
AC_SUBST(extra_programs)
AC_SUBST(float_h_file)
AC_SUBST(gcc_config_arguments)
AC_SUBST(gcc_gxx_include_dir)
AC_SUBST(gcc_gxx_include_dir_add_sysroot)
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
AC_SUBST(common_out_file)
AC_SUBST(common_out_object_file)
AC_SUBST(tm_file_list)
AC_SUBST(tm_include_list)
AC_SUBST(tm_defines)
AC_SUBST(tm_p_file_list)
AC_SUBST(tm_p_include_list)
AC_SUBST(xm_file_list)
AC_SUBST(xm_include_list)
AC_SUBST(xm_defines)
AC_SUBST(use_gcc_stdint)
AC_SUBST(c_target_objs)
AC_SUBST(cxx_target_objs)
AC_SUBST(fortran_target_objs)
AC_SUBST(target_cpu_default)

AC_SUBST_FILE(language_hooks)

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

AC_ARG_VAR(GMPLIBS,[How to link GMP])
AC_ARG_VAR(GMPINC,[How to find GMP include files])

AC_ARG_VAR(ISLLIBS,[How to link ISL])
AC_ARG_VAR(ISLINC,[How to find ISL include files])

AC_ARG_VAR(CLOOGLIBS,[How to link CLOOG])
AC_ARG_VAR(CLOOGINC,[How to find CLOOG include files])
if test "x${CLOOGLIBS}" != "x" ; then 
   AC_DEFINE(HAVE_cloog, 1, [Define if cloog is in use.])
fi

# Check for plugin support
AC_ARG_ENABLE(plugin,
[AS_HELP_STRING([--enable-plugin], [enable plugin support])],
enable_plugin=$enableval,
enable_plugin=yes; default_plugin=yes)

pluginlibs=

case "${host}" in
  *-*-darwin*)
    if test x$build = x$host; then
      export_sym_check="nm${exeext} -g"
    elif test x$host = x$target; then
      export_sym_check="$gcc_cv_nm -g"
    else
      export_sym_check=
    fi
  ;;
  *)
    if test x$build = x$host; then
      export_sym_check="objdump${exeext} -T"
    elif test x$host = x$target; then
      export_sym_check="$gcc_cv_objdump -T"
    else
      export_sym_check=
    fi
  ;;
esac

if test x"$enable_plugin" = x"yes"; then

  AC_MSG_CHECKING([for exported symbols])
  if test "x$export_sym_check" != x; then
    echo "int main() {return 0;} int foobar() {return 0;}" > conftest.c
    ${CC} ${CFLAGS} ${LDFLAGS} conftest.c -o conftest$ac_exeext > /dev/null 2>&1
    if $export_sym_check conftest$ac_exeext | grep -q foobar > /dev/null; then
      : # No need to use a flag
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([yes])
      AC_MSG_CHECKING([for -rdynamic])
      ${CC} ${CFLAGS} ${LDFLAGS} -rdynamic conftest.c -o conftest$ac_exeext > /dev/null 2>&1
      if $export_sym_check conftest$ac_exeext | grep -q foobar > /dev/null; then
        plugin_rdynamic=yes
        pluginlibs="-rdynamic"
      else
        plugin_rdynamic=no
        enable_plugin=no
      fi
      AC_MSG_RESULT([$plugin_rdynamic])
    fi
  else
    AC_MSG_RESULT([unable to check])
  fi

  # Check -ldl
  saved_LIBS="$LIBS"
  AC_SEARCH_LIBS([dlopen], [dl])
  if test x"$ac_cv_search_dlopen" = x"-ldl"; then
    pluginlibs="$pluginlibs -ldl"
  fi
  LIBS="$saved_LIBS"

  # Check that we can build shared objects with -fPIC -shared
  saved_LDFLAGS="$LDFLAGS"
  saved_CFLAGS="$CFLAGS"
  case "${host}" in
    *-*-darwin*)
      CFLAGS=`echo $CFLAGS | sed s/-mdynamic-no-pic//g`
      CFLAGS="$CFLAGS -fPIC"
      LDFLAGS="$LDFLAGS -shared -undefined dynamic_lookup"
    ;;
    *)
      CFLAGS="$CFLAGS -fPIC"
      LDFLAGS="$LDFLAGS -fPIC -shared"
    ;;
  esac
  AC_MSG_CHECKING([for -fPIC -shared])
  AC_TRY_LINK(
    [extern int X;],[return X == 0;],
    [AC_MSG_RESULT([yes]); have_pic_shared=yes],
    [AC_MSG_RESULT([no]); have_pic_shared=no])
  if test x"$have_pic_shared" != x"yes" -o x"$ac_cv_search_dlopen" = x"no"; then
    pluginlibs=
    enable_plugin=no
  fi
  LDFLAGS="$saved_LDFLAGS"
  CFLAGS="$saved_CFLAGS"

  # If plugin support had been requested but not available, fail.
  if test x"$enable_plugin" = x"no" ; then
    if test x"$default_plugin" != x"yes"; then
      AC_MSG_ERROR([
Building GCC with plugin support requires a host that supports
-fPIC, -shared, -ldl and -rdynamic.])
    fi
  fi
fi

AC_SUBST(pluginlibs)
AC_SUBST(enable_plugin)
if test x"$enable_plugin" = x"yes"; then
  AC_DEFINE(ENABLE_PLUGIN, 1, [Define to enable plugin support.])
fi


# Enable --enable-host-shared
AC_ARG_ENABLE(host-shared,
[AS_HELP_STRING([--enable-host-shared],
		[build host code as shared libraries])],
[PICFLAG=-fPIC], [PICFLAG=])
AC_SUBST(enable_host_shared)
AC_SUBST(PICFLAG)


AC_ARG_ENABLE(libquadmath-support,
[AS_HELP_STRING([--disable-libquadmath-support],
  [disable libquadmath support for Fortran])],
ENABLE_LIBQUADMATH_SUPPORT=$enableval,
ENABLE_LIBQUADMATH_SUPPORT=yes)
if test "${ENABLE_LIBQUADMATH_SUPPORT}" != "no" ; then
  AC_DEFINE(ENABLE_LIBQUADMATH_SUPPORT, 1,
            [Define to 1 to enable libquadmath support])
fi


# Specify what hash style to use by default.
AC_ARG_WITH([linker-hash-style],
[AC_HELP_STRING([--with-linker-hash-style={sysv,gnu,both}],
                [specify the linker hash style])],
[case x"$withval" in
   xsysv)
     LINKER_HASH_STYLE=sysv
     ;;
   xgnu)
     LINKER_HASH_STYLE=gnu
     ;;
   xboth)
     LINKER_HASH_STYLE=both
     ;;
   *)
     AC_MSG_ERROR([$withval is an invalid option to --with-linker-hash-style])
     ;;
 esac],
[LINKER_HASH_STYLE=''])
if test x"${LINKER_HASH_STYLE}" != x; then
  AC_DEFINE_UNQUOTED(LINKER_HASH_STYLE, "$LINKER_HASH_STYLE",
                                         [The linker hash style])
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
for d in $subdirs doc build common c-family
do
    test -d $d || mkdir $d
done
], 
[subdirs='$subdirs'])
AC_OUTPUT

