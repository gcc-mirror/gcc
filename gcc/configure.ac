# configure.ac for GCC
# Process this file with autoconf to generate a configuration script.

# Copyright (C) 1997-2025 Free Software Foundation, Inc.

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

AC_INIT
AC_CONFIG_SRCDIR(tree.cc)
AC_CONFIG_MACRO_DIRS([../config] [..])
AC_CONFIG_HEADER(auto-host.h:config.in)

gcc_version=`cat $srcdir/BASE-VER`

# Determine the host, build, and target systems
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Determine the noncanonical host name, for Ada.
ACX_NONCANONICAL_HOST

# Determine the noncanonical target name, for directory use.
ACX_NONCANONICAL_TARGET

# Used for constructing correct paths for offload compilers.
real_target_noncanonical=${target_noncanonical}
accel_dir_suffix=

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
 /) ;;
 */) with_sysroot=`echo $with_sysroot | sed 's,/$,,'` ;;
 esac
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

# If both --with-sysroot and --with-gxx-include-dir are passed, we interpolate
# the former in the latter and, upon success, compute gcc_gxx_include_dir as
# relative to the sysroot.
gcc_gxx_include_dir_add_sysroot=0

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
elif test "${with_sysroot+set}" = set; then
  gcc_gxx_without_sysroot=`expr "${gcc_gxx_include_dir}" : "${with_sysroot}"'\(.*\)'`
  if test "${gcc_gxx_without_sysroot}"; then
    gcc_gxx_include_dir="${gcc_gxx_without_sysroot}"
    gcc_gxx_include_dir_add_sysroot=1
  fi
fi

# Configuration for an alternate set of C++ headers.
gcc_gxx_libcxx_include_dir=
# Specify the alternate g++ header file directory
AC_ARG_WITH(gxx-libcxx-include-dir,
[AS_HELP_STRING([--with-gxx-libcxx-include-dir=DIR],
                [specifies directory to find libc++ header files])],
[case "${withval}" in
yes)	AC_MSG_ERROR(bad value ${withval} given for libc++ include directory) ;;
*)	gcc_gxx_libcxx_include_dir=$with_gxx_libcxx_include_dir ;;
esac])

# --with-gxx-libcxx-include-dir controls the enabling of the -stdlib option.
# if --with-gxx-libcxx-include-dir is 'no' we disable the stdlib option.
# if --with-gxx-libcxx-include-dir is unset we enable the stdlib option
# based on the platform (to be available on platform versions where it is the
# default for the system tools). We also use a default path within the compiler
# install tree.
# Otherwise, we use the path provided and enable the stdlib option.
# If both --with-sysroot and --with-gxx-libcxx-include-dir are passed, we
# check to see if the latter starts with the former and, upon success, compute
# gcc_gxx_libcxx_include_dir as relative to the sysroot.
gcc_gxx_libcxx_include_dir_add_sysroot=0
gcc_enable_stdlib_opt=0
if test x${gcc_gxx_libcxx_include_dir} != x; then
  if test x${gcc_gxx_libcxx_include_dir} = xno; then
    # set defaults for the dir, but the option is disabled anyway.
    gcc_gxx_libcxx_include_dir=
  else
    gcc_enable_stdlib_opt=1
  fi
else
  case $target in
    *-darwin1[[1-9]]* | *-darwin2*)
       # Default this on for Darwin versions which default to libcxx,
       # and embed the path in the compiler install so that we get a
       # self-contained toolchain.
       gcc_enable_stdlib_opt=1
       ;;
    *) ;;
  esac
fi
AC_DEFINE_UNQUOTED(ENABLE_STDLIB_OPTION, $gcc_enable_stdlib_opt,
          [Define if the -stdlib= option should be enabled.])

# Sysroot behaviour as for gxx-include-dir
if test x${gcc_gxx_libcxx_include_dir} = x; then
  # default path,embedded in the compiler tree.
  libcxx_incdir='include/c++/v1'
  if test x${enable_version_specific_runtime_libs} = xyes; then
    gcc_gxx_libcxx_include_dir='${libsubdir}/$libcxx_incdir'
  else
    if test x$host != x$target; then
       libcxx_incdir="$target_alias/$libcxx_incdir"
    fi
    gcc_gxx_libcxx_include_dir="\$(libsubdir)/\$(libsubdir_to_prefix)$libcxx_incdir"
  fi
elif test "${with_sysroot+set}" = set; then
  gcc_gxx_libcxx_without_sysroot=`expr "${gcc_gxx_libcxx_include_dir}" : "${with_sysroot}"'\(.*\)'`
  if test "${gcc_gxx_libcxx_without_sysroot}"; then
    gcc_gxx_libcxx_include_dir="${gcc_gxx_libcxx_without_sysroot}"
    gcc_gxx_libcxx_include_dir_add_sysroot=1
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

case $target in
    *darwin*)
	ld64_flag=yes # Darwin can only use a ld64-compatible linker.
	dsymutil_flag=yes # Darwin uses dsymutil to link debug.
	;;
    *)
	ld64_flag=no
	dsymutil_flag=no
	;;
esac

# With pre-defined ld
AC_ARG_WITH(ld,
[AS_HELP_STRING([--with-ld], [arrange to use the specified ld (full pathname)])],
DEFAULT_LINKER="$with_ld")
if test x"${DEFAULT_LINKER+set}" = x"set"; then
  if test ! -x "$DEFAULT_LINKER"; then
    AC_MSG_ERROR([cannot execute: $DEFAULT_LINKER: check --with-ld or env. var. DEFAULT_LINKER])
  elif $DEFAULT_LINKER -v < /dev/null 2>&1 | grep GNU > /dev/null; then
    gnu_ld_flag=yes
  elif $DEFAULT_LINKER -v < /dev/null 2>&1 | grep ld64- > /dev/null; then
    ld64_flag=yes
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

# Allow the user to specify a dsymutil executable (used on Darwin only, so far)
AC_ARG_WITH(dsymutil,
[AS_HELP_STRING([--with-dsymutil], [arrange to use the specified dsymutil (full pathname)])],
DEFAULT_DSYMUTIL="$with_dsymutil")

dsymutil_vers=
if test x"${DEFAULT_DSYMUTIL+set}" = x"set"; then
  if test ! -x "$DEFAULT_DSYMUTIL"; then
    AC_MSG_ERROR([cannot execute: $DEFAULT_DSYMUTIL: check --with-dsymutil or env. var. DEFAULT_DSYMUTIL])
  else
    if dsymutil_vers=`$DEFAULT_DSYMUTIL -v /dev/null 2>&1`; then
      dsymutil_flag=yes
    fi
  fi
  AC_DEFINE_UNQUOTED(DEFAULT_DSYMUTIL,"$DEFAULT_DSYMUTIL",
	[Define to enable the use of a default debug linker.])
fi

AC_MSG_CHECKING([whether a default dsymutil was specified])
if test x"${DEFAULT_DSYMUTIL+set}" = x"set"; then
  AC_MSG_RESULT([yes ($DEFAULT_DSYMUTIL)])
else
  AC_MSG_RESULT(no)
fi

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
ACX_PROG_GNAT([-I"$srcdir"/ada/libgnat])
ACX_PROG_GDC([-I"$srcdir"/d])

# Do configure tests with the C++ compiler, since that's what we build with.
AC_LANG(C++)

# Remove all -O flags: for historical reasons, unless bootstrapping we prefer
# optimizations to be activated explicitly by the toplevel.
case "$CC" in
  */prev-gcc/xgcc*) ;;
  *)
    new_CFLAGS=
    for flag in $CFLAGS; do
      case $flag in
	-O*) ;;
	*) new_CFLAGS="$new_CFLAGS $flag" ;;
      esac
    done
    CFLAGS=$new_CFLAGS
    new_CXXFLAGS=
    for flag in $CXXFLAGS; do
      case $flag in
	-O*) ;;
	*) new_CXXFLAGS="$new_CXXFLAGS $flag" ;;
      esac
    done
    CXXFLAGS=$new_CXXFLAGS
    ;;
esac
AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(GDCFLAGS)

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
AC_CHECK_SIZEOF(ino_t)
AC_CHECK_SIZEOF(dev_t)

# Select the right stat being able to handle 64bit inodes, if needed.
if test "$enable_largefile" != no; then
  case "$host, $build" in
    *-*-aix*,*|*,*-*-aix*)
      if test "$ac_cv_sizeof_ino_t" = "4" -a "$ac_cv_sizeof_dev_t" = 4; then
	AC_DEFINE(HOST_STAT_FOR_64BIT_INODES, stat64x,
	  [Define which stat syscall is able to handle 64bit indodes.])
      fi;;
  esac
fi


# sizeof(char) is 1 by definition.
AC_CHECK_SIZEOF(void *)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_TYPES([long long], [AC_CHECK_SIZEOF(long long)])
GCC_STDINT_TYPES
if test x"$ac_cv_c_uint64_t" = x"no" -o x"$ac_cv_c_int64_t" = x"no"; then
  AC_MSG_ERROR([uint64_t or int64_t not found])
fi

# check what underlying integer type int64_t uses
AC_CACHE_CHECK(for int64_t underlying type, ac_cv_int64_t_type, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
template <typename T> struct X { };
template <>
struct X<long> { typedef long t; };
]], [[X<int64_t>::t x;]])],[ac_cv_int64_t_type=long],[ac_cv_int64_t_type="long long"])])
if test "$ac_cv_int64_t_type" = "long"; then
  AC_DEFINE(INT64_T_IS_LONG, 1,
  [Define if int64_t uses long as underlying type.])
else
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif
template <typename T> struct X { };
template <>
struct X<long long> { typedef long long t; };
]], [[X<int64_t>::t x;]])],[],[AC_MSG_ERROR([error verifying int64_t uses long long])])
fi

AC_CACHE_CHECK(for std::swap in <utility>, ac_cv_std_swap_in_utility, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <utility>
]], [[int a, b; std::swap(a,b);]])],[ac_cv_std_swap_in_utility=yes],[ac_cv_std_swap_in_utility=no])])
if test $ac_cv_std_swap_in_utility = yes; then
  AC_DEFINE(HAVE_SWAP_IN_UTILITY, 1,
  [Define if <utility> defines std::swap.])
fi

# Check whether compiler is affected by placement new aliasing bug (PR 29286).
# If the host compiler is affected by the bug, and we build with optimization
# enabled (which happens e.g. when cross-compiling), the pool allocator may
# get miscompiled.  Use -fno-strict-aliasing to work around this problem.
# Since there is no reliable feature check for the presence of this bug,
# we simply use a GCC version number check.  (This should never trigger for
# stages 2 or 3 of a native bootstrap.)
aliasing_flags=
if test "$GCC" = yes; then
  saved_CXXFLAGS="$CXXFLAGS"

  # The following test compilation will succeed if and only if $CXX accepts
  # -fno-strict-aliasing *and* is older than GCC 4.3.
  CXXFLAGS="$CXXFLAGS -fno-strict-aliasing"
  AC_MSG_CHECKING([whether $CXX is affected by placement new aliasing bug])
  AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#if (__GNUC__ > 4) || (__GNUC__ == 4 && __GNUC_MINOR__ >= 3)
#error compiler not affected by placement new aliasing bug
#endif
])],
    [AC_MSG_RESULT([yes]); aliasing_flags='-fno-strict-aliasing'],
    [AC_MSG_RESULT([no])])

  CXXFLAGS="$saved_CXXFLAGS"
fi
AC_SUBST(aliasing_flags)



# ---------------------
# Warnings and checking
# ---------------------

# Check $CC warning features (if it's GCC).
# We want to use -pedantic, but we don't want warnings about
# * 'long long'
# * variadic macros
# * overlong strings
# So, we only use -pedantic if we can disable those warnings.

# In stage 1, disable -Wformat warnings from old GCCs about new % codes
AC_ARG_ENABLE(build-format-warnings,
  AS_HELP_STRING([--disable-build-format-warnings],[don't use -Wformat while building GCC]),
  [],[enable_build_format_warnings=yes])
AS_IF([test $enable_build_format_warnings = no],
      [wf_opt=-Wno-format],[wf_opt=])
ACX_PROG_CXX_WARNING_OPTS(
	m4_quote(m4_do([-W -Wall -Wno-error=narrowing -Wwrite-strings ],
		       [-Wcast-qual $wf_opt])),
		       [loose_warn])
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-Wstrict-prototypes -Wmissing-prototypes ],
		       [])), [c_loose_warn])
ACX_PROG_CXX_WARNING_OPTS(
	m4_quote(m4_do([-Wmissing-format-attribute ], [-Wconditionally-supported ],
		       [-Woverloaded-virtual])), [strict_warn])
ACX_PROG_CC_WARNING_OPTS(
	m4_quote(m4_do([-Wold-style-definition -Wc++-compat])), [c_strict_warn])
ACX_PROG_CXX_WARNING_ALMOST_PEDANTIC(
	m4_quote(m4_do([-Wno-long-long -Wno-variadic-macros ], 
		       [-Wno-overlength-strings])), [strict_warn])
ACX_PROG_CXX_WARNINGS_ARE_ERRORS([manual], [strict_warn])

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
		 Flags are: assert,df,extra,fold,gc,gcac,gimple,misc,
		 rtlflag,rtl,runtime,tree,valgrind,types])],
[ac_checking_flags="${enableval}"],[
# Determine the default checks.
if test x$is_release = x ; then
  ac_checking_flags=yes,extra
else
  ac_checking_flags=release
fi])
IFS="${IFS= 	}"; ac_save_IFS="$IFS"; IFS="$IFS,"
for check in release $ac_checking_flags
do
	case $check in
	# these set all the flags to specific states
	yes)		ac_assert_checking=1 ; ac_checking=1 ; ac_df_checking= ;
			ac_fold_checking= ; ac_gc_checking=1 ; ac_extra_checking= ;
			ac_gc_always_collect= ; ac_gimple_checking=1 ; ac_rtl_checking= ;
			ac_rtlflag_checking=1 ; ac_runtime_checking=1 ;
			ac_tree_checking=1 ; ac_valgrind_checking= ;
			ac_types_checking=1 ;;
	no|none)	ac_assert_checking= ; ac_checking= ; ac_df_checking= ;
			ac_fold_checking= ; ac_gc_checking= ; ac_extra_checking= ;
			ac_gc_always_collect= ; ac_gimple_checking= ; ac_rtl_checking= ;
			ac_rtlflag_checking= ; ac_runtime_checking= ;
			ac_tree_checking= ; ac_valgrind_checking= ;
			ac_types_checking= ;;
	all)		ac_assert_checking=1 ; ac_checking=1 ; ac_df_checking=1 ;
			ac_fold_checking=1 ; ac_gc_checking=1 ; ac_extra_checking=1 ;
			ac_gc_always_collect=1 ; ac_gimple_checking=1 ; ac_rtl_checking=1 ;
			ac_rtlflag_checking=1 ; ac_runtime_checking=1 ;
			ac_tree_checking=1 ; ac_valgrind_checking= ;
			ac_types_checking=1 ;;
	release)	ac_assert_checking=1 ; ac_checking= ; ac_df_checking= ;
			ac_fold_checking= ; ac_gc_checking= ; ac_extra_checking= ;
			ac_gc_always_collect= ; ac_gimple_checking= ; ac_rtl_checking= ;
			ac_rtlflag_checking= ; ac_runtime_checking=1 ;
			ac_tree_checking= ; ac_valgrind_checking= ;
			ac_types_checking= ;;
	# these enable particular checks
	assert) 	ac_assert_checking=1 ;;
	df)		ac_df_checking=1 ;;
	extra)		ac_extra_checking=1 ;;
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

if test x$ac_checking != x ; then
  AC_DEFINE(CHECKING_P, 1,
[Define to 0/1 if you want more run-time sanity checks.  This one gets a grab
bag of miscellaneous but relatively cheap checks.])
else
  AC_DEFINE(CHECKING_P, 0)
fi
if test x$ac_extra_checking != x ; then
  AC_DEFINE(ENABLE_EXTRA_CHECKING, 1,
[Define to 0/1 if you want extra run-time checking that might affect code
generation.])
else
  AC_DEFINE(ENABLE_EXTRA_CHECKING, 0)
fi
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
   at runtime.  This is moderately expensive.
   ])
  TREECHECKING=yes
fi
if test x$ac_types_checking != x ; then
  AC_DEFINE(ENABLE_TYPES_CHECKING, 1,
[Define if you want all gimple types to be verified after gimplifiation.
   This is cheap.
   ])
fi
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
if test x$ac_valgrind_checking != x ; then
  AM_PATH_PROG_WITH_TEST(valgrind_path, valgrind,
	[$ac_dir/$ac_word --version | grep valgrind- >/dev/null 2>&1])
  if test "x$valgrind_path" = "x"; then
    AC_MSG_ERROR([*** Cannot find valgrind])
  fi
  valgrind_path_defines=-DVALGRIND_PATH='\"'$valgrind_path'\"'
  valgrind_command="$valgrind_path -q"
  AC_DEFINE(ENABLE_VALGRIND_CHECKING, 1,
[Define if you want to run subprograms and generated programs
   through valgrind (a memory checker).  This is extremely expensive.])
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
    coverage_flags="-fprofile-arcs -ftest-coverage -frandom-seed=\$@ -O0 -fkeep-static-functions"
    ;;
  opt)
    coverage_flags="-fprofile-arcs -ftest-coverage -frandom-seed=\$@ -O2 -fkeep-static-functions"
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

AC_ARG_ENABLE(valgrind-annotations,
[AS_HELP_STRING([--enable-valgrind-annotations],
		[enable valgrind runtime interaction])], [],
[enable_valgrind_annotations=no])
AC_CHECK_HEADER(valgrind/memcheck.h)
if test x$enable_valgrind_annotations != xno \
    || test x$ac_valgrind_checking != x; then
  if test $ac_cv_header_valgrind_memcheck_h = no; then
    AC_MSG_ERROR([*** Cannot find valgrind/memcheck.h])
  fi
  AC_DEFINE(ENABLE_VALGRIND_ANNOTATIONS, 1,
[Define to get calls to the valgrind runtime enabled.])
fi

# -------------------------------
# Miscenalleous configure options
# -------------------------------

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
if test x${enable_multiarch} = xyes; then
  AC_DEFINE(ENABLE_MULTIARCH, 1, [If --with-multiarch option is used])
fi
AC_MSG_CHECKING(for multiarch configuration)
AC_SUBST(enable_multiarch)
AC_MSG_RESULT($enable_multiarch$ma_msg_suffix)

# needed for setting the multiarch name for soft-float/hard-float ABIs
AC_SUBST(with_cpu)
AC_SUBST(with_float)

# default stack clash protection guard size as power of twos in bytes.
# Please keep these in sync with params.def.
stk_clash_min=12
stk_clash_max=30

# Keep the default value when the option is not used to 0, this allows us to
# distinguish between the cases where the user specifially set a value via
# configure and when the normal default value is used.
AC_ARG_WITH(stack-clash-protection-guard-size,
[AS_HELP_STRING([--with-stack-clash-protection-guard-size=size],
[Set the default stack clash protection guard size for specific targets as a power of two in bytes.])],
[DEFAULT_STK_CLASH_GUARD_SIZE="$with_stack_clash_protection_guard_size"], [DEFAULT_STK_CLASH_GUARD_SIZE=0])
if test $DEFAULT_STK_CLASH_GUARD_SIZE -ne 0 \
     && (test $DEFAULT_STK_CLASH_GUARD_SIZE -lt $stk_clash_min \
	 || test $DEFAULT_STK_CLASH_GUARD_SIZE -gt $stk_clash_max); then
  AC_MSG_ERROR(m4_normalize([
		Invalid value $DEFAULT_STK_CLASH_GUARD_SIZE for --with-stack-clash-protection-guard-size. \
		Must be between $stk_clash_min and $stk_clash_max.]))
fi

AC_DEFINE_UNQUOTED(DEFAULT_STK_CLASH_GUARD_SIZE, $DEFAULT_STK_CLASH_GUARD_SIZE,
	[Define to larger than zero set the default stack clash protector size.])

# Specify the number of splits of match.pd to generate.
AC_ARG_WITH(matchpd-partitions,
[AS_HELP_STRING([--with-matchpd-partitions=num],
[Set the number of partitions to make for gimple and generic when splitting match.pd. [default=10]])],
[DEFAULT_MATCHPD_PARTITIONS="$with_matchpd_partitions"], [DEFAULT_MATCHPD_PARTITIONS=10])
if (test $DEFAULT_MATCHPD_PARTITIONS -lt 1); then
  AC_MSG_ERROR(m4_normalize([
		Invalid value $DEFAULT_MATCHPD_PARTITIONS for --with-matchpd-partitions. \
		Cannot be negative.]))
fi

AC_SUBST(DEFAULT_MATCHPD_PARTITIONS)

# Specify the number of splits of insn-emit.cc and insn-recog.cc to generate.
AC_ARG_WITH(insnemit-partitions,
[AS_HELP_STRING([--with-insnemit-partitions=num],
[Set the number of partitions of insn-emit.cc for genemit and genrecog to create. [default=10]])],
[DEFAULT_INSNEMIT_PARTITIONS="$with_insnemit_partitions"], [DEFAULT_INSNEMIT_PARTITIONS=10])
if (test $DEFAULT_INSNEMIT_PARTITIONS -lt 1); then
  AC_MSG_ERROR(m4_normalize([
		Invalid value $DEFAULT_INSNEMIT_PARTITIONS for --with-insnemit-partitions. \
		Cannot be negative.]))
fi

AC_SUBST(DEFAULT_INSNEMIT_PARTITIONS)

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
    loongarch*-*-*)
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

AC_ARG_ENABLE(vtable-verify,
[AS_HELP_STRING([--enable-vtable-verify],
		[enable vtable verification feature])],,
[enable_vtable_verify=no])
vtable_verify=`if test x$enable_vtable_verify = xyes; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(ENABLE_VTABLE_VERIFY, $vtable_verify,
[Define 0/1 if vtable verification feature is enabled.])

AC_ARG_ENABLE(analyzer,
[AS_HELP_STRING([--disable-analyzer],
		[disable -fanalyzer static analyzer])],
if test x$enable_analyzer = xno; then
	analyzer=0
else
	analyzer=1
fi,
analyzer=1)
AC_DEFINE_UNQUOTED(ENABLE_ANALYZER, $analyzer,
[Define 0/1 if static analyzer feature is enabled.])

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
[AS_HELP_STRING([--with-dwarf2], [force the default debug format to be DWARF 2 (or later)])],
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

AC_ARG_ENABLE(gcov,
[  --disable-gcov          don't provide libgcov and related host tools],
[], [case $target in
       bpf-*-*)
         enable_gcov=no
       ;;
       *)
         enable_gcov=yes
       ;;
     esac])

AC_SUBST(enable_gcov)

AC_ARG_WITH(specs,
  [AS_HELP_STRING([--with-specs=SPECS],
                  [add SPECS to driver command-line processing])],
  [CONFIGURE_SPECS=$withval],
  [CONFIGURE_SPECS=]
)
AC_SUBST(CONFIGURE_SPECS)

ACX_PKGVERSION([GCC])
ACX_BUGURL([https://gcc.gnu.org/bugs/])

# Allow overriding the default URL for documentation
AC_ARG_WITH(documentation-root-url,
    AS_HELP_STRING([--with-documentation-root-url=URL],
                   [Root for documentation URLs]),
    [case "$withval" in
      yes) AC_MSG_ERROR([documentation root URL not specified]) ;;
      no)  AC_MSG_ERROR([documentation root URL not specified]) ;;
      */)  DOCUMENTATION_ROOT_URL="$withval" ;;
      *)   AC_MSG_ERROR([documentation root URL does not end with /]) ;;
     esac],[
     docroot_url_suffix=
changequote(,)dnl
     case "$gcc_version" in
       *.[123456].0) docroot_url_suffix="gcc-$gcc_version/";;
       *.[123456].1) docroot_url_suffix="gcc-`echo $gcc_version | sed 's/1$/0/'`/";;
     esac
changequote([,])dnl
     DOCUMENTATION_ROOT_URL="https://gcc.gnu.org/onlinedocs/$docroot_url_suffix"
])
AC_DEFINE_UNQUOTED(DOCUMENTATION_ROOT_URL,"$DOCUMENTATION_ROOT_URL",
	[Define to the root for documentation URLs.])

# Allow overriding the default URL for GCC changes
AC_ARG_WITH(changes-root-url,
    AS_HELP_STRING([--with-changes-root-url=URL],
                   [Root for GCC changes URLs]),
    [case "$withval" in
      yes) AC_MSG_ERROR([changes root URL not specified]) ;;
      no)  AC_MSG_ERROR([changes root URL not specified]) ;;
      */)  CHANGES_ROOT_URL="$withval" ;;
      *)   AC_MSG_ERROR([changes root URL does not end with /]) ;;
     esac],
     CHANGES_ROOT_URL="https://gcc.gnu.org/"
)
AC_DEFINE_UNQUOTED(CHANGES_ROOT_URL,"$CHANGES_ROOT_URL",
	[Define to the root for URLs about GCC changes.])

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

# If top-level libada has been disabled, then wire in install-gnatlib
# invocation with `make install', so that one can build and install
# the library manually with `make -C gcc all gnatlib gnattools install'.
if test x"$enable_libada" = xno; then
  gnat_install_lib=gnat-install-lib
else
  gnat_install_lib=
fi
AC_SUBST(gnat_install_lib)

if test x"$enable_as_accelerator_for" != x; then
  AC_DEFINE(ACCEL_COMPILER, 1,
    [Define if this compiler should be built as the offload target compiler.])
  enable_as_accelerator=yes
  sedscript="s#${target_noncanonical}#${enable_as_accelerator_for}-accel-${target_noncanonical}#"
  program_transform_name=`echo $program_transform_name | sed $sedscript`
  accel_dir_suffix=/accel/${target_noncanonical}
  real_target_noncanonical=${enable_as_accelerator_for}
fi
AC_SUBST(enable_as_accelerator)
AC_SUBST(real_target_noncanonical)
AC_SUBST(accel_dir_suffix)

for tgt in `echo $enable_offload_targets | sed 's/,/ /g'`; do
  tgt=`echo $tgt | sed 's/=.*//'`

  enable_offloading=1
  case "$tgt" in
    amdgcn*)
  	omp_device_property=omp-device-properties-gcn
  	omp_device_property_tmake_file="${omp_device_property_tmake_file} \$(srcdir)/config/gcn/t-omp-device"
  	;;
    nvptx*)
  	omp_device_property=omp-device-properties-nvptx
  	omp_device_property_tmake_file="${omp_device_property_tmake_file} \$(srcdir)/config/nvptx/t-omp-device"
  	;;
    *)
  	AC_MSG_ERROR([unknown offload target specified])
  	;;
  esac
  omp_device_properties="${omp_device_properties} ${tgt}=${omp_device_property}"
  omp_device_property_deps="${omp_device_property_deps} ${omp_device_property}"

  if test x"$offload_targets" = x; then
    offload_targets=$tgt
  else
    offload_targets="$offload_targets,$tgt"
  fi
done
AC_SUBST(omp_device_properties)
AC_SUBST(omp_device_property_deps)

AC_DEFINE_UNQUOTED(OFFLOAD_TARGETS, "$offload_targets",
  [Define to offload targets, separated by commas.])
if test x"$enable_offloading" != x; then
  AC_DEFINE(ENABLE_OFFLOADING, 1,
    [Define this to enable support for offloading.])
else
  AC_DEFINE(ENABLE_OFFLOADING, 0,
    [Define this to enable support for offloading.])
fi

if test "x$enable_offload_defaulted" = xyes; then
  AC_DEFINE(OFFLOAD_DEFAULTED, 1,
    [Define to 1 to if -foffload is defaulted])
fi

AC_ARG_WITH(multilib-list,
[AS_HELP_STRING([--with-multilib-list], [select multilibs (AArch64, ARM, AVR, nvptx, OR1K, RISC-V, SH and x86-64 only)])],
:,
with_multilib_list=default)

AC_ARG_WITH(multilib-generator,
[AS_HELP_STRING([--with-multilib-generator], [Multi-libs configuration string (RISC-V only)])],
:,
with_multilib_generator=default)

# -------------------------
# Checks for other programs
# -------------------------

AC_PROG_MAKE_SET

# Find some useful tools
AC_PROG_AWK
# We need awk to create options.cc and options.h.
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
  BUILD_INFO=no-info
else
  BUILD_INFO=info
fi
AC_SUBST(BUILD_INFO)

# Determine whether makeinfo supports the CONTENTS_OUTPUT_LOCATION variable.
# If it does, we want to pass it to makeinfo in order to restore the old
# behavior of @{summary,}contents placement.
MAKEINFO_TOC_INLINE_FLAG=
AS_IF([test x"$MAKEINFO" != x], [
  AC_CACHE_CHECK([for CONTENTS_OUTPUT_LOCATION support in $MAKEINFO],
  		 [gcc_cv_texi_add_toc_inline_flags], [
    # Detect the unknown variable CONTENTS_OUTPUT_LOCATION warning
    if "$MAKEINFO" -c CONTENTS_OUTPUT_LOCATION=inline </dev/null 2>&1 \
       | grep CONTENTS_OUTPUT_LOCATION >/dev/null; then
      gcc_cv_texi_add_toc_inline_flags=no
    else
      gcc_cv_texi_add_toc_inline_flags=yes
    fi
  ])
  if test x"$gcc_cv_texi_add_toc_inline_flags" = xyes; then
    MAKEINFO_TOC_INLINE_FLAG='-c CONTENTS_OUTPUT_LOCATION=inline'
  fi
])
AC_SUBST([MAKEINFO_TOC_INLINE_FLAG])

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

# Python3?
ACX_CHECK_PROG_VER(PYTHON, python3, --version,
  [Python.* \([0-9][0-9.]*\)],
  [3.[4-9]*|3.[1-9][0-9]*|[4-9].*|[1-9][0-9]*])
if test $gcc_cv_prog_python3_modern = no; then
  AC_MSG_WARN([
*** Python3 is missing.
*** Documentation for modula-2 will not include the target SYSTEM module.])
  HAVE_PYTHON=no
else
  HAVE_PYTHON=yes
fi
AC_SUBST(HAVE_PYTHON)

# Binutils are not build modules, unlike bison/flex/makeinfo.  So we
# check for build == host before using them.

# NM
if test x${build} = x${host} && test -f $srcdir/../binutils/nm.c \
  && test -d ../binutils ; then
  NM='../binutils/nm-new'
else
  AC_CHECK_PROG(NM, nm, nm, ${CONFIG_SHELL-/bin/sh} ${srcdir}/../missing nm)
fi

# AR
if test x${build} = x${host} && test -f $srcdir/../binutils/ar.c \
  && test -d ../binutils ; then
  AR='../binutils/ar'
else
  AC_CHECK_PROG(AR, ar, ar, ${CONFIG_SHELL-/bin/sh} ${srcdir}/../missing ar)
fi

# The jit documentation looks better if built with sphinx, but can be
# built with texinfo if sphinx is not available.
# Set "doc_build_sys" to "sphinx" or "texinfo" accordingly.
AC_CHECK_PROG(doc_build_sys, sphinx-build, sphinx, texinfo)

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
		 fcntl.h ftw.h unistd.h sys/auxv.h sys/file.h sys/time.h sys/mman.h \
		 sys/resource.h sys/param.h sys/times.h sys/stat.h sys/locking.h \
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

AC_CHECK_HEADERS(unordered_map)
AC_CHECK_HEADERS(tr1/unordered_map)
AC_CHECK_HEADERS(ext/hash_map)

# --------
# Dependency checking.
# --------

ZW_CREATE_DEPDIR
AC_CONFIG_COMMANDS([gccdepdir],[
  ${CONFIG_SHELL-/bin/sh} $ac_aux_dir/mkinstalldirs build/$DEPDIR
  for lang in $subdirs c-family common analyzer text-art rtl-ssa sym-exec
  do
      ${CONFIG_SHELL-/bin/sh} $ac_aux_dir/mkinstalldirs $lang/$DEPDIR
  done], [subdirs="$subdirs" ac_aux_dir=$ac_aux_dir DEPDIR=$DEPDIR])

ZW_PROG_COMPILER_DEPENDENCIES([CXX])

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

# Some systems need dlopen
save_LIBS="$LIBS"
LIBS=
AC_SEARCH_LIBS(dlopen, dl)
DL_LIB="$LIBS"
LIBS="$save_LIBS"
AC_SUBST(DL_LIB)

# Use <inttypes.h> only if it exists,
# doesn't clash with <sys/types.h>, declares intmax_t and defines
# PRId64
AC_MSG_CHECKING(for inttypes.h)
AC_CACHE_VAL(gcc_cv_header_inttypes_h,
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[#define __STDC_FORMAT_MACROS
#include <sys/types.h>
#include <inttypes.h>]],
  [[intmax_t i = -1;
#ifndef PRId64
choke me
#endif]])],
  [gcc_cv_header_inttypes_h=yes],
  [gcc_cv_header_inttypes_h=no])])
AC_MSG_RESULT($gcc_cv_header_inttypes_h)
if test $gcc_cv_header_inttypes_h = yes; then
  AC_DEFINE(HAVE_INTTYPES_H, 1,
	[Define if you have a working <inttypes.h> header file.])
fi

# Look for the ZSTD package.
ZSTD_INCLUDE=
ZSTD_LIB=
AC_SUBST(ZSTD_INCLUDE)
AC_SUBST(ZSTD_LIB)
ZSTD_CPPFLAGS=
ZSTD_LDFLAGS=
AC_SUBST(ZSTD_CPPFLAGS)
AC_SUBST(ZSTD_LDFLAGS)
AC_ARG_WITH(zstd,
	[AS_HELP_STRING([--with-zstd=PATH],
		[specify prefix directory for installed zstd library.
		 Equivalent to --with-zstd-include=PATH/include
		 plus --with-zstd-lib=PATH/lib])])
AC_ARG_WITH(zstd-include,
	[AS_HELP_STRING([--with-zstd-include=PATH],
		[specify directory for installed zstd include files])])
AC_ARG_WITH(zstd-lib,
	[AS_HELP_STRING([--with-zstd-lib=PATH],
		[specify directory for the installed zstd library])])
case "x$with_zstd" in
  x) ;;
  xno)
    ZSTD_INCLUDE=
    ZSTD_LIB=
    ;;
  *) ZSTD_INCLUDE=$with_zstd/include
     ZSTD_LIB=$with_zstd/lib
     ;;
esac

if test "x$with_zstd" != xno; then
if test "x$with_zstd_include" != x; then
  ZSTD_INCLUDE=$with_zstd_include
fi
if test "x$with_zstd_lib" != x; then
  ZSTD_LIB=$with_zstd_lib
fi
if test "x$ZSTD_INCLUDE" != x \
   && test "x$ZSTD_INCLUDE" != xno; then
  ZSTD_CPPFLAGS=-I$ZSTD_INCLUDE
fi
if test "x$ZSTD_LIB" != x \
   && test "x$ZSTD_LIB" != xno; then
  ZSTD_LDFLAGS=-L$ZSTD_LIB
fi

CXXFLAGS="$CXXFLAGS $ZSTD_CPPFLAGS"
LDFLAGS="$LDFLAGS $ZSTD_LDFLAGS"

# LTO can use zstd compression algorithm
save_LIBS="$LIBS"
LIBS=
AC_SEARCH_LIBS(ZSTD_compress, zstd,, gcc_cv_header_zstd_h=no)
ZSTD_LIB="$LIBS"
LIBS="$save_LIBS"
AC_SUBST(ZSTD_LIB)

AC_MSG_CHECKING(for zstd.h)
AC_CACHE_VAL(gcc_cv_header_zstd_h,
# We require version 1.3.0 or later.  This is the first version that has
# ZSTD_getFrameContentSize.
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
[[#include <zstd.h>
#if ZSTD_VERSION_NUMBER < 10300
#error "need zstd 1.3.0 or better"
#endif]])],
  [gcc_cv_header_zstd_h=yes],
  [gcc_cv_header_zstd_h=no])])
AC_MSG_RESULT($gcc_cv_header_zstd_h)
if test $gcc_cv_header_zstd_h = yes; then
  AC_DEFINE(HAVE_ZSTD_H, 1,
	[Define if you have a working <zstd.h> header file.])
elif test "x$with_zstd" != x; then
    as_fn_error $? "Unable to find zstd.h.  See config.log for details." "$LINENO" 5
fi
fi

dnl Disabled until we have a complete test for buggy enum bitfields.
dnl gcc_AC_C_ENUM_BF_UNSIGNED

define(gcc_UNLOCKED_FUNCS, clearerr_unlocked feof_unlocked dnl
  ferror_unlocked fflush_unlocked fgetc_unlocked fgets_unlocked dnl
  fileno_unlocked fprintf_unlocked fputc_unlocked fputs_unlocked dnl
  fread_unlocked fwrite_unlocked getchar_unlocked getc_unlocked dnl
  putchar_unlocked putc_unlocked)
AC_CHECK_FUNCS(times clock kill getrlimit setrlimit atoq \
	popen sysconf strsignal getrusage nl_langinfo \
	gettimeofday mbstowcs wcswidth mmap posix_fallocate setlocale \
	gcc_UNLOCKED_FUNCS madvise mallinfo mallinfo2 fstatat getauxval \
	clock_gettime)

# At least for glibc, clock_gettime is in librt.  But don't pull that
# in if it still doesn't give us the function we want.
if test $ac_cv_func_clock_gettime = no; then
  AC_CHECK_LIB(rt, clock_gettime,
    [LIBS="-lrt $LIBS"
     AC_DEFINE(HAVE_CLOCK_GETTIME, 1,
	       [Define to 1 if you have the `clock_gettime' function.])])
fi

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
AC_CHECK_TYPE(sighander_t,
  AC_DEFINE(HAVE_SIGHANDLER_T, 1,
    [Define if <sys/signal.h> defines sighandler_t]),
    ,signal.h)

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

AM_ICONV

# Until we have in-tree GNU iconv:
LIBICONV_DEP=
if test -f "$LTLIBICONV"; then
  LIBICONV_DEP=$LTLIBICONV
fi
AC_SUBST(LIBICONV_DEP)

AM_LC_MESSAGES

AM_LANGINFO_CODESET

# We will need to find libiberty.h and ansidecl.h
saved_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -I${srcdir} -I${srcdir}/../include $GMPINC"
saved_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -I${srcdir} -I${srcdir}/../include $GMPINC"

# gcc_AC_CHECK_DECLS doesn't support overloaded functions, so use the
# normal autoconf function for these.  But force definition of
# HAVE_DECL_BASENAME like gcc_AC_CHECK_DECLS does, to suppress the bizarre
# basename handling in libiberty.h.
AC_CHECK_DECLS([basename(const char*), strstr(const char*,const char*)], , ,[
#undef HAVE_DECL_BASENAME
#define HAVE_DECL_BASENAME 1
#include "ansidecl.h"
#include "system.h"])

gcc_AC_CHECK_DECLS(getenv atol atoll asprintf sbrk abort atof getcwd getwd \
	madvise mkstemps stpcpy strnlen strsignal strverscmp \
	strtol strtoul strtoll strtoull setenv unsetenv \
	errno snprintf vsnprintf vasprintf malloc realloc calloc \
	free getopt clock getpagesize ffs fmemopen gcc_UNLOCKED_FUNCS, , ,[
#include "ansidecl.h"
#include "system.h"])

gcc_AC_CHECK_DECLS(getrlimit setrlimit getrusage, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif
])

gcc_AC_CHECK_DECLS(mallinfo mallinfo2, , ,[
#include "ansidecl.h"
#include "system.h"
#ifdef HAVE_MALLOC_H
#include <malloc.h>
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
# in collect2.cc, <fcntl.h> isn't visible, but the configure test below needs
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

# Check if O_CLOEXEC is defined by fcntl
AC_CACHE_CHECK(for O_CLOEXEC, ac_cv_o_cloexec, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <fcntl.h>]], [[
return open ("/dev/null", O_RDONLY | O_CLOEXEC);]])],
[ac_cv_o_cloexec=yes],[ac_cv_o_cloexec=no])])
if test $ac_cv_o_cloexec = yes; then
  AC_DEFINE(HOST_HAS_O_CLOEXEC, 1,
  [Define if O_CLOEXEC supported by fcntl.])
fi

# Check if O_NONBLOCK is defined by fcntl
AC_CHECK_HEADERS([fcntl.h])
AC_CHECK_DECL(O_NONBLOCK,,,[#include <fcntl.h>])
if test $ac_cv_have_decl_O_NONBLOCK = yes; then
  AC_DEFINE(HOST_HAS_O_NONBLOCK, 1,
  [Define if O_NONBLOCK supported by fcntl.])
fi


# C++ Modules would like some networking features to provide the mapping
# server.  You can still use modules without them though.
# The following network-related checks could probably do with some
# Windows and other non-linux defenses and checking.

# Local socket connectivity wants AF_UNIX networking
# Check for AF_UNIX networking
AC_CACHE_CHECK(for AF_UNIX, ac_cv_af_unix, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>]],[[
sockaddr_un un;
un.sun_family = AF_UNSPEC;
int fd = socket (AF_UNIX, SOCK_STREAM, 0);
connect (fd, (sockaddr *)&un, sizeof (un));]])],
[ac_cv_af_unix=yes],
[ac_cv_af_unix=no])])
if test $ac_cv_af_unix = yes; then
  AC_DEFINE(HAVE_AF_UNIX, 1,
  [Define if AF_UNIX supported.])
fi

# Remote socket connectivity wants AF_INET6 networking
# Check for AF_INET6 networking
AC_CACHE_CHECK(for AF_INET6, ac_cv_af_inet6, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>]],[[
sockaddr_in6 in6;
in6.sin6_family = AF_UNSPEC;
struct addrinfo *addrs = 0;
struct addrinfo hints;
hints.ai_flags = 0;
hints.ai_family = AF_INET6;
hints.ai_socktype = SOCK_STREAM;
hints.ai_protocol = 0;
hints.ai_canonname = 0;
hints.ai_addr = 0;
hints.ai_next = 0;
int e = getaddrinfo ("localhost", 0, &hints, &addrs);
const char *str = gai_strerror (e);
freeaddrinfo (addrs);
int fd = socket (AF_INET6, SOCK_STREAM, 0);
connect (fd, (sockaddr *)&in6, sizeof (in6));]])],
[ac_cv_af_inet6=yes],
[ac_cv_af_inet6=no])])
if test $ac_cv_af_inet6 = yes; then
  AC_DEFINE(HAVE_AF_INET6, 1,
  [Define if AF_INET6 supported.])
fi

# Check if _LK_LOCK is supported by _locking
AC_CACHE_CHECK(for _LK_LOCK, ac_cv_lk_lock, [
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <io.h>
#include <sys/locking.h>]], [[
  int fd;
  return _locking (fd, _LK_LOCK, 0);]])],
[ac_cv_lk_lock=yes],[ac_cv_lk_lock=no])])
if test $ac_cv_lk_lock = yes; then
  AC_DEFINE(HOST_HAS_LK_LOCK, 1,
  [Define if _LK_LOC supported by _locking.])
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
[force_sjlj_exceptions=yes],
[case $target in
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

# Windows32 UTF-8 manifest support for running the driver and compiler
# executables with the UTF-8 active code page on mingw hosts.
# Non-mingw hosts ignore this option.
# The shell variables this sets are picked up from the mingw branches
# of config.host so they have to be set before it gets sourced.
AC_ARG_ENABLE(win32-utf8-manifest,
[AS_HELP_STRING([--disable-win32-utf8-manifest],
                [disable embedding a utf8 manifest on mingw hosts])
AS_HELP_STRING([--enable-win32-utf8-manifest],
               [enable embedding a utf8 manifest on mingw hosts (default)])],,)

if test "x$enable_win32_utf8_manifest" != xno; then
  host_xmake_mingw=i386/x-mingw32-utf8
  host_extra_gcc_objs_mingw=utf8rc-mingw32.o
  host_extra_objs_mingw=utf8-mingw32.o
fi

# --------------------------------------------------------
# Build, host, and target specific configuration fragments
# --------------------------------------------------------

# Collect build-machine-specific information.
. ${srcdir}/config.build || exit 1

# Collect host-machine-specific information.
. ${srcdir}/config.host || exit 1

target_gtfiles=

# Collect target-machine-specific information.
. ${srcdir}/config.gcc || exit 1

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
then out_file=$cpu_type/$cpu_type.cc; fi

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

if test x$use_long_long_for_widest_fast_int = xyes; then
	AC_DEFINE(USE_LONG_LONG_FOR_WIDEST_FAST_INT, 1,
[Define to 1 if the 'long long' type is wider than 'long' but still
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
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([
#include <sys/cdefs.h>
#include <sys/signal.h>
#include <ucontext.h>
int main() { mcontext_t m; if (m->ss.srr0) return 0; return 0; }
])],
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
  single | tpf | vxworks | win32 | mcf)
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
	HAVE_AUTO_BUILD='# '
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
	GMPINC="" CPPFLAGS="${CPPFLAGS_FOR_BUILD} -DGENERATOR_FILE" \
	${realsrcdir}/configure \
		--enable-languages=${enable_languages-all} \
		${enable_obsolete+--enable-obsolete="$enable_obsolete"} \
		${enable_option_checking+--enable-option-checking="$enable_option_checking"} \
		--target=$target_alias --host=$build_alias \
		--build=$build_alias || exit # retaining $tempdir

	# We just finished tests for the build machine, so rename
	# the file auto-build.h in the gcc directory.
	mv auto-host.h ../auto-build.h
	cd ..
	rm -rf $tempdir
	build_auto=auto-build.h
	HAVE_AUTO_BUILD=
fi
AC_SUBST(build_subdir)
AC_SUBST(HAVE_AUTO_BUILD)

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
	gcc_reconf_arguments=`echo "$gcc_config_arguments" | sed -e 's/^.*\( : (reconfigured) .*$\)/\1/'`
	if [ "$gcc_reconf_arguments" != " : (reconfigured) $TOPLEVEL_CONFIGURE_ARGUMENTS" ]; then
		gcc_config_arguments="$gcc_config_arguments : (reconfigured) $TOPLEVEL_CONFIGURE_ARGUMENTS"
	fi
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

# Determine what GCC version number to use in filesystem paths.
GCC_BASE_VER

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

AC_ARG_ENABLE(mingw-wildcard,
[AS_HELP_STRING([--enable-mingw-wildcard],
		[Set whether to expand wildcard on command-line.
		 Default to platform configuration])],
[],[enable_mingw_wildcard=platform])
AS_IF([test x"$enable_mingw_wildcard" != xplatform ],
      [AC_DEFINE_UNQUOTED(MINGW_DOWILDCARD,
		 $(test x"$enable_mingw_wildcard" = xno; echo $?),
		 [Value to set mingw's _dowildcard to.])])

AC_ARG_ENABLE(large-address-aware,
[AS_HELP_STRING([--enable-large-address-aware],
		[Link mingw executables with --large-address-aware])])
AS_IF([test x"$enable_large_address_aware" = xyes],
  [AC_DEFINE([MINGW_DEFAULT_LARGE_ADDR_AWARE], 1,
    [Define if we should link mingw executables with --large-address-aware])])

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
		[enable -fno-omit-frame-pointer by default for x86])], [],
[
case $target_os in
linux* | gnu* | darwin[[8912]]* | cygwin* | mingw*)
  # Enable -fomit-frame-pointer by default for these systems with DWARF2.
  enable_frame_pointer=no
  ;;
*)
  enable_frame_pointer=yes
  ;;
esac
])

case $target in
i[[34567]]86-*-* | x86_64-*-*)
	if test "x$enable_cld" = xyes; then
		tm_defines="${tm_defines} USE_IX86_CLD=1"
	fi
	if test "x$enable_frame_pointer" = xyes; then
		tm_defines="${tm_defines} USE_IX86_FRAME_POINTER=1"
	fi
	;;
esac

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
  win32 | pe | cygwin* | mingw32*)
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
tmake_file="${tmake_file_}${omp_device_property_tmake_file}"

out_object_file=`basename $out_file .cc`.o
common_out_object_file=`basename $common_out_file .cc`.o

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

tm_d_file_list=
tm_d_include_list=
for f in $tm_d_file; do
  case $f in
    * )
       tm_d_file_list="${tm_d_file_list} \$(srcdir)/config/$f"
       tm_d_include_list="${tm_d_include_list} config/$f"
       ;;
  esac
done

tm_rust_file_list=
tm_rust_include_list=
for f in $tm_rust_file; do
  case $f in
    * )
       tm_rust_file_list="${tm_rust_file_list} \$(srcdir)/config/$f"
       tm_rust_include_list="${tm_rust_include_list} config/$f"
       ;;
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
BUILD_SYSTEM_HEADER_DIR=$SYSTEM_HEADER_DIR	AC_SUBST(BUILD_SYSTEM_HEADER_DIR)

if test x$host != x$target || test "x$TARGET_SYSTEM_ROOT" != x ||
   test x$build != x$host || test "x$with_build_sysroot" != x; then
  if test "x$with_build_sysroot" != x; then
    BUILD_SYSTEM_HEADER_DIR=$with_build_sysroot'$${sysroot_headers_suffix}$(NATIVE_SYSTEM_HEADER_DIR)'
  else
    BUILD_SYSTEM_HEADER_DIR='$(CROSS_SYSTEM_HEADER_DIR)'
  fi

  if test x$host != x$target
  then
    CROSS="-DCROSS_DIRECTORY_STRUCTURE"
    ALL=all.cross
    SYSTEM_HEADER_DIR=$BUILD_SYSTEM_HEADER_DIR
  elif test "x$TARGET_SYSTEM_ROOT" != x; then
    SYSTEM_HEADER_DIR='$(CROSS_SYSTEM_HEADER_DIR)'
  fi

  if test "x$with_build_sysroot" != "x"; then
    target_header_dir="${with_build_sysroot}${native_system_header_dir}"
  elif test "x$with_sysroot" = x; then
    target_header_dir="${test_exec_prefix}/${target_noncanonical}/sys-include"
  elif test "x$with_sysroot" = xyes; then
    target_header_dir="${test_exec_prefix}/${target_noncanonical}/sys-root${native_system_header_dir}"
  else
    target_header_dir="${with_sysroot}${native_system_header_dir}"
  fi
else
  target_header_dir=${native_system_header_dir}
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
     { test "x$with_headers" = xno || test ! -f "$target_header_dir/stdio.h"; } ; then
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
    BUILD_CFLAGS='$(INTERNAL_CFLAGS) $(T_CFLAGS) $(CFLAGS-$@) $(CFLAGS_FOR_BUILD)'
    BUILD_CXXFLAGS='$(INTERNAL_CFLAGS) $(T_CFLAGS) $(CFLAGS-$@) $(CXXFLAGS_FOR_BUILD)'
    BUILD_LDFLAGS='$(LDFLAGS_FOR_BUILD)'
fi


if test x$enable_fixincludes = xno;
then
    STMP_FIXINC=''
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

AM_CONDITIONAL([ENABLE_DARWIN_AT_RPATH], [test x$enable_darwin_at_rpath = xyes])
DARWIN_DO_EXTRA_RPATH=0
AC_ARG_WITH(darwin-extra-rpath,
[AS_HELP_STRING(
  [[--with-darwin-extra-rpath=[ARG]]],
   [Specify a runpath directory, additional to those provided by the compiler])],
[if test x"$withval" != x; then
   DARWIN_ADD_RPATH="$withval"
   DARWIN_DO_EXTRA_RPATH=1
 fi])
AC_DEFINE_UNQUOTED(DARWIN_DO_EXTRA_RPATH, $DARWIN_DO_EXTRA_RPATH,
  [Should add an extra runpath directory])
AC_DEFINE_UNQUOTED(DARWIN_ADD_RPATH, "$DARWIN_ADD_RPATH",
  [Specify a runpath directory, additional to those provided by the compiler])

# Identify the assembler which will work hand-in-glove with the newly
# built GCC, so that we can examine its features.  This is the assembler
# which will be driven by the driver program.
#
# If build != host, and we aren't building gas in-tree, we identify a
# build->target assembler and hope that it will have the same features
# as the host->target assembler we'll be using.
gcc_cv_as_gas_srcdir=`echo $srcdir | sed -e 's,/gcc$,,'`/gas

m4_pattern_allow([AS_FOR_TARGET])dnl
AS_VAR_SET_IF(gcc_cv_as,, [
if test -x "$DEFAULT_ASSEMBLER"; then
	gcc_cv_as="$DEFAULT_ASSEMBLER"
elif test -f $gcc_cv_as_gas_srcdir/configure.ac \
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

default_ld=
AC_ARG_ENABLE(ld,
[[  --enable-ld[=ARG]       build ld [ARG={default,yes,no}]]],
[case "${enableval}" in
 no)
   default_ld=ld.gold
   ;;
 esac])

install_gold_as_default=no
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
 esac])

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
elif test -f $gcc_cv_ld_gld_srcdir/configure.ac \
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
   PLUGIN_LD_SUFFIX="$withval"
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

# Check to see if we are using mold instead of ld
AC_MSG_CHECKING(whether we are using mold)
ld_is_mold=no
if test x$gcc_cv_ld != x; then
  if $gcc_cv_ld --version 2>/dev/null | sed 1q \
     | grep "mold" > /dev/null; then
    ld_is_mold=yes
  fi
fi
AC_MSG_RESULT($ld_is_mold)


AC_MSG_CHECKING(gold linker with split stack support as non default)
# Check to see if default ld is not gold, but gold is
# available and has support for split stack.  If gcc was configured
# with gold then no checking is done.
# 
if test x$ld_is_gold = xno && which ${gcc_cv_ld}.gold >/dev/null 2>&1; then

# For platforms other than powerpc64*, enable as appropriate.

  gold_non_default=no
  ld_gold=`which ${gcc_cv_ld}.gold`
# Make sure this gold has minimal split stack support
  if $ld_gold --help 2>/dev/null | grep split-stack-adjust-size >/dev/null 2>&1; then
    ld_vers=`$ld_gold --version | sed 1q`
    gold_vers=`echo $ld_vers | sed -n \
          -e 's,^[[^)]]*[[  ]]\([[0-9]][[0-9]]*\.[[0-9]][[0-9]]*[[^)]]*\)) .*$,\1,p'`
    case $target in
# check that the gold version contains the complete split stack support
# on powerpc64 big and little endian
      powerpc64*-*-*)
        case "$gold_vers" in
          2.25.[[1-9]]*|2.2[[6-9]][[.0-9]]*|2.[[3-9]][[.0-9]]*|[[3-9]].[[.0-9]]*) gold_non_default=yes
          ;;
          *) gold_non_default=no
          ;;
        esac
        ;;
    esac
  fi
  if test $gold_non_default = yes; then
    AC_DEFINE(HAVE_GOLD_NON_DEFAULT_SPLIT_STACK, 1,
    	    [Define if the gold linker supports split stack and is available as a non-default])
  fi
fi
AC_MSG_RESULT($gold_non_default)

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
	for f in $gcc_cv_ld_bfd_srcdir/configure $gcc_cv_ld_gld_srcdir/configure $gcc_cv_ld_gld_srcdir/configure.ac $gcc_cv_ld_gld_srcdir/Makefile.in
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
if test -f $gcc_cv_binutils_srcdir/configure.ac \
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
if test -f $gcc_cv_binutils_srcdir/configure.ac \
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
if test -f $gcc_cv_binutils_srcdir/configure.ac \
     && test -f ../binutils/Makefile \
     && test x$build = x$host; then
	# Single tree build which includes binutils.
	gcc_cv_readelf=../binutils/readelf$build_exeext
elif test -x readelf$build_exeext; then
	gcc_cv_readelf=./readelf$build_exeext
elif ( set dummy $READELF_FOR_TARGET; test -x $[2] ); then
        gcc_cv_readelf="$READELF_FOR_TARGET"
else
        AC_PATH_PROG(gcc_cv_readelf, $READELF_FOR_TARGET)
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

# Figure out what otool we will be using.
AS_VAR_SET_IF(gcc_cv_otool,, [
if test -x otool$build_exeext; then
	gcc_cv_otool=./otool$build_exeext
elif ( set dummy $OTOOL_FOR_TARGET; test -x $[2] ); then
        gcc_cv_otool="$OTOOL_FOR_TARGET"
else
        AC_PATH_PROG(gcc_cv_otool, $OTOOL_FOR_TARGET)
fi])

AC_MSG_CHECKING(what otool to use)
if test x$gcc_cv_otool = x; then
	AC_MSG_RESULT(not found)
else
	AC_MSG_RESULT($gcc_cv_otool)
fi

# Figure out the dsymutil we will use.
AS_VAR_SET_IF(gcc_cv_dsymutil,, [
if test -x "$DEFAULT_DSYMUTIL"; then
    gcc_cv_dsymutil="$DEFAULT_DSYMUTIL"
elif test -x dsymutil$build_exeext; then
    gcc_cv_dsymutil=./dsymutil$build_exeext
elif ( set dummy $DSYMUTIL_FOR_TARGET; test -x $[2] ); then
    gcc_cv_dsymutil=$DSYMUTIL_FOR_TARGET
elif ( set dummy $DSYMUTIL; test -x $[2] ); then
    gcc_cv_dsymutil=$DSYMUTIL
else
    AC_PATH_PROG(gcc_cv_dsymutil, $DSYMUTIL_FOR_TARGET)
fi])

ORIGINAL_DSYMUTIL_FOR_TARGET=$gcc_cv_dsymutil
AC_SUBST(ORIGINAL_DSYMUTIL_FOR_TARGET)
case "$ORIGINAL_DSYMUTIL_FOR_TARGET" in
  ./dsymutil | ./dsymutil$build_exeext) ;;
  *) AC_CONFIG_FILES(dsymutil:exec-tool.in, [chmod +x dsymutil]) ;;
esac 

# Figure out what assembler alignment features are present.
gcc_GAS_CHECK_FEATURE([.balign and .p2align], gcc_cv_as_balign_and_p2align,,
[.balign 4
.p2align 2],,
[AC_DEFINE(HAVE_GAS_BALIGN_AND_P2ALIGN, 1,
  [Define if your assembler supports .balign and .p2align.])])

gcc_GAS_CHECK_FEATURE([.p2align with maximum skip], gcc_cv_as_max_skip_p2align,,
 [.p2align 4,,7],,
[AC_DEFINE(HAVE_GAS_MAX_SKIP_P2ALIGN, 1,
  [Define if your assembler supports specifying the maximum number
   of bytes to skip when using the GAS .p2align command.])])

gcc_GAS_CHECK_FEATURE([.literal16], gcc_cv_as_literal16,,
 [.literal16],,
[AC_DEFINE(HAVE_GAS_LITERAL16, 1,
  [Define if your assembler supports .literal16.])])

gcc_GAS_CHECK_FEATURE([working .subsection -1], gcc_cv_as_subsection_m1,,
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

gcc_GAS_CHECK_FEATURE([.weak], gcc_cv_as_weak,,
 [	.weak foobar],,
[AC_DEFINE(HAVE_GAS_WEAK, 1, [Define if your assembler supports .weak.])])

gcc_GAS_CHECK_FEATURE([.weakref], gcc_cv_as_weakref,,
 [	.weakref foobar, barfnot],,
[AC_DEFINE(HAVE_GAS_WEAKREF, 1, [Define if your assembler supports .weakref.])])

gcc_GAS_CHECK_FEATURE([.nsubspa comdat], gcc_cv_as_nsubspa_comdat,,
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
case "${target}" in
  *-*-aix*)
    conftest_s='	.globl foobar,hidden'
    ;;
  *)
    conftest_s='	.hidden foobar
foobar:'
    ;;
esac
gcc_GAS_CHECK_FEATURE([.hidden], gcc_cv_as_hidden,, [$conftest_s])
case "${target}" in
  *-*-darwin*)
    # Darwin as has some visibility support, though with a different syntax.
    gcc_cv_as_hidden=yes
    ;;
esac

gcc_GAS_CHECK_FEATURE([.base64], gcc_cv_as_base64,,
 [	.section .rodata
 	.base64 "Tm9uIHB1ZG9yIGVzdCBuaWwgc2NpcmUsIHB1ZG9yIG5pbCBkaXNjZXJlIHZlbGxlLgo="],,
[AC_DEFINE(HAVE_GAS_BASE64, 1, [Define if your assembler supports .base64.])])

gcc_GAS_CHECK_FEATURE([.cv_ucomp], gcc_cv_as_cv_ucomp,,[.cv_ucomp 0],,
[AC_DEFINE(HAVE_GAS_CV_UCOMP, 1, [Define if your assembler supports .cv_ucomp.])])

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

case "${target}" in
  riscv*-*-linux*)
    AC_MSG_CHECKING(linker ifunc IRELATIVE support)
    cat > conftest.s <<EOF
	.text
	.type	foo_resolver, @function
foo_resolver:
	ret
	.size	foo_resolver, .-foo_resolver

	.globl	foo
	.type	foo, %gnu_indirect_function
	.set	foo, foo_resolver

	.globl	bar
	.type	bar, @function
bar:
	call	foo
	ret
	.size	bar, .-bar
EOF
    if test x$gcc_cv_as != x \
       && test x$gcc_cv_ld != x \
       && test x$gcc_cv_readelf != x \
       && $gcc_cv_as -o conftest.o conftest.s > /dev/null 2>&1 \
       && $gcc_cv_ld -o conftest conftest.o > /dev/null 2>&1 \
       && $gcc_cv_readelf --relocs --wide conftest \
	  | grep R_RISCV_IRELATIVE > /dev/null 2>&1; then
      enable_gnu_indirect_function=yes
    fi
    rm -f conftest conftest.o conftest.s
    AC_MSG_RESULT($enable_gnu_indirect_function)
    ;;
esac

gif=`if test x$enable_gnu_indirect_function = xyes; then echo 1; else echo 0; fi`
AC_DEFINE_UNQUOTED(HAVE_GNU_INDIRECT_FUNCTION, $gif,
[Define if your system supports gnu indirect functions.])


changequote(,)dnl
ld_ver=`$gcc_cv_ld --version 2>/dev/null | sed 1q`
if test $in_tree_ld != yes ; then
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
    ld_date=`echo $ld_ver | sed -n 's,^.*\([2-9][0-9][0-9][0-9]\)\(-*\)\([01][0-9]\)\2\([0-3][0-9]\).*$,\1\3\4,p'`
    ld_vers_major=`expr "$ld_vers" : '\([0-9]*\)'`
    ld_vers_minor=`expr "$ld_vers" : '[0-9]*\.\([0-9]*\)'`
    ld_vers_patch=`expr "$ld_vers" : '[0-9]*\.[0-9]*\.\([0-9]*\)'`
  else
    case "${target}" in
      *-*-solaris2*)
	# Solaris 2 ld -V output looks like this for a regular version:
	#
	# ld: Software Generation Utilities - Solaris Link Editors: 5.11-1.1699
	#
	# but test versions add stuff at the end:
	#
	# ld: Software Generation Utilities - Solaris Link Editors: 5.11-1.1701:onnv-ab196087-6931056-03/25/10
	#
	# ld and ld.so.1 are guaranteed to be updated in lockstep, so ld version
	# numbers can be used in ld.so.1 feature checks even if a different
	# linker is configured.
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
  elif test x"$ld_is_mold" = xyes; then
    :
  elif echo "$ld_ver" | grep GNU > /dev/null; then
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
      *-*-aix[789]*)
        gcc_cv_ld_hidden=yes
        ;;
      *-*-darwin*)
	# Darwin ld has some visibility support.
	gcc_cv_ld_hidden=yes
        ;;
      hppa64*-*-hpux* | ia64*-*-hpux*)
	gcc_cv_ld_hidden=yes
	;;
      *-*-solaris2*)
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
     && $gcc_cv_as -o conftest3.o conftest3.s > /dev/null 2>&1; then
    if $gcc_cv_ld -shared -o conftest1.so conftest1.o \
       conftest2.o conftest3.o > /dev/null 2>&1 \
       || $gcc_cv_ld -r -o conftest1.so conftest1.o \
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

# Some assemblers (GNU as for LoongArch) generates relocations for
# leb128 symbol arithmetic for relaxation, we need to disable relaxation
# probing leb128 support then.
case $target in
  loongarch*-*-*)
    gcc_GAS_CHECK_FEATURE([-mno-relax support],
      gcc_cv_as_mno_relax,[-mno-relax],[.text],,
      [check_leb128_asflags=-mno-relax])
    ;;
  *)
    check_leb128_asflags=
    ;;
esac

# Check if we have .[us]leb128, and support symbol arithmetic with it.
# Older versions of GAS and some non-GNU assemblers, have a bugs handling
# these directives, even when they appear to accept them.
gcc_GAS_CHECK_FEATURE([.sleb128 and .uleb128], gcc_cv_as_leb128,
[$check_leb128_asflags],
[	.data
	.uleb128 L2 - L1
L1:
	.uleb128 1280
	.sleb128 -1010
L2:
	.uleb128 0x8000000000000000
],
[[
if test "x$gcc_cv_objdump" != x; then
  if $gcc_cv_objdump -s conftest.o 2>/dev/null \
     | grep '04800a8e 78808080 80808080 808001' >/dev/null; then
    gcc_cv_as_leb128=yes
  fi
elif test "x$gcc_cv_otool" != x; then
  if $gcc_cv_otool -d conftest.o 2>/dev/null \
     | grep '04 80 0a 8e 78 80 80 80 80 80 80 80 80 80 01' >/dev/null; then
    gcc_cv_as_leb128=yes
  fi
else
  # play safe, assume the assembler is broken.
  :
fi
]],
 [AC_DEFINE(HAVE_AS_LEB128, 1,
   [Define if your assembler supports .sleb128 and .uleb128.])],
 [AC_DEFINE(HAVE_AS_LEB128, 0,
   [Define if your assembler supports .sleb128 and .uleb128.])])

# Determine if an .eh_frame section is read-only.
gcc_fn_eh_frame_ro () {
  $gcc_cv_as $1 -o conftest.o conftest.s > /dev/null 2>&1 && \
    $gcc_cv_objdump -h conftest.o 2>/dev/null | \
    sed -e '/.eh_frame/!d' -e N | grep READONLY > /dev/null
}

# Check if we have assembler support for unwind directives.
gcc_GAS_CHECK_FEATURE([cfi directives], gcc_cv_as_cfi_directive,,
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
    if test "x$gcc_cv_ld_ro_rw_mix" = xread-write; then
      gcc_cv_as_cfi_directive=yes
    elif test "x$gcc_cv_objdump" = x; then
      # No objdump, err on the side of caution.
      gcc_cv_as_cfi_directive=no
    else
      if test x$gas = xyes; then
	as_32_opt="--32"
	as_64_opt="--64"
      else
	as_32_opt="-m32"
	as_64_opt="-m64"
      fi
      case "$target" in
	sparc*-*-solaris2.*)
	  # On Solaris/SPARC, .eh_frame sections should always be read-write.
	  if gcc_fn_eh_frame_ro $as_32_opt \
	     || gcc_fn_eh_frame_ro $as_64_opt; then
	    gcc_cv_as_cfi_directive=no
	  else
	    gcc_cv_as_cfi_directive=yes
	  fi
	  ;;
	i?86-*-solaris2.* | x86_64-*-solaris2.*)
	  # On Solaris/x86, make sure that GCC and assembler agree on using
	  # read-only .eh_frame sections for 64-bit.
	  if gcc_fn_eh_frame_ro $as_32_opt; then
	    gcc_cv_as_cfi_directive=no
	  elif gcc_fn_eh_frame_ro $as_64_opt; then
	    gcc_cv_as_cfi_directive=yes
	  else
	    gcc_cv_as_cfi_directive=no
	  fi
	  ;;
      esac
    fi
    ;;
  *-*-*)
    gcc_cv_as_cfi_directive=yes
    ;;
esac])
if test $gcc_cv_as_cfi_directive = yes && test x$gcc_cv_objdump != x; then
gcc_GAS_CHECK_FEATURE([working cfi advance], gcc_cv_as_cfi_advance_working,,
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
  gcc_cv_as_cfi_personality_directive,,
[	.text
	.cfi_startproc
	.cfi_personality 0, symbol
	.cfi_endproc])
AC_DEFINE_UNQUOTED(HAVE_GAS_CFI_PERSONALITY_DIRECTIVE,
  [`if test $gcc_cv_as_cfi_personality_directive = yes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports .cfi_personality.])

gcc_GAS_CHECK_FEATURE([cfi sections directive],
  gcc_cv_as_cfi_sections_directive,,
[	.text
	.cfi_sections .debug_frame, .eh_frame
	.cfi_startproc
	.cfi_endproc],
[case $target_os in
  win32 | pe | cygwin* | mingw32*)
    # Need to check that we generated the correct relocation for the
    # .debug_frame section.  This was fixed for binutils 2.21.
    gcc_cv_as_cfi_sections_directive=no
    if test "x$gcc_cv_objdump" != x; then
     if $gcc_cv_objdump -j .debug_frame -r conftest.o 2>/dev/null | \
	grep -i secrel > /dev/null; then
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
  [`if test $gcc_cv_as_cfi_sections_directive = yes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports .cfi_sections.])

# GAS versions up to and including 2.11.0 may mis-optimize
# .eh_frame data.
gcc_GAS_CHECK_FEATURE(eh_frame optimization, gcc_cv_as_eh_frame,,
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

# Test if the assembler supports the section flag 'e' or #exclude for
# specifying an excluded section.
gcc_GAS_CHECK_FEATURE([section exclude flag], gcc_cv_as_section_exclude_e,
 [--fatal-warnings],
 [.section foo1,"e"
  .byte 0,0,0,0])
if test $gcc_cv_as_section_exclude_e = no; then
  case "${target}" in
    # Solaris as uses #exclude instead.
    *-*-solaris2*)
      case "${target}" in
	sparc*-*-solaris2*)
	  conftest_s='.section "foo1", #exclude'
	  ;;
	i?86-*-solaris2* | x86_64-*-solaris2*)
	  conftest_s='.section foo1, #exclude'
	  ;;      
      esac
      ;;
    esac
  gcc_GAS_CHECK_FEATURE([section exclude flag], gcc_cv_as_section_exclude_hash,,
    [$conftest_s
     .byte 0,0,0,0])
fi
AC_DEFINE_UNQUOTED(HAVE_GAS_SECTION_EXCLUDE,
  [`if test $gcc_cv_as_section_exclude_e = yes || test $gcc_cv_as_section_exclude_hash = yes; then echo 1; else echo 0; fi`],
[Define if your assembler supports specifying the exclude section flag.])

# Test if the assembler supports the section flag 'R' for specifying
# section with SHF_GNU_RETAIN.
case "${target}" in
  # Solaris may use GNU assembler with Solairs ld.  Even if GNU
  # assembler supports the section flag 'R', it doesn't mean that
  # Solairs ld supports it.
  *-*-solaris2*)
    gcc_cv_as_shf_gnu_retain=no
    ;;
  *)
    gcc_GAS_CHECK_FEATURE([section 'R' flag], gcc_cv_as_shf_gnu_retain,
      [--fatal-warnings],
      [.section .foo,"awR",%progbits
.byte 0])
    ;;
esac
AC_DEFINE_UNQUOTED(HAVE_GAS_SHF_GNU_RETAIN,
  [`if test $gcc_cv_as_shf_gnu_retain = yes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports marking sections with SHF_GNU_RETAIN flag.])

# Test if the assembler supports the section flag 'o' for specifying
# section with link-order.
case "${target}" in
  # Solaris may use GNU assembler with Solairs ld.  Even if GNU
  # assembler supports the section flag 'o', it doesn't mean that
  # Solairs ld supports it.
  *-*-solaris2*)
    gcc_cv_as_section_link_order=no
    ;;
  *)
    gcc_GAS_CHECK_FEATURE([section 'o' flag], gcc_cv_as_section_link_order,
      [--fatal-warnings],
      [.section .foo,"a"
.byte 0
.section __patchable_function_entries,"awo",%progbits,.foo
.byte 0])
    ;;
esac
AC_DEFINE_UNQUOTED(HAVE_GAS_SECTION_LINK_ORDER,
  [`if test $gcc_cv_as_section_link_order = yes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your assembler supports 'o' flag in .section directive.])

gcc_GAS_CHECK_FEATURE(section merging support, gcc_cv_as_shf_merge,
 [--fatal-warnings],
 [.section .rodata.str, "aMS", @progbits, 1])
if test $gcc_cv_as_shf_merge = no; then
  gcc_GAS_CHECK_FEATURE(section merging support (%progbits),
    gcc_cv_as_shf_merge_percent,
    [--fatal-warnings],
    [.section .rodata.str, "aMS", %progbits, 1])
fi
AC_DEFINE_UNQUOTED(HAVE_GAS_SHF_MERGE,
  [`if test $gcc_cv_as_shf_merge = yes \
    || test $gcc_cv_as_shf_merge_percent = yes; then echo 1; else echo 0; fi`],
[Define 0/1 if your assembler supports marking sections with SHF_MERGE flag.])

gcc_GAS_CHECK_FEATURE([COMDAT group support (GNU as)],
 gcc_cv_as_comdat_group,
 [--fatal-warnings],
 [.section .text,"axG",@progbits,.foo,comdat])
if test $gcc_cv_as_comdat_group = yes; then
  gcc_cv_as_comdat_group_percent=no
  gcc_cv_as_comdat_group_group=no
else
 gcc_GAS_CHECK_FEATURE([COMDAT group support (GNU as, %type)],
   gcc_cv_as_comdat_group_percent,
   [--fatal-warnings],
   [.section .text,"axG",%progbits,.foo,comdat])
 if test $gcc_cv_as_comdat_group_percent = yes; then
   gcc_cv_as_comdat_group_group=no
 else
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
         i?86-*-solaris2* | x86_64-*-solaris2*)
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
         , [$conftest_s])
       ;;
   esac
   if test -z "${gcc_cv_as_comdat_group_group+set}"; then
     gcc_cv_as_comdat_group_group=no
   fi
 fi
fi
if test x"$ld_is_gold" = xyes; then
  comdat_group=yes
elif test x"$ld_is_mold" = xyes; then
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

gcc_GAS_CHECK_FEATURE([line table is_stmt support],
 gcc_cv_as_is_stmt,,
[	.text
	.file 1 "conf.c"
	.loc 1 1 0 is_stmt 1],,
[AC_DEFINE(HAVE_GAS_LOC_STMT, 1,
  [Define if your assembler supports the .loc is_stmt sub-directive.])])

gcc_GAS_CHECK_FEATURE([line table discriminator support],
 gcc_cv_as_discriminator,,
[	.text
	.file 1 "conf.c"
	.loc 1 1 0 discriminator 1],,
[AC_DEFINE(HAVE_GAS_DISCRIMINATOR, 1,
  [Define if your assembler supports the .loc discriminator sub-directive.])])

# Catch the newlib flag of the same name so we can gate GCC features on it.
AC_ARG_ENABLE(newlib-nano-formatted-io,
[AS_HELP_STRING([--enable-newlib-nano-formatted-io], [Use nano version
 formatted IO])],
[case "${enableval}" in
  yes|no)
    ;;
  *)
    AC_MSG_ERROR([unknown newlib-nano-formatted-io setting $enableval])
    ;;
esac], [])

# Thread-local storage - the check is heavily parameterized.
conftest_s=
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
	tls_as_opt=--fatal-warnings
	;;
  arc*-*-*)
    conftest_s='
	add_s r0,r0, @foo@tpoff'
	;;
  cris-*-*|crisv32-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
x:      .long   25
        .text
	move.d x:IE,$r10
	nop'
	tls_as_opt=--fatal-warnings
	;;
  frv*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
x:      .long   25
        .text
        call    #gettlsoff(x)'
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
	;;
  i[34567]86-*-* | x86_64-*-*)
    case "$target" in
      i[34567]86-*-solaris2.* | x86_64-*-solaris2.*)
	on_solaris=yes
        ;;
      *)
	on_solaris=no
	;;
    esac
    if test x$on_solaris = xyes && test x$gas_flag = xno; then
      conftest_s='
	.section .tdata,"awt",@progbits'
      tls_section_flag=t
changequote([,])dnl
      AC_DEFINE(TLS_SECTION_ASM_FLAG, 't',
[Define to the flag used to mark TLS sections if the default (`T') doesn't work.])
changequote(,)dnl
    else
      conftest_s='
	.section ".tdata","awT",@progbits'
      tls_section_flag=T
      tls_as_opt="--fatal-warnings"
    fi
    case "$target" in
      i[34567]86-*-*)
	if test x$on_solaris = xyes; then
	  case $gas_flag in
	    yes) tls_as_opt="$tls_as_opt --32" ;;
	  esac
	fi
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
	tls_as_opt=--fatal-warnings
	;;
  loongarch*-*-*)
    conftest_s='
	.section .tdata,"awT",@progbits
x:	.word 2
	.text
	la.tls.gd $a0,x
	bl __tls_get_addr'
	tls_first_major=0
	tls_first_minor=0
	tls_as_opt='--fatal-warnings'
	;;
  microblaze*-*-*)
    conftest_s='
	.section .tdata,"awT",@progbits
x:
	.word 2
	.text
	addik r5,r20,x@TLSGD
	addik r5,r20,x@TLSLDM'
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
	tls_as_opt='--fatal-warnings'
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
	tls_as_opt='--fatal-warnings'
	;;
  or1k*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	l.movhi	r3, tpoffha(foo)
	l.add	r3, r3, r10
	l.lwz	r4, tpofflo(foo)(r3)'
    tls_as_opt=--fatal-warnings
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
	tls_as_opt="-a32 --fatal-warnings"
	;;
  riscv*-*-*)
    conftest_s='
	.section .tdata,"awT",@progbits
x:	.word 2
	.text
	la.tls.gd a0,x
        call __tls_get_addr'
	tls_as_opt='--fatal-warnings'
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
	tls_as_opt="-m64 -Aesame --fatal-warnings"
	;;
  sh-*-* | sh[123456789lbe]*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	.long	foo@TLSGD
	.long	foo@TLSLDM
	.long	foo@DTPOFF
	.long	foo@GOTTPOFF
	.long	foo@TPOFF'
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
	.section ".tdata",#alloc,#write,#tls'
    else
      conftest_s='
	.section ".tdata","awT",@progbits'
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
  xtensa*-*-*)
    conftest_s='
	.section ".tdata","awT",@progbits
foo:	.long	25
	.text
	movi	a8, foo@TLSFUNC
	movi	a10, foo@TLSARG
	callx8.tls a8, foo@TLSCALL'
	;;
changequote([,])dnl
esac
set_have_as_tls=no
if test "x$enable_tls" = xno ; then
  : # TLS explicitly disabled.
elif test "x$enable_tls" = xyes ; then
  set_have_as_tls=yes # TLS explicitly enabled.
elif test -z "$conftest_s"; then
  : # If we don't have a check, assume no support.
else
  gcc_GAS_CHECK_FEATURE(thread-local storage support, gcc_cv_as_tls,
  [$tls_as_opt], [$conftest_s],,
  [set_have_as_tls=yes])
fi
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
  if $gcc_cv_ld --help 2>&1 | grep -- -Bstatic > /dev/null \
     && $gcc_cv_ld --help 2>&1 | grep -- -Bdynamic > /dev/null; then
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

AC_MSG_CHECKING(linker --version-script option)
gcc_cv_ld_version_script=no
ld_version_script_option=''
if test $in_tree_ld = yes || test x"$gnu_ld" = xyes; then
  gcc_cv_ld_version_script=yes
  ld_version_script_option='--version-script'
elif test x$gcc_cv_ld != x; then
  case "$target" in
    # Solaris 2 ld always supports -M.  It also supports a subset of
    # --version-script since Solaris 11.4, but requires
    # -z gnu-version-script-compat to activate.
    *-*-solaris2*)
      gcc_cv_ld_version_script=yes
      ld_version_script_option='-M'
      ;;
  esac
fi
# Don't AC_DEFINE result, only used in jit/Make-lang.in so far.
AC_MSG_RESULT($gcc_cv_ld_version_script)
AC_SUBST(ld_version_script_option)

AC_MSG_CHECKING(linker soname option)
gcc_cv_ld_soname=no
if test $in_tree_ld = yes || test x"$gnu_ld" = xyes; then
  gcc_cv_ld_soname=yes
  ld_soname_option='-soname'
elif test x$gcc_cv_ld != x; then
  case "$target" in
    *-*-darwin*)
      gcc_cv_ld_soname=yes
      ld_soname_option='-install_name'
      ;;
    # Solaris 2 ld always supports -h.  It also supports --soname for GNU
    # ld compatiblity since some Solaris 10 update.
    *-*-solaris2*)
      gcc_cv_ld_soname=yes
      ld_soname_option='-h'
      ;;
  esac
fi
# Don't AC_DEFINE result, only used in jit/Make-lang.in so far.
AC_MSG_RESULT($gcc_cv_ld_soname)
AC_SUBST(ld_soname_option)

if test x"$demangler_in_ld" = xyes; then
  AC_MSG_CHECKING(linker --demangle support)
  gcc_cv_ld_demangle=no
  if test $in_tree_ld = yes; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 14 -o "$gcc_cv_gld_major_version" -gt 2; then \
      gcc_cv_ld_demangle=yes
    fi
  elif test x$gcc_cv_ld != x -a x"$gnu_ld" = xyes; then
    # Check if the GNU linker supports --demangle option
    if $gcc_cv_ld --help 2>&1 | grep no-demangle > /dev/null; then
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
    elif test "$ld_is_mold" = yes; then
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

# Target OS-specific assembler checks.

case "$target_os" in
  darwin*)
    gcc_GAS_CHECK_FEATURE([-mmacosx-version-min option],
      gcc_cv_as_mmacosx_version_min,
      [-mmacosx-version-min=10.1], [.text],,
      [AC_DEFINE(HAVE_AS_MMACOSX_VERSION_MIN_OPTION, 1,
	[Define if your macOS assembler supports the -mmacos-version-min option.])])
    if test x$gcc_cv_as_mmacosx_version_min = "xyes"; then
       gcc_GAS_CHECK_FEATURE([.build_version],
           gcc_cv_as_darwin_build_version,
           [-mmacosx-version-min=10.14],
           [ .build_version macos, 10, 14 sdk_version 10, 14],,
      [AC_DEFINE(HAVE_AS_MACOS_BUILD_VERSION, 1,
	[Define if your macOS assembler supports .build_version directives])])
    fi
    ;;
esac

# Target CPU-specific assembler checks.

case "$target" in
  aarch64*-*-*)
    gcc_GAS_CHECK_FEATURE([-mabi option], gcc_cv_as_aarch64_mabi,
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
    # Check if we have binutils support for relocations types needed by -fpic
    gcc_GAS_CHECK_FEATURE([-fpic relocs], gcc_cv_as_aarch64_picreloc,,
    [
	.text
	ldr     x0, [[x2, #:gotpage_lo15:globalsym]]
    ],,[AC_DEFINE(HAVE_AS_SMALL_PIC_RELOCS, 1,
	[Define if your assembler supports relocs needed by -fpic.])])
    # Enable Branch Target Identification Mechanism and Return Address
    # Signing by default.
    AC_ARG_ENABLE(standard-branch-protection,
    [
AS_HELP_STRING([--enable-standard-branch-protection],
        [enable Branch Target Identification Mechanism, Return Address Signing, and Guarded Control Stack by default for AArch64])
AS_HELP_STRING([--disable-standard-branch-protection],
        [disable Branch Target Identification Mechanism, Return Address Signing, and Guarded Control Stack by default for AArch64])
    ],
      [
        case $enableval in
          yes)
            tm_defines="${tm_defines} TARGET_ENABLE_BTI=1 TARGET_ENABLE_PAC_RET=1 TARGET_ENABLE_GCS=1"
            ;;
          no)
            ;;
          *)
            AC_MSG_ERROR(['$enableval' is an invalid value for --enable-standard-branch-protection.\
  Valid choices are 'yes' and 'no'.])
            ;;
        esac
      ],
    [])
    # Enable default workaround for AArch64 Cortex-A53 erratum 835769.
    AC_ARG_ENABLE(fix-cortex-a53-835769,
    [
AS_HELP_STRING([--enable-fix-cortex-a53-835769],
        [enable workaround for AArch64 Cortex-A53 erratum 835769 by default])
AS_HELP_STRING([--disable-fix-cortex-a53-835769],
        [disable workaround for AArch64 Cortex-A53 erratum 835769 by default])
    ],
      [
        case $enableval in
          yes)
            tm_defines="${tm_defines} TARGET_FIX_ERR_A53_835769_DEFAULT=1"
            ;;
          no)
            ;;
          *)
            AC_MSG_ERROR(['$enableval' is an invalid value for --enable-fix-cortex-a53-835769.\
  Valid choices are 'yes' and 'no'.])
            ;;

        esac
      ],
    [])
    # Enable default workaround for AArch64 Cortex-A53 erratum 843419.
    AC_ARG_ENABLE(fix-cortex-a53-843419,
    [
AS_HELP_STRING([--enable-fix-cortex-a53-843419],
        [enable workaround for AArch64 Cortex-A53 erratum 843419 by default])
AS_HELP_STRING([--disable-fix-cortex-a53-843419],
        [disable workaround for AArch64 Cortex-A53 erratum 843419 by default])
    ],
      [
        case $enableval in
          yes)
            tm_defines="${tm_defines} TARGET_FIX_ERR_A53_843419_DEFAULT=1"
            ;;
          no)
            ;;
          *)
            AC_MSG_ERROR(['$enableval' is an invalid value for --enable-fix-cortex-a53-843419.\
  Valid choices are 'yes' and 'no'.])
            ;;

        esac
      ],
    [])
    ;;

  # All TARGET_ABI_OSF targets.
  alpha*-*-linux* | alpha*-*-*bsd*)
    gcc_GAS_CHECK_FEATURE([explicit relocation support],
	gcc_cv_as_alpha_explicit_relocs,,
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
	gcc_cv_as_alpha_jsrdirect_relocs,,
[	.set nomacro
	.text
	ldq	$27, a($29)	!literal!1
	jsr	$26, ($27), a	!lituse_jsrdirect!1],,
    [AC_DEFINE(HAVE_AS_JSRDIRECT_RELOCS, 1,
  [Define if your assembler supports the lituse_jsrdirect relocation.])])
    ;;

  avr-*-*)
    gcc_GAS_CHECK_FEATURE([--mlink-relax option], gcc_cv_as_avr_mlink_relax,
      [--mlink-relax], [.text],,
      [AC_DEFINE(HAVE_AS_AVR_MLINK_RELAX_OPTION, 1,
		[Define if your avr assembler supports --mlink-relax option.])])

    gcc_GAS_CHECK_FEATURE([-mrmw option], gcc_cv_as_avr_mrmw,
      [-mrmw], [.text],,
      [AC_DEFINE(HAVE_AS_AVR_MRMW_OPTION, 1,
		[Define if your avr assembler supports -mrmw option.])])

    gcc_GAS_CHECK_FEATURE([__gcc_isr pseudo instruction],
      gcc_cv_as_avr_mgccisr,
      [-mgcc-isr], [.text
      		    __gcc_isr 1
      		    __gcc_isr 2
      		    __gcc_isr 0,r24
      		   ],,
      [AC_DEFINE(HAVE_AS_AVR_MGCCISR_OPTION, 1,
		[Define if your avr assembler supports -mgcc-isr option.])])

    avr_ld_ver="`$gcc_cv_ld -v | sed -e 's:^.* ::'`"
    # Check how default linker description file implements .rodata for
    # avrxmega3 (PR21472).  avr-gcc assumes .rodata is *not* loaded to
    # RAM so avr-gcc skips __do_copy_data for .rodata objects.
    AC_MSG_CHECKING(binutils for avrxmega3 .rodata support)
    cat > conftest.s <<EOF
        .section .rodata,"a",@progbits
        .global xxvaryy
    ;; avr-nm should print "... R xxvaryy", not "... D xxvaryy".
    xxvaryy:
        .word 1
EOF
    rm -f conftest.nm
    AC_TRY_COMMAND([$gcc_cv_as -mmcu=avrxmega3 conftest.s -o conftest.o])
    AC_TRY_COMMAND([$gcc_cv_ld -mavrxmega3 conftest.o -o conftest.elf])
    AC_TRY_COMMAND([$gcc_cv_nm conftest.elf > conftest.nm])
    if test -s conftest.nm
    then
	if grep ' R xxvaryy' conftest.nm > /dev/null; then
	    AC_MSG_RESULT(yes)
	    AC_DEFINE(HAVE_LD_AVR_AVRXMEGA3_RODATA_IN_FLASH, 1,
		[Define if your default avr linker script for avrxmega3 leaves .rodata in flash.])
	else
	    AC_MSG_RESULT(no: avrxmega3 .rodata located in RAM)
	    echo "$as_me: nm output was" >&AS_MESSAGE_LOG_FD
	    cat conftest.nm >&AS_MESSAGE_LOG_FD
	    AC_MSG_WARN([[support for avrxmega3 .rodata in flash needs Binutils 2.29 or higher (have $avr_ld_ver)]])
	fi
    else
	AC_MSG_RESULT(test failed)
	echo "$as_me: failed program was" >&AS_MESSAGE_LOG_FD
	cat conftest.s >&AS_MESSAGE_LOG_FD
	AC_MSG_WARN([[see `config.log' for details]])
    fi
    rm -f conftest.s conftest.o conftest.elf conftest.nm

    # Check for emulation avrxmega2_flmap.
    AC_MSG_CHECKING(binutils for avrxmega2_flmap support (PR31124))
    cat > conftest.s <<EOF
        .section .text,"ax",@progbits
        ldi r16, __flmap_value_with_lock
EOF
    AC_TRY_COMMAND([$gcc_cv_as -mmcu=avrxmega2 conftest.s -o conftest.o])
    AC_TRY_COMMAND([$gcc_cv_ld -mavrxmega2_flmap conftest.o -o conftest.elf])
    if test -s conftest.elf
    then
	AC_MSG_RESULT(yes)
	AC_DEFINE(HAVE_LD_AVR_AVRXMEGA2_FLMAP, 1,
	    [Define if your linker supports emulation avrxmega2_flmap.])
    else
	AC_MSG_RESULT(no)
	AC_MSG_WARN([[support for avrxmega2_flmap requires Binutils 2.42 or higher (have $avr_ld_ver)]])
	AC_MSG_WARN([[see `config.log' for details]])
    fi
    rm -f conftest.o conftest.elf

    # Check for emulation avrxmega4_flmap.
    AC_MSG_CHECKING(binutils for avrxmega4_flmap support (PR31124))
    AC_TRY_COMMAND([$gcc_cv_as -mmcu=avrxmega4 conftest.s -o conftest.o])
    AC_TRY_COMMAND([$gcc_cv_ld -mavrxmega4_flmap conftest.o -o conftest.elf])
    if test -s conftest.elf
    then
	AC_MSG_RESULT(yes)
	AC_DEFINE(HAVE_LD_AVR_AVRXMEGA4_FLMAP, 1,
	    [Define if your linker supports emulation avrxmega4_flmap.])
    else
	AC_MSG_RESULT(no)
	AC_MSG_WARN([[support for avrxmega4_flmap requires Binutils 2.42 or higher (have $avr_ld_ver)]])
	AC_MSG_WARN([[see `config.log' for details]])
    fi
    rm -f conftest.s conftest.o conftest.elf
    ;;

  cris-*-*)
    gcc_GAS_CHECK_FEATURE([-no-mul-bug-abort option],
      gcc_cv_as_cris_no_mul_bug,
      [-no-mul-bug-abort], [.text],,
      [AC_DEFINE(HAVE_AS_NO_MUL_BUG_ABORT_OPTION, 1,
		[Define if your assembler supports the -no-mul-bug-abort option.])])
    ;;

  sparc*-*-*)
    gcc_GAS_CHECK_FEATURE([-relax option], gcc_cv_as_sparc_relax,
      [-relax], [.text],,
      [AC_DEFINE(HAVE_AS_RELAX_OPTION, 1,
		[Define if your assembler supports -relax option.])])

    gcc_GAS_CHECK_FEATURE([GOTDATA_OP relocs],
      gcc_cv_as_sparc_gotdata_op,
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
      gcc_cv_as_sparc_ua_pcrel,
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
	gcc_cv_as_sparc_ua_pcrel_hidden,
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
      gcc_cv_as_sparc_offsetable_lo10,
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
      gcc_cv_as_sparc_fmaf,
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
      gcc_cv_as_sparc_sparc4,
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

    gcc_GAS_CHECK_FEATURE([SPARC5 and VIS 4.0 instructions],
      gcc_cv_as_sparc_sparc5,
      [-xarch=sparc5],
      [.text
       .register %g2, #scratch
       .register %g3, #scratch
       .align 4
       subxc %g1, %g2, %g3
       fpadd8 %f0, %f2, %f4],,
      [AC_DEFINE(HAVE_AS_SPARC5_VIS4, 1,
                [Define if your assembler supports SPARC5 and VIS 4.0 instructions.])])

    gcc_GAS_CHECK_FEATURE([SPARC6 instructions],
      gcc_cv_as_sparc_sparc6,
      [-xarch=sparc6],
      [.text
       .register %g2, #scratch
       .register %g3, #scratch
       .align 4
       rd %entropy, %g1
       fpsll64x %f0, %f2, %f4],,
      [AC_DEFINE(HAVE_AS_SPARC6, 1,
                [Define if your assembler supports SPARC6 instructions.])])

    gcc_GAS_CHECK_FEATURE([LEON instructions],
      gcc_cv_as_sparc_leon,
      [-Aleon],
      [.text
       .register %g2, #scratch
       .register %g3, #scratch
       .align 4
       smac %g2, %g3, %g1
       umac %g2, %g3, %g1
       casa [[%g2]] 0xb, %g3, %g1],,
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
      cygwin* | pe | mingw32*)
	# Recent binutils allows the three-operand form of ".comm" on PE.  This
	# definition is used unconditionally to initialise the default state of
	# the target option variable that governs usage of the feature.
	gcc_GAS_CHECK_FEATURE([.comm with alignment], gcc_cv_as_comm_has_align,,
	  [.comm foo,1,32])
	AC_DEFINE_UNQUOTED(HAVE_GAS_ALIGNED_COMM,
	  [`if test $gcc_cv_as_comm_has_align = yes; then echo 1; else echo 0; fi`],
	  [Define if your assembler supports specifying the alignment
	   of objects allocated using the GAS .comm command.])
	# Used for DWARF 2 in PE
	gcc_GAS_CHECK_FEATURE([.secrel32 relocs],
	  gcc_cv_as_ix86_pe_secrel32,,
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
	  -fatal-warnings,[.section lto_test,"dr0"])
	if test x$gcc_cv_as_section_has_align != xyes; then
	  case ",$enable_languages," in
	    *,lto,*)
	      AC_MSG_WARN([LTO for $target requires binutils >= 2.20.1, but version found appears insufficient; LTO will not work until binutils is upgraded.])
	      ;;
	  esac
	fi
	;;
    esac
    case $target_os in
       darwin2* | darwin19*)
        gcc_GAS_CHECK_FEATURE([llvm assembler x86-pad-for-align option],
          gcc_cv_as_mllvm_x86_pad_for_align,
          [-mllvm -x86-pad-for-align=false], [.text],,
          [AC_DEFINE(HAVE_AS_MLLVM_X86_PAD_FOR_ALIGN, 1,
	    [Define if your macOS assembler supports -mllvm -x86-pad-for-align=false.])])
       ;;
    esac

    gcc_GAS_CHECK_FEATURE([-xbrace_comment], gcc_cv_as_ix86_xbrace_comment,
      [-xbrace_comment=no], [.text],,
      [AC_DEFINE(HAVE_AS_XBRACE_COMMENT_OPTION, 1,
		[Define if your assembler supports -xbrace_comment option.])])

    gcc_GAS_CHECK_FEATURE([filds and fists mnemonics],
       gcc_cv_as_ix86_filds,,
       [filds (%ebp); fists (%ebp)],,
       [AC_DEFINE(HAVE_AS_IX86_FILDS, 1,
         [Define if your assembler uses filds and fists mnemonics.])])

    gcc_GAS_CHECK_FEATURE([fildq and fistpq mnemonics],
       gcc_cv_as_ix86_fildq,,
       [fildq (%ebp); fistpq (%ebp)],,
       [AC_DEFINE(HAVE_AS_IX86_FILDQ, 1,
         [Define if your assembler uses fildq and fistq mnemonics.])])

    gcc_GAS_CHECK_FEATURE([cmov syntax],
      gcc_cv_as_ix86_cmov_sun_syntax,,
      [cmovl.l %edx, %eax],,
      [AC_DEFINE(HAVE_AS_IX86_CMOV_SUN_SYNTAX, 1,
        [Define if your assembler supports the Sun syntax for cmov.])])

    gcc_GAS_CHECK_FEATURE([ffreep mnemonic],
      gcc_cv_as_ix86_ffreep,,
      [ffreep %st(1)],,
      [AC_DEFINE(HAVE_AS_IX86_FFREEP, 1,
        [Define if your assembler supports the ffreep mnemonic.])])

    gcc_GAS_CHECK_FEATURE([.quad directive],
      gcc_cv_as_ix86_quad,,
      [.quad 0],,
      [AC_DEFINE(HAVE_AS_IX86_QUAD, 1,
        [Define if your assembler supports the .quad directive.])])

    gcc_GAS_CHECK_FEATURE([sahf mnemonic],
      gcc_cv_as_ix86_sahf,,
      [.code64
       sahf],,
      [AC_DEFINE(HAVE_AS_IX86_SAHF, 1,
        [Define if your assembler supports the sahf mnemonic in 64bit mode.])])

    gcc_GAS_CHECK_FEATURE([interunit movq mnemonic],
      gcc_cv_as_ix86_interunit_movq,,
      [.code64
       movq %mm0, %rax
       movq %rax, %xmm0])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_INTERUNIT_MOVQ,
      [`if test $gcc_cv_as_ix86_interunit_movq = yes; then echo 1; else echo 0; fi`],
      [Define if your assembler supports interunit movq mnemonic.])

    gcc_GAS_CHECK_FEATURE([hle prefixes],
      gcc_cv_as_ix86_hle,,
      [lock xacquire cmpxchg %esi, (%ecx)],,
      [AC_DEFINE(HAVE_AS_IX86_HLE, 1,
        [Define if your assembler supports HLE prefixes.])])

    gcc_GAS_CHECK_FEATURE([swap suffix],
      gcc_cv_as_ix86_swap,,
      [movl.s %esp, %ebp],,
      [AC_DEFINE(HAVE_AS_IX86_SWAP, 1,
        [Define if your assembler supports the swap suffix.])])

    gcc_GAS_CHECK_FEATURE([different section symbol subtraction],
      gcc_cv_as_ix86_diff_sect_delta,,
      [.section .rodata
.L1:
        .long .L2-.L1
        .long .L3-.L1
        .text
.L3:    nop
.L2:    nop],,
      [AC_DEFINE(HAVE_AS_IX86_DIFF_SECT_DELTA, 1,
        [Define if your assembler supports the subtraction of symbols in different sections.])])

    gcc_GAS_CHECK_FEATURE([rep and lock prefix],
        gcc_cv_as_ix86_rep_lock_prefix,,
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
	gcc_cv_as_ix86_ud2,,
	[ud2],,
      [AC_DEFINE(HAVE_AS_IX86_UD2, 1,
	[Define if your assembler supports the 'ud2' mnemonic.])])

    # Enforce 32-bit output with gas and gld.
    if test x$gas = xyes; then
      as_ix86_gas_32_opt="--32"
    fi
    if echo "$ld_ver" | grep GNU > /dev/null; then
      if $gcc_cv_ld -V 2>/dev/null | grep elf_i386_sol2 > /dev/null; then
        ld_ix86_gld_32_opt="-melf_i386_sol2"
      else
        ld_ix86_gld_32_opt="-melf_i386"
      fi
    fi

    gcc_GAS_CHECK_FEATURE([R_386_TLS_GD_PLT reloc],
        gcc_cv_as_ix86_tlsgdplt,
	[$as_ix86_gas_32_opt],
	[call    tls_gd@tlsgdplt],
	[if test x$gcc_cv_ld != x \
	 && $gcc_cv_ld $ld_ix86_gld_32_opt -o conftest conftest.o -G > /dev/null 2>&1; then
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
        gcc_cv_as_ix86_tlsldmplt,
	[$as_ix86_gas_32_opt],
	[$conftest_s],
	[if test x$gcc_cv_ld != x \
	 && $gcc_cv_ld $ld_ix86_gld_32_opt -o conftest conftest.o -G > /dev/null 2>&1; then
	   gcc_cv_as_ix86_tlsldmplt=yes
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_TLSLDMPLT,
      [`if test $gcc_cv_as_ix86_tlsldmplt = yes; then echo 1; else echo 0; fi`],
      [Define to 1 if your assembler and linker support @tlsldmplt.])

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
        gcc_cv_as_ix86_tlsldm,
	[$as_ix86_gas_32_opt],
	[$conftest_s],
	[if test x$gcc_cv_ld != x && test x$gcc_cv_objdump != x \
	    && $gcc_cv_ld $ld_ix86_gld_32_opt -o conftest conftest.o $ld_tls_libs -lc > /dev/null 2>&1; then
	   if $gcc_cv_objdump -d conftest 2>/dev/null | grep nop > /dev/null \
	      || dis conftest 2>/dev/null | grep nop > /dev/null; then
	     gcc_cv_as_ix86_tlsldm=yes
	   fi
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_TLSLDM,
      [`if test $gcc_cv_as_ix86_tlsldm = yes; then echo 1; else echo 0; fi`],
      [Define to 1 if your assembler and linker support @tlsldm.])

    conftest_s='
	.data
bar:
	.byte 1
	.text
	.global _start
_start:
	 cmpl $0, bar@GOT
	 jmp *_start@GOT'
    gcc_GAS_CHECK_FEATURE([R_386_GOT32X reloc],
        gcc_cv_as_ix86_got32x,
	[$as_ix86_gas_32_opt],
	[$conftest_s],
	[if test x$gcc_cv_ld != x && test x$gcc_cv_objdump != x \
	    && test x$gcc_cv_readelf != x \
	    && $gcc_cv_readelf --relocs --wide conftest.o 2>&1 \
	       | grep R_386_GOT32X > /dev/null 2>&1 \
	    && $gcc_cv_ld $ld_ix86_gld_32_opt -o conftest conftest.o > /dev/null 2>&1; then
	   if $gcc_cv_objdump -dw conftest 2>&1 \
	      | grep 0xffffff > /dev/null 2>&1; then
	     gcc_cv_as_ix86_got32x=no
	   else
	     gcc_cv_as_ix86_got32x=yes
	   fi
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_GOT32X,
      [`if test x"$gcc_cv_as_ix86_got32x" = xyes; then echo 1; else echo 0; fi`],
      [Define 0/1 if your assembler and linker support @GOT.])

    # Check if gas and gld support "addq %r23,foo@GOTTPOFF(%rip), %r15"
    # with R_X86_64_CODE_6_GOTTPOFF relocation.
    if echo "$ld_ver" | grep GNU > /dev/null; then
      if $gcc_cv_ld -V 2>/dev/null | grep elf_x86_64_sol2 > /dev/null; then
        ld_ix86_gld_64_opt="-melf_x86_64_sol2"
      else
        ld_ix86_gld_64_opt="-melf_x86_64"
      fi
    fi
    # Enforce 64-bit output with gas and gld.
    if test x$gas = xyes; then
      as_ix86_gas_64_opt="--64"
    fi
    conftest_s='
	.text
	.globl	_start
	.type _start, @function
_start:
	addq	%r23,foo@GOTTPOFF(%rip), %r15
	.section .tdata,"awT",@progbits
	.type foo, @object
foo:
	.quad 0'
    gcc_GAS_CHECK_FEATURE([R_X86_64_CODE_6_GOTTPOFF reloc],
	gcc_cv_as_x86_64_code_6_gottpoff,
	[$as_ix86_gas_64_opt],
	[$conftest_s],
	[if test x$gcc_cv_ld != x && test x$gcc_cv_objdump != x \
	    && test x$gcc_cv_readelf != x \
	    && $gcc_cv_readelf --relocs --wide conftest.o 2>&1 \
	       | grep R_X86_64_CODE_6_GOTTPOFF > /dev/null 2>&1 \
	    && $gcc_cv_ld $ld_ix86_gld_64_opt -o conftest conftest.o > /dev/null 2>&1; then
	   if $gcc_cv_objdump -dw conftest 2>&1 \
	      | grep "add \+\$0xf\+8,%r23,%r15" > /dev/null 2>&1; then
	     gcc_cv_as_x86_64_code_6_gottpoff=yes
	   else
	     gcc_cv_as_x86_64_code_6_gottpoff=no
	   fi
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_R_X86_64_CODE_6_GOTTPOFF,
      [`if test x"$gcc_cv_as_x86_64_code_6_gottpoff" = xyes; then echo 1; else echo 0; fi`],
      [Define 0/1 if your assembler and linker support R_X86_64_CODE_6_GOTTPOFF.])

    gcc_GAS_CHECK_FEATURE([GOTOFF in data],
      gcc_cv_as_ix86_gotoff_in_data,
      [$as_ix86_gas_32_opt],
[	.text
.L0:
	nop
	.data
	.long .L0@GOTOFF])
    AC_DEFINE_UNQUOTED(HAVE_AS_GOTOFF_IN_DATA,
      [`if test $gcc_cv_as_ix86_gotoff_in_data = yes; then echo 1; else echo 0; fi`],
      [Define true if the assembler supports '.long foo@GOTOFF'.])

    conftest_s='
	.section .text,"ax",@progbits
	.globl  _start
	.type   _start, @function
_start:
	leal	ld@tlsldm(%ecx), %eax
	call	*___tls_get_addr@GOT(%ecx)
	leal	gd@tlsgd(%ecx), %eax
	call	*___tls_get_addr@GOT(%ecx)

	.section .tdata,"aw'$tls_section_flag'",@progbits
	.type	ld, @object
ld:
	.byte 0
	.globl  gd
	.type	gd, @object
gd:
	.byte 0'
    gcc_GAS_CHECK_FEATURE([calling ___tls_get_addr via GOT],
        gcc_cv_as_ix86_tls_get_addr_via_got,
	[$as_ix86_gas_32_opt],
	[$conftest_s],
	[if test x$gcc_cv_ld != x \
	    && $gcc_cv_ld $ld_ix86_gld_32_opt -o conftest conftest.o > /dev/null 2>&1; then
	   gcc_cv_as_ix86_tls_get_addr_via_got=yes
	 fi
	 rm -f conftest])
    AC_DEFINE_UNQUOTED(HAVE_AS_IX86_TLS_GET_ADDR_GOT,
      [`if test x"$gcc_cv_as_ix86_tls_get_addr_via_got" = xyes; then echo 1; else echo 0; fi`],
      [Define 0/1 if your assembler and linker support calling ___tls_get_addr via GOT.])
    ;;

  ia64*-*-*)
    gcc_GAS_CHECK_FEATURE([ltoffx and ldxmov relocs],
	gcc_cv_as_ia64_ltoffx_ldxmov_relocs,,
[	.text
	addl r15 = @ltoffx(x#), gp
	;;
	ld8.mov r16 = [[r15]], x#
],,
    [AC_DEFINE(HAVE_AS_LTOFFX_LDXMOV_RELOCS, 1,
	  [Define if your assembler supports ltoffx and ldxmov relocations.])])

    ;;

  powerpc*-*-*)

    case $target in
      *-*-darwin*)
	gcc_GAS_CHECK_FEATURE([.machine directive support],
	  gcc_cv_as_machine_directive,,
	  [	.machine ppc7400])
	if test x$gcc_cv_as_machine_directive != xyes; then
	  echo "*** This target requires an assembler supporting \".machine\"" >&2
	  echo you can get it from: https://gcc.gnu.org/pub/gcc/infrastructure/cctools-528.5.dmg >&2
	  test x$build = x$target && exit 1
	fi
        ;;
    esac

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr5"
	.csect .text[[PR]]
	mfcr 3,128';;
      *-*-darwin*) conftest_s='	.text
	mfcr r3,128';;
      *) conftest_s='	.machine power4
	.text
	mfcr 3,128';;
    esac

    gcc_GAS_CHECK_FEATURE([mfcr field support],
      gcc_cv_as_powerpc_mfcrf,,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_MFCRF, 1,
	  [Define if your assembler supports mfcr field.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr10"
	.csect .text[[PR]]
	tend. 0';;
      *-*-darwin*) conftest_s='	.text
	tend. 0';;
      *) conftest_s='	.machine power10
	.text
	tend. 0';;
    esac

    gcc_GAS_CHECK_FEATURE([htm support on Power10],
      gcc_cv_as_power10_htm,,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_POWER10_HTM, 1,
	  [Define if your assembler supports htm insns on power10.])])

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
      gcc_cv_as_powerpc_rel16, -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_REL16, 1,
	  [Define if your assembler supports R_PPC_REL16 relocs.])])

    case $target in
      *-*-aix*) conftest_s='	.machine "pwr7"
	.csect .text[[PR]]
	lxvd2x 1,2,3';;
      *) conftest_s='	.machine power7
	.text
	lxvd2x 1,2,3';;
    esac

    gcc_GAS_CHECK_FEATURE([vector-scalar support],
      gcc_cv_as_powerpc_vsx, -a32,
      [$conftest_s],,
      [AC_DEFINE(HAVE_AS_VSX, 1,
	  [Define if your assembler supports VSX instructions.])])

    gcc_GAS_CHECK_FEATURE([.gnu_attribute support],
      gcc_cv_as_powerpc_gnu_attribute,,
      [.gnu_attribute 4,1],,
      [AC_DEFINE(HAVE_AS_GNU_ATTRIBUTE, 1,
	  [Define if your assembler supports .gnu_attribute.])])

    gcc_GAS_CHECK_FEATURE([prologue entry point marker support],
      gcc_cv_as_powerpc_entry_markers,-a64 --fatal-warnings,
      [ .reloc .,R_PPC64_ENTRY; nop],,
      [AC_DEFINE(HAVE_AS_ENTRY_MARKERS, 1,
	  [Define if your assembler supports the R_PPC64_ENTRY relocation.])])

    gcc_GAS_CHECK_FEATURE([plt sequence marker support],
      gcc_cv_as_powerpc_pltseq_markers,-a32 --fatal-warnings,
      [ .reloc .,R_PPC_PLTSEQ; nop],,
      [AC_DEFINE(HAVE_AS_PLTSEQ, 1,
	  [Define if your assembler supports R_PPC*_PLTSEQ relocations.])])

    case $target in
      *-*-aix*)
	gcc_GAS_CHECK_FEATURE([AIX .ref support],
	  gcc_cv_as_aix_ref,,
	  [	.csect stuff[[rw]]
	     stuff:
		.long 1
		.extern sym
		.ref sym
	  ],,
	  [AC_DEFINE(HAVE_AS_REF, 1,
	    [Define if your assembler supports .ref])])

	gcc_GAS_CHECK_FEATURE([AIX DWARF location lists section support],
	  gcc_cv_as_aix_dwloc,,
	  [	.dwsect 0xA0000
	Lframe..0:
		.vbyte 4,Lframe..0
	  ],,
	  [AC_DEFINE(HAVE_XCOFF_DWARF_EXTRAS, 1,
	    [Define if your assembler supports AIX debug frame section label reference.])])
	;;
    esac
    ;;

  mips*-*-*)
    gcc_GAS_CHECK_FEATURE([explicit relocation support: %pcrel_hi/%pcrel_lo],
      gcc_cv_as_mips_explicit_relocs_pcrel,,
[	lui $4,%pcrel_hi(foo)],,
      [AC_DEFINE(MIPS_EXPLICIT_RELOCS, MIPS_EXPLICIT_RELOCS_PCREL,
		 [Define if assembler supports %pcrel_hi/%pcrel_lo.])])

    if test x$gcc_cv_as_mips_explicit_relocs_pcrel = xno; then \
    gcc_GAS_CHECK_FEATURE([explicit relocation support: %gp_rel],
      gcc_cv_as_mips_explicit_relocs,,
[	lw $4,%gp_rel(foo)($4)],,
      [AC_DEFINE(MIPS_EXPLICIT_RELOCS, MIPS_EXPLICIT_RELOCS_BASE,
		 [Define if assembler supports %reloc.])])
    else
      gcc_cv_as_mips_explicit_relocs=yes
    fi

    if test x$gcc_cv_as_mips_explicit_relocs = xno; then \
      AC_DEFINE(MIPS_EXPLICIT_RELOCS, MIPS_EXPLICIT_RELOCS_NONE,
		[Define if assembler supports %reloc.])
    fi

    gcc_GAS_CHECK_FEATURE([-mno-shared support],
      gcc_cv_as_mips_no_shared,[-mno-shared], [nop],,
      [AC_DEFINE(HAVE_AS_NO_SHARED, 1,
		 [Define if the assembler understands -mno-shared.])])

    gcc_GAS_CHECK_FEATURE([.gnu_attribute support],
      gcc_cv_as_mips_gnu_attribute,,
      [.gnu_attribute 4,1],,
      [AC_DEFINE(HAVE_AS_GNU_ATTRIBUTE, 1,
	  [Define if your assembler supports .gnu_attribute.])])

    gcc_GAS_CHECK_FEATURE([.module support],
      gcc_cv_as_mips_dot_module,[-32],
      [.module mips2
       .module fp=xx],,
      [AC_DEFINE(HAVE_AS_DOT_MODULE, 1,
	  [Define if your assembler supports .module.])])
    if test x$gcc_cv_as_mips_dot_module = xno \
       && test x$with_fp_32 != x; then
      AC_MSG_ERROR(
	[Requesting --with-fp-32= requires assembler support for .module.])
    fi

    gcc_GAS_CHECK_FEATURE([.micromips support],
      gcc_cv_as_micromips_support,[--fatal-warnings],
      [.set micromips],,
      [AC_DEFINE(HAVE_GAS_MICROMIPS, 1,
          [Define if your assembler supports the .set micromips directive])])

    gcc_GAS_CHECK_FEATURE([.dtprelword support],
      gcc_cv_as_mips_dtprelword,,
      [.section .tdata,"awT",@progbits
x:
	.word 2
	.text
	.dtprelword x+0x8000],,
      [AC_DEFINE(HAVE_AS_DTPRELWORD, 1,
	  [Define if your assembler supports .dtprelword.])])

    gcc_GAS_CHECK_FEATURE([DSPR1 mult with four accumulators support],
      gcc_cv_as_mips_dspr1_mult,,
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
        echo 'x:	lw $2,%got_disp(y)($3)' >> conftest.s
        echo '	lw $25,%call16(y)($28)' >> conftest.s
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
      gcc_cv_as_mips_nan,
      [-mnan=2008],,,
      [AC_DEFINE(HAVE_AS_NAN, 1,
		 [Define if the assembler understands -mnan=.])])
    if test x$gcc_cv_as_mips_nan = xno \
       && test x$with_nan != x; then
      AC_MSG_ERROR(
	[Requesting --with-nan= requires assembler support for -mnan=])
    fi
    ;;
    msp430-*-*)
    # Earlier GAS versions generically support .gnu_attribute, but the
    # msp430 assembler will not do anything with it.
    gcc_GAS_CHECK_FEATURE([.gnu_attribute support],
      gcc_cv_as_msp430_gnu_attribute,,
      [.gnu_attribute 4,1],,
      [AC_DEFINE(HAVE_AS_GNU_ATTRIBUTE, 1,
	  [Define if your assembler supports .gnu_attribute.])])
    gcc_GAS_CHECK_FEATURE([.mspabi_attribute support],
      gcc_cv_as_msp430_mspabi_attribute,,
      [.mspabi_attribute 4,2],,
      [AC_DEFINE(HAVE_AS_MSPABI_ATTRIBUTE, 1,
	  [Define if your assembler supports .mspabi_attribute.])])
    if test x$enable_newlib_nano_formatted_io = xyes; then
      AC_DEFINE(HAVE_NEWLIB_NANO_FORMATTED_IO, 1, [Define if GCC has been
configured with --enable-newlib-nano-formatted-io.])
      fi
    ;;
    riscv*-*-*)
    gcc_GAS_CHECK_FEATURE([.attribute support],
      gcc_cv_as_riscv_attribute,,
      [.attribute stack_align,4],,
      [AC_DEFINE(HAVE_AS_RISCV_ATTRIBUTE, 1,
	  [Define if your assembler supports .attribute.])])
    gcc_GAS_CHECK_FEATURE([-misa-spec= support],
      gcc_cv_as_riscv_isa_spec,
      [-misa-spec=2.2],,,
      [AC_DEFINE(HAVE_AS_MISA_SPEC, 1,
		 [Define if the assembler understands -misa-spec=.])])
    gcc_GAS_CHECK_FEATURE([-march=rv32i_zifencei support],
      gcc_cv_as_riscv_march_zifencei,
      [-march=rv32i_zifencei2p0],,,
      [AC_DEFINE(HAVE_AS_MARCH_ZIFENCEI, 1,
		 [Define if the assembler understands -march=rv*_zifencei.])])
    gcc_GAS_CHECK_FEATURE([-march=rv32i_zaamo_zalrsc support],
      gcc_cv_as_riscv_march_zaamo_zalrsc,
      [-march=rv32i_zaamo_zalrsc],,,
      [AC_DEFINE(HAVE_AS_MARCH_ZAAMO_ZALRSC, 1,
		 [Define if the assembler understands -march=rv*_zaamo_zalrsc.])])
    gcc_GAS_CHECK_FEATURE([-march=rv32i_b support],
      gcc_cv_as_riscv_march_b,
      [-march=rv32i_b],,,
      [AC_DEFINE(HAVE_AS_MARCH_B, 1,
		 [Define if the assembler understands -march=rv*_b.])])
    ;;
    loongarch*-*-*)
    gcc_GAS_CHECK_FEATURE([.dtprelword support],
      gcc_cv_as_loongarch_dtprelword, [2,18,0],,
      [.section .tdata,"awT",@progbits
x:
	.word 2
	.text
	.dtprelword x+0x8000],,
      [AC_DEFINE(HAVE_AS_DTPRELWORD, 1,
	  [Define if your assembler supports .dtprelword.])])
    gcc_GAS_CHECK_FEATURE([explicit relocation support],
      gcc_cv_as_loongarch_explicit_relocs,,
      [a:pcalau12i $t0,%pc_hi20(a)],,
      [AC_DEFINE(HAVE_AS_EXPLICIT_RELOCS, 1,
	  [Define if your assembler supports explicit relocation.])])
    gcc_GAS_CHECK_FEATURE([call36 relocation support],
      gcc_cv_as_loongarch_call36,,
      [pcaddu18i $r1, %call36(a)
       jirl $r1, $r1, 0],,
      [AC_DEFINE(HAVE_AS_SUPPORT_CALL36, 1,
	  [Define if your assembler supports call36 relocation.])])
    gcc_GAS_CHECK_FEATURE([eh_frame pcrel encoding support],
      gcc_cv_as_loongarch_eh_frame_pcrel_encoding_support,,
      [.cfi_startproc
       .cfi_personality 0x9b,a
       .cfi_lsda 0x1b,b
       .cfi_endproc],,
      [AC_DEFINE(HAVE_AS_EH_FRAME_PCREL_ENCODING_SUPPORT, 1,
	  [Define if your assembler supports eh_frame pcrel encoding.])])
    gcc_GAS_CHECK_FEATURE([-mrelax option], gcc_cv_as_loongarch_relax,
      [-mrelax], [.text],,
      [AC_DEFINE(HAVE_AS_MRELAX_OPTION, 1,
		[Define if your assembler supports -mrelax option.])])
    gcc_GAS_CHECK_FEATURE([conditional branch relaxation support],
      gcc_cv_as_loongarch_cond_branch_relax,
      [--fatal-warnings],
      [a:
       .rept 32769
       nop
       .endr
       beq $a0,$a1,a],,
      [AC_DEFINE(HAVE_AS_COND_BRANCH_RELAXATION, 1,
		[Define if your assembler supports conditional branch relaxation.])])
    gcc_GAS_CHECK_FEATURE([tls le relaxation support],
      gcc_cv_as_loongarch_tls_le_relaxation_support,,
      [lu12i.w $t0,%le_hi20_r(a)],,
      [AC_DEFINE(HAVE_AS_TLS_LE_RELAXATION, 1,
	  [Define if your assembler supports tls le relocation.])])
    ;;
    s390*-*-*)
    gcc_GAS_CHECK_FEATURE([.gnu_attribute support],
      gcc_cv_as_s390_gnu_attribute,,
      [.gnu_attribute 8,1],,
      [AC_DEFINE(HAVE_AS_GNU_ATTRIBUTE, 1,
	  [Define if your assembler supports .gnu_attribute.])])
    gcc_GAS_CHECK_FEATURE([.machine and .machinemode support],
      gcc_cv_as_s390_machine_machinemode,,
      [	.machinemode push
	.machinemode pop
	.machine push
	.machine pop],,
      [AC_DEFINE(HAVE_AS_MACHINE_MACHINEMODE, 1,
	  [Define if your assembler supports .machine and .machinemode.])])
    gcc_GAS_CHECK_FEATURE([architecture modifiers support],
      gcc_cv_as_s390_architecture_modifiers,,
      [	.machine z13+vx ],,
      [AC_DEFINE(HAVE_AS_ARCHITECTURE_MODIFIERS, 1,
	  [Define if your assembler supports architecture modifiers.])])
    gcc_GAS_CHECK_FEATURE([vector load/store alignment hints],
      gcc_cv_as_s390_vector_loadstore_alignment_hints,,
      [	vl %v24,0(%r15),3 ],,
      [AC_DEFINE(HAVE_AS_VECTOR_LOADSTORE_ALIGNMENT_HINTS, 1,
	  [Define if your assembler supports vl/vst/vlm/vstm with an optional alignment hint argument.])])
    gcc_GAS_CHECK_FEATURE([vector load/store alignment hints on z13],
      gcc_cv_as_s390_vector_loadstore_alignment_hints_on_z13, [-mzarch -march=z13],
      [	vl %v24,0(%r15),3 ],,
      [AC_DEFINE(HAVE_AS_VECTOR_LOADSTORE_ALIGNMENT_HINTS_ON_Z13, 1,
	  [Define if your assembler supports vl/vst/vlm/vstm with an optional alignment hint argument on z13.])])

    ;;
esac

# Mips, LoongArch and HP-UX need the GNU assembler.
# Linux on IA64 might be able to use the Intel assembler.

case "$target" in
  mips*-*-* | loongarch*-*-* | *-*-hpux* )
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

# AMD GCN needs the LLVM assembler and linker.
# Test that LLVM is at least 13.0.1.
case "$target" in
  amdgcn-* | gcn-*)
    AC_MSG_CHECKING(llvm assembler version)
    gcc_cv_as_version="unknown"
    if test x$gcc_cv_as != x; then
      gcc_cv_as_version=`$gcc_cv_as --version 2>&1 | sed -ne '/version/s/.* \([[0-9]]\)/\1/p' || echo error`
      case "$gcc_cv_as_version" in
	13.0.[[1-9]]*) ;;          # 13.0.1+
	13.[[1-9]]*) ;;            # 13.1+
	1[[4-9]]*) ;;              # 14..19
	[[2-9]][[0-9]]*) ;;        # 20..99
	[[1-9]][[0-9]][[0-9]]*) ;; # 100+
	error) AC_MSG_ERROR([cannot determine LLVM version]) ;;
	*) AC_MSG_ERROR([LLVM 13.0.1 or later is required (found LLVM $gcc_cv_as_version)]) ;;
      esac
    fi
    AC_MSG_RESULT([$gcc_cv_as_version, ok])
    ;;
esac

case "$target" in
  arm*)
    gcc_GAS_CHECK_FEATURE([assembler for arm accepts context-specific architecture extensions],
      gcc_cv_as_arm_option_extensions,
      [-march=armv8.1-m.main+mve],
      [.text
	.thumb
	.syntax unified
	vmov.f32 s0, s1],,
      [AC_DEFINE(HAVE_GAS_ARM_EXTENDED_ARCH, 1,
       [Define if your Arm assembler permits context-specific feature extensions.])])
esac

# ??? Not all targets support dwarf2 debug_line, even within a version
# of gas.  Moreover, we need to emit a valid instruction to trigger any
# info to the output file.  So, as supported targets are added to gas 2.11,
# add some instruction here to (also) show we expect this might work.
# ??? Once 2.11 is released, probably need to add first known working
# version to the per-target configury.
case "$cpu_type" in
  aarch64 | alpha | arc | arm | avr | bfin | cris | csky | i386 | loongarch | m32c \
  | m68k | microblaze | mips | nds32 | pa | riscv | rs6000 | score | sparc \
  | visium | xstormy16 | xtensa | ft32)
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
  gcc_cv_as_dwarf2_debug_line,,
  [$conftest_s],
  [if test x$gcc_cv_objdump != x \
   && $gcc_cv_objdump -h conftest.o 2> /dev/null \
      | grep debug_line > /dev/null 2>&1; then
     gcc_cv_as_dwarf2_debug_line=yes
   elif test x$gcc_cv_otool != x \
   && $gcc_cv_otool -l conftest.o 2> /dev/null \
      | grep debug_line > /dev/null 2>&1; then
     gcc_cv_as_dwarf2_debug_line=yes
   fi])

# The .debug_line file table must be in the exact order that
# we specified the files, since these indices are also used
# by DW_AT_decl_file.  Approximate this test by testing if
# the assembler bitches if the same index is assigned twice.
 gcc_GAS_CHECK_FEATURE([buggy dwarf2 .file directive],
  gcc_cv_as_dwarf2_file_buggy,,
[	.file 1 "foo.s"
	.file 1 "bar.s"])

 if test $gcc_cv_as_dwarf2_debug_line = yes \
 && test $gcc_cv_as_dwarf2_file_buggy = no; then
    AC_DEFINE(HAVE_AS_DWARF2_DEBUG_LINE, 1,
  [Define if your assembler supports dwarf2 .file/.loc directives,
   and preserves file table indices exactly as given.])

    if test $gcc_cv_as_leb128 = yes; then
	conftest_s="\
	.file 1 \"conftest.s\"
	.loc 1 3 0 view .LVU1
	$insn
	.data
	.uleb128 .LVU1
	.uleb128 .LVU1
"
	gcc_GAS_CHECK_FEATURE([dwarf2 debug_view support],
	  gcc_cv_as_dwarf2_debug_view,,
	  [$conftest_s],,
	  [AC_DEFINE(HAVE_AS_DWARF2_DEBUG_VIEW, 1,
  [Define if your assembler supports views in dwarf2 .loc directives.])])
    fi
 fi

 gcc_GAS_CHECK_FEATURE([--gdwarf2 option],
  gcc_cv_as_gdwarf2_flag,
  [--gdwarf2], [$insn],,
  [AC_DEFINE(HAVE_AS_GDWARF2_DEBUG_FLAG, 1,
[Define if your assembler supports the --gdwarf2 option.])])

 gcc_GAS_CHECK_FEATURE([--gdwarf-5 option],
  gcc_cv_as_gdwarf_5_flag,
  [--gdwarf-5], [$insn],
  [if test x$gcc_cv_objdump != x \
      && $gcc_cv_objdump -Wi conftest.o 2>&1 \
	 | grep DW_TAG_compile_unit > /dev/null 2>&1; then
     gcc_cv_as_gdwarf_5_flag=yes;
   fi],[AC_DEFINE(HAVE_AS_GDWARF_5_DEBUG_FLAG, 1,
[Define if your assembler supports the --gdwarf-5 option.])])

 case $target_os in
   win32 | pe | cygwin* | mingw32*)
     section_flags=\"dr\"
     function_type=".def foo; .scl 2; .type 32; .endef"
     function_size="";;
   *)
     section_flags=\"\",%progbits
     function_type=".type foo, %function"
     function_size=".size foo, .-foo";;
 esac
 dwarf4_debug_info_size=0x46
 dwarf4_high_pc_form=7
 dwarf4_debug_aranges_size=0x2c
 dwarf4_line_sz=9
 for dwarf4_addr_size in 8 4; do
   conftest_s="\
	.file	\"a.c\"
	.text
.Ltext0:
	.p2align 4
	.globl	foo
	$function_type
foo:
.LFB0:
.LM1:
	$insn
.LM2:
.LFE0:
	$function_size
.Letext0:
	.section	.debug_info,$section_flags
.Ldebug_info0:
	.4byte	$dwarf4_debug_info_size
	.2byte	0x4
	.4byte	.Ldebug_abbrev0
	.byte	0x$dwarf4_addr_size
	.byte	0x1
	.ascii \"GNU C17\\0\"
	.byte	0xc
	.ascii \"a.c\\0\"
	.ascii \"/\\0\"
	.${dwarf4_addr_size}byte	.Ltext0
	.${dwarf4_addr_size}byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.byte	0x2
	.ascii \"foo\\0\"
	.byte	0x1
	.byte	0x2
	.byte	0x1
	.${dwarf4_addr_size}byte	.LFB0
	.${dwarf4_addr_size}byte	.LFE0-.LFB0
	.byte	0x1
	.byte	0x9c
	.byte	0
	.section	.debug_abbrev,$section_flags
.Ldebug_abbrev0:
	.byte	0x1
	.byte	0x11
	.byte	0x1
	.byte	0x25
	.byte	0x8
	.byte	0x13
	.byte	0xb
	.byte	0x3
	.byte	0x8
	.byte	0x1b
	.byte	0x8
	.byte	0x11
	.byte	0x1
	.byte	0x12
	.byte	0x$dwarf4_high_pc_form
	.byte	0x10
	.byte	0x17
	.byte	0
	.byte	0
	.byte	0x2
	.byte	0x2e
	.byte	0
	.byte	0x3f
	.byte	0x19
	.byte	0x3
	.byte	0x8
	.byte	0x3a
	.byte	0xb
	.byte	0x3b
	.byte	0xb
	.byte	0x39
	.byte	0xb
	.byte	0x11
	.byte	0x1
	.byte	0x12
	.byte	0x$dwarf4_high_pc_form
	.byte	0x40
	.byte	0x18
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,$section_flags
	.4byte	$dwarf4_debug_aranges_size
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x8
	.byte	0
	.2byte	0
	.2byte	0
	.${dwarf4_addr_size}byte	.Ltext0
	.${dwarf4_addr_size}byte	.Letext0-.Ltext0
	.${dwarf4_addr_size}byte	0
	.${dwarf4_addr_size}byte	0
	.section	.debug_line,$section_flags
.Ldebug_line0:
	.4byte	.LELT0-.LSLT0
.LSLT0:
	.2byte	0x4
	.4byte	.LELTP0-.LASLTP0
.LASLTP0:
	.byte	0x1
	.byte	0x1
	.byte	0x1
	.byte	0xf6
	.byte	0xf2
	.byte	0xd
	.byte	0
	.byte	0x1
	.byte	0x1
	.byte	0x1
	.byte	0x1
	.byte	0
	.byte	0
	.byte	0
	.byte	0x1
	.byte	0
	.byte	0
	.byte	0x1
	.byte	0
	.ascii \"a.c\\0\"
	.byte	0
	.byte	0
	.byte	0
	.byte	0
.LELTP0:
	.byte	0
	.byte	0x$dwarf4_line_sz
	.byte	0x2
	.${dwarf4_addr_size}byte	.LM1
	.byte	0x18
	.byte	0x5
	.byte	0x1
	.byte	0
	.byte	0x$dwarf4_line_sz
	.byte	0x2
	.${dwarf4_addr_size}byte	.LM2
	.byte	0x1
	.byte	0x5
	.byte	0x1
	.byte	0
	.byte	0x$dwarf4_line_sz
	.byte	0x2
	.${dwarf4_addr_size}byte	.Letext0
	.byte	0
	.byte	0x1
	.byte	0x1
.LELT0:
	.section	.debug_str,$section_flags
	.ident	\"GCC\"
"
   dwarf4_success=no
   if test $dwarf4_addr_size = 4; then
     gcc_GAS_CHECK_FEATURE([assembly of compiler generated 32-bit .debug_line],
      gcc_cv_as_debug_line_32_flag,,
      [$conftest_s],,
      [dwarf4_success=yes])
   else
     gcc_GAS_CHECK_FEATURE([assembly of compiler generated 64-bit .debug_line],
      gcc_cv_as_debug_line_64_flag,,
      [$conftest_s],,
      [dwarf4_success=yes])
   fi
   if test $dwarf4_success = yes; then
     dwarf4_success=no
     gcc_GAS_CHECK_FEATURE([--gdwarf-4 not refusing compiler generated .debug_line],
      gcc_cv_as_dwarf_4_debug_line_flag,
      [--gdwarf-4], [$conftest_s],,
      [dwarf4_success=yes])
     break
   fi
   dwarf4_debug_info_size=0x36
   dwarf4_high_pc_form=6
   dwarf4_debug_aranges_size=0x1c
   dwarf4_line_sz=5
 done

 if test $dwarf4_success = yes; then
   conftest_s="\
	.file	\"foo.c\"
	.text
bar:
#APP
# 82 \"xxx.h\" 1
	$insn
# 0 \"\" 2
#NO_APP
	$insn
foo:
	.file 1 \"foo.c\"
	$insn
	.file 2 \"foo.h\"
	ret
"
   dwarf4_success=no
   gcc_GAS_CHECK_FEATURE([--gdwarf-4 with the APP marker],
     gcc_cv_as_dwarf_4_app_flag,
     [--gdwarf-4], [$conftest_s],, [dwarf4_success=yes])
 fi

 if test $dwarf4_success = yes; then
   conftest_s="\
	.text
	.globl	foo
	$function_type
foo:
	$insn
	$function_size
	.file	1 \"foo.c\"
"
   gcc_GAS_CHECK_FEATURE([working --gdwarf-4/--gdwarf-5 for all sources],
     gcc_cv_as_working_gdwarf_n_flag,
     [--gdwarf-4],
     [$conftest_s],
     [changequote(,)dnl
      if test x$gcc_cv_objdump != x \
	 && $gcc_cv_objdump -W conftest.o 2>&1 \
		| grep conftest.s > /dev/null 2>&1; then
	gcc_cv_as_working_gdwarf_n_flag=no
      else
	gcc_cv_as_working_gdwarf_n_flag=yes
      fi
      changequote([,])dnl])
   if test $gcc_cv_as_working_gdwarf_n_flag = yes; then
     AC_DEFINE(HAVE_AS_WORKING_DWARF_N_FLAG, 1,
[Define if your assembler supports --gdwarf-4/--gdwarf-5 even with
 compiler generated .debug_line.])
   fi
 fi

 gcc_GAS_CHECK_FEATURE([--debug-prefix-map option],
  gcc_cv_as_debug_prefix_map_flag,
  [--debug-prefix-map /a=/b], [$insn],,
  [AC_DEFINE(HAVE_AS_DEBUG_PREFIX_MAP, 1,
[Define if your assembler supports the --debug-prefix-map option.])])
fi

gcc_GAS_CHECK_FEATURE([compressed debug sections],
  gcc_cv_as_compress_debug,,,
  [# gas compiled without zlib cannot compress debug sections and warns
   # about it, but still exits successfully.  So check for this, too.
   if $gcc_cv_as --compress-debug-sections -o conftest.o conftest.s 2>&1 | grep -i warning > /dev/null
   then
     gcc_cv_as_compress_debug=0
   elif $gcc_cv_as --compress-debug-sections=zlib -o conftest.o conftest.s > /dev/null 2>&1
   then
     gcc_cv_as_compress_debug=1
     gcc_cv_as_compress_debug_option="--compress-debug-sections"
     gcc_cv_as_no_compress_debug_option="--nocompress-debug-sections"
     # Since binutils 2.40, gas supports --compress-debug-sections=zstd.
     if $gcc_cv_as --compress-debug-sections=zstd -o conftest.o conftest.s > /dev/null 2>&1
     then
       gcc_cv_as_compress_debug=2
     fi
   else
     gcc_cv_as_compress_debug=0
   fi])
AC_DEFINE_UNQUOTED(HAVE_AS_COMPRESS_DEBUG, $gcc_cv_as_compress_debug,
[Define to the level of your assembler's compressed debug section support.])
AC_DEFINE_UNQUOTED(AS_COMPRESS_DEBUG_OPTION, "$gcc_cv_as_compress_debug_option",
[Define to the assembler option to enable compressed debug sections.])
AC_DEFINE_UNQUOTED(AS_NO_COMPRESS_DEBUG_OPTION, "$gcc_cv_as_no_compress_debug_option",
[Define to the assembler option to disable compressed debug sections.])

gcc_GAS_CHECK_FEATURE([.lcomm with alignment], gcc_cv_as_lcomm_with_alignment,,
[.lcomm bar,4,16],,
[AC_DEFINE(HAVE_GAS_LCOMM_WITH_ALIGNMENT, 1,
  [Define if your assembler supports .lcomm with an alignment field.])])

if test x$with_sysroot = x && test x$host = x$target \
   && test "$prefix" != "/usr" && test "x$prefix" != "x$local_prefix" \
   && test "$prefix" != "NONE"; then
  AC_DEFINE_UNQUOTED(PREFIX_INCLUDE_DIR, "$prefix/include",
[Define to PREFIX/include if cpp should also search that directory.])
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
AC_DEFINE_UNQUOTED([TARGET_GLIBC_MAJOR], [$glibc_version_major],
[GNU C Library major version number used on the target, or 0.])
AC_DEFINE_UNQUOTED([TARGET_GLIBC_MINOR], [$glibc_version_minor],
[GNU C Library minor version number used on the target, or 0.])

AC_ARG_ENABLE(gnu-unique-object,
 [AS_HELP_STRING([--enable-gnu-unique-object],
   [enable the use of the @gnu_unique_object ELF extension on glibc systems])],
 [case $enable_gnu_unique_object in
    yes | no) ;;
    *) AC_MSG_ERROR(['$enable_gnu_unique_object' is an invalid value for --enable-gnu-unique-object.
Valid choices are 'yes' and 'no'.]) ;;
  esac],
 [gcc_GAS_CHECK_FEATURE([gnu_unique_object], gcc_cv_as_gnu_unique_object,,
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
  if test "x$gcc_cv_as" != x; then
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

AC_MSG_CHECKING(support for thin archives)
thin_archive_support=no
echo 'int main (void) { return 0; }' > conftest.c
if ($AR --version | sed 1q | grep "GNU ar" \
    && $CC $CFLAGS -c conftest.c \
    && $AR rcT conftest.a conftest.o \
    && $CC $CFLAGS $LDFLAGS -o conftest conftest.a) >/dev/null 2>&1; then
  thin_archive_support=yes
fi
rm -f conftest.c conftest.o conftest.a conftest
AC_MSG_RESULT($thin_archive_support)
AC_SUBST(thin_archive_support)

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
    if $gcc_cv_ld --help 2>&1 | grep eh-frame-hdr > /dev/null; then
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
  case "$target" in
    # Full PIE support on Solaris was only introduced in gld 2.26.
    *-*-solaris2*)  gcc_gld_pie_min_version=26 ;;
    *) 		    gcc_gld_pie_min_version=15 ;;
  esac
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge "$gcc_gld_pie_min_version" -o "$gcc_cv_gld_major_version" -gt 2 \
     && test $in_tree_ld_is_elf = yes; then
    gcc_cv_ld_pie=yes
  fi
elif test x$gcc_cv_ld != x; then
  # Check if linker supports -pie option
  if $gcc_cv_ld --help 2>&1 | grep -- -pie > /dev/null; then
    gcc_cv_ld_pie=yes
    case "$target" in
      *-*-solaris2*)
	if echo "$ld_ver" | grep GNU > /dev/null \
	  && test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -lt 26; then
	  gcc_cv_ld_pie=no
	fi
	;;
    esac
  else
    case "$target" in
      *-*-solaris2.1[[1-9]]*)
	# Solaris 11.3 added PIE support.
	if $gcc_cv_ld -z help 2>&1 | grep -- type.*pie > /dev/null; then
	  gcc_cv_ld_pie=yes
	fi
	;;
    esac
  fi
fi
if test x"$gcc_cv_ld_pie" = xyes; then
	AC_DEFINE(HAVE_LD_PIE, 1,
[Define if your linker supports PIE option.])
fi
AC_MSG_RESULT($gcc_cv_ld_pie)

AC_MSG_CHECKING(linker PIE support with copy reloc)
gcc_cv_ld_pie_copyreloc=no
if test $gcc_cv_ld_pie = yes ; then
  if test $in_tree_ld = yes ; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 25 -o "$gcc_cv_gld_major_version" -gt 2; then
      gcc_cv_ld_pie_copyreloc=yes
    fi
  elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x ; then
    # Check if linker supports -pie option with copy reloc
    case "$target" in
    i?86-*-linux* | x86_64-*-linux*)
      cat > conftest1.s <<EOF
	.globl	a_glob
	.data
	.type	a_glob, @object
	.size	a_glob, 4
a_glob:
	.long	2
EOF
      cat > conftest2.s <<EOF
	.text
	.globl	main
	.type	main, @function
main:
	movl	%eax, a_glob(%rip)
	.size	main, .-main
	.globl	ptr
	.section	.data.rel,"aw",@progbits
	.type	ptr, @object
ptr:
	.quad	a_glob
EOF
      if $gcc_cv_as --64 -o conftest1.o conftest1.s > /dev/null 2>&1 \
         && $gcc_cv_ld -shared -melf_x86_64 -o conftest1.so conftest1.o > /dev/null 2>&1 \
         && $gcc_cv_as --64 -o conftest2.o conftest2.s > /dev/null 2>&1 \
         && $gcc_cv_ld -pie -melf_x86_64 -o conftest conftest2.o conftest1.so > /dev/null 2>&1; then
        gcc_cv_ld_pie_copyreloc=yes
      fi
      rm -f conftest conftest1.so conftest1.o conftest2.o conftest1.s conftest2.s
      ;;
    esac
  fi
fi
AC_DEFINE_UNQUOTED(HAVE_LD_PIE_COPYRELOC,
  [`if test x"$gcc_cv_ld_pie_copyreloc" = xyes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your linker supports -pie option with copy reloc.])
AC_MSG_RESULT($gcc_cv_ld_pie_copyreloc)

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

AC_MSG_CHECKING(linker for compressed debug sections)
# GNU ld/gold support --compressed-debug-sections=zlib since binutils 2.26.
if $gcc_cv_ld --help 2>&1 | grep -- '--compress-debug-sections.*\<zlib-gabi\>' > /dev/null; then
    gcc_cv_ld_compress_debug=1
    gcc_cv_ld_compress_debug_option="--compress-debug-sections"
    # Detect zstd debug section compression support
    if $gcc_cv_ld --help 2>&1 | grep -- '--compress-debug-sections.*\<zstd\>' > /dev/null; then
      gcc_cv_ld_compress_debug=2
    fi
else
changequote(,)dnl
  case "${target}" in
    *-*-solaris2*)
      # Introduced in Solaris 11.2.
      if $gcc_cv_ld --help 2>&1 | grep -- '-z compress-sections' > /dev/null; then
        gcc_cv_ld_compress_debug=1
        gcc_cv_ld_compress_debug_option="-z compress-sections"
      else
        gcc_cv_ld_compress_debug=0
      fi
      ;;
    *)
      # Assume linkers other than GNU ld don't support compessed debug
      # sections.
      gcc_cv_ld_compress_debug=0
      ;;
  esac
changequote([,])dnl
fi
AC_DEFINE_UNQUOTED(HAVE_LD_COMPRESS_DEBUG, $gcc_cv_ld_compress_debug,
[Define to the level of your linker's compressed debug section support.])
AC_DEFINE_UNQUOTED(LD_COMPRESS_DEBUG_OPTION, "$gcc_cv_ld_compress_debug_option",
[Define to the linker option to enable compressed debug sections.])
AC_MSG_RESULT($gcc_cv_ld_compress_debug)

if test x"$ld64_flag" = x"yes"; then

  # Set defaults for possibly untestable items.
  gcc_cv_ld64_export_dynamic=0
  gcc_cv_ld64_platform_version=0
  gcc_cv_ld64_demangle=0

  if test "$build" = "$host"; then
    darwin_try_test=1
  else
    darwin_try_test=0
  fi

  # On Darwin, because of FAT library support, it is often possible to execute
  # exes from compatible archs even when the host differs from the build system.
  case "$build","$host" in
    x86_64-*-darwin*,i?86-*-darwin* | powerpc64*-*-darwin*,powerpc*-*-darwin*)
	darwin_try_test=1;;
    *) ;;
  esac

  # If the configurer specified a minimum ld64 version to be supported, then use
  # that to determine feature support.
  if test x"${gcc_cv_ld64_version}" != x; then
    AC_MSG_CHECKING(ld64 specified version)
    gcc_cv_ld64_major=`echo "$gcc_cv_ld64_version" | sed -e 's/\..*//'`
    AC_MSG_RESULT($gcc_cv_ld64_major)
    if test "$gcc_cv_ld64_major" -ge 97; then
      gcc_cv_ld64_demangle=1
    fi
    if test "$gcc_cv_ld64_major" -ge 236; then
      gcc_cv_ld64_export_dynamic=1
    fi
    if test "$gcc_cv_ld64_major" -ge 512; then
      gcc_cv_ld64_platform_version=1
    fi
  elif test -x "$gcc_cv_ld" -a "$darwin_try_test" -eq 1; then
    # If the version was not specified, try to find it.
    AC_MSG_CHECKING(linker version)
    if test x"${gcc_cv_ld64_version}" = x; then
      gcc_cv_ld64_version=`$gcc_cv_ld -v 2>&1 | $EGREP 'ld64|dyld' \
      | sed -e 's/.*ld64-//' -e 's/.*dyld-//'| awk '{print $1}'`
    fi
    AC_MSG_RESULT($gcc_cv_ld64_version)

    AC_MSG_CHECKING(linker for -demangle support)
    gcc_cv_ld64_demangle=1
    if $gcc_cv_ld -demangle < /dev/null 2>&1 | grep 'unknown option' > /dev/null; then
      gcc_cv_ld64_demangle=0
    fi
    AC_MSG_RESULT($gcc_cv_ld64_demangle)

    AC_MSG_CHECKING(linker for -export_dynamic support)
    gcc_cv_ld64_export_dynamic=1
    if $gcc_cv_ld -export_dynamic < /dev/null 2>&1 | grep 'unknown option' > /dev/null; then
      gcc_cv_ld64_export_dynamic=0
    fi
    AC_MSG_RESULT($gcc_cv_ld64_export_dynamic)

    AC_MSG_CHECKING(linker for -platform_version support)
    gcc_cv_ld64_platform_version=1
    if $gcc_cv_ld -platform_version macos 10.5 0.0 < /dev/null 2>&1 | grep 'unknown option' > /dev/null; then
      gcc_cv_ld64_platform_version=0
    fi
    AC_MSG_RESULT($gcc_cv_ld64_platform_version)
  fi

  if test x"${gcc_cv_ld64_version}" != x; then
    AC_DEFINE_UNQUOTED(LD64_VERSION, "${gcc_cv_ld64_version}",
      [Define to ld64 version.])
  fi

  AC_DEFINE_UNQUOTED(LD64_HAS_DEMANGLE, $gcc_cv_ld64_demangle,
  [Define to 1 if ld64 supports '-demangle'.])

  AC_DEFINE_UNQUOTED(LD64_HAS_EXPORT_DYNAMIC, $gcc_cv_ld64_export_dynamic,
  [Define to 1 if ld64 supports '-export_dynamic'.])

  AC_DEFINE_UNQUOTED(LD64_HAS_PLATFORM_VERSION, $gcc_cv_ld64_platform_version,
  [Define to 1 if ld64 supports '-platform_version'.])
fi

if test x"$dsymutil_flag" = x"yes"; then

    # If the user specified a dsymutil path, then we will already have the
    # version string, otherwise, pick it up.
    if test x"$gcc_cv_dsymutil" = x; then
        AC_MSG_WARN([dsymutil is a required tool for this system, but not found])
        dsymutil_vers="tool unspecified"
    elif test x"$dsymutil_vers" = x; then
	dsymutil_vers=`$gcc_cv_dsymutil -v /dev/null 2>&1`
    fi

    dsymutil_temp=`echo $dsymutil_vers | sed 1q`
    AC_MSG_CHECKING(dsymutil version "$dsymutil_temp")
    if echo $dsymutil_temp | grep dwarfutils- > /dev/null; then
      dsymutil_kind=DWARFUTILS
      dsymutil_vers=`echo $dsymutil_temp | sed 's/.*dwarfutils-\([[0-9\.]]*\).*/\1/'`
    elif echo $dsymutil_temp | grep clang- > /dev/null; then
      dsymutil_kind=CLANG
      dsymutil_vers=`echo $dsymutil_temp | sed 's/.*clang-\([[0-9\.]]*\).*/\1/'`
    elif echo $dsymutil_temp | grep 'LLVM version ' > /dev/null; then
      dsymutil_kind=LLVM
      dsymutil_vers=`echo $dsymutil_temp | sed 's/.*LLVM\ version\ \([[0-9\.]]*\).*/\1/'`
    else
      dsymutil_kind=DET_UNKNOWN
      dsymutil_vers="0.0"
    fi
    dsymutil_major=`expr "$dsymutil_vers" : '\([[0-9]]*\)'`
    dsymutil_minor=`expr "$dsymutil_vers" : '[[0-9]]*\.\([[0-9]]*\)'`
    dsymutil_tiny=`expr "$dsymutil_vers" : '[[0-9]]*\.[[0-9]]*\.\([[0-9]]*\)'`
    if test x"${dsymutil_minor}" = x; then
      dsymutil_minor=0
    fi
    if test x"${dsymutil_tiny}" = x; then
      dsymutil_tiny=0
    fi
    AC_DEFINE_UNQUOTED(DSYMUTIL_VERSION, [$dsymutil_kind,${dsymutil_major},${dsymutil_minor},${dsymutil_tiny}],
        [Define to the dsymutil version.])
    AC_MSG_RESULT($dsymutil_vers : $dsymutil_kind ${dsymutil_major} ${dsymutil_minor} ${dsymutil_tiny} )
fi

case $target_os in
  win32 | pe | cygwin* | mingw32*)
    AC_MSG_CHECKING(broken PE linker dwarf5 support)
    gcc_cv_ld_broken_pe_dwarf5=yes
    if test $in_tree_ld = yes ; then
      if grep -q '\.debug_loclists.*BLOCK.*__section_alignment__.*NOLOAD.*:' \
	   $gcc_cv_ld_gld_srcdir/scripttempl/pe*.sc \
	 && grep -q '\.debug_rnglists.*BLOCK.*__section_alignment__.*NOLOAD.*:' \
	      $gcc_cv_ld_gld_srcdir/scripttempl/pe*.sc; then
	gcc_cv_ld_broken_pe_dwarf5=no
      fi
    else
      if $gcc_cv_ld --verbose 2>&1 | grep -q '\.debug_loclists.*BLOCK.*__section_alignment__.*NOLOAD.*:' \
	 && $gcc_cv_ld --verbose 2>&1 | grep -q '\.debug_rnglists.*BLOCK.*__section_alignment__.*NOLOAD.*:'; then
	gcc_cv_ld_broken_pe_dwarf5=no
      fi
    fi
    if test x$gcc_cv_ld_broken_pe_dwarf5 = xyes; then
      AC_DEFINE(HAVE_LD_BROKEN_PE_DWARF5, 1,
	        [Define if the PE linker has broken DWARF 5 support.])
    fi
    AC_MSG_RESULT($gcc_cv_ld_broken_pe_dwarf5)

    AC_MSG_CHECKING(PE linker --disable-dynamicbase support)
    gcc_cv_ld_disable_dynamicbase=no
    if test $in_tree_ld = yes; then
      if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 36 -o "$gcc_cv_gld_major_version" -gt 2; then \
        gcc_cv_ld_disable_dynamicbase=yes
      fi
    else
      if $gcc_cv_ld --help 2>&1 | grep -q 'disable\-]dynamicbase' > /dev/null; then
        gcc_cv_ld_disable_dynamicbase=yes
      fi
    fi
    if test x"$gcc_cv_ld_disable_dynamicbase" = xyes; then
      AC_DEFINE(HAVE_LD_PE_DISABLE_DYNAMICBASE, 1,
                [Define if the PE linker supports --disable-dynamicbase option.])
    fi
    AC_MSG_RESULT($gcc_cv_ld_disable_dynamicbase)
    ;;
esac

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
    if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 28; then
      gcc_cv_ld_as_needed_option='--push-state --as-needed'
      gcc_cv_ld_no_as_needed_option='--pop-state'
    fi
  fi
elif test x$gcc_cv_ld != x; then
  # Check if linker supports --as-needed and --no-as-needed options
  if $gcc_cv_ld --help 2>&1 | grep as-needed > /dev/null; then
    gcc_cv_ld_as_needed=yes
    if $gcc_cv_ld --help 2>&1 | grep push-state > /dev/null \
       && $gcc_cv_ld --help 2>&1 | grep pop-state > /dev/null \
       && echo "$ld_ver" | grep GNU > /dev/null \
       && test "$ld_vers_major" -eq 2 -a "$ld_vers_minor" -ge 28; then
      # Use these options only when both ld.bfd and ld.gold support
      # --push-state/--pop-state, which unfortunately wasn't added
      # at the same time.
      gcc_cv_ld_as_needed_option='--push-state --as-needed'
      gcc_cv_ld_no_as_needed_option='--pop-state'
    fi
  fi
  case "$target:$gnu_ld" in
    *-*-solaris2*:no)
      # Solaris 2 ld always supports -z ignore/-z record.  Prefer the native
      # forms.
      gcc_cv_ld_as_needed=yes
      gcc_cv_ld_as_needed_option="-z ignore"
      gcc_cv_ld_no_as_needed_option="-z record"
      ;;
  esac
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

AC_MSG_CHECKING(linker mapfile support for clearing hardware capabilities)
saved_LDFLAGS="$LDFLAGS"
for clearcap_map in sol2-clearcapv2.map sol2-clearcap.map; do
  LDFLAGS="$saved_LDFLAGS -Wl,-M,${srcdir}/config/$clearcap_map"
  AC_LINK_IFELSE([AC_LANG_SOURCE([int main(void) {return 0;}])],
    [gcc_cv_ld_clearcap=yes; break], [gcc_cv_ld_clearcap=no])
done
LDFLAGS="$saved_LDFLAGS"
if test "x$gcc_cv_ld_clearcap" = xyes; then
  AC_DEFINE([HAVE_LD_CLEARCAP], 1,
[Define if the linker supports clearing hardware capabilities via mapfile.])
  AC_CONFIG_LINKS([clearcap.map:${srcdir}/config/$clearcap_map])
fi
AC_MSG_RESULT($gcc_cv_ld_clearcap)

case "$target" in
  powerpc*-*-*)
    case "$target" in
      *le-*-linux*)
	emul_name="-melf32lppc"
	;;
      *)
	emul_name="-melf32ppc"
	;;
    esac
    AC_CACHE_CHECK(linker .gnu.attributes long double support,
    gcc_cv_ld_ppc_attr,
    [gcc_cv_ld_ppc_attr=no
    if test x"$ld_is_gold" = xyes; then
      gcc_cv_ld_ppc_attr=yes
    elif test $in_tree_ld = yes ; then
      if test "$gcc_cv_gld_major_version" -eq 2 \
		-a "$gcc_cv_gld_minor_version" -ge 28 \
		-o "$gcc_cv_gld_major_version" -gt 2; then
        gcc_cv_ld_ppc_attr=yes
      fi
    elif test x$gcc_cv_as != x -a x$gcc_cv_ld != x ; then
      # check that merging the long double .gnu_attribute doesn't warn
      cat > conftest1.s <<EOF
	.gnu_attribute 4,1
EOF
      cat > conftest2.s <<EOF
	.gnu_attribute 4,9
EOF
      if $gcc_cv_as -a32 -o conftest1.o conftest1.s > /dev/null 2>&1 \
         && $gcc_cv_as -a32 -o conftest2.o conftest2.s > /dev/null 2>&1 \
         && $gcc_cv_ld $emul_name -r -o conftest.o conftest1.o conftest2.o > /dev/null 2> conftest.err \
	 && test ! -s conftest.err; then
        gcc_cv_ld_ppc_attr=yes
      fi
      rm -f conftest.err conftest.o conftest1.o conftest2.o conftest1.s conftest2.s
    fi
    ])
    if test x$gcc_cv_ld_ppc_attr = xyes; then
      AC_DEFINE(HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE, 1,
    [Define if your PowerPC linker has .gnu.attributes long double support.])
    fi
    ;;
esac

case "$target:$tm_file" in
  powerpc64*-*-freebsd* | powerpc64*-*-linux* | powerpc*-*-linux*rs6000/biarch64.h*)
  case "$target" in
     *le-*-linux*)
     emul_name="-melf64lppc"
      ;;
     *-*-linux*)
     emul_name="-melf64ppc"
      ;;
     *le-*-freebsd*)
     emul_name="-melf64lppc_fbsd"
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

    AC_CACHE_CHECK(linker toc pointer alignment,
    gcc_cv_ld_toc_align,
    [if test x$gcc_cv_as != x -a x$gcc_cv_ld != x -a x$gcc_cv_nm != x; then
      cat > conftest.s <<EOF
	.global _start
	.text
_start:
	addis 9,2,x@got@ha
	.section .data.rel.ro,"aw",@progbits
	.p2align 16
	.space 32768
x:	.quad .TOC.
EOF
      if $gcc_cv_as -a64 -o conftest.o conftest.s > /dev/null 2>&1 \
         && $gcc_cv_ld $emul_name -z norelro -o conftest conftest.o > /dev/null 2>&1; then
        gcc_cv_ld_toc_align=`$gcc_cv_nm conftest | ${AWK} '/\.TOC\./ { match ($0, "0[[[:xdigit:]]]*", a); print strtonum ("0x" substr(a[[0]], length(a[[0]])-3)) }'`
      fi
      rm -f conftest conftest.o conftest.s
    fi
    ])
    if test -n "$gcc_cv_ld_toc_align" && test $gcc_cv_ld_toc_align -gt 8; then
      AC_DEFINE_UNQUOTED(POWERPC64_TOC_POINTER_ALIGNMENT, $gcc_cv_ld_toc_align,
    [Define to .TOC. alignment forced by your linker.])
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
    if $gcc_cv_ld --help 2>&1 | grep build-id > /dev/null; then
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
    if $gcc_cv_ld --help 2>&1 | grep sysroot > /dev/null; then
      gcc_cv_ld_sysroot=yes
    fi
  fi])
if test x"$gcc_cv_ld_sysroot" = xyes; then
  AC_DEFINE(HAVE_LD_SYSROOT, 1,
  [Define if your linker supports --sysroot.])
fi	  

case $target in
*-*-solaris2*)
  # Check for system-provided CRTs on Solaris 11.x and Solaris 12.
  AC_CACHE_CHECK([system-provided CRTs on Solaris],
    gcc_cv_solaris_crts,
    [gcc_cv_solaris_crts=no
     if test x$host != x$target; then
       if test "x$with_sysroot" = xyes; then
         target_sysroot="${test_exec_prefix}/${target_noncanonical}/sys-root"
       else
         target_sysroot="${with_sysroot}"
       fi
     fi
     target_libdir="$target_sysroot/usr/lib"
     # At the time they were added, gcrt1.o became a symlink for backwards
     # compatibility on x86, while crt1.o was added on sparc, so check for that.
     case $target in
       i?86-*-solaris2* | x86_64-*-solaris2*)
         if test -h "$target_libdir/gcrt1.o"; then gcc_cv_solaris_crts=yes; fi
	 ;;
       sparc*-*-solaris2*)
         if test -f "$target_libdir/crt1.o"; then gcc_cv_solaris_crts=yes; fi
	 ;;
     esac])
  ;;
esac
if test x$gcc_cv_solaris_crts = xyes; then
  AC_DEFINE(HAVE_SOLARIS_CRTS, 1,
  	    [Define if the system-provided CRTs are present on Solaris.])
fi

AC_ARG_ENABLE(libssp,
[AS_HELP_STRING([--enable-libssp], [enable linking against libssp])],
[case "${enableval}" in
  yes|no)
    ;;
  *)
    AC_MSG_ERROR([unknown libssp setting $enableval])
    ;;
esac], [])

# Test for stack protector support in target C library.
AC_CACHE_CHECK(__stack_chk_fail in target C library,
  gcc_cv_libc_provides_ssp,
  [gcc_cv_libc_provides_ssp=no
  if test "x$enable_libssp" = "xno"; then
    gcc_cv_libc_provides_ssp=yes
  elif test "x$enable_libssp" = "xyes"; then
    gcc_cv_libc_provides_ssp=no
  else
    case "$target" in
       *-*-musl*)
	 # All versions of musl provide stack protector
	 gcc_cv_libc_provides_ssp=yes;;
       *-*-linux* | *-*-kfreebsd*-gnu)
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
	 # All supported versions of musl provide it as well
	 gcc_cv_libc_provides_ssp=yes;;
       *-*-darwin* | *-*-freebsd* | *-*-netbsd*)
	 AC_CHECK_FUNC(__stack_chk_fail,[gcc_cv_libc_provides_ssp=yes],
           [echo "no __stack_chk_fail on this target"])
        ;;
       *) gcc_cv_libc_provides_ssp=no ;;
    esac
  fi])

if test x$gcc_cv_libc_provides_ssp = xyes; then
  AC_DEFINE(TARGET_LIBC_PROVIDES_SSP, 1,
	    [Define if your target C library provides stack protector support])
fi

# Check whether --enable-default-ssp was given.
AC_ARG_ENABLE(default-ssp,
[AS_HELP_STRING([--enable-default-ssp],
  [enable Stack Smashing Protection as default])],[
if test x$gcc_cv_libc_provides_ssp = xyes; then
  case "$target" in
    ia64*-*-*) enable_default_ssp=no ;;
    *) enable_default_ssp=$enableval ;;
  esac
else
  enable_default_ssp=no
fi],
enable_default_ssp=no)
if test x$enable_default_ssp = xyes ; then
  AC_DEFINE(ENABLE_DEFAULT_SSP, 1,
      [Define if your target supports default stack protector and it is enabled.])
fi
AC_SUBST([enable_default_ssp])

# Test for <sys/sdt.h> on the target.
GCC_TARGET_TEMPLATE([HAVE_SYS_SDT_H])
AC_CACHE_CHECK([sys/sdt.h in the target C library], [gcc_cv_sys_sdt_h], [
  gcc_cv_sys_sdt_h=no
  if test -f $target_header_dir/sys/sdt.h; then
    gcc_cv_sys_sdt_h=yes
  fi
])
AS_IF([test x$gcc_cv_sys_sdt_h = xyes], [
  AC_DEFINE([HAVE_SYS_SDT_H], [1],
            [Define if your target C library provides sys/sdt.h])
])

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
      gcc_cv_target_ldbl128="$with_long_double_128", [
      case "$target" in
	s390*-*-linux-musl*)
	  gcc_cv_target_ldbl128=yes
	  ;;
	powerpc*-*-linux-musl*)
	  gcc_cv_target_ldbl128=no
	  ;;
	*)]
      [GCC_GLIBC_VERSION_GTE_IFELSE([2], [4], [gcc_cv_target_ldbl128=yes], [
      [gcc_cv_target_ldbl128=no
      grep '^[ 	]*#[ 	]*define[ 	][ 	]*__LONG_DOUBLE_MATH_OPTIONAL' \
        $target_header_dir/bits/wordsize.h > /dev/null 2>&1 \
      && gcc_cv_target_ldbl128=yes
      ]])]
      [
	  ;;
      esac
      ])
    ;;
esac
if test x$gcc_cv_target_ldbl128 = xyes; then
  AC_DEFINE(TARGET_DEFAULT_LONG_DOUBLE_128, 1,
	    [Define if TFmode long double should be the default])
fi

# Check if TFmode long double target should use the IBM extended double or IEEE
# 128-bit floating point formats if long doubles are 128-bits long.  The long
# double type can only be switched on powerpc64 bit Linux systems where VSX is
# supported.  Other PowerPC systems do not build the IEEE 128-bit emulator in
# libgcc.
AC_ARG_WITH([long-double-format],
  [AS_HELP_STRING([--with-long-double-format={ieee,ibm}]
		  [Specify whether PowerPC long double uses IEEE or IBM format])],[
case "$target:$with_long_double_format" in
  powerpc64le-*-linux*:ieee | powerpc64le-*-linux*:ibm)
    :
    ;;
  powerpc64-*-linux*:ieee | powerpc64-*-linux*:ibm)
    # IEEE 128-bit emulation is only built on 64-bit VSX Linux systems
    case "$with_cpu" in
      power7 | power8 | power9 | power1*)
	:
	;;
      *)
	AC_MSG_ERROR([Configuration option --with-long-double-format is only \
supported if the default cpu is power7 or newer])
	with_long_double_format=""
	;;
      esac
      ;;
  powerpc64*-*-linux*:*)
    AC_MSG_ERROR([--with-long-double-format argument should be ibm or ieee])
    with_long_double_format=""
    ;;
  *)
    AC_MSG_ERROR([Configure option --with-long-double-format is only supported \
on 64-bit PowerPC VSX Linux systems])
    with_long_double_format=""
    ;;
esac],
  [])

# Check if the target LIBC supports exporting the AT_PLATFORM and AT_HWCAP
# values in the TCB.  Currently, only GLIBC 2.23 and later support this.
gcc_cv_libc_provides_hwcap_in_tcb=no
case "$target" in
  powerpc*-*-linux*)
    GCC_GLIBC_VERSION_GTE_IFELSE([2], [23], [gcc_cv_libc_provides_hwcap_in_tcb=yes], )
    ;;
esac
if test x$gcc_cv_libc_provides_hwcap_in_tcb = xyes; then
  AC_DEFINE(TARGET_LIBC_PROVIDES_HWCAP_IN_TCB, 1,
	    [Define if your target C Library provides the AT_HWCAP value in the TCB])
fi

# Check if the target LIBC handles PT_GNU_STACK.
gcc_cv_libc_gnustack=unknown
case "$target" in
  mips*-*-linux-musl*)
    gcc_cv_libc_gnustack=yes
    ;;
  mips*-*-linux*)
    GCC_GLIBC_VERSION_GTE_IFELSE([2], [31], [gcc_cv_libc_gnustack=yes], )
    ;;
esac
if test x$gcc_cv_libc_gnustack = xyes; then
  AC_DEFINE(TARGET_LIBC_GNUSTACK, 1,
            [Define if your target C Library properly handles PT_GNU_STACK])
fi

AC_MSG_CHECKING(dl_iterate_phdr in target C library)
gcc_cv_target_dl_iterate_phdr=unknown
case "$target" in
  *-*-solaris2*)
    gcc_cv_target_dl_iterate_phdr=yes
    ;;
  *-*-dragonfly* | *-*-freebsd*)
    if grep dl_iterate_phdr $target_header_dir/sys/link_elf.h > /dev/null 2>&1; then
      gcc_cv_target_dl_iterate_phdr=yes
    else
      gcc_cv_target_dl_iterate_phdr=no
    fi
    ;;
  *-linux-musl*)
    gcc_cv_target_dl_iterate_phdr=yes
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
AM_ZLIB

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

dnl Variables for tags utilities; copied from automake 1.16.4+'s init.m4
if test -z "$CTAGS"; then
  CTAGS=ctags
fi
AC_SUBST([CTAGS])
if test -z "$ETAGS"; then
  ETAGS=etags
fi
AC_SUBST([ETAGS])
if test -z "$CSCOPE"; then
  CSCOPE=cscope
fi
AC_SUBST([CSCOPE])

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
   AC_MSG_WARN([--enable-link-mutex is deprecated and will be removed in the next release, use --enable-link-serialization instead])
else
   DO_LINK_MUTEX=false
fi
AC_SUBST(DO_LINK_MUTEX)

dnl Whether to prevent multiple GCC front-ends from linking at the same time

AC_MSG_CHECKING([whether to serialize linking of multiple front-ends])
  AC_ARG_ENABLE(link-serialization,
[AS_HELP_STRING([--enable-link-serialization],
		[avoid linking multiple GCC front-ends at once using make
		 dependencies to avoid thrashing on the build machine])],
      do_link_serialization=$enableval,
      do_link_serialization=no)
AC_MSG_RESULT($do_link_serialization)

case "$do_link_serialization" in
  yes)
    DO_LINK_SERIALIZATION=1;;
  [[1-9]] | [[1-9]][[0-9]] | [[1-9]][[0-9]][[0-9]])
    DO_LINK_SERIALIZATION=$do_link_serialization;;
  no)
    DO_LINK_SERIALIZATION=;;
  *)
    AC_MSG_ERROR(bad value ${do_link_serialization} given for --enable-link-serialization) ;;
esac
AC_SUBST(DO_LINK_SERIALIZATION)

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
# List of language configure and makefile fragments.
all_lang_configurefrags=
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

	all_lang_configurefrags="$all_lang_configurefrags \$(srcdir)/$gcc_subdir/config-lang.in"
	if test "x$language" = xc && test -n "$all_lang_makefrags"; then
	    # Put c/Make-lang.in fragment first to match serialization languages order.
	    all_lang_makefrags="\$(srcdir)/$gcc_subdir/Make-lang.in $all_lang_makefrags"
	else
	    all_lang_makefrags="$all_lang_makefrags \$(srcdir)/$gcc_subdir/Make-lang.in"
	fi
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
if test x$enable_libgdiagnostics = xyes; then
	check_languages="$check_languages check-libgdiagnostics check-sarif-replay"
fi

selftest_languages=
for language in $all_selected_languages
do
	selftest_languages="$selftest_languages selftest-$language"
done

# We link each language in with a set of hooks, reached indirectly via
# lang.${target}.  Only do so for selected languages.

rm -f Make-hooks
touch Make-hooks
target_list="all.cross start.encap rest.encap tags \
	install-common install-man install-info install-dvi install-pdf \
	install-html dvi pdf html uninstall info man srcextra srcman srcinfo \
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

echo "ifeq (\$(DO_LINK_SERIALIZATION),)" >> Make-hooks
echo "SERIAL_LIST =" >> Make-hooks
echo else >> Make-hooks
lang_cnt=0
lang_list=
prev=c
serialization_languages=c
for lang in $all_selected_languages
do
	test $lang = c && continue
	if test $lang = lto; then
		serialization_languages="$serialization_languages lto1 lto2"
	else
		serialization_languages="$serialization_languages $lang"
	fi
done
for lang in $serialization_languages
do
	test $lang = c && continue
	lang_cnt=`expr $lang_cnt + 1`
	lang_list=" $prev$lang_list"
	prev=${lang}
done
echo "SERIAL_LIST = \$(wordlist \$(DO_LINK_SERIALIZATION),$lang_cnt,$lang_list)" >> Make-hooks
echo endif >> Make-hooks
echo "SERIAL_COUNT = `expr $lang_cnt + 1`" >> Make-hooks
echo "INDEX.c = 0" >> Make-hooks
lang_idx=1
for lang in $serialization_languages
do
	test $lang = c && continue
	echo "$lang.prev = \$(if \$(word $lang_cnt,\$(SERIAL_LIST)),\$(\$(word $lang_cnt,\$(SERIAL_LIST)).serial))" >> Make-hooks
	echo "INDEX.$lang = $lang_idx" >> Make-hooks
	lang_cnt=`expr $lang_cnt - 1`
	lang_idx=`expr $lang_idx + 1`
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
AC_SUBST(all_lang_configurefrags)
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
AC_SUBST(selftest_languages)
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
AC_SUBST(gcc_gxx_libcxx_include_dir)
AC_SUBST(gcc_gxx_libcxx_include_dir_add_sysroot)
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
AC_SUBST(tm_d_file_list)
AC_SUBST(tm_d_include_list)
AC_SUBST(tm_rust_file_list)
AC_SUBST(tm_rust_include_list)
AC_SUBST(xm_file_list)
AC_SUBST(xm_include_list)
AC_SUBST(xm_defines)
AC_SUBST(use_gcc_stdint)
AC_SUBST(c_target_objs)
AC_SUBST(cxx_target_objs)
AC_SUBST(fortran_target_objs)
AC_SUBST(d_target_objs)
AC_SUBST(rust_target_objs)
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

AC_ARG_VAR(ISLLIBS,[How to link isl])
AC_ARG_VAR(ISLINC,[How to find isl include files])
if test "x${ISLLIBS}" != "x" ; then 
   AC_DEFINE(HAVE_isl, 1, [Define if isl is in use.])
fi

GCC_ENABLE_PLUGINS
AC_SUBST(pluginlibs)
AC_SUBST(enable_plugin)
if test x"$enable_plugin" = x"yes"; then
  AC_DEFINE(ENABLE_PLUGIN, 1, [Define to enable plugin support.])
fi


# Enable --enable-host-shared
AC_ARG_ENABLE(host-shared,
[AS_HELP_STRING([--enable-host-shared],
		[build host code as shared libraries])])
AC_SUBST(enable_host_shared)

# Enable --enable-host-pie
AC_ARG_ENABLE(host-pie,
[AS_HELP_STRING([--enable-host-pie],
		[build host code as PIE])])
AC_SUBST(enable_host_pie)

AC_ARG_ENABLE(libgdiagnostics,
[AS_HELP_STRING([--enable-libgdiagnostics],
		[build libgdiagnostics shared library])])
AC_SUBST(enable_libgdiagnostics)

if test "$enable_libgdiagnostics" = "yes"; then
  LIBGDIAGNOSTICS='libgdiagnostics sarif-replay'
else
  LIBGDIAGNOSTICS=''
fi
AC_SUBST(LIBGDIAGNOSTICS)


# Enable --enable-host-bind-now
AC_ARG_ENABLE(host-bind-now,
[AS_HELP_STRING([--enable-host-bind-now],
		[link host code as BIND_NOW])])
AC_SUBST(enable_host_bind_now)

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

# Specify what should be the default of -fdiagnostics-color option.
AC_ARG_WITH([diagnostics-color],
[AC_HELP_STRING([--with-diagnostics-color={never,auto,auto-if-env,always}],
                [specify the default of -fdiagnostics-color option
                 auto-if-env stands for -fdiagnostics-color=auto if
                 GCC_COLOR environment variable is present and
                 -fdiagnostics-color=never otherwise])],
[case x"$withval" in
   xnever)
     DIAGNOSTICS_COLOR_DEFAULT=DIAGNOSTICS_COLOR_NO
     ;;
   xauto)
     DIAGNOSTICS_COLOR_DEFAULT=DIAGNOSTICS_COLOR_AUTO
     ;;
   xauto-if-env)
     DIAGNOSTICS_COLOR_DEFAULT=-1
     ;;
   xalways)
     DIAGNOSTICS_COLOR_DEFAULT=DIAGNOSTICS_COLOR_YES
     ;;
   *)
     AC_MSG_ERROR([$withval is an invalid option to --with-diagnostics-color])
     ;;
 esac],
[DIAGNOSTICS_COLOR_DEFAULT=DIAGNOSTICS_COLOR_AUTO])
AC_DEFINE_UNQUOTED(DIAGNOSTICS_COLOR_DEFAULT, $DIAGNOSTICS_COLOR_DEFAULT,
		   [The default for -fdiagnostics-color option])

# Specify what should be the default of -fdiagnostics-urls option.
AC_ARG_WITH([diagnostics-urls],
[AC_HELP_STRING([--with-diagnostics-urls={never,auto,auto-if-env,always}],
                [specify the default of -fdiagnostics-urls option
                 auto-if-env stands for -fdiagnostics-urls=auto if
                 GCC_URLS or TERM_URLS environment variable is present and
                 -fdiagnostics-urls=never otherwise])],
[case x"$withval" in
   xnever)
     DIAGNOSTICS_URLS_DEFAULT=DIAGNOSTICS_URL_NO
     ;;
   xauto)
     DIAGNOSTICS_URLS_DEFAULT=DIAGNOSTICS_URL_AUTO
     ;;
   xauto-if-env)
     DIAGNOSTICS_URLS_DEFAULT=-1
     ;;
   xalways)
     DIAGNOSTICS_URLS_DEFAULT=DIAGNOSTICS_URL_YES
     ;;
   *)
     AC_MSG_ERROR([$withval is an invalid option to --with-diagnostics-urls])
     ;;
 esac],
[DIAGNOSTICS_URLS_DEFAULT=DIAGNOSTICS_URL_AUTO])
AC_DEFINE_UNQUOTED(DIAGNOSTICS_URLS_DEFAULT, $DIAGNOSTICS_URLS_DEFAULT,
		   [The default for -fdiagnostics-urls option])

# Generate gcc-driver-name.h containing GCC_DRIVER_NAME for the benefit
# of jit/jit-playback.cc.
gcc_driver_version=`eval "${get_gcc_base_ver} $srcdir/BASE-VER"`
echo "gcc_driver_version: ${gcc_driver_version}"
cat > gcc-driver-name.h <<EOF
#define GCC_DRIVER_NAME "${target_noncanonical}-gcc-${gcc_driver_version}${exeext}"
EOF

# Check whether --enable-default-pie was given.
AC_ARG_ENABLE(default-pie,
[AS_HELP_STRING([--enable-default-pie],
  [enable Position Independent Executable as default])],
enable_default_pie=$enableval,
enable_default_pie=no)
if test x$enable_default_pie = xyes ; then
  AC_DEFINE(ENABLE_DEFAULT_PIE, 1,
      [Define if your target supports default PIE and it is enabled.])
fi
AC_SUBST([enable_default_pie])

# Check if -fno-PIE works.
AC_CACHE_CHECK([for -fno-PIE option],
  [gcc_cv_c_no_fpie],
  [saved_CXXFLAGS="$CXXFLAGS"
   CXXFLAGS="$CXXFLAGS -fno-PIE"
   AC_COMPILE_IFELSE([AC_LANG_SOURCE([int main(void) {return 0;}])],
     [gcc_cv_c_no_fpie=yes],
     [gcc_cv_c_no_fpie=no])
   CXXFLAGS="$saved_CXXFLAGS"])

# Check if -no-pie works.
AC_CACHE_CHECK([for -no-pie option],
  [gcc_cv_no_pie],
  [saved_LDFLAGS="$LDFLAGS"
   LDFLAGS="$LDFLAGS -no-pie"
   AC_LINK_IFELSE([AC_LANG_SOURCE([int main(void) {return 0;}])],
     [gcc_cv_no_pie=yes],
     [gcc_cv_no_pie=no])
   LDFLAGS="$saved_LDFLAGS"])

if test x$enable_host_shared = xyes; then
  PICFLAG=-fPIC
elif test x$enable_host_pie = xyes; then
  PICFLAG=-fPIE
elif test x$gcc_cv_c_no_fpie = xyes; then
  PICFLAG=-fno-PIE
else
  PICFLAG=
fi

AC_SUBST([PICFLAG])

if test x$enable_host_pie = xyes; then
  LD_PICFLAG=-pie
elif test x$gcc_cv_no_pie = xyes; then
  LD_PICFLAG=-no-pie
else
  LD_PICFLAG=
fi

if test x$enable_host_bind_now = xyes; then
  LD_PICFLAG="$LD_PICFLAG -Wl,-z,now"
fi

AC_SUBST([LD_PICFLAG])

# Enable Intel CET on Intel CET enabled host if jit is enabled.
GCC_CET_HOST_FLAGS(CET_HOST_FLAGS)
case x$enable_languages in
*jit*)
  ;;
*)
  CET_HOST_FLAGS=
  ;;
esac
AC_SUBST(CET_HOST_FLAGS)

# Check linker supports '-z bndplt'
ld_bndplt_support=no
AC_MSG_CHECKING(linker -z bndplt option)
if test x"$ld_is_gold" = xno; then
  if test $in_tree_ld = yes ; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 25 -o "$gcc_cv_gld_major_version" -gt 2; then
      ld_bndplt_support=yes
    fi
  elif test x$gcc_cv_ld != x; then
    # Check if linker supports -z bndplt option
    if $gcc_cv_ld --help 2>&1 | grep -- '-z bndplt' > /dev/null; then
      ld_bndplt_support=yes
    fi
  fi
fi
if test x"$ld_bndplt_support" = xyes; then
  AC_DEFINE(HAVE_LD_BNDPLT_SUPPORT, 1,
	[Define if your linker supports -z bndplt])
fi
AC_MSG_RESULT($ld_bndplt_support)

# Check linker supports '--push-state'/'--pop-state'
ld_pushpopstate_support=no
AC_MSG_CHECKING(linker --push-state/--pop-state options)
if test x"$ld_is_gold" = xno; then
  if test $in_tree_ld = yes ; then
    if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 25 -o "$gcc_cv_gld_major_version" -gt 2; then
      ld_pushpopstate_support=yes
    fi
  elif test x$gcc_cv_ld != x; then
    # Check if linker supports --push-state/--pop-state options
    if $gcc_cv_ld --help 2>&1 | grep -- '--push-state' > /dev/null; then
      ld_pushpopstate_support=yes
    fi
  fi
fi
if test x"$ld_pushpopstate_support" = xyes; then
  AC_DEFINE(HAVE_LD_PUSHPOPSTATE_SUPPORT, 1,
	[Define if your linker supports --push-state/--pop-state])
fi
AC_MSG_RESULT($ld_pushpopstate_support)

# On s390, float_t has historically been statically defined as double for no
# good reason. To comply with the C standard in the light of this definition,
# gcc has evaluated float expressions in double precision when in
# standards-compatible mode or when given -fexcess-precision=standard. To enable
# a smooth transition towards the new model used by most architectures, where
# gcc describes its behavior via the macro __FLT_EVAL_METHOD__ and glibc derives
# float_t from that, this behavior can be configured with
# --enable-s390-excess-float-precision. When given as enabled, that flag selects
# the old model. When omitted, native builds and cross compiles that have target
# libc headers will detect whether libc clamps float_t to double and in that
# case maintain the old model. Otherwise, they will default to the new model.
AC_ARG_ENABLE(s390-excess-float-precision,
  [AS_HELP_STRING([--enable-s390-excess-float-precision],
		  [on s390 targets, evaluate float with double precision
		   when in standards-conforming mode])],
  [],[enable_s390_excess_float_precision=auto])

case $target in
  s390*-linux*)
  if test x"$enable_s390_excess_float_precision" = xauto; then
    # Can we autodetect the behavior of the target libc?
    if test "$target" = "$host" -a "$host" = "$build"; then
      enable_s390_excess_float_precision=autodetect
    elif test "x$with_headers" != xno; then
      # cross build. are target headers available?
      # carefully coerce the build-system compiler to use target headers
      saved_CXXFLAGS="$CXXFLAGS"
      fixed_XGCC_FLAGS_FOR_TARGET=`echo "$XGCC_FLAGS_FOR_TARGET" | sed 's/-B/-idirafter/g'`
      CROSS_TEST_CXXFLAGS="-nostdinc $fixed_XGCC_FLAGS_FOR_TARGET"
      CXXFLAGS="$CROSS_TEST_CXXFLAGS"
      AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#include <math.h>
]])], [enable_s390_excess_float_precision=autodetect], [])
      CXXFLAGS="$saved_CXXFLAGS"
    fi

    if test x"$enable_s390_excess_float_precision" = xautodetect; then
      saved_CXXFLAGS="$CXXFLAGS"
      if ! test "$target" = "$host" -a "$host" = "$build"; then
        CXXFLAGS="$CROSS_TEST_CXXFLAGS"
	unset CROSS_TEST_CXXFLAGS
      fi
      AC_CACHE_CHECK([for glibc clamping float_t to double],
        gcc_cv_float_t_clamped_to_double, [
        AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
#define __FLT_EVAL_METHOD__ 0
#include <math.h>
int dummy[sizeof(float_t) == sizeof(double) ? 1 : -1];
]])],
          [gcc_cv_float_t_clamped_to_double=yes],
          [gcc_cv_float_t_clamped_to_double=no])])
      CXXFLAGS="$saved_CXXFLAGS"
      enable_s390_excess_float_precision="$gcc_cv_float_t_clamped_to_double"
    else
      # no way to detect behavior of target libc, default to new model
      enable_s390_excess_float_precision=no
    fi
  fi

  GCC_TARGET_TEMPLATE(ENABLE_S390_EXCESS_FLOAT_PRECISION)
  if test x"$enable_s390_excess_float_precision" = xyes; then
    AC_DEFINE(ENABLE_S390_EXCESS_FLOAT_PRECISION, 1,
[Define to enable evaluating float expressions with double precision in
standards-compatible mode on s390 targets.])
  fi
  ;;
esac

# Check if the linker supports '-z now'
ld_now_support=no
AC_MSG_CHECKING(linker -z now option)
if test x"$ld_is_gold" = xyes; then
  ld_now_support=yes
elif test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 14 -o "$gcc_cv_gld_major_version" -gt 2; then
    ld_now_support=yes
  fi
elif test x$gcc_cv_ld != x; then
  # Check if linker supports -z now
  if $gcc_cv_ld --help 2>&1 | grep -- '-z now' > /dev/null; then
    ld_now_support=yes
  fi
fi
AC_DEFINE_UNQUOTED(HAVE_LD_NOW_SUPPORT,
  [`if test x"$ld_now_support" = xyes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your linker supports -z now])
AC_MSG_RESULT($ld_now_support)

# Check if the linker supports '-z relro'
ld_relro_support=no
AC_MSG_CHECKING(linker -z relro option)
if test x"$ld_is_gold" = xyes; then
  ld_relro_support=yes
elif test $in_tree_ld = yes ; then
  if test "$gcc_cv_gld_major_version" -eq 2 -a "$gcc_cv_gld_minor_version" -ge 15 -o "$gcc_cv_gld_major_version" -gt 2; then
    ld_relro_support=yes
  fi
elif test x$gcc_cv_ld != x; then
  # Check if linker supports -z relro
  if $gcc_cv_ld --help 2>&1 | grep -- '-z relro' > /dev/null; then
    ld_relro_support=yes
  fi
fi
AC_DEFINE_UNQUOTED(HAVE_LD_RELRO_SUPPORT,
  [`if test x"$ld_relro_support" = xyes; then echo 1; else echo 0; fi`],
  [Define 0/1 if your linker supports -z relro])
AC_MSG_RESULT($ld_relro_support)

case $target_os in
linux* | gnu*)
  # -fhardened is only supported on GNU/Linux.
  fhardened_support=yes
  ;;
*)
  fhardened_support=no
  ;;
esac

AC_DEFINE_UNQUOTED(HAVE_FHARDENED_SUPPORT,
  [`if test x"$fhardened_support" = xyes; then echo 1; else echo 0; fi`],
  [Define 0/1 if -fhardened is supported])
AC_MSG_RESULT($fhardened_support)

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
