# Autoconf M4 include file defining utility macros for complex Canadian
# cross builds.

dnl ####
dnl # _GCC_TOPLEV_NONCANONICAL_BUILD
dnl # $build_alias or canonical $build if blank.
dnl # Used when we would use $build_alias, but empty is not OK.
AC_DEFUN([_GCC_TOPLEV_NONCANONICAL_BUILD],
[AC_REQUIRE([AC_CANONICAL_BUILD]) []dnl
case ${build_alias} in
  "") build_noncanonical=${build} ;;
  *) build_noncanonical=${build_alias} ;;
esac
]) []dnl # _GCC_TOPLEV_NONCANONICAL_BUILD

dnl ####
dnl # _GCC_TOPLEV_NONCANONICAL_HOST
dnl # $host_alias, or $build_noncanonical if blank.
dnl # Used when we would use $host_alias, but empty is not OK.
AC_DEFUN([_GCC_TOPLEV_NONCANONICAL_HOST],
[AC_REQUIRE([_GCC_TOPLEV_NONCANONICAL_BUILD]) []dnl
case ${host_alias} in
  "") host_noncanonical=${build_noncanonical} ;;
  *) host_noncanonical=${host_alias} ;;
esac
]) []dnl # _GCC_TOPLEV_NONCANONICAL_HOST

dnl ####
dnl # _GCC_TOPLEV_NONCANONICAL_TARGET
dnl # $target_alias or $host_noncanonical if blank.
dnl # Used when we would use $target_alias, but empty is not OK.
AC_DEFUN([_GCC_TOPLEV_NONCANONICAL_TARGET],
[AC_REQUIRE([_GCC_TOPLEV_NONCANONICAL_HOST]) []dnl
case ${target_alias} in
  "") target_noncanonical=${host_noncanonical} ;;
  *) target_noncanonical=${target_alias} ;;
esac
]) []dnl # _GCC_TOPLEV_NONCANONICAL_TARGET

dnl ####
dnl # GCC_TOPLEV_SUBDIRS
dnl # GCC & friends build 'build', 'host', and 'target' tools.  These must
dnl # be separated into three well-known subdirectories of the build directory:
dnl # build_subdir, host_subdir, and target_subdir.  The values are determined
dnl # here so that they can (theoretically) be changed in the future.  They
dnl # were previously reproduced across many different files.
dnl #
dnl # This logic really amounts to very little with autoconf 2.13; it will
dnl # amount to a lot more with autoconf 2.5x.
AC_DEFUN([GCC_TOPLEV_SUBDIRS],
[AC_REQUIRE([_GCC_TOPLEV_NONCANONICAL_TARGET]) []dnl
AC_REQUIRE([_GCC_TOPLEV_NONCANONICAL_BUILD]) []dnl
# Prefix 'build-' so this never conflicts with target_subdir.
build_subdir="build-${build_noncanonical}"
# Not really a subdirectory, but here for completeness.
host_subdir=.
# No prefix.
target_subdir=${target_noncanonical}
AC_SUBST([build_subdir]) []dnl
AC_SUBST([host_subdir]) []dnl
AC_SUBST([target_subdir]) []dnl
]) []dnl # GCC_TOPLEV_SUBDIRS


####
# _NCN_TOOL_PREFIXES:  Some stuff that oughtta be done in AC_CANONICAL_SYSTEM 
# or AC_INIT.
# These demand that AC_CANONICAL_SYSTEM be called beforehand.
AC_DEFUN([_NCN_TOOL_PREFIXES],
[ncn_tool_prefix=
test -n "$host_alias" && ncn_tool_prefix=$host_alias-
ncn_target_tool_prefix=
test -n "$target_alias" && ncn_target_tool_prefix=$target_alias-
]) []dnl # _NCN_TOOL_PREFIXES

####
# NCN_CHECK_TARGET_TOOL(variable, prog-to-check-for,[value-if-not-found],[path])
# Like AC_CHECK_TOOL, but tries a prefix of the target, not the host.
# Code is pretty much lifted from autoconf2.53.

AC_DEFUN([NCN_CHECK_TARGET_TOOL],
[AC_REQUIRE([_NCN_TOOL_PREFIXES]) []dnl
if test -n "$ncn_target_tool_prefix"; then
  AC_CHECK_PROG([$1], [${ncn_target_tool_prefix}$2], 
                [${ncn_target_tool_prefix}$2], , [$4])
fi
if test -z "$ac_cv_prog_$1" ; then
  ncn_cv_$1=$$1
  AC_CHECK_PROG([ncn_cv_$1], [$2], [$2], [$3], [$4])
  $1=$ncn_cv_$1
else
  $1="$ac_cv_prog_$1"
fi
]) []dnl # NCN_CHECK_TARGET_TOOL


####
# NCN_STRICT_CHECK_TOOL(variable, prog-to-check-for,[value-if-not-found],[path])
# Like AC_CHECK_TOOL, but requires the prefix if build!=host.

AC_DEFUN([NCN_STRICT_CHECK_TOOL],
[AC_REQUIRE([_NCN_TOOL_PREFIXES]) []dnl
if test -n "$ncn_tool_prefix"; then
  AC_CHECK_PROG([$1], [${ncn_tool_prefix}$2], 
                [${ncn_tool_prefix}$2], , [$4])
fi
if test -z "$ac_cv_prog_$1" ; then
  if test $build = $host ; then
    ncn_cv_$1=$$1
    AC_CHECK_PROG([ncn_cv_$1], [$2], [$2], [ifelse([$3],[],[$2],[$3])], [$4]) 
    $1=$ncn_cv_$1
  else
    $1="ifelse([$3],[],[${ncn_tool_prefix}$2],[$3])"
  fi
else
  $1="$ac_cv_prog_$1"
fi
]) []dnl # NCN_STRICT_CHECK_TOOL


####
# NCN_STRICT_CHECK_TARGET_TOOL(variable, prog-to-check-for,[value-if-not-found],[path])
# Like NCN_CHECK_TARGET_TOOL, but requires the prefix if build!=target.

AC_DEFUN([NCN_STRICT_CHECK_TARGET_TOOL],
[AC_REQUIRE([_NCN_TOOL_PREFIXES]) []dnl
if test -n "$ncn_target_tool_prefix"; then
  AC_CHECK_PROG([$1], [${ncn_target_tool_prefix}$2], 
                [${ncn_target_tool_prefix}$2], , [$4])
fi
if test -z "$ac_cv_prog_$1" ; then
  if test $build = $target ; then
    ncn_cv_$1=$$1
    AC_CHECK_PROG([ncn_cv_$1], [$2], [$2], [ifelse([$3],[],[$2],[$3])], [$4]) 
    $1=$ncn_cv_$1
  else
    $1="ifelse([$3],[],[${ncn_target_tool_prefix}$2],[$3])"
  fi
else
  $1="$ac_cv_prog_$1"
fi
]) []dnl # NCN_STRICT_CHECK_TARGET_TOOL

###
# AC_PROG_CPP_WERROR
# Used for autoconf 2.5x to force AC_PREPROC_IFELSE to reject code which
# triggers warnings from the preprocessor.  Will be in autoconf 2.58.
# For now, using this also overrides header checks to use only the
# preprocessor (matches 2.13 behavior; matching 2.58's behavior is a
# bit harder from here).
# Eventually autoconf will default to checking headers with the compiler
# instead, and we'll have to do this differently.

AC_DEFUN([AC_PROG_CPP_WERROR],
[AC_REQUIRE([AC_PROG_CPP])dnl
m4_define([AC_CHECK_HEADER],m4_defn([_AC_CHECK_HEADER_OLD]))
ac_c_preproc_warn_flag=yes])# AC_PROG_CPP_WERROR
