# Autoconf M4 include file defining utility macros for complex Canadian
# cross builds.

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
