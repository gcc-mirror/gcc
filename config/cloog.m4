# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any later
# version.
#
# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.
#
# Contributed by Andreas Simbuerger <simbuerg@fim.uni-passau.de>

# CLOOG_INIT_FLAGS ()
# -------------------------
# Provide configure switches for CLooG support.
# Initialize clooglibs/clooginc according to the user input.
AC_DEFUN([CLOOG_INIT_FLAGS],
[
  AC_ARG_WITH(cloog,
    [AS_HELP_STRING(
      [--with-cloog=PATH],
      [Specify prefix directory for the installed CLooG-PPL package.
       Equivalent to --with-cloog-include=PATH/include
       plus --with-cloog-lib=PATH/lib])])
  AC_ARG_WITH([cloog-include],
    [AS_HELP_STRING(
      [--with-cloog-include=PATH],
      [Specify directory for installed CLooG include files])])
  AC_ARG_WITH([cloog-lib],
    [AS_HELP_STRING(
      [--with-cloog-lib=PATH],
      [Specify the directory for the installed CLooG library])])

  AC_ARG_ENABLE(cloog-version-check,
    [AS_HELP_STRING(
      [--disable-cloog-version-check],
      [disable check for CLooG version])],
    ENABLE_CLOOG_CHECK=$enableval,
    ENABLE_CLOOG_CHECK=yes)
  
  # Initialize clooglibs and clooginc.
  case $with_cloog in
    no)
      clooglibs=
      clooginc=
      ;;
    "" | yes)
      ;;
    *)
      clooglibs="-L$with_cloog/lib"
      clooginc="-I$with_cloog/include"
      ;;
  esac
  if test "x${with_cloog_include}" != x ; then
    clooginc="-I$with_cloog_include"
  fi
  if test "x${with_cloog_lib}" != x; then
    clooglibs="-L$with_cloog_lib"
  fi
  dnl If no --with-cloog flag was specified and there is in-tree ClooG
  dnl source, set up flags to use that.
  if test "x${clooginc}" == x && test "x${clooglibs}" == x \
     && test -d ${srcdir}/cloog; then
     clooglibs='-L$$r/$(HOST_SUBDIR)/cloog/'"$lt_cv_objdir"' '
     clooginc='-I$$r/$(HOST_SUBDIR)/cloog/include -I$$s/cloog/include -I'${srcdir}'/cloog/include '
  fi

  clooginc="-DCLOOG_INT_GMP ${clooginc}"
  clooglibs="${clooglibs} -lcloog-isl ${isllibs}"

  dnl Flags needed for CLOOG
  AC_SUBST(clooglibs)
  AC_SUBST(clooginc)
]
)

# CLOOG_REQUESTED (ACTION-IF-REQUESTED, ACTION-IF-NOT)
# ----------------------------------------------------
# Provide actions for failed CLooG detection.
AC_DEFUN([CLOOG_REQUESTED],
[
  AC_REQUIRE([CLOOG_INIT_FLAGS])

  if test "x${with_cloog}" = xno; then
    $2
  elif test "x${with_cloog}" != x \
    || test "x${with_cloog_include}" != x \
    || test "x${with_cloog_lib}" != x ; then
    $1
  else
    $2
  fi
]
)

# _CLOOG_CHECK_CT_PROG(MAJOR, MINOR, REVISION)
# --------------------------------------------
# Helper for verifying CLooG's compile time version.
m4_define([_CLOOG_CHECK_CT_PROG],[AC_LANG_PROGRAM(
  [#include "cloog/version.h"],
  [#if CLOOG_VERSION_MAJOR != $1 \
    || CLOOG_VERSION_MINOR != $2 \
    || CLOOG_VERSION_REVISION < $3
    choke me
   #endif])])

# CLOOG_CHECK_VERSION CLOOG_CHECK_VERSION (MAJOR, MINOR, REVISION)
# ----------------------------------------------------------------
# Test the found CLooG to be exact of version MAJOR.MINOR and at least
# REVISION.
AC_DEFUN([CLOOG_CHECK_VERSION],
[
  AC_REQUIRE([CLOOG_INIT_FLAGS])

  if test "${ENABLE_CLOOG_CHECK}" = yes ; then
    _cloog_saved_CFLAGS=$CFLAGS
    _cloog_saved_LDFLAGS=$LDFLAGS

    CFLAGS="${_cloog_saved_CFLAGS} ${clooginc} ${islinc} ${gmpinc}"
    LDFLAGS="${_cloog_saved_LDFLAGS} ${clooglibs} ${isllibs} ${gmplib}"

    AC_CACHE_CHECK([for version $1.$2.$3 of CLooG],
      [gcc_cv_cloog],
      [AC_COMPILE_IFELSE([_CLOOG_CHECK_CT_PROG($1,$2,$3)],
	[gcc_cv_cloog=yes],
	[gcc_cv_cloog=no])])

    CFLAGS=$_cloog_saved_CFLAGS
    LDFLAGS=$_cloog_saved_LDFLAGS
  fi
]
)

# CLOOG_IF_FAILED (ACTION-IF-FAILED)
# ----------------------------------
# Executes ACTION-IF-FAILED, if GRAPHITE was requested and
# the checks failed.
AC_DEFUN([CLOOG_IF_FAILED],
[
  CLOOG_REQUESTED([graphite_requested=yes], [graphite_requested=no])
  
  if test "${gcc_cv_cloog}" = no ; then
    clooglibs=
    clooginc=
  fi

  if test "${graphite_requested}" = yes \
    && test "x${clooglibs}" = x \
    && test "x${clooginc}" = x ; then
    $1
  fi
]
)
