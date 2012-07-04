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
# Contributed by Richard Guenther <rguenther@suse.de>
# Based on cloog.m4

# ISL_INIT_FLAGS ()
# -------------------------
# Provide configure switches for ISL support.
# Initialize isllibs/islinc according to the user input.
AC_DEFUN([ISL_INIT_FLAGS],
[
  AC_ARG_WITH(isl,
    [AS_HELP_STRING(
      [--with-isl=PATH],
      [Specify prefix directory for the installed ISL package.
       Equivalent to --with-isl-include=PATH/include
       plus --with-isl-lib=PATH/lib])])
  AC_ARG_WITH([isl-include],
    [AS_HELP_STRING(
      [--with-isl-include=PATH],
      [Specify directory for installed ISL include files])])
  AC_ARG_WITH([isl-lib],
    [AS_HELP_STRING(
      [--with-isl-lib=PATH],
      [Specify the directory for the installed ISL library])])

  AC_ARG_ENABLE(isl-version-check,
    [AS_HELP_STRING(
      [--disable-isl-version-check],
      [disable check for ISL version])],
    ENABLE_ISL_CHECK=$enableval,
    ENABLE_ISL_CHECK=yes)
  
  # Initialize isllibs and islinc.
  case $with_isl in
    no)
      isllibs=
      islinc=
      ;;
    "" | yes)
      ;;
    *)
      isllibs="-L$with_isl/lib"
      islinc="-I$with_isl/include"
      ;;
  esac
  if test "x${with_isl_include}" != x ; then
    islinc="-I$with_isl_include"
  fi
  if test "x${with_isl_lib}" != x; then
    isllibs="-L$with_isl_lib"
  fi
  dnl If no --with-isl flag was specified and there is in-tree ISL
  dnl source, set up flags to use that and skip any version tests
  dnl as we cannot run them before building ISL.
  if test "x${islinc}" = x && test "x${isllibs}" = x \
     && test -d ${srcdir}/isl; then
    isllibs='-L$$r/$(HOST_SUBDIR)/isl/'"$lt_cv_objdir"' '
    islinc='-I$$r/$(HOST_SUBDIR)/isl/include -I$$s/isl/include'
    ENABLE_ISL_CHECK=no
  fi

  isllibs="${isllibs} -lisl"

  dnl Flags needed for ISL
  AC_SUBST(isllibs)
  AC_SUBST(islinc)
]
)

# ISL_REQUESTED (ACTION-IF-REQUESTED, ACTION-IF-NOT)
# ----------------------------------------------------
# Provide actions for failed ISL detection.
AC_DEFUN([ISL_REQUESTED],
[
  AC_REQUIRE([ISL_INIT_FLAGS])

  if test "x${with_isl}" = xno; then
    $2
  elif test "x${with_isl}" != x \
    || test "x${with_isl_include}" != x \
    || test "x${with_isl_lib}" != x ; then
    $1
  else
    $2
  fi
]
)

# _ISL_CHECK_CT_PROG(MAJOR, MINOR)
# --------------------------------------------
# Helper for verifying ISL compile time version.
m4_define([_ISL_CHECK_CT_PROG],[AC_LANG_PROGRAM(
  [#include <isl/version.h>
   #include <string.h>],
  [if (strncmp (isl_version (), "isl-$1.$2", strlen ("isl-$1.$2")) != 0)
     return 1;
   ])])

# ISL_CHECK_VERSION ISL_CHECK_VERSION (MAJOR, MINOR)
# ----------------------------------------------------------------
# Test the found ISL to be exact of version MAJOR.MINOR and at least
# REVISION.
AC_DEFUN([ISL_CHECK_VERSION],
[
  if test "${ENABLE_ISL_CHECK}" = yes ; then
    _isl_saved_CFLAGS=$CFLAGS
    _isl_saved_LDFLAGS=$LDFLAGS
    _isl_saved_LIBS=$LIBS

    CFLAGS="${_isl_saved_CFLAGS} ${islinc} ${gmpinc}"
    LDFLAGS="${_isl_saved_LDFLAGS} ${isllibs}"
    LIBS="${_isl_saved_LIBS} -lisl"
    echo $CFLAGS

    AC_CACHE_CHECK([for version $1.$2 of ISL],
      [gcc_cv_isl],
      [AC_RUN_IFELSE([_ISL_CHECK_CT_PROG($1,$2)],
	[gcc_cv_isl=yes],
	[gcc_cv_isl=no],
	[gcc_cv_isl=yes])])

    CFLAGS=$_isl_saved_CFLAGS
    LDFLAGS=$_isl_saved_LDFLAGS
    LIBS=$_isl_saved_LIBS
  fi
]
)

# ISL_IF_FAILED (ACTION-IF-FAILED)
# ----------------------------------
# Executes ACTION-IF-FAILED, if GRAPHITE was requested and
# the checks failed.
AC_DEFUN([ISL_IF_FAILED],
[
  ISL_REQUESTED([graphite_requested=yes], [graphite_requested=no])
  
  if test "${gcc_cv_isl}" = no ; then
    isllibs=
    islinc=
  fi

  if test "${graphite_requested}" = yes \
    && test "x${isllibs}" = x \
    && test "x${islinc}" = x ; then
    $1
  fi
]
)
