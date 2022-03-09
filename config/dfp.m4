# Copyright (C) 2010-2022 Free Software Foundation, Inc.

# This file is part of GCC.

# GCC is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 3, or (at your option) any
# later version.

# GCC is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
# License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

dnl @synopsis GCC_AC_ENABLE_DECIMAL_FLOAT([target triplet])
dnl
dnl Enable C extension for decimal float if target supports it.
dnl
dnl @author Andreas Krebbel  <Andreas.Krebbel@de.ibm.com>

AC_DEFUN([GCC_AC_ENABLE_DECIMAL_FLOAT],
[
AC_ARG_ENABLE(decimal-float,
[  --enable-decimal-float={no,yes,bid,dpd}
			enable decimal float extension to C.  Selecting 'bid'
			or 'dpd' choses which decimal floating point format
			to use],
[
  case $enable_decimal_float in
    yes | no | bid | dpd) default_decimal_float=$enable_decimal_float ;;
    *) AC_MSG_ERROR(['$enable_decimal_float' is an invalid value for --enable-decimal-float.
Valid choices are 'yes', 'bid', 'dpd', and 'no'.]) ;;
  esac
],
[
  case $1 in
    aarch64* | \
    powerpc*-*-linux* | i?86*-*-linux* | x86_64*-*-linux* | s390*-*-linux* | \
    i?86*-*-elfiamcu | i?86*-*-gnu* | x86_64*-*-gnu* | \
    i?86*-*-mingw* | x86_64*-*-mingw* | \
    i?86*-*-cygwin* | x86_64*-*-cygwin*)
      enable_decimal_float=yes
      ;;
    *)
      AC_MSG_WARN([decimal float is not supported for this target, ignored])
      enable_decimal_float=no
      ;;
  esac
])

# x86's use BID format instead of DPD
case x$enable_decimal_float in
  xyes)
    case $1 in
      aarch64* | i?86*-*-* | x86_64*-*-*)
	enable_decimal_float=bid
	;;
      *)
	enable_decimal_float=dpd
	;;
    esac
    default_decimal_float=$enable_decimal_float
    ;;
  xno)
    # ENABLE_DECIMAL_FLOAT is set to 0. But we have to have proper
    # dependency on libdecnumber.
    default_decimal_float=dpd
    ;;
esac
AC_SUBST(enable_decimal_float)

])
