dnl toolexeclibdir override support.
dnl Copyright (C) 2020  Free Software Foundation, Inc.
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 3, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; see the file COPYING3.  If not see
dnl <http://www.gnu.org/licenses/>.

AC_DEFUN([GCC_WITH_TOOLEXECLIBDIR],
[AC_ARG_WITH(toolexeclibdir,
  [AS_HELP_STRING([--with-toolexeclibdir=DIR],
		  [install libraries built with a cross compiler within DIR])],
  [dnl
case ${with_toolexeclibdir} in
  /)
    ;;
  */)
    with_toolexeclibdir=`echo $with_toolexeclibdir | sed 's,/$,,'`
    ;;
esac],
  [with_toolexeclibdir=no])
])
