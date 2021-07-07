# configure.ac provides gm2spec.c with access to config values.

# Copyright (C) 2001-2021 Free Software Foundation, Inc.
# Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING3.  If not see
# <http://www.gnu.org/licenses/>.

AC_INIT(m2, [ ])

# Determine the host, build, and target systems
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Determine the noncanonical host name, for Ada.
ACX_NONCANONICAL_HOST

# Determine the noncanonical target name, for directory use.
ACX_NONCANONICAL_TARGET

AC_PATH_PROGS(AR_PATH, ${AR}, ar)

AC_CHECK_PROGS(regex_flex, flex)
if test x$regex_flex = "x" ; then
    AC_MSG_ERROR([flex is required to build GNU Modula-2 (hint install flex).])
fi

AC_CHECK_FUNCS([stpcpy])

AC_DEFINE_UNQUOTED(AR_PATH, "$AR_PATH", the path to target archiver)
AC_PATH_PROGS(RANLIB_PATH, ${RANLIB}, ranlib)
AC_DEFINE_UNQUOTED(RANLIB_PATH, "$RANLIB_PATH", the path to target ranlib)

AC_CHECK_HEADERS(sys/types.h)
AC_HEADER_DIRENT
AC_CHECK_LIB([c],[opendir],[AC_DEFINE([HAVE_OPENDIR],[1],[found opendir])])
AC_CONFIG_HEADERS(gm2config.h, [echo timestamp > stamp-h])
AC_OUTPUT
