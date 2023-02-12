# configure.ac provides gm2spec.cc with access to config values.

# Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

AC_CHECK_FUNCS([stpcpy])

AC_CHECK_HEADERS(sys/types.h)
AC_HEADER_DIRENT
AC_CHECK_LIB([c],[opendir],[AC_DEFINE([HAVE_OPENDIR],[1],[found opendir])])
AC_CONFIG_HEADERS(gm2config.aci, [echo timestamp > stamp-h])
AC_OUTPUT
