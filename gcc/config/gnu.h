/* Configuration common to all targets running the GNU system.  */

/*
Copyright (C) 1994-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC.  If not, see <http://www.gnu.org/licenses/>.
*/

/* Provide GCC options for standard feature-test macros.  */
#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

/* Default C library spec.  */
#undef LIB_SPEC
#define LIB_SPEC "%{pthread:-lpthread} %{pg|p|profile:-lc_p;:-lc}"

#undef GNU_USER_TARGET_OS_CPP_BUILTINS
#define GNU_USER_TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define ("__gnu_hurd__");	\
	builtin_define ("__GNU__");		\
	builtin_define_std ("unix");		\
	builtin_define ("__MACH__");		\
	builtin_assert ("system=gnu");		\
	builtin_assert ("system=mach");		\
	builtin_assert ("system=unix");		\
	builtin_assert ("system=posix");	\
    } while (0)
