/* Configuration common to all targets running the GNU system.  */

/*
Copyright (C) 1994-2024 Free Software Foundation, Inc.

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


#ifndef GNU_USER_TARGET_STARTFILE_SPEC
# warning This file should be included after gnu-user.h, to override its STARTFILE_SPEC
#endif

#undef	STARTFILE_SPEC
#if defined HAVE_LD_PIE
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:%{static-pie:grcrt0.o%s;static:gcrt0.o%s;:gcrt1.o%s};static-pie:rcrt0.o%s;static:crt0.o%s;" PIE_SPEC ":Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|static-pie|" PIE_SPEC ":crtbeginS.o%s;:crtbegin.o%s}"
#else
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:%{static:gcrt0.o%s;:gcrt1.o%s};static:crt0.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared:crtbeginS.o%s;:crtbegin.o%s}"
#endif
