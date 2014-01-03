/* ELF definitions for TI C6X
   Copyright (C) 2010-2014 Free Software Foundation, Inc.
   Contributed by Andrew Jenner <andrew@codesourcery.com>
   Contributed by Bernd Schmidt <bernds@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* crt0.o should come from the linker script, but for compatibility,
   we mention it here for -msim.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{msim:crt0%O%s} crti%O%s crtbegin%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC	"crtend%O%s crtn%O%s"

#undef  LIB_SPEC
#define LIB_SPEC "%{msim:--start-group -lc -lsim --end-group;" \
	":-lc}"

#undef LINK_SPEC
#define LINK_SPEC ENDIAN_LINK_SPEC
