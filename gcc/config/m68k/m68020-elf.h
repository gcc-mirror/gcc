/* Definitions of target machine for GNU compiler.  "naked" 68020,
   elf object files and debugging, version.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This comment is here to see if it will keep Sun's cpp from dying.  */

/* We need to override the default specs from elfos.h.  This suppresses the
   loading of crt0.o by gcc's default linker spec.  For embedded targets crt0
   now comes from the linker script.  */

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "crtbegin.o%s"

/* end of m68020-elf.h */
