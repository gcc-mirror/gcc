/* Definitions of target machine for GNU compiler.  "naked" 68020,
   elf object files and debugging, version.
   Copyright (C) 1987, 1988, 1992, 1995, 1996 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This comment is here to see if it will keep Sun's cpp from dying.  */

#include "m68k/m68k-none.h"
#include "m68k/m68kelf.h"
#include "elfos.h"
/* m68k/m68kemb.h sets some macros in ways that override the svr4 abi.  */
#include "m68k/m68kemb.h" 

/* We need to override the default specs from elfos.h.  This suppresses the
   loading of crt0.o by gcc's default linker spec.  For embedded targets crt0
   now comes from the linker script.  */

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC "crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s"

#define LIB_SPEC "-lc"

/* ??? Quick hack to get constructors working.  Make this look more like a
   COFF target, so the existing dejagnu/libgloss support works.  A better
   solution would be to make the necessary dejagnu and libgloss changes so
   that we can use normal the ELF constructor mechanism.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""
#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

/* end of m68020-elf.h */
