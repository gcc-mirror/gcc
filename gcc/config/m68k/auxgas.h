/* Definitions for Motorola 680x0 running A/UX using GAS
   Copyright (C) 1996 Free Software Foundation, Inc.

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

#define USE_GAS

#ifndef __ASSEMBLY__

#include "m68k/m68k.h"
#include "m68k/coff.h"

#define ASM_SPEC "%{m68000:-Am68000 }%{m68030:-Am68030 }%{m68040:-Am68040 }"

/* Output #ident as a .ident.  */
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "\t.ident \"%s\"\n", NAME);

#ifdef IDENTIFY_WITH_IDENT
/* Put the GCC identification somewhere nicer, I think.
   Does the COFF GDB use the "gcc2_complied." symbol anyway? */
#define ASM_IDENTIFY_GCC(FILE) /* nothing */
#define ASM_IDENTIFY_LANGUAGE(FILE) \
  fprintf (FILE, "\t.ident \"GCC (%s) %s\"\n", lang_identify(), version_string)
#endif

#ifdef USE_COLLECT2
#undef ASM_OUTPUT_CONSTRUCTOR
#undef ASM_OUTPUT_DESTRUCTOR
/* for the sake of link-level compatibility with /bin/as version */
#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL
#endif

#define ADDITIONAL_REGISTER_NAMES  { "%fp", 14, "%a7", 15 }

#define ASM_MOV_INSN	"movel"

#define FUNCTION_PROFILER_SYMBOL "__mcount"

#endif /* !__ASSEMBLY__ */
