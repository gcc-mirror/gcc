/* Definitions of target machine for GNU compiler, for SPARClite w/o FPU, COFF.
   Copyright (C) 1994, 1996, 2000 Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com).

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

#include "sparc/lite.h"

#undef ASM_OUTPUT_IDENT

#undef SELECT_SECTION
#undef SELECT_RTX_SECTION
#define BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

#include "svr3.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Dsparclite -Acpu=sparc -Amachine=sparc"

/* Default to stabs in COFF.  */

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "dbxcoff.h"

/* Support the ctors and dtors sections for g++.  */

#undef INIT_SECTION_ASM_OP

/* A list of other sections which the compiler might be "in" at any
   given time.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const

/* A list of extra section function definitions.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION

#define INT_ASM_OP "\t.long\t"

#undef DO_GLOBAL_CTORS_BODY
#undef DO_GLOBAL_DTORS_BODY
