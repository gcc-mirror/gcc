/* Definitions for Intel 386 running SunOS 4.0.
   Copyright (C) 1988, 1995 Free Software Foundation, Inc.

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


#include "i386/i386.h"

/* Use the Sun assembler syntax.  */

#include "i386/sun386.h"

/* Use crt0.o as a startup file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} \
%{g:-lg} %{sun386:}"
/* That last item is just to prevent a spurious error.  */

#undef LINK_SPEC
#define LINK_SPEC \
  "%{!nostdlib:%{!r*:%{!e*:-e _start}}} -dc -dp %{static:-Bstatic}"

/* Extra switches to give the assembler.  */

#define ASM_SPEC "%{R} -i386 %{keep-local-as-symbols:-L}"

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Dsun386 -Dsun -Asystem=unix -Asystem=bsd"

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME);

/* We don't want to output SDB debugging information.  */

#undef SDB_DEBUGGING_INFO

/* We want to output DBX debugging information.  */

#define DBX_DEBUGGING_INFO

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Force structure alignment to the type used for a bitfield.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* This is partly guess.  */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
  ((n) == 0 ? 11 : (n)  == 1 ? 9 : (n) == 2 ? 10 : (n) == 3 ? 8	\
   : (n) == 4 ? 5 : (n) == 5 ? 4 : (n) == 6 ? 6 : (n))

/* Every debugger symbol must be in the text section.
   Otherwise the assembler or the linker screws up.  */

#define DEBUG_SYMS_TEXT
