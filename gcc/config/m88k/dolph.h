/* Definitions of target machine for GNU compiler.
   Motorola m88100 running the Dolphin Triton88 release 3.5/5.60.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "m88k.h"

/* Default switches */
#undef	TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_CHECK_ZERO_DIV	| \
			 MASK_OCS_DEBUG_INFO	| \
			 MASK_OCS_FRAME_POSITION)

/* Override svr3.h to link with ?crt0.o instead of ?crt1.o and ?crtn.o.
   From arul@sdsu.edu.  */
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
   "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

/* Profiled libraries live in a different directory but keep the same
   names other than that.  arul@sdsu.edu says -lg is always needed.  */
#undef	LIB_SPEC
#define LIB_SPEC "%{p:-L/lib/libp}%{pg:%{!p:-L/lib/libp}} -lg -lc"

/* Hot version of the profiler that uses r10 to pass the address of
   the counter.  the _gcc_mcount routine knows not to screw with
   the parameter registers.

   DG/UX does this; i wrote a gnu-c/88k specific version and put it
   in libgcc2.c -- RBE; this macro knows about the leading underscore
   convention.  */
#undef	FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  output_function_profiler (FILE, LABELNO, "_gcc_mcount", 0)

/* Various other changes that we want to have in place without
   too many changes to the m88k.h file.  */
#undef	USE_LIBG
#define	USE_LIBG

/* Don't output structure tag names when it causes a forward reference.
   Symptom?  */
#undef SDB_ALLOW_FORWARD_REFERENCES

/* Use T_ARG as T_VOID.  Symptom?  */
#define T_VOID T_ARG
