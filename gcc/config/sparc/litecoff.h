/* Definitions of target machine for GCC, for SPARClite w/o FPU, COFF.
   Copyright (C) 1994, 1996, 2000, 2002 Free Software Foundation, Inc.
   Written by Ken Raeburn (raeburn@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define_std ("sparc");		\
	builtin_define_std ("sparclite");	\
    }						\
  while (0)

/* Default to stabs in COFF.  */

#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Support the ctors and dtors sections for g++.  */

#undef INIT_SECTION_ASM_OP

#undef DO_GLOBAL_CTORS_BODY
#undef DO_GLOBAL_DTORS_BODY

/* These compiler options take an argument.  We ignore -target for now.  */

#define WORD_SWITCH_TAKES_ARG(STR)				\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)				\
  || !strcmp (STR, "target") || !strcmp (STR, "assert"))

/* Output the label for a function definition.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
do {									\
  ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
  ASM_OUTPUT_LABEL (FILE, NAME);					\
} while (0)

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* How to renumber registers for dbx and gdb.  In the flat model, the frame
   pointer is really %i7.  */

#define DBX_REGISTER_NUMBER(REGNO) \
  (TARGET_FLAT && (REGNO) == HARD_FRAME_POINTER_REGNUM ? 31 : REGNO)
