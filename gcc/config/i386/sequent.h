/* Definitions for Sequent Intel 386.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

#include "i386.h"

/* Use the BSD assembler syntax.  */

#include "bsd386.h"

/* By default, don't use IEEE compatible arithmetic comparisons
   because the assembler can't handle the fucom insn.
   Return float values in the 387.
   (TARGET_80387 | TARGET_FLOAT_RETURNS_IN_80387) */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT 0201

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Di386 -Dsequent"

/* Pass -Z and -ZO options to the linker.  */

#define LINK_SPEC "%{Z*}"

/* Link with libg.a when debugging, for dbx's sake.  */

#define LIB_SPEC "%{g:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* We don't want to output SDB debugging information.  */

#undef SDB_DEBUGGING_INFO

/* We want to output DBX debugging information.  */

#define DBX_DEBUGGING_INFO

/* Sequent Symmetry has size_t defined as int in /usr/include/sys/types.h */
#define SIZE_TYPE "int"

/* gcc order is ax, dx, cx, bx, si, di, bp, sp, st, st.
 * dbx order is ax, dx, cx, st(0), st(1), bx, si, di, st(2), st(3),
 * 		st(4), st(5), st(6), st(7), sp, bp  */

/* ??? The right thing would be to change the ordering of the
   registers to correspond to the conventions of this system,
   and get rid of DBX_REGISTER_NUMBER.  */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)		\
((n) < 3 ? (n) : (n) < 6 ? (n) + 2	\
 : (n) == 6 ? 15 : (n) == 7 ? 14 : 3)

/* Prevent anything from being allocated in the register pair cx/bx,
   since that would confuse GDB.  */

#undef HARD_REGNO_MODE_OK
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (((REGNO) < 2 ? 1							\
    : (REGNO) < 4 ? 1							\
    : (REGNO) >= 8 ? (GET_MODE_CLASS (MODE) == MODE_FLOAT		\
		    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)	\
    : (MODE) != QImode)							\
   && ! (REGNO == 2 && GET_MODE_UNIT_SIZE (MODE) > 4))

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry. */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tmovl $.LP%d,%%eax\n\tcall mcount\n", (LABELNO));

/* Assembler pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP ".shdata"

/* A C statement or statements which output an assembler instruction
   opcode to the stdio stream STREAM.  The macro-operand PTR is a
   variable of type `char *' which points to the opcode name in its
   "internal" form--the form that is written in the machine description.

   The Sequent assembler (identified as "Balance 8000 Assembler
   07/17/85 3.90" by "as -v") does not understand the `movs[bwl]' string
   move mnemonics - it uses `smov[bwl]' instead.  Change "movs" into
   "smov", carefully avoiding the sign-extend opcodes.  */

#define ASM_OUTPUT_OPCODE(STREAM, PTR)	\
{									\
  if ((PTR)[0] == 'm'							\
      && (PTR)[1] == 'o'						\
      && (PTR)[2] == 'v'						\
      && (PTR)[3] == 's'						\
      && ((PTR)[4] == 'b' || (PTR)[4] == 'w' || (PTR)[4] == 'l')	\
      && ((PTR)[5] == ' ' || (PTR)[5] == '\t'|| (PTR)[5] == '\0'))	\
    {									\
      fprintf (STREAM, "smov");						\
      (PTR) += 4;							\
    }									\
}

/* 10-Aug-92 pes  Local labels are prefixed with ".L" */
#undef LPREFIX
#define LPREFIX ".L"

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)\
  sprintf ((BUF), "*.%s%d", (PREFIX), (NUMBER))

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)
