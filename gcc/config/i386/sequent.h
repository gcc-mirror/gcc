/* Definitions for Sequent Intel 386.
   Copyright (C) 1988, 1994, 1999 Free Software Foundation, Inc.

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

/* Use the BSD assembler syntax.  */

#include "i386/bsd.h"

/* By default, don't use IEEE compatible arithmetic comparisons
   because the assembler can't handle the fucom insn.
   Return float values in the 387.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_80387 | MASK_FLOAT_RETURNS)

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Di386 -Dsequent -Asystem(unix) -Acpu(i386) -Amachine(i386)"

/* Pass -Z and -ZO options to the linker.  */

#define LINK_SPEC "%{Z*}"

#if 0 /* Dynix 3.1 is said to accept -L.  */
/* Dynix V3.0.12 doesn't accept -L at all.  */

#define LINK_LIBGCC_SPECIAL
#endif

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

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)		\
((n) < 3 ? (n) : (n) < 6 ? (n) + 2	\
 : (n) == 6 ? 15 : (n) == 7 ? 14 : 3)

/* malcolmp@hydra.maths.unsw.EDU.AU says these two definitions
   fix trouble in dbx.  */
#undef DBX_OUTPUT_LBRAC
#define DBX_OUTPUT_LBRAC(file,name)	\
	      fprintf (asmfile, "%s %d,0,%d,", ASM_STABN_OP, N_LBRAC, depth); \
	      assemble_name (asmfile, buf); \
	      fprintf (asmfile, "\n");

#undef DBX_OUTPUT_RBRAC
#define DBX_OUTPUT_RBRAC(file,name)	\
	      fprintf (asmfile, "%s %d,0,%d,", ASM_STABN_OP, N_RBRAC, depth); \
	      assemble_name (asmfile, buf); \
	      fprintf (asmfile, "\n");

/* Prevent anything from being allocated in the register pair cx/bx,
   since that would confuse GDB.  */

#undef HARD_REGNO_MODE_OK
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (((REGNO) < 2 ? 1							\
    : (REGNO) < 4 ? 1							\
    : FP_REGNO_P (REGNO) ? (GET_MODE_CLASS (MODE) == MODE_FLOAT         \
			    || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT) \
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

/* The native compiler passes the address of the returned structure in eax. */
#undef STRUCT_VALUE
#undef STRUCT_VALUE_INCOMING
#define STRUCT_VALUE_REGNUM	0
