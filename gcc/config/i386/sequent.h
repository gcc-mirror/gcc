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

/* By default, target has a 80387.  */

#define TARGET_DEFAULT 1

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

/* Floating-point return values come in the FP register.  */

#define VALUE_REGNO(MODE) \
  (((MODE)==SFmode || (MODE)==DFmode) ? FIRST_FLOAT_REG : 0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N)== FIRST_FLOAT_REG)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry. */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tmovl $LP%d,%%eax\n\tcall mcount\n", (LABELNO));

/* Assember pseudo-op for shared data segment. */
#define SHARED_SECTION_ASM_OP ".shdata"
