/* Subroutines used for code generation on IA-32.
   Copyright (C) 1988, 1992, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "basic-block.h"
#include "ggc.h"
#include "target.h"
#include "target-def.h"
#include "langhooks.h"

#ifndef CHECK_STACK_LIMIT
#define CHECK_STACK_LIMIT (-1)
#endif

/* Processor costs (relative to an add) */
static const
struct processor_costs size_cost = {	/* costs for tunning for size */
  2,					/* cost of an add instruction */
  3,					/* cost of a lea instruction */
  2,					/* variable shift costs */
  3,					/* constant shift costs */
  3,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  3,					/* cost of a divide/mod */
  3,					/* cost of movsx */
  3,					/* cost of movzx */
  0,					/* "large" insn */
  2,					/* MOVE_RATIO */
  2,					/* cost for loading QImode using movzbl */
  {2, 2, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 2, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 2},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {2, 2, 2},				/* cost of loading integer registers */
  3,					/* cost of moving MMX register */
  {3, 3},				/* cost of loading MMX registers
					   in SImode and DImode */
  {3, 3},				/* cost of storing MMX registers
					   in SImode and DImode */
  3,					/* cost of moving SSE register */
  {3, 3, 3},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {3, 3, 3},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  2,					/* cost of FADD and FSUB insns.  */
  2,					/* cost of FMUL instruction.  */
  2,					/* cost of FDIV instruction.  */
  2,					/* cost of FABS instruction.  */
  2,					/* cost of FCHS instruction.  */
  2,					/* cost of FSQRT instruction.  */
};

/* Processor costs (relative to an add) */
static const
struct processor_costs i386_cost = {	/* 386 specific costs */
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  3,					/* variable shift costs */
  2,					/* constant shift costs */
  6,					/* cost of starting a multiply */
  1,					/* cost of multiply per each bit set */
  23,					/* cost of a divide/mod */
  3,					/* cost of movsx */
  2,					/* cost of movzx */
  15,					/* "large" insn */
  3,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {8, 8, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {8, 8, 8},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {4, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 8},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 8, 16},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 8, 16},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  23,					/* cost of FADD and FSUB insns.  */
  27,					/* cost of FMUL instruction.  */
  88,					/* cost of FDIV instruction.  */
  22,					/* cost of FABS instruction.  */
  24,					/* cost of FCHS instruction.  */
  122,					/* cost of FSQRT instruction.  */
};

static const
struct processor_costs i486_cost = {	/* 486 specific costs */
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  3,					/* variable shift costs */
  2,					/* constant shift costs */
  12,					/* cost of starting a multiply */
  1,					/* cost of multiply per each bit set */
  40,					/* cost of a divide/mod */
  3,					/* cost of movsx */
  2,					/* cost of movzx */
  15,					/* "large" insn */
  3,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {8, 8, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {8, 8, 8},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {4, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 8},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 8, 16},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 8, 16},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  8,					/* cost of FADD and FSUB insns.  */
  16,					/* cost of FMUL instruction.  */
  73,					/* cost of FDIV instruction.  */
  3,					/* cost of FABS instruction.  */
  3,					/* cost of FCHS instruction.  */
  83,					/* cost of FSQRT instruction.  */
};

static const
struct processor_costs pentium_cost = {
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  4,					/* variable shift costs */
  1,					/* constant shift costs */
  11,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  25,					/* cost of a divide/mod */
  3,					/* cost of movsx */
  2,					/* cost of movzx */
  8,					/* "large" insn */
  6,					/* MOVE_RATIO */
  6,					/* cost for loading QImode using movzbl */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 6},				/* cost of loading integer registers */
  8,					/* cost of moving MMX register */
  {8, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {8, 8},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 8, 16},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 8, 16},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  3,					/* cost of FADD and FSUB insns.  */
  3,					/* cost of FMUL instruction.  */
  39,					/* cost of FDIV instruction.  */
  1,					/* cost of FABS instruction.  */
  1,					/* cost of FCHS instruction.  */
  70,					/* cost of FSQRT instruction.  */
};

static const
struct processor_costs pentiumpro_cost = {
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  1,					/* variable shift costs */
  1,					/* constant shift costs */
  4,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  17,					/* cost of a divide/mod */
  1,					/* cost of movsx */
  1,					/* cost of movzx */
  8,					/* "large" insn */
  6,					/* MOVE_RATIO */
  2,					/* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 2, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 6},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {2, 2},				/* cost of loading MMX registers
					   in SImode and DImode */
  {2, 2},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {2, 2, 8},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {2, 2, 8},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  32,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  3,					/* cost of FADD and FSUB insns.  */
  5,					/* cost of FMUL instruction.  */
  56,					/* cost of FDIV instruction.  */
  2,					/* cost of FABS instruction.  */
  2,					/* cost of FCHS instruction.  */
  56,					/* cost of FSQRT instruction.  */
};

static const
struct processor_costs k6_cost = {
  1,					/* cost of an add instruction */
  2,					/* cost of a lea instruction */
  1,					/* variable shift costs */
  1,					/* constant shift costs */
  3,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  18,					/* cost of a divide/mod */
  2,					/* cost of movsx */
  2,					/* cost of movzx */
  8,					/* "large" insn */
  4,					/* MOVE_RATIO */
  3,					/* cost for loading QImode using movzbl */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 3, 2},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {6, 6, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 4},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {2, 2},				/* cost of loading MMX registers
					   in SImode and DImode */
  {2, 2},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {2, 2, 8},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {2, 2, 8},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  6,					/* MMX or SSE register to integer */
  32,					/* size of prefetch block */
  1,					/* number of parallel prefetches */
  2,					/* cost of FADD and FSUB insns.  */
  2,					/* cost of FMUL instruction.  */
  56,					/* cost of FDIV instruction.  */
  2,					/* cost of FABS instruction.  */
  2,					/* cost of FCHS instruction.  */
  56,					/* cost of FSQRT instruction.  */
};

static const
struct processor_costs athlon_cost = {
  1,					/* cost of an add instruction */
  2,					/* cost of a lea instruction */
  1,					/* variable shift costs */
  1,					/* constant shift costs */
  5,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  42,					/* cost of a divide/mod */
  1,					/* cost of movsx */
  1,					/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {3, 4, 3},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {3, 4, 3},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {4, 4, 12},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 8},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {4, 4},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 4, 6},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 5},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  5,					/* MMX or SSE register to integer */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  4,					/* cost of FADD and FSUB insns.  */
  4,					/* cost of FMUL instruction.  */
  24,					/* cost of FDIV instruction.  */
  2,					/* cost of FABS instruction.  */
  2,					/* cost of FCHS instruction.  */
  35,					/* cost of FSQRT instruction.  */
};

static const
struct processor_costs pentium4_cost = {
  1,					/* cost of an add instruction */
  1,					/* cost of a lea instruction */
  8,					/* variable shift costs */
  8,					/* constant shift costs */
  30,					/* cost of starting a multiply */
  0,					/* cost of multiply per each bit set */
  112,					/* cost of a divide/mod */
  1,					/* cost of movsx */
  1,					/* cost of movzx */
  16,					/* "large" insn */
  6,					/* MOVE_RATIO */
  2,					/* cost for loading QImode using movzbl */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 3, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 6},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {2, 2},				/* cost of loading MMX registers
					   in SImode and DImode */
  {2, 2},				/* cost of storing MMX registers
					   in SImode and DImode */
  12,					/* cost of moving SSE register */
  {12, 12, 12},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {2, 2, 8},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  10,					/* MMX or SSE register to integer */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  5,					/* cost of FADD and FSUB insns.  */
  7,					/* cost of FMUL instruction.  */
  43,					/* cost of FDIV instruction.  */
  2,					/* cost of FABS instruction.  */
  2,					/* cost of FCHS instruction.  */
  43,					/* cost of FSQRT instruction.  */
};

const struct processor_costs *ix86_cost = &pentium_cost;

/* Processor feature/optimization bitmasks.  */
#define m_386 (1<<PROCESSOR_I386)
#define m_486 (1<<PROCESSOR_I486)
#define m_PENT (1<<PROCESSOR_PENTIUM)
#define m_PPRO (1<<PROCESSOR_PENTIUMPRO)
#define m_K6  (1<<PROCESSOR_K6)
#define m_ATHLON  (1<<PROCESSOR_ATHLON)
#define m_PENT4  (1<<PROCESSOR_PENTIUM4)

const int x86_use_leave = m_386 | m_K6 | m_ATHLON;
const int x86_push_memory = m_386 | m_K6 | m_ATHLON | m_PENT4;
const int x86_zero_extend_with_and = m_486 | m_PENT;
const int x86_movx = m_ATHLON | m_PPRO | m_PENT4 /* m_386 | m_K6 */;
const int x86_double_with_add = ~m_386;
const int x86_use_bit_test = m_386;
const int x86_unroll_strlen = m_486 | m_PENT | m_PPRO | m_ATHLON | m_K6;
const int x86_cmove = m_PPRO | m_ATHLON | m_PENT4;
const int x86_3dnow_a = m_ATHLON;
const int x86_deep_branch = m_PPRO | m_K6 | m_ATHLON | m_PENT4;
const int x86_branch_hints = m_PENT4;
const int x86_use_sahf = m_PPRO | m_K6 | m_PENT4;
const int x86_partial_reg_stall = m_PPRO;
const int x86_use_loop = m_K6;
const int x86_use_fiop = ~(m_PPRO | m_ATHLON | m_PENT);
const int x86_use_mov0 = m_K6;
const int x86_use_cltd = ~(m_PENT | m_K6);
const int x86_read_modify_write = ~m_PENT;
const int x86_read_modify = ~(m_PENT | m_PPRO);
const int x86_split_long_moves = m_PPRO;
const int x86_promote_QImode = m_K6 | m_PENT | m_386 | m_486 | m_ATHLON;
const int x86_fast_prefix = ~(m_PENT | m_486 | m_386);
const int x86_single_stringop = m_386 | m_PENT4;
const int x86_qimode_math = ~(0);
const int x86_promote_qi_regs = 0;
const int x86_himode_math = ~(m_PPRO);
const int x86_promote_hi_regs = m_PPRO;
const int x86_sub_esp_4 = m_ATHLON | m_PPRO | m_PENT4;
const int x86_sub_esp_8 = m_ATHLON | m_PPRO | m_386 | m_486 | m_PENT4;
const int x86_add_esp_4 = m_ATHLON | m_K6 | m_PENT4;
const int x86_add_esp_8 = m_ATHLON | m_PPRO | m_K6 | m_386 | m_486 | m_PENT4;
const int x86_integer_DFmode_moves = ~(m_ATHLON | m_PENT4 | m_PPRO);
const int x86_partial_reg_dependency = m_ATHLON | m_PENT4;
const int x86_memory_mismatch_stall = m_ATHLON | m_PENT4;
const int x86_accumulate_outgoing_args = m_ATHLON | m_PENT4 | m_PPRO;
const int x86_prologue_using_move = m_ATHLON | m_PENT4 | m_PPRO;
const int x86_epilogue_using_move = m_ATHLON | m_PENT4 | m_PPRO;
const int x86_decompose_lea = m_PENT4;
const int x86_shift1 = ~m_486;
const int x86_arch_always_fancy_math_387 = m_PENT | m_PPRO | m_ATHLON | m_PENT4;

/* In case the avreage insn count for single function invocation is
   lower than this constant, emit fast (but longer) prologue and
   epilogue code.  */
#define FAST_PROLOGUE_INSN_COUNT 30

/* Set by prologue expander and used by epilogue expander to determine
   the style used.  */
static int use_fast_prologue_epilogue;

/* Names for 8 (low), 8 (high), and 16-bit registers, respectively.  */
static const char *const qi_reg_name[] = QI_REGISTER_NAMES;
static const char *const qi_high_reg_name[] = QI_HIGH_REGISTER_NAMES;
static const char *const hi_reg_name[] = HI_REGISTER_NAMES;

/* Array of the smallest class containing reg number REGNO, indexed by
   REGNO.  Used by REGNO_REG_CLASS in i386.h.  */

enum reg_class const regclass_map[FIRST_PSEUDO_REGISTER] =
{
  /* ax, dx, cx, bx */
  AREG, DREG, CREG, BREG,
  /* si, di, bp, sp */
  SIREG, DIREG, NON_Q_REGS, NON_Q_REGS,
  /* FP registers */
  FP_TOP_REG, FP_SECOND_REG, FLOAT_REGS, FLOAT_REGS,
  FLOAT_REGS, FLOAT_REGS, FLOAT_REGS, FLOAT_REGS,
  /* arg pointer */
  NON_Q_REGS,
  /* flags, fpsr, dirflag, frame */
  NO_REGS, NO_REGS, NO_REGS, NON_Q_REGS,
  SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  SSE_REGS, SSE_REGS,
  MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS,
  MMX_REGS, MMX_REGS,
  NON_Q_REGS, NON_Q_REGS, NON_Q_REGS, NON_Q_REGS,
  NON_Q_REGS, NON_Q_REGS, NON_Q_REGS, NON_Q_REGS,
  SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  SSE_REGS, SSE_REGS,
};

/* The "default" register map used in 32bit mode.  */

int const dbx_register_map[FIRST_PSEUDO_REGISTER] =
{
  0, 2, 1, 3, 6, 7, 4, 5,		/* general regs */
  12, 13, 14, 15, 16, 17, 18, 19,	/* fp regs */
  -1, -1, -1, -1, -1,			/* arg, flags, fpsr, dir, frame */
  21, 22, 23, 24, 25, 26, 27, 28,	/* SSE */
  29, 30, 31, 32, 33, 34, 35, 36,       /* MMX */
  -1, -1, -1, -1, -1, -1, -1, -1,	/* extended integer registers */
  -1, -1, -1, -1, -1, -1, -1, -1,	/* extended SSE registers */
};

static int const x86_64_int_parameter_registers[6] =
{
  5 /*RDI*/, 4 /*RSI*/, 1 /*RDX*/, 2 /*RCX*/,
  FIRST_REX_INT_REG /*R8 */, FIRST_REX_INT_REG + 1 /*R9 */
};

static int const x86_64_int_return_registers[4] =
{
  0 /*RAX*/, 1 /*RDI*/, 5 /*RDI*/, 4 /*RSI*/
};

/* The "default" register map used in 64bit mode.  */
int const dbx64_register_map[FIRST_PSEUDO_REGISTER] =
{
  0, 1, 2, 3, 4, 5, 6, 7,		/* general regs */
  33, 34, 35, 36, 37, 38, 39, 40,	/* fp regs */
  -1, -1, -1, -1, -1,			/* arg, flags, fpsr, dir, frame */
  17, 18, 19, 20, 21, 22, 23, 24,	/* SSE */
  41, 42, 43, 44, 45, 46, 47, 48,       /* MMX */
  8,9,10,11,12,13,14,15,		/* extended integer registers */
  25, 26, 27, 28, 29, 30, 31, 32,	/* extended SSE registers */
};

/* Define the register numbers to be used in Dwarf debugging information.
   The SVR4 reference port C compiler uses the following register numbers
   in its Dwarf output code:
	0 for %eax (gcc regno = 0)
	1 for %ecx (gcc regno = 2)
	2 for %edx (gcc regno = 1)
	3 for %ebx (gcc regno = 3)
	4 for %esp (gcc regno = 7)
	5 for %ebp (gcc regno = 6)
	6 for %esi (gcc regno = 4)
	7 for %edi (gcc regno = 5)
   The following three DWARF register numbers are never generated by
   the SVR4 C compiler or by the GNU compilers, but SDB on x86/svr4
   believes these numbers have these meanings.
	8  for %eip    (no gcc equivalent)
	9  for %eflags (gcc regno = 17)
	10 for %trapno (no gcc equivalent)
   It is not at all clear how we should number the FP stack registers
   for the x86 architecture.  If the version of SDB on x86/svr4 were
   a bit less brain dead with respect to floating-point then we would
   have a precedent to follow with respect to DWARF register numbers
   for x86 FP registers, but the SDB on x86/svr4 is so completely
   broken with respect to FP registers that it is hardly worth thinking
   of it as something to strive for compatibility with.
   The version of x86/svr4 SDB I have at the moment does (partially)
   seem to believe that DWARF register number 11 is associated with
   the x86 register %st(0), but that's about all.  Higher DWARF
   register numbers don't seem to be associated with anything in
   particular, and even for DWARF regno 11, SDB only seems to under-
   stand that it should say that a variable lives in %st(0) (when
   asked via an `=' command) if we said it was in DWARF regno 11,
   but SDB still prints garbage when asked for the value of the
   variable in question (via a `/' command).
   (Also note that the labels SDB prints for various FP stack regs
   when doing an `x' command are all wrong.)
   Note that these problems generally don't affect the native SVR4
   C compiler because it doesn't allow the use of -O with -g and
   because when it is *not* optimizing, it allocates a memory
   location for each floating-point variable, and the memory
   location is what gets described in the DWARF AT_location
   attribute for the variable in question.
   Regardless of the severe mental illness of the x86/svr4 SDB, we
   do something sensible here and we use the following DWARF
   register numbers.  Note that these are all stack-top-relative
   numbers.
	11 for %st(0) (gcc regno = 8)
	12 for %st(1) (gcc regno = 9)
	13 for %st(2) (gcc regno = 10)
	14 for %st(3) (gcc regno = 11)
	15 for %st(4) (gcc regno = 12)
	16 for %st(5) (gcc regno = 13)
	17 for %st(6) (gcc regno = 14)
	18 for %st(7) (gcc regno = 15)
*/
int const svr4_dbx_register_map[FIRST_PSEUDO_REGISTER] =
{
  0, 2, 1, 3, 6, 7, 5, 4,		/* general regs */
  11, 12, 13, 14, 15, 16, 17, 18,	/* fp regs */
  -1, 9, -1, -1, -1,			/* arg, flags, fpsr, dir, frame */
  21, 22, 23, 24, 25, 26, 27, 28,	/* SSE registers */
  29, 30, 31, 32, 33, 34, 35, 36,	/* MMX registers */
  -1, -1, -1, -1, -1, -1, -1, -1,	/* extemded integer registers */
  -1, -1, -1, -1, -1, -1, -1, -1,	/* extemded SSE registers */
};

/* Test and compare insns in i386.md store the information needed to
   generate branch and scc insns here.  */

rtx ix86_compare_op0 = NULL_RTX;
rtx ix86_compare_op1 = NULL_RTX;

/* The encoding characters for the four TLS models present in ELF.  */

static char const tls_model_chars[] = " GLil";

#define MAX_386_STACK_LOCALS 3
/* Size of the register save area.  */
#define X86_64_VARARGS_SIZE (REGPARM_MAX * UNITS_PER_WORD + SSE_REGPARM_MAX * 16)

/* Define the structure for the machine field in struct function.  */
struct machine_function GTY(())
{
  rtx stack_locals[(int) MAX_MACHINE_MODE][MAX_386_STACK_LOCALS];
  const char *some_ld_name;
  int save_varrargs_registers;
  int accesses_prev_frame;
};

#define ix86_stack_locals (cfun->machine->stack_locals)
#define ix86_save_varrargs_registers (cfun->machine->save_varrargs_registers)

/* Structure describing stack frame layout.
   Stack grows downward:

   [arguments]
					      <- ARG_POINTER
   saved pc

   saved frame pointer if frame_pointer_needed
					      <- HARD_FRAME_POINTER
   [saved regs]

   [padding1]          \
		        )
   [va_arg registers]  (
		        > to_allocate	      <- FRAME_POINTER
   [frame]	       (
		        )
   [padding2]	       /
  */
struct ix86_frame
{
  int nregs;
  int padding1;
  int va_arg_size;
  HOST_WIDE_INT frame;
  int padding2;
  int outgoing_arguments_size;
  int red_zone_size;

  HOST_WIDE_INT to_allocate;
  /* The offsets relative to ARG_POINTER.  */
  HOST_WIDE_INT frame_pointer_offset;
  HOST_WIDE_INT hard_frame_pointer_offset;
  HOST_WIDE_INT stack_pointer_offset;
};

/* Used to enable/disable debugging features.  */
const char *ix86_debug_arg_string, *ix86_debug_addr_string;
/* Code model option as passed by user.  */
const char *ix86_cmodel_string;
/* Parsed value.  */
enum cmodel ix86_cmodel;
/* Asm dialect.  */
const char *ix86_asm_string;
enum asm_dialect ix86_asm_dialect = ASM_ATT;
/* TLS dialext.  */
const char *ix86_tls_dialect_string;
enum tls_dialect ix86_tls_dialect = TLS_DIALECT_GNU;

/* Which unit we are generating floating point math for.  */
enum fpmath_unit ix86_fpmath;

/* Which cpu are we scheduling for.  */
enum processor_type ix86_cpu;
/* Which instruction set architecture to use.  */
enum processor_type ix86_arch;

/* Strings to hold which cpu and instruction set architecture  to use.  */
const char *ix86_cpu_string;		/* for -mcpu=<xxx> */
const char *ix86_arch_string;		/* for -march=<xxx> */
const char *ix86_fpmath_string;		/* for -mfpmath=<xxx> */

/* # of registers to use to pass arguments.  */
const char *ix86_regparm_string;

/* true if sse prefetch instruction is not NOOP.  */
int x86_prefetch_sse;

/* ix86_regparm_string as a number */
int ix86_regparm;

/* Alignment to use for loops and jumps:  */

/* Power of two alignment for loops.  */
const char *ix86_align_loops_string;

/* Power of two alignment for non-loop jumps.  */
const char *ix86_align_jumps_string;

/* Power of two alignment for stack boundary in bytes.  */
const char *ix86_preferred_stack_boundary_string;

/* Preferred alignment for stack boundary in bits.  */
int ix86_preferred_stack_boundary;

/* Values 1-5: see jump.c */
int ix86_branch_cost;
const char *ix86_branch_cost_string;

/* Power of two alignment for functions.  */
const char *ix86_align_funcs_string;

/* Prefix built by ASM_GENERATE_INTERNAL_LABEL.  */
static char internal_label_prefix[16];
static int internal_label_prefix_len;

static int local_symbolic_operand PARAMS ((rtx, enum machine_mode));
static int tls_symbolic_operand_1 PARAMS ((rtx, enum tls_model));
static void output_pic_addr_const PARAMS ((FILE *, rtx, int));
static void put_condition_code PARAMS ((enum rtx_code, enum machine_mode,
				       int, int, FILE *));
static const char *get_some_local_dynamic_name PARAMS ((void));
static int get_some_local_dynamic_name_1 PARAMS ((rtx *, void *));
static rtx maybe_get_pool_constant PARAMS ((rtx));
static rtx ix86_expand_int_compare PARAMS ((enum rtx_code, rtx, rtx));
static enum rtx_code ix86_prepare_fp_compare_args PARAMS ((enum rtx_code,
							   rtx *, rtx *));
static rtx get_thread_pointer PARAMS ((void));
static void get_pc_thunk_name PARAMS ((char [32], unsigned int));
static rtx gen_push PARAMS ((rtx));
static int memory_address_length PARAMS ((rtx addr));
static int ix86_flags_dependant PARAMS ((rtx, rtx, enum attr_type));
static int ix86_agi_dependant PARAMS ((rtx, rtx, enum attr_type));
static enum attr_ppro_uops ix86_safe_ppro_uops PARAMS ((rtx));
static void ix86_dump_ppro_packet PARAMS ((FILE *));
static void ix86_reorder_insn PARAMS ((rtx *, rtx *));
static struct machine_function * ix86_init_machine_status PARAMS ((void));
static int ix86_split_to_parts PARAMS ((rtx, rtx *, enum machine_mode));
static int ix86_nsaved_regs PARAMS ((void));
static void ix86_emit_save_regs PARAMS ((void));
static void ix86_emit_save_regs_using_mov PARAMS ((rtx, HOST_WIDE_INT));
static void ix86_emit_restore_regs_using_mov PARAMS ((rtx, int, int));
static void ix86_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static void ix86_set_move_mem_attrs_1 PARAMS ((rtx, rtx, rtx, rtx, rtx));
static void ix86_sched_reorder_ppro PARAMS ((rtx *, rtx *));
static HOST_WIDE_INT ix86_GOT_alias_set PARAMS ((void));
static void ix86_adjust_counter PARAMS ((rtx, HOST_WIDE_INT));
static rtx ix86_expand_aligntest PARAMS ((rtx, int));
static void ix86_expand_strlensi_unroll_1 PARAMS ((rtx, rtx));
static int ix86_issue_rate PARAMS ((void));
static int ix86_adjust_cost PARAMS ((rtx, rtx, rtx, int));
static void ix86_sched_init PARAMS ((FILE *, int, int));
static int ix86_sched_reorder PARAMS ((FILE *, int, rtx *, int *, int));
static int ix86_variable_issue PARAMS ((FILE *, int, rtx, int));
static int ia32_use_dfa_pipeline_interface PARAMS ((void));
static int ia32_multipass_dfa_lookahead PARAMS ((void));
static void ix86_init_mmx_sse_builtins PARAMS ((void));
static rtx x86_this_parameter PARAMS ((tree));
static void x86_output_mi_thunk PARAMS ((FILE *, tree, HOST_WIDE_INT,
					 HOST_WIDE_INT, tree));
static bool x86_can_output_mi_thunk PARAMS ((tree, HOST_WIDE_INT,
					     HOST_WIDE_INT, tree));

struct ix86_address
{
  rtx base, index, disp;
  HOST_WIDE_INT scale;
};

static int ix86_decompose_address PARAMS ((rtx, struct ix86_address *));
static bool ix86_cannot_force_const_mem PARAMS ((rtx));

static void ix86_encode_section_info PARAMS ((tree, int)) ATTRIBUTE_UNUSED;
static const char *ix86_strip_name_encoding PARAMS ((const char *))
     ATTRIBUTE_UNUSED;

struct builtin_description;
static rtx ix86_expand_sse_comi PARAMS ((const struct builtin_description *,
					 tree, rtx));
static rtx ix86_expand_sse_compare PARAMS ((const struct builtin_description *,
					    tree, rtx));
static rtx ix86_expand_unop1_builtin PARAMS ((enum insn_code, tree, rtx));
static rtx ix86_expand_unop_builtin PARAMS ((enum insn_code, tree, rtx, int));
static rtx ix86_expand_binop_builtin PARAMS ((enum insn_code, tree, rtx));
static rtx ix86_expand_store_builtin PARAMS ((enum insn_code, tree));
static rtx safe_vector_operand PARAMS ((rtx, enum machine_mode));
static enum rtx_code ix86_fp_compare_code_to_integer PARAMS ((enum rtx_code));
static void ix86_fp_comparison_codes PARAMS ((enum rtx_code code,
					      enum rtx_code *,
					      enum rtx_code *,
					      enum rtx_code *));
static rtx ix86_expand_fp_compare PARAMS ((enum rtx_code, rtx, rtx, rtx,
					  rtx *, rtx *));
static int ix86_fp_comparison_arithmetics_cost PARAMS ((enum rtx_code code));
static int ix86_fp_comparison_fcomi_cost PARAMS ((enum rtx_code code));
static int ix86_fp_comparison_sahf_cost PARAMS ((enum rtx_code code));
static int ix86_fp_comparison_cost PARAMS ((enum rtx_code code));
static unsigned int ix86_select_alt_pic_regnum PARAMS ((void));
static int ix86_save_reg PARAMS ((unsigned int, int));
static void ix86_compute_frame_layout PARAMS ((struct ix86_frame *));
static int ix86_comp_type_attributes PARAMS ((tree, tree));
static int ix86_fntype_regparm PARAMS ((tree));
const struct attribute_spec ix86_attribute_table[];
static tree ix86_handle_cdecl_attribute PARAMS ((tree *, tree, tree, int, bool *));
static tree ix86_handle_regparm_attribute PARAMS ((tree *, tree, tree, int, bool *));
static int ix86_value_regno PARAMS ((enum machine_mode));
static bool contains_128bit_aligned_vector_p PARAMS ((tree));

#if defined (DO_GLOBAL_CTORS_BODY) && defined (HAS_INIT_SECTION)
static void ix86_svr3_asm_out_constructor PARAMS ((rtx, int));
#endif

/* Register class used for passing given 64bit part of the argument.
   These represent classes as documented by the PS ABI, with the exception
   of SSESF, SSEDF classes, that are basically SSE class, just gcc will
   use SF or DFmode move instead of DImode to avoid reformating penalties.

   Similary we play games with INTEGERSI_CLASS to use cheaper SImode moves
   whenever possible (upper half does contain padding).
 */
enum x86_64_reg_class
  {
    X86_64_NO_CLASS,
    X86_64_INTEGER_CLASS,
    X86_64_INTEGERSI_CLASS,
    X86_64_SSE_CLASS,
    X86_64_SSESF_CLASS,
    X86_64_SSEDF_CLASS,
    X86_64_SSEUP_CLASS,
    X86_64_X87_CLASS,
    X86_64_X87UP_CLASS,
    X86_64_MEMORY_CLASS
  };
static const char * const x86_64_reg_class_name[] =
   {"no", "integer", "integerSI", "sse", "sseSF", "sseDF", "sseup", "x87", "x87up", "no"};

#define MAX_CLASSES 4
static int classify_argument PARAMS ((enum machine_mode, tree,
				      enum x86_64_reg_class [MAX_CLASSES],
				      int));
static int examine_argument PARAMS ((enum machine_mode, tree, int, int *,
				     int *));
static rtx construct_container PARAMS ((enum machine_mode, tree, int, int, int,
					const int *, int));
static enum x86_64_reg_class merge_classes PARAMS ((enum x86_64_reg_class,
						    enum x86_64_reg_class));

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ix86_attribute_table
#ifdef TARGET_DLLIMPORT_DECL_ATTRIBUTES
#  undef TARGET_MERGE_DECL_ATTRIBUTES
#  define TARGET_MERGE_DECL_ATTRIBUTES merge_dllimport_decl_attributes
#endif

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES ix86_comp_type_attributes

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS ix86_init_builtins

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN ix86_expand_builtin

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE ix86_output_function_epilogue

#undef TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN ""
#undef TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ""

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP ASM_SHORT
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP ASM_LONG
#ifdef ASM_QUAD
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP ASM_QUAD
#endif

#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP TARGET_ASM_ALIGNED_HI_OP
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP TARGET_ASM_ALIGNED_SI_OP
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP TARGET_ASM_ALIGNED_DI_OP

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST ix86_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE ix86_issue_rate
#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE ix86_variable_issue
#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT ix86_sched_init
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER ix86_sched_reorder
#undef TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE
#define TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE \
  ia32_use_dfa_pipeline_interface
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  ia32_multipass_dfa_lookahead

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif
#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM ix86_cannot_force_const_mem

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK x86_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK x86_can_output_mi_thunk

struct gcc_target targetm = TARGET_INITIALIZER;

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */
#ifndef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1
#endif

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
override_options ()
{
  int i;
  /* Comes from final.c -- no real reason to change it.  */
#define MAX_CODE_ALIGN 16

  static struct ptt
    {
      const struct processor_costs *cost;	/* Processor costs */
      const int target_enable;			/* Target flags to enable.  */
      const int target_disable;			/* Target flags to disable.  */
      const int align_loop;			/* Default alignments.  */
      const int align_loop_max_skip;
      const int align_jump;
      const int align_jump_max_skip;
      const int align_func;
      const int branch_cost;
    }
  const processor_target_table[PROCESSOR_max] =
    {
      {&i386_cost, 0, 0, 4, 3, 4, 3, 4, 1},
      {&i486_cost, 0, 0, 16, 15, 16, 15, 16, 1},
      {&pentium_cost, 0, 0, 16, 7, 16, 7, 16, 1},
      {&pentiumpro_cost, 0, 0, 16, 15, 16, 7, 16, 1},
      {&k6_cost, 0, 0, 32, 7, 32, 7, 32, 1},
      {&athlon_cost, 0, 0, 16, 7, 64, 7, 16, 1},
      {&pentium4_cost, 0, 0, 0, 0, 0, 0, 0, 1}
    };

  static const char * const cpu_names[] = TARGET_CPU_DEFAULT_NAMES;
  static struct pta
    {
      const char *const name;		/* processor name or nickname.  */
      const enum processor_type processor;
      const enum pta_flags
	{
	  PTA_SSE = 1,
	  PTA_SSE2 = 2,
	  PTA_MMX = 4,
	  PTA_PREFETCH_SSE = 8,
	  PTA_3DNOW = 16,
	  PTA_3DNOW_A = 64
	} flags;
    }
  const processor_alias_table[] =
    {
      {"i386", PROCESSOR_I386, 0},
      {"i486", PROCESSOR_I486, 0},
      {"i586", PROCESSOR_PENTIUM, 0},
      {"pentium", PROCESSOR_PENTIUM, 0},
      {"pentium-mmx", PROCESSOR_PENTIUM, PTA_MMX},
      {"winchip-c6", PROCESSOR_I486, PTA_MMX},
      {"winchip2", PROCESSOR_I486, PTA_MMX | PTA_3DNOW},
      {"c3", PROCESSOR_I486, PTA_MMX | PTA_3DNOW},
      {"i686", PROCESSOR_PENTIUMPRO, 0},
      {"pentiumpro", PROCESSOR_PENTIUMPRO, 0},
      {"pentium2", PROCESSOR_PENTIUMPRO, PTA_MMX},
      {"pentium3", PROCESSOR_PENTIUMPRO, PTA_MMX | PTA_SSE | PTA_PREFETCH_SSE},
      {"pentium4", PROCESSOR_PENTIUM4, PTA_SSE | PTA_SSE2 |
				       PTA_MMX | PTA_PREFETCH_SSE},
      {"k6", PROCESSOR_K6, PTA_MMX},
      {"k6-2", PROCESSOR_K6, PTA_MMX | PTA_3DNOW},
      {"k6-3", PROCESSOR_K6, PTA_MMX | PTA_3DNOW},
      {"athlon", PROCESSOR_ATHLON, PTA_MMX | PTA_PREFETCH_SSE | PTA_3DNOW
				   | PTA_3DNOW_A},
      {"athlon-tbird", PROCESSOR_ATHLON, PTA_MMX | PTA_PREFETCH_SSE
					 | PTA_3DNOW | PTA_3DNOW_A},
      {"athlon-4", PROCESSOR_ATHLON, PTA_MMX | PTA_PREFETCH_SSE | PTA_3DNOW
				    | PTA_3DNOW_A | PTA_SSE},
      {"athlon-xp", PROCESSOR_ATHLON, PTA_MMX | PTA_PREFETCH_SSE | PTA_3DNOW
				      | PTA_3DNOW_A | PTA_SSE},
      {"athlon-mp", PROCESSOR_ATHLON, PTA_MMX | PTA_PREFETCH_SSE | PTA_3DNOW
				      | PTA_3DNOW_A | PTA_SSE},
    };

  int const pta_size = ARRAY_SIZE (processor_alias_table);

  /* By default our XFmode is the 80-bit extended format.  If we have
     use TFmode instead, it's also the 80-bit format, but with padding.  */
  real_format_for_mode[XFmode - QFmode] = &ieee_extended_intel_96_format;
  real_format_for_mode[TFmode - QFmode] = &ieee_extended_intel_128_format;

  /* Set the default values for switches whose default depends on TARGET_64BIT
     in case they weren't overwriten by command line options.  */
  if (TARGET_64BIT)
    {
      if (flag_omit_frame_pointer == 2)
	flag_omit_frame_pointer = 1;
      if (flag_asynchronous_unwind_tables == 2)
	flag_asynchronous_unwind_tables = 1;
      if (flag_pcc_struct_return == 2)
	flag_pcc_struct_return = 0;
    }
  else
    {
      if (flag_omit_frame_pointer == 2)
	flag_omit_frame_pointer = 0;
      if (flag_asynchronous_unwind_tables == 2)
	flag_asynchronous_unwind_tables = 0;
      if (flag_pcc_struct_return == 2)
	flag_pcc_struct_return = DEFAULT_PCC_STRUCT_RETURN;
    }

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  if (!ix86_cpu_string && ix86_arch_string)
    ix86_cpu_string = ix86_arch_string;
  if (!ix86_cpu_string)
    ix86_cpu_string = cpu_names [TARGET_CPU_DEFAULT];
  if (!ix86_arch_string)
    ix86_arch_string = TARGET_64BIT ? "athlon-4" : "i386";

  if (ix86_cmodel_string != 0)
    {
      if (!strcmp (ix86_cmodel_string, "small"))
	ix86_cmodel = flag_pic ? CM_SMALL_PIC : CM_SMALL;
      else if (flag_pic)
	sorry ("code model %s not supported in PIC mode", ix86_cmodel_string);
      else if (!strcmp (ix86_cmodel_string, "32"))
	ix86_cmodel = CM_32;
      else if (!strcmp (ix86_cmodel_string, "kernel") && !flag_pic)
	ix86_cmodel = CM_KERNEL;
      else if (!strcmp (ix86_cmodel_string, "medium") && !flag_pic)
	ix86_cmodel = CM_MEDIUM;
      else if (!strcmp (ix86_cmodel_string, "large") && !flag_pic)
	ix86_cmodel = CM_LARGE;
      else
	error ("bad value (%s) for -mcmodel= switch", ix86_cmodel_string);
    }
  else
    {
      ix86_cmodel = CM_32;
      if (TARGET_64BIT)
	ix86_cmodel = flag_pic ? CM_SMALL_PIC : CM_SMALL;
    }
  if (ix86_asm_string != 0)
    {
      if (!strcmp (ix86_asm_string, "intel"))
	ix86_asm_dialect = ASM_INTEL;
      else if (!strcmp (ix86_asm_string, "att"))
	ix86_asm_dialect = ASM_ATT;
      else
	error ("bad value (%s) for -masm= switch", ix86_asm_string);
    }
  if ((TARGET_64BIT == 0) != (ix86_cmodel == CM_32))
    error ("code model `%s' not supported in the %s bit mode",
	   ix86_cmodel_string, TARGET_64BIT ? "64" : "32");
  if (ix86_cmodel == CM_LARGE)
    sorry ("code model `large' not supported yet");
  if ((TARGET_64BIT != 0) != ((target_flags & MASK_64BIT) != 0))
    sorry ("%i-bit mode not compiled in",
	   (target_flags & MASK_64BIT) ? 64 : 32);

  for (i = 0; i < pta_size; i++)
    if (! strcmp (ix86_arch_string, processor_alias_table[i].name))
      {
	ix86_arch = processor_alias_table[i].processor;
	/* Default cpu tuning to the architecture.  */
	ix86_cpu = ix86_arch;
	if (processor_alias_table[i].flags & PTA_MMX
	    && !(target_flags_explicit & MASK_MMX))
	  target_flags |= MASK_MMX;
	if (processor_alias_table[i].flags & PTA_3DNOW
	    && !(target_flags_explicit & MASK_3DNOW))
	  target_flags |= MASK_3DNOW;
	if (processor_alias_table[i].flags & PTA_3DNOW_A
	    && !(target_flags_explicit & MASK_3DNOW_A))
	  target_flags |= MASK_3DNOW_A;
	if (processor_alias_table[i].flags & PTA_SSE
	    && !(target_flags_explicit & MASK_SSE))
	  target_flags |= MASK_SSE;
	if (processor_alias_table[i].flags & PTA_SSE2
	    && !(target_flags_explicit & MASK_SSE2))
	  target_flags |= MASK_SSE2;
	if (processor_alias_table[i].flags & PTA_PREFETCH_SSE)
	  x86_prefetch_sse = true;
	break;
      }

  if (i == pta_size)
    error ("bad value (%s) for -march= switch", ix86_arch_string);

  for (i = 0; i < pta_size; i++)
    if (! strcmp (ix86_cpu_string, processor_alias_table[i].name))
      {
	ix86_cpu = processor_alias_table[i].processor;
	break;
      }
  if (processor_alias_table[i].flags & PTA_PREFETCH_SSE)
    x86_prefetch_sse = true;
  if (i == pta_size)
    error ("bad value (%s) for -mcpu= switch", ix86_cpu_string);

  if (optimize_size)
    ix86_cost = &size_cost;
  else
    ix86_cost = processor_target_table[ix86_cpu].cost;
  target_flags |= processor_target_table[ix86_cpu].target_enable;
  target_flags &= ~processor_target_table[ix86_cpu].target_disable;

  /* Arrange to set up i386_stack_locals for all functions.  */
  init_machine_status = ix86_init_machine_status;

  /* Validate -mregparm= value.  */
  if (ix86_regparm_string)
    {
      i = atoi (ix86_regparm_string);
      if (i < 0 || i > REGPARM_MAX)
	error ("-mregparm=%d is not between 0 and %d", i, REGPARM_MAX);
      else
	ix86_regparm = i;
    }
  else
   if (TARGET_64BIT)
     ix86_regparm = REGPARM_MAX;

  /* If the user has provided any of the -malign-* options,
     warn and use that value only if -falign-* is not set.
     Remove this code in GCC 3.2 or later.  */
  if (ix86_align_loops_string)
    {
      warning ("-malign-loops is obsolete, use -falign-loops");
      if (align_loops == 0)
	{
	  i = atoi (ix86_align_loops_string);
	  if (i < 0 || i > MAX_CODE_ALIGN)
	    error ("-malign-loops=%d is not between 0 and %d", i, MAX_CODE_ALIGN);
	  else
	    align_loops = 1 << i;
	}
    }

  if (ix86_align_jumps_string)
    {
      warning ("-malign-jumps is obsolete, use -falign-jumps");
      if (align_jumps == 0)
	{
	  i = atoi (ix86_align_jumps_string);
	  if (i < 0 || i > MAX_CODE_ALIGN)
	    error ("-malign-loops=%d is not between 0 and %d", i, MAX_CODE_ALIGN);
	  else
	    align_jumps = 1 << i;
	}
    }

  if (ix86_align_funcs_string)
    {
      warning ("-malign-functions is obsolete, use -falign-functions");
      if (align_functions == 0)
	{
	  i = atoi (ix86_align_funcs_string);
	  if (i < 0 || i > MAX_CODE_ALIGN)
	    error ("-malign-loops=%d is not between 0 and %d", i, MAX_CODE_ALIGN);
	  else
	    align_functions = 1 << i;
	}
    }

  /* Default align_* from the processor table.  */
  if (align_loops == 0)
    {
      align_loops = processor_target_table[ix86_cpu].align_loop;
      align_loops_max_skip = processor_target_table[ix86_cpu].align_loop_max_skip;
    }
  if (align_jumps == 0)
    {
      align_jumps = processor_target_table[ix86_cpu].align_jump;
      align_jumps_max_skip = processor_target_table[ix86_cpu].align_jump_max_skip;
    }
  if (align_functions == 0)
    {
      align_functions = processor_target_table[ix86_cpu].align_func;
    }

  /* Validate -mpreferred-stack-boundary= value, or provide default.
     The default of 128 bits is for Pentium III's SSE __m128, but we
     don't want additional code to keep the stack aligned when
     optimizing for code size.  */
  ix86_preferred_stack_boundary = (optimize_size
				   ? TARGET_64BIT ? 128 : 32
				   : 128);
  if (ix86_preferred_stack_boundary_string)
    {
      i = atoi (ix86_preferred_stack_boundary_string);
      if (i < (TARGET_64BIT ? 4 : 2) || i > 12)
	error ("-mpreferred-stack-boundary=%d is not between %d and 12", i,
	       TARGET_64BIT ? 4 : 2);
      else
	ix86_preferred_stack_boundary = (1 << i) * BITS_PER_UNIT;
    }

  /* Validate -mbranch-cost= value, or provide default.  */
  ix86_branch_cost = processor_target_table[ix86_cpu].branch_cost;
  if (ix86_branch_cost_string)
    {
      i = atoi (ix86_branch_cost_string);
      if (i < 0 || i > 5)
	error ("-mbranch-cost=%d is not between 0 and 5", i);
      else
	ix86_branch_cost = i;
    }

  if (ix86_tls_dialect_string)
    {
      if (strcmp (ix86_tls_dialect_string, "gnu") == 0)
	ix86_tls_dialect = TLS_DIALECT_GNU;
      else if (strcmp (ix86_tls_dialect_string, "sun") == 0)
	ix86_tls_dialect = TLS_DIALECT_SUN;
      else
	error ("bad value (%s) for -mtls-dialect= switch",
	       ix86_tls_dialect_string);
    }

  /* Keep nonleaf frame pointers.  */
  if (TARGET_OMIT_LEAF_FRAME_POINTER)
    flag_omit_frame_pointer = 1;

  /* If we're doing fast math, we don't care about comparison order
     wrt NaNs.  This lets us use a shorter comparison sequence.  */
  if (flag_unsafe_math_optimizations)
    target_flags &= ~MASK_IEEE_FP;

  /* If the architecture always has an FPU, turn off NO_FANCY_MATH_387,
     since the insns won't need emulation.  */
  if (x86_arch_always_fancy_math_387 & (1 << ix86_arch))
    target_flags &= ~MASK_NO_FANCY_MATH_387;

  if (TARGET_64BIT)
    {
      if (TARGET_ALIGN_DOUBLE)
	error ("-malign-double makes no sense in the 64bit mode");
      if (TARGET_RTD)
	error ("-mrtd calling convention not supported in the 64bit mode");
      /* Enable by default the SSE and MMX builtins.  */
      target_flags |= (MASK_SSE2 | MASK_SSE | MASK_MMX | MASK_128BIT_LONG_DOUBLE);
      ix86_fpmath = FPMATH_SSE;
     }
  else
    ix86_fpmath = FPMATH_387;

  if (ix86_fpmath_string != 0)
    {
      if (! strcmp (ix86_fpmath_string, "387"))
	ix86_fpmath = FPMATH_387;
      else if (! strcmp (ix86_fpmath_string, "sse"))
	{
	  if (!TARGET_SSE)
	    {
	      warning ("SSE instruction set disabled, using 387 arithmetics");
	      ix86_fpmath = FPMATH_387;
	    }
	  else
	    ix86_fpmath = FPMATH_SSE;
	}
      else if (! strcmp (ix86_fpmath_string, "387,sse")
	       || ! strcmp (ix86_fpmath_string, "sse,387"))
	{
	  if (!TARGET_SSE)
	    {
	      warning ("SSE instruction set disabled, using 387 arithmetics");
	      ix86_fpmath = FPMATH_387;
	    }
	  else if (!TARGET_80387)
	    {
	      warning ("387 instruction set disabled, using SSE arithmetics");
	      ix86_fpmath = FPMATH_SSE;
	    }
	  else
	    ix86_fpmath = FPMATH_SSE | FPMATH_387;
	}
      else
	error ("bad value (%s) for -mfpmath= switch", ix86_fpmath_string);
    }

  /* It makes no sense to ask for just SSE builtins, so MMX is also turned
     on by -msse.  */
  if (TARGET_SSE)
    {
      target_flags |= MASK_MMX;
      x86_prefetch_sse = true;
    }

  /* If it has 3DNow! it also has MMX so MMX is also turned on by -m3dnow */
  if (TARGET_3DNOW)
    {
      target_flags |= MASK_MMX;
      /* If we are targetting the Athlon architecture, enable the 3Dnow/MMX
	 extensions it adds.  */
      if (x86_3dnow_a & (1 << ix86_arch))
	target_flags |= MASK_3DNOW_A;
    }
  if ((x86_accumulate_outgoing_args & CPUMASK)
      && !(target_flags_explicit & MASK_ACCUMULATE_OUTGOING_ARGS)
      && !optimize_size)
    target_flags |= MASK_ACCUMULATE_OUTGOING_ARGS;

  /* Figure out what ASM_GENERATE_INTERNAL_LABEL builds as a prefix.  */
  {
    char *p;
    ASM_GENERATE_INTERNAL_LABEL (internal_label_prefix, "LX", 0);
    p = strchr (internal_label_prefix, 'X');
    internal_label_prefix_len = p - internal_label_prefix;
    *p = '\0';
  }
}

void
optimization_options (level, size)
     int level;
     int size ATTRIBUTE_UNUSED;
{
  /* For -O2 and beyond, turn off -fschedule-insns by default.  It tends to
     make the problem with not enough registers even worse.  */
#ifdef INSN_SCHEDULING
  if (level > 1)
    flag_schedule_insns = 0;
#endif

  /* The default values of these switches depend on the TARGET_64BIT
     that is not known at this moment.  Mark these values with 2 and
     let user the to override these.  In case there is no command line option
     specifying them, we will set the defaults in override_options.  */
  if (optimize >= 1)
    flag_omit_frame_pointer = 2;
  flag_pcc_struct_return = 2;
  flag_asynchronous_unwind_tables = 2;
}

/* Table of valid machine attributes.  */
const struct attribute_spec ix86_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Stdcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "stdcall",   0, 0, false, true,  true,  ix86_handle_cdecl_attribute },
  /* Cdecl attribute says the callee is a normal C declaration */
  { "cdecl",     0, 0, false, true,  true,  ix86_handle_cdecl_attribute },
  /* Regparm attribute specifies how many integer arguments are to be
     passed in registers.  */
  { "regparm",   1, 1, false, true,  true,  ix86_handle_regparm_attribute },
#ifdef TARGET_DLLIMPORT_DECL_ATTRIBUTES
  { "dllimport", 0, 0, false, false, false, ix86_handle_dll_attribute },
  { "dllexport", 0, 0, false, false, false, ix86_handle_dll_attribute },
  { "shared",    0, 0, true,  false, false, ix86_handle_shared_attribute },
#endif
  { NULL,        0, 0, false, false, false, NULL }
};

/* Handle a "cdecl" or "stdcall" attribute;
   arguments as in struct attribute_spec.handler.  */
static tree
ix86_handle_cdecl_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args ATTRIBUTE_UNUSED;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  if (TARGET_64BIT)
    {
      warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "regparm" attribute;
   arguments as in struct attribute_spec.handler.  */
static tree
ix86_handle_regparm_attribute (node, name, args, flags, no_add_attrs)
     tree *node;
     tree name;
     tree args;
     int flags ATTRIBUTE_UNUSED;
     bool *no_add_attrs;
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning ("`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }
  else
    {
      tree cst;

      cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != INTEGER_CST)
	{
	  warning ("`%s' attribute requires an integer constant argument",
		   IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
      else if (compare_tree_int (cst, REGPARM_MAX) > 0)
	{
	  warning ("argument to `%s' attribute larger than %d",
		   IDENTIFIER_POINTER (name), REGPARM_MAX);
	  *no_add_attrs = true;
	}
    }

  return NULL_TREE;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

static int
ix86_comp_type_attributes (type1, type2)
     tree type1;
     tree type2;
{
  /* Check for mismatch of non-default calling convention.  */
  const char *const rtdstr = TARGET_RTD ? "cdecl" : "stdcall";

  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched return types (cdecl vs stdcall).  */
  if (!lookup_attribute (rtdstr, TYPE_ATTRIBUTES (type1))
      != !lookup_attribute (rtdstr, TYPE_ATTRIBUTES (type2)))
    return 0;
  return 1;
}

/* Return the regparm value for a fuctio with the indicated TYPE.  */

static int
ix86_fntype_regparm (type)
     tree type;
{
  tree attr;

  attr = lookup_attribute ("regparm", TYPE_ATTRIBUTES (type));
  if (attr)
    return TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr)));
  else
    return ix86_regparm;
}

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the 80386, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.

   The attribute stdcall is equivalent to RTD on a per module basis.  */

int
ix86_return_pops_args (fundecl, funtype, size)
     tree fundecl;
     tree funtype;
     int size;
{
  int rtd = TARGET_RTD && (!fundecl || TREE_CODE (fundecl) != IDENTIFIER_NODE);

    /* Cdecl functions override -mrtd, and never pop the stack.  */
  if (! lookup_attribute ("cdecl", TYPE_ATTRIBUTES (funtype))) {

    /* Stdcall functions will pop the stack if not variable args.  */
    if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (funtype)))
      rtd = 1;

    if (rtd
        && (TYPE_ARG_TYPES (funtype) == NULL_TREE
	    || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (funtype)))
		== void_type_node)))
      return size;
  }

  /* Lose any fake structure return argument if it is passed on the stack.  */
  if (aggregate_value_p (TREE_TYPE (funtype))
      && !TARGET_64BIT)
    {
      int nregs = ix86_fntype_regparm (funtype);

      if (!nregs)
	return GET_MODE_SIZE (Pmode);
    }

  return 0;
}

/* Argument support functions.  */

/* Return true when register may be used to pass function parameters.  */
bool
ix86_function_arg_regno_p (regno)
     int regno;
{
  int i;
  if (!TARGET_64BIT)
    return (regno < REGPARM_MAX
	    || (TARGET_SSE && SSE_REGNO_P (regno) && !fixed_regs[regno]));
  if (SSE_REGNO_P (regno) && TARGET_SSE)
    return true;
  /* RAX is used as hidden argument to va_arg functions.  */
  if (!regno)
    return true;
  for (i = 0; i < REGPARM_MAX; i++)
    if (regno == x86_64_int_parameter_registers[i])
      return true;
  return false;
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
init_cumulative_args (cum, fntype, libname)
     CUMULATIVE_ARGS *cum;	/* Argument info to initialize */
     tree fntype;		/* tree ptr for function decl */
     rtx libname;		/* SYMBOL_REF of library name or 0 */
{
  static CUMULATIVE_ARGS zero_cum;
  tree param, next_param;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args (");
      if (fntype)
	fprintf (stderr, "fntype code = %s, ret code = %s",
		 tree_code_name[(int) TREE_CODE (fntype)],
		 tree_code_name[(int) TREE_CODE (TREE_TYPE (fntype))]);
      else
	fprintf (stderr, "no fntype");

      if (libname)
	fprintf (stderr, ", libname = %s", XSTR (libname, 0));
    }

  *cum = zero_cum;

  /* Set up the number of registers to use for passing arguments.  */
  cum->nregs = ix86_regparm;
  cum->sse_nregs = SSE_REGPARM_MAX;
  if (fntype && !TARGET_64BIT)
    {
      tree attr = lookup_attribute ("regparm", TYPE_ATTRIBUTES (fntype));

      if (attr)
	cum->nregs = TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr)));
    }
  cum->maybe_vaarg = false;

  /* Determine if this function has variable arguments.  This is
     indicated by the last argument being 'void_type_mode' if there
     are no variable arguments.  If there are variable arguments, then
     we won't pass anything in registers */

  if (cum->nregs)
    {
      for (param = (fntype) ? TYPE_ARG_TYPES (fntype) : 0;
	   param != 0; param = next_param)
	{
	  next_param = TREE_CHAIN (param);
	  if (next_param == 0 && TREE_VALUE (param) != void_type_node)
	    {
	      if (!TARGET_64BIT)
		cum->nregs = 0;
	      cum->maybe_vaarg = true;
	    }
	}
    }
  if ((!fntype && !libname)
      || (fntype && !TYPE_ARG_TYPES (fntype)))
    cum->maybe_vaarg = 1;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, ", nregs=%d )\n", cum->nregs);

  return;
}

/* x86-64 register passing impleemntation.  See x86-64 ABI for details.  Goal
   of this code is to classify each 8bytes of incoming argument by the register
   class and assign registers accordingly.  */

/* Return the union class of CLASS1 and CLASS2.
   See the x86-64 PS ABI for details.  */

static enum x86_64_reg_class
merge_classes (class1, class2)
     enum x86_64_reg_class class1, class2;
{
  /* Rule #1: If both classes are equal, this is the resulting class.  */
  if (class1 == class2)
    return class1;

  /* Rule #2: If one of the classes is NO_CLASS, the resulting class is
     the other class.  */
  if (class1 == X86_64_NO_CLASS)
    return class2;
  if (class2 == X86_64_NO_CLASS)
    return class1;

  /* Rule #3: If one of the classes is MEMORY, the result is MEMORY.  */
  if (class1 == X86_64_MEMORY_CLASS || class2 == X86_64_MEMORY_CLASS)
    return X86_64_MEMORY_CLASS;

  /* Rule #4: If one of the classes is INTEGER, the result is INTEGER.  */
  if ((class1 == X86_64_INTEGERSI_CLASS && class2 == X86_64_SSESF_CLASS)
      || (class2 == X86_64_INTEGERSI_CLASS && class1 == X86_64_SSESF_CLASS))
    return X86_64_INTEGERSI_CLASS;
  if (class1 == X86_64_INTEGER_CLASS || class1 == X86_64_INTEGERSI_CLASS
      || class2 == X86_64_INTEGER_CLASS || class2 == X86_64_INTEGERSI_CLASS)
    return X86_64_INTEGER_CLASS;

  /* Rule #5: If one of the classes is X87 or X87UP class, MEMORY is used.  */
  if (class1 == X86_64_X87_CLASS || class1 == X86_64_X87UP_CLASS
      || class2 == X86_64_X87_CLASS || class2 == X86_64_X87UP_CLASS)
    return X86_64_MEMORY_CLASS;

  /* Rule #6: Otherwise class SSE is used.  */
  return X86_64_SSE_CLASS;
}

/* Classify the argument of type TYPE and mode MODE.
   CLASSES will be filled by the register class used to pass each word
   of the operand.  The number of words is returned.  In case the parameter
   should be passed in memory, 0 is returned. As a special case for zero
   sized containers, classes[0] will be NO_CLASS and 1 is returned.

   BIT_OFFSET is used internally for handling records and specifies offset
   of the offset in bits modulo 256 to avoid overflow cases.

   See the x86-64 PS ABI for details.
*/

static int
classify_argument (mode, type, classes, bit_offset)
     enum machine_mode mode;
     tree type;
     enum x86_64_reg_class classes[MAX_CLASSES];
     int bit_offset;
{
  int bytes =
    (mode == BLKmode) ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  int words = (bytes + (bit_offset % 64) / 8 + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* Variable sized entities are always passed/returned in memory.  */
  if (bytes < 0)
    return 0;

  if (type && AGGREGATE_TYPE_P (type))
    {
      int i;
      tree field;
      enum x86_64_reg_class subclasses[MAX_CLASSES];

      /* On x86-64 we pass structures larger than 16 bytes on the stack.  */
      if (bytes > 16)
	return 0;

      for (i = 0; i < words; i++)
	classes[i] = X86_64_NO_CLASS;

      /* Zero sized arrays or structures are NO_CLASS.  We return 0 to
	 signalize memory class, so handle it as special case.  */
      if (!words)
	{
	  classes[0] = X86_64_NO_CLASS;
	  return 1;
	}

      /* Classify each field of record and merge classes.  */
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  /* For classes first merge in the field of the subclasses.  */
	  if (TYPE_BINFO (type) != NULL && TYPE_BINFO_BASETYPES (type) != NULL)
	    {
	      tree bases = TYPE_BINFO_BASETYPES (type);
	      int n_bases = TREE_VEC_LENGTH (bases);
	      int i;

	      for (i = 0; i < n_bases; ++i)
		{
		   tree binfo = TREE_VEC_ELT (bases, i);
		   int num;
		   int offset = tree_low_cst (BINFO_OFFSET (binfo), 0) * 8;
		   tree type = BINFO_TYPE (binfo);

		   num = classify_argument (TYPE_MODE (type),
					    type, subclasses,
					    (offset + bit_offset) % 256);
		   if (!num)
		     return 0;
		   for (i = 0; i < num; i++)
		     {
		       int pos = (offset + (bit_offset % 64)) / 8 / 8;
		       classes[i + pos] =
			 merge_classes (subclasses[i], classes[i + pos]);
		     }
		}
	    }
	  /* And now merge the fields of structure.   */
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  int num;

		  /* Bitfields are always classified as integer.  Handle them
		     early, since later code would consider them to be
		     misaligned integers.  */
		  if (DECL_BIT_FIELD (field))
		    {
		      for (i = int_bit_position (field) / 8 / 8;
			   i < (int_bit_position (field)
			        + tree_low_cst (DECL_SIZE (field), 0)
			       	+ 63) / 8 / 8; i++)
			classes[i] =
			  merge_classes (X86_64_INTEGER_CLASS,
					 classes[i]);
		    }
		  else
		    {
		      num = classify_argument (TYPE_MODE (TREE_TYPE (field)),
					       TREE_TYPE (field), subclasses,
					       (int_bit_position (field)
						+ bit_offset) % 256);
		      if (!num)
			return 0;
		      for (i = 0; i < num; i++)
			{
			  int pos =
			    (int_bit_position (field) + (bit_offset % 64)) / 8 / 8;
			  classes[i + pos] =
			    merge_classes (subclasses[i], classes[i + pos]);
			}
		    }
		}
	    }
	}
      /* Arrays are handled as small records.  */
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  int num;
	  num = classify_argument (TYPE_MODE (TREE_TYPE (type)),
				   TREE_TYPE (type), subclasses, bit_offset);
	  if (!num)
	    return 0;

	  /* The partial classes are now full classes.  */
	  if (subclasses[0] == X86_64_SSESF_CLASS && bytes != 4)
	    subclasses[0] = X86_64_SSE_CLASS;
	  if (subclasses[0] == X86_64_INTEGERSI_CLASS && bytes != 4)
	    subclasses[0] = X86_64_INTEGER_CLASS;

	  for (i = 0; i < words; i++)
	    classes[i] = subclasses[i % num];
	}
      /* Unions are similar to RECORD_TYPE but offset is always 0.  */
      else if (TREE_CODE (type) == UNION_TYPE
	       || TREE_CODE (type) == QUAL_UNION_TYPE)
	{
	  /* For classes first merge in the field of the subclasses.  */
	  if (TYPE_BINFO (type) != NULL && TYPE_BINFO_BASETYPES (type) != NULL)
	    {
	      tree bases = TYPE_BINFO_BASETYPES (type);
	      int n_bases = TREE_VEC_LENGTH (bases);
	      int i;

	      for (i = 0; i < n_bases; ++i)
		{
		   tree binfo = TREE_VEC_ELT (bases, i);
		   int num;
		   int offset = tree_low_cst (BINFO_OFFSET (binfo), 0) * 8;
		   tree type = BINFO_TYPE (binfo);

		   num = classify_argument (TYPE_MODE (type),
					    type, subclasses,
					    (offset + (bit_offset % 64)) % 256);
		   if (!num)
		     return 0;
		   for (i = 0; i < num; i++)
		     {
		       int pos = (offset + (bit_offset % 64)) / 8 / 8;
		       classes[i + pos] =
			 merge_classes (subclasses[i], classes[i + pos]);
		     }
		}
	    }
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  int num;
		  num = classify_argument (TYPE_MODE (TREE_TYPE (field)),
					   TREE_TYPE (field), subclasses,
					   bit_offset);
		  if (!num)
		    return 0;
		  for (i = 0; i < num; i++)
		    classes[i] = merge_classes (subclasses[i], classes[i]);
		}
	    }
	}
      else
	abort ();

      /* Final merger cleanup.  */
      for (i = 0; i < words; i++)
	{
	  /* If one class is MEMORY, everything should be passed in
	     memory.  */
	  if (classes[i] == X86_64_MEMORY_CLASS)
	    return 0;

	  /* The X86_64_SSEUP_CLASS should be always preceded by
	     X86_64_SSE_CLASS.  */
	  if (classes[i] == X86_64_SSEUP_CLASS
	      && (i == 0 || classes[i - 1] != X86_64_SSE_CLASS))
	    classes[i] = X86_64_SSE_CLASS;

	  /*  X86_64_X87UP_CLASS should be preceded by X86_64_X87_CLASS.  */
	  if (classes[i] == X86_64_X87UP_CLASS
	      && (i == 0 || classes[i - 1] != X86_64_X87_CLASS))
	    classes[i] = X86_64_SSE_CLASS;
	}
      return words;
    }

  /* Compute alignment needed.  We align all types to natural boundaries with
     exception of XFmode that is aligned to 64bits.  */
  if (mode != VOIDmode && mode != BLKmode)
    {
      int mode_alignment = GET_MODE_BITSIZE (mode);

      if (mode == XFmode)
	mode_alignment = 128;
      else if (mode == XCmode)
	mode_alignment = 256;
      /* Misaligned fields are always returned in memory.  */
      if (bit_offset % mode_alignment)
	return 0;
    }

  /* Classification of atomic types.  */
  switch (mode)
    {
    case DImode:
    case SImode:
    case HImode:
    case QImode:
    case CSImode:
    case CHImode:
    case CQImode:
      if (bit_offset + GET_MODE_BITSIZE (mode) <= 32)
	classes[0] = X86_64_INTEGERSI_CLASS;
      else
	classes[0] = X86_64_INTEGER_CLASS;
      return 1;
    case CDImode:
    case TImode:
      classes[0] = classes[1] = X86_64_INTEGER_CLASS;
      return 2;
    case CTImode:
      classes[0] = classes[1] = X86_64_INTEGER_CLASS;
      classes[2] = classes[3] = X86_64_INTEGER_CLASS;
      return 4;
    case SFmode:
      if (!(bit_offset % 64))
	classes[0] = X86_64_SSESF_CLASS;
      else
	classes[0] = X86_64_SSE_CLASS;
      return 1;
    case DFmode:
      classes[0] = X86_64_SSEDF_CLASS;
      return 1;
    case TFmode:
      classes[0] = X86_64_X87_CLASS;
      classes[1] = X86_64_X87UP_CLASS;
      return 2;
    case TCmode:
      classes[0] = X86_64_X87_CLASS;
      classes[1] = X86_64_X87UP_CLASS;
      classes[2] = X86_64_X87_CLASS;
      classes[3] = X86_64_X87UP_CLASS;
      return 4;
    case DCmode:
      classes[0] = X86_64_SSEDF_CLASS;
      classes[1] = X86_64_SSEDF_CLASS;
      return 2;
    case SCmode:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case V4SFmode:
    case V4SImode:
    case V16QImode:
    case V8HImode:
    case V2DFmode:
    case V2DImode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
    case V2SFmode:
    case V2SImode:
    case V4HImode:
    case V8QImode:
      return 0;
    case BLKmode:
    case VOIDmode:
      return 0;
    default:
      abort ();
    }
}

/* Examine the argument and return set number of register required in each
   class.  Return 0 iff parameter should be passed in memory.  */
static int
examine_argument (mode, type, in_return, int_nregs, sse_nregs)
     enum machine_mode mode;
     tree type;
     int *int_nregs, *sse_nregs;
     int in_return;
{
  enum x86_64_reg_class class[MAX_CLASSES];
  int n = classify_argument (mode, type, class, 0);

  *int_nregs = 0;
  *sse_nregs = 0;
  if (!n)
    return 0;
  for (n--; n >= 0; n--)
    switch (class[n])
      {
      case X86_64_INTEGER_CLASS:
      case X86_64_INTEGERSI_CLASS:
	(*int_nregs)++;
	break;
      case X86_64_SSE_CLASS:
      case X86_64_SSESF_CLASS:
      case X86_64_SSEDF_CLASS:
	(*sse_nregs)++;
	break;
      case X86_64_NO_CLASS:
      case X86_64_SSEUP_CLASS:
	break;
      case X86_64_X87_CLASS:
      case X86_64_X87UP_CLASS:
	if (!in_return)
	  return 0;
	break;
      case X86_64_MEMORY_CLASS:
	abort ();
      }
  return 1;
}
/* Construct container for the argument used by GCC interface.  See
   FUNCTION_ARG for the detailed description.  */
static rtx
construct_container (mode, type, in_return, nintregs, nsseregs, intreg, sse_regno)
     enum machine_mode mode;
     tree type;
     int in_return;
     int nintregs, nsseregs;
     const int * intreg;
     int sse_regno;
{
  enum machine_mode tmpmode;
  int bytes =
    (mode == BLKmode) ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  enum x86_64_reg_class class[MAX_CLASSES];
  int n;
  int i;
  int nexps = 0;
  int needed_sseregs, needed_intregs;
  rtx exp[MAX_CLASSES];
  rtx ret;

  n = classify_argument (mode, type, class, 0);
  if (TARGET_DEBUG_ARG)
    {
      if (!n)
	fprintf (stderr, "Memory class\n");
      else
	{
	  fprintf (stderr, "Classes:");
	  for (i = 0; i < n; i++)
	    {
	      fprintf (stderr, " %s", x86_64_reg_class_name[class[i]]);
	    }
	   fprintf (stderr, "\n");
	}
    }
  if (!n)
    return NULL;
  if (!examine_argument (mode, type, in_return, &needed_intregs, &needed_sseregs))
    return NULL;
  if (needed_intregs > nintregs || needed_sseregs > nsseregs)
    return NULL;

  /* First construct simple cases.  Avoid SCmode, since we want to use
     single register to pass this type.  */
  if (n == 1 && mode != SCmode)
    switch (class[0])
      {
      case X86_64_INTEGER_CLASS:
      case X86_64_INTEGERSI_CLASS:
	return gen_rtx_REG (mode, intreg[0]);
      case X86_64_SSE_CLASS:
      case X86_64_SSESF_CLASS:
      case X86_64_SSEDF_CLASS:
	return gen_rtx_REG (mode, SSE_REGNO (sse_regno));
      case X86_64_X87_CLASS:
	return gen_rtx_REG (mode, FIRST_STACK_REG);
      case X86_64_NO_CLASS:
	/* Zero sized array, struct or class.  */
	return NULL;
      default:
	abort ();
      }
  if (n == 2 && class[0] == X86_64_SSE_CLASS && class[1] == X86_64_SSEUP_CLASS)
    return gen_rtx_REG (mode, SSE_REGNO (sse_regno));
  if (n == 2
      && class[0] == X86_64_X87_CLASS && class[1] == X86_64_X87UP_CLASS)
    return gen_rtx_REG (TFmode, FIRST_STACK_REG);
  if (n == 2 && class[0] == X86_64_INTEGER_CLASS
      && class[1] == X86_64_INTEGER_CLASS
      && (mode == CDImode || mode == TImode)
      && intreg[0] + 1 == intreg[1])
    return gen_rtx_REG (mode, intreg[0]);
  if (n == 4
      && class[0] == X86_64_X87_CLASS && class[1] == X86_64_X87UP_CLASS
      && class[2] == X86_64_X87_CLASS && class[3] == X86_64_X87UP_CLASS)
    return gen_rtx_REG (TCmode, FIRST_STACK_REG);

  /* Otherwise figure out the entries of the PARALLEL.  */
  for (i = 0; i < n; i++)
    {
      switch (class[i])
        {
	  case X86_64_NO_CLASS:
	    break;
	  case X86_64_INTEGER_CLASS:
	  case X86_64_INTEGERSI_CLASS:
	    /* Merge TImodes on aligned occassions here too.  */
	    if (i * 8 + 8 > bytes)
	      tmpmode = mode_for_size ((bytes - i * 8) * BITS_PER_UNIT, MODE_INT, 0);
	    else if (class[i] == X86_64_INTEGERSI_CLASS)
	      tmpmode = SImode;
	    else
	      tmpmode = DImode;
	    /* We've requested 24 bytes we don't have mode for.  Use DImode.  */
	    if (tmpmode == BLKmode)
	      tmpmode = DImode;
	    exp [nexps++] = gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (tmpmode, *intreg),
					       GEN_INT (i*8));
	    intreg++;
	    break;
	  case X86_64_SSESF_CLASS:
	    exp [nexps++] = gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (SFmode,
							    SSE_REGNO (sse_regno)),
					       GEN_INT (i*8));
	    sse_regno++;
	    break;
	  case X86_64_SSEDF_CLASS:
	    exp [nexps++] = gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (DFmode,
							    SSE_REGNO (sse_regno)),
					       GEN_INT (i*8));
	    sse_regno++;
	    break;
	  case X86_64_SSE_CLASS:
	    if (i < n - 1 && class[i + 1] == X86_64_SSEUP_CLASS)
	      tmpmode = TImode;
	    else
	      tmpmode = DImode;
	    exp [nexps++] = gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (tmpmode,
							    SSE_REGNO (sse_regno)),
					       GEN_INT (i*8));
	    if (tmpmode == TImode)
	      i++;
	    sse_regno++;
	    break;
	  default:
	    abort ();
	}
    }
  ret =  gen_rtx_PARALLEL (mode, rtvec_alloc (nexps));
  for (i = 0; i < nexps; i++)
    XVECEXP (ret, 0, i) = exp [i];
  return ret;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* whether or not the argument was named */
{
  int bytes =
    (mode == BLKmode) ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "function_adv (sz=%d, wds=%2d, nregs=%d, mode=%s, named=%d)\n\n",
	     words, cum->words, cum->nregs, GET_MODE_NAME (mode), named);
  if (TARGET_64BIT)
    {
      int int_nregs, sse_nregs;
      if (!examine_argument (mode, type, 0, &int_nregs, &sse_nregs))
	cum->words += words;
      else if (sse_nregs <= cum->sse_nregs && int_nregs <= cum->nregs)
	{
	  cum->nregs -= int_nregs;
	  cum->sse_nregs -= sse_nregs;
	  cum->regno += int_nregs;
	  cum->sse_regno += sse_nregs;
	}
      else
	cum->words += words;
    }
  else
    {
      if (TARGET_SSE && mode == TImode)
	{
	  cum->sse_words += words;
	  cum->sse_nregs -= 1;
	  cum->sse_regno += 1;
	  if (cum->sse_nregs <= 0)
	    {
	      cum->sse_nregs = 0;
	      cum->sse_regno = 0;
	    }
	}
      else
	{
	  cum->words += words;
	  cum->nregs -= words;
	  cum->regno += words;

	  if (cum->nregs <= 0)
	    {
	      cum->nregs = 0;
	      cum->regno = 0;
	    }
	}
    }
  return;
}

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

rtx
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  rtx ret   = NULL_RTX;
  int bytes =
    (mode == BLKmode) ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  int words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* Handle an hidden AL argument containing number of registers for varargs
     x86-64 functions.  For i386 ABI just return constm1_rtx to avoid
     any AL settings.  */
  if (mode == VOIDmode)
    {
      if (TARGET_64BIT)
	return GEN_INT (cum->maybe_vaarg
			? (cum->sse_nregs < 0
			   ? SSE_REGPARM_MAX
			   : cum->sse_regno)
			: -1);
      else
	return constm1_rtx;
    }
  if (TARGET_64BIT)
    ret = construct_container (mode, type, 0, cum->nregs, cum->sse_nregs,
			       &x86_64_int_parameter_registers [cum->regno],
			       cum->sse_regno);
  else
    switch (mode)
      {
	/* For now, pass fp/complex values on the stack.  */
      default:
	break;

      case BLKmode:
	if (bytes < 0)
	  break;
	/* FALLTHRU */
      case DImode:
      case SImode:
      case HImode:
      case QImode:
	if (words <= cum->nregs)
	  ret = gen_rtx_REG (mode, cum->regno);
	break;
      case TImode:
	if (cum->sse_nregs)
	  ret = gen_rtx_REG (mode, cum->sse_regno);
	break;
      }

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr,
	       "function_arg (size=%d, wds=%2d, nregs=%d, mode=%4s, named=%d, ",
	       words, cum->words, cum->nregs, GET_MODE_NAME (mode), named);

      if (ret)
	print_simple_rtl (stderr, ret);
      else
	fprintf (stderr, ", stack");

      fprintf (stderr, " )\n");
    }

  return ret;
}

/* Return true when TYPE should be 128bit aligned for 32bit argument passing
   ABI  */
static bool
contains_128bit_aligned_vector_p (type)
     tree type;
{
  enum machine_mode mode = TYPE_MODE (type);
  if (SSE_REG_MODE_P (mode)
      && (!TYPE_USER_ALIGN (type) || TYPE_ALIGN (type) > 128))
    return true;
  if (TYPE_ALIGN (type) < 128)
    return false;

  if (AGGREGATE_TYPE_P (type))
    {
      /* Walk the agregates recursivly.  */
      if (TREE_CODE (type) == RECORD_TYPE
	  || TREE_CODE (type) == UNION_TYPE
	  || TREE_CODE (type) == QUAL_UNION_TYPE)
	{
	  tree field;

	  if (TYPE_BINFO (type) != NULL
	      && TYPE_BINFO_BASETYPES (type) != NULL)
	    {
	      tree bases = TYPE_BINFO_BASETYPES (type);
	      int n_bases = TREE_VEC_LENGTH (bases);
	      int i;

	      for (i = 0; i < n_bases; ++i)
		{
		  tree binfo = TREE_VEC_ELT (bases, i);
		  tree type = BINFO_TYPE (binfo);

		  if (contains_128bit_aligned_vector_p (type))
		    return true;
		}
	    }
	  /* And now merge the fields of structure.   */
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL
		  && contains_128bit_aligned_vector_p (TREE_TYPE (field)))
		return true;
	    }
	}
      /* Just for use if some languages passes arrays by value.  */
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (contains_128bit_aligned_vector_p (TREE_TYPE (type)))
	    return true;
	}
      else
	abort ();
    }
  return false;
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  if (!TARGET_64BIT)
    return 0;

  if (type && int_size_in_bytes (type) == -1)
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference\n");
      return 1;
    }

  return 0;
}

/* Gives the alignment boundary, in bits, of an argument with the specified mode
   and type.   */

int
ix86_function_arg_boundary (mode, type)
     enum machine_mode mode;
     tree type;
{
  int align;
  if (type)
    align = TYPE_ALIGN (type);
  else
    align = GET_MODE_ALIGNMENT (mode);
  if (align < PARM_BOUNDARY)
    align = PARM_BOUNDARY;
  if (!TARGET_64BIT)
    {
      /* i386 ABI defines all arguments to be 4 byte aligned.  We have to
	 make an exception for SSE modes since these require 128bit
	 alignment.  

	 The handling here differs from field_alignment.  ICC aligns MMX
	 arguments to 4 byte boundaries, while structure fields are aligned
	 to 8 byte boundaries.  */
      if (!type)
	{
	  if (!SSE_REG_MODE_P (mode))
	    align = PARM_BOUNDARY;
	}
      else
	{
	  if (!contains_128bit_aligned_vector_p (type))
	    align = PARM_BOUNDARY;
	}
      if (align != PARM_BOUNDARY && !TARGET_SSE)
	abort();
    }
  if (align > 128)
    align = 128;
  return align;
}

/* Return true if N is a possible register number of function value.  */
bool
ix86_function_value_regno_p (regno)
     int regno;
{
  if (!TARGET_64BIT)
    {
      return ((regno) == 0
	      || ((regno) == FIRST_FLOAT_REG && TARGET_FLOAT_RETURNS_IN_80387)
	      || ((regno) == FIRST_SSE_REG && TARGET_SSE));
    }
  return ((regno) == 0 || (regno) == FIRST_FLOAT_REG
	  || ((regno) == FIRST_SSE_REG && TARGET_SSE)
	  || ((regno) == FIRST_FLOAT_REG && TARGET_FLOAT_RETURNS_IN_80387));
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
rtx
ix86_function_value (valtype)
     tree valtype;
{
  if (TARGET_64BIT)
    {
      rtx ret = construct_container (TYPE_MODE (valtype), valtype, 1,
				     REGPARM_MAX, SSE_REGPARM_MAX,
				     x86_64_int_return_registers, 0);
      /* For zero sized structures, construct_continer return NULL, but we need
         to keep rest of compiler happy by returning meaningfull value.  */
      if (!ret)
	ret = gen_rtx_REG (TYPE_MODE (valtype), 0);
      return ret;
    }
  else
    return gen_rtx_REG (TYPE_MODE (valtype),
			ix86_value_regno (TYPE_MODE (valtype)));
}

/* Return false iff type is returned in memory.  */
int
ix86_return_in_memory (type)
     tree type;
{
  int needed_intregs, needed_sseregs;
  if (TARGET_64BIT)
    {
      return !examine_argument (TYPE_MODE (type), type, 1,
				&needed_intregs, &needed_sseregs);
    }
  else
    {
      if (TYPE_MODE (type) == BLKmode
	  || (VECTOR_MODE_P (TYPE_MODE (type))
	      && int_size_in_bytes (type) == 8)
	  || (int_size_in_bytes (type) > 12 && TYPE_MODE (type) != TImode
	      && TYPE_MODE (type) != TFmode
	      && !VECTOR_MODE_P (TYPE_MODE (type))))
	return 1;
      return 0;
    }
}

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
rtx
ix86_libcall_value (mode)
   enum machine_mode mode;
{
  if (TARGET_64BIT)
    {
      switch (mode)
	{
	  case SFmode:
	  case SCmode:
	  case DFmode:
	  case DCmode:
	    return gen_rtx_REG (mode, FIRST_SSE_REG);
	  case TFmode:
	  case TCmode:
	    return gen_rtx_REG (mode, FIRST_FLOAT_REG);
	  default:
	    return gen_rtx_REG (mode, 0);
	}
    }
  else
   return gen_rtx_REG (mode, ix86_value_regno (mode));
}

/* Given a mode, return the register to use for a return value.  */

static int
ix86_value_regno (mode)
     enum machine_mode mode;
{
  if (GET_MODE_CLASS (mode) == MODE_FLOAT && TARGET_FLOAT_RETURNS_IN_80387)
    return FIRST_FLOAT_REG;
  if (mode == TImode || VECTOR_MODE_P (mode))
    return FIRST_SSE_REG;
  return 0;
}

/* Create the va_list data type.  */

tree
ix86_build_va_list ()
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  /* For i386 we use plain pointer to argument area.  */
  if (!TARGET_64BIT)
    return build_pointer_type (char_type_node);

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (FIELD_DECL, get_identifier ("gp_offset"),
		      unsigned_type_node);
  f_fpr = build_decl (FIELD_DECL, get_identifier ("fp_offset"),
		      unsigned_type_node);
  f_ovf = build_decl (FIELD_DECL, get_identifier ("overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (FIELD_DECL, get_identifier ("reg_save_area"),
		      ptr_type_node);

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TREE_CHAIN (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  TREE_CHAIN (f_gpr) = f_fpr;
  TREE_CHAIN (f_fpr) = f_ovf;
  TREE_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

void
ix86_setup_incoming_varargs (cum, mode, type, pretend_size, no_rtl)
     CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int *pretend_size ATTRIBUTE_UNUSED;
     int no_rtl;

{
  CUMULATIVE_ARGS next_cum;
  rtx save_area = NULL_RTX, mem;
  rtx label;
  rtx label_ref;
  rtx tmp_reg;
  rtx nsse_reg;
  int set;
  tree fntype;
  int stdarg_p;
  int i;

  if (!TARGET_64BIT)
    return;

  /* Indicate to allocate space on the stack for varargs save area.  */
  ix86_save_varrargs_registers = 1;

  cfun->stack_alignment_needed = 128;

  fntype = TREE_TYPE (current_function_decl);
  stdarg_p = (TYPE_ARG_TYPES (fntype) != 0
	      && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		  != void_type_node));

  /* For varargs, we do not want to skip the dummy va_dcl argument.
     For stdargs, we do want to skip the last named argument.  */
  next_cum = *cum;
  if (stdarg_p)
    function_arg_advance (&next_cum, mode, type, 1);

  if (!no_rtl)
    save_area = frame_pointer_rtx;

  set = get_varargs_alias_set ();

  for (i = next_cum.regno; i < ix86_regparm; i++)
    {
      mem = gen_rtx_MEM (Pmode,
			 plus_constant (save_area, i * UNITS_PER_WORD));
      set_mem_alias_set (mem, set);
      emit_move_insn (mem, gen_rtx_REG (Pmode,
					x86_64_int_parameter_registers[i]));
    }

  if (next_cum.sse_nregs)
    {
      /* Now emit code to save SSE registers.  The AX parameter contains number
	 of SSE parameter regsiters used to call this function.  We use
	 sse_prologue_save insn template that produces computed jump across
	 SSE saves.  We need some preparation work to get this working.  */

      label = gen_label_rtx ();
      label_ref = gen_rtx_LABEL_REF (Pmode, label);

      /* Compute address to jump to :
         label - 5*eax + nnamed_sse_arguments*5  */
      tmp_reg = gen_reg_rtx (Pmode);
      nsse_reg = gen_reg_rtx (Pmode);
      emit_insn (gen_zero_extendqidi2 (nsse_reg, gen_rtx_REG (QImode, 0)));
      emit_insn (gen_rtx_SET (VOIDmode, tmp_reg,
			      gen_rtx_MULT (Pmode, nsse_reg,
					    GEN_INT (4))));
      if (next_cum.sse_regno)
	emit_move_insn
	  (nsse_reg,
	   gen_rtx_CONST (DImode,
			  gen_rtx_PLUS (DImode,
					label_ref,
					GEN_INT (next_cum.sse_regno * 4))));
      else
	emit_move_insn (nsse_reg, label_ref);
      emit_insn (gen_subdi3 (nsse_reg, nsse_reg, tmp_reg));

      /* Compute address of memory block we save into.  We always use pointer
	 pointing 127 bytes after first byte to store - this is needed to keep
	 instruction size limited by 4 bytes.  */
      tmp_reg = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx_SET (VOIDmode, tmp_reg,
			      plus_constant (save_area,
					     8 * REGPARM_MAX + 127)));
      mem = gen_rtx_MEM (BLKmode, plus_constant (tmp_reg, -127));
      set_mem_alias_set (mem, set);
      set_mem_align (mem, BITS_PER_WORD);

      /* And finally do the dirty job!  */
      emit_insn (gen_sse_prologue_save (mem, nsse_reg,
					GEN_INT (next_cum.sse_regno), label));
    }

}

/* Implement va_start.  */

void
ix86_va_start (valist, nextarg)
     tree valist;
     rtx nextarg;
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  /* Only 64bit target needs something special.  */
  if (!TARGET_64BIT)
    {
      std_expand_builtin_va_start (valist, nextarg);
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  /* Count number of gp and fp argument registers used.  */
  words = current_function_args_info.words;
  n_gpr = current_function_args_info.regno;
  n_fpr = current_function_args_info.sse_regno;

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "va_start: words = %d, n_gpr = %d, n_fpr = %d\n",
	     (int) words, (int) n_gpr, (int) n_fpr);

  t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr,
	     build_int_2 (n_gpr * 8, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  t = build (MODIFY_EXPR, TREE_TYPE (fpr), fpr,
	     build_int_2 (n_fpr * 16 + 8*REGPARM_MAX, 0));
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the overflow area.  */
  t = make_tree (TREE_TYPE (ovf), virtual_incoming_args_rtx);
  if (words != 0)
    t = build (PLUS_EXPR, TREE_TYPE (ovf), t,
	       build_int_2 (words * UNITS_PER_WORD, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Find the register save area.
     Prologue of the function save it right above stack frame.  */
  t = make_tree (TREE_TYPE (sav), frame_pointer_rtx);
  t = build (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement va_arg.  */
rtx
ix86_va_arg (valist, type)
     tree valist, type;
{
  static const int intreg[6] = { 0, 1, 2, 3, 4, 5 };
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;
  int size, rsize;
  rtx lab_false, lab_over = NULL_RTX;
  rtx addr_rtx, r;
  rtx container;
  int indirect_p = 0;

  /* Only 64bit target needs something special.  */
  if (!TARGET_64BIT)
    {
      return std_expand_builtin_va_arg (valist, type);
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr);
  fpr = build (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr);
  ovf = build (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf);
  sav = build (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav);

  size = int_size_in_bytes (type);
  if (size == -1)
    {
      /* Passed by reference.  */
      indirect_p = 1;
      type = build_pointer_type (type);
      size = int_size_in_bytes (type);
    }
  rsize = (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  container = construct_container (TYPE_MODE (type), type, 0,
				   REGPARM_MAX, SSE_REGPARM_MAX, intreg, 0);
  /*
   * Pull the value out of the saved registers ...
   */

  addr_rtx = gen_reg_rtx (Pmode);

  if (container)
    {
      rtx int_addr_rtx, sse_addr_rtx;
      int needed_intregs, needed_sseregs;
      int need_temp;

      lab_over = gen_label_rtx ();
      lab_false = gen_label_rtx ();

      examine_argument (TYPE_MODE (type), type, 0,
		        &needed_intregs, &needed_sseregs);


      need_temp = ((needed_intregs && TYPE_ALIGN (type) > 64)
		   || TYPE_ALIGN (type) > 128);

      /* In case we are passing structure, verify that it is consetuctive block
         on the register save area.  If not we need to do moves.  */
      if (!need_temp && !REG_P (container))
	{
	  /* Verify that all registers are strictly consetuctive  */
	  if (SSE_REGNO_P (REGNO (XEXP (XVECEXP (container, 0, 0), 0))))
	    {
	      int i;

	      for (i = 0; i < XVECLEN (container, 0) && !need_temp; i++)
		{
		  rtx slot = XVECEXP (container, 0, i);
		  if (REGNO (XEXP (slot, 0)) != FIRST_SSE_REG + (unsigned int) i
		      || INTVAL (XEXP (slot, 1)) != i * 16)
		    need_temp = 1;
		}
	    }
	  else
	    {
	      int i;

	      for (i = 0; i < XVECLEN (container, 0) && !need_temp; i++)
		{
		  rtx slot = XVECEXP (container, 0, i);
		  if (REGNO (XEXP (slot, 0)) != (unsigned int) i
		      || INTVAL (XEXP (slot, 1)) != i * 8)
		    need_temp = 1;
		}
	    }
	}
      if (!need_temp)
	{
	  int_addr_rtx = addr_rtx;
	  sse_addr_rtx = addr_rtx;
	}
      else
	{
	  int_addr_rtx = gen_reg_rtx (Pmode);
	  sse_addr_rtx = gen_reg_rtx (Pmode);
	}
      /* First ensure that we fit completely in registers.  */
      if (needed_intregs)
	{
	  emit_cmp_and_jump_insns (expand_expr
				   (gpr, NULL_RTX, SImode, EXPAND_NORMAL),
				   GEN_INT ((REGPARM_MAX - needed_intregs +
					     1) * 8), GE, const1_rtx, SImode,
				   1, lab_false);
	}
      if (needed_sseregs)
	{
	  emit_cmp_and_jump_insns (expand_expr
				   (fpr, NULL_RTX, SImode, EXPAND_NORMAL),
				   GEN_INT ((SSE_REGPARM_MAX -
					     needed_sseregs + 1) * 16 +
					    REGPARM_MAX * 8), GE, const1_rtx,
				   SImode, 1, lab_false);
	}

      /* Compute index to start of area used for integer regs.  */
      if (needed_intregs)
	{
	  t = build (PLUS_EXPR, ptr_type_node, sav, gpr);
	  r = expand_expr (t, int_addr_rtx, Pmode, EXPAND_NORMAL);
	  if (r != int_addr_rtx)
	    emit_move_insn (int_addr_rtx, r);
	}
      if (needed_sseregs)
	{
	  t = build (PLUS_EXPR, ptr_type_node, sav, fpr);
	  r = expand_expr (t, sse_addr_rtx, Pmode, EXPAND_NORMAL);
	  if (r != sse_addr_rtx)
	    emit_move_insn (sse_addr_rtx, r);
	}
      if (need_temp)
	{
	  int i;
	  rtx mem;
	  rtx x;

	  /* Never use the memory itself, as it has the alias set.  */
	  x = XEXP (assign_temp (type, 0, 1, 0), 0);
	  mem = gen_rtx_MEM (BLKmode, x);
	  force_operand (x, addr_rtx);
	  set_mem_alias_set (mem, get_varargs_alias_set ());
	  set_mem_align (mem, BITS_PER_UNIT);

	  for (i = 0; i < XVECLEN (container, 0); i++)
	    {
	      rtx slot = XVECEXP (container, 0, i);
	      rtx reg = XEXP (slot, 0);
	      enum machine_mode mode = GET_MODE (reg);
	      rtx src_addr;
	      rtx src_mem;
	      int src_offset;
	      rtx dest_mem;

	      if (SSE_REGNO_P (REGNO (reg)))
		{
		  src_addr = sse_addr_rtx;
		  src_offset = (REGNO (reg) - FIRST_SSE_REG) * 16;
		}
	      else
		{
		  src_addr = int_addr_rtx;
		  src_offset = REGNO (reg) * 8;
		}
	      src_mem = gen_rtx_MEM (mode, src_addr);
	      set_mem_alias_set (src_mem, get_varargs_alias_set ());
	      src_mem = adjust_address (src_mem, mode, src_offset);
	      dest_mem = adjust_address (mem, mode, INTVAL (XEXP (slot, 1)));
	      emit_move_insn (dest_mem, src_mem);
	    }
	}

      if (needed_intregs)
	{
	  t =
	    build (PLUS_EXPR, TREE_TYPE (gpr), gpr,
		   build_int_2 (needed_intregs * 8, 0));
	  t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr, t);
	  TREE_SIDE_EFFECTS (t) = 1;
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
	}
      if (needed_sseregs)
	{
	  t =
	    build (PLUS_EXPR, TREE_TYPE (fpr), fpr,
		   build_int_2 (needed_sseregs * 16, 0));
	  t = build (MODIFY_EXPR, TREE_TYPE (fpr), fpr, t);
	  TREE_SIDE_EFFECTS (t) = 1;
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
	}

      emit_jump_insn (gen_jump (lab_over));
      emit_barrier ();
      emit_label (lab_false);
    }

  /* ... otherwise out of the overflow area.  */

  /* Care for on-stack alignment if needed.  */
  if (FUNCTION_ARG_BOUNDARY (VOIDmode, type) <= 64)
    t = ovf;
  else
    {
      HOST_WIDE_INT align = FUNCTION_ARG_BOUNDARY (VOIDmode, type) / 8;
      t = build (PLUS_EXPR, TREE_TYPE (ovf), ovf, build_int_2 (align - 1, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t, build_int_2 (-align, -1));
    }
  t = save_expr (t);

  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
  if (r != addr_rtx)
    emit_move_insn (addr_rtx, r);

  t =
    build (PLUS_EXPR, TREE_TYPE (t), t,
	   build_int_2 (rsize * UNITS_PER_WORD, 0));
  t = build (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  if (container)
    emit_label (lab_over);

  if (indirect_p)
    {
      r = gen_rtx_MEM (Pmode, addr_rtx);
      set_mem_alias_set (r, get_varargs_alias_set ());
      emit_move_insn (addr_rtx, r);
    }

  return addr_rtx;
}

/* Return nonzero if OP is either a i387 or SSE fp register.  */
int
any_fp_register_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return ANY_FP_REG_P (op);
}

/* Return nonzero if OP is an i387 fp register.  */
int
fp_register_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return FP_REG_P (op);
}

/* Return nonzero if OP is a non-fp register_operand.  */
int
register_and_not_any_fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) && !ANY_FP_REG_P (op);
}

/* Return nonzero of OP is a register operand other than an
   i387 fp register.  */
int
register_and_not_fp_reg_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return register_operand (op, mode) && !FP_REG_P (op);
}

/* Return nonzero if OP is general operand representable on x86_64.  */

int
x86_64_general_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!TARGET_64BIT)
    return general_operand (op, mode);
  if (nonimmediate_operand (op, mode))
    return 1;
  return x86_64_sign_extended_value (op);
}

/* Return nonzero if OP is general operand representable on x86_64
   as either sign extended or zero extended constant.  */

int
x86_64_szext_general_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!TARGET_64BIT)
    return general_operand (op, mode);
  if (nonimmediate_operand (op, mode))
    return 1;
  return x86_64_sign_extended_value (op) || x86_64_zero_extended_value (op);
}

/* Return nonzero if OP is nonmemory operand representable on x86_64.  */

int
x86_64_nonmemory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!TARGET_64BIT)
    return nonmemory_operand (op, mode);
  if (register_operand (op, mode))
    return 1;
  return x86_64_sign_extended_value (op);
}

/* Return nonzero if OP is nonmemory operand acceptable by movabs patterns.  */

int
x86_64_movabs_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!TARGET_64BIT || !flag_pic)
    return nonmemory_operand (op, mode);
  if (register_operand (op, mode) || x86_64_sign_extended_value (op))
    return 1;
  if (CONSTANT_P (op) && !symbolic_reference_mentioned_p (op))
    return 1;
  return 0;
}

/* Return nonzero if OPNUM's MEM should be matched
   in movabs* patterns.  */

int
ix86_check_movabs (insn, opnum)
     rtx insn;
     int opnum;
{
  rtx set, mem;

  set = PATTERN (insn);
  if (GET_CODE (set) == PARALLEL)
    set = XVECEXP (set, 0, 0);
  if (GET_CODE (set) != SET)
    abort ();
  mem = XEXP (set, opnum);
  while (GET_CODE (mem) == SUBREG)
    mem = SUBREG_REG (mem);
  if (GET_CODE (mem) != MEM)
    abort ();
  return (volatile_ok || !MEM_VOLATILE_P (mem));
}

/* Return nonzero if OP is nonmemory operand representable on x86_64.  */

int
x86_64_szext_nonmemory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!TARGET_64BIT)
    return nonmemory_operand (op, mode);
  if (register_operand (op, mode))
    return 1;
  return x86_64_sign_extended_value (op) || x86_64_zero_extended_value (op);
}

/* Return nonzero if OP is immediate operand representable on x86_64.  */

int
x86_64_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (!TARGET_64BIT)
    return immediate_operand (op, mode);
  return x86_64_sign_extended_value (op);
}

/* Return nonzero if OP is immediate operand representable on x86_64.  */

int
x86_64_zext_immediate_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return x86_64_zero_extended_value (op);
}

/* Return nonzero if OP is (const_int 1), else return zero.  */

int
const_int_1_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && INTVAL (op) == 1);
}

/* Return nonzero if OP is CONST_INT >= 1 and <= 31 (a valid operand
   for shift & compare patterns, as shifting by 0 does not change flags),
   else return zero.  */

int
const_int_1_31_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && INTVAL (op) >= 1 && INTVAL (op) <= 31);
}

/* Returns 1 if OP is either a symbol reference or a sum of a symbol
   reference and a constant.  */

int
symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
    case LABEL_REF:
      return 1;

    case CONST:
      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF
	  || (GET_CODE (op) == UNSPEC
	      && (XINT (op, 1) == UNSPEC_GOT
		  || XINT (op, 1) == UNSPEC_GOTOFF
		  || XINT (op, 1) == UNSPEC_GOTPCREL)))
	return 1;
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return 0;

      op = XEXP (op, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return 1;
      /* Only @GOTOFF gets offsets.  */
      if (GET_CODE (op) != UNSPEC
	  || XINT (op, 1) != UNSPEC_GOTOFF)
	return 0;

      op = XVECEXP (op, 0, 0);
      if (GET_CODE (op) == SYMBOL_REF
	  || GET_CODE (op) == LABEL_REF)
	return 1;
      return 0;

    default:
      return 0;
    }
}

/* Return true if the operand contains a @GOT or @GOTOFF reference.  */

int
pic_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) != CONST)
    return 0;
  op = XEXP (op, 0);
  if (TARGET_64BIT)
    {
      if (GET_CODE (XEXP (op, 0)) == UNSPEC)
	return 1;
    }
  else
    {
      if (GET_CODE (op) == UNSPEC)
	return 1;
      if (GET_CODE (op) != PLUS
	  || GET_CODE (XEXP (op, 1)) != CONST_INT)
	return 0;
      op = XEXP (op, 0);
      if (GET_CODE (op) == UNSPEC)
	return 1;
    }
  return 0;
}

/* Return true if OP is a symbolic operand that resolves locally.  */

static int
local_symbolic_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT)
    op = XEXP (XEXP (op, 0), 0);

  if (GET_CODE (op) == LABEL_REF)
    return 1;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;

  /* These we've been told are local by varasm and encode_section_info
     respectively.  */
  if (CONSTANT_POOL_ADDRESS_P (op) || SYMBOL_REF_FLAG (op))
    return 1;

  /* There is, however, a not insubstantial body of code in the rest of
     the compiler that assumes it can just stick the results of
     ASM_GENERATE_INTERNAL_LABEL in a symbol_ref and have done.  */
  /* ??? This is a hack.  Should update the body of the compiler to
     always create a DECL an invoke targetm.encode_section_info.  */
  if (strncmp (XSTR (op, 0), internal_label_prefix,
	       internal_label_prefix_len) == 0)
    return 1;

  return 0;
}

/* Test for various thread-local symbols.  See ix86_encode_section_info. */

int
tls_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  const char *symbol_str;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;
  symbol_str = XSTR (op, 0);

  if (symbol_str[0] != '%')
    return 0;
  return strchr (tls_model_chars, symbol_str[1]) - tls_model_chars;
}

static int
tls_symbolic_operand_1 (op, kind)
     rtx op;
     enum tls_model kind;
{
  const char *symbol_str;

  if (GET_CODE (op) != SYMBOL_REF)
    return 0;
  symbol_str = XSTR (op, 0);

  return symbol_str[0] == '%' && symbol_str[1] == tls_model_chars[kind];
}

int
global_dynamic_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return tls_symbolic_operand_1 (op, TLS_MODEL_GLOBAL_DYNAMIC);
}

int
local_dynamic_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return tls_symbolic_operand_1 (op, TLS_MODEL_LOCAL_DYNAMIC);
}

int
initial_exec_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return tls_symbolic_operand_1 (op, TLS_MODEL_INITIAL_EXEC);
}

int
local_exec_symbolic_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return tls_symbolic_operand_1 (op, TLS_MODEL_LOCAL_EXEC);
}

/* Test for a valid operand for a call instruction.  Don't allow the
   arg pointer register or virtual regs since they may decay into
   reg + const, which the patterns can't handle.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* Disallow indirect through a virtual register.  This leads to
     compiler aborts when trying to eliminate them.  */
  if (GET_CODE (op) == REG
      && (op == arg_pointer_rtx
	  || op == frame_pointer_rtx
	  || (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      && REGNO (op) <= LAST_VIRTUAL_REGISTER)))
    return 0;

  /* Disallow `call 1234'.  Due to varying assembler lameness this
     gets either rejected or translated to `call .+1234'.  */
  if (GET_CODE (op) == CONST_INT)
    return 0;

  /* Explicitly allow SYMBOL_REF even if pic.  */
  if (GET_CODE (op) == SYMBOL_REF)
    return 1;

  /* Otherwise we can allow any general_operand in the address.  */
  return general_operand (op, Pmode);
}

int
constant_call_address_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT)
    op = XEXP (XEXP (op, 0), 0);
  return GET_CODE (op) == SYMBOL_REF;
}

/* Match exactly zero and one.  */

int
const0_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return op == CONST0_RTX (mode);
}

int
const1_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return op == const1_rtx;
}

/* Match 2, 4, or 8.  Used for leal multiplicands.  */

int
const248_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT
	  && (INTVAL (op) == 2 || INTVAL (op) == 4 || INTVAL (op) == 8));
}

/* True if this is a constant appropriate for an increment or decremenmt.  */

int
incdec_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  /* On Pentium4, the inc and dec operations causes extra dependency on flag
     registers, since carry flag is not set.  */
  if (TARGET_PENTIUM4 && !optimize_size)
    return 0;
  return op == const1_rtx || op == constm1_rtx;
}

/* Return nonzero if OP is acceptable as operand of DImode shift
   expander.  */

int
shiftdi_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (TARGET_64BIT)
    return nonimmediate_operand (op, mode);
  else
    return register_operand (op, mode);
}

/* Return false if this is the stack pointer, or any other fake
   register eliminable to the stack pointer.  Otherwise, this is
   a register operand.

   This is used to prevent esp from being used as an index reg.
   Which would only happen in pathological cases.  */

int
reg_no_sp_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (t == stack_pointer_rtx || t == arg_pointer_rtx || t == frame_pointer_rtx)
    return 0;

  return register_operand (op, mode);
}

int
mmx_reg_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return MMX_REG_P (op);
}

/* Return false if this is any eliminable register.  Otherwise
   general_operand.  */

int
general_no_elim_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (t == arg_pointer_rtx || t == frame_pointer_rtx
      || t == virtual_incoming_args_rtx || t == virtual_stack_vars_rtx
      || t == virtual_stack_dynamic_rtx)
    return 0;
  if (REG_P (t)
      && REGNO (t) >= FIRST_VIRTUAL_REGISTER
      && REGNO (t) <= LAST_VIRTUAL_REGISTER)
    return 0;

  return general_operand (op, mode);
}

/* Return false if this is any eliminable register.  Otherwise
   register_operand or const_int.  */

int
nonmemory_no_elim_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (t == arg_pointer_rtx || t == frame_pointer_rtx
      || t == virtual_incoming_args_rtx || t == virtual_stack_vars_rtx
      || t == virtual_stack_dynamic_rtx)
    return 0;

  return GET_CODE (op) == CONST_INT || register_operand (op, mode);
}

/* Return false if this is any eliminable register or stack register,
   otherwise work like register_operand.  */

int
index_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  rtx t = op;
  if (GET_CODE (t) == SUBREG)
    t = SUBREG_REG (t);
  if (!REG_P (t))
    return 0;
  if (t == arg_pointer_rtx
      || t == frame_pointer_rtx
      || t == virtual_incoming_args_rtx
      || t == virtual_stack_vars_rtx
      || t == virtual_stack_dynamic_rtx
      || REGNO (t) == STACK_POINTER_REGNUM)
    return 0;

  return general_operand (op, mode);
}

/* Return true if op is a Q_REGS class register.  */

int
q_regs_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return ANY_QI_REG_P (op);
}

/* Return true if op is a NON_Q_REGS class register.  */

int
non_q_regs_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  return NON_QI_REG_P (op);
}

/*  Return 1 when OP is operand acceptable for standard SSE move.  */
int
vector_move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (nonimmediate_operand (op, mode))
    return 1;
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;
  return (op == CONST0_RTX (GET_MODE (op)));
}

/* Return 1 if OP is a comparison that can be used in the CMPSS/CMPPS
   insns.  */
int
sse_comparison_operator (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (op);
  switch (code)
    {
    /* Operations supported directly.  */
    case EQ:
    case LT:
    case LE:
    case UNORDERED:
    case NE:
    case UNGE:
    case UNGT:
    case ORDERED:
      return 1;
    /* These are equivalent to ones above in non-IEEE comparisons.  */
    case UNEQ:
    case UNLT:
    case UNLE:
    case LTGT:
    case GE:
    case GT:
      return !TARGET_IEEE_FP;
    default:
      return 0;
    }
}
/* Return 1 if OP is a valid comparison operator in valid mode.  */
int
ix86_comparison_operator (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  enum machine_mode inmode;
  enum rtx_code code = GET_CODE (op);
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;
  if (GET_RTX_CLASS (code) != '<')
    return 0;
  inmode = GET_MODE (XEXP (op, 0));

  if (inmode == CCFPmode || inmode == CCFPUmode)
    {
      enum rtx_code second_code, bypass_code;
      ix86_fp_comparison_codes (code, &bypass_code, &code, &second_code);
      return (bypass_code == NIL && second_code == NIL);
    }
  switch (code)
    {
    case EQ: case NE:
      return 1;
    case LT: case GE:
      if (inmode == CCmode || inmode == CCGCmode
	  || inmode == CCGOCmode || inmode == CCNOmode)
	return 1;
      return 0;
    case LTU: case GTU: case LEU: case ORDERED: case UNORDERED: case GEU:
      if (inmode == CCmode)
	return 1;
      return 0;
    case GT: case LE:
      if (inmode == CCmode || inmode == CCGCmode || inmode == CCNOmode)
	return 1;
      return 0;
    default:
      return 0;
    }
}

/* Return 1 if OP is a comparison operator that can be issued by fcmov.  */

int
fcmov_comparison_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  enum machine_mode inmode;
  enum rtx_code code = GET_CODE (op);
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;
  if (GET_RTX_CLASS (code) != '<')
    return 0;
  inmode = GET_MODE (XEXP (op, 0));
  if (inmode == CCFPmode || inmode == CCFPUmode)
    {
      enum rtx_code second_code, bypass_code;
      ix86_fp_comparison_codes (code, &bypass_code, &code, &second_code);
      if (bypass_code != NIL || second_code != NIL)
	return 0;
      code = ix86_fp_compare_code_to_integer (code);
    }
  /* i387 supports just limited amount of conditional codes.  */
  switch (code)
    {
    case LTU: case GTU: case LEU: case GEU:
      if (inmode == CCmode || inmode == CCFPmode || inmode == CCFPUmode)
	return 1;
      return 0;
    case ORDERED: case UNORDERED:
    case EQ: case NE:
      return 1;
    default:
      return 0;
    }
}

/* Return 1 if OP is a binary operator that can be promoted to wider mode.  */

int
promotable_binary_operator (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  switch (GET_CODE (op))
    {
    case MULT:
      /* Modern CPUs have same latency for HImode and SImode multiply,
         but 386 and 486 do HImode multiply faster.  */
      return ix86_cpu > PROCESSOR_I486;
    case PLUS:
    case AND:
    case IOR:
    case XOR:
    case ASHIFT:
      return 1;
    default:
      return 0;
    }
}

/* Nearly general operand, but accept any const_double, since we wish
   to be able to drop them into memory rather than have them get pulled
   into registers.  */

int
cmp_fp_expander_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == CONST_DOUBLE)
    return 1;
  return general_operand (op, mode);
}

/* Match an SI or HImode register for a zero_extract.  */

int
ext_register_operand (op, mode)
     register rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  int regno;
  if ((!TARGET_64BIT || GET_MODE (op) != DImode)
      && GET_MODE (op) != SImode && GET_MODE (op) != HImode)
    return 0;

  if (!register_operand (op, VOIDmode))
    return 0;

  /* Be curefull to accept only registers having upper parts.  */
  regno = REG_P (op) ? REGNO (op) : REGNO (SUBREG_REG (op));
  return (regno > LAST_VIRTUAL_REGISTER || regno < 4);
}

/* Return 1 if this is a valid binary floating-point operation.
   OP is the expression matched, and MODE is its mode.  */

int
binary_fp_operator (op, mode)
    register rtx op;
    enum machine_mode mode;
{
  if (mode != VOIDmode && mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case PLUS:
    case MINUS:
    case MULT:
    case DIV:
      return GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT;

    default:
      return 0;
    }
}

int
mult_operator (op, mode)
    register rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == MULT;
}

int
div_operator (op, mode)
    register rtx op;
    enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return GET_CODE (op) == DIV;
}

int
arith_or_logical_operator (op, mode)
      rtx op;
      enum machine_mode mode;
{
  return ((mode == VOIDmode || GET_MODE (op) == mode)
          && (GET_RTX_CLASS (GET_CODE (op)) == 'c'
              || GET_RTX_CLASS (GET_CODE (op)) == '2'));
}

/* Returns 1 if OP is memory operand with a displacement.  */

int
memory_displacement_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  struct ix86_address parts;

  if (! memory_operand (op, mode))
    return 0;

  if (! ix86_decompose_address (XEXP (op, 0), &parts))
    abort ();

  return parts.disp != NULL_RTX;
}

/* To avoid problems when jump re-emits comparisons like testqi_ext_ccno_0,
   re-recognize the operand to avoid a copy_to_mode_reg that will fail.

   ??? It seems likely that this will only work because cmpsi is an
   expander, and no actual insns use this.  */

int
cmpsi_operand (op, mode)
      rtx op;
      enum machine_mode mode;
{
  if (nonimmediate_operand (op, mode))
    return 1;

  if (GET_CODE (op) == AND
      && GET_MODE (op) == SImode
      && GET_CODE (XEXP (op, 0)) == ZERO_EXTRACT
      && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (XEXP (op, 0), 2)) == CONST_INT
      && INTVAL (XEXP (XEXP (op, 0), 1)) == 8
      && INTVAL (XEXP (XEXP (op, 0), 2)) == 8
      && GET_CODE (XEXP (op, 1)) == CONST_INT)
    return 1;

  return 0;
}

/* Returns 1 if OP is memory operand that can not be represented by the
   modRM array.  */

int
long_memory_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (! memory_operand (op, mode))
    return 0;

  return memory_address_length (op) != 0;
}

/* Return nonzero if the rtx is known aligned.  */

int
aligned_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  struct ix86_address parts;

  if (!general_operand (op, mode))
    return 0;

  /* Registers and immediate operands are always "aligned".  */
  if (GET_CODE (op) != MEM)
    return 1;

  /* Don't even try to do any aligned optimizations with volatiles.  */
  if (MEM_VOLATILE_P (op))
    return 0;

  op = XEXP (op, 0);

  /* Pushes and pops are only valid on the stack pointer.  */
  if (GET_CODE (op) == PRE_DEC
      || GET_CODE (op) == POST_INC)
    return 1;

  /* Decode the address.  */
  if (! ix86_decompose_address (op, &parts))
    abort ();

  if (parts.base && GET_CODE (parts.base) == SUBREG)
    parts.base = SUBREG_REG (parts.base);
  if (parts.index && GET_CODE (parts.index) == SUBREG)
    parts.index = SUBREG_REG (parts.index);

  /* Look for some component that isn't known to be aligned.  */
  if (parts.index)
    {
      if (parts.scale < 4
	  && REGNO_POINTER_ALIGN (REGNO (parts.index)) < 32)
	return 0;
    }
  if (parts.base)
    {
      if (REGNO_POINTER_ALIGN (REGNO (parts.base)) < 32)
	return 0;
    }
  if (parts.disp)
    {
      if (GET_CODE (parts.disp) != CONST_INT
	  || (INTVAL (parts.disp) & 3) != 0)
	return 0;
    }

  /* Didn't find one -- this must be an aligned address.  */
  return 1;
}

/* Return true if the constant is something that can be loaded with
   a special instruction.  Only handle 0.0 and 1.0; others are less
   worthwhile.  */

int
standard_80387_constant_p (x)
     rtx x;
{
  if (GET_CODE (x) != CONST_DOUBLE || !FLOAT_MODE_P (GET_MODE (x)))
    return -1;
  /* Note that on the 80387, other constants, such as pi, that we should support
     too.  On some machines, these are much slower to load as standard constant,
     than to load from doubles in memory.  */
  if (x == CONST0_RTX (GET_MODE (x)))
    return 1;
  if (x == CONST1_RTX (GET_MODE (x)))
    return 2;
  return 0;
}

/* Return 1 if X is FP constant we can load to SSE register w/o using memory.
 */
int
standard_sse_constant_p (x)
     rtx x;
{
  if (x == const0_rtx)
    return 1;
  return (x == CONST0_RTX (GET_MODE (x)));
}

/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (op)
     rtx op;
{
  register const char *fmt;
  register int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Return 1 if it is appropriate to emit `ret' instructions in the
   body of a function.  Do this only if the epilogue is simple, needing a
   couple of insns.  Prior to reloading, we can't tell how many registers
   must be saved, so return 0 then.  Return 0 if there is no frame
   marker to de-allocate.

   If NON_SAVING_SETJMP is defined and true, then it is not possible
   for the epilogue to be simple, so return 0.  This is a special case
   since NON_SAVING_SETJMP will not cause regs_ever_live to change
   until final, but jump_optimize may need to know sooner if a
   `return' is OK.  */

int
ix86_can_use_return_insn_p ()
{
  struct ix86_frame frame;

#ifdef NON_SAVING_SETJMP
  if (NON_SAVING_SETJMP && current_function_calls_setjmp)
    return 0;
#endif

  if (! reload_completed || frame_pointer_needed)
    return 0;

  /* Don't allow more than 32 pop, since that's all we can do
     with one instruction.  */
  if (current_function_pops_args
      && current_function_args_size >= 32768)
    return 0;

  ix86_compute_frame_layout (&frame);
  return frame.to_allocate == 0 && frame.nregs == 0;
}

/* Return 1 if VALUE can be stored in the sign extended immediate field.  */
int
x86_64_sign_extended_value (value)
     rtx value;
{
  switch (GET_CODE (value))
    {
      /* CONST_DOUBLES never match, since HOST_BITS_PER_WIDE_INT is known
         to be at least 32 and this all acceptable constants are
	 represented as CONST_INT.  */
      case CONST_INT:
	if (HOST_BITS_PER_WIDE_INT == 32)
	  return 1;
	else
	  {
	    HOST_WIDE_INT val = trunc_int_for_mode (INTVAL (value), DImode);
	    return trunc_int_for_mode (val, SImode) == val;
	  }
	break;

      /* For certain code models, the symbolic references are known to fit.
	 in CM_SMALL_PIC model we know it fits if it is local to the shared
	 library.  Don't count TLS SYMBOL_REFs here, since they should fit
	 only if inside of UNSPEC handled below.  */
      case SYMBOL_REF:
	return (ix86_cmodel == CM_SMALL || ix86_cmodel == CM_KERNEL);

      /* For certain code models, the code is near as well.  */
      case LABEL_REF:
	return (ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM
		|| ix86_cmodel == CM_KERNEL);

      /* We also may accept the offsetted memory references in certain special
         cases.  */
      case CONST:
	if (GET_CODE (XEXP (value, 0)) == UNSPEC)
	  switch (XINT (XEXP (value, 0), 1))
	    {
	    case UNSPEC_GOTPCREL:
	    case UNSPEC_DTPOFF:
	    case UNSPEC_GOTNTPOFF:
	    case UNSPEC_NTPOFF:
	      return 1;
	    default:
	      break;
	    }
	if (GET_CODE (XEXP (value, 0)) == PLUS)
	  {
	    rtx op1 = XEXP (XEXP (value, 0), 0);
	    rtx op2 = XEXP (XEXP (value, 0), 1);
	    HOST_WIDE_INT offset;

	    if (ix86_cmodel == CM_LARGE)
	      return 0;
	    if (GET_CODE (op2) != CONST_INT)
	      return 0;
	    offset = trunc_int_for_mode (INTVAL (op2), DImode);
	    switch (GET_CODE (op1))
	      {
		case SYMBOL_REF:
		  /* For CM_SMALL assume that latest object is 16MB before
		     end of 31bits boundary.  We may also accept pretty
		     large negative constants knowing that all objects are
		     in the positive half of address space.  */
		  if (ix86_cmodel == CM_SMALL
		      && offset < 16*1024*1024
		      && trunc_int_for_mode (offset, SImode) == offset)
		    return 1;
		  /* For CM_KERNEL we know that all object resist in the
		     negative half of 32bits address space.  We may not
		     accept negative offsets, since they may be just off
		     and we may accept pretty large positive ones.  */
		  if (ix86_cmodel == CM_KERNEL
		      && offset > 0
		      && trunc_int_for_mode (offset, SImode) == offset)
		    return 1;
		  break;
		case LABEL_REF:
		  /* These conditions are similar to SYMBOL_REF ones, just the
		     constraints for code models differ.  */
		  if ((ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM)
		      && offset < 16*1024*1024
		      && trunc_int_for_mode (offset, SImode) == offset)
		    return 1;
		  if (ix86_cmodel == CM_KERNEL
		      && offset > 0
		      && trunc_int_for_mode (offset, SImode) == offset)
		    return 1;
		  break;
		case UNSPEC:
		  switch (XINT (op1, 1))
		    {
		    case UNSPEC_DTPOFF:
		    case UNSPEC_NTPOFF:
		      if (offset > 0
			  && trunc_int_for_mode (offset, SImode) == offset)
			return 1;
		    }
		  break;
		default:
		  return 0;
	      }
	  }
	return 0;
      default:
	return 0;
    }
}

/* Return 1 if VALUE can be stored in the zero extended immediate field.  */
int
x86_64_zero_extended_value (value)
     rtx value;
{
  switch (GET_CODE (value))
    {
      case CONST_DOUBLE:
	if (HOST_BITS_PER_WIDE_INT == 32)
	  return  (GET_MODE (value) == VOIDmode
		   && !CONST_DOUBLE_HIGH (value));
	else
	  return 0;
      case CONST_INT:
	if (HOST_BITS_PER_WIDE_INT == 32)
	  return INTVAL (value) >= 0;
	else
	  return !(INTVAL (value) & ~(HOST_WIDE_INT) 0xffffffff);
	break;

      /* For certain code models, the symbolic references are known to fit.  */
      case SYMBOL_REF:
	return ix86_cmodel == CM_SMALL;

      /* For certain code models, the code is near as well.  */
      case LABEL_REF:
	return ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM;

      /* We also may accept the offsetted memory references in certain special
         cases.  */
      case CONST:
	if (GET_CODE (XEXP (value, 0)) == PLUS)
	  {
	    rtx op1 = XEXP (XEXP (value, 0), 0);
	    rtx op2 = XEXP (XEXP (value, 0), 1);

	    if (ix86_cmodel == CM_LARGE)
	      return 0;
	    switch (GET_CODE (op1))
	      {
		case SYMBOL_REF:
		    return 0;
		  /* For small code model we may accept pretty large positive
		     offsets, since one bit is available for free.  Negative
		     offsets are limited by the size of NULL pointer area
		     specified by the ABI.  */
		  if (ix86_cmodel == CM_SMALL
		      && GET_CODE (op2) == CONST_INT
		      && trunc_int_for_mode (INTVAL (op2), DImode) > -0x10000
		      && (trunc_int_for_mode (INTVAL (op2), SImode)
			  == INTVAL (op2)))
		    return 1;
	          /* ??? For the kernel, we may accept adjustment of
		     -0x10000000, since we know that it will just convert
		     negative address space to positive, but perhaps this
		     is not worthwhile.  */
		  break;
		case LABEL_REF:
		  /* These conditions are similar to SYMBOL_REF ones, just the
		     constraints for code models differ.  */
		  if ((ix86_cmodel == CM_SMALL || ix86_cmodel == CM_MEDIUM)
		      && GET_CODE (op2) == CONST_INT
		      && trunc_int_for_mode (INTVAL (op2), DImode) > -0x10000
		      && (trunc_int_for_mode (INTVAL (op2), SImode)
			  == INTVAL (op2)))
		    return 1;
		  break;
		default:
		  return 0;
	      }
	  }
	return 0;
      default:
	return 0;
    }
}

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may
   be accessed via the stack pointer) in functions that seem suitable.  */

int
ix86_frame_pointer_required ()
{
  /* If we accessed previous frames, then the generated code expects
     to be able to access the saved ebp value in our frame.  */
  if (cfun->machine->accesses_prev_frame)
    return 1;

  /* Several x86 os'es need a frame pointer for other reasons,
     usually pertaining to setjmp.  */
  if (SUBTARGET_FRAME_POINTER_REQUIRED)
    return 1;

  /* In override_options, TARGET_OMIT_LEAF_FRAME_POINTER turns off
     the frame pointer by default.  Turn it back on now if we've not
     got a leaf function.  */
  if (TARGET_OMIT_LEAF_FRAME_POINTER
      && (!current_function_is_leaf))
    return 1;

  if (current_function_profile)
    return 1;

  return 0;
}

/* Record that the current function accesses previous call frames.  */

void
ix86_setup_frame_addresses ()
{
  cfun->machine->accesses_prev_frame = 1;
}

#if defined(HAVE_GAS_HIDDEN) && defined(SUPPORTS_ONE_ONLY)
# define USE_HIDDEN_LINKONCE 1
#else
# define USE_HIDDEN_LINKONCE 0
#endif

static int pic_labels_used;

/* Fills in the label name that should be used for a pc thunk for
   the given register.  */

static void
get_pc_thunk_name (name, regno)
     char name[32];
     unsigned int regno;
{
  if (USE_HIDDEN_LINKONCE)
    sprintf (name, "__i686.get_pc_thunk.%s", reg_names[regno]);
  else
    ASM_GENERATE_INTERNAL_LABEL (name, "LPR", regno);
}


/* This function generates code for -fpic that loads %ebx with
   the return address of the caller and then returns.  */

void
ix86_asm_file_end (file)
     FILE *file;
{
  rtx xops[2];
  int regno;

  for (regno = 0; regno < 8; ++regno)
    {
      char name[32];

      if (! ((pic_labels_used >> regno) & 1))
	continue;

      get_pc_thunk_name (name, regno);

      if (USE_HIDDEN_LINKONCE)
	{
	  tree decl;

	  decl = build_decl (FUNCTION_DECL, get_identifier (name),
			     error_mark_node);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_ONE_ONLY (decl) = 1;

	  (*targetm.asm_out.unique_section) (decl, 0);
	  named_section (decl, NULL, 0);

	  (*targetm.asm_out.globalize_label) (file, name);
	  fputs ("\t.hidden\t", file);
	  assemble_name (file, name);
	  fputc ('\n', file);
	  ASM_DECLARE_FUNCTION_NAME (file, name, decl);
	}
      else
	{
	  text_section ();
	  ASM_OUTPUT_LABEL (file, name);
	}

      xops[0] = gen_rtx_REG (SImode, regno);
      xops[1] = gen_rtx_MEM (SImode, stack_pointer_rtx);
      output_asm_insn ("mov{l}\t{%1, %0|%0, %1}", xops);
      output_asm_insn ("ret", xops);
    }
}

/* Emit code for the SET_GOT patterns.  */

const char *
output_set_got (dest)
     rtx dest;
{
  rtx xops[3];

  xops[0] = dest;
  xops[1] = gen_rtx_SYMBOL_REF (Pmode, GOT_SYMBOL_NAME);

  if (! TARGET_DEEP_BRANCH_PREDICTION || !flag_pic)
    {
      xops[2] = gen_rtx_LABEL_REF (Pmode, gen_label_rtx ());

      if (!flag_pic)
	output_asm_insn ("mov{l}\t{%2, %0|%0, %2}", xops);
      else
	output_asm_insn ("call\t%a2", xops);

#if TARGET_MACHO
      /* Output the "canonical" label name ("Lxx$pb") here too.  This
         is what will be referred to by the Mach-O PIC subsystem.  */
      ASM_OUTPUT_LABEL (asm_out_file, machopic_function_base_name ());
#endif
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
				 CODE_LABEL_NUMBER (XEXP (xops[2], 0)));

      if (flag_pic)
	output_asm_insn ("pop{l}\t%0", xops);
    }
  else
    {
      char name[32];
      get_pc_thunk_name (name, REGNO (dest));
      pic_labels_used |= 1 << REGNO (dest);

      xops[2] = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (name));
      xops[2] = gen_rtx_MEM (QImode, xops[2]);
      output_asm_insn ("call\t%X2", xops);
    }

  if (!flag_pic || TARGET_DEEP_BRANCH_PREDICTION)
    output_asm_insn ("add{l}\t{%1, %0|%0, %1}", xops);
  else if (!TARGET_MACHO)
    output_asm_insn ("add{l}\t{%1+[.-%a2], %0|%0, %a1+(.-%a2)}", xops);

  return "";
}

/* Generate an "push" pattern for input ARG.  */

static rtx
gen_push (arg)
     rtx arg;
{
  return gen_rtx_SET (VOIDmode,
		      gen_rtx_MEM (Pmode,
				   gen_rtx_PRE_DEC (Pmode,
						    stack_pointer_rtx)),
		      arg);
}

/* Return >= 0 if there is an unused call-clobbered register available
   for the entire function.  */

static unsigned int
ix86_select_alt_pic_regnum ()
{
  if (current_function_is_leaf && !current_function_profile)
    {
      int i;
      for (i = 2; i >= 0; --i)
        if (!regs_ever_live[i])
	  return i;
    }

  return INVALID_REGNUM;
}

/* Return 1 if we need to save REGNO.  */
static int
ix86_save_reg (regno, maybe_eh_return)
     unsigned int regno;
     int maybe_eh_return;
{
  if (pic_offset_table_rtx
      && regno == REAL_PIC_OFFSET_TABLE_REGNUM
      && (regs_ever_live[REAL_PIC_OFFSET_TABLE_REGNUM]
	  || current_function_profile
	  || current_function_calls_eh_return
	  || current_function_uses_const_pool))
    {
      if (ix86_select_alt_pic_regnum () != INVALID_REGNUM)
	return 0;
      return 1;
    }

  if (current_function_calls_eh_return && maybe_eh_return)
    {
      unsigned i;
      for (i = 0; ; i++)
	{
	  unsigned test = EH_RETURN_DATA_REGNO (i);
	  if (test == INVALID_REGNUM)
	    break;
	  if (test == regno)
	    return 1;
	}
    }

  return (regs_ever_live[regno]
	  && !call_used_regs[regno]
	  && !fixed_regs[regno]
	  && (regno != HARD_FRAME_POINTER_REGNUM || !frame_pointer_needed));
}

/* Return number of registers to be saved on the stack.  */

static int
ix86_nsaved_regs ()
{
  int nregs = 0;
  int regno;

  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
    if (ix86_save_reg (regno, true))
      nregs++;
  return nregs;
}

/* Return the offset between two registers, one to be eliminated, and the other
   its replacement, at the start of a routine.  */

HOST_WIDE_INT
ix86_initial_elimination_offset (from, to)
     int from;
     int to;
{
  struct ix86_frame frame;
  ix86_compute_frame_layout (&frame);

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return frame.hard_frame_pointer_offset;
  else if (from == FRAME_POINTER_REGNUM
	   && to == HARD_FRAME_POINTER_REGNUM)
    return frame.hard_frame_pointer_offset - frame.frame_pointer_offset;
  else
    {
      if (to != STACK_POINTER_REGNUM)
	abort ();
      else if (from == ARG_POINTER_REGNUM)
	return frame.stack_pointer_offset;
      else if (from != FRAME_POINTER_REGNUM)
	abort ();
      else
	return frame.stack_pointer_offset - frame.frame_pointer_offset;
    }
}

/* Fill structure ix86_frame about frame of currently computed function.  */

static void
ix86_compute_frame_layout (frame)
     struct ix86_frame *frame;
{
  HOST_WIDE_INT total_size;
  int stack_alignment_needed = cfun->stack_alignment_needed / BITS_PER_UNIT;
  int offset;
  int preferred_alignment = cfun->preferred_stack_boundary / BITS_PER_UNIT;
  HOST_WIDE_INT size = get_frame_size ();

  frame->nregs = ix86_nsaved_regs ();
  total_size = size;

  /* Skip return address and saved base pointer.  */
  offset = frame_pointer_needed ? UNITS_PER_WORD * 2 : UNITS_PER_WORD;

  frame->hard_frame_pointer_offset = offset;

  /* Do some sanity checking of stack_alignment_needed and
     preferred_alignment, since i386 port is the only using those features
     that may break easily.  */

  if (size && !stack_alignment_needed)
    abort ();
  if (preferred_alignment < STACK_BOUNDARY / BITS_PER_UNIT)
    abort ();
  if (preferred_alignment > PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT)
    abort ();
  if (stack_alignment_needed > PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT)
    abort ();

  if (stack_alignment_needed < STACK_BOUNDARY / BITS_PER_UNIT)
    stack_alignment_needed = STACK_BOUNDARY / BITS_PER_UNIT;

  /* Register save area */
  offset += frame->nregs * UNITS_PER_WORD;

  /* Va-arg area */
  if (ix86_save_varrargs_registers)
    {
      offset += X86_64_VARARGS_SIZE;
      frame->va_arg_size = X86_64_VARARGS_SIZE;
    }
  else
    frame->va_arg_size = 0;

  /* Align start of frame for local function.  */
  frame->padding1 = ((offset + stack_alignment_needed - 1)
		     & -stack_alignment_needed) - offset;

  offset += frame->padding1;

  /* Frame pointer points here.  */
  frame->frame_pointer_offset = offset;

  offset += size;

  /* Add outgoing arguments area.  Can be skipped if we eliminated
     all the function calls as dead code.  */
  if (ACCUMULATE_OUTGOING_ARGS && !current_function_is_leaf)
    {
      offset += current_function_outgoing_args_size;
      frame->outgoing_arguments_size = current_function_outgoing_args_size;
    }
  else
    frame->outgoing_arguments_size = 0;

  /* Align stack boundary.  Only needed if we're calling another function
     or using alloca.  */
  if (!current_function_is_leaf || current_function_calls_alloca)
    frame->padding2 = ((offset + preferred_alignment - 1)
		       & -preferred_alignment) - offset;
  else
    frame->padding2 = 0;

  offset += frame->padding2;

  /* We've reached end of stack frame.  */
  frame->stack_pointer_offset = offset;

  /* Size prologue needs to allocate.  */
  frame->to_allocate =
    (size + frame->padding1 + frame->padding2
     + frame->outgoing_arguments_size + frame->va_arg_size);

  if (TARGET_64BIT && TARGET_RED_ZONE && current_function_sp_is_unchanging
      && current_function_is_leaf)
    {
      frame->red_zone_size = frame->to_allocate;
      if (frame->red_zone_size > RED_ZONE_SIZE - RED_ZONE_RESERVE)
	frame->red_zone_size = RED_ZONE_SIZE - RED_ZONE_RESERVE;
    }
  else
    frame->red_zone_size = 0;
  frame->to_allocate -= frame->red_zone_size;
  frame->stack_pointer_offset -= frame->red_zone_size;
#if 0
  fprintf (stderr, "nregs: %i\n", frame->nregs);
  fprintf (stderr, "size: %i\n", size);
  fprintf (stderr, "alignment1: %i\n", stack_alignment_needed);
  fprintf (stderr, "padding1: %i\n", frame->padding1);
  fprintf (stderr, "va_arg: %i\n", frame->va_arg_size);
  fprintf (stderr, "padding2: %i\n", frame->padding2);
  fprintf (stderr, "to_allocate: %i\n", frame->to_allocate);
  fprintf (stderr, "red_zone_size: %i\n", frame->red_zone_size);
  fprintf (stderr, "frame_pointer_offset: %i\n", frame->frame_pointer_offset);
  fprintf (stderr, "hard_frame_pointer_offset: %i\n",
	   frame->hard_frame_pointer_offset);
  fprintf (stderr, "stack_pointer_offset: %i\n", frame->stack_pointer_offset);
#endif
}

/* Emit code to save registers in the prologue.  */

static void
ix86_emit_save_regs ()
{
  register int regno;
  rtx insn;

  for (regno = FIRST_PSEUDO_REGISTER - 1; regno >= 0; regno--)
    if (ix86_save_reg (regno, true))
      {
	insn = emit_insn (gen_push (gen_rtx_REG (Pmode, regno)));
	RTX_FRAME_RELATED_P (insn) = 1;
      }
}

/* Emit code to save registers using MOV insns.  First register
   is restored from POINTER + OFFSET.  */
static void
ix86_emit_save_regs_using_mov (pointer, offset)
     rtx pointer;
     HOST_WIDE_INT offset;
{
  int regno;
  rtx insn;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (ix86_save_reg (regno, true))
      {
	insn = emit_move_insn (adjust_address (gen_rtx_MEM (Pmode, pointer),
					       Pmode, offset),
			       gen_rtx_REG (Pmode, regno));
	RTX_FRAME_RELATED_P (insn) = 1;
	offset += UNITS_PER_WORD;
      }
}

/* Expand the prologue into a bunch of separate insns.  */

void
ix86_expand_prologue ()
{
  rtx insn;
  bool pic_reg_used;
  struct ix86_frame frame;
  int use_mov = 0;
  HOST_WIDE_INT allocate;

  if (!optimize_size)
    {
      use_fast_prologue_epilogue
	 = !expensive_function_p (FAST_PROLOGUE_INSN_COUNT);
      if (TARGET_PROLOGUE_USING_MOVE)
        use_mov = use_fast_prologue_epilogue;
    }
  ix86_compute_frame_layout (&frame);

  /* Note: AT&T enter does NOT have reversed args.  Enter is probably
     slower on all targets.  Also sdb doesn't like it.  */

  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_push (hard_frame_pointer_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;

      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  allocate = frame.to_allocate;
  /* In case we are dealing only with single register and empty frame,
     push is equivalent of the mov+add sequence.  */
  if (allocate == 0 && frame.nregs <= 1)
    use_mov = 0;

  if (!use_mov)
    ix86_emit_save_regs ();
  else
    allocate += frame.nregs * UNITS_PER_WORD;

  if (allocate == 0)
    ;
  else if (! TARGET_STACK_PROBE || allocate < CHECK_STACK_LIMIT)
    {
      insn = emit_insn (gen_pro_epilogue_adjust_stack
			(stack_pointer_rtx, stack_pointer_rtx,
			 GEN_INT (-allocate)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      /* ??? Is this only valid for Win32?  */

      rtx arg0, sym;

      if (TARGET_64BIT)
	abort ();

      arg0 = gen_rtx_REG (SImode, 0);
      emit_move_insn (arg0, GEN_INT (allocate));

      sym = gen_rtx_MEM (FUNCTION_MODE,
			 gen_rtx_SYMBOL_REF (Pmode, "_alloca"));
      insn = emit_call_insn (gen_call (sym, const0_rtx, constm1_rtx));

      CALL_INSN_FUNCTION_USAGE (insn)
	= gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_USE (VOIDmode, arg0),
			     CALL_INSN_FUNCTION_USAGE (insn));

      /* Don't allow scheduling pass to move insns across __alloca
         call.  */
      emit_insn (gen_blockage (const0_rtx));
    }
  if (use_mov)
    {
      if (!frame_pointer_needed || !frame.to_allocate)
        ix86_emit_save_regs_using_mov (stack_pointer_rtx, frame.to_allocate);
      else
        ix86_emit_save_regs_using_mov (hard_frame_pointer_rtx,
				       -frame.nregs * UNITS_PER_WORD);
    }

#ifdef SUBTARGET_PROLOGUE
  SUBTARGET_PROLOGUE;
#endif

  pic_reg_used = false;
  if (pic_offset_table_rtx
      && (regs_ever_live[REAL_PIC_OFFSET_TABLE_REGNUM]
	  || current_function_profile))
    {
      unsigned int alt_pic_reg_used = ix86_select_alt_pic_regnum ();

      if (alt_pic_reg_used != INVALID_REGNUM)
	REGNO (pic_offset_table_rtx) = alt_pic_reg_used;

      pic_reg_used = true;
    }

  if (pic_reg_used)
    {
      insn = emit_insn (gen_set_got (pic_offset_table_rtx));

      /* Even with accurate pre-reload life analysis, we can wind up
	 deleting all references to the pic register after reload.
	 Consider if cross-jumping unifies two sides of a branch
	 controled by a comparison vs the only read from a global.
	 In which case, allow the set_got to be deleted, though we're
	 too late to do anything about the ebx save in the prologue.  */
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, const0_rtx, NULL);
    }

  /* Prevent function calls from be scheduled before the call to mcount.
     In the pic_reg_used case, make sure that the got load isn't deleted.  */
  if (current_function_profile)
    emit_insn (gen_blockage (pic_reg_used ? pic_offset_table_rtx : const0_rtx));
}

/* Emit code to restore saved registers using MOV insns.  First register
   is restored from POINTER + OFFSET.  */
static void
ix86_emit_restore_regs_using_mov (pointer, offset, maybe_eh_return)
     rtx pointer;
     int offset;
     int maybe_eh_return;
{
  int regno;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (ix86_save_reg (regno, maybe_eh_return))
      {
	emit_move_insn (gen_rtx_REG (Pmode, regno),
			adjust_address (gen_rtx_MEM (Pmode, pointer),
					Pmode, offset));
	offset += UNITS_PER_WORD;
      }
}

/* Restore function stack, frame, and registers.  */

void
ix86_expand_epilogue (style)
     int style;
{
  int regno;
  int sp_valid = !frame_pointer_needed || current_function_sp_is_unchanging;
  struct ix86_frame frame;
  HOST_WIDE_INT offset;

  ix86_compute_frame_layout (&frame);

  /* Calculate start of saved registers relative to ebp.  Special care
     must be taken for the normal return case of a function using
     eh_return: the eax and edx registers are marked as saved, but not
     restored along this path.  */
  offset = frame.nregs;
  if (current_function_calls_eh_return && style != 2)
    offset -= 2;
  offset *= -UNITS_PER_WORD;

  /* If we're only restoring one register and sp is not valid then
     using a move instruction to restore the register since it's
     less work than reloading sp and popping the register.

     The default code result in stack adjustment using add/lea instruction,
     while this code results in LEAVE instruction (or discrete equivalent),
     so it is profitable in some other cases as well.  Especially when there
     are no registers to restore.  We also use this code when TARGET_USE_LEAVE
     and there is exactly one register to pop. This heruistic may need some
     tuning in future.  */
  if ((!sp_valid && frame.nregs <= 1)
      || (TARGET_EPILOGUE_USING_MOVE
	  && use_fast_prologue_epilogue
	  && (frame.nregs > 1 || frame.to_allocate))
      || (frame_pointer_needed && !frame.nregs && frame.to_allocate)
      || (frame_pointer_needed && TARGET_USE_LEAVE
	  && use_fast_prologue_epilogue && frame.nregs == 1)
      || current_function_calls_eh_return)
    {
      /* Restore registers.  We can use ebp or esp to address the memory
	 locations.  If both are available, default to ebp, since offsets
	 are known to be small.  Only exception is esp pointing directly to the
	 end of block of saved registers, where we may simplify addressing
	 mode.  */

      if (!frame_pointer_needed || (sp_valid && !frame.to_allocate))
	ix86_emit_restore_regs_using_mov (stack_pointer_rtx,
					  frame.to_allocate, style == 2);
      else
	ix86_emit_restore_regs_using_mov (hard_frame_pointer_rtx,
					  offset, style == 2);

      /* eh_return epilogues need %ecx added to the stack pointer.  */
      if (style == 2)
	{
	  rtx tmp, sa = EH_RETURN_STACKADJ_RTX;

	  if (frame_pointer_needed)
	    {
	      tmp = gen_rtx_PLUS (Pmode, hard_frame_pointer_rtx, sa);
	      tmp = plus_constant (tmp, UNITS_PER_WORD);
	      emit_insn (gen_rtx_SET (VOIDmode, sa, tmp));

	      tmp = gen_rtx_MEM (Pmode, hard_frame_pointer_rtx);
	      emit_move_insn (hard_frame_pointer_rtx, tmp);

	      emit_insn (gen_pro_epilogue_adjust_stack
			 (stack_pointer_rtx, sa, const0_rtx));
	    }
	  else
	    {
	      tmp = gen_rtx_PLUS (Pmode, stack_pointer_rtx, sa);
	      tmp = plus_constant (tmp, (frame.to_allocate
                                         + frame.nregs * UNITS_PER_WORD));
	      emit_insn (gen_rtx_SET (VOIDmode, stack_pointer_rtx, tmp));
	    }
	}
      else if (!frame_pointer_needed)
	emit_insn (gen_pro_epilogue_adjust_stack
		   (stack_pointer_rtx, stack_pointer_rtx,
		    GEN_INT (frame.to_allocate
			     + frame.nregs * UNITS_PER_WORD)));
      /* If not an i386, mov & pop is faster than "leave".  */
      else if (TARGET_USE_LEAVE || optimize_size || !use_fast_prologue_epilogue)
	emit_insn (TARGET_64BIT ? gen_leave_rex64 () : gen_leave ());
      else
	{
	  emit_insn (gen_pro_epilogue_adjust_stack (stack_pointer_rtx,
						    hard_frame_pointer_rtx,
						    const0_rtx));
	  if (TARGET_64BIT)
	    emit_insn (gen_popdi1 (hard_frame_pointer_rtx));
	  else
	    emit_insn (gen_popsi1 (hard_frame_pointer_rtx));
	}
    }
  else
    {
      /* First step is to deallocate the stack frame so that we can
	 pop the registers.  */
      if (!sp_valid)
	{
	  if (!frame_pointer_needed)
	    abort ();
          emit_insn (gen_pro_epilogue_adjust_stack (stack_pointer_rtx,
						    hard_frame_pointer_rtx,
						    GEN_INT (offset)));
	}
      else if (frame.to_allocate)
	emit_insn (gen_pro_epilogue_adjust_stack
		   (stack_pointer_rtx, stack_pointer_rtx,
		    GEN_INT (frame.to_allocate)));

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (ix86_save_reg (regno, false))
	  {
	    if (TARGET_64BIT)
	      emit_insn (gen_popdi1 (gen_rtx_REG (Pmode, regno)));
	    else
	      emit_insn (gen_popsi1 (gen_rtx_REG (Pmode, regno)));
	  }
      if (frame_pointer_needed)
	{
	  /* Leave results in shorter dependency chains on CPUs that are
	     able to grok it fast.  */
	  if (TARGET_USE_LEAVE)
	    emit_insn (TARGET_64BIT ? gen_leave_rex64 () : gen_leave ());
	  else if (TARGET_64BIT)
	    emit_insn (gen_popdi1 (hard_frame_pointer_rtx));
	  else
	    emit_insn (gen_popsi1 (hard_frame_pointer_rtx));
	}
    }

  /* Sibcall epilogues don't want a return instruction.  */
  if (style == 0)
    return;

  if (current_function_pops_args && current_function_args_size)
    {
      rtx popc = GEN_INT (current_function_pops_args);

      /* i386 can only pop 64K bytes.  If asked to pop more, pop
	 return address, do explicit add, and jump indirectly to the
	 caller.  */

      if (current_function_pops_args >= 65536)
	{
	  rtx ecx = gen_rtx_REG (SImode, 2);

	  /* There are is no "pascal" calling convention in 64bit ABI.  */
	  if (TARGET_64BIT)
	    abort ();

	  emit_insn (gen_popsi1 (ecx));
	  emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, popc));
	  emit_jump_insn (gen_return_indirect_internal (ecx));
	}
      else
	emit_jump_insn (gen_return_pop_internal (popc));
    }
  else
    emit_jump_insn (gen_return_internal ());
}

/* Reset from the function's potential modifications.  */

static void
ix86_output_function_epilogue (file, size)
     FILE *file ATTRIBUTE_UNUSED;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  if (pic_offset_table_rtx)
    REGNO (pic_offset_table_rtx) = REAL_PIC_OFFSET_TABLE_REGNUM;
}

/* Extract the parts of an RTL expression that is a valid memory address
   for an instruction.  Return 0 if the structure of the address is
   grossly off.  Return -1 if the address contains ASHIFT, so it is not
   strictly valid, but still used for computing length of lea instruction.
   */

static int
ix86_decompose_address (addr, out)
     register rtx addr;
     struct ix86_address *out;
{
  rtx base = NULL_RTX;
  rtx index = NULL_RTX;
  rtx disp = NULL_RTX;
  HOST_WIDE_INT scale = 1;
  rtx scale_rtx = NULL_RTX;
  int retval = 1;

  if (REG_P (addr) || GET_CODE (addr) == SUBREG)
    base = addr;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      enum rtx_code code0 = GET_CODE (op0);
      enum rtx_code code1 = GET_CODE (op1);

      if (code0 == REG || code0 == SUBREG)
	{
	  if (code1 == REG || code1 == SUBREG)
	    index = op0, base = op1;	/* index + base */
	  else
	    base = op0, disp = op1;	/* base + displacement */
	}
      else if (code0 == MULT)
	{
	  index = XEXP (op0, 0);
	  scale_rtx = XEXP (op0, 1);
	  if (code1 == REG || code1 == SUBREG)
	    base = op1;			/* index*scale + base */
	  else
	    disp = op1;			/* index*scale + disp */
	}
      else if (code0 == PLUS && GET_CODE (XEXP (op0, 0)) == MULT)
	{
	  index = XEXP (XEXP (op0, 0), 0);	/* index*scale + base + disp */
	  scale_rtx = XEXP (XEXP (op0, 0), 1);
	  base = XEXP (op0, 1);
	  disp = op1;
	}
      else if (code0 == PLUS)
	{
	  index = XEXP (op0, 0);	/* index + base + disp */
	  base = XEXP (op0, 1);
	  disp = op1;
	}
      else
	return 0;
    }
  else if (GET_CODE (addr) == MULT)
    {
      index = XEXP (addr, 0);		/* index*scale */
      scale_rtx = XEXP (addr, 1);
    }
  else if (GET_CODE (addr) == ASHIFT)
    {
      rtx tmp;

      /* We're called for lea too, which implements ashift on occasion.  */
      index = XEXP (addr, 0);
      tmp = XEXP (addr, 1);
      if (GET_CODE (tmp) != CONST_INT)
	return 0;
      scale = INTVAL (tmp);
      if ((unsigned HOST_WIDE_INT) scale > 3)
	return 0;
      scale = 1 << scale;
      retval = -1;
    }
  else
    disp = addr;			/* displacement */

  /* Extract the integral value of scale.  */
  if (scale_rtx)
    {
      if (GET_CODE (scale_rtx) != CONST_INT)
	return 0;
      scale = INTVAL (scale_rtx);
    }

  /* Allow arg pointer and stack pointer as index if there is not scaling */
  if (base && index && scale == 1
      && (index == arg_pointer_rtx || index == frame_pointer_rtx
          || index == stack_pointer_rtx))
    {
      rtx tmp = base;
      base = index;
      index = tmp;
    }

  /* Special case: %ebp cannot be encoded as a base without a displacement.  */
  if ((base == hard_frame_pointer_rtx
       || base == frame_pointer_rtx
       || base == arg_pointer_rtx) && !disp)
    disp = const0_rtx;

  /* Special case: on K6, [%esi] makes the instruction vector decoded.
     Avoid this by transforming to [%esi+0].  */
  if (ix86_cpu == PROCESSOR_K6 && !optimize_size
      && base && !index && !disp
      && REG_P (base)
      && REGNO_REG_CLASS (REGNO (base)) == SIREG)
    disp = const0_rtx;

  /* Special case: encode reg+reg instead of reg*2.  */
  if (!base && index && scale && scale == 2)
    base = index, scale = 1;

  /* Special case: scaling cannot be encoded without base or displacement.  */
  if (!base && !disp && index && scale != 1)
    disp = const0_rtx;

  out->base = base;
  out->index = index;
  out->disp = disp;
  out->scale = scale;

  return retval;
}

/* Return cost of the memory address x.
   For i386, it is better to use a complex address than let gcc copy
   the address into a reg and make a new pseudo.  But not if the address
   requires to two regs - that would mean more pseudos with longer
   lifetimes.  */
int
ix86_address_cost (x)
     rtx x;
{
  struct ix86_address parts;
  int cost = 1;

  if (!ix86_decompose_address (x, &parts))
    abort ();

  if (parts.base && GET_CODE (parts.base) == SUBREG)
    parts.base = SUBREG_REG (parts.base);
  if (parts.index && GET_CODE (parts.index) == SUBREG)
    parts.index = SUBREG_REG (parts.index);

  /* More complex memory references are better.  */
  if (parts.disp && parts.disp != const0_rtx)
    cost--;

  /* Attempt to minimize number of registers in the address.  */
  if ((parts.base
       && (!REG_P (parts.base) || REGNO (parts.base) >= FIRST_PSEUDO_REGISTER))
      || (parts.index
	  && (!REG_P (parts.index)
	      || REGNO (parts.index) >= FIRST_PSEUDO_REGISTER)))
    cost++;

  if (parts.base
      && (!REG_P (parts.base) || REGNO (parts.base) >= FIRST_PSEUDO_REGISTER)
      && parts.index
      && (!REG_P (parts.index) || REGNO (parts.index) >= FIRST_PSEUDO_REGISTER)
      && parts.base != parts.index)
    cost++;

  /* AMD-K6 don't like addresses with ModR/M set to 00_xxx_100b,
     since it's predecode logic can't detect the length of instructions
     and it degenerates to vector decoded.  Increase cost of such
     addresses here.  The penalty is minimally 2 cycles.  It may be worthwhile
     to split such addresses or even refuse such addresses at all.

     Following addressing modes are affected:
      [base+scale*index]
      [scale*index+disp]
      [base+index]

     The first and last case  may be avoidable by explicitly coding the zero in
     memory address, but I don't have AMD-K6 machine handy to check this
     theory.  */

  if (TARGET_K6
      && ((!parts.disp && parts.base && parts.index && parts.scale != 1)
	  || (parts.disp && !parts.base && parts.index && parts.scale != 1)
	  || (!parts.disp && parts.base && parts.index && parts.scale == 1)))
    cost += 10;

  return cost;
}

/* If X is a machine specific address (i.e. a symbol or label being
   referenced as a displacement from the GOT implemented using an
   UNSPEC), then return the base term.  Otherwise return X.  */

rtx
ix86_find_base_term (x)
     rtx x;
{
  rtx term;

  if (TARGET_64BIT)
    {
      if (GET_CODE (x) != CONST)
	return x;
      term = XEXP (x, 0);
      if (GET_CODE (term) == PLUS
	  && (GET_CODE (XEXP (term, 1)) == CONST_INT
	      || GET_CODE (XEXP (term, 1)) == CONST_DOUBLE))
	term = XEXP (term, 0);
      if (GET_CODE (term) != UNSPEC
	  || XINT (term, 1) != UNSPEC_GOTPCREL)
	return x;

      term = XVECEXP (term, 0, 0);

      if (GET_CODE (term) != SYMBOL_REF
	  && GET_CODE (term) != LABEL_REF)
	return x;

      return term;
    }

  if (GET_CODE (x) != PLUS
      || XEXP (x, 0) != pic_offset_table_rtx
      || GET_CODE (XEXP (x, 1)) != CONST)
    return x;

  term = XEXP (XEXP (x, 1), 0);

  if (GET_CODE (term) == PLUS && GET_CODE (XEXP (term, 1)) == CONST_INT)
    term = XEXP (term, 0);

  if (GET_CODE (term) != UNSPEC
      || XINT (term, 1) != UNSPEC_GOTOFF)
    return x;

  term = XVECEXP (term, 0, 0);

  if (GET_CODE (term) != SYMBOL_REF
      && GET_CODE (term) != LABEL_REF)
    return x;

  return term;
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */

bool
legitimate_constant_p (x)
     rtx x;
{
  rtx inner;

  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      /* TLS symbols are not constant.  */
      if (tls_symbolic_operand (x, Pmode))
	return false;
      break;

    case CONST:
      inner = XEXP (x, 0);

      /* Offsets of TLS symbols are never valid.
	 Discourage CSE from creating them.  */
      if (GET_CODE (inner) == PLUS
	  && tls_symbolic_operand (XEXP (inner, 0), Pmode))
	return false;

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (inner) == UNSPEC)
	switch (XINT (inner, 1))
	  {
	  case UNSPEC_TPOFF:
	    return local_exec_symbolic_operand (XVECEXP (inner, 0, 0), Pmode);
	  default:
	    return false;
	  }
      break;

    default:
      break;
    }

  /* Otherwise we handle everything else in the move patterns.  */
  return true;
}

/* Determine if it's legal to put X into the constant pool.  This
   is not possible for the address of thread-local symbols, which
   is checked above.  */

static bool
ix86_cannot_force_const_mem (x)
     rtx x;
{
  return !legitimate_constant_p (x);
}

/* Determine if a given RTX is a valid constant address.  */

bool
constant_address_p (x)
     rtx x;
{
  switch (GET_CODE (x))
    {
    case LABEL_REF:
    case CONST_INT:
      return true;

    case CONST_DOUBLE:
      return TARGET_64BIT;

    case CONST:
      /* For Mach-O, really believe the CONST.  */
      if (TARGET_MACHO)
	return true;
      /* Otherwise fall through.  */
    case SYMBOL_REF:
      return !flag_pic && legitimate_constant_p (x);

    default:
      return false;
    }
}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

bool
legitimate_pic_operand_p (x)
     rtx x;
{
  rtx inner;

  switch (GET_CODE (x))
    {
    case CONST:
      inner = XEXP (x, 0);

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (inner) == UNSPEC)
	switch (XINT (inner, 1))
	  {
	  case UNSPEC_TPOFF:
	    return local_exec_symbolic_operand (XVECEXP (inner, 0, 0), Pmode);
	  default:
	    return false;
	  }
      /* FALLTHRU */

    case SYMBOL_REF:
    case LABEL_REF:
      return legitimate_pic_address_disp_p (x);

    default:
      return true;
    }
}

/* Determine if a given CONST RTX is a valid memory displacement
   in PIC mode.  */

int
legitimate_pic_address_disp_p (disp)
     register rtx disp;
{
  bool saw_plus;

  /* In 64bit mode we can allow direct addresses of symbols and labels
     when they are not dynamic symbols.  */
  if (TARGET_64BIT)
    {
      /* TLS references should always be enclosed in UNSPEC.  */
      if (tls_symbolic_operand (disp, GET_MODE (disp)))
	return 0;
      if (GET_CODE (disp) == SYMBOL_REF
	  && ix86_cmodel == CM_SMALL_PIC
	  && (CONSTANT_POOL_ADDRESS_P (disp)
	      || SYMBOL_REF_FLAG (disp)))
	return 1;
      if (GET_CODE (disp) == LABEL_REF)
	return 1;
      if (GET_CODE (disp) == CONST
	  && GET_CODE (XEXP (disp, 0)) == PLUS
	  && ((GET_CODE (XEXP (XEXP (disp, 0), 0)) == SYMBOL_REF
	       && ix86_cmodel == CM_SMALL_PIC
	       && (CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (disp, 0), 0))
		   || SYMBOL_REF_FLAG (XEXP (XEXP (disp, 0), 0))))
	      || GET_CODE (XEXP (XEXP (disp, 0), 0)) == LABEL_REF)
	  && GET_CODE (XEXP (XEXP (disp, 0), 1)) == CONST_INT
	  && INTVAL (XEXP (XEXP (disp, 0), 1)) < 16*1024*1024
	  && INTVAL (XEXP (XEXP (disp, 0), 1)) >= -16*1024*1024)
	return 1;
    }
  if (GET_CODE (disp) != CONST)
    return 0;
  disp = XEXP (disp, 0);

  if (TARGET_64BIT)
    {
      /* We are unsafe to allow PLUS expressions.  This limit allowed distance
         of GOT tables.  We should not need these anyway.  */
      if (GET_CODE (disp) != UNSPEC
	  || XINT (disp, 1) != UNSPEC_GOTPCREL)
	return 0;

      if (GET_CODE (XVECEXP (disp, 0, 0)) != SYMBOL_REF
	  && GET_CODE (XVECEXP (disp, 0, 0)) != LABEL_REF)
	return 0;
      return 1;
    }

  saw_plus = false;
  if (GET_CODE (disp) == PLUS)
    {
      if (GET_CODE (XEXP (disp, 1)) != CONST_INT)
	return 0;
      disp = XEXP (disp, 0);
      saw_plus = true;
    }

  /* Allow {LABEL | SYMBOL}_REF - SYMBOL_REF-FOR-PICBASE for Mach-O.  */
  if (TARGET_MACHO && GET_CODE (disp) == MINUS)
    {
      if (GET_CODE (XEXP (disp, 0)) == LABEL_REF
          || GET_CODE (XEXP (disp, 0)) == SYMBOL_REF)
        if (GET_CODE (XEXP (disp, 1)) == SYMBOL_REF)
          {
            const char *sym_name = XSTR (XEXP (disp, 1), 0);
            if (strstr (sym_name, "$pb") != 0)
              return 1;
          }
    }

  if (GET_CODE (disp) != UNSPEC)
    return 0;

  switch (XINT (disp, 1))
    {
    case UNSPEC_GOT:
      if (saw_plus)
	return false;
      return GET_CODE (XVECEXP (disp, 0, 0)) == SYMBOL_REF;
    case UNSPEC_GOTOFF:
      return local_symbolic_operand (XVECEXP (disp, 0, 0), Pmode);
    case UNSPEC_GOTTPOFF:
    case UNSPEC_GOTNTPOFF:
    case UNSPEC_INDNTPOFF:
      if (saw_plus)
	return false;
      return initial_exec_symbolic_operand (XVECEXP (disp, 0, 0), Pmode);
    case UNSPEC_NTPOFF:
      return local_exec_symbolic_operand (XVECEXP (disp, 0, 0), Pmode);
    case UNSPEC_DTPOFF:
      return local_dynamic_symbolic_operand (XVECEXP (disp, 0, 0), Pmode);
    }

  return 0;
}

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression that is a valid
   memory address for an instruction.  The MODE argument is the machine mode
   for the MEM expression that wants to use this address.

   It only recognizes address in canonical form.  LEGITIMIZE_ADDRESS should
   convert common non-canonical forms to canonical form so that they will
   be recognized.  */

int
legitimate_address_p (mode, addr, strict)
     enum machine_mode mode;
     register rtx addr;
     int strict;
{
  struct ix86_address parts;
  rtx base, index, disp;
  HOST_WIDE_INT scale;
  const char *reason = NULL;
  rtx reason_rtx = NULL_RTX;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\n======\nGO_IF_LEGITIMATE_ADDRESS, mode = %s, strict = %d\n",
	       GET_MODE_NAME (mode), strict);
      debug_rtx (addr);
    }

  if (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_TP)
    {
      if (TARGET_DEBUG_ADDR)
	fprintf (stderr, "Success.\n");
      return TRUE;
    }

  if (ix86_decompose_address (addr, &parts) <= 0)
    {
      reason = "decomposition failed";
      goto report_error;
    }

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;

  /* Validate base register.

     Don't allow SUBREG's here, it can lead to spill failures when the base
     is one word out of a two word structure, which is represented internally
     as a DImode int.  */

  if (base)
    {
      rtx reg;
      reason_rtx = base;

      if (GET_CODE (base) == SUBREG)
	reg = SUBREG_REG (base);
      else
	reg = base;

      if (GET_CODE (reg) != REG)
	{
	  reason = "base is not a register";
	  goto report_error;
	}

      if (GET_MODE (base) != Pmode)
	{
	  reason = "base is not in Pmode";
	  goto report_error;
	}

      if ((strict && ! REG_OK_FOR_BASE_STRICT_P (reg))
	  || (! strict && ! REG_OK_FOR_BASE_NONSTRICT_P (reg)))
	{
	  reason = "base is not valid";
	  goto report_error;
	}
    }

  /* Validate index register.

     Don't allow SUBREG's here, it can lead to spill failures when the index
     is one word out of a two word structure, which is represented internally
     as a DImode int.  */

  if (index)
    {
      rtx reg;
      reason_rtx = index;

      if (GET_CODE (index) == SUBREG)
	reg = SUBREG_REG (index);
      else
	reg = index;

      if (GET_CODE (reg) != REG)
	{
	  reason = "index is not a register";
	  goto report_error;
	}

      if (GET_MODE (index) != Pmode)
	{
	  reason = "index is not in Pmode";
	  goto report_error;
	}

      if ((strict && ! REG_OK_FOR_INDEX_STRICT_P (reg))
	  || (! strict && ! REG_OK_FOR_INDEX_NONSTRICT_P (reg)))
	{
	  reason = "index is not valid";
	  goto report_error;
	}
    }

  /* Validate scale factor.  */
  if (scale != 1)
    {
      reason_rtx = GEN_INT (scale);
      if (!index)
	{
	  reason = "scale without index";
	  goto report_error;
	}

      if (scale != 2 && scale != 4 && scale != 8)
	{
	  reason = "scale is not a valid multiplier";
	  goto report_error;
	}
    }

  /* Validate displacement.  */
  if (disp)
    {
      reason_rtx = disp;

      if (GET_CODE (disp) == CONST
	  && GET_CODE (XEXP (disp, 0)) == UNSPEC)
	switch (XINT (XEXP (disp, 0), 1))
	  {
	  case UNSPEC_GOT:
	  case UNSPEC_GOTOFF:
	  case UNSPEC_GOTPCREL:
	    if (!flag_pic)
	      abort ();
	    goto is_legitimate_pic;

	  case UNSPEC_GOTTPOFF:
	  case UNSPEC_GOTNTPOFF:
	  case UNSPEC_INDNTPOFF:
	  case UNSPEC_NTPOFF:
	  case UNSPEC_DTPOFF:
	    break;

	  default:
	    reason = "invalid address unspec";
	    goto report_error;
	  }

      else if (flag_pic && (SYMBOLIC_CONST (disp)
#if TARGET_MACHO
			    && !machopic_operand_p (disp)
#endif
			    ))
	{
	is_legitimate_pic:
	  if (TARGET_64BIT && (index || base))
	    {
	      /* foo@dtpoff(%rX) is ok.  */
	      if (GET_CODE (disp) != CONST
		  || GET_CODE (XEXP (disp, 0)) != PLUS
		  || GET_CODE (XEXP (XEXP (disp, 0), 0)) != UNSPEC
		  || GET_CODE (XEXP (XEXP (disp, 0), 1)) != CONST_INT
		  || (XINT (XEXP (XEXP (disp, 0), 0), 1) != UNSPEC_DTPOFF
		      && XINT (XEXP (XEXP (disp, 0), 0), 1) != UNSPEC_NTPOFF))
		{
		  reason = "non-constant pic memory reference";
		  goto report_error;
		}
	    }
	  else if (! legitimate_pic_address_disp_p (disp))
	    {
	      reason = "displacement is an invalid pic construct";
	      goto report_error;
	    }

          /* This code used to verify that a symbolic pic displacement
	     includes the pic_offset_table_rtx register.

	     While this is good idea, unfortunately these constructs may
	     be created by "adds using lea" optimization for incorrect
	     code like:

	     int a;
	     int foo(int i)
	       {
	         return *(&a+i);
	       }

	     This code is nonsensical, but results in addressing
	     GOT table with pic_offset_table_rtx base.  We can't
	     just refuse it easily, since it gets matched by
	     "addsi3" pattern, that later gets split to lea in the
	     case output register differs from input.  While this
	     can be handled by separate addsi pattern for this case
	     that never results in lea, this seems to be easier and
	     correct fix for crash to disable this test.  */
	}
      else if (!CONSTANT_ADDRESS_P (disp))
	{
	  reason = "displacement is not constant";
	  goto report_error;
	}
      else if (TARGET_64BIT && !x86_64_sign_extended_value (disp))
	{
	  reason = "displacement is out of range";
	  goto report_error;
	}
      else if (!TARGET_64BIT && GET_CODE (disp) == CONST_DOUBLE)
	{
	  reason = "displacement is a const_double";
	  goto report_error;
	}
    }

  /* Everything looks valid.  */
  if (TARGET_DEBUG_ADDR)
    fprintf (stderr, "Success.\n");
  return TRUE;

 report_error:
  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "Error: %s\n", reason);
      debug_rtx (reason_rtx);
    }
  return FALSE;
}

/* Return an unique alias set for the GOT.  */

static HOST_WIDE_INT
ix86_GOT_alias_set ()
{
  static HOST_WIDE_INT set = -1;
  if (set == -1)
    set = new_alias_set ();
  return set;
}

/* Return a legitimate reference for ORIG (an address) using the
   register REG.  If REG is 0, a new pseudo is generated.

   There are two types of references that must be handled:

   1. Global data references must load the address from the GOT, via
      the PIC reg.  An insn is emitted to do this load, and the reg is
      returned.

   2. Static data references, constant pool addresses, and code labels
      compute the address as an offset from the GOT, whose base is in
      the PIC reg.  Static data objects have SYMBOL_REF_FLAG set to
      differentiate them from global data objects.  The returned
      address is the PIC reg + an unspec constant.

   GO_IF_LEGITIMATE_ADDRESS rejects symbolic references unless the PIC
   reg also appears in the address.  */

rtx
legitimize_pic_address (orig, reg)
     rtx orig;
     rtx reg;
{
  rtx addr = orig;
  rtx new = orig;
  rtx base;

#if TARGET_MACHO
  if (reg == 0)
    reg = gen_reg_rtx (Pmode);
  /* Use the generic Mach-O PIC machinery.  */
  return machopic_legitimize_pic_address (orig, GET_MODE (orig), reg);
#endif

  if (TARGET_64BIT && legitimate_pic_address_disp_p (addr))
    new = addr;
  else if (!TARGET_64BIT && local_symbolic_operand (addr, Pmode))
    {
      /* This symbol may be referenced via a displacement from the PIC
	 base address (@GOTOFF).  */

      if (reload_in_progress)
	regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;
      new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTOFF);
      new = gen_rtx_CONST (Pmode, new);
      new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);

      if (reg != 0)
	{
	  emit_move_insn (reg, new);
	  new = reg;
	}
    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    {
      if (TARGET_64BIT)
	{
	  new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTPCREL);
	  new = gen_rtx_CONST (Pmode, new);
	  new = gen_rtx_MEM (Pmode, new);
	  RTX_UNCHANGING_P (new) = 1;
	  set_mem_alias_set (new, ix86_GOT_alias_set ());

	  if (reg == 0)
	    reg = gen_reg_rtx (Pmode);
	  /* Use directly gen_movsi, otherwise the address is loaded
	     into register for CSE.  We don't want to CSE this addresses,
	     instead we CSE addresses from the GOT table, so skip this.  */
	  emit_insn (gen_movsi (reg, new));
	  new = reg;
	}
      else
	{
	  /* This symbol must be referenced via a load from the
	     Global Offset Table (@GOT).  */

	  if (reload_in_progress)
	    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;
	  new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOT);
	  new = gen_rtx_CONST (Pmode, new);
	  new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);
	  new = gen_rtx_MEM (Pmode, new);
	  RTX_UNCHANGING_P (new) = 1;
	  set_mem_alias_set (new, ix86_GOT_alias_set ());

	  if (reg == 0)
	    reg = gen_reg_rtx (Pmode);
	  emit_move_insn (reg, new);
	  new = reg;
	}
    }
  else
    {
      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);

	  /* We must match stuff we generate before.  Assume the only
	     unspecs that can get here are ours.  Not that we could do
	     anything with them anyway...  */
	  if (GET_CODE (addr) == UNSPEC
	      || (GET_CODE (addr) == PLUS
		  && GET_CODE (XEXP (addr, 0)) == UNSPEC))
	    return orig;
	  if (GET_CODE (addr) != PLUS)
	    abort ();
	}
      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  /* Check first to see if this is a constant offset from a @GOTOFF
	     symbol reference.  */
	  if (local_symbolic_operand (op0, Pmode)
	      && GET_CODE (op1) == CONST_INT)
	    {
	      if (!TARGET_64BIT)
		{
		  if (reload_in_progress)
		    regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;
		  new = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0),
					UNSPEC_GOTOFF);
		  new = gen_rtx_PLUS (Pmode, new, op1);
		  new = gen_rtx_CONST (Pmode, new);
		  new = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new);

		  if (reg != 0)
		    {
		      emit_move_insn (reg, new);
		      new = reg;
		    }
		}
	      else
		{
		  if (INTVAL (op1) < -16*1024*1024
		      || INTVAL (op1) >= 16*1024*1024)
		    new = gen_rtx_PLUS (Pmode, op0, force_reg (Pmode, op1));
		}
	    }
	  else
	    {
	      base = legitimize_pic_address (XEXP (addr, 0), reg);
	      new  = legitimize_pic_address (XEXP (addr, 1),
					     base == reg ? NULL_RTX : reg);

	      if (GET_CODE (new) == CONST_INT)
		new = plus_constant (base, INTVAL (new));
	      else
		{
		  if (GET_CODE (new) == PLUS && CONSTANT_P (XEXP (new, 1)))
		    {
		      base = gen_rtx_PLUS (Pmode, base, XEXP (new, 0));
		      new = XEXP (new, 1);
		    }
		  new = gen_rtx_PLUS (Pmode, base, new);
		}
	    }
	}
    }
  return new;
}

static void
ix86_encode_section_info (decl, first)
     tree decl;
     int first ATTRIBUTE_UNUSED;
{
  bool local_p = (*targetm.binds_local_p) (decl);
  rtx rtl, symbol;

  rtl = DECL_P (decl) ? DECL_RTL (decl) : TREE_CST_RTL (decl);
  if (GET_CODE (rtl) != MEM)
    return;
  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  /* For basic x86, if using PIC, mark a SYMBOL_REF for a non-global
     symbol so that we may access it directly in the GOT.  */

  if (flag_pic)
    SYMBOL_REF_FLAG (symbol) = local_p;

  /* For ELF, encode thread-local data with %[GLil] for "global dynamic",
     "local dynamic", "initial exec" or "local exec" TLS models
     respectively.  */

  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL (decl))
    {
      const char *symbol_str;
      char *newstr;
      size_t len;
      enum tls_model kind = decl_tls_model (decl);

      if (TARGET_64BIT && ! flag_pic)
	{
	  /* x86-64 doesn't allow non-pic code for shared libraries,
	     so don't generate GD/LD TLS models for non-pic code.  */
	  switch (kind)
	    {
	    case TLS_MODEL_GLOBAL_DYNAMIC:
	      kind = TLS_MODEL_INITIAL_EXEC; break;
	    case TLS_MODEL_LOCAL_DYNAMIC:
	      kind = TLS_MODEL_LOCAL_EXEC; break;
	    default:
	      break;
	    }
	}

      symbol_str = XSTR (symbol, 0);

      if (symbol_str[0] == '%')
	{
	  if (symbol_str[1] == tls_model_chars[kind])
	    return;
	  symbol_str += 2;
	}
      len = strlen (symbol_str) + 1;
      newstr = alloca (len + 2);

      newstr[0] = '%';
      newstr[1] = tls_model_chars[kind];
      memcpy (newstr + 2, symbol_str, len);

      XSTR (symbol, 0) = ggc_alloc_string (newstr, len + 2 - 1);
    }
}

/* Undo the above when printing symbol names.  */

static const char *
ix86_strip_name_encoding (str)
     const char *str;
{
  if (str[0] == '%')
    str += 2;
  if (str [0] == '*')
    str += 1;
  return str;
}

/* Load the thread pointer into a register.  */

static rtx
get_thread_pointer ()
{
  rtx tp;

  tp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TP);
  tp = gen_rtx_MEM (Pmode, tp);
  RTX_UNCHANGING_P (tp) = 1;
  set_mem_alias_set (tp, ix86_GOT_alias_set ());
  tp = force_reg (Pmode, tp);

  return tp;
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 80386, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in a general reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in a general reg.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address in i386.c for details.  */

rtx
legitimize_address (x, oldx, mode)
     register rtx x;
     register rtx oldx ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  int changed = 0;
  unsigned log;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n==========\nLEGITIMIZE_ADDRESS, mode = %s\n",
	       GET_MODE_NAME (mode));
      debug_rtx (x);
    }

  log = tls_symbolic_operand (x, mode);
  if (log)
    {
      rtx dest, base, off, pic;
      int type;

      switch (log)
        {
        case TLS_MODEL_GLOBAL_DYNAMIC:
	  dest = gen_reg_rtx (Pmode);
	  if (TARGET_64BIT)
	    {
	      rtx rax = gen_rtx_REG (Pmode, 0), insns;

	      start_sequence ();
	      emit_call_insn (gen_tls_global_dynamic_64 (rax, x));
	      insns = get_insns ();
	      end_sequence ();

	      emit_libcall_block (insns, dest, rax, x);
	    }
	  else
	    emit_insn (gen_tls_global_dynamic_32 (dest, x));
	  break;

        case TLS_MODEL_LOCAL_DYNAMIC:
	  base = gen_reg_rtx (Pmode);
	  if (TARGET_64BIT)
	    {
	      rtx rax = gen_rtx_REG (Pmode, 0), insns, note;

	      start_sequence ();
	      emit_call_insn (gen_tls_local_dynamic_base_64 (rax));
	      insns = get_insns ();
	      end_sequence ();

	      note = gen_rtx_EXPR_LIST (VOIDmode, const0_rtx, NULL);
	      note = gen_rtx_EXPR_LIST (VOIDmode, ix86_tls_get_addr (), note);
	      emit_libcall_block (insns, base, rax, note);
	    }
	  else
	    emit_insn (gen_tls_local_dynamic_base_32 (base));

	  off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), UNSPEC_DTPOFF);
	  off = gen_rtx_CONST (Pmode, off);

	  return gen_rtx_PLUS (Pmode, base, off);

        case TLS_MODEL_INITIAL_EXEC:
	  if (TARGET_64BIT)
	    {
	      pic = NULL;
	      type = UNSPEC_GOTNTPOFF;
	    }
	  else if (flag_pic)
	    {
	      if (reload_in_progress)
		regs_ever_live[PIC_OFFSET_TABLE_REGNUM] = 1;
	      pic = pic_offset_table_rtx;
	      type = TARGET_GNU_TLS ? UNSPEC_GOTNTPOFF : UNSPEC_GOTTPOFF;
	    }
	  else if (!TARGET_GNU_TLS)
	    {
	      pic = gen_reg_rtx (Pmode);
	      emit_insn (gen_set_got (pic));
	      type = UNSPEC_GOTTPOFF;
	    }
	  else
	    {
	      pic = NULL;
	      type = UNSPEC_INDNTPOFF;
	    }

	  base = get_thread_pointer ();

	  off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), type);
	  off = gen_rtx_CONST (Pmode, off);
	  if (pic)
	    off = gen_rtx_PLUS (Pmode, pic, off);
	  off = gen_rtx_MEM (Pmode, off);
	  RTX_UNCHANGING_P (off) = 1;
	  set_mem_alias_set (off, ix86_GOT_alias_set ());
	  dest = gen_reg_rtx (Pmode);

	  if (TARGET_64BIT || TARGET_GNU_TLS)
	    {
	      emit_move_insn (dest, off);
	      return gen_rtx_PLUS (Pmode, base, dest);
	    }
	  else
	    emit_insn (gen_subsi3 (dest, base, off));
	  break;

        case TLS_MODEL_LOCAL_EXEC:
	  base = get_thread_pointer ();

	  off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x),
				(TARGET_64BIT || TARGET_GNU_TLS)
				? UNSPEC_NTPOFF : UNSPEC_TPOFF);
	  off = gen_rtx_CONST (Pmode, off);

	  if (TARGET_64BIT || TARGET_GNU_TLS)
	    return gen_rtx_PLUS (Pmode, base, off);
	  else
	    {
	      dest = gen_reg_rtx (Pmode);
	      emit_insn (gen_subsi3 (dest, base, off));
	    }
	  break;

	default:
	  abort ();
        }

      return dest;
    }

  if (flag_pic && SYMBOLIC_CONST (x))
    return legitimize_pic_address (x, 0);

  /* Canonicalize shifts by 0, 1, 2, 3 into multiply */
  if (GET_CODE (x) == ASHIFT
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (log = (unsigned) exact_log2 (INTVAL (XEXP (x, 1)))) < 4)
    {
      changed = 1;
      x = gen_rtx_MULT (Pmode, force_reg (Pmode, XEXP (x, 0)),
			GEN_INT (1 << log));
    }

  if (GET_CODE (x) == PLUS)
    {
      /* Canonicalize shifts by 0, 1, 2, 3 into multiply.  */

      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (log = (unsigned) exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)))) < 4)
	{
	  changed = 1;
	  XEXP (x, 0) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 0), 0)),
				      GEN_INT (1 << log));
	}

      if (GET_CODE (XEXP (x, 1)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  && (log = (unsigned) exact_log2 (INTVAL (XEXP (XEXP (x, 1), 1)))) < 4)
	{
	  changed = 1;
	  XEXP (x, 1) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 1), 0)),
				      GEN_INT (1 << log));
	}

      /* Put multiply first if it isn't already.  */
      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  rtx tmp = XEXP (x, 0);
	  XEXP (x, 0) = XEXP (x, 1);
	  XEXP (x, 1) = tmp;
	  changed = 1;
	}

      /* Canonicalize (plus (mult (reg) (const)) (plus (reg) (const)))
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  This can be
	 created by virtual register instantiation, register elimination, and
	 similar optimizations.  */
      if (GET_CODE (XEXP (x, 0)) == MULT && GET_CODE (XEXP (x, 1)) == PLUS)
	{
	  changed = 1;
	  x = gen_rtx_PLUS (Pmode,
			    gen_rtx_PLUS (Pmode, XEXP (x, 0),
					  XEXP (XEXP (x, 1), 0)),
			    XEXP (XEXP (x, 1), 1));
	}

      /* Canonicalize
	 (plus (plus (mult (reg) (const)) (plus (reg) (const))) const)
	 into (plus (plus (mult (reg) (const)) (reg)) (const)).  */
      else if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == PLUS
	       && CONSTANT_P (XEXP (x, 1)))
	{
	  rtx constant;
	  rtx other = NULL_RTX;

	  if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      constant = XEXP (x, 1);
	      other = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    }
	  else if (GET_CODE (XEXP (XEXP (XEXP (x, 0), 1), 1)) == CONST_INT)
	    {
	      constant = XEXP (XEXP (XEXP (x, 0), 1), 1);
	      other = XEXP (x, 1);
	    }
	  else
	    constant = 0;

	  if (constant)
	    {
	      changed = 1;
	      x = gen_rtx_PLUS (Pmode,
				gen_rtx_PLUS (Pmode, XEXP (XEXP (x, 0), 0),
					      XEXP (XEXP (XEXP (x, 0), 1), 0)),
				plus_constant (other, INTVAL (constant)));
	    }
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (GET_CODE (XEXP (x, 0)) == MULT)
	{
	  changed = 1;
	  XEXP (x, 0) = force_operand (XEXP (x, 0), 0);
	}

      if (GET_CODE (XEXP (x, 1)) == MULT)
	{
	  changed = 1;
	  XEXP (x, 1) = force_operand (XEXP (x, 1), 0);
	}

      if (changed
	  && GET_CODE (XEXP (x, 1)) == REG
	  && GET_CODE (XEXP (x, 0)) == REG)
	return x;

      if (flag_pic && SYMBOLIC_CONST (XEXP (x, 1)))
	{
	  changed = 1;
	  x = legitimize_pic_address (x, 0);
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 1) = temp;
	  return x;
	}

      else if (GET_CODE (XEXP (x, 1)) == REG)
	{
	  register rtx temp = gen_reg_rtx (Pmode);
	  register rtx val  = force_operand (XEXP (x, 0), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 0) = temp;
	  return x;
	}
    }

  return x;
}

/* Print an integer constant expression in assembler syntax.  Addition
   and subtraction are the only arithmetic that may appear in these
   expressions.  FILE is the stdio stream to write to, X is the rtx, and
   CODE is the operand print code from the output string.  */

static void
output_pic_addr_const (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  char buf[256];

  switch (GET_CODE (x))
    {
    case PC:
      if (flag_pic)
	putc ('.', file);
      else
	abort ();
      break;

    case SYMBOL_REF:
      assemble_name (file, XSTR (x, 0));
      if (!TARGET_MACHO && code == 'P' && ! SYMBOL_REF_FLAG (x))
	fputs ("@PLT", file);
      break;

    case LABEL_REF:
      x = XEXP (x, 0);
      /* FALLTHRU */
    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (x));
      assemble_name (asm_out_file, buf);
      break;

    case CONST_INT:
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      break;

    case CONST:
      /* This used to output parentheses around the expression,
	 but that does not work on the 386 (either ATT or BSD assembler).  */
      output_pic_addr_const (file, XEXP (x, 0), code);
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  /* We can use %d if the number is <32 bits and positive.  */
	  if (CONST_DOUBLE_HIGH (x) || CONST_DOUBLE_LOW (x) < 0)
	    fprintf (file, "0x%lx%08lx",
		     (unsigned long) CONST_DOUBLE_HIGH (x),
		     (unsigned long) CONST_DOUBLE_LOW (x));
	  else
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, CONST_DOUBLE_LOW (x));
	}
      else
	/* We can't handle floating point constants;
	   PRINT_OPERAND must handle them.  */
	output_operand_lossage ("floating constant misused");
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear first.  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  output_pic_addr_const (file, XEXP (x, 0), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 1), code);
	}
      else if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  output_pic_addr_const (file, XEXP (x, 1), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 0), code);
	}
      else
	abort ();
      break;

    case MINUS:
      if (!TARGET_MACHO)
	putc (ASSEMBLER_DIALECT == ASM_INTEL ? '(' : '[', file);
      output_pic_addr_const (file, XEXP (x, 0), code);
      putc ('-', file);
      output_pic_addr_const (file, XEXP (x, 1), code);
      if (!TARGET_MACHO)
	putc (ASSEMBLER_DIALECT == ASM_INTEL ? ')' : ']', file);
      break;

     case UNSPEC:
       if (XVECLEN (x, 0) != 1)
	 abort ();
       output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
       switch (XINT (x, 1))
	{
	case UNSPEC_GOT:
	  fputs ("@GOT", file);
	  break;
	case UNSPEC_GOTOFF:
	  fputs ("@GOTOFF", file);
	  break;
	case UNSPEC_GOTPCREL:
	  fputs ("@GOTPCREL(%rip)", file);
	  break;
	case UNSPEC_GOTTPOFF:
	  /* FIXME: This might be @TPOFF in Sun ld too.  */
	  fputs ("@GOTTPOFF", file);
	  break;
	case UNSPEC_TPOFF:
	  fputs ("@TPOFF", file);
	  break;
	case UNSPEC_NTPOFF:
	  if (TARGET_64BIT)
	    fputs ("@TPOFF", file);
	  else
	    fputs ("@NTPOFF", file);
	  break;
	case UNSPEC_DTPOFF:
	  fputs ("@DTPOFF", file);
	  break;
	case UNSPEC_GOTNTPOFF:
	  if (TARGET_64BIT)
	    fputs ("@GOTTPOFF(%rip)", file);
	  else
	    fputs ("@GOTNTPOFF", file);
	  break;
	case UNSPEC_INDNTPOFF:
	  fputs ("@INDNTPOFF", file);
	  break;
	default:
	  output_operand_lossage ("invalid UNSPEC as operand");
	  break;
	}
       break;

    default:
      output_operand_lossage ("invalid expression as operand");
    }
}

/* This is called from dwarfout.c via ASM_OUTPUT_DWARF_ADDR_CONST.
   We need to handle our special PIC relocations.  */

void
i386_dwarf_output_addr_const (file, x)
     FILE *file;
     rtx x;
{
#ifdef ASM_QUAD
  fprintf (file, "%s", TARGET_64BIT ? ASM_QUAD : ASM_LONG);
#else
  if (TARGET_64BIT)
    abort ();
  fprintf (file, "%s", ASM_LONG);
#endif
  if (flag_pic)
    output_pic_addr_const (file, x, '\0');
  else
    output_addr_const (file, x);
  fputc ('\n', file);
}

/* This is called from dwarf2out.c via ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

void
i386_output_dwarf_dtprel (file, size, x)
     FILE *file;
     int size;
     rtx x;
{
  fputs (ASM_LONG, file);
  output_addr_const (file, x);
  fputs ("@DTPOFF", file);
  switch (size)
    {
    case 4:
      break;
    case 8:
      fputs (", 0", file);
      break;
    default:
      abort ();
   }
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler losage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.  */

rtx
i386_simplify_dwarf_addr (orig_x)
     rtx orig_x;
{
  rtx x = orig_x, y;

  if (GET_CODE (x) == MEM)
    x = XEXP (x, 0);

  if (TARGET_64BIT)
    {
      if (GET_CODE (x) != CONST
	  || GET_CODE (XEXP (x, 0)) != UNSPEC
	  || XINT (XEXP (x, 0), 1) != UNSPEC_GOTPCREL
	  || GET_CODE (orig_x) != MEM)
	return orig_x;
      return XVECEXP (XEXP (x, 0), 0, 0);
    }

  if (GET_CODE (x) != PLUS
      || GET_CODE (XEXP (x, 1)) != CONST)
    return orig_x;

  if (GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM)
    /* %ebx + GOT/GOTOFF */
    y = NULL;
  else if (GET_CODE (XEXP (x, 0)) == PLUS)
    {
      /* %ebx + %reg * scale + GOT/GOTOFF */
      y = XEXP (x, 0);
      if (GET_CODE (XEXP (y, 0)) == REG
	  && REGNO (XEXP (y, 0)) == PIC_OFFSET_TABLE_REGNUM)
	y = XEXP (y, 1);
      else if (GET_CODE (XEXP (y, 1)) == REG
	       && REGNO (XEXP (y, 1)) == PIC_OFFSET_TABLE_REGNUM)
	y = XEXP (y, 0);
      else
	return orig_x;
      if (GET_CODE (y) != REG
	  && GET_CODE (y) != MULT
	  && GET_CODE (y) != ASHIFT)
	return orig_x;
    }
  else
    return orig_x;

  x = XEXP (XEXP (x, 1), 0);
  if (GET_CODE (x) == UNSPEC
      && ((XINT (x, 1) == UNSPEC_GOT && GET_CODE (orig_x) == MEM)
	  || (XINT (x, 1) == UNSPEC_GOTOFF && GET_CODE (orig_x) != MEM)))
    {
      if (y)
	return gen_rtx_PLUS (Pmode, y, XVECEXP (x, 0, 0));
      return XVECEXP (x, 0, 0);
    }

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == UNSPEC
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && ((XINT (XEXP (x, 0), 1) == UNSPEC_GOT && GET_CODE (orig_x) == MEM)
	  || (XINT (XEXP (x, 0), 1) == UNSPEC_GOTOFF
	      && GET_CODE (orig_x) != MEM)))
    {
      x = gen_rtx_PLUS (VOIDmode, XVECEXP (XEXP (x, 0), 0, 0), XEXP (x, 1));
      if (y)
	return gen_rtx_PLUS (Pmode, y, x);
      return x;
    }

  return orig_x;
}

static void
put_condition_code (code, mode, reverse, fp, file)
     enum rtx_code code;
     enum machine_mode mode;
     int reverse, fp;
     FILE *file;
{
  const char *suffix;

  if (mode == CCFPmode || mode == CCFPUmode)
    {
      enum rtx_code second_code, bypass_code;
      ix86_fp_comparison_codes (code, &bypass_code, &code, &second_code);
      if (bypass_code != NIL || second_code != NIL)
	abort ();
      code = ix86_fp_compare_code_to_integer (code);
      mode = CCmode;
    }
  if (reverse)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:
      suffix = "e";
      break;
    case NE:
      suffix = "ne";
      break;
    case GT:
      if (mode != CCmode && mode != CCNOmode && mode != CCGCmode)
	abort ();
      suffix = "g";
      break;
    case GTU:
      /* ??? Use "nbe" instead of "a" for fcmov losage on some assemblers.
	 Those same assemblers have the same but opposite losage on cmov.  */
      if (mode != CCmode)
	abort ();
      suffix = fp ? "nbe" : "a";
      break;
    case LT:
      if (mode == CCNOmode || mode == CCGOCmode)
	suffix = "s";
      else if (mode == CCmode || mode == CCGCmode)
	suffix = "l";
      else
	abort ();
      break;
    case LTU:
      if (mode != CCmode)
	abort ();
      suffix = "b";
      break;
    case GE:
      if (mode == CCNOmode || mode == CCGOCmode)
	suffix = "ns";
      else if (mode == CCmode || mode == CCGCmode)
	suffix = "ge";
      else
	abort ();
      break;
    case GEU:
      /* ??? As above.  */
      if (mode != CCmode)
	abort ();
      suffix = fp ? "nb" : "ae";
      break;
    case LE:
      if (mode != CCmode && mode != CCGCmode && mode != CCNOmode)
	abort ();
      suffix = "le";
      break;
    case LEU:
      if (mode != CCmode)
	abort ();
      suffix = "be";
      break;
    case UNORDERED:
      suffix = fp ? "u" : "p";
      break;
    case ORDERED:
      suffix = fp ? "nu" : "np";
      break;
    default:
      abort ();
    }
  fputs (suffix, file);
}

void
print_reg (x, code, file)
     rtx x;
     int code;
     FILE *file;
{
  if (REGNO (x) == ARG_POINTER_REGNUM
      || REGNO (x) == FRAME_POINTER_REGNUM
      || REGNO (x) == FLAGS_REG
      || REGNO (x) == FPSR_REG)
    abort ();

  if (ASSEMBLER_DIALECT == ASM_ATT || USER_LABEL_PREFIX[0] == 0)
    putc ('%', file);

  if (code == 'w' || MMX_REG_P (x))
    code = 2;
  else if (code == 'b')
    code = 1;
  else if (code == 'k')
    code = 4;
  else if (code == 'q')
    code = 8;
  else if (code == 'y')
    code = 3;
  else if (code == 'h')
    code = 0;
  else
    code = GET_MODE_SIZE (GET_MODE (x));

  /* Irritatingly, AMD extended registers use different naming convention
     from the normal registers.  */
  if (REX_INT_REG_P (x))
    {
      if (!TARGET_64BIT)
	abort ();
      switch (code)
	{
	  case 0:
	    error ("extended registers have no high halves");
	    break;
	  case 1:
	    fprintf (file, "r%ib", REGNO (x) - FIRST_REX_INT_REG + 8);
	    break;
	  case 2:
	    fprintf (file, "r%iw", REGNO (x) - FIRST_REX_INT_REG + 8);
	    break;
	  case 4:
	    fprintf (file, "r%id", REGNO (x) - FIRST_REX_INT_REG + 8);
	    break;
	  case 8:
	    fprintf (file, "r%i", REGNO (x) - FIRST_REX_INT_REG + 8);
	    break;
	  default:
	    error ("unsupported operand size for extended register");
	    break;
	}
      return;
    }
  switch (code)
    {
    case 3:
      if (STACK_TOP_P (x))
	{
	  fputs ("st(0)", file);
	  break;
	}
      /* FALLTHRU */
    case 8:
    case 4:
    case 12:
      if (! ANY_FP_REG_P (x))
	putc (code == 8 && TARGET_64BIT ? 'r' : 'e', file);
      /* FALLTHRU */
    case 16:
    case 2:
      fputs (hi_reg_name[REGNO (x)], file);
      break;
    case 1:
      fputs (qi_reg_name[REGNO (x)], file);
      break;
    case 0:
      fputs (qi_high_reg_name[REGNO (x)], file);
      break;
    default:
      abort ();
    }
}

/* Locate some local-dynamic symbol still in use by this function
   so that we can print its name in some tls_local_dynamic_base
   pattern.  */

static const char *
get_some_local_dynamic_name ()
{
  rtx insn;

  if (cfun->machine->some_ld_name)
    return cfun->machine->some_ld_name;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& for_each_rtx (&PATTERN (insn), get_some_local_dynamic_name_1, 0))
      return cfun->machine->some_ld_name;

  abort ();
}

static int
get_some_local_dynamic_name_1 (px, data)
     rtx *px;
     void *data ATTRIBUTE_UNUSED;
{
  rtx x = *px;

  if (GET_CODE (x) == SYMBOL_REF
      && local_dynamic_symbolic_operand (x, Pmode))
    {
      cfun->machine->some_ld_name = XSTR (x, 0);
      return 1;
    }

  return 0;
}

/* Meaning of CODE:
   L,W,B,Q,S,T -- print the opcode suffix for specified size of operand.
   C -- print opcode suffix for set/cmov insn.
   c -- like C, but print reversed condition
   F,f -- likewise, but for floating-point.
   O -- if CMOV_SUN_AS_SYNTAX, expand to "w.", "l." or "q.", otherwise
        nothing
   R -- print the prefix for register names.
   z -- print the opcode suffix for the size of the current operand.
   * -- print a star (in certain assembler syntax)
   A -- print an absolute memory reference.
   w -- print the operand as if it's a "word" (HImode) even if it isn't.
   s -- print a shift double count, followed by the assemblers argument
	delimiter.
   b -- print the QImode name of the register for the indicated operand.
	%b0 would print %al if operands[0] is reg 0.
   w --  likewise, print the HImode name of the register.
   k --  likewise, print the SImode name of the register.
   q --  likewise, print the DImode name of the register.
   h -- print the QImode name for a "high" register, either ah, bh, ch or dh.
   y -- print "st(0)" instead of "st" as a register.
   D -- print condition for SSE cmp instruction.
   P -- if PIC, print an @PLT suffix.
   X -- don't print any sort of PIC '@' suffix for a symbol.
   & -- print some in-use local-dynamic symbol name.
 */

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  if (code)
    {
      switch (code)
	{
	case '*':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('*', file);
	  return;

	case '&':
	  assemble_name (file, get_some_local_dynamic_name ());
	  return;

	case 'A':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('*', file);
	  else if (ASSEMBLER_DIALECT == ASM_INTEL)
	    {
	      /* Intel syntax. For absolute addresses, registers should not
		 be surrounded by braces.  */
	      if (GET_CODE (x) != REG)
		{
		  putc ('[', file);
		  PRINT_OPERAND (file, x, 0);
		  putc (']', file);
		  return;
		}
	    }
	  else
	    abort ();

	  PRINT_OPERAND (file, x, 0);
	  return;


	case 'L':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('l', file);
	  return;

	case 'W':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('w', file);
	  return;

	case 'B':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('b', file);
	  return;

	case 'Q':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('l', file);
	  return;

	case 'S':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('s', file);
	  return;

	case 'T':
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('t', file);
	  return;

	case 'z':
	  /* 387 opcodes don't get size suffixes if the operands are
	     registers.  */
	  if (STACK_REG_P (x))
	    return;

	  /* Likewise if using Intel opcodes.  */
	  if (ASSEMBLER_DIALECT == ASM_INTEL)
	    return;

	  /* This is the size of op from size of operand.  */
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 2:
#ifdef HAVE_GAS_FILDS_FISTS
	      putc ('s', file);
#endif
	      return;

	    case 4:
	      if (GET_MODE (x) == SFmode)
		{
		  putc ('s', file);
		  return;
		}
	      else
		putc ('l', file);
	      return;

	    case 12:
	    case 16:
	      putc ('t', file);
	      return;

	    case 8:
	      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
		{
#ifdef GAS_MNEMONICS
		  putc ('q', file);
#else
		  putc ('l', file);
		  putc ('l', file);
#endif
		}
	      else
	        putc ('l', file);
	      return;

	    default:
	      abort ();
	    }

	case 'b':
	case 'w':
	case 'k':
	case 'q':
	case 'h':
	case 'y':
	case 'X':
	case 'P':
	  break;

	case 's':
	  if (GET_CODE (x) == CONST_INT || ! SHIFT_DOUBLE_OMITS_COUNT)
	    {
	      PRINT_OPERAND (file, x, 0);
	      putc (',', file);
	    }
	  return;

	case 'D':
	  /* Little bit of braindamage here.  The SSE compare instructions
	     does use completely different names for the comparisons that the
	     fp conditional moves.  */
	  switch (GET_CODE (x))
	    {
	    case EQ:
	    case UNEQ:
	      fputs ("eq", file);
	      break;
	    case LT:
	    case UNLT:
	      fputs ("lt", file);
	      break;
	    case LE:
	    case UNLE:
	      fputs ("le", file);
	      break;
	    case UNORDERED:
	      fputs ("unord", file);
	      break;
	    case NE:
	    case LTGT:
	      fputs ("neq", file);
	      break;
	    case UNGE:
	    case GE:
	      fputs ("nlt", file);
	      break;
	    case UNGT:
	    case GT:
	      fputs ("nle", file);
	      break;
	    case ORDERED:
	      fputs ("ord", file);
	      break;
	    default:
	      abort ();
	      break;
	    }
	  return;
	case 'O':
#ifdef CMOV_SUN_AS_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    {
	      switch (GET_MODE (x))
		{
		case HImode: putc ('w', file); break;
		case SImode:
		case SFmode: putc ('l', file); break;
		case DImode:
		case DFmode: putc ('q', file); break;
		default: abort ();
		}
	      putc ('.', file);
	    }
#endif
	  return;
	case 'C':
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 0, 0, file);
	  return;
	case 'F':
#ifdef CMOV_SUN_AS_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('.', file);
#endif
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 0, 1, file);
	  return;

	  /* Like above, but reverse condition */
	case 'c':
	  /* Check to see if argument to %c is really a constant
	     and not a condition code which needs to be reversed.  */
	  if (GET_RTX_CLASS (GET_CODE (x)) != '<')
	  {
	    output_operand_lossage ("operand is neither a constant nor a condition code, invalid operand code 'c'");
	     return;
	  }
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 1, 0, file);
	  return;
	case 'f':
#ifdef CMOV_SUN_AS_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('.', file);
#endif
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 1, 1, file);
	  return;
	case '+':
	  {
	    rtx x;

	    if (!optimize || optimize_size || !TARGET_BRANCH_PREDICTION_HINTS)
	      return;

	    x = find_reg_note (current_output_insn, REG_BR_PROB, 0);
	    if (x)
	      {
		int pred_val = INTVAL (XEXP (x, 0));

		if (pred_val < REG_BR_PROB_BASE * 45 / 100
		    || pred_val > REG_BR_PROB_BASE * 55 / 100)
		  {
		    int taken = pred_val > REG_BR_PROB_BASE / 2;
		    int cputaken = final_forward_branch_p (current_output_insn) == 0;

		    /* Emit hints only in the case default branch prediction
		       heruistics would fail.  */
		    if (taken != cputaken)
		      {
			/* We use 3e (DS) prefix for taken branches and
			   2e (CS) prefix for not taken branches.  */
			if (taken)
			  fputs ("ds ; ", file);
			else
			  fputs ("cs ; ", file);
		      }
		  }
	      }
	    return;
	  }
	default:
	    output_operand_lossage ("invalid operand code `%c'", code);
	}
    }

  if (GET_CODE (x) == REG)
    {
      PRINT_REG (x, code, file);
    }

  else if (GET_CODE (x) == MEM)
    {
      /* No `byte ptr' prefix for call instructions.  */
      if (ASSEMBLER_DIALECT == ASM_INTEL && code != 'X' && code != 'P')
	{
	  const char * size;
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 1: size = "BYTE"; break;
	    case 2: size = "WORD"; break;
	    case 4: size = "DWORD"; break;
	    case 8: size = "QWORD"; break;
	    case 12: size = "XWORD"; break;
	    case 16: size = "XMMWORD"; break;
	    default:
	      abort ();
	    }

	  /* Check for explicit size override (codes 'b', 'w' and 'k')  */
	  if (code == 'b')
	    size = "BYTE";
	  else if (code == 'w')
	    size = "WORD";
	  else if (code == 'k')
	    size = "DWORD";

	  fputs (size, file);
	  fputs (" PTR ", file);
	}

      x = XEXP (x, 0);
      if (flag_pic && CONSTANT_ADDRESS_P (x))
	output_pic_addr_const (file, x, code);
      /* Avoid (%rip) for call operands.  */
      else if (CONSTANT_ADDRESS_P (x) && code == 'P'
	       && GET_CODE (x) != CONST_INT)
	output_addr_const (file, x);
      else if (this_is_asm_operands && ! address_operand (x, VOIDmode))
	output_operand_lossage ("invalid constraints for operand");
      else
	output_address (x);
    }

  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      REAL_VALUE_TYPE r;
      long l;

      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
      REAL_VALUE_TO_TARGET_SINGLE (r, l);

      if (ASSEMBLER_DIALECT == ASM_ATT)
	putc ('$', file);
      fprintf (file, "0x%lx", l);
    }

 /* These float cases don't actually occur as immediate operands.  */
 else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
    {
      char dstr[30];

      real_to_decimal (dstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (dstr), 0, 1);
      fprintf (file, "%s", dstr);
    }

  else if (GET_CODE (x) == CONST_DOUBLE
	   && (GET_MODE (x) == XFmode || GET_MODE (x) == TFmode))
    {
      char dstr[30];

      real_to_decimal (dstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (dstr), 0, 1);
      fprintf (file, "%s", dstr);
    }

  else
    {
      if (code != 'P')
	{
	  if (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE)
	    {
	      if (ASSEMBLER_DIALECT == ASM_ATT)
		putc ('$', file);
	    }
	  else if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF
		   || GET_CODE (x) == LABEL_REF)
	    {
	      if (ASSEMBLER_DIALECT == ASM_ATT)
		putc ('$', file);
	      else
		fputs ("OFFSET FLAT:", file);
	    }
	}
      if (GET_CODE (x) == CONST_INT)
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      else if (flag_pic)
	output_pic_addr_const (file, x, code);
      else
	output_addr_const (file, x);
    }
}

/* Print a memory operand whose address is ADDR.  */

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  struct ix86_address parts;
  rtx base, index, disp;
  int scale;

  if (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_TP)
    {
      if (ASSEMBLER_DIALECT == ASM_INTEL)
	fputs ("DWORD PTR ", file);
      if (ASSEMBLER_DIALECT == ASM_ATT || USER_LABEL_PREFIX[0] == 0)
	putc ('%', file);
      if (TARGET_64BIT)
	fputs ("fs:0", file);
      else
	fputs ("gs:0", file);
      return;
    }

  if (! ix86_decompose_address (addr, &parts))
    abort ();

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;

  if (!base && !index)
    {
      /* Displacement only requires special attention.  */

      if (GET_CODE (disp) == CONST_INT)
	{
	  if (ASSEMBLER_DIALECT == ASM_INTEL)
	    {
	      if (USER_LABEL_PREFIX[0] == 0)
		putc ('%', file);
	      fputs ("ds:", file);
	    }
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (addr));
	}
      else if (flag_pic)
	output_pic_addr_const (file, addr, 0);
      else
	output_addr_const (file, addr);

      /* Use one byte shorter RIP relative addressing for 64bit mode.  */
      if (TARGET_64BIT
	  && ((GET_CODE (addr) == SYMBOL_REF
	       && ! tls_symbolic_operand (addr, GET_MODE (addr)))
	      || GET_CODE (addr) == LABEL_REF
	      || (GET_CODE (addr) == CONST
		  && GET_CODE (XEXP (addr, 0)) == PLUS
		  && (GET_CODE (XEXP (XEXP (addr, 0), 0)) == SYMBOL_REF
		      || GET_CODE (XEXP (XEXP (addr, 0), 0)) == LABEL_REF)
		  && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT)))
	fputs ("(%rip)", file);
    }
  else
    {
      if (ASSEMBLER_DIALECT == ASM_ATT)
	{
	  if (disp)
	    {
	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else
		output_addr_const (file, disp);
	    }

	  putc ('(', file);
	  if (base)
	    PRINT_REG (base, 0, file);
	  if (index)
	    {
	      putc (',', file);
	      PRINT_REG (index, 0, file);
	      if (scale != 1)
		fprintf (file, ",%d", scale);
	    }
	  putc (')', file);
	}
      else
	{
	  rtx offset = NULL_RTX;

	  if (disp)
	    {
	      /* Pull out the offset of a symbol; print any symbol itself.  */
	      if (GET_CODE (disp) == CONST
		  && GET_CODE (XEXP (disp, 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (disp, 0), 1)) == CONST_INT)
		{
		  offset = XEXP (XEXP (disp, 0), 1);
		  disp = gen_rtx_CONST (VOIDmode,
					XEXP (XEXP (disp, 0), 0));
		}

	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else if (GET_CODE (disp) == CONST_INT)
		offset = disp;
	      else
		output_addr_const (file, disp);
	    }

	  putc ('[', file);
	  if (base)
	    {
	      PRINT_REG (base, 0, file);
	      if (offset)
		{
		  if (INTVAL (offset) >= 0)
		    putc ('+', file);
		  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
		}
	    }
	  else if (offset)
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (offset));
	  else
	    putc ('0', file);

	  if (index)
	    {
	      putc ('+', file);
	      PRINT_REG (index, 0, file);
	      if (scale != 1)
		fprintf (file, "*%d", scale);
	    }
	  putc (']', file);
	}
    }
}

bool
output_addr_const_extra (file, x)
     FILE *file;
     rtx x;
{
  rtx op;

  if (GET_CODE (x) != UNSPEC)
    return false;

  op = XVECEXP (x, 0, 0);
  switch (XINT (x, 1))
    {
    case UNSPEC_GOTTPOFF:
      output_addr_const (file, op);
      /* FIXME: This might be @TPOFF in Sun ld.  */
      fputs ("@GOTTPOFF", file);
      break;
    case UNSPEC_TPOFF:
      output_addr_const (file, op);
      fputs ("@TPOFF", file);
      break;
    case UNSPEC_NTPOFF:
      output_addr_const (file, op);
      if (TARGET_64BIT)
	fputs ("@TPOFF", file);
      else
	fputs ("@NTPOFF", file);
      break;
    case UNSPEC_DTPOFF:
      output_addr_const (file, op);
      fputs ("@DTPOFF", file);
      break;
    case UNSPEC_GOTNTPOFF:
      output_addr_const (file, op);
      if (TARGET_64BIT)
	fputs ("@GOTTPOFF(%rip)", file);
      else
	fputs ("@GOTNTPOFF", file);
      break;
    case UNSPEC_INDNTPOFF:
      output_addr_const (file, op);
      fputs ("@INDNTPOFF", file);
      break;

    default:
      return false;
    }

  return true;
}

/* Split one or more DImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_di (operands, num, lo_half, hi_half)
     rtx operands[];
     int num;
     rtx lo_half[], hi_half[];
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses,
         but we still have to handle it.  */
      if (GET_CODE (op) == MEM)
	{
	  lo_half[num] = adjust_address (op, SImode, 0);
	  hi_half[num] = adjust_address (op, SImode, 4);
	}
      else
	{
	  lo_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 0);
	  hi_half[num] = simplify_gen_subreg (SImode, op,
					      GET_MODE (op) == VOIDmode
					      ? DImode : GET_MODE (op), 4);
	}
    }
}
/* Split one or more TImode RTL references into pairs of SImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_ti (operands, num, lo_half, hi_half)
     rtx operands[];
     int num;
     rtx lo_half[], hi_half[];
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses, but we
         still have to handle it.  */
      if (GET_CODE (op) == MEM)
	{
	  lo_half[num] = adjust_address (op, DImode, 0);
	  hi_half[num] = adjust_address (op, DImode, 8);
	}
      else
	{
	  lo_half[num] = simplify_gen_subreg (DImode, op, TImode, 0);
	  hi_half[num] = simplify_gen_subreg (DImode, op, TImode, 8);
	}
    }
}

/* Output code to perform a 387 binary operation in INSN, one of PLUS,
   MINUS, MULT or DIV.  OPERANDS are the insn operands, where operands[3]
   is the expression of the binary operation.  The output may either be
   emitted here, or returned to the caller, like all output_* functions.

   There is no guarantee that the operands are the same mode, as they
   might be within FLOAT or FLOAT_EXTEND expressions.  */

#ifndef SYSV386_COMPAT
/* Set to 1 for compatibility with brain-damaged assemblers.  No-one
   wants to fix the assemblers because that causes incompatibility
   with gcc.  No-one wants to fix gcc because that causes
   incompatibility with assemblers...  You can use the option of
   -DSYSV386_COMPAT=0 if you recompile both gcc and gas this way.  */
#define SYSV386_COMPAT 1
#endif

const char *
output_387_binary_op (insn, operands)
     rtx insn;
     rtx *operands;
{
  static char buf[30];
  const char *p;
  const char *ssep;
  int is_sse = SSE_REG_P (operands[0]) | SSE_REG_P (operands[1]) | SSE_REG_P (operands[2]);

#ifdef ENABLE_CHECKING
  /* Even if we do not want to check the inputs, this documents input
     constraints.  Which helps in understanding the following code.  */
  if (STACK_REG_P (operands[0])
      && ((REG_P (operands[1])
	   && REGNO (operands[0]) == REGNO (operands[1])
	   && (STACK_REG_P (operands[2]) || GET_CODE (operands[2]) == MEM))
	  || (REG_P (operands[2])
	      && REGNO (operands[0]) == REGNO (operands[2])
	      && (STACK_REG_P (operands[1]) || GET_CODE (operands[1]) == MEM)))
      && (STACK_TOP_P (operands[1]) || STACK_TOP_P (operands[2])))
    ; /* ok */
  else if (!is_sse)
    abort ();
#endif

  switch (GET_CODE (operands[3]))
    {
    case PLUS:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fiadd";
      else
	p = "fadd";
      ssep = "add";
      break;

    case MINUS:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fisub";
      else
	p = "fsub";
      ssep = "sub";
      break;

    case MULT:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fimul";
      else
	p = "fmul";
      ssep = "mul";
      break;

    case DIV:
      if (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT
	  || GET_MODE_CLASS (GET_MODE (operands[2])) == MODE_INT)
	p = "fidiv";
      else
	p = "fdiv";
      ssep = "div";
      break;

    default:
      abort ();
    }

  if (is_sse)
   {
      strcpy (buf, ssep);
      if (GET_MODE (operands[0]) == SFmode)
	strcat (buf, "ss\t{%2, %0|%0, %2}");
      else
	strcat (buf, "sd\t{%2, %0|%0, %2}");
      return buf;
   }
  strcpy (buf, p);

  switch (GET_CODE (operands[3]))
    {
    case MULT:
    case PLUS:
      if (REG_P (operands[2]) && REGNO (operands[0]) == REGNO (operands[2]))
	{
	  rtx temp = operands[2];
	  operands[2] = operands[1];
	  operands[1] = temp;
	}

      /* know operands[0] == operands[1].  */

      if (GET_CODE (operands[2]) == MEM)
	{
	  p = "%z2\t%2";
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	{
	  if (STACK_TOP_P (operands[0]))
	    /* How is it that we are storing to a dead operand[2]?
	       Well, presumably operands[1] is dead too.  We can't
	       store the result to st(0) as st(0) gets popped on this
	       instruction.  Instead store to operands[2] (which I
	       think has to be st(1)).  st(1) will be popped later.
	       gcc <= 2.8.1 didn't have this check and generated
	       assembly code that the Unixware assembler rejected.  */
	    p = "p\t{%0, %2|%2, %0}";	/* st(1) = st(0) op st(1); pop */
	  else
	    p = "p\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0); pop */
	  break;
	}

      if (STACK_TOP_P (operands[0]))
	p = "\t{%y2, %0|%0, %y2}";	/* st(0) = st(0) op st(r2) */
      else
	p = "\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0) */
      break;

    case MINUS:
    case DIV:
      if (GET_CODE (operands[1]) == MEM)
	{
	  p = "r%z1\t%1";
	  break;
	}

      if (GET_CODE (operands[2]) == MEM)
	{
	  p = "%z2\t%2";
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[2])))
	{
#if SYSV386_COMPAT
	  /* The SystemV/386 SVR3.2 assembler, and probably all AT&T
	     derived assemblers, confusingly reverse the direction of
	     the operation for fsub{r} and fdiv{r} when the
	     destination register is not st(0).  The Intel assembler
	     doesn't have this brain damage.  Read !SYSV386_COMPAT to
	     figure out what the hardware really does.  */
	  if (STACK_TOP_P (operands[0]))
	    p = "{p\t%0, %2|rp\t%2, %0}";
	  else
	    p = "{rp\t%2, %0|p\t%0, %2}";
#else
	  if (STACK_TOP_P (operands[0]))
	    /* As above for fmul/fadd, we can't store to st(0).  */
	    p = "rp\t{%0, %2|%2, %0}";	/* st(1) = st(0) op st(1); pop */
	  else
	    p = "p\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0); pop */
#endif
	  break;
	}

      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	{
#if SYSV386_COMPAT
	  if (STACK_TOP_P (operands[0]))
	    p = "{rp\t%0, %1|p\t%1, %0}";
	  else
	    p = "{p\t%1, %0|rp\t%0, %1}";
#else
	  if (STACK_TOP_P (operands[0]))
	    p = "p\t{%0, %1|%1, %0}";	/* st(1) = st(1) op st(0); pop */
	  else
	    p = "rp\t{%1, %0|%0, %1}";	/* st(r2) = st(0) op st(r2); pop */
#endif
	  break;
	}

      if (STACK_TOP_P (operands[0]))
	{
	  if (STACK_TOP_P (operands[1]))
	    p = "\t{%y2, %0|%0, %y2}";	/* st(0) = st(0) op st(r2) */
	  else
	    p = "r\t{%y1, %0|%0, %y1}";	/* st(0) = st(r1) op st(0) */
	  break;
	}
      else if (STACK_TOP_P (operands[1]))
	{
#if SYSV386_COMPAT
	  p = "{\t%1, %0|r\t%0, %1}";
#else
	  p = "r\t{%1, %0|%0, %1}";	/* st(r2) = st(0) op st(r2) */
#endif
	}
      else
	{
#if SYSV386_COMPAT
	  p = "{r\t%2, %0|\t%0, %2}";
#else
	  p = "\t{%2, %0|%0, %2}";	/* st(r1) = st(r1) op st(0) */
#endif
	}
      break;

    default:
      abort ();
    }

  strcat (buf, p);
  return buf;
}

/* Output code to initialize control word copies used by
   trunc?f?i patterns.  NORMAL is set to current control word, while ROUND_DOWN
   is set to control word rounding downwards.  */
void
emit_i387_cw_initialization (normal, round_down)
     rtx normal, round_down;
{
  rtx reg = gen_reg_rtx (HImode);

  emit_insn (gen_x86_fnstcw_1 (normal));
  emit_move_insn (reg, normal);
  if (!TARGET_PARTIAL_REG_STALL && !optimize_size
      && !TARGET_64BIT)
    emit_insn (gen_movsi_insv_1 (reg, GEN_INT (0xc)));
  else
    emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0xc00)));
  emit_move_insn (round_down, reg);
}

/* Output code for INSN to convert a float to a signed int.  OPERANDS
   are the insn operands.  The output may be [HSD]Imode and the input
   operand may be [SDX]Fmode.  */

const char *
output_fix_trunc (insn, operands)
     rtx insn;
     rtx *operands;
{
  int stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;
  int dimode_p = GET_MODE (operands[0]) == DImode;

  /* Jump through a hoop or two for DImode, since the hardware has no
     non-popping instruction.  We used to do this a different way, but
     that was somewhat fragile and broke with post-reload splitters.  */
  if (dimode_p && !stack_top_dies)
    output_asm_insn ("fld\t%y1", operands);

  if (!STACK_TOP_P (operands[1]))
    abort ();

  if (GET_CODE (operands[0]) != MEM)
    abort ();

  output_asm_insn ("fldcw\t%3", operands);
  if (stack_top_dies || dimode_p)
    output_asm_insn ("fistp%z0\t%0", operands);
  else
    output_asm_insn ("fist%z0\t%0", operands);
  output_asm_insn ("fldcw\t%2", operands);

  return "";
}

/* Output code for INSN to compare OPERANDS.  EFLAGS_P is 1 when fcomi
   should be used and 2 when fnstsw should be used.  UNORDERED_P is true
   when fucom should be used.  */

const char *
output_fp_compare (insn, operands, eflags_p, unordered_p)
     rtx insn;
     rtx *operands;
     int eflags_p, unordered_p;
{
  int stack_top_dies;
  rtx cmp_op0 = operands[0];
  rtx cmp_op1 = operands[1];
  int is_sse = SSE_REG_P (operands[0]) | SSE_REG_P (operands[1]);

  if (eflags_p == 2)
    {
      cmp_op0 = cmp_op1;
      cmp_op1 = operands[2];
    }
  if (is_sse)
    {
      if (GET_MODE (operands[0]) == SFmode)
	if (unordered_p)
	  return "ucomiss\t{%1, %0|%0, %1}";
	else
	  return "comiss\t{%1, %0|%0, %1}";
      else
	if (unordered_p)
	  return "ucomisd\t{%1, %0|%0, %1}";
	else
	  return "comisd\t{%1, %0|%0, %1}";
    }

  if (! STACK_TOP_P (cmp_op0))
    abort ();

  stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;

  if (STACK_REG_P (cmp_op1)
      && stack_top_dies
      && find_regno_note (insn, REG_DEAD, REGNO (cmp_op1))
      && REGNO (cmp_op1) != FIRST_STACK_REG)
    {
      /* If both the top of the 387 stack dies, and the other operand
	 is also a stack register that dies, then this must be a
	 `fcompp' float compare */

      if (eflags_p == 1)
	{
	  /* There is no double popping fcomi variant.  Fortunately,
	     eflags is immune from the fstp's cc clobbering.  */
	  if (unordered_p)
	    output_asm_insn ("fucomip\t{%y1, %0|%0, %y1}", operands);
	  else
	    output_asm_insn ("fcomip\t{%y1, %0|%0, %y1}", operands);
	  return "fstp\t%y0";
	}
      else
	{
	  if (eflags_p == 2)
	    {
	      if (unordered_p)
		return "fucompp\n\tfnstsw\t%0";
	      else
		return "fcompp\n\tfnstsw\t%0";
	    }
	  else
	    {
	      if (unordered_p)
		return "fucompp";
	      else
		return "fcompp";
	    }
	}
    }
  else
    {
      /* Encoded here as eflags_p | intmode | unordered_p | stack_top_dies.  */

      static const char * const alt[24] =
      {
	"fcom%z1\t%y1",
	"fcomp%z1\t%y1",
	"fucom%z1\t%y1",
	"fucomp%z1\t%y1",

	"ficom%z1\t%y1",
	"ficomp%z1\t%y1",
	NULL,
	NULL,

	"fcomi\t{%y1, %0|%0, %y1}",
	"fcomip\t{%y1, %0|%0, %y1}",
	"fucomi\t{%y1, %0|%0, %y1}",
	"fucomip\t{%y1, %0|%0, %y1}",

	NULL,
	NULL,
	NULL,
	NULL,

	"fcom%z2\t%y2\n\tfnstsw\t%0",
	"fcomp%z2\t%y2\n\tfnstsw\t%0",
	"fucom%z2\t%y2\n\tfnstsw\t%0",
	"fucomp%z2\t%y2\n\tfnstsw\t%0",

	"ficom%z2\t%y2\n\tfnstsw\t%0",
	"ficomp%z2\t%y2\n\tfnstsw\t%0",
	NULL,
	NULL
      };

      int mask;
      const char *ret;

      mask  = eflags_p << 3;
      mask |= (GET_MODE_CLASS (GET_MODE (operands[1])) == MODE_INT) << 2;
      mask |= unordered_p << 1;
      mask |= stack_top_dies;

      if (mask >= 24)
	abort ();
      ret = alt[mask];
      if (ret == NULL)
	abort ();

      return ret;
    }
}

void
ix86_output_addr_vec_elt (file, value)
     FILE *file;
     int value;
{
  const char *directive = ASM_LONG;

  if (TARGET_64BIT)
    {
#ifdef ASM_QUAD
      directive = ASM_QUAD;
#else
      abort ();
#endif
    }

  fprintf (file, "%s%s%d\n", directive, LPREFIX, value);
}

void
ix86_output_addr_diff_elt (file, value, rel)
     FILE *file;
     int value, rel;
{
  if (TARGET_64BIT)
    fprintf (file, "%s%s%d-%s%d\n",
	     ASM_LONG, LPREFIX, value, LPREFIX, rel);
  else if (HAVE_AS_GOTOFF_IN_DATA)
    fprintf (file, "%s%s%d@GOTOFF\n", ASM_LONG, LPREFIX, value);
#if TARGET_MACHO
  else if (TARGET_MACHO)
    fprintf (file, "%s%s%d-%s\n", ASM_LONG, LPREFIX, value,
	     machopic_function_base_name () + 1);
#endif
  else
    asm_fprintf (file, "%s%U%s+[.-%s%d]\n",
		 ASM_LONG, GOT_SYMBOL_NAME, LPREFIX, value);
}

/* Generate either "mov $0, reg" or "xor reg, reg", as appropriate
   for the target.  */

void
ix86_expand_clear (dest)
     rtx dest;
{
  rtx tmp;

  /* We play register width games, which are only valid after reload.  */
  if (!reload_completed)
    abort ();

  /* Avoid HImode and its attendant prefix byte.  */
  if (GET_MODE_SIZE (GET_MODE (dest)) < 4)
    dest = gen_rtx_REG (SImode, REGNO (dest));

  tmp = gen_rtx_SET (VOIDmode, dest, const0_rtx);

  /* This predicate should match that for movsi_xor and movdi_xor_rex64.  */
  if (reload_completed && (!TARGET_USE_MOV0 || optimize_size))
    {
      rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, 17));
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, clob));
    }

  emit_insn (tmp);
}

/* X is an unchanging MEM.  If it is a constant pool reference, return
   the constant pool rtx, else NULL.  */

static rtx
maybe_get_pool_constant (x)
     rtx x;
{
  x = XEXP (x, 0);

  if (flag_pic && ! TARGET_64BIT)
    {
      if (GET_CODE (x) != PLUS)
	return NULL_RTX;
      if (XEXP (x, 0) != pic_offset_table_rtx)
	return NULL_RTX;
      x = XEXP (x, 1);
      if (GET_CODE (x) != CONST)
	return NULL_RTX;
      x = XEXP (x, 0);
      if (GET_CODE (x) != UNSPEC)
	return NULL_RTX;
      if (XINT (x, 1) != UNSPEC_GOTOFF)
	return NULL_RTX;
      x = XVECEXP (x, 0, 0);
    }

  if (GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x))
    return get_pool_constant (x);

  return NULL_RTX;
}

void
ix86_expand_move (mode, operands)
     enum machine_mode mode;
     rtx operands[];
{
  int strict = (reload_in_progress || reload_completed);
  rtx insn, op0, op1, tmp;

  op0 = operands[0];
  op1 = operands[1];

  if (tls_symbolic_operand (op1, Pmode))
    {
      op1 = legitimize_address (op1, op1, VOIDmode);
      if (GET_CODE (op0) == MEM)
	{
	  tmp = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (VOIDmode, tmp, op1));
	  op1 = tmp;
	}
    }
  else if (flag_pic && mode == Pmode && symbolic_operand (op1, Pmode))
    {
#if TARGET_MACHO
      if (MACHOPIC_PURE)
	{
	  rtx temp = ((reload_in_progress
		       || ((op0 && GET_CODE (op0) == REG)
			   && mode == Pmode))
		      ? op0 : gen_reg_rtx (Pmode));
	  op1 = machopic_indirect_data_reference (op1, temp);
	  op1 = machopic_legitimize_pic_address (op1, mode,
						 temp == op1 ? 0 : temp);
	}
      else
	{
	  if (MACHOPIC_INDIRECT)
	    op1 = machopic_indirect_data_reference (op1, 0);
	}
      if (op0 != op1)
	{
	  insn = gen_rtx_SET (VOIDmode, op0, op1);
	  emit_insn (insn);
	}
      return;
#endif /* TARGET_MACHO */
      if (GET_CODE (op0) == MEM)
	op1 = force_reg (Pmode, op1);
      else
	{
	  rtx temp = op0;
	  if (GET_CODE (temp) != REG)
	    temp = gen_reg_rtx (Pmode);
	  temp = legitimize_pic_address (op1, temp);
	  if (temp == op0)
	    return;
	  op1 = temp;
	}
    }
  else
    {
      if (GET_CODE (op0) == MEM
	  && (PUSH_ROUNDING (GET_MODE_SIZE (mode)) != GET_MODE_SIZE (mode)
	      || !push_operand (op0, mode))
	  && GET_CODE (op1) == MEM)
	op1 = force_reg (mode, op1);

      if (push_operand (op0, mode)
	  && ! general_no_elim_operand (op1, mode))
	op1 = copy_to_mode_reg (mode, op1);

      /* Force large constants in 64bit compilation into register
	 to get them CSEed.  */
      if (TARGET_64BIT && mode == DImode
	  && immediate_operand (op1, mode)
	  && !x86_64_zero_extended_value (op1)
	  && !register_operand (op0, mode)
	  && optimize && !reload_completed && !reload_in_progress)
	op1 = copy_to_mode_reg (mode, op1);

      if (FLOAT_MODE_P (mode))
	{
	  /* If we are loading a floating point constant to a register,
	     force the value to memory now, since we'll get better code
	     out the back end.  */

	  if (strict)
	    ;
	  else if (GET_CODE (op1) == CONST_DOUBLE)
	    {
	      op1 = validize_mem (force_const_mem (mode, op1));
	      if (!register_operand (op0, mode))
		{
		  rtx temp = gen_reg_rtx (mode);
		  emit_insn (gen_rtx_SET (VOIDmode, temp, op1));
		  emit_move_insn (op0, temp);
		  return;
		}
	    }
	}
    }

  insn = gen_rtx_SET (VOIDmode, op0, op1);

  emit_insn (insn);
}

void
ix86_expand_vector_move (mode, operands)
     enum machine_mode mode;
     rtx operands[];
{
  /* Force constants other than zero into memory.  We do not know how
     the instructions used to build constants modify the upper 64 bits
     of the register, once we have that information we may be able
     to handle some of them more efficiently.  */
  if ((reload_in_progress | reload_completed) == 0
      && register_operand (operands[0], mode)
      && CONSTANT_P (operands[1]) && operands[1] != CONST0_RTX (mode))
    {
      operands[1] = force_const_mem (mode, operands[1]);
      emit_move_insn (operands[0], operands[1]);
      return;
    }

  /* Make operand1 a register if it isn't already.  */
  if (!no_new_pseudos
      && !register_operand (operands[0], mode)
      && !register_operand (operands[1], mode))
    {
      rtx temp = force_reg (GET_MODE (operands[1]), operands[1]);
      emit_move_insn (operands[0], temp);
      return;
    }

  emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
}

/* Attempt to expand a binary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 3 separate
   memory references (one output, two input) in a single insn.  */

void
ix86_expand_binary_operator (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode;
     rtx operands[];
{
  int matching_memory;
  rtx src1, src2, dst, op, clob;

  dst = operands[0];
  src1 = operands[1];
  src2 = operands[2];

  /* Recognize <var1> = <value> <op> <var1> for commutative operators */
  if (GET_RTX_CLASS (code) == 'c'
      && (rtx_equal_p (dst, src2)
	  || immediate_operand (src1, mode)))
    {
      rtx temp = src1;
      src1 = src2;
      src2 = temp;
    }

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  matching_memory = 0;
  if (GET_CODE (dst) == MEM)
    {
      if (rtx_equal_p (dst, src1))
	matching_memory = 1;
      else if (GET_RTX_CLASS (code) == 'c'
	       && rtx_equal_p (dst, src2))
	matching_memory = 2;
      else
	dst = gen_reg_rtx (mode);
    }

  /* Both source operands cannot be in memory.  */
  if (GET_CODE (src1) == MEM && GET_CODE (src2) == MEM)
    {
      if (matching_memory != 2)
	src2 = force_reg (mode, src2);
      else
	src1 = force_reg (mode, src1);
    }

  /* If the operation is not commutable, source 1 cannot be a constant
     or non-matching memory.  */
  if ((CONSTANT_P (src1)
       || (!matching_memory && GET_CODE (src1) == MEM))
      && GET_RTX_CLASS (code) != 'c')
    src1 = force_reg (mode, src1);

  /* If optimizing, copy to regs to improve CSE */
  if (optimize && ! no_new_pseudos)
    {
      if (GET_CODE (dst) == MEM)
	dst = gen_reg_rtx (mode);
      if (GET_CODE (src1) == MEM)
	src1 = force_reg (mode, src1);
      if (GET_CODE (src2) == MEM)
	src2 = force_reg (mode, src2);
    }

  /* Emit the instruction.  */

  op = gen_rtx_SET (VOIDmode, dst, gen_rtx_fmt_ee (code, mode, src1, src2));
  if (reload_in_progress)
    {
      /* Reload doesn't know about the flags register, and doesn't know that
         it doesn't want to clobber it.  We can only do this with PLUS.  */
      if (code != PLUS)
	abort ();
      emit_insn (op);
    }
  else
    {
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
    }

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);
}

/* Return TRUE or FALSE depending on whether the binary operator meets the
   appropriate constraints.  */

int
ix86_binary_operator_ok (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx operands[3];
{
  /* Both source operands cannot be in memory.  */
  if (GET_CODE (operands[1]) == MEM && GET_CODE (operands[2]) == MEM)
    return 0;
  /* If the operation is not commutable, source 1 cannot be a constant.  */
  if (CONSTANT_P (operands[1]) && GET_RTX_CLASS (code) != 'c')
    return 0;
  /* If the destination is memory, we must have a matching source operand.  */
  if (GET_CODE (operands[0]) == MEM
      && ! (rtx_equal_p (operands[0], operands[1])
	    || (GET_RTX_CLASS (code) == 'c'
		&& rtx_equal_p (operands[0], operands[2]))))
    return 0;
  /* If the operation is not commutable and the source 1 is memory, we must
     have a matching destination.  */
  if (GET_CODE (operands[1]) == MEM
      && GET_RTX_CLASS (code) != 'c'
      && ! rtx_equal_p (operands[0], operands[1]))
    return 0;
  return 1;
}

/* Attempt to expand a unary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 2 separate
   memory references (one output, one input) in a single insn.  */

void
ix86_expand_unary_operator (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode;
     rtx operands[];
{
  int matching_memory;
  rtx src, dst, op, clob;

  dst = operands[0];
  src = operands[1];

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  matching_memory = 0;
  if (GET_CODE (dst) == MEM)
    {
      if (rtx_equal_p (dst, src))
	matching_memory = 1;
      else
	dst = gen_reg_rtx (mode);
    }

  /* When source operand is memory, destination must match.  */
  if (!matching_memory && GET_CODE (src) == MEM)
    src = force_reg (mode, src);

  /* If optimizing, copy to regs to improve CSE */
  if (optimize && ! no_new_pseudos)
    {
      if (GET_CODE (dst) == MEM)
	dst = gen_reg_rtx (mode);
      if (GET_CODE (src) == MEM)
	src = force_reg (mode, src);
    }

  /* Emit the instruction.  */

  op = gen_rtx_SET (VOIDmode, dst, gen_rtx_fmt_e (code, mode, src));
  if (reload_in_progress || code == NOT)
    {
      /* Reload doesn't know about the flags register, and doesn't know that
         it doesn't want to clobber it.  */
      if (code != NOT)
        abort ();
      emit_insn (op);
    }
  else
    {
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
    }

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);
}

/* Return TRUE or FALSE depending on whether the unary operator meets the
   appropriate constraints.  */

int
ix86_unary_operator_ok (code, mode, operands)
     enum rtx_code code ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     rtx operands[2] ATTRIBUTE_UNUSED;
{
  /* If one of operands is memory, source and destination must match.  */
  if ((GET_CODE (operands[0]) == MEM
       || GET_CODE (operands[1]) == MEM)
      && ! rtx_equal_p (operands[0], operands[1]))
    return FALSE;
  return TRUE;
}

/* Return TRUE or FALSE depending on whether the first SET in INSN
   has source and destination with matching CC modes, and that the
   CC mode is at least as constrained as REQ_MODE.  */

int
ix86_match_ccmode (insn, req_mode)
     rtx insn;
     enum machine_mode req_mode;
{
  rtx set;
  enum machine_mode set_mode;

  set = PATTERN (insn);
  if (GET_CODE (set) == PARALLEL)
    set = XVECEXP (set, 0, 0);
  if (GET_CODE (set) != SET)
    abort ();
  if (GET_CODE (SET_SRC (set)) != COMPARE)
    abort ();

  set_mode = GET_MODE (SET_DEST (set));
  switch (set_mode)
    {
    case CCNOmode:
      if (req_mode != CCNOmode
	  && (req_mode != CCmode
	      || XEXP (SET_SRC (set), 1) != const0_rtx))
	return 0;
      break;
    case CCmode:
      if (req_mode == CCGCmode)
	return 0;
      /* FALLTHRU */
    case CCGCmode:
      if (req_mode == CCGOCmode || req_mode == CCNOmode)
	return 0;
      /* FALLTHRU */
    case CCGOCmode:
      if (req_mode == CCZmode)
	return 0;
      /* FALLTHRU */
    case CCZmode:
      break;

    default:
      abort ();
    }

  return (GET_MODE (SET_SRC (set)) == set_mode);
}

/* Generate insn patterns to do an integer compare of OPERANDS.  */

static rtx
ix86_expand_int_compare (code, op0, op1)
     enum rtx_code code;
     rtx op0, op1;
{
  enum machine_mode cmpmode;
  rtx tmp, flags;

  cmpmode = SELECT_CC_MODE (code, op0, op1);
  flags = gen_rtx_REG (cmpmode, FLAGS_REG);

  /* This is very simple, but making the interface the same as in the
     FP case makes the rest of the code easier.  */
  tmp = gen_rtx_COMPARE (cmpmode, op0, op1);
  emit_insn (gen_rtx_SET (VOIDmode, flags, tmp));

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode, flags, const0_rtx);
}

/* Figure out whether to use ordered or unordered fp comparisons.
   Return the appropriate mode to use.  */

enum machine_mode
ix86_fp_compare_mode (code)
     enum rtx_code code ATTRIBUTE_UNUSED;
{
  /* ??? In order to make all comparisons reversible, we do all comparisons
     non-trapping when compiling for IEEE.  Once gcc is able to distinguish
     all forms trapping and nontrapping comparisons, we can make inequality
     comparisons trapping again, since it results in better code when using
     FCOM based compares.  */
  return TARGET_IEEE_FP ? CCFPUmode : CCFPmode;
}

enum machine_mode
ix86_cc_mode (code, op0, op1)
     enum rtx_code code;
     rtx op0, op1;
{
  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    return ix86_fp_compare_mode (code);
  switch (code)
    {
      /* Only zero flag is needed.  */
    case EQ:			/* ZF=0 */
    case NE:			/* ZF!=0 */
      return CCZmode;
      /* Codes needing carry flag.  */
    case GEU:			/* CF=0 */
    case GTU:			/* CF=0 & ZF=0 */
    case LTU:			/* CF=1 */
    case LEU:			/* CF=1 | ZF=1 */
      return CCmode;
      /* Codes possibly doable only with sign flag when
         comparing against zero.  */
    case GE:			/* SF=OF   or   SF=0 */
    case LT:			/* SF<>OF  or   SF=1 */
      if (op1 == const0_rtx)
	return CCGOCmode;
      else
	/* For other cases Carry flag is not required.  */
	return CCGCmode;
      /* Codes doable only with sign flag when comparing
         against zero, but we miss jump instruction for it
         so we need to use relational tests agains overflow
         that thus needs to be zero.  */
    case GT:			/* ZF=0 & SF=OF */
    case LE:			/* ZF=1 | SF<>OF */
      if (op1 == const0_rtx)
	return CCNOmode;
      else
	return CCGCmode;
      /* strcmp pattern do (use flags) and combine may ask us for proper
	 mode.  */
    case USE:
      return CCmode;
    default:
      abort ();
    }
}

/* Return true if we should use an FCOMI instruction for this fp comparison.  */

int
ix86_use_fcomi_compare (code)
     enum rtx_code code ATTRIBUTE_UNUSED;
{
  enum rtx_code swapped_code = swap_condition (code);
  return ((ix86_fp_comparison_cost (code) == ix86_fp_comparison_fcomi_cost (code))
	  || (ix86_fp_comparison_cost (swapped_code)
	      == ix86_fp_comparison_fcomi_cost (swapped_code)));
}

/* Swap, force into registers, or otherwise massage the two operands
   to a fp comparison.  The operands are updated in place; the new
   comparsion code is returned.  */

static enum rtx_code
ix86_prepare_fp_compare_args (code, pop0, pop1)
     enum rtx_code code;
     rtx *pop0, *pop1;
{
  enum machine_mode fpcmp_mode = ix86_fp_compare_mode (code);
  rtx op0 = *pop0, op1 = *pop1;
  enum machine_mode op_mode = GET_MODE (op0);
  int is_sse = SSE_REG_P (op0) | SSE_REG_P (op1);

  /* All of the unordered compare instructions only work on registers.
     The same is true of the XFmode compare instructions.  The same is
     true of the fcomi compare instructions.  */

  if (!is_sse
      && (fpcmp_mode == CCFPUmode
	  || op_mode == XFmode
	  || op_mode == TFmode
	  || ix86_use_fcomi_compare (code)))
    {
      op0 = force_reg (op_mode, op0);
      op1 = force_reg (op_mode, op1);
    }
  else
    {
      /* %%% We only allow op1 in memory; op0 must be st(0).  So swap
	 things around if they appear profitable, otherwise force op0
	 into a register.  */

      if (standard_80387_constant_p (op0) == 0
	  || (GET_CODE (op0) == MEM
	      && ! (standard_80387_constant_p (op1) == 0
		    || GET_CODE (op1) == MEM)))
	{
	  rtx tmp;
	  tmp = op0, op0 = op1, op1 = tmp;
	  code = swap_condition (code);
	}

      if (GET_CODE (op0) != REG)
	op0 = force_reg (op_mode, op0);

      if (CONSTANT_P (op1))
	{
	  if (standard_80387_constant_p (op1))
	    op1 = force_reg (op_mode, op1);
	  else
	    op1 = validize_mem (force_const_mem (op_mode, op1));
	}
    }

  /* Try to rearrange the comparison to make it cheaper.  */
  if (ix86_fp_comparison_cost (code)
      > ix86_fp_comparison_cost (swap_condition (code))
      && (GET_CODE (op1) == REG || !no_new_pseudos))
    {
      rtx tmp;
      tmp = op0, op0 = op1, op1 = tmp;
      code = swap_condition (code);
      if (GET_CODE (op0) != REG)
	op0 = force_reg (op_mode, op0);
    }

  *pop0 = op0;
  *pop1 = op1;
  return code;
}

/* Convert comparison codes we use to represent FP comparison to integer
   code that will result in proper branch.  Return UNKNOWN if no such code
   is available.  */
static enum rtx_code
ix86_fp_compare_code_to_integer (code)
     enum rtx_code code;
{
  switch (code)
    {
    case GT:
      return GTU;
    case GE:
      return GEU;
    case ORDERED:
    case UNORDERED:
      return code;
      break;
    case UNEQ:
      return EQ;
      break;
    case UNLT:
      return LTU;
      break;
    case UNLE:
      return LEU;
      break;
    case LTGT:
      return NE;
      break;
    default:
      return UNKNOWN;
    }
}

/* Split comparison code CODE into comparisons we can do using branch
   instructions.  BYPASS_CODE is comparison code for branch that will
   branch around FIRST_CODE and SECOND_CODE.  If some of branches
   is not required, set value to NIL.
   We never require more than two branches.  */
static void
ix86_fp_comparison_codes (code, bypass_code, first_code, second_code)
     enum rtx_code code, *bypass_code, *first_code, *second_code;
{
  *first_code = code;
  *bypass_code = NIL;
  *second_code = NIL;

  /* The fcomi comparison sets flags as follows:

     cmp    ZF PF CF
     >      0  0  0
     <      0  0  1
     =      1  0  0
     un     1  1  1 */

  switch (code)
    {
    case GT:			/* GTU - CF=0 & ZF=0 */
    case GE:			/* GEU - CF=0 */
    case ORDERED:		/* PF=0 */
    case UNORDERED:		/* PF=1 */
    case UNEQ:			/* EQ - ZF=1 */
    case UNLT:			/* LTU - CF=1 */
    case UNLE:			/* LEU - CF=1 | ZF=1 */
    case LTGT:			/* EQ - ZF=0 */
      break;
    case LT:			/* LTU - CF=1 - fails on unordered */
      *first_code = UNLT;
      *bypass_code = UNORDERED;
      break;
    case LE:			/* LEU - CF=1 | ZF=1 - fails on unordered */
      *first_code = UNLE;
      *bypass_code = UNORDERED;
      break;
    case EQ:			/* EQ - ZF=1 - fails on unordered */
      *first_code = UNEQ;
      *bypass_code = UNORDERED;
      break;
    case NE:			/* NE - ZF=0 - fails on unordered */
      *first_code = LTGT;
      *second_code = UNORDERED;
      break;
    case UNGE:			/* GEU - CF=0 - fails on unordered */
      *first_code = GE;
      *second_code = UNORDERED;
      break;
    case UNGT:			/* GTU - CF=0 & ZF=0 - fails on unordered */
      *first_code = GT;
      *second_code = UNORDERED;
      break;
    default:
      abort ();
    }
  if (!TARGET_IEEE_FP)
    {
      *second_code = NIL;
      *bypass_code = NIL;
    }
}

/* Return cost of comparison done fcom + arithmetics operations on AX.
   All following functions do use number of instructions as an cost metrics.
   In future this should be tweaked to compute bytes for optimize_size and
   take into account performance of various instructions on various CPUs.  */
static int
ix86_fp_comparison_arithmetics_cost (code)
     enum rtx_code code;
{
  if (!TARGET_IEEE_FP)
    return 4;
  /* The cost of code output by ix86_expand_fp_compare.  */
  switch (code)
    {
    case UNLE:
    case UNLT:
    case LTGT:
    case GT:
    case GE:
    case UNORDERED:
    case ORDERED:
    case UNEQ:
      return 4;
      break;
    case LT:
    case NE:
    case EQ:
    case UNGE:
      return 5;
      break;
    case LE:
    case UNGT:
      return 6;
      break;
    default:
      abort ();
    }
}

/* Return cost of comparison done using fcomi operation.
   See ix86_fp_comparison_arithmetics_cost for the metrics.  */
static int
ix86_fp_comparison_fcomi_cost (code)
     enum rtx_code code;
{
  enum rtx_code bypass_code, first_code, second_code;
  /* Return arbitarily high cost when instruction is not supported - this
     prevents gcc from using it.  */
  if (!TARGET_CMOVE)
    return 1024;
  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);
  return (bypass_code != NIL || second_code != NIL) + 2;
}

/* Return cost of comparison done using sahf operation.
   See ix86_fp_comparison_arithmetics_cost for the metrics.  */
static int
ix86_fp_comparison_sahf_cost (code)
     enum rtx_code code;
{
  enum rtx_code bypass_code, first_code, second_code;
  /* Return arbitarily high cost when instruction is not preferred - this
     avoids gcc from using it.  */
  if (!TARGET_USE_SAHF && !optimize_size)
    return 1024;
  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);
  return (bypass_code != NIL || second_code != NIL) + 3;
}

/* Compute cost of the comparison done using any method.
   See ix86_fp_comparison_arithmetics_cost for the metrics.  */
static int
ix86_fp_comparison_cost (code)
     enum rtx_code code;
{
  int fcomi_cost, sahf_cost, arithmetics_cost = 1024;
  int min;

  fcomi_cost = ix86_fp_comparison_fcomi_cost (code);
  sahf_cost = ix86_fp_comparison_sahf_cost (code);

  min = arithmetics_cost = ix86_fp_comparison_arithmetics_cost (code);
  if (min > sahf_cost)
    min = sahf_cost;
  if (min > fcomi_cost)
    min = fcomi_cost;
  return min;
}

/* Generate insn patterns to do a floating point compare of OPERANDS.  */

static rtx
ix86_expand_fp_compare (code, op0, op1, scratch, second_test, bypass_test)
     enum rtx_code code;
     rtx op0, op1, scratch;
     rtx *second_test;
     rtx *bypass_test;
{
  enum machine_mode fpcmp_mode, intcmp_mode;
  rtx tmp, tmp2;
  int cost = ix86_fp_comparison_cost (code);
  enum rtx_code bypass_code, first_code, second_code;

  fpcmp_mode = ix86_fp_compare_mode (code);
  code = ix86_prepare_fp_compare_args (code, &op0, &op1);

  if (second_test)
    *second_test = NULL_RTX;
  if (bypass_test)
    *bypass_test = NULL_RTX;

  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);

  /* Do fcomi/sahf based test when profitable.  */
  if ((bypass_code == NIL || bypass_test)
      && (second_code == NIL || second_test)
      && ix86_fp_comparison_arithmetics_cost (code) > cost)
    {
      if (TARGET_CMOVE)
	{
	  tmp = gen_rtx_COMPARE (fpcmp_mode, op0, op1);
	  tmp = gen_rtx_SET (VOIDmode, gen_rtx_REG (fpcmp_mode, FLAGS_REG),
			     tmp);
	  emit_insn (tmp);
	}
      else
	{
	  tmp = gen_rtx_COMPARE (fpcmp_mode, op0, op1);
	  tmp2 = gen_rtx_UNSPEC (HImode, gen_rtvec (1, tmp), UNSPEC_FNSTSW);
	  if (!scratch)
	    scratch = gen_reg_rtx (HImode);
	  emit_insn (gen_rtx_SET (VOIDmode, scratch, tmp2));
	  emit_insn (gen_x86_sahf_1 (scratch));
	}

      /* The FP codes work out to act like unsigned.  */
      intcmp_mode = fpcmp_mode;
      code = first_code;
      if (bypass_code != NIL)
	*bypass_test = gen_rtx_fmt_ee (bypass_code, VOIDmode,
				       gen_rtx_REG (intcmp_mode, FLAGS_REG),
				       const0_rtx);
      if (second_code != NIL)
	*second_test = gen_rtx_fmt_ee (second_code, VOIDmode,
				       gen_rtx_REG (intcmp_mode, FLAGS_REG),
				       const0_rtx);
    }
  else
    {
      /* Sadness wrt reg-stack pops killing fpsr -- gotta get fnstsw first.  */
      tmp = gen_rtx_COMPARE (fpcmp_mode, op0, op1);
      tmp2 = gen_rtx_UNSPEC (HImode, gen_rtvec (1, tmp), UNSPEC_FNSTSW);
      if (!scratch)
	scratch = gen_reg_rtx (HImode);
      emit_insn (gen_rtx_SET (VOIDmode, scratch, tmp2));

      /* In the unordered case, we have to check C2 for NaN's, which
	 doesn't happen to work out to anything nice combination-wise.
	 So do some bit twiddling on the value we've got in AH to come
	 up with an appropriate set of condition codes.  */

      intcmp_mode = CCNOmode;
      switch (code)
	{
	case GT:
	case UNGT:
	  if (code == GT || !TARGET_IEEE_FP)
	    {
	      emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x45)));
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_andqi_ext_0 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_addqi_ext_1 (scratch, scratch, constm1_rtx));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x44)));
	      intcmp_mode = CCmode;
	      code = GEU;
	    }
	  break;
	case LT:
	case UNLT:
	  if (code == LT && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_0 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x01)));
	      intcmp_mode = CCmode;
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x01)));
	      code = NE;
	    }
	  break;
	case GE:
	case UNGE:
	  if (code == GE || !TARGET_IEEE_FP)
	    {
	      emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x05)));
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_andqi_ext_0 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_xorqi_cc_ext_1 (scratch, scratch,
					     GEN_INT (0x01)));
	      code = NE;
	    }
	  break;
	case LE:
	case UNLE:
	  if (code == LE && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_0 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_addqi_ext_1 (scratch, scratch, constm1_rtx));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x40)));
	      intcmp_mode = CCmode;
	      code = LTU;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x45)));
	      code = NE;
	    }
	  break;
	case EQ:
	case UNEQ:
	  if (code == EQ && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_0 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x40)));
	      intcmp_mode = CCmode;
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x40)));
	      code = NE;
	      break;
	    }
	  break;
	case NE:
	case LTGT:
	  if (code == NE && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_0 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_xorqi_cc_ext_1 (scratch, scratch,
					     GEN_INT (0x40)));
	      code = NE;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x40)));
	      code = EQ;
	    }
	  break;

	case UNORDERED:
	  emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x04)));
	  code = NE;
	  break;
	case ORDERED:
	  emit_insn (gen_testqi_ext_ccno_0 (scratch, GEN_INT (0x04)));
	  code = EQ;
	  break;

	default:
	  abort ();
	}
    }

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode,
			 gen_rtx_REG (intcmp_mode, FLAGS_REG),
			 const0_rtx);
}

rtx
ix86_expand_compare (code, second_test, bypass_test)
     enum rtx_code code;
     rtx *second_test, *bypass_test;
{
  rtx op0, op1, ret;
  op0 = ix86_compare_op0;
  op1 = ix86_compare_op1;

  if (second_test)
    *second_test = NULL_RTX;
  if (bypass_test)
    *bypass_test = NULL_RTX;

  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
    ret = ix86_expand_fp_compare (code, op0, op1, NULL_RTX,
				  second_test, bypass_test);
  else
    ret = ix86_expand_int_compare (code, op0, op1);

  return ret;
}

/* Return true if the CODE will result in nontrivial jump sequence.  */
bool
ix86_fp_jump_nontrivial_p (code)
    enum rtx_code code;
{
  enum rtx_code bypass_code, first_code, second_code;
  if (!TARGET_CMOVE)
    return true;
  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);
  return bypass_code != NIL || second_code != NIL;
}

void
ix86_expand_branch (code, label)
     enum rtx_code code;
     rtx label;
{
  rtx tmp;

  switch (GET_MODE (ix86_compare_op0))
    {
    case QImode:
    case HImode:
    case SImode:
      simple:
      tmp = ix86_expand_compare (code, NULL, NULL);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
      return;

    case SFmode:
    case DFmode:
    case XFmode:
    case TFmode:
      {
	rtvec vec;
	int use_fcomi;
	enum rtx_code bypass_code, first_code, second_code;

	code = ix86_prepare_fp_compare_args (code, &ix86_compare_op0,
					     &ix86_compare_op1);

	ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);

	/* Check whether we will use the natural sequence with one jump.  If
	   so, we can expand jump early.  Otherwise delay expansion by
	   creating compound insn to not confuse optimizers.  */
	if (bypass_code == NIL && second_code == NIL
	    && TARGET_CMOVE)
	  {
	    ix86_split_fp_branch (code, ix86_compare_op0, ix86_compare_op1,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx, NULL_RTX);
	  }
	else
	  {
	    tmp = gen_rtx_fmt_ee (code, VOIDmode,
				  ix86_compare_op0, ix86_compare_op1);
	    tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
					gen_rtx_LABEL_REF (VOIDmode, label),
					pc_rtx);
	    tmp = gen_rtx_SET (VOIDmode, pc_rtx, tmp);

	    use_fcomi = ix86_use_fcomi_compare (code);
	    vec = rtvec_alloc (3 + !use_fcomi);
	    RTVEC_ELT (vec, 0) = tmp;
	    RTVEC_ELT (vec, 1)
	      = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCFPmode, 18));
	    RTVEC_ELT (vec, 2)
	      = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCFPmode, 17));
	    if (! use_fcomi)
	      RTVEC_ELT (vec, 3)
		= gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (HImode));

	    emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, vec));
	  }
	return;
      }

    case DImode:
      if (TARGET_64BIT)
	goto simple;
      /* Expand DImode branch into multiple compare+branch.  */
      {
	rtx lo[2], hi[2], label2;
	enum rtx_code code1, code2, code3;

	if (CONSTANT_P (ix86_compare_op0) && ! CONSTANT_P (ix86_compare_op1))
	  {
	    tmp = ix86_compare_op0;
	    ix86_compare_op0 = ix86_compare_op1;
	    ix86_compare_op1 = tmp;
	    code = swap_condition (code);
	  }
	split_di (&ix86_compare_op0, 1, lo+0, hi+0);
	split_di (&ix86_compare_op1, 1, lo+1, hi+1);

	/* When comparing for equality, we can use (hi0^hi1)|(lo0^lo1) to
	   avoid two branches.  This costs one extra insn, so disable when
	   optimizing for size.  */

	if ((code == EQ || code == NE)
	    && (!optimize_size
	        || hi[1] == const0_rtx || lo[1] == const0_rtx))
	  {
	    rtx xor0, xor1;

	    xor1 = hi[0];
	    if (hi[1] != const0_rtx)
	      xor1 = expand_binop (SImode, xor_optab, xor1, hi[1],
				   NULL_RTX, 0, OPTAB_WIDEN);

	    xor0 = lo[0];
	    if (lo[1] != const0_rtx)
	      xor0 = expand_binop (SImode, xor_optab, xor0, lo[1],
				   NULL_RTX, 0, OPTAB_WIDEN);

	    tmp = expand_binop (SImode, ior_optab, xor1, xor0,
				NULL_RTX, 0, OPTAB_WIDEN);

	    ix86_compare_op0 = tmp;
	    ix86_compare_op1 = const0_rtx;
	    ix86_expand_branch (code, label);
	    return;
	  }

	/* Otherwise, if we are doing less-than or greater-or-equal-than,
	   op1 is a constant and the low word is zero, then we can just
	   examine the high word.  */

	if (GET_CODE (hi[1]) == CONST_INT && lo[1] == const0_rtx)
	  switch (code)
	    {
	    case LT: case LTU: case GE: case GEU:
	      ix86_compare_op0 = hi[0];
	      ix86_compare_op1 = hi[1];
	      ix86_expand_branch (code, label);
	      return;
	    default:
	      break;
	    }

	/* Otherwise, we need two or three jumps.  */

	label2 = gen_label_rtx ();

	code1 = code;
	code2 = swap_condition (code);
	code3 = unsigned_condition (code);

	switch (code)
	  {
	  case LT: case GT: case LTU: case GTU:
	    break;

	  case LE:   code1 = LT;  code2 = GT;  break;
	  case GE:   code1 = GT;  code2 = LT;  break;
	  case LEU:  code1 = LTU; code2 = GTU; break;
	  case GEU:  code1 = GTU; code2 = LTU; break;

	  case EQ:   code1 = NIL; code2 = NE;  break;
	  case NE:   code2 = NIL; break;

	  default:
	    abort ();
	  }

	/*
	 * a < b =>
	 *    if (hi(a) < hi(b)) goto true;
	 *    if (hi(a) > hi(b)) goto false;
	 *    if (lo(a) < lo(b)) goto true;
	 *  false:
	 */

	ix86_compare_op0 = hi[0];
	ix86_compare_op1 = hi[1];

	if (code1 != NIL)
	  ix86_expand_branch (code1, label);
	if (code2 != NIL)
	  ix86_expand_branch (code2, label2);

	ix86_compare_op0 = lo[0];
	ix86_compare_op1 = lo[1];
	ix86_expand_branch (code3, label);

	if (code2 != NIL)
	  emit_label (label2);
	return;
      }

    default:
      abort ();
    }
}

/* Split branch based on floating point condition.  */
void
ix86_split_fp_branch (code, op1, op2, target1, target2, tmp)
     enum rtx_code code;
     rtx op1, op2, target1, target2, tmp;
{
  rtx second, bypass;
  rtx label = NULL_RTX;
  rtx condition;
  int bypass_probability = -1, second_probability = -1, probability = -1;
  rtx i;

  if (target2 != pc_rtx)
    {
      rtx tmp = target2;
      code = reverse_condition_maybe_unordered (code);
      target2 = target1;
      target1 = tmp;
    }

  condition = ix86_expand_fp_compare (code, op1, op2,
				      tmp, &second, &bypass);

  if (split_branch_probability >= 0)
    {
      /* Distribute the probabilities across the jumps.
	 Assume the BYPASS and SECOND to be always test
	 for UNORDERED.  */
      probability = split_branch_probability;

      /* Value of 1 is low enough to make no need for probability
	 to be updated.  Later we may run some experiments and see
	 if unordered values are more frequent in practice.  */
      if (bypass)
	bypass_probability = 1;
      if (second)
	second_probability = 1;
    }
  if (bypass != NULL_RTX)
    {
      label = gen_label_rtx ();
      i = emit_jump_insn (gen_rtx_SET
			  (VOIDmode, pc_rtx,
			   gen_rtx_IF_THEN_ELSE (VOIDmode,
						 bypass,
						 gen_rtx_LABEL_REF (VOIDmode,
								    label),
						 pc_rtx)));
      if (bypass_probability >= 0)
	REG_NOTES (i)
	  = gen_rtx_EXPR_LIST (REG_BR_PROB,
			       GEN_INT (bypass_probability),
			       REG_NOTES (i));
    }
  i = emit_jump_insn (gen_rtx_SET
		      (VOIDmode, pc_rtx,
		       gen_rtx_IF_THEN_ELSE (VOIDmode,
					     condition, target1, target2)));
  if (probability >= 0)
    REG_NOTES (i)
      = gen_rtx_EXPR_LIST (REG_BR_PROB,
			   GEN_INT (probability),
			   REG_NOTES (i));
  if (second != NULL_RTX)
    {
      i = emit_jump_insn (gen_rtx_SET
			  (VOIDmode, pc_rtx,
			   gen_rtx_IF_THEN_ELSE (VOIDmode, second, target1,
						 target2)));
      if (second_probability >= 0)
	REG_NOTES (i)
	  = gen_rtx_EXPR_LIST (REG_BR_PROB,
			       GEN_INT (second_probability),
			       REG_NOTES (i));
    }
  if (label != NULL_RTX)
    emit_label (label);
}

int
ix86_expand_setcc (code, dest)
     enum rtx_code code;
     rtx dest;
{
  rtx ret, tmp, tmpreg;
  rtx second_test, bypass_test;

  if (GET_MODE (ix86_compare_op0) == DImode
      && !TARGET_64BIT)
    return 0; /* FAIL */

  if (GET_MODE (dest) != QImode)
    abort ();

  ret = ix86_expand_compare (code, &second_test, &bypass_test);
  PUT_MODE (ret, QImode);

  tmp = dest;
  tmpreg = dest;

  emit_insn (gen_rtx_SET (VOIDmode, tmp, ret));
  if (bypass_test || second_test)
    {
      rtx test = second_test;
      int bypass = 0;
      rtx tmp2 = gen_reg_rtx (QImode);
      if (bypass_test)
	{
	  if (second_test)
	    abort ();
	  test = bypass_test;
	  bypass = 1;
	  PUT_CODE (test, reverse_condition_maybe_unordered (GET_CODE (test)));
	}
      PUT_MODE (test, QImode);
      emit_insn (gen_rtx_SET (VOIDmode, tmp2, test));

      if (bypass)
	emit_insn (gen_andqi3 (tmp, tmpreg, tmp2));
      else
	emit_insn (gen_iorqi3 (tmp, tmpreg, tmp2));
    }

  return 1; /* DONE */
}

int
ix86_expand_int_movcc (operands)
     rtx operands[];
{
  enum rtx_code code = GET_CODE (operands[1]), compare_code;
  rtx compare_seq, compare_op;
  rtx second_test, bypass_test;
  enum machine_mode mode = GET_MODE (operands[0]);

  /* When the compare code is not LTU or GEU, we can not use sbbl case.
     In case comparsion is done with immediate, we can convert it to LTU or
     GEU by altering the integer.  */

  if ((code == LEU || code == GTU)
      && GET_CODE (ix86_compare_op1) == CONST_INT
      && mode != HImode
      && INTVAL (ix86_compare_op1) != -1
      /* For x86-64, the immediate field in the instruction is 32-bit
	 signed, so we can't increment a DImode value above 0x7fffffff.  */
      && (!TARGET_64BIT
	  || GET_MODE (ix86_compare_op0) != DImode
	  || INTVAL (ix86_compare_op1) != 0x7fffffff)
      && GET_CODE (operands[2]) == CONST_INT
      && GET_CODE (operands[3]) == CONST_INT)
    {
      if (code == LEU)
	code = LTU;
      else
	code = GEU;
      ix86_compare_op1 = gen_int_mode (INTVAL (ix86_compare_op1) + 1,
				       GET_MODE (ix86_compare_op0));
    }

  start_sequence ();
  compare_op = ix86_expand_compare (code, &second_test, &bypass_test);
  compare_seq = get_insns ();
  end_sequence ();

  compare_code = GET_CODE (compare_op);

  /* Don't attempt mode expansion here -- if we had to expand 5 or 6
     HImode insns, we'd be swallowed in word prefix ops.  */

  if (mode != HImode
      && (mode != DImode || TARGET_64BIT)
      && GET_CODE (operands[2]) == CONST_INT
      && GET_CODE (operands[3]) == CONST_INT)
    {
      rtx out = operands[0];
      HOST_WIDE_INT ct = INTVAL (operands[2]);
      HOST_WIDE_INT cf = INTVAL (operands[3]);
      HOST_WIDE_INT diff;

      if ((compare_code == LTU || compare_code == GEU)
	  && !second_test && !bypass_test)
	{
	  /* Detect overlap between destination and compare sources.  */
	  rtx tmp = out;

	  /* To simplify rest of code, restrict to the GEU case.  */
	  if (compare_code == LTU)
	    {
	      HOST_WIDE_INT tmp = ct;
	      ct = cf;
	      cf = tmp;
	      compare_code = reverse_condition (compare_code);
	      code = reverse_condition (code);
	    }
	  diff = ct - cf;

	  if (reg_overlap_mentioned_p (out, ix86_compare_op0)
	      || reg_overlap_mentioned_p (out, ix86_compare_op1))
	    tmp = gen_reg_rtx (mode);

	  emit_insn (compare_seq);
	  if (mode == DImode)
	    emit_insn (gen_x86_movdicc_0_m1_rex64 (tmp));
	  else
	    emit_insn (gen_x86_movsicc_0_m1 (tmp));

	  if (diff == 1)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * [addl dest, ct]
	       *
	       * Size 5 - 8.
	       */
	      if (ct)
	       	tmp = expand_simple_binop (mode, PLUS,
					   tmp, GEN_INT (ct),
					   tmp, 1, OPTAB_DIRECT);
	    }
	  else if (cf == -1)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * orl $ct, dest
	       *
	       * Size 8.
	       */
	      tmp = expand_simple_binop (mode, IOR,
					 tmp, GEN_INT (ct),
					 tmp, 1, OPTAB_DIRECT);
	    }
	  else if (diff == -1 && ct)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * notl dest
	       * [addl dest, cf]
	       *
	       * Size 8 - 11.
	       */
	      tmp = expand_simple_unop (mode, NOT, tmp, tmp, 1);
	      if (cf)
	       	tmp = expand_simple_binop (mode, PLUS,
					   tmp, GEN_INT (cf),
					   tmp, 1, OPTAB_DIRECT);
	    }
	  else
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * [notl dest]
	       * andl cf - ct, dest
	       * [addl dest, ct]
	       *
	       * Size 8 - 11.
	       */

	      if (cf == 0)
		{
		  cf = ct;
		  ct = 0;
		  tmp = expand_simple_unop (mode, NOT, tmp, tmp, 1);
		}

	      tmp = expand_simple_binop (mode, AND,
					 tmp,
					 gen_int_mode (cf - ct, mode),
					 tmp, 1, OPTAB_DIRECT);
	      if (ct)
	       	tmp = expand_simple_binop (mode, PLUS,
					   tmp, GEN_INT (ct),
					   tmp, 1, OPTAB_DIRECT);
	    }

	  if (tmp != out)
	    emit_move_insn (out, tmp);

	  return 1; /* DONE */
	}

      diff = ct - cf;
      if (diff < 0)
	{
	  HOST_WIDE_INT tmp;
	  tmp = ct, ct = cf, cf = tmp;
	  diff = -diff;
	  if (FLOAT_MODE_P (GET_MODE (ix86_compare_op0)))
	    {
	      /* We may be reversing unordered compare to normal compare, that
		 is not valid in general (we may convert non-trapping condition
		 to trapping one), however on i386 we currently emit all
		 comparisons unordered.  */
	      compare_code = reverse_condition_maybe_unordered (compare_code);
	      code = reverse_condition_maybe_unordered (code);
	    }
	  else
	    {
	      compare_code = reverse_condition (compare_code);
	      code = reverse_condition (code);
	    }
	}

      compare_code = NIL;
      if (GET_MODE_CLASS (GET_MODE (ix86_compare_op0)) == MODE_INT
	  && GET_CODE (ix86_compare_op1) == CONST_INT)
	{
	  if (ix86_compare_op1 == const0_rtx
	      && (code == LT || code == GE))
	    compare_code = code;
	  else if (ix86_compare_op1 == constm1_rtx)
	    {
	      if (code == LE)
		compare_code = LT;
	      else if (code == GT)
		compare_code = GE;
	    }
	}

      /* Optimize dest = (op0 < 0) ? -1 : cf.  */
      if (compare_code != NIL
	  && GET_MODE (ix86_compare_op0) == GET_MODE (out)
	  && (cf == -1 || ct == -1))
	{
	  /* If lea code below could be used, only optimize
	     if it results in a 2 insn sequence.  */

	  if (! (diff == 1 || diff == 2 || diff == 4 || diff == 8
		 || diff == 3 || diff == 5 || diff == 9)
	      || (compare_code == LT && ct == -1)
	      || (compare_code == GE && cf == -1))
	    {
	      /*
	       * notl op1	(if necessary)
	       * sarl $31, op1
	       * orl cf, op1
	       */
	      if (ct != -1)
		{
		  cf = ct;
	  	  ct = -1;
		  code = reverse_condition (code);
		}

	      out = emit_store_flag (out, code, ix86_compare_op0,
				     ix86_compare_op1, VOIDmode, 0, -1);

	      out = expand_simple_binop (mode, IOR,
					 out, GEN_INT (cf),
					 out, 1, OPTAB_DIRECT);
	      if (out != operands[0])
		emit_move_insn (operands[0], out);

	      return 1; /* DONE */
	    }
	}

      if ((diff == 1 || diff == 2 || diff == 4 || diff == 8
	   || diff == 3 || diff == 5 || diff == 9)
	  && (mode != DImode || x86_64_sign_extended_value (GEN_INT (cf))))
	{
	  /*
	   * xorl dest,dest
	   * cmpl op1,op2
	   * setcc dest
	   * lea cf(dest*(ct-cf)),dest
	   *
	   * Size 14.
	   *
	   * This also catches the degenerate setcc-only case.
	   */

	  rtx tmp;
	  int nops;

	  out = emit_store_flag (out, code, ix86_compare_op0,
				 ix86_compare_op1, VOIDmode, 0, 1);

	  nops = 0;
	  /* On x86_64 the lea instruction operates on Pmode, so we need
	     to get arithmetics done in proper mode to match.  */
	  if (diff == 1)
	    tmp = copy_rtx (out);
	  else
	    {
	      rtx out1;
	      out1 = copy_rtx (out);
	      tmp = gen_rtx_MULT (mode, out1, GEN_INT (diff & ~1));
	      nops++;
	      if (diff & 1)
		{
		  tmp = gen_rtx_PLUS (mode, tmp, out1);
		  nops++;
		}
	    }
	  if (cf != 0)
	    {
	      tmp = gen_rtx_PLUS (mode, tmp, GEN_INT (cf));
	      nops++;
	    }
	  if (tmp != out
	      && (GET_CODE (tmp) != SUBREG || SUBREG_REG (tmp) != out))
	    {
	      if (nops == 1)
		out = force_operand (tmp, copy_rtx (out));
	      else
		emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (out), copy_rtx (tmp)));
	    }
	  if (out != operands[0])
	    emit_move_insn (operands[0], copy_rtx (out));

	  return 1; /* DONE */
	}

      /*
       * General case:			Jumpful:
       *   xorl dest,dest		cmpl op1, op2
       *   cmpl op1, op2		movl ct, dest
       *   setcc dest			jcc 1f
       *   decl dest			movl cf, dest
       *   andl (cf-ct),dest		1:
       *   addl ct,dest
       *
       * Size 20.			Size 14.
       *
       * This is reasonably steep, but branch mispredict costs are
       * high on modern cpus, so consider failing only if optimizing
       * for space.
       *
       * %%% Parameterize branch_cost on the tuning architecture, then
       * use that.  The 80386 couldn't care less about mispredicts.
       */

      if (!optimize_size && !TARGET_CMOVE)
	{
	  if (cf == 0)
	    {
	      cf = ct;
	      ct = 0;
	      if (FLOAT_MODE_P (GET_MODE (ix86_compare_op0)))
		/* We may be reversing unordered compare to normal compare,
		   that is not valid in general (we may convert non-trapping
		   condition to trapping one), however on i386 we currently
		   emit all comparisons unordered.  */
		code = reverse_condition_maybe_unordered (code);
	      else
		{
		  code = reverse_condition (code);
		  if (compare_code != NIL)
		    compare_code = reverse_condition (compare_code);
		}
	    }

	  if (compare_code != NIL)
	    {
	      /* notl op1	(if needed)
		 sarl $31, op1
		 andl (cf-ct), op1
	 	 addl ct, op1

		 For x < 0 (resp. x <= -1) there will be no notl,
		 so if possible swap the constants to get rid of the
		 complement.
		 True/false will be -1/0 while code below (store flag
		 followed by decrement) is 0/-1, so the constants need
		 to be exchanged once more.  */

	      if (compare_code == GE || !cf)
		{
	  	  code = reverse_condition (code);
		  compare_code = LT;
		}
	      else
		{
		  HOST_WIDE_INT tmp = cf;
	  	  cf = ct;
		  ct = tmp;
		}

	      out = emit_store_flag (out, code, ix86_compare_op0,
				     ix86_compare_op1, VOIDmode, 0, -1);
	    }
	  else
	    {
	      out = emit_store_flag (out, code, ix86_compare_op0,
				     ix86_compare_op1, VOIDmode, 0, 1);

	      out = expand_simple_binop (mode, PLUS, out, constm1_rtx,
					 out, 1, OPTAB_DIRECT);
	    }

	  out = expand_simple_binop (mode, AND, out,
				     gen_int_mode (cf - ct, mode),
				     out, 1, OPTAB_DIRECT);
	  if (ct)
	    out = expand_simple_binop (mode, PLUS, out, GEN_INT (ct),
				       out, 1, OPTAB_DIRECT);
	  if (out != operands[0])
	    emit_move_insn (operands[0], out);

	  return 1; /* DONE */
	}
    }

  if (!TARGET_CMOVE)
    {
      /* Try a few things more with specific constants and a variable.  */

      optab op;
      rtx var, orig_out, out, tmp;

      if (optimize_size)
	return 0; /* FAIL */

      /* If one of the two operands is an interesting constant, load a
	 constant with the above and mask it in with a logical operation.  */

      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  var = operands[3];
	  if (INTVAL (operands[2]) == 0)
	    operands[3] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[2]) == -1)
	    operands[3] = const0_rtx, op = ior_optab;
	  else
	    return 0; /* FAIL */
	}
      else if (GET_CODE (operands[3]) == CONST_INT)
	{
	  var = operands[2];
	  if (INTVAL (operands[3]) == 0)
	    operands[2] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[3]) == -1)
	    operands[2] = const0_rtx, op = ior_optab;
	  else
	    return 0; /* FAIL */
	}
      else
        return 0; /* FAIL */

      orig_out = operands[0];
      tmp = gen_reg_rtx (mode);
      operands[0] = tmp;

      /* Recurse to get the constant loaded.  */
      if (ix86_expand_int_movcc (operands) == 0)
        return 0; /* FAIL */

      /* Mask in the interesting variable.  */
      out = expand_binop (mode, op, var, tmp, orig_out, 0,
			  OPTAB_WIDEN);
      if (out != orig_out)
	emit_move_insn (orig_out, out);

      return 1; /* DONE */
    }

  /*
   * For comparison with above,
   *
   * movl cf,dest
   * movl ct,tmp
   * cmpl op1,op2
   * cmovcc tmp,dest
   *
   * Size 15.
   */

  if (! nonimmediate_operand (operands[2], mode))
    operands[2] = force_reg (mode, operands[2]);
  if (! nonimmediate_operand (operands[3], mode))
    operands[3] = force_reg (mode, operands[3]);

  if (bypass_test && reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      rtx tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, operands[3]);
      operands[3] = tmp;
    }
  if (second_test && reg_overlap_mentioned_p (operands[0], operands[2]))
    {
      rtx tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, operands[2]);
      operands[2] = tmp;
    }
  if (! register_operand (operands[2], VOIDmode)
      && ! register_operand (operands[3], VOIDmode))
    operands[2] = force_reg (mode, operands[2]);

  emit_insn (compare_seq);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (mode,
						compare_op, operands[2],
						operands[3])));
  if (bypass_test)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_IF_THEN_ELSE (mode,
				  bypass_test,
				  operands[3],
				  operands[0])));
  if (second_test)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_IF_THEN_ELSE (mode,
				  second_test,
				  operands[2],
				  operands[0])));

  return 1; /* DONE */
}

int
ix86_expand_fp_movcc (operands)
     rtx operands[];
{
  enum rtx_code code;
  rtx tmp;
  rtx compare_op, second_test, bypass_test;

  /* For SF/DFmode conditional moves based on comparisons
     in same mode, we may want to use SSE min/max instructions.  */
  if (((TARGET_SSE_MATH && GET_MODE (operands[0]) == SFmode)
       || (TARGET_SSE2 && TARGET_SSE_MATH && GET_MODE (operands[0]) == DFmode))
      && GET_MODE (ix86_compare_op0) == GET_MODE (operands[0])
      /* The SSE comparisons does not support the LTGT/UNEQ pair.  */
      && (!TARGET_IEEE_FP
	  || (GET_CODE (operands[1]) != LTGT && GET_CODE (operands[1]) != UNEQ))
      /* We may be called from the post-reload splitter.  */
      && (!REG_P (operands[0])
	  || SSE_REG_P (operands[0])
	  || REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER))
    {
      rtx op0 = ix86_compare_op0, op1 = ix86_compare_op1;
      code = GET_CODE (operands[1]);

      /* See if we have (cross) match between comparison operands and
         conditional move operands.  */
      if (rtx_equal_p (operands[2], op1))
	{
	  rtx tmp = op0;
	  op0 = op1;
	  op1 = tmp;
	  code = reverse_condition_maybe_unordered (code);
	}
      if (rtx_equal_p (operands[2], op0) && rtx_equal_p (operands[3], op1))
	{
	  /* Check for min operation.  */
	  if (code == LT)
	    {
	       operands[0] = force_reg (GET_MODE (operands[0]), operands[0]);
	       if (memory_operand (op0, VOIDmode))
		 op0 = force_reg (GET_MODE (operands[0]), op0);
	       if (GET_MODE (operands[0]) == SFmode)
		 emit_insn (gen_minsf3 (operands[0], op0, op1));
	       else
		 emit_insn (gen_mindf3 (operands[0], op0, op1));
	       return 1;
	    }
	  /* Check for max operation.  */
	  if (code == GT)
	    {
	       operands[0] = force_reg (GET_MODE (operands[0]), operands[0]);
	       if (memory_operand (op0, VOIDmode))
		 op0 = force_reg (GET_MODE (operands[0]), op0);
	       if (GET_MODE (operands[0]) == SFmode)
		 emit_insn (gen_maxsf3 (operands[0], op0, op1));
	       else
		 emit_insn (gen_maxdf3 (operands[0], op0, op1));
	       return 1;
	    }
	}
      /* Manage condition to be sse_comparison_operator.  In case we are
	 in non-ieee mode, try to canonicalize the destination operand
	 to be first in the comparison - this helps reload to avoid extra
	 moves.  */
      if (!sse_comparison_operator (operands[1], VOIDmode)
	  || (rtx_equal_p (operands[0], ix86_compare_op1) && !TARGET_IEEE_FP))
	{
	  rtx tmp = ix86_compare_op0;
	  ix86_compare_op0 = ix86_compare_op1;
	  ix86_compare_op1 = tmp;
	  operands[1] = gen_rtx_fmt_ee (swap_condition (GET_CODE (operands[1])),
					VOIDmode, ix86_compare_op0,
					ix86_compare_op1);
	}
      /* Similary try to manage result to be first operand of conditional
	 move. We also don't support the NE comparison on SSE, so try to
	 avoid it.  */
      if ((rtx_equal_p (operands[0], operands[3])
	   && (!TARGET_IEEE_FP || GET_CODE (operands[1]) != EQ))
	  || (GET_CODE (operands[1]) == NE && TARGET_IEEE_FP))
	{
	  rtx tmp = operands[2];
	  operands[2] = operands[3];
	  operands[3] = tmp;
	  operands[1] = gen_rtx_fmt_ee (reverse_condition_maybe_unordered
					  (GET_CODE (operands[1])),
					VOIDmode, ix86_compare_op0,
					ix86_compare_op1);
	}
      if (GET_MODE (operands[0]) == SFmode)
	emit_insn (gen_sse_movsfcc (operands[0], operands[1],
				    operands[2], operands[3],
				    ix86_compare_op0, ix86_compare_op1));
      else
	emit_insn (gen_sse_movdfcc (operands[0], operands[1],
				    operands[2], operands[3],
				    ix86_compare_op0, ix86_compare_op1));
      return 1;
    }

  /* The floating point conditional move instructions don't directly
     support conditions resulting from a signed integer comparison.  */

  code = GET_CODE (operands[1]);
  compare_op = ix86_expand_compare (code, &second_test, &bypass_test);

  /* The floating point conditional move instructions don't directly
     support signed integer comparisons.  */

  if (!fcmov_comparison_operator (compare_op, VOIDmode))
    {
      if (second_test != NULL || bypass_test != NULL)
	abort ();
      tmp = gen_reg_rtx (QImode);
      ix86_expand_setcc (code, tmp);
      code = NE;
      ix86_compare_op0 = tmp;
      ix86_compare_op1 = const0_rtx;
      compare_op = ix86_expand_compare (code,  &second_test, &bypass_test);
    }
  if (bypass_test && reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      tmp = gen_reg_rtx (GET_MODE (operands[0]));
      emit_move_insn (tmp, operands[3]);
      operands[3] = tmp;
    }
  if (second_test && reg_overlap_mentioned_p (operands[0], operands[2]))
    {
      tmp = gen_reg_rtx (GET_MODE (operands[0]));
      emit_move_insn (tmp, operands[2]);
      operands[2] = tmp;
    }

  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
				compare_op,
				operands[2],
				operands[3])));
  if (bypass_test)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
				  bypass_test,
				  operands[3],
				  operands[0])));
  if (second_test)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_IF_THEN_ELSE (GET_MODE (operands[0]),
				  second_test,
				  operands[2],
				  operands[0])));

  return 1;
}

/* Split operands 0 and 1 into SImode parts.  Similar to split_di, but
   works for floating pointer parameters and nonoffsetable memories.
   For pushes, it returns just stack offsets; the values will be saved
   in the right order.  Maximally three parts are generated.  */

static int
ix86_split_to_parts (operand, parts, mode)
     rtx operand;
     rtx *parts;
     enum machine_mode mode;
{
  int size;

  if (!TARGET_64BIT)
    size = mode == TFmode ? 3 : (GET_MODE_SIZE (mode) / 4);
  else
    size = (GET_MODE_SIZE (mode) + 4) / 8;

  if (GET_CODE (operand) == REG && MMX_REGNO_P (REGNO (operand)))
    abort ();
  if (size < 2 || size > 3)
    abort ();

  /* Optimize constant pool reference to immediates.  This is used by fp
     moves, that force all constants to memory to allow combining.  */
  if (GET_CODE (operand) == MEM && RTX_UNCHANGING_P (operand))
    {
      rtx tmp = maybe_get_pool_constant (operand);
      if (tmp)
	operand = tmp;
    }

  if (GET_CODE (operand) == MEM && !offsettable_memref_p (operand))
    {
      /* The only non-offsetable memories we handle are pushes.  */
      if (! push_operand (operand, VOIDmode))
	abort ();

      operand = copy_rtx (operand);
      PUT_MODE (operand, Pmode);
      parts[0] = parts[1] = parts[2] = operand;
    }
  else if (!TARGET_64BIT)
    {
      if (mode == DImode)
	split_di (&operand, 1, &parts[0], &parts[1]);
      else
	{
	  if (REG_P (operand))
	    {
	      if (!reload_completed)
		abort ();
	      parts[0] = gen_rtx_REG (SImode, REGNO (operand) + 0);
	      parts[1] = gen_rtx_REG (SImode, REGNO (operand) + 1);
	      if (size == 3)
		parts[2] = gen_rtx_REG (SImode, REGNO (operand) + 2);
	    }
	  else if (offsettable_memref_p (operand))
	    {
	      operand = adjust_address (operand, SImode, 0);
	      parts[0] = operand;
	      parts[1] = adjust_address (operand, SImode, 4);
	      if (size == 3)
		parts[2] = adjust_address (operand, SImode, 8);
	    }
	  else if (GET_CODE (operand) == CONST_DOUBLE)
	    {
	      REAL_VALUE_TYPE r;
	      long l[4];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
	      switch (mode)
		{
		case XFmode:
		case TFmode:
		  REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
		  parts[2] = gen_int_mode (l[2], SImode);
		  break;
		case DFmode:
		  REAL_VALUE_TO_TARGET_DOUBLE (r, l);
		  break;
		default:
		  abort ();
		}
	      parts[1] = gen_int_mode (l[1], SImode);
	      parts[0] = gen_int_mode (l[0], SImode);
	    }
	  else
	    abort ();
	}
    }
  else
    {
      if (mode == TImode)
	split_ti (&operand, 1, &parts[0], &parts[1]);
      if (mode == XFmode || mode == TFmode)
	{
	  if (REG_P (operand))
	    {
	      if (!reload_completed)
		abort ();
	      parts[0] = gen_rtx_REG (DImode, REGNO (operand) + 0);
	      parts[1] = gen_rtx_REG (SImode, REGNO (operand) + 1);
	    }
	  else if (offsettable_memref_p (operand))
	    {
	      operand = adjust_address (operand, DImode, 0);
	      parts[0] = operand;
	      parts[1] = adjust_address (operand, SImode, 8);
	    }
	  else if (GET_CODE (operand) == CONST_DOUBLE)
	    {
	      REAL_VALUE_TYPE r;
	      long l[3];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
	      REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
	      /* Do not use shift by 32 to avoid warning on 32bit systems.  */
	      if (HOST_BITS_PER_WIDE_INT >= 64)
	        parts[0]
		  = gen_int_mode
		      ((l[0] & (((HOST_WIDE_INT) 2 << 31) - 1))
		       + ((((HOST_WIDE_INT) l[1]) << 31) << 1),
		       DImode);
	      else
	        parts[0] = immed_double_const (l[0], l[1], DImode);
	      parts[1] = gen_int_mode (l[2], SImode);
	    }
	  else
	    abort ();
	}
    }

  return size;
}

/* Emit insns to perform a move or push of DI, DF, and XF values.
   Return false when normal moves are needed; true when all required
   insns have been emitted.  Operands 2-4 contain the input values
   int the correct order; operands 5-7 contain the output values.  */

void
ix86_split_long_move (operands)
     rtx operands[];
{
  rtx part[2][3];
  int nparts;
  int push = 0;
  int collisions = 0;
  enum machine_mode mode = GET_MODE (operands[0]);

  /* The DFmode expanders may ask us to move double.
     For 64bit target this is single move.  By hiding the fact
     here we simplify i386.md splitters.  */
  if (GET_MODE_SIZE (GET_MODE (operands[0])) == 8 && TARGET_64BIT)
    {
      /* Optimize constant pool reference to immediates.  This is used by
	 fp moves, that force all constants to memory to allow combining.  */

      if (GET_CODE (operands[1]) == MEM
	  && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0)))
	operands[1] = get_pool_constant (XEXP (operands[1], 0));
      if (push_operand (operands[0], VOIDmode))
	{
	  operands[0] = copy_rtx (operands[0]);
	  PUT_MODE (operands[0], Pmode);
	}
      else
        operands[0] = gen_lowpart (DImode, operands[0]);
      operands[1] = gen_lowpart (DImode, operands[1]);
      emit_move_insn (operands[0], operands[1]);
      return;
    }

  /* The only non-offsettable memory we handle is push.  */
  if (push_operand (operands[0], VOIDmode))
    push = 1;
  else if (GET_CODE (operands[0]) == MEM
	   && ! offsettable_memref_p (operands[0]))
    abort ();

  nparts = ix86_split_to_parts (operands[1], part[1], GET_MODE (operands[0]));
  ix86_split_to_parts (operands[0], part[0], GET_MODE (operands[0]));

  /* When emitting push, take care for source operands on the stack.  */
  if (push && GET_CODE (operands[1]) == MEM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    {
      if (nparts == 3)
	part[1][1] = change_address (part[1][1], GET_MODE (part[1][1]),
				     XEXP (part[1][2], 0));
      part[1][0] = change_address (part[1][0], GET_MODE (part[1][0]),
				   XEXP (part[1][1], 0));
    }

  /* We need to do copy in the right order in case an address register
     of the source overlaps the destination.  */
  if (REG_P (part[0][0]) && GET_CODE (part[1][0]) == MEM)
    {
      if (reg_overlap_mentioned_p (part[0][0], XEXP (part[1][0], 0)))
	collisions++;
      if (reg_overlap_mentioned_p (part[0][1], XEXP (part[1][0], 0)))
	collisions++;
      if (nparts == 3
	  && reg_overlap_mentioned_p (part[0][2], XEXP (part[1][0], 0)))
	collisions++;

      /* Collision in the middle part can be handled by reordering.  */
      if (collisions == 1 && nparts == 3
	  && reg_overlap_mentioned_p (part[0][1], XEXP (part[1][0], 0)))
	{
	  rtx tmp;
	  tmp = part[0][1]; part[0][1] = part[0][2]; part[0][2] = tmp;
	  tmp = part[1][1]; part[1][1] = part[1][2]; part[1][2] = tmp;
	}

      /* If there are more collisions, we can't handle it by reordering.
	 Do an lea to the last part and use only one colliding move.  */
      else if (collisions > 1)
	{
	  rtx base;

	  collisions = 1;

	  base = part[0][nparts - 1];

	  /* Handle the case when the last part isn't valid for lea.
	     Happens in 64-bit mode storing the 12-byte XFmode.  */
	  if (GET_MODE (base) != Pmode)
	    base = gen_rtx_REG (Pmode, REGNO (base));

	  emit_insn (gen_rtx_SET (VOIDmode, base, XEXP (part[1][0], 0)));
	  part[1][0] = replace_equiv_address (part[1][0], base);
	  part[1][1] = replace_equiv_address (part[1][1],
				      plus_constant (base, UNITS_PER_WORD));
	  if (nparts == 3)
	    part[1][2] = replace_equiv_address (part[1][2],
				      plus_constant (base, 8));
	}
    }

  if (push)
    {
      if (!TARGET_64BIT)
	{
	  if (nparts == 3)
	    {
	      /* We use only first 12 bytes of TFmode value, but for pushing we
		 are required to adjust stack as if we were pushing real 16byte
		 value.  */
	      if (mode == TFmode && !TARGET_64BIT)
		emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				       GEN_INT (-4)));
	      emit_move_insn (part[0][2], part[1][2]);
	    }
	}
      else
	{
	  /* In 64bit mode we don't have 32bit push available.  In case this is
	     register, it is OK - we will just use larger counterpart.  We also
	     retype memory - these comes from attempt to avoid REX prefix on
	     moving of second half of TFmode value.  */
	  if (GET_MODE (part[1][1]) == SImode)
	    {
	      if (GET_CODE (part[1][1]) == MEM)
		part[1][1] = adjust_address (part[1][1], DImode, 0);
	      else if (REG_P (part[1][1]))
		part[1][1] = gen_rtx_REG (DImode, REGNO (part[1][1]));
	      else
		abort ();
	      if (GET_MODE (part[1][0]) == SImode)
		part[1][0] = part[1][1];
	    }
	}
      emit_move_insn (part[0][1], part[1][1]);
      emit_move_insn (part[0][0], part[1][0]);
      return;
    }

  /* Choose correct order to not overwrite the source before it is copied.  */
  if ((REG_P (part[0][0])
       && REG_P (part[1][1])
       && (REGNO (part[0][0]) == REGNO (part[1][1])
	   || (nparts == 3
	       && REGNO (part[0][0]) == REGNO (part[1][2]))))
      || (collisions > 0
	  && reg_overlap_mentioned_p (part[0][0], XEXP (part[1][0], 0))))
    {
      if (nparts == 3)
	{
	  operands[2] = part[0][2];
	  operands[3] = part[0][1];
	  operands[4] = part[0][0];
	  operands[5] = part[1][2];
	  operands[6] = part[1][1];
	  operands[7] = part[1][0];
	}
      else
	{
	  operands[2] = part[0][1];
	  operands[3] = part[0][0];
	  operands[5] = part[1][1];
	  operands[6] = part[1][0];
	}
    }
  else
    {
      if (nparts == 3)
	{
	  operands[2] = part[0][0];
	  operands[3] = part[0][1];
	  operands[4] = part[0][2];
	  operands[5] = part[1][0];
	  operands[6] = part[1][1];
	  operands[7] = part[1][2];
	}
      else
	{
	  operands[2] = part[0][0];
	  operands[3] = part[0][1];
	  operands[5] = part[1][0];
	  operands[6] = part[1][1];
	}
    }
  emit_move_insn (operands[2], operands[5]);
  emit_move_insn (operands[3], operands[6]);
  if (nparts == 3)
    emit_move_insn (operands[4], operands[7]);

  return;
}

void
ix86_split_ashldi (operands, scratch)
     rtx *operands, scratch;
{
  rtx low[2], high[2];
  int count;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      split_di (operands, 2, low, high);
      count = INTVAL (operands[2]) & 63;

      if (count >= 32)
	{
	  emit_move_insn (high[0], low[1]);
	  emit_move_insn (low[0], const0_rtx);

	  if (count > 32)
	    emit_insn (gen_ashlsi3 (high[0], high[0], GEN_INT (count - 32)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_x86_shld_1 (high[0], low[0], GEN_INT (count)));
	  emit_insn (gen_ashlsi3 (low[0], low[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_di (operands, 1, low, high);

      emit_insn (gen_x86_shld_1 (high[0], low[0], operands[2]));
      emit_insn (gen_ashlsi3 (low[0], low[0], operands[2]));

      if (TARGET_CMOVE && (! no_new_pseudos || scratch))
	{
	  if (! no_new_pseudos)
	    scratch = force_reg (SImode, const0_rtx);
	  else
	    emit_move_insn (scratch, const0_rtx);

	  emit_insn (gen_x86_shift_adj_1 (high[0], low[0], operands[2],
					  scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2 (high[0], low[0], operands[2]));
    }
}

void
ix86_split_ashrdi (operands, scratch)
     rtx *operands, scratch;
{
  rtx low[2], high[2];
  int count;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      split_di (operands, 2, low, high);
      count = INTVAL (operands[2]) & 63;

      if (count >= 32)
	{
	  emit_move_insn (low[0], high[1]);

	  if (! reload_completed)
	    emit_insn (gen_ashrsi3 (high[0], low[0], GEN_INT (31)));
	  else
	    {
	      emit_move_insn (high[0], low[0]);
	      emit_insn (gen_ashrsi3 (high[0], high[0], GEN_INT (31)));
	    }

	  if (count > 32)
	    emit_insn (gen_ashrsi3 (low[0], low[0], GEN_INT (count - 32)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_x86_shrd_1 (low[0], high[0], GEN_INT (count)));
	  emit_insn (gen_ashrsi3 (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_di (operands, 1, low, high);

      emit_insn (gen_x86_shrd_1 (low[0], high[0], operands[2]));
      emit_insn (gen_ashrsi3 (high[0], high[0], operands[2]));

      if (TARGET_CMOVE && (! no_new_pseudos || scratch))
	{
	  if (! no_new_pseudos)
	    scratch = gen_reg_rtx (SImode);
	  emit_move_insn (scratch, high[0]);
	  emit_insn (gen_ashrsi3 (scratch, scratch, GEN_INT (31)));
	  emit_insn (gen_x86_shift_adj_1 (low[0], high[0], operands[2],
					  scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_3 (low[0], high[0], operands[2]));
    }
}

void
ix86_split_lshrdi (operands, scratch)
     rtx *operands, scratch;
{
  rtx low[2], high[2];
  int count;

  if (GET_CODE (operands[2]) == CONST_INT)
    {
      split_di (operands, 2, low, high);
      count = INTVAL (operands[2]) & 63;

      if (count >= 32)
	{
	  emit_move_insn (low[0], high[1]);
	  emit_move_insn (high[0], const0_rtx);

	  if (count > 32)
	    emit_insn (gen_lshrsi3 (low[0], low[0], GEN_INT (count - 32)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn (gen_x86_shrd_1 (low[0], high[0], GEN_INT (count)));
	  emit_insn (gen_lshrsi3 (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_di (operands, 1, low, high);

      emit_insn (gen_x86_shrd_1 (low[0], high[0], operands[2]));
      emit_insn (gen_lshrsi3 (high[0], high[0], operands[2]));

      /* Heh.  By reversing the arguments, we can reuse this pattern.  */
      if (TARGET_CMOVE && (! no_new_pseudos || scratch))
	{
	  if (! no_new_pseudos)
	    scratch = force_reg (SImode, const0_rtx);
	  else
	    emit_move_insn (scratch, const0_rtx);

	  emit_insn (gen_x86_shift_adj_1 (low[0], high[0], operands[2],
					  scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2 (low[0], high[0], operands[2]));
    }
}

/* Helper function for the string operations below.  Dest VARIABLE whether
   it is aligned to VALUE bytes.  If true, jump to the label.  */
static rtx
ix86_expand_aligntest (variable, value)
     rtx variable;
     int value;
{
  rtx label = gen_label_rtx ();
  rtx tmpcount = gen_reg_rtx (GET_MODE (variable));
  if (GET_MODE (variable) == DImode)
    emit_insn (gen_anddi3 (tmpcount, variable, GEN_INT (value)));
  else
    emit_insn (gen_andsi3 (tmpcount, variable, GEN_INT (value)));
  emit_cmp_and_jump_insns (tmpcount, const0_rtx, EQ, 0, GET_MODE (variable),
			   1, label);
  return label;
}

/* Adjust COUNTER by the VALUE.  */
static void
ix86_adjust_counter (countreg, value)
     rtx countreg;
     HOST_WIDE_INT value;
{
  if (GET_MODE (countreg) == DImode)
    emit_insn (gen_adddi3 (countreg, countreg, GEN_INT (-value)));
  else
    emit_insn (gen_addsi3 (countreg, countreg, GEN_INT (-value)));
}

/* Zero extend possibly SImode EXP to Pmode register.  */
rtx
ix86_zero_extend_to_Pmode (exp)
   rtx exp;
{
  rtx r;
  if (GET_MODE (exp) == VOIDmode)
    return force_reg (Pmode, exp);
  if (GET_MODE (exp) == Pmode)
    return copy_to_mode_reg (Pmode, exp);
  r = gen_reg_rtx (Pmode);
  emit_insn (gen_zero_extendsidi2 (r, exp));
  return r;
}

/* Expand string move (memcpy) operation.  Use i386 string operations when
   profitable.  expand_clrstr contains similar code.  */
int
ix86_expand_movstr (dst, src, count_exp, align_exp)
     rtx dst, src, count_exp, align_exp;
{
  rtx srcreg, destreg, countreg;
  enum machine_mode counter_mode;
  HOST_WIDE_INT align = 0;
  unsigned HOST_WIDE_INT count = 0;
  rtx insns;

  start_sequence ();

  if (GET_CODE (align_exp) == CONST_INT)
    align = INTVAL (align_exp);

  /* This simple hack avoids all inlining code and simplifies code below.  */
  if (!TARGET_ALIGN_STRINGOPS)
    align = 64;

  if (GET_CODE (count_exp) == CONST_INT)
    count = INTVAL (count_exp);

  /* Figure out proper mode for counter.  For 32bits it is always SImode,
     for 64bits use SImode when possible, otherwise DImode.
     Set count to number of bytes copied when known at compile time.  */
  if (!TARGET_64BIT || GET_MODE (count_exp) == SImode
      || x86_64_zero_extended_value (count_exp))
    counter_mode = SImode;
  else
    counter_mode = DImode;

  if (counter_mode != SImode && counter_mode != DImode)
    abort ();

  destreg = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  srcreg = copy_to_mode_reg (Pmode, XEXP (src, 0));

  emit_insn (gen_cld ());

  /* When optimizing for size emit simple rep ; movsb instruction for
     counts not divisible by 4.  */

  if ((!optimize || optimize_size) && (count == 0 || (count & 0x03)))
    {
      countreg = ix86_zero_extend_to_Pmode (count_exp);
      if (TARGET_64BIT)
	emit_insn (gen_rep_movqi_rex64 (destreg, srcreg, countreg,
				        destreg, srcreg, countreg));
      else
	emit_insn (gen_rep_movqi (destreg, srcreg, countreg,
				  destreg, srcreg, countreg));
    }

  /* For constant aligned (or small unaligned) copies use rep movsl
     followed by code copying the rest.  For PentiumPro ensure 8 byte
     alignment to allow rep movsl acceleration.  */

  else if (count != 0
	   && (align >= 8
	       || (!TARGET_PENTIUMPRO && !TARGET_64BIT && align >= 4)
	       || optimize_size || count < (unsigned int) 64))
    {
      int size = TARGET_64BIT && !optimize_size ? 8 : 4;
      if (count & ~(size - 1))
	{
	  countreg = copy_to_mode_reg (counter_mode,
				       GEN_INT ((count >> (size == 4 ? 2 : 3))
						& (TARGET_64BIT ? -1 : 0x3fffffff)));
	  countreg = ix86_zero_extend_to_Pmode (countreg);
	  if (size == 4)
	    {
	      if (TARGET_64BIT)
		emit_insn (gen_rep_movsi_rex64 (destreg, srcreg, countreg,
					        destreg, srcreg, countreg));
	      else
		emit_insn (gen_rep_movsi (destreg, srcreg, countreg,
					  destreg, srcreg, countreg));
	    }
	  else
	    emit_insn (gen_rep_movdi_rex64 (destreg, srcreg, countreg,
					    destreg, srcreg, countreg));
	}
      if (size == 8 && (count & 0x04))
	emit_insn (gen_strmovsi (destreg, srcreg));
      if (count & 0x02)
	emit_insn (gen_strmovhi (destreg, srcreg));
      if (count & 0x01)
	emit_insn (gen_strmovqi (destreg, srcreg));
    }
  /* The generic code based on the glibc implementation:
     - align destination to 4 bytes (8 byte alignment is used for PentiumPro
     allowing accelerated copying there)
     - copy the data using rep movsl
     - copy the rest.  */
  else
    {
      rtx countreg2;
      rtx label = NULL;
      int desired_alignment = (TARGET_PENTIUMPRO
			       && (count == 0 || count >= (unsigned int) 260)
			       ? 8 : UNITS_PER_WORD);

      /* In case we don't know anything about the alignment, default to
         library version, since it is usually equally fast and result in
         shorter code.  */
      if (!TARGET_INLINE_ALL_STRINGOPS && align < UNITS_PER_WORD)
	{
	  end_sequence ();
	  return 0;
	}

      if (TARGET_SINGLE_STRINGOP)
	emit_insn (gen_cld ());

      countreg2 = gen_reg_rtx (Pmode);
      countreg = copy_to_mode_reg (counter_mode, count_exp);

      /* We don't use loops to align destination and to copy parts smaller
         than 4 bytes, because gcc is able to optimize such code better (in
         the case the destination or the count really is aligned, gcc is often
         able to predict the branches) and also it is friendlier to the
         hardware branch prediction.

         Using loops is benefical for generic case, because we can
         handle small counts using the loops.  Many CPUs (such as Athlon)
         have large REP prefix setup costs.

         This is quite costy.  Maybe we can revisit this decision later or
         add some customizability to this code.  */

      if (count == 0 && align < desired_alignment)
	{
	  label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (countreg, GEN_INT (desired_alignment - 1),
				   LEU, 0, counter_mode, 1, label);
	}
      if (align <= 1)
	{
	  rtx label = ix86_expand_aligntest (destreg, 1);
	  emit_insn (gen_strmovqi (destreg, srcreg));
	  ix86_adjust_counter (countreg, 1);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align <= 2)
	{
	  rtx label = ix86_expand_aligntest (destreg, 2);
	  emit_insn (gen_strmovhi (destreg, srcreg));
	  ix86_adjust_counter (countreg, 2);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align <= 4 && desired_alignment > 4)
	{
	  rtx label = ix86_expand_aligntest (destreg, 4);
	  emit_insn (gen_strmovsi (destreg, srcreg));
	  ix86_adjust_counter (countreg, 4);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}

      if (label && desired_alignment > 4 && !TARGET_64BIT)
	{
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	  label = NULL_RTX;
	}
      if (!TARGET_SINGLE_STRINGOP)
	emit_insn (gen_cld ());
      if (TARGET_64BIT)
	{
	  emit_insn (gen_lshrdi3 (countreg2, ix86_zero_extend_to_Pmode (countreg),
				  GEN_INT (3)));
	  emit_insn (gen_rep_movdi_rex64 (destreg, srcreg, countreg2,
					  destreg, srcreg, countreg2));
	}
      else
	{
	  emit_insn (gen_lshrsi3 (countreg2, countreg, GEN_INT (2)));
	  emit_insn (gen_rep_movsi (destreg, srcreg, countreg2,
				    destreg, srcreg, countreg2));
	}

      if (label)
	{
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (TARGET_64BIT && align > 4 && count != 0 && (count & 4))
	emit_insn (gen_strmovsi (destreg, srcreg));
      if ((align <= 4 || count == 0) && TARGET_64BIT)
	{
	  rtx label = ix86_expand_aligntest (countreg, 4);
	  emit_insn (gen_strmovsi (destreg, srcreg));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align > 2 && count != 0 && (count & 2))
	emit_insn (gen_strmovhi (destreg, srcreg));
      if (align <= 2 || count == 0)
	{
	  rtx label = ix86_expand_aligntest (countreg, 2);
	  emit_insn (gen_strmovhi (destreg, srcreg));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align > 1 && count != 0 && (count & 1))
	emit_insn (gen_strmovqi (destreg, srcreg));
      if (align <= 1 || count == 0)
	{
	  rtx label = ix86_expand_aligntest (countreg, 1);
	  emit_insn (gen_strmovqi (destreg, srcreg));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
    }

  insns = get_insns ();
  end_sequence ();

  ix86_set_move_mem_attrs (insns, dst, src, destreg, srcreg);
  emit_insn (insns);
  return 1;
}

/* Expand string clear operation (bzero).  Use i386 string operations when
   profitable.  expand_movstr contains similar code.  */
int
ix86_expand_clrstr (src, count_exp, align_exp)
     rtx src, count_exp, align_exp;
{
  rtx destreg, zeroreg, countreg;
  enum machine_mode counter_mode;
  HOST_WIDE_INT align = 0;
  unsigned HOST_WIDE_INT count = 0;

  if (GET_CODE (align_exp) == CONST_INT)
    align = INTVAL (align_exp);

  /* This simple hack avoids all inlining code and simplifies code below.  */
  if (!TARGET_ALIGN_STRINGOPS)
    align = 32;

  if (GET_CODE (count_exp) == CONST_INT)
    count = INTVAL (count_exp);
  /* Figure out proper mode for counter.  For 32bits it is always SImode,
     for 64bits use SImode when possible, otherwise DImode.
     Set count to number of bytes copied when known at compile time.  */
  if (!TARGET_64BIT || GET_MODE (count_exp) == SImode
      || x86_64_zero_extended_value (count_exp))
    counter_mode = SImode;
  else
    counter_mode = DImode;

  destreg = copy_to_mode_reg (Pmode, XEXP (src, 0));

  emit_insn (gen_cld ());

  /* When optimizing for size emit simple rep ; movsb instruction for
     counts not divisible by 4.  */

  if ((!optimize || optimize_size) && (count == 0 || (count & 0x03)))
    {
      countreg = ix86_zero_extend_to_Pmode (count_exp);
      zeroreg = copy_to_mode_reg (QImode, const0_rtx);
      if (TARGET_64BIT)
	emit_insn (gen_rep_stosqi_rex64 (destreg, countreg, zeroreg,
				         destreg, countreg));
      else
	emit_insn (gen_rep_stosqi (destreg, countreg, zeroreg,
				   destreg, countreg));
    }
  else if (count != 0
	   && (align >= 8
	       || (!TARGET_PENTIUMPRO && !TARGET_64BIT && align >= 4)
	       || optimize_size || count < (unsigned int) 64))
    {
      int size = TARGET_64BIT && !optimize_size ? 8 : 4;
      zeroreg = copy_to_mode_reg (size == 4 ? SImode : DImode, const0_rtx);
      if (count & ~(size - 1))
	{
	  countreg = copy_to_mode_reg (counter_mode,
				       GEN_INT ((count >> (size == 4 ? 2 : 3))
						& (TARGET_64BIT ? -1 : 0x3fffffff)));
	  countreg = ix86_zero_extend_to_Pmode (countreg);
	  if (size == 4)
	    {
	      if (TARGET_64BIT)
		emit_insn (gen_rep_stossi_rex64 (destreg, countreg, zeroreg,
					         destreg, countreg));
	      else
		emit_insn (gen_rep_stossi (destreg, countreg, zeroreg,
					   destreg, countreg));
	    }
	  else
	    emit_insn (gen_rep_stosdi_rex64 (destreg, countreg, zeroreg,
					     destreg, countreg));
	}
      if (size == 8 && (count & 0x04))
	emit_insn (gen_strsetsi (destreg,
				 gen_rtx_SUBREG (SImode, zeroreg, 0)));
      if (count & 0x02)
	emit_insn (gen_strsethi (destreg,
				 gen_rtx_SUBREG (HImode, zeroreg, 0)));
      if (count & 0x01)
	emit_insn (gen_strsetqi (destreg,
				 gen_rtx_SUBREG (QImode, zeroreg, 0)));
    }
  else
    {
      rtx countreg2;
      rtx label = NULL;
      /* Compute desired alignment of the string operation.  */
      int desired_alignment = (TARGET_PENTIUMPRO
			       && (count == 0 || count >= (unsigned int) 260)
			       ? 8 : UNITS_PER_WORD);

      /* In case we don't know anything about the alignment, default to
         library version, since it is usually equally fast and result in
         shorter code.  */
      if (!TARGET_INLINE_ALL_STRINGOPS && align < UNITS_PER_WORD)
	return 0;

      if (TARGET_SINGLE_STRINGOP)
	emit_insn (gen_cld ());

      countreg2 = gen_reg_rtx (Pmode);
      countreg = copy_to_mode_reg (counter_mode, count_exp);
      zeroreg = copy_to_mode_reg (Pmode, const0_rtx);

      if (count == 0 && align < desired_alignment)
	{
	  label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (countreg, GEN_INT (desired_alignment - 1),
				   LEU, 0, counter_mode, 1, label);
	}
      if (align <= 1)
	{
	  rtx label = ix86_expand_aligntest (destreg, 1);
	  emit_insn (gen_strsetqi (destreg,
				   gen_rtx_SUBREG (QImode, zeroreg, 0)));
	  ix86_adjust_counter (countreg, 1);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align <= 2)
	{
	  rtx label = ix86_expand_aligntest (destreg, 2);
	  emit_insn (gen_strsethi (destreg,
				   gen_rtx_SUBREG (HImode, zeroreg, 0)));
	  ix86_adjust_counter (countreg, 2);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align <= 4 && desired_alignment > 4)
	{
	  rtx label = ix86_expand_aligntest (destreg, 4);
	  emit_insn (gen_strsetsi (destreg, (TARGET_64BIT
					     ? gen_rtx_SUBREG (SImode, zeroreg, 0)
					     : zeroreg)));
	  ix86_adjust_counter (countreg, 4);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}

      if (label && desired_alignment > 4 && !TARGET_64BIT)
	{
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	  label = NULL_RTX;
	}

      if (!TARGET_SINGLE_STRINGOP)
	emit_insn (gen_cld ());
      if (TARGET_64BIT)
	{
	  emit_insn (gen_lshrdi3 (countreg2, ix86_zero_extend_to_Pmode (countreg),
				  GEN_INT (3)));
	  emit_insn (gen_rep_stosdi_rex64 (destreg, countreg2, zeroreg,
					   destreg, countreg2));
	}
      else
	{
	  emit_insn (gen_lshrsi3 (countreg2, countreg, GEN_INT (2)));
	  emit_insn (gen_rep_stossi (destreg, countreg2, zeroreg,
				     destreg, countreg2));
	}
      if (label)
	{
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}

      if (TARGET_64BIT && align > 4 && count != 0 && (count & 4))
	emit_insn (gen_strsetsi (destreg,
				 gen_rtx_SUBREG (SImode, zeroreg, 0)));
      if (TARGET_64BIT && (align <= 4 || count == 0))
	{
	  rtx label = ix86_expand_aligntest (countreg, 4);
	  emit_insn (gen_strsetsi (destreg,
				   gen_rtx_SUBREG (SImode, zeroreg, 0)));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align > 2 && count != 0 && (count & 2))
	emit_insn (gen_strsethi (destreg,
				 gen_rtx_SUBREG (HImode, zeroreg, 0)));
      if (align <= 2 || count == 0)
	{
	  rtx label = ix86_expand_aligntest (countreg, 2);
	  emit_insn (gen_strsethi (destreg,
				   gen_rtx_SUBREG (HImode, zeroreg, 0)));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (align > 1 && count != 0 && (count & 1))
	emit_insn (gen_strsetqi (destreg,
				 gen_rtx_SUBREG (QImode, zeroreg, 0)));
      if (align <= 1 || count == 0)
	{
	  rtx label = ix86_expand_aligntest (countreg, 1);
	  emit_insn (gen_strsetqi (destreg,
				   gen_rtx_SUBREG (QImode, zeroreg, 0)));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
    }
  return 1;
}
/* Expand strlen.  */
int
ix86_expand_strlen (out, src, eoschar, align)
     rtx out, src, eoschar, align;
{
  rtx addr, scratch1, scratch2, scratch3, scratch4;

  /* The generic case of strlen expander is long.  Avoid it's
     expanding unless TARGET_INLINE_ALL_STRINGOPS.  */

  if (TARGET_UNROLL_STRLEN && eoschar == const0_rtx && optimize > 1
      && !TARGET_INLINE_ALL_STRINGOPS
      && !optimize_size
      && (GET_CODE (align) != CONST_INT || INTVAL (align) < 4))
    return 0;

  addr = force_reg (Pmode, XEXP (src, 0));
  scratch1 = gen_reg_rtx (Pmode);

  if (TARGET_UNROLL_STRLEN && eoschar == const0_rtx && optimize > 1
      && !optimize_size)
    {
      /* Well it seems that some optimizer does not combine a call like
         foo(strlen(bar), strlen(bar));
         when the move and the subtraction is done here.  It does calculate
         the length just once when these instructions are done inside of
         output_strlen_unroll().  But I think since &bar[strlen(bar)] is
         often used and I use one fewer register for the lifetime of
         output_strlen_unroll() this is better.  */

      emit_move_insn (out, addr);

      ix86_expand_strlensi_unroll_1 (out, align);

      /* strlensi_unroll_1 returns the address of the zero at the end of
         the string, like memchr(), so compute the length by subtracting
         the start address.  */
      if (TARGET_64BIT)
	emit_insn (gen_subdi3 (out, out, addr));
      else
	emit_insn (gen_subsi3 (out, out, addr));
    }
  else
    {
      scratch2 = gen_reg_rtx (Pmode);
      scratch3 = gen_reg_rtx (Pmode);
      scratch4 = force_reg (Pmode, constm1_rtx);

      emit_move_insn (scratch3, addr);
      eoschar = force_reg (QImode, eoschar);

      emit_insn (gen_cld ());
      if (TARGET_64BIT)
	{
	  emit_insn (gen_strlenqi_rex_1 (scratch1, scratch3, eoschar,
					 align, scratch4, scratch3));
	  emit_insn (gen_one_cmpldi2 (scratch2, scratch1));
	  emit_insn (gen_adddi3 (out, scratch2, constm1_rtx));
	}
      else
	{
	  emit_insn (gen_strlenqi_1 (scratch1, scratch3, eoschar,
				     align, scratch4, scratch3));
	  emit_insn (gen_one_cmplsi2 (scratch2, scratch1));
	  emit_insn (gen_addsi3 (out, scratch2, constm1_rtx));
	}
    }
  return 1;
}

/* Expand the appropriate insns for doing strlen if not just doing
   repnz; scasb

   out = result, initialized with the start address
   align_rtx = alignment of the address.
   scratch = scratch register, initialized with the startaddress when
	not aligned, otherwise undefined

   This is just the body. It needs the initialisations mentioned above and
   some address computing at the end.  These things are done in i386.md.  */

static void
ix86_expand_strlensi_unroll_1 (out, align_rtx)
     rtx out, align_rtx;
{
  int align;
  rtx tmp;
  rtx align_2_label = NULL_RTX;
  rtx align_3_label = NULL_RTX;
  rtx align_4_label = gen_label_rtx ();
  rtx end_0_label = gen_label_rtx ();
  rtx mem;
  rtx tmpreg = gen_reg_rtx (SImode);
  rtx scratch = gen_reg_rtx (SImode);

  align = 0;
  if (GET_CODE (align_rtx) == CONST_INT)
    align = INTVAL (align_rtx);

  /* Loop to check 1..3 bytes for null to get an aligned pointer.  */

  /* Is there a known alignment and is it less than 4?  */
  if (align < 4)
    {
      rtx scratch1 = gen_reg_rtx (Pmode);
      emit_move_insn (scratch1, out);
      /* Is there a known alignment and is it not 2? */
      if (align != 2)
	{
	  align_3_label = gen_label_rtx (); /* Label when aligned to 3-byte */
	  align_2_label = gen_label_rtx (); /* Label when aligned to 2-byte */

	  /* Leave just the 3 lower bits.  */
	  align_rtx = expand_binop (Pmode, and_optab, scratch1, GEN_INT (3),
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_cmp_and_jump_insns (align_rtx, const0_rtx, EQ, NULL,
				   Pmode, 1, align_4_label);
	  emit_cmp_and_jump_insns (align_rtx, GEN_INT (2), EQ, NULL,
				   Pmode, 1, align_2_label);
	  emit_cmp_and_jump_insns (align_rtx, GEN_INT (2), GTU, NULL,
				   Pmode, 1, align_3_label);
	}
      else
        {
	  /* Since the alignment is 2, we have to check 2 or 0 bytes;
	     check if is aligned to 4 - byte.  */

	  align_rtx = expand_binop (Pmode, and_optab, scratch1, GEN_INT (2),
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_cmp_and_jump_insns (align_rtx, const0_rtx, EQ, NULL,
				   Pmode, 1, align_4_label);
        }

      mem = gen_rtx_MEM (QImode, out);

      /* Now compare the bytes.  */

      /* Compare the first n unaligned byte on a byte per byte basis.  */
      emit_cmp_and_jump_insns (mem, const0_rtx, EQ, NULL,
			       QImode, 1, end_0_label);

      /* Increment the address.  */
      if (TARGET_64BIT)
	emit_insn (gen_adddi3 (out, out, const1_rtx));
      else
	emit_insn (gen_addsi3 (out, out, const1_rtx));

      /* Not needed with an alignment of 2 */
      if (align != 2)
	{
	  emit_label (align_2_label);

	  emit_cmp_and_jump_insns (mem, const0_rtx, EQ, NULL, QImode, 1,
				   end_0_label);

	  if (TARGET_64BIT)
	    emit_insn (gen_adddi3 (out, out, const1_rtx));
	  else
	    emit_insn (gen_addsi3 (out, out, const1_rtx));

	  emit_label (align_3_label);
	}

      emit_cmp_and_jump_insns (mem, const0_rtx, EQ, NULL, QImode, 1,
			       end_0_label);

      if (TARGET_64BIT)
	emit_insn (gen_adddi3 (out, out, const1_rtx));
      else
	emit_insn (gen_addsi3 (out, out, const1_rtx));
    }

  /* Generate loop to check 4 bytes at a time.  It is not a good idea to
     align this loop.  It gives only huge programs, but does not help to
     speed up.  */
  emit_label (align_4_label);

  mem = gen_rtx_MEM (SImode, out);
  emit_move_insn (scratch, mem);
  if (TARGET_64BIT)
    emit_insn (gen_adddi3 (out, out, GEN_INT (4)));
  else
    emit_insn (gen_addsi3 (out, out, GEN_INT (4)));

  /* This formula yields a nonzero result iff one of the bytes is zero.
     This saves three branches inside loop and many cycles.  */

  emit_insn (gen_addsi3 (tmpreg, scratch, GEN_INT (-0x01010101)));
  emit_insn (gen_one_cmplsi2 (scratch, scratch));
  emit_insn (gen_andsi3 (tmpreg, tmpreg, scratch));
  emit_insn (gen_andsi3 (tmpreg, tmpreg,
			 gen_int_mode (0x80808080, SImode)));
  emit_cmp_and_jump_insns (tmpreg, const0_rtx, EQ, 0, SImode, 1,
			   align_4_label);

  if (TARGET_CMOVE)
    {
       rtx reg = gen_reg_rtx (SImode);
       rtx reg2 = gen_reg_rtx (Pmode);
       emit_move_insn (reg, tmpreg);
       emit_insn (gen_lshrsi3 (reg, reg, GEN_INT (16)));

       /* If zero is not in the first two bytes, move two bytes forward.  */
       emit_insn (gen_testsi_ccno_1 (tmpreg, GEN_INT (0x8080)));
       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
       emit_insn (gen_rtx_SET (VOIDmode, tmpreg,
			       gen_rtx_IF_THEN_ELSE (SImode, tmp,
						     reg,
						     tmpreg)));
       /* Emit lea manually to avoid clobbering of flags.  */
       emit_insn (gen_rtx_SET (SImode, reg2,
			       gen_rtx_PLUS (Pmode, out, GEN_INT (2))));

       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
       emit_insn (gen_rtx_SET (VOIDmode, out,
			       gen_rtx_IF_THEN_ELSE (Pmode, tmp,
						     reg2,
						     out)));

    }
  else
    {
       rtx end_2_label = gen_label_rtx ();
       /* Is zero in the first two bytes? */

       emit_insn (gen_testsi_ccno_1 (tmpreg, GEN_INT (0x8080)));
       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_NE (VOIDmode, tmp, const0_rtx);
       tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
                            gen_rtx_LABEL_REF (VOIDmode, end_2_label),
                            pc_rtx);
       tmp = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
       JUMP_LABEL (tmp) = end_2_label;

       /* Not in the first two.  Move two bytes forward.  */
       emit_insn (gen_lshrsi3 (tmpreg, tmpreg, GEN_INT (16)));
       if (TARGET_64BIT)
	 emit_insn (gen_adddi3 (out, out, GEN_INT (2)));
       else
	 emit_insn (gen_addsi3 (out, out, GEN_INT (2)));

       emit_label (end_2_label);

    }

  /* Avoid branch in fixing the byte.  */
  tmpreg = gen_lowpart (QImode, tmpreg);
  emit_insn (gen_addqi3_cc (tmpreg, tmpreg, tmpreg));
  if (TARGET_64BIT)
    emit_insn (gen_subdi3_carry_rex64 (out, out, GEN_INT (3)));
  else
    emit_insn (gen_subsi3_carry (out, out, GEN_INT (3)));

  emit_label (end_0_label);
}

void
ix86_expand_call (retval, fnaddr, callarg1, callarg2, pop)
     rtx retval, fnaddr, callarg1, callarg2, pop;
{
  rtx use = NULL, call;

  if (pop == const0_rtx)
    pop = NULL;
  if (TARGET_64BIT && pop)
    abort ();

#if TARGET_MACHO
  if (flag_pic && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF)
    fnaddr = machopic_indirect_call_target (fnaddr);
#else
  /* Static functions and indirect calls don't need the pic register.  */
  if (! TARGET_64BIT && flag_pic
      && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
      && ! SYMBOL_REF_FLAG (XEXP (fnaddr, 0)))
    use_reg (&use, pic_offset_table_rtx);

  if (TARGET_64BIT && INTVAL (callarg2) >= 0)
    {
      rtx al = gen_rtx_REG (QImode, 0);
      emit_move_insn (al, callarg2);
      use_reg (&use, al);
    }
#endif /* TARGET_MACHO */

  if (! call_insn_operand (XEXP (fnaddr, 0), Pmode))
    {
      fnaddr = copy_to_mode_reg (Pmode, XEXP (fnaddr, 0));
      fnaddr = gen_rtx_MEM (QImode, fnaddr);
    }

  call = gen_rtx_CALL (VOIDmode, fnaddr, callarg1);
  if (retval)
    call = gen_rtx_SET (VOIDmode, retval, call);
  if (pop)
    {
      pop = gen_rtx_PLUS (Pmode, stack_pointer_rtx, pop);
      pop = gen_rtx_SET (VOIDmode, stack_pointer_rtx, pop);
      call = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, call, pop));
    }

  call = emit_call_insn (call);
  if (use)
    CALL_INSN_FUNCTION_USAGE (call) = use;
}


/* Clear stack slot assignments remembered from previous functions.
   This is called from INIT_EXPANDERS once before RTL is emitted for each
   function.  */

static struct machine_function *
ix86_init_machine_status ()
{
  return ggc_alloc_cleared (sizeof (struct machine_function));
}

/* Return a MEM corresponding to a stack slot with mode MODE.
   Allocate a new slot if necessary.

   The RTL for a function can have several slots available: N is
   which slot to use.  */

rtx
assign_386_stack_local (mode, n)
     enum machine_mode mode;
     int n;
{
  if (n < 0 || n >= MAX_386_STACK_LOCALS)
    abort ();

  if (ix86_stack_locals[(int) mode][n] == NULL_RTX)
    ix86_stack_locals[(int) mode][n]
      = assign_stack_local (mode, GET_MODE_SIZE (mode), 0);

  return ix86_stack_locals[(int) mode][n];
}

/* Construct the SYMBOL_REF for the tls_get_addr function.  */

static GTY(()) rtx ix86_tls_symbol;
rtx
ix86_tls_get_addr ()
{

  if (!ix86_tls_symbol)
    {
      ix86_tls_symbol = gen_rtx_SYMBOL_REF (Pmode,
					    (TARGET_GNU_TLS && !TARGET_64BIT)
					    ? "___tls_get_addr"
					    : "__tls_get_addr");
    }

  return ix86_tls_symbol;
}

/* Calculate the length of the memory address in the instruction
   encoding.  Does not include the one-byte modrm, opcode, or prefix.  */

static int
memory_address_length (addr)
     rtx addr;
{
  struct ix86_address parts;
  rtx base, index, disp;
  int len;

  if (GET_CODE (addr) == PRE_DEC
      || GET_CODE (addr) == POST_INC
      || GET_CODE (addr) == PRE_MODIFY
      || GET_CODE (addr) == POST_MODIFY)
    return 0;

  if (! ix86_decompose_address (addr, &parts))
    abort ();

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  len = 0;

  /* Register Indirect.  */
  if (base && !index && !disp)
    {
      /* Special cases: ebp and esp need the two-byte modrm form.  */
      if (addr == stack_pointer_rtx
	  || addr == arg_pointer_rtx
	  || addr == frame_pointer_rtx
	  || addr == hard_frame_pointer_rtx)
	len = 1;
    }

  /* Direct Addressing.  */
  else if (disp && !base && !index)
    len = 4;

  else
    {
      /* Find the length of the displacement constant.  */
      if (disp)
	{
	  if (GET_CODE (disp) == CONST_INT
	      && CONST_OK_FOR_LETTER_P (INTVAL (disp), 'K')
	      && base)
	    len = 1;
	  else
	    len = 4;
	}

      /* An index requires the two-byte modrm form.  */
      if (index)
	len += 1;
    }

  return len;
}

/* Compute default value for "length_immediate" attribute.  When SHORTFORM
   is set, expect that insn have 8bit immediate alternative.  */
int
ix86_attr_length_immediate_default (insn, shortform)
     rtx insn;
     int shortform;
{
  int len = 0;
  int i;
  extract_insn_cached (insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (CONSTANT_P (recog_data.operand[i]))
      {
	if (len)
	  abort ();
	if (shortform
	    && GET_CODE (recog_data.operand[i]) == CONST_INT
	    && CONST_OK_FOR_LETTER_P (INTVAL (recog_data.operand[i]), 'K'))
	  len = 1;
	else
	  {
	    switch (get_attr_mode (insn))
	      {
		case MODE_QI:
		  len+=1;
		  break;
		case MODE_HI:
		  len+=2;
		  break;
		case MODE_SI:
		  len+=4;
		  break;
		/* Immediates for DImode instructions are encoded as 32bit sign extended values.  */
		case MODE_DI:
		  len+=4;
		  break;
		default:
		  fatal_insn ("unknown insn mode", insn);
	      }
	  }
      }
  return len;
}
/* Compute default value for "length_address" attribute.  */
int
ix86_attr_length_address_default (insn)
     rtx insn;
{
  int i;

  if (get_attr_type (insn) == TYPE_LEA)
    {
      rtx set = PATTERN (insn);
      if (GET_CODE (set) == SET)
	;
      else if (GET_CODE (set) == PARALLEL
	       && GET_CODE (XVECEXP (set, 0, 0)) == SET)
	set = XVECEXP (set, 0, 0);
      else
	{
#ifdef ENABLE_CHECKING
	  abort ();
#endif
	  return 0;
	}

      return memory_address_length (SET_SRC (set));
    }

  extract_insn_cached (insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (GET_CODE (recog_data.operand[i]) == MEM)
      {
	return memory_address_length (XEXP (recog_data.operand[i], 0));
	break;
      }
  return 0;
}

/* Return the maximum number of instructions a cpu can issue.  */

static int
ix86_issue_rate ()
{
  switch (ix86_cpu)
    {
    case PROCESSOR_PENTIUM:
    case PROCESSOR_K6:
      return 2;

    case PROCESSOR_PENTIUMPRO:
    case PROCESSOR_PENTIUM4:
    case PROCESSOR_ATHLON:
      return 3;

    default:
      return 1;
    }
}

/* A subroutine of ix86_adjust_cost -- return true iff INSN reads flags set
   by DEP_INSN and nothing set by DEP_INSN.  */

static int
ix86_flags_dependant (insn, dep_insn, insn_type)
     rtx insn, dep_insn;
     enum attr_type insn_type;
{
  rtx set, set2;

  /* Simplify the test for uninteresting insns.  */
  if (insn_type != TYPE_SETCC
      && insn_type != TYPE_ICMOV
      && insn_type != TYPE_FCMOV
      && insn_type != TYPE_IBR)
    return 0;

  if ((set = single_set (dep_insn)) != 0)
    {
      set = SET_DEST (set);
      set2 = NULL_RTX;
    }
  else if (GET_CODE (PATTERN (dep_insn)) == PARALLEL
	   && XVECLEN (PATTERN (dep_insn), 0) == 2
	   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 0)) == SET
	   && GET_CODE (XVECEXP (PATTERN (dep_insn), 0, 1)) == SET)
    {
      set = SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0));
      set2 = SET_DEST (XVECEXP (PATTERN (dep_insn), 0, 0));
    }
  else
    return 0;

  if (GET_CODE (set) != REG || REGNO (set) != FLAGS_REG)
    return 0;

  /* This test is true if the dependent insn reads the flags but
     not any other potentially set register.  */
  if (!reg_overlap_mentioned_p (set, PATTERN (insn)))
    return 0;

  if (set2 && reg_overlap_mentioned_p (set2, PATTERN (insn)))
    return 0;

  return 1;
}

/* A subroutine of ix86_adjust_cost -- return true iff INSN has a memory
   address with operands set by DEP_INSN.  */

static int
ix86_agi_dependant (insn, dep_insn, insn_type)
     rtx insn, dep_insn;
     enum attr_type insn_type;
{
  rtx addr;

  if (insn_type == TYPE_LEA
      && TARGET_PENTIUM)
    {
      addr = PATTERN (insn);
      if (GET_CODE (addr) == SET)
	;
      else if (GET_CODE (addr) == PARALLEL
	       && GET_CODE (XVECEXP (addr, 0, 0)) == SET)
	addr = XVECEXP (addr, 0, 0);
      else
	abort ();
      addr = SET_SRC (addr);
    }
  else
    {
      int i;
      extract_insn_cached (insn);
      for (i = recog_data.n_operands - 1; i >= 0; --i)
	if (GET_CODE (recog_data.operand[i]) == MEM)
	  {
	    addr = XEXP (recog_data.operand[i], 0);
	    goto found;
	  }
      return 0;
    found:;
    }

  return modified_in_p (addr, dep_insn);
}

static int
ix86_adjust_cost (insn, link, dep_insn, cost)
     rtx insn, link, dep_insn;
     int cost;
{
  enum attr_type insn_type, dep_insn_type;
  enum attr_memory memory, dep_memory;
  rtx set, set2;
  int dep_insn_code_number;

  /* Anti and output depenancies have zero cost on all CPUs.  */
  if (REG_NOTE_KIND (link) != 0)
    return 0;

  dep_insn_code_number = recog_memoized (dep_insn);

  /* If we can't recognize the insns, we can't really do anything.  */
  if (dep_insn_code_number < 0 || recog_memoized (insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_insn_type = get_attr_type (dep_insn);

  switch (ix86_cpu)
    {
    case PROCESSOR_PENTIUM:
      /* Address Generation Interlock adds a cycle of latency.  */
      if (ix86_agi_dependant (insn, dep_insn, insn_type))
	cost += 1;

      /* ??? Compares pair with jump/setcc.  */
      if (ix86_flags_dependant (insn, dep_insn, insn_type))
	cost = 0;

      /* Floating point stores require value to be ready one cycle ealier.  */
      if (insn_type == TYPE_FMOV
	  && get_attr_memory (insn) == MEMORY_STORE
	  && !ix86_agi_dependant (insn, dep_insn, insn_type))
	cost += 1;
      break;

    case PROCESSOR_PENTIUMPRO:
      memory = get_attr_memory (insn);
      dep_memory = get_attr_memory (dep_insn);

      /* Since we can't represent delayed latencies of load+operation,
	 increase the cost here for non-imov insns.  */
      if (dep_insn_type != TYPE_IMOV
          && dep_insn_type != TYPE_FMOV
          && (dep_memory == MEMORY_LOAD || dep_memory == MEMORY_BOTH))
	cost += 1;

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      /* There is one cycle extra latency between an FP op and a store.  */
      if (insn_type == TYPE_FMOV
	  && (set = single_set (dep_insn)) != NULL_RTX
	  && (set2 = single_set (insn)) != NULL_RTX
	  && rtx_equal_p (SET_DEST (set), SET_SRC (set2))
	  && GET_CODE (SET_DEST (set2)) == MEM)
	cost += 1;

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependant (insn, dep_insn, insn_type))
 	{
	  /* Claim moves to take one cycle, as core can issue one load
	     at time and the next load can start cycle later.  */
	  if (dep_insn_type == TYPE_IMOV
	      || dep_insn_type == TYPE_FMOV)
	    cost = 1;
	  else if (cost > 1)
	    cost--;
	}
      break;

    case PROCESSOR_K6:
      memory = get_attr_memory (insn);
      dep_memory = get_attr_memory (dep_insn);
      /* The esp dependency is resolved before the instruction is really
         finished.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 1;

      /* Since we can't represent delayed latencies of load+operation,
	 increase the cost here for non-imov insns.  */
      if (dep_memory == MEMORY_LOAD || dep_memory == MEMORY_BOTH)
	cost += (dep_insn_type != TYPE_IMOV) ? 2 : 1;

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependant (insn, dep_insn, insn_type))
 	{
	  /* Claim moves to take one cycle, as core can issue one load
	     at time and the next load can start cycle later.  */
	  if (dep_insn_type == TYPE_IMOV
	      || dep_insn_type == TYPE_FMOV)
	    cost = 1;
	  else if (cost > 2)
	    cost -= 2;
	  else
	    cost = 1;
	}
      break;

    case PROCESSOR_ATHLON:
      memory = get_attr_memory (insn);
      dep_memory = get_attr_memory (dep_insn);

      if (dep_memory == MEMORY_LOAD || dep_memory == MEMORY_BOTH)
	{
	  if (dep_insn_type == TYPE_IMOV || dep_insn_type == TYPE_FMOV)
	    cost += 2;
	  else
	    cost += 3;
        }
      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependant (insn, dep_insn, insn_type))
 	{
	  /* Claim moves to take one cycle, as core can issue one load
	     at time and the next load can start cycle later.  */
	  if (dep_insn_type == TYPE_IMOV
	      || dep_insn_type == TYPE_FMOV)
	    cost = 0;
	  else if (cost >= 3)
	    cost -= 3;
	  else
	    cost = 0;
	}

    default:
      break;
    }

  return cost;
}

static union
{
  struct ppro_sched_data
  {
    rtx decode[3];
    int issued_this_cycle;
  } ppro;
} ix86_sched_data;

static enum attr_ppro_uops
ix86_safe_ppro_uops (insn)
     rtx insn;
{
  if (recog_memoized (insn) >= 0)
    return get_attr_ppro_uops (insn);
  else
    return PPRO_UOPS_MANY;
}

static void
ix86_dump_ppro_packet (dump)
     FILE *dump;
{
  if (ix86_sched_data.ppro.decode[0])
    {
      fprintf (dump, "PPRO packet: %d",
	       INSN_UID (ix86_sched_data.ppro.decode[0]));
      if (ix86_sched_data.ppro.decode[1])
	fprintf (dump, " %d", INSN_UID (ix86_sched_data.ppro.decode[1]));
      if (ix86_sched_data.ppro.decode[2])
	fprintf (dump, " %d", INSN_UID (ix86_sched_data.ppro.decode[2]));
      fputc ('\n', dump);
    }
}

/* We're beginning a new block.  Initialize data structures as necessary.  */

static void
ix86_sched_init (dump, sched_verbose, veclen)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
     int veclen ATTRIBUTE_UNUSED;
{
  memset (&ix86_sched_data, 0, sizeof (ix86_sched_data));
}

/* Shift INSN to SLOT, and shift everything else down.  */

static void
ix86_reorder_insn (insnp, slot)
     rtx *insnp, *slot;
{
  if (insnp != slot)
    {
      rtx insn = *insnp;
      do
	insnp[0] = insnp[1];
      while (++insnp != slot);
      *insnp = insn;
    }
}

static void
ix86_sched_reorder_ppro (ready, e_ready)
     rtx *ready;
     rtx *e_ready;
{
  rtx decode[3];
  enum attr_ppro_uops cur_uops;
  int issued_this_cycle;
  rtx *insnp;
  int i;

  /* At this point .ppro.decode contains the state of the three
     decoders from last "cycle".  That is, those insns that were
     actually independent.  But here we're scheduling for the
     decoder, and we may find things that are decodable in the
     same cycle.  */

  memcpy (decode, ix86_sched_data.ppro.decode, sizeof (decode));
  issued_this_cycle = 0;

  insnp = e_ready;
  cur_uops = ix86_safe_ppro_uops (*insnp);

  /* If the decoders are empty, and we've a complex insn at the
     head of the priority queue, let it issue without complaint.  */
  if (decode[0] == NULL)
    {
      if (cur_uops == PPRO_UOPS_MANY)
	{
	  decode[0] = *insnp;
	  goto ppro_done;
	}

      /* Otherwise, search for a 2-4 uop unsn to issue.  */
      while (cur_uops != PPRO_UOPS_FEW)
	{
	  if (insnp == ready)
	    break;
	  cur_uops = ix86_safe_ppro_uops (*--insnp);
	}

      /* If so, move it to the head of the line.  */
      if (cur_uops == PPRO_UOPS_FEW)
	ix86_reorder_insn (insnp, e_ready);

      /* Issue the head of the queue.  */
      issued_this_cycle = 1;
      decode[0] = *e_ready--;
    }

  /* Look for simple insns to fill in the other two slots.  */
  for (i = 1; i < 3; ++i)
    if (decode[i] == NULL)
      {
	if (ready > e_ready)
	  goto ppro_done;

	insnp = e_ready;
	cur_uops = ix86_safe_ppro_uops (*insnp);
	while (cur_uops != PPRO_UOPS_ONE)
	  {
	    if (insnp == ready)
	      break;
	    cur_uops = ix86_safe_ppro_uops (*--insnp);
	  }

	/* Found one.  Move it to the head of the queue and issue it.  */
	if (cur_uops == PPRO_UOPS_ONE)
	  {
	    ix86_reorder_insn (insnp, e_ready);
	    decode[i] = *e_ready--;
	    issued_this_cycle++;
	    continue;
	  }

	/* ??? Didn't find one.  Ideally, here we would do a lazy split
	   of 2-uop insns, issue one and queue the other.  */
      }

 ppro_done:
  if (issued_this_cycle == 0)
    issued_this_cycle = 1;
  ix86_sched_data.ppro.issued_this_cycle = issued_this_cycle;
}

/* We are about to being issuing insns for this clock cycle.
   Override the default sort algorithm to better slot instructions.  */
static int
ix86_sched_reorder (dump, sched_verbose, ready, n_readyp, clock_var)
     FILE *dump ATTRIBUTE_UNUSED;
     int sched_verbose ATTRIBUTE_UNUSED;
     rtx *ready;
     int *n_readyp;
     int clock_var ATTRIBUTE_UNUSED;
{
  int n_ready = *n_readyp;
  rtx *e_ready = ready + n_ready - 1;

  /* Make sure to go ahead and initialize key items in
     ix86_sched_data if we are not going to bother trying to
     reorder the ready queue.  */
  if (n_ready < 2)
    {
      ix86_sched_data.ppro.issued_this_cycle = 1;
      goto out;
    }

  switch (ix86_cpu)
    {
    default:
      break;

    case PROCESSOR_PENTIUMPRO:
      ix86_sched_reorder_ppro (ready, e_ready);
      break;
    }

out:
  return ix86_issue_rate ();
}

/* We are about to issue INSN.  Return the number of insns left on the
   ready queue that can be issued this cycle.  */

static int
ix86_variable_issue (dump, sched_verbose, insn, can_issue_more)
     FILE *dump;
     int sched_verbose;
     rtx insn;
     int can_issue_more;
{
  int i;
  switch (ix86_cpu)
    {
    default:
      return can_issue_more - 1;

    case PROCESSOR_PENTIUMPRO:
      {
	enum attr_ppro_uops uops = ix86_safe_ppro_uops (insn);

	if (uops == PPRO_UOPS_MANY)
	  {
	    if (sched_verbose)
	      ix86_dump_ppro_packet (dump);
	    ix86_sched_data.ppro.decode[0] = insn;
	    ix86_sched_data.ppro.decode[1] = NULL;
	    ix86_sched_data.ppro.decode[2] = NULL;
	    if (sched_verbose)
	      ix86_dump_ppro_packet (dump);
	    ix86_sched_data.ppro.decode[0] = NULL;
	  }
	else if (uops == PPRO_UOPS_FEW)
	  {
	    if (sched_verbose)
	      ix86_dump_ppro_packet (dump);
	    ix86_sched_data.ppro.decode[0] = insn;
	    ix86_sched_data.ppro.decode[1] = NULL;
	    ix86_sched_data.ppro.decode[2] = NULL;
	  }
	else
	  {
	    for (i = 0; i < 3; ++i)
	      if (ix86_sched_data.ppro.decode[i] == NULL)
		{
		  ix86_sched_data.ppro.decode[i] = insn;
		  break;
		}
	    if (i == 3)
	      abort ();
	    if (i == 2)
	      {
	        if (sched_verbose)
	          ix86_dump_ppro_packet (dump);
		ix86_sched_data.ppro.decode[0] = NULL;
		ix86_sched_data.ppro.decode[1] = NULL;
		ix86_sched_data.ppro.decode[2] = NULL;
	      }
	  }
      }
      return --ix86_sched_data.ppro.issued_this_cycle;
    }
}

static int
ia32_use_dfa_pipeline_interface ()
{
  if (ix86_cpu == PROCESSOR_PENTIUM)
    return 1;
  return 0;
}

/* How many alternative schedules to try.  This should be as wide as the
   scheduling freedom in the DFA, but no wider.  Making this value too
   large results extra work for the scheduler.  */

static int
ia32_multipass_dfa_lookahead ()
{
  if (ix86_cpu == PROCESSOR_PENTIUM)
    return 2;
  else
   return 0;
}


/* Walk through INSNS and look for MEM references whose address is DSTREG or
   SRCREG and set the memory attribute to those of DSTREF and SRCREF, as
   appropriate.  */

void
ix86_set_move_mem_attrs (insns, dstref, srcref, dstreg, srcreg)
     rtx insns;
     rtx dstref, srcref, dstreg, srcreg;
{
  rtx insn;

  for (insn = insns; insn != 0 ; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      ix86_set_move_mem_attrs_1 (PATTERN (insn), dstref, srcref,
				 dstreg, srcreg);
}

/* Subroutine of above to actually do the updating by recursively walking
   the rtx.  */

static void
ix86_set_move_mem_attrs_1 (x, dstref, srcref, dstreg, srcreg)
     rtx x;
     rtx dstref, srcref, dstreg, srcreg;
{
  enum rtx_code code = GET_CODE (x);
  const char *format_ptr = GET_RTX_FORMAT (code);
  int i, j;

  if (code == MEM && XEXP (x, 0) == dstreg)
    MEM_COPY_ATTRIBUTES (x, dstref);
  else if (code == MEM && XEXP (x, 0) == srcreg)
    MEM_COPY_ATTRIBUTES (x, srcref);

  for (i = 0; i < GET_RTX_LENGTH (code); i++, format_ptr++)
    {
      if (*format_ptr == 'e')
	ix86_set_move_mem_attrs_1 (XEXP (x, i), dstref, srcref,
				   dstreg, srcreg);
      else if (*format_ptr == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  ix86_set_move_mem_attrs_1 (XVECEXP (x, i, j), dstref, srcref,
				     dstreg, srcreg);
    }
}

/* Compute the alignment given to a constant that is being placed in memory.
   EXP is the constant and ALIGN is the alignment that the object would
   ordinarily have.
   The value of this function is used instead of that alignment to align
   the object.  */

int
ix86_constant_alignment (exp, align)
     tree exp;
     int align;
{
  if (TREE_CODE (exp) == REAL_CST)
    {
      if (TYPE_MODE (TREE_TYPE (exp)) == DFmode && align < 64)
	return 64;
      else if (ALIGN_MODE_128 (TYPE_MODE (TREE_TYPE (exp))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (exp) == STRING_CST && TREE_STRING_LENGTH (exp) >= 31
	   && align < 256)
    return 256;

  return align;
}

/* Compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this function is used
   instead of that alignment to align the object.  */

int
ix86_data_alignment (type, align)
     tree type;
     int align;
{
  if (AGGREGATE_TYPE_P (type)
       && TYPE_SIZE (type)
       && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
       && (TREE_INT_CST_LOW (TYPE_SIZE (type)) >= 256
	   || TREE_INT_CST_HIGH (TYPE_SIZE (type))) && align < 256)
    return 256;

  /* x86-64 ABI requires arrays greater than 16 bytes to be aligned
     to 16byte boundary.  */
  if (TARGET_64BIT)
    {
      if (AGGREGATE_TYPE_P (type)
	   && TYPE_SIZE (type)
	   && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	   && (TREE_INT_CST_LOW (TYPE_SIZE (type)) >= 128
	       || TREE_INT_CST_HIGH (TYPE_SIZE (type))) && align < 128)
	return 128;
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_MODE (TREE_TYPE (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (TREE_TYPE (type))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (type) == COMPLEX_TYPE)
    {

      if (TYPE_MODE (type) == DCmode && align < 64)
	return 64;
      if (TYPE_MODE (type) == XCmode && align < 128)
	return 128;
    }
  else if ((TREE_CODE (type) == RECORD_TYPE
	    || TREE_CODE (type) == UNION_TYPE
	    || TREE_CODE (type) == QUAL_UNION_TYPE)
	   && TYPE_FIELDS (type))
    {
      if (DECL_MODE (TYPE_FIELDS (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (DECL_MODE (TYPE_FIELDS (type))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (type) == REAL_TYPE || TREE_CODE (type) == VECTOR_TYPE
	   || TREE_CODE (type) == INTEGER_TYPE)
    {
      if (TYPE_MODE (type) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (type)) && align < 128)
	return 128;
    }

  return align;
}

/* Compute the alignment for a local variable.
   TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.  */

int
ix86_local_alignment (type, align)
     tree type;
     int align;
{
  /* x86-64 ABI requires arrays greater than 16 bytes to be aligned
     to 16byte boundary.  */
  if (TARGET_64BIT)
    {
      if (AGGREGATE_TYPE_P (type)
	   && TYPE_SIZE (type)
	   && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	   && (TREE_INT_CST_LOW (TYPE_SIZE (type)) >= 16
	       || TREE_INT_CST_HIGH (TYPE_SIZE (type))) && align < 128)
	return 128;
    }
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_MODE (TREE_TYPE (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (TREE_TYPE (type))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      if (TYPE_MODE (type) == DCmode && align < 64)
	return 64;
      if (TYPE_MODE (type) == XCmode && align < 128)
	return 128;
    }
  else if ((TREE_CODE (type) == RECORD_TYPE
	    || TREE_CODE (type) == UNION_TYPE
	    || TREE_CODE (type) == QUAL_UNION_TYPE)
	   && TYPE_FIELDS (type))
    {
      if (DECL_MODE (TYPE_FIELDS (type)) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (DECL_MODE (TYPE_FIELDS (type))) && align < 128)
	return 128;
    }
  else if (TREE_CODE (type) == REAL_TYPE || TREE_CODE (type) == VECTOR_TYPE
	   || TREE_CODE (type) == INTEGER_TYPE)
    {

      if (TYPE_MODE (type) == DFmode && align < 64)
	return 64;
      if (ALIGN_MODE_128 (TYPE_MODE (type)) && align < 128)
	return 128;
    }
  return align;
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
void
x86_initialize_trampoline (tramp, fnaddr, cxt)
     rtx tramp, fnaddr, cxt;
{
  if (!TARGET_64BIT)
    {
      /* Compute offset from the end of the jmp to the target function.  */
      rtx disp = expand_binop (SImode, sub_optab, fnaddr,
			       plus_constant (tramp, 10),
			       NULL_RTX, 1, OPTAB_DIRECT);
      emit_move_insn (gen_rtx_MEM (QImode, tramp),
		      gen_int_mode (0xb9, QImode));
      emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 1)), cxt);
      emit_move_insn (gen_rtx_MEM (QImode, plus_constant (tramp, 5)),
		      gen_int_mode (0xe9, QImode));
      emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 6)), disp);
    }
  else
    {
      int offset = 0;
      /* Try to load address using shorter movl instead of movabs.
         We may want to support movq for kernel mode, but kernel does not use
         trampolines at the moment.  */
      if (x86_64_zero_extended_value (fnaddr))
	{
	  fnaddr = copy_to_mode_reg (DImode, fnaddr);
	  emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
			  gen_int_mode (0xbb41, HImode));
	  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, offset + 2)),
			  gen_lowpart (SImode, fnaddr));
	  offset += 6;
	}
      else
	{
	  emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
			  gen_int_mode (0xbb49, HImode));
	  emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, offset + 2)),
			  fnaddr);
	  offset += 10;
	}
      /* Load static chain using movabs to r10.  */
      emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
		      gen_int_mode (0xba49, HImode));
      emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, offset + 2)),
		      cxt);
      offset += 10;
      /* Jump to the r11 */
      emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
		      gen_int_mode (0xff49, HImode));
      emit_move_insn (gen_rtx_MEM (QImode, plus_constant (tramp, offset+2)),
		      gen_int_mode (0xe3, QImode));
      offset += 3;
      if (offset > TRAMPOLINE_SIZE)
	abort ();
    }

#ifdef TRANSFER_FROM_TRAMPOLINE
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__enable_execute_stack"),
		     LCT_NORMAL, VOIDmode, 1, tramp, Pmode);
#endif
}

#define def_builtin(MASK, NAME, TYPE, CODE)			\
do {								\
  if ((MASK) & target_flags					\
      && (!((MASK) & MASK_64BIT) || TARGET_64BIT))		\
    builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD,	\
		      NULL, NULL_TREE);				\
} while (0)

struct builtin_description
{
  const unsigned int mask;
  const enum insn_code icode;
  const char *const name;
  const enum ix86_builtins code;
  const enum rtx_code comparison;
  const unsigned int flag;
};

/* Used for builtins that are enabled both by -msse and -msse2.  */
#define MASK_SSE1 (MASK_SSE | MASK_SSE2)
#define MASK_SSE164 (MASK_SSE | MASK_SSE2 | MASK_64BIT)
#define MASK_SSE264 (MASK_SSE2 | MASK_64BIT)

static const struct builtin_description bdesc_comi[] =
{
  { MASK_SSE1, CODE_FOR_sse_comi, "__builtin_ia32_comieq", IX86_BUILTIN_COMIEQSS, UNEQ, 0 },
  { MASK_SSE1, CODE_FOR_sse_comi, "__builtin_ia32_comilt", IX86_BUILTIN_COMILTSS, UNLT, 0 },
  { MASK_SSE1, CODE_FOR_sse_comi, "__builtin_ia32_comile", IX86_BUILTIN_COMILESS, UNLE, 0 },
  { MASK_SSE1, CODE_FOR_sse_comi, "__builtin_ia32_comigt", IX86_BUILTIN_COMIGTSS, GT, 0 },
  { MASK_SSE1, CODE_FOR_sse_comi, "__builtin_ia32_comige", IX86_BUILTIN_COMIGESS, GE, 0 },
  { MASK_SSE1, CODE_FOR_sse_comi, "__builtin_ia32_comineq", IX86_BUILTIN_COMINEQSS, LTGT, 0 },
  { MASK_SSE1, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomieq", IX86_BUILTIN_UCOMIEQSS, UNEQ, 0 },
  { MASK_SSE1, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomilt", IX86_BUILTIN_UCOMILTSS, UNLT, 0 },
  { MASK_SSE1, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomile", IX86_BUILTIN_UCOMILESS, UNLE, 0 },
  { MASK_SSE1, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomigt", IX86_BUILTIN_UCOMIGTSS, GT, 0 },
  { MASK_SSE1, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomige", IX86_BUILTIN_UCOMIGESS, GE, 0 },
  { MASK_SSE1, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomineq", IX86_BUILTIN_UCOMINEQSS, LTGT, 0 },
  { MASK_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdeq", IX86_BUILTIN_COMIEQSD, UNEQ, 0 },
  { MASK_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdlt", IX86_BUILTIN_COMILTSD, UNLT, 0 },
  { MASK_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdle", IX86_BUILTIN_COMILESD, UNLE, 0 },
  { MASK_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdgt", IX86_BUILTIN_COMIGTSD, GT, 0 },
  { MASK_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdge", IX86_BUILTIN_COMIGESD, GE, 0 },
  { MASK_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdneq", IX86_BUILTIN_COMINEQSD, LTGT, 0 },
  { MASK_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdeq", IX86_BUILTIN_UCOMIEQSD, UNEQ, 0 },
  { MASK_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdlt", IX86_BUILTIN_UCOMILTSD, UNLT, 0 },
  { MASK_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdle", IX86_BUILTIN_UCOMILESD, UNLE, 0 },
  { MASK_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdgt", IX86_BUILTIN_UCOMIGTSD, GT, 0 },
  { MASK_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdge", IX86_BUILTIN_UCOMIGESD, GE, 0 },
  { MASK_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdneq", IX86_BUILTIN_UCOMINEQSD, LTGT, 0 },
};

static const struct builtin_description bdesc_2arg[] =
{
  /* SSE */
  { MASK_SSE1, CODE_FOR_addv4sf3, "__builtin_ia32_addps", IX86_BUILTIN_ADDPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_subv4sf3, "__builtin_ia32_subps", IX86_BUILTIN_SUBPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_mulv4sf3, "__builtin_ia32_mulps", IX86_BUILTIN_MULPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_divv4sf3, "__builtin_ia32_divps", IX86_BUILTIN_DIVPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_vmaddv4sf3,  "__builtin_ia32_addss", IX86_BUILTIN_ADDSS, 0, 0 },
  { MASK_SSE1, CODE_FOR_vmsubv4sf3,  "__builtin_ia32_subss", IX86_BUILTIN_SUBSS, 0, 0 },
  { MASK_SSE1, CODE_FOR_vmmulv4sf3,  "__builtin_ia32_mulss", IX86_BUILTIN_MULSS, 0, 0 },
  { MASK_SSE1, CODE_FOR_vmdivv4sf3,  "__builtin_ia32_divss", IX86_BUILTIN_DIVSS, 0, 0 },

  { MASK_SSE1, CODE_FOR_maskcmpv4sf3, "__builtin_ia32_cmpeqps", IX86_BUILTIN_CMPEQPS, EQ, 0 },
  { MASK_SSE1, CODE_FOR_maskcmpv4sf3, "__builtin_ia32_cmpltps", IX86_BUILTIN_CMPLTPS, LT, 0 },
  { MASK_SSE1, CODE_FOR_maskcmpv4sf3, "__builtin_ia32_cmpleps", IX86_BUILTIN_CMPLEPS, LE, 0 },
  { MASK_SSE1, CODE_FOR_maskcmpv4sf3, "__builtin_ia32_cmpgtps", IX86_BUILTIN_CMPGTPS, LT, 1 },
  { MASK_SSE1, CODE_FOR_maskcmpv4sf3, "__builtin_ia32_cmpgeps", IX86_BUILTIN_CMPGEPS, LE, 1 },
  { MASK_SSE1, CODE_FOR_maskcmpv4sf3, "__builtin_ia32_cmpunordps", IX86_BUILTIN_CMPUNORDPS, UNORDERED, 0 },
  { MASK_SSE1, CODE_FOR_maskncmpv4sf3, "__builtin_ia32_cmpneqps", IX86_BUILTIN_CMPNEQPS, EQ, 0 },
  { MASK_SSE1, CODE_FOR_maskncmpv4sf3, "__builtin_ia32_cmpnltps", IX86_BUILTIN_CMPNLTPS, LT, 0 },
  { MASK_SSE1, CODE_FOR_maskncmpv4sf3, "__builtin_ia32_cmpnleps", IX86_BUILTIN_CMPNLEPS, LE, 0 },
  { MASK_SSE1, CODE_FOR_maskncmpv4sf3, "__builtin_ia32_cmpngtps", IX86_BUILTIN_CMPNGTPS, LT, 1 },
  { MASK_SSE1, CODE_FOR_maskncmpv4sf3, "__builtin_ia32_cmpngeps", IX86_BUILTIN_CMPNGEPS, LE, 1 },
  { MASK_SSE1, CODE_FOR_maskncmpv4sf3, "__builtin_ia32_cmpordps", IX86_BUILTIN_CMPORDPS, UNORDERED, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskcmpv4sf3, "__builtin_ia32_cmpeqss", IX86_BUILTIN_CMPEQSS, EQ, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskcmpv4sf3, "__builtin_ia32_cmpltss", IX86_BUILTIN_CMPLTSS, LT, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskcmpv4sf3, "__builtin_ia32_cmpless", IX86_BUILTIN_CMPLESS, LE, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskcmpv4sf3, "__builtin_ia32_cmpunordss", IX86_BUILTIN_CMPUNORDSS, UNORDERED, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskncmpv4sf3, "__builtin_ia32_cmpneqss", IX86_BUILTIN_CMPNEQSS, EQ, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskncmpv4sf3, "__builtin_ia32_cmpnltss", IX86_BUILTIN_CMPNLTSS, LT, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskncmpv4sf3, "__builtin_ia32_cmpnless", IX86_BUILTIN_CMPNLESS, LE, 0 },
  { MASK_SSE1, CODE_FOR_vmmaskncmpv4sf3, "__builtin_ia32_cmpordss", IX86_BUILTIN_CMPORDSS, UNORDERED, 0 },

  { MASK_SSE1, CODE_FOR_sminv4sf3, "__builtin_ia32_minps", IX86_BUILTIN_MINPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_smaxv4sf3, "__builtin_ia32_maxps", IX86_BUILTIN_MAXPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_vmsminv4sf3, "__builtin_ia32_minss", IX86_BUILTIN_MINSS, 0, 0 },
  { MASK_SSE1, CODE_FOR_vmsmaxv4sf3, "__builtin_ia32_maxss", IX86_BUILTIN_MAXSS, 0, 0 },

  { MASK_SSE1, CODE_FOR_sse_andv4sf3, "__builtin_ia32_andps", IX86_BUILTIN_ANDPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_nandv4sf3,  "__builtin_ia32_andnps", IX86_BUILTIN_ANDNPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_iorv4sf3, "__builtin_ia32_orps", IX86_BUILTIN_ORPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_xorv4sf3,  "__builtin_ia32_xorps", IX86_BUILTIN_XORPS, 0, 0 },

  { MASK_SSE1, CODE_FOR_sse_movss,  "__builtin_ia32_movss", IX86_BUILTIN_MOVSS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_movhlps,  "__builtin_ia32_movhlps", IX86_BUILTIN_MOVHLPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_movlhps,  "__builtin_ia32_movlhps", IX86_BUILTIN_MOVLHPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_unpckhps, "__builtin_ia32_unpckhps", IX86_BUILTIN_UNPCKHPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_unpcklps, "__builtin_ia32_unpcklps", IX86_BUILTIN_UNPCKLPS, 0, 0 },

  /* MMX */
  { MASK_MMX, CODE_FOR_addv8qi3, "__builtin_ia32_paddb", IX86_BUILTIN_PADDB, 0, 0 },
  { MASK_MMX, CODE_FOR_addv4hi3, "__builtin_ia32_paddw", IX86_BUILTIN_PADDW, 0, 0 },
  { MASK_MMX, CODE_FOR_addv2si3, "__builtin_ia32_paddd", IX86_BUILTIN_PADDD, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_adddi3, "__builtin_ia32_paddq", IX86_BUILTIN_PADDQ, 0, 0 },
  { MASK_MMX, CODE_FOR_subv8qi3, "__builtin_ia32_psubb", IX86_BUILTIN_PSUBB, 0, 0 },
  { MASK_MMX, CODE_FOR_subv4hi3, "__builtin_ia32_psubw", IX86_BUILTIN_PSUBW, 0, 0 },
  { MASK_MMX, CODE_FOR_subv2si3, "__builtin_ia32_psubd", IX86_BUILTIN_PSUBD, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_subdi3, "__builtin_ia32_psubq", IX86_BUILTIN_PSUBQ, 0, 0 },

  { MASK_MMX, CODE_FOR_ssaddv8qi3, "__builtin_ia32_paddsb", IX86_BUILTIN_PADDSB, 0, 0 },
  { MASK_MMX, CODE_FOR_ssaddv4hi3, "__builtin_ia32_paddsw", IX86_BUILTIN_PADDSW, 0, 0 },
  { MASK_MMX, CODE_FOR_sssubv8qi3, "__builtin_ia32_psubsb", IX86_BUILTIN_PSUBSB, 0, 0 },
  { MASK_MMX, CODE_FOR_sssubv4hi3, "__builtin_ia32_psubsw", IX86_BUILTIN_PSUBSW, 0, 0 },
  { MASK_MMX, CODE_FOR_usaddv8qi3, "__builtin_ia32_paddusb", IX86_BUILTIN_PADDUSB, 0, 0 },
  { MASK_MMX, CODE_FOR_usaddv4hi3, "__builtin_ia32_paddusw", IX86_BUILTIN_PADDUSW, 0, 0 },
  { MASK_MMX, CODE_FOR_ussubv8qi3, "__builtin_ia32_psubusb", IX86_BUILTIN_PSUBUSB, 0, 0 },
  { MASK_MMX, CODE_FOR_ussubv4hi3, "__builtin_ia32_psubusw", IX86_BUILTIN_PSUBUSW, 0, 0 },

  { MASK_MMX, CODE_FOR_mulv4hi3, "__builtin_ia32_pmullw", IX86_BUILTIN_PMULLW, 0, 0 },
  { MASK_MMX, CODE_FOR_smulv4hi3_highpart, "__builtin_ia32_pmulhw", IX86_BUILTIN_PMULHW, 0, 0 },
  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_umulv4hi3_highpart, "__builtin_ia32_pmulhuw", IX86_BUILTIN_PMULHUW, 0, 0 },

  { MASK_MMX, CODE_FOR_mmx_anddi3, "__builtin_ia32_pand", IX86_BUILTIN_PAND, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_nanddi3, "__builtin_ia32_pandn", IX86_BUILTIN_PANDN, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_iordi3, "__builtin_ia32_por", IX86_BUILTIN_POR, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_xordi3, "__builtin_ia32_pxor", IX86_BUILTIN_PXOR, 0, 0 },

  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_mmx_uavgv8qi3, "__builtin_ia32_pavgb", IX86_BUILTIN_PAVGB, 0, 0 },
  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_mmx_uavgv4hi3, "__builtin_ia32_pavgw", IX86_BUILTIN_PAVGW, 0, 0 },

  { MASK_MMX, CODE_FOR_eqv8qi3, "__builtin_ia32_pcmpeqb", IX86_BUILTIN_PCMPEQB, 0, 0 },
  { MASK_MMX, CODE_FOR_eqv4hi3, "__builtin_ia32_pcmpeqw", IX86_BUILTIN_PCMPEQW, 0, 0 },
  { MASK_MMX, CODE_FOR_eqv2si3, "__builtin_ia32_pcmpeqd", IX86_BUILTIN_PCMPEQD, 0, 0 },
  { MASK_MMX, CODE_FOR_gtv8qi3, "__builtin_ia32_pcmpgtb", IX86_BUILTIN_PCMPGTB, 0, 0 },
  { MASK_MMX, CODE_FOR_gtv4hi3, "__builtin_ia32_pcmpgtw", IX86_BUILTIN_PCMPGTW, 0, 0 },
  { MASK_MMX, CODE_FOR_gtv2si3, "__builtin_ia32_pcmpgtd", IX86_BUILTIN_PCMPGTD, 0, 0 },

  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_umaxv8qi3, "__builtin_ia32_pmaxub", IX86_BUILTIN_PMAXUB, 0, 0 },
  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_smaxv4hi3, "__builtin_ia32_pmaxsw", IX86_BUILTIN_PMAXSW, 0, 0 },
  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_uminv8qi3, "__builtin_ia32_pminub", IX86_BUILTIN_PMINUB, 0, 0 },
  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_sminv4hi3, "__builtin_ia32_pminsw", IX86_BUILTIN_PMINSW, 0, 0 },

  { MASK_MMX, CODE_FOR_mmx_punpckhbw, "__builtin_ia32_punpckhbw", IX86_BUILTIN_PUNPCKHBW, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_punpckhwd, "__builtin_ia32_punpckhwd", IX86_BUILTIN_PUNPCKHWD, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_punpckhdq, "__builtin_ia32_punpckhdq", IX86_BUILTIN_PUNPCKHDQ, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_punpcklbw, "__builtin_ia32_punpcklbw", IX86_BUILTIN_PUNPCKLBW, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_punpcklwd, "__builtin_ia32_punpcklwd", IX86_BUILTIN_PUNPCKLWD, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_punpckldq, "__builtin_ia32_punpckldq", IX86_BUILTIN_PUNPCKLDQ, 0, 0 },

  /* Special.  */
  { MASK_MMX, CODE_FOR_mmx_packsswb, 0, IX86_BUILTIN_PACKSSWB, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_packssdw, 0, IX86_BUILTIN_PACKSSDW, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_packuswb, 0, IX86_BUILTIN_PACKUSWB, 0, 0 },

  { MASK_SSE1, CODE_FOR_cvtpi2ps, 0, IX86_BUILTIN_CVTPI2PS, 0, 0 },
  { MASK_SSE1, CODE_FOR_cvtsi2ss, 0, IX86_BUILTIN_CVTSI2SS, 0, 0 },
  { MASK_SSE164, CODE_FOR_cvtsi2ssq, 0, IX86_BUILTIN_CVTSI642SS, 0, 0 },

  { MASK_MMX, CODE_FOR_ashlv4hi3, 0, IX86_BUILTIN_PSLLW, 0, 0 },
  { MASK_MMX, CODE_FOR_ashlv4hi3, 0, IX86_BUILTIN_PSLLWI, 0, 0 },
  { MASK_MMX, CODE_FOR_ashlv2si3, 0, IX86_BUILTIN_PSLLD, 0, 0 },
  { MASK_MMX, CODE_FOR_ashlv2si3, 0, IX86_BUILTIN_PSLLDI, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_ashldi3, 0, IX86_BUILTIN_PSLLQ, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_ashldi3, 0, IX86_BUILTIN_PSLLQI, 0, 0 },

  { MASK_MMX, CODE_FOR_lshrv4hi3, 0, IX86_BUILTIN_PSRLW, 0, 0 },
  { MASK_MMX, CODE_FOR_lshrv4hi3, 0, IX86_BUILTIN_PSRLWI, 0, 0 },
  { MASK_MMX, CODE_FOR_lshrv2si3, 0, IX86_BUILTIN_PSRLD, 0, 0 },
  { MASK_MMX, CODE_FOR_lshrv2si3, 0, IX86_BUILTIN_PSRLDI, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_lshrdi3, 0, IX86_BUILTIN_PSRLQ, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_lshrdi3, 0, IX86_BUILTIN_PSRLQI, 0, 0 },

  { MASK_MMX, CODE_FOR_ashrv4hi3, 0, IX86_BUILTIN_PSRAW, 0, 0 },
  { MASK_MMX, CODE_FOR_ashrv4hi3, 0, IX86_BUILTIN_PSRAWI, 0, 0 },
  { MASK_MMX, CODE_FOR_ashrv2si3, 0, IX86_BUILTIN_PSRAD, 0, 0 },
  { MASK_MMX, CODE_FOR_ashrv2si3, 0, IX86_BUILTIN_PSRADI, 0, 0 },

  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_mmx_psadbw, 0, IX86_BUILTIN_PSADBW, 0, 0 },
  { MASK_MMX, CODE_FOR_mmx_pmaddwd, 0, IX86_BUILTIN_PMADDWD, 0, 0 },

  /* SSE2 */
  { MASK_SSE2, CODE_FOR_addv2df3, "__builtin_ia32_addpd", IX86_BUILTIN_ADDPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_subv2df3, "__builtin_ia32_subpd", IX86_BUILTIN_SUBPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_mulv2df3, "__builtin_ia32_mulpd", IX86_BUILTIN_MULPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_divv2df3, "__builtin_ia32_divpd", IX86_BUILTIN_DIVPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_vmaddv2df3,  "__builtin_ia32_addsd", IX86_BUILTIN_ADDSD, 0, 0 },
  { MASK_SSE2, CODE_FOR_vmsubv2df3,  "__builtin_ia32_subsd", IX86_BUILTIN_SUBSD, 0, 0 },
  { MASK_SSE2, CODE_FOR_vmmulv2df3,  "__builtin_ia32_mulsd", IX86_BUILTIN_MULSD, 0, 0 },
  { MASK_SSE2, CODE_FOR_vmdivv2df3,  "__builtin_ia32_divsd", IX86_BUILTIN_DIVSD, 0, 0 },

  { MASK_SSE2, CODE_FOR_maskcmpv2df3, "__builtin_ia32_cmpeqpd", IX86_BUILTIN_CMPEQPD, EQ, 0 },
  { MASK_SSE2, CODE_FOR_maskcmpv2df3, "__builtin_ia32_cmpltpd", IX86_BUILTIN_CMPLTPD, LT, 0 },
  { MASK_SSE2, CODE_FOR_maskcmpv2df3, "__builtin_ia32_cmplepd", IX86_BUILTIN_CMPLEPD, LE, 0 },
  { MASK_SSE2, CODE_FOR_maskcmpv2df3, "__builtin_ia32_cmpgtpd", IX86_BUILTIN_CMPGTPD, LT, 1 },
  { MASK_SSE2, CODE_FOR_maskcmpv2df3, "__builtin_ia32_cmpgepd", IX86_BUILTIN_CMPGEPD, LE, 1 },
  { MASK_SSE2, CODE_FOR_maskcmpv2df3, "__builtin_ia32_cmpunordpd", IX86_BUILTIN_CMPUNORDPD, UNORDERED, 0 },
  { MASK_SSE2, CODE_FOR_maskncmpv2df3, "__builtin_ia32_cmpneqpd", IX86_BUILTIN_CMPNEQPD, EQ, 0 },
  { MASK_SSE2, CODE_FOR_maskncmpv2df3, "__builtin_ia32_cmpnltpd", IX86_BUILTIN_CMPNLTPD, LT, 0 },
  { MASK_SSE2, CODE_FOR_maskncmpv2df3, "__builtin_ia32_cmpnlepd", IX86_BUILTIN_CMPNLEPD, LE, 0 },
  { MASK_SSE2, CODE_FOR_maskncmpv2df3, "__builtin_ia32_cmpngtpd", IX86_BUILTIN_CMPNGTPD, LT, 1 },
  { MASK_SSE2, CODE_FOR_maskncmpv2df3, "__builtin_ia32_cmpngepd", IX86_BUILTIN_CMPNGEPD, LE, 1 },
  { MASK_SSE2, CODE_FOR_maskncmpv2df3, "__builtin_ia32_cmpordpd", IX86_BUILTIN_CMPORDPD, UNORDERED, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskcmpv2df3, "__builtin_ia32_cmpeqsd", IX86_BUILTIN_CMPEQSD, EQ, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskcmpv2df3, "__builtin_ia32_cmpltsd", IX86_BUILTIN_CMPLTSD, LT, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskcmpv2df3, "__builtin_ia32_cmplesd", IX86_BUILTIN_CMPLESD, LE, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskcmpv2df3, "__builtin_ia32_cmpunordsd", IX86_BUILTIN_CMPUNORDSD, UNORDERED, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskncmpv2df3, "__builtin_ia32_cmpneqsd", IX86_BUILTIN_CMPNEQSD, EQ, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskncmpv2df3, "__builtin_ia32_cmpnltsd", IX86_BUILTIN_CMPNLTSD, LT, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskncmpv2df3, "__builtin_ia32_cmpnlesd", IX86_BUILTIN_CMPNLESD, LE, 0 },
  { MASK_SSE2, CODE_FOR_vmmaskncmpv2df3, "__builtin_ia32_cmpordsd", IX86_BUILTIN_CMPORDSD, UNORDERED, 0 },

  { MASK_SSE2, CODE_FOR_sminv2df3, "__builtin_ia32_minpd", IX86_BUILTIN_MINPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_smaxv2df3, "__builtin_ia32_maxpd", IX86_BUILTIN_MAXPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_vmsminv2df3, "__builtin_ia32_minsd", IX86_BUILTIN_MINSD, 0, 0 },
  { MASK_SSE2, CODE_FOR_vmsmaxv2df3, "__builtin_ia32_maxsd", IX86_BUILTIN_MAXSD, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_andv2df3, "__builtin_ia32_andpd", IX86_BUILTIN_ANDPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_nandv2df3,  "__builtin_ia32_andnpd", IX86_BUILTIN_ANDNPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_iorv2df3, "__builtin_ia32_orpd", IX86_BUILTIN_ORPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_xorv2df3,  "__builtin_ia32_xorpd", IX86_BUILTIN_XORPD, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_movsd,  "__builtin_ia32_movsd", IX86_BUILTIN_MOVSD, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_unpckhpd, "__builtin_ia32_unpckhpd", IX86_BUILTIN_UNPCKHPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_unpcklpd, "__builtin_ia32_unpcklpd", IX86_BUILTIN_UNPCKLPD, 0, 0 },

  /* SSE2 MMX */
  { MASK_SSE2, CODE_FOR_addv16qi3, "__builtin_ia32_paddb128", IX86_BUILTIN_PADDB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_addv8hi3, "__builtin_ia32_paddw128", IX86_BUILTIN_PADDW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_addv4si3, "__builtin_ia32_paddd128", IX86_BUILTIN_PADDD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_addv2di3, "__builtin_ia32_paddq128", IX86_BUILTIN_PADDQ128, 0, 0 },
  { MASK_SSE2, CODE_FOR_subv16qi3, "__builtin_ia32_psubb128", IX86_BUILTIN_PSUBB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_subv8hi3, "__builtin_ia32_psubw128", IX86_BUILTIN_PSUBW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_subv4si3, "__builtin_ia32_psubd128", IX86_BUILTIN_PSUBD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_subv2di3, "__builtin_ia32_psubq128", IX86_BUILTIN_PSUBQ128, 0, 0 },

  { MASK_MMX, CODE_FOR_ssaddv16qi3, "__builtin_ia32_paddsb128", IX86_BUILTIN_PADDSB128, 0, 0 },
  { MASK_MMX, CODE_FOR_ssaddv8hi3, "__builtin_ia32_paddsw128", IX86_BUILTIN_PADDSW128, 0, 0 },
  { MASK_MMX, CODE_FOR_sssubv16qi3, "__builtin_ia32_psubsb128", IX86_BUILTIN_PSUBSB128, 0, 0 },
  { MASK_MMX, CODE_FOR_sssubv8hi3, "__builtin_ia32_psubsw128", IX86_BUILTIN_PSUBSW128, 0, 0 },
  { MASK_MMX, CODE_FOR_usaddv16qi3, "__builtin_ia32_paddusb128", IX86_BUILTIN_PADDUSB128, 0, 0 },
  { MASK_MMX, CODE_FOR_usaddv8hi3, "__builtin_ia32_paddusw128", IX86_BUILTIN_PADDUSW128, 0, 0 },
  { MASK_MMX, CODE_FOR_ussubv16qi3, "__builtin_ia32_psubusb128", IX86_BUILTIN_PSUBUSB128, 0, 0 },
  { MASK_MMX, CODE_FOR_ussubv8hi3, "__builtin_ia32_psubusw128", IX86_BUILTIN_PSUBUSW128, 0, 0 },

  { MASK_SSE2, CODE_FOR_mulv8hi3, "__builtin_ia32_pmullw128", IX86_BUILTIN_PMULLW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_smulv8hi3_highpart, "__builtin_ia32_pmulhw128", IX86_BUILTIN_PMULHW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_umulsidi3, "__builtin_ia32_pmuludq", IX86_BUILTIN_PMULUDQ, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_umulv2siv2di3, "__builtin_ia32_pmuludq128", IX86_BUILTIN_PMULUDQ128, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_andv2di3, "__builtin_ia32_pand128", IX86_BUILTIN_PAND128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_nandv2di3, "__builtin_ia32_pandn128", IX86_BUILTIN_PANDN128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_iorv2di3, "__builtin_ia32_por128", IX86_BUILTIN_POR128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_xorv2di3, "__builtin_ia32_pxor128", IX86_BUILTIN_PXOR128, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_uavgv16qi3, "__builtin_ia32_pavgb128", IX86_BUILTIN_PAVGB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_uavgv8hi3, "__builtin_ia32_pavgw128", IX86_BUILTIN_PAVGW128, 0, 0 },

  { MASK_SSE2, CODE_FOR_eqv16qi3, "__builtin_ia32_pcmpeqb128", IX86_BUILTIN_PCMPEQB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_eqv8hi3, "__builtin_ia32_pcmpeqw128", IX86_BUILTIN_PCMPEQW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_eqv4si3, "__builtin_ia32_pcmpeqd128", IX86_BUILTIN_PCMPEQD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_gtv16qi3, "__builtin_ia32_pcmpgtb128", IX86_BUILTIN_PCMPGTB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_gtv8hi3, "__builtin_ia32_pcmpgtw128", IX86_BUILTIN_PCMPGTW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_gtv4si3, "__builtin_ia32_pcmpgtd128", IX86_BUILTIN_PCMPGTD128, 0, 0 },

  { MASK_SSE2, CODE_FOR_umaxv16qi3, "__builtin_ia32_pmaxub128", IX86_BUILTIN_PMAXUB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_smaxv8hi3, "__builtin_ia32_pmaxsw128", IX86_BUILTIN_PMAXSW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_uminv16qi3, "__builtin_ia32_pminub128", IX86_BUILTIN_PMINUB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sminv8hi3, "__builtin_ia32_pminsw128", IX86_BUILTIN_PMINSW128, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_punpckhbw, "__builtin_ia32_punpckhbw128", IX86_BUILTIN_PUNPCKHBW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpckhwd, "__builtin_ia32_punpckhwd128", IX86_BUILTIN_PUNPCKHWD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpckhdq, "__builtin_ia32_punpckhdq128", IX86_BUILTIN_PUNPCKHDQ128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpckhqdq, "__builtin_ia32_punpckhqdq128", IX86_BUILTIN_PUNPCKHQDQ128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpcklbw, "__builtin_ia32_punpcklbw128", IX86_BUILTIN_PUNPCKLBW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpcklwd, "__builtin_ia32_punpcklwd128", IX86_BUILTIN_PUNPCKLWD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpckldq, "__builtin_ia32_punpckldq128", IX86_BUILTIN_PUNPCKLDQ128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_punpcklqdq, "__builtin_ia32_punpcklqdq128", IX86_BUILTIN_PUNPCKLQDQ128, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_packsswb, "__builtin_ia32_packsswb128", IX86_BUILTIN_PACKSSWB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_packssdw, "__builtin_ia32_packssdw128", IX86_BUILTIN_PACKSSDW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_packuswb, "__builtin_ia32_packuswb128", IX86_BUILTIN_PACKUSWB128, 0, 0 },

  { MASK_SSE2, CODE_FOR_umulv8hi3_highpart, "__builtin_ia32_pmulhuw128", IX86_BUILTIN_PMULHUW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_psadbw, 0, IX86_BUILTIN_PSADBW128, 0, 0 },

  { MASK_SSE2, CODE_FOR_ashlv8hi3_ti, 0, IX86_BUILTIN_PSLLW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashlv8hi3, 0, IX86_BUILTIN_PSLLWI128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashlv4si3_ti, 0, IX86_BUILTIN_PSLLD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashlv4si3, 0, IX86_BUILTIN_PSLLDI128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashlv2di3_ti, 0, IX86_BUILTIN_PSLLQ128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashlv2di3, 0, IX86_BUILTIN_PSLLQI128, 0, 0 },

  { MASK_SSE2, CODE_FOR_lshrv8hi3_ti, 0, IX86_BUILTIN_PSRLW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_lshrv8hi3, 0, IX86_BUILTIN_PSRLWI128, 0, 0 },
  { MASK_SSE2, CODE_FOR_lshrv4si3_ti, 0, IX86_BUILTIN_PSRLD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_lshrv4si3, 0, IX86_BUILTIN_PSRLDI128, 0, 0 },
  { MASK_SSE2, CODE_FOR_lshrv2di3_ti, 0, IX86_BUILTIN_PSRLQ128, 0, 0 },
  { MASK_SSE2, CODE_FOR_lshrv2di3, 0, IX86_BUILTIN_PSRLQI128, 0, 0 },

  { MASK_SSE2, CODE_FOR_ashrv8hi3_ti, 0, IX86_BUILTIN_PSRAW128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashrv8hi3, 0, IX86_BUILTIN_PSRAWI128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashrv4si3_ti, 0, IX86_BUILTIN_PSRAD128, 0, 0 },
  { MASK_SSE2, CODE_FOR_ashrv4si3, 0, IX86_BUILTIN_PSRADI128, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_pmaddwd, 0, IX86_BUILTIN_PMADDWD128, 0, 0 },

  { MASK_SSE2, CODE_FOR_cvtsi2sd, 0, IX86_BUILTIN_CVTSI2SD, 0, 0 },
  { MASK_SSE264, CODE_FOR_cvtsi2sdq, 0, IX86_BUILTIN_CVTSI642SD, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvtsd2ss, 0, IX86_BUILTIN_CVTSD2SS, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvtss2sd, 0, IX86_BUILTIN_CVTSS2SD, 0, 0 }
};

static const struct builtin_description bdesc_1arg[] =
{
  { MASK_SSE1 | MASK_3DNOW_A, CODE_FOR_mmx_pmovmskb, 0, IX86_BUILTIN_PMOVMSKB, 0, 0 },
  { MASK_SSE1, CODE_FOR_sse_movmskps, 0, IX86_BUILTIN_MOVMSKPS, 0, 0 },

  { MASK_SSE1, CODE_FOR_sqrtv4sf2, 0, IX86_BUILTIN_SQRTPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_rsqrtv4sf2, 0, IX86_BUILTIN_RSQRTPS, 0, 0 },
  { MASK_SSE1, CODE_FOR_rcpv4sf2, 0, IX86_BUILTIN_RCPPS, 0, 0 },

  { MASK_SSE1, CODE_FOR_cvtps2pi, 0, IX86_BUILTIN_CVTPS2PI, 0, 0 },
  { MASK_SSE1, CODE_FOR_cvtss2si, 0, IX86_BUILTIN_CVTSS2SI, 0, 0 },
  { MASK_SSE164, CODE_FOR_cvtss2siq, 0, IX86_BUILTIN_CVTSS2SI64, 0, 0 },
  { MASK_SSE1, CODE_FOR_cvttps2pi, 0, IX86_BUILTIN_CVTTPS2PI, 0, 0 },
  { MASK_SSE1, CODE_FOR_cvttss2si, 0, IX86_BUILTIN_CVTTSS2SI, 0, 0 },
  { MASK_SSE164, CODE_FOR_cvttss2siq, 0, IX86_BUILTIN_CVTTSS2SI64, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_pmovmskb, 0, IX86_BUILTIN_PMOVMSKB128, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_movmskpd, 0, IX86_BUILTIN_MOVMSKPD, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_movq2dq, 0, IX86_BUILTIN_MOVQ2DQ, 0, 0 },
  { MASK_SSE2, CODE_FOR_sse2_movdq2q, 0, IX86_BUILTIN_MOVDQ2Q, 0, 0 },

  { MASK_SSE2, CODE_FOR_sqrtv2df2, 0, IX86_BUILTIN_SQRTPD, 0, 0 },

  { MASK_SSE2, CODE_FOR_cvtdq2pd, 0, IX86_BUILTIN_CVTDQ2PD, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvtdq2ps, 0, IX86_BUILTIN_CVTDQ2PS, 0, 0 },

  { MASK_SSE2, CODE_FOR_cvtpd2dq, 0, IX86_BUILTIN_CVTPD2DQ, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvtpd2pi, 0, IX86_BUILTIN_CVTPD2PI, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvtpd2ps, 0, IX86_BUILTIN_CVTPD2PS, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvttpd2dq, 0, IX86_BUILTIN_CVTTPD2DQ, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvttpd2pi, 0, IX86_BUILTIN_CVTTPD2PI, 0, 0 },

  { MASK_SSE2, CODE_FOR_cvtpi2pd, 0, IX86_BUILTIN_CVTPI2PD, 0, 0 },

  { MASK_SSE2, CODE_FOR_cvtsd2si, 0, IX86_BUILTIN_CVTSD2SI, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvttsd2si, 0, IX86_BUILTIN_CVTTSD2SI, 0, 0 },
  { MASK_SSE264, CODE_FOR_cvtsd2siq, 0, IX86_BUILTIN_CVTSD2SI64, 0, 0 },
  { MASK_SSE264, CODE_FOR_cvttsd2siq, 0, IX86_BUILTIN_CVTTSD2SI64, 0, 0 },

  { MASK_SSE2, CODE_FOR_cvtps2dq, 0, IX86_BUILTIN_CVTPS2DQ, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvtps2pd, 0, IX86_BUILTIN_CVTPS2PD, 0, 0 },
  { MASK_SSE2, CODE_FOR_cvttps2dq, 0, IX86_BUILTIN_CVTTPS2DQ, 0, 0 },

  { MASK_SSE2, CODE_FOR_sse2_movq, 0, IX86_BUILTIN_MOVQ, 0, 0 }
};

void
ix86_init_builtins ()
{
  if (TARGET_MMX)
    ix86_init_mmx_sse_builtins ();
}

/* Set up all the MMX/SSE builtins.  This is not called if TARGET_MMX
   is zero.  Otherwise, if TARGET_SSE is not set, only expand the MMX
   builtins.  */
static void
ix86_init_mmx_sse_builtins ()
{
  const struct builtin_description * d;
  size_t i;

  tree pchar_type_node = build_pointer_type (char_type_node);
  tree pcchar_type_node = build_pointer_type (
			     build_type_variant (char_type_node, 1, 0));
  tree pfloat_type_node = build_pointer_type (float_type_node);
  tree pcfloat_type_node = build_pointer_type (
			     build_type_variant (float_type_node, 1, 0));
  tree pv2si_type_node = build_pointer_type (V2SI_type_node);
  tree pv2di_type_node = build_pointer_type (V2DI_type_node);
  tree pdi_type_node = build_pointer_type (long_long_unsigned_type_node);

  /* Comparisons.  */
  tree int_ftype_v4sf_v4sf
    = build_function_type_list (integer_type_node,
				V4SF_type_node, V4SF_type_node, NULL_TREE);
  tree v4si_ftype_v4sf_v4sf
    = build_function_type_list (V4SI_type_node,
				V4SF_type_node, V4SF_type_node, NULL_TREE);
  /* MMX/SSE/integer conversions.  */
  tree int_ftype_v4sf
    = build_function_type_list (integer_type_node,
				V4SF_type_node, NULL_TREE);
  tree int64_ftype_v4sf
    = build_function_type_list (long_long_integer_type_node,
				V4SF_type_node, NULL_TREE);
  tree int_ftype_v8qi
    = build_function_type_list (integer_type_node, V8QI_type_node, NULL_TREE);
  tree v4sf_ftype_v4sf_int
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, integer_type_node, NULL_TREE);
  tree v4sf_ftype_v4sf_int64
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, long_long_integer_type_node,
				NULL_TREE);
  tree v4sf_ftype_v4sf_v2si
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, V2SI_type_node, NULL_TREE);
  tree int_ftype_v4hi_int
    = build_function_type_list (integer_type_node,
				V4HI_type_node, integer_type_node, NULL_TREE);
  tree v4hi_ftype_v4hi_int_int
    = build_function_type_list (V4HI_type_node, V4HI_type_node,
				integer_type_node, integer_type_node,
				NULL_TREE);
  /* Miscellaneous.  */
  tree v8qi_ftype_v4hi_v4hi
    = build_function_type_list (V8QI_type_node,
				V4HI_type_node, V4HI_type_node, NULL_TREE);
  tree v4hi_ftype_v2si_v2si
    = build_function_type_list (V4HI_type_node,
				V2SI_type_node, V2SI_type_node, NULL_TREE);
  tree v4sf_ftype_v4sf_v4sf_int
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, V4SF_type_node,
				integer_type_node, NULL_TREE);
  tree v2si_ftype_v4hi_v4hi
    = build_function_type_list (V2SI_type_node,
				V4HI_type_node, V4HI_type_node, NULL_TREE);
  tree v4hi_ftype_v4hi_int
    = build_function_type_list (V4HI_type_node,
				V4HI_type_node, integer_type_node, NULL_TREE);
  tree v4hi_ftype_v4hi_di
    = build_function_type_list (V4HI_type_node,
				V4HI_type_node, long_long_unsigned_type_node,
				NULL_TREE);
  tree v2si_ftype_v2si_di
    = build_function_type_list (V2SI_type_node,
				V2SI_type_node, long_long_unsigned_type_node,
				NULL_TREE);
  tree void_ftype_void
    = build_function_type (void_type_node, void_list_node);
  tree void_ftype_unsigned
    = build_function_type_list (void_type_node, unsigned_type_node, NULL_TREE);
  tree unsigned_ftype_void
    = build_function_type (unsigned_type_node, void_list_node);
  tree di_ftype_void
    = build_function_type (long_long_unsigned_type_node, void_list_node);
  tree v4sf_ftype_void
    = build_function_type (V4SF_type_node, void_list_node);
  tree v2si_ftype_v4sf
    = build_function_type_list (V2SI_type_node, V4SF_type_node, NULL_TREE);
  /* Loads/stores.  */
  tree void_ftype_v8qi_v8qi_pchar
    = build_function_type_list (void_type_node,
				V8QI_type_node, V8QI_type_node,
				pchar_type_node, NULL_TREE);
  tree v4sf_ftype_pcfloat
    = build_function_type_list (V4SF_type_node, pcfloat_type_node, NULL_TREE);
  /* @@@ the type is bogus */
  tree v4sf_ftype_v4sf_pv2si
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, pv2si_type_node, NULL_TREE);
  tree void_ftype_pv2si_v4sf
    = build_function_type_list (void_type_node,
				pv2si_type_node, V4SF_type_node, NULL_TREE);
  tree void_ftype_pfloat_v4sf
    = build_function_type_list (void_type_node,
				pfloat_type_node, V4SF_type_node, NULL_TREE);
  tree void_ftype_pdi_di
    = build_function_type_list (void_type_node,
				pdi_type_node, long_long_unsigned_type_node,
				NULL_TREE);
  tree void_ftype_pv2di_v2di
    = build_function_type_list (void_type_node,
				pv2di_type_node, V2DI_type_node, NULL_TREE);
  /* Normal vector unops.  */
  tree v4sf_ftype_v4sf
    = build_function_type_list (V4SF_type_node, V4SF_type_node, NULL_TREE);

  /* Normal vector binops.  */
  tree v4sf_ftype_v4sf_v4sf
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, V4SF_type_node, NULL_TREE);
  tree v8qi_ftype_v8qi_v8qi
    = build_function_type_list (V8QI_type_node,
				V8QI_type_node, V8QI_type_node, NULL_TREE);
  tree v4hi_ftype_v4hi_v4hi
    = build_function_type_list (V4HI_type_node,
				V4HI_type_node, V4HI_type_node, NULL_TREE);
  tree v2si_ftype_v2si_v2si
    = build_function_type_list (V2SI_type_node,
				V2SI_type_node, V2SI_type_node, NULL_TREE);
  tree di_ftype_di_di
    = build_function_type_list (long_long_unsigned_type_node,
				long_long_unsigned_type_node,
				long_long_unsigned_type_node, NULL_TREE);

  tree v2si_ftype_v2sf
    = build_function_type_list (V2SI_type_node, V2SF_type_node, NULL_TREE);
  tree v2sf_ftype_v2si
    = build_function_type_list (V2SF_type_node, V2SI_type_node, NULL_TREE);
  tree v2si_ftype_v2si
    = build_function_type_list (V2SI_type_node, V2SI_type_node, NULL_TREE);
  tree v2sf_ftype_v2sf
    = build_function_type_list (V2SF_type_node, V2SF_type_node, NULL_TREE);
  tree v2sf_ftype_v2sf_v2sf
    = build_function_type_list (V2SF_type_node,
				V2SF_type_node, V2SF_type_node, NULL_TREE);
  tree v2si_ftype_v2sf_v2sf
    = build_function_type_list (V2SI_type_node,
				V2SF_type_node, V2SF_type_node, NULL_TREE);
  tree pint_type_node    = build_pointer_type (integer_type_node);
  tree pcint_type_node = build_pointer_type (
			     build_type_variant (integer_type_node, 1, 0));
  tree pdouble_type_node = build_pointer_type (double_type_node);
  tree pcdouble_type_node = build_pointer_type (
				build_type_variant (double_type_node, 1, 0));
  tree int_ftype_v2df_v2df
    = build_function_type_list (integer_type_node,
				V2DF_type_node, V2DF_type_node, NULL_TREE);

  tree ti_ftype_void
    = build_function_type (intTI_type_node, void_list_node);
  tree v2di_ftype_void
    = build_function_type (V2DI_type_node, void_list_node);
  tree ti_ftype_ti_ti
    = build_function_type_list (intTI_type_node,
				intTI_type_node, intTI_type_node, NULL_TREE);
  tree void_ftype_pcvoid
    = build_function_type_list (void_type_node, const_ptr_type_node, NULL_TREE);
  tree v2di_ftype_di
    = build_function_type_list (V2DI_type_node,
				long_long_unsigned_type_node, NULL_TREE);
  tree di_ftype_v2di
    = build_function_type_list (long_long_unsigned_type_node,
				V2DI_type_node, NULL_TREE);
  tree v4sf_ftype_v4si
    = build_function_type_list (V4SF_type_node, V4SI_type_node, NULL_TREE);
  tree v4si_ftype_v4sf
    = build_function_type_list (V4SI_type_node, V4SF_type_node, NULL_TREE);
  tree v2df_ftype_v4si
    = build_function_type_list (V2DF_type_node, V4SI_type_node, NULL_TREE);
  tree v4si_ftype_v2df
    = build_function_type_list (V4SI_type_node, V2DF_type_node, NULL_TREE);
  tree v2si_ftype_v2df
    = build_function_type_list (V2SI_type_node, V2DF_type_node, NULL_TREE);
  tree v4sf_ftype_v2df
    = build_function_type_list (V4SF_type_node, V2DF_type_node, NULL_TREE);
  tree v2df_ftype_v2si
    = build_function_type_list (V2DF_type_node, V2SI_type_node, NULL_TREE);
  tree v2df_ftype_v4sf
    = build_function_type_list (V2DF_type_node, V4SF_type_node, NULL_TREE);
  tree int_ftype_v2df
    = build_function_type_list (integer_type_node, V2DF_type_node, NULL_TREE);
  tree int64_ftype_v2df
    = build_function_type_list (long_long_integer_type_node,
		    		V2DF_type_node, NULL_TREE);
  tree v2df_ftype_v2df_int
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, integer_type_node, NULL_TREE);
  tree v2df_ftype_v2df_int64
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, long_long_integer_type_node,
				NULL_TREE);
  tree v4sf_ftype_v4sf_v2df
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, V2DF_type_node, NULL_TREE);
  tree v2df_ftype_v2df_v4sf
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, V4SF_type_node, NULL_TREE);
  tree v2df_ftype_v2df_v2df_int
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, V2DF_type_node,
				integer_type_node,
				NULL_TREE);
  tree v2df_ftype_v2df_pv2si
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, pv2si_type_node, NULL_TREE);
  tree void_ftype_pv2si_v2df
    = build_function_type_list (void_type_node,
				pv2si_type_node, V2DF_type_node, NULL_TREE);
  tree void_ftype_pdouble_v2df
    = build_function_type_list (void_type_node,
				pdouble_type_node, V2DF_type_node, NULL_TREE);
  tree void_ftype_pint_int
    = build_function_type_list (void_type_node,
				pint_type_node, integer_type_node, NULL_TREE);
  tree void_ftype_v16qi_v16qi_pchar
    = build_function_type_list (void_type_node,
				V16QI_type_node, V16QI_type_node,
				pchar_type_node, NULL_TREE);
  tree v2df_ftype_pcdouble
    = build_function_type_list (V2DF_type_node, pcdouble_type_node, NULL_TREE);
  tree v2df_ftype_v2df_v2df
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, V2DF_type_node, NULL_TREE);
  tree v16qi_ftype_v16qi_v16qi
    = build_function_type_list (V16QI_type_node,
				V16QI_type_node, V16QI_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_v8hi
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node, V8HI_type_node, NULL_TREE);
  tree v4si_ftype_v4si_v4si
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node, V4SI_type_node, NULL_TREE);
  tree v2di_ftype_v2di_v2di
    = build_function_type_list (V2DI_type_node,
				V2DI_type_node, V2DI_type_node, NULL_TREE);
  tree v2di_ftype_v2df_v2df
    = build_function_type_list (V2DI_type_node,
				V2DF_type_node, V2DF_type_node, NULL_TREE);
  tree v2df_ftype_v2df
    = build_function_type_list (V2DF_type_node, V2DF_type_node, NULL_TREE);
  tree v2df_ftype_double
    = build_function_type_list (V2DF_type_node, double_type_node, NULL_TREE);
  tree v2df_ftype_double_double
    = build_function_type_list (V2DF_type_node,
				double_type_node, double_type_node, NULL_TREE);
  tree int_ftype_v8hi_int
    = build_function_type_list (integer_type_node,
				V8HI_type_node, integer_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_int_int
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node, integer_type_node,
				integer_type_node, NULL_TREE);
  tree v2di_ftype_v2di_int
    = build_function_type_list (V2DI_type_node,
				V2DI_type_node, integer_type_node, NULL_TREE);
  tree v4si_ftype_v4si_int
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node, integer_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_int
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node, integer_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_v2di
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node, V2DI_type_node, NULL_TREE);
  tree v4si_ftype_v4si_v2di
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node, V2DI_type_node, NULL_TREE);
  tree v4si_ftype_v8hi_v8hi
    = build_function_type_list (V4SI_type_node,
				V8HI_type_node, V8HI_type_node, NULL_TREE);
  tree di_ftype_v8qi_v8qi
    = build_function_type_list (long_long_unsigned_type_node,
				V8QI_type_node, V8QI_type_node, NULL_TREE);
  tree v2di_ftype_v16qi_v16qi
    = build_function_type_list (V2DI_type_node,
				V16QI_type_node, V16QI_type_node, NULL_TREE);
  tree int_ftype_v16qi
    = build_function_type_list (integer_type_node, V16QI_type_node, NULL_TREE);
  tree v16qi_ftype_pcchar
    = build_function_type_list (V16QI_type_node, pcchar_type_node, NULL_TREE);
  tree void_ftype_pchar_v16qi
    = build_function_type_list (void_type_node,
			        pchar_type_node, V16QI_type_node, NULL_TREE);
  tree v4si_ftype_pcint
    = build_function_type_list (V4SI_type_node, pcint_type_node, NULL_TREE);
  tree void_ftype_pcint_v4si
    = build_function_type_list (void_type_node,
			        pcint_type_node, V4SI_type_node, NULL_TREE);
  tree v2di_ftype_v2di
    = build_function_type_list (V2DI_type_node, V2DI_type_node, NULL_TREE);

  /* Add all builtins that are more or less simple operations on two
     operands.  */
  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    {
      /* Use one of the operands; the target can have a different mode for
	 mask-generating compares.  */
      enum machine_mode mode;
      tree type;

      if (d->name == 0)
	continue;
      mode = insn_data[d->icode].operand[1].mode;

      switch (mode)
	{
	case V16QImode:
	  type = v16qi_ftype_v16qi_v16qi;
	  break;
	case V8HImode:
	  type = v8hi_ftype_v8hi_v8hi;
	  break;
	case V4SImode:
	  type = v4si_ftype_v4si_v4si;
	  break;
	case V2DImode:
	  type = v2di_ftype_v2di_v2di;
	  break;
	case V2DFmode:
	  type = v2df_ftype_v2df_v2df;
	  break;
	case TImode:
	  type = ti_ftype_ti_ti;
	  break;
	case V4SFmode:
	  type = v4sf_ftype_v4sf_v4sf;
	  break;
	case V8QImode:
	  type = v8qi_ftype_v8qi_v8qi;
	  break;
	case V4HImode:
	  type = v4hi_ftype_v4hi_v4hi;
	  break;
	case V2SImode:
	  type = v2si_ftype_v2si_v2si;
	  break;
	case DImode:
	  type = di_ftype_di_di;
	  break;

	default:
	  abort ();
	}

      /* Override for comparisons.  */
      if (d->icode == CODE_FOR_maskcmpv4sf3
	  || d->icode == CODE_FOR_maskncmpv4sf3
	  || d->icode == CODE_FOR_vmmaskcmpv4sf3
	  || d->icode == CODE_FOR_vmmaskncmpv4sf3)
	type = v4si_ftype_v4sf_v4sf;

      if (d->icode == CODE_FOR_maskcmpv2df3
	  || d->icode == CODE_FOR_maskncmpv2df3
	  || d->icode == CODE_FOR_vmmaskcmpv2df3
	  || d->icode == CODE_FOR_vmmaskncmpv2df3)
	type = v2di_ftype_v2df_v2df;

      def_builtin (d->mask, d->name, type, d->code);
    }

  /* Add the remaining MMX insns with somewhat more complicated types.  */
  def_builtin (MASK_MMX, "__builtin_ia32_mmx_zero", di_ftype_void, IX86_BUILTIN_MMX_ZERO);
  def_builtin (MASK_MMX, "__builtin_ia32_emms", void_ftype_void, IX86_BUILTIN_EMMS);
  def_builtin (MASK_MMX, "__builtin_ia32_psllw", v4hi_ftype_v4hi_di, IX86_BUILTIN_PSLLW);
  def_builtin (MASK_MMX, "__builtin_ia32_pslld", v2si_ftype_v2si_di, IX86_BUILTIN_PSLLD);
  def_builtin (MASK_MMX, "__builtin_ia32_psllq", di_ftype_di_di, IX86_BUILTIN_PSLLQ);

  def_builtin (MASK_MMX, "__builtin_ia32_psrlw", v4hi_ftype_v4hi_di, IX86_BUILTIN_PSRLW);
  def_builtin (MASK_MMX, "__builtin_ia32_psrld", v2si_ftype_v2si_di, IX86_BUILTIN_PSRLD);
  def_builtin (MASK_MMX, "__builtin_ia32_psrlq", di_ftype_di_di, IX86_BUILTIN_PSRLQ);

  def_builtin (MASK_MMX, "__builtin_ia32_psraw", v4hi_ftype_v4hi_di, IX86_BUILTIN_PSRAW);
  def_builtin (MASK_MMX, "__builtin_ia32_psrad", v2si_ftype_v2si_di, IX86_BUILTIN_PSRAD);

  def_builtin (MASK_MMX, "__builtin_ia32_pshufw", v4hi_ftype_v4hi_int, IX86_BUILTIN_PSHUFW);
  def_builtin (MASK_MMX, "__builtin_ia32_pmaddwd", v2si_ftype_v4hi_v4hi, IX86_BUILTIN_PMADDWD);

  /* comi/ucomi insns.  */
  for (i = 0, d = bdesc_comi; i < ARRAY_SIZE (bdesc_comi); i++, d++)
    if (d->mask == MASK_SSE2)
      def_builtin (d->mask, d->name, int_ftype_v2df_v2df, d->code);
    else
      def_builtin (d->mask, d->name, int_ftype_v4sf_v4sf, d->code);

  def_builtin (MASK_MMX, "__builtin_ia32_packsswb", v8qi_ftype_v4hi_v4hi, IX86_BUILTIN_PACKSSWB);
  def_builtin (MASK_MMX, "__builtin_ia32_packssdw", v4hi_ftype_v2si_v2si, IX86_BUILTIN_PACKSSDW);
  def_builtin (MASK_MMX, "__builtin_ia32_packuswb", v8qi_ftype_v4hi_v4hi, IX86_BUILTIN_PACKUSWB);

  def_builtin (MASK_SSE1, "__builtin_ia32_ldmxcsr", void_ftype_unsigned, IX86_BUILTIN_LDMXCSR);
  def_builtin (MASK_SSE1, "__builtin_ia32_stmxcsr", unsigned_ftype_void, IX86_BUILTIN_STMXCSR);
  def_builtin (MASK_SSE1, "__builtin_ia32_cvtpi2ps", v4sf_ftype_v4sf_v2si, IX86_BUILTIN_CVTPI2PS);
  def_builtin (MASK_SSE1, "__builtin_ia32_cvtps2pi", v2si_ftype_v4sf, IX86_BUILTIN_CVTPS2PI);
  def_builtin (MASK_SSE1, "__builtin_ia32_cvtsi2ss", v4sf_ftype_v4sf_int, IX86_BUILTIN_CVTSI2SS);
  def_builtin (MASK_SSE164, "__builtin_ia32_cvtsi642ss", v4sf_ftype_v4sf_int64, IX86_BUILTIN_CVTSI642SS);
  def_builtin (MASK_SSE1, "__builtin_ia32_cvtss2si", int_ftype_v4sf, IX86_BUILTIN_CVTSS2SI);
  def_builtin (MASK_SSE164, "__builtin_ia32_cvtss2si64", int64_ftype_v4sf, IX86_BUILTIN_CVTSS2SI64);
  def_builtin (MASK_SSE1, "__builtin_ia32_cvttps2pi", v2si_ftype_v4sf, IX86_BUILTIN_CVTTPS2PI);
  def_builtin (MASK_SSE1, "__builtin_ia32_cvttss2si", int_ftype_v4sf, IX86_BUILTIN_CVTTSS2SI);
  def_builtin (MASK_SSE164, "__builtin_ia32_cvttss2si64", int64_ftype_v4sf, IX86_BUILTIN_CVTTSS2SI64);

  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_pextrw", int_ftype_v4hi_int, IX86_BUILTIN_PEXTRW);
  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_pinsrw", v4hi_ftype_v4hi_int_int, IX86_BUILTIN_PINSRW);

  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_maskmovq", void_ftype_v8qi_v8qi_pchar, IX86_BUILTIN_MASKMOVQ);

  def_builtin (MASK_SSE1, "__builtin_ia32_loadaps", v4sf_ftype_pcfloat, IX86_BUILTIN_LOADAPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_loadups", v4sf_ftype_pcfloat, IX86_BUILTIN_LOADUPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_loadss", v4sf_ftype_pcfloat, IX86_BUILTIN_LOADSS);
  def_builtin (MASK_SSE1, "__builtin_ia32_storeaps", void_ftype_pfloat_v4sf, IX86_BUILTIN_STOREAPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_storeups", void_ftype_pfloat_v4sf, IX86_BUILTIN_STOREUPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_storess", void_ftype_pfloat_v4sf, IX86_BUILTIN_STORESS);

  def_builtin (MASK_SSE1, "__builtin_ia32_loadhps", v4sf_ftype_v4sf_pv2si, IX86_BUILTIN_LOADHPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_loadlps", v4sf_ftype_v4sf_pv2si, IX86_BUILTIN_LOADLPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_storehps", void_ftype_pv2si_v4sf, IX86_BUILTIN_STOREHPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_storelps", void_ftype_pv2si_v4sf, IX86_BUILTIN_STORELPS);

  def_builtin (MASK_SSE1, "__builtin_ia32_movmskps", int_ftype_v4sf, IX86_BUILTIN_MOVMSKPS);
  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_pmovmskb", int_ftype_v8qi, IX86_BUILTIN_PMOVMSKB);
  def_builtin (MASK_SSE1, "__builtin_ia32_movntps", void_ftype_pfloat_v4sf, IX86_BUILTIN_MOVNTPS);
  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_movntq", void_ftype_pdi_di, IX86_BUILTIN_MOVNTQ);

  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_sfence", void_ftype_void, IX86_BUILTIN_SFENCE);

  def_builtin (MASK_SSE1 | MASK_3DNOW_A, "__builtin_ia32_psadbw", di_ftype_v8qi_v8qi, IX86_BUILTIN_PSADBW);

  def_builtin (MASK_SSE1, "__builtin_ia32_rcpps", v4sf_ftype_v4sf, IX86_BUILTIN_RCPPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_rcpss", v4sf_ftype_v4sf, IX86_BUILTIN_RCPSS);
  def_builtin (MASK_SSE1, "__builtin_ia32_rsqrtps", v4sf_ftype_v4sf, IX86_BUILTIN_RSQRTPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_rsqrtss", v4sf_ftype_v4sf, IX86_BUILTIN_RSQRTSS);
  def_builtin (MASK_SSE1, "__builtin_ia32_sqrtps", v4sf_ftype_v4sf, IX86_BUILTIN_SQRTPS);
  def_builtin (MASK_SSE1, "__builtin_ia32_sqrtss", v4sf_ftype_v4sf, IX86_BUILTIN_SQRTSS);

  def_builtin (MASK_SSE1, "__builtin_ia32_shufps", v4sf_ftype_v4sf_v4sf_int, IX86_BUILTIN_SHUFPS);

  /* Original 3DNow!  */
  def_builtin (MASK_3DNOW, "__builtin_ia32_femms", void_ftype_void, IX86_BUILTIN_FEMMS);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pavgusb", v8qi_ftype_v8qi_v8qi, IX86_BUILTIN_PAVGUSB);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pf2id", v2si_ftype_v2sf, IX86_BUILTIN_PF2ID);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfacc", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFACC);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfadd", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFADD);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfcmpeq", v2si_ftype_v2sf_v2sf, IX86_BUILTIN_PFCMPEQ);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfcmpge", v2si_ftype_v2sf_v2sf, IX86_BUILTIN_PFCMPGE);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfcmpgt", v2si_ftype_v2sf_v2sf, IX86_BUILTIN_PFCMPGT);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfmax", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFMAX);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfmin", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFMIN);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfmul", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFMUL);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfrcp", v2sf_ftype_v2sf, IX86_BUILTIN_PFRCP);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfrcpit1", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFRCPIT1);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfrcpit2", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFRCPIT2);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfrsqrt", v2sf_ftype_v2sf, IX86_BUILTIN_PFRSQRT);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfrsqit1", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFRSQIT1);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfsub", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFSUB);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pfsubr", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFSUBR);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pi2fd", v2sf_ftype_v2si, IX86_BUILTIN_PI2FD);
  def_builtin (MASK_3DNOW, "__builtin_ia32_pmulhrw", v4hi_ftype_v4hi_v4hi, IX86_BUILTIN_PMULHRW);

  /* 3DNow! extension as used in the Athlon CPU.  */
  def_builtin (MASK_3DNOW_A, "__builtin_ia32_pf2iw", v2si_ftype_v2sf, IX86_BUILTIN_PF2IW);
  def_builtin (MASK_3DNOW_A, "__builtin_ia32_pfnacc", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFNACC);
  def_builtin (MASK_3DNOW_A, "__builtin_ia32_pfpnacc", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFPNACC);
  def_builtin (MASK_3DNOW_A, "__builtin_ia32_pi2fw", v2sf_ftype_v2si, IX86_BUILTIN_PI2FW);
  def_builtin (MASK_3DNOW_A, "__builtin_ia32_pswapdsf", v2sf_ftype_v2sf, IX86_BUILTIN_PSWAPDSF);
  def_builtin (MASK_3DNOW_A, "__builtin_ia32_pswapdsi", v2si_ftype_v2si, IX86_BUILTIN_PSWAPDSI);

  def_builtin (MASK_SSE1, "__builtin_ia32_setzerops", v4sf_ftype_void, IX86_BUILTIN_SSE_ZERO);

  /* SSE2 */
  def_builtin (MASK_SSE2, "__builtin_ia32_pextrw128", int_ftype_v8hi_int, IX86_BUILTIN_PEXTRW128);
  def_builtin (MASK_SSE2, "__builtin_ia32_pinsrw128", v8hi_ftype_v8hi_int_int, IX86_BUILTIN_PINSRW128);

  def_builtin (MASK_SSE2, "__builtin_ia32_maskmovdqu", void_ftype_v16qi_v16qi_pchar, IX86_BUILTIN_MASKMOVDQU);
  def_builtin (MASK_SSE2, "__builtin_ia32_movq2dq", v2di_ftype_di, IX86_BUILTIN_MOVQ2DQ);
  def_builtin (MASK_SSE2, "__builtin_ia32_movdq2q", di_ftype_v2di, IX86_BUILTIN_MOVDQ2Q);

  def_builtin (MASK_SSE2, "__builtin_ia32_loadapd", v2df_ftype_pcdouble, IX86_BUILTIN_LOADAPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_loadupd", v2df_ftype_pcdouble, IX86_BUILTIN_LOADUPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_loadsd", v2df_ftype_pcdouble, IX86_BUILTIN_LOADSD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storeapd", void_ftype_pdouble_v2df, IX86_BUILTIN_STOREAPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storeupd", void_ftype_pdouble_v2df, IX86_BUILTIN_STOREUPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storesd", void_ftype_pdouble_v2df, IX86_BUILTIN_STORESD);

  def_builtin (MASK_SSE2, "__builtin_ia32_loadhpd", v2df_ftype_v2df_pv2si, IX86_BUILTIN_LOADHPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_loadlpd", v2df_ftype_v2df_pv2si, IX86_BUILTIN_LOADLPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storehpd", void_ftype_pv2si_v2df, IX86_BUILTIN_STOREHPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storelpd", void_ftype_pv2si_v2df, IX86_BUILTIN_STORELPD);

  def_builtin (MASK_SSE2, "__builtin_ia32_movmskpd", int_ftype_v2df, IX86_BUILTIN_MOVMSKPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_pmovmskb128", int_ftype_v16qi, IX86_BUILTIN_PMOVMSKB128);
  def_builtin (MASK_SSE2, "__builtin_ia32_movnti", void_ftype_pint_int, IX86_BUILTIN_MOVNTI);
  def_builtin (MASK_SSE2, "__builtin_ia32_movntpd", void_ftype_pdouble_v2df, IX86_BUILTIN_MOVNTPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_movntdq", void_ftype_pv2di_v2di, IX86_BUILTIN_MOVNTDQ);

  def_builtin (MASK_SSE2, "__builtin_ia32_pshufd", v4si_ftype_v4si_int, IX86_BUILTIN_PSHUFD);
  def_builtin (MASK_SSE2, "__builtin_ia32_pshuflw", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSHUFLW);
  def_builtin (MASK_SSE2, "__builtin_ia32_pshufhw", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSHUFHW);
  def_builtin (MASK_SSE2, "__builtin_ia32_psadbw128", v2di_ftype_v16qi_v16qi, IX86_BUILTIN_PSADBW128);

  def_builtin (MASK_SSE2, "__builtin_ia32_sqrtpd", v2df_ftype_v2df, IX86_BUILTIN_SQRTPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_sqrtsd", v2df_ftype_v2df, IX86_BUILTIN_SQRTSD);

  def_builtin (MASK_SSE2, "__builtin_ia32_shufpd", v2df_ftype_v2df_v2df_int, IX86_BUILTIN_SHUFPD);

  def_builtin (MASK_SSE2, "__builtin_ia32_cvtdq2pd", v2df_ftype_v4si, IX86_BUILTIN_CVTDQ2PD);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvtdq2ps", v4sf_ftype_v4si, IX86_BUILTIN_CVTDQ2PS);

  def_builtin (MASK_SSE2, "__builtin_ia32_cvtpd2dq", v4si_ftype_v2df, IX86_BUILTIN_CVTPD2DQ);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvtpd2pi", v2si_ftype_v2df, IX86_BUILTIN_CVTPD2PI);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvtpd2ps", v4sf_ftype_v2df, IX86_BUILTIN_CVTPD2PS);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvttpd2dq", v4si_ftype_v2df, IX86_BUILTIN_CVTTPD2DQ);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvttpd2pi", v2si_ftype_v2df, IX86_BUILTIN_CVTTPD2PI);

  def_builtin (MASK_SSE2, "__builtin_ia32_cvtpi2pd", v2df_ftype_v2si, IX86_BUILTIN_CVTPI2PD);

  def_builtin (MASK_SSE2, "__builtin_ia32_cvtsd2si", int_ftype_v2df, IX86_BUILTIN_CVTSD2SI);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvttsd2si", int_ftype_v2df, IX86_BUILTIN_CVTTSD2SI);
  def_builtin (MASK_SSE264, "__builtin_ia32_cvtsd2si64", int64_ftype_v2df, IX86_BUILTIN_CVTSD2SI64);
  def_builtin (MASK_SSE264, "__builtin_ia32_cvttsd2si64", int64_ftype_v2df, IX86_BUILTIN_CVTTSD2SI64);

  def_builtin (MASK_SSE2, "__builtin_ia32_cvtps2dq", v4si_ftype_v4sf, IX86_BUILTIN_CVTPS2DQ);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvtps2pd", v2df_ftype_v4sf, IX86_BUILTIN_CVTPS2PD);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvttps2dq", v4si_ftype_v4sf, IX86_BUILTIN_CVTTPS2DQ);

  def_builtin (MASK_SSE2, "__builtin_ia32_cvtsi2sd", v2df_ftype_v2df_int, IX86_BUILTIN_CVTSI2SD);
  def_builtin (MASK_SSE264, "__builtin_ia32_cvtsi642sd", v2df_ftype_v2df_int64, IX86_BUILTIN_CVTSI642SD);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvtsd2ss", v4sf_ftype_v4sf_v2df, IX86_BUILTIN_CVTSD2SS);
  def_builtin (MASK_SSE2, "__builtin_ia32_cvtss2sd", v2df_ftype_v2df_v4sf, IX86_BUILTIN_CVTSS2SD);

  def_builtin (MASK_SSE2, "__builtin_ia32_setpd1", v2df_ftype_double, IX86_BUILTIN_SETPD1);
  def_builtin (MASK_SSE2, "__builtin_ia32_setpd", v2df_ftype_double_double, IX86_BUILTIN_SETPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_setzeropd", ti_ftype_void, IX86_BUILTIN_CLRPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_loadpd1", v2df_ftype_pcdouble, IX86_BUILTIN_LOADPD1);
  def_builtin (MASK_SSE2, "__builtin_ia32_loadrpd", v2df_ftype_pcdouble, IX86_BUILTIN_LOADRPD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storepd1", void_ftype_pdouble_v2df, IX86_BUILTIN_STOREPD1);
  def_builtin (MASK_SSE2, "__builtin_ia32_storerpd", void_ftype_pdouble_v2df, IX86_BUILTIN_STORERPD);

  def_builtin (MASK_SSE2, "__builtin_ia32_clflush", void_ftype_pcvoid, IX86_BUILTIN_CLFLUSH);
  def_builtin (MASK_SSE2, "__builtin_ia32_lfence", void_ftype_void, IX86_BUILTIN_LFENCE);
  def_builtin (MASK_SSE2, "__builtin_ia32_mfence", void_ftype_void, IX86_BUILTIN_MFENCE);

  def_builtin (MASK_SSE2, "__builtin_ia32_loaddqa", v16qi_ftype_pcchar, IX86_BUILTIN_LOADDQA);
  def_builtin (MASK_SSE2, "__builtin_ia32_loaddqu", v16qi_ftype_pcchar, IX86_BUILTIN_LOADDQU);
  def_builtin (MASK_SSE2, "__builtin_ia32_loadd", v4si_ftype_pcint, IX86_BUILTIN_LOADD);
  def_builtin (MASK_SSE2, "__builtin_ia32_storedqa", void_ftype_pchar_v16qi, IX86_BUILTIN_STOREDQA);
  def_builtin (MASK_SSE2, "__builtin_ia32_storedqu", void_ftype_pchar_v16qi, IX86_BUILTIN_STOREDQU);
  def_builtin (MASK_SSE2, "__builtin_ia32_stored", void_ftype_pcint_v4si, IX86_BUILTIN_STORED);
  def_builtin (MASK_SSE2, "__builtin_ia32_movq", v2di_ftype_v2di, IX86_BUILTIN_MOVQ);

  def_builtin (MASK_SSE1, "__builtin_ia32_setzero128", v2di_ftype_void, IX86_BUILTIN_CLRTI);

  def_builtin (MASK_SSE2, "__builtin_ia32_psllw128", v8hi_ftype_v8hi_v2di, IX86_BUILTIN_PSLLW128);
  def_builtin (MASK_SSE2, "__builtin_ia32_pslld128", v4si_ftype_v4si_v2di, IX86_BUILTIN_PSLLD128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psllq128", v2di_ftype_v2di_v2di, IX86_BUILTIN_PSLLQ128);

  def_builtin (MASK_SSE2, "__builtin_ia32_psrlw128", v8hi_ftype_v8hi_v2di, IX86_BUILTIN_PSRLW128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psrld128", v4si_ftype_v4si_v2di, IX86_BUILTIN_PSRLD128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psrlq128", v2di_ftype_v2di_v2di, IX86_BUILTIN_PSRLQ128);

  def_builtin (MASK_SSE2, "__builtin_ia32_psraw128", v8hi_ftype_v8hi_v2di, IX86_BUILTIN_PSRAW128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psrad128", v4si_ftype_v4si_v2di, IX86_BUILTIN_PSRAD128);

  def_builtin (MASK_SSE2, "__builtin_ia32_pslldqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSLLDQI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psllwi128", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSLLWI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_pslldi128", v4si_ftype_v4si_int, IX86_BUILTIN_PSLLDI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psllqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSLLQI128);

  def_builtin (MASK_SSE2, "__builtin_ia32_psrldqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSRLDQI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psrlwi128", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSRLWI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psrldi128", v4si_ftype_v4si_int, IX86_BUILTIN_PSRLDI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psrlqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSRLQI128);

  def_builtin (MASK_SSE2, "__builtin_ia32_psrawi128", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSRAWI128);
  def_builtin (MASK_SSE2, "__builtin_ia32_psradi128", v4si_ftype_v4si_int, IX86_BUILTIN_PSRADI128);

  def_builtin (MASK_SSE2, "__builtin_ia32_pmaddwd128", v4si_ftype_v8hi_v8hi, IX86_BUILTIN_PMADDWD128);
}

/* Errors in the source file can cause expand_expr to return const0_rtx
   where we expect a vector.  To avoid crashing, use one of the vector
   clear instructions.  */
static rtx
safe_vector_operand (x, mode)
     rtx x;
     enum machine_mode mode;
{
  if (x != const0_rtx)
    return x;
  x = gen_reg_rtx (mode);

  if (VALID_MMX_REG_MODE (mode) || VALID_MMX_REG_MODE_3DNOW (mode))
    emit_insn (gen_mmx_clrdi (mode == DImode ? x
			      : gen_rtx_SUBREG (DImode, x, 0)));
  else
    emit_insn (gen_sse_clrv4sf (mode == V4SFmode ? x
				: gen_rtx_SUBREG (V4SFmode, x, 0)));
  return x;
}

/* Subroutine of ix86_expand_builtin to take care of binop insns.  */

static rtx
ix86_expand_binop_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (GET_MODE (op1) == SImode && mode1 == TImode)
    {
      rtx x = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_loadd (x, op1));
      op1 = gen_lowpart (TImode, x);
    }

  /* In case the insn wants input operands in modes different from
     the result, abort.  */
  if (GET_MODE (op0) != mode0 || GET_MODE (op1) != mode1)
    abort ();

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  /* In the commutative cases, both op0 and op1 are nonimmediate_operand,
     yet one of the two must not be a memory.  This is normally enforced
     by expanders, but we didn't bother to create one here.  */
  if (GET_CODE (op0) == MEM && GET_CODE (op1) == MEM)
    op0 = copy_to_mode_reg (mode0, op0);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of stores.  */

static rtx
ix86_expand_store_builtin (icode, arglist)
     enum insn_code icode;
     tree arglist;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  enum machine_mode mode0 = insn_data[icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[icode].operand[1].mode;

  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (op0, op1);
  if (pat)
    emit_insn (pat);
  return 0;
}

/* Subroutine of ix86_expand_builtin to take care of unop insns.  */

static rtx
ix86_expand_unop_builtin (icode, arglist, target, do_load)
     enum insn_code icode;
     tree arglist;
     rtx target;
     int do_load;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);
  if (do_load)
    op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  else
    {
      if (VECTOR_MODE_P (mode0))
	op0 = safe_vector_operand (op0, mode0);

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
    }

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of three special unop insns:
   sqrtss, rsqrtss, rcpss.  */

static rtx
ix86_expand_unop1_builtin (icode, arglist, target)
     enum insn_code icode;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  rtx op1, op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  op1 = op0;
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode0))
    op1 = copy_to_mode_reg (mode0, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of comparison insns.  */

static rtx
ix86_expand_sse_compare (d, arglist, target)
     const struct builtin_description *d;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  rtx op2;
  enum machine_mode tmode = insn_data[d->icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[d->icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[d->icode].operand[2].mode;
  enum rtx_code comparison = d->comparison;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  /* Swap operands if we have a comparison that isn't available in
     hardware.  */
  if (d->flag)
    {
      rtx tmp = gen_reg_rtx (mode1);
      emit_move_insn (tmp, op1);
      op1 = op0;
      op0 = tmp;
    }

  if (! target
      || GET_MODE (target) != tmode
      || ! (*insn_data[d->icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[d->icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[d->icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  op2 = gen_rtx_fmt_ee (comparison, mode0, op0, op1);
  pat = GEN_FCN (d->icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of comi insns.  */

static rtx
ix86_expand_sse_comi (d, arglist, target)
     const struct builtin_description *d;
     tree arglist;
     rtx target;
{
  rtx pat;
  tree arg0 = TREE_VALUE (arglist);
  tree arg1 = TREE_VALUE (TREE_CHAIN (arglist));
  rtx op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
  rtx op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
  rtx op2;
  enum machine_mode mode0 = insn_data[d->icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[d->icode].operand[1].mode;
  enum rtx_code comparison = d->comparison;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  /* Swap operands if we have a comparison that isn't available in
     hardware.  */
  if (d->flag)
    {
      rtx tmp = op1;
      op1 = op0;
      op0 = tmp;
    }

  target = gen_reg_rtx (SImode);
  emit_move_insn (target, const0_rtx);
  target = gen_rtx_SUBREG (QImode, target, 0);

  if (! (*insn_data[d->icode].operand[0].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[d->icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  op2 = gen_rtx_fmt_ee (comparison, mode0, op0, op1);
  pat = GEN_FCN (d->icode) (op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  emit_insn (gen_rtx_SET (VOIDmode,
			  gen_rtx_STRICT_LOW_PART (VOIDmode, target),
			  gen_rtx_fmt_ee (comparison, QImode,
					  SET_DEST (pat),
					  const0_rtx)));

  return SUBREG_REG (target);
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
ix86_expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget ATTRIBUTE_UNUSED;
     enum machine_mode mode ATTRIBUTE_UNUSED;
     int ignore ATTRIBUTE_UNUSED;
{
  const struct builtin_description *d;
  size_t i;
  enum insn_code icode;
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  tree arg0, arg1, arg2;
  rtx op0, op1, op2, pat;
  enum machine_mode tmode, mode0, mode1, mode2;
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case IX86_BUILTIN_EMMS:
      emit_insn (gen_emms ());
      return 0;

    case IX86_BUILTIN_SFENCE:
      emit_insn (gen_sfence ());
      return 0;

    case IX86_BUILTIN_PEXTRW:
    case IX86_BUILTIN_PEXTRW128:
      icode = (fcode == IX86_BUILTIN_PEXTRW
	       ? CODE_FOR_mmx_pextrw
	       : CODE_FOR_sse2_pextrw);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	{
	  /* @@@ better error message */
	  error ("selector must be an immediate");
	  return gen_reg_rtx (tmode);
	}
      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_PINSRW:
    case IX86_BUILTIN_PINSRW128:
      icode = (fcode == IX86_BUILTIN_PINSRW
	       ? CODE_FOR_mmx_pinsrw
	       : CODE_FOR_sse2_pinsrw);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;
      mode2 = insn_data[icode].operand[3].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
	{
	  /* @@@ better error message */
	  error ("selector must be an immediate");
	  return const0_rtx;
	}
      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_MASKMOVQ:
    case IX86_BUILTIN_MASKMOVDQU:
      icode = (fcode == IX86_BUILTIN_MASKMOVQ
	       ? (TARGET_64BIT ? CODE_FOR_mmx_maskmovq_rex : CODE_FOR_mmx_maskmovq)
	       : (TARGET_64BIT ? CODE_FOR_sse2_maskmovdqu_rex64
		  : CODE_FOR_sse2_maskmovdqu));
      /* Note the arg order is different from the operand order.  */
      arg1 = TREE_VALUE (arglist);
      arg2 = TREE_VALUE (TREE_CHAIN (arglist));
      arg0 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      if (! (*insn_data[icode].operand[2].predicate) (op2, mode2))
	op2 = copy_to_mode_reg (mode2, op2);
      pat = GEN_FCN (icode) (op0, op1, op2);
      if (! pat)
	return 0;
      emit_insn (pat);
      return 0;

    case IX86_BUILTIN_SQRTSS:
      return ix86_expand_unop1_builtin (CODE_FOR_vmsqrtv4sf2, arglist, target);
    case IX86_BUILTIN_RSQRTSS:
      return ix86_expand_unop1_builtin (CODE_FOR_vmrsqrtv4sf2, arglist, target);
    case IX86_BUILTIN_RCPSS:
      return ix86_expand_unop1_builtin (CODE_FOR_vmrcpv4sf2, arglist, target);

    case IX86_BUILTIN_LOADAPS:
      return ix86_expand_unop_builtin (CODE_FOR_sse_movaps, arglist, target, 1);

    case IX86_BUILTIN_LOADUPS:
      return ix86_expand_unop_builtin (CODE_FOR_sse_movups, arglist, target, 1);

    case IX86_BUILTIN_STOREAPS:
      return ix86_expand_store_builtin (CODE_FOR_sse_movaps, arglist);

    case IX86_BUILTIN_STOREUPS:
      return ix86_expand_store_builtin (CODE_FOR_sse_movups, arglist);

    case IX86_BUILTIN_LOADSS:
      return ix86_expand_unop_builtin (CODE_FOR_sse_loadss, arglist, target, 1);

    case IX86_BUILTIN_STORESS:
      return ix86_expand_store_builtin (CODE_FOR_sse_storess, arglist);

    case IX86_BUILTIN_LOADHPS:
    case IX86_BUILTIN_LOADLPS:
    case IX86_BUILTIN_LOADHPD:
    case IX86_BUILTIN_LOADLPD:
      icode = (fcode == IX86_BUILTIN_LOADHPS ? CODE_FOR_sse_movhps
	       : fcode == IX86_BUILTIN_LOADLPS ? CODE_FOR_sse_movlps
	       : fcode == IX86_BUILTIN_LOADHPD ? CODE_FOR_sse2_movhpd
	       : CODE_FOR_sse2_movlpd);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      op1 = gen_rtx_MEM (mode1, copy_to_mode_reg (Pmode, op1));
      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_STOREHPS:
    case IX86_BUILTIN_STORELPS:
    case IX86_BUILTIN_STOREHPD:
    case IX86_BUILTIN_STORELPD:
      icode = (fcode == IX86_BUILTIN_STOREHPS ? CODE_FOR_sse_movhps
	       : fcode == IX86_BUILTIN_STORELPS ? CODE_FOR_sse_movlps
	       : fcode == IX86_BUILTIN_STOREHPD ? CODE_FOR_sse2_movhpd
	       : CODE_FOR_sse2_movlpd);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;

      op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      pat = GEN_FCN (icode) (op0, op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return 0;

    case IX86_BUILTIN_MOVNTPS:
      return ix86_expand_store_builtin (CODE_FOR_sse_movntv4sf, arglist);
    case IX86_BUILTIN_MOVNTQ:
      return ix86_expand_store_builtin (CODE_FOR_sse_movntdi, arglist);

    case IX86_BUILTIN_LDMXCSR:
      op0 = expand_expr (TREE_VALUE (arglist), NULL_RTX, VOIDmode, 0);
      target = assign_386_stack_local (SImode, 0);
      emit_move_insn (target, op0);
      emit_insn (gen_ldmxcsr (target));
      return 0;

    case IX86_BUILTIN_STMXCSR:
      target = assign_386_stack_local (SImode, 0);
      emit_insn (gen_stmxcsr (target));
      return copy_to_mode_reg (SImode, target);

    case IX86_BUILTIN_SHUFPS:
    case IX86_BUILTIN_SHUFPD:
      icode = (fcode == IX86_BUILTIN_SHUFPS
	       ? CODE_FOR_sse_shufps
	       : CODE_FOR_sse2_shufpd);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      op2 = expand_expr (arg2, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;
      mode2 = insn_data[icode].operand[3].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
	{
	  /* @@@ better error message */
	  error ("mask must be an immediate");
	  return gen_reg_rtx (tmode);
	}
      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_PSHUFW:
    case IX86_BUILTIN_PSHUFD:
    case IX86_BUILTIN_PSHUFHW:
    case IX86_BUILTIN_PSHUFLW:
      icode = (  fcode == IX86_BUILTIN_PSHUFHW ? CODE_FOR_sse2_pshufhw
	       : fcode == IX86_BUILTIN_PSHUFLW ? CODE_FOR_sse2_pshuflw
	       : fcode == IX86_BUILTIN_PSHUFD ? CODE_FOR_sse2_pshufd
	       : CODE_FOR_mmx_pshufw);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
	op0 = copy_to_mode_reg (mode1, op0);
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode2))
	{
	  /* @@@ better error message */
	  error ("mask must be an immediate");
	  return const0_rtx;
	}
      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_PSLLDQI128:
    case IX86_BUILTIN_PSRLDQI128:
      icode = (  fcode == IX86_BUILTIN_PSLLDQI128 ? CODE_FOR_sse2_ashlti3
	       : CODE_FOR_sse2_lshrti3);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
	{
	  op0 = copy_to_reg (op0);
	  op0 = simplify_gen_subreg (mode1, op0, GET_MODE (op0), 0);
	}
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode2))
	{
	  error ("shift must be an immediate");
	  return const0_rtx;
	}
      target = gen_reg_rtx (V2DImode);
      pat = GEN_FCN (icode) (simplify_gen_subreg (tmode, target, V2DImode, 0), op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_FEMMS:
      emit_insn (gen_femms ());
      return NULL_RTX;

    case IX86_BUILTIN_PAVGUSB:
      return ix86_expand_binop_builtin (CODE_FOR_pavgusb, arglist, target);

    case IX86_BUILTIN_PF2ID:
      return ix86_expand_unop_builtin (CODE_FOR_pf2id, arglist, target, 0);

    case IX86_BUILTIN_PFACC:
      return ix86_expand_binop_builtin (CODE_FOR_pfacc, arglist, target);

    case IX86_BUILTIN_PFADD:
     return ix86_expand_binop_builtin (CODE_FOR_addv2sf3, arglist, target);

    case IX86_BUILTIN_PFCMPEQ:
      return ix86_expand_binop_builtin (CODE_FOR_eqv2sf3, arglist, target);

    case IX86_BUILTIN_PFCMPGE:
      return ix86_expand_binop_builtin (CODE_FOR_gev2sf3, arglist, target);

    case IX86_BUILTIN_PFCMPGT:
      return ix86_expand_binop_builtin (CODE_FOR_gtv2sf3, arglist, target);

    case IX86_BUILTIN_PFMAX:
      return ix86_expand_binop_builtin (CODE_FOR_pfmaxv2sf3, arglist, target);

    case IX86_BUILTIN_PFMIN:
      return ix86_expand_binop_builtin (CODE_FOR_pfminv2sf3, arglist, target);

    case IX86_BUILTIN_PFMUL:
      return ix86_expand_binop_builtin (CODE_FOR_mulv2sf3, arglist, target);

    case IX86_BUILTIN_PFRCP:
      return ix86_expand_unop_builtin (CODE_FOR_pfrcpv2sf2, arglist, target, 0);

    case IX86_BUILTIN_PFRCPIT1:
      return ix86_expand_binop_builtin (CODE_FOR_pfrcpit1v2sf3, arglist, target);

    case IX86_BUILTIN_PFRCPIT2:
      return ix86_expand_binop_builtin (CODE_FOR_pfrcpit2v2sf3, arglist, target);

    case IX86_BUILTIN_PFRSQIT1:
      return ix86_expand_binop_builtin (CODE_FOR_pfrsqit1v2sf3, arglist, target);

    case IX86_BUILTIN_PFRSQRT:
      return ix86_expand_unop_builtin (CODE_FOR_pfrsqrtv2sf2, arglist, target, 0);

    case IX86_BUILTIN_PFSUB:
      return ix86_expand_binop_builtin (CODE_FOR_subv2sf3, arglist, target);

    case IX86_BUILTIN_PFSUBR:
      return ix86_expand_binop_builtin (CODE_FOR_subrv2sf3, arglist, target);

    case IX86_BUILTIN_PI2FD:
      return ix86_expand_unop_builtin (CODE_FOR_floatv2si2, arglist, target, 0);

    case IX86_BUILTIN_PMULHRW:
      return ix86_expand_binop_builtin (CODE_FOR_pmulhrwv4hi3, arglist, target);

    case IX86_BUILTIN_PF2IW:
      return ix86_expand_unop_builtin (CODE_FOR_pf2iw, arglist, target, 0);

    case IX86_BUILTIN_PFNACC:
      return ix86_expand_binop_builtin (CODE_FOR_pfnacc, arglist, target);

    case IX86_BUILTIN_PFPNACC:
      return ix86_expand_binop_builtin (CODE_FOR_pfpnacc, arglist, target);

    case IX86_BUILTIN_PI2FW:
      return ix86_expand_unop_builtin (CODE_FOR_pi2fw, arglist, target, 0);

    case IX86_BUILTIN_PSWAPDSI:
      return ix86_expand_unop_builtin (CODE_FOR_pswapdv2si2, arglist, target, 0);

    case IX86_BUILTIN_PSWAPDSF:
      return ix86_expand_unop_builtin (CODE_FOR_pswapdv2sf2, arglist, target, 0);

    case IX86_BUILTIN_SSE_ZERO:
      target = gen_reg_rtx (V4SFmode);
      emit_insn (gen_sse_clrv4sf (target));
      return target;

    case IX86_BUILTIN_MMX_ZERO:
      target = gen_reg_rtx (DImode);
      emit_insn (gen_mmx_clrdi (target));
      return target;

    case IX86_BUILTIN_CLRTI:
      target = gen_reg_rtx (V2DImode);
      emit_insn (gen_sse2_clrti (simplify_gen_subreg (TImode, target, V2DImode, 0)));
      return target;


    case IX86_BUILTIN_SQRTSD:
      return ix86_expand_unop1_builtin (CODE_FOR_vmsqrtv2df2, arglist, target);
    case IX86_BUILTIN_LOADAPD:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_movapd, arglist, target, 1);
    case IX86_BUILTIN_LOADUPD:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_movupd, arglist, target, 1);

    case IX86_BUILTIN_STOREAPD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movapd, arglist);
    case IX86_BUILTIN_STOREUPD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movupd, arglist);

    case IX86_BUILTIN_LOADSD:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_loadsd, arglist, target, 1);

    case IX86_BUILTIN_STORESD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_storesd, arglist);

    case IX86_BUILTIN_SETPD1:
      target = assign_386_stack_local (DFmode, 0);
      arg0 = TREE_VALUE (arglist);
      emit_move_insn (adjust_address (target, DFmode, 0),
		      expand_expr (arg0, NULL_RTX, VOIDmode, 0));
      op0 = gen_reg_rtx (V2DFmode);
      emit_insn (gen_sse2_loadsd (op0, adjust_address (target, V2DFmode, 0)));
      emit_insn (gen_sse2_shufpd (op0, op0, op0, GEN_INT (0)));
      return op0;

    case IX86_BUILTIN_SETPD:
      target = assign_386_stack_local (V2DFmode, 0);
      arg0 = TREE_VALUE (arglist);
      arg1 = TREE_VALUE (TREE_CHAIN (arglist));
      emit_move_insn (adjust_address (target, DFmode, 0),
		      expand_expr (arg0, NULL_RTX, VOIDmode, 0));
      emit_move_insn (adjust_address (target, DFmode, 8),
		      expand_expr (arg1, NULL_RTX, VOIDmode, 0));
      op0 = gen_reg_rtx (V2DFmode);
      emit_insn (gen_sse2_movapd (op0, target));
      return op0;

    case IX86_BUILTIN_LOADRPD:
      target = ix86_expand_unop_builtin (CODE_FOR_sse2_movapd, arglist,
					 gen_reg_rtx (V2DFmode), 1);
      emit_insn (gen_sse2_shufpd (target, target, target, GEN_INT (1)));
      return target;

    case IX86_BUILTIN_LOADPD1:
      target = ix86_expand_unop_builtin (CODE_FOR_sse2_loadsd, arglist,
					 gen_reg_rtx (V2DFmode), 1);
      emit_insn (gen_sse2_shufpd (target, target, target, const0_rtx));
      return target;

    case IX86_BUILTIN_STOREPD1:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movapd, arglist);
    case IX86_BUILTIN_STORERPD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movapd, arglist);

    case IX86_BUILTIN_CLRPD:
      target = gen_reg_rtx (V2DFmode);
      emit_insn (gen_sse_clrv2df (target));
      return target;

    case IX86_BUILTIN_MFENCE:
	emit_insn (gen_sse2_mfence ());
	return 0;
    case IX86_BUILTIN_LFENCE:
	emit_insn (gen_sse2_lfence ());
	return 0;

    case IX86_BUILTIN_CLFLUSH:
	arg0 = TREE_VALUE (arglist);
	op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
	icode = CODE_FOR_sse2_clflush;
	if (! (*insn_data[icode].operand[0].predicate) (op0, Pmode))
	    op0 = copy_to_mode_reg (Pmode, op0);

	emit_insn (gen_sse2_clflush (op0));
	return 0;

    case IX86_BUILTIN_MOVNTPD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movntv2df, arglist);
    case IX86_BUILTIN_MOVNTDQ:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movntv2di, arglist);
    case IX86_BUILTIN_MOVNTI:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movntsi, arglist);

    case IX86_BUILTIN_LOADDQA:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_movdqa, arglist, target, 1);
    case IX86_BUILTIN_LOADDQU:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_movdqu, arglist, target, 1);
    case IX86_BUILTIN_LOADD:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_loadd, arglist, target, 1);

    case IX86_BUILTIN_STOREDQA:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movdqa, arglist);
    case IX86_BUILTIN_STOREDQU:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movdqu, arglist);
    case IX86_BUILTIN_STORED:
      return ix86_expand_store_builtin (CODE_FOR_sse2_stored, arglist);

    default:
      break;
    }

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      {
	/* Compares are treated specially.  */
	if (d->icode == CODE_FOR_maskcmpv4sf3
	    || d->icode == CODE_FOR_vmmaskcmpv4sf3
	    || d->icode == CODE_FOR_maskncmpv4sf3
	    || d->icode == CODE_FOR_vmmaskncmpv4sf3
	    || d->icode == CODE_FOR_maskcmpv2df3
	    || d->icode == CODE_FOR_vmmaskcmpv2df3
	    || d->icode == CODE_FOR_maskncmpv2df3
	    || d->icode == CODE_FOR_vmmaskncmpv2df3)
	  return ix86_expand_sse_compare (d, arglist, target);

	return ix86_expand_binop_builtin (d->icode, arglist, target);
      }

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return ix86_expand_unop_builtin (d->icode, arglist, target, 0);

  for (i = 0, d = bdesc_comi; i < ARRAY_SIZE (bdesc_comi); i++, d++)
    if (d->code == fcode)
      return ix86_expand_sse_comi (d, arglist, target);

  /* @@@ Should really do something sensible here.  */
  return 0;
}

/* Store OPERAND to the memory after reload is completed.  This means
   that we can't easily use assign_stack_local.  */
rtx
ix86_force_to_memory (mode, operand)
     enum machine_mode mode;
     rtx operand;
{
  rtx result;
  if (!reload_completed)
    abort ();
  if (TARGET_64BIT && TARGET_RED_ZONE)
    {
      result = gen_rtx_MEM (mode,
			    gen_rtx_PLUS (Pmode,
					  stack_pointer_rtx,
					  GEN_INT (-RED_ZONE_SIZE)));
      emit_move_insn (result, operand);
    }
  else if (TARGET_64BIT && !TARGET_RED_ZONE)
    {
      switch (mode)
	{
	case HImode:
	case SImode:
	  operand = gen_lowpart (DImode, operand);
	  /* FALLTHRU */
	case DImode:
	  emit_insn (
		      gen_rtx_SET (VOIDmode,
				   gen_rtx_MEM (DImode,
						gen_rtx_PRE_DEC (DImode,
							stack_pointer_rtx)),
				   operand));
	  break;
	default:
	  abort ();
	}
      result = gen_rtx_MEM (mode, stack_pointer_rtx);
    }
  else
    {
      switch (mode)
	{
	case DImode:
	  {
	    rtx operands[2];
	    split_di (&operand, 1, operands, operands + 1);
	    emit_insn (
			gen_rtx_SET (VOIDmode,
				     gen_rtx_MEM (SImode,
						  gen_rtx_PRE_DEC (Pmode,
							stack_pointer_rtx)),
				     operands[1]));
	    emit_insn (
			gen_rtx_SET (VOIDmode,
				     gen_rtx_MEM (SImode,
						  gen_rtx_PRE_DEC (Pmode,
							stack_pointer_rtx)),
				     operands[0]));
	  }
	  break;
	case HImode:
	  /* It is better to store HImodes as SImodes.  */
	  if (!TARGET_PARTIAL_REG_STALL)
	    operand = gen_lowpart (SImode, operand);
	  /* FALLTHRU */
	case SImode:
	  emit_insn (
		      gen_rtx_SET (VOIDmode,
				   gen_rtx_MEM (GET_MODE (operand),
						gen_rtx_PRE_DEC (SImode,
							stack_pointer_rtx)),
				   operand));
	  break;
	default:
	  abort ();
	}
      result = gen_rtx_MEM (mode, stack_pointer_rtx);
    }
  return result;
}

/* Free operand from the memory.  */
void
ix86_free_from_memory (mode)
     enum machine_mode mode;
{
  if (!TARGET_64BIT || !TARGET_RED_ZONE)
    {
      int size;

      if (mode == DImode || TARGET_64BIT)
	size = 8;
      else if (mode == HImode && TARGET_PARTIAL_REG_STALL)
	size = 2;
      else
	size = 4;
      /* Use LEA to deallocate stack space.  In peephole2 it will be converted
         to pop or add instruction if registers are available.  */
      emit_insn (gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			      gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					    GEN_INT (size))));
    }
}

/* Put float CONST_DOUBLE in the constant pool instead of fp regs.
   QImode must go into class Q_REGS.
   Narrow ALL_REGS to GENERAL_REGS.  This supports allowing movsf and
   movdf to do mem-to-mem moves through integer regs.  */
enum reg_class
ix86_preferred_reload_class (x, class)
     rtx x;
     enum reg_class class;
{
  if (GET_CODE (x) == CONST_VECTOR && x != CONST0_RTX (GET_MODE (x)))
    return NO_REGS;
  if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != VOIDmode)
    {
      /* SSE can't load any constant directly yet.  */
      if (SSE_CLASS_P (class))
	return NO_REGS;
      /* Floats can load 0 and 1.  */
      if (MAYBE_FLOAT_CLASS_P (class) && standard_80387_constant_p (x))
	{
	  /* Limit class to non-SSE.  Use GENERAL_REGS if possible.  */
	  if (MAYBE_SSE_CLASS_P (class))
	    return (reg_class_subset_p (class, GENERAL_REGS)
		    ? GENERAL_REGS : FLOAT_REGS);
	  else
	    return class;
	}
      /* General regs can load everything.  */
      if (reg_class_subset_p (class, GENERAL_REGS))
	return GENERAL_REGS;
      /* In case we haven't resolved FLOAT or SSE yet, give up.  */
      if (MAYBE_FLOAT_CLASS_P (class) || MAYBE_SSE_CLASS_P (class))
	return NO_REGS;
    }
  if (MAYBE_MMX_CLASS_P (class) && CONSTANT_P (x))
    return NO_REGS;
  if (GET_MODE (x) == QImode && ! reg_class_subset_p (class, Q_REGS))
    return Q_REGS;
  return class;
}

/* If we are copying between general and FP registers, we need a memory
   location. The same is true for SSE and MMX registers.

   The macro can't work reliably when one of the CLASSES is class containing
   registers from multiple units (SSE, MMX, integer).  We avoid this by never
   combining those units in single alternative in the machine description.
   Ensure that this constraint holds to avoid unexpected surprises.

   When STRICT is false, we are being called from REGISTER_MOVE_COST, so do not
   enforce these sanity checks.  */
int
ix86_secondary_memory_needed (class1, class2, mode, strict)
     enum reg_class class1, class2;
     enum machine_mode mode;
     int strict;
{
  if (MAYBE_FLOAT_CLASS_P (class1) != FLOAT_CLASS_P (class1)
      || MAYBE_FLOAT_CLASS_P (class2) != FLOAT_CLASS_P (class2)
      || MAYBE_SSE_CLASS_P (class1) != SSE_CLASS_P (class1)
      || MAYBE_SSE_CLASS_P (class2) != SSE_CLASS_P (class2)
      || MAYBE_MMX_CLASS_P (class1) != MMX_CLASS_P (class1)
      || MAYBE_MMX_CLASS_P (class2) != MMX_CLASS_P (class2))
    {
      if (strict)
	abort ();
      else
	return 1;
    }
  return (FLOAT_CLASS_P (class1) != FLOAT_CLASS_P (class2)
	  || (SSE_CLASS_P (class1) != SSE_CLASS_P (class2)
	      && (mode) != SImode)
	  || (MMX_CLASS_P (class1) != MMX_CLASS_P (class2)
	      && (mode) != SImode));
}
/* Return the cost of moving data from a register in class CLASS1 to
   one in class CLASS2.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.  */
int
ix86_register_move_cost (mode, class1, class2)
     enum machine_mode mode;
     enum reg_class class1, class2;
{
  /* In case we require secondary memory, compute cost of the store followed
     by load.  In order to avoid bad register allocation choices, we need 
     for this to be *at least* as high as the symmetric MEMORY_MOVE_COST.  */

  if (ix86_secondary_memory_needed (class1, class2, mode, 0))
    {
      int cost = 1;

      cost += MAX (MEMORY_MOVE_COST (mode, class1, 0),
		   MEMORY_MOVE_COST (mode, class1, 1));
      cost += MAX (MEMORY_MOVE_COST (mode, class2, 0),
		   MEMORY_MOVE_COST (mode, class2, 1));
      
      /* In case of copying from general_purpose_register we may emit multiple
         stores followed by single load causing memory size mismatch stall.
         Count this as arbitarily high cost of 20.  */
      if (CLASS_MAX_NREGS (class1, mode) > CLASS_MAX_NREGS (class2, mode))
	cost += 20;

      /* In the case of FP/MMX moves, the registers actually overlap, and we
	 have to switch modes in order to treat them differently.  */
      if ((MMX_CLASS_P (class1) && MAYBE_FLOAT_CLASS_P (class2))
          || (MMX_CLASS_P (class2) && MAYBE_FLOAT_CLASS_P (class1)))
	cost += 20;

      return cost;
    }

  /* Moves between SSE/MMX and integer unit are expensive.  */
  if (MMX_CLASS_P (class1) != MMX_CLASS_P (class2)
      || SSE_CLASS_P (class1) != SSE_CLASS_P (class2))
    return ix86_cost->mmxsse_to_integer;
  if (MAYBE_FLOAT_CLASS_P (class1))
    return ix86_cost->fp_move;
  if (MAYBE_SSE_CLASS_P (class1))
    return ix86_cost->sse_move;
  if (MAYBE_MMX_CLASS_P (class1))
    return ix86_cost->mmx_move;
  return 2;
}

/* Return 1 if hard register REGNO can hold a value of machine-mode MODE.  */
int
ix86_hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  /* Flags and only flags can only hold CCmode values.  */
  if (CC_REGNO_P (regno))
    return GET_MODE_CLASS (mode) == MODE_CC;
  if (GET_MODE_CLASS (mode) == MODE_CC
      || GET_MODE_CLASS (mode) == MODE_RANDOM
      || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
    return 0;
  if (FP_REGNO_P (regno))
    return VALID_FP_MODE_P (mode);
  if (SSE_REGNO_P (regno))
    return (TARGET_SSE ? VALID_SSE_REG_MODE (mode) : 0);
  if (MMX_REGNO_P (regno))
    return (TARGET_MMX
	    ? VALID_MMX_REG_MODE (mode) || VALID_MMX_REG_MODE_3DNOW (mode) : 0);
  /* We handle both integer and floats in the general purpose registers.
     In future we should be able to handle vector modes as well.  */
  if (!VALID_INT_MODE_P (mode) && !VALID_FP_MODE_P (mode))
    return 0;
  /* Take care for QImode values - they can be in non-QI regs, but then
     they do cause partial register stalls.  */
  if (regno < 4 || mode != QImode || TARGET_64BIT)
    return 1;
  return reload_in_progress || reload_completed || !TARGET_PARTIAL_REG_STALL;
}

/* Return the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.

   Model also increased moving costs of QImode registers in non
   Q_REGS classes.
 */
int
ix86_memory_move_cost (mode, class, in)
     enum machine_mode mode;
     enum reg_class class;
     int in;
{
  if (FLOAT_CLASS_P (class))
    {
      int index;
      switch (mode)
	{
	  case SFmode:
	    index = 0;
	    break;
	  case DFmode:
	    index = 1;
	    break;
	  case XFmode:
	  case TFmode:
	    index = 2;
	    break;
	  default:
	    return 100;
	}
      return in ? ix86_cost->fp_load [index] : ix86_cost->fp_store [index];
    }
  if (SSE_CLASS_P (class))
    {
      int index;
      switch (GET_MODE_SIZE (mode))
	{
	  case 4:
	    index = 0;
	    break;
	  case 8:
	    index = 1;
	    break;
	  case 16:
	    index = 2;
	    break;
	  default:
	    return 100;
	}
      return in ? ix86_cost->sse_load [index] : ix86_cost->sse_store [index];
    }
  if (MMX_CLASS_P (class))
    {
      int index;
      switch (GET_MODE_SIZE (mode))
	{
	  case 4:
	    index = 0;
	    break;
	  case 8:
	    index = 1;
	    break;
	  default:
	    return 100;
	}
      return in ? ix86_cost->mmx_load [index] : ix86_cost->mmx_store [index];
    }
  switch (GET_MODE_SIZE (mode))
    {
      case 1:
	if (in)
	  return (Q_CLASS_P (class) ? ix86_cost->int_load[0]
		  : ix86_cost->movzbl_load);
	else
	  return (Q_CLASS_P (class) ? ix86_cost->int_store[0]
		  : ix86_cost->int_store[0] + 4);
	break;
      case 2:
	return in ? ix86_cost->int_load[1] : ix86_cost->int_store[1];
      default:
	/* Compute number of 32bit moves needed.  TFmode is moved as XFmode.  */
	if (mode == TFmode)
	  mode = XFmode;
	return ((in ? ix86_cost->int_load[2] : ix86_cost->int_store[2])
		* ((int) GET_MODE_SIZE (mode)
		   + UNITS_PER_WORD -1 ) / UNITS_PER_WORD);
    }
}

#if defined (DO_GLOBAL_CTORS_BODY) && defined (HAS_INIT_SECTION)
static void
ix86_svr3_asm_out_constructor (symbol, priority)
     rtx symbol;
     int priority ATTRIBUTE_UNUSED;
{
  init_section ();
  fputs ("\tpushl $", asm_out_file);
  assemble_name (asm_out_file, XSTR (symbol, 0));
  fputc ('\n', asm_out_file);
}
#endif

#if TARGET_MACHO

static int current_machopic_label_num;

/* Given a symbol name and its associated stub, write out the
   definition of the stub.  */

void
machopic_output_stub (file, symb, stub)
     FILE *file;
     const char *symb, *stub;
{
  unsigned int length;
  char *binder_name, *symbol_name, lazy_ptr_name[32];
  int label = ++current_machopic_label_num;

  /* Lose our funky encoding stuff so it doesn't contaminate the stub.  */
  symb = (*targetm.strip_name_encoding) (symb);

  length = strlen (stub);
  binder_name = alloca (length + 32);
  GEN_BINDER_NAME_FOR_STUB (binder_name, stub, length);

  length = strlen (symb);
  symbol_name = alloca (length + 32);
  GEN_SYMBOL_NAME_FOR_SYMBOL (symbol_name, symb, length);

  sprintf (lazy_ptr_name, "L%d$lz", label);

  if (MACHOPIC_PURE)
    machopic_picsymbol_stub_section ();
  else
    machopic_symbol_stub_section ();

  fprintf (file, "%s:\n", stub);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

  if (MACHOPIC_PURE)
    {
      fprintf (file, "\tcall LPC$%d\nLPC$%d:\tpopl %%eax\n", label, label);
      fprintf (file, "\tmovl %s-LPC$%d(%%eax),%%edx\n", lazy_ptr_name, label);
      fprintf (file, "\tjmp %%edx\n");
    }
  else
    fprintf (file, "\tjmp *%s\n", lazy_ptr_name);
  
  fprintf (file, "%s:\n", binder_name);
  
  if (MACHOPIC_PURE)
    {
      fprintf (file, "\tlea %s-LPC$%d(%%eax),%%eax\n", lazy_ptr_name, label);
      fprintf (file, "\tpushl %%eax\n");
    }
  else
    fprintf (file, "\t pushl $%s\n", lazy_ptr_name);

  fprintf (file, "\tjmp dyld_stub_binding_helper\n");

  machopic_lazy_symbol_ptr_section ();
  fprintf (file, "%s:\n", lazy_ptr_name);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);
  fprintf (file, "\t.long %s\n", binder_name);
}
#endif /* TARGET_MACHO */

/* Order the registers for register allocator.  */

void
x86_order_regs_for_local_alloc ()
{
   int pos = 0;
   int i;

   /* First allocate the local general purpose registers.  */
   for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
     if (GENERAL_REGNO_P (i) && call_used_regs[i])
	reg_alloc_order [pos++] = i;

   /* Global general purpose registers.  */
   for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
     if (GENERAL_REGNO_P (i) && !call_used_regs[i])
	reg_alloc_order [pos++] = i;

   /* x87 registers come first in case we are doing FP math
      using them.  */
   if (!TARGET_SSE_MATH)
     for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
       reg_alloc_order [pos++] = i;

   /* SSE registers.  */
   for (i = FIRST_SSE_REG; i <= LAST_SSE_REG; i++)
     reg_alloc_order [pos++] = i;
   for (i = FIRST_REX_SSE_REG; i <= LAST_REX_SSE_REG; i++)
     reg_alloc_order [pos++] = i;

   /* x87 registerts.  */
   if (TARGET_SSE_MATH)
     for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
       reg_alloc_order [pos++] = i;

   for (i = FIRST_MMX_REG; i <= LAST_MMX_REG; i++)
     reg_alloc_order [pos++] = i;

   /* Initialize the rest of array as we do not allocate some registers
      at all.  */
   while (pos < FIRST_PSEUDO_REGISTER)
     reg_alloc_order [pos++] = 0;
}

/* Returns an expression indicating where the this parameter is
   located on entry to the FUNCTION.  */

static rtx
x86_this_parameter (function)
     tree function;
{
  tree type = TREE_TYPE (function);

  if (TARGET_64BIT)
    {
      int n = aggregate_value_p (TREE_TYPE (type)) != 0;
      return gen_rtx_REG (DImode, x86_64_int_parameter_registers[n]);
    }

  if (ix86_fntype_regparm (type) > 0)
    {
      tree parm;

      parm = TYPE_ARG_TYPES (type);
      /* Figure out whether or not the function has a variable number of
	 arguments.  */
      for (; parm; parm = TREE_CHAIN (parm))
	if (TREE_VALUE (parm) == void_type_node)
	  break;
      /* If not, the this parameter is in %eax.  */
      if (parm)
	return gen_rtx_REG (SImode, 0);
    }

  if (aggregate_value_p (TREE_TYPE (type)))
    return gen_rtx_MEM (SImode, plus_constant (stack_pointer_rtx, 8));
  else
    return gen_rtx_MEM (SImode, plus_constant (stack_pointer_rtx, 4));
}

/* Determine whether x86_output_mi_thunk can succeed.  */

static bool
x86_can_output_mi_thunk (thunk, delta, vcall_offset, function)
     tree thunk ATTRIBUTE_UNUSED;
     HOST_WIDE_INT delta ATTRIBUTE_UNUSED;
     HOST_WIDE_INT vcall_offset;
     tree function;
{
  /* 64-bit can handle anything.  */
  if (TARGET_64BIT)
    return true;

  /* For 32-bit, everything's fine if we have one free register.  */
  if (ix86_fntype_regparm (TREE_TYPE (function)) < 3)
    return true;

  /* Need a free register for vcall_offset.  */
  if (vcall_offset)
    return false;

  /* Need a free register for GOT references.  */
  if (flag_pic && !(*targetm.binds_local_p) (function))
    return false;

  /* Otherwise ok.  */
  return true;
}

/* Output the assembler code for a thunk function.  THUNK_DECL is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is non-zero, the word at
   *(*this + vcall_offset) should be added to THIS.  */

static void
x86_output_mi_thunk (file, thunk, delta, vcall_offset, function)
     FILE *file ATTRIBUTE_UNUSED;
     tree thunk ATTRIBUTE_UNUSED;
     HOST_WIDE_INT delta;
     HOST_WIDE_INT vcall_offset;
     tree function;
{
  rtx xops[3];
  rtx this = x86_this_parameter (function);
  rtx this_reg, tmp;

  /* If VCALL_OFFSET, we'll need THIS in a register.  Might as well
     pull it in now and let DELTA benefit.  */
  if (REG_P (this))
    this_reg = this;
  else if (vcall_offset)
    {
      /* Put the this parameter into %eax.  */
      xops[0] = this;
      xops[1] = this_reg = gen_rtx_REG (Pmode, 0);
      output_asm_insn ("mov{l}\t{%0, %1|%1, %0}", xops);
    }
  else
    this_reg = NULL_RTX;

  /* Adjust the this parameter by a fixed constant.  */
  if (delta)
    {
      xops[0] = GEN_INT (delta);
      xops[1] = this_reg ? this_reg : this;
      if (TARGET_64BIT)
	{
	  if (!x86_64_general_operand (xops[0], DImode))
	    {
	      tmp = gen_rtx_REG (DImode, FIRST_REX_INT_REG + 2 /* R10 */);
	      xops[1] = tmp;
	      output_asm_insn ("mov{q}\t{%1, %0|%0, %1}", xops);
	      xops[0] = tmp;
	      xops[1] = this;
	    }
	  output_asm_insn ("add{q}\t{%0, %1|%1, %0}", xops);
	}
      else
	output_asm_insn ("add{l}\t{%0, %1|%1, %0}", xops);
    }

  /* Adjust the this parameter by a value stored in the vtable.  */
  if (vcall_offset)
    {
      if (TARGET_64BIT)
	tmp = gen_rtx_REG (DImode, FIRST_REX_INT_REG + 2 /* R10 */);
      else
	tmp = gen_rtx_REG (SImode, 2 /* ECX */);

      xops[0] = gen_rtx_MEM (Pmode, this_reg);
      xops[1] = tmp;
      if (TARGET_64BIT)
	output_asm_insn ("mov{q}\t{%0, %1|%1, %0}", xops);
      else
	output_asm_insn ("mov{l}\t{%0, %1|%1, %0}", xops);

      /* Adjust the this parameter.  */
      xops[0] = gen_rtx_MEM (Pmode, plus_constant (tmp, vcall_offset));
      if (TARGET_64BIT && !memory_operand (xops[0], Pmode))
	{
	  rtx tmp2 = gen_rtx_REG (DImode, FIRST_REX_INT_REG + 3 /* R11 */);
	  xops[0] = GEN_INT (vcall_offset);
	  xops[1] = tmp2;
	  output_asm_insn ("mov{q}\t{%0, %1|%1, %0}", xops);
	  xops[0] = gen_rtx_MEM (Pmode, gen_rtx_PLUS (Pmode, tmp, tmp2));
	}
      xops[1] = this_reg;
      if (TARGET_64BIT)
	output_asm_insn ("add{q}\t{%0, %1|%1, %0}", xops);
      else
	output_asm_insn ("add{l}\t{%0, %1|%1, %0}", xops);
    }

  /* If necessary, drop THIS back to its stack slot.  */
  if (this_reg && this_reg != this)
    {
      xops[0] = this_reg;
      xops[1] = this;
      output_asm_insn ("mov{l}\t{%0, %1|%1, %0}", xops);
    }

  xops[0] = DECL_RTL (function);
  if (TARGET_64BIT)
    {
      if (!flag_pic || (*targetm.binds_local_p) (function))
	output_asm_insn ("jmp\t%P0", xops);
      else
	{
	  tmp = XEXP (xops[0], 0);
	  tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, tmp), UNSPEC_GOTPCREL);
	  tmp = gen_rtx_CONST (Pmode, tmp);
	  tmp = gen_rtx_MEM (QImode, tmp);
	  xops[0] = tmp;
	  output_asm_insn ("jmp\t%A0", xops);
	}
    }
  else
    {
      if (!flag_pic || (*targetm.binds_local_p) (function))
	output_asm_insn ("jmp\t%P0", xops);
      else
	{
	  tmp = gen_rtx_REG (SImode, 2 /* ECX */);
	  output_set_got (tmp);

	  xops[1] = tmp;
	  output_asm_insn ("mov{l}\t{%0@GOT(%1), %1|%1, %0@GOT[%1]}", xops);
	  output_asm_insn ("jmp\t{*}%1", xops);
	}
    }
}

int
x86_field_alignment (field, computed)
     tree field;
     int computed;
{
  enum machine_mode mode;
  tree type = TREE_TYPE (field);

  if (TARGET_64BIT || TARGET_ALIGN_DOUBLE)
    return computed;
  mode = TYPE_MODE (TREE_CODE (type) == ARRAY_TYPE
		    ? get_inner_array_type (type) : type);
  if (mode == DFmode || mode == DCmode
      || GET_MODE_CLASS (mode) == MODE_INT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    return MIN (32, computed);
  return computed;
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */
void
x86_function_profiler (file, labelno)
     FILE *file;
     int labelno;
{
  if (TARGET_64BIT)
    if (flag_pic)
      {
#ifndef NO_PROFILE_COUNTERS
	fprintf (file, "\tleaq\t%sP%d@(%%rip),%%r11\n", LPREFIX, labelno);
#endif
	fprintf (file, "\tcall\t*%s@GOTPCREL(%%rip)\n", MCOUNT_NAME);
      }
    else
      {
#ifndef NO_PROFILE_COUNTERS
	fprintf (file, "\tmovq\t$%sP%d,%%r11\n", LPREFIX, labelno);
#endif
	fprintf (file, "\tcall\t%s\n", MCOUNT_NAME);
      }
  else if (flag_pic)
    {
#ifndef NO_PROFILE_COUNTERS
      fprintf (file, "\tleal\t%sP%d@GOTOFF(%%ebx),%%%s\n",
	       LPREFIX, labelno, PROFILE_COUNT_REGISTER);
#endif
      fprintf (file, "\tcall\t*%s@GOT(%%ebx)\n", MCOUNT_NAME);
    }
  else
    {
#ifndef NO_PROFILE_COUNTERS
      fprintf (file, "\tmovl\t$%sP%d,%%%s\n", LPREFIX, labelno,
	       PROFILE_COUNT_REGISTER);
#endif
      fprintf (file, "\tcall\t%s\n", MCOUNT_NAME);
    }
}

/* Implement machine specific optimizations.  
   At the moment we implement single transformation: AMD Athlon works faster
   when RET is not destination of conditional jump or directly preceeded
   by other jump instruction.  We avoid the penalty by inserting NOP just
   before the RET instructions in such cases.  */
void
x86_machine_dependent_reorg (first)
     rtx first ATTRIBUTE_UNUSED;
{
  edge e;

  if (!TARGET_ATHLON || !optimize || optimize_size)
    return;
  for (e = EXIT_BLOCK_PTR->pred; e; e = e->pred_next)
  {
    basic_block bb = e->src;
    rtx ret = bb->end;
    rtx prev;
    bool insert = false;

    if (!returnjump_p (ret) || !maybe_hot_bb_p (bb))
      continue;
    prev = prev_nonnote_insn (ret);
    if (prev && GET_CODE (prev) == CODE_LABEL)
      {
	edge e;
	for (e = bb->pred; e; e = e->pred_next)
	  if (EDGE_FREQUENCY (e) && e->src->index > 0
	      && !(e->flags & EDGE_FALLTHRU))
	    insert = 1;
      }
    if (!insert)
      {
	prev = prev_real_insn (ret);
	if (prev && GET_CODE (prev) == JUMP_INSN
	    && any_condjump_p (prev))
	  insert = 1;
      }
    if (insert)
      emit_insn_before (gen_nop (), ret);
  }
}

#include "gt-i386.h"
