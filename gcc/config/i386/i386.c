/* Subroutines used for code generation on IA-32.
   Copyright (C) 1988, 1992, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-codes.h"
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
#include "cgraph.h"
#include "tree-gimple.h"
#include "dwarf2.h"
#include "df.h"
#include "tm-constrs.h"
#include "params.h"

static int x86_builtin_vectorization_cost (bool);

#ifndef CHECK_STACK_LIMIT
#define CHECK_STACK_LIMIT (-1)
#endif

/* Return index of given mode in mult and division cost tables.  */
#define MODE_INDEX(mode)					\
  ((mode) == QImode ? 0						\
   : (mode) == HImode ? 1					\
   : (mode) == SImode ? 2					\
   : (mode) == DImode ? 3					\
   : 4)

/* Processor costs (relative to an add) */
/* We assume COSTS_N_INSNS is defined as (N)*4 and an addition is 2 bytes.  */
#define COSTS_N_BYTES(N) ((N) * 2)

#define DUMMY_STRINGOP_ALGS {libcall, {{-1, libcall}}}

static const
struct processor_costs size_cost = {	/* costs for tuning for size */
  COSTS_N_BYTES (2),			/* cost of an add instruction */
  COSTS_N_BYTES (3),			/* cost of a lea instruction */
  COSTS_N_BYTES (2),			/* variable shift costs */
  COSTS_N_BYTES (3),			/* constant shift costs */
  {COSTS_N_BYTES (3),			/* cost of starting multiply for QI */
   COSTS_N_BYTES (3),			/*                               HI */
   COSTS_N_BYTES (3),			/*                               SI */
   COSTS_N_BYTES (3),			/*                               DI */
   COSTS_N_BYTES (5)},			/*                            other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_BYTES (3),			/* cost of a divide/mod for QI */
   COSTS_N_BYTES (3),			/*                          HI */
   COSTS_N_BYTES (3),			/*                          SI */
   COSTS_N_BYTES (3),			/*                          DI */
   COSTS_N_BYTES (5)},			/*                       other */
  COSTS_N_BYTES (3),			/* cost of movsx */
  COSTS_N_BYTES (3),			/* cost of movzx */
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
  {2, 2, 2},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  0,					/* size of l1 cache  */
  0,					/* size of l2 cache  */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_BYTES (2),			/* cost of FADD and FSUB insns.  */
  COSTS_N_BYTES (2),			/* cost of FMUL instruction.  */
  COSTS_N_BYTES (2),			/* cost of FDIV instruction.  */
  COSTS_N_BYTES (2),			/* cost of FABS instruction.  */
  COSTS_N_BYTES (2),			/* cost of FCHS instruction.  */
  COSTS_N_BYTES (2),			/* cost of FSQRT instruction.  */
  {{rep_prefix_1_byte, {{-1, rep_prefix_1_byte}}},
   {rep_prefix_1_byte, {{-1, rep_prefix_1_byte}}}},
  {{rep_prefix_1_byte, {{-1, rep_prefix_1_byte}}},
   {rep_prefix_1_byte, {{-1, rep_prefix_1_byte}}}},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  1,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  1,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

/* Processor costs (relative to an add) */
static const
struct processor_costs i386_cost = {	/* 386 specific costs */
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (3),			/* variable shift costs */
  COSTS_N_INSNS (2),			/* constant shift costs */
  {COSTS_N_INSNS (6),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (6),			/*                               HI */
   COSTS_N_INSNS (6),			/*                               SI */
   COSTS_N_INSNS (6),			/*                               DI */
   COSTS_N_INSNS (6)},			/*                               other */
  COSTS_N_INSNS (1),			/* cost of multiply per each bit set */
  {COSTS_N_INSNS (23),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (23),			/*                          HI */
   COSTS_N_INSNS (23),			/*                          SI */
   COSTS_N_INSNS (23),			/*                          DI */
   COSTS_N_INSNS (23)},			/*                          other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
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
  {8, 8, 8},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  0,					/* size of l1 cache  */
  0,					/* size of l2 cache  */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  1,					/* Branch cost */
  COSTS_N_INSNS (23),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (27),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (88),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (22),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (24),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (122),			/* cost of FSQRT instruction.  */
  {{rep_prefix_1_byte, {{-1, rep_prefix_1_byte}}},
   DUMMY_STRINGOP_ALGS},
  {{rep_prefix_1_byte, {{-1, rep_prefix_1_byte}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs i486_cost = {	/* 486 specific costs */
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (3),			/* variable shift costs */
  COSTS_N_INSNS (2),			/* constant shift costs */
  {COSTS_N_INSNS (12),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (12),			/*                               HI */
   COSTS_N_INSNS (12),			/*                               SI */
   COSTS_N_INSNS (12),			/*                               DI */
   COSTS_N_INSNS (12)},			/*                               other */
  1,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (40),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (40),			/*                          HI */
   COSTS_N_INSNS (40),			/*                          SI */
   COSTS_N_INSNS (40),			/*                          DI */
   COSTS_N_INSNS (40)},			/*                          other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
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
  {8, 8, 8},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  4,					/* size of l1 cache.  486 has 8kB cache
					   shared for code and data, so 4kB is
					   not really precise.  */
  4,					/* size of l2 cache  */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  1,					/* Branch cost */
  COSTS_N_INSNS (8),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (16),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (73),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (3),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (3),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (83),			/* cost of FSQRT instruction.  */
  {{rep_prefix_4_byte, {{-1, rep_prefix_4_byte}}},
   DUMMY_STRINGOP_ALGS},
  {{rep_prefix_4_byte, {{-1, rep_prefix_4_byte}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs pentium_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (4),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (11),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (11),			/*                               HI */
   COSTS_N_INSNS (11),			/*                               SI */
   COSTS_N_INSNS (11),			/*                               DI */
   COSTS_N_INSNS (11)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (25),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (25),			/*                          HI */
   COSTS_N_INSNS (25),			/*                          SI */
   COSTS_N_INSNS (25),			/*                          DI */
   COSTS_N_INSNS (25)},			/*                          other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
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
  {4, 4, 6},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  8,					/* size of l1 cache.  */
  8,					/* size of l2 cache  */
  0,					/* size of prefetch block */
  0,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (3),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (39),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (70),			/* cost of FSQRT instruction.  */
  {{libcall, {{256, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  {{libcall, {{-1, rep_prefix_4_byte}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs pentiumpro_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (4),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*                               HI */
   COSTS_N_INSNS (4),			/*                               SI */
   COSTS_N_INSNS (4),			/*                               DI */
   COSTS_N_INSNS (4)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (17),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (17),			/*                          HI */
   COSTS_N_INSNS (17),			/*                          SI */
   COSTS_N_INSNS (17),			/*                          DI */
   COSTS_N_INSNS (17)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
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
  {4, 4, 6},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  8,					/* size of l1 cache.  */
  256,					/* size of l2 cache  */
  32,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (56),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (56),			/* cost of FSQRT instruction.  */
  /* PentiumPro has optimized rep instructions for blocks aligned by 8 bytes (we ensure
     the alignment).  For small blocks inline loop is still a noticeable win, for bigger
     blocks either rep movsl or rep movsb is way to go.  Rep movsb has apparently
     more expensive startup time in CPU, but after 4K the difference is down in the noise.
   */
  {{rep_prefix_4_byte, {{128, loop}, {1024, unrolled_loop},
			{8192, rep_prefix_4_byte}, {-1, rep_prefix_1_byte}}},
   DUMMY_STRINGOP_ALGS},
  {{rep_prefix_4_byte, {{1024, unrolled_loop},
  		        {8192, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs geode_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (2),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*                               HI */
   COSTS_N_INSNS (7),			/*                               SI */
   COSTS_N_INSNS (7),			/*                               DI */
   COSTS_N_INSNS (7)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (15),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (23),			/*                          HI */
   COSTS_N_INSNS (39),			/*                          SI */
   COSTS_N_INSNS (39),			/*                          DI */
   COSTS_N_INSNS (39)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  4,					/* MOVE_RATIO */
  1,					/* cost for loading QImode using movzbl */
  {1, 1, 1},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {1, 1, 1},				/* cost of storing integer registers */
  1,					/* cost of reg,reg fld/fst */
  {1, 1, 1},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 6, 6},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */

  1,					/* cost of moving MMX register */
  {1, 1},				/* cost of loading MMX registers
					   in SImode and DImode */
  {1, 1},				/* cost of storing MMX registers
					   in SImode and DImode */
  1,					/* cost of moving SSE register */
  {1, 1, 1},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {1, 1, 1},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  1,					/* MMX or SSE register to integer */
  64,					/* size of l1 cache.  */
  128,					/* size of l2 cache.  */
  32,					/* size of prefetch block */
  1,					/* number of parallel prefetches */
  1,					/* Branch cost */
  COSTS_N_INSNS (6),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (11),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (47),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (54),			/* cost of FSQRT instruction.  */
  {{libcall, {{256, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  {{libcall, {{256, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs k6_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (3),			/*                               HI */
   COSTS_N_INSNS (3),			/*                               SI */
   COSTS_N_INSNS (3),			/*                               DI */
   COSTS_N_INSNS (3)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (18),			/*                          HI */
   COSTS_N_INSNS (18),			/*                          SI */
   COSTS_N_INSNS (18),			/*                          DI */
   COSTS_N_INSNS (18)},			/*                          other */
  COSTS_N_INSNS (2),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
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
  {4, 4, 4},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  32,					/* size of l1 cache.  */
  32,					/* size of l2 cache.  Some models
					   have integrated l2 cache, but
					   optimizing for k6 is not important
					   enough to worry about that.  */
  32,					/* size of prefetch block */
  1,					/* number of parallel prefetches */
  1,					/* Branch cost */
  COSTS_N_INSNS (2),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (2),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (56),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (56),			/* cost of FSQRT instruction.  */
  {{libcall, {{256, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  {{libcall, {{256, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs athlon_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (5),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (5),			/*                               HI */
   COSTS_N_INSNS (5),			/*                               SI */
   COSTS_N_INSNS (5),			/*                               DI */
   COSTS_N_INSNS (5)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*                          HI */
   COSTS_N_INSNS (42),			/*                          SI */
   COSTS_N_INSNS (74),			/*                          DI */
   COSTS_N_INSNS (74)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
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
  {6, 6, 8},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  64,					/* size of l1 cache.  */
  256,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  5,					/* Branch cost */
  COSTS_N_INSNS (4),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (4),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (24),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (35),			/* cost of FSQRT instruction.  */
  /* For some reason, Athlon deals better with REP prefix (relative to loops)
     compared to K8. Alignment becomes important after 8 bytes for memcpy and
     128 bytes for memset.  */
  {{libcall, {{2048, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  {{libcall, {{2048, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs k8_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*                               HI */
   COSTS_N_INSNS (3),			/*                               SI */
   COSTS_N_INSNS (4),			/*                               DI */
   COSTS_N_INSNS (5)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*                          HI */
   COSTS_N_INSNS (42),			/*                          SI */
   COSTS_N_INSNS (74),			/*                          DI */
   COSTS_N_INSNS (74)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
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
  {6, 6, 8},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {3, 3},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 3, 6},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 5},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  5,					/* MMX or SSE register to integer */
  64,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  /* New AMD processors never drop prefetches; if they cannot be performed
     immediately, they are queued.  We set number of simultaneous prefetches
     to a large constant to reflect this (it probably is not a good idea not
     to limit number of prefetches at all, as their execution also takes some
     time).  */
  100,					/* number of parallel prefetches */
  3,					/* Branch cost */
  COSTS_N_INSNS (4),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (4),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (19),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (35),			/* cost of FSQRT instruction.  */
  /* K8 has optimized REP instruction for medium sized blocks, but for very small
     blocks it is better to use loop. For large blocks, libcall can do
     nontemporary accesses and beat inline considerably.  */
  {{libcall, {{6, loop}, {14, unrolled_loop}, {-1, rep_prefix_4_byte}}},
   {libcall, {{16, loop}, {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  {{libcall, {{8, loop}, {24, unrolled_loop},
	      {2048, rep_prefix_4_byte}, {-1, libcall}}},
   {libcall, {{48, unrolled_loop}, {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  4,                                    /* scalar_stmt_cost.  */
  2,                                    /* scalar load_cost.  */
  2,                                    /* scalar_store_cost.  */
  5,                                    /* vec_stmt_cost.  */
  0,                                    /* vec_to_scalar_cost.  */
  2,                                    /* scalar_to_vec_cost.  */
  2,                                    /* vec_align_load_cost.  */
  3,                                    /* vec_unalign_load_cost.  */
  3,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  2,                                    /* cond_not_taken_branch_cost.  */
};

struct processor_costs amdfam10_cost = {
  COSTS_N_INSNS (1),                    /* cost of an add instruction */
  COSTS_N_INSNS (2),                    /* cost of a lea instruction */
  COSTS_N_INSNS (1),                    /* variable shift costs */
  COSTS_N_INSNS (1),                    /* constant shift costs */
  {COSTS_N_INSNS (3),                   /* cost of starting multiply for QI */
   COSTS_N_INSNS (4),                   /*                               HI */
   COSTS_N_INSNS (3),                   /*                               SI */
   COSTS_N_INSNS (4),                   /*                               DI */
   COSTS_N_INSNS (5)},                  /*                               other */
  0,                                    /* cost of multiply per each bit set */
  {COSTS_N_INSNS (19),                  /* cost of a divide/mod for QI */
   COSTS_N_INSNS (35),                  /*                          HI */
   COSTS_N_INSNS (51),                  /*                          SI */
   COSTS_N_INSNS (83),                  /*                          DI */
   COSTS_N_INSNS (83)},                 /*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
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
  {6, 6, 8},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {3, 3},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 4, 3},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 5},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  					/* On K8
  					    MOVD reg64, xmmreg 	Double	FSTORE 4
					    MOVD reg32, xmmreg 	Double	FSTORE 4
					   On AMDFAM10
					    MOVD reg64, xmmreg 	Double	FADD 3
                                                                1/1  1/1
					    MOVD reg32, xmmreg 	Double	FADD 3
                                                                1/1  1/1 */
  64,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  /* New AMD processors never drop prefetches; if they cannot be performed
     immediately, they are queued.  We set number of simultaneous prefetches
     to a large constant to reflect this (it probably is not a good idea not
     to limit number of prefetches at all, as their execution also takes some
     time).  */
  100,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (4),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (4),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (19),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (35),			/* cost of FSQRT instruction.  */

  /* AMDFAM10 has optimized REP instruction for medium sized blocks, but for
     very small blocks it is better to use loop. For large blocks, libcall can
     do nontemporary accesses and beat inline considerably.  */
  {{libcall, {{6, loop}, {14, unrolled_loop}, {-1, rep_prefix_4_byte}}},
   {libcall, {{16, loop}, {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  {{libcall, {{8, loop}, {24, unrolled_loop},
	      {2048, rep_prefix_4_byte}, {-1, libcall}}},
   {libcall, {{48, unrolled_loop}, {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  4,                                    /* scalar_stmt_cost.  */
  2,                                    /* scalar load_cost.  */
  2,                                    /* scalar_store_cost.  */
  6,                                    /* vec_stmt_cost.  */
  0,                                    /* vec_to_scalar_cost.  */
  2,                                    /* scalar_to_vec_cost.  */
  2,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  2,                                    /* vec_store_cost.  */
  2,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs pentium4_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (3),			/* cost of a lea instruction */
  COSTS_N_INSNS (4),			/* variable shift costs */
  COSTS_N_INSNS (4),			/* constant shift costs */
  {COSTS_N_INSNS (15),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (15),			/*                               HI */
   COSTS_N_INSNS (15),			/*                               SI */
   COSTS_N_INSNS (15),			/*                               DI */
   COSTS_N_INSNS (15)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (56),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (56),			/*                          HI */
   COSTS_N_INSNS (56),			/*                          SI */
   COSTS_N_INSNS (56),			/*                          DI */
   COSTS_N_INSNS (56)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
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
  {4, 4, 6},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
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
  8,					/* size of l1 cache.  */
  256,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (5),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (7),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (43),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (43),			/* cost of FSQRT instruction.  */
  {{libcall, {{12, loop_1_byte}, {-1, rep_prefix_4_byte}}},
   DUMMY_STRINGOP_ALGS},
  {{libcall, {{6, loop_1_byte}, {48, loop}, {20480, rep_prefix_4_byte},
   {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs nocona_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (10),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (10),			/*                               HI */
   COSTS_N_INSNS (10),			/*                               SI */
   COSTS_N_INSNS (10),			/*                               DI */
   COSTS_N_INSNS (10)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (66),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (66),			/*                          HI */
   COSTS_N_INSNS (66),			/*                          SI */
   COSTS_N_INSNS (66),			/*                          DI */
   COSTS_N_INSNS (66)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  16,					/* "large" insn */
  17,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  3,					/* cost of reg,reg fld/fst */
  {12, 12, 12},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 4},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  6,					/* cost of moving MMX register */
  {12, 12},				/* cost of loading MMX registers
					   in SImode and DImode */
  {12, 12},				/* cost of storing MMX registers
					   in SImode and DImode */
  6,					/* cost of moving SSE register */
  {12, 12, 12},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {12, 12, 12},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  8,					/* MMX or SSE register to integer */
  8,					/* size of l1 cache.  */
  1024,					/* size of l2 cache.  */
  128,					/* size of prefetch block */
  8,					/* number of parallel prefetches */
  1,					/* Branch cost */
  COSTS_N_INSNS (6),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (8),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (40),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (3),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (3),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (44),			/* cost of FSQRT instruction.  */
  {{libcall, {{12, loop_1_byte}, {-1, rep_prefix_4_byte}}},
   {libcall, {{32, loop}, {20000, rep_prefix_8_byte},
	      {100000, unrolled_loop}, {-1, libcall}}}},
  {{libcall, {{6, loop_1_byte}, {48, loop}, {20480, rep_prefix_4_byte},
   {-1, libcall}}},
   {libcall, {{24, loop}, {64, unrolled_loop},
	      {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs core2_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (3),			/*                               HI */
   COSTS_N_INSNS (3),			/*                               SI */
   COSTS_N_INSNS (3),			/*                               DI */
   COSTS_N_INSNS (3)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (22),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (22),			/*                          HI */
   COSTS_N_INSNS (22),			/*                          SI */
   COSTS_N_INSNS (22),			/*                          DI */
   COSTS_N_INSNS (22)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  16,					/* MOVE_RATIO */
  2,					/* cost for loading QImode using movzbl */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {6, 6, 6},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 4, 4},				/* cost of loading integer registers */
  2,					/* cost of moving MMX register */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {6, 6, 6},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 4},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  2,					/* MMX or SSE register to integer */
  32,					/* size of l1 cache.  */
  2048,					/* size of l2 cache.  */
  128,					/* size of prefetch block */
  8,					/* number of parallel prefetches */
  3,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (32),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (58),			/* cost of FSQRT instruction.  */
  {{libcall, {{11, loop}, {-1, rep_prefix_4_byte}}},
   {libcall, {{32, loop}, {64, rep_prefix_4_byte},
	      {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  {{libcall, {{8, loop}, {15, unrolled_loop},
	      {2048, rep_prefix_4_byte}, {-1, libcall}}},
   {libcall, {{24, loop}, {32, unrolled_loop},
	      {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

/* Generic64 should produce code tuned for Nocona and K8.  */
static const
struct processor_costs generic64_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  /* On all chips taken into consideration lea is 2 cycles and more.  With
     this cost however our current implementation of synth_mult results in
     use of unnecessary temporary registers causing regression on several
     SPECfp benchmarks.  */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*                               HI */
   COSTS_N_INSNS (3),			/*                               SI */
   COSTS_N_INSNS (4),			/*                               DI */
   COSTS_N_INSNS (2)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*                          HI */
   COSTS_N_INSNS (42),			/*                          SI */
   COSTS_N_INSNS (74),			/*                          DI */
   COSTS_N_INSNS (74)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {12, 12, 12},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 8},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {8, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {8, 8},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {8, 8, 8},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {8, 8, 8},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  5,					/* MMX or SSE register to integer */
  32,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  /* Benchmarks shows large regressions on K8 sixtrack benchmark when this value
     is increased to perhaps more appropriate value of 5.  */
  3,					/* Branch cost */
  COSTS_N_INSNS (8),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (8),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (20),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (8),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (8),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (40),			/* cost of FSQRT instruction.  */
  {DUMMY_STRINGOP_ALGS,
   {libcall, {{32, loop}, {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  {DUMMY_STRINGOP_ALGS,
   {libcall, {{32, loop}, {8192, rep_prefix_8_byte}, {-1, libcall}}}},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

/* Generic32 should produce code tuned for Athlon, PPro, Pentium4, Nocona and K8.  */
static const
struct processor_costs generic32_cost = {
  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*                               HI */
   COSTS_N_INSNS (3),			/*                               SI */
   COSTS_N_INSNS (4),			/*                               DI */
   COSTS_N_INSNS (2)},			/*                               other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*                          HI */
   COSTS_N_INSNS (42),			/*                          SI */
   COSTS_N_INSNS (74),			/*                          DI */
   COSTS_N_INSNS (74)},			/*                          other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  4,					/* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {12, 12, 12},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 8},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {8, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {8, 8},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {8, 8, 8},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {8, 8, 8},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  5,					/* MMX or SSE register to integer */
  32,					/* size of l1 cache.  */
  256,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  3,					/* Branch cost */
  COSTS_N_INSNS (8),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (8),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (20),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (8),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (8),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (40),			/* cost of FSQRT instruction.  */
  {{libcall, {{32, loop}, {8192, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  {{libcall, {{32, loop}, {8192, rep_prefix_4_byte}, {-1, libcall}}},
   DUMMY_STRINGOP_ALGS},
  1,                                    /* scalar_stmt_cost.  */
  1,                                    /* scalar load_cost.  */
  1,                                    /* scalar_store_cost.  */
  1,                                    /* vec_stmt_cost.  */
  1,                                    /* vec_to_scalar_cost.  */
  1,                                    /* scalar_to_vec_cost.  */
  1,                                    /* vec_align_load_cost.  */
  2,                                    /* vec_unalign_load_cost.  */
  1,                                    /* vec_store_cost.  */
  3,                                    /* cond_taken_branch_cost.  */
  1,                                    /* cond_not_taken_branch_cost.  */
};

const struct processor_costs *ix86_cost = &pentium_cost;

/* Processor feature/optimization bitmasks.  */
#define m_386 (1<<PROCESSOR_I386)
#define m_486 (1<<PROCESSOR_I486)
#define m_PENT (1<<PROCESSOR_PENTIUM)
#define m_PPRO (1<<PROCESSOR_PENTIUMPRO)
#define m_PENT4  (1<<PROCESSOR_PENTIUM4)
#define m_NOCONA  (1<<PROCESSOR_NOCONA)
#define m_CORE2  (1<<PROCESSOR_CORE2)

#define m_GEODE  (1<<PROCESSOR_GEODE)
#define m_K6  (1<<PROCESSOR_K6)
#define m_K6_GEODE  (m_K6 | m_GEODE)
#define m_K8  (1<<PROCESSOR_K8)
#define m_ATHLON  (1<<PROCESSOR_ATHLON)
#define m_ATHLON_K8  (m_K8 | m_ATHLON)
#define m_AMDFAM10  (1<<PROCESSOR_AMDFAM10)
#define m_AMD_MULTIPLE  (m_K8 | m_ATHLON | m_AMDFAM10)

#define m_GENERIC32 (1<<PROCESSOR_GENERIC32)
#define m_GENERIC64 (1<<PROCESSOR_GENERIC64)

/* Generic instruction choice should be common subset of supported CPUs
   (PPro/PENT4/NOCONA/CORE2/Athlon/K8).  */
#define m_GENERIC (m_GENERIC32 | m_GENERIC64)

/* Feature tests against the various tunings.  */
unsigned int ix86_tune_features[X86_TUNE_LAST] = {
  /* X86_TUNE_USE_LEAVE: Leave does not affect Nocona SPEC2000 results
     negatively, so enabling for Generic64 seems like good code size
     tradeoff.  We can't enable it for 32bit generic because it does not
     work well with PPro base chips.  */
  m_386 | m_K6_GEODE | m_AMD_MULTIPLE | m_CORE2 | m_GENERIC64,

  /* X86_TUNE_PUSH_MEMORY */
  m_386 | m_K6_GEODE | m_AMD_MULTIPLE | m_PENT4
  | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_ZERO_EXTEND_WITH_AND */
  m_486 | m_PENT,

  /* X86_TUNE_USE_BIT_TEST */
  m_386,

  /* X86_TUNE_UNROLL_STRLEN */
  m_486 | m_PENT | m_PPRO | m_AMD_MULTIPLE | m_K6 | m_CORE2 | m_GENERIC,

  /* X86_TUNE_DEEP_BRANCH_PREDICTION */
  m_PPRO | m_K6_GEODE | m_AMD_MULTIPLE | m_PENT4 | m_GENERIC,

  /* X86_TUNE_BRANCH_PREDICTION_HINTS: Branch hints were put in P4 based
     on simulation result. But after P4 was made, no performance benefit
     was observed with branch hints.  It also increases the code size.
     As a result, icc never generates branch hints.  */
  0,

  /* X86_TUNE_DOUBLE_WITH_ADD */
  ~m_386,

  /* X86_TUNE_USE_SAHF */
  m_PPRO | m_K6_GEODE | m_K8 | m_AMDFAM10 | m_PENT4
  | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_MOVX: Enable to zero extend integer registers to avoid
     partial dependencies.  */
  m_AMD_MULTIPLE | m_PPRO | m_PENT4 | m_NOCONA
  | m_CORE2 | m_GENERIC | m_GEODE /* m_386 | m_K6 */,

  /* X86_TUNE_PARTIAL_REG_STALL: We probably ought to watch for partial
     register stalls on Generic32 compilation setting as well.  However
     in current implementation the partial register stalls are not eliminated
     very well - they can be introduced via subregs synthesized by combine
     and can happen in caller/callee saving sequences.  Because this option
     pays back little on PPro based chips and is in conflict with partial reg
     dependencies used by Athlon/P4 based chips, it is better to leave it off
     for generic32 for now.  */
  m_PPRO,

  /* X86_TUNE_PARTIAL_FLAG_REG_STALL */
  m_CORE2 | m_GENERIC,

  /* X86_TUNE_USE_HIMODE_FIOP */
  m_386 | m_486 | m_K6_GEODE,

  /* X86_TUNE_USE_SIMODE_FIOP */
  ~(m_PPRO | m_AMD_MULTIPLE | m_PENT | m_CORE2 | m_GENERIC),

  /* X86_TUNE_USE_MOV0 */
  m_K6,

  /* X86_TUNE_USE_CLTD */
  ~(m_PENT | m_K6 | m_CORE2 | m_GENERIC),

  /* X86_TUNE_USE_XCHGB: Use xchgb %rh,%rl instead of rolw/rorw $8,rx.  */
  m_PENT4,

  /* X86_TUNE_SPLIT_LONG_MOVES */
  m_PPRO,

  /* X86_TUNE_READ_MODIFY_WRITE */
  ~m_PENT,

  /* X86_TUNE_READ_MODIFY */
  ~(m_PENT | m_PPRO),

  /* X86_TUNE_PROMOTE_QIMODE */
  m_K6_GEODE | m_PENT | m_386 | m_486 | m_AMD_MULTIPLE | m_CORE2
  | m_GENERIC /* | m_PENT4 ? */,

  /* X86_TUNE_FAST_PREFIX */
  ~(m_PENT | m_486 | m_386),

  /* X86_TUNE_SINGLE_STRINGOP */
  m_386 | m_PENT4 | m_NOCONA,

  /* X86_TUNE_QIMODE_MATH */
  ~0,

  /* X86_TUNE_HIMODE_MATH: On PPro this flag is meant to avoid partial
     register stalls.  Just like X86_TUNE_PARTIAL_REG_STALL this option
     might be considered for Generic32 if our scheme for avoiding partial
     stalls was more effective.  */
  ~m_PPRO,

  /* X86_TUNE_PROMOTE_QI_REGS */
  0,

  /* X86_TUNE_PROMOTE_HI_REGS */
  m_PPRO,

  /* X86_TUNE_ADD_ESP_4: Enable if add/sub is preferred over 1/2 push/pop.  */
  m_AMD_MULTIPLE | m_K6_GEODE | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_ADD_ESP_8 */
  m_AMD_MULTIPLE | m_PPRO | m_K6_GEODE | m_386
  | m_486 | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_SUB_ESP_4 */
  m_AMD_MULTIPLE | m_PPRO | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_SUB_ESP_8 */
  m_AMD_MULTIPLE | m_PPRO | m_386 | m_486
  | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_INTEGER_DFMODE_MOVES: Enable if integer moves are preferred
     for DFmode copies */
  ~(m_AMD_MULTIPLE | m_PENT4 | m_NOCONA | m_PPRO | m_CORE2
    | m_GENERIC | m_GEODE),

  /* X86_TUNE_PARTIAL_REG_DEPENDENCY */
  m_AMD_MULTIPLE | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_SSE_PARTIAL_REG_DEPENDENCY: In the Generic model we have a
     conflict here in between PPro/Pentium4 based chips that thread 128bit
     SSE registers as single units versus K8 based chips that divide SSE
     registers to two 64bit halves.  This knob promotes all store destinations
     to be 128bit to allow register renaming on 128bit SSE units, but usually
     results in one extra microop on 64bit SSE units.  Experimental results
     shows that disabling this option on P4 brings over 20% SPECfp regression,
     while enabling it on K8 brings roughly 2.4% regression that can be partly
     masked by careful scheduling of moves.  */
  m_PENT4 | m_NOCONA | m_PPRO | m_CORE2 | m_GENERIC | m_AMDFAM10,

  /* X86_TUNE_SSE_UNALIGNED_MOVE_OPTIMAL */
  m_AMDFAM10,

  /* X86_TUNE_SSE_SPLIT_REGS: Set for machines where the type and dependencies
     are resolved on SSE register parts instead of whole registers, so we may
     maintain just lower part of scalar values in proper format leaving the
     upper part undefined.  */
  m_ATHLON_K8,

  /* X86_TUNE_SSE_TYPELESS_STORES */
  m_AMD_MULTIPLE,

  /* X86_TUNE_SSE_LOAD0_BY_PXOR */
  m_PPRO | m_PENT4 | m_NOCONA,

  /* X86_TUNE_MEMORY_MISMATCH_STALL */
  m_AMD_MULTIPLE | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_PROLOGUE_USING_MOVE */
  m_ATHLON_K8 | m_PPRO | m_CORE2 | m_GENERIC,

  /* X86_TUNE_EPILOGUE_USING_MOVE */
  m_ATHLON_K8 | m_PPRO | m_CORE2 | m_GENERIC,

  /* X86_TUNE_SHIFT1 */
  ~m_486,

  /* X86_TUNE_USE_FFREEP */
  m_AMD_MULTIPLE,

  /* X86_TUNE_INTER_UNIT_MOVES */
  ~(m_AMD_MULTIPLE | m_GENERIC),

  /* X86_TUNE_INTER_UNIT_CONVERSIONS */
  ~(m_AMDFAM10),

  /* X86_TUNE_FOUR_JUMP_LIMIT: Some CPU cores are not able to predict more
     than 4 branch instructions in the 16 byte window.  */
  m_PPRO | m_AMD_MULTIPLE | m_PENT4 | m_NOCONA | m_CORE2 | m_GENERIC,

  /* X86_TUNE_SCHEDULE */
  m_PPRO | m_AMD_MULTIPLE | m_K6_GEODE | m_PENT | m_CORE2 | m_GENERIC,

  /* X86_TUNE_USE_BT */
  m_AMD_MULTIPLE,

  /* X86_TUNE_USE_INCDEC */
  ~(m_PENT4 | m_NOCONA | m_GENERIC),

  /* X86_TUNE_PAD_RETURNS */
  m_AMD_MULTIPLE | m_CORE2 | m_GENERIC,

  /* X86_TUNE_EXT_80387_CONSTANTS */
  m_K6_GEODE | m_ATHLON_K8 | m_PENT4 | m_NOCONA | m_PPRO | m_CORE2 | m_GENERIC,

  /* X86_TUNE_SHORTEN_X87_SSE */
  ~m_K8,

  /* X86_TUNE_AVOID_VECTOR_DECODE */
  m_K8 | m_GENERIC64,

  /* X86_TUNE_PROMOTE_HIMODE_IMUL: Modern CPUs have same latency for HImode
     and SImode multiply, but 386 and 486 do HImode multiply faster.  */
  ~(m_386 | m_486),

  /* X86_TUNE_SLOW_IMUL_IMM32_MEM: Imul of 32-bit constant and memory is
     vector path on AMD machines.  */
  m_K8 | m_GENERIC64 | m_AMDFAM10,

  /* X86_TUNE_SLOW_IMUL_IMM8: Imul of 8-bit constant is vector path on AMD
     machines.  */
  m_K8 | m_GENERIC64 | m_AMDFAM10,

  /* X86_TUNE_MOVE_M1_VIA_OR: On pentiums, it is faster to load -1 via OR
     than a MOV.  */
  m_PENT,

  /* X86_TUNE_NOT_UNPAIRABLE: NOT is not pairable on Pentium, while XOR is,
     but one byte longer.  */
  m_PENT,

  /* X86_TUNE_NOT_VECTORMODE: On AMD K6, NOT is vector decoded with memory
     operand that cannot be represented using a modRM byte.  The XOR
     replacement is long decoded, so this split helps here as well.  */
  m_K6,

  /* X86_TUNE_USE_VECTOR_CONVERTS: Prefer vector packed SSE conversion
     from integer to FP. */
  m_AMDFAM10,
};

/* Feature tests against the various architecture variations.  */
unsigned int ix86_arch_features[X86_ARCH_LAST] = {
  /* X86_ARCH_CMOVE: Conditional move was added for pentiumpro.  */
  ~(m_386 | m_486 | m_PENT | m_K6),

  /* X86_ARCH_CMPXCHG: Compare and exchange was added for 80486.  */
  ~m_386,

  /* X86_ARCH_CMPXCHG8B: Compare and exchange 8 bytes was added for pentium. */
  ~(m_386 | m_486),

  /* X86_ARCH_XADD: Exchange and add was added for 80486.  */
  ~m_386,

  /* X86_ARCH_BSWAP: Byteswap was added for 80486.  */
  ~m_386,
};

static const unsigned int x86_accumulate_outgoing_args
  = m_AMD_MULTIPLE | m_PENT4 | m_NOCONA | m_PPRO | m_CORE2 | m_GENERIC;

static const unsigned int x86_arch_always_fancy_math_387
  = m_PENT | m_PPRO | m_AMD_MULTIPLE | m_PENT4
    | m_NOCONA | m_CORE2 | m_GENERIC;

static enum stringop_alg stringop_alg = no_stringop;

/* In case the average insn count for single function invocation is
   lower than this constant, emit fast (but longer) prologue and
   epilogue code.  */
#define FAST_PROLOGUE_INSN_COUNT 20

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
  /* flags, fpsr, fpcr, frame */
  NO_REGS, NO_REGS, NO_REGS, NON_Q_REGS,
  /* SSE registers */
  SSE_FIRST_REG, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  SSE_REGS, SSE_REGS,
  /* MMX registers */
  MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS, MMX_REGS,
  MMX_REGS, MMX_REGS,
  /* REX registers */
  NON_Q_REGS, NON_Q_REGS, NON_Q_REGS, NON_Q_REGS,
  NON_Q_REGS, NON_Q_REGS, NON_Q_REGS, NON_Q_REGS,
  /* SSE REX registers */
  SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS, SSE_REGS,
  SSE_REGS, SSE_REGS,
};

/* The "default" register map used in 32bit mode.  */

int const dbx_register_map[FIRST_PSEUDO_REGISTER] =
{
  0, 2, 1, 3, 6, 7, 4, 5,		/* general regs */
  12, 13, 14, 15, 16, 17, 18, 19,	/* fp regs */
  -1, -1, -1, -1, -1,			/* arg, flags, fpsr, fpcr, frame */
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

static int const x86_64_ms_abi_int_parameter_registers[4] =
{
  2 /*RCX*/, 1 /*RDX*/,
  FIRST_REX_INT_REG /*R8 */, FIRST_REX_INT_REG + 1 /*R9 */
};

static int const x86_64_int_return_registers[4] =
{
  0 /*RAX*/, 1 /*RDX*/, 5 /*RDI*/, 4 /*RSI*/
};

/* The "default" register map used in 64bit mode.  */
int const dbx64_register_map[FIRST_PSEUDO_REGISTER] =
{
  0, 1, 2, 3, 4, 5, 6, 7,		/* general regs */
  33, 34, 35, 36, 37, 38, 39, 40,	/* fp regs */
  -1, -1, -1, -1, -1,			/* arg, flags, fpsr, fpcr, frame */
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
  -1, 9, -1, -1, -1,			/* arg, flags, fpsr, fpcr, frame */
  21, 22, 23, 24, 25, 26, 27, 28,	/* SSE registers */
  29, 30, 31, 32, 33, 34, 35, 36,	/* MMX registers */
  -1, -1, -1, -1, -1, -1, -1, -1,	/* extended integer registers */
  -1, -1, -1, -1, -1, -1, -1, -1,	/* extended SSE registers */
};

/* Test and compare insns in i386.md store the information needed to
   generate branch and scc insns here.  */

rtx ix86_compare_op0 = NULL_RTX;
rtx ix86_compare_op1 = NULL_RTX;
rtx ix86_compare_emitted = NULL_RTX;

/* Size of the register save area.  */
#define X86_64_VARARGS_SIZE (REGPARM_MAX * UNITS_PER_WORD + SSE_REGPARM_MAX * 16)

/* Define the structure for the machine field in struct function.  */

struct stack_local_entry GTY(())
{
  unsigned short mode;
  unsigned short n;
  rtx rtl;
  struct stack_local_entry *next;
};

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

  /* When save_regs_using_mov is set, emit prologue using
     move instead of push instructions.  */
  bool save_regs_using_mov;
};

/* Code model option.  */
enum cmodel ix86_cmodel;
/* Asm dialect.  */
enum asm_dialect ix86_asm_dialect = ASM_ATT;
/* TLS dialects.  */
enum tls_dialect ix86_tls_dialect = TLS_DIALECT_GNU;

/* Which unit we are generating floating point math for.  */
enum fpmath_unit ix86_fpmath;

/* Which cpu are we scheduling for.  */
enum processor_type ix86_tune;

/* Which instruction set architecture to use.  */
enum processor_type ix86_arch;

/* true if sse prefetch instruction is not NOOP.  */
int x86_prefetch_sse;

/* ix86_regparm_string as a number */
static int ix86_regparm;

/* -mstackrealign option */
extern int ix86_force_align_arg_pointer;
static const char ix86_force_align_arg_pointer_string[] = "force_align_arg_pointer";

/* Preferred alignment for stack boundary in bits.  */
unsigned int ix86_preferred_stack_boundary;

/* Values 1-5: see jump.c */
int ix86_branch_cost;

/* Variables which are this size or smaller are put in the data/bss
   or ldata/lbss sections.  */

int ix86_section_threshold = 65536;

/* Prefix built by ASM_GENERATE_INTERNAL_LABEL.  */
char internal_label_prefix[16];
int internal_label_prefix_len;

/* Fence to use after loop using movnt.  */
tree x86_mfence;

/* Register class used for passing given 64bit part of the argument.
   These represent classes as documented by the PS ABI, with the exception
   of SSESF, SSEDF classes, that are basically SSE class, just gcc will
   use SF or DFmode move instead of DImode to avoid reformatting penalties.

   Similarly we play games with INTEGERSI_CLASS to use cheaper SImode moves
   whenever possible (upper half does contain padding).  */
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
    X86_64_COMPLEX_X87_CLASS,
    X86_64_MEMORY_CLASS
  };
static const char * const x86_64_reg_class_name[] =
{
  "no", "integer", "integerSI", "sse", "sseSF", "sseDF",
  "sseup", "x87", "x87up", "cplx87", "no"
};

#define MAX_CLASSES 4

/* Table of constants used by fldpi, fldln2, etc....  */
static REAL_VALUE_TYPE ext_80387_constants_table [5];
static bool ext_80387_constants_init = 0;


static struct machine_function * ix86_init_machine_status (void);
static rtx ix86_function_value (const_tree, const_tree, bool);
static int ix86_function_regparm (const_tree, const_tree);
static void ix86_compute_frame_layout (struct ix86_frame *);
static bool ix86_expand_vector_init_one_nonzero (bool, enum machine_mode,
						 rtx, rtx, int);


/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  */
#ifndef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1
#endif

/* Bit flags that specify the ISA we are compiling for.  */
int ix86_isa_flags = TARGET_64BIT_DEFAULT | TARGET_SUBTARGET_ISA_DEFAULT;

/* A mask of ix86_isa_flags that includes bit X if X
   was set or cleared on the command line.  */
static int ix86_isa_flags_explicit;

/* Define a set of ISAs which are available when a given ISA is
   enabled.  MMX and SSE ISAs are handled separately.  */

#define OPTION_MASK_ISA_MMX_SET OPTION_MASK_ISA_MMX
#define OPTION_MASK_ISA_3DNOW_SET \
  (OPTION_MASK_ISA_3DNOW | OPTION_MASK_ISA_MMX_SET)

#define OPTION_MASK_ISA_SSE_SET OPTION_MASK_ISA_SSE
#define OPTION_MASK_ISA_SSE2_SET \
  (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_SSE_SET)
#define OPTION_MASK_ISA_SSE3_SET \
  (OPTION_MASK_ISA_SSE3 | OPTION_MASK_ISA_SSE2_SET)
#define OPTION_MASK_ISA_SSSE3_SET \
  (OPTION_MASK_ISA_SSSE3 | OPTION_MASK_ISA_SSE3_SET)
#define OPTION_MASK_ISA_SSE4_1_SET \
  (OPTION_MASK_ISA_SSE4_1 | OPTION_MASK_ISA_SSSE3_SET)
#define OPTION_MASK_ISA_SSE4_2_SET \
  (OPTION_MASK_ISA_SSE4_2 | OPTION_MASK_ISA_SSE4_1_SET)

/* SSE4 includes both SSE4.1 and SSE4.2. -msse4 should be the same
   as -msse4.2.  */
#define OPTION_MASK_ISA_SSE4_SET OPTION_MASK_ISA_SSE4_2_SET

#define OPTION_MASK_ISA_SSE4A_SET \
  (OPTION_MASK_ISA_SSE4A | OPTION_MASK_ISA_SSE3_SET)
#define OPTION_MASK_ISA_SSE5_SET \
  (OPTION_MASK_ISA_SSE5 | OPTION_MASK_ISA_SSE4A_SET)

/* Define a set of ISAs which aren't available when a given ISA is
   disabled.  MMX and SSE ISAs are handled separately.  */

#define OPTION_MASK_ISA_MMX_UNSET \
  (OPTION_MASK_ISA_MMX | OPTION_MASK_ISA_3DNOW_UNSET)
#define OPTION_MASK_ISA_3DNOW_UNSET \
  (OPTION_MASK_ISA_3DNOW | OPTION_MASK_ISA_3DNOW_A_UNSET)
#define OPTION_MASK_ISA_3DNOW_A_UNSET OPTION_MASK_ISA_3DNOW_A

#define OPTION_MASK_ISA_SSE_UNSET \
  (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_SSE2_UNSET)
#define OPTION_MASK_ISA_SSE2_UNSET \
  (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_SSE3_UNSET)
#define OPTION_MASK_ISA_SSE3_UNSET \
  (OPTION_MASK_ISA_SSE3 \
   | OPTION_MASK_ISA_SSSE3_UNSET \
   | OPTION_MASK_ISA_SSE4A_UNSET )
#define OPTION_MASK_ISA_SSSE3_UNSET \
  (OPTION_MASK_ISA_SSSE3 | OPTION_MASK_ISA_SSE4_1_UNSET)
#define OPTION_MASK_ISA_SSE4_1_UNSET \
  (OPTION_MASK_ISA_SSE4_1 | OPTION_MASK_ISA_SSE4_2_UNSET)
#define OPTION_MASK_ISA_SSE4_2_UNSET OPTION_MASK_ISA_SSE4_2

/* SSE4 includes both SSE4.1 and SSE4.2.  -mno-sse4 should the same
   as -mno-sse4.1. */
#define OPTION_MASK_ISA_SSE4_UNSET OPTION_MASK_ISA_SSE4_1_UNSET

#define OPTION_MASK_ISA_SSE4A_UNSET \
  (OPTION_MASK_ISA_SSE4A | OPTION_MASK_ISA_SSE5_UNSET)

#define OPTION_MASK_ISA_SSE5_UNSET OPTION_MASK_ISA_SSE5

/* Vectorization library interface and handlers.  */
tree (*ix86_veclib_handler)(enum built_in_function, tree, tree) = NULL;
static tree ix86_veclibabi_acml (enum built_in_function, tree, tree);

/* Implement TARGET_HANDLE_OPTION.  */

static bool
ix86_handle_option (size_t code, const char *arg ATTRIBUTE_UNUSED, int value)
{
  switch (code)
    {
    case OPT_mmmx:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_MMX_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_MMX_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_MMX_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_MMX_UNSET;
	}
      return true;

    case OPT_m3dnow:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_3DNOW_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_3DNOW_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_3DNOW_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_3DNOW_UNSET;
	}
      return true;

    case OPT_m3dnowa:
      return false;

    case OPT_msse:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE_UNSET;
	}
      return true;

    case OPT_msse2:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE2_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE2_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE2_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE2_UNSET;
	}
      return true;

    case OPT_msse3:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE3_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE3_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE3_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE3_UNSET;
	}
      return true;

    case OPT_mssse3:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSSE3_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSSE3_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSSE3_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSSE3_UNSET;
	}
      return true;

    case OPT_msse4_1:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE4_1_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_1_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4_1_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_1_UNSET;
	}
      return true;

    case OPT_msse4_2:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE4_2_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_2_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4_2_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_2_UNSET;
	}
      return true;

    case OPT_msse4:
      ix86_isa_flags |= OPTION_MASK_ISA_SSE4_SET;
      ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_SET;
      return true;

    case OPT_mno_sse4:
      ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4_UNSET;
      ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_UNSET;
      return true;

    case OPT_msse4a:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE4A_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4A_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4A_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4A_UNSET;
	}
      return true;

    case OPT_msse5:
      if (value)
	{
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE5_SET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE5_SET;
	}
      else
	{
	  ix86_isa_flags &= ~OPTION_MASK_ISA_SSE5_UNSET;
	  ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE5_UNSET;
	}
      return true;

    default:
      return true;
    }
}

/* Sometimes certain combinations of command options do not make
   sense on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   Don't use this macro to turn on various extra optimizations for
   `-O'.  That is what `OPTIMIZATION_OPTIONS' is for.  */

void
override_options (void)
{
  int i;
  int ix86_tune_defaulted = 0;
  int ix86_arch_specified = 0;
  unsigned int ix86_arch_mask, ix86_tune_mask;

  /* Comes from final.c -- no real reason to change it.  */
#define MAX_CODE_ALIGN 16

  static struct ptt
    {
      const struct processor_costs *cost;	/* Processor costs */
      const int align_loop;			/* Default alignments.  */
      const int align_loop_max_skip;
      const int align_jump;
      const int align_jump_max_skip;
      const int align_func;
    }
  const processor_target_table[PROCESSOR_max] =
    {
      {&i386_cost, 4, 3, 4, 3, 4},
      {&i486_cost, 16, 15, 16, 15, 16},
      {&pentium_cost, 16, 7, 16, 7, 16},
      {&pentiumpro_cost, 16, 15, 16, 10, 16},
      {&geode_cost, 0, 0, 0, 0, 0},
      {&k6_cost, 32, 7, 32, 7, 32},
      {&athlon_cost, 16, 7, 16, 7, 16},
      {&pentium4_cost, 0, 0, 0, 0, 0},
      {&k8_cost, 16, 7, 16, 7, 16},
      {&nocona_cost, 0, 0, 0, 0, 0},
      {&core2_cost, 16, 10, 16, 10, 16},
      {&generic32_cost, 16, 7, 16, 7, 16},
      {&generic64_cost, 16, 10, 16, 10, 16},
      {&amdfam10_cost, 32, 24, 32, 7, 32}
    };

  static const char *const cpu_names[TARGET_CPU_DEFAULT_max] =
    {
      "generic",
      "i386",
      "i486",
      "pentium",
      "pentium-mmx",
      "pentiumpro",
      "pentium2",
      "pentium3",
      "pentium4",
      "pentium-m",
      "prescott",
      "nocona",
      "core2",
      "geode",
      "k6",
      "k6-2",
      "k6-3",
      "athlon",
      "athlon-4",
      "k8",
      "amdfam10"
    };

  enum pta_flags
    {
      PTA_SSE = 1 << 0,
      PTA_SSE2 = 1 << 1,
      PTA_SSE3 = 1 << 2,
      PTA_MMX = 1 << 3,
      PTA_PREFETCH_SSE = 1 << 4,
      PTA_3DNOW = 1 << 5,
      PTA_3DNOW_A = 1 << 6,
      PTA_64BIT = 1 << 7,
      PTA_SSSE3 = 1 << 8,
      PTA_CX16 = 1 << 9,
      PTA_POPCNT = 1 << 10,
      PTA_ABM = 1 << 11,
      PTA_SSE4A = 1 << 12,
      PTA_NO_SAHF = 1 << 13,
      PTA_SSE4_1 = 1 << 14,
      PTA_SSE4_2 = 1 << 15,
      PTA_SSE5 = 1 << 16
    };

  static struct pta
    {
      const char *const name;		/* processor name or nickname.  */
      const enum processor_type processor;
      const unsigned /*enum pta_flags*/ flags;
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
      {"c3-2", PROCESSOR_PENTIUMPRO, PTA_MMX | PTA_SSE},
      {"i686", PROCESSOR_PENTIUMPRO, 0},
      {"pentiumpro", PROCESSOR_PENTIUMPRO, 0},
      {"pentium2", PROCESSOR_PENTIUMPRO, PTA_MMX},
      {"pentium3", PROCESSOR_PENTIUMPRO, PTA_MMX | PTA_SSE},
      {"pentium3m", PROCESSOR_PENTIUMPRO, PTA_MMX | PTA_SSE},
      {"pentium-m", PROCESSOR_PENTIUMPRO, PTA_MMX | PTA_SSE | PTA_SSE2},
      {"pentium4", PROCESSOR_PENTIUM4, PTA_MMX |PTA_SSE | PTA_SSE2},
      {"pentium4m", PROCESSOR_PENTIUM4, PTA_MMX | PTA_SSE | PTA_SSE2},
      {"prescott", PROCESSOR_NOCONA, PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3},
      {"nocona", PROCESSOR_NOCONA, (PTA_64BIT
				    | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
				    | PTA_CX16 | PTA_NO_SAHF)},
      {"core2", PROCESSOR_CORE2, (PTA_64BIT
				  | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
				  | PTA_SSSE3
				  | PTA_CX16)},
      {"geode", PROCESSOR_GEODE, (PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				  |PTA_PREFETCH_SSE)},
      {"k6", PROCESSOR_K6, PTA_MMX},
      {"k6-2", PROCESSOR_K6, PTA_MMX | PTA_3DNOW},
      {"k6-3", PROCESSOR_K6, PTA_MMX | PTA_3DNOW},
      {"athlon", PROCESSOR_ATHLON, (PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				    | PTA_PREFETCH_SSE)},
      {"athlon-tbird", PROCESSOR_ATHLON, (PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
					  | PTA_PREFETCH_SSE)},
      {"athlon-4", PROCESSOR_ATHLON, (PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				      | PTA_SSE)},
      {"athlon-xp", PROCESSOR_ATHLON, (PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				       | PTA_SSE)},
      {"athlon-mp", PROCESSOR_ATHLON, (PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				       | PTA_SSE)},
      {"x86-64", PROCESSOR_K8, (PTA_64BIT
				| PTA_MMX | PTA_SSE | PTA_SSE2
				| PTA_NO_SAHF)},
      {"k8", PROCESSOR_K8, (PTA_64BIT
			    | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
			    | PTA_SSE | PTA_SSE2
			    | PTA_NO_SAHF)},
      {"k8-sse3", PROCESSOR_K8, (PTA_64BIT
				 | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				 | PTA_SSE | PTA_SSE2 | PTA_SSE3
				 | PTA_NO_SAHF)},
      {"opteron", PROCESSOR_K8, (PTA_64BIT
				 | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				 | PTA_SSE | PTA_SSE2
				 | PTA_NO_SAHF)},
      {"opteron-sse3", PROCESSOR_K8, (PTA_64BIT
				      | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				      | PTA_SSE | PTA_SSE2 | PTA_SSE3
				      | PTA_NO_SAHF)},
      {"athlon64", PROCESSOR_K8, (PTA_64BIT
				  | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				  | PTA_SSE | PTA_SSE2
				  | PTA_NO_SAHF)},
      {"athlon64-sse3", PROCESSOR_K8, (PTA_64BIT
				       | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				       | PTA_SSE | PTA_SSE2 | PTA_SSE3
				       | PTA_NO_SAHF)},
      {"athlon-fx", PROCESSOR_K8, (PTA_64BIT
				   | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
				   | PTA_SSE | PTA_SSE2
				   | PTA_NO_SAHF)},
      {"amdfam10", PROCESSOR_AMDFAM10, (PTA_64BIT
					| PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
					| PTA_SSE | PTA_SSE2 | PTA_SSE3
					| PTA_SSE4A
					| PTA_CX16 | PTA_ABM)},
      {"barcelona", PROCESSOR_AMDFAM10, (PTA_64BIT
					 | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A
					 | PTA_SSE | PTA_SSE2 | PTA_SSE3
					 | PTA_SSE4A
					 | PTA_CX16 | PTA_ABM)},
      {"generic32", PROCESSOR_GENERIC32, 0 /* flags are only used for -march switch.  */ },
      {"generic64", PROCESSOR_GENERIC64, PTA_64BIT /* flags are only used for -march switch.  */ },
    };

  int const pta_size = ARRAY_SIZE (processor_alias_table);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

#ifdef SUBSUBTARGET_OVERRIDE_OPTIONS
  SUBSUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* -fPIC is the default for x86_64.  */
  if (TARGET_MACHO && TARGET_64BIT)
    flag_pic = 2;

  /* Set the default values for switches whose default depends on TARGET_64BIT
     in case they weren't overwritten by command line options.  */
  if (TARGET_64BIT)
    {
      /* Mach-O doesn't support omitting the frame pointer for now.  */
      if (flag_omit_frame_pointer == 2)
	flag_omit_frame_pointer = (TARGET_MACHO ? 0 : 1);
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

  /* Need to check -mtune=generic first.  */
  if (ix86_tune_string)
    {
      if (!strcmp (ix86_tune_string, "generic")
	  || !strcmp (ix86_tune_string, "i686")
	  /* As special support for cross compilers we read -mtune=native
	     as -mtune=generic.  With native compilers we won't see the
	     -mtune=native, as it was changed by the driver.  */
	  || !strcmp (ix86_tune_string, "native"))
	{
	  if (TARGET_64BIT)
	    ix86_tune_string = "generic64";
	  else
	    ix86_tune_string = "generic32";
	}
      else if (!strncmp (ix86_tune_string, "generic", 7))
	error ("bad value (%s) for -mtune= switch", ix86_tune_string);
    }
  else
    {
      if (ix86_arch_string)
	ix86_tune_string = ix86_arch_string;
      if (!ix86_tune_string)
	{
	  ix86_tune_string = cpu_names[TARGET_CPU_DEFAULT];
	  ix86_tune_defaulted = 1;
	}

      /* ix86_tune_string is set to ix86_arch_string or defaulted.  We
	 need to use a sensible tune option.  */
      if (!strcmp (ix86_tune_string, "generic")
	  || !strcmp (ix86_tune_string, "x86-64")
	  || !strcmp (ix86_tune_string, "i686"))
	{
	  if (TARGET_64BIT)
	    ix86_tune_string = "generic64";
	  else
	    ix86_tune_string = "generic32";
	}
    }
  if (ix86_stringop_string)
    {
      if (!strcmp (ix86_stringop_string, "rep_byte"))
	stringop_alg = rep_prefix_1_byte;
      else if (!strcmp (ix86_stringop_string, "libcall"))
	stringop_alg = libcall;
      else if (!strcmp (ix86_stringop_string, "rep_4byte"))
	stringop_alg = rep_prefix_4_byte;
      else if (!strcmp (ix86_stringop_string, "rep_8byte"))
	stringop_alg = rep_prefix_8_byte;
      else if (!strcmp (ix86_stringop_string, "byte_loop"))
	stringop_alg = loop_1_byte;
      else if (!strcmp (ix86_stringop_string, "loop"))
	stringop_alg = loop;
      else if (!strcmp (ix86_stringop_string, "unrolled_loop"))
	stringop_alg = unrolled_loop;
      else
	error ("bad value (%s) for -mstringop-strategy= switch", ix86_stringop_string);
    }
  if (!strcmp (ix86_tune_string, "x86-64"))
    warning (OPT_Wdeprecated, "-mtune=x86-64 is deprecated.  Use -mtune=k8 or "
	     "-mtune=generic instead as appropriate.");

  if (!ix86_arch_string)
    ix86_arch_string = TARGET_64BIT ? "x86-64" : "i386";
  else
    ix86_arch_specified = 1;

  if (!strcmp (ix86_arch_string, "generic"))
    error ("generic CPU can be used only for -mtune= switch");
  if (!strncmp (ix86_arch_string, "generic", 7))
    error ("bad value (%s) for -march= switch", ix86_arch_string);

  if (ix86_cmodel_string != 0)
    {
      if (!strcmp (ix86_cmodel_string, "small"))
	ix86_cmodel = flag_pic ? CM_SMALL_PIC : CM_SMALL;
      else if (!strcmp (ix86_cmodel_string, "medium"))
	ix86_cmodel = flag_pic ? CM_MEDIUM_PIC : CM_MEDIUM;
      else if (!strcmp (ix86_cmodel_string, "large"))
	ix86_cmodel = flag_pic ? CM_LARGE_PIC : CM_LARGE;
      else if (flag_pic)
	error ("code model %s does not support PIC mode", ix86_cmodel_string);
      else if (!strcmp (ix86_cmodel_string, "32"))
	ix86_cmodel = CM_32;
      else if (!strcmp (ix86_cmodel_string, "kernel") && !flag_pic)
	ix86_cmodel = CM_KERNEL;
      else
	error ("bad value (%s) for -mcmodel= switch", ix86_cmodel_string);
    }
  else
    {
      /* For TARGET_64BIT_MS_ABI, force pic on, in order to enable the
	 use of rip-relative addressing.  This eliminates fixups that
	 would otherwise be needed if this object is to be placed in a
	 DLL, and is essentially just as efficient as direct addressing.  */
      if (TARGET_64BIT_MS_ABI)
	ix86_cmodel = CM_SMALL_PIC, flag_pic = 1;
      else if (TARGET_64BIT)
	ix86_cmodel = flag_pic ? CM_SMALL_PIC : CM_SMALL;
      else
        ix86_cmodel = CM_32;
    }
  if (ix86_asm_string != 0)
    {
      if (! TARGET_MACHO
	  && !strcmp (ix86_asm_string, "intel"))
	ix86_asm_dialect = ASM_INTEL;
      else if (!strcmp (ix86_asm_string, "att"))
	ix86_asm_dialect = ASM_ATT;
      else
	error ("bad value (%s) for -masm= switch", ix86_asm_string);
    }
  if ((TARGET_64BIT == 0) != (ix86_cmodel == CM_32))
    error ("code model %qs not supported in the %s bit mode",
	   ix86_cmodel_string, TARGET_64BIT ? "64" : "32");
  if ((TARGET_64BIT != 0) != ((ix86_isa_flags & OPTION_MASK_ISA_64BIT) != 0))
    sorry ("%i-bit mode not compiled in",
	   (ix86_isa_flags & OPTION_MASK_ISA_64BIT) ? 64 : 32);

  for (i = 0; i < pta_size; i++)
    if (! strcmp (ix86_arch_string, processor_alias_table[i].name))
      {
	ix86_arch = processor_alias_table[i].processor;
	/* Default cpu tuning to the architecture.  */
	ix86_tune = ix86_arch;

	if (TARGET_64BIT && !(processor_alias_table[i].flags & PTA_64BIT))
	  error ("CPU you selected does not support x86-64 "
		 "instruction set");

	if (processor_alias_table[i].flags & PTA_MMX
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_MMX))
	  ix86_isa_flags |= OPTION_MASK_ISA_MMX;
	if (processor_alias_table[i].flags & PTA_3DNOW
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_3DNOW))
	  ix86_isa_flags |= OPTION_MASK_ISA_3DNOW;
	if (processor_alias_table[i].flags & PTA_3DNOW_A
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_3DNOW_A))
	  ix86_isa_flags |= OPTION_MASK_ISA_3DNOW_A;
	if (processor_alias_table[i].flags & PTA_SSE
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE;
	if (processor_alias_table[i].flags & PTA_SSE2
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE2))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE2;
	if (processor_alias_table[i].flags & PTA_SSE3
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE3))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE3;
	if (processor_alias_table[i].flags & PTA_SSSE3
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSSE3))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSSE3;
	if (processor_alias_table[i].flags & PTA_SSE4_1
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE4_1))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE4_1;
	if (processor_alias_table[i].flags & PTA_SSE4_2
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE4_2))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE4_2;
	if (processor_alias_table[i].flags & PTA_SSE4A
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE4A))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE4A;
	if (processor_alias_table[i].flags & PTA_SSE5
	    && !(ix86_isa_flags_explicit & OPTION_MASK_ISA_SSE5))
	  ix86_isa_flags |= OPTION_MASK_ISA_SSE5;

	if (processor_alias_table[i].flags & PTA_ABM)
	  x86_abm = true;
	if (processor_alias_table[i].flags & PTA_CX16)
	  x86_cmpxchg16b = true;
	if (processor_alias_table[i].flags & (PTA_POPCNT | PTA_ABM))
	  x86_popcnt = true;
	if (processor_alias_table[i].flags & (PTA_PREFETCH_SSE | PTA_SSE))
	  x86_prefetch_sse = true;
	if (!(TARGET_64BIT && (processor_alias_table[i].flags & PTA_NO_SAHF)))
	  x86_sahf = true;

	break;
      }

  if (i == pta_size)
    error ("bad value (%s) for -march= switch", ix86_arch_string);

  ix86_arch_mask = 1u << ix86_arch;
  for (i = 0; i < X86_ARCH_LAST; ++i)
    ix86_arch_features[i] &= ix86_arch_mask;

  for (i = 0; i < pta_size; i++)
    if (! strcmp (ix86_tune_string, processor_alias_table[i].name))
      {
	ix86_tune = processor_alias_table[i].processor;
	if (TARGET_64BIT && !(processor_alias_table[i].flags & PTA_64BIT))
	  {
	    if (ix86_tune_defaulted)
	      {
		ix86_tune_string = "x86-64";
		for (i = 0; i < pta_size; i++)
		  if (! strcmp (ix86_tune_string,
				processor_alias_table[i].name))
		    break;
		ix86_tune = processor_alias_table[i].processor;
	      }
	    else
	      error ("CPU you selected does not support x86-64 "
		     "instruction set");
	  }
        /* Intel CPUs have always interpreted SSE prefetch instructions as
	   NOPs; so, we can enable SSE prefetch instructions even when
	   -mtune (rather than -march) points us to a processor that has them.
	   However, the VIA C3 gives a SIGILL, so we only do that for i686 and
	   higher processors.  */
	if (TARGET_CMOVE
	    && (processor_alias_table[i].flags & (PTA_PREFETCH_SSE | PTA_SSE)))
	  x86_prefetch_sse = true;
	break;
      }
  if (i == pta_size)
    error ("bad value (%s) for -mtune= switch", ix86_tune_string);

  ix86_tune_mask = 1u << ix86_tune;
  for (i = 0; i < X86_TUNE_LAST; ++i)
    ix86_tune_features[i] &= ix86_tune_mask;

  if (optimize_size)
    ix86_cost = &size_cost;
  else
    ix86_cost = processor_target_table[ix86_tune].cost;

  /* Arrange to set up i386_stack_locals for all functions.  */
  init_machine_status = ix86_init_machine_status;

  /* Validate -mregparm= value.  */
  if (ix86_regparm_string)
    {
      if (TARGET_64BIT)
	warning (0, "-mregparm is ignored in 64-bit mode");
      i = atoi (ix86_regparm_string);
      if (i < 0 || i > REGPARM_MAX)
	error ("-mregparm=%d is not between 0 and %d", i, REGPARM_MAX);
      else
	ix86_regparm = i;
    }
  if (TARGET_64BIT)
    ix86_regparm = REGPARM_MAX;

  /* If the user has provided any of the -malign-* options,
     warn and use that value only if -falign-* is not set.
     Remove this code in GCC 3.2 or later.  */
  if (ix86_align_loops_string)
    {
      warning (0, "-malign-loops is obsolete, use -falign-loops");
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
      warning (0, "-malign-jumps is obsolete, use -falign-jumps");
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
      warning (0, "-malign-functions is obsolete, use -falign-functions");
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
      align_loops = processor_target_table[ix86_tune].align_loop;
      align_loops_max_skip = processor_target_table[ix86_tune].align_loop_max_skip;
    }
  if (align_jumps == 0)
    {
      align_jumps = processor_target_table[ix86_tune].align_jump;
      align_jumps_max_skip = processor_target_table[ix86_tune].align_jump_max_skip;
    }
  if (align_functions == 0)
    {
      align_functions = processor_target_table[ix86_tune].align_func;
    }

  /* Validate -mbranch-cost= value, or provide default.  */
  ix86_branch_cost = ix86_cost->branch_cost;
  if (ix86_branch_cost_string)
    {
      i = atoi (ix86_branch_cost_string);
      if (i < 0 || i > 5)
	error ("-mbranch-cost=%d is not between 0 and 5", i);
      else
	ix86_branch_cost = i;
    }
  if (ix86_section_threshold_string)
    {
      i = atoi (ix86_section_threshold_string);
      if (i < 0)
	error ("-mlarge-data-threshold=%d is negative", i);
      else
	ix86_section_threshold = i;
    }

  if (ix86_tls_dialect_string)
    {
      if (strcmp (ix86_tls_dialect_string, "gnu") == 0)
	ix86_tls_dialect = TLS_DIALECT_GNU;
      else if (strcmp (ix86_tls_dialect_string, "gnu2") == 0)
	ix86_tls_dialect = TLS_DIALECT_GNU2;
      else if (strcmp (ix86_tls_dialect_string, "sun") == 0)
	ix86_tls_dialect = TLS_DIALECT_SUN;
      else
	error ("bad value (%s) for -mtls-dialect= switch",
	       ix86_tls_dialect_string);
    }

  if (ix87_precision_string)
    {
      i = atoi (ix87_precision_string);
      if (i != 32 && i != 64 && i != 80)
	error ("pc%d is not valid precision setting (32, 64 or 80)", i);
    }

  if (TARGET_64BIT)
    {
      target_flags |= TARGET_SUBTARGET64_DEFAULT & ~target_flags_explicit;

      /* Enable by default the SSE and MMX builtins.  Do allow the user to
	 explicitly disable any of these.  In particular, disabling SSE and
	 MMX for kernel code is extremely useful.  */
      if (!ix86_arch_specified)
      ix86_isa_flags
	|= ((OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_MMX
	     | TARGET_SUBTARGET64_ISA_DEFAULT) & ~ix86_isa_flags_explicit);

      if (TARGET_RTD)
	warning (0, "-mrtd is ignored in 64bit mode");
    }
  else
    {
      target_flags |= TARGET_SUBTARGET32_DEFAULT & ~target_flags_explicit;

      if (!ix86_arch_specified)
      ix86_isa_flags
	|= TARGET_SUBTARGET32_ISA_DEFAULT & ~ix86_isa_flags_explicit;

      /* i386 ABI does not specify red zone.  It still makes sense to use it
         when programmer takes care to stack from being destroyed.  */
      if (!(target_flags_explicit & MASK_NO_RED_ZONE))
        target_flags |= MASK_NO_RED_ZONE;
    }

  /* Keep nonleaf frame pointers.  */
  if (flag_omit_frame_pointer)
    target_flags &= ~MASK_OMIT_LEAF_FRAME_POINTER;
  else if (TARGET_OMIT_LEAF_FRAME_POINTER)
    flag_omit_frame_pointer = 1;

  /* If we're doing fast math, we don't care about comparison order
     wrt NaNs.  This lets us use a shorter comparison sequence.  */
  if (flag_finite_math_only)
    target_flags &= ~MASK_IEEE_FP;

  /* If the architecture always has an FPU, turn off NO_FANCY_MATH_387,
     since the insns won't need emulation.  */
  if (x86_arch_always_fancy_math_387 & ix86_arch_mask)
    target_flags &= ~MASK_NO_FANCY_MATH_387;

  /* Likewise, if the target doesn't have a 387, or we've specified
     software floating point, don't use 387 inline intrinsics.  */
  if (!TARGET_80387)
    target_flags |= MASK_NO_FANCY_MATH_387;

  /* Turn on MMX builtins for -msse.  */
  if (TARGET_SSE)
    {
      ix86_isa_flags |= OPTION_MASK_ISA_MMX & ~ix86_isa_flags_explicit;
      x86_prefetch_sse = true;
    }

  /* Turn on popcnt instruction for -msse4.2 or -mabm.  */
  if (TARGET_SSE4_2 || TARGET_ABM)
    x86_popcnt = true;

  /* Validate -mpreferred-stack-boundary= value, or provide default.
     The default of 128 bits is for Pentium III's SSE __m128.  We can't
     change it because of optimize_size.  Otherwise, we can't mix object
     files compiled with -Os and -On.  */
  ix86_preferred_stack_boundary = 128;
  if (ix86_preferred_stack_boundary_string)
    {
      i = atoi (ix86_preferred_stack_boundary_string);
      if (i < (TARGET_64BIT ? 4 : 2) || i > 12)
	error ("-mpreferred-stack-boundary=%d is not between %d and 12", i,
	       TARGET_64BIT ? 4 : 2);
      else
	ix86_preferred_stack_boundary = (1 << i) * BITS_PER_UNIT;
    }

  /* Accept -msseregparm only if at least SSE support is enabled.  */
  if (TARGET_SSEREGPARM
      && ! TARGET_SSE)
    error ("-msseregparm used without SSE enabled");

  ix86_fpmath = TARGET_FPMATH_DEFAULT;
  if (ix86_fpmath_string != 0)
    {
      if (! strcmp (ix86_fpmath_string, "387"))
	ix86_fpmath = FPMATH_387;
      else if (! strcmp (ix86_fpmath_string, "sse"))
	{
	  if (!TARGET_SSE)
	    {
	      warning (0, "SSE instruction set disabled, using 387 arithmetics");
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
	      warning (0, "SSE instruction set disabled, using 387 arithmetics");
	      ix86_fpmath = FPMATH_387;
	    }
	  else if (!TARGET_80387)
	    {
	      warning (0, "387 instruction set disabled, using SSE arithmetics");
	      ix86_fpmath = FPMATH_SSE;
	    }
	  else
	    ix86_fpmath = (enum fpmath_unit) (FPMATH_SSE | FPMATH_387);
	}
      else
	error ("bad value (%s) for -mfpmath= switch", ix86_fpmath_string);
    }

  /* If the i387 is disabled, then do not return values in it. */
  if (!TARGET_80387)
    target_flags &= ~MASK_FLOAT_RETURNS;

  /* Use external vectorized library in vectorizing intrinsics.  */
  if (ix86_veclibabi_string)
    {
      if (strcmp (ix86_veclibabi_string, "acml") == 0)
	ix86_veclib_handler = ix86_veclibabi_acml;
      else
	error ("unknown vectorization library ABI type (%s) for "
	       "-mveclibabi= switch", ix86_veclibabi_string);
    }

  if ((x86_accumulate_outgoing_args & ix86_tune_mask)
      && !(target_flags_explicit & MASK_ACCUMULATE_OUTGOING_ARGS)
      && !optimize_size)
    target_flags |= MASK_ACCUMULATE_OUTGOING_ARGS;

  /* ??? Unwind info is not correct around the CFG unless either a frame
     pointer is present or M_A_O_A is set.  Fixing this requires rewriting
     unwind info generation to be aware of the CFG and propagating states
     around edges.  */
  if ((flag_unwind_tables || flag_asynchronous_unwind_tables
       || flag_exceptions || flag_non_call_exceptions)
      && flag_omit_frame_pointer
      && !(target_flags & MASK_ACCUMULATE_OUTGOING_ARGS))
    {
      if (target_flags_explicit & MASK_ACCUMULATE_OUTGOING_ARGS)
	warning (0, "unwind tables currently require either a frame pointer "
		 "or -maccumulate-outgoing-args for correctness");
      target_flags |= MASK_ACCUMULATE_OUTGOING_ARGS;
    }

  /* For sane SSE instruction set generation we need fcomi instruction.
     It is safe to enable all CMOVE instructions.  */
  if (TARGET_SSE)
    TARGET_CMOVE = 1;

  /* Figure out what ASM_GENERATE_INTERNAL_LABEL builds as a prefix.  */
  {
    char *p;
    ASM_GENERATE_INTERNAL_LABEL (internal_label_prefix, "LX", 0);
    p = strchr (internal_label_prefix, 'X');
    internal_label_prefix_len = p - internal_label_prefix;
    *p = '\0';
  }

  /* When scheduling description is not available, disable scheduler pass
     so it won't slow down the compilation and make x87 code slower.  */
  if (!TARGET_SCHEDULE)
    flag_schedule_insns_after_reload = flag_schedule_insns = 0;

  if (!PARAM_SET_P (PARAM_SIMULTANEOUS_PREFETCHES))
    set_param_value ("simultaneous-prefetches",
		     ix86_cost->simultaneous_prefetches);
  if (!PARAM_SET_P (PARAM_L1_CACHE_LINE_SIZE))
    set_param_value ("l1-cache-line-size", ix86_cost->prefetch_block);
  if (!PARAM_SET_P (PARAM_L1_CACHE_SIZE))
    set_param_value ("l1-cache-size", ix86_cost->l1_cache_size);
  if (!PARAM_SET_P (PARAM_L2_CACHE_SIZE))
    set_param_value ("l2-cache-size", ix86_cost->l2_cache_size);

  /* If using typedef char *va_list, signal that __builtin_va_start (&ap, 0)
     can be optimized to ap = __builtin_next_arg (0).  */
  if (!TARGET_64BIT || TARGET_64BIT_MS_ABI)
    targetm.expand_builtin_va_start = NULL;

#ifdef USE_IX86_CLD
  /* Use -mcld by default for 32-bit code if configured with --enable-cld.  */
  if (!TARGET_64BIT)
    target_flags |= MASK_CLD & ~target_flags_explicit;
#endif
}

/* Return true if this goes in large data/bss.  */

static bool
ix86_in_large_data_p (tree exp)
{
  if (ix86_cmodel != CM_MEDIUM && ix86_cmodel != CM_MEDIUM_PIC)
    return false;

  /* Functions are never large data.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (exp) == VAR_DECL && DECL_SECTION_NAME (exp))
    {
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (exp));
      if (strcmp (section, ".ldata") == 0
	  || strcmp (section, ".lbss") == 0)
	return true;
      return false;
    }
  else
    {
      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

      /* If this is an incomplete type with size 0, then we can't put it
	 in data because it might be too big when completed.  */
      if (!size || size > ix86_section_threshold)
	return true;
    }

  return false;
}

/* Switch to the appropriate section for output of DECL.
   DECL is either a `VAR_DECL' node or a constant of some sort.
   RELOC indicates whether forming the initial value of DECL requires
   link-time relocations.  */

static section * x86_64_elf_select_section (tree, int, unsigned HOST_WIDE_INT)
	ATTRIBUTE_UNUSED;

static section *
x86_64_elf_select_section (tree decl, int reloc,
			   unsigned HOST_WIDE_INT align)
{
  if ((ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_MEDIUM_PIC)
      && ix86_in_large_data_p (decl))
    {
      const char *sname = NULL;
      unsigned int flags = SECTION_WRITE;
      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_DATA:
	  sname = ".ldata";
	  break;
	case SECCAT_DATA_REL:
	  sname = ".ldata.rel";
	  break;
	case SECCAT_DATA_REL_LOCAL:
	  sname = ".ldata.rel.local";
	  break;
	case SECCAT_DATA_REL_RO:
	  sname = ".ldata.rel.ro";
	  break;
	case SECCAT_DATA_REL_RO_LOCAL:
	  sname = ".ldata.rel.ro.local";
	  break;
	case SECCAT_BSS:
	  sname = ".lbss";
	  flags |= SECTION_BSS;
	  break;
	case SECCAT_RODATA:
	case SECCAT_RODATA_MERGE_STR:
	case SECCAT_RODATA_MERGE_STR_INIT:
	case SECCAT_RODATA_MERGE_CONST:
	  sname = ".lrodata";
	  flags = 0;
	  break;
	case SECCAT_SRODATA:
	case SECCAT_SDATA:
	case SECCAT_SBSS:
	  gcc_unreachable ();
	case SECCAT_TEXT:
	case SECCAT_TDATA:
	case SECCAT_TBSS:
	  /* We don't split these for medium model.  Place them into
	     default sections and hope for best.  */
	  break;
	}
      if (sname)
	{
	  /* We might get called with string constants, but get_named_section
	     doesn't like them as they are not DECLs.  Also, we need to set
	     flags in that case.  */
	  if (!DECL_P (decl))
	    return get_section (sname, flags, NULL);
	  return get_named_section (decl, sname, reloc);
	}
    }
  return default_elf_select_section (decl, reloc, align);
}

/* Build up a unique section name, expressed as a
   STRING_CST node, and assign it to DECL_SECTION_NAME (decl).
   RELOC indicates whether the initial value of EXP requires
   link-time relocations.  */

static void ATTRIBUTE_UNUSED
x86_64_elf_unique_section (tree decl, int reloc)
{
  if ((ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_MEDIUM_PIC)
      && ix86_in_large_data_p (decl))
    {
      const char *prefix = NULL;
      /* We only need to use .gnu.linkonce if we don't have COMDAT groups.  */
      bool one_only = DECL_ONE_ONLY (decl) && !HAVE_COMDAT_GROUP;

      switch (categorize_decl_for_section (decl, reloc))
	{
	case SECCAT_DATA:
	case SECCAT_DATA_REL:
	case SECCAT_DATA_REL_LOCAL:
	case SECCAT_DATA_REL_RO:
	case SECCAT_DATA_REL_RO_LOCAL:
          prefix = one_only ? ".gnu.linkonce.ld." : ".ldata.";
	  break;
	case SECCAT_BSS:
          prefix = one_only ? ".gnu.linkonce.lb." : ".lbss.";
	  break;
	case SECCAT_RODATA:
	case SECCAT_RODATA_MERGE_STR:
	case SECCAT_RODATA_MERGE_STR_INIT:
	case SECCAT_RODATA_MERGE_CONST:
          prefix = one_only ? ".gnu.linkonce.lr." : ".lrodata.";
	  break;
	case SECCAT_SRODATA:
	case SECCAT_SDATA:
	case SECCAT_SBSS:
	  gcc_unreachable ();
	case SECCAT_TEXT:
	case SECCAT_TDATA:
	case SECCAT_TBSS:
	  /* We don't split these for medium model.  Place them into
	     default sections and hope for best.  */
	  break;
	}
      if (prefix)
	{
	  const char *name;
	  size_t nlen, plen;
	  char *string;
	  plen = strlen (prefix);

	  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
	  name = targetm.strip_name_encoding (name);
	  nlen = strlen (name);

	  string = (char *) alloca (nlen + plen + 1);
	  memcpy (string, prefix, plen);
	  memcpy (string + plen, name, nlen + 1);

	  DECL_SECTION_NAME (decl) = build_string (nlen + plen, string);
	  return;
	}
    }
  default_unique_section (decl, reloc);
}

#ifdef COMMON_ASM_OP
/* This says how to output assembler code to declare an
   uninitialized external linkage data object.

   For medium model x86-64 we need to use .largecomm opcode for
   large objects.  */
void
x86_elf_aligned_common (FILE *file,
			const char *name, unsigned HOST_WIDE_INT size,
			int align)
{
  if ((ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_MEDIUM_PIC)
      && size > (unsigned int)ix86_section_threshold)
    fprintf (file, ".largecomm\t");
  else
    fprintf (file, "%s", COMMON_ASM_OP);
  assemble_name (file, name);
  fprintf (file, ","HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",
	   size, align / BITS_PER_UNIT);
}
#endif

/* Utility function for targets to use in implementing
   ASM_OUTPUT_ALIGNED_BSS.  */

void
x86_output_aligned_bss (FILE *file, tree decl ATTRIBUTE_UNUSED,
			const char *name, unsigned HOST_WIDE_INT size,
			int align)
{
  if ((ix86_cmodel == CM_MEDIUM || ix86_cmodel == CM_MEDIUM_PIC)
      && size > (unsigned int)ix86_section_threshold)
    switch_to_section (get_named_section (decl, ".lbss", 0));
  else
    switch_to_section (bss_section);
  ASM_OUTPUT_ALIGN (file, floor_log2 (align / BITS_PER_UNIT));
#ifdef ASM_DECLARE_OBJECT_NAME
  last_assemble_variable_decl = decl;
  ASM_DECLARE_OBJECT_NAME (file, name, decl);
#else
  /* Standard thing is just output label for the object.  */
  ASM_OUTPUT_LABEL (file, name);
#endif /* ASM_DECLARE_OBJECT_NAME */
  ASM_OUTPUT_SKIP (file, size ? size : 1);
}

void
optimization_options (int level, int size ATTRIBUTE_UNUSED)
{
  /* For -O2 and beyond, turn off -fschedule-insns by default.  It tends to
     make the problem with not enough registers even worse.  */
#ifdef INSN_SCHEDULING
  if (level > 1)
    flag_schedule_insns = 0;
#endif

  if (TARGET_MACHO)
    /* The Darwin libraries never set errno, so we might as well
       avoid calling them when that's the only reason we would.  */
    flag_errno_math = 0;

  /* The default values of these switches depend on the TARGET_64BIT
     that is not known at this moment.  Mark these values with 2 and
     let user the to override these.  In case there is no command line option
     specifying them, we will set the defaults in override_options.  */
  if (optimize >= 1)
    flag_omit_frame_pointer = 2;
  flag_pcc_struct_return = 2;
  flag_asynchronous_unwind_tables = 2;
  flag_vect_cost_model = 1;
#ifdef SUBTARGET_OPTIMIZATION_OPTIONS
  SUBTARGET_OPTIMIZATION_OPTIONS;
#endif
}

/* Decide whether we can make a sibling call to a function.  DECL is the
   declaration of the function being targeted by the call and EXP is the
   CALL_EXPR representing the call.  */

static bool
ix86_function_ok_for_sibcall (tree decl, tree exp)
{
  tree func;
  rtx a, b;

  /* If we are generating position-independent code, we cannot sibcall
     optimize any indirect call, or a direct call to a global function,
     as the PLT requires %ebx be live.  */
  if (!TARGET_64BIT && flag_pic && (!decl || !targetm.binds_local_p (decl)))
    return false;

  if (decl)
    func = decl;
  else
    {
      func = TREE_TYPE (CALL_EXPR_FN (exp));
      if (POINTER_TYPE_P (func))
        func = TREE_TYPE (func);
    }

  /* Check that the return value locations are the same.  Like
     if we are returning floats on the 80387 register stack, we cannot
     make a sibcall from a function that doesn't return a float to a
     function that does or, conversely, from a function that does return
     a float to a function that doesn't; the necessary stack adjustment
     would not be executed.  This is also the place we notice
     differences in the return value ABI.  Note that it is ok for one
     of the functions to have void return type as long as the return
     value of the other is passed in a register.  */
  a = ix86_function_value (TREE_TYPE (exp), func, false);
  b = ix86_function_value (TREE_TYPE (DECL_RESULT (cfun->decl)),
			   cfun->decl, false);
  if (STACK_REG_P (a) || STACK_REG_P (b))
    {
      if (!rtx_equal_p (a, b))
	return false;
    }
  else if (VOID_TYPE_P (TREE_TYPE (DECL_RESULT (cfun->decl))))
    ;
  else if (!rtx_equal_p (a, b))
    return false;

  /* If this call is indirect, we'll need to be able to use a call-clobbered
     register for the address of the target function.  Make sure that all
     such registers are not used for passing parameters.  */
  if (!decl && !TARGET_64BIT)
    {
      tree type;

      /* We're looking at the CALL_EXPR, we need the type of the function.  */
      type = CALL_EXPR_FN (exp);		/* pointer expression */
      type = TREE_TYPE (type);			/* pointer type */
      type = TREE_TYPE (type);			/* function type */

      if (ix86_function_regparm (type, NULL) >= 3)
	{
	  /* ??? Need to count the actual number of registers to be used,
	     not the possible number of registers.  Fix later.  */
	  return false;
	}
    }

  /* Dllimport'd functions are also called indirectly.  */
  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
      && decl && DECL_DLLIMPORT_P (decl)
      && ix86_function_regparm (TREE_TYPE (decl), NULL) >= 3)
    return false;

  /* If we forced aligned the stack, then sibcalling would unalign the
     stack, which may break the called function.  */
  if (cfun->machine->force_align_arg_pointer)
    return false;

  /* Otherwise okay.  That also includes certain types of indirect calls.  */
  return true;
}

/* Handle "cdecl", "stdcall", "fastcall", "regparm" and "sseregparm"
   calling convention attributes;
   arguments as in struct attribute_spec.handler.  */

static tree
ix86_handle_cconv_attribute (tree *node, tree name,
				   tree args,
				   int flags ATTRIBUTE_UNUSED,
				   bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != METHOD_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qs attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Can combine regparm with all attributes but fastcall.  */
  if (is_attribute_p ("regparm", name))
    {
      tree cst;

      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and regparm attributes are not compatible");
	}

      cst = TREE_VALUE (args);
      if (TREE_CODE (cst) != INTEGER_CST)
	{
	  warning (OPT_Wattributes,
		   "%qs attribute requires an integer constant argument",
		   IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
      else if (compare_tree_int (cst, REGPARM_MAX) > 0)
	{
	  warning (OPT_Wattributes, "argument to %qs attribute larger than %d",
		   IDENTIFIER_POINTER (name), REGPARM_MAX);
	  *no_add_attrs = true;
	}

      if (!TARGET_64BIT
	  && lookup_attribute (ix86_force_align_arg_pointer_string,
			       TYPE_ATTRIBUTES (*node))
	  && compare_tree_int (cst, REGPARM_MAX-1))
	{
	  error ("%s functions limited to %d register parameters",
		 ix86_force_align_arg_pointer_string, REGPARM_MAX-1);
	}

      return NULL_TREE;
    }

  if (TARGET_64BIT)
    {
      /* Do not warn when emulating the MS ABI.  */
      if (!TARGET_64BIT_MS_ABI)
	warning (OPT_Wattributes, "%qs attribute ignored",
	         IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Can combine fastcall with stdcall (redundant) and sseregparm.  */
  if (is_attribute_p ("fastcall", name))
    {
      if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and stdcall attributes are not compatible");
	}
      if (lookup_attribute ("regparm", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and regparm attributes are not compatible");
	}
    }

  /* Can combine stdcall with fastcall (redundant), regparm and
     sseregparm.  */
  else if (is_attribute_p ("stdcall", name))
    {
      if (lookup_attribute ("cdecl", TYPE_ATTRIBUTES (*node)))
        {
	  error ("stdcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("stdcall and fastcall attributes are not compatible");
	}
    }

  /* Can combine cdecl with regparm and sseregparm.  */
  else if (is_attribute_p ("cdecl", name))
    {
      if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("stdcall and cdecl attributes are not compatible");
	}
      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (*node)))
        {
	  error ("fastcall and cdecl attributes are not compatible");
	}
    }

  /* Can combine sseregparm with all attributes.  */

  return NULL_TREE;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

static int
ix86_comp_type_attributes (const_tree type1, const_tree type2)
{
  /* Check for mismatch of non-default calling convention.  */
  const char *const rtdstr = TARGET_RTD ? "cdecl" : "stdcall";

  if (TREE_CODE (type1) != FUNCTION_TYPE
      && TREE_CODE (type1) != METHOD_TYPE)
    return 1;

  /* Check for mismatched fastcall/regparm types.  */
  if ((!lookup_attribute ("fastcall", TYPE_ATTRIBUTES (type1))
       != !lookup_attribute ("fastcall", TYPE_ATTRIBUTES (type2)))
      || (ix86_function_regparm (type1, NULL)
	  != ix86_function_regparm (type2, NULL)))
    return 0;

  /* Check for mismatched sseregparm types.  */
  if (!lookup_attribute ("sseregparm", TYPE_ATTRIBUTES (type1))
      != !lookup_attribute ("sseregparm", TYPE_ATTRIBUTES (type2)))
    return 0;

  /* Check for mismatched return types (cdecl vs stdcall).  */
  if (!lookup_attribute (rtdstr, TYPE_ATTRIBUTES (type1))
      != !lookup_attribute (rtdstr, TYPE_ATTRIBUTES (type2)))
    return 0;

  return 1;
}

/* Return the regparm value for a function with the indicated TYPE and DECL.
   DECL may be NULL when calling function indirectly
   or considering a libcall.  */

static int
ix86_function_regparm (const_tree type, const_tree decl)
{
  tree attr;
  int regparm = ix86_regparm;

  if (TARGET_64BIT)
    return regparm;

  attr = lookup_attribute ("regparm", TYPE_ATTRIBUTES (type));
  if (attr)
    return TREE_INT_CST_LOW (TREE_VALUE (TREE_VALUE (attr)));

  if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (type)))
    return 2;

  /* Use register calling convention for local functions when possible.  */
  if (decl && TREE_CODE (decl) == FUNCTION_DECL
      && flag_unit_at_a_time && !profile_flag)
    {
      /* FIXME: remove this CONST_CAST when cgraph.[ch] is constified.  */
      struct cgraph_local_info *i = cgraph_local_info (CONST_CAST_TREE(decl));
      if (i && i->local)
	{
	  int local_regparm, globals = 0, regno;
	  struct function *f;

	  /* Make sure no regparm register is taken by a
	     fixed register variable.  */
	  for (local_regparm = 0; local_regparm < REGPARM_MAX; local_regparm++)
	    if (fixed_regs[local_regparm])
	      break;

	  /* We can't use regparm(3) for nested functions as these use
	     static chain pointer in third argument.  */
	  if (local_regparm == 3
	      && (decl_function_context (decl)
                  || ix86_force_align_arg_pointer)
	      && !DECL_NO_STATIC_CHAIN (decl))
	    local_regparm = 2;

	  /* If the function realigns its stackpointer, the prologue will
	     clobber %ecx.  If we've already generated code for the callee,
	     the callee DECL_STRUCT_FUNCTION is gone, so we fall back to
	     scanning the attributes for the self-realigning property.  */
	  f = DECL_STRUCT_FUNCTION (decl);
	  if (local_regparm == 3
	      && (f ? !!f->machine->force_align_arg_pointer
		  : !!lookup_attribute (ix86_force_align_arg_pointer_string,
					TYPE_ATTRIBUTES (TREE_TYPE (decl)))))
	    local_regparm = 2;

	  /* Each fixed register usage increases register pressure,
	     so less registers should be used for argument passing.
	     This functionality can be overriden by an explicit
	     regparm value.  */
	  for (regno = 0; regno <= DI_REG; regno++)
	    if (fixed_regs[regno])
	      globals++;

	  local_regparm
	    = globals < local_regparm ? local_regparm - globals : 0;

	  if (local_regparm > regparm)
	    regparm = local_regparm;
	}
    }

  return regparm;
}

/* Return 1 or 2, if we can pass up to SSE_REGPARM_MAX SFmode (1) and
   DFmode (2) arguments in SSE registers for a function with the
   indicated TYPE and DECL.  DECL may be NULL when calling function
   indirectly or considering a libcall.  Otherwise return 0.  */

static int
ix86_function_sseregparm (const_tree type, const_tree decl, bool warn)
{
  gcc_assert (!TARGET_64BIT);

  /* Use SSE registers to pass SFmode and DFmode arguments if requested
     by the sseregparm attribute.  */
  if (TARGET_SSEREGPARM
      || (type && lookup_attribute ("sseregparm", TYPE_ATTRIBUTES (type))))
    {
      if (!TARGET_SSE)
	{
	  if (warn)
	    {
	      if (decl)
		error ("Calling %qD with attribute sseregparm without "
		       "SSE/SSE2 enabled", decl);
	      else
		error ("Calling %qT with attribute sseregparm without "
		       "SSE/SSE2 enabled", type);
	    }
	  return 0;
	}

      return 2;
    }

  /* For local functions, pass up to SSE_REGPARM_MAX SFmode
     (and DFmode for SSE2) arguments in SSE registers.  */
  if (decl && TARGET_SSE_MATH && flag_unit_at_a_time && !profile_flag)
    {
      /* FIXME: remove this CONST_CAST when cgraph.[ch] is constified.  */
      struct cgraph_local_info *i = cgraph_local_info (CONST_CAST_TREE(decl));
      if (i && i->local)
	return TARGET_SSE2 ? 2 : 1;
    }

  return 0;
}

/* Return true if EAX is live at the start of the function.  Used by
   ix86_expand_prologue to determine if we need special help before
   calling allocate_stack_worker.  */

static bool
ix86_eax_live_at_start_p (void)
{
  /* Cheat.  Don't bother working forward from ix86_function_regparm
     to the function type to whether an actual argument is located in
     eax.  Instead just look at cfg info, which is still close enough
     to correct at this point.  This gives false positives for broken
     functions that might use uninitialized data that happens to be
     allocated in eax, but who cares?  */
  return REGNO_REG_SET_P (df_get_live_out (ENTRY_BLOCK_PTR), 0);
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
ix86_return_pops_args (tree fundecl, tree funtype, int size)
{
  int rtd;

  /* None of the 64-bit ABIs pop arguments.  */
  if (TARGET_64BIT)
    return 0;

  rtd = TARGET_RTD && (!fundecl || TREE_CODE (fundecl) != IDENTIFIER_NODE);

  /* Cdecl functions override -mrtd, and never pop the stack.  */
  if (! lookup_attribute ("cdecl", TYPE_ATTRIBUTES (funtype)))
    {
      /* Stdcall and fastcall functions will pop the stack if not
         variable args.  */
      if (lookup_attribute ("stdcall", TYPE_ATTRIBUTES (funtype))
          || lookup_attribute ("fastcall", TYPE_ATTRIBUTES (funtype)))
	rtd = 1;

      if (rtd && ! stdarg_p (funtype))
	return size;
    }

  /* Lose any fake structure return argument if it is passed on the stack.  */
  if (aggregate_value_p (TREE_TYPE (funtype), fundecl)
      && !KEEP_AGGREGATE_RETURN_POINTER)
    {
      int nregs = ix86_function_regparm (funtype, fundecl);
      if (nregs == 0)
	return GET_MODE_SIZE (Pmode);
    }

  return 0;
}

/* Argument support functions.  */

/* Return true when register may be used to pass function parameters.  */
bool
ix86_function_arg_regno_p (int regno)
{
  int i;
  const int *parm_regs;

  if (!TARGET_64BIT)
    {
      if (TARGET_MACHO)
        return (regno < REGPARM_MAX
                || (TARGET_SSE && SSE_REGNO_P (regno) && !fixed_regs[regno]));
      else
        return (regno < REGPARM_MAX
	        || (TARGET_MMX && MMX_REGNO_P (regno)
	  	    && (regno < FIRST_MMX_REG + MMX_REGPARM_MAX))
	        || (TARGET_SSE && SSE_REGNO_P (regno)
		    && (regno < FIRST_SSE_REG + SSE_REGPARM_MAX)));
    }

  if (TARGET_MACHO)
    {
      if (SSE_REGNO_P (regno) && TARGET_SSE)
        return true;
    }
  else
    {
      if (TARGET_SSE && SSE_REGNO_P (regno)
          && (regno < FIRST_SSE_REG + SSE_REGPARM_MAX))
        return true;
    }

  /* RAX is used as hidden argument to va_arg functions.  */
  if (!TARGET_64BIT_MS_ABI && regno == AX_REG)
    return true;

  if (TARGET_64BIT_MS_ABI)
    parm_regs = x86_64_ms_abi_int_parameter_registers;
  else
    parm_regs = x86_64_int_parameter_registers;
  for (i = 0; i < REGPARM_MAX; i++)
    if (regno == parm_regs[i])
      return true;
  return false;
}

/* Return if we do not know how to pass TYPE solely in registers.  */

static bool
ix86_must_pass_in_stack (enum machine_mode mode, const_tree type)
{
  if (must_pass_in_stack_var_size_or_pad (mode, type))
    return true;

  /* For 32-bit, we want TImode aggregates to go on the stack.  But watch out!
     The layout_type routine is crafty and tries to trick us into passing
     currently unsupported vector types on the stack by using TImode.  */
  return (!TARGET_64BIT && mode == TImode
	  && type && TREE_CODE (type) != VECTOR_TYPE);
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
init_cumulative_args (CUMULATIVE_ARGS *cum,  /* Argument info to initialize */
		      tree fntype,	/* tree ptr for function decl */
		      rtx libname,	/* SYMBOL_REF of library name or 0 */
		      tree fndecl)
{
  struct cgraph_local_info *i = fndecl ? cgraph_local_info (fndecl) : NULL;
  memset (cum, 0, sizeof (*cum));

  /* Set up the number of registers to use for passing arguments.  */
  cum->nregs = ix86_regparm;
  if (TARGET_SSE)
    cum->sse_nregs = SSE_REGPARM_MAX;
  if (TARGET_MMX)
    cum->mmx_nregs = MMX_REGPARM_MAX;
  cum->warn_sse = true;
  cum->warn_mmx = true;

  /* Because type might mismatch in between caller and callee, we need to
     use actual type of function for local calls.
     FIXME: cgraph_analyze can be told to actually record if function uses
     va_start so for local functions maybe_vaarg can be made aggressive
     helping K&R code.
     FIXME: once typesytem is fixed, we won't need this code anymore.  */
  if (i && i->local)
    fntype = TREE_TYPE (fndecl);
  cum->maybe_vaarg = (fntype
		      ? (!prototype_p (fntype) || stdarg_p (fntype))
		      : !libname);

  if (!TARGET_64BIT)
    {
      /* If there are variable arguments, then we won't pass anything
         in registers in 32-bit mode. */
      if (stdarg_p (fntype))
	{
	  cum->nregs = 0;
	  cum->sse_nregs = 0;
	  cum->mmx_nregs = 0;
	  cum->warn_sse = 0;
	  cum->warn_mmx = 0;
	  return;
	}

      /* Use ecx and edx registers if function has fastcall attribute,
	 else look for regparm information.  */
      if (fntype)
	{
	  if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (fntype)))
	    {
	      cum->nregs = 2;
	      cum->fastcall = 1;
	    }
	  else
	    cum->nregs = ix86_function_regparm (fntype, fndecl);
	}

      /* Set up the number of SSE registers used for passing SFmode
	 and DFmode arguments.  Warn for mismatching ABI.  */
      cum->float_in_sse = ix86_function_sseregparm (fntype, fndecl, true);
    }
}

/* Return the "natural" mode for TYPE.  In most cases, this is just TYPE_MODE.
   But in the case of vector types, it is some vector mode.

   When we have only some of our vector isa extensions enabled, then there
   are some modes for which vector_mode_supported_p is false.  For these
   modes, the generic vector support in gcc will choose some non-vector mode
   in order to implement the type.  By computing the natural mode, we'll
   select the proper ABI location for the operand and not depend on whatever
   the middle-end decides to do with these vector types.  */

static enum machine_mode
type_natural_mode (const_tree type)
{
  enum machine_mode mode = TYPE_MODE (type);

  if (TREE_CODE (type) == VECTOR_TYPE && !VECTOR_MODE_P (mode))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      if ((size == 8 || size == 16)
	  /* ??? Generic code allows us to create width 1 vectors.  Ignore.  */
	  && TYPE_VECTOR_SUBPARTS (type) > 1)
	{
	  enum machine_mode innermode = TYPE_MODE (TREE_TYPE (type));

	  if (TREE_CODE (TREE_TYPE (type)) == REAL_TYPE)
	    mode = MIN_MODE_VECTOR_FLOAT;
	  else
	    mode = MIN_MODE_VECTOR_INT;

	  /* Get the mode which has this inner mode and number of units.  */
	  for (; mode != VOIDmode; mode = GET_MODE_WIDER_MODE (mode))
	    if (GET_MODE_NUNITS (mode) == TYPE_VECTOR_SUBPARTS (type)
		&& GET_MODE_INNER (mode) == innermode)
	      return mode;

	  gcc_unreachable ();
	}
    }

  return mode;
}

/* We want to pass a value in REGNO whose "natural" mode is MODE.  However,
   this may not agree with the mode that the type system has chosen for the
   register, which is ORIG_MODE.  If ORIG_MODE is not BLKmode, then we can
   go ahead and use it.  Otherwise we have to build a PARALLEL instead.  */

static rtx
gen_reg_or_parallel (enum machine_mode mode, enum machine_mode orig_mode,
		     unsigned int regno)
{
  rtx tmp;

  if (orig_mode != BLKmode)
    tmp = gen_rtx_REG (orig_mode, regno);
  else
    {
      tmp = gen_rtx_REG (mode, regno);
      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp, const0_rtx);
      tmp = gen_rtx_PARALLEL (orig_mode, gen_rtvec (1, tmp));
    }

  return tmp;
}

/* x86-64 register passing implementation.  See x86-64 ABI for details.  Goal
   of this code is to classify each 8bytes of incoming argument by the register
   class and assign registers accordingly.  */

/* Return the union class of CLASS1 and CLASS2.
   See the x86-64 PS ABI for details.  */

static enum x86_64_reg_class
merge_classes (enum x86_64_reg_class class1, enum x86_64_reg_class class2)
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

  /* Rule #5: If one of the classes is X87, X87UP, or COMPLEX_X87 class,
     MEMORY is used.  */
  if (class1 == X86_64_X87_CLASS
      || class1 == X86_64_X87UP_CLASS
      || class1 == X86_64_COMPLEX_X87_CLASS
      || class2 == X86_64_X87_CLASS
      || class2 == X86_64_X87UP_CLASS
      || class2 == X86_64_COMPLEX_X87_CLASS)
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
classify_argument (enum machine_mode mode, const_tree type,
		   enum x86_64_reg_class classes[MAX_CLASSES], int bit_offset)
{
  HOST_WIDE_INT bytes =
    (mode == BLKmode) ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  int words = (bytes + (bit_offset % 64) / 8 + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* Variable sized entities are always passed/returned in memory.  */
  if (bytes < 0)
    return 0;

  if (mode != VOIDmode
      && targetm.calls.must_pass_in_stack (mode, type))
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
      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	  /* And now merge the fields of structure.  */
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  int num;

		  if (TREE_TYPE (field) == error_mark_node)
		    continue;

		  /* Bitfields are always classified as integer.  Handle them
		     early, since later code would consider them to be
		     misaligned integers.  */
		  if (DECL_BIT_FIELD (field))
		    {
		      for (i = (int_bit_position (field) + (bit_offset % 64)) / 8 / 8;
			   i < ((int_bit_position (field) + (bit_offset % 64))
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
	  break;

	case ARRAY_TYPE:
	  /* Arrays are handled as small records.  */
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

	    break;
	  }
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  /* Unions are similar to RECORD_TYPE but offset is always 0.
	     */
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    {
	      if (TREE_CODE (field) == FIELD_DECL)
		{
		  int num;

		  if (TREE_TYPE (field) == error_mark_node)
		    continue;

		  num = classify_argument (TYPE_MODE (TREE_TYPE (field)),
					   TREE_TYPE (field), subclasses,
					   bit_offset);
		  if (!num)
		    return 0;
		  for (i = 0; i < num; i++)
		    classes[i] = merge_classes (subclasses[i], classes[i]);
		}
	    }
	  break;

	default:
	  gcc_unreachable ();
	}

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
      if (COMPLEX_MODE_P (mode))
	mode_alignment /= 2;
      /* Misaligned fields are always returned in memory.  */
      if (bit_offset % mode_alignment)
	return 0;
    }

  /* for V1xx modes, just use the base mode */
  if (VECTOR_MODE_P (mode)
      && GET_MODE_SIZE (GET_MODE_INNER (mode)) == bytes)
    mode = GET_MODE_INNER (mode);

  /* Classification of atomic types.  */
  switch (mode)
    {
    case SDmode:
    case DDmode:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case TDmode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
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
      return 0;
    case SFmode:
      if (!(bit_offset % 64))
	classes[0] = X86_64_SSESF_CLASS;
      else
	classes[0] = X86_64_SSE_CLASS;
      return 1;
    case DFmode:
      classes[0] = X86_64_SSEDF_CLASS;
      return 1;
    case XFmode:
      classes[0] = X86_64_X87_CLASS;
      classes[1] = X86_64_X87UP_CLASS;
      return 2;
    case TFmode:
      classes[0] = X86_64_SSE_CLASS;
      classes[1] = X86_64_SSEUP_CLASS;
      return 2;
    case SCmode:
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case DCmode:
      classes[0] = X86_64_SSEDF_CLASS;
      classes[1] = X86_64_SSEDF_CLASS;
      return 2;
    case XCmode:
      classes[0] = X86_64_COMPLEX_X87_CLASS;
      return 1;
    case TCmode:
      /* This modes is larger than 16 bytes.  */
      return 0;
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
      classes[0] = X86_64_SSE_CLASS;
      return 1;
    case BLKmode:
    case VOIDmode:
      return 0;
    default:
      gcc_assert (VECTOR_MODE_P (mode));

      if (bytes > 16)
	return 0;

      gcc_assert (GET_MODE_CLASS (GET_MODE_INNER (mode)) == MODE_INT);

      if (bit_offset + GET_MODE_BITSIZE (mode) <= 32)
	classes[0] = X86_64_INTEGERSI_CLASS;
      else
	classes[0] = X86_64_INTEGER_CLASS;
      classes[1] = X86_64_INTEGER_CLASS;
      return 1 + (bytes > 8);
    }
}

/* Examine the argument and return set number of register required in each
   class.  Return 0 iff parameter should be passed in memory.  */
static int
examine_argument (enum machine_mode mode, const_tree type, int in_return,
		  int *int_nregs, int *sse_nregs)
{
  enum x86_64_reg_class regclass[MAX_CLASSES];
  int n = classify_argument (mode, type, regclass, 0);

  *int_nregs = 0;
  *sse_nregs = 0;
  if (!n)
    return 0;
  for (n--; n >= 0; n--)
    switch (regclass[n])
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
      case X86_64_COMPLEX_X87_CLASS:
	return in_return ? 2 : 0;
      case X86_64_MEMORY_CLASS:
	gcc_unreachable ();
      }
  return 1;
}

/* Construct container for the argument used by GCC interface.  See
   FUNCTION_ARG for the detailed description.  */

static rtx
construct_container (enum machine_mode mode, enum machine_mode orig_mode,
		     const_tree type, int in_return, int nintregs, int nsseregs,
		     const int *intreg, int sse_regno)
{
  /* The following variables hold the static issued_error state.  */
  static bool issued_sse_arg_error;
  static bool issued_sse_ret_error;
  static bool issued_x87_ret_error;

  enum machine_mode tmpmode;
  int bytes =
    (mode == BLKmode) ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);
  enum x86_64_reg_class regclass[MAX_CLASSES];
  int n;
  int i;
  int nexps = 0;
  int needed_sseregs, needed_intregs;
  rtx exp[MAX_CLASSES];
  rtx ret;

  n = classify_argument (mode, type, regclass, 0);
  if (!n)
    return NULL;
  if (!examine_argument (mode, type, in_return, &needed_intregs,
			 &needed_sseregs))
    return NULL;
  if (needed_intregs > nintregs || needed_sseregs > nsseregs)
    return NULL;

  /* We allowed the user to turn off SSE for kernel mode.  Don't crash if
     some less clueful developer tries to use floating-point anyway.  */
  if (needed_sseregs && !TARGET_SSE)
    {
      if (in_return)
	{
	  if (!issued_sse_ret_error)
	    {
	      error ("SSE register return with SSE disabled");
	      issued_sse_ret_error = true;
	    }
	}
      else if (!issued_sse_arg_error)
	{
	  error ("SSE register argument with SSE disabled");
	  issued_sse_arg_error = true;
	}
      return NULL;
    }

  /* Likewise, error if the ABI requires us to return values in the
     x87 registers and the user specified -mno-80387.  */
  if (!TARGET_80387 && in_return)
    for (i = 0; i < n; i++)
      if (regclass[i] == X86_64_X87_CLASS
	  || regclass[i] == X86_64_X87UP_CLASS
	  || regclass[i] == X86_64_COMPLEX_X87_CLASS)
	{
	  if (!issued_x87_ret_error)
	    {
	      error ("x87 register return with x87 disabled");
	      issued_x87_ret_error = true;
	    }
	  return NULL;
	}

  /* First construct simple cases.  Avoid SCmode, since we want to use
     single register to pass this type.  */
  if (n == 1 && mode != SCmode)
    switch (regclass[0])
      {
      case X86_64_INTEGER_CLASS:
      case X86_64_INTEGERSI_CLASS:
	return gen_rtx_REG (mode, intreg[0]);
      case X86_64_SSE_CLASS:
      case X86_64_SSESF_CLASS:
      case X86_64_SSEDF_CLASS:
	return gen_reg_or_parallel (mode, orig_mode, SSE_REGNO (sse_regno));
      case X86_64_X87_CLASS:
      case X86_64_COMPLEX_X87_CLASS:
	return gen_rtx_REG (mode, FIRST_STACK_REG);
      case X86_64_NO_CLASS:
	/* Zero sized array, struct or class.  */
	return NULL;
      default:
	gcc_unreachable ();
      }
  if (n == 2 && regclass[0] == X86_64_SSE_CLASS
      && regclass[1] == X86_64_SSEUP_CLASS && mode != BLKmode)
    return gen_rtx_REG (mode, SSE_REGNO (sse_regno));

  if (n == 2
      && regclass[0] == X86_64_X87_CLASS && regclass[1] == X86_64_X87UP_CLASS)
    return gen_rtx_REG (XFmode, FIRST_STACK_REG);
  if (n == 2 && regclass[0] == X86_64_INTEGER_CLASS
      && regclass[1] == X86_64_INTEGER_CLASS
      && (mode == CDImode || mode == TImode || mode == TFmode)
      && intreg[0] + 1 == intreg[1])
    return gen_rtx_REG (mode, intreg[0]);

  /* Otherwise figure out the entries of the PARALLEL.  */
  for (i = 0; i < n; i++)
    {
      switch (regclass[i])
        {
	  case X86_64_NO_CLASS:
	    break;
	  case X86_64_INTEGER_CLASS:
	  case X86_64_INTEGERSI_CLASS:
	    /* Merge TImodes on aligned occasions here too.  */
	    if (i * 8 + 8 > bytes)
	      tmpmode = mode_for_size ((bytes - i * 8) * BITS_PER_UNIT, MODE_INT, 0);
	    else if (regclass[i] == X86_64_INTEGERSI_CLASS)
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
	    if (i < n - 1 && regclass[i + 1] == X86_64_SSEUP_CLASS)
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
	    gcc_unreachable ();
	}
    }

  /* Empty aligned struct, union or class.  */
  if (nexps == 0)
    return NULL;

  ret =  gen_rtx_PARALLEL (mode, rtvec_alloc (nexps));
  for (i = 0; i < nexps; i++)
    XVECEXP (ret, 0, i) = exp [i];
  return ret;
}

/* Update the data in CUM to advance over an argument of mode MODE
   and data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.)  */

static void
function_arg_advance_32 (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			 tree type, HOST_WIDE_INT bytes, HOST_WIDE_INT words)
{
  switch (mode)
    {
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
      cum->words += words;
      cum->nregs -= words;
      cum->regno += words;

      if (cum->nregs <= 0)
	{
	  cum->nregs = 0;
	  cum->regno = 0;
	}
      break;

    case DFmode:
      if (cum->float_in_sse < 2)
	break;
    case SFmode:
      if (cum->float_in_sse < 1)
	break;
      /* FALLTHRU */

    case TImode:
    case V16QImode:
    case V8HImode:
    case V4SImode:
    case V2DImode:
    case V4SFmode:
    case V2DFmode:
      if (!type || !AGGREGATE_TYPE_P (type))
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
      break;

    case V8QImode:
    case V4HImode:
    case V2SImode:
    case V2SFmode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  cum->mmx_words += words;
	  cum->mmx_nregs -= 1;
	  cum->mmx_regno += 1;
	  if (cum->mmx_nregs <= 0)
	    {
	      cum->mmx_nregs = 0;
	      cum->mmx_regno = 0;
	    }
	}
      break;
    }
}

static void
function_arg_advance_64 (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			 tree type, HOST_WIDE_INT words)
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

static void
function_arg_advance_ms_64 (CUMULATIVE_ARGS *cum, HOST_WIDE_INT bytes,
			    HOST_WIDE_INT words)
{
  /* Otherwise, this should be passed indirect.  */
  gcc_assert (bytes == 1 || bytes == 2 || bytes == 4 || bytes == 8);

  cum->words += words;
  if (cum->nregs > 0)
    {
      cum->nregs -= 1;
      cum->regno += 1;
    }
}

void
function_arg_advance (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		      tree type, int named ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT bytes, words;

  if (mode == BLKmode)
    bytes = int_size_in_bytes (type);
  else
    bytes = GET_MODE_SIZE (mode);
  words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (type)
    mode = type_natural_mode (type);

  if (TARGET_64BIT_MS_ABI)
    function_arg_advance_ms_64 (cum, bytes, words);
  else if (TARGET_64BIT)
    function_arg_advance_64 (cum, mode, type, words);
  else
    function_arg_advance_32 (cum, mode, type, bytes, words);
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

static rtx
function_arg_32 (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		 enum machine_mode orig_mode, tree type,
		 HOST_WIDE_INT bytes, HOST_WIDE_INT words)
{
  static bool warnedsse, warnedmmx;

  /* Avoid the AL settings for the Unix64 ABI.  */
  if (mode == VOIDmode)
    return constm1_rtx;

  switch (mode)
    {
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
	{
	  int regno = cum->regno;

	  /* Fastcall allocates the first two DWORD (SImode) or
            smaller arguments to ECX and EDX if it isn't an
            aggregate type .  */
	  if (cum->fastcall)
	    {
	      if (mode == BLKmode
		  || mode == DImode
		  || (type && AGGREGATE_TYPE_P (type)))
	        break;

	      /* ECX not EAX is the first allocated register.  */
	      if (regno == AX_REG)
		regno = CX_REG;
	    }
	  return gen_rtx_REG (mode, regno);
	}
      break;

    case DFmode:
      if (cum->float_in_sse < 2)
	break;
    case SFmode:
      if (cum->float_in_sse < 1)
	break;
      /* FALLTHRU */
    case TImode:
    case V16QImode:
    case V8HImode:
    case V4SImode:
    case V2DImode:
    case V4SFmode:
    case V2DFmode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  if (!TARGET_SSE && !warnedsse && cum->warn_sse)
	    {
	      warnedsse = true;
	      warning (0, "SSE vector argument without SSE enabled "
		       "changes the ABI");
	    }
	  if (cum->sse_nregs)
	    return gen_reg_or_parallel (mode, orig_mode,
				        cum->sse_regno + FIRST_SSE_REG);
	}
      break;

    case V8QImode:
    case V4HImode:
    case V2SImode:
    case V2SFmode:
      if (!type || !AGGREGATE_TYPE_P (type))
	{
	  if (!TARGET_MMX && !warnedmmx && cum->warn_mmx)
	    {
	      warnedmmx = true;
	      warning (0, "MMX vector argument without MMX enabled "
		       "changes the ABI");
	    }
	  if (cum->mmx_nregs)
	    return gen_reg_or_parallel (mode, orig_mode,
				        cum->mmx_regno + FIRST_MMX_REG);
	}
      break;
    }

  return NULL_RTX;
}

static rtx
function_arg_64 (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		 enum machine_mode orig_mode, tree type)
{
  /* Handle a hidden AL argument containing number of registers
     for varargs x86-64 functions.  */
  if (mode == VOIDmode)
    return GEN_INT (cum->maybe_vaarg
		    ? (cum->sse_nregs < 0
		       ? SSE_REGPARM_MAX
		       : cum->sse_regno)
		    : -1);

  return construct_container (mode, orig_mode, type, 0, cum->nregs,
			      cum->sse_nregs,
			      &x86_64_int_parameter_registers [cum->regno],
			      cum->sse_regno);
}

static rtx
function_arg_ms_64 (CUMULATIVE_ARGS *cum, enum machine_mode mode,
		    enum machine_mode orig_mode, int named)
{
  unsigned int regno;

  /* Avoid the AL settings for the Unix64 ABI.  */
  if (mode == VOIDmode)
    return constm1_rtx;

  /* If we've run out of registers, it goes on the stack.  */
  if (cum->nregs == 0)
    return NULL_RTX;

  regno = x86_64_ms_abi_int_parameter_registers[cum->regno];

  /* Only floating point modes are passed in anything but integer regs.  */
  if (TARGET_SSE && (mode == SFmode || mode == DFmode))
    {
      if (named)
	regno = cum->regno + FIRST_SSE_REG;
      else
	{
	  rtx t1, t2;

	  /* Unnamed floating parameters are passed in both the
	     SSE and integer registers.  */
	  t1 = gen_rtx_REG (mode, cum->regno + FIRST_SSE_REG);
	  t2 = gen_rtx_REG (mode, regno);
	  t1 = gen_rtx_EXPR_LIST (VOIDmode, t1, const0_rtx);
	  t2 = gen_rtx_EXPR_LIST (VOIDmode, t2, const0_rtx);
	  return gen_rtx_PARALLEL (mode, gen_rtvec (2, t1, t2));
	}
    }

  return gen_reg_or_parallel (mode, orig_mode, regno);
}

rtx
function_arg (CUMULATIVE_ARGS *cum, enum machine_mode omode,
	      tree type, int named)
{
  enum machine_mode mode = omode;
  HOST_WIDE_INT bytes, words;

  if (mode == BLKmode)
    bytes = int_size_in_bytes (type);
  else
    bytes = GET_MODE_SIZE (mode);
  words = (bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* To simplify the code below, represent vector types with a vector mode
     even if MMX/SSE are not active.  */
  if (type && TREE_CODE (type) == VECTOR_TYPE)
    mode = type_natural_mode (type);

  if (TARGET_64BIT_MS_ABI)
    return function_arg_ms_64 (cum, mode, omode, named);
  else if (TARGET_64BIT)
    return function_arg_64 (cum, mode, omode, type);
  else
    return function_arg_32 (cum, mode, omode, type, bytes, words);
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.  */

static bool
ix86_pass_by_reference (CUMULATIVE_ARGS *cum ATTRIBUTE_UNUSED,
			enum machine_mode mode ATTRIBUTE_UNUSED,
			const_tree type, bool named ATTRIBUTE_UNUSED)
{
  if (TARGET_64BIT_MS_ABI)
    {
      if (type)
	{
	  /* Arrays are passed by reference.  */
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    return true;

	  if (AGGREGATE_TYPE_P (type))
	    {
	      /* Structs/unions of sizes other than 8, 16, 32, or 64 bits
	         are passed by reference.  */
	      int el2 = exact_log2 (int_size_in_bytes (type));
	      return !(el2 >= 0 && el2 <= 3);
	    }
	}

      /* __m128 is passed by reference.  */
      /* ??? How to handle complex?  For now treat them as structs,
	 and pass them by reference if they're too large.  */
      if (GET_MODE_SIZE (mode) > 8)
	return true;
    }
  else if (TARGET_64BIT && type && int_size_in_bytes (type) == -1)
    return 1;

  return 0;
}

/* Return true when TYPE should be 128bit aligned for 32bit argument passing
   ABI.  */
static bool
contains_aligned_value_p (tree type)
{
  enum machine_mode mode = TYPE_MODE (type);
  if (((TARGET_SSE && SSE_REG_MODE_P (mode)) || mode == TDmode)
      && (!TYPE_USER_ALIGN (type) || TYPE_ALIGN (type) > 128))
    return true;
  if (TYPE_ALIGN (type) < 128)
    return false;

  if (AGGREGATE_TYPE_P (type))
    {
      /* Walk the aggregates recursively.  */
      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  {
	    tree field;

	    /* Walk all the structure fields.  */
	    for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	      {
		if (TREE_CODE (field) == FIELD_DECL
		    && contains_aligned_value_p (TREE_TYPE (field)))
		  return true;
	      }
	    break;
	  }

	case ARRAY_TYPE:
	  /* Just for use if some languages passes arrays by value.  */
	  if (contains_aligned_value_p (TREE_TYPE (type)))
	    return true;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  return false;
}

/* Gives the alignment boundary, in bits, of an argument with the
   specified mode and type.  */

int
ix86_function_arg_boundary (enum machine_mode mode, tree type)
{
  int align;
  if (type)
    align = TYPE_ALIGN (type);
  else
    align = GET_MODE_ALIGNMENT (mode);
  if (align < PARM_BOUNDARY)
    align = PARM_BOUNDARY;
  /* In 32bit, only _Decimal128 is aligned to its natural boundary.  */
  if (!TARGET_64BIT && mode != TDmode)
    {
      /* i386 ABI defines all arguments to be 4 byte aligned.  We have to
	 make an exception for SSE modes since these require 128bit
	 alignment.

	 The handling here differs from field_alignment.  ICC aligns MMX
	 arguments to 4 byte boundaries, while structure fields are aligned
	 to 8 byte boundaries.  */
      if (!type)
	{
	  if (!(TARGET_SSE && SSE_REG_MODE_P (mode)) && mode != TDmode)
	    align = PARM_BOUNDARY;
	}
      else
	{
	  if (!contains_aligned_value_p (type))
	    align = PARM_BOUNDARY;
	}
    }
  if (align > 128)
    align = 128;
  return align;
}

/* Return true if N is a possible register number of function value.  */

bool
ix86_function_value_regno_p (int regno)
{
  switch (regno)
    {
    case 0:
      return true;

    case FIRST_FLOAT_REG:
      if (TARGET_64BIT_MS_ABI)
	return false;
      return TARGET_FLOAT_RETURNS_IN_80387;

    case FIRST_SSE_REG:
      return TARGET_SSE;

    case FIRST_MMX_REG:
      if (TARGET_MACHO || TARGET_64BIT)
	return false;
      return TARGET_MMX;
    }

  return false;
}

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

static rtx
function_value_32 (enum machine_mode orig_mode, enum machine_mode mode,
		   const_tree fntype, const_tree fn)
{
  unsigned int regno;

  /* 8-byte vector modes in %mm0. See ix86_return_in_memory for where
     we normally prevent this case when mmx is not available.  However
     some ABIs may require the result to be returned like DImode.  */
  if (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 8)
    regno = TARGET_MMX ? FIRST_MMX_REG : 0;

  /* 16-byte vector modes in %xmm0.  See ix86_return_in_memory for where
     we prevent this case when sse is not available.  However some ABIs
     may require the result to be returned like integer TImode.  */
  else if (mode == TImode
	   || (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 16))
    regno = TARGET_SSE ? FIRST_SSE_REG : 0;

  /* Floating point return values in %st(0) (unless -mno-fp-ret-in-387).  */
  else if (X87_FLOAT_MODE_P (mode) && TARGET_FLOAT_RETURNS_IN_80387)
    regno = FIRST_FLOAT_REG;
  else
    /* Most things go in %eax.  */
    regno = AX_REG;

  /* Override FP return register with %xmm0 for local functions when
     SSE math is enabled or for functions with sseregparm attribute.  */
  if ((fn || fntype) && (mode == SFmode || mode == DFmode))
    {
      int sse_level = ix86_function_sseregparm (fntype, fn, false);
      if ((sse_level >= 1 && mode == SFmode)
	  || (sse_level == 2 && mode == DFmode))
	regno = FIRST_SSE_REG;
    }

  return gen_rtx_REG (orig_mode, regno);
}

static rtx
function_value_64 (enum machine_mode orig_mode, enum machine_mode mode,
		   const_tree valtype)
{
  rtx ret;

  /* Handle libcalls, which don't provide a type node.  */
  if (valtype == NULL)
    {
      switch (mode)
	{
	case SFmode:
	case SCmode:
	case DFmode:
	case DCmode:
	case TFmode:
	case SDmode:
	case DDmode:
	case TDmode:
	  return gen_rtx_REG (mode, FIRST_SSE_REG);
	case XFmode:
	case XCmode:
	  return gen_rtx_REG (mode, FIRST_FLOAT_REG);
	case TCmode:
	  return NULL;
	default:
	  return gen_rtx_REG (mode, AX_REG);
	}
    }

  ret = construct_container (mode, orig_mode, valtype, 1,
			     REGPARM_MAX, SSE_REGPARM_MAX,
			     x86_64_int_return_registers, 0);

  /* For zero sized structures, construct_container returns NULL, but we
     need to keep rest of compiler happy by returning meaningful value.  */
  if (!ret)
    ret = gen_rtx_REG (orig_mode, AX_REG);

  return ret;
}

static rtx
function_value_ms_64 (enum machine_mode orig_mode, enum machine_mode mode)
{
  unsigned int regno = AX_REG;

  if (TARGET_SSE)
    {
      if (mode == SFmode || mode == DFmode)
	regno = FIRST_SSE_REG;
      else if (VECTOR_MODE_P (mode) || GET_MODE_SIZE (mode) == 16)
	regno = FIRST_SSE_REG;
    }

  return gen_rtx_REG (orig_mode, regno);
}

static rtx
ix86_function_value_1 (const_tree valtype, const_tree fntype_or_decl,
		       enum machine_mode orig_mode, enum machine_mode mode)
{
  const_tree fn, fntype;

  fn = NULL_TREE;
  if (fntype_or_decl && DECL_P (fntype_or_decl))
    fn = fntype_or_decl;
  fntype = fn ? TREE_TYPE (fn) : fntype_or_decl;

  if (TARGET_64BIT_MS_ABI)
    return function_value_ms_64 (orig_mode, mode);
  else if (TARGET_64BIT)
    return function_value_64 (orig_mode, mode, valtype);
  else
    return function_value_32 (orig_mode, mode, fntype, fn);
}

static rtx
ix86_function_value (const_tree valtype, const_tree fntype_or_decl,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode, orig_mode;

  orig_mode = TYPE_MODE (valtype);
  mode = type_natural_mode (valtype);
  return ix86_function_value_1 (valtype, fntype_or_decl, orig_mode, mode);
}

rtx
ix86_libcall_value (enum machine_mode mode)
{
  return ix86_function_value_1 (NULL, NULL, mode, mode);
}

/* Return true iff type is returned in memory.  */

static int
return_in_memory_32 (const_tree type, enum machine_mode mode)
{
  HOST_WIDE_INT size;

  if (mode == BLKmode)
    return 1;

  size = int_size_in_bytes (type);

  if (MS_AGGREGATE_RETURN && AGGREGATE_TYPE_P (type) && size <= 8)
    return 0;

  if (VECTOR_MODE_P (mode) || mode == TImode)
    {
      /* User-created vectors small enough to fit in EAX.  */
      if (size < 8)
	return 0;

      /* MMX/3dNow values are returned in MM0,
	 except when it doesn't exits.  */
      if (size == 8)
	return (TARGET_MMX ? 0 : 1);

      /* SSE values are returned in XMM0, except when it doesn't exist.  */
      if (size == 16)
	return (TARGET_SSE ? 0 : 1);
    }

  if (mode == XFmode)
    return 0;

  if (mode == TDmode)
    return 1;

  if (size > 12)
    return 1;
  return 0;
}

static int
return_in_memory_64 (const_tree type, enum machine_mode mode)
{
  int needed_intregs, needed_sseregs;
  return !examine_argument (mode, type, 1, &needed_intregs, &needed_sseregs);
}

static int
return_in_memory_ms_64 (const_tree type, enum machine_mode mode)
{
  HOST_WIDE_INT size = int_size_in_bytes (type);

  /* __m128 and friends are returned in xmm0.  */
  if (!COMPLEX_MODE_P (mode) && size == 16 && VECTOR_MODE_P (mode))
    return 0;

  /* Otherwise, the size must be exactly in [1248]. But not for complex. */
  return (size != 1 && size != 2 && size != 4 && size != 8)
         || COMPLEX_MODE_P (mode);
}

int
ix86_return_in_memory (const_tree type)
{
  const enum machine_mode mode = type_natural_mode (type);

  if (TARGET_64BIT_MS_ABI)
    return return_in_memory_ms_64 (type, mode);
  else if (TARGET_64BIT)
    return return_in_memory_64 (type, mode);
  else
    return return_in_memory_32 (type, mode);
}

/* Return false iff TYPE is returned in memory.  This version is used
   on Solaris 10.  It is similar to the generic ix86_return_in_memory,
   but differs notably in that when MMX is available, 8-byte vectors
   are returned in memory, rather than in MMX registers.  */

int
ix86_sol10_return_in_memory (const_tree type)
{
  int size;
  enum machine_mode mode = type_natural_mode (type);

  if (TARGET_64BIT)
    return return_in_memory_64 (type, mode);

  if (mode == BLKmode)
    return 1;

  size = int_size_in_bytes (type);

  if (VECTOR_MODE_P (mode))
    {
      /* Return in memory only if MMX registers *are* available.  This
	 seems backwards, but it is consistent with the existing
	 Solaris x86 ABI.  */
      if (size == 8)
	return TARGET_MMX;
      if (size == 16)
	return !TARGET_SSE;
    }
  else if (mode == TImode)
    return !TARGET_SSE;
  else if (mode == XFmode)
    return 0;

  return size > 12;
}

/* When returning SSE vector types, we have a choice of either
     (1) being abi incompatible with a -march switch, or
     (2) generating an error.
   Given no good solution, I think the safest thing is one warning.
   The user won't be able to use -Werror, but....

   Choose the STRUCT_VALUE_RTX hook because that's (at present) only
   called in response to actually generating a caller or callee that
   uses such a type.  As opposed to RETURN_IN_MEMORY, which is called
   via aggregate_value_p for general type probing from tree-ssa.  */

static rtx
ix86_struct_value_rtx (tree type, int incoming ATTRIBUTE_UNUSED)
{
  static bool warnedsse, warnedmmx;

  if (!TARGET_64BIT && type)
    {
      /* Look at the return type of the function, not the function type.  */
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (type));

      if (!TARGET_SSE && !warnedsse)
	{
	  if (mode == TImode
	      || (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 16))
	    {
	      warnedsse = true;
	      warning (0, "SSE vector return without SSE enabled "
		       "changes the ABI");
	    }
	}

      if (!TARGET_MMX && !warnedmmx)
	{
	  if (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 8)
	    {
	      warnedmmx = true;
	      warning (0, "MMX vector return without MMX enabled "
		       "changes the ABI");
	    }
	}
    }

  return NULL;
}


/* Create the va_list data type.  */

static tree
ix86_build_builtin_va_list (void)
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  /* For i386 we use plain pointer to argument area.  */
  if (!TARGET_64BIT || TARGET_64BIT_MS_ABI)
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

  va_list_gpr_counter_field = f_gpr;
  va_list_fpr_counter_field = f_fpr;

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

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.  */

static void
setup_incoming_varargs_64 (CUMULATIVE_ARGS *cum)
{
  rtx save_area, mem;
  rtx label;
  rtx label_ref;
  rtx tmp_reg;
  rtx nsse_reg;
  alias_set_type set;
  int i;

  if (! cfun->va_list_gpr_size && ! cfun->va_list_fpr_size)
    return;

  /* Indicate to allocate space on the stack for varargs save area.  */
  ix86_save_varrargs_registers = 1;
  /* We need 16-byte stack alignment to save SSE registers.  If user
     asked for lower preferred_stack_boundary, lets just hope that he knows
     what he is doing and won't varargs SSE values.

     We also may end up assuming that only 64bit values are stored in SSE
     register let some floating point program work.  */
  if (ix86_preferred_stack_boundary >= 128)
    cfun->stack_alignment_needed = 128;

  save_area = frame_pointer_rtx;
  set = get_varargs_alias_set ();

  for (i = cum->regno;
       i < ix86_regparm
       && i < cum->regno + cfun->va_list_gpr_size / UNITS_PER_WORD;
       i++)
    {
      mem = gen_rtx_MEM (Pmode,
			 plus_constant (save_area, i * UNITS_PER_WORD));
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);
      emit_move_insn (mem, gen_rtx_REG (Pmode,
					x86_64_int_parameter_registers[i]));
    }

  if (cum->sse_nregs && cfun->va_list_fpr_size)
    {
      /* Now emit code to save SSE registers.  The AX parameter contains number
	 of SSE parameter registers used to call this function.  We use
	 sse_prologue_save insn template that produces computed jump across
	 SSE saves.  We need some preparation work to get this working.  */

      label = gen_label_rtx ();
      label_ref = gen_rtx_LABEL_REF (Pmode, label);

      /* Compute address to jump to :
         label - 5*eax + nnamed_sse_arguments*5  */
      tmp_reg = gen_reg_rtx (Pmode);
      nsse_reg = gen_reg_rtx (Pmode);
      emit_insn (gen_zero_extendqidi2 (nsse_reg, gen_rtx_REG (QImode, AX_REG)));
      emit_insn (gen_rtx_SET (VOIDmode, tmp_reg,
			      gen_rtx_MULT (Pmode, nsse_reg,
					    GEN_INT (4))));
      if (cum->sse_regno)
	emit_move_insn
	  (nsse_reg,
	   gen_rtx_CONST (DImode,
			  gen_rtx_PLUS (DImode,
					label_ref,
					GEN_INT (cum->sse_regno * 4))));
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
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);
      set_mem_align (mem, BITS_PER_WORD);

      /* And finally do the dirty job!  */
      emit_insn (gen_sse_prologue_save (mem, nsse_reg,
					GEN_INT (cum->sse_regno), label));
    }
}

static void
setup_incoming_varargs_ms_64 (CUMULATIVE_ARGS *cum)
{
  alias_set_type set = get_varargs_alias_set ();
  int i;

  for (i = cum->regno; i < REGPARM_MAX; i++)
    {
      rtx reg, mem;

      mem = gen_rtx_MEM (Pmode,
			 plus_constant (virtual_incoming_args_rtx,
					i * UNITS_PER_WORD));
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);

      reg = gen_rtx_REG (Pmode, x86_64_ms_abi_int_parameter_registers[i]);
      emit_move_insn (mem, reg);
    }
}

static void
ix86_setup_incoming_varargs (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			     tree type, int *pretend_size ATTRIBUTE_UNUSED,
			     int no_rtl)
{
  CUMULATIVE_ARGS next_cum;
  tree fntype;

  /* This argument doesn't appear to be used anymore.  Which is good,
     because the old code here didn't suppress rtl generation.  */
  gcc_assert (!no_rtl);

  if (!TARGET_64BIT)
    return;

  fntype = TREE_TYPE (current_function_decl);

  /* For varargs, we do not want to skip the dummy va_dcl argument.
     For stdargs, we do want to skip the last named argument.  */
  next_cum = *cum;
  if (stdarg_p (fntype))
    function_arg_advance (&next_cum, mode, type, 1);

  if (TARGET_64BIT_MS_ABI)
    setup_incoming_varargs_ms_64 (&next_cum);
  else
    setup_incoming_varargs_64 (&next_cum);
}

/* Implement va_start.  */

static void
ix86_va_start (tree valist, rtx nextarg)
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;
  tree type;

  /* Only 64bit target needs something special.  */
  if (!TARGET_64BIT || TARGET_64BIT_MS_ABI)
    {
      std_expand_builtin_va_start (valist, nextarg);
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)), valist);
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);

  /* Count number of gp and fp argument registers used.  */
  words = current_function_args_info.words;
  n_gpr = current_function_args_info.regno;
  n_fpr = current_function_args_info.sse_regno;

  if (cfun->va_list_gpr_size)
    {
      type = TREE_TYPE (gpr);
      t = build2 (GIMPLE_MODIFY_STMT, type, gpr,
		  build_int_cst (type, n_gpr * 8));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  if (cfun->va_list_fpr_size)
    {
      type = TREE_TYPE (fpr);
      t = build2 (GIMPLE_MODIFY_STMT, type, fpr,
		  build_int_cst (type, n_fpr * 16 + 8*REGPARM_MAX));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  /* Find the overflow area.  */
  type = TREE_TYPE (ovf);
  t = make_tree (type, virtual_incoming_args_rtx);
  if (words != 0)
    t = build2 (POINTER_PLUS_EXPR, type, t,
	        size_int (words * UNITS_PER_WORD));
  t = build2 (GIMPLE_MODIFY_STMT, type, ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  if (cfun->va_list_gpr_size || cfun->va_list_fpr_size)
    {
      /* Find the register save area.
	 Prologue of the function save it right above stack frame.  */
      type = TREE_TYPE (sav);
      t = make_tree (type, frame_pointer_rtx);
      t = build2 (GIMPLE_MODIFY_STMT, type, sav, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
}

/* Implement va_arg.  */

static tree
ix86_gimplify_va_arg (tree valist, tree type, tree *pre_p, tree *post_p)
{
  static const int intreg[6] = { 0, 1, 2, 3, 4, 5 };
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;
  int size, rsize;
  tree lab_false, lab_over = NULL_TREE;
  tree addr, t2;
  rtx container;
  int indirect_p = 0;
  tree ptrtype;
  enum machine_mode nat_mode;

  /* Only 64bit target needs something special.  */
  if (!TARGET_64BIT || TARGET_64BIT_MS_ABI)
    return std_gimplify_va_arg_expr (valist, type, pre_p, post_p);

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = TREE_CHAIN (f_gpr);
  f_ovf = TREE_CHAIN (f_fpr);
  f_sav = TREE_CHAIN (f_ovf);

  valist = build_va_arg_indirect_ref (valist);
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);

  indirect_p = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect_p)
    type = build_pointer_type (type);
  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  nat_mode = type_natural_mode (type);
  container = construct_container (nat_mode, TYPE_MODE (type), type, 0,
				   REGPARM_MAX, SSE_REGPARM_MAX, intreg, 0);

  /* Pull the value out of the saved registers.  */

  addr = create_tmp_var (ptr_type_node, "addr");
  DECL_POINTER_ALIAS_SET (addr) = get_varargs_alias_set ();

  if (container)
    {
      int needed_intregs, needed_sseregs;
      bool need_temp;
      tree int_addr, sse_addr;

      lab_false = create_artificial_label ();
      lab_over = create_artificial_label ();

      examine_argument (nat_mode, type, 0, &needed_intregs, &needed_sseregs);

      need_temp = (!REG_P (container)
		   && ((needed_intregs && TYPE_ALIGN (type) > 64)
		       || TYPE_ALIGN (type) > 128));

      /* In case we are passing structure, verify that it is consecutive block
         on the register save area.  If not we need to do moves.  */
      if (!need_temp && !REG_P (container))
	{
	  /* Verify that all registers are strictly consecutive  */
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
	  int_addr = addr;
	  sse_addr = addr;
	}
      else
	{
	  int_addr = create_tmp_var (ptr_type_node, "int_addr");
	  DECL_POINTER_ALIAS_SET (int_addr) = get_varargs_alias_set ();
	  sse_addr = create_tmp_var (ptr_type_node, "sse_addr");
	  DECL_POINTER_ALIAS_SET (sse_addr) = get_varargs_alias_set ();
	}

      /* First ensure that we fit completely in registers.  */
      if (needed_intregs)
	{
	  t = build_int_cst (TREE_TYPE (gpr),
			     (REGPARM_MAX - needed_intregs + 1) * 8);
	  t = build2 (GE_EXPR, boolean_type_node, gpr, t);
	  t2 = build1 (GOTO_EXPR, void_type_node, lab_false);
	  t = build3 (COND_EXPR, void_type_node, t, t2, NULL_TREE);
	  gimplify_and_add (t, pre_p);
	}
      if (needed_sseregs)
	{
	  t = build_int_cst (TREE_TYPE (fpr),
			     (SSE_REGPARM_MAX - needed_sseregs + 1) * 16
			     + REGPARM_MAX * 8);
	  t = build2 (GE_EXPR, boolean_type_node, fpr, t);
	  t2 = build1 (GOTO_EXPR, void_type_node, lab_false);
	  t = build3 (COND_EXPR, void_type_node, t, t2, NULL_TREE);
	  gimplify_and_add (t, pre_p);
	}

      /* Compute index to start of area used for integer regs.  */
      if (needed_intregs)
	{
	  /* int_addr = gpr + sav; */
	  t = fold_convert (sizetype, gpr);
	  t = build2 (POINTER_PLUS_EXPR, ptr_type_node, sav, t);
	  t = build2 (GIMPLE_MODIFY_STMT, void_type_node, int_addr, t);
	  gimplify_and_add (t, pre_p);
	}
      if (needed_sseregs)
	{
	  /* sse_addr = fpr + sav; */
	  t = fold_convert (sizetype, fpr);
	  t = build2 (POINTER_PLUS_EXPR, ptr_type_node, sav, t);
	  t = build2 (GIMPLE_MODIFY_STMT, void_type_node, sse_addr, t);
	  gimplify_and_add (t, pre_p);
	}
      if (need_temp)
	{
	  int i;
	  tree temp = create_tmp_var (type, "va_arg_tmp");

	  /* addr = &temp; */
	  t = build1 (ADDR_EXPR, build_pointer_type (type), temp);
	  t = build2 (GIMPLE_MODIFY_STMT, void_type_node, addr, t);
	  gimplify_and_add (t, pre_p);

	  for (i = 0; i < XVECLEN (container, 0); i++)
	    {
	      rtx slot = XVECEXP (container, 0, i);
	      rtx reg = XEXP (slot, 0);
	      enum machine_mode mode = GET_MODE (reg);
	      tree piece_type = lang_hooks.types.type_for_mode (mode, 1);
	      tree addr_type = build_pointer_type (piece_type);
	      tree src_addr, src;
	      int src_offset;
	      tree dest_addr, dest;

	      if (SSE_REGNO_P (REGNO (reg)))
		{
		  src_addr = sse_addr;
		  src_offset = (REGNO (reg) - FIRST_SSE_REG) * 16;
		}
	      else
		{
		  src_addr = int_addr;
		  src_offset = REGNO (reg) * 8;
		}
	      src_addr = fold_convert (addr_type, src_addr);
	      src_addr = fold_build2 (POINTER_PLUS_EXPR, addr_type, src_addr,
				      size_int (src_offset));
	      src = build_va_arg_indirect_ref (src_addr);

	      dest_addr = fold_convert (addr_type, addr);
	      dest_addr = fold_build2 (POINTER_PLUS_EXPR, addr_type, dest_addr,
				       size_int (INTVAL (XEXP (slot, 1))));
	      dest = build_va_arg_indirect_ref (dest_addr);

	      t = build2 (GIMPLE_MODIFY_STMT, void_type_node, dest, src);
	      gimplify_and_add (t, pre_p);
	    }
	}

      if (needed_intregs)
	{
	  t = build2 (PLUS_EXPR, TREE_TYPE (gpr), gpr,
		      build_int_cst (TREE_TYPE (gpr), needed_intregs * 8));
	  t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (gpr), gpr, t);
	  gimplify_and_add (t, pre_p);
	}
      if (needed_sseregs)
	{
	  t = build2 (PLUS_EXPR, TREE_TYPE (fpr), fpr,
		      build_int_cst (TREE_TYPE (fpr), needed_sseregs * 16));
	  t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (fpr), fpr, t);
	  gimplify_and_add (t, pre_p);
	}

      t = build1 (GOTO_EXPR, void_type_node, lab_over);
      gimplify_and_add (t, pre_p);

      t = build1 (LABEL_EXPR, void_type_node, lab_false);
      append_to_statement_list (t, pre_p);
    }

  /* ... otherwise out of the overflow area.  */

  /* Care for on-stack alignment if needed.  */
  if (FUNCTION_ARG_BOUNDARY (VOIDmode, type) <= 64
      || integer_zerop (TYPE_SIZE (type)))
    t = ovf;
 else
    {
      HOST_WIDE_INT align = FUNCTION_ARG_BOUNDARY (VOIDmode, type) / 8;
      t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (ovf), ovf,
		  size_int (align - 1));
      t = fold_convert (sizetype, t);
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  size_int (-align));
      t = fold_convert (TREE_TYPE (ovf), t);
    }
  gimplify_expr (&t, pre_p, NULL, is_gimple_val, fb_rvalue);

  t2 = build2 (GIMPLE_MODIFY_STMT, void_type_node, addr, t);
  gimplify_and_add (t2, pre_p);

  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (t), t,
	      size_int (rsize * UNITS_PER_WORD));
  t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (ovf), ovf, t);
  gimplify_and_add (t, pre_p);

  if (container)
    {
      t = build1 (LABEL_EXPR, void_type_node, lab_over);
      append_to_statement_list (t, pre_p);
    }

  ptrtype = build_pointer_type (type);
  addr = fold_convert (ptrtype, addr);

  if (indirect_p)
    addr = build_va_arg_indirect_ref (addr);
  return build_va_arg_indirect_ref (addr);
}

/* Return nonzero if OPNUM's MEM should be matched
   in movabs* patterns.  */

int
ix86_check_movabs (rtx insn, int opnum)
{
  rtx set, mem;

  set = PATTERN (insn);
  if (GET_CODE (set) == PARALLEL)
    set = XVECEXP (set, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  mem = XEXP (set, opnum);
  while (GET_CODE (mem) == SUBREG)
    mem = SUBREG_REG (mem);
  gcc_assert (MEM_P (mem));
  return (volatile_ok || !MEM_VOLATILE_P (mem));
}

/* Initialize the table of extra 80387 mathematical constants.  */

static void
init_ext_80387_constants (void)
{
  static const char * cst[5] =
  {
    "0.3010299956639811952256464283594894482",  /* 0: fldlg2  */
    "0.6931471805599453094286904741849753009",  /* 1: fldln2  */
    "1.4426950408889634073876517827983434472",  /* 2: fldl2e  */
    "3.3219280948873623478083405569094566090",  /* 3: fldl2t  */
    "3.1415926535897932385128089594061862044",  /* 4: fldpi   */
  };
  int i;

  for (i = 0; i < 5; i++)
    {
      real_from_string (&ext_80387_constants_table[i], cst[i]);
      /* Ensure each constant is rounded to XFmode precision.  */
      real_convert (&ext_80387_constants_table[i],
		    XFmode, &ext_80387_constants_table[i]);
    }

  ext_80387_constants_init = 1;
}

/* Return true if the constant is something that can be loaded with
   a special instruction.  */

int
standard_80387_constant_p (rtx x)
{
  enum machine_mode mode = GET_MODE (x);

  REAL_VALUE_TYPE r;

  if (!(X87_FLOAT_MODE_P (mode) && (GET_CODE (x) == CONST_DOUBLE)))
    return -1;

  if (x == CONST0_RTX (mode))
    return 1;
  if (x == CONST1_RTX (mode))
    return 2;

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);

  /* For XFmode constants, try to find a special 80387 instruction when
     optimizing for size or on those CPUs that benefit from them.  */
  if (mode == XFmode
      && (optimize_size || TARGET_EXT_80387_CONSTANTS))
    {
      int i;

      if (! ext_80387_constants_init)
	init_ext_80387_constants ();

      for (i = 0; i < 5; i++)
        if (real_identical (&r, &ext_80387_constants_table[i]))
	  return i + 3;
    }

  /* Load of the constant -0.0 or -1.0 will be split as
     fldz;fchs or fld1;fchs sequence.  */
  if (real_isnegzero (&r))
    return 8;
  if (real_identical (&r, &dconstm1))
    return 9;

  return 0;
}

/* Return the opcode of the special instruction to be used to load
   the constant X.  */

const char *
standard_80387_constant_opcode (rtx x)
{
  switch (standard_80387_constant_p (x))
    {
    case 1:
      return "fldz";
    case 2:
      return "fld1";
    case 3:
      return "fldlg2";
    case 4:
      return "fldln2";
    case 5:
      return "fldl2e";
    case 6:
      return "fldl2t";
    case 7:
      return "fldpi";
    case 8:
    case 9:
      return "#";
    default:
      gcc_unreachable ();
    }
}

/* Return the CONST_DOUBLE representing the 80387 constant that is
   loaded by the specified special instruction.  The argument IDX
   matches the return value from standard_80387_constant_p.  */

rtx
standard_80387_constant_rtx (int idx)
{
  int i;

  if (! ext_80387_constants_init)
    init_ext_80387_constants ();

  switch (idx)
    {
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
      i = idx - 3;
      break;

    default:
      gcc_unreachable ();
    }

  return CONST_DOUBLE_FROM_REAL_VALUE (ext_80387_constants_table[i],
				       XFmode);
}

/* Return 1 if mode is a valid mode for sse.  */
static int
standard_sse_mode_p (enum machine_mode mode)
{
  switch (mode)
    {
    case V16QImode:
    case V8HImode:
    case V4SImode:
    case V2DImode:
    case V4SFmode:
    case V2DFmode:
      return 1;

    default:
      return 0;
    }
}

/* Return 1 if X is FP constant we can load to SSE register w/o using memory.
 */
int
standard_sse_constant_p (rtx x)
{
  enum machine_mode mode = GET_MODE (x);

  if (x == const0_rtx || x == CONST0_RTX (GET_MODE (x)))
    return 1;
  if (vector_all_ones_operand (x, mode)
      && standard_sse_mode_p (mode))
    return TARGET_SSE2 ? 2 : -1;

  return 0;
}

/* Return the opcode of the special instruction to be used to load
   the constant X.  */

const char *
standard_sse_constant_opcode (rtx insn, rtx x)
{
  switch (standard_sse_constant_p (x))
    {
    case 1:
      if (get_attr_mode (insn) == MODE_V4SF)
        return "xorps\t%0, %0";
      else if (get_attr_mode (insn) == MODE_V2DF)
        return "xorpd\t%0, %0";
      else
        return "pxor\t%0, %0";
    case 2:
      return "pcmpeqd\t%0, %0";
    }
  gcc_unreachable ();
}

/* Returns 1 if OP contains a symbol reference */

int
symbolic_reference_mentioned_p (rtx op)
{
  const char *fmt;
  int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

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
   marker to de-allocate.  */

int
ix86_can_use_return_insn_p (void)
{
  struct ix86_frame frame;

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

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may
   be accessed via the stack pointer) in functions that seem suitable.  */

int
ix86_frame_pointer_required (void)
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
      && (!current_function_is_leaf
	  || ix86_current_function_calls_tls_descriptor))
    return 1;

  if (current_function_profile)
    return 1;

  return 0;
}

/* Record that the current function accesses previous call frames.  */

void
ix86_setup_frame_addresses (void)
{
  cfun->machine->accesses_prev_frame = 1;
}

#if (defined(HAVE_GAS_HIDDEN) && (SUPPORTS_ONE_ONLY - 0)) || TARGET_MACHO
# define USE_HIDDEN_LINKONCE 1
#else
# define USE_HIDDEN_LINKONCE 0
#endif

static int pic_labels_used;

/* Fills in the label name that should be used for a pc thunk for
   the given register.  */

static void
get_pc_thunk_name (char name[32], unsigned int regno)
{
  gcc_assert (!TARGET_64BIT);

  if (USE_HIDDEN_LINKONCE)
    sprintf (name, "__i686.get_pc_thunk.%s", reg_names[regno]);
  else
    ASM_GENERATE_INTERNAL_LABEL (name, "LPR", regno);
}


/* This function generates code for -fpic that loads %ebx with
   the return address of the caller and then returns.  */

void
ix86_file_end (void)
{
  rtx xops[2];
  int regno;

  for (regno = 0; regno < 8; ++regno)
    {
      char name[32];

      if (! ((pic_labels_used >> regno) & 1))
	continue;

      get_pc_thunk_name (name, regno);

#if TARGET_MACHO
      if (TARGET_MACHO)
	{
	  switch_to_section (darwin_sections[text_coal_section]);
	  fputs ("\t.weak_definition\t", asm_out_file);
	  assemble_name (asm_out_file, name);
	  fputs ("\n\t.private_extern\t", asm_out_file);
	  assemble_name (asm_out_file, name);
	  fputs ("\n", asm_out_file);
	  ASM_OUTPUT_LABEL (asm_out_file, name);
	}
      else
#endif
      if (USE_HIDDEN_LINKONCE)
	{
	  tree decl;

	  decl = build_decl (FUNCTION_DECL, get_identifier (name),
			     error_mark_node);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_ONE_ONLY (decl) = 1;

	  (*targetm.asm_out.unique_section) (decl, 0);
	  switch_to_section (get_named_section (decl, NULL, 0));

	  (*targetm.asm_out.globalize_label) (asm_out_file, name);
	  fputs ("\t.hidden\t", asm_out_file);
	  assemble_name (asm_out_file, name);
	  fputc ('\n', asm_out_file);
	  ASM_DECLARE_FUNCTION_NAME (asm_out_file, name, decl);
	}
      else
	{
	  switch_to_section (text_section);
	  ASM_OUTPUT_LABEL (asm_out_file, name);
	}

      xops[0] = gen_rtx_REG (SImode, regno);
      xops[1] = gen_rtx_MEM (SImode, stack_pointer_rtx);
      output_asm_insn ("mov{l}\t{%1, %0|%0, %1}", xops);
      output_asm_insn ("ret", xops);
    }

  if (NEED_INDICATE_EXEC_STACK)
    file_end_indicate_exec_stack ();
}

/* Emit code for the SET_GOT patterns.  */

const char *
output_set_got (rtx dest, rtx label ATTRIBUTE_UNUSED)
{
  rtx xops[3];

  xops[0] = dest;

  if (TARGET_VXWORKS_RTP && flag_pic)
    {
      /* Load (*VXWORKS_GOTT_BASE) into the PIC register.  */
      xops[2] = gen_rtx_MEM (Pmode,
			     gen_rtx_SYMBOL_REF (Pmode, VXWORKS_GOTT_BASE));
      output_asm_insn ("mov{l}\t{%2, %0|%0, %2}", xops);

      /* Load (*VXWORKS_GOTT_BASE)[VXWORKS_GOTT_INDEX] into the PIC register.
	 Use %P and a local symbol in order to print VXWORKS_GOTT_INDEX as
	 an unadorned address.  */
      xops[2] = gen_rtx_SYMBOL_REF (Pmode, VXWORKS_GOTT_INDEX);
      SYMBOL_REF_FLAGS (xops[2]) |= SYMBOL_FLAG_LOCAL;
      output_asm_insn ("mov{l}\t{%P2(%0), %0|%0, DWORD PTR %P2[%0]}", xops);
      return "";
    }

  xops[1] = gen_rtx_SYMBOL_REF (Pmode, GOT_SYMBOL_NAME);

  if (! TARGET_DEEP_BRANCH_PREDICTION || !flag_pic)
    {
      xops[2] = gen_rtx_LABEL_REF (Pmode, label ? label : gen_label_rtx ());

      if (!flag_pic)
	output_asm_insn ("mov{l}\t{%2, %0|%0, %2}", xops);
      else
	output_asm_insn ("call\t%a2", xops);

#if TARGET_MACHO
      /* Output the Mach-O "canonical" label name ("Lxx$pb") here too.  This
         is what will be referenced by the Mach-O PIC subsystem.  */
      if (!label)
	ASM_OUTPUT_LABEL (asm_out_file, machopic_function_base_name ());
#endif

      (*targetm.asm_out.internal_label) (asm_out_file, "L",
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
      /* Output the Mach-O "canonical" label name ("Lxx$pb") here too.  This
         is what will be referenced by the Mach-O PIC subsystem.  */
#if TARGET_MACHO
      if (!label)
	ASM_OUTPUT_LABEL (asm_out_file, machopic_function_base_name ());
      else
        targetm.asm_out.internal_label (asm_out_file, "L",
					   CODE_LABEL_NUMBER (label));
#endif
    }

  if (TARGET_MACHO)
    return "";

  if (!flag_pic || TARGET_DEEP_BRANCH_PREDICTION)
    output_asm_insn ("add{l}\t{%1, %0|%0, %1}", xops);
  else
    output_asm_insn ("add{l}\t{%1+[.-%a2], %0|%0, %1+(.-%a2)}", xops);

  return "";
}

/* Generate an "push" pattern for input ARG.  */

static rtx
gen_push (rtx arg)
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
ix86_select_alt_pic_regnum (void)
{
  if (current_function_is_leaf && !current_function_profile
      && !ix86_current_function_calls_tls_descriptor)
    {
      int i;
      for (i = 2; i >= 0; --i)
        if (!df_regs_ever_live_p (i))
	  return i;
    }

  return INVALID_REGNUM;
}

/* Return 1 if we need to save REGNO.  */
static int
ix86_save_reg (unsigned int regno, int maybe_eh_return)
{
  if (pic_offset_table_rtx
      && regno == REAL_PIC_OFFSET_TABLE_REGNUM
      && (df_regs_ever_live_p (REAL_PIC_OFFSET_TABLE_REGNUM)
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

  if (cfun->machine->force_align_arg_pointer
      && regno == REGNO (cfun->machine->force_align_arg_pointer))
    return 1;

  return (df_regs_ever_live_p (regno)
	  && !call_used_regs[regno]
	  && !fixed_regs[regno]
	  && (regno != HARD_FRAME_POINTER_REGNUM || !frame_pointer_needed));
}

/* Return number of registers to be saved on the stack.  */

static int
ix86_nsaved_regs (void)
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
ix86_initial_elimination_offset (int from, int to)
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
      gcc_assert (to == STACK_POINTER_REGNUM);

      if (from == ARG_POINTER_REGNUM)
	return frame.stack_pointer_offset;

      gcc_assert (from == FRAME_POINTER_REGNUM);
      return frame.stack_pointer_offset - frame.frame_pointer_offset;
    }
}

/* Fill structure ix86_frame about frame of currently computed function.  */

static void
ix86_compute_frame_layout (struct ix86_frame *frame)
{
  HOST_WIDE_INT total_size;
  unsigned int stack_alignment_needed;
  HOST_WIDE_INT offset;
  unsigned int preferred_alignment;
  HOST_WIDE_INT size = get_frame_size ();

  frame->nregs = ix86_nsaved_regs ();
  total_size = size;

  stack_alignment_needed = cfun->stack_alignment_needed / BITS_PER_UNIT;
  preferred_alignment = cfun->preferred_stack_boundary / BITS_PER_UNIT;

  /* During reload iteration the amount of registers saved can change.
     Recompute the value as needed.  Do not recompute when amount of registers
     didn't change as reload does multiple calls to the function and does not
     expect the decision to change within single iteration.  */
  if (!optimize_size
      && cfun->machine->use_fast_prologue_epilogue_nregs != frame->nregs)
    {
      int count = frame->nregs;

      cfun->machine->use_fast_prologue_epilogue_nregs = count;
      /* The fast prologue uses move instead of push to save registers.  This
         is significantly longer, but also executes faster as modern hardware
         can execute the moves in parallel, but can't do that for push/pop.

	 Be careful about choosing what prologue to emit:  When function takes
	 many instructions to execute we may use slow version as well as in
	 case function is known to be outside hot spot (this is known with
	 feedback only).  Weight the size of function by number of registers
	 to save as it is cheap to use one or two push instructions but very
	 slow to use many of them.  */
      if (count)
	count = (count - 1) * FAST_PROLOGUE_INSN_COUNT;
      if (cfun->function_frequency < FUNCTION_FREQUENCY_NORMAL
	  || (flag_branch_probabilities
	      && cfun->function_frequency < FUNCTION_FREQUENCY_HOT))
        cfun->machine->use_fast_prologue_epilogue = false;
      else
        cfun->machine->use_fast_prologue_epilogue
	   = !expensive_function_p (count);
    }
  if (TARGET_PROLOGUE_USING_MOVE
      && cfun->machine->use_fast_prologue_epilogue)
    frame->save_regs_using_mov = true;
  else
    frame->save_regs_using_mov = false;


  /* Skip return address and saved base pointer.  */
  offset = frame_pointer_needed ? UNITS_PER_WORD * 2 : UNITS_PER_WORD;

  frame->hard_frame_pointer_offset = offset;

  /* Do some sanity checking of stack_alignment_needed and
     preferred_alignment, since i386 port is the only using those features
     that may break easily.  */

  gcc_assert (!size || stack_alignment_needed);
  gcc_assert (preferred_alignment >= STACK_BOUNDARY / BITS_PER_UNIT);
  gcc_assert (preferred_alignment <= PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT);
  gcc_assert (stack_alignment_needed
	      <= PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT);

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
     all the function calls as dead code.
     Skipping is however impossible when function calls alloca.  Alloca
     expander assumes that last current_function_outgoing_args_size
     of stack frame are unused.  */
  if (ACCUMULATE_OUTGOING_ARGS
      && (!current_function_is_leaf || current_function_calls_alloca
	  || ix86_current_function_calls_tls_descriptor))
    {
      offset += current_function_outgoing_args_size;
      frame->outgoing_arguments_size = current_function_outgoing_args_size;
    }
  else
    frame->outgoing_arguments_size = 0;

  /* Align stack boundary.  Only needed if we're calling another function
     or using alloca.  */
  if (!current_function_is_leaf || current_function_calls_alloca
      || ix86_current_function_calls_tls_descriptor)
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

  if ((!frame->to_allocate && frame->nregs <= 1)
      || (TARGET_64BIT && frame->to_allocate >= (HOST_WIDE_INT) 0x80000000))
    frame->save_regs_using_mov = false;

  if (TARGET_RED_ZONE && current_function_sp_is_unchanging
      && current_function_is_leaf
      && !ix86_current_function_calls_tls_descriptor)
    {
      frame->red_zone_size = frame->to_allocate;
      if (frame->save_regs_using_mov)
	frame->red_zone_size += frame->nregs * UNITS_PER_WORD;
      if (frame->red_zone_size > RED_ZONE_SIZE - RED_ZONE_RESERVE)
	frame->red_zone_size = RED_ZONE_SIZE - RED_ZONE_RESERVE;
    }
  else
    frame->red_zone_size = 0;
  frame->to_allocate -= frame->red_zone_size;
  frame->stack_pointer_offset -= frame->red_zone_size;
#if 0
  fprintf (stderr, "\n");
  fprintf (stderr, "nregs: %ld\n", (long)frame->nregs);
  fprintf (stderr, "size: %ld\n", (long)size);
  fprintf (stderr, "alignment1: %ld\n", (long)stack_alignment_needed);
  fprintf (stderr, "padding1: %ld\n", (long)frame->padding1);
  fprintf (stderr, "va_arg: %ld\n", (long)frame->va_arg_size);
  fprintf (stderr, "padding2: %ld\n", (long)frame->padding2);
  fprintf (stderr, "to_allocate: %ld\n", (long)frame->to_allocate);
  fprintf (stderr, "red_zone_size: %ld\n", (long)frame->red_zone_size);
  fprintf (stderr, "frame_pointer_offset: %ld\n", (long)frame->frame_pointer_offset);
  fprintf (stderr, "hard_frame_pointer_offset: %ld\n",
	   (long)frame->hard_frame_pointer_offset);
  fprintf (stderr, "stack_pointer_offset: %ld\n", (long)frame->stack_pointer_offset);
  fprintf (stderr, "current_function_is_leaf: %ld\n", (long)current_function_is_leaf);
  fprintf (stderr, "current_function_calls_alloca: %ld\n", (long)current_function_calls_alloca);
  fprintf (stderr, "x86_current_function_calls_tls_descriptor: %ld\n", (long)ix86_current_function_calls_tls_descriptor);
#endif
}

/* Emit code to save registers in the prologue.  */

static void
ix86_emit_save_regs (void)
{
  unsigned int regno;
  rtx insn;

  for (regno = FIRST_PSEUDO_REGISTER; regno-- > 0; )
    if (ix86_save_reg (regno, true))
      {
	insn = emit_insn (gen_push (gen_rtx_REG (Pmode, regno)));
	RTX_FRAME_RELATED_P (insn) = 1;
      }
}

/* Emit code to save registers using MOV insns.  First register
   is restored from POINTER + OFFSET.  */
static void
ix86_emit_save_regs_using_mov (rtx pointer, HOST_WIDE_INT offset)
{
  unsigned int regno;
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

/* Expand prologue or epilogue stack adjustment.
   The pattern exist to put a dependency on all ebp-based memory accesses.
   STYLE should be negative if instructions should be marked as frame related,
   zero if %r11 register is live and cannot be freely used and positive
   otherwise.  */

static void
pro_epilogue_adjust_stack (rtx dest, rtx src, rtx offset, int style)
{
  rtx insn;

  if (! TARGET_64BIT)
    insn = emit_insn (gen_pro_epilogue_adjust_stack_1 (dest, src, offset));
  else if (x86_64_immediate_operand (offset, DImode))
    insn = emit_insn (gen_pro_epilogue_adjust_stack_rex64 (dest, src, offset));
  else
    {
      rtx r11;
      /* r11 is used by indirect sibcall return as well, set before the
	 epilogue and used after the epilogue.  ATM indirect sibcall
	 shouldn't be used together with huge frame sizes in one
	 function because of the frame_size check in sibcall.c.  */
      gcc_assert (style);
      r11 = gen_rtx_REG (DImode, R11_REG);
      insn = emit_insn (gen_rtx_SET (DImode, r11, offset));
      if (style < 0)
	RTX_FRAME_RELATED_P (insn) = 1;
      insn = emit_insn (gen_pro_epilogue_adjust_stack_rex64_2 (dest, src, r11,
							       offset));
    }
  if (style < 0)
    RTX_FRAME_RELATED_P (insn) = 1;
}

/* Handle the TARGET_INTERNAL_ARG_POINTER hook.  */

static rtx
ix86_internal_arg_pointer (void)
{
  bool has_force_align_arg_pointer =
    (0 != lookup_attribute (ix86_force_align_arg_pointer_string,
			    TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl))));
  if ((FORCE_PREFERRED_STACK_BOUNDARY_IN_MAIN
       && DECL_NAME (current_function_decl)
       && MAIN_NAME_P (DECL_NAME (current_function_decl))
       && DECL_FILE_SCOPE_P (current_function_decl))
      || ix86_force_align_arg_pointer
      || has_force_align_arg_pointer)
    {
      /* Nested functions can't realign the stack due to a register
	 conflict.  */
      if (DECL_CONTEXT (current_function_decl)
	  && TREE_CODE (DECL_CONTEXT (current_function_decl)) == FUNCTION_DECL)
	{
	  if (ix86_force_align_arg_pointer)
	    warning (0, "-mstackrealign ignored for nested functions");
	  if (has_force_align_arg_pointer)
	    error ("%s not supported for nested functions",
		   ix86_force_align_arg_pointer_string);
	  return virtual_incoming_args_rtx;
	}
      cfun->machine->force_align_arg_pointer = gen_rtx_REG (Pmode, CX_REG);
      return copy_to_reg (cfun->machine->force_align_arg_pointer);
    }
  else
    return virtual_incoming_args_rtx;
}

/* Handle the TARGET_DWARF_HANDLE_FRAME_UNSPEC hook.
   This is called from dwarf2out.c to emit call frame instructions
   for frame-related insns containing UNSPECs and UNSPEC_VOLATILEs. */
static void
ix86_dwarf_handle_frame_unspec (const char *label, rtx pattern, int index)
{
  rtx unspec = SET_SRC (pattern);
  gcc_assert (GET_CODE (unspec) == UNSPEC);

  switch (index)
    {
    case UNSPEC_REG_SAVE:
      dwarf2out_reg_save_reg (label, XVECEXP (unspec, 0, 0),
			      SET_DEST (pattern));
      break;
    case UNSPEC_DEF_CFA:
      dwarf2out_def_cfa (label, REGNO (SET_DEST (pattern)),
			 INTVAL (XVECEXP (unspec, 0, 0)));
      break;
    default:
      gcc_unreachable ();
    }
}

/* Expand the prologue into a bunch of separate insns.  */

void
ix86_expand_prologue (void)
{
  rtx insn;
  bool pic_reg_used;
  struct ix86_frame frame;
  HOST_WIDE_INT allocate;

  ix86_compute_frame_layout (&frame);

  if (cfun->machine->force_align_arg_pointer)
    {
      rtx x, y;

      /* Grab the argument pointer.  */
      x = plus_constant (stack_pointer_rtx, 4);
      y = cfun->machine->force_align_arg_pointer;
      insn = emit_insn (gen_rtx_SET (VOIDmode, y, x));
      RTX_FRAME_RELATED_P (insn) = 1;

      /* The unwind info consists of two parts: install the fafp as the cfa,
	 and record the fafp as the "save register" of the stack pointer.
	 The later is there in order that the unwinder can see where it
	 should restore the stack pointer across the and insn.  */
      x = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, const0_rtx), UNSPEC_DEF_CFA);
      x = gen_rtx_SET (VOIDmode, y, x);
      RTX_FRAME_RELATED_P (x) = 1;
      y = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, stack_pointer_rtx),
			  UNSPEC_REG_SAVE);
      y = gen_rtx_SET (VOIDmode, cfun->machine->force_align_arg_pointer, y);
      RTX_FRAME_RELATED_P (y) = 1;
      x = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, x, y));
      x = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, x, NULL);
      REG_NOTES (insn) = x;

      /* Align the stack.  */
      emit_insn (gen_andsi3 (stack_pointer_rtx, stack_pointer_rtx,
			     GEN_INT (-16)));

      /* And here we cheat like madmen with the unwind info.  We force the
	 cfa register back to sp+4, which is exactly what it was at the
	 start of the function.  Re-pushing the return address results in
	 the return at the same spot relative to the cfa, and thus is
	 correct wrt the unwind info.  */
      x = cfun->machine->force_align_arg_pointer;
      x = gen_frame_mem (Pmode, plus_constant (x, -4));
      insn = emit_insn (gen_push (x));
      RTX_FRAME_RELATED_P (insn) = 1;

      x = GEN_INT (4);
      x = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, x), UNSPEC_DEF_CFA);
      x = gen_rtx_SET (VOIDmode, stack_pointer_rtx, x);
      x = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, x, NULL);
      REG_NOTES (insn) = x;
    }

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

  if (!frame.save_regs_using_mov)
    ix86_emit_save_regs ();
  else
    allocate += frame.nregs * UNITS_PER_WORD;

  /* When using red zone we may start register saving before allocating
     the stack frame saving one cycle of the prologue. However I will
     avoid doing this if I am going to have to probe the stack since
     at least on x86_64 the stack probe can turn into a call that clobbers
     a red zone location */
  if (TARGET_RED_ZONE && frame.save_regs_using_mov
      && (! TARGET_STACK_PROBE || allocate < CHECK_STACK_LIMIT))
    ix86_emit_save_regs_using_mov (frame_pointer_needed ? hard_frame_pointer_rtx
				   : stack_pointer_rtx,
				   -frame.nregs * UNITS_PER_WORD);

  if (allocate == 0)
    ;
  else if (! TARGET_STACK_PROBE || allocate < CHECK_STACK_LIMIT)
    pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (-allocate), -1);
  else
    {
      /* Only valid for Win32.  */
      rtx eax = gen_rtx_REG (Pmode, AX_REG);
      bool eax_live;
      rtx t;

      gcc_assert (!TARGET_64BIT || TARGET_64BIT_MS_ABI);

      if (TARGET_64BIT_MS_ABI)
	eax_live = false;
      else
	eax_live = ix86_eax_live_at_start_p ();

      if (eax_live)
	{
	  emit_insn (gen_push (eax));
	  allocate -= UNITS_PER_WORD;
	}

      emit_move_insn (eax, GEN_INT (allocate));

      if (TARGET_64BIT)
	insn = gen_allocate_stack_worker_64 (eax);
      else
	insn = gen_allocate_stack_worker_32 (eax);
      insn = emit_insn (insn);
      RTX_FRAME_RELATED_P (insn) = 1;
      t = gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (-allocate));
      t = gen_rtx_SET (VOIDmode, stack_pointer_rtx, t);
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
					    t, REG_NOTES (insn));

      if (eax_live)
	{
	  if (frame_pointer_needed)
	    t = plus_constant (hard_frame_pointer_rtx,
			       allocate
			       - frame.to_allocate
			       - frame.nregs * UNITS_PER_WORD);
	  else
	    t = plus_constant (stack_pointer_rtx, allocate);
	  emit_move_insn (eax, gen_rtx_MEM (Pmode, t));
	}
    }

  if (frame.save_regs_using_mov
      && !(TARGET_RED_ZONE
         && (! TARGET_STACK_PROBE || allocate < CHECK_STACK_LIMIT)))
    {
      if (!frame_pointer_needed || !frame.to_allocate)
        ix86_emit_save_regs_using_mov (stack_pointer_rtx, frame.to_allocate);
      else
        ix86_emit_save_regs_using_mov (hard_frame_pointer_rtx,
				       -frame.nregs * UNITS_PER_WORD);
    }

  pic_reg_used = false;
  if (pic_offset_table_rtx
      && (df_regs_ever_live_p (REAL_PIC_OFFSET_TABLE_REGNUM)
	  || current_function_profile))
    {
      unsigned int alt_pic_reg_used = ix86_select_alt_pic_regnum ();

      if (alt_pic_reg_used != INVALID_REGNUM)
	SET_REGNO (pic_offset_table_rtx, alt_pic_reg_used);

      pic_reg_used = true;
    }

  if (pic_reg_used)
    {
      if (TARGET_64BIT)
	{
	  if (ix86_cmodel == CM_LARGE_PIC)
	    {
              rtx tmp_reg = gen_rtx_REG (DImode, R11_REG);
	      rtx label = gen_label_rtx ();
	      emit_label (label);
	      LABEL_PRESERVE_P (label) = 1;
	      gcc_assert (REGNO (pic_offset_table_rtx) != REGNO (tmp_reg));
	      insn = emit_insn (gen_set_rip_rex64 (pic_offset_table_rtx, label));
	      insn = emit_insn (gen_set_got_offset_rex64 (tmp_reg, label));
	      insn = emit_insn (gen_adddi3 (pic_offset_table_rtx,
					    pic_offset_table_rtx, tmp_reg));
	    }
	  else
            insn = emit_insn (gen_set_got_rex64 (pic_offset_table_rtx));
	}
      else
        insn = emit_insn (gen_set_got (pic_offset_table_rtx));
    }

  /* Prevent function calls from being scheduled before the call to mcount.
     In the pic_reg_used case, make sure that the got load isn't deleted.  */
  if (current_function_profile)
    {
      if (pic_reg_used)
	emit_insn (gen_prologue_use (pic_offset_table_rtx));
      emit_insn (gen_blockage ());
    }

  /* Emit cld instruction if stringops are used in the function.  */
  if (TARGET_CLD && ix86_current_function_needs_cld)
    emit_insn (gen_cld ());
}

/* Emit code to restore saved registers using MOV insns.  First register
   is restored from POINTER + OFFSET.  */
static void
ix86_emit_restore_regs_using_mov (rtx pointer, HOST_WIDE_INT offset,
				  int maybe_eh_return)
{
  int regno;
  rtx base_address = gen_rtx_MEM (Pmode, pointer);

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (ix86_save_reg (regno, maybe_eh_return))
      {
	/* Ensure that adjust_address won't be forced to produce pointer
	   out of range allowed by x86-64 instruction set.  */
	if (TARGET_64BIT && offset != trunc_int_for_mode (offset, SImode))
	  {
	    rtx r11;

	    r11 = gen_rtx_REG (DImode, R11_REG);
	    emit_move_insn (r11, GEN_INT (offset));
	    emit_insn (gen_adddi3 (r11, r11, pointer));
	    base_address = gen_rtx_MEM (Pmode, r11);
	    offset = 0;
	  }
	emit_move_insn (gen_rtx_REG (Pmode, regno),
			adjust_address (base_address, Pmode, offset));
	offset += UNITS_PER_WORD;
      }
}

/* Restore function stack, frame, and registers.  */

void
ix86_expand_epilogue (int style)
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
     and there is exactly one register to pop. This heuristic may need some
     tuning in future.  */
  if ((!sp_valid && frame.nregs <= 1)
      || (TARGET_EPILOGUE_USING_MOVE
	  && cfun->machine->use_fast_prologue_epilogue
	  && (frame.nregs > 1 || frame.to_allocate))
      || (frame_pointer_needed && !frame.nregs && frame.to_allocate)
      || (frame_pointer_needed && TARGET_USE_LEAVE
	  && cfun->machine->use_fast_prologue_epilogue
	  && frame.nregs == 1)
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

	      pro_epilogue_adjust_stack (stack_pointer_rtx, sa,
					 const0_rtx, style);
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
	pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				   GEN_INT (frame.to_allocate
					    + frame.nregs * UNITS_PER_WORD),
				   style);
      /* If not an i386, mov & pop is faster than "leave".  */
      else if (TARGET_USE_LEAVE || optimize_size
	       || !cfun->machine->use_fast_prologue_epilogue)
	emit_insn (TARGET_64BIT ? gen_leave_rex64 () : gen_leave ());
      else
	{
	  pro_epilogue_adjust_stack (stack_pointer_rtx,
				     hard_frame_pointer_rtx,
				     const0_rtx, style);
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
	  gcc_assert (frame_pointer_needed);
	  pro_epilogue_adjust_stack (stack_pointer_rtx,
				     hard_frame_pointer_rtx,
				     GEN_INT (offset), style);
	}
      else if (frame.to_allocate)
	pro_epilogue_adjust_stack (stack_pointer_rtx, stack_pointer_rtx,
				   GEN_INT (frame.to_allocate), style);

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

  if (cfun->machine->force_align_arg_pointer)
    {
      emit_insn (gen_addsi3 (stack_pointer_rtx,
			     cfun->machine->force_align_arg_pointer,
			     GEN_INT (-4)));
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
	  rtx ecx = gen_rtx_REG (SImode, CX_REG);

	  /* There is no "pascal" calling convention in any 64bit ABI.  */
	  gcc_assert (!TARGET_64BIT);

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
ix86_output_function_epilogue (FILE *file ATTRIBUTE_UNUSED,
			       HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (pic_offset_table_rtx)
    SET_REGNO (pic_offset_table_rtx, REAL_PIC_OFFSET_TABLE_REGNUM);
#if TARGET_MACHO
  /* Mach-O doesn't support labels at the end of objects, so if
     it looks like we might want one, insert a NOP.  */
  {
    rtx insn = get_last_insn ();
    while (insn
	   && NOTE_P (insn)
	   && NOTE_KIND (insn) != NOTE_INSN_DELETED_LABEL)
      insn = PREV_INSN (insn);
    if (insn
	&& (LABEL_P (insn)
	    || (NOTE_P (insn)
		&& NOTE_KIND (insn) == NOTE_INSN_DELETED_LABEL)))
      fputs ("\tnop\n", file);
  }
#endif

}

/* Extract the parts of an RTL expression that is a valid memory address
   for an instruction.  Return 0 if the structure of the address is
   grossly off.  Return -1 if the address contains ASHIFT, so it is not
   strictly valid, but still used for computing length of lea instruction.  */

int
ix86_decompose_address (rtx addr, struct ix86_address *out)
{
  rtx base = NULL_RTX, index = NULL_RTX, disp = NULL_RTX;
  rtx base_reg, index_reg;
  HOST_WIDE_INT scale = 1;
  rtx scale_rtx = NULL_RTX;
  int retval = 1;
  enum ix86_address_seg seg = SEG_DEFAULT;

  if (REG_P (addr) || GET_CODE (addr) == SUBREG)
    base = addr;
  else if (GET_CODE (addr) == PLUS)
    {
      rtx addends[4], op;
      int n = 0, i;

      op = addr;
      do
	{
	  if (n >= 4)
	    return 0;
	  addends[n++] = XEXP (op, 1);
	  op = XEXP (op, 0);
	}
      while (GET_CODE (op) == PLUS);
      if (n >= 4)
	return 0;
      addends[n] = op;

      for (i = n; i >= 0; --i)
	{
	  op = addends[i];
	  switch (GET_CODE (op))
	    {
	    case MULT:
	      if (index)
		return 0;
	      index = XEXP (op, 0);
	      scale_rtx = XEXP (op, 1);
	      break;

	    case UNSPEC:
	      if (XINT (op, 1) == UNSPEC_TP
	          && TARGET_TLS_DIRECT_SEG_REFS
	          && seg == SEG_DEFAULT)
		seg = TARGET_64BIT ? SEG_FS : SEG_GS;
	      else
		return 0;
	      break;

	    case REG:
	    case SUBREG:
	      if (!base)
		base = op;
	      else if (!index)
		index = op;
	      else
		return 0;
	      break;

	    case CONST:
	    case CONST_INT:
	    case SYMBOL_REF:
	    case LABEL_REF:
	      if (disp)
		return 0;
	      disp = op;
	      break;

	    default:
	      return 0;
	    }
	}
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
      if (!CONST_INT_P (tmp))
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
      if (!CONST_INT_P (scale_rtx))
	return 0;
      scale = INTVAL (scale_rtx);
    }

  base_reg = base && GET_CODE (base) == SUBREG ? SUBREG_REG (base) : base;
  index_reg = index && GET_CODE (index) == SUBREG ? SUBREG_REG (index) : index;

  /* Allow arg pointer and stack pointer as index if there is not scaling.  */
  if (base_reg && index_reg && scale == 1
      && (index_reg == arg_pointer_rtx
	  || index_reg == frame_pointer_rtx
	  || (REG_P (index_reg) && REGNO (index_reg) == STACK_POINTER_REGNUM)))
    {
      rtx tmp;
      tmp = base, base = index, index = tmp;
      tmp = base_reg, base_reg = index_reg, index_reg = tmp;
    }

  /* Special case: %ebp cannot be encoded as a base without a displacement.  */
  if ((base_reg == hard_frame_pointer_rtx
       || base_reg == frame_pointer_rtx
       || base_reg == arg_pointer_rtx) && !disp)
    disp = const0_rtx;

  /* Special case: on K6, [%esi] makes the instruction vector decoded.
     Avoid this by transforming to [%esi+0].  */
  if (TARGET_K6 && !optimize_size
      && base_reg && !index_reg && !disp
      && REG_P (base_reg)
      && REGNO_REG_CLASS (REGNO (base_reg)) == SIREG)
    disp = const0_rtx;

  /* Special case: encode reg+reg instead of reg*2.  */
  if (!base && index && scale && scale == 2)
    base = index, base_reg = index_reg, scale = 1;

  /* Special case: scaling cannot be encoded without base or displacement.  */
  if (!base && !disp && index && scale != 1)
    disp = const0_rtx;

  out->base = base;
  out->index = index;
  out->disp = disp;
  out->scale = scale;
  out->seg = seg;

  return retval;
}

/* Return cost of the memory address x.
   For i386, it is better to use a complex address than let gcc copy
   the address into a reg and make a new pseudo.  But not if the address
   requires to two regs - that would mean more pseudos with longer
   lifetimes.  */
static int
ix86_address_cost (rtx x)
{
  struct ix86_address parts;
  int cost = 1;
  int ok = ix86_decompose_address (x, &parts);

  gcc_assert (ok);

  if (parts.base && GET_CODE (parts.base) == SUBREG)
    parts.base = SUBREG_REG (parts.base);
  if (parts.index && GET_CODE (parts.index) == SUBREG)
    parts.index = SUBREG_REG (parts.index);

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

/* Allow {LABEL | SYMBOL}_REF - SYMBOL_REF-FOR-PICBASE for Mach-O as
   this is used for to form addresses to local data when -fPIC is in
   use.  */

static bool
darwin_local_data_pic (rtx disp)
{
  if (GET_CODE (disp) == MINUS)
    {
      if (GET_CODE (XEXP (disp, 0)) == LABEL_REF
          || GET_CODE (XEXP (disp, 0)) == SYMBOL_REF)
        if (GET_CODE (XEXP (disp, 1)) == SYMBOL_REF)
          {
            const char *sym_name = XSTR (XEXP (disp, 1), 0);
            if (! strcmp (sym_name, "<pic base>"))
              return true;
          }
    }

  return false;
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */

bool
legitimate_constant_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  if (!CONST_INT_P (XEXP (x, 1)))
	    return false;
	  x = XEXP (x, 0);
	}

      if (TARGET_MACHO && darwin_local_data_pic (x))
	return true;

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (x) == UNSPEC)
	switch (XINT (x, 1))
	  {
	  case UNSPEC_GOT:
	  case UNSPEC_GOTOFF:
	  case UNSPEC_PLTOFF:
	    return TARGET_64BIT;
	  case UNSPEC_TPOFF:
	  case UNSPEC_NTPOFF:
	    x = XVECEXP (x, 0, 0);
	    return (GET_CODE (x) == SYMBOL_REF
		    && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_EXEC);
	  case UNSPEC_DTPOFF:
	    x = XVECEXP (x, 0, 0);
	    return (GET_CODE (x) == SYMBOL_REF
		    && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC);
	  default:
	    return false;
	  }

      /* We must have drilled down to a symbol.  */
      if (GET_CODE (x) == LABEL_REF)
	return true;
      if (GET_CODE (x) != SYMBOL_REF)
	return false;
      /* FALLTHRU */

    case SYMBOL_REF:
      /* TLS symbols are never valid.  */
      if (SYMBOL_REF_TLS_MODEL (x))
	return false;

      /* DLLIMPORT symbols are never valid.  */
      if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	  && SYMBOL_REF_DLLIMPORT_P (x))
	return false;
      break;

    case CONST_DOUBLE:
      if (GET_MODE (x) == TImode
	  && x != CONST0_RTX (TImode)
          && !TARGET_64BIT)
	return false;
      break;

    case CONST_VECTOR:
      if (x == CONST0_RTX (GET_MODE (x)))
	return true;
      return false;

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
ix86_cannot_force_const_mem (rtx x)
{
  /* We can always put integral constants and vectors in memory.  */
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
      return false;

    default:
      break;
    }
  return !legitimate_constant_p (x);
}

/* Determine if a given RTX is a valid constant address.  */

bool
constant_address_p (rtx x)
{
  return CONSTANT_P (x) && legitimate_address_p (Pmode, x, 1);
}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

bool
legitimate_pic_operand_p (rtx x)
{
  rtx inner;

  switch (GET_CODE (x))
    {
    case CONST:
      inner = XEXP (x, 0);
      if (GET_CODE (inner) == PLUS
	  && CONST_INT_P (XEXP (inner, 1)))
	inner = XEXP (inner, 0);

      /* Only some unspecs are valid as "constants".  */
      if (GET_CODE (inner) == UNSPEC)
	switch (XINT (inner, 1))
	  {
	  case UNSPEC_GOT:
	  case UNSPEC_GOTOFF:
	  case UNSPEC_PLTOFF:
	    return TARGET_64BIT;
	  case UNSPEC_TPOFF:
	    x = XVECEXP (inner, 0, 0);
	    return (GET_CODE (x) == SYMBOL_REF
		    && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_EXEC);
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
legitimate_pic_address_disp_p (rtx disp)
{
  bool saw_plus;

  /* In 64bit mode we can allow direct addresses of symbols and labels
     when they are not dynamic symbols.  */
  if (TARGET_64BIT)
    {
      rtx op0 = disp, op1;

      switch (GET_CODE (disp))
	{
	case LABEL_REF:
	  return true;

	case CONST:
	  if (GET_CODE (XEXP (disp, 0)) != PLUS)
	    break;
	  op0 = XEXP (XEXP (disp, 0), 0);
	  op1 = XEXP (XEXP (disp, 0), 1);
	  if (!CONST_INT_P (op1)
	      || INTVAL (op1) >= 16*1024*1024
	      || INTVAL (op1) < -16*1024*1024)
            break;
	  if (GET_CODE (op0) == LABEL_REF)
	    return true;
	  if (GET_CODE (op0) != SYMBOL_REF)
	    break;
	  /* FALLTHRU */

	case SYMBOL_REF:
	  /* TLS references should always be enclosed in UNSPEC.  */
	  if (SYMBOL_REF_TLS_MODEL (op0))
	    return false;
	  if (!SYMBOL_REF_FAR_ADDR_P (op0) && SYMBOL_REF_LOCAL_P (op0)
	      && ix86_cmodel != CM_LARGE_PIC)
	    return true;
	  break;

	default:
	  break;
	}
    }
  if (GET_CODE (disp) != CONST)
    return 0;
  disp = XEXP (disp, 0);

  if (TARGET_64BIT)
    {
      /* We are unsafe to allow PLUS expressions.  This limit allowed distance
         of GOT tables.  We should not need these anyway.  */
      if (GET_CODE (disp) != UNSPEC
	  || (XINT (disp, 1) != UNSPEC_GOTPCREL
	      && XINT (disp, 1) != UNSPEC_GOTOFF
	      && XINT (disp, 1) != UNSPEC_PLTOFF))
	return 0;

      if (GET_CODE (XVECEXP (disp, 0, 0)) != SYMBOL_REF
	  && GET_CODE (XVECEXP (disp, 0, 0)) != LABEL_REF)
	return 0;
      return 1;
    }

  saw_plus = false;
  if (GET_CODE (disp) == PLUS)
    {
      if (!CONST_INT_P (XEXP (disp, 1)))
	return 0;
      disp = XEXP (disp, 0);
      saw_plus = true;
    }

  if (TARGET_MACHO && darwin_local_data_pic (disp))
    return 1;

  if (GET_CODE (disp) != UNSPEC)
    return 0;

  switch (XINT (disp, 1))
    {
    case UNSPEC_GOT:
      if (saw_plus)
	return false;
      /* We need to check for both symbols and labels because VxWorks loads
	 text labels with @GOT rather than @GOTOFF.  See gotoff_operand for
	 details.  */
      return (GET_CODE (XVECEXP (disp, 0, 0)) == SYMBOL_REF
	      || GET_CODE (XVECEXP (disp, 0, 0)) == LABEL_REF);
    case UNSPEC_GOTOFF:
      /* Refuse GOTOFF in 64bit mode since it is always 64bit when used.
	 While ABI specify also 32bit relocation but we don't produce it in
	 small PIC model at all.  */
      if ((GET_CODE (XVECEXP (disp, 0, 0)) == SYMBOL_REF
	   || GET_CODE (XVECEXP (disp, 0, 0)) == LABEL_REF)
	  && !TARGET_64BIT)
        return gotoff_operand (XVECEXP (disp, 0, 0), Pmode);
      return false;
    case UNSPEC_GOTTPOFF:
    case UNSPEC_GOTNTPOFF:
    case UNSPEC_INDNTPOFF:
      if (saw_plus)
	return false;
      disp = XVECEXP (disp, 0, 0);
      return (GET_CODE (disp) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (disp) == TLS_MODEL_INITIAL_EXEC);
    case UNSPEC_NTPOFF:
      disp = XVECEXP (disp, 0, 0);
      return (GET_CODE (disp) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (disp) == TLS_MODEL_LOCAL_EXEC);
    case UNSPEC_DTPOFF:
      disp = XVECEXP (disp, 0, 0);
      return (GET_CODE (disp) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (disp) == TLS_MODEL_LOCAL_DYNAMIC);
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
legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
		      rtx addr, int strict)
{
  struct ix86_address parts;
  rtx base, index, disp;
  HOST_WIDE_INT scale;
  const char *reason = NULL;
  rtx reason_rtx = NULL_RTX;

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

     Don't allow SUBREG's that span more than a word here.  It can lead to spill
     failures when the base is one word out of a two word structure, which is
     represented internally as a DImode int.  */

  if (base)
    {
      rtx reg;
      reason_rtx = base;

      if (REG_P (base))
  	reg = base;
      else if (GET_CODE (base) == SUBREG
	       && REG_P (SUBREG_REG (base))
	       && GET_MODE_SIZE (GET_MODE (SUBREG_REG (base)))
		  <= UNITS_PER_WORD)
  	reg = SUBREG_REG (base);
      else
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

     Don't allow SUBREG's that span more than a word here -- same as above.  */

  if (index)
    {
      rtx reg;
      reason_rtx = index;

      if (REG_P (index))
  	reg = index;
      else if (GET_CODE (index) == SUBREG
	       && REG_P (SUBREG_REG (index))
	       && GET_MODE_SIZE (GET_MODE (SUBREG_REG (index)))
		  <= UNITS_PER_WORD)
  	reg = SUBREG_REG (index);
      else
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
	  /* Refuse GOTOFF and GOT in 64bit mode since it is always 64bit when
	     used.  While ABI specify also 32bit relocations, we don't produce
	     them at all and use IP relative instead.  */
	  case UNSPEC_GOT:
	  case UNSPEC_GOTOFF:
	    gcc_assert (flag_pic);
	    if (!TARGET_64BIT)
	      goto is_legitimate_pic;
	    reason = "64bit address unspec";
	    goto report_error;

	  case UNSPEC_GOTPCREL:
	    gcc_assert (flag_pic);
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

      else if (SYMBOLIC_CONST (disp)
	       && (flag_pic
		   || (TARGET_MACHO
#if TARGET_MACHO
		       && MACHOPIC_INDIRECT
		       && !machopic_operand_p (disp)
#endif
	       )))
	{

	is_legitimate_pic:
	  if (TARGET_64BIT && (index || base))
	    {
	      /* foo@dtpoff(%rX) is ok.  */
	      if (GET_CODE (disp) != CONST
		  || GET_CODE (XEXP (disp, 0)) != PLUS
		  || GET_CODE (XEXP (XEXP (disp, 0), 0)) != UNSPEC
		  || !CONST_INT_P (XEXP (XEXP (disp, 0), 1))
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
      else if (GET_CODE (disp) != LABEL_REF
	       && !CONST_INT_P (disp)
	       && (GET_CODE (disp) != CONST
		   || !legitimate_constant_p (disp))
	       && (GET_CODE (disp) != SYMBOL_REF
		   || !legitimate_constant_p (disp)))
	{
	  reason = "displacement is not constant";
	  goto report_error;
	}
      else if (TARGET_64BIT
	       && !x86_64_immediate_operand (disp, VOIDmode))
	{
	  reason = "displacement is out of range";
	  goto report_error;
	}
    }

  /* Everything looks valid.  */
  return TRUE;

 report_error:
  return FALSE;
}

/* Return a unique alias set for the GOT.  */

static alias_set_type
ix86_GOT_alias_set (void)
{
  static alias_set_type set = -1;
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
      the PIC reg.  Static data objects have SYMBOL_FLAG_LOCAL set to
      differentiate them from global data objects.  The returned
      address is the PIC reg + an unspec constant.

   GO_IF_LEGITIMATE_ADDRESS rejects symbolic references unless the PIC
   reg also appears in the address.  */

static rtx
legitimize_pic_address (rtx orig, rtx reg)
{
  rtx addr = orig;
  rtx new_rtx = orig;
  rtx base;

#if TARGET_MACHO
  if (TARGET_MACHO && !TARGET_64BIT)
    {
      if (reg == 0)
	reg = gen_reg_rtx (Pmode);
      /* Use the generic Mach-O PIC machinery.  */
      return machopic_legitimize_pic_address (orig, GET_MODE (orig), reg);
    }
#endif

  if (TARGET_64BIT && legitimate_pic_address_disp_p (addr))
    new_rtx = addr;
  else if (TARGET_64BIT
	   && ix86_cmodel != CM_SMALL_PIC
	   && gotoff_operand (addr, Pmode))
    {
      rtx tmpreg;
      /* This symbol may be referenced via a displacement from the PIC
	 base address (@GOTOFF).  */

      if (reload_in_progress)
	df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
      if (GET_CODE (addr) == CONST)
	addr = XEXP (addr, 0);
      if (GET_CODE (addr) == PLUS)
	  {
            new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, XEXP (addr, 0)),
				      UNSPEC_GOTOFF);
	    new_rtx = gen_rtx_PLUS (Pmode, new_rtx, XEXP (addr, 1));
	  }
	else
          new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTOFF);
      new_rtx = gen_rtx_CONST (Pmode, new_rtx);
      if (!reg)
        tmpreg = gen_reg_rtx (Pmode);
      else
	tmpreg = reg;
      emit_move_insn (tmpreg, new_rtx);

      if (reg != 0)
	{
	  new_rtx = expand_simple_binop (Pmode, PLUS, reg, pic_offset_table_rtx,
					 tmpreg, 1, OPTAB_DIRECT);
	  new_rtx = reg;
	}
      else new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, tmpreg);
    }
  else if (!TARGET_64BIT && gotoff_operand (addr, Pmode))
    {
      /* This symbol may be referenced via a displacement from the PIC
	 base address (@GOTOFF).  */

      if (reload_in_progress)
	df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
      if (GET_CODE (addr) == CONST)
	addr = XEXP (addr, 0);
      if (GET_CODE (addr) == PLUS)
	  {
            new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, XEXP (addr, 0)),
				      UNSPEC_GOTOFF);
	    new_rtx = gen_rtx_PLUS (Pmode, new_rtx, XEXP (addr, 1));
	  }
	else
          new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTOFF);
      new_rtx = gen_rtx_CONST (Pmode, new_rtx);
      new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);

      if (reg != 0)
	{
	  emit_move_insn (reg, new_rtx);
	  new_rtx = reg;
	}
    }
  else if ((GET_CODE (addr) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (addr) == 0)
	   /* We can't use @GOTOFF for text labels on VxWorks;
	      see gotoff_operand.  */
	   || (TARGET_VXWORKS_RTP && GET_CODE (addr) == LABEL_REF))
    {
      /* Given that we've already handled dllimport variables separately
	 in legitimize_address, and all other variables should satisfy
	 legitimate_pic_address_disp_p, we should never arrive here.  */
      gcc_assert (!TARGET_64BIT_MS_ABI);

      if (TARGET_64BIT && ix86_cmodel != CM_LARGE_PIC)
	{
	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTPCREL);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	  new_rtx = gen_const_mem (Pmode, new_rtx);
	  set_mem_alias_set (new_rtx, ix86_GOT_alias_set ());

	  if (reg == 0)
	    reg = gen_reg_rtx (Pmode);
	  /* Use directly gen_movsi, otherwise the address is loaded
	     into register for CSE.  We don't want to CSE this addresses,
	     instead we CSE addresses from the GOT table, so skip this.  */
	  emit_insn (gen_movsi (reg, new_rtx));
	  new_rtx = reg;
	}
      else
	{
	  /* This symbol must be referenced via a load from the
	     Global Offset Table (@GOT).  */

	  if (reload_in_progress)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOT);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	  if (TARGET_64BIT)
	    new_rtx = force_reg (Pmode, new_rtx);
	  new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);
	  new_rtx = gen_const_mem (Pmode, new_rtx);
	  set_mem_alias_set (new_rtx, ix86_GOT_alias_set ());

	  if (reg == 0)
	    reg = gen_reg_rtx (Pmode);
	  emit_move_insn (reg, new_rtx);
	  new_rtx = reg;
	}
    }
  else
    {
      if (CONST_INT_P (addr)
	  && !x86_64_immediate_operand (addr, VOIDmode))
	{
	  if (reg)
	    {
	      emit_move_insn (reg, addr);
	      new_rtx = reg;
	    }
	  else
	    new_rtx = force_reg (Pmode, addr);
	}
      else if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);

	  /* We must match stuff we generate before.  Assume the only
	     unspecs that can get here are ours.  Not that we could do
	     anything with them anyway....  */
	  if (GET_CODE (addr) == UNSPEC
	      || (GET_CODE (addr) == PLUS
		  && GET_CODE (XEXP (addr, 0)) == UNSPEC))
	    return orig;
	  gcc_assert (GET_CODE (addr) == PLUS);
	}
      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0), op1 = XEXP (addr, 1);

	  /* Check first to see if this is a constant offset from a @GOTOFF
	     symbol reference.  */
	  if (gotoff_operand (op0, Pmode)
	      && CONST_INT_P (op1))
	    {
	      if (!TARGET_64BIT)
		{
		  if (reload_in_progress)
		    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
		  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op0),
					    UNSPEC_GOTOFF);
		  new_rtx = gen_rtx_PLUS (Pmode, new_rtx, op1);
		  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
		  new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);

		  if (reg != 0)
		    {
		      emit_move_insn (reg, new_rtx);
		      new_rtx = reg;
		    }
		}
	      else
		{
		  if (INTVAL (op1) < -16*1024*1024
		      || INTVAL (op1) >= 16*1024*1024)
		    {
		      if (!x86_64_immediate_operand (op1, Pmode))
			op1 = force_reg (Pmode, op1);
		      new_rtx = gen_rtx_PLUS (Pmode, force_reg (Pmode, op0), op1);
		    }
		}
	    }
	  else
	    {
	      base = legitimize_pic_address (XEXP (addr, 0), reg);
	      new_rtx  = legitimize_pic_address (XEXP (addr, 1),
						 base == reg ? NULL_RTX : reg);

	      if (CONST_INT_P (new_rtx))
		new_rtx = plus_constant (base, INTVAL (new_rtx));
	      else
		{
		  if (GET_CODE (new_rtx) == PLUS && CONSTANT_P (XEXP (new_rtx, 1)))
		    {
		      base = gen_rtx_PLUS (Pmode, base, XEXP (new_rtx, 0));
		      new_rtx = XEXP (new_rtx, 1);
		    }
		  new_rtx = gen_rtx_PLUS (Pmode, base, new_rtx);
		}
	    }
	}
    }
  return new_rtx;
}

/* Load the thread pointer.  If TO_REG is true, force it into a register.  */

static rtx
get_thread_pointer (int to_reg)
{
  rtx tp, reg, insn;

  tp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TP);
  if (!to_reg)
    return tp;

  reg = gen_reg_rtx (Pmode);
  insn = gen_rtx_SET (VOIDmode, reg, tp);
  insn = emit_insn (insn);

  return reg;
}

/* A subroutine of legitimize_address and ix86_expand_move.  FOR_MOV is
   false if we expect this to be used for a memory address and true if
   we expect to load the address into a register.  */

static rtx
legitimize_tls_address (rtx x, enum tls_model model, int for_mov)
{
  rtx dest, base, off, pic, tp;
  int type;

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      dest = gen_reg_rtx (Pmode);
      tp = TARGET_GNU2_TLS ? get_thread_pointer (1) : 0;

      if (TARGET_64BIT && ! TARGET_GNU2_TLS)
	{
	  rtx rax = gen_rtx_REG (Pmode, AX_REG), insns;

	  start_sequence ();
	  emit_call_insn (gen_tls_global_dynamic_64 (rax, x));
	  insns = get_insns ();
	  end_sequence ();

	  CONST_OR_PURE_CALL_P (insns) = 1;
	  emit_libcall_block (insns, dest, rax, x);
	}
      else if (TARGET_64BIT && TARGET_GNU2_TLS)
	emit_insn (gen_tls_global_dynamic_64 (dest, x));
      else
	emit_insn (gen_tls_global_dynamic_32 (dest, x));

      if (TARGET_GNU2_TLS)
	{
	  dest = force_reg (Pmode, gen_rtx_PLUS (Pmode, tp, dest));

	  set_unique_reg_note (get_last_insn (), REG_EQUIV, x);
	}
      break;

    case TLS_MODEL_LOCAL_DYNAMIC:
      base = gen_reg_rtx (Pmode);
      tp = TARGET_GNU2_TLS ? get_thread_pointer (1) : 0;

      if (TARGET_64BIT && ! TARGET_GNU2_TLS)
	{
	  rtx rax = gen_rtx_REG (Pmode, AX_REG), insns, note;

	  start_sequence ();
	  emit_call_insn (gen_tls_local_dynamic_base_64 (rax));
	  insns = get_insns ();
	  end_sequence ();

	  note = gen_rtx_EXPR_LIST (VOIDmode, const0_rtx, NULL);
	  note = gen_rtx_EXPR_LIST (VOIDmode, ix86_tls_get_addr (), note);
	  CONST_OR_PURE_CALL_P (insns) = 1;
	  emit_libcall_block (insns, base, rax, note);
	}
      else if (TARGET_64BIT && TARGET_GNU2_TLS)
	emit_insn (gen_tls_local_dynamic_base_64 (base));
      else
	emit_insn (gen_tls_local_dynamic_base_32 (base));

      if (TARGET_GNU2_TLS)
	{
	  rtx x = ix86_tls_module_base ();

	  set_unique_reg_note (get_last_insn (), REG_EQUIV,
			       gen_rtx_MINUS (Pmode, x, tp));
	}

      off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), UNSPEC_DTPOFF);
      off = gen_rtx_CONST (Pmode, off);

      dest = force_reg (Pmode, gen_rtx_PLUS (Pmode, base, off));

      if (TARGET_GNU2_TLS)
	{
	  dest = force_reg (Pmode, gen_rtx_PLUS (Pmode, dest, tp));

	  set_unique_reg_note (get_last_insn (), REG_EQUIV, x);
	}

      break;

    case TLS_MODEL_INITIAL_EXEC:
      if (TARGET_64BIT)
	{
	  pic = NULL;
	  type = UNSPEC_GOTNTPOFF;
	}
      else if (flag_pic)
	{
	  if (reload_in_progress)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);
	  pic = pic_offset_table_rtx;
	  type = TARGET_ANY_GNU_TLS ? UNSPEC_GOTNTPOFF : UNSPEC_GOTTPOFF;
	}
      else if (!TARGET_ANY_GNU_TLS)
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

      off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), type);
      off = gen_rtx_CONST (Pmode, off);
      if (pic)
	off = gen_rtx_PLUS (Pmode, pic, off);
      off = gen_const_mem (Pmode, off);
      set_mem_alias_set (off, ix86_GOT_alias_set ());

      if (TARGET_64BIT || TARGET_ANY_GNU_TLS)
	{
          base = get_thread_pointer (for_mov || !TARGET_TLS_DIRECT_SEG_REFS);
	  off = force_reg (Pmode, off);
	  return gen_rtx_PLUS (Pmode, base, off);
	}
      else
	{
	  base = get_thread_pointer (true);
	  dest = gen_reg_rtx (Pmode);
	  emit_insn (gen_subsi3 (dest, base, off));
	}
      break;

    case TLS_MODEL_LOCAL_EXEC:
      off = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x),
			    (TARGET_64BIT || TARGET_ANY_GNU_TLS)
			    ? UNSPEC_NTPOFF : UNSPEC_TPOFF);
      off = gen_rtx_CONST (Pmode, off);

      if (TARGET_64BIT || TARGET_ANY_GNU_TLS)
	{
	  base = get_thread_pointer (for_mov || !TARGET_TLS_DIRECT_SEG_REFS);
	  return gen_rtx_PLUS (Pmode, base, off);
	}
      else
	{
	  base = get_thread_pointer (true);
	  dest = gen_reg_rtx (Pmode);
	  emit_insn (gen_subsi3 (dest, base, off));
	}
      break;

    default:
      gcc_unreachable ();
    }

  return dest;
}

/* Create or return the unique __imp_DECL dllimport symbol corresponding
   to symbol DECL.  */

static GTY((if_marked ("tree_map_marked_p"), param_is (struct tree_map)))
  htab_t dllimport_map;

static tree
get_dllimport_decl (tree decl)
{
  struct tree_map *h, in;
  void **loc;
  const char *name;
  const char *prefix;
  size_t namelen, prefixlen;
  char *imp_name;
  tree to;
  rtx rtl;

  if (!dllimport_map)
    dllimport_map = htab_create_ggc (512, tree_map_hash, tree_map_eq, 0);

  in.hash = htab_hash_pointer (decl);
  in.base.from = decl;
  loc = htab_find_slot_with_hash (dllimport_map, &in, in.hash, INSERT);
  h = (struct tree_map *) *loc;
  if (h)
    return h->to;

  *loc = h = GGC_NEW (struct tree_map);
  h->hash = in.hash;
  h->base.from = decl;
  h->to = to = build_decl (VAR_DECL, NULL, ptr_type_node);
  DECL_ARTIFICIAL (to) = 1;
  DECL_IGNORED_P (to) = 1;
  DECL_EXTERNAL (to) = 1;
  TREE_READONLY (to) = 1;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = targetm.strip_name_encoding (name);
  prefix = name[0] == FASTCALL_PREFIX  ?  "*__imp_": "*__imp__";
  namelen = strlen (name);
  prefixlen = strlen (prefix);
  imp_name = (char *) alloca (namelen + prefixlen + 1);
  memcpy (imp_name, prefix, prefixlen);
  memcpy (imp_name + prefixlen, name, namelen + 1);

  name = ggc_alloc_string (imp_name, namelen + prefixlen);
  rtl = gen_rtx_SYMBOL_REF (Pmode, name);
  SET_SYMBOL_REF_DECL (rtl, to);
  SYMBOL_REF_FLAGS (rtl) = SYMBOL_FLAG_LOCAL;

  rtl = gen_const_mem (Pmode, rtl);
  set_mem_alias_set (rtl, ix86_GOT_alias_set ());

  SET_DECL_RTL (to, rtl);
  SET_DECL_ASSEMBLER_NAME (to, get_identifier (name));

  return to;
}

/* Expand SYMBOL into its corresponding dllimport symbol.  WANT_REG is
   true if we require the result be a register.  */

static rtx
legitimize_dllimport_symbol (rtx symbol, bool want_reg)
{
  tree imp_decl;
  rtx x;

  gcc_assert (SYMBOL_REF_DECL (symbol));
  imp_decl = get_dllimport_decl (SYMBOL_REF_DECL (symbol));

  x = DECL_RTL (imp_decl);
  if (want_reg)
    x = force_reg (Pmode, x);
  return x;
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
legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED, enum machine_mode mode)
{
  int changed = 0;
  unsigned log;

  log = GET_CODE (x) == SYMBOL_REF ? SYMBOL_REF_TLS_MODEL (x) : 0;
  if (log)
    return legitimize_tls_address (x, (enum tls_model) log, false);
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && (log = SYMBOL_REF_TLS_MODEL (XEXP (XEXP (x, 0), 0))))
    {
      rtx t = legitimize_tls_address (XEXP (XEXP (x, 0), 0),
				      (enum tls_model) log, false);
      return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (x, 0), 1));
    }

  if (TARGET_DLLIMPORT_DECL_ATTRIBUTES)
    {
      if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_DLLIMPORT_P (x))
	return legitimize_dllimport_symbol (x, true);
      if (GET_CODE (x) == CONST
	  && GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	  && SYMBOL_REF_DLLIMPORT_P (XEXP (XEXP (x, 0), 0)))
	{
	  rtx t = legitimize_dllimport_symbol (XEXP (XEXP (x, 0), 0), true);
	  return gen_rtx_PLUS (Pmode, t, XEXP (XEXP (x, 0), 1));
	}
    }

  if (flag_pic && SYMBOLIC_CONST (x))
    return legitimize_pic_address (x, 0);

  /* Canonicalize shifts by 0, 1, 2, 3 into multiply */
  if (GET_CODE (x) == ASHIFT
      && CONST_INT_P (XEXP (x, 1))
      && (unsigned HOST_WIDE_INT) INTVAL (XEXP (x, 1)) < 4)
    {
      changed = 1;
      log = INTVAL (XEXP (x, 1));
      x = gen_rtx_MULT (Pmode, force_reg (Pmode, XEXP (x, 0)),
			GEN_INT (1 << log));
    }

  if (GET_CODE (x) == PLUS)
    {
      /* Canonicalize shifts by 0, 1, 2, 3 into multiply.  */

      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && (unsigned HOST_WIDE_INT) INTVAL (XEXP (XEXP (x, 0), 1)) < 4)
	{
	  changed = 1;
	  log = INTVAL (XEXP (XEXP (x, 0), 1));
	  XEXP (x, 0) = gen_rtx_MULT (Pmode,
				      force_reg (Pmode, XEXP (XEXP (x, 0), 0)),
				      GEN_INT (1 << log));
	}

      if (GET_CODE (XEXP (x, 1)) == ASHIFT
	  && CONST_INT_P (XEXP (XEXP (x, 1), 1))
	  && (unsigned HOST_WIDE_INT) INTVAL (XEXP (XEXP (x, 1), 1)) < 4)
	{
	  changed = 1;
	  log = INTVAL (XEXP (XEXP (x, 1), 1));
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

	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      constant = XEXP (x, 1);
	      other = XEXP (XEXP (XEXP (x, 0), 1), 1);
	    }
	  else if (CONST_INT_P (XEXP (XEXP (XEXP (x, 0), 1), 1)))
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
	  && REG_P (XEXP (x, 1))
	  && REG_P (XEXP (x, 0)))
	return x;

      if (flag_pic && SYMBOLIC_CONST (XEXP (x, 1)))
	{
	  changed = 1;
	  x = legitimize_pic_address (x, 0);
	}

      if (changed && legitimate_address_p (mode, x, FALSE))
	return x;

      if (REG_P (XEXP (x, 0)))
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  XEXP (x, 1) = temp;
	  return x;
	}

      else if (REG_P (XEXP (x, 1)))
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val  = force_operand (XEXP (x, 0), temp);
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
output_pic_addr_const (FILE *file, rtx x, int code)
{
  char buf[256];

  switch (GET_CODE (x))
    {
    case PC:
      gcc_assert (flag_pic);
      putc ('.', file);
      break;

    case SYMBOL_REF:
      if (! TARGET_MACHO || TARGET_64BIT)
	output_addr_const (file, x);
      else
	{
	  const char *name = XSTR (x, 0);

	  /* Mark the decl as referenced so that cgraph will
	     output the function.  */
	  if (SYMBOL_REF_DECL (x))
	    mark_decl_referenced (SYMBOL_REF_DECL (x));

#if TARGET_MACHO
	  if (MACHOPIC_INDIRECT
	      && machopic_classify_symbol (x) == MACHOPIC_UNDEFINED_FUNCTION)
	    name = machopic_indirection_name (x, /*stub_p=*/true);
#endif
	  assemble_name (file, name);
	}
      if (!TARGET_MACHO && !TARGET_64BIT_MS_ABI
	  && code == 'P' && ! SYMBOL_REF_LOCAL_P (x))
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
      if (CONST_INT_P (XEXP (x, 0)))
	{
	  output_pic_addr_const (file, XEXP (x, 0), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 1), code);
	}
      else
	{
	  gcc_assert (CONST_INT_P (XEXP (x, 1)));
	  output_pic_addr_const (file, XEXP (x, 1), code);
	  putc ('+', file);
	  output_pic_addr_const (file, XEXP (x, 0), code);
	}
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
       gcc_assert (XVECLEN (x, 0) == 1);
       output_pic_addr_const (file, XVECEXP (x, 0, 0), code);
       switch (XINT (x, 1))
	{
	case UNSPEC_GOT:
	  fputs ("@GOT", file);
	  break;
	case UNSPEC_GOTOFF:
	  fputs ("@GOTOFF", file);
	  break;
	case UNSPEC_PLTOFF:
	  fputs ("@PLTOFF", file);
	  break;
	case UNSPEC_GOTPCREL:
	  fputs (ASSEMBLER_DIALECT == ASM_ATT ?
		 "@GOTPCREL(%rip)" : "@GOTPCREL[rip]", file);
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
	    fputs (ASSEMBLER_DIALECT == ASM_ATT ?
		   "@GOTTPOFF(%rip)": "@GOTTPOFF[rip]", file);
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

/* This is called from dwarf2out.c via TARGET_ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

static void ATTRIBUTE_UNUSED
i386_output_dwarf_dtprel (FILE *file, int size, rtx x)
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
      gcc_unreachable ();
   }
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.

   On Darwin, this is necessary to avoid a crash, because Darwin
   has a different PIC label for each routine but the DWARF debugging
   information is not associated with any particular routine, so it's
   necessary to remove references to the PIC label from RTL stored by
   the DWARF output code.  */

static rtx
ix86_delegitimize_address (rtx orig_x)
{
  rtx x = orig_x;
  /* reg_addend is NULL or a multiple of some register.  */
  rtx reg_addend = NULL_RTX;
  /* const_addend is NULL or a const_int.  */
  rtx const_addend = NULL_RTX;
  /* This is the result, or NULL.  */
  rtx result = NULL_RTX;

  if (MEM_P (x))
    x = XEXP (x, 0);

  if (TARGET_64BIT)
    {
      if (GET_CODE (x) != CONST
	  || GET_CODE (XEXP (x, 0)) != UNSPEC
	  || XINT (XEXP (x, 0), 1) != UNSPEC_GOTPCREL
	  || !MEM_P (orig_x))
	return orig_x;
      return XVECEXP (XEXP (x, 0), 0, 0);
    }

  if (GET_CODE (x) != PLUS
      || GET_CODE (XEXP (x, 1)) != CONST)
    return orig_x;

  if (REG_P (XEXP (x, 0))
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM)
    /* %ebx + GOT/GOTOFF */
    ;
  else if (GET_CODE (XEXP (x, 0)) == PLUS)
    {
      /* %ebx + %reg * scale + GOT/GOTOFF */
      reg_addend = XEXP (x, 0);
      if (REG_P (XEXP (reg_addend, 0))
	  && REGNO (XEXP (reg_addend, 0)) == PIC_OFFSET_TABLE_REGNUM)
	reg_addend = XEXP (reg_addend, 1);
      else if (REG_P (XEXP (reg_addend, 1))
	       && REGNO (XEXP (reg_addend, 1)) == PIC_OFFSET_TABLE_REGNUM)
	reg_addend = XEXP (reg_addend, 0);
      else
	return orig_x;
      if (!REG_P (reg_addend)
	  && GET_CODE (reg_addend) != MULT
	  && GET_CODE (reg_addend) != ASHIFT)
	return orig_x;
    }
  else
    return orig_x;

  x = XEXP (XEXP (x, 1), 0);
  if (GET_CODE (x) == PLUS
      && CONST_INT_P (XEXP (x, 1)))
    {
      const_addend = XEXP (x, 1);
      x = XEXP (x, 0);
    }

  if (GET_CODE (x) == UNSPEC
      && ((XINT (x, 1) == UNSPEC_GOT && MEM_P (orig_x))
	  || (XINT (x, 1) == UNSPEC_GOTOFF && !MEM_P (orig_x))))
    result = XVECEXP (x, 0, 0);

  if (TARGET_MACHO && darwin_local_data_pic (x)
      && !MEM_P (orig_x))
    result = XEXP (x, 0);

  if (! result)
    return orig_x;

  if (const_addend)
    result = gen_rtx_PLUS (Pmode, result, const_addend);
  if (reg_addend)
    result = gen_rtx_PLUS (Pmode, reg_addend, result);
  return result;
}

/* If X is a machine specific address (i.e. a symbol or label being
   referenced as a displacement from the GOT implemented using an
   UNSPEC), then return the base term.  Otherwise return X.  */

rtx
ix86_find_base_term (rtx x)
{
  rtx term;

  if (TARGET_64BIT)
    {
      if (GET_CODE (x) != CONST)
	return x;
      term = XEXP (x, 0);
      if (GET_CODE (term) == PLUS
	  && (CONST_INT_P (XEXP (term, 1))
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

  term = ix86_delegitimize_address (x);

  if (GET_CODE (term) != SYMBOL_REF
      && GET_CODE (term) != LABEL_REF)
    return x;

  return term;
}

static void
put_condition_code (enum rtx_code code, enum machine_mode mode, int reverse,
		    int fp, FILE *file)
{
  const char *suffix;

  if (mode == CCFPmode || mode == CCFPUmode)
    {
      enum rtx_code second_code, bypass_code;
      ix86_fp_comparison_codes (code, &bypass_code, &code, &second_code);
      gcc_assert (bypass_code == UNKNOWN && second_code == UNKNOWN);
      code = ix86_fp_compare_code_to_integer (code);
      mode = CCmode;
    }
  if (reverse)
    code = reverse_condition (code);

  switch (code)
    {
    case EQ:
      switch (mode)
	{
	case CCAmode:
	  suffix = "a";
	  break;

	case CCCmode:
	  suffix = "c";
	  break;

	case CCOmode:
	  suffix = "o";
	  break;

	case CCSmode:
	  suffix = "s";
	  break;

	default:
	  suffix = "e";
	}
      break;
    case NE:
      switch (mode)
	{
	case CCAmode:
	  suffix = "na";
	  break;

	case CCCmode:
	  suffix = "nc";
	  break;

	case CCOmode:
	  suffix = "no";
	  break;

	case CCSmode:
	  suffix = "ns";
	  break;

	default:
	  suffix = "ne";
	}
      break;
    case GT:
      gcc_assert (mode == CCmode || mode == CCNOmode || mode == CCGCmode);
      suffix = "g";
      break;
    case GTU:
      /* ??? Use "nbe" instead of "a" for fcmov lossage on some assemblers.
	 Those same assemblers have the same but opposite lossage on cmov.  */
      if (mode == CCmode)
	suffix = fp ? "nbe" : "a";
      else if (mode == CCCmode)
	suffix = "b";
      else
	gcc_unreachable ();
      break;
    case LT:
      switch (mode)
	{
	case CCNOmode:
	case CCGOCmode:
	  suffix = "s";
	  break;

	case CCmode:
	case CCGCmode:
	  suffix = "l";
	  break;

	default:
	  gcc_unreachable ();
	}
      break;
    case LTU:
      gcc_assert (mode == CCmode || mode == CCCmode);
      suffix = "b";
      break;
    case GE:
      switch (mode)
	{
	case CCNOmode:
	case CCGOCmode:
	  suffix = "ns";
	  break;

	case CCmode:
	case CCGCmode:
	  suffix = "ge";
	  break;

	default:
	  gcc_unreachable ();
	}
      break;
    case GEU:
      /* ??? As above.  */
      gcc_assert (mode == CCmode || mode == CCCmode);
      suffix = fp ? "nb" : "ae";
      break;
    case LE:
      gcc_assert (mode == CCmode || mode == CCGCmode || mode == CCNOmode);
      suffix = "le";
      break;
    case LEU:
      /* ??? As above.  */
      if (mode == CCmode)
	suffix = "be";
      else if (mode == CCCmode)
	suffix = fp ? "nb" : "ae";
      else
	gcc_unreachable ();
      break;
    case UNORDERED:
      suffix = fp ? "u" : "p";
      break;
    case ORDERED:
      suffix = fp ? "nu" : "np";
      break;
    default:
      gcc_unreachable ();
    }
  fputs (suffix, file);
}

/* Print the name of register X to FILE based on its machine mode and number.
   If CODE is 'w', pretend the mode is HImode.
   If CODE is 'b', pretend the mode is QImode.
   If CODE is 'k', pretend the mode is SImode.
   If CODE is 'q', pretend the mode is DImode.
   If CODE is 'h', pretend the reg is the 'high' byte register.
   If CODE is 'y', print "st(0)" instead of "st", if the reg is stack op.  */

void
print_reg (rtx x, int code, FILE *file)
{
  gcc_assert (x == pc_rtx
	      || (REGNO (x) != ARG_POINTER_REGNUM
		  && REGNO (x) != FRAME_POINTER_REGNUM
		  && REGNO (x) != FLAGS_REG
		  && REGNO (x) != FPSR_REG
		  && REGNO (x) != FPCR_REG));

  if (ASSEMBLER_DIALECT == ASM_ATT)
    putc ('%', file);

  if (x == pc_rtx)
    {
      gcc_assert (TARGET_64BIT);
      fputs ("rip", file);
      return;
    }

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
      gcc_assert (TARGET_64BIT);
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
    normal:
      fputs (hi_reg_name[REGNO (x)], file);
      break;
    case 1:
      if (REGNO (x) >= ARRAY_SIZE (qi_reg_name))
	goto normal;
      fputs (qi_reg_name[REGNO (x)], file);
      break;
    case 0:
      if (REGNO (x) >= ARRAY_SIZE (qi_high_reg_name))
	goto normal;
      fputs (qi_high_reg_name[REGNO (x)], file);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Locate some local-dynamic symbol still in use by this function
   so that we can print its name in some tls_local_dynamic_base
   pattern.  */

static int
get_some_local_dynamic_name_1 (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *px;

  if (GET_CODE (x) == SYMBOL_REF
      && SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC)
    {
      cfun->machine->some_ld_name = XSTR (x, 0);
      return 1;
    }

  return 0;
}

static const char *
get_some_local_dynamic_name (void)
{
  rtx insn;

  if (cfun->machine->some_ld_name)
    return cfun->machine->some_ld_name;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& for_each_rtx (&PATTERN (insn), get_some_local_dynamic_name_1, 0))
      return cfun->machine->some_ld_name;

  gcc_unreachable ();
}

/* Meaning of CODE:
   L,W,B,Q,S,T -- print the opcode suffix for specified size of operand.
   C -- print opcode suffix for set/cmov insn.
   c -- like C, but print reversed condition
   F,f -- likewise, but for floating-point.
   O -- if HAVE_AS_IX86_CMOV_SUN_SYNTAX, expand to "w.", "l." or "q.",
        otherwise nothing
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
   H -- print a memory address offset by 8; used for sse high-parts
   Y -- print condition for SSE5 com* instruction.
   + -- print a branch hint as 'cs' or 'ds' prefix
   ; -- print a semicolon (after prefixes due to bug in older gas).
 */

void
print_operand (FILE *file, rtx x, int code)
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
	  switch (ASSEMBLER_DIALECT)
	    {
	    case ASM_ATT:
	      putc ('*', file);
	      break;

	    case ASM_INTEL:
	      /* Intel syntax. For absolute addresses, registers should not
		 be surrounded by braces.  */
	      if (!REG_P (x))
		{
		  putc ('[', file);
		  PRINT_OPERAND (file, x, 0);
		  putc (']', file);
		  return;
		}
	      break;

	    default:
	      gcc_unreachable ();
	    }

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
	    case 1:
	      putc ('b', file);
	      return;

	    case 2:
	      if (MEM_P (x))
		{
#ifdef HAVE_GAS_FILDS_FISTS
		  putc ('s', file);
#endif
		  return;
		}
	      else
		putc ('w', file);
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
	      gcc_unreachable ();
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
	  if (CONST_INT_P (x) || ! SHIFT_DOUBLE_OMITS_COUNT)
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
	      gcc_unreachable ();
	    }
	  return;
	case 'O':
#ifdef HAVE_AS_IX86_CMOV_SUN_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    {
	      switch (GET_MODE (x))
		{
		case HImode: putc ('w', file); break;
		case SImode:
		case SFmode: putc ('l', file); break;
		case DImode:
		case DFmode: putc ('q', file); break;
		default: gcc_unreachable ();
		}
	      putc ('.', file);
	    }
#endif
	  return;
	case 'C':
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 0, 0, file);
	  return;
	case 'F':
#ifdef HAVE_AS_IX86_CMOV_SUN_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('.', file);
#endif
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 0, 1, file);
	  return;

	  /* Like above, but reverse condition */
	case 'c':
	  /* Check to see if argument to %c is really a constant
	     and not a condition code which needs to be reversed.  */
	  if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("operand is neither a constant nor a condition code, invalid operand code 'c'");
	     return;
	  }
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 1, 0, file);
	  return;
	case 'f':
#ifdef HAVE_AS_IX86_CMOV_SUN_SYNTAX
	  if (ASSEMBLER_DIALECT == ASM_ATT)
	    putc ('.', file);
#endif
	  put_condition_code (GET_CODE (x), GET_MODE (XEXP (x, 0)), 1, 1, file);
	  return;

	case 'H':
	  /* It doesn't actually matter what mode we use here, as we're
	     only going to use this for printing.  */
	  x = adjust_address_nv (x, DImode, 8);
	  break;

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
		       heuristics would fail.  */
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

	case 'Y':
	  switch (GET_CODE (x))
	    {
	    case NE:
	      fputs ("neq", file);
	      break;
	    case EQ:
	      fputs ("eq", file);
	      break;
	    case GE:
	    case GEU:
	      fputs (INTEGRAL_MODE_P (GET_MODE (x)) ? "ge" : "unlt", file);
	      break;
	    case GT:
	    case GTU:
	      fputs (INTEGRAL_MODE_P (GET_MODE (x)) ? "gt" : "unle", file);
	      break;
	    case LE:
	    case LEU:
	      fputs ("le", file);
	      break;
	    case LT:
	    case LTU:
	      fputs ("lt", file);
	      break;
	    case UNORDERED:
	      fputs ("unord", file);
	      break;
	    case ORDERED:
	      fputs ("ord", file);
	      break;
	    case UNEQ:
	      fputs ("ueq", file);
	      break;
	    case UNGE:
	      fputs ("nlt", file);
	      break;
	    case UNGT:
	      fputs ("nle", file);
	      break;
	    case UNLE:
	      fputs ("ule", file);
	      break;
	    case UNLT:
	      fputs ("ult", file);
	      break;
	    case LTGT:
	      fputs ("une", file);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  return;

	case ';':
#if TARGET_MACHO
	  fputs (" ; ", file);
#else
	  fputc (' ', file);
#endif
	  return;

	default:
	    output_operand_lossage ("invalid operand code '%c'", code);
	}
    }

  if (REG_P (x))
    print_reg (x, code, file);

  else if (MEM_P (x))
    {
      /* No `byte ptr' prefix for call instructions or BLKmode operands.  */
      if (ASSEMBLER_DIALECT == ASM_INTEL && code != 'X' && code != 'P'
	  && GET_MODE (x) != BLKmode)
	{
	  const char * size;
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 1: size = "BYTE"; break;
	    case 2: size = "WORD"; break;
	    case 4: size = "DWORD"; break;
	    case 8: size = "QWORD"; break;
	    case 12: size = "XWORD"; break;
	    case 16:
	      if (GET_MODE (x) == XFmode)
		size = "XWORD";
              else
		size = "XMMWORD";
              break;
	    default:
	      gcc_unreachable ();
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
      /* Avoid (%rip) for call operands.  */
      if (CONSTANT_ADDRESS_P (x) && code == 'P'
	  && !CONST_INT_P (x))
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
      fprintf (file, "0x%08lx", l);
    }

  /* These float cases don't actually occur as immediate operands.  */
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
    {
      char dstr[30];

      real_to_decimal (dstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (dstr), 0, 1);
      fprintf (file, "%s", dstr);
    }

  else if (GET_CODE (x) == CONST_DOUBLE
	   && GET_MODE (x) == XFmode)
    {
      char dstr[30];

      real_to_decimal (dstr, CONST_DOUBLE_REAL_VALUE (x), sizeof (dstr), 0, 1);
      fprintf (file, "%s", dstr);
    }

  else
    {
      /* We have patterns that allow zero sets of memory, for instance.
	 In 64-bit mode, we should probably support all 8-byte vectors,
	 since we can in fact encode that into an immediate.  */
      if (GET_CODE (x) == CONST_VECTOR)
	{
	  gcc_assert (x == CONST0_RTX (GET_MODE (x)));
	  x = const0_rtx;
	}

      if (code != 'P')
	{
	  if (CONST_INT_P (x) || GET_CODE (x) == CONST_DOUBLE)
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
      if (CONST_INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
      else if (flag_pic)
	output_pic_addr_const (file, x, code);
      else
	output_addr_const (file, x);
    }
}

/* Print a memory operand whose address is ADDR.  */

void
print_operand_address (FILE *file, rtx addr)
{
  struct ix86_address parts;
  rtx base, index, disp;
  int scale;
  int ok = ix86_decompose_address (addr, &parts);

  gcc_assert (ok);

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  scale = parts.scale;

  switch (parts.seg)
    {
    case SEG_DEFAULT:
      break;
    case SEG_FS:
    case SEG_GS:
      if (ASSEMBLER_DIALECT == ASM_ATT)
	putc ('%', file);
      fputs ((parts.seg == SEG_FS ? "fs:" : "gs:"), file);
      break;
    default:
      gcc_unreachable ();
    }

  /* Use one byte shorter RIP relative addressing for 64bit mode.  */
  if (TARGET_64BIT && !base && !index)
    {
      rtx symbol = disp;

      if (GET_CODE (disp) == CONST
	  && GET_CODE (XEXP (disp, 0)) == PLUS
	  && CONST_INT_P (XEXP (XEXP (disp, 0), 1)))
	symbol = XEXP (XEXP (disp, 0), 0);

      if (GET_CODE (symbol) == LABEL_REF
	  || (GET_CODE (symbol) == SYMBOL_REF
	      && SYMBOL_REF_TLS_MODEL (symbol) == 0))
	base = pc_rtx;
    }
  if (!base && !index)
    {
      /* Displacement only requires special attention.  */

      if (CONST_INT_P (disp))
	{
	  if (ASSEMBLER_DIALECT == ASM_INTEL && parts.seg == SEG_DEFAULT)
	    fputs ("ds:", file);
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (disp));
	}
      else if (flag_pic)
	output_pic_addr_const (file, disp, 0);
      else
	output_addr_const (file, disp);
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
	    print_reg (base, 0, file);
	  if (index)
	    {
	      putc (',', file);
	      print_reg (index, 0, file);
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
		  && CONST_INT_P (XEXP (XEXP (disp, 0), 1)))
		{
		  offset = XEXP (XEXP (disp, 0), 1);
		  disp = gen_rtx_CONST (VOIDmode,
					XEXP (XEXP (disp, 0), 0));
		}

	      if (flag_pic)
		output_pic_addr_const (file, disp, 0);
	      else if (GET_CODE (disp) == LABEL_REF)
		output_asm_label (disp);
	      else if (CONST_INT_P (disp))
		offset = disp;
	      else
		output_addr_const (file, disp);
	    }

	  putc ('[', file);
	  if (base)
	    {
	      print_reg (base, 0, file);
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
	      print_reg (index, 0, file);
	      if (scale != 1)
		fprintf (file, "*%d", scale);
	    }
	  putc (']', file);
	}
    }
}

bool
output_addr_const_extra (FILE *file, rtx x)
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
	fputs (ASSEMBLER_DIALECT == ASM_ATT ?
	       "@GOTTPOFF(%rip)" : "@GOTTPOFF[rip]", file);
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
split_di (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses,
         but we still have to handle it.  */
      if (MEM_P (op))
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
/* Split one or more TImode RTL references into pairs of DImode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of DImode RTL to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_ti (rtx operands[], int num, rtx lo_half[], rtx hi_half[])
{
  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses, but we
         still have to handle it.  */
      if (MEM_P (op))
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
output_387_binary_op (rtx insn, rtx *operands)
{
  static char buf[30];
  const char *p;
  const char *ssep;
  int is_sse = SSE_REG_P (operands[0]) || SSE_REG_P (operands[1]) || SSE_REG_P (operands[2]);

#ifdef ENABLE_CHECKING
  /* Even if we do not want to check the inputs, this documents input
     constraints.  Which helps in understanding the following code.  */
  if (STACK_REG_P (operands[0])
      && ((REG_P (operands[1])
	   && REGNO (operands[0]) == REGNO (operands[1])
	   && (STACK_REG_P (operands[2]) || MEM_P (operands[2])))
	  || (REG_P (operands[2])
	      && REGNO (operands[0]) == REGNO (operands[2])
	      && (STACK_REG_P (operands[1]) || MEM_P (operands[1]))))
      && (STACK_TOP_P (operands[1]) || STACK_TOP_P (operands[2])))
    ; /* ok */
  else
    gcc_assert (is_sse);
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
      gcc_unreachable ();
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

      if (MEM_P (operands[2]))
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
      if (MEM_P (operands[1]))
	{
	  p = "r%z1\t%1";
	  break;
	}

      if (MEM_P (operands[2]))
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
      gcc_unreachable ();
    }

  strcat (buf, p);
  return buf;
}

/* Return needed mode for entity in optimize_mode_switching pass.  */

int
ix86_mode_needed (int entity, rtx insn)
{
  enum attr_i387_cw mode;

  /* The mode UNINITIALIZED is used to store control word after a
     function call or ASM pattern.  The mode ANY specify that function
     has no requirements on the control word and make no changes in the
     bits we are interested in.  */

  if (CALL_P (insn)
      || (NONJUMP_INSN_P (insn)
	  && (asm_noperands (PATTERN (insn)) >= 0
	      || GET_CODE (PATTERN (insn)) == ASM_INPUT)))
    return I387_CW_UNINITIALIZED;

  if (recog_memoized (insn) < 0)
    return I387_CW_ANY;

  mode = get_attr_i387_cw (insn);

  switch (entity)
    {
    case I387_TRUNC:
      if (mode == I387_CW_TRUNC)
	return mode;
      break;

    case I387_FLOOR:
      if (mode == I387_CW_FLOOR)
	return mode;
      break;

    case I387_CEIL:
      if (mode == I387_CW_CEIL)
	return mode;
      break;

    case I387_MASK_PM:
      if (mode == I387_CW_MASK_PM)
	return mode;
      break;

    default:
      gcc_unreachable ();
    }

  return I387_CW_ANY;
}

/* Output code to initialize control word copies used by trunc?f?i and
   rounding patterns.  CURRENT_MODE is set to current control word,
   while NEW_MODE is set to new control word.  */

void
emit_i387_cw_initialization (int mode)
{
  rtx stored_mode = assign_386_stack_local (HImode, SLOT_CW_STORED);
  rtx new_mode;

  enum ix86_stack_slot slot;

  rtx reg = gen_reg_rtx (HImode);

  emit_insn (gen_x86_fnstcw_1 (stored_mode));
  emit_move_insn (reg, copy_rtx (stored_mode));

  if (TARGET_64BIT || TARGET_PARTIAL_REG_STALL || optimize_size)
    {
      switch (mode)
	{
	case I387_CW_TRUNC:
	  /* round toward zero (truncate) */
	  emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0c00)));
	  slot = SLOT_CW_TRUNC;
	  break;

	case I387_CW_FLOOR:
	  /* round down toward -oo */
	  emit_insn (gen_andhi3 (reg, reg, GEN_INT (~0x0c00)));
	  emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0400)));
	  slot = SLOT_CW_FLOOR;
	  break;

	case I387_CW_CEIL:
	  /* round up toward +oo */
	  emit_insn (gen_andhi3 (reg, reg, GEN_INT (~0x0c00)));
	  emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0800)));
	  slot = SLOT_CW_CEIL;
	  break;

	case I387_CW_MASK_PM:
	  /* mask precision exception for nearbyint() */
	  emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0020)));
	  slot = SLOT_CW_MASK_PM;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  else
    {
      switch (mode)
	{
	case I387_CW_TRUNC:
	  /* round toward zero (truncate) */
	  emit_insn (gen_movsi_insv_1 (reg, GEN_INT (0xc)));
	  slot = SLOT_CW_TRUNC;
	  break;

	case I387_CW_FLOOR:
	  /* round down toward -oo */
	  emit_insn (gen_movsi_insv_1 (reg, GEN_INT (0x4)));
	  slot = SLOT_CW_FLOOR;
	  break;

	case I387_CW_CEIL:
	  /* round up toward +oo */
	  emit_insn (gen_movsi_insv_1 (reg, GEN_INT (0x8)));
	  slot = SLOT_CW_CEIL;
	  break;

	case I387_CW_MASK_PM:
	  /* mask precision exception for nearbyint() */
	  emit_insn (gen_iorhi3 (reg, reg, GEN_INT (0x0020)));
	  slot = SLOT_CW_MASK_PM;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  gcc_assert (slot < MAX_386_STACK_LOCALS);

  new_mode = assign_386_stack_local (HImode, slot);
  emit_move_insn (new_mode, reg);
}

/* Output code for INSN to convert a float to a signed int.  OPERANDS
   are the insn operands.  The output may be [HSD]Imode and the input
   operand may be [SDX]Fmode.  */

const char *
output_fix_trunc (rtx insn, rtx *operands, int fisttp)
{
  int stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;
  int dimode_p = GET_MODE (operands[0]) == DImode;
  int round_mode = get_attr_i387_cw (insn);

  /* Jump through a hoop or two for DImode, since the hardware has no
     non-popping instruction.  We used to do this a different way, but
     that was somewhat fragile and broke with post-reload splitters.  */
  if ((dimode_p || fisttp) && !stack_top_dies)
    output_asm_insn ("fld\t%y1", operands);

  gcc_assert (STACK_TOP_P (operands[1]));
  gcc_assert (MEM_P (operands[0]));
  gcc_assert (GET_MODE (operands[1]) != TFmode);

  if (fisttp)
      output_asm_insn ("fisttp%z0\t%0", operands);
  else
    {
      if (round_mode != I387_CW_ANY)
	output_asm_insn ("fldcw\t%3", operands);
      if (stack_top_dies || dimode_p)
	output_asm_insn ("fistp%z0\t%0", operands);
      else
	output_asm_insn ("fist%z0\t%0", operands);
      if (round_mode != I387_CW_ANY)
	output_asm_insn ("fldcw\t%2", operands);
    }

  return "";
}

/* Output code for x87 ffreep insn.  The OPNO argument, which may only
   have the values zero or one, indicates the ffreep insn's operand
   from the OPERANDS array.  */

static const char *
output_387_ffreep (rtx *operands ATTRIBUTE_UNUSED, int opno)
{
  if (TARGET_USE_FFREEP)
#if HAVE_AS_IX86_FFREEP
    return opno ? "ffreep\t%y1" : "ffreep\t%y0";
#else
    {
      static char retval[] = ".word\t0xc_df";
      int regno = REGNO (operands[opno]);

      gcc_assert (FP_REGNO_P (regno));

      retval[9] = '0' + (regno - FIRST_STACK_REG);
      return retval;
    }
#endif

  return opno ? "fstp\t%y1" : "fstp\t%y0";
}


/* Output code for INSN to compare OPERANDS.  EFLAGS_P is 1 when fcomi
   should be used.  UNORDERED_P is true when fucom should be used.  */

const char *
output_fp_compare (rtx insn, rtx *operands, int eflags_p, int unordered_p)
{
  int stack_top_dies;
  rtx cmp_op0, cmp_op1;
  int is_sse = SSE_REG_P (operands[0]) || SSE_REG_P (operands[1]);

  if (eflags_p)
    {
      cmp_op0 = operands[0];
      cmp_op1 = operands[1];
    }
  else
    {
      cmp_op0 = operands[1];
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

  gcc_assert (STACK_TOP_P (cmp_op0));

  stack_top_dies = find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != 0;

  if (cmp_op1 == CONST0_RTX (GET_MODE (cmp_op1)))
    {
      if (stack_top_dies)
	{
	  output_asm_insn ("ftst\n\tfnstsw\t%0", operands);
	  return output_387_ffreep (operands, 1);
	}
      else
	return "ftst\n\tfnstsw\t%0";
    }

  if (STACK_REG_P (cmp_op1)
      && stack_top_dies
      && find_regno_note (insn, REG_DEAD, REGNO (cmp_op1))
      && REGNO (cmp_op1) != FIRST_STACK_REG)
    {
      /* If both the top of the 387 stack dies, and the other operand
	 is also a stack register that dies, then this must be a
	 `fcompp' float compare */

      if (eflags_p)
	{
	  /* There is no double popping fcomi variant.  Fortunately,
	     eflags is immune from the fstp's cc clobbering.  */
	  if (unordered_p)
	    output_asm_insn ("fucomip\t{%y1, %0|%0, %y1}", operands);
	  else
	    output_asm_insn ("fcomip\t{%y1, %0|%0, %y1}", operands);
	  return output_387_ffreep (operands, 0);
	}
      else
	{
	  if (unordered_p)
	    return "fucompp\n\tfnstsw\t%0";
	  else
	    return "fcompp\n\tfnstsw\t%0";
	}
    }
  else
    {
      /* Encoded here as eflags_p | intmode | unordered_p | stack_top_dies.  */

      static const char * const alt[16] =
      {
	"fcom%z2\t%y2\n\tfnstsw\t%0",
	"fcomp%z2\t%y2\n\tfnstsw\t%0",
	"fucom%z2\t%y2\n\tfnstsw\t%0",
	"fucomp%z2\t%y2\n\tfnstsw\t%0",

	"ficom%z2\t%y2\n\tfnstsw\t%0",
	"ficomp%z2\t%y2\n\tfnstsw\t%0",
	NULL,
	NULL,

	"fcomi\t{%y1, %0|%0, %y1}",
	"fcomip\t{%y1, %0|%0, %y1}",
	"fucomi\t{%y1, %0|%0, %y1}",
	"fucomip\t{%y1, %0|%0, %y1}",

	NULL,
	NULL,
	NULL,
	NULL
      };

      int mask;
      const char *ret;

      mask  = eflags_p << 3;
      mask |= (GET_MODE_CLASS (GET_MODE (cmp_op1)) == MODE_INT) << 2;
      mask |= unordered_p << 1;
      mask |= stack_top_dies;

      gcc_assert (mask < 16);
      ret = alt[mask];
      gcc_assert (ret);

      return ret;
    }
}

void
ix86_output_addr_vec_elt (FILE *file, int value)
{
  const char *directive = ASM_LONG;

#ifdef ASM_QUAD
  if (TARGET_64BIT)
    directive = ASM_QUAD;
#else
  gcc_assert (!TARGET_64BIT);
#endif

  fprintf (file, "%s%s%d\n", directive, LPREFIX, value);
}

void
ix86_output_addr_diff_elt (FILE *file, int value, int rel)
{
  const char *directive = ASM_LONG;

#ifdef ASM_QUAD
  if (TARGET_64BIT && CASE_VECTOR_MODE == DImode)
    directive = ASM_QUAD;
#else
  gcc_assert (!TARGET_64BIT);
#endif
  /* We can't use @GOTOFF for text labels on VxWorks; see gotoff_operand.  */
  if (TARGET_64BIT || TARGET_VXWORKS_RTP)
    fprintf (file, "%s%s%d-%s%d\n",
	     directive, LPREFIX, value, LPREFIX, rel);
  else if (HAVE_AS_GOTOFF_IN_DATA)
    fprintf (file, "%s%s%d@GOTOFF\n", ASM_LONG, LPREFIX, value);
#if TARGET_MACHO
  else if (TARGET_MACHO)
    {
      fprintf (file, "%s%s%d-", ASM_LONG, LPREFIX, value);
      machopic_output_function_base_name (file);
      fprintf(file, "\n");
    }
#endif
  else
    asm_fprintf (file, "%s%U%s+[.-%s%d]\n",
		 ASM_LONG, GOT_SYMBOL_NAME, LPREFIX, value);
}

/* Generate either "mov $0, reg" or "xor reg, reg", as appropriate
   for the target.  */

void
ix86_expand_clear (rtx dest)
{
  rtx tmp;

  /* We play register width games, which are only valid after reload.  */
  gcc_assert (reload_completed);

  /* Avoid HImode and its attendant prefix byte.  */
  if (GET_MODE_SIZE (GET_MODE (dest)) < 4)
    dest = gen_rtx_REG (SImode, REGNO (dest));
  tmp = gen_rtx_SET (VOIDmode, dest, const0_rtx);

  /* This predicate should match that for movsi_xor and movdi_xor_rex64.  */
  if (reload_completed && (!TARGET_USE_MOV0 || optimize_size))
    {
      rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, clob));
    }

  emit_insn (tmp);
}

/* X is an unchanging MEM.  If it is a constant pool reference, return
   the constant pool rtx, else NULL.  */

rtx
maybe_get_pool_constant (rtx x)
{
  x = ix86_delegitimize_address (XEXP (x, 0));

  if (GET_CODE (x) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (x))
    return get_pool_constant (x);

  return NULL_RTX;
}

void
ix86_expand_move (enum machine_mode mode, rtx operands[])
{
  rtx op0, op1;
  enum tls_model model;

  op0 = operands[0];
  op1 = operands[1];

  if (GET_CODE (op1) == SYMBOL_REF)
    {
      model = SYMBOL_REF_TLS_MODEL (op1);
      if (model)
	{
	  op1 = legitimize_tls_address (op1, model, true);
	  op1 = force_operand (op1, op0);
	  if (op1 == op0)
	    return;
	}
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && SYMBOL_REF_DLLIMPORT_P (op1))
	op1 = legitimize_dllimport_symbol (op1, false);
    }
  else if (GET_CODE (op1) == CONST
	   && GET_CODE (XEXP (op1, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (op1, 0), 0)) == SYMBOL_REF)
    {
      rtx addend = XEXP (XEXP (op1, 0), 1);
      rtx symbol = XEXP (XEXP (op1, 0), 0);
      rtx tmp = NULL;

      model = SYMBOL_REF_TLS_MODEL (symbol);
      if (model)
	tmp = legitimize_tls_address (symbol, model, true);
      else if (TARGET_DLLIMPORT_DECL_ATTRIBUTES
	       && SYMBOL_REF_DLLIMPORT_P (symbol))
	tmp = legitimize_dllimport_symbol (symbol, true);

      if (tmp)
	{
	  tmp = force_operand (tmp, NULL);
	  tmp = expand_simple_binop (Pmode, PLUS, tmp, addend,
				     op0, 1, OPTAB_DIRECT);
	  if (tmp == op0)
	    return;
	}
    }

  if (flag_pic && mode == Pmode && symbolic_operand (op1, Pmode))
    {
      if (TARGET_MACHO && !TARGET_64BIT)
	{
#if TARGET_MACHO
	  if (MACHOPIC_PURE)
	    {
	      rtx temp = ((reload_in_progress
			   || ((op0 && REG_P (op0))
			       && mode == Pmode))
			  ? op0 : gen_reg_rtx (Pmode));
	      op1 = machopic_indirect_data_reference (op1, temp);
	      op1 = machopic_legitimize_pic_address (op1, mode,
						     temp == op1 ? 0 : temp);
	    }
	  else if (MACHOPIC_INDIRECT)
	    op1 = machopic_indirect_data_reference (op1, 0);
	  if (op0 == op1)
	    return;
#endif
	}
      else
	{
	  if (MEM_P (op0))
	    op1 = force_reg (Pmode, op1);
	  else if (!TARGET_64BIT || !x86_64_movabs_operand (op1, Pmode))
	    {
	      rtx reg = !can_create_pseudo_p () ? op0 : NULL_RTX;
	      op1 = legitimize_pic_address (op1, reg);
	      if (op0 == op1)
		return;
	    }
	}
    }
  else
    {
      if (MEM_P (op0)
	  && (PUSH_ROUNDING (GET_MODE_SIZE (mode)) != GET_MODE_SIZE (mode)
	      || !push_operand (op0, mode))
	  && MEM_P (op1))
	op1 = force_reg (mode, op1);

      if (push_operand (op0, mode)
	  && ! general_no_elim_operand (op1, mode))
	op1 = copy_to_mode_reg (mode, op1);

      /* Force large constants in 64bit compilation into register
	 to get them CSEed.  */
      if (can_create_pseudo_p ()
	  && (mode == DImode) && TARGET_64BIT
	  && immediate_operand (op1, mode)
	  && !x86_64_zext_immediate_operand (op1, VOIDmode)
	  && !register_operand (op0, mode)
	  && optimize)
	op1 = copy_to_mode_reg (mode, op1);

      if (can_create_pseudo_p ()
	  && FLOAT_MODE_P (mode)
	  && GET_CODE (op1) == CONST_DOUBLE)
	{
	  /* If we are loading a floating point constant to a register,
	     force the value to memory now, since we'll get better code
	     out the back end.  */

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

  emit_insn (gen_rtx_SET (VOIDmode, op0, op1));
}

void
ix86_expand_vector_move (enum machine_mode mode, rtx operands[])
{
  rtx op0 = operands[0], op1 = operands[1];
  unsigned int align = GET_MODE_ALIGNMENT (mode);

  /* Force constants other than zero into memory.  We do not know how
     the instructions used to build constants modify the upper 64 bits
     of the register, once we have that information we may be able
     to handle some of them more efficiently.  */
  if (can_create_pseudo_p ()
      && register_operand (op0, mode)
      && (CONSTANT_P (op1)
	  || (GET_CODE (op1) == SUBREG
	      && CONSTANT_P (SUBREG_REG (op1))))
      && standard_sse_constant_p (op1) <= 0)
    op1 = validize_mem (force_const_mem (mode, op1));

  /* TDmode values are passed as TImode on the stack.  TImode values
     are moved via xmm registers, and moving them to stack can result in
     unaligned memory access.  Use ix86_expand_vector_move_misalign()
     if memory operand is not aligned correctly.  */
  if (can_create_pseudo_p ()
      && (mode == TImode) && !TARGET_64BIT
      && ((MEM_P (op0) && (MEM_ALIGN (op0) < align))
	  || (MEM_P (op1) && (MEM_ALIGN (op1) < align))))
    {
      rtx tmp[2];

      /* ix86_expand_vector_move_misalign() does not like constants ... */
      if (CONSTANT_P (op1)
	  || (GET_CODE (op1) == SUBREG
	      && CONSTANT_P (SUBREG_REG (op1))))
	op1 = validize_mem (force_const_mem (mode, op1));

      /* ... nor both arguments in memory.  */
      if (!register_operand (op0, mode)
	  && !register_operand (op1, mode))
	op1 = force_reg (mode, op1);

      tmp[0] = op0; tmp[1] = op1;
      ix86_expand_vector_move_misalign (mode, tmp);
      return;
    }

  /* Make operand1 a register if it isn't already.  */
  if (can_create_pseudo_p ()
      && !register_operand (op0, mode)
      && !register_operand (op1, mode))
    {
      emit_move_insn (op0, force_reg (GET_MODE (op0), op1));
      return;
    }

  emit_insn (gen_rtx_SET (VOIDmode, op0, op1));
}

/* Implement the movmisalign patterns for SSE.  Non-SSE modes go
   straight to ix86_expand_vector_move.  */
/* Code generation for scalar reg-reg moves of single and double precision data:
     if (x86_sse_partial_reg_dependency == true | x86_sse_split_regs == true)
       movaps reg, reg
     else
       movss reg, reg
     if (x86_sse_partial_reg_dependency == true)
       movapd reg, reg
     else
       movsd reg, reg

   Code generation for scalar loads of double precision data:
     if (x86_sse_split_regs == true)
       movlpd mem, reg      (gas syntax)
     else
       movsd mem, reg

   Code generation for unaligned packed loads of single precision data
   (x86_sse_unaligned_move_optimal overrides x86_sse_partial_reg_dependency):
     if (x86_sse_unaligned_move_optimal)
       movups mem, reg

     if (x86_sse_partial_reg_dependency == true)
       {
         xorps  reg, reg
         movlps mem, reg
         movhps mem+8, reg
       }
     else
       {
         movlps mem, reg
         movhps mem+8, reg
       }

   Code generation for unaligned packed loads of double precision data
   (x86_sse_unaligned_move_optimal overrides x86_sse_split_regs):
     if (x86_sse_unaligned_move_optimal)
       movupd mem, reg

     if (x86_sse_split_regs == true)
       {
         movlpd mem, reg
         movhpd mem+8, reg
       }
     else
       {
         movsd  mem, reg
         movhpd mem+8, reg
       }
 */

void
ix86_expand_vector_move_misalign (enum machine_mode mode, rtx operands[])
{
  rtx op0, op1, m;

  op0 = operands[0];
  op1 = operands[1];

  if (MEM_P (op1))
    {
      /* If we're optimizing for size, movups is the smallest.  */
      if (optimize_size)
	{
	  op0 = gen_lowpart (V4SFmode, op0);
	  op1 = gen_lowpart (V4SFmode, op1);
	  emit_insn (gen_sse_movups (op0, op1));
	  return;
	}

      /* ??? If we have typed data, then it would appear that using
	 movdqu is the only way to get unaligned data loaded with
	 integer type.  */
      if (TARGET_SSE2 && GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	{
	  op0 = gen_lowpart (V16QImode, op0);
	  op1 = gen_lowpart (V16QImode, op1);
	  emit_insn (gen_sse2_movdqu (op0, op1));
	  return;
	}

      if (TARGET_SSE2 && mode == V2DFmode)
        {
          rtx zero;

          if (TARGET_SSE_UNALIGNED_MOVE_OPTIMAL)
            {
              op0 = gen_lowpart (V2DFmode, op0);
              op1 = gen_lowpart (V2DFmode, op1);
              emit_insn (gen_sse2_movupd (op0, op1));
              return;
            }

	  /* When SSE registers are split into halves, we can avoid
	     writing to the top half twice.  */
	  if (TARGET_SSE_SPLIT_REGS)
	    {
	      emit_insn (gen_rtx_CLOBBER (VOIDmode, op0));
	      zero = op0;
	    }
	  else
	    {
	      /* ??? Not sure about the best option for the Intel chips.
		 The following would seem to satisfy; the register is
		 entirely cleared, breaking the dependency chain.  We
		 then store to the upper half, with a dependency depth
		 of one.  A rumor has it that Intel recommends two movsd
		 followed by an unpacklpd, but this is unconfirmed.  And
		 given that the dependency depth of the unpacklpd would
		 still be one, I'm not sure why this would be better.  */
	      zero = CONST0_RTX (V2DFmode);
	    }

	  m = adjust_address (op1, DFmode, 0);
	  emit_insn (gen_sse2_loadlpd (op0, zero, m));
	  m = adjust_address (op1, DFmode, 8);
	  emit_insn (gen_sse2_loadhpd (op0, op0, m));
	}
      else
        {
          if (TARGET_SSE_UNALIGNED_MOVE_OPTIMAL)
            {
              op0 = gen_lowpart (V4SFmode, op0);
              op1 = gen_lowpart (V4SFmode, op1);
              emit_insn (gen_sse_movups (op0, op1));
              return;
            }

	  if (TARGET_SSE_PARTIAL_REG_DEPENDENCY)
	    emit_move_insn (op0, CONST0_RTX (mode));
	  else
	    emit_insn (gen_rtx_CLOBBER (VOIDmode, op0));

	  if (mode != V4SFmode)
	    op0 = gen_lowpart (V4SFmode, op0);
	  m = adjust_address (op1, V2SFmode, 0);
	  emit_insn (gen_sse_loadlps (op0, op0, m));
	  m = adjust_address (op1, V2SFmode, 8);
	  emit_insn (gen_sse_loadhps (op0, op0, m));
	}
    }
  else if (MEM_P (op0))
    {
      /* If we're optimizing for size, movups is the smallest.  */
      if (optimize_size)
	{
	  op0 = gen_lowpart (V4SFmode, op0);
	  op1 = gen_lowpart (V4SFmode, op1);
	  emit_insn (gen_sse_movups (op0, op1));
	  return;
	}

      /* ??? Similar to above, only less clear because of quote
	 typeless stores unquote.  */
      if (TARGET_SSE2 && !TARGET_SSE_TYPELESS_STORES
	  && GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
        {
	  op0 = gen_lowpart (V16QImode, op0);
	  op1 = gen_lowpart (V16QImode, op1);
	  emit_insn (gen_sse2_movdqu (op0, op1));
	  return;
	}

      if (TARGET_SSE2 && mode == V2DFmode)
	{
	  m = adjust_address (op0, DFmode, 0);
	  emit_insn (gen_sse2_storelpd (m, op1));
	  m = adjust_address (op0, DFmode, 8);
	  emit_insn (gen_sse2_storehpd (m, op1));
	}
      else
	{
	  if (mode != V4SFmode)
	    op1 = gen_lowpart (V4SFmode, op1);
	  m = adjust_address (op0, V2SFmode, 0);
	  emit_insn (gen_sse_storelps (m, op1));
	  m = adjust_address (op0, V2SFmode, 8);
	  emit_insn (gen_sse_storehps (m, op1));
	}
    }
  else
    gcc_unreachable ();
}

/* Expand a push in MODE.  This is some mode for which we do not support
   proper push instructions, at least from the registers that we expect
   the value to live in.  */

void
ix86_expand_push (enum machine_mode mode, rtx x)
{
  rtx tmp;

  tmp = expand_simple_binop (Pmode, PLUS, stack_pointer_rtx,
			     GEN_INT (-GET_MODE_SIZE (mode)),
			     stack_pointer_rtx, 1, OPTAB_DIRECT);
  if (tmp != stack_pointer_rtx)
    emit_move_insn (stack_pointer_rtx, tmp);

  tmp = gen_rtx_MEM (mode, stack_pointer_rtx);
  emit_move_insn (tmp, x);
}

/* Helper function of ix86_fixup_binary_operands to canonicalize
   operand order.  Returns true if the operands should be swapped.  */

static bool
ix86_swap_binary_operands_p (enum rtx_code code, enum machine_mode mode,
			     rtx operands[])
{
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];

  /* If the operation is not commutative, we can't do anything.  */
  if (GET_RTX_CLASS (code) != RTX_COMM_ARITH)
    return false;

  /* Highest priority is that src1 should match dst.  */
  if (rtx_equal_p (dst, src1))
    return false;
  if (rtx_equal_p (dst, src2))
    return true;

  /* Next highest priority is that immediate constants come second.  */
  if (immediate_operand (src2, mode))
    return false;
  if (immediate_operand (src1, mode))
    return true;

  /* Lowest priority is that memory references should come second.  */
  if (MEM_P (src2))
    return false;
  if (MEM_P (src1))
    return true;

  return false;
}


/* Fix up OPERANDS to satisfy ix86_binary_operator_ok.  Return the
   destination to use for the operation.  If different from the true
   destination in operands[0], a copy operation will be required.  */

rtx
ix86_fixup_binary_operands (enum rtx_code code, enum machine_mode mode,
			    rtx operands[])
{
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];

  /* Canonicalize operand order.  */
  if (ix86_swap_binary_operands_p (code, mode, operands))
    {
      rtx temp = src1;
      src1 = src2;
      src2 = temp;
    }

  /* Both source operands cannot be in memory.  */
  if (MEM_P (src1) && MEM_P (src2))
    {
      /* Optimization: Only read from memory once.  */
      if (rtx_equal_p (src1, src2))
	{
	  src2 = force_reg (mode, src2);
	  src1 = src2;
	}
      else
	src2 = force_reg (mode, src2);
    }

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  if (MEM_P (dst) && !rtx_equal_p (dst, src1))
    dst = gen_reg_rtx (mode);

  /* Source 1 cannot be a constant.  */
  if (CONSTANT_P (src1))
    src1 = force_reg (mode, src1);

  /* Source 1 cannot be a non-matching memory.  */
  if (MEM_P (src1) && !rtx_equal_p (dst, src1))
    src1 = force_reg (mode, src1);

  operands[1] = src1;
  operands[2] = src2;
  return dst;
}

/* Similarly, but assume that the destination has already been
   set up properly.  */

void
ix86_fixup_binary_operands_no_copy (enum rtx_code code,
				    enum machine_mode mode, rtx operands[])
{
  rtx dst = ix86_fixup_binary_operands (code, mode, operands);
  gcc_assert (dst == operands[0]);
}

/* Attempt to expand a binary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 3 separate
   memory references (one output, two input) in a single insn.  */

void
ix86_expand_binary_operator (enum rtx_code code, enum machine_mode mode,
			     rtx operands[])
{
  rtx src1, src2, dst, op, clob;

  dst = ix86_fixup_binary_operands (code, mode, operands);
  src1 = operands[1];
  src2 = operands[2];

 /* Emit the instruction.  */

  op = gen_rtx_SET (VOIDmode, dst, gen_rtx_fmt_ee (code, mode, src1, src2));
  if (reload_in_progress)
    {
      /* Reload doesn't know about the flags register, and doesn't know that
         it doesn't want to clobber it.  We can only do this with PLUS.  */
      gcc_assert (code == PLUS);
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
ix86_binary_operator_ok (enum rtx_code code, enum machine_mode mode,
			 rtx operands[3])
{
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];

  /* Both source operands cannot be in memory.  */
  if (MEM_P (src1) && MEM_P (src2))
    return 0;

  /* Canonicalize operand order for commutative operators.  */
  if (ix86_swap_binary_operands_p (code, mode, operands))
    {
      rtx temp = src1;
      src1 = src2;
      src2 = temp;
    }

  /* If the destination is memory, we must have a matching source operand.  */
  if (MEM_P (dst) && !rtx_equal_p (dst, src1))
      return 0;

  /* Source 1 cannot be a constant.  */
  if (CONSTANT_P (src1))
    return 0;

  /* Source 1 cannot be a non-matching memory.  */
  if (MEM_P (src1) && !rtx_equal_p (dst, src1))
    return 0;

  return 1;
}

/* Attempt to expand a unary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 2 separate
   memory references (one output, one input) in a single insn.  */

void
ix86_expand_unary_operator (enum rtx_code code, enum machine_mode mode,
			    rtx operands[])
{
  int matching_memory;
  rtx src, dst, op, clob;

  dst = operands[0];
  src = operands[1];

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  matching_memory = 0;
  if (MEM_P (dst))
    {
      if (rtx_equal_p (dst, src))
	matching_memory = 1;
      else
	dst = gen_reg_rtx (mode);
    }

  /* When source operand is memory, destination must match.  */
  if (MEM_P (src) && !matching_memory)
    src = force_reg (mode, src);

  /* Emit the instruction.  */

  op = gen_rtx_SET (VOIDmode, dst, gen_rtx_fmt_e (code, mode, src));
  if (reload_in_progress || code == NOT)
    {
      /* Reload doesn't know about the flags register, and doesn't know that
         it doesn't want to clobber it.  */
      gcc_assert (code == NOT);
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
ix86_unary_operator_ok (enum rtx_code code ATTRIBUTE_UNUSED,
			enum machine_mode mode ATTRIBUTE_UNUSED,
			rtx operands[2] ATTRIBUTE_UNUSED)
{
  /* If one of operands is memory, source and destination must match.  */
  if ((MEM_P (operands[0])
       || MEM_P (operands[1]))
      && ! rtx_equal_p (operands[0], operands[1]))
    return FALSE;
  return TRUE;
}

/* Post-reload splitter for converting an SF or DFmode value in an
   SSE register into an unsigned SImode.  */

void
ix86_split_convert_uns_si_sse (rtx operands[])
{
  enum machine_mode vecmode;
  rtx value, large, zero_or_two31, input, two31, x;

  large = operands[1];
  zero_or_two31 = operands[2];
  input = operands[3];
  two31 = operands[4];
  vecmode = GET_MODE (large);
  value = gen_rtx_REG (vecmode, REGNO (operands[0]));

  /* Load up the value into the low element.  We must ensure that the other
     elements are valid floats -- zero is the easiest such value.  */
  if (MEM_P (input))
    {
      if (vecmode == V4SFmode)
	emit_insn (gen_vec_setv4sf_0 (value, CONST0_RTX (V4SFmode), input));
      else
	emit_insn (gen_sse2_loadlpd (value, CONST0_RTX (V2DFmode), input));
    }
  else
    {
      input = gen_rtx_REG (vecmode, REGNO (input));
      emit_move_insn (value, CONST0_RTX (vecmode));
      if (vecmode == V4SFmode)
	emit_insn (gen_sse_movss (value, value, input));
      else
	emit_insn (gen_sse2_movsd (value, value, input));
    }

  emit_move_insn (large, two31);
  emit_move_insn (zero_or_two31, MEM_P (two31) ? large : two31);

  x = gen_rtx_fmt_ee (LE, vecmode, large, value);
  emit_insn (gen_rtx_SET (VOIDmode, large, x));

  x = gen_rtx_AND (vecmode, zero_or_two31, large);
  emit_insn (gen_rtx_SET (VOIDmode, zero_or_two31, x));

  x = gen_rtx_MINUS (vecmode, value, zero_or_two31);
  emit_insn (gen_rtx_SET (VOIDmode, value, x));

  large = gen_rtx_REG (V4SImode, REGNO (large));
  emit_insn (gen_ashlv4si3 (large, large, GEN_INT (31)));

  x = gen_rtx_REG (V4SImode, REGNO (value));
  if (vecmode == V4SFmode)
    emit_insn (gen_sse2_cvttps2dq (x, value));
  else
    emit_insn (gen_sse2_cvttpd2dq (x, value));
  value = x;

  emit_insn (gen_xorv4si3 (value, value, large));
}

/* Convert an unsigned DImode value into a DFmode, using only SSE.
   Expects the 64-bit DImode to be supplied in a pair of integral
   registers.  Requires SSE2; will use SSE3 if available.  For x86_32,
   -mfpmath=sse, !optimize_size only.  */

void
ix86_expand_convert_uns_didf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE bias_lo_rvt, bias_hi_rvt;
  rtx int_xmm, fp_xmm;
  rtx biases, exponents;
  rtx x;

  int_xmm = gen_reg_rtx (V4SImode);
  if (TARGET_INTER_UNIT_MOVES)
    emit_insn (gen_movdi_to_sse (int_xmm, input));
  else if (TARGET_SSE_SPLIT_REGS)
    {
      emit_insn (gen_rtx_CLOBBER (VOIDmode, int_xmm));
      emit_move_insn (gen_lowpart (DImode, int_xmm), input);
    }
  else
    {
      x = gen_reg_rtx (V2DImode);
      ix86_expand_vector_init_one_nonzero (false, V2DImode, x, input, 0);
      emit_move_insn (int_xmm, gen_lowpart (V4SImode, x));
    }

  x = gen_rtx_CONST_VECTOR (V4SImode,
			    gen_rtvec (4, GEN_INT (0x43300000UL),
				       GEN_INT (0x45300000UL),
				       const0_rtx, const0_rtx));
  exponents = validize_mem (force_const_mem (V4SImode, x));

  /* int_xmm = {0x45300000UL, fp_xmm/hi, 0x43300000, fp_xmm/lo } */
  emit_insn (gen_sse2_punpckldq (int_xmm, int_xmm, exponents));

  /* Concatenating (juxtaposing) (0x43300000UL ## fp_value_low_xmm)
     yields a valid DF value equal to (0x1.0p52 + double(fp_value_lo_xmm)).
     Similarly (0x45300000UL ## fp_value_hi_xmm) yields
     (0x1.0p84 + double(fp_value_hi_xmm)).
     Note these exponents differ by 32.  */

  fp_xmm = copy_to_mode_reg (V2DFmode, gen_lowpart (V2DFmode, int_xmm));

  /* Subtract off those 0x1.0p52 and 0x1.0p84 biases, to produce values
     in [0,2**32-1] and [0]+[2**32,2**64-1] respectively.  */
  real_ldexp (&bias_lo_rvt, &dconst1, 52);
  real_ldexp (&bias_hi_rvt, &dconst1, 84);
  biases = const_double_from_real_value (bias_lo_rvt, DFmode);
  x = const_double_from_real_value (bias_hi_rvt, DFmode);
  biases = gen_rtx_CONST_VECTOR (V2DFmode, gen_rtvec (2, biases, x));
  biases = validize_mem (force_const_mem (V2DFmode, biases));
  emit_insn (gen_subv2df3 (fp_xmm, fp_xmm, biases));

  /* Add the upper and lower DFmode values together.  */
  if (TARGET_SSE3)
    emit_insn (gen_sse3_haddv2df3 (fp_xmm, fp_xmm, fp_xmm));
  else
    {
      x = copy_to_mode_reg (V2DFmode, fp_xmm);
      emit_insn (gen_sse2_unpckhpd (fp_xmm, fp_xmm, fp_xmm));
      emit_insn (gen_addv2df3 (fp_xmm, fp_xmm, x));
    }

  ix86_expand_vector_extract (false, target, fp_xmm, 0);
}

/* Convert an unsigned SImode value into a DFmode.  Only currently used
   for SSE, but applicable anywhere.  */

void
ix86_expand_convert_uns_sidf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE TWO31r;
  rtx x, fp;

  x = expand_simple_binop (SImode, PLUS, input, GEN_INT (-2147483647 - 1),
			   NULL, 1, OPTAB_DIRECT);

  fp = gen_reg_rtx (DFmode);
  emit_insn (gen_floatsidf2 (fp, x));

  real_ldexp (&TWO31r, &dconst1, 31);
  x = const_double_from_real_value (TWO31r, DFmode);

  x = expand_simple_binop (DFmode, PLUS, fp, x, target, 0, OPTAB_DIRECT);
  if (x != target)
    emit_move_insn (target, x);
}

/* Convert a signed DImode value into a DFmode.  Only used for SSE in
   32-bit mode; otherwise we have a direct convert instruction.  */

void
ix86_expand_convert_sign_didf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE TWO32r;
  rtx fp_lo, fp_hi, x;

  fp_lo = gen_reg_rtx (DFmode);
  fp_hi = gen_reg_rtx (DFmode);

  emit_insn (gen_floatsidf2 (fp_hi, gen_highpart (SImode, input)));

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);
  fp_hi = expand_simple_binop (DFmode, MULT, fp_hi, x, fp_hi, 0, OPTAB_DIRECT);

  ix86_expand_convert_uns_sidf_sse (fp_lo, gen_lowpart (SImode, input));

  x = expand_simple_binop (DFmode, PLUS, fp_hi, fp_lo, target,
			   0, OPTAB_DIRECT);
  if (x != target)
    emit_move_insn (target, x);
}

/* Convert an unsigned SImode value into a SFmode, using only SSE.
   For x86_32, -mfpmath=sse, !optimize_size only.  */
void
ix86_expand_convert_uns_sisf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE ONE16r;
  rtx fp_hi, fp_lo, int_hi, int_lo, x;

  real_ldexp (&ONE16r, &dconst1, 16);
  x = const_double_from_real_value (ONE16r, SFmode);
  int_lo = expand_simple_binop (SImode, AND, input, GEN_INT(0xffff),
				      NULL, 0, OPTAB_DIRECT);
  int_hi = expand_simple_binop (SImode, LSHIFTRT, input, GEN_INT(16),
				      NULL, 0, OPTAB_DIRECT);
  fp_hi = gen_reg_rtx (SFmode);
  fp_lo = gen_reg_rtx (SFmode);
  emit_insn (gen_floatsisf2 (fp_hi, int_hi));
  emit_insn (gen_floatsisf2 (fp_lo, int_lo));
  fp_hi = expand_simple_binop (SFmode, MULT, fp_hi, x, fp_hi,
			       0, OPTAB_DIRECT);
  fp_hi = expand_simple_binop (SFmode, PLUS, fp_hi, fp_lo, target,
			       0, OPTAB_DIRECT);
  if (!rtx_equal_p (target, fp_hi))
    emit_move_insn (target, fp_hi);
}

/* A subroutine of ix86_build_signbit_mask_vector.  If VECT is true,
   then replicate the value for all elements of the vector
   register.  */

rtx
ix86_build_const_vector (enum machine_mode mode, bool vect, rtx value)
{
  rtvec v;
  switch (mode)
    {
    case SImode:
      gcc_assert (vect);
      v = gen_rtvec (4, value, value, value, value);
      return gen_rtx_CONST_VECTOR (V4SImode, v);

    case DImode:
      gcc_assert (vect);
      v = gen_rtvec (2, value, value);
      return gen_rtx_CONST_VECTOR (V2DImode, v);

    case SFmode:
      if (vect)
	v = gen_rtvec (4, value, value, value, value);
      else
	v = gen_rtvec (4, value, CONST0_RTX (SFmode),
		       CONST0_RTX (SFmode), CONST0_RTX (SFmode));
      return gen_rtx_CONST_VECTOR (V4SFmode, v);

    case DFmode:
      if (vect)
	v = gen_rtvec (2, value, value);
      else
	v = gen_rtvec (2, value, CONST0_RTX (DFmode));
      return gen_rtx_CONST_VECTOR (V2DFmode, v);

    default:
      gcc_unreachable ();
    }
}

/* A subroutine of ix86_expand_fp_absneg_operator, copysign expanders
   and ix86_expand_int_vcond.  Create a mask for the sign bit in MODE
   for an SSE register.  If VECT is true, then replicate the mask for
   all elements of the vector register.  If INVERT is true, then create
   a mask excluding the sign bit.  */

rtx
ix86_build_signbit_mask (enum machine_mode mode, bool vect, bool invert)
{
  enum machine_mode vec_mode, imode;
  HOST_WIDE_INT hi, lo;
  int shift = 63;
  rtx v;
  rtx mask;

  /* Find the sign bit, sign extended to 2*HWI.  */
  switch (mode)
    {
    case SImode:
    case SFmode:
      imode = SImode;
      vec_mode = (mode == SImode) ? V4SImode : V4SFmode;
      lo = 0x80000000, hi = lo < 0;
      break;

    case DImode:
    case DFmode:
      imode = DImode;
      vec_mode = (mode == DImode) ? V2DImode : V2DFmode;
      if (HOST_BITS_PER_WIDE_INT >= 64)
	lo = (HOST_WIDE_INT)1 << shift, hi = -1;
      else
	lo = 0, hi = (HOST_WIDE_INT)1 << (shift - HOST_BITS_PER_WIDE_INT);
      break;

    case TImode:
    case TFmode:
      imode = TImode;
      vec_mode = VOIDmode;
      gcc_assert (HOST_BITS_PER_WIDE_INT >= 64);
      lo = 0, hi = (HOST_WIDE_INT)1 << shift;
     break;

    default:
      gcc_unreachable ();
    }

  if (invert)
    lo = ~lo, hi = ~hi;

  /* Force this value into the low part of a fp vector constant.  */
  mask = immed_double_const (lo, hi, imode);
  mask = gen_lowpart (mode, mask);

  if (vec_mode == VOIDmode)
    return force_reg (mode, mask);

  v = ix86_build_const_vector (mode, vect, mask);
  return force_reg (vec_mode, v);
}

/* Generate code for floating point ABS or NEG.  */

void
ix86_expand_fp_absneg_operator (enum rtx_code code, enum machine_mode mode,
				rtx operands[])
{
  rtx mask, set, use, clob, dst, src;
  bool use_sse = false;
  bool vector_mode = VECTOR_MODE_P (mode);
  enum machine_mode elt_mode = mode;

  if (vector_mode)
    {
      elt_mode = GET_MODE_INNER (mode);
      use_sse = true;
    }
  else if (mode == TFmode)
    use_sse = true;
  else if (TARGET_SSE_MATH)
    use_sse = SSE_FLOAT_MODE_P (mode);

  /* NEG and ABS performed with SSE use bitwise mask operations.
     Create the appropriate mask now.  */
  if (use_sse)
    mask = ix86_build_signbit_mask (elt_mode, vector_mode, code == ABS);
  else
    mask = NULL_RTX;

  dst = operands[0];
  src = operands[1];

  if (vector_mode)
    {
      set = gen_rtx_fmt_ee (code == NEG ? XOR : AND, mode, src, mask);
      set = gen_rtx_SET (VOIDmode, dst, set);
      emit_insn (set);
    }
  else
    {
      set = gen_rtx_fmt_e (code, mode, src);
      set = gen_rtx_SET (VOIDmode, dst, set);
      if (mask)
        {
          use = gen_rtx_USE (VOIDmode, mask);
          clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
          emit_insn (gen_rtx_PARALLEL (VOIDmode,
				       gen_rtvec (3, set, use, clob)));
        }
      else
	emit_insn (set);
    }
}

/* Expand a copysign operation.  Special case operand 0 being a constant.  */

void
ix86_expand_copysign (rtx operands[])
{
  enum machine_mode mode, vmode;
  rtx dest, op0, op1, mask, nmask;

  dest = operands[0];
  op0 = operands[1];
  op1 = operands[2];

  mode = GET_MODE (dest);
  vmode = mode == SFmode ? V4SFmode : V2DFmode;

  if (GET_CODE (op0) == CONST_DOUBLE)
    {
      rtx (*copysign_insn)(rtx, rtx, rtx, rtx);

      if (real_isneg (CONST_DOUBLE_REAL_VALUE (op0)))
	op0 = simplify_unary_operation (ABS, mode, op0, mode);

      if (mode == SFmode || mode == DFmode)
	{
	  if (op0 == CONST0_RTX (mode))
	    op0 = CONST0_RTX (vmode);
	  else
	    {
	      rtvec v;

	      if (mode == SFmode)
		v = gen_rtvec (4, op0, CONST0_RTX (SFmode),
			       CONST0_RTX (SFmode), CONST0_RTX (SFmode));
	      else
		v = gen_rtvec (2, op0, CONST0_RTX (DFmode));
	      op0 = force_reg (vmode, gen_rtx_CONST_VECTOR (vmode, v));
	    }
	}

      mask = ix86_build_signbit_mask (mode, 0, 0);

      if (mode == SFmode)
	copysign_insn = gen_copysignsf3_const;
      else if (mode == DFmode)
	copysign_insn = gen_copysigndf3_const;
      else
	copysign_insn = gen_copysigntf3_const;

	emit_insn (copysign_insn (dest, op0, op1, mask));
    }
  else
    {
      rtx (*copysign_insn)(rtx, rtx, rtx, rtx, rtx, rtx);

      nmask = ix86_build_signbit_mask (mode, 0, 1);
      mask = ix86_build_signbit_mask (mode, 0, 0);

      if (mode == SFmode)
	copysign_insn = gen_copysignsf3_var;
      else if (mode == DFmode)
	copysign_insn = gen_copysigndf3_var;
      else
	copysign_insn = gen_copysigntf3_var;

      emit_insn (copysign_insn (dest, NULL_RTX, op0, op1, nmask, mask));
    }
}

/* Deconstruct a copysign operation into bit masks.  Operand 0 is known to
   be a constant, and so has already been expanded into a vector constant.  */

void
ix86_split_copysign_const (rtx operands[])
{
  enum machine_mode mode, vmode;
  rtx dest, op0, op1, mask, x;

  dest = operands[0];
  op0 = operands[1];
  op1 = operands[2];
  mask = operands[3];

  mode = GET_MODE (dest);
  vmode = GET_MODE (mask);

  dest = simplify_gen_subreg (vmode, dest, mode, 0);
  x = gen_rtx_AND (vmode, dest, mask);
  emit_insn (gen_rtx_SET (VOIDmode, dest, x));

  if (op0 != CONST0_RTX (vmode))
    {
      x = gen_rtx_IOR (vmode, dest, op0);
      emit_insn (gen_rtx_SET (VOIDmode, dest, x));
    }
}

/* Deconstruct a copysign operation into bit masks.  Operand 0 is variable,
   so we have to do two masks.  */

void
ix86_split_copysign_var (rtx operands[])
{
  enum machine_mode mode, vmode;
  rtx dest, scratch, op0, op1, mask, nmask, x;

  dest = operands[0];
  scratch = operands[1];
  op0 = operands[2];
  op1 = operands[3];
  nmask = operands[4];
  mask = operands[5];

  mode = GET_MODE (dest);
  vmode = GET_MODE (mask);

  if (rtx_equal_p (op0, op1))
    {
      /* Shouldn't happen often (it's useless, obviously), but when it does
	 we'd generate incorrect code if we continue below.  */
      emit_move_insn (dest, op0);
      return;
    }

  if (REG_P (mask) && REGNO (dest) == REGNO (mask))	/* alternative 0 */
    {
      gcc_assert (REGNO (op1) == REGNO (scratch));

      x = gen_rtx_AND (vmode, scratch, mask);
      emit_insn (gen_rtx_SET (VOIDmode, scratch, x));

      dest = mask;
      op0 = simplify_gen_subreg (vmode, op0, mode, 0);
      x = gen_rtx_NOT (vmode, dest);
      x = gen_rtx_AND (vmode, x, op0);
      emit_insn (gen_rtx_SET (VOIDmode, dest, x));
    }
  else
    {
      if (REGNO (op1) == REGNO (scratch))		/* alternative 1,3 */
	{
	  x = gen_rtx_AND (vmode, scratch, mask);
	}
      else						/* alternative 2,4 */
	{
          gcc_assert (REGNO (mask) == REGNO (scratch));
          op1 = simplify_gen_subreg (vmode, op1, mode, 0);
	  x = gen_rtx_AND (vmode, scratch, op1);
	}
      emit_insn (gen_rtx_SET (VOIDmode, scratch, x));

      if (REGNO (op0) == REGNO (dest))			/* alternative 1,2 */
	{
	  dest = simplify_gen_subreg (vmode, op0, mode, 0);
	  x = gen_rtx_AND (vmode, dest, nmask);
	}
      else						/* alternative 3,4 */
	{
          gcc_assert (REGNO (nmask) == REGNO (dest));
	  dest = nmask;
	  op0 = simplify_gen_subreg (vmode, op0, mode, 0);
	  x = gen_rtx_AND (vmode, dest, op0);
	}
      emit_insn (gen_rtx_SET (VOIDmode, dest, x));
    }

  x = gen_rtx_IOR (vmode, dest, scratch);
  emit_insn (gen_rtx_SET (VOIDmode, dest, x));
}

/* Return TRUE or FALSE depending on whether the first SET in INSN
   has source and destination with matching CC modes, and that the
   CC mode is at least as constrained as REQ_MODE.  */

int
ix86_match_ccmode (rtx insn, enum machine_mode req_mode)
{
  rtx set;
  enum machine_mode set_mode;

  set = PATTERN (insn);
  if (GET_CODE (set) == PARALLEL)
    set = XVECEXP (set, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  gcc_assert (GET_CODE (SET_SRC (set)) == COMPARE);

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
    case CCAmode:
    case CCCmode:
    case CCOmode:
    case CCSmode:
    case CCZmode:
      break;

    default:
      gcc_unreachable ();
    }

  return (GET_MODE (SET_SRC (set)) == set_mode);
}

/* Generate insn patterns to do an integer compare of OPERANDS.  */

static rtx
ix86_expand_int_compare (enum rtx_code code, rtx op0, rtx op1)
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
ix86_fp_compare_mode (enum rtx_code code ATTRIBUTE_UNUSED)
{
  /* ??? In order to make all comparisons reversible, we do all comparisons
     non-trapping when compiling for IEEE.  Once gcc is able to distinguish
     all forms trapping and nontrapping comparisons, we can make inequality
     comparisons trapping again, since it results in better code when using
     FCOM based compares.  */
  return TARGET_IEEE_FP ? CCFPUmode : CCFPmode;
}

enum machine_mode
ix86_cc_mode (enum rtx_code code, rtx op0, rtx op1)
{
  enum machine_mode mode = GET_MODE (op0);

  if (SCALAR_FLOAT_MODE_P (mode))
    {
      gcc_assert (!DECIMAL_FLOAT_MODE_P (mode));
      return ix86_fp_compare_mode (code);
    }

  switch (code)
    {
      /* Only zero flag is needed.  */
    case EQ:			/* ZF=0 */
    case NE:			/* ZF!=0 */
      return CCZmode;
      /* Codes needing carry flag.  */
    case GEU:			/* CF=0 */
    case LTU:			/* CF=1 */
      /* Detect overflow checks.  They need just the carry flag.  */
      if (GET_CODE (op0) == PLUS
	  && rtx_equal_p (op1, XEXP (op0, 0)))
	return CCCmode;
      else
	return CCmode;
    case GTU:			/* CF=0 & ZF=0 */
    case LEU:			/* CF=1 | ZF=1 */
      /* Detect overflow checks.  They need just the carry flag.  */
      if (GET_CODE (op0) == MINUS
	  && rtx_equal_p (op1, XEXP (op0, 0)))
	return CCCmode;
      else
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
         so we need to use relational tests against overflow
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
      gcc_unreachable ();
    }
}

/* Return the fixed registers used for condition codes.  */

static bool
ix86_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = FLAGS_REG;
  *p2 = FPSR_REG;
  return true;
}

/* If two condition code modes are compatible, return a condition code
   mode which is compatible with both.  Otherwise, return
   VOIDmode.  */

static enum machine_mode
ix86_cc_modes_compatible (enum machine_mode m1, enum machine_mode m2)
{
  if (m1 == m2)
    return m1;

  if (GET_MODE_CLASS (m1) != MODE_CC || GET_MODE_CLASS (m2) != MODE_CC)
    return VOIDmode;

  if ((m1 == CCGCmode && m2 == CCGOCmode)
      || (m1 == CCGOCmode && m2 == CCGCmode))
    return CCGCmode;

  switch (m1)
    {
    default:
      gcc_unreachable ();

    case CCmode:
    case CCGCmode:
    case CCGOCmode:
    case CCNOmode:
    case CCAmode:
    case CCCmode:
    case CCOmode:
    case CCSmode:
    case CCZmode:
      switch (m2)
	{
	default:
	  return VOIDmode;

	case CCmode:
	case CCGCmode:
	case CCGOCmode:
	case CCNOmode:
	case CCAmode:
	case CCCmode:
	case CCOmode:
	case CCSmode:
	case CCZmode:
	  return CCmode;
	}

    case CCFPmode:
    case CCFPUmode:
      /* These are only compatible with themselves, which we already
	 checked above.  */
      return VOIDmode;
    }
}

/* Split comparison code CODE into comparisons we can do using branch
   instructions.  BYPASS_CODE is comparison code for branch that will
   branch around FIRST_CODE and SECOND_CODE.  If some of branches
   is not required, set value to UNKNOWN.
   We never require more than two branches.  */

void
ix86_fp_comparison_codes (enum rtx_code code, enum rtx_code *bypass_code,
			  enum rtx_code *first_code,
			  enum rtx_code *second_code)
{
  *first_code = code;
  *bypass_code = UNKNOWN;
  *second_code = UNKNOWN;

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
      gcc_unreachable ();
    }
  if (!TARGET_IEEE_FP)
    {
      *second_code = UNKNOWN;
      *bypass_code = UNKNOWN;
    }
}

/* Return cost of comparison done fcom + arithmetics operations on AX.
   All following functions do use number of instructions as a cost metrics.
   In future this should be tweaked to compute bytes for optimize_size and
   take into account performance of various instructions on various CPUs.  */
static int
ix86_fp_comparison_arithmetics_cost (enum rtx_code code)
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
      gcc_unreachable ();
    }
}

/* Return cost of comparison done using fcomi operation.
   See ix86_fp_comparison_arithmetics_cost for the metrics.  */
static int
ix86_fp_comparison_fcomi_cost (enum rtx_code code)
{
  enum rtx_code bypass_code, first_code, second_code;
  /* Return arbitrarily high cost when instruction is not supported - this
     prevents gcc from using it.  */
  if (!TARGET_CMOVE)
    return 1024;
  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);
  return (bypass_code != UNKNOWN || second_code != UNKNOWN) + 2;
}

/* Return cost of comparison done using sahf operation.
   See ix86_fp_comparison_arithmetics_cost for the metrics.  */
static int
ix86_fp_comparison_sahf_cost (enum rtx_code code)
{
  enum rtx_code bypass_code, first_code, second_code;
  /* Return arbitrarily high cost when instruction is not preferred - this
     avoids gcc from using it.  */
  if (!(TARGET_SAHF && (TARGET_USE_SAHF || optimize_size)))
    return 1024;
  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);
  return (bypass_code != UNKNOWN || second_code != UNKNOWN) + 3;
}

/* Compute cost of the comparison done using any method.
   See ix86_fp_comparison_arithmetics_cost for the metrics.  */
static int
ix86_fp_comparison_cost (enum rtx_code code)
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

/* Return true if we should use an FCOMI instruction for this
   fp comparison.  */

int
ix86_use_fcomi_compare (enum rtx_code code ATTRIBUTE_UNUSED)
{
  enum rtx_code swapped_code = swap_condition (code);

  return ((ix86_fp_comparison_cost (code)
	   == ix86_fp_comparison_fcomi_cost (code))
	  || (ix86_fp_comparison_cost (swapped_code)
	      == ix86_fp_comparison_fcomi_cost (swapped_code)));
}

/* Swap, force into registers, or otherwise massage the two operands
   to a fp comparison.  The operands are updated in place; the new
   comparison code is returned.  */

static enum rtx_code
ix86_prepare_fp_compare_args (enum rtx_code code, rtx *pop0, rtx *pop1)
{
  enum machine_mode fpcmp_mode = ix86_fp_compare_mode (code);
  rtx op0 = *pop0, op1 = *pop1;
  enum machine_mode op_mode = GET_MODE (op0);
  int is_sse = TARGET_SSE_MATH && SSE_FLOAT_MODE_P (op_mode);

  /* All of the unordered compare instructions only work on registers.
     The same is true of the fcomi compare instructions.  The XFmode
     compare instructions require registers except when comparing
     against zero or when converting operand 1 from fixed point to
     floating point.  */

  if (!is_sse
      && (fpcmp_mode == CCFPUmode
	  || (op_mode == XFmode
	      && ! (standard_80387_constant_p (op0) == 1
		    || standard_80387_constant_p (op1) == 1)
	      && GET_CODE (op1) != FLOAT)
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
	  || (MEM_P (op0)
	      && ! (standard_80387_constant_p (op1) == 0
		    || MEM_P (op1))))
	{
	  rtx tmp;
	  tmp = op0, op0 = op1, op1 = tmp;
	  code = swap_condition (code);
	}

      if (!REG_P (op0))
	op0 = force_reg (op_mode, op0);

      if (CONSTANT_P (op1))
	{
	  int tmp = standard_80387_constant_p (op1);
	  if (tmp == 0)
	    op1 = validize_mem (force_const_mem (op_mode, op1));
	  else if (tmp == 1)
	    {
	      if (TARGET_CMOVE)
		op1 = force_reg (op_mode, op1);
	    }
	  else
	    op1 = force_reg (op_mode, op1);
	}
    }

  /* Try to rearrange the comparison to make it cheaper.  */
  if (ix86_fp_comparison_cost (code)
      > ix86_fp_comparison_cost (swap_condition (code))
      && (REG_P (op1) || can_create_pseudo_p ()))
    {
      rtx tmp;
      tmp = op0, op0 = op1, op1 = tmp;
      code = swap_condition (code);
      if (!REG_P (op0))
	op0 = force_reg (op_mode, op0);
    }

  *pop0 = op0;
  *pop1 = op1;
  return code;
}

/* Convert comparison codes we use to represent FP comparison to integer
   code that will result in proper branch.  Return UNKNOWN if no such code
   is available.  */

enum rtx_code
ix86_fp_compare_code_to_integer (enum rtx_code code)
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

/* Generate insn patterns to do a floating point compare of OPERANDS.  */

static rtx
ix86_expand_fp_compare (enum rtx_code code, rtx op0, rtx op1, rtx scratch,
			rtx *second_test, rtx *bypass_test)
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
  if (ix86_fp_comparison_arithmetics_cost (code) > cost
      && (bypass_code == UNKNOWN || bypass_test)
      && (second_code == UNKNOWN || second_test))
    {
      tmp = gen_rtx_COMPARE (fpcmp_mode, op0, op1);
      tmp = gen_rtx_SET (VOIDmode, gen_rtx_REG (fpcmp_mode, FLAGS_REG),
			 tmp);
      if (TARGET_CMOVE)
	emit_insn (tmp);
      else
	{
	  gcc_assert (TARGET_SAHF);

	  if (!scratch)
	    scratch = gen_reg_rtx (HImode);
	  tmp2 = gen_rtx_CLOBBER (VOIDmode, scratch);

	  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, tmp2)));
	}

      /* The FP codes work out to act like unsigned.  */
      intcmp_mode = fpcmp_mode;
      code = first_code;
      if (bypass_code != UNKNOWN)
	*bypass_test = gen_rtx_fmt_ee (bypass_code, VOIDmode,
				       gen_rtx_REG (intcmp_mode, FLAGS_REG),
				       const0_rtx);
      if (second_code != UNKNOWN)
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
	  gcc_unreachable ();
	}
    }

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode,
			 gen_rtx_REG (intcmp_mode, FLAGS_REG),
			 const0_rtx);
}

rtx
ix86_expand_compare (enum rtx_code code, rtx *second_test, rtx *bypass_test)
{
  rtx op0, op1, ret;
  op0 = ix86_compare_op0;
  op1 = ix86_compare_op1;

  if (second_test)
    *second_test = NULL_RTX;
  if (bypass_test)
    *bypass_test = NULL_RTX;

  if (ix86_compare_emitted)
    {
      ret = gen_rtx_fmt_ee (code, VOIDmode, ix86_compare_emitted, const0_rtx);
      ix86_compare_emitted = NULL_RTX;
    }
  else if (SCALAR_FLOAT_MODE_P (GET_MODE (op0)))
    {
      gcc_assert (!DECIMAL_FLOAT_MODE_P (GET_MODE (op0)));
      ret = ix86_expand_fp_compare (code, op0, op1, NULL_RTX,
				    second_test, bypass_test);
    }
  else
    ret = ix86_expand_int_compare (code, op0, op1);

  return ret;
}

/* Return true if the CODE will result in nontrivial jump sequence.  */
bool
ix86_fp_jump_nontrivial_p (enum rtx_code code)
{
  enum rtx_code bypass_code, first_code, second_code;
  if (!TARGET_CMOVE)
    return true;
  ix86_fp_comparison_codes (code, &bypass_code, &first_code, &second_code);
  return bypass_code != UNKNOWN || second_code != UNKNOWN;
}

void
ix86_expand_branch (enum rtx_code code, rtx label)
{
  rtx tmp;

  /* If we have emitted a compare insn, go straight to simple.
     ix86_expand_compare won't emit anything if ix86_compare_emitted
     is non NULL.  */
  if (ix86_compare_emitted)
    goto simple;

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
	if (bypass_code == UNKNOWN && second_code == UNKNOWN)
	  {
	    ix86_split_fp_branch (code, ix86_compare_op0, ix86_compare_op1,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx, NULL_RTX, NULL_RTX);
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
	      = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCFPmode, FPSR_REG));
	    RTVEC_ELT (vec, 2)
	      = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCFPmode, FLAGS_REG));
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
    case TImode:
      /* Expand DImode branch into multiple compare+branch.  */
      {
	rtx lo[2], hi[2], label2;
	enum rtx_code code1, code2, code3;
	enum machine_mode submode;

	if (CONSTANT_P (ix86_compare_op0) && ! CONSTANT_P (ix86_compare_op1))
	  {
	    tmp = ix86_compare_op0;
	    ix86_compare_op0 = ix86_compare_op1;
	    ix86_compare_op1 = tmp;
	    code = swap_condition (code);
	  }
	if (GET_MODE (ix86_compare_op0) == DImode)
	  {
	    split_di (&ix86_compare_op0, 1, lo+0, hi+0);
	    split_di (&ix86_compare_op1, 1, lo+1, hi+1);
	    submode = SImode;
	  }
	else
	  {
	    split_ti (&ix86_compare_op0, 1, lo+0, hi+0);
	    split_ti (&ix86_compare_op1, 1, lo+1, hi+1);
	    submode = DImode;
	  }

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
	      xor1 = expand_binop (submode, xor_optab, xor1, hi[1],
				   NULL_RTX, 0, OPTAB_WIDEN);

	    xor0 = lo[0];
	    if (lo[1] != const0_rtx)
	      xor0 = expand_binop (submode, xor_optab, xor0, lo[1],
				   NULL_RTX, 0, OPTAB_WIDEN);

	    tmp = expand_binop (submode, ior_optab, xor1, xor0,
				NULL_RTX, 0, OPTAB_WIDEN);

	    ix86_compare_op0 = tmp;
	    ix86_compare_op1 = const0_rtx;
	    ix86_expand_branch (code, label);
	    return;
	  }

	/* Otherwise, if we are doing less-than or greater-or-equal-than,
	   op1 is a constant and the low word is zero, then we can just
	   examine the high word.  Similarly for low word -1 and
	   less-or-equal-than or greater-than.  */

	if (CONST_INT_P (hi[1]))
	  switch (code)
	    {
	    case LT: case LTU: case GE: case GEU:
	      if (lo[1] == const0_rtx)
		{
		  ix86_compare_op0 = hi[0];
		  ix86_compare_op1 = hi[1];
		  ix86_expand_branch (code, label);
		  return;
		}
	      break;
	    case LE: case LEU: case GT: case GTU:
	      if (lo[1] == constm1_rtx)
		{
		  ix86_compare_op0 = hi[0];
		  ix86_compare_op1 = hi[1];
		  ix86_expand_branch (code, label);
		  return;
		}
	      break;
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

	  case EQ:   code1 = UNKNOWN; code2 = NE;  break;
	  case NE:   code2 = UNKNOWN; break;

	  default:
	    gcc_unreachable ();
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

	if (code1 != UNKNOWN)
	  ix86_expand_branch (code1, label);
	if (code2 != UNKNOWN)
	  ix86_expand_branch (code2, label2);

	ix86_compare_op0 = lo[0];
	ix86_compare_op1 = lo[1];
	ix86_expand_branch (code3, label);

	if (code2 != UNKNOWN)
	  emit_label (label2);
	return;
      }

    default:
      gcc_unreachable ();
    }
}

/* Split branch based on floating point condition.  */
void
ix86_split_fp_branch (enum rtx_code code, rtx op1, rtx op2,
		      rtx target1, rtx target2, rtx tmp, rtx pushed)
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

  /* Remove pushed operand from stack.  */
  if (pushed)
    ix86_free_from_memory (GET_MODE (pushed));

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
ix86_expand_setcc (enum rtx_code code, rtx dest)
{
  rtx ret, tmp, tmpreg, equiv;
  rtx second_test, bypass_test;

  if (GET_MODE (ix86_compare_op0) == (TARGET_64BIT ? TImode : DImode))
    return 0; /* FAIL */

  gcc_assert (GET_MODE (dest) == QImode);

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
	  gcc_assert (!second_test);
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

  /* Attach a REG_EQUAL note describing the comparison result.  */
  if (ix86_compare_op0 && ix86_compare_op1)
    {
      equiv = simplify_gen_relational (code, QImode,
				       GET_MODE (ix86_compare_op0),
				       ix86_compare_op0, ix86_compare_op1);
      set_unique_reg_note (get_last_insn (), REG_EQUAL, equiv);
    }

  return 1; /* DONE */
}

/* Expand comparison setting or clearing carry flag.  Return true when
   successful and set pop for the operation.  */
static bool
ix86_expand_carry_flag_compare (enum rtx_code code, rtx op0, rtx op1, rtx *pop)
{
  enum machine_mode mode =
    GET_MODE (op0) != VOIDmode ? GET_MODE (op0) : GET_MODE (op1);

  /* Do not handle DImode compares that go through special path.  */
  if (mode == (TARGET_64BIT ? TImode : DImode))
    return false;

  if (SCALAR_FLOAT_MODE_P (mode))
    {
      rtx second_test = NULL, bypass_test = NULL;
      rtx compare_op, compare_seq;

      gcc_assert (!DECIMAL_FLOAT_MODE_P (mode));

      /* Shortcut:  following common codes never translate
	 into carry flag compares.  */
      if (code == EQ || code == NE || code == UNEQ || code == LTGT
	  || code == ORDERED || code == UNORDERED)
	return false;

      /* These comparisons require zero flag; swap operands so they won't.  */
      if ((code == GT || code == UNLE || code == LE || code == UNGT)
	  && !TARGET_IEEE_FP)
	{
	  rtx tmp = op0;
	  op0 = op1;
	  op1 = tmp;
	  code = swap_condition (code);
	}

      /* Try to expand the comparison and verify that we end up with
	 carry flag based comparison.  This fails to be true only when
	 we decide to expand comparison using arithmetic that is not
	 too common scenario.  */
      start_sequence ();
      compare_op = ix86_expand_fp_compare (code, op0, op1, NULL_RTX,
					   &second_test, &bypass_test);
      compare_seq = get_insns ();
      end_sequence ();

      if (second_test || bypass_test)
	return false;

      if (GET_MODE (XEXP (compare_op, 0)) == CCFPmode
	  || GET_MODE (XEXP (compare_op, 0)) == CCFPUmode)
        code = ix86_fp_compare_code_to_integer (GET_CODE (compare_op));
      else
	code = GET_CODE (compare_op);

      if (code != LTU && code != GEU)
	return false;

      emit_insn (compare_seq);
      *pop = compare_op;
      return true;
    }

  if (!INTEGRAL_MODE_P (mode))
    return false;

  switch (code)
    {
    case LTU:
    case GEU:
      break;

    /* Convert a==0 into (unsigned)a<1.  */
    case EQ:
    case NE:
      if (op1 != const0_rtx)
	return false;
      op1 = const1_rtx;
      code = (code == EQ ? LTU : GEU);
      break;

    /* Convert a>b into b<a or a>=b-1.  */
    case GTU:
    case LEU:
      if (CONST_INT_P (op1))
	{
	  op1 = gen_int_mode (INTVAL (op1) + 1, GET_MODE (op0));
	  /* Bail out on overflow.  We still can swap operands but that
	     would force loading of the constant into register.  */
	  if (op1 == const0_rtx
	      || !x86_64_immediate_operand (op1, GET_MODE (op1)))
	    return false;
	  code = (code == GTU ? GEU : LTU);
	}
      else
	{
	  rtx tmp = op1;
	  op1 = op0;
	  op0 = tmp;
	  code = (code == GTU ? LTU : GEU);
	}
      break;

    /* Convert a>=0 into (unsigned)a<0x80000000.  */
    case LT:
    case GE:
      if (mode == DImode || op1 != const0_rtx)
	return false;
      op1 = gen_int_mode (1 << (GET_MODE_BITSIZE (mode) - 1), mode);
      code = (code == LT ? GEU : LTU);
      break;
    case LE:
    case GT:
      if (mode == DImode || op1 != constm1_rtx)
	return false;
      op1 = gen_int_mode (1 << (GET_MODE_BITSIZE (mode) - 1), mode);
      code = (code == LE ? GEU : LTU);
      break;

    default:
      return false;
    }
  /* Swapping operands may cause constant to appear as first operand.  */
  if (!nonimmediate_operand (op0, VOIDmode))
    {
      if (!can_create_pseudo_p ())
	return false;
      op0 = force_reg (mode, op0);
    }
  ix86_compare_op0 = op0;
  ix86_compare_op1 = op1;
  *pop = ix86_expand_compare (code, NULL, NULL);
  gcc_assert (GET_CODE (*pop) == LTU || GET_CODE (*pop) == GEU);
  return true;
}

int
ix86_expand_int_movcc (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[1]), compare_code;
  rtx compare_seq, compare_op;
  rtx second_test, bypass_test;
  enum machine_mode mode = GET_MODE (operands[0]);
  bool sign_bit_compare_p = false;;

  start_sequence ();
  compare_op = ix86_expand_compare (code, &second_test, &bypass_test);
  compare_seq = get_insns ();
  end_sequence ();

  compare_code = GET_CODE (compare_op);

  if ((ix86_compare_op1 == const0_rtx && (code == GE || code == LT))
      || (ix86_compare_op1 == constm1_rtx && (code == GT || code == LE)))
    sign_bit_compare_p = true;

  /* Don't attempt mode expansion here -- if we had to expand 5 or 6
     HImode insns, we'd be swallowed in word prefix ops.  */

  if ((mode != HImode || TARGET_FAST_PREFIX)
      && (mode != (TARGET_64BIT ? TImode : DImode))
      && CONST_INT_P (operands[2])
      && CONST_INT_P (operands[3]))
    {
      rtx out = operands[0];
      HOST_WIDE_INT ct = INTVAL (operands[2]);
      HOST_WIDE_INT cf = INTVAL (operands[3]);
      HOST_WIDE_INT diff;

      diff = ct - cf;
      /*  Sign bit compares are better done using shifts than we do by using
	  sbb.  */
      if (sign_bit_compare_p
	  || ix86_expand_carry_flag_compare (code, ix86_compare_op0,
					     ix86_compare_op1, &compare_op))
	{
	  /* Detect overlap between destination and compare sources.  */
	  rtx tmp = out;

          if (!sign_bit_compare_p)
	    {
	      bool fpcmp = false;

	      compare_code = GET_CODE (compare_op);

	      if (GET_MODE (XEXP (compare_op, 0)) == CCFPmode
		  || GET_MODE (XEXP (compare_op, 0)) == CCFPUmode)
		{
		  fpcmp = true;
		  compare_code = ix86_fp_compare_code_to_integer (compare_code);
		}

	      /* To simplify rest of code, restrict to the GEU case.  */
	      if (compare_code == LTU)
		{
		  HOST_WIDE_INT tmp = ct;
		  ct = cf;
		  cf = tmp;
		  compare_code = reverse_condition (compare_code);
		  code = reverse_condition (code);
		}
	      else
		{
		  if (fpcmp)
		    PUT_CODE (compare_op,
			      reverse_condition_maybe_unordered
			        (GET_CODE (compare_op)));
		  else
		    PUT_CODE (compare_op, reverse_condition (GET_CODE (compare_op)));
		}
	      diff = ct - cf;

	      if (reg_overlap_mentioned_p (out, ix86_compare_op0)
		  || reg_overlap_mentioned_p (out, ix86_compare_op1))
		tmp = gen_reg_rtx (mode);

	      if (mode == DImode)
		emit_insn (gen_x86_movdicc_0_m1_rex64 (tmp, compare_op));
	      else
		emit_insn (gen_x86_movsicc_0_m1 (gen_lowpart (SImode, tmp), compare_op));
	    }
	  else
	    {
	      if (code == GT || code == GE)
		code = reverse_condition (code);
	      else
		{
		  HOST_WIDE_INT tmp = ct;
		  ct = cf;
		  cf = tmp;
		  diff = ct - cf;
		}
	      tmp = emit_store_flag (tmp, code, ix86_compare_op0,
				     ix86_compare_op1, VOIDmode, 0, -1);
	    }

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
					   copy_rtx (tmp), 1, OPTAB_DIRECT);
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
					 copy_rtx (tmp), 1, OPTAB_DIRECT);
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
	      tmp = expand_simple_unop (mode, NOT, tmp, copy_rtx (tmp), 1);
	      if (cf)
		tmp = expand_simple_binop (mode, PLUS,
					   copy_rtx (tmp), GEN_INT (cf),
					   copy_rtx (tmp), 1, OPTAB_DIRECT);
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
		  tmp = expand_simple_unop (mode, NOT, tmp, copy_rtx (tmp), 1);
		}

	      tmp = expand_simple_binop (mode, AND,
					 copy_rtx (tmp),
					 gen_int_mode (cf - ct, mode),
					 copy_rtx (tmp), 1, OPTAB_DIRECT);
	      if (ct)
		tmp = expand_simple_binop (mode, PLUS,
					   copy_rtx (tmp), GEN_INT (ct),
					   copy_rtx (tmp), 1, OPTAB_DIRECT);
	    }

	  if (!rtx_equal_p (tmp, out))
	    emit_move_insn (copy_rtx (out), copy_rtx (tmp));

	  return 1; /* DONE */
	}

      if (diff < 0)
	{
	  enum machine_mode cmp_mode = GET_MODE (ix86_compare_op0);

	  HOST_WIDE_INT tmp;
	  tmp = ct, ct = cf, cf = tmp;
	  diff = -diff;

	  if (SCALAR_FLOAT_MODE_P (cmp_mode))
	    {
	      gcc_assert (!DECIMAL_FLOAT_MODE_P (cmp_mode));

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

      compare_code = UNKNOWN;
      if (GET_MODE_CLASS (GET_MODE (ix86_compare_op0)) == MODE_INT
	  && CONST_INT_P (ix86_compare_op1))
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
      if (compare_code != UNKNOWN
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
	  && ((mode != QImode && mode != HImode) || !TARGET_PARTIAL_REG_STALL)
	  && (mode != DImode
	      || x86_64_immediate_operand (GEN_INT (cf), VOIDmode)))
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
	  if (!rtx_equal_p (tmp, out))
	    {
	      if (nops == 1)
		out = force_operand (tmp, copy_rtx (out));
	      else
		emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (out), copy_rtx (tmp)));
	    }
	  if (!rtx_equal_p (out, operands[0]))
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
       */

      if ((!TARGET_CMOVE || (mode == QImode && TARGET_PARTIAL_REG_STALL))
	  && BRANCH_COST >= 2)
	{
	  if (cf == 0)
	    {
	      enum machine_mode cmp_mode = GET_MODE (ix86_compare_op0);

	      cf = ct;
	      ct = 0;

	      if (SCALAR_FLOAT_MODE_P (cmp_mode))
		{
		  gcc_assert (!DECIMAL_FLOAT_MODE_P (cmp_mode));

		  /* We may be reversing unordered compare to normal compare,
		     that is not valid in general (we may convert non-trapping
		     condition to trapping one), however on i386 we currently
		     emit all comparisons unordered.  */
		  code = reverse_condition_maybe_unordered (code);
		}
	      else
		{
		  code = reverse_condition (code);
		  if (compare_code != UNKNOWN)
		    compare_code = reverse_condition (compare_code);
		}
	    }

	  if (compare_code != UNKNOWN)
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

	      out = expand_simple_binop (mode, PLUS, copy_rtx (out), constm1_rtx,
					 copy_rtx (out), 1, OPTAB_DIRECT);
	    }

	  out = expand_simple_binop (mode, AND, copy_rtx (out),
				     gen_int_mode (cf - ct, mode),
				     copy_rtx (out), 1, OPTAB_DIRECT);
	  if (ct)
	    out = expand_simple_binop (mode, PLUS, copy_rtx (out), GEN_INT (ct),
				       copy_rtx (out), 1, OPTAB_DIRECT);
	  if (!rtx_equal_p (out, operands[0]))
	    emit_move_insn (operands[0], copy_rtx (out));

	  return 1; /* DONE */
	}
    }

  if (!TARGET_CMOVE || (mode == QImode && TARGET_PARTIAL_REG_STALL))
    {
      /* Try a few things more with specific constants and a variable.  */

      optab op;
      rtx var, orig_out, out, tmp;

      if (BRANCH_COST <= 2)
	return 0; /* FAIL */

      /* If one of the two operands is an interesting constant, load a
	 constant with the above and mask it in with a logical operation.  */

      if (CONST_INT_P (operands[2]))
	{
	  var = operands[3];
	  if (INTVAL (operands[2]) == 0 && operands[3] != constm1_rtx)
	    operands[3] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[2]) == -1 && operands[3] != const0_rtx)
	    operands[3] = const0_rtx, op = ior_optab;
	  else
	    return 0; /* FAIL */
	}
      else if (CONST_INT_P (operands[3]))
	{
	  var = operands[2];
	  if (INTVAL (operands[3]) == 0 && operands[2] != constm1_rtx)
	    operands[2] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[3]) == -1 && operands[3] != const0_rtx)
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
      if (!rtx_equal_p (out, orig_out))
	emit_move_insn (copy_rtx (orig_out), copy_rtx (out));

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
      && (mode == QImode
          || ! register_operand (operands[3], VOIDmode)))
    operands[2] = force_reg (mode, operands[2]);

  if (mode == QImode
      && ! register_operand (operands[3], VOIDmode))
    operands[3] = force_reg (mode, operands[3]);

  emit_insn (compare_seq);
  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (mode,
						compare_op, operands[2],
						operands[3])));
  if (bypass_test)
    emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (operands[0]),
			    gen_rtx_IF_THEN_ELSE (mode,
				  bypass_test,
				  copy_rtx (operands[3]),
				  copy_rtx (operands[0]))));
  if (second_test)
    emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (operands[0]),
			    gen_rtx_IF_THEN_ELSE (mode,
				  second_test,
				  copy_rtx (operands[2]),
				  copy_rtx (operands[0]))));

  return 1; /* DONE */
}

/* Swap, force into registers, or otherwise massage the two operands
   to an sse comparison with a mask result.  Thus we differ a bit from
   ix86_prepare_fp_compare_args which expects to produce a flags result.

   The DEST operand exists to help determine whether to commute commutative
   operators.  The POP0/POP1 operands are updated in place.  The new
   comparison code is returned, or UNKNOWN if not implementable.  */

static enum rtx_code
ix86_prepare_sse_fp_compare_args (rtx dest, enum rtx_code code,
				  rtx *pop0, rtx *pop1)
{
  rtx tmp;

  switch (code)
    {
    case LTGT:
    case UNEQ:
      /* We have no LTGT as an operator.  We could implement it with
	 NE & ORDERED, but this requires an extra temporary.  It's
	 not clear that it's worth it.  */
      return UNKNOWN;

    case LT:
    case LE:
    case UNGT:
    case UNGE:
      /* These are supported directly.  */
      break;

    case EQ:
    case NE:
    case UNORDERED:
    case ORDERED:
      /* For commutative operators, try to canonicalize the destination
	 operand to be first in the comparison - this helps reload to
	 avoid extra moves.  */
      if (!dest || !rtx_equal_p (dest, *pop1))
	break;
      /* FALLTHRU */

    case GE:
    case GT:
    case UNLE:
    case UNLT:
      /* These are not supported directly.  Swap the comparison operands
	 to transform into something that is supported.  */
      tmp = *pop0;
      *pop0 = *pop1;
      *pop1 = tmp;
      code = swap_condition (code);
      break;

    default:
      gcc_unreachable ();
    }

  return code;
}

/* Detect conditional moves that exactly match min/max operational
   semantics.  Note that this is IEEE safe, as long as we don't
   interchange the operands.

   Returns FALSE if this conditional move doesn't match a MIN/MAX,
   and TRUE if the operation is successful and instructions are emitted.  */

static bool
ix86_expand_sse_fp_minmax (rtx dest, enum rtx_code code, rtx cmp_op0,
			   rtx cmp_op1, rtx if_true, rtx if_false)
{
  enum machine_mode mode;
  bool is_min;
  rtx tmp;

  if (code == LT)
    ;
  else if (code == UNGE)
    {
      tmp = if_true;
      if_true = if_false;
      if_false = tmp;
    }
  else
    return false;

  if (rtx_equal_p (cmp_op0, if_true) && rtx_equal_p (cmp_op1, if_false))
    is_min = true;
  else if (rtx_equal_p (cmp_op1, if_true) && rtx_equal_p (cmp_op0, if_false))
    is_min = false;
  else
    return false;

  mode = GET_MODE (dest);

  /* We want to check HONOR_NANS and HONOR_SIGNED_ZEROS here,
     but MODE may be a vector mode and thus not appropriate.  */
  if (!flag_finite_math_only || !flag_unsafe_math_optimizations)
    {
      int u = is_min ? UNSPEC_IEEE_MIN : UNSPEC_IEEE_MAX;
      rtvec v;

      if_true = force_reg (mode, if_true);
      v = gen_rtvec (2, if_true, if_false);
      tmp = gen_rtx_UNSPEC (mode, v, u);
    }
  else
    {
      code = is_min ? SMIN : SMAX;
      tmp = gen_rtx_fmt_ee (code, mode, if_true, if_false);
    }

  emit_insn (gen_rtx_SET (VOIDmode, dest, tmp));
  return true;
}

/* Expand an sse vector comparison.  Return the register with the result.  */

static rtx
ix86_expand_sse_cmp (rtx dest, enum rtx_code code, rtx cmp_op0, rtx cmp_op1,
		     rtx op_true, rtx op_false)
{
  enum machine_mode mode = GET_MODE (dest);
  rtx x;

  cmp_op0 = force_reg (mode, cmp_op0);
  if (!nonimmediate_operand (cmp_op1, mode))
    cmp_op1 = force_reg (mode, cmp_op1);

  if (optimize
      || reg_overlap_mentioned_p (dest, op_true)
      || reg_overlap_mentioned_p (dest, op_false))
    dest = gen_reg_rtx (mode);

  x = gen_rtx_fmt_ee (code, mode, cmp_op0, cmp_op1);
  emit_insn (gen_rtx_SET (VOIDmode, dest, x));

  return dest;
}

/* Expand DEST = CMP ? OP_TRUE : OP_FALSE into a sequence of logical
   operations.  This is used for both scalar and vector conditional moves.  */

static void
ix86_expand_sse_movcc (rtx dest, rtx cmp, rtx op_true, rtx op_false)
{
  enum machine_mode mode = GET_MODE (dest);
  rtx t2, t3, x;

  if (TARGET_SSE5)
    {
      rtx pcmov = gen_rtx_SET (mode, dest,
			       gen_rtx_IF_THEN_ELSE (mode, cmp,
						     op_true,
						     op_false));
      emit_insn (pcmov);
    }
  else if (op_false == CONST0_RTX (mode))
    {
      op_true = force_reg (mode, op_true);
      x = gen_rtx_AND (mode, cmp, op_true);
      emit_insn (gen_rtx_SET (VOIDmode, dest, x));
    }
  else if (op_true == CONST0_RTX (mode))
    {
      op_false = force_reg (mode, op_false);
      x = gen_rtx_NOT (mode, cmp);
      x = gen_rtx_AND (mode, x, op_false);
      emit_insn (gen_rtx_SET (VOIDmode, dest, x));
    }
  else
    {
      op_true = force_reg (mode, op_true);
      op_false = force_reg (mode, op_false);

      t2 = gen_reg_rtx (mode);
      if (optimize)
	t3 = gen_reg_rtx (mode);
      else
	t3 = dest;

      x = gen_rtx_AND (mode, op_true, cmp);
      emit_insn (gen_rtx_SET (VOIDmode, t2, x));

      x = gen_rtx_NOT (mode, cmp);
      x = gen_rtx_AND (mode, x, op_false);
      emit_insn (gen_rtx_SET (VOIDmode, t3, x));

      x = gen_rtx_IOR (mode, t3, t2);
      emit_insn (gen_rtx_SET (VOIDmode, dest, x));
    }
}

/* Expand a floating-point conditional move.  Return true if successful.  */

int
ix86_expand_fp_movcc (rtx operands[])
{
  enum machine_mode mode = GET_MODE (operands[0]);
  enum rtx_code code = GET_CODE (operands[1]);
  rtx tmp, compare_op, second_test, bypass_test;

  if (TARGET_SSE_MATH && SSE_FLOAT_MODE_P (mode))
    {
      enum machine_mode cmode;

      /* Since we've no cmove for sse registers, don't force bad register
	 allocation just to gain access to it.  Deny movcc when the
	 comparison mode doesn't match the move mode.  */
      cmode = GET_MODE (ix86_compare_op0);
      if (cmode == VOIDmode)
	cmode = GET_MODE (ix86_compare_op1);
      if (cmode != mode)
	return 0;

      code = ix86_prepare_sse_fp_compare_args (operands[0], code,
					       &ix86_compare_op0,
					       &ix86_compare_op1);
      if (code == UNKNOWN)
	return 0;

      if (ix86_expand_sse_fp_minmax (operands[0], code, ix86_compare_op0,
				     ix86_compare_op1, operands[2],
				     operands[3]))
	return 1;

      tmp = ix86_expand_sse_cmp (operands[0], code, ix86_compare_op0,
				 ix86_compare_op1, operands[2], operands[3]);
      ix86_expand_sse_movcc (operands[0], tmp, operands[2], operands[3]);
      return 1;
    }

  /* The floating point conditional move instructions don't directly
     support conditions resulting from a signed integer comparison.  */

  compare_op = ix86_expand_compare (code, &second_test, &bypass_test);

  /* The floating point conditional move instructions don't directly
     support signed integer comparisons.  */

  if (!fcmov_comparison_operator (compare_op, VOIDmode))
    {
      gcc_assert (!second_test && !bypass_test);
      tmp = gen_reg_rtx (QImode);
      ix86_expand_setcc (code, tmp);
      code = NE;
      ix86_compare_op0 = tmp;
      ix86_compare_op1 = const0_rtx;
      compare_op = ix86_expand_compare (code,  &second_test, &bypass_test);
    }
  if (bypass_test && reg_overlap_mentioned_p (operands[0], operands[3]))
    {
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, operands[3]);
      operands[3] = tmp;
    }
  if (second_test && reg_overlap_mentioned_p (operands[0], operands[2]))
    {
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, operands[2]);
      operands[2] = tmp;
    }

  emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			  gen_rtx_IF_THEN_ELSE (mode, compare_op,
						operands[2], operands[3])));
  if (bypass_test)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_IF_THEN_ELSE (mode, bypass_test,
						  operands[3], operands[0])));
  if (second_test)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_IF_THEN_ELSE (mode, second_test,
						  operands[2], operands[0])));

  return 1;
}

/* Expand a floating-point vector conditional move; a vcond operation
   rather than a movcc operation.  */

bool
ix86_expand_fp_vcond (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[3]);
  rtx cmp;

  code = ix86_prepare_sse_fp_compare_args (operands[0], code,
					   &operands[4], &operands[5]);
  if (code == UNKNOWN)
    return false;

  if (ix86_expand_sse_fp_minmax (operands[0], code, operands[4],
				 operands[5], operands[1], operands[2]))
    return true;

  cmp = ix86_expand_sse_cmp (operands[0], code, operands[4], operands[5],
			     operands[1], operands[2]);
  ix86_expand_sse_movcc (operands[0], cmp, operands[1], operands[2]);
  return true;
}

/* Expand a signed/unsigned integral vector conditional move.  */

bool
ix86_expand_int_vcond (rtx operands[])
{
  enum machine_mode mode = GET_MODE (operands[0]);
  enum rtx_code code = GET_CODE (operands[3]);
  bool negate = false;
  rtx x, cop0, cop1;

  cop0 = operands[4];
  cop1 = operands[5];

  /* Canonicalize the comparison to EQ, GT, GTU.  */
  switch (code)
    {
    case EQ:
    case GT:
    case GTU:
      break;

    case NE:
    case LE:
    case LEU:
      code = reverse_condition (code);
      negate = true;
      break;

    case GE:
    case GEU:
      code = reverse_condition (code);
      negate = true;
      /* FALLTHRU */

    case LT:
    case LTU:
      code = swap_condition (code);
      x = cop0, cop0 = cop1, cop1 = x;
      break;

    default:
      gcc_unreachable ();
    }

  /* Only SSE4.1/SSE4.2 supports V2DImode.  */
  if (mode == V2DImode)
    {
      switch (code)
	{
	case EQ:
	  /* SSE4.1 supports EQ.  */
	  if (!TARGET_SSE4_1)
	    return false;
	  break;

	case GT:
	case GTU:
	  /* SSE4.2 supports GT/GTU.  */
	  if (!TARGET_SSE4_2)
	    return false;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Unsigned parallel compare is not supported by the hardware.  Play some
     tricks to turn this into a signed comparison against 0.  */
  if (code == GTU)
    {
      cop0 = force_reg (mode, cop0);

      switch (mode)
	{
	case V4SImode:
	case V2DImode:
	  {
	    rtx t1, t2, mask;

	    /* Perform a parallel modulo subtraction.  */
	    t1 = gen_reg_rtx (mode);
	    emit_insn ((mode == V4SImode
			? gen_subv4si3
			: gen_subv2di3) (t1, cop0, cop1));

	    /* Extract the original sign bit of op0.  */
	    mask = ix86_build_signbit_mask (GET_MODE_INNER (mode),
					    true, false);
	    t2 = gen_reg_rtx (mode);
	    emit_insn ((mode == V4SImode
			? gen_andv4si3
			: gen_andv2di3) (t2, cop0, mask));

	    /* XOR it back into the result of the subtraction.  This results
	       in the sign bit set iff we saw unsigned underflow.  */
	    x = gen_reg_rtx (mode);
	    emit_insn ((mode == V4SImode
			? gen_xorv4si3
			: gen_xorv2di3) (x, t1, t2));

	    code = GT;
	  }
	  break;

	case V16QImode:
	case V8HImode:
	  /* Perform a parallel unsigned saturating subtraction.  */
	  x = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (VOIDmode, x,
				  gen_rtx_US_MINUS (mode, cop0, cop1)));

	  code = EQ;
	  negate = !negate;
	  break;

	default:
	  gcc_unreachable ();
	}

      cop0 = x;
      cop1 = CONST0_RTX (mode);
    }

  x = ix86_expand_sse_cmp (operands[0], code, cop0, cop1,
			   operands[1+negate], operands[2-negate]);

  ix86_expand_sse_movcc (operands[0], x, operands[1+negate],
			 operands[2-negate]);
  return true;
}

/* Unpack OP[1] into the next wider integer vector type.  UNSIGNED_P is
   true if we should do zero extension, else sign extension.  HIGH_P is
   true if we want the N/2 high elements, else the low elements.  */

void
ix86_expand_sse_unpack (rtx operands[2], bool unsigned_p, bool high_p)
{
  enum machine_mode imode = GET_MODE (operands[1]);
  rtx (*unpack)(rtx, rtx, rtx);
  rtx se, dest;

  switch (imode)
    {
    case V16QImode:
      if (high_p)
        unpack = gen_vec_interleave_highv16qi;
      else
        unpack = gen_vec_interleave_lowv16qi;
      break;
    case V8HImode:
      if (high_p)
        unpack = gen_vec_interleave_highv8hi;
      else
        unpack = gen_vec_interleave_lowv8hi;
      break;
    case V4SImode:
      if (high_p)
        unpack = gen_vec_interleave_highv4si;
      else
        unpack = gen_vec_interleave_lowv4si;
      break;
    default:
      gcc_unreachable ();
    }

  dest = gen_lowpart (imode, operands[0]);

  if (unsigned_p)
    se = force_reg (imode, CONST0_RTX (imode));
  else
    se = ix86_expand_sse_cmp (gen_reg_rtx (imode), GT, CONST0_RTX (imode),
                              operands[1], pc_rtx, pc_rtx);

  emit_insn (unpack (dest, operands[1], se));
}

/* This function performs the same task as ix86_expand_sse_unpack,
   but with SSE4.1 instructions.  */

void
ix86_expand_sse4_unpack (rtx operands[2], bool unsigned_p, bool high_p)
{
  enum machine_mode imode = GET_MODE (operands[1]);
  rtx (*unpack)(rtx, rtx);
  rtx src, dest;

  switch (imode)
    {
    case V16QImode:
      if (unsigned_p)
	unpack = gen_sse4_1_zero_extendv8qiv8hi2;
      else
	unpack = gen_sse4_1_extendv8qiv8hi2;
      break;
    case V8HImode:
      if (unsigned_p)
	unpack = gen_sse4_1_zero_extendv4hiv4si2;
      else
	unpack = gen_sse4_1_extendv4hiv4si2;
      break;
    case V4SImode:
      if (unsigned_p)
	unpack = gen_sse4_1_zero_extendv2siv2di2;
      else
	unpack = gen_sse4_1_extendv2siv2di2;
      break;
    default:
      gcc_unreachable ();
    }

  dest = operands[0];
  if (high_p)
    {
      /* Shift higher 8 bytes to lower 8 bytes.  */
      src = gen_reg_rtx (imode);
      emit_insn (gen_sse2_lshrti3 (gen_lowpart (TImode, src),
				   gen_lowpart (TImode, operands[1]),
				   GEN_INT (64)));
    }
  else
    src = operands[1];

  emit_insn (unpack (dest, src));
}

/* This function performs the same task as ix86_expand_sse_unpack,
   but with amdfam15 instructions.  */

#define PPERM_SRC	0x00		/* copy source */
#define PPERM_INVERT	0x20		/* invert source */
#define PPERM_REVERSE	0x40		/* bit reverse source */
#define PPERM_REV_INV	0x60		/* bit reverse & invert src */
#define PPERM_ZERO	0x80		/* all 0's */
#define PPERM_ONES	0xa0		/* all 1's */
#define PPERM_SIGN	0xc0		/* propagate sign bit */
#define PPERM_INV_SIGN	0xe0		/* invert & propagate sign */

#define PPERM_SRC1	0x00		/* use first source byte */
#define PPERM_SRC2	0x10		/* use second source byte */

void
ix86_expand_sse5_unpack (rtx operands[2], bool unsigned_p, bool high_p)
{
  enum machine_mode imode = GET_MODE (operands[1]);
  int pperm_bytes[16];
  int i;
  int h = (high_p) ? 8 : 0;
  int h2;
  int sign_extend;
  rtvec v = rtvec_alloc (16);
  rtvec vs;
  rtx x, p;
  rtx op0 = operands[0], op1 = operands[1];

  switch (imode)
    {
    case V16QImode:
      vs = rtvec_alloc (8);
      h2 = (high_p) ? 8 : 0;
      for (i = 0; i < 8; i++)
	{
	  pperm_bytes[2*i+0] = PPERM_SRC | PPERM_SRC2 | i | h;
	  pperm_bytes[2*i+1] = ((unsigned_p)
				? PPERM_ZERO
				: PPERM_SIGN | PPERM_SRC2 | i | h);
	}

      for (i = 0; i < 16; i++)
	RTVEC_ELT (v, i) = GEN_INT (pperm_bytes[i]);

      for (i = 0; i < 8; i++)
	RTVEC_ELT (vs, i) = GEN_INT (i + h2);

      p = gen_rtx_PARALLEL (VOIDmode, vs);
      x = force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, v));
      if (unsigned_p)
	emit_insn (gen_sse5_pperm_zero_v16qi_v8hi (op0, op1, p, x));
      else
	emit_insn (gen_sse5_pperm_sign_v16qi_v8hi (op0, op1, p, x));
      break;

    case V8HImode:
      vs = rtvec_alloc (4);
      h2 = (high_p) ? 4 : 0;
      for (i = 0; i < 4; i++)
	{
	  sign_extend = ((unsigned_p)
			 ? PPERM_ZERO
			 : PPERM_SIGN | PPERM_SRC2 | ((2*i) + 1 + h));
	  pperm_bytes[4*i+0] = PPERM_SRC | PPERM_SRC2 | ((2*i) + 0 + h);
	  pperm_bytes[4*i+1] = PPERM_SRC | PPERM_SRC2 | ((2*i) + 1 + h);
	  pperm_bytes[4*i+2] = sign_extend;
	  pperm_bytes[4*i+3] = sign_extend;
	}

      for (i = 0; i < 16; i++)
	RTVEC_ELT (v, i) = GEN_INT (pperm_bytes[i]);

      for (i = 0; i < 4; i++)
	RTVEC_ELT (vs, i) = GEN_INT (i + h2);

      p = gen_rtx_PARALLEL (VOIDmode, vs);
      x = force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, v));
      if (unsigned_p)
	emit_insn (gen_sse5_pperm_zero_v8hi_v4si (op0, op1, p, x));
      else
	emit_insn (gen_sse5_pperm_sign_v8hi_v4si (op0, op1, p, x));
      break;

    case V4SImode:
      vs = rtvec_alloc (2);
      h2 = (high_p) ? 2 : 0;
      for (i = 0; i < 2; i++)
	{
	  sign_extend = ((unsigned_p)
			 ? PPERM_ZERO
			 : PPERM_SIGN | PPERM_SRC2 | ((4*i) + 3 + h));
	  pperm_bytes[8*i+0] = PPERM_SRC | PPERM_SRC2 | ((4*i) + 0 + h);
	  pperm_bytes[8*i+1] = PPERM_SRC | PPERM_SRC2 | ((4*i) + 1 + h);
	  pperm_bytes[8*i+2] = PPERM_SRC | PPERM_SRC2 | ((4*i) + 2 + h);
	  pperm_bytes[8*i+3] = PPERM_SRC | PPERM_SRC2 | ((4*i) + 3 + h);
	  pperm_bytes[8*i+4] = sign_extend;
	  pperm_bytes[8*i+5] = sign_extend;
	  pperm_bytes[8*i+6] = sign_extend;
	  pperm_bytes[8*i+7] = sign_extend;
	}

      for (i = 0; i < 16; i++)
	RTVEC_ELT (v, i) = GEN_INT (pperm_bytes[i]);

      for (i = 0; i < 2; i++)
	RTVEC_ELT (vs, i) = GEN_INT (i + h2);

      p = gen_rtx_PARALLEL (VOIDmode, vs);
      x = force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, v));
      if (unsigned_p)
	emit_insn (gen_sse5_pperm_zero_v4si_v2di (op0, op1, p, x));
      else
	emit_insn (gen_sse5_pperm_sign_v4si_v2di (op0, op1, p, x));
      break;

    default:
      gcc_unreachable ();
    }

  return;
}

/* Pack the high bits from OPERANDS[1] and low bits from OPERANDS[2] into the
   next narrower integer vector type */
void
ix86_expand_sse5_pack (rtx operands[3])
{
  enum machine_mode imode = GET_MODE (operands[0]);
  int pperm_bytes[16];
  int i;
  rtvec v = rtvec_alloc (16);
  rtx x;
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];

  switch (imode)
    {
    case V16QImode:
      for (i = 0; i < 8; i++)
	{
	  pperm_bytes[i+0] = PPERM_SRC | PPERM_SRC1 | (i*2);
	  pperm_bytes[i+8] = PPERM_SRC | PPERM_SRC2 | (i*2);
	}

      for (i = 0; i < 16; i++)
	RTVEC_ELT (v, i) = GEN_INT (pperm_bytes[i]);

      x = force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, v));
      emit_insn (gen_sse5_pperm_pack_v8hi_v16qi (op0, op1, op2, x));
      break;

    case V8HImode:
      for (i = 0; i < 4; i++)
	{
	  pperm_bytes[(2*i)+0] = PPERM_SRC | PPERM_SRC1 | ((i*4) + 0);
	  pperm_bytes[(2*i)+1] = PPERM_SRC | PPERM_SRC1 | ((i*4) + 1);
	  pperm_bytes[(2*i)+8] = PPERM_SRC | PPERM_SRC2 | ((i*4) + 0);
	  pperm_bytes[(2*i)+9] = PPERM_SRC | PPERM_SRC2 | ((i*4) + 1);
	}

      for (i = 0; i < 16; i++)
	RTVEC_ELT (v, i) = GEN_INT (pperm_bytes[i]);

      x = force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, v));
      emit_insn (gen_sse5_pperm_pack_v4si_v8hi (op0, op1, op2, x));
      break;

    case V4SImode:
      for (i = 0; i < 2; i++)
	{
	  pperm_bytes[(4*i)+0]  = PPERM_SRC | PPERM_SRC1 | ((i*8) + 0);
	  pperm_bytes[(4*i)+1]  = PPERM_SRC | PPERM_SRC1 | ((i*8) + 1);
	  pperm_bytes[(4*i)+2]  = PPERM_SRC | PPERM_SRC1 | ((i*8) + 2);
	  pperm_bytes[(4*i)+3]  = PPERM_SRC | PPERM_SRC1 | ((i*8) + 3);
	  pperm_bytes[(4*i)+8]  = PPERM_SRC | PPERM_SRC2 | ((i*8) + 0);
	  pperm_bytes[(4*i)+9]  = PPERM_SRC | PPERM_SRC2 | ((i*8) + 1);
	  pperm_bytes[(4*i)+10] = PPERM_SRC | PPERM_SRC2 | ((i*8) + 2);
	  pperm_bytes[(4*i)+11] = PPERM_SRC | PPERM_SRC2 | ((i*8) + 3);
	}

      for (i = 0; i < 16; i++)
	RTVEC_ELT (v, i) = GEN_INT (pperm_bytes[i]);

      x = force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, v));
      emit_insn (gen_sse5_pperm_pack_v2di_v4si (op0, op1, op2, x));
      break;

    default:
      gcc_unreachable ();
    }

  return;
}

/* Expand conditional increment or decrement using adb/sbb instructions.
   The default case using setcc followed by the conditional move can be
   done by generic code.  */
int
ix86_expand_int_addcc (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx compare_op;
  rtx val = const0_rtx;
  bool fpcmp = false;
  enum machine_mode mode = GET_MODE (operands[0]);

  if (operands[3] != const1_rtx
      && operands[3] != constm1_rtx)
    return 0;
  if (!ix86_expand_carry_flag_compare (code, ix86_compare_op0,
				       ix86_compare_op1, &compare_op))
     return 0;
  code = GET_CODE (compare_op);

  if (GET_MODE (XEXP (compare_op, 0)) == CCFPmode
      || GET_MODE (XEXP (compare_op, 0)) == CCFPUmode)
    {
      fpcmp = true;
      code = ix86_fp_compare_code_to_integer (code);
    }

  if (code != LTU)
    {
      val = constm1_rtx;
      if (fpcmp)
	PUT_CODE (compare_op,
		  reverse_condition_maybe_unordered
		    (GET_CODE (compare_op)));
      else
	PUT_CODE (compare_op, reverse_condition (GET_CODE (compare_op)));
    }
  PUT_MODE (compare_op, mode);

  /* Construct either adc or sbb insn.  */
  if ((code == LTU) == (operands[3] == constm1_rtx))
    {
      switch (GET_MODE (operands[0]))
	{
	  case QImode:
            emit_insn (gen_subqi3_carry (operands[0], operands[2], val, compare_op));
	    break;
	  case HImode:
            emit_insn (gen_subhi3_carry (operands[0], operands[2], val, compare_op));
	    break;
	  case SImode:
            emit_insn (gen_subsi3_carry (operands[0], operands[2], val, compare_op));
	    break;
	  case DImode:
            emit_insn (gen_subdi3_carry_rex64 (operands[0], operands[2], val, compare_op));
	    break;
	  default:
	    gcc_unreachable ();
	}
    }
  else
    {
      switch (GET_MODE (operands[0]))
	{
	  case QImode:
            emit_insn (gen_addqi3_carry (operands[0], operands[2], val, compare_op));
	    break;
	  case HImode:
            emit_insn (gen_addhi3_carry (operands[0], operands[2], val, compare_op));
	    break;
	  case SImode:
            emit_insn (gen_addsi3_carry (operands[0], operands[2], val, compare_op));
	    break;
	  case DImode:
            emit_insn (gen_adddi3_carry_rex64 (operands[0], operands[2], val, compare_op));
	    break;
	  default:
	    gcc_unreachable ();
	}
    }
  return 1; /* DONE */
}


/* Split operands 0 and 1 into SImode parts.  Similar to split_di, but
   works for floating pointer parameters and nonoffsetable memories.
   For pushes, it returns just stack offsets; the values will be saved
   in the right order.  Maximally three parts are generated.  */

static int
ix86_split_to_parts (rtx operand, rtx *parts, enum machine_mode mode)
{
  int size;

  if (!TARGET_64BIT)
    size = mode==XFmode ? 3 : GET_MODE_SIZE (mode) / 4;
  else
    size = (GET_MODE_SIZE (mode) + 4) / 8;

  gcc_assert (!REG_P (operand) || !MMX_REGNO_P (REGNO (operand)));
  gcc_assert (size >= 2 && size <= 3);

  /* Optimize constant pool reference to immediates.  This is used by fp
     moves, that force all constants to memory to allow combining.  */
  if (MEM_P (operand) && MEM_READONLY_P (operand))
    {
      rtx tmp = maybe_get_pool_constant (operand);
      if (tmp)
	operand = tmp;
    }

  if (MEM_P (operand) && !offsettable_memref_p (operand))
    {
      /* The only non-offsetable memories we handle are pushes.  */
      int ok = push_operand (operand, VOIDmode);

      gcc_assert (ok);

      operand = copy_rtx (operand);
      PUT_MODE (operand, Pmode);
      parts[0] = parts[1] = parts[2] = operand;
      return size;
    }

  if (GET_CODE (operand) == CONST_VECTOR)
    {
      enum machine_mode imode = int_mode_for_mode (mode);
      /* Caution: if we looked through a constant pool memory above,
	 the operand may actually have a different mode now.  That's
	 ok, since we want to pun this all the way back to an integer.  */
      operand = simplify_subreg (imode, operand, GET_MODE (operand), 0);
      gcc_assert (operand != NULL);
      mode = imode;
    }

  if (!TARGET_64BIT)
    {
      if (mode == DImode)
	split_di (&operand, 1, &parts[0], &parts[1]);
      else
	{
	  if (REG_P (operand))
	    {
	      gcc_assert (reload_completed);
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
		  REAL_VALUE_TO_TARGET_LONG_DOUBLE (r, l);
		  parts[2] = gen_int_mode (l[2], SImode);
		  break;
		case DFmode:
		  REAL_VALUE_TO_TARGET_DOUBLE (r, l);
		  break;
		default:
		  gcc_unreachable ();
		}
	      parts[1] = gen_int_mode (l[1], SImode);
	      parts[0] = gen_int_mode (l[0], SImode);
	    }
	  else
	    gcc_unreachable ();
	}
    }
  else
    {
      if (mode == TImode)
	split_ti (&operand, 1, &parts[0], &parts[1]);
      if (mode == XFmode || mode == TFmode)
	{
	  enum machine_mode upper_mode = mode==XFmode ? SImode : DImode;
	  if (REG_P (operand))
	    {
	      gcc_assert (reload_completed);
	      parts[0] = gen_rtx_REG (DImode, REGNO (operand) + 0);
	      parts[1] = gen_rtx_REG (upper_mode, REGNO (operand) + 1);
	    }
	  else if (offsettable_memref_p (operand))
	    {
	      operand = adjust_address (operand, DImode, 0);
	      parts[0] = operand;
	      parts[1] = adjust_address (operand, upper_mode, 8);
	    }
	  else if (GET_CODE (operand) == CONST_DOUBLE)
	    {
	      REAL_VALUE_TYPE r;
	      long l[4];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, operand);
	      real_to_target (l, &r, mode);

	      /* Do not use shift by 32 to avoid warning on 32bit systems.  */
	      if (HOST_BITS_PER_WIDE_INT >= 64)
	        parts[0]
		  = gen_int_mode
		      ((l[0] & (((HOST_WIDE_INT) 2 << 31) - 1))
		       + ((((HOST_WIDE_INT) l[1]) << 31) << 1),
		       DImode);
	      else
	        parts[0] = immed_double_const (l[0], l[1], DImode);

	      if (upper_mode == SImode)
	        parts[1] = gen_int_mode (l[2], SImode);
	      else if (HOST_BITS_PER_WIDE_INT >= 64)
	        parts[1]
		  = gen_int_mode
		      ((l[2] & (((HOST_WIDE_INT) 2 << 31) - 1))
		       + ((((HOST_WIDE_INT) l[3]) << 31) << 1),
		       DImode);
	      else
	        parts[1] = immed_double_const (l[2], l[3], DImode);
	    }
	  else
	    gcc_unreachable ();
	}
    }

  return size;
}

/* Emit insns to perform a move or push of DI, DF, and XF values.
   Return false when normal moves are needed; true when all required
   insns have been emitted.  Operands 2-4 contain the input values
   int the correct order; operands 5-7 contain the output values.  */

void
ix86_split_long_move (rtx operands[])
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

      if (MEM_P (operands[1])
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
  else
    gcc_assert (!MEM_P (operands[0])
		|| offsettable_memref_p (operands[0]));

  nparts = ix86_split_to_parts (operands[1], part[1], GET_MODE (operands[0]));
  ix86_split_to_parts (operands[0], part[0], GET_MODE (operands[0]));

  /* When emitting push, take care for source operands on the stack.  */
  if (push && MEM_P (operands[1])
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
  if (REG_P (part[0][0]) && MEM_P (part[1][0]))
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
	      if (TARGET_128BIT_LONG_DOUBLE && mode == XFmode)
                emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, GEN_INT (-4)));
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
	      switch (GET_CODE (part[1][1]))
		{
		case MEM:
		  part[1][1] = adjust_address (part[1][1], DImode, 0);
		  break;

		case REG:
		  part[1][1] = gen_rtx_REG (DImode, REGNO (part[1][1]));
		  break;

		default:
		  gcc_unreachable ();
		}

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

  /* If optimizing for size, attempt to locally unCSE nonzero constants.  */
  if (optimize_size)
    {
      if (CONST_INT_P (operands[5])
	  && operands[5] != const0_rtx
	  && REG_P (operands[2]))
	{
	  if (CONST_INT_P (operands[6])
	      && INTVAL (operands[6]) == INTVAL (operands[5]))
	    operands[6] = operands[2];

	  if (nparts == 3
	      && CONST_INT_P (operands[7])
	      && INTVAL (operands[7]) == INTVAL (operands[5]))
	    operands[7] = operands[2];
	}

      if (nparts == 3
	  && CONST_INT_P (operands[6])
	  && operands[6] != const0_rtx
	  && REG_P (operands[3])
	  && CONST_INT_P (operands[7])
	  && INTVAL (operands[7]) == INTVAL (operands[6]))
	operands[7] = operands[3];
    }

  emit_move_insn (operands[2], operands[5]);
  emit_move_insn (operands[3], operands[6]);
  if (nparts == 3)
    emit_move_insn (operands[4], operands[7]);

  return;
}

/* Helper function of ix86_split_ashl used to generate an SImode/DImode
   left shift by a constant, either using a single shift or
   a sequence of add instructions.  */

static void
ix86_expand_ashl_const (rtx operand, int count, enum machine_mode mode)
{
  if (count == 1)
    {
      emit_insn ((mode == DImode
		  ? gen_addsi3
		  : gen_adddi3) (operand, operand, operand));
    }
  else if (!optimize_size
	   && count * ix86_cost->add <= ix86_cost->shift_const)
    {
      int i;
      for (i=0; i<count; i++)
	{
	  emit_insn ((mode == DImode
		      ? gen_addsi3
		      : gen_adddi3) (operand, operand, operand));
	}
    }
  else
    emit_insn ((mode == DImode
		? gen_ashlsi3
		: gen_ashldi3) (operand, operand, GEN_INT (count)));
}

void
ix86_split_ashl (rtx *operands, rtx scratch, enum machine_mode mode)
{
  rtx low[2], high[2];
  int count;
  const int single_width = mode == DImode ? 32 : 64;

  if (CONST_INT_P (operands[2]))
    {
      (mode == DImode ? split_di : split_ti) (operands, 2, low, high);
      count = INTVAL (operands[2]) & (single_width * 2 - 1);

      if (count >= single_width)
	{
	  emit_move_insn (high[0], low[1]);
	  emit_move_insn (low[0], const0_rtx);

	  if (count > single_width)
	    ix86_expand_ashl_const (high[0], count - single_width, mode);
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn ((mode == DImode
		     ? gen_x86_shld_1
		     : gen_x86_64_shld) (high[0], low[0], GEN_INT (count)));
	  ix86_expand_ashl_const (low[0], count, mode);
	}
      return;
    }

  (mode == DImode ? split_di : split_ti) (operands, 1, low, high);

  if (operands[1] == const1_rtx)
    {
      /* Assuming we've chosen a QImode capable registers, then 1 << N
	 can be done with two 32/64-bit shifts, no branches, no cmoves.  */
      if (ANY_QI_REG_P (low[0]) && ANY_QI_REG_P (high[0]))
	{
	  rtx s, d, flags = gen_rtx_REG (CCZmode, FLAGS_REG);

	  ix86_expand_clear (low[0]);
	  ix86_expand_clear (high[0]);
	  emit_insn (gen_testqi_ccz_1 (operands[2], GEN_INT (single_width)));

	  d = gen_lowpart (QImode, low[0]);
	  d = gen_rtx_STRICT_LOW_PART (VOIDmode, d);
	  s = gen_rtx_EQ (QImode, flags, const0_rtx);
	  emit_insn (gen_rtx_SET (VOIDmode, d, s));

	  d = gen_lowpart (QImode, high[0]);
	  d = gen_rtx_STRICT_LOW_PART (VOIDmode, d);
	  s = gen_rtx_NE (QImode, flags, const0_rtx);
	  emit_insn (gen_rtx_SET (VOIDmode, d, s));
	}

      /* Otherwise, we can get the same results by manually performing
	 a bit extract operation on bit 5/6, and then performing the two
	 shifts.  The two methods of getting 0/1 into low/high are exactly
	 the same size.  Avoiding the shift in the bit extract case helps
	 pentium4 a bit; no one else seems to care much either way.  */
      else
	{
	  rtx x;

	  if (TARGET_PARTIAL_REG_STALL && !optimize_size)
	    x = gen_rtx_ZERO_EXTEND (mode == DImode ? SImode : DImode, operands[2]);
	  else
	    x = gen_lowpart (mode == DImode ? SImode : DImode, operands[2]);
	  emit_insn (gen_rtx_SET (VOIDmode, high[0], x));

	  emit_insn ((mode == DImode
		      ? gen_lshrsi3
		      : gen_lshrdi3) (high[0], high[0], GEN_INT (mode == DImode ? 5 : 6)));
	  emit_insn ((mode == DImode
		      ? gen_andsi3
		      : gen_anddi3) (high[0], high[0], GEN_INT (1)));
	  emit_move_insn (low[0], high[0]);
	  emit_insn ((mode == DImode
		      ? gen_xorsi3
		      : gen_xordi3) (low[0], low[0], GEN_INT (1)));
	}

      emit_insn ((mode == DImode
		    ? gen_ashlsi3
		    : gen_ashldi3) (low[0], low[0], operands[2]));
      emit_insn ((mode == DImode
		    ? gen_ashlsi3
		    : gen_ashldi3) (high[0], high[0], operands[2]));
      return;
    }

  if (operands[1] == constm1_rtx)
    {
      /* For -1 << N, we can avoid the shld instruction, because we
	 know that we're shifting 0...31/63 ones into a -1.  */
      emit_move_insn (low[0], constm1_rtx);
      if (optimize_size)
	emit_move_insn (high[0], low[0]);
      else
	emit_move_insn (high[0], constm1_rtx);
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      (mode == DImode ? split_di : split_ti) (operands, 1, low, high);
      emit_insn ((mode == DImode
		  ? gen_x86_shld_1
		  : gen_x86_64_shld) (high[0], low[0], operands[2]));
    }

  emit_insn ((mode == DImode ? gen_ashlsi3 : gen_ashldi3) (low[0], low[0], operands[2]));

  if (TARGET_CMOVE && scratch)
    {
      ix86_expand_clear (scratch);
      emit_insn ((mode == DImode
		  ? gen_x86_shift_adj_1
		  : gen_x86_64_shift_adj) (high[0], low[0], operands[2], scratch));
    }
  else
    emit_insn (gen_x86_shift_adj_2 (high[0], low[0], operands[2]));
}

void
ix86_split_ashr (rtx *operands, rtx scratch, enum machine_mode mode)
{
  rtx low[2], high[2];
  int count;
  const int single_width = mode == DImode ? 32 : 64;

  if (CONST_INT_P (operands[2]))
    {
      (mode == DImode ? split_di : split_ti) (operands, 2, low, high);
      count = INTVAL (operands[2]) & (single_width * 2 - 1);

      if (count == single_width * 2 - 1)
	{
	  emit_move_insn (high[0], high[1]);
	  emit_insn ((mode == DImode
		      ? gen_ashrsi3
		      : gen_ashrdi3) (high[0], high[0],
				      GEN_INT (single_width - 1)));
	  emit_move_insn (low[0], high[0]);

	}
      else if (count >= single_width)
	{
	  emit_move_insn (low[0], high[1]);
	  emit_move_insn (high[0], low[0]);
	  emit_insn ((mode == DImode
		      ? gen_ashrsi3
		      : gen_ashrdi3) (high[0], high[0],
				      GEN_INT (single_width - 1)));
	  if (count > single_width)
	    emit_insn ((mode == DImode
			? gen_ashrsi3
			: gen_ashrdi3) (low[0], low[0],
					GEN_INT (count - single_width)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn ((mode == DImode
		      ? gen_x86_shrd_1
		      : gen_x86_64_shrd) (low[0], high[0], GEN_INT (count)));
	  emit_insn ((mode == DImode
		      ? gen_ashrsi3
		      : gen_ashrdi3) (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      (mode == DImode ? split_di : split_ti) (operands, 1, low, high);

      emit_insn ((mode == DImode
		  ? gen_x86_shrd_1
		  : gen_x86_64_shrd) (low[0], high[0], operands[2]));
      emit_insn ((mode == DImode
		  ? gen_ashrsi3
		  : gen_ashrdi3)  (high[0], high[0], operands[2]));

      if (TARGET_CMOVE && scratch)
	{
	  emit_move_insn (scratch, high[0]);
	  emit_insn ((mode == DImode
		      ? gen_ashrsi3
		      : gen_ashrdi3) (scratch, scratch,
				      GEN_INT (single_width - 1)));
	  emit_insn ((mode == DImode
		      ? gen_x86_shift_adj_1
		      : gen_x86_64_shift_adj) (low[0], high[0], operands[2],
					 scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_3 (low[0], high[0], operands[2]));
    }
}

void
ix86_split_lshr (rtx *operands, rtx scratch, enum machine_mode mode)
{
  rtx low[2], high[2];
  int count;
  const int single_width = mode == DImode ? 32 : 64;

  if (CONST_INT_P (operands[2]))
    {
      (mode == DImode ? split_di : split_ti) (operands, 2, low, high);
      count = INTVAL (operands[2]) & (single_width * 2 - 1);

      if (count >= single_width)
	{
	  emit_move_insn (low[0], high[1]);
	  ix86_expand_clear (high[0]);

	  if (count > single_width)
	    emit_insn ((mode == DImode
			? gen_lshrsi3
			: gen_lshrdi3) (low[0], low[0],
					GEN_INT (count - single_width)));
	}
      else
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  emit_insn ((mode == DImode
		      ? gen_x86_shrd_1
		      : gen_x86_64_shrd) (low[0], high[0], GEN_INT (count)));
	  emit_insn ((mode == DImode
		      ? gen_lshrsi3
		      : gen_lshrdi3) (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      (mode == DImode ? split_di : split_ti) (operands, 1, low, high);

      emit_insn ((mode == DImode
		  ? gen_x86_shrd_1
		  : gen_x86_64_shrd) (low[0], high[0], operands[2]));
      emit_insn ((mode == DImode
		  ? gen_lshrsi3
		  : gen_lshrdi3) (high[0], high[0], operands[2]));

      /* Heh.  By reversing the arguments, we can reuse this pattern.  */
      if (TARGET_CMOVE && scratch)
	{
	  ix86_expand_clear (scratch);
	  emit_insn ((mode == DImode
		      ? gen_x86_shift_adj_1
		      : gen_x86_64_shift_adj) (low[0], high[0], operands[2],
					       scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2 (low[0], high[0], operands[2]));
    }
}

/* Predict just emitted jump instruction to be taken with probability PROB.  */
static void
predict_jump (int prob)
{
  rtx insn = get_last_insn ();
  gcc_assert (JUMP_P (insn));
  REG_NOTES (insn)
    = gen_rtx_EXPR_LIST (REG_BR_PROB,
			 GEN_INT (prob),
			 REG_NOTES (insn));
}

/* Helper function for the string operations below.  Dest VARIABLE whether
   it is aligned to VALUE bytes.  If true, jump to the label.  */
static rtx
ix86_expand_aligntest (rtx variable, int value, bool epilogue)
{
  rtx label = gen_label_rtx ();
  rtx tmpcount = gen_reg_rtx (GET_MODE (variable));
  if (GET_MODE (variable) == DImode)
    emit_insn (gen_anddi3 (tmpcount, variable, GEN_INT (value)));
  else
    emit_insn (gen_andsi3 (tmpcount, variable, GEN_INT (value)));
  emit_cmp_and_jump_insns (tmpcount, const0_rtx, EQ, 0, GET_MODE (variable),
			   1, label);
  if (epilogue)
    predict_jump (REG_BR_PROB_BASE * 50 / 100);
  else
    predict_jump (REG_BR_PROB_BASE * 90 / 100);
  return label;
}

/* Adjust COUNTER by the VALUE.  */
static void
ix86_adjust_counter (rtx countreg, HOST_WIDE_INT value)
{
  if (GET_MODE (countreg) == DImode)
    emit_insn (gen_adddi3 (countreg, countreg, GEN_INT (-value)));
  else
    emit_insn (gen_addsi3 (countreg, countreg, GEN_INT (-value)));
}

/* Zero extend possibly SImode EXP to Pmode register.  */
rtx
ix86_zero_extend_to_Pmode (rtx exp)
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

/* Divide COUNTREG by SCALE.  */
static rtx
scale_counter (rtx countreg, int scale)
{
  rtx sc;
  rtx piece_size_mask;

  if (scale == 1)
    return countreg;
  if (CONST_INT_P (countreg))
    return GEN_INT (INTVAL (countreg) / scale);
  gcc_assert (REG_P (countreg));

  piece_size_mask = GEN_INT (scale - 1);
  sc = expand_simple_binop (GET_MODE (countreg), LSHIFTRT, countreg,
			    GEN_INT (exact_log2 (scale)),
			    NULL, 1, OPTAB_DIRECT);
  return sc;
}

/* Return mode for the memcpy/memset loop counter.  Prefer SImode over
   DImode for constant loop counts.  */

static enum machine_mode
counter_mode (rtx count_exp)
{
  if (GET_MODE (count_exp) != VOIDmode)
    return GET_MODE (count_exp);
  if (GET_CODE (count_exp) != CONST_INT)
    return Pmode;
  if (TARGET_64BIT && (INTVAL (count_exp) & ~0xffffffff))
    return DImode;
  return SImode;
}

/* When SRCPTR is non-NULL, output simple loop to move memory
   pointer to SRCPTR to DESTPTR via chunks of MODE unrolled UNROLL times,
   overall size is COUNT specified in bytes.  When SRCPTR is NULL, output the
   equivalent loop to set memory by VALUE (supposed to be in MODE).

   The size is rounded down to whole number of chunk size moved at once.
   SRCMEM and DESTMEM provide MEMrtx to feed proper aliasing info.  */


static void
expand_set_or_movmem_via_loop (rtx destmem, rtx srcmem,
			       rtx destptr, rtx srcptr, rtx value,
			       rtx count, enum machine_mode mode, int unroll,
			       int expected_size)
{
  rtx out_label, top_label, iter, tmp;
  enum machine_mode iter_mode = counter_mode (count);
  rtx piece_size = GEN_INT (GET_MODE_SIZE (mode) * unroll);
  rtx piece_size_mask = GEN_INT (~((GET_MODE_SIZE (mode) * unroll) - 1));
  rtx size;
  rtx x_addr;
  rtx y_addr;
  int i;

  top_label = gen_label_rtx ();
  out_label = gen_label_rtx ();
  iter = gen_reg_rtx (iter_mode);

  size = expand_simple_binop (iter_mode, AND, count, piece_size_mask,
			      NULL, 1, OPTAB_DIRECT);
  /* Those two should combine.  */
  if (piece_size == const1_rtx)
    {
      emit_cmp_and_jump_insns (size, const0_rtx, EQ, NULL_RTX, iter_mode,
			       true, out_label);
      predict_jump (REG_BR_PROB_BASE * 10 / 100);
    }
  emit_move_insn (iter, const0_rtx);

  emit_label (top_label);

  tmp = convert_modes (Pmode, iter_mode, iter, true);
  x_addr = gen_rtx_PLUS (Pmode, destptr, tmp);
  destmem = change_address (destmem, mode, x_addr);

  if (srcmem)
    {
      y_addr = gen_rtx_PLUS (Pmode, srcptr, copy_rtx (tmp));
      srcmem = change_address (srcmem, mode, y_addr);

      /* When unrolling for chips that reorder memory reads and writes,
	 we can save registers by using single temporary.
	 Also using 4 temporaries is overkill in 32bit mode.  */
      if (!TARGET_64BIT && 0)
	{
	  for (i = 0; i < unroll; i++)
	    {
	      if (i)
		{
		  destmem =
		    adjust_address (copy_rtx (destmem), mode, GET_MODE_SIZE (mode));
		  srcmem =
		    adjust_address (copy_rtx (srcmem), mode, GET_MODE_SIZE (mode));
		}
	      emit_move_insn (destmem, srcmem);
	    }
	}
      else
	{
	  rtx tmpreg[4];
	  gcc_assert (unroll <= 4);
	  for (i = 0; i < unroll; i++)
	    {
	      tmpreg[i] = gen_reg_rtx (mode);
	      if (i)
		{
		  srcmem =
		    adjust_address (copy_rtx (srcmem), mode, GET_MODE_SIZE (mode));
		}
	      emit_move_insn (tmpreg[i], srcmem);
	    }
	  for (i = 0; i < unroll; i++)
	    {
	      if (i)
		{
		  destmem =
		    adjust_address (copy_rtx (destmem), mode, GET_MODE_SIZE (mode));
		}
	      emit_move_insn (destmem, tmpreg[i]);
	    }
	}
    }
  else
    for (i = 0; i < unroll; i++)
      {
	if (i)
	  destmem =
	    adjust_address (copy_rtx (destmem), mode, GET_MODE_SIZE (mode));
	emit_move_insn (destmem, value);
      }

  tmp = expand_simple_binop (iter_mode, PLUS, iter, piece_size, iter,
			     true, OPTAB_LIB_WIDEN);
  if (tmp != iter)
    emit_move_insn (iter, tmp);

  emit_cmp_and_jump_insns (iter, size, LT, NULL_RTX, iter_mode,
			   true, top_label);
  if (expected_size != -1)
    {
      expected_size /= GET_MODE_SIZE (mode) * unroll;
      if (expected_size == 0)
	predict_jump (0);
      else if (expected_size > REG_BR_PROB_BASE)
	predict_jump (REG_BR_PROB_BASE - 1);
      else
        predict_jump (REG_BR_PROB_BASE - (REG_BR_PROB_BASE + expected_size / 2) / expected_size);
    }
  else
    predict_jump (REG_BR_PROB_BASE * 80 / 100);
  iter = ix86_zero_extend_to_Pmode (iter);
  tmp = expand_simple_binop (Pmode, PLUS, destptr, iter, destptr,
			     true, OPTAB_LIB_WIDEN);
  if (tmp != destptr)
    emit_move_insn (destptr, tmp);
  if (srcptr)
    {
      tmp = expand_simple_binop (Pmode, PLUS, srcptr, iter, srcptr,
				 true, OPTAB_LIB_WIDEN);
      if (tmp != srcptr)
	emit_move_insn (srcptr, tmp);
    }
  emit_label (out_label);
}

/* Output "rep; mov" instruction.
   Arguments have same meaning as for previous function */
static void
expand_movmem_via_rep_mov (rtx destmem, rtx srcmem,
			   rtx destptr, rtx srcptr,
			   rtx count,
			   enum machine_mode mode)
{
  rtx destexp;
  rtx srcexp;
  rtx countreg;

  /* If the size is known, it is shorter to use rep movs.  */
  if (mode == QImode && CONST_INT_P (count)
      && !(INTVAL (count) & 3))
    mode = SImode;

  if (destptr != XEXP (destmem, 0) || GET_MODE (destmem) != BLKmode)
    destmem = adjust_automodify_address_nv (destmem, BLKmode, destptr, 0);
  if (srcptr != XEXP (srcmem, 0) || GET_MODE (srcmem) != BLKmode)
    srcmem = adjust_automodify_address_nv (srcmem, BLKmode, srcptr, 0);
  countreg = ix86_zero_extend_to_Pmode (scale_counter (count, GET_MODE_SIZE (mode)));
  if (mode != QImode)
    {
      destexp = gen_rtx_ASHIFT (Pmode, countreg,
				GEN_INT (exact_log2 (GET_MODE_SIZE (mode))));
      destexp = gen_rtx_PLUS (Pmode, destexp, destptr);
      srcexp = gen_rtx_ASHIFT (Pmode, countreg,
			       GEN_INT (exact_log2 (GET_MODE_SIZE (mode))));
      srcexp = gen_rtx_PLUS (Pmode, srcexp, srcptr);
    }
  else
    {
      destexp = gen_rtx_PLUS (Pmode, destptr, countreg);
      srcexp = gen_rtx_PLUS (Pmode, srcptr, countreg);
    }
  if (CONST_INT_P (count))
    {
      count = GEN_INT (INTVAL (count)
		       & ~((HOST_WIDE_INT) GET_MODE_SIZE (mode) - 1));
      destmem = shallow_copy_rtx (destmem);
      srcmem = shallow_copy_rtx (srcmem);
      set_mem_size (destmem, count);
      set_mem_size (srcmem, count);
    }
  else
    {
      if (MEM_SIZE (destmem))
	set_mem_size (destmem, NULL_RTX);
      if (MEM_SIZE (srcmem))
	set_mem_size (srcmem, NULL_RTX);
    }
  emit_insn (gen_rep_mov (destptr, destmem, srcptr, srcmem, countreg,
			  destexp, srcexp));
}

/* Output "rep; stos" instruction.
   Arguments have same meaning as for previous function */
static void
expand_setmem_via_rep_stos (rtx destmem, rtx destptr, rtx value,
			    rtx count, enum machine_mode mode,
			    rtx orig_value)
{
  rtx destexp;
  rtx countreg;

  if (destptr != XEXP (destmem, 0) || GET_MODE (destmem) != BLKmode)
    destmem = adjust_automodify_address_nv (destmem, BLKmode, destptr, 0);
  value = force_reg (mode, gen_lowpart (mode, value));
  countreg = ix86_zero_extend_to_Pmode (scale_counter (count, GET_MODE_SIZE (mode)));
  if (mode != QImode)
    {
      destexp = gen_rtx_ASHIFT (Pmode, countreg,
				GEN_INT (exact_log2 (GET_MODE_SIZE (mode))));
      destexp = gen_rtx_PLUS (Pmode, destexp, destptr);
    }
  else
    destexp = gen_rtx_PLUS (Pmode, destptr, countreg);
  if (orig_value == const0_rtx && CONST_INT_P (count))
    {
      count = GEN_INT (INTVAL (count)
		       & ~((HOST_WIDE_INT) GET_MODE_SIZE (mode) - 1));
      destmem = shallow_copy_rtx (destmem);
      set_mem_size (destmem, count);
    }
  else if (MEM_SIZE (destmem))
    set_mem_size (destmem, NULL_RTX);
  emit_insn (gen_rep_stos (destptr, countreg, destmem, value, destexp));
}

static void
emit_strmov (rtx destmem, rtx srcmem,
	     rtx destptr, rtx srcptr, enum machine_mode mode, int offset)
{
  rtx src = adjust_automodify_address_nv (srcmem, mode, srcptr, offset);
  rtx dest = adjust_automodify_address_nv (destmem, mode, destptr, offset);
  emit_insn (gen_strmov (destptr, dest, srcptr, src));
}

/* Output code to copy at most count & (max_size - 1) bytes from SRC to DEST.  */
static void
expand_movmem_epilogue (rtx destmem, rtx srcmem,
			rtx destptr, rtx srcptr, rtx count, int max_size)
{
  rtx src, dest;
  if (CONST_INT_P (count))
    {
      HOST_WIDE_INT countval = INTVAL (count);
      int offset = 0;

      if ((countval & 0x10) && max_size > 16)
	{
	  if (TARGET_64BIT)
	    {
	      emit_strmov (destmem, srcmem, destptr, srcptr, DImode, offset);
	      emit_strmov (destmem, srcmem, destptr, srcptr, DImode, offset + 8);
	    }
	  else
	    gcc_unreachable ();
	  offset += 16;
	}
      if ((countval & 0x08) && max_size > 8)
	{
	  if (TARGET_64BIT)
	    emit_strmov (destmem, srcmem, destptr, srcptr, DImode, offset);
	  else
	    {
	      emit_strmov (destmem, srcmem, destptr, srcptr, SImode, offset);
	      emit_strmov (destmem, srcmem, destptr, srcptr, SImode, offset + 4);
	    }
	  offset += 8;
	}
      if ((countval & 0x04) && max_size > 4)
	{
          emit_strmov (destmem, srcmem, destptr, srcptr, SImode, offset);
	  offset += 4;
	}
      if ((countval & 0x02) && max_size > 2)
	{
          emit_strmov (destmem, srcmem, destptr, srcptr, HImode, offset);
	  offset += 2;
	}
      if ((countval & 0x01) && max_size > 1)
	{
          emit_strmov (destmem, srcmem, destptr, srcptr, QImode, offset);
	  offset += 1;
	}
      return;
    }
  if (max_size > 8)
    {
      count = expand_simple_binop (GET_MODE (count), AND, count, GEN_INT (max_size - 1),
				    count, 1, OPTAB_DIRECT);
      expand_set_or_movmem_via_loop (destmem, srcmem, destptr, srcptr, NULL,
				     count, QImode, 1, 4);
      return;
    }

  /* When there are stringops, we can cheaply increase dest and src pointers.
     Otherwise we save code size by maintaining offset (zero is readily
     available from preceding rep operation) and using x86 addressing modes.
   */
  if (TARGET_SINGLE_STRINGOP)
    {
      if (max_size > 4)
	{
	  rtx label = ix86_expand_aligntest (count, 4, true);
	  src = change_address (srcmem, SImode, srcptr);
	  dest = change_address (destmem, SImode, destptr);
	  emit_insn (gen_strmov (destptr, dest, srcptr, src));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 2)
	{
	  rtx label = ix86_expand_aligntest (count, 2, true);
	  src = change_address (srcmem, HImode, srcptr);
	  dest = change_address (destmem, HImode, destptr);
	  emit_insn (gen_strmov (destptr, dest, srcptr, src));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 1)
	{
	  rtx label = ix86_expand_aligntest (count, 1, true);
	  src = change_address (srcmem, QImode, srcptr);
	  dest = change_address (destmem, QImode, destptr);
	  emit_insn (gen_strmov (destptr, dest, srcptr, src));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
    }
  else
    {
      rtx offset = force_reg (Pmode, const0_rtx);
      rtx tmp;

      if (max_size > 4)
	{
	  rtx label = ix86_expand_aligntest (count, 4, true);
	  src = change_address (srcmem, SImode, srcptr);
	  dest = change_address (destmem, SImode, destptr);
	  emit_move_insn (dest, src);
	  tmp = expand_simple_binop (Pmode, PLUS, offset, GEN_INT (4), NULL,
				     true, OPTAB_LIB_WIDEN);
	  if (tmp != offset)
	    emit_move_insn (offset, tmp);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 2)
	{
	  rtx label = ix86_expand_aligntest (count, 2, true);
	  tmp = gen_rtx_PLUS (Pmode, srcptr, offset);
	  src = change_address (srcmem, HImode, tmp);
	  tmp = gen_rtx_PLUS (Pmode, destptr, offset);
	  dest = change_address (destmem, HImode, tmp);
	  emit_move_insn (dest, src);
	  tmp = expand_simple_binop (Pmode, PLUS, offset, GEN_INT (2), tmp,
				     true, OPTAB_LIB_WIDEN);
	  if (tmp != offset)
	    emit_move_insn (offset, tmp);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 1)
	{
	  rtx label = ix86_expand_aligntest (count, 1, true);
	  tmp = gen_rtx_PLUS (Pmode, srcptr, offset);
	  src = change_address (srcmem, QImode, tmp);
	  tmp = gen_rtx_PLUS (Pmode, destptr, offset);
	  dest = change_address (destmem, QImode, tmp);
	  emit_move_insn (dest, src);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
    }
}

/* Output code to set at most count & (max_size - 1) bytes starting by DEST.  */
static void
expand_setmem_epilogue_via_loop (rtx destmem, rtx destptr, rtx value,
				 rtx count, int max_size)
{
  count =
    expand_simple_binop (counter_mode (count), AND, count,
			 GEN_INT (max_size - 1), count, 1, OPTAB_DIRECT);
  expand_set_or_movmem_via_loop (destmem, NULL, destptr, NULL,
				 gen_lowpart (QImode, value), count, QImode,
				 1, max_size / 2);
}

/* Output code to set at most count & (max_size - 1) bytes starting by DEST.  */
static void
expand_setmem_epilogue (rtx destmem, rtx destptr, rtx value, rtx count, int max_size)
{
  rtx dest;

  if (CONST_INT_P (count))
    {
      HOST_WIDE_INT countval = INTVAL (count);
      int offset = 0;

      if ((countval & 0x10) && max_size > 16)
	{
	  if (TARGET_64BIT)
	    {
	      dest = adjust_automodify_address_nv (destmem, DImode, destptr, offset);
	      emit_insn (gen_strset (destptr, dest, value));
	      dest = adjust_automodify_address_nv (destmem, DImode, destptr, offset + 8);
	      emit_insn (gen_strset (destptr, dest, value));
	    }
	  else
	    gcc_unreachable ();
	  offset += 16;
	}
      if ((countval & 0x08) && max_size > 8)
	{
	  if (TARGET_64BIT)
	    {
	      dest = adjust_automodify_address_nv (destmem, DImode, destptr, offset);
	      emit_insn (gen_strset (destptr, dest, value));
	    }
	  else
	    {
	      dest = adjust_automodify_address_nv (destmem, SImode, destptr, offset);
	      emit_insn (gen_strset (destptr, dest, value));
	      dest = adjust_automodify_address_nv (destmem, SImode, destptr, offset + 4);
	      emit_insn (gen_strset (destptr, dest, value));
	    }
	  offset += 8;
	}
      if ((countval & 0x04) && max_size > 4)
	{
	  dest = adjust_automodify_address_nv (destmem, SImode, destptr, offset);
	  emit_insn (gen_strset (destptr, dest, gen_lowpart (SImode, value)));
	  offset += 4;
	}
      if ((countval & 0x02) && max_size > 2)
	{
	  dest = adjust_automodify_address_nv (destmem, HImode, destptr, offset);
	  emit_insn (gen_strset (destptr, dest, gen_lowpart (HImode, value)));
	  offset += 2;
	}
      if ((countval & 0x01) && max_size > 1)
	{
	  dest = adjust_automodify_address_nv (destmem, QImode, destptr, offset);
	  emit_insn (gen_strset (destptr, dest, gen_lowpart (QImode, value)));
	  offset += 1;
	}
      return;
    }
  if (max_size > 32)
    {
      expand_setmem_epilogue_via_loop (destmem, destptr, value, count, max_size);
      return;
    }
  if (max_size > 16)
    {
      rtx label = ix86_expand_aligntest (count, 16, true);
      if (TARGET_64BIT)
	{
	  dest = change_address (destmem, DImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	  emit_insn (gen_strset (destptr, dest, value));
	}
      else
	{
	  dest = change_address (destmem, SImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	  emit_insn (gen_strset (destptr, dest, value));
	  emit_insn (gen_strset (destptr, dest, value));
	  emit_insn (gen_strset (destptr, dest, value));
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 8)
    {
      rtx label = ix86_expand_aligntest (count, 8, true);
      if (TARGET_64BIT)
	{
	  dest = change_address (destmem, DImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	}
      else
	{
	  dest = change_address (destmem, SImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	  emit_insn (gen_strset (destptr, dest, value));
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 4)
    {
      rtx label = ix86_expand_aligntest (count, 4, true);
      dest = change_address (destmem, SImode, destptr);
      emit_insn (gen_strset (destptr, dest, gen_lowpart (SImode, value)));
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 2)
    {
      rtx label = ix86_expand_aligntest (count, 2, true);
      dest = change_address (destmem, HImode, destptr);
      emit_insn (gen_strset (destptr, dest, gen_lowpart (HImode, value)));
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 1)
    {
      rtx label = ix86_expand_aligntest (count, 1, true);
      dest = change_address (destmem, QImode, destptr);
      emit_insn (gen_strset (destptr, dest, gen_lowpart (QImode, value)));
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
}

/* Copy enough from DEST to SRC to align DEST known to by aligned by ALIGN to
   DESIRED_ALIGNMENT.  */
static void
expand_movmem_prologue (rtx destmem, rtx srcmem,
			rtx destptr, rtx srcptr, rtx count,
			int align, int desired_alignment)
{
  if (align <= 1 && desired_alignment > 1)
    {
      rtx label = ix86_expand_aligntest (destptr, 1, false);
      srcmem = change_address (srcmem, QImode, srcptr);
      destmem = change_address (destmem, QImode, destptr);
      emit_insn (gen_strmov (destptr, destmem, srcptr, srcmem));
      ix86_adjust_counter (count, 1);
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (align <= 2 && desired_alignment > 2)
    {
      rtx label = ix86_expand_aligntest (destptr, 2, false);
      srcmem = change_address (srcmem, HImode, srcptr);
      destmem = change_address (destmem, HImode, destptr);
      emit_insn (gen_strmov (destptr, destmem, srcptr, srcmem));
      ix86_adjust_counter (count, 2);
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (align <= 4 && desired_alignment > 4)
    {
      rtx label = ix86_expand_aligntest (destptr, 4, false);
      srcmem = change_address (srcmem, SImode, srcptr);
      destmem = change_address (destmem, SImode, destptr);
      emit_insn (gen_strmov (destptr, destmem, srcptr, srcmem));
      ix86_adjust_counter (count, 4);
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  gcc_assert (desired_alignment <= 8);
}

/* Set enough from DEST to align DEST known to by aligned by ALIGN to
   DESIRED_ALIGNMENT.  */
static void
expand_setmem_prologue (rtx destmem, rtx destptr, rtx value, rtx count,
			int align, int desired_alignment)
{
  if (align <= 1 && desired_alignment > 1)
    {
      rtx label = ix86_expand_aligntest (destptr, 1, false);
      destmem = change_address (destmem, QImode, destptr);
      emit_insn (gen_strset (destptr, destmem, gen_lowpart (QImode, value)));
      ix86_adjust_counter (count, 1);
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (align <= 2 && desired_alignment > 2)
    {
      rtx label = ix86_expand_aligntest (destptr, 2, false);
      destmem = change_address (destmem, HImode, destptr);
      emit_insn (gen_strset (destptr, destmem, gen_lowpart (HImode, value)));
      ix86_adjust_counter (count, 2);
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (align <= 4 && desired_alignment > 4)
    {
      rtx label = ix86_expand_aligntest (destptr, 4, false);
      destmem = change_address (destmem, SImode, destptr);
      emit_insn (gen_strset (destptr, destmem, gen_lowpart (SImode, value)));
      ix86_adjust_counter (count, 4);
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  gcc_assert (desired_alignment <= 8);
}

/* Given COUNT and EXPECTED_SIZE, decide on codegen of string operation.  */
static enum stringop_alg
decide_alg (HOST_WIDE_INT count, HOST_WIDE_INT expected_size, bool memset,
	    int *dynamic_check)
{
  const struct stringop_algs * algs;
  /* Algorithms using the rep prefix want at least edi and ecx;
     additionally, memset wants eax and memcpy wants esi.  Don't
     consider such algorithms if the user has appropriated those
     registers for their own purposes.	*/
  bool rep_prefix_usable = !(fixed_regs[CX_REG] || fixed_regs[DI_REG]
                             || (memset
				 ? fixed_regs[AX_REG] : fixed_regs[SI_REG]));

#define ALG_USABLE_P(alg) (rep_prefix_usable			\
			   || (alg != rep_prefix_1_byte		\
			       && alg != rep_prefix_4_byte      \
			       && alg != rep_prefix_8_byte))

  *dynamic_check = -1;
  if (memset)
    algs = &ix86_cost->memset[TARGET_64BIT != 0];
  else
    algs = &ix86_cost->memcpy[TARGET_64BIT != 0];
  if (stringop_alg != no_stringop && ALG_USABLE_P (stringop_alg))
    return stringop_alg;
  /* rep; movq or rep; movl is the smallest variant.  */
  else if (optimize_size)
    {
      if (!count || (count & 3))
	return rep_prefix_usable ? rep_prefix_1_byte : loop_1_byte;
      else
	return rep_prefix_usable ? rep_prefix_4_byte : loop;
    }
  /* Very tiny blocks are best handled via the loop, REP is expensive to setup.
   */
  else if (expected_size != -1 && expected_size < 4)
    return loop_1_byte;
  else if (expected_size != -1)
    {
      unsigned int i;
      enum stringop_alg alg = libcall;
      for (i = 0; i < NAX_STRINGOP_ALGS; i++)
	{
	  /* We get here if the algorithms that were not libcall-based
	     were rep-prefix based and we are unable to use rep prefixes
	     based on global register usage.  Break out of the loop and
	     use the heuristic below.  */
	  if (algs->size[i].max == 0)
	    break;
	  if (algs->size[i].max >= expected_size || algs->size[i].max == -1)
	    {
	      enum stringop_alg candidate = algs->size[i].alg;

	      if (candidate != libcall && ALG_USABLE_P (candidate))
		alg = candidate;
	      /* Honor TARGET_INLINE_ALL_STRINGOPS by picking
		 last non-libcall inline algorithm.  */
	      if (TARGET_INLINE_ALL_STRINGOPS)
		{
		  /* When the current size is best to be copied by a libcall,
		     but we are still forced to inline, run the heuristic below
		     that will pick code for medium sized blocks.  */
		  if (alg != libcall)
		    return alg;
		  break;
		}
	      else if (ALG_USABLE_P (candidate))
		return candidate;
	    }
	}
      gcc_assert (TARGET_INLINE_ALL_STRINGOPS || !rep_prefix_usable);
    }
  /* When asked to inline the call anyway, try to pick meaningful choice.
     We look for maximal size of block that is faster to copy by hand and
     take blocks of at most of that size guessing that average size will
     be roughly half of the block.

     If this turns out to be bad, we might simply specify the preferred
     choice in ix86_costs.  */
  if ((TARGET_INLINE_ALL_STRINGOPS || TARGET_INLINE_STRINGOPS_DYNAMICALLY)
      && (algs->unknown_size == libcall || !ALG_USABLE_P (algs->unknown_size)))
    {
      int max = -1;
      enum stringop_alg alg;
      int i;
      bool any_alg_usable_p = true;

      for (i = 0; i < NAX_STRINGOP_ALGS; i++)
        {
          enum stringop_alg candidate = algs->size[i].alg;
          any_alg_usable_p = any_alg_usable_p && ALG_USABLE_P (candidate);

          if (candidate != libcall && candidate
              && ALG_USABLE_P (candidate))
              max = algs->size[i].max;
        }
      /* If there aren't any usable algorithms, then recursing on
         smaller sizes isn't going to find anything.  Just return the
         simple byte-at-a-time copy loop.  */
      if (!any_alg_usable_p)
        {
          /* Pick something reasonable.  */
          if (TARGET_INLINE_STRINGOPS_DYNAMICALLY)
            *dynamic_check = 128;
          return loop_1_byte;
        }
      if (max == -1)
	max = 4096;
      alg = decide_alg (count, max / 2, memset, dynamic_check);
      gcc_assert (*dynamic_check == -1);
      gcc_assert (alg != libcall);
      if (TARGET_INLINE_STRINGOPS_DYNAMICALLY)
	*dynamic_check = max;
      return alg;
    }
  return ALG_USABLE_P (algs->unknown_size) ? algs->unknown_size : libcall;
#undef ALG_USABLE_P
}

/* Decide on alignment.  We know that the operand is already aligned to ALIGN
   (ALIGN can be based on profile feedback and thus it is not 100% guaranteed).  */
static int
decide_alignment (int align,
		  enum stringop_alg alg,
		  int expected_size)
{
  int desired_align = 0;
  switch (alg)
    {
      case no_stringop:
	gcc_unreachable ();
      case loop:
      case unrolled_loop:
	desired_align = GET_MODE_SIZE (Pmode);
	break;
      case rep_prefix_8_byte:
	desired_align = 8;
	break;
      case rep_prefix_4_byte:
	/* PentiumPro has special logic triggering for 8 byte aligned blocks.
	   copying whole cacheline at once.  */
	if (TARGET_PENTIUMPRO)
	  desired_align = 8;
	else
	  desired_align = 4;
	break;
      case rep_prefix_1_byte:
	/* PentiumPro has special logic triggering for 8 byte aligned blocks.
	   copying whole cacheline at once.  */
	if (TARGET_PENTIUMPRO)
	  desired_align = 8;
	else
	  desired_align = 1;
	break;
      case loop_1_byte:
	desired_align = 1;
	break;
      case libcall:
	return 0;
    }

  if (optimize_size)
    desired_align = 1;
  if (desired_align < align)
    desired_align = align;
  if (expected_size != -1 && expected_size < 4)
    desired_align = align;
  return desired_align;
}

/* Return the smallest power of 2 greater than VAL.  */
static int
smallest_pow2_greater_than (int val)
{
  int ret = 1;
  while (ret <= val)
    ret <<= 1;
  return ret;
}

/* Expand string move (memcpy) operation.  Use i386 string operations when
   profitable.  expand_setmem contains similar code.  The code depends upon
   architecture, block size and alignment, but always has the same
   overall structure:

   1) Prologue guard: Conditional that jumps up to epilogues for small
      blocks that can be handled by epilogue alone.  This is faster but
      also needed for correctness, since prologue assume the block is larger
      than the desired alignment.

      Optional dynamic check for size and libcall for large
      blocks is emitted here too, with -minline-stringops-dynamically.

   2) Prologue: copy first few bytes in order to get destination aligned
      to DESIRED_ALIGN.  It is emitted only when ALIGN is less than
      DESIRED_ALIGN and and up to DESIRED_ALIGN - ALIGN bytes can be copied.
      We emit either a jump tree on power of two sized blocks, or a byte loop.

   3) Main body: the copying loop itself, copying in SIZE_NEEDED chunks
      with specified algorithm.

   4) Epilogue: code copying tail of the block that is too small to be
      handled by main body (or up to size guarded by prologue guard).  */

int
ix86_expand_movmem (rtx dst, rtx src, rtx count_exp, rtx align_exp,
		    rtx expected_align_exp, rtx expected_size_exp)
{
  rtx destreg;
  rtx srcreg;
  rtx label = NULL;
  rtx tmp;
  rtx jump_around_label = NULL;
  HOST_WIDE_INT align = 1;
  unsigned HOST_WIDE_INT count = 0;
  HOST_WIDE_INT expected_size = -1;
  int size_needed = 0, epilogue_size_needed;
  int desired_align = 0;
  enum stringop_alg alg;
  int dynamic_check;

  if (CONST_INT_P (align_exp))
    align = INTVAL (align_exp);
  /* i386 can do misaligned access on reasonably increased cost.  */
  if (CONST_INT_P (expected_align_exp)
      && INTVAL (expected_align_exp) > align)
    align = INTVAL (expected_align_exp);
  if (CONST_INT_P (count_exp))
    count = expected_size = INTVAL (count_exp);
  if (CONST_INT_P (expected_size_exp) && count == 0)
    expected_size = INTVAL (expected_size_exp);

  /* Make sure we don't need to care about overflow later on.  */
  if (count > ((unsigned HOST_WIDE_INT) 1 << 30))
    return 0;

  /* Step 0: Decide on preferred algorithm, desired alignment and
     size of chunks to be copied by main loop.  */

  alg = decide_alg (count, expected_size, false, &dynamic_check);
  desired_align = decide_alignment (align, alg, expected_size);

  if (!TARGET_ALIGN_STRINGOPS)
    align = desired_align;

  if (alg == libcall)
    return 0;
  gcc_assert (alg != no_stringop);
  if (!count)
    count_exp = copy_to_mode_reg (GET_MODE (count_exp), count_exp);
  destreg = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  srcreg = copy_to_mode_reg (Pmode, XEXP (src, 0));
  switch (alg)
    {
    case libcall:
    case no_stringop:
      gcc_unreachable ();
    case loop:
      size_needed = GET_MODE_SIZE (Pmode);
      break;
    case unrolled_loop:
      size_needed = GET_MODE_SIZE (Pmode) * (TARGET_64BIT ? 4 : 2);
      break;
    case rep_prefix_8_byte:
      size_needed = 8;
      break;
    case rep_prefix_4_byte:
      size_needed = 4;
      break;
    case rep_prefix_1_byte:
    case loop_1_byte:
      size_needed = 1;
      break;
    }

  epilogue_size_needed = size_needed;

  /* Step 1: Prologue guard.  */

  /* Alignment code needs count to be in register.  */
  if (CONST_INT_P (count_exp) && desired_align > align)
    count_exp = force_reg (counter_mode (count_exp), count_exp);
  gcc_assert (desired_align >= 1 && align >= 1);

  /* Ensure that alignment prologue won't copy past end of block.  */
  if (size_needed > 1 || (desired_align > 1 && desired_align > align))
    {
      epilogue_size_needed = MAX (size_needed - 1, desired_align - align);
      /* Epilogue always copies COUNT_EXP & EPILOGUE_SIZE_NEEDED bytes.
	 Make sure it is power of 2.  */
      epilogue_size_needed = smallest_pow2_greater_than (epilogue_size_needed);

      if (CONST_INT_P (count_exp))
	{
	  if (UINTVAL (count_exp) < (unsigned HOST_WIDE_INT)epilogue_size_needed)
	    goto epilogue;
	}
      else
	{
	  label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (count_exp,
				   GEN_INT (epilogue_size_needed),
				   LTU, 0, counter_mode (count_exp), 1, label);
	  if (expected_size == -1 || expected_size < epilogue_size_needed)
	    predict_jump (REG_BR_PROB_BASE * 60 / 100);
	  else
	    predict_jump (REG_BR_PROB_BASE * 20 / 100);
	}
    }

  /* Emit code to decide on runtime whether library call or inline should be
     used.  */
  if (dynamic_check != -1)
    {
      if (CONST_INT_P (count_exp))
	{
	  if (UINTVAL (count_exp) >= (unsigned HOST_WIDE_INT)dynamic_check)
	    {
	      emit_block_move_via_libcall (dst, src, count_exp, false);
	      count_exp = const0_rtx;
	      goto epilogue;
	    }
	}
      else
	{
	  rtx hot_label = gen_label_rtx ();
	  jump_around_label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (count_exp, GEN_INT (dynamic_check - 1),
				   LEU, 0, GET_MODE (count_exp), 1, hot_label);
	  predict_jump (REG_BR_PROB_BASE * 90 / 100);
	  emit_block_move_via_libcall (dst, src, count_exp, false);
	  emit_jump (jump_around_label);
	  emit_label (hot_label);
	}
    }

  /* Step 2: Alignment prologue.  */

  if (desired_align > align)
    {
      /* Except for the first move in epilogue, we no longer know
         constant offset in aliasing info.  It don't seems to worth
	 the pain to maintain it for the first move, so throw away
	 the info early.  */
      src = change_address (src, BLKmode, srcreg);
      dst = change_address (dst, BLKmode, destreg);
      expand_movmem_prologue (dst, src, destreg, srcreg, count_exp, align,
			      desired_align);
    }
  if (label && size_needed == 1)
    {
      emit_label (label);
      LABEL_NUSES (label) = 1;
      label = NULL;
    }

  /* Step 3: Main loop.  */

  switch (alg)
    {
    case libcall:
    case no_stringop:
      gcc_unreachable ();
    case loop_1_byte:
      expand_set_or_movmem_via_loop (dst, src, destreg, srcreg, NULL,
				     count_exp, QImode, 1, expected_size);
      break;
    case loop:
      expand_set_or_movmem_via_loop (dst, src, destreg, srcreg, NULL,
				     count_exp, Pmode, 1, expected_size);
      break;
    case unrolled_loop:
      /* Unroll only by factor of 2 in 32bit mode, since we don't have enough
	 registers for 4 temporaries anyway.  */
      expand_set_or_movmem_via_loop (dst, src, destreg, srcreg, NULL,
				     count_exp, Pmode, TARGET_64BIT ? 4 : 2,
				     expected_size);
      break;
    case rep_prefix_8_byte:
      expand_movmem_via_rep_mov (dst, src, destreg, srcreg, count_exp,
				 DImode);
      break;
    case rep_prefix_4_byte:
      expand_movmem_via_rep_mov (dst, src, destreg, srcreg, count_exp,
				 SImode);
      break;
    case rep_prefix_1_byte:
      expand_movmem_via_rep_mov (dst, src, destreg, srcreg, count_exp,
				 QImode);
      break;
    }
  /* Adjust properly the offset of src and dest memory for aliasing.  */
  if (CONST_INT_P (count_exp))
    {
      src = adjust_automodify_address_nv (src, BLKmode, srcreg,
					  (count / size_needed) * size_needed);
      dst = adjust_automodify_address_nv (dst, BLKmode, destreg,
					  (count / size_needed) * size_needed);
    }
  else
    {
      src = change_address (src, BLKmode, srcreg);
      dst = change_address (dst, BLKmode, destreg);
    }

  /* Step 4: Epilogue to copy the remaining bytes.  */
 epilogue:
  if (label)
    {
      /* When the main loop is done, COUNT_EXP might hold original count,
 	 while we want to copy only COUNT_EXP & SIZE_NEEDED bytes.
	 Epilogue code will actually copy COUNT_EXP & EPILOGUE_SIZE_NEEDED
	 bytes. Compensate if needed.  */

      if (size_needed < epilogue_size_needed)
	{
	  tmp =
	    expand_simple_binop (counter_mode (count_exp), AND, count_exp,
				 GEN_INT (size_needed - 1), count_exp, 1,
				 OPTAB_DIRECT);
	  if (tmp != count_exp)
	    emit_move_insn (count_exp, tmp);
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }

  if (count_exp != const0_rtx && epilogue_size_needed > 1)
    expand_movmem_epilogue (dst, src, destreg, srcreg, count_exp,
			    epilogue_size_needed);
  if (jump_around_label)
    emit_label (jump_around_label);
  return 1;
}

/* Helper function for memcpy.  For QImode value 0xXY produce
   0xXYXYXYXY of wide specified by MODE.  This is essentially
   a * 0x10101010, but we can do slightly better than
   synth_mult by unwinding the sequence by hand on CPUs with
   slow multiply.  */
static rtx
promote_duplicated_reg (enum machine_mode mode, rtx val)
{
  enum machine_mode valmode = GET_MODE (val);
  rtx tmp;
  int nops = mode == DImode ? 3 : 2;

  gcc_assert (mode == SImode || mode == DImode);
  if (val == const0_rtx)
    return copy_to_mode_reg (mode, const0_rtx);
  if (CONST_INT_P (val))
    {
      HOST_WIDE_INT v = INTVAL (val) & 255;

      v |= v << 8;
      v |= v << 16;
      if (mode == DImode)
        v |= (v << 16) << 16;
      return copy_to_mode_reg (mode, gen_int_mode (v, mode));
    }

  if (valmode == VOIDmode)
    valmode = QImode;
  if (valmode != QImode)
    val = gen_lowpart (QImode, val);
  if (mode == QImode)
    return val;
  if (!TARGET_PARTIAL_REG_STALL)
    nops--;
  if (ix86_cost->mult_init[mode == DImode ? 3 : 2]
      + ix86_cost->mult_bit * (mode == DImode ? 8 : 4)
      <= (ix86_cost->shift_const + ix86_cost->add) * nops
          + (COSTS_N_INSNS (TARGET_PARTIAL_REG_STALL == 0)))
    {
      rtx reg = convert_modes (mode, QImode, val, true);
      tmp = promote_duplicated_reg (mode, const1_rtx);
      return expand_simple_binop (mode, MULT, reg, tmp, NULL, 1,
				  OPTAB_DIRECT);
    }
  else
    {
      rtx reg = convert_modes (mode, QImode, val, true);

      if (!TARGET_PARTIAL_REG_STALL)
	if (mode == SImode)
	  emit_insn (gen_movsi_insv_1 (reg, reg));
	else
	  emit_insn (gen_movdi_insv_1_rex64 (reg, reg));
      else
	{
	  tmp = expand_simple_binop (mode, ASHIFT, reg, GEN_INT (8),
				     NULL, 1, OPTAB_DIRECT);
	  reg =
	    expand_simple_binop (mode, IOR, reg, tmp, reg, 1, OPTAB_DIRECT);
	}
      tmp = expand_simple_binop (mode, ASHIFT, reg, GEN_INT (16),
			         NULL, 1, OPTAB_DIRECT);
      reg = expand_simple_binop (mode, IOR, reg, tmp, reg, 1, OPTAB_DIRECT);
      if (mode == SImode)
	return reg;
      tmp = expand_simple_binop (mode, ASHIFT, reg, GEN_INT (32),
				 NULL, 1, OPTAB_DIRECT);
      reg = expand_simple_binop (mode, IOR, reg, tmp, reg, 1, OPTAB_DIRECT);
      return reg;
    }
}

/* Duplicate value VAL using promote_duplicated_reg into maximal size that will
   be needed by main loop copying SIZE_NEEDED chunks and prologue getting
   alignment from ALIGN to DESIRED_ALIGN.  */
static rtx
promote_duplicated_reg_to_size (rtx val, int size_needed, int desired_align, int align)
{
  rtx promoted_val;

  if (TARGET_64BIT
      && (size_needed > 4 || (desired_align > align && desired_align > 4)))
    promoted_val = promote_duplicated_reg (DImode, val);
  else if (size_needed > 2 || (desired_align > align && desired_align > 2))
    promoted_val = promote_duplicated_reg (SImode, val);
  else if (size_needed > 1 || (desired_align > align && desired_align > 1))
    promoted_val = promote_duplicated_reg (HImode, val);
  else
    promoted_val = val;

  return promoted_val;
}

/* Expand string clear operation (bzero).  Use i386 string operations when
   profitable.  See expand_movmem comment for explanation of individual
   steps performed.  */
int
ix86_expand_setmem (rtx dst, rtx count_exp, rtx val_exp, rtx align_exp,
		    rtx expected_align_exp, rtx expected_size_exp)
{
  rtx destreg;
  rtx label = NULL;
  rtx tmp;
  rtx jump_around_label = NULL;
  HOST_WIDE_INT align = 1;
  unsigned HOST_WIDE_INT count = 0;
  HOST_WIDE_INT expected_size = -1;
  int size_needed = 0, epilogue_size_needed;
  int desired_align = 0;
  enum stringop_alg alg;
  rtx promoted_val = NULL;
  bool force_loopy_epilogue = false;
  int dynamic_check;

  if (CONST_INT_P (align_exp))
    align = INTVAL (align_exp);
  /* i386 can do misaligned access on reasonably increased cost.  */
  if (CONST_INT_P (expected_align_exp)
      && INTVAL (expected_align_exp) > align)
    align = INTVAL (expected_align_exp);
  if (CONST_INT_P (count_exp))
    count = expected_size = INTVAL (count_exp);
  if (CONST_INT_P (expected_size_exp) && count == 0)
    expected_size = INTVAL (expected_size_exp);

  /* Make sure we don't need to care about overflow later on.  */
  if (count > ((unsigned HOST_WIDE_INT) 1 << 30))
    return 0;

  /* Step 0: Decide on preferred algorithm, desired alignment and
     size of chunks to be copied by main loop.  */

  alg = decide_alg (count, expected_size, true, &dynamic_check);
  desired_align = decide_alignment (align, alg, expected_size);

  if (!TARGET_ALIGN_STRINGOPS)
    align = desired_align;

  if (alg == libcall)
    return 0;
  gcc_assert (alg != no_stringop);
  if (!count)
    count_exp = copy_to_mode_reg (counter_mode (count_exp), count_exp);
  destreg = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  switch (alg)
    {
    case libcall:
    case no_stringop:
      gcc_unreachable ();
    case loop:
      size_needed = GET_MODE_SIZE (Pmode);
      break;
    case unrolled_loop:
      size_needed = GET_MODE_SIZE (Pmode) * 4;
      break;
    case rep_prefix_8_byte:
      size_needed = 8;
      break;
    case rep_prefix_4_byte:
      size_needed = 4;
      break;
    case rep_prefix_1_byte:
    case loop_1_byte:
      size_needed = 1;
      break;
    }
  epilogue_size_needed = size_needed;

  /* Step 1: Prologue guard.  */

  /* Alignment code needs count to be in register.  */
  if (CONST_INT_P (count_exp) && desired_align > align)
    {
      enum machine_mode mode = SImode;
      if (TARGET_64BIT && (count & ~0xffffffff))
	mode = DImode;
      count_exp = force_reg (mode, count_exp);
    }
  /* Do the cheap promotion to allow better CSE across the
     main loop and epilogue (ie one load of the big constant in the
     front of all code.  */
  if (CONST_INT_P (val_exp))
    promoted_val = promote_duplicated_reg_to_size (val_exp, size_needed,
						   desired_align, align);
  /* Ensure that alignment prologue won't copy past end of block.  */
  if (size_needed > 1 || (desired_align > 1 && desired_align > align))
    {
      epilogue_size_needed = MAX (size_needed - 1, desired_align - align);
      /* Epilogue always copies COUNT_EXP & EPILOGUE_SIZE_NEEDED bytes.
	 Make sure it is power of 2.  */
      epilogue_size_needed = smallest_pow2_greater_than (epilogue_size_needed);

      /* To improve performance of small blocks, we jump around the VAL
	 promoting mode.  This mean that if the promoted VAL is not constant,
	 we might not use it in the epilogue and have to use byte
	 loop variant.  */
      if (epilogue_size_needed > 2 && !promoted_val)
        force_loopy_epilogue = true;
      label = gen_label_rtx ();
      emit_cmp_and_jump_insns (count_exp,
			       GEN_INT (epilogue_size_needed),
			       LTU, 0, counter_mode (count_exp), 1, label);
      if (GET_CODE (count_exp) == CONST_INT)
	;
      else if (expected_size == -1 || expected_size <= epilogue_size_needed)
	predict_jump (REG_BR_PROB_BASE * 60 / 100);
      else
	predict_jump (REG_BR_PROB_BASE * 20 / 100);
    }
  if (dynamic_check != -1)
    {
      rtx hot_label = gen_label_rtx ();
      jump_around_label = gen_label_rtx ();
      emit_cmp_and_jump_insns (count_exp, GEN_INT (dynamic_check - 1),
			       LEU, 0, counter_mode (count_exp), 1, hot_label);
      predict_jump (REG_BR_PROB_BASE * 90 / 100);
      set_storage_via_libcall (dst, count_exp, val_exp, false);
      emit_jump (jump_around_label);
      emit_label (hot_label);
    }

  /* Step 2: Alignment prologue.  */

  /* Do the expensive promotion once we branched off the small blocks.  */
  if (!promoted_val)
    promoted_val = promote_duplicated_reg_to_size (val_exp, size_needed,
						   desired_align, align);
  gcc_assert (desired_align >= 1 && align >= 1);

  if (desired_align > align)
    {
      /* Except for the first move in epilogue, we no longer know
         constant offset in aliasing info.  It don't seems to worth
	 the pain to maintain it for the first move, so throw away
	 the info early.  */
      dst = change_address (dst, BLKmode, destreg);
      expand_setmem_prologue (dst, destreg, promoted_val, count_exp, align,
			      desired_align);
    }
  if (label && size_needed == 1)
    {
      emit_label (label);
      LABEL_NUSES (label) = 1;
      label = NULL;
    }

  /* Step 3: Main loop.  */

  switch (alg)
    {
    case libcall:
    case no_stringop:
      gcc_unreachable ();
    case loop_1_byte:
      expand_set_or_movmem_via_loop (dst, NULL, destreg, NULL, promoted_val,
				     count_exp, QImode, 1, expected_size);
      break;
    case loop:
      expand_set_or_movmem_via_loop (dst, NULL, destreg, NULL, promoted_val,
				     count_exp, Pmode, 1, expected_size);
      break;
    case unrolled_loop:
      expand_set_or_movmem_via_loop (dst, NULL, destreg, NULL, promoted_val,
				     count_exp, Pmode, 4, expected_size);
      break;
    case rep_prefix_8_byte:
      expand_setmem_via_rep_stos (dst, destreg, promoted_val, count_exp,
				  DImode, val_exp);
      break;
    case rep_prefix_4_byte:
      expand_setmem_via_rep_stos (dst, destreg, promoted_val, count_exp,
				  SImode, val_exp);
      break;
    case rep_prefix_1_byte:
      expand_setmem_via_rep_stos (dst, destreg, promoted_val, count_exp,
				  QImode, val_exp);
      break;
    }
  /* Adjust properly the offset of src and dest memory for aliasing.  */
  if (CONST_INT_P (count_exp))
    dst = adjust_automodify_address_nv (dst, BLKmode, destreg,
					(count / size_needed) * size_needed);
  else
    dst = change_address (dst, BLKmode, destreg);

  /* Step 4: Epilogue to copy the remaining bytes.  */

  if (label)
    {
      /* When the main loop is done, COUNT_EXP might hold original count,
 	 while we want to copy only COUNT_EXP & SIZE_NEEDED bytes.
	 Epilogue code will actually copy COUNT_EXP & EPILOGUE_SIZE_NEEDED
	 bytes. Compensate if needed.  */

      if (size_needed < desired_align - align)
	{
	  tmp =
	    expand_simple_binop (counter_mode (count_exp), AND, count_exp,
				 GEN_INT (size_needed - 1), count_exp, 1,
				 OPTAB_DIRECT);
	  size_needed = desired_align - align + 1;
	  if (tmp != count_exp)
	    emit_move_insn (count_exp, tmp);
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (count_exp != const0_rtx && epilogue_size_needed > 1)
    {
      if (force_loopy_epilogue)
	expand_setmem_epilogue_via_loop (dst, destreg, val_exp, count_exp,
					 size_needed);
      else
	expand_setmem_epilogue (dst, destreg, promoted_val, count_exp,
				size_needed);
    }
  if (jump_around_label)
    emit_label (jump_around_label);
  return 1;
}

/* Expand the appropriate insns for doing strlen if not just doing
   repnz; scasb

   out = result, initialized with the start address
   align_rtx = alignment of the address.
   scratch = scratch register, initialized with the startaddress when
	not aligned, otherwise undefined

   This is just the body. It needs the initializations mentioned above and
   some address computing at the end.  These things are done in i386.md.  */

static void
ix86_expand_strlensi_unroll_1 (rtx out, rtx src, rtx align_rtx)
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
  rtx cmp;

  align = 0;
  if (CONST_INT_P (align_rtx))
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
	  emit_cmp_and_jump_insns (align_rtx, const2_rtx, EQ, NULL,
				   Pmode, 1, align_2_label);
	  emit_cmp_and_jump_insns (align_rtx, const2_rtx, GTU, NULL,
				   Pmode, 1, align_3_label);
	}
      else
        {
	  /* Since the alignment is 2, we have to check 2 or 0 bytes;
	     check if is aligned to 4 - byte.  */

	  align_rtx = expand_binop (Pmode, and_optab, scratch1, const2_rtx,
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_cmp_and_jump_insns (align_rtx, const0_rtx, EQ, NULL,
				   Pmode, 1, align_4_label);
        }

      mem = change_address (src, QImode, out);

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

  mem = change_address (src, SImode, out);
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
			       gen_rtx_PLUS (Pmode, out, const2_rtx)));

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
	 emit_insn (gen_adddi3 (out, out, const2_rtx));
       else
	 emit_insn (gen_addsi3 (out, out, const2_rtx));

       emit_label (end_2_label);

    }

  /* Avoid branch in fixing the byte.  */
  tmpreg = gen_lowpart (QImode, tmpreg);
  emit_insn (gen_addqi3_cc (tmpreg, tmpreg, tmpreg));
  cmp = gen_rtx_LTU (Pmode, gen_rtx_REG (CCmode, FLAGS_REG), const0_rtx);
  if (TARGET_64BIT)
    emit_insn (gen_subdi3_carry_rex64 (out, out, GEN_INT (3), cmp));
  else
    emit_insn (gen_subsi3_carry (out, out, GEN_INT (3), cmp));

  emit_label (end_0_label);
}

/* Expand strlen.  */

int
ix86_expand_strlen (rtx out, rtx src, rtx eoschar, rtx align)
{
  rtx addr, scratch1, scratch2, scratch3, scratch4;

  /* The generic case of strlen expander is long.  Avoid it's
     expanding unless TARGET_INLINE_ALL_STRINGOPS.  */

  if (TARGET_UNROLL_STRLEN && eoschar == const0_rtx && optimize > 1
      && !TARGET_INLINE_ALL_STRINGOPS
      && !optimize_size
      && (!CONST_INT_P (align) || INTVAL (align) < 4))
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

      ix86_expand_strlensi_unroll_1 (out, src, align);

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
      rtx unspec;

      /* Can't use this if the user has appropriated eax, ecx, or edi.  */
      if (fixed_regs[AX_REG] || fixed_regs[CX_REG] || fixed_regs[DI_REG])
        return false;

      scratch2 = gen_reg_rtx (Pmode);
      scratch3 = gen_reg_rtx (Pmode);
      scratch4 = force_reg (Pmode, constm1_rtx);

      emit_move_insn (scratch3, addr);
      eoschar = force_reg (QImode, eoschar);

      src = replace_equiv_address_nv (src, scratch3);

      /* If .md starts supporting :P, this can be done in .md.  */
      unspec = gen_rtx_UNSPEC (Pmode, gen_rtvec (4, src, eoschar, align,
						 scratch4), UNSPEC_SCAS);
      emit_insn (gen_strlenqi_1 (scratch1, scratch3, unspec));
      if (TARGET_64BIT)
	{
	  emit_insn (gen_one_cmpldi2 (scratch2, scratch1));
	  emit_insn (gen_adddi3 (out, scratch2, constm1_rtx));
	}
      else
	{
	  emit_insn (gen_one_cmplsi2 (scratch2, scratch1));
	  emit_insn (gen_addsi3 (out, scratch2, constm1_rtx));
	}
    }
  return 1;
}

/* For given symbol (function) construct code to compute address of it's PLT
   entry in large x86-64 PIC model.  */
rtx
construct_plt_address (rtx symbol)
{
  rtx tmp = gen_reg_rtx (Pmode);
  rtx unspec = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, symbol), UNSPEC_PLTOFF);

  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
  gcc_assert (ix86_cmodel == CM_LARGE_PIC);

  emit_move_insn (tmp, gen_rtx_CONST (Pmode, unspec));
  emit_insn (gen_adddi3 (tmp, tmp, pic_offset_table_rtx));
  return tmp;
}

void
ix86_expand_call (rtx retval, rtx fnaddr, rtx callarg1,
		  rtx callarg2 ATTRIBUTE_UNUSED,
		  rtx pop, int sibcall)
{
  rtx use = NULL, call;

  if (pop == const0_rtx)
    pop = NULL;
  gcc_assert (!TARGET_64BIT || !pop);

  if (TARGET_MACHO && !TARGET_64BIT)
    {
#if TARGET_MACHO
      if (flag_pic && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF)
	fnaddr = machopic_indirect_call_target (fnaddr);
#endif
    }
  else
    {
      /* Static functions and indirect calls don't need the pic register.  */
      if (flag_pic && (!TARGET_64BIT || ix86_cmodel == CM_LARGE_PIC)
	  && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
	  && ! SYMBOL_REF_LOCAL_P (XEXP (fnaddr, 0)))
	use_reg (&use, pic_offset_table_rtx);
    }

  if (TARGET_64BIT && INTVAL (callarg2) >= 0)
    {
      rtx al = gen_rtx_REG (QImode, AX_REG);
      emit_move_insn (al, callarg2);
      use_reg (&use, al);
    }

  if (ix86_cmodel == CM_LARGE_PIC
      && GET_CODE (fnaddr) == MEM
      && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
      && !local_symbolic_operand (XEXP (fnaddr, 0), VOIDmode))
    fnaddr = gen_rtx_MEM (QImode, construct_plt_address (XEXP (fnaddr, 0)));
  else if (! call_insn_operand (XEXP (fnaddr, 0), Pmode))
    {
      fnaddr = copy_to_mode_reg (Pmode, XEXP (fnaddr, 0));
      fnaddr = gen_rtx_MEM (QImode, fnaddr);
    }
  if (sibcall && TARGET_64BIT
      && !constant_call_address_operand (XEXP (fnaddr, 0), Pmode))
    {
      rtx addr;
      addr = copy_to_mode_reg (Pmode, XEXP (fnaddr, 0));
      fnaddr = gen_rtx_REG (Pmode, R11_REG);
      emit_move_insn (fnaddr, addr);
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
ix86_init_machine_status (void)
{
  struct machine_function *f;

  f = GGC_CNEW (struct machine_function);
  f->use_fast_prologue_epilogue_nregs = -1;
  f->tls_descriptor_call_expanded_p = 0;

  return f;
}

/* Return a MEM corresponding to a stack slot with mode MODE.
   Allocate a new slot if necessary.

   The RTL for a function can have several slots available: N is
   which slot to use.  */

rtx
assign_386_stack_local (enum machine_mode mode, enum ix86_stack_slot n)
{
  struct stack_local_entry *s;

  gcc_assert (n < MAX_386_STACK_LOCALS);

  /* Virtual slot is valid only before vregs are instantiated.  */
  gcc_assert ((n == SLOT_VIRTUAL) == !virtuals_instantiated);

  for (s = ix86_stack_locals; s; s = s->next)
    if (s->mode == mode && s->n == n)
      return copy_rtx (s->rtl);

  s = (struct stack_local_entry *)
    ggc_alloc (sizeof (struct stack_local_entry));
  s->n = n;
  s->mode = mode;
  s->rtl = assign_stack_local (mode, GET_MODE_SIZE (mode), 0);

  s->next = ix86_stack_locals;
  ix86_stack_locals = s;
  return s->rtl;
}

/* Construct the SYMBOL_REF for the tls_get_addr function.  */

static GTY(()) rtx ix86_tls_symbol;
rtx
ix86_tls_get_addr (void)
{

  if (!ix86_tls_symbol)
    {
      ix86_tls_symbol = gen_rtx_SYMBOL_REF (Pmode,
					    (TARGET_ANY_GNU_TLS
					     && !TARGET_64BIT)
					    ? "___tls_get_addr"
					    : "__tls_get_addr");
    }

  return ix86_tls_symbol;
}

/* Construct the SYMBOL_REF for the _TLS_MODULE_BASE_ symbol.  */

static GTY(()) rtx ix86_tls_module_base_symbol;
rtx
ix86_tls_module_base (void)
{

  if (!ix86_tls_module_base_symbol)
    {
      ix86_tls_module_base_symbol = gen_rtx_SYMBOL_REF (Pmode,
							"_TLS_MODULE_BASE_");
      SYMBOL_REF_FLAGS (ix86_tls_module_base_symbol)
	|= TLS_MODEL_GLOBAL_DYNAMIC << SYMBOL_FLAG_TLS_SHIFT;
    }

  return ix86_tls_module_base_symbol;
}

/* Calculate the length of the memory address in the instruction
   encoding.  Does not include the one-byte modrm, opcode, or prefix.  */

int
memory_address_length (rtx addr)
{
  struct ix86_address parts;
  rtx base, index, disp;
  int len;
  int ok;

  if (GET_CODE (addr) == PRE_DEC
      || GET_CODE (addr) == POST_INC
      || GET_CODE (addr) == PRE_MODIFY
      || GET_CODE (addr) == POST_MODIFY)
    return 0;

  ok = ix86_decompose_address (addr, &parts);
  gcc_assert (ok);

  if (parts.base && GET_CODE (parts.base) == SUBREG)
    parts.base = SUBREG_REG (parts.base);
  if (parts.index && GET_CODE (parts.index) == SUBREG)
    parts.index = SUBREG_REG (parts.index);

  base = parts.base;
  index = parts.index;
  disp = parts.disp;
  len = 0;

  /* Rule of thumb:
       - esp as the base always wants an index,
       - ebp as the base always wants a displacement.  */

  /* Register Indirect.  */
  if (base && !index && !disp)
    {
      /* esp (for its index) and ebp (for its displacement) need
	 the two-byte modrm form.  */
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
	  if (base && satisfies_constraint_K (disp))
	    len = 1;
	  else
	    len = 4;
	}
      /* ebp always wants a displacement.  */
      else if (base == hard_frame_pointer_rtx)
        len = 1;

      /* An index requires the two-byte modrm form....  */
      if (index
	  /* ...like esp, which always wants an index.  */
	  || base == stack_pointer_rtx
	  || base == arg_pointer_rtx
	  || base == frame_pointer_rtx)
	len += 1;
    }

  return len;
}

/* Compute default value for "length_immediate" attribute.  When SHORTFORM
   is set, expect that insn have 8bit immediate alternative.  */
int
ix86_attr_length_immediate_default (rtx insn, int shortform)
{
  int len = 0;
  int i;
  extract_insn_cached (insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (CONSTANT_P (recog_data.operand[i]))
      {
	gcc_assert (!len);
	if (shortform && satisfies_constraint_K (recog_data.operand[i]))
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
ix86_attr_length_address_default (rtx insn)
{
  int i;

  if (get_attr_type (insn) == TYPE_LEA)
    {
      rtx set = PATTERN (insn);

      if (GET_CODE (set) == PARALLEL)
	set = XVECEXP (set, 0, 0);

      gcc_assert (GET_CODE (set) == SET);

      return memory_address_length (SET_SRC (set));
    }

  extract_insn_cached (insn);
  for (i = recog_data.n_operands - 1; i >= 0; --i)
    if (MEM_P (recog_data.operand[i]))
      {
	return memory_address_length (XEXP (recog_data.operand[i], 0));
	break;
      }
  return 0;
}

/* Return the maximum number of instructions a cpu can issue.  */

static int
ix86_issue_rate (void)
{
  switch (ix86_tune)
    {
    case PROCESSOR_PENTIUM:
    case PROCESSOR_K6:
      return 2;

    case PROCESSOR_PENTIUMPRO:
    case PROCESSOR_PENTIUM4:
    case PROCESSOR_ATHLON:
    case PROCESSOR_K8:
    case PROCESSOR_AMDFAM10:
    case PROCESSOR_NOCONA:
    case PROCESSOR_GENERIC32:
    case PROCESSOR_GENERIC64:
      return 3;

    case PROCESSOR_CORE2:
      return 4;

    default:
      return 1;
    }
}

/* A subroutine of ix86_adjust_cost -- return true iff INSN reads flags set
   by DEP_INSN and nothing set by DEP_INSN.  */

static int
ix86_flags_dependent (rtx insn, rtx dep_insn, enum attr_type insn_type)
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

  if (!REG_P (set) || REGNO (set) != FLAGS_REG)
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
ix86_agi_dependent (rtx insn, rtx dep_insn, enum attr_type insn_type)
{
  rtx addr;

  if (insn_type == TYPE_LEA
      && TARGET_PENTIUM)
    {
      addr = PATTERN (insn);

      if (GET_CODE (addr) == PARALLEL)
	addr = XVECEXP (addr, 0, 0);

      gcc_assert (GET_CODE (addr) == SET);

      addr = SET_SRC (addr);
    }
  else
    {
      int i;
      extract_insn_cached (insn);
      for (i = recog_data.n_operands - 1; i >= 0; --i)
	if (MEM_P (recog_data.operand[i]))
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
ix86_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  enum attr_type insn_type, dep_insn_type;
  enum attr_memory memory;
  rtx set, set2;
  int dep_insn_code_number;

  /* Anti and output dependencies have zero cost on all CPUs.  */
  if (REG_NOTE_KIND (link) != 0)
    return 0;

  dep_insn_code_number = recog_memoized (dep_insn);

  /* If we can't recognize the insns, we can't really do anything.  */
  if (dep_insn_code_number < 0 || recog_memoized (insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_insn_type = get_attr_type (dep_insn);

  switch (ix86_tune)
    {
    case PROCESSOR_PENTIUM:
      /* Address Generation Interlock adds a cycle of latency.  */
      if (ix86_agi_dependent (insn, dep_insn, insn_type))
	cost += 1;

      /* ??? Compares pair with jump/setcc.  */
      if (ix86_flags_dependent (insn, dep_insn, insn_type))
	cost = 0;

      /* Floating point stores require value to be ready one cycle earlier.  */
      if (insn_type == TYPE_FMOV
	  && get_attr_memory (insn) == MEMORY_STORE
	  && !ix86_agi_dependent (insn, dep_insn, insn_type))
	cost += 1;
      break;

    case PROCESSOR_PENTIUMPRO:
      memory = get_attr_memory (insn);

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      /* There is one cycle extra latency between an FP op and a store.  */
      if (insn_type == TYPE_FMOV
	  && (set = single_set (dep_insn)) != NULL_RTX
	  && (set2 = single_set (insn)) != NULL_RTX
	  && rtx_equal_p (SET_DEST (set), SET_SRC (set2))
	  && MEM_P (SET_DEST (set2)))
	cost += 1;

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (insn, dep_insn, insn_type))
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

      /* The esp dependency is resolved before the instruction is really
         finished.  */
      if ((insn_type == TYPE_PUSH || insn_type == TYPE_POP)
	  && (dep_insn_type == TYPE_PUSH || dep_insn_type == TYPE_POP))
	return 1;

      /* INT->FP conversion is expensive.  */
      if (get_attr_fp_int_src (dep_insn))
	cost += 5;

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (insn, dep_insn, insn_type))
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
    case PROCESSOR_K8:
    case PROCESSOR_AMDFAM10:
    case PROCESSOR_GENERIC32:
    case PROCESSOR_GENERIC64:
      memory = get_attr_memory (insn);

      /* Show ability of reorder buffer to hide latency of load by executing
	 in parallel with previous instruction in case
	 previous instruction is not needed to compute the address.  */
      if ((memory == MEMORY_LOAD || memory == MEMORY_BOTH)
	  && !ix86_agi_dependent (insn, dep_insn, insn_type))
	{
	  enum attr_unit unit = get_attr_unit (insn);
	  int loadcost = 3;

	  /* Because of the difference between the length of integer and
	     floating unit pipeline preparation stages, the memory operands
	     for floating point are cheaper.

	     ??? For Athlon it the difference is most probably 2.  */
	  if (unit == UNIT_INTEGER || unit == UNIT_UNKNOWN)
	    loadcost = 3;
	  else
	    loadcost = TARGET_ATHLON ? 2 : 0;

	  if (cost >= loadcost)
	    cost -= loadcost;
	  else
	    cost = 0;
	}

    default:
      break;
    }

  return cost;
}

/* How many alternative schedules to try.  This should be as wide as the
   scheduling freedom in the DFA, but no wider.  Making this value too
   large results extra work for the scheduler.  */

static int
ia32_multipass_dfa_lookahead (void)
{
  switch (ix86_tune)
    {
    case PROCESSOR_PENTIUM:
      return 2;

    case PROCESSOR_PENTIUMPRO:
    case PROCESSOR_K6:
      return 1;

    default:
      return 0;
    }
}


/* Compute the alignment given to a constant that is being placed in memory.
   EXP is the constant and ALIGN is the alignment that the object would
   ordinarily have.
   The value of this function is used instead of that alignment to align
   the object.  */

int
ix86_constant_alignment (tree exp, int align)
{
  if (TREE_CODE (exp) == REAL_CST || TREE_CODE (exp) == VECTOR_CST
      || TREE_CODE (exp) == INTEGER_CST)
    {
      if (TYPE_MODE (TREE_TYPE (exp)) == DFmode && align < 64)
	return 64;
      else if (ALIGN_MODE_128 (TYPE_MODE (TREE_TYPE (exp))) && align < 128)
	return 128;
    }
  else if (!optimize_size && TREE_CODE (exp) == STRING_CST
	   && TREE_STRING_LENGTH (exp) >= 31 && align < BITS_PER_WORD)
    return BITS_PER_WORD;

  return align;
}

/* Compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this function is used
   instead of that alignment to align the object.  */

int
ix86_data_alignment (tree type, int align)
{
  int max_align = optimize_size ? BITS_PER_WORD : MIN (256, MAX_OFILE_ALIGNMENT);

  if (AGGREGATE_TYPE_P (type)
      && TYPE_SIZE (type)
      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
      && (TREE_INT_CST_LOW (TYPE_SIZE (type)) >= (unsigned) max_align
	  || TREE_INT_CST_HIGH (TYPE_SIZE (type)))
      && align < max_align)
    align = max_align;

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
ix86_local_alignment (tree type, int align)
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
x86_initialize_trampoline (rtx tramp, rtx fnaddr, rtx cxt)
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
      if (x86_64_zext_immediate_operand (fnaddr, VOIDmode))
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
      gcc_assert (offset <= TRAMPOLINE_SIZE);
    }

#ifdef ENABLE_EXECUTE_STACK
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
		     LCT_NORMAL, VOIDmode, 1, tramp, Pmode);
#endif
}

/* Codes for all the SSE/MMX builtins.  */
enum ix86_builtins
{
  IX86_BUILTIN_ADDPS,
  IX86_BUILTIN_ADDSS,
  IX86_BUILTIN_DIVPS,
  IX86_BUILTIN_DIVSS,
  IX86_BUILTIN_MULPS,
  IX86_BUILTIN_MULSS,
  IX86_BUILTIN_SUBPS,
  IX86_BUILTIN_SUBSS,

  IX86_BUILTIN_CMPEQPS,
  IX86_BUILTIN_CMPLTPS,
  IX86_BUILTIN_CMPLEPS,
  IX86_BUILTIN_CMPGTPS,
  IX86_BUILTIN_CMPGEPS,
  IX86_BUILTIN_CMPNEQPS,
  IX86_BUILTIN_CMPNLTPS,
  IX86_BUILTIN_CMPNLEPS,
  IX86_BUILTIN_CMPNGTPS,
  IX86_BUILTIN_CMPNGEPS,
  IX86_BUILTIN_CMPORDPS,
  IX86_BUILTIN_CMPUNORDPS,
  IX86_BUILTIN_CMPEQSS,
  IX86_BUILTIN_CMPLTSS,
  IX86_BUILTIN_CMPLESS,
  IX86_BUILTIN_CMPNEQSS,
  IX86_BUILTIN_CMPNLTSS,
  IX86_BUILTIN_CMPNLESS,
  IX86_BUILTIN_CMPNGTSS,
  IX86_BUILTIN_CMPNGESS,
  IX86_BUILTIN_CMPORDSS,
  IX86_BUILTIN_CMPUNORDSS,

  IX86_BUILTIN_COMIEQSS,
  IX86_BUILTIN_COMILTSS,
  IX86_BUILTIN_COMILESS,
  IX86_BUILTIN_COMIGTSS,
  IX86_BUILTIN_COMIGESS,
  IX86_BUILTIN_COMINEQSS,
  IX86_BUILTIN_UCOMIEQSS,
  IX86_BUILTIN_UCOMILTSS,
  IX86_BUILTIN_UCOMILESS,
  IX86_BUILTIN_UCOMIGTSS,
  IX86_BUILTIN_UCOMIGESS,
  IX86_BUILTIN_UCOMINEQSS,

  IX86_BUILTIN_CVTPI2PS,
  IX86_BUILTIN_CVTPS2PI,
  IX86_BUILTIN_CVTSI2SS,
  IX86_BUILTIN_CVTSI642SS,
  IX86_BUILTIN_CVTSS2SI,
  IX86_BUILTIN_CVTSS2SI64,
  IX86_BUILTIN_CVTTPS2PI,
  IX86_BUILTIN_CVTTSS2SI,
  IX86_BUILTIN_CVTTSS2SI64,

  IX86_BUILTIN_MAXPS,
  IX86_BUILTIN_MAXSS,
  IX86_BUILTIN_MINPS,
  IX86_BUILTIN_MINSS,

  IX86_BUILTIN_LOADUPS,
  IX86_BUILTIN_STOREUPS,
  IX86_BUILTIN_MOVSS,

  IX86_BUILTIN_MOVHLPS,
  IX86_BUILTIN_MOVLHPS,
  IX86_BUILTIN_LOADHPS,
  IX86_BUILTIN_LOADLPS,
  IX86_BUILTIN_STOREHPS,
  IX86_BUILTIN_STORELPS,

  IX86_BUILTIN_MASKMOVQ,
  IX86_BUILTIN_MOVMSKPS,
  IX86_BUILTIN_PMOVMSKB,

  IX86_BUILTIN_MOVNTPS,
  IX86_BUILTIN_MOVNTQ,

  IX86_BUILTIN_LOADDQU,
  IX86_BUILTIN_STOREDQU,

  IX86_BUILTIN_PACKSSWB,
  IX86_BUILTIN_PACKSSDW,
  IX86_BUILTIN_PACKUSWB,

  IX86_BUILTIN_PADDB,
  IX86_BUILTIN_PADDW,
  IX86_BUILTIN_PADDD,
  IX86_BUILTIN_PADDQ,
  IX86_BUILTIN_PADDSB,
  IX86_BUILTIN_PADDSW,
  IX86_BUILTIN_PADDUSB,
  IX86_BUILTIN_PADDUSW,
  IX86_BUILTIN_PSUBB,
  IX86_BUILTIN_PSUBW,
  IX86_BUILTIN_PSUBD,
  IX86_BUILTIN_PSUBQ,
  IX86_BUILTIN_PSUBSB,
  IX86_BUILTIN_PSUBSW,
  IX86_BUILTIN_PSUBUSB,
  IX86_BUILTIN_PSUBUSW,

  IX86_BUILTIN_PAND,
  IX86_BUILTIN_PANDN,
  IX86_BUILTIN_POR,
  IX86_BUILTIN_PXOR,

  IX86_BUILTIN_PAVGB,
  IX86_BUILTIN_PAVGW,

  IX86_BUILTIN_PCMPEQB,
  IX86_BUILTIN_PCMPEQW,
  IX86_BUILTIN_PCMPEQD,
  IX86_BUILTIN_PCMPGTB,
  IX86_BUILTIN_PCMPGTW,
  IX86_BUILTIN_PCMPGTD,

  IX86_BUILTIN_PMADDWD,

  IX86_BUILTIN_PMAXSW,
  IX86_BUILTIN_PMAXUB,
  IX86_BUILTIN_PMINSW,
  IX86_BUILTIN_PMINUB,

  IX86_BUILTIN_PMULHUW,
  IX86_BUILTIN_PMULHW,
  IX86_BUILTIN_PMULLW,

  IX86_BUILTIN_PSADBW,
  IX86_BUILTIN_PSHUFW,

  IX86_BUILTIN_PSLLW,
  IX86_BUILTIN_PSLLD,
  IX86_BUILTIN_PSLLQ,
  IX86_BUILTIN_PSRAW,
  IX86_BUILTIN_PSRAD,
  IX86_BUILTIN_PSRLW,
  IX86_BUILTIN_PSRLD,
  IX86_BUILTIN_PSRLQ,
  IX86_BUILTIN_PSLLWI,
  IX86_BUILTIN_PSLLDI,
  IX86_BUILTIN_PSLLQI,
  IX86_BUILTIN_PSRAWI,
  IX86_BUILTIN_PSRADI,
  IX86_BUILTIN_PSRLWI,
  IX86_BUILTIN_PSRLDI,
  IX86_BUILTIN_PSRLQI,

  IX86_BUILTIN_PUNPCKHBW,
  IX86_BUILTIN_PUNPCKHWD,
  IX86_BUILTIN_PUNPCKHDQ,
  IX86_BUILTIN_PUNPCKLBW,
  IX86_BUILTIN_PUNPCKLWD,
  IX86_BUILTIN_PUNPCKLDQ,

  IX86_BUILTIN_SHUFPS,

  IX86_BUILTIN_RCPPS,
  IX86_BUILTIN_RCPSS,
  IX86_BUILTIN_RSQRTPS,
  IX86_BUILTIN_RSQRTPS_NR,
  IX86_BUILTIN_RSQRTSS,
  IX86_BUILTIN_RSQRTF,
  IX86_BUILTIN_SQRTPS,
  IX86_BUILTIN_SQRTPS_NR,
  IX86_BUILTIN_SQRTSS,

  IX86_BUILTIN_UNPCKHPS,
  IX86_BUILTIN_UNPCKLPS,

  IX86_BUILTIN_ANDPS,
  IX86_BUILTIN_ANDNPS,
  IX86_BUILTIN_ORPS,
  IX86_BUILTIN_XORPS,

  IX86_BUILTIN_EMMS,
  IX86_BUILTIN_LDMXCSR,
  IX86_BUILTIN_STMXCSR,
  IX86_BUILTIN_SFENCE,

  /* 3DNow! Original */
  IX86_BUILTIN_FEMMS,
  IX86_BUILTIN_PAVGUSB,
  IX86_BUILTIN_PF2ID,
  IX86_BUILTIN_PFACC,
  IX86_BUILTIN_PFADD,
  IX86_BUILTIN_PFCMPEQ,
  IX86_BUILTIN_PFCMPGE,
  IX86_BUILTIN_PFCMPGT,
  IX86_BUILTIN_PFMAX,
  IX86_BUILTIN_PFMIN,
  IX86_BUILTIN_PFMUL,
  IX86_BUILTIN_PFRCP,
  IX86_BUILTIN_PFRCPIT1,
  IX86_BUILTIN_PFRCPIT2,
  IX86_BUILTIN_PFRSQIT1,
  IX86_BUILTIN_PFRSQRT,
  IX86_BUILTIN_PFSUB,
  IX86_BUILTIN_PFSUBR,
  IX86_BUILTIN_PI2FD,
  IX86_BUILTIN_PMULHRW,

  /* 3DNow! Athlon Extensions */
  IX86_BUILTIN_PF2IW,
  IX86_BUILTIN_PFNACC,
  IX86_BUILTIN_PFPNACC,
  IX86_BUILTIN_PI2FW,
  IX86_BUILTIN_PSWAPDSI,
  IX86_BUILTIN_PSWAPDSF,

  /* SSE2 */
  IX86_BUILTIN_ADDPD,
  IX86_BUILTIN_ADDSD,
  IX86_BUILTIN_DIVPD,
  IX86_BUILTIN_DIVSD,
  IX86_BUILTIN_MULPD,
  IX86_BUILTIN_MULSD,
  IX86_BUILTIN_SUBPD,
  IX86_BUILTIN_SUBSD,

  IX86_BUILTIN_CMPEQPD,
  IX86_BUILTIN_CMPLTPD,
  IX86_BUILTIN_CMPLEPD,
  IX86_BUILTIN_CMPGTPD,
  IX86_BUILTIN_CMPGEPD,
  IX86_BUILTIN_CMPNEQPD,
  IX86_BUILTIN_CMPNLTPD,
  IX86_BUILTIN_CMPNLEPD,
  IX86_BUILTIN_CMPNGTPD,
  IX86_BUILTIN_CMPNGEPD,
  IX86_BUILTIN_CMPORDPD,
  IX86_BUILTIN_CMPUNORDPD,
  IX86_BUILTIN_CMPEQSD,
  IX86_BUILTIN_CMPLTSD,
  IX86_BUILTIN_CMPLESD,
  IX86_BUILTIN_CMPNEQSD,
  IX86_BUILTIN_CMPNLTSD,
  IX86_BUILTIN_CMPNLESD,
  IX86_BUILTIN_CMPORDSD,
  IX86_BUILTIN_CMPUNORDSD,

  IX86_BUILTIN_COMIEQSD,
  IX86_BUILTIN_COMILTSD,
  IX86_BUILTIN_COMILESD,
  IX86_BUILTIN_COMIGTSD,
  IX86_BUILTIN_COMIGESD,
  IX86_BUILTIN_COMINEQSD,
  IX86_BUILTIN_UCOMIEQSD,
  IX86_BUILTIN_UCOMILTSD,
  IX86_BUILTIN_UCOMILESD,
  IX86_BUILTIN_UCOMIGTSD,
  IX86_BUILTIN_UCOMIGESD,
  IX86_BUILTIN_UCOMINEQSD,

  IX86_BUILTIN_MAXPD,
  IX86_BUILTIN_MAXSD,
  IX86_BUILTIN_MINPD,
  IX86_BUILTIN_MINSD,

  IX86_BUILTIN_ANDPD,
  IX86_BUILTIN_ANDNPD,
  IX86_BUILTIN_ORPD,
  IX86_BUILTIN_XORPD,

  IX86_BUILTIN_SQRTPD,
  IX86_BUILTIN_SQRTSD,

  IX86_BUILTIN_UNPCKHPD,
  IX86_BUILTIN_UNPCKLPD,

  IX86_BUILTIN_SHUFPD,

  IX86_BUILTIN_LOADUPD,
  IX86_BUILTIN_STOREUPD,
  IX86_BUILTIN_MOVSD,

  IX86_BUILTIN_LOADHPD,
  IX86_BUILTIN_LOADLPD,

  IX86_BUILTIN_CVTDQ2PD,
  IX86_BUILTIN_CVTDQ2PS,

  IX86_BUILTIN_CVTPD2DQ,
  IX86_BUILTIN_CVTPD2PI,
  IX86_BUILTIN_CVTPD2PS,
  IX86_BUILTIN_CVTTPD2DQ,
  IX86_BUILTIN_CVTTPD2PI,

  IX86_BUILTIN_CVTPI2PD,
  IX86_BUILTIN_CVTSI2SD,
  IX86_BUILTIN_CVTSI642SD,

  IX86_BUILTIN_CVTSD2SI,
  IX86_BUILTIN_CVTSD2SI64,
  IX86_BUILTIN_CVTSD2SS,
  IX86_BUILTIN_CVTSS2SD,
  IX86_BUILTIN_CVTTSD2SI,
  IX86_BUILTIN_CVTTSD2SI64,

  IX86_BUILTIN_CVTPS2DQ,
  IX86_BUILTIN_CVTPS2PD,
  IX86_BUILTIN_CVTTPS2DQ,

  IX86_BUILTIN_MOVNTI,
  IX86_BUILTIN_MOVNTPD,
  IX86_BUILTIN_MOVNTDQ,

  /* SSE2 MMX */
  IX86_BUILTIN_MASKMOVDQU,
  IX86_BUILTIN_MOVMSKPD,
  IX86_BUILTIN_PMOVMSKB128,

  IX86_BUILTIN_PACKSSWB128,
  IX86_BUILTIN_PACKSSDW128,
  IX86_BUILTIN_PACKUSWB128,

  IX86_BUILTIN_PADDB128,
  IX86_BUILTIN_PADDW128,
  IX86_BUILTIN_PADDD128,
  IX86_BUILTIN_PADDQ128,
  IX86_BUILTIN_PADDSB128,
  IX86_BUILTIN_PADDSW128,
  IX86_BUILTIN_PADDUSB128,
  IX86_BUILTIN_PADDUSW128,
  IX86_BUILTIN_PSUBB128,
  IX86_BUILTIN_PSUBW128,
  IX86_BUILTIN_PSUBD128,
  IX86_BUILTIN_PSUBQ128,
  IX86_BUILTIN_PSUBSB128,
  IX86_BUILTIN_PSUBSW128,
  IX86_BUILTIN_PSUBUSB128,
  IX86_BUILTIN_PSUBUSW128,

  IX86_BUILTIN_PAND128,
  IX86_BUILTIN_PANDN128,
  IX86_BUILTIN_POR128,
  IX86_BUILTIN_PXOR128,

  IX86_BUILTIN_PAVGB128,
  IX86_BUILTIN_PAVGW128,

  IX86_BUILTIN_PCMPEQB128,
  IX86_BUILTIN_PCMPEQW128,
  IX86_BUILTIN_PCMPEQD128,
  IX86_BUILTIN_PCMPGTB128,
  IX86_BUILTIN_PCMPGTW128,
  IX86_BUILTIN_PCMPGTD128,

  IX86_BUILTIN_PMADDWD128,

  IX86_BUILTIN_PMAXSW128,
  IX86_BUILTIN_PMAXUB128,
  IX86_BUILTIN_PMINSW128,
  IX86_BUILTIN_PMINUB128,

  IX86_BUILTIN_PMULUDQ,
  IX86_BUILTIN_PMULUDQ128,
  IX86_BUILTIN_PMULHUW128,
  IX86_BUILTIN_PMULHW128,
  IX86_BUILTIN_PMULLW128,

  IX86_BUILTIN_PSADBW128,
  IX86_BUILTIN_PSHUFHW,
  IX86_BUILTIN_PSHUFLW,
  IX86_BUILTIN_PSHUFD,

  IX86_BUILTIN_PSLLDQI128,
  IX86_BUILTIN_PSLLWI128,
  IX86_BUILTIN_PSLLDI128,
  IX86_BUILTIN_PSLLQI128,
  IX86_BUILTIN_PSRAWI128,
  IX86_BUILTIN_PSRADI128,
  IX86_BUILTIN_PSRLDQI128,
  IX86_BUILTIN_PSRLWI128,
  IX86_BUILTIN_PSRLDI128,
  IX86_BUILTIN_PSRLQI128,

  IX86_BUILTIN_PSLLDQ128,
  IX86_BUILTIN_PSLLW128,
  IX86_BUILTIN_PSLLD128,
  IX86_BUILTIN_PSLLQ128,
  IX86_BUILTIN_PSRAW128,
  IX86_BUILTIN_PSRAD128,
  IX86_BUILTIN_PSRLW128,
  IX86_BUILTIN_PSRLD128,
  IX86_BUILTIN_PSRLQ128,

  IX86_BUILTIN_PUNPCKHBW128,
  IX86_BUILTIN_PUNPCKHWD128,
  IX86_BUILTIN_PUNPCKHDQ128,
  IX86_BUILTIN_PUNPCKHQDQ128,
  IX86_BUILTIN_PUNPCKLBW128,
  IX86_BUILTIN_PUNPCKLWD128,
  IX86_BUILTIN_PUNPCKLDQ128,
  IX86_BUILTIN_PUNPCKLQDQ128,

  IX86_BUILTIN_CLFLUSH,
  IX86_BUILTIN_MFENCE,
  IX86_BUILTIN_LFENCE,

  /* Prescott New Instructions.  */
  IX86_BUILTIN_ADDSUBPS,
  IX86_BUILTIN_HADDPS,
  IX86_BUILTIN_HSUBPS,
  IX86_BUILTIN_MOVSHDUP,
  IX86_BUILTIN_MOVSLDUP,
  IX86_BUILTIN_ADDSUBPD,
  IX86_BUILTIN_HADDPD,
  IX86_BUILTIN_HSUBPD,
  IX86_BUILTIN_LDDQU,

  IX86_BUILTIN_MONITOR,
  IX86_BUILTIN_MWAIT,

  /* SSSE3.  */
  IX86_BUILTIN_PHADDW,
  IX86_BUILTIN_PHADDD,
  IX86_BUILTIN_PHADDSW,
  IX86_BUILTIN_PHSUBW,
  IX86_BUILTIN_PHSUBD,
  IX86_BUILTIN_PHSUBSW,
  IX86_BUILTIN_PMADDUBSW,
  IX86_BUILTIN_PMULHRSW,
  IX86_BUILTIN_PSHUFB,
  IX86_BUILTIN_PSIGNB,
  IX86_BUILTIN_PSIGNW,
  IX86_BUILTIN_PSIGND,
  IX86_BUILTIN_PALIGNR,
  IX86_BUILTIN_PABSB,
  IX86_BUILTIN_PABSW,
  IX86_BUILTIN_PABSD,

  IX86_BUILTIN_PHADDW128,
  IX86_BUILTIN_PHADDD128,
  IX86_BUILTIN_PHADDSW128,
  IX86_BUILTIN_PHSUBW128,
  IX86_BUILTIN_PHSUBD128,
  IX86_BUILTIN_PHSUBSW128,
  IX86_BUILTIN_PMADDUBSW128,
  IX86_BUILTIN_PMULHRSW128,
  IX86_BUILTIN_PSHUFB128,
  IX86_BUILTIN_PSIGNB128,
  IX86_BUILTIN_PSIGNW128,
  IX86_BUILTIN_PSIGND128,
  IX86_BUILTIN_PALIGNR128,
  IX86_BUILTIN_PABSB128,
  IX86_BUILTIN_PABSW128,
  IX86_BUILTIN_PABSD128,

  /* AMDFAM10 - SSE4A New Instructions.  */
  IX86_BUILTIN_MOVNTSD,
  IX86_BUILTIN_MOVNTSS,
  IX86_BUILTIN_EXTRQI,
  IX86_BUILTIN_EXTRQ,
  IX86_BUILTIN_INSERTQI,
  IX86_BUILTIN_INSERTQ,

  /* SSE4.1.  */
  IX86_BUILTIN_BLENDPD,
  IX86_BUILTIN_BLENDPS,
  IX86_BUILTIN_BLENDVPD,
  IX86_BUILTIN_BLENDVPS,
  IX86_BUILTIN_PBLENDVB128,
  IX86_BUILTIN_PBLENDW128,

  IX86_BUILTIN_DPPD,
  IX86_BUILTIN_DPPS,

  IX86_BUILTIN_INSERTPS128,

  IX86_BUILTIN_MOVNTDQA,
  IX86_BUILTIN_MPSADBW128,
  IX86_BUILTIN_PACKUSDW128,
  IX86_BUILTIN_PCMPEQQ,
  IX86_BUILTIN_PHMINPOSUW128,

  IX86_BUILTIN_PMAXSB128,
  IX86_BUILTIN_PMAXSD128,
  IX86_BUILTIN_PMAXUD128,
  IX86_BUILTIN_PMAXUW128,

  IX86_BUILTIN_PMINSB128,
  IX86_BUILTIN_PMINSD128,
  IX86_BUILTIN_PMINUD128,
  IX86_BUILTIN_PMINUW128,

  IX86_BUILTIN_PMOVSXBW128,
  IX86_BUILTIN_PMOVSXBD128,
  IX86_BUILTIN_PMOVSXBQ128,
  IX86_BUILTIN_PMOVSXWD128,
  IX86_BUILTIN_PMOVSXWQ128,
  IX86_BUILTIN_PMOVSXDQ128,

  IX86_BUILTIN_PMOVZXBW128,
  IX86_BUILTIN_PMOVZXBD128,
  IX86_BUILTIN_PMOVZXBQ128,
  IX86_BUILTIN_PMOVZXWD128,
  IX86_BUILTIN_PMOVZXWQ128,
  IX86_BUILTIN_PMOVZXDQ128,

  IX86_BUILTIN_PMULDQ128,
  IX86_BUILTIN_PMULLD128,

  IX86_BUILTIN_ROUNDPD,
  IX86_BUILTIN_ROUNDPS,
  IX86_BUILTIN_ROUNDSD,
  IX86_BUILTIN_ROUNDSS,

  IX86_BUILTIN_PTESTZ,
  IX86_BUILTIN_PTESTC,
  IX86_BUILTIN_PTESTNZC,

  IX86_BUILTIN_VEC_INIT_V2SI,
  IX86_BUILTIN_VEC_INIT_V4HI,
  IX86_BUILTIN_VEC_INIT_V8QI,
  IX86_BUILTIN_VEC_EXT_V2DF,
  IX86_BUILTIN_VEC_EXT_V2DI,
  IX86_BUILTIN_VEC_EXT_V4SF,
  IX86_BUILTIN_VEC_EXT_V4SI,
  IX86_BUILTIN_VEC_EXT_V8HI,
  IX86_BUILTIN_VEC_EXT_V2SI,
  IX86_BUILTIN_VEC_EXT_V4HI,
  IX86_BUILTIN_VEC_EXT_V16QI,
  IX86_BUILTIN_VEC_SET_V2DI,
  IX86_BUILTIN_VEC_SET_V4SF,
  IX86_BUILTIN_VEC_SET_V4SI,
  IX86_BUILTIN_VEC_SET_V8HI,
  IX86_BUILTIN_VEC_SET_V4HI,
  IX86_BUILTIN_VEC_SET_V16QI,

  IX86_BUILTIN_VEC_PACK_SFIX,

  /* SSE4.2.  */
  IX86_BUILTIN_CRC32QI,
  IX86_BUILTIN_CRC32HI,
  IX86_BUILTIN_CRC32SI,
  IX86_BUILTIN_CRC32DI,

  IX86_BUILTIN_PCMPESTRI128,
  IX86_BUILTIN_PCMPESTRM128,
  IX86_BUILTIN_PCMPESTRA128,
  IX86_BUILTIN_PCMPESTRC128,
  IX86_BUILTIN_PCMPESTRO128,
  IX86_BUILTIN_PCMPESTRS128,
  IX86_BUILTIN_PCMPESTRZ128,
  IX86_BUILTIN_PCMPISTRI128,
  IX86_BUILTIN_PCMPISTRM128,
  IX86_BUILTIN_PCMPISTRA128,
  IX86_BUILTIN_PCMPISTRC128,
  IX86_BUILTIN_PCMPISTRO128,
  IX86_BUILTIN_PCMPISTRS128,
  IX86_BUILTIN_PCMPISTRZ128,

  IX86_BUILTIN_PCMPGTQ,

  /* TFmode support builtins.  */
  IX86_BUILTIN_INFQ,
  IX86_BUILTIN_FABSQ,
  IX86_BUILTIN_COPYSIGNQ,

  /* SSE5 instructions */
  IX86_BUILTIN_FMADDSS,
  IX86_BUILTIN_FMADDSD,
  IX86_BUILTIN_FMADDPS,
  IX86_BUILTIN_FMADDPD,
  IX86_BUILTIN_FMSUBSS,
  IX86_BUILTIN_FMSUBSD,
  IX86_BUILTIN_FMSUBPS,
  IX86_BUILTIN_FMSUBPD,
  IX86_BUILTIN_FNMADDSS,
  IX86_BUILTIN_FNMADDSD,
  IX86_BUILTIN_FNMADDPS,
  IX86_BUILTIN_FNMADDPD,
  IX86_BUILTIN_FNMSUBSS,
  IX86_BUILTIN_FNMSUBSD,
  IX86_BUILTIN_FNMSUBPS,
  IX86_BUILTIN_FNMSUBPD,
  IX86_BUILTIN_PCMOV_V2DI,
  IX86_BUILTIN_PCMOV_V4SI,
  IX86_BUILTIN_PCMOV_V8HI,
  IX86_BUILTIN_PCMOV_V16QI,
  IX86_BUILTIN_PCMOV_V4SF,
  IX86_BUILTIN_PCMOV_V2DF,
  IX86_BUILTIN_PPERM,
  IX86_BUILTIN_PERMPS,
  IX86_BUILTIN_PERMPD,
  IX86_BUILTIN_PMACSSWW,
  IX86_BUILTIN_PMACSWW,
  IX86_BUILTIN_PMACSSWD,
  IX86_BUILTIN_PMACSWD,
  IX86_BUILTIN_PMACSSDD,
  IX86_BUILTIN_PMACSDD,
  IX86_BUILTIN_PMACSSDQL,
  IX86_BUILTIN_PMACSSDQH,
  IX86_BUILTIN_PMACSDQL,
  IX86_BUILTIN_PMACSDQH,
  IX86_BUILTIN_PMADCSSWD,
  IX86_BUILTIN_PMADCSWD,
  IX86_BUILTIN_PHADDBW,
  IX86_BUILTIN_PHADDBD,
  IX86_BUILTIN_PHADDBQ,
  IX86_BUILTIN_PHADDWD,
  IX86_BUILTIN_PHADDWQ,
  IX86_BUILTIN_PHADDDQ,
  IX86_BUILTIN_PHADDUBW,
  IX86_BUILTIN_PHADDUBD,
  IX86_BUILTIN_PHADDUBQ,
  IX86_BUILTIN_PHADDUWD,
  IX86_BUILTIN_PHADDUWQ,
  IX86_BUILTIN_PHADDUDQ,
  IX86_BUILTIN_PHSUBBW,
  IX86_BUILTIN_PHSUBWD,
  IX86_BUILTIN_PHSUBDQ,
  IX86_BUILTIN_PROTB,
  IX86_BUILTIN_PROTW,
  IX86_BUILTIN_PROTD,
  IX86_BUILTIN_PROTQ,
  IX86_BUILTIN_PROTB_IMM,
  IX86_BUILTIN_PROTW_IMM,
  IX86_BUILTIN_PROTD_IMM,
  IX86_BUILTIN_PROTQ_IMM,
  IX86_BUILTIN_PSHLB,
  IX86_BUILTIN_PSHLW,
  IX86_BUILTIN_PSHLD,
  IX86_BUILTIN_PSHLQ,
  IX86_BUILTIN_PSHAB,
  IX86_BUILTIN_PSHAW,
  IX86_BUILTIN_PSHAD,
  IX86_BUILTIN_PSHAQ,
  IX86_BUILTIN_FRCZSS,
  IX86_BUILTIN_FRCZSD,
  IX86_BUILTIN_FRCZPS,
  IX86_BUILTIN_FRCZPD,
  IX86_BUILTIN_CVTPH2PS,
  IX86_BUILTIN_CVTPS2PH,

  IX86_BUILTIN_COMEQSS,
  IX86_BUILTIN_COMNESS,
  IX86_BUILTIN_COMLTSS,
  IX86_BUILTIN_COMLESS,
  IX86_BUILTIN_COMGTSS,
  IX86_BUILTIN_COMGESS,
  IX86_BUILTIN_COMUEQSS,
  IX86_BUILTIN_COMUNESS,
  IX86_BUILTIN_COMULTSS,
  IX86_BUILTIN_COMULESS,
  IX86_BUILTIN_COMUGTSS,
  IX86_BUILTIN_COMUGESS,
  IX86_BUILTIN_COMORDSS,
  IX86_BUILTIN_COMUNORDSS,
  IX86_BUILTIN_COMFALSESS,
  IX86_BUILTIN_COMTRUESS,

  IX86_BUILTIN_COMEQSD,
  IX86_BUILTIN_COMNESD,
  IX86_BUILTIN_COMLTSD,
  IX86_BUILTIN_COMLESD,
  IX86_BUILTIN_COMGTSD,
  IX86_BUILTIN_COMGESD,
  IX86_BUILTIN_COMUEQSD,
  IX86_BUILTIN_COMUNESD,
  IX86_BUILTIN_COMULTSD,
  IX86_BUILTIN_COMULESD,
  IX86_BUILTIN_COMUGTSD,
  IX86_BUILTIN_COMUGESD,
  IX86_BUILTIN_COMORDSD,
  IX86_BUILTIN_COMUNORDSD,
  IX86_BUILTIN_COMFALSESD,
  IX86_BUILTIN_COMTRUESD,

  IX86_BUILTIN_COMEQPS,
  IX86_BUILTIN_COMNEPS,
  IX86_BUILTIN_COMLTPS,
  IX86_BUILTIN_COMLEPS,
  IX86_BUILTIN_COMGTPS,
  IX86_BUILTIN_COMGEPS,
  IX86_BUILTIN_COMUEQPS,
  IX86_BUILTIN_COMUNEPS,
  IX86_BUILTIN_COMULTPS,
  IX86_BUILTIN_COMULEPS,
  IX86_BUILTIN_COMUGTPS,
  IX86_BUILTIN_COMUGEPS,
  IX86_BUILTIN_COMORDPS,
  IX86_BUILTIN_COMUNORDPS,
  IX86_BUILTIN_COMFALSEPS,
  IX86_BUILTIN_COMTRUEPS,

  IX86_BUILTIN_COMEQPD,
  IX86_BUILTIN_COMNEPD,
  IX86_BUILTIN_COMLTPD,
  IX86_BUILTIN_COMLEPD,
  IX86_BUILTIN_COMGTPD,
  IX86_BUILTIN_COMGEPD,
  IX86_BUILTIN_COMUEQPD,
  IX86_BUILTIN_COMUNEPD,
  IX86_BUILTIN_COMULTPD,
  IX86_BUILTIN_COMULEPD,
  IX86_BUILTIN_COMUGTPD,
  IX86_BUILTIN_COMUGEPD,
  IX86_BUILTIN_COMORDPD,
  IX86_BUILTIN_COMUNORDPD,
  IX86_BUILTIN_COMFALSEPD,
  IX86_BUILTIN_COMTRUEPD,

  IX86_BUILTIN_PCOMEQUB,
  IX86_BUILTIN_PCOMNEUB,
  IX86_BUILTIN_PCOMLTUB,
  IX86_BUILTIN_PCOMLEUB,
  IX86_BUILTIN_PCOMGTUB,
  IX86_BUILTIN_PCOMGEUB,
  IX86_BUILTIN_PCOMFALSEUB,
  IX86_BUILTIN_PCOMTRUEUB,
  IX86_BUILTIN_PCOMEQUW,
  IX86_BUILTIN_PCOMNEUW,
  IX86_BUILTIN_PCOMLTUW,
  IX86_BUILTIN_PCOMLEUW,
  IX86_BUILTIN_PCOMGTUW,
  IX86_BUILTIN_PCOMGEUW,
  IX86_BUILTIN_PCOMFALSEUW,
  IX86_BUILTIN_PCOMTRUEUW,
  IX86_BUILTIN_PCOMEQUD,
  IX86_BUILTIN_PCOMNEUD,
  IX86_BUILTIN_PCOMLTUD,
  IX86_BUILTIN_PCOMLEUD,
  IX86_BUILTIN_PCOMGTUD,
  IX86_BUILTIN_PCOMGEUD,
  IX86_BUILTIN_PCOMFALSEUD,
  IX86_BUILTIN_PCOMTRUEUD,
  IX86_BUILTIN_PCOMEQUQ,
  IX86_BUILTIN_PCOMNEUQ,
  IX86_BUILTIN_PCOMLTUQ,
  IX86_BUILTIN_PCOMLEUQ,
  IX86_BUILTIN_PCOMGTUQ,
  IX86_BUILTIN_PCOMGEUQ,
  IX86_BUILTIN_PCOMFALSEUQ,
  IX86_BUILTIN_PCOMTRUEUQ,

  IX86_BUILTIN_PCOMEQB,
  IX86_BUILTIN_PCOMNEB,
  IX86_BUILTIN_PCOMLTB,
  IX86_BUILTIN_PCOMLEB,
  IX86_BUILTIN_PCOMGTB,
  IX86_BUILTIN_PCOMGEB,
  IX86_BUILTIN_PCOMFALSEB,
  IX86_BUILTIN_PCOMTRUEB,
  IX86_BUILTIN_PCOMEQW,
  IX86_BUILTIN_PCOMNEW,
  IX86_BUILTIN_PCOMLTW,
  IX86_BUILTIN_PCOMLEW,
  IX86_BUILTIN_PCOMGTW,
  IX86_BUILTIN_PCOMGEW,
  IX86_BUILTIN_PCOMFALSEW,
  IX86_BUILTIN_PCOMTRUEW,
  IX86_BUILTIN_PCOMEQD,
  IX86_BUILTIN_PCOMNED,
  IX86_BUILTIN_PCOMLTD,
  IX86_BUILTIN_PCOMLED,
  IX86_BUILTIN_PCOMGTD,
  IX86_BUILTIN_PCOMGED,
  IX86_BUILTIN_PCOMFALSED,
  IX86_BUILTIN_PCOMTRUED,
  IX86_BUILTIN_PCOMEQQ,
  IX86_BUILTIN_PCOMNEQ,
  IX86_BUILTIN_PCOMLTQ,
  IX86_BUILTIN_PCOMLEQ,
  IX86_BUILTIN_PCOMGTQ,
  IX86_BUILTIN_PCOMGEQ,
  IX86_BUILTIN_PCOMFALSEQ,
  IX86_BUILTIN_PCOMTRUEQ,

  IX86_BUILTIN_MAX
};

/* Table for the ix86 builtin decls.  */
static GTY(()) tree ix86_builtins[(int) IX86_BUILTIN_MAX];

/* Add an ix86 target builtin function with CODE, NAME and TYPE.  Do so,
 * if the target_flags include one of MASK.  Stores the function decl
 * in the ix86_builtins array.
 * Returns the function decl or NULL_TREE, if the builtin was not added.  */

static inline tree
def_builtin (int mask, const char *name, tree type, enum ix86_builtins code)
{
  tree decl = NULL_TREE;

  if (mask & ix86_isa_flags
      && (!(mask & OPTION_MASK_ISA_64BIT) || TARGET_64BIT))
    {
      decl = add_builtin_function (name, type, code, BUILT_IN_MD,
				   NULL, NULL_TREE);
      ix86_builtins[(int) code] = decl;
    }

  return decl;
}

/* Like def_builtin, but also marks the function decl "const".  */

static inline tree
def_builtin_const (int mask, const char *name, tree type,
		   enum ix86_builtins code)
{
  tree decl = def_builtin (mask, name, type, code);
  if (decl)
    TREE_READONLY (decl) = 1;
  return decl;
}

/* Bits for builtin_description.flag.  */

/* Set when we don't support the comparison natively, and should
   swap_comparison in order to support it.  */
#define BUILTIN_DESC_SWAP_OPERANDS	1

struct builtin_description
{
  const unsigned int mask;
  const enum insn_code icode;
  const char *const name;
  const enum ix86_builtins code;
  const enum rtx_code comparison;
  const int flag;
};

static const struct builtin_description bdesc_comi[] =
{
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_comi, "__builtin_ia32_comieq", IX86_BUILTIN_COMIEQSS, UNEQ, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_comi, "__builtin_ia32_comilt", IX86_BUILTIN_COMILTSS, UNLT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_comi, "__builtin_ia32_comile", IX86_BUILTIN_COMILESS, UNLE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_comi, "__builtin_ia32_comigt", IX86_BUILTIN_COMIGTSS, GT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_comi, "__builtin_ia32_comige", IX86_BUILTIN_COMIGESS, GE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_comi, "__builtin_ia32_comineq", IX86_BUILTIN_COMINEQSS, LTGT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomieq", IX86_BUILTIN_UCOMIEQSS, UNEQ, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomilt", IX86_BUILTIN_UCOMILTSS, UNLT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomile", IX86_BUILTIN_UCOMILESS, UNLE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomigt", IX86_BUILTIN_UCOMIGTSS, GT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomige", IX86_BUILTIN_UCOMIGESS, GE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_ucomi, "__builtin_ia32_ucomineq", IX86_BUILTIN_UCOMINEQSS, LTGT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdeq", IX86_BUILTIN_COMIEQSD, UNEQ, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdlt", IX86_BUILTIN_COMILTSD, UNLT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdle", IX86_BUILTIN_COMILESD, UNLE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdgt", IX86_BUILTIN_COMIGTSD, GT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdge", IX86_BUILTIN_COMIGESD, GE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_comi, "__builtin_ia32_comisdneq", IX86_BUILTIN_COMINEQSD, LTGT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdeq", IX86_BUILTIN_UCOMIEQSD, UNEQ, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdlt", IX86_BUILTIN_UCOMILTSD, UNLT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdle", IX86_BUILTIN_UCOMILESD, UNLE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdgt", IX86_BUILTIN_UCOMIGTSD, GT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdge", IX86_BUILTIN_UCOMIGESD, GE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_ucomi, "__builtin_ia32_ucomisdneq", IX86_BUILTIN_UCOMINEQSD, LTGT, 0 },
};

static const struct builtin_description bdesc_ptest[] =
{
  /* SSE4.1 */
  { OPTION_MASK_ISA_ROUND, CODE_FOR_sse4_1_ptest, "__builtin_ia32_ptestz128", IX86_BUILTIN_PTESTZ, EQ, 0 },
  { OPTION_MASK_ISA_ROUND, CODE_FOR_sse4_1_ptest, "__builtin_ia32_ptestc128", IX86_BUILTIN_PTESTC, LTU, 0 },
  { OPTION_MASK_ISA_ROUND, CODE_FOR_sse4_1_ptest, "__builtin_ia32_ptestnzc128", IX86_BUILTIN_PTESTNZC, GTU, 0 },
};

static const struct builtin_description bdesc_pcmpestr[] =
{
  /* SSE4.2 */
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestri128", IX86_BUILTIN_PCMPESTRI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestrm128", IX86_BUILTIN_PCMPESTRM128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestria128", IX86_BUILTIN_PCMPESTRA128, UNKNOWN, (int) CCAmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestric128", IX86_BUILTIN_PCMPESTRC128, UNKNOWN, (int) CCCmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestrio128", IX86_BUILTIN_PCMPESTRO128, UNKNOWN, (int) CCOmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestris128", IX86_BUILTIN_PCMPESTRS128, UNKNOWN, (int) CCSmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpestr, "__builtin_ia32_pcmpestriz128", IX86_BUILTIN_PCMPESTRZ128, UNKNOWN, (int) CCZmode },
};

static const struct builtin_description bdesc_pcmpistr[] =
{
  /* SSE4.2 */
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistri128", IX86_BUILTIN_PCMPISTRI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistrm128", IX86_BUILTIN_PCMPISTRM128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistria128", IX86_BUILTIN_PCMPISTRA128, UNKNOWN, (int) CCAmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistric128", IX86_BUILTIN_PCMPISTRC128, UNKNOWN, (int) CCCmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistrio128", IX86_BUILTIN_PCMPISTRO128, UNKNOWN, (int) CCOmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistris128", IX86_BUILTIN_PCMPISTRS128, UNKNOWN, (int) CCSmode },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_pcmpistr, "__builtin_ia32_pcmpistriz128", IX86_BUILTIN_PCMPISTRZ128, UNKNOWN, (int) CCZmode },
};

static const struct builtin_description bdesc_crc32[] =
{
  /* SSE4.2 */
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_crc32qi, 0, IX86_BUILTIN_CRC32QI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_crc32hi, 0, IX86_BUILTIN_CRC32HI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_crc32si, 0, IX86_BUILTIN_CRC32SI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_2 | OPTION_MASK_ISA_64BIT, CODE_FOR_sse4_2_crc32di, 0, IX86_BUILTIN_CRC32DI, UNKNOWN, 0 },
};

/* SSE builtins with 3 arguments and the last argument must be an immediate or xmm0.  */
static const struct builtin_description bdesc_sse_3arg[] =
{
  /* SSE4.1 */
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_blendpd, "__builtin_ia32_blendpd", IX86_BUILTIN_BLENDPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_blendps, "__builtin_ia32_blendps", IX86_BUILTIN_BLENDPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_blendvpd, "__builtin_ia32_blendvpd", IX86_BUILTIN_BLENDVPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_blendvps, "__builtin_ia32_blendvps", IX86_BUILTIN_BLENDVPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_dppd, "__builtin_ia32_dppd", IX86_BUILTIN_DPPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_dpps, "__builtin_ia32_dpps", IX86_BUILTIN_DPPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_insertps, "__builtin_ia32_insertps128", IX86_BUILTIN_INSERTPS128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_mpsadbw, "__builtin_ia32_mpsadbw128", IX86_BUILTIN_MPSADBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_pblendvb, "__builtin_ia32_pblendvb128", IX86_BUILTIN_PBLENDVB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_pblendw, "__builtin_ia32_pblendw128", IX86_BUILTIN_PBLENDW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_ROUND, CODE_FOR_sse4_1_roundsd, 0, IX86_BUILTIN_ROUNDSD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_ROUND, CODE_FOR_sse4_1_roundss, 0, IX86_BUILTIN_ROUNDSS, UNKNOWN, 0 },
};

static const struct builtin_description bdesc_2arg[] =
{
  /* SSE */
  { OPTION_MASK_ISA_SSE, CODE_FOR_addv4sf3, "__builtin_ia32_addps", IX86_BUILTIN_ADDPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_subv4sf3, "__builtin_ia32_subps", IX86_BUILTIN_SUBPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_mulv4sf3, "__builtin_ia32_mulps", IX86_BUILTIN_MULPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_divv4sf3, "__builtin_ia32_divps", IX86_BUILTIN_DIVPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmaddv4sf3,  "__builtin_ia32_addss", IX86_BUILTIN_ADDSS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmsubv4sf3,  "__builtin_ia32_subss", IX86_BUILTIN_SUBSS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmulv4sf3,  "__builtin_ia32_mulss", IX86_BUILTIN_MULSS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmdivv4sf3,  "__builtin_ia32_divss", IX86_BUILTIN_DIVSS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpeqps", IX86_BUILTIN_CMPEQPS, EQ, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpltps", IX86_BUILTIN_CMPLTPS, LT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpleps", IX86_BUILTIN_CMPLEPS, LE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpgtps", IX86_BUILTIN_CMPGTPS, LT, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpgeps", IX86_BUILTIN_CMPGEPS, LE, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpunordps", IX86_BUILTIN_CMPUNORDPS, UNORDERED, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpneqps", IX86_BUILTIN_CMPNEQPS, NE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpnltps", IX86_BUILTIN_CMPNLTPS, UNGE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpnleps", IX86_BUILTIN_CMPNLEPS, UNGT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpngtps", IX86_BUILTIN_CMPNGTPS, UNGE, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpngeps", IX86_BUILTIN_CMPNGEPS, UNGT, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_maskcmpv4sf3, "__builtin_ia32_cmpordps", IX86_BUILTIN_CMPORDPS, ORDERED, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpeqss", IX86_BUILTIN_CMPEQSS, EQ, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpltss", IX86_BUILTIN_CMPLTSS, LT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpless", IX86_BUILTIN_CMPLESS, LE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpunordss", IX86_BUILTIN_CMPUNORDSS, UNORDERED, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpneqss", IX86_BUILTIN_CMPNEQSS, NE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpnltss", IX86_BUILTIN_CMPNLTSS, UNGE, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpnless", IX86_BUILTIN_CMPNLESS, UNGT, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpngtss", IX86_BUILTIN_CMPNGTSS, UNGE, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpngess", IX86_BUILTIN_CMPNGESS, UNGT, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmmaskcmpv4sf3, "__builtin_ia32_cmpordss", IX86_BUILTIN_CMPORDSS, ORDERED, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_sminv4sf3, "__builtin_ia32_minps", IX86_BUILTIN_MINPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_smaxv4sf3, "__builtin_ia32_maxps", IX86_BUILTIN_MAXPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmsminv4sf3, "__builtin_ia32_minss", IX86_BUILTIN_MINSS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_vmsmaxv4sf3, "__builtin_ia32_maxss", IX86_BUILTIN_MAXSS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_andv4sf3, "__builtin_ia32_andps", IX86_BUILTIN_ANDPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_nandv4sf3,  "__builtin_ia32_andnps", IX86_BUILTIN_ANDNPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_iorv4sf3, "__builtin_ia32_orps", IX86_BUILTIN_ORPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_xorv4sf3,  "__builtin_ia32_xorps", IX86_BUILTIN_XORPS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_movss,  "__builtin_ia32_movss", IX86_BUILTIN_MOVSS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_movhlps,  "__builtin_ia32_movhlps", IX86_BUILTIN_MOVHLPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_movlhps,  "__builtin_ia32_movlhps", IX86_BUILTIN_MOVLHPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_unpckhps, "__builtin_ia32_unpckhps", IX86_BUILTIN_UNPCKHPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_unpcklps, "__builtin_ia32_unpcklps", IX86_BUILTIN_UNPCKLPS, UNKNOWN, 0 },

  /* MMX */
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_addv8qi3, "__builtin_ia32_paddb", IX86_BUILTIN_PADDB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_addv4hi3, "__builtin_ia32_paddw", IX86_BUILTIN_PADDW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_addv2si3, "__builtin_ia32_paddd", IX86_BUILTIN_PADDD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_mmx_adddi3, "__builtin_ia32_paddq", IX86_BUILTIN_PADDQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_subv8qi3, "__builtin_ia32_psubb", IX86_BUILTIN_PSUBB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_subv4hi3, "__builtin_ia32_psubw", IX86_BUILTIN_PSUBW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_subv2si3, "__builtin_ia32_psubd", IX86_BUILTIN_PSUBD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_mmx_subdi3, "__builtin_ia32_psubq", IX86_BUILTIN_PSUBQ, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ssaddv8qi3, "__builtin_ia32_paddsb", IX86_BUILTIN_PADDSB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ssaddv4hi3, "__builtin_ia32_paddsw", IX86_BUILTIN_PADDSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_sssubv8qi3, "__builtin_ia32_psubsb", IX86_BUILTIN_PSUBSB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_sssubv4hi3, "__builtin_ia32_psubsw", IX86_BUILTIN_PSUBSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_usaddv8qi3, "__builtin_ia32_paddusb", IX86_BUILTIN_PADDUSB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_usaddv4hi3, "__builtin_ia32_paddusw", IX86_BUILTIN_PADDUSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ussubv8qi3, "__builtin_ia32_psubusb", IX86_BUILTIN_PSUBUSB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ussubv4hi3, "__builtin_ia32_psubusw", IX86_BUILTIN_PSUBUSW, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_mulv4hi3, "__builtin_ia32_pmullw", IX86_BUILTIN_PMULLW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_smulv4hi3_highpart, "__builtin_ia32_pmulhw", IX86_BUILTIN_PMULHW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_umulv4hi3_highpart, "__builtin_ia32_pmulhuw", IX86_BUILTIN_PMULHUW, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_andv2si3, "__builtin_ia32_pand", IX86_BUILTIN_PAND, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_nandv2si3, "__builtin_ia32_pandn", IX86_BUILTIN_PANDN, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_iorv2si3, "__builtin_ia32_por", IX86_BUILTIN_POR, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_xorv2si3, "__builtin_ia32_pxor", IX86_BUILTIN_PXOR, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_uavgv8qi3, "__builtin_ia32_pavgb", IX86_BUILTIN_PAVGB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_uavgv4hi3, "__builtin_ia32_pavgw", IX86_BUILTIN_PAVGW, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_eqv8qi3, "__builtin_ia32_pcmpeqb", IX86_BUILTIN_PCMPEQB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_eqv4hi3, "__builtin_ia32_pcmpeqw", IX86_BUILTIN_PCMPEQW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_eqv2si3, "__builtin_ia32_pcmpeqd", IX86_BUILTIN_PCMPEQD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_gtv8qi3, "__builtin_ia32_pcmpgtb", IX86_BUILTIN_PCMPGTB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_gtv4hi3, "__builtin_ia32_pcmpgtw", IX86_BUILTIN_PCMPGTW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_gtv2si3, "__builtin_ia32_pcmpgtd", IX86_BUILTIN_PCMPGTD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_umaxv8qi3, "__builtin_ia32_pmaxub", IX86_BUILTIN_PMAXUB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_smaxv4hi3, "__builtin_ia32_pmaxsw", IX86_BUILTIN_PMAXSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_uminv8qi3, "__builtin_ia32_pminub", IX86_BUILTIN_PMINUB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_sminv4hi3, "__builtin_ia32_pminsw", IX86_BUILTIN_PMINSW, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_punpckhbw, "__builtin_ia32_punpckhbw", IX86_BUILTIN_PUNPCKHBW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_punpckhwd, "__builtin_ia32_punpckhwd", IX86_BUILTIN_PUNPCKHWD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_punpckhdq, "__builtin_ia32_punpckhdq", IX86_BUILTIN_PUNPCKHDQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_punpcklbw, "__builtin_ia32_punpcklbw", IX86_BUILTIN_PUNPCKLBW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_punpcklwd, "__builtin_ia32_punpcklwd", IX86_BUILTIN_PUNPCKLWD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_punpckldq, "__builtin_ia32_punpckldq", IX86_BUILTIN_PUNPCKLDQ, UNKNOWN, 0 },

  /* Special.  */
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_packsswb, 0, IX86_BUILTIN_PACKSSWB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_packssdw, 0, IX86_BUILTIN_PACKSSDW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_packuswb, 0, IX86_BUILTIN_PACKUSWB, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_cvtpi2ps, 0, IX86_BUILTIN_CVTPI2PS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_cvtsi2ss, 0, IX86_BUILTIN_CVTSI2SS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_64BIT, CODE_FOR_sse_cvtsi2ssq, 0, IX86_BUILTIN_CVTSI642SS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashlv4hi3, 0, IX86_BUILTIN_PSLLW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashlv4hi3, 0, IX86_BUILTIN_PSLLWI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashlv2si3, 0, IX86_BUILTIN_PSLLD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashlv2si3, 0, IX86_BUILTIN_PSLLDI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashldi3, 0, IX86_BUILTIN_PSLLQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashldi3, 0, IX86_BUILTIN_PSLLQI, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_lshrv4hi3, 0, IX86_BUILTIN_PSRLW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_lshrv4hi3, 0, IX86_BUILTIN_PSRLWI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_lshrv2si3, 0, IX86_BUILTIN_PSRLD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_lshrv2si3, 0, IX86_BUILTIN_PSRLDI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_lshrdi3, 0, IX86_BUILTIN_PSRLQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_lshrdi3, 0, IX86_BUILTIN_PSRLQI, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashrv4hi3, 0, IX86_BUILTIN_PSRAW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashrv4hi3, 0, IX86_BUILTIN_PSRAWI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashrv2si3, 0, IX86_BUILTIN_PSRAD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_ashrv2si3, 0, IX86_BUILTIN_PSRADI, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_psadbw, 0, IX86_BUILTIN_PSADBW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_mmx_pmaddwd, 0, IX86_BUILTIN_PMADDWD, UNKNOWN, 0 },

  /* SSE2 */
  { OPTION_MASK_ISA_SSE2, CODE_FOR_addv2df3, "__builtin_ia32_addpd", IX86_BUILTIN_ADDPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_subv2df3, "__builtin_ia32_subpd", IX86_BUILTIN_SUBPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_mulv2df3, "__builtin_ia32_mulpd", IX86_BUILTIN_MULPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_divv2df3, "__builtin_ia32_divpd", IX86_BUILTIN_DIVPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmaddv2df3,  "__builtin_ia32_addsd", IX86_BUILTIN_ADDSD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmsubv2df3,  "__builtin_ia32_subsd", IX86_BUILTIN_SUBSD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmulv2df3,  "__builtin_ia32_mulsd", IX86_BUILTIN_MULSD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmdivv2df3,  "__builtin_ia32_divsd", IX86_BUILTIN_DIVSD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpeqpd", IX86_BUILTIN_CMPEQPD, EQ, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpltpd", IX86_BUILTIN_CMPLTPD, LT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmplepd", IX86_BUILTIN_CMPLEPD, LE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpgtpd", IX86_BUILTIN_CMPGTPD, LT, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpgepd", IX86_BUILTIN_CMPGEPD, LE, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpunordpd", IX86_BUILTIN_CMPUNORDPD, UNORDERED, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpneqpd", IX86_BUILTIN_CMPNEQPD, NE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpnltpd", IX86_BUILTIN_CMPNLTPD, UNGE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpnlepd", IX86_BUILTIN_CMPNLEPD, UNGT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpngtpd", IX86_BUILTIN_CMPNGTPD, UNGE, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpngepd", IX86_BUILTIN_CMPNGEPD, UNGT, BUILTIN_DESC_SWAP_OPERANDS },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_maskcmpv2df3, "__builtin_ia32_cmpordpd", IX86_BUILTIN_CMPORDPD, ORDERED, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpeqsd", IX86_BUILTIN_CMPEQSD, EQ, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpltsd", IX86_BUILTIN_CMPLTSD, LT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmplesd", IX86_BUILTIN_CMPLESD, LE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpunordsd", IX86_BUILTIN_CMPUNORDSD, UNORDERED, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpneqsd", IX86_BUILTIN_CMPNEQSD, NE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpnltsd", IX86_BUILTIN_CMPNLTSD, UNGE, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpnlesd", IX86_BUILTIN_CMPNLESD, UNGT, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmmaskcmpv2df3, "__builtin_ia32_cmpordsd", IX86_BUILTIN_CMPORDSD, ORDERED, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sminv2df3, "__builtin_ia32_minpd", IX86_BUILTIN_MINPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_smaxv2df3, "__builtin_ia32_maxpd", IX86_BUILTIN_MAXPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmsminv2df3, "__builtin_ia32_minsd", IX86_BUILTIN_MINSD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_vmsmaxv2df3, "__builtin_ia32_maxsd", IX86_BUILTIN_MAXSD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_andv2df3, "__builtin_ia32_andpd", IX86_BUILTIN_ANDPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_nandv2df3,  "__builtin_ia32_andnpd", IX86_BUILTIN_ANDNPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_iorv2df3, "__builtin_ia32_orpd", IX86_BUILTIN_ORPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_xorv2df3,  "__builtin_ia32_xorpd", IX86_BUILTIN_XORPD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_movsd,  "__builtin_ia32_movsd", IX86_BUILTIN_MOVSD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_unpckhpd, "__builtin_ia32_unpckhpd", IX86_BUILTIN_UNPCKHPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_unpcklpd, "__builtin_ia32_unpcklpd", IX86_BUILTIN_UNPCKLPD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_vec_pack_sfix_v2df, "__builtin_ia32_vec_pack_sfix", IX86_BUILTIN_VEC_PACK_SFIX, UNKNOWN, 0 },

  /* SSE2 MMX */
  { OPTION_MASK_ISA_SSE2, CODE_FOR_addv16qi3, "__builtin_ia32_paddb128", IX86_BUILTIN_PADDB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_addv8hi3, "__builtin_ia32_paddw128", IX86_BUILTIN_PADDW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_addv4si3, "__builtin_ia32_paddd128", IX86_BUILTIN_PADDD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_addv2di3, "__builtin_ia32_paddq128", IX86_BUILTIN_PADDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_subv16qi3, "__builtin_ia32_psubb128", IX86_BUILTIN_PSUBB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_subv8hi3, "__builtin_ia32_psubw128", IX86_BUILTIN_PSUBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_subv4si3, "__builtin_ia32_psubd128", IX86_BUILTIN_PSUBD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_subv2di3, "__builtin_ia32_psubq128", IX86_BUILTIN_PSUBQ128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_ssaddv16qi3, "__builtin_ia32_paddsb128", IX86_BUILTIN_PADDSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_ssaddv8hi3, "__builtin_ia32_paddsw128", IX86_BUILTIN_PADDSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_sssubv16qi3, "__builtin_ia32_psubsb128", IX86_BUILTIN_PSUBSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_sssubv8hi3, "__builtin_ia32_psubsw128", IX86_BUILTIN_PSUBSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_usaddv16qi3, "__builtin_ia32_paddusb128", IX86_BUILTIN_PADDUSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_usaddv8hi3, "__builtin_ia32_paddusw128", IX86_BUILTIN_PADDUSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_ussubv16qi3, "__builtin_ia32_psubusb128", IX86_BUILTIN_PSUBUSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_MMX, CODE_FOR_sse2_ussubv8hi3, "__builtin_ia32_psubusw128", IX86_BUILTIN_PSUBUSW128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_mulv8hi3, "__builtin_ia32_pmullw128", IX86_BUILTIN_PMULLW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_smulv8hi3_highpart, "__builtin_ia32_pmulhw128", IX86_BUILTIN_PMULHW128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_andv2di3, "__builtin_ia32_pand128", IX86_BUILTIN_PAND128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_nandv2di3, "__builtin_ia32_pandn128", IX86_BUILTIN_PANDN128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_iorv2di3, "__builtin_ia32_por128", IX86_BUILTIN_POR128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_xorv2di3, "__builtin_ia32_pxor128", IX86_BUILTIN_PXOR128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_uavgv16qi3, "__builtin_ia32_pavgb128", IX86_BUILTIN_PAVGB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_uavgv8hi3, "__builtin_ia32_pavgw128", IX86_BUILTIN_PAVGW128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_eqv16qi3, "__builtin_ia32_pcmpeqb128", IX86_BUILTIN_PCMPEQB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_eqv8hi3, "__builtin_ia32_pcmpeqw128", IX86_BUILTIN_PCMPEQW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_eqv4si3, "__builtin_ia32_pcmpeqd128", IX86_BUILTIN_PCMPEQD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_gtv16qi3, "__builtin_ia32_pcmpgtb128", IX86_BUILTIN_PCMPGTB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_gtv8hi3, "__builtin_ia32_pcmpgtw128", IX86_BUILTIN_PCMPGTW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_gtv4si3, "__builtin_ia32_pcmpgtd128", IX86_BUILTIN_PCMPGTD128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_umaxv16qi3, "__builtin_ia32_pmaxub128", IX86_BUILTIN_PMAXUB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_smaxv8hi3, "__builtin_ia32_pmaxsw128", IX86_BUILTIN_PMAXSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_uminv16qi3, "__builtin_ia32_pminub128", IX86_BUILTIN_PMINUB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sminv8hi3, "__builtin_ia32_pminsw128", IX86_BUILTIN_PMINSW128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpckhbw, "__builtin_ia32_punpckhbw128", IX86_BUILTIN_PUNPCKHBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpckhwd, "__builtin_ia32_punpckhwd128", IX86_BUILTIN_PUNPCKHWD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpckhdq, "__builtin_ia32_punpckhdq128", IX86_BUILTIN_PUNPCKHDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpckhqdq, "__builtin_ia32_punpckhqdq128", IX86_BUILTIN_PUNPCKHQDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpcklbw, "__builtin_ia32_punpcklbw128", IX86_BUILTIN_PUNPCKLBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpcklwd, "__builtin_ia32_punpcklwd128", IX86_BUILTIN_PUNPCKLWD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpckldq, "__builtin_ia32_punpckldq128", IX86_BUILTIN_PUNPCKLDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_punpcklqdq, "__builtin_ia32_punpcklqdq128", IX86_BUILTIN_PUNPCKLQDQ128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_packsswb, "__builtin_ia32_packsswb128", IX86_BUILTIN_PACKSSWB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_packssdw, "__builtin_ia32_packssdw128", IX86_BUILTIN_PACKSSDW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_packuswb, "__builtin_ia32_packuswb128", IX86_BUILTIN_PACKUSWB128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_umulv8hi3_highpart, "__builtin_ia32_pmulhuw128", IX86_BUILTIN_PMULHUW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_psadbw, 0, IX86_BUILTIN_PSADBW128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_umulsidi3, 0, IX86_BUILTIN_PMULUDQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_umulv2siv2di3, 0, IX86_BUILTIN_PMULUDQ128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_ashlv8hi3, 0, IX86_BUILTIN_PSLLWI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_ashlv4si3, 0, IX86_BUILTIN_PSLLDI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_ashlv2di3, 0, IX86_BUILTIN_PSLLQI128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_lshrv8hi3, 0, IX86_BUILTIN_PSRLWI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_lshrv4si3, 0, IX86_BUILTIN_PSRLDI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_lshrv2di3, 0, IX86_BUILTIN_PSRLQI128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_ashrv8hi3, 0, IX86_BUILTIN_PSRAWI128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_ashrv4si3, 0, IX86_BUILTIN_PSRADI128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_pmaddwd, 0, IX86_BUILTIN_PMADDWD128, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtsi2sd, 0, IX86_BUILTIN_CVTSI2SD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_64BIT, CODE_FOR_sse2_cvtsi2sdq, 0, IX86_BUILTIN_CVTSI642SD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtsd2ss, 0, IX86_BUILTIN_CVTSD2SS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtss2sd, 0, IX86_BUILTIN_CVTSS2SD, UNKNOWN, 0 },

  /* SSE3 MMX */
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_addsubv4sf3, "__builtin_ia32_addsubps", IX86_BUILTIN_ADDSUBPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_addsubv2df3, "__builtin_ia32_addsubpd", IX86_BUILTIN_ADDSUBPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_haddv4sf3, "__builtin_ia32_haddps", IX86_BUILTIN_HADDPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_haddv2df3, "__builtin_ia32_haddpd", IX86_BUILTIN_HADDPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_hsubv4sf3, "__builtin_ia32_hsubps", IX86_BUILTIN_HSUBPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_hsubv2df3, "__builtin_ia32_hsubpd", IX86_BUILTIN_HSUBPD, UNKNOWN, 0 },

  /* SSSE3 */
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phaddwv8hi3, "__builtin_ia32_phaddw128", IX86_BUILTIN_PHADDW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phaddwv4hi3, "__builtin_ia32_phaddw", IX86_BUILTIN_PHADDW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phadddv4si3, "__builtin_ia32_phaddd128", IX86_BUILTIN_PHADDD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phadddv2si3, "__builtin_ia32_phaddd", IX86_BUILTIN_PHADDD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phaddswv8hi3, "__builtin_ia32_phaddsw128", IX86_BUILTIN_PHADDSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phaddswv4hi3, "__builtin_ia32_phaddsw", IX86_BUILTIN_PHADDSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phsubwv8hi3, "__builtin_ia32_phsubw128", IX86_BUILTIN_PHSUBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phsubwv4hi3, "__builtin_ia32_phsubw", IX86_BUILTIN_PHSUBW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phsubdv4si3, "__builtin_ia32_phsubd128", IX86_BUILTIN_PHSUBD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phsubdv2si3, "__builtin_ia32_phsubd", IX86_BUILTIN_PHSUBD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phsubswv8hi3, "__builtin_ia32_phsubsw128", IX86_BUILTIN_PHSUBSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_phsubswv4hi3, "__builtin_ia32_phsubsw", IX86_BUILTIN_PHSUBSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_pmaddubswv8hi3, "__builtin_ia32_pmaddubsw128", IX86_BUILTIN_PMADDUBSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_pmaddubswv4hi3, "__builtin_ia32_pmaddubsw", IX86_BUILTIN_PMADDUBSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_pmulhrswv8hi3, "__builtin_ia32_pmulhrsw128", IX86_BUILTIN_PMULHRSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_pmulhrswv4hi3, "__builtin_ia32_pmulhrsw", IX86_BUILTIN_PMULHRSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_pshufbv16qi3, "__builtin_ia32_pshufb128", IX86_BUILTIN_PSHUFB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_pshufbv8qi3, "__builtin_ia32_pshufb", IX86_BUILTIN_PSHUFB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_psignv16qi3, "__builtin_ia32_psignb128", IX86_BUILTIN_PSIGNB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_psignv8qi3, "__builtin_ia32_psignb", IX86_BUILTIN_PSIGNB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_psignv8hi3, "__builtin_ia32_psignw128", IX86_BUILTIN_PSIGNW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_psignv4hi3, "__builtin_ia32_psignw", IX86_BUILTIN_PSIGNW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_psignv4si3, "__builtin_ia32_psignd128", IX86_BUILTIN_PSIGND128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_ssse3_psignv2si3, "__builtin_ia32_psignd", IX86_BUILTIN_PSIGND, UNKNOWN, 0 },

  /* SSE4.1 */
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_packusdw, "__builtin_ia32_packusdw128", IX86_BUILTIN_PACKUSDW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_eqv2di3, "__builtin_ia32_pcmpeqq", IX86_BUILTIN_PCMPEQQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_smaxv16qi3, "__builtin_ia32_pmaxsb128", IX86_BUILTIN_PMAXSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_smaxv4si3, "__builtin_ia32_pmaxsd128", IX86_BUILTIN_PMAXSD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_umaxv4si3, "__builtin_ia32_pmaxud128", IX86_BUILTIN_PMAXUD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_umaxv8hi3, "__builtin_ia32_pmaxuw128", IX86_BUILTIN_PMAXUW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sminv16qi3, "__builtin_ia32_pminsb128", IX86_BUILTIN_PMINSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sminv4si3, "__builtin_ia32_pminsd128", IX86_BUILTIN_PMINSD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_uminv4si3, "__builtin_ia32_pminud128", IX86_BUILTIN_PMINUD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_uminv8hi3, "__builtin_ia32_pminuw128", IX86_BUILTIN_PMINUW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_mulv2siv2di3, 0, IX86_BUILTIN_PMULDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_mulv4si3, "__builtin_ia32_pmulld128", IX86_BUILTIN_PMULLD128, UNKNOWN, 0 },

  /* SSE4.2 */
  { OPTION_MASK_ISA_SSE4_2, CODE_FOR_sse4_2_gtv2di3, "__builtin_ia32_pcmpgtq", IX86_BUILTIN_PCMPGTQ, UNKNOWN, 0 },
};

static const struct builtin_description bdesc_1arg[] =
{
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, CODE_FOR_mmx_pmovmskb, 0, IX86_BUILTIN_PMOVMSKB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_movmskps, 0, IX86_BUILTIN_MOVMSKPS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_sqrtv4sf2, 0, IX86_BUILTIN_SQRTPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sqrtv4sf2, 0, IX86_BUILTIN_SQRTPS_NR, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_rsqrtv4sf2, 0, IX86_BUILTIN_RSQRTPS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_rsqrtv4sf2, 0, IX86_BUILTIN_RSQRTPS_NR, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_rcpv4sf2, 0, IX86_BUILTIN_RCPPS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_cvtps2pi, 0, IX86_BUILTIN_CVTPS2PI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_cvtss2si, 0, IX86_BUILTIN_CVTSS2SI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_64BIT, CODE_FOR_sse_cvtss2siq, 0, IX86_BUILTIN_CVTSS2SI64, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_cvttps2pi, 0, IX86_BUILTIN_CVTTPS2PI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE, CODE_FOR_sse_cvttss2si, 0, IX86_BUILTIN_CVTTSS2SI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_64BIT, CODE_FOR_sse_cvttss2siq, 0, IX86_BUILTIN_CVTTSS2SI64, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_pmovmskb, 0, IX86_BUILTIN_PMOVMSKB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_movmskpd, 0, IX86_BUILTIN_MOVMSKPD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sqrtv2df2, 0, IX86_BUILTIN_SQRTPD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtdq2pd, 0, IX86_BUILTIN_CVTDQ2PD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtdq2ps, 0, IX86_BUILTIN_CVTDQ2PS, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtpd2dq, 0, IX86_BUILTIN_CVTPD2DQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtpd2pi, 0, IX86_BUILTIN_CVTPD2PI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtpd2ps, 0, IX86_BUILTIN_CVTPD2PS, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvttpd2dq, 0, IX86_BUILTIN_CVTTPD2DQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvttpd2pi, 0, IX86_BUILTIN_CVTTPD2PI, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtpi2pd, 0, IX86_BUILTIN_CVTPI2PD, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtsd2si, 0, IX86_BUILTIN_CVTSD2SI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvttsd2si, 0, IX86_BUILTIN_CVTTSD2SI, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_64BIT, CODE_FOR_sse2_cvtsd2siq, 0, IX86_BUILTIN_CVTSD2SI64, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_64BIT, CODE_FOR_sse2_cvttsd2siq, 0, IX86_BUILTIN_CVTTSD2SI64, UNKNOWN, 0 },

  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtps2dq, 0, IX86_BUILTIN_CVTPS2DQ, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvtps2pd, 0, IX86_BUILTIN_CVTPS2PD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE2, CODE_FOR_sse2_cvttps2dq, 0, IX86_BUILTIN_CVTTPS2DQ, UNKNOWN, 0 },

  /* SSE3 */
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_movshdup, "__builtin_ia32_movshdup", IX86_BUILTIN_MOVSHDUP, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE3, CODE_FOR_sse3_movsldup, "__builtin_ia32_movsldup", IX86_BUILTIN_MOVSLDUP, UNKNOWN, 0 },

  /* SSSE3 */
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_absv16qi2, "__builtin_ia32_pabsb128", IX86_BUILTIN_PABSB128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_absv8qi2, "__builtin_ia32_pabsb", IX86_BUILTIN_PABSB, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_absv8hi2, "__builtin_ia32_pabsw128", IX86_BUILTIN_PABSW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_absv4hi2, "__builtin_ia32_pabsw", IX86_BUILTIN_PABSW, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_absv4si2, "__builtin_ia32_pabsd128", IX86_BUILTIN_PABSD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSSE3, CODE_FOR_absv2si2, "__builtin_ia32_pabsd", IX86_BUILTIN_PABSD, UNKNOWN, 0 },

  /* SSE4.1 */
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_extendv8qiv8hi2, 0, IX86_BUILTIN_PMOVSXBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_extendv4qiv4si2, 0, IX86_BUILTIN_PMOVSXBD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_extendv2qiv2di2, 0, IX86_BUILTIN_PMOVSXBQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_extendv4hiv4si2, 0, IX86_BUILTIN_PMOVSXWD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_extendv2hiv2di2, 0, IX86_BUILTIN_PMOVSXWQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_extendv2siv2di2, 0, IX86_BUILTIN_PMOVSXDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_zero_extendv8qiv8hi2, 0, IX86_BUILTIN_PMOVZXBW128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_zero_extendv4qiv4si2, 0, IX86_BUILTIN_PMOVZXBD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_zero_extendv2qiv2di2, 0, IX86_BUILTIN_PMOVZXBQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_zero_extendv4hiv4si2, 0, IX86_BUILTIN_PMOVZXWD128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_zero_extendv2hiv2di2, 0, IX86_BUILTIN_PMOVZXWQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_zero_extendv2siv2di2, 0, IX86_BUILTIN_PMOVZXDQ128, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_phminposuw, "__builtin_ia32_phminposuw128", IX86_BUILTIN_PHMINPOSUW128, UNKNOWN, 0 },

  /* Fake 1 arg builtins with a constant smaller than 8 bits as the 2nd arg.  */
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_roundpd, 0, IX86_BUILTIN_ROUNDPD, UNKNOWN, 0 },
  { OPTION_MASK_ISA_SSE4_1, CODE_FOR_sse4_1_roundps, 0, IX86_BUILTIN_ROUNDPS, UNKNOWN, 0 },
};

/* SSE5 */
enum multi_arg_type {
  MULTI_ARG_UNKNOWN,
  MULTI_ARG_3_SF,
  MULTI_ARG_3_DF,
  MULTI_ARG_3_DI,
  MULTI_ARG_3_SI,
  MULTI_ARG_3_SI_DI,
  MULTI_ARG_3_HI,
  MULTI_ARG_3_HI_SI,
  MULTI_ARG_3_QI,
  MULTI_ARG_3_PERMPS,
  MULTI_ARG_3_PERMPD,
  MULTI_ARG_2_SF,
  MULTI_ARG_2_DF,
  MULTI_ARG_2_DI,
  MULTI_ARG_2_SI,
  MULTI_ARG_2_HI,
  MULTI_ARG_2_QI,
  MULTI_ARG_2_DI_IMM,
  MULTI_ARG_2_SI_IMM,
  MULTI_ARG_2_HI_IMM,
  MULTI_ARG_2_QI_IMM,
  MULTI_ARG_2_SF_CMP,
  MULTI_ARG_2_DF_CMP,
  MULTI_ARG_2_DI_CMP,
  MULTI_ARG_2_SI_CMP,
  MULTI_ARG_2_HI_CMP,
  MULTI_ARG_2_QI_CMP,
  MULTI_ARG_2_DI_TF,
  MULTI_ARG_2_SI_TF,
  MULTI_ARG_2_HI_TF,
  MULTI_ARG_2_QI_TF,
  MULTI_ARG_2_SF_TF,
  MULTI_ARG_2_DF_TF,
  MULTI_ARG_1_SF,
  MULTI_ARG_1_DF,
  MULTI_ARG_1_DI,
  MULTI_ARG_1_SI,
  MULTI_ARG_1_HI,
  MULTI_ARG_1_QI,
  MULTI_ARG_1_SI_DI,
  MULTI_ARG_1_HI_DI,
  MULTI_ARG_1_HI_SI,
  MULTI_ARG_1_QI_DI,
  MULTI_ARG_1_QI_SI,
  MULTI_ARG_1_QI_HI,
  MULTI_ARG_1_PH2PS,
  MULTI_ARG_1_PS2PH
};

static const struct builtin_description bdesc_multi_arg[] =
{
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfmaddv4sf4,     "__builtin_ia32_fmaddss",    IX86_BUILTIN_FMADDSS,    0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfmaddv2df4,     "__builtin_ia32_fmaddsd",    IX86_BUILTIN_FMADDSD,    0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fmaddv4sf4,       "__builtin_ia32_fmaddps",    IX86_BUILTIN_FMADDPS,    0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fmaddv2df4,       "__builtin_ia32_fmaddpd",    IX86_BUILTIN_FMADDPD,    0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfmsubv4sf4,     "__builtin_ia32_fmsubss",    IX86_BUILTIN_FMSUBSS,    0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfmsubv2df4,     "__builtin_ia32_fmsubsd",    IX86_BUILTIN_FMSUBSD,    0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fmsubv4sf4,       "__builtin_ia32_fmsubps",    IX86_BUILTIN_FMSUBPS,    0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fmsubv2df4,       "__builtin_ia32_fmsubpd",    IX86_BUILTIN_FMSUBPD,    0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfnmaddv4sf4,    "__builtin_ia32_fnmaddss",   IX86_BUILTIN_FNMADDSS,   0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfnmaddv2df4,    "__builtin_ia32_fnmaddsd",   IX86_BUILTIN_FNMADDSD,   0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fnmaddv4sf4,      "__builtin_ia32_fnmaddps",   IX86_BUILTIN_FNMADDPS,   0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fnmaddv2df4,      "__builtin_ia32_fnmaddpd",   IX86_BUILTIN_FNMADDPD,   0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfnmsubv4sf4,    "__builtin_ia32_fnmsubss",   IX86_BUILTIN_FNMSUBSS,   0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_vmfnmsubv2df4,    "__builtin_ia32_fnmsubsd",   IX86_BUILTIN_FNMSUBSD,   0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fnmsubv4sf4,      "__builtin_ia32_fnmsubps",   IX86_BUILTIN_FNMSUBPS,   0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5i_fnmsubv2df4,      "__builtin_ia32_fnmsubpd",   IX86_BUILTIN_FNMSUBPD,   0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v2di,        "__builtin_ia32_pcmov",      IX86_BUILTIN_PCMOV_V2DI, 0,            (int)MULTI_ARG_3_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v2di,        "__builtin_ia32_pcmov_v2di", IX86_BUILTIN_PCMOV_V2DI, 0,            (int)MULTI_ARG_3_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v4si,        "__builtin_ia32_pcmov_v4si", IX86_BUILTIN_PCMOV_V4SI, 0,            (int)MULTI_ARG_3_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v8hi,        "__builtin_ia32_pcmov_v8hi", IX86_BUILTIN_PCMOV_V8HI, 0,            (int)MULTI_ARG_3_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v16qi,       "__builtin_ia32_pcmov_v16qi",IX86_BUILTIN_PCMOV_V16QI,0,            (int)MULTI_ARG_3_QI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v2df,        "__builtin_ia32_pcmov_v2df", IX86_BUILTIN_PCMOV_V2DF, 0,            (int)MULTI_ARG_3_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcmov_v4sf,        "__builtin_ia32_pcmov_v4sf", IX86_BUILTIN_PCMOV_V4SF, 0,            (int)MULTI_ARG_3_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pperm,             "__builtin_ia32_pperm",      IX86_BUILTIN_PPERM,      0,            (int)MULTI_ARG_3_QI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_permv4sf,          "__builtin_ia32_permps",     IX86_BUILTIN_PERMPS,     0,            (int)MULTI_ARG_3_PERMPS },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_permv2df,          "__builtin_ia32_permpd",     IX86_BUILTIN_PERMPD,     0,            (int)MULTI_ARG_3_PERMPD },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacssww,          "__builtin_ia32_pmacssww",   IX86_BUILTIN_PMACSSWW,   0,            (int)MULTI_ARG_3_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacsww,           "__builtin_ia32_pmacsww",    IX86_BUILTIN_PMACSWW,    0,            (int)MULTI_ARG_3_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacsswd,          "__builtin_ia32_pmacsswd",   IX86_BUILTIN_PMACSSWD,   0,            (int)MULTI_ARG_3_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacswd,           "__builtin_ia32_pmacswd",    IX86_BUILTIN_PMACSWD,    0,            (int)MULTI_ARG_3_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacssdd,          "__builtin_ia32_pmacssdd",   IX86_BUILTIN_PMACSSDD,   0,            (int)MULTI_ARG_3_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacsdd,           "__builtin_ia32_pmacsdd",    IX86_BUILTIN_PMACSDD,    0,            (int)MULTI_ARG_3_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacssdql,         "__builtin_ia32_pmacssdql",  IX86_BUILTIN_PMACSSDQL,  0,            (int)MULTI_ARG_3_SI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacssdqh,         "__builtin_ia32_pmacssdqh",  IX86_BUILTIN_PMACSSDQH,  0,            (int)MULTI_ARG_3_SI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacsdql,          "__builtin_ia32_pmacsdql",   IX86_BUILTIN_PMACSDQL,   0,            (int)MULTI_ARG_3_SI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmacsdqh,          "__builtin_ia32_pmacsdqh",   IX86_BUILTIN_PMACSDQH,   0,            (int)MULTI_ARG_3_SI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmadcsswd,         "__builtin_ia32_pmadcsswd",  IX86_BUILTIN_PMADCSSWD,  0,            (int)MULTI_ARG_3_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pmadcswd,          "__builtin_ia32_pmadcswd",   IX86_BUILTIN_PMADCSWD,   0,            (int)MULTI_ARG_3_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_rotlv2di3,         "__builtin_ia32_protq",      IX86_BUILTIN_PROTQ,      0,            (int)MULTI_ARG_2_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_rotlv4si3,         "__builtin_ia32_protd",      IX86_BUILTIN_PROTD,      0,            (int)MULTI_ARG_2_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_rotlv8hi3,         "__builtin_ia32_protw",      IX86_BUILTIN_PROTW,      0,            (int)MULTI_ARG_2_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_rotlv16qi3,        "__builtin_ia32_protb",      IX86_BUILTIN_PROTB,      0,            (int)MULTI_ARG_2_QI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_rotlv2di3,              "__builtin_ia32_protqi",     IX86_BUILTIN_PROTQ_IMM,  0,            (int)MULTI_ARG_2_DI_IMM },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_rotlv4si3,              "__builtin_ia32_protdi",     IX86_BUILTIN_PROTD_IMM,  0,            (int)MULTI_ARG_2_SI_IMM },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_rotlv8hi3,              "__builtin_ia32_protwi",     IX86_BUILTIN_PROTW_IMM,  0,            (int)MULTI_ARG_2_HI_IMM },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_rotlv16qi3,             "__builtin_ia32_protbi",     IX86_BUILTIN_PROTB_IMM,  0,            (int)MULTI_ARG_2_QI_IMM },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_ashlv2di3,         "__builtin_ia32_pshaq",      IX86_BUILTIN_PSHAQ,      0,            (int)MULTI_ARG_2_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_ashlv4si3,         "__builtin_ia32_pshad",      IX86_BUILTIN_PSHAD,      0,            (int)MULTI_ARG_2_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_ashlv8hi3,         "__builtin_ia32_pshaw",      IX86_BUILTIN_PSHAW,      0,            (int)MULTI_ARG_2_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_ashlv16qi3,        "__builtin_ia32_pshab",      IX86_BUILTIN_PSHAB,      0,            (int)MULTI_ARG_2_QI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_lshlv2di3,         "__builtin_ia32_pshlq",      IX86_BUILTIN_PSHLQ,      0,            (int)MULTI_ARG_2_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_lshlv4si3,         "__builtin_ia32_pshld",      IX86_BUILTIN_PSHLD,      0,            (int)MULTI_ARG_2_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_lshlv8hi3,         "__builtin_ia32_pshlw",      IX86_BUILTIN_PSHLW,      0,            (int)MULTI_ARG_2_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_lshlv16qi3,        "__builtin_ia32_pshlb",      IX86_BUILTIN_PSHLB,      0,            (int)MULTI_ARG_2_QI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmfrczv4sf2,       "__builtin_ia32_frczss",     IX86_BUILTIN_FRCZSS,     0,            (int)MULTI_ARG_2_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmfrczv2df2,       "__builtin_ia32_frczsd",     IX86_BUILTIN_FRCZSD,     0,            (int)MULTI_ARG_2_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_frczv4sf2,         "__builtin_ia32_frczps",     IX86_BUILTIN_FRCZPS,     0,            (int)MULTI_ARG_1_SF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_frczv2df2,         "__builtin_ia32_frczpd",     IX86_BUILTIN_FRCZPD,     0,            (int)MULTI_ARG_1_DF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_cvtph2ps,          "__builtin_ia32_cvtph2ps",   IX86_BUILTIN_CVTPH2PS,   0,            (int)MULTI_ARG_1_PH2PS },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_cvtps2ph,          "__builtin_ia32_cvtps2ph",   IX86_BUILTIN_CVTPS2PH,   0,            (int)MULTI_ARG_1_PS2PH },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddbw,           "__builtin_ia32_phaddbw",    IX86_BUILTIN_PHADDBW,    0,            (int)MULTI_ARG_1_QI_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddbd,           "__builtin_ia32_phaddbd",    IX86_BUILTIN_PHADDBD,    0,            (int)MULTI_ARG_1_QI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddbq,           "__builtin_ia32_phaddbq",    IX86_BUILTIN_PHADDBQ,    0,            (int)MULTI_ARG_1_QI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddwd,           "__builtin_ia32_phaddwd",    IX86_BUILTIN_PHADDWD,    0,            (int)MULTI_ARG_1_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddwq,           "__builtin_ia32_phaddwq",    IX86_BUILTIN_PHADDWQ,    0,            (int)MULTI_ARG_1_HI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phadddq,           "__builtin_ia32_phadddq",    IX86_BUILTIN_PHADDDQ,    0,            (int)MULTI_ARG_1_SI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddubw,          "__builtin_ia32_phaddubw",   IX86_BUILTIN_PHADDUBW,   0,            (int)MULTI_ARG_1_QI_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddubd,          "__builtin_ia32_phaddubd",   IX86_BUILTIN_PHADDUBD,   0,            (int)MULTI_ARG_1_QI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddubq,          "__builtin_ia32_phaddubq",   IX86_BUILTIN_PHADDUBQ,   0,            (int)MULTI_ARG_1_QI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phadduwd,          "__builtin_ia32_phadduwd",   IX86_BUILTIN_PHADDUWD,   0,            (int)MULTI_ARG_1_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phadduwq,          "__builtin_ia32_phadduwq",   IX86_BUILTIN_PHADDUWQ,   0,            (int)MULTI_ARG_1_HI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phaddudq,          "__builtin_ia32_phaddudq",   IX86_BUILTIN_PHADDUDQ,   0,            (int)MULTI_ARG_1_SI_DI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phsubbw,           "__builtin_ia32_phsubbw",    IX86_BUILTIN_PHSUBBW,    0,            (int)MULTI_ARG_1_QI_HI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phsubwd,           "__builtin_ia32_phsubwd",    IX86_BUILTIN_PHSUBWD,    0,            (int)MULTI_ARG_1_HI_SI },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_phsubdq,           "__builtin_ia32_phsubdq",    IX86_BUILTIN_PHSUBDQ,    0,            (int)MULTI_ARG_1_SI_DI },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comeqss",    IX86_BUILTIN_COMEQSS,    EQ,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comness",    IX86_BUILTIN_COMNESS,    NE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comneqss",   IX86_BUILTIN_COMNESS,    NE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comltss",    IX86_BUILTIN_COMLTSS,    LT,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comless",    IX86_BUILTIN_COMLESS,    LE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comgtss",    IX86_BUILTIN_COMGTSS,    GT,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comgess",    IX86_BUILTIN_COMGESS,    GE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comueqss",   IX86_BUILTIN_COMUEQSS,   UNEQ,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comuness",   IX86_BUILTIN_COMUNESS,   LTGT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comuneqss",  IX86_BUILTIN_COMUNESS,   LTGT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comunltss",  IX86_BUILTIN_COMULTSS,   UNLT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comunless",  IX86_BUILTIN_COMULESS,   UNLE,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comungtss",  IX86_BUILTIN_COMUGTSS,   UNGT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comungess",  IX86_BUILTIN_COMUGESS,   UNGE,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comordss",   IX86_BUILTIN_COMORDSS,   ORDERED,      (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv4sf3,    "__builtin_ia32_comunordss", IX86_BUILTIN_COMUNORDSS, UNORDERED,    (int)MULTI_ARG_2_SF_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comeqsd",    IX86_BUILTIN_COMEQSD,    EQ,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comnesd",    IX86_BUILTIN_COMNESD,    NE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comneqsd",   IX86_BUILTIN_COMNESD,    NE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comltsd",    IX86_BUILTIN_COMLTSD,    LT,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comlesd",    IX86_BUILTIN_COMLESD,    LE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comgtsd",    IX86_BUILTIN_COMGTSD,    GT,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comgesd",    IX86_BUILTIN_COMGESD,    GE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comueqsd",   IX86_BUILTIN_COMUEQSD,   UNEQ,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comunesd",   IX86_BUILTIN_COMUNESD,   LTGT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comuneqsd",  IX86_BUILTIN_COMUNESD,   LTGT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comunltsd",  IX86_BUILTIN_COMULTSD,   UNLT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comunlesd",  IX86_BUILTIN_COMULESD,   UNLE,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comungtsd",  IX86_BUILTIN_COMUGTSD,   UNGT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comungesd",  IX86_BUILTIN_COMUGESD,   UNGE,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comordsd",   IX86_BUILTIN_COMORDSD,   ORDERED,      (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_vmmaskcmpv2df3,    "__builtin_ia32_comunordsd", IX86_BUILTIN_COMUNORDSD, UNORDERED,    (int)MULTI_ARG_2_DF_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comeqps",    IX86_BUILTIN_COMEQPS,    EQ,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comneps",    IX86_BUILTIN_COMNEPS,    NE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comneqps",   IX86_BUILTIN_COMNEPS,    NE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comltps",    IX86_BUILTIN_COMLTPS,    LT,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comleps",    IX86_BUILTIN_COMLEPS,    LE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comgtps",    IX86_BUILTIN_COMGTPS,    GT,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comgeps",    IX86_BUILTIN_COMGEPS,    GE,           (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comueqps",   IX86_BUILTIN_COMUEQPS,   UNEQ,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comuneps",   IX86_BUILTIN_COMUNEPS,   LTGT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comuneqps",  IX86_BUILTIN_COMUNEPS,   LTGT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comunltps",  IX86_BUILTIN_COMULTPS,   UNLT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comunleps",  IX86_BUILTIN_COMULEPS,   UNLE,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comungtps",  IX86_BUILTIN_COMUGTPS,   UNGT,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comungeps",  IX86_BUILTIN_COMUGEPS,   UNGE,         (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comordps",   IX86_BUILTIN_COMORDPS,   ORDERED,      (int)MULTI_ARG_2_SF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4sf3,      "__builtin_ia32_comunordps", IX86_BUILTIN_COMUNORDPS, UNORDERED,    (int)MULTI_ARG_2_SF_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comeqpd",    IX86_BUILTIN_COMEQPD,    EQ,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comnepd",    IX86_BUILTIN_COMNEPD,    NE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comneqpd",   IX86_BUILTIN_COMNEPD,    NE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comltpd",    IX86_BUILTIN_COMLTPD,    LT,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comlepd",    IX86_BUILTIN_COMLEPD,    LE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comgtpd",    IX86_BUILTIN_COMGTPD,    GT,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comgepd",    IX86_BUILTIN_COMGEPD,    GE,           (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comueqpd",   IX86_BUILTIN_COMUEQPD,   UNEQ,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comunepd",   IX86_BUILTIN_COMUNEPD,   LTGT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comuneqpd",  IX86_BUILTIN_COMUNEPD,   LTGT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comunltpd",  IX86_BUILTIN_COMULTPD,   UNLT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comunlepd",  IX86_BUILTIN_COMULEPD,   UNLE,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comungtpd",  IX86_BUILTIN_COMUGTPD,   UNGT,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comungepd",  IX86_BUILTIN_COMUGEPD,   UNGE,         (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comordpd",   IX86_BUILTIN_COMORDPD,   ORDERED,      (int)MULTI_ARG_2_DF_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2df3,      "__builtin_ia32_comunordpd", IX86_BUILTIN_COMUNORDPD, UNORDERED,    (int)MULTI_ARG_2_DF_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomeqb",    IX86_BUILTIN_PCOMEQB,    EQ,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomneb",    IX86_BUILTIN_PCOMNEB,    NE,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomneqb",   IX86_BUILTIN_PCOMNEB,    NE,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomltb",    IX86_BUILTIN_PCOMLTB,    LT,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomleb",    IX86_BUILTIN_PCOMLEB,    LE,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomgtb",    IX86_BUILTIN_PCOMGTB,    GT,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv16qi3,     "__builtin_ia32_pcomgeb",    IX86_BUILTIN_PCOMGEB,    GE,           (int)MULTI_ARG_2_QI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomeqw",    IX86_BUILTIN_PCOMEQW,    EQ,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomnew",    IX86_BUILTIN_PCOMNEW,    NE,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomneqw",   IX86_BUILTIN_PCOMNEW,    NE,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomltw",    IX86_BUILTIN_PCOMLTW,    LT,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomlew",    IX86_BUILTIN_PCOMLEW,    LE,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomgtw",    IX86_BUILTIN_PCOMGTW,    GT,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv8hi3,      "__builtin_ia32_pcomgew",    IX86_BUILTIN_PCOMGEW,    GE,           (int)MULTI_ARG_2_HI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomeqd",    IX86_BUILTIN_PCOMEQD,    EQ,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomned",    IX86_BUILTIN_PCOMNED,    NE,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomneqd",   IX86_BUILTIN_PCOMNED,    NE,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomltd",    IX86_BUILTIN_PCOMLTD,    LT,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomled",    IX86_BUILTIN_PCOMLED,    LE,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomgtd",    IX86_BUILTIN_PCOMGTD,    GT,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv4si3,      "__builtin_ia32_pcomged",    IX86_BUILTIN_PCOMGED,    GE,           (int)MULTI_ARG_2_SI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomeqq",    IX86_BUILTIN_PCOMEQQ,    EQ,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomneq",    IX86_BUILTIN_PCOMNEQ,    NE,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomneqq",   IX86_BUILTIN_PCOMNEQ,    NE,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomltq",    IX86_BUILTIN_PCOMLTQ,    LT,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomleq",    IX86_BUILTIN_PCOMLEQ,    LE,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomgtq",    IX86_BUILTIN_PCOMGTQ,    GT,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmpv2di3,      "__builtin_ia32_pcomgeq",    IX86_BUILTIN_PCOMGEQ,    GE,           (int)MULTI_ARG_2_DI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v16qi3,"__builtin_ia32_pcomequb",   IX86_BUILTIN_PCOMEQUB,   EQ,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v16qi3,"__builtin_ia32_pcomneub",   IX86_BUILTIN_PCOMNEUB,   NE,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v16qi3,"__builtin_ia32_pcomnequb",  IX86_BUILTIN_PCOMNEUB,   NE,           (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv16qi3, "__builtin_ia32_pcomltub",   IX86_BUILTIN_PCOMLTUB,   LTU,          (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv16qi3, "__builtin_ia32_pcomleub",   IX86_BUILTIN_PCOMLEUB,   LEU,          (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv16qi3, "__builtin_ia32_pcomgtub",   IX86_BUILTIN_PCOMGTUB,   GTU,          (int)MULTI_ARG_2_QI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv16qi3, "__builtin_ia32_pcomgeub",   IX86_BUILTIN_PCOMGEUB,   GEU,          (int)MULTI_ARG_2_QI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v8hi3, "__builtin_ia32_pcomequw",   IX86_BUILTIN_PCOMEQUW,   EQ,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v8hi3, "__builtin_ia32_pcomneuw",   IX86_BUILTIN_PCOMNEUW,   NE,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v8hi3, "__builtin_ia32_pcomnequw",  IX86_BUILTIN_PCOMNEUW,   NE,           (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv8hi3,  "__builtin_ia32_pcomltuw",   IX86_BUILTIN_PCOMLTUW,   LTU,          (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv8hi3,  "__builtin_ia32_pcomleuw",   IX86_BUILTIN_PCOMLEUW,   LEU,          (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv8hi3,  "__builtin_ia32_pcomgtuw",   IX86_BUILTIN_PCOMGTUW,   GTU,          (int)MULTI_ARG_2_HI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv8hi3,  "__builtin_ia32_pcomgeuw",   IX86_BUILTIN_PCOMGEUW,   GEU,          (int)MULTI_ARG_2_HI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v4si3, "__builtin_ia32_pcomequd",   IX86_BUILTIN_PCOMEQUD,   EQ,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v4si3, "__builtin_ia32_pcomneud",   IX86_BUILTIN_PCOMNEUD,   NE,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v4si3, "__builtin_ia32_pcomnequd",  IX86_BUILTIN_PCOMNEUD,   NE,           (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv4si3,  "__builtin_ia32_pcomltud",   IX86_BUILTIN_PCOMLTUD,   LTU,          (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv4si3,  "__builtin_ia32_pcomleud",   IX86_BUILTIN_PCOMLEUD,   LEU,          (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv4si3,  "__builtin_ia32_pcomgtud",   IX86_BUILTIN_PCOMGTUD,   GTU,          (int)MULTI_ARG_2_SI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv4si3,  "__builtin_ia32_pcomgeud",   IX86_BUILTIN_PCOMGEUD,   GEU,          (int)MULTI_ARG_2_SI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v2di3, "__builtin_ia32_pcomequq",   IX86_BUILTIN_PCOMEQUQ,   EQ,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v2di3, "__builtin_ia32_pcomneuq",   IX86_BUILTIN_PCOMNEUQ,   NE,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_uns2v2di3, "__builtin_ia32_pcomnequq",  IX86_BUILTIN_PCOMNEUQ,   NE,           (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv2di3,  "__builtin_ia32_pcomltuq",   IX86_BUILTIN_PCOMLTUQ,   LTU,          (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv2di3,  "__builtin_ia32_pcomleuq",   IX86_BUILTIN_PCOMLEUQ,   LEU,          (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv2di3,  "__builtin_ia32_pcomgtuq",   IX86_BUILTIN_PCOMGTUQ,   GTU,          (int)MULTI_ARG_2_DI_CMP },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_maskcmp_unsv2di3,  "__builtin_ia32_pcomgeuq",   IX86_BUILTIN_PCOMGEUQ,   GEU,          (int)MULTI_ARG_2_DI_CMP },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv4sf3,       "__builtin_ia32_comfalsess", IX86_BUILTIN_COMFALSESS, COM_FALSE_S,  (int)MULTI_ARG_2_SF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv4sf3,       "__builtin_ia32_comtruess",  IX86_BUILTIN_COMTRUESS,  COM_TRUE_S,   (int)MULTI_ARG_2_SF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv4sf3,       "__builtin_ia32_comfalseps", IX86_BUILTIN_COMFALSEPS, COM_FALSE_P,  (int)MULTI_ARG_2_SF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv4sf3,       "__builtin_ia32_comtrueps",  IX86_BUILTIN_COMTRUEPS,  COM_TRUE_P,   (int)MULTI_ARG_2_SF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv2df3,       "__builtin_ia32_comfalsesd", IX86_BUILTIN_COMFALSESD, COM_FALSE_S,  (int)MULTI_ARG_2_DF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv2df3,       "__builtin_ia32_comtruesd",  IX86_BUILTIN_COMTRUESD,  COM_TRUE_S,   (int)MULTI_ARG_2_DF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv2df3,       "__builtin_ia32_comfalsepd", IX86_BUILTIN_COMFALSEPD, COM_FALSE_P,  (int)MULTI_ARG_2_DF_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_com_tfv2df3,       "__builtin_ia32_comtruepd",  IX86_BUILTIN_COMTRUEPD,  COM_TRUE_P,   (int)MULTI_ARG_2_DF_TF },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv16qi3,     "__builtin_ia32_pcomfalseb", IX86_BUILTIN_PCOMFALSEB, PCOM_FALSE,   (int)MULTI_ARG_2_QI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv8hi3,      "__builtin_ia32_pcomfalsew", IX86_BUILTIN_PCOMFALSEW, PCOM_FALSE,   (int)MULTI_ARG_2_HI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv4si3,      "__builtin_ia32_pcomfalsed", IX86_BUILTIN_PCOMFALSED, PCOM_FALSE,   (int)MULTI_ARG_2_SI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv2di3,      "__builtin_ia32_pcomfalseq", IX86_BUILTIN_PCOMFALSEQ, PCOM_FALSE,   (int)MULTI_ARG_2_DI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv16qi3,     "__builtin_ia32_pcomfalseub",IX86_BUILTIN_PCOMFALSEUB,PCOM_FALSE,   (int)MULTI_ARG_2_QI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv8hi3,      "__builtin_ia32_pcomfalseuw",IX86_BUILTIN_PCOMFALSEUW,PCOM_FALSE,   (int)MULTI_ARG_2_HI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv4si3,      "__builtin_ia32_pcomfalseud",IX86_BUILTIN_PCOMFALSEUD,PCOM_FALSE,   (int)MULTI_ARG_2_SI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv2di3,      "__builtin_ia32_pcomfalseuq",IX86_BUILTIN_PCOMFALSEUQ,PCOM_FALSE,   (int)MULTI_ARG_2_DI_TF },

  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv16qi3,     "__builtin_ia32_pcomtrueb",  IX86_BUILTIN_PCOMTRUEB,  PCOM_TRUE,    (int)MULTI_ARG_2_QI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv8hi3,      "__builtin_ia32_pcomtruew",  IX86_BUILTIN_PCOMTRUEW,  PCOM_TRUE,    (int)MULTI_ARG_2_HI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv4si3,      "__builtin_ia32_pcomtrued",  IX86_BUILTIN_PCOMTRUED,  PCOM_TRUE,    (int)MULTI_ARG_2_SI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv2di3,      "__builtin_ia32_pcomtrueq",  IX86_BUILTIN_PCOMTRUEQ,  PCOM_TRUE,    (int)MULTI_ARG_2_DI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv16qi3,     "__builtin_ia32_pcomtrueub", IX86_BUILTIN_PCOMTRUEUB, PCOM_TRUE,    (int)MULTI_ARG_2_QI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv8hi3,      "__builtin_ia32_pcomtrueuw", IX86_BUILTIN_PCOMTRUEUW, PCOM_TRUE,    (int)MULTI_ARG_2_HI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv4si3,      "__builtin_ia32_pcomtrueud", IX86_BUILTIN_PCOMTRUEUD, PCOM_TRUE,    (int)MULTI_ARG_2_SI_TF },
  { OPTION_MASK_ISA_SSE5, CODE_FOR_sse5_pcom_tfv2di3,      "__builtin_ia32_pcomtrueuq", IX86_BUILTIN_PCOMTRUEUQ, PCOM_TRUE,    (int)MULTI_ARG_2_DI_TF },
};

/* Set up all the MMX/SSE builtins.  This is not called if TARGET_MMX
   is zero.  Otherwise, if TARGET_SSE is not set, only expand the MMX
   builtins.  */
static void
ix86_init_mmx_sse_builtins (void)
{
  const struct builtin_description * d;
  size_t i;

  tree V16QI_type_node = build_vector_type_for_mode (char_type_node, V16QImode);
  tree V2SI_type_node = build_vector_type_for_mode (intSI_type_node, V2SImode);
  tree V2SF_type_node = build_vector_type_for_mode (float_type_node, V2SFmode);
  tree V2DI_type_node
    = build_vector_type_for_mode (long_long_integer_type_node, V2DImode);
  tree V2DF_type_node = build_vector_type_for_mode (double_type_node, V2DFmode);
  tree V4SF_type_node = build_vector_type_for_mode (float_type_node, V4SFmode);
  tree V4SI_type_node = build_vector_type_for_mode (intSI_type_node, V4SImode);
  tree V4HI_type_node = build_vector_type_for_mode (intHI_type_node, V4HImode);
  tree V8QI_type_node = build_vector_type_for_mode (char_type_node, V8QImode);
  tree V8HI_type_node = build_vector_type_for_mode (intHI_type_node, V8HImode);

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
  tree void_ftype_unsigned_unsigned
    = build_function_type_list (void_type_node, unsigned_type_node,
				unsigned_type_node, NULL_TREE);
  tree void_ftype_pcvoid_unsigned_unsigned
    = build_function_type_list (void_type_node, const_ptr_type_node,
				unsigned_type_node, unsigned_type_node,
				NULL_TREE);
  tree unsigned_ftype_void
    = build_function_type (unsigned_type_node, void_list_node);
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
  tree v16qi_ftype_v16qi
    = build_function_type_list (V16QI_type_node, V16QI_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi
    = build_function_type_list (V8HI_type_node, V8HI_type_node, NULL_TREE);
  tree v4si_ftype_v4si
    = build_function_type_list (V4SI_type_node, V4SI_type_node, NULL_TREE);
  tree v8qi_ftype_v8qi
    = build_function_type_list (V8QI_type_node, V8QI_type_node, NULL_TREE);
  tree v4hi_ftype_v4hi
    = build_function_type_list (V4HI_type_node, V4HI_type_node, NULL_TREE);

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

  tree di_ftype_di_di_int
    = build_function_type_list (long_long_unsigned_type_node,
				long_long_unsigned_type_node,
				long_long_unsigned_type_node,
				integer_type_node, NULL_TREE);

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
  tree pdouble_type_node = build_pointer_type (double_type_node);
  tree pcdouble_type_node = build_pointer_type (
				build_type_variant (double_type_node, 1, 0));
  tree int_ftype_v2df_v2df
    = build_function_type_list (integer_type_node,
				V2DF_type_node, V2DF_type_node, NULL_TREE);

  tree void_ftype_pcvoid
    = build_function_type_list (void_type_node, const_ptr_type_node, NULL_TREE);
  tree v4sf_ftype_v4si
    = build_function_type_list (V4SF_type_node, V4SI_type_node, NULL_TREE);
  tree v4si_ftype_v4sf
    = build_function_type_list (V4SI_type_node, V4SF_type_node, NULL_TREE);
  tree v2df_ftype_v4si
    = build_function_type_list (V2DF_type_node, V4SI_type_node, NULL_TREE);
  tree v4si_ftype_v2df
    = build_function_type_list (V4SI_type_node, V2DF_type_node, NULL_TREE);
  tree v4si_ftype_v2df_v2df
    = build_function_type_list (V4SI_type_node,
				V2DF_type_node, V2DF_type_node, NULL_TREE);
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
  tree v2df_ftype_v2df_pcdouble
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, pcdouble_type_node, NULL_TREE);
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
  tree v2di_ftype_v2di_int
    = build_function_type_list (V2DI_type_node,
				V2DI_type_node, integer_type_node, NULL_TREE);
  tree v2di_ftype_v2di_v2di_int
    = build_function_type_list (V2DI_type_node, V2DI_type_node,
				V2DI_type_node, integer_type_node, NULL_TREE);
  tree v4si_ftype_v4si_int
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node, integer_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi_int
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node, integer_type_node, NULL_TREE);
  tree v4si_ftype_v8hi_v8hi
    = build_function_type_list (V4SI_type_node,
				V8HI_type_node, V8HI_type_node, NULL_TREE);
  tree di_ftype_v8qi_v8qi
    = build_function_type_list (long_long_unsigned_type_node,
				V8QI_type_node, V8QI_type_node, NULL_TREE);
  tree di_ftype_v2si_v2si
    = build_function_type_list (long_long_unsigned_type_node,
				V2SI_type_node, V2SI_type_node, NULL_TREE);
  tree v2di_ftype_v16qi_v16qi
    = build_function_type_list (V2DI_type_node,
				V16QI_type_node, V16QI_type_node, NULL_TREE);
  tree v2di_ftype_v4si_v4si
    = build_function_type_list (V2DI_type_node,
				V4SI_type_node, V4SI_type_node, NULL_TREE);
  tree int_ftype_v16qi
    = build_function_type_list (integer_type_node, V16QI_type_node, NULL_TREE);
  tree v16qi_ftype_pcchar
    = build_function_type_list (V16QI_type_node, pcchar_type_node, NULL_TREE);
  tree void_ftype_pchar_v16qi
    = build_function_type_list (void_type_node,
			        pchar_type_node, V16QI_type_node, NULL_TREE);

  tree v2di_ftype_v2di_unsigned_unsigned
    = build_function_type_list (V2DI_type_node, V2DI_type_node,
                                unsigned_type_node, unsigned_type_node,
                                NULL_TREE);
  tree v2di_ftype_v2di_v2di_unsigned_unsigned
    = build_function_type_list (V2DI_type_node, V2DI_type_node, V2DI_type_node,
                                unsigned_type_node, unsigned_type_node,
                                NULL_TREE);
  tree v2di_ftype_v2di_v16qi
    = build_function_type_list (V2DI_type_node, V2DI_type_node, V16QI_type_node,
                                NULL_TREE);
  tree v2df_ftype_v2df_v2df_v2df
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node, V2DF_type_node,
				V2DF_type_node, NULL_TREE);
  tree v4sf_ftype_v4sf_v4sf_v4sf
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node, V4SF_type_node,
				V4SF_type_node, NULL_TREE);
  tree v8hi_ftype_v16qi
    = build_function_type_list (V8HI_type_node, V16QI_type_node,
				NULL_TREE);
  tree v4si_ftype_v16qi
    = build_function_type_list (V4SI_type_node, V16QI_type_node,
				NULL_TREE);
  tree v2di_ftype_v16qi
    = build_function_type_list (V2DI_type_node, V16QI_type_node,
				NULL_TREE);
  tree v4si_ftype_v8hi
    = build_function_type_list (V4SI_type_node, V8HI_type_node,
				NULL_TREE);
  tree v2di_ftype_v8hi
    = build_function_type_list (V2DI_type_node, V8HI_type_node,
				NULL_TREE);
  tree v2di_ftype_v4si
    = build_function_type_list (V2DI_type_node, V4SI_type_node,
				NULL_TREE);
  tree v2di_ftype_pv2di
    = build_function_type_list (V2DI_type_node, pv2di_type_node,
				NULL_TREE);
  tree v16qi_ftype_v16qi_v16qi_int
    = build_function_type_list (V16QI_type_node, V16QI_type_node,
				V16QI_type_node, integer_type_node,
				NULL_TREE);
  tree v16qi_ftype_v16qi_v16qi_v16qi
    = build_function_type_list (V16QI_type_node, V16QI_type_node,
				V16QI_type_node, V16QI_type_node,
				NULL_TREE);
  tree v8hi_ftype_v8hi_v8hi_int
    = build_function_type_list (V8HI_type_node, V8HI_type_node,
				V8HI_type_node, integer_type_node,
				NULL_TREE);
  tree v4si_ftype_v4si_v4si_int
    = build_function_type_list (V4SI_type_node, V4SI_type_node,
				V4SI_type_node, integer_type_node,
				NULL_TREE);
  tree int_ftype_v2di_v2di
    = build_function_type_list (integer_type_node,
				V2DI_type_node, V2DI_type_node,
				NULL_TREE);
  tree int_ftype_v16qi_int_v16qi_int_int
    = build_function_type_list (integer_type_node,
				V16QI_type_node,
				integer_type_node,
				V16QI_type_node,
				integer_type_node,
				integer_type_node,
				NULL_TREE);
  tree v16qi_ftype_v16qi_int_v16qi_int_int
    = build_function_type_list (V16QI_type_node,
				V16QI_type_node,
				integer_type_node,
				V16QI_type_node,
				integer_type_node,
				integer_type_node,
				NULL_TREE);
  tree int_ftype_v16qi_v16qi_int
    = build_function_type_list (integer_type_node,
				V16QI_type_node,
				V16QI_type_node,
				integer_type_node,
				NULL_TREE);

  /* SSE5 instructions */
  tree v2di_ftype_v2di_v2di_v2di
    = build_function_type_list (V2DI_type_node,
				V2DI_type_node,
				V2DI_type_node,
				V2DI_type_node,
				NULL_TREE);

  tree v4si_ftype_v4si_v4si_v4si
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node,
				V4SI_type_node,
				V4SI_type_node,
				NULL_TREE);

  tree v4si_ftype_v4si_v4si_v2di
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node,
				V4SI_type_node,
				V2DI_type_node,
				NULL_TREE);

  tree v8hi_ftype_v8hi_v8hi_v8hi
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node,
				V8HI_type_node,
				V8HI_type_node,
				NULL_TREE);

  tree v8hi_ftype_v8hi_v8hi_v4si
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node,
				V8HI_type_node,
				V4SI_type_node,
				NULL_TREE);

  tree v2df_ftype_v2df_v2df_v16qi
    = build_function_type_list (V2DF_type_node,
				V2DF_type_node,
				V2DF_type_node,
				V16QI_type_node,
				NULL_TREE);

  tree v4sf_ftype_v4sf_v4sf_v16qi
    = build_function_type_list (V4SF_type_node,
				V4SF_type_node,
				V4SF_type_node,
				V16QI_type_node,
				NULL_TREE);

  tree v2di_ftype_v2di_si
    = build_function_type_list (V2DI_type_node,
				V2DI_type_node,
				integer_type_node,
				NULL_TREE);

  tree v4si_ftype_v4si_si
    = build_function_type_list (V4SI_type_node,
				V4SI_type_node,
				integer_type_node,
				NULL_TREE);

  tree v8hi_ftype_v8hi_si
    = build_function_type_list (V8HI_type_node,
				V8HI_type_node,
				integer_type_node,
				NULL_TREE);

  tree v16qi_ftype_v16qi_si
    = build_function_type_list (V16QI_type_node,
				V16QI_type_node,
				integer_type_node,
				NULL_TREE);
  tree v4sf_ftype_v4hi
    = build_function_type_list (V4SF_type_node,
				V4HI_type_node,
				NULL_TREE);

  tree v4hi_ftype_v4sf
    = build_function_type_list (V4HI_type_node,
				V4SF_type_node,
				NULL_TREE);

  tree v2di_ftype_v2di
    = build_function_type_list (V2DI_type_node, V2DI_type_node, NULL_TREE);

  tree ftype;

  /* The __float80 type.  */
  if (TYPE_MODE (long_double_type_node) == XFmode)
    (*lang_hooks.types.register_builtin_type) (long_double_type_node,
					       "__float80");
  else
    {
      /* The __float80 type.  */
      tree float80_type_node = make_node (REAL_TYPE);

      TYPE_PRECISION (float80_type_node) = 80;
      layout_type (float80_type_node);
      (*lang_hooks.types.register_builtin_type) (float80_type_node,
						 "__float80");
    }

  if (TARGET_64BIT)
    {
      tree float128_type_node = make_node (REAL_TYPE);

      TYPE_PRECISION (float128_type_node) = 128;
      layout_type (float128_type_node);
      (*lang_hooks.types.register_builtin_type) (float128_type_node,
						 "__float128");

      /* TFmode support builtins.  */
      ftype = build_function_type (float128_type_node,
				   void_list_node);
      def_builtin (OPTION_MASK_ISA_64BIT, "__builtin_infq", ftype, IX86_BUILTIN_INFQ);

      ftype = build_function_type_list (float128_type_node,
					float128_type_node,
					NULL_TREE);
      def_builtin_const (OPTION_MASK_ISA_64BIT, "__builtin_fabsq", ftype, IX86_BUILTIN_FABSQ);

      ftype = build_function_type_list (float128_type_node,
					float128_type_node,
					float128_type_node,
					NULL_TREE);
      def_builtin_const (OPTION_MASK_ISA_64BIT, "__builtin_copysignq", ftype, IX86_BUILTIN_COPYSIGNQ);
    }

  /* Add all SSE builtins that are more or less simple operations on
     three operands.  */
  for (i = 0, d = bdesc_sse_3arg;
       i < ARRAY_SIZE (bdesc_sse_3arg);
       i++, d++)
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
	  type = v16qi_ftype_v16qi_v16qi_int;
	  break;
	case V8HImode:
	  type = v8hi_ftype_v8hi_v8hi_int;
	  break;
	case V4SImode:
	  type = v4si_ftype_v4si_v4si_int;
	  break;
	case V2DImode:
	  type = v2di_ftype_v2di_v2di_int;
	  break;
	case V2DFmode:
	  type = v2df_ftype_v2df_v2df_int;
	  break;
	case V4SFmode:
	  type = v4sf_ftype_v4sf_v4sf_int;
	  break;
	default:
	  gcc_unreachable ();
	}

      /* Override for variable blends.  */
      switch (d->icode)
	{
	case CODE_FOR_sse4_1_blendvpd:
	  type = v2df_ftype_v2df_v2df_v2df;
	  break;
	case CODE_FOR_sse4_1_blendvps:
	  type = v4sf_ftype_v4sf_v4sf_v4sf;
	  break;
	case CODE_FOR_sse4_1_pblendvb:
	  type = v16qi_ftype_v16qi_v16qi_v16qi;
	  break;
	default:
	  break;
	}

      def_builtin_const (d->mask, d->name, type, d->code);
    }

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
	  gcc_unreachable ();
	}

      /* Override for comparisons.  */
      if (d->icode == CODE_FOR_sse_maskcmpv4sf3
	  || d->icode == CODE_FOR_sse_vmmaskcmpv4sf3)
	type = v4si_ftype_v4sf_v4sf;

      if (d->icode == CODE_FOR_sse2_maskcmpv2df3
	  || d->icode == CODE_FOR_sse2_vmmaskcmpv2df3)
	type = v2di_ftype_v2df_v2df;

      if (d->icode == CODE_FOR_vec_pack_sfix_v2df)
	type = v4si_ftype_v2df_v2df;

      def_builtin_const (d->mask, d->name, type, d->code);
    }

  /* Add all builtins that are more or less simple operations on 1 operand.  */
  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    {
      enum machine_mode mode;
      tree type;

      if (d->name == 0)
	continue;
      mode = insn_data[d->icode].operand[1].mode;

      switch (mode)
	{
	case V16QImode:
	  type = v16qi_ftype_v16qi;
	  break;
	case V8HImode:
	  type = v8hi_ftype_v8hi;
	  break;
	case V4SImode:
	  type = v4si_ftype_v4si;
	  break;
	case V2DFmode:
	  type = v2df_ftype_v2df;
	  break;
	case V4SFmode:
	  type = v4sf_ftype_v4sf;
	  break;
	case V8QImode:
	  type = v8qi_ftype_v8qi;
	  break;
	case V4HImode:
	  type = v4hi_ftype_v4hi;
	  break;
	case V2SImode:
	  type = v2si_ftype_v2si;
	  break;

	default:
	  abort ();
	}

      def_builtin_const (d->mask, d->name, type, d->code);
    }

  /* pcmpestr[im] insns.  */
  for (i = 0, d = bdesc_pcmpestr;
       i < ARRAY_SIZE (bdesc_pcmpestr);
       i++, d++)
    {
      if (d->code == IX86_BUILTIN_PCMPESTRM128)
	ftype = v16qi_ftype_v16qi_int_v16qi_int_int;
      else
	ftype = int_ftype_v16qi_int_v16qi_int_int;
      def_builtin_const (d->mask, d->name, ftype, d->code);
    }

  /* pcmpistr[im] insns.  */
  for (i = 0, d = bdesc_pcmpistr;
       i < ARRAY_SIZE (bdesc_pcmpistr);
       i++, d++)
    {
      if (d->code == IX86_BUILTIN_PCMPISTRM128)
	ftype = v16qi_ftype_v16qi_v16qi_int;
      else
	ftype = int_ftype_v16qi_v16qi_int;
      def_builtin_const (d->mask, d->name, ftype, d->code);
    }

  /* Add the remaining MMX insns with somewhat more complicated types.  */
  def_builtin (OPTION_MASK_ISA_MMX, "__builtin_ia32_emms", void_ftype_void, IX86_BUILTIN_EMMS);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psllw", v4hi_ftype_v4hi_di, IX86_BUILTIN_PSLLW);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_pslld", v2si_ftype_v2si_di, IX86_BUILTIN_PSLLD);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psllq", di_ftype_di_di, IX86_BUILTIN_PSLLQ);

  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psrlw", v4hi_ftype_v4hi_di, IX86_BUILTIN_PSRLW);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psrld", v2si_ftype_v2si_di, IX86_BUILTIN_PSRLD);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psrlq", di_ftype_di_di, IX86_BUILTIN_PSRLQ);

  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psraw", v4hi_ftype_v4hi_di, IX86_BUILTIN_PSRAW);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_psrad", v2si_ftype_v2si_di, IX86_BUILTIN_PSRAD);

  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pshufw", v4hi_ftype_v4hi_int, IX86_BUILTIN_PSHUFW);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_pmaddwd", v2si_ftype_v4hi_v4hi, IX86_BUILTIN_PMADDWD);

  /* comi/ucomi insns.  */
  for (i = 0, d = bdesc_comi; i < ARRAY_SIZE (bdesc_comi); i++, d++)
    if (d->mask == OPTION_MASK_ISA_SSE2)
      def_builtin_const (d->mask, d->name, int_ftype_v2df_v2df, d->code);
    else
      def_builtin_const (d->mask, d->name, int_ftype_v4sf_v4sf, d->code);

  /* ptest insns.  */
  for (i = 0, d = bdesc_ptest; i < ARRAY_SIZE (bdesc_ptest); i++, d++)
    def_builtin_const (d->mask, d->name, int_ftype_v2di_v2di, d->code);

  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_packsswb", v8qi_ftype_v4hi_v4hi, IX86_BUILTIN_PACKSSWB);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_packssdw", v4hi_ftype_v2si_v2si, IX86_BUILTIN_PACKSSDW);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_packuswb", v8qi_ftype_v4hi_v4hi, IX86_BUILTIN_PACKUSWB);

  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_ldmxcsr", void_ftype_unsigned, IX86_BUILTIN_LDMXCSR);
  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_stmxcsr", unsigned_ftype_void, IX86_BUILTIN_STMXCSR);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_cvtpi2ps", v4sf_ftype_v4sf_v2si, IX86_BUILTIN_CVTPI2PS);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_cvtps2pi", v2si_ftype_v4sf, IX86_BUILTIN_CVTPS2PI);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_cvtsi2ss", v4sf_ftype_v4sf_int, IX86_BUILTIN_CVTSI2SS);
  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_64BIT, "__builtin_ia32_cvtsi642ss", v4sf_ftype_v4sf_int64, IX86_BUILTIN_CVTSI642SS);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_cvtss2si", int_ftype_v4sf, IX86_BUILTIN_CVTSS2SI);
  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_64BIT, "__builtin_ia32_cvtss2si64", int64_ftype_v4sf, IX86_BUILTIN_CVTSS2SI64);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_cvttps2pi", v2si_ftype_v4sf, IX86_BUILTIN_CVTTPS2PI);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_cvttss2si", int_ftype_v4sf, IX86_BUILTIN_CVTTSS2SI);
  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_64BIT, "__builtin_ia32_cvttss2si64", int64_ftype_v4sf, IX86_BUILTIN_CVTTSS2SI64);

  def_builtin (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_maskmovq", void_ftype_v8qi_v8qi_pchar, IX86_BUILTIN_MASKMOVQ);

  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_loadups", v4sf_ftype_pcfloat, IX86_BUILTIN_LOADUPS);
  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_storeups", void_ftype_pfloat_v4sf, IX86_BUILTIN_STOREUPS);

  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_loadhps", v4sf_ftype_v4sf_pv2si, IX86_BUILTIN_LOADHPS);
  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_loadlps", v4sf_ftype_v4sf_pv2si, IX86_BUILTIN_LOADLPS);
  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_storehps", void_ftype_pv2si_v4sf, IX86_BUILTIN_STOREHPS);
  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_storelps", void_ftype_pv2si_v4sf, IX86_BUILTIN_STORELPS);

  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_movmskps", int_ftype_v4sf, IX86_BUILTIN_MOVMSKPS);
  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pmovmskb", int_ftype_v8qi, IX86_BUILTIN_PMOVMSKB);
  def_builtin (OPTION_MASK_ISA_SSE, "__builtin_ia32_movntps", void_ftype_pfloat_v4sf, IX86_BUILTIN_MOVNTPS);
  def_builtin (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_movntq", void_ftype_pdi_di, IX86_BUILTIN_MOVNTQ);

  def_builtin (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_sfence", void_ftype_void, IX86_BUILTIN_SFENCE);

  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_psadbw", di_ftype_v8qi_v8qi, IX86_BUILTIN_PSADBW);

  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_rcpps", v4sf_ftype_v4sf, IX86_BUILTIN_RCPPS);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_rcpss", v4sf_ftype_v4sf, IX86_BUILTIN_RCPSS);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_rsqrtps", v4sf_ftype_v4sf, IX86_BUILTIN_RSQRTPS);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_rsqrtps_nr", v4sf_ftype_v4sf, IX86_BUILTIN_RSQRTPS_NR);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_rsqrtss", v4sf_ftype_v4sf, IX86_BUILTIN_RSQRTSS);
  ftype = build_function_type_list (float_type_node,
				    float_type_node,
				    NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_rsqrtf", ftype, IX86_BUILTIN_RSQRTF);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_sqrtps", v4sf_ftype_v4sf, IX86_BUILTIN_SQRTPS);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_sqrtps_nr", v4sf_ftype_v4sf, IX86_BUILTIN_SQRTPS_NR);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_sqrtss", v4sf_ftype_v4sf, IX86_BUILTIN_SQRTSS);

  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_shufps", v4sf_ftype_v4sf_v4sf_int, IX86_BUILTIN_SHUFPS);

  /* Original 3DNow!  */
  def_builtin (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_femms", void_ftype_void, IX86_BUILTIN_FEMMS);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pavgusb", v8qi_ftype_v8qi_v8qi, IX86_BUILTIN_PAVGUSB);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pf2id", v2si_ftype_v2sf, IX86_BUILTIN_PF2ID);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfacc", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFACC);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfadd", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFADD);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfcmpeq", v2si_ftype_v2sf_v2sf, IX86_BUILTIN_PFCMPEQ);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfcmpge", v2si_ftype_v2sf_v2sf, IX86_BUILTIN_PFCMPGE);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfcmpgt", v2si_ftype_v2sf_v2sf, IX86_BUILTIN_PFCMPGT);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfmax", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFMAX);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfmin", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFMIN);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfmul", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFMUL);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfrcp", v2sf_ftype_v2sf, IX86_BUILTIN_PFRCP);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfrcpit1", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFRCPIT1);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfrcpit2", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFRCPIT2);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfrsqrt", v2sf_ftype_v2sf, IX86_BUILTIN_PFRSQRT);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfrsqit1", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFRSQIT1);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfsub", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFSUB);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pfsubr", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFSUBR);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pi2fd", v2sf_ftype_v2si, IX86_BUILTIN_PI2FD);
  def_builtin_const (OPTION_MASK_ISA_3DNOW, "__builtin_ia32_pmulhrw", v4hi_ftype_v4hi_v4hi, IX86_BUILTIN_PMULHRW);

  /* 3DNow! extension as used in the Athlon CPU.  */
  def_builtin_const (OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pf2iw", v2si_ftype_v2sf, IX86_BUILTIN_PF2IW);
  def_builtin_const (OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pfnacc", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFNACC);
  def_builtin_const (OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pfpnacc", v2sf_ftype_v2sf_v2sf, IX86_BUILTIN_PFPNACC);
  def_builtin_const (OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pi2fw", v2sf_ftype_v2si, IX86_BUILTIN_PI2FW);
  def_builtin_const (OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pswapdsf", v2sf_ftype_v2sf, IX86_BUILTIN_PSWAPDSF);
  def_builtin_const (OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_pswapdsi", v2si_ftype_v2si, IX86_BUILTIN_PSWAPDSI);

  /* SSE2 */
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_maskmovdqu", void_ftype_v16qi_v16qi_pchar, IX86_BUILTIN_MASKMOVDQU);

  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_loadupd", v2df_ftype_pcdouble, IX86_BUILTIN_LOADUPD);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_storeupd", void_ftype_pdouble_v2df, IX86_BUILTIN_STOREUPD);

  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_loadhpd", v2df_ftype_v2df_pcdouble, IX86_BUILTIN_LOADHPD);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_loadlpd", v2df_ftype_v2df_pcdouble, IX86_BUILTIN_LOADLPD);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_movmskpd", int_ftype_v2df, IX86_BUILTIN_MOVMSKPD);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pmovmskb128", int_ftype_v16qi, IX86_BUILTIN_PMOVMSKB128);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_movnti", void_ftype_pint_int, IX86_BUILTIN_MOVNTI);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_movntpd", void_ftype_pdouble_v2df, IX86_BUILTIN_MOVNTPD);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_movntdq", void_ftype_pv2di_v2di, IX86_BUILTIN_MOVNTDQ);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pshufd", v4si_ftype_v4si_int, IX86_BUILTIN_PSHUFD);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pshuflw", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSHUFLW);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pshufhw", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSHUFHW);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psadbw128", v2di_ftype_v16qi_v16qi, IX86_BUILTIN_PSADBW128);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_sqrtpd", v2df_ftype_v2df, IX86_BUILTIN_SQRTPD);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_sqrtsd", v2df_ftype_v2df, IX86_BUILTIN_SQRTSD);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_shufpd", v2df_ftype_v2df_v2df_int, IX86_BUILTIN_SHUFPD);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtdq2pd", v2df_ftype_v4si, IX86_BUILTIN_CVTDQ2PD);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtdq2ps", v4sf_ftype_v4si, IX86_BUILTIN_CVTDQ2PS);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtpd2dq", v4si_ftype_v2df, IX86_BUILTIN_CVTPD2DQ);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtpd2pi", v2si_ftype_v2df, IX86_BUILTIN_CVTPD2PI);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtpd2ps", v4sf_ftype_v2df, IX86_BUILTIN_CVTPD2PS);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvttpd2dq", v4si_ftype_v2df, IX86_BUILTIN_CVTTPD2DQ);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvttpd2pi", v2si_ftype_v2df, IX86_BUILTIN_CVTTPD2PI);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtpi2pd", v2df_ftype_v2si, IX86_BUILTIN_CVTPI2PD);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtsd2si", int_ftype_v2df, IX86_BUILTIN_CVTSD2SI);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvttsd2si", int_ftype_v2df, IX86_BUILTIN_CVTTSD2SI);
  def_builtin_const (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_64BIT, "__builtin_ia32_cvtsd2si64", int64_ftype_v2df, IX86_BUILTIN_CVTSD2SI64);
  def_builtin_const (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_64BIT, "__builtin_ia32_cvttsd2si64", int64_ftype_v2df, IX86_BUILTIN_CVTTSD2SI64);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtps2dq", v4si_ftype_v4sf, IX86_BUILTIN_CVTPS2DQ);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtps2pd", v2df_ftype_v4sf, IX86_BUILTIN_CVTPS2PD);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvttps2dq", v4si_ftype_v4sf, IX86_BUILTIN_CVTTPS2DQ);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtsi2sd", v2df_ftype_v2df_int, IX86_BUILTIN_CVTSI2SD);
  def_builtin_const (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_64BIT, "__builtin_ia32_cvtsi642sd", v2df_ftype_v2df_int64, IX86_BUILTIN_CVTSI642SD);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtsd2ss", v4sf_ftype_v4sf_v2df, IX86_BUILTIN_CVTSD2SS);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_cvtss2sd", v2df_ftype_v2df_v4sf, IX86_BUILTIN_CVTSS2SD);

  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_clflush", void_ftype_pcvoid, IX86_BUILTIN_CLFLUSH);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_lfence", void_ftype_void, IX86_BUILTIN_LFENCE);
  x86_mfence = def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_mfence", void_ftype_void, IX86_BUILTIN_MFENCE);

  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_loaddqu", v16qi_ftype_pcchar, IX86_BUILTIN_LOADDQU);
  def_builtin (OPTION_MASK_ISA_SSE2, "__builtin_ia32_storedqu", void_ftype_pchar_v16qi, IX86_BUILTIN_STOREDQU);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pmuludq", di_ftype_v2si_v2si, IX86_BUILTIN_PMULUDQ);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pmuludq128", v2di_ftype_v4si_v4si, IX86_BUILTIN_PMULUDQ128);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pslldqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSLLDQI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psllwi128", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSLLWI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pslldi128", v4si_ftype_v4si_int, IX86_BUILTIN_PSLLDI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psllqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSLLQI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psllw128", v8hi_ftype_v8hi_v8hi, IX86_BUILTIN_PSLLW128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pslld128", v4si_ftype_v4si_v4si, IX86_BUILTIN_PSLLD128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psllq128", v2di_ftype_v2di_v2di, IX86_BUILTIN_PSLLQ128);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrldqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSRLDQI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrlwi128", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSRLWI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrldi128", v4si_ftype_v4si_int, IX86_BUILTIN_PSRLDI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrlqi128", v2di_ftype_v2di_int, IX86_BUILTIN_PSRLQI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrlw128", v8hi_ftype_v8hi_v8hi, IX86_BUILTIN_PSRLW128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrld128", v4si_ftype_v4si_v4si, IX86_BUILTIN_PSRLD128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrlq128", v2di_ftype_v2di_v2di, IX86_BUILTIN_PSRLQ128);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrawi128", v8hi_ftype_v8hi_int, IX86_BUILTIN_PSRAWI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psradi128", v4si_ftype_v4si_int, IX86_BUILTIN_PSRADI128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psraw128", v8hi_ftype_v8hi_v8hi, IX86_BUILTIN_PSRAW128);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_psrad128", v4si_ftype_v4si_v4si, IX86_BUILTIN_PSRAD128);

  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_pmaddwd128", v4si_ftype_v8hi_v8hi, IX86_BUILTIN_PMADDWD128);

  /* Prescott New Instructions.  */
  def_builtin (OPTION_MASK_ISA_SSE3, "__builtin_ia32_monitor", void_ftype_pcvoid_unsigned_unsigned, IX86_BUILTIN_MONITOR);
  def_builtin (OPTION_MASK_ISA_SSE3, "__builtin_ia32_mwait", void_ftype_unsigned_unsigned, IX86_BUILTIN_MWAIT);
  def_builtin (OPTION_MASK_ISA_SSE3, "__builtin_ia32_lddqu", v16qi_ftype_pcchar, IX86_BUILTIN_LDDQU);

  /* SSSE3.  */
  def_builtin_const (OPTION_MASK_ISA_SSSE3, "__builtin_ia32_palignr128", v2di_ftype_v2di_v2di_int, IX86_BUILTIN_PALIGNR128);
  def_builtin_const (OPTION_MASK_ISA_SSSE3, "__builtin_ia32_palignr", di_ftype_di_di_int, IX86_BUILTIN_PALIGNR);

  /* SSE4.1. */
  def_builtin (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_movntdqa", v2di_ftype_pv2di, IX86_BUILTIN_MOVNTDQA);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovsxbw128", v8hi_ftype_v16qi, IX86_BUILTIN_PMOVSXBW128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovsxbd128", v4si_ftype_v16qi, IX86_BUILTIN_PMOVSXBD128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovsxbq128", v2di_ftype_v16qi, IX86_BUILTIN_PMOVSXBQ128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovsxwd128", v4si_ftype_v8hi, IX86_BUILTIN_PMOVSXWD128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovsxwq128", v2di_ftype_v8hi, IX86_BUILTIN_PMOVSXWQ128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovsxdq128", v2di_ftype_v4si, IX86_BUILTIN_PMOVSXDQ128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovzxbw128", v8hi_ftype_v16qi, IX86_BUILTIN_PMOVZXBW128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovzxbd128", v4si_ftype_v16qi, IX86_BUILTIN_PMOVZXBD128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovzxbq128", v2di_ftype_v16qi, IX86_BUILTIN_PMOVZXBQ128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovzxwd128", v4si_ftype_v8hi, IX86_BUILTIN_PMOVZXWD128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovzxwq128", v2di_ftype_v8hi, IX86_BUILTIN_PMOVZXWQ128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmovzxdq128", v2di_ftype_v4si, IX86_BUILTIN_PMOVZXDQ128);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_pmuldq128", v2di_ftype_v4si_v4si, IX86_BUILTIN_PMULDQ128);

  /* SSE4.1 and SSE5 */
  def_builtin_const (OPTION_MASK_ISA_ROUND, "__builtin_ia32_roundpd", v2df_ftype_v2df_int, IX86_BUILTIN_ROUNDPD);
  def_builtin_const (OPTION_MASK_ISA_ROUND, "__builtin_ia32_roundps", v4sf_ftype_v4sf_int, IX86_BUILTIN_ROUNDPS);
  def_builtin_const (OPTION_MASK_ISA_ROUND, "__builtin_ia32_roundsd", v2df_ftype_v2df_v2df_int, IX86_BUILTIN_ROUNDSD);
  def_builtin_const (OPTION_MASK_ISA_ROUND, "__builtin_ia32_roundss", v4sf_ftype_v4sf_v4sf_int, IX86_BUILTIN_ROUNDSS);

  /* SSE4.2. */
  ftype = build_function_type_list (unsigned_type_node,
				    unsigned_type_node,
				    unsigned_char_type_node,
				    NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_2, "__builtin_ia32_crc32qi", ftype, IX86_BUILTIN_CRC32QI);
  ftype = build_function_type_list (unsigned_type_node,
				    unsigned_type_node,
				    short_unsigned_type_node,
				    NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_2, "__builtin_ia32_crc32hi", ftype, IX86_BUILTIN_CRC32HI);
  ftype = build_function_type_list (unsigned_type_node,
				    unsigned_type_node,
				    unsigned_type_node,
				    NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_2, "__builtin_ia32_crc32si", ftype, IX86_BUILTIN_CRC32SI);
  ftype = build_function_type_list (long_long_unsigned_type_node,
				    long_long_unsigned_type_node,
				    long_long_unsigned_type_node,
				    NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_2 | OPTION_MASK_ISA_64BIT, "__builtin_ia32_crc32di", ftype, IX86_BUILTIN_CRC32DI);

  /* AMDFAM10 SSE4A New built-ins  */
  def_builtin (OPTION_MASK_ISA_SSE4A, "__builtin_ia32_movntsd", void_ftype_pdouble_v2df, IX86_BUILTIN_MOVNTSD);
  def_builtin (OPTION_MASK_ISA_SSE4A, "__builtin_ia32_movntss", void_ftype_pfloat_v4sf, IX86_BUILTIN_MOVNTSS);
  def_builtin_const (OPTION_MASK_ISA_SSE4A, "__builtin_ia32_extrqi", v2di_ftype_v2di_unsigned_unsigned, IX86_BUILTIN_EXTRQI);
  def_builtin_const (OPTION_MASK_ISA_SSE4A, "__builtin_ia32_extrq", v2di_ftype_v2di_v16qi,  IX86_BUILTIN_EXTRQ);
  def_builtin_const (OPTION_MASK_ISA_SSE4A, "__builtin_ia32_insertqi", v2di_ftype_v2di_v2di_unsigned_unsigned, IX86_BUILTIN_INSERTQI);
  def_builtin_const (OPTION_MASK_ISA_SSE4A, "__builtin_ia32_insertq", v2di_ftype_v2di_v2di, IX86_BUILTIN_INSERTQ);

  /* Access to the vec_init patterns.  */
  ftype = build_function_type_list (V2SI_type_node, integer_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_vec_init_v2si", ftype, IX86_BUILTIN_VEC_INIT_V2SI);

  ftype = build_function_type_list (V4HI_type_node, short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_vec_init_v4hi", ftype, IX86_BUILTIN_VEC_INIT_V4HI);

  ftype = build_function_type_list (V8QI_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_vec_init_v8qi", ftype, IX86_BUILTIN_VEC_INIT_V8QI);

  /* Access to the vec_extract patterns.  */
  ftype = build_function_type_list (double_type_node, V2DF_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_vec_ext_v2df", ftype, IX86_BUILTIN_VEC_EXT_V2DF);

  ftype = build_function_type_list (long_long_integer_type_node,
				    V2DI_type_node, integer_type_node,
				    NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_vec_ext_v2di", ftype, IX86_BUILTIN_VEC_EXT_V2DI);

  ftype = build_function_type_list (float_type_node, V4SF_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE, "__builtin_ia32_vec_ext_v4sf", ftype, IX86_BUILTIN_VEC_EXT_V4SF);

  ftype = build_function_type_list (intSI_type_node, V4SI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_vec_ext_v4si", ftype, IX86_BUILTIN_VEC_EXT_V4SI);

  ftype = build_function_type_list (intHI_type_node, V8HI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_vec_ext_v8hi", ftype, IX86_BUILTIN_VEC_EXT_V8HI);

  ftype = build_function_type_list (intHI_type_node, V4HI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_vec_ext_v4hi", ftype, IX86_BUILTIN_VEC_EXT_V4HI);

  ftype = build_function_type_list (intSI_type_node, V2SI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_MMX, "__builtin_ia32_vec_ext_v2si", ftype, IX86_BUILTIN_VEC_EXT_V2SI);

  ftype = build_function_type_list (intQI_type_node, V16QI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_vec_ext_v16qi", ftype, IX86_BUILTIN_VEC_EXT_V16QI);

  /* Access to the vec_set patterns.  */
  ftype = build_function_type_list (V2DI_type_node, V2DI_type_node,
				    intDI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1 | OPTION_MASK_ISA_64BIT, "__builtin_ia32_vec_set_v2di", ftype, IX86_BUILTIN_VEC_SET_V2DI);

  ftype = build_function_type_list (V4SF_type_node, V4SF_type_node,
				    float_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_vec_set_v4sf", ftype, IX86_BUILTIN_VEC_SET_V4SF);

  ftype = build_function_type_list (V4SI_type_node, V4SI_type_node,
				    intSI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_vec_set_v4si", ftype, IX86_BUILTIN_VEC_SET_V4SI);

  ftype = build_function_type_list (V8HI_type_node, V8HI_type_node,
				    intHI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE2, "__builtin_ia32_vec_set_v8hi", ftype, IX86_BUILTIN_VEC_SET_V8HI);

  ftype = build_function_type_list (V4HI_type_node, V4HI_type_node,
				    intHI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A, "__builtin_ia32_vec_set_v4hi", ftype, IX86_BUILTIN_VEC_SET_V4HI);

  ftype = build_function_type_list (V16QI_type_node, V16QI_type_node,
				    intQI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin_const (OPTION_MASK_ISA_SSE4_1, "__builtin_ia32_vec_set_v16qi", ftype, IX86_BUILTIN_VEC_SET_V16QI);

  /* Add SSE5 multi-arg argument instructions */
  for (i = 0, d = bdesc_multi_arg; i < ARRAY_SIZE (bdesc_multi_arg); i++, d++)
    {
      tree mtype = NULL_TREE;

      if (d->name == 0)
	continue;

      switch ((enum multi_arg_type)d->flag)
	{
	case MULTI_ARG_3_SF:     mtype = v4sf_ftype_v4sf_v4sf_v4sf; 	break;
	case MULTI_ARG_3_DF:     mtype = v2df_ftype_v2df_v2df_v2df; 	break;
	case MULTI_ARG_3_DI:     mtype = v2di_ftype_v2di_v2di_v2di; 	break;
	case MULTI_ARG_3_SI:     mtype = v4si_ftype_v4si_v4si_v4si; 	break;
	case MULTI_ARG_3_SI_DI:  mtype = v4si_ftype_v4si_v4si_v2di; 	break;
	case MULTI_ARG_3_HI:     mtype = v8hi_ftype_v8hi_v8hi_v8hi; 	break;
	case MULTI_ARG_3_HI_SI:  mtype = v8hi_ftype_v8hi_v8hi_v4si; 	break;
	case MULTI_ARG_3_QI:     mtype = v16qi_ftype_v16qi_v16qi_v16qi; break;
	case MULTI_ARG_3_PERMPS: mtype = v4sf_ftype_v4sf_v4sf_v16qi; 	break;
	case MULTI_ARG_3_PERMPD: mtype = v2df_ftype_v2df_v2df_v16qi; 	break;
	case MULTI_ARG_2_SF:     mtype = v4sf_ftype_v4sf_v4sf;      	break;
	case MULTI_ARG_2_DF:     mtype = v2df_ftype_v2df_v2df;      	break;
	case MULTI_ARG_2_DI:     mtype = v2di_ftype_v2di_v2di;      	break;
	case MULTI_ARG_2_SI:     mtype = v4si_ftype_v4si_v4si;      	break;
	case MULTI_ARG_2_HI:     mtype = v8hi_ftype_v8hi_v8hi;      	break;
	case MULTI_ARG_2_QI:     mtype = v16qi_ftype_v16qi_v16qi;      	break;
	case MULTI_ARG_2_DI_IMM: mtype = v2di_ftype_v2di_si;        	break;
	case MULTI_ARG_2_SI_IMM: mtype = v4si_ftype_v4si_si;        	break;
	case MULTI_ARG_2_HI_IMM: mtype = v8hi_ftype_v8hi_si;        	break;
	case MULTI_ARG_2_QI_IMM: mtype = v16qi_ftype_v16qi_si;        	break;
	case MULTI_ARG_2_SF_CMP: mtype = v4sf_ftype_v4sf_v4sf;      	break;
	case MULTI_ARG_2_DF_CMP: mtype = v2df_ftype_v2df_v2df;      	break;
	case MULTI_ARG_2_DI_CMP: mtype = v2di_ftype_v2di_v2di;      	break;
	case MULTI_ARG_2_SI_CMP: mtype = v4si_ftype_v4si_v4si;      	break;
	case MULTI_ARG_2_HI_CMP: mtype = v8hi_ftype_v8hi_v8hi;      	break;
	case MULTI_ARG_2_QI_CMP: mtype = v16qi_ftype_v16qi_v16qi;      	break;
	case MULTI_ARG_2_SF_TF:  mtype = v4sf_ftype_v4sf_v4sf;      	break;
	case MULTI_ARG_2_DF_TF:  mtype = v2df_ftype_v2df_v2df;      	break;
	case MULTI_ARG_2_DI_TF:  mtype = v2di_ftype_v2di_v2di;      	break;
	case MULTI_ARG_2_SI_TF:  mtype = v4si_ftype_v4si_v4si;      	break;
	case MULTI_ARG_2_HI_TF:  mtype = v8hi_ftype_v8hi_v8hi;      	break;
	case MULTI_ARG_2_QI_TF:  mtype = v16qi_ftype_v16qi_v16qi;      	break;
	case MULTI_ARG_1_SF:     mtype = v4sf_ftype_v4sf;           	break;
	case MULTI_ARG_1_DF:     mtype = v2df_ftype_v2df;           	break;
	case MULTI_ARG_1_DI:     mtype = v2di_ftype_v2di;           	break;
	case MULTI_ARG_1_SI:     mtype = v4si_ftype_v4si;           	break;
	case MULTI_ARG_1_HI:     mtype = v8hi_ftype_v8hi;           	break;
	case MULTI_ARG_1_QI:     mtype = v16qi_ftype_v16qi;           	break;
	case MULTI_ARG_1_SI_DI:  mtype = v2di_ftype_v4si;           	break;
	case MULTI_ARG_1_HI_DI:  mtype = v2di_ftype_v8hi;           	break;
	case MULTI_ARG_1_HI_SI:  mtype = v4si_ftype_v8hi;           	break;
	case MULTI_ARG_1_QI_DI:  mtype = v2di_ftype_v16qi;           	break;
	case MULTI_ARG_1_QI_SI:  mtype = v4si_ftype_v16qi;           	break;
	case MULTI_ARG_1_QI_HI:  mtype = v8hi_ftype_v16qi;           	break;
	case MULTI_ARG_1_PH2PS:  mtype = v4sf_ftype_v4hi;		break;
	case MULTI_ARG_1_PS2PH:  mtype = v4hi_ftype_v4sf;		break;
	case MULTI_ARG_UNKNOWN:
	default:
	  gcc_unreachable ();
	}

      if (mtype)
	def_builtin_const (d->mask, d->name, mtype, d->code);
    }
}

static void
ix86_init_builtins (void)
{
  if (TARGET_MMX)
    ix86_init_mmx_sse_builtins ();
}

/* Errors in the source file can cause expand_expr to return const0_rtx
   where we expect a vector.  To avoid crashing, use one of the vector
   clear instructions.  */
static rtx
safe_vector_operand (rtx x, enum machine_mode mode)
{
  if (x == const0_rtx)
    x = CONST0_RTX (mode);
  return x;
}

/* Subroutine of ix86_expand_builtin to take care of SSE insns with
   4 operands. The third argument must be a constant smaller than 8
   bits or xmm0.  */

static rtx
ix86_expand_sse_4_operands_builtin (enum insn_code icode, tree exp,
				    rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[icode].operand[1].mode;
  enum machine_mode mode2 = insn_data[icode].operand[2].mode;
  enum machine_mode mode3 = insn_data[icode].operand[3].mode;

  if (VECTOR_MODE_P (mode1))
    op0 = safe_vector_operand (op0, mode1);
  if (VECTOR_MODE_P (mode2))
    op1 = safe_vector_operand (op1, mode2);
  if (VECTOR_MODE_P (mode3))
    op2 = safe_vector_operand (op2, mode3);

  if (optimize
      || target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
    op0 = copy_to_mode_reg (mode1, op0);
  if ((optimize && !register_operand (op1, mode2))
      || !(*insn_data[icode].operand[2].predicate) (op1, mode2))
    op1 = copy_to_mode_reg (mode2, op1);

  if (! (*insn_data[icode].operand[3].predicate) (op2, mode3))
    switch (icode)
      {
      case CODE_FOR_sse4_1_blendvpd:
      case CODE_FOR_sse4_1_blendvps:
      case CODE_FOR_sse4_1_pblendvb:
	op2 = copy_to_mode_reg (mode3, op2);
	break;

      case CODE_FOR_sse4_1_roundsd:
      case CODE_FOR_sse4_1_roundss:
	error ("the third argument must be a 4-bit immediate");
	return const0_rtx;

      default:
	error ("the third argument must be an 8-bit immediate");
	return const0_rtx;
      }

  pat = GEN_FCN (icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of crc32 insns.  */

static rtx
ix86_expand_crc32 (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (optimize
      || !target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
    {
      op1 = copy_to_reg (op1);
      op1 = simplify_gen_subreg (mode1, op1, GET_MODE (op1), 0);
    }

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of binop insns.  */

static rtx
ix86_expand_binop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, xops[3];
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  if (optimize || !target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (GET_MODE (op1) == SImode && mode1 == TImode)
    {
      rtx x = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_loadd (x, op1));
      op1 = gen_lowpart (TImode, x);
    }

  if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  /* ??? Using ix86_fixup_binary_operands is problematic when
     we've got mismatched modes.  Fake it.  */

  xops[0] = target;
  xops[1] = op0;
  xops[2] = op1;

  if (tmode == mode0 && tmode == mode1)
    {
      target = ix86_fixup_binary_operands (UNKNOWN, tmode, xops);
      op0 = xops[1];
      op1 = xops[2];
    }
  else if (optimize || !ix86_binary_operator_ok (UNKNOWN, tmode, xops))
    {
      op0 = force_reg (mode0, op0);
      op1 = force_reg (mode1, op1);
      target = gen_reg_rtx (tmode);
    }

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of 2-4 argument insns.  */

static rtx
ix86_expand_multi_arg_builtin (enum insn_code icode, tree exp, rtx target,
			       enum multi_arg_type m_type,
			       enum insn_code sub_code)
{
  rtx pat;
  int i;
  int nargs;
  bool comparison_p = false;
  bool tf_p = false;
  bool last_arg_constant = false;
  int num_memory = 0;
  struct {
    rtx op;
    enum machine_mode mode;
  } args[4];

  enum machine_mode tmode = insn_data[icode].operand[0].mode;

  switch (m_type)
    {
    case MULTI_ARG_3_SF:
    case MULTI_ARG_3_DF:
    case MULTI_ARG_3_DI:
    case MULTI_ARG_3_SI:
    case MULTI_ARG_3_SI_DI:
    case MULTI_ARG_3_HI:
    case MULTI_ARG_3_HI_SI:
    case MULTI_ARG_3_QI:
    case MULTI_ARG_3_PERMPS:
    case MULTI_ARG_3_PERMPD:
      nargs = 3;
      break;

    case MULTI_ARG_2_SF:
    case MULTI_ARG_2_DF:
    case MULTI_ARG_2_DI:
    case MULTI_ARG_2_SI:
    case MULTI_ARG_2_HI:
    case MULTI_ARG_2_QI:
      nargs = 2;
      break;

    case MULTI_ARG_2_DI_IMM:
    case MULTI_ARG_2_SI_IMM:
    case MULTI_ARG_2_HI_IMM:
    case MULTI_ARG_2_QI_IMM:
      nargs = 2;
      last_arg_constant = true;
      break;

    case MULTI_ARG_1_SF:
    case MULTI_ARG_1_DF:
    case MULTI_ARG_1_DI:
    case MULTI_ARG_1_SI:
    case MULTI_ARG_1_HI:
    case MULTI_ARG_1_QI:
    case MULTI_ARG_1_SI_DI:
    case MULTI_ARG_1_HI_DI:
    case MULTI_ARG_1_HI_SI:
    case MULTI_ARG_1_QI_DI:
    case MULTI_ARG_1_QI_SI:
    case MULTI_ARG_1_QI_HI:
    case MULTI_ARG_1_PH2PS:
    case MULTI_ARG_1_PS2PH:
      nargs = 1;
      break;

    case MULTI_ARG_2_SF_CMP:
    case MULTI_ARG_2_DF_CMP:
    case MULTI_ARG_2_DI_CMP:
    case MULTI_ARG_2_SI_CMP:
    case MULTI_ARG_2_HI_CMP:
    case MULTI_ARG_2_QI_CMP:
      nargs = 2;
      comparison_p = true;
      break;

    case MULTI_ARG_2_SF_TF:
    case MULTI_ARG_2_DF_TF:
    case MULTI_ARG_2_DI_TF:
    case MULTI_ARG_2_SI_TF:
    case MULTI_ARG_2_HI_TF:
    case MULTI_ARG_2_QI_TF:
      nargs = 2;
      tf_p = true;
      break;

    case MULTI_ARG_UNKNOWN:
    default:
      gcc_unreachable ();
    }

  if (optimize || !target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  gcc_assert (nargs <= 4);

  for (i = 0; i < nargs; i++)
    {
      tree arg = CALL_EXPR_ARG (exp, i);
      rtx op = expand_normal (arg);
      int adjust = (comparison_p) ? 1 : 0;
      enum machine_mode mode = insn_data[icode].operand[i+adjust+1].mode;

      if (last_arg_constant && i == nargs-1)
	{
	  if (GET_CODE (op) != CONST_INT)
	    {
	      error ("last argument must be an immediate");
	      return gen_reg_rtx (tmode);
	    }
	}
      else
	{
	  if (VECTOR_MODE_P (mode))
	    op = safe_vector_operand (op, mode);

	  /* If we aren't optimizing, only allow one memory operand to be
	     generated.  */
	  if (memory_operand (op, mode))
	    num_memory++;

	  gcc_assert (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode);

	  if (optimize
	      || ! (*insn_data[icode].operand[i+adjust+1].predicate) (op, mode)
	      || num_memory > 1)
	    op = force_reg (mode, op);
	}

      args[i].op = op;
      args[i].mode = mode;
    }

  switch (nargs)
    {
    case 1:
      pat = GEN_FCN (icode) (target, args[0].op);
      break;

    case 2:
      if (tf_p)
	pat = GEN_FCN (icode) (target, args[0].op, args[1].op,
			       GEN_INT ((int)sub_code));
      else if (! comparison_p)
	pat = GEN_FCN (icode) (target, args[0].op, args[1].op);
      else
	{
	  rtx cmp_op = gen_rtx_fmt_ee (sub_code, GET_MODE (target),
				       args[0].op,
				       args[1].op);

	  pat = GEN_FCN (icode) (target, cmp_op, args[0].op, args[1].op);
	}
      break;

    case 3:
      pat = GEN_FCN (icode) (target, args[0].op, args[1].op, args[2].op);
      break;

    default:
      gcc_unreachable ();
    }

  if (! pat)
    return 0;

  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of stores.  */

static rtx
ix86_expand_store_builtin (enum insn_code icode, tree exp)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
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
ix86_expand_unop_builtin (enum insn_code icode, tree exp,
			  rtx target, int do_load)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_normal (arg0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (optimize || !target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);
  if (do_load)
    op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  else
    {
      if (VECTOR_MODE_P (mode0))
	op0 = safe_vector_operand (op0, mode0);

      if ((optimize && !register_operand (op0, mode0))
	  || ! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
    }

  switch (icode)
    {
    case CODE_FOR_sse4_1_roundpd:
    case CODE_FOR_sse4_1_roundps:
	{
	  tree arg1 = CALL_EXPR_ARG (exp, 1);
	  rtx op1 = expand_normal (arg1);
	  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

	  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
	    {
	      error ("the second argument must be a 4-bit immediate");
	      return const0_rtx;
	    }
	  pat = GEN_FCN (icode) (target, op0, op1);
	}
      break;
    default:
      pat = GEN_FCN (icode) (target, op0);
      break;
    }

  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of three special unop insns:
   sqrtss, rsqrtss, rcpss.  */

static rtx
ix86_expand_unop1_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op1, op0 = expand_normal (arg0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (optimize || !target
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);

  if ((optimize && !register_operand (op0, mode0))
      || ! (*insn_data[icode].operand[1].predicate) (op0, mode0))
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
ix86_expand_sse_compare (const struct builtin_description *d, tree exp,
			 rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
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
  if (d->flag & BUILTIN_DESC_SWAP_OPERANDS)
    {
      rtx tmp = gen_reg_rtx (mode1);
      emit_move_insn (tmp, op1);
      op1 = op0;
      op0 = tmp;
    }

  if (optimize || !target
      || GET_MODE (target) != tmode
      || ! (*insn_data[d->icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if ((optimize && !register_operand (op0, mode0))
      || ! (*insn_data[d->icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || ! (*insn_data[d->icode].operand[2].predicate) (op1, mode1))
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
ix86_expand_sse_comi (const struct builtin_description *d, tree exp,
		      rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode mode0 = insn_data[d->icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[d->icode].operand[1].mode;
  enum rtx_code comparison = d->comparison;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  /* Swap operands if we have a comparison that isn't available in
     hardware.  */
  if (d->flag & BUILTIN_DESC_SWAP_OPERANDS)
    {
      rtx tmp = op1;
      op1 = op0;
      op0 = tmp;
    }

  target = gen_reg_rtx (SImode);
  emit_move_insn (target, const0_rtx);
  target = gen_rtx_SUBREG (QImode, target, 0);

  if ((optimize && !register_operand (op0, mode0))
      || !(*insn_data[d->icode].operand[0].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !(*insn_data[d->icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

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

/* Subroutine of ix86_expand_builtin to take care of ptest insns.  */

static rtx
ix86_expand_sse_ptest (const struct builtin_description *d, tree exp,
		       rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode mode0 = insn_data[d->icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[d->icode].operand[1].mode;
  enum rtx_code comparison = d->comparison;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  target = gen_reg_rtx (SImode);
  emit_move_insn (target, const0_rtx);
  target = gen_rtx_SUBREG (QImode, target, 0);

  if ((optimize && !register_operand (op0, mode0))
      || !(*insn_data[d->icode].operand[0].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !(*insn_data[d->icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

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

/* Subroutine of ix86_expand_builtin to take care of pcmpestr[im] insns.  */

static rtx
ix86_expand_sse_pcmpestr (const struct builtin_description *d,
			  tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  tree arg3 = CALL_EXPR_ARG (exp, 3);
  tree arg4 = CALL_EXPR_ARG (exp, 4);
  rtx scratch0, scratch1;
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx op3 = expand_normal (arg3);
  rtx op4 = expand_normal (arg4);
  enum machine_mode tmode0, tmode1, modev2, modei3, modev4, modei5, modeimm;

  tmode0 = insn_data[d->icode].operand[0].mode;
  tmode1 = insn_data[d->icode].operand[1].mode;
  modev2 = insn_data[d->icode].operand[2].mode;
  modei3 = insn_data[d->icode].operand[3].mode;
  modev4 = insn_data[d->icode].operand[4].mode;
  modei5 = insn_data[d->icode].operand[5].mode;
  modeimm = insn_data[d->icode].operand[6].mode;

  if (VECTOR_MODE_P (modev2))
    op0 = safe_vector_operand (op0, modev2);
  if (VECTOR_MODE_P (modev4))
    op2 = safe_vector_operand (op2, modev4);

  if (! (*insn_data[d->icode].operand[2].predicate) (op0, modev2))
    op0 = copy_to_mode_reg (modev2, op0);
  if (! (*insn_data[d->icode].operand[3].predicate) (op1, modei3))
    op1 = copy_to_mode_reg (modei3, op1);
  if ((optimize && !register_operand (op2, modev4))
      || !(*insn_data[d->icode].operand[4].predicate) (op2, modev4))
    op2 = copy_to_mode_reg (modev4, op2);
  if (! (*insn_data[d->icode].operand[5].predicate) (op3, modei5))
    op3 = copy_to_mode_reg (modei5, op3);

  if (! (*insn_data[d->icode].operand[6].predicate) (op4, modeimm))
    {
      error ("the fifth argument must be a 8-bit immediate");
      return const0_rtx;
    }

  if (d->code == IX86_BUILTIN_PCMPESTRI128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode0
	  || ! (*insn_data[d->icode].operand[0].predicate) (target, tmode0))
	target = gen_reg_rtx (tmode0);

      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (target, scratch1, op0, op1, op2, op3, op4);
    }
  else if (d->code == IX86_BUILTIN_PCMPESTRM128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode1
	  || ! (*insn_data[d->icode].operand[1].predicate) (target, tmode1))
	target = gen_reg_rtx (tmode1);

      scratch0 = gen_reg_rtx (tmode0);

      pat = GEN_FCN (d->icode) (scratch0, target, op0, op1, op2, op3, op4);
    }
  else
    {
      gcc_assert (d->flag);

      scratch0 = gen_reg_rtx (tmode0);
      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (scratch0, scratch1, op0, op1, op2, op3, op4);
    }

  if (! pat)
    return 0;

  emit_insn (pat);

  if (d->flag)
    {
      target = gen_reg_rtx (SImode);
      emit_move_insn (target, const0_rtx);
      target = gen_rtx_SUBREG (QImode, target, 0);

      emit_insn
	(gen_rtx_SET (VOIDmode, gen_rtx_STRICT_LOW_PART (VOIDmode, target),
		      gen_rtx_fmt_ee (EQ, QImode,
				      gen_rtx_REG ((enum machine_mode) d->flag,
						   FLAGS_REG),
				      const0_rtx)));
      return SUBREG_REG (target);
    }
  else
    return target;
}


/* Subroutine of ix86_expand_builtin to take care of pcmpistr[im] insns.  */

static rtx
ix86_expand_sse_pcmpistr (const struct builtin_description *d,
			  tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx scratch0, scratch1;
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  enum machine_mode tmode0, tmode1, modev2, modev3, modeimm;

  tmode0 = insn_data[d->icode].operand[0].mode;
  tmode1 = insn_data[d->icode].operand[1].mode;
  modev2 = insn_data[d->icode].operand[2].mode;
  modev3 = insn_data[d->icode].operand[3].mode;
  modeimm = insn_data[d->icode].operand[4].mode;

  if (VECTOR_MODE_P (modev2))
    op0 = safe_vector_operand (op0, modev2);
  if (VECTOR_MODE_P (modev3))
    op1 = safe_vector_operand (op1, modev3);

  if (! (*insn_data[d->icode].operand[2].predicate) (op0, modev2))
    op0 = copy_to_mode_reg (modev2, op0);
  if ((optimize && !register_operand (op1, modev3))
      || !(*insn_data[d->icode].operand[3].predicate) (op1, modev3))
    op1 = copy_to_mode_reg (modev3, op1);

  if (! (*insn_data[d->icode].operand[4].predicate) (op2, modeimm))
    {
      error ("the third argument must be a 8-bit immediate");
      return const0_rtx;
    }

  if (d->code == IX86_BUILTIN_PCMPISTRI128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode0
	  || ! (*insn_data[d->icode].operand[0].predicate) (target, tmode0))
	target = gen_reg_rtx (tmode0);

      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (target, scratch1, op0, op1, op2);
    }
  else if (d->code == IX86_BUILTIN_PCMPISTRM128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode1
	  || ! (*insn_data[d->icode].operand[1].predicate) (target, tmode1))
	target = gen_reg_rtx (tmode1);

      scratch0 = gen_reg_rtx (tmode0);

      pat = GEN_FCN (d->icode) (scratch0, target, op0, op1, op2);
    }
  else
    {
      gcc_assert (d->flag);

      scratch0 = gen_reg_rtx (tmode0);
      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (scratch0, scratch1, op0, op1, op2);
    }

  if (! pat)
    return 0;

  emit_insn (pat);

  if (d->flag)
    {
      target = gen_reg_rtx (SImode);
      emit_move_insn (target, const0_rtx);
      target = gen_rtx_SUBREG (QImode, target, 0);

      emit_insn
	(gen_rtx_SET (VOIDmode, gen_rtx_STRICT_LOW_PART (VOIDmode, target),
		      gen_rtx_fmt_ee (EQ, QImode,
				      gen_rtx_REG ((enum machine_mode) d->flag,
						   FLAGS_REG),
				      const0_rtx)));
      return SUBREG_REG (target);
    }
  else
    return target;
}

/* Return the integer constant in ARG.  Constrain it to be in the range
   of the subparts of VEC_TYPE; issue an error if not.  */

static int
get_element_number (tree vec_type, tree arg)
{
  unsigned HOST_WIDE_INT elt, max = TYPE_VECTOR_SUBPARTS (vec_type) - 1;

  if (!host_integerp (arg, 1)
      || (elt = tree_low_cst (arg, 1), elt > max))
    {
      error ("selector must be an integer constant in the range 0..%wi", max);
      return 0;
    }

  return elt;
}

/* A subroutine of ix86_expand_builtin.  These builtins are a wrapper around
   ix86_expand_vector_init.  We DO have language-level syntax for this, in
   the form of  (type){ init-list }.  Except that since we can't place emms
   instructions from inside the compiler, we can't allow the use of MMX
   registers unless the user explicitly asks for it.  So we do *not* define
   vec_set/vec_extract/vec_init patterns for MMX modes in mmx.md.  Instead
   we have builtins invoked by mmintrin.h that gives us license to emit
   these sorts of instructions.  */

static rtx
ix86_expand_vec_init_builtin (tree type, tree exp, rtx target)
{
  enum machine_mode tmode = TYPE_MODE (type);
  enum machine_mode inner_mode = GET_MODE_INNER (tmode);
  int i, n_elt = GET_MODE_NUNITS (tmode);
  rtvec v = rtvec_alloc (n_elt);

  gcc_assert (VECTOR_MODE_P (tmode));
  gcc_assert (call_expr_nargs (exp) == n_elt);

  for (i = 0; i < n_elt; ++i)
    {
      rtx x = expand_normal (CALL_EXPR_ARG (exp, i));
      RTVEC_ELT (v, i) = gen_lowpart (inner_mode, x);
    }

  if (!target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  ix86_expand_vector_init (true, target, gen_rtx_PARALLEL (tmode, v));
  return target;
}

/* A subroutine of ix86_expand_builtin.  These builtins are a wrapper around
   ix86_expand_vector_extract.  They would be redundant (for non-MMX) if we
   had a language-level syntax for referencing vector elements.  */

static rtx
ix86_expand_vec_ext_builtin (tree exp, rtx target)
{
  enum machine_mode tmode, mode0;
  tree arg0, arg1;
  int elt;
  rtx op0;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);

  op0 = expand_normal (arg0);
  elt = get_element_number (TREE_TYPE (arg0), arg1);

  tmode = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  mode0 = TYPE_MODE (TREE_TYPE (arg0));
  gcc_assert (VECTOR_MODE_P (mode0));

  op0 = force_reg (mode0, op0);

  if (optimize || !target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  ix86_expand_vector_extract (true, target, op0, elt);

  return target;
}

/* A subroutine of ix86_expand_builtin.  These builtins are a wrapper around
   ix86_expand_vector_set.  They would be redundant (for non-MMX) if we had
   a language-level syntax for referencing vector elements.  */

static rtx
ix86_expand_vec_set_builtin (tree exp)
{
  enum machine_mode tmode, mode1;
  tree arg0, arg1, arg2;
  int elt;
  rtx op0, op1, target;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  arg2 = CALL_EXPR_ARG (exp, 2);

  tmode = TYPE_MODE (TREE_TYPE (arg0));
  mode1 = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  gcc_assert (VECTOR_MODE_P (tmode));

  op0 = expand_expr (arg0, NULL_RTX, tmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, mode1, EXPAND_NORMAL);
  elt = get_element_number (TREE_TYPE (arg0), arg2);

  if (GET_MODE (op1) != mode1 && GET_MODE (op1) != VOIDmode)
    op1 = convert_modes (mode1, GET_MODE (op1), op1, true);

  op0 = force_reg (tmode, op0);
  op1 = force_reg (mode1, op1);

  /* OP0 is the source of these builtin functions and shouldn't be
     modified.  Create a copy, use it and return it as target.  */
  target = gen_reg_rtx (tmode);
  emit_move_insn (target, op0);
  ix86_expand_vector_set (true, target, op1, elt);

  return target;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
ix86_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
		     enum machine_mode mode ATTRIBUTE_UNUSED,
		     int ignore ATTRIBUTE_UNUSED)
{
  const struct builtin_description *d;
  size_t i;
  enum insn_code icode;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree arg0, arg1, arg2, arg3;
  rtx op0, op1, op2, op3, pat;
  enum machine_mode tmode, mode0, mode1, mode2, mode3, mode4;
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case IX86_BUILTIN_EMMS:
      emit_insn (gen_mmx_emms ());
      return 0;

    case IX86_BUILTIN_SFENCE:
      emit_insn (gen_sse_sfence ());
      return 0;

    case IX86_BUILTIN_MASKMOVQ:
    case IX86_BUILTIN_MASKMOVDQU:
      icode = (fcode == IX86_BUILTIN_MASKMOVQ
	       ? CODE_FOR_mmx_maskmovq
	       : CODE_FOR_sse2_maskmovdqu);
      /* Note the arg order is different from the operand order.  */
      arg1 = CALL_EXPR_ARG (exp, 0);
      arg2 = CALL_EXPR_ARG (exp, 1);
      arg0 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;

      op0 = force_reg (Pmode, op0);
      op0 = gen_rtx_MEM (mode1, op0);

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

    case IX86_BUILTIN_RSQRTF:
      return ix86_expand_unop1_builtin (CODE_FOR_rsqrtsf2, exp, target);

    case IX86_BUILTIN_SQRTSS:
      return ix86_expand_unop1_builtin (CODE_FOR_sse_vmsqrtv4sf2, exp, target);
    case IX86_BUILTIN_RSQRTSS:
      return ix86_expand_unop1_builtin (CODE_FOR_sse_vmrsqrtv4sf2, exp, target);
    case IX86_BUILTIN_RCPSS:
      return ix86_expand_unop1_builtin (CODE_FOR_sse_vmrcpv4sf2, exp, target);

    case IX86_BUILTIN_LOADUPS:
      return ix86_expand_unop_builtin (CODE_FOR_sse_movups, exp, target, 1);

    case IX86_BUILTIN_STOREUPS:
      return ix86_expand_store_builtin (CODE_FOR_sse_movups, exp);

    case IX86_BUILTIN_LOADHPS:
    case IX86_BUILTIN_LOADLPS:
    case IX86_BUILTIN_LOADHPD:
    case IX86_BUILTIN_LOADLPD:
      icode = (fcode == IX86_BUILTIN_LOADHPS ? CODE_FOR_sse_loadhps
	       : fcode == IX86_BUILTIN_LOADLPS ? CODE_FOR_sse_loadlps
	       : fcode == IX86_BUILTIN_LOADHPD ? CODE_FOR_sse2_loadhpd
	       : CODE_FOR_sse2_loadlpd);
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;

      op0 = force_reg (mode0, op0);
      op1 = gen_rtx_MEM (mode1, copy_to_mode_reg (Pmode, op1));
      if (optimize || target == 0
	  || GET_MODE (target) != tmode
	  || !register_operand (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_STOREHPS:
    case IX86_BUILTIN_STORELPS:
      icode = (fcode == IX86_BUILTIN_STOREHPS ? CODE_FOR_sse_storehps
	       : CODE_FOR_sse_storelps);
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
      op1 = force_reg (mode1, op1);

      pat = GEN_FCN (icode) (op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return const0_rtx;

    case IX86_BUILTIN_MOVNTPS:
      return ix86_expand_store_builtin (CODE_FOR_sse_movntv4sf, exp);
    case IX86_BUILTIN_MOVNTQ:
      return ix86_expand_store_builtin (CODE_FOR_sse_movntdi, exp);

    case IX86_BUILTIN_LDMXCSR:
      op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
      target = assign_386_stack_local (SImode, SLOT_VIRTUAL);
      emit_move_insn (target, op0);
      emit_insn (gen_sse_ldmxcsr (target));
      return 0;

    case IX86_BUILTIN_STMXCSR:
      target = assign_386_stack_local (SImode, SLOT_VIRTUAL);
      emit_insn (gen_sse_stmxcsr (target));
      return copy_to_mode_reg (SImode, target);

    case IX86_BUILTIN_SHUFPS:
    case IX86_BUILTIN_SHUFPD:
      icode = (fcode == IX86_BUILTIN_SHUFPS
	       ? CODE_FOR_sse_shufps
	       : CODE_FOR_sse2_shufpd);
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;
      mode2 = insn_data[icode].operand[3].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if ((optimize && !register_operand (op1, mode1))
	  || !(*insn_data[icode].operand[2].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
	{
	  /* @@@ better error message */
	  error ("mask must be an immediate");
	  return gen_reg_rtx (tmode);
	}
      if (optimize || target == 0
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
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
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

    case IX86_BUILTIN_PSLLW128:
    case IX86_BUILTIN_PSLLWI128:
      icode = CODE_FOR_ashlv8hi3;
      goto do_pshift;
    case IX86_BUILTIN_PSLLD128:
    case IX86_BUILTIN_PSLLDI128:
      icode = CODE_FOR_ashlv4si3;
      goto do_pshift;
    case IX86_BUILTIN_PSLLQ128:
    case IX86_BUILTIN_PSLLQI128:
      icode = CODE_FOR_ashlv2di3;
      goto do_pshift;
    case IX86_BUILTIN_PSRAW128:
    case IX86_BUILTIN_PSRAWI128:
      icode = CODE_FOR_ashrv8hi3;
      goto do_pshift;
    case IX86_BUILTIN_PSRAD128:
    case IX86_BUILTIN_PSRADI128:
      icode = CODE_FOR_ashrv4si3;
      goto do_pshift;
    case IX86_BUILTIN_PSRLW128:
    case IX86_BUILTIN_PSRLWI128:
      icode = CODE_FOR_lshrv8hi3;
      goto do_pshift;
    case IX86_BUILTIN_PSRLD128:
    case IX86_BUILTIN_PSRLDI128:
      icode = CODE_FOR_lshrv4si3;
      goto do_pshift;
    case IX86_BUILTIN_PSRLQ128:
    case IX86_BUILTIN_PSRLQI128:
      icode = CODE_FOR_lshrv2di3;

    do_pshift:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
	op0 = copy_to_reg (op0);

      if (!CONST_INT_P (op1))
	op1 = simplify_gen_subreg (SImode, op1, GET_MODE (op1), 0);

      if (! (*insn_data[icode].operand[2].predicate) (op1, SImode))
	op1 = copy_to_reg (op1);

      target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_PSLLDQI128:
    case IX86_BUILTIN_PSRLDQI128:
      icode = (fcode == IX86_BUILTIN_PSLLDQI128 ? CODE_FOR_sse2_ashlti3
	       : CODE_FOR_sse2_lshrti3);
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
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
      pat = GEN_FCN (icode) (simplify_gen_subreg (tmode, target, V2DImode, 0),
			     op0, op1);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_FEMMS:
      emit_insn (gen_mmx_femms ());
      return NULL_RTX;

    case IX86_BUILTIN_PAVGUSB:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_uavgv8qi3, exp, target);

    case IX86_BUILTIN_PF2ID:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_pf2id, exp, target, 0);

    case IX86_BUILTIN_PFACC:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_haddv2sf3, exp, target);

    case IX86_BUILTIN_PFADD:
     return ix86_expand_binop_builtin (CODE_FOR_mmx_addv2sf3, exp, target);

    case IX86_BUILTIN_PFCMPEQ:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_eqv2sf3, exp, target);

    case IX86_BUILTIN_PFCMPGE:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_gev2sf3, exp, target);

    case IX86_BUILTIN_PFCMPGT:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_gtv2sf3, exp, target);

    case IX86_BUILTIN_PFMAX:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_smaxv2sf3, exp, target);

    case IX86_BUILTIN_PFMIN:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_sminv2sf3, exp, target);

    case IX86_BUILTIN_PFMUL:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_mulv2sf3, exp, target);

    case IX86_BUILTIN_PFRCP:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_rcpv2sf2, exp, target, 0);

    case IX86_BUILTIN_PFRCPIT1:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_rcpit1v2sf3, exp, target);

    case IX86_BUILTIN_PFRCPIT2:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_rcpit2v2sf3, exp, target);

    case IX86_BUILTIN_PFRSQIT1:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_rsqit1v2sf3, exp, target);

    case IX86_BUILTIN_PFRSQRT:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_rsqrtv2sf2, exp, target, 0);

    case IX86_BUILTIN_PFSUB:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_subv2sf3, exp, target);

    case IX86_BUILTIN_PFSUBR:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_subrv2sf3, exp, target);

    case IX86_BUILTIN_PI2FD:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_floatv2si2, exp, target, 0);

    case IX86_BUILTIN_PMULHRW:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_pmulhrwv4hi3, exp, target);

    case IX86_BUILTIN_PF2IW:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_pf2iw, exp, target, 0);

    case IX86_BUILTIN_PFNACC:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_hsubv2sf3, exp, target);

    case IX86_BUILTIN_PFPNACC:
      return ix86_expand_binop_builtin (CODE_FOR_mmx_addsubv2sf3, exp, target);

    case IX86_BUILTIN_PI2FW:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_pi2fw, exp, target, 0);

    case IX86_BUILTIN_PSWAPDSI:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_pswapdv2si2, exp, target, 0);

    case IX86_BUILTIN_PSWAPDSF:
      return ix86_expand_unop_builtin (CODE_FOR_mmx_pswapdv2sf2, exp, target, 0);

    case IX86_BUILTIN_SQRTSD:
      return ix86_expand_unop1_builtin (CODE_FOR_sse2_vmsqrtv2df2, exp, target);
    case IX86_BUILTIN_LOADUPD:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_movupd, exp, target, 1);
    case IX86_BUILTIN_STOREUPD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movupd, exp);

    case IX86_BUILTIN_MFENCE:
	emit_insn (gen_sse2_mfence ());
	return 0;
    case IX86_BUILTIN_LFENCE:
	emit_insn (gen_sse2_lfence ());
	return 0;

    case IX86_BUILTIN_CLFLUSH:
	arg0 = CALL_EXPR_ARG (exp, 0);
	op0 = expand_normal (arg0);
	icode = CODE_FOR_sse2_clflush;
	if (! (*insn_data[icode].operand[0].predicate) (op0, Pmode))
	    op0 = copy_to_mode_reg (Pmode, op0);

	emit_insn (gen_sse2_clflush (op0));
	return 0;

    case IX86_BUILTIN_MOVNTPD:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movntv2df, exp);
    case IX86_BUILTIN_MOVNTDQ:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movntv2di, exp);
    case IX86_BUILTIN_MOVNTI:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movntsi, exp);

    case IX86_BUILTIN_LOADDQU:
      return ix86_expand_unop_builtin (CODE_FOR_sse2_movdqu, exp, target, 1);
    case IX86_BUILTIN_STOREDQU:
      return ix86_expand_store_builtin (CODE_FOR_sse2_movdqu, exp);

    case IX86_BUILTIN_MONITOR:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      if (!REG_P (op0))
	op0 = copy_to_mode_reg (Pmode, op0);
      if (!REG_P (op1))
	op1 = copy_to_mode_reg (SImode, op1);
      if (!REG_P (op2))
	op2 = copy_to_mode_reg (SImode, op2);
      if (!TARGET_64BIT)
	emit_insn (gen_sse3_monitor (op0, op1, op2));
      else
	emit_insn (gen_sse3_monitor64 (op0, op1, op2));
      return 0;

    case IX86_BUILTIN_MWAIT:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      if (!REG_P (op0))
	op0 = copy_to_mode_reg (SImode, op0);
      if (!REG_P (op1))
	op1 = copy_to_mode_reg (SImode, op1);
      emit_insn (gen_sse3_mwait (op0, op1));
      return 0;

    case IX86_BUILTIN_LDDQU:
      return ix86_expand_unop_builtin (CODE_FOR_sse3_lddqu, exp,
				       target, 1);

    case IX86_BUILTIN_PALIGNR:
    case IX86_BUILTIN_PALIGNR128:
      if (fcode == IX86_BUILTIN_PALIGNR)
	{
	  icode = CODE_FOR_ssse3_palignrdi;
	  mode = DImode;
	}
      else
	{
	  icode = CODE_FOR_ssse3_palignrti;
	  mode = V2DImode;
	}
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op1 = expand_expr (arg1, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      op2 = expand_expr (arg2, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;
      mode3 = insn_data[icode].operand[3].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
	{
	  op0 = copy_to_reg (op0);
	  op0 = simplify_gen_subreg (mode1, op0, GET_MODE (op0), 0);
	}
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode2))
	{
	  op1 = copy_to_reg (op1);
	  op1 = simplify_gen_subreg (mode2, op1, GET_MODE (op1), 0);
	}
      if (! (*insn_data[icode].operand[3].predicate) (op2, mode3))
	{
	  error ("shift must be an immediate");
	  return const0_rtx;
	}
      target = gen_reg_rtx (mode);
      pat = GEN_FCN (icode) (simplify_gen_subreg (tmode, target, mode, 0),
			     op0, op1, op2);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_MOVNTDQA:
      return ix86_expand_unop_builtin (CODE_FOR_sse4_1_movntdqa, exp,
				       target, 1);

    case IX86_BUILTIN_MOVNTSD:
      return ix86_expand_store_builtin (CODE_FOR_sse4a_vmmovntv2df, exp);

    case IX86_BUILTIN_MOVNTSS:
      return ix86_expand_store_builtin (CODE_FOR_sse4a_vmmovntv4sf, exp);

    case IX86_BUILTIN_INSERTQ:
    case IX86_BUILTIN_EXTRQ:
      icode = (fcode == IX86_BUILTIN_EXTRQ
               ? CODE_FOR_sse4a_extrq
               : CODE_FOR_sse4a_insertq);
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;
      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
        op0 = copy_to_mode_reg (mode1, op0);
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode2))
        op1 = copy_to_mode_reg (mode2, op1);
      if (optimize || target == 0
          || GET_MODE (target) != tmode
          || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
        target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (! pat)
        return NULL_RTX;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_EXTRQI:
      icode = CODE_FOR_sse4a_extrqi;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;
      mode3 = insn_data[icode].operand[3].mode;
      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
        op0 = copy_to_mode_reg (mode1, op0);
      if (! (*insn_data[icode].operand[2].predicate) (op1, mode2))
        {
          error ("index mask must be an immediate");
          return gen_reg_rtx (tmode);
        }
      if (! (*insn_data[icode].operand[3].predicate) (op2, mode3))
        {
          error ("length mask must be an immediate");
          return gen_reg_rtx (tmode);
        }
      if (optimize || target == 0
          || GET_MODE (target) != tmode
          || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
        target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (! pat)
        return NULL_RTX;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_INSERTQI:
      icode = CODE_FOR_sse4a_insertqi;
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      arg3 = CALL_EXPR_ARG (exp, 3);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      op3 = expand_normal (arg3);
      tmode = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;
      mode3 = insn_data[icode].operand[3].mode;
      mode4 = insn_data[icode].operand[4].mode;

      if (! (*insn_data[icode].operand[1].predicate) (op0, mode1))
        op0 = copy_to_mode_reg (mode1, op0);

      if (! (*insn_data[icode].operand[2].predicate) (op1, mode2))
        op1 = copy_to_mode_reg (mode2, op1);

      if (! (*insn_data[icode].operand[3].predicate) (op2, mode3))
        {
          error ("index mask must be an immediate");
          return gen_reg_rtx (tmode);
        }
      if (! (*insn_data[icode].operand[4].predicate) (op3, mode4))
        {
          error ("length mask must be an immediate");
          return gen_reg_rtx (tmode);
        }
      if (optimize || target == 0
          || GET_MODE (target) != tmode
          || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
        target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1, op2, op3);
      if (! pat)
        return NULL_RTX;
      emit_insn (pat);
      return target;

    case IX86_BUILTIN_VEC_INIT_V2SI:
    case IX86_BUILTIN_VEC_INIT_V4HI:
    case IX86_BUILTIN_VEC_INIT_V8QI:
      return ix86_expand_vec_init_builtin (TREE_TYPE (exp), exp, target);

    case IX86_BUILTIN_VEC_EXT_V2DF:
    case IX86_BUILTIN_VEC_EXT_V2DI:
    case IX86_BUILTIN_VEC_EXT_V4SF:
    case IX86_BUILTIN_VEC_EXT_V4SI:
    case IX86_BUILTIN_VEC_EXT_V8HI:
    case IX86_BUILTIN_VEC_EXT_V2SI:
    case IX86_BUILTIN_VEC_EXT_V4HI:
    case IX86_BUILTIN_VEC_EXT_V16QI:
      return ix86_expand_vec_ext_builtin (exp, target);

    case IX86_BUILTIN_VEC_SET_V2DI:
    case IX86_BUILTIN_VEC_SET_V4SF:
    case IX86_BUILTIN_VEC_SET_V4SI:
    case IX86_BUILTIN_VEC_SET_V8HI:
    case IX86_BUILTIN_VEC_SET_V4HI:
    case IX86_BUILTIN_VEC_SET_V16QI:
      return ix86_expand_vec_set_builtin (exp);

    case IX86_BUILTIN_INFQ:
      {
	REAL_VALUE_TYPE inf;
	rtx tmp;

	real_inf (&inf);
	tmp = CONST_DOUBLE_FROM_REAL_VALUE (inf, mode);

	tmp = validize_mem (force_const_mem (mode, tmp));

	if (target == 0)
	  target = gen_reg_rtx (mode);

	emit_move_insn (target, tmp);
	return target;
      }

    case IX86_BUILTIN_FABSQ:
      return ix86_expand_unop_builtin (CODE_FOR_abstf2, exp, target, 0);

    case IX86_BUILTIN_COPYSIGNQ:
      return ix86_expand_binop_builtin (CODE_FOR_copysigntf3, exp, target);

    default:
      break;
    }

  for (i = 0, d = bdesc_sse_3arg;
       i < ARRAY_SIZE (bdesc_sse_3arg);
       i++, d++)
    if (d->code == fcode)
      return ix86_expand_sse_4_operands_builtin (d->icode, exp,
						 target);

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      {
	/* Compares are treated specially.  */
	if (d->icode == CODE_FOR_sse_maskcmpv4sf3
	    || d->icode == CODE_FOR_sse_vmmaskcmpv4sf3
	    || d->icode == CODE_FOR_sse2_maskcmpv2df3
	    || d->icode == CODE_FOR_sse2_vmmaskcmpv2df3)
	  return ix86_expand_sse_compare (d, exp, target);

	return ix86_expand_binop_builtin (d->icode, exp, target);
      }

  for (i = 0, d = bdesc_1arg; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return ix86_expand_unop_builtin (d->icode, exp, target, 0);

  for (i = 0, d = bdesc_comi; i < ARRAY_SIZE (bdesc_comi); i++, d++)
    if (d->code == fcode)
      return ix86_expand_sse_comi (d, exp, target);

  for (i = 0, d = bdesc_ptest; i < ARRAY_SIZE (bdesc_ptest); i++, d++)
    if (d->code == fcode)
      return ix86_expand_sse_ptest (d, exp, target);

  for (i = 0, d = bdesc_crc32; i < ARRAY_SIZE (bdesc_crc32); i++, d++)
    if (d->code == fcode)
      return ix86_expand_crc32 (d->icode, exp, target);

  for (i = 0, d = bdesc_pcmpestr;
       i < ARRAY_SIZE (bdesc_pcmpestr);
       i++, d++)
    if (d->code == fcode)
      return ix86_expand_sse_pcmpestr (d, exp, target);

  for (i = 0, d = bdesc_pcmpistr;
       i < ARRAY_SIZE (bdesc_pcmpistr);
       i++, d++)
    if (d->code == fcode)
      return ix86_expand_sse_pcmpistr (d, exp, target);

  for (i = 0, d = bdesc_multi_arg; i < ARRAY_SIZE (bdesc_multi_arg); i++, d++)
    if (d->code == fcode)
      return ix86_expand_multi_arg_builtin (d->icode, exp, target,
					    (enum multi_arg_type)d->flag,
					    d->comparison);

  gcc_unreachable ();
}

/* Returns a function decl for a vectorized version of the builtin function
   with builtin function code FN and the result vector type TYPE, or NULL_TREE
   if it is not available.  */

static tree
ix86_builtin_vectorized_function (unsigned int fn, tree type_out,
				  tree type_in)
{
  enum machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  switch (fn)
    {
    case BUILT_IN_SQRT:
      if (out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return ix86_builtins[IX86_BUILTIN_SQRTPD];
      break;

    case BUILT_IN_SQRTF:
      if (out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return ix86_builtins[IX86_BUILTIN_SQRTPS_NR];
      break;

    case BUILT_IN_LRINT:
      if (out_mode == SImode && out_n == 4
	  && in_mode == DFmode && in_n == 2)
	return ix86_builtins[IX86_BUILTIN_VEC_PACK_SFIX];
      break;

    case BUILT_IN_LRINTF:
      if (out_mode == SImode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return ix86_builtins[IX86_BUILTIN_CVTPS2DQ];
      break;

    default:
      ;
    }

  /* Dispatch to a handler for a vectorization library.  */
  if (ix86_veclib_handler)
    return (*ix86_veclib_handler)(fn, type_out, type_in);

  return NULL_TREE;
}

/* Handler for an ACML-style interface to a library with vectorized
   intrinsics.  */

static tree
ix86_veclibabi_acml (enum built_in_function fn, tree type_out, tree type_in)
{
  char name[20] = "__vr.._";
  tree fntype, new_fndecl, args;
  unsigned arity;
  const char *bname;
  enum machine_mode el_mode, in_mode;
  int n, in_n;

  /* The ACML is 64bits only and suitable for unsafe math only as
     it does not correctly support parts of IEEE with the required
     precision such as denormals.  */
  if (!TARGET_64BIT
      || !flag_unsafe_math_optimizations)
    return NULL_TREE;

  el_mode = TYPE_MODE (TREE_TYPE (type_out));
  n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);
  if (el_mode != in_mode
      || n != in_n)
    return NULL_TREE;

  switch (fn)
    {
    case BUILT_IN_SIN:
    case BUILT_IN_COS:
    case BUILT_IN_EXP:
    case BUILT_IN_LOG:
    case BUILT_IN_LOG2:
    case BUILT_IN_LOG10:
      name[4] = 'd';
      name[5] = '2';
      if (el_mode != DFmode
	  || n != 2)
	return NULL_TREE;
      break;

    case BUILT_IN_SINF:
    case BUILT_IN_COSF:
    case BUILT_IN_EXPF:
    case BUILT_IN_POWF:
    case BUILT_IN_LOGF:
    case BUILT_IN_LOG2F:
    case BUILT_IN_LOG10F:
      name[4] = 's';
      name[5] = '4';
      if (el_mode != SFmode
	  || n != 4)
	return NULL_TREE;
      break;

    default:
      return NULL_TREE;
    }

  bname = IDENTIFIER_POINTER (DECL_NAME (implicit_built_in_decls[fn]));
  sprintf (name + 7, "%s", bname+10);

  arity = 0;
  for (args = DECL_ARGUMENTS (implicit_built_in_decls[fn]); args;
       args = TREE_CHAIN (args))
    arity++;

  if (arity == 1)
    fntype = build_function_type_list (type_out, type_in, NULL);
  else
    fntype = build_function_type_list (type_out, type_in, type_in, NULL);

  /* Build a function declaration for the vectorized function.  */
  new_fndecl = build_decl (FUNCTION_DECL, get_identifier (name), fntype);
  TREE_PUBLIC (new_fndecl) = 1;
  DECL_EXTERNAL (new_fndecl) = 1;
  DECL_IS_NOVOPS (new_fndecl) = 1;
  TREE_READONLY (new_fndecl) = 1;

  return new_fndecl;
}


/* Returns a decl of a function that implements conversion of the
   input vector of type TYPE, or NULL_TREE if it is not available.  */

static tree
ix86_vectorize_builtin_conversion (unsigned int code, tree type)
{
  if (TREE_CODE (type) != VECTOR_TYPE)
    return NULL_TREE;

  switch (code)
    {
    case FLOAT_EXPR:
      switch (TYPE_MODE (type))
	{
	case V4SImode:
	  return ix86_builtins[IX86_BUILTIN_CVTDQ2PS];
	default:
	  return NULL_TREE;
	}

    case FIX_TRUNC_EXPR:
      switch (TYPE_MODE (type))
	{
	case V4SFmode:
	  return ix86_builtins[IX86_BUILTIN_CVTTPS2DQ];
	default:
	  return NULL_TREE;
	}
    default:
      return NULL_TREE;

    }
}

/* Returns a code for a target-specific builtin that implements
   reciprocal of the function, or NULL_TREE if not available.  */

static tree
ix86_builtin_reciprocal (unsigned int fn, bool md_fn,
			 bool sqrt ATTRIBUTE_UNUSED)
{
  if (! (TARGET_SSE_MATH && TARGET_RECIP && !optimize_size
	 && flag_finite_math_only && !flag_trapping_math
	 && flag_unsafe_math_optimizations))
    return NULL_TREE;

  if (md_fn)
    /* Machine dependent builtins.  */
    switch (fn)
      {
	/* Vectorized version of sqrt to rsqrt conversion.  */
      case IX86_BUILTIN_SQRTPS_NR:
	return ix86_builtins[IX86_BUILTIN_RSQRTPS_NR];

      default:
	return NULL_TREE;
      }
  else
    /* Normal builtins.  */
    switch (fn)
      {
	/* Sqrt to rsqrt conversion.  */
      case BUILT_IN_SQRTF:
	return ix86_builtins[IX86_BUILTIN_RSQRTF];

      default:
	return NULL_TREE;
      }
}

/* Store OPERAND to the memory after reload is completed.  This means
   that we can't easily use assign_stack_local.  */
rtx
ix86_force_to_memory (enum machine_mode mode, rtx operand)
{
  rtx result;

  gcc_assert (reload_completed);
  if (TARGET_RED_ZONE)
    {
      result = gen_rtx_MEM (mode,
			    gen_rtx_PLUS (Pmode,
					  stack_pointer_rtx,
					  GEN_INT (-RED_ZONE_SIZE)));
      emit_move_insn (result, operand);
    }
  else if (!TARGET_RED_ZONE && TARGET_64BIT)
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
	  gcc_unreachable ();
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
	  /* Store HImodes as SImodes.  */
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
	  gcc_unreachable ();
	}
      result = gen_rtx_MEM (mode, stack_pointer_rtx);
    }
  return result;
}

/* Free operand from the memory.  */
void
ix86_free_from_memory (enum machine_mode mode)
{
  if (!TARGET_RED_ZONE)
    {
      int size;

      if (mode == DImode || TARGET_64BIT)
	size = 8;
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
ix86_preferred_reload_class (rtx x, enum reg_class regclass)
{
  enum machine_mode mode = GET_MODE (x);

  /* We're only allowed to return a subclass of CLASS.  Many of the
     following checks fail for NO_REGS, so eliminate that early.  */
  if (regclass == NO_REGS)
    return NO_REGS;

  /* All classes can load zeros.  */
  if (x == CONST0_RTX (mode))
    return regclass;

  /* Force constants into memory if we are loading a (nonzero) constant into
     an MMX or SSE register.  This is because there are no MMX/SSE instructions
     to load from a constant.  */
  if (CONSTANT_P (x)
      && (MAYBE_MMX_CLASS_P (regclass) || MAYBE_SSE_CLASS_P (regclass)))
    return NO_REGS;

  /* Prefer SSE regs only, if we can use them for math.  */
  if (TARGET_SSE_MATH && !TARGET_MIX_SSE_I387 && SSE_FLOAT_MODE_P (mode))
    return SSE_CLASS_P (regclass) ? regclass : NO_REGS;

  /* Floating-point constants need more complex checks.  */
  if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) != VOIDmode)
    {
      /* General regs can load everything.  */
      if (reg_class_subset_p (regclass, GENERAL_REGS))
        return regclass;

      /* Floats can load 0 and 1 plus some others.  Note that we eliminated
	 zero above.  We only want to wind up preferring 80387 registers if
	 we plan on doing computation with them.  */
      if (TARGET_80387
	  && standard_80387_constant_p (x))
	{
	  /* Limit class to non-sse.  */
	  if (regclass == FLOAT_SSE_REGS)
	    return FLOAT_REGS;
	  if (regclass == FP_TOP_SSE_REGS)
	    return FP_TOP_REG;
	  if (regclass == FP_SECOND_SSE_REGS)
	    return FP_SECOND_REG;
	  if (regclass == FLOAT_INT_REGS || regclass == FLOAT_REGS)
	    return regclass;
	}

      return NO_REGS;
    }

  /* Generally when we see PLUS here, it's the function invariant
     (plus soft-fp const_int).  Which can only be computed into general
     regs.  */
  if (GET_CODE (x) == PLUS)
    return reg_class_subset_p (regclass, GENERAL_REGS) ? regclass : NO_REGS;

  /* QImode constants are easy to load, but non-constant QImode data
     must go into Q_REGS.  */
  if (GET_MODE (x) == QImode && !CONSTANT_P (x))
    {
      if (reg_class_subset_p (regclass, Q_REGS))
	return regclass;
      if (reg_class_subset_p (Q_REGS, regclass))
	return Q_REGS;
      return NO_REGS;
    }

  return regclass;
}

/* Discourage putting floating-point values in SSE registers unless
   SSE math is being used, and likewise for the 387 registers.  */
enum reg_class
ix86_preferred_output_reload_class (rtx x, enum reg_class regclass)
{
  enum machine_mode mode = GET_MODE (x);

  /* Restrict the output reload class to the register bank that we are doing
     math on.  If we would like not to return a subset of CLASS, reject this
     alternative: if reload cannot do this, it will still use its choice.  */
  mode = GET_MODE (x);
  if (TARGET_SSE_MATH && SSE_FLOAT_MODE_P (mode))
    return MAYBE_SSE_CLASS_P (regclass) ? SSE_REGS : NO_REGS;

  if (X87_FLOAT_MODE_P (mode))
    {
      if (regclass == FP_TOP_SSE_REGS)
	return FP_TOP_REG;
      else if (regclass == FP_SECOND_SSE_REGS)
	return FP_SECOND_REG;
      else
	return FLOAT_CLASS_P (regclass) ? regclass : NO_REGS;
    }

  return regclass;
}

/* If we are copying between general and FP registers, we need a memory
   location. The same is true for SSE and MMX registers.

   To optimize register_move_cost performance, allow inline variant.

   The macro can't work reliably when one of the CLASSES is class containing
   registers from multiple units (SSE, MMX, integer).  We avoid this by never
   combining those units in single alternative in the machine description.
   Ensure that this constraint holds to avoid unexpected surprises.

   When STRICT is false, we are being called from REGISTER_MOVE_COST, so do not
   enforce these sanity checks.  */

static inline int
inline_secondary_memory_needed (enum reg_class class1, enum reg_class class2,
			      enum machine_mode mode, int strict)
{
  if (MAYBE_FLOAT_CLASS_P (class1) != FLOAT_CLASS_P (class1)
      || MAYBE_FLOAT_CLASS_P (class2) != FLOAT_CLASS_P (class2)
      || MAYBE_SSE_CLASS_P (class1) != SSE_CLASS_P (class1)
      || MAYBE_SSE_CLASS_P (class2) != SSE_CLASS_P (class2)
      || MAYBE_MMX_CLASS_P (class1) != MMX_CLASS_P (class1)
      || MAYBE_MMX_CLASS_P (class2) != MMX_CLASS_P (class2))
    {
      gcc_assert (!strict);
      return true;
    }

  if (FLOAT_CLASS_P (class1) != FLOAT_CLASS_P (class2))
    return true;

  /* ??? This is a lie.  We do have moves between mmx/general, and for
     mmx/sse2.  But by saying we need secondary memory we discourage the
     register allocator from using the mmx registers unless needed.  */
  if (MMX_CLASS_P (class1) != MMX_CLASS_P (class2))
    return true;

  if (SSE_CLASS_P (class1) != SSE_CLASS_P (class2))
    {
      /* SSE1 doesn't have any direct moves from other classes.  */
      if (!TARGET_SSE2)
	return true;

      /* If the target says that inter-unit moves are more expensive
	 than moving through memory, then don't generate them.  */
      if (!TARGET_INTER_UNIT_MOVES)
	return true;

      /* Between SSE and general, we have moves no larger than word size.  */
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return true;
    }

  return false;
}

int
ix86_secondary_memory_needed (enum reg_class class1, enum reg_class class2,
			      enum machine_mode mode, int strict)
{
  return inline_secondary_memory_needed (class1, class2, mode, strict);
}

/* Return true if the registers in CLASS cannot represent the change from
   modes FROM to TO.  */

bool
ix86_cannot_change_mode_class (enum machine_mode from, enum machine_mode to,
			       enum reg_class regclass)
{
  if (from == to)
    return false;

  /* x87 registers can't do subreg at all, as all values are reformatted
     to extended precision.  */
  if (MAYBE_FLOAT_CLASS_P (regclass))
    return true;

  if (MAYBE_SSE_CLASS_P (regclass) || MAYBE_MMX_CLASS_P (regclass))
    {
      /* Vector registers do not support QI or HImode loads.  If we don't
	 disallow a change to these modes, reload will assume it's ok to
	 drop the subreg from (subreg:SI (reg:HI 100) 0).  This affects
	 the vec_dupv4hi pattern.  */
      if (GET_MODE_SIZE (from) < 4)
	return true;

      /* Vector registers do not support subreg with nonzero offsets, which
	 are otherwise valid for integer registers.  Since we can't see
	 whether we have a nonzero offset from here, prohibit all
         nonparadoxical subregs changing size.  */
      if (GET_MODE_SIZE (to) < GET_MODE_SIZE (from))
	return true;
    }

  return false;
}

/* Return the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   This function is used extensively by register_move_cost that is used to
   build tables at startup.  Make it inline in this case.
   When IN is 2, return maximum of in and out move cost.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.

   Model also increased moving costs of QImode registers in non
   Q_REGS classes.
 */
static inline int
inline_memory_move_cost (enum machine_mode mode, enum reg_class regclass,
			 int in)
{
  int cost;
  if (FLOAT_CLASS_P (regclass))
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
	    index = 2;
	    break;
	  default:
	    return 100;
	}
      if (in == 2)
        return MAX (ix86_cost->fp_load [index], ix86_cost->fp_store [index]);
      return in ? ix86_cost->fp_load [index] : ix86_cost->fp_store [index];
    }
  if (SSE_CLASS_P (regclass))
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
      if (in == 2)
        return MAX (ix86_cost->sse_load [index], ix86_cost->sse_store [index]);
      return in ? ix86_cost->sse_load [index] : ix86_cost->sse_store [index];
    }
  if (MMX_CLASS_P (regclass))
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
      if (in)
        return MAX (ix86_cost->mmx_load [index], ix86_cost->mmx_store [index]);
      return in ? ix86_cost->mmx_load [index] : ix86_cost->mmx_store [index];
    }
  switch (GET_MODE_SIZE (mode))
    {
      case 1:
	if (Q_CLASS_P (regclass) || TARGET_64BIT)
	  {
	    if (!in)
	      return ix86_cost->int_store[0];
	    if (TARGET_PARTIAL_REG_DEPENDENCY && !optimize_size)
	      cost = ix86_cost->movzbl_load;
	    else
	      cost = ix86_cost->int_load[0];
	    if (in == 2)
	      return MAX (cost, ix86_cost->int_store[0]);
	    return cost;
	  }
	else
	  {
	   if (in == 2)
	     return MAX (ix86_cost->movzbl_load, ix86_cost->int_store[0] + 4);
	   if (in)
	     return ix86_cost->movzbl_load;
	   else
	     return ix86_cost->int_store[0] + 4;
	  }
	break;
      case 2:
	if (in == 2)
	  return MAX (ix86_cost->int_load[1], ix86_cost->int_store[1]);
	return in ? ix86_cost->int_load[1] : ix86_cost->int_store[1];
      default:
	/* Compute number of 32bit moves needed.  TFmode is moved as XFmode.  */
	if (mode == TFmode)
	  mode = XFmode;
	if (in == 2)
	  cost = MAX (ix86_cost->int_load[2] , ix86_cost->int_store[2]);
	else if (in)
	  cost = ix86_cost->int_load[2];
	else
	  cost = ix86_cost->int_store[2];
	return (cost * (((int) GET_MODE_SIZE (mode)
		        + UNITS_PER_WORD - 1) / UNITS_PER_WORD));
    }
}

int
ix86_memory_move_cost (enum machine_mode mode, enum reg_class regclass, int in)
{
  return inline_memory_move_cost (mode, regclass, in);
}


/* Return the cost of moving data from a register in class CLASS1 to
   one in class CLASS2.

   It is not required that the cost always equal 2 when FROM is the same as TO;
   on some machines it is expensive to move between registers if they are not
   general registers.  */

int
ix86_register_move_cost (enum machine_mode mode, enum reg_class class1,
			 enum reg_class class2)
{
  /* In case we require secondary memory, compute cost of the store followed
     by load.  In order to avoid bad register allocation choices, we need
     for this to be *at least* as high as the symmetric MEMORY_MOVE_COST.  */

  if (inline_secondary_memory_needed (class1, class2, mode, 0))
    {
      int cost = 1;

      cost += inline_memory_move_cost (mode, class1, 2);
      cost += inline_memory_move_cost (mode, class2, 2);

      /* In case of copying from general_purpose_register we may emit multiple
         stores followed by single load causing memory size mismatch stall.
         Count this as arbitrarily high cost of 20.  */
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

    /* ??? By keeping returned value relatively high, we limit the number
       of moves between integer and MMX/SSE registers for all targets.
       Additionally, high value prevents problem with x86_modes_tieable_p(),
       where integer modes in MMX/SSE registers are not tieable
       because of missing QImode and HImode moves to, from or between
       MMX/SSE registers.  */
    return MAX (8, ix86_cost->mmxsse_to_integer);

  if (MAYBE_FLOAT_CLASS_P (class1))
    return ix86_cost->fp_move;
  if (MAYBE_SSE_CLASS_P (class1))
    return ix86_cost->sse_move;
  if (MAYBE_MMX_CLASS_P (class1))
    return ix86_cost->mmx_move;
  return 2;
}

/* Return 1 if hard register REGNO can hold a value of machine-mode MODE.  */

bool
ix86_hard_regno_mode_ok (int regno, enum machine_mode mode)
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
    {
      /* We implement the move patterns for all vector modes into and
	 out of SSE registers, even when no operation instructions
	 are available.  */
      return (VALID_SSE_REG_MODE (mode)
	      || VALID_SSE2_REG_MODE (mode)
	      || VALID_MMX_REG_MODE (mode)
	      || VALID_MMX_REG_MODE_3DNOW (mode));
    }
  if (MMX_REGNO_P (regno))
    {
      /* We implement the move patterns for 3DNOW modes even in MMX mode,
	 so if the register is available at all, then we can move data of
	 the given mode into or out of it.  */
      return (VALID_MMX_REG_MODE (mode)
	      || VALID_MMX_REG_MODE_3DNOW (mode));
    }

  if (mode == QImode)
    {
      /* Take care for QImode values - they can be in non-QI regs,
	 but then they do cause partial register stalls.  */
      if (regno < 4 || TARGET_64BIT)
	return 1;
      if (!TARGET_PARTIAL_REG_STALL)
	return 1;
      return reload_in_progress || reload_completed;
    }
  /* We handle both integer and floats in the general purpose registers.  */
  else if (VALID_INT_MODE_P (mode))
    return 1;
  else if (VALID_FP_MODE_P (mode))
    return 1;
  else if (VALID_DFP_MODE_P (mode))
    return 1;
  /* Lots of MMX code casts 8 byte vector modes to DImode.  If we then go
     on to use that value in smaller contexts, this can easily force a
     pseudo to be allocated to GENERAL_REGS.  Since this is no worse than
     supporting DImode, allow it.  */
  else if (VALID_MMX_REG_MODE_3DNOW (mode) || VALID_MMX_REG_MODE (mode))
    return 1;

  return 0;
}

/* A subroutine of ix86_modes_tieable_p.  Return true if MODE is a
   tieable integer mode.  */

static bool
ix86_tieable_integer_mode_p (enum machine_mode mode)
{
  switch (mode)
    {
    case HImode:
    case SImode:
      return true;

    case QImode:
      return TARGET_64BIT || !TARGET_PARTIAL_REG_STALL;

    case DImode:
      return TARGET_64BIT;

    default:
      return false;
    }
}

/* Return true if MODE1 is accessible in a register that can hold MODE2
   without copying.  That is, all register classes that can hold MODE2
   can also hold MODE1.  */

bool
ix86_modes_tieable_p (enum machine_mode mode1, enum machine_mode mode2)
{
  if (mode1 == mode2)
    return true;

  if (ix86_tieable_integer_mode_p (mode1)
      && ix86_tieable_integer_mode_p (mode2))
    return true;

  /* MODE2 being XFmode implies fp stack or general regs, which means we
     can tie any smaller floating point modes to it.  Note that we do not
     tie this with TFmode.  */
  if (mode2 == XFmode)
    return mode1 == SFmode || mode1 == DFmode;

  /* MODE2 being DFmode implies fp stack, general or sse regs, which means
     that we can tie it with SFmode.  */
  if (mode2 == DFmode)
    return mode1 == SFmode;

  /* If MODE2 is only appropriate for an SSE register, then tie with
     any other mode acceptable to SSE registers.  */
  if (GET_MODE_SIZE (mode2) == 16
      && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode2))
    return (GET_MODE_SIZE (mode1) == 16
	    && ix86_hard_regno_mode_ok (FIRST_SSE_REG, mode1));

  /* If MODE2 is appropriate for an MMX register, then tie
     with any other mode acceptable to MMX registers.  */
  if (GET_MODE_SIZE (mode2) == 8
      && ix86_hard_regno_mode_ok (FIRST_MMX_REG, mode2))
    return (GET_MODE_SIZE (mode1) == 8
	    && ix86_hard_regno_mode_ok (FIRST_MMX_REG, mode1));

  return false;
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
ix86_rtx_costs (rtx x, int code, int outer_code_i, int *total)
{
  enum rtx_code outer_code = (enum rtx_code) outer_code_i;
  enum machine_mode mode = GET_MODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      if (TARGET_64BIT && !x86_64_immediate_operand (x, VOIDmode))
	*total = 3;
      else if (TARGET_64BIT && !x86_64_zext_immediate_operand (x, VOIDmode))
	*total = 2;
      else if (flag_pic && SYMBOLIC_CONST (x)
	       && (!TARGET_64BIT
		   || (!GET_CODE (x) != LABEL_REF
		       && (GET_CODE (x) != SYMBOL_REF
		           || !SYMBOL_REF_LOCAL_P (x)))))
	*total = 1;
      else
	*total = 0;
      return true;

    case CONST_DOUBLE:
      if (mode == VOIDmode)
	*total = 0;
      else
	switch (standard_80387_constant_p (x))
	  {
	  case 1: /* 0.0 */
	    *total = 1;
	    break;
	  default: /* Other constants */
	    *total = 2;
	    break;
	  case 0:
	  case -1:
	    /* Start with (MEM (SYMBOL_REF)), since that's where
	       it'll probably end up.  Add a penalty for size.  */
	    *total = (COSTS_N_INSNS (1)
		      + (flag_pic != 0 && !TARGET_64BIT)
		      + (mode == SFmode ? 0 : mode == DFmode ? 1 : 2));
	    break;
	  }
      return true;

    case ZERO_EXTEND:
      /* The zero extensions is often completely free on x86_64, so make
	 it as cheap as possible.  */
      if (TARGET_64BIT && mode == DImode
	  && GET_MODE (XEXP (x, 0)) == SImode)
	*total = 1;
      else if (TARGET_ZERO_EXTEND_WITH_AND)
	*total = ix86_cost->add;
      else
	*total = ix86_cost->movzx;
      return false;

    case SIGN_EXTEND:
      *total = ix86_cost->movsx;
      return false;

    case ASHIFT:
      if (CONST_INT_P (XEXP (x, 1))
	  && (GET_MODE (XEXP (x, 0)) != DImode || TARGET_64BIT))
	{
	  HOST_WIDE_INT value = INTVAL (XEXP (x, 1));
	  if (value == 1)
	    {
	      *total = ix86_cost->add;
	      return false;
	    }
	  if ((value == 2 || value == 3)
	      && ix86_cost->lea <= ix86_cost->shift_const)
	    {
	      *total = ix86_cost->lea;
	      return false;
	    }
	}
      /* FALLTHRU */

    case ROTATE:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATERT:
      if (!TARGET_64BIT && GET_MODE (XEXP (x, 0)) == DImode)
	{
	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      if (INTVAL (XEXP (x, 1)) > 32)
		*total = ix86_cost->shift_const + COSTS_N_INSNS (2);
	      else
		*total = ix86_cost->shift_const * 2;
	    }
	  else
	    {
	      if (GET_CODE (XEXP (x, 1)) == AND)
		*total = ix86_cost->shift_var * 2;
	      else
		*total = ix86_cost->shift_var * 6 + COSTS_N_INSNS (2);
	    }
	}
      else
	{
	  if (CONST_INT_P (XEXP (x, 1)))
	    *total = ix86_cost->shift_const;
	  else
	    *total = ix86_cost->shift_var;
	}
      return false;

    case MULT:
      if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
	{
	  /* ??? SSE scalar cost should be used here.  */
	  *total = ix86_cost->fmul;
	  return false;
	}
      else if (X87_FLOAT_MODE_P (mode))
	{
	  *total = ix86_cost->fmul;
	  return false;
	}
      else if (FLOAT_MODE_P (mode))
	{
	  /* ??? SSE vector cost should be used here.  */
	  *total = ix86_cost->fmul;
	  return false;
	}
      else
	{
	  rtx op0 = XEXP (x, 0);
	  rtx op1 = XEXP (x, 1);
	  int nbits;
	  if (CONST_INT_P (XEXP (x, 1)))
	    {
	      unsigned HOST_WIDE_INT value = INTVAL (XEXP (x, 1));
	      for (nbits = 0; value != 0; value &= value - 1)
	        nbits++;
	    }
	  else
	    /* This is arbitrary.  */
	    nbits = 7;

	  /* Compute costs correctly for widening multiplication.  */
	  if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	      && GET_MODE_SIZE (GET_MODE (XEXP (op0, 0))) * 2
	         == GET_MODE_SIZE (mode))
	    {
	      int is_mulwiden = 0;
	      enum machine_mode inner_mode = GET_MODE (op0);

	      if (GET_CODE (op0) == GET_CODE (op1))
		is_mulwiden = 1, op1 = XEXP (op1, 0);
	      else if (CONST_INT_P (op1))
		{
		  if (GET_CODE (op0) == SIGN_EXTEND)
		    is_mulwiden = trunc_int_for_mode (INTVAL (op1), inner_mode)
			          == INTVAL (op1);
		  else
		    is_mulwiden = !(INTVAL (op1) & ~GET_MODE_MASK (inner_mode));
	        }

	      if (is_mulwiden)
	        op0 = XEXP (op0, 0), mode = GET_MODE (op0);
	    }

  	  *total = (ix86_cost->mult_init[MODE_INDEX (mode)]
		    + nbits * ix86_cost->mult_bit
	            + rtx_cost (op0, outer_code) + rtx_cost (op1, outer_code));

          return true;
	}

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
	/* ??? SSE cost should be used here.  */
	*total = ix86_cost->fdiv;
      else if (X87_FLOAT_MODE_P (mode))
	*total = ix86_cost->fdiv;
      else if (FLOAT_MODE_P (mode))
	/* ??? SSE vector cost should be used here.  */
	*total = ix86_cost->fdiv;
      else
	*total = ix86_cost->divide[MODE_INDEX (mode)];
      return false;

    case PLUS:
      if (GET_MODE_CLASS (mode) == MODE_INT
	       && GET_MODE_BITSIZE (mode) <= GET_MODE_BITSIZE (Pmode))
	{
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	      && CONST_INT_P (XEXP (XEXP (XEXP (x, 0), 0), 1))
	      && CONSTANT_P (XEXP (x, 1)))
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1));
	      if (val == 2 || val == 4 || val == 8)
		{
		  *total = ix86_cost->lea;
		  *total += rtx_cost (XEXP (XEXP (x, 0), 1), outer_code);
		  *total += rtx_cost (XEXP (XEXP (XEXP (x, 0), 0), 0),
				      outer_code);
		  *total += rtx_cost (XEXP (x, 1), outer_code);
		  return true;
		}
	    }
	  else if (GET_CODE (XEXP (x, 0)) == MULT
		   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (XEXP (x, 0), 1));
	      if (val == 2 || val == 4 || val == 8)
		{
		  *total = ix86_cost->lea;
		  *total += rtx_cost (XEXP (XEXP (x, 0), 0), outer_code);
		  *total += rtx_cost (XEXP (x, 1), outer_code);
		  return true;
		}
	    }
	  else if (GET_CODE (XEXP (x, 0)) == PLUS)
	    {
	      *total = ix86_cost->lea;
	      *total += rtx_cost (XEXP (XEXP (x, 0), 0), outer_code);
	      *total += rtx_cost (XEXP (XEXP (x, 0), 1), outer_code);
	      *total += rtx_cost (XEXP (x, 1), outer_code);
	      return true;
	    }
	}
      /* FALLTHRU */

    case MINUS:
      if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
	{
	  /* ??? SSE cost should be used here.  */
	  *total = ix86_cost->fadd;
	  return false;
	}
      else if (X87_FLOAT_MODE_P (mode))
	{
	  *total = ix86_cost->fadd;
	  return false;
	}
      else if (FLOAT_MODE_P (mode))
	{
	  /* ??? SSE vector cost should be used here.  */
	  *total = ix86_cost->fadd;
	  return false;
	}
      /* FALLTHRU */

    case AND:
    case IOR:
    case XOR:
      if (!TARGET_64BIT && mode == DImode)
	{
	  *total = (ix86_cost->add * 2
		    + (rtx_cost (XEXP (x, 0), outer_code)
		       << (GET_MODE (XEXP (x, 0)) != DImode))
		    + (rtx_cost (XEXP (x, 1), outer_code)
	               << (GET_MODE (XEXP (x, 1)) != DImode)));
	  return true;
	}
      /* FALLTHRU */

    case NEG:
      if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
	{
	  /* ??? SSE cost should be used here.  */
	  *total = ix86_cost->fchs;
	  return false;
	}
      else if (X87_FLOAT_MODE_P (mode))
	{
	  *total = ix86_cost->fchs;
	  return false;
	}
      else if (FLOAT_MODE_P (mode))
	{
	  /* ??? SSE vector cost should be used here.  */
	  *total = ix86_cost->fchs;
	  return false;
	}
      /* FALLTHRU */

    case NOT:
      if (!TARGET_64BIT && mode == DImode)
	*total = ix86_cost->add * 2;
      else
	*total = ix86_cost->add;
      return false;

    case COMPARE:
      if (GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT
	  && XEXP (XEXP (x, 0), 1) == const1_rtx
	  && CONST_INT_P (XEXP (XEXP (x, 0), 2))
	  && XEXP (x, 1) == const0_rtx)
	{
	  /* This kind of construct is implemented using test[bwl].
	     Treat it as if we had an AND.  */
	  *total = (ix86_cost->add
		    + rtx_cost (XEXP (XEXP (x, 0), 0), outer_code)
		    + rtx_cost (const1_rtx, outer_code));
	  return true;
	}
      return false;

    case FLOAT_EXTEND:
      if (!(SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH))
	*total = 0;
      return false;

    case ABS:
      if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
	/* ??? SSE cost should be used here.  */
	*total = ix86_cost->fabs;
      else if (X87_FLOAT_MODE_P (mode))
	*total = ix86_cost->fabs;
      else if (FLOAT_MODE_P (mode))
	/* ??? SSE vector cost should be used here.  */
	*total = ix86_cost->fabs;
      return false;

    case SQRT:
      if (SSE_FLOAT_MODE_P (mode) && TARGET_SSE_MATH)
	/* ??? SSE cost should be used here.  */
	*total = ix86_cost->fsqrt;
      else if (X87_FLOAT_MODE_P (mode))
	*total = ix86_cost->fsqrt;
      else if (FLOAT_MODE_P (mode))
	/* ??? SSE vector cost should be used here.  */
	*total = ix86_cost->fsqrt;
      return false;

    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_TP)
	*total = 0;
      return false;

    default:
      return false;
    }
}

#if TARGET_MACHO

static int current_machopic_label_num;

/* Given a symbol name and its associated stub, write out the
   definition of the stub.  */

void
machopic_output_stub (FILE *file, const char *symb, const char *stub)
{
  unsigned int length;
  char *binder_name, *symbol_name, lazy_ptr_name[32];
  int label = ++current_machopic_label_num;

  /* For 64-bit we shouldn't get here.  */
  gcc_assert (!TARGET_64BIT);

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
    switch_to_section (darwin_sections[machopic_picsymbol_stub_section]);
  else
    switch_to_section (darwin_sections[machopic_symbol_stub_section]);

  fprintf (file, "%s:\n", stub);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

  if (MACHOPIC_PURE)
    {
      fprintf (file, "\tcall\tLPC$%d\nLPC$%d:\tpopl\t%%eax\n", label, label);
      fprintf (file, "\tmovl\t%s-LPC$%d(%%eax),%%edx\n", lazy_ptr_name, label);
      fprintf (file, "\tjmp\t*%%edx\n");
    }
  else
    fprintf (file, "\tjmp\t*%s\n", lazy_ptr_name);

  fprintf (file, "%s:\n", binder_name);

  if (MACHOPIC_PURE)
    {
      fprintf (file, "\tlea\t%s-LPC$%d(%%eax),%%eax\n", lazy_ptr_name, label);
      fprintf (file, "\tpushl\t%%eax\n");
    }
  else
    fprintf (file, "\tpushl\t$%s\n", lazy_ptr_name);

  fprintf (file, "\tjmp\tdyld_stub_binding_helper\n");

  switch_to_section (darwin_sections[machopic_lazy_symbol_ptr_section]);
  fprintf (file, "%s:\n", lazy_ptr_name);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);
  fprintf (file, "\t.long %s\n", binder_name);
}

void
darwin_x86_file_end (void)
{
  darwin_file_end ();
  ix86_file_end ();
}
#endif /* TARGET_MACHO */

/* Order the registers for register allocator.  */

void
x86_order_regs_for_local_alloc (void)
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

   /* x87 registers.  */
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

/* Handle a "ms_struct" or "gcc_struct" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
ix86_handle_struct_attribute (tree *node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  tree *type = NULL;
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
	type = &TREE_TYPE (*node);
    }
  else
    type = node;

  if (!(type && (TREE_CODE (*type) == RECORD_TYPE
		 || TREE_CODE (*type) == UNION_TYPE)))
    {
      warning (OPT_Wattributes, "%qs attribute ignored",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  else if ((is_attribute_p ("ms_struct", name)
	    && lookup_attribute ("gcc_struct", TYPE_ATTRIBUTES (*type)))
	   || ((is_attribute_p ("gcc_struct", name)
		&& lookup_attribute ("ms_struct", TYPE_ATTRIBUTES (*type)))))
    {
      warning (OPT_Wattributes, "%qs incompatible attribute ignored",
               IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static bool
ix86_ms_bitfield_layout_p (const_tree record_type)
{
  return (TARGET_MS_BITFIELD_LAYOUT &&
	  !lookup_attribute ("gcc_struct", TYPE_ATTRIBUTES (record_type)))
    || lookup_attribute ("ms_struct", TYPE_ATTRIBUTES (record_type));
}

/* Returns an expression indicating where the this parameter is
   located on entry to the FUNCTION.  */

static rtx
x86_this_parameter (tree function)
{
  tree type = TREE_TYPE (function);
  bool aggr = aggregate_value_p (TREE_TYPE (type), type) != 0;
  int nregs;

  if (TARGET_64BIT)
    {
      const int *parm_regs;

      if (TARGET_64BIT_MS_ABI)
        parm_regs = x86_64_ms_abi_int_parameter_registers;
      else
        parm_regs = x86_64_int_parameter_registers;
      return gen_rtx_REG (DImode, parm_regs[aggr]);
    }

  nregs = ix86_function_regparm (type, function);

  if (nregs > 0 && !stdarg_p (type))
    {
      int regno;

      if (lookup_attribute ("fastcall", TYPE_ATTRIBUTES (type)))
	regno = aggr ? DX_REG : CX_REG;
      else
        {
	  regno = AX_REG;
	  if (aggr)
	    {
	      regno = DX_REG;
	      if (nregs == 1)
		return gen_rtx_MEM (SImode,
				    plus_constant (stack_pointer_rtx, 4));
	    }
	}
      return gen_rtx_REG (SImode, regno);
    }

  return gen_rtx_MEM (SImode, plus_constant (stack_pointer_rtx, aggr ? 8 : 4));
}

/* Determine whether x86_output_mi_thunk can succeed.  */

static bool
x86_can_output_mi_thunk (const_tree thunk ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT delta ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT vcall_offset, const_tree function)
{
  /* 64-bit can handle anything.  */
  if (TARGET_64BIT)
    return true;

  /* For 32-bit, everything's fine if we have one free register.  */
  if (ix86_function_regparm (TREE_TYPE (function), function) < 3)
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
   added to THIS.  If VCALL_OFFSET is nonzero, the word at
   *(*this + vcall_offset) should be added to THIS.  */

static void
x86_output_mi_thunk (FILE *file ATTRIBUTE_UNUSED,
		     tree thunk ATTRIBUTE_UNUSED, HOST_WIDE_INT delta,
		     HOST_WIDE_INT vcall_offset, tree function)
{
  rtx xops[3];
  rtx this_param = x86_this_parameter (function);
  rtx this_reg, tmp;

  /* If VCALL_OFFSET, we'll need THIS in a register.  Might as well
     pull it in now and let DELTA benefit.  */
  if (REG_P (this_param))
    this_reg = this_param;
  else if (vcall_offset)
    {
      /* Put the this parameter into %eax.  */
      xops[0] = this_param;
      xops[1] = this_reg = gen_rtx_REG (Pmode, AX_REG);
      output_asm_insn ("mov{l}\t{%0, %1|%1, %0}", xops);
    }
  else
    this_reg = NULL_RTX;

  /* Adjust the this parameter by a fixed constant.  */
  if (delta)
    {
      xops[0] = GEN_INT (delta);
      xops[1] = this_reg ? this_reg : this_param;
      if (TARGET_64BIT)
	{
	  if (!x86_64_general_operand (xops[0], DImode))
	    {
	      tmp = gen_rtx_REG (DImode, R10_REG);
	      xops[1] = tmp;
	      output_asm_insn ("mov{q}\t{%1, %0|%0, %1}", xops);
	      xops[0] = tmp;
	      xops[1] = this_param;
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
	tmp = gen_rtx_REG (DImode, R10_REG);
      else
	{
	  int tmp_regno = CX_REG;
	  if (lookup_attribute ("fastcall",
				TYPE_ATTRIBUTES (TREE_TYPE (function))))
	    tmp_regno = AX_REG;
	  tmp = gen_rtx_REG (SImode, tmp_regno);
	}

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
	  rtx tmp2 = gen_rtx_REG (DImode, R11_REG);
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
  if (this_reg && this_reg != this_param)
    {
      xops[0] = this_reg;
      xops[1] = this_param;
      output_asm_insn ("mov{l}\t{%0, %1|%1, %0}", xops);
    }

  xops[0] = XEXP (DECL_RTL (function), 0);
  if (TARGET_64BIT)
    {
      if (!flag_pic || (*targetm.binds_local_p) (function))
	output_asm_insn ("jmp\t%P0", xops);
      /* All thunks should be in the same object as their target,
	 and thus binds_local_p should be true.  */
      else if (TARGET_64BIT_MS_ABI)
	gcc_unreachable ();
      else
	{
	  tmp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, xops[0]), UNSPEC_GOTPCREL);
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
#if TARGET_MACHO
	if (TARGET_MACHO)
	  {
	    rtx sym_ref = XEXP (DECL_RTL (function), 0);
	    tmp = (gen_rtx_SYMBOL_REF
		   (Pmode,
		    machopic_indirection_name (sym_ref, /*stub_p=*/true)));
	    tmp = gen_rtx_MEM (QImode, tmp);
	    xops[0] = tmp;
	    output_asm_insn ("jmp\t%0", xops);
	  }
	else
#endif /* TARGET_MACHO */
	{
	  tmp = gen_rtx_REG (SImode, CX_REG);
	  output_set_got (tmp, NULL_RTX);

	  xops[1] = tmp;
	  output_asm_insn ("mov{l}\t{%0@GOT(%1), %1|%1, %0@GOT[%1]}", xops);
	  output_asm_insn ("jmp\t{*}%1", xops);
	}
    }
}

static void
x86_file_start (void)
{
  default_file_start ();
#if TARGET_MACHO
  darwin_file_start ();
#endif
  if (X86_FILE_START_VERSION_DIRECTIVE)
    fputs ("\t.version\t\"01.01\"\n", asm_out_file);
  if (X86_FILE_START_FLTUSED)
    fputs ("\t.global\t__fltused\n", asm_out_file);
  if (ix86_asm_dialect == ASM_INTEL)
    fputs ("\t.intel_syntax noprefix\n", asm_out_file);
}

int
x86_field_alignment (tree field, int computed)
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
x86_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  if (TARGET_64BIT)
    {
#ifndef NO_PROFILE_COUNTERS
      fprintf (file, "\tleaq\t%sP%d@(%%rip),%%r11\n", LPREFIX, labelno);
#endif

      if (!TARGET_64BIT_MS_ABI && flag_pic)
	fprintf (file, "\tcall\t*%s@GOTPCREL(%%rip)\n", MCOUNT_NAME);
      else
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

/* We don't have exact information about the insn sizes, but we may assume
   quite safely that we are informed about all 1 byte insns and memory
   address sizes.  This is enough to eliminate unnecessary padding in
   99% of cases.  */

static int
min_insn_size (rtx insn)
{
  int l = 0;

  if (!INSN_P (insn) || !active_insn_p (insn))
    return 0;

  /* Discard alignments we've emit and jump instructions.  */
  if (GET_CODE (PATTERN (insn)) == UNSPEC_VOLATILE
      && XINT (PATTERN (insn), 1) == UNSPECV_ALIGN)
    return 0;
  if (JUMP_P (insn)
      && (GET_CODE (PATTERN (insn)) == ADDR_VEC
	  || GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC))
    return 0;

  /* Important case - calls are always 5 bytes.
     It is common to have many calls in the row.  */
  if (CALL_P (insn)
      && symbolic_reference_mentioned_p (PATTERN (insn))
      && !SIBLING_CALL_P (insn))
    return 5;
  if (get_attr_length (insn) <= 1)
    return 1;

  /* For normal instructions we may rely on the sizes of addresses
     and the presence of symbol to require 4 bytes of encoding.
     This is not the case for jumps where references are PC relative.  */
  if (!JUMP_P (insn))
    {
      l = get_attr_length_address (insn);
      if (l < 4 && symbolic_reference_mentioned_p (PATTERN (insn)))
	l = 4;
    }
  if (l)
    return 1+l;
  else
    return 2;
}

/* AMD K8 core mispredicts jumps when there are more than 3 jumps in 16 byte
   window.  */

static void
ix86_avoid_jump_misspredicts (void)
{
  rtx insn, start = get_insns ();
  int nbytes = 0, njumps = 0;
  int isjump = 0;

  /* Look for all minimal intervals of instructions containing 4 jumps.
     The intervals are bounded by START and INSN.  NBYTES is the total
     size of instructions in the interval including INSN and not including
     START.  When the NBYTES is smaller than 16 bytes, it is possible
     that the end of START and INSN ends up in the same 16byte page.

     The smallest offset in the page INSN can start is the case where START
     ends on the offset 0.  Offset of INSN is then NBYTES - sizeof (INSN).
     We add p2align to 16byte window with maxskip 17 - NBYTES + sizeof (INSN).
     */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {

      nbytes += min_insn_size (insn);
      if (dump_file)
        fprintf(dump_file, "Insn %i estimated to %i bytes\n",
		INSN_UID (insn), min_insn_size (insn));
      if ((JUMP_P (insn)
	   && GET_CODE (PATTERN (insn)) != ADDR_VEC
	   && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	  || CALL_P (insn))
	njumps++;
      else
	continue;

      while (njumps > 3)
	{
	  start = NEXT_INSN (start);
	  if ((JUMP_P (start)
	       && GET_CODE (PATTERN (start)) != ADDR_VEC
	       && GET_CODE (PATTERN (start)) != ADDR_DIFF_VEC)
	      || CALL_P (start))
	    njumps--, isjump = 1;
	  else
	    isjump = 0;
	  nbytes -= min_insn_size (start);
	}
      gcc_assert (njumps >= 0);
      if (dump_file)
        fprintf (dump_file, "Interval %i to %i has %i bytes\n",
		INSN_UID (start), INSN_UID (insn), nbytes);

      if (njumps == 3 && isjump && nbytes < 16)
	{
	  int padsize = 15 - nbytes + min_insn_size (insn);

	  if (dump_file)
	    fprintf (dump_file, "Padding insn %i by %i bytes!\n",
		     INSN_UID (insn), padsize);
          emit_insn_before (gen_align (GEN_INT (padsize)), insn);
	}
    }
}

/* AMD Athlon works faster
   when RET is not destination of conditional jump or directly preceded
   by other jump instruction.  We avoid the penalty by inserting NOP just
   before the RET instructions in such cases.  */
static void
ix86_pad_returns (void)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
    {
      basic_block bb = e->src;
      rtx ret = BB_END (bb);
      rtx prev;
      bool replace = false;

      if (!JUMP_P (ret) || GET_CODE (PATTERN (ret)) != RETURN
	  || !maybe_hot_bb_p (bb))
	continue;
      for (prev = PREV_INSN (ret); prev; prev = PREV_INSN (prev))
	if (active_insn_p (prev) || LABEL_P (prev))
	  break;
      if (prev && LABEL_P (prev))
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (EDGE_FREQUENCY (e) && e->src->index >= 0
		&& !(e->flags & EDGE_FALLTHRU))
	      replace = true;
	}
      if (!replace)
	{
	  prev = prev_active_insn (ret);
	  if (prev
	      && ((JUMP_P (prev) && any_condjump_p (prev))
		  || CALL_P (prev)))
	    replace = true;
	  /* Empty functions get branch mispredict even when the jump destination
	     is not visible to us.  */
	  if (!prev && cfun->function_frequency > FUNCTION_FREQUENCY_UNLIKELY_EXECUTED)
	    replace = true;
	}
      if (replace)
	{
	  emit_insn_before (gen_return_internal_long (), ret);
	  delete_insn (ret);
	}
    }
}

/* Implement machine specific optimizations.  We implement padding of returns
   for K8 CPUs and pass to avoid 4 jumps in the single 16 byte window.  */
static void
ix86_reorg (void)
{
  if (TARGET_PAD_RETURNS && optimize && !optimize_size)
    ix86_pad_returns ();
  if (TARGET_FOUR_JUMP_LIMIT && optimize && !optimize_size)
    ix86_avoid_jump_misspredicts ();
}

/* Return nonzero when QImode register that must be represented via REX prefix
   is used.  */
bool
x86_extended_QIreg_mentioned_p (rtx insn)
{
  int i;
  extract_insn_cached (insn);
  for (i = 0; i < recog_data.n_operands; i++)
    if (REG_P (recog_data.operand[i])
	&& REGNO (recog_data.operand[i]) >= 4)
       return true;
  return false;
}

/* Return nonzero when P points to register encoded via REX prefix.
   Called via for_each_rtx.  */
static int
extended_reg_mentioned_1 (rtx *p, void *data ATTRIBUTE_UNUSED)
{
   unsigned int regno;
   if (!REG_P (*p))
     return 0;
   regno = REGNO (*p);
   return REX_INT_REGNO_P (regno) || REX_SSE_REGNO_P (regno);
}

/* Return true when INSN mentions register that must be encoded using REX
   prefix.  */
bool
x86_extended_reg_mentioned_p (rtx insn)
{
  return for_each_rtx (&PATTERN (insn), extended_reg_mentioned_1, NULL);
}

/* Generate an unsigned DImode/SImode to FP conversion.  This is the same code
   optabs would emit if we didn't have TFmode patterns.  */

void
x86_emit_floatuns (rtx operands[2])
{
  rtx neglab, donelab, i0, i1, f0, in, out;
  enum machine_mode mode, inmode;

  inmode = GET_MODE (operands[1]);
  gcc_assert (inmode == SImode || inmode == DImode);

  out = operands[0];
  in = force_reg (inmode, operands[1]);
  mode = GET_MODE (out);
  neglab = gen_label_rtx ();
  donelab = gen_label_rtx ();
  f0 = gen_reg_rtx (mode);

  emit_cmp_and_jump_insns (in, const0_rtx, LT, const0_rtx, inmode, 0, neglab);

  expand_float (out, in, 0);

  emit_jump_insn (gen_jump (donelab));
  emit_barrier ();

  emit_label (neglab);

  i0 = expand_simple_binop (inmode, LSHIFTRT, in, const1_rtx, NULL,
			    1, OPTAB_DIRECT);
  i1 = expand_simple_binop (inmode, AND, in, const1_rtx, NULL,
			    1, OPTAB_DIRECT);
  i0 = expand_simple_binop (inmode, IOR, i0, i1, i0, 1, OPTAB_DIRECT);

  expand_float (f0, i0, 0);

  emit_insn (gen_rtx_SET (VOIDmode, out, gen_rtx_PLUS (mode, f0, f0)));

  emit_label (donelab);
}

/* A subroutine of ix86_expand_vector_init.  Store into TARGET a vector
   with all elements equal to VAR.  Return true if successful.  */

static bool
ix86_expand_vector_init_duplicate (bool mmx_ok, enum machine_mode mode,
				   rtx target, rtx val)
{
  enum machine_mode smode, wsmode, wvmode;
  rtx x;

  switch (mode)
    {
    case V2SImode:
    case V2SFmode:
      if (!mmx_ok)
	return false;
      /* FALLTHRU */

    case V2DFmode:
    case V2DImode:
    case V4SFmode:
    case V4SImode:
      val = force_reg (GET_MODE_INNER (mode), val);
      x = gen_rtx_VEC_DUPLICATE (mode, val);
      emit_insn (gen_rtx_SET (VOIDmode, target, x));
      return true;

    case V4HImode:
      if (!mmx_ok)
	return false;
      if (TARGET_SSE || TARGET_3DNOW_A)
	{
	  val = gen_lowpart (SImode, val);
	  x = gen_rtx_TRUNCATE (HImode, val);
	  x = gen_rtx_VEC_DUPLICATE (mode, x);
	  emit_insn (gen_rtx_SET (VOIDmode, target, x));
	  return true;
	}
      else
	{
	  smode = HImode;
	  wsmode = SImode;
	  wvmode = V2SImode;
	  goto widen;
	}

    case V8QImode:
      if (!mmx_ok)
	return false;
      smode = QImode;
      wsmode = HImode;
      wvmode = V4HImode;
      goto widen;
    case V8HImode:
      if (TARGET_SSE2)
	{
	  rtx tmp1, tmp2;
	  /* Extend HImode to SImode using a paradoxical SUBREG.  */
	  tmp1 = gen_reg_rtx (SImode);
	  emit_move_insn (tmp1, gen_lowpart (SImode, val));
	  /* Insert the SImode value as low element of V4SImode vector. */
	  tmp2 = gen_reg_rtx (V4SImode);
	  tmp1 = gen_rtx_VEC_MERGE (V4SImode,
				    gen_rtx_VEC_DUPLICATE (V4SImode, tmp1),
				    CONST0_RTX (V4SImode),
				    const1_rtx);
	  emit_insn (gen_rtx_SET (VOIDmode, tmp2, tmp1));
	  /* Cast the V4SImode vector back to a V8HImode vector.  */
	  tmp1 = gen_reg_rtx (V8HImode);
	  emit_move_insn (tmp1, gen_lowpart (V8HImode, tmp2));
	  /* Duplicate the low short through the whole low SImode word.  */
	  emit_insn (gen_sse2_punpcklwd (tmp1, tmp1, tmp1));
	  /* Cast the V8HImode vector back to a V4SImode vector.  */
	  tmp2 = gen_reg_rtx (V4SImode);
	  emit_move_insn (tmp2, gen_lowpart (V4SImode, tmp1));
	  /* Replicate the low element of the V4SImode vector.  */
	  emit_insn (gen_sse2_pshufd (tmp2, tmp2, const0_rtx));
	  /* Cast the V2SImode back to V8HImode, and store in target.  */
	  emit_move_insn (target, gen_lowpart (V8HImode, tmp2));
	  return true;
	}
      smode = HImode;
      wsmode = SImode;
      wvmode = V4SImode;
      goto widen;
    case V16QImode:
      if (TARGET_SSE2)
	{
	  rtx tmp1, tmp2;
	  /* Extend QImode to SImode using a paradoxical SUBREG.  */
	  tmp1 = gen_reg_rtx (SImode);
	  emit_move_insn (tmp1, gen_lowpart (SImode, val));
	  /* Insert the SImode value as low element of V4SImode vector. */
	  tmp2 = gen_reg_rtx (V4SImode);
	  tmp1 = gen_rtx_VEC_MERGE (V4SImode,
				    gen_rtx_VEC_DUPLICATE (V4SImode, tmp1),
				    CONST0_RTX (V4SImode),
				    const1_rtx);
	  emit_insn (gen_rtx_SET (VOIDmode, tmp2, tmp1));
	  /* Cast the V4SImode vector back to a V16QImode vector.  */
	  tmp1 = gen_reg_rtx (V16QImode);
	  emit_move_insn (tmp1, gen_lowpart (V16QImode, tmp2));
	  /* Duplicate the low byte through the whole low SImode word.  */
	  emit_insn (gen_sse2_punpcklbw (tmp1, tmp1, tmp1));
	  emit_insn (gen_sse2_punpcklbw (tmp1, tmp1, tmp1));
	  /* Cast the V16QImode vector back to a V4SImode vector.  */
	  tmp2 = gen_reg_rtx (V4SImode);
	  emit_move_insn (tmp2, gen_lowpart (V4SImode, tmp1));
	  /* Replicate the low element of the V4SImode vector.  */
	  emit_insn (gen_sse2_pshufd (tmp2, tmp2, const0_rtx));
	  /* Cast the V2SImode back to V16QImode, and store in target.  */
	  emit_move_insn (target, gen_lowpart (V16QImode, tmp2));
	  return true;
	}
      smode = QImode;
      wsmode = HImode;
      wvmode = V8HImode;
      goto widen;
    widen:
      /* Replicate the value once into the next wider mode and recurse.  */
      val = convert_modes (wsmode, smode, val, true);
      x = expand_simple_binop (wsmode, ASHIFT, val,
			       GEN_INT (GET_MODE_BITSIZE (smode)),
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
      val = expand_simple_binop (wsmode, IOR, val, x, x, 1, OPTAB_LIB_WIDEN);

      x = gen_reg_rtx (wvmode);
      if (!ix86_expand_vector_init_duplicate (mmx_ok, wvmode, x, val))
	gcc_unreachable ();
      emit_move_insn (target, gen_lowpart (mode, x));
      return true;

    default:
      return false;
    }
}

/* A subroutine of ix86_expand_vector_init.  Store into TARGET a vector
   whose ONE_VAR element is VAR, and other elements are zero.  Return true
   if successful.  */

static bool
ix86_expand_vector_init_one_nonzero (bool mmx_ok, enum machine_mode mode,
				     rtx target, rtx var, int one_var)
{
  enum machine_mode vsimode;
  rtx new_target;
  rtx x, tmp;

  switch (mode)
    {
    case V2SFmode:
    case V2SImode:
      if (!mmx_ok)
	return false;
      /* FALLTHRU */

    case V2DFmode:
    case V2DImode:
      if (one_var != 0)
	return false;
      var = force_reg (GET_MODE_INNER (mode), var);
      x = gen_rtx_VEC_CONCAT (mode, var, CONST0_RTX (GET_MODE_INNER (mode)));
      emit_insn (gen_rtx_SET (VOIDmode, target, x));
      return true;

    case V4SFmode:
    case V4SImode:
      if (!REG_P (target) || REGNO (target) < FIRST_PSEUDO_REGISTER)
	new_target = gen_reg_rtx (mode);
      else
	new_target = target;
      var = force_reg (GET_MODE_INNER (mode), var);
      x = gen_rtx_VEC_DUPLICATE (mode, var);
      x = gen_rtx_VEC_MERGE (mode, x, CONST0_RTX (mode), const1_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, new_target, x));
      if (one_var != 0)
	{
	  /* We need to shuffle the value to the correct position, so
	     create a new pseudo to store the intermediate result.  */

	  /* With SSE2, we can use the integer shuffle insns.  */
	  if (mode != V4SFmode && TARGET_SSE2)
	    {
	      emit_insn (gen_sse2_pshufd_1 (new_target, new_target,
					    GEN_INT (1),
					    GEN_INT (one_var == 1 ? 0 : 1),
					    GEN_INT (one_var == 2 ? 0 : 1),
					    GEN_INT (one_var == 3 ? 0 : 1)));
	      if (target != new_target)
		emit_move_insn (target, new_target);
	      return true;
	    }

	  /* Otherwise convert the intermediate result to V4SFmode and
	     use the SSE1 shuffle instructions.  */
	  if (mode != V4SFmode)
	    {
	      tmp = gen_reg_rtx (V4SFmode);
	      emit_move_insn (tmp, gen_lowpart (V4SFmode, new_target));
	    }
	  else
	    tmp = new_target;

	  emit_insn (gen_sse_shufps_1 (tmp, tmp, tmp,
				       GEN_INT (1),
				       GEN_INT (one_var == 1 ? 0 : 1),
				       GEN_INT (one_var == 2 ? 0+4 : 1+4),
				       GEN_INT (one_var == 3 ? 0+4 : 1+4)));

	  if (mode != V4SFmode)
	    emit_move_insn (target, gen_lowpart (V4SImode, tmp));
	  else if (tmp != target)
	    emit_move_insn (target, tmp);
	}
      else if (target != new_target)
	emit_move_insn (target, new_target);
      return true;

    case V8HImode:
    case V16QImode:
      vsimode = V4SImode;
      goto widen;
    case V4HImode:
    case V8QImode:
      if (!mmx_ok)
	return false;
      vsimode = V2SImode;
      goto widen;
    widen:
      if (one_var != 0)
	return false;

      /* Zero extend the variable element to SImode and recurse.  */
      var = convert_modes (SImode, GET_MODE_INNER (mode), var, true);

      x = gen_reg_rtx (vsimode);
      if (!ix86_expand_vector_init_one_nonzero (mmx_ok, vsimode, x,
						var, one_var))
	gcc_unreachable ();

      emit_move_insn (target, gen_lowpart (mode, x));
      return true;

    default:
      return false;
    }
}

/* A subroutine of ix86_expand_vector_init.  Store into TARGET a vector
   consisting of the values in VALS.  It is known that all elements
   except ONE_VAR are constants.  Return true if successful.  */

static bool
ix86_expand_vector_init_one_var (bool mmx_ok, enum machine_mode mode,
				 rtx target, rtx vals, int one_var)
{
  rtx var = XVECEXP (vals, 0, one_var);
  enum machine_mode wmode;
  rtx const_vec, x;

  const_vec = copy_rtx (vals);
  XVECEXP (const_vec, 0, one_var) = CONST0_RTX (GET_MODE_INNER (mode));
  const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (const_vec, 0));

  switch (mode)
    {
    case V2DFmode:
    case V2DImode:
    case V2SFmode:
    case V2SImode:
      /* For the two element vectors, it's just as easy to use
	 the general case.  */
      return false;

    case V4SFmode:
    case V4SImode:
    case V8HImode:
    case V4HImode:
      break;

    case V16QImode:
      wmode = V8HImode;
      goto widen;
    case V8QImode:
      wmode = V4HImode;
      goto widen;
    widen:
      /* There's no way to set one QImode entry easily.  Combine
	 the variable value with its adjacent constant value, and
	 promote to an HImode set.  */
      x = XVECEXP (vals, 0, one_var ^ 1);
      if (one_var & 1)
	{
	  var = convert_modes (HImode, QImode, var, true);
	  var = expand_simple_binop (HImode, ASHIFT, var, GEN_INT (8),
				     NULL_RTX, 1, OPTAB_LIB_WIDEN);
	  x = GEN_INT (INTVAL (x) & 0xff);
	}
      else
	{
	  var = convert_modes (HImode, QImode, var, true);
	  x = gen_int_mode (INTVAL (x) << 8, HImode);
	}
      if (x != const0_rtx)
	var = expand_simple_binop (HImode, IOR, var, x, var,
				   1, OPTAB_LIB_WIDEN);

      x = gen_reg_rtx (wmode);
      emit_move_insn (x, gen_lowpart (wmode, const_vec));
      ix86_expand_vector_set (mmx_ok, x, var, one_var >> 1);

      emit_move_insn (target, gen_lowpart (mode, x));
      return true;

    default:
      return false;
    }

  emit_move_insn (target, const_vec);
  ix86_expand_vector_set (mmx_ok, target, var, one_var);
  return true;
}

/* A subroutine of ix86_expand_vector_init.  Handle the most general case:
   all values variable, and none identical.  */

static void
ix86_expand_vector_init_general (bool mmx_ok, enum machine_mode mode,
				 rtx target, rtx vals)
{
  enum machine_mode half_mode = GET_MODE_INNER (mode);
  rtx op0 = NULL, op1 = NULL;
  bool use_vec_concat = false;

  switch (mode)
    {
    case V2SFmode:
    case V2SImode:
      if (!mmx_ok && !TARGET_SSE)
	break;
      /* FALLTHRU */

    case V2DFmode:
    case V2DImode:
      /* For the two element vectors, we always implement VEC_CONCAT.  */
      op0 = XVECEXP (vals, 0, 0);
      op1 = XVECEXP (vals, 0, 1);
      use_vec_concat = true;
      break;

    case V4SFmode:
      half_mode = V2SFmode;
      goto half;
    case V4SImode:
      half_mode = V2SImode;
      goto half;
    half:
      {
	rtvec v;

	/* For V4SF and V4SI, we implement a concat of two V2 vectors.
	   Recurse to load the two halves.  */

	op0 = gen_reg_rtx (half_mode);
	v = gen_rtvec (2, XVECEXP (vals, 0, 0), XVECEXP (vals, 0, 1));
	ix86_expand_vector_init (false, op0, gen_rtx_PARALLEL (half_mode, v));

	op1 = gen_reg_rtx (half_mode);
	v = gen_rtvec (2, XVECEXP (vals, 0, 2), XVECEXP (vals, 0, 3));
	ix86_expand_vector_init (false, op1, gen_rtx_PARALLEL (half_mode, v));

	use_vec_concat = true;
      }
      break;

    case V8HImode:
    case V16QImode:
    case V4HImode:
    case V8QImode:
      break;

    default:
      gcc_unreachable ();
    }

  if (use_vec_concat)
    {
      if (!register_operand (op0, half_mode))
	op0 = force_reg (half_mode, op0);
      if (!register_operand (op1, half_mode))
	op1 = force_reg (half_mode, op1);

      emit_insn (gen_rtx_SET (VOIDmode, target,
			      gen_rtx_VEC_CONCAT (mode, op0, op1)));
    }
  else
    {
      int i, j, n_elts, n_words, n_elt_per_word;
      enum machine_mode inner_mode;
      rtx words[4], shift;

      inner_mode = GET_MODE_INNER (mode);
      n_elts = GET_MODE_NUNITS (mode);
      n_words = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
      n_elt_per_word = n_elts / n_words;
      shift = GEN_INT (GET_MODE_BITSIZE (inner_mode));

      for (i = 0; i < n_words; ++i)
	{
	  rtx word = NULL_RTX;

	  for (j = 0; j < n_elt_per_word; ++j)
	    {
	      rtx elt = XVECEXP (vals, 0, (i+1)*n_elt_per_word - j - 1);
	      elt = convert_modes (word_mode, inner_mode, elt, true);

	      if (j == 0)
		word = elt;
	      else
		{
		  word = expand_simple_binop (word_mode, ASHIFT, word, shift,
					      word, 1, OPTAB_LIB_WIDEN);
		  word = expand_simple_binop (word_mode, IOR, word, elt,
					      word, 1, OPTAB_LIB_WIDEN);
		}
	    }

	  words[i] = word;
	}

      if (n_words == 1)
	emit_move_insn (target, gen_lowpart (mode, words[0]));
      else if (n_words == 2)
	{
	  rtx tmp = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_CLOBBER (VOIDmode, tmp));
	  emit_move_insn (gen_lowpart (word_mode, tmp), words[0]);
	  emit_move_insn (gen_highpart (word_mode, tmp), words[1]);
	  emit_move_insn (target, tmp);
	}
      else if (n_words == 4)
	{
	  rtx tmp = gen_reg_rtx (V4SImode);
	  vals = gen_rtx_PARALLEL (V4SImode, gen_rtvec_v (4, words));
	  ix86_expand_vector_init_general (false, V4SImode, tmp, vals);
	  emit_move_insn (target, gen_lowpart (mode, tmp));
	}
      else
	gcc_unreachable ();
    }
}

/* Initialize vector TARGET via VALS.  Suppress the use of MMX
   instructions unless MMX_OK is true.  */

void
ix86_expand_vector_init (bool mmx_ok, rtx target, rtx vals)
{
  enum machine_mode mode = GET_MODE (target);
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0, one_var = -1;
  bool all_same = true, all_const_zero = true;
  int i;
  rtx x;

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!(CONST_INT_P (x)
	    || GET_CODE (x) == CONST_DOUBLE
	    || GET_CODE (x) == CONST_FIXED))
	n_var++, one_var = i;
      else if (x != CONST0_RTX (inner_mode))
	all_const_zero = false;
      if (i > 0 && !rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;
    }

  /* Constants are best loaded from the constant pool.  */
  if (n_var == 0)
    {
      emit_move_insn (target, gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0)));
      return;
    }

  /* If all values are identical, broadcast the value.  */
  if (all_same
      && ix86_expand_vector_init_duplicate (mmx_ok, mode, target,
					    XVECEXP (vals, 0, 0)))
    return;

  /* Values where only one field is non-constant are best loaded from
     the pool and overwritten via move later.  */
  if (n_var == 1)
    {
      if (all_const_zero
	  && ix86_expand_vector_init_one_nonzero (mmx_ok, mode, target,
						  XVECEXP (vals, 0, one_var),
						  one_var))
	return;

      if (ix86_expand_vector_init_one_var (mmx_ok, mode, target, vals, one_var))
	return;
    }

  ix86_expand_vector_init_general (mmx_ok, mode, target, vals);
}

void
ix86_expand_vector_set (bool mmx_ok, rtx target, rtx val, int elt)
{
  enum machine_mode mode = GET_MODE (target);
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  bool use_vec_merge = false;
  rtx tmp;

  switch (mode)
    {
    case V2SFmode:
    case V2SImode:
      if (mmx_ok)
	{
	  tmp = gen_reg_rtx (GET_MODE_INNER (mode));
	  ix86_expand_vector_extract (true, tmp, target, 1 - elt);
	  if (elt == 0)
	    tmp = gen_rtx_VEC_CONCAT (mode, tmp, val);
	  else
	    tmp = gen_rtx_VEC_CONCAT (mode, val, tmp);
	  emit_insn (gen_rtx_SET (VOIDmode, target, tmp));
	  return;
	}
      break;

    case V2DImode:
      use_vec_merge = TARGET_SSE4_1;
      if (use_vec_merge)
	break;

    case V2DFmode:
      {
	rtx op0, op1;

	/* For the two element vectors, we implement a VEC_CONCAT with
	   the extraction of the other element.  */

	tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, GEN_INT (1 - elt)));
	tmp = gen_rtx_VEC_SELECT (inner_mode, target, tmp);

	if (elt == 0)
	  op0 = val, op1 = tmp;
	else
	  op0 = tmp, op1 = val;

	tmp = gen_rtx_VEC_CONCAT (mode, op0, op1);
	emit_insn (gen_rtx_SET (VOIDmode, target, tmp));
      }
      return;

    case V4SFmode:
      use_vec_merge = TARGET_SSE4_1;
      if (use_vec_merge)
	break;

      switch (elt)
	{
	case 0:
	  use_vec_merge = true;
	  break;

	case 1:
	  /* tmp = target = A B C D */
	  tmp = copy_to_reg (target);
	  /* target = A A B B */
	  emit_insn (gen_sse_unpcklps (target, target, target));
	  /* target = X A B B */
	  ix86_expand_vector_set (false, target, val, 0);
	  /* target = A X C D  */
	  emit_insn (gen_sse_shufps_1 (target, target, tmp,
				       GEN_INT (1), GEN_INT (0),
				       GEN_INT (2+4), GEN_INT (3+4)));
	  return;

	case 2:
	  /* tmp = target = A B C D */
	  tmp = copy_to_reg (target);
	  /* tmp = X B C D */
	  ix86_expand_vector_set (false, tmp, val, 0);
	  /* target = A B X D */
	  emit_insn (gen_sse_shufps_1 (target, target, tmp,
				       GEN_INT (0), GEN_INT (1),
				       GEN_INT (0+4), GEN_INT (3+4)));
	  return;

	case 3:
	  /* tmp = target = A B C D */
	  tmp = copy_to_reg (target);
	  /* tmp = X B C D */
	  ix86_expand_vector_set (false, tmp, val, 0);
	  /* target = A B X D */
	  emit_insn (gen_sse_shufps_1 (target, target, tmp,
				       GEN_INT (0), GEN_INT (1),
				       GEN_INT (2+4), GEN_INT (0+4)));
	  return;

	default:
	  gcc_unreachable ();
	}
      break;

    case V4SImode:
      use_vec_merge = TARGET_SSE4_1;
      if (use_vec_merge)
	break;

      /* Element 0 handled by vec_merge below.  */
      if (elt == 0)
	{
	  use_vec_merge = true;
	  break;
	}

      if (TARGET_SSE2)
	{
	  /* With SSE2, use integer shuffles to swap element 0 and ELT,
	     store into element 0, then shuffle them back.  */

	  rtx order[4];

	  order[0] = GEN_INT (elt);
	  order[1] = const1_rtx;
	  order[2] = const2_rtx;
	  order[3] = GEN_INT (3);
	  order[elt] = const0_rtx;

	  emit_insn (gen_sse2_pshufd_1 (target, target, order[0],
					order[1], order[2], order[3]));

	  ix86_expand_vector_set (false, target, val, 0);

	  emit_insn (gen_sse2_pshufd_1 (target, target, order[0],
					order[1], order[2], order[3]));
	}
      else
	{
	  /* For SSE1, we have to reuse the V4SF code.  */
	  ix86_expand_vector_set (false, gen_lowpart (V4SFmode, target),
				  gen_lowpart (SFmode, val), elt);
	}
      return;

    case V8HImode:
      use_vec_merge = TARGET_SSE2;
      break;
    case V4HImode:
      use_vec_merge = mmx_ok && (TARGET_SSE || TARGET_3DNOW_A);
      break;

    case V16QImode:
      use_vec_merge = TARGET_SSE4_1;
      break;

    case V8QImode:
    default:
      break;
    }

  if (use_vec_merge)
    {
      tmp = gen_rtx_VEC_DUPLICATE (mode, val);
      tmp = gen_rtx_VEC_MERGE (mode, tmp, target, GEN_INT (1 << elt));
      emit_insn (gen_rtx_SET (VOIDmode, target, tmp));
    }
  else
    {
      rtx mem = assign_stack_temp (mode, GET_MODE_SIZE (mode), false);

      emit_move_insn (mem, target);

      tmp = adjust_address (mem, inner_mode, elt*GET_MODE_SIZE (inner_mode));
      emit_move_insn (tmp, val);

      emit_move_insn (target, mem);
    }
}

void
ix86_expand_vector_extract (bool mmx_ok, rtx target, rtx vec, int elt)
{
  enum machine_mode mode = GET_MODE (vec);
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  bool use_vec_extr = false;
  rtx tmp;

  switch (mode)
    {
    case V2SImode:
    case V2SFmode:
      if (!mmx_ok)
	break;
      /* FALLTHRU */

    case V2DFmode:
    case V2DImode:
      use_vec_extr = true;
      break;

    case V4SFmode:
      use_vec_extr = TARGET_SSE4_1;
      if (use_vec_extr)
	break;

      switch (elt)
	{
	case 0:
	  tmp = vec;
	  break;

	case 1:
	case 3:
	  tmp = gen_reg_rtx (mode);
	  emit_insn (gen_sse_shufps_1 (tmp, vec, vec,
				       GEN_INT (elt), GEN_INT (elt),
				       GEN_INT (elt+4), GEN_INT (elt+4)));
	  break;

	case 2:
	  tmp = gen_reg_rtx (mode);
	  emit_insn (gen_sse_unpckhps (tmp, vec, vec));
	  break;

	default:
	  gcc_unreachable ();
	}
      vec = tmp;
      use_vec_extr = true;
      elt = 0;
      break;

    case V4SImode:
      use_vec_extr = TARGET_SSE4_1;
      if (use_vec_extr)
	break;

      if (TARGET_SSE2)
	{
	  switch (elt)
	    {
	    case 0:
	      tmp = vec;
	      break;

	    case 1:
	    case 3:
	      tmp = gen_reg_rtx (mode);
	      emit_insn (gen_sse2_pshufd_1 (tmp, vec,
					    GEN_INT (elt), GEN_INT (elt),
					    GEN_INT (elt), GEN_INT (elt)));
	      break;

	    case 2:
	      tmp = gen_reg_rtx (mode);
	      emit_insn (gen_sse2_punpckhdq (tmp, vec, vec));
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  vec = tmp;
	  use_vec_extr = true;
	  elt = 0;
	}
      else
	{
	  /* For SSE1, we have to reuse the V4SF code.  */
	  ix86_expand_vector_extract (false, gen_lowpart (SFmode, target),
				      gen_lowpart (V4SFmode, vec), elt);
	  return;
	}
      break;

    case V8HImode:
      use_vec_extr = TARGET_SSE2;
      break;
    case V4HImode:
      use_vec_extr = mmx_ok && (TARGET_SSE || TARGET_3DNOW_A);
      break;

    case V16QImode:
      use_vec_extr = TARGET_SSE4_1;
      break;

    case V8QImode:
      /* ??? Could extract the appropriate HImode element and shift.  */
    default:
      break;
    }

  if (use_vec_extr)
    {
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, GEN_INT (elt)));
      tmp = gen_rtx_VEC_SELECT (inner_mode, vec, tmp);

      /* Let the rtl optimizers know about the zero extension performed.  */
      if (inner_mode == QImode || inner_mode == HImode)
	{
	  tmp = gen_rtx_ZERO_EXTEND (SImode, tmp);
	  target = gen_lowpart (SImode, target);
	}

      emit_insn (gen_rtx_SET (VOIDmode, target, tmp));
    }
  else
    {
      rtx mem = assign_stack_temp (mode, GET_MODE_SIZE (mode), false);

      emit_move_insn (mem, vec);

      tmp = adjust_address (mem, inner_mode, elt*GET_MODE_SIZE (inner_mode));
      emit_move_insn (target, tmp);
    }
}

/* Expand a vector reduction on V4SFmode for SSE1.  FN is the binary
   pattern to reduce; DEST is the destination; IN is the input vector.  */

void
ix86_expand_reduc_v4sf (rtx (*fn) (rtx, rtx, rtx), rtx dest, rtx in)
{
  rtx tmp1, tmp2, tmp3;

  tmp1 = gen_reg_rtx (V4SFmode);
  tmp2 = gen_reg_rtx (V4SFmode);
  tmp3 = gen_reg_rtx (V4SFmode);

  emit_insn (gen_sse_movhlps (tmp1, in, in));
  emit_insn (fn (tmp2, tmp1, in));

  emit_insn (gen_sse_shufps_1 (tmp3, tmp2, tmp2,
			       GEN_INT (1), GEN_INT (1),
			       GEN_INT (1+4), GEN_INT (1+4)));
  emit_insn (fn (dest, tmp2, tmp3));
}

/* Target hook for scalar_mode_supported_p.  */
static bool
ix86_scalar_mode_supported_p (enum machine_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode))
    return true;
  else if (mode == TFmode)
    return TARGET_64BIT;
  else
    return default_scalar_mode_supported_p (mode);
}

/* Implements target hook vector_mode_supported_p.  */
static bool
ix86_vector_mode_supported_p (enum machine_mode mode)
{
  if (TARGET_SSE && VALID_SSE_REG_MODE (mode))
    return true;
  if (TARGET_SSE2 && VALID_SSE2_REG_MODE (mode))
    return true;
  if (TARGET_MMX && VALID_MMX_REG_MODE (mode))
    return true;
  if (TARGET_3DNOW && VALID_MMX_REG_MODE_3DNOW (mode))
    return true;
  return false;
}

/* Target hook for c_mode_for_suffix.  */
static enum machine_mode
ix86_c_mode_for_suffix (char suffix)
{
  if (TARGET_64BIT && suffix == 'q')
    return TFmode;
  if (TARGET_MMX && suffix == 'w')
    return XFmode;

  return VOIDmode;
}

/* Worker function for TARGET_MD_ASM_CLOBBERS.

   We do this in the new i386 backend to maintain source compatibility
   with the old cc0-based compiler.  */

static tree
ix86_md_asm_clobbers (tree outputs ATTRIBUTE_UNUSED,
		      tree inputs ATTRIBUTE_UNUSED,
		      tree clobbers)
{
  clobbers = tree_cons (NULL_TREE, build_string (5, "flags"),
			clobbers);
  clobbers = tree_cons (NULL_TREE, build_string (4, "fpsr"),
			clobbers);
  return clobbers;
}

/* Implements target vector targetm.asm.encode_section_info.  This
   is not used by netware.  */

static void ATTRIBUTE_UNUSED
ix86_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (TREE_CODE (decl) == VAR_DECL
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl))
      && ix86_in_large_data_p (decl))
    SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= SYMBOL_FLAG_FAR_ADDR;
}

/* Worker function for REVERSE_CONDITION.  */

enum rtx_code
ix86_reverse_condition (enum rtx_code code, enum machine_mode mode)
{
  return (mode != CCFPmode && mode != CCFPUmode
	  ? reverse_condition (code)
	  : reverse_condition_maybe_unordered (code));
}

/* Output code to perform an x87 FP register move, from OPERANDS[1]
   to OPERANDS[0].  */

const char *
output_387_reg_move (rtx insn, rtx *operands)
{
  if (REG_P (operands[0]))
    {
      if (REG_P (operands[1])
	  && find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	{
	  if (REGNO (operands[0]) == FIRST_STACK_REG)
	    return output_387_ffreep (operands, 0);
	  return "fstp\t%y0";
	}
      if (STACK_TOP_P (operands[0]))
	return "fld%z1\t%y1";
      return "fst\t%y0";
    }
  else if (MEM_P (operands[0]))
    {
      gcc_assert (REG_P (operands[1]));
      if (find_regno_note (insn, REG_DEAD, REGNO (operands[1])))
	return "fstp%z0\t%y0";
      else
	{
	  /* There is no non-popping store to memory for XFmode.
	     So if we need one, follow the store with a load.  */
	  if (GET_MODE (operands[0]) == XFmode)
	    return "fstp%z0\t%y0\n\tfld%z0\t%y0";
	  else
	    return "fst%z0\t%y0";
	}
    }
  else
    gcc_unreachable();
}

/* Output code to perform a conditional jump to LABEL, if C2 flag in
   FP status register is set.  */

void
ix86_emit_fp_unordered_jump (rtx label)
{
  rtx reg = gen_reg_rtx (HImode);
  rtx temp;

  emit_insn (gen_x86_fnstsw_1 (reg));

  if (TARGET_SAHF && (TARGET_USE_SAHF || optimize_size))
    {
      emit_insn (gen_x86_sahf_1 (reg));

      temp = gen_rtx_REG (CCmode, FLAGS_REG);
      temp = gen_rtx_UNORDERED (VOIDmode, temp, const0_rtx);
    }
  else
    {
      emit_insn (gen_testqi_ext_ccno_0 (reg, GEN_INT (0x04)));

      temp = gen_rtx_REG (CCNOmode, FLAGS_REG);
      temp = gen_rtx_NE (VOIDmode, temp, const0_rtx);
    }

  temp = gen_rtx_IF_THEN_ELSE (VOIDmode, temp,
			      gen_rtx_LABEL_REF (VOIDmode, label),
			      pc_rtx);
  temp = gen_rtx_SET (VOIDmode, pc_rtx, temp);

  emit_jump_insn (temp);
  predict_jump (REG_BR_PROB_BASE * 10 / 100);
}

/* Output code to perform a log1p XFmode calculation.  */

void ix86_emit_i387_log1p (rtx op0, rtx op1)
{
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();

  rtx tmp = gen_reg_rtx (XFmode);
  rtx tmp2 = gen_reg_rtx (XFmode);

  emit_insn (gen_absxf2 (tmp, op1));
  emit_insn (gen_cmpxf (tmp,
    CONST_DOUBLE_FROM_REAL_VALUE (
       REAL_VALUE_ATOF ("0.29289321881345247561810596348408353", XFmode),
       XFmode)));
  emit_jump_insn (gen_bge (label1));

  emit_move_insn (tmp2, standard_80387_constant_rtx (4)); /* fldln2 */
  emit_insn (gen_fyl2xp1xf3_i387 (op0, op1, tmp2));
  emit_jump (label2);

  emit_label (label1);
  emit_move_insn (tmp, CONST1_RTX (XFmode));
  emit_insn (gen_addxf3 (tmp, op1, tmp));
  emit_move_insn (tmp2, standard_80387_constant_rtx (4)); /* fldln2 */
  emit_insn (gen_fyl2xxf3_i387 (op0, tmp, tmp2));

  emit_label (label2);
}

/* Output code to perform a Newton-Rhapson approximation of a single precision
   floating point divide [http://en.wikipedia.org/wiki/N-th_root_algorithm].  */

void ix86_emit_swdivsf (rtx res, rtx a, rtx b, enum machine_mode mode)
{
  rtx x0, x1, e0, e1, two;

  x0 = gen_reg_rtx (mode);
  e0 = gen_reg_rtx (mode);
  e1 = gen_reg_rtx (mode);
  x1 = gen_reg_rtx (mode);

  two = CONST_DOUBLE_FROM_REAL_VALUE (dconst2, SFmode);

  if (VECTOR_MODE_P (mode))
    two = ix86_build_const_vector (SFmode, true, two);

  two = force_reg (mode, two);

  /* a / b = a * rcp(b) * (2.0 - b * rcp(b)) */

  /* x0 = rcp(b) estimate */
  emit_insn (gen_rtx_SET (VOIDmode, x0,
			  gen_rtx_UNSPEC (mode, gen_rtvec (1, b),
					  UNSPEC_RCP)));
  /* e0 = x0 * b */
  emit_insn (gen_rtx_SET (VOIDmode, e0,
			  gen_rtx_MULT (mode, x0, b)));
  /* e1 = 2. - e0 */
  emit_insn (gen_rtx_SET (VOIDmode, e1,
			  gen_rtx_MINUS (mode, two, e0)));
  /* x1 = x0 * e1 */
  emit_insn (gen_rtx_SET (VOIDmode, x1,
			  gen_rtx_MULT (mode, x0, e1)));
  /* res = a * x1 */
  emit_insn (gen_rtx_SET (VOIDmode, res,
			  gen_rtx_MULT (mode, a, x1)));
}

/* Output code to perform a Newton-Rhapson approximation of a
   single precision floating point [reciprocal] square root.  */

void ix86_emit_swsqrtsf (rtx res, rtx a, enum machine_mode mode,
			 bool recip)
{
  rtx x0, e0, e1, e2, e3, mthree, mhalf;
  REAL_VALUE_TYPE r;

  x0 = gen_reg_rtx (mode);
  e0 = gen_reg_rtx (mode);
  e1 = gen_reg_rtx (mode);
  e2 = gen_reg_rtx (mode);
  e3 = gen_reg_rtx (mode);

  real_arithmetic (&r, NEGATE_EXPR, &dconst3, NULL);
  mthree = CONST_DOUBLE_FROM_REAL_VALUE (r, SFmode);

  real_arithmetic (&r, NEGATE_EXPR, &dconsthalf, NULL);
  mhalf = CONST_DOUBLE_FROM_REAL_VALUE (r, SFmode);

  if (VECTOR_MODE_P (mode))
    {
      mthree = ix86_build_const_vector (SFmode, true, mthree);
      mhalf = ix86_build_const_vector (SFmode, true, mhalf);
    }

  /* sqrt(a)  = -0.5 * a * rsqrtss(a) * (a * rsqrtss(a) * rsqrtss(a) - 3.0)
     rsqrt(a) = -0.5     * rsqrtss(a) * (a * rsqrtss(a) * rsqrtss(a) - 3.0) */

  /* x0 = rsqrt(a) estimate */
  emit_insn (gen_rtx_SET (VOIDmode, x0,
			  gen_rtx_UNSPEC (mode, gen_rtvec (1, a),
					  UNSPEC_RSQRT)));

  /* If (a == 0.0) Filter out infinity to prevent NaN for sqrt(0.0).  */
  if (!recip)
    {
      rtx zero, mask;

      zero = gen_reg_rtx (mode);
      mask = gen_reg_rtx (mode);

      zero = force_reg (mode, CONST0_RTX(mode));
      emit_insn (gen_rtx_SET (VOIDmode, mask,
			      gen_rtx_NE (mode, zero, a)));

      emit_insn (gen_rtx_SET (VOIDmode, x0,
			      gen_rtx_AND (mode, x0, mask)));
    }

  /* e0 = x0 * a */
  emit_insn (gen_rtx_SET (VOIDmode, e0,
			  gen_rtx_MULT (mode, x0, a)));
  /* e1 = e0 * x0 */
  emit_insn (gen_rtx_SET (VOIDmode, e1,
			  gen_rtx_MULT (mode, e0, x0)));

  /* e2 = e1 - 3. */
  mthree = force_reg (mode, mthree);
  emit_insn (gen_rtx_SET (VOIDmode, e2,
			  gen_rtx_PLUS (mode, e1, mthree)));

  mhalf = force_reg (mode, mhalf);
  if (recip)
    /* e3 = -.5 * x0 */
    emit_insn (gen_rtx_SET (VOIDmode, e3,
			    gen_rtx_MULT (mode, x0, mhalf)));
  else
    /* e3 = -.5 * e0 */
    emit_insn (gen_rtx_SET (VOIDmode, e3,
			    gen_rtx_MULT (mode, e0, mhalf)));
  /* ret = e2 * e3 */
  emit_insn (gen_rtx_SET (VOIDmode, res,
			  gen_rtx_MULT (mode, e2, e3)));
}

/* Solaris implementation of TARGET_ASM_NAMED_SECTION.  */

static void ATTRIBUTE_UNUSED
i386_solaris_elf_named_section (const char *name, unsigned int flags,
				tree decl)
{
  /* With Binutils 2.15, the "@unwind" marker must be specified on
     every occurrence of the ".eh_frame" section, not just the first
     one.  */
  if (TARGET_64BIT
      && strcmp (name, ".eh_frame") == 0)
    {
      fprintf (asm_out_file, "\t.section\t%s,\"%s\",@unwind\n", name,
	       flags & SECTION_WRITE ? "aw" : "a");
      return;
    }
  default_elf_asm_named_section (name, flags, decl);
}

/* Return the mangling of TYPE if it is an extended fundamental type.  */

static const char *
ix86_mangle_type (const_tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) != VOID_TYPE && TREE_CODE (type) != BOOLEAN_TYPE
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    return NULL;

  switch (TYPE_MODE (type))
    {
    case TFmode:
      /* __float128 is "g".  */
      return "g";
    case XFmode:
      /* "long double" or __float80 is "e".  */
      return "e";
    default:
      return NULL;
    }
}

/* For 32-bit code we can save PIC register setup by using
   __stack_chk_fail_local hidden function instead of calling
   __stack_chk_fail directly.  64-bit code doesn't need to setup any PIC
   register, so it is better to call __stack_chk_fail directly.  */

static tree
ix86_stack_protect_fail (void)
{
  return TARGET_64BIT
	 ? default_external_stack_protect_fail ()
	 : default_hidden_stack_protect_fail ();
}

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   ??? All x86 object file formats are capable of representing this.
   After all, the relocation needed is the same as for the call insn.
   Whether or not a particular assembler allows us to enter such, I
   guess we'll have to see.  */
int
asm_preferred_eh_data_format (int code, int global)
{
  if (flag_pic)
    {
      int type = DW_EH_PE_sdata8;
      if (!TARGET_64BIT
	  || ix86_cmodel == CM_SMALL_PIC
	  || (ix86_cmodel == CM_MEDIUM_PIC && (global || code)))
	type = DW_EH_PE_sdata4;
      return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
    }
  if (ix86_cmodel == CM_SMALL
      || (ix86_cmodel == CM_MEDIUM && code))
    return DW_EH_PE_udata4;
  return DW_EH_PE_absptr;
}

/* Expand copysign from SIGN to the positive value ABS_VALUE
   storing in RESULT.  If MASK is non-null, it shall be a mask to mask out
   the sign-bit.  */
static void
ix86_sse_copysign_to_positive (rtx result, rtx abs_value, rtx sign, rtx mask)
{
  enum machine_mode mode = GET_MODE (sign);
  rtx sgn = gen_reg_rtx (mode);
  if (mask == NULL_RTX)
    {
      mask = ix86_build_signbit_mask (mode, VECTOR_MODE_P (mode), false);
      if (!VECTOR_MODE_P (mode))
	{
	  /* We need to generate a scalar mode mask in this case.  */
	  rtx tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, const0_rtx));
	  tmp = gen_rtx_VEC_SELECT (mode, mask, tmp);
	  mask = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (VOIDmode, mask, tmp));
	}
    }
  else
    mask = gen_rtx_NOT (mode, mask);
  emit_insn (gen_rtx_SET (VOIDmode, sgn,
			  gen_rtx_AND (mode, mask, sign)));
  emit_insn (gen_rtx_SET (VOIDmode, result,
			  gen_rtx_IOR (mode, abs_value, sgn)));
}

/* Expand fabs (OP0) and return a new rtx that holds the result.  The
   mask for masking out the sign-bit is stored in *SMASK, if that is
   non-null.  */
static rtx
ix86_expand_sse_fabs (rtx op0, rtx *smask)
{
  enum machine_mode mode = GET_MODE (op0);
  rtx xa, mask;

  xa = gen_reg_rtx (mode);
  mask = ix86_build_signbit_mask (mode, VECTOR_MODE_P (mode), true);
  if (!VECTOR_MODE_P (mode))
    {
      /* We need to generate a scalar mode mask in this case.  */
      rtx tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, const0_rtx));
      tmp = gen_rtx_VEC_SELECT (mode, mask, tmp);
      mask = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (VOIDmode, mask, tmp));
    }
  emit_insn (gen_rtx_SET (VOIDmode, xa,
			  gen_rtx_AND (mode, op0, mask)));

  if (smask)
    *smask = mask;

  return xa;
}

/* Expands a comparison of OP0 with OP1 using comparison code CODE,
   swapping the operands if SWAP_OPERANDS is true.  The expanded
   code is a forward jump to a newly created label in case the
   comparison is true.  The generated label rtx is returned.  */
static rtx
ix86_expand_sse_compare_and_jump (enum rtx_code code, rtx op0, rtx op1,
                                  bool swap_operands)
{
  rtx label, tmp;

  if (swap_operands)
    {
      tmp = op0;
      op0 = op1;
      op1 = tmp;
    }

  label = gen_label_rtx ();
  tmp = gen_rtx_REG (CCFPUmode, FLAGS_REG);
  emit_insn (gen_rtx_SET (VOIDmode, tmp,
			  gen_rtx_COMPARE (CCFPUmode, op0, op1)));
  tmp = gen_rtx_fmt_ee (code, VOIDmode, tmp, const0_rtx);
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
			      gen_rtx_LABEL_REF (VOIDmode, label), pc_rtx);
  tmp = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
  JUMP_LABEL (tmp) = label;

  return label;
}

/* Expand a mask generating SSE comparison instruction comparing OP0 with OP1
   using comparison code CODE.  Operands are swapped for the comparison if
   SWAP_OPERANDS is true.  Returns a rtx for the generated mask.  */
static rtx
ix86_expand_sse_compare_mask (enum rtx_code code, rtx op0, rtx op1,
			      bool swap_operands)
{
  enum machine_mode mode = GET_MODE (op0);
  rtx mask = gen_reg_rtx (mode);

  if (swap_operands)
    {
      rtx tmp = op0;
      op0 = op1;
      op1 = tmp;
    }

  if (mode == DFmode)
    emit_insn (gen_sse2_maskcmpdf3 (mask, op0, op1,
				    gen_rtx_fmt_ee (code, mode, op0, op1)));
  else
    emit_insn (gen_sse_maskcmpsf3 (mask, op0, op1,
				   gen_rtx_fmt_ee (code, mode, op0, op1)));

  return mask;
}

/* Generate and return a rtx of mode MODE for 2**n where n is the number
   of bits of the mantissa of MODE, which must be one of DFmode or SFmode.  */
static rtx
ix86_gen_TWO52 (enum machine_mode mode)
{
  REAL_VALUE_TYPE TWO52r;
  rtx TWO52;

  real_ldexp (&TWO52r, &dconst1, mode == DFmode ? 52 : 23);
  TWO52 = const_double_from_real_value (TWO52r, mode);
  TWO52 = force_reg (mode, TWO52);

  return TWO52;
}

/* Expand SSE sequence for computing lround from OP1 storing
   into OP0.  */
void
ix86_expand_lround (rtx op0, rtx op1)
{
  /* C code for the stuff we're doing below:
       tmp = op1 + copysign (nextafter (0.5, 0.0), op1)
       return (long)tmp;
   */
  enum machine_mode mode = GET_MODE (op1);
  const struct real_format *fmt;
  REAL_VALUE_TYPE pred_half, half_minus_pred_half;
  rtx adj;

  /* load nextafter (0.5, 0.0) */
  fmt = REAL_MODE_FORMAT (mode);
  real_2expN (&half_minus_pred_half, -(fmt->p) - 1, mode);
  REAL_ARITHMETIC (pred_half, MINUS_EXPR, dconsthalf, half_minus_pred_half);

  /* adj = copysign (0.5, op1) */
  adj = force_reg (mode, const_double_from_real_value (pred_half, mode));
  ix86_sse_copysign_to_positive (adj, adj, force_reg (mode, op1), NULL_RTX);

  /* adj = op1 + adj */
  adj = expand_simple_binop (mode, PLUS, adj, op1, NULL_RTX, 0, OPTAB_DIRECT);

  /* op0 = (imode)adj */
  expand_fix (op0, adj, 0);
}

/* Expand SSE2 sequence for computing lround from OPERAND1 storing
   into OPERAND0.  */
void
ix86_expand_lfloorceil (rtx op0, rtx op1, bool do_floor)
{
  /* C code for the stuff we're doing below (for do_floor):
	xi = (long)op1;
        xi -= (double)xi > op1 ? 1 : 0;
        return xi;
   */
  enum machine_mode fmode = GET_MODE (op1);
  enum machine_mode imode = GET_MODE (op0);
  rtx ireg, freg, label, tmp;

  /* reg = (long)op1 */
  ireg = gen_reg_rtx (imode);
  expand_fix (ireg, op1, 0);

  /* freg = (double)reg */
  freg = gen_reg_rtx (fmode);
  expand_float (freg, ireg, 0);

  /* ireg = (freg > op1) ? ireg - 1 : ireg */
  label = ix86_expand_sse_compare_and_jump (UNLE,
					    freg, op1, !do_floor);
  tmp = expand_simple_binop (imode, do_floor ? MINUS : PLUS,
			     ireg, const1_rtx, NULL_RTX, 0, OPTAB_DIRECT);
  emit_move_insn (ireg, tmp);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (op0, ireg);
}

/* Expand rint (IEEE round to nearest) rounding OPERAND1 and storing the
   result in OPERAND0.  */
void
ix86_expand_rint (rtx operand0, rtx operand1)
{
  /* C code for the stuff we're doing below:
	xa = fabs (operand1);
        if (!isless (xa, 2**52))
	  return operand1;
        xa = xa + 2**52 - 2**52;
        return copysign (xa, operand1);
   */
  enum machine_mode mode = GET_MODE (operand0);
  rtx res, xa, label, TWO52, mask;

  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  TWO52 = ix86_gen_TWO52 (mode);
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  xa = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa = expand_simple_binop (mode, MINUS, xa, TWO52, xa, 0, OPTAB_DIRECT);

  ix86_sse_copysign_to_positive (res, xa, res, mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE2 sequence for computing floor or ceil from OPERAND1 storing
   into OPERAND0.  */
void
ix86_expand_floorceildf_32 (rtx operand0, rtx operand1, bool do_floor)
{
  /* C code for the stuff we expand below.
        double xa = fabs (x), x2;
        if (!isless (xa, TWO52))
          return x;
        xa = xa + TWO52 - TWO52;
        x2 = copysign (xa, x);
     Compensate.  Floor:
        if (x2 > x)
          x2 -= 1;
     Compensate.  Ceil:
        if (x2 < x)
          x2 -= -1;
        return x2;
   */
  enum machine_mode mode = GET_MODE (operand0);
  rtx xa, TWO52, tmp, label, one, res, mask;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa = xa + TWO52 - TWO52; */
  xa = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa = expand_simple_binop (mode, MINUS, xa, TWO52, xa, 0, OPTAB_DIRECT);

  /* xa = copysign (xa, operand1) */
  ix86_sse_copysign_to_positive (xa, xa, res, mask);

  /* generate 1.0 or -1.0 */
  one = force_reg (mode,
	           const_double_from_real_value (do_floor
						 ? dconst1 : dconstm1, mode));

  /* Compensate: xa = xa - (xa > operand1 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGT, xa, res, !do_floor);
  emit_insn (gen_rtx_SET (VOIDmode, tmp,
                          gen_rtx_AND (mode, one, tmp)));
  /* We always need to subtract here to preserve signed zero.  */
  tmp = expand_simple_binop (mode, MINUS,
			     xa, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  emit_move_insn (res, tmp);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE2 sequence for computing floor or ceil from OPERAND1 storing
   into OPERAND0.  */
void
ix86_expand_floorceil (rtx operand0, rtx operand1, bool do_floor)
{
  /* C code for the stuff we expand below.
	double xa = fabs (x), x2;
        if (!isless (xa, TWO52))
          return x;
	x2 = (double)(long)x;
     Compensate.  Floor:
	if (x2 > x)
	  x2 -= 1;
     Compensate.  Ceil:
	if (x2 < x)
	  x2 += 1;
	if (HONOR_SIGNED_ZEROS (mode))
	  return copysign (x2, x);
	return x2;
   */
  enum machine_mode mode = GET_MODE (operand0);
  rtx xa, xi, TWO52, tmp, label, one, res, mask;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa = (double)(long)x */
  xi = gen_reg_rtx (mode == DFmode ? DImode : SImode);
  expand_fix (xi, res, 0);
  expand_float (xa, xi, 0);

  /* generate 1.0 */
  one = force_reg (mode, const_double_from_real_value (dconst1, mode));

  /* Compensate: xa = xa - (xa > operand1 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGT, xa, res, !do_floor);
  emit_insn (gen_rtx_SET (VOIDmode, tmp,
                          gen_rtx_AND (mode, one, tmp)));
  tmp = expand_simple_binop (mode, do_floor ? MINUS : PLUS,
			     xa, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  emit_move_insn (res, tmp);

  if (HONOR_SIGNED_ZEROS (mode))
    ix86_sse_copysign_to_positive (res, res, force_reg (mode, operand1), mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing round from OPERAND1 storing
   into OPERAND0.  Sequence that works without relying on DImode truncation
   via cvttsd2siq that is only available on 64bit targets.  */
void
ix86_expand_rounddf_32 (rtx operand0, rtx operand1)
{
  /* C code for the stuff we expand below.
        double xa = fabs (x), xa2, x2;
        if (!isless (xa, TWO52))
          return x;
     Using the absolute value and copying back sign makes
     -0.0 -> -0.0 correct.
        xa2 = xa + TWO52 - TWO52;
     Compensate.
	dxa = xa2 - xa;
        if (dxa <= -0.5)
          xa2 += 1;
        else if (dxa > 0.5)
          xa2 -= 1;
        x2 = copysign (xa2, x);
        return x2;
   */
  enum machine_mode mode = GET_MODE (operand0);
  rtx xa, xa2, dxa, TWO52, tmp, label, half, mhalf, one, res, mask;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa2 = xa + TWO52 - TWO52; */
  xa2 = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa2 = expand_simple_binop (mode, MINUS, xa2, TWO52, xa2, 0, OPTAB_DIRECT);

  /* dxa = xa2 - xa; */
  dxa = expand_simple_binop (mode, MINUS, xa2, xa, NULL_RTX, 0, OPTAB_DIRECT);

  /* generate 0.5, 1.0 and -0.5 */
  half = force_reg (mode, const_double_from_real_value (dconsthalf, mode));
  one = expand_simple_binop (mode, PLUS, half, half, NULL_RTX, 0, OPTAB_DIRECT);
  mhalf = expand_simple_binop (mode, MINUS, half, one, NULL_RTX,
			       0, OPTAB_DIRECT);

  /* Compensate.  */
  tmp = gen_reg_rtx (mode);
  /* xa2 = xa2 - (dxa > 0.5 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGT, dxa, half, false);
  emit_insn (gen_rtx_SET (VOIDmode, tmp,
                          gen_rtx_AND (mode, one, tmp)));
  xa2 = expand_simple_binop (mode, MINUS, xa2, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  /* xa2 = xa2 + (dxa <= -0.5 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGE, mhalf, dxa, false);
  emit_insn (gen_rtx_SET (VOIDmode, tmp,
                          gen_rtx_AND (mode, one, tmp)));
  xa2 = expand_simple_binop (mode, PLUS, xa2, tmp, NULL_RTX, 0, OPTAB_DIRECT);

  /* res = copysign (xa2, operand1) */
  ix86_sse_copysign_to_positive (res, xa2, force_reg (mode, operand1), mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing trunc from OPERAND1 storing
   into OPERAND0.  */
void
ix86_expand_trunc (rtx operand0, rtx operand1)
{
  /* C code for SSE variant we expand below.
        double xa = fabs (x), x2;
        if (!isless (xa, TWO52))
          return x;
        x2 = (double)(long)x;
	if (HONOR_SIGNED_ZEROS (mode))
	  return copysign (x2, x);
	return x2;
   */
  enum machine_mode mode = GET_MODE (operand0);
  rtx xa, xi, TWO52, label, res, mask;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* x = (double)(long)x */
  xi = gen_reg_rtx (mode == DFmode ? DImode : SImode);
  expand_fix (xi, res, 0);
  expand_float (res, xi, 0);

  if (HONOR_SIGNED_ZEROS (mode))
    ix86_sse_copysign_to_positive (res, res, force_reg (mode, operand1), mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing trunc from OPERAND1 storing
   into OPERAND0.  */
void
ix86_expand_truncdf_32 (rtx operand0, rtx operand1)
{
  enum machine_mode mode = GET_MODE (operand0);
  rtx xa, mask, TWO52, label, one, res, smask, tmp;

  /* C code for SSE variant we expand below.
        double xa = fabs (x), x2;
        if (!isless (xa, TWO52))
          return x;
        xa2 = xa + TWO52 - TWO52;
     Compensate:
        if (xa2 > xa)
          xa2 -= 1.0;
        x2 = copysign (xa2, x);
        return x2;
   */

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &smask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* res = xa + TWO52 - TWO52; */
  tmp = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  tmp = expand_simple_binop (mode, MINUS, tmp, TWO52, tmp, 0, OPTAB_DIRECT);
  emit_move_insn (res, tmp);

  /* generate 1.0 */
  one = force_reg (mode, const_double_from_real_value (dconst1, mode));

  /* Compensate: res = xa2 - (res > xa ? 1 : 0)  */
  mask = ix86_expand_sse_compare_mask (UNGT, res, xa, false);
  emit_insn (gen_rtx_SET (VOIDmode, mask,
                          gen_rtx_AND (mode, mask, one)));
  tmp = expand_simple_binop (mode, MINUS,
			     res, mask, NULL_RTX, 0, OPTAB_DIRECT);
  emit_move_insn (res, tmp);

  /* res = copysign (res, operand1) */
  ix86_sse_copysign_to_positive (res, res, force_reg (mode, operand1), smask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing round from OPERAND1 storing
   into OPERAND0.  */
void
ix86_expand_round (rtx operand0, rtx operand1)
{
  /* C code for the stuff we're doing below:
        double xa = fabs (x);
        if (!isless (xa, TWO52))
          return x;
        xa = (double)(long)(xa + nextafter (0.5, 0.0));
        return copysign (xa, x);
   */
  enum machine_mode mode = GET_MODE (operand0);
  rtx res, TWO52, xa, label, xi, half, mask;
  const struct real_format *fmt;
  REAL_VALUE_TYPE pred_half, half_minus_pred_half;

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = gen_reg_rtx (mode);
  emit_move_insn (res, operand1);

  TWO52 = ix86_gen_TWO52 (mode);
  xa = ix86_expand_sse_fabs (res, &mask);
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* load nextafter (0.5, 0.0) */
  fmt = REAL_MODE_FORMAT (mode);
  real_2expN (&half_minus_pred_half, -(fmt->p) - 1, mode);
  REAL_ARITHMETIC (pred_half, MINUS_EXPR, dconsthalf, half_minus_pred_half);

  /* xa = xa + 0.5 */
  half = force_reg (mode, const_double_from_real_value (pred_half, mode));
  xa = expand_simple_binop (mode, PLUS, xa, half, NULL_RTX, 0, OPTAB_DIRECT);

  /* xa = (double)(int64_t)xa */
  xi = gen_reg_rtx (mode == DFmode ? DImode : SImode);
  expand_fix (xi, xa, 0);
  expand_float (xa, xi, 0);

  /* res = copysign (xa, operand1) */
  ix86_sse_copysign_to_positive (res, xa, force_reg (mode, operand1), mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}


/* Validate whether a SSE5 instruction is valid or not.
   OPERANDS is the array of operands.
   NUM is the number of operands.
   USES_OC0 is true if the instruction uses OC0 and provides 4 variants.
   NUM_MEMORY is the maximum number of memory operands to accept.  */
bool
ix86_sse5_valid_op_p (rtx operands[], rtx insn, int num, bool uses_oc0, int num_memory)
{
  int mem_mask;
  int mem_count;
  int i;

  /* Count the number of memory arguments */
  mem_mask = 0;
  mem_count = 0;
  for (i = 0; i < num; i++)
    {
      enum machine_mode mode = GET_MODE (operands[i]);
      if (register_operand (operands[i], mode))
	;

      else if (memory_operand (operands[i], mode))
	{
	  mem_mask |= (1 << i);
	  mem_count++;
	}

      else
	{
	  rtx pattern = PATTERN (insn);

	  /* allow 0 for pcmov */
	  if (GET_CODE (pattern) != SET
	      || GET_CODE (SET_SRC (pattern)) != IF_THEN_ELSE
	      || i < 2
	      || operands[i] != CONST0_RTX (mode))
	    return false;
	}
    }

  /* If there were no memory operations, allow the insn */
  if (mem_mask == 0)
    return true;

  /* Do not allow the destination register to be a memory operand.  */
  else if (mem_mask & (1 << 0))
    return false;

  /* If there are too many memory operations, disallow the instruction.  While
     the hardware only allows 1 memory reference, before register allocation
     for some insns, we allow two memory operations sometimes in order to allow
     code like the following to be optimized:

	float fmadd (float *a, float *b, float *c) { return (*a * *b) + *c; }

    or similar cases that are vectorized into using the fmaddss
    instruction.  */
  else if (mem_count > num_memory)
    return false;

  /* Don't allow more than one memory operation if not optimizing.  */
  else if (mem_count > 1 && !optimize)
    return false;

  else if (num == 4 && mem_count == 1)
    {
      /* formats (destination is the first argument), example fmaddss:
	 xmm1, xmm1, xmm2, xmm3/mem
	 xmm1, xmm1, xmm2/mem, xmm3
	 xmm1, xmm2, xmm3/mem, xmm1
	 xmm1, xmm2/mem, xmm3, xmm1 */
      if (uses_oc0)
	return ((mem_mask == (1 << 1))
		|| (mem_mask == (1 << 2))
		|| (mem_mask == (1 << 3)));

      /* format, example pmacsdd:
	 xmm1, xmm2, xmm3/mem, xmm1 */
      else
	return (mem_mask == (1 << 2));
    }

  else if (num == 4 && num_memory == 2)
    {
      /* If there are two memory operations, we can load one of the memory ops
	 into the destination register.  This is for optimizing the
	 multiply/add ops, which the combiner has optimized both the multiply
	 and the add insns to have a memory operation.  We have to be careful
	 that the destination doesn't overlap with the inputs.  */
      rtx op0 = operands[0];

      if (reg_mentioned_p (op0, operands[1])
	  || reg_mentioned_p (op0, operands[2])
	  || reg_mentioned_p (op0, operands[3]))
	return false;

      /* formats (destination is the first argument), example fmaddss:
	 xmm1, xmm1, xmm2, xmm3/mem
	 xmm1, xmm1, xmm2/mem, xmm3
	 xmm1, xmm2, xmm3/mem, xmm1
	 xmm1, xmm2/mem, xmm3, xmm1

         For the oc0 case, we will load either operands[1] or operands[3] into
         operands[0], so any combination of 2 memory operands is ok.  */
      if (uses_oc0)
	return true;

      /* format, example pmacsdd:
	 xmm1, xmm2, xmm3/mem, xmm1

         For the integer multiply/add instructions be more restrictive and
         require operands[2] and operands[3] to be the memory operands.  */
      else
	return (mem_mask == ((1 << 2) | (1 << 3)));
    }

  else if (num == 3 && num_memory == 1)
    {
      /* formats, example protb:
	 xmm1, xmm2, xmm3/mem
	 xmm1, xmm2/mem, xmm3 */
      if (uses_oc0)
	return ((mem_mask == (1 << 1)) || (mem_mask == (1 << 2)));

      /* format, example comeq:
	 xmm1, xmm2, xmm3/mem */
      else
	return (mem_mask == (1 << 2));
    }

  else
    gcc_unreachable ();

  return false;
}


/* Fixup an SSE5 instruction that has 2 memory input references into a form the
   hardware will allow by using the destination register to load one of the
   memory operations.  Presently this is used by the multiply/add routines to
   allow 2 memory references.  */

void
ix86_expand_sse5_multiple_memory (rtx operands[],
				  int num,
				  enum machine_mode mode)
{
  rtx op0 = operands[0];
  if (num != 4
      || memory_operand (op0, mode)
      || reg_mentioned_p (op0, operands[1])
      || reg_mentioned_p (op0, operands[2])
      || reg_mentioned_p (op0, operands[3]))
    gcc_unreachable ();

  /* For 2 memory operands, pick either operands[1] or operands[3] to move into
     the destination register.  */
  if (memory_operand (operands[1], mode))
    {
      emit_move_insn (op0, operands[1]);
      operands[1] = op0;
    }
  else if (memory_operand (operands[3], mode))
    {
      emit_move_insn (op0, operands[3]);
      operands[3] = op0;
    }
  else
    gcc_unreachable ();

  return;
}


/* Table of valid machine attributes.  */
static const struct attribute_spec ix86_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Stdcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "stdcall",   0, 0, false, true,  true,  ix86_handle_cconv_attribute },
  /* Fastcall attribute says callee is responsible for popping arguments
     if they are not variable.  */
  { "fastcall",  0, 0, false, true,  true,  ix86_handle_cconv_attribute },
  /* Cdecl attribute says the callee is a normal C declaration */
  { "cdecl",     0, 0, false, true,  true,  ix86_handle_cconv_attribute },
  /* Regparm attribute specifies how many integer arguments are to be
     passed in registers.  */
  { "regparm",   1, 1, false, true,  true,  ix86_handle_cconv_attribute },
  /* Sseregparm attribute says we are using x86_64 calling conventions
     for FP arguments.  */
  { "sseregparm", 0, 0, false, true, true, ix86_handle_cconv_attribute },
  /* force_align_arg_pointer says this function realigns the stack at entry.  */
  { (const char *)&ix86_force_align_arg_pointer_string, 0, 0,
    false, true,  true, ix86_handle_cconv_attribute },
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
  { "dllimport", 0, 0, false, false, false, handle_dll_attribute },
  { "dllexport", 0, 0, false, false, false, handle_dll_attribute },
  { "shared",    0, 0, true,  false, false, ix86_handle_shared_attribute },
#endif
  { "ms_struct", 0, 0, false, false,  false, ix86_handle_struct_attribute },
  { "gcc_struct", 0, 0, false, false,  false, ix86_handle_struct_attribute },
#ifdef SUBTARGET_ATTRIBUTE_TABLE
  SUBTARGET_ATTRIBUTE_TABLE,
#endif
  { NULL,        0, 0, false, false, false, NULL }
};

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
x86_builtin_vectorization_cost (bool runtime_test)
{
  /* If the branch of the runtime test is taken - i.e. - the vectorized
     version is skipped - this incurs a misprediction cost (because the
     vectorized version is expected to be the fall-through).  So we subtract
     the latency of a mispredicted branch from the costs that are incured
     when the vectorized version is executed.

     TODO: The values in individual target tables have to be tuned or new
     fields may be needed. For eg. on K8, the default branch path is the
     not-taken path. If the taken path is predicted correctly, the minimum
     penalty of going down the taken-path is 1 cycle. If the taken-path is
     not predicted correctly, then the minimum penalty is 10 cycles.  */

  if (runtime_test)
    {
      return (-(ix86_cost->cond_taken_branch_cost));
    }
  else
    return 0;
}

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE ix86_attribute_table
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#  undef TARGET_MERGE_DECL_ATTRIBUTES
#  define TARGET_MERGE_DECL_ATTRIBUTES merge_dllimport_decl_attributes
#endif

#undef TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES ix86_comp_type_attributes

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS ix86_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN ix86_expand_builtin

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  ix86_builtin_vectorized_function

#undef TARGET_VECTORIZE_BUILTIN_CONVERSION
#define TARGET_VECTORIZE_BUILTIN_CONVERSION ix86_vectorize_builtin_conversion

#undef TARGET_BUILTIN_RECIPROCAL
#define TARGET_BUILTIN_RECIPROCAL ix86_builtin_reciprocal

#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE ix86_output_function_epilogue

#undef TARGET_ENCODE_SECTION_INFO
#ifndef SUBTARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO ix86_encode_section_info
#else
#define TARGET_ENCODE_SECTION_INFO SUBTARGET_ENCODE_SECTION_INFO
#endif

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
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  ia32_multipass_dfa_lookahead

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL ix86_function_ok_for_sibcall

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif
#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM ix86_cannot_force_const_mem
#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P hook_bool_mode_const_rtx_true

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS ix86_delegitimize_address

#undef TARGET_MS_BITFIELD_LAYOUT_P
#define TARGET_MS_BITFIELD_LAYOUT_P ix86_ms_bitfield_layout_p

#if TARGET_MACHO
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P darwin_binds_local_p
#endif
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P i386_pe_binds_local_p
#endif

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK x86_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK x86_can_output_mi_thunk

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START x86_file_start

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS	\
  (TARGET_DEFAULT			\
   | TARGET_SUBTARGET_DEFAULT		\
   | TARGET_TLS_DIRECT_SEG_REFS_DEFAULT)

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION ix86_handle_option

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS ix86_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST ix86_address_cost

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS ix86_fixed_condition_code_regs
#undef TARGET_CC_MODES_COMPATIBLE
#define TARGET_CC_MODES_COMPATIBLE ix86_cc_modes_compatible

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG ix86_reorg

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST ix86_build_builtin_va_list

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START ix86_va_start

#undef TARGET_MD_ASM_CLOBBERS
#define TARGET_MD_ASM_CLOBBERS ix86_md_asm_clobbers

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX ix86_struct_value_rtx
#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS ix86_setup_incoming_varargs
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK ix86_must_pass_in_stack
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE ix86_pass_by_reference
#undef TARGET_INTERNAL_ARG_POINTER
#define TARGET_INTERNAL_ARG_POINTER ix86_internal_arg_pointer
#undef TARGET_DWARF_HANDLE_FRAME_UNSPEC
#define TARGET_DWARF_HANDLE_FRAME_UNSPEC ix86_dwarf_handle_frame_unspec
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR ix86_gimplify_va_arg

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P ix86_scalar_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P ix86_vector_mode_supported_p

#undef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX ix86_c_mode_for_suffix

#ifdef HAVE_AS_TLS
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL i386_output_dwarf_dtprel
#endif

#ifdef SUBTARGET_INSERT_ATTRIBUTES
#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES SUBTARGET_INSERT_ATTRIBUTES
#endif

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE ix86_mangle_type

#undef TARGET_STACK_PROTECT_FAIL
#define TARGET_STACK_PROTECT_FAIL ix86_stack_protect_fail

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE ix86_function_value

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST x86_builtin_vectorization_cost

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-i386.h"
