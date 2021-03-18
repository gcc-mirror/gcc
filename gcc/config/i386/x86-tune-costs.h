/* Costs of operations of individual x86 CPUs.
   Copyright (C) 1988-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */
/* Processor costs (relative to an add) */
/* We assume COSTS_N_INSNS is defined as (N)*4 and an addition is 2 bytes.  */
#define COSTS_N_BYTES(N) ((N) * 2)

#define DUMMY_STRINGOP_ALGS {libcall, {{-1, libcall, false}}}

static stringop_algs ix86_size_memcpy[2] = {
  {rep_prefix_1_byte, {{-1, rep_prefix_1_byte, false}}},
  {rep_prefix_1_byte, {{-1, rep_prefix_1_byte, false}}}};
static stringop_algs ix86_size_memset[2] = {
  {rep_prefix_1_byte, {{-1, rep_prefix_1_byte, false}}},
  {rep_prefix_1_byte, {{-1, rep_prefix_1_byte, false}}}};

const
struct processor_costs ix86_size_cost = {/* costs for tuning for size */
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  2,				     /* cost for loading QImode using movzbl */
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
  3, 3, 3,				/* cost of moving XMM,YMM,ZMM register */
  {3, 3, 3, 3, 3},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {3, 3, 3, 3, 3},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_BYTES (2),			/* cost of an add instruction */
  COSTS_N_BYTES (3),			/* cost of a lea instruction */
  COSTS_N_BYTES (2),			/* variable shift costs */
  COSTS_N_BYTES (3),			/* constant shift costs */
  {COSTS_N_BYTES (3),			/* cost of starting multiply for QI */
   COSTS_N_BYTES (3),			/*				 HI */
   COSTS_N_BYTES (3),			/*				 SI */
   COSTS_N_BYTES (3),			/*				 DI */
   COSTS_N_BYTES (5)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_BYTES (3),			/* cost of a divide/mod for QI */
   COSTS_N_BYTES (3),			/*			    HI */
   COSTS_N_BYTES (3),			/*			    SI */
   COSTS_N_BYTES (3),			/*			    DI */
   COSTS_N_BYTES (5)},			/*			    other */
  COSTS_N_BYTES (3),			/* cost of movsx */
  COSTS_N_BYTES (3),			/* cost of movzx */
  0,					/* "large" insn */
  2,					/* MOVE_RATIO */
  2,					/* CLEAR_RATIO */
  {2, 2, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 2, 2},				/* cost of storing integer registers */
  {3, 3, 3, 3, 3},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {3, 3, 3, 3, 3},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {3, 3, 3, 3, 3},			/* cost of unaligned SSE load
					   in 128bit, 256bit and 512bit */
  {3, 3, 3, 3, 3},			/* cost of unaligned SSE store
					   in 128bit, 256bit and 512bit */
  3, 3, 3,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  5, 0,					/* Gather load static, per_elt.  */
  5, 0,					/* Gather store static, per_elt.  */
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

  COSTS_N_BYTES (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_BYTES (2),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_BYTES (2),			/* cost of MULSS instruction.  */
  COSTS_N_BYTES (2),			/* cost of MULSD instruction.  */
  COSTS_N_BYTES (2),			/* cost of FMA SS instruction.  */
  COSTS_N_BYTES (2),			/* cost of FMA SD instruction.  */
  COSTS_N_BYTES (2),			/* cost of DIVSS instruction.  */
  COSTS_N_BYTES (2),			/* cost of DIVSD instruction.  */
  COSTS_N_BYTES (2),			/* cost of SQRTSS instruction.  */
  COSTS_N_BYTES (2),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  ix86_size_memcpy,
  ix86_size_memset,
  COSTS_N_BYTES (1),			/* cond_taken_branch_cost.  */
  COSTS_N_BYTES (1),			/* cond_not_taken_branch_cost.  */
  NULL,					/* Loop alignment.  */
  NULL,					/* Jump alignment.  */
  NULL,					/* Label alignment.  */
  NULL,					/* Func alignment.  */
};

/* Processor costs (relative to an add) */
static stringop_algs i386_memcpy[2] = {
  {rep_prefix_1_byte, {{-1, rep_prefix_1_byte, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs i386_memset[2] = {
  {rep_prefix_1_byte, {{-1, rep_prefix_1_byte, false}}},
  DUMMY_STRINGOP_ALGS};

static const
struct processor_costs i386_cost = {	/* 386 specific costs */
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  4,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 8, 16, 32, 64},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (3),			/* variable shift costs */
  COSTS_N_INSNS (2),			/* constant shift costs */
  {COSTS_N_INSNS (6),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (6),			/*				 HI */
   COSTS_N_INSNS (6),			/*				 SI */
   COSTS_N_INSNS (6),			/*				 DI */
   COSTS_N_INSNS (6)},			/*			      other */
  COSTS_N_INSNS (1),			/* cost of multiply per each bit set */
  {COSTS_N_INSNS (23),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (23),			/*			    HI */
   COSTS_N_INSNS (23),			/*			    SI */
   COSTS_N_INSNS (23),			/*			    DI */
   COSTS_N_INSNS (23)},			/*			    other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
  15,					/* "large" insn */
  3,					/* MOVE_RATIO */
  3,					/* CLEAR_RATIO */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  {4, 8, 16, 32, 64},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of unaligned loads.  */
  {4, 8, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (23),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (27),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (27),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (27),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (27),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (88),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (88),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (122),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (122),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  i386_memcpy,
  i386_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "4",					/* Loop alignment.  */
  "4",					/* Jump alignment.  */
  NULL,					/* Label alignment.  */
  "4",					/* Func alignment.  */
};

static stringop_algs i486_memcpy[2] = {
  {rep_prefix_4_byte, {{-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs i486_memset[2] = {
  {rep_prefix_4_byte, {{-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};

static const
struct processor_costs i486_cost = {	/* 486 specific costs */
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  4,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 8, 16, 32, 64},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (3),			/* variable shift costs */
  COSTS_N_INSNS (2),			/* constant shift costs */
  {COSTS_N_INSNS (12),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (12),			/*				 HI */
   COSTS_N_INSNS (12),			/*				 SI */
   COSTS_N_INSNS (12),			/*				 DI */
   COSTS_N_INSNS (12)},			/*			      other */
  1,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (40),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (40),			/*			    HI */
   COSTS_N_INSNS (40),			/*			    SI */
   COSTS_N_INSNS (40),			/*			    DI */
   COSTS_N_INSNS (40)},			/*			    other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
  15,					/* "large" insn */
  3,					/* MOVE_RATIO */
  3,					/* CLEAR_RATIO */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  {4, 8, 16, 32, 64},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of unaligned loads.  */
  {4, 8, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (8),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (16),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (16),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (16),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (16),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (73),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (74),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (83),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (83),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  i486_memcpy,
  i486_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16",					/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

static stringop_algs pentium_memcpy[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs pentium_memset[2] = {
  {libcall, {{-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};

static const
struct processor_costs pentium_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 8, 16, 32, 64},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (4),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (11),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (11),			/*				 HI */
   COSTS_N_INSNS (11),			/*				 SI */
   COSTS_N_INSNS (11),			/*				 DI */
   COSTS_N_INSNS (11)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (25),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (25),			/*			    HI */
   COSTS_N_INSNS (25),			/*			    SI */
   COSTS_N_INSNS (25),			/*			    DI */
   COSTS_N_INSNS (25)},			/*			    other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
  8,					/* "large" insn */
  6,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  {4, 8, 16, 32, 64},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of unaligned loads.  */
  {4, 8, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (3),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (3),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (39),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (39),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (70),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (70),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  pentium_memcpy,
  pentium_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:8:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

static const
struct processor_costs lakemont_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 8, 16, 32, 64},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (11),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (11),			/*				 HI */
   COSTS_N_INSNS (11),			/*				 SI */
   COSTS_N_INSNS (11),			/*				 DI */
   COSTS_N_INSNS (11)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (25),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (25),			/*			    HI */
   COSTS_N_INSNS (25),			/*			    SI */
   COSTS_N_INSNS (25),			/*			    DI */
   COSTS_N_INSNS (25)},			/*			    other */
  COSTS_N_INSNS (3),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {2, 4, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 4, 2},				/* cost of storing integer registers */
  {4, 8, 16, 32, 64},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of unaligned loads.  */
  {4, 8, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (5),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (5),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (5),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (10),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (10),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (31),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (60),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (31),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (63),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  pentium_memcpy,
  pentium_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:8:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/* PentiumPro has optimized rep instructions for blocks aligned by 8 bytes
   (we ensure the alignment).  For small blocks inline loop is still a
   noticeable win, for bigger blocks either rep movsl or rep movsb is
   way to go.  Rep movsb has apparently more expensive startup time in CPU,
   but after 4K the difference is down in the noise.  */
static stringop_algs pentiumpro_memcpy[2] = {
  {rep_prefix_4_byte, {{128, loop, false}, {1024, unrolled_loop, false},
                       {8192, rep_prefix_4_byte, false},
                       {-1, rep_prefix_1_byte, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs pentiumpro_memset[2] = {
  {rep_prefix_4_byte, {{1024, unrolled_loop, false},
                       {8192, rep_prefix_4_byte, false},
                       {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static const
struct processor_costs pentiumpro_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  2,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 8, 16, 32, 64},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (4),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (4),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (4)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (17),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (17),			/*			    HI */
   COSTS_N_INSNS (17),			/*			    SI */
   COSTS_N_INSNS (17),			/*			    DI */
   COSTS_N_INSNS (17)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  6,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 2, 2},				/* cost of storing integer registers */
  {4, 8, 16, 32, 64},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 8, 16, 32, 64},			/* cost of unaligned loads.  */
  {4, 8, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (7),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (7),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (18),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (18),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (31),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (31),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  pentiumpro_memcpy,
  pentiumpro_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16:11:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

static stringop_algs geode_memcpy[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs geode_memset[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static const
struct processor_costs geode_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  2,				     /* cost for loading QImode using movzbl */
  {2, 2, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 2, 2},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {2, 2, 2},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 6, 6},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {2, 2},				/* cost of loading MMX registers
					   in SImode and DImode */
  {2, 2},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {2, 2, 8, 16, 32},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {2, 2, 8, 16, 32},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  6, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (2),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (7),			/*				 SI */
   COSTS_N_INSNS (7),			/*				 DI */
   COSTS_N_INSNS (7)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (15),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (23),			/*			    HI */
   COSTS_N_INSNS (39),			/*			    SI */
   COSTS_N_INSNS (39),			/*			    DI */
   COSTS_N_INSNS (39)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  4,					/* MOVE_RATIO */
  4,					/* CLEAR_RATIO */
  {2, 2, 2},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 2, 2},				/* cost of storing integer registers */
  {2, 2, 8, 16, 32},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {2, 2, 8, 16, 32},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {2, 2, 8, 16, 32},			/* cost of unaligned loads.  */
  {2, 2, 8, 16, 32},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  6,					/* cost of moving SSE register to integer.  */
  2, 2,					/* Gather load static, per_elt.  */
  2, 2,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (6),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (11),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (11),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (17),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (17),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (47),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (47),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (54),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (54),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  geode_memcpy,
  geode_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  NULL,					/* Loop alignment.  */
  NULL,					/* Jump alignment.  */
  NULL,					/* Label alignment.  */
  NULL,					/* Func alignment.  */
};

static stringop_algs k6_memcpy[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs k6_memset[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static const
struct processor_costs k6_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  3,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {2, 2, 8, 16, 32},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {2, 2, 8, 16, 32},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  6, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (3),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (3),			/*				 DI */
   COSTS_N_INSNS (3)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (18),			/*			    HI */
   COSTS_N_INSNS (18),			/*			    SI */
   COSTS_N_INSNS (18),			/*			    DI */
   COSTS_N_INSNS (18)},			/*			    other */
  COSTS_N_INSNS (2),			/* cost of movsx */
  COSTS_N_INSNS (2),			/* cost of movzx */
  8,					/* "large" insn */
  4,					/* MOVE_RATIO */
  4,					/* CLEAR_RATIO */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 3, 2},				/* cost of storing integer registers */
  {2, 2, 8, 16, 32},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {2, 2, 8, 16, 32},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {2, 2, 8, 16, 32},			/* cost of unaligned loads.  */
  {2, 2, 8, 16, 32},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  6,					/* cost of moving SSE register to integer.  */
  2, 2,					/* Gather load static, per_elt.  */
  2, 2,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (2),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (2),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (2),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (4),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (4),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (56),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (56),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (56),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (56),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  k6_memcpy,
  k6_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "32:8:8",				/* Loop alignment.  */
  "32:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "32",					/* Func alignment.  */
};

/* For some reason, Athlon deals better with REP prefix (relative to loops)
   compared to K8. Alignment becomes important after 8 bytes for memcpy and
   128 bytes for memset.  */
static stringop_algs athlon_memcpy[2] = {
  {libcall, {{2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs athlon_memset[2] = {
  {libcall, {{2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static const
struct processor_costs athlon_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  4,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 4, 12, 12, 24},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 4, 10, 10, 20},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  5, 5,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (5),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (5),			/*				 HI */
   COSTS_N_INSNS (5),			/*				 SI */
   COSTS_N_INSNS (5),			/*				 DI */
   COSTS_N_INSNS (5)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*			    HI */
   COSTS_N_INSNS (42),			/*			    SI */
   COSTS_N_INSNS (74),			/*			    DI */
   COSTS_N_INSNS (74)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {3, 4, 3},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {3, 4, 3},				/* cost of storing integer registers */
  {4, 4, 12, 12, 24},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 4, 10, 10, 20},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 4, 12, 12, 24},			/* cost of unaligned loads.  */
  {4, 4, 10, 10, 20},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  5,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (4),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (8),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (8),			/* cost of FMA SD instruction.  */
  /* 11-16  */
  COSTS_N_INSNS (16),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (24),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (19),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (19),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  athlon_memcpy,
  athlon_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:8:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/* K8 has optimized REP instruction for medium sized blocks, but for very
   small blocks it is better to use loop. For large blocks, libcall can
   do nontemporary accesses and beat inline considerably.  */
static stringop_algs k8_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs k8_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static const
struct processor_costs k8_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  4,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 3, 12, 12, 24},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 4, 10, 10, 20},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  5, 5,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (5)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*			    HI */
   COSTS_N_INSNS (42),			/*			    SI */
   COSTS_N_INSNS (74),			/*			    DI */
   COSTS_N_INSNS (74)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {3, 4, 3},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {3, 4, 3},				/* cost of storing integer registers */
  {4, 3, 12, 12, 24},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 4, 10, 10, 20},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 3, 12, 12, 24},			/* cost of unaligned loads.  */
  {4, 4, 10, 10, 20},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  5,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (4),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (8),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (8),			/* cost of FMA SD instruction.  */
  /* 11-16  */
  COSTS_N_INSNS (16),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (20),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (19),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (27),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  k8_memcpy,
  k8_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (2),			/* cond_not_taken_branch_cost.  */
  "16:8:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/* AMDFAM10 has optimized REP instruction for medium sized blocks, but for
   very small blocks it is better to use loop. For large blocks, libcall can
   do nontemporary accesses and beat inline considerably.  */
static stringop_algs amdfam10_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs amdfam10_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
struct processor_costs amdfam10_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  4,				     /* cost for loading QImode using movzbl */
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
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {4, 4, 3, 6, 12},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {4, 4, 5, 10, 20},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  3, 3,					/* SSE->integer and integer->SSE moves */

  					/* On K8:
  					    MOVD reg64, xmmreg Double FSTORE 4
					    MOVD reg32, xmmreg Double FSTORE 4
					   On AMDFAM10:
					    MOVD reg64, xmmreg Double FADD 3
							       1/1  1/1
					    MOVD reg32, xmmreg Double FADD 3
							       1/1  1/1 */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (5)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (19),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (35),			/*			    HI */
   COSTS_N_INSNS (51),			/*			    SI */
   COSTS_N_INSNS (83),			/*			    DI */
   COSTS_N_INSNS (83)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {3, 4, 3},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {3, 4, 3},				/* cost of storing integer registers */
  {4, 4, 3, 6, 12},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 4, 5, 10, 20},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {4, 4, 3, 7, 12},			/* cost of unaligned loads.  */
  {4, 4, 5, 10, 20},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  3,					/* cost of moving SSE register to integer.  */
  4, 4,					/* Gather load static, per_elt.  */
  4, 4,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (4),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (8),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (8),			/* cost of FMA SD instruction.  */
  /* 11-16  */
  COSTS_N_INSNS (16),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (20),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (19),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (27),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  amdfam10_memcpy,
  amdfam10_memset,
  COSTS_N_INSNS (2),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "32:25:8",				/* Loop alignment.  */
  "32:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "32",					/* Func alignment.  */
};

/*  BDVER has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop. For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */
static stringop_algs bdver_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs bdver_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};

const struct processor_costs bdver_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  8,				     /* cost for loading QImode using movzbl */
  {8, 8, 8},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {12, 12, 28},				/* cost of loading fp registers
		   			   in SFmode, DFmode and XFmode */
  {10, 10, 18},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode */
  4,					/* cost of moving MMX register */
  {12, 12},				/* cost of loading MMX registers
					   in SImode and DImode */
  {10, 10},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {12, 12, 10, 40, 60},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {10, 10, 10, 40, 60},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  16, 20,				/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (4),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (4),			/*				 SI */
   COSTS_N_INSNS (6),			/*				 DI */
   COSTS_N_INSNS (6)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (19),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (35),			/*			    HI */
   COSTS_N_INSNS (51),			/*			    SI */
   COSTS_N_INSNS (83),			/*			    DI */
   COSTS_N_INSNS (83)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {8, 8, 8},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer registers */
  {12, 12, 10, 40, 60},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {10, 10, 10, 40, 60},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {12, 12, 10, 40, 60},			/* cost of unaligned loads.  */
  {10, 10, 10, 40, 60},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  16,					/* cost of moving SSE register to integer.  */
  12, 12,				/* Gather load static, per_elt.  */
  10, 10,				/* Gather store static, per_elt.  */
  16,					/* size of l1 cache.  */
  2048,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  /* New AMD processors never drop prefetches; if they cannot be performed
     immediately, they are queued.  We set number of simultaneous prefetches
     to a large constant to reflect this (it probably is not a good idea not
     to limit number of prefetches at all, as their execution also takes some
     time).  */
  100,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (6),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (6),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (42),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (52),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (6),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (6),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (6),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SD instruction.  */
  /* 9-24  */
  COSTS_N_INSNS (24),			/* cost of DIVSS instruction.  */
  /* 9-27  */
  COSTS_N_INSNS (27),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (15),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (26),			/* cost of SQRTSD instruction.  */
  1, 2, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  bdver_memcpy,
  bdver_memset,
  COSTS_N_INSNS (4),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (2),			/* cond_not_taken_branch_cost.  */
  "16:11:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "11",					/* Func alignment.  */
};


/*  ZNVER1 has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop.  For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */
static stringop_algs znver1_memcpy[2] = {
  /* 32-bit tuning.  */
  {libcall, {{6, loop, false},
	     {14, unrolled_loop, false},
	     {-1, libcall, false}}},
  /* 64-bit tuning.  */
  {libcall, {{16, loop, false},
	     {128, rep_prefix_8_byte, false},
	     {-1, libcall, false}}}};
static stringop_algs znver1_memset[2] = {
  /* 32-bit tuning.  */
  {libcall, {{8, loop, false},
	     {24, unrolled_loop, false},
	     {128, rep_prefix_4_byte, false},
	     {-1, libcall, false}}},
  /* 64-bit tuning.  */
  {libcall, {{48, unrolled_loop, false},
	     {128, rep_prefix_8_byte, false},
	     {-1, libcall, false}}}};
struct processor_costs znver1_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */

  /* reg-reg moves are done by renaming and thus they are even cheaper than
     1 cycle. Becuase reg-reg move cost is 2 and the following tables correspond
     to doubles of latencies, we do not model this correctly.  It does not
     seem to make practical difference to bump prices up even more.  */
  6,					/* cost for loading QImode using
					   movzbl.  */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer
					   registers.  */
  2,					/* cost of reg,reg fld/fst.  */
  {6, 6, 16},				/* cost of loading fp registers
		   			   in SFmode, DFmode and XFmode.  */
  {8, 8, 16},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode.  */
  2,					/* cost of moving MMX register.  */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode.  */
  {8, 8},				/* cost of storing MMX registers
					   in SImode and DImode.  */
  2, 3, 6,				/* cost of moving XMM,YMM,ZMM register.  */
  {6, 6, 6, 12, 24},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit.  */
  {8, 8, 8, 16, 32},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit.  */
  6, 6,					/* SSE->integer and integer->SSE moves.  */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction.  */
  COSTS_N_INSNS (1),			/* cost of a lea instruction.  */
  COSTS_N_INSNS (1),			/* variable shift costs.  */
  COSTS_N_INSNS (1),			/* constant shift costs.  */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI.  */
   COSTS_N_INSNS (3),			/*				 HI.  */
   COSTS_N_INSNS (3),			/*				 SI.  */
   COSTS_N_INSNS (3),			/*				 DI.  */
   COSTS_N_INSNS (3)},			/*			      other.  */
  0,					/* cost of multiply per each bit
					    set.  */
   /* Depending on parameters, idiv can get faster on ryzen.  This is upper
      bound.  */
  {COSTS_N_INSNS (16),			/* cost of a divide/mod for QI.  */
   COSTS_N_INSNS (22),			/*			    HI.  */
   COSTS_N_INSNS (30),			/*			    SI.  */
   COSTS_N_INSNS (45),			/*			    DI.  */
   COSTS_N_INSNS (45)},			/*			    other.  */
  COSTS_N_INSNS (1),			/* cost of movsx.  */
  COSTS_N_INSNS (1),			/* cost of movzx.  */
  8,					/* "large" insn.  */
  9,					/* MOVE_RATIO.  */
  6,					/* CLEAR_RATIO */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer
					   registers.  */
  {6, 6, 6, 12, 24},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {8, 8, 8, 16, 32},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 12, 24},			/* cost of unaligned loads.  */
  {8, 8, 8, 16, 32},			/* cost of unaligned stores.  */
  2, 3, 6,				/* cost of moving XMM,YMM,ZMM register.  */
  6,					/* cost of moving SSE register to integer.  */
  /* VGATHERDPD is 23 uops and throughput is 9, VGATHERDPD is 35 uops,
     throughput 12.  Approx 9 uops do not depend on vector size and every load
     is 7 uops.  */
  18, 8,				/* Gather load static, per_elt.  */
  18, 10,				/* Gather store static, per_elt.  */
  32,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block.  */
  /* New AMD processors never drop prefetches; if they cannot be performed
     immediately, they are queued.  We set number of simultaneous prefetches
     to a large constant to reflect this (it probably is not a good idea not
     to limit number of prefetches at all, as their execution also takes some
     time).  */
  100,					/* number of parallel prefetches.  */
  3,					/* Branch cost.  */
  COSTS_N_INSNS (5),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  /* Latency of fdiv is 8-15.  */
  COSTS_N_INSNS (15),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  /* Latency of fsqrt is 4-10.  */
  COSTS_N_INSNS (10),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (3),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (10),			/* cost of DIVSS instruction.  */
  /* 9-13  */
  COSTS_N_INSNS (13),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (10),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (15),			/* cost of SQRTSD instruction.  */
  /* Zen can execute 4 integer operations per cycle. FP operations take 3 cycles
     and it can execute 2 integer additions and 2 multiplications thus
     reassociation may make sense up to with of 6.  SPEC2k6 bencharks suggests
     that 4 works better than 6 probably due to register pressure.

     Integer vector operations are taken by FP unit and execute 3 vector
     plus/minus operations per cycle but only one multiply.  This is adjusted
     in ix86_reassociation_width.  */
  4, 4, 3, 6,				/* reassoc int, fp, vec_int, vec_fp.  */
  znver1_memcpy,
  znver1_memset,
  COSTS_N_INSNS (4),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (2),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16",					/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/*  ZNVER2 has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop.  For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */
static stringop_algs znver2_memcpy[2] = {
  /* 32-bit tuning.  */
  {libcall, {{6, loop, false},
	     {14, unrolled_loop, false},
	     {-1, libcall, false}}},
  /* 64-bit tuning.  */
  {libcall, {{16, loop, false},
	     {64, rep_prefix_4_byte, false},
	     {-1, libcall, false}}}};
static stringop_algs znver2_memset[2] = {
  /* 32-bit tuning.  */
  {libcall, {{8, loop, false},
	     {24, unrolled_loop, false},
	     {128, rep_prefix_4_byte, false},
	     {-1, libcall, false}}},
  /* 64-bit tuning.  */
  {libcall, {{24, rep_prefix_4_byte, false},
	     {128, rep_prefix_8_byte, false},
	     {-1, libcall, false}}}};

struct processor_costs znver2_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */

  /* reg-reg moves are done by renaming and thus they are even cheaper than
     1 cycle.  Because reg-reg move cost is 2 and following tables correspond
     to doubles of latencies, we do not model this correctly.  It does not
     seem to make practical difference to bump prices up even more.  */
  6,					/* cost for loading QImode using
					   movzbl.  */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer
					   registers.  */
  2,					/* cost of reg,reg fld/fst.  */
  {6, 6, 16},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode.  */
  {8, 8, 16},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode.  */
  2,					/* cost of moving MMX register.  */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode.  */
  {8, 8},				/* cost of storing MMX registers
					   in SImode and DImode.  */
  2, 2, 3,				/* cost of moving XMM,YMM,ZMM
					   register.  */
  {6, 6, 6, 6, 12},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit.  */
  {8, 8, 8, 8, 16},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit.  */
  6, 6,					/* SSE->integer and integer->SSE
					   moves.  */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction.  */
  COSTS_N_INSNS (1),			/* cost of a lea instruction.  */
  COSTS_N_INSNS (1),			/* variable shift costs.  */
  COSTS_N_INSNS (1),			/* constant shift costs.  */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI.  */
   COSTS_N_INSNS (3),			/* 				 HI.  */
   COSTS_N_INSNS (3),			/*				 SI.  */
   COSTS_N_INSNS (3),			/*				 DI.  */
   COSTS_N_INSNS (3)},			/*			other.  */
  0,					/* cost of multiply per each bit
					   set.  */
   /* Depending on parameters, idiv can get faster on ryzen.  This is upper
      bound.  */
  {COSTS_N_INSNS (16),			/* cost of a divide/mod for QI.  */
   COSTS_N_INSNS (22),			/* 			    HI.  */
   COSTS_N_INSNS (30),			/*			    SI.  */
   COSTS_N_INSNS (45),			/*			    DI.  */
   COSTS_N_INSNS (45)},			/*			    other.  */
  COSTS_N_INSNS (1),			/* cost of movsx.  */
  COSTS_N_INSNS (1),			/* cost of movzx.  */
  8,					/* "large" insn.  */
  9,					/* MOVE_RATIO.  */
  6,					/* CLEAR_RATIO */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer
					   registers.  */
  {6, 6, 6, 6, 12},			/* cost of loading SSE registers
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {8, 8, 8, 8, 16},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 6, 12},			/* cost of unaligned loads.  */
  {8, 8, 8, 8, 16},			/* cost of unaligned stores.  */
  2, 2, 3,				/* cost of moving XMM,YMM,ZMM
					   register.  */
  6,					/* cost of moving SSE register to integer.  */
  /* VGATHERDPD is 23 uops and throughput is 9, VGATHERDPD is 35 uops,
     throughput 12.  Approx 9 uops do not depend on vector size and every load
     is 7 uops.  */
  18, 8,				/* Gather load static, per_elt.  */
  18, 10,				/* Gather store static, per_elt.  */
  32,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block.  */
  /* New AMD processors never drop prefetches; if they cannot be performed
     immediately, they are queued.  We set number of simultaneous prefetches
     to a large constant to reflect this (it probably is not a good idea not
     to limit number of prefetches at all, as their execution also takes some
     time).  */
  100,					/* number of parallel prefetches.  */
  3,					/* Branch cost.  */
  COSTS_N_INSNS (5),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  /* Latency of fdiv is 8-15.  */
  COSTS_N_INSNS (15),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  /* Latency of fsqrt is 4-10.  */
  COSTS_N_INSNS (10),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (3),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (3),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (10),			/* cost of DIVSS instruction.  */
  /* 9-13.  */
  COSTS_N_INSNS (13),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (10),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (15),			/* cost of SQRTSD instruction.  */
  /* Zen can execute 4 integer operations per cycle.  FP operations
     take 3 cycles and it can execute 2 integer additions and 2
     multiplications thus reassociation may make sense up to with of 6.
     SPEC2k6 bencharks suggests
     that 4 works better than 6 probably due to register pressure.

     Integer vector operations are taken by FP unit and execute 3 vector
     plus/minus operations per cycle but only one multiply.  This is adjusted
     in ix86_reassociation_width.  */
  4, 4, 3, 6,				/* reassoc int, fp, vec_int, vec_fp.  */
  znver2_memcpy,
  znver2_memset,
  COSTS_N_INSNS (4),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (2),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16",					/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

struct processor_costs znver3_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */

  /* reg-reg moves are done by renaming and thus they are even cheaper than
     1 cycle.  Because reg-reg move cost is 2 and following tables correspond
     to doubles of latencies, we do not model this correctly.  It does not
     seem to make practical difference to bump prices up even more.  */
  6,					/* cost for loading QImode using
					   movzbl.  */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer
					   registers.  */
  2,					/* cost of reg,reg fld/fst.  */
  {6, 6, 16},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode.  */
  {8, 8, 16},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode.  */
  2,					/* cost of moving MMX register.  */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode.  */
  {8, 8},				/* cost of storing MMX registers
					   in SImode and DImode.  */
  2, 2, 3,				/* cost of moving XMM,YMM,ZMM
					   register.  */
  {6, 6, 6, 6, 12},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit.  */
  {8, 8, 8, 8, 16},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit.  */
  6, 6,					/* SSE->integer and integer->SSE
					   moves.  */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction.  */
  COSTS_N_INSNS (1),			/* cost of a lea instruction.  */
  COSTS_N_INSNS (1),			/* variable shift costs.  */
  COSTS_N_INSNS (1),			/* constant shift costs.  */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI.  */
   COSTS_N_INSNS (3),			/* 				 HI.  */
   COSTS_N_INSNS (3),			/*				 SI.  */
   COSTS_N_INSNS (3),			/*				 DI.  */
   COSTS_N_INSNS (3)},			/*			other.  */
  0,					/* cost of multiply per each bit
					   set.  */
  {COSTS_N_INSNS (9),			/* cost of a divide/mod for QI.  */
   COSTS_N_INSNS (10),			/* 			    HI.  */
   COSTS_N_INSNS (12),			/*			    SI.  */
   COSTS_N_INSNS (17),			/*			    DI.  */
   COSTS_N_INSNS (17)},			/*			    other.  */
  COSTS_N_INSNS (1),			/* cost of movsx.  */
  COSTS_N_INSNS (1),			/* cost of movzx.  */
  8,					/* "large" insn.  */
  9,					/* MOVE_RATIO.  */
  6,					/* CLEAR_RATIO */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 8},				/* cost of storing integer
					   registers.  */
  {6, 6, 6, 6, 12},			/* cost of loading SSE registers
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {8, 8, 8, 8, 16},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 6, 12},			/* cost of unaligned loads.  */
  {8, 8, 8, 8, 16},			/* cost of unaligned stores.  */
  2, 2, 3,				/* cost of moving XMM,YMM,ZMM
					   register.  */
  6,					/* cost of moving SSE register to integer.  */
  /* VGATHERDPD is 23 uops and throughput is 9, VGATHERDPD is 35 uops,
     throughput 12.  Approx 9 uops do not depend on vector size and every load
     is 7 uops.  */
  18, 8,				/* Gather load static, per_elt.  */
  18, 10,				/* Gather store static, per_elt.  */
  32,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block.  */
  /* New AMD processors never drop prefetches; if they cannot be performed
     immediately, they are queued.  We set number of simultaneous prefetches
     to a large constant to reflect this (it probably is not a good idea not
     to limit number of prefetches at all, as their execution also takes some
     time).  */
  100,					/* number of parallel prefetches.  */
  3,					/* Branch cost.  */
  COSTS_N_INSNS (5),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  /* Latency of fdiv is 8-15.  */
  COSTS_N_INSNS (15),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  /* Latency of fsqrt is 4-10.  */
  COSTS_N_INSNS (10),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (3),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (3),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (10),			/* cost of DIVSS instruction.  */
  /* 9-13.  */
  COSTS_N_INSNS (13),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (10),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (15),			/* cost of SQRTSD instruction.  */
  /* Zen can execute 4 integer operations per cycle.  FP operations
     take 3 cycles and it can execute 2 integer additions and 2
     multiplications thus reassociation may make sense up to with of 6.
     SPEC2k6 bencharks suggests
     that 4 works better than 6 probably due to register pressure.

     Integer vector operations are taken by FP unit and execute 3 vector
     plus/minus operations per cycle but only one multiply.  This is adjusted
     in ix86_reassociation_width.  */
  4, 4, 3, 6,				/* reassoc int, fp, vec_int, vec_fp.  */
  znver2_memcpy,
  znver2_memset,
  COSTS_N_INSNS (4),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (2),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16",					/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/* skylake_cost should produce code tuned for Skylake familly of CPUs.  */
static stringop_algs skylake_memcpy[2] =   {
  {libcall, {{1024, rep_prefix_4_byte, true}, {-1, libcall, false}}},
  {libcall, {{16, loop, false}, {512, unrolled_loop, false},
             {-1, libcall, false}}}};

static stringop_algs skylake_memset[2] = {
  {libcall, {{6, loop_1_byte, true},
             {24, loop, true},
             {8192, rep_prefix_4_byte, true},
             {-1, libcall, false}}},
  {libcall, {{24, loop, true}, {512, unrolled_loop, false},
             {-1, libcall, false}}}};

static const
struct processor_costs skylake_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,				     /* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {6, 6, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 10},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode */
  {6, 6},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 2, 4,				/* cost of moving XMM,YMM,ZMM register */
  {6, 6, 6, 10, 20},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {8, 8, 8, 12, 24},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  6, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1)+1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (3),			/*				 DI */
   COSTS_N_INSNS (3)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  /* Expanding div/mod currently doesn't consider parallelism. So the cost
     model is not realistic. We compensate by increasing the latencies a bit.  */
  {COSTS_N_INSNS (11),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (11),			/*			    HI */
   COSTS_N_INSNS (14),			/*			    SI */
   COSTS_N_INSNS (76),			/*			    DI */
   COSTS_N_INSNS (76)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (0),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  {6, 6, 6, 10, 20},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {8, 8, 8, 12, 24},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 10, 20},			/* cost of unaligned loads.  */
  {8, 8, 8, 8, 16},			/* cost of unaligned stores.  */
  2, 2, 4,				/* cost of moving XMM,YMM,ZMM register */
  2,					/* cost of moving SSE register to integer.  */
  20, 8,				/* Gather load static, per_elt.  */
  22, 10,				/* Gather store static, per_elt.  */
  64,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  3,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (4),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (20),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (20),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (4),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (4),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (4),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (11),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (14),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (12),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (18),			/* cost of SQRTSD instruction.  */
  1, 4, 2, 2,				/* reassoc int, fp, vec_int, vec_fp.  */
  skylake_memcpy,
  skylake_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:11:8",				/* Loop alignment.  */
  "16:11:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};
  /* BTVER1 has optimized REP instruction for medium sized blocks, but for
     very small blocks it is better to use loop. For large blocks, libcall can
     do nontemporary accesses and beat inline considerably.  */
static stringop_algs btver1_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs btver1_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
const struct processor_costs btver1_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  8,				     /* cost for loading QImode using movzbl */
  {6, 8, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 8, 6},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {12, 12, 28},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {12, 12, 38},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  4,					/* cost of moving MMX register */
  {10, 10},				/* cost of loading MMX registers
					   in SImode and DImode */
  {12, 12},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {10, 10, 12, 48, 96},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {10, 10, 12, 48, 96},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  14, 14,				/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (5)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (19),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (35),			/*			    HI */
   COSTS_N_INSNS (51),			/*			    SI */
   COSTS_N_INSNS (83),			/*			    DI */
   COSTS_N_INSNS (83)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {6, 8, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 8, 6},				/* cost of storing integer registers */
  {10, 10, 12, 48, 96},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {10, 10, 12, 48, 96},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {10, 10, 12, 48, 96},			/* cost of unaligned loads.  */
  {10, 10, 12, 48, 96},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  14,					/* cost of moving SSE register to integer.  */
  10, 10,				/* Gather load static, per_elt.  */
  10, 10,				/* Gather store static, per_elt.  */
  32,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  100,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (4),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (4),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (19),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (35),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (2),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (13),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (17),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (14),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (48),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  btver1_memcpy,
  btver1_memset,
  COSTS_N_INSNS (2),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:11:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "11",					/* Func alignment.  */
};

static stringop_algs btver2_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs btver2_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
const struct processor_costs btver2_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  8,				     /* cost for loading QImode using movzbl */
  {8, 8, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 6},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {12, 12, 28},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {12, 12, 38},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  4,					/* cost of moving MMX register */
  {10, 10},				/* cost of loading MMX registers
					   in SImode and DImode */
  {12, 12},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {10, 10, 12, 48, 96},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {10, 10, 12, 48, 96},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  14, 14,				/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (2),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (5)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (19),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (35),			/*			    HI */
   COSTS_N_INSNS (51),			/*			    SI */
   COSTS_N_INSNS (83),			/*			    DI */
   COSTS_N_INSNS (83)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  9,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {8, 8, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {8, 8, 6},				/* cost of storing integer registers */
  {10, 10, 12, 48, 96},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {10, 10, 12, 48, 96},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {10, 10, 12, 48, 96},			/* cost of unaligned loads.  */
  {10, 10, 12, 48, 96},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  14,					/* cost of moving SSE register to integer.  */
  10, 10,				/* Gather load static, per_elt.  */
  10, 10,				/* Gather store static, per_elt.  */
  32,					/* size of l1 cache.  */
  2048,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  100,					/* number of parallel prefetches */
  2,					/* Branch cost */
  COSTS_N_INSNS (4),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (4),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (19),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (2),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (2),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (35),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (2),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (4),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (13),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (19),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (16),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (21),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  btver2_memcpy,
  btver2_memset,
  COSTS_N_INSNS (2),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:11:8",				/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "11",					/* Func alignment.  */
};

static stringop_algs pentium4_memcpy[2] = {
  {libcall, {{12, loop_1_byte, false}, {-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs pentium4_memset[2] = {
  {libcall, {{6, loop_1_byte, false}, {48, loop, false},
             {20480, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};

static const
struct processor_costs pentium4_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  5,				     /* cost for loading QImode using movzbl */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 3, 2},				/* cost of storing integer registers */
  12,					/* cost of reg,reg fld/fst */
  {14, 14, 14},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {14, 14, 14},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  12,					/* cost of moving MMX register */
  {16, 16},				/* cost of loading MMX registers
					   in SImode and DImode */
  {16, 16},				/* cost of storing MMX registers
					   in SImode and DImode */
  12, 24, 48,				/* cost of moving XMM,YMM,ZMM register */
  {16, 16, 16, 32, 64},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {16, 16, 16, 32, 64},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  20, 12,				/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (3),			/* cost of a lea instruction */
  COSTS_N_INSNS (4),			/* variable shift costs */
  COSTS_N_INSNS (4),			/* constant shift costs */
  {COSTS_N_INSNS (15),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (15),			/*				 HI */
   COSTS_N_INSNS (15),			/*				 SI */
   COSTS_N_INSNS (15),			/*				 DI */
   COSTS_N_INSNS (15)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (56),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (56),			/*			    HI */
   COSTS_N_INSNS (56),			/*			    SI */
   COSTS_N_INSNS (56),			/*			    DI */
   COSTS_N_INSNS (56)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  16,					/* "large" insn */
  6,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {4, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {2, 3, 2},				/* cost of storing integer registers */
  {16, 16, 16, 32, 64},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {16, 16, 16, 32, 64},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {32, 32, 32, 64, 128},		/* cost of unaligned loads.  */
  {32, 32, 32, 64, 128},		/* cost of unaligned stores.  */
  12, 24, 48,				/* cost of moving XMM,YMM,ZMM register */
  20,					/* cost of moving SSE register to integer.  */
  16, 16,				/* Gather load static, per_elt.  */
  16, 16,				/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (4),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (6),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (6),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (23),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (38),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (23),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (38),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  pentium4_memcpy,
  pentium4_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  NULL,					/* Loop alignment.  */
  NULL,					/* Jump alignment.  */
  NULL,					/* Label alignment.  */
  NULL,					/* Func alignment.  */
};

static stringop_algs nocona_memcpy[2] = {
  {libcall, {{12, loop_1_byte, false}, {-1, rep_prefix_4_byte, false}}},
  {libcall, {{32, loop, false}, {20000, rep_prefix_8_byte, false},
             {100000, unrolled_loop, false}, {-1, libcall, false}}}};

static stringop_algs nocona_memset[2] = {
  {libcall, {{6, loop_1_byte, false}, {48, loop, false},
             {20480, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{24, loop, false}, {64, unrolled_loop, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};

static const
struct processor_costs nocona_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  4,				     /* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  12,					/* cost of reg,reg fld/fst */
  {14, 14, 14},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {14, 14, 14},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  14,					/* cost of moving MMX register */
  {12, 12},				/* cost of loading MMX registers
					   in SImode and DImode */
  {12, 12},				/* cost of storing MMX registers
					   in SImode and DImode */
  6, 12, 24,				/* cost of moving XMM,YMM,ZMM register */
  {12, 12, 12, 24, 48},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {12, 12, 12, 24, 48},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  20, 12,				/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1),			/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (10),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (10),			/*				 HI */
   COSTS_N_INSNS (10),			/*				 SI */
   COSTS_N_INSNS (10),			/*				 DI */
   COSTS_N_INSNS (10)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (66),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (66),			/*			    HI */
   COSTS_N_INSNS (66),			/*			    SI */
   COSTS_N_INSNS (66),			/*			    DI */
   COSTS_N_INSNS (66)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  16,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  {12, 12, 12, 24, 48},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {12, 12, 12, 24, 48},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {24, 24, 24, 48, 96},			/* cost of unaligned loads.  */
  {24, 24, 24, 48, 96},			/* cost of unaligned stores.  */
  6, 12, 24,				/* cost of moving XMM,YMM,ZMM register */
  20,					/* cost of moving SSE register to integer.  */
  12, 12,				/* Gather load static, per_elt.  */
  12, 12,				/* Gather store static, per_elt.  */
  8,					/* size of l1 cache.  */
  1024,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  8,					/* number of parallel prefetches */
  1,					/* Branch cost */
  COSTS_N_INSNS (6),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (8),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (40),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (3),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (3),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (44),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (2),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (5),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (7),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (7),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (7),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (7),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (32),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (40),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (32),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (41),			/* cost of SQRTSD instruction.  */
  1, 1, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  nocona_memcpy,
  nocona_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  NULL,					/* Loop alignment.  */
  NULL,					/* Jump alignment.  */
  NULL,					/* Label alignment.  */
  NULL,					/* Func alignment.  */
};

static stringop_algs atom_memcpy[2] = {
  {libcall, {{11, loop, false}, {-1, rep_prefix_4_byte, false}}},
  {libcall, {{32, loop, false}, {64, rep_prefix_4_byte, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static stringop_algs atom_memset[2] = {
  {libcall, {{8, loop, false}, {15, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{24, loop, false}, {32, unrolled_loop, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static const
struct processor_costs atom_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,					/* cost for loading QImode using movzbl */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {6, 6, 18},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {14, 14, 24},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {8, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {10, 10},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {8, 8, 8, 16, 32},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {8, 8, 8, 16, 32},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  8, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (2)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*			    HI */
   COSTS_N_INSNS (42),			/*			    SI */
   COSTS_N_INSNS (74),			/*			    DI */
   COSTS_N_INSNS (74)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  {8, 8, 8, 16, 32},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {8, 8, 8, 16, 32},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {16, 16, 16, 32, 64},			/* cost of unaligned loads.  */
  {16, 16, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  8,					/* cost of moving SSE register to integer.  */
  8, 8,					/* Gather load static, per_elt.  */
  8, 8,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (5),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (5),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (31),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (60),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (31),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (63),			/* cost of SQRTSD instruction.  */
  2, 2, 2, 2,				/* reassoc int, fp, vec_int, vec_fp.  */
  atom_memcpy,
  atom_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

static stringop_algs slm_memcpy[2] = {
  {libcall, {{11, loop, false}, {-1, rep_prefix_4_byte, false}}},
  {libcall, {{32, loop, false}, {64, rep_prefix_4_byte, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static stringop_algs slm_memset[2] = {
  {libcall, {{8, loop, false}, {15, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{24, loop, false}, {32, unrolled_loop, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static const
struct processor_costs slm_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  8,					/* cost for loading QImode using movzbl */
  {8, 8, 8},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {8, 8, 18},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 18},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {8, 8},				/* cost of loading MMX registers
					   in SImode and DImode */
  {6, 6},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  {8, 8, 8, 16, 32},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {8, 8, 8, 16, 32},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  8, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (3),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (2)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*			    HI */
   COSTS_N_INSNS (42),			/*			    SI */
   COSTS_N_INSNS (74),			/*			    DI */
   COSTS_N_INSNS (74)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {8, 8, 8},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  {8, 8, 8, 16, 32},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {8, 8, 8, 16, 32},			/* cost of storing SSE register
					   in SImode, DImode and TImode.  */
  {16, 16, 16, 32, 64},			/* cost of unaligned loads.  */
  {16, 16, 16, 32, 64},			/* cost of unaligned stores.  */
  2, 4, 8,				/* cost of moving XMM,YMM,ZMM register */
  8,					/* cost of moving SSE register to integer.  */
  8, 8,					/* Gather load static, per_elt.  */
  8, 8,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (5),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (39),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (69),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (20),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (35),			/* cost of SQRTSD instruction.  */
  1, 2, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  slm_memcpy,
  slm_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

static stringop_algs intel_memcpy[2] = {
  {libcall, {{11, loop, false}, {-1, rep_prefix_4_byte, false}}},
  {libcall, {{32, loop, false}, {64, rep_prefix_4_byte, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static stringop_algs intel_memset[2] = {
  {libcall, {{8, loop, false}, {15, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{24, loop, false}, {32, unrolled_loop, false},
             {8192, rep_prefix_8_byte, false}, {-1, libcall, false}}}};
static const
struct processor_costs intel_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,				     /* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {6, 6, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 10},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode */
  {6, 6},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 2, 2,				/* cost of moving XMM,YMM,ZMM register */
  {6, 6, 6, 6, 6},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {6, 6, 6, 6, 6},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  4, 4,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (3),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (2)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (18),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (26),			/*			    HI */
   COSTS_N_INSNS (42),			/*			    SI */
   COSTS_N_INSNS (74),			/*			    DI */
   COSTS_N_INSNS (74)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  {6, 6, 6, 6, 6},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 6, 6},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {10, 10, 10, 10, 10},			/* cost of unaligned loads.  */
  {10, 10, 10, 10, 10},			/* cost of unaligned loads.  */
  2, 2, 2,				/* cost of moving XMM,YMM,ZMM register */
  4,					/* cost of moving SSE register to integer.  */
  6, 6,					/* Gather load static, per_elt.  */
  6, 6,					/* Gather store static, per_elt.  */
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

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (8),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (8),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (8),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (6),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (20),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (20),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (40),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (40),			/* cost of SQRTSD instruction.  */
  1, 4, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  intel_memcpy,
  intel_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16",					/* Loop alignment.  */
  "16:8:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/* Generic should produce code tuned for Core-i7 (and newer chips)
   and btver1 (and newer chips).  */

static stringop_algs generic_memcpy[2] = {
  {libcall, {{32, loop, false}, {8192, rep_prefix_4_byte, false},
             {-1, libcall, false}}},
  {libcall, {{32, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs generic_memset[2] = {
  {libcall, {{32, loop, false}, {8192, rep_prefix_4_byte, false},
             {-1, libcall, false}}},
  {libcall, {{32, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static const
struct processor_costs generic_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,				     /* cost for loading QImode using movzbl */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  4,					/* cost of reg,reg fld/fst */
  {6, 6, 12},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 12},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode */
  {6, 6},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 3, 4,				/* cost of moving XMM,YMM,ZMM register */
  {6, 6, 6, 10, 15},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {6, 6, 6, 10, 15},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  6, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  /* Setting cost to 2 makes our current implementation of synth_mult result in
     use of unnecessary temporary registers causing regression on several
     SPECfp benchmarks.  */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   COSTS_N_INSNS (4),			/*				 DI */
   COSTS_N_INSNS (4)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  {COSTS_N_INSNS (16),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (22),			/*			    HI */
   COSTS_N_INSNS (30),			/*			    SI */
   COSTS_N_INSNS (74),			/*			    DI */
   COSTS_N_INSNS (74)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {6, 6, 6},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  {6, 6, 6, 10, 15},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 10, 15},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 10, 15},			/* cost of unaligned loads.  */
  {6, 6, 6, 10, 15},			/* cost of unaligned storess.  */
  2, 3, 4,				/* cost of moving XMM,YMM,ZMM register */
  6,					/* cost of moving SSE register to integer.  */
  18, 6,				/* Gather load static, per_elt.  */
  18, 6,				/* Gather store static, per_elt.  */
  32,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  /* Benchmarks shows large regressions on K8 sixtrack benchmark when this
     value is increased to perhaps more appropriate value of 5.  */
  3,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (17),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (14),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (5),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (13),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (17),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (14),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (18),			/* cost of SQRTSD instruction.  */
  1, 4, 3, 3,				/* reassoc int, fp, vec_int, vec_fp.  */
  generic_memcpy,
  generic_memset,
  COSTS_N_INSNS (4),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (2),			/* cond_not_taken_branch_cost.  */
  "16:11:8",				/* Loop alignment.  */
  "16:11:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

/* core_cost should produce code tuned for Core familly of CPUs.  */
static stringop_algs core_memcpy[2] = {
  {libcall, {{1024, rep_prefix_4_byte, true}, {-1, libcall, false}}},
  {libcall, {{24, loop, true}, {128, rep_prefix_8_byte, true},
             {-1, libcall, false}}}};
static stringop_algs core_memset[2] = {
  {libcall, {{6, loop_1_byte, true},
             {24, loop, true},
             {8192, rep_prefix_4_byte, true},
             {-1, libcall, false}}},
  {libcall, {{24, loop, true}, {512, rep_prefix_8_byte, true},
             {-1, libcall, false}}}};

static const
struct processor_costs core_cost = {
  {
  /* Start of register allocator costs.  integer->integer move cost is 2. */
  6,				     /* cost for loading QImode using movzbl */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {6, 6, 8},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {6, 6, 10},				/* cost of storing fp registers
					   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {6, 6},				/* cost of loading MMX registers
					   in SImode and DImode */
  {6, 6},				/* cost of storing MMX registers
					   in SImode and DImode */
  2, 2, 4,				/* cost of moving XMM,YMM,ZMM register */
  {6, 6, 6, 6, 12},			/* cost of loading SSE registers
					   in 32,64,128,256 and 512-bit */
  {6, 6, 6, 6, 12},			/* cost of storing SSE registers
					   in 32,64,128,256 and 512-bit */
  6, 6,					/* SSE->integer and integer->SSE moves */
  /* End of register allocator costs.  */
  },

  COSTS_N_INSNS (1),			/* cost of an add instruction */
  /* On all chips taken into consideration lea is 2 cycles and more.  With
     this cost however our current implementation of synth_mult results in
     use of unnecessary temporary registers causing regression on several
     SPECfp benchmarks.  */
  COSTS_N_INSNS (1) + 1,		/* cost of a lea instruction */
  COSTS_N_INSNS (1),			/* variable shift costs */
  COSTS_N_INSNS (1),			/* constant shift costs */
  {COSTS_N_INSNS (3),			/* cost of starting multiply for QI */
   COSTS_N_INSNS (4),			/*				 HI */
   COSTS_N_INSNS (3),			/*				 SI */
   /* Here we tune for Sandybridge or newer.  */
   COSTS_N_INSNS (3),			/*				 DI */
   COSTS_N_INSNS (3)},			/*			      other */
  0,					/* cost of multiply per each bit set */
  /* Expanding div/mod currently doesn't consider parallelism. So the cost
     model is not realistic. We compensate by increasing the latencies a bit.  */
  {COSTS_N_INSNS (11),			/* cost of a divide/mod for QI */
   COSTS_N_INSNS (11),			/*			    HI */
   COSTS_N_INSNS (14),			/*			    SI */
   COSTS_N_INSNS (81),			/*			    DI */
   COSTS_N_INSNS (81)},			/*			    other */
  COSTS_N_INSNS (1),			/* cost of movsx */
  COSTS_N_INSNS (1),			/* cost of movzx */
  8,					/* "large" insn */
  17,					/* MOVE_RATIO */
  6,					/* CLEAR_RATIO */
  {4, 4, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {6, 6, 6},				/* cost of storing integer registers */
  {6, 6, 6, 6, 12},			/* cost of loading SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 6, 12},			/* cost of storing SSE register
					   in 32bit, 64bit, 128bit, 256bit and 512bit */
  {6, 6, 6, 6, 12},			/* cost of unaligned loads.  */
  {6, 6, 6, 6, 12},			/* cost of unaligned stores.  */
  2, 2, 4,				/* cost of moving XMM,YMM,ZMM register */
  2,					/* cost of moving SSE register to integer.  */
  /* VGATHERDPD is 7 uops, rec throughput 5, while VGATHERDPD is 9 uops,
     rec. throughput 6.
     So 5 uops statically and one uops per load.  */
  10, 6,				/* Gather load static, per_elt.  */
  10, 6,				/* Gather store static, per_elt.  */
  64,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  /* FIXME perhaps more appropriate value is 5.  */
  3,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  /* 10-24 */
  COSTS_N_INSNS (24),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (23),			/* cost of FSQRT instruction.  */

  COSTS_N_INSNS (1),			/* cost of cheap SSE instruction.  */
  COSTS_N_INSNS (3),			/* cost of ADDSS/SD SUBSS/SD insns.  */
  COSTS_N_INSNS (4),			/* cost of MULSS instruction.  */
  COSTS_N_INSNS (5),			/* cost of MULSD instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SS instruction.  */
  COSTS_N_INSNS (5),			/* cost of FMA SD instruction.  */
  COSTS_N_INSNS (18),			/* cost of DIVSS instruction.  */
  COSTS_N_INSNS (32),			/* cost of DIVSD instruction.  */
  COSTS_N_INSNS (30),			/* cost of SQRTSS instruction.  */
  COSTS_N_INSNS (58),			/* cost of SQRTSD instruction.  */
  1, 4, 2, 2,				/* reassoc int, fp, vec_int, vec_fp.  */
  core_memcpy,
  core_memset,
  COSTS_N_INSNS (3),			/* cond_taken_branch_cost.  */
  COSTS_N_INSNS (1),			/* cond_not_taken_branch_cost.  */
  "16:11:8",				/* Loop alignment.  */
  "16:11:8",				/* Jump alignment.  */
  "0:0:8",				/* Label alignment.  */
  "16",					/* Func alignment.  */
};

