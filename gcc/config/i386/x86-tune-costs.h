
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  1,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  1,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

static stringop_algs i486_memcpy[2] = {
  {rep_prefix_4_byte, {{-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs i486_memset[2] = {
  {rep_prefix_4_byte, {{-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};

static const
struct processor_costs i486_cost = {	/* 486 specific costs */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

static stringop_algs pentium_memcpy[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs pentium_memset[2] = {
  {libcall, {{-1, rep_prefix_4_byte, false}}},
  DUMMY_STRINGOP_ALGS};

static const
struct processor_costs pentium_cost = {
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

static const
struct processor_costs lakemont_cost = {
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

static stringop_algs geode_memcpy[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs geode_memset[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static const
struct processor_costs geode_cost = {
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
  1,				     /* cost for loading QImode using movzbl */
  {1, 1, 1},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {1, 1, 1},				/* cost of storing integer registers */
  1,					/* cost of reg,reg fld/fst */
  {1, 1, 1},				/* cost of loading fp registers
					   in SFmode, DFmode and XFmode */
  {4, 6, 6},				/* cost of storing fp registers
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

static stringop_algs k6_memcpy[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static stringop_algs k6_memset[2] = {
  {libcall, {{256, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  DUMMY_STRINGOP_ALGS};
static const
struct processor_costs k6_cost = {
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  4,					/* scalar_stmt_cost.  */
  2,					/* scalar load_cost.  */
  2,					/* scalar_store_cost.  */
  5,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  2,					/* vec_align_load_cost.  */
  3,					/* vec_unalign_load_cost.  */
  3,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  2,					/* cond_not_taken_branch_cost.  */
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
  2,					/* cost of moving SSE register */
  {4, 4, 3},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 5},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
  					/* On K8:
  					    MOVD reg64, xmmreg Double FSTORE 4
					    MOVD reg32, xmmreg Double FSTORE 4
					   On AMDFAM10:
					    MOVD reg64, xmmreg Double FADD 3
							       1/1  1/1
					    MOVD reg32, xmmreg Double FADD 3
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
  4,					/* scalar_stmt_cost.  */
  2,					/* scalar load_cost.  */
  2,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  2,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  2,					/* vec_store_cost.  */
  2,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

/*  BDVER1 has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop. For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */
static stringop_algs bdver1_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs bdver1_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};

const struct processor_costs bdver1_cost = {
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
  4,				     /* cost for loading QImode using movzbl */
  {5, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {5, 5, 12},				/* cost of loading fp registers
		   			   in SFmode, DFmode and XFmode */
  {4, 4, 8},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {4, 4},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 4, 4},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 4},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  2,					/* MMX or SSE register to integer */
  					/* On K8:
					    MOVD reg64, xmmreg Double FSTORE 4
					    MOVD reg32, xmmreg Double FSTORE 4
					   On AMDFAM10:
					    MOVD reg64, xmmreg Double FADD 3
							       1/1  1/1
					    MOVD reg32, xmmreg Double FADD 3
							       1/1  1/1 */
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
  bdver1_memcpy,
  bdver1_memset,
  6,					/* scalar_stmt_cost.  */
  4,					/* scalar load_cost.  */
  4,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  4,					/* vec_align_load_cost.  */
  4,					/* vec_unalign_load_cost.  */
  4,					/* vec_store_cost.  */
  4,					/* cond_taken_branch_cost.  */
  2,					/* cond_not_taken_branch_cost.  */
};

/*  BDVER2 has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop. For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */

static stringop_algs bdver2_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs bdver2_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};

const struct processor_costs bdver2_cost = {
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
  4,				     /* cost for loading QImode using movzbl */
  {5, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {5, 5, 12},				/* cost of loading fp registers
		   			   in SFmode, DFmode and XFmode */
  {4, 4, 8},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {4, 4},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 4, 4},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 4},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  2,					/* MMX or SSE register to integer */
  					/* On K8:
					    MOVD reg64, xmmreg Double FSTORE 4
					    MOVD reg32, xmmreg Double FSTORE 4
					   On AMDFAM10:
					    MOVD reg64, xmmreg Double FADD 3
							       1/1  1/1
					    MOVD reg32, xmmreg Double FADD 3
							       1/1  1/1 */
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
  bdver2_memcpy,
  bdver2_memset,
  6,					/* scalar_stmt_cost.  */
  4,					/* scalar load_cost.  */
  4,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  4,					/* vec_align_load_cost.  */
  4,					/* vec_unalign_load_cost.  */
  4,					/* vec_store_cost.  */
  4,					/* cond_taken_branch_cost.  */
  2,					/* cond_not_taken_branch_cost.  */
};


  /*  BDVER3 has optimized REP instruction for medium sized blocks, but for
      very small blocks it is better to use loop. For large blocks, libcall
      can do nontemporary accesses and beat inline considerably.  */
static stringop_algs bdver3_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs bdver3_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
struct processor_costs bdver3_cost = {
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
  4,				     /* cost for loading QImode using movzbl */
  {5, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {5, 5, 12},				/* cost of loading fp registers
		   			   in SFmode, DFmode and XFmode */
  {4, 4, 8},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {4, 4},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 4, 4},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 4},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  2,					/* MMX or SSE register to integer */
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
  bdver3_memcpy,
  bdver3_memset,
  6,					/* scalar_stmt_cost.  */
  4,					/* scalar load_cost.  */
  4,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  4,					/* vec_align_load_cost.  */
  4,					/* vec_unalign_load_cost.  */
  4,					/* vec_store_cost.  */
  4,					/* cond_taken_branch_cost.  */
  2,					/* cond_not_taken_branch_cost.  */
};

/*  BDVER4 has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop. For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */
static stringop_algs bdver4_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
             {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
static stringop_algs bdver4_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
             {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
             {-1, libcall, false}}}};
struct processor_costs bdver4_cost = {
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
  4,				     /* cost for loading QImode using movzbl */
  {5, 5, 4},				/* cost of loading integer registers
					   in QImode, HImode and SImode.
					   Relative to reg-reg move (2).  */
  {4, 4, 4},				/* cost of storing integer registers */
  2,					/* cost of reg,reg fld/fst */
  {5, 5, 12},				/* cost of loading fp registers
		   			   in SFmode, DFmode and XFmode */
  {4, 4, 8},				/* cost of storing fp registers
 		   			   in SFmode, DFmode and XFmode */
  2,					/* cost of moving MMX register */
  {4, 4},				/* cost of loading MMX registers
					   in SImode and DImode */
  {4, 4},				/* cost of storing MMX registers
					   in SImode and DImode */
  2,					/* cost of moving SSE register */
  {4, 4, 4},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 4},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  2,					/* MMX or SSE register to integer */
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
  bdver4_memcpy,
  bdver4_memset,
  6,					/* scalar_stmt_cost.  */
  4,					/* scalar load_cost.  */
  4,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  4,					/* vec_align_load_cost.  */
  4,					/* vec_unalign_load_cost.  */
  4,					/* vec_store_cost.  */
  4,					/* cond_taken_branch_cost.  */
  2,					/* cond_not_taken_branch_cost.  */
};


/*  ZNVER1 has optimized REP instruction for medium sized blocks, but for
    very small blocks it is better to use loop.  For large blocks, libcall
    can do nontemporary accesses and beat inline considerably.  */
static stringop_algs znver1_memcpy[2] = {
  {libcall, {{6, loop, false}, {14, unrolled_loop, false},
	     {-1, rep_prefix_4_byte, false}}},
  {libcall, {{16, loop, false}, {8192, rep_prefix_8_byte, false},
	     {-1, libcall, false}}}};
static stringop_algs znver1_memset[2] = {
  {libcall, {{8, loop, false}, {24, unrolled_loop, false},
	     {2048, rep_prefix_4_byte, false}, {-1, libcall, false}}},
  {libcall, {{48, unrolled_loop, false}, {8192, rep_prefix_8_byte, false},
	     {-1, libcall, false}}}};
struct processor_costs znver1_cost = {
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
  2,					/* cost of moving SSE register.  */
  {6, 6, 6},				/* cost of loading SSE registers
					   in SImode, DImode and TImode.  */
  {8, 8, 8},				/* cost of storing SSE registers
					   in SImode, DImode and TImode.  */
  6,					/* MMX or SSE register to integer.  */
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
  6,					/* scalar_stmt_cost.  */
  4,					/* scalar load_cost.  */
  4,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  4,					/* vec_align_load_cost.  */
  4,					/* vec_unalign_load_cost.  */
  4,					/* vec_store_cost.  */
  4,					/* cond_taken_branch_cost.  */
  2,					/* cond_not_taken_branch_cost.  */
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
  2,					/* cost of moving SSE register */
  {4, 4, 3},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 5},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
					/* On K8:
					   MOVD reg64, xmmreg Double FSTORE 4
					   MOVD reg32, xmmreg Double FSTORE 4
					   On AMDFAM10:
					   MOVD reg64, xmmreg Double FADD 3
							       1/1  1/1
					    MOVD reg32, xmmreg Double FADD 3
							       1/1  1/1 */
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
  4,					/* scalar_stmt_cost.  */
  2,					/* scalar load_cost.  */
  2,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  2,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  2,					/* vec_store_cost.  */
  2,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  2,					/* cost of moving SSE register */
  {4, 4, 3},				/* cost of loading SSE registers
					   in SImode, DImode and TImode */
  {4, 4, 5},				/* cost of storing SSE registers
					   in SImode, DImode and TImode */
  3,					/* MMX or SSE register to integer */
					/* On K8:
					   MOVD reg64, xmmreg Double FSTORE 4
					   MOVD reg32, xmmreg Double FSTORE 4
					   On AMDFAM10:
					   MOVD reg64, xmmreg Double FADD 3
							       1/1  1/1
					    MOVD reg32, xmmreg Double FADD 3
							       1/1  1/1 */
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
  4,					/* scalar_stmt_cost.  */
  2,					/* scalar load_cost.  */
  2,					/* scalar_store_cost.  */
  6,					/* vec_stmt_cost.  */
  0,					/* vec_to_scalar_cost.  */
  2,					/* scalar_to_vec_cost.  */
  2,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  2,					/* vec_store_cost.  */
  2,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  2,				     /* cost for loading QImode using movzbl */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  4,				     /* cost for loading QImode using movzbl */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  4,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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

  COSTS_N_INSNS (8),			/* cost of cheap SSE instruction.  */
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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  4,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  4,				     /* cost for loading QImode using movzbl */
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
  /* Benchmarks shows large regressions on K8 sixtrack benchmark when this
     value is increased to perhaps more appropriate value of 5.  */
  3,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (3),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (20),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (40),			/* cost of FSQRT instruction.  */

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
  1, 2, 1, 1,				/* reassoc int, fp, vec_int, vec_fp.  */
  generic_memcpy,
  generic_memset,
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
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
  4,				     /* cost for loading QImode using movzbl */
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
  64,					/* size of l1 cache.  */
  512,					/* size of l2 cache.  */
  64,					/* size of prefetch block */
  6,					/* number of parallel prefetches */
  /* FIXME perhaps more appropriate value is 5.  */
  3,					/* Branch cost */
  COSTS_N_INSNS (3),			/* cost of FADD and FSUB insns.  */
  COSTS_N_INSNS (5),			/* cost of FMUL instruction.  */
  COSTS_N_INSNS (24),			/* cost of FDIV instruction.  */
  COSTS_N_INSNS (1),			/* cost of FABS instruction.  */
  COSTS_N_INSNS (1),			/* cost of FCHS instruction.  */
  COSTS_N_INSNS (24),			/* cost of FSQRT instruction.  */

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
  1,					/* scalar_stmt_cost.  */
  1,					/* scalar load_cost.  */
  1,					/* scalar_store_cost.  */
  1,					/* vec_stmt_cost.  */
  1,					/* vec_to_scalar_cost.  */
  1,					/* scalar_to_vec_cost.  */
  1,					/* vec_align_load_cost.  */
  2,					/* vec_unalign_load_cost.  */
  1,					/* vec_store_cost.  */
  3,					/* cond_taken_branch_cost.  */
  1,					/* cond_not_taken_branch_cost.  */
};

