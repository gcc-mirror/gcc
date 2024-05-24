/* Subroutines used for code generation for RISC-V.
   Copyright (C) 2011-2024 Free Software Foundation, Inc.
   Contributed by Andrew Waterman (andrew@sifive.com).
   Based on MIPS target for GNU compiler.

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

#define IN_TARGET_CODE 1

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "backend.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "output.h"
#include "alias.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "function.h"
#include "explow.h"
#include "ifcvt.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "reload.h"
#include "tm_p.h"
#include "basic-block.h"
#include "expr.h"
#include "optabs.h"
#include "bitmap.h"
#include "df.h"
#include "function-abi.h"
#include "diagnostic.h"
#include "builtins.h"
#include "predict.h"
#include "tree-pass.h"
#include "opts.h"
#include "tm-constrs.h"
#include "rtl-iter.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "cfgrtl.h"
#include "shrink-wrap.h"
#include "sel-sched.h"
#include "sched-int.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "gimple-expr.h"
#include "tree-vectorizer.h"
#include "gcse.h"
#include "tree-dfa.h"
#include "target-globals.h"

/* This file should be included last.  */
#include "target-def.h"
#include "riscv-vector-costs.h"
#include "riscv-subset.h"

/* True if X is an UNSPEC wrapper around a SYMBOL_REF or LABEL_REF.  */
#define UNSPEC_ADDRESS_P(X)					\
  (GET_CODE (X) == UNSPEC					\
   && XINT (X, 1) >= UNSPEC_ADDRESS_FIRST			\
   && XINT (X, 1) < UNSPEC_ADDRESS_FIRST + NUM_SYMBOL_TYPES)

/* Extract the symbol or label from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS(X) \
  XVECEXP (X, 0, 0)

/* Extract the symbol type from UNSPEC wrapper X.  */
#define UNSPEC_ADDRESS_TYPE(X) \
  ((enum riscv_symbol_type) (XINT (X, 1) - UNSPEC_ADDRESS_FIRST))

/* Extract the backup dynamic frm rtl.  */
#define DYNAMIC_FRM_RTL(c) ((c)->machine->mode_sw_info.dynamic_frm)

/* True the mode switching has static frm, or false.  */
#define STATIC_FRM_P(c) ((c)->machine->mode_sw_info.static_frm_p)

/* True if we can use the instructions in the XTheadInt extension
   to handle interrupts, or false.  */
#define TH_INT_INTERRUPT(c)						\
  (TARGET_XTHEADINT							\
   /* The XTheadInt extension only supports rv32.  */			\
   && !TARGET_64BIT							\
   && (c)->machine->interrupt_handler_p					\
   /* The XTheadInt instructions can only be executed in M-mode.  */	\
   && (c)->machine->interrupt_mode == MACHINE_MODE)

/* Information about a function's frame layout.  */
struct GTY(())  riscv_frame_info {
  /* The size of the frame in bytes.  */
  poly_int64 total_size;

  /* Bit X is set if the function saves or restores GPR X.  */
  unsigned int mask;

  /* Likewise FPR X.  */
  unsigned int fmask;

  /* Likewise for vector registers.  */
  unsigned int vmask;

  /* How much the GPR save/restore routines adjust sp (or 0 if unused).  */
  unsigned save_libcall_adjustment;

  /* the minimum number of bytes, in multiples of 16-byte address increments,
     required to cover the registers in a multi push & pop.  */
  unsigned multi_push_adj_base;

  /* the number of additional 16-byte address increments allocated for the stack
     frame in a multi push & pop.  */
  unsigned multi_push_adj_addi;

  /* Offsets of fixed-point and floating-point save areas from frame bottom */
  poly_int64 gp_sp_offset;
  poly_int64 fp_sp_offset;

  /* Top and bottom offsets of vector save areas from frame bottom.  */
  poly_int64 v_sp_offset_top;
  poly_int64 v_sp_offset_bottom;

  /* Offset of virtual frame pointer from stack pointer/frame bottom */
  poly_int64 frame_pointer_offset;

  /* Offset of hard frame pointer from stack pointer/frame bottom */
  poly_int64 hard_frame_pointer_offset;

  /* The offset of arg_pointer_rtx from the bottom of the frame.  */
  poly_int64 arg_pointer_offset;

  /* Reset this struct, clean all field to zero.  */
  void reset(void);
};

enum riscv_privilege_levels {
  UNKNOWN_MODE, USER_MODE, SUPERVISOR_MODE, MACHINE_MODE
};

struct GTY(()) mode_switching_info {
  /* The RTL variable which stores the dynamic FRM value.  We always use this
     RTX to restore dynamic FRM rounding mode in mode switching.  */
  rtx dynamic_frm;

  /* The boolean variables indicates there is at least one static rounding
     mode instruction in the function or not.  */
  bool static_frm_p;

  mode_switching_info ()
    {
      dynamic_frm = NULL_RTX;
      static_frm_p = false;
    }
};

struct GTY(())  machine_function {
  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the frame.  */
  int varargs_size;

  /* True if current function is a naked function.  */
  bool naked_p;

  /* True if current function is an interrupt function.  */
  bool interrupt_handler_p;
  /* For an interrupt handler, indicates the privilege level.  */
  enum riscv_privilege_levels interrupt_mode;

  /* True if attributes on current function have been checked.  */
  bool attributes_checked_p;

  /* True if RA must be saved because of a far jump.  */
  bool far_jump_used;

  /* The current frame information, calculated by riscv_compute_frame_info.  */
  struct riscv_frame_info frame;

  /* The components already handled by separate shrink-wrapping, which should
     not be considered by the prologue and epilogue.  */
  bool reg_is_wrapped_separately[FIRST_PSEUDO_REGISTER];

  /* The mode swithching information for the FRM rounding modes.  */
  struct mode_switching_info mode_sw_info;
};

/* Information about a single argument.  */
struct riscv_arg_info {
  /* True if the argument is at least partially passed on the stack.  */
  bool stack_p;

  /* The number of integer registers allocated to this argument.  */
  unsigned int num_gprs;

  /* The offset of the first register used, provided num_gprs is nonzero.
     If passed entirely on the stack, the value is MAX_ARGS_IN_REGISTERS.  */
  unsigned int gpr_offset;

  /* The number of floating-point registers allocated to this argument.  */
  unsigned int num_fprs;

  /* The offset of the first register used, provided num_fprs is nonzero.  */
  unsigned int fpr_offset;

  /* The number of vector registers allocated to this argument.  */
  unsigned int num_vrs;

  /* The offset of the first register used, provided num_vrs is nonzero.  */
  unsigned int vr_offset;

  /* The number of mask registers allocated to this argument.  */
  unsigned int num_mrs;

  /* The offset of the first register used, provided num_mrs is nonzero.  */
  unsigned int mr_offset;
};

/* One stage in a constant building sequence.  These sequences have
   the form:

	A = VALUE[0]
	A = A CODE[1] VALUE[1]
	A = A CODE[2] VALUE[2]
	...

   where A is an accumulator, each CODE[i] is a binary rtl operation
   and each VALUE[i] is a constant integer.  CODE[0] is undefined.  */
struct riscv_integer_op {
  enum rtx_code code;
  unsigned HOST_WIDE_INT value;
};

/* The largest number of operations needed to load an integer constant.
   The worst case is LUI, ADDI, SLLI, ADDI, SLLI, ADDI, SLLI, ADDI.  */
#define RISCV_MAX_INTEGER_OPS 8

enum riscv_fusion_pairs
{
  RISCV_FUSE_NOTHING = 0,
  RISCV_FUSE_ZEXTW = (1 << 0),
  RISCV_FUSE_ZEXTH = (1 << 1),
  RISCV_FUSE_ZEXTWS = (1 << 2),
  RISCV_FUSE_LDINDEXED = (1 << 3),
  RISCV_FUSE_LUI_ADDI = (1 << 4),
  RISCV_FUSE_AUIPC_ADDI = (1 << 5),
  RISCV_FUSE_LUI_LD = (1 << 6),
  RISCV_FUSE_AUIPC_LD = (1 << 7),
  RISCV_FUSE_LDPREINCREMENT = (1 << 8),
  RISCV_FUSE_ALIGNED_STD = (1 << 9),
};

/* Costs of various operations on the different architectures.  */

struct riscv_tune_param
{
  unsigned short fp_add[2];
  unsigned short fp_mul[2];
  unsigned short fp_div[2];
  unsigned short int_mul[2];
  unsigned short int_div[2];
  unsigned short issue_rate;
  unsigned short branch_cost;
  unsigned short memory_cost;
  unsigned short fmv_cost;
  bool slow_unaligned_access;
  bool use_divmod_expansion;
  unsigned int fusible_ops;
  const struct cpu_vector_cost *vec_costs;
};


/* Global variables for machine-dependent things.  */

/* Whether unaligned accesses execute very slowly.  */
bool riscv_slow_unaligned_access_p;

/* Whether user explicitly passed -mstrict-align.  */
bool riscv_user_wants_strict_align;

/* Stack alignment to assume/maintain.  */
unsigned riscv_stack_boundary;

/* Whether in riscv_output_mi_thunk. */
static bool riscv_in_thunk_func = false;

/* If non-zero, this is an offset to be added to SP to redefine the CFA
   when restoring the FP register from the stack.  Only valid when generating
   the epilogue.  */
static poly_int64 epilogue_cfa_sp_offset;

/* Which tuning parameters to use.  */
static const struct riscv_tune_param *tune_param;

/* Which automaton to use for tuning.  */
enum riscv_microarchitecture_type riscv_microarchitecture;

/* The number of chunks in a single vector register.  */
poly_uint16 riscv_vector_chunks;

/* The number of bytes in a vector chunk.  */
unsigned riscv_bytes_per_vector_chunk;

/* Index R is the smallest register class that contains register R.  */
const enum reg_class riscv_regno_to_class[FIRST_PSEUDO_REGISTER] = {
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	SIBCALL_REGS,	SIBCALL_REGS,
  JALR_REGS,	JALR_REGS,	SIBCALL_REGS,	SIBCALL_REGS,
  SIBCALL_REGS,	SIBCALL_REGS,	SIBCALL_REGS,	SIBCALL_REGS,
  SIBCALL_REGS,	SIBCALL_REGS,	JALR_REGS,	JALR_REGS,
  JALR_REGS,	JALR_REGS,	JALR_REGS,	JALR_REGS,
  JALR_REGS,	JALR_REGS,	JALR_REGS,	JALR_REGS,
  SIBCALL_REGS,	SIBCALL_REGS,	SIBCALL_REGS,	SIBCALL_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FRAME_REGS,	FRAME_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  VM_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
  VD_REGS,	VD_REGS,	VD_REGS,	VD_REGS,
};

/* RVV costs for VLS vector operations.   */
static const common_vector_cost rvv_vls_vector_cost = {
  1, /* int_stmt_cost  */
  1, /* fp_stmt_cost  */
  1, /* gather_load_cost  */
  1, /* scatter_store_cost  */
  1, /* vec_to_scalar_cost  */
  1, /* scalar_to_vec_cost  */
  1, /* permute_cost  */
  1, /* align_load_cost  */
  1, /* align_store_cost  */
  2, /* unalign_load_cost  */
  2, /* unalign_store_cost  */
};

/* RVV costs for VLA vector operations.  */
static const scalable_vector_cost rvv_vla_vector_cost = {
  {
    1, /* int_stmt_cost  */
    1, /* fp_stmt_cost  */
    1, /* gather_load_cost  */
    1, /* scatter_store_cost  */
    1, /* vec_to_scalar_cost  */
    1, /* scalar_to_vec_cost  */
    1, /* permute_cost  */
    1, /* align_load_cost  */
    1, /* align_store_cost  */
    2, /* unalign_load_cost  */
    2, /* unalign_store_cost  */
  },
};

/* RVV register move cost.   */
static const regmove_vector_cost rvv_regmove_vector_cost = {
  2, /* GR2VR  */
  2, /* FR2VR  */
  2, /* VR2GR  */
  2, /* VR2FR  */
};

/* Generic costs for vector insn classes.  It is supposed to be the vector cost
   models used by default if no other cost model was specified.  */
static const struct cpu_vector_cost generic_vector_cost = {
  1,			    /* scalar_int_stmt_cost  */
  1,			    /* scalar_fp_stmt_cost  */
  1,			    /* scalar_load_cost  */
  1,			    /* scalar_store_cost  */
  3,			    /* cond_taken_branch_cost  */
  1,			    /* cond_not_taken_branch_cost  */
  &rvv_vls_vector_cost,	    /* vls  */
  &rvv_vla_vector_cost,	    /* vla  */
  &rvv_regmove_vector_cost, /* regmove  */
};

/* Costs to use when optimizing for rocket.  */
static const struct riscv_tune_param rocket_tune_info = {
  {COSTS_N_INSNS (4), COSTS_N_INSNS (5)},	/* fp_add */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (5)},	/* fp_mul */
  {COSTS_N_INSNS (20), COSTS_N_INSNS (20)},	/* fp_div */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* int_mul */
  {COSTS_N_INSNS (33), COSTS_N_INSNS (65)},	/* int_div */
  1,						/* issue_rate */
  3,						/* branch_cost */
  5,						/* memory_cost */
  8,						/* fmv_cost */
  true,						/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_NOTHING,                           /* fusible_ops */
  NULL,						/* vector cost */
};

/* Costs to use when optimizing for Sifive 7 Series.  */
static const struct riscv_tune_param sifive_7_tune_info = {
  {COSTS_N_INSNS (4), COSTS_N_INSNS (5)},	/* fp_add */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (5)},	/* fp_mul */
  {COSTS_N_INSNS (20), COSTS_N_INSNS (20)},	/* fp_div */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* int_mul */
  {COSTS_N_INSNS (33), COSTS_N_INSNS (65)},	/* int_div */
  2,						/* issue_rate */
  4,						/* branch_cost */
  3,						/* memory_cost */
  8,						/* fmv_cost */
  true,						/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_NOTHING,                           /* fusible_ops */
  NULL,						/* vector cost */
};

/* Costs to use when optimizing for Sifive p400 Series.  */
static const struct riscv_tune_param sifive_p400_tune_info = {
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* fp_add */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* fp_mul */
  {COSTS_N_INSNS (20), COSTS_N_INSNS (20)},	/* fp_div */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* int_mul */
  {COSTS_N_INSNS (6), COSTS_N_INSNS (6)},	/* int_div */
  3,						/* issue_rate */
  4,						/* branch_cost */
  3,						/* memory_cost */
  4,						/* fmv_cost */
  true,						/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_LUI_ADDI | RISCV_FUSE_AUIPC_ADDI,  /* fusible_ops */
  &generic_vector_cost,				/* vector cost */
};

/* Costs to use when optimizing for Sifive p600 Series.  */
static const struct riscv_tune_param sifive_p600_tune_info = {
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* fp_add */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* fp_mul */
  {COSTS_N_INSNS (20), COSTS_N_INSNS (20)},	/* fp_div */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)},	/* int_mul */
  {COSTS_N_INSNS (6), COSTS_N_INSNS (6)},	/* int_div */
  4,						/* issue_rate */
  4,						/* branch_cost */
  3,						/* memory_cost */
  4,						/* fmv_cost */
  true,						/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_LUI_ADDI | RISCV_FUSE_AUIPC_ADDI,  /* fusible_ops */
  &generic_vector_cost,				/* vector cost */
};

/* Costs to use when optimizing for T-HEAD c906.  */
static const struct riscv_tune_param thead_c906_tune_info = {
  {COSTS_N_INSNS (4), COSTS_N_INSNS (5)}, /* fp_add */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (5)}, /* fp_mul */
  {COSTS_N_INSNS (20), COSTS_N_INSNS (20)}, /* fp_div */
  {COSTS_N_INSNS (4), COSTS_N_INSNS (4)}, /* int_mul */
  {COSTS_N_INSNS (18), COSTS_N_INSNS (34)}, /* int_div */
  1,            /* issue_rate */
  3,            /* branch_cost */
  5,            /* memory_cost */
  8,		/* fmv_cost */
  false,            /* slow_unaligned_access */
  false,	/* use_divmod_expansion */
  RISCV_FUSE_NOTHING,                           /* fusible_ops */
  NULL,						/* vector cost */
};

/* Costs to use when optimizing for xiangshan nanhu.  */
static const struct riscv_tune_param xiangshan_nanhu_tune_info = {
  {COSTS_N_INSNS (3), COSTS_N_INSNS (3)},	/* fp_add */
  {COSTS_N_INSNS (3), COSTS_N_INSNS (3)},	/* fp_mul */
  {COSTS_N_INSNS (10), COSTS_N_INSNS (20)},	/* fp_div */
  {COSTS_N_INSNS (3), COSTS_N_INSNS (3)},	/* int_mul */
  {COSTS_N_INSNS (6), COSTS_N_INSNS (6)},	/* int_div */
  6,						/* issue_rate */
  3,						/* branch_cost */
  3,						/* memory_cost */
  3,						/* fmv_cost */
  true,						/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_ZEXTW | RISCV_FUSE_ZEXTH,          /* fusible_ops */
  NULL,						/* vector cost */
};

/* Costs to use when optimizing for a generic ooo profile.  */
static const struct riscv_tune_param generic_ooo_tune_info = {
  {COSTS_N_INSNS (2), COSTS_N_INSNS (2)},	/* fp_add */
  {COSTS_N_INSNS (5), COSTS_N_INSNS (6)},	/* fp_mul */
  {COSTS_N_INSNS (7), COSTS_N_INSNS (8)},	/* fp_div */
  {COSTS_N_INSNS (2), COSTS_N_INSNS (2)},	/* int_mul */
  {COSTS_N_INSNS (6), COSTS_N_INSNS (6)},	/* int_div */
  1,						/* issue_rate */
  3,						/* branch_cost */
  4,						/* memory_cost */
  4,						/* fmv_cost */
  false,					/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_NOTHING,                           /* fusible_ops */
  &generic_vector_cost,				/* vector cost */
};

/* Costs to use when optimizing for size.  */
static const struct riscv_tune_param optimize_size_tune_info = {
  {COSTS_N_INSNS (1), COSTS_N_INSNS (1)},	/* fp_add */
  {COSTS_N_INSNS (1), COSTS_N_INSNS (1)},	/* fp_mul */
  {COSTS_N_INSNS (1), COSTS_N_INSNS (1)},	/* fp_div */
  {COSTS_N_INSNS (1), COSTS_N_INSNS (1)},	/* int_mul */
  {COSTS_N_INSNS (1), COSTS_N_INSNS (1)},	/* int_div */
  1,						/* issue_rate */
  1,						/* branch_cost */
  2,						/* memory_cost */
  8,						/* fmv_cost */
  false,					/* slow_unaligned_access */
  false,					/* use_divmod_expansion */
  RISCV_FUSE_NOTHING,                           /* fusible_ops */
  NULL,						/* vector cost */
};

static bool riscv_avoid_shrink_wrapping_separate ();
static tree riscv_handle_fndecl_attribute (tree *, tree, tree, int, bool *);
static tree riscv_handle_type_attribute (tree *, tree, tree, int, bool *);
static tree riscv_handle_rvv_vector_bits_attribute (tree *, tree, tree, int,
						    bool *);

/* Defining target-specific uses of __attribute__.  */
static const attribute_spec riscv_gnu_attributes[] =
{
  /* Syntax: { name, min_len, max_len, decl_required, type_required,
	       function_type_required, affects_type_identity, handler,
	       exclude } */

  /* The attribute telling no prologue/epilogue.  */
  {"naked", 0, 0, true, false, false, false, riscv_handle_fndecl_attribute,
   NULL},
  /* This attribute generates prologue/epilogue for interrupt handlers.  */
  {"interrupt", 0, 1, false, true, true, false, riscv_handle_type_attribute,
   NULL},

  /* The following two are used for the built-in properties of the Vector type
     and are not used externally */
  {"RVV sizeless type", 4, 4, false, true, false, true, NULL, NULL},
  {"RVV type", 0, 0, false, true, false, true, NULL, NULL},
  /* This attribute is used to declare a function, forcing it to use the
    standard vector calling convention variant. Syntax:
    __attribute__((riscv_vector_cc)). */
  {"riscv_vector_cc", 0, 0, false, true, true, true, NULL, NULL},
  /* This attribute is used to declare a new type, to appoint the exactly
     bits size of the type.  For example:

     typedef vint8m1_t f_vint8m1_t __attribute__((riscv_rvv_vector_bits(256)));

     The new created type f_vint8m1_t will be exactly 256 bits.  It can be
     be used in globals, structs, unions, and arrays instead of sizeless
     types.  */
  {"riscv_rvv_vector_bits", 1, 1, false, true, false, true,
   riscv_handle_rvv_vector_bits_attribute, NULL},
};

static const scoped_attribute_specs riscv_gnu_attribute_table  =
{
  "gnu", {riscv_gnu_attributes}
};

static const attribute_spec riscv_attributes[] =
{
  /* This attribute is used to declare a function, forcing it to use the
     standard vector calling convention variant. Syntax:
     [[riscv::vector_cc]]. */
  {"vector_cc", 0, 0, false, true, true, true, NULL, NULL},
  /* This attribute is used to declare a new type, to appoint the exactly
     bits size of the type.  For example:

     typedef vint8m1_t f_vint8m1_t __attribute__((riscv_rvv_vector_bits(256)));

     The new created type f_vint8m1_t will be exactly 256 bits.  It can be
     be used in globals, structs, unions, and arrays instead of sizeless
     types.  */
  {"rvv_vector_bits", 1, 1, false, true, false, true,
   riscv_handle_rvv_vector_bits_attribute, NULL},
};

static const scoped_attribute_specs riscv_nongnu_attribute_table =
{
  "riscv", {riscv_attributes}
};

static const scoped_attribute_specs *const riscv_attribute_table[] =
{
  &riscv_gnu_attribute_table,
  &riscv_nongnu_attribute_table
};

/* Order for the CLOBBERs/USEs of gpr_save.  */
static const unsigned gpr_save_reg_order[] = {
  INVALID_REGNUM, T0_REGNUM, T1_REGNUM, RETURN_ADDR_REGNUM,
  S0_REGNUM, S1_REGNUM, S2_REGNUM, S3_REGNUM, S4_REGNUM,
  S5_REGNUM, S6_REGNUM, S7_REGNUM, S8_REGNUM, S9_REGNUM,
  S10_REGNUM, S11_REGNUM
};

/* A table describing all the processors GCC knows about.  */
static const struct riscv_tune_info riscv_tune_info_table[] = {
#define RISCV_TUNE(TUNE_NAME, PIPELINE_MODEL, TUNE_INFO)	\
  { TUNE_NAME, PIPELINE_MODEL, & TUNE_INFO},
#include "riscv-cores.def"
};

/* Global variable to distinguish whether we should save and restore s0/fp for
   function.  */
static bool riscv_save_frame_pointer;

typedef enum
{
  PUSH_IDX = 0,
  POP_IDX,
  POPRET_IDX,
  POPRETZ_IDX,
  ZCMP_OP_NUM
} riscv_zcmp_op_t;

typedef insn_code (*code_for_push_pop_t) (machine_mode);

void riscv_frame_info::reset(void)
{
  total_size = 0;
  mask = 0;
  fmask = 0;
  vmask = 0;
  save_libcall_adjustment = 0;

  gp_sp_offset = 0;
  fp_sp_offset = 0;
  v_sp_offset_top = 0;
  v_sp_offset_bottom = 0;

  frame_pointer_offset = 0;

  hard_frame_pointer_offset = 0;

  arg_pointer_offset = 0;
}

/* Implement TARGET_MIN_ARITHMETIC_PRECISION.  */

static unsigned int
riscv_min_arithmetic_precision (void)
{
  return 32;
}

/* Get the arch string from an options object.  */

template <class T>
static const char *
get_arch_str (const T *opts)
{
  return opts->x_riscv_arch_string;
}

template <class T>
static const char *
get_tune_str (const T *opts)
{
  const char *tune_string = RISCV_TUNE_STRING_DEFAULT;
  if (opts->x_riscv_tune_string)
    tune_string = opts->x_riscv_tune_string;
  else if (opts->x_riscv_cpu_string)
    tune_string = opts->x_riscv_cpu_string;
  return tune_string;
}

/* Return the riscv_tune_info entry for the given name string, return nullptr
   if NULL_P is true, otherwise return an placeholder and report error.  */

const struct riscv_tune_info *
riscv_parse_tune (const char *tune_string, bool null_p)
{
  const riscv_cpu_info *cpu = riscv_find_cpu (tune_string);

  if (cpu)
    tune_string = cpu->tune;

  for (unsigned i = 0; i < ARRAY_SIZE (riscv_tune_info_table); i++)
    if (strcmp (riscv_tune_info_table[i].name, tune_string) == 0)
      return riscv_tune_info_table + i;

  if (null_p)
    return nullptr;

  error ("unknown cpu %qs for %<-mtune%>", tune_string);
  return riscv_tune_info_table;
}

/* Helper function for riscv_build_integer; arguments are as for
   riscv_build_integer.  */

static int
riscv_build_integer_1 (struct riscv_integer_op codes[RISCV_MAX_INTEGER_OPS],
		       HOST_WIDE_INT value, machine_mode mode)
{
  HOST_WIDE_INT low_part = CONST_LOW_PART (value);
  int cost = RISCV_MAX_INTEGER_OPS + 1, alt_cost;
  struct riscv_integer_op alt_codes[RISCV_MAX_INTEGER_OPS];

  if (SMALL_OPERAND (value) || LUI_OPERAND (value))
    {
      /* Simply ADDI or LUI.  */
      codes[0].code = UNKNOWN;
      codes[0].value = value;
      return 1;
    }
  if (TARGET_ZBS && SINGLE_BIT_MASK_OPERAND (value))
    {
      /* Simply BSETI.  */
      codes[0].code = UNKNOWN;
      codes[0].value = value;

      /* RISC-V sign-extends all 32bit values that live in a 32bit
	 register.  To avoid paradoxes, we thus need to use the
	 sign-extended (negative) representation (-1 << 31) for the
	 value, if we want to build (1 << 31) in SImode.  This will
	 then expand to an LUI instruction.  */
      if (TARGET_64BIT && mode == SImode && value == (HOST_WIDE_INT_1U << 31))
	codes[0].value = (HOST_WIDE_INT_M1U << 31);

      return 1;
    }

  /* End with ADDI.  When constructing HImode constants, do not generate any
     intermediate value that is not itself a valid HImode constant.  The
     XORI case below will handle those remaining HImode constants.  */
  if (low_part != 0
      && (mode != HImode
	  || value - low_part <= ((1 << (GET_MODE_BITSIZE (HImode) - 1)) - 1)))
    {
      HOST_WIDE_INT upper_part = value - low_part;
      if (mode != VOIDmode)
	upper_part = trunc_int_for_mode (value - low_part, mode);

      alt_cost = 1 + riscv_build_integer_1 (alt_codes, upper_part, mode);
      if (alt_cost < cost)
	{
	  alt_codes[alt_cost-1].code = PLUS;
	  alt_codes[alt_cost-1].value = low_part;
	  memcpy (codes, alt_codes, sizeof (alt_codes));
	  cost = alt_cost;
	}
    }

  /* End with XORI.  */
  if (cost > 2 && (low_part < 0 || mode == HImode))
    {
      alt_cost = 1 + riscv_build_integer_1 (alt_codes, value ^ low_part, mode);
      if (alt_cost < cost)
	{
	  alt_codes[alt_cost-1].code = XOR;
	  alt_codes[alt_cost-1].value = low_part;
	  memcpy (codes, alt_codes, sizeof (alt_codes));
	  cost = alt_cost;
	}
    }

  /* Eliminate trailing zeros and end with SLLI.  */
  if (cost > 2 && (value & 1) == 0)
    {
      int shift = ctz_hwi (value);
      unsigned HOST_WIDE_INT x = value;
      x = sext_hwi (x >> shift, HOST_BITS_PER_WIDE_INT - shift);

      /* Don't eliminate the lower 12 bits if LUI might apply.  */
      if (shift > IMM_BITS && !SMALL_OPERAND (x) && LUI_OPERAND (x << IMM_BITS))
	shift -= IMM_BITS, x <<= IMM_BITS;

      alt_cost = 1 + riscv_build_integer_1 (alt_codes, x, mode);
      if (alt_cost < cost)
	{
	  alt_codes[alt_cost-1].code = ASHIFT;
	  alt_codes[alt_cost-1].value = shift;
	  memcpy (codes, alt_codes, sizeof (alt_codes));
	  cost = alt_cost;
	}
    }

  if (cost > 2 && TARGET_64BIT && (TARGET_ZBB || TARGET_XTHEADBB))
    {
      int leading_ones = clz_hwi (~value);
      int trailing_ones = ctz_hwi (~value);

      /* If all bits are one except a few that are zero, and the zero bits
	 are within a range of 11 bits, then we can synthesize a constant
	 by loading a small negative constant and rotating.  */
      if (leading_ones < 64
	  && ((64 - leading_ones - trailing_ones) < 12))
	{
	  codes[0].code = UNKNOWN;
	  /* The sign-bit might be zero, so just rotate to be safe.  */
	  codes[0].value = (((unsigned HOST_WIDE_INT) value >> trailing_ones)
			    | (value << (64 - trailing_ones)));
	  codes[1].code = ROTATERT;
	  codes[1].value = 64 - trailing_ones;
	  cost = 2;
	}
      /* Handle the case where the 11 bit range of zero bits wraps around.  */
      else
	{
	  int upper_trailing_ones = ctz_hwi (~value >> 32);
	  int lower_leading_ones = clz_hwi (~value << 32);

	  if (upper_trailing_ones < 32 && lower_leading_ones < 32
	      && ((64 - upper_trailing_ones - lower_leading_ones) < 12))
	    {
	      codes[0].code = UNKNOWN;
	      /* The sign-bit might be zero, so just rotate to be safe.  */
	      codes[0].value = ((value << (32 - upper_trailing_ones))
				| ((unsigned HOST_WIDE_INT) value
				   >> (32 + upper_trailing_ones)));
	      codes[1].code = ROTATERT;
	      codes[1].value = 32 - upper_trailing_ones;
	      cost = 2;
	    }
	}
    }

  gcc_assert (cost <= RISCV_MAX_INTEGER_OPS);
  return cost;
}

/* Fill CODES with a sequence of rtl operations to load VALUE.
   Return the number of operations needed.  */

static int
riscv_build_integer (struct riscv_integer_op *codes, HOST_WIDE_INT value,
		     machine_mode mode)
{
  int cost = riscv_build_integer_1 (codes, value, mode);

  /* Eliminate leading zeros and end with SRLI.  */
  if (value > 0 && cost > 2)
    {
      struct riscv_integer_op alt_codes[RISCV_MAX_INTEGER_OPS];
      int alt_cost, shift = clz_hwi (value);
      HOST_WIDE_INT shifted_val;

      /* Try filling trailing bits with 1s.  */
      shifted_val = (value << shift) | ((((HOST_WIDE_INT) 1) << shift) - 1);
      alt_cost = 1 + riscv_build_integer_1 (alt_codes, shifted_val, mode);
      if (alt_cost < cost)
	{
	  alt_codes[alt_cost-1].code = LSHIFTRT;
	  alt_codes[alt_cost-1].value = shift;
	  memcpy (codes, alt_codes, sizeof (alt_codes));
	  cost = alt_cost;
	}

      /* Try filling trailing bits with 0s.  */
      shifted_val = value << shift;
      alt_cost = 1 + riscv_build_integer_1 (alt_codes, shifted_val, mode);
      if (alt_cost < cost)
	{
	  alt_codes[alt_cost-1].code = LSHIFTRT;
	  alt_codes[alt_cost-1].value = shift;
	  memcpy (codes, alt_codes, sizeof (alt_codes));
	  cost = alt_cost;
	}
    }

  if (!TARGET_64BIT
      && (value > INT32_MAX || value < INT32_MIN))
    {
      unsigned HOST_WIDE_INT loval = sext_hwi (value, 32);
      unsigned HOST_WIDE_INT hival = sext_hwi ((value - loval) >> 32, 32);
      struct riscv_integer_op alt_codes[RISCV_MAX_INTEGER_OPS];
      struct riscv_integer_op hicode[RISCV_MAX_INTEGER_OPS];
      int hi_cost, lo_cost;

      hi_cost = riscv_build_integer_1 (hicode, hival, mode);
      if (hi_cost < cost)
	{
	  lo_cost = riscv_build_integer_1 (alt_codes, loval, mode);
	  if (lo_cost + hi_cost < cost)
	    {
	      memcpy (codes, alt_codes,
		      lo_cost * sizeof (struct riscv_integer_op));
	      memcpy (codes + lo_cost, hicode,
		      hi_cost * sizeof (struct riscv_integer_op));
	      cost = lo_cost + hi_cost;
	    }
	}
    }

  return cost;
}

/* Return the cost of constructing VAL in the event that a scratch
   register is available.  */

static int
riscv_split_integer_cost (HOST_WIDE_INT val)
{
  int cost;
  unsigned HOST_WIDE_INT loval = sext_hwi (val, 32);
  unsigned HOST_WIDE_INT hival = sext_hwi ((val - loval) >> 32, 32);
  struct riscv_integer_op codes[RISCV_MAX_INTEGER_OPS];

  cost = 2 + riscv_build_integer (codes, loval, VOIDmode);
  if (loval != hival)
    cost += riscv_build_integer (codes, hival, VOIDmode);

  return cost;
}

/* Return the cost of constructing the integer constant VAL.  */

static int
riscv_integer_cost (HOST_WIDE_INT val)
{
  struct riscv_integer_op codes[RISCV_MAX_INTEGER_OPS];
  return MIN (riscv_build_integer (codes, val, VOIDmode),
	      riscv_split_integer_cost (val));
}

/* Try to split a 64b integer into 32b parts, then reassemble.  */

static rtx
riscv_split_integer (HOST_WIDE_INT val, machine_mode mode)
{
  unsigned HOST_WIDE_INT loval = sext_hwi (val, 32);
  unsigned HOST_WIDE_INT hival = sext_hwi ((val - loval) >> 32, 32);
  rtx hi = gen_reg_rtx (mode), lo = gen_reg_rtx (mode);

  riscv_move_integer (lo, lo, loval, mode);

  if (loval == hival)
      hi = gen_rtx_ASHIFT (mode, lo, GEN_INT (32));
  else
    {
      riscv_move_integer (hi, hi, hival, mode);
      hi = gen_rtx_ASHIFT (mode, hi, GEN_INT (32));
    }

  hi = force_reg (mode, hi);
  return gen_rtx_PLUS (mode, hi, lo);
}

/* Return true if X is a thread-local symbol.  */

static bool
riscv_tls_symbol_p (const_rtx x)
{
  return SYMBOL_REF_P (x) && SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Return true if symbol X binds locally.  */

static bool
riscv_symbol_binds_local_p (const_rtx x)
{
  if (SYMBOL_REF_P (x))
    return (SYMBOL_REF_DECL (x)
	    ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	    : SYMBOL_REF_LOCAL_P (x));
  else
    return false;
}

/* Return the method that should be used to access SYMBOL_REF or
   LABEL_REF X.  */

static enum riscv_symbol_type
riscv_classify_symbol (const_rtx x)
{
  if (riscv_tls_symbol_p (x))
    return SYMBOL_TLS;

  if (GET_CODE (x) == SYMBOL_REF && flag_pic && !riscv_symbol_binds_local_p (x))
    return SYMBOL_GOT_DISP;

  switch (riscv_cmodel)
    {
    case CM_MEDLOW:
      return SYMBOL_ABSOLUTE;
    case CM_LARGE:
      if (SYMBOL_REF_P (x))
	return CONSTANT_POOL_ADDRESS_P (x) ? SYMBOL_PCREL : SYMBOL_FORCE_TO_MEM;
      return SYMBOL_PCREL;
    default:
      return SYMBOL_PCREL;
    }
}

/* Classify the base of symbolic expression X.  */

enum riscv_symbol_type
riscv_classify_symbolic_expression (rtx x)
{
  rtx offset;

  split_const (x, &x, &offset);
  if (UNSPEC_ADDRESS_P (x))
    return UNSPEC_ADDRESS_TYPE (x);

  return riscv_classify_symbol (x);
}

/* Return true if X is a symbolic constant.  If it is, store the type of
   the symbol in *SYMBOL_TYPE.  */

bool
riscv_symbolic_constant_p (rtx x, enum riscv_symbol_type *symbol_type)
{
  rtx offset;

  split_const (x, &x, &offset);
  if (UNSPEC_ADDRESS_P (x))
    {
      *symbol_type = UNSPEC_ADDRESS_TYPE (x);
      x = UNSPEC_ADDRESS (x);
    }
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF)
    *symbol_type = riscv_classify_symbol (x);
  else
    return false;

  if (offset == const0_rtx)
    return true;

  /* Nonzero offsets are only valid for references that don't use the GOT.  */
  switch (*symbol_type)
    {
    case SYMBOL_ABSOLUTE:
    case SYMBOL_PCREL:
    case SYMBOL_TLS_LE:
      /* GAS rejects offsets outside the range [-2^31, 2^31-1].  */
      return sext_hwi (INTVAL (offset), 32) == INTVAL (offset);

    default:
      return false;
    }
}

/* Returns the number of instructions necessary to reference a symbol. */

static int riscv_symbol_insns (enum riscv_symbol_type type)
{
  switch (type)
    {
    case SYMBOL_TLS: return 0; /* Depends on the TLS model.  */
    case SYMBOL_ABSOLUTE: return 2; /* LUI + the reference.  */
    case SYMBOL_PCREL: return 2; /* AUIPC + the reference.  */
    case SYMBOL_TLS_LE: return 3; /* LUI + ADD TP + the reference.  */
    case SYMBOL_TLSDESC: return 6; /* 4-instruction call + ADD TP + the reference.  */
    case SYMBOL_GOT_DISP: return 3; /* AUIPC + LD GOT + the reference.  */
    case SYMBOL_FORCE_TO_MEM: return 3; /* AUIPC + LD + the reference.  */
    default: gcc_unreachable ();
    }
}

/* Immediate values loaded by the FLI.S instruction in Chapter 25 of the latest RISC-V ISA
   Manual draft. For details, please see:
   https://github.com/riscv/riscv-isa-manual/releases/tag/isa-449cd0c  */

static unsigned HOST_WIDE_INT fli_value_hf[32] =
{
  0xbcp8, 0x4p8, 0x1p8, 0x2p8, 0x1cp8, 0x20p8, 0x2cp8, 0x30p8,
  0x34p8, 0x35p8, 0x36p8, 0x37p8, 0x38p8, 0x39p8, 0x3ap8, 0x3bp8,
  0x3cp8, 0x3dp8, 0x3ep8, 0x3fp8, 0x40p8, 0x41p8, 0x42p8, 0x44p8,
  0x48p8, 0x4cp8, 0x58p8, 0x5cp8, 0x78p8,
  /* Only used for filling, ensuring that 29 and 30 of HF are the same.  */
  0x78p8,
  0x7cp8, 0x7ep8
};

static unsigned HOST_WIDE_INT fli_value_sf[32] =
{
  0xbf8p20, 0x008p20, 0x378p20, 0x380p20, 0x3b8p20, 0x3c0p20, 0x3d8p20, 0x3e0p20,
  0x3e8p20, 0x3eap20, 0x3ecp20, 0x3eep20, 0x3f0p20, 0x3f2p20, 0x3f4p20, 0x3f6p20,
  0x3f8p20, 0x3fap20, 0x3fcp20, 0x3fep20, 0x400p20, 0x402p20, 0x404p20, 0x408p20,
  0x410p20, 0x418p20, 0x430p20, 0x438p20, 0x470p20, 0x478p20, 0x7f8p20, 0x7fcp20
};

static unsigned HOST_WIDE_INT fli_value_df[32] =
{
  0xbff0p48, 0x10p48, 0x3ef0p48, 0x3f00p48,
  0x3f70p48, 0x3f80p48, 0x3fb0p48, 0x3fc0p48,
  0x3fd0p48, 0x3fd4p48, 0x3fd8p48, 0x3fdcp48,
  0x3fe0p48, 0x3fe4p48, 0x3fe8p48, 0x3fecp48,
  0x3ff0p48, 0x3ff4p48, 0x3ff8p48, 0x3ffcp48,
  0x4000p48, 0x4004p48, 0x4008p48, 0x4010p48,
  0x4020p48, 0x4030p48, 0x4060p48, 0x4070p48,
  0x40e0p48, 0x40f0p48, 0x7ff0p48, 0x7ff8p48
};

/* Display floating-point values at the assembly level, which is consistent
   with the zfa extension of llvm:
   https://reviews.llvm.org/D145645.  */

const char *fli_value_print[32] =
{
  "-1.0", "min", "1.52587890625e-05", "3.0517578125e-05", "0.00390625", "0.0078125", "0.0625", "0.125",
  "0.25", "0.3125", "0.375", "0.4375", "0.5", "0.625", "0.75", "0.875",
  "1.0", "1.25", "1.5", "1.75", "2.0", "2.5", "3.0", "4.0",
  "8.0", "16.0", "128.0", "256.0", "32768.0", "65536.0", "inf", "nan"
};

/* Return index of the FLI instruction table if rtx X is an immediate constant that can
   be moved using a single FLI instruction in zfa extension. Return -1 if not found.  */

int
riscv_float_const_rtx_index_for_fli (rtx x)
{
  unsigned HOST_WIDE_INT *fli_value_array;

  machine_mode mode = GET_MODE (x);

  if (!TARGET_ZFA
      || !CONST_DOUBLE_P(x)
      || mode == VOIDmode
      || (mode == HFmode && !(TARGET_ZFH || TARGET_ZVFH))
      || (mode == SFmode && !TARGET_HARD_FLOAT)
      || (mode == DFmode && !TARGET_DOUBLE_FLOAT))
    return -1;

  if (!SCALAR_FLOAT_MODE_P (mode)
      || GET_MODE_BITSIZE (mode).to_constant () > HOST_BITS_PER_WIDE_INT
      /* Only support up to DF mode.  */
      || GET_MODE_BITSIZE (mode).to_constant () > GET_MODE_BITSIZE (DFmode))
    return -1;

  unsigned HOST_WIDE_INT ival = 0;

  long res[2];
  real_to_target (res,
		  CONST_DOUBLE_REAL_VALUE (x),
		  REAL_MODE_FORMAT (mode));

  if (mode == DFmode)
    {
      int order = BYTES_BIG_ENDIAN ? 1 : 0;
      ival = zext_hwi (res[order], 32);
      ival |= (zext_hwi (res[1 - order], 32) << 32);

      /* When the lower 32 bits are not all 0, it is impossible to be in the table.  */
      if (ival & (unsigned HOST_WIDE_INT)0xffffffff)
	return -1;
    }
  else
      ival = zext_hwi (res[0], 32);

  switch (mode)
    {
      case E_HFmode:
	fli_value_array = fli_value_hf;
	break;
      case E_SFmode:
	fli_value_array = fli_value_sf;
	break;
      case E_DFmode:
	fli_value_array = fli_value_df;
	break;
      default:
	return -1;
    }

  if (fli_value_array[0] == ival)
    return 0;

  if (fli_value_array[1] == ival)
    return 1;

  /* Perform a binary search to find target index.  */
  unsigned l, r, m;

  l = 2;
  r = 31;

  while (l <= r)
    {
      m = (l + r) / 2;
      if (fli_value_array[m] == ival)
	return m;
      else if (fli_value_array[m] < ival)
	l = m+1;
      else
	r = m-1;
    }

  return -1;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */

static bool
riscv_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return riscv_const_insns (x) > 0;
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
riscv_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  enum riscv_symbol_type type;
  rtx base, offset;

  /* There's no way to calculate VL-based values using relocations.  */
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    if (GET_CODE (*iter) == CONST_POLY_INT)
      return true;

  /* There is no assembler syntax for expressing an address-sized
     high part.  */
  if (GET_CODE (x) == HIGH)
    return true;

  if (satisfies_constraint_zfli (x))
   return true;

  split_const (x, &base, &offset);
  if (riscv_symbolic_constant_p (base, &type))
    {
      if (type == SYMBOL_FORCE_TO_MEM)
	return false;

      /* As an optimization, don't spill symbolic constants that are as
	 cheap to rematerialize as to access in the constant pool.  */
      if (SMALL_OPERAND (INTVAL (offset)) && riscv_symbol_insns (type) > 0)
	return true;

      /* As an optimization, avoid needlessly generate dynamic relocations.  */
      if (flag_pic)
	return true;
    }

  /* TLS symbols must be computed by riscv_legitimize_move.  */
  if (tls_referenced_p (x))
    return true;

  return false;
}

/* Return true if register REGNO is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

int
riscv_regno_mode_ok_for_base_p (int regno,
				machine_mode mode ATTRIBUTE_UNUSED,
				bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;
      regno = reg_renumber[regno];
    }

  /* These fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  if (regno == ARG_POINTER_REGNUM || regno == FRAME_POINTER_REGNUM)
    return true;

  return GP_REG_P (regno);
}

/* Get valid index register class.
   The RISC-V base instructions don't support index registers,
   but extensions might support that.  */

enum reg_class
riscv_index_reg_class ()
{
  if (TARGET_XTHEADMEMIDX || TARGET_XTHEADFMEMIDX)
    return GR_REGS;

  return NO_REGS;
}

/* Return true if register REGNO is a valid index register.
   The RISC-V base instructions don't support index registers,
   but extensions might support that.  */

int
riscv_regno_ok_for_index_p (int regno)
{
  if (TARGET_XTHEADMEMIDX || TARGET_XTHEADFMEMIDX)
    return riscv_regno_mode_ok_for_base_p (regno, VOIDmode, 1);

  return 0;
}

/* Return true if X is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
riscv_valid_base_register_p (rtx x, machine_mode mode, bool strict_p)
{
  if (!strict_p && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x)
	  && riscv_regno_mode_ok_for_base_p (REGNO (x), mode, strict_p));
}

/* Return true if, for every base register BASE_REG, (plus BASE_REG X)
   can address a value of mode MODE.  */

static bool
riscv_valid_offset_p (rtx x, machine_mode mode)
{
  /* Check that X is a signed 12-bit number.  */
  if (!const_arith_operand (x, Pmode))
    return false;

  /* We may need to split multiword moves, so make sure that every word
     is accessible.  */
  if (GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD
      && !SMALL_OPERAND (INTVAL (x) + GET_MODE_SIZE (mode).to_constant () - UNITS_PER_WORD))
    return false;

  return true;
}

/* Should a symbol of type SYMBOL_TYPE should be split in two?  */

bool
riscv_split_symbol_type (enum riscv_symbol_type symbol_type)
{
  if (symbol_type == SYMBOL_TLS_LE)
    return true;

  if (!TARGET_EXPLICIT_RELOCS)
    return false;

  return symbol_type == SYMBOL_ABSOLUTE || symbol_type == SYMBOL_PCREL;
}

/* Return true if a LO_SUM can address a value of mode MODE when the
   LO_SUM symbol has type SYM_TYPE.  X is the LO_SUM second operand, which
   is used when the mode is BLKmode.  */

static bool
riscv_valid_lo_sum_p (enum riscv_symbol_type sym_type, machine_mode mode,
		      rtx x)
{
  int align, size;

  /* Check that symbols of type SYMBOL_TYPE can be used to access values
     of mode MODE.  */
  if (riscv_symbol_insns (sym_type) == 0)
    return false;

  /* Check that there is a known low-part relocation.  */
  if (!riscv_split_symbol_type (sym_type))
    return false;

  /* We can't tell size or alignment when we have BLKmode, so try extracing a
     decl from the symbol if possible.  */
  if (mode == BLKmode)
    {
      rtx offset;

      /* Extract the symbol from the LO_SUM operand, if any.  */
      split_const (x, &x, &offset);

      /* Might be a CODE_LABEL.  We can compute align but not size for that,
	 so don't bother trying to handle it.  */
      if (!SYMBOL_REF_P (x))
	return false;

      /* Use worst case assumptions if we don't have a SYMBOL_REF_DECL.  */
      align = (SYMBOL_REF_DECL (x)
	       ? DECL_ALIGN (SYMBOL_REF_DECL (x))
	       : 1);
      size = (SYMBOL_REF_DECL (x)
	      && DECL_SIZE (SYMBOL_REF_DECL (x))
	      && tree_fits_uhwi_p (DECL_SIZE (SYMBOL_REF_DECL (x)))
	      ? tree_to_uhwi (DECL_SIZE (SYMBOL_REF_DECL (x)))
	      : 2*BITS_PER_WORD);
    }
  else
    {
      align = GET_MODE_ALIGNMENT (mode);
      size = GET_MODE_BITSIZE (mode).to_constant ();
    }

  /* We may need to split multiword moves, so make sure that each word
     can be accessed without inducing a carry.  */
  if (size > BITS_PER_WORD
      && (!TARGET_STRICT_ALIGN || size > align))
    return false;

  return true;
}

/* Return true if mode is the RVV enabled mode.
   For example: 'RVVMF2SI' mode is disabled,
   wheras 'RVVM1SI' mode is enabled if MIN_VLEN == 32.  */

bool
riscv_v_ext_vector_mode_p (machine_mode mode)
{
#define ENTRY(MODE, REQUIREMENT, ...)                                          \
  case MODE##mode:                                                             \
    return REQUIREMENT;
  switch (mode)
    {
#include "riscv-vector-switch.def"
    default:
      return false;
    }

  return false;
}

/* Return true if mode is the RVV enabled tuple mode.  */

bool
riscv_v_ext_tuple_mode_p (machine_mode mode)
{
#define TUPLE_ENTRY(MODE, REQUIREMENT, ...)                                    \
  case MODE##mode:                                                             \
    return REQUIREMENT;
  switch (mode)
    {
#include "riscv-vector-switch.def"
    default:
      return false;
    }

  return false;
}

/* Return true if mode is the RVV enabled vls mode.  */

bool
riscv_v_ext_vls_mode_p (machine_mode mode)
{
#define VLS_ENTRY(MODE, REQUIREMENT)                                           \
  case MODE##mode:                                                             \
    return REQUIREMENT;
  switch (mode)
    {
#include "riscv-vector-switch.def"
    default:
      return false;
    }

  return false;
}

/* Return true if it is either of below modes.
   1. RVV vector mode.
   2. RVV tuple mode.
   3. RVV vls mode.  */

static bool
riscv_v_ext_mode_p (machine_mode mode)
{
  return riscv_v_ext_vector_mode_p (mode) || riscv_v_ext_tuple_mode_p (mode)
	 || riscv_v_ext_vls_mode_p (mode);
}

static unsigned
riscv_v_vls_mode_aggregate_gpr_count (unsigned vls_unit_size,
				      unsigned scalar_unit_size)
{
  gcc_assert (vls_unit_size != 0 && scalar_unit_size != 0);

  if (vls_unit_size < scalar_unit_size)
    return 1;

  /* Ensure the vls mode is exact_div by scalar_unit_size.  */
  gcc_assert ((vls_unit_size % scalar_unit_size) == 0);

  return vls_unit_size / scalar_unit_size;
}

static machine_mode
riscv_v_vls_to_gpr_mode (unsigned vls_mode_size)
{
  switch (vls_mode_size)
    {
      case 16:
	return TImode;
      case 8:
	return DImode;
      case 4:
	return SImode;
      case 2:
	return HImode;
      case 1:
	return QImode;
      default:
	gcc_unreachable ();
    }
}

/* Call from ADJUST_NUNITS in riscv-modes.def. Return the correct
   NUNITS size for corresponding machine_mode.  */

poly_int64
riscv_v_adjust_nunits (machine_mode mode, int scale)
{
  gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL);
  if (riscv_v_ext_mode_p (mode))
    {
      if (TARGET_MIN_VLEN == 32)
	scale = scale / 2;
      return riscv_vector_chunks * scale;
    }
  return scale;
}

/* Call from ADJUST_NUNITS in riscv-modes.def. Return the correct
   NUNITS size for corresponding machine_mode.  */

poly_int64
riscv_v_adjust_nunits (machine_mode mode, bool fractional_p, int lmul, int nf)
{
  if (riscv_v_ext_mode_p (mode))
    {
      scalar_mode smode = GET_MODE_INNER (mode);
      int size = GET_MODE_SIZE (smode);
      int nunits_per_chunk = riscv_bytes_per_vector_chunk / size;
      if (fractional_p)
	return nunits_per_chunk / lmul * riscv_vector_chunks * nf;
      else
	return nunits_per_chunk * lmul * riscv_vector_chunks * nf;
    }
  /* Set the disabled RVV modes size as 1 by default.  */
  return 1;
}

/* Call from ADJUST_BYTESIZE in riscv-modes.def.  Return the correct
   BYTE size for corresponding machine_mode.  */

poly_int64
riscv_v_adjust_bytesize (machine_mode mode, int scale)
{
  if (riscv_v_ext_vector_mode_p (mode))
    {
      if (TARGET_XTHEADVECTOR)
	return BYTES_PER_RISCV_VECTOR;

      poly_int64 nunits = GET_MODE_NUNITS (mode);

      if (nunits.coeffs[0] > 8)
	return exact_div (nunits, 8);
      else if (nunits.is_constant ())
	return 1;
      else
	return poly_int64 (1, 1);
    }

  return scale;
}

/* Call from ADJUST_PRECISION in riscv-modes.def.  Return the correct
   PRECISION size for corresponding machine_mode.  */

poly_int64
riscv_v_adjust_precision (machine_mode mode, int scale)
{
  return riscv_v_adjust_nunits (mode, scale);
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT_P is true if REG_OK_STRICT is in
   effect.  */

static bool
riscv_classify_address (struct riscv_address_info *info, rtx x,
			machine_mode mode, bool strict_p)
{
  if (th_classify_address (info, x, mode, strict_p))
    return true;

  switch (GET_CODE (x))
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG;
      info->reg = x;
      info->offset = const0_rtx;
      return riscv_valid_base_register_p (info->reg, mode, strict_p);

    case PLUS:
      /* RVV load/store disallow any offset.  */
      if (riscv_v_ext_mode_p (mode))
	return false;

      info->type = ADDRESS_REG;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      return (riscv_valid_base_register_p (info->reg, mode, strict_p)
	      && riscv_valid_offset_p (info->offset, mode));

    case LO_SUM:
      /* RVV load/store disallow LO_SUM.  */
      if (riscv_v_ext_mode_p (mode))
	return false;

      info->type = ADDRESS_LO_SUM;
      info->reg = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      /* We have to trust the creator of the LO_SUM to do something vaguely
	 sane.  Target-independent code that creates a LO_SUM should also
	 create and verify the matching HIGH.  Target-independent code that
	 adds an offset to a LO_SUM must prove that the offset will not
	 induce a carry.  Failure to do either of these things would be
	 a bug, and we are not required to check for it here.  The RISC-V
	 backend itself should only create LO_SUMs for valid symbolic
	 constants, with the high part being either a HIGH or a copy
	 of _gp. */
      info->symbol_type
	= riscv_classify_symbolic_expression (info->offset);
      return (riscv_valid_base_register_p (info->reg, mode, strict_p)
	      && riscv_valid_lo_sum_p (info->symbol_type, mode, info->offset));

    case CONST_INT:
      /* We only allow the const0_rtx for the RVV load/store.  For example:
	 +----------------------------------------------------------+
	 | li      a5,0						    |
	 | vsetvli zero,a1,e32,m1,ta,ma				    |
	 | vle32.v v24,0(a5)  <- propagate the const 0 to a5 here.  |
	 | vs1r.v  v24,0(a0)					    |
	 +----------------------------------------------------------+
	 It can be folded to:
	 +----------------------------------------------------------+
	 | vsetvli zero,a1,e32,m1,ta,ma				    |
	 | vle32.v v24,0(zero)					    |
	 | vs1r.v  v24,0(a0)					    |
	 +----------------------------------------------------------+
	 This behavior will benefit the underlying RVV auto vectorization.  */
      if (riscv_v_ext_mode_p (mode))
	return x == const0_rtx;

      /* Small-integer addresses don't occur very often, but they
	 are legitimate if x0 is a valid base register.  */
      info->type = ADDRESS_CONST_INT;
      return SMALL_OPERAND (INTVAL (x));

    default:
      return false;
    }
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
riscv_legitimate_address_p (machine_mode mode, rtx x, bool strict_p,
			    code_helper = ERROR_MARK)
{
  /* Disallow RVV modes base address.
     E.g. (mem:SI (subreg:DI (reg:V1DI 155) 0).  */
  if (SUBREG_P (x) && riscv_v_ext_mode_p (GET_MODE (SUBREG_REG (x))))
    return false;
  struct riscv_address_info addr;

  return riscv_classify_address (&addr, x, mode, strict_p);
}

/* Return true if hard reg REGNO can be used in compressed instructions.  */

static bool
riscv_compressed_reg_p (int regno)
{
  /* x8-x15/f8-f15 are compressible registers.  */
  return ((TARGET_RVC  || TARGET_ZCA)
	  && (IN_RANGE (regno, GP_REG_FIRST + 8, GP_REG_FIRST + 15)
	  || IN_RANGE (regno, FP_REG_FIRST + 8, FP_REG_FIRST + 15)));
}

/* Return true if x is an unsigned 5-bit immediate scaled by 4.  */

static bool
riscv_compressed_lw_offset_p (rtx x)
{
  return (CONST_INT_P (x)
	  && (INTVAL (x) & 3) == 0
	  && IN_RANGE (INTVAL (x), 0, CSW_MAX_OFFSET));
}

/* Return true if load/store from/to address x can be compressed.  */

static bool
riscv_compressed_lw_address_p (rtx x)
{
  struct riscv_address_info addr;
  bool result = riscv_classify_address (&addr, x, GET_MODE (x),
					reload_completed);

  /* Return false if address is not compressed_reg + small_offset.  */
  if (!result
      || addr.type != ADDRESS_REG
      /* Before reload, assume all registers are OK.  */
      || (reload_completed
	  && !riscv_compressed_reg_p (REGNO (addr.reg))
	  && addr.reg != stack_pointer_rtx)
      || !riscv_compressed_lw_offset_p (addr.offset))
    return false;

  return result;
}

/* Return the number of instructions needed to load or store a value
   of mode MODE at address X.  Return 0 if X isn't valid for MODE.
   Assume that multiword moves may need to be split into word moves
   if MIGHT_SPLIT_P, otherwise assume that a single load or store is
   enough. */

int
riscv_address_insns (rtx x, machine_mode mode, bool might_split_p)
{
  struct riscv_address_info addr = {};
  int n = 1;

  if (!riscv_classify_address (&addr, x, mode, false))
    {
      /* This could be a pattern from the pic.md file.  In which case we want
	 this address to always have a cost of 3 to make it as expensive as the
	 most expensive symbol.  This prevents constant propagation from
	 preferring symbols over register plus offset.  */
      return 3;
    }

  /* BLKmode is used for single unaligned loads and stores and should
     not count as a multiword mode. */
  if (!riscv_v_ext_vector_mode_p (mode) && mode != BLKmode && might_split_p)
    n += (GET_MODE_SIZE (mode).to_constant () + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (addr.type == ADDRESS_LO_SUM)
    n += riscv_symbol_insns (addr.symbol_type) - 1;

  return n;
}

/* Return the number of instructions needed to load constant X.
   Return 0 if X isn't a valid constant.  */

int
riscv_const_insns (rtx x)
{
  enum riscv_symbol_type symbol_type;
  rtx offset;

  switch (GET_CODE (x))
    {
    case HIGH:
      if (!riscv_symbolic_constant_p (XEXP (x, 0), &symbol_type)
	  || !riscv_split_symbol_type (symbol_type))
	return 0;

      /* This is simply an LUI.  */
      return 1;

    case CONST_INT:
      {
	int cost = riscv_integer_cost (INTVAL (x));
	/* Force complicated constants to memory.  */
	return cost < 4 ? cost : 0;
      }

    case CONST_DOUBLE:
      /* See if we can use FMV directly.  */
      if (satisfies_constraint_zfli (x))
	return 1;

      /* We can use x0 to load floating-point zero.  */
      return x == CONST0_RTX (GET_MODE (x)) ? 1 : 0;
    case CONST_VECTOR:
      {
	/* TODO: This is not accurate, we will need to
	   adapt the COST of CONST_VECTOR in the future
	   for the following cases:

	  - 1. const duplicate vector with element value
	       in range of [-16, 15].
	  - 2. const duplicate vector with element value
	       out range of [-16, 15].
	  - 3. const series vector.
	  ...etc.  */
	if (riscv_v_ext_mode_p (GET_MODE (x)))
	  {
	    /* const series vector.  */
	    rtx base, step;
	    if (const_vec_series_p (x, &base, &step))
	      {
		/* This is not accurate, we will need to adapt the COST
		 * accurately according to BASE && STEP.  */
		return 1;
	      }

	    rtx elt;
	    if (const_vec_duplicate_p (x, &elt))
	      {
		/* We don't allow CONST_VECTOR for DI vector on RV32
		   system since the ELT constant value can not held
		   within a single register to disable reload a DI
		   register vec_duplicate into vmv.v.x.  */
		scalar_mode smode = GET_MODE_INNER (GET_MODE (x));
		if (maybe_gt (GET_MODE_SIZE (smode), UNITS_PER_WORD)
		    && !immediate_operand (elt, Pmode))
		  return 0;
		/* Constants from -16 to 15 can be loaded with vmv.v.i.
		   The Wc0, Wc1 constraints are already covered by the
		   vi constraint so we do not need to check them here
		   separately.  */
		if (satisfies_constraint_vi (x))
		  return 1;

		/* Any int/FP constants can always be broadcast from a
		   scalar register.  Loading of a floating-point
		   constant incurs a literal-pool access.  Allow this in
		   order to increase vectorization possibilities.  */
		int n = riscv_const_insns (elt);
		if (CONST_DOUBLE_P (elt))
		    return 1 + 4; /* vfmv.v.f + memory access.  */
		else
		  {
		    /* We need as many insns as it takes to load the constant
		       into a GPR and one vmv.v.x.  */
		    if (n != 0)
		      return 1 + n;
		    else
		      return 1 + 4; /*vmv.v.x + memory access.  */
		  }
	      }
	  }

	/* TODO: We may support more const vector in the future.  */
	return x == CONST0_RTX (GET_MODE (x)) ? 1 : 0;
      }

    case CONST:
      /* See if we can refer to X directly.  */
      if (riscv_symbolic_constant_p (x, &symbol_type))
	return riscv_symbol_insns (symbol_type);

      /* Otherwise try splitting the constant into a base and offset.  */
      split_const (x, &x, &offset);
      if (offset != 0)
	{
	  int n = riscv_const_insns (x);
	  if (n != 0)
	    return n + riscv_integer_cost (INTVAL (offset));
	}
      return 0;

    case SYMBOL_REF:
    case LABEL_REF:
      return riscv_symbol_insns (riscv_classify_symbol (x));

    /* TODO: In RVV, we get CONST_POLY_INT by using csrr VLENB
       instruction and several scalar shift or mult instructions,
       it is so far unknown. We set it to 4 temporarily.  */
    case CONST_POLY_INT:
      return 4;

    default:
      return 0;
    }
}

/* X is a doubleword constant that can be handled by splitting it into
   two words and loading each word separately.  Return the number of
   instructions required to do this.  */

int
riscv_split_const_insns (rtx x)
{
  unsigned int low, high;

  low = riscv_const_insns (riscv_subword (x, false));
  high = riscv_const_insns (riscv_subword (x, true));
  gcc_assert (low > 0 && high > 0);
  return low + high;
}

/* Return the number of instructions needed to implement INSN,
   given that it loads from or stores to MEM. */

int
riscv_load_store_insns (rtx mem, rtx_insn *insn)
{
  machine_mode mode;
  bool might_split_p;
  rtx set;

  gcc_assert (MEM_P (mem));
  mode = GET_MODE (mem);

  /* Try to prove that INSN does not need to be split.  */
  might_split_p = true;
  if (GET_MODE_BITSIZE (mode).to_constant () <= 32)
    might_split_p = false;
  else if (GET_MODE_BITSIZE (mode).to_constant () == 64)
    {
      set = single_set (insn);
      if (set && !riscv_split_64bit_move_p (SET_DEST (set), SET_SRC (set)))
	might_split_p = false;
    }

  return riscv_address_insns (XEXP (mem, 0), mode, might_split_p);
}

/* Emit a move from SRC to DEST.  Assume that the move expanders can
   handle all moves if !can_create_pseudo_p ().  The distinction is
   important because, unlike emit_move_insn, the move expanders know
   how to force Pmode objects into the constant pool even when the
   constant pool address is not itself legitimate.  */

rtx
riscv_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p ()
	  ? emit_move_insn (dest, src)
	  : emit_move_insn_1 (dest, src));
}

/* Emit an instruction of the form (set TARGET SRC).  */

static rtx
riscv_emit_set (rtx target, rtx src)
{
  emit_insn (gen_rtx_SET (target, src));
  return target;
}

/* Emit an instruction of the form (set DEST (CODE X)).  */

rtx
riscv_emit_unary (enum rtx_code code, rtx dest, rtx x)
{
  return riscv_emit_set (dest, gen_rtx_fmt_e (code, GET_MODE (dest), x));
}

/* Emit an instruction of the form (set DEST (CODE X Y)).  */

rtx
riscv_emit_binary (enum rtx_code code, rtx dest, rtx x, rtx y)
{
  return riscv_emit_set (dest, gen_rtx_fmt_ee (code, GET_MODE (dest), x, y));
}

/* Compute (CODE X Y) and store the result in a new register
   of mode MODE.  Return that new register.  */

static rtx
riscv_force_binary (machine_mode mode, enum rtx_code code, rtx x, rtx y)
{
  return riscv_emit_binary (code, gen_reg_rtx (mode), x, y);
}

static rtx
riscv_swap_instruction (rtx inst)
{
  gcc_assert (GET_MODE (inst) == SImode);
  if (BYTES_BIG_ENDIAN)
    inst = expand_unop (SImode, bswap_optab, inst, gen_reg_rtx (SImode), 1);
  return inst;
}

/* Copy VALUE to a register and return that register.  If new pseudos
   are allowed, copy it into a new register, otherwise use DEST.  */

static rtx
riscv_force_temporary (rtx dest, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (Pmode, value);
  else
    {
      riscv_emit_move (dest, value);
      return dest;
    }
}

/* Wrap symbol or label BASE in an UNSPEC address of type SYMBOL_TYPE,
   then add CONST_INT OFFSET to the result.  */

static rtx
riscv_unspec_address_offset (rtx base, rtx offset,
			     enum riscv_symbol_type symbol_type)
{
  base = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, base),
			 UNSPEC_ADDRESS_FIRST + symbol_type);
  if (offset != const0_rtx)
    base = gen_rtx_PLUS (Pmode, base, offset);
  return gen_rtx_CONST (Pmode, base);
}

/* Return an UNSPEC address with underlying address ADDRESS and symbol
   type SYMBOL_TYPE.  */

rtx
riscv_unspec_address (rtx address, enum riscv_symbol_type symbol_type)
{
  rtx base, offset;

  split_const (address, &base, &offset);
  return riscv_unspec_address_offset (base, offset, symbol_type);
}

/* If OP is an UNSPEC address, return the address to which it refers,
   otherwise return OP itself.  */

static rtx
riscv_strip_unspec_address (rtx op)
{
  rtx base, offset;

  split_const (op, &base, &offset);
  if (UNSPEC_ADDRESS_P (base))
    op = plus_constant (Pmode, UNSPEC_ADDRESS (base), INTVAL (offset));
  return op;
}

/* If riscv_unspec_address (ADDR, SYMBOL_TYPE) is a 32-bit value, add the
   high part to BASE and return the result.  Just return BASE otherwise.
   TEMP is as for riscv_force_temporary.

   The returned expression can be used as the first operand to a LO_SUM.  */

static rtx
riscv_unspec_offset_high (rtx temp, rtx addr, enum riscv_symbol_type symbol_type)
{
  addr = gen_rtx_HIGH (Pmode, riscv_unspec_address (addr, symbol_type));
  return riscv_force_temporary (temp, addr);
}

/* Load an entry from the GOT for a TLS GD access.  */

static rtx riscv_got_load_tls_gd (rtx dest, rtx sym)
{
  if (Pmode == DImode)
    return gen_got_load_tls_gddi (dest, sym);
  else
    return gen_got_load_tls_gdsi (dest, sym);
}

/* Load an entry from the GOT for a TLS IE access.  */

static rtx riscv_got_load_tls_ie (rtx dest, rtx sym)
{
  if (Pmode == DImode)
    return gen_got_load_tls_iedi (dest, sym);
  else
    return gen_got_load_tls_iesi (dest, sym);
}

/* Add in the thread pointer for a TLS LE access.  */

static rtx riscv_tls_add_tp_le (rtx dest, rtx base, rtx sym)
{
  rtx tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
  if (Pmode == DImode)
    return gen_tls_add_tp_ledi (dest, base, tp, sym);
  else
    return gen_tls_add_tp_lesi (dest, base, tp, sym);
}

/* If MODE is MAX_MACHINE_MODE, ADDR appears as a move operand, otherwise
   it appears in a MEM of that mode.  Return true if ADDR is a legitimate
   constant in that context and can be split into high and low parts.
   If so, and if LOW_OUT is nonnull, emit the high part and store the
   low part in *LOW_OUT.  Leave *LOW_OUT unchanged otherwise.

   TEMP is as for riscv_force_temporary and is used to load the high
   part into a register.

   When MODE is MAX_MACHINE_MODE, the low part is guaranteed to be
   a legitimize SET_SRC for an .md pattern, otherwise the low part
   is guaranteed to be a legitimate address for mode MODE.  */

bool
riscv_split_symbol (rtx temp, rtx addr, machine_mode mode, rtx *low_out)
{
  enum riscv_symbol_type symbol_type;

  if ((GET_CODE (addr) == HIGH && mode == MAX_MACHINE_MODE)
      || !riscv_symbolic_constant_p (addr, &symbol_type)
      || riscv_symbol_insns (symbol_type) == 0
      || !riscv_split_symbol_type (symbol_type))
    return false;

  if (low_out)
    switch (symbol_type)
      {
      case SYMBOL_FORCE_TO_MEM:
	return false;

      case SYMBOL_ABSOLUTE:
	{
	  rtx high = gen_rtx_HIGH (Pmode, copy_rtx (addr));
	  high = riscv_force_temporary (temp, high);
	  *low_out = gen_rtx_LO_SUM (Pmode, high, addr);
	}
	break;

      case SYMBOL_PCREL:
	{
	  static unsigned seqno;
	  char buf[32];
	  rtx label;

	  ssize_t bytes = snprintf (buf, sizeof (buf), ".LA%u", seqno);
	  gcc_assert ((size_t) bytes < sizeof (buf));

	  label = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
	  SYMBOL_REF_FLAGS (label) |= SYMBOL_FLAG_LOCAL;
	  /* ??? Ugly hack to make weak symbols work.  May need to change the
	     RTL for the auipc and/or low patterns to get a better fix for
	     this.  */
	  if (! nonzero_address_p (addr))
	    SYMBOL_REF_WEAK (label) = 1;

	  if (temp == NULL)
	    temp = gen_reg_rtx (Pmode);

	  if (Pmode == DImode)
	    emit_insn (gen_auipcdi (temp, copy_rtx (addr), GEN_INT (seqno)));
	  else
	    emit_insn (gen_auipcsi (temp, copy_rtx (addr), GEN_INT (seqno)));

	  *low_out = gen_rtx_LO_SUM (Pmode, temp, label);

	  seqno++;
	}
	break;

      default:
	gcc_unreachable ();
      }

  return true;
}

/* Return a legitimate address for REG + OFFSET.  TEMP is as for
   riscv_force_temporary; it is only needed when OFFSET is not a
   SMALL_OPERAND.  */

static rtx
riscv_add_offset (rtx temp, rtx reg, HOST_WIDE_INT offset)
{
  if (!SMALL_OPERAND (offset))
    {
      rtx high;

      /* Leave OFFSET as a 16-bit offset and put the excess in HIGH.
	 The addition inside the macro CONST_HIGH_PART may cause an
	 overflow, so we need to force a sign-extension check.  */
      high = gen_int_mode (CONST_HIGH_PART (offset), Pmode);
      offset = CONST_LOW_PART (offset);
      high = riscv_force_temporary (temp, high);
      reg = riscv_force_temporary (temp, gen_rtx_PLUS (Pmode, high, reg));
    }
  return plus_constant (Pmode, reg, offset);
}

/* The __tls_get_attr symbol.  */
static GTY(()) rtx riscv_tls_symbol;

/* Return an instruction sequence that calls __tls_get_addr.  SYM is
   the TLS symbol we are referencing and TYPE is the symbol type to use
   (either global dynamic or local dynamic).  RESULT is an RTX for the
   return value location.  */

static rtx_insn *
riscv_call_tls_get_addr (rtx sym, rtx result)
{
  rtx a0 = gen_rtx_REG (Pmode, GP_ARG_FIRST), func;
  rtx_insn *insn;

  if (!riscv_tls_symbol)
    riscv_tls_symbol = init_one_libfunc ("__tls_get_addr");
  func = gen_rtx_MEM (FUNCTION_MODE, riscv_tls_symbol);

  start_sequence ();

  emit_insn (riscv_got_load_tls_gd (a0, sym));
  insn = emit_call_insn (gen_call_value (result, func, const0_rtx,
					 gen_int_mode (RISCV_CC_BASE, SImode)));
  RTL_CONST_CALL_P (insn) = 1;
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), a0);
  insn = get_insns ();

  end_sequence ();

  return insn;
}

/* Generate the code to access LOC, a thread-local SYMBOL_REF, and return
   its address.  The return value will be both a valid address and a valid
   SET_SRC (either a REG or a LO_SUM).  */

static rtx
riscv_legitimize_tls_address (rtx loc)
{
  rtx dest, tp, tmp, a0;
  enum tls_model model = SYMBOL_REF_TLS_MODEL (loc);

#if 0
  /* TLS copy relocs are now deprecated and should not be used.  */
  /* Since we support TLS copy relocs, non-PIC TLS accesses may all use LE.  */
  if (!flag_pic)
    model = TLS_MODEL_LOCAL_EXEC;
#endif

  switch (model)
    {
    case TLS_MODEL_LOCAL_DYNAMIC:
      /* Rely on section anchors for the optimization that LDM TLS
	 provides.  The anchor's address is loaded with GD TLS. */
    case TLS_MODEL_GLOBAL_DYNAMIC:
      if (TARGET_TLSDESC)
	{
	  static unsigned seqno;
	  tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
	  a0 = gen_rtx_REG (Pmode, GP_ARG_FIRST);
	  dest = gen_reg_rtx (Pmode);

	  emit_insn (gen_tlsdesc (Pmode, loc, GEN_INT (seqno)));
	  emit_insn (gen_add3_insn (dest, a0, tp));
	  seqno++;
	}
      else
	{
	  tmp = gen_rtx_REG (Pmode, GP_RETURN);
	  dest = gen_reg_rtx (Pmode);
	  emit_libcall_block (riscv_call_tls_get_addr (loc, tmp), dest, tmp,
			      loc);
	}
      break;

    case TLS_MODEL_INITIAL_EXEC:
      /* la.tls.ie; tp-relative add */
      tp = gen_rtx_REG (Pmode, THREAD_POINTER_REGNUM);
      tmp = gen_reg_rtx (Pmode);
      emit_insn (riscv_got_load_tls_ie (tmp, loc));
      dest = gen_reg_rtx (Pmode);
      emit_insn (gen_add3_insn (dest, tmp, tp));
      break;

    case TLS_MODEL_LOCAL_EXEC:
      tmp = riscv_unspec_offset_high (NULL, loc, SYMBOL_TLS_LE);
      dest = gen_reg_rtx (Pmode);
      emit_insn (riscv_tls_add_tp_le (dest, tmp, loc));
      dest = gen_rtx_LO_SUM (Pmode, dest,
			     riscv_unspec_address (loc, SYMBOL_TLS_LE));
      break;

    default:
      gcc_unreachable ();
    }
  return dest;
}

/* If X is not a valid address for mode MODE, force it into a register.  */

static rtx
riscv_force_address (rtx x, machine_mode mode)
{
  if (!riscv_legitimate_address_p (mode, x, false))
    {
      if (can_create_pseudo_p ())
	return force_reg (Pmode, x);
      else
	{
	  /* It's only safe for the thunk function.
	     Use ra as the temp regiater.  */
	  gcc_assert (riscv_in_thunk_func);
	  rtx reg = RISCV_PROLOGUE_TEMP2 (Pmode);
	  riscv_emit_move (reg, x);
	  return reg;
	}
    }

  return x;
}

/* Modify base + offset so that offset fits within a compressed load/store insn
   and the excess is added to base.  */

static rtx
riscv_shorten_lw_offset (rtx base, HOST_WIDE_INT offset)
{
  rtx addr, high;
  /* Leave OFFSET as an unsigned 5-bit offset scaled by 4 and put the excess
     into HIGH.  */
  high = GEN_INT (offset & ~CSW_MAX_OFFSET);
  offset &= CSW_MAX_OFFSET;
  if (!SMALL_OPERAND (INTVAL (high)))
    high = force_reg (Pmode, high);
  base = force_reg (Pmode, gen_rtx_PLUS (Pmode, high, base));
  addr = plus_constant (Pmode, base, offset);
  return addr;
}

/* Helper for riscv_legitimize_address. Given X, return true if it
   is a left shift by 1, 2 or 3 positions or a multiply by 2, 4 or 8.

   This respectively represent canonical shift-add rtxs or scaled
   memory addresses.  */
static bool
mem_shadd_or_shadd_rtx_p (rtx x)
{
  return ((GET_CODE (x) == ASHIFT
	   || GET_CODE (x) == MULT)
	  && CONST_INT_P (XEXP (x, 1))
	  && ((GET_CODE (x) == ASHIFT && IN_RANGE (INTVAL (XEXP (x, 1)), 1, 3))
	      || (GET_CODE (x) == MULT
		  && IN_RANGE (exact_log2 (INTVAL (XEXP (x, 1))), 1, 3))));
}

/* This function is used to implement LEGITIMIZE_ADDRESS.  If X can
   be legitimized in a way that the generic machinery might not expect,
   return a new address, otherwise return NULL.  MODE is the mode of
   the memory being accessed.  */

static rtx
riscv_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  machine_mode mode)
{
  rtx addr;

  if (riscv_tls_symbol_p (x))
    return riscv_legitimize_tls_address (x);

  /* See if the address can split into a high part and a LO_SUM.  */
  if (riscv_split_symbol (NULL, x, mode, &addr))
    return riscv_force_address (addr, mode);

  /* Handle BASE + OFFSET.  */
  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1))
      && INTVAL (XEXP (x, 1)) != 0)
    {
      rtx base = XEXP (x, 0);
      HOST_WIDE_INT offset = INTVAL (XEXP (x, 1));

      /* Handle (plus (plus (mult (a) (mem_shadd_constant)) (fp)) (C)) case.  */
      if (GET_CODE (base) == PLUS && mem_shadd_or_shadd_rtx_p (XEXP (base, 0))
	  && SMALL_OPERAND (offset))
	{
	  rtx index = XEXP (base, 0);
	  rtx fp = XEXP (base, 1);
	  if (REG_P (fp) && REGNO (fp) == VIRTUAL_STACK_VARS_REGNUM)
	    {

	      /* If we were given a MULT, we must fix the constant
		 as we're going to create the ASHIFT form.  */
	      int shift_val = INTVAL (XEXP (index, 1));
	      if (GET_CODE (index) == MULT)
		shift_val = exact_log2 (shift_val);

	      rtx reg1 = gen_reg_rtx (Pmode);
	      rtx reg2 = gen_reg_rtx (Pmode);
	      rtx reg3 = gen_reg_rtx (Pmode);
	      riscv_emit_binary (PLUS, reg1, fp, GEN_INT (offset));
	      riscv_emit_binary (ASHIFT, reg2, XEXP (index, 0), GEN_INT (shift_val));
	      riscv_emit_binary (PLUS, reg3, reg2, reg1);

	      return reg3;
	    }
	}

      if (!riscv_valid_base_register_p (base, mode, false))
	base = copy_to_mode_reg (Pmode, base);
      if (optimize_function_for_size_p (cfun)
	  && (strcmp (current_pass->name, "shorten_memrefs") == 0)
	  && mode == SImode)
	/* Convert BASE + LARGE_OFFSET into NEW_BASE + SMALL_OFFSET to allow
	   possible compressed load/store.  */
	addr = riscv_shorten_lw_offset (base, offset);
      else
	addr = riscv_add_offset (NULL, base, offset);
      return riscv_force_address (addr, mode);
    }

  return x;
}

/* Load VALUE into DEST.  TEMP is as for riscv_force_temporary.  ORIG_MODE
   is the original src mode before promotion.  */

void
riscv_move_integer (rtx temp, rtx dest, HOST_WIDE_INT value,
		    machine_mode orig_mode)
{
  struct riscv_integer_op codes[RISCV_MAX_INTEGER_OPS];
  machine_mode mode;
  int i, num_ops;
  rtx x;

  mode = GET_MODE (dest);
  /* We use the original mode for the riscv_build_integer call, because HImode
     values are given special treatment.  */
  num_ops = riscv_build_integer (codes, value, orig_mode);

  if (can_create_pseudo_p () && num_ops > 2 /* not a simple constant */
      && num_ops >= riscv_split_integer_cost (value))
    x = riscv_split_integer (value, mode);
  else
    {
      codes[0].value = trunc_int_for_mode (codes[0].value, mode);
      /* Apply each binary operation to X. */
      x = GEN_INT (codes[0].value);

      for (i = 1; i < num_ops; i++)
	{
	  if (!can_create_pseudo_p ())
	    x = riscv_emit_set (temp, x);
	  else
	    x = force_reg (mode, x);
	  codes[i].value = trunc_int_for_mode (codes[i].value, mode);
	  x = gen_rtx_fmt_ee (codes[i].code, mode, x, GEN_INT (codes[i].value));
	}
    }

  riscv_emit_set (dest, x);
}

/* Subroutine of riscv_legitimize_move.  Move constant SRC into register
   DEST given that SRC satisfies immediate_operand but doesn't satisfy
   move_operand.  */

static void
riscv_legitimize_const_move (machine_mode mode, rtx dest, rtx src)
{
  rtx base, offset;

  /* Split moves of big integers into smaller pieces.  */
  if (splittable_const_int_operand (src, mode))
    {
      riscv_move_integer (dest, dest, INTVAL (src), mode);
      return;
    }

  if (satisfies_constraint_zfli (src))
    {
      riscv_emit_set (dest, src);
      return;
    }

  /* Split moves of symbolic constants into high/low pairs.  */
  if (riscv_split_symbol (dest, src, MAX_MACHINE_MODE, &src))
    {
      riscv_emit_set (dest, src);
      return;
    }

  /* Generate the appropriate access sequences for TLS symbols.  */
  if (riscv_tls_symbol_p (src))
    {
      riscv_emit_move (dest, riscv_legitimize_tls_address (src));
      return;
    }

  /* If we have (const (plus symbol offset)), and that expression cannot
     be forced into memory, load the symbol first and add in the offset.  Also
     prefer to do this even if the constant _can_ be forced into memory, as it
     usually produces better code.  */
  split_const (src, &base, &offset);
  if (offset != const0_rtx
      && (targetm.cannot_force_const_mem (mode, src) || can_create_pseudo_p ()))
    {
      base = riscv_force_temporary (dest, base);
      riscv_emit_move (dest, riscv_add_offset (NULL, base, INTVAL (offset)));
      return;
    }

  /* Handle below format.
     (const:DI
       (plus:DI
	 (symbol_ref:DI ("ic") [flags 0x2] <var_decl 0x7fe57740be10 ic>) <- op_0
	 (const_poly_int:DI [16, 16]) // <- op_1
     ))
   */
  if (GET_CODE (src) == CONST && GET_CODE (XEXP (src, 0)) == PLUS
      && CONST_POLY_INT_P (XEXP (XEXP (src, 0), 1)))
    {
      rtx dest_tmp = gen_reg_rtx (mode);
      rtx tmp = gen_reg_rtx (mode);

      riscv_emit_move (dest, XEXP (XEXP (src, 0), 0));
      riscv_legitimize_poly_move (mode, dest_tmp, tmp, XEXP (XEXP (src, 0), 1));

      emit_insn (gen_rtx_SET (dest, gen_rtx_PLUS (mode, dest, dest_tmp)));
      return;
    }

  src = force_const_mem (mode, src);

  /* When using explicit relocs, constant pool references are sometimes
     not legitimate addresses.  */
  riscv_split_symbol (dest, XEXP (src, 0), mode, &XEXP (src, 0));
  riscv_emit_move (dest, src);
}

/* Report when we try to do something that requires vector when vector is
   disabled. This is an error of last resort and isn't very high-quality.  It
   usually involves attempts to measure the vector length in some way.  */

static void
riscv_report_v_required (void)
{
  static bool reported_p = false;

  /* Avoid reporting a slew of messages for a single oversight.  */
  if (reported_p)
    return;

  error ("this operation requires the RVV ISA extension");
  inform (input_location, "you can enable RVV using the command-line"
			  " option %<-march%>, or by using the %<target%>"
			  " attribute or pragma");
  reported_p = true;
}

/* Helper function to operation for rtx_code CODE.  */
static void
riscv_expand_op (enum rtx_code code, machine_mode mode, rtx op0, rtx op1,
		 rtx op2)
{
  if (can_create_pseudo_p ())
    {
      rtx result;
      if (GET_RTX_CLASS (code) == RTX_UNARY)
	result = expand_simple_unop (mode, code, op1, NULL_RTX, false);
      else
	result = expand_simple_binop (mode, code, op1, op2, NULL_RTX, false,
				      OPTAB_DIRECT);
      riscv_emit_move (op0, result);
    }
  else
    {
      rtx pat;
      /* The following implementation is for prologue and epilogue.
	 Because prologue and epilogue can not use pseudo register.
	 We can't using expand_simple_binop or expand_simple_unop.  */
      if (GET_RTX_CLASS (code) == RTX_UNARY)
	pat = gen_rtx_fmt_e (code, mode, op1);
      else
	pat = gen_rtx_fmt_ee (code, mode, op1, op2);
      emit_insn (gen_rtx_SET (op0, pat));
    }
}

/* Expand mult operation with constant integer, multiplicand also used as a
 * temporary register.  */

static void
riscv_expand_mult_with_const_int (machine_mode mode, rtx dest, rtx multiplicand,
				  HOST_WIDE_INT multiplier)
{
  if (multiplier == 0)
    {
      riscv_emit_move (dest, GEN_INT (0));
      return;
    }

  bool neg_p = multiplier < 0;
  unsigned HOST_WIDE_INT multiplier_abs = abs (multiplier);

  if (multiplier_abs == 1)
    {
      if (neg_p)
	riscv_expand_op (NEG, mode, dest, multiplicand, NULL_RTX);
      else
	riscv_emit_move (dest, multiplicand);
    }
  else
    {
      if (pow2p_hwi (multiplier_abs))
	{
	  /*
	    multiplicand = [BYTES_PER_RISCV_VECTOR].
	     1. const_poly_int:P [BYTES_PER_RISCV_VECTOR * 8].
	    Sequence:
		    csrr a5, vlenb
		    slli a5, a5, 3
	    2. const_poly_int:P [-BYTES_PER_RISCV_VECTOR * 8].
	    Sequence:
		    csrr a5, vlenb
		    slli a5, a5, 3
		    neg a5, a5
	  */
	  riscv_expand_op (ASHIFT, mode, dest, multiplicand,
			   gen_int_mode (exact_log2 (multiplier_abs), QImode));
	  if (neg_p)
	    riscv_expand_op (NEG, mode, dest, dest, NULL_RTX);
	}
      else if (pow2p_hwi (multiplier_abs + 1))
	{
	  /*
	    multiplicand = [BYTES_PER_RISCV_VECTOR].
	     1. const_poly_int:P [BYTES_PER_RISCV_VECTOR * 7].
	    Sequence:
		    csrr a5, vlenb
		    slli a4, a5, 3
		    sub a5, a4, a5
	     2. const_poly_int:P [-BYTES_PER_RISCV_VECTOR * 7].
	    Sequence:
		    csrr a5, vlenb
		    slli a4, a5, 3
		    sub a5, a4, a5 + neg a5, a5 => sub a5, a5, a4
	  */
	  riscv_expand_op (ASHIFT, mode, dest, multiplicand,
			   gen_int_mode (exact_log2 (multiplier_abs + 1),
					 QImode));
	  if (neg_p)
	    riscv_expand_op (MINUS, mode, dest, multiplicand, dest);
	  else
	    riscv_expand_op (MINUS, mode, dest, dest, multiplicand);
	}
      else if (pow2p_hwi (multiplier - 1))
	{
	  /*
	    multiplicand = [BYTES_PER_RISCV_VECTOR].
	     1. const_poly_int:P [BYTES_PER_RISCV_VECTOR * 9].
	    Sequence:
		    csrr a5, vlenb
		    slli a4, a5, 3
		    add a5, a4, a5
	     2. const_poly_int:P [-BYTES_PER_RISCV_VECTOR * 9].
	    Sequence:
		    csrr a5, vlenb
		    slli a4, a5, 3
		    add a5, a4, a5
		    neg a5, a5
	  */
	  riscv_expand_op (ASHIFT, mode, dest, multiplicand,
			   gen_int_mode (exact_log2 (multiplier_abs - 1),
					 QImode));
	  riscv_expand_op (PLUS, mode, dest, dest, multiplicand);
	  if (neg_p)
	    riscv_expand_op (NEG, mode, dest, dest, NULL_RTX);
	}
      else
	{
	  /* We use multiplication for remaining cases.  */
	  gcc_assert (
	    TARGET_MUL
	    && "M-extension must be enabled to calculate the poly_int "
	       "size/offset.");
	  riscv_emit_move (dest, gen_int_mode (multiplier, mode));
	  riscv_expand_op (MULT, mode, dest, dest, multiplicand);
	}
    }
}

/* Analyze src and emit const_poly_int mov sequence.  */

void
riscv_legitimize_poly_move (machine_mode mode, rtx dest, rtx tmp, rtx src)
{
  poly_int64 value = rtx_to_poly_int64 (src);
  /* It use HOST_WIDE_INT intead of int since 32bit type is not enough
     for e.g. (const_poly_int:DI [549755813888, 549755813888]).  */
  HOST_WIDE_INT offset = value.coeffs[0];
  HOST_WIDE_INT factor = value.coeffs[1];
  int vlenb = BYTES_PER_RISCV_VECTOR.coeffs[1];
  int div_factor = 0;
  /* Calculate (const_poly_int:MODE [m, n]) using scalar instructions.
     For any (const_poly_int:MODE [m, n]), the calculation formula is as
     follows.
     constant = m - n.
     When minimum VLEN = 32, poly of VLENB = (4, 4).
     base = vlenb(4, 4) or vlenb/2(2, 2) or vlenb/4(1, 1).
     When minimum VLEN > 32, poly of VLENB = (8, 8).
     base = vlenb(8, 8) or vlenb/2(4, 4) or vlenb/4(2, 2) or vlenb/8(1, 1).
     magn = (n, n) / base.
     (m, n) = base * magn + constant.
     This calculation doesn't need div operation.  */

  if (known_le (GET_MODE_SIZE (mode), GET_MODE_SIZE (Pmode)))
    emit_move_insn (tmp, gen_int_mode (BYTES_PER_RISCV_VECTOR, mode));
  else
    {
      emit_move_insn (gen_highpart (Pmode, tmp), CONST0_RTX (Pmode));
      emit_move_insn (gen_lowpart (Pmode, tmp),
		      gen_int_mode (BYTES_PER_RISCV_VECTOR, Pmode));
    }

  if (BYTES_PER_RISCV_VECTOR.is_constant ())
    {
      gcc_assert (value.is_constant ());
      riscv_emit_move (dest, GEN_INT (value.to_constant ()));
      return;
    }
  else
    {
      int max_power = exact_log2 (MAX_POLY_VARIANT);
      for (int i = 0; i <= max_power; i++)
	{
	  int possible_div_factor = 1 << i;
	  if (factor % (vlenb / possible_div_factor) == 0)
	    {
	      div_factor = possible_div_factor;
	      break;
	    }
	}
      gcc_assert (div_factor != 0);
    }

  if (div_factor != 1)
    riscv_expand_op (LSHIFTRT, mode, tmp, tmp,
		     gen_int_mode (exact_log2 (div_factor), QImode));

  riscv_expand_mult_with_const_int (mode, dest, tmp,
				    factor / (vlenb / div_factor));
  HOST_WIDE_INT constant = offset - factor;

  if (constant == 0)
    return;
  else if (SMALL_OPERAND (constant))
    riscv_expand_op (PLUS, mode, dest, dest, gen_int_mode (constant, mode));
  else
    {
      /* Handle the constant value is not a 12-bit value.  */
      rtx high;

      /* Leave OFFSET as a 16-bit offset and put the excess in HIGH.
	 The addition inside the macro CONST_HIGH_PART may cause an
	 overflow, so we need to force a sign-extension check.  */
      high = gen_int_mode (CONST_HIGH_PART (constant), mode);
      constant = CONST_LOW_PART (constant);
      riscv_emit_move (tmp, high);
      riscv_expand_op (PLUS, mode, dest, tmp, dest);
      riscv_expand_op (PLUS, mode, dest, dest, gen_int_mode (constant, mode));
    }
}

/* Adjust scalable frame of vector for prologue && epilogue. */

static void
riscv_v_adjust_scalable_frame (rtx target, poly_int64 offset, bool epilogue)
{
  rtx tmp = RISCV_PROLOGUE_TEMP (Pmode);
  rtx adjust_size = RISCV_PROLOGUE_TEMP2 (Pmode);
  rtx insn, dwarf, adjust_frame_rtx;

  riscv_legitimize_poly_move (Pmode, adjust_size, tmp,
			      gen_int_mode (offset, Pmode));

  if (epilogue)
    insn = gen_add3_insn (target, target, adjust_size);
  else
    insn = gen_sub3_insn (target, target, adjust_size);

  insn = emit_insn (insn);

  RTX_FRAME_RELATED_P (insn) = 1;

  adjust_frame_rtx
    = gen_rtx_SET (target,
		   plus_constant (Pmode, target, epilogue ? offset : -offset));

  dwarf = alloc_reg_note (REG_FRAME_RELATED_EXPR, copy_rtx (adjust_frame_rtx),
			  NULL_RTX);

  REG_NOTES (insn) = dwarf;
}

/* Take care below subreg const_poly_int move:

   1. (set (subreg:DI (reg:TI 237) 8)
	   (subreg:DI (const_poly_int:TI [4, 2]) 8))
      =>
      (set (subreg:DI (reg:TI 237) 8)
	   (const_int 0)) */

static bool
riscv_legitimize_subreg_const_poly_move (machine_mode mode, rtx dest, rtx src)
{
  gcc_assert (SUBREG_P (src) && CONST_POLY_INT_P (SUBREG_REG (src)));
  gcc_assert (SUBREG_BYTE (src).is_constant ());

  int byte_offset = SUBREG_BYTE (src).to_constant ();
  rtx const_poly = SUBREG_REG (src);
  machine_mode subreg_mode = GET_MODE (const_poly);

  if (subreg_mode != TImode) /* Only TImode is needed for now.  */
    return false;

  if (byte_offset == 8)
    {
      /* The const_poly_int cannot exceed int64, just set zero here.  */
      emit_move_insn (dest, CONST0_RTX (mode));
      return true;
    }

  /* The below transform will be covered in somewhere else.
     Thus, ignore this here.
     (set (subreg:DI (reg:TI 237) 0)
	  (subreg:DI (const_poly_int:TI [4, 2]) 0))
     =>
     (set (subreg:DI (reg:TI 237) 0)
	  (const_poly_int:DI [4, 2])) */

  return false;
}

/* If (set DEST SRC) is not a valid move instruction, emit an equivalent
   sequence that is valid.  */

bool
riscv_legitimize_move (machine_mode mode, rtx dest, rtx src)
{
  if (CONST_POLY_INT_P (src))
    {
      /*
	Handle:
	  (insn 183 182 184 6 (set (mem:QI (plus:DI (reg/f:DI 156)
		  (const_int 96 [0x60])) [0  S1 A8])
	  (const_poly_int:QI [8, 8]))
	"../../../../riscv-gcc/libgcc/unwind-dw2.c":1579:3 -1 (nil))
      */
      if (MEM_P (dest))
	{
	  emit_move_insn (dest, force_reg (mode, src));
	  return true;
	}
      poly_int64 value = rtx_to_poly_int64 (src);
      if (!value.is_constant () && !TARGET_VECTOR)
	{
	  riscv_report_v_required ();
	  return false;
	}

      if (satisfies_constraint_vp (src) && GET_MODE (src) == Pmode)
	return false;

      if (GET_MODE_SIZE (mode).to_constant () < GET_MODE_SIZE (Pmode))
	{
	  /* In RV32 system, handle (const_poly_int:QI [m, n])
				    (const_poly_int:HI [m, n]).
	     In RV64 system, handle (const_poly_int:QI [m, n])
				    (const_poly_int:HI [m, n])
				    (const_poly_int:SI [m, n]).  */
	  rtx tmp = gen_reg_rtx (Pmode);
	  riscv_legitimize_poly_move (Pmode, gen_lowpart (Pmode, dest), tmp,
				      src);
	}
      else
	{
	  /* In RV32 system, handle (const_poly_int:SI [m, n])
				    (const_poly_int:DI [m, n]).
	     In RV64 system, handle (const_poly_int:DI [m, n]).
       FIXME: Maybe we could gen SImode in RV32 and then sign-extend to DImode,
       the offset should not exceed 4GiB in general.  */
	  rtx tmp = gen_reg_rtx (mode);
	  riscv_legitimize_poly_move (mode, dest, tmp, src);
	}
      return true;
    }

  if (SUBREG_P (src) && CONST_POLY_INT_P (SUBREG_REG (src))
    && riscv_legitimize_subreg_const_poly_move (mode, dest, src))
    return true;

  /* Expand
       (set (reg:DI target) (subreg:DI (reg:V8QI reg) 0))
     Expand this data movement instead of simply forbid it since
     we can improve the code generation for this following scenario
     by RVV auto-vectorization:
       (set (reg:V8QI 149) (vec_duplicate:V8QI (reg:QI))
       (set (reg:DI target) (subreg:DI (reg:V8QI reg) 0))
     Since RVV mode and scalar mode are in different REG_CLASS,
     we need to explicitly move data from V_REGS to GR_REGS by scalar move.  */
  if (SUBREG_P (src) && riscv_v_ext_mode_p (GET_MODE (SUBREG_REG (src))))
    {
      machine_mode vmode = GET_MODE (SUBREG_REG (src));
      unsigned int mode_size = GET_MODE_SIZE (mode).to_constant ();
      unsigned int vmode_size = GET_MODE_SIZE (vmode).to_constant ();
      /* We should be able to handle both partial and paradoxical subreg.  */
      unsigned int nunits = vmode_size > mode_size ? vmode_size / mode_size : 1;
      scalar_mode smode = as_a<scalar_mode> (mode);
      unsigned int index = SUBREG_BYTE (src).to_constant () / mode_size;
      unsigned int num = known_eq (GET_MODE_SIZE (smode), 8)
	&& !TARGET_VECTOR_ELEN_64 ? 2 : 1;
      bool need_int_reg_p = false;

      if (num == 2)
	{
	  /* If we want to extract 64bit value but ELEN < 64,
	     we use RVV vector mode with EEW = 32 to extract
	     the highpart and lowpart.  */
	  need_int_reg_p = smode == DFmode;
	  smode = SImode;
	  nunits = nunits * 2;
	}

      if (riscv_vector::get_vector_mode (smode, nunits).exists (&vmode))
	{
	  rtx v = gen_lowpart (vmode, SUBREG_REG (src));
	  rtx int_reg = dest;

	  if (need_int_reg_p)
	    {
	      int_reg = gen_reg_rtx (DImode);
	      emit_move_insn (int_reg, gen_lowpart (GET_MODE (int_reg), dest));
	    }

	  for (unsigned int i = 0; i < num; i++)
	    {
	      rtx result;
	      if (num == 1)
		result = int_reg;
	      else if (i == 0)
		result = gen_lowpart (smode, int_reg);
	      else
		result = gen_reg_rtx (smode);

	      riscv_vector::emit_vec_extract (result, v,
					      gen_int_mode (index + i, Pmode));

	      if (i == 1)
		{
		  if (UNITS_PER_WORD < mode_size)
		    /* If Pmode = SImode and mode = DImode, we just need to
		       extract element of index = 1 from the vector and move it
		       into the highpart of the DEST since DEST consists of 2
		       scalar registers.  */
		    emit_move_insn (gen_highpart (smode, int_reg), result);
		  else
		    {
		      rtx tmp = expand_binop (Pmode, ashl_optab,
					      gen_lowpart (Pmode, result),
					      gen_int_mode (32, Pmode),
					      NULL_RTX, 0, OPTAB_DIRECT);
		      rtx tmp2 = expand_binop (Pmode, ior_optab, tmp, int_reg,
					       NULL_RTX, 0, OPTAB_DIRECT);
		      emit_move_insn (int_reg, tmp2);
		    }
		}
	    }

	  if (need_int_reg_p)
	    emit_move_insn (dest, gen_lowpart (GET_MODE (dest), int_reg));
	  else
	    emit_move_insn (dest, int_reg);
	}
      else
	gcc_unreachable ();

      return true;
    }
  /* Expand
       (set (reg:QI target) (mem:QI (address)))
     to
       (set (reg:DI temp) (zero_extend:DI (mem:QI (address))))
       (set (reg:QI target) (subreg:QI (reg:DI temp) 0))
     with auto-sign/zero extend.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode).to_constant () < UNITS_PER_WORD
      && can_create_pseudo_p ()
      && MEM_P (src))
    {
      rtx temp_reg;
      int zero_extend_p;

      temp_reg = gen_reg_rtx (word_mode);
      zero_extend_p = (LOAD_EXTEND_OP (mode) == ZERO_EXTEND);
      emit_insn (gen_extend_insn (temp_reg, src, word_mode, mode,
				  zero_extend_p));
      riscv_emit_move (dest, gen_lowpart (mode, temp_reg));
      return true;
    }

  if (!register_operand (dest, mode) && !reg_or_0_operand (src, mode))
    {
      rtx reg;

      if (GET_CODE (src) == CONST_INT)
	{
	  /* Apply the equivalent of PROMOTE_MODE here for constants to
	     improve cse.  */
	  machine_mode promoted_mode = mode;
	  if (GET_MODE_CLASS (mode) == MODE_INT
	      && GET_MODE_SIZE (mode).to_constant () < UNITS_PER_WORD)
	    promoted_mode = word_mode;

	  if (splittable_const_int_operand (src, mode))
	    {
	      reg = gen_reg_rtx (promoted_mode);
	      riscv_move_integer (reg, reg, INTVAL (src), mode);
	    }
	  else
	    reg = force_reg (promoted_mode, src);

	  if (promoted_mode != mode)
	    reg = gen_lowpart (mode, reg);
	}
      else
	reg = force_reg (mode, src);
      riscv_emit_move (dest, reg);
      return true;
    }

  /* In order to fit NaN boxing, expand
     (set FP_REG (reg:HF src))
     to
     (set (reg:SI/DI mask) (const_int -65536)
     (set (reg:SI/DI temp) (zero_extend:SI/DI (subreg:HI (reg:HF src) 0)))
     (set (reg:SI/DI temp) (ior:SI/DI (reg:SI/DI mask) (reg:SI/DI temp)))
     (set (reg:HF dest) (unspec:HF [ (reg:SI/DI temp) ] UNSPEC_FMV_SFP16_X))
     */

 if (TARGET_HARD_FLOAT
     && !TARGET_ZFHMIN && mode == HFmode
     && REG_P (dest) && FP_REG_P (REGNO (dest))
     && REG_P (src) && !FP_REG_P (REGNO (src))
     && can_create_pseudo_p ())
   {
     rtx mask = force_reg (word_mode, gen_int_mode (-65536, word_mode));
     rtx temp = gen_reg_rtx (word_mode);
     emit_insn (gen_extend_insn (temp,
				 simplify_gen_subreg (HImode, src, mode, 0),
				 word_mode, HImode, 1));
     if (word_mode == SImode)
       emit_insn (gen_iorsi3 (temp, mask, temp));
     else
       emit_insn (gen_iordi3 (temp, mask, temp));

     riscv_emit_move (dest, gen_rtx_UNSPEC (HFmode, gen_rtvec (1, temp),
					    UNSPEC_FMV_SFP16_X));

     return true;
   }

  /* We need to deal with constants that would be legitimate
     immediate_operands but aren't legitimate move_operands.  */
  if (CONSTANT_P (src) && !move_operand (src, mode))
    {
      riscv_legitimize_const_move (mode, dest, src);
      set_unique_reg_note (get_last_insn (), REG_EQUAL, copy_rtx (src));
      return true;
    }

  /* RISC-V GCC may generate non-legitimate address due to we provide some
     pattern for optimize access PIC local symbol and it's make GCC generate
     unrecognizable instruction during optmizing.  */

  if (MEM_P (dest) && !riscv_legitimate_address_p (mode, XEXP (dest, 0),
						   reload_completed))
    {
      XEXP (dest, 0) = riscv_force_address (XEXP (dest, 0), mode);
    }

  if (MEM_P (src) && !riscv_legitimate_address_p (mode, XEXP (src, 0),
						  reload_completed))
    {
      XEXP (src, 0) = riscv_force_address (XEXP (src, 0), mode);
    }

  return false;
}

/* Return true if there is an instruction that implements CODE and accepts
   X as an immediate operand. */

static int
riscv_immediate_operand_p (int code, HOST_WIDE_INT x)
{
  switch (code)
    {
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      /* All shift counts are truncated to a valid constant.  */
      return true;

    case AND:
    case IOR:
    case XOR:
    case PLUS:
    case LT:
    case LTU:
      /* These instructions take 12-bit signed immediates.  */
      return SMALL_OPERAND (x);

    case LE:
      /* We add 1 to the immediate and use SLT.  */
      return SMALL_OPERAND (x + 1);

    case LEU:
      /* Likewise SLTU, but reject the always-true case.  */
      return SMALL_OPERAND (x + 1) && x + 1 != 0;

    case GE:
    case GEU:
      /* We can emulate an immediate of 1 by using GT/GTU against x0.  */
      return x == 1;

    default:
      /* By default assume that x0 can be used for 0.  */
      return x == 0;
    }
}

/* Return the cost of binary operation X, given that the instruction
   sequence for a word-sized or smaller operation takes SIGNLE_INSNS
   instructions and that the sequence of a double-word operation takes
   DOUBLE_INSNS instructions.  */

static int
riscv_binary_cost (rtx x, int single_insns, int double_insns)
{
  if (!riscv_v_ext_mode_p (GET_MODE (x))
      && GET_MODE_SIZE (GET_MODE (x)).to_constant () == UNITS_PER_WORD * 2)
    return COSTS_N_INSNS (double_insns);
  return COSTS_N_INSNS (single_insns);
}

/* Return the cost of sign- or zero-extending OP.  */

static int
riscv_extend_cost (rtx op, bool unsigned_p)
{
  if (MEM_P (op))
    return 0;

  if (unsigned_p && GET_MODE (op) == QImode)
    /* We can use ANDI.  */
    return COSTS_N_INSNS (1);

  /* ZBA provide zext.w.  */
  if (TARGET_ZBA && TARGET_64BIT && unsigned_p && GET_MODE (op) == SImode)
    return COSTS_N_INSNS (1);

  /* ZBB provide zext.h, sext.b and sext.h.  */
  if (TARGET_ZBB)
    {
      if (!unsigned_p && GET_MODE (op) == QImode)
	return COSTS_N_INSNS (1);

      if (GET_MODE (op) == HImode)
	return COSTS_N_INSNS (1);
    }

  if (!unsigned_p && GET_MODE (op) == SImode)
    /* We can use SEXT.W.  */
    return COSTS_N_INSNS (1);

  /* We need to use a shift left and a shift right.  */
  return COSTS_N_INSNS (2);
}

/* Implement TARGET_RTX_COSTS.  */

#define SINGLE_SHIFT_COST 1

static bool
riscv_rtx_costs (rtx x, machine_mode mode, int outer_code, int opno ATTRIBUTE_UNUSED,
		 int *total, bool speed)
{
  /* TODO: We set RVV instruction cost as 1 by default.
     Cost Model need to be well analyzed and supported in the future. */
  if (riscv_v_ext_mode_p (mode))
    {
      *total = COSTS_N_INSNS (1);
      return true;
    }

  bool float_mode_p = FLOAT_MODE_P (mode);
  int cost;

  switch (GET_CODE (x))
    {
    case SET:
      /* If we are called for an INSN that's a simple set of a register,
	 then cost based on the SET_SRC alone.  */
      if (outer_code == INSN
	  && register_operand (SET_DEST (x), GET_MODE (SET_DEST (x))))
	{
	  riscv_rtx_costs (SET_SRC (x), mode, outer_code, opno, total, speed);
	  return true;
	}

      /* Otherwise return FALSE indicating we should recurse into both the
	 SET_DEST and SET_SRC combining the cost of both.  */
      return false;

    case CONST_INT:
      /* trivial constants checked using OUTER_CODE in case they are
	 encodable in insn itself w/o need for additional insn(s).  */
      if (riscv_immediate_operand_p (outer_code, INTVAL (x)))
	{
	  *total = 0;
	  return true;
	}
      /* Fall through.  */

    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
      /* With TARGET_SUPPORTS_WIDE_INT const int can't be in CONST_DOUBLE
	 rtl object. Weird recheck due to switch-case fall through above.  */
      if (GET_CODE (x) == CONST_DOUBLE)
	gcc_assert (GET_MODE (x) != VOIDmode);
      /* Fall through.  */

    case CONST:
      /* Non trivial CONST_INT Fall through: check if need multiple insns.  */
      if ((cost = riscv_const_insns (x)) > 0)
	{
	  /* 1. Hoist will GCSE constants only if TOTAL returned is non-zero.
	     2. For constants loaded more than once, the approach so far has
		been to duplicate the operation than to CSE the constant.
	     3. TODO: make cost more accurate specially if riscv_const_insns
		returns > 1.  */
	  if (outer_code == SET || GET_MODE (x) == VOIDmode)
	    *total = COSTS_N_INSNS (1);
	}
      else /* The instruction will be fetched from the constant pool.  */
	*total = COSTS_N_INSNS (riscv_symbol_insns (SYMBOL_ABSOLUTE));
      return true;

    case MEM:
      /* If the address is legitimate, return the number of
	 instructions it needs.  */
      if ((cost = riscv_address_insns (XEXP (x, 0), mode, true)) > 0)
	{
	  /* When optimizing for size, make uncompressible 32-bit addresses
	     more expensive so that compressible 32-bit addresses are
	     preferred.  */
	  if ((TARGET_RVC || TARGET_ZCA)
	      && !speed && riscv_mshorten_memrefs && mode == SImode
	      && !riscv_compressed_lw_address_p (XEXP (x, 0)))
	    cost++;

	  *total = COSTS_N_INSNS (cost + tune_param->memory_cost);
	  return true;
	}
      /* Otherwise use the default handling.  */
      return false;

    case IF_THEN_ELSE:
      if ((TARGET_SFB_ALU || TARGET_XTHEADCONDMOV)
	  && reg_or_0_operand (XEXP (x, 1), mode)
	  && sfb_alu_operand (XEXP (x, 2), mode)
	  && comparison_operator (XEXP (x, 0), VOIDmode))
	{
	  /* For predicated conditional-move operations we assume the cost
	     of a single instruction even though there are actually two.  */
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else if (TARGET_ZICOND_LIKE
	       && outer_code == SET
	       && ((GET_CODE (XEXP (x, 1)) == REG
		    && XEXP (x, 2) == CONST0_RTX (GET_MODE (XEXP (x, 1))))
		   || (GET_CODE (XEXP (x, 2)) == REG
		       && XEXP (x, 1) == CONST0_RTX (GET_MODE (XEXP (x, 2))))
		   || (GET_CODE (XEXP (x, 1)) == REG
		       && rtx_equal_p (XEXP (x, 1), XEXP (XEXP (x, 0), 0)))
		   || (GET_CODE (XEXP (x, 1)) == REG
		       && rtx_equal_p (XEXP (x, 2), XEXP (XEXP (x, 0), 0)))))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else if (LABEL_REF_P (XEXP (x, 1)) && XEXP (x, 2) == pc_rtx)
	{
	  if (equality_operator (XEXP (x, 0), mode)
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTRACT)
	    {
	      *total = COSTS_N_INSNS (SINGLE_SHIFT_COST + 1);
	      return true;
	    }
	  if (ordered_comparison_operator (XEXP (x, 0), mode))
	    {
	      *total = COSTS_N_INSNS (1);
	      return true;
	    }
	}
      return false;

    case NOT:
      *total = COSTS_N_INSNS (GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD ? 2 : 1);
      return false;

    case AND:
      /* slli.uw pattern for zba.  */
      if (TARGET_ZBA && TARGET_64BIT && mode == DImode
	  && GET_CODE (XEXP (x, 0)) == ASHIFT)
	{
	  rtx and_rhs = XEXP (x, 1);
	  rtx ashift_lhs = XEXP (XEXP (x, 0), 0);
	  rtx ashift_rhs = XEXP (XEXP (x, 0), 1);
	  if (register_operand (ashift_lhs, GET_MODE (ashift_lhs))
	      && CONST_INT_P (ashift_rhs)
	      && CONST_INT_P (and_rhs)
	      && ((INTVAL (and_rhs) >> INTVAL (ashift_rhs)) == 0xffffffff))
	    *total = COSTS_N_INSNS (1);
	    return true;
	}
      /* bclri pattern for zbs.  */
      if (TARGET_ZBS
	  && not_single_bit_mask_operand (XEXP (x, 1), VOIDmode))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* bclr pattern for zbs.  */
      if (TARGET_ZBS
	  && register_operand (XEXP (x, 1), GET_MODE (XEXP (x, 1)))
	  && GET_CODE (XEXP (x, 0)) == ROTATE
	  && CONST_INT_P (XEXP ((XEXP (x, 0)), 0))
	  && INTVAL (XEXP ((XEXP (x, 0)), 0)) == -2)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}

      gcc_fallthrough ();
    case IOR:
    case XOR:
      /* orn, andn and xorn pattern for zbb.  */
      if (TARGET_ZBB
	  && GET_CODE (XEXP (x, 0)) == NOT)
	{
	  *total = riscv_binary_cost (x, 1, 2);
	  return true;
	}

      /* bset[i] and binv[i] pattern for zbs.  */
      if ((GET_CODE (x) == IOR || GET_CODE (x) == XOR)
	  && TARGET_ZBS
	  && ((GET_CODE (XEXP (x, 0)) == ASHIFT
	      && CONST_INT_P (XEXP (XEXP (x, 0), 0)))
	      || single_bit_mask_operand (XEXP (x, 1), VOIDmode)))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}

      /* Double-word operations use two single-word operations.  */
      *total = riscv_binary_cost (x, 1, 2);
      return false;

    case ZERO_EXTRACT:
      /* This is an SImode shift.  */
      if (outer_code == SET
	  && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (x, 2))
	  && (INTVAL (XEXP (x, 2)) > 0)
	  && (INTVAL (XEXP (x, 1)) + INTVAL (XEXP (x, 2)) == 32))
	{
	  *total = COSTS_N_INSNS (SINGLE_SHIFT_COST);
	  return true;
	}
      /* bit extraction pattern (zbs:bext, xtheadbs:tst).  */
      if ((TARGET_ZBS || TARGET_XTHEADBS) && outer_code == SET
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) == 1)
	{
	  *total = COSTS_N_INSNS (SINGLE_SHIFT_COST);
	  return true;
	}
      gcc_fallthrough ();
    case SIGN_EXTRACT:
      if (TARGET_XTHEADBB && outer_code == SET
	  && CONST_INT_P (XEXP (x, 1))
	  && CONST_INT_P (XEXP (x, 2)))
	{
	  *total = COSTS_N_INSNS (SINGLE_SHIFT_COST);
	  return true;
	}
      return false;

    case ASHIFT:
      /* bset pattern for zbs.  */
      if (TARGET_ZBS
	  && CONST_INT_P (XEXP (x, 0))
	  && INTVAL (XEXP (x, 0)) == 1)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      gcc_fallthrough ();
    case ASHIFTRT:
    case LSHIFTRT:
      *total = riscv_binary_cost (x, SINGLE_SHIFT_COST,
				  CONSTANT_P (XEXP (x, 1)) ? 4 : 9);
      return false;

    case ABS:
      *total = COSTS_N_INSNS (float_mode_p ? 1 : 3);
      return false;

    case LO_SUM:
      *total = set_src_cost (XEXP (x, 0), mode, speed);
      return true;

    case LT:
      /* This is an SImode shift.  */
      if (outer_code == SET && GET_MODE (x) == DImode
	  && GET_MODE (XEXP (x, 0)) == SImode)
	{
	  *total = COSTS_N_INSNS (SINGLE_SHIFT_COST);
	  return true;
	}
      /* Fall through.  */
    case LTU:
    case LE:
    case LEU:
    case GT:
    case GTU:
    case GE:
    case GEU:
    case EQ:
    case NE:
      /* Branch comparisons have VOIDmode, so use the first operand's
	 mode instead.  */
      mode = GET_MODE (XEXP (x, 0));
      if (float_mode_p)
	*total = tune_param->fp_add[mode == DFmode];
      else
	*total = riscv_binary_cost (x, 1, 3);
      return false;

    case UNORDERED:
    case ORDERED:
      /* (FEQ(A, A) & FEQ(B, B)) compared against 0.  */
      mode = GET_MODE (XEXP (x, 0));
      *total = tune_param->fp_add[mode == DFmode] + COSTS_N_INSNS (2);
      return false;

    case UNEQ:
      /* (FEQ(A, A) & FEQ(B, B)) compared against FEQ(A, B).  */
      mode = GET_MODE (XEXP (x, 0));
      *total = tune_param->fp_add[mode == DFmode] + COSTS_N_INSNS (3);
      return false;

    case LTGT:
      /* (FLT(A, A) || FGT(B, B)).  */
      mode = GET_MODE (XEXP (x, 0));
      *total = tune_param->fp_add[mode == DFmode] + COSTS_N_INSNS (2);
      return false;

    case UNGE:
    case UNGT:
    case UNLE:
    case UNLT:
      /* FLT or FLE, but guarded by an FFLAGS read and write.  */
      mode = GET_MODE (XEXP (x, 0));
      *total = tune_param->fp_add[mode == DFmode] + COSTS_N_INSNS (4);
      return false;

    case MINUS:
      if (float_mode_p)
	*total = tune_param->fp_add[mode == DFmode];
      else
	*total = riscv_binary_cost (x, 1, 4);
      return false;

    case PLUS:
      /* add.uw pattern for zba.  */
      if (TARGET_ZBA
	  && (TARGET_64BIT && (mode == DImode))
	  && GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	  && register_operand (XEXP (XEXP (x, 0), 0),
			       GET_MODE (XEXP (XEXP (x, 0), 0)))
	  && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* shNadd pattern for zba.  */
      if (TARGET_ZBA
	  && ((!TARGET_64BIT && (mode == SImode)) ||
	      (TARGET_64BIT && (mode == DImode)))
	  && (GET_CODE (XEXP (x, 0)) == ASHIFT)
	  && register_operand (XEXP (XEXP (x, 0), 0),
			       GET_MODE (XEXP (XEXP (x, 0), 0)))
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && IN_RANGE (INTVAL (XEXP (XEXP (x, 0), 1)), 1, 3))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* Before strength-reduction, the shNadd can be expressed as the addition
	 of a multiplication with a power-of-two.  If this case is not handled,
	 the strength-reduction in expmed.c will calculate an inflated cost. */
      if (TARGET_ZBA
	  && mode == word_mode
	  && GET_CODE (XEXP (x, 0)) == MULT
	  && register_operand (XEXP (XEXP (x, 0), 0),
			       GET_MODE (XEXP (XEXP (x, 0), 0)))
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && pow2p_hwi (INTVAL (XEXP (XEXP (x, 0), 1)))
	  && IN_RANGE (exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1))), 1, 3))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* shNadd.uw pattern for zba.
	 [(set (match_operand:DI 0 "register_operand" "=r")
	       (plus:DI
		 (and:DI (ashift:DI (match_operand:DI 1 "register_operand" "r")
				    (match_operand:QI 2 "immediate_operand" "I"))
			 (match_operand 3 "immediate_operand" ""))
		 (match_operand:DI 4 "register_operand" "r")))]
	 "TARGET_64BIT && TARGET_ZBA
	  && (INTVAL (operands[2]) >= 1) && (INTVAL (operands[2]) <= 3)
	  && (INTVAL (operands[3]) >> INTVAL (operands[2])) == 0xffffffff"
      */
      if (TARGET_ZBA
	  && (TARGET_64BIT && (mode == DImode))
	  && (GET_CODE (XEXP (x, 0)) == AND)
	  && register_operand (XEXP (x, 1), GET_MODE (XEXP (x, 1))))
	{
	  do {
	    rtx and_lhs = XEXP (XEXP (x, 0), 0);
	    rtx and_rhs = XEXP (XEXP (x, 0), 1);
	    if (GET_CODE (and_lhs) != ASHIFT)
	      break;
	    if (!CONST_INT_P (and_rhs))
	      break;

	    rtx ashift_rhs = XEXP (and_lhs, 1);

	    if (!CONST_INT_P (ashift_rhs)
		|| !IN_RANGE (INTVAL (ashift_rhs), 1, 3))
	      break;

	    if (CONST_INT_P (and_rhs)
		&& ((INTVAL (and_rhs) >> INTVAL (ashift_rhs)) == 0xffffffff))
	      {
		*total = COSTS_N_INSNS (1);
		return true;
	      }
	  } while (false);
	}

      if (float_mode_p)
	*total = tune_param->fp_add[mode == DFmode];
      else
	*total = riscv_binary_cost (x, 1, 4);
      return false;

    case NEG:
      {
	rtx op = XEXP (x, 0);
	if (GET_CODE (op) == FMA && !HONOR_SIGNED_ZEROS (mode))
	  {
	    *total = (tune_param->fp_mul[mode == DFmode]
		      + set_src_cost (XEXP (op, 0), mode, speed)
		      + set_src_cost (XEXP (op, 1), mode, speed)
		      + set_src_cost (XEXP (op, 2), mode, speed));
	    return true;
	  }
      }

      if (float_mode_p)
	*total = tune_param->fp_add[mode == DFmode];
      else
	*total = COSTS_N_INSNS (GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD ? 4 : 1);
      return false;

    case MULT:
      if (float_mode_p)
	*total = tune_param->fp_mul[mode == DFmode];
      else if (!(TARGET_MUL || TARGET_ZMMUL))
	/* Estimate the cost of a library call.  */
	*total = COSTS_N_INSNS (speed ? 32 : 6);
      else if (GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD)
	*total = 3 * tune_param->int_mul[0] + COSTS_N_INSNS (2);
      else if (!speed)
	*total = COSTS_N_INSNS (1);
      else
	*total = tune_param->int_mul[mode == DImode];
      return false;

    case DIV:
    case SQRT:
    case MOD:
      if (float_mode_p)
	{
	  *total = tune_param->fp_div[mode == DFmode];
	  return false;
	}
      /* Fall through.  */

    case UDIV:
    case UMOD:
      if (!TARGET_DIV)
	/* Estimate the cost of a library call.  */
	*total = COSTS_N_INSNS (speed ? 32 : 6);
      else if (speed)
	*total = tune_param->int_div[mode == DImode];
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case ZERO_EXTEND:
      /* This is an SImode shift.  */
      if (GET_CODE (XEXP (x, 0)) == LSHIFTRT)
	{
	  *total = COSTS_N_INSNS (SINGLE_SHIFT_COST);
	  return true;
	}
      /* Fall through.  */
    case SIGN_EXTEND:
      *total = riscv_extend_cost (XEXP (x, 0), GET_CODE (x) == ZERO_EXTEND);
      return false;

    case BSWAP:
      if (TARGET_ZBB)
	{
	  /* RISC-V only defines rev8 for XLEN, so we will need an extra
	     shift-right instruction for smaller modes. */
	  *total = COSTS_N_INSNS (mode == word_mode ? 1 : 2);
	  return true;
	}
      return false;

    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
      *total = tune_param->fp_add[mode == DFmode];
      return false;

    case FMA:
      *total = (tune_param->fp_mul[mode == DFmode]
		+ set_src_cost (XEXP (x, 0), mode, speed)
		+ set_src_cost (XEXP (x, 1), mode, speed)
		+ set_src_cost (XEXP (x, 2), mode, speed));
      return true;

    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_AUIPC)
	{
	  /* Make AUIPC cheap to avoid spilling its result to the stack.  */
	  *total = 1;
	  return true;
	}
      return false;

    default:
      return false;
    }
}

/* Implement TARGET_ADDRESS_COST.  */

static int
riscv_address_cost (rtx addr, machine_mode mode,
		    addr_space_t as ATTRIBUTE_UNUSED,
		    bool speed ATTRIBUTE_UNUSED)
{
  /* When optimizing for size, make uncompressible 32-bit addresses more
   * expensive so that compressible 32-bit addresses are preferred.  */
  if ((TARGET_RVC || TARGET_ZCA)
      && !speed && riscv_mshorten_memrefs && mode == SImode
      && !riscv_compressed_lw_address_p (addr))
    return riscv_address_insns (addr, mode, false) + 1;
  return riscv_address_insns (addr, mode, false);
}

/* Implement TARGET_INSN_COST.  We factor in the branch cost in the cost
   calculation for conditional branches: one unit is considered the cost
   of microarchitecture-dependent actual branch execution and therefore
   multiplied by BRANCH_COST and any remaining units are considered fixed
   branch overhead.  Branches on a floating-point condition incur an extra
   instruction cost as they will be split into an FCMP operation followed
   by a branch on an integer condition.  */

static int
riscv_insn_cost (rtx_insn *insn, bool speed)
{
  rtx x = PATTERN (insn);
  int cost = pattern_cost (x, speed);

  if (JUMP_P (insn))
    {
      if (GET_CODE (x) == PARALLEL)
	x = XVECEXP (x, 0, 0);
      if (GET_CODE (x) == SET
	  && GET_CODE (SET_DEST (x)) == PC
	  && GET_CODE (SET_SRC (x)) == IF_THEN_ELSE)
	{
	  cost += COSTS_N_INSNS (BRANCH_COST (speed, false) - 1);
	  if (FLOAT_MODE_P (GET_MODE (XEXP (XEXP (SET_SRC (x), 0), 0))))
	    cost += COSTS_N_INSNS (1);
	}
    }
  return cost;
}

/* Implement TARGET_MAX_NOCE_IFCVT_SEQ_COST.  Like the default implementation,
   but we consider cost units of branch instructions equal to cost units of
   other instructions.  */

static unsigned int
riscv_max_noce_ifcvt_seq_cost (edge e)
{
  bool predictable_p = predictable_edge_p (e);

  if (predictable_p)
    {
      if (OPTION_SET_P (param_max_rtl_if_conversion_predictable_cost))
	return param_max_rtl_if_conversion_predictable_cost;
    }
  else
    {
      if (OPTION_SET_P (param_max_rtl_if_conversion_unpredictable_cost))
	return param_max_rtl_if_conversion_unpredictable_cost;
    }

  return COSTS_N_INSNS (BRANCH_COST (true, predictable_p));
}

/* Implement TARGET_NOCE_CONVERSION_PROFITABLE_P.  We replace the cost of a
   conditional branch assumed by `noce_find_if_block' at `COSTS_N_INSNS (2)'
   by our actual conditional branch cost, observing that our branches test
   conditions directly, so there is no preparatory extra condition-set
   instruction.  */

static bool
riscv_noce_conversion_profitable_p (rtx_insn *seq,
				    struct noce_if_info *if_info)
{
  struct noce_if_info riscv_if_info = *if_info;

  riscv_if_info.original_cost -= COSTS_N_INSNS (2);
  riscv_if_info.original_cost += insn_cost (if_info->jump, if_info->speed_p);

  /* Hack alert!  When `noce_try_store_flag_mask' uses `cstore<mode>4'
     to emit a conditional set operation on DImode output it comes up
     with a sequence such as:

     (insn 26 0 27 (set (reg:SI 140)
	     (eq:SI (reg/v:DI 137 [ c ])
		 (const_int 0 [0]))) 302 {*seq_zero_disi}
	  (nil))
     (insn 27 26 28 (set (reg:DI 139)
	     (zero_extend:DI (reg:SI 140))) 116 {*zero_extendsidi2_internal}
	  (nil))

     because our `cstore<mode>4' pattern expands to an insn that gives
     a SImode output.  The output of conditional set is 0 or 1 boolean,
     so it is valid for input in any scalar integer mode and therefore
     combine later folds the zero extend operation into an equivalent
     conditional set operation that produces a DImode output, however
     this redundant zero extend operation counts towards the cost of
     the replacement sequence.  Compensate for that by incrementing the
     cost of the original sequence as well as the maximum sequence cost
     accordingly.  Likewise for sign extension.  */
  rtx last_dest = NULL_RTX;
  for (rtx_insn *insn = seq; insn; insn = NEXT_INSN (insn))
    {
      if (!NONDEBUG_INSN_P (insn))
	continue;

      rtx x = PATTERN (insn);
      if (NONJUMP_INSN_P (insn)
	  && GET_CODE (x) == SET)
	{
	  rtx src = SET_SRC (x);
	  enum rtx_code code = GET_CODE (src);
	  if (last_dest != NULL_RTX
	      && (code == SIGN_EXTEND || code == ZERO_EXTEND)
	      && REG_P (XEXP (src, 0))
	      && REGNO (XEXP (src, 0)) == REGNO (last_dest))
	    {
	      riscv_if_info.original_cost += COSTS_N_INSNS (1);
	      riscv_if_info.max_seq_cost += COSTS_N_INSNS (1);
	    }
	  last_dest = NULL_RTX;
	  rtx dest = SET_DEST (x);
	  if (COMPARISON_P (src)
	      && REG_P (dest)
	      && GET_MODE (dest) == SImode)
	    last_dest = dest;
	}
      else
	last_dest = NULL_RTX;
    }

  return default_noce_conversion_profitable_p (seq, &riscv_if_info);
}

/* Return one word of double-word value OP.  HIGH_P is true to select the
   high part or false to select the low part. */

rtx
riscv_subword (rtx op, bool high_p)
{
  unsigned int byte = (high_p != BYTES_BIG_ENDIAN) ? UNITS_PER_WORD : 0;
  machine_mode mode = GET_MODE (op);

  if (mode == VOIDmode)
    mode = TARGET_64BIT ? TImode : DImode;

  if (MEM_P (op))
    return adjust_address (op, word_mode, byte);

  if (REG_P (op))
    gcc_assert (!FP_REG_RTX_P (op));

  return simplify_gen_subreg (word_mode, op, mode, byte);
}

/* Return true if a 64-bit move from SRC to DEST should be split into two.  */

bool
riscv_split_64bit_move_p (rtx dest, rtx src)
{
  if (TARGET_64BIT)
    return false;

  /* There is no need to split if the FLI instruction in the `Zfa` extension can be used.  */
  if (satisfies_constraint_zfli (src))
    return false;

  /* Allow FPR <-> FPR and FPR <-> MEM moves, and permit the special case
     of zeroing an FPR with FCVT.D.W.  */
  if (TARGET_DOUBLE_FLOAT
      && ((FP_REG_RTX_P (src) && FP_REG_RTX_P (dest))
	  || (FP_REG_RTX_P (dest) && MEM_P (src))
	  || (FP_REG_RTX_P (src) && MEM_P (dest))
	  || (FP_REG_RTX_P (dest) && src == CONST0_RTX (GET_MODE (src)))))
    return false;

  return true;
}

/* Split a doubleword move from SRC to DEST.  On 32-bit targets,
   this function handles 64-bit moves for which riscv_split_64bit_move_p
   holds.  For 64-bit targets, this function handles 128-bit moves.  */

void
riscv_split_doubleword_move (rtx dest, rtx src)
{
  /* ZFA or XTheadFmv has instructions for accessing the upper bits of a double.  */
  if (!TARGET_64BIT && (TARGET_ZFA || TARGET_XTHEADFMV))
    {
      if (FP_REG_RTX_P (dest))
	{
	  rtx low_src = riscv_subword (src, false);
	  rtx high_src = riscv_subword (src, true);

	  if (TARGET_ZFA)
	    emit_insn (gen_movdfsisi3_rv32 (dest, high_src, low_src));
	  else
	    emit_insn (gen_th_fmv_hw_w_x (dest, high_src, low_src));
	  return;
	}
      if (FP_REG_RTX_P (src))
	{
	  rtx low_dest = riscv_subword (dest, false);
	  rtx high_dest = riscv_subword (dest, true);

	  if (TARGET_ZFA)
	    {
	      emit_insn (gen_movsidf2_low_rv32 (low_dest, src));
	      emit_insn (gen_movsidf2_high_rv32 (high_dest, src));
	      return;
	    }
	  else
	    {
	      emit_insn (gen_th_fmv_x_w (low_dest, src));
	      emit_insn (gen_th_fmv_x_hw (high_dest, src));
	    }
	  return;
	}
    }

   /* The operation can be split into two normal moves.  Decide in
      which order to do them.  */
   rtx low_dest = riscv_subword (dest, false);
   if (REG_P (low_dest) && reg_overlap_mentioned_p (low_dest, src))
     {
       riscv_emit_move (riscv_subword (dest, true), riscv_subword (src, true));
       riscv_emit_move (low_dest, riscv_subword (src, false));
     }
   else
     {
       riscv_emit_move (low_dest, riscv_subword (src, false));
       riscv_emit_move (riscv_subword (dest, true), riscv_subword (src, true));
     }
}

/* Return the appropriate instructions to move SRC into DEST.  Assume
   that SRC is operand 1 and DEST is operand 0.  */

const char *
riscv_output_move (rtx dest, rtx src)
{
  enum rtx_code dest_code, src_code;
  machine_mode mode;
  bool dbl_p;
  unsigned width;
  const char *insn;

  if ((insn = th_output_move (dest, src)))
    return insn;

  dest_code = GET_CODE (dest);
  src_code = GET_CODE (src);
  mode = GET_MODE (dest);
  dbl_p = (GET_MODE_SIZE (mode).to_constant () == 8);
  width = GET_MODE_SIZE (mode).to_constant ();

  if (dbl_p && riscv_split_64bit_move_p (dest, src))
    return "#";

  if (dest_code == REG && GP_REG_P (REGNO (dest)))
    {
      if (src_code == REG && FP_REG_P (REGNO (src)))
	switch (width)
	  {
	  case 2:
	    if (TARGET_ZFHMIN)
	      return "fmv.x.h\t%0,%1";
	    /* Using fmv.x.s + sign-extend to emulate fmv.x.h.  */
	    return "fmv.x.s\t%0,%1;slli\t%0,%0,16;srai\t%0,%0,16";
	  case 4:
	    return "fmv.x.s\t%0,%1";
	  case 8:
	    return "fmv.x.d\t%0,%1";
	  }

      if (src_code == MEM)
	switch (width)
	  {
	  case 1: return "lbu\t%0,%1";
	  case 2: return "lhu\t%0,%1";
	  case 4: return "lw\t%0,%1";
	  case 8: return "ld\t%0,%1";
	  }

      if (src_code == CONST_INT)
	{
	  if (SMALL_OPERAND (INTVAL (src)) || LUI_OPERAND (INTVAL (src)))
	    return "li\t%0,%1";

	  if (TARGET_ZBS
	      && SINGLE_BIT_MASK_OPERAND (INTVAL (src)))
	    return "bseti\t%0,zero,%S1";

	  /* Should never reach here.  */
	  abort ();
	}

      if (src_code == HIGH)
	return "lui\t%0,%h1";

      if (symbolic_operand (src, VOIDmode))
	switch (riscv_classify_symbolic_expression (src))
	  {
	  case SYMBOL_GOT_DISP: return "la\t%0,%1";
	  case SYMBOL_ABSOLUTE: return "lla\t%0,%1";
	  case SYMBOL_PCREL: return "lla\t%0,%1";
	  default: gcc_unreachable ();
	  }
    }
  if ((src_code == REG && GP_REG_P (REGNO (src)))
      || (src == CONST0_RTX (mode)))
    {
      if (dest_code == REG)
	{
	  if (GP_REG_P (REGNO (dest)))
	    return "mv\t%0,%z1";

	  if (FP_REG_P (REGNO (dest)))
	    switch (width)
	      {
	      case 2:
		if (TARGET_ZFHMIN)
		  return "fmv.h.x\t%0,%z1";
		/* High 16 bits should be all-1, otherwise HW will treated
		   as a n-bit canonical NaN, but isn't matter for softfloat.  */
		return "fmv.s.x\t%0,%1";
	      case 4:
		return "fmv.s.x\t%0,%z1";
	      case 8:
		if (TARGET_64BIT)
		  return "fmv.d.x\t%0,%z1";
		/* in RV32, we can emulate fmv.d.x %0, x0 using fcvt.d.w */
		gcc_assert (src == CONST0_RTX (mode));
		return "fcvt.d.w\t%0,x0";
	      }
	}
      if (dest_code == MEM)
	switch (width)
	  {
	  case 1: return "sb\t%z1,%0";
	  case 2: return "sh\t%z1,%0";
	  case 4: return "sw\t%z1,%0";
	  case 8: return "sd\t%z1,%0";
	  }
    }
  if (src_code == REG && FP_REG_P (REGNO (src)))
    {
      if (dest_code == REG && FP_REG_P (REGNO (dest)))
	switch (width)
	  {
	  case 2:
	    if (TARGET_ZFH)
	      return "fmv.h\t%0,%1";
	    return "fmv.s\t%0,%1";
	  case 4:
	    return "fmv.s\t%0,%1";
	  case 8:
	    return "fmv.d\t%0,%1";
	  }

      if (dest_code == MEM)
	switch (width)
	  {
	  case 2:
	    return "fsh\t%1,%0";
	  case 4:
	    return "fsw\t%1,%0";
	  case 8:
	    return "fsd\t%1,%0";
	  }
    }
  if (dest_code == REG && FP_REG_P (REGNO (dest)))
    {
      if (src_code == MEM)
	switch (width)
	  {
	  case 2:
	    return "flh\t%0,%1";
	  case 4:
	    return "flw\t%0,%1";
	  case 8:
	    return "fld\t%0,%1";
	  }

      if (src_code == CONST_DOUBLE && satisfies_constraint_zfli (src))
	switch (width)
	  {
	    case 2:
	      return "fli.h\t%0,%1";
	    case 4:
	      return "fli.s\t%0,%1";
	    case 8:
	      return "fli.d\t%0,%1";
	  }
    }
  if (dest_code == REG && GP_REG_P (REGNO (dest)) && src_code == CONST_POLY_INT)
    {
      /* We only want a single full vector register VLEN read after reload. */
      gcc_assert (known_eq (rtx_to_poly_int64 (src), BYTES_PER_RISCV_VECTOR));
      return "csrr\t%0,vlenb";
    }
  gcc_unreachable ();
}

const char *
riscv_output_return ()
{
  if (cfun->machine->naked_p)
    return "";

  return "ret";
}


/* Return true if CMP1 is a suitable second operand for integer ordering
   test CODE.  See also the *sCC patterns in riscv.md.  */

static bool
riscv_int_order_operand_ok_p (enum rtx_code code, rtx cmp1)
{
  switch (code)
    {
    case GT:
    case GTU:
      return reg_or_0_operand (cmp1, VOIDmode);

    case GE:
    case GEU:
      return cmp1 == const1_rtx;

    case LT:
    case LTU:
      return arith_operand (cmp1, VOIDmode);

    case LE:
      return sle_operand (cmp1, VOIDmode);

    case LEU:
      return sleu_operand (cmp1, VOIDmode);

    default:
      gcc_unreachable ();
    }
}

/* Return true if *CMP1 (of mode MODE) is a valid second operand for
   integer ordering test *CODE, or if an equivalent combination can
   be formed by adjusting *CODE and *CMP1.  When returning true, update
   *CODE and *CMP1 with the chosen code and operand, otherwise leave
   them alone.  */

static bool
riscv_canonicalize_int_order_test (enum rtx_code *code, rtx *cmp1,
				   machine_mode mode)
{
  HOST_WIDE_INT plus_one;

  if (riscv_int_order_operand_ok_p (*code, *cmp1))
    return true;

  if (CONST_INT_P (*cmp1))
    switch (*code)
      {
      case LE:
	plus_one = trunc_int_for_mode (UINTVAL (*cmp1) + 1, mode);
	if (INTVAL (*cmp1) < plus_one)
	  {
	    *code = LT;
	    *cmp1 = force_reg (mode, GEN_INT (plus_one));
	    return true;
	  }
	break;

      case LEU:
	plus_one = trunc_int_for_mode (UINTVAL (*cmp1) + 1, mode);
	if (plus_one != 0)
	  {
	    *code = LTU;
	    *cmp1 = force_reg (mode, GEN_INT (plus_one));
	    return true;
	  }
	break;

      default:
	break;
      }
  return false;
}

/* Compare CMP0 and CMP1 using ordering test CODE and store the result
   in TARGET.  CMP0 and TARGET are register_operands.  If INVERT_PTR
   is nonnull, it's OK to set TARGET to the inverse of the result and
   flip *INVERT_PTR instead.  */

static void
riscv_emit_int_order_test (enum rtx_code code, bool *invert_ptr,
			  rtx target, rtx cmp0, rtx cmp1)
{
  machine_mode mode;

  /* First see if there is a RISCV instruction that can do this operation.
     If not, try doing the same for the inverse operation.  If that also
     fails, force CMP1 into a register and try again.  */
  mode = GET_MODE (cmp0);
  if (riscv_canonicalize_int_order_test (&code, &cmp1, mode))
    riscv_emit_binary (code, target, cmp0, cmp1);
  else
    {
      enum rtx_code inv_code = reverse_condition (code);
      if (!riscv_canonicalize_int_order_test (&inv_code, &cmp1, mode))
	{
	  cmp1 = force_reg (mode, cmp1);
	  riscv_emit_int_order_test (code, invert_ptr, target, cmp0, cmp1);
	}
      else if (invert_ptr == 0)
	{
	  rtx inv_target = riscv_force_binary (word_mode,
					       inv_code, cmp0, cmp1);
	  riscv_emit_binary (EQ, target, inv_target, const0_rtx);
	}
      else
	{
	  *invert_ptr = !*invert_ptr;
	  riscv_emit_binary (inv_code, target, cmp0, cmp1);
	}
    }
}

/* Return a register that is zero iff CMP0 and CMP1 are equal.
   The register will have the same mode as CMP0.  */

static rtx
riscv_zero_if_equal (rtx cmp0, rtx cmp1)
{
  if (cmp1 == const0_rtx)
    return cmp0;

  return expand_binop (GET_MODE (cmp0), sub_optab,
		       cmp0, cmp1, 0, 0, OPTAB_DIRECT);
}

/* Helper function for riscv_extend_comparands to Sign-extend the OP.
   However if the OP is SI subreg promoted with an inner DI, such as
       (subreg/s/v:SI (reg/v:DI) 0)
   just peel off the SUBREG to get DI, avoiding extraneous extension.  */

static void
riscv_sign_extend_if_not_subreg_prom (rtx *op)
{
  if (GET_CODE (*op) == SUBREG
      && SUBREG_PROMOTED_VAR_P (*op)
      && SUBREG_PROMOTED_SIGNED_P (*op)
      && (GET_MODE_SIZE (GET_MODE (XEXP (*op, 0))).to_constant ()
	  == GET_MODE_SIZE (word_mode)))
    *op = XEXP (*op, 0);
  else
    *op = gen_rtx_SIGN_EXTEND (word_mode, *op);
}

/* Sign- or zero-extend OP0 and OP1 for integer comparisons.  */

static void
riscv_extend_comparands (rtx_code code, rtx *op0, rtx *op1)
{
  /* Comparisons consider all XLEN bits, so extend sub-XLEN values.  */
  if (GET_MODE_SIZE (word_mode) > GET_MODE_SIZE (GET_MODE (*op0)).to_constant ())
    {
      /* It is more profitable to zero-extend QImode values.  But not if the
	 first operand has already been sign-extended, and the second one is
	 is a constant or has already been sign-extended also.  */
      if (unsigned_condition (code) == code
	  && (GET_MODE (*op0) == QImode
	      && ! (GET_CODE (*op0) == SUBREG
		    && SUBREG_PROMOTED_VAR_P (*op0)
		    && SUBREG_PROMOTED_SIGNED_P (*op0)
		    && (CONST_INT_P (*op1)
			|| (GET_CODE (*op1) == SUBREG
			    && SUBREG_PROMOTED_VAR_P (*op1)
			    && SUBREG_PROMOTED_SIGNED_P (*op1))))))
	{
	  *op0 = gen_rtx_ZERO_EXTEND (word_mode, *op0);
	  if (CONST_INT_P (*op1))
	    *op1 = GEN_INT ((uint8_t) INTVAL (*op1));
	  else
	    *op1 = gen_rtx_ZERO_EXTEND (word_mode, *op1);
	}
      else
	{
	  riscv_sign_extend_if_not_subreg_prom (op0);

	  if (*op1 != const0_rtx)
	    riscv_sign_extend_if_not_subreg_prom (op1);
	}
    }
}

/* Convert a comparison into something that can be used in a branch or
   conditional move.  On entry, *OP0 and *OP1 are the values being
   compared and *CODE is the code used to compare them.

   Update *CODE, *OP0 and *OP1 so that they describe the final comparison.
   If NEED_EQ_NE_P, then only EQ or NE comparisons against zero are
   emitted.  */

static void
riscv_emit_int_compare (enum rtx_code *code, rtx *op0, rtx *op1,
			bool need_eq_ne_p = false)
{
  if (need_eq_ne_p)
    {
      rtx cmp_op0 = *op0;
      rtx cmp_op1 = *op1;
      if (*code == EQ || *code == NE)
	{
	  *op0 = riscv_zero_if_equal (cmp_op0, cmp_op1);
	  *op1 = const0_rtx;
	  return;
	}
      gcc_unreachable ();
    }

  if (splittable_const_int_operand (*op1, VOIDmode))
    {
      HOST_WIDE_INT rhs = INTVAL (*op1);

      if (*code == EQ || *code == NE)
	{
	  /* Convert e.g. OP0 == 2048 into OP0 - 2048 == 0.  */
	  if (SMALL_OPERAND (-rhs))
	    {
	      *op0 = riscv_force_binary (GET_MODE (*op0), PLUS, *op0,
					 GEN_INT (-rhs));
	      *op1 = const0_rtx;
	    }
	}
      else
	{
	  static const enum rtx_code mag_comparisons[][2] = {
	    {LEU, LTU}, {GTU, GEU}, {LE, LT}, {GT, GE}
	  };

	  /* Convert e.g. (OP0 <= 0xFFF) into (OP0 < 0x1000).  */
	  for (size_t i = 0; i < ARRAY_SIZE (mag_comparisons); i++)
	    {
	      HOST_WIDE_INT new_rhs;
	      bool increment = *code == mag_comparisons[i][0];
	      bool decrement = *code == mag_comparisons[i][1];
	      if (!increment && !decrement)
		continue;

	      new_rhs = rhs + (increment ? 1 : -1);
	      new_rhs = trunc_int_for_mode (new_rhs, GET_MODE (*op0));
	      if (riscv_integer_cost (new_rhs) < riscv_integer_cost (rhs)
		  && (rhs < 0) == (new_rhs < 0))
		{
		  *op1 = GEN_INT (new_rhs);
		  *code = mag_comparisons[i][increment];
		}
	      break;
	    }
	}
    }

  riscv_extend_comparands (*code, op0, op1);

  *op0 = force_reg (word_mode, *op0);
  if (*op1 != const0_rtx)
    *op1 = force_reg (word_mode, *op1);
}

/* Like riscv_emit_int_compare, but for floating-point comparisons.  */

static void
riscv_emit_float_compare (enum rtx_code *code, rtx *op0, rtx *op1,
			  bool *invert_ptr = nullptr)
{
  rtx tmp0, tmp1, cmp_op0 = *op0, cmp_op1 = *op1;
  enum rtx_code fp_code = *code;
  *code = NE;

  switch (fp_code)
    {
    case UNORDERED:
      *code = EQ;
      /* Fall through.  */

    case ORDERED:
      /* a == a && b == b */
      tmp0 = riscv_force_binary (word_mode, EQ, cmp_op0, cmp_op0);
      tmp1 = riscv_force_binary (word_mode, EQ, cmp_op1, cmp_op1);
      *op0 = riscv_force_binary (word_mode, AND, tmp0, tmp1);
      *op1 = const0_rtx;
      break;

    case UNEQ:
      /* ordered(a, b) > (a == b) */
      *code = EQ;
      tmp0 = riscv_force_binary (word_mode, EQ, cmp_op0, cmp_op0);
      tmp1 = riscv_force_binary (word_mode, EQ, cmp_op1, cmp_op1);
      *op0 = riscv_force_binary (word_mode, AND, tmp0, tmp1);
      *op1 = riscv_force_binary (word_mode, EQ, cmp_op0, cmp_op1);
      break;

#define UNORDERED_COMPARISON(CODE, CMP)					\
    case CODE:								\
      *code = EQ;							\
      *op0 = gen_reg_rtx (word_mode);					\
      if (GET_MODE (cmp_op0) == SFmode && TARGET_64BIT)			\
	emit_insn (gen_f##CMP##_quietsfdi4 (*op0, cmp_op0, cmp_op1));	\
      else if (GET_MODE (cmp_op0) == SFmode)				\
	emit_insn (gen_f##CMP##_quietsfsi4 (*op0, cmp_op0, cmp_op1));	\
      else if (GET_MODE (cmp_op0) == DFmode && TARGET_64BIT)		\
	emit_insn (gen_f##CMP##_quietdfdi4 (*op0, cmp_op0, cmp_op1));	\
      else if (GET_MODE (cmp_op0) == DFmode)				\
	emit_insn (gen_f##CMP##_quietdfsi4 (*op0, cmp_op0, cmp_op1));	\
      else if (GET_MODE (cmp_op0) == HFmode && TARGET_64BIT)		\
	emit_insn (gen_f##CMP##_quiethfdi4 (*op0, cmp_op0, cmp_op1));	\
      else if (GET_MODE (cmp_op0) == HFmode)				\
	emit_insn (gen_f##CMP##_quiethfsi4 (*op0, cmp_op0, cmp_op1));	\
      else								\
	gcc_unreachable ();						\
      *op1 = const0_rtx;						\
      break;

    case UNLT:
      std::swap (cmp_op0, cmp_op1);
      gcc_fallthrough ();

    UNORDERED_COMPARISON(UNGT, le)

    case UNLE:
      std::swap (cmp_op0, cmp_op1);
      gcc_fallthrough ();

    UNORDERED_COMPARISON(UNGE, lt)
#undef UNORDERED_COMPARISON

    case NE:
      fp_code = EQ;
      if (invert_ptr != nullptr)
	*invert_ptr = !*invert_ptr;
      else
	{
	  cmp_op0 = riscv_force_binary (word_mode, fp_code, cmp_op0, cmp_op1);
	  cmp_op1 = const0_rtx;
	}
      gcc_fallthrough ();

    case EQ:
    case LE:
    case LT:
    case GE:
    case GT:
      /* We have instructions for these cases.  */
      *code = fp_code;
      *op0 = cmp_op0;
      *op1 = cmp_op1;
      break;

    case LTGT:
      /* (a < b) | (a > b) */
      tmp0 = riscv_force_binary (word_mode, LT, cmp_op0, cmp_op1);
      tmp1 = riscv_force_binary (word_mode, GT, cmp_op0, cmp_op1);
      *op0 = riscv_force_binary (word_mode, IOR, tmp0, tmp1);
      *op1 = const0_rtx;
      break;

    default:
      gcc_unreachable ();
    }
}

/* CODE-compare OP0 and OP1.  Store the result in TARGET.  */

void
riscv_expand_int_scc (rtx target, enum rtx_code code, rtx op0, rtx op1, bool *invert_ptr)
{
  riscv_extend_comparands (code, &op0, &op1);
  op0 = force_reg (word_mode, op0);

  if (code == EQ || code == NE)
    {
      rtx zie = riscv_zero_if_equal (op0, op1);
      riscv_emit_binary (code, target, zie, const0_rtx);
    }
  else
    riscv_emit_int_order_test (code, invert_ptr, target, op0, op1);
}

/* Like riscv_expand_int_scc, but for floating-point comparisons.  */

void
riscv_expand_float_scc (rtx target, enum rtx_code code, rtx op0, rtx op1,
			bool *invert_ptr)
{
  riscv_emit_float_compare (&code, &op0, &op1, invert_ptr);

  machine_mode mode = GET_MODE (target);
  if (mode != word_mode)
    {
      rtx cmp = riscv_force_binary (word_mode, code, op0, op1);
      riscv_emit_set (target, lowpart_subreg (mode, cmp, word_mode));
    }
  else
    riscv_emit_binary (code, target, op0, op1);
}

/* Jump to LABEL if (CODE OP0 OP1) holds.  */

void
riscv_expand_conditional_branch (rtx label, rtx_code code, rtx op0, rtx op1)
{
  if (FLOAT_MODE_P (GET_MODE (op1)))
    riscv_emit_float_compare (&code, &op0, &op1);
  else
    riscv_emit_int_compare (&code, &op0, &op1);

  if (FLOAT_MODE_P (GET_MODE (op0)))
    {
      op0 = riscv_force_binary (word_mode, code, op0, op1);
      op1 = const0_rtx;
      code = NE;
    }

  rtx condition = gen_rtx_fmt_ee (code, VOIDmode, op0, op1);
  emit_jump_insn (gen_condjump (condition, label));
}

/* Emit a cond move: If OP holds, move CONS to DEST; else move ALT to DEST.
   Return 0 if expansion failed.  */

bool
riscv_expand_conditional_move (rtx dest, rtx op, rtx cons, rtx alt)
{
  machine_mode mode = GET_MODE (dest);
  rtx_code code = GET_CODE (op);
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);

  if (((TARGET_ZICOND_LIKE
	|| (arith_operand (cons, mode) && arith_operand (alt, mode)))
       && (GET_MODE_CLASS (mode) == MODE_INT))
      || TARGET_SFB_ALU || TARGET_XTHEADCONDMOV)
    {
      machine_mode mode0 = GET_MODE (op0);
      machine_mode mode1 = GET_MODE (op1);

      /* An integer comparison must be comparing WORD_MODE objects.  We
	 must enforce that so that we don't strip away a sign_extension
	 thinking it is unnecessary.  We might consider using
	 riscv_extend_operands if they are not already properly extended.  */
      if ((INTEGRAL_MODE_P (mode0) && mode0 != word_mode)
	  || (INTEGRAL_MODE_P (mode1) && mode1 != word_mode))
	return false;

      /* In the fallback generic case use MODE rather than WORD_MODE for
	 the output of the SCC instruction, to match the mode of the NEG
	 operation below.  The output of SCC is 0 or 1 boolean, so it is
	 valid for input in any scalar integer mode.  */
      rtx tmp = gen_reg_rtx ((TARGET_ZICOND_LIKE
			      || TARGET_SFB_ALU || TARGET_XTHEADCONDMOV)
			     ? word_mode : mode);
      bool invert = false;

      /* Canonicalize the comparison.  It must be an equality comparison
	 of integer operands, or with SFB it can be any comparison of
	 integer operands.  If it isn't, then emit an SCC instruction
	 so that we can then use an equality comparison against zero.  */
      if ((!TARGET_SFB_ALU && !equality_operator (op, VOIDmode))
	  || !INTEGRAL_MODE_P (mode0))
	{
	  bool *invert_ptr = nullptr;

	  /* If riscv_expand_int_scc inverts the condition, then it will
	     flip the value of INVERT.  We need to know where so that
	     we can adjust it for our needs.  */
	  if (code == LE || code == LEU || code == GE || code == GEU)
	    invert_ptr = &invert;

	  /* Emit an SCC-like instruction into a temporary so that we can
	     use an EQ/NE comparison.  We can support both FP and integer
	     conditional moves.  */
	  if (INTEGRAL_MODE_P (mode0))
	    riscv_expand_int_scc (tmp, code, op0, op1, invert_ptr);
	  else if (FLOAT_MODE_P (mode0)
		   && fp_scc_comparison (op, GET_MODE (op)))
	    riscv_expand_float_scc (tmp, code, op0, op1, &invert);
	  else
	    return false;

	  op = gen_rtx_fmt_ee (invert ? EQ : NE, mode, tmp, const0_rtx);

	  /* We've generated a new comparison.  Update the local variables.  */
	  code = GET_CODE (op);
	  op0 = XEXP (op, 0);
	  op1 = XEXP (op, 1);
	}
      else if (!TARGET_ZICOND_LIKE && !TARGET_SFB_ALU && !TARGET_XTHEADCONDMOV)
	riscv_expand_int_scc (tmp, code, op0, op1, &invert);

      if (TARGET_SFB_ALU || TARGET_XTHEADCONDMOV)
	{
	  riscv_emit_int_compare (&code, &op0, &op1, !TARGET_SFB_ALU);
	  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);

	  /* The expander is a bit loose in its specification of the true
	     arm of the conditional move.  That allows us to support more
	     cases for extensions which are more general than SFB.  But
	     does mean we need to force CONS into a register at this point.  */
	  cons = force_reg (mode, cons);
	  /* With XTheadCondMov we need to force ALT into a register too.  */
	  alt = force_reg (mode, alt);
	  emit_insn (gen_rtx_SET (dest, gen_rtx_IF_THEN_ELSE (mode, cond,
							      cons, alt)));
	  return true;
	}
      else if (!TARGET_ZICOND_LIKE)
	{
	  if (invert)
	    std::swap (cons, alt);

	  rtx reg1 = gen_reg_rtx (mode);
	  rtx reg2 = gen_reg_rtx (mode);
	  rtx reg3 = gen_reg_rtx (mode);
	  rtx reg4 = gen_reg_rtx (mode);

	  riscv_emit_unary (NEG, reg1, tmp);
	  riscv_emit_binary (AND, reg2, reg1, cons);
	  riscv_emit_unary (NOT, reg3, reg1);
	  riscv_emit_binary (AND, reg4, reg3, alt);
	  riscv_emit_binary (IOR, dest, reg2, reg4);
	  return true;
	}
      /* 0, reg or 0, imm */
      else if (cons == CONST0_RTX (mode)
	       && (REG_P (alt)
		   || (CONST_INT_P (alt) && alt != CONST0_RTX (mode))))
	{
	  riscv_emit_int_compare (&code, &op0, &op1, true);
	  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	  alt = force_reg (mode, alt);
	  emit_insn (gen_rtx_SET (dest,
				  gen_rtx_IF_THEN_ELSE (mode, cond,
							cons, alt)));
	  return true;
	}
      /* imm, imm */
      else if (CONST_INT_P (cons) && cons != CONST0_RTX (mode)
	       && CONST_INT_P (alt) && alt != CONST0_RTX (mode))
	{
	  riscv_emit_int_compare (&code, &op0, &op1, true);
	  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	  HOST_WIDE_INT t = INTVAL (alt) - INTVAL (cons);
	  alt = force_reg (mode, gen_int_mode (t, mode));
	  emit_insn (gen_rtx_SET (dest,
				  gen_rtx_IF_THEN_ELSE (mode, cond,
							CONST0_RTX (mode),
							alt)));
	  /* CONS might not fit into a signed 12 bit immediate suitable
	     for an addi instruction.  If that's the case, force it
	     into a register.  */
	  if (!SMALL_OPERAND (INTVAL (cons)))
	    cons = force_reg (mode, cons);
	  riscv_emit_binary (PLUS, dest, dest, cons);
	  return true;
	}
      /* imm, reg  */
      else if (CONST_INT_P (cons) && cons != CONST0_RTX (mode) && REG_P (alt))
	{
	  /* Optimize for register value of 0.  */
	  if (code == NE && rtx_equal_p (op0, alt) && op1 == CONST0_RTX (mode))
	    {
	      rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	      cons = force_reg (mode, cons);
	      emit_insn (gen_rtx_SET (dest,
				      gen_rtx_IF_THEN_ELSE (mode, cond,
							    cons, alt)));
	      return true;
	    }

	  riscv_emit_int_compare (&code, &op0, &op1, true);
	  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);

	  rtx temp1 = gen_reg_rtx (mode);
	  rtx temp2 = gen_int_mode (-1 * INTVAL (cons), mode);

	  /* TEMP2 and/or CONS might not fit into a signed 12 bit immediate
	     suitable for an addi instruction.  If that's the case, force it
	     into a register.  */
	  if (!SMALL_OPERAND (INTVAL (temp2)))
	    temp2 = force_reg (mode, temp2);
	  if (!SMALL_OPERAND (INTVAL (cons)))
	    cons = force_reg (mode, cons);

	  riscv_emit_binary (PLUS, temp1, alt, temp2);
	  emit_insn (gen_rtx_SET (dest,
				  gen_rtx_IF_THEN_ELSE (mode, cond,
							CONST0_RTX (mode),
							temp1)));
	  riscv_emit_binary (PLUS, dest, dest, cons);
	  return true;
	}
      /* reg, 0 or imm, 0  */
      else if ((REG_P (cons)
		|| (CONST_INT_P (cons) && cons != CONST0_RTX (mode)))
	       && alt == CONST0_RTX (mode))
	{
	  riscv_emit_int_compare (&code, &op0, &op1, true);
	  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	  cons = force_reg (mode, cons);
	  emit_insn (gen_rtx_SET (dest, gen_rtx_IF_THEN_ELSE (mode, cond,
							      cons, alt)));
	  return true;
	}
      /* reg, imm  */
      else if (REG_P (cons) && CONST_INT_P (alt) && alt != CONST0_RTX (mode))
	{
	  /* Optimize for register value of 0.  */
	  if (code == EQ && rtx_equal_p (op0, cons) && op1 == CONST0_RTX (mode))
	    {
	      rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	      alt = force_reg (mode, alt);
	      emit_insn (gen_rtx_SET (dest,
				      gen_rtx_IF_THEN_ELSE (mode, cond,
							    cons, alt)));
	      return true;
	    }

	  riscv_emit_int_compare (&code, &op0, &op1, true);
	  rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);

	  rtx temp1 = gen_reg_rtx (mode);
	  rtx temp2 = gen_int_mode (-1 * INTVAL (alt), mode);

	  /* TEMP2 and/or ALT might not fit into a signed 12 bit immediate
	     suitable for an addi instruction.  If that's the case, force it
	     into a register.  */
	  if (!SMALL_OPERAND (INTVAL (temp2)))
	    temp2 = force_reg (mode, temp2);
	  if (!SMALL_OPERAND (INTVAL (alt)))
	    alt = force_reg (mode, alt);

	  riscv_emit_binary (PLUS, temp1, cons, temp2);
	  emit_insn (gen_rtx_SET (dest,
				  gen_rtx_IF_THEN_ELSE (mode, cond,
							temp1,
							CONST0_RTX (mode))));
	  riscv_emit_binary (PLUS, dest, dest, alt);
	  return true;
	}
      /* reg, reg  */
      else if (REG_P (cons) && REG_P (alt))
	{
	  if (((code == EQ && rtx_equal_p (cons, op0))
	       || (code == NE && rtx_equal_p (alt, op0)))
	      && op1 == CONST0_RTX (mode))
	    {
	      rtx cond = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	      alt = force_reg (mode, alt);
	      emit_insn (gen_rtx_SET (dest,
				      gen_rtx_IF_THEN_ELSE (mode, cond,
							    cons, alt)));
	      return true;
	    }

	  rtx reg1 = gen_reg_rtx (mode);
	  rtx reg2 = gen_reg_rtx (mode);
	  riscv_emit_int_compare (&code, &op0, &op1, true);
	  rtx cond1 = gen_rtx_fmt_ee (code, GET_MODE (op0), op0, op1);
	  rtx cond2 = gen_rtx_fmt_ee (code == NE ? EQ : NE,
				      GET_MODE (op0), op0, op1);
	  emit_insn (gen_rtx_SET (reg2,
				  gen_rtx_IF_THEN_ELSE (mode, cond2,
							CONST0_RTX (mode),
							cons)));
	  emit_insn (gen_rtx_SET (reg1,
				  gen_rtx_IF_THEN_ELSE (mode, cond1,
							CONST0_RTX (mode),
							alt)));
	  riscv_emit_binary (PLUS, dest, reg1, reg2);
	  return true;
	}
    }

  return false;
}

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  Every parameter gets at
   least PARM_BOUNDARY bits of alignment, but will be given anything up
   to PREFERRED_STACK_BOUNDARY bits if the type requires it.  */

static unsigned int
riscv_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int alignment;

  /* Use natural alignment if the type is not aggregate data.  */
  if (type && !AGGREGATE_TYPE_P (type))
    alignment = TYPE_ALIGN (TYPE_MAIN_VARIANT (type));
  else
    alignment = type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode);

  return MIN (PREFERRED_STACK_BOUNDARY, MAX (PARM_BOUNDARY, alignment));
}

/* If MODE represents an argument that can be passed or returned in
   floating-point registers, return the number of registers, else 0.  */

static unsigned
riscv_pass_mode_in_fpr_p (machine_mode mode)
{
  if (GET_MODE_UNIT_SIZE (mode) <= UNITS_PER_FP_ARG)
    {
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return 1;

      if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	return 2;
    }

  return 0;
}

typedef struct {
  const_tree type;
  HOST_WIDE_INT offset;
} riscv_aggregate_field;

/* Identify subfields of aggregates that are candidates for passing in
   floating-point registers.  */

static int
riscv_flatten_aggregate_field (const_tree type,
			       riscv_aggregate_field fields[2],
			       int n, HOST_WIDE_INT offset,
			       bool ignore_zero_width_bit_field_p)
{
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
     /* Can't handle incomplete types nor sizes that are not fixed.  */
     if (!COMPLETE_TYPE_P (type)
	 || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	 || !tree_fits_uhwi_p (TYPE_SIZE (type)))
       return -1;

      for (tree f = TYPE_FIELDS (type); f; f = DECL_CHAIN (f))
	if (TREE_CODE (f) == FIELD_DECL)
	  {
	    if (!TYPE_P (TREE_TYPE (f)))
	      return -1;

	    /* The C++ front end strips zero-length bit-fields from structs.
	       So we need to ignore them in the C front end to make C code
	       compatible with C++ code.  */
	    if (ignore_zero_width_bit_field_p
		&& DECL_BIT_FIELD (f)
		&& (DECL_SIZE (f) == NULL_TREE
		    || integer_zerop (DECL_SIZE (f))))
	      ;
	    else
	      {
		HOST_WIDE_INT pos = offset + int_byte_position (f);
		n = riscv_flatten_aggregate_field (TREE_TYPE (f),
						   fields, n, pos,
						   ignore_zero_width_bit_field_p);
	      }
	    if (n < 0)
	      return -1;
	  }
      return n;

    case ARRAY_TYPE:
      {
	HOST_WIDE_INT n_elts;
	riscv_aggregate_field subfields[2];
	tree index = TYPE_DOMAIN (type);
	tree elt_size = TYPE_SIZE_UNIT (TREE_TYPE (type));
	int n_subfields = riscv_flatten_aggregate_field (TREE_TYPE (type),
							 subfields, 0, offset,
							 ignore_zero_width_bit_field_p);

	/* Can't handle incomplete types nor sizes that are not fixed.  */
	if (n_subfields <= 0
	    || !COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || !tree_fits_uhwi_p (elt_size))
	  return -1;

	n_elts = 1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		   - tree_to_uhwi (TYPE_MIN_VALUE (index));
	gcc_assert (n_elts >= 0);

	for (HOST_WIDE_INT i = 0; i < n_elts; i++)
	  for (int j = 0; j < n_subfields; j++)
	    {
	      if (n >= 2)
		return -1;

	      fields[n] = subfields[j];
	      fields[n++].offset += i * tree_to_uhwi (elt_size);
	    }

	return n;
      }

    case COMPLEX_TYPE:
      {
	/* Complex type need consume 2 field, so n must be 0.  */
	if (n != 0)
	  return -1;

	HOST_WIDE_INT elt_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (type))).to_constant ();

	if (elt_size <= UNITS_PER_FP_ARG)
	  {
	    fields[0].type = TREE_TYPE (type);
	    fields[0].offset = offset;
	    fields[1].type = TREE_TYPE (type);
	    fields[1].offset = offset + elt_size;

	    return 2;
	  }

	return -1;
      }

    default:
      if (n < 2
	  && ((SCALAR_FLOAT_TYPE_P (type)
	       && GET_MODE_SIZE (TYPE_MODE (type)).to_constant () <= UNITS_PER_FP_ARG)
	      || (INTEGRAL_TYPE_P (type)
		  && GET_MODE_SIZE (TYPE_MODE (type)).to_constant () <= UNITS_PER_WORD)))
	{
	  fields[n].type = type;
	  fields[n].offset = offset;
	  return n + 1;
	}
      else
	return -1;
    }
}

/* Identify candidate aggregates for passing in floating-point registers.
   Candidates have at most two fields after flattening.  */

static int
riscv_flatten_aggregate_argument (const_tree type,
				  riscv_aggregate_field fields[2],
				  bool ignore_zero_width_bit_field_p)
{
  if (!type || TREE_CODE (type) != RECORD_TYPE)
    return -1;

  return riscv_flatten_aggregate_field (type, fields, 0, 0,
					ignore_zero_width_bit_field_p);
}

/* See whether TYPE is a record whose fields should be returned in one or
   two floating-point registers.  If so, populate FIELDS accordingly.  */

static unsigned
riscv_pass_aggregate_in_fpr_pair_p (const_tree type,
				    riscv_aggregate_field fields[2])
{
  static int warned = 0;

  /* This is the old ABI, which differs for C++ and C.  */
  int n_old = riscv_flatten_aggregate_argument (type, fields, false);
  for (int i = 0; i < n_old; i++)
    if (!SCALAR_FLOAT_TYPE_P (fields[i].type))
      {
	n_old = -1;
	break;
      }

  /* This is the new ABI, which is the same for C++ and C.  */
  int n_new = riscv_flatten_aggregate_argument (type, fields, true);
  for (int i = 0; i < n_new; i++)
    if (!SCALAR_FLOAT_TYPE_P (fields[i].type))
      {
	n_new = -1;
	break;
      }

  if ((n_old != n_new) && (warned == 0))
    {
      warning (OPT_Wpsabi, "ABI for flattened struct with zero-length "
			   "bit-fields changed in GCC 10");
      warned = 1;
    }

  return n_new > 0 ? n_new : 0;
}

/* See whether TYPE is a record whose fields should be returned in one or
   floating-point register and one integer register.  If so, populate
   FIELDS accordingly.  */

static bool
riscv_pass_aggregate_in_fpr_and_gpr_p (const_tree type,
				       riscv_aggregate_field fields[2])
{
  static int warned = 0;

  /* This is the old ABI, which differs for C++ and C.  */
  unsigned num_int_old = 0, num_float_old = 0;
  int n_old = riscv_flatten_aggregate_argument (type, fields, false);
  for (int i = 0; i < n_old; i++)
    {
      num_float_old += SCALAR_FLOAT_TYPE_P (fields[i].type);
      num_int_old += INTEGRAL_TYPE_P (fields[i].type);
    }

  /* This is the new ABI, which is the same for C++ and C.  */
  unsigned num_int_new = 0, num_float_new = 0;
  int n_new = riscv_flatten_aggregate_argument (type, fields, true);
  for (int i = 0; i < n_new; i++)
    {
      num_float_new += SCALAR_FLOAT_TYPE_P (fields[i].type);
      num_int_new += INTEGRAL_TYPE_P (fields[i].type);
    }

  if (((num_int_old == 1 && num_float_old == 1
	&& (num_int_old != num_int_new || num_float_old != num_float_new))
       || (num_int_new == 1 && num_float_new == 1
	   && (num_int_old != num_int_new || num_float_old != num_float_new)))
      && (warned == 0))
    {
      warning (OPT_Wpsabi, "ABI for flattened struct with zero-length "
			   "bit-fields changed in GCC 10");
      warned = 1;
    }

  return num_int_new == 1 && num_float_new == 1;
}

/* Return the representation of an argument passed or returned in an FPR
   when the value has mode VALUE_MODE and the type has TYPE_MODE.  The
   two modes may be different for structures like:

       struct __attribute__((packed)) foo { float f; }

  where the SFmode value "f" is passed in REGNO but the struct itself
  has mode BLKmode.  */

static rtx
riscv_pass_fpr_single (machine_mode type_mode, unsigned regno,
		       machine_mode value_mode,
		       HOST_WIDE_INT offset)
{
  rtx x = gen_rtx_REG (value_mode, regno);

  if (type_mode != value_mode)
    {
      x = gen_rtx_EXPR_LIST (VOIDmode, x, GEN_INT (offset));
      x = gen_rtx_PARALLEL (type_mode, gen_rtvec (1, x));
    }
  return x;
}

/* Pass or return a composite value in the FPR pair REGNO and REGNO + 1.
   MODE is the mode of the composite.  MODE1 and OFFSET1 are the mode and
   byte offset for the first value, likewise MODE2 and OFFSET2 for the
   second value.  */

static rtx
riscv_pass_fpr_pair (machine_mode mode, unsigned regno1,
		     machine_mode mode1, HOST_WIDE_INT offset1,
		     unsigned regno2, machine_mode mode2,
		     HOST_WIDE_INT offset2)
{
  return gen_rtx_PARALLEL
    (mode,
     gen_rtvec (2,
		gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode1, regno1),
				   GEN_INT (offset1)),
		gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode2, regno2),
				   GEN_INT (offset2))));
}

static rtx
riscv_pass_vls_aggregate_in_gpr (struct riscv_arg_info *info, machine_mode mode,
				 unsigned gpr_base)
{
  gcc_assert (riscv_v_ext_vls_mode_p (mode));

  unsigned count = 0;
  unsigned regnum = 0;
  machine_mode gpr_mode = VOIDmode;
  unsigned vls_size = GET_MODE_SIZE (mode).to_constant ();
  unsigned gpr_size =  GET_MODE_SIZE (Xmode);

  if (IN_RANGE (vls_size, 0, gpr_size * 2))
    {
      count = riscv_v_vls_mode_aggregate_gpr_count (vls_size, gpr_size);

      if (count + info->gpr_offset <= MAX_ARGS_IN_REGISTERS)
	{
	  regnum = gpr_base + info->gpr_offset;
	  info->num_gprs = count;
	  gpr_mode = riscv_v_vls_to_gpr_mode (vls_size);
	}
    }

  if (!regnum)
    return NULL_RTX; /* Return NULL_RTX if we cannot find a suitable reg.  */

  gcc_assert (gpr_mode != VOIDmode);

  rtx reg = gen_rtx_REG (gpr_mode, regnum);
  rtx x = gen_rtx_EXPR_LIST (VOIDmode, reg, CONST0_RTX (gpr_mode));

  return gen_rtx_PARALLEL (mode, gen_rtvec (1, x));
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
riscv_init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype, rtx, tree, int)
{
  memset (cum, 0, sizeof (*cum));

  if (fntype)
    cum->variant_cc = (riscv_cc) fntype_abi (fntype).id ();
  else
    cum->variant_cc = RISCV_CC_BASE;
}

/* Return true if TYPE is a vector type that can be passed in vector registers.
 */

static bool
riscv_vector_type_p (const_tree type)
{
  /* Currently, only builtin scalabler vector type is allowed, in the future,
     more vector types may be allowed, such as GNU vector type, etc.  */
  return riscv_vector::builtin_type_p (type);
}

static unsigned int
riscv_hard_regno_nregs (unsigned int regno, machine_mode mode);

/* Subroutine of riscv_get_arg_info.  */

static rtx
riscv_get_vector_arg (struct riscv_arg_info *info, const CUMULATIVE_ARGS *cum,
		      machine_mode mode, bool return_p)
{
  gcc_assert (riscv_v_ext_mode_p (mode));

  info->mr_offset = cum->num_mrs;
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
    {
      /* For scalable mask return value.  */
      if (return_p)
	return gen_rtx_REG (mode, V_REG_FIRST);

      /* For the first scalable mask argument.  */
      if (info->mr_offset < MAX_ARGS_IN_MASK_REGISTERS)
	{
	  info->num_mrs = 1;
	  return gen_rtx_REG (mode, V_REG_FIRST);
	}
      else
	{
	  /* Rest scalable mask arguments are treated as scalable data
	     arguments.  */
	}
    }

  /* The number and alignment of vector registers need for this scalable vector
     argument. When the mode size is less than a full vector, we use 1 vector
     register to pass. Just call TARGET_HARD_REGNO_NREGS for the number
     information.  */
  int nregs = riscv_hard_regno_nregs (V_ARG_FIRST, mode);
  int LMUL = riscv_v_ext_tuple_mode_p (mode)
	       ? nregs / riscv_vector::get_nf (mode)
	       : nregs;
  int arg_reg_start = V_ARG_FIRST - V_REG_FIRST;
  int arg_reg_end = V_ARG_LAST - V_REG_FIRST;
  int aligned_reg_start = ROUND_UP (arg_reg_start, LMUL);

  /* For scalable data and scalable tuple return value.  */
  if (return_p)
    return gen_rtx_REG (mode, aligned_reg_start + V_REG_FIRST);

  /* Iterate through the USED_VRS array to find vector register groups that have
     not been allocated and the first register is aligned with LMUL.  */
  for (int i = aligned_reg_start; i + nregs - 1 <= arg_reg_end; i += LMUL)
    {
      /* The index in USED_VRS array.  */
      int idx = i - arg_reg_start;
      /* Find the first register unused.  */
      if (!cum->used_vrs[idx])
	{
	  bool find_set = true;
	  /* Ensure there are NREGS continuous unused registers.  */
	  for (int j = 1; j < nregs; j++)
	    if (cum->used_vrs[idx + j])
	      {
		find_set = false;
		/* Update I to the last aligned register which
		   cannot be used and the next iteration will add
		   LMUL step to I.  */
		i += (j / LMUL) * LMUL;
		break;
	      }

	  if (find_set)
	    {
	      info->num_vrs = nregs;
	      info->vr_offset = idx;
	      return gen_rtx_REG (mode, i + V_REG_FIRST);
	    }
	}
    }

  return NULL_RTX;
}

/* Fill INFO with information about a single argument, and return an RTL
   pattern to pass or return the argument. Return NULL_RTX if argument cannot
   pass or return in registers, then the argument may be passed by reference or
   through the stack or  .  CUM is the cumulative state for earlier arguments.
   MODE is the mode of this argument and TYPE is its type (if known). NAMED is
   true if this is a named (fixed) argument rather than a variable one. RETURN_P
   is true if returning the argument, or false if passing the argument.  */

static rtx
riscv_get_arg_info (struct riscv_arg_info *info, const CUMULATIVE_ARGS *cum,
		    machine_mode mode, const_tree type, bool named,
		    bool return_p)
{
  unsigned num_bytes, num_words;
  unsigned fpr_base = return_p ? FP_RETURN : FP_ARG_FIRST;
  unsigned gpr_base = return_p ? GP_RETURN : GP_ARG_FIRST;
  unsigned alignment = riscv_function_arg_boundary (mode, type);

  memset (info, 0, sizeof (*info));
  info->gpr_offset = cum->num_gprs;
  info->fpr_offset = cum->num_fprs;

  /* Passed by reference when the scalable vector argument is anonymous.  */
  if (riscv_v_ext_mode_p (mode) && !named)
    return NULL_RTX;

  if (named)
    {
      riscv_aggregate_field fields[2];
      unsigned fregno = fpr_base + info->fpr_offset;
      unsigned gregno = gpr_base + info->gpr_offset;

      /* Pass one- or two-element floating-point aggregates in FPRs.  */
      if ((info->num_fprs = riscv_pass_aggregate_in_fpr_pair_p (type, fields))
	  && info->fpr_offset + info->num_fprs <= MAX_ARGS_IN_REGISTERS)
	switch (info->num_fprs)
	  {
	  case 1:
	    return riscv_pass_fpr_single (mode, fregno,
					  TYPE_MODE (fields[0].type),
					  fields[0].offset);

	  case 2:
	    return riscv_pass_fpr_pair (mode, fregno,
					TYPE_MODE (fields[0].type),
					fields[0].offset,
					fregno + 1,
					TYPE_MODE (fields[1].type),
					fields[1].offset);

	  default:
	    gcc_unreachable ();
	  }

      /* Pass real and complex floating-point numbers in FPRs.  */
      if ((info->num_fprs = riscv_pass_mode_in_fpr_p (mode))
	  && info->fpr_offset + info->num_fprs <= MAX_ARGS_IN_REGISTERS)
	switch (GET_MODE_CLASS (mode))
	  {
	  case MODE_FLOAT:
	    return gen_rtx_REG (mode, fregno);

	  case MODE_COMPLEX_FLOAT:
	    return riscv_pass_fpr_pair (mode, fregno, GET_MODE_INNER (mode), 0,
					fregno + 1, GET_MODE_INNER (mode),
					GET_MODE_UNIT_SIZE (mode));

	  default:
	    gcc_unreachable ();
	  }

      /* Pass structs with one float and one integer in an FPR and a GPR.  */
      if (riscv_pass_aggregate_in_fpr_and_gpr_p (type, fields)
	  && info->gpr_offset < MAX_ARGS_IN_REGISTERS
	  && info->fpr_offset < MAX_ARGS_IN_REGISTERS)
	{
	  info->num_gprs = 1;
	  info->num_fprs = 1;

	  if (!SCALAR_FLOAT_TYPE_P (fields[0].type))
	    std::swap (fregno, gregno);

	  return riscv_pass_fpr_pair (mode, fregno, TYPE_MODE (fields[0].type),
				      fields[0].offset,
				      gregno, TYPE_MODE (fields[1].type),
				      fields[1].offset);
	}

      /* For scalable vector argument.  */
      if (riscv_vector_type_p (type) && riscv_v_ext_mode_p (mode))
	return riscv_get_vector_arg (info, cum, mode, return_p);

      /* For vls mode aggregated in gpr.  */
      if (riscv_v_ext_vls_mode_p (mode))
	return riscv_pass_vls_aggregate_in_gpr (info, mode, gpr_base);
    }

  /* Work out the size of the argument.  */
  num_bytes = type ? int_size_in_bytes (type) : GET_MODE_SIZE (mode).to_constant ();
  num_words = (num_bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  /* Doubleword-aligned varargs start on an even register boundary.  */
  if (!named && num_bytes != 0 && alignment > BITS_PER_WORD)
    info->gpr_offset += info->gpr_offset & 1;

  /* Partition the argument between registers and stack.  */
  info->num_fprs = 0;
  info->num_gprs = MIN (num_words, MAX_ARGS_IN_REGISTERS - info->gpr_offset);
  info->stack_p = (num_words - info->num_gprs) != 0;

  if (info->num_gprs || return_p)
    return gen_rtx_REG (mode, gpr_base + info->gpr_offset);

  return NULL_RTX;
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
riscv_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  struct riscv_arg_info info;

  if (arg.end_marker_p ())
    /* Return the calling convention that used by the current function. */
    return gen_int_mode (cum->variant_cc, SImode);

  return riscv_get_arg_info (&info, cum, arg.mode, arg.type, arg.named, false);
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */

static void
riscv_function_arg_advance (cumulative_args_t cum_v,
			    const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  struct riscv_arg_info info;

  riscv_get_arg_info (&info, cum, arg.mode, arg.type, arg.named, false);

  /* Set the corresponding register in USED_VRS to used status.  */
  for (unsigned int i = 0; i < info.num_vrs; i++)
    {
      gcc_assert (!cum->used_vrs[info.vr_offset + i]);
      cum->used_vrs[info.vr_offset + i] = true;
    }

  if ((info.num_vrs > 0 || info.num_mrs > 0) && cum->variant_cc != RISCV_CC_V)
    {
      error ("RVV type %qT cannot be passed to an unprototyped function",
	     arg.type);
      /* Avoid repeating the message */
      cum->variant_cc = RISCV_CC_V;
    }

  /* Advance the register count.  This has the effect of setting
     num_gprs to MAX_ARGS_IN_REGISTERS if a doubleword-aligned
     argument required us to skip the final GPR and pass the whole
     argument on the stack.  */
  cum->num_fprs = info.fpr_offset + info.num_fprs;
  cum->num_gprs = info.gpr_offset + info.num_gprs;
  cum->num_mrs = info.mr_offset + info.num_mrs;
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
riscv_arg_partial_bytes (cumulative_args_t cum,
			 const function_arg_info &generic_arg)
{
  struct riscv_arg_info arg;

  riscv_get_arg_info (&arg, get_cumulative_args (cum), generic_arg.mode,
		      generic_arg.type, generic_arg.named, false);
  return arg.stack_p ? arg.num_gprs * UNITS_PER_WORD : 0;
}

/* Implement FUNCTION_VALUE and LIBCALL_VALUE.  For normal calls,
   VALTYPE is the return type and MODE is VOIDmode.  For libcalls,
   VALTYPE is null and MODE is the mode of the return value.  */

rtx
riscv_function_value (const_tree type, const_tree func, machine_mode mode)
{
  struct riscv_arg_info info;
  CUMULATIVE_ARGS args;

  if (type)
    {
      int unsigned_p = TYPE_UNSIGNED (type);

      mode = TYPE_MODE (type);

      /* Since TARGET_PROMOTE_FUNCTION_MODE unconditionally promotes,
	 return values, promote the mode here too.  */
      mode = promote_function_mode (type, mode, &unsigned_p, func, 1);
    }

  memset (&args, 0, sizeof args);

  return riscv_get_arg_info (&info, &args, mode, type, true, true);
}

/* Implement TARGET_PASS_BY_REFERENCE. */

static bool
riscv_pass_by_reference (cumulative_args_t cum_v, const function_arg_info &arg)
{
  HOST_WIDE_INT size = arg.type_size_in_bytes ().to_constant ();;
  struct riscv_arg_info info;
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  /* ??? std_gimplify_va_arg_expr passes NULL for cum.  Fortunately, we
     never pass variadic arguments in floating-point and vector registers,
     so we can avoid the call to riscv_get_arg_info in this case.  */
  if (cum != NULL)
    {
      riscv_get_arg_info (&info, cum, arg.mode, arg.type, arg.named, false);

      /* Don't pass by reference if we can use a floating-point register.  */
      if (info.num_fprs)
	return false;

      /* Don't pass by reference if we can use general register(s) for vls.  */
      if (info.num_gprs && riscv_v_ext_vls_mode_p (arg.mode))
	return false;

      /* Don't pass by reference if we can use vector register groups.  */
      if (info.num_vrs > 0 || info.num_mrs > 0)
	return false;
    }

  /* Passed by reference when:
     1. The scalable vector argument is anonymous.
     2. Args cannot be passed through vector registers.  */
  if (riscv_v_ext_mode_p (arg.mode))
    return true;

  /* Pass by reference if the data do not fit in two integer registers.  */
  return !IN_RANGE (size, 0, 2 * UNITS_PER_WORD);
}

/* Implement TARGET_RETURN_IN_MEMORY.  */

static bool
riscv_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS args;
  cumulative_args_t cum = pack_cumulative_args (&args);

  /* The rules for returning in memory are the same as for passing the
     first named argument by reference.  */
  memset (&args, 0, sizeof args);
  function_arg_info arg (const_cast<tree> (type), /*named=*/true);
  return riscv_pass_by_reference (cum, arg);
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
riscv_setup_incoming_varargs (cumulative_args_t cum,
			      const function_arg_info &arg,
			      int *pretend_size ATTRIBUTE_UNUSED, int no_rtl)
{
  CUMULATIVE_ARGS local_cum;
  int gp_saved;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  local_cum = *get_cumulative_args (cum);
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    riscv_function_arg_advance (pack_cumulative_args (&local_cum), arg);

  /* Found out how many registers we need to save.  */
  gp_saved = MAX_ARGS_IN_REGISTERS - local_cum.num_gprs;

  if (!no_rtl && gp_saved > 0)
    {
      rtx ptr = plus_constant (Pmode, virtual_incoming_args_rtx,
			       REG_PARM_STACK_SPACE (cfun->decl)
			       - gp_saved * UNITS_PER_WORD);
      rtx mem = gen_frame_mem (BLKmode, ptr);
      set_mem_alias_set (mem, get_varargs_alias_set ());

      move_block_from_reg (local_cum.num_gprs + GP_ARG_FIRST,
			   mem, gp_saved);
    }
  if (REG_PARM_STACK_SPACE (cfun->decl) == 0)
    cfun->machine->varargs_size = gp_saved * UNITS_PER_WORD;
}

/* Return the descriptor of the Standard Vector Calling Convention Variant.  */

static const predefined_function_abi &
riscv_v_abi ()
{
  predefined_function_abi &v_abi = function_abis[RISCV_CC_V];
  if (!v_abi.initialized_p ())
    {
      HARD_REG_SET full_reg_clobbers
	= default_function_abi.full_reg_clobbers ();
      /* Callee-saved vector registers: v1-v7, v24-v31.  */
      for (int regno = V_REG_FIRST + 1; regno <= V_REG_FIRST + 7; regno += 1)
	CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      for (int regno = V_REG_FIRST + 24; regno <= V_REG_FIRST + 31; regno += 1)
	CLEAR_HARD_REG_BIT (full_reg_clobbers, regno);
      v_abi.initialize (RISCV_CC_V, full_reg_clobbers);
    }
  return v_abi;
}

static bool
riscv_vector_int_type_p (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);

  if (VECTOR_MODE_P (mode))
    return INTEGRAL_MODE_P (GET_MODE_INNER (mode));

  const char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  return strstr (name, "int") != NULL || strstr (name, "uint") != NULL;
}

static bool
riscv_vector_float_type_p (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);

  if (VECTOR_MODE_P (mode))
    return FLOAT_MODE_P (GET_MODE_INNER (mode));

  const char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  return strstr (name, "vfloat") != NULL;
}

static int
riscv_vector_element_bitsize (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);

  if (VECTOR_MODE_P (mode))
    return GET_MODE_BITSIZE (GET_MODE_INNER (mode));

  const char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));

  if (strstr (name, "bool") != NULL)
    return 1;
  else if (strstr (name, "int8") != NULL)
    return 8;
  else if (strstr (name, "int16") != NULL || strstr (name, "float16") != NULL)
    return 16;
  else if (strstr (name, "int32") != NULL || strstr (name, "float32") != NULL)
    return 32;
  else if (strstr (name, "int64") != NULL || strstr (name, "float64") != NULL)
    return 64;

  gcc_unreachable ();
}

static int
riscv_vector_required_min_vlen (const_tree type)
{
  machine_mode mode = TYPE_MODE (type);

  if (riscv_v_ext_mode_p (mode))
    return TARGET_MIN_VLEN;

  int element_bitsize = riscv_vector_element_bitsize (type);
  const char *name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));

  if (strstr (name, "bool64") != NULL)
    return element_bitsize * 64;
  else if (strstr (name, "bool32") != NULL)
    return element_bitsize * 32;
  else if (strstr (name, "bool16") != NULL)
    return element_bitsize * 16;
  else if (strstr (name, "bool8") != NULL)
    return element_bitsize * 8;
  else if (strstr (name, "bool4") != NULL)
    return element_bitsize * 4;
  else if (strstr (name, "bool2") != NULL)
    return element_bitsize * 2;

  if (strstr (name, "mf8") != NULL)
    return element_bitsize * 8;
  else if (strstr (name, "mf4") != NULL)
    return element_bitsize * 4;
  else if (strstr (name, "mf2") != NULL)
    return element_bitsize * 2;

  return element_bitsize;
}

static void
riscv_validate_vector_type (const_tree type, const char *hint)
{
  gcc_assert (riscv_vector_type_p (type));

  if (!TARGET_VECTOR)
    {
      error_at (input_location, "%s %qT requires the V ISA extension",
		hint, type);
      return;
    }

  int element_bitsize = riscv_vector_element_bitsize (type);
  bool int_type_p = riscv_vector_int_type_p (type);

  if (int_type_p && element_bitsize == 64
    && !TARGET_VECTOR_ELEN_64_P (riscv_vector_elen_flags))
    {
      error_at (input_location,
		"%s %qT requires the zve64x, zve64f, zve64d or v ISA extension",
		hint, type);
      return;
    }

  bool float_type_p = riscv_vector_float_type_p (type);

  if (float_type_p && element_bitsize == 16
    && !TARGET_VECTOR_ELEN_FP_16_P (riscv_vector_elen_flags))
    {
      error_at (input_location,
		"%s %qT requires the zvfhmin or zvfh ISA extension",
		hint, type);
      return;
    }

  if (float_type_p && element_bitsize == 32
    && !TARGET_VECTOR_ELEN_FP_32_P (riscv_vector_elen_flags))
    {
      error_at (input_location,
		"%s %qT requires the zve32f, zve64f, zve64d or v ISA extension",
		hint, type);
      return;
    }

  if (float_type_p && element_bitsize == 64
    && !TARGET_VECTOR_ELEN_FP_64_P (riscv_vector_elen_flags))
    {
      error_at (input_location,
		"%s %qT requires the zve64d or v ISA extension", hint, type);
      return;
    }

  int required_min_vlen = riscv_vector_required_min_vlen (type);

  if (TARGET_MIN_VLEN < required_min_vlen)
    {
      error_at (
	input_location,
	"%s %qT requires the minimal vector length %qd but %qd is given",
	hint, type, required_min_vlen, TARGET_MIN_VLEN);
      return;
    }
}

/* Return true if a function with type FNTYPE returns its value in
   RISC-V V registers.  */

static bool
riscv_return_value_is_vector_type_p (const_tree fntype)
{
  tree return_type = TREE_TYPE (fntype);

  if (riscv_vector_type_p (return_type))
    {
      riscv_validate_vector_type (return_type, "return type");
      return true;
    }
  else
    return false;
}

/* Return true if a function with type FNTYPE takes arguments in
   RISC-V V registers.  */

static bool
riscv_arguments_is_vector_type_p (const_tree fntype)
{
  for (tree chain = TYPE_ARG_TYPES (fntype); chain && chain != void_list_node;
       chain = TREE_CHAIN (chain))
    {
      tree arg_type = TREE_VALUE (chain);
      if (riscv_vector_type_p (arg_type))
	{
	  riscv_validate_vector_type (arg_type, "argument type");
	  return true;
	}
    }

  return false;
}

/* Return true if FUNC is a riscv_vector_cc function.
   For more details please reference the below link.
   https://github.com/riscv-non-isa/riscv-c-api-doc/pull/67 */
static bool
riscv_vector_cc_function_p (const_tree fntype)
{
  tree attr = TYPE_ATTRIBUTES (fntype);
  bool vector_cc_p = lookup_attribute ("vector_cc", attr) != NULL_TREE
    || lookup_attribute ("riscv_vector_cc", attr) != NULL_TREE;

  if (vector_cc_p && !TARGET_VECTOR)
    error_at (input_location,
	      "function attribute %qs requires the V ISA extension",
	      "riscv_vector_cc");

  return vector_cc_p;
}

/* Implement TARGET_FNTYPE_ABI.  */

static const predefined_function_abi &
riscv_fntype_abi (const_tree fntype)
{
  /* Implement the vector calling convention.  For more details please
     reference the below link.
     https://github.com/riscv-non-isa/riscv-elf-psabi-doc/pull/389  */
  if (riscv_return_value_is_vector_type_p (fntype)
	  || riscv_arguments_is_vector_type_p (fntype)
	  || riscv_vector_cc_function_p (fntype))
    return riscv_v_abi ();

  return default_function_abi;
}

/* Return riscv calling convention of call_insn.  */
riscv_cc
get_riscv_cc (const rtx use)
{
  gcc_assert (GET_CODE (use) == USE);
  rtx unspec = XEXP (use, 0);
  gcc_assert (GET_CODE (unspec) == UNSPEC
	      && XINT (unspec, 1) == UNSPEC_CALLEE_CC);
  riscv_cc cc = (riscv_cc) INTVAL (XVECEXP (unspec, 0, 0));
  gcc_assert (cc < RISCV_CC_UNKNOWN);
  return cc;
}

/* Implement TARGET_INSN_CALLEE_ABI.  */

const predefined_function_abi &
riscv_insn_callee_abi (const rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  gcc_assert (GET_CODE (pat) == PARALLEL);
  riscv_cc cc = get_riscv_cc (XVECEXP (pat, 0, 1));
  return function_abis[cc];
}

/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */
static tree
riscv_handle_fndecl_attribute (tree *node, tree name,
			       tree args ATTRIBUTE_UNUSED,
			       int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Verify type based attributes.  NODE is the what the attribute is being
   applied to.  NAME is the attribute name.  ARGS are the attribute args.
   FLAGS gives info about the context.  NO_ADD_ATTRS should be set to true if
   the attribute should be ignored.  */

static tree
riscv_handle_type_attribute (tree *node ATTRIBUTE_UNUSED, tree name, tree args,
			     int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  /* Check for an argument.  */
  if (is_attribute_p ("interrupt", name))
    {
      if (args)
	{
	  tree cst = TREE_VALUE (args);
	  const char *string;

	  if (TREE_CODE (cst) != STRING_CST)
	    {
	      warning (OPT_Wattributes,
		       "%qE attribute requires a string argument",
		       name);
	      *no_add_attrs = true;
	      return NULL_TREE;
	    }

	  string = TREE_STRING_POINTER (cst);
	  if (strcmp (string, "user") && strcmp (string, "supervisor")
	      && strcmp (string, "machine"))
	    {
	      warning (OPT_Wattributes,
		       "argument to %qE attribute is not %<\"user\"%>, %<\"supervisor\"%>, "
		       "or %<\"machine\"%>", name);
	      *no_add_attrs = true;
	    }
	}
    }

  return NULL_TREE;
}

static tree
riscv_handle_rvv_vector_bits_attribute (tree *node, tree name, tree args,
					ATTRIBUTE_UNUSED int flags,
					bool *no_add_attrs)
{
  if (!is_attribute_p ("riscv_rvv_vector_bits", name))
    return NULL_TREE;

  *no_add_attrs = true;

  if (rvv_vector_bits != RVV_VECTOR_BITS_ZVL)
    {
      error (
	"%qs is only supported when %<-mrvv-vector-bits=zvl%> is specified",
	"riscv_rvv_vector_bits");
      return NULL_TREE;
    }

  tree type = *node;

  if (!VECTOR_TYPE_P (type) || !riscv_vector::builtin_type_p (type))
    {
      error ("%qs applied to non-RVV type %qT", "riscv_rvv_vector_bits", type);
      return NULL_TREE;
    }

  tree size = TREE_VALUE (args);

  if (TREE_CODE (size) != INTEGER_CST)
    {
      error ("%qs requires an integer constant", "riscv_rvv_vector_bits");
      return NULL_TREE;
    }

  unsigned HOST_WIDE_INT args_in_bits = tree_to_uhwi (size);
  unsigned HOST_WIDE_INT type_mode_bits
    = GET_MODE_PRECISION (TYPE_MODE (type)).to_constant ();

  if (args_in_bits != type_mode_bits)
    {
      error ("invalid RVV vector size %qd, "
	     "expected size is %qd based on LMUL of type and %qs",
	     (int)args_in_bits, (int)type_mode_bits, "-mrvv-vector-bits=zvl");
      return NULL_TREE;
    }

  type = build_distinct_type_copy (type);
  TYPE_ATTRIBUTES (type)
    = remove_attribute ("RVV sizeless type",
			copy_list (TYPE_ATTRIBUTES (type)));

  /* The operations like alu/cmp on vbool*_t is not well defined,
     continue to treat vbool*_t as indivisible.  */
  if (!VECTOR_BOOLEAN_TYPE_P (type))
    TYPE_INDIVISIBLE_P (type) = 0;

  *node = type;

  return NULL_TREE;
}

/* Return true if function TYPE is an interrupt function.  */
static bool
riscv_interrupt_type_p (tree type)
{
  return lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type)) != NULL;
}

/* Return true if FUNC is a naked function.  */
static bool
riscv_naked_function_p (tree func)
{
  tree func_decl = func;
  if (func == NULL_TREE)
    func_decl = current_function_decl;
  return NULL_TREE != lookup_attribute ("naked", DECL_ATTRIBUTES (func_decl));
}

/* Implement TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS.  */
static bool
riscv_allocate_stack_slots_for_args ()
{
  /* Naked functions should not allocate stack slots for arguments.  */
  return !riscv_naked_function_p (current_function_decl);
}

/* Implement TARGET_WARN_FUNC_RETURN.  */
static bool
riscv_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */
  return !riscv_naked_function_p (decl);
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */

static void
riscv_va_start (tree valist, rtx nextarg)
{
  nextarg = plus_constant (Pmode, nextarg, -cfun->machine->varargs_size);
  std_expand_builtin_va_start (valist, nextarg);
}

/* Make ADDR suitable for use as a call or sibcall target.  */

rtx
riscv_legitimize_call_address (rtx addr)
{
  if (!call_insn_operand (addr, VOIDmode))
    {
      rtx reg = RISCV_CALL_ADDRESS_TEMP (Pmode);
      riscv_emit_move (reg, addr);
      return reg;
    }
  return addr;
}

/* Print symbolic operand OP, which is part of a HIGH or LO_SUM
   in context CONTEXT.  HI_RELOC indicates a high-part reloc.  */

static void
riscv_print_operand_reloc (FILE *file, rtx op, bool hi_reloc)
{
  const char *reloc;

  switch (riscv_classify_symbolic_expression (op))
    {
      case SYMBOL_ABSOLUTE:
	reloc = hi_reloc ? "%hi" : "%lo";
	break;

      case SYMBOL_PCREL:
	reloc = hi_reloc ? "%pcrel_hi" : "%pcrel_lo";
	break;

      case SYMBOL_TLS_LE:
	reloc = hi_reloc ? "%tprel_hi" : "%tprel_lo";
	break;

      default:
	output_operand_lossage ("invalid use of '%%%c'", hi_reloc ? 'h' : 'R');
	return;
    }

  fprintf (file, "%s(", reloc);
  output_addr_const (file, riscv_strip_unspec_address (op));
  fputc (')', file);
}

/* Return the memory model that encapuslates both given models.  */

enum memmodel
riscv_union_memmodels (enum memmodel model1, enum memmodel model2)
{
  model1 = memmodel_base (model1);
  model2 = memmodel_base (model2);

  enum memmodel weaker = model1 <= model2 ? model1: model2;
  enum memmodel stronger = model1 > model2 ? model1: model2;

  switch (stronger)
    {
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_ACQ_REL:
	return stronger;
      case MEMMODEL_RELEASE:
	if (weaker == MEMMODEL_ACQUIRE || weaker == MEMMODEL_CONSUME)
	  return MEMMODEL_ACQ_REL;
	else
	  return stronger;
      case MEMMODEL_ACQUIRE:
      case MEMMODEL_CONSUME:
      case MEMMODEL_RELAXED:
	return stronger;
      default:
	gcc_unreachable ();
    }
}

/* Return true if the .AQ suffix should be added to an AMO to implement the
   acquire portion of memory model MODEL.  */

static bool
riscv_memmodel_needs_amo_acquire (enum memmodel model)
{
  /* ZTSO amo mappings require no annotations.  */
  if (TARGET_ZTSO)
    return false;

  switch (model)
    {
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_ACQUIRE:
      case MEMMODEL_CONSUME:
	return true;

      case MEMMODEL_RELEASE:
      case MEMMODEL_RELAXED:
	return false;

      default:
	gcc_unreachable ();
    }
}

/* Return true if the .RL suffix should be added to an AMO to implement the
   release portion of memory model MODEL.  */

static bool
riscv_memmodel_needs_amo_release (enum memmodel model)
{
  /* ZTSO amo mappings require no annotations.  */
  if (TARGET_ZTSO)
    return false;

  switch (model)
    {
      case MEMMODEL_ACQ_REL:
      case MEMMODEL_SEQ_CST:
      case MEMMODEL_RELEASE:
	return true;

      case MEMMODEL_ACQUIRE:
      case MEMMODEL_CONSUME:
      case MEMMODEL_RELAXED:
	return false;

      default:
	gcc_unreachable ();
    }
}

/* Get REGNO alignment of vector mode.
   The alignment = LMUL when the LMUL >= 1.
   Otherwise, alignment = 1.  */
int
riscv_get_v_regno_alignment (machine_mode mode)
{
  /* 3.3.2. LMUL = 2,4,8, register numbers should be multiple of 2,4,8.
     but for mask vector register, register numbers can be any number. */
  int lmul = 1;
  machine_mode rvv_mode = mode;
  if (riscv_v_ext_vls_mode_p (rvv_mode))
    {
      int size = GET_MODE_BITSIZE (rvv_mode).to_constant ();
      if (size < TARGET_MIN_VLEN)
	return 1;
      else
	return size / TARGET_MIN_VLEN;
    }
  if (riscv_v_ext_tuple_mode_p (rvv_mode))
    rvv_mode = riscv_vector::get_subpart_mode (rvv_mode);
  poly_int64 size = GET_MODE_SIZE (rvv_mode);
  if (known_gt (size, UNITS_PER_V_REG))
    lmul = exact_div (size, UNITS_PER_V_REG).to_constant ();
  return lmul;
}

/* Define ASM_OUTPUT_OPCODE to do anything special before
   emitting an opcode.  */
const char *
riscv_asm_output_opcode (FILE *asm_out_file, const char *p)
{
  if (TARGET_XTHEADVECTOR)
     return th_asm_output_opcode (asm_out_file, p);

  return p;
}

/* Implement TARGET_PRINT_OPERAND.  The RISCV-specific operand codes are:

   'h'	Print the high-part relocation associated with OP, after stripping
	  any outermost HIGH.
   'R'	Print the low-part relocation associated with OP.
   'C'	Print the integer branch condition for comparison OP.
   'N'	Print the inverse of the integer branch condition for comparison OP.
   'A'	Print the atomic operation suffix for memory model OP.
   'I'	Print the LR suffix for memory model OP.
   'J'	Print the SC suffix for memory model OP.
   'z'	Print x0 if OP is zero, otherwise print OP normally.
   'i'	Print i if the operand is not a register.
   'S'	Print shift-index of single-bit mask OP.
   'T'	Print shift-index of inverted single-bit mask OP.
   '~'	Print w if TARGET_64BIT is true; otherwise not print anything.

   Note please keep this list and the list in riscv.md in sync.  */

static void
riscv_print_operand (FILE *file, rtx op, int letter)
{
  /* `~` does not take an operand so op will be null
     Check for before accessing op.
  */
  if (letter == '~')
    {
      if (TARGET_64BIT)
	fputc('w', file);
      return;
    }
  machine_mode mode = GET_MODE (op);
  enum rtx_code code = GET_CODE (op);

  switch (letter)
    {
      case 'o': {
	/* Print 'OP' variant for RVV instructions.
	   1. If the operand is VECTOR REG, we print 'v'(vnsrl.wv).
	   2. If the operand is CONST_INT/CONST_VECTOR, we print 'i'(vnsrl.wi).
	   3. If the operand is SCALAR REG, we print 'x'(vnsrl.wx).  */
	if (riscv_v_ext_mode_p (mode))
	  {
	    if (REG_P (op))
	      asm_fprintf (file, "v");
	    else if (CONST_VECTOR_P (op))
	      asm_fprintf (file, "i");
	    else
	      output_operand_lossage ("invalid vector operand");
	  }
	else
	  {
	    if (CONST_INT_P (op))
	      asm_fprintf (file, "i");
	    else
	      asm_fprintf (file, "x");
	  }
	break;
      }
      case 'v': {
	rtx elt;

	if (REG_P (op))
	  asm_fprintf (file, "%s", reg_names[REGNO (op)]);
	else
	  {
	    if (!const_vec_duplicate_p (op, &elt))
	      output_operand_lossage ("invalid vector constant");
	    else if (satisfies_constraint_Wc0 (op))
	      asm_fprintf (file, "0");
	    else if (satisfies_constraint_vi (op)
		     || satisfies_constraint_vj (op)
		     || satisfies_constraint_vk (op))
	      asm_fprintf (file, "%wd", INTVAL (elt));
	    else
	      output_operand_lossage ("invalid vector constant");
	  }
	break;
      }
      case 'V': {
	rtx elt;
	if (!const_vec_duplicate_p (op, &elt))
	  output_operand_lossage ("invalid vector constant");
	else if (satisfies_constraint_vj (op))
	  asm_fprintf (file, "%wd", -INTVAL (elt));
	else
	  output_operand_lossage ("invalid vector constant");
	break;
      }
      case 'm': {
	if (riscv_v_ext_mode_p (mode))
	  {
	    /* Calculate lmul according to mode and print the value.  */
	    int lmul = riscv_get_v_regno_alignment (mode);
	    asm_fprintf (file, "%d", lmul);
	  }
	else if (code == CONST_INT)
	  {
	    /* If it is a const_int value, it denotes the VLMUL field enum.  */
	    unsigned int vlmul = UINTVAL (op);
	    switch (vlmul)
	      {
	      case riscv_vector::LMUL_1:
		asm_fprintf (file, "%s", "m1");
		break;
	      case riscv_vector::LMUL_2:
		asm_fprintf (file, "%s", "m2");
		break;
	      case riscv_vector::LMUL_4:
		asm_fprintf (file, "%s", "m4");
		break;
	      case riscv_vector::LMUL_8:
		asm_fprintf (file, "%s", "m8");
		break;
	      case riscv_vector::LMUL_F8:
		asm_fprintf (file, "%s", "mf8");
		break;
	      case riscv_vector::LMUL_F4:
		asm_fprintf (file, "%s", "mf4");
		break;
	      case riscv_vector::LMUL_F2:
		asm_fprintf (file, "%s", "mf2");
		break;
	      default:
		gcc_unreachable ();
	      }
	  }
	else
	  output_operand_lossage ("invalid vector constant");
	break;
      }
      case 'p': {
	if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
	  {
	    /* Print for RVV mask operand.
	       If op is reg, print ",v0.t".
	       Otherwise, don't print anything.  */
	    if (code == REG)
	      fprintf (file, ",%s.t", reg_names[REGNO (op)]);
	  }
	else if (code == CONST_INT)
	  {
	    /* Tail && Mask policy.  */
	    asm_fprintf (file, "%s", IS_AGNOSTIC (UINTVAL (op)) ? "a" : "u");
	  }
	else
	  output_operand_lossage ("invalid vector constant");
	break;
      }
    case 'h':
      if (code == HIGH)
	op = XEXP (op, 0);
      riscv_print_operand_reloc (file, op, true);
      break;

    case 'R':
      riscv_print_operand_reloc (file, op, false);
      break;

    case 'C':
      /* The RTL names match the instruction names. */
      fputs (GET_RTX_NAME (code), file);
      break;

    case 'N':
      /* The RTL names match the instruction names. */
      fputs (GET_RTX_NAME (reverse_condition (code)), file);
      break;

    case 'A': {
      const enum memmodel model = memmodel_base (INTVAL (op));
      if (riscv_memmodel_needs_amo_acquire (model)
	  && riscv_memmodel_needs_amo_release (model))
	fputs (".aqrl", file);
      else if (riscv_memmodel_needs_amo_acquire (model))
	fputs (".aq", file);
      else if (riscv_memmodel_needs_amo_release (model))
	fputs (".rl", file);
      break;
    }

    case 'I': {
      const enum memmodel model = memmodel_base (INTVAL (op));
      if (TARGET_ZTSO && model != MEMMODEL_SEQ_CST)
	/* LR ops only have an annotation for SEQ_CST in the Ztso mapping.  */
	break;
      else if (model == MEMMODEL_SEQ_CST)
	fputs (".aqrl", file);
      else if (riscv_memmodel_needs_amo_acquire (model))
	fputs (".aq", file);
      break;
    }

    case 'J': {
      const enum memmodel model = memmodel_base (INTVAL (op));
      if (TARGET_ZTSO && model == MEMMODEL_SEQ_CST)
	/* SC ops only have an annotation for SEQ_CST in the Ztso mapping.  */
	fputs (".rl", file);
      else if (TARGET_ZTSO)
	break;
      else if (riscv_memmodel_needs_amo_release (model))
	fputs (".rl", file);
      break;
    }

    case 'i':
      if (code != REG)
        fputs ("i", file);
      break;

    case 'B':
      fputs (GET_RTX_NAME (code), file);
      break;

    case 'S':
      {
	rtx newop = GEN_INT (ctz_hwi (INTVAL (op)));
	output_addr_const (file, newop);
	break;
      }
    case 'T':
      {
	rtx newop = GEN_INT (ctz_hwi (~INTVAL (op)));
	output_addr_const (file, newop);
	break;
      }
    case 'X':
      {
	int ival = INTVAL (op) + 1;
	rtx newop = GEN_INT (ctz_hwi (ival) + 1);
	output_addr_const (file, newop);
	break;
      }
    case 'Y':
      {
	unsigned int imm = (UINTVAL (op) & 63);
	gcc_assert (imm <= 63);
	rtx newop = GEN_INT (imm);
	output_addr_const (file, newop);
	break;
      }
    default:
      switch (code)
	{
	case REG:
	  if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  fprintf (file, "%s", reg_names[REGNO (op)]);
	  break;

	case MEM:
	  if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  else
	    output_address (mode, XEXP (op, 0));
	  break;

	case CONST_DOUBLE:
	  {
	    if (letter == 'z' && op == CONST0_RTX (GET_MODE (op)))
	      {
		fputs (reg_names[GP_REG_FIRST], file);
		break;
	      }

	    int fli_index = riscv_float_const_rtx_index_for_fli (op);
	    if (fli_index == -1 || fli_index > 31)
	      {
		output_operand_lossage ("invalid use of '%%%c'", letter);
		break;
	      }
	    asm_fprintf (file, "%s", fli_value_print[fli_index]);
	    break;
	  }

	default:
	  if (letter == 'z' && op == CONST0_RTX (GET_MODE (op)))
	    fputs (reg_names[GP_REG_FIRST], file);
	  else if (letter && letter != 'z')
	    output_operand_lossage ("invalid use of '%%%c'", letter);
	  else
	    output_addr_const (file, riscv_strip_unspec_address (op));
	  break;
	}
    }
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P */
static bool
riscv_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '~');
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */

static void
riscv_print_operand_address (FILE *file, machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  struct riscv_address_info addr;

  if (th_print_operand_address (file, mode, x))
    return;

  if (riscv_classify_address (&addr, x, word_mode, true))
    switch (addr.type)
      {
      case ADDRESS_REG:
	output_addr_const (file, riscv_strip_unspec_address (addr.offset));
	fprintf (file, "(%s)", reg_names[REGNO (addr.reg)]);
	return;

      case ADDRESS_LO_SUM:
	riscv_print_operand_reloc (file, addr.offset, false);
	fprintf (file, "(%s)", reg_names[REGNO (addr.reg)]);
	return;

      case ADDRESS_CONST_INT:
	output_addr_const (file, x);
	fprintf (file, "(%s)", reg_names[GP_REG_FIRST]);
	return;

      case ADDRESS_SYMBOLIC:
	output_addr_const (file, riscv_strip_unspec_address (x));
	return;

      default:
	gcc_unreachable ();
      }

  gcc_unreachable ();
}

static bool
riscv_size_ok_for_small_data_p (int size)
{
  return g_switch_value && IN_RANGE (size, 1, g_switch_value);
}

/* Return true if EXP should be placed in the small data section. */

static bool
riscv_in_small_data_p (const_tree x)
{
  /* Because default_use_anchors_for_symbol_p doesn't gather small data to use
     the anchor symbol to address nearby objects.  In large model, it can get
     the better result using the anchor optiomization.  */
  if (riscv_cmodel == CM_LARGE)
    return false;

  if (TREE_CODE (x) == STRING_CST || TREE_CODE (x) == FUNCTION_DECL)
    return false;

  if (VAR_P (x) && DECL_SECTION_NAME (x))
    {
      const char *sec = DECL_SECTION_NAME (x);
      return strcmp (sec, ".sdata") == 0 || strcmp (sec, ".sbss") == 0;
    }

  return riscv_size_ok_for_small_data_p (int_size_in_bytes (TREE_TYPE (x)));
}

/* Switch to the appropriate section for output of DECL.  */

static section *
riscv_select_section (tree decl, int reloc,
		      unsigned HOST_WIDE_INT align)
{
  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_SRODATA:
      return get_named_section (decl, ".srodata", reloc);

    default:
      return default_elf_select_section (decl, reloc, align);
    }
}

/* Switch to the appropriate section for output of DECL.  */

static void
riscv_unique_section (tree decl, int reloc)
{
  const char *prefix = NULL;
  bool one_only = DECL_ONE_ONLY (decl) && !HAVE_COMDAT_GROUP;

  switch (categorize_decl_for_section (decl, reloc))
    {
    case SECCAT_SRODATA:
      prefix = one_only ? ".sr" : ".srodata";
      break;

    default:
      break;
    }
  if (prefix)
    {
      const char *name, *linkonce;
      char *string;

      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      name = targetm.strip_name_encoding (name);

      /* If we're using one_only, then there needs to be a .gnu.linkonce
	 prefix to the section name.  */
      linkonce = one_only ? ".gnu.linkonce" : "";

      string = ACONCAT ((linkonce, prefix, ".", name, NULL));

      set_decl_section_name (decl, string);
      return;
    }
  default_unique_section (decl, reloc);
}

/* Constant pools are per-function when in large code model.  */

static inline bool
riscv_can_use_per_function_literal_pools_p (void)
{
  return riscv_cmodel == CM_LARGE;
}

static bool
riscv_use_blocks_for_constant_p (machine_mode, const_rtx)
{
  /* We can't use blocks for constants when we're using a per-function
     constant pool.  */
  return !riscv_can_use_per_function_literal_pools_p ();
}

/* Return a section for X, handling small data. */

static section *
riscv_elf_select_rtx_section (machine_mode mode, rtx x,
			      unsigned HOST_WIDE_INT align)
{
  /* The literal pool stays with the function.  */
  if (riscv_can_use_per_function_literal_pools_p ())
    return function_section (current_function_decl);

  section *s = default_elf_select_rtx_section (mode, x, align);

  if (riscv_size_ok_for_small_data_p (GET_MODE_SIZE (mode).to_constant ()))
    {
      if (startswith (s->named.name, ".rodata.cst"))
	{
	  /* Rename .rodata.cst* to .srodata.cst*. */
	  char *name = (char *) alloca (strlen (s->named.name) + 2);
	  sprintf (name, ".s%s", s->named.name + 1);
	  return get_section (name, s->named.common.flags, NULL);
	}

      if (s == data_section)
	return sdata_section;
    }

  return s;
}

/* Make the last instruction frame-related and note that it performs
   the operation described by FRAME_PATTERN.  */

static void
riscv_set_frame_expr (rtx frame_pattern)
{
  rtx insn;

  insn = get_last_insn ();
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = alloc_EXPR_LIST (REG_FRAME_RELATED_EXPR,
				      frame_pattern,
				      REG_NOTES (insn));
}

/* Return a frame-related rtx that stores REG at MEM.
   REG must be a single register.  */

static rtx
riscv_frame_set (rtx mem, rtx reg)
{
  rtx set = gen_rtx_SET (mem, reg);
  RTX_FRAME_RELATED_P (set) = 1;
  return set;
}

/* Returns true if the current function might contain a far jump.  */

static bool
riscv_far_jump_used_p ()
{
  size_t func_size = 0;

  if (cfun->machine->far_jump_used)
    return true;

  /* We can't change far_jump_used during or after reload, as there is
     no chance to change stack frame layout.  So we must rely on the
     conservative heuristic below having done the right thing.  */
  if (reload_in_progress || reload_completed)
    return false;

  /* Estimate the function length.  */
  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    func_size += get_attr_length (insn);

  /* Conservatively determine whether some jump might exceed 1 MiB
     displacement.  */
  if (func_size * 2 >= 0x100000)
    cfun->machine->far_jump_used = true;

  return cfun->machine->far_jump_used;
}

/* Return true, if the current function must save the incoming return
   address.  */

static bool
riscv_save_return_addr_reg_p (void)
{
  /* The $ra register is call-clobbered: if this is not a leaf function,
     save it.  */
  if (!crtl->is_leaf)
    return true;

  /* We need to save the incoming return address if __builtin_eh_return
     is being used to set a different return address.  */
  if (crtl->calls_eh_return)
    return true;

  /* Far jumps/branches use $ra as a temporary to set up the target jump
     location (clobbering the incoming return address).  */
  if (riscv_far_jump_used_p ())
    return true;

  /* We need to save it if anyone has used that.  */
  if (df_regs_ever_live_p (RETURN_ADDR_REGNUM))
    return true;

  /* Need not to use ra for leaf when frame pointer is turned off by
     option whatever the omit-leaf-frame's value.  */
  if (frame_pointer_needed && crtl->is_leaf
      && !TARGET_OMIT_LEAF_FRAME_POINTER)
    return true;

  return false;
}

/* Return true if the current function must save register REGNO.  */

static bool
riscv_save_reg_p (unsigned int regno)
{
  bool call_saved = !global_regs[regno] && !call_used_or_fixed_reg_p (regno);
  bool might_clobber = crtl->saves_all_registers
		       || df_regs_ever_live_p (regno);

  if (call_saved && might_clobber)
    return true;

  /* Save callee-saved V registers.  */
  if (V_REG_P (regno) && !crtl->abi->clobbers_full_reg_p (regno)
      && might_clobber)
    return true;

  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return true;

  if (regno == RETURN_ADDR_REGNUM && riscv_save_return_addr_reg_p ())
    return true;

  /* If this is an interrupt handler, then must save extra registers.  */
  if (cfun->machine->interrupt_handler_p)
    {
      /* zero register is always zero.  */
      if (regno == GP_REG_FIRST)
	return false;

      /* The function will return the stack pointer to its original value.  */
      if (regno == STACK_POINTER_REGNUM)
	return false;

      /* By convention, we assume that gp and tp are safe.  */
      if (regno == GP_REGNUM || regno == THREAD_POINTER_REGNUM)
	return false;

      /* We must save every register used in this function.  If this is not a
	 leaf function, then we must save all temporary registers.  */
      if (df_regs_ever_live_p (regno)
	  || (!crtl->is_leaf && call_used_or_fixed_reg_p (regno)))
	return true;
    }

  return false;
}

/* Return TRUE if Zcmp push and pop insns should be
   avoided. FALSE otherwise.
   Only use multi push & pop if all GPRs masked can be covered,
   and stack access is SP based,
   and GPRs are at top of the stack frame,
   and no conflicts in stack allocation with other features  */
static bool
riscv_avoid_multi_push (const struct riscv_frame_info *frame)
{
  if (!TARGET_ZCMP || crtl->calls_eh_return || frame_pointer_needed
      || cfun->machine->interrupt_handler_p || cfun->machine->varargs_size != 0
      || crtl->args.pretend_args_size != 0
      || (use_shrink_wrapping_separate ()
	  && !riscv_avoid_shrink_wrapping_separate ())
      || (frame->mask & ~MULTI_PUSH_GPR_MASK))
    return true;

  return false;
}

/* Determine whether to use multi push insn.  */
static bool
riscv_use_multi_push (const struct riscv_frame_info *frame)
{
  if (riscv_avoid_multi_push (frame))
    return false;

  return (frame->multi_push_adj_base != 0);
}

/* Return TRUE if a libcall to save/restore GPRs should be
   avoided.  FALSE otherwise.  */
static bool
riscv_avoid_save_libcall (void)
{
  if (!TARGET_SAVE_RESTORE
      || crtl->calls_eh_return
      || frame_pointer_needed
      || cfun->machine->interrupt_handler_p
      || cfun->machine->varargs_size != 0
      || crtl->args.pretend_args_size != 0)
    return true;

  return false;
}

/* Determine whether to call GPR save/restore routines.  */
static bool
riscv_use_save_libcall (const struct riscv_frame_info *frame)
{
  if (riscv_avoid_save_libcall ())
    return false;

  return frame->save_libcall_adjustment != 0;
}

/* Determine which GPR save/restore routine to call.  */

static unsigned
riscv_save_libcall_count (unsigned mask)
{
  for (unsigned n = GP_REG_LAST; n > GP_REG_FIRST; n--)
    if (BITSET_P (mask, n))
      return CALLEE_SAVED_REG_NUMBER (n) + 1;
  abort ();
}

/* calculate number of s regs in multi push and pop.
   Note that {s0-s10} is not valid in Zcmp, use {s0-s11} instead.  */
static unsigned
riscv_multi_push_sregs_count (unsigned mask)
{
  unsigned num = riscv_save_libcall_count (mask);
  return (num == ZCMP_INVALID_S0S10_SREGS_COUNTS) ? ZCMP_S0S11_SREGS_COUNTS
						  : num;
}

/* calculate number of regs(ra, s0-sx) in multi push and pop.  */
static unsigned
riscv_multi_push_regs_count (unsigned mask)
{
  /* 1 is for ra  */
  return riscv_multi_push_sregs_count (mask) + 1;
}

/* Handle 16 bytes align for poly_int.  */
static poly_int64
riscv_16bytes_align (poly_int64 value)
{
  return aligned_upper_bound (value, 16);
}

static HOST_WIDE_INT
riscv_16bytes_align (HOST_WIDE_INT value)
{
  return ROUND_UP (value, 16);
}

/* Handle stack align for poly_int.  */
static poly_int64
riscv_stack_align (poly_int64 value)
{
  return aligned_upper_bound (value, PREFERRED_STACK_BOUNDARY / 8);
}

static HOST_WIDE_INT
riscv_stack_align (HOST_WIDE_INT value)
{
  return RISCV_STACK_ALIGN (value);
}

/* Populate the current function's riscv_frame_info structure.

   RISC-V stack frames grown downward.  High addresses are at the top.

	+-------------------------------+
	|                               |
	|  incoming stack arguments     |
	|                               |
	+-------------------------------+ <-- incoming stack pointer
	|                               |
	|  callee-allocated save area   |
	|  for arguments that are       |
	|  split between registers and  |
	|  the stack                    |
	|                               |
	+-------------------------------+ <-- arg_pointer_rtx
	|                               |
	|  callee-allocated save area   |
	|  for register varargs         |
	|                               |
	+-------------------------------+ <-- hard_frame_pointer_rtx;
	|                               |     stack_pointer_rtx + gp_sp_offset
	|  GPR save area                |       + UNITS_PER_WORD
	|                               |
	+-------------------------------+ <-- stack_pointer_rtx + fp_sp_offset
	|                               |       + UNITS_PER_FP_REG
	|  FPR save area                |
	|                               |
	+-------------------------------+ <-- stack_pointer_rtx
	|                               |       + v_sp_offset_top
	|  Vector Registers save area   |
	|                               |
	| ----------------------------- | <-- stack_pointer_rtx
	| padding                       |       + v_sp_offset_bottom
	+-------------------------------+ <-- frame_pointer_rtx (virtual)
	|                               |
	|  local variables              |
	|                               |
      P +-------------------------------+
	|                               |
	|  outgoing stack arguments     |
	|                               |
	+-------------------------------+ <-- stack_pointer_rtx

   Dynamic stack allocations such as alloca insert data at point P.
   They decrease stack_pointer_rtx but leave frame_pointer_rtx and
   hard_frame_pointer_rtx unchanged.  */

static HOST_WIDE_INT riscv_first_stack_step (struct riscv_frame_info *frame, poly_int64 remaining_size);

static void
riscv_compute_frame_info (void)
{
  struct riscv_frame_info *frame;
  poly_int64 offset;
  bool interrupt_save_prologue_temp = false;
  unsigned int regno, i, num_x_saved = 0, num_f_saved = 0, x_save_size = 0;
  unsigned int num_v_saved = 0;

  frame = &cfun->machine->frame;

  /* In an interrupt function, there are two cases in which t0 needs to be used:
     1, If we have a large frame, then we need to save/restore t0.  We check for
     this before clearing the frame struct.
     2, Need to save and restore some CSRs in the frame.  */
  if (cfun->machine->interrupt_handler_p)
    {
      HOST_WIDE_INT step1 = riscv_first_stack_step (frame, frame->total_size);
      if (! POLY_SMALL_OPERAND_P ((frame->total_size - step1))
	  || (TARGET_HARD_FLOAT || TARGET_ZFINX))
	interrupt_save_prologue_temp = true;
    }

  frame->reset();

  if (!cfun->machine->naked_p)
    {
      /* Find out which GPRs we need to save.  */
      for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
	if (riscv_save_reg_p (regno)
	    || (interrupt_save_prologue_temp
		&& (regno == RISCV_PROLOGUE_TEMP_REGNUM)))
	  frame->mask |= 1 << (regno - GP_REG_FIRST), num_x_saved++;

      /* If this function calls eh_return, we must also save and restore the
	 EH data registers.  */
      if (crtl->calls_eh_return)
	for (i = 0; (regno = EH_RETURN_DATA_REGNO (i)) != INVALID_REGNUM; i++)
	  frame->mask |= 1 << (regno - GP_REG_FIRST), num_x_saved++;

      /* Find out which FPRs we need to save.  This loop must iterate over
	 the same space as its companion in riscv_for_each_saved_reg.  */
      if (TARGET_HARD_FLOAT)
	for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
	  if (riscv_save_reg_p (regno))
	    frame->fmask |= 1 << (regno - FP_REG_FIRST), num_f_saved++;

      /* Find out which V registers we need to save. */
      if (TARGET_VECTOR)
	for (regno = V_REG_FIRST; regno <= V_REG_LAST; regno++)
	  if (riscv_save_reg_p (regno))
	    {
	      frame->vmask |= 1 << (regno - V_REG_FIRST);
	      num_v_saved++;
	    }
    }

  if (frame->mask)
    {
      x_save_size = riscv_stack_align (num_x_saved * UNITS_PER_WORD);

      /* 1 is for ra  */
      unsigned num_save_restore = 1 + riscv_save_libcall_count (frame->mask);
      /* Only use save/restore routines if they don't alter the stack size.  */
      if (riscv_stack_align (num_save_restore * UNITS_PER_WORD) == x_save_size
          && !riscv_avoid_save_libcall ())
	{
	  /* Libcall saves/restores 3 registers at once, so we need to
	     allocate 12 bytes for callee-saved register.  */
	  if (TARGET_RVE)
	    x_save_size = 3 * UNITS_PER_WORD;

	  frame->save_libcall_adjustment = x_save_size;
	}

      if (!riscv_avoid_multi_push (frame))
	{
	  /* num(ra, s0-sx)  */
	  unsigned num_multi_push = riscv_multi_push_regs_count (frame->mask);
	  x_save_size = riscv_stack_align (num_multi_push * UNITS_PER_WORD);
	  frame->multi_push_adj_base = riscv_16bytes_align (x_save_size);
	}
    }

  /* In an interrupt function, we need extra space for the initial saves of CSRs.  */
  if (cfun->machine->interrupt_handler_p
      && ((TARGET_HARD_FLOAT && frame->fmask)
	  || (TARGET_ZFINX
	      /* Except for RISCV_PROLOGUE_TEMP_REGNUM.  */
	      && (frame->mask & ~(1 << RISCV_PROLOGUE_TEMP_REGNUM)))))
    /* Save and restore FCSR.  */
    /* TODO: When P or V extensions support interrupts, some of their CSRs
       may also need to be saved and restored.  */
    x_save_size += riscv_stack_align (1 * UNITS_PER_WORD);

  /* At the bottom of the frame are any outgoing stack arguments. */
  offset = riscv_stack_align (crtl->outgoing_args_size);
  /* Next are local stack variables. */
  offset += riscv_stack_align (get_frame_size ());
  /* The virtual frame pointer points above the local variables. */
  frame->frame_pointer_offset = offset;
  /* Next are the callee-saved VRs.  */
  if (frame->vmask)
    offset += riscv_stack_align (num_v_saved * UNITS_PER_V_REG);
  frame->v_sp_offset_top = offset;
  frame->v_sp_offset_bottom
    = frame->v_sp_offset_top - num_v_saved * UNITS_PER_V_REG;
  /* Next are the callee-saved FPRs. */
  if (frame->fmask)
    offset += riscv_stack_align (num_f_saved * UNITS_PER_FP_REG);
  frame->fp_sp_offset = offset - UNITS_PER_FP_REG;
  /* Next are the callee-saved GPRs. */
  if (frame->mask)
    {
      offset += x_save_size;
      /* align to 16 bytes and add paddings to GPR part to honor
	 both stack alignment and zcmp pus/pop size alignment. */
      if (riscv_use_multi_push (frame)
	  && known_lt (offset, frame->multi_push_adj_base
				 + ZCMP_SP_INC_STEP * ZCMP_MAX_SPIMM))
	offset = riscv_16bytes_align (offset);
    }
  frame->gp_sp_offset = offset - UNITS_PER_WORD;
  /* The hard frame pointer points above the callee-saved GPRs. */
  frame->hard_frame_pointer_offset = offset;
  /* Above the hard frame pointer is the callee-allocated varags save area. */
  offset += riscv_stack_align (cfun->machine->varargs_size);
  /* Next is the callee-allocated area for pretend stack arguments.  */
  offset += riscv_stack_align (crtl->args.pretend_args_size);
  /* Arg pointer must be below pretend args, but must be above alignment
     padding.  */
  frame->arg_pointer_offset = offset - crtl->args.pretend_args_size;
  frame->total_size = offset;

  /* Next points the incoming stack pointer and any incoming arguments. */
}

/* Make sure that we're not trying to eliminate to the wrong hard frame
   pointer.  */

static bool
riscv_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == HARD_FRAME_POINTER_REGNUM || to == STACK_POINTER_REGNUM);
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame pointer
   or argument pointer.  TO is either the stack pointer or hard frame
   pointer.  */

poly_int64
riscv_initial_elimination_offset (int from, int to)
{
  poly_int64 src, dest;

  riscv_compute_frame_info ();

  if (to == HARD_FRAME_POINTER_REGNUM)
    dest = cfun->machine->frame.hard_frame_pointer_offset;
  else if (to == STACK_POINTER_REGNUM)
    dest = 0; /* The stack pointer is the base of all offsets, hence 0.  */
  else
    gcc_unreachable ();

  if (from == FRAME_POINTER_REGNUM)
    src = cfun->machine->frame.frame_pointer_offset;
  else if (from == ARG_POINTER_REGNUM)
    src = cfun->machine->frame.arg_pointer_offset;
  else
    gcc_unreachable ();

  return src - dest;
}

/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
riscv_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RETURN_ADDR_REGNUM);
}

/* Emit code to change the current function's return address to
   ADDRESS.  SCRATCH is available as a scratch register, if needed.
   ADDRESS and SCRATCH are both word-mode GPRs.  */

void
riscv_set_return_address (rtx address, rtx scratch)
{
  rtx slot_address;

  gcc_assert (BITSET_P (cfun->machine->frame.mask, RETURN_ADDR_REGNUM));
  slot_address = riscv_add_offset (scratch, stack_pointer_rtx,
				  cfun->machine->frame.gp_sp_offset.to_constant());
  riscv_emit_move (gen_frame_mem (GET_MODE (address), slot_address), address);
}

/* Save register REG to MEM.  Make the instruction frame-related.  */

static void
riscv_save_reg (rtx reg, rtx mem)
{
  riscv_emit_move (mem, reg);
  riscv_set_frame_expr (riscv_frame_set (mem, reg));
}

/* Restore register REG from MEM.  */

static void
riscv_restore_reg (rtx reg, rtx mem)
{
  rtx insn = riscv_emit_move (reg, mem);
  rtx dwarf = NULL_RTX;
  dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);

  if (known_gt (epilogue_cfa_sp_offset, 0)
      && REGNO (reg) == HARD_FRAME_POINTER_REGNUM)
    {
      rtx cfa_adjust_rtx
	= gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			gen_int_mode (epilogue_cfa_sp_offset, Pmode));
      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, cfa_adjust_rtx, dwarf);
    }

  REG_NOTES (insn) = dwarf;
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* A function to save or store a register.  The first argument is the
   register and the second is the stack slot.  */
typedef void (*riscv_save_restore_fn) (rtx, rtx);

/* Use FN to save or restore register REGNO.  MODE is the register's
   mode and OFFSET is the offset of its save slot from the current
   stack pointer.  */

static void
riscv_save_restore_reg (machine_mode mode, int regno,
		       HOST_WIDE_INT offset, riscv_save_restore_fn fn)
{
  rtx mem;

  mem = gen_frame_mem (mode, plus_constant (Pmode, stack_pointer_rtx, offset));
  fn (gen_rtx_REG (mode, regno), mem);
}

/* Return the next register up from REGNO up to LIMIT for the callee
   to save or restore.  OFFSET will be adjusted accordingly.
   If INC is set, then REGNO will be incremented first.
   Returns INVALID_REGNUM if there is no such next register.  */

static unsigned int
riscv_next_saved_reg (unsigned int regno, unsigned int limit,
		      HOST_WIDE_INT *offset, bool inc = true)
{
  if (inc)
    regno++;

  while (regno <= limit)
    {
      if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
	{
	  *offset = *offset - UNITS_PER_WORD;
	  return regno;
	}

      regno++;
    }
  return INVALID_REGNUM;
}

/* Return TRUE if provided REGNO is eh return data register.  */

static bool
riscv_is_eh_return_data_register (unsigned int regno)
{
  unsigned int i, regnum;

  if (!crtl->calls_eh_return)
    return false;

  for (i = 0; (regnum = EH_RETURN_DATA_REGNO (i)) != INVALID_REGNUM; i++)
    if (regno == regnum)
      {
	return true;
      }

  return false;
}

/* Call FN for each register that is saved by the current function.
   SP_OFFSET is the offset of the current stack pointer from the start
   of the frame.  */

static void
riscv_for_each_saved_reg (poly_int64 sp_offset, riscv_save_restore_fn fn,
			  bool epilogue, bool maybe_eh_return)
{
  HOST_WIDE_INT offset, first_fp_offset;
  unsigned int regno, num_masked_fp = 0;
  unsigned int start = GP_REG_FIRST;
  unsigned int limit = GP_REG_LAST;

  /* Save the link register and s-registers. */
  offset = (cfun->machine->frame.gp_sp_offset - sp_offset).to_constant ()
	   + UNITS_PER_WORD;
  for (regno = riscv_next_saved_reg (start, limit, &offset, false);
       regno != INVALID_REGNUM;
       regno = riscv_next_saved_reg (regno, limit, &offset))
    {
      if (cfun->machine->reg_is_wrapped_separately[regno])
	continue;

      /* If this is a normal return in a function that calls the eh_return
	 builtin, then do not restore the eh return data registers as that
	 would clobber the return value.  But we do still need to save them
	 in the prologue, and restore them for an exception return, so we
	 need special handling here.  */
      if (epilogue && !maybe_eh_return
	  && riscv_is_eh_return_data_register (regno))
	continue;

      /* In an interrupt function, save and restore some necessary CSRs in the stack
	 to avoid changes in CSRs.  */
      if (regno == RISCV_PROLOGUE_TEMP_REGNUM
	  && cfun->machine->interrupt_handler_p
	  && ((TARGET_HARD_FLOAT  && cfun->machine->frame.fmask)
	      || (TARGET_ZFINX
		  && (cfun->machine->frame.mask & ~(1 << RISCV_PROLOGUE_TEMP_REGNUM)))))
	{
	  /* Always assume FCSR occupy UNITS_PER_WORD to prevent stack
	     offset misaligned later.  */
	  unsigned int fcsr_size = UNITS_PER_WORD;
	  if (!epilogue)
	    {
	      riscv_save_restore_reg (word_mode, regno, offset, fn);
	      offset -= fcsr_size;
	      emit_insn (gen_riscv_frcsr (RISCV_PROLOGUE_TEMP (SImode)));
	      riscv_save_restore_reg (SImode, RISCV_PROLOGUE_TEMP_REGNUM,
				      offset, riscv_save_reg);
	    }
	  else
	    {
	      riscv_save_restore_reg (SImode, RISCV_PROLOGUE_TEMP_REGNUM,
				      offset - fcsr_size, riscv_restore_reg);
	      emit_insn (gen_riscv_fscsr (RISCV_PROLOGUE_TEMP (SImode)));
	      riscv_save_restore_reg (word_mode, regno, offset, fn);
	      offset -= fcsr_size;
	    }
	  continue;
	}

      if (TARGET_XTHEADMEMPAIR)
	{
	  /* Get the next reg/offset pair.  */
	  HOST_WIDE_INT offset2 = offset;
	  unsigned int regno2 = riscv_next_saved_reg (regno, limit, &offset2);

	  /* Validate everything before emitting a mempair instruction.  */
	  if (regno2 != INVALID_REGNUM
	      && !cfun->machine->reg_is_wrapped_separately[regno2]
	      && !(epilogue && !maybe_eh_return
		   && riscv_is_eh_return_data_register (regno2)))
	    {
	      bool load_p = (fn == riscv_restore_reg);
	      rtx operands[4];
	      th_mempair_prepare_save_restore_operands (operands,
							load_p, word_mode,
							regno, offset,
							regno2, offset2);

	      /* If the operands fit into a mempair insn, then emit one.  */
	      if (th_mempair_operands_p (operands, load_p, word_mode))
		{
		  th_mempair_save_restore_regs (operands, load_p, word_mode);
		  offset = offset2;
		  regno = regno2;
		  continue;
		}
	    }
	}

      riscv_save_restore_reg (word_mode, regno, offset, fn);
    }

  /* This loop must iterate over the same space as its companion in
     riscv_compute_frame_info.  */
  first_fp_offset
    = (cfun->machine->frame.fp_sp_offset - sp_offset).to_constant ();
  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	bool handle_reg = !cfun->machine->reg_is_wrapped_separately[regno];
	machine_mode mode = TARGET_DOUBLE_FLOAT ? DFmode : SFmode;
	unsigned int slot = (riscv_use_multi_push (&cfun->machine->frame))
			      ? CALLEE_SAVED_FREG_NUMBER (regno)
			      : num_masked_fp;
	offset = first_fp_offset - slot * GET_MODE_SIZE (mode).to_constant ();
	if (handle_reg)
	  riscv_save_restore_reg (mode, regno, offset, fn);
	num_masked_fp++;
      }
}

/* Call FN for each V register that is saved by the current function.  */

static void
riscv_for_each_saved_v_reg (poly_int64 &remaining_size,
			    riscv_save_restore_fn fn, bool prologue)
{
  rtx vlen = NULL_RTX;
  if (cfun->machine->frame.vmask != 0)
    {
      if (UNITS_PER_V_REG.is_constant ()
	  && SMALL_OPERAND (UNITS_PER_V_REG.to_constant ()))
	vlen = GEN_INT (UNITS_PER_V_REG.to_constant ());
      else
	{
	  vlen = RISCV_PROLOGUE_TEMP (Pmode);
	  rtx insn
	    = emit_move_insn (vlen, gen_int_mode (UNITS_PER_V_REG, Pmode));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  /* Select the mode where LMUL is 1 and SEW is largest.  */
  machine_mode m1_mode = TARGET_VECTOR_ELEN_64 ? RVVM1DImode : RVVM1SImode;

  if (prologue)
    {
      /* This loop must iterate over the same space as its companion in
	 riscv_compute_frame_info.  */
      for (unsigned int regno = V_REG_FIRST; regno <= V_REG_LAST; regno++)
	if (BITSET_P (cfun->machine->frame.vmask, regno - V_REG_FIRST))
	  {
	    bool handle_reg = !cfun->machine->reg_is_wrapped_separately[regno];
	    if (handle_reg)
	      {
		rtx insn = NULL_RTX;
		if (CONST_INT_P (vlen))
		  {
		    gcc_assert (SMALL_OPERAND (-INTVAL (vlen)));
		    insn = emit_insn (gen_add3_insn (stack_pointer_rtx,
						     stack_pointer_rtx,
						     GEN_INT (-INTVAL (vlen))));
		  }
		else
		  insn = emit_insn (
		    gen_sub3_insn (stack_pointer_rtx, stack_pointer_rtx, vlen));
		gcc_assert (insn != NULL_RTX);
		RTX_FRAME_RELATED_P (insn) = 1;
		riscv_save_restore_reg (m1_mode, regno, 0, fn);
		remaining_size -= UNITS_PER_V_REG;
	      }
	  }
    }
  else
    {
      /* This loop must iterate over the same space as its companion in
	 riscv_compute_frame_info.  */
      for (unsigned int regno = V_REG_LAST; regno >= V_REG_FIRST; regno--)
	if (BITSET_P (cfun->machine->frame.vmask, regno - V_REG_FIRST))
	  {
	    bool handle_reg = !cfun->machine->reg_is_wrapped_separately[regno];
	    if (handle_reg)
	      {
		riscv_save_restore_reg (m1_mode, regno, 0, fn);
		rtx insn = emit_insn (
		  gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx, vlen));
		gcc_assert (insn != NULL_RTX);
		RTX_FRAME_RELATED_P (insn) = 1;
		remaining_size -= UNITS_PER_V_REG;
	      }
	  }
    }
}

/* For stack frames that can't be allocated with a single ADDI instruction,
   compute the best value to initially allocate.  It must at a minimum
   allocate enough space to spill the callee-saved registers.  If TARGET_RVC,
   try to pick a value that will allow compression of the register saves
   without adding extra instructions.  */

static HOST_WIDE_INT
riscv_first_stack_step (struct riscv_frame_info *frame, poly_int64 remaining_size)
{
  HOST_WIDE_INT remaining_const_size;
  if (!remaining_size.is_constant ())
    remaining_const_size
      = riscv_stack_align (remaining_size.coeffs[0])
	- riscv_stack_align (remaining_size.coeffs[1]);
  else
    remaining_const_size = remaining_size.to_constant ();

  /* First step must be set to the top of vector registers save area if any
     vector registers need be preversed.  */
  if (frame->vmask != 0)
    return (remaining_size - frame->v_sp_offset_top).to_constant ();

  if (SMALL_OPERAND (remaining_const_size))
    return remaining_const_size;

  poly_int64 callee_saved_first_step =
    remaining_size - frame->frame_pointer_offset;
  gcc_assert(callee_saved_first_step.is_constant ());
  HOST_WIDE_INT min_first_step =
    riscv_stack_align (callee_saved_first_step.to_constant ());
  HOST_WIDE_INT max_first_step = IMM_REACH / 2 - PREFERRED_STACK_BOUNDARY / 8;
  HOST_WIDE_INT min_second_step = remaining_const_size - max_first_step;
  gcc_assert (min_first_step <= max_first_step);

  /* As an optimization, use the least-significant bits of the total frame
     size, so that the second adjustment step is just LUI + ADD.  */
  if (!SMALL_OPERAND (min_second_step)
      && remaining_const_size % IMM_REACH <= max_first_step
      && remaining_const_size % IMM_REACH >= min_first_step)
    return remaining_const_size % IMM_REACH;

  if (TARGET_RVC || TARGET_ZCA)
    {
      /* If we need two subtracts, and one is small enough to allow compressed
	 loads and stores, then put that one first.  */
      if (IN_RANGE (min_second_step, 0,
		    (TARGET_64BIT ? SDSP_REACH : SWSP_REACH)))
       return MAX (min_second_step, min_first_step);

      /* If we need LUI + ADDI + ADD for the second adjustment step, then start
	 with the minimum first step, so that we can get compressed loads and
	 stores.  */
      else if (!SMALL_OPERAND (min_second_step))
       return min_first_step;
    }

  return max_first_step;
}

static rtx
riscv_adjust_libcall_cfi_prologue ()
{
  rtx dwarf = NULL_RTX;
  rtx adjust_sp_rtx, reg, mem, insn;
  int saved_size = cfun->machine->frame.save_libcall_adjustment;
  int offset;

  for (int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	/* The save order is ra, s0, s1, s2 to s11.  */
	if (regno == RETURN_ADDR_REGNUM)
	  offset = saved_size - UNITS_PER_WORD;
	else if (regno == S0_REGNUM)
	  offset = saved_size - UNITS_PER_WORD * 2;
	else if (regno == S1_REGNUM)
	  offset = saved_size - UNITS_PER_WORD * 3;
	else
	  offset = saved_size - ((regno - S2_REGNUM + 4) * UNITS_PER_WORD);

	reg = gen_rtx_REG (Pmode, regno);
	mem = gen_frame_mem (Pmode, plus_constant (Pmode,
						   stack_pointer_rtx,
						   offset));

	insn = gen_rtx_SET (mem, reg);
	dwarf = alloc_reg_note (REG_CFA_OFFSET, insn, dwarf);
      }

  /* Debug info for adjust sp.  */
  adjust_sp_rtx =
    gen_rtx_SET (stack_pointer_rtx,
		 gen_rtx_PLUS (GET_MODE(stack_pointer_rtx), stack_pointer_rtx, GEN_INT (-saved_size)));
  dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, adjust_sp_rtx,
			  dwarf);
  return dwarf;
}

static rtx
riscv_adjust_multi_push_cfi_prologue (int saved_size)
{
  rtx dwarf = NULL_RTX;
  rtx adjust_sp_rtx, reg, mem, insn;
  unsigned int mask = cfun->machine->frame.mask;
  int offset;
  int saved_cnt = 0;

  if (mask & S10_MASK)
    mask |= S11_MASK;

  for (int regno = GP_REG_LAST; regno >= GP_REG_FIRST; regno--)
    if (BITSET_P (mask & MULTI_PUSH_GPR_MASK, regno - GP_REG_FIRST))
      {
	/* The save order is s11-s0, ra
	   from high to low addr.  */
	offset = saved_size - UNITS_PER_WORD * (++saved_cnt);

	reg = gen_rtx_REG (Pmode, regno);
	mem = gen_frame_mem (Pmode,
			     plus_constant (Pmode, stack_pointer_rtx, offset));

	insn = gen_rtx_SET (mem, reg);
	dwarf = alloc_reg_note (REG_CFA_OFFSET, insn, dwarf);
      }

  /* Debug info for adjust sp.  */
  adjust_sp_rtx
    = gen_rtx_SET (stack_pointer_rtx,
		   plus_constant (Pmode, stack_pointer_rtx, -saved_size));
  dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, adjust_sp_rtx, dwarf);
  return dwarf;
}

static void
riscv_emit_stack_tie (void)
{
  if (Pmode == SImode)
    emit_insn (gen_stack_tiesi (stack_pointer_rtx, hard_frame_pointer_rtx));
  else
    emit_insn (gen_stack_tiedi (stack_pointer_rtx, hard_frame_pointer_rtx));
}

/*zcmp multi push and pop code_for_push_pop function ptr array  */
static const code_for_push_pop_t code_for_push_pop[ZCMP_MAX_GRP_SLOTS][ZCMP_OP_NUM]
  = {{code_for_gpr_multi_push_up_to_ra, code_for_gpr_multi_pop_up_to_ra,
      code_for_gpr_multi_popret_up_to_ra, code_for_gpr_multi_popretz_up_to_ra},
     {code_for_gpr_multi_push_up_to_s0, code_for_gpr_multi_pop_up_to_s0,
      code_for_gpr_multi_popret_up_to_s0, code_for_gpr_multi_popretz_up_to_s0},
     {code_for_gpr_multi_push_up_to_s1, code_for_gpr_multi_pop_up_to_s1,
      code_for_gpr_multi_popret_up_to_s1, code_for_gpr_multi_popretz_up_to_s1},
     {code_for_gpr_multi_push_up_to_s2, code_for_gpr_multi_pop_up_to_s2,
      code_for_gpr_multi_popret_up_to_s2, code_for_gpr_multi_popretz_up_to_s2},
     {code_for_gpr_multi_push_up_to_s3, code_for_gpr_multi_pop_up_to_s3,
      code_for_gpr_multi_popret_up_to_s3, code_for_gpr_multi_popretz_up_to_s3},
     {code_for_gpr_multi_push_up_to_s4, code_for_gpr_multi_pop_up_to_s4,
      code_for_gpr_multi_popret_up_to_s4, code_for_gpr_multi_popretz_up_to_s4},
     {code_for_gpr_multi_push_up_to_s5, code_for_gpr_multi_pop_up_to_s5,
      code_for_gpr_multi_popret_up_to_s5, code_for_gpr_multi_popretz_up_to_s5},
     {code_for_gpr_multi_push_up_to_s6, code_for_gpr_multi_pop_up_to_s6,
      code_for_gpr_multi_popret_up_to_s6, code_for_gpr_multi_popretz_up_to_s6},
     {code_for_gpr_multi_push_up_to_s7, code_for_gpr_multi_pop_up_to_s7,
      code_for_gpr_multi_popret_up_to_s7, code_for_gpr_multi_popretz_up_to_s7},
     {code_for_gpr_multi_push_up_to_s8, code_for_gpr_multi_pop_up_to_s8,
      code_for_gpr_multi_popret_up_to_s8, code_for_gpr_multi_popretz_up_to_s8},
     {code_for_gpr_multi_push_up_to_s9, code_for_gpr_multi_pop_up_to_s9,
      code_for_gpr_multi_popret_up_to_s9, code_for_gpr_multi_popretz_up_to_s9},
     {nullptr, nullptr, nullptr, nullptr},
     {code_for_gpr_multi_push_up_to_s11, code_for_gpr_multi_pop_up_to_s11,
      code_for_gpr_multi_popret_up_to_s11,
      code_for_gpr_multi_popretz_up_to_s11}};

static rtx
riscv_gen_multi_push_pop_insn (riscv_zcmp_op_t op, HOST_WIDE_INT adj_size,
			       unsigned int regs_num)
{
  gcc_assert (op < ZCMP_OP_NUM);
  gcc_assert (regs_num <= ZCMP_MAX_GRP_SLOTS
	      && regs_num != ZCMP_INVALID_S0S10_SREGS_COUNTS + 1); /* 1 for ra*/
  rtx stack_adj = GEN_INT (adj_size);
  return GEN_FCN (code_for_push_pop[regs_num - 1][op](Pmode)) (stack_adj);
}

static unsigned
get_multi_push_fpr_mask (unsigned max_fprs_push)
{
  unsigned mask_fprs_push = 0, num_f_pushed = 0;
  for (unsigned regno = FP_REG_FIRST;
       regno <= FP_REG_LAST && num_f_pushed < max_fprs_push; regno++)
    if (riscv_save_reg_p (regno))
      mask_fprs_push |= 1 << (regno - FP_REG_FIRST), num_f_pushed++;
  return mask_fprs_push;
}

/* Expand the "prologue" pattern.  */

void
riscv_expand_prologue (void)
{
  struct riscv_frame_info *frame = &cfun->machine->frame;
  poly_int64 remaining_size = frame->total_size;
  unsigned mask = frame->mask;
  unsigned fmask = frame->fmask;
  int spimm, multi_push_additional, stack_adj;
  rtx insn, dwarf = NULL_RTX;
  unsigned th_int_mask = 0;

  if (flag_stack_usage_info)
    current_function_static_stack_size = constant_lower_bound (remaining_size);

  if (cfun->machine->naked_p)
    return;

  /* prefer muti-push to save-restore libcall.  */
  if (riscv_use_multi_push (frame))
    {
      remaining_size -= frame->multi_push_adj_base;
      /* If there are vector registers that need to be saved, then it can only
	 be reduced to the frame->v_sp_offset_top position at most, since the
	 vector registers will need to be saved one by one by decreasing the SP
	 later.  */
      poly_int64 remaining_size_above_varea
	= frame->vmask != 0
	    ? remaining_size - frame->v_sp_offset_top
	    : remaining_size;

      if (known_gt (remaining_size_above_varea, 2 * ZCMP_SP_INC_STEP))
	spimm = 3;
      else if (known_gt (remaining_size_above_varea, ZCMP_SP_INC_STEP))
	spimm = 2;
      else if (known_gt (remaining_size_above_varea, 0))
	spimm = 1;
      else
	spimm = 0;
      multi_push_additional = spimm * ZCMP_SP_INC_STEP;
      frame->multi_push_adj_addi = multi_push_additional;
      remaining_size -= multi_push_additional;

      /* emit multi push insn & dwarf along with it.  */
      stack_adj = frame->multi_push_adj_base + multi_push_additional;
      insn = emit_insn (riscv_gen_multi_push_pop_insn (
	PUSH_IDX, -stack_adj, riscv_multi_push_regs_count (frame->mask)));
      dwarf = riscv_adjust_multi_push_cfi_prologue (stack_adj);
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = dwarf;

      /* Temporarily fib that we need not save GPRs.  */
      frame->mask = 0;

      /* push FPRs into the addtional reserved space by cm.push. */
      if (fmask)
	{
	  unsigned mask_fprs_push
	    = get_multi_push_fpr_mask (multi_push_additional / UNITS_PER_WORD);
	  frame->fmask &= mask_fprs_push;
	  riscv_for_each_saved_reg (remaining_size, riscv_save_reg, false,
				    false);
	  frame->fmask = fmask & ~mask_fprs_push; /* mask for the rest FPRs.  */
	}
    }
  /* When optimizing for size, call a subroutine to save the registers.  */
  else if (riscv_use_save_libcall (frame))
    {
      rtx dwarf = NULL_RTX;
      dwarf = riscv_adjust_libcall_cfi_prologue ();

      remaining_size -= frame->save_libcall_adjustment;
      insn = emit_insn (riscv_gen_gpr_save_insn (frame));
      frame->mask = 0; /* Temporarily fib that we need not save GPRs.  */

      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = dwarf;
    }

  th_int_mask = th_int_get_mask (frame->mask);
  if (th_int_mask && TH_INT_INTERRUPT (cfun))
    {
      frame->mask &= ~th_int_mask;

      /* RISCV_PROLOGUE_TEMP may be used to handle some CSR for
	 interrupts, such as fcsr.  */
      if ((TARGET_HARD_FLOAT  && frame->fmask)
	  || (TARGET_ZFINX && frame->mask))
	frame->mask |= (1 << RISCV_PROLOGUE_TEMP_REGNUM);

      unsigned save_adjustment = th_int_get_save_adjustment ();
      frame->gp_sp_offset -= save_adjustment;
      remaining_size -= save_adjustment;

      insn = emit_insn (gen_th_int_push ());

      rtx dwarf = th_int_adjust_cfi_prologue (th_int_mask);
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = dwarf;
    }

  /* Save the GP, FP registers.  */
  if ((frame->mask | frame->fmask) != 0)
    {
      if (known_gt (remaining_size, frame->frame_pointer_offset))
	{
	  HOST_WIDE_INT step1 = riscv_first_stack_step (frame, remaining_size);
	  remaining_size -= step1;
	  insn = gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (-step1));
	  RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
	}
      riscv_for_each_saved_reg (remaining_size, riscv_save_reg, false, false);
    }

  /* Undo the above fib.  */
  frame->mask = mask;
  frame->fmask = fmask;

  /* Set up the frame pointer, if we're using one.  */
  if (frame_pointer_needed)
    {
      insn = gen_add3_insn (hard_frame_pointer_rtx, stack_pointer_rtx,
			    GEN_INT ((frame->hard_frame_pointer_offset - remaining_size).to_constant ()));
      RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;

      riscv_emit_stack_tie ();
    }

  /* Save the V registers.  */
  if (frame->vmask != 0)
    riscv_for_each_saved_v_reg (remaining_size, riscv_save_reg, true);

  /* Allocate the rest of the frame.  */
  if (known_gt (remaining_size, 0))
    {
      /* Two step adjustment:
	 1.scalable frame. 2.constant frame.  */
      poly_int64 scalable_frame (0, 0);
      if (!remaining_size.is_constant ())
	{
	  /* First for scalable frame.  */
	  poly_int64 scalable_frame = remaining_size;
	  scalable_frame.coeffs[0] = remaining_size.coeffs[1];
	  riscv_v_adjust_scalable_frame (stack_pointer_rtx, scalable_frame, false);
	  remaining_size -= scalable_frame;
	}

      /* Second step for constant frame.  */
      HOST_WIDE_INT constant_frame = remaining_size.to_constant ();
      if (constant_frame == 0)
	{
	  /* We must have allocated stack space for the scalable frame.
	     Emit a stack tie if we have a frame pointer so that the
	     allocation is ordered WRT fp setup and subsequent writes
	     into the frame.  */
	  if (frame_pointer_needed)
	    riscv_emit_stack_tie ();
	  return;
	}

      if (SMALL_OPERAND (-constant_frame))
	{
	  insn = gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (-constant_frame));
	  RTX_FRAME_RELATED_P (emit_insn (insn)) = 1;
	}
      else
	{
	  riscv_emit_move (RISCV_PROLOGUE_TEMP (Pmode), GEN_INT (-constant_frame));
	  emit_insn (gen_add3_insn (stack_pointer_rtx,
				    stack_pointer_rtx,
				    RISCV_PROLOGUE_TEMP (Pmode)));

	  /* Describe the effect of the previous instructions.  */
	  insn = plus_constant (Pmode, stack_pointer_rtx, -constant_frame);
	  insn = gen_rtx_SET (stack_pointer_rtx, insn);
	  riscv_set_frame_expr (insn);
	}

      /* We must have allocated the remainder of the stack frame.
	 Emit a stack tie if we have a frame pointer so that the
	 allocation is ordered WRT fp setup and subsequent writes
	 into the frame.  */
      if (frame_pointer_needed)
	riscv_emit_stack_tie ();
    }
}

static rtx
riscv_adjust_multi_pop_cfi_epilogue (int saved_size)
{
  rtx dwarf = NULL_RTX;
  rtx adjust_sp_rtx, reg;
  unsigned int mask = cfun->machine->frame.mask;

  if (mask & S10_MASK)
    mask |= S11_MASK;

  /* Debug info for adjust sp.  */
  adjust_sp_rtx
    = gen_rtx_SET (stack_pointer_rtx,
		   plus_constant (Pmode, stack_pointer_rtx, saved_size));
  dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, adjust_sp_rtx, dwarf);

  for (int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (mask, regno - GP_REG_FIRST))
      {
	reg = gen_rtx_REG (Pmode, regno);
	dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
      }

  return dwarf;
}

static rtx
riscv_adjust_libcall_cfi_epilogue ()
{
  rtx dwarf = NULL_RTX;
  rtx adjust_sp_rtx, reg;
  int saved_size = cfun->machine->frame.save_libcall_adjustment;

  /* Debug info for adjust sp.  */
  adjust_sp_rtx =
    gen_rtx_SET (stack_pointer_rtx,
		 gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (saved_size)));
  dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, adjust_sp_rtx,
			  dwarf);

  for (int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	reg = gen_rtx_REG (Pmode, regno);
	dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, dwarf);
      }

  return dwarf;
}

static void
riscv_gen_multi_pop_insn (bool use_multi_pop_normal, unsigned mask,
			  unsigned multipop_size)
{
  rtx insn;
  unsigned regs_count = riscv_multi_push_regs_count (mask);

  if (!use_multi_pop_normal)
    insn = emit_insn (
      riscv_gen_multi_push_pop_insn (POP_IDX, multipop_size, regs_count));
  else
    insn = emit_jump_insn (
      riscv_gen_multi_push_pop_insn (POPRET_IDX, multipop_size, regs_count));

  rtx dwarf = riscv_adjust_multi_pop_cfi_epilogue (multipop_size);
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = dwarf;
}

/* Expand an "epilogue", "sibcall_epilogue", or "eh_return_internal" pattern;
   style says which.  */

void
riscv_expand_epilogue (int style)
{
  /* Split the frame into 3 steps. STEP1 is the amount of stack we should
     deallocate before restoring the registers. STEP2 is the amount we
     should deallocate afterwards including the callee saved regs. STEP3
     is the amount deallocated by save-restore libcall.

     Start off by assuming that no registers need to be restored.  */
  struct riscv_frame_info *frame = &cfun->machine->frame;
  unsigned mask = frame->mask;
  unsigned fmask = frame->fmask;
  unsigned mask_fprs_push = 0;
  poly_int64 step2 = 0;
  bool use_multi_pop_normal
    = ((style == NORMAL_RETURN) && riscv_use_multi_push (frame));
  bool use_multi_pop_sibcall
    = ((style == SIBCALL_RETURN) && riscv_use_multi_push (frame));
  bool use_multi_pop = use_multi_pop_normal || use_multi_pop_sibcall;

  bool use_restore_libcall
    = !use_multi_pop
      && ((style == NORMAL_RETURN) && riscv_use_save_libcall (frame));
  unsigned libcall_size = use_restore_libcall && !use_multi_pop
			    ? frame->save_libcall_adjustment
			    : 0;
  unsigned multipop_size
    = use_multi_pop ? frame->multi_push_adj_base + frame->multi_push_adj_addi
		    : 0;
  rtx ra = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
  unsigned th_int_mask = 0;
  rtx insn;

  /* We need to add memory barrier to prevent read from deallocated stack.  */
  bool need_barrier_p = known_ne (get_frame_size ()
				  + cfun->machine->frame.arg_pointer_offset, 0);

  if (cfun->machine->naked_p)
    {
      gcc_assert (style == NORMAL_RETURN);

      emit_jump_insn (gen_return ());

      return;
    }

  if ((style == NORMAL_RETURN) && riscv_can_use_return_insn ())
    {
      emit_jump_insn (gen_return ());
      return;
    }

  /* Reset the epilogue cfa info before starting to emit the epilogue.  */
  epilogue_cfa_sp_offset = 0;

  /* Move past any dynamic stack allocations.  */
  if (cfun->calls_alloca)
    {
      /* Emit a barrier to prevent loads from a deallocated stack.  */
      riscv_emit_stack_tie ();
      need_barrier_p = false;

      poly_int64 adjust_offset = -frame->hard_frame_pointer_offset;
      rtx adjust = NULL_RTX;

      if (!adjust_offset.is_constant ())
	{
	  rtx tmp1 = RISCV_PROLOGUE_TEMP (Pmode);
	  rtx tmp2 = RISCV_PROLOGUE_TEMP2 (Pmode);
	  riscv_legitimize_poly_move (Pmode, tmp1, tmp2,
				      gen_int_mode (adjust_offset, Pmode));
	  adjust = tmp1;
	}
      else
	{
	  if (!SMALL_OPERAND (adjust_offset.to_constant ()))
	    {
	      riscv_emit_move (RISCV_PROLOGUE_TEMP (Pmode),
			       GEN_INT (adjust_offset.to_constant ()));
	      adjust = RISCV_PROLOGUE_TEMP (Pmode);
	    }
	  else
	    adjust = GEN_INT (adjust_offset.to_constant ());
	}

      insn = emit_insn (
	       gen_add3_insn (stack_pointer_rtx, hard_frame_pointer_rtx,
			      adjust));

      rtx dwarf = NULL_RTX;
      rtx cfa_adjust_value = gen_rtx_PLUS (
			       Pmode, hard_frame_pointer_rtx,
			       gen_int_mode (-frame->hard_frame_pointer_offset, Pmode));
      rtx cfa_adjust_rtx = gen_rtx_SET (stack_pointer_rtx, cfa_adjust_value);
      dwarf = alloc_reg_note (REG_CFA_ADJUST_CFA, cfa_adjust_rtx, dwarf);
      RTX_FRAME_RELATED_P (insn) = 1;

      REG_NOTES (insn) = dwarf;
    }

  if (use_restore_libcall || use_multi_pop)
    frame->mask = 0; /* Temporarily fib that we need not restore GPRs.  */

  /* If we need to restore registers, deallocate as much stack as
     possible in the second step without going out of range.  */
  if (use_multi_pop)
    {
      if (frame->fmask
	  && known_gt (frame->total_size - multipop_size,
		       frame->frame_pointer_offset))
	step2
	  = riscv_first_stack_step (frame, frame->total_size - multipop_size);
    }
  else if ((frame->mask | frame->fmask) != 0)
    step2 = riscv_first_stack_step (frame, frame->total_size - libcall_size);

  if (use_restore_libcall || use_multi_pop)
    frame->mask = mask; /* Undo the above fib.  */

  poly_int64 step1;
  /* STEP1 must be set to the bottom of vector registers save area if any
     vector registers need be preversed.  */
  if (frame->vmask != 0)
    {
      step1 = frame->v_sp_offset_bottom;
      step2 = frame->total_size - step1 - libcall_size - multipop_size;
    }
  else
    step1 = frame->total_size - step2 - libcall_size - multipop_size;

  /* Set TARGET to BASE + STEP1.  */
  if (known_gt (step1, 0))
    {
      /* Emit a barrier to prevent loads from a deallocated stack.  */
      riscv_emit_stack_tie ();
      need_barrier_p = false;

      /* Restore the scalable frame which is assigned in prologue.  */
      if (!step1.is_constant ())
	{
	  poly_int64 scalable_frame = step1;
	  scalable_frame.coeffs[0] = step1.coeffs[1];
	  riscv_v_adjust_scalable_frame (stack_pointer_rtx, scalable_frame,
					 true);
	  step1 -= scalable_frame;
	}

      /* Get an rtx for STEP1 that we can add to BASE.
	 Skip if adjust equal to zero.  */
      if (step1.to_constant () != 0)
	{
	  rtx adjust = GEN_INT (step1.to_constant ());
	  if (!SMALL_OPERAND (step1.to_constant ()))
	    {
	      riscv_emit_move (RISCV_PROLOGUE_TEMP (Pmode), adjust);
	      adjust = RISCV_PROLOGUE_TEMP (Pmode);
	    }

	  insn = emit_insn (gen_add3_insn (stack_pointer_rtx,
					   stack_pointer_rtx,
					   adjust));
	  rtx dwarf = NULL_RTX;
	  rtx cfa_adjust_rtx
	    = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			    gen_int_mode (step2 + libcall_size + multipop_size,
					  Pmode));

	  dwarf = alloc_reg_note (REG_CFA_DEF_CFA, cfa_adjust_rtx, dwarf);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  REG_NOTES (insn) = dwarf;
	}
    }
  else if (frame_pointer_needed)
    {
      /* Tell riscv_restore_reg to emit dwarf to redefine CFA when restoring
	 old value of FP.  */
      epilogue_cfa_sp_offset = step2;
    }

  if (use_multi_pop)
    {
      frame->mask = 0; /* Temporarily fib that we need not restore GPRs.  */
      if (fmask)
	{
	  mask_fprs_push = get_multi_push_fpr_mask (frame->multi_push_adj_addi
						    / UNITS_PER_WORD);
	  frame->fmask &= ~mask_fprs_push; /* FPRs not saved by cm.push  */
	}
    }
  else if (use_restore_libcall)
    frame->mask = 0; /* Temporarily fib that we need not restore GPRs.  */

  th_int_mask = th_int_get_mask (frame->mask);
  if (th_int_mask && TH_INT_INTERRUPT (cfun))
    {
      frame->mask &= ~th_int_mask;

      /* RISCV_PROLOGUE_TEMP may be used to handle some CSR for
	 interrupts, such as fcsr.  */
      if ((TARGET_HARD_FLOAT  && frame->fmask)
	  || (TARGET_ZFINX && frame->mask))
	frame->mask |= (1 << RISCV_PROLOGUE_TEMP_REGNUM);
    }

  /* Restore the registers.  */
  riscv_for_each_saved_v_reg (step2, riscv_restore_reg, false);
  riscv_for_each_saved_reg (frame->total_size - step2 - libcall_size
			      - multipop_size,
			    riscv_restore_reg, true, style == EXCEPTION_RETURN);

  if (th_int_mask && TH_INT_INTERRUPT (cfun))
    {
      frame->mask = mask; /* Undo the above fib.  */
      unsigned save_adjustment = th_int_get_save_adjustment ();
      gcc_assert (step2.to_constant () >= save_adjustment);
      step2 -= save_adjustment;
    }

  if (use_restore_libcall)
    frame->mask = mask; /* Undo the above fib.  */

  if (need_barrier_p)
    riscv_emit_stack_tie ();

  /* Deallocate the final bit of the frame.  */
  if (step2.to_constant () > 0)
    {
      insn = emit_insn (gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
				       GEN_INT (step2.to_constant ())));

      rtx dwarf = NULL_RTX;
      rtx cfa_adjust_rtx
	= gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			GEN_INT (libcall_size + multipop_size));
      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, cfa_adjust_rtx, dwarf);
      RTX_FRAME_RELATED_P (insn) = 1;

      REG_NOTES (insn) = dwarf;
    }

  if (use_multi_pop)
    {
      /* restore FPRs pushed by cm.push. */
      frame->fmask = fmask & mask_fprs_push;
      if (frame->fmask)
	riscv_for_each_saved_reg (frame->total_size - libcall_size
				    - multipop_size,
				  riscv_restore_reg, true,
				  style == EXCEPTION_RETURN);
      /* Undo the above fib.  */
      frame->mask = mask;
      frame->fmask = fmask;
      riscv_gen_multi_pop_insn (use_multi_pop_normal, frame->mask,
				multipop_size);
      if (use_multi_pop_normal)
	return;
    }
  else if (use_restore_libcall)
    {
      rtx dwarf = riscv_adjust_libcall_cfi_epilogue ();
      insn = emit_insn (gen_gpr_restore (GEN_INT (riscv_save_libcall_count (mask))));
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn) = dwarf;

      emit_jump_insn (gen_gpr_restore_return (ra));
      return;
    }

  /* Add in the __builtin_eh_return stack adjustment. */
  if ((style == EXCEPTION_RETURN) && crtl->calls_eh_return)
    emit_insn (gen_add3_insn (stack_pointer_rtx, stack_pointer_rtx,
			      EH_RETURN_STACKADJ_RTX));

  /* Return from interrupt.  */
  if (cfun->machine->interrupt_handler_p)
    {
      enum riscv_privilege_levels mode = cfun->machine->interrupt_mode;

      gcc_assert (mode != UNKNOWN_MODE);

      if (th_int_mask && TH_INT_INTERRUPT (cfun))
	emit_jump_insn (gen_th_int_pop ());
      else if (mode == MACHINE_MODE)
	emit_jump_insn (gen_riscv_mret ());
      else if (mode == SUPERVISOR_MODE)
	emit_jump_insn (gen_riscv_sret ());
      else
	emit_jump_insn (gen_riscv_uret ());
    }
  else if (style != SIBCALL_RETURN)
    emit_jump_insn (gen_simple_return_internal (ra));
}

/* Implement EPILOGUE_USES.  */

bool
riscv_epilogue_uses (unsigned int regno)
{
  if (regno == RETURN_ADDR_REGNUM)
    return true;

  if (epilogue_completed && cfun->machine->interrupt_handler_p)
    {
      /* An interrupt function restores temp regs, so we must indicate that
	 they are live at function end.  */
      if (df_regs_ever_live_p (regno)
	    || (!crtl->is_leaf && call_used_or_fixed_reg_p (regno)))
	return true;
    }

  return false;
}

static bool
riscv_avoid_shrink_wrapping_separate ()
{
  if (riscv_use_save_libcall (&cfun->machine->frame)
      || cfun->machine->interrupt_handler_p
      || !cfun->machine->frame.gp_sp_offset.is_constant ())
    return true;

  return false;
}

/* Implement TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS.  */

static sbitmap
riscv_get_separate_components (void)
{
  HOST_WIDE_INT offset;
  sbitmap components = sbitmap_alloc (FIRST_PSEUDO_REGISTER);
  bitmap_clear (components);

  if (riscv_avoid_shrink_wrapping_separate ())
    return components;

  offset = cfun->machine->frame.gp_sp_offset.to_constant ();
  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	/* We can only wrap registers that have small operand offsets.
	   For large offsets a pseudo register might be needed which
	   cannot be created during the shrink wrapping pass.  */
	if (SMALL_OPERAND (offset))
	  bitmap_set_bit (components, regno);

	offset -= UNITS_PER_WORD;
      }

  offset = cfun->machine->frame.fp_sp_offset.to_constant ();
  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	machine_mode mode = TARGET_DOUBLE_FLOAT ? DFmode : SFmode;

	/* We can only wrap registers that have small operand offsets.
	   For large offsets a pseudo register might be needed which
	   cannot be created during the shrink wrapping pass.  */
	if (SMALL_OPERAND (offset))
	  bitmap_set_bit (components, regno);

	offset -= GET_MODE_SIZE (mode).to_constant ();
      }

  /* Don't mess with the hard frame pointer.  */
  if (frame_pointer_needed)
    bitmap_clear_bit (components, HARD_FRAME_POINTER_REGNUM);

  bitmap_clear_bit (components, RETURN_ADDR_REGNUM);

  return components;
}

/* Implement TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB.  */

static sbitmap
riscv_components_for_bb (basic_block bb)
{
  bitmap in = DF_LIVE_IN (bb);
  bitmap gen = &DF_LIVE_BB_INFO (bb)->gen;
  bitmap kill = &DF_LIVE_BB_INFO (bb)->kill;

  sbitmap components = sbitmap_alloc (FIRST_PSEUDO_REGISTER);
  bitmap_clear (components);

  function_abi_aggregator callee_abis;
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    if (CALL_P (insn))
      callee_abis.note_callee_abi (insn_callee_abi (insn));
  HARD_REG_SET extra_caller_saves = callee_abis.caller_save_regs (*crtl->abi);

  /* GPRs are used in a bb if they are in the IN, GEN, or KILL sets.  */
  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (!fixed_regs[regno]
       && !crtl->abi->clobbers_full_reg_p (regno)
       && (TEST_HARD_REG_BIT (extra_caller_saves, regno)
	   || bitmap_bit_p (in, regno)
	   || bitmap_bit_p (gen, regno)
	   || bitmap_bit_p (kill, regno)))
      bitmap_set_bit (components, regno);

  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (!fixed_regs[regno]
       && !crtl->abi->clobbers_full_reg_p (regno)
       && (TEST_HARD_REG_BIT (extra_caller_saves, regno)
	   || bitmap_bit_p (in, regno)
	   || bitmap_bit_p (gen, regno)
	   || bitmap_bit_p (kill, regno)))
      bitmap_set_bit (components, regno);

  return components;
}

/* Implement TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS.  */

static void
riscv_disqualify_components (sbitmap, edge, sbitmap, bool)
{
  /* Nothing to do for riscv.  */
}

static void
riscv_process_components (sbitmap components, bool prologue_p)
{
  HOST_WIDE_INT offset;
  riscv_save_restore_fn fn = prologue_p? riscv_save_reg : riscv_restore_reg;

  offset = cfun->machine->frame.gp_sp_offset.to_constant ();
  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.mask, regno - GP_REG_FIRST))
      {
	if (bitmap_bit_p (components, regno))
	  riscv_save_restore_reg (word_mode, regno, offset, fn);

	offset -= UNITS_PER_WORD;
      }

  offset = cfun->machine->frame.fp_sp_offset.to_constant ();
  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (BITSET_P (cfun->machine->frame.fmask, regno - FP_REG_FIRST))
      {
	machine_mode mode = TARGET_DOUBLE_FLOAT ? DFmode : SFmode;

	if (bitmap_bit_p (components, regno))
	  riscv_save_restore_reg (mode, regno, offset, fn);

	offset -= GET_MODE_SIZE (mode).to_constant ();
      }
}

/* Implement TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS.  */

static void
riscv_emit_prologue_components (sbitmap components)
{
  riscv_process_components (components, true);
}

/* Implement TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS.  */

static void
riscv_emit_epilogue_components (sbitmap components)
{
  riscv_process_components (components, false);
}

static void
riscv_set_handled_components (sbitmap components)
{
  for (unsigned int regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (bitmap_bit_p (components, regno))
      cfun->machine->reg_is_wrapped_separately[regno] = true;

  for (unsigned int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
    if (bitmap_bit_p (components, regno))
      cfun->machine->reg_is_wrapped_separately[regno] = true;
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

bool
riscv_can_use_return_insn (void)
{
  return (reload_completed && known_eq (cfun->machine->frame.total_size, 0)
	  && ! cfun->machine->interrupt_handler_p);
}

/* Given that there exists at least one variable that is set (produced)
   by OUT_INSN and read (consumed) by IN_INSN, return true iff
   IN_INSN represents one or more memory store operations and none of
   the variables set by OUT_INSN is used by IN_INSN as the address of a
   store operation.  If either IN_INSN or OUT_INSN does not represent
   a "single" RTL SET expression (as loosely defined by the
   implementation of the single_set function) or a PARALLEL with only
   SETs, CLOBBERs, and USEs inside, this function returns false.

   Borrowed from rs6000, riscv_store_data_bypass_p checks for certain
   conditions that result in assertion failures in the generic
   store_data_bypass_p function and returns FALSE in such cases.

   This is required to make -msave-restore work with the sifive-7
   pipeline description.  */

bool
riscv_store_data_bypass_p (rtx_insn *out_insn, rtx_insn *in_insn)
{
  rtx out_set, in_set;
  rtx out_pat, in_pat;
  rtx out_exp, in_exp;
  int i, j;

  in_set = single_set (in_insn);
  if (in_set)
    {
      if (MEM_P (SET_DEST (in_set)))
	{
	  out_set = single_set (out_insn);
	  if (!out_set)
	    {
	      out_pat = PATTERN (out_insn);
	      if (GET_CODE (out_pat) == PARALLEL)
		{
		  for (i = 0; i < XVECLEN (out_pat, 0); i++)
		    {
		      out_exp = XVECEXP (out_pat, 0, i);
		      if ((GET_CODE (out_exp) == CLOBBER)
			  || (GET_CODE (out_exp) == USE))
			continue;
		      else if (GET_CODE (out_exp) != SET)
			return false;
		    }
		}
	    }
	}
    }
  else
    {
      in_pat = PATTERN (in_insn);
      if (GET_CODE (in_pat) != PARALLEL)
	return false;

      for (i = 0; i < XVECLEN (in_pat, 0); i++)
	{
	  in_exp = XVECEXP (in_pat, 0, i);
	  if ((GET_CODE (in_exp) == CLOBBER) || (GET_CODE (in_exp) == USE))
	    continue;
	  else if (GET_CODE (in_exp) != SET)
	    return false;

	  if (MEM_P (SET_DEST (in_exp)))
	    {
	      out_set = single_set (out_insn);
	      if (!out_set)
		{
		  out_pat = PATTERN (out_insn);
		  if (GET_CODE (out_pat) != PARALLEL)
		    return false;
		  for (j = 0; j < XVECLEN (out_pat, 0); j++)
		    {
		      out_exp = XVECEXP (out_pat, 0, j);
		      if ((GET_CODE (out_exp) == CLOBBER)
			  || (GET_CODE (out_exp) == USE))
			continue;
		      else if (GET_CODE (out_exp) != SET)
			return false;
		    }
		}
	    }
	}
    }

  return store_data_bypass_p (out_insn, in_insn);
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED.

   When floating-point registers are wider than integer ones, moves between
   them must go through memory.  */

static bool
riscv_secondary_memory_needed (machine_mode mode, reg_class_t class1,
			       reg_class_t class2)
{
  return (!riscv_v_ext_mode_p (mode)
	  && GET_MODE_SIZE (mode).to_constant () > UNITS_PER_WORD
	  && (class1 == FP_REGS) != (class2 == FP_REGS)
	  && !TARGET_XTHEADFMV
	  && !TARGET_ZFA);
}

/* Implement TARGET_REGISTER_MOVE_COST.  */

static int
riscv_register_move_cost (machine_mode mode,
			  reg_class_t from, reg_class_t to)
{
  if ((from == FP_REGS && to == GR_REGS) ||
      (from == GR_REGS && to == FP_REGS))
    return tune_param->fmv_cost;

  return riscv_secondary_memory_needed (mode, from, to) ? 8 : 2;
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
riscv_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  if (riscv_v_ext_vector_mode_p (mode))
    {
      /* Handle fractional LMUL, it only occupy part of vector register but
	 still need one vector register to hold.  */
      if (maybe_lt (GET_MODE_SIZE (mode), UNITS_PER_V_REG))
	return 1;

      return exact_div (GET_MODE_SIZE (mode), UNITS_PER_V_REG).to_constant ();
    }

  /* For tuple modes, the number of register = NF * LMUL.  */
  if (riscv_v_ext_tuple_mode_p (mode))
    {
      unsigned int nf = riscv_vector::get_nf (mode);
      machine_mode subpart_mode = riscv_vector::get_subpart_mode (mode);
      poly_int64 size = GET_MODE_SIZE (subpart_mode);
      gcc_assert (known_eq (size * nf, GET_MODE_SIZE (mode)));
      if (maybe_lt (size, UNITS_PER_V_REG))
	return nf;
      else
	{
	  unsigned int lmul = exact_div (size, UNITS_PER_V_REG).to_constant ();
	  return nf * lmul;
	}
    }

  /* For VLS modes, we allocate registers according to TARGET_MIN_VLEN.  */
  if (riscv_v_ext_vls_mode_p (mode))
    {
      int size = GET_MODE_BITSIZE (mode).to_constant ();
      if (size < TARGET_MIN_VLEN)
	return 1;
      else
	return size / TARGET_MIN_VLEN;
    }

  /* mode for VL or VTYPE are just a marker, not holding value,
     so it always consume one register.  */
  if (VTYPE_REG_P (regno) || VL_REG_P (regno) || VXRM_REG_P (regno)
      || FRM_REG_P (regno))
    return 1;

  /* Assume every valid non-vector mode fits in one vector register.  */
  if (V_REG_P (regno))
    return 1;

  if (FP_REG_P (regno))
    return (GET_MODE_SIZE (mode).to_constant () + UNITS_PER_FP_REG - 1) / UNITS_PER_FP_REG;

  /* All other registers are word-sized.  */
  return (GET_MODE_SIZE (mode).to_constant () + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
riscv_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  unsigned int nregs = riscv_hard_regno_nregs (regno, mode);

  if (GP_REG_P (regno))
    {
      if (riscv_v_ext_mode_p (mode))
	return false;

      if (!GP_REG_P (regno + nregs - 1))
	return false;
    }
  else if (FP_REG_P (regno))
    {
      if (riscv_v_ext_mode_p (mode))
	return false;

      if (!FP_REG_P (regno + nregs - 1))
	return false;

      if (GET_MODE_CLASS (mode) != MODE_FLOAT
	  && GET_MODE_CLASS (mode) != MODE_COMPLEX_FLOAT)
	return false;

      /* Only use callee-saved registers if a potential callee is guaranteed
	 to spill the requisite width.  */
      if (GET_MODE_UNIT_SIZE (mode) > UNITS_PER_FP_REG
	  || (!call_used_or_fixed_reg_p (regno)
	      && GET_MODE_UNIT_SIZE (mode) > UNITS_PER_FP_ARG))
	return false;
    }
  else if (V_REG_P (regno))
    {
      if (!riscv_v_ext_mode_p (mode))
	return false;

      if (!V_REG_P (regno + nregs - 1))
	return false;

      int regno_alignment = riscv_get_v_regno_alignment (mode);
      if (regno_alignment != 1)
	return ((regno % regno_alignment) == 0);
    }
  else if (VTYPE_REG_P (regno) || VL_REG_P (regno) || VXRM_REG_P (regno)
	   || FRM_REG_P (regno))
    return true;
  else
    return false;

  /* Require same callee-savedness for all registers.  */
  for (unsigned i = 1; i < nregs; i++)
    if (call_used_or_fixed_reg_p (regno)
	!= call_used_or_fixed_reg_p (regno + i))
      return false;

  /* Only use even registers in RV32 ZDINX */
  if (!TARGET_64BIT && TARGET_ZDINX){
    if (GET_MODE_CLASS (mode) == MODE_FLOAT &&
     GET_MODE_UNIT_SIZE (mode) == GET_MODE_SIZE (DFmode))
    return !(regno & 1);
  }

  return true;
}

/* Implement TARGET_MODES_TIEABLE_P.

   Don't allow floating-point modes to be tied, since type punning of
   single-precision and double-precision is implementation defined.  */

static bool
riscv_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  /* We don't allow different REG_CLASS modes tieable since it
     will cause ICE in register allocation (RA).
     E.g. V2SI and DI are not tieable.  */
  if (riscv_v_ext_mode_p (mode1) != riscv_v_ext_mode_p (mode2))
    return false;
  return (mode1 == mode2
	  || !(GET_MODE_CLASS (mode1) == MODE_FLOAT
	       && GET_MODE_CLASS (mode2) == MODE_FLOAT));
}

/* Implement CLASS_MAX_NREGS.  */

static unsigned char
riscv_class_max_nregs (reg_class_t rclass, machine_mode mode)
{
  if (reg_class_subset_p (rclass, FP_REGS))
    return riscv_hard_regno_nregs (FP_REG_FIRST, mode);

  if (reg_class_subset_p (rclass, GR_REGS))
    return riscv_hard_regno_nregs (GP_REG_FIRST, mode);

  if (reg_class_subset_p (rclass, V_REGS))
    return riscv_hard_regno_nregs (V_REG_FIRST, mode);

  return 0;
}

/* Implement TARGET_MEMORY_MOVE_COST.  */

static int
riscv_memory_move_cost (machine_mode mode, reg_class_t rclass, bool in)
{
  return (tune_param->memory_cost
	  + memory_move_secondary_cost (mode, rclass, in));
}

/* Return the number of instructions that can be issued per cycle.  */

static int
riscv_issue_rate (void)
{
  return tune_param->issue_rate;
}

/* Implement TARGET_SCHED_VARIABLE_ISSUE.  */
static int
riscv_sched_variable_issue (FILE *, int, rtx_insn *insn, int more)
{
  if (DEBUG_INSN_P (insn))
    return more;

  rtx_code code = GET_CODE (PATTERN (insn));
  if (code == USE || code == CLOBBER)
    return more;

  /* GHOST insns are used for blockage and similar cases which
     effectively end a cycle.  */
  if (get_attr_type (insn) == TYPE_GHOST)
    return 0;

  /* If we ever encounter an insn with an unknown type, trip
     an assert so we can find and fix this problem.  */
  gcc_assert (get_attr_type (insn) != TYPE_UNKNOWN);

  /* If we ever encounter an insn without an insn reservation, trip
     an assert so we can find and fix this problem.  */
  gcc_assert (insn_has_dfa_reservation_p (insn));

  return more - 1;
}

/* Implement TARGET_SCHED_MACRO_FUSION_P.  Return true if target supports
   instruction fusion of some sort.  */

static bool
riscv_macro_fusion_p (void)
{
  return tune_param->fusible_ops != RISCV_FUSE_NOTHING;
}

/* Return true iff the instruction fusion described by OP is enabled.  */

static bool
riscv_fusion_enabled_p(enum riscv_fusion_pairs op)
{
  return tune_param->fusible_ops & op;
}

/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

static bool
riscv_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  /* prev and curr are simple SET insns i.e. no flag setting or branching.  */
  bool simple_sets_p = prev_set && curr_set && !any_condjump_p (curr);

  if (!riscv_macro_fusion_p ())
    return false;

  if (simple_sets_p
      && (riscv_fusion_enabled_p (RISCV_FUSE_ZEXTW)
	  || riscv_fusion_enabled_p (RISCV_FUSE_ZEXTWS)))
    {
      /* We are trying to match the following:
	   prev (slli) == (set (reg:DI rD)
			       (ashift:DI (reg:DI rS) (const_int 32)))
	   curr (slri) == (set (reg:DI rD)
			       (lshiftrt:DI (reg:DI rD) (const_int <shift>)))
	 with <shift> being either 32 for FUSE_ZEXTW, or
			 `less than 32 for FUSE_ZEXTWS. */

      if (GET_CODE (SET_SRC (prev_set)) == ASHIFT
	  && GET_CODE (SET_SRC (curr_set)) == LSHIFTRT
	  && REG_P (SET_DEST (prev_set))
	  && REG_P (SET_DEST (curr_set))
	  && REGNO (SET_DEST (prev_set)) == REGNO (SET_DEST (curr_set))
	  && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO(SET_DEST (curr_set))
	  && CONST_INT_P (XEXP (SET_SRC (prev_set), 1))
	  && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
	  && INTVAL (XEXP (SET_SRC (prev_set), 1)) == 32
	  && (( INTVAL (XEXP (SET_SRC (curr_set), 1)) == 32
		&& riscv_fusion_enabled_p(RISCV_FUSE_ZEXTW) )
	      || ( INTVAL (XEXP (SET_SRC (curr_set), 1)) < 32
		   && riscv_fusion_enabled_p(RISCV_FUSE_ZEXTWS))))
	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_ZEXTH))
    {
      /* We are trying to match the following:
	   prev (slli) == (set (reg:DI rD)
			       (ashift:DI (reg:DI rS) (const_int 48)))
	   curr (slri) == (set (reg:DI rD)
			       (lshiftrt:DI (reg:DI rD) (const_int 48))) */

      if (GET_CODE (SET_SRC (prev_set)) == ASHIFT
	  && GET_CODE (SET_SRC (curr_set)) == LSHIFTRT
	  && REG_P (SET_DEST (prev_set))
	  && REG_P (SET_DEST (curr_set))
	  && REGNO (SET_DEST (prev_set)) == REGNO (SET_DEST (curr_set))
	  && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO(SET_DEST (curr_set))
	  && CONST_INT_P (XEXP (SET_SRC (prev_set), 1))
	  && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
	  && INTVAL (XEXP (SET_SRC (prev_set), 1)) == 48
	  && INTVAL (XEXP (SET_SRC (curr_set), 1)) == 48)
	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_LDINDEXED))
    {
      /* We are trying to match the following:
	   prev (add) == (set (reg:DI rD)
			      (plus:DI (reg:DI rS1) (reg:DI rS2))
	   curr (ld)  == (set (reg:DI rD)
			      (mem:DI (reg:DI rD))) */

      if (MEM_P (SET_SRC (curr_set))
	  && REG_P (XEXP (SET_SRC (curr_set), 0))
	  && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
	  && GET_CODE (SET_SRC (prev_set)) == PLUS
	  && REG_P (XEXP (SET_SRC (prev_set), 0))
	  && REG_P (XEXP (SET_SRC (prev_set), 1)))
	return true;

      /* We are trying to match the following:
	   prev (add) == (set (reg:DI rD)
			      (plus:DI (reg:DI rS1) (reg:DI rS2)))
	   curr (lw)  == (set (any_extend:DI (mem:SUBX (reg:DI rD)))) */

      if ((GET_CODE (SET_SRC (curr_set)) == SIGN_EXTEND
	   || (GET_CODE (SET_SRC (curr_set)) == ZERO_EXTEND))
	  && MEM_P (XEXP (SET_SRC (curr_set), 0))
	  && REG_P (XEXP (XEXP (SET_SRC (curr_set), 0), 0))
	  && REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0)) == REGNO (SET_DEST (prev_set))
	  && GET_CODE (SET_SRC (prev_set)) == PLUS
	  && REG_P (XEXP (SET_SRC (prev_set), 0))
	  && REG_P (XEXP (SET_SRC (prev_set), 1)))
	return true;
    }

    if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_LDPREINCREMENT))
    {
      /* We are trying to match the following:
	   prev (add) == (set (reg:DI rS)
			      (plus:DI (reg:DI rS) (const_int))
	   curr (ld)  == (set (reg:DI rD)
			      (mem:DI (reg:DI rS))) */

      if (MEM_P (SET_SRC (curr_set))
	  && REG_P (XEXP (SET_SRC (curr_set), 0))
	  && REGNO (XEXP (SET_SRC (curr_set), 0)) == REGNO (SET_DEST (prev_set))
	  && GET_CODE (SET_SRC (prev_set)) == PLUS
	  && REG_P (XEXP (SET_SRC (prev_set), 0))
	  && CONST_INT_P (XEXP (SET_SRC (prev_set), 1)))
	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_LUI_ADDI))
    {
      /* We are trying to match the following:
	   prev (lui)  == (set (reg:DI rD) (const_int UPPER_IMM_20))
	   curr (addi) == (set (reg:DI rD)
			       (plus:DI (reg:DI rD) (const_int IMM12))) */

      if ((GET_CODE (SET_SRC (curr_set)) == LO_SUM
	   || (GET_CODE (SET_SRC (curr_set)) == PLUS
	       && CONST_INT_P (XEXP (SET_SRC (curr_set), 1))
	       && SMALL_OPERAND (INTVAL (XEXP (SET_SRC (curr_set), 1)))))
	  && (GET_CODE (SET_SRC (prev_set)) == HIGH
	      || (CONST_INT_P (SET_SRC (prev_set))
		  && LUI_OPERAND (INTVAL (SET_SRC (prev_set))))))
	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_AUIPC_ADDI))
    {
      /* We are trying to match the following:
	   prev (auipc) == (set (reg:DI rD) (unspec:DI [...] UNSPEC_AUIPC))
	   curr (addi)  == (set (reg:DI rD)
				(plus:DI (reg:DI rD) (const_int IMM12)))
	 and
	   prev (auipc) == (set (reg:DI rD) (unspec:DI [...] UNSPEC_AUIPC))
	   curr (addi)  == (set (reg:DI rD)
				(lo_sum:DI (reg:DI rD) (const_int IMM12))) */

      if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
	  && XINT (SET_SRC (prev_set), 1) == UNSPEC_AUIPC
	  && (GET_CODE (SET_SRC (curr_set)) == LO_SUM
	      || (GET_CODE (SET_SRC (curr_set)) == PLUS
		  && SMALL_OPERAND (INTVAL (XEXP (SET_SRC (curr_set), 1))))))

	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_LUI_LD))
    {
      /* We are trying to match the following:
	   prev (lui)  == (set (reg:DI rD) (const_int UPPER_IMM_20))
	   curr (ld)  == (set (reg:DI rD)
			      (mem:DI (plus:DI (reg:DI rD) (const_int IMM12)))) */

      if (CONST_INT_P (SET_SRC (prev_set))
	  && LUI_OPERAND (INTVAL (SET_SRC (prev_set)))
	  && MEM_P (SET_SRC (curr_set))
	  && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS)
	return true;

      if (GET_CODE (SET_SRC (prev_set)) == HIGH
	  && MEM_P (SET_SRC (curr_set))
	  && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == LO_SUM
	  && REGNO (SET_DEST (prev_set)) == REGNO (XEXP (XEXP (SET_SRC (curr_set), 0), 0)))
	return true;

      if (GET_CODE (SET_SRC (prev_set)) == HIGH
	  && (GET_CODE (SET_SRC (curr_set)) == SIGN_EXTEND
	      || GET_CODE (SET_SRC (curr_set)) == ZERO_EXTEND)
	  && MEM_P (XEXP (SET_SRC (curr_set), 0))
	  && (GET_CODE (XEXP (XEXP (SET_SRC (curr_set), 0), 0)) == LO_SUM
	      && REGNO (SET_DEST (prev_set)) == REGNO (XEXP (XEXP (XEXP (SET_SRC (curr_set), 0), 0), 0))))
	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_AUIPC_LD))
    {
      /* We are trying to match the following:
	   prev (auipc) == (set (reg:DI rD) (unspec:DI [...] UNSPEC_AUIPC))
	   curr (ld)  == (set (reg:DI rD)
			      (mem:DI (plus:DI (reg:DI rD) (const_int IMM12)))) */

      if (GET_CODE (SET_SRC (prev_set)) == UNSPEC
	  && XINT (prev_set, 1) == UNSPEC_AUIPC
	  && MEM_P (SET_SRC (curr_set))
	  && GET_CODE (XEXP (SET_SRC (curr_set), 0)) == PLUS)
	return true;
    }

  if (simple_sets_p && riscv_fusion_enabled_p (RISCV_FUSE_ALIGNED_STD))
    {
      /* We are trying to match the following:
	   prev (sd) == (set (mem (plus (reg sp|fp) (const_int)))
			      (reg rS1))
	   curr (sd) == (set (mem (plus (reg sp|fp) (const_int)))
			      (reg rS2)) */

      if (MEM_P (SET_DEST (prev_set))
	  && MEM_P (SET_DEST (curr_set))
	  /* We can probably relax this condition.  The documentation is a bit
	     unclear about sub-word cases.  So we just model DImode for now.  */
	  && GET_MODE (SET_DEST (curr_set)) == DImode
	  && GET_MODE (SET_DEST (prev_set)) == DImode)
	{
	  rtx base_prev, base_curr, offset_prev, offset_curr;

	  extract_base_offset_in_addr (SET_DEST (prev_set), &base_prev, &offset_prev);
	  extract_base_offset_in_addr (SET_DEST (curr_set), &base_curr, &offset_curr);

	  /* The two stores must be contained within opposite halves of the same
	     16 byte aligned block of memory.  We know that the stack pointer and
	     the frame pointer have suitable alignment.  So we just need to check
	     the offsets of the two stores for suitable alignment.

	     Originally the thought was to check MEM_ALIGN, but that was reporting
	     incorrect alignments, even for SP/FP accesses, so we gave up on that
	     approach.  */
	  if (base_prev != NULL_RTX
	      && base_curr != NULL_RTX
	      && REG_P (base_prev)
	      && REG_P (base_curr)
	      && REGNO (base_prev) == REGNO (base_curr)
	      && (REGNO (base_prev) == STACK_POINTER_REGNUM
		  || REGNO (base_prev) == HARD_FRAME_POINTER_REGNUM)
	      && ((INTVAL (offset_prev) == INTVAL (offset_curr) + 8
		   && (INTVAL (offset_prev) % 16) == 0)
		  || ((INTVAL (offset_curr) == INTVAL (offset_prev) + 8)
		      && (INTVAL (offset_curr) % 16) == 0)))
	    return true;
	}
    }

  return false;
}

/* Adjust the cost/latency of instructions for scheduling.
   For now this is just used to change the latency of vector instructions
   according to their LMUL.  We assume that an insn with LMUL == 8 requires
   eight times more execution cycles than the same insn with LMUL == 1.
   As this may cause very high latencies which lead to scheduling artifacts
   we currently only perform the adjustment when -madjust-lmul-cost is given.
   */
static int
riscv_sched_adjust_cost (rtx_insn *, int, rtx_insn *insn, int cost,
			 unsigned int)
{
  /* Only do adjustments for the generic out-of-order scheduling model.  */
  if (!TARGET_VECTOR || riscv_microarchitecture != generic_ooo)
    return cost;

  if (recog_memoized (insn) < 0)
    return cost;

  enum attr_type type = get_attr_type (insn);

  if (type == TYPE_VFREDO || type == TYPE_VFWREDO)
    {
      /* TODO: For ordered reductions scale the base cost relative to the
	 number of units.  */
      ;
    }

  /* Don't do any LMUL-based latency adjustment unless explicitly asked to.  */
  if (!TARGET_ADJUST_LMUL_COST)
    return cost;

  /* vsetvl has a vlmul attribute but its latency does not depend on it.  */
  if (type == TYPE_VSETVL || type == TYPE_VSETVL_PRE)
    return cost;

  enum riscv_vector::vlmul_type lmul =
    (riscv_vector::vlmul_type)get_attr_vlmul (insn);

  double factor = 1;
  switch (lmul)
    {
    case riscv_vector::LMUL_2:
      factor = 2;
      break;
    case riscv_vector::LMUL_4:
      factor = 4;
      break;
    case riscv_vector::LMUL_8:
      factor = 8;
      break;
    case riscv_vector::LMUL_F2:
      factor = 0.5;
      break;
    case riscv_vector::LMUL_F4:
      factor = 0.25;
      break;
    case riscv_vector::LMUL_F8:
      factor = 0.125;
      break;
    default:
      factor = 1;
    }

  /* If the latency was nonzero, keep it that way.  */
  int new_cost = MAX (cost > 0 ? 1 : 0, cost * factor);

  return new_cost;
}

/* Auxiliary function to emit RISC-V ELF attribute. */
static void
riscv_emit_attribute ()
{
  fprintf (asm_out_file, "\t.attribute arch, \"%s\"\n",
	   riscv_arch_str ().c_str ());

  fprintf (asm_out_file, "\t.attribute unaligned_access, %d\n",
           TARGET_STRICT_ALIGN ? 0 : 1);

  fprintf (asm_out_file, "\t.attribute stack_align, %d\n",
           riscv_stack_boundary / 8);
}

/* Output .variant_cc for function symbol which follows vector calling
   convention.  */

static void
riscv_asm_output_variant_cc (FILE *stream, const tree decl, const char *name)
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      riscv_cc cc = (riscv_cc) fndecl_abi (decl).id ();
      if (cc == RISCV_CC_V)
	{
	  fprintf (stream, "\t.variant_cc\t");
	  assemble_name (stream, name);
	  fprintf (stream, "\n");
	}
    }
}

/* Implement ASM_DECLARE_FUNCTION_NAME.  */

void
riscv_declare_function_name (FILE *stream, const char *name, tree fndecl)
{
  riscv_asm_output_variant_cc (stream, fndecl, name);
  ASM_OUTPUT_TYPE_DIRECTIVE (stream, name, "function");
  ASM_OUTPUT_FUNCTION_LABEL (stream, name, fndecl);
  if (DECL_FUNCTION_SPECIFIC_TARGET (fndecl))
    {
      fprintf (stream, "\t.option push\n");

      struct cl_target_option *local_cl_target =
	TREE_TARGET_OPTION (DECL_FUNCTION_SPECIFIC_TARGET (fndecl));
      struct cl_target_option *global_cl_target =
	TREE_TARGET_OPTION (target_option_default_node);

      const char *local_arch_str = get_arch_str (local_cl_target);
      const char *arch_str = local_arch_str != NULL
	? local_arch_str
	: riscv_arch_str (true).c_str ();
      fprintf (stream, "\t.option arch, %s\n", arch_str);
      const char *local_tune_str = get_tune_str (local_cl_target);
      const char *global_tune_str = get_tune_str (global_cl_target);
      if (strcmp (local_tune_str, global_tune_str) != 0)
	fprintf (stream, "\t# tune = %s\n", local_tune_str);
    }
}

void
riscv_declare_function_size (FILE *stream, const char *name, tree fndecl)
{
  if (!flag_inhibit_size_directive)
    ASM_OUTPUT_MEASURED_SIZE (stream, name);

  if (DECL_FUNCTION_SPECIFIC_TARGET (fndecl))
    {
      fprintf (stream, "\t.option pop\n");
    }
}

/* Implement ASM_OUTPUT_DEF_FROM_DECLS.  */

void
riscv_asm_output_alias (FILE *stream, const tree decl, const tree target)
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  const char *value = IDENTIFIER_POINTER (target);
  riscv_asm_output_variant_cc (stream, decl, name);
  ASM_OUTPUT_DEF (stream, name, value);
}

/* Implement ASM_OUTPUT_EXTERNAL.  */

void
riscv_asm_output_external (FILE *stream, tree decl, const char *name)
{
  default_elf_asm_output_external (stream, decl, name);
  riscv_asm_output_variant_cc (stream, decl, name);
}

/* Implement TARGET_ASM_FILE_START.  */

static void
riscv_file_start (void)
{
  default_file_start ();

  /* Instruct GAS to generate position-[in]dependent code.  */
  fprintf (asm_out_file, "\t.option %spic\n", (flag_pic ? "" : "no"));

  /* If the user specifies "-mno-relax" on the command line then disable linker
     relaxation in the assembler.  */
  if (! riscv_mrelax)
    fprintf (asm_out_file, "\t.option norelax\n");

  /* If the user specifies "-mcsr-check" on the command line then enable csr
     check in the assembler.  */
  if (riscv_mcsr_check)
    fprintf (asm_out_file, "\t.option csr-check\n");

  if (riscv_emit_attribute_p)
    riscv_emit_attribute ();
}

/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  Generate rtl rather than asm text
   in order to avoid duplicating too much logic from elsewhere.  */

static void
riscv_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
		      HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		      tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));
  rtx this_rtx, temp1, temp2, fnaddr;
  rtx_insn *insn;

  riscv_in_thunk_func = true;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Determine if we can use a sibcall to call FUNCTION directly.  */
  fnaddr = gen_rtx_MEM (FUNCTION_MODE, XEXP (DECL_RTL (function), 0));

  /* We need two temporary registers in some cases.  */
  temp1 = gen_rtx_REG (Pmode, RISCV_PROLOGUE_TEMP_REGNUM);
  temp2 = gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);

  /* Find out which register contains the "this" pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, GP_ARG_FIRST + 1);
  else
    this_rtx = gen_rtx_REG (Pmode, GP_ARG_FIRST);

  /* Add DELTA to THIS_RTX.  */
  if (delta != 0)
    {
      rtx offset = GEN_INT (delta);
      if (!SMALL_OPERAND (delta))
	{
	  riscv_emit_move (temp1, offset);
	  offset = temp1;
	}
      emit_insn (gen_add3_insn (this_rtx, this_rtx, offset));
    }

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset != 0)
    {
      rtx addr;

      /* Set TEMP1 to *THIS_RTX.  */
      riscv_emit_move (temp1, gen_rtx_MEM (Pmode, this_rtx));

      /* Set ADDR to a legitimate address for *THIS_RTX + VCALL_OFFSET.  */
      addr = riscv_add_offset (temp2, temp1, vcall_offset);

      /* Load the offset and add it to THIS_RTX.  */
      riscv_emit_move (temp1, gen_rtx_MEM (Pmode, addr));
      emit_insn (gen_add3_insn (this_rtx, this_rtx, temp1));
    }

  /* Jump to the target function.  */
  rtx callee_cc = gen_int_mode (fndecl_abi (function).id (), SImode);
  insn = emit_call_insn (gen_sibcall (fnaddr, const0_rtx, callee_cc));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation.  This sequence was
     "borrowed" from alpha.cc.  */
  insn = get_insns ();
  split_all_insns_noflow ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  /* Clean up the vars set above.  Note that final_end_function resets
     the global pointer for us.  */
  reload_completed = 0;
  riscv_in_thunk_func = false;
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */

static struct machine_function *
riscv_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Return the VLEN value associated with -march and -mwrvv-vector-bits.
   TODO: So far we only support length-agnostic value. */
static poly_uint16
riscv_convert_vector_chunks (struct gcc_options *opts)
{
  int chunk_num;
  int min_vlen = TARGET_MIN_VLEN_OPTS (opts);
  if (min_vlen > 32)
    {
      /* When targetting minimum VLEN > 32, we should use 64-bit chunk size.
	 Otherwise we can not include SEW = 64bits.
	 Runtime invariant: The single indeterminate represent the
	 number of 64-bit chunks in a vector beyond minimum length of 64 bits.
	 Thus the number of bytes in a vector is 8 + 8 * x1 which is
	 riscv_vector_chunks * 8 = poly_int (8, 8).  */
      riscv_bytes_per_vector_chunk = 8;
      /* Adjust BYTES_PER_RISCV_VECTOR according to TARGET_MIN_VLEN:
	   - TARGET_MIN_VLEN = 64bit: [8,8]
	   - TARGET_MIN_VLEN = 128bit: [16,16]
	   - TARGET_MIN_VLEN = 256bit: [32,32]
	   - TARGET_MIN_VLEN = 512bit: [64,64]
	   - TARGET_MIN_VLEN = 1024bit: [128,128]
	   - TARGET_MIN_VLEN = 2048bit: [256,256]
	   - TARGET_MIN_VLEN = 4096bit: [512,512]
	   FIXME: We currently DON'T support TARGET_MIN_VLEN > 4096bit.  */
      chunk_num = min_vlen / 64;
    }
  else
    {
      /* When targetting minimum VLEN = 32, we should use 32-bit
	 chunk size. Runtime invariant: The single indeterminate represent the
	 number of 32-bit chunks in a vector beyond minimum length of 32 bits.
	 Thus the number of bytes in a vector is 4 + 4 * x1 which is
	 riscv_vector_chunks * 4 = poly_int (4, 4).  */
      riscv_bytes_per_vector_chunk = 4;
      chunk_num = 1;
    }

  /* Set riscv_vector_chunks as poly (1, 1) run-time constant if TARGET_VECTOR
     is enabled. Set riscv_vector_chunks as 1 compile-time constant if
     TARGET_VECTOR is disabled. riscv_vector_chunks is used in "riscv-modes.def"
     to set RVV mode size. The RVV machine modes size are run-time constant if
     TARGET_VECTOR is enabled. The RVV machine modes size remains default
     compile-time constant if TARGET_VECTOR is disabled.  */
  if (TARGET_VECTOR_OPTS_P (opts))
    {
      switch (opts->x_rvv_vector_bits)
	{
	case RVV_VECTOR_BITS_SCALABLE:
	  return poly_uint16 (chunk_num, chunk_num);
	case RVV_VECTOR_BITS_ZVL:
	  return (int) min_vlen / (riscv_bytes_per_vector_chunk * 8);
	default:
	  gcc_unreachable ();
	}
    }
  else
    return 1;
}

/* 'Unpack' up the internal tuning structs and update the options
    in OPTS.  The caller must have set up selected_tune and selected_arch
    as all the other target-specific codegen decisions are
    derived from them.  */
void
riscv_override_options_internal (struct gcc_options *opts)
{
  const struct riscv_tune_info *cpu;

  /* The presence of the M extension implies that division instructions
     are present, so include them unless explicitly disabled.  */
  if (TARGET_MUL_OPTS_P (opts) && (target_flags_explicit & MASK_DIV) == 0)
    opts->x_target_flags |= MASK_DIV;
  else if (!TARGET_MUL_OPTS_P (opts) && TARGET_DIV_OPTS_P (opts))
    error ("%<-mdiv%> requires %<-march%> to subsume the %<M%> extension");

  /* Likewise floating-point division and square root.  */
  if ((TARGET_HARD_FLOAT_OPTS_P (opts) || TARGET_ZFINX_OPTS_P (opts))
      && ((target_flags_explicit & MASK_FDIV) == 0))
    opts->x_target_flags |= MASK_FDIV;

  /* Handle -mtune, use -mcpu if -mtune is not given, and use default -mtune
     if both -mtune and -mcpu are not given.  */
  const char *tune_string = get_tune_str (opts);
  cpu = riscv_parse_tune (tune_string, false);
  riscv_microarchitecture = cpu->microarchitecture;
  tune_param = opts->x_optimize_size
		 ? &optimize_size_tune_info
		 : cpu->tune_param;

  /* Use -mtune's setting for slow_unaligned_access, even when optimizing
     for size.  For architectures that trap and emulate unaligned accesses,
     the performance cost is too great, even for -Os.  Similarly, if
     -m[no-]strict-align is left unspecified, heed -mtune's advice.  */
  riscv_slow_unaligned_access_p = (cpu->tune_param->slow_unaligned_access
				   || TARGET_STRICT_ALIGN);

  /* Make a note if user explicity passed -mstrict-align for later
     builtin macro generation.  Can't use target_flags_explicitly since
     it is set even for -mno-strict-align.  */
  riscv_user_wants_strict_align = TARGET_STRICT_ALIGN_OPTS_P (opts);

  if ((target_flags_explicit & MASK_STRICT_ALIGN) == 0
      && cpu->tune_param->slow_unaligned_access)
    opts->x_target_flags |= MASK_STRICT_ALIGN;

  /* If the user hasn't specified a branch cost, use the processor's
     default.  */
  if (opts->x_riscv_branch_cost == 0)
    opts->x_riscv_branch_cost = tune_param->branch_cost;

  /* FIXME: We don't allow TARGET_MIN_VLEN > 4096 since the datatypes of
     both GET_MODE_SIZE and GET_MODE_BITSIZE are poly_uint16.

     We can only allow TARGET_MIN_VLEN * 8 (LMUL) < 65535.  */
  if (TARGET_MIN_VLEN_OPTS (opts) > 4096)
    sorry ("Current RISC-V GCC does not support VLEN greater than 4096bit for "
	   "'V' Extension");

  /* FIXME: We don't support RVV in big-endian for now, we may enable RVV with
     big-endian after finishing full coverage testing.  */
  if (TARGET_VECTOR && TARGET_BIG_ENDIAN)
    sorry ("Current RISC-V GCC does not support RVV in big-endian mode");

  /* Convert -march and -mrvv-vector-bits to a chunks count.  */
  riscv_vector_chunks = riscv_convert_vector_chunks (opts);
}

/* Implement TARGET_OPTION_OVERRIDE.  */

void
riscv_option_override (void)
{
#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  flag_pcc_struct_return = 0;

  if (flag_pic)
    g_switch_value = 0;

  /* Always prefer medlow than medany for RV32 since medlow can access
     full address space. */
  if (riscv_cmodel == CM_LARGE && !TARGET_64BIT)
    riscv_cmodel = CM_MEDLOW;

  if (riscv_cmodel == CM_LARGE && TARGET_EXPLICIT_RELOCS)
    sorry ("code model %qs with %qs", "large", "-mexplicit-relocs");

  if (riscv_cmodel == CM_LARGE && flag_pic)
    sorry ("code model %qs with %qs", "large",
	   global_options.x_flag_pic > 1 ? "-fPIC" : "-fpic");

  if (flag_pic)
    riscv_cmodel = CM_PIC;

  /* We need to save the fp with ra for non-leaf functions with no fp and ra
     for leaf functions while no-omit-frame-pointer with
     omit-leaf-frame-pointer.  The x_flag_omit_frame_pointer has the first
     priority to determine whether the frame pointer is needed.  If we do not
     override it, the fp and ra will be stored for leaf functions, which is not
     our wanted.  */
  riscv_save_frame_pointer = false;
  if (TARGET_OMIT_LEAF_FRAME_POINTER_P (global_options.x_target_flags))
    {
      if (!global_options.x_flag_omit_frame_pointer)
	riscv_save_frame_pointer = true;

      global_options.x_flag_omit_frame_pointer = 1;
    }

  /* We get better code with explicit relocs for CM_MEDLOW, but
     worse code for the others (for now).  Pick the best default.  */
  if ((target_flags_explicit & MASK_EXPLICIT_RELOCS) == 0)
    if (riscv_cmodel == CM_MEDLOW)
      target_flags |= MASK_EXPLICIT_RELOCS;

  /* Require that the ISA supports the requested floating-point ABI.  */
  if (UNITS_PER_FP_ARG > (TARGET_HARD_FLOAT ? UNITS_PER_FP_REG : 0))
    error ("requested ABI requires %<-march%> to subsume the %qc extension",
	   UNITS_PER_FP_ARG > 8 ? 'Q' : (UNITS_PER_FP_ARG > 4 ? 'D' : 'F'));

  /* RVE requires specific ABI.  */
  if (TARGET_RVE)
    {
      if (!TARGET_64BIT && riscv_abi != ABI_ILP32E)
	error ("rv32e requires ilp32e ABI");
      else if (TARGET_64BIT && riscv_abi != ABI_LP64E)
	error ("rv64e requires lp64e ABI");
    }

  /* Zfinx require abi ilp32, ilp32e, lp64 or lp64e.  */
  if (TARGET_ZFINX
      && riscv_abi != ABI_ILP32 && riscv_abi != ABI_LP64
      && riscv_abi != ABI_ILP32E && riscv_abi != ABI_LP64E)
    error ("z*inx requires ABI ilp32, ilp32e, lp64 or lp64e");

  /* We do not yet support ILP32 on RV64.  */
  if (BITS_PER_WORD != POINTER_SIZE)
    error ("ABI requires %<-march=rv%d%>", POINTER_SIZE);

  /* Validate -mpreferred-stack-boundary= value.  */
  riscv_stack_boundary = ABI_STACK_BOUNDARY;
  if (riscv_preferred_stack_boundary_arg)
    {
      int min = ctz_hwi (STACK_BOUNDARY / 8);
      int max = 8;

      if (!IN_RANGE (riscv_preferred_stack_boundary_arg, min, max))
	error ("%<-mpreferred-stack-boundary=%d%> must be between %d and %d",
	       riscv_preferred_stack_boundary_arg, min, max);

      riscv_stack_boundary = 8 << riscv_preferred_stack_boundary_arg;
    }

  if (riscv_emit_attribute_p < 0)
#ifdef HAVE_AS_RISCV_ATTRIBUTE
    riscv_emit_attribute_p = TARGET_RISCV_ATTRIBUTE;
#else
    riscv_emit_attribute_p = 0;

  if (riscv_emit_attribute_p)
    error ("%<-mriscv-attribute%> RISC-V ELF attribute requires GNU as 2.32"
	   " [%<-mriscv-attribute%>]");
#endif

  if (riscv_stack_protector_guard == SSP_GLOBAL
      && OPTION_SET_P (riscv_stack_protector_guard_offset_str))
    {
      error ("incompatible options %<-mstack-protector-guard=global%> and "
	     "%<-mstack-protector-guard-offset=%s%>",
	     riscv_stack_protector_guard_offset_str);
    }

  if (riscv_stack_protector_guard == SSP_TLS
      && !(OPTION_SET_P (riscv_stack_protector_guard_offset_str)
	   && OPTION_SET_P (riscv_stack_protector_guard_reg_str)))
    {
      error ("both %<-mstack-protector-guard-offset%> and "
	     "%<-mstack-protector-guard-reg%> must be used "
	     "with %<-mstack-protector-guard=sysreg%>");
    }

  if (OPTION_SET_P (riscv_stack_protector_guard_reg_str))
    {
      const char *str = riscv_stack_protector_guard_reg_str;
      int reg = decode_reg_name (str);

      if (!IN_RANGE (reg, GP_REG_FIRST + 1, GP_REG_LAST))
	error ("%qs is not a valid base register in %qs", str,
	       "-mstack-protector-guard-reg=");

      riscv_stack_protector_guard_reg = reg;
    }

  if (OPTION_SET_P (riscv_stack_protector_guard_offset_str))
    {
      char *end;
      const char *str = riscv_stack_protector_guard_offset_str;
      errno = 0;
      long offs = strtol (riscv_stack_protector_guard_offset_str, &end, 0);

      if (!*str || *end || errno)
	error ("%qs is not a valid number in %qs", str,
	       "-mstack-protector-guard-offset=");

      if (!SMALL_OPERAND (offs))
	error ("%qs is not a valid offset in %qs", str,
	       "-mstack-protector-guard-offset=");

      riscv_stack_protector_guard_offset = offs;
    }

  SET_OPTION_IF_UNSET (&global_options, &global_options_set,
		       param_sched_pressure_algorithm,
		       SCHED_PRESSURE_MODEL);

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &riscv_init_machine_status;

  riscv_override_options_internal (&global_options);

  /* Save these options as the default ones in case we push and pop them later
     while processing functions with potential target attributes.  */
  target_option_default_node = target_option_current_node
    = build_target_option_node (&global_options, &global_options_set);
}

/* Restore or save the TREE_TARGET_GLOBALS from or to NEW_TREE.
   Used by riscv_set_current_function to
   make sure optab availability predicates are recomputed when necessary.  */

void
riscv_save_restore_target_globals (tree new_tree)
{
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
}

/* Implements TARGET_OPTION_RESTORE.  Restore the backend codegen decisions
   using the information saved in PTR.  */

static void
riscv_option_restore (struct gcc_options *opts,
		      struct gcc_options * /* opts_set */,
		      struct cl_target_option * /* ptr */)
{
  riscv_override_options_internal (opts);
}

static GTY (()) tree riscv_previous_fndecl;

/* Implement TARGET_CONDITIONAL_REGISTER_USAGE.  */

static void
riscv_conditional_register_usage (void)
{
  /* We have only x0~x15 on RV32E/RV64E.  */
  if (TARGET_RVE)
    {
      for (int r = 16; r <= 31; r++)
	fixed_regs[r] = 1;
    }

  if (riscv_abi == ABI_ILP32E)
    {
      for (int r = 16; r <= 31; r++)
	call_used_regs[r] = 1;
    }

  if (!TARGET_HARD_FLOAT)
    {
      for (int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }

  /* In the soft-float ABI, there are no callee-saved FP registers.  */
  if (UNITS_PER_FP_ARG == 0)
    {
      for (int regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
	call_used_regs[regno] = 1;
    }

  if (!TARGET_VECTOR)
    {
      for (int regno = V_REG_FIRST; regno <= V_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;

      fixed_regs[VTYPE_REGNUM] = call_used_regs[VTYPE_REGNUM] = 1;
      fixed_regs[VL_REGNUM] = call_used_regs[VL_REGNUM] = 1;
      fixed_regs[VXRM_REGNUM] = call_used_regs[VXRM_REGNUM] = 1;
      fixed_regs[FRM_REGNUM] = call_used_regs[FRM_REGNUM] = 1;
    }
}

/* Return a register priority for hard reg REGNO.  */

static int
riscv_register_priority (int regno)
{
  /* Favor compressed registers to improve the odds of RVC instruction
     selection.  */
  if (riscv_compressed_reg_p (regno))
    return 1;

  return 0;
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
riscv_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx addr, end_addr, mem;
  uint32_t trampoline[4];
  unsigned int i;
  HOST_WIDE_INT static_chain_offset, target_function_offset;

  /* Work out the offsets of the pointers from the start of the
     trampoline code.  */
  gcc_assert (ARRAY_SIZE (trampoline) * 4 == TRAMPOLINE_CODE_SIZE);

  /* Get pointers to the beginning and end of the code block.  */
  addr = force_reg (Pmode, XEXP (m_tramp, 0));
  end_addr = riscv_force_binary (Pmode, PLUS, addr,
				 GEN_INT (TRAMPOLINE_CODE_SIZE));


  if (Pmode == SImode)
    {
      chain_value = force_reg (Pmode, chain_value);

      rtx target_function = force_reg (Pmode, XEXP (DECL_RTL (fndecl), 0));
      /* lui     t2, hi(chain)
	 lui     t0, hi(func)
	 addi    t2, t2, lo(chain)
	 jr      t0, lo(func)
      */
      unsigned HOST_WIDE_INT lui_hi_chain_code, lui_hi_func_code;
      unsigned HOST_WIDE_INT lo_chain_code, lo_func_code;

      rtx uimm_mask = force_reg (SImode, gen_int_mode (-IMM_REACH, SImode));

      /* 0xfff.  */
      rtx imm12_mask = gen_reg_rtx (SImode);
      emit_insn (gen_one_cmplsi2 (imm12_mask, uimm_mask));

      rtx fixup_value = force_reg (SImode, gen_int_mode (IMM_REACH/2, SImode));

      /* Gen lui t2, hi(chain).  */
      rtx hi_chain = riscv_force_binary (SImode, PLUS, chain_value,
					 fixup_value);
      hi_chain = riscv_force_binary (SImode, AND, hi_chain,
				     uimm_mask);
      lui_hi_chain_code = OPCODE_LUI | (STATIC_CHAIN_REGNUM << SHIFT_RD);
      rtx lui_hi_chain = riscv_force_binary (SImode, IOR, hi_chain,
					     gen_int_mode (lui_hi_chain_code, SImode));

      mem = adjust_address (m_tramp, SImode, 0);
      riscv_emit_move (mem, riscv_swap_instruction (lui_hi_chain));

      /* Gen lui t0, hi(func).  */
      rtx hi_func = riscv_force_binary (SImode, PLUS, target_function,
					fixup_value);
      hi_func = riscv_force_binary (SImode, AND, hi_func,
				    uimm_mask);
      lui_hi_func_code = OPCODE_LUI | (RISCV_PROLOGUE_TEMP_REGNUM << SHIFT_RD);
      rtx lui_hi_func = riscv_force_binary (SImode, IOR, hi_func,
					    gen_int_mode (lui_hi_func_code, SImode));

      mem = adjust_address (m_tramp, SImode, 1 * GET_MODE_SIZE (SImode));
      riscv_emit_move (mem, riscv_swap_instruction (lui_hi_func));

      /* Gen addi t2, t2, lo(chain).  */
      rtx lo_chain = riscv_force_binary (SImode, AND, chain_value,
					 imm12_mask);
      lo_chain = riscv_force_binary (SImode, ASHIFT, lo_chain, GEN_INT (20));

      lo_chain_code = OPCODE_ADDI
		      | (STATIC_CHAIN_REGNUM << SHIFT_RD)
		      | (STATIC_CHAIN_REGNUM << SHIFT_RS1);

      rtx addi_lo_chain = riscv_force_binary (SImode, IOR, lo_chain,
					      force_reg (SImode, GEN_INT (lo_chain_code)));

      mem = adjust_address (m_tramp, SImode, 2 * GET_MODE_SIZE (SImode));
      riscv_emit_move (mem, riscv_swap_instruction (addi_lo_chain));

      /* Gen jr t0, lo(func).  */
      rtx lo_func = riscv_force_binary (SImode, AND, target_function,
					imm12_mask);
      lo_func = riscv_force_binary (SImode, ASHIFT, lo_func, GEN_INT (20));

      lo_func_code = OPCODE_JALR | (RISCV_PROLOGUE_TEMP_REGNUM << SHIFT_RS1);

      rtx jr_lo_func = riscv_force_binary (SImode, IOR, lo_func,
					   force_reg (SImode, GEN_INT (lo_func_code)));

      mem = adjust_address (m_tramp, SImode, 3 * GET_MODE_SIZE (SImode));
      riscv_emit_move (mem, riscv_swap_instruction (jr_lo_func));
    }
  else
    {
      static_chain_offset = TRAMPOLINE_CODE_SIZE;
      target_function_offset = static_chain_offset + GET_MODE_SIZE (ptr_mode);

      /* auipc   t2, 0
	 l[wd]   t0, target_function_offset(t2)
	 l[wd]   t2, static_chain_offset(t2)
	 jr      t0
      */
      trampoline[0] = OPCODE_AUIPC | (STATIC_CHAIN_REGNUM << SHIFT_RD);
      trampoline[1] = (Pmode == DImode ? OPCODE_LD : OPCODE_LW)
		      | (RISCV_PROLOGUE_TEMP_REGNUM << SHIFT_RD)
		      | (STATIC_CHAIN_REGNUM << SHIFT_RS1)
		      | (target_function_offset << SHIFT_IMM);
      trampoline[2] = (Pmode == DImode ? OPCODE_LD : OPCODE_LW)
		      | (STATIC_CHAIN_REGNUM << SHIFT_RD)
		      | (STATIC_CHAIN_REGNUM << SHIFT_RS1)
		      | (static_chain_offset << SHIFT_IMM);
      trampoline[3] = OPCODE_JALR | (RISCV_PROLOGUE_TEMP_REGNUM << SHIFT_RS1);

      /* Copy the trampoline code.  */
      for (i = 0; i < ARRAY_SIZE (trampoline); i++)
	{
	  if (BYTES_BIG_ENDIAN)
	    trampoline[i] = __builtin_bswap32(trampoline[i]);
	  mem = adjust_address (m_tramp, SImode, i * GET_MODE_SIZE (SImode));
	  riscv_emit_move (mem, gen_int_mode (trampoline[i], SImode));
	}

      /* Set up the static chain pointer field.  */
      mem = adjust_address (m_tramp, ptr_mode, static_chain_offset);
      riscv_emit_move (mem, chain_value);

      /* Set up the target function field.  */
      mem = adjust_address (m_tramp, ptr_mode, target_function_offset);
      riscv_emit_move (mem, XEXP (DECL_RTL (fndecl), 0));
    }

  /* Flush the code part of the trampoline.  */
  emit_insn (gen_add3_insn (end_addr, addr, GEN_INT (TRAMPOLINE_SIZE)));
  emit_insn (gen_clear_cache (addr, end_addr));
}

/* Implement TARGET_FUNCTION_OK_FOR_SIBCALL.  */

static bool
riscv_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
			       tree exp ATTRIBUTE_UNUSED)
{
  /* Don't use sibcalls when use save-restore routine.  */
  if (TARGET_SAVE_RESTORE)
    return false;

  /* Don't use sibcall for naked functions.  */
  if (cfun->machine->naked_p)
    return false;

  /* Don't use sibcall for interrupt functions.  */
  if (cfun->machine->interrupt_handler_p)
    return false;

  /* Don't use sibcalls in the large model, because a sibcall instruction
     expanding and a epilogue expanding both use RISCV_PROLOGUE_TEMP
     register.  */
  if (riscv_cmodel == CM_LARGE)
    return false;

  return true;
}

/* Get the interrupt type, return UNKNOWN_MODE if it's not
   interrupt function. */
static enum riscv_privilege_levels
riscv_get_interrupt_type (tree decl)
{
  gcc_assert (decl != NULL_TREE);

  if ((TREE_CODE(decl) != FUNCTION_DECL)
      || (!riscv_interrupt_type_p (TREE_TYPE (decl))))
    return UNKNOWN_MODE;

  tree attr_args
    = TREE_VALUE (lookup_attribute ("interrupt",
				    TYPE_ATTRIBUTES (TREE_TYPE (decl))));

  if (attr_args && TREE_CODE (TREE_VALUE (attr_args)) != VOID_TYPE)
    {
      const char *string = TREE_STRING_POINTER (TREE_VALUE (attr_args));

      if (!strcmp (string, "user"))
	return USER_MODE;
      else if (!strcmp (string, "supervisor"))
	return SUPERVISOR_MODE;
      else /* Must be "machine".  */
	return MACHINE_MODE;
    }
  else
    /* Interrupt attributes are machine mode by default.  */
    return MACHINE_MODE;
}

/* Implement `TARGET_SET_CURRENT_FUNCTION'.  Unpack the codegen decisions
   like tuning and ISA features from the DECL_FUNCTION_SPECIFIC_TARGET
   of the function, if such exists.  This function may be called multiple
   times on a single function so use aarch64_previous_fndecl to avoid
   setting up identical state.  */

/* Sanity cheching for above function attributes.  */
static void
riscv_set_current_function (tree decl)
{
  if (decl == NULL_TREE
      || current_function_decl == NULL_TREE
      || current_function_decl == error_mark_node
      || ! cfun->machine)
    return;

  if (!cfun->machine->attributes_checked_p)
    {
      cfun->machine->naked_p = riscv_naked_function_p (decl);
      cfun->machine->interrupt_handler_p
	= riscv_interrupt_type_p (TREE_TYPE (decl));

      if (cfun->machine->naked_p && cfun->machine->interrupt_handler_p)
	error ("function attributes %qs and %qs are mutually exclusive",
	       "interrupt", "naked");

      if (cfun->machine->interrupt_handler_p)
	{
	  tree ret = TREE_TYPE (TREE_TYPE (decl));
	  tree args = TYPE_ARG_TYPES (TREE_TYPE (decl));

	  if (TREE_CODE (ret) != VOID_TYPE)
	    error ("%qs function cannot return a value", "interrupt");

	  if (args && TREE_CODE (TREE_VALUE (args)) != VOID_TYPE)
	    error ("%qs function cannot have arguments", "interrupt");

	  cfun->machine->interrupt_mode = riscv_get_interrupt_type (decl);

	  gcc_assert (cfun->machine->interrupt_mode != UNKNOWN_MODE);
	}

      /* Don't print the above diagnostics more than once.  */
      cfun->machine->attributes_checked_p = 1;
    }

  if (!decl || decl == riscv_previous_fndecl)
    return;

  tree old_tree = (riscv_previous_fndecl
		     ? DECL_FUNCTION_SPECIFIC_TARGET (riscv_previous_fndecl)
		     : NULL_TREE);

  tree new_tree = DECL_FUNCTION_SPECIFIC_TARGET (decl);

  /* If current function has no attributes but the previous one did,
     use the default node.  */
  if (!new_tree && old_tree)
    new_tree = target_option_default_node;

  /* If nothing to do, return.  #pragma GCC reset or #pragma GCC pop to
     the default have been handled by aarch64_save_restore_target_globals from
     aarch64_pragma_target_parse.  */
  if (old_tree == new_tree)
    return;

  riscv_previous_fndecl = decl;

  /* First set the target options.  */
  cl_target_option_restore (&global_options, &global_options_set,
			    TREE_TARGET_OPTION (new_tree));

  /* The ISA extension can vary based on the function extension like target.
     Thus, make sure that the machine modes are reflected correctly here.  */
  init_adjust_machine_modes ();

  riscv_save_restore_target_globals (new_tree);
}

/* Implement TARGET_MERGE_DECL_ATTRIBUTES. */
static tree
riscv_merge_decl_attributes (tree olddecl, tree newdecl)
{
  tree combined_attrs;

  enum riscv_privilege_levels old_interrupt_type
    = riscv_get_interrupt_type (olddecl);
  enum riscv_privilege_levels new_interrupt_type
    = riscv_get_interrupt_type (newdecl);

  /* Check old and new has same interrupt type. */
  if ((old_interrupt_type != UNKNOWN_MODE)
      && (new_interrupt_type != UNKNOWN_MODE)
      && (old_interrupt_type != new_interrupt_type))
    error ("%qs function cannot have different interrupt type", "interrupt");

  /* Create combined attributes.  */
  combined_attrs = merge_attributes (DECL_ATTRIBUTES (olddecl),
                                     DECL_ATTRIBUTES (newdecl));

  return combined_attrs;
}

/* Implement TARGET_CANNOT_COPY_INSN_P.  */

static bool
riscv_cannot_copy_insn_p (rtx_insn *insn)
{
  return recog_memoized (insn) >= 0 && get_attr_cannot_copy (insn);
}

/* Implement TARGET_SLOW_UNALIGNED_ACCESS.  */

static bool
riscv_slow_unaligned_access (machine_mode, unsigned int)
{
  return riscv_slow_unaligned_access_p;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
riscv_can_change_mode_class (machine_mode from, machine_mode to,
			     reg_class_t rclass)
{
  /* We have RVV VLS modes and VLA modes sharing same REG_CLASS.
     In 'cprop_hardreg' stage, we will try to do hard reg copy propagation
     between wider mode (FROM) and narrow mode (TO).

     E.g. We should not allow copy propagation
	- RVVMF8BI (precision = [16, 16]) -> V32BI (precision = [32, 0])
     since we can't order their size which will cause ICE in regcprop.

     TODO: Even though they are have different size, they always change
     the whole register.  We may enhance such case in regcprop to optimize
     it in the future.  */
  if (reg_classes_intersect_p (V_REGS, rclass)
      && !ordered_p (GET_MODE_PRECISION (from), GET_MODE_PRECISION (to)))
    return false;
  return !reg_classes_intersect_p (FP_REGS, rclass);
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  */

static HOST_WIDE_INT
riscv_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if ((TREE_CODE (exp) == STRING_CST || TREE_CODE (exp) == CONSTRUCTOR)
      && (riscv_align_data_type == riscv_align_data_type_xlen))
    return MAX (align, BITS_PER_WORD);
  return align;
}

/* Implement TARGET_PROMOTE_FUNCTION_MODE.  */

/* This function is equivalent to default_promote_function_mode_always_promote
   except that it returns a promoted mode even if type is NULL_TREE.  This is
   needed by libcalls which have no type (only a mode) such as fixed conversion
   routines that take a signed or unsigned char/short/int argument and convert
   it to a fixed type.  */

static machine_mode
riscv_promote_function_mode (const_tree type ATTRIBUTE_UNUSED,
			     machine_mode mode,
			     int *punsignedp ATTRIBUTE_UNUSED,
			     const_tree fntype ATTRIBUTE_UNUSED,
			     int for_return ATTRIBUTE_UNUSED)
{
  int unsignedp;

  if (type != NULL_TREE)
    return promote_mode (type, mode, punsignedp);

  unsignedp = *punsignedp;
  scalar_mode smode = as_a <scalar_mode> (mode);
  PROMOTE_MODE (smode, unsignedp, type);
  *punsignedp = unsignedp;
  return smode;
}

/* Implement TARGET_MACHINE_DEPENDENT_REORG.  */

static void
riscv_reorg (void)
{
  /* Do nothing unless we have -msave-restore */
  if (TARGET_SAVE_RESTORE)
    riscv_remove_unneeded_save_restore_calls ();
}

/* Return nonzero if register FROM_REGNO can be renamed to register
   TO_REGNO.  */

bool
riscv_hard_regno_rename_ok (unsigned from_regno ATTRIBUTE_UNUSED,
			    unsigned to_regno)
{
  /* Interrupt functions can only use registers that have already been
     saved by the prologue, even if they would normally be
     call-clobbered.  */
  return !cfun->machine->interrupt_handler_p || df_regs_ever_live_p (to_regno);
}

/* Implement TARGET_NEW_ADDRESS_PROFITABLE_P.  */

bool
riscv_new_address_profitable_p (rtx memref, rtx_insn *insn, rtx new_addr)
{
  /* Prefer old address if it is less expensive.  */
  addr_space_t as = MEM_ADDR_SPACE (memref);
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));
  int old_cost = address_cost (XEXP (memref, 0), GET_MODE (memref), as, speed);
  int new_cost = address_cost (new_addr, GET_MODE (memref), as, speed);
  return new_cost <= old_cost;
}

/* Helper function for generating gpr_save pattern.  */

rtx
riscv_gen_gpr_save_insn (struct riscv_frame_info *frame)
{
  unsigned count = riscv_save_libcall_count (frame->mask);
  /* 1 for unspec 2 for clobber t0/t1 and 1 for ra.  */
  unsigned veclen = 1 + 2 + 1 + count;
  rtvec vec = rtvec_alloc (veclen);

  gcc_assert (veclen <= ARRAY_SIZE (gpr_save_reg_order));

  RTVEC_ELT (vec, 0) =
    gen_rtx_UNSPEC_VOLATILE (VOIDmode,
      gen_rtvec (1, GEN_INT (count)), UNSPECV_GPR_SAVE);

  for (unsigned i = 1; i < veclen; ++i)
    {
      unsigned regno = gpr_save_reg_order[i];
      rtx reg = gen_rtx_REG (Pmode, regno);
      rtx elt;

      /* t0 and t1 are CLOBBERs, others are USEs.  */
      if (i < 3)
	elt = gen_rtx_CLOBBER (Pmode, reg);
      else
	elt = gen_rtx_USE (Pmode, reg);

      RTVEC_ELT (vec, i) = elt;
    }

  /* Largest number of caller-save register must set in mask if we are
     not using __riscv_save_0.  */
  gcc_assert ((count == 0) ||
	      BITSET_P (frame->mask, gpr_save_reg_order[veclen - 1]));

  return gen_rtx_PARALLEL (VOIDmode, vec);
}

static HOST_WIDE_INT
zcmp_base_adj (int regs_num)
{
  return riscv_16bytes_align ((regs_num) *GET_MODE_SIZE (word_mode));
}

static HOST_WIDE_INT
zcmp_additional_adj (HOST_WIDE_INT total, int regs_num)
{
  return total - zcmp_base_adj (regs_num);
}

bool
riscv_zcmp_valid_stack_adj_bytes_p (HOST_WIDE_INT total, int regs_num)
{
  HOST_WIDE_INT additioanl_bytes = zcmp_additional_adj (total, regs_num);
  return additioanl_bytes == 0 || additioanl_bytes == 1 * ZCMP_SP_INC_STEP
	 || additioanl_bytes == 2 * ZCMP_SP_INC_STEP
	 || additioanl_bytes == ZCMP_MAX_SPIMM * ZCMP_SP_INC_STEP;
}

/* Return true if it's valid gpr_save pattern.  */

bool
riscv_gpr_save_operation_p (rtx op)
{
  unsigned len = XVECLEN (op, 0);

  if (len > ARRAY_SIZE (gpr_save_reg_order))
    return false;

  for (unsigned i = 0; i < len; i++)
    {
      rtx elt = XVECEXP (op, 0, i);
      if (i == 0)
	{
	  /* First element in parallel is unspec.  */
	  if (GET_CODE (elt) != UNSPEC_VOLATILE
	      || GET_CODE (XVECEXP (elt, 0, 0)) != CONST_INT
	      || XINT (elt, 1) != UNSPECV_GPR_SAVE)
	    return false;
	}
      else
	{
	  /* Two CLOBBER and USEs, must check the order.  */
	  unsigned expect_code = i < 3 ? CLOBBER : USE;
	  if (GET_CODE (elt) != expect_code
	      || !REG_P (XEXP (elt, 1))
	      || (REGNO (XEXP (elt, 1)) != gpr_save_reg_order[i]))
	    return false;
	}
	break;
    }
  return true;
}

/* Implement TARGET_ASAN_SHADOW_OFFSET.  */

static unsigned HOST_WIDE_INT
riscv_asan_shadow_offset (void)
{
  /* We only have libsanitizer support for RV64 at present.

     This number must match ASAN_SHADOW_OFFSET_CONST in the file
     libsanitizer/asan/asan_mapping.h.  */
  return TARGET_64BIT ? HOST_WIDE_INT_UC (0xd55550000) : 0;
}

/* Implement TARGET_MANGLE_TYPE.  */

static const char *
riscv_mangle_type (const_tree type)
{
  /* Half-precision float, _Float16 is "DF16_".  */
  if (SCALAR_FLOAT_TYPE_P (type) && TYPE_PRECISION (type) == 16)
    return "DF16_";

  /* Mangle all vector type for vector extension.  */
  /* The mangle name follows the rule of RVV LLVM
     that is "u" + length of (abi_name) + abi_name. */
  if (TYPE_NAME (type) != NULL)
    {
      const char *res = riscv_vector::mangle_builtin_type (type);
      if (res)
	return res;
    }

  /* Use the default mangling.  */
  return NULL;
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P.  */

static bool
riscv_scalar_mode_supported_p (scalar_mode mode)
{
  if (mode == HFmode)
    return true;
  else
    return default_scalar_mode_supported_p (mode);
}

/* Implement TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P - return TRUE
   if MODE is HFmode, and punt to the generic implementation otherwise.  */

static bool
riscv_libgcc_floating_mode_supported_p (scalar_float_mode mode)
{
  if (mode == HFmode)
    return true;
  else
    return default_libgcc_floating_mode_supported_p (mode);
}

/* Set the value of FLT_EVAL_METHOD.
   ISO/IEC TS 18661-3 defines two values that we'd like to make use of:

    0: evaluate all operations and constants, whose semantic type has at
       most the range and precision of type float, to the range and
       precision of float; evaluate all other operations and constants to
       the range and precision of the semantic type;

    N, where _FloatN is a supported interchange floating type
       evaluate all operations and constants, whose semantic type has at
       most the range and precision of _FloatN type, to the range and
       precision of the _FloatN type; evaluate all other operations and
       constants to the range and precision of the semantic type;

   If we have the zfh/zhinx/zvfh extensions then we support _Float16
   in native precision, so we should set this to 16.  */
static enum flt_eval_method
riscv_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
    case EXCESS_PRECISION_TYPE_FAST:
    case EXCESS_PRECISION_TYPE_STANDARD:
      return ((TARGET_ZFH || TARGET_ZHINX || TARGET_ZVFH)
		? FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16
		: FLT_EVAL_METHOD_PROMOTE_TO_FLOAT);
    case EXCESS_PRECISION_TYPE_IMPLICIT:
    case EXCESS_PRECISION_TYPE_FLOAT16:
      return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16;
    default:
      gcc_unreachable ();
    }
  return FLT_EVAL_METHOD_UNPREDICTABLE;
}

/* Implement TARGET_FLOATN_MODE.  */
static opt_scalar_float_mode
riscv_floatn_mode (int n, bool extended)
{
  if (!extended && n == 16)
    return HFmode;

  return default_floatn_mode (n, extended);
}

static void
riscv_init_libfuncs (void)
{
  /* Half-precision float operations.  The compiler handles all operations
     with NULL libfuncs by converting to SFmode.  */

  /* Arithmetic.  */
  set_optab_libfunc (add_optab, HFmode, NULL);
  set_optab_libfunc (sdiv_optab, HFmode, NULL);
  set_optab_libfunc (smul_optab, HFmode, NULL);
  set_optab_libfunc (neg_optab, HFmode, NULL);
  set_optab_libfunc (sub_optab, HFmode, NULL);

  /* Comparisons.  */
  set_optab_libfunc (eq_optab, HFmode, NULL);
  set_optab_libfunc (ne_optab, HFmode, NULL);
  set_optab_libfunc (lt_optab, HFmode, NULL);
  set_optab_libfunc (le_optab, HFmode, NULL);
  set_optab_libfunc (ge_optab, HFmode, NULL);
  set_optab_libfunc (gt_optab, HFmode, NULL);
  set_optab_libfunc (unord_optab, HFmode, NULL);
}

#if CHECKING_P
void
riscv_reinit (void)
{
  riscv_option_override ();
  init_adjust_machine_modes ();
  init_derived_machine_modes ();
  reinit_regs ();
  init_optabs ();
}
#endif

#if CHECKING_P
#undef TARGET_RUN_TARGET_SELFTESTS
#define TARGET_RUN_TARGET_SELFTESTS selftest::riscv_run_selftests
#endif /* #if CHECKING_P */

/* Implement TARGET_VECTOR_MODE_SUPPORTED_P.  */

static bool
riscv_vector_mode_supported_p (machine_mode mode)
{
  if (TARGET_VECTOR)
    return riscv_v_ext_mode_p (mode);

  return false;
}

/* Implement TARGET_VERIFY_TYPE_CONTEXT.  */

static bool
riscv_verify_type_context (location_t loc, type_context_kind context,
			   const_tree type, bool silent_p)
{
  return riscv_vector::verify_type_context (loc, context, type, silent_p);
}

/* Implement TARGET_VECTOR_ALIGNMENT.  */

static HOST_WIDE_INT
riscv_vector_alignment (const_tree type)
{
  /* ??? Checking the mode isn't ideal, but VECTOR_BOOLEAN_TYPE_P can
     be set for non-predicate vectors of booleans.  Modes are the most
     direct way we have of identifying real RVV predicate types.  */
  /* FIXME: RVV didn't mention the alignment of bool, we uses
     one byte align.  */
  if (GET_MODE_CLASS (TYPE_MODE (type)) == MODE_VECTOR_BOOL)
    return 8;

  widest_int min_size
    = constant_lower_bound (wi::to_poly_widest (TYPE_SIZE (type)));
  return wi::umin (min_size, 128).to_uhwi ();
}

/* Implement REGMODE_NATURAL_SIZE.  */

poly_uint64
riscv_regmode_natural_size (machine_mode mode)
{
  /* The natural size for RVV data modes is one RVV data vector,
     and similarly for predicates.  We can't independently modify
     anything smaller than that.  */
  /* ??? For now, only do this for variable-width RVV registers.
     Doing it for constant-sized registers breaks lower-subreg.c.  */

  if (riscv_v_ext_mode_p (mode))
    {
      poly_uint64 size = GET_MODE_SIZE (mode);
      if (riscv_v_ext_tuple_mode_p (mode))
	{
	  size = GET_MODE_SIZE (riscv_vector::get_subpart_mode (mode));
	  if (known_lt (size, BYTES_PER_RISCV_VECTOR))
	    return size;
	}
      else if (riscv_v_ext_vector_mode_p (mode))
	{
	  /* RVV mask modes always consume a single register.  */
	  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
	    return BYTES_PER_RISCV_VECTOR;
	}
      if (!size.is_constant ())
	return BYTES_PER_RISCV_VECTOR;
      else if (!riscv_v_ext_vls_mode_p (mode))
	/* For -march=rv64gc_zve32f, the natural vector register size
	   is 32bits which is smaller than scalar register size, so we
	   return minimum size between vector register size and scalar
	   register size.  */
	return MIN (size.to_constant (), UNITS_PER_WORD);
    }
  return UNITS_PER_WORD;
}

/* Implement the TARGET_DWARF_POLY_INDETERMINATE_VALUE hook.  */

static unsigned int
riscv_dwarf_poly_indeterminate_value (unsigned int i, unsigned int *factor,
				      int *offset)
{
  /* Polynomial invariant 1 == (VLENB / riscv_bytes_per_vector_chunk) - 1.
     1. TARGET_MIN_VLEN == 32, polynomial invariant 1 == (VLENB / 4) - 1.
     2. TARGET_MIN_VLEN > 32, polynomial invariant 1 == (VLENB / 8) - 1.
  */
  gcc_assert (i == 1);
  *factor = riscv_bytes_per_vector_chunk;
  *offset = 1;
  return RISCV_DWARF_VLENB;
}

/* Implement TARGET_ESTIMATED_POLY_VALUE.  */

static HOST_WIDE_INT
riscv_estimated_poly_value (poly_int64 val,
			    poly_value_estimate_kind kind = POLY_VALUE_LIKELY)
{
  if (TARGET_VECTOR)
    return riscv_vector::estimated_poly_value (val, kind);
  return default_estimated_poly_value (val, kind);
}

/* Return true if the vector misalignment factor is supported by the
   target.  */
bool
riscv_support_vector_misalignment (machine_mode mode,
				   const_tree type ATTRIBUTE_UNUSED,
				   int misalignment,
				   bool is_packed ATTRIBUTE_UNUSED)
{
  /* Depend on movmisalign pattern.  */
  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

/* Implement TARGET_VECTORIZE_GET_MASK_MODE.  */

static opt_machine_mode
riscv_get_mask_mode (machine_mode mode)
{
  if (TARGET_VECTOR && riscv_v_ext_mode_p (mode))
    return riscv_vector::get_mask_mode (mode);

  return default_get_mask_mode (mode);
}

/* Implement TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE.  Assume for now that
   it isn't worth branching around empty masked ops (including masked
   stores).  */

static bool
riscv_empty_mask_is_expensive (unsigned)
{
  return false;
}

/* Return true if a shift-amount matches the trailing cleared bits on
   a bitmask.  */

bool
riscv_shamt_matches_mask_p (int shamt, HOST_WIDE_INT mask)
{
  return shamt == ctz_hwi (mask);
}

static HARD_REG_SET
vector_zero_call_used_regs (HARD_REG_SET need_zeroed_hardregs)
{
  HARD_REG_SET zeroed_hardregs;
  CLEAR_HARD_REG_SET (zeroed_hardregs);

  /* Find a register to hold vl.  */
  unsigned vl_regno = INVALID_REGNUM;
  /* Skip the first GPR, otherwise the existing vl is kept due to the same
     between vl and avl.  */
  for (unsigned regno = GP_REG_FIRST + 1; regno <= GP_REG_LAST; regno++)
    {
      if (TEST_HARD_REG_BIT (need_zeroed_hardregs, regno))
	{
	  vl_regno = regno;
	  break;
	}
    }

  if (vl_regno > GP_REG_LAST)
    sorry ("cannot allocate vl register for %qs on this target",
	   "-fzero-call-used-regs");

  /* Vector configurations need not be saved and restored here.  The
     -fzero-call-used-regs=* option will zero all vector registers and
     return.  So there's no vector operations between them.  */

  bool emitted_vlmax_vsetvl = false;
  rtx vl = gen_rtx_REG (Pmode, vl_regno); /* vl is VLMAX.  */
  for (unsigned regno = V_REG_FIRST; regno <= V_REG_LAST; ++regno)
    {
      if (TEST_HARD_REG_BIT (need_zeroed_hardregs, regno))
	{
	  rtx target = regno_reg_rtx[regno];
	  machine_mode mode = GET_MODE (target);

	  if (!emitted_vlmax_vsetvl)
	    {
	      riscv_vector::emit_hard_vlmax_vsetvl (mode, vl);
	      emitted_vlmax_vsetvl = true;
	    }

	  rtx ops[] = {target, CONST0_RTX (mode)};
	  riscv_vector::emit_vlmax_insn_lra (code_for_pred_mov (mode),
					      riscv_vector::UNARY_OP, ops, vl);

	  SET_HARD_REG_BIT (zeroed_hardregs, regno);
	}
    }

  return zeroed_hardregs;
}

/* Generate a sequence of instructions that zero registers specified by
   NEED_ZEROED_HARDREGS.  Return the ZEROED_HARDREGS that are actually
   zeroed.  */
HARD_REG_SET
riscv_zero_call_used_regs (HARD_REG_SET need_zeroed_hardregs)
{
  HARD_REG_SET zeroed_hardregs;
  CLEAR_HARD_REG_SET (zeroed_hardregs);

  if (TARGET_VECTOR)
    zeroed_hardregs |= vector_zero_call_used_regs (need_zeroed_hardregs);

  return zeroed_hardregs | default_zero_call_used_regs (need_zeroed_hardregs
							& ~zeroed_hardregs);
}

/* Implement target hook TARGET_ARRAY_MODE.  */

static opt_machine_mode
riscv_array_mode (machine_mode mode, unsigned HOST_WIDE_INT nelems)
{
  machine_mode vmode;
  if (TARGET_VECTOR
      && riscv_vector::get_tuple_mode (mode, nelems).exists (&vmode))
    return vmode;

  return opt_machine_mode ();
}

/* Given memory reference MEM, expand code to compute the aligned
   memory address, shift and mask values and store them into
   *ALIGNED_MEM, *SHIFT, *MASK and *NOT_MASK.  */

void
riscv_subword_address (rtx mem, rtx *aligned_mem, rtx *shift, rtx *mask,
		       rtx *not_mask)
{
  /* Align the memory address to a word.  */
  rtx addr = force_reg (Pmode, XEXP (mem, 0));

  rtx addr_mask = gen_int_mode (-4, Pmode);

  rtx aligned_addr = gen_reg_rtx (Pmode);
  emit_move_insn (aligned_addr,  gen_rtx_AND (Pmode, addr, addr_mask));

  *aligned_mem = change_address (mem, SImode, aligned_addr);

  /* Calculate the shift amount.  */
  emit_move_insn (*shift, gen_rtx_AND (SImode, gen_lowpart (SImode, addr),
				       gen_int_mode (3, SImode)));
  emit_move_insn (*shift, gen_rtx_ASHIFT (SImode, *shift,
					  gen_int_mode (3, SImode)));

  /* Calculate the mask.  */
  int unshifted_mask = GET_MODE_MASK (GET_MODE (mem));

  emit_move_insn (*mask, gen_int_mode (unshifted_mask, SImode));

  emit_move_insn (*mask, gen_rtx_ASHIFT (SImode, *mask,
					 gen_lowpart (QImode, *shift)));

  emit_move_insn (*not_mask, gen_rtx_NOT (SImode, *mask));
}

/* Leftshift a subword within an SImode register.  */

void
riscv_lshift_subword (machine_mode mode, rtx value, rtx shift,
		      rtx *shifted_value)
{
  rtx value_reg = gen_reg_rtx (SImode);
  emit_move_insn (value_reg, simplify_gen_subreg (SImode, value,
						  mode, 0));

  emit_move_insn (*shifted_value, gen_rtx_ASHIFT (SImode, value_reg,
						  gen_lowpart (QImode, shift)));
}

/* Return TRUE if we should use the divmod expander, FALSE otherwise.  This
   allows the behavior to be tuned for specific implementations as well as
   when optimizing for size.  */

bool
riscv_use_divmod_expander (void)
{
  return tune_param->use_divmod_expansion;
}

/* Implement TARGET_VECTORIZE_PREFERRED_SIMD_MODE.  */

static machine_mode
riscv_preferred_simd_mode (scalar_mode mode)
{
  if (TARGET_VECTOR && !TARGET_XTHEADVECTOR)
    return riscv_vector::preferred_simd_mode (mode);

  return word_mode;
}

/* Implement target hook TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT.  */

static poly_uint64
riscv_vectorize_preferred_vector_alignment (const_tree type)
{
  if (riscv_v_ext_mode_p (TYPE_MODE (type)))
    return TYPE_ALIGN (TREE_TYPE (type));
  return TYPE_ALIGN (type);
}

/* Return true if it is static FRM rounding mode.  */

static bool
riscv_static_frm_mode_p (int mode)
{
  switch (mode)
    {
    case riscv_vector::FRM_RDN:
    case riscv_vector::FRM_RUP:
    case riscv_vector::FRM_RTZ:
    case riscv_vector::FRM_RMM:
    case riscv_vector::FRM_RNE:
      return true;
    default:
      return false;
    }

  gcc_unreachable ();
}

/* Implement the floating-point Mode Switching.  */

static void
riscv_emit_frm_mode_set (int mode, int prev_mode)
{
  rtx backup_reg = DYNAMIC_FRM_RTL (cfun);

  if (prev_mode == riscv_vector::FRM_DYN_CALL)
    emit_insn (gen_frrmsi (backup_reg)); /* Backup frm when DYN_CALL.  */

  if (mode != prev_mode)
    {
      rtx frm = gen_int_mode (mode, SImode);

      if (mode == riscv_vector::FRM_DYN_CALL
	&& prev_mode != riscv_vector::FRM_DYN && STATIC_FRM_P (cfun))
	/* No need to emit when prev mode is DYN already.  */
	emit_insn (gen_fsrmsi_restore_volatile (backup_reg));
      else if (mode == riscv_vector::FRM_DYN_EXIT && STATIC_FRM_P (cfun)
	&& prev_mode != riscv_vector::FRM_DYN
	&& prev_mode != riscv_vector::FRM_DYN_CALL)
	/* No need to emit when prev mode is DYN or DYN_CALL already.  */
	emit_insn (gen_fsrmsi_restore_volatile (backup_reg));
      else if (mode == riscv_vector::FRM_DYN
	&& prev_mode != riscv_vector::FRM_DYN_CALL)
	/* Restore frm value from backup when switch to DYN mode.  */
	emit_insn (gen_fsrmsi_restore (backup_reg));
      else if (riscv_static_frm_mode_p (mode))
	/* Set frm value when switch to static mode.  */
	emit_insn (gen_fsrmsi_restore (frm));
    }
}

/* Implement Mode switching.  */

static void
riscv_emit_mode_set (int entity, int mode, int prev_mode,
		     HARD_REG_SET regs_live ATTRIBUTE_UNUSED)
{
  switch (entity)
    {
    case RISCV_VXRM:
      if (mode != VXRM_MODE_NONE && mode != prev_mode)
	emit_insn (gen_vxrmsi (gen_int_mode (mode, SImode)));
      break;
    case RISCV_FRM:
      riscv_emit_frm_mode_set (mode, prev_mode);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Adjust the FRM_NONE insn after a call to FRM_DYN for the
   underlying emit.  */

static int
riscv_frm_adjust_mode_after_call (rtx_insn *cur_insn, int mode)
{
  rtx_insn *insn = prev_nonnote_nondebug_insn_bb (cur_insn);

  if (insn && CALL_P (insn))
    return riscv_vector::FRM_DYN;

  return mode;
}

/* Insert the backup frm insn to the end of the bb if and only if the call
   is the last insn of this bb.  */

static void
riscv_frm_emit_after_bb_end (rtx_insn *cur_insn)
{
  edge eg;
  bool abnormal_edge_p = false;
  edge_iterator eg_iterator;
  basic_block bb = BLOCK_FOR_INSN (cur_insn);

  FOR_EACH_EDGE (eg, eg_iterator, bb->succs)
    {
      if (eg->flags & EDGE_ABNORMAL)
	abnormal_edge_p = true;
      else
	{
	  start_sequence ();
	  emit_insn (gen_frrmsi (DYNAMIC_FRM_RTL (cfun)));
	  rtx_insn *backup_insn = get_insns ();
	  end_sequence ();

	  insert_insn_on_edge (backup_insn, eg);
	}
    }

  if (abnormal_edge_p)
    {
      start_sequence ();
      emit_insn (gen_frrmsi (DYNAMIC_FRM_RTL (cfun)));
      rtx_insn *backup_insn = get_insns ();
      end_sequence ();

      insert_insn_end_basic_block (backup_insn, bb);
    }

  commit_edge_insertions ();
}

/* Return mode that frm must be switched into
   prior to the execution of insn.  */

static int
riscv_frm_mode_needed (rtx_insn *cur_insn, int code)
{
  if (!DYNAMIC_FRM_RTL(cfun))
    {
      /* The dynamic frm will be initialized only onece during cfun.  */
      DYNAMIC_FRM_RTL (cfun) = gen_reg_rtx (SImode);
      emit_insn_at_entry (gen_frrmsi (DYNAMIC_FRM_RTL (cfun)));
    }

  if (CALL_P (cur_insn))
    {
      rtx_insn *insn = next_nonnote_nondebug_insn_bb (cur_insn);

      if (!insn)
	riscv_frm_emit_after_bb_end (cur_insn);

      return riscv_vector::FRM_DYN_CALL;
    }

  int mode = code >= 0 ? get_attr_frm_mode (cur_insn) : riscv_vector::FRM_NONE;

  if (mode == riscv_vector::FRM_NONE)
      /* After meet a call, we need to backup the frm because it may be
	 updated during the call. Here, for each insn, we will check if
	 the previous insn is a call or not. When previous insn is call,
	 there will be 2 cases for the emit mode set.

	 1. Current insn is not MODE_NONE, then the mode switch framework
	    will do the mode switch from MODE_CALL to MODE_NONE natively.
	 2. Current insn is MODE_NONE, we need to adjust the MODE_NONE to
	    the MODE_DYN, and leave the mode switch itself to perform
	    the emit mode set.
       */
    mode = riscv_frm_adjust_mode_after_call (cur_insn, mode);

  return mode;
}

/* Return mode that entity must be switched into
   prior to the execution of insn.  */

static int
riscv_mode_needed (int entity, rtx_insn *insn, HARD_REG_SET)
{
  int code = recog_memoized (insn);

  switch (entity)
    {
    case RISCV_VXRM:
      return code >= 0 ? get_attr_vxrm_mode (insn) : VXRM_MODE_NONE;
    case RISCV_FRM:
      return riscv_frm_mode_needed (insn, code);
    default:
      gcc_unreachable ();
    }
}

/* Return TRUE that an insn is asm.  */

static bool
asm_insn_p (rtx_insn *insn)
{
  extract_insn (insn);

  return recog_data.is_asm;
}

/* Return TRUE that an insn is unknown for VXRM.  */

static bool
vxrm_unknown_p (rtx_insn *insn)
{
  /* Return true if there is a definition of VXRM.  */
  if (reg_set_p (gen_rtx_REG (SImode, VXRM_REGNUM), insn))
    return true;

  /* A CALL function may contain an instruction that modifies the VXRM,
     return true in this situation.  */
  if (CALL_P (insn))
    return true;

  /* Return true for all assembly since users may hardcode a assembly
     like this: asm volatile ("csrwi vxrm, 0").  */
  if (asm_insn_p (insn))
    return true;

  return false;
}

/* Return TRUE that an insn is unknown dynamic for FRM.  */

static bool
frm_unknown_dynamic_p (rtx_insn *insn)
{
  /* Return true if there is a definition of FRM.  */
  if (reg_set_p (gen_rtx_REG (SImode, FRM_REGNUM), insn))
    return true;

  return false;
}

/* Return the mode that an insn results in for VXRM.  */

static int
riscv_vxrm_mode_after (rtx_insn *insn, int mode)
{
  if (vxrm_unknown_p (insn))
    return VXRM_MODE_NONE;

  if (recog_memoized (insn) < 0)
    return mode;

  if (reg_mentioned_p (gen_rtx_REG (SImode, VXRM_REGNUM), PATTERN (insn)))
    return get_attr_vxrm_mode (insn);
  else
    return mode;
}

/* Return the mode that an insn results in for FRM.  */

static int
riscv_frm_mode_after (rtx_insn *insn, int mode)
{
  STATIC_FRM_P (cfun) = STATIC_FRM_P (cfun) || riscv_static_frm_mode_p (mode);

  if (CALL_P (insn))
    return mode;

  if (frm_unknown_dynamic_p (insn))
    return riscv_vector::FRM_DYN;

  if (recog_memoized (insn) < 0)
    return mode;

  if (reg_mentioned_p (gen_rtx_REG (SImode, FRM_REGNUM), PATTERN (insn)))
    return get_attr_frm_mode (insn);
  else
    return mode;
}

/* Return the mode that an insn results in.  */

static int
riscv_mode_after (int entity, int mode, rtx_insn *insn, HARD_REG_SET)
{
  switch (entity)
    {
    case RISCV_VXRM:
      return riscv_vxrm_mode_after (insn, mode);
    case RISCV_FRM:
      return riscv_frm_mode_after (insn, mode);
    default:
      gcc_unreachable ();
    }
}

/* Return a mode that ENTITY is assumed to be
   switched to at function entry.  */

static int
riscv_mode_entry (int entity)
{
  switch (entity)
    {
    case RISCV_VXRM:
      return VXRM_MODE_NONE;
    case RISCV_FRM:
      {
	  /* According to RVV 1.0 spec, all vector floating-point operations use
	     the dynamic rounding mode in the frm register.  Likewise in other
	     similar places.  */
	return riscv_vector::FRM_DYN;
      }
    default:
      gcc_unreachable ();
    }
}

/* Return a mode that ENTITY is assumed to be
   switched to at function exit.  */

static int
riscv_mode_exit (int entity)
{
  switch (entity)
    {
    case RISCV_VXRM:
      return VXRM_MODE_NONE;
    case RISCV_FRM:
      return riscv_vector::FRM_DYN_EXIT;
    default:
      gcc_unreachable ();
    }
}

static int
riscv_mode_priority (int, int n)
{
  return n;
}

/* Implement TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES.  */
unsigned int
riscv_autovectorize_vector_modes (vector_modes *modes, bool all)
{
  if (TARGET_VECTOR && !TARGET_XTHEADVECTOR)
    return riscv_vector::autovectorize_vector_modes (modes, all);

  return default_autovectorize_vector_modes (modes, all);
}

/* Implement TARGET_VECTORIZE_RELATED_MODE.  */
opt_machine_mode
riscv_vectorize_related_mode (machine_mode vector_mode, scalar_mode element_mode,
			      poly_uint64 nunits)
{
  if (TARGET_VECTOR)
    return riscv_vector::vectorize_related_mode (vector_mode, element_mode,
						 nunits);
  return default_vectorize_related_mode (vector_mode, element_mode, nunits);
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
riscv_vectorize_vec_perm_const (machine_mode vmode, machine_mode op_mode,
				rtx target, rtx op0, rtx op1,
				const vec_perm_indices &sel)
{
  if (TARGET_VECTOR && riscv_v_ext_mode_p (vmode))
    return riscv_vector::expand_vec_perm_const (vmode, op_mode, target, op0,
						op1, sel);

  return false;
}

static bool
riscv_frame_pointer_required (void)
{
  return riscv_save_frame_pointer && !crtl->is_leaf;
}

/* Return the appropriate common costs according to VECTYPE from COSTS.  */
static const common_vector_cost *
get_common_costs (const cpu_vector_cost *costs, tree vectype)
{
  gcc_assert (costs);

  if (vectype && riscv_v_ext_vls_mode_p (TYPE_MODE (vectype)))
    return costs->vls;
  return costs->vla;
}

/* Return the CPU vector costs according to -mtune if tune info has non-NULL
   vector cost.  Otherwide, return the default generic vector costs.  */
const cpu_vector_cost *
get_vector_costs ()
{
  const cpu_vector_cost *costs = tune_param->vec_costs;
  if (!costs)
    return &generic_vector_cost;
  return costs;
}

/* Implement targetm.vectorize.builtin_vectorization_cost.  */

static int
riscv_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				  tree vectype, int misalign ATTRIBUTE_UNUSED)
{
  const cpu_vector_cost *costs = get_vector_costs ();
  bool fp = false;

  if (vectype != NULL)
    fp = FLOAT_TYPE_P (vectype);

  const common_vector_cost *common_costs = get_common_costs (costs, vectype);
  gcc_assert (common_costs != NULL);
  switch (type_of_cost)
    {
    case scalar_stmt:
      return fp ? costs->scalar_fp_stmt_cost : costs->scalar_int_stmt_cost;

    case scalar_load:
      return costs->scalar_load_cost;

    case scalar_store:
      return costs->scalar_store_cost;

    case vector_stmt:
      return fp ? common_costs->fp_stmt_cost : common_costs->int_stmt_cost;

    case vector_load:
      return common_costs->align_load_cost;

    case vector_store:
      return common_costs->align_store_cost;

    case vec_to_scalar:
      return common_costs->vec_to_scalar_cost;

    case scalar_to_vec:
      return common_costs->scalar_to_vec_cost;

    case unaligned_load:
      return common_costs->unalign_load_cost;
    case vector_gather_load:
      return common_costs->gather_load_cost;

    case unaligned_store:
      return common_costs->unalign_store_cost;
    case vector_scatter_store:
      return common_costs->scatter_store_cost;

    case cond_branch_taken:
      return costs->cond_taken_branch_cost;

    case cond_branch_not_taken:
      return costs->cond_not_taken_branch_cost;

    case vec_perm:
      return common_costs->permute_cost;

    case vec_promote_demote:
      return fp ? common_costs->fp_stmt_cost : common_costs->int_stmt_cost;

    case vec_construct:
      return estimated_poly_value (TYPE_VECTOR_SUBPARTS (vectype));

    default:
      gcc_unreachable ();
    }

  return default_builtin_vectorization_cost (type_of_cost, vectype, misalign);
}

/* Implement targetm.vectorize.create_costs.  */

static vector_costs *
riscv_vectorize_create_costs (vec_info *vinfo, bool costing_for_scalar)
{
  if (TARGET_VECTOR)
    return new riscv_vector::costs (vinfo, costing_for_scalar);
  /* Default vector costs.  */
  return new vector_costs (vinfo, costing_for_scalar);
}

/* Implement TARGET_PREFERRED_ELSE_VALUE.  */

static tree
riscv_preferred_else_value (unsigned ifn, tree vectype, unsigned int nops,
			    tree *ops)
{
  if (riscv_v_ext_mode_p (TYPE_MODE (vectype)))
    {
      tree tmp_var = create_tmp_var (vectype);
      TREE_NO_WARNING (tmp_var) = 1;
      return get_or_create_ssa_default_def (cfun, tmp_var);
    }

  return default_preferred_else_value (ifn, vectype, nops, ops);
}

/* If MEM is in the form of "base+offset", extract the two parts
   of address and set to BASE and OFFSET, otherwise return false
   after clearing BASE and OFFSET.  */

bool
extract_base_offset_in_addr (rtx mem, rtx *base, rtx *offset)
{
  rtx addr;

  gcc_assert (MEM_P (mem));

  addr = XEXP (mem, 0);

  if (REG_P (addr))
    {
      *base = addr;
      *offset = const0_rtx;
      return true;
    }

  if (GET_CODE (addr) == PLUS
      && REG_P (XEXP (addr, 0)) && CONST_INT_P (XEXP (addr, 1)))
    {
      *base = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return true;
    }

  *base = NULL_RTX;
  *offset = NULL_RTX;

  return false;
}

/* Implements target hook vector_mode_supported_any_target_p.  */

static bool
riscv_vector_mode_supported_any_target_p (machine_mode)
{
  if (TARGET_XTHEADVECTOR)
    return false;
  return true;
}

/* Implements hook TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
riscv_function_value_regno_p (const unsigned regno)
{
  if (GP_RETURN_FIRST <= regno && regno <= GP_RETURN_LAST)
    return true;

  if (FP_RETURN_FIRST <= regno && regno <= FP_RETURN_LAST)
    return true;

  if (TARGET_VECTOR && regno == V_RETURN)
    return true;

  return false;
}

/* Implements hook TARGET_GET_RAW_RESULT_MODE.  */

static fixed_size_mode
riscv_get_raw_result_mode (int regno)
{
  if (!is_a <fixed_size_mode> (reg_raw_mode[regno]))
    return as_a <fixed_size_mode> (VOIDmode);

  return default_get_reg_raw_mode (regno);
}

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.dword\t"

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE riscv_option_override

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE riscv_option_restore

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P riscv_option_valid_attribute_p

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS riscv_legitimize_address

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE riscv_issue_rate
#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P riscv_macro_fusion_p
#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P riscv_macro_fusion_pair_p

#undef  TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE riscv_sched_variable_issue

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST riscv_sched_adjust_cost

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL riscv_function_ok_for_sibcall

#undef  TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION riscv_set_current_function

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST riscv_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST riscv_memory_move_cost
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS riscv_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST riscv_address_cost
#undef TARGET_INSN_COST
#define TARGET_INSN_COST riscv_insn_cost

#undef TARGET_MAX_NOCE_IFCVT_SEQ_COST
#define TARGET_MAX_NOCE_IFCVT_SEQ_COST riscv_max_noce_ifcvt_seq_cost
#undef TARGET_NOCE_CONVERSION_PROFITABLE_P
#define TARGET_NOCE_CONVERSION_PROFITABLE_P riscv_noce_conversion_profitable_p

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START riscv_file_start
#undef TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE true
#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START riscv_va_start

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE riscv_promote_function_mode

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY riscv_return_in_memory

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK riscv_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND riscv_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS riscv_print_operand_address
#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P riscv_print_operand_punct_valid_p

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS riscv_setup_incoming_varargs
#undef TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS riscv_allocate_stack_slots_for_args
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE riscv_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES riscv_arg_partial_bytes
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG riscv_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE riscv_function_arg_advance
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY riscv_function_arg_boundary
#undef TARGET_FNTYPE_ABI
#define TARGET_FNTYPE_ABI riscv_fntype_abi
#undef TARGET_INSN_CALLEE_ABI
#define TARGET_INSN_CALLEE_ABI riscv_insn_callee_abi

#undef TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS
#define TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS \
  riscv_get_separate_components

#undef TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB
#define TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB \
  riscv_components_for_bb

#undef TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS
#define TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS \
  riscv_disqualify_components

#undef TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS \
  riscv_emit_prologue_components

#undef TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS \
  riscv_emit_epilogue_components

#undef TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS
#define TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS \
  riscv_set_handled_components

/* The generic ELF target does not always have TLS support.  */
#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM riscv_cannot_force_const_mem

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P riscv_legitimate_constant_p

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P riscv_use_blocks_for_constant_p

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P	riscv_legitimate_address_p

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE riscv_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE riscv_conditional_register_usage

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS riscv_class_max_nregs

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT riscv_trampoline_init

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P riscv_in_small_data_p

#undef TARGET_HAVE_SRODATA_SECTION
#define TARGET_HAVE_SRODATA_SECTION true

#undef TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION riscv_select_section

#undef TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION riscv_unique_section

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION  riscv_elf_select_rtx_section

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET (-IMM_REACH/2)

#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET (IMM_REACH/2-1)

#undef TARGET_REGISTER_PRIORITY
#define TARGET_REGISTER_PRIORITY riscv_register_priority

#undef TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P riscv_cannot_copy_insn_p

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV riscv_atomic_assign_expand_fenv

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS riscv_init_builtins

#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL riscv_builtin_decl

#undef TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN riscv_gimple_fold_builtin

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN riscv_expand_builtin

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS riscv_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK riscv_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P riscv_modes_tieable_p

#undef TARGET_SLOW_UNALIGNED_ACCESS
#define TARGET_SLOW_UNALIGNED_ACCESS riscv_slow_unaligned_access

#undef TARGET_SECONDARY_MEMORY_NEEDED
#define TARGET_SECONDARY_MEMORY_NEEDED riscv_secondary_memory_needed

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS riscv_can_change_mode_class

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT riscv_constant_alignment

#undef TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES riscv_merge_decl_attributes

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE riscv_attribute_table

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN riscv_warn_func_return

/* The low bit is ignored by jump instructions so is safe to use.  */
#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 1

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG riscv_reorg

#undef TARGET_NEW_ADDRESS_PROFITABLE_P
#define TARGET_NEW_ADDRESS_PROFITABLE_P riscv_new_address_profitable_p

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE riscv_mangle_type

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P riscv_scalar_mode_supported_p

#undef TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P
#define TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P                                \
  riscv_libgcc_floating_mode_supported_p

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS riscv_init_libfuncs

#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION riscv_excess_precision

#undef TARGET_FLOATN_MODE
#define TARGET_FLOATN_MODE riscv_floatn_mode

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET riscv_asan_shadow_offset

#ifdef TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_ENDIAN)
#endif

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P riscv_vector_mode_supported_p

#undef TARGET_VERIFY_TYPE_CONTEXT
#define TARGET_VERIFY_TYPE_CONTEXT riscv_verify_type_context

#undef TARGET_ESTIMATED_POLY_VALUE
#define TARGET_ESTIMATED_POLY_VALUE riscv_estimated_poly_value

#undef TARGET_VECTORIZE_GET_MASK_MODE
#define TARGET_VECTORIZE_GET_MASK_MODE riscv_get_mask_mode

#undef TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE
#define TARGET_VECTORIZE_EMPTY_MASK_IS_EXPENSIVE riscv_empty_mask_is_expensive

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT riscv_vector_alignment

#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT riscv_support_vector_misalignment

#undef TARGET_DWARF_POLY_INDETERMINATE_VALUE
#define TARGET_DWARF_POLY_INDETERMINATE_VALUE riscv_dwarf_poly_indeterminate_value

#undef TARGET_ZERO_CALL_USED_REGS
#define TARGET_ZERO_CALL_USED_REGS riscv_zero_call_used_regs

#undef TARGET_ARRAY_MODE
#define TARGET_ARRAY_MODE riscv_array_mode

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE riscv_preferred_simd_mode

#undef TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT
#define TARGET_VECTORIZE_PREFERRED_VECTOR_ALIGNMENT \
  riscv_vectorize_preferred_vector_alignment

/* Mode switching hooks.  */

#undef TARGET_MODE_EMIT
#define TARGET_MODE_EMIT riscv_emit_mode_set
#undef TARGET_MODE_NEEDED
#define TARGET_MODE_NEEDED riscv_mode_needed
#undef TARGET_MODE_AFTER
#define TARGET_MODE_AFTER riscv_mode_after
#undef TARGET_MODE_ENTRY
#define TARGET_MODE_ENTRY riscv_mode_entry
#undef TARGET_MODE_EXIT
#define TARGET_MODE_EXIT riscv_mode_exit
#undef TARGET_MODE_PRIORITY
#define TARGET_MODE_PRIORITY riscv_mode_priority

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES \
  riscv_autovectorize_vector_modes

#undef TARGET_VECTORIZE_RELATED_MODE
#define TARGET_VECTORIZE_RELATED_MODE riscv_vectorize_related_mode

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST riscv_vectorize_vec_perm_const

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED riscv_frame_pointer_required

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  riscv_builtin_vectorization_cost

#undef TARGET_VECTORIZE_CREATE_COSTS
#define TARGET_VECTORIZE_CREATE_COSTS riscv_vectorize_create_costs

#undef TARGET_PREFERRED_ELSE_VALUE
#define TARGET_PREFERRED_ELSE_VALUE riscv_preferred_else_value

#undef TARGET_VECTOR_MODE_SUPPORTED_ANY_TARGET_P
#define TARGET_VECTOR_MODE_SUPPORTED_ANY_TARGET_P riscv_vector_mode_supported_any_target_p

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P riscv_function_value_regno_p

#undef TARGET_GET_RAW_RESULT_MODE
#define TARGET_GET_RAW_RESULT_MODE riscv_get_raw_result_mode

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-riscv.h"
