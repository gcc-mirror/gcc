/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2015 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "insn-codes.h"
#include "rtl.h"
#include "insn-attr.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "tree.h"
#include "fold-const.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "regs.h"
#include "dominance.h"
#include "cfg.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "predict.h"
#include "basic-block.h"
#include "df.h"
#include "hard-reg-set.h"
#include "output.h"
#include "hashtab.h"
#include "function.h"
#include "flags.h"
#include "statistics.h"
#include "real.h"
#include "fixed-value.h"
#include "insn-config.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "emit-rtl.h"
#include "stmt.h"
#include "expr.h"
#include "reload.h"
#include "toplev.h"
#include "target.h"
#include "target-def.h"
#include "targhooks.h"
#include "ggc.h"
#include "tm_p.h"
#include "recog.h"
#include "langhooks.h"
#include "diagnostic-core.h"
#include "hash-table.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "optabs.h"
#include "dwarf2.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "aarch64-cost-tables.h"
#include "dumpfile.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tm-constrs.h"
#include "sched-int.h"

/* Defined for convenience.  */
#define POINTER_BYTES (POINTER_SIZE / BITS_PER_UNIT)

/* Classifies an address.

   ADDRESS_REG_IMM
       A simple base register plus immediate offset.

   ADDRESS_REG_WB
       A base register indexed by immediate offset with writeback.

   ADDRESS_REG_REG
       A base register indexed by (optionally scaled) register.

   ADDRESS_REG_UXTW
       A base register indexed by (optionally scaled) zero-extended register.

   ADDRESS_REG_SXTW
       A base register indexed by (optionally scaled) sign-extended register.

   ADDRESS_LO_SUM
       A LO_SUM rtx with a base register and "LO12" symbol relocation.

   ADDRESS_SYMBOLIC:
       A constant symbolic address, in pc-relative literal pool.  */

enum aarch64_address_type {
  ADDRESS_REG_IMM,
  ADDRESS_REG_WB,
  ADDRESS_REG_REG,
  ADDRESS_REG_UXTW,
  ADDRESS_REG_SXTW,
  ADDRESS_LO_SUM,
  ADDRESS_SYMBOLIC
};

struct aarch64_address_info {
  enum aarch64_address_type type;
  rtx base;
  rtx offset;
  int shift;
  enum aarch64_symbol_type symbol_type;
};

struct simd_immediate_info
{
  rtx value;
  int shift;
  int element_width;
  bool mvn;
  bool msl;
};

/* The current code model.  */
enum aarch64_code_model aarch64_cmodel;

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS 1
#endif

static bool aarch64_composite_type_p (const_tree, machine_mode);
static bool aarch64_vfp_is_call_or_return_candidate (machine_mode,
						     const_tree,
						     machine_mode *, int *,
						     bool *);
static void aarch64_elf_asm_constructor (rtx, int) ATTRIBUTE_UNUSED;
static void aarch64_elf_asm_destructor (rtx, int) ATTRIBUTE_UNUSED;
static void aarch64_override_options_after_change (void);
static bool aarch64_vector_mode_supported_p (machine_mode);
static unsigned bit_count (unsigned HOST_WIDE_INT);
static bool aarch64_vectorize_vec_perm_const_ok (machine_mode vmode,
						 const unsigned char *sel);
static int aarch64_address_cost (rtx, machine_mode, addr_space_t, bool);

/* Major revision number of the ARM Architecture implemented by the target.  */
unsigned aarch64_architecture_version;

/* The processor for which instructions should be scheduled.  */
enum aarch64_processor aarch64_tune = cortexa53;

/* The current tuning set.  */
const struct tune_params *aarch64_tune_params;

/* Mask to specify which instructions we are allowed to generate.  */
unsigned long aarch64_isa_flags = 0;

/* Mask to specify which instruction scheduling options should be used.  */
unsigned long aarch64_tune_flags = 0;

/* Tuning parameters.  */

static const struct cpu_addrcost_table generic_addrcost_table =
{
    {
      0, /* hi  */
      0, /* si  */
      0, /* di  */
      0, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  0, /* register_offset  */
  0, /* register_extend  */
  0 /* imm_offset  */
};

static const struct cpu_addrcost_table cortexa57_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  0, /* register_offset  */
  0, /* register_extend  */
  0, /* imm_offset  */
};

static const struct cpu_addrcost_table xgene1_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  1, /* pre_modify  */
  0, /* post_modify  */
  0, /* register_offset  */
  1, /* register_extend  */
  0, /* imm_offset  */
};

static const struct cpu_regmove_cost generic_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const struct cpu_regmove_cost cortexa57_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const struct cpu_regmove_cost cortexa53_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const struct cpu_regmove_cost thunderx_regmove_cost =
{
  2, /* GP2GP  */
  2, /* GP2FP  */
  6, /* FP2GP  */
  4 /* FP2FP  */
};

static const struct cpu_regmove_cost xgene1_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  8, /* GP2FP  */
  8, /* FP2GP  */
  2 /* FP2FP  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost generic_vector_cost =
{
  1, /* scalar_stmt_cost  */
  1, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* vec_stmt_cost  */
  1, /* vec_to_scalar_cost  */
  1, /* scalar_to_vec_cost  */
  1, /* vec_align_load_cost  */
  1, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  3, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost cortexa57_vector_cost =
{
  1, /* scalar_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  3, /* vec_stmt_cost  */
  8, /* vec_to_scalar_cost  */
  8, /* scalar_to_vec_cost  */
  5, /* vec_align_load_cost  */
  5, /* vec_unalign_load_cost  */
  1, /* vec_unalign_store_cost  */
  1, /* vec_store_cost  */
  1, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

/* Generic costs for vector insn classes.  */
static const struct cpu_vector_cost xgene1_vector_cost =
{
  1, /* scalar_stmt_cost  */
  5, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  2, /* vec_stmt_cost  */
  4, /* vec_to_scalar_cost  */
  4, /* scalar_to_vec_cost  */
  10, /* vec_align_load_cost  */
  10, /* vec_unalign_load_cost  */
  2, /* vec_unalign_store_cost  */
  2, /* vec_store_cost  */
  2, /* cond_taken_branch_cost  */
  1 /* cond_not_taken_branch_cost  */
};

#define AARCH64_FUSE_NOTHING	(0)
#define AARCH64_FUSE_MOV_MOVK	(1 << 0)
#define AARCH64_FUSE_ADRP_ADD	(1 << 1)
#define AARCH64_FUSE_MOVK_MOVK	(1 << 2)
#define AARCH64_FUSE_ADRP_LDR	(1 << 3)
#define AARCH64_FUSE_CMP_BRANCH	(1 << 4)

static const struct tune_params generic_tunings =
{
  &cortexa57_extra_costs,
  &generic_addrcost_table,
  &generic_regmove_cost,
  &generic_vector_cost,
  4, /* memmov_cost  */
  2, /* issue_rate  */
  AARCH64_FUSE_NOTHING, /* fuseable_ops  */
  8,	/* function_align.  */
  8,	/* jump_align.  */
  4,	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1	/* vec_reassoc_width.  */
};

static const struct tune_params cortexa53_tunings =
{
  &cortexa53_extra_costs,
  &generic_addrcost_table,
  &cortexa53_regmove_cost,
  &generic_vector_cost,
  4, /* memmov_cost  */
  2, /* issue_rate  */
  (AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK | AARCH64_FUSE_ADRP_LDR), /* fuseable_ops  */
  8,	/* function_align.  */
  8,	/* jump_align.  */
  4,	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1	/* vec_reassoc_width.  */
};

static const struct tune_params cortexa57_tunings =
{
  &cortexa57_extra_costs,
  &cortexa57_addrcost_table,
  &cortexa57_regmove_cost,
  &cortexa57_vector_cost,
  4, /* memmov_cost  */
  3, /* issue_rate  */
  (AARCH64_FUSE_MOV_MOVK | AARCH64_FUSE_ADRP_ADD
   | AARCH64_FUSE_MOVK_MOVK), /* fuseable_ops  */
  16,	/* function_align.  */
  8,	/* jump_align.  */
  4,	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1	/* vec_reassoc_width.  */
};

static const struct tune_params thunderx_tunings =
{
  &thunderx_extra_costs,
  &generic_addrcost_table,
  &thunderx_regmove_cost,
  &generic_vector_cost,
  6, /* memmov_cost  */
  2, /* issue_rate  */
  AARCH64_FUSE_CMP_BRANCH, /* fuseable_ops  */
  8,	/* function_align.  */
  8,	/* jump_align.  */
  8,	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1	/* vec_reassoc_width.  */
};

static const struct tune_params xgene1_tunings =
{
  &xgene1_extra_costs,
  &xgene1_addrcost_table,
  &xgene1_regmove_cost,
  &xgene1_vector_cost,
  6, /* memmov_cost  */
  4, /* issue_rate  */
  AARCH64_FUSE_NOTHING, /* fuseable_ops  */
  16,	/* function_align.  */
  8,	/* jump_align.  */
  16,	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1	/* vec_reassoc_width.  */
};

/* A processor implementing AArch64.  */
struct processor
{
  const char *const name;
  enum aarch64_processor core;
  const char *arch;
  unsigned architecture_version;
  const unsigned long flags;
  const struct tune_params *const tune;
};

/* Processor cores implementing AArch64.  */
static const struct processor all_cores[] =
{
#define AARCH64_CORE(NAME, IDENT, SCHED, ARCH, FLAGS, COSTS) \
  {NAME, SCHED, #ARCH, ARCH, FLAGS, &COSTS##_tunings},
#include "aarch64-cores.def"
#undef AARCH64_CORE
  {"generic", cortexa53, "8", 8, AARCH64_FL_FOR_ARCH8, &generic_tunings},
  {NULL, aarch64_none, NULL, 0, 0, NULL}
};

/* Architectures implementing AArch64.  */
static const struct processor all_architectures[] =
{
#define AARCH64_ARCH(NAME, CORE, ARCH, FLAGS) \
  {NAME, CORE, #ARCH, ARCH, FLAGS, NULL},
#include "aarch64-arches.def"
#undef AARCH64_ARCH
  {NULL, aarch64_none, NULL, 0, 0, NULL}
};

/* Target specification.  These are populated as commandline arguments
   are processed, or NULL if not specified.  */
static const struct processor *selected_arch;
static const struct processor *selected_cpu;
static const struct processor *selected_tune;

#define AARCH64_CPU_DEFAULT_FLAGS ((selected_cpu) ? selected_cpu->flags : 0)

/* An ISA extension in the co-processor and main instruction set space.  */
struct aarch64_option_extension
{
  const char *const name;
  const unsigned long flags_on;
  const unsigned long flags_off;
};

/* ISA extensions in AArch64.  */
static const struct aarch64_option_extension all_extensions[] =
{
#define AARCH64_OPT_EXTENSION(NAME, FLAGS_ON, FLAGS_OFF) \
  {NAME, FLAGS_ON, FLAGS_OFF},
#include "aarch64-option-extensions.def"
#undef AARCH64_OPT_EXTENSION
  {NULL, 0, 0}
};

/* Used to track the size of an address when generating a pre/post
   increment address.  */
static machine_mode aarch64_memory_reference_mode;

/* A table of valid AArch64 "bitmask immediate" values for
   logical instructions.  */

#define AARCH64_NUM_BITMASKS  5334
static unsigned HOST_WIDE_INT aarch64_bitmasks[AARCH64_NUM_BITMASKS];

typedef enum aarch64_cond_code
{
  AARCH64_EQ = 0, AARCH64_NE, AARCH64_CS, AARCH64_CC, AARCH64_MI, AARCH64_PL,
  AARCH64_VS, AARCH64_VC, AARCH64_HI, AARCH64_LS, AARCH64_GE, AARCH64_LT,
  AARCH64_GT, AARCH64_LE, AARCH64_AL, AARCH64_NV
}
aarch64_cc;

#define AARCH64_INVERSE_CONDITION_CODE(X) ((aarch64_cc) (((int) X) ^ 1))

/* The condition codes of the processor, and the inverse function.  */
static const char * const aarch64_condition_codes[] =
{
  "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
  "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
};

static unsigned int
aarch64_min_divisions_for_recip_mul (enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return 2;
}

static int
aarch64_reassociation_width (unsigned opc ATTRIBUTE_UNUSED,
			     enum machine_mode mode)
{
  if (VECTOR_MODE_P (mode))
    return aarch64_tune_params->vec_reassoc_width;
  if (INTEGRAL_MODE_P (mode))
    return aarch64_tune_params->int_reassoc_width;
  if (FLOAT_MODE_P (mode))
    return aarch64_tune_params->fp_reassoc_width;
  return 1;
}

/* Provide a mapping from gcc register numbers to dwarf register numbers.  */
unsigned
aarch64_dbx_register_number (unsigned regno)
{
   if (GP_REGNUM_P (regno))
     return AARCH64_DWARF_R0 + regno - R0_REGNUM;
   else if (regno == SP_REGNUM)
     return AARCH64_DWARF_SP;
   else if (FP_REGNUM_P (regno))
     return AARCH64_DWARF_V0 + regno - V0_REGNUM;

   /* Return values >= DWARF_FRAME_REGISTERS indicate that there is no
      equivalent DWARF register.  */
   return DWARF_FRAME_REGISTERS;
}

/* Return TRUE if MODE is any of the large INT modes.  */
static bool
aarch64_vect_struct_mode_p (machine_mode mode)
{
  return mode == OImode || mode == CImode || mode == XImode;
}

/* Return TRUE if MODE is any of the vector modes.  */
static bool
aarch64_vector_mode_p (machine_mode mode)
{
  return aarch64_vector_mode_supported_p (mode)
	 || aarch64_vect_struct_mode_p (mode);
}

/* Implement target hook TARGET_ARRAY_MODE_SUPPORTED_P.  */
static bool
aarch64_array_mode_supported_p (machine_mode mode,
				unsigned HOST_WIDE_INT nelems)
{
  if (TARGET_SIMD
      && AARCH64_VALID_SIMD_QREG_MODE (mode)
      && (nelems >= 2 && nelems <= 4))
    return true;

  return false;
}

/* Implement HARD_REGNO_NREGS.  */

int
aarch64_hard_regno_nregs (unsigned regno, machine_mode mode)
{
  switch (aarch64_regno_regclass (regno))
    {
    case FP_REGS:
    case FP_LO_REGS:
      return (GET_MODE_SIZE (mode) + UNITS_PER_VREG - 1) / UNITS_PER_VREG;
    default:
      return (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
    }
  gcc_unreachable ();
}

/* Implement HARD_REGNO_MODE_OK.  */

int
aarch64_hard_regno_mode_ok (unsigned regno, machine_mode mode)
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;

  if (regno == SP_REGNUM)
    /* The purpose of comparing with ptr_mode is to support the
       global register variable associated with the stack pointer
       register via the syntax of asm ("wsp") in ILP32.  */
    return mode == Pmode || mode == ptr_mode;

  if (regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    return mode == Pmode;

  if (GP_REGNUM_P (regno) && ! aarch64_vect_struct_mode_p (mode))
    return 1;

  if (FP_REGNUM_P (regno))
    {
      if (aarch64_vect_struct_mode_p (mode))
	return
	  (regno + aarch64_hard_regno_nregs (regno, mode) - 1) <= V31_REGNUM;
      else
	return 1;
    }

  return 0;
}

/* Implement HARD_REGNO_CALLER_SAVE_MODE.  */
machine_mode
aarch64_hard_regno_caller_save_mode (unsigned regno, unsigned nregs,
				     machine_mode mode)
{
  /* Handle modes that fit within single registers.  */
  if (nregs == 1 && GET_MODE_SIZE (mode) <= 16)
    {
      if (GET_MODE_SIZE (mode) >= 4)
        return mode;
      else
        return SImode;
    }
  /* Fall back to generic for multi-reg and very large modes.  */
  else
    return choose_hard_reg_mode (regno, nregs, false);
}

/* Return true if calls to DECL should be treated as
   long-calls (ie called via a register).  */
static bool
aarch64_decl_is_long_call_p (const_tree decl ATTRIBUTE_UNUSED)
{
  return false;
}

/* Return true if calls to symbol-ref SYM should be treated as
   long-calls (ie called via a register).  */
bool
aarch64_is_long_call_p (rtx sym)
{
  return aarch64_decl_is_long_call_p (SYMBOL_REF_DECL (sym));
}

/* Return true if the offsets to a zero/sign-extract operation
   represent an expression that matches an extend operation.  The
   operands represent the paramters from

   (extract:MODE (mult (reg) (MULT_IMM)) (EXTRACT_IMM) (const_int 0)).  */
bool
aarch64_is_extend_from_extract (machine_mode mode, rtx mult_imm,
				rtx extract_imm)
{
  HOST_WIDE_INT mult_val, extract_val;

  if (! CONST_INT_P (mult_imm) || ! CONST_INT_P (extract_imm))
    return false;

  mult_val = INTVAL (mult_imm);
  extract_val = INTVAL (extract_imm);

  if (extract_val > 8
      && extract_val < GET_MODE_BITSIZE (mode)
      && exact_log2 (extract_val & ~7) > 0
      && (extract_val & 7) <= 4
      && mult_val == (1 << (extract_val & 7)))
    return true;

  return false;
}

/* Emit an insn that's a simple single-set.  Both the operands must be
   known to be valid.  */
inline static rtx
emit_set_insn (rtx x, rtx y)
{
  return emit_insn (gen_rtx_SET (VOIDmode, x, y));
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  */
rtx
aarch64_gen_compare_reg (RTX_CODE code, rtx x, rtx y)
{
  machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx_REG (mode, CC_REGNUM);

  emit_set_insn (cc_reg, gen_rtx_COMPARE (mode, x, y));
  return cc_reg;
}

/* Build the SYMBOL_REF for __tls_get_addr.  */

static GTY(()) rtx tls_get_addr_libfunc;

rtx
aarch64_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}

/* Return the TLS model to use for ADDR.  */

static enum tls_model
tls_symbolic_operand_type (rtx addr)
{
  enum tls_model tls_kind = TLS_MODEL_NONE;
  rtx sym, addend;

  if (GET_CODE (addr) == CONST)
    {
      split_const (addr, &sym, &addend);
      if (GET_CODE (sym) == SYMBOL_REF)
	tls_kind = SYMBOL_REF_TLS_MODEL (sym);
    }
  else if (GET_CODE (addr) == SYMBOL_REF)
    tls_kind = SYMBOL_REF_TLS_MODEL (addr);

  return tls_kind;
}

/* We'll allow lo_sum's in addresses in our legitimate addresses
   so that combine would take care of combining addresses where
   necessary, but for generation purposes, we'll generate the address
   as :
   RTL                               Absolute
   tmp = hi (symbol_ref);            adrp  x1, foo
   dest = lo_sum (tmp, symbol_ref);  add dest, x1, :lo_12:foo
                                     nop

   PIC                               TLS
   adrp x1, :got:foo                 adrp tmp, :tlsgd:foo
   ldr  x1, [:got_lo12:foo]          add  dest, tmp, :tlsgd_lo12:foo
                                     bl   __tls_get_addr
                                     nop

   Load TLS symbol, depending on TLS mechanism and TLS access model.

   Global Dynamic - Traditional TLS:
   adrp tmp, :tlsgd:imm
   add  dest, tmp, #:tlsgd_lo12:imm
   bl   __tls_get_addr

   Global Dynamic - TLS Descriptors:
   adrp dest, :tlsdesc:imm
   ldr  tmp, [dest, #:tlsdesc_lo12:imm]
   add  dest, dest, #:tlsdesc_lo12:imm
   blr  tmp
   mrs  tp, tpidr_el0
   add  dest, dest, tp

   Initial Exec:
   mrs  tp, tpidr_el0
   adrp tmp, :gottprel:imm
   ldr  dest, [tmp, #:gottprel_lo12:imm]
   add  dest, dest, tp

   Local Exec:
   mrs  tp, tpidr_el0
   add  t0, tp, #:tprel_hi12:imm, lsl #12
   add  t0, t0, #:tprel_lo12_nc:imm
*/

static void
aarch64_load_symref_appropriately (rtx dest, rtx imm,
				   enum aarch64_symbol_type type)
{
  switch (type)
    {
    case SYMBOL_SMALL_ABSOLUTE:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode.  */
	rtx tmp_reg = dest;
	machine_mode mode = GET_MODE (dest);

	gcc_assert (mode == Pmode || mode == ptr_mode);

	if (can_create_pseudo_p ())
	  tmp_reg = gen_reg_rtx (mode);

	emit_move_insn (tmp_reg, gen_rtx_HIGH (mode, imm));
	emit_insn (gen_add_losym (dest, tmp_reg, imm));
	return;
      }

    case SYMBOL_TINY_ABSOLUTE:
      emit_insn (gen_rtx_SET (Pmode, dest, imm));
      return;

    case SYMBOL_SMALL_GOT:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode,
	   while the got entry is always of SImode size.  The mode of
	   dest depends on how dest is used: if dest is assigned to a
	   pointer (e.g. in the memory), it has SImode; it may have
	   DImode if dest is dereferenced to access the memeory.
	   This is why we have to handle three different ldr_got_small
	   patterns here (two patterns for ILP32).  */
	rtx tmp_reg = dest;
	machine_mode mode = GET_MODE (dest);

	if (can_create_pseudo_p ())
	  tmp_reg = gen_reg_rtx (mode);

	emit_move_insn (tmp_reg, gen_rtx_HIGH (mode, imm));
	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      emit_insn (gen_ldr_got_small_di (dest, tmp_reg, imm));
	    else
	      emit_insn (gen_ldr_got_small_si (dest, tmp_reg, imm));
	  }
	else
	  {
	    gcc_assert (mode == Pmode);
	    emit_insn (gen_ldr_got_small_sidi (dest, tmp_reg, imm));
	  }

	return;
      }

    case SYMBOL_SMALL_TLSGD:
      {
	rtx_insn *insns;
	rtx result = gen_rtx_REG (Pmode, R0_REGNUM);

	start_sequence ();
	aarch64_emit_call_insn (gen_tlsgd_small (result, imm));
	insns = get_insns ();
	end_sequence ();

	RTL_CONST_CALL_P (insns) = 1;
	emit_libcall_block (insns, dest, result, imm);
	return;
      }

    case SYMBOL_SMALL_TLSDESC:
      {
	machine_mode mode = GET_MODE (dest);
	rtx x0 = gen_rtx_REG (mode, R0_REGNUM);
	rtx tp;

	gcc_assert (mode == Pmode || mode == ptr_mode);

	/* In ILP32, the got entry is always of SImode size.  Unlike
	   small GOT, the dest is fixed at reg 0.  */
	if (TARGET_ILP32)
	  emit_insn (gen_tlsdesc_small_si (imm));
	else
	  emit_insn (gen_tlsdesc_small_di (imm));
	tp = aarch64_load_tp (NULL);

	if (mode != Pmode)
	  tp = gen_lowpart (mode, tp);

	emit_insn (gen_rtx_SET (mode, dest, gen_rtx_PLUS (mode, tp, x0)));
	set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_SMALL_GOTTPREL:
      {
	/* In ILP32, the mode of dest can be either SImode or DImode,
	   while the got entry is always of SImode size.  The mode of
	   dest depends on how dest is used: if dest is assigned to a
	   pointer (e.g. in the memory), it has SImode; it may have
	   DImode if dest is dereferenced to access the memeory.
	   This is why we have to handle three different tlsie_small
	   patterns here (two patterns for ILP32).  */
	machine_mode mode = GET_MODE (dest);
	rtx tmp_reg = gen_reg_rtx (mode);
	rtx tp = aarch64_load_tp (NULL);

	if (mode == ptr_mode)
	  {
	    if (mode == DImode)
	      emit_insn (gen_tlsie_small_di (tmp_reg, imm));
	    else
	      {
		emit_insn (gen_tlsie_small_si (tmp_reg, imm));
		tp = gen_lowpart (mode, tp);
	      }
	  }
	else
	  {
	    gcc_assert (mode == Pmode);
	    emit_insn (gen_tlsie_small_sidi (tmp_reg, imm));
	  }

	emit_insn (gen_rtx_SET (mode, dest, gen_rtx_PLUS (mode, tp, tmp_reg)));
	set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_SMALL_TPREL:
      {
	rtx tp = aarch64_load_tp (NULL);

	if (GET_MODE (dest) != Pmode)
	  tp = gen_lowpart (GET_MODE (dest), tp);

	emit_insn (gen_tlsle_small (dest, tp, imm));
	set_unique_reg_note (get_last_insn (), REG_EQUIV, imm);
	return;
      }

    case SYMBOL_TINY_GOT:
      emit_insn (gen_ldr_got_tiny (dest, imm));
      return;

    default:
      gcc_unreachable ();
    }
}

/* Emit a move from SRC to DEST.  Assume that the move expanders can
   handle all moves if !can_create_pseudo_p ().  The distinction is
   important because, unlike emit_move_insn, the move expanders know
   how to force Pmode objects into the constant pool even when the
   constant pool address is not itself legitimate.  */
static rtx
aarch64_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p ()
	  ? emit_move_insn (dest, src)
	  : emit_move_insn_1 (dest, src));
}

/* Split a 128-bit move operation into two 64-bit move operations,
   taking care to handle partial overlap of register to register
   copies.  Special cases are needed when moving between GP regs and
   FP regs.  SRC can be a register, constant or memory; DST a register
   or memory.  If either operand is memory it must not have any side
   effects.  */
void
aarch64_split_128bit_move (rtx dst, rtx src)
{
  rtx dst_lo, dst_hi;
  rtx src_lo, src_hi;

  machine_mode mode = GET_MODE (dst);

  gcc_assert (mode == TImode || mode == TFmode);
  gcc_assert (!(side_effects_p (src) || side_effects_p (dst)));
  gcc_assert (mode == GET_MODE (src) || GET_MODE (src) == VOIDmode);

  if (REG_P (dst) && REG_P (src))
    {
      int src_regno = REGNO (src);
      int dst_regno = REGNO (dst);

      /* Handle FP <-> GP regs.  */
      if (FP_REGNUM_P (dst_regno) && GP_REGNUM_P (src_regno))
	{
	  src_lo = gen_lowpart (word_mode, src);
	  src_hi = gen_highpart (word_mode, src);

	  if (mode == TImode)
	    {
	      emit_insn (gen_aarch64_movtilow_di (dst, src_lo));
	      emit_insn (gen_aarch64_movtihigh_di (dst, src_hi));
	    }
	  else
	    {
	      emit_insn (gen_aarch64_movtflow_di (dst, src_lo));
	      emit_insn (gen_aarch64_movtfhigh_di (dst, src_hi));
	    }
	  return;
	}
      else if (GP_REGNUM_P (dst_regno) && FP_REGNUM_P (src_regno))
	{
	  dst_lo = gen_lowpart (word_mode, dst);
	  dst_hi = gen_highpart (word_mode, dst);

	  if (mode == TImode)
	    {
	      emit_insn (gen_aarch64_movdi_tilow (dst_lo, src));
	      emit_insn (gen_aarch64_movdi_tihigh (dst_hi, src));
	    }
	  else
	    {
	      emit_insn (gen_aarch64_movdi_tflow (dst_lo, src));
	      emit_insn (gen_aarch64_movdi_tfhigh (dst_hi, src));
	    }
	  return;
	}
    }

  dst_lo = gen_lowpart (word_mode, dst);
  dst_hi = gen_highpart (word_mode, dst);
  src_lo = gen_lowpart (word_mode, src);
  src_hi = gen_highpart_mode (word_mode, mode, src);

  /* At most one pairing may overlap.  */
  if (reg_overlap_mentioned_p (dst_lo, src_hi))
    {
      aarch64_emit_move (dst_hi, src_hi);
      aarch64_emit_move (dst_lo, src_lo);
    }
  else
    {
      aarch64_emit_move (dst_lo, src_lo);
      aarch64_emit_move (dst_hi, src_hi);
    }
}

bool
aarch64_split_128bit_move_p (rtx dst, rtx src)
{
  return (! REG_P (src)
	  || ! (FP_REGNUM_P (REGNO (dst)) && FP_REGNUM_P (REGNO (src))));
}

/* Split a complex SIMD combine.  */

void
aarch64_split_simd_combine (rtx dst, rtx src1, rtx src2)
{
  machine_mode src_mode = GET_MODE (src1);
  machine_mode dst_mode = GET_MODE (dst);

  gcc_assert (VECTOR_MODE_P (dst_mode));

  if (REG_P (dst) && REG_P (src1) && REG_P (src2))
    {
      rtx (*gen) (rtx, rtx, rtx);

      switch (src_mode)
	{
	case V8QImode:
	  gen = gen_aarch64_simd_combinev8qi;
	  break;
	case V4HImode:
	  gen = gen_aarch64_simd_combinev4hi;
	  break;
	case V2SImode:
	  gen = gen_aarch64_simd_combinev2si;
	  break;
	case V2SFmode:
	  gen = gen_aarch64_simd_combinev2sf;
	  break;
	case DImode:
	  gen = gen_aarch64_simd_combinedi;
	  break;
	case DFmode:
	  gen = gen_aarch64_simd_combinedf;
	  break;
	default:
	  gcc_unreachable ();
	}

      emit_insn (gen (dst, src1, src2));
      return;
    }
}

/* Split a complex SIMD move.  */

void
aarch64_split_simd_move (rtx dst, rtx src)
{
  machine_mode src_mode = GET_MODE (src);
  machine_mode dst_mode = GET_MODE (dst);

  gcc_assert (VECTOR_MODE_P (dst_mode));

  if (REG_P (dst) && REG_P (src))
    {
      rtx (*gen) (rtx, rtx);

      gcc_assert (VECTOR_MODE_P (src_mode));

      switch (src_mode)
	{
	case V16QImode:
	  gen = gen_aarch64_split_simd_movv16qi;
	  break;
	case V8HImode:
	  gen = gen_aarch64_split_simd_movv8hi;
	  break;
	case V4SImode:
	  gen = gen_aarch64_split_simd_movv4si;
	  break;
	case V2DImode:
	  gen = gen_aarch64_split_simd_movv2di;
	  break;
	case V4SFmode:
	  gen = gen_aarch64_split_simd_movv4sf;
	  break;
	case V2DFmode:
	  gen = gen_aarch64_split_simd_movv2df;
	  break;
	default:
	  gcc_unreachable ();
	}

      emit_insn (gen (dst, src));
      return;
    }
}

bool
aarch64_zero_extend_const_eq (machine_mode xmode, rtx x,
			      machine_mode ymode, rtx y)
{
  rtx r = simplify_const_unary_operation (ZERO_EXTEND, xmode, y, ymode);
  gcc_assert (r != NULL);
  return rtx_equal_p (x, r);
}
			      

static rtx
aarch64_force_temporary (machine_mode mode, rtx x, rtx value)
{
  if (can_create_pseudo_p ())
    return force_reg (mode, value);
  else
    {
      x = aarch64_emit_move (x, value);
      return x;
    }
}


static rtx
aarch64_add_offset (machine_mode mode, rtx temp, rtx reg, HOST_WIDE_INT offset)
{
  if (!aarch64_plus_immediate (GEN_INT (offset), mode))
    {
      rtx high;
      /* Load the full offset into a register.  This
         might be improvable in the future.  */
      high = GEN_INT (offset);
      offset = 0;
      high = aarch64_force_temporary (mode, temp, high);
      reg = aarch64_force_temporary (mode, temp,
				     gen_rtx_PLUS (mode, high, reg));
    }
  return plus_constant (mode, reg, offset);
}

static int
aarch64_internal_mov_immediate (rtx dest, rtx imm, bool generate,
				machine_mode mode)
{
  unsigned HOST_WIDE_INT mask;
  int i;
  bool first;
  unsigned HOST_WIDE_INT val;
  bool subtargets;
  rtx subtarget;
  int one_match, zero_match, first_not_ffff_match;
  int num_insns = 0;

  if (CONST_INT_P (imm) && aarch64_move_imm (INTVAL (imm), mode))
    {
      if (generate)
	emit_insn (gen_rtx_SET (VOIDmode, dest, imm));
      num_insns++;
      return num_insns;
    }

  if (mode == SImode)
    {
      /* We know we can't do this in 1 insn, and we must be able to do it
	 in two; so don't mess around looking for sequences that don't buy
	 us anything.  */
      if (generate)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, dest,
				  GEN_INT (INTVAL (imm) & 0xffff)));
	  emit_insn (gen_insv_immsi (dest, GEN_INT (16),
				     GEN_INT ((INTVAL (imm) >> 16) & 0xffff)));
	}
      num_insns += 2;
      return num_insns;
    }

  /* Remaining cases are all for DImode.  */

  val = INTVAL (imm);
  subtargets = optimize && can_create_pseudo_p ();

  one_match = 0;
  zero_match = 0;
  mask = 0xffff;
  first_not_ffff_match = -1;

  for (i = 0; i < 64; i += 16, mask <<= 16)
    {
      if ((val & mask) == mask)
	one_match++;
      else
	{
	  if (first_not_ffff_match < 0)
	    first_not_ffff_match = i;
	  if ((val & mask) == 0)
	    zero_match++;
	}
    }

  if (one_match == 2)
    {
      /* Set one of the quarters and then insert back into result.  */
      mask = 0xffffll << first_not_ffff_match;
      if (generate)
	{
	  emit_insn (gen_rtx_SET (VOIDmode, dest, GEN_INT (val | mask)));
	  emit_insn (gen_insv_immdi (dest, GEN_INT (first_not_ffff_match),
				     GEN_INT ((val >> first_not_ffff_match)
					      & 0xffff)));
	}
      num_insns += 2;
      return num_insns;
    }

  if (zero_match == 2)
    goto simple_sequence;

  mask = 0x0ffff0000UL;
  for (i = 16; i < 64; i += 16, mask <<= 16)
    {
      HOST_WIDE_INT comp = mask & ~(mask - 1);

      if (aarch64_uimm12_shift (val - (val & mask)))
	{
	  if (generate)
	    {
	      subtarget = subtargets ? gen_reg_rtx (DImode) : dest;
	      emit_insn (gen_rtx_SET (VOIDmode, subtarget,
				      GEN_INT (val & mask)));
	      emit_insn (gen_adddi3 (dest, subtarget,
				     GEN_INT (val - (val & mask))));
	    }
	  num_insns += 2;
	  return num_insns;
	}
      else if (aarch64_uimm12_shift (-(val - ((val + comp) & mask))))
	{
	  if (generate)
	    {
	      subtarget = subtargets ? gen_reg_rtx (DImode) : dest;
	      emit_insn (gen_rtx_SET (VOIDmode, subtarget,
				      GEN_INT ((val + comp) & mask)));
	      emit_insn (gen_adddi3 (dest, subtarget,
				     GEN_INT (val - ((val + comp) & mask))));
	    }
	  num_insns += 2;
	  return num_insns;
	}
      else if (aarch64_uimm12_shift (val - ((val - comp) | ~mask)))
	{
	  if (generate)
	    {
	      subtarget = subtargets ? gen_reg_rtx (DImode) : dest;
	      emit_insn (gen_rtx_SET (VOIDmode, subtarget,
				      GEN_INT ((val - comp) | ~mask)));
	      emit_insn (gen_adddi3 (dest, subtarget,
				     GEN_INT (val - ((val - comp) | ~mask))));
	    }
	  num_insns += 2;
	  return num_insns;
	}
      else if (aarch64_uimm12_shift (-(val - (val | ~mask))))
	{
	  if (generate)
	    {
	      subtarget = subtargets ? gen_reg_rtx (DImode) : dest;
	      emit_insn (gen_rtx_SET (VOIDmode, subtarget,
				      GEN_INT (val | ~mask)));
	      emit_insn (gen_adddi3 (dest, subtarget,
				     GEN_INT (val - (val | ~mask))));
	    }
	  num_insns += 2;
	  return num_insns;
	}
    }

  /* See if we can do it by arithmetically combining two
     immediates.  */
  for (i = 0; i < AARCH64_NUM_BITMASKS; i++)
    {
      int j;
      mask = 0xffff;

      if (aarch64_uimm12_shift (val - aarch64_bitmasks[i])
	  || aarch64_uimm12_shift (-val + aarch64_bitmasks[i]))
	{
	  if (generate)
	    {
	      subtarget = subtargets ? gen_reg_rtx (DImode) : dest;
	      emit_insn (gen_rtx_SET (VOIDmode, subtarget,
				      GEN_INT (aarch64_bitmasks[i])));
	      emit_insn (gen_adddi3 (dest, subtarget,
				     GEN_INT (val - aarch64_bitmasks[i])));
	    }
	  num_insns += 2;
	  return num_insns;
	}

      for (j = 0; j < 64; j += 16, mask <<= 16)
	{
	  if ((aarch64_bitmasks[i] & ~mask) == (val & ~mask))
	    {
	      if (generate)
		{
		  emit_insn (gen_rtx_SET (VOIDmode, dest,
					  GEN_INT (aarch64_bitmasks[i])));
		  emit_insn (gen_insv_immdi (dest, GEN_INT (j),
					     GEN_INT ((val >> j) & 0xffff)));
		}
	      num_insns += 2;
	      return num_insns;
	    }
	}
    }

  /* See if we can do it by logically combining two immediates.  */
  for (i = 0; i < AARCH64_NUM_BITMASKS; i++)
    {
      if ((aarch64_bitmasks[i] & val) == aarch64_bitmasks[i])
	{
	  int j;

	  for (j = i + 1; j < AARCH64_NUM_BITMASKS; j++)
	    if (val == (aarch64_bitmasks[i] | aarch64_bitmasks[j]))
	      {
		if (generate)
		  {
		    subtarget = subtargets ? gen_reg_rtx (mode) : dest;
		    emit_insn (gen_rtx_SET (VOIDmode, subtarget,
					    GEN_INT (aarch64_bitmasks[i])));
		    emit_insn (gen_iordi3 (dest, subtarget,
					   GEN_INT (aarch64_bitmasks[j])));
		  }
		num_insns += 2;
		return num_insns;
	      }
	}
      else if ((val & aarch64_bitmasks[i]) == val)
	{
	  int j;

	  for (j = i + 1; j < AARCH64_NUM_BITMASKS; j++)
	    if (val == (aarch64_bitmasks[j] & aarch64_bitmasks[i]))
	      {
		if (generate)
		  {
		    subtarget = subtargets ? gen_reg_rtx (mode) : dest;
		    emit_insn (gen_rtx_SET (VOIDmode, subtarget,
					    GEN_INT (aarch64_bitmasks[j])));
		    emit_insn (gen_anddi3 (dest, subtarget,
					   GEN_INT (aarch64_bitmasks[i])));
		  }
		num_insns += 2;
		return num_insns;
	      }
	}
    }

  if (one_match > zero_match)
    {
      /* Set either first three quarters or all but the third.	 */
      mask = 0xffffll << (16 - first_not_ffff_match);
      if (generate)
	emit_insn (gen_rtx_SET (VOIDmode, dest,
				GEN_INT (val | mask | 0xffffffff00000000ull)));
      num_insns ++;

      /* Now insert other two quarters.	 */
      for (i = first_not_ffff_match + 16, mask <<= (first_not_ffff_match << 1);
	   i < 64; i += 16, mask <<= 16)
	{
	  if ((val & mask) != mask)
	    {
	      if (generate)
		emit_insn (gen_insv_immdi (dest, GEN_INT (i),
					   GEN_INT ((val >> i) & 0xffff)));
	      num_insns ++;
	    }
	}
      return num_insns;
    }

 simple_sequence:
  first = true;
  mask = 0xffff;
  for (i = 0; i < 64; i += 16, mask <<= 16)
    {
      if ((val & mask) != 0)
	{
	  if (first)
	    {
	      if (generate)
		emit_insn (gen_rtx_SET (VOIDmode, dest,
					GEN_INT (val & mask)));
	      num_insns ++;
	      first = false;
	    }
	  else
	    {
	      if (generate)
		emit_insn (gen_insv_immdi (dest, GEN_INT (i),
					   GEN_INT ((val >> i) & 0xffff)));
	      num_insns ++;
	    }
	}
    }

  return num_insns;
}


void
aarch64_expand_mov_immediate (rtx dest, rtx imm)
{
  machine_mode mode = GET_MODE (dest);

  gcc_assert (mode == SImode || mode == DImode);

  /* Check on what type of symbol it is.  */
  if (GET_CODE (imm) == SYMBOL_REF
      || GET_CODE (imm) == LABEL_REF
      || GET_CODE (imm) == CONST)
    {
      rtx mem, base, offset;
      enum aarch64_symbol_type sty;

      /* If we have (const (plus symbol offset)), separate out the offset
	 before we start classifying the symbol.  */
      split_const (imm, &base, &offset);

      sty = aarch64_classify_symbol (base, offset, SYMBOL_CONTEXT_ADR);
      switch (sty)
	{
	case SYMBOL_FORCE_TO_MEM:
	  if (offset != const0_rtx
	      && targetm.cannot_force_const_mem (mode, imm))
	    {
	      gcc_assert (can_create_pseudo_p ());
	      base = aarch64_force_temporary (mode, dest, base);
	      base = aarch64_add_offset (mode, NULL, base, INTVAL (offset));
	      aarch64_emit_move (dest, base);
	      return;
	    }
	  mem = force_const_mem (ptr_mode, imm);
	  gcc_assert (mem);
	  if (mode != ptr_mode)
	    mem = gen_rtx_ZERO_EXTEND (mode, mem);
	  emit_insn (gen_rtx_SET (VOIDmode, dest, mem));
	  return;

        case SYMBOL_SMALL_TLSGD:
        case SYMBOL_SMALL_TLSDESC:
        case SYMBOL_SMALL_GOTTPREL:
	case SYMBOL_SMALL_GOT:
	case SYMBOL_TINY_GOT:
	  if (offset != const0_rtx)
	    {
	      gcc_assert(can_create_pseudo_p ());
	      base = aarch64_force_temporary (mode, dest, base);
	      base = aarch64_add_offset (mode, NULL, base, INTVAL (offset));
	      aarch64_emit_move (dest, base);
	      return;
	    }
	  /* FALLTHRU */

        case SYMBOL_SMALL_TPREL:
	case SYMBOL_SMALL_ABSOLUTE:
	case SYMBOL_TINY_ABSOLUTE:
	  aarch64_load_symref_appropriately (dest, imm, sty);
	  return;

	default:
	  gcc_unreachable ();
	}
    }

  if (!CONST_INT_P (imm))
    {
      if (GET_CODE (imm) == HIGH)
	emit_insn (gen_rtx_SET (VOIDmode, dest, imm));
      else
        {
	  rtx mem = force_const_mem (mode, imm);
	  gcc_assert (mem);
	  emit_insn (gen_rtx_SET (VOIDmode, dest, mem));
	}

      return;
    }

  aarch64_internal_mov_immediate (dest, imm, true, GET_MODE (dest));
}

static bool
aarch64_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
				 tree exp ATTRIBUTE_UNUSED)
{
  /* Currently, always true.  */
  return true;
}

/* Implement TARGET_PASS_BY_REFERENCE.  */

static bool
aarch64_pass_by_reference (cumulative_args_t pcum ATTRIBUTE_UNUSED,
			   machine_mode mode,
			   const_tree type,
			   bool named ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size;
  machine_mode dummymode;
  int nregs;

  /* GET_MODE_SIZE (BLKmode) is useless since it is 0.  */
  size = (mode == BLKmode && type)
    ? int_size_in_bytes (type) : (int) GET_MODE_SIZE (mode);

  /* Aggregates are passed by reference based on their size.  */
  if (type && AGGREGATE_TYPE_P (type))
    {
      size = int_size_in_bytes (type);
    }

  /* Variable sized arguments are always returned by reference.  */
  if (size < 0)
    return true;

  /* Can this be a candidate to be passed in fp/simd register(s)?  */
  if (aarch64_vfp_is_call_or_return_candidate (mode, type,
					       &dummymode, &nregs,
					       NULL))
    return false;

  /* Arguments which are variable sized or larger than 2 registers are
     passed by reference unless they are a homogenous floating point
     aggregate.  */
  return size > 2 * UNITS_PER_WORD;
}

/* Return TRUE if VALTYPE is padded to its least significant bits.  */
static bool
aarch64_return_in_msb (const_tree valtype)
{
  machine_mode dummy_mode;
  int dummy_int;

  /* Never happens in little-endian mode.  */
  if (!BYTES_BIG_ENDIAN)
    return false;

  /* Only composite types smaller than or equal to 16 bytes can
     be potentially returned in registers.  */
  if (!aarch64_composite_type_p (valtype, TYPE_MODE (valtype))
      || int_size_in_bytes (valtype) <= 0
      || int_size_in_bytes (valtype) > 16)
    return false;

  /* But not a composite that is an HFA (Homogeneous Floating-point Aggregate)
     or an HVA (Homogeneous Short-Vector Aggregate); such a special composite
     is always passed/returned in the least significant bits of fp/simd
     register(s).  */
  if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (valtype), valtype,
					       &dummy_mode, &dummy_int, NULL))
    return false;

  return true;
}

/* Implement TARGET_FUNCTION_VALUE.
   Define how to find the value returned by a function.  */

static rtx
aarch64_function_value (const_tree type, const_tree func,
			bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  int unsignedp;
  int count;
  machine_mode ag_mode;

  mode = TYPE_MODE (type);
  if (INTEGRAL_TYPE_P (type))
    mode = promote_function_mode (type, mode, &unsignedp, func, 1);

  if (aarch64_return_in_msb (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);

      if (size % UNITS_PER_WORD != 0)
	{
	  size += UNITS_PER_WORD - size % UNITS_PER_WORD;
	  mode = mode_for_size (size * BITS_PER_UNIT, MODE_INT, 0);
	}
    }

  if (aarch64_vfp_is_call_or_return_candidate (mode, type,
					       &ag_mode, &count, NULL))
    {
      if (!aarch64_composite_type_p (type, mode))
	{
	  gcc_assert (count == 1 && mode == ag_mode);
	  return gen_rtx_REG (mode, V0_REGNUM);
	}
      else
	{
	  int i;
	  rtx par;

	  par = gen_rtx_PARALLEL (mode, rtvec_alloc (count));
	  for (i = 0; i < count; i++)
	    {
	      rtx tmp = gen_rtx_REG (ag_mode, V0_REGNUM + i);
	      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp,
				       GEN_INT (i * GET_MODE_SIZE (ag_mode)));
	      XVECEXP (par, 0, i) = tmp;
	    }
	  return par;
	}
    }
  else
    return gen_rtx_REG (mode, R0_REGNUM);
}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.
   Return true if REGNO is the number of a hard register in which the values
   of called function may come back.  */

static bool
aarch64_function_value_regno_p (const unsigned int regno)
{
  /* Maximum of 16 bytes can be returned in the general registers.  Examples
     of 16-byte return values are: 128-bit integers and 16-byte small
     structures (excluding homogeneous floating-point aggregates).  */
  if (regno == R0_REGNUM || regno == R1_REGNUM)
    return true;

  /* Up to four fp/simd registers can return a function value, e.g. a
     homogeneous floating-point aggregate having four members.  */
  if (regno >= V0_REGNUM && regno < V0_REGNUM + HA_MAX_NUM_FLDS)
    return TARGET_FLOAT;

  return false;
}

/* Implement TARGET_RETURN_IN_MEMORY.

   If the type T of the result of a function is such that
     void func (T arg)
   would require that arg be passed as a value in a register (or set of
   registers) according to the parameter passing rules, then the result
   is returned in the same registers as would be used for such an
   argument.  */

static bool
aarch64_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size;
  machine_mode ag_mode;
  int count;

  if (!AGGREGATE_TYPE_P (type)
      && TREE_CODE (type) != COMPLEX_TYPE
      && TREE_CODE (type) != VECTOR_TYPE)
    /* Simple scalar types always returned in registers.  */
    return false;

  if (aarch64_vfp_is_call_or_return_candidate (TYPE_MODE (type),
					       type,
					       &ag_mode,
					       &count,
					       NULL))
    return false;

  /* Types larger than 2 registers returned in memory.  */
  size = int_size_in_bytes (type);
  return (size < 0 || size > 2 * UNITS_PER_WORD);
}

static bool
aarch64_vfp_is_call_candidate (cumulative_args_t pcum_v, machine_mode mode,
			       const_tree type, int *nregs)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  return aarch64_vfp_is_call_or_return_candidate (mode,
						  type,
						  &pcum->aapcs_vfp_rmode,
						  nregs,
						  NULL);
}

/* Given MODE and TYPE of a function argument, return the alignment in
   bits.  The idea is to suppress any stronger alignment requested by
   the user and opt for the natural alignment (specified in AAPCS64 \S 4.1).
   This is a helper function for local use only.  */

static unsigned int
aarch64_function_arg_alignment (machine_mode mode, const_tree type)
{
  unsigned int alignment;

  if (type)
    {
      if (!integer_zerop (TYPE_SIZE (type)))
	{
	  if (TYPE_MODE (type) == mode)
	    alignment = TYPE_ALIGN (type);
	  else
	    alignment = GET_MODE_ALIGNMENT (mode);
	}
      else
	alignment = 0;
    }
  else
    alignment = GET_MODE_ALIGNMENT (mode);

  return alignment;
}

/* Layout a function argument according to the AAPCS64 rules.  The rule
   numbers refer to the rule numbers in the AAPCS64.  */

static void
aarch64_layout_arg (cumulative_args_t pcum_v, machine_mode mode,
		    const_tree type,
		    bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  int ncrn, nvrn, nregs;
  bool allocate_ncrn, allocate_nvrn;
  HOST_WIDE_INT size;

  /* We need to do this once per argument.  */
  if (pcum->aapcs_arg_processed)
    return;

  pcum->aapcs_arg_processed = true;

  /* Size in bytes, rounded to the nearest multiple of 8 bytes.  */
  size
    = AARCH64_ROUND_UP (type ? int_size_in_bytes (type) : GET_MODE_SIZE (mode),
			UNITS_PER_WORD);

  allocate_ncrn = (type) ? !(FLOAT_TYPE_P (type)) : !FLOAT_MODE_P (mode);
  allocate_nvrn = aarch64_vfp_is_call_candidate (pcum_v,
						 mode,
						 type,
						 &nregs);

  /* allocate_ncrn may be false-positive, but allocate_nvrn is quite reliable.
     The following code thus handles passing by SIMD/FP registers first.  */

  nvrn = pcum->aapcs_nvrn;

  /* C1 - C5 for floating point, homogenous floating point aggregates (HFA)
     and homogenous short-vector aggregates (HVA).  */
  if (allocate_nvrn)
    {
      if (nvrn + nregs <= NUM_FP_ARG_REGS)
	{
	  pcum->aapcs_nextnvrn = nvrn + nregs;
	  if (!aarch64_composite_type_p (type, mode))
	    {
	      gcc_assert (nregs == 1);
	      pcum->aapcs_reg = gen_rtx_REG (mode, V0_REGNUM + nvrn);
	    }
	  else
	    {
	      rtx par;
	      int i;
	      par = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));
	      for (i = 0; i < nregs; i++)
		{
		  rtx tmp = gen_rtx_REG (pcum->aapcs_vfp_rmode,
					 V0_REGNUM + nvrn + i);
		  tmp = gen_rtx_EXPR_LIST
		    (VOIDmode, tmp,
		     GEN_INT (i * GET_MODE_SIZE (pcum->aapcs_vfp_rmode)));
		  XVECEXP (par, 0, i) = tmp;
		}
	      pcum->aapcs_reg = par;
	    }
	  return;
	}
      else
	{
	  /* C.3 NSRN is set to 8.  */
	  pcum->aapcs_nextnvrn = NUM_FP_ARG_REGS;
	  goto on_stack;
	}
    }

  ncrn = pcum->aapcs_ncrn;
  nregs = size / UNITS_PER_WORD;

  /* C6 - C9.  though the sign and zero extension semantics are
     handled elsewhere.  This is the case where the argument fits
     entirely general registers.  */
  if (allocate_ncrn && (ncrn + nregs <= NUM_ARG_REGS))
    {
      unsigned int alignment = aarch64_function_arg_alignment (mode, type);

      gcc_assert (nregs == 0 || nregs == 1 || nregs == 2);

      /* C.8 if the argument has an alignment of 16 then the NGRN is
         rounded up to the next even number.  */
      if (nregs == 2 && alignment == 16 * BITS_PER_UNIT && ncrn % 2)
	{
	  ++ncrn;
	  gcc_assert (ncrn + nregs <= NUM_ARG_REGS);
	}
      /* NREGS can be 0 when e.g. an empty structure is to be passed.
         A reg is still generated for it, but the caller should be smart
	 enough not to use it.  */
      if (nregs == 0 || nregs == 1 || GET_MODE_CLASS (mode) == MODE_INT)
	{
	  pcum->aapcs_reg = gen_rtx_REG (mode, R0_REGNUM + ncrn);
	}
      else
	{
	  rtx par;
	  int i;

	  par = gen_rtx_PARALLEL (mode, rtvec_alloc (nregs));
	  for (i = 0; i < nregs; i++)
	    {
	      rtx tmp = gen_rtx_REG (word_mode, R0_REGNUM + ncrn + i);
	      tmp = gen_rtx_EXPR_LIST (VOIDmode, tmp,
				       GEN_INT (i * UNITS_PER_WORD));
	      XVECEXP (par, 0, i) = tmp;
	    }
	  pcum->aapcs_reg = par;
	}

      pcum->aapcs_nextncrn = ncrn + nregs;
      return;
    }

  /* C.11  */
  pcum->aapcs_nextncrn = NUM_ARG_REGS;

  /* The argument is passed on stack; record the needed number of words for
     this argument and align the total size if necessary.  */
on_stack:
  pcum->aapcs_stack_words = size / UNITS_PER_WORD;
  if (aarch64_function_arg_alignment (mode, type) == 16 * BITS_PER_UNIT)
    pcum->aapcs_stack_size = AARCH64_ROUND_UP (pcum->aapcs_stack_size,
					       16 / UNITS_PER_WORD);
  return;
}

/* Implement TARGET_FUNCTION_ARG.  */

static rtx
aarch64_function_arg (cumulative_args_t pcum_v, machine_mode mode,
		      const_tree type, bool named)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  gcc_assert (pcum->pcs_variant == ARM_PCS_AAPCS64);

  if (mode == VOIDmode)
    return NULL_RTX;

  aarch64_layout_arg (pcum_v, mode, type, named);
  return pcum->aapcs_reg;
}

void
aarch64_init_cumulative_args (CUMULATIVE_ARGS *pcum,
			   const_tree fntype ATTRIBUTE_UNUSED,
			   rtx libname ATTRIBUTE_UNUSED,
			   const_tree fndecl ATTRIBUTE_UNUSED,
			   unsigned n_named ATTRIBUTE_UNUSED)
{
  pcum->aapcs_ncrn = 0;
  pcum->aapcs_nvrn = 0;
  pcum->aapcs_nextncrn = 0;
  pcum->aapcs_nextnvrn = 0;
  pcum->pcs_variant = ARM_PCS_AAPCS64;
  pcum->aapcs_reg = NULL_RTX;
  pcum->aapcs_arg_processed = false;
  pcum->aapcs_stack_words = 0;
  pcum->aapcs_stack_size = 0;

  return;
}

static void
aarch64_function_arg_advance (cumulative_args_t pcum_v,
			      machine_mode mode,
			      const_tree type,
			      bool named)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  if (pcum->pcs_variant == ARM_PCS_AAPCS64)
    {
      aarch64_layout_arg (pcum_v, mode, type, named);
      gcc_assert ((pcum->aapcs_reg != NULL_RTX)
		  != (pcum->aapcs_stack_words != 0));
      pcum->aapcs_arg_processed = false;
      pcum->aapcs_ncrn = pcum->aapcs_nextncrn;
      pcum->aapcs_nvrn = pcum->aapcs_nextnvrn;
      pcum->aapcs_stack_size += pcum->aapcs_stack_words;
      pcum->aapcs_stack_words = 0;
      pcum->aapcs_reg = NULL_RTX;
    }
}

bool
aarch64_function_arg_regno_p (unsigned regno)
{
  return ((GP_REGNUM_P (regno) && regno < R0_REGNUM + NUM_ARG_REGS)
	  || (FP_REGNUM_P (regno) && regno < V0_REGNUM + NUM_FP_ARG_REGS));
}

/* Implement FUNCTION_ARG_BOUNDARY.  Every parameter gets at least
   PARM_BOUNDARY bits of alignment, but will be given anything up
   to STACK_BOUNDARY bits if the type requires it.  This makes sure
   that both before and after the layout of each argument, the Next
   Stacked Argument Address (NSAA) will have a minimum alignment of
   8 bytes.  */

static unsigned int
aarch64_function_arg_boundary (machine_mode mode, const_tree type)
{
  unsigned int alignment = aarch64_function_arg_alignment (mode, type);

  if (alignment < PARM_BOUNDARY)
    alignment = PARM_BOUNDARY;
  if (alignment > STACK_BOUNDARY)
    alignment = STACK_BOUNDARY;
  return alignment;
}

/* For use by FUNCTION_ARG_PADDING (MODE, TYPE).

   Return true if an argument passed on the stack should be padded upwards,
   i.e. if the least-significant byte of the stack slot has useful data.

   Small aggregate types are placed in the lowest memory address.

   The related parameter passing rules are B.4, C.3, C.5 and C.14.  */

bool
aarch64_pad_arg_upward (machine_mode mode, const_tree type)
{
  /* On little-endian targets, the least significant byte of every stack
     argument is passed at the lowest byte address of the stack slot.  */
  if (!BYTES_BIG_ENDIAN)
    return true;

  /* Otherwise, integral, floating-point and pointer types are padded downward:
     the least significant byte of a stack argument is passed at the highest
     byte address of the stack slot.  */
  if (type
      ? (INTEGRAL_TYPE_P (type) || SCALAR_FLOAT_TYPE_P (type)
	 || POINTER_TYPE_P (type))
      : (SCALAR_INT_MODE_P (mode) || SCALAR_FLOAT_MODE_P (mode)))
    return false;

  /* Everything else padded upward, i.e. data in first byte of stack slot.  */
  return true;
}

/* Similarly, for use by BLOCK_REG_PADDING (MODE, TYPE, FIRST).

   It specifies padding for the last (may also be the only)
   element of a block move between registers and memory.  If
   assuming the block is in the memory, padding upward means that
   the last element is padded after its highest significant byte,
   while in downward padding, the last element is padded at the
   its least significant byte side.

   Small aggregates and small complex types are always padded
   upwards.

   We don't need to worry about homogeneous floating-point or
   short-vector aggregates; their move is not affected by the
   padding direction determined here.  Regardless of endianness,
   each element of such an aggregate is put in the least
   significant bits of a fp/simd register.

   Return !BYTES_BIG_ENDIAN if the least significant byte of the
   register has useful data, and return the opposite if the most
   significant byte does.  */

bool
aarch64_pad_reg_upward (machine_mode mode, const_tree type,
		     bool first ATTRIBUTE_UNUSED)
{

  /* Small composite types are always padded upward.  */
  if (BYTES_BIG_ENDIAN && aarch64_composite_type_p (type, mode))
    {
      HOST_WIDE_INT size = (type ? int_size_in_bytes (type)
			    : GET_MODE_SIZE (mode));
      if (size < 2 * UNITS_PER_WORD)
	return true;
    }

  /* Otherwise, use the default padding.  */
  return !BYTES_BIG_ENDIAN;
}

static machine_mode
aarch64_libgcc_cmp_return_mode (void)
{
  return SImode;
}

static bool
aarch64_frame_pointer_required (void)
{
  /* In aarch64_override_options_after_change
     flag_omit_leaf_frame_pointer turns off the frame pointer by
     default.  Turn it back on now if we've not got a leaf
     function.  */
  if (flag_omit_leaf_frame_pointer
      && (!crtl->is_leaf || df_regs_ever_live_p (LR_REGNUM)))
    return true;

  return false;
}

/* Mark the registers that need to be saved by the callee and calculate
   the size of the callee-saved registers area and frame record (both FP
   and LR may be omitted).  */
static void
aarch64_layout_frame (void)
{
  HOST_WIDE_INT offset = 0;
  int regno;

  if (reload_completed && cfun->machine->frame.laid_out)
    return;

#define SLOT_NOT_REQUIRED (-2)
#define SLOT_REQUIRED     (-1)

  cfun->machine->frame.wb_candidate1 = FIRST_PSEUDO_REGISTER;
  cfun->machine->frame.wb_candidate2 = FIRST_PSEUDO_REGISTER;

  /* First mark all the registers that really need to be saved...  */
  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    cfun->machine->frame.reg_offset[regno] = SLOT_NOT_REQUIRED;

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    cfun->machine->frame.reg_offset[regno] = SLOT_NOT_REQUIRED;

  /* ... that includes the eh data registers (if needed)...  */
  if (crtl->calls_eh_return)
    for (regno = 0; EH_RETURN_DATA_REGNO (regno) != INVALID_REGNUM; regno++)
      cfun->machine->frame.reg_offset[EH_RETURN_DATA_REGNO (regno)]
	= SLOT_REQUIRED;

  /* ... and any callee saved register that dataflow says is live.  */
  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    if (df_regs_ever_live_p (regno)
	&& (regno == R30_REGNUM
	    || !call_used_regs[regno]))
      cfun->machine->frame.reg_offset[regno] = SLOT_REQUIRED;

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    if (df_regs_ever_live_p (regno)
	&& !call_used_regs[regno])
      cfun->machine->frame.reg_offset[regno] = SLOT_REQUIRED;

  if (frame_pointer_needed)
    {
      /* FP and LR are placed in the linkage record.  */
      cfun->machine->frame.reg_offset[R29_REGNUM] = 0;
      cfun->machine->frame.wb_candidate1 = R29_REGNUM;
      cfun->machine->frame.reg_offset[R30_REGNUM] = UNITS_PER_WORD;
      cfun->machine->frame.wb_candidate2 = R30_REGNUM;
      cfun->machine->frame.hardfp_offset = 2 * UNITS_PER_WORD;
      offset += 2 * UNITS_PER_WORD;
    }

  /* Now assign stack slots for them.  */
  for (regno = R0_REGNUM; regno <= R30_REGNUM; regno++)
    if (cfun->machine->frame.reg_offset[regno] == SLOT_REQUIRED)
      {
	cfun->machine->frame.reg_offset[regno] = offset;
	if (cfun->machine->frame.wb_candidate1 == FIRST_PSEUDO_REGISTER)
	  cfun->machine->frame.wb_candidate1 = regno;
	else if (cfun->machine->frame.wb_candidate2 == FIRST_PSEUDO_REGISTER)
	  cfun->machine->frame.wb_candidate2 = regno;
	offset += UNITS_PER_WORD;
      }

  for (regno = V0_REGNUM; regno <= V31_REGNUM; regno++)
    if (cfun->machine->frame.reg_offset[regno] == SLOT_REQUIRED)
      {
	cfun->machine->frame.reg_offset[regno] = offset;
	if (cfun->machine->frame.wb_candidate1 == FIRST_PSEUDO_REGISTER)
	  cfun->machine->frame.wb_candidate1 = regno;
	else if (cfun->machine->frame.wb_candidate2 == FIRST_PSEUDO_REGISTER
		 && cfun->machine->frame.wb_candidate1 >= V0_REGNUM)
	  cfun->machine->frame.wb_candidate2 = regno;
	offset += UNITS_PER_WORD;
      }

  cfun->machine->frame.padding0 =
    (AARCH64_ROUND_UP (offset, STACK_BOUNDARY / BITS_PER_UNIT) - offset);
  offset = AARCH64_ROUND_UP (offset, STACK_BOUNDARY / BITS_PER_UNIT);

  cfun->machine->frame.saved_regs_size = offset;

  cfun->machine->frame.hard_fp_offset
    = AARCH64_ROUND_UP (cfun->machine->frame.saved_varargs_size
			+ get_frame_size ()
			+ cfun->machine->frame.saved_regs_size,
			STACK_BOUNDARY / BITS_PER_UNIT);

  cfun->machine->frame.frame_size
    = AARCH64_ROUND_UP (cfun->machine->frame.hard_fp_offset
			+ crtl->outgoing_args_size,
			STACK_BOUNDARY / BITS_PER_UNIT);

  cfun->machine->frame.laid_out = true;
}

static bool
aarch64_register_saved_on_entry (int regno)
{
  return cfun->machine->frame.reg_offset[regno] >= 0;
}

static unsigned
aarch64_next_callee_save (unsigned regno, unsigned limit)
{
  while (regno <= limit && !aarch64_register_saved_on_entry (regno))
    regno ++;
  return regno;
}

static void
aarch64_pushwb_single_reg (machine_mode mode, unsigned regno,
			   HOST_WIDE_INT adjustment)
 {
  rtx base_rtx = stack_pointer_rtx;
  rtx insn, reg, mem;

  reg = gen_rtx_REG (mode, regno);
  mem = gen_rtx_PRE_MODIFY (Pmode, base_rtx,
			    plus_constant (Pmode, base_rtx, -adjustment));
  mem = gen_rtx_MEM (mode, mem);

  insn = emit_move_insn (mem, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

static rtx
aarch64_gen_storewb_pair (machine_mode mode, rtx base, rtx reg, rtx reg2,
			  HOST_WIDE_INT adjustment)
{
  switch (mode)
    {
    case DImode:
      return gen_storewb_pairdi_di (base, base, reg, reg2,
				    GEN_INT (-adjustment),
				    GEN_INT (UNITS_PER_WORD - adjustment));
    case DFmode:
      return gen_storewb_pairdf_di (base, base, reg, reg2,
				    GEN_INT (-adjustment),
				    GEN_INT (UNITS_PER_WORD - adjustment));
    default:
      gcc_unreachable ();
    }
}

static void
aarch64_pushwb_pair_reg (machine_mode mode, unsigned regno1,
			 unsigned regno2, HOST_WIDE_INT adjustment)
{
  rtx_insn *insn;
  rtx reg1 = gen_rtx_REG (mode, regno1);
  rtx reg2 = gen_rtx_REG (mode, regno2);

  insn = emit_insn (aarch64_gen_storewb_pair (mode, stack_pointer_rtx, reg1,
					      reg2, adjustment));
  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 2)) = 1;
  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 1)) = 1;
  RTX_FRAME_RELATED_P (insn) = 1;
}

static rtx
aarch64_gen_loadwb_pair (machine_mode mode, rtx base, rtx reg, rtx reg2,
			 HOST_WIDE_INT adjustment)
{
  switch (mode)
    {
    case DImode:
      return gen_loadwb_pairdi_di (base, base, reg, reg2, GEN_INT (adjustment),
				   GEN_INT (UNITS_PER_WORD));
    case DFmode:
      return gen_loadwb_pairdf_di (base, base, reg, reg2, GEN_INT (adjustment),
				   GEN_INT (UNITS_PER_WORD));
    default:
      gcc_unreachable ();
    }
}

static rtx
aarch64_gen_store_pair (machine_mode mode, rtx mem1, rtx reg1, rtx mem2,
			rtx reg2)
{
  switch (mode)
    {
    case DImode:
      return gen_store_pairdi (mem1, reg1, mem2, reg2);

    case DFmode:
      return gen_store_pairdf (mem1, reg1, mem2, reg2);

    default:
      gcc_unreachable ();
    }
}

static rtx
aarch64_gen_load_pair (machine_mode mode, rtx reg1, rtx mem1, rtx reg2,
		       rtx mem2)
{
  switch (mode)
    {
    case DImode:
      return gen_load_pairdi (reg1, mem1, reg2, mem2);

    case DFmode:
      return gen_load_pairdf (reg1, mem1, reg2, mem2);

    default:
      gcc_unreachable ();
    }
}


static void
aarch64_save_callee_saves (machine_mode mode, HOST_WIDE_INT start_offset,
			   unsigned start, unsigned limit, bool skip_wb)
{
  rtx_insn *insn;
  rtx (*gen_mem_ref) (machine_mode, rtx) = (frame_pointer_needed
						 ? gen_frame_mem : gen_rtx_MEM);
  unsigned regno;
  unsigned regno2;

  for (regno = aarch64_next_callee_save (start, limit);
       regno <= limit;
       regno = aarch64_next_callee_save (regno + 1, limit))
    {
      rtx reg, mem;
      HOST_WIDE_INT offset;

      if (skip_wb
	  && (regno == cfun->machine->frame.wb_candidate1
	      || regno == cfun->machine->frame.wb_candidate2))
	continue;

      reg = gen_rtx_REG (mode, regno);
      offset = start_offset + cfun->machine->frame.reg_offset[regno];
      mem = gen_mem_ref (mode, plus_constant (Pmode, stack_pointer_rtx,
					      offset));

      regno2 = aarch64_next_callee_save (regno + 1, limit);

      if (regno2 <= limit
	  && ((cfun->machine->frame.reg_offset[regno] + UNITS_PER_WORD)
	      == cfun->machine->frame.reg_offset[regno2]))

	{
	  rtx reg2 = gen_rtx_REG (mode, regno2);
	  rtx mem2;

	  offset = start_offset + cfun->machine->frame.reg_offset[regno2];
	  mem2 = gen_mem_ref (mode, plus_constant (Pmode, stack_pointer_rtx,
						   offset));
	  insn = emit_insn (aarch64_gen_store_pair (mode, mem, reg, mem2,
						    reg2));

	  /* The first part of a frame-related parallel insn is
	     always assumed to be relevant to the frame
	     calculations; subsequent parts, are only
	     frame-related if explicitly marked.  */
	  RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 1)) = 1;
	  regno = regno2;
	}
      else
	insn = emit_move_insn (mem, reg);

      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

static void
aarch64_restore_callee_saves (machine_mode mode,
			      HOST_WIDE_INT start_offset, unsigned start,
			      unsigned limit, bool skip_wb, rtx *cfi_ops)
{
  rtx base_rtx = stack_pointer_rtx;
  rtx (*gen_mem_ref) (machine_mode, rtx) = (frame_pointer_needed
						 ? gen_frame_mem : gen_rtx_MEM);
  unsigned regno;
  unsigned regno2;
  HOST_WIDE_INT offset;

  for (regno = aarch64_next_callee_save (start, limit);
       regno <= limit;
       regno = aarch64_next_callee_save (regno + 1, limit))
    {
      rtx reg, mem;

      if (skip_wb
	  && (regno == cfun->machine->frame.wb_candidate1
	      || regno == cfun->machine->frame.wb_candidate2))
	continue;

      reg = gen_rtx_REG (mode, regno);
      offset = start_offset + cfun->machine->frame.reg_offset[regno];
      mem = gen_mem_ref (mode, plus_constant (Pmode, base_rtx, offset));

      regno2 = aarch64_next_callee_save (regno + 1, limit);

      if (regno2 <= limit
	  && ((cfun->machine->frame.reg_offset[regno] + UNITS_PER_WORD)
	      == cfun->machine->frame.reg_offset[regno2]))
	{
	  rtx reg2 = gen_rtx_REG (mode, regno2);
	  rtx mem2;

	  offset = start_offset + cfun->machine->frame.reg_offset[regno2];
	  mem2 = gen_mem_ref (mode, plus_constant (Pmode, base_rtx, offset));
	  emit_insn (aarch64_gen_load_pair (mode, reg, mem, reg2, mem2));

	  *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg2, *cfi_ops);
	  regno = regno2;
	}
      else
	emit_move_insn (reg, mem);
      *cfi_ops = alloc_reg_note (REG_CFA_RESTORE, reg, *cfi_ops);
    }
}

/* AArch64 stack frames generated by this compiler look like:

	+-------------------------------+
	|                               |
	|  incoming stack arguments     |
	|                               |
	+-------------------------------+
	|                               | <-- incoming stack pointer (aligned)
	|  callee-allocated save area   |
	|  for register varargs         |
	|                               |
	+-------------------------------+
	|  local variables              | <-- frame_pointer_rtx
	|                               |
	+-------------------------------+
	|  padding0                     | \
	+-------------------------------+  |
	|  callee-saved registers       |  | frame.saved_regs_size
	+-------------------------------+  |
	|  LR'                          |  |
	+-------------------------------+  |
	|  FP'                          | / <- hard_frame_pointer_rtx (aligned)
        +-------------------------------+
	|  dynamic allocation           |
	+-------------------------------+
	|  padding                      |
	+-------------------------------+
	|  outgoing stack arguments     | <-- arg_pointer
        |                               |
	+-------------------------------+
	|                               | <-- stack_pointer_rtx (aligned)

   Dynamic stack allocations via alloca() decrease stack_pointer_rtx
   but leave frame_pointer_rtx and hard_frame_pointer_rtx
   unchanged.  */

/* Generate the prologue instructions for entry into a function.
   Establish the stack frame by decreasing the stack pointer with a
   properly calculated size and, if necessary, create a frame record
   filled with the values of LR and previous frame pointer.  The
   current FP is also set up if it is in use.  */

void
aarch64_expand_prologue (void)
{
  /* sub sp, sp, #<frame_size>
     stp {fp, lr}, [sp, #<frame_size> - 16]
     add fp, sp, #<frame_size> - hardfp_offset
     stp {cs_reg}, [fp, #-16] etc.

     sub sp, sp, <final_adjustment_if_any>
  */
  HOST_WIDE_INT frame_size, offset;
  HOST_WIDE_INT fp_offset;		/* Offset from hard FP to SP.  */
  HOST_WIDE_INT hard_fp_offset;
  rtx_insn *insn;

  aarch64_layout_frame ();

  offset = frame_size = cfun->machine->frame.frame_size;
  hard_fp_offset = cfun->machine->frame.hard_fp_offset;
  fp_offset = frame_size - hard_fp_offset;

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame_size;

  /* Store pairs and load pairs have a range only -512 to 504.  */
  if (offset >= 512)
    {
      /* When the frame has a large size, an initial decrease is done on
	 the stack pointer to jump over the callee-allocated save area for
	 register varargs, the local variable area and/or the callee-saved
	 register area.  This will allow the pre-index write-back
	 store pair instructions to be used for setting up the stack frame
	 efficiently.  */
      offset = hard_fp_offset;
      if (offset >= 512)
	offset = cfun->machine->frame.saved_regs_size;

      frame_size -= (offset + crtl->outgoing_args_size);
      fp_offset = 0;

      if (frame_size >= 0x1000000)
	{
	  rtx op0 = gen_rtx_REG (Pmode, IP0_REGNUM);
	  emit_move_insn (op0, GEN_INT (-frame_size));
	  insn = emit_insn (gen_add2_insn (stack_pointer_rtx, op0));

	  add_reg_note (insn, REG_CFA_ADJUST_CFA,
			gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				     plus_constant (Pmode, stack_pointer_rtx,
						    -frame_size)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else if (frame_size > 0)
	{
	  int hi_ofs = frame_size & 0xfff000;
	  int lo_ofs = frame_size & 0x000fff;

	  if (hi_ofs)
	    {
	      insn = emit_insn (gen_add2_insn
				(stack_pointer_rtx, GEN_INT (-hi_ofs)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  if (lo_ofs)
	    {
	      insn = emit_insn (gen_add2_insn
				(stack_pointer_rtx, GEN_INT (-lo_ofs)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }
  else
    frame_size = -1;

  if (offset > 0)
    {
      bool skip_wb = false;

      if (frame_pointer_needed)
	{
	  skip_wb = true;

	  if (fp_offset)
	    {
	      insn = emit_insn (gen_add2_insn (stack_pointer_rtx,
					       GEN_INT (-offset)));
	      RTX_FRAME_RELATED_P (insn) = 1;

	      aarch64_save_callee_saves (DImode, fp_offset, R29_REGNUM,
					 R30_REGNUM, false);
	    }
	  else
	    aarch64_pushwb_pair_reg (DImode, R29_REGNUM, R30_REGNUM, offset);

	  /* Set up frame pointer to point to the location of the
	     previous frame pointer on the stack.  */
	  insn = emit_insn (gen_add3_insn (hard_frame_pointer_rtx,
					   stack_pointer_rtx,
					   GEN_INT (fp_offset)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  emit_insn (gen_stack_tie (stack_pointer_rtx, hard_frame_pointer_rtx));
	}
      else
	{
	  unsigned reg1 = cfun->machine->frame.wb_candidate1;
	  unsigned reg2 = cfun->machine->frame.wb_candidate2;

	  if (fp_offset
	      || reg1 == FIRST_PSEUDO_REGISTER
	      || (reg2 == FIRST_PSEUDO_REGISTER
		  && offset >= 256))
	    {
	      insn = emit_insn (gen_add2_insn (stack_pointer_rtx,
					       GEN_INT (-offset)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    {
	      machine_mode mode1 = (reg1 <= R30_REGNUM) ? DImode : DFmode;

	      skip_wb = true;

	      if (reg2 == FIRST_PSEUDO_REGISTER)
		aarch64_pushwb_single_reg (mode1, reg1, offset);
	      else
		aarch64_pushwb_pair_reg (mode1, reg1, reg2, offset);
	    }
	}

      aarch64_save_callee_saves (DImode, fp_offset, R0_REGNUM, R30_REGNUM,
				 skip_wb);
      aarch64_save_callee_saves (DFmode, fp_offset, V0_REGNUM, V31_REGNUM,
				 skip_wb);
    }

  /* when offset >= 512,
     sub sp, sp, #<outgoing_args_size> */
  if (frame_size > -1)
    {
      if (crtl->outgoing_args_size > 0)
	{
	  insn = emit_insn (gen_add2_insn
			    (stack_pointer_rtx,
			     GEN_INT (- crtl->outgoing_args_size)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
}

/* Return TRUE if we can use a simple_return insn.

   This function checks whether the callee saved stack is empty, which
   means no restore actions are need. The pro_and_epilogue will use
   this to check whether shrink-wrapping opt is feasible.  */

bool
aarch64_use_return_insn_p (void)
{
  if (!reload_completed)
    return false;

  if (crtl->profile)
    return false;

  aarch64_layout_frame ();

  return cfun->machine->frame.frame_size == 0;
}

/* Generate the epilogue instructions for returning from a function.  */
void
aarch64_expand_epilogue (bool for_sibcall)
{
  HOST_WIDE_INT frame_size, offset;
  HOST_WIDE_INT fp_offset;
  HOST_WIDE_INT hard_fp_offset;
  rtx_insn *insn;
  /* We need to add memory barrier to prevent read from deallocated stack.  */
  bool need_barrier_p = (get_frame_size () != 0
			 || cfun->machine->frame.saved_varargs_size);

  aarch64_layout_frame ();

  offset = frame_size = cfun->machine->frame.frame_size;
  hard_fp_offset = cfun->machine->frame.hard_fp_offset;
  fp_offset = frame_size - hard_fp_offset;

  /* Store pairs and load pairs have a range only -512 to 504.  */
  if (offset >= 512)
    {
      offset = hard_fp_offset;
      if (offset >= 512)
	offset = cfun->machine->frame.saved_regs_size;

      frame_size -= (offset + crtl->outgoing_args_size);
      fp_offset = 0;
      if (!frame_pointer_needed && crtl->outgoing_args_size > 0)
	{
	  insn = emit_insn (gen_add2_insn
			    (stack_pointer_rtx,
			     GEN_INT (crtl->outgoing_args_size)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
  else
    frame_size = -1;

  /* If there were outgoing arguments or we've done dynamic stack
     allocation, then restore the stack pointer from the frame
     pointer.  This is at most one insn and more efficient than using
     GCC's internal mechanism.  */
  if (frame_pointer_needed
      && (crtl->outgoing_args_size || cfun->calls_alloca))
    {
      if (cfun->calls_alloca)
	emit_insn (gen_stack_tie (stack_pointer_rtx, stack_pointer_rtx));

      insn = emit_insn (gen_add3_insn (stack_pointer_rtx,
				       hard_frame_pointer_rtx,
				       GEN_INT (0)));
      offset = offset - fp_offset;
    }

  if (offset > 0)
    {
      unsigned reg1 = cfun->machine->frame.wb_candidate1;
      unsigned reg2 = cfun->machine->frame.wb_candidate2;
      bool skip_wb = true;
      rtx cfi_ops = NULL;

      if (frame_pointer_needed)
	fp_offset = 0;
      else if (fp_offset
	       || reg1 == FIRST_PSEUDO_REGISTER
	       || (reg2 == FIRST_PSEUDO_REGISTER
		   && offset >= 256))
	skip_wb = false;

      aarch64_restore_callee_saves (DImode, fp_offset, R0_REGNUM, R30_REGNUM,
				    skip_wb, &cfi_ops);
      aarch64_restore_callee_saves (DFmode, fp_offset, V0_REGNUM, V31_REGNUM,
				    skip_wb, &cfi_ops);

      if (need_barrier_p)
	emit_insn (gen_stack_tie (stack_pointer_rtx, stack_pointer_rtx));

      if (skip_wb)
	{
	  machine_mode mode1 = (reg1 <= R30_REGNUM) ? DImode : DFmode;
	  rtx rreg1 = gen_rtx_REG (mode1, reg1);

	  cfi_ops = alloc_reg_note (REG_CFA_RESTORE, rreg1, cfi_ops);
	  if (reg2 == FIRST_PSEUDO_REGISTER)
	    {
	      rtx mem = plus_constant (Pmode, stack_pointer_rtx, offset);
	      mem = gen_rtx_POST_MODIFY (Pmode, stack_pointer_rtx, mem);
	      mem = gen_rtx_MEM (mode1, mem);
	      insn = emit_move_insn (rreg1, mem);
	    }
	  else
	    {
	      rtx rreg2 = gen_rtx_REG (mode1, reg2);

	      cfi_ops = alloc_reg_note (REG_CFA_RESTORE, rreg2, cfi_ops);
	      insn = emit_insn (aarch64_gen_loadwb_pair
				(mode1, stack_pointer_rtx, rreg1,
				 rreg2, offset));
	    }
	}
      else
	{
	  insn = emit_insn (gen_add2_insn (stack_pointer_rtx,
					   GEN_INT (offset)));
	}

      /* Reset the CFA to be SP + FRAME_SIZE.  */
      rtx new_cfa = stack_pointer_rtx;
      if (frame_size > 0)
	new_cfa = plus_constant (Pmode, new_cfa, frame_size);
      cfi_ops = alloc_reg_note (REG_CFA_DEF_CFA, new_cfa, cfi_ops);
      REG_NOTES (insn) = cfi_ops;
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (frame_size > 0)
    {
      if (need_barrier_p)
	emit_insn (gen_stack_tie (stack_pointer_rtx, stack_pointer_rtx));

      if (frame_size >= 0x1000000)
	{
	  rtx op0 = gen_rtx_REG (Pmode, IP0_REGNUM);
	  emit_move_insn (op0, GEN_INT (frame_size));
	  insn = emit_insn (gen_add2_insn (stack_pointer_rtx, op0));
	}
      else
	{
          int hi_ofs = frame_size & 0xfff000;
          int lo_ofs = frame_size & 0x000fff;

	  if (hi_ofs && lo_ofs)
	    {
	      insn = emit_insn (gen_add2_insn
				(stack_pointer_rtx, GEN_INT (hi_ofs)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      frame_size = lo_ofs;
	    }
	  insn = emit_insn (gen_add2_insn
			    (stack_pointer_rtx, GEN_INT (frame_size)));
	}

      /* Reset the CFA to be SP + 0.  */
      add_reg_note (insn, REG_CFA_DEF_CFA, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Stack adjustment for exception handler.  */
  if (crtl->calls_eh_return)
    {
      /* We need to unwind the stack by the offset computed by
	 EH_RETURN_STACKADJ_RTX.  We have already reset the CFA
	 to be SP; letting the CFA move during this adjustment
	 is just as correct as retaining the CFA from the body
	 of the function.  Therefore, do nothing special.  */
      emit_insn (gen_add2_insn (stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));
    }

  emit_use (gen_rtx_REG (DImode, LR_REGNUM));
  if (!for_sibcall)
    emit_jump_insn (ret_rtx);
}

/* Return the place to copy the exception unwinding return address to.
   This will probably be a stack slot, but could (in theory be the
   return register).  */
rtx
aarch64_final_eh_return_addr (void)
{
  HOST_WIDE_INT fp_offset;

  aarch64_layout_frame ();

  fp_offset = cfun->machine->frame.frame_size
	      - cfun->machine->frame.hard_fp_offset;

  if (cfun->machine->frame.reg_offset[LR_REGNUM] < 0)
    return gen_rtx_REG (DImode, LR_REGNUM);

  /* DSE and CSELIB do not detect an alias between sp+k1 and fp+k2.  This can
     result in a store to save LR introduced by builtin_eh_return () being
     incorrectly deleted because the alias is not detected.
     So in the calculation of the address to copy the exception unwinding
     return address to, we note 2 cases.
     If FP is needed and the fp_offset is 0, it means that SP = FP and hence
     we return a SP-relative location since all the addresses are SP-relative
     in this case.  This prevents the store from being optimized away.
     If the fp_offset is not 0, then the addresses will be FP-relative and
     therefore we return a FP-relative location.  */

  if (frame_pointer_needed)
    {
      if (fp_offset)
        return gen_frame_mem (DImode,
			      plus_constant (Pmode, hard_frame_pointer_rtx, UNITS_PER_WORD));
      else
        return gen_frame_mem (DImode,
			      plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD));
    }

  /* If FP is not needed, we calculate the location of LR, which would be
     at the top of the saved registers block.  */

  return gen_frame_mem (DImode,
			plus_constant (Pmode,
				       stack_pointer_rtx,
				       fp_offset
				       + cfun->machine->frame.saved_regs_size
				       - 2 * UNITS_PER_WORD));
}

/* Possibly output code to build up a constant in a register.  For
   the benefit of the costs infrastructure, returns the number of
   instructions which would be emitted.  GENERATE inhibits or
   enables code generation.  */

static int
aarch64_build_constant (int regnum, HOST_WIDE_INT val, bool generate)
{
  int insns = 0;

  if (aarch64_bitmask_imm (val, DImode))
    {
      if (generate)
	emit_move_insn (gen_rtx_REG (Pmode, regnum), GEN_INT (val));
      insns = 1;
    }
  else
    {
      int i;
      int ncount = 0;
      int zcount = 0;
      HOST_WIDE_INT valp = val >> 16;
      HOST_WIDE_INT valm;
      HOST_WIDE_INT tval;

      for (i = 16; i < 64; i += 16)
	{
	  valm = (valp & 0xffff);

	  if (valm != 0)
	    ++ zcount;

	  if (valm != 0xffff)
	    ++ ncount;

	  valp >>= 16;
	}

      /* zcount contains the number of additional MOVK instructions
	 required if the constant is built up with an initial MOVZ instruction,
	 while ncount is the number of MOVK instructions required if starting
	 with a MOVN instruction.  Choose the sequence that yields the fewest
	 number of instructions, preferring MOVZ instructions when they are both
	 the same.  */
      if (ncount < zcount)
	{
	  if (generate)
	    emit_move_insn (gen_rtx_REG (Pmode, regnum),
			    GEN_INT (val | ~(HOST_WIDE_INT) 0xffff));
	  tval = 0xffff;
	  insns++;
	}
      else
	{
	  if (generate)
	    emit_move_insn (gen_rtx_REG (Pmode, regnum),
			    GEN_INT (val & 0xffff));
	  tval = 0;
	  insns++;
	}

      val >>= 16;

      for (i = 16; i < 64; i += 16)
	{
	  if ((val & 0xffff) != tval)
	    {
	      if (generate)
		emit_insn (gen_insv_immdi (gen_rtx_REG (Pmode, regnum),
					   GEN_INT (i),
					   GEN_INT (val & 0xffff)));
	      insns++;
	    }
	  val >>= 16;
	}
    }
  return insns;
}

static void
aarch64_add_constant (int regnum, int scratchreg, HOST_WIDE_INT delta)
{
  HOST_WIDE_INT mdelta = delta;
  rtx this_rtx = gen_rtx_REG (Pmode, regnum);
  rtx scratch_rtx = gen_rtx_REG (Pmode, scratchreg);

  if (mdelta < 0)
    mdelta = -mdelta;

  if (mdelta >= 4096 * 4096)
    {
      (void) aarch64_build_constant (scratchreg, delta, true);
      emit_insn (gen_add3_insn (this_rtx, this_rtx, scratch_rtx));
    }
  else if (mdelta > 0)
    {
      if (mdelta >= 4096)
	{
	  emit_insn (gen_rtx_SET (Pmode, scratch_rtx, GEN_INT (mdelta / 4096)));
	  rtx shift = gen_rtx_ASHIFT (Pmode, scratch_rtx, GEN_INT (12));
	  if (delta < 0)
	    emit_insn (gen_rtx_SET (Pmode, this_rtx,
				    gen_rtx_MINUS (Pmode, this_rtx, shift)));
	  else
	    emit_insn (gen_rtx_SET (Pmode, this_rtx,
				    gen_rtx_PLUS (Pmode, this_rtx, shift)));
	}
      if (mdelta % 4096 != 0)
	{
	  scratch_rtx = GEN_INT ((delta < 0 ? -1 : 1) * (mdelta % 4096));
	  emit_insn (gen_rtx_SET (Pmode, this_rtx,
				  gen_rtx_PLUS (Pmode, this_rtx, scratch_rtx)));
	}
    }
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */
static void
aarch64_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
			 HOST_WIDE_INT delta,
			 HOST_WIDE_INT vcall_offset,
			 tree function)
{
  /* The this pointer is always in x0.  Note that this differs from
     Arm where the this pointer maybe bumped to r1 if r0 is required
     to return a pointer to an aggregate.  On AArch64 a result value
     pointer will be in x8.  */
  int this_regno = R0_REGNUM;
  rtx this_rtx, temp0, temp1, addr, funexp;
  rtx_insn *insn;

  reload_completed = 1;
  emit_note (NOTE_INSN_PROLOGUE_END);

  if (vcall_offset == 0)
    aarch64_add_constant (this_regno, IP1_REGNUM, delta);
  else
    {
      gcc_assert ((vcall_offset & (POINTER_BYTES - 1)) == 0);

      this_rtx = gen_rtx_REG (Pmode, this_regno);
      temp0 = gen_rtx_REG (Pmode, IP0_REGNUM);
      temp1 = gen_rtx_REG (Pmode, IP1_REGNUM);

      addr = this_rtx;
      if (delta != 0)
	{
	  if (delta >= -256 && delta < 256)
	    addr = gen_rtx_PRE_MODIFY (Pmode, this_rtx,
				       plus_constant (Pmode, this_rtx, delta));
	  else
	    aarch64_add_constant (this_regno, IP1_REGNUM, delta);
	}

      if (Pmode == ptr_mode)
	aarch64_emit_move (temp0, gen_rtx_MEM (ptr_mode, addr));
      else
	aarch64_emit_move (temp0,
			   gen_rtx_ZERO_EXTEND (Pmode,
						gen_rtx_MEM (ptr_mode, addr)));

      if (vcall_offset >= -256 && vcall_offset < 4096 * POINTER_BYTES)
	  addr = plus_constant (Pmode, temp0, vcall_offset);
      else
	{
	  (void) aarch64_build_constant (IP1_REGNUM, vcall_offset, true);
	  addr = gen_rtx_PLUS (Pmode, temp0, temp1);
	}

      if (Pmode == ptr_mode)
	aarch64_emit_move (temp1, gen_rtx_MEM (ptr_mode,addr));
      else
	aarch64_emit_move (temp1,
			   gen_rtx_SIGN_EXTEND (Pmode,
						gen_rtx_MEM (ptr_mode, addr)));

      emit_insn (gen_add2_insn (this_rtx, temp1));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
  insn = emit_call_insn (gen_sibcall (funexp, const0_rtx, NULL_RTX));
  SIBLING_CALL_P (insn) = 1;

  insn = get_insns ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}

static bool
aarch64_tls_referenced_p (rtx x)
{
  if (!TARGET_HAVE_TLS)
    return false;
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, ALL)
    {
      const_rtx x = *iter;
      if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x) != 0)
	return true;
      /* Don't recurse into UNSPEC_TLS looking for TLS symbols; these are
	 TLS offsets, not real symbol references.  */
      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
	iter.skip_subrtxes ();
    }
  return false;
}


static int
aarch64_bitmasks_cmp (const void *i1, const void *i2)
{
  const unsigned HOST_WIDE_INT *imm1 = (const unsigned HOST_WIDE_INT *) i1;
  const unsigned HOST_WIDE_INT *imm2 = (const unsigned HOST_WIDE_INT *) i2;

  if (*imm1 < *imm2)
    return -1;
  if (*imm1 > *imm2)
    return +1;
  return 0;
}


static void
aarch64_build_bitmask_table (void)
{
  unsigned HOST_WIDE_INT mask, imm;
  unsigned int log_e, e, s, r;
  unsigned int nimms = 0;

  for (log_e = 1; log_e <= 6; log_e++)
    {
      e = 1 << log_e;
      if (e == 64)
	mask = ~(HOST_WIDE_INT) 0;
      else
	mask = ((HOST_WIDE_INT) 1 << e) - 1;
      for (s = 1; s < e; s++)
	{
	  for (r = 0; r < e; r++)
	    {
	      /* set s consecutive bits to 1 (s < 64) */
	      imm = ((unsigned HOST_WIDE_INT)1 << s) - 1;
	      /* rotate right by r */
	      if (r != 0)
		imm = ((imm >> r) | (imm << (e - r))) & mask;
	      /* replicate the constant depending on SIMD size */
	      switch (log_e) {
	      case 1: imm |= (imm <<  2);
	      case 2: imm |= (imm <<  4);
	      case 3: imm |= (imm <<  8);
	      case 4: imm |= (imm << 16);
	      case 5: imm |= (imm << 32);
	      case 6:
		break;
	      default:
		gcc_unreachable ();
	      }
	      gcc_assert (nimms < AARCH64_NUM_BITMASKS);
	      aarch64_bitmasks[nimms++] = imm;
	    }
	}
    }

  gcc_assert (nimms == AARCH64_NUM_BITMASKS);
  qsort (aarch64_bitmasks, nimms, sizeof (aarch64_bitmasks[0]),
	 aarch64_bitmasks_cmp);
}


/* Return true if val can be encoded as a 12-bit unsigned immediate with
   a left shift of 0 or 12 bits.  */
bool
aarch64_uimm12_shift (HOST_WIDE_INT val)
{
  return ((val & (((HOST_WIDE_INT) 0xfff) << 0)) == val
	  || (val & (((HOST_WIDE_INT) 0xfff) << 12)) == val
	  );
}


/* Return true if val is an immediate that can be loaded into a
   register by a MOVZ instruction.  */
static bool
aarch64_movw_imm (HOST_WIDE_INT val, machine_mode mode)
{
  if (GET_MODE_SIZE (mode) > 4)
    {
      if ((val & (((HOST_WIDE_INT) 0xffff) << 32)) == val
	  || (val & (((HOST_WIDE_INT) 0xffff) << 48)) == val)
	return 1;
    }
  else
    {
      /* Ignore sign extension.  */
      val &= (HOST_WIDE_INT) 0xffffffff;
    }
  return ((val & (((HOST_WIDE_INT) 0xffff) << 0)) == val
	  || (val & (((HOST_WIDE_INT) 0xffff) << 16)) == val);
}


/* Return true if val is a valid bitmask immediate.  */
bool
aarch64_bitmask_imm (HOST_WIDE_INT val, machine_mode mode)
{
  if (GET_MODE_SIZE (mode) < 8)
    {
      /* Replicate bit pattern.  */
      val &= (HOST_WIDE_INT) 0xffffffff;
      val |= val << 32;
    }
  return bsearch (&val, aarch64_bitmasks, AARCH64_NUM_BITMASKS,
		  sizeof (aarch64_bitmasks[0]), aarch64_bitmasks_cmp) != NULL;
}


/* Return true if val is an immediate that can be loaded into a
   register in a single instruction.  */
bool
aarch64_move_imm (HOST_WIDE_INT val, machine_mode mode)
{
  if (aarch64_movw_imm (val, mode) || aarch64_movw_imm (~val, mode))
    return 1;
  return aarch64_bitmask_imm (val, mode);
}

static bool
aarch64_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  rtx base, offset;

  if (GET_CODE (x) == HIGH)
    return true;

  split_const (x, &base, &offset);
  if (GET_CODE (base) == SYMBOL_REF || GET_CODE (base) == LABEL_REF)
    {
      if (aarch64_classify_symbol (base, offset, SYMBOL_CONTEXT_ADR)
	  != SYMBOL_FORCE_TO_MEM)
	return true;
      else
	/* Avoid generating a 64-bit relocation in ILP32; leave
	   to aarch64_expand_mov_immediate to handle it properly.  */
	return mode != ptr_mode;
    }

  return aarch64_tls_referenced_p (x);
}

/* Return true if register REGNO is a valid index register.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
aarch64_regno_ok_for_index_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }
  return GP_REGNUM_P (regno);
}

/* Return true if register REGNO is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
aarch64_regno_ok_for_base_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  /* The fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  return (GP_REGNUM_P (regno)
	  || regno == SP_REGNUM
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if X is a valid base register for mode MODE.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
aarch64_base_register_rtx_p (rtx x, bool strict_p)
{
  if (!strict_p && GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);

  return (REG_P (x) && aarch64_regno_ok_for_base_p (REGNO (x), strict_p));
}

/* Return true if address offset is a valid index.  If it is, fill in INFO
   appropriately.  STRICT_P is true if REG_OK_STRICT is in effect.  */

static bool
aarch64_classify_index (struct aarch64_address_info *info, rtx x,
			machine_mode mode, bool strict_p)
{
  enum aarch64_address_type type;
  rtx index;
  int shift;

  /* (reg:P) */
  if ((REG_P (x) || GET_CODE (x) == SUBREG)
      && GET_MODE (x) == Pmode)
    {
      type = ADDRESS_REG_REG;
      index = x;
      shift = 0;
    }
  /* (sign_extend:DI (reg:SI)) */
  else if ((GET_CODE (x) == SIGN_EXTEND
	    || GET_CODE (x) == ZERO_EXTEND)
	   && GET_MODE (x) == DImode
	   && GET_MODE (XEXP (x, 0)) == SImode)
    {
      type = (GET_CODE (x) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (x, 0);
      shift = 0;
    }
  /* (mult:DI (sign_extend:DI (reg:SI)) (const_int scale)) */
  else if (GET_CODE (x) == MULT
	   && (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	       || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	   && GET_MODE (XEXP (x, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (x, 1)));
    }
  /* (ashift:DI (sign_extend:DI (reg:SI)) (const_int shift)) */
  else if (GET_CODE (x) == ASHIFT
	   && (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	       || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	   && GET_MODE (XEXP (x, 0)) == DImode
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (x, 1));
    }
  /* (sign_extract:DI (mult:DI (reg:DI) (const_int scale)) 32+shift 0) */
  else if ((GET_CODE (x) == SIGN_EXTRACT
	    || GET_CODE (x) == ZERO_EXTRACT)
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == MULT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    {
      type = (GET_CODE (x) == SIGN_EXTRACT)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)));
      if (INTVAL (XEXP (x, 1)) != 32 + shift
	  || INTVAL (XEXP (x, 2)) != 0)
	shift = -1;
    }
  /* (and:DI (mult:DI (reg:DI) (const_int scale))
     (const_int 0xffffffff<<shift)) */
  else if (GET_CODE (x) == AND
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == MULT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)));
      if (INTVAL (XEXP (x, 1)) != (HOST_WIDE_INT)0xffffffff << shift)
	shift = -1;
    }
  /* (sign_extract:DI (ashift:DI (reg:DI) (const_int shift)) 32+shift 0) */
  else if ((GET_CODE (x) == SIGN_EXTRACT
	    || GET_CODE (x) == ZERO_EXTRACT)
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == ASHIFT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    {
      type = (GET_CODE (x) == SIGN_EXTRACT)
	? ADDRESS_REG_SXTW : ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (XEXP (x, 0), 1));
      if (INTVAL (XEXP (x, 1)) != 32 + shift
	  || INTVAL (XEXP (x, 2)) != 0)
	shift = -1;
    }
  /* (and:DI (ashift:DI (reg:DI) (const_int shift))
     (const_int 0xffffffff<<shift)) */
  else if (GET_CODE (x) == AND
	   && GET_MODE (x) == DImode
	   && GET_CODE (XEXP (x, 0)) == ASHIFT
	   && GET_MODE (XEXP (XEXP (x, 0), 0)) == DImode
	   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_UXTW;
      index = XEXP (XEXP (x, 0), 0);
      shift = INTVAL (XEXP (XEXP (x, 0), 1));
      if (INTVAL (XEXP (x, 1)) != (HOST_WIDE_INT)0xffffffff << shift)
	shift = -1;
    }
  /* (mult:P (reg:P) (const_int scale)) */
  else if (GET_CODE (x) == MULT
	   && GET_MODE (x) == Pmode
	   && GET_MODE (XEXP (x, 0)) == Pmode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_REG;
      index = XEXP (x, 0);
      shift = exact_log2 (INTVAL (XEXP (x, 1)));
    }
  /* (ashift:P (reg:P) (const_int shift)) */
  else if (GET_CODE (x) == ASHIFT
	   && GET_MODE (x) == Pmode
	   && GET_MODE (XEXP (x, 0)) == Pmode
	   && CONST_INT_P (XEXP (x, 1)))
    {
      type = ADDRESS_REG_REG;
      index = XEXP (x, 0);
      shift = INTVAL (XEXP (x, 1));
    }
  else
    return false;

  if (GET_CODE (index) == SUBREG)
    index = SUBREG_REG (index);

  if ((shift == 0 ||
       (shift > 0 && shift <= 3
	&& (1 << shift) == GET_MODE_SIZE (mode)))
      && REG_P (index)
      && aarch64_regno_ok_for_index_p (REGNO (index), strict_p))
    {
      info->type = type;
      info->offset = index;
      info->shift = shift;
      return true;
    }

  return false;
}

bool
aarch64_offset_7bit_signed_scaled_p (machine_mode mode, HOST_WIDE_INT offset)
{
  return (offset >= -64 * GET_MODE_SIZE (mode)
	  && offset < 64 * GET_MODE_SIZE (mode)
	  && offset % GET_MODE_SIZE (mode) == 0);
}

static inline bool
offset_9bit_signed_unscaled_p (machine_mode mode ATTRIBUTE_UNUSED,
			       HOST_WIDE_INT offset)
{
  return offset >= -256 && offset < 256;
}

static inline bool
offset_12bit_unsigned_scaled_p (machine_mode mode, HOST_WIDE_INT offset)
{
  return (offset >= 0
	  && offset < 4096 * GET_MODE_SIZE (mode)
	  && offset % GET_MODE_SIZE (mode) == 0);
}

/* Return true if X is a valid address for machine mode MODE.  If it is,
   fill in INFO appropriately.  STRICT_P is true if REG_OK_STRICT is in
   effect.  OUTER_CODE is PARALLEL for a load/store pair.  */

static bool
aarch64_classify_address (struct aarch64_address_info *info,
			  rtx x, machine_mode mode,
			  RTX_CODE outer_code, bool strict_p)
{
  enum rtx_code code = GET_CODE (x);
  rtx op0, op1;

  /* On BE, we use load/store pair for all large int mode load/stores.  */
  bool load_store_pair_p = (outer_code == PARALLEL
			    || (BYTES_BIG_ENDIAN
				&& aarch64_vect_struct_mode_p (mode)));

  bool allow_reg_index_p =
    !load_store_pair_p
    && (GET_MODE_SIZE (mode) != 16 || aarch64_vector_mode_supported_p (mode))
    && !aarch64_vect_struct_mode_p (mode);

  /* On LE, for AdvSIMD, don't support anything other than POST_INC or
     REG addressing.  */
  if (aarch64_vect_struct_mode_p (mode) && !BYTES_BIG_ENDIAN
      && (code != POST_INC && code != REG))
    return false;

  switch (code)
    {
    case REG:
    case SUBREG:
      info->type = ADDRESS_REG_IMM;
      info->base = x;
      info->offset = const0_rtx;
      return aarch64_base_register_rtx_p (x, strict_p);

    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (! strict_p
	  && REG_P (op0)
	  && (op0 == virtual_stack_vars_rtx
	      || op0 == frame_pointer_rtx
	      || op0 == arg_pointer_rtx)
	  && CONST_INT_P (op1))
	{
	  info->type = ADDRESS_REG_IMM;
	  info->base = op0;
	  info->offset = op1;

	  return true;
	}

      if (GET_MODE_SIZE (mode) != 0
	  && CONST_INT_P (op1)
	  && aarch64_base_register_rtx_p (op0, strict_p))
	{
	  HOST_WIDE_INT offset = INTVAL (op1);

	  info->type = ADDRESS_REG_IMM;
	  info->base = op0;
	  info->offset = op1;

	  /* TImode and TFmode values are allowed in both pairs of X
	     registers and individual Q registers.  The available
	     address modes are:
	     X,X: 7-bit signed scaled offset
	     Q:   9-bit signed offset
	     We conservatively require an offset representable in either mode.
	   */
	  if (mode == TImode || mode == TFmode)
	    return (aarch64_offset_7bit_signed_scaled_p (mode, offset)
		    && offset_9bit_signed_unscaled_p (mode, offset));

	  /* A 7bit offset check because OImode will emit a ldp/stp
	     instruction (only big endian will get here).
	     For ldp/stp instructions, the offset is scaled for the size of a
	     single element of the pair.  */
	  if (mode == OImode)
	    return aarch64_offset_7bit_signed_scaled_p (TImode, offset);

	  /* Three 9/12 bit offsets checks because CImode will emit three
	     ldr/str instructions (only big endian will get here).  */
	  if (mode == CImode)
	    return (aarch64_offset_7bit_signed_scaled_p (TImode, offset)
		    && (offset_9bit_signed_unscaled_p (V16QImode, offset + 32)
			|| offset_12bit_unsigned_scaled_p (V16QImode,
							   offset + 32)));

	  /* Two 7bit offsets checks because XImode will emit two ldp/stp
	     instructions (only big endian will get here).  */
	  if (mode == XImode)
	    return (aarch64_offset_7bit_signed_scaled_p (TImode, offset)
		    && aarch64_offset_7bit_signed_scaled_p (TImode,
							    offset + 32));

	  if (load_store_pair_p)
	    return ((GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)
		    && aarch64_offset_7bit_signed_scaled_p (mode, offset));
	  else
	    return (offset_9bit_signed_unscaled_p (mode, offset)
		    || offset_12bit_unsigned_scaled_p (mode, offset));
	}

      if (allow_reg_index_p)
	{
	  /* Look for base + (scaled/extended) index register.  */
	  if (aarch64_base_register_rtx_p (op0, strict_p)
	      && aarch64_classify_index (info, op1, mode, strict_p))
	    {
	      info->base = op0;
	      return true;
	    }
	  if (aarch64_base_register_rtx_p (op1, strict_p)
	      && aarch64_classify_index (info, op0, mode, strict_p))
	    {
	      info->base = op1;
	      return true;
	    }
	}

      return false;

    case POST_INC:
    case POST_DEC:
    case PRE_INC:
    case PRE_DEC:
      info->type = ADDRESS_REG_WB;
      info->base = XEXP (x, 0);
      info->offset = NULL_RTX;
      return aarch64_base_register_rtx_p (info->base, strict_p);

    case POST_MODIFY:
    case PRE_MODIFY:
      info->type = ADDRESS_REG_WB;
      info->base = XEXP (x, 0);
      if (GET_CODE (XEXP (x, 1)) == PLUS
	  && CONST_INT_P (XEXP (XEXP (x, 1), 1))
	  && rtx_equal_p (XEXP (XEXP (x, 1), 0), info->base)
	  && aarch64_base_register_rtx_p (info->base, strict_p))
	{
	  HOST_WIDE_INT offset;
	  info->offset = XEXP (XEXP (x, 1), 1);
	  offset = INTVAL (info->offset);

	  /* TImode and TFmode values are allowed in both pairs of X
	     registers and individual Q registers.  The available
	     address modes are:
	     X,X: 7-bit signed scaled offset
	     Q:   9-bit signed offset
	     We conservatively require an offset representable in either mode.
	   */
	  if (mode == TImode || mode == TFmode)
	    return (aarch64_offset_7bit_signed_scaled_p (mode, offset)
		    && offset_9bit_signed_unscaled_p (mode, offset));

	  if (load_store_pair_p)
	    return ((GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)
		    && aarch64_offset_7bit_signed_scaled_p (mode, offset));
	  else
	    return offset_9bit_signed_unscaled_p (mode, offset);
	}
      return false;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* load literal: pc-relative constant pool entry.  Only supported
         for SI mode or larger.  */
      info->type = ADDRESS_SYMBOLIC;

      if (!load_store_pair_p && GET_MODE_SIZE (mode) >= 4)
	{
	  rtx sym, addend;

	  split_const (x, &sym, &addend);
	  return (GET_CODE (sym) == LABEL_REF
		  || (GET_CODE (sym) == SYMBOL_REF
		      && CONSTANT_POOL_ADDRESS_P (sym)));
	}
      return false;

    case LO_SUM:
      info->type = ADDRESS_LO_SUM;
      info->base = XEXP (x, 0);
      info->offset = XEXP (x, 1);
      if (allow_reg_index_p
	  && aarch64_base_register_rtx_p (info->base, strict_p))
	{
	  rtx sym, offs;
	  split_const (info->offset, &sym, &offs);
	  if (GET_CODE (sym) == SYMBOL_REF
	      && (aarch64_classify_symbol (sym, offs, SYMBOL_CONTEXT_MEM)
		  == SYMBOL_SMALL_ABSOLUTE))
	    {
	      /* The symbol and offset must be aligned to the access size.  */
	      unsigned int align;
	      unsigned int ref_size;

	      if (CONSTANT_POOL_ADDRESS_P (sym))
		align = GET_MODE_ALIGNMENT (get_pool_mode (sym));
	      else if (TREE_CONSTANT_POOL_ADDRESS_P (sym))
		{
		  tree exp = SYMBOL_REF_DECL (sym);
		  align = TYPE_ALIGN (TREE_TYPE (exp));
		  align = CONSTANT_ALIGNMENT (exp, align);
		}
	      else if (SYMBOL_REF_DECL (sym))
		align = DECL_ALIGN (SYMBOL_REF_DECL (sym));
	      else if (SYMBOL_REF_HAS_BLOCK_INFO_P (sym)
		       && SYMBOL_REF_BLOCK (sym) != NULL)
		align = SYMBOL_REF_BLOCK (sym)->alignment;
	      else
		align = BITS_PER_UNIT;

	      ref_size = GET_MODE_SIZE (mode);
	      if (ref_size == 0)
		ref_size = GET_MODE_SIZE (DImode);

	      return ((INTVAL (offs) & (ref_size - 1)) == 0
		      && ((align / BITS_PER_UNIT) & (ref_size - 1)) == 0);
	    }
	}
      return false;

    default:
      return false;
    }
}

bool
aarch64_symbolic_address_p (rtx x)
{
  rtx offset;

  split_const (x, &x, &offset);
  return GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF;
}

/* Classify the base of symbolic expression X, given that X appears in
   context CONTEXT.  */

enum aarch64_symbol_type
aarch64_classify_symbolic_expression (rtx x,
				      enum aarch64_symbol_context context)
{
  rtx offset;

  split_const (x, &x, &offset);
  return aarch64_classify_symbol (x, offset, context);
}


/* Return TRUE if X is a legitimate address for accessing memory in
   mode MODE.  */
static bool
aarch64_legitimate_address_hook_p (machine_mode mode, rtx x, bool strict_p)
{
  struct aarch64_address_info addr;

  return aarch64_classify_address (&addr, x, mode, MEM, strict_p);
}

/* Return TRUE if X is a legitimate address for accessing memory in
   mode MODE.  OUTER_CODE will be PARALLEL if this is a load/store
   pair operation.  */
bool
aarch64_legitimate_address_p (machine_mode mode, rtx x,
			      RTX_CODE outer_code, bool strict_p)
{
  struct aarch64_address_info addr;

  return aarch64_classify_address (&addr, x, mode, outer_code, strict_p);
}

/* Return TRUE if rtx X is immediate constant 0.0 */
bool
aarch64_float_const_zero_rtx_p (rtx x)
{
  REAL_VALUE_TYPE r;

  if (GET_MODE (x) == VOIDmode)
    return false;

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  if (REAL_VALUE_MINUS_ZERO (r))
    return !HONOR_SIGNED_ZEROS (GET_MODE (x));
  return REAL_VALUES_EQUAL (r, dconst0);
}

/* Return the fixed registers used for condition codes.  */

static bool
aarch64_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REGNUM;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Emit call insn with PAT and do aarch64-specific handling.  */

void
aarch64_emit_call_insn (rtx pat)
{
  rtx insn = emit_call_insn (pat);

  rtx *fusage = &CALL_INSN_FUNCTION_USAGE (insn);
  clobber_reg (fusage, gen_rtx_REG (word_mode, IP0_REGNUM));
  clobber_reg (fusage, gen_rtx_REG (word_mode, IP1_REGNUM));
}

machine_mode
aarch64_select_cc_mode (RTX_CODE code, rtx x, rtx y)
{
  /* All floating point compares return CCFP if it is an equality
     comparison, and CCFPE otherwise.  */
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      switch (code)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	case LTGT:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	default:
	  gcc_unreachable ();
	}
    }

  if ((GET_MODE (x) == SImode || GET_MODE (x) == DImode)
      && y == const0_rtx
      && (code == EQ || code == NE || code == LT || code == GE)
      && (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS || GET_CODE (x) == AND
	  || GET_CODE (x) == NEG))
    return CC_NZmode;

  /* A compare with a shifted operand.  Because of canonicalization,
     the comparison will have to be swapped when we emit the assembly
     code.  */
  if ((GET_MODE (x) == SImode || GET_MODE (x) == DImode)
      && (REG_P (y) || GET_CODE (y) == SUBREG)
      && (GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == LSHIFTRT
	  || GET_CODE (x) == ZERO_EXTEND || GET_CODE (x) == SIGN_EXTEND))
    return CC_SWPmode;

  /* Similarly for a negated operand, but we can only do this for
     equalities.  */
  if ((GET_MODE (x) == SImode || GET_MODE (x) == DImode)
      && (REG_P (y) || GET_CODE (y) == SUBREG)
      && (code == EQ || code == NE)
      && GET_CODE (x) == NEG)
    return CC_Zmode;

  /* A compare of a mode narrower than SI mode against zero can be done
     by extending the value in the comparison.  */
  if ((GET_MODE (x) == QImode || GET_MODE (x) == HImode)
      && y == const0_rtx)
    /* Only use sign-extension if we really need it.  */
    return ((code == GT || code == GE || code == LE || code == LT)
	    ? CC_SESWPmode : CC_ZESWPmode);

  /* A test for unsigned overflow.  */
  if ((GET_MODE (x) == DImode || GET_MODE (x) == TImode)
      && code == NE
      && GET_CODE (x) == PLUS
      && GET_CODE (y) == ZERO_EXTEND)
    return CC_Cmode;

  /* For everything else, return CCmode.  */
  return CCmode;
}

static int
aarch64_get_condition_code_1 (enum machine_mode, enum rtx_code);

int
aarch64_get_condition_code (rtx x)
{
  machine_mode mode = GET_MODE (XEXP (x, 0));
  enum rtx_code comp_code = GET_CODE (x);

  if (GET_MODE_CLASS (mode) != MODE_CC)
    mode = SELECT_CC_MODE (comp_code, XEXP (x, 0), XEXP (x, 1));
  return aarch64_get_condition_code_1 (mode, comp_code);
}

static int
aarch64_get_condition_code_1 (enum machine_mode mode, enum rtx_code comp_code)
{
  int ne = -1, eq = -1;
  switch (mode)
    {
    case CCFPmode:
    case CCFPEmode:
      switch (comp_code)
	{
	case GE: return AARCH64_GE;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LS;
	case LT: return AARCH64_MI;
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case ORDERED: return AARCH64_VC;
	case UNORDERED: return AARCH64_VS;
	case UNLT: return AARCH64_LT;
	case UNLE: return AARCH64_LE;
	case UNGT: return AARCH64_HI;
	case UNGE: return AARCH64_PL;
	default: return -1;
	}
      break;

    case CC_DNEmode:
      ne = AARCH64_NE;
      eq = AARCH64_EQ;
      break;

    case CC_DEQmode:
      ne = AARCH64_EQ;
      eq = AARCH64_NE;
      break;

    case CC_DGEmode:
      ne = AARCH64_GE;
      eq = AARCH64_LT;
      break;

    case CC_DLTmode:
      ne = AARCH64_LT;
      eq = AARCH64_GE;
      break;

    case CC_DGTmode:
      ne = AARCH64_GT;
      eq = AARCH64_LE;
      break;

    case CC_DLEmode:
      ne = AARCH64_LE;
      eq = AARCH64_GT;
      break;

    case CC_DGEUmode:
      ne = AARCH64_CS;
      eq = AARCH64_CC;
      break;

    case CC_DLTUmode:
      ne = AARCH64_CC;
      eq = AARCH64_CS;
      break;

    case CC_DGTUmode:
      ne = AARCH64_HI;
      eq = AARCH64_LS;
      break;

    case CC_DLEUmode:
      ne = AARCH64_LS;
      eq = AARCH64_HI;
      break;

    case CCmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_GE;
	case GT: return AARCH64_GT;
	case LE: return AARCH64_LE;
	case LT: return AARCH64_LT;
	case GEU: return AARCH64_CS;
	case GTU: return AARCH64_HI;
	case LEU: return AARCH64_LS;
	case LTU: return AARCH64_CC;
	default: return -1;
	}
      break;

    case CC_SWPmode:
    case CC_ZESWPmode:
    case CC_SESWPmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_LE;
	case GT: return AARCH64_LT;
	case LE: return AARCH64_GE;
	case LT: return AARCH64_GT;
	case GEU: return AARCH64_LS;
	case GTU: return AARCH64_CC;
	case LEU: return AARCH64_CS;
	case LTU: return AARCH64_HI;
	default: return -1;
	}
      break;

    case CC_NZmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	case GE: return AARCH64_PL;
	case LT: return AARCH64_MI;
	default: return -1;
	}
      break;

    case CC_Zmode:
      switch (comp_code)
	{
	case NE: return AARCH64_NE;
	case EQ: return AARCH64_EQ;
	default: return -1;
	}
      break;

    case CC_Cmode:
      switch (comp_code)
	{
	case NE: return AARCH64_CS;
	case EQ: return AARCH64_CC;
	default: return -1;
	}
      break;

    default:
      return -1;
      break;
    }

  if (comp_code == NE)
    return ne;

  if (comp_code == EQ)
    return eq;

  return -1;
}

bool
aarch64_const_vec_all_same_in_range_p (rtx x,
				  HOST_WIDE_INT minval,
				  HOST_WIDE_INT maxval)
{
  HOST_WIDE_INT firstval;
  int count, i;

  if (GET_CODE (x) != CONST_VECTOR
      || GET_MODE_CLASS (GET_MODE (x)) != MODE_VECTOR_INT)
    return false;

  firstval = INTVAL (CONST_VECTOR_ELT (x, 0));
  if (firstval < minval || firstval > maxval)
    return false;

  count = CONST_VECTOR_NUNITS (x);
  for (i = 1; i < count; i++)
    if (INTVAL (CONST_VECTOR_ELT (x, i)) != firstval)
      return false;

  return true;
}

bool
aarch64_const_vec_all_same_int_p (rtx x, HOST_WIDE_INT val)
{
  return aarch64_const_vec_all_same_in_range_p (x, val, val);
}

static unsigned
bit_count (unsigned HOST_WIDE_INT value)
{
  unsigned count = 0;

  while (value)
    {
      count++;
      value &= value - 1;
    }

  return count;
}

/* N Z C V.  */
#define AARCH64_CC_V 1
#define AARCH64_CC_C (1 << 1)
#define AARCH64_CC_Z (1 << 2)
#define AARCH64_CC_N (1 << 3)

/* N Z C V flags for ccmp.  The first code is for AND op and the other
   is for IOR op.  Indexed by AARCH64_COND_CODE.  */
static const int aarch64_nzcv_codes[][2] =
{
  {AARCH64_CC_Z, 0}, /* EQ, Z == 1.  */
  {0, AARCH64_CC_Z}, /* NE, Z == 0.  */
  {AARCH64_CC_C, 0}, /* CS, C == 1.  */
  {0, AARCH64_CC_C}, /* CC, C == 0.  */
  {AARCH64_CC_N, 0}, /* MI, N == 1.  */
  {0, AARCH64_CC_N}, /* PL, N == 0.  */
  {AARCH64_CC_V, 0}, /* VS, V == 1.  */
  {0, AARCH64_CC_V}, /* VC, V == 0.  */
  {AARCH64_CC_C, 0}, /* HI, C ==1 && Z == 0.  */
  {0, AARCH64_CC_C}, /* LS, !(C == 1 && Z == 0).  */
  {0, AARCH64_CC_V}, /* GE, N == V.  */
  {AARCH64_CC_V, 0}, /* LT, N != V.  */
  {0, AARCH64_CC_Z}, /* GT, Z == 0 && N == V.  */
  {AARCH64_CC_Z, 0}, /* LE, !(Z == 0 && N == V).  */
  {0, 0}, /* AL, Any.  */
  {0, 0}, /* NV, Any.  */
};

int
aarch64_ccmp_mode_to_code (enum machine_mode mode)
{
  switch (mode)
    {
    case CC_DNEmode:
      return NE;

    case CC_DEQmode:
      return EQ;

    case CC_DLEmode:
      return LE;

    case CC_DGTmode:
      return GT;

    case CC_DLTmode:
      return LT;

    case CC_DGEmode:
      return GE;

    case CC_DLEUmode:
      return LEU;

    case CC_DGTUmode:
      return GTU;

    case CC_DLTUmode:
      return LTU;

    case CC_DGEUmode:
      return GEU;

    default:
      gcc_unreachable ();
    }
}


void
aarch64_print_operand (FILE *f, rtx x, char code)
{
  switch (code)
    {
    /* An integer or symbol address without a preceding # sign.  */
    case 'c':
      switch (GET_CODE (x))
	{
	case CONST_INT:
	  fprintf (f, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
	  break;

	case SYMBOL_REF:
	  output_addr_const (f, x);
	  break;

	case CONST:
	  if (GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)
	    {
	      output_addr_const (f, x);
	      break;
	    }
	  /* Fall through.  */

	default:
	  output_operand_lossage ("Unsupported operand for code '%c'", code);
	}
      break;

    case 'e':
      /* Print the sign/zero-extend size as a character 8->b, 16->h, 32->w.  */
      {
	int n;

	if (!CONST_INT_P (x)
	    || (n = exact_log2 (INTVAL (x) & ~7)) <= 0)
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	switch (n)
	  {
	  case 3:
	    fputc ('b', f);
	    break;
	  case 4:
	    fputc ('h', f);
	    break;
	  case 5:
	    fputc ('w', f);
	    break;
	  default:
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
      }
      break;

    case 'p':
      {
	int n;

	/* Print N such that 2^N == X.  */
	if (!CONST_INT_P (x) || (n = exact_log2 (INTVAL (x))) < 0)
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	asm_fprintf (f, "%d", n);
      }
      break;

    case 'P':
      /* Print the number of non-zero bits in X (a const_int).  */
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}

      asm_fprintf (f, "%u", bit_count (INTVAL (x)));
      break;

    case 'H':
      /* Print the higher numbered register of a pair (TImode) of regs.  */
      if (!REG_P (x) || !GP_REGNUM_P (REGNO (x) + 1))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}

      asm_fprintf (f, "%s", reg_names [REGNO (x) + 1]);
      break;

    case 'm':
      {
        int cond_code;
        /* Print a condition (eq, ne, etc).  */

        /* CONST_TRUE_RTX means always -- that's the default.  */
        if (x == const_true_rtx)
	  return;

        if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

        cond_code = aarch64_get_condition_code (x);
        gcc_assert (cond_code >= 0);
        fputs (aarch64_condition_codes[cond_code], f);
      }
      break;

    case 'M':
      {
        int cond_code;
        /* Print the inverse of a condition (eq <-> ne, etc).  */

        /* CONST_TRUE_RTX means never -- that's the default.  */
        if (x == const_true_rtx)
	  {
	    fputs ("nv", f);
	    return;
	  }

        if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }
        cond_code = aarch64_get_condition_code (x);
        gcc_assert (cond_code >= 0);
        fputs (aarch64_condition_codes[AARCH64_INVERSE_CONDITION_CODE
                                       (cond_code)], f);
      }
      break;

    case 'b':
    case 'h':
    case 's':
    case 'd':
    case 'q':
      /* Print a scalar FP/SIMD register name.  */
      if (!REG_P (x) || !FP_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("incompatible floating point / vector register operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "%c%d", code, REGNO (x) - V0_REGNUM);
      break;

    case 'S':
    case 'T':
    case 'U':
    case 'V':
      /* Print the first FP/SIMD register name in a list.  */
      if (!REG_P (x) || !FP_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("incompatible floating point / vector register operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "v%d", REGNO (x) - V0_REGNUM + (code - 'S'));
      break;

    case 'R':
      /* Print a scalar FP/SIMD register name + 1.  */
      if (!REG_P (x) || !FP_REGNUM_P (REGNO (x)))
	{
	  output_operand_lossage ("incompatible floating point / vector register operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "q%d", REGNO (x) - V0_REGNUM + 1);
      break;

    case 'X':
      /* Print bottom 16 bits of integer constant in hex.  */
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for '%%%c'", code);
	  return;
	}
      asm_fprintf (f, "0x%wx", UINTVAL (x) & 0xffff);
      break;

    case 'w':
    case 'x':
      /* Print a general register name or the zero register (32-bit or
         64-bit).  */
      if (x == const0_rtx
	  || (CONST_DOUBLE_P (x) && aarch64_float_const_zero_rtx_p (x)))
	{
	  asm_fprintf (f, "%czr", code);
	  break;
	}

      if (REG_P (x) && GP_REGNUM_P (REGNO (x)))
	{
	  asm_fprintf (f, "%c%d", code, REGNO (x) - R0_REGNUM);
	  break;
	}

      if (REG_P (x) && REGNO (x) == SP_REGNUM)
	{
	  asm_fprintf (f, "%ssp", code == 'w' ? "w" : "");
	  break;
	}

      /* Fall through */

    case 0:
      /* Print a normal operand, if it's a general register, then we
	 assume DImode.  */
      if (x == NULL)
	{
	  output_operand_lossage ("missing operand");
	  return;
	}

      switch (GET_CODE (x))
	{
	case REG:
	  asm_fprintf (f, "%s", reg_names [REGNO (x)]);
	  break;

	case MEM:
	  aarch64_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	  break;

	case LABEL_REF:
	case SYMBOL_REF:
	  output_addr_const (asm_out_file, x);
	  break;

	case CONST_INT:
	  asm_fprintf (f, "%wd", INTVAL (x));
	  break;

	case CONST_VECTOR:
	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_VECTOR_INT)
	    {
	      gcc_assert (
		  aarch64_const_vec_all_same_in_range_p (x,
							 HOST_WIDE_INT_MIN,
							 HOST_WIDE_INT_MAX));
	      asm_fprintf (f, "%wd", INTVAL (CONST_VECTOR_ELT (x, 0)));
	    }
	  else if (aarch64_simd_imm_zero_p (x, GET_MODE (x)))
	    {
	      fputc ('0', f);
	    }
	  else
	    gcc_unreachable ();
	  break;

	case CONST_DOUBLE:
	  /* Since we define TARGET_SUPPORTS_WIDE_INT we shouldn't ever
	     be getting CONST_DOUBLEs holding integers.  */
	  gcc_assert (GET_MODE (x) != VOIDmode);
	  if (aarch64_float_const_zero_rtx_p (x))
	    {
	      fputc ('0', f);
	      break;
	    }
	  else if (aarch64_float_const_representable_p (x))
	    {
#define buf_size 20
	      char float_buf[buf_size] = {'\0'};
	      REAL_VALUE_TYPE r;
	      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	      real_to_decimal_for_mode (float_buf, &r,
					buf_size, buf_size,
					1, GET_MODE (x));
	      asm_fprintf (asm_out_file, "%s", float_buf);
	      break;
#undef buf_size
	    }
	  output_operand_lossage ("invalid constant");
	  return;
	default:
	  output_operand_lossage ("invalid operand");
	  return;
	}
      break;

    case 'A':
      if (GET_CODE (x) == HIGH)
	x = XEXP (x, 0);

      switch (aarch64_classify_symbolic_expression (x, SYMBOL_CONTEXT_ADR))
	{
	case SYMBOL_SMALL_GOT:
	  asm_fprintf (asm_out_file, ":got:");
	  break;

	case SYMBOL_SMALL_TLSGD:
	  asm_fprintf (asm_out_file, ":tlsgd:");
	  break;

	case SYMBOL_SMALL_TLSDESC:
	  asm_fprintf (asm_out_file, ":tlsdesc:");
	  break;

	case SYMBOL_SMALL_GOTTPREL:
	  asm_fprintf (asm_out_file, ":gottprel:");
	  break;

	case SYMBOL_SMALL_TPREL:
	  asm_fprintf (asm_out_file, ":tprel:");
	  break;

	case SYMBOL_TINY_GOT:
	  gcc_unreachable ();
	  break;

	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'L':
      switch (aarch64_classify_symbolic_expression (x, SYMBOL_CONTEXT_ADR))
	{
	case SYMBOL_SMALL_GOT:
	  asm_fprintf (asm_out_file, ":lo12:");
	  break;

	case SYMBOL_SMALL_TLSGD:
	  asm_fprintf (asm_out_file, ":tlsgd_lo12:");
	  break;

	case SYMBOL_SMALL_TLSDESC:
	  asm_fprintf (asm_out_file, ":tlsdesc_lo12:");
	  break;

	case SYMBOL_SMALL_GOTTPREL:
	  asm_fprintf (asm_out_file, ":gottprel_lo12:");
	  break;

	case SYMBOL_SMALL_TPREL:
	  asm_fprintf (asm_out_file, ":tprel_lo12_nc:");
	  break;

	case SYMBOL_TINY_GOT:
	  asm_fprintf (asm_out_file, ":got:");
	  break;

	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'G':

      switch (aarch64_classify_symbolic_expression (x, SYMBOL_CONTEXT_ADR))
	{
	case SYMBOL_SMALL_TPREL:
	  asm_fprintf (asm_out_file, ":tprel_hi12:");
	  break;
	default:
	  break;
	}
      output_addr_const (asm_out_file, x);
      break;

    case 'K':
      {
	int cond_code;
	/* Print nzcv.  */

	if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	cond_code = aarch64_get_condition_code_1 (CCmode, GET_CODE (x));
	gcc_assert (cond_code >= 0);
	asm_fprintf (f, "%d", aarch64_nzcv_codes[cond_code][0]);
      }
      break;

    case 'k':
      {
	int cond_code;
	/* Print nzcv.  */

	if (!COMPARISON_P (x))
	  {
	    output_operand_lossage ("invalid operand for '%%%c'", code);
	    return;
	  }

	cond_code = aarch64_get_condition_code_1 (CCmode, GET_CODE (x));
	gcc_assert (cond_code >= 0);
	asm_fprintf (f, "%d", aarch64_nzcv_codes[cond_code][1]);
      }
      break;

    default:
      output_operand_lossage ("invalid operand prefix '%%%c'", code);
      return;
    }
}

void
aarch64_print_operand_address (FILE *f, rtx x)
{
  struct aarch64_address_info addr;

  if (aarch64_classify_address (&addr, x, aarch64_memory_reference_mode,
			     MEM, true))
    switch (addr.type)
      {
      case ADDRESS_REG_IMM:
	if (addr.offset == const0_rtx)
	  asm_fprintf (f, "[%s]", reg_names [REGNO (addr.base)]);
	else
	  asm_fprintf (f, "[%s, %wd]", reg_names [REGNO (addr.base)],
		       INTVAL (addr.offset));
	return;

      case ADDRESS_REG_REG:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, %s]", reg_names [REGNO (addr.base)],
		       reg_names [REGNO (addr.offset)]);
	else
	  asm_fprintf (f, "[%s, %s, lsl %u]", reg_names [REGNO (addr.base)],
		       reg_names [REGNO (addr.offset)], addr.shift);
	return;

      case ADDRESS_REG_UXTW:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, w%d, uxtw]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM);
	else
	  asm_fprintf (f, "[%s, w%d, uxtw %u]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM, addr.shift);
	return;

      case ADDRESS_REG_SXTW:
	if (addr.shift == 0)
	  asm_fprintf (f, "[%s, w%d, sxtw]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM);
	else
	  asm_fprintf (f, "[%s, w%d, sxtw %u]", reg_names [REGNO (addr.base)],
		       REGNO (addr.offset) - R0_REGNUM, addr.shift);
	return;

      case ADDRESS_REG_WB:
	switch (GET_CODE (x))
	  {
	  case PRE_INC:
	    asm_fprintf (f, "[%s, %d]!", reg_names [REGNO (addr.base)],
			 GET_MODE_SIZE (aarch64_memory_reference_mode));
	    return;
	  case POST_INC:
	    asm_fprintf (f, "[%s], %d", reg_names [REGNO (addr.base)],
			 GET_MODE_SIZE (aarch64_memory_reference_mode));
	    return;
	  case PRE_DEC:
	    asm_fprintf (f, "[%s, -%d]!", reg_names [REGNO (addr.base)],
			 GET_MODE_SIZE (aarch64_memory_reference_mode));
	    return;
	  case POST_DEC:
	    asm_fprintf (f, "[%s], -%d", reg_names [REGNO (addr.base)],
			 GET_MODE_SIZE (aarch64_memory_reference_mode));
	    return;
	  case PRE_MODIFY:
	    asm_fprintf (f, "[%s, %wd]!", reg_names [REGNO (addr.base)],
			 INTVAL (addr.offset));
	    return;
	  case POST_MODIFY:
	    asm_fprintf (f, "[%s], %wd", reg_names [REGNO (addr.base)],
			 INTVAL (addr.offset));
	    return;
	  default:
	    break;
	  }
	break;

      case ADDRESS_LO_SUM:
	asm_fprintf (f, "[%s, #:lo12:", reg_names [REGNO (addr.base)]);
	output_addr_const (f, addr.offset);
	asm_fprintf (f, "]");
	return;

      case ADDRESS_SYMBOLIC:
	break;
      }

  output_addr_const (f, x);
}

bool
aarch64_label_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return true;

  /* UNSPEC_TLS entries for a symbol include a LABEL_REF for the
     referencing instruction, but they are constant offsets, not
     symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return false;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (aarch64_label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && aarch64_label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Implement REGNO_REG_CLASS.  */

enum reg_class
aarch64_regno_regclass (unsigned regno)
{
  if (GP_REGNUM_P (regno))
    return GENERAL_REGS;

  if (regno == SP_REGNUM)
    return STACK_REG;

  if (regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return POINTER_REGS;

  if (FP_REGNUM_P (regno))
    return FP_LO_REGNUM_P (regno) ?  FP_LO_REGS : FP_REGS;

  return NO_REGS;
}

static rtx
aarch64_legitimize_address (rtx x, rtx /* orig_x  */, machine_mode mode)
{
  /* Try to split X+CONST into Y=X+(CONST & ~mask), Y+(CONST&mask),
     where mask is selected by alignment and size of the offset.
     We try to pick as large a range for the offset as possible to
     maximize the chance of a CSE.  However, for aligned addresses
     we limit the range to 4k so that structures with different sized
     elements are likely to use the same base.  */

  if (GET_CODE (x) == PLUS && CONST_INT_P (XEXP (x, 1)))
    {
      HOST_WIDE_INT offset = INTVAL (XEXP (x, 1));
      HOST_WIDE_INT base_offset;

      /* Does it look like we'll need a load/store-pair operation?  */
      if (GET_MODE_SIZE (mode) > 16
	  || mode == TImode)
	base_offset = ((offset + 64 * GET_MODE_SIZE (mode))
		       & ~((128 * GET_MODE_SIZE (mode)) - 1));
      /* For offsets aren't a multiple of the access size, the limit is
	 -256...255.  */
      else if (offset & (GET_MODE_SIZE (mode) - 1))
	base_offset = (offset + 0x100) & ~0x1ff;
      else
	base_offset = offset & ~0xfff;

      if (base_offset == 0)
	return x;

      offset -= base_offset;
      rtx base_reg = gen_reg_rtx (Pmode);
      rtx val = force_operand (plus_constant (Pmode, XEXP (x, 0), base_offset),
			   NULL_RTX);
      emit_move_insn (base_reg, val);
      x = plus_constant (Pmode, base_reg, offset);
    }

  return x;
}

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and return the new rtx.  */

rtx
aarch64_legitimize_reload_address (rtx *x_p,
				   machine_mode mode,
				   int opnum, int type,
				   int ind_levels ATTRIBUTE_UNUSED)
{
  rtx x = *x_p;

  /* Do not allow mem (plus (reg, const)) if vector struct mode.  */
  if (aarch64_vect_struct_mode_p (mode)
      && GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    {
      rtx orig_rtx = x;
      x = copy_rtx (x);
      push_reload (orig_rtx, NULL_RTX, x_p, NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      return x;
    }

  /* We must recognize output that we have already generated ourselves.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == PLUS
      && REG_P (XEXP (XEXP (x, 0), 0))
      && CONST_INT_P (XEXP (XEXP (x, 0), 1))
      && CONST_INT_P (XEXP (x, 1)))
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      return x;
    }

  /* We wish to handle large displacements off a base register by splitting
     the addend across an add and the mem insn.  This can cut the number of
     extra insns needed from 3 to 1.  It is only useful for load/store of a
     single register with 12 bit offset field.  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1))
      && HARD_REGISTER_P (XEXP (x, 0))
      && mode != TImode
      && mode != TFmode
      && aarch64_regno_ok_for_base_p (REGNO (XEXP (x, 0)), true))
    {
      HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
      HOST_WIDE_INT low = val & 0xfff;
      HOST_WIDE_INT high = val - low;
      HOST_WIDE_INT offs;
      rtx cst;
      machine_mode xmode = GET_MODE (x);

      /* In ILP32, xmode can be either DImode or SImode.  */
      gcc_assert (xmode == DImode || xmode == SImode);

      /* Reload non-zero BLKmode offsets.  This is because we cannot ascertain
	 BLKmode alignment.  */
      if (GET_MODE_SIZE (mode) == 0)
	return NULL_RTX;

      offs = low % GET_MODE_SIZE (mode);

      /* Align misaligned offset by adjusting high part to compensate.  */
      if (offs != 0)
	{
	  if (aarch64_uimm12_shift (high + offs))
	    {
	      /* Align down.  */
	      low = low - offs;
	      high = high + offs;
	    }
	  else
	    {
	      /* Align up.  */
	      offs = GET_MODE_SIZE (mode) - offs;
	      low = low + offs;
	      high = high + (low & 0x1000) - offs;
	      low &= 0xfff;
	    }
	}

      /* Check for overflow.  */
      if (high + low != val)
	return NULL_RTX;

      cst = GEN_INT (high);
      if (!aarch64_uimm12_shift (high))
	cst = force_const_mem (xmode, cst);

      /* Reload high part into base reg, leaving the low part
	 in the mem instruction.
	 Note that replacing this gen_rtx_PLUS with plus_constant is
	 wrong in this case because we rely on the
	 (plus (plus reg c1) c2) structure being preserved so that
	 XEXP (*p, 0) in push_reload below uses the correct term.  */
      x = gen_rtx_PLUS (xmode,
			gen_rtx_PLUS (xmode, XEXP (x, 0), cst),
			GEN_INT (low));

      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, xmode, VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      return x;
    }

  return NULL_RTX;
}


static reg_class_t
aarch64_secondary_reload (bool in_p ATTRIBUTE_UNUSED, rtx x,
			  reg_class_t rclass,
			  machine_mode mode,
			  secondary_reload_info *sri)
{
  /* Without the TARGET_SIMD instructions we cannot move a Q register
     to a Q register directly.  We need a scratch.  */
  if (REG_P (x) && (mode == TFmode || mode == TImode) && mode == GET_MODE (x)
      && FP_REGNUM_P (REGNO (x)) && !TARGET_SIMD
      && reg_class_subset_p (rclass, FP_REGS))
    {
      if (mode == TFmode)
        sri->icode = CODE_FOR_aarch64_reload_movtf;
      else if (mode == TImode)
        sri->icode = CODE_FOR_aarch64_reload_movti;
      return NO_REGS;
    }

  /* A TFmode or TImode memory access should be handled via an FP_REGS
     because AArch64 has richer addressing modes for LDR/STR instructions
     than LDP/STP instructions.  */
  if (TARGET_FLOAT && rclass == GENERAL_REGS
      && GET_MODE_SIZE (mode) == 16 && MEM_P (x))
    return FP_REGS;

  if (rclass == FP_REGS && (mode == TImode || mode == TFmode) && CONSTANT_P(x))
      return GENERAL_REGS;

  return NO_REGS;
}

static bool
aarch64_can_eliminate (const int from, const int to)
{
  /* If we need a frame pointer, we must eliminate FRAME_POINTER_REGNUM into
     HARD_FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM.  */

  if (frame_pointer_needed)
    {
      if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
	return true;
      if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
	return false;
      if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM
	  && !cfun->calls_alloca)
	return true;
      if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
	return true;

      return false;
    }
  else
    {
      /* If we decided that we didn't need a leaf frame pointer but then used
	 LR in the function, then we'll want a frame pointer after all, so
	 prevent this elimination to ensure a frame pointer is used.  */
      if (to == STACK_POINTER_REGNUM
	  && flag_omit_leaf_frame_pointer
	  && df_regs_ever_live_p (LR_REGNUM))
	return false;
    }

  return true;
}

HOST_WIDE_INT
aarch64_initial_elimination_offset (unsigned from, unsigned to)
{
  aarch64_layout_frame ();

  if (to == HARD_FRAME_POINTER_REGNUM)
    {
      if (from == ARG_POINTER_REGNUM)
	return cfun->machine->frame.frame_size - crtl->outgoing_args_size;

      if (from == FRAME_POINTER_REGNUM)
	return (cfun->machine->frame.hard_fp_offset
		- cfun->machine->frame.saved_varargs_size);
    }

  if (to == STACK_POINTER_REGNUM)
    {
      if (from == FRAME_POINTER_REGNUM)
	  return (cfun->machine->frame.frame_size
		  - cfun->machine->frame.saved_varargs_size);
    }

  return cfun->machine->frame.frame_size;
}

/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
aarch64_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;
  return get_hard_reg_initial_val (Pmode, LR_REGNUM);
}


static void
aarch64_asm_trampoline_template (FILE *f)
{
  if (TARGET_ILP32)
    {
      asm_fprintf (f, "\tldr\tw%d, .+16\n", IP1_REGNUM - R0_REGNUM);
      asm_fprintf (f, "\tldr\tw%d, .+16\n", STATIC_CHAIN_REGNUM - R0_REGNUM);
    }
  else
    {
      asm_fprintf (f, "\tldr\t%s, .+16\n", reg_names [IP1_REGNUM]);
      asm_fprintf (f, "\tldr\t%s, .+20\n", reg_names [STATIC_CHAIN_REGNUM]);
    }
  asm_fprintf (f, "\tbr\t%s\n", reg_names [IP1_REGNUM]);
  assemble_aligned_integer (4, const0_rtx);
  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
}

static void
aarch64_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr, mem, a_tramp;
  const int tramp_code_sz = 16;

  /* Don't need to copy the trailing D-words, we fill those in below.  */
  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (tramp_code_sz), BLOCK_OP_NORMAL);
  mem = adjust_address (m_tramp, ptr_mode, tramp_code_sz);
  fnaddr = XEXP (DECL_RTL (fndecl), 0);
  if (GET_MODE (fnaddr) != ptr_mode)
    fnaddr = convert_memory_address (ptr_mode, fnaddr);
  emit_move_insn (mem, fnaddr);

  mem = adjust_address (m_tramp, ptr_mode, tramp_code_sz + POINTER_BYTES);
  emit_move_insn (mem, chain_value);

  /* XXX We should really define a "clear_cache" pattern and use
     gen_clear_cache().  */
  a_tramp = XEXP (m_tramp, 0);
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),
		     LCT_NORMAL, VOIDmode, 2, a_tramp, ptr_mode,
		     plus_constant (ptr_mode, a_tramp, TRAMPOLINE_SIZE),
		     ptr_mode);
}

static unsigned char
aarch64_class_max_nregs (reg_class_t regclass, machine_mode mode)
{
  switch (regclass)
    {
    case CALLER_SAVE_REGS:
    case POINTER_REGS:
    case GENERAL_REGS:
    case ALL_REGS:
    case FP_REGS:
    case FP_LO_REGS:
      return
	aarch64_vector_mode_p (mode) ? (GET_MODE_SIZE (mode) + 15) / 16 :
				       (GET_MODE_SIZE (mode) + 7) / 8;
    case STACK_REG:
      return 1;

    case NO_REGS:
      return 0;

    default:
      break;
    }
  gcc_unreachable ();
}

static reg_class_t
aarch64_preferred_reload_class (rtx x, reg_class_t regclass)
{
  if (regclass == POINTER_REGS)
    return GENERAL_REGS;

  if (regclass == STACK_REG)
    {
      if (REG_P(x)
	  && reg_class_subset_p (REGNO_REG_CLASS (REGNO (x)), POINTER_REGS))
	  return regclass;

      return NO_REGS;
    }

  /* If it's an integer immediate that MOVI can't handle, then
     FP_REGS is not an option, so we return NO_REGS instead.  */
  if (CONST_INT_P (x) && reg_class_subset_p (regclass, FP_REGS)
      && !aarch64_simd_imm_scalar_p (x, GET_MODE (x)))
    return NO_REGS;

  /* Register eliminiation can result in a request for
     SP+constant->FP_REGS.  We cannot support such operations which
     use SP as source and an FP_REG as destination, so reject out
     right now.  */
  if (! reg_class_subset_p (regclass, GENERAL_REGS) && GET_CODE (x) == PLUS)
    {
      rtx lhs = XEXP (x, 0);

      /* Look through a possible SUBREG introduced by ILP32.  */
      if (GET_CODE (lhs) == SUBREG)
	lhs = SUBREG_REG (lhs);

      gcc_assert (REG_P (lhs));
      gcc_assert (reg_class_subset_p (REGNO_REG_CLASS (REGNO (lhs)),
				      POINTER_REGS));
      return NO_REGS;
    }

  return regclass;
}

void
aarch64_asm_output_labelref (FILE* f, const char *name)
{
  asm_fprintf (f, "%U%s", name);
}

static void
aarch64_elf_asm_constructor (rtx symbol, int priority)
{
  if (priority == DEFAULT_INIT_PRIORITY)
    default_ctor_section_asm_out_constructor (symbol, priority);
  else
    {
      section *s;
      char buf[18];
      snprintf (buf, sizeof (buf), ".init_array.%.5u", priority);
      s = get_section (buf, SECTION_WRITE, NULL);
      switch_to_section (s);
      assemble_align (POINTER_SIZE);
      assemble_aligned_integer (POINTER_BYTES, symbol);
    }
}

static void
aarch64_elf_asm_destructor (rtx symbol, int priority)
{
  if (priority == DEFAULT_INIT_PRIORITY)
    default_dtor_section_asm_out_destructor (symbol, priority);
  else
    {
      section *s;
      char buf[18];
      snprintf (buf, sizeof (buf), ".fini_array.%.5u", priority);
      s = get_section (buf, SECTION_WRITE, NULL);
      switch_to_section (s);
      assemble_align (POINTER_SIZE);
      assemble_aligned_integer (POINTER_BYTES, symbol);
    }
}

const char*
aarch64_output_casesi (rtx *operands)
{
  char buf[100];
  char label[100];
  rtx diff_vec = PATTERN (NEXT_INSN (as_a <rtx_insn *> (operands[2])));
  int index;
  static const char *const patterns[4][2] =
  {
    {
      "ldrb\t%w3, [%0,%w1,uxtw]",
      "add\t%3, %4, %w3, sxtb #2"
    },
    {
      "ldrh\t%w3, [%0,%w1,uxtw #1]",
      "add\t%3, %4, %w3, sxth #2"
    },
    {
      "ldr\t%w3, [%0,%w1,uxtw #2]",
      "add\t%3, %4, %w3, sxtw #2"
    },
    /* We assume that DImode is only generated when not optimizing and
       that we don't really need 64-bit address offsets.  That would
       imply an object file with 8GB of code in a single function!  */
    {
      "ldr\t%w3, [%0,%w1,uxtw #2]",
      "add\t%3, %4, %w3, sxtw #2"
    }
  };

  gcc_assert (GET_CODE (diff_vec) == ADDR_DIFF_VEC);

  index = exact_log2 (GET_MODE_SIZE (GET_MODE (diff_vec)));

  gcc_assert (index >= 0 && index <= 3);

  /* Need to implement table size reduction, by chaning the code below.  */
  output_asm_insn (patterns[index][0], operands);
  ASM_GENERATE_INTERNAL_LABEL (label, "Lrtx", CODE_LABEL_NUMBER (operands[2]));
  snprintf (buf, sizeof (buf),
	    "adr\t%%4, %s", targetm.strip_name_encoding (label));
  output_asm_insn (buf, operands);
  output_asm_insn (patterns[index][1], operands);
  output_asm_insn ("br\t%3", operands);
  assemble_label (asm_out_file, label);
  return "";
}


/* Return size in bits of an arithmetic operand which is shifted/scaled and
   masked such that it is suitable for a UXTB, UXTH, or UXTW extend
   operator.  */

int
aarch64_uxt_size (int shift, HOST_WIDE_INT mask)
{
  if (shift >= 0 && shift <= 3)
    {
      int size;
      for (size = 8; size <= 32; size *= 2)
	{
	  HOST_WIDE_INT bits = ((HOST_WIDE_INT)1U << size) - 1;
	  if (mask == bits << shift)
	    return size;
	}
    }
  return 0;
}

static bool
aarch64_use_blocks_for_constant_p (machine_mode mode ATTRIBUTE_UNUSED,
				   const_rtx x ATTRIBUTE_UNUSED)
{
  /* We can't use blocks for constants when we're using a per-function
     constant pool.  */
  return false;
}

static section *
aarch64_select_rtx_section (machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x ATTRIBUTE_UNUSED,
			    unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  /* Force all constant pool entries into the current function section.  */
  return function_section (current_function_decl);
}


/* Costs.  */

/* Helper function for rtx cost calculation.  Strip a shift expression
   from X.  Returns the inner operand if successful, or the original
   expression on failure.  */
static rtx
aarch64_strip_shift (rtx x)
{
  rtx op = x;

  /* We accept both ROTATERT and ROTATE: since the RHS must be a constant
     we can convert both to ROR during final output.  */
  if ((GET_CODE (op) == ASHIFT
       || GET_CODE (op) == ASHIFTRT
       || GET_CODE (op) == LSHIFTRT
       || GET_CODE (op) == ROTATERT
       || GET_CODE (op) == ROTATE)
      && CONST_INT_P (XEXP (op, 1)))
    return XEXP (op, 0);

  if (GET_CODE (op) == MULT
      && CONST_INT_P (XEXP (op, 1))
      && ((unsigned) exact_log2 (INTVAL (XEXP (op, 1)))) < 64)
    return XEXP (op, 0);

  return x;
}

/* Helper function for rtx cost calculation.  Strip an extend
   expression from X.  Returns the inner operand if successful, or the
   original expression on failure.  We deal with a number of possible
   canonicalization variations here.  */
static rtx
aarch64_strip_extend (rtx x)
{
  rtx op = x;

  /* Zero and sign extraction of a widened value.  */
  if ((GET_CODE (op) == ZERO_EXTRACT || GET_CODE (op) == SIGN_EXTRACT)
      && XEXP (op, 2) == const0_rtx
      && GET_CODE (XEXP (op, 0)) == MULT
      && aarch64_is_extend_from_extract (GET_MODE (op), XEXP (XEXP (op, 0), 1),
					 XEXP (op, 1)))
    return XEXP (XEXP (op, 0), 0);

  /* It can also be represented (for zero-extend) as an AND with an
     immediate.  */
  if (GET_CODE (op) == AND
      && GET_CODE (XEXP (op, 0)) == MULT
      && CONST_INT_P (XEXP (XEXP (op, 0), 1))
      && CONST_INT_P (XEXP (op, 1))
      && aarch64_uxt_size (exact_log2 (INTVAL (XEXP (XEXP (op, 0), 1))),
			   INTVAL (XEXP (op, 1))) != 0)
    return XEXP (XEXP (op, 0), 0);

  /* Now handle extended register, as this may also have an optional
     left shift by 1..4.  */
  if (GET_CODE (op) == ASHIFT
      && CONST_INT_P (XEXP (op, 1))
      && ((unsigned HOST_WIDE_INT) INTVAL (XEXP (op, 1))) <= 4)
    op = XEXP (op, 0);

  if (GET_CODE (op) == ZERO_EXTEND
      || GET_CODE (op) == SIGN_EXTEND)
    op = XEXP (op, 0);

  if (op != x)
    return op;

  return x;
}

/* Helper function for rtx cost calculation.  Calculate the cost of
   a MULT, which may be part of a multiply-accumulate rtx.  Return
   the calculated cost of the expression, recursing manually in to
   operands where needed.  */

static int
aarch64_rtx_mult_cost (rtx x, int code, int outer, bool speed)
{
  rtx op0, op1;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params->insn_extra_cost;
  int cost = 0;
  bool maybe_fma = (outer == PLUS || outer == MINUS);
  machine_mode mode = GET_MODE (x);

  gcc_checking_assert (code == MULT);

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  if (VECTOR_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  /* Integer multiply/fma.  */
  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      /* The multiply will be canonicalized as a shift, cost it as such.  */
      if (CONST_INT_P (op1)
	  && exact_log2 (INTVAL (op1)) > 0)
	{
	  if (speed)
	    {
	      if (maybe_fma)
		/* ADD (shifted register).  */
		cost += extra_cost->alu.arith_shift;
	      else
		/* LSL (immediate).  */
		cost += extra_cost->alu.shift;
	    }

	  cost += rtx_cost (op0, GET_CODE (op0), 0, speed);

	  return cost;
	}

      /* Integer multiplies or FMAs have zero/sign extending variants.  */
      if ((GET_CODE (op0) == ZERO_EXTEND
	   && GET_CODE (op1) == ZERO_EXTEND)
	  || (GET_CODE (op0) == SIGN_EXTEND
	      && GET_CODE (op1) == SIGN_EXTEND))
	{
	  cost += rtx_cost (XEXP (op0, 0), MULT, 0, speed)
		  + rtx_cost (XEXP (op1, 0), MULT, 1, speed);

	  if (speed)
	    {
	      if (maybe_fma)
		/* MADD/SMADDL/UMADDL.  */
		cost += extra_cost->mult[0].extend_add;
	      else
		/* MUL/SMULL/UMULL.  */
		cost += extra_cost->mult[0].extend;
	    }

	  return cost;
	}

      /* This is either an integer multiply or an FMA.  In both cases
	 we want to recurse and cost the operands.  */
      cost += rtx_cost (op0, MULT, 0, speed)
	      + rtx_cost (op1, MULT, 1, speed);

      if (speed)
	{
	  if (maybe_fma)
	    /* MADD.  */
	    cost += extra_cost->mult[mode == DImode].add;
	  else
	    /* MUL.  */
	    cost += extra_cost->mult[mode == DImode].simple;
	}

      return cost;
    }
  else
    {
      if (speed)
	{
	  /* Floating-point FMA/FMUL can also support negations of the
	     operands, unless the rounding mode is upward or downward in
	     which case FNMUL is different than FMUL with operand negation.  */
	  bool neg0 = GET_CODE (op0) == NEG;
	  bool neg1 = GET_CODE (op1) == NEG;
	  if (maybe_fma || !flag_rounding_math || (neg0 && neg1))
	    {
	      if (neg0)
		op0 = XEXP (op0, 0);
	      if (neg1)
		op1 = XEXP (op1, 0);
	    }

	  if (maybe_fma)
	    /* FMADD/FNMADD/FNMSUB/FMSUB.  */
	    cost += extra_cost->fp[mode == DFmode].fma;
	  else
	    /* FMUL/FNMUL.  */
	    cost += extra_cost->fp[mode == DFmode].mult;
	}

      cost += rtx_cost (op0, MULT, 0, speed)
	      + rtx_cost (op1, MULT, 1, speed);
      return cost;
    }
}

static int
aarch64_address_cost (rtx x,
		      machine_mode mode,
		      addr_space_t as ATTRIBUTE_UNUSED,
		      bool speed)
{
  enum rtx_code c = GET_CODE (x);
  const struct cpu_addrcost_table *addr_cost = aarch64_tune_params->addr_cost;
  struct aarch64_address_info info;
  int cost = 0;
  info.shift = 0;

  if (!aarch64_classify_address (&info, x, mode, c, false))
    {
      if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF)
	{
	  /* This is a CONST or SYMBOL ref which will be split
	     in a different way depending on the code model in use.
	     Cost it through the generic infrastructure.  */
	  int cost_symbol_ref = rtx_cost (x, MEM, 1, speed);
	  /* Divide through by the cost of one instruction to
	     bring it to the same units as the address costs.  */
	  cost_symbol_ref /= COSTS_N_INSNS (1);
	  /* The cost is then the cost of preparing the address,
	     followed by an immediate (possibly 0) offset.  */
	  return cost_symbol_ref + addr_cost->imm_offset;
	}
      else
	{
	  /* This is most likely a jump table from a case
	     statement.  */
	  return addr_cost->register_offset;
	}
    }

  switch (info.type)
    {
      case ADDRESS_LO_SUM:
      case ADDRESS_SYMBOLIC:
      case ADDRESS_REG_IMM:
	cost += addr_cost->imm_offset;
	break;

      case ADDRESS_REG_WB:
	if (c == PRE_INC || c == PRE_DEC || c == PRE_MODIFY)
	  cost += addr_cost->pre_modify;
	else if (c == POST_INC || c == POST_DEC || c == POST_MODIFY)
	  cost += addr_cost->post_modify;
	else
	  gcc_unreachable ();

	break;

      case ADDRESS_REG_REG:
	cost += addr_cost->register_offset;
	break;

      case ADDRESS_REG_UXTW:
      case ADDRESS_REG_SXTW:
	cost += addr_cost->register_extend;
	break;

      default:
	gcc_unreachable ();
    }


  if (info.shift > 0)
    {
      /* For the sake of calculating the cost of the shifted register
	 component, we can treat same sized modes in the same way.  */
      switch (GET_MODE_BITSIZE (mode))
	{
	  case 16:
	    cost += addr_cost->addr_scale_costs.hi;
	    break;

	  case 32:
	    cost += addr_cost->addr_scale_costs.si;
	    break;

	  case 64:
	    cost += addr_cost->addr_scale_costs.di;
	    break;

	  /* We can't tell, or this is a 128-bit vector.  */
	  default:
	    cost += addr_cost->addr_scale_costs.ti;
	    break;
	}
    }

  return cost;
}

/* Return true if the RTX X in mode MODE is a zero or sign extract
   usable in an ADD or SUB (extended register) instruction.  */
static bool
aarch64_rtx_arith_op_extract_p (rtx x, machine_mode mode)
{
  /* Catch add with a sign extract.
     This is add_<optab><mode>_multp2.  */
  if (GET_CODE (x) == SIGN_EXTRACT
      || GET_CODE (x) == ZERO_EXTRACT)
    {
      rtx op0 = XEXP (x, 0);
      rtx op1 = XEXP (x, 1);
      rtx op2 = XEXP (x, 2);

      if (GET_CODE (op0) == MULT
	  && CONST_INT_P (op1)
	  && op2 == const0_rtx
	  && CONST_INT_P (XEXP (op0, 1))
	  && aarch64_is_extend_from_extract (mode,
					     XEXP (op0, 1),
					     op1))
	{
	  return true;
	}
    }

  return false;
}

static bool
aarch64_frint_unspec_p (unsigned int u)
{
  switch (u)
    {
      case UNSPEC_FRINTZ:
      case UNSPEC_FRINTP:
      case UNSPEC_FRINTM:
      case UNSPEC_FRINTA:
      case UNSPEC_FRINTN:
      case UNSPEC_FRINTX:
      case UNSPEC_FRINTI:
        return true;

      default:
        return false;
    }
}

/* Calculate the cost of calculating (if_then_else (OP0) (OP1) (OP2)),
   storing it in *COST.  Result is true if the total cost of the operation
   has now been calculated.  */
static bool
aarch64_if_then_else_costs (rtx op0, rtx op1, rtx op2, int *cost, bool speed)
{
  rtx inner;
  rtx comparator;
  enum rtx_code cmpcode;

  if (COMPARISON_P (op0))
    {
      inner = XEXP (op0, 0);
      comparator = XEXP (op0, 1);
      cmpcode = GET_CODE (op0);
    }
  else
    {
      inner = op0;
      comparator = const0_rtx;
      cmpcode = NE;
    }

  if (GET_CODE (op1) == PC || GET_CODE (op2) == PC)
    {
      /* Conditional branch.  */
      if (GET_MODE_CLASS (GET_MODE (inner)) == MODE_CC)
	return true;
      else
	{
	  if (cmpcode == NE || cmpcode == EQ)
	    {
	      if (comparator == const0_rtx)
		{
		  /* TBZ/TBNZ/CBZ/CBNZ.  */
		  if (GET_CODE (inner) == ZERO_EXTRACT)
		    /* TBZ/TBNZ.  */
		    *cost += rtx_cost (XEXP (inner, 0), ZERO_EXTRACT,
			 	       0, speed);
		else
		  /* CBZ/CBNZ.  */
		  *cost += rtx_cost (inner, cmpcode, 0, speed);

	        return true;
	      }
	    }
	  else if (cmpcode == LT || cmpcode == GE)
	    {
	      /* TBZ/TBNZ.  */
	      if (comparator == const0_rtx)
		return true;
	    }
	}
    }
  else if (GET_MODE_CLASS (GET_MODE (inner)) == MODE_CC)
    {
      /* It's a conditional operation based on the status flags,
	 so it must be some flavor of CSEL.  */

      /* CSNEG, CSINV, and CSINC are handled for free as part of CSEL.  */
      if (GET_CODE (op1) == NEG
          || GET_CODE (op1) == NOT
          || (GET_CODE (op1) == PLUS && XEXP (op1, 1) == const1_rtx))
	op1 = XEXP (op1, 0);

      *cost += rtx_cost (op1, IF_THEN_ELSE, 1, speed);
      *cost += rtx_cost (op2, IF_THEN_ELSE, 2, speed);
      return true;
    }

  /* We don't know what this is, cost all operands.  */
  return false;
}

/* Calculate the cost of calculating X, storing it in *COST.  Result
   is true if the total cost of the operation has now been calculated.  */
static bool
aarch64_rtx_costs (rtx x, int code, int outer ATTRIBUTE_UNUSED,
		   int param ATTRIBUTE_UNUSED, int *cost, bool speed)
{
  rtx op0, op1, op2;
  const struct cpu_cost_table *extra_cost
    = aarch64_tune_params->insn_extra_cost;
  machine_mode mode = GET_MODE (x);

  /* By default, assume that everything has equivalent cost to the
     cheapest instruction.  Any additional costs are applied as a delta
     above this default.  */
  *cost = COSTS_N_INSNS (1);

  /* TODO: The cost infrastructure currently does not handle
     vector operations.  Assume that all vector operations
     are equally expensive.  */
  if (VECTOR_MODE_P (mode))
    {
      if (speed)
	*cost += extra_cost->vect.alu;
      return true;
    }

  switch (code)
    {
    case SET:
      /* The cost depends entirely on the operands to SET.  */
      *cost = 0;
      op0 = SET_DEST (x);
      op1 = SET_SRC (x);

      switch (GET_CODE (op0))
	{
	case MEM:
	  if (speed)
	    {
	      rtx address = XEXP (op0, 0);
	      if (GET_MODE_CLASS (mode) == MODE_INT)
		*cost += extra_cost->ldst.store;
	      else if (mode == SFmode)
		*cost += extra_cost->ldst.storef;
	      else if (mode == DFmode)
		*cost += extra_cost->ldst.stored;

	      *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	    }

	  *cost += rtx_cost (op1, SET, 1, speed);
	  return true;

	case SUBREG:
	  if (! REG_P (SUBREG_REG (op0)))
	    *cost += rtx_cost (SUBREG_REG (op0), SET, 0, speed);

	  /* Fall through.  */
	case REG:
	  /* const0_rtx is in general free, but we will use an
	     instruction to set a register to 0.  */
          if (REG_P (op1) || op1 == const0_rtx)
            {
              /* The cost is 1 per register copied.  */
              int n_minus_1 = (GET_MODE_SIZE (GET_MODE (op0)) - 1)
			      / UNITS_PER_WORD;
              *cost = COSTS_N_INSNS (n_minus_1 + 1);
            }
          else
	    /* Cost is just the cost of the RHS of the set.  */
	    *cost += rtx_cost (op1, SET, 1, speed);
	  return true;

	case ZERO_EXTRACT:
	case SIGN_EXTRACT:
	  /* Bit-field insertion.  Strip any redundant widening of
	     the RHS to meet the width of the target.  */
	  if (GET_CODE (op1) == SUBREG)
	    op1 = SUBREG_REG (op1);
	  if ((GET_CODE (op1) == ZERO_EXTEND
	       || GET_CODE (op1) == SIGN_EXTEND)
	      && CONST_INT_P (XEXP (op0, 1))
	      && (GET_MODE_BITSIZE (GET_MODE (XEXP (op1, 0)))
		  >= INTVAL (XEXP (op0, 1))))
	    op1 = XEXP (op1, 0);

          if (CONST_INT_P (op1))
            {
              /* MOV immediate is assumed to always be cheap.  */
              *cost = COSTS_N_INSNS (1);
            }
          else
            {
              /* BFM.  */
	      if (speed)
		*cost += extra_cost->alu.bfi;
              *cost += rtx_cost (op1, (enum rtx_code) code, 1, speed);
            }

	  return true;

	default:
	  /* We can't make sense of this, assume default cost.  */
          *cost = COSTS_N_INSNS (1);
	  return false;
	}
      return false;

    case CONST_INT:
      /* If an instruction can incorporate a constant within the
	 instruction, the instruction's expression avoids calling
	 rtx_cost() on the constant.  If rtx_cost() is called on a
	 constant, then it is usually because the constant must be
	 moved into a register by one or more instructions.

	 The exception is constant 0, which can be expressed
	 as XZR/WZR and is therefore free.  The exception to this is
	 if we have (set (reg) (const0_rtx)) in which case we must cost
	 the move.  However, we can catch that when we cost the SET, so
	 we don't need to consider that here.  */
      if (x == const0_rtx)
	*cost = 0;
      else
	{
	  /* To an approximation, building any other constant is
	     proportionally expensive to the number of instructions
	     required to build that constant.  This is true whether we
	     are compiling for SPEED or otherwise.  */
	  *cost = COSTS_N_INSNS (aarch64_internal_mov_immediate
				 (NULL_RTX, x, false, mode));
	}
      return true;

    case CONST_DOUBLE:
      if (speed)
	{
	  /* mov[df,sf]_aarch64.  */
	  if (aarch64_float_const_representable_p (x))
	    /* FMOV (scalar immediate).  */
	    *cost += extra_cost->fp[mode == DFmode].fpconst;
	  else if (!aarch64_float_const_zero_rtx_p (x))
	    {
	      /* This will be a load from memory.  */
	      if (mode == DFmode)
		*cost += extra_cost->ldst.loadd;
	      else
		*cost += extra_cost->ldst.loadf;
	    }
	  else
	    /* Otherwise this is +0.0.  We get this using MOVI d0, #0
	       or MOV v0.s[0], wzr - neither of which are modeled by the
	       cost tables.  Just use the default cost.  */
	    {
	    }
	}

      return true;

    case MEM:
      if (speed)
	{
	  /* For loads we want the base cost of a load, plus an
	     approximation for the additional cost of the addressing
	     mode.  */
	  rtx address = XEXP (x, 0);
	  if (GET_MODE_CLASS (mode) == MODE_INT)
	    *cost += extra_cost->ldst.load;
	  else if (mode == SFmode)
	    *cost += extra_cost->ldst.loadf;
	  else if (mode == DFmode)
	    *cost += extra_cost->ldst.loadd;

	  *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	}

      return true;

    case NEG:
      op0 = XEXP (x, 0);

      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
       {
          if (GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMPARE
              || GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMM_COMPARE)
            {
              /* CSETM.  */
              *cost += rtx_cost (XEXP (op0, 0), NEG, 0, speed);
              return true;
            }

	  /* Cost this as SUB wzr, X.  */
          op0 = CONST0_RTX (GET_MODE (x));
          op1 = XEXP (x, 0);
          goto cost_minus;
        }

      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
        {
          /* Support (neg(fma...)) as a single instruction only if
             sign of zeros is unimportant.  This matches the decision
             making in aarch64.md.  */
          if (GET_CODE (op0) == FMA && !HONOR_SIGNED_ZEROS (GET_MODE (op0)))
            {
	      /* FNMADD.  */
              *cost = rtx_cost (op0, NEG, 0, speed);
              return true;
            }
	  if (GET_CODE (op0) == MULT)
	    {
	      /* FNMUL.  */
	      *cost = rtx_cost (op0, NEG, 0, speed);
	      return true;
	    }
	  if (speed)
	    /* FNEG.  */
	    *cost += extra_cost->fp[mode == DFmode].neg;
          return false;
        }

      return false;

    case CLRSB:
    case CLZ:
      if (speed)
        *cost += extra_cost->alu.clz;

      return false;

    case COMPARE:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (op1 == const0_rtx
	  && GET_CODE (op0) == AND)
	{
	  x = op0;
	  goto cost_logic;
	}

      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
        {
          /* TODO: A write to the CC flags possibly costs extra, this
	     needs encoding in the cost tables.  */

          /* CC_ZESWPmode supports zero extend for free.  */
          if (GET_MODE (x) == CC_ZESWPmode && GET_CODE (op0) == ZERO_EXTEND)
            op0 = XEXP (op0, 0);

          /* ANDS.  */
          if (GET_CODE (op0) == AND)
            {
              x = op0;
              goto cost_logic;
            }

          if (GET_CODE (op0) == PLUS)
            {
	      /* ADDS (and CMN alias).  */
              x = op0;
              goto cost_plus;
            }

          if (GET_CODE (op0) == MINUS)
            {
	      /* SUBS.  */
              x = op0;
              goto cost_minus;
            }

          if (GET_CODE (op1) == NEG)
            {
	      /* CMN.  */
	      if (speed)
		*cost += extra_cost->alu.arith;

              *cost += rtx_cost (op0, COMPARE, 0, speed);
	      *cost += rtx_cost (XEXP (op1, 0), NEG, 1, speed);
              return true;
            }

          /* CMP.

	     Compare can freely swap the order of operands, and
             canonicalization puts the more complex operation first.
             But the integer MINUS logic expects the shift/extend
             operation in op1.  */
          if (! (REG_P (op0)
                 || (GET_CODE (op0) == SUBREG && REG_P (SUBREG_REG (op0)))))
          {
            op0 = XEXP (x, 1);
            op1 = XEXP (x, 0);
          }
          goto cost_minus;
        }

      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_FLOAT)
        {
	  /* FCMP.  */
	  if (speed)
	    *cost += extra_cost->fp[mode == DFmode].compare;

          if (CONST_DOUBLE_P (op1) && aarch64_float_const_zero_rtx_p (op1))
            {
              /* FCMP supports constant 0.0 for no extra cost. */
              return true;
            }
          return false;
        }

      return false;

    case MINUS:
      {
	op0 = XEXP (x, 0);
	op1 = XEXP (x, 1);

cost_minus:
	/* Detect valid immediates.  */
	if ((GET_MODE_CLASS (mode) == MODE_INT
	     || (GET_MODE_CLASS (mode) == MODE_CC
		 && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT))
	    && CONST_INT_P (op1)
	    && aarch64_uimm12_shift (INTVAL (op1)))
	  {
	    *cost += rtx_cost (op0, MINUS, 0, speed);

	    if (speed)
	      /* SUB(S) (immediate).  */
	      *cost += extra_cost->alu.arith;
	    return true;

	  }

	/* Look for SUB (extended register).  */
        if (aarch64_rtx_arith_op_extract_p (op1, mode))
	  {
	    if (speed)
	      *cost += extra_cost->alu.arith_shift;

	    *cost += rtx_cost (XEXP (XEXP (op1, 0), 0),
			       (enum rtx_code) GET_CODE (op1),
			       0, speed);
	    return true;
	  }

	rtx new_op1 = aarch64_strip_extend (op1);

	/* Cost this as an FMA-alike operation.  */
	if ((GET_CODE (new_op1) == MULT
	     || GET_CODE (new_op1) == ASHIFT)
	    && code != COMPARE)
	  {
	    *cost += aarch64_rtx_mult_cost (new_op1, MULT,
					    (enum rtx_code) code,
					    speed);
	    *cost += rtx_cost (op0, MINUS, 0, speed);
	    return true;
	  }

	*cost += rtx_cost (new_op1, MINUS, 1, speed);

	if (speed)
	  {
	    if (GET_MODE_CLASS (mode) == MODE_INT)
	      /* SUB(S).  */
	      *cost += extra_cost->alu.arith;
	    else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	      /* FSUB.  */
	      *cost += extra_cost->fp[mode == DFmode].addsub;
	  }
	return true;
      }

    case PLUS:
      {
	rtx new_op0;

	op0 = XEXP (x, 0);
	op1 = XEXP (x, 1);

cost_plus:
	if (GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMPARE
	    || GET_RTX_CLASS (GET_CODE (op0)) == RTX_COMM_COMPARE)
	  {
	    /* CSINC.  */
	    *cost += rtx_cost (XEXP (op0, 0), PLUS, 0, speed);
	    *cost += rtx_cost (op1, PLUS, 1, speed);
	    return true;
	  }

	if (GET_MODE_CLASS (mode) == MODE_INT
	    && CONST_INT_P (op1)
	    && aarch64_uimm12_shift (INTVAL (op1)))
	  {
	    *cost += rtx_cost (op0, PLUS, 0, speed);

	    if (speed)
	      /* ADD (immediate).  */
	      *cost += extra_cost->alu.arith;
	    return true;
	  }

	/* Look for ADD (extended register).  */
        if (aarch64_rtx_arith_op_extract_p (op0, mode))
	  {
	    if (speed)
	      *cost += extra_cost->alu.arith_shift;

	    *cost += rtx_cost (XEXP (XEXP (op0, 0), 0),
			       (enum rtx_code) GET_CODE (op0),
			       0, speed);
	    return true;
	  }

	/* Strip any extend, leave shifts behind as we will
	   cost them through mult_cost.  */
	new_op0 = aarch64_strip_extend (op0);

	if (GET_CODE (new_op0) == MULT
	    || GET_CODE (new_op0) == ASHIFT)
	  {
	    *cost += aarch64_rtx_mult_cost (new_op0, MULT, PLUS,
					    speed);
	    *cost += rtx_cost (op1, PLUS, 1, speed);
	    return true;
	  }

	*cost += (rtx_cost (new_op0, PLUS, 0, speed)
		  + rtx_cost (op1, PLUS, 1, speed));

	if (speed)
	  {
	    if (GET_MODE_CLASS (mode) == MODE_INT)
	      /* ADD.  */
	      *cost += extra_cost->alu.arith;
	    else if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	      /* FADD.  */
	      *cost += extra_cost->fp[mode == DFmode].addsub;
	  }
	return true;
      }

    case BSWAP:
      *cost = COSTS_N_INSNS (1);

      if (speed)
        *cost += extra_cost->alu.rev;

      return false;

    case IOR:
      if (aarch_rev16_p (x))
        {
          *cost = COSTS_N_INSNS (1);

          if (speed)
            *cost += extra_cost->alu.rev;

          return true;
        }
    /* Fall through.  */
    case XOR:
    case AND:
    cost_logic:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (code == AND
          && GET_CODE (op0) == MULT
          && CONST_INT_P (XEXP (op0, 1))
          && CONST_INT_P (op1)
          && aarch64_uxt_size (exact_log2 (INTVAL (XEXP (op0, 1))),
                               INTVAL (op1)) != 0)
        {
          /* This is a UBFM/SBFM.  */
          *cost += rtx_cost (XEXP (op0, 0), ZERO_EXTRACT, 0, speed);
	  if (speed)
	    *cost += extra_cost->alu.bfx;
          return true;
        }

      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
	{
	  /* We possibly get the immediate for free, this is not
	     modelled.  */
	  if (CONST_INT_P (op1)
	      && aarch64_bitmask_imm (INTVAL (op1), GET_MODE (x)))
	    {
	      *cost += rtx_cost (op0, (enum rtx_code) code, 0, speed);

	      if (speed)
		*cost += extra_cost->alu.logical;

	      return true;
	    }
	  else
	    {
	      rtx new_op0 = op0;

	      /* Handle ORN, EON, or BIC.  */
	      if (GET_CODE (op0) == NOT)
		op0 = XEXP (op0, 0);

	      new_op0 = aarch64_strip_shift (op0);

	      /* If we had a shift on op0 then this is a logical-shift-
		 by-register/immediate operation.  Otherwise, this is just
		 a logical operation.  */
	      if (speed)
		{
		  if (new_op0 != op0)
		    {
		      /* Shift by immediate.  */
		      if (CONST_INT_P (XEXP (op0, 1)))
			*cost += extra_cost->alu.log_shift;
		      else
			*cost += extra_cost->alu.log_shift_reg;
		    }
		  else
		    *cost += extra_cost->alu.logical;
		}

	      /* In both cases we want to cost both operands.  */
	      *cost += rtx_cost (new_op0, (enum rtx_code) code, 0, speed)
		       + rtx_cost (op1, (enum rtx_code) code, 1, speed);

	      return true;
	    }
	}
      return false;

    case NOT:
      /* MVN.  */
      if (speed)
	*cost += extra_cost->alu.logical;

      /* The logical instruction could have the shifted register form,
         but the cost is the same if the shift is processed as a separate
         instruction, so we don't bother with it here.  */
      return false;

    case ZERO_EXTEND:

      op0 = XEXP (x, 0);
      /* If a value is written in SI mode, then zero extended to DI
	 mode, the operation will in general be free as a write to
	 a 'w' register implicitly zeroes the upper bits of an 'x'
	 register.  However, if this is

	   (set (reg) (zero_extend (reg)))

	 we must cost the explicit register move.  */
      if (mode == DImode
	  && GET_MODE (op0) == SImode
	  && outer == SET)
	{
	  int op_cost = rtx_cost (XEXP (x, 0), ZERO_EXTEND, 0, speed);

	  if (!op_cost && speed)
	    /* MOV.  */
	    *cost += extra_cost->alu.extend;
	  else
	    /* Free, the cost is that of the SI mode operation.  */
	    *cost = op_cost;

	  return true;
	}
      else if (MEM_P (XEXP (x, 0)))
	{
	  /* All loads can zero extend to any size for free.  */
	  *cost = rtx_cost (XEXP (x, 0), ZERO_EXTEND, param, speed);
	  return true;
	}

      /* UXTB/UXTH.  */
      if (speed)
	*cost += extra_cost->alu.extend;

      return false;

    case SIGN_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	{
	  /* LDRSH.  */
	  if (speed)
	    {
	      rtx address = XEXP (XEXP (x, 0), 0);
	      *cost += extra_cost->ldst.load_sign_extend;

	      *cost +=
		COSTS_N_INSNS (aarch64_address_cost (address, mode,
						     0, speed));
	    }
	  return true;
	}

      if (speed)
	*cost += extra_cost->alu.extend;
      return false;

    case ASHIFT:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (CONST_INT_P (op1))
        {
	  /* LSL (immediate), UBMF, UBFIZ and friends.  These are all
	     aliases.  */
	  if (speed)
	    *cost += extra_cost->alu.shift;

          /* We can incorporate zero/sign extend for free.  */
          if (GET_CODE (op0) == ZERO_EXTEND
              || GET_CODE (op0) == SIGN_EXTEND)
            op0 = XEXP (op0, 0);

          *cost += rtx_cost (op0, ASHIFT, 0, speed);
          return true;
        }
      else
        {
	  /* LSLV.  */
	  if (speed)
	    *cost += extra_cost->alu.shift_reg;

	  return false;  /* All arguments need to be in registers.  */
        }

    case ROTATE:
    case ROTATERT:
    case LSHIFTRT:
    case ASHIFTRT:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if (CONST_INT_P (op1))
	{
	  /* ASR (immediate) and friends.  */
	  if (speed)
	    *cost += extra_cost->alu.shift;

	  *cost += rtx_cost (op0, (enum rtx_code) code, 0, speed);
	  return true;
	}
      else
	{

	  /* ASR (register) and friends.  */
	  if (speed)
	    *cost += extra_cost->alu.shift_reg;

	  return false;  /* All arguments need to be in registers.  */
	}

    case SYMBOL_REF:

      if (aarch64_cmodel == AARCH64_CMODEL_LARGE)
	{
	  /* LDR.  */
	  if (speed)
	    *cost += extra_cost->ldst.load;
	}
      else if (aarch64_cmodel == AARCH64_CMODEL_SMALL
	       || aarch64_cmodel == AARCH64_CMODEL_SMALL_PIC)
	{
	  /* ADRP, followed by ADD.  */
	  *cost += COSTS_N_INSNS (1);
	  if (speed)
	    *cost += 2 * extra_cost->alu.arith;
	}
      else if (aarch64_cmodel == AARCH64_CMODEL_TINY
	       || aarch64_cmodel == AARCH64_CMODEL_TINY_PIC)
	{
	  /* ADR.  */
	  if (speed)
	    *cost += extra_cost->alu.arith;
	}

      if (flag_pic)
	{
	  /* One extra load instruction, after accessing the GOT.  */
	  *cost += COSTS_N_INSNS (1);
	  if (speed)
	    *cost += extra_cost->ldst.load;
	}
      return true;

    case HIGH:
    case LO_SUM:
      /* ADRP/ADD (immediate).  */
      if (speed)
	*cost += extra_cost->alu.arith;
      return true;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      /* UBFX/SBFX.  */
      if (speed)
	*cost += extra_cost->alu.bfx;

      /* We can trust that the immediates used will be correct (there
	 are no by-register forms), so we need only cost op0.  */
      *cost += rtx_cost (XEXP (x, 0), (enum rtx_code) code, 0, speed);
      return true;

    case MULT:
      *cost += aarch64_rtx_mult_cost (x, MULT, 0, speed);
      /* aarch64_rtx_mult_cost always handles recursion to its
	 operands.  */
      return true;

    case MOD:
    case UMOD:
      if (speed)
	{
	  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
	    *cost += (extra_cost->mult[GET_MODE (x) == DImode].add
		      + extra_cost->mult[GET_MODE (x) == DImode].idiv);
	  else if (GET_MODE (x) == DFmode)
	    *cost += (extra_cost->fp[1].mult
		      + extra_cost->fp[1].div);
	  else if (GET_MODE (x) == SFmode)
	    *cost += (extra_cost->fp[0].mult
		      + extra_cost->fp[0].div);
	}
      return false;  /* All arguments need to be in registers.  */

    case DIV:
    case UDIV:
    case SQRT:
      if (speed)
	{
	  if (GET_MODE_CLASS (mode) == MODE_INT)
	    /* There is no integer SQRT, so only DIV and UDIV can get
	       here.  */
	    *cost += extra_cost->mult[mode == DImode].idiv;
	  else
	    *cost += extra_cost->fp[mode == DFmode].div;
	}
      return false;  /* All arguments need to be in registers.  */

    case IF_THEN_ELSE:
      return aarch64_if_then_else_costs (XEXP (x, 0), XEXP (x, 1),
					 XEXP (x, 2), cost, speed);

    case EQ:
    case NE:
    case GT:
    case GTU:
    case LT:
    case LTU:
    case GE:
    case GEU:
    case LE:
    case LEU:

      return false; /* All arguments must be in registers.  */

    case FMA:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      op2 = XEXP (x, 2);

      if (speed)
	*cost += extra_cost->fp[mode == DFmode].fma;

      /* FMSUB, FNMADD, and FNMSUB are free.  */
      if (GET_CODE (op0) == NEG)
        op0 = XEXP (op0, 0);

      if (GET_CODE (op2) == NEG)
        op2 = XEXP (op2, 0);

      /* aarch64_fnma4_elt_to_64v2df has the NEG as operand 1,
	 and the by-element operand as operand 0.  */
      if (GET_CODE (op1) == NEG)
        op1 = XEXP (op1, 0);

      /* Catch vector-by-element operations.  The by-element operand can
	 either be (vec_duplicate (vec_select (x))) or just
	 (vec_select (x)), depending on whether we are multiplying by
	 a vector or a scalar.

	 Canonicalization is not very good in these cases, FMA4 will put the
	 by-element operand as operand 0, FNMA4 will have it as operand 1.  */
      if (GET_CODE (op0) == VEC_DUPLICATE)
	op0 = XEXP (op0, 0);
      else if (GET_CODE (op1) == VEC_DUPLICATE)
	op1 = XEXP (op1, 0);

      if (GET_CODE (op0) == VEC_SELECT)
	op0 = XEXP (op0, 0);
      else if (GET_CODE (op1) == VEC_SELECT)
	op1 = XEXP (op1, 0);

      /* If the remaining parameters are not registers,
         get the cost to put them into registers.  */
      *cost += rtx_cost (op0, FMA, 0, speed);
      *cost += rtx_cost (op1, FMA, 1, speed);
      *cost += rtx_cost (op2, FMA, 2, speed);
      return true;

    case FLOAT_EXTEND:
      if (speed)
	*cost += extra_cost->fp[mode == DFmode].widen;
      return false;

    case FLOAT_TRUNCATE:
      if (speed)
	*cost += extra_cost->fp[mode == DFmode].narrow;
      return false;

    case FIX:
    case UNSIGNED_FIX:
      x = XEXP (x, 0);
      /* Strip the rounding part.  They will all be implemented
         by the fcvt* family of instructions anyway.  */
      if (GET_CODE (x) == UNSPEC)
        {
          unsigned int uns_code = XINT (x, 1);

          if (uns_code == UNSPEC_FRINTA
              || uns_code == UNSPEC_FRINTM
              || uns_code == UNSPEC_FRINTN
              || uns_code == UNSPEC_FRINTP
              || uns_code == UNSPEC_FRINTZ)
            x = XVECEXP (x, 0, 0);
        }

      if (speed)
        *cost += extra_cost->fp[GET_MODE (x) == DFmode].toint;

      *cost += rtx_cost (x, (enum rtx_code) code, 0, speed);
      return true;

    case ABS:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  /* FABS and FNEG are analogous.  */
	  if (speed)
	    *cost += extra_cost->fp[mode == DFmode].neg;
	}
      else
	{
	  /* Integer ABS will either be split to
	     two arithmetic instructions, or will be an ABS
	     (scalar), which we don't model.  */
	  *cost = COSTS_N_INSNS (2);
	  if (speed)
	    *cost += 2 * extra_cost->alu.arith;
	}
      return false;

    case SMAX:
    case SMIN:
      if (speed)
	{
	  /* FMAXNM/FMINNM/FMAX/FMIN.
	     TODO: This may not be accurate for all implementations, but
	     we do not model this in the cost tables.  */
	  *cost += extra_cost->fp[mode == DFmode].addsub;
	}
      return false;

    case UNSPEC:
      /* The floating point round to integer frint* instructions.  */
      if (aarch64_frint_unspec_p (XINT (x, 1)))
        {
          if (speed)
            *cost += extra_cost->fp[mode == DFmode].roundint;

          return false;
        }

      if (XINT (x, 1) == UNSPEC_RBIT)
        {
          if (speed)
            *cost += extra_cost->alu.rev;

          return false;
        }
      break;

    case TRUNCATE:

      /* Decompose <su>muldi3_highpart.  */
      if (/* (truncate:DI  */
	  mode == DImode
	  /*   (lshiftrt:TI  */
          && GET_MODE (XEXP (x, 0)) == TImode
          && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  /*      (mult:TI  */
          && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  /*        (ANY_EXTEND:TI (reg:DI))
	            (ANY_EXTEND:TI (reg:DI)))  */
          && ((GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
               && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == ZERO_EXTEND)
              || (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND
                  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == SIGN_EXTEND))
          && GET_MODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0)) == DImode
          && GET_MODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 1), 0)) == DImode
	  /*     (const_int 64)  */
          && CONST_INT_P (XEXP (XEXP (x, 0), 1))
          && UINTVAL (XEXP (XEXP (x, 0), 1)) == 64)
        {
          /* UMULH/SMULH.  */
	  if (speed)
	    *cost += extra_cost->mult[mode == DImode].extend;
          *cost += rtx_cost (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0),
			     MULT, 0, speed);
          *cost += rtx_cost (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 1), 0),
			     MULT, 1, speed);
          return true;
        }

      /* Fall through.  */
    default:
      break;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
      "\nFailed to cost RTX.  Assuming default cost.\n");

  return true;
}

/* Wrapper around aarch64_rtx_costs, dumps the partial, or total cost
   calculated for X.  This cost is stored in *COST.  Returns true
   if the total cost of X was calculated.  */
static bool
aarch64_rtx_costs_wrapper (rtx x, int code, int outer,
		   int param, int *cost, bool speed)
{
  bool result = aarch64_rtx_costs (x, code, outer, param, cost, speed);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      print_rtl_single (dump_file, x);
      fprintf (dump_file, "\n%s cost: %d (%s)\n",
	       speed ? "Hot" : "Cold",
	       *cost, result ? "final" : "partial");
    }

  return result;
}

static int
aarch64_register_move_cost (machine_mode mode,
			    reg_class_t from_i, reg_class_t to_i)
{
  enum reg_class from = (enum reg_class) from_i;
  enum reg_class to = (enum reg_class) to_i;
  const struct cpu_regmove_cost *regmove_cost
    = aarch64_tune_params->regmove_cost;

  /* Caller save and pointer regs are equivalent to GENERAL_REGS.  */
  if (to == CALLER_SAVE_REGS || to == POINTER_REGS)
    to = GENERAL_REGS;

  if (from == CALLER_SAVE_REGS || from == POINTER_REGS)
    from = GENERAL_REGS;

  /* Moving between GPR and stack cost is the same as GP2GP.  */
  if ((from == GENERAL_REGS && to == STACK_REG)
      || (to == GENERAL_REGS && from == STACK_REG))
    return regmove_cost->GP2GP;

  /* To/From the stack register, we move via the gprs.  */
  if (to == STACK_REG || from == STACK_REG)
    return aarch64_register_move_cost (mode, from, GENERAL_REGS)
            + aarch64_register_move_cost (mode, GENERAL_REGS, to);

  if (GET_MODE_SIZE (mode) == 16)
    {
      /* 128-bit operations on general registers require 2 instructions.  */
      if (from == GENERAL_REGS && to == GENERAL_REGS)
	return regmove_cost->GP2GP * 2;
      else if (from == GENERAL_REGS)
	return regmove_cost->GP2FP * 2;
      else if (to == GENERAL_REGS)
	return regmove_cost->FP2GP * 2;

      /* When AdvSIMD instructions are disabled it is not possible to move
	 a 128-bit value directly between Q registers.  This is handled in
	 secondary reload.  A general register is used as a scratch to move
	 the upper DI value and the lower DI value is moved directly,
	 hence the cost is the sum of three moves. */
      if (! TARGET_SIMD)
	return regmove_cost->GP2FP + regmove_cost->FP2GP + regmove_cost->FP2FP;

      return regmove_cost->FP2FP;
    }

  if (from == GENERAL_REGS && to == GENERAL_REGS)
    return regmove_cost->GP2GP;
  else if (from == GENERAL_REGS)
    return regmove_cost->GP2FP;
  else if (to == GENERAL_REGS)
    return regmove_cost->FP2GP;

  return regmove_cost->FP2FP;
}

static int
aarch64_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			  reg_class_t rclass ATTRIBUTE_UNUSED,
			  bool in ATTRIBUTE_UNUSED)
{
  return aarch64_tune_params->memmov_cost;
}

/* Return the number of instructions that can be issued per cycle.  */
static int
aarch64_sched_issue_rate (void)
{
  return aarch64_tune_params->issue_rate;
}

static int
aarch64_sched_first_cycle_multipass_dfa_lookahead (void)
{
  int issue_rate = aarch64_sched_issue_rate ();

  return issue_rate > 1 && !sched_fusion ? issue_rate : 0;
}

/* Vectorizer cost model target hooks.  */

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
aarch64_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				    tree vectype,
				    int misalign ATTRIBUTE_UNUSED)
{
  unsigned elements;

  switch (type_of_cost)
    {
      case scalar_stmt:
	return aarch64_tune_params->vec_costs->scalar_stmt_cost;

      case scalar_load:
	return aarch64_tune_params->vec_costs->scalar_load_cost;

      case scalar_store:
	return aarch64_tune_params->vec_costs->scalar_store_cost;

      case vector_stmt:
	return aarch64_tune_params->vec_costs->vec_stmt_cost;

      case vector_load:
	return aarch64_tune_params->vec_costs->vec_align_load_cost;

      case vector_store:
	return aarch64_tune_params->vec_costs->vec_store_cost;

      case vec_to_scalar:
	return aarch64_tune_params->vec_costs->vec_to_scalar_cost;

      case scalar_to_vec:
	return aarch64_tune_params->vec_costs->scalar_to_vec_cost;

      case unaligned_load:
	return aarch64_tune_params->vec_costs->vec_unalign_load_cost;

      case unaligned_store:
	return aarch64_tune_params->vec_costs->vec_unalign_store_cost;

      case cond_branch_taken:
	return aarch64_tune_params->vec_costs->cond_taken_branch_cost;

      case cond_branch_not_taken:
	return aarch64_tune_params->vec_costs->cond_not_taken_branch_cost;

      case vec_perm:
      case vec_promote_demote:
	return aarch64_tune_params->vec_costs->vec_stmt_cost;

      case vec_construct:
        elements = TYPE_VECTOR_SUBPARTS (vectype);
	return elements / 2 + 1;

      default:
	gcc_unreachable ();
    }
}

/* Implement targetm.vectorize.add_stmt_cost.  */
static unsigned
aarch64_add_stmt_cost (void *data, int count, enum vect_cost_for_stmt kind,
		       struct _stmt_vec_info *stmt_info, int misalign,
		       enum vect_cost_model_location where)
{
  unsigned *cost = (unsigned *) data;
  unsigned retval = 0;

  if (flag_vect_cost_model)
    {
      tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
      int stmt_cost =
	    aarch64_builtin_vectorization_cost (kind, vectype, misalign);

      /* Statements in an inner loop relative to the loop being
	 vectorized are weighted more heavily.  The value here is
	 a function (linear for now) of the loop nest level.  */
      if (where == vect_body && stmt_info && stmt_in_inner_loop_p (stmt_info))
	{
	  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_info);
	  struct loop *loop =  LOOP_VINFO_LOOP (loop_info);
	  unsigned nest_level = loop_depth (loop);

	  count *= nest_level;
	}

      retval = (unsigned) (count * stmt_cost);
      cost[where] += retval;
    }

  return retval;
}

static void initialize_aarch64_code_model (void);

/* Parse the architecture extension string.  */

static void
aarch64_parse_extension (char *str)
{
  /* The extension string is parsed left to right.  */
  const struct aarch64_option_extension *opt = NULL;

  /* Flag to say whether we are adding or removing an extension.  */
  int adding_ext = -1;

  while (str != NULL && *str != 0)
    {
      char *ext;
      size_t len;

      str++;
      ext = strchr (str, '+');

      if (ext != NULL)
	len = ext - str;
      else
	len = strlen (str);

      if (len >= 2 && strncmp (str, "no", 2) == 0)
	{
	  adding_ext = 0;
	  len -= 2;
	  str += 2;
	}
      else if (len > 0)
	adding_ext = 1;

      if (len == 0)
	{
	  error ("missing feature modifier after %qs", adding_ext ? "+"
	                                                          : "+no");
	  return;
	}

      /* Scan over the extensions table trying to find an exact match.  */
      for (opt = all_extensions; opt->name != NULL; opt++)
	{
	  if (strlen (opt->name) == len && strncmp (opt->name, str, len) == 0)
	    {
	      /* Add or remove the extension.  */
	      if (adding_ext)
		aarch64_isa_flags |= opt->flags_on;
	      else
		aarch64_isa_flags &= ~(opt->flags_off);
	      break;
	    }
	}

      if (opt->name == NULL)
	{
	  /* Extension not found in list.  */
	  error ("unknown feature modifier %qs", str);
	  return;
	}

      str = ext;
    };

  return;
}

/* Parse the ARCH string.  */

static void
aarch64_parse_arch (void)
{
  char *ext;
  const struct processor *arch;
  char *str = (char *) alloca (strlen (aarch64_arch_string) + 1);
  size_t len;

  strcpy (str, aarch64_arch_string);

  ext = strchr (str, '+');

  if (ext != NULL)
    len = ext - str;
  else
    len = strlen (str);

  if (len == 0)
    {
      error ("missing arch name in -march=%qs", str);
      return;
    }

  /* Loop through the list of supported ARCHs to find a match.  */
  for (arch = all_architectures; arch->name != NULL; arch++)
    {
      if (strlen (arch->name) == len && strncmp (arch->name, str, len) == 0)
	{
	  selected_arch = arch;
	  aarch64_isa_flags = selected_arch->flags;

	  if (!selected_cpu)
	    selected_cpu = &all_cores[selected_arch->core];

	  if (ext != NULL)
	    {
	      /* ARCH string contains at least one extension.  */
	      aarch64_parse_extension (ext);
	    }

	  if (strcmp (selected_arch->arch, selected_cpu->arch))
	    {
	      warning (0, "switch -mcpu=%s conflicts with -march=%s switch",
		       selected_cpu->name, selected_arch->name);
	    }

	  return;
	}
    }

  /* ARCH name not found in list.  */
  error ("unknown value %qs for -march", str);
  return;
}

/* Parse the CPU string.  */

static void
aarch64_parse_cpu (void)
{
  char *ext;
  const struct processor *cpu;
  char *str = (char *) alloca (strlen (aarch64_cpu_string) + 1);
  size_t len;

  strcpy (str, aarch64_cpu_string);

  ext = strchr (str, '+');

  if (ext != NULL)
    len = ext - str;
  else
    len = strlen (str);

  if (len == 0)
    {
      error ("missing cpu name in -mcpu=%qs", str);
      return;
    }

  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strlen (cpu->name) == len && strncmp (cpu->name, str, len) == 0)
	{
	  selected_cpu = cpu;
	  aarch64_isa_flags = selected_cpu->flags;

	  if (ext != NULL)
	    {
	      /* CPU string contains at least one extension.  */
	      aarch64_parse_extension (ext);
	    }

	  return;
	}
    }

  /* CPU name not found in list.  */
  error ("unknown value %qs for -mcpu", str);
  return;
}

/* Parse the TUNE string.  */

static void
aarch64_parse_tune (void)
{
  const struct processor *cpu;
  char *str = (char *) alloca (strlen (aarch64_tune_string) + 1);
  strcpy (str, aarch64_tune_string);

  /* Loop through the list of supported CPUs to find a match.  */
  for (cpu = all_cores; cpu->name != NULL; cpu++)
    {
      if (strcmp (cpu->name, str) == 0)
	{
	  selected_tune = cpu;
	  return;
	}
    }

  /* CPU name not found in list.  */
  error ("unknown value %qs for -mtune", str);
  return;
}


/* Implement TARGET_OPTION_OVERRIDE.  */

static void
aarch64_override_options (void)
{
  /* -mcpu=CPU is shorthand for -march=ARCH_FOR_CPU, -mtune=CPU.
     If either of -march or -mtune is given, they override their
     respective component of -mcpu.

     So, first parse AARCH64_CPU_STRING, then the others, be careful
     with -march as, if -mcpu is not present on the command line, march
     must set a sensible default CPU.  */
  if (aarch64_cpu_string)
    {
      aarch64_parse_cpu ();
    }

  if (aarch64_arch_string)
    {
      aarch64_parse_arch ();
    }

  if (aarch64_tune_string)
    {
      aarch64_parse_tune ();
    }

#ifndef HAVE_AS_MABI_OPTION
  /* The compiler may have been configured with 2.23.* binutils, which does
     not have support for ILP32.  */
  if (TARGET_ILP32)
    error ("Assembler does not support -mabi=ilp32");
#endif

  initialize_aarch64_code_model ();

  aarch64_build_bitmask_table ();

  /* This target defaults to strict volatile bitfields.  */
  if (flag_strict_volatile_bitfields < 0 && abi_version_at_least (2))
    flag_strict_volatile_bitfields = 1;

  /* If the user did not specify a processor, choose the default
     one for them.  This will be the CPU set during configuration using
     --with-cpu, otherwise it is "generic".  */
  if (!selected_cpu)
    {
      selected_cpu = &all_cores[TARGET_CPU_DEFAULT & 0x3f];
      aarch64_isa_flags = TARGET_CPU_DEFAULT >> 6;
    }

  gcc_assert (selected_cpu);

  if (!selected_tune)
    selected_tune = selected_cpu;

  aarch64_tune_flags = selected_tune->flags;
  aarch64_tune = selected_tune->core;
  aarch64_tune_params = selected_tune->tune;
  aarch64_architecture_version = selected_cpu->architecture_version;

  if (aarch64_fix_a53_err835769 == 2)
    {
#ifdef TARGET_FIX_ERR_A53_835769_DEFAULT
      aarch64_fix_a53_err835769 = 1;
#else
      aarch64_fix_a53_err835769 = 0;
#endif
    }

  aarch64_override_options_after_change ();
}

/* Implement targetm.override_options_after_change.  */

static void
aarch64_override_options_after_change (void)
{
  /* The logic here is that if we are disabling all frame pointer generation
     then we do not need to disable leaf frame pointer generation as a
     separate operation.  But if we are *only* disabling leaf frame pointer
     generation then we set flag_omit_frame_pointer to true, but in
     aarch64_frame_pointer_required we return false only for leaf functions.

     PR 70044: We have to be careful about being called multiple times for the
     same function.  Once we have decided to set flag_omit_frame_pointer just
     so that we can omit leaf frame pointers, we must then not interpret a
     second call as meaning that all frame pointer generation should be
     omitted.  We do this by setting flag_omit_frame_pointer to a special,
     non-zero value.  */

  if (flag_omit_frame_pointer == 2)
    flag_omit_frame_pointer = 0;

  if (flag_omit_frame_pointer)
    flag_omit_leaf_frame_pointer = false;
  else if (flag_omit_leaf_frame_pointer)
    flag_omit_frame_pointer = 2;

  /* If not optimizing for size, set the default
     alignment to what the target wants */
  if (!optimize_size)
    {
      if (align_loops <= 0)
	align_loops = aarch64_tune_params->loop_align;
      if (align_jumps <= 0)
	align_jumps = aarch64_tune_params->jump_align;
      if (align_functions <= 0)
	align_functions = aarch64_tune_params->function_align;
    }
}

static struct machine_function *
aarch64_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();
  return machine;
}

void
aarch64_init_expanders (void)
{
  init_machine_status = aarch64_init_machine_status;
}

/* A checking mechanism for the implementation of the various code models.  */
static void
initialize_aarch64_code_model (void)
{
   if (flag_pic)
     {
       switch (aarch64_cmodel_var)
	 {
	 case AARCH64_CMODEL_TINY:
	   aarch64_cmodel = AARCH64_CMODEL_TINY_PIC;
	   break;
	 case AARCH64_CMODEL_SMALL:
	   aarch64_cmodel = AARCH64_CMODEL_SMALL_PIC;
	   break;
	 case AARCH64_CMODEL_LARGE:
	   sorry ("code model %qs with -f%s", "large",
		  flag_pic > 1 ? "PIC" : "pic");
	 default:
	   gcc_unreachable ();
	 }
     }
   else
     aarch64_cmodel = aarch64_cmodel_var;
}

/* Return true if SYMBOL_REF X binds locally.  */

static bool
aarch64_symbol_binds_local_p (const_rtx x)
{
  return (SYMBOL_REF_DECL (x)
	  ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	  : SYMBOL_REF_LOCAL_P (x));
}

/* Return true if SYMBOL_REF X is thread local */
static bool
aarch64_tls_symbol_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  if (GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Classify a TLS symbol into one of the TLS kinds.  */
enum aarch64_symbol_type
aarch64_classify_tls_symbol (rtx x)
{
  enum tls_model tls_kind = tls_symbolic_operand_type (x);

  switch (tls_kind)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
    case TLS_MODEL_LOCAL_DYNAMIC:
      return TARGET_TLS_DESC ? SYMBOL_SMALL_TLSDESC : SYMBOL_SMALL_TLSGD;

    case TLS_MODEL_INITIAL_EXEC:
      return SYMBOL_SMALL_GOTTPREL;

    case TLS_MODEL_LOCAL_EXEC:
      return SYMBOL_SMALL_TPREL;

    case TLS_MODEL_EMULATED:
    case TLS_MODEL_NONE:
      return SYMBOL_FORCE_TO_MEM;

    default:
      gcc_unreachable ();
    }
}

/* Return the method that should be used to access SYMBOL_REF or
   LABEL_REF X in context CONTEXT.  */

enum aarch64_symbol_type
aarch64_classify_symbol (rtx x, rtx offset,
			 enum aarch64_symbol_context context ATTRIBUTE_UNUSED)
{
  if (GET_CODE (x) == LABEL_REF)
    {
      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_LARGE:
	  return SYMBOL_FORCE_TO_MEM;

	case AARCH64_CMODEL_TINY_PIC:
	case AARCH64_CMODEL_TINY:
	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL_PIC:
	case AARCH64_CMODEL_SMALL:
	  return SYMBOL_SMALL_ABSOLUTE;

	default:
	  gcc_unreachable ();
	}
    }

  if (GET_CODE (x) == SYMBOL_REF)
    {
      if (aarch64_tls_symbol_p (x))
	return aarch64_classify_tls_symbol (x);

      switch (aarch64_cmodel)
	{
	case AARCH64_CMODEL_TINY:
	  /* When we retreive symbol + offset address, we have to make sure
	     the offset does not cause overflow of the final address.  But
	     we have no way of knowing the address of symbol at compile time
	     so we can't accurately say if the distance between the PC and
	     symbol + offset is outside the addressible range of +/-1M in the
	     TINY code model.  So we rely on images not being greater than
	     1M and cap the offset at 1M and anything beyond 1M will have to
	     be loaded using an alternative mechanism.  */
	  if (SYMBOL_REF_WEAK (x)
	      || INTVAL (offset) < -1048575 || INTVAL (offset) > 1048575)
	    return SYMBOL_FORCE_TO_MEM;
	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL:
	  /* Same reasoning as the tiny code model, but the offset cap here is
	     4G.  */
	  if (SYMBOL_REF_WEAK (x)
	      || !IN_RANGE (INTVAL (offset), HOST_WIDE_INT_C (-4294967263),
			    HOST_WIDE_INT_C (4294967264)))
	    return SYMBOL_FORCE_TO_MEM;
	  return SYMBOL_SMALL_ABSOLUTE;

	case AARCH64_CMODEL_TINY_PIC:
	  if (!aarch64_symbol_binds_local_p (x))
	    return SYMBOL_TINY_GOT;
	  return SYMBOL_TINY_ABSOLUTE;

	case AARCH64_CMODEL_SMALL_PIC:
	  if (!aarch64_symbol_binds_local_p (x))
	    return SYMBOL_SMALL_GOT;
	  return SYMBOL_SMALL_ABSOLUTE;

	case AARCH64_CMODEL_LARGE:
	  return SYMBOL_FORCE_TO_MEM;

	default:
	  gcc_unreachable ();
	}
    }

  /* By default push everything into the constant pool.  */
  return SYMBOL_FORCE_TO_MEM;
}

bool
aarch64_constant_address_p (rtx x)
{
  return (CONSTANT_P (x) && memory_address_p (DImode, x));
}

bool
aarch64_legitimate_pic_operand_p (rtx x)
{
  if (GET_CODE (x) == SYMBOL_REF
      || (GET_CODE (x) == CONST
	  && GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF))
     return false;

  return true;
}

/* Return true if X holds either a quarter-precision or
     floating-point +0.0 constant.  */
static bool
aarch64_valid_floating_const (machine_mode mode, rtx x)
{
  if (!CONST_DOUBLE_P (x))
    return false;

  /* TODO: We could handle moving 0.0 to a TFmode register,
     but first we would like to refactor the movtf_aarch64
     to be more amicable to split moves properly and
     correctly gate on TARGET_SIMD.  For now - reject all
     constants which are not to SFmode or DFmode registers.  */
  if (!(mode == SFmode || mode == DFmode))
    return false;

  if (aarch64_float_const_zero_rtx_p (x))
    return true;
  return aarch64_float_const_representable_p (x);
}

static bool
aarch64_legitimate_constant_p (machine_mode mode, rtx x)
{
  /* Do not allow vector struct mode constants.  We could support
     0 and -1 easily, but they need support in aarch64-simd.md.  */
  if (TARGET_SIMD && aarch64_vect_struct_mode_p (mode))
    return false;

  /* This could probably go away because
     we now decompose CONST_INTs according to expand_mov_immediate.  */
  if ((GET_CODE (x) == CONST_VECTOR
       && aarch64_simd_valid_immediate (x, mode, false, NULL))
      || CONST_INT_P (x) || aarch64_valid_floating_const (mode, x))
	return !targetm.cannot_force_const_mem (mode, x);

  if (GET_CODE (x) == HIGH
      && aarch64_valid_symref (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
    return true;

  return aarch64_constant_address_p (x);
}

rtx
aarch64_load_tp (rtx target)
{
  if (!target
      || GET_MODE (target) != Pmode
      || !register_operand (target, Pmode))
    target = gen_reg_rtx (Pmode);

  /* Can return in any reg.  */
  emit_insn (gen_aarch64_load_tp_hard (target));
  return target;
}

/* On AAPCS systems, this is the "struct __va_list".  */
static GTY(()) tree va_list_type;

/* Implement TARGET_BUILD_BUILTIN_VA_LIST.
   Return the type to use as __builtin_va_list.

   AAPCS64 \S 7.1.4 requires that va_list be a typedef for a type defined as:

   struct __va_list
   {
     void *__stack;
     void *__gr_top;
     void *__vr_top;
     int   __gr_offs;
     int   __vr_offs;
   };  */

static tree
aarch64_build_builtin_va_list (void)
{
  tree va_list_name;
  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;

  /* Create the type.  */
  va_list_type = lang_hooks.types.make_type (RECORD_TYPE);
  /* Give it the required name.  */
  va_list_name = build_decl (BUILTINS_LOCATION,
			     TYPE_DECL,
			     get_identifier ("__va_list"),
			     va_list_type);
  DECL_ARTIFICIAL (va_list_name) = 1;
  TYPE_NAME (va_list_type) = va_list_name;
  TYPE_STUB_DECL (va_list_type) = va_list_name;

  /* Create the fields.  */
  f_stack = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__stack"),
			ptr_type_node);
  f_grtop = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__gr_top"),
			ptr_type_node);
  f_vrtop = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__vr_top"),
			ptr_type_node);
  f_groff = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__gr_offs"),
			integer_type_node);
  f_vroff = build_decl (BUILTINS_LOCATION,
			FIELD_DECL, get_identifier ("__vr_offs"),
			integer_type_node);

  DECL_ARTIFICIAL (f_stack) = 1;
  DECL_ARTIFICIAL (f_grtop) = 1;
  DECL_ARTIFICIAL (f_vrtop) = 1;
  DECL_ARTIFICIAL (f_groff) = 1;
  DECL_ARTIFICIAL (f_vroff) = 1;

  DECL_FIELD_CONTEXT (f_stack) = va_list_type;
  DECL_FIELD_CONTEXT (f_grtop) = va_list_type;
  DECL_FIELD_CONTEXT (f_vrtop) = va_list_type;
  DECL_FIELD_CONTEXT (f_groff) = va_list_type;
  DECL_FIELD_CONTEXT (f_vroff) = va_list_type;

  TYPE_FIELDS (va_list_type) = f_stack;
  DECL_CHAIN (f_stack) = f_grtop;
  DECL_CHAIN (f_grtop) = f_vrtop;
  DECL_CHAIN (f_vrtop) = f_groff;
  DECL_CHAIN (f_groff) = f_vroff;

  /* Compute its layout.  */
  layout_type (va_list_type);

  return va_list_type;
}

/* Implement TARGET_EXPAND_BUILTIN_VA_START.  */
static void
aarch64_expand_builtin_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  const CUMULATIVE_ARGS *cum;
  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;
  tree stack, grtop, vrtop, groff, vroff;
  tree t;
  int gr_save_area_size;
  int vr_save_area_size;
  int vr_offset;

  cum = &crtl->args.info;
  gr_save_area_size
    = (NUM_ARG_REGS - cum->aapcs_ncrn) * UNITS_PER_WORD;
  vr_save_area_size
    = (NUM_FP_ARG_REGS - cum->aapcs_nvrn) * UNITS_PER_VREG;

  if (!TARGET_FLOAT)
    {
      if (cum->aapcs_nvrn > 0)
	sorry ("%qs and floating point or vector arguments",
	       "-mgeneral-regs-only");
      vr_save_area_size = 0;
    }

  f_stack = TYPE_FIELDS (va_list_type_node);
  f_grtop = DECL_CHAIN (f_stack);
  f_vrtop = DECL_CHAIN (f_grtop);
  f_groff = DECL_CHAIN (f_vrtop);
  f_vroff = DECL_CHAIN (f_groff);

  stack = build3 (COMPONENT_REF, TREE_TYPE (f_stack), valist, f_stack,
		  NULL_TREE);
  grtop = build3 (COMPONENT_REF, TREE_TYPE (f_grtop), valist, f_grtop,
		  NULL_TREE);
  vrtop = build3 (COMPONENT_REF, TREE_TYPE (f_vrtop), valist, f_vrtop,
		  NULL_TREE);
  groff = build3 (COMPONENT_REF, TREE_TYPE (f_groff), valist, f_groff,
		  NULL_TREE);
  vroff = build3 (COMPONENT_REF, TREE_TYPE (f_vroff), valist, f_vroff,
		  NULL_TREE);

  /* Emit code to initialize STACK, which points to the next varargs stack
     argument.  CUM->AAPCS_STACK_SIZE gives the number of stack words used
     by named arguments.  STACK is 8-byte aligned.  */
  t = make_tree (TREE_TYPE (stack), virtual_incoming_args_rtx);
  if (cum->aapcs_stack_size > 0)
    t = fold_build_pointer_plus_hwi (t, cum->aapcs_stack_size * UNITS_PER_WORD);
  t = build2 (MODIFY_EXPR, TREE_TYPE (stack), stack, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize GRTOP, the top of the GR save area.
     virtual_incoming_args_rtx should have been 16 byte aligned.  */
  t = make_tree (TREE_TYPE (grtop), virtual_incoming_args_rtx);
  t = build2 (MODIFY_EXPR, TREE_TYPE (grtop), grtop, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize VRTOP, the top of the VR save area.
     This address is gr_save_area_bytes below GRTOP, rounded
     down to the next 16-byte boundary.  */
  t = make_tree (TREE_TYPE (vrtop), virtual_incoming_args_rtx);
  vr_offset = AARCH64_ROUND_UP (gr_save_area_size,
			     STACK_BOUNDARY / BITS_PER_UNIT);

  if (vr_offset)
    t = fold_build_pointer_plus_hwi (t, -vr_offset);
  t = build2 (MODIFY_EXPR, TREE_TYPE (vrtop), vrtop, t);
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Emit code to initialize GROFF, the offset from GRTOP of the
     next GPR argument.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (groff), groff,
	      build_int_cst (TREE_TYPE (groff), -gr_save_area_size));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* Likewise emit code to initialize VROFF, the offset from FTOP
     of the next VR argument.  */
  t = build2 (MODIFY_EXPR, TREE_TYPE (vroff), vroff,
	      build_int_cst (TREE_TYPE (vroff), -vr_save_area_size));
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement TARGET_GIMPLIFY_VA_ARG_EXPR.  */

static tree
aarch64_gimplify_va_arg_expr (tree valist, tree type, gimple_seq *pre_p,
			      gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree addr;
  bool indirect_p;
  bool is_ha;		/* is HFA or HVA.  */
  bool dw_align;	/* double-word align.  */
  machine_mode ag_mode = VOIDmode;
  int nregs;
  machine_mode mode;

  tree f_stack, f_grtop, f_vrtop, f_groff, f_vroff;
  tree stack, f_top, f_off, off, arg, roundup, on_stack;
  HOST_WIDE_INT size, rsize, adjust, align;
  tree t, u, cond1, cond2;

  indirect_p = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect_p)
    type = build_pointer_type (type);

  mode = TYPE_MODE (type);

  f_stack = TYPE_FIELDS (va_list_type_node);
  f_grtop = DECL_CHAIN (f_stack);
  f_vrtop = DECL_CHAIN (f_grtop);
  f_groff = DECL_CHAIN (f_vrtop);
  f_vroff = DECL_CHAIN (f_groff);

  stack = build3 (COMPONENT_REF, TREE_TYPE (f_stack), unshare_expr (valist),
		  f_stack, NULL_TREE);
  size = int_size_in_bytes (type);
  align = aarch64_function_arg_alignment (mode, type) / BITS_PER_UNIT;

  dw_align = false;
  adjust = 0;
  if (aarch64_vfp_is_call_or_return_candidate (mode,
					       type,
					       &ag_mode,
					       &nregs,
					       &is_ha))
    {
      /* TYPE passed in fp/simd registers.  */
      if (!TARGET_FLOAT)
	sorry ("%qs and floating point or vector arguments",
	       "-mgeneral-regs-only");

      f_top = build3 (COMPONENT_REF, TREE_TYPE (f_vrtop),
		      unshare_expr (valist), f_vrtop, NULL_TREE);
      f_off = build3 (COMPONENT_REF, TREE_TYPE (f_vroff),
		      unshare_expr (valist), f_vroff, NULL_TREE);

      rsize = nregs * UNITS_PER_VREG;

      if (is_ha)
	{
	  if (BYTES_BIG_ENDIAN && GET_MODE_SIZE (ag_mode) < UNITS_PER_VREG)
	    adjust = UNITS_PER_VREG - GET_MODE_SIZE (ag_mode);
	}
      else if (BLOCK_REG_PADDING (mode, type, 1) == downward
	       && size < UNITS_PER_VREG)
	{
	  adjust = UNITS_PER_VREG - size;
	}
    }
  else
    {
      /* TYPE passed in general registers.  */
      f_top = build3 (COMPONENT_REF, TREE_TYPE (f_grtop),
		      unshare_expr (valist), f_grtop, NULL_TREE);
      f_off = build3 (COMPONENT_REF, TREE_TYPE (f_groff),
		      unshare_expr (valist), f_groff, NULL_TREE);
      rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;
      nregs = rsize / UNITS_PER_WORD;

      if (align > 8)
	dw_align = true;

      if (BLOCK_REG_PADDING (mode, type, 1) == downward
	  && size < UNITS_PER_WORD)
	{
	  adjust = UNITS_PER_WORD  - size;
	}
    }

  /* Get a local temporary for the field value.  */
  off = get_initialized_tmp_var (f_off, pre_p, NULL);

  /* Emit code to branch if off >= 0.  */
  t = build2 (GE_EXPR, boolean_type_node, off,
	      build_int_cst (TREE_TYPE (off), 0));
  cond1 = build3 (COND_EXPR, ptr_type_node, t, NULL_TREE, NULL_TREE);

  if (dw_align)
    {
      /* Emit: offs = (offs + 15) & -16.  */
      t = build2 (PLUS_EXPR, TREE_TYPE (off), off,
		  build_int_cst (TREE_TYPE (off), 15));
      t = build2 (BIT_AND_EXPR, TREE_TYPE (off), t,
		  build_int_cst (TREE_TYPE (off), -16));
      roundup = build2 (MODIFY_EXPR, TREE_TYPE (off), off, t);
    }
  else
    roundup = NULL;

  /* Update ap.__[g|v]r_offs  */
  t = build2 (PLUS_EXPR, TREE_TYPE (off), off,
	      build_int_cst (TREE_TYPE (off), rsize));
  t = build2 (MODIFY_EXPR, TREE_TYPE (f_off), unshare_expr (f_off), t);

  /* String up.  */
  if (roundup)
    t = build2 (COMPOUND_EXPR, TREE_TYPE (t), roundup, t);

  /* [cond2] if (ap.__[g|v]r_offs > 0)  */
  u = build2 (GT_EXPR, boolean_type_node, unshare_expr (f_off),
	      build_int_cst (TREE_TYPE (f_off), 0));
  cond2 = build3 (COND_EXPR, ptr_type_node, u, NULL_TREE, NULL_TREE);

  /* String up: make sure the assignment happens before the use.  */
  t = build2 (COMPOUND_EXPR, TREE_TYPE (cond2), t, cond2);
  COND_EXPR_ELSE (cond1) = t;

  /* Prepare the trees handling the argument that is passed on the stack;
     the top level node will store in ON_STACK.  */
  arg = get_initialized_tmp_var (stack, pre_p, NULL);
  if (align > 8)
    {
      /* if (alignof(type) > 8) (arg = arg + 15) & -16;  */
      t = fold_convert (intDI_type_node, arg);
      t = build2 (PLUS_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), 15));
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), -16));
      t = fold_convert (TREE_TYPE (arg), t);
      roundup = build2 (MODIFY_EXPR, TREE_TYPE (arg), arg, t);
    }
  else
    roundup = NULL;
  /* Advance ap.__stack  */
  t = fold_convert (intDI_type_node, arg);
  t = build2 (PLUS_EXPR, TREE_TYPE (t), t,
	      build_int_cst (TREE_TYPE (t), size + 7));
  t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
	      build_int_cst (TREE_TYPE (t), -8));
  t = fold_convert (TREE_TYPE (arg), t);
  t = build2 (MODIFY_EXPR, TREE_TYPE (stack), unshare_expr (stack), t);
  /* String up roundup and advance.  */
  if (roundup)
    t = build2 (COMPOUND_EXPR, TREE_TYPE (t), roundup, t);
  /* String up with arg */
  on_stack = build2 (COMPOUND_EXPR, TREE_TYPE (arg), t, arg);
  /* Big-endianness related address adjustment.  */
  if (BLOCK_REG_PADDING (mode, type, 1) == downward
      && size < UNITS_PER_WORD)
  {
    t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (arg), arg,
		size_int (UNITS_PER_WORD - size));
    on_stack = build2 (COMPOUND_EXPR, TREE_TYPE (arg), on_stack, t);
  }

  COND_EXPR_THEN (cond1) = unshare_expr (on_stack);
  COND_EXPR_THEN (cond2) = unshare_expr (on_stack);

  /* Adjustment to OFFSET in the case of BIG_ENDIAN.  */
  t = off;
  if (adjust)
    t = build2 (PREINCREMENT_EXPR, TREE_TYPE (off), off,
		build_int_cst (TREE_TYPE (off), adjust));

  t = fold_convert (sizetype, t);
  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (f_top), f_top, t);

  if (is_ha)
    {
      /* type ha; // treat as "struct {ftype field[n];}"
         ... [computing offs]
         for (i = 0; i <nregs; ++i, offs += 16)
	   ha.field[i] = *((ftype *)(ap.__vr_top + offs));
	 return ha;  */
      int i;
      tree tmp_ha, field_t, field_ptr_t;

      /* Declare a local variable.  */
      tmp_ha = create_tmp_var_raw (type, "ha");
      gimple_add_tmp_var (tmp_ha);

      /* Establish the base type.  */
      switch (ag_mode)
	{
	case SFmode:
	  field_t = float_type_node;
	  field_ptr_t = float_ptr_type_node;
	  break;
	case DFmode:
	  field_t = double_type_node;
	  field_ptr_t = double_ptr_type_node;
	  break;
	case TFmode:
	  field_t = long_double_type_node;
	  field_ptr_t = long_double_ptr_type_node;
	  break;
/* The half precision and quad precision are not fully supported yet.  Enable
   the following code after the support is complete.  Need to find the correct
   type node for __fp16 *.  */
#if 0
	case HFmode:
	  field_t = float_type_node;
	  field_ptr_t = float_ptr_type_node;
	  break;
#endif
	case V2SImode:
	case V4SImode:
	    {
	      tree innertype = make_signed_type (GET_MODE_PRECISION (SImode));
	      field_t = build_vector_type_for_mode (innertype, ag_mode);
	      field_ptr_t = build_pointer_type (field_t);
	    }
	  break;
	default:
	  gcc_assert (0);
	}

      /* *(field_ptr_t)&ha = *((field_ptr_t)vr_saved_area  */
      tmp_ha = build1 (ADDR_EXPR, field_ptr_t, tmp_ha);
      addr = t;
      t = fold_convert (field_ptr_t, addr);
      t = build2 (MODIFY_EXPR, field_t,
		  build1 (INDIRECT_REF, field_t, tmp_ha),
		  build1 (INDIRECT_REF, field_t, t));

      /* ha.field[i] = *((field_ptr_t)vr_saved_area + i)  */
      for (i = 1; i < nregs; ++i)
	{
	  addr = fold_build_pointer_plus_hwi (addr, UNITS_PER_VREG);
	  u = fold_convert (field_ptr_t, addr);
	  u = build2 (MODIFY_EXPR, field_t,
		      build2 (MEM_REF, field_t, tmp_ha,
			      build_int_cst (field_ptr_t,
					     (i *
					      int_size_in_bytes (field_t)))),
		      build1 (INDIRECT_REF, field_t, u));
	  t = build2 (COMPOUND_EXPR, TREE_TYPE (t), t, u);
	}

      u = fold_convert (TREE_TYPE (f_top), tmp_ha);
      t = build2 (COMPOUND_EXPR, TREE_TYPE (f_top), t, u);
    }

  COND_EXPR_ELSE (cond2) = t;
  addr = fold_convert (build_pointer_type (type), cond1);
  addr = build_va_arg_indirect_ref (addr);

  if (indirect_p)
    addr = build_va_arg_indirect_ref (addr);

  return addr;
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
aarch64_setup_incoming_varargs (cumulative_args_t cum_v, machine_mode mode,
				tree type, int *pretend_size ATTRIBUTE_UNUSED,
				int no_rtl)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  CUMULATIVE_ARGS local_cum;
  int gr_saved, vr_saved;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  local_cum = *cum;
  aarch64_function_arg_advance (pack_cumulative_args(&local_cum), mode, type, true);

  /* Found out how many registers we need to save.  */
  gr_saved = NUM_ARG_REGS - local_cum.aapcs_ncrn;
  vr_saved = NUM_FP_ARG_REGS - local_cum.aapcs_nvrn;

  if (!TARGET_FLOAT)
    {
      if (local_cum.aapcs_nvrn > 0)
	sorry ("%qs and floating point or vector arguments",
	       "-mgeneral-regs-only");
      vr_saved = 0;
    }

  if (!no_rtl)
    {
      if (gr_saved > 0)
	{
	  rtx ptr, mem;

	  /* virtual_incoming_args_rtx should have been 16-byte aligned.  */
	  ptr = plus_constant (Pmode, virtual_incoming_args_rtx,
			       - gr_saved * UNITS_PER_WORD);
	  mem = gen_frame_mem (BLKmode, ptr);
	  set_mem_alias_set (mem, get_varargs_alias_set ());

	  move_block_from_reg (local_cum.aapcs_ncrn + R0_REGNUM,
			       mem, gr_saved);
	}
      if (vr_saved > 0)
	{
	  /* We can't use move_block_from_reg, because it will use
	     the wrong mode, storing D regs only.  */
	  machine_mode mode = TImode;
	  int off, i;

	  /* Set OFF to the offset from virtual_incoming_args_rtx of
	     the first vector register.  The VR save area lies below
	     the GR one, and is aligned to 16 bytes.  */
	  off = -AARCH64_ROUND_UP (gr_saved * UNITS_PER_WORD,
				   STACK_BOUNDARY / BITS_PER_UNIT);
	  off -= vr_saved * UNITS_PER_VREG;

	  for (i = local_cum.aapcs_nvrn; i < NUM_FP_ARG_REGS; ++i)
	    {
	      rtx ptr, mem;

	      ptr = plus_constant (Pmode, virtual_incoming_args_rtx, off);
	      mem = gen_frame_mem (mode, ptr);
	      set_mem_alias_set (mem, get_varargs_alias_set ());
	      aarch64_emit_move (mem, gen_rtx_REG (mode, V0_REGNUM + i));
	      off += UNITS_PER_VREG;
	    }
	}
    }

  /* We don't save the size into *PRETEND_SIZE because we want to avoid
     any complication of having crtl->args.pretend_args_size changed.  */
  cfun->machine->frame.saved_varargs_size
    = (AARCH64_ROUND_UP (gr_saved * UNITS_PER_WORD,
		      STACK_BOUNDARY / BITS_PER_UNIT)
       + vr_saved * UNITS_PER_VREG);
}

static void
aarch64_conditional_register_usage (void)
{
  int i;
  if (!TARGET_FLOAT)
    {
      for (i = V0_REGNUM; i <= V31_REGNUM; i++)
	{
	  fixed_regs[i] = 1;
	  call_used_regs[i] = 1;
	}
    }
}

/* Walk down the type tree of TYPE counting consecutive base elements.
   If *MODEP is VOIDmode, then set it to the first valid floating point
   type.  If a non-floating point type is found, or if a floating point
   type that doesn't match a non-VOIDmode *MODEP is found, then return -1,
   otherwise return the count in the sub-tree.  */
static int
aapcs_vfp_sub_candidate (const_tree type, machine_mode *modep)
{
  machine_mode mode;
  HOST_WIDE_INT size;

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      mode = TYPE_MODE (type);
      if (mode != DFmode && mode != SFmode && mode != TFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 1;

      break;

    case COMPLEX_TYPE:
      mode = TYPE_MODE (TREE_TYPE (type));
      if (mode != DFmode && mode != SFmode && mode != TFmode)
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 2;

      break;

    case VECTOR_TYPE:
      /* Use V2SImode and V4SImode as representatives of all 64-bit
	 and 128-bit vector types.  */
      size = int_size_in_bytes (type);
      switch (size)
	{
	case 8:
	  mode = V2SImode;
	  break;
	case 16:
	  mode = V4SImode;
	  break;
	default:
	  return -1;
	}

      if (*modep == VOIDmode)
	*modep = mode;

      /* Vector modes are considered to be opaque: two vectors are
	 equivalent for the purposes of being homogeneous aggregates
	 if they are the same size.  */
      if (*modep == mode)
	return 1;

      break;

    case ARRAY_TYPE:
      {
	int count;
	tree index = TYPE_DOMAIN (type);

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	count = aapcs_vfp_sub_candidate (TREE_TYPE (type), modep);
	if (count == -1
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || count < 0)
	  return -1;

	count *= (1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		      - tree_to_uhwi (TYPE_MIN_VALUE (index)));

	/* There must be no padding.  */
	if (wi::ne_p (TYPE_SIZE (type), count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case RECORD_TYPE:
      {
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count += sub_count;
	  }

	/* There must be no padding.  */
	if (wi::ne_p (TYPE_SIZE (type), count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* These aren't very interesting except in a degenerate case.  */
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types nor sizes that are not
	   fixed.  */
	if (!COMPLETE_TYPE_P (type)
	    || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = aapcs_vfp_sub_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count = count > sub_count ? count : sub_count;
	  }

	/* There must be no padding.  */
	if (wi::ne_p (TYPE_SIZE (type), count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    default:
      break;
    }

  return -1;
}

/* Return TRUE if the type, as described by TYPE and MODE, is a composite
   type as described in AAPCS64 \S 4.3.  This includes aggregate, union and
   array types.  The C99 floating-point complex types are also considered
   as composite types, according to AAPCS64 \S 7.1.1.  The complex integer
   types, which are GCC extensions and out of the scope of AAPCS64, are
   treated as composite types here as well.

   Note that MODE itself is not sufficient in determining whether a type
   is such a composite type or not.  This is because
   stor-layout.c:compute_record_mode may have already changed the MODE
   (BLKmode) of a RECORD_TYPE TYPE to some other mode.  For example, a
   structure with only one field may have its MODE set to the mode of the
   field.  Also an integer mode whose size matches the size of the
   RECORD_TYPE type may be used to substitute the original mode
   (i.e. BLKmode) in certain circumstances.  In other words, MODE cannot be
   solely relied on.  */

static bool
aarch64_composite_type_p (const_tree type,
			  machine_mode mode)
{
  if (type && (AGGREGATE_TYPE_P (type) || TREE_CODE (type) == COMPLEX_TYPE))
    return true;

  if (mode == BLKmode
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    return true;

  return false;
}

/* Return TRUE if the type, as described by TYPE and MODE, is a short vector
   type as described in AAPCS64 \S 4.1.2.

   See the comment above aarch64_composite_type_p for the notes on MODE.  */

static bool
aarch64_short_vector_p (const_tree type,
			machine_mode mode)
{
  HOST_WIDE_INT size = -1;

  if (type && TREE_CODE (type) == VECTOR_TYPE)
    size = int_size_in_bytes (type);
  else if (!aarch64_composite_type_p (type, mode)
	   && (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	       || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT))
    size = GET_MODE_SIZE (mode);

  return (size == 8 || size == 16) ? true : false;
}

/* Return TRUE if an argument, whose type is described by TYPE and MODE,
   shall be passed or returned in simd/fp register(s) (providing these
   parameter passing registers are available).

   Upon successful return, *COUNT returns the number of needed registers,
   *BASE_MODE returns the mode of the individual register and when IS_HAF
   is not NULL, *IS_HA indicates whether or not the argument is a homogeneous
   floating-point aggregate or a homogeneous short-vector aggregate.  */

static bool
aarch64_vfp_is_call_or_return_candidate (machine_mode mode,
					 const_tree type,
					 machine_mode *base_mode,
					 int *count,
					 bool *is_ha)
{
  machine_mode new_mode = VOIDmode;
  bool composite_p = aarch64_composite_type_p (type, mode);

  if (is_ha != NULL) *is_ha = false;

  if ((!composite_p && GET_MODE_CLASS (mode) == MODE_FLOAT)
      || aarch64_short_vector_p (type, mode))
    {
      *count = 1;
      new_mode = mode;
    }
  else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    {
      if (is_ha != NULL) *is_ha = true;
      *count = 2;
      new_mode = GET_MODE_INNER (mode);
    }
  else if (type && composite_p)
    {
      int ag_count = aapcs_vfp_sub_candidate (type, &new_mode);

      if (ag_count > 0 && ag_count <= HA_MAX_NUM_FLDS)
	{
	  if (is_ha != NULL) *is_ha = true;
	  *count = ag_count;
	}
      else
	return false;
    }
  else
    return false;

  *base_mode = new_mode;
  return true;
}

/* Implement TARGET_STRUCT_VALUE_RTX.  */

static rtx
aarch64_struct_value_rtx (tree fndecl ATTRIBUTE_UNUSED,
			  int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, AARCH64_STRUCT_VALUE_REGNUM);
}

/* Implements target hook vector_mode_supported_p.  */
static bool
aarch64_vector_mode_supported_p (machine_mode mode)
{
  if (TARGET_SIMD
      && (mode == V4SImode  || mode == V8HImode
	  || mode == V16QImode || mode == V2DImode
	  || mode == V2SImode  || mode == V4HImode
	  || mode == V8QImode || mode == V2SFmode
	  || mode == V4SFmode || mode == V2DFmode
	  || mode == V1DFmode))
    return true;

  return false;
}

/* Return appropriate SIMD container
   for MODE within a vector of WIDTH bits.  */
static machine_mode
aarch64_simd_container_mode (machine_mode mode, unsigned width)
{
  gcc_assert (width == 64 || width == 128);
  if (TARGET_SIMD)
    {
      if (width == 128)
	switch (mode)
	  {
	  case DFmode:
	    return V2DFmode;
	  case SFmode:
	    return V4SFmode;
	  case SImode:
	    return V4SImode;
	  case HImode:
	    return V8HImode;
	  case QImode:
	    return V16QImode;
	  case DImode:
	    return V2DImode;
	  default:
	    break;
	  }
      else
	switch (mode)
	  {
	  case SFmode:
	    return V2SFmode;
	  case SImode:
	    return V2SImode;
	  case HImode:
	    return V4HImode;
	  case QImode:
	    return V8QImode;
	  default:
	    break;
	  }
    }
  return word_mode;
}

/* Return 128-bit container as the preferred SIMD mode for MODE.  */
static machine_mode
aarch64_preferred_simd_mode (machine_mode mode)
{
  return aarch64_simd_container_mode (mode, 128);
}

/* Return the bitmask of possible vector sizes for the vectorizer
   to iterate over.  */
static unsigned int
aarch64_autovectorize_vector_sizes (void)
{
  return (16 | 8);
}

/* Implement TARGET_MANGLE_TYPE.  */

static const char *
aarch64_mangle_type (const_tree type)
{
  /* The AArch64 ABI documents say that "__va_list" has to be
     managled as if it is in the "std" namespace.  */
  if (lang_hooks.types_compatible_p (CONST_CAST_TREE (type), va_list_type))
    return "St9__va_list";

  /* Mangle AArch64-specific internal types.  TYPE_NAME is non-NULL_TREE for
     builtin types.  */
  if (TYPE_NAME (type) != NULL)
    return aarch64_mangle_builtin_type (type);

  /* Use the default mangling.  */
  return NULL;
}


/* Return true if the rtx_insn contains a MEM RTX somewhere
   in it.  */

static bool
has_memory_op (rtx_insn *mem_insn)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, PATTERN (mem_insn), ALL)
    if (MEM_P (*iter))
      return true;

  return false;
}

/* Find the first rtx_insn before insn that will generate an assembly
   instruction.  */

static rtx_insn *
aarch64_prev_real_insn (rtx_insn *insn)
{
  if (!insn)
    return NULL;

  do
    {
      insn = prev_real_insn (insn);
    }
  while (insn && recog_memoized (insn) < 0);

  return insn;
}

static bool
is_madd_op (enum attr_type t1)
{
  unsigned int i;
  /* A number of these may be AArch32 only.  */
  enum attr_type mlatypes[] = {
    TYPE_MLA, TYPE_MLAS, TYPE_SMLAD, TYPE_SMLADX, TYPE_SMLAL, TYPE_SMLALD,
    TYPE_SMLALS, TYPE_SMLALXY, TYPE_SMLAWX, TYPE_SMLAWY, TYPE_SMLAXY,
    TYPE_SMMLA, TYPE_UMLAL, TYPE_UMLALS,TYPE_SMLSD, TYPE_SMLSDX, TYPE_SMLSLD
  };

  for (i = 0; i < sizeof (mlatypes) / sizeof (enum attr_type); i++)
    {
      if (t1 == mlatypes[i])
	return true;
    }

  return false;
}

/* Check if there is a register dependency between a load and the insn
   for which we hold recog_data.  */

static bool
dep_between_memop_and_curr (rtx memop)
{
  rtx load_reg;
  int opno;

  gcc_assert (GET_CODE (memop) == SET);

  if (!REG_P (SET_DEST (memop)))
    return false;

  load_reg = SET_DEST (memop);
  for (opno = 1; opno < recog_data.n_operands; opno++)
    {
      rtx operand = recog_data.operand[opno];
      if (REG_P (operand)
          && reg_overlap_mentioned_p (load_reg, operand))
        return true;

    }
  return false;
}


/* When working around the Cortex-A53 erratum 835769,
   given rtx_insn INSN, return true if it is a 64-bit multiply-accumulate
   instruction and has a preceding memory instruction such that a NOP
   should be inserted between them.  */

bool
aarch64_madd_needs_nop (rtx_insn* insn)
{
  enum attr_type attr_type;
  rtx_insn *prev;
  rtx body;

  if (!aarch64_fix_a53_err835769)
    return false;

  if (!INSN_P (insn) || recog_memoized (insn) < 0)
    return false;

  attr_type = get_attr_type (insn);
  if (!is_madd_op (attr_type))
    return false;

  prev = aarch64_prev_real_insn (insn);
  /* aarch64_prev_real_insn can call recog_memoized on insns other than INSN.
     Restore recog state to INSN to avoid state corruption.  */
  extract_constrain_insn_cached (insn);

  if (!prev || !has_memory_op (prev))
    return false;

  body = single_set (prev);

  /* If the previous insn is a memory op and there is no dependency between
     it and the DImode madd, emit a NOP between them.  If body is NULL then we
     have a complex memory operation, probably a load/store pair.
     Be conservative for now and emit a NOP.  */
  if (GET_MODE (recog_data.operand[0]) == DImode
      && (!body || !dep_between_memop_and_curr (body)))
    return true;

  return false;

}


/* Implement FINAL_PRESCAN_INSN.  */

void
aarch64_final_prescan_insn (rtx_insn *insn)
{
  if (aarch64_madd_needs_nop (insn))
    fprintf (asm_out_file, "\tnop // between mem op and mult-accumulate\n");
}


/* Return the equivalent letter for size.  */
static char
sizetochar (int size)
{
  switch (size)
    {
    case 64: return 'd';
    case 32: return 's';
    case 16: return 'h';
    case 8 : return 'b';
    default: gcc_unreachable ();
    }
}

/* Return true iff x is a uniform vector of floating-point
   constants, and the constant can be represented in
   quarter-precision form.  Note, as aarch64_float_const_representable
   rejects both +0.0 and -0.0, we will also reject +0.0 and -0.0.  */
static bool
aarch64_vect_float_const_representable_p (rtx x)
{
  int i = 0;
  REAL_VALUE_TYPE r0, ri;
  rtx x0, xi;

  if (GET_MODE_CLASS (GET_MODE (x)) != MODE_VECTOR_FLOAT)
    return false;

  x0 = CONST_VECTOR_ELT (x, 0);
  if (!CONST_DOUBLE_P (x0))
    return false;

  REAL_VALUE_FROM_CONST_DOUBLE (r0, x0);

  for (i = 1; i < CONST_VECTOR_NUNITS (x); i++)
    {
      xi = CONST_VECTOR_ELT (x, i);
      if (!CONST_DOUBLE_P (xi))
	return false;

      REAL_VALUE_FROM_CONST_DOUBLE (ri, xi);
      if (!REAL_VALUES_EQUAL (r0, ri))
	return false;
    }

  return aarch64_float_const_representable_p (x0);
}

/* Return true for valid and false for invalid.  */
bool
aarch64_simd_valid_immediate (rtx op, machine_mode mode, bool inverse,
			      struct simd_immediate_info *info)
{
#define CHECK(STRIDE, ELSIZE, CLASS, TEST, SHIFT, NEG)	\
  matches = 1;						\
  for (i = 0; i < idx; i += (STRIDE))			\
    if (!(TEST))					\
      matches = 0;					\
  if (matches)						\
    {							\
      immtype = (CLASS);				\
      elsize = (ELSIZE);				\
      eshift = (SHIFT);					\
      emvn = (NEG);					\
      break;						\
    }

  unsigned int i, elsize = 0, idx = 0, n_elts = CONST_VECTOR_NUNITS (op);
  unsigned int innersize = GET_MODE_SIZE (GET_MODE_INNER (mode));
  unsigned char bytes[16];
  int immtype = -1, matches;
  unsigned int invmask = inverse ? 0xff : 0;
  int eshift, emvn;

  if (GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT)
    {
      if (! (aarch64_simd_imm_zero_p (op, mode)
	     || aarch64_vect_float_const_representable_p (op)))
	return false;

      if (info)
	{
	  info->value = CONST_VECTOR_ELT (op, 0);
	  info->element_width = GET_MODE_BITSIZE (GET_MODE (info->value));
	  info->mvn = false;
	  info->shift = 0;
	}

      return true;
    }

  /* Splat vector constant out into a byte vector.  */
  for (i = 0; i < n_elts; i++)
    {
      /* The vector is provided in gcc endian-neutral fashion.  For aarch64_be,
         it must be laid out in the vector register in reverse order.  */
      rtx el = CONST_VECTOR_ELT (op, BYTES_BIG_ENDIAN ? (n_elts - 1 - i) : i);
      unsigned HOST_WIDE_INT elpart;
      unsigned int part, parts;

      if (CONST_INT_P (el))
        {
          elpart = INTVAL (el);
          parts = 1;
        }
      else if (GET_CODE (el) == CONST_DOUBLE)
        {
          elpart = CONST_DOUBLE_LOW (el);
          parts = 2;
        }
      else
        gcc_unreachable ();

      for (part = 0; part < parts; part++)
        {
          unsigned int byte;
          for (byte = 0; byte < innersize; byte++)
            {
              bytes[idx++] = (elpart & 0xff) ^ invmask;
              elpart >>= BITS_PER_UNIT;
            }
          if (GET_CODE (el) == CONST_DOUBLE)
            elpart = CONST_DOUBLE_HIGH (el);
        }
    }

  /* Sanity check.  */
  gcc_assert (idx == GET_MODE_SIZE (mode));

  do
    {
      CHECK (4, 32, 0, bytes[i] == bytes[0] && bytes[i + 1] == 0
	     && bytes[i + 2] == 0 && bytes[i + 3] == 0, 0, 0);

      CHECK (4, 32, 1, bytes[i] == 0 && bytes[i + 1] == bytes[1]
	     && bytes[i + 2] == 0 && bytes[i + 3] == 0, 8, 0);

      CHECK (4, 32, 2, bytes[i] == 0 && bytes[i + 1] == 0
	     && bytes[i + 2] == bytes[2] && bytes[i + 3] == 0, 16, 0);

      CHECK (4, 32, 3, bytes[i] == 0 && bytes[i + 1] == 0
	     && bytes[i + 2] == 0 && bytes[i + 3] == bytes[3], 24, 0);

      CHECK (2, 16, 4, bytes[i] == bytes[0] && bytes[i + 1] == 0, 0, 0);

      CHECK (2, 16, 5, bytes[i] == 0 && bytes[i + 1] == bytes[1], 8, 0);

      CHECK (4, 32, 6, bytes[i] == bytes[0] && bytes[i + 1] == 0xff
	     && bytes[i + 2] == 0xff && bytes[i + 3] == 0xff, 0, 1);

      CHECK (4, 32, 7, bytes[i] == 0xff && bytes[i + 1] == bytes[1]
	     && bytes[i + 2] == 0xff && bytes[i + 3] == 0xff, 8, 1);

      CHECK (4, 32, 8, bytes[i] == 0xff && bytes[i + 1] == 0xff
	     && bytes[i + 2] == bytes[2] && bytes[i + 3] == 0xff, 16, 1);

      CHECK (4, 32, 9, bytes[i] == 0xff && bytes[i + 1] == 0xff
	     && bytes[i + 2] == 0xff && bytes[i + 3] == bytes[3], 24, 1);

      CHECK (2, 16, 10, bytes[i] == bytes[0] && bytes[i + 1] == 0xff, 0, 1);

      CHECK (2, 16, 11, bytes[i] == 0xff && bytes[i + 1] == bytes[1], 8, 1);

      CHECK (4, 32, 12, bytes[i] == 0xff && bytes[i + 1] == bytes[1]
	     && bytes[i + 2] == 0 && bytes[i + 3] == 0, 8, 0);

      CHECK (4, 32, 13, bytes[i] == 0 && bytes[i + 1] == bytes[1]
	     && bytes[i + 2] == 0xff && bytes[i + 3] == 0xff, 8, 1);

      CHECK (4, 32, 14, bytes[i] == 0xff && bytes[i + 1] == 0xff
	     && bytes[i + 2] == bytes[2] && bytes[i + 3] == 0, 16, 0);

      CHECK (4, 32, 15, bytes[i] == 0 && bytes[i + 1] == 0
	     && bytes[i + 2] == bytes[2] && bytes[i + 3] == 0xff, 16, 1);

      CHECK (1, 8, 16, bytes[i] == bytes[0], 0, 0);

      CHECK (1, 64, 17, (bytes[i] == 0 || bytes[i] == 0xff)
	     && bytes[i] == bytes[(i + 8) % idx], 0, 0);
    }
  while (0);

  if (immtype == -1)
    return false;

  if (info)
    {
      info->element_width = elsize;
      info->mvn = emvn != 0;
      info->shift = eshift;

      unsigned HOST_WIDE_INT imm = 0;

      if (immtype >= 12 && immtype <= 15)
	info->msl = true;

      /* Un-invert bytes of recognized vector, if necessary.  */
      if (invmask != 0)
        for (i = 0; i < idx; i++)
          bytes[i] ^= invmask;

      if (immtype == 17)
        {
          /* FIXME: Broken on 32-bit H_W_I hosts.  */
          gcc_assert (sizeof (HOST_WIDE_INT) == 8);

          for (i = 0; i < 8; i++)
            imm |= (unsigned HOST_WIDE_INT) (bytes[i] ? 0xff : 0)
	      << (i * BITS_PER_UNIT);


	  info->value = GEN_INT (imm);
	}
      else
	{
	  for (i = 0; i < elsize / BITS_PER_UNIT; i++)
	    imm |= (unsigned HOST_WIDE_INT) bytes[i] << (i * BITS_PER_UNIT);

	  /* Construct 'abcdefgh' because the assembler cannot handle
	     generic constants.	 */
	  if (info->mvn)
	    imm = ~imm;
	  imm = (imm >> info->shift) & 0xff;
	  info->value = GEN_INT (imm);
	}
    }

  return true;
#undef CHECK
}

/* Check of immediate shift constants are within range.  */
bool
aarch64_simd_shift_imm_p (rtx x, machine_mode mode, bool left)
{
  int bit_width = GET_MODE_UNIT_SIZE (mode) * BITS_PER_UNIT;
  if (left)
    return aarch64_const_vec_all_same_in_range_p (x, 0, bit_width - 1);
  else
    return aarch64_const_vec_all_same_in_range_p (x, 1, bit_width);
}

/* Return true if X is a uniform vector where all elements
   are either the floating-point constant 0.0 or the
   integer constant 0.  */
bool
aarch64_simd_imm_zero_p (rtx x, machine_mode mode)
{
  return x == CONST0_RTX (mode);
}

bool
aarch64_simd_imm_scalar_p (rtx x, machine_mode mode ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT imm = INTVAL (x);
  int i;

  for (i = 0; i < 8; i++)
    {
      unsigned int byte = imm & 0xff;
      if (byte != 0xff && byte != 0)
       return false;
      imm >>= 8;
    }

  return true;
}

bool
aarch64_mov_operand_p (rtx x,
		       enum aarch64_symbol_context context,
		       machine_mode mode)
{
  if (GET_CODE (x) == HIGH
      && aarch64_valid_symref (XEXP (x, 0), GET_MODE (XEXP (x, 0))))
    return true;

  if (CONST_INT_P (x))
    return true;

  if (GET_CODE (x) == SYMBOL_REF && mode == DImode && CONSTANT_ADDRESS_P (x))
    return true;

  return aarch64_classify_symbolic_expression (x, context)
    == SYMBOL_TINY_ABSOLUTE;
}

/* Return a const_int vector of VAL.  */
rtx
aarch64_simd_gen_const_vector_dup (machine_mode mode, int val)
{
  int nunits = GET_MODE_NUNITS (mode);
  rtvec v = rtvec_alloc (nunits);
  int i;

  for (i=0; i < nunits; i++)
    RTVEC_ELT (v, i) = GEN_INT (val);

  return gen_rtx_CONST_VECTOR (mode, v);
}

/* Check OP is a legal scalar immediate for the MOVI instruction.  */

bool
aarch64_simd_scalar_immediate_valid_for_move (rtx op, machine_mode mode)
{
  machine_mode vmode;

  gcc_assert (!VECTOR_MODE_P (mode));
  vmode = aarch64_preferred_simd_mode (mode);
  rtx op_v = aarch64_simd_gen_const_vector_dup (vmode, INTVAL (op));
  return aarch64_simd_valid_immediate (op_v, vmode, false, NULL);
}

/* Construct and return a PARALLEL RTX vector with elements numbering the
   lanes of either the high (HIGH == TRUE) or low (HIGH == FALSE) half of
   the vector - from the perspective of the architecture.  This does not
   line up with GCC's perspective on lane numbers, so we end up with
   different masks depending on our target endian-ness.  The diagram
   below may help.  We must draw the distinction when building masks
   which select one half of the vector.  An instruction selecting
   architectural low-lanes for a big-endian target, must be described using
   a mask selecting GCC high-lanes.

                 Big-Endian             Little-Endian

GCC             0   1   2   3           3   2   1   0
              | x | x | x | x |       | x | x | x | x |
Architecture    3   2   1   0           3   2   1   0

Low Mask:         { 2, 3 }                { 0, 1 }
High Mask:        { 0, 1 }                { 2, 3 }
*/

rtx
aarch64_simd_vect_par_cnst_half (machine_mode mode, bool high)
{
  int nunits = GET_MODE_NUNITS (mode);
  rtvec v = rtvec_alloc (nunits / 2);
  int high_base = nunits / 2;
  int low_base = 0;
  int base;
  rtx t1;
  int i;

  if (BYTES_BIG_ENDIAN)
    base = high ? low_base : high_base;
  else
    base = high ? high_base : low_base;

  for (i = 0; i < nunits / 2; i++)
    RTVEC_ELT (v, i) = GEN_INT (base + i);

  t1 = gen_rtx_PARALLEL (mode, v);
  return t1;
}

/* Check OP for validity as a PARALLEL RTX vector with elements
   numbering the lanes of either the high (HIGH == TRUE) or low lanes,
   from the perspective of the architecture.  See the diagram above
   aarch64_simd_vect_par_cnst_half for more details.  */

bool
aarch64_simd_check_vect_par_cnst_half (rtx op, machine_mode mode,
				       bool high)
{
  rtx ideal = aarch64_simd_vect_par_cnst_half (mode, high);
  HOST_WIDE_INT count_op = XVECLEN (op, 0);
  HOST_WIDE_INT count_ideal = XVECLEN (ideal, 0);
  int i = 0;

  if (!VECTOR_MODE_P (mode))
    return false;

  if (count_op != count_ideal)
    return false;

  for (i = 0; i < count_ideal; i++)
    {
      rtx elt_op = XVECEXP (op, 0, i);
      rtx elt_ideal = XVECEXP (ideal, 0, i);

      if (!CONST_INT_P (elt_op)
	  || INTVAL (elt_ideal) != INTVAL (elt_op))
	return false;
    }
  return true;
}

/* Bounds-check lanes.  Ensure OPERAND lies between LOW (inclusive) and
   HIGH (exclusive).  */
void
aarch64_simd_lane_bounds (rtx operand, HOST_WIDE_INT low, HOST_WIDE_INT high,
			  const_tree exp)
{
  HOST_WIDE_INT lane;
  gcc_assert (CONST_INT_P (operand));
  lane = INTVAL (operand);

  if (lane < low || lane >= high)
  {
    if (exp)
      error ("%Klane %ld out of range %ld - %ld", exp, lane, low, high - 1);
    else
      error ("lane %ld out of range %ld - %ld", lane, low, high - 1);
  }
}

/* Emit code to place a AdvSIMD pair result in memory locations (with equal
   registers).  */
void
aarch64_simd_emit_pair_result_insn (machine_mode mode,
			    rtx (*intfn) (rtx, rtx, rtx), rtx destaddr,
                            rtx op1)
{
  rtx mem = gen_rtx_MEM (mode, destaddr);
  rtx tmp1 = gen_reg_rtx (mode);
  rtx tmp2 = gen_reg_rtx (mode);

  emit_insn (intfn (tmp1, op1, tmp2));

  emit_move_insn (mem, tmp1);
  mem = adjust_address (mem, mode, GET_MODE_SIZE (mode));
  emit_move_insn (mem, tmp2);
}

/* Return TRUE if OP is a valid vector addressing mode.  */
bool
aarch64_simd_mem_operand_p (rtx op)
{
  return MEM_P (op) && (GET_CODE (XEXP (op, 0)) == POST_INC
			|| REG_P (XEXP (op, 0)));
}

/* Emit a register copy from operand to operand, taking care not to
   early-clobber source registers in the process.

   COUNT is the number of components into which the copy needs to be
   decomposed.  */
void
aarch64_simd_emit_reg_reg_move (rtx *operands, enum machine_mode mode,
				unsigned int count)
{
  unsigned int i;
  int rdest = REGNO (operands[0]);
  int rsrc = REGNO (operands[1]);

  if (!reg_overlap_mentioned_p (operands[0], operands[1])
      || rdest < rsrc)
    for (i = 0; i < count; i++)
      emit_move_insn (gen_rtx_REG (mode, rdest + i),
		      gen_rtx_REG (mode, rsrc + i));
  else
    for (i = 0; i < count; i++)
      emit_move_insn (gen_rtx_REG (mode, rdest + count - i - 1),
		      gen_rtx_REG (mode, rsrc + count - i - 1));
}

/* Compute and return the length of aarch64_simd_mov<mode>, where <mode> is
   one of VSTRUCT modes: OI, CI or XI.  */
int
aarch64_simd_attr_length_move (rtx_insn *insn)
{
  machine_mode mode;

  extract_insn_cached (insn);

  if (REG_P (recog_data.operand[0]) && REG_P (recog_data.operand[1]))
    {
      mode = GET_MODE (recog_data.operand[0]);
      switch (mode)
	{
	case OImode:
	  return 8;
	case CImode:
	  return 12;
	case XImode:
	  return 16;
	default:
	  gcc_unreachable ();
	}
    }
  return 4;
}

/* Compute and return the length of aarch64_simd_reglist<mode>, where <mode> is
   one of VSTRUCT modes: OI, CI, EI, or XI.  */
int
aarch64_simd_attr_length_rglist (enum machine_mode mode)
{
  return (GET_MODE_SIZE (mode) / UNITS_PER_VREG) * 4;
}

/* Implement target hook TARGET_VECTOR_ALIGNMENT.  The AAPCS64 sets the maximum
   alignment of a vector to 128 bits.  */
static HOST_WIDE_INT
aarch64_simd_vector_alignment (const_tree type)
{
  HOST_WIDE_INT align = tree_to_shwi (TYPE_SIZE (type));
  return MIN (align, 128);
}

/* Implement target hook TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE.  */
static bool
aarch64_simd_vector_alignment_reachable (const_tree type, bool is_packed)
{
  if (is_packed)
    return false;

  /* We guarantee alignment for vectors up to 128-bits.  */
  if (tree_int_cst_compare (TYPE_SIZE (type),
			    bitsize_int (BIGGEST_ALIGNMENT)) > 0)
    return false;

  /* Vectors whose size is <= BIGGEST_ALIGNMENT are naturally aligned.  */
  return true;
}

/* If VALS is a vector constant that can be loaded into a register
   using DUP, generate instructions to do so and return an RTX to
   assign to the register.  Otherwise return NULL_RTX.  */
static rtx
aarch64_simd_dup_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  bool all_same = true;
  rtx x;
  int i;

  if (GET_CODE (vals) != CONST_VECTOR)
    return NULL_RTX;

  for (i = 1; i < n_elts; ++i)
    {
      x = CONST_VECTOR_ELT (vals, i);
      if (!rtx_equal_p (x, CONST_VECTOR_ELT (vals, 0)))
	all_same = false;
    }

  if (!all_same)
    return NULL_RTX;

  /* We can load this constant by using DUP and a constant in a
     single ARM register.  This will be cheaper than a vector
     load.  */
  x = copy_to_mode_reg (inner_mode, CONST_VECTOR_ELT (vals, 0));
  return gen_rtx_VEC_DUPLICATE (mode, x);
}


/* Generate code to load VALS, which is a PARALLEL containing only
   constants (for vec_init) or CONST_VECTOR, efficiently into a
   register.  Returns an RTX to copy into the register, or NULL_RTX
   for a PARALLEL that can not be converted into a CONST_VECTOR.  */
static rtx
aarch64_simd_make_constant (rtx vals)
{
  machine_mode mode = GET_MODE (vals);
  rtx const_dup;
  rtx const_vec = NULL_RTX;
  int n_elts = GET_MODE_NUNITS (mode);
  int n_const = 0;
  int i;

  if (GET_CODE (vals) == CONST_VECTOR)
    const_vec = vals;
  else if (GET_CODE (vals) == PARALLEL)
    {
      /* A CONST_VECTOR must contain only CONST_INTs and
	 CONST_DOUBLEs, but CONSTANT_P allows more (e.g. SYMBOL_REF).
	 Only store valid constants in a CONST_VECTOR.  */
      for (i = 0; i < n_elts; ++i)
	{
	  rtx x = XVECEXP (vals, 0, i);
	  if (CONST_INT_P (x) || CONST_DOUBLE_P (x))
	    n_const++;
	}
      if (n_const == n_elts)
	const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
    }
  else
    gcc_unreachable ();

  if (const_vec != NULL_RTX
      && aarch64_simd_valid_immediate (const_vec, mode, false, NULL))
    /* Load using MOVI/MVNI.  */
    return const_vec;
  else if ((const_dup = aarch64_simd_dup_constant (vals)) != NULL_RTX)
    /* Loaded using DUP.  */
    return const_dup;
  else if (const_vec != NULL_RTX)
    /* Load from constant pool. We can not take advantage of single-cycle
       LD1 because we need a PC-relative addressing mode.  */
    return const_vec;
  else
    /* A PARALLEL containing something not valid inside CONST_VECTOR.
       We can not construct an initializer.  */
    return NULL_RTX;
}

void
aarch64_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0, one_var = -1;
  bool all_same = true;
  rtx x, mem;
  int i;

  x = XVECEXP (vals, 0, 0);
  if (!CONST_INT_P (x) && !CONST_DOUBLE_P (x))
    n_var = 1, one_var = 0;
  
  for (i = 1; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!CONST_INT_P (x) && !CONST_DOUBLE_P (x))
	++n_var, one_var = i;

      if (!rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;
    }

  if (n_var == 0)
    {
      rtx constant = aarch64_simd_make_constant (vals);
      if (constant != NULL_RTX)
	{
	  emit_move_insn (target, constant);
	  return;
	}
    }

  /* Splat a single non-constant element if we can.  */
  if (all_same)
    {
      x = copy_to_mode_reg (inner_mode, XVECEXP (vals, 0, 0));
      aarch64_emit_move (target, gen_rtx_VEC_DUPLICATE (mode, x));
      return;
    }

  /* One field is non-constant.  Load constant then overwrite varying
     field.  This is more efficient than using the stack.  */
  if (n_var == 1)
    {
      rtx copy = copy_rtx (vals);
      rtx index = GEN_INT (one_var);
      enum insn_code icode;

      /* Load constant part of vector, substitute neighboring value for
	 varying element.  */
      XVECEXP (copy, 0, one_var) = XVECEXP (vals, 0, one_var ^ 1);
      aarch64_expand_vector_init (target, copy);

      /* Insert variable.  */
      x = copy_to_mode_reg (inner_mode, XVECEXP (vals, 0, one_var));
      icode = optab_handler (vec_set_optab, mode);
      gcc_assert (icode != CODE_FOR_nothing);
      emit_insn (GEN_FCN (icode) (target, x, index));
      return;
    }

  /* Construct the vector in memory one field at a time
     and load the whole vector.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));
  for (i = 0; i < n_elts; i++)
    emit_move_insn (adjust_address_nv (mem, inner_mode,
				    i * GET_MODE_SIZE (inner_mode)),
		    XVECEXP (vals, 0, i));
  emit_move_insn (target, mem);

}

static unsigned HOST_WIDE_INT
aarch64_shift_truncation_mask (machine_mode mode)
{
  return
    (aarch64_vector_mode_supported_p (mode)
     || aarch64_vect_struct_mode_p (mode)) ? 0 : (GET_MODE_BITSIZE (mode) - 1);
}

#ifndef TLS_SECTION_ASM_FLAG
#define TLS_SECTION_ASM_FLAG 'T'
#endif

void
aarch64_elf_asm_named_section (const char *name, unsigned int flags,
			       tree decl ATTRIBUTE_UNUSED)
{
  char flagchars[10], *f = flagchars;

  /* If we have already declared this section, we can use an
     abbreviated form to switch back to it -- unless this section is
     part of a COMDAT groups, in which case GAS requires the full
     declaration every time.  */
  if (!(HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
      && (flags & SECTION_DECLARED))
    {
      fprintf (asm_out_file, "\t.section\t%s\n", name);
      return;
    }

  if (!(flags & SECTION_DEBUG))
    *f++ = 'a';
  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';
  if (flags & SECTION_SMALL)
    *f++ = 's';
  if (flags & SECTION_MERGE)
    *f++ = 'M';
  if (flags & SECTION_STRINGS)
    *f++ = 'S';
  if (flags & SECTION_TLS)
    *f++ = TLS_SECTION_ASM_FLAG;
  if (HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
    *f++ = 'G';
  *f = '\0';

  fprintf (asm_out_file, "\t.section\t%s,\"%s\"", name, flagchars);

  if (!(flags & SECTION_NOTYPE))
    {
      const char *type;
      const char *format;

      if (flags & SECTION_BSS)
	type = "nobits";
      else
	type = "progbits";

#ifdef TYPE_OPERAND_FMT
      format = "," TYPE_OPERAND_FMT;
#else
      format = ",@%s";
#endif

      fprintf (asm_out_file, format, type);

      if (flags & SECTION_ENTSIZE)
	fprintf (asm_out_file, ",%d", flags & SECTION_ENTSIZE);
      if (HAVE_COMDAT_GROUP && (flags & SECTION_LINKONCE))
	{
	  if (TREE_CODE (decl) == IDENTIFIER_NODE)
	    fprintf (asm_out_file, ",%s,comdat", IDENTIFIER_POINTER (decl));
	  else
	    fprintf (asm_out_file, ",%s,comdat",
		     IDENTIFIER_POINTER (DECL_COMDAT_GROUP (decl)));
	}
    }

  putc ('\n', asm_out_file);
}

/* Select a format to encode pointers in exception handling data.  */
int
aarch64_asm_preferred_eh_data_format (int code ATTRIBUTE_UNUSED, int global)
{
   int type;
   switch (aarch64_cmodel)
     {
     case AARCH64_CMODEL_TINY:
     case AARCH64_CMODEL_TINY_PIC:
     case AARCH64_CMODEL_SMALL:
     case AARCH64_CMODEL_SMALL_PIC:
       /* text+got+data < 4Gb.  4-byte signed relocs are sufficient
	  for everything.  */
       type = DW_EH_PE_sdata4;
       break;
     default:
       /* No assumptions here.  8-byte relocs required.  */
       type = DW_EH_PE_sdata8;
       break;
     }
   return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
}

/* Emit load exclusive.  */

static void
aarch64_emit_load_exclusive (machine_mode mode, rtx rval,
			     rtx mem, rtx model_rtx)
{
  rtx (*gen) (rtx, rtx, rtx);

  switch (mode)
    {
    case QImode: gen = gen_aarch64_load_exclusiveqi; break;
    case HImode: gen = gen_aarch64_load_exclusivehi; break;
    case SImode: gen = gen_aarch64_load_exclusivesi; break;
    case DImode: gen = gen_aarch64_load_exclusivedi; break;
    default:
      gcc_unreachable ();
    }

  emit_insn (gen (rval, mem, model_rtx));
}

/* Emit store exclusive.  */

static void
aarch64_emit_store_exclusive (machine_mode mode, rtx bval,
			      rtx rval, rtx mem, rtx model_rtx)
{
  rtx (*gen) (rtx, rtx, rtx, rtx);

  switch (mode)
    {
    case QImode: gen = gen_aarch64_store_exclusiveqi; break;
    case HImode: gen = gen_aarch64_store_exclusivehi; break;
    case SImode: gen = gen_aarch64_store_exclusivesi; break;
    case DImode: gen = gen_aarch64_store_exclusivedi; break;
    default:
      gcc_unreachable ();
    }

  emit_insn (gen (bval, rval, mem, model_rtx));
}

/* Mark the previous jump instruction as unlikely.  */

static void
aarch64_emit_unlikely_jump (rtx insn)
{
  int very_unlikely = REG_BR_PROB_BASE / 100 - 1;

  insn = emit_jump_insn (insn);
  add_int_reg_note (insn, REG_BR_PROB, very_unlikely);
}

/* Expand a compare and swap pattern.  */

void
aarch64_expand_compare_and_swap (rtx operands[])
{
  rtx bval, rval, mem, oldval, newval, is_weak, mod_s, mod_f, x;
  machine_mode mode, cmp_mode;
  rtx (*gen) (rtx, rtx, rtx, rtx, rtx, rtx, rtx);

  bval = operands[0];
  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);
  cmp_mode = mode;

  /* Normally the succ memory model must be stronger than fail, but in the
     unlikely event of fail being ACQUIRE and succ being RELEASE we need to
     promote succ to ACQ_REL so that we don't lose the acquire semantics.  */

  if (is_mm_acquire (memmodel_from_int (INTVAL (mod_f)))
      && is_mm_release (memmodel_from_int (INTVAL (mod_s))))
    mod_s = GEN_INT (MEMMODEL_ACQ_REL);

  switch (mode)
    {
    case QImode:
    case HImode:
      /* For short modes, we're going to perform the comparison in SImode,
	 so do the zero-extension now.  */
      cmp_mode = SImode;
      rval = gen_reg_rtx (SImode);
      oldval = convert_modes (SImode, mode, oldval, true);
      /* Fall through.  */

    case SImode:
    case DImode:
      /* Force the value into a register if needed.  */
      if (!aarch64_plus_operand (oldval, mode))
	oldval = force_reg (cmp_mode, oldval);
      break;

    default:
      gcc_unreachable ();
    }

  switch (mode)
    {
    case QImode: gen = gen_atomic_compare_and_swapqi_1; break;
    case HImode: gen = gen_atomic_compare_and_swaphi_1; break;
    case SImode: gen = gen_atomic_compare_and_swapsi_1; break;
    case DImode: gen = gen_atomic_compare_and_swapdi_1; break;
    default:
      gcc_unreachable ();
    }

  emit_insn (gen (rval, mem, oldval, newval, is_weak, mod_s, mod_f));

  if (mode == QImode || mode == HImode)
    emit_move_insn (operands[1], gen_lowpart (mode, rval));

  x = gen_rtx_REG (CCmode, CC_REGNUM);
  x = gen_rtx_EQ (SImode, x, const0_rtx);
  emit_insn (gen_rtx_SET (VOIDmode, bval, x));
}

/* Emit a barrier, that is appropriate for memory model MODEL, at the end of a
   sequence implementing an atomic operation.  */

static void
aarch64_emit_post_barrier (enum memmodel model)
{
  const enum memmodel base_model = memmodel_base (model);

  if (is_mm_sync (model)
      && (base_model == MEMMODEL_ACQUIRE
	  || base_model == MEMMODEL_ACQ_REL
	  || base_model == MEMMODEL_SEQ_CST))
    {
      emit_insn (gen_mem_thread_fence (GEN_INT (MEMMODEL_SEQ_CST)));
    }
}

/* Split a compare and swap pattern.  */

void
aarch64_split_compare_and_swap (rtx operands[])
{
  rtx rval, mem, oldval, newval, scratch;
  machine_mode mode;
  bool is_weak;
  rtx_code_label *label1, *label2;
  rtx x, cond;
  enum memmodel model;
  rtx model_rtx;

  rval = operands[0];
  mem = operands[1];
  oldval = operands[2];
  newval = operands[3];
  is_weak = (operands[4] != const0_rtx);
  model_rtx = operands[5];
  scratch = operands[7];
  mode = GET_MODE (mem);
  model = memmodel_from_int (INTVAL (model_rtx));

  label1 = NULL;
  if (!is_weak)
    {
      label1 = gen_label_rtx ();
      emit_label (label1);
    }
  label2 = gen_label_rtx ();

  /* The initial load can be relaxed for a __sync operation since a final
     barrier will be emitted to stop code hoisting.  */
  if (is_mm_sync (model))
    aarch64_emit_load_exclusive (mode, rval, mem,
				 GEN_INT (MEMMODEL_RELAXED));
  else
    aarch64_emit_load_exclusive (mode, rval, mem, model_rtx);

  cond = aarch64_gen_compare_reg (NE, rval, oldval);
  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label2), pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (VOIDmode, pc_rtx, x));

  aarch64_emit_store_exclusive (mode, scratch, mem, newval, model_rtx);

  if (!is_weak)
    {
      x = gen_rtx_NE (VOIDmode, scratch, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, label1), pc_rtx);
      aarch64_emit_unlikely_jump (gen_rtx_SET (VOIDmode, pc_rtx, x));
    }
  else
    {
      cond = gen_rtx_REG (CCmode, CC_REGNUM);
      x = gen_rtx_COMPARE (CCmode, scratch, const0_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, cond, x));
    }

  emit_label (label2);

  /* Emit any final barrier needed for a __sync operation.  */
  if (is_mm_sync (model))
    aarch64_emit_post_barrier (model);
}

/* Split an atomic operation.  */

void
aarch64_split_atomic_op (enum rtx_code code, rtx old_out, rtx new_out, rtx mem,
		     rtx value, rtx model_rtx, rtx cond)
{
  machine_mode mode = GET_MODE (mem);
  machine_mode wmode = (mode == DImode ? DImode : SImode);
  const enum memmodel model = memmodel_from_int (INTVAL (model_rtx));
  const bool is_sync = is_mm_sync (model);
  rtx_code_label *label;
  rtx x;

  label = gen_label_rtx ();
  emit_label (label);

  if (new_out)
    new_out = gen_lowpart (wmode, new_out);
  if (old_out)
    old_out = gen_lowpart (wmode, old_out);
  else
    old_out = new_out;
  value = simplify_gen_subreg (wmode, value, mode, 0);

  /* The initial load can be relaxed for a __sync operation since a final
     barrier will be emitted to stop code hoisting.  */
 if (is_sync)
    aarch64_emit_load_exclusive (mode, old_out, mem,
				 GEN_INT (MEMMODEL_RELAXED));
  else
    aarch64_emit_load_exclusive (mode, old_out, mem, model_rtx);

  switch (code)
    {
    case SET:
      new_out = value;
      break;

    case NOT:
      x = gen_rtx_AND (wmode, old_out, value);
      emit_insn (gen_rtx_SET (VOIDmode, new_out, x));
      x = gen_rtx_NOT (wmode, new_out);
      emit_insn (gen_rtx_SET (VOIDmode, new_out, x));
      break;

    case MINUS:
      if (CONST_INT_P (value))
	{
	  value = GEN_INT (-INTVAL (value));
	  code = PLUS;
	}
      /* Fall through.  */

    default:
      x = gen_rtx_fmt_ee (code, wmode, old_out, value);
      emit_insn (gen_rtx_SET (VOIDmode, new_out, x));
      break;
    }

  aarch64_emit_store_exclusive (mode, cond, mem,
				gen_lowpart (mode, new_out), model_rtx);

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label), pc_rtx);
  aarch64_emit_unlikely_jump (gen_rtx_SET (VOIDmode, pc_rtx, x));

  /* Emit any final barrier needed for a __sync operation.  */
  if (is_sync)
    aarch64_emit_post_barrier (model);
}

static void
aarch64_print_extension (void)
{
  const struct aarch64_option_extension *opt = NULL;

  for (opt = all_extensions; opt->name != NULL; opt++)
    if ((aarch64_isa_flags & opt->flags_on) == opt->flags_on)
      asm_fprintf (asm_out_file, "+%s", opt->name);

  asm_fprintf (asm_out_file, "\n");
}

static void
aarch64_start_file (void)
{
  if (selected_arch)
    {
      asm_fprintf (asm_out_file, "\t.arch %s", selected_arch->name);
      aarch64_print_extension ();
    }
  else if (selected_cpu)
    {
      const char *truncated_name
	    = aarch64_rewrite_selected_cpu (selected_cpu->name);
      asm_fprintf (asm_out_file, "\t.cpu %s", truncated_name);
      aarch64_print_extension ();
    }
  default_file_start();
}

/* Target hook for c_mode_for_suffix.  */
static machine_mode
aarch64_c_mode_for_suffix (char suffix)
{
  if (suffix == 'q')
    return TFmode;

  return VOIDmode;
}

/* We can only represent floating point constants which will fit in
   "quarter-precision" values.  These values are characterised by
   a sign bit, a 4-bit mantissa and a 3-bit exponent.  And are given
   by:

   (-1)^s * (n/16) * 2^r

   Where:
     's' is the sign bit.
     'n' is an integer in the range 16 <= n <= 31.
     'r' is an integer in the range -3 <= r <= 4.  */

/* Return true iff X can be represented by a quarter-precision
   floating point immediate operand X.  Note, we cannot represent 0.0.  */
bool
aarch64_float_const_representable_p (rtx x)
{
  /* This represents our current view of how many bits
     make up the mantissa.  */
  int point_pos = 2 * HOST_BITS_PER_WIDE_INT - 1;
  int exponent;
  unsigned HOST_WIDE_INT mantissa, mask;
  REAL_VALUE_TYPE r, m;
  bool fail;

  if (!CONST_DOUBLE_P (x))
    return false;

  if (GET_MODE (x) == VOIDmode)
    return false;

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);

  /* We cannot represent infinities, NaNs or +/-zero.  We won't
     know if we have +zero until we analyse the mantissa, but we
     can reject the other invalid values.  */
  if (REAL_VALUE_ISINF (r) || REAL_VALUE_ISNAN (r)
      || REAL_VALUE_MINUS_ZERO (r))
    return false;

  /* Extract exponent.  */
  r = real_value_abs (&r);
  exponent = REAL_EXP (&r);

  /* For the mantissa, we expand into two HOST_WIDE_INTS, apart from the
     highest (sign) bit, with a fixed binary point at bit point_pos.
     m1 holds the low part of the mantissa, m2 the high part.
     WARNING: If we ever have a representation using more than 2 * H_W_I - 1
     bits for the mantissa, this can fail (low bits will be lost).  */
  real_ldexp (&m, &r, point_pos - exponent);
  wide_int w = real_to_integer (&m, &fail, HOST_BITS_PER_WIDE_INT * 2);

  /* If the low part of the mantissa has bits set we cannot represent
     the value.  */
  if (w.elt (0) != 0)
    return false;
  /* We have rejected the lower HOST_WIDE_INT, so update our
     understanding of how many bits lie in the mantissa and
     look only at the high HOST_WIDE_INT.  */
  mantissa = w.elt (1);
  point_pos -= HOST_BITS_PER_WIDE_INT;

  /* We can only represent values with a mantissa of the form 1.xxxx.  */
  mask = ((unsigned HOST_WIDE_INT)1 << (point_pos - 5)) - 1;
  if ((mantissa & mask) != 0)
    return false;

  /* Having filtered unrepresentable values, we may now remove all
     but the highest 5 bits.  */
  mantissa >>= point_pos - 5;

  /* We cannot represent the value 0.0, so reject it.  This is handled
     elsewhere.  */
  if (mantissa == 0)
    return false;

  /* Then, as bit 4 is always set, we can mask it off, leaving
     the mantissa in the range [0, 15].  */
  mantissa &= ~(1 << 4);
  gcc_assert (mantissa <= 15);

  /* GCC internally does not use IEEE754-like encoding (where normalized
     significands are in the range [1, 2).  GCC uses [0.5, 1) (see real.c).
     Our mantissa values are shifted 4 places to the left relative to
     normalized IEEE754 so we must modify the exponent returned by REAL_EXP
     by 5 places to correct for GCC's representation.  */
  exponent = 5 - exponent;

  return (exponent >= 0 && exponent <= 7);
}

char*
aarch64_output_simd_mov_immediate (rtx const_vector,
				   machine_mode mode,
				   unsigned width)
{
  bool is_valid;
  static char templ[40];
  const char *mnemonic;
  const char *shift_op;
  unsigned int lane_count = 0;
  char element_char;

  struct simd_immediate_info info = { NULL_RTX, 0, 0, false, false };

  /* This will return true to show const_vector is legal for use as either
     a AdvSIMD MOVI instruction (or, implicitly, MVNI) immediate.  It will
     also update INFO to show how the immediate should be generated.  */
  is_valid = aarch64_simd_valid_immediate (const_vector, mode, false, &info);
  gcc_assert (is_valid);

  element_char = sizetochar (info.element_width);
  lane_count = width / info.element_width;

  mode = GET_MODE_INNER (mode);
  if (mode == SFmode || mode == DFmode)
    {
      gcc_assert (info.shift == 0 && ! info.mvn);
      if (aarch64_float_const_zero_rtx_p (info.value))
        info.value = GEN_INT (0);
      else
	{
#define buf_size 20
	  REAL_VALUE_TYPE r;
	  REAL_VALUE_FROM_CONST_DOUBLE (r, info.value);
	  char float_buf[buf_size] = {'\0'};
	  real_to_decimal_for_mode (float_buf, &r, buf_size, buf_size, 1, mode);
#undef buf_size

	  if (lane_count == 1)
	    snprintf (templ, sizeof (templ), "fmov\t%%d0, %s", float_buf);
	  else
	    snprintf (templ, sizeof (templ), "fmov\t%%0.%d%c, %s",
		      lane_count, element_char, float_buf);
	  return templ;
	}
    }

  mnemonic = info.mvn ? "mvni" : "movi";
  shift_op = info.msl ? "msl" : "lsl";

  if (lane_count == 1)
    snprintf (templ, sizeof (templ), "%s\t%%d0, " HOST_WIDE_INT_PRINT_HEX,
	      mnemonic, UINTVAL (info.value));
  else if (info.shift)
    snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, " HOST_WIDE_INT_PRINT_HEX
	      ", %s %d", mnemonic, lane_count, element_char,
	      UINTVAL (info.value), shift_op, info.shift);
  else
    snprintf (templ, sizeof (templ), "%s\t%%0.%d%c, " HOST_WIDE_INT_PRINT_HEX,
	      mnemonic, lane_count, element_char, UINTVAL (info.value));
  return templ;
}

char*
aarch64_output_scalar_simd_mov_immediate (rtx immediate,
					  machine_mode mode)
{
  machine_mode vmode;

  gcc_assert (!VECTOR_MODE_P (mode));
  vmode = aarch64_simd_container_mode (mode, 64);
  rtx v_op = aarch64_simd_gen_const_vector_dup (vmode, INTVAL (immediate));
  return aarch64_output_simd_mov_immediate (v_op, vmode, 64);
}

/* Split operands into moves from op[1] + op[2] into op[0].  */

void
aarch64_split_combinev16qi (rtx operands[3])
{
  unsigned int dest = REGNO (operands[0]);
  unsigned int src1 = REGNO (operands[1]);
  unsigned int src2 = REGNO (operands[2]);
  machine_mode halfmode = GET_MODE (operands[1]);
  unsigned int halfregs = HARD_REGNO_NREGS (src1, halfmode);
  rtx destlo, desthi;

  gcc_assert (halfmode == V16QImode);

  if (src1 == dest && src2 == dest + halfregs)
    {
      /* No-op move.  Can't split to nothing; emit something.  */
      emit_note (NOTE_INSN_DELETED);
      return;
    }

  /* Preserve register attributes for variable tracking.  */
  destlo = gen_rtx_REG_offset (operands[0], halfmode, dest, 0);
  desthi = gen_rtx_REG_offset (operands[0], halfmode, dest + halfregs,
			       GET_MODE_SIZE (halfmode));

  /* Special case of reversed high/low parts.  */
  if (reg_overlap_mentioned_p (operands[2], destlo)
      && reg_overlap_mentioned_p (operands[1], desthi))
    {
      emit_insn (gen_xorv16qi3 (operands[1], operands[1], operands[2]));
      emit_insn (gen_xorv16qi3 (operands[2], operands[1], operands[2]));
      emit_insn (gen_xorv16qi3 (operands[1], operands[1], operands[2]));
    }
  else if (!reg_overlap_mentioned_p (operands[2], destlo))
    {
      /* Try to avoid unnecessary moves if part of the result
	 is in the right place already.  */
      if (src1 != dest)
	emit_move_insn (destlo, operands[1]);
      if (src2 != dest + halfregs)
	emit_move_insn (desthi, operands[2]);
    }
  else
    {
      if (src2 != dest + halfregs)
	emit_move_insn (desthi, operands[2]);
      if (src1 != dest)
	emit_move_insn (destlo, operands[1]);
    }
}

/* vec_perm support.  */

#define MAX_VECT_LEN 16

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  unsigned char perm[MAX_VECT_LEN];
  machine_mode vmode;
  unsigned char nelt;
  bool one_vector_p;
  bool testing_p;
};

/* Generate a variable permutation.  */

static void
aarch64_expand_vec_perm_1 (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  bool one_vector_p = rtx_equal_p (op0, op1);

  gcc_checking_assert (vmode == V8QImode || vmode == V16QImode);
  gcc_checking_assert (GET_MODE (op0) == vmode);
  gcc_checking_assert (GET_MODE (op1) == vmode);
  gcc_checking_assert (GET_MODE (sel) == vmode);
  gcc_checking_assert (TARGET_SIMD);

  if (one_vector_p)
    {
      if (vmode == V8QImode)
	{
	  /* Expand the argument to a V16QI mode by duplicating it.  */
	  rtx pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_aarch64_combinev8qi (pair, op0, op0));
	  emit_insn (gen_aarch64_tbl1v8qi (target, pair, sel));
	}
      else
	{
	  emit_insn (gen_aarch64_tbl1v16qi (target, op0, sel));
	}
    }
  else
    {
      rtx pair;

      if (vmode == V8QImode)
	{
	  pair = gen_reg_rtx (V16QImode);
	  emit_insn (gen_aarch64_combinev8qi (pair, op0, op1));
	  emit_insn (gen_aarch64_tbl1v8qi (target, pair, sel));
	}
      else
	{
	  pair = gen_reg_rtx (OImode);
	  emit_insn (gen_aarch64_combinev16qi (pair, op0, op1));
	  emit_insn (gen_aarch64_tbl2v16qi (target, pair, sel));
	}
    }
}

void
aarch64_expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode vmode = GET_MODE (target);
  unsigned int nelt = GET_MODE_NUNITS (vmode);
  bool one_vector_p = rtx_equal_p (op0, op1);
  rtx mask;

  /* The TBL instruction does not use a modulo index, so we must take care
     of that ourselves.  */
  mask = aarch64_simd_gen_const_vector_dup (vmode,
      one_vector_p ? nelt - 1 : 2 * nelt - 1);
  sel = expand_simple_binop (vmode, AND, sel, mask, NULL, 0, OPTAB_LIB_WIDEN);

  /* For big-endian, we also need to reverse the index within the vector
     (but not which vector).  */
  if (BYTES_BIG_ENDIAN)
    {
      /* If one_vector_p, mask is a vector of (nelt - 1)'s already.  */
      if (!one_vector_p)
        mask = aarch64_simd_gen_const_vector_dup (vmode, nelt - 1);
      sel = expand_simple_binop (vmode, XOR, sel, mask,
				 NULL, 0, OPTAB_LIB_WIDEN);
    }
  aarch64_expand_vec_perm_1 (target, op0, op1, sel);
}

/* Recognize patterns suitable for the TRN instructions.  */
static bool
aarch64_evpc_trn (struct expand_vec_perm_d *d)
{
  unsigned int i, odd, mask, nelt = d->nelt;
  rtx out, in0, in1, x;
  rtx (*gen) (rtx, rtx, rtx);
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  if (d->perm[0] == 0)
    odd = 0;
  else if (d->perm[0] == 1)
    odd = 1;
  else
    return false;
  mask = (d->one_vector_p ? nelt - 1 : 2 * nelt - 1);

  for (i = 0; i < nelt; i += 2)
    {
      if (d->perm[i] != i + odd)
	return false;
      if (d->perm[i + 1] != ((i + nelt + odd) & mask))
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  if (BYTES_BIG_ENDIAN)
    {
      x = in0, in0 = in1, in1 = x;
      odd = !odd;
    }
  out = d->target;

  if (odd)
    {
      switch (vmode)
	{
	case V16QImode: gen = gen_aarch64_trn2v16qi; break;
	case V8QImode: gen = gen_aarch64_trn2v8qi; break;
	case V8HImode: gen = gen_aarch64_trn2v8hi; break;
	case V4HImode: gen = gen_aarch64_trn2v4hi; break;
	case V4SImode: gen = gen_aarch64_trn2v4si; break;
	case V2SImode: gen = gen_aarch64_trn2v2si; break;
	case V2DImode: gen = gen_aarch64_trn2v2di; break;
	case V4SFmode: gen = gen_aarch64_trn2v4sf; break;
	case V2SFmode: gen = gen_aarch64_trn2v2sf; break;
	case V2DFmode: gen = gen_aarch64_trn2v2df; break;
	default:
	  return false;
	}
    }
  else
    {
      switch (vmode)
	{
	case V16QImode: gen = gen_aarch64_trn1v16qi; break;
	case V8QImode: gen = gen_aarch64_trn1v8qi; break;
	case V8HImode: gen = gen_aarch64_trn1v8hi; break;
	case V4HImode: gen = gen_aarch64_trn1v4hi; break;
	case V4SImode: gen = gen_aarch64_trn1v4si; break;
	case V2SImode: gen = gen_aarch64_trn1v2si; break;
	case V2DImode: gen = gen_aarch64_trn1v2di; break;
	case V4SFmode: gen = gen_aarch64_trn1v4sf; break;
	case V2SFmode: gen = gen_aarch64_trn1v2sf; break;
	case V2DFmode: gen = gen_aarch64_trn1v2df; break;
	default:
	  return false;
	}
    }

  emit_insn (gen (out, in0, in1));
  return true;
}

/* Recognize patterns suitable for the UZP instructions.  */
static bool
aarch64_evpc_uzp (struct expand_vec_perm_d *d)
{
  unsigned int i, odd, mask, nelt = d->nelt;
  rtx out, in0, in1, x;
  rtx (*gen) (rtx, rtx, rtx);
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  if (d->perm[0] == 0)
    odd = 0;
  else if (d->perm[0] == 1)
    odd = 1;
  else
    return false;
  mask = (d->one_vector_p ? nelt - 1 : 2 * nelt - 1);

  for (i = 0; i < nelt; i++)
    {
      unsigned elt = (i * 2 + odd) & mask;
      if (d->perm[i] != elt)
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  if (BYTES_BIG_ENDIAN)
    {
      x = in0, in0 = in1, in1 = x;
      odd = !odd;
    }
  out = d->target;

  if (odd)
    {
      switch (vmode)
	{
	case V16QImode: gen = gen_aarch64_uzp2v16qi; break;
	case V8QImode: gen = gen_aarch64_uzp2v8qi; break;
	case V8HImode: gen = gen_aarch64_uzp2v8hi; break;
	case V4HImode: gen = gen_aarch64_uzp2v4hi; break;
	case V4SImode: gen = gen_aarch64_uzp2v4si; break;
	case V2SImode: gen = gen_aarch64_uzp2v2si; break;
	case V2DImode: gen = gen_aarch64_uzp2v2di; break;
	case V4SFmode: gen = gen_aarch64_uzp2v4sf; break;
	case V2SFmode: gen = gen_aarch64_uzp2v2sf; break;
	case V2DFmode: gen = gen_aarch64_uzp2v2df; break;
	default:
	  return false;
	}
    }
  else
    {
      switch (vmode)
	{
	case V16QImode: gen = gen_aarch64_uzp1v16qi; break;
	case V8QImode: gen = gen_aarch64_uzp1v8qi; break;
	case V8HImode: gen = gen_aarch64_uzp1v8hi; break;
	case V4HImode: gen = gen_aarch64_uzp1v4hi; break;
	case V4SImode: gen = gen_aarch64_uzp1v4si; break;
	case V2SImode: gen = gen_aarch64_uzp1v2si; break;
	case V2DImode: gen = gen_aarch64_uzp1v2di; break;
	case V4SFmode: gen = gen_aarch64_uzp1v4sf; break;
	case V2SFmode: gen = gen_aarch64_uzp1v2sf; break;
	case V2DFmode: gen = gen_aarch64_uzp1v2df; break;
	default:
	  return false;
	}
    }

  emit_insn (gen (out, in0, in1));
  return true;
}

/* Recognize patterns suitable for the ZIP instructions.  */
static bool
aarch64_evpc_zip (struct expand_vec_perm_d *d)
{
  unsigned int i, high, mask, nelt = d->nelt;
  rtx out, in0, in1, x;
  rtx (*gen) (rtx, rtx, rtx);
  machine_mode vmode = d->vmode;

  if (GET_MODE_UNIT_SIZE (vmode) > 8)
    return false;

  /* Note that these are little-endian tests.
     We correct for big-endian later.  */
  high = nelt / 2;
  if (d->perm[0] == high)
    /* Do Nothing.  */
    ;
  else if (d->perm[0] == 0)
    high = 0;
  else
    return false;
  mask = (d->one_vector_p ? nelt - 1 : 2 * nelt - 1);

  for (i = 0; i < nelt / 2; i++)
    {
      unsigned elt = (i + high) & mask;
      if (d->perm[i * 2] != elt)
	return false;
      elt = (elt + nelt) & mask;
      if (d->perm[i * 2 + 1] != elt)
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  if (BYTES_BIG_ENDIAN)
    {
      x = in0, in0 = in1, in1 = x;
      high = !high;
    }
  out = d->target;

  if (high)
    {
      switch (vmode)
	{
	case V16QImode: gen = gen_aarch64_zip2v16qi; break;
	case V8QImode: gen = gen_aarch64_zip2v8qi; break;
	case V8HImode: gen = gen_aarch64_zip2v8hi; break;
	case V4HImode: gen = gen_aarch64_zip2v4hi; break;
	case V4SImode: gen = gen_aarch64_zip2v4si; break;
	case V2SImode: gen = gen_aarch64_zip2v2si; break;
	case V2DImode: gen = gen_aarch64_zip2v2di; break;
	case V4SFmode: gen = gen_aarch64_zip2v4sf; break;
	case V2SFmode: gen = gen_aarch64_zip2v2sf; break;
	case V2DFmode: gen = gen_aarch64_zip2v2df; break;
	default:
	  return false;
	}
    }
  else
    {
      switch (vmode)
	{
	case V16QImode: gen = gen_aarch64_zip1v16qi; break;
	case V8QImode: gen = gen_aarch64_zip1v8qi; break;
	case V8HImode: gen = gen_aarch64_zip1v8hi; break;
	case V4HImode: gen = gen_aarch64_zip1v4hi; break;
	case V4SImode: gen = gen_aarch64_zip1v4si; break;
	case V2SImode: gen = gen_aarch64_zip1v2si; break;
	case V2DImode: gen = gen_aarch64_zip1v2di; break;
	case V4SFmode: gen = gen_aarch64_zip1v4sf; break;
	case V2SFmode: gen = gen_aarch64_zip1v2sf; break;
	case V2DFmode: gen = gen_aarch64_zip1v2df; break;
	default:
	  return false;
	}
    }

  emit_insn (gen (out, in0, in1));
  return true;
}

/* Recognize patterns for the EXT insn.  */

static bool
aarch64_evpc_ext (struct expand_vec_perm_d *d)
{
  unsigned int i, nelt = d->nelt;
  rtx (*gen) (rtx, rtx, rtx, rtx);
  rtx offset;

  unsigned int location = d->perm[0]; /* Always < nelt.  */

  /* Check if the extracted indices are increasing by one.  */
  for (i = 1; i < nelt; i++)
    {
      unsigned int required = location + i;
      if (d->one_vector_p)
        {
          /* We'll pass the same vector in twice, so allow indices to wrap.  */
	  required &= (nelt - 1);
	}
      if (d->perm[i] != required)
        return false;
    }

  switch (d->vmode)
    {
    case V16QImode: gen = gen_aarch64_extv16qi; break;
    case V8QImode: gen = gen_aarch64_extv8qi; break;
    case V4HImode: gen = gen_aarch64_extv4hi; break;
    case V8HImode: gen = gen_aarch64_extv8hi; break;
    case V2SImode: gen = gen_aarch64_extv2si; break;
    case V4SImode: gen = gen_aarch64_extv4si; break;
    case V2SFmode: gen = gen_aarch64_extv2sf; break;
    case V4SFmode: gen = gen_aarch64_extv4sf; break;
    case V2DImode: gen = gen_aarch64_extv2di; break;
    case V2DFmode: gen = gen_aarch64_extv2df; break;
    default:
      return false;
    }

  /* Success! */
  if (d->testing_p)
    return true;

  /* The case where (location == 0) is a no-op for both big- and little-endian,
     and is removed by the mid-end at optimization levels -O1 and higher.  */

  if (BYTES_BIG_ENDIAN && (location != 0))
    {
      /* After setup, we want the high elements of the first vector (stored
         at the LSB end of the register), and the low elements of the second
         vector (stored at the MSB end of the register). So swap.  */
      std::swap (d->op0, d->op1);
      /* location != 0 (above), so safe to assume (nelt - location) < nelt.  */
      location = nelt - location;
    }

  offset = GEN_INT (location);
  emit_insn (gen (d->target, d->op0, d->op1, offset));
  return true;
}

/* Recognize patterns for the REV insns.  */

static bool
aarch64_evpc_rev (struct expand_vec_perm_d *d)
{
  unsigned int i, j, diff, nelt = d->nelt;
  rtx (*gen) (rtx, rtx);

  if (!d->one_vector_p)
    return false;

  diff = d->perm[0];
  switch (diff)
    {
    case 7:
      switch (d->vmode)
	{
	case V16QImode: gen = gen_aarch64_rev64v16qi; break;
	case V8QImode: gen = gen_aarch64_rev64v8qi;  break;
	default:
	  return false;
	}
      break;
    case 3:
      switch (d->vmode)
	{
	case V16QImode: gen = gen_aarch64_rev32v16qi; break;
	case V8QImode: gen = gen_aarch64_rev32v8qi;  break;
	case V8HImode: gen = gen_aarch64_rev64v8hi;  break;
	case V4HImode: gen = gen_aarch64_rev64v4hi;  break;
	default:
	  return false;
	}
      break;
    case 1:
      switch (d->vmode)
	{
	case V16QImode: gen = gen_aarch64_rev16v16qi; break;
	case V8QImode: gen = gen_aarch64_rev16v8qi;  break;
	case V8HImode: gen = gen_aarch64_rev32v8hi;  break;
	case V4HImode: gen = gen_aarch64_rev32v4hi;  break;
	case V4SImode: gen = gen_aarch64_rev64v4si;  break;
	case V2SImode: gen = gen_aarch64_rev64v2si;  break;
	case V4SFmode: gen = gen_aarch64_rev64v4sf;  break;
	case V2SFmode: gen = gen_aarch64_rev64v2sf;  break;
	default:
	  return false;
	}
      break;
    default:
      return false;
    }

  for (i = 0; i < nelt ; i += diff + 1)
    for (j = 0; j <= diff; j += 1)
      {
	/* This is guaranteed to be true as the value of diff
	   is 7, 3, 1 and we should have enough elements in the
	   queue to generate this.  Getting a vector mask with a
	   value of diff other than these values implies that
	   something is wrong by the time we get here.  */
	gcc_assert (i + j < nelt);
	if (d->perm[i + j] != i + diff - j)
	  return false;
      }

  /* Success! */
  if (d->testing_p)
    return true;

  emit_insn (gen (d->target, d->op0));
  return true;
}

static bool
aarch64_evpc_dup (struct expand_vec_perm_d *d)
{
  rtx (*gen) (rtx, rtx, rtx);
  rtx out = d->target;
  rtx in0;
  machine_mode vmode = d->vmode;
  unsigned int i, elt, nelt = d->nelt;
  rtx lane;

  elt = d->perm[0];
  for (i = 1; i < nelt; i++)
    {
      if (elt != d->perm[i])
	return false;
    }

  /* The generic preparation in aarch64_expand_vec_perm_const_1
     swaps the operand order and the permute indices if it finds
     d->perm[0] to be in the second operand.  Thus, we can always
     use d->op0 and need not do any extra arithmetic to get the
     correct lane number.  */
  in0 = d->op0;
  lane = GEN_INT (elt); /* The pattern corrects for big-endian.  */

  switch (vmode)
    {
    case V16QImode: gen = gen_aarch64_dup_lanev16qi; break;
    case V8QImode: gen = gen_aarch64_dup_lanev8qi; break;
    case V8HImode: gen = gen_aarch64_dup_lanev8hi; break;
    case V4HImode: gen = gen_aarch64_dup_lanev4hi; break;
    case V4SImode: gen = gen_aarch64_dup_lanev4si; break;
    case V2SImode: gen = gen_aarch64_dup_lanev2si; break;
    case V2DImode: gen = gen_aarch64_dup_lanev2di; break;
    case V4SFmode: gen = gen_aarch64_dup_lanev4sf; break;
    case V2SFmode: gen = gen_aarch64_dup_lanev2sf; break;
    case V2DFmode: gen = gen_aarch64_dup_lanev2df; break;
    default:
      return false;
    }

  emit_insn (gen (out, in0, lane));
  return true;
}

static bool
aarch64_evpc_tbl (struct expand_vec_perm_d *d)
{
  rtx rperm[MAX_VECT_LEN], sel;
  machine_mode vmode = d->vmode;
  unsigned int i, nelt = d->nelt;

  if (d->testing_p)
    return true;

  /* Generic code will try constant permutation twice.  Once with the
     original mode and again with the elements lowered to QImode.
     So wait and don't do the selector expansion ourselves.  */
  if (vmode != V8QImode && vmode != V16QImode)
    return false;

  for (i = 0; i < nelt; ++i)
    {
      int nunits = GET_MODE_NUNITS (vmode);

      /* If big-endian and two vectors we end up with a weird mixed-endian
	 mode on NEON.  Reverse the index within each word but not the word
	 itself.  */
      rperm[i] = GEN_INT (BYTES_BIG_ENDIAN ? d->perm[i] ^ (nunits - 1)
					   : d->perm[i]);
    }
  sel = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (nelt, rperm));
  sel = force_reg (vmode, sel);

  aarch64_expand_vec_perm_1 (d->target, d->op0, d->op1, sel);
  return true;
}

static bool
aarch64_expand_vec_perm_const_1 (struct expand_vec_perm_d *d)
{
  /* The pattern matching functions above are written to look for a small
     number to begin the sequence (0, 1, N/2).  If we begin with an index
     from the second operand, we can swap the operands.  */
  if (d->perm[0] >= d->nelt)
    {
      unsigned i, nelt = d->nelt;

      gcc_assert (nelt == (nelt & -nelt));
      for (i = 0; i < nelt; ++i)
	d->perm[i] ^= nelt; /* Keep the same index, but in the other vector.  */

      std::swap (d->op0, d->op1);
    }

  if (TARGET_SIMD)
    {
      if (aarch64_evpc_rev (d))
	return true;
      else if (aarch64_evpc_ext (d))
	return true;
      else if (aarch64_evpc_dup (d))
	return true;
      else if (aarch64_evpc_zip (d))
	return true;
      else if (aarch64_evpc_uzp (d))
	return true;
      else if (aarch64_evpc_trn (d))
	return true;
      return aarch64_evpc_tbl (d);
    }
  return false;
}

/* Expand a vec_perm_const pattern.  */

bool
aarch64_expand_vec_perm_const (rtx target, rtx op0, rtx op1, rtx sel)
{
  struct expand_vec_perm_d d;
  int i, nelt, which;

  d.target = target;
  d.op0 = op0;
  d.op1 = op1;

  d.vmode = GET_MODE (target);
  gcc_assert (VECTOR_MODE_P (d.vmode));
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.testing_p = false;

  for (i = which = 0; i < nelt; ++i)
    {
      rtx e = XVECEXP (sel, 0, i);
      int ei = INTVAL (e) & (2 * nelt - 1);
      which |= (ei < nelt ? 1 : 2);
      d.perm[i] = ei;
    }

  switch (which)
    {
    default:
      gcc_unreachable ();

    case 3:
      d.one_vector_p = false;
      if (!rtx_equal_p (op0, op1))
	break;

      /* The elements of PERM do not suggest that only the first operand
	 is used, but both operands are identical.  Allow easier matching
	 of the permutation by folding the permutation into the single
	 input vector.  */
      /* Fall Through.  */
    case 2:
      for (i = 0; i < nelt; ++i)
	d.perm[i] &= nelt - 1;
      d.op0 = op1;
      d.one_vector_p = true;
      break;

    case 1:
      d.op1 = op0;
      d.one_vector_p = true;
      break;
    }

  return aarch64_expand_vec_perm_const_1 (&d);
}

static bool
aarch64_vectorize_vec_perm_const_ok (machine_mode vmode,
				     const unsigned char *sel)
{
  struct expand_vec_perm_d d;
  unsigned int i, nelt, which;
  bool ret;

  d.vmode = vmode;
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.testing_p = true;
  memcpy (d.perm, sel, nelt);

  /* Calculate whether all elements are in one vector.  */
  for (i = which = 0; i < nelt; ++i)
    {
      unsigned char e = d.perm[i];
      gcc_assert (e < 2 * nelt);
      which |= (e < nelt ? 1 : 2);
    }

  /* If all elements are from the second vector, reindex as if from the
     first vector.  */
  if (which == 2)
    for (i = 0; i < nelt; ++i)
      d.perm[i] -= nelt;

  /* Check whether the mask can be applied to a single vector.  */
  d.one_vector_p = (which != 3);

  d.target = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 1);
  d.op1 = d.op0 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 2);
  if (!d.one_vector_p)
    d.op1 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 3);

  start_sequence ();
  ret = aarch64_expand_vec_perm_const_1 (&d);
  end_sequence ();

  return ret;
}

/* Implement target hook CANNOT_CHANGE_MODE_CLASS.  */
bool
aarch64_cannot_change_mode_class (machine_mode from,
				  machine_mode to,
				  enum reg_class rclass)
{
  /* We cannot allow word_mode subregs of full vector modes.
     Otherwise the middle-end will assume it's ok to store to
     (subreg:DI (reg:TI 100) 0) in order to modify only the low 64 bits
     of the 128-bit register.  However, after reload the subreg will
     be dropped leaving a plain DImode store.  See PR67609 for a more
     detailed dicussion.  In all other cases, we want to be permissive
     and return false.  */
  return (reg_classes_intersect_p (FP_REGS, rclass)
	  && GET_MODE_SIZE (to) == UNITS_PER_WORD
	  && GET_MODE_SIZE (from) > UNITS_PER_WORD);
}

rtx
aarch64_reverse_mask (enum machine_mode mode)
{
  /* We have to reverse each vector because we dont have
     a permuted load that can reverse-load according to ABI rules.  */
  rtx mask;
  rtvec v = rtvec_alloc (16);
  int i, j;
  int nunits = GET_MODE_NUNITS (mode);
  int usize = GET_MODE_UNIT_SIZE (mode);

  gcc_assert (BYTES_BIG_ENDIAN);
  gcc_assert (AARCH64_VALID_SIMD_QREG_MODE (mode));

  for (i = 0; i < nunits; i++)
    for (j = 0; j < usize; j++)
      RTVEC_ELT (v, i * usize + j) = GEN_INT ((i + 1) * usize - 1 - j);
  mask = gen_rtx_CONST_VECTOR (V16QImode, v);
  return force_reg (V16QImode, mask);
}

/* Implement MODES_TIEABLE_P.  */

bool
aarch64_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (GET_MODE_CLASS (mode1) == GET_MODE_CLASS (mode2))
    return true;

  /* We specifically want to allow elements of "structure" modes to
     be tieable to the structure.  This more general condition allows
     other rarer situations too.  */
  if (TARGET_SIMD
      && aarch64_vector_mode_p (mode1)
      && aarch64_vector_mode_p (mode2))
    return true;

  return false;
}

/* Return a new RTX holding the result of moving POINTER forward by
   AMOUNT bytes.  */

static rtx
aarch64_move_pointer (rtx pointer, int amount)
{
  rtx next = plus_constant (Pmode, XEXP (pointer, 0), amount);

  return adjust_automodify_address (pointer, GET_MODE (pointer),
				    next, amount);
}

/* Return a new RTX holding the result of moving POINTER forward by the
   size of the mode it points to.  */

static rtx
aarch64_progress_pointer (rtx pointer)
{
  HOST_WIDE_INT amount = GET_MODE_SIZE (GET_MODE (pointer));

  return aarch64_move_pointer (pointer, amount);
}

/* Copy one MODE sized block from SRC to DST, then progress SRC and DST by
   MODE bytes.  */

static void
aarch64_copy_one_block_and_progress_pointers (rtx *src, rtx *dst,
					      machine_mode mode)
{
  rtx reg = gen_reg_rtx (mode);

  /* "Cast" the pointers to the correct mode.  */
  *src = adjust_address (*src, mode, 0);
  *dst = adjust_address (*dst, mode, 0);
  /* Emit the memcpy.  */
  emit_move_insn (reg, *src);
  emit_move_insn (*dst, reg);
  /* Move the pointers forward.  */
  *src = aarch64_progress_pointer (*src);
  *dst = aarch64_progress_pointer (*dst);
}

/* Expand movmem, as if from a __builtin_memcpy.  Return true if
   we succeed, otherwise return false.  */

bool
aarch64_expand_movmem (rtx *operands)
{
  unsigned int n;
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx base;
  bool speed_p = !optimize_function_for_size_p (cfun);

  /* When optimizing for size, give a better estimate of the length of a
     memcpy call, but use the default otherwise.  */
  unsigned int max_instructions = (speed_p ? 15 : AARCH64_CALL_RATIO) / 2;

  /* We can't do anything smart if the amount to copy is not constant.  */
  if (!CONST_INT_P (operands[2]))
    return false;

  n = UINTVAL (operands[2]);

  /* Try to keep the number of instructions low.  For cases below 16 bytes we
     need to make at most two moves.  For cases above 16 bytes it will be one
     move for each 16 byte chunk, then at most two additional moves.  */
  if (((n / 16) + (n % 16 ? 2 : 0)) > max_instructions)
    return false;

  base = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  dst = adjust_automodify_address (dst, VOIDmode, base, 0);

  base = copy_to_mode_reg (Pmode, XEXP (src, 0));
  src = adjust_automodify_address (src, VOIDmode, base, 0);

  /* Simple cases.  Copy 0-3 bytes, as (if applicable) a 2-byte, then a
     1-byte chunk.  */
  if (n < 4)
    {
      if (n >= 2)
	{
	  aarch64_copy_one_block_and_progress_pointers (&src, &dst, HImode);
	  n -= 2;
	}

      if (n == 1)
	aarch64_copy_one_block_and_progress_pointers (&src, &dst, QImode);

      return true;
    }

  /* Copy 4-8 bytes.  First a 4-byte chunk, then (if applicable) a second
     4-byte chunk, partially overlapping with the previously copied chunk.  */
  if (n < 8)
    {
      aarch64_copy_one_block_and_progress_pointers (&src, &dst, SImode);
      n -= 4;
      if (n > 0)
	{
	  int move = n - 4;

	  src = aarch64_move_pointer (src, move);
	  dst = aarch64_move_pointer (dst, move);
	  aarch64_copy_one_block_and_progress_pointers (&src, &dst, SImode);
	}
      return true;
    }

  /* Copy more than 8 bytes.  Copy chunks of 16 bytes until we run out of
     them, then (if applicable) an 8-byte chunk.  */
  while (n >= 8)
    {
      if (n / 16)
	{
	  aarch64_copy_one_block_and_progress_pointers (&src, &dst, TImode);
	  n -= 16;
	}
      else
	{
	  aarch64_copy_one_block_and_progress_pointers (&src, &dst, DImode);
	  n -= 8;
	}
    }

  /* Finish the final bytes of the copy.  We can always do this in one
     instruction.  We either copy the exact amount we need, or partially
     overlap with the previous chunk we copied and copy 8-bytes.  */
  if (n == 0)
    return true;
  else if (n == 1)
    aarch64_copy_one_block_and_progress_pointers (&src, &dst, QImode);
  else if (n == 2)
    aarch64_copy_one_block_and_progress_pointers (&src, &dst, HImode);
  else if (n == 4)
    aarch64_copy_one_block_and_progress_pointers (&src, &dst, SImode);
  else
    {
      if (n == 3)
	{
	  src = aarch64_move_pointer (src, -1);
	  dst = aarch64_move_pointer (dst, -1);
	  aarch64_copy_one_block_and_progress_pointers (&src, &dst, SImode);
	}
      else
	{
	  int move = n - 8;

	  src = aarch64_move_pointer (src, move);
	  dst = aarch64_move_pointer (dst, move);
	  aarch64_copy_one_block_and_progress_pointers (&src, &dst, DImode);
	}
    }

  return true;
}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
aarch64_asan_shadow_offset (void)
{
  return (HOST_WIDE_INT_1 << 36);
}

static bool
aarch64_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
					unsigned int align,
					enum by_pieces_operation op,
					bool speed_p)
{
  /* STORE_BY_PIECES can be used when copying a constant string, but
     in that case each 64-bit chunk takes 5 insns instead of 2 (LDR/STR).
     For now we always fail this and let the move_by_pieces code copy
     the string from read-only memory.  */
  if (op == STORE_BY_PIECES)
    return false;

  return default_use_by_pieces_infrastructure_p (size, align, op, speed_p);
}

static enum machine_mode
aarch64_code_to_ccmode (enum rtx_code code)
{
  switch (code)
    {
    case NE:
      return CC_DNEmode;

    case EQ:
      return CC_DEQmode;

    case LE:
      return CC_DLEmode;

    case LT:
      return CC_DLTmode;

    case GE:
      return CC_DGEmode;

    case GT:
      return CC_DGTmode;

    case LEU:
      return CC_DLEUmode;

    case LTU:
      return CC_DLTUmode;

    case GEU:
      return CC_DGEUmode;

    case GTU:
      return CC_DGTUmode;

    default:
      return CCmode;
    }
}

static rtx
aarch64_gen_ccmp_first (rtx *prep_seq, rtx *gen_seq,
			int code, tree treeop0, tree treeop1)
{
  enum machine_mode op_mode, cmp_mode, cc_mode;
  rtx op0, op1, cmp, target;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));
  enum insn_code icode;
  struct expand_operand ops[4];

  cc_mode = aarch64_code_to_ccmode ((enum rtx_code) code);
  if (cc_mode == CCmode)
    return NULL_RTX;

  start_sequence ();
  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);

  op_mode = GET_MODE (op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (op1);

  switch (op_mode)
    {
    case QImode:
    case HImode:
    case SImode:
      cmp_mode = SImode;
      icode = CODE_FOR_cmpsi;
      break;

    case DImode:
      cmp_mode = DImode;
      icode = CODE_FOR_cmpdi;
      break;

    default:
      end_sequence ();
      return NULL_RTX;
    }

  op0 = prepare_operand (icode, op0, 2, op_mode, cmp_mode, unsignedp);
  op1 = prepare_operand (icode, op1, 3, op_mode, cmp_mode, unsignedp);
  if (!op0 || !op1)
    {
      end_sequence ();
      return NULL_RTX;
    }
  *prep_seq = get_insns ();
  end_sequence ();

  cmp = gen_rtx_fmt_ee ((enum rtx_code) code, cmp_mode, op0, op1);
  target = gen_rtx_REG (CCmode, CC_REGNUM);

  create_output_operand (&ops[0], target, CCmode);
  create_fixed_operand (&ops[1], cmp);
  create_fixed_operand (&ops[2], op0);
  create_fixed_operand (&ops[3], op1);

  start_sequence ();
  if (!maybe_expand_insn (icode, 4, ops))
    {
      end_sequence ();
      return NULL_RTX;
    }
  *gen_seq = get_insns ();
  end_sequence ();

  return gen_rtx_REG (cc_mode, CC_REGNUM);
}

static rtx
aarch64_gen_ccmp_next (rtx *prep_seq, rtx *gen_seq, rtx prev, int cmp_code,
		       tree treeop0, tree treeop1, int bit_code)
{
  rtx op0, op1, cmp0, cmp1, target;
  enum machine_mode op_mode, cmp_mode, cc_mode;
  int unsignedp = TYPE_UNSIGNED (TREE_TYPE (treeop0));
  enum insn_code icode = CODE_FOR_ccmp_andsi;
  struct expand_operand ops[6];

  cc_mode = aarch64_code_to_ccmode ((enum rtx_code) cmp_code);
  if (cc_mode == CCmode)
    return NULL_RTX;

  push_to_sequence ((rtx_insn*) *prep_seq);
  expand_operands (treeop0, treeop1, NULL_RTX, &op0, &op1, EXPAND_NORMAL);

  op_mode = GET_MODE (op0);
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (op1);

  switch (op_mode)
    {
    case QImode:
    case HImode:
    case SImode:
      cmp_mode = SImode;
      icode = (enum rtx_code) bit_code == AND ? CODE_FOR_ccmp_andsi
						: CODE_FOR_ccmp_iorsi;
      break;

    case DImode:
      cmp_mode = DImode;
      icode = (enum rtx_code) bit_code == AND ? CODE_FOR_ccmp_anddi
						: CODE_FOR_ccmp_iordi;
      break;

    default:
      end_sequence ();
      return NULL_RTX;
    }

  op0 = prepare_operand (icode, op0, 2, op_mode, cmp_mode, unsignedp);
  op1 = prepare_operand (icode, op1, 3, op_mode, cmp_mode, unsignedp);
  if (!op0 || !op1)
    {
      end_sequence ();
      return NULL_RTX;
    }
  *prep_seq = get_insns ();
  end_sequence ();

  target = gen_rtx_REG (cc_mode, CC_REGNUM);
  cmp1 = gen_rtx_fmt_ee ((enum rtx_code) cmp_code, cmp_mode, op0, op1);
  cmp0 = gen_rtx_fmt_ee (NE, cmp_mode, prev, const0_rtx);

  create_fixed_operand (&ops[0], prev);
  create_fixed_operand (&ops[1], target);
  create_fixed_operand (&ops[2], op0);
  create_fixed_operand (&ops[3], op1);
  create_fixed_operand (&ops[4], cmp0);
  create_fixed_operand (&ops[5], cmp1);

  push_to_sequence ((rtx_insn*) *gen_seq);
  if (!maybe_expand_insn (icode, 6, ops))
    {
      end_sequence ();
      return NULL_RTX;
    }

  *gen_seq = get_insns ();
  end_sequence ();

  return target;
}

#undef TARGET_GEN_CCMP_FIRST
#define TARGET_GEN_CCMP_FIRST aarch64_gen_ccmp_first

#undef TARGET_GEN_CCMP_NEXT
#define TARGET_GEN_CCMP_NEXT aarch64_gen_ccmp_next

/* Implement TARGET_SCHED_MACRO_FUSION_P.  Return true if target supports
   instruction fusion of some sort.  */

static bool
aarch64_macro_fusion_p (void)
{
  return aarch64_tune_params->fuseable_ops != AARCH64_FUSE_NOTHING;
}


/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

static bool
aarch_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  rtx set_dest;
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  /* prev and curr are simple SET insns i.e. no flag setting or branching.  */
  bool simple_sets_p = prev_set && curr_set && !any_condjump_p (curr);

  if (!aarch64_macro_fusion_p ())
    return false;

  if (simple_sets_p
      && (aarch64_tune_params->fuseable_ops & AARCH64_FUSE_MOV_MOVK))
    {
      /* We are trying to match:
         prev (mov)  == (set (reg r0) (const_int imm16))
         curr (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 16))
                             (const_int imm16_1))  */

      set_dest = SET_DEST (curr_set);

      if (GET_CODE (set_dest) == ZERO_EXTRACT
          && CONST_INT_P (SET_SRC (curr_set))
          && CONST_INT_P (SET_SRC (prev_set))
          && CONST_INT_P (XEXP (set_dest, 2))
          && INTVAL (XEXP (set_dest, 2)) == 16
          && REG_P (XEXP (set_dest, 0))
          && REG_P (SET_DEST (prev_set))
          && REGNO (XEXP (set_dest, 0)) == REGNO (SET_DEST (prev_set)))
        {
          return true;
        }
    }

  if (simple_sets_p
      && (aarch64_tune_params->fuseable_ops & AARCH64_FUSE_ADRP_ADD))
    {

      /*  We're trying to match:
          prev (adrp) == (set (reg r1)
                              (high (symbol_ref ("SYM"))))
          curr (add) == (set (reg r0)
                             (lo_sum (reg r1)
                                     (symbol_ref ("SYM"))))
          Note that r0 need not necessarily be the same as r1, especially
          during pre-regalloc scheduling.  */

      if (satisfies_constraint_Ush (SET_SRC (prev_set))
          && REG_P (SET_DEST (prev_set)) && REG_P (SET_DEST (curr_set)))
        {
          if (GET_CODE (SET_SRC (curr_set)) == LO_SUM
              && REG_P (XEXP (SET_SRC (curr_set), 0))
              && REGNO (XEXP (SET_SRC (curr_set), 0))
                 == REGNO (SET_DEST (prev_set))
              && rtx_equal_p (XEXP (SET_SRC (prev_set), 0),
                              XEXP (SET_SRC (curr_set), 1)))
            return true;
        }
    }

  if (simple_sets_p
      && (aarch64_tune_params->fuseable_ops & AARCH64_FUSE_MOVK_MOVK))
    {

      /* We're trying to match:
         prev (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 32))
                             (const_int imm16_1))
         curr (movk) == (set (zero_extract (reg r0)
                                           (const_int 16)
                                           (const_int 48))
                             (const_int imm16_2))  */

      if (GET_CODE (SET_DEST (prev_set)) == ZERO_EXTRACT
          && GET_CODE (SET_DEST (curr_set)) == ZERO_EXTRACT
          && REG_P (XEXP (SET_DEST (prev_set), 0))
          && REG_P (XEXP (SET_DEST (curr_set), 0))
          && REGNO (XEXP (SET_DEST (prev_set), 0))
             == REGNO (XEXP (SET_DEST (curr_set), 0))
          && CONST_INT_P (XEXP (SET_DEST (prev_set), 2))
          && CONST_INT_P (XEXP (SET_DEST (curr_set), 2))
          && INTVAL (XEXP (SET_DEST (prev_set), 2)) == 32
          && INTVAL (XEXP (SET_DEST (curr_set), 2)) == 48
          && CONST_INT_P (SET_SRC (prev_set))
          && CONST_INT_P (SET_SRC (curr_set)))
        return true;

    }
  if (simple_sets_p
      && (aarch64_tune_params->fuseable_ops & AARCH64_FUSE_ADRP_LDR))
    {
      /* We're trying to match:
          prev (adrp) == (set (reg r0)
                              (high (symbol_ref ("SYM"))))
          curr (ldr) == (set (reg r1)
                             (mem (lo_sum (reg r0)
                                             (symbol_ref ("SYM")))))
                 or
          curr (ldr) == (set (reg r1)
                             (zero_extend (mem
                                           (lo_sum (reg r0)
                                                   (symbol_ref ("SYM"))))))  */
      if (satisfies_constraint_Ush (SET_SRC (prev_set))
          && REG_P (SET_DEST (prev_set)) && REG_P (SET_DEST (curr_set)))
        {
          rtx curr_src = SET_SRC (curr_set);

          if (GET_CODE (curr_src) == ZERO_EXTEND)
            curr_src = XEXP (curr_src, 0);

          if (MEM_P (curr_src) && GET_CODE (XEXP (curr_src, 0)) == LO_SUM
              && REG_P (XEXP (XEXP (curr_src, 0), 0))
              && REGNO (XEXP (XEXP (curr_src, 0), 0))
                 == REGNO (SET_DEST (prev_set))
              && rtx_equal_p (XEXP (XEXP (curr_src, 0), 1),
                              XEXP (SET_SRC (prev_set), 0)))
              return true;
        }
    }

  if ((aarch64_tune_params->fuseable_ops & AARCH64_FUSE_CMP_BRANCH)
      && any_condjump_p (curr))
    {
      enum attr_type prev_type = get_attr_type (prev);

      /* FIXME: this misses some which is considered simple arthematic
         instructions for ThunderX.  Simple shifts are missed here.  */
      if (prev_type == TYPE_ALUS_SREG
          || prev_type == TYPE_ALUS_IMM
          || prev_type == TYPE_LOGICS_REG
          || prev_type == TYPE_LOGICS_IMM)
        return true;
    }

  return false;
}

/* If MEM is in the form of [base+offset], extract the two parts
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

/* Types for scheduling fusion.  */
enum sched_fusion_type
{
  SCHED_FUSION_NONE = 0,
  SCHED_FUSION_LD_SIGN_EXTEND,
  SCHED_FUSION_LD_ZERO_EXTEND,
  SCHED_FUSION_LD,
  SCHED_FUSION_ST,
  SCHED_FUSION_NUM
};

/* If INSN is a load or store of address in the form of [base+offset],
   extract the two parts and set to BASE and OFFSET.  Return scheduling
   fusion type this INSN is.  */

static enum sched_fusion_type
fusion_load_store (rtx_insn *insn, rtx *base, rtx *offset)
{
  rtx x, dest, src;
  enum sched_fusion_type fusion = SCHED_FUSION_LD;

  gcc_assert (INSN_P (insn));
  x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return SCHED_FUSION_NONE;

  src = SET_SRC (x);
  dest = SET_DEST (x);

  if (GET_MODE (dest) != SImode && GET_MODE (dest) != DImode
      && GET_MODE (dest) != SFmode && GET_MODE (dest) != DFmode)
    return SCHED_FUSION_NONE;

  if (GET_CODE (src) == SIGN_EXTEND)
    {
      fusion = SCHED_FUSION_LD_SIGN_EXTEND;
      src = XEXP (src, 0);
      if (GET_CODE (src) != MEM || GET_MODE (src) != SImode)
	return SCHED_FUSION_NONE;
    }
  else if (GET_CODE (src) == ZERO_EXTEND)
    {
      fusion = SCHED_FUSION_LD_ZERO_EXTEND;
      src = XEXP (src, 0);
      if (GET_CODE (src) != MEM || GET_MODE (src) != SImode)
	return SCHED_FUSION_NONE;
    }

  if (GET_CODE (src) == MEM && REG_P (dest))
    extract_base_offset_in_addr (src, base, offset);
  else if (GET_CODE (dest) == MEM && (REG_P (src) || src == const0_rtx))
    {
      fusion = SCHED_FUSION_ST;
      extract_base_offset_in_addr (dest, base, offset);
    }
  else
    return SCHED_FUSION_NONE;

  if (*base == NULL_RTX || *offset == NULL_RTX)
    fusion = SCHED_FUSION_NONE;

  return fusion;
}

/* Implement the TARGET_SCHED_FUSION_PRIORITY hook.

   Currently we only support to fuse ldr or str instructions, so FUSION_PRI
   and PRI are only calculated for these instructions.  For other instruction,
   FUSION_PRI and PRI are simply set to MAX_PRI - 1.  In the future, other
   type instruction fusion can be added by returning different priorities.

   It's important that irrelevant instructions get the largest FUSION_PRI.  */

static void
aarch64_sched_fusion_priority (rtx_insn *insn, int max_pri,
			       int *fusion_pri, int *pri)
{
  int tmp, off_val;
  rtx base, offset;
  enum sched_fusion_type fusion;

  gcc_assert (INSN_P (insn));

  tmp = max_pri - 1;
  fusion = fusion_load_store (insn, &base, &offset);
  if (fusion == SCHED_FUSION_NONE)
    {
      *pri = tmp;
      *fusion_pri = tmp;
      return;
    }

  /* Set FUSION_PRI according to fusion type and base register.  */
  *fusion_pri = tmp - fusion * FIRST_PSEUDO_REGISTER - REGNO (base);

  /* Calculate PRI.  */
  tmp /= 2;

  /* INSN with smaller offset goes first.  */
  off_val = (int)(INTVAL (offset));
  if (off_val >= 0)
    tmp -= (off_val & 0xfffff);
  else
    tmp += ((- off_val) & 0xfffff);

  *pri = tmp;
  return;
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into ldp/stp.  LOAD is true if they are load instructions.
   MODE is the mode of memory operands.  */

bool
aarch64_operands_ok_for_ldpstp (rtx *operands, bool load,
				enum machine_mode mode)
{
  HOST_WIDE_INT offval_1, offval_2, msize;
  enum reg_class rclass_1, rclass_2;
  rtx mem_1, mem_2, reg_1, reg_2, base_1, base_2, offset_1, offset_2;

  if (load)
    {
      mem_1 = operands[1];
      mem_2 = operands[3];
      reg_1 = operands[0];
      reg_2 = operands[2];
      gcc_assert (REG_P (reg_1) && REG_P (reg_2));
      if (REGNO (reg_1) == REGNO (reg_2))
	return false;
    }
  else
    {
      mem_1 = operands[0];
      mem_2 = operands[2];
      reg_1 = operands[1];
      reg_2 = operands[3];
    }

  /* The mems cannot be volatile.  */
  if (MEM_VOLATILE_P (mem_1) || MEM_VOLATILE_P (mem_2))
    return false;

  /* Check if the addresses are in the form of [base+offset].  */
  extract_base_offset_in_addr (mem_1, &base_1, &offset_1);
  if (base_1 == NULL_RTX || offset_1 == NULL_RTX)
    return false;
  extract_base_offset_in_addr (mem_2, &base_2, &offset_2);
  if (base_2 == NULL_RTX || offset_2 == NULL_RTX)
    return false;

  /* Check if the bases are same.  */
  if (!rtx_equal_p (base_1, base_2))
    return false;

  offval_1 = INTVAL (offset_1);
  offval_2 = INTVAL (offset_2);
  msize = GET_MODE_SIZE (mode);
  /* Check if the offsets are consecutive.  */
  if (offval_1 != (offval_2 + msize) && offval_2 != (offval_1 + msize))
    return false;

  /* Check if the addresses are clobbered by load.  */
  if (load)
    {
      if (reg_mentioned_p (reg_1, mem_1))
	return false;

      /* In increasing order, the last load can clobber the address.  */
      if (offval_1 > offval_2 && reg_mentioned_p (reg_2, mem_2))
      return false;
    }

  if (REG_P (reg_1) && FP_REGNUM_P (REGNO (reg_1)))
    rclass_1 = FP_REGS;
  else
    rclass_1 = GENERAL_REGS;

  if (REG_P (reg_2) && FP_REGNUM_P (REGNO (reg_2)))
    rclass_2 = FP_REGS;
  else
    rclass_2 = GENERAL_REGS;

  /* Check if the registers are of same class.  */
  if (rclass_1 != rclass_2)
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store, check if we can merge
   them into ldp/stp by adjusting the offset.  LOAD is true if they
   are load instructions.  MODE is the mode of memory operands.

   Given below consecutive stores:

     str  w1, [xb, 0x100]
     str  w1, [xb, 0x104]
     str  w1, [xb, 0x108]
     str  w1, [xb, 0x10c]

   Though the offsets are out of the range supported by stp, we can
   still pair them after adjusting the offset, like:

     add  scratch, xb, 0x100
     stp  w1, w1, [scratch]
     stp  w1, w1, [scratch, 0x8]

   The peephole patterns detecting this opportunity should guarantee
   the scratch register is avaliable.  */

bool
aarch64_operands_adjust_ok_for_ldpstp (rtx *operands, bool load,
				       enum machine_mode mode)
{
  enum reg_class rclass_1, rclass_2, rclass_3, rclass_4;
  HOST_WIDE_INT offval_1, offval_2, offval_3, offval_4, msize;
  rtx mem_1, mem_2, mem_3, mem_4, reg_1, reg_2, reg_3, reg_4;
  rtx base_1, base_2, base_3, base_4, offset_1, offset_2, offset_3, offset_4;

  if (load)
    {
      reg_1 = operands[0];
      mem_1 = operands[1];
      reg_2 = operands[2];
      mem_2 = operands[3];
      reg_3 = operands[4];
      mem_3 = operands[5];
      reg_4 = operands[6];
      mem_4 = operands[7];
      gcc_assert (REG_P (reg_1) && REG_P (reg_2)
		  && REG_P (reg_3) && REG_P (reg_4));
      if (REGNO (reg_1) == REGNO (reg_2) || REGNO (reg_3) == REGNO (reg_4))
	return false;
    }
  else
    {
      mem_1 = operands[0];
      reg_1 = operands[1];
      mem_2 = operands[2];
      reg_2 = operands[3];
      mem_3 = operands[4];
      reg_3 = operands[5];
      mem_4 = operands[6];
      reg_4 = operands[7];
    }
  /* Skip if memory operand is by itslef valid for ldp/stp.  */
  if (!MEM_P (mem_1) || aarch64_mem_pair_operand (mem_1, mode))
    return false;

  /* The mems cannot be volatile.  */
  if (MEM_VOLATILE_P (mem_1) || MEM_VOLATILE_P (mem_2)
      || MEM_VOLATILE_P (mem_3) ||MEM_VOLATILE_P (mem_4))
    return false;

  /* Check if the addresses are in the form of [base+offset].  */
  extract_base_offset_in_addr (mem_1, &base_1, &offset_1);
  if (base_1 == NULL_RTX || offset_1 == NULL_RTX)
    return false;
  extract_base_offset_in_addr (mem_2, &base_2, &offset_2);
  if (base_2 == NULL_RTX || offset_2 == NULL_RTX)
    return false;
  extract_base_offset_in_addr (mem_3, &base_3, &offset_3);
  if (base_3 == NULL_RTX || offset_3 == NULL_RTX)
    return false;
  extract_base_offset_in_addr (mem_4, &base_4, &offset_4);
  if (base_4 == NULL_RTX || offset_4 == NULL_RTX)
    return false;

  /* Check if the bases are same.  */
  if (!rtx_equal_p (base_1, base_2)
      || !rtx_equal_p (base_2, base_3)
      || !rtx_equal_p (base_3, base_4))
    return false;

  offval_1 = INTVAL (offset_1);
  offval_2 = INTVAL (offset_2);
  offval_3 = INTVAL (offset_3);
  offval_4 = INTVAL (offset_4);
  msize = GET_MODE_SIZE (mode);
  /* Check if the offsets are consecutive.  */
  if ((offval_1 != (offval_2 + msize)
       || offval_1 != (offval_3 + msize * 2)
       || offval_1 != (offval_4 + msize * 3))
      && (offval_4 != (offval_3 + msize)
	  || offval_4 != (offval_2 + msize * 2)
	  || offval_4 != (offval_1 + msize * 3)))
    return false;

  /* Check if the addresses are clobbered by load.  */
  if (load)
    {
      if (reg_mentioned_p (reg_1, mem_1)
	  || reg_mentioned_p (reg_2, mem_2)
	  || reg_mentioned_p (reg_3, mem_3))
	return false;

      /* In increasing order, the last load can clobber the address.  */
      if (offval_1 > offval_2 && reg_mentioned_p (reg_4, mem_4))
	return false;
    }

  if (REG_P (reg_1) && FP_REGNUM_P (REGNO (reg_1)))
    rclass_1 = FP_REGS;
  else
    rclass_1 = GENERAL_REGS;

  if (REG_P (reg_2) && FP_REGNUM_P (REGNO (reg_2)))
    rclass_2 = FP_REGS;
  else
    rclass_2 = GENERAL_REGS;

  if (REG_P (reg_3) && FP_REGNUM_P (REGNO (reg_3)))
    rclass_3 = FP_REGS;
  else
    rclass_3 = GENERAL_REGS;

  if (REG_P (reg_4) && FP_REGNUM_P (REGNO (reg_4)))
    rclass_4 = FP_REGS;
  else
    rclass_4 = GENERAL_REGS;

  /* Check if the registers are of same class.  */
  if (rclass_1 != rclass_2 || rclass_2 != rclass_3 || rclass_3 != rclass_4)
    return false;

  return true;
}

/* Given OPERANDS of consecutive load/store, this function pairs them
   into ldp/stp after adjusting the offset.  It depends on the fact
   that addresses of load/store instructions are in increasing order.
   MODE is the mode of memory operands.  CODE is the rtl operator
   which should be applied to all memory operands, it's SIGN_EXTEND,
   ZERO_EXTEND or UNKNOWN.  */

bool
aarch64_gen_adjusted_ldpstp (rtx *operands, bool load,
			     enum machine_mode mode, RTX_CODE code)
{
  rtx base, offset, t1, t2;
  rtx mem_1, mem_2, mem_3, mem_4;
  HOST_WIDE_INT off_val, abs_off, adj_off, new_off, stp_off_limit, msize;

  if (load)
    {
      mem_1 = operands[1];
      mem_2 = operands[3];
      mem_3 = operands[5];
      mem_4 = operands[7];
    }
  else
    {
      mem_1 = operands[0];
      mem_2 = operands[2];
      mem_3 = operands[4];
      mem_4 = operands[6];
      gcc_assert (code == UNKNOWN);
    }

  extract_base_offset_in_addr (mem_1, &base, &offset);
  gcc_assert (base != NULL_RTX && offset != NULL_RTX);

  /* Adjust offset thus it can fit in ldp/stp instruction.  */
  msize = GET_MODE_SIZE (mode);
  stp_off_limit = msize * 0x40;
  off_val = INTVAL (offset);
  abs_off = (off_val < 0) ? -off_val : off_val;
  new_off = abs_off % stp_off_limit;
  adj_off = abs_off - new_off;

  /* Further adjust to make sure all offsets are OK.  */
  if ((new_off + msize * 2) >= stp_off_limit)
    {
      adj_off += stp_off_limit;
      new_off -= stp_off_limit;
    }

  /* Make sure the adjustment can be done with ADD/SUB instructions.  */
  if (adj_off >= 0x1000)
    return false;

  if (off_val < 0)
    {
      adj_off = -adj_off;
      new_off = -new_off;
    }

  /* Create new memory references.  */
  mem_1 = change_address (mem_1, VOIDmode,
			  plus_constant (DImode, operands[8], new_off));

  /* Check if the adjusted address is OK for ldp/stp.  */
  if (!aarch64_mem_pair_operand (mem_1, mode))
    return false;

  msize = GET_MODE_SIZE (mode);
  mem_2 = change_address (mem_2, VOIDmode,
			  plus_constant (DImode,
					 operands[8],
					 new_off + msize));
  mem_3 = change_address (mem_3, VOIDmode,
			  plus_constant (DImode,
					 operands[8],
					 new_off + msize * 2));
  mem_4 = change_address (mem_4, VOIDmode,
			  plus_constant (DImode,
					 operands[8],
					 new_off + msize * 3));

  if (code == ZERO_EXTEND)
    {
      mem_1 = gen_rtx_ZERO_EXTEND (DImode, mem_1);
      mem_2 = gen_rtx_ZERO_EXTEND (DImode, mem_2);
      mem_3 = gen_rtx_ZERO_EXTEND (DImode, mem_3);
      mem_4 = gen_rtx_ZERO_EXTEND (DImode, mem_4);
    }
  else if (code == SIGN_EXTEND)
    {
      mem_1 = gen_rtx_SIGN_EXTEND (DImode, mem_1);
      mem_2 = gen_rtx_SIGN_EXTEND (DImode, mem_2);
      mem_3 = gen_rtx_SIGN_EXTEND (DImode, mem_3);
      mem_4 = gen_rtx_SIGN_EXTEND (DImode, mem_4);
    }

  if (load)
    {
      operands[1] = mem_1;
      operands[3] = mem_2;
      operands[5] = mem_3;
      operands[7] = mem_4;
    }
  else
    {
      operands[0] = mem_1;
      operands[2] = mem_2;
      operands[4] = mem_3;
      operands[6] = mem_4;
    }

  /* Emit adjusting instruction.  */
  emit_insn (gen_rtx_SET (VOIDmode, operands[8],
			  plus_constant (DImode, base, adj_off)));
  /* Emit ldp/stp instructions.  */
  t1 = gen_rtx_SET (VOIDmode, operands[0], operands[1]);
  t2 = gen_rtx_SET (VOIDmode, operands[2], operands[3]);
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, t1, t2)));
  t1 = gen_rtx_SET (VOIDmode, operands[4], operands[5]);
  t2 = gen_rtx_SET (VOIDmode, operands[6], operands[7]);
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, t1, t2)));
  return true;
}

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST aarch64_address_cost

/* This hook will determines whether unnamed bitfields affect the alignment
   of the containing structure.  The hook returns true if the structure
   should inherit the alignment requirements of an unnamed bitfield's
   type.  */
#undef TARGET_ALIGN_ANON_BITFIELD
#define TARGET_ALIGN_ANON_BITFIELD hook_bool_void_true

#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.xword\t"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START aarch64_start_file

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK aarch64_output_mi_thunk

#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION aarch64_select_rtx_section

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE aarch64_asm_trampoline_template

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST aarch64_build_builtin_va_list

#undef TARGET_CALLEE_COPIES
#define TARGET_CALLEE_COPIES hook_bool_CUMULATIVE_ARGS_mode_tree_bool_false

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE aarch64_can_eliminate

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM aarch64_cannot_force_const_mem

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE aarch64_conditional_register_usage

/* Only the least significant bit is used for initialization guard
   variables.  */
#undef TARGET_CXX_GUARD_MASK_BIT
#define TARGET_CXX_GUARD_MASK_BIT hook_bool_void_true

#undef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX aarch64_c_mode_for_suffix

#ifdef TARGET_BIG_ENDIAN_DEFAULT
#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (MASK_BIG_END)
#endif

#undef TARGET_CLASS_MAX_NREGS
#define TARGET_CLASS_MAX_NREGS aarch64_class_max_nregs

#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL aarch64_builtin_decl

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN aarch64_expand_builtin

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START aarch64_expand_builtin_va_start

#undef TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN aarch64_fold_builtin

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG aarch64_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE aarch64_function_arg_advance

#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY aarch64_function_arg_boundary

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL aarch64_function_ok_for_sibcall

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE aarch64_function_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P aarch64_function_value_regno_p

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED aarch64_frame_pointer_required

#undef TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN aarch64_gimple_fold_builtin

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR aarch64_gimplify_va_arg_expr

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  aarch64_init_builtins

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P aarch64_legitimate_address_hook_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P aarch64_legitimate_constant_p

#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE aarch64_libgcc_cmp_return_mode

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_true

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE aarch64_mangle_type

#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST aarch64_memory_move_cost

#undef TARGET_MIN_DIVISIONS_FOR_RECIP_MUL
#define TARGET_MIN_DIVISIONS_FOR_RECIP_MUL aarch64_min_divisions_for_recip_mul

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

/* This target hook should return true if accesses to volatile bitfields
   should use the narrowest mode possible.  It should return false if these
   accesses should use the bitfield container type.  */
#undef TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false

#undef  TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE aarch64_override_options

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE \
  aarch64_override_options_after_change

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE aarch64_pass_by_reference

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS aarch64_preferred_reload_class

#undef TARGET_SCHED_REASSOCIATION_WIDTH
#define TARGET_SCHED_REASSOCIATION_WIDTH aarch64_reassociation_width

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD aarch64_secondary_reload

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK aarch64_shift_truncation_mask

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS aarch64_setup_incoming_varargs

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX   aarch64_struct_value_rtx

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST aarch64_register_move_cost

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY aarch64_return_in_memory

#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB aarch64_return_in_msb

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS aarch64_rtx_costs_wrapper

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE aarch64_sched_issue_rate

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD \
  aarch64_sched_first_cycle_multipass_dfa_lookahead

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT aarch64_trampoline_init

#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P aarch64_use_blocks_for_constant_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P aarch64_vector_mode_supported_p

#undef TARGET_ARRAY_MODE_SUPPORTED_P
#define TARGET_ARRAY_MODE_SUPPORTED_P aarch64_array_mode_supported_p

#undef TARGET_VECTORIZE_ADD_STMT_COST
#define TARGET_VECTORIZE_ADD_STMT_COST aarch64_add_stmt_cost

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  aarch64_builtin_vectorization_cost

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE aarch64_preferred_simd_mode

#undef TARGET_VECTORIZE_BUILTINS
#define TARGET_VECTORIZE_BUILTINS

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  aarch64_builtin_vectorized_function

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_SIZES \
  aarch64_autovectorize_vector_sizes

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV \
  aarch64_atomic_assign_expand_fenv

/* Section anchor support.  */

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -256

/* Limit the maximum anchor offset to 4k-1, since that's the limit for a
   byte offset; we can do much more for larger data types, but have no way
   to determine the size of the access.  We assume accesses are aligned.  */
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 4095

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT aarch64_simd_vector_alignment

#undef TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE \
  aarch64_simd_vector_alignment_reachable

/* vec_perm support.  */

#undef TARGET_VECTORIZE_VEC_PERM_CONST_OK
#define TARGET_VECTORIZE_VEC_PERM_CONST_OK \
  aarch64_vectorize_vec_perm_const_ok


#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS aarch64_fixed_condition_code_regs

#undef TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM CC_REGNUM

#undef TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS
#define TARGET_CALL_FUSAGE_CONTAINS_NON_CALLEE_CLOBBERS true

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET aarch64_asan_shadow_offset

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS aarch64_legitimize_address

#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P \
  aarch64_use_by_pieces_infrastructure_p

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost

#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P aarch64_macro_fusion_p

#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P aarch_macro_fusion_pair_p

#undef TARGET_SCHED_FUSION_PRIORITY
#define TARGET_SCHED_FUSION_PRIORITY aarch64_sched_fusion_priority

#undef TARGET_RELAXED_ORDERING
#define TARGET_RELAXED_ORDERING true

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-aarch64.h"
