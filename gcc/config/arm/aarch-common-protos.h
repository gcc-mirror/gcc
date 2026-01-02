/* Functions and structures shared between arm and aarch64.

   Copyright (C) 1991-2026 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


#ifndef GCC_AARCH_COMMON_PROTOS_H
#define GCC_AARCH_COMMON_PROTOS_H

#include "hard-reg-set.h"

extern int aarch_accumulator_forwarding (rtx_insn *, rtx_insn *);
extern bool aarch_rev16_p (rtx);
extern bool aarch_rev16_shleft_mask_imm_p (rtx, machine_mode);
extern bool aarch_rev16_shright_mask_imm_p (rtx, machine_mode);
extern bool aarch_mm_needs_acquire (rtx);
extern bool aarch_mm_needs_release (rtx);
extern int arm_early_load_addr_dep (rtx, rtx);
extern int arm_early_load_addr_dep_ptr (rtx, rtx);
extern int arm_early_store_addr_dep (rtx, rtx);
extern int arm_early_store_addr_dep_ptr (rtx, rtx);
extern int arm_mac_accumulator_is_mul_result (rtx, rtx);
extern int arm_mac_accumulator_is_result (rtx, rtx);
extern int arm_no_early_alu_shift_dep (rtx, rtx);
extern int arm_no_early_alu_shift_value_dep (rtx, rtx);
extern int arm_no_early_mul_dep (rtx, rtx);
extern int arm_no_early_store_addr_dep (rtx, rtx);
extern bool arm_rtx_shift_left_p (rtx);
extern void aarch_bti_arch_check (void);
extern bool aarch_bti_enabled (void);
extern bool aarch_bti_j_insn_p (rtx_insn *);
extern bool aarch_pac_insn_p (rtx);
extern rtx aarch_gen_bti_c (void);
extern rtx aarch_gen_bti_j (void);
extern bool aarch_fun_is_indirect_return (rtx_insn *);

/* RTX cost table definitions.  These are used when tuning for speed rather
   than for size and should reflect the _additional_ cost over the cost
   of the fastest instruction in the machine, which is COSTS_N_INSNS (1).
   Therefore it's okay for some costs to be 0.
   Costs may not have a negative value.  */
struct alu_cost_table
{
  int arith;		/* ADD/SUB.  */
  int logical;		/* AND/ORR/EOR/BIC, etc.  */
  int shift;		/* Simple shift.  */
  int shift_reg;	/* Simple shift by reg.  */
  int arith_shift;	/* Additional when arith also shifts...  */
  int arith_shift_reg;	/* ... and when the shift is by a reg.  */
  int log_shift;	/* Additional when logic also shifts...  */
  int log_shift_reg;	/* ... and when the shift is by a reg.  */
  int extend;		/* Zero/sign extension.  */
  int extend_arith;	/* Extend and arith.  */
  int bfi;		/* Bit-field insert.  */
  int bfx;		/* Bit-field extraction.  */
  int clz;		/* Count Leading Zeros.  */
  int rev;		/* Reverse bits/bytes.  */
  int non_exec;		/* Extra cost when not executing insn.  */
  bool non_exec_costs_exec; /* True if non-execution must add the exec
				     cost.  */
};

struct mult_cost_table
{
  int simple;
  int flag_setting;	/* Additional cost if multiply sets flags. */
  int extend;
  int add;
  int extend_add;
  int idiv;
};

/* Calculations of LDM costs are complex.  We assume an initial cost
   (ldm_1st) which will load the number of registers mentioned in
   ldm_regs_per_insn_1st registers; then each additional
   ldm_regs_per_insn_subsequent registers cost one more insn.
   Similarly for STM operations.
   Therefore the ldm_regs_per_insn_1st/stm_regs_per_insn_1st and
   ldm_regs_per_insn_subsequent/stm_regs_per_insn_subsequent fields indicate
   the number of registers loaded/stored and are expressed by a simple integer
   and not by a COSTS_N_INSNS (N) expression.
   */
struct mem_cost_table
{
  int load;
  int load_sign_extend;	/* Additional to load cost.  */
  int ldrd;		/* Cost of LDRD.  */
  int ldm_1st;
  int ldm_regs_per_insn_1st;
  int ldm_regs_per_insn_subsequent;
  int loadf;		/* SFmode.  */
  int loadd;		/* DFmode.  */
  int load_unaligned;	/* Extra for unaligned loads.  */
  int store;
  int strd;
  int stm_1st;
  int stm_regs_per_insn_1st;
  int stm_regs_per_insn_subsequent;
  int storef;		/* SFmode.  */
  int stored;		/* DFmode.  */
  int store_unaligned;	/* Extra for unaligned stores.  */
  int loadv;		/* Vector load.  */
  int storev;		/* Vector store.  */
};

struct fp_cost_table
{
  int div;
  int mult;
  int mult_addsub;	/* Non-fused.  */
  int fma;		/* Fused.  */
  int addsub;
  int fpconst;		/* Immediate.  */
  int neg;		/* NEG and ABS.  */
  int compare;
  int widen;		/* Widen to this size.  */
  int narrow;		/* Narrow from this size.  */
  int toint;
  int fromint;
  int roundint;		/* V8 round to integral, remains FP format.  */
};

struct vector_cost_table
{
  int alu;
  int mult;
  int movi;
  int dup;
  int extract;
};

struct cpu_cost_table
{
  struct alu_cost_table alu;
  struct mult_cost_table mult[2]; /* SImode and DImode.  */
  struct mem_cost_table ldst;
  struct fp_cost_table fp[2]; /* SFmode and DFmode.  */
  struct vector_cost_table vect;
};

rtx_insn *arm_md_asm_adjust (vec<rtx> &outputs, vec<rtx> & /*inputs*/,
			     vec<machine_mode> & /*input_modes*/,
			     vec<const char *> &constraints, vec<rtx> &,
			     vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs,
			     location_t loc);

/* Specifies a -mbranch-protection= argument.  */
struct aarch_branch_protect_type
{
  /* The type's name that the user passes to the branch-protection option
     string.  */
  const char* name;
  /* The type can only appear alone, other types should be rejected.  */
  int alone;
  /* Function to handle the protection type and set global variables.  */
  void (*handler)(void);
  /* A list of types that can follow this type in the option string.  */
  const struct aarch_branch_protect_type* subtypes;
  unsigned int num_subtypes;
};

bool aarch_validate_mbranch_protection (
  const struct aarch_branch_protect_type *, const char *, const char *);

#endif /* GCC_AARCH_COMMON_PROTOS_H */
