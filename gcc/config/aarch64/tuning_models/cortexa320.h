/* Tuning model description for AArch64 architecture.
   Copyright (C) 2009-2026 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_H_CORTEXA320
#define GCC_AARCH64_H_CORTEXA320

#include "generic.h"

static const struct cpu_addrcost_table cortexa320_addrcost_table =
{
    {
      1, /* hi  */
      0, /* si  */
      0, /* di  */
      1, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  2, /* post_modify_ld3_st3  */
  2, /* post_modify_ld4_st4  */
  0, /* register_offset  */
  0, /* register_sextend  */
  0, /* register_zextend  */
  0 /* imm_offset  */
};

static const struct cpu_regmove_cost cortexa320_regmove_cost =
{
  1, /* GP2GP  */
  /* Avoid the use of slow int<->fp moves for spilling by setting
     their cost higher than memmov_cost.  */
  5, /* GP2FP  */
  5, /* FP2GP  */
  2 /* FP2FP  */
};

static const advsimd_vec_cost cortexa320_advsimd_vector_cost =
{
  3, /* int_stmt_cost  */
  4, /* fp_stmt_cost  */
  1, /* ld2_st2_permute_cost */
  2, /* ld3_st3_permute_cost  */
  3, /* ld4_st4_permute_cost  */
  3, /* permute_cost  */
  3, /* reduc_i8_cost  */
  4, /* reduc_i16_cost  */
  4, /* reduc_i32_cost  */
  3, /* reduc_i64_cost  */
  12, /* reduc_f16_cost  */
  8, /* reduc_f32_cost  */
  4, /* reduc_f64_cost  */
  0, /* reduc_dotprod_cost  */
  0, /* store_elt_extra_cost  */
  /* This value is just inherited from the Cortex-A57 table.  */
  8, /* vec_to_scalar_cost  */
  /* This depends very much on what the scalar value is and
     where it comes from.  E.g. some constants take two dependent
     instructions or a load, while others might be moved from a GPR.
     4 seems to be a reasonable compromise in practice.  */
  4, /* scalar_to_vec_cost  */
  5, /* align_load_cost  */
  5, /* unalign_load_cost  */
  /* Although stores have a latency of 2 and compete for the
     vector pipes, in practice it's better not to model that.  */
  2, /* unalign_store_cost  */
  2  /* store_cost  */
};

static const sve_vec_cost cortexa320_sve_vector_cost =
{
  {
    3, /* int_stmt_cost  */
    4, /* fp_stmt_cost  */
    1, /* ld2_st2_permute_cost  */
    2, /* ld3_st3_permute_cost  */
    3, /* ld4_st4_permute_cost  */
    3, /* permute_cost  */
    /* Theoretically, a reduction involving 15 scalar ADDs could
       complete in ~15 cycles and would have a cost of 15.  [SU]ADDV
       completes in 4 cycles, so give it a cost of 15 + -11.  */
    4, /* reduc_i8_cost  */
    /* Likewise for 7 scalar ADDs (~7 cycles) vs. 4: 7 + -3.  */
    4, /* reduc_i16_cost  */
    /* Likewise for 3 scalar ADDs (~3 cycles) vs. 4: 3 + 1.  */
    4, /* reduc_i32_cost  */
    /* Likewise for 1 scalar ADD (~1 cycles) vs. 4: 1 + 3.  */
    4, /* reduc_i64_cost  */
    /* Theoretically, a reduction involving 7 scalar FADDs could
       complete in ~28 cycles and would have a cost of  28.  FADDV
       completes in 12 cycles, so give it a cost of 28 + -16.  */
    12, /* reduc_f16_cost  */
    /* Likewise for 3 scalar FADDs (~12 cycles) vs. 8: 12 + -4.  */
    8, /* reduc_f32_cost  */
    /* Likewise for 1 scalar FADD (~4 cycles) vs. 4: 4 + 0.  */
    4, /* reduc_f64_cost  */
    0, /* reduc_dotprod_cost  */
    0, /* store_elt_extra_cost  */
    /* This value is just inherited from the Cortex-A57 table.  */
    8, /* vec_to_scalar_cost  */
    /* See the comment above the Advanced SIMD versions.  */
    4, /* scalar_to_vec_cost  */
    5, /* align_load_cost  */
    5, /* unalign_load_cost  */
    /* Although stores have a latency of 2 and compete for the
       vector pipes, in practice it's better not to model that.  */
    2, /* unalign_store_cost  */
    2  /* store_cost  */
  },
  4, /* clast_cost  */
  25, /* fadda_f16_cost  */
  9, /* fadda_f32_cost  */
  3, /* fadda_f64_cost  */
  /* A strided Advanced SIMD x64 load would take two parallel FP loads
     (10 cycles) plus an insertion (3 cycles).  Assume a 64-bit SVE gather
     is 1 cycle more.  The Advanced SIMD version is costed as 2 scalar loads
     (cost 10) and a vec_construct (cost 3).  Add a full vector operation
     (cost 3) to that, to avoid the difference being lost in rounding.

     There is no easy comparison between a strided Advanced SIMD x32 load
     and an SVE 32-bit gather, but cost an SVE 32-bit gather as 1 vector
     operation more than a 64-bit gather.  */
  16, /* gather_load_x32_cost  */
  19, /* gather_load_x64_cost  */
  48, /* gather_load_x32_init_cost  */
  38, /* gather_load_x64_init_cost  */
  0 /* scatter_store_elt_cost  */
};

/* Cortexa320 costs for vector insn classes.  */
static const struct cpu_vector_cost cortexa320_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  2, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &cortexa320_advsimd_vector_cost, /* advsimd  */
  &cortexa320_sve_vector_cost, /* sve  */
  nullptr /* issue_info  */
};

static const struct tune_params cortexa320_tunings =
{
  &cortexa76_extra_costs,
  &cortexa320_addrcost_table,
  &cortexa320_regmove_cost,
  &cortexa320_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_128, /* sve_width  */
  { 4, /* load_int.  */
    1, /* store_int.  */
    5, /* load_fp.  */
    2, /* store_fp.  */
    4, /* load_pred.  */
    1 /* store_pred.  */
  }, /* memmov_cost.  */
  1, /* issue_rate  */
  AARCH64_FUSE_NEOVERSE_BASE, /* fusible_ops  */
  "32:16",	/* function_align.  */
  "4",		/* jump_align.  */
  "32:16",	/* loop_align.  */
  2,	/* int_reassoc_width.  */
  4,	/* fp_reassoc_width.  */
  1,	/* fma_reassoc_width.  */
  2,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND
   | AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS
   | AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT),	/* tune_flags.  */
  &generic_armv9a_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* stp_policy_model.  */
  nullptr	/* dispatch_constraints.  */
};

#endif /* GCC_AARCH64_H_CORTEXA320.  */
