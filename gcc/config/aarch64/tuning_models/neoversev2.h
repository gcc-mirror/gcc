/* Tuning model description for AArch64 architecture.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.

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

#ifndef GCC_AARCH64_H_NEOVERSEV2
#define GCC_AARCH64_H_NEOVERSEV2

#include "generic.h"

static const struct cpu_addrcost_table neoversev2_addrcost_table =
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

static const struct cpu_regmove_cost neoversev2_regmove_cost =
{
  1, /* GP2GP  */
  /* Spilling to int<->fp instead of memory is recommended so set
     realistic costs compared to memmov_cost.  */
  3, /* GP2FP  */
  2, /* FP2GP  */
  2 /* FP2FP  */
};

static const advsimd_vec_cost neoversev2_advsimd_vector_cost =
{
  2, /* int_stmt_cost  */
  2, /* fp_stmt_cost  */
  2, /* ld2_st2_permute_cost */
  2, /* ld3_st3_permute_cost  */
  3, /* ld4_st4_permute_cost  */
  3, /* permute_cost  */
  4, /* reduc_i8_cost  */
  4, /* reduc_i16_cost  */
  2, /* reduc_i32_cost  */
  2, /* reduc_i64_cost  */
  6, /* reduc_f16_cost  */
  3, /* reduc_f32_cost  */
  2, /* reduc_f64_cost  */
  2, /* store_elt_extra_cost  */
  /* This value is just inherited from the Cortex-A57 table.  */
  8, /* vec_to_scalar_cost  */
  /* This depends very much on what the scalar value is and
     where it comes from.  E.g. some constants take two dependent
     instructions or a load, while others might be moved from a GPR.
     4 seems to be a reasonable compromise in practice.  */
  4, /* scalar_to_vec_cost  */
  4, /* align_load_cost  */
  4, /* unalign_load_cost  */
  /* Although stores have a latency of 2 and compete for the
     vector pipes, in practice it's better not to model that.  */
  1, /* unalign_store_cost  */
  1  /* store_cost  */
};

static const sve_vec_cost neoversev2_sve_vector_cost =
{
  {
    2, /* int_stmt_cost  */
    2, /* fp_stmt_cost  */
    3, /* ld2_st2_permute_cost  */
    3, /* ld3_st3_permute_cost  */
    4, /* ld4_st4_permute_cost  */
    3, /* permute_cost  */
    /* Theoretically, a reduction involving 15 scalar ADDs could
       complete in ~3 cycles and would have a cost of 15.  [SU]ADDV
       completes in 11 cycles, so give it a cost of 15 + 8.  */
    21, /* reduc_i8_cost  */
    /* Likewise for 7 scalar ADDs (~2 cycles) vs. 9: 7 + 7.  */
    14, /* reduc_i16_cost  */
    /* Likewise for 3 scalar ADDs (~2 cycles) vs. 8: 3 + 4.  */
    7, /* reduc_i32_cost  */
    /* Likewise for 1 scalar ADD (~1 cycles) vs. 2: 1 + 1.  */
    2, /* reduc_i64_cost  */
    /* Theoretically, a reduction involving 7 scalar FADDs could
       complete in ~6 cycles and would have a cost of 14.  FADDV
       completes in 8 cycles, so give it a cost of 14 + 2.  */
    16, /* reduc_f16_cost  */
    /* Likewise for 3 scalar FADDs (~4 cycles) vs. 6: 6 + 2.  */
    8, /* reduc_f32_cost  */
    /* Likewise for 1 scalar FADD (~2 cycles) vs. 4: 2 + 2.  */
    4, /* reduc_f64_cost  */
    2, /* store_elt_extra_cost  */
    /* This value is just inherited from the Cortex-A57 table.  */
    8, /* vec_to_scalar_cost  */
    /* See the comment above the Advanced SIMD versions.  */
    4, /* scalar_to_vec_cost  */
    4, /* align_load_cost  */
    4, /* unalign_load_cost  */
    /* Although stores have a latency of 2 and compete for the
       vector pipes, in practice it's better not to model that.  */
    1, /* unalign_store_cost  */
    1  /* store_cost  */
  },
  3, /* clast_cost  */
  10, /* fadda_f16_cost  */
  6, /* fadda_f32_cost  */
  4, /* fadda_f64_cost  */
  /* A strided Advanced SIMD x64 load would take two parallel FP loads
     (8 cycles) plus an insertion (2 cycles).  Assume a 64-bit SVE gather
     is 1 cycle more.  The Advanced SIMD version is costed as 2 scalar loads
     (cost 8) and a vec_construct (cost 2).  Add a full vector operation
     (cost 2) to that, to avoid the difference being lost in rounding.

     There is no easy comparison between a strided Advanced SIMD x32 load
     and an SVE 32-bit gather, but cost an SVE 32-bit gather as 1 vector
     operation more than a 64-bit gather.  */
  14, /* gather_load_x32_cost  */
  12, /* gather_load_x64_cost  */
  3 /* scatter_store_elt_cost  */
};

static const aarch64_scalar_vec_issue_info neoversev2_scalar_issue_info =
{
  3, /* loads_stores_per_cycle  */
  2, /* stores_per_cycle  */
  6, /* general_ops_per_cycle  */
  0, /* fp_simd_load_general_ops  */
  1 /* fp_simd_store_general_ops  */
};

static const aarch64_advsimd_vec_issue_info neoversev2_advsimd_issue_info =
{
  {
    3, /* loads_stores_per_cycle  */
    2, /* stores_per_cycle  */
    4, /* general_ops_per_cycle  */
    0, /* fp_simd_load_general_ops  */
    1 /* fp_simd_store_general_ops  */
  },
  2, /* ld2_st2_general_ops  */
  2, /* ld3_st3_general_ops  */
  3 /* ld4_st4_general_ops  */
};

static const aarch64_sve_vec_issue_info neoversev2_sve_issue_info =
{
  {
    {
      3, /* loads_per_cycle  */
      2, /* stores_per_cycle  */
      4, /* general_ops_per_cycle  */
      0, /* fp_simd_load_general_ops  */
      1 /* fp_simd_store_general_ops  */
    },
    2, /* ld2_st2_general_ops  */
    3, /* ld3_st3_general_ops  */
    3 /* ld4_st4_general_ops  */
  },
  2, /* pred_ops_per_cycle  */
  2, /* while_pred_ops  */
  2, /* int_cmp_pred_ops  */
  1, /* fp_cmp_pred_ops  */
  1, /* gather_scatter_pair_general_ops  */
  1 /* gather_scatter_pair_pred_ops  */
};

static const aarch64_vec_issue_info neoversev2_vec_issue_info =
{
  &neoversev2_scalar_issue_info,
  &neoversev2_advsimd_issue_info,
  &neoversev2_sve_issue_info
};

/* Demeter costs for vector insn classes.  */
static const struct cpu_vector_cost neoversev2_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  2, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &neoversev2_advsimd_vector_cost, /* advsimd  */
  &neoversev2_sve_vector_cost, /* sve  */
  &neoversev2_vec_issue_info /* issue_info  */
};

static const struct tune_params neoversev2_tunings =
{
  &cortexa76_extra_costs,
  &neoversev2_addrcost_table,
  &neoversev2_regmove_cost,
  &neoversev2_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_128, /* sve_width  */
  { 4, /* load_int.  */
    2, /* store_int.  */
    6, /* load_fp.  */
    1, /* store_fp.  */
    6, /* load_pred.  */
    2 /* store_pred.  */
  }, /* memmov_cost.  */
  5, /* issue_rate  */
  (AARCH64_FUSE_AES_AESMC | AARCH64_FUSE_CMP_BRANCH), /* fusible_ops  */
  "32:16",	/* function_align.  */
  "4",		/* jump_align.  */
  "32:16",	/* loop_align.  */
  3,	/* int_reassoc_width.  */
  6,	/* fp_reassoc_width.  */
  4,	/* fma_reassoc_width.  */
  3,	/* vec_reassoc_width.  */
  2,	/* min_div_recip_mul_sf.  */
  2,	/* min_div_recip_mul_df.  */
  0,	/* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,	/* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND
   | AARCH64_EXTRA_TUNE_CSE_SVE_VL_CONSTANTS
   | AARCH64_EXTRA_TUNE_USE_NEW_VECTOR_COSTS
   | AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT),	/* tune_flags.  */
  &generic_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,   /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS	   /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_NEOVERSEV2.  */
