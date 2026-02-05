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

#ifndef GCC_AARCH64_H_HIP12
#define GCC_AARCH64_H_HIP12

#include "generic.h"

static const struct cpu_addrcost_table hip12_addrcost_table =
{
    {
      0, /* hi  */
      0, /* si  */
      0, /* di  */
      2, /* ti  */
    },
  0, /* pre_modify  */
  0, /* post_modify  */
  2, /* post_modify_ld3_st3  */
  2, /* post_modify_ld4_st4  */
  0, /* register_offset  */
  0, /* register_sextend  */
  0, /* register_zextend  */
  0, /* imm_offset  */
};

static const struct cpu_regmove_cost hip12_regmove_cost =
{
  1, /* GP2GP  */
  /* Spilling to int<->fp instead of memory is recommended so set
     realistic costs compared to memmov_cost.  */
  5, /* GP2FP  */
  2, /* FP2GP  */
  2  /* FP2FP  */
};

static const advsimd_vec_cost hip12_advsimd_vector_cost =
{
  2, /* int_stmt_cost  */
  2, /* fp_stmt_cost  */
  2, /* ld2_st2_permute_cost  */
  2, /* ld3_st3_permute_cost  */
  3, /* ld4_st4_permute_cost  */
  2, /* permute_cost  */
  9, /* reduc_i8_cost  */
  7, /* reduc_i16_cost  */
  5, /* reduc_i32_cost  */
  3, /* reduc_i64_cost  */
  9, /* reduc_f16_cost  */
  6, /* reduc_f32_cost  */
  3, /* reduc_f64_cost  */
  2, /* store_elt_extra_cost  */
  2, /* vec_to_scalar_cost  */
  4, /* scalar_to_vec_cost  */
  6, /* align_load_cost  */
  6, /* unalign_load_cost  */
  1, /* unalign_store_cost  */
  1  /* store_cost  */
};

static const sve_vec_cost hip12_sve_vector_cost =
{
  {
    2, /* int_stmt_cost  */
    2, /* fp_stmt_cost  */
    2, /* ld2_st2_permute_cost  */
    3, /* ld3_st3_permute_cost  */
    3, /* ld4_st4_permute_cost  */
    2, /* permute_cost  */
    /* Theoretically, a reduction involving 31 scalar ADDs could
       complete in ~6 cycles and would have a cost of 31.  [SU]ADDV
       completes in 13 cycles, so give it a cost of 31 + 7.  */
    38, /* reduc_i8_cost  */
    /* Likewise for 15 scalar ADDs (~3 cycles) vs. 10: 15 + 7.  */
    22, /* reduc_i16_cost  */
    /* Likewise for 7 scalar ADDs (~2 cycles) vs. 7: 7 + 5.  */
    12, /* reduc_i32_cost  */
    /* Likewise for 3 scalar ADDs (~1 cycles) vs. 4: 3 + 3.  */
    6, /* reduc_i64_cost  */
    /* Theoretically, a reduction involving 15 scalar FADDs could
       complete in ~8 cycles and would have a cost of 30.  FADDV
       completes in 15 cycles, so give it a cost of 30 + 7.  */
    37, /* reduc_f16_cost  */
    /* Likewise for 7 scalar FADDs (~4 cycles) vs. 12: 14 + 8.  */
    22, /* reduc_f32_cost  */
    /* Likewise for 3 scalar FADDs (~2 cycles) vs. 9: 6 + 7.  */
    13, /* reduc_f64_cost  */
    2, /* store_elt_extra_cost  */
    2, /* vec_to_scalar_cost  */
    4, /* scalar_to_vec_cost  */
    6, /* align_load_cost  */
    6, /* unalign_load_cost  */
    1, /* unalign_store_cost  */
    1  /* store_cost  */
  },
  3, /* clast_cost  */
  42, /* fadda_f16_cost  */
  26, /* fadda_f32_cost  */
  20, /* fadda_f64_cost  */
  32, /* gather_load_x32_cost  */
  16, /* gather_load_x64_cost  */
  3 /* scatter_store_elt_cost  */
};

static const aarch64_scalar_vec_issue_info hip12_scalar_issue_info =
{
  5, /* loads_stores_per_cycle  */
  2, /* stores_per_cycle  */
  6, /* general_ops_per_cycle  */
  0, /* fp_simd_load_general_ops  */
  1 /* fp_simd_store_general_ops  */
};

static const aarch64_advsimd_vec_issue_info hip12_advsimd_issue_info =
{
  {
    5, /* loads_stores_per_cycle  */
    2, /* stores_per_cycle  */
    4, /* general_ops_per_cycle  */
    0, /* fp_simd_load_general_ops  */
    1 /* fp_simd_store_general_ops  */
  },
  2, /* ld2_st2_general_ops  */
  2, /* ld3_st3_general_ops  */
  3 /* ld4_st4_general_ops  */
};

static const aarch64_sve_vec_issue_info hip12_sve_issue_info =
{
  {
    {
      5, /* loads_stores_per_cycle  */
      2, /* stores_per_cycle  */
      2, /* general_ops_per_cycle  */
      0, /* fp_simd_load_general_ops  */
      1 /* fp_simd_store_general_ops  */
    },
    2, /* ld2_st2_general_ops  */
    2, /* ld3_st3_general_ops  */
    3 /* ld4_st4_general_ops  */
  },
  2, /* pred_ops_per_cycle  */
  2, /* while_pred_ops  */
  2, /* int_cmp_pred_ops  */
  1, /* fp_cmp_pred_ops  */
  1, /* gather_scatter_pair_general_ops  */
  1 /* gather_scatter_pair_pred_ops  */
};

static const aarch64_vec_issue_info hip12_vec_issue_info =
{
  &hip12_scalar_issue_info,
  &hip12_advsimd_issue_info,
  &hip12_sve_issue_info
};

static const struct cpu_vector_cost hip12_vector_cost =
{
  1, /* scalar_int_stmt_cost  */
  2, /* scalar_fp_stmt_cost  */
  4, /* scalar_load_cost  */
  1, /* scalar_store_cost  */
  1, /* cond_taken_branch_cost  */
  1, /* cond_not_taken_branch_cost  */
  &hip12_advsimd_vector_cost, /* advsimd  */
  &hip12_sve_vector_cost, /* sve  */
  &hip12_vec_issue_info /* issue_info  */
};

static const struct tune_params hip12_tunings =
{
  &hip12_extra_costs,
  &hip12_addrcost_table,
  &hip12_regmove_cost,
  &hip12_vector_cost,
  &generic_branch_cost,
  &generic_approx_modes,
  SVE_256, /* sve_width  */
  { 4, /* load_int.  */
    1, /* store_int.  */
    6, /* load_fp.  */
    1, /* store_fp.  */
    8, /* load_pred.  */
    4 /* store_pred.  */
  }, /* memmov_cost.  */
  8,    /* issue_rate  */
  (AARCH64_FUSE_CMP_BRANCH
   | AARCH64_FUSE_AES_AESMC), /* fusible_ops  */
  "16", /* function_align.  */
  "4",  /* jump_align.  */
  "8",  /* loop_align.  */
  2,    /* int_reassoc_width.  */
  4,    /* fp_reassoc_width.  */
  2,    /* fma_reassoc_width.  */
  2,    /* vec_reassoc_width.  */
  2,    /* min_div_recip_mul_sf.  */
  2,    /* min_div_recip_mul_df.  */
  0,    /* max_case_values.  */
  tune_params::AUTOPREFETCHER_WEAK,    /* autoprefetcher_model.  */
  (AARCH64_EXTRA_TUNE_CHEAP_SHIFT_EXTEND
   | AARCH64_EXTRA_TUNE_FULLY_PIPELINED_FMA
   | AARCH64_EXTRA_TUNE_USE_NEW_VECTOR_COSTS
   | AARCH64_EXTRA_TUNE_MATCHED_VECTOR_THROUGHPUT),    /* tune_flags.  */
  &generic_armv8_a_prefetch_tune,
  AARCH64_LDP_STP_POLICY_ALWAYS,    /* ldp_policy_model.  */
  AARCH64_LDP_STP_POLICY_ALWAYS     /* stp_policy_model.  */
};

#endif /* GCC_AARCH64_H_HIP12.  */
