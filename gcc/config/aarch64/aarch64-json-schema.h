/* Raw JSON schema for the AArch64 tuning parameters.
   Copyright The GNU Toolchain Authors.

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

#ifndef AARCH64_JSON_SCHEMA_H
#define AARCH64_JSON_SCHEMA_H

static const char *schema_json = R"json(
{
  "metadata": {
    "gcc_version": "int"
  },
  "tune_params": {
    "insn_extra_cost": {
      "alu": {
	"arith": "int",
	"logical": "int",
	"shift": "int",
	"shift_reg": "int",
	"arith_shift": "int",
	"arith_shift_reg": "int",
	"log_shift": "int",
	"log_shift_reg": "int",
	"extend": "int",
	"extend_arith": "int",
	"bfi": "int",
	"bfx": "int",
	"clz": "int",
	"rev": "int",
	"non_exec": "int",
	"non_exec_costs_exec": "boolean"
      },
      "mult": [
	{
	  "simple": "int",
	  "flag_setting": "int",
	  "extend": "int",
	  "add": "int",
	  "extend_add": "int",
	  "idiv": "int"
	},
	{
	  "simple": "int",
	  "flag_setting": "int",
	  "extend": "int",
	  "add": "int",
	  "extend_add": "int",
	  "idiv": "int"
	}
      ],
      "ldst": {
	"load": "int",
	"load_sign_extend": "int",
	"ldrd": "int",
	"ldm_1st": "int",
	"ldm_regs_per_insn_1st": "int",
	"ldm_regs_per_insn_subsequent": "int",
	"loadf": "int",
	"loadd": "int",
	"load_unaligned": "int",
	"store": "int",
	"strd": "int",
	"stm_1st": "int",
	"stm_regs_per_insn_1st": "int",
	"stm_regs_per_insn_subsequent": "int",
	"storef": "int",
	"stored": "int",
	"store_unaligned": "int",
	"loadv": "int",
	"storev": "int"
      },
      "fp": [
	{
	  "div": "int",
	  "mult": "int",
	  "mult_addsub": "int",
	  "fma": "int",
	  "addsub": "int",
	  "fpconst": "int",
	  "neg": "int",
	  "compare": "int",
	  "widen": "int",
	  "narrow": "int",
	  "toint": "int",
	  "fromint": "int",
	  "roundint": "int"
	},
	{
	  "div": "int",
	  "mult": "int",
	  "mult_addsub": "int",
	  "fma": "int",
	  "addsub": "int",
	  "fpconst": "int",
	  "neg": "int",
	  "compare": "int",
	  "widen": "int",
	  "narrow": "int",
	  "toint": "int",
	  "fromint": "int",
	  "roundint": "int"
	}
      ],
      "vect": {
	"alu": "int",
	"mult": "int",
	"movi": "int",
	"dup": "int",
	"extract": "int"
      }
    },
    "addr_cost": {
      "addr_scale_costs": {
	"hi": "int",
	"si": "int",
	"di": "int",
	"ti": "int"
      },
      "pre_modify": "int",
      "post_modify": "int",
      "post_modify_ld3_st3": "int",
      "post_modify_ld4_st4": "int",
      "register_offset": "int",
      "register_sextend": "int",
      "register_zextend": "int",
      "imm_offset": "int"
    },
    "regmove_cost": {
      "GP2GP": "int",
      "GP2FP": "int",
      "FP2GP": "int",
      "FP2FP": "int"
    },
    "vec_costs": {
      "scalar_int_stmt_cost": "int",
      "scalar_fp_stmt_cost": "int",
      "scalar_load_cost": "int",
      "scalar_store_cost": "int",
      "cond_taken_branch_cost": "int",
      "cond_not_taken_branch_cost": "int",
      "advsimd": {
	"int_stmt_cost": "int",
	"fp_stmt_cost": "int",
	"ld2_st2_permute_cost": "int",
	"ld3_st3_permute_cost": "int",
	"ld4_st4_permute_cost": "int",
	"permute_cost": "int",
	"reduc_i8_cost": "int",
	"reduc_i16_cost": "int",
	"reduc_i32_cost": "int",
	"reduc_i64_cost": "int",
	"reduc_f16_cost": "int",
	"reduc_f32_cost": "int",
	"reduc_f64_cost": "int",
	"store_elt_extra_cost": "int",
	"vec_to_scalar_cost": "int",
	"scalar_to_vec_cost": "int",
	"align_load_cost": "int",
	"unalign_load_cost": "int",
	"unalign_store_cost": "int",
	"store_cost": "int"
      },
      "sve": {
	"clast_cost": "int",
	"fadda_f16_cost": "int",
	"fadda_f32_cost": "int",
	"fadda_f64_cost": "int",
	"gather_load_x32_cost": "uint",
	"gather_load_x64_cost": "uint",
	"gather_load_x32_init_cost": "int",
	"gather_load_x64_init_cost": "int",
	"scatter_store_elt_cost": "int"
      },
      "issue_info": {
	"scalar": {
	  "loads_stores_per_cycle": "uint",
	  "stores_per_cycle": "uint",
	  "general_ops_per_cycle": "uint",
	  "fp_simd_load_general_ops": "uint",
	  "fp_simd_store_general_ops": "uint"
	},
	"advsimd": {
	  "loads_stores_per_cycle": "uint",
	  "stores_per_cycle": "uint",
	  "general_ops_per_cycle": "uint",
	  "fp_simd_load_general_ops": "uint",
	  "fp_simd_store_general_ops": "uint",
	  "ld2_st2_general_ops": "uint",
	  "ld3_st3_general_ops": "uint",
	  "ld4_st4_general_ops": "uint"
	},
	"sve": {
	  "loads_stores_per_cycle": "uint",
	  "stores_per_cycle": "uint",
	  "general_ops_per_cycle": "uint",
	  "fp_simd_load_general_ops": "uint",
	  "fp_simd_store_general_ops": "uint",
	  "ld2_st2_general_ops": "uint",
	  "ld3_st3_general_ops": "uint",
	  "ld4_st4_general_ops": "uint",
	  "pred_ops_per_cycle": "uint",
	  "while_pred_ops": "uint",
	  "int_cmp_pred_ops": "uint",
	  "fp_cmp_pred_ops": "uint",
	  "gather_scatter_pair_general_ops": "uint",
	  "gather_scatter_pair_pred_ops": "uint"
	}
      }
    },
    "branch_costs": {
      "predictable": "int",
      "unpredictable": "int",
      "br_mispredict_factor": "int"
    },
    "approx_modes": { "division": "int", "sqrt": "int", "recip_sqrt": "int" },
    "sve_width": "uint",
    "memmov_cost": {
      "load_int": "int",
      "store_int": "int",
      "load_fp": "int",
      "store_fp": "int",
      "load_pred": "int",
      "store_pred": "int"
    },
    "issue_rate": "int",
    "fusible_ops": "uint",
    "function_align": "string",
    "jump_align": "string",
    "loop_align": "string",
    "int_reassoc_width": "int",
    "fp_reassoc_width": "int",
    "fma_reassoc_width": "int",
    "vec_reassoc_width": "int",
    "min_div_recip_mul_sf": "int",
    "min_div_recip_mul_df": "int",
    "max_case_values": "uint",
    "autoprefetcher_model": "enum",
    "extra_tuning_flags": "uint",
    "prefetch": {
      "num_slots": "int",
      "l1_cache_size": "int",
      "l1_cache_line_size": "int",
      "l2_cache_size": "int",
      "prefetch_dynamic_strides": "boolean",
      "minimum_stride": "int",
      "default_opt_level": "int"
    },
    "ldp_policy_model": "enum",
    "stp_policy_model": "enum"
  }
})json";

#endif