/* Routines to print the AArch64 tuning parameters to a JSON file.
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

#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "aarch64-json-tunings-printer.h"
#include "aarch64-protos.h"
#include "config/arm/aarch-common-protos.h"
#include "json.h"
#include "version.h"

#define SERIALIZE_INTEGER_FIELD(obj, key, member)                              \
  (obj)->set_integer ((key), (member))

#define SERIALIZE_UNSIGNED_INTEGER_FIELD(obj, key, member)                     \
  (obj)->set_integer ((key), (member))

#define SERIALIZE_BOOLEAN_FIELD(obj, key, member)                              \
  (obj)->set_bool ((key), (member))

#define SERIALIZE_STRING_FIELD(obj, key, member)                               \
  (obj)->set_string ((key), (member))

#define SERIALIZE_OBJECT(obj, key, member, serialize_func)                     \
  {                                                                            \
    auto field_obj = serialize_object_helper ((member), (serialize_func));     \
    if (field_obj)                                                             \
      (obj)->set ((key), std::move (field_obj));                               \
  }

#define SERIALIZE_ARRAY_FIELD(obj, key, member, size, serialize_func)          \
  {                                                                            \
    auto field_array = std::make_unique<json::array> ();                       \
    for (size_t i = 0; i < (size); ++i)                                        \
      {                                                                        \
	auto element_obj = serialize_func ((member)[i]);                       \
	if (element_obj)                                                       \
	  field_array->append (std::move (element_obj));                       \
      }                                                                        \
    (obj)->set ((key), std::move (field_array));                               \
  }

#define SERIALIZE_ENUM_FIELD(obj, key, member, mappings)                       \
  (obj)->set_string ((key), serialize_enum ((member), (mappings),              \
					    sizeof (mappings)                  \
					      / sizeof (mappings[0])))

/* Type alias for serialize function pointer.  */
template <typename T>
using serialize_func_type = std::unique_ptr<json::object> (*) (
  const typename std::remove_pointer<T>::type &);

/* Serialize JSON object from non-pointer members.  */
template <typename T>
static typename std::enable_if<!std::is_pointer<T>::value,
			       std::unique_ptr<json::object>>::type
serialize_object_helper (const T &member, serialize_func_type<T> serialize_func)
{
  return serialize_func (member);
}

/* Serialize JSON object from pointer members.  */
template <typename T>
static typename std::enable_if<std::is_pointer<T>::value,
			       std::unique_ptr<json::object>>::type
serialize_object_helper (const T &member, serialize_func_type<T> serialize_func)
{
  if (member)
    return serialize_func (*member);
  return std::make_unique<json::object> ();
}

/* Mapping structure for enum-to-string conversion.  */
template <typename EnumType> struct enum_mapping
{
  const char *name;
  EnumType value;
};

static const enum_mapping<tune_params::aarch64_autoprefetch_model>
  autoprefetcher_model_mappings[]
  = {{"AUTOPREFETCHER_OFF", tune_params::AUTOPREFETCHER_OFF},
     {"AUTOPREFETCHER_WEAK", tune_params::AUTOPREFETCHER_WEAK},
     {"AUTOPREFETCHER_STRONG", tune_params::AUTOPREFETCHER_STRONG}};

static const enum_mapping<aarch64_ldp_stp_policy> ldp_policy_model_mappings[]
  = {{"AARCH64_LDP_STP_POLICY_DEFAULT", AARCH64_LDP_STP_POLICY_DEFAULT},
     {"AARCH64_LDP_STP_POLICY_ALIGNED", AARCH64_LDP_STP_POLICY_ALIGNED},
     {"AARCH64_LDP_STP_POLICY_ALWAYS", AARCH64_LDP_STP_POLICY_ALWAYS},
     {"AARCH64_LDP_STP_POLICY_NEVER", AARCH64_LDP_STP_POLICY_NEVER}};

static const enum_mapping<aarch64_ldp_stp_policy> stp_policy_model_mappings[]
  = {{"AARCH64_LDP_STP_POLICY_DEFAULT", AARCH64_LDP_STP_POLICY_DEFAULT},
     {"AARCH64_LDP_STP_POLICY_ALIGNED", AARCH64_LDP_STP_POLICY_ALIGNED},
     {"AARCH64_LDP_STP_POLICY_ALWAYS", AARCH64_LDP_STP_POLICY_ALWAYS},
     {"AARCH64_LDP_STP_POLICY_NEVER", AARCH64_LDP_STP_POLICY_NEVER}};

/* Convert enum value to string using enum-to-string mappings.  */
template <typename EnumType>
static const char *
serialize_enum (EnumType enum_value, const enum_mapping<EnumType> *mappings,
		size_t num_mappings)
{
  for (size_t i = 0; i < num_mappings; ++i)
    if (enum_value == mappings[i].value)
      return mappings[i].name;
  return mappings[0].name;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_insn_extra_cost_alu (const T &alu)
{
  auto alu_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (alu_obj, "arith", alu.arith);
  SERIALIZE_INTEGER_FIELD (alu_obj, "logical", alu.logical);
  SERIALIZE_INTEGER_FIELD (alu_obj, "shift", alu.shift);
  SERIALIZE_INTEGER_FIELD (alu_obj, "shift_reg", alu.shift_reg);
  SERIALIZE_INTEGER_FIELD (alu_obj, "arith_shift", alu.arith_shift);
  SERIALIZE_INTEGER_FIELD (alu_obj, "arith_shift_reg", alu.arith_shift_reg);
  SERIALIZE_INTEGER_FIELD (alu_obj, "log_shift", alu.log_shift);
  SERIALIZE_INTEGER_FIELD (alu_obj, "log_shift_reg", alu.log_shift_reg);
  SERIALIZE_INTEGER_FIELD (alu_obj, "extend", alu.extend);
  SERIALIZE_INTEGER_FIELD (alu_obj, "extend_arith", alu.extend_arith);
  SERIALIZE_INTEGER_FIELD (alu_obj, "bfi", alu.bfi);
  SERIALIZE_INTEGER_FIELD (alu_obj, "bfx", alu.bfx);
  SERIALIZE_INTEGER_FIELD (alu_obj, "clz", alu.clz);
  SERIALIZE_INTEGER_FIELD (alu_obj, "rev", alu.rev);
  SERIALIZE_INTEGER_FIELD (alu_obj, "non_exec", alu.non_exec);
  SERIALIZE_BOOLEAN_FIELD (alu_obj, "non_exec_costs_exec",
			   alu.non_exec_costs_exec);

  return alu_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_insn_extra_cost_mult_element (const T &mult_element)
{
  auto mult_element_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (mult_element_obj, "simple", mult_element.simple);
  SERIALIZE_INTEGER_FIELD (mult_element_obj, "flag_setting",
			   mult_element.flag_setting);
  SERIALIZE_INTEGER_FIELD (mult_element_obj, "extend", mult_element.extend);
  SERIALIZE_INTEGER_FIELD (mult_element_obj, "add", mult_element.add);
  SERIALIZE_INTEGER_FIELD (mult_element_obj, "extend_add",
			   mult_element.extend_add);
  SERIALIZE_INTEGER_FIELD (mult_element_obj, "idiv", mult_element.idiv);

  return mult_element_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_insn_extra_cost_ldst (const T &ldst)
{
  auto ldst_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (ldst_obj, "load", ldst.load);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "load_sign_extend", ldst.load_sign_extend);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "ldrd", ldst.ldrd);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "ldm_1st", ldst.ldm_1st);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "ldm_regs_per_insn_1st",
			   ldst.ldm_regs_per_insn_1st);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "ldm_regs_per_insn_subsequent",
			   ldst.ldm_regs_per_insn_subsequent);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "loadf", ldst.loadf);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "loadd", ldst.loadd);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "load_unaligned", ldst.load_unaligned);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "store", ldst.store);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "strd", ldst.strd);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "stm_1st", ldst.stm_1st);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "stm_regs_per_insn_1st",
			   ldst.stm_regs_per_insn_1st);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "stm_regs_per_insn_subsequent",
			   ldst.stm_regs_per_insn_subsequent);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "storef", ldst.storef);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "stored", ldst.stored);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "store_unaligned", ldst.store_unaligned);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "loadv", ldst.loadv);
  SERIALIZE_INTEGER_FIELD (ldst_obj, "storev", ldst.storev);

  return ldst_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_insn_extra_cost_fp_element (const T &fp_element)
{
  auto fp_element_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (fp_element_obj, "div", fp_element.div);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "mult", fp_element.mult);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "mult_addsub",
			   fp_element.mult_addsub);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "fma", fp_element.fma);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "addsub", fp_element.addsub);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "fpconst", fp_element.fpconst);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "neg", fp_element.neg);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "compare", fp_element.compare);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "widen", fp_element.widen);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "narrow", fp_element.narrow);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "toint", fp_element.toint);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "fromint", fp_element.fromint);
  SERIALIZE_INTEGER_FIELD (fp_element_obj, "roundint", fp_element.roundint);

  return fp_element_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_insn_extra_cost_vect (const T &vect)
{
  auto vect_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (vect_obj, "alu", vect.alu);
  SERIALIZE_INTEGER_FIELD (vect_obj, "mult", vect.mult);
  SERIALIZE_INTEGER_FIELD (vect_obj, "movi", vect.movi);
  SERIALIZE_INTEGER_FIELD (vect_obj, "dup", vect.dup);
  SERIALIZE_INTEGER_FIELD (vect_obj, "extract", vect.extract);

  return vect_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_addr_cost_addr_scale_costs (const T &addr_scale_costs)
{
  auto addr_scale_costs_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (addr_scale_costs_obj, "hi", addr_scale_costs.hi);
  SERIALIZE_INTEGER_FIELD (addr_scale_costs_obj, "si", addr_scale_costs.si);
  SERIALIZE_INTEGER_FIELD (addr_scale_costs_obj, "di", addr_scale_costs.di);
  SERIALIZE_INTEGER_FIELD (addr_scale_costs_obj, "ti", addr_scale_costs.ti);

  return addr_scale_costs_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_regmove_cost (const T &regmove_cost)
{
  auto regmove_cost_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (regmove_cost_obj, "GP2GP", regmove_cost.GP2GP);
  SERIALIZE_INTEGER_FIELD (regmove_cost_obj, "GP2FP", regmove_cost.GP2FP);
  SERIALIZE_INTEGER_FIELD (regmove_cost_obj, "FP2GP", regmove_cost.FP2GP);
  SERIALIZE_INTEGER_FIELD (regmove_cost_obj, "FP2FP", regmove_cost.FP2FP);

  return regmove_cost_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs_advsimd (const T &advsimd)
{
  auto advsimd_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (advsimd_obj, "int_stmt_cost", advsimd.int_stmt_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "fp_stmt_cost", advsimd.fp_stmt_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "ld2_st2_permute_cost",
			   advsimd.ld2_st2_permute_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "ld3_st3_permute_cost",
			   advsimd.ld3_st3_permute_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "ld4_st4_permute_cost",
			   advsimd.ld4_st4_permute_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "permute_cost", advsimd.permute_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_i8_cost", advsimd.reduc_i8_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_i16_cost",
			   advsimd.reduc_i16_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_i32_cost",
			   advsimd.reduc_i32_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_i64_cost",
			   advsimd.reduc_i64_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_f16_cost",
			   advsimd.reduc_f16_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_f32_cost",
			   advsimd.reduc_f32_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "reduc_f64_cost",
			   advsimd.reduc_f64_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "store_elt_extra_cost",
			   advsimd.store_elt_extra_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "vec_to_scalar_cost",
			   advsimd.vec_to_scalar_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "scalar_to_vec_cost",
			   advsimd.scalar_to_vec_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "align_load_cost",
			   advsimd.align_load_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "unalign_load_cost",
			   advsimd.unalign_load_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "unalign_store_cost",
			   advsimd.unalign_store_cost);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "store_cost", advsimd.store_cost);

  return advsimd_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs_sve (const T &sve)
{
  auto sve_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (sve_obj, "clast_cost", sve.clast_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "fadda_f16_cost", sve.fadda_f16_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "fadda_f32_cost", sve.fadda_f32_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "fadda_f64_cost", sve.fadda_f64_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "gather_load_x32_cost",
			   sve.gather_load_x32_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "gather_load_x64_cost",
			   sve.gather_load_x64_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "gather_load_x32_init_cost",
			   sve.gather_load_x32_init_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "gather_load_x64_init_cost",
			   sve.gather_load_x64_init_cost);
  SERIALIZE_INTEGER_FIELD (sve_obj, "scatter_store_elt_cost",
			   sve.scatter_store_elt_cost);

  return sve_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs_issue_info_scalar (const T &scalar)
{
  auto scalar_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (scalar_obj, "loads_stores_per_cycle",
			   scalar.loads_stores_per_cycle);
  SERIALIZE_INTEGER_FIELD (scalar_obj, "stores_per_cycle",
			   scalar.stores_per_cycle);
  SERIALIZE_INTEGER_FIELD (scalar_obj, "general_ops_per_cycle",
			   scalar.general_ops_per_cycle);
  SERIALIZE_INTEGER_FIELD (scalar_obj, "fp_simd_load_general_ops",
			   scalar.fp_simd_load_general_ops);
  SERIALIZE_INTEGER_FIELD (scalar_obj, "fp_simd_store_general_ops",
			   scalar.fp_simd_store_general_ops);

  return scalar_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs_issue_info_advsimd (const T &advsimd)
{
  auto advsimd_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (advsimd_obj, "loads_stores_per_cycle",
			   advsimd.loads_stores_per_cycle);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "stores_per_cycle",
			   advsimd.stores_per_cycle);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "general_ops_per_cycle",
			   advsimd.general_ops_per_cycle);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "fp_simd_load_general_ops",
			   advsimd.fp_simd_load_general_ops);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "fp_simd_store_general_ops",
			   advsimd.fp_simd_store_general_ops);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "ld2_st2_general_ops",
			   advsimd.ld2_st2_general_ops);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "ld3_st3_general_ops",
			   advsimd.ld3_st3_general_ops);
  SERIALIZE_INTEGER_FIELD (advsimd_obj, "ld4_st4_general_ops",
			   advsimd.ld4_st4_general_ops);

  return advsimd_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs_issue_info_sve (const T &sve)
{
  auto sve_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (sve_obj, "loads_stores_per_cycle",
			   sve.loads_stores_per_cycle);
  SERIALIZE_INTEGER_FIELD (sve_obj, "stores_per_cycle", sve.stores_per_cycle);
  SERIALIZE_INTEGER_FIELD (sve_obj, "general_ops_per_cycle",
			   sve.general_ops_per_cycle);
  SERIALIZE_INTEGER_FIELD (sve_obj, "fp_simd_load_general_ops",
			   sve.fp_simd_load_general_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "fp_simd_store_general_ops",
			   sve.fp_simd_store_general_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "ld2_st2_general_ops",
			   sve.ld2_st2_general_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "ld3_st3_general_ops",
			   sve.ld3_st3_general_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "ld4_st4_general_ops",
			   sve.ld4_st4_general_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "pred_ops_per_cycle",
			   sve.pred_ops_per_cycle);
  SERIALIZE_INTEGER_FIELD (sve_obj, "while_pred_ops", sve.while_pred_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "int_cmp_pred_ops", sve.int_cmp_pred_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "fp_cmp_pred_ops", sve.fp_cmp_pred_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "gather_scatter_pair_general_ops",
			   sve.gather_scatter_pair_general_ops);
  SERIALIZE_INTEGER_FIELD (sve_obj, "gather_scatter_pair_pred_ops",
			   sve.gather_scatter_pair_pred_ops);

  return sve_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_branch_costs (const T &branch_costs)
{
  auto branch_costs_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (branch_costs_obj, "predictable",
			   branch_costs.predictable);
  SERIALIZE_INTEGER_FIELD (branch_costs_obj, "unpredictable",
			   branch_costs.unpredictable);

  return branch_costs_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_approx_modes (const T &approx_modes)
{
  auto approx_modes_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (approx_modes_obj, "division", approx_modes.division);
  SERIALIZE_INTEGER_FIELD (approx_modes_obj, "sqrt", approx_modes.sqrt);
  SERIALIZE_INTEGER_FIELD (approx_modes_obj, "recip_sqrt",
			   approx_modes.recip_sqrt);

  return approx_modes_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_memmov_cost (const T &memmov_cost)
{
  auto memmov_cost_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (memmov_cost_obj, "load_int", memmov_cost.load_int);
  SERIALIZE_INTEGER_FIELD (memmov_cost_obj, "store_int", memmov_cost.store_int);
  SERIALIZE_INTEGER_FIELD (memmov_cost_obj, "load_fp", memmov_cost.load_fp);
  SERIALIZE_INTEGER_FIELD (memmov_cost_obj, "store_fp", memmov_cost.store_fp);
  SERIALIZE_INTEGER_FIELD (memmov_cost_obj, "load_pred", memmov_cost.load_pred);
  SERIALIZE_INTEGER_FIELD (memmov_cost_obj, "store_pred",
			   memmov_cost.store_pred);

  return memmov_cost_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_prefetch (const T &prefetch)
{
  auto prefetch_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (prefetch_obj, "num_slots", prefetch.num_slots);
  SERIALIZE_INTEGER_FIELD (prefetch_obj, "l1_cache_size",
			   prefetch.l1_cache_size);
  SERIALIZE_INTEGER_FIELD (prefetch_obj, "l1_cache_line_size",
			   prefetch.l1_cache_line_size);
  SERIALIZE_INTEGER_FIELD (prefetch_obj, "l2_cache_size",
			   prefetch.l2_cache_size);
  SERIALIZE_BOOLEAN_FIELD (prefetch_obj, "prefetch_dynamic_strides",
			   prefetch.prefetch_dynamic_strides);
  SERIALIZE_INTEGER_FIELD (prefetch_obj, "minimum_stride",
			   prefetch.minimum_stride);
  SERIALIZE_INTEGER_FIELD (prefetch_obj, "default_opt_level",
			   prefetch.default_opt_level);

  return prefetch_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_insn_extra_cost (const T &insn_extra_cost)
{
  auto insn_extra_cost_obj = std::make_unique<json::object> ();

  SERIALIZE_OBJECT (insn_extra_cost_obj, "alu", insn_extra_cost.alu,
		    serialize_insn_extra_cost_alu);
  SERIALIZE_ARRAY_FIELD (insn_extra_cost_obj, "mult", insn_extra_cost.mult, 2,
			 serialize_insn_extra_cost_mult_element);
  SERIALIZE_OBJECT (insn_extra_cost_obj, "ldst", insn_extra_cost.ldst,
		    serialize_insn_extra_cost_ldst);
  SERIALIZE_ARRAY_FIELD (insn_extra_cost_obj, "fp", insn_extra_cost.fp, 2,
			 serialize_insn_extra_cost_fp_element);
  SERIALIZE_OBJECT (insn_extra_cost_obj, "vect", insn_extra_cost.vect,
		    serialize_insn_extra_cost_vect);

  return insn_extra_cost_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_addr_cost (const T &addr_cost)
{
  auto addr_cost_obj = std::make_unique<json::object> ();

  SERIALIZE_OBJECT (addr_cost_obj, "addr_scale_costs",
		    addr_cost.addr_scale_costs,
		    serialize_addr_cost_addr_scale_costs);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "pre_modify", addr_cost.pre_modify);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "post_modify", addr_cost.post_modify);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "post_modify_ld3_st3",
			   addr_cost.post_modify_ld3_st3);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "post_modify_ld4_st4",
			   addr_cost.post_modify_ld4_st4);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "register_offset",
			   addr_cost.register_offset);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "register_sextend",
			   addr_cost.register_sextend);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "register_zextend",
			   addr_cost.register_zextend);
  SERIALIZE_INTEGER_FIELD (addr_cost_obj, "imm_offset", addr_cost.imm_offset);

  return addr_cost_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs_issue_info (const T &issue_info)
{
  auto issue_info_obj = std::make_unique<json::object> ();

  SERIALIZE_OBJECT (issue_info_obj, "scalar", issue_info.scalar,
		    serialize_vec_costs_issue_info_scalar);
  SERIALIZE_OBJECT (issue_info_obj, "advsimd", issue_info.advsimd,
		    serialize_vec_costs_issue_info_advsimd);
  SERIALIZE_OBJECT (issue_info_obj, "sve", issue_info.sve,
		    serialize_vec_costs_issue_info_sve);

  return issue_info_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_vec_costs (const T &vec_costs)
{
  auto vec_costs_obj = std::make_unique<json::object> ();

  SERIALIZE_INTEGER_FIELD (vec_costs_obj, "scalar_int_stmt_cost",
			   vec_costs.scalar_int_stmt_cost);
  SERIALIZE_INTEGER_FIELD (vec_costs_obj, "scalar_fp_stmt_cost",
			   vec_costs.scalar_fp_stmt_cost);
  SERIALIZE_INTEGER_FIELD (vec_costs_obj, "scalar_load_cost",
			   vec_costs.scalar_load_cost);
  SERIALIZE_INTEGER_FIELD (vec_costs_obj, "scalar_store_cost",
			   vec_costs.scalar_store_cost);
  SERIALIZE_INTEGER_FIELD (vec_costs_obj, "cond_taken_branch_cost",
			   vec_costs.cond_taken_branch_cost);
  SERIALIZE_INTEGER_FIELD (vec_costs_obj, "cond_not_taken_branch_cost",
			   vec_costs.cond_not_taken_branch_cost);
  SERIALIZE_OBJECT (vec_costs_obj, "advsimd", vec_costs.advsimd,
		    serialize_vec_costs_advsimd);
  SERIALIZE_OBJECT (vec_costs_obj, "sve", vec_costs.sve,
		    serialize_vec_costs_sve);
  SERIALIZE_OBJECT (vec_costs_obj, "issue_info", vec_costs.issue_info,
		    serialize_vec_costs_issue_info);

  return vec_costs_obj;
}

template <typename T>
static std::unique_ptr<json::object>
serialize_tunings (const T &tunings)
{
  auto tunings_obj = std::make_unique<json::object> ();

  SERIALIZE_OBJECT (tunings_obj, "insn_extra_cost", tunings.insn_extra_cost,
		    serialize_insn_extra_cost);
  SERIALIZE_OBJECT (tunings_obj, "addr_cost", tunings.addr_cost,
		    serialize_addr_cost);
  SERIALIZE_OBJECT (tunings_obj, "regmove_cost", tunings.regmove_cost,
		    serialize_regmove_cost);
  SERIALIZE_OBJECT (tunings_obj, "vec_costs", tunings.vec_costs,
		    serialize_vec_costs);
  SERIALIZE_OBJECT (tunings_obj, "branch_costs", tunings.branch_costs,
		    serialize_branch_costs);
  SERIALIZE_OBJECT (tunings_obj, "approx_modes", tunings.approx_modes,
		    serialize_approx_modes);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "sve_width", tunings.sve_width);
  SERIALIZE_OBJECT (tunings_obj, "memmov_cost", tunings.memmov_cost,
		    serialize_memmov_cost);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "issue_rate", tunings.issue_rate);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "fusible_ops", tunings.fusible_ops);
  SERIALIZE_STRING_FIELD (tunings_obj, "function_align",
			  tunings.function_align);
  SERIALIZE_STRING_FIELD (tunings_obj, "jump_align", tunings.jump_align);
  SERIALIZE_STRING_FIELD (tunings_obj, "loop_align", tunings.loop_align);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "int_reassoc_width",
			   tunings.int_reassoc_width);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "fp_reassoc_width",
			   tunings.fp_reassoc_width);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "fma_reassoc_width",
			   tunings.fma_reassoc_width);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "vec_reassoc_width",
			   tunings.vec_reassoc_width);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "min_div_recip_mul_sf",
			   tunings.min_div_recip_mul_sf);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "min_div_recip_mul_df",
			   tunings.min_div_recip_mul_df);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "max_case_values",
			   tunings.max_case_values);
  SERIALIZE_ENUM_FIELD (tunings_obj, "autoprefetcher_model",
			tunings.autoprefetcher_model,
			autoprefetcher_model_mappings);
  SERIALIZE_INTEGER_FIELD (tunings_obj, "extra_tuning_flags",
			   tunings.extra_tuning_flags);
  SERIALIZE_OBJECT (tunings_obj, "prefetch", tunings.prefetch,
		    serialize_prefetch);
  SERIALIZE_ENUM_FIELD (tunings_obj, "ldp_policy_model",
			tunings.ldp_policy_model, ldp_policy_model_mappings);
  SERIALIZE_ENUM_FIELD (tunings_obj, "stp_policy_model",
			tunings.stp_policy_model, stp_policy_model_mappings);

  return tunings_obj;
}

/* Print tune_params structure to JSON file.  */
void
aarch64_print_tune_params (const tune_params &params, const char *filename)
{
  /* Use default filename if none provided or empty string given.  */
  const char *output_filename = filename;
  if (!output_filename || *output_filename == '\0')
    output_filename = "aarch64-tuning.json";

  auto aarch64_tune_params_json = std::make_unique<json::object> ();

  auto metadata = std::make_unique<json::object> ();
  metadata->set_integer ("gcc_version", GCC_major_version);
  aarch64_tune_params_json->set ("metadata", std::move (metadata));

  aarch64_tune_params_json->set ("tune_params", serialize_tunings (params));

  pretty_printer pp;
  aarch64_tune_params_json->print (&pp, true);

  FILE *outputFile = fopen (output_filename, "w");
  if (!outputFile)
    {
      error ("Error opening file %s", output_filename);
      return;
    }

  fprintf (outputFile, "%s", pp_formatted_text (&pp));
  fclose (outputFile);
  return;
}