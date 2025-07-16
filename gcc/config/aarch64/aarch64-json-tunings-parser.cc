/* Routines to parse the AArch64 tuning parameters from a JSON file.
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

#define INCLUDE_STRING
#define INCLUDE_VECTOR
#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "json-parsing.h"
#include "aarch64-json-schema.h"
#include "aarch64-json-tunings-parser.h"
#include "aarch64-protos.h"
#include "config/arm/aarch-common-protos.h"
#include "selftest.h"
#include "version.h"

#define PARSE_INTEGER_FIELD(obj, key, member)                                  \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_integer (val);                                          \
  }

#define PARSE_UNSIGNED_INTEGER_FIELD(obj, key, member)                         \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_unsigned_integer (val);                                 \
  }

#define PARSE_BOOLEAN_FIELD(obj, key, member)                                  \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_boolean (val);                                          \
  }

#define PARSE_STRING_FIELD(obj, key, member)                                   \
  {                                                                            \
    const json::value *val = obj->get (key);                                   \
    if (val)                                                                   \
      member = extract_string (val);                                           \
  }

#define PARSE_OBJECT(obj, key, member, parse_func)                             \
  {                                                                            \
    const json::value *field_value = obj->get (key);                           \
    if (field_value)                                                           \
      if (auto *field_obj = dyn_cast<const json::object *> (field_value))      \
	parse_object_helper (field_obj, (member), (parse_func));               \
  }

#define PARSE_ARRAY_FIELD(obj, key, member, parse_func)                        \
  {                                                                            \
    const json::value *field_value = obj->get (key);                           \
    if (field_value)                                                           \
      if (auto *field_array = dyn_cast<const json::array *> (field_value))     \
	for (size_t i = 0; i < field_array->size (); ++i)                      \
	  {                                                                    \
	    const json::value *elem = field_array->get (i);                    \
	    if (elem)                                                          \
	      if (auto *array_obj = dyn_cast<const json::object *> (elem))     \
		parse_func (array_obj, member[i]);                             \
	  }                                                                    \
  }

#define PARSE_ENUM_FIELD(obj, key, member, mappings)                           \
  parse_enum_field (obj, key, member, mappings,                                \
		    sizeof (mappings) / sizeof (mappings[0]))

/* Type alias for parse function pointer.  */
template <typename T>
using parse_func_type
  = void (*) (const json::object *,
	      std::remove_const_t<std::remove_pointer_t<T>> &);

/* Parse JSON object into non-pointer member type.  */
template <typename T>
static std::enable_if_t<!std::is_pointer<T>::value>
parse_object_helper (const json::object *field_obj, T &member,
		     parse_func_type<T> parse_func)
{
  parse_func (field_obj, member);
}

/* Parse JSON object into a const pointer member by creating a temp copy.  */
template <typename T>
static std::enable_if_t<std::is_pointer<T>::value
			&& std::is_const<std::remove_pointer_t<T>>::value>
parse_object_helper (const json::object *field_obj, T &member,
		     parse_func_type<T> parse_func)
{
  if (!member)
    return;

  /* Use static storage for the non-const copy.
     This works because tune_params does not have nested structures of the
     same type, but has room for errors if we end up having pointers to the
     same structure at some point.  */
  static bool already_initialized = false;
  if (already_initialized)
    {
      error ("static storage conflict - multiple pointer members of the "
	     "same type cannot be parsed");
      return;
    }
  already_initialized = true;
  using NonConstType = std::remove_const_t<std::remove_pointer_t<T>>;
  static NonConstType new_obj = *member;
  parse_func (field_obj, new_obj);
  member = &new_obj;
}

/* Extract string value from JSON, returning allocated C string.  */
char *
extract_string (const json::value *val)
{
  if (auto *string_val = dyn_cast<const json::string *> (val))
    return xstrdup (string_val->get_string ());
  warning (0, "expected a string but got something else or NULL");
  return nullptr;
}

/* Extract signed integer value from JSON.  */
int
extract_integer (const json::value *val)
{
  if (auto *int_val = dyn_cast<const json::integer_number *> (val))
    {
      long value = int_val->get ();
      gcc_assert (value >= INT_MIN && value <= INT_MAX);
      return static_cast<int> (value);
    }
  warning (0, "expected an integer value but got something else or NULL");
  return 0;
}

/* Extract unsigned integer value from JSON.  */
unsigned int
extract_unsigned_integer (const json::value *val)
{
  if (auto *int_val = dyn_cast<const json::integer_number *> (val))
    {
      long value = int_val->get ();
      gcc_assert (value >= 0 && value <= UINT_MAX);
      return static_cast<unsigned int> (value);
    }
  warning (0,
	   "expected an unsigned integer value but got something else or NULL");
  return 0;
}

/* Extract boolean value from JSON literal.  */
bool
extract_boolean (const json::value *val)
{
  if (auto *literal_val = dyn_cast<const json::literal *> (val))
    {
      json::kind kind = literal_val->get_kind ();
      if (kind == json::JSON_TRUE || kind == json::JSON_FALSE)
	return (kind == json::JSON_TRUE);
    }
  warning (0, "expected a boolean value but got something else or NULL");
  return false;
}

template <typename EnumType> struct enum_mapping
{
  const char *name;
  EnumType value;
};

/* Parse JSON string field into enum value using string-to-enum mappings.  */
template <typename EnumType>
static void
parse_enum_field (const json::object *jo, const std::string &key,
		  EnumType &enum_var, const enum_mapping<EnumType> *mappings,
		  size_t num_mappings)
{
  const json::value *field_value = jo->get (key.c_str ());
  if (!field_value)
    return;

  auto *string_val = dyn_cast<const json::string *> (field_value);
  if (!string_val)
    {
      warning (0, "expected string for enum field %s", key.c_str ());
      enum_var = mappings[0].value;
      return;
    }

  const char *field_string = string_val->get_string ();
  for (size_t i = 0; i < num_mappings; ++i)
    {
      if (strcmp (field_string, mappings[i].name) == 0)
	{
	  enum_var = mappings[i].value;
	  return;
	}
    }

  warning (0, "%s not recognized, defaulting to %qs", key.c_str (),
	   mappings[0].name);
  enum_var = mappings[0].value;
}

/* Enum mappings for known tuning parameter enums.  */
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

template <typename T>
static void
parse_insn_extra_cost_alu (const json::object *jo, T &alu)
{
  PARSE_INTEGER_FIELD (jo, "arith", alu.arith);
  PARSE_INTEGER_FIELD (jo, "logical", alu.logical);
  PARSE_INTEGER_FIELD (jo, "shift", alu.shift);
  PARSE_INTEGER_FIELD (jo, "shift_reg", alu.shift_reg);
  PARSE_INTEGER_FIELD (jo, "arith_shift", alu.arith_shift);
  PARSE_INTEGER_FIELD (jo, "arith_shift_reg", alu.arith_shift_reg);
  PARSE_INTEGER_FIELD (jo, "log_shift", alu.log_shift);
  PARSE_INTEGER_FIELD (jo, "log_shift_reg", alu.log_shift_reg);
  PARSE_INTEGER_FIELD (jo, "extend", alu.extend);
  PARSE_INTEGER_FIELD (jo, "extend_arith", alu.extend_arith);
  PARSE_INTEGER_FIELD (jo, "bfi", alu.bfi);
  PARSE_INTEGER_FIELD (jo, "bfx", alu.bfx);
  PARSE_INTEGER_FIELD (jo, "clz", alu.clz);
  PARSE_INTEGER_FIELD (jo, "rev", alu.rev);
  PARSE_INTEGER_FIELD (jo, "non_exec", alu.non_exec);
  PARSE_BOOLEAN_FIELD (jo, "non_exec_costs_exec", alu.non_exec_costs_exec);
}

template <typename T>
static void
parse_insn_extra_cost_mult_element (const json::object *jo, T &mult_element)
{
  PARSE_INTEGER_FIELD (jo, "simple", mult_element.simple);
  PARSE_INTEGER_FIELD (jo, "flag_setting", mult_element.flag_setting);
  PARSE_INTEGER_FIELD (jo, "extend", mult_element.extend);
  PARSE_INTEGER_FIELD (jo, "add", mult_element.add);
  PARSE_INTEGER_FIELD (jo, "extend_add", mult_element.extend_add);
  PARSE_INTEGER_FIELD (jo, "idiv", mult_element.idiv);
}

template <typename T>
static void
parse_insn_extra_cost_ldst (const json::object *jo, T &ldst)
{
  PARSE_INTEGER_FIELD (jo, "load", ldst.load);
  PARSE_INTEGER_FIELD (jo, "load_sign_extend", ldst.load_sign_extend);
  PARSE_INTEGER_FIELD (jo, "ldrd", ldst.ldrd);
  PARSE_INTEGER_FIELD (jo, "ldm_1st", ldst.ldm_1st);
  PARSE_INTEGER_FIELD (jo, "ldm_regs_per_insn_1st", ldst.ldm_regs_per_insn_1st);
  PARSE_INTEGER_FIELD (jo, "ldm_regs_per_insn_subsequent",
		       ldst.ldm_regs_per_insn_subsequent);
  PARSE_INTEGER_FIELD (jo, "loadf", ldst.loadf);
  PARSE_INTEGER_FIELD (jo, "loadd", ldst.loadd);
  PARSE_INTEGER_FIELD (jo, "load_unaligned", ldst.load_unaligned);
  PARSE_INTEGER_FIELD (jo, "store", ldst.store);
  PARSE_INTEGER_FIELD (jo, "strd", ldst.strd);
  PARSE_INTEGER_FIELD (jo, "stm_1st", ldst.stm_1st);
  PARSE_INTEGER_FIELD (jo, "stm_regs_per_insn_1st", ldst.stm_regs_per_insn_1st);
  PARSE_INTEGER_FIELD (jo, "stm_regs_per_insn_subsequent",
		       ldst.stm_regs_per_insn_subsequent);
  PARSE_INTEGER_FIELD (jo, "storef", ldst.storef);
  PARSE_INTEGER_FIELD (jo, "stored", ldst.stored);
  PARSE_INTEGER_FIELD (jo, "store_unaligned", ldst.store_unaligned);
  PARSE_INTEGER_FIELD (jo, "loadv", ldst.loadv);
  PARSE_INTEGER_FIELD (jo, "storev", ldst.storev);
}

template <typename T>
static void
parse_insn_extra_cost_fp_element (const json::object *jo, T &fp_element)
{
  PARSE_INTEGER_FIELD (jo, "div", fp_element.div);
  PARSE_INTEGER_FIELD (jo, "mult", fp_element.mult);
  PARSE_INTEGER_FIELD (jo, "mult_addsub", fp_element.mult_addsub);
  PARSE_INTEGER_FIELD (jo, "fma", fp_element.fma);
  PARSE_INTEGER_FIELD (jo, "addsub", fp_element.addsub);
  PARSE_INTEGER_FIELD (jo, "fpconst", fp_element.fpconst);
  PARSE_INTEGER_FIELD (jo, "neg", fp_element.neg);
  PARSE_INTEGER_FIELD (jo, "compare", fp_element.compare);
  PARSE_INTEGER_FIELD (jo, "widen", fp_element.widen);
  PARSE_INTEGER_FIELD (jo, "narrow", fp_element.narrow);
  PARSE_INTEGER_FIELD (jo, "toint", fp_element.toint);
  PARSE_INTEGER_FIELD (jo, "fromint", fp_element.fromint);
  PARSE_INTEGER_FIELD (jo, "roundint", fp_element.roundint);
}

template <typename T>
static void
parse_insn_extra_cost_vect (const json::object *jo, T &vect)
{
  PARSE_INTEGER_FIELD (jo, "alu", vect.alu);
  PARSE_INTEGER_FIELD (jo, "mult", vect.mult);
  PARSE_INTEGER_FIELD (jo, "movi", vect.movi);
  PARSE_INTEGER_FIELD (jo, "dup", vect.dup);
  PARSE_INTEGER_FIELD (jo, "extract", vect.extract);
}

template <typename T>
static void
parse_addr_cost_addr_scale_costs (const json::object *jo, T &addr_scale_costs)
{
  PARSE_INTEGER_FIELD (jo, "hi", addr_scale_costs.hi);
  PARSE_INTEGER_FIELD (jo, "si", addr_scale_costs.si);
  PARSE_INTEGER_FIELD (jo, "di", addr_scale_costs.di);
  PARSE_INTEGER_FIELD (jo, "ti", addr_scale_costs.ti);
}

template <typename T>
static void
parse_regmove_cost (const json::object *jo, T &regmove_cost)
{
  PARSE_INTEGER_FIELD (jo, "GP2GP", regmove_cost.GP2GP);
  PARSE_INTEGER_FIELD (jo, "GP2FP", regmove_cost.GP2FP);
  PARSE_INTEGER_FIELD (jo, "FP2GP", regmove_cost.FP2GP);
  PARSE_INTEGER_FIELD (jo, "FP2FP", regmove_cost.FP2FP);
}

template <typename T>
static void
parse_vec_costs_advsimd (const json::object *jo, T &advsimd)
{
  PARSE_INTEGER_FIELD (jo, "int_stmt_cost", advsimd.int_stmt_cost);
  PARSE_INTEGER_FIELD (jo, "fp_stmt_cost", advsimd.fp_stmt_cost);
  PARSE_INTEGER_FIELD (jo, "ld2_st2_permute_cost",
		       advsimd.ld2_st2_permute_cost);
  PARSE_INTEGER_FIELD (jo, "ld3_st3_permute_cost",
		       advsimd.ld3_st3_permute_cost);
  PARSE_INTEGER_FIELD (jo, "ld4_st4_permute_cost",
		       advsimd.ld4_st4_permute_cost);
  PARSE_INTEGER_FIELD (jo, "permute_cost", advsimd.permute_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_i8_cost", advsimd.reduc_i8_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_i16_cost", advsimd.reduc_i16_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_i32_cost", advsimd.reduc_i32_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_i64_cost", advsimd.reduc_i64_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_f16_cost", advsimd.reduc_f16_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_f32_cost", advsimd.reduc_f32_cost);
  PARSE_INTEGER_FIELD (jo, "reduc_f64_cost", advsimd.reduc_f64_cost);
  PARSE_INTEGER_FIELD (jo, "store_elt_extra_cost",
		       advsimd.store_elt_extra_cost);
  PARSE_INTEGER_FIELD (jo, "vec_to_scalar_cost", advsimd.vec_to_scalar_cost);
  PARSE_INTEGER_FIELD (jo, "scalar_to_vec_cost", advsimd.scalar_to_vec_cost);
  PARSE_INTEGER_FIELD (jo, "align_load_cost", advsimd.align_load_cost);
  PARSE_INTEGER_FIELD (jo, "unalign_load_cost", advsimd.unalign_load_cost);
  PARSE_INTEGER_FIELD (jo, "unalign_store_cost", advsimd.unalign_store_cost);
  PARSE_INTEGER_FIELD (jo, "store_cost", advsimd.store_cost);
}

template <typename T>
static void
parse_vec_costs_sve (const json::object *jo, T &sve)
{
  PARSE_INTEGER_FIELD (jo, "clast_cost", sve.clast_cost);
  PARSE_INTEGER_FIELD (jo, "fadda_f16_cost", sve.fadda_f16_cost);
  PARSE_INTEGER_FIELD (jo, "fadda_f32_cost", sve.fadda_f32_cost);
  PARSE_INTEGER_FIELD (jo, "fadda_f64_cost", sve.fadda_f64_cost);
  PARSE_INTEGER_FIELD (jo, "gather_load_x32_cost", sve.gather_load_x32_cost);
  PARSE_INTEGER_FIELD (jo, "gather_load_x64_cost", sve.gather_load_x64_cost);
  PARSE_INTEGER_FIELD (jo, "gather_load_x32_init_cost",
		       sve.gather_load_x32_init_cost);
  PARSE_INTEGER_FIELD (jo, "gather_load_x64_init_cost",
		       sve.gather_load_x64_init_cost);
  PARSE_INTEGER_FIELD (jo, "scatter_store_elt_cost",
		       sve.scatter_store_elt_cost);
}

template <typename T>
static void
parse_vec_costs_issue_info_scalar (const json::object *jo, T &scalar)
{
  PARSE_INTEGER_FIELD (jo, "loads_stores_per_cycle",
		       scalar.loads_stores_per_cycle);
  PARSE_INTEGER_FIELD (jo, "stores_per_cycle", scalar.stores_per_cycle);
  PARSE_INTEGER_FIELD (jo, "general_ops_per_cycle",
		       scalar.general_ops_per_cycle);
  PARSE_INTEGER_FIELD (jo, "fp_simd_load_general_ops",
		       scalar.fp_simd_load_general_ops);
  PARSE_INTEGER_FIELD (jo, "fp_simd_store_general_ops",
		       scalar.fp_simd_store_general_ops);
}

template <typename T>
static void
parse_vec_costs_issue_info_advsimd (const json::object *jo, T &advsimd)
{
  PARSE_INTEGER_FIELD (jo, "loads_stores_per_cycle",
		       advsimd.loads_stores_per_cycle);
  PARSE_INTEGER_FIELD (jo, "stores_per_cycle", advsimd.stores_per_cycle);
  PARSE_INTEGER_FIELD (jo, "general_ops_per_cycle",
		       advsimd.general_ops_per_cycle);
  PARSE_INTEGER_FIELD (jo, "fp_simd_load_general_ops",
		       advsimd.fp_simd_load_general_ops);
  PARSE_INTEGER_FIELD (jo, "fp_simd_store_general_ops",
		       advsimd.fp_simd_store_general_ops);
  PARSE_INTEGER_FIELD (jo, "ld2_st2_general_ops", advsimd.ld2_st2_general_ops);
  PARSE_INTEGER_FIELD (jo, "ld3_st3_general_ops", advsimd.ld3_st3_general_ops);
  PARSE_INTEGER_FIELD (jo, "ld4_st4_general_ops", advsimd.ld4_st4_general_ops);
}

template <typename T>
static void
parse_vec_costs_issue_info_sve (const json::object *jo, T &sve)
{
  PARSE_INTEGER_FIELD (jo, "loads_stores_per_cycle",
		       sve.loads_stores_per_cycle);
  PARSE_INTEGER_FIELD (jo, "stores_per_cycle", sve.stores_per_cycle);
  PARSE_INTEGER_FIELD (jo, "general_ops_per_cycle", sve.general_ops_per_cycle);
  PARSE_INTEGER_FIELD (jo, "fp_simd_load_general_ops",
		       sve.fp_simd_load_general_ops);
  PARSE_INTEGER_FIELD (jo, "fp_simd_store_general_ops",
		       sve.fp_simd_store_general_ops);
  PARSE_INTEGER_FIELD (jo, "ld2_st2_general_ops", sve.ld2_st2_general_ops);
  PARSE_INTEGER_FIELD (jo, "ld3_st3_general_ops", sve.ld3_st3_general_ops);
  PARSE_INTEGER_FIELD (jo, "ld4_st4_general_ops", sve.ld4_st4_general_ops);
  PARSE_INTEGER_FIELD (jo, "pred_ops_per_cycle", sve.pred_ops_per_cycle);
  PARSE_INTEGER_FIELD (jo, "while_pred_ops", sve.while_pred_ops);
  PARSE_INTEGER_FIELD (jo, "int_cmp_pred_ops", sve.int_cmp_pred_ops);
  PARSE_INTEGER_FIELD (jo, "fp_cmp_pred_ops", sve.fp_cmp_pred_ops);
  PARSE_INTEGER_FIELD (jo, "gather_scatter_pair_general_ops",
		       sve.gather_scatter_pair_general_ops);
  PARSE_INTEGER_FIELD (jo, "gather_scatter_pair_pred_ops",
		       sve.gather_scatter_pair_pred_ops);
}

template <typename T>
static void
parse_branch_costs (const json::object *jo, T &branch_costs)
{
  PARSE_INTEGER_FIELD (jo, "predictable", branch_costs.predictable);
  PARSE_INTEGER_FIELD (jo, "unpredictable", branch_costs.unpredictable);
}

template <typename T>
static void
parse_approx_modes (const json::object *jo, T &approx_modes)
{
  PARSE_INTEGER_FIELD (jo, "division", approx_modes.division);
  PARSE_INTEGER_FIELD (jo, "sqrt", approx_modes.sqrt);
  PARSE_INTEGER_FIELD (jo, "recip_sqrt", approx_modes.recip_sqrt);
}

template <typename T>
static void
parse_memmov_cost (const json::object *jo, T &memmov_cost)
{
  PARSE_INTEGER_FIELD (jo, "load_int", memmov_cost.load_int);
  PARSE_INTEGER_FIELD (jo, "store_int", memmov_cost.store_int);
  PARSE_INTEGER_FIELD (jo, "load_fp", memmov_cost.load_fp);
  PARSE_INTEGER_FIELD (jo, "store_fp", memmov_cost.store_fp);
  PARSE_INTEGER_FIELD (jo, "load_pred", memmov_cost.load_pred);
  PARSE_INTEGER_FIELD (jo, "store_pred", memmov_cost.store_pred);
}

template <typename T>
static void
parse_prefetch (const json::object *jo, T &prefetch)
{
  PARSE_INTEGER_FIELD (jo, "num_slots", prefetch.num_slots);
  PARSE_INTEGER_FIELD (jo, "l1_cache_size", prefetch.l1_cache_size);
  PARSE_INTEGER_FIELD (jo, "l1_cache_line_size", prefetch.l1_cache_line_size);
  PARSE_INTEGER_FIELD (jo, "l2_cache_size", prefetch.l2_cache_size);
  PARSE_BOOLEAN_FIELD (jo, "prefetch_dynamic_strides",
		       prefetch.prefetch_dynamic_strides);
  PARSE_INTEGER_FIELD (jo, "minimum_stride", prefetch.minimum_stride);
  PARSE_INTEGER_FIELD (jo, "default_opt_level", prefetch.default_opt_level);
}

template <typename T>
static void
parse_insn_extra_cost (const json::object *jo, T &insn_extra_cost)
{
  PARSE_OBJECT (jo, "alu", insn_extra_cost.alu, parse_insn_extra_cost_alu);
  PARSE_ARRAY_FIELD (jo, "mult", insn_extra_cost.mult,
		     parse_insn_extra_cost_mult_element);
  PARSE_OBJECT (jo, "ldst", insn_extra_cost.ldst, parse_insn_extra_cost_ldst);
  PARSE_ARRAY_FIELD (jo, "fp", insn_extra_cost.fp,
		     parse_insn_extra_cost_fp_element);
  PARSE_OBJECT (jo, "vect", insn_extra_cost.vect, parse_insn_extra_cost_vect);
}

template <typename T>
static void
parse_addr_cost (const json::object *jo, T &addr_cost)
{
  PARSE_OBJECT (jo, "addr_scale_costs", addr_cost.addr_scale_costs,
		parse_addr_cost_addr_scale_costs);
  PARSE_INTEGER_FIELD (jo, "pre_modify", addr_cost.pre_modify);
  PARSE_INTEGER_FIELD (jo, "post_modify", addr_cost.post_modify);
  PARSE_INTEGER_FIELD (jo, "post_modify_ld3_st3",
		       addr_cost.post_modify_ld3_st3);
  PARSE_INTEGER_FIELD (jo, "post_modify_ld4_st4",
		       addr_cost.post_modify_ld4_st4);
  PARSE_INTEGER_FIELD (jo, "register_offset", addr_cost.register_offset);
  PARSE_INTEGER_FIELD (jo, "register_sextend", addr_cost.register_sextend);
  PARSE_INTEGER_FIELD (jo, "register_zextend", addr_cost.register_zextend);
  PARSE_INTEGER_FIELD (jo, "imm_offset", addr_cost.imm_offset);
}

template <typename T>
static void
parse_vec_costs_issue_info (const json::object *jo, T &issue_info)
{
  PARSE_OBJECT (jo, "scalar", issue_info.scalar,
		parse_vec_costs_issue_info_scalar);
  PARSE_OBJECT (jo, "advsimd", issue_info.advsimd,
		parse_vec_costs_issue_info_advsimd);
  PARSE_OBJECT (jo, "sve", issue_info.sve, parse_vec_costs_issue_info_sve);
}

template <typename T>
static void
parse_vec_costs (const json::object *jo, T &vec_costs)
{
  PARSE_INTEGER_FIELD (jo, "scalar_int_stmt_cost",
		       vec_costs.scalar_int_stmt_cost);
  PARSE_INTEGER_FIELD (jo, "scalar_fp_stmt_cost",
		       vec_costs.scalar_fp_stmt_cost);
  PARSE_INTEGER_FIELD (jo, "scalar_load_cost", vec_costs.scalar_load_cost);
  PARSE_INTEGER_FIELD (jo, "scalar_store_cost", vec_costs.scalar_store_cost);
  PARSE_INTEGER_FIELD (jo, "cond_taken_branch_cost",
		       vec_costs.cond_taken_branch_cost);
  PARSE_INTEGER_FIELD (jo, "cond_not_taken_branch_cost",
		       vec_costs.cond_not_taken_branch_cost);
  PARSE_OBJECT (jo, "advsimd", vec_costs.advsimd, parse_vec_costs_advsimd);
  PARSE_OBJECT (jo, "sve", vec_costs.sve, parse_vec_costs_sve);
  PARSE_OBJECT (jo, "issue_info", vec_costs.issue_info,
		parse_vec_costs_issue_info);
}

template <typename T>
static void
parse_tunings (const json::object *jo, T &tunings)
{
  PARSE_OBJECT (jo, "insn_extra_cost", tunings.insn_extra_cost,
		parse_insn_extra_cost);
  PARSE_OBJECT (jo, "addr_cost", tunings.addr_cost, parse_addr_cost);
  PARSE_OBJECT (jo, "regmove_cost", tunings.regmove_cost, parse_regmove_cost);
  PARSE_OBJECT (jo, "vec_costs", tunings.vec_costs, parse_vec_costs);
  PARSE_OBJECT (jo, "branch_costs", tunings.branch_costs, parse_branch_costs);
  PARSE_OBJECT (jo, "approx_modes", tunings.approx_modes, parse_approx_modes);
  PARSE_INTEGER_FIELD (jo, "sve_width", tunings.sve_width);
  PARSE_OBJECT (jo, "memmov_cost", tunings.memmov_cost, parse_memmov_cost);
  PARSE_INTEGER_FIELD (jo, "issue_rate", tunings.issue_rate);
  PARSE_INTEGER_FIELD (jo, "fusible_ops", tunings.fusible_ops);
  PARSE_STRING_FIELD (jo, "function_align", tunings.function_align);
  PARSE_STRING_FIELD (jo, "jump_align", tunings.jump_align);
  PARSE_STRING_FIELD (jo, "loop_align", tunings.loop_align);
  PARSE_INTEGER_FIELD (jo, "int_reassoc_width", tunings.int_reassoc_width);
  PARSE_INTEGER_FIELD (jo, "fp_reassoc_width", tunings.fp_reassoc_width);
  PARSE_INTEGER_FIELD (jo, "fma_reassoc_width", tunings.fma_reassoc_width);
  PARSE_INTEGER_FIELD (jo, "vec_reassoc_width", tunings.vec_reassoc_width);
  PARSE_INTEGER_FIELD (jo, "min_div_recip_mul_sf",
		       tunings.min_div_recip_mul_sf);
  PARSE_INTEGER_FIELD (jo, "min_div_recip_mul_df",
		       tunings.min_div_recip_mul_df);
  PARSE_INTEGER_FIELD (jo, "max_case_values", tunings.max_case_values);
  PARSE_ENUM_FIELD (jo, "autoprefetcher_model", tunings.autoprefetcher_model,
		    autoprefetcher_model_mappings);
  PARSE_INTEGER_FIELD (jo, "extra_tuning_flags", tunings.extra_tuning_flags);
  PARSE_OBJECT (jo, "prefetch", tunings.prefetch, parse_prefetch);
  PARSE_ENUM_FIELD (jo, "ldp_policy_model", tunings.ldp_policy_model,
		    ldp_policy_model_mappings);
  PARSE_ENUM_FIELD (jo, "stp_policy_model", tunings.stp_policy_model,
		    stp_policy_model_mappings);
}

/* Validate the user provided JSON data against the present schema.
   Checks for correct types, fields, and expected format.  */
static bool
validate_and_traverse (const json::object *json_obj,
		       const json::object *schema_obj,
		       const std::string &parent_key = "")
{
  for (const auto &json_entry : json_obj->get_map ())
    {
      const std::string &key = json_entry.first;
      const json::value *json_value = json_entry.second;

      std::string full_key = parent_key.empty () ? key : parent_key + "." + key;

      const json::value *schema_value = schema_obj->get (key.c_str ());
      if (!schema_value)
	{
	  warning (0, "key %qs is not a tuning parameter, skipping",
		   full_key.c_str ());
	  continue;
	}

      if (auto *sub_schema_obj = dyn_cast<const json::object *> (schema_value))
	{
	  if (auto *sub_json_obj = dyn_cast<const json::object *> (json_value))
	    {
	      if (!validate_and_traverse (sub_json_obj, sub_schema_obj,
					  full_key))
		return false;
	    }
	  else
	    {
	      error ("key %qs expected to be an object", full_key.c_str ());
	      return false;
	    }
	}
      else if (schema_value->get_kind () == json::JSON_ARRAY)
	{
	  if (json_value->get_kind () != json::JSON_ARRAY)
	    {
	      error ("key %qs expected to be an array", full_key.c_str ());
	      return false;
	    }
	}
      else if (auto *schema_string
	       = dyn_cast<const json::string *> (schema_value))
	{
	  const char *schema_type_str = schema_string->get_string ();

	  if (strcmp (schema_type_str, "int") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_INTEGER)
		{
		  error ("key %qs expected to be an integer",
			 full_key.c_str ());
		  return false;
		}
	      // Check if the value is valid for signed integer
	      if (auto *int_val
		  = dyn_cast<const json::integer_number *> (json_value))
		{
		  long value = int_val->get ();
		  if (value > INT_MAX || value < INT_MIN)
		    {
		      error ("key %qs value %ld is out of range for %<int%> "
			     "type [%d, %d]",
			     full_key.c_str (), value, INT_MIN, INT_MAX);
		      return false;
		    }
		}
	    }
	  else if (strcmp (schema_type_str, "uint") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_INTEGER)
		{
		  error ("key %qs expected to be an unsigned integer",
			 full_key.c_str ());
		  return false;
		}
	      // Check if the value is valid for unsigned integer
	      if (auto *int_val
		  = dyn_cast<const json::integer_number *> (json_value))
		{
		  long value = int_val->get ();
		  if (value < 0 || value > UINT_MAX)
		    {
		      error ("key %qs value %ld is out of range for %<uint%> "
			     "type [0, %u]",
			     full_key.c_str (), value, UINT_MAX);
		      return false;
		    }
		}
	    }
	  else if (strcmp (schema_type_str, "string") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_STRING)
		{
		  error ("key %qs expected to be a string", full_key.c_str ());
		  return false;
		}
	    }
	  else if (strcmp (schema_type_str, "boolean") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_TRUE
		  && json_value->get_kind () != json::JSON_FALSE)
		{
		  error ("key %qs expected to be a boolean (true/false)",
			 full_key.c_str ());
		  return false;
		}
	    }
	  else if (strcmp (schema_type_str, "enum") == 0)
	    {
	      if (json_value->get_kind () != json::JSON_STRING)
		{
		  error ("key %qs expected to be an enum (string)",
			 full_key.c_str ());
		  return false;
		}
	    }
	  else
	    {
	      error ("key %qs has unsupported type", full_key.c_str ());
	      return false;
	    }
	}
      else
	{
	  error ("key %qs has unexpected format in schema", full_key.c_str ());
	  return false;
	}
    }
  return true;
}

/* Helper routine for reading the provided JSON file.  */
static std::unique_ptr<std::vector<char>>
read_file (const char *path)
{
  FILE *f_in = fopen (path, "r");
  if (!f_in)
    {
      error ("could not open file %s", path);
      return nullptr;
    }

  auto result = std::make_unique<std::vector<char>> ();
  char buf[4096];

  while (size_t iter_sz_in = fread (buf, 1, sizeof (buf), f_in))
    result->insert (result->end (), buf, buf + iter_sz_in);

  if (!feof (f_in))
    {
      error ("error reading file %s", path);
      fclose (f_in);
      return nullptr;
    }

  fclose (f_in);
  result->push_back ('\0');
  return result;
}

static bool
check_version_compatibility (const json::object *root_obj)
{
  const json::value *metadata_value = root_obj->get ("metadata");
  int json_gcc_major_version = -1;

  if (metadata_value)
    {
      if (auto *metadata_obj = dyn_cast<const json::object *> (metadata_value))
	{
	  const json::value *version_value = metadata_obj->get ("gcc_version");
	  if (version_value)
	    {
	      if (auto *version_int_val
		  = dyn_cast<const json::integer_number *> (version_value))
		json_gcc_major_version = version_int_val->get ();
	    }
	}
    }

  if (json_gcc_major_version == -1)
    {
      warning (0, "JSON tuning file does not contain version information; "
		  "compatibility cannot be verified");
      return true;
    }

  if (json_gcc_major_version != GCC_major_version)
    {
      error ("JSON tuning file was created with GCC version %d "
	     "but current GCC version is %d",
	     json_gcc_major_version, GCC_major_version);
      inform (UNKNOWN_LOCATION, "JSON tuning files must be regenerated "
				"when switching between major GCC versions");
      return false;
    }

  return true;
}

/* Main routine for setting up the parsing of JSON data.  */
static void
aarch64_load_tuning_params_from_json_string (const char *json_string,
					     const char *schema_string,
					     struct tune_params *tune)
{
  /* Try parsing the JSON string.  */
  json::parser_result_t data_result
    = json::parse_utf8_string (strlen (json_string), json_string, true,
			       nullptr);

  if (auto json_err = data_result.m_err.get ())
    {
      error ("error parsing JSON data: %s", json_err->get_msg ());
      return;
    }

  const std::unique_ptr<json::value> &root = data_result.m_val;
  if (!root)
    {
      error ("JSON parsing returned null data");
      return;
    }
  auto *root_obj = dyn_cast<const json::object *> (root.get ());
  if (!root_obj)
    {
      warning (0, "no JSON object found in the provided data");
      return;
    }

  /* Check version compatibility before proceeding.  */
  if (!check_version_compatibility (root_obj))
    return;

  json::parser_result_t schema_result
    = json::parse_utf8_string (strlen (schema_string), schema_string, true,
			       nullptr);

  gcc_assert (!schema_result.m_err.get ());
  gcc_assert (schema_result.m_val);

  auto *schema_obj
    = dyn_cast<const json::object *> (schema_result.m_val.get ());
  gcc_assert (schema_obj);

  const json::value *tune_params_value = root_obj->get ("tune_params");
  if (!tune_params_value)
    {
      warning (0, "key %<tune_params%> not found in JSON data");
      return;
    }

  auto *jo = dyn_cast<const json::object *> (tune_params_value);
  if (!jo)
    {
      error ("key %<tune_params%> is not a JSON object");
      return;
    }

  if (!validate_and_traverse (root_obj, schema_obj))
    {
      error ("validation failed for the provided JSON data");
      return;
    }

  parse_tunings (jo, *tune);
  return;
}

/* Wrapper for calling aarch64_load_tuning_params_from_json_string.  */
void
aarch64_load_tuning_params_from_json (const char *data_filename,
				      struct tune_params *tune)
{
  std::unique_ptr<std::vector<char>> json_data = read_file (data_filename);
  if (!json_data || !json_data->data ())
    {
      error ("cannot read JSON data in %s", data_filename);
      return;
    }
  aarch64_load_tuning_params_from_json_string (
    (const char *) json_data->data (), schema_json, tune);
}

#if CHECKING_P
namespace selftest {

#define STR_(X) #X
#define STR(X) STR_(X)

void
test_json_integers ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	  "sve_width": 256,
	  "issue_rate": 4
	}
    })json";

  tune_params params;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_EQ (params.sve_width, 256);
  ASSERT_EQ (params.issue_rate, 4);
}

void
test_json_boolean ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	    "insn_extra_cost": {
		"alu": {
		    "non_exec_costs_exec": false
		}
	    }
	}
    })json";

  static const cpu_cost_table default_cost_table = {};

  tune_params params;
  params.insn_extra_cost = &default_cost_table;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_EQ (params.insn_extra_cost->alu.non_exec_costs_exec, false);
}

void
test_json_strings ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	    "function_align": "16",
	    "jump_align": "2",
	    "loop_align": "8"
	}
    })json";

  tune_params params;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_STREQ (params.function_align, "16");
  ASSERT_STREQ (params.jump_align, "2");
  ASSERT_STREQ (params.loop_align, "8");
}

void
test_json_enums ()
{
  const char *test_json = R"json({
	"metadata": {
	  "gcc_version": )json" STR (GCC_major_version) R"json(
	},
	"tune_params": {
	    "autoprefetcher_model": "AUTOPREFETCHER_OFF",
	    "ldp_policy_model": "AARCH64_LDP_STP_POLICY_NEVER",
	    "stp_policy_model": "AARCH64_LDP_STP_POLICY_DEFAULT"
	}
    })json";

  tune_params params;

  aarch64_load_tuning_params_from_json_string (test_json, schema_json, &params);

  ASSERT_EQ (params.autoprefetcher_model, tune_params::AUTOPREFETCHER_OFF);
  ASSERT_EQ (params.ldp_policy_model, AARCH64_LDP_STP_POLICY_NEVER);
  ASSERT_EQ (params.stp_policy_model, AARCH64_LDP_STP_POLICY_DEFAULT);
}

void
aarch64_json_tunings_tests ()
{
  test_json_integers ();
  test_json_boolean ();
  test_json_strings ();
  test_json_enums ();
}

} // namespace selftest

#undef STR
#undef STR_

#endif /* CHECKING_P */