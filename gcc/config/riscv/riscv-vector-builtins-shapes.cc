/* function_shape implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Ju-Zhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "riscv-vector-builtins.h"
#include "riscv-vector-builtins-shapes.h"

namespace riscv_vector {

/* Add one function instance for GROUP, using operand suffix at index OI,
   mode suffix at index PAIR && bi and predication suffix at index pred_idx.  */
static void
build_one (function_builder &b, const function_group_info &group,
	   unsigned int pred_idx, unsigned int vec_type_idx)
{
  /* Byte forms of non-tuple vlxusegei take 21 arguments.  */
  auto_vec<tree, 21> argument_types;
  function_instance function_instance (group.base_name, *group.base,
				       *group.shape,
				       group.ops_infos.types[vec_type_idx],
				       group.preds[pred_idx], &group.ops_infos);
  tree return_type = group.ops_infos.ret.get_tree_type (
    group.ops_infos.types[vec_type_idx].index);
  b.allocate_argument_types (function_instance, argument_types);
  b.apply_predication (function_instance, return_type, argument_types);
  b.add_unique_function (function_instance, (*group.shape), return_type,
			 argument_types);
}

/* Add a function instance for every operand && predicate && args
   combination in GROUP.  Take the function base name from GROUP && operand
   suffix from operand_suffixes && mode suffix from type_suffixes && predication
   suffix from predication_suffixes. Use apply_predication to add in
   the predicate.  */
static void
build_all (function_builder &b, const function_group_info &group)
{
  for (unsigned int pred_idx = 0; group.preds[pred_idx] != NUM_PRED_TYPES;
       ++pred_idx)
    for (unsigned int vec_type_idx = 0;
	 group.ops_infos.types[vec_type_idx].index != NUM_VECTOR_TYPES;
	 ++vec_type_idx)
      build_one (b, group, pred_idx, vec_type_idx);
}

/* Declare the function shape NAME, pointing it to an instance
   of class <NAME>_def.  */
#define SHAPE(DEF, VAR) \
  static CONSTEXPR const DEF##_def VAR##_obj; \
  namespace shapes { const function_shape *const VAR = &VAR##_obj; }

#define BASE_NAME_MAX_LEN 16

/* Base class for for build.  */
struct build_base : public function_shape
{
  void build (function_builder &b,
	      const function_group_info &group) const override
  {
    build_all (b, group);
  }
};

/* vsetvl_def class.  */
struct vsetvl_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* vsetvl* instruction doesn't have C++ overloaded functions.  */
    if (overloaded_p)
      return nullptr;
    b.append_base_name (instance.base_name);
    b.append_name (type_suffixes[instance.type.index].vsetvl);
    return b.finish_name ();
  }
};

/* loadstore_def class.  */
struct loadstore_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_base_name (instance.base_name);

    tree type = builtin_types[instance.type.index].vector;
    machine_mode mode = TYPE_MODE (type);
    int sew = GET_MODE_BITSIZE (GET_MODE_INNER (mode));
    /* vop --> vop<sew>.  */
    if (GET_MODE_CLASS (mode) != MODE_VECTOR_BOOL)
      b.append_sew (sew);

    /* vop<sew>_v --> vop<sew>_v_<type>.  */
    if (!overloaded_p)
      {
	/* vop<sew> --> vop<sew>_v.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop<sew>_v --> vop<sew>_v_<type>.  */
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* indexed_loadstore_def class.  */
struct indexed_loadstore_def : public function_shape
{
  void build (function_builder &b,
	      const function_group_info &group) const override
  {
    for (unsigned int pred_idx = 0; group.preds[pred_idx] != NUM_PRED_TYPES;
	 ++pred_idx)
      {
	for (unsigned int vec_type_idx = 0;
	     group.ops_infos.types[vec_type_idx].index != NUM_VECTOR_TYPES;
	     ++vec_type_idx)
	  {
	    tree index_type = group.ops_infos.args[1].get_tree_type (
	      group.ops_infos.types[vec_type_idx].index);
	    if (!index_type)
	      continue;
	    build_one (b, group, pred_idx, vec_type_idx);
	  }
      }
  }

  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_base_name (instance.base_name);
    /* vop<sew>_v --> vop<sew>_v_<type>.  */
    if (!overloaded_p)
      {
	/* vop<sew> --> vop<sew>_v.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop<sew>_v --> vop<sew>_v_<type>.  */
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* alu_def class.  */
struct alu_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_base_name (instance.base_name);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      {
	b.append_name (operand_suffixes[instance.op_info->op]);
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }

  bool check (function_checker &c) const override
  {
    /* Check whether rounding mode argument is a valid immediate.  */
    if (c.base->has_rounding_mode_operand_p ())
      {
	if (!c.any_type_float_p ())
	  return c.require_immediate (c.arg_num () - 2, VXRM_RNU, VXRM_ROD);
	/* TODO: We will support floating-point intrinsic modeling
	   rounding mode in the future.  */
      }
    return true;
  }
};

/* The base class for frm build.  */
struct build_frm_base : public build_base
{
  /* Normalize vf<op>_frm to vf<op>.  */
  static void normalize_base_name (char *to, const char *from, int limit)
  {
    strncpy (to, from, limit - 1);
    char *suffix = strstr (to, "_frm");

    if (suffix)
      *suffix = '\0';

    to[limit - 1] = '\0';
  }

  bool check (function_checker &c) const override
  {
    gcc_assert (c.any_type_float_p ());

    /* Check whether rounding mode argument is a valid immediate.  */
    if (c.base->has_rounding_mode_operand_p ())
      {
	unsigned int frm_num = c.arg_num () - 2;

	return c.require_immediate (frm_num, FRM_STATIC_MIN, FRM_STATIC_MAX);
      }

    return true;
  }
};

/* alu_frm_def class.  */
struct alu_frm_def : public build_frm_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    char base_name[BASE_NAME_MAX_LEN] = {};

    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    normalize_base_name (base_name, instance.base_name, sizeof (base_name));

    b.append_base_name (base_name);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      {
	b.append_name (operand_suffixes[instance.op_info->op]);
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_rm" suffix
       for vop_rm C++ overloaded API.  */
    if (!overloaded_p)
      b.append_name ("_rm");

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();

    b.append_name (predication_suffixes[instance.pred]);

    return b.finish_name ();
  }
};

/* widen_alu_frm_def class.  */
struct widen_alu_frm_def : public build_frm_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    char base_name[BASE_NAME_MAX_LEN] = {};

    normalize_base_name (base_name, instance.base_name, sizeof (base_name));

    b.append_base_name (base_name);

    /* vop<sew> --> vop<sew>_<op>.  */
    b.append_name (operand_suffixes[instance.op_info->op]);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      b.append_name (type_suffixes[instance.type.index].vector);

    /* According to rvv-intrinsic-doc, it does not add "_rm" suffix
       for vop_rm C++ overloaded API.  */
    if (!overloaded_p)
      b.append_name ("_rm");

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();

    b.append_name (predication_suffixes[instance.pred]);

    return b.finish_name ();
  }
};

/* widen_alu_def class. Handle vwadd/vwsub. Unlike
   vadd.vx/vadd.vv/vwmul.vv/vwmul.vx, vwadd.vv/vwadd.vx/vwadd.wv/vwadd.wx has
   'OP' suffix in overloaded API.  */
struct widen_alu_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    /* vop<sew> --> vop<sew>_<op>.  */
    b.append_name (operand_suffixes[instance.op_info->op]);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      b.append_name (type_suffixes[instance.type.index].vector);

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* no_mask_policy_def class. Such instructions belong to this class
   doesn't need mask policy.  */
struct no_mask_policy_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    if (!overloaded_p)
      b.append_name (operand_suffixes[instance.op_info->op]);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      b.append_name (type_suffixes[instance.type.index].vector);

    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* return_mask_def class. Such instructions belong to this class
   is returning mask value.  */
struct return_mask_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    if (!overloaded_p)
      b.append_name (operand_suffixes[instance.op_info->op]);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type1>_<type2>.  */
    if (!overloaded_p)
      {
	b.append_name (type_suffixes[instance.type.index].vector);
	vector_type_index ret_type_idx
	  = instance.op_info->ret.get_function_type_index (instance.type.index);
	b.append_name (type_suffixes[ret_type_idx].vector);
      }

    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* narrow_alu_def class. Handle narrowing instructions like vnsrl.wv.  */
struct narrow_alu_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    if (!overloaded_p)
      {
	/* vop --> vop_<op>.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop_<op> --> vop_<op>_<type>.  */
	vector_type_index ret_type_idx
	  = instance.op_info->ret.get_function_type_index (instance.type.index);
	b.append_name (type_suffixes[ret_type_idx].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }

  bool check (function_checker &c) const override
  {
    /* Check whether rounding mode argument is a valid immediate.  */
    if (c.base->has_rounding_mode_operand_p ())
      {
	if (!c.any_type_float_p ())
	  return c.require_immediate (c.arg_num () - 2, VXRM_RNU, VXRM_ROD);
	/* TODO: We will support floating-point intrinsic modeling
	   rounding mode in the future.  */
      }
    return true;
  }
};

/* move_def class. Handle vmv.v.v/vmv.v.x.  */
struct move_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* vmv.v.x/vfmv.v.f (PRED_none) can not be overloaded.  */
    if ((instance.op_info->op == OP_TYPE_x || instance.op_info->op == OP_TYPE_f)
	&& overloaded_p && instance.pred == PRED_TYPE_none)
      return nullptr;

    b.append_base_name (instance.base_name);

    if (!overloaded_p)
      {
	b.append_name (operand_suffixes[instance.op_info->op]);
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* mask_alu_def class.  */
struct mask_alu_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_base_name (instance.base_name);

    if (instance.op_info->op == OP_TYPE_mm || instance.op_info->op == OP_TYPE_m)
      if (!overloaded_p)
	b.append_name (operand_suffixes[instance.op_info->op]);

    /* vop<sew>_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      b.append_name (type_suffixes[instance.type.index].vector);

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* reduc_alu_def class.  */
struct reduc_alu_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    /* vop_<op> --> vop<sew>_<op>_<type>.  */
    if (!overloaded_p)
      {
	b.append_name (operand_suffixes[instance.op_info->op]);
	b.append_name (type_suffixes[instance.type.index].vector);
	vector_type_index ret_type_idx
	  = instance.op_info->ret.get_function_type_index (instance.type.index);
	b.append_name (type_suffixes[ret_type_idx].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* scalar_move_def class.  */
struct scalar_move_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);
    if (overloaded_p)
      return b.finish_name ();
    b.append_name (operand_suffixes[instance.op_info->op]);
    b.append_name (type_suffixes[instance.type.index].vector);
    b.append_name (type_suffixes[instance.type.index].scalar);
    return b.finish_name ();
  }
};

/* vundefined_def class.  */
struct vundefined_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    if (overloaded_p)
      return nullptr;
    b.append_base_name (instance.base_name);
    b.append_name (type_suffixes[instance.type.index].vector);
    return b.finish_name ();
  }
};

/* misc_def class.  */
struct misc_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    if (!overloaded_p)
      {
	b.append_name (operand_suffixes[instance.op_info->op]);
	vector_type_index arg0_type_idx
	  = instance.op_info->args[0].get_function_type_index (
	    instance.type.index);
	b.append_name (type_suffixes[arg0_type_idx].vector);
      }

    vector_type_index ret_type_idx
      = instance.op_info->ret.get_function_type_index (instance.type.index);
    b.append_name (type_suffixes[ret_type_idx].vector);
    return b.finish_name ();
  }
};

/* vset_def class.  */
struct vset_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    b.append_base_name (instance.base_name);

    if (!overloaded_p)
      {
	b.append_name (operand_suffixes[instance.op_info->op]);
	vector_type_index arg_type_idx
	  = instance.op_info->args[2].get_function_type_index (
	    instance.type.index);
	b.append_name (type_suffixes[arg_type_idx].vector);

	vector_type_index ret_type_idx
	  = instance.op_info->ret.get_function_type_index (instance.type.index);
	b.append_name (type_suffixes[ret_type_idx].vector);
      }
    return b.finish_name ();
  }

  bool check (function_checker &c) const override
  {
    poly_int64 outer_size = GET_MODE_SIZE (c.arg_mode (0));
    poly_int64 inner_size = GET_MODE_SIZE (c.arg_mode (2));
    unsigned int nvecs = exact_div (outer_size, inner_size).to_constant ();
    return c.require_immediate (1, 0, nvecs - 1);
  }
};

/* vget_def class.  */
struct vget_def : public misc_def
{
  bool check (function_checker &c) const override
  {
    poly_int64 outer_size = GET_MODE_SIZE (c.arg_mode (0));
    poly_int64 inner_size = GET_MODE_SIZE (c.ret_mode ());
    unsigned int nvecs = exact_div (outer_size, inner_size).to_constant ();
    return c.require_immediate (1, 0, nvecs - 1);
  }
};

/* read_vl_def class.  */
struct read_vl_def : public function_shape
{
  void build (function_builder &b,
	      const function_group_info &group) const override
  {
    auto_vec<tree> argument_types;
    b.add_unique_function (get_read_vl_instance (), (*group.shape),
			   size_type_node, argument_types);
  }

  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    if (overloaded_p)
      return nullptr;
    b.append_base_name (instance.base_name);
    return b.finish_name ();
  }
};

/* fault_load_def class.  */
struct fault_load_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;
    tree type = builtin_types[instance.type.index].vector;
    machine_mode mode = TYPE_MODE (type);
    int sew = GET_MODE_BITSIZE (GET_MODE_INNER (mode));
    b.append_name ("__riscv_");
    b.append_name ("vle");
    b.append_sew (sew);
    b.append_name ("ff");

    /* vop<sew>_v --> vop<sew>_v_<type>.  */
    if (!overloaded_p)
      {
	/* vop<sew> --> vop<sew>_v.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop<sew>_v --> vop<sew>_v_<type>.  */
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* vlenb_def class.  */
struct vlenb_def : public function_shape
{
  void build (function_builder &b,
	      const function_group_info &group) const override
  {
    auto_vec<tree> argument_types;
    function_instance function_instance (group.base_name, *group.base,
					 *group.shape, group.ops_infos.types[0],
					 group.preds[0], &group.ops_infos);
    b.add_unique_function (function_instance, (*group.shape),
			   long_unsigned_type_node, argument_types);
  }

  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    if (overloaded_p)
      return nullptr;
    b.append_base_name (instance.base_name);
    return b.finish_name ();
  }
};

/* seg_loadstore_def class.  */
struct seg_loadstore_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_base_name (instance.base_name);

    tree type = builtin_types[instance.type.index].vector;
    machine_mode mode = TYPE_MODE (type);

    int nf = get_nf (mode);
    /* vop --> vop<nf>.  */
    b.append_nf (nf);

    /* vop<nf> --> vop<nf>e.  */
    b.append_name ("e");

    int sew = GET_MODE_BITSIZE (GET_MODE_INNER (mode));
    /* vop<nf>e --> vop<nf>e<sew>.  */
    b.append_sew (sew);

    if (!overloaded_p)
      {
	/* vop<nf>e<sew> --> vop<nf>e<sew>_v.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop<nf>e<sew>_v --> vop<nf>e<sew>_v_<type>.  */
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* seg_indexed_loadstore_def class.  */
struct seg_indexed_loadstore_def : public indexed_loadstore_def
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_base_name (instance.base_name);

    tree type = builtin_types[instance.type.index].vector;
    machine_mode mode = TYPE_MODE (type);

    int nf = get_nf (mode);
    /* vop --> vop<nf>.  */
    b.append_nf (nf);

    /* vop<nf> --> vop<nf>ei.  */
    b.append_name ("ei");

    /* vop<nf>ei --> vop<nf>ei<eew>.  */
    vector_type_index arg1_type_idx
      = instance.op_info->args[1].get_function_type_index (instance.type.index);
    tree index_type = builtin_types[arg1_type_idx].vector;
    machine_mode index_mode = TYPE_MODE (index_type);
    int eew = GET_MODE_BITSIZE (GET_MODE_INNER (index_mode));
    b.append_sew (eew);

    if (!overloaded_p)
      {
	/* vop<sew> --> vop<sew>_v.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop<sew>_v --> vop<sew>_v_<type>.  */
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

/* seg_fault_load_def class.  */
struct seg_fault_load_def : public build_base
{
  char *get_name (function_builder &b, const function_instance &instance,
		  bool overloaded_p) const override
  {
    /* Return nullptr if it can not be overloaded.  */
    if (overloaded_p && !instance.base->can_be_overloaded_p (instance.pred))
      return nullptr;

    b.append_name ("__riscv_vlseg");

    tree type = builtin_types[instance.type.index].vector;
    machine_mode mode = TYPE_MODE (type);

    int nf = get_nf (mode);
    /* vop --> vop<nf>.  */
    b.append_nf (nf);

    /* vop<nf> --> vop<nf>e.  */
    b.append_name ("e");

    int sew = GET_MODE_BITSIZE (GET_MODE_INNER (mode));
    /* vop<nf>e --> vop<nf>e<sew>.  */
    b.append_sew (sew);

    /* vop<nf>e<sew> --> vop<nf>e<sew>ff.  */
    b.append_name ("ff");

    if (!overloaded_p)
      {
	/* vop<nf>e<sew>ff --> vop<nf>e<sew>ff_v.  */
	b.append_name (operand_suffixes[instance.op_info->op]);
	/* vop<nf>e<sew>ff_v --> vop<nf>e<sew>ff_v_<type>.  */
	b.append_name (type_suffixes[instance.type.index].vector);
      }

    /* According to rvv-intrinsic-doc, it does not add "_m" suffix
       for vop_m C++ overloaded API.  */
    if (overloaded_p && instance.pred == PRED_TYPE_m)
      return b.finish_name ();
    b.append_name (predication_suffixes[instance.pred]);
    return b.finish_name ();
  }
};

SHAPE(vsetvl, vsetvl)
SHAPE(vsetvl, vsetvlmax)
SHAPE(loadstore, loadstore)
SHAPE(indexed_loadstore, indexed_loadstore)
SHAPE(alu, alu)
SHAPE(alu_frm, alu_frm)
SHAPE(widen_alu, widen_alu)
SHAPE(widen_alu_frm, widen_alu_frm)
SHAPE(no_mask_policy, no_mask_policy)
SHAPE(return_mask, return_mask)
SHAPE(narrow_alu, narrow_alu)
SHAPE(move, move)
SHAPE(mask_alu, mask_alu)
SHAPE(reduc_alu, reduc_alu)
SHAPE(scalar_move, scalar_move)
SHAPE(vundefined, vundefined)
SHAPE(misc, misc)
SHAPE(vset, vset)
SHAPE(vget, vget)
SHAPE(read_vl, read_vl)
SHAPE(fault_load, fault_load)
SHAPE(vlenb, vlenb)
SHAPE(seg_loadstore, seg_loadstore)
SHAPE(seg_indexed_loadstore, seg_indexed_loadstore)
SHAPE(seg_fault_load, seg_fault_load)

} // end namespace riscv_vector
