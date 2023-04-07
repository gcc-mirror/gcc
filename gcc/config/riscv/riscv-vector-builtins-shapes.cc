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
	  = instance.op_info->ret.get_base_vector_type (
	    builtin_types[instance.type.index].vector);
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
	  = instance.op_info->ret.get_base_vector_type (
	    builtin_types[instance.type.index].vector);
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
	  = instance.op_info->ret.get_base_vector_type (
	    builtin_types[instance.type.index].vector);
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

SHAPE(vsetvl, vsetvl)
SHAPE(vsetvl, vsetvlmax)
SHAPE(loadstore, loadstore)
SHAPE(indexed_loadstore, indexed_loadstore)
SHAPE(alu, alu)
SHAPE(widen_alu, widen_alu)
SHAPE(no_mask_policy, no_mask_policy)
SHAPE(return_mask, return_mask)
SHAPE(narrow_alu, narrow_alu)
SHAPE(move, move)
SHAPE(mask_alu, mask_alu)
SHAPE(reduc_alu, reduc_alu)

} // end namespace riscv_vector
