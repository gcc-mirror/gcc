/* function_base implementation for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
#include "recog.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "explow.h"
#include "emit-rtl.h"
#include "tree-vector-builder.h"
#include "rtx-vector-builder.h"
#include "riscv-vector-builtins.h"
#include "riscv-vector-builtins-shapes.h"
#include "riscv-vector-builtins-bases.h"

using namespace riscv_vector;

namespace riscv_vector {

/* Enumerates types of loads/stores operations.
   It's only used in here so we don't define it
   in riscv-vector-builtins-bases.h.  */
enum lst_type
{
  LST_UNIT_STRIDE,
  LST_STRIDED,
  LST_INDEXED,
};

enum frm_op_type
{
  NO_FRM,
  HAS_FRM,
};

/* Helper function to fold vleff and vlsegff.  */
static gimple *
fold_fault_load (gimple_folder &f)
{
  /* fold fault_load (const *base, size_t *new_vl, size_t vl)

     ====> fault_load (const *base, size_t vl)
	   new_vl = MEM_REF[read_vl ()].  */

  auto_vec<tree> vargs (gimple_call_num_args (f.call) - 1);

  for (unsigned i = 0; i < gimple_call_num_args (f.call); i++)
    {
      /* Exclude size_t *new_vl argument.  */
      if (i == gimple_call_num_args (f.call) - 2)
	continue;

      vargs.quick_push (gimple_call_arg (f.call, i));
    }

  gimple *repl = gimple_build_call_vec (gimple_call_fn (f.call), vargs);
  gimple_call_set_lhs (repl, f.lhs);

  /* Handle size_t *new_vl by read_vl.  */
  tree new_vl = gimple_call_arg (f.call, gimple_call_num_args (f.call) - 2);
  if (integer_zerop (new_vl))
    {
      /* This case happens when user passes the nullptr to new_vl argument.
	 In this case, we just need to ignore the new_vl argument and return
	 fault_load instruction directly. */
      return repl;
    }

  tree tmp_var = create_tmp_var (size_type_node, "new_vl");
  tree decl = get_read_vl_decl ();
  gimple *g = gimple_build_call (decl, 0);
  gimple_call_set_lhs (g, tmp_var);
  tree indirect
    = fold_build2 (MEM_REF, size_type_node,
		   gimple_call_arg (f.call, gimple_call_num_args (f.call) - 2),
		   build_int_cst (build_pointer_type (size_type_node), 0));
  gassign *assign = gimple_build_assign (indirect, tmp_var);

  gsi_insert_after (f.gsi, assign, GSI_SAME_STMT);
  gsi_insert_after (f.gsi, g, GSI_SAME_STMT);
  return repl;
}

/* Implements vsetvl<mode> && vsetvlmax<mode>.  */
template<bool VLMAX_P>
class vsetvl : public function_base
{
public:
  bool apply_vl_p () const override
  {
    return false;
  }

  rtx expand (function_expander &e) const override
  {
    if (VLMAX_P)
      e.add_input_operand (Pmode, gen_rtx_REG (Pmode, 0));
    else
      e.add_input_operand (0);

    tree type = builtin_types[e.type.index].vector;
    machine_mode mode = TYPE_MODE (type);

    if (TARGET_XTHEADVECTOR)
      {
	machine_mode inner_mode = GET_MODE_INNER (mode);
	/* SEW.  */
	e.add_input_operand (Pmode,
	  gen_int_mode (GET_MODE_BITSIZE (inner_mode), Pmode));
	/* LMUL.  */
	e.add_input_operand (Pmode,
	  gen_int_mode (get_vlmul (mode), Pmode));
      }
    else
      {
	/* Normalize same RATO (SEW/LMUL) into same vsetvl instruction.

	     - e8,mf8/e16,mf4/e32,mf2/e64,m1 --> e8mf8
	     - e8,mf4/e16,mf2/e32,m1/e64,m2  --> e8mf4
	     - e8,mf2/e16,m1/e32,m2/e64,m4   --> e8mf2
	     - e8,m1/e16,m2/e32,m4/e64,m8    --> e8m1
	     - e8,m2/e16,m4/e32,m8           --> e8m2
	     - e8,m4/e16,m8                  --> e8m4
	     - e8,m8                         --> e8m8
	*/
	/* SEW.  */
	e.add_input_operand (Pmode, gen_int_mode (8, Pmode));

	/* LMUL.  */
	machine_mode e8_mode
	  = get_vector_mode (QImode, GET_MODE_NUNITS (mode)).require ();
	e.add_input_operand (Pmode, gen_int_mode (get_vlmul (e8_mode), Pmode));
      }

    /* TAIL_ANY.  */
    e.add_input_operand (Pmode,
			 gen_int_mode (get_prefer_tail_policy (), Pmode));

    /* MASK_ANY.  */
    e.add_input_operand (Pmode,
			 gen_int_mode (get_prefer_mask_policy (), Pmode));
    return e.generate_insn (code_for_vsetvl_no_side_effects (Pmode));
  }
};

/* Implements
 * vle.v/vse.v/vlm.v/vsm.v/vlse.v/vsse.v/vluxei.v/vloxei.v/vsuxei.v/vsoxei.v
 * codegen.  */
template<bool STORE_P, lst_type LST_TYPE, bool ORDERED_P>
class loadstore : public function_base
{
public:
  bool apply_tail_policy_p () const override { return !STORE_P; }
  bool apply_mask_policy_p () const override { return !STORE_P; }

  unsigned int call_properties (const function_instance &) const override
  {
    if (STORE_P)
      return CP_WRITE_MEMORY;
    else
      return CP_READ_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    if (STORE_P || LST_TYPE == LST_INDEXED)
      return true;
    return pred != PRED_TYPE_none;
  }

  rtx expand (function_expander &e) const override
  {
    if (LST_TYPE == LST_INDEXED)
      {
	int unspec = ORDERED_P ? UNSPEC_ORDERED : UNSPEC_UNORDERED;
	if (STORE_P)
	  return e.use_exact_insn (
	    code_for_pred_indexed_store (unspec, e.vector_mode (),
					 e.index_mode ()));
	else
	  {
	    unsigned src_eew_bitsize
	      = GET_MODE_BITSIZE (GET_MODE_INNER (e.index_mode ()));
	    unsigned dst_eew_bitsize
	      = GET_MODE_BITSIZE (GET_MODE_INNER (e.vector_mode ()));
	    if (dst_eew_bitsize == src_eew_bitsize)
	      return e.use_exact_insn (
		code_for_pred_indexed_load_same_eew (unspec, e.vector_mode ()));
	    else if (dst_eew_bitsize > src_eew_bitsize)
	      {
		unsigned factor = dst_eew_bitsize / src_eew_bitsize;
		switch (factor)
		  {
		  case 2:
		    return e.use_exact_insn (
		      code_for_pred_indexed_load_x2_greater_eew (
			unspec, e.vector_mode ()));
		  case 4:
		    return e.use_exact_insn (
		      code_for_pred_indexed_load_x4_greater_eew (
			unspec, e.vector_mode ()));
		  case 8:
		    return e.use_exact_insn (
		      code_for_pred_indexed_load_x8_greater_eew (
			unspec, e.vector_mode ()));
		  default:
		    gcc_unreachable ();
		  }
	      }
	    else
	      {
		unsigned factor = src_eew_bitsize / dst_eew_bitsize;
		switch (factor)
		  {
		  case 2:
		    return e.use_exact_insn (
		      code_for_pred_indexed_load_x2_smaller_eew (
			unspec, e.vector_mode ()));
		  case 4:
		    return e.use_exact_insn (
		      code_for_pred_indexed_load_x4_smaller_eew (
			unspec, e.vector_mode ()));
		  case 8:
		    return e.use_exact_insn (
		      code_for_pred_indexed_load_x8_smaller_eew (
			unspec, e.vector_mode ()));
		  default:
		    gcc_unreachable ();
		  }
	      }
	  }
      }
    else if (LST_TYPE == LST_STRIDED)
      {
	if (STORE_P)
	  return e.use_contiguous_store_insn (
	    code_for_pred_strided_store (e.vector_mode ()));
	else
	  return e.use_contiguous_load_insn (
	    code_for_pred_strided_load (e.vector_mode ()));
      }
    else
      {
	if (STORE_P)
	  return e.use_contiguous_store_insn (
	    code_for_pred_store (e.vector_mode ()));
	else
	  return e.use_contiguous_load_insn (
	    code_for_pred_mov (e.vector_mode ()));
      }
  }
};

/* Implements
   vadd/vsub/vand/vor/vxor/vsll/vsra/vsrl/
   vmin/vmax/vminu/vmaxu/vdiv/vrem/vdivu/
   vremu/vsadd/vsaddu/vssub/vssubu
   vfadd/vfsub/
*/
template <rtx_code CODE, bool MAY_REQUIRE_FRM = false,
	  enum frm_op_type FRM_OP = NO_FRM>
class binop : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return MAY_REQUIRE_FRM; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vx:
	gcc_assert (FRM_OP == NO_FRM);
      case OP_TYPE_vf:
	return e.use_exact_insn (code_for_pred_scalar (CODE, e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (code_for_pred (CODE, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vrsub.  */
class vrsub : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_sub_reverse_scalar (e.vector_mode ()));
  }
};

/* Implements vneg/vnot.  */
template<rtx_code CODE, enum frm_op_type FRM_OP = NO_FRM>
class unop : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (CODE, e.vector_mode ()));
  }
};

/* Implements vsext.vf2/vsext.vf4/vsext.vf8/vzext.vf2/vzext.vf4/vzext.vf8.  */
template<rtx_code CODE>
class ext : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vf2:
	return e.use_exact_insn (code_for_pred_vf2 (CODE, e.vector_mode ()));
      case OP_TYPE_vf4:
	return e.use_exact_insn (code_for_pred_vf4 (CODE, e.vector_mode ()));
      case OP_TYPE_vf8:
	return e.use_exact_insn (code_for_pred_vf8 (CODE, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vmulh/vmulhu/vmulhsu.  */
template<int UNSPEC>
class vmulh : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_mulh_scalar (UNSPEC, e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (
	  code_for_pred_mulh (UNSPEC, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vwadd/vwsub/vwmul.  */
template<rtx_code CODE1, rtx_code CODE2 = FLOAT_EXTEND>
class widen_binop : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vv:
	return e.use_exact_insn (
	  code_for_pred_dual_widen (CODE1, CODE2, e.vector_mode ()));
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_dual_widen_scalar (CODE1, CODE2, e.vector_mode ()));
      case OP_TYPE_wv:
	if (CODE1 == PLUS)
	  return e.use_exact_insn (
	    code_for_pred_single_widen_add (CODE2, e.vector_mode ()));
	else
	  return e.use_exact_insn (
	    code_for_pred_single_widen_sub (CODE2, e.vector_mode ()));
      case OP_TYPE_wx:
	return e.use_exact_insn (
	  code_for_pred_single_widen_scalar (CODE1, CODE2, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implement vfwadd/vfwsub/vfwmul.  */
template<rtx_code CODE, enum frm_op_type FRM_OP = NO_FRM>
class widen_binop_fp : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vv:
	return e.use_exact_insn (
	  code_for_pred_dual_widen (CODE, e.vector_mode ()));
      case OP_TYPE_vf:
	return e.use_exact_insn (
	  code_for_pred_dual_widen_scalar (CODE, e.vector_mode ()));
      case OP_TYPE_wv:
	if (CODE == PLUS)
	  return e.use_exact_insn (
	    code_for_pred_single_widen_add (e.vector_mode ()));
	else
	  return e.use_exact_insn (
	    code_for_pred_single_widen_sub (e.vector_mode ()));
      case OP_TYPE_wf:
	return e.use_exact_insn (
	  code_for_pred_single_widen_scalar (CODE, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vwmulsu.  */
class vwmulsu : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vv:
	return e.use_exact_insn (code_for_pred_widen_mulsu (e.vector_mode ()));
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_widen_mulsu_scalar (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vwcvt.  */
template<rtx_code CODE>
class vwcvt : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (CODE, e.vector_mode ()));
  }
};

/* Implements vadc.  */
class vadc : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vvm:
	return e.use_exact_insn (code_for_pred_adc (e.vector_mode ()));
      case OP_TYPE_vxm:
	return e.use_exact_insn (code_for_pred_adc_scalar (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vsbc.  */
class vsbc : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vvm:
	return e.use_exact_insn (code_for_pred_sbc (e.vector_mode ()));
      case OP_TYPE_vxm:
	return e.use_exact_insn (code_for_pred_sbc_scalar (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vmadc.  */
class vmadc : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vvm:
	return e.use_exact_insn (code_for_pred_madc (e.vector_mode ()));
      case OP_TYPE_vxm:
	return e.use_exact_insn (code_for_pred_madc_scalar (e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (
	  code_for_pred_madc_overflow (e.vector_mode ()));
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_madc_overflow_scalar (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vmsbc.  */
class vmsbc : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vvm:
	return e.use_exact_insn (code_for_pred_msbc (e.vector_mode ()));
      case OP_TYPE_vxm:
	return e.use_exact_insn (code_for_pred_msbc_scalar (e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (
	  code_for_pred_msbc_overflow (e.vector_mode ()));
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_msbc_overflow_scalar (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vnsrl/vnsra.  */
template<rtx_code CODE>
class vnshift : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_wx:
	return e.use_exact_insn (
	  code_for_pred_narrow_scalar (CODE, e.vector_mode ()));
      case OP_TYPE_wv:
	return e.use_exact_insn (code_for_pred_narrow (CODE, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vncvt.  */
class vncvt_x : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_trunc (e.vector_mode ()));
  }
};

/* Implements vmerge/vfmerge.  */
class vmerge : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vvm:
	return e.use_exact_insn (code_for_pred_merge (e.vector_mode ()));
      case OP_TYPE_vxm:
      case OP_TYPE_vfm:
	return e.use_exact_insn (code_for_pred_merge_scalar (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vmv.v.x/vmv.v.v/vfmv.v.f.  */
class vmv_v : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_v:
	return e.use_exact_insn (code_for_pred_mov (e.vector_mode ()));
      case OP_TYPE_x:
      case OP_TYPE_f:
	return e.use_exact_insn (code_for_pred_broadcast (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vaadd/vasub/vsmul/vssra/vssrl.  */
template<int UNSPEC>
class sat_op : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override { return true; }

  bool may_require_vxrm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_scalar (UNSPEC, e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (code_for_pred (UNSPEC, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vnclip/vnclipu.  */
template<int UNSPEC>
class vnclip : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override { return true; }

  bool may_require_vxrm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_wx:
	return e.use_exact_insn (
	  code_for_pred_narrow_clip_scalar (UNSPEC, e.vector_mode ()));
      case OP_TYPE_wv:
	return e.use_exact_insn (
	  code_for_pred_narrow_clip (UNSPEC, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vmseq/vmsne/vmslt/vmsgt/vmsle/vmsge.  */
template<rtx_code CODE>
class icmp : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
	case OP_TYPE_vx: {
	  if (CODE == GE || CODE == GEU)
	    return e.use_compare_insn (CODE, code_for_pred_ge_scalar (
					       e.vector_mode ()));
	  else
	    return e.use_compare_insn (CODE, code_for_pred_cmp_scalar (
					       e.vector_mode ()));
	}
	case OP_TYPE_vv: {
	  if (CODE == LT || CODE == LTU || CODE == GE || CODE == GEU)
	    return e.use_compare_insn (CODE,
				       code_for_pred_ltge (e.vector_mode ()));
	  else
	    return e.use_compare_insn (CODE,
				       code_for_pred_cmp (e.vector_mode ()));
	}
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vmacc/vnmsac/vmadd/vnmsub.  */
class vmacc : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_ternop_insn (true, code_for_pred_mul_plus_scalar (
					e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (true,
				code_for_pred_mul_plus (e.vector_mode ()));
    gcc_unreachable ();
  }
};

class vnmsac : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_ternop_insn (true, code_for_pred_minus_mul_scalar (
					e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (true,
				code_for_pred_minus_mul (e.vector_mode ()));
    gcc_unreachable ();
  }
};

class vmadd : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_ternop_insn (false, code_for_pred_mul_plus_scalar (
					 e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (false,
				code_for_pred_mul_plus (e.vector_mode ()));
    gcc_unreachable ();
  }
};

class vnmsub : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_ternop_insn (false, code_for_pred_minus_mul_scalar (
					 e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (false,
				code_for_pred_minus_mul (e.vector_mode ()));
    gcc_unreachable ();
  }
};

/* Implements vwmacc<su><su>.  */
class vwmacc : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_plus_scalar (SIGN_EXTEND, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_plus (SIGN_EXTEND, e.vector_mode ()));
    gcc_unreachable ();
  }
};

class vwmaccu : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_plus_scalar (ZERO_EXTEND, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_plus (ZERO_EXTEND, e.vector_mode ()));
    gcc_unreachable ();
  }
};

class vwmaccsu : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vx)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_plussu_scalar (e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_plussu (e.vector_mode ()));
    gcc_unreachable ();
  }
};

class vwmaccus : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_widen_ternop_insn (
      code_for_pred_widen_mul_plusus_scalar (e.vector_mode ()));
  }
};

/* Implements vmand/vmnand/vmandn/vmxor/vmor/vmnor/vmorn/vmxnor  */
template<rtx_code CODE>
class mask_logic : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (CODE, e.vector_mode ()));
  }
};
template<rtx_code CODE>
class mask_nlogic : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_n (CODE, e.vector_mode ()));
  }
};
template<rtx_code CODE>
class mask_notlogic : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_not (CODE, e.vector_mode ()));
  }
};

/* Implements vmmv.  */
class vmmv : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_mov (e.vector_mode ()));
  }
};

/* Implements vmclr.  */
class vmclr : public function_base
{
public:
  bool can_be_overloaded_p (enum predication_type_index) const override
  {
    return false;
  }

  rtx expand (function_expander &e) const override
  {
    machine_mode mode = TYPE_MODE (TREE_TYPE (e.exp));
    e.add_all_one_mask_operand (mode);
    e.add_vundef_operand (mode);
    e.add_input_operand (mode, CONST0_RTX (mode));
    e.add_input_operand (call_expr_nargs (e.exp) - 1);
    e.add_input_operand (Pmode, get_avl_type_rtx (avl_type::NONVLMAX));
    return e.generate_insn (code_for_pred_mov (e.vector_mode ()));
  }
};

/* Implements vmset.  */
class vmset : public function_base
{
public:
  bool can_be_overloaded_p (enum predication_type_index) const override
  {
    return false;
  }

  rtx expand (function_expander &e) const override
  {
    machine_mode mode = TYPE_MODE (TREE_TYPE (e.exp));
    e.add_all_one_mask_operand (mode);
    e.add_vundef_operand (mode);
    e.add_input_operand (mode, CONSTM1_RTX (mode));
    e.add_input_operand (call_expr_nargs (e.exp) - 1);
    e.add_input_operand (Pmode, get_avl_type_rtx (avl_type::NONVLMAX));
    return e.generate_insn (code_for_pred_mov (e.vector_mode ()));
  }
};

/* Implements vmnot.  */
class vmnot : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_not (e.vector_mode ()));
  }
};

/* Implements vcpop.  */
class vcpop : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_popcount (e.vector_mode (), Pmode));
  }
};

/* Implements vfirst.  */
class vfirst : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_ffs (e.vector_mode (), Pmode));
  }
};

/* Implements vmsbf/vmsif/vmsof.  */
template<int UNSPEC>
class mask_misc : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (UNSPEC, e.vector_mode ()));
  }
};

/* Implements viota.  */
class viota : public function_base
{
public:
  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    return pred == PRED_TYPE_tu || pred == PRED_TYPE_tum
	   || pred == PRED_TYPE_tumu || pred == PRED_TYPE_mu;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_iota (e.vector_mode ()));
  }
};

/* Implements vid.  */
class vid : public function_base
{
public:
  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    return pred == PRED_TYPE_tu || pred == PRED_TYPE_tum
	   || pred == PRED_TYPE_tumu || pred == PRED_TYPE_mu;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_series (e.vector_mode ()));
  }
};

/* Implements vfrsub/vfrdiv.  */
template<rtx_code CODE, enum frm_op_type FRM_OP = NO_FRM>
class reverse_binop : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_reverse_scalar (CODE, e.vector_mode ()));
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfmacc : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (true,
				code_for_pred_mul_scalar (PLUS,
							  e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (true,
				code_for_pred_mul (PLUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfnmsac : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (
	true, code_for_pred_mul_neg_scalar (PLUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (true,
				code_for_pred_mul_neg (PLUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfmadd : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (false,
				code_for_pred_mul_scalar (PLUS,
							  e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (false,
				code_for_pred_mul (PLUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfnmsub : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (
	false, code_for_pred_mul_neg_scalar (PLUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (false,
				code_for_pred_mul_neg (PLUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfnmacc : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (
	true, code_for_pred_mul_neg_scalar (MINUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (true,
				code_for_pred_mul_neg (MINUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfmsac : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (true,
				code_for_pred_mul_scalar (MINUS,
							  e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (true,
				code_for_pred_mul (MINUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfnmadd : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (
	false, code_for_pred_mul_neg_scalar (MINUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (false,
				code_for_pred_mul_neg (MINUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfmsub : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_ternop_insn (false,
				code_for_pred_mul_scalar (MINUS,
							  e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_ternop_insn (false,
				code_for_pred_mul (MINUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfwmacc : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_scalar (PLUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul (PLUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfwnmacc : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_neg_scalar (MINUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_neg (MINUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfwmsac : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_scalar (MINUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul (MINUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfwnmsac : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_neg_scalar (PLUS, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_mul_neg (PLUS, e.vector_mode ()));
    gcc_unreachable ();
  }
};

/* Implements vfsqrt7/vfrec7/vfclass/vfsgnj/vfsgnjx.  */
template<int UNSPEC, enum frm_op_type FRM_OP = NO_FRM>
class float_misc : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_exact_insn (code_for_pred_scalar (UNSPEC, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv || e.op_info->op == OP_TYPE_v)
      return e.use_exact_insn (code_for_pred (UNSPEC, e.vector_mode ()));
    gcc_unreachable ();
  }
};

/* Implements vfsgnjn.  */
class vfsgnjn : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_exact_insn (code_for_pred_ncopysign_scalar (e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_exact_insn (code_for_pred_ncopysign (e.vector_mode ()));
    gcc_unreachable ();
  }
};

/* Implements vmfeq/vmfne/vmflt/vmfgt/vmfle/vmfge.  */
template<rtx_code CODE>
class fcmp : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
	case OP_TYPE_vf: {
	  return e.use_compare_insn (CODE, code_for_pred_cmp_scalar (
					     e.vector_mode ()));
	}
	case OP_TYPE_vv: {
	  return e.use_compare_insn (CODE,
				     code_for_pred_cmp (e.vector_mode ()));
	}
      default:
	gcc_unreachable ();
      }
  }
};

/* Implements vfclass.  */
class vfclass : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_class (e.arg_mode (0)));
  }
};

/* Implements vfcvt.x.  */
template<int UNSPEC, enum frm_op_type FRM_OP = NO_FRM>
class vfcvt_x : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_fcvt_x_f (UNSPEC, e.arg_mode (0)));
  }
};

/* Implements vfcvt.rtz.x.  */
template<rtx_code CODE>
class vfcvt_rtz_x : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (CODE, e.arg_mode (0)));
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfcvt_f : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_x_v)
      return e.use_exact_insn (code_for_pred (FLOAT, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_xu_v)
      return e.use_exact_insn (
	code_for_pred (UNSIGNED_FLOAT, e.vector_mode ()));
    gcc_unreachable ();
  }
};

/* Implements vfwcvt.x.  */
template<int UNSPEC, enum frm_op_type FRM_OP = NO_FRM>
class vfwcvt_x : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_widen_fcvt_x_f (UNSPEC, e.vector_mode ()));
  }
};

/* Implements vfwcvt.rtz.x.  */
template<rtx_code CODE>
class vfwcvt_rtz_x : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_widen (CODE, e.vector_mode ()));
  }
};

class vfwcvt_f : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_f_v)
      return e.use_exact_insn (code_for_pred_extend (e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_x_v)
      return e.use_exact_insn (code_for_pred_widen (FLOAT, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_xu_v)
      return e.use_exact_insn (
	code_for_pred_widen (UNSIGNED_FLOAT, e.vector_mode ()));
    gcc_unreachable ();
  }
};

/* Implements vfncvt.x.  */
template<int UNSPEC, enum frm_op_type FRM_OP = NO_FRM>
class vfncvt_x : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_narrow_fcvt_x_f (UNSPEC, e.arg_mode (0)));
  }
};

/* Implements vfncvt.rtz.x.  */
template<rtx_code CODE>
class vfncvt_rtz_x : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_narrow (CODE, e.vector_mode ()));
  }
};

template<enum frm_op_type FRM_OP = NO_FRM>
class vfncvt_f : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_f_w)
      return e.use_exact_insn (code_for_pred_trunc (e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_x_w)
      return e.use_exact_insn (code_for_pred_narrow (FLOAT, e.arg_mode (0)));
    if (e.op_info->op == OP_TYPE_xu_w)
      return e.use_exact_insn (
	code_for_pred_narrow (UNSIGNED_FLOAT, e.arg_mode (0)));
    gcc_unreachable ();
  }
};

class vfncvt_rod_f : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_rod_trunc (e.vector_mode ()));
  }
};

/* Implements reduction instructions.  */
template<unsigned UNSPEC>
class reducop : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (UNSPEC, e.vector_mode ()));
  }
};

/* Implements floating-point reduction instructions.  */
template<unsigned UNSPEC, enum frm_op_type FRM_OP = NO_FRM>
class freducop : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred (UNSPEC, e.vector_mode ()));
  }
};

/* Implements vmv/vfmv instructions.  */
class vmv : public function_base
{
public:
  bool apply_vl_p () const override { return false; }
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_extract_first (e.vector_mode ()));
  }
};

/* Implements vmv.s.x/vfmv.s.f.  */
class vmv_s : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_scalar_move_insn (code_for_pred_broadcast (e.vector_mode ()));
  }
};

template<int UNSPEC>
class slideop : public function_base
{
public:
  bool has_merge_operand_p () const override
  {
    if (UNSPEC == UNSPEC_VSLIDEUP)
      return false;
    return true;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_slide (UNSPEC, e.vector_mode ()));
  }
};

class vrgather : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vx:
	return e.use_exact_insn (
	  code_for_pred_gather_scalar (e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (code_for_pred_gather (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};

class vrgatherei16 : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_gatherei16 (e.vector_mode ()));
  }
};

class vcompress : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_compress (e.vector_mode ()));
  }
};

class vundefined : public function_base
{
public:
  bool apply_vl_p () const override
  {
    return false;
  }

  rtx expand (function_expander &e) const override
  {
    return e.generate_insn (code_for_vundefined (e.vector_mode ()));
  }
};

class vreinterpret : public function_base
{
public:
  bool apply_vl_p () const override
  {
    return false;
  }

  rtx expand (function_expander &e) const override
  {
    e.add_input_operand (0);
    return e.generate_insn (code_for_vreinterpret (e.ret_mode ()));
  }
};

class vlmul_ext : public function_base
{
public:
  bool apply_vl_p () const override
  {
    return false;
  }

  rtx expand (function_expander &e) const override
  {
    tree arg = CALL_EXPR_ARG (e.exp, 0);
    rtx src = expand_normal (arg);
    emit_move_insn (gen_lowpart (e.vector_mode (), e.target), src);
    return e.target;
  }
};

class vlmul_trunc : public function_base
{
public:
  bool apply_vl_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    rtx src = expand_normal (CALL_EXPR_ARG (e.exp, 0));
    emit_move_insn (e.target, gen_lowpart (GET_MODE (e.target), src));
    return e.target;
  }
};

class vset : public function_base
{
public:
  bool apply_vl_p () const override { return false; }

  gimple *fold (gimple_folder &f) const override
  {
    tree rhs_tuple = gimple_call_arg (f.call, 0);
    /* LMUL > 1 non-tuple vector types are not structure,
       we can't use __val[index] to set the subpart.  */
    if (!riscv_v_ext_tuple_mode_p (TYPE_MODE (TREE_TYPE (rhs_tuple))))
      return NULL;
    tree index = gimple_call_arg (f.call, 1);
    tree rhs_vector = gimple_call_arg (f.call, 2);

    /* Replace the call with two statements: a copy of the full tuple
       to the call result, followed by an update of the individual vector.

       The fold routines expect the replacement statement to have the
       same lhs as the original call, so return the copy statement
       rather than the field update.  */
    gassign *copy = gimple_build_assign (unshare_expr (f.lhs), rhs_tuple);

    /* Get a reference to the individual vector.  */
    tree field = tuple_type_field (TREE_TYPE (f.lhs));
    tree lhs_array
      = build3 (COMPONENT_REF, TREE_TYPE (field), f.lhs, field, NULL_TREE);
    tree lhs_vector = build4 (ARRAY_REF, TREE_TYPE (rhs_vector), lhs_array,
			      index, NULL_TREE, NULL_TREE);
    gassign *update = gimple_build_assign (lhs_vector, rhs_vector);
    gsi_insert_after (f.gsi, update, GSI_SAME_STMT);

    return copy;
  }

  rtx expand (function_expander &e) const override
  {
    if (!e.target)
      return NULL_RTX;
    rtx dest = expand_normal (CALL_EXPR_ARG (e.exp, 0));
    gcc_assert (riscv_v_ext_vector_mode_p (GET_MODE (dest)));
    rtx index = expand_normal (CALL_EXPR_ARG (e.exp, 1));
    rtx src = expand_normal (CALL_EXPR_ARG (e.exp, 2));
    poly_int64 offset = INTVAL (index) * GET_MODE_SIZE (GET_MODE (src));
    emit_move_insn (e.target, dest);
    rtx subreg = simplify_gen_subreg (GET_MODE (src), e.target,
				      GET_MODE (e.target), offset);
    emit_move_insn (subreg, src);
    return e.target;
  }
};

class vget : public function_base
{
public:
  bool apply_vl_p () const override { return false; }

  gimple *fold (gimple_folder &f) const override
  {
    /* Fold into a normal gimple component access.  */
    tree rhs_tuple = gimple_call_arg (f.call, 0);
    /* LMUL > 1 non-tuple vector types are not structure,
       we can't use __val[index] to get the subpart.  */
    if (!riscv_v_ext_tuple_mode_p (TYPE_MODE (TREE_TYPE (rhs_tuple))))
      return NULL;
    tree index = gimple_call_arg (f.call, 1);
    tree field = tuple_type_field (TREE_TYPE (rhs_tuple));
    tree rhs_array
      = build3 (COMPONENT_REF, TREE_TYPE (field), rhs_tuple, field, NULL_TREE);
    tree rhs_vector = build4 (ARRAY_REF, TREE_TYPE (f.lhs), rhs_array, index,
			      NULL_TREE, NULL_TREE);
    return gimple_build_assign (f.lhs, rhs_vector);
  }

  rtx expand (function_expander &e) const override
  {
    if (!e.target)
      return NULL_RTX;
    rtx src = expand_normal (CALL_EXPR_ARG (e.exp, 0));
    gcc_assert (riscv_v_ext_vector_mode_p (GET_MODE (src)));
    rtx index = expand_normal (CALL_EXPR_ARG (e.exp, 1));
    poly_int64 offset = INTVAL (index) * GET_MODE_SIZE (GET_MODE (e.target));
    rtx subreg
      = simplify_gen_subreg (GET_MODE (e.target), src, GET_MODE (src), offset);
    return subreg;
  }
};

class vcreate : public function_base
{
public:
  gimple *fold (gimple_folder &f) const override
  {
    unsigned int nargs = gimple_call_num_args (f.call);
    tree lhs_type = TREE_TYPE (f.lhs);
    /* LMUL > 1 non-tuple vector types are not structure,
   we can't use __val[index] to set the subpart.  */
    if (!riscv_v_ext_tuple_mode_p (TYPE_MODE (lhs_type)))
      return NULL;

    /* Replace the call with a clobber of the result (to prevent it from
       becoming upwards exposed) followed by stores into each individual
       vector of tuple.

       The fold routines expect the replacement statement to have the
       same lhs as the original call, so return the clobber statement
       rather than the final vector store.  */
    gassign *clobber = gimple_build_assign (f.lhs, build_clobber (lhs_type));

    for (unsigned int i = nargs; i-- > 0; )
      {
	tree rhs_vector = gimple_call_arg (f.call, i);
	tree field = tuple_type_field (TREE_TYPE (f.lhs));
	tree lhs_array = build3 (COMPONENT_REF, TREE_TYPE (field),
				 unshare_expr (f.lhs), field, NULL_TREE);
	tree lhs_vector = build4 (ARRAY_REF, TREE_TYPE (rhs_vector),
				  lhs_array, size_int (i),
				  NULL_TREE, NULL_TREE);
	gassign *assign = gimple_build_assign (lhs_vector, rhs_vector);
	gsi_insert_after (f.gsi, assign, GSI_SAME_STMT);
      }
    return clobber;
  }

  rtx expand (function_expander &e) const override
  {
    if (!e.target)
      return NULL_RTX;
    gcc_assert (riscv_v_ext_vector_mode_p (GET_MODE (e.target)));
    unsigned int nargs = call_expr_nargs (e.exp);
    for (unsigned int i = 0; i < nargs; i++)
      {
	rtx src = expand_normal (CALL_EXPR_ARG (e.exp, i));
	poly_int64 offset = i * GET_MODE_SIZE (GET_MODE (src));
	rtx subreg = simplify_gen_subreg (GET_MODE (src), e.target,
					  GET_MODE (e.target), offset);
	emit_move_insn (subreg, src);
      }

    return e.target;
  }
};

class read_vl : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_CSR;
  }

  rtx expand (function_expander &e) const override
  {
    if (Pmode == SImode)
      emit_insn (gen_read_vlsi (e.target));
    else
      emit_insn (gen_read_vldi_zero_extend (e.target));
    return e.target;
  }
};

class vleff : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY | CP_WRITE_CSR;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    return pred != PRED_TYPE_none;
  }

  gimple *fold (gimple_folder &f) const override
  {
    return fold_fault_load (f);
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_contiguous_load_insn (
      code_for_pred_fault_load (e.vector_mode ()));
  }
};

/* Implements vlenb.  */
class vlenb : public function_base
{
public:
  bool apply_vl_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    machine_mode mode = GET_MODE (e.target);
    rtx vlenb = gen_int_mode (BYTES_PER_RISCV_VECTOR, mode);
    emit_move_insn (e.target, vlenb);
    return e.target;
  }
};

/* Implements vlseg.v.  */
class vlseg : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    return pred != PRED_TYPE_none;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_unit_strided_load (e.vector_mode ()));
  }
};

/* Implements vsseg.v.  */
class vsseg : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  unsigned int call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index) const override
  {
    return true;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_unit_strided_store (e.vector_mode ()));
  }
};

/* Implements vlsseg.v.  */
class vlsseg : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    return pred != PRED_TYPE_none;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_strided_load (e.vector_mode ()));
  }
};

/* Implements vssseg.v.  */
class vssseg : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  unsigned int call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index) const override
  {
    return true;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_strided_store (e.vector_mode ()));
  }
};

template<int UNSPEC>
class seg_indexed_load : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index) const override
  {
    return true;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_indexed_load (UNSPEC, e.vector_mode (), e.index_mode ()));
  }
};

template<int UNSPEC>
class seg_indexed_store : public function_base
{
public:
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }

  unsigned int call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index) const override
  {
    return true;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_indexed_store (UNSPEC, e.vector_mode (), e.index_mode ()));
  }
};

/* Implements vlsegff.v.  */
class vlsegff : public function_base
{
public:
  unsigned int call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY | CP_WRITE_CSR;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    return pred != PRED_TYPE_none;
  }

  gimple *fold (gimple_folder &f) const override
  {
    return fold_fault_load (f);
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_fault_load (e.vector_mode ()));
  }
};

/* Implements
 * th.vl(b/h/w)[u].v/th.vs(b/h/w)[u].v/th.vls(b/h/w)[u].v/th.vss(b/h/w)[u].v/
 * th.vlx(b/h/w)[u].v/th.vs[u]x(b/h/w).v
 * codegen.  */
template<bool STORE_P, lst_type LST_TYPE, int UNSPEC>
class th_loadstore_width : public function_base
{
public:
  bool apply_tail_policy_p () const override { return !STORE_P; }
  bool apply_mask_policy_p () const override { return !STORE_P; }

  unsigned int call_properties (const function_instance &) const override
  {
    if (STORE_P)
      return CP_WRITE_MEMORY;
    else
      return CP_READ_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    if (STORE_P || LST_TYPE == LST_INDEXED)
      return true;
    return pred != PRED_TYPE_none;
  }

  rtx expand (function_expander &e) const override
  {
    gcc_assert (TARGET_XTHEADVECTOR);
    if (LST_TYPE == LST_INDEXED)
      {
	if (STORE_P)
	  return e.use_exact_insn (
	    code_for_pred_indexed_store_width (UNSPEC, UNSPEC,
					       e.vector_mode ()));
	else
	  return e.use_exact_insn (
	    code_for_pred_indexed_load_width (UNSPEC, e.vector_mode ()));
      }
    else if (LST_TYPE == LST_STRIDED)
      {
	if (STORE_P)
	  return e.use_contiguous_store_insn (
	    code_for_pred_strided_store_width (UNSPEC, e.vector_mode ()));
	else
	  return e.use_contiguous_load_insn (
	    code_for_pred_strided_load_width (UNSPEC, e.vector_mode ()));
      }
    else
      {
	if (STORE_P)
	  return e.use_contiguous_store_insn (
	    code_for_pred_store_width (UNSPEC, e.vector_mode ()));
	else
	  return e.use_contiguous_load_insn (
	    code_for_pred_mov_width (UNSPEC, e.vector_mode ()));
      }
  }
};

/* Implements vext.x.v.  */
class th_extract : public function_base
{
public:
  bool apply_vl_p () const override { return false; }
  bool apply_tail_policy_p () const override { return false; }
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    gcc_assert (TARGET_XTHEADVECTOR);
    return e.use_exact_insn (code_for_pred_th_extract (e.vector_mode ()));
  }
};

/* Below implements are vector crypto */
/* Implements vandn.[vv,vx] */
class vandn : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vv:
        return e.use_exact_insn (code_for_pred_vandn (e.vector_mode ()));
      case OP_TYPE_vx:
        return e.use_exact_insn (code_for_pred_vandn_scalar (e.vector_mode ()));
      default:
        gcc_unreachable ();
      }
  }
};

/* Implements vrol/vror/clz/ctz.  */
template<rtx_code CODE>
class bitmanip : public function_base
{
public:
  bool apply_tail_policy_p () const override
  {
    return (CODE == CLZ || CODE == CTZ) ? false : true;
  }
  bool apply_mask_policy_p () const override
  {
    return (CODE == CLZ || CODE == CTZ) ? false : true;
  }
  bool has_merge_operand_p () const override
  {
    return (CODE == CLZ || CODE == CTZ) ? false : true;
  }

  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
    {
      case OP_TYPE_v:
      case OP_TYPE_vv:
        return e.use_exact_insn (code_for_pred_v (CODE, e.vector_mode ()));
      case OP_TYPE_vx:
        return e.use_exact_insn (code_for_pred_v_scalar (CODE, e.vector_mode ()));
      default:
        gcc_unreachable ();
    }
  }
};

/* Implements vbrev/vbrev8/vrev8.  */
template<int UNSPEC>
class b_reverse : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
      return e.use_exact_insn (code_for_pred_v (UNSPEC, e.vector_mode ()));
  }
};

class vwsll : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vv:
        return e.use_exact_insn (code_for_pred_vwsll (e.vector_mode ()));
      case OP_TYPE_vx:
        return e.use_exact_insn (code_for_pred_vwsll_scalar (e.vector_mode ()));
      default:
        gcc_unreachable ();
      }
  }
};

/* Implements clmul */
template<int UNSPEC>
class clmul : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vv:
        return e.use_exact_insn (
                 code_for_pred_vclmul (UNSPEC, e.vector_mode ()));
      case OP_TYPE_vx:
        return e.use_exact_insn
                 (code_for_pred_vclmul_scalar (UNSPEC, e.vector_mode ()));
      default:
        gcc_unreachable ();
      }
  }
};

/* Implements vghsh/vsh2ms/vsha2c[hl]. */
template<int UNSPEC>
class vg_nhab : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_v (UNSPEC, e.vector_mode ()));
  }
};

/* Implements vgmul/vaes*. */
template<int UNSPEC>
class crypto_vv : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    poly_uint64 nunits = 0U;
    switch (e.op_info->op)
    {
      case OP_TYPE_vv:
        if (UNSPEC == UNSPEC_VGMUL)
          return e.use_exact_insn
                   (code_for_pred_crypto_vv (UNSPEC, UNSPEC, e.vector_mode ()));
        else
          return e.use_exact_insn
                   (code_for_pred_crypto_vv (UNSPEC + 1, UNSPEC + 1, e.vector_mode ()));
      case OP_TYPE_vs:
        /* Calculate the ratio between arg0 and arg1*/
        gcc_assert (multiple_p (GET_MODE_BITSIZE (e.arg_mode (0)),
                                GET_MODE_BITSIZE (e.arg_mode (1)), &nunits));
        if (maybe_eq (nunits, 1U))
          return e.use_exact_insn (code_for_pred_crypto_vvx1_scalar
                                   (UNSPEC + 2, UNSPEC + 2, e.vector_mode ()));
        else if (maybe_eq (nunits, 2U))
          return e.use_exact_insn (code_for_pred_crypto_vvx2_scalar
                                   (UNSPEC + 2, UNSPEC + 2, e.vector_mode ()));
        else if (maybe_eq (nunits, 4U))
          return e.use_exact_insn (code_for_pred_crypto_vvx4_scalar
                                   (UNSPEC + 2, UNSPEC + 2, e.vector_mode ()));
        else if (maybe_eq (nunits, 8U))
          return e.use_exact_insn (code_for_pred_crypto_vvx8_scalar
                                   (UNSPEC + 2, UNSPEC + 2, e.vector_mode ()));
        else
          return e.use_exact_insn (code_for_pred_crypto_vvx16_scalar
                                   (UNSPEC + 2, UNSPEC + 2, e.vector_mode ()));
      default:
        gcc_unreachable ();
    }
  }
};

/* Implements vaeskf1/vsm4k. */
template<int UNSPEC>
class crypto_vi : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn
             (code_for_pred_crypto_vi_scalar (UNSPEC, e.vector_mode ()));
  }
};

/* Implements vaeskf2/vsm3c. */
template<int UNSPEC>
class vaeskf2_vsm3c : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn
             (code_for_pred_vi_nomaskedoff_scalar (UNSPEC, e.vector_mode ()));
  }
};

/* Implements vsm3me. */
class vsm3me : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_vsm3me (e.vector_mode ()));
  }
};

/* Implements vfncvtbf16_f. */
template <enum frm_op_type FRM_OP = NO_FRM>
class vfncvtbf16_f : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_trunc_to_bf16 (e.vector_mode ()));
  }
};

/* Implements vfwcvtbf16_f. */
class vfwcvtbf16_f : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_extend_bf16_to (e.vector_mode ()));
  }
};

/* Implements vfwmaccbf16. */
template <enum frm_op_type FRM_OP = NO_FRM>
class vfwmaccbf16 : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }

  bool may_require_frm_p () const override { return true; }

  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_bf16_mul_scalar (e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv)
      return e.use_widen_ternop_insn (
	code_for_pred_widen_bf16_mul (e.vector_mode ()));
    gcc_unreachable ();
  }
};

static CONSTEXPR const vsetvl<false> vsetvl_obj;
static CONSTEXPR const vsetvl<true> vsetvlmax_obj;
static CONSTEXPR const loadstore<false, LST_UNIT_STRIDE, false> vle_obj;
static CONSTEXPR const loadstore<true, LST_UNIT_STRIDE, false> vse_obj;
static CONSTEXPR const loadstore<false, LST_UNIT_STRIDE, false> vlm_obj;
static CONSTEXPR const loadstore<true, LST_UNIT_STRIDE, false> vsm_obj;
static CONSTEXPR const loadstore<false, LST_STRIDED, false> vlse_obj;
static CONSTEXPR const loadstore<true, LST_STRIDED, false> vsse_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, false> vluxei8_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, false> vluxei16_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, false> vluxei32_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, false> vluxei64_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, true> vloxei8_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, true> vloxei16_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, true> vloxei32_obj;
static CONSTEXPR const loadstore<false, LST_INDEXED, true> vloxei64_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, false> vsuxei8_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, false> vsuxei16_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, false> vsuxei32_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, false> vsuxei64_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, true> vsoxei8_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, true> vsoxei16_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, true> vsoxei32_obj;
static CONSTEXPR const loadstore<true, LST_INDEXED, true> vsoxei64_obj;
static CONSTEXPR const binop<PLUS> vadd_obj;
static CONSTEXPR const binop<MINUS> vsub_obj;
static CONSTEXPR const vrsub vrsub_obj;
static CONSTEXPR const binop<AND> vand_obj;
static CONSTEXPR const binop<IOR> vor_obj;
static CONSTEXPR const binop<XOR> vxor_obj;
static CONSTEXPR const binop<ASHIFT> vsll_obj;
static CONSTEXPR const binop<ASHIFTRT> vsra_obj;
static CONSTEXPR const binop<LSHIFTRT> vsrl_obj;
static CONSTEXPR const binop<SMIN> vmin_obj;
static CONSTEXPR const binop<SMAX> vmax_obj;
static CONSTEXPR const binop<UMIN> vminu_obj;
static CONSTEXPR const binop<UMAX> vmaxu_obj;
static CONSTEXPR const binop<MULT> vmul_obj;
static CONSTEXPR const vmulh<UNSPEC_VMULHS> vmulh_obj;
static CONSTEXPR const vmulh<UNSPEC_VMULHU> vmulhu_obj;
static CONSTEXPR const vmulh<UNSPEC_VMULHSU> vmulhsu_obj;
static CONSTEXPR const binop<DIV> vdiv_obj;
static CONSTEXPR const binop<MOD> vrem_obj;
static CONSTEXPR const binop<UDIV> vdivu_obj;
static CONSTEXPR const binop<UMOD> vremu_obj;
static CONSTEXPR const unop<NEG> vneg_obj;
static CONSTEXPR const unop<NOT> vnot_obj;
static CONSTEXPR const ext<SIGN_EXTEND> vsext_obj;
static CONSTEXPR const ext<ZERO_EXTEND> vzext_obj;
static CONSTEXPR const widen_binop<PLUS, SIGN_EXTEND>vwadd_obj;
static CONSTEXPR const widen_binop<MINUS, SIGN_EXTEND>vwsub_obj;
static CONSTEXPR const widen_binop<MULT, SIGN_EXTEND>vwmul_obj;
static CONSTEXPR const widen_binop<PLUS, ZERO_EXTEND>vwaddu_obj;
static CONSTEXPR const widen_binop<MINUS, ZERO_EXTEND>vwsubu_obj;
static CONSTEXPR const widen_binop<MULT, ZERO_EXTEND>vwmulu_obj;
static CONSTEXPR const vwmulsu vwmulsu_obj;
static CONSTEXPR const vwcvt<SIGN_EXTEND> vwcvt_x_obj;
static CONSTEXPR const vwcvt<ZERO_EXTEND> vwcvtu_x_obj;
static CONSTEXPR const vadc vadc_obj;
static CONSTEXPR const vsbc vsbc_obj;
static CONSTEXPR const vmadc vmadc_obj;
static CONSTEXPR const vmsbc vmsbc_obj;
static CONSTEXPR const vnshift<LSHIFTRT> vnsrl_obj;
static CONSTEXPR const vnshift<ASHIFTRT> vnsra_obj;
static CONSTEXPR const vncvt_x vncvt_x_obj;
static CONSTEXPR const vmerge vmerge_obj;
static CONSTEXPR const vmv_v vmv_v_obj;
static CONSTEXPR const icmp<EQ> vmseq_obj;
static CONSTEXPR const icmp<NE> vmsne_obj;
static CONSTEXPR const icmp<LT> vmslt_obj;
static CONSTEXPR const icmp<GT> vmsgt_obj;
static CONSTEXPR const icmp<LE> vmsle_obj;
static CONSTEXPR const icmp<GE> vmsge_obj;
static CONSTEXPR const icmp<LTU> vmsltu_obj;
static CONSTEXPR const icmp<GTU> vmsgtu_obj;
static CONSTEXPR const icmp<LEU> vmsleu_obj;
static CONSTEXPR const icmp<GEU> vmsgeu_obj;
static CONSTEXPR const vmacc vmacc_obj;
static CONSTEXPR const vnmsac vnmsac_obj;
static CONSTEXPR const vmadd vmadd_obj;
static CONSTEXPR const vnmsub vnmsub_obj;
static CONSTEXPR const vwmacc vwmacc_obj;
static CONSTEXPR const vwmaccu vwmaccu_obj;
static CONSTEXPR const vwmaccsu vwmaccsu_obj;
static CONSTEXPR const vwmaccus vwmaccus_obj;
static CONSTEXPR const binop<SS_PLUS> vsadd_obj;
static CONSTEXPR const binop<SS_MINUS> vssub_obj;
static CONSTEXPR const binop<US_PLUS> vsaddu_obj;
static CONSTEXPR const binop<US_MINUS> vssubu_obj;
static CONSTEXPR const sat_op<UNSPEC_VAADDU> vaaddu_obj;
static CONSTEXPR const sat_op<UNSPEC_VAADD> vaadd_obj;
static CONSTEXPR const sat_op<UNSPEC_VASUBU> vasubu_obj;
static CONSTEXPR const sat_op<UNSPEC_VASUB> vasub_obj;
static CONSTEXPR const sat_op<UNSPEC_VSMUL> vsmul_obj;
static CONSTEXPR const sat_op<UNSPEC_VSSRL> vssrl_obj;
static CONSTEXPR const sat_op<UNSPEC_VSSRA> vssra_obj;
static CONSTEXPR const vnclip<UNSPEC_VNCLIP> vnclip_obj;
static CONSTEXPR const vnclip<UNSPEC_VNCLIPU> vnclipu_obj;
static CONSTEXPR const mask_logic<AND> vmand_obj;
static CONSTEXPR const mask_nlogic<AND> vmnand_obj;
static CONSTEXPR const mask_notlogic<AND> vmandn_obj;
static CONSTEXPR const mask_logic<XOR> vmxor_obj;
static CONSTEXPR const mask_logic<IOR> vmor_obj;
static CONSTEXPR const mask_nlogic<IOR> vmnor_obj;
static CONSTEXPR const mask_notlogic<IOR> vmorn_obj;
static CONSTEXPR const mask_nlogic<XOR> vmxnor_obj;
static CONSTEXPR const vmmv vmmv_obj;
static CONSTEXPR const vmclr vmclr_obj;
static CONSTEXPR const vmset vmset_obj;
static CONSTEXPR const vmnot vmnot_obj;
static CONSTEXPR const vcpop vcpop_obj;
static CONSTEXPR const vfirst vfirst_obj;
static CONSTEXPR const mask_misc<UNSPEC_VMSBF> vmsbf_obj;
static CONSTEXPR const mask_misc<UNSPEC_VMSIF> vmsif_obj;
static CONSTEXPR const mask_misc<UNSPEC_VMSOF> vmsof_obj;
static CONSTEXPR const viota viota_obj;
static CONSTEXPR const vid vid_obj;
static CONSTEXPR const binop<PLUS, true> vfadd_obj;
static CONSTEXPR const binop<MINUS, true> vfsub_obj;
static CONSTEXPR const binop<PLUS, true, HAS_FRM> vfadd_frm_obj;
static CONSTEXPR const binop<MINUS, true, HAS_FRM> vfsub_frm_obj;
static CONSTEXPR const reverse_binop<MINUS> vfrsub_obj;
static CONSTEXPR const reverse_binop<MINUS, HAS_FRM> vfrsub_frm_obj;
static CONSTEXPR const widen_binop_fp<PLUS> vfwadd_obj;
static CONSTEXPR const widen_binop_fp<PLUS, HAS_FRM> vfwadd_frm_obj;
static CONSTEXPR const widen_binop_fp<MINUS> vfwsub_obj;
static CONSTEXPR const widen_binop_fp<MINUS, HAS_FRM> vfwsub_frm_obj;
static CONSTEXPR const binop<MULT, true> vfmul_obj;
static CONSTEXPR const binop<MULT,  true, HAS_FRM> vfmul_frm_obj;
static CONSTEXPR const binop<DIV, true> vfdiv_obj;
static CONSTEXPR const binop<DIV,  true, HAS_FRM> vfdiv_frm_obj;
static CONSTEXPR const reverse_binop<DIV> vfrdiv_obj;
static CONSTEXPR const reverse_binop<DIV, HAS_FRM> vfrdiv_frm_obj;
static CONSTEXPR const widen_binop_fp<MULT> vfwmul_obj;
static CONSTEXPR const widen_binop_fp<MULT, HAS_FRM> vfwmul_frm_obj;
static CONSTEXPR const vfmacc<NO_FRM> vfmacc_obj;
static CONSTEXPR const vfmacc<HAS_FRM> vfmacc_frm_obj;
static CONSTEXPR const vfnmsac<NO_FRM> vfnmsac_obj;
static CONSTEXPR const vfnmsac<HAS_FRM> vfnmsac_frm_obj;
static CONSTEXPR const vfmadd<NO_FRM> vfmadd_obj;
static CONSTEXPR const vfmadd<HAS_FRM> vfmadd_frm_obj;
static CONSTEXPR const vfnmsub<NO_FRM> vfnmsub_obj;
static CONSTEXPR const vfnmsub<HAS_FRM> vfnmsub_frm_obj;
static CONSTEXPR const vfnmacc<NO_FRM> vfnmacc_obj;
static CONSTEXPR const vfnmacc<HAS_FRM> vfnmacc_frm_obj;
static CONSTEXPR const vfmsac<NO_FRM> vfmsac_obj;
static CONSTEXPR const vfmsac<HAS_FRM> vfmsac_frm_obj;
static CONSTEXPR const vfnmadd<NO_FRM> vfnmadd_obj;
static CONSTEXPR const vfnmadd<HAS_FRM> vfnmadd_frm_obj;
static CONSTEXPR const vfmsub<NO_FRM> vfmsub_obj;
static CONSTEXPR const vfmsub<HAS_FRM> vfmsub_frm_obj;
static CONSTEXPR const vfwmacc<NO_FRM> vfwmacc_obj;
static CONSTEXPR const vfwmacc<HAS_FRM> vfwmacc_frm_obj;
static CONSTEXPR const vfwnmacc<NO_FRM> vfwnmacc_obj;
static CONSTEXPR const vfwnmacc<HAS_FRM> vfwnmacc_frm_obj;
static CONSTEXPR const vfwmsac<NO_FRM> vfwmsac_obj;
static CONSTEXPR const vfwmsac<HAS_FRM> vfwmsac_frm_obj;
static CONSTEXPR const vfwnmsac<NO_FRM> vfwnmsac_obj;
static CONSTEXPR const vfwnmsac<HAS_FRM> vfwnmsac_frm_obj;
static CONSTEXPR const unop<SQRT> vfsqrt_obj;
static CONSTEXPR const unop<SQRT, HAS_FRM> vfsqrt_frm_obj;
static CONSTEXPR const float_misc<UNSPEC_VFRSQRT7> vfrsqrt7_obj;
static CONSTEXPR const float_misc<UNSPEC_VFREC7> vfrec7_obj;
static CONSTEXPR const float_misc<UNSPEC_VFREC7, HAS_FRM> vfrec7_frm_obj;
static CONSTEXPR const binop<SMIN> vfmin_obj;
static CONSTEXPR const binop<SMAX> vfmax_obj;
static CONSTEXPR const float_misc<UNSPEC_VCOPYSIGN> vfsgnj_obj;
static CONSTEXPR const vfsgnjn vfsgnjn_obj;
static CONSTEXPR const float_misc<UNSPEC_VXORSIGN> vfsgnjx_obj;
static CONSTEXPR const unop<NEG> vfneg_obj;
static CONSTEXPR const unop<ABS> vfabs_obj;
static CONSTEXPR const fcmp<EQ> vmfeq_obj;
static CONSTEXPR const fcmp<NE> vmfne_obj;
static CONSTEXPR const fcmp<LT> vmflt_obj;
static CONSTEXPR const fcmp<GT> vmfgt_obj;
static CONSTEXPR const fcmp<LE> vmfle_obj;
static CONSTEXPR const fcmp<GE> vmfge_obj;
static CONSTEXPR const vfclass vfclass_obj;
static CONSTEXPR const vmerge vfmerge_obj;
static CONSTEXPR const vmv_v vfmv_v_obj;
static CONSTEXPR const vfcvt_x<UNSPEC_VFCVT> vfcvt_x_obj;
static CONSTEXPR const vfcvt_x<UNSPEC_VFCVT, HAS_FRM> vfcvt_x_frm_obj;
static CONSTEXPR const vfcvt_x<UNSPEC_UNSIGNED_VFCVT> vfcvt_xu_obj;
static CONSTEXPR const vfcvt_x<UNSPEC_UNSIGNED_VFCVT, HAS_FRM> vfcvt_xu_frm_obj;
static CONSTEXPR const vfcvt_rtz_x<FIX> vfcvt_rtz_x_obj;
static CONSTEXPR const vfcvt_rtz_x<UNSIGNED_FIX> vfcvt_rtz_xu_obj;
static CONSTEXPR const vfcvt_f<NO_FRM> vfcvt_f_obj;
static CONSTEXPR const vfcvt_f<HAS_FRM> vfcvt_f_frm_obj;
static CONSTEXPR const vfwcvt_x<UNSPEC_VFCVT> vfwcvt_x_obj;
static CONSTEXPR const vfwcvt_x<UNSPEC_VFCVT, HAS_FRM> vfwcvt_x_frm_obj;
static CONSTEXPR const vfwcvt_x<UNSPEC_UNSIGNED_VFCVT> vfwcvt_xu_obj;
static CONSTEXPR const vfwcvt_x<UNSPEC_UNSIGNED_VFCVT, HAS_FRM> vfwcvt_xu_frm_obj;
static CONSTEXPR const vfwcvt_rtz_x<FIX> vfwcvt_rtz_x_obj;
static CONSTEXPR const vfwcvt_rtz_x<UNSIGNED_FIX> vfwcvt_rtz_xu_obj;
static CONSTEXPR const vfwcvt_f vfwcvt_f_obj;
static CONSTEXPR const vfncvt_x<UNSPEC_VFCVT> vfncvt_x_obj;
static CONSTEXPR const vfncvt_x<UNSPEC_VFCVT, HAS_FRM> vfncvt_x_frm_obj;
static CONSTEXPR const vfncvt_x<UNSPEC_UNSIGNED_VFCVT> vfncvt_xu_obj;
static CONSTEXPR const vfncvt_x<UNSPEC_UNSIGNED_VFCVT, HAS_FRM> vfncvt_xu_frm_obj;
static CONSTEXPR const vfncvt_rtz_x<FIX> vfncvt_rtz_x_obj;
static CONSTEXPR const vfncvt_rtz_x<UNSIGNED_FIX> vfncvt_rtz_xu_obj;
static CONSTEXPR const vfncvt_f<NO_FRM> vfncvt_f_obj;
static CONSTEXPR const vfncvt_f<HAS_FRM> vfncvt_f_frm_obj;
static CONSTEXPR const vfncvt_rod_f vfncvt_rod_f_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_SUM> vredsum_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_MAXU> vredmaxu_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_MAX> vredmax_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_MINU> vredminu_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_MIN> vredmin_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_AND> vredand_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_OR> vredor_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_XOR> vredxor_obj;
static CONSTEXPR const reducop<UNSPEC_WREDUC_SUM> vwredsum_obj;
static CONSTEXPR const reducop<UNSPEC_WREDUC_SUMU> vwredsumu_obj;
static CONSTEXPR const freducop<UNSPEC_REDUC_SUM_UNORDERED> vfredusum_obj;
static CONSTEXPR const freducop<UNSPEC_REDUC_SUM_UNORDERED, HAS_FRM> vfredusum_frm_obj;
static CONSTEXPR const freducop<UNSPEC_REDUC_SUM_ORDERED> vfredosum_obj;
static CONSTEXPR const freducop<UNSPEC_REDUC_SUM_ORDERED, HAS_FRM> vfredosum_frm_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_MAX> vfredmax_obj;
static CONSTEXPR const reducop<UNSPEC_REDUC_MIN> vfredmin_obj;
static CONSTEXPR const freducop<UNSPEC_WREDUC_SUM_UNORDERED> vfwredusum_obj;
static CONSTEXPR const freducop<UNSPEC_WREDUC_SUM_UNORDERED, HAS_FRM> vfwredusum_frm_obj;
static CONSTEXPR const freducop<UNSPEC_WREDUC_SUM_ORDERED> vfwredosum_obj;
static CONSTEXPR const freducop<UNSPEC_WREDUC_SUM_ORDERED, HAS_FRM> vfwredosum_frm_obj;
static CONSTEXPR const vmv vmv_x_obj;
static CONSTEXPR const vmv_s vmv_s_obj;
static CONSTEXPR const vmv vfmv_f_obj;
static CONSTEXPR const vmv_s vfmv_s_obj;
static CONSTEXPR const slideop<UNSPEC_VSLIDEUP> vslideup_obj;
static CONSTEXPR const slideop<UNSPEC_VSLIDEDOWN> vslidedown_obj;
static CONSTEXPR const slideop<UNSPEC_VSLIDE1UP> vslide1up_obj;
static CONSTEXPR const slideop<UNSPEC_VSLIDE1DOWN> vslide1down_obj;
static CONSTEXPR const slideop<UNSPEC_VFSLIDE1UP> vfslide1up_obj;
static CONSTEXPR const slideop<UNSPEC_VFSLIDE1DOWN> vfslide1down_obj;
static CONSTEXPR const vrgather vrgather_obj;
static CONSTEXPR const vrgatherei16 vrgatherei16_obj;
static CONSTEXPR const vcompress vcompress_obj;
static CONSTEXPR const vundefined vundefined_obj;
static CONSTEXPR const vreinterpret vreinterpret_obj;
static CONSTEXPR const vlmul_ext vlmul_ext_obj;
static CONSTEXPR const vlmul_trunc vlmul_trunc_obj;
static CONSTEXPR const vset vset_obj;
static CONSTEXPR const vget vget_obj;
static CONSTEXPR const vcreate vcreate_obj;
static CONSTEXPR const read_vl read_vl_obj;
static CONSTEXPR const vleff vleff_obj;
static CONSTEXPR const vlenb vlenb_obj;
static CONSTEXPR const vlseg vlseg_obj;
static CONSTEXPR const vsseg vsseg_obj;
static CONSTEXPR const vlsseg vlsseg_obj;
static CONSTEXPR const vssseg vssseg_obj;
static CONSTEXPR const seg_indexed_load<UNSPEC_UNORDERED> vluxseg_obj;
static CONSTEXPR const seg_indexed_load<UNSPEC_ORDERED> vloxseg_obj;
static CONSTEXPR const seg_indexed_store<UNSPEC_UNORDERED> vsuxseg_obj;
static CONSTEXPR const seg_indexed_store<UNSPEC_ORDERED> vsoxseg_obj;
static CONSTEXPR const vlsegff vlsegff_obj;
static CONSTEXPR const th_loadstore_width<false, LST_UNIT_STRIDE, UNSPEC_TH_VLB> vlb_obj;
static CONSTEXPR const th_loadstore_width<false, LST_UNIT_STRIDE, UNSPEC_TH_VLBU> vlbu_obj;
static CONSTEXPR const th_loadstore_width<false, LST_UNIT_STRIDE, UNSPEC_TH_VLH> vlh_obj;
static CONSTEXPR const th_loadstore_width<false, LST_UNIT_STRIDE, UNSPEC_TH_VLHU> vlhu_obj;
static CONSTEXPR const th_loadstore_width<false, LST_UNIT_STRIDE, UNSPEC_TH_VLW> vlw_obj;
static CONSTEXPR const th_loadstore_width<false, LST_UNIT_STRIDE, UNSPEC_TH_VLWU> vlwu_obj;
static CONSTEXPR const th_loadstore_width<true, LST_UNIT_STRIDE, UNSPEC_TH_VLB> vsb_obj;
static CONSTEXPR const th_loadstore_width<true, LST_UNIT_STRIDE, UNSPEC_TH_VLH> vsh_obj;
static CONSTEXPR const th_loadstore_width<true, LST_UNIT_STRIDE, UNSPEC_TH_VLW> vsw_obj;
static CONSTEXPR const th_loadstore_width<false, LST_STRIDED, UNSPEC_TH_VLSB> vlsb_obj;
static CONSTEXPR const th_loadstore_width<false, LST_STRIDED, UNSPEC_TH_VLSBU> vlsbu_obj;
static CONSTEXPR const th_loadstore_width<false, LST_STRIDED, UNSPEC_TH_VLSH> vlsh_obj;
static CONSTEXPR const th_loadstore_width<false, LST_STRIDED, UNSPEC_TH_VLSHU> vlshu_obj;
static CONSTEXPR const th_loadstore_width<false, LST_STRIDED, UNSPEC_TH_VLSW> vlsw_obj;
static CONSTEXPR const th_loadstore_width<false, LST_STRIDED, UNSPEC_TH_VLSWU> vlswu_obj;
static CONSTEXPR const th_loadstore_width<true, LST_STRIDED, UNSPEC_TH_VLSB> vssb_obj;
static CONSTEXPR const th_loadstore_width<true, LST_STRIDED, UNSPEC_TH_VLSH> vssh_obj;
static CONSTEXPR const th_loadstore_width<true, LST_STRIDED, UNSPEC_TH_VLSW> vssw_obj;
static CONSTEXPR const th_loadstore_width<false, LST_INDEXED, UNSPEC_TH_VLXB> vlxb_obj;
static CONSTEXPR const th_loadstore_width<false, LST_INDEXED, UNSPEC_TH_VLXBU> vlxbu_obj;
static CONSTEXPR const th_loadstore_width<false, LST_INDEXED, UNSPEC_TH_VLXH> vlxh_obj;
static CONSTEXPR const th_loadstore_width<false, LST_INDEXED, UNSPEC_TH_VLXHU> vlxhu_obj;
static CONSTEXPR const th_loadstore_width<false, LST_INDEXED, UNSPEC_TH_VLXW> vlxw_obj;
static CONSTEXPR const th_loadstore_width<false, LST_INDEXED, UNSPEC_TH_VLXWU> vlxwu_obj;
static CONSTEXPR const th_loadstore_width<true, LST_INDEXED, UNSPEC_TH_VLXB> vsxb_obj;
static CONSTEXPR const th_loadstore_width<true, LST_INDEXED, UNSPEC_TH_VLXH> vsxh_obj;
static CONSTEXPR const th_loadstore_width<true, LST_INDEXED, UNSPEC_TH_VLXW> vsxw_obj;
static CONSTEXPR const th_loadstore_width<true, LST_INDEXED, UNSPEC_TH_VSUXB> vsuxb_obj;
static CONSTEXPR const th_loadstore_width<true, LST_INDEXED, UNSPEC_TH_VSUXH> vsuxh_obj;
static CONSTEXPR const th_loadstore_width<true, LST_INDEXED, UNSPEC_TH_VSUXW> vsuxw_obj;
static CONSTEXPR const th_extract vext_x_v_obj;

/* Crypto Vector */
static CONSTEXPR const vandn vandn_obj;
static CONSTEXPR const bitmanip<ROTATE>   vrol_obj;
static CONSTEXPR const bitmanip<ROTATERT> vror_obj;
static CONSTEXPR const b_reverse<UNSPEC_VBREV>   vbrev_obj;
static CONSTEXPR const b_reverse<UNSPEC_VBREV8>  vbrev8_obj;
static CONSTEXPR const b_reverse<UNSPEC_VREV8>   vrev8_obj;
static CONSTEXPR const bitmanip<CLZ> vclz_obj;
static CONSTEXPR const bitmanip<CTZ> vctz_obj;
static CONSTEXPR const vwsll vwsll_obj;
static CONSTEXPR const clmul<UNSPEC_VCLMUL>      vclmul_obj;
static CONSTEXPR const clmul<UNSPEC_VCLMULH>     vclmulh_obj;
static CONSTEXPR const vg_nhab<UNSPEC_VGHSH>     vghsh_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VGMUL>   vgmul_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VAESEF>  vaesef_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VAESEM>  vaesem_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VAESDF>  vaesdf_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VAESDM>  vaesdm_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VAESZ>   vaesz_obj;
static CONSTEXPR const crypto_vi<UNSPEC_VAESKF1> vaeskf1_obj;
static CONSTEXPR const vaeskf2_vsm3c<UNSPEC_VAESKF2> vaeskf2_obj;
static CONSTEXPR const vg_nhab<UNSPEC_VSHA2MS>   vsha2ms_obj;
static CONSTEXPR const vg_nhab<UNSPEC_VSHA2CH>   vsha2ch_obj;
static CONSTEXPR const vg_nhab<UNSPEC_VSHA2CL>   vsha2cl_obj;
static CONSTEXPR const crypto_vi<UNSPEC_VSM4K>   vsm4k_obj;
static CONSTEXPR const crypto_vv<UNSPEC_VSM4R>   vsm4r_obj;
static CONSTEXPR const vsm3me vsm3me_obj;
static CONSTEXPR const vaeskf2_vsm3c<UNSPEC_VSM3C>   vsm3c_obj;

/* Zvfbfmin */
static CONSTEXPR const vfncvtbf16_f<NO_FRM> vfncvtbf16_f_obj;
static CONSTEXPR const vfncvtbf16_f<HAS_FRM> vfncvtbf16_f_frm_obj;
static CONSTEXPR const vfwcvtbf16_f vfwcvtbf16_f_obj;
/* Zvfbfwma; */
static CONSTEXPR const vfwmaccbf16<NO_FRM> vfwmaccbf16_obj;
static CONSTEXPR const vfwmaccbf16<HAS_FRM> vfwmaccbf16_frm_obj;

/* Declare the function base NAME, pointing it to an instance
   of class <NAME>_obj.  */
#define BASE(NAME) \
  namespace bases { const function_base *const NAME = &NAME##_obj; }

BASE (vsetvl)
BASE (vsetvlmax)
BASE (vle)
BASE (vse)
BASE (vlm)
BASE (vsm)
BASE (vlse)
BASE (vsse)
BASE (vluxei8)
BASE (vluxei16)
BASE (vluxei32)
BASE (vluxei64)
BASE (vloxei8)
BASE (vloxei16)
BASE (vloxei32)
BASE (vloxei64)
BASE (vsuxei8)
BASE (vsuxei16)
BASE (vsuxei32)
BASE (vsuxei64)
BASE (vsoxei8)
BASE (vsoxei16)
BASE (vsoxei32)
BASE (vsoxei64)
BASE (vadd)
BASE (vsub)
BASE (vrsub)
BASE (vand)
BASE (vor)
BASE (vxor)
BASE (vsll)
BASE (vsra)
BASE (vsrl)
BASE (vmin)
BASE (vmax)
BASE (vminu)
BASE (vmaxu)
BASE (vmul)
BASE (vmulh)
BASE (vmulhu)
BASE (vmulhsu)
BASE (vdiv)
BASE (vrem)
BASE (vdivu)
BASE (vremu)
BASE (vneg)
BASE (vnot)
BASE (vsext)
BASE (vzext)
BASE (vwadd)
BASE (vwsub)
BASE (vwmul)
BASE (vwaddu)
BASE (vwsubu)
BASE (vwmulu)
BASE (vwmulsu)
BASE (vwcvt_x)
BASE (vwcvtu_x)
BASE (vadc)
BASE (vsbc)
BASE (vmadc)
BASE (vmsbc)
BASE (vnsrl)
BASE (vnsra)
BASE (vncvt_x)
BASE (vmerge)
BASE (vmv_v)
BASE (vmseq)
BASE (vmsne)
BASE (vmslt)
BASE (vmsgt)
BASE (vmsle)
BASE (vmsge)
BASE (vmsltu)
BASE (vmsgtu)
BASE (vmsleu)
BASE (vmsgeu)
BASE (vmacc)
BASE (vnmsac)
BASE (vmadd)
BASE (vnmsub)
BASE (vwmacc)
BASE (vwmaccu)
BASE (vwmaccsu)
BASE (vwmaccus)
BASE (vsadd)
BASE (vssub)
BASE (vsaddu)
BASE (vssubu)
BASE (vaadd)
BASE (vasub)
BASE (vaaddu)
BASE (vasubu)
BASE (vsmul)
BASE (vssra)
BASE (vssrl)
BASE (vnclip)
BASE (vnclipu)
BASE (vmand)
BASE (vmnand)
BASE (vmandn)
BASE (vmxor)
BASE (vmor)
BASE (vmnor)
BASE (vmorn)
BASE (vmxnor)
BASE (vmmv)
BASE (vmclr)
BASE (vmset)
BASE (vmnot)
BASE (vcpop)
BASE (vfirst)
BASE (vmsbf)
BASE (vmsif)
BASE (vmsof)
BASE (viota)
BASE (vid)
BASE (vfadd)
BASE (vfadd_frm)
BASE (vfsub)
BASE (vfsub_frm)
BASE (vfrsub)
BASE (vfrsub_frm)
BASE (vfwadd)
BASE (vfwadd_frm)
BASE (vfwsub)
BASE (vfwsub_frm)
BASE (vfmul)
BASE (vfmul_frm)
BASE (vfdiv)
BASE (vfdiv_frm)
BASE (vfrdiv)
BASE (vfrdiv_frm)
BASE (vfwmul)
BASE (vfwmul_frm)
BASE (vfmacc)
BASE (vfmacc_frm)
BASE (vfnmsac)
BASE (vfnmsac_frm)
BASE (vfmadd)
BASE (vfmadd_frm)
BASE (vfnmsub)
BASE (vfnmsub_frm)
BASE (vfnmacc)
BASE (vfnmacc_frm)
BASE (vfmsac)
BASE (vfmsac_frm)
BASE (vfnmadd)
BASE (vfnmadd_frm)
BASE (vfmsub)
BASE (vfmsub_frm)
BASE (vfwmacc)
BASE (vfwmacc_frm)
BASE (vfwnmacc)
BASE (vfwnmacc_frm)
BASE (vfwmsac)
BASE (vfwmsac_frm)
BASE (vfwnmsac)
BASE (vfwnmsac_frm)
BASE (vfsqrt)
BASE (vfsqrt_frm)
BASE (vfrsqrt7)
BASE (vfrec7)
BASE (vfrec7_frm)
BASE (vfmin)
BASE (vfmax)
BASE (vfsgnj)
BASE (vfsgnjn)
BASE (vfsgnjx)
BASE (vfneg)
BASE (vfabs)
BASE (vmfeq)
BASE (vmfne)
BASE (vmflt)
BASE (vmfgt)
BASE (vmfle)
BASE (vmfge)
BASE (vfclass)
BASE (vfmerge)
BASE (vfmv_v)
BASE (vfcvt_x)
BASE (vfcvt_x_frm)
BASE (vfcvt_xu)
BASE (vfcvt_xu_frm)
BASE (vfcvt_rtz_x)
BASE (vfcvt_rtz_xu)
BASE (vfcvt_f)
BASE (vfcvt_f_frm)
BASE (vfwcvt_x)
BASE (vfwcvt_x_frm)
BASE (vfwcvt_xu)
BASE (vfwcvt_xu_frm)
BASE (vfwcvt_rtz_x)
BASE (vfwcvt_rtz_xu)
BASE (vfwcvt_f)
BASE (vfncvt_x)
BASE (vfncvt_x_frm)
BASE (vfncvt_xu)
BASE (vfncvt_xu_frm)
BASE (vfncvt_rtz_x)
BASE (vfncvt_rtz_xu)
BASE (vfncvt_f)
BASE (vfncvt_f_frm)
BASE (vfncvt_rod_f)
BASE (vredsum)
BASE (vredmaxu)
BASE (vredmax)
BASE (vredminu)
BASE (vredmin)
BASE (vredand)
BASE (vredor)
BASE (vredxor)
BASE (vwredsum)
BASE (vwredsumu)
BASE (vfredusum)
BASE (vfredusum_frm)
BASE (vfredosum)
BASE (vfredosum_frm)
BASE (vfredmax)
BASE (vfredmin)
BASE (vfwredosum)
BASE (vfwredosum_frm)
BASE (vfwredusum)
BASE (vfwredusum_frm)
BASE (vmv_x)
BASE (vmv_s)
BASE (vfmv_f)
BASE (vfmv_s)
BASE (vslideup)
BASE (vslidedown)
BASE (vslide1up)
BASE (vslide1down)
BASE (vfslide1up)
BASE (vfslide1down)
BASE (vrgather)
BASE (vrgatherei16)
BASE (vcompress)
BASE (vundefined)
BASE (vreinterpret)
BASE (vlmul_ext)
BASE (vlmul_trunc)
BASE (vset)
BASE (vget)
BASE (vcreate)
BASE (read_vl)
BASE (vleff)
BASE (vlenb)
BASE (vlseg)
BASE (vsseg)
BASE (vlsseg)
BASE (vssseg)
BASE (vluxseg)
BASE (vloxseg)
BASE (vsuxseg)
BASE (vsoxseg)
BASE (vlsegff)
BASE (vlb)
BASE (vlh)
BASE (vlw)
BASE (vlbu)
BASE (vlhu)
BASE (vlwu)
BASE (vsb)
BASE (vsh)
BASE (vsw)
BASE (vlsb)
BASE (vlsh)
BASE (vlsw)
BASE (vlsbu)
BASE (vlshu)
BASE (vlswu)
BASE (vssb)
BASE (vssh)
BASE (vssw)
BASE (vlxb)
BASE (vlxh)
BASE (vlxw)
BASE (vlxbu)
BASE (vlxhu)
BASE (vlxwu)
BASE (vsxb)
BASE (vsxh)
BASE (vsxw)
BASE (vsuxb)
BASE (vsuxh)
BASE (vsuxw)
BASE (vext_x_v)
/* Crypto vector */
BASE (vandn)
BASE (vbrev)
BASE (vbrev8)
BASE (vrev8)
BASE (vclz)
BASE (vctz)
BASE (vrol)
BASE (vror)
BASE (vwsll)
BASE (vclmul)
BASE (vclmulh)
BASE (vghsh)
BASE (vgmul)
BASE (vaesef)
BASE (vaesem)
BASE (vaesdf)
BASE (vaesdm)
BASE (vaesz)
BASE (vaeskf1)
BASE (vaeskf2)
BASE (vsha2ms)
BASE (vsha2ch)
BASE (vsha2cl)
BASE (vsm4k)
BASE (vsm4r)
BASE (vsm3me)
BASE (vsm3c)
/* Zvfbfmin */
BASE (vfncvtbf16_f)
BASE (vfncvtbf16_f_frm)
BASE (vfwcvtbf16_f)
/* Zvfbfwma */
BASE (vfwmaccbf16)
BASE (vfwmaccbf16_frm)
} // end namespace riscv_vector
