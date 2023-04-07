/* function_base implementation for RISC-V 'V' Extension for GNU compiler.
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
    machine_mode inner_mode = GET_MODE_INNER (mode);
    /* SEW.  */
    e.add_input_operand (Pmode,
			 gen_int_mode (GET_MODE_BITSIZE (inner_mode), Pmode));

    /* LMUL.  */
    e.add_input_operand (Pmode, gen_int_mode (get_vlmul (mode), Pmode));

    /* TA.  */
    e.add_input_operand (Pmode, gen_int_mode (1, Pmode));

    /* MU.  */
    e.add_input_operand (Pmode, gen_int_mode (0, Pmode));
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
    return pred != PRED_TYPE_none && pred != PRED_TYPE_mu;
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
template<rtx_code CODE>
class binop : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    switch (e.op_info->op)
      {
      case OP_TYPE_vx:
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
template<rtx_code CODE>
class unop : public function_base
{
public:
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

/* Implements vwadd/vwsub/vwmul/vfwadd/vfwsub/vfwmul.  */
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
	return e.use_exact_insn (
	  code_for_pred_single_widen (CODE1, CODE2, e.vector_mode ()));
      case OP_TYPE_wx:
	return e.use_exact_insn (
	  code_for_pred_single_widen_scalar (CODE1, CODE2, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
  }
};
template<rtx_code CODE>
class widen_binop<CODE, FLOAT_EXTEND> : public function_base
{
public:
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
	return e.use_exact_insn (
	  code_for_pred_single_widen (CODE, e.vector_mode ()));
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
	  else if (CODE == EQ || CODE == NE)
	    return e.use_compare_insn (CODE, code_for_pred_eqne_scalar (
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
	   || pred == PRED_TYPE_tumu;
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
	   || pred == PRED_TYPE_tumu;
  }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_series (e.vector_mode ()));
  }
};

/* Implements vfrsub/vfrdiv.  */
template<rtx_code CODE>
class reverse_binop : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_reverse_scalar (CODE, e.vector_mode ()));
  }
};

class vfmacc : public function_base
{
public:
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

class vfnmsac : public function_base
{
public:
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

class vfmadd : public function_base
{
public:
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

class vfnmsub : public function_base
{
public:
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

class vfnmacc : public function_base
{
public:
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

class vfmsac : public function_base
{
public:
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

class vfnmadd : public function_base
{
public:
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

class vfmsub : public function_base
{
public:
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

class vfwmacc : public function_base
{
public:
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

class vfwnmacc : public function_base
{
public:
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

class vfwmsac : public function_base
{
public:
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

class vfwnmsac : public function_base
{
public:
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

/* Implements vfsqrt7/vfrec7/vfclass/vfsgnj/vfsgnjn/vfsgnjx.  */
template<int UNSPEC>
class float_misc : public function_base
{
public:
  rtx expand (function_expander &e) const override
  {
    if (e.op_info->op == OP_TYPE_vf)
      return e.use_exact_insn (code_for_pred_scalar (UNSPEC, e.vector_mode ()));
    if (e.op_info->op == OP_TYPE_vv || e.op_info->op == OP_TYPE_v)
      return e.use_exact_insn (code_for_pred (UNSPEC, e.vector_mode ()));
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
	  if (CODE == EQ || CODE == NE)
	    return e.use_compare_insn (CODE, code_for_pred_eqne_scalar (
					       e.vector_mode ()));
	  else
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
template<int UNSPEC>
class vfcvt_x : public function_base
{
public:
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

class vfcvt_f : public function_base
{
public:
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
template<int UNSPEC>
class vfwcvt_x : public function_base
{
public:
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
template<int UNSPEC>
class vfncvt_x : public function_base
{
public:
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

class vfncvt_f : public function_base
{
public:
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
template<rtx_code CODE>
class reducop : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_reduc (CODE, e.vector_mode (), e.vector_mode ()));
  }
};

/* Implements widen reduction instructions.  */
template<int UNSPEC>
class widen_reducop : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_widen_reduc_plus (UNSPEC,
							     e.vector_mode (),
							     e.vector_mode ()));
  }
};

/* Implements floating-point reduction instructions.  */
template<int UNSPEC>
class freducop : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (
      code_for_pred_reduc_plus (UNSPEC, e.vector_mode (), e.vector_mode ()));
  }
};

/* Implements widening floating-point reduction instructions.  */
template<int UNSPEC>
class widen_freducop : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_widen_reduc_plus (UNSPEC,
							     e.vector_mode (),
							     e.vector_mode ()));
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
    e.add_input_operand (0);
    switch (e.op_info->ret.base_type)
      {
      case RVV_BASE_vlmul_ext_x2:
	return e.generate_insn (
	  code_for_vlmul_extx2 (e.vector_mode ()));
      case RVV_BASE_vlmul_ext_x4:
	return e.generate_insn (
	  code_for_vlmul_extx4 (e.vector_mode ()));
      case RVV_BASE_vlmul_ext_x8:
	return e.generate_insn (
	  code_for_vlmul_extx8 (e.vector_mode ()));
      case RVV_BASE_vlmul_ext_x16:
	return e.generate_insn (
	  code_for_vlmul_extx16 (e.vector_mode ()));
      case RVV_BASE_vlmul_ext_x32:
	return e.generate_insn (
	  code_for_vlmul_extx32 (e.vector_mode ()));
      case RVV_BASE_vlmul_ext_x64:
	return e.generate_insn (
	  code_for_vlmul_extx64 (e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
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

  rtx expand (function_expander &e) const override
  {
    rtx dest = expand_normal (CALL_EXPR_ARG (e.exp, 0));
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

  rtx expand (function_expander &e) const override
  {
    rtx src = expand_normal (CALL_EXPR_ARG (e.exp, 0));
    rtx index = expand_normal (CALL_EXPR_ARG (e.exp, 1));
    poly_int64 offset = INTVAL (index) * GET_MODE_SIZE (GET_MODE (e.target));
    rtx subreg
      = simplify_gen_subreg (GET_MODE (e.target), src, GET_MODE (src), offset);
    return subreg;
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

  gimple *fold (gimple_folder &f) const override
  {
    /* fold vleff (const *base, size_t *new_vl, size_t vl)

       ====> vleff (const *base, size_t vl)
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
	   vleff instruction directly. */
	return repl;
      }

    tree tmp_var = create_tmp_var (size_type_node, "new_vl");
    tree decl = get_read_vl_decl ();
    gimple *g = gimple_build_call (decl, 0);
    gimple_call_set_lhs (g, tmp_var);
    tree indirect
      = fold_build2 (MEM_REF, size_type_node,
		     gimple_call_arg (f.call,
				      gimple_call_num_args (f.call) - 2),
		     build_int_cst (build_pointer_type (size_type_node), 0));
    gassign *assign = gimple_build_assign (indirect, tmp_var);

    gsi_insert_after (f.gsi, assign, GSI_SAME_STMT);
    gsi_insert_after (f.gsi, g, GSI_SAME_STMT);
    return repl;
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
static CONSTEXPR const binop<PLUS> vfadd_obj;
static CONSTEXPR const binop<MINUS> vfsub_obj;
static CONSTEXPR const reverse_binop<MINUS> vfrsub_obj;
static CONSTEXPR const widen_binop<PLUS> vfwadd_obj;
static CONSTEXPR const widen_binop<MINUS> vfwsub_obj;
static CONSTEXPR const binop<MULT> vfmul_obj;
static CONSTEXPR const binop<DIV> vfdiv_obj;
static CONSTEXPR const reverse_binop<DIV> vfrdiv_obj;
static CONSTEXPR const widen_binop<MULT> vfwmul_obj;
static CONSTEXPR const vfmacc vfmacc_obj;
static CONSTEXPR const vfnmsac vfnmsac_obj;
static CONSTEXPR const vfmadd vfmadd_obj;
static CONSTEXPR const vfnmsub vfnmsub_obj;
static CONSTEXPR const vfnmacc vfnmacc_obj;
static CONSTEXPR const vfmsac vfmsac_obj;
static CONSTEXPR const vfnmadd vfnmadd_obj;
static CONSTEXPR const vfmsub vfmsub_obj;
static CONSTEXPR const vfwmacc vfwmacc_obj;
static CONSTEXPR const vfwnmacc vfwnmacc_obj;
static CONSTEXPR const vfwmsac vfwmsac_obj;
static CONSTEXPR const vfwnmsac vfwnmsac_obj;
static CONSTEXPR const unop<SQRT> vfsqrt_obj;
static CONSTEXPR const float_misc<UNSPEC_VFRSQRT7> vfrsqrt7_obj;
static CONSTEXPR const float_misc<UNSPEC_VFREC7> vfrec7_obj;
static CONSTEXPR const binop<SMIN> vfmin_obj;
static CONSTEXPR const binop<SMAX> vfmax_obj;
static CONSTEXPR const float_misc<UNSPEC_VCOPYSIGN> vfsgnj_obj;
static CONSTEXPR const float_misc<UNSPEC_VNCOPYSIGN> vfsgnjn_obj;
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
static CONSTEXPR const vfcvt_x<UNSPEC_UNSIGNED_VFCVT> vfcvt_xu_obj;
static CONSTEXPR const vfcvt_rtz_x<FIX> vfcvt_rtz_x_obj;
static CONSTEXPR const vfcvt_rtz_x<UNSIGNED_FIX> vfcvt_rtz_xu_obj;
static CONSTEXPR const vfcvt_f vfcvt_f_obj;
static CONSTEXPR const vfwcvt_x<UNSPEC_VFCVT> vfwcvt_x_obj;
static CONSTEXPR const vfwcvt_x<UNSPEC_UNSIGNED_VFCVT> vfwcvt_xu_obj;
static CONSTEXPR const vfwcvt_rtz_x<FIX> vfwcvt_rtz_x_obj;
static CONSTEXPR const vfwcvt_rtz_x<UNSIGNED_FIX> vfwcvt_rtz_xu_obj;
static CONSTEXPR const vfwcvt_f vfwcvt_f_obj;
static CONSTEXPR const vfncvt_x<UNSPEC_VFCVT> vfncvt_x_obj;
static CONSTEXPR const vfncvt_x<UNSPEC_UNSIGNED_VFCVT> vfncvt_xu_obj;
static CONSTEXPR const vfncvt_rtz_x<FIX> vfncvt_rtz_x_obj;
static CONSTEXPR const vfncvt_rtz_x<UNSIGNED_FIX> vfncvt_rtz_xu_obj;
static CONSTEXPR const vfncvt_f vfncvt_f_obj;
static CONSTEXPR const vfncvt_rod_f vfncvt_rod_f_obj;
static CONSTEXPR const reducop<PLUS> vredsum_obj;
static CONSTEXPR const reducop<UMAX> vredmaxu_obj;
static CONSTEXPR const reducop<SMAX> vredmax_obj;
static CONSTEXPR const reducop<UMIN> vredminu_obj;
static CONSTEXPR const reducop<SMIN> vredmin_obj;
static CONSTEXPR const reducop<AND> vredand_obj;
static CONSTEXPR const reducop<IOR> vredor_obj;
static CONSTEXPR const reducop<XOR> vredxor_obj;
static CONSTEXPR const widen_reducop<UNSPEC_WREDUC_SUM> vwredsum_obj;
static CONSTEXPR const widen_reducop<UNSPEC_WREDUC_USUM> vwredsumu_obj;
static CONSTEXPR const freducop<UNSPEC_UNORDERED> vfredusum_obj;
static CONSTEXPR const freducop<UNSPEC_ORDERED> vfredosum_obj;
static CONSTEXPR const reducop<SMAX> vfredmax_obj;
static CONSTEXPR const reducop<SMIN> vfredmin_obj;
static CONSTEXPR const widen_freducop<UNSPEC_UNORDERED> vfwredusum_obj;
static CONSTEXPR const widen_freducop<UNSPEC_ORDERED> vfwredosum_obj;
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
static CONSTEXPR const read_vl read_vl_obj;
static CONSTEXPR const vleff vleff_obj;
static CONSTEXPR const vlenb vlenb_obj;

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
BASE (vfsub)
BASE (vfrsub)
BASE (vfwadd)
BASE (vfwsub)
BASE (vfmul)
BASE (vfdiv)
BASE (vfrdiv)
BASE (vfwmul)
BASE (vfmacc)
BASE (vfnmsac)
BASE (vfmadd)
BASE (vfnmsub)
BASE (vfnmacc)
BASE (vfmsac)
BASE (vfnmadd)
BASE (vfmsub)
BASE (vfwmacc)
BASE (vfwnmacc)
BASE (vfwmsac)
BASE (vfwnmsac)
BASE (vfsqrt)
BASE (vfrsqrt7)
BASE (vfrec7)
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
BASE (vfcvt_xu)
BASE (vfcvt_rtz_x)
BASE (vfcvt_rtz_xu)
BASE (vfcvt_f)
BASE (vfwcvt_x)
BASE (vfwcvt_xu)
BASE (vfwcvt_rtz_x)
BASE (vfwcvt_rtz_xu)
BASE (vfwcvt_f)
BASE (vfncvt_x)
BASE (vfncvt_xu)
BASE (vfncvt_rtz_x)
BASE (vfncvt_rtz_xu)
BASE (vfncvt_f)
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
BASE (vfredosum)
BASE (vfredmax)
BASE (vfredmin)
BASE (vfwredosum)
BASE (vfwredusum)
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
BASE (read_vl)
BASE (vleff)
BASE (vlenb)

} // end namespace riscv_vector
