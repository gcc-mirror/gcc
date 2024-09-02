/* ACLE support for Arm MVE (__ARM_FEATURE_MVE intrinsics)
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "gimple.h"
#include "emit-rtl.h"
#include "arm-mve-builtins.h"
#include "arm-mve-builtins-shapes.h"
#include "arm-mve-builtins-base.h"
#include "arm-mve-builtins-functions.h"

using namespace arm_mve;

namespace {

/* Implements vdup_* intrinsics.  */
class vdupq_impl : public quiet<function_base>
{
public:
  CONSTEXPR vdupq_impl (int unspec_for_m_n_sint,
			int unspec_for_m_n_uint,
			int unspec_for_m_n_fp)
    : m_unspec_for_m_n_sint (unspec_for_m_n_sint),
      m_unspec_for_m_n_uint (unspec_for_m_n_uint),
      m_unspec_for_m_n_fp (unspec_for_m_n_fp)
  {}
  int m_unspec_for_m_n_sint;
  int m_unspec_for_m_n_uint;
  int m_unspec_for_m_n_fp;

  rtx expand (function_expander &e) const override
  {
    gcc_assert (e.mode_suffix_id == MODE_n);

    insn_code code;
    machine_mode mode = e.vector_mode (0);

    switch (e.pred)
    {
    case PRED_none:
      /* No predicate, _n suffix.  */
      code = code_for_mve_vdupq_n (mode);
      return e.use_exact_insn (code);

    case PRED_m:
    case PRED_x:
      /* "m" or "x" predicate, _n suffix.  */
      if (e.type_suffix (0).integer_p)
	if (e.type_suffix (0).unsigned_p)
	  code = code_for_mve_q_m_n (m_unspec_for_m_n_uint,
				     m_unspec_for_m_n_uint, mode);
	else
	  code = code_for_mve_q_m_n (m_unspec_for_m_n_sint,
				     m_unspec_for_m_n_sint, mode);
      else
	code = code_for_mve_q_m_n_f (m_unspec_for_m_n_fp, mode);

      if (e.pred == PRED_m)
	return e.use_cond_insn (code, 0);
      else
	return e.use_pred_x_insn (code);

    default:
      gcc_unreachable ();
    }
  }
};

/* Implements vreinterpretq_* intrinsics.  */
class vreinterpretq_impl : public quiet<function_base>
{
  gimple *
  fold (gimple_folder &f) const override
  {
    /* We should punt to rtl if the effect of the reinterpret on
       registers does not conform to GCC's endianness model like we do
       on aarch64, but MVE intrinsics are not currently supported on
       big-endian.  For this, we'd need to handle big-endian properly
       in the .md file, like we do on aarch64 with
       define_insn_and_split "*aarch64_sve_reinterpret<mode>".  */
    gcc_assert (targetm.can_change_mode_class (f.vector_mode (0),
					       f.vector_mode (1),
					       VFP_REGS));

    /* Otherwise vreinterpret corresponds directly to a VIEW_CONVERT_EXPR
       reinterpretation.  */
    tree rhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (f.lhs),
		       gimple_call_arg (f.call, 0));
    return gimple_build_assign (f.lhs, VIEW_CONVERT_EXPR, rhs);
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    return e.use_exact_insn (code_for_arm_mve_reinterpret (mode));
  }
};

/* Implements vuninitializedq_* intrinsics.  */
class vuninitializedq_impl : public quiet<function_base>
{

  rtx
  expand (function_expander &e) const override
  {
    rtx target = e.get_reg_target ();
    emit_clobber (copy_rtx (target));
    return target;
  }
};

class vld1_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.type_suffix (0).float_p)
      icode = code_for_mve_vld1q_f(e.vector_mode (0));
    else
      {
	if (e.type_suffix (0).unsigned_p)
	  icode = code_for_mve_vld1q(VLD1Q_U,
				     e.vector_mode (0));
	else
	  icode = code_for_mve_vld1q(VLD1Q_S,
				     e.vector_mode (0));
      }
    return e.use_contiguous_load_insn (icode);
  }
};

class vst1_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.type_suffix (0).float_p)
      icode = code_for_mve_vst1q_f(e.vector_mode (0));
    else
      {
	if (e.type_suffix (0).unsigned_p)
	  icode = code_for_mve_vst1q(VST1Q_U,
				     e.vector_mode (0));
	else
	  icode = code_for_mve_vst1q(VST1Q_S,
				     e.vector_mode (0));
      }
    return e.use_contiguous_store_insn (icode);
  }
};

  /* Implements vctp8q, vctp16q, vctp32q and vctp64q intrinsics.  */
class vctpq_impl : public function_base
{
public:
  CONSTEXPR vctpq_impl (machine_mode mode)
    : m_mode (mode)
  {}

  /* Mode this intrinsic operates on.  */
  machine_mode m_mode;

  rtx
  expand (function_expander &e) const override
  {
    insn_code code;
    rtx target;

    if (e.mode_suffix_id != MODE_none)
      gcc_unreachable ();

    switch (e.pred)
      {
      case PRED_none:
	/* No predicate, no suffix.  */
	code = code_for_mve_vctpq (m_mode, m_mode);
	target = e.use_exact_insn (code);
	break;

      case PRED_m:
	/* No suffix, "m" predicate.  */
	code = code_for_mve_vctpq_m (m_mode, m_mode);
	target = e.use_cond_insn (code, 0);
	break;

      default:
	gcc_unreachable ();
      }

    rtx HItarget = gen_reg_rtx (HImode);
    emit_move_insn (HItarget, gen_lowpart (HImode, target));
    return HItarget;
  }
};

  /* Implements vcvtq intrinsics.  */
class vcvtq_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    insn_code code;
    machine_mode target_mode = e.vector_mode (0);
    int unspec;
    switch (e.pred)
      {
      case PRED_none:
	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    /* No predicate, no suffix.  */
	    if (e.type_suffix (0).integer_p)
	      {
		unspec = (e.type_suffix (0).unsigned_p
			  ? VCVTQ_FROM_F_U
			  : VCVTQ_FROM_F_S);
		code = code_for_mve_q_from_f (unspec, unspec, target_mode);
	      }
	    else
	      {
		unspec = (e.type_suffix (1).unsigned_p
			  ? VCVTQ_TO_F_U
			  : VCVTQ_TO_F_S);
		code = code_for_mve_q_to_f (unspec, unspec, target_mode);
	      }
	    break;

	  case MODE_n:
	    /* No predicate, _n suffix.  */
	    if (e.type_suffix (0).integer_p)
	      {
		unspec = (e.type_suffix (0).unsigned_p
			  ? VCVTQ_N_FROM_F_U
			  : VCVTQ_N_FROM_F_S);
		code = code_for_mve_q_n_from_f (unspec, unspec, target_mode);
	      }
	    else
	      {
		unspec = (e.type_suffix (1).unsigned_p
			  ? VCVTQ_N_TO_F_U
			  : VCVTQ_N_TO_F_S);
		code = code_for_mve_q_n_to_f (unspec, unspec, target_mode);
	      }
	    break;

	  default:
	    gcc_unreachable ();
	  }
	return e.use_exact_insn (code);

      case PRED_m:
      case PRED_x:
	switch (e.mode_suffix_id)
	  {
	  case MODE_none:
	    /* No suffix, "m" or "x" predicate.  */
	    if (e.type_suffix (0).integer_p)
	      {
		unspec = (e.type_suffix (0).unsigned_p
			  ? VCVTQ_M_FROM_F_U
			  : VCVTQ_M_FROM_F_S);
		code = code_for_mve_q_m_from_f (unspec, unspec, target_mode);
	      }
	    else
	      {
		unspec = (e.type_suffix (1).unsigned_p
			  ? VCVTQ_M_TO_F_U
			  : VCVTQ_M_TO_F_S);
		code = code_for_mve_q_m_to_f (unspec, unspec, target_mode);
	      }
	    break;

	  case MODE_n:
	    /* _n suffix, "m" or "x" predicate.  */
	    if (e.type_suffix (0).integer_p)
	      {
		unspec = (e.type_suffix (0).unsigned_p
			  ? VCVTQ_M_N_FROM_F_U
			  : VCVTQ_M_N_FROM_F_S);
		code = code_for_mve_q_m_n_from_f (unspec, unspec, target_mode);
	      }
	    else
	      {
		unspec = (e.type_suffix (1).unsigned_p
			  ? VCVTQ_M_N_TO_F_U
			  : VCVTQ_M_N_TO_F_S);
		code = code_for_mve_q_m_n_to_f (unspec, unspec, target_mode);
	      }
	    break;

	  default:
	    gcc_unreachable ();
	  }
	if (e.pred == PRED_m)
	  return e.use_cond_insn (code, 0);
	else
	  return e.use_pred_x_insn (code);

      default:
	gcc_unreachable ();
      }

    gcc_unreachable ();
  }
};

  /* Implements vcvt[bt]q_f32_f16 and vcvt[bt]q_f16_f32
     intrinsics.  */
class vcvtxq_impl : public function_base
{
public:
  CONSTEXPR vcvtxq_impl (int unspec_f16_f32, int unspec_for_m_f16_f32,
			 int unspec_f32_f16, int unspec_for_m_f32_f16)
    : m_unspec_f16_f32 (unspec_f16_f32),
      m_unspec_for_m_f16_f32 (unspec_for_m_f16_f32),
      m_unspec_f32_f16 (unspec_f32_f16),
      m_unspec_for_m_f32_f16 (unspec_for_m_f32_f16)
  {}

  /* The unspec code associated with vcvt[bt]q.  */
  int m_unspec_f16_f32;
  int m_unspec_for_m_f16_f32;
  int m_unspec_f32_f16;
  int m_unspec_for_m_f32_f16;

  rtx
  expand (function_expander &e) const override
  {
    insn_code code;
    switch (e.pred)
      {
      case PRED_none:
	/* No predicate.  */
	if (e.type_suffix (0).element_bits == 16)
	  code = code_for_mve_q_f16_f32v8hf (m_unspec_f16_f32);
	else
	  code = code_for_mve_q_f32_f16v4sf (m_unspec_f32_f16);
	return e.use_exact_insn (code);

      case PRED_m:
      case PRED_x:
	/* "m" or "x" predicate.  */
	if (e.type_suffix (0).element_bits == 16)
	  code = code_for_mve_q_m_f16_f32v8hf (m_unspec_for_m_f16_f32);
	else
	  code = code_for_mve_q_m_f32_f16v4sf (m_unspec_for_m_f32_f16);

	if (e.pred == PRED_m)
	  return e.use_cond_insn (code, 0);
	else
	  return e.use_pred_x_insn (code);

      default:
	gcc_unreachable ();
      }

    gcc_unreachable ();
  }
};

/* Map the vidup / vddup function directly to CODE (UNSPEC, M) where M is the
   vector mode associated with type suffix 0.  We need this special case
   because in MODE_wb the builtins derefrence the first parameter and update
   its contents.  We also have to insert the two additional parameters needed
   by the builtins compared to the intrinsics.  In wrapping mode, we have to
   match the 'hack' to make sure the 'wrap' parameters is in odd register.  */
class viddup_impl : public function_base
{
public:
  CONSTEXPR viddup_impl (bool inc_dec, bool wrap)
    : m_inc_dec (inc_dec), m_wrap (wrap)
  {}

  /* Increment (true) or decrement (false).  */
  bool m_inc_dec;
  /* v[id]wdup (true) or v[id]dup (false).  */
  bool m_wrap;

  unsigned int
  call_properties (const function_instance &fi) const override
  {
    if (fi.mode_suffix_id == MODE_wb)
      return CP_WRITE_MEMORY | CP_READ_MEMORY;
    else
      return 0;
  }

  tree
  memory_scalar_type (const function_instance &) const override
  {
    return get_typenode_from_name (UINT32_TYPE);
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;
    rtx insns, offset_ptr;
    rtx new_offset;
    int offset_arg_no;

    if (! e.type_suffix (0).integer_p)
      gcc_unreachable ();

    if ((e.mode_suffix_id != MODE_n)
	&& (e.mode_suffix_id != MODE_wb))
      gcc_unreachable ();

    offset_arg_no = (e.pred == PRED_m) ? 1 : 0;

    /* In _wb mode, the start offset is passed via a pointer,
       dereference it.  */
    if (e.mode_suffix_id == MODE_wb)
      {
	rtx offset = gen_reg_rtx (SImode);
	offset_ptr = e.args[offset_arg_no];
	emit_insn (gen_rtx_SET (offset, gen_rtx_MEM (SImode, offset_ptr)));
	e.args[offset_arg_no] = offset;
      }

    /* We have to shuffle parameters because the builtin needs additional
       arguments:
       - the updated "new_offset"
       - total increment (incr * number of lanes) in the non-wrapping case
       - hack to pass wrap in the top end of DImode operand so that it is
         actually in a odd register  */
    new_offset = gen_reg_rtx (SImode);
    e.args.quick_insert (offset_arg_no, new_offset);

    if (m_wrap)
      {
	rtx wrap = gen_reg_rtx (DImode);
	emit_insn (gen_rtx_SET (gen_rtx_SUBREG (SImode, wrap, 4),
				e.args[offset_arg_no + 2]));
	emit_insn (gen_rtx_SET (gen_rtx_SUBREG (SImode, wrap, 0),
				GEN_INT (0)));
	e.args[offset_arg_no + 2] = wrap;
      }
    else
      {
	rtx incr = e.args[offset_arg_no + 2];
	rtx total_incr = gen_int_mode (INTVAL (incr)
				       * GET_MODE_NUNITS (e.vector_mode (0)),
				       SImode);
	e.args.quick_push (total_incr);
      }

    /* _wb mode uses the _n builtins and adds code to update the
       offset.  */
    switch (e.pred)
      {
      case PRED_none:
	/* No predicate.  */
	code = m_wrap
	  ? (m_inc_dec
	     ? code_for_mve_q_wb_u_insn (VIWDUPQ, mode)
	     : code_for_mve_q_wb_u_insn (VDWDUPQ, mode))
	  : (m_inc_dec
	     ? code_for_mve_q_u_insn (VIDUPQ, mode)
	     : code_for_mve_q_u_insn (VDDUPQ, mode));
	insns = e.use_exact_insn (code);
	break;

      case PRED_m:
      case PRED_x:
	/* "m" or "x" predicate.  */
	code = m_wrap
	  ? (m_inc_dec
	     ? code_for_mve_q_m_wb_u_insn (VIWDUPQ_M, mode)
	     : code_for_mve_q_m_wb_u_insn (VDWDUPQ_M, mode))
	  : (m_inc_dec
	     ? code_for_mve_q_m_wb_u_insn (VIDUPQ_M, mode)
	     : code_for_mve_q_m_wb_u_insn (VDDUPQ_M, mode));

	if (e.pred == PRED_m)
	  insns = e.use_cond_insn (code, 0);
	else
	  insns = e.use_pred_x_insn (code);
	break;

      default:
	gcc_unreachable ();
      }

    /* Update offset as appropriate.  */
    if (e.mode_suffix_id == MODE_wb)
      emit_insn (gen_rtx_SET (gen_rtx_MEM (Pmode, offset_ptr), new_offset));

    return insns;
  }
};

/* Map the vshlc function directly to CODE (UNSPEC, M) where M is the vector
   mode associated with type suffix 0.  We need this special case because the
   intrinsics derefrence the second parameter and update its contents.  */
class vshlc_impl : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY | CP_READ_MEMORY;
  }

  tree
  memory_scalar_type (const function_instance &) const override
  {
    return get_typenode_from_name (UINT32_TYPE);
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    insn_code code;
    rtx insns, carry_ptr, carry, new_carry;
    int carry_arg_no;

    if (! e.type_suffix (0).integer_p)
      gcc_unreachable ();

    if (e.mode_suffix_id != MODE_none)
      gcc_unreachable ();

    carry_arg_no = 1;

    carry = gen_reg_rtx (SImode);
    carry_ptr = e.args[carry_arg_no];
    emit_insn (gen_rtx_SET (carry, gen_rtx_MEM (SImode, carry_ptr)));
    e.args[carry_arg_no] = carry;

    new_carry = gen_reg_rtx (SImode);
    e.args.quick_insert (0, new_carry);

    switch (e.pred)
      {
      case PRED_none:
	/* No predicate.  */
	code = e.type_suffix (0).unsigned_p
	  ? code_for_mve_vshlcq (VSHLCQ_U, mode)
	  : code_for_mve_vshlcq (VSHLCQ_S, mode);
	insns = e.use_exact_insn (code);
	break;

      case PRED_m:
	/* "m" predicate.  */
	code = e.type_suffix (0).unsigned_p
	  ? code_for_mve_vshlcq_m (VSHLCQ_M_U, mode)
	  : code_for_mve_vshlcq_m (VSHLCQ_M_S, mode);
	insns = e.use_cond_insn (code, 0);
	break;

      default:
	gcc_unreachable ();
      }

    /* Update carry.  */
    emit_insn (gen_rtx_SET (gen_rtx_MEM (Pmode, carry_ptr), new_carry));

    return insns;
  }
};

/* Map the vadc and similar functions directly to CODE (UNSPEC, UNSPEC).  Take
   care of the implicit carry argument.  */
class vadc_vsbc_impl : public function_base
{
public:
  CONSTEXPR vadc_vsbc_impl (bool init_carry, bool add)
    : m_init_carry (init_carry), m_add (add)
  {}

  /* Initialize carry with 0 (vadci).  */
  bool m_init_carry;
  /* Add (true) or Sub (false).  */
  bool m_add;

  unsigned int
  call_properties (const function_instance &) const override
  {
    unsigned int flags = CP_WRITE_MEMORY | CP_READ_FPCR;
    if (!m_init_carry)
      flags |= CP_READ_MEMORY;
    return flags;
  }

  tree
  memory_scalar_type (const function_instance &) const override
  {
    /* carry is "unsigned int".  */
    return get_typenode_from_name ("unsigned int");
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code code;
    rtx insns, carry_ptr, carry_out;
    int carry_out_arg_no;
    int unspec;

    if (! e.type_suffix (0).integer_p)
      gcc_unreachable ();

    if (e.mode_suffix_id != MODE_none)
      gcc_unreachable ();

    /* Remove carry from arguments, it is implicit for the builtin.  */
    switch (e.pred)
      {
      case PRED_none:
	carry_out_arg_no = 2;
	break;

      case PRED_m:
	carry_out_arg_no = 3;
	break;

      default:
	gcc_unreachable ();
      }

    carry_ptr = e.args[carry_out_arg_no];
    e.args.ordered_remove (carry_out_arg_no);

    if (!m_init_carry)
      {
	/* Prepare carry in:
	   set_fpscr ( (fpscr & ~0x20000000u)
		       | ((*carry & 1u) << 29) )  */
	rtx carry_in = gen_reg_rtx (SImode);
	rtx fpscr = gen_reg_rtx (SImode);
	emit_insn (gen_get_fpscr_nzcvqc (fpscr));
	emit_insn (gen_rtx_SET (carry_in, gen_rtx_MEM (SImode, carry_ptr)));

	emit_insn (gen_rtx_SET (carry_in,
				gen_rtx_ASHIFT (SImode,
						carry_in,
						GEN_INT (29))));
	emit_insn (gen_rtx_SET (carry_in,
				gen_rtx_AND (SImode,
					     carry_in,
					     GEN_INT (0x20000000))));
	emit_insn (gen_rtx_SET (fpscr,
				gen_rtx_AND (SImode,
					     fpscr,
					     GEN_INT (~0x20000000))));
	emit_insn (gen_rtx_SET (carry_in,
				gen_rtx_IOR (SImode,
					     carry_in,
					     fpscr)));
	emit_insn (gen_set_fpscr_nzcvqc (carry_in));
      }

    switch (e.pred)
      {
      case PRED_none:
	/* No predicate.  */
	unspec = m_add
	  ? (m_init_carry
	     ? (e.type_suffix (0).unsigned_p
		? VADCIQ_U
		: VADCIQ_S)
	     : (e.type_suffix (0).unsigned_p
		? VADCQ_U
		: VADCQ_S))
	  : (m_init_carry
	     ? (e.type_suffix (0).unsigned_p
		? VSBCIQ_U
		: VSBCIQ_S)
	     : (e.type_suffix (0).unsigned_p
		? VSBCQ_U
		: VSBCQ_S));
	code = code_for_mve_q_v4si (unspec, unspec);
	insns = e.use_exact_insn (code);
	break;

      case PRED_m:
	/* "m" predicate.  */
	unspec = m_add
	  ? (m_init_carry
	     ? (e.type_suffix (0).unsigned_p
		? VADCIQ_M_U
		: VADCIQ_M_S)
	     : (e.type_suffix (0).unsigned_p
		? VADCQ_M_U
		: VADCQ_M_S))
	  : (m_init_carry
	     ? (e.type_suffix (0).unsigned_p
		? VSBCIQ_M_U
		: VSBCIQ_M_S)
	     : (e.type_suffix (0).unsigned_p
		? VSBCQ_M_U
		: VSBCQ_M_S));
	code = code_for_mve_q_m_v4si (unspec, unspec);
	insns = e.use_cond_insn (code, 0);
	break;

      default:
	gcc_unreachable ();
      }

    /* Update carry_out.  */
    carry_out = gen_reg_rtx (SImode);
    emit_insn (gen_get_fpscr_nzcvqc (carry_out));
    emit_insn (gen_rtx_SET (carry_out,
			    gen_rtx_LSHIFTRT (SImode,
					      carry_out,
					      GEN_INT (29))));
    emit_insn (gen_rtx_SET (carry_out,
			    gen_rtx_AND (SImode,
					 carry_out,
					 GEN_INT (1))));
    emit_insn (gen_rtx_SET (gen_rtx_MEM (Pmode, carry_ptr), carry_out));

    return insns;
  }
};

} /* end anonymous namespace */

namespace arm_mve {

  /* Helper for builtins with RTX codes, _m predicated and _n overrides.  */
#define FUNCTION_WITH_RTX_M_N(NAME, RTX, UNSPEC) FUNCTION		\
  (NAME, unspec_based_mve_function_exact_insn,				\
   (RTX, RTX, RTX,							\
    UNSPEC##_N_S, UNSPEC##_N_U, UNSPEC##_N_F,				\
    UNSPEC##_M_S, UNSPEC##_M_U, UNSPEC##_M_F,				\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U, UNSPEC##_M_N_F))

  /* Helper for builtins with RTX codes, and _m predicated overrides.  */
#define FUNCTION_WITH_RTX_M(NAME, RTX, UNSPEC) FUNCTION			\
  (NAME, unspec_based_mve_function_exact_insn,				\
   (RTX, RTX, RTX,							\
    -1, -1, -1,								\
    UNSPEC##_M_S, UNSPEC##_M_U, UNSPEC##_M_F,				\
    -1, -1, -1))

  /* Helper for builtins with RTX codes, _m predicated and _n
     overrides, but no floating-point version.  */
#define FUNCTION_WITH_RTX_M_N_NO_F(NAME, RTX, UNSPEC) FUNCTION		\
  (NAME, unspec_based_mve_function_exact_insn,				\
   (RTX, RTX, UNKNOWN,							\
    UNSPEC##_N_S, UNSPEC##_N_U, -1,					\
    UNSPEC##_M_S, UNSPEC##_M_U, -1,					\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U, -1))

  /* Helper for builtins with RTX codes, _m predicated and _n overrides.  */
#define FUNCTION_WITH_RTX_M_N_NO_N_F(NAME, RTX, UNSPEC) FUNCTION	\
  (NAME, unspec_based_mve_function_exact_insn,				\
   (RTX, RTX, RTX,							\
    UNSPEC##_N_S, UNSPEC##_N_U, -1,					\
    UNSPEC##_M_S, UNSPEC##_M_U, UNSPEC##_M_F,				\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U, -1))

  /* Helper for builtins with RTX codes, _m predicated override, but
     no floating-point versions.  */
#define FUNCTION_WITH_RTX_M_NO_F(NAME, RTX_S, RTX_U, UNSPEC) FUNCTION	\
  (NAME, unspec_based_mve_function_exact_insn,				\
   (RTX_S, RTX_U, UNKNOWN,						\
    -1, -1, -1,								\
    UNSPEC##_M_S, UNSPEC##_M_U, -1,					\
    -1, -1, -1))

  /* Helper for builtins without RTX codes, no _m predicated and no _n
     overrides.  */
#define FUNCTION_WITHOUT_M_N(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (UNSPEC##_S, UNSPEC##_U, UNSPEC##_F,					\
    -1, -1, -1,								\
    -1, -1, -1,								\
    -1, -1, -1))

  /* Helper for builtins with only unspec codes, _m predicated and _n
     overrides, but no floating-point version.  */
#define FUNCTION_WITH_M_N_NO_F(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (UNSPEC##_S, UNSPEC##_U, -1,						\
    UNSPEC##_N_S, UNSPEC##_N_U, -1,					\
    UNSPEC##_M_S, UNSPEC##_M_U, -1,					\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U, -1))

  /* Helper for vshl builtins with only unspec codes, _m predicated
     and _n and _r overrides.  */
#define FUNCTION_WITH_M_N_R(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn_vshl,				\
   (UNSPEC##_S, UNSPEC##_U,						\
    UNSPEC##_N_S, UNSPEC##_N_U,						\
    UNSPEC##_M_S, UNSPEC##_M_U,						\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U,					\
    UNSPEC##_M_R_S, UNSPEC##_M_R_U,					\
    UNSPEC##_R_S, UNSPEC##_R_U))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, no _n and no floating-point version.  */
#define FUNCTION_WITHOUT_N_NO_F(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (UNSPEC##_S, UNSPEC##_U, -1,						\
    -1, -1, -1,								\
    UNSPEC##_M_S, UNSPEC##_M_U, -1,					\
    -1, -1, -1))

  /* Helper for builtins with only unspec codes, _m predicated and _n
     overrides, but no unsigned and floating-point versions.  */
#define FUNCTION_WITH_M_N_NO_U_F(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (UNSPEC##_S, -1, -1,							\
    UNSPEC##_N_S, -1, -1,						\
    UNSPEC##_M_S, -1, -1,						\
    UNSPEC##_M_N_S, -1, -1))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, but no _n version.  */
#define FUNCTION_WITHOUT_N(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (UNSPEC##_S, UNSPEC##_U, UNSPEC##_F,					\
    -1, -1, -1,								\
    UNSPEC##_M_S, UNSPEC##_M_U, UNSPEC##_M_F,				\
    -1, -1, -1))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, only _n version.  */
#define FUNCTION_ONLY_N(NAME, UNSPEC) FUNCTION				\
  (NAME, unspec_mve_function_exact_insn,				\
   (-1, -1, -1,								\
    UNSPEC##_N_S, UNSPEC##_N_U, UNSPEC##_N_F,				\
    -1, -1, -1,								\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U, UNSPEC##_M_N_F))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, only _n version, no floating-point.  */
#define FUNCTION_ONLY_N_NO_F(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (-1, -1, -1,								\
    UNSPEC##_N_S, UNSPEC##_N_U, -1,					\
    -1, -1, -1,								\
    UNSPEC##_M_N_S, UNSPEC##_M_N_U, -1))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, only _n version, no unsigned, no floating-point.  */
#define FUNCTION_ONLY_N_NO_U_F(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn,				\
   (-1, -1, -1,								\
    UNSPEC##_N_S, -1, -1,						\
    -1, -1, -1,								\
    UNSPEC##_M_N_S, -1, -1))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, but no _n version, no unsigned and no
     floating-point.  */
#define FUNCTION_WITHOUT_N_NO_U_F(NAME, UNSPEC) FUNCTION		\
  (NAME, unspec_mve_function_exact_insn,				\
   (UNSPEC##_S, -1, -1,							\
    -1, -1, -1,								\
    UNSPEC##_M_S, -1, -1,						\
    -1, -1, -1))

  /* Helper for builtins with only unspec codes, _m predicated
     overrides, only floating-point.  */
#define FUNCTION_ONLY_F(NAME, UNSPEC) FUNCTION				\
  (NAME, unspec_mve_function_exact_insn,				\
   (-1, -1, UNSPEC##_F,							\
    -1, -1, -1,								\
    -1, -1, UNSPEC##_M_F,						\
    -1, -1, -1))

  /* Helper for builtins without RTX codes, _S mode, _p predicated.  */
#define FUNCTION_PRED_P_S(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn_pred_p,				\
   (UNSPEC##_S, -1, -1,							\
    UNSPEC##_P_S, -1, -1))

  /* Helper for builtins without RTX codes, _S and _U modes, _p
     predicated.  */
#define FUNCTION_PRED_P_S_U(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn_pred_p,				\
   (UNSPEC##_S, UNSPEC##_U, -1,						\
    UNSPEC##_P_S, UNSPEC##_P_U, -1))

  /* Helper for builtins without RTX codes, _F mode, _p predicated.  */
#define FUNCTION_PRED_P_F(NAME, UNSPEC) FUNCTION			\
  (NAME, unspec_mve_function_exact_insn_pred_p,				\
   (-1, -1, UNSPEC##_F,							\
    -1, -1, UNSPEC##_P_F))

FUNCTION_PRED_P_S_U (vabavq, VABAVQ)
FUNCTION_WITHOUT_N (vabdq, VABDQ)
FUNCTION (vabsq, unspec_based_mve_function_exact_insn, (ABS, ABS, ABS, -1, -1, -1, VABSQ_M_S, -1, VABSQ_M_F, -1, -1, -1))
FUNCTION (vadciq, vadc_vsbc_impl, (true, true))
FUNCTION (vadcq, vadc_vsbc_impl, (false, true))
FUNCTION_WITH_RTX_M_N (vaddq, PLUS, VADDQ)
FUNCTION_PRED_P_S_U (vaddlvaq, VADDLVAQ)
FUNCTION_PRED_P_S_U (vaddlvq, VADDLVQ)
FUNCTION_PRED_P_S_U (vaddvq, VADDVQ)
FUNCTION_PRED_P_S_U (vaddvaq, VADDVAQ)
FUNCTION_WITH_RTX_M (vandq, AND, VANDQ)
FUNCTION (vbicq, unspec_based_mve_function_exact_insn_vbic, (VBICQ_N_S, VBICQ_N_U, VBICQ_M_S, VBICQ_M_U, VBICQ_M_F, VBICQ_M_N_S, VBICQ_M_N_U))
FUNCTION_ONLY_N (vbrsrq, VBRSRQ)
FUNCTION (vcaddq_rot90, unspec_mve_function_exact_insn_rot, (UNSPEC_VCADD90, UNSPEC_VCADD90, UNSPEC_VCADD90, VCADDQ_ROT90_M, VCADDQ_ROT90_M, VCADDQ_ROT90_M_F))
FUNCTION (vcaddq_rot270, unspec_mve_function_exact_insn_rot, (UNSPEC_VCADD270, UNSPEC_VCADD270, UNSPEC_VCADD270, VCADDQ_ROT270_M, VCADDQ_ROT270_M, VCADDQ_ROT270_M_F))
FUNCTION (vcmlaq, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMLA, -1, -1, VCMLAQ_M_F))
FUNCTION (vcmlaq_rot90, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMLA90, -1, -1, VCMLAQ_ROT90_M_F))
FUNCTION (vcmlaq_rot180, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMLA180, -1, -1, VCMLAQ_ROT180_M_F))
FUNCTION (vcmlaq_rot270, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMLA270, -1, -1, VCMLAQ_ROT270_M_F))
FUNCTION (vcmulq, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMUL, -1, -1, VCMULQ_M_F))
FUNCTION (vcmulq_rot90, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMUL90, -1, -1, VCMULQ_ROT90_M_F))
FUNCTION (vcmulq_rot180, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMUL180, -1, -1, VCMULQ_ROT180_M_F))
FUNCTION (vcmulq_rot270, unspec_mve_function_exact_insn_rot, (-1, -1, UNSPEC_VCMUL270, -1, -1, VCMULQ_ROT270_M_F))
FUNCTION (vhcaddq_rot90, unspec_mve_function_exact_insn_rot, (VHCADDQ_ROT90_S, -1, -1, VHCADDQ_ROT90_M_S, -1, -1))
FUNCTION (vhcaddq_rot270, unspec_mve_function_exact_insn_rot, (VHCADDQ_ROT270_S, -1, -1, VHCADDQ_ROT270_M_S, -1, -1))
FUNCTION_WITHOUT_N_NO_U_F (vclsq, VCLSQ)
FUNCTION (vclzq, unspec_based_mve_function_exact_insn, (CLZ, CLZ, CLZ, -1, -1, -1, VCLZQ_M_S, VCLZQ_M_U, -1, -1, -1 ,-1))
FUNCTION (vcmpeqq, unspec_based_mve_function_exact_insn_vcmp, (EQ, EQ, EQ, VCMPEQQ_M_S, VCMPEQQ_M_U, VCMPEQQ_M_F, VCMPEQQ_M_N_S, VCMPEQQ_M_N_U, VCMPEQQ_M_N_F))
FUNCTION (vcmpneq, unspec_based_mve_function_exact_insn_vcmp, (NE, NE, NE, VCMPNEQ_M_S, VCMPNEQ_M_U, VCMPNEQ_M_F, VCMPNEQ_M_N_S, VCMPNEQ_M_N_U, VCMPNEQ_M_N_F))
FUNCTION (vcmpgeq, unspec_based_mve_function_exact_insn_vcmp, (GE, UNKNOWN, GE, VCMPGEQ_M_S, UNKNOWN, VCMPGEQ_M_F, VCMPGEQ_M_N_S, UNKNOWN, VCMPGEQ_M_N_F))
FUNCTION (vcmpgtq, unspec_based_mve_function_exact_insn_vcmp, (GT, UNKNOWN, GT, VCMPGTQ_M_S, UNKNOWN, VCMPGTQ_M_F, VCMPGTQ_M_N_S, UNKNOWN, VCMPGTQ_M_N_F))
FUNCTION (vcmpleq, unspec_based_mve_function_exact_insn_vcmp, (LE, UNKNOWN, LE, VCMPLEQ_M_S, UNKNOWN, VCMPLEQ_M_F, VCMPLEQ_M_N_S, UNKNOWN, VCMPLEQ_M_N_F))
FUNCTION (vcmpltq, unspec_based_mve_function_exact_insn_vcmp, (LT, UNKNOWN, LT, VCMPLTQ_M_S, UNKNOWN, VCMPLTQ_M_F, VCMPLTQ_M_N_S, UNKNOWN, VCMPLTQ_M_N_F))
FUNCTION (vcmpcsq, unspec_based_mve_function_exact_insn_vcmp, (UNKNOWN, GEU, UNKNOWN, UNKNOWN, VCMPCSQ_M_U, UNKNOWN, UNKNOWN, VCMPCSQ_M_N_U, UNKNOWN))
FUNCTION (vcmphiq, unspec_based_mve_function_exact_insn_vcmp, (UNKNOWN, GTU, UNKNOWN, UNKNOWN, VCMPHIQ_M_U, UNKNOWN, UNKNOWN, VCMPHIQ_M_N_U, UNKNOWN))
FUNCTION_WITHOUT_M_N (vcreateq, VCREATEQ)
FUNCTION (vctp8q, vctpq_impl, (V16BImode))
FUNCTION (vctp16q, vctpq_impl, (V8BImode))
FUNCTION (vctp32q, vctpq_impl, (V4BImode))
FUNCTION (vctp64q, vctpq_impl, (V2QImode))
FUNCTION_WITHOUT_N_NO_F (vcvtaq, VCVTAQ)
FUNCTION (vcvtbq, vcvtxq_impl, (VCVTBQ_F16_F32, VCVTBQ_M_F16_F32, VCVTBQ_F32_F16, VCVTBQ_M_F32_F16))
FUNCTION (vcvtq, vcvtq_impl,)
FUNCTION_WITHOUT_N_NO_F (vcvtmq, VCVTMQ)
FUNCTION_WITHOUT_N_NO_F (vcvtnq, VCVTNQ)
FUNCTION_WITHOUT_N_NO_F (vcvtpq, VCVTPQ)
FUNCTION (vcvttq, vcvtxq_impl, (VCVTTQ_F16_F32, VCVTTQ_M_F16_F32, VCVTTQ_F32_F16, VCVTTQ_M_F32_F16))
FUNCTION (vddupq, viddup_impl, (false, false))
FUNCTION (vdupq, vdupq_impl, (VDUPQ_M_N_S, VDUPQ_M_N_U, VDUPQ_M_N_F))
FUNCTION (vdwdupq, viddup_impl, (false, true))
FUNCTION (vidupq, viddup_impl, (true, false))
FUNCTION (viwdupq, viddup_impl, (true, true))
FUNCTION_WITH_RTX_M (veorq, XOR, VEORQ)
FUNCTION (vfmaq, unspec_mve_function_exact_insn, (-1, -1, VFMAQ_F, -1, -1, VFMAQ_N_F, -1, -1, VFMAQ_M_F, -1, -1, VFMAQ_M_N_F))
FUNCTION (vfmasq, unspec_mve_function_exact_insn, (-1, -1, -1, -1, -1, VFMASQ_N_F, -1, -1, -1, -1, -1, VFMASQ_M_N_F))
FUNCTION (vfmsq, unspec_mve_function_exact_insn, (-1, -1, VFMSQ_F, -1, -1, -1, -1, -1, VFMSQ_M_F, -1, -1, -1))
FUNCTION_WITH_M_N_NO_F (vhaddq, VHADDQ)
FUNCTION_WITH_M_N_NO_F (vhsubq, VHSUBQ)
FUNCTION (vld1q, vld1_impl,)
FUNCTION_PRED_P_S (vmaxavq, VMAXAVQ)
FUNCTION_WITHOUT_N_NO_U_F (vmaxaq, VMAXAQ)
FUNCTION_ONLY_F (vmaxnmaq, VMAXNMAQ)
FUNCTION_PRED_P_F (vmaxnmavq, VMAXNMAVQ)
FUNCTION (vmaxnmq, unspec_based_mve_function_exact_insn, (UNKNOWN, UNKNOWN, SMAX, -1, -1, -1, -1, -1, VMAXNMQ_M_F, -1, -1, -1))
FUNCTION_PRED_P_F (vmaxnmvq, VMAXNMVQ)
FUNCTION_WITH_RTX_M_NO_F (vmaxq, SMAX, UMAX, VMAXQ)
FUNCTION_PRED_P_S_U (vmaxvq, VMAXVQ)
FUNCTION_PRED_P_S (vminavq, VMINAVQ)
FUNCTION_WITHOUT_N_NO_U_F (vminaq, VMINAQ)
FUNCTION_ONLY_F (vminnmaq, VMINNMAQ)
FUNCTION_PRED_P_F (vminnmavq, VMINNMAVQ)
FUNCTION (vminnmq, unspec_based_mve_function_exact_insn, (UNKNOWN, UNKNOWN, SMIN, -1, -1, -1, -1, -1, VMINNMQ_M_F, -1, -1, -1))
FUNCTION_PRED_P_F (vminnmvq, VMINNMVQ)
FUNCTION_WITH_RTX_M_NO_F (vminq, SMIN, UMIN, VMINQ)
FUNCTION_PRED_P_S_U (vminvq, VMINVQ)
FUNCTION_PRED_P_S (vmladavaxq, VMLADAVAXQ)
FUNCTION_PRED_P_S_U (vmladavaq, VMLADAVAQ)
FUNCTION_PRED_P_S_U (vmladavq, VMLADAVQ)
FUNCTION_PRED_P_S (vmladavxq, VMLADAVXQ)
FUNCTION_PRED_P_S_U (vmlaldavaq, VMLALDAVAQ)
FUNCTION_PRED_P_S (vmlaldavaxq, VMLALDAVAXQ)
FUNCTION_PRED_P_S_U (vmlaldavq, VMLALDAVQ)
FUNCTION_PRED_P_S (vmlaldavxq, VMLALDAVXQ)
FUNCTION_ONLY_N_NO_F (vmlaq, VMLAQ)
FUNCTION_ONLY_N_NO_F (vmlasq, VMLASQ)
FUNCTION_PRED_P_S (vmlsdavaq, VMLSDAVAQ)
FUNCTION_PRED_P_S (vmlsdavaxq, VMLSDAVAXQ)
FUNCTION_PRED_P_S (vmlsdavq, VMLSDAVQ)
FUNCTION_PRED_P_S (vmlsdavxq, VMLSDAVXQ)
FUNCTION_PRED_P_S (vmlsldavaq, VMLSLDAVAQ)
FUNCTION_PRED_P_S (vmlsldavaxq, VMLSLDAVAXQ)
FUNCTION_PRED_P_S (vmlsldavq, VMLSLDAVQ)
FUNCTION_PRED_P_S (vmlsldavxq, VMLSLDAVXQ)
FUNCTION_WITHOUT_N_NO_F (vmovlbq, VMOVLBQ)
FUNCTION_WITHOUT_N_NO_F (vmovltq, VMOVLTQ)
FUNCTION_WITHOUT_N_NO_F (vmovnbq, VMOVNBQ)
FUNCTION_WITHOUT_N_NO_F (vmovntq, VMOVNTQ)
FUNCTION_WITHOUT_N_NO_F (vmulhq, VMULHQ)
FUNCTION (vmullbq_int, unspec_mve_function_exact_insn_vmull, (VMULLBQ_INT_S, VMULLBQ_INT_U, VMULLBQ_INT_M_S, VMULLBQ_INT_M_U))
FUNCTION (vmulltq_int, unspec_mve_function_exact_insn_vmull, (VMULLTQ_INT_S, VMULLTQ_INT_U, VMULLTQ_INT_M_S, VMULLTQ_INT_M_U))
FUNCTION (vmullbq_poly, unspec_mve_function_exact_insn_vmull_poly, (VMULLBQ_POLY_P, VMULLBQ_POLY_M_P))
FUNCTION (vmulltq_poly, unspec_mve_function_exact_insn_vmull_poly, (VMULLTQ_POLY_P, VMULLTQ_POLY_M_P))
FUNCTION_WITH_RTX_M_N (vmulq, MULT, VMULQ)
FUNCTION_WITH_RTX_M_N_NO_F (vmvnq, NOT, VMVNQ)
FUNCTION (vnegq, unspec_based_mve_function_exact_insn, (NEG, NEG, NEG, -1, -1, -1, VNEGQ_M_S, -1, VNEGQ_M_F, -1, -1, -1))
FUNCTION_WITHOUT_M_N (vpselq, VPSELQ)
FUNCTION (vornq, unspec_based_mve_function_exact_insn_vorn, (-1, -1, VORNQ_M_S, VORNQ_M_U, VORNQ_M_F, -1, -1))
FUNCTION_WITH_RTX_M_N_NO_N_F (vorrq, IOR, VORRQ)
FUNCTION_WITHOUT_N_NO_U_F (vqabsq, VQABSQ)
FUNCTION_WITH_M_N_NO_F (vqaddq, VQADDQ)
FUNCTION_WITHOUT_N_NO_F (vqmovnbq, VQMOVNBQ)
FUNCTION_WITHOUT_N_NO_F (vqmovntq, VQMOVNTQ)
FUNCTION_WITHOUT_N_NO_U_F (vqmovunbq, VQMOVUNBQ)
FUNCTION_WITHOUT_N_NO_U_F (vqmovuntq, VQMOVUNTQ)
FUNCTION_WITHOUT_N_NO_U_F (vqdmladhq, VQDMLADHQ)
FUNCTION_WITHOUT_N_NO_U_F (vqdmladhxq, VQDMLADHXQ)
FUNCTION_ONLY_N_NO_U_F (vqdmlahq, VQDMLAHQ)
FUNCTION_ONLY_N_NO_U_F (vqdmlashq, VQDMLASHQ)
FUNCTION_WITHOUT_N_NO_U_F (vqdmlsdhq, VQDMLSDHQ)
FUNCTION_WITHOUT_N_NO_U_F (vqdmlsdhxq, VQDMLSDHXQ)
FUNCTION_WITH_M_N_NO_U_F (vqdmulhq, VQDMULHQ)
FUNCTION_WITH_M_N_NO_U_F (vqdmullbq, VQDMULLBQ)
FUNCTION_WITH_M_N_NO_U_F (vqdmulltq, VQDMULLTQ)
FUNCTION_WITHOUT_N_NO_U_F (vqrdmladhq, VQRDMLADHQ)
FUNCTION_WITHOUT_N_NO_U_F (vqrdmladhxq, VQRDMLADHXQ)
FUNCTION_ONLY_N_NO_U_F (vqrdmlahq, VQRDMLAHQ)
FUNCTION_ONLY_N_NO_U_F (vqrdmlashq, VQRDMLASHQ)
FUNCTION_WITHOUT_N_NO_U_F (vqrdmlsdhq, VQRDMLSDHQ)
FUNCTION_WITHOUT_N_NO_U_F (vqrdmlsdhxq, VQRDMLSDHXQ)
FUNCTION_WITHOUT_N_NO_U_F (vqnegq, VQNEGQ)
FUNCTION_WITH_M_N_NO_F (vqrshlq, VQRSHLQ)
FUNCTION_WITH_M_N_NO_U_F (vqrdmulhq, VQRDMULHQ)
FUNCTION_WITH_M_N_R (vqshlq, VQSHLQ)
FUNCTION_ONLY_N_NO_U_F (vqshluq, VQSHLUQ)
FUNCTION_ONLY_N_NO_F (vqrshrnbq, VQRSHRNBQ)
FUNCTION_ONLY_N_NO_F (vqrshrntq, VQRSHRNTQ)
FUNCTION_ONLY_N_NO_U_F (vqrshrunbq, VQRSHRUNBQ)
FUNCTION_ONLY_N_NO_U_F (vqrshruntq, VQRSHRUNTQ)
FUNCTION_ONLY_N_NO_F (vqshrnbq, VQSHRNBQ)
FUNCTION_ONLY_N_NO_F (vqshrntq, VQSHRNTQ)
FUNCTION_ONLY_N_NO_U_F (vqshrunbq, VQSHRUNBQ)
FUNCTION_ONLY_N_NO_U_F (vqshruntq, VQSHRUNTQ)
FUNCTION_WITH_M_N_NO_F (vqsubq, VQSUBQ)
FUNCTION (vreinterpretq, vreinterpretq_impl,)
FUNCTION_WITHOUT_N_NO_F (vrev16q, VREV16Q)
FUNCTION_WITHOUT_N (vrev32q, VREV32Q)
FUNCTION_WITHOUT_N (vrev64q, VREV64Q)
FUNCTION_WITHOUT_N_NO_F (vrhaddq, VRHADDQ)
FUNCTION_PRED_P_S_U (vrmlaldavhaq, VRMLALDAVHAQ)
FUNCTION_PRED_P_S (vrmlaldavhaxq, VRMLALDAVHAXQ)
FUNCTION_PRED_P_S_U (vrmlaldavhq, VRMLALDAVHQ)
FUNCTION_PRED_P_S (vrmlaldavhxq, VRMLALDAVHXQ)
FUNCTION_PRED_P_S (vrmlsldavhaq, VRMLSLDAVHAQ)
FUNCTION_PRED_P_S (vrmlsldavhaxq, VRMLSLDAVHAXQ)
FUNCTION_PRED_P_S (vrmlsldavhq, VRMLSLDAVHQ)
FUNCTION_PRED_P_S (vrmlsldavhxq, VRMLSLDAVHXQ)
FUNCTION_WITHOUT_N_NO_F (vrmulhq, VRMULHQ)
FUNCTION_ONLY_F (vrndq, VRNDQ)
FUNCTION_ONLY_F (vrndaq, VRNDAQ)
FUNCTION_ONLY_F (vrndmq, VRNDMQ)
FUNCTION_ONLY_F (vrndnq, VRNDNQ)
FUNCTION_ONLY_F (vrndpq, VRNDPQ)
FUNCTION_ONLY_F (vrndxq, VRNDXQ)
FUNCTION_WITH_M_N_NO_F (vrshlq, VRSHLQ)
FUNCTION_ONLY_N_NO_F (vrshrnbq, VRSHRNBQ)
FUNCTION_ONLY_N_NO_F (vrshrntq, VRSHRNTQ)
FUNCTION_ONLY_N_NO_F (vrshrq, VRSHRQ)
FUNCTION (vsbciq, vadc_vsbc_impl, (true, false))
FUNCTION (vsbcq, vadc_vsbc_impl, (false, false))
FUNCTION (vshlcq, vshlc_impl,)
FUNCTION_ONLY_N_NO_F (vshllbq, VSHLLBQ)
FUNCTION_ONLY_N_NO_F (vshlltq, VSHLLTQ)
FUNCTION_WITH_M_N_R (vshlq, VSHLQ)
FUNCTION_ONLY_N_NO_F (vshrnbq, VSHRNBQ)
FUNCTION_ONLY_N_NO_F (vshrntq, VSHRNTQ)
FUNCTION_ONLY_N_NO_F (vshrq, VSHRQ)
FUNCTION_ONLY_N_NO_F (vsliq, VSLIQ)
FUNCTION_ONLY_N_NO_F (vsriq, VSRIQ)
FUNCTION (vst1q, vst1_impl,)
FUNCTION_WITH_RTX_M_N (vsubq, MINUS, VSUBQ)
FUNCTION (vuninitializedq, vuninitializedq_impl,)

} /* end namespace arm_mve */
