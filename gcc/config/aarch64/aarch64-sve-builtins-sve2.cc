/* ACLE support for AArch64 SVE (__ARM_FEATURE_SVE2 intrinsics)
   Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
#include "vec-perm-indices.h"
#include "aarch64-sve-builtins.h"
#include "aarch64-sve-builtins-shapes.h"
#include "aarch64-sve-builtins-base.h"
#include "aarch64-sve-builtins-sve2.h"
#include "aarch64-sve-builtins-functions.h"

using namespace aarch64_sve;

namespace {

/* Return the UNSPEC_CDOT* unspec for rotation amount ROT.  */
static int
unspec_cdot (int rot)
{
  switch (rot)
    {
    case 0: return UNSPEC_CDOT;
    case 90: return UNSPEC_CDOT90;
    case 180: return UNSPEC_CDOT180;
    case 270: return UNSPEC_CDOT270;
    default: gcc_unreachable ();
    }
}

/* Return the UNSPEC_SQRDCMLAH* unspec for rotation amount ROT.  */
static int
unspec_sqrdcmlah (int rot)
{
  switch (rot)
    {
    case 0: return UNSPEC_SQRDCMLAH;
    case 90: return UNSPEC_SQRDCMLAH90;
    case 180: return UNSPEC_SQRDCMLAH180;
    case 270: return UNSPEC_SQRDCMLAH270;
    default: gcc_unreachable ();
    }
}

class ld1uxq_st1xq_base : public function_base
{
public:
  CONSTEXPR ld1uxq_st1xq_base (machine_mode memory_mode)
    : m_memory_mode (memory_mode) {}

  tree
  memory_scalar_type (const function_instance &fi) const override
  {
    return fi.scalar_type (0);
  }

  machine_mode
  memory_vector_mode (const function_instance &) const override
  {
    return m_memory_mode;
  }

protected:
  machine_mode m_memory_mode;
};

class ld234q_st234q_base : public full_width_access
{
public:
  CONSTEXPR ld234q_st234q_base (unsigned int vector_count, machine_mode mode)
    : full_width_access (vector_count), m_mode (mode)
  {}

  machine_mode
  memory_vector_mode (const function_instance &) const override
  {
    return m_mode;
  }

  machine_mode m_mode;
};

class svaba_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    /* Fold to svabd if op1 is all zeros.  */
    tree op1 = gimple_call_arg (f.call, 0);
    if (!integer_zerop (op1))
      return NULL;
    function_instance instance ("svabd", functions::svabd, shapes::binary_opt_n,
				f.mode_suffix_id, f.type_suffix_ids, GROUP_none,
				PRED_x, FPM_unused);
    gcall *call = f.redirect_call (instance);
    /* Add a ptrue as predicate, because unlike svaba, svabd is
       predicated.  */
    gimple_call_set_arg (call, 0, build_all_ones_cst (f.gp_type ()));
    return call;
  }

public:
  rtx
  expand (function_expander &e) const override
  {
    rtx_code max_code = e.type_suffix (0).unsigned_p ? UMAX : SMAX;
    machine_mode mode = e.vector_mode (0);
    return e.use_exact_insn (code_for_aarch64_sve2_aba (max_code, mode));
  }
};

class svxar_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* aarch64_sve2_xar represents this operation with a left-rotate RTX.
       Convert the right-rotate amount from the intrinsic to fit this.  */
    machine_mode mode = e.vector_mode (0);
    HOST_WIDE_INT rot = GET_MODE_UNIT_BITSIZE (mode)
			- INTVAL (e.args[2]);
    e.args[2] = aarch64_simd_gen_const_vector_dup (mode, rot);
    return e.use_exact_insn (code_for_aarch64_sve2_xar (mode));
  }
};

class svcdot_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    return e.use_exact_insn (code_for_aarch64_sve (unspec_cdot (rot),
						   e.vector_mode (0)));
  }
};

class svcdot_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    return e.use_exact_insn (code_for_aarch64_lane (unspec_cdot (rot),
						    e.vector_mode (0)));
  }
};

class svclamp_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    auto mode = e.tuple_mode (0);
    insn_code icode;
    if (e.type_suffix (0).float_p)
      icode = (e.vectors_per_tuple () > 1
	       ? code_for_aarch64_sve_fclamp_single (mode)
	       : code_for_aarch64_sve_fclamp (mode));
    else
      {
	auto max = e.type_suffix (0).unsigned_p ? UMAX : SMAX;
	icode = (e.vectors_per_tuple () > 1
		 ? code_for_aarch64_sve_clamp_single (max, mode)
		 : code_for_aarch64_sve_clamp (max, mode));
      }
    return e.use_exact_insn (icode);
  }
};

class svcvtl_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_sve_cvtl (e.result_mode ()));
  }
};

class svcvt_fp8_impl : public function_base
{
public:
  CONSTEXPR
  svcvt_fp8_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    auto icode = code_for_aarch64_sve2_fp8_cvt (m_unspec, e.result_mode ());
    return e.use_exact_insn (icode);
  }

  int m_unspec;
};

class svcvtn_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.fpm_mode == FPM_set)
      icode = code_for_aarch64_sve2_fp8_cvtn (GET_MODE (e.args[0]));
    else
      icode = code_for_aarch64_sve_cvtn (e.result_mode ());
    return e.use_exact_insn (icode);
  }
};

class svcvtxnt_impl : public CODE_FOR_MODE1 (aarch64_sve2_cvtxnt)
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.pred == PRED_x && is_pfalse (gimple_call_arg (f.call, 1)))
      return f.fold_call_to (build_zero_cst (TREE_TYPE (f.lhs)));
    return NULL;
  }
};

class svdup_laneq_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_sve_dupq (e.result_mode ()));
  }
};

class svextq_impl : public permute
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    unsigned int index = tree_to_uhwi (gimple_call_arg (f.call, 2));
    machine_mode mode = f.vector_mode (0);
    unsigned int subelts = 128U / GET_MODE_UNIT_BITSIZE (mode);
    poly_uint64 nelts = GET_MODE_NUNITS (mode);
    vec_perm_builder builder (nelts, subelts, 3);
    for (unsigned int i = 0; i < 3; ++i)
      for (unsigned int j = 0; j < subelts; ++j)
	{
	  if (index + j < subelts)
	    builder.quick_push (i * subelts + index + j);
	  else
	    builder.quick_push (i * subelts + index + j - subelts + nelts);
	}
    return fold_permute (f, builder);
  }

  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_sve_extq (e.vector_mode (0)));
  }
};

class svld1q_gather_impl : public full_width_access
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
    e.prepare_gather_address_operands (1, false);
    return e.use_exact_insn (CODE_FOR_aarch64_gather_ld1q);
  }
};

class svld1uxq_impl : public ld1uxq_st1xq_base
{
public:
  using ld1uxq_st1xq_base::ld1uxq_st1xq_base;

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_sve_ld1_extendq (e.vector_mode (0));
    return e.use_contiguous_load_insn (icode);
  }
};

class svld234q_impl : public ld234q_st234q_base
{
public:
  using ld234q_st234q_base::ld234q_st234q_base;

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_sve_ldnq (e.result_mode ());
    return e.use_contiguous_load_insn (icode);
  }
};

class svldnt1_gather_impl : public full_width_access
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
    e.prepare_gather_address_operands (1, false);
    machine_mode mem_mode = e.memory_vector_mode ();
    return e.use_exact_insn (code_for_aarch64_gather_ldnt (mem_mode));
  }
};

/* Implements extending forms of svldnt1_gather.  */
class svldnt1_gather_extend_impl : public extending_load
{
public:
  using extending_load::extending_load;

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_gather_address_operands (1, false);
    /* Add a constant predicate for the extension rtx.  */
    e.args.quick_push (CONSTM1_RTX (VNx16BImode));
    insn_code icode = code_for_aarch64_gather_ldnt (extend_rtx_code (),
						    e.vector_mode (0),
						    e.memory_vector_mode ());
    return e.use_exact_insn (icode);
  }
};

/* Implements both svmatch and svnmatch; the unspec parameter decides
   between them.  */
class svmatch_svnmatch_impl : public function_base
{
public:
  CONSTEXPR svmatch_svnmatch_impl (int unspec) : m_unspec (unspec) {}
  gimple *
  fold (gimple_folder &f) const override
  {
    tree pg = gimple_call_arg (f.call, 0);
    if (is_pfalse (pg))
      return f.fold_call_to (pg);
    return NULL;
  }

  rtx
  expand (function_expander &e) const override
  {
    /* These are UNSPEC_PRED_Z operations and so need a hint operand.  */
    e.add_ptrue_hint (0, e.gp_mode (0));
    return e.use_exact_insn (code_for_aarch64_pred (m_unspec,
						    e.vector_mode (0)));
  }

  int m_unspec;
};

/* Implements both svmovlb and svmovlt; the unspec parameters decide
   between them.  */
class svmovl_lb_impl : public unspec_based_function_base
{
public:
  using unspec_based_function_base::unspec_based_function_base;

  rtx
  expand (function_expander &e) const override
  {
    e.args.quick_push (const0_rtx);
    return e.map_to_unspecs (m_unspec_for_sint, m_unspec_for_uint,
			     m_unspec_for_fp);
  }
};

class svpext_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    unsigned int bits = e.type_suffix (0).element_bits;
    return e.use_exact_insn (e.vectors_per_tuple () == 2
			     ? code_for_aarch64_sve_pextx2 (bits)
			     : code_for_aarch64_sve_pext (bits));
  }
};

class svpmov_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.pred == PRED_z)
      icode = code_for_aarch64_pmov_to (e.vector_mode (0));
    else
      icode = code_for_aarch64_pmov_from (e.vector_mode (0));
    return e.use_exact_insn (icode);
  }
};

class svpmov_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.pred == PRED_m)
      icode = code_for_aarch64_pmov_lane_to (e.vector_mode (0));
    else if (e.args[1] == const0_rtx)
      icode = code_for_aarch64_pmov_from (e.vector_mode (0));
    else
      icode = code_for_aarch64_pmov_lane_from (e.vector_mode (0));
    return e.use_exact_insn (icode);
  }
};

class svpsel_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    unsigned int bits = e.type_suffix (0).element_bits;
    return e.use_exact_insn (code_for_aarch64_sve_psel (bits));
  }
};

class svqcadd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    if (rot == 90)
      return e.map_to_unspecs (UNSPEC_SQCADD90, -1, -1);
    if (rot == 270)
      return e.map_to_unspecs (UNSPEC_SQCADD270, -1, -1);
    gcc_unreachable ();
  }
};

class svqrdcmlah_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    return e.use_exact_insn (code_for_aarch64_sve (unspec_sqrdcmlah (rot),
						   e.vector_mode (0)));
  }
};

class svqrdcmlah_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    return e.use_exact_insn (code_for_aarch64_lane (unspec_sqrdcmlah (rot),
						    e.vector_mode (0)));
  }
};

class svqrshl_impl : public unspec_based_function
{
public:
  CONSTEXPR svqrshl_impl ()
    : unspec_based_function (UNSPEC_SQRSHL, UNSPEC_UQRSHL, -1) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    if (tree amount = uniform_integer_cst_p (gimple_call_arg (f.call, 2)))
      {
	if (wi::to_widest (amount) >= 0)
	  {
	    /* The rounding has no effect, and [SU]QSHL has immediate forms
	       that we can use for sensible shift amounts.  */
	    function_instance instance ("svqshl", functions::svqshl,
					shapes::binary_int_opt_n, MODE_n,
					f.type_suffix_ids, GROUP_none, f.pred,
					FPM_unused);
	    return f.redirect_call (instance);
	  }
	else
	  {
	    /* The saturation has no effect, and [SU]RSHL has immediate forms
	       that we can use for sensible shift amounts.  */
	    function_instance instance ("svrshl", functions::svrshl,
					shapes::binary_int_opt_single_n, MODE_n,
					f.type_suffix_ids, GROUP_none, f.pred,
					FPM_unused);
	    return f.redirect_call (instance);
	  }
      }
    return NULL;
  }
};

class svqshl_impl : public unspec_based_function
{
public:
  CONSTEXPR svqshl_impl ()
    : unspec_based_function (UNSPEC_SQSHL, UNSPEC_UQSHL, -1) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    if (tree amount = uniform_integer_cst_p (gimple_call_arg (f.call, 2)))
      {
	int element_bits = f.type_suffix (0).element_bits;
	if (wi::to_widest (amount) >= -element_bits
	    && wi::to_widest (amount) < 0)
	  {
	    /* The saturation has no effect for right shifts, so we can
	       use the immediate form of ASR or LSR.  */
	    amount = wide_int_to_tree (TREE_TYPE (amount),
				       -wi::to_wide (amount));
	    function_instance instance ("svasr", functions::svasr,
					shapes::binary_uint_opt_n, MODE_n,
					f.type_suffix_ids, GROUP_none, f.pred,
					FPM_unused);
	    if (f.type_suffix (0).unsigned_p)
	      {
		instance.base_name = "svlsr";
		instance.base = functions::svlsr;
	      }
	    gcall *call = f.redirect_call (instance);
	    gimple_call_set_arg (call, 2, amount);
	    return call;
	  }
      }
    return NULL;
  }
};

class svrshl_impl : public unspec_based_function
{
public:
  CONSTEXPR svrshl_impl ()
    : unspec_based_function (UNSPEC_SRSHL, UNSPEC_URSHL, -1) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.vectors_per_tuple () > 1)
      return nullptr;

    if (tree amount = uniform_integer_cst_p (gimple_call_arg (f.call, 2)))
      {
	if (wi::to_widest (amount) >= 0)
	  {
	    /* The rounding has no effect, and LSL has immediate forms
	       that we can use for sensible shift amounts.  */
	    function_instance instance ("svlsl", functions::svlsl,
					shapes::binary_uint_opt_n, MODE_n,
					f.type_suffix_ids, GROUP_none, f.pred,
					FPM_unused);
	    gcall *call = f.redirect_call (instance);
	    gimple_call_set_arg (call, 2, amount);
	    return call;
	  }
	int element_bits = f.type_suffix (0).element_bits;
	if (wi::to_widest (amount) >= -element_bits)
	  {
	    /* The shift amount is in range of [SU]RSHR.  */
	    amount = wide_int_to_tree (TREE_TYPE (amount),
				       -wi::to_wide (amount));
	    function_instance instance ("svrshr", functions::svrshr,
					shapes::shift_right_imm, MODE_n,
					f.type_suffix_ids, GROUP_none, f.pred,
					FPM_unused);
	    gcall *call = f.redirect_call (instance);
	    gimple_call_set_arg (call, 2, amount);
	    return call;
	  }
      }
    return NULL;
  }
};

class svsqadd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    if (e.pred == PRED_x
	&& aarch64_sve_sqadd_sqsub_immediate_p (mode, e.args[2], false))
      return e.map_to_rtx_codes (UNKNOWN, US_PLUS, -1, -1);
    return e.map_to_unspecs (-1, UNSPEC_USQADD, -1);
  }
};

class svsra_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    /* Fold to svlsr/svasr if op1 is all zeros.  */
    tree op1 = gimple_call_arg (f.call, 0);
    if (!integer_zerop (op1))
      return NULL;
    function_instance instance ("svlsr", functions::svlsr,
				shapes::binary_uint_opt_n, MODE_n,
				f.type_suffix_ids, GROUP_none, PRED_x,
				FPM_unused);
    if (!f.type_suffix (0).unsigned_p)
      {
	instance.base_name = "svasr";
	instance.base = functions::svasr;
      }
    gcall *call = f.redirect_call (instance);
    /* Add a ptrue as predicate, because unlike svsra, svlsr/svasr are
       predicated intrinsics.  */
    gimple_call_set_arg (call, 0, build_all_ones_cst (f.gp_type ()));
    /* For svsra, the shift amount (imm3) is uint64_t for all function types,
       but for svlsr/svasr, imm3 has the same width as the function type.  */
    tree imm3 = gimple_call_arg (f.call, 2);
    tree imm3_prec = wide_int_to_tree (f.scalar_type (0),
				       wi::to_widest (imm3));
    gimple_call_set_arg (call, 2, imm3_prec);
    return call;
  }

  rtx
  expand (function_expander &e) const override
  {
    rtx_code shift_code = e.type_suffix (0).unsigned_p ? LSHIFTRT : ASHIFTRT;
    machine_mode mode = e.vector_mode (0);
    return e.use_exact_insn (code_for_aarch64_sve_add (shift_code, mode));
  }
};

class svst1q_scatter_impl : public full_width_access
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
    rtx data = e.args.last ();
    e.args.last () = force_lowpart_subreg (VNx2DImode, data, GET_MODE (data));
    e.prepare_gather_address_operands (1, false);
    return e.use_exact_insn (CODE_FOR_aarch64_scatter_st1q);
  }
};

class svst1xq_impl : public ld1uxq_st1xq_base
{
public:
  using ld1uxq_st1xq_base::ld1uxq_st1xq_base;

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_sve_st1_truncq (e.vector_mode (0));
    return e.use_contiguous_store_insn (icode);
  }
};

class svst234q_impl : public ld234q_st234q_base
{
public:
  using ld234q_st234q_base::ld234q_st234q_base;

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode tuple_mode = GET_MODE (e.args.last ());
    insn_code icode = code_for_aarch64_sve_stnq (tuple_mode);
    return e.use_contiguous_store_insn (icode);
  }
};

class svstnt1_scatter_impl : public full_width_access
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
    e.prepare_gather_address_operands (1, false);
    machine_mode mem_mode = e.memory_vector_mode ();
    return e.use_exact_insn (code_for_aarch64_scatter_stnt (mem_mode));
  }
};

/* Implements truncating forms of svstnt1_scatter.  */
class svstnt1_scatter_truncate_impl : public truncating_store
{
public:
  using truncating_store::truncating_store;

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_gather_address_operands (1, false);
    insn_code icode = code_for_aarch64_scatter_stnt (e.vector_mode (0),
						     e.memory_vector_mode ());
    return e.use_exact_insn (icode);
  }
};

class svtbl2_impl : public quiet<multi_vector_function>
{
public:
  CONSTEXPR svtbl2_impl () : quiet<multi_vector_function> (2) {}

  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_sve2_tbl2 (e.vector_mode (0)));
  }
};

class svunpk_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    optab op = (e.type_suffix (0).unsigned_p ? zext_optab : sext_optab);
    insn_code icode = convert_optab_handler (op, e.result_mode (),
					     GET_MODE (e.args[0]));
    return e.use_exact_insn (icode);
  }
};

class svuqadd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    if (e.pred == PRED_x
	&& aarch64_sve_arith_immediate_p (mode, e.args[2], false))
      return e.use_unpred_insn (code_for_aarch64_sve_suqadd_const (mode));
    return e.map_to_unspecs (UNSPEC_SUQADD, -1, -1);
  }
};

/* Implements svuzpq1 and svuzpq2.  */
class svuzpq_impl : public binary_permute
{
public:
  CONSTEXPR svuzpq_impl (unsigned int base)
    : binary_permute (base ? UNSPEC_UZPQ2 : UNSPEC_UZPQ1), m_base (base) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    machine_mode mode = f.vector_mode (0);
    unsigned int subelts = 128U / GET_MODE_UNIT_BITSIZE (mode);
    poly_uint64 nelts = GET_MODE_NUNITS (mode);
    vec_perm_builder builder (nelts, subelts, 3);
    for (unsigned int i = 0; i < 3; ++i)
      {
	for (unsigned int j = 0; j < subelts / 2; ++j)
	  builder.quick_push (m_base + j * 2 + i * subelts);
	for (unsigned int j = 0; j < subelts / 2; ++j)
	  builder.quick_push (m_base + j * 2 + i * subelts + nelts);
      }
    return fold_permute (f, builder);
  }

  /* 0 for svuzpq1, 1 for svuzpq2.  */
  unsigned int m_base;
};

/* Implements both svwhilerw and svwhilewr; the unspec parameter decides
   between them.  */
class svwhilerw_svwhilewr_impl : public full_width_access
{
public:
  CONSTEXPR svwhilerw_svwhilewr_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    for (unsigned int i = 0; i < 2; ++i)
      e.args[i] = e.convert_to_pmode (e.args[i]);
    return e.use_exact_insn (code_for_while (m_unspec, Pmode, e.gp_mode (0)));
  }

  int m_unspec;
};

/* Implements svzipq1 and svzipq2.  */
class svzipq_impl : public binary_permute
{
public:
  CONSTEXPR svzipq_impl (unsigned int base)
    : binary_permute (base ? UNSPEC_ZIPQ2 : UNSPEC_ZIPQ1), m_base (base) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    machine_mode mode = f.vector_mode (0);
    unsigned int pairs = 64U / GET_MODE_UNIT_BITSIZE (mode);
    poly_uint64 nelts = GET_MODE_NUNITS (mode);
    auto base = m_base * pairs;
    vec_perm_builder builder (nelts, pairs * 2, 3);
    for (unsigned int i = 0; i < 3; ++i)
      for (unsigned int j = 0; j < pairs; ++j)
	{
	  builder.quick_push (base + j + i * pairs * 2);
	  builder.quick_push (base + j + i * pairs * 2 + nelts);
	}
    return fold_permute (f, builder);
  }

  /* 0 for svzipq1, 1 for svzipq2.  */
  unsigned int m_base;
};

} /* end anonymous namespace */

namespace aarch64_sve {

FUNCTION (svaba, svaba_impl,)
FUNCTION (svabalb, unspec_based_add_function, (UNSPEC_SABDLB,
					       UNSPEC_UABDLB, -1))
FUNCTION (svabalt, unspec_based_add_function, (UNSPEC_SABDLT,
					       UNSPEC_UABDLT, -1))
FUNCTION (svabdlb, unspec_based_function, (UNSPEC_SABDLB, UNSPEC_UABDLB, -1))
FUNCTION (svabdlt, unspec_based_function, (UNSPEC_SABDLT, UNSPEC_UABDLT, -1))
FUNCTION (svadalp, unspec_based_function, (UNSPEC_SADALP, UNSPEC_UADALP, -1))
FUNCTION (svadclb, unspec_based_function, (-1, UNSPEC_ADCLB, -1))
FUNCTION (svadclt, unspec_based_function, (-1, UNSPEC_ADCLT, -1))
FUNCTION (svaddhnb, unspec_based_function, (UNSPEC_ADDHNB, UNSPEC_ADDHNB, -1))
FUNCTION (svaddhnt, unspec_based_function, (UNSPEC_ADDHNT, UNSPEC_ADDHNT, -1))
FUNCTION (svaddlb, unspec_based_function, (UNSPEC_SADDLB, UNSPEC_UADDLB, -1))
FUNCTION (svaddlbt, unspec_based_function, (UNSPEC_SADDLBT, -1, -1))
FUNCTION (svaddlt, unspec_based_function, (UNSPEC_SADDLT, UNSPEC_UADDLT, -1))
FUNCTION (svaddp, unspec_based_pred_function, (UNSPEC_ADDP, UNSPEC_ADDP,
					       UNSPEC_FADDP))
FUNCTION (svaddqv, reduction, (UNSPEC_ADDQV, UNSPEC_ADDQV, UNSPEC_FADDQV))
FUNCTION (svaddwb, unspec_based_function, (UNSPEC_SADDWB, UNSPEC_UADDWB, -1))
FUNCTION (svaddwt, unspec_based_function, (UNSPEC_SADDWT, UNSPEC_UADDWT, -1))
FUNCTION (svaesd, fixed_insn_function, (CODE_FOR_aarch64_sve2_aesd))
FUNCTION (svaese, fixed_insn_function, (CODE_FOR_aarch64_sve2_aese))
FUNCTION (svaesimc, fixed_insn_function, (CODE_FOR_aarch64_sve2_aesimc))
FUNCTION (svaesmc, fixed_insn_function, (CODE_FOR_aarch64_sve2_aesmc))
FUNCTION (svamax, cond_or_uncond_unspec_function,
	  (UNSPEC_COND_FAMAX, UNSPEC_FAMAX))
FUNCTION (svamin, cond_or_uncond_unspec_function,
	  (UNSPEC_COND_FAMIN, UNSPEC_FAMIN))
FUNCTION (svandqv, reduction, (UNSPEC_ANDQV, UNSPEC_ANDQV, -1))
FUNCTION (svbcax, CODE_FOR_MODE0 (aarch64_sve2_bcax),)
FUNCTION (svbdep, unspec_based_function, (UNSPEC_BDEP, UNSPEC_BDEP, -1))
FUNCTION (svbext, unspec_based_function, (UNSPEC_BEXT, UNSPEC_BEXT, -1))
FUNCTION (svbfmlslb, fixed_insn_function, (CODE_FOR_aarch64_sve_bfmlslbvnx4sf))
FUNCTION (svbfmlslb_lane, fixed_insn_function,
	  (CODE_FOR_aarch64_sve_bfmlslb_lanevnx4sf))
FUNCTION (svbfmlslt, fixed_insn_function, (CODE_FOR_aarch64_sve_bfmlsltvnx4sf))
FUNCTION (svbfmlslt_lane, fixed_insn_function,
	  (CODE_FOR_aarch64_sve_bfmlslt_lanevnx4sf))
FUNCTION (svbgrp, unspec_based_function, (UNSPEC_BGRP, UNSPEC_BGRP, -1))
FUNCTION (svbsl, CODE_FOR_MODE0 (aarch64_sve2_bsl),)
FUNCTION (svbsl1n, CODE_FOR_MODE0 (aarch64_sve2_bsl1n),)
FUNCTION (svbsl2n, CODE_FOR_MODE0 (aarch64_sve2_bsl2n),)
FUNCTION (svcdot, svcdot_impl,)
FUNCTION (svcdot_lane, svcdot_lane_impl,)
FUNCTION (svclamp, svclamp_impl,)
FUNCTION (svcvt1, svcvt_fp8_impl, (UNSPEC_F1CVT))
FUNCTION (svcvt2, svcvt_fp8_impl, (UNSPEC_F2CVT))
FUNCTION (svcvtl, svcvtl_impl,)
FUNCTION (svcvtlt1, svcvt_fp8_impl, (UNSPEC_F1CVTLT))
FUNCTION (svcvtlt2, svcvt_fp8_impl, (UNSPEC_F2CVTLT))
FUNCTION (svcvtlt, unspec_based_function, (-1, -1, UNSPEC_COND_FCVTLT))
FUNCTION (svcvtn, svcvtn_impl,)
FUNCTION (svcvtnb, fixed_insn_function, (CODE_FOR_aarch64_sve2_fp8_cvtnbvnx16qi))
FUNCTION (svcvtx, unspec_based_function, (-1, -1, UNSPEC_COND_FCVTX))
FUNCTION (svcvtxnt, svcvtxnt_impl,)
FUNCTION (svdup_laneq, svdup_laneq_impl,)
FUNCTION (sveor3, CODE_FOR_MODE0 (aarch64_sve2_eor3),)
FUNCTION (sveorbt, unspec_based_function, (UNSPEC_EORBT, UNSPEC_EORBT, -1))
FUNCTION (sveorqv, reduction, (UNSPEC_EORQV, UNSPEC_EORQV, -1))
FUNCTION (sveortb, unspec_based_function, (UNSPEC_EORTB, UNSPEC_EORTB, -1))
FUNCTION (svextq, svextq_impl,)
FUNCTION (svhadd, unspec_based_function, (UNSPEC_SHADD, UNSPEC_UHADD, -1))
FUNCTION (svhsub, unspec_based_function, (UNSPEC_SHSUB, UNSPEC_UHSUB, -1))
FUNCTION (svhistcnt, CODE_FOR_MODE0 (aarch64_sve2_histcnt),)
FUNCTION (svhistseg, CODE_FOR_MODE0 (aarch64_sve2_histseg),)
FUNCTION (svhsubr, unspec_based_function_rotated, (UNSPEC_SHSUB,
						   UNSPEC_UHSUB, -1))
FUNCTION (svld1q_gather, svld1q_gather_impl,)
FUNCTION (svld1udq, svld1uxq_impl, (VNx1DImode))
FUNCTION (svld1uwq, svld1uxq_impl, (VNx1SImode))
FUNCTION (svld2q, svld234q_impl, (2, VNx2TImode))
FUNCTION (svld3q, svld234q_impl, (3, VNx3TImode))
FUNCTION (svld4q, svld234q_impl, (4, VNx4TImode))
FUNCTION (svldnt1_gather, svldnt1_gather_impl,)
FUNCTION (svldnt1sb_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_s8))
FUNCTION (svldnt1sh_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_s16))
FUNCTION (svldnt1sw_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_s32))
FUNCTION (svldnt1ub_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_u8))
FUNCTION (svldnt1uh_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_u16))
FUNCTION (svldnt1uw_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_u32))
FUNCTION (svlogb, unspec_based_function, (-1, -1, UNSPEC_COND_FLOGB))
FUNCTION (svmatch, svmatch_svnmatch_impl, (UNSPEC_MATCH))
FUNCTION (svmaxnmp, unspec_based_pred_function, (-1, -1, UNSPEC_FMAXNMP))
FUNCTION (svmaxnmqv, reduction, (-1, -1, UNSPEC_FMAXNMQV))
FUNCTION (svmaxp, unspec_based_pred_function, (UNSPEC_SMAXP, UNSPEC_UMAXP,
					       UNSPEC_FMAXP))
FUNCTION (svmaxqv, reduction, (UNSPEC_SMAXQV, UNSPEC_UMAXQV, UNSPEC_FMAXQV))
FUNCTION (svminnmp, unspec_based_pred_function, (-1, -1, UNSPEC_FMINNMP))
FUNCTION (svminnmqv, reduction, (-1, -1, UNSPEC_FMINNMQV))
FUNCTION (svminp, unspec_based_pred_function, (UNSPEC_SMINP, UNSPEC_UMINP,
					       UNSPEC_FMINP))
FUNCTION (svminqv, reduction, (UNSPEC_SMINQV, UNSPEC_UMINQV, UNSPEC_FMINQV))
FUNCTION (svmlalb_lane, unspec_based_mla_lane_function,
	  (UNSPEC_SMULLB, UNSPEC_UMULLB, UNSPEC_FMLALB,
	   UNSPEC_FMLALB_FP8))
FUNCTION (svmlalb, unspec_based_mla_function,
	  (UNSPEC_SMULLB, UNSPEC_UMULLB, UNSPEC_FMLALB,
	   UNSPEC_FMLALB_FP8))
FUNCTION (svmlallbb_lane, unspec_based_mla_lane_function,
	  (-1, -1, -1, UNSPEC_FMLALLBB_FP8))
FUNCTION (svmlallbb, unspec_based_mla_function,
	  (-1, -1, -1, UNSPEC_FMLALLBB_FP8))
FUNCTION (svmlallbt_lane, unspec_based_mla_lane_function,
	  (-1, -1, -1, UNSPEC_FMLALLBT_FP8))
FUNCTION (svmlallbt, unspec_based_mla_function,
	  (-1, -1, -1, UNSPEC_FMLALLBT_FP8))
FUNCTION (svmlalltb_lane, unspec_based_mla_lane_function,
	  (-1, -1, -1, UNSPEC_FMLALLTB_FP8))
FUNCTION (svmlalltb, unspec_based_mla_function,
	  (-1, -1, -1, UNSPEC_FMLALLTB_FP8))
FUNCTION (svmlalltt_lane, unspec_based_mla_lane_function,
	  (-1, -1, -1, UNSPEC_FMLALLTT_FP8))
FUNCTION (svmlalltt, unspec_based_mla_function,
	  (-1, -1, -1, UNSPEC_FMLALLTT_FP8))
FUNCTION (svmlalt_lane, unspec_based_mla_lane_function,
	  (UNSPEC_SMULLT, UNSPEC_UMULLT, UNSPEC_FMLALT,
	   UNSPEC_FMLALT_FP8))
FUNCTION (svmlalt, unspec_based_mla_function,
	  (UNSPEC_SMULLT, UNSPEC_UMULLT, UNSPEC_FMLALT,
	   UNSPEC_FMLALT_FP8))
FUNCTION (svmlslb, unspec_based_mls_function, (UNSPEC_SMULLB,
					       UNSPEC_UMULLB, UNSPEC_FMLSLB))
FUNCTION (svmlslb_lane, unspec_based_mls_lane_function, (UNSPEC_SMULLB,
							 UNSPEC_UMULLB,
							 UNSPEC_FMLSLB))
FUNCTION (svmlslt, unspec_based_mls_function, (UNSPEC_SMULLT,
					       UNSPEC_UMULLT, UNSPEC_FMLSLT))
FUNCTION (svmlslt_lane, unspec_based_mls_lane_function, (UNSPEC_SMULLT,
							 UNSPEC_UMULLT,
							 UNSPEC_FMLSLT))
FUNCTION (svmovlb, svmovl_lb_impl, (UNSPEC_SSHLLB, UNSPEC_USHLLB, -1))
FUNCTION (svmovlt, svmovl_lb_impl, (UNSPEC_SSHLLT, UNSPEC_USHLLT, -1))
FUNCTION (svmullb, unspec_based_function, (UNSPEC_SMULLB, UNSPEC_UMULLB, -1))
FUNCTION (svmullb_lane, unspec_based_lane_function, (UNSPEC_SMULLB,
						     UNSPEC_UMULLB, -1))
FUNCTION (svmullt, unspec_based_function, (UNSPEC_SMULLT, UNSPEC_UMULLT, -1))
FUNCTION (svmullt_lane, unspec_based_lane_function, (UNSPEC_SMULLT,
						     UNSPEC_UMULLT, -1))
FUNCTION (svnbsl, CODE_FOR_MODE0 (aarch64_sve2_nbsl),)
FUNCTION (svnmatch, svmatch_svnmatch_impl, (UNSPEC_NMATCH))
FUNCTION (svorqv, reduction, (UNSPEC_ORQV, UNSPEC_ORQV, -1))
FUNCTION (svpext_lane, svpext_lane_impl,)
FUNCTION (svpmov, svpmov_impl,)
FUNCTION (svpmov_lane, svpmov_lane_impl,)
FUNCTION (svpmul, CODE_FOR_MODE0 (aarch64_sve2_pmul),)
FUNCTION (svpmullb, unspec_based_function, (-1, UNSPEC_PMULLB, -1))
FUNCTION (svpmullb_pair, unspec_based_function, (-1, UNSPEC_PMULLB_PAIR, -1))
FUNCTION (svpmullt, unspec_based_function, (-1, UNSPEC_PMULLT, -1))
FUNCTION (svpmullt_pair, unspec_based_function, (-1, UNSPEC_PMULLT_PAIR, -1))
FUNCTION (svpsel_lane, svpsel_lane_impl,)
FUNCTION (svqabs, rtx_code_function, (SS_ABS, UNKNOWN, UNKNOWN))
FUNCTION (svqcadd, svqcadd_impl,)
FUNCTION (svqcvt, integer_conversion, (UNSPEC_SQCVT, UNSPEC_SQCVTU,
				       UNSPEC_UQCVT, -1))
FUNCTION (svqcvtn, integer_conversion, (UNSPEC_SQCVTN, UNSPEC_SQCVTUN,
					UNSPEC_UQCVTN, -1))
FUNCTION (svqdmlalb, unspec_based_qadd_function, (UNSPEC_SQDMULLB, -1, -1))
FUNCTION (svqdmlalb_lane, unspec_based_qadd_lane_function, (UNSPEC_SQDMULLB,
							    -1, -1))
FUNCTION (svqdmlalbt, unspec_based_qadd_function, (UNSPEC_SQDMULLBT, -1, -1))
FUNCTION (svqdmlalt, unspec_based_qadd_function, (UNSPEC_SQDMULLT, -1, -1))
FUNCTION (svqdmlalt_lane, unspec_based_qadd_lane_function, (UNSPEC_SQDMULLT,
							    -1, -1))
FUNCTION (svqdmlslb, unspec_based_qsub_function, (UNSPEC_SQDMULLB, -1, -1))
FUNCTION (svqdmlslb_lane, unspec_based_qsub_lane_function, (UNSPEC_SQDMULLB,
							    -1, -1))
FUNCTION (svqdmlslbt, unspec_based_qsub_function, (UNSPEC_SQDMULLBT, -1, -1))
FUNCTION (svqdmlslt, unspec_based_qsub_function, (UNSPEC_SQDMULLT, -1, -1))
FUNCTION (svqdmlslt_lane, unspec_based_qsub_lane_function, (UNSPEC_SQDMULLT,
							    -1, -1))
FUNCTION (svqdmulh, unspec_based_function, (UNSPEC_SQDMULH, -1, -1))
FUNCTION (svqdmulh_lane, unspec_based_lane_function, (UNSPEC_SQDMULH, -1, -1))
FUNCTION (svqdmullb, unspec_based_function, (UNSPEC_SQDMULLB, -1, -1))
FUNCTION (svqdmullb_lane, unspec_based_lane_function, (UNSPEC_SQDMULLB,
						       -1, -1))
FUNCTION (svqdmullt, unspec_based_function, (UNSPEC_SQDMULLT, -1, -1))
FUNCTION (svqdmullt_lane, unspec_based_lane_function, (UNSPEC_SQDMULLT,
						       -1, -1))
FUNCTION (svqneg, rtx_code_function, (SS_NEG, UNKNOWN, UNKNOWN))
FUNCTION (svqrdcmlah, svqrdcmlah_impl,)
FUNCTION (svqrdcmlah_lane, svqrdcmlah_lane_impl,)
FUNCTION (svqrdmlah, unspec_based_function, (UNSPEC_SQRDMLAH, -1, -1))
FUNCTION (svqrdmlah_lane, unspec_based_lane_function, (UNSPEC_SQRDMLAH,
						       -1, -1))
FUNCTION (svqrdmlsh, unspec_based_function, (UNSPEC_SQRDMLSH, -1, -1))
FUNCTION (svqrdmlsh_lane, unspec_based_lane_function, (UNSPEC_SQRDMLSH,
						       -1, -1))
FUNCTION (svqrdmulh, unspec_based_function, (UNSPEC_SQRDMULH, -1, -1))
FUNCTION (svqrdmulh_lane, unspec_based_lane_function, (UNSPEC_SQRDMULH,
						       -1, -1))
FUNCTION (svqrshl, svqrshl_impl,)
FUNCTION (svqrshr, unspec_based_uncond_function, (UNSPEC_SQRSHR,
						  UNSPEC_UQRSHR, -1, -1, 1))
FUNCTION (svqrshrn, unspec_based_uncond_function, (UNSPEC_SQRSHRN,
						   UNSPEC_UQRSHRN, -1, -1, 1))
FUNCTION (svqrshrnb, unspec_based_function, (UNSPEC_SQRSHRNB,
					     UNSPEC_UQRSHRNB, -1))
FUNCTION (svqrshrnt, unspec_based_function, (UNSPEC_SQRSHRNT,
					     UNSPEC_UQRSHRNT, -1))
FUNCTION (svqrshru, unspec_based_uncond_function, (UNSPEC_SQRSHRU, -1, -1, -1, 1))
FUNCTION (svqrshrun, unspec_based_uncond_function, (UNSPEC_SQRSHRUN, -1, -1, -1, 1))
FUNCTION (svqrshrunb, unspec_based_function, (UNSPEC_SQRSHRUNB, -1, -1))
FUNCTION (svqrshrunt, unspec_based_function, (UNSPEC_SQRSHRUNT, -1, -1))
FUNCTION (svqshl, svqshl_impl,)
FUNCTION (svqshlu, unspec_based_function, (UNSPEC_SQSHLU, -1, -1))
FUNCTION (svqshrnb, unspec_based_function, (UNSPEC_SQSHRNB,
					    UNSPEC_UQSHRNB, -1))
FUNCTION (svqshrnt, unspec_based_function, (UNSPEC_SQSHRNT,
					    UNSPEC_UQSHRNT, -1))
FUNCTION (svqshrunb, unspec_based_function, (UNSPEC_SQSHRUNB, -1, -1))
FUNCTION (svqshrunt, unspec_based_function, (UNSPEC_SQSHRUNT, -1, -1))
FUNCTION (svqsubr, rtx_code_function_rotated, (SS_MINUS, US_MINUS, -1))
FUNCTION (svqxtnb, unspec_based_function, (UNSPEC_SQXTNB, UNSPEC_UQXTNB, -1))
FUNCTION (svqxtnt, unspec_based_function, (UNSPEC_SQXTNT, UNSPEC_UQXTNT, -1))
FUNCTION (svqxtunb, unspec_based_function, (UNSPEC_SQXTUNB, -1, -1))
FUNCTION (svqxtunt, unspec_based_function, (UNSPEC_SQXTUNT, -1, -1))
FUNCTION (svraddhnb, unspec_based_function, (UNSPEC_RADDHNB,
					     UNSPEC_RADDHNB, -1))
FUNCTION (svraddhnt, unspec_based_function, (UNSPEC_RADDHNT,
					     UNSPEC_RADDHNT, -1))
FUNCTION (svrax1, fixed_insn_function, (CODE_FOR_aarch64_sve2_rax1))
FUNCTION (svrevd, unspec_based_function, (UNSPEC_REVD, UNSPEC_REVD,
					  UNSPEC_REVD))
FUNCTION (svrhadd, unspec_based_function, (UNSPEC_SRHADD, UNSPEC_URHADD, -1))
FUNCTION (svrshl, svrshl_impl,)
FUNCTION (svrshr, unspec_based_function, (UNSPEC_SRSHR, UNSPEC_URSHR, -1))
FUNCTION (svrshrnb, unspec_based_function, (UNSPEC_RSHRNB, UNSPEC_RSHRNB, -1))
FUNCTION (svrshrnt, unspec_based_function, (UNSPEC_RSHRNT, UNSPEC_RSHRNT, -1))
FUNCTION (svrsra, unspec_based_add_function, (UNSPEC_SRSHR, UNSPEC_URSHR, -1))
FUNCTION (svrsubhnb, unspec_based_function, (UNSPEC_RSUBHNB,
					     UNSPEC_RSUBHNB, -1))
FUNCTION (svrsubhnt, unspec_based_function, (UNSPEC_RSUBHNT,
					     UNSPEC_RSUBHNT, -1))
FUNCTION (svsbclb, unspec_based_function, (-1, UNSPEC_SBCLB, -1))
FUNCTION (svsbclt, unspec_based_function, (-1, UNSPEC_SBCLT, -1))
FUNCTION (svshllb, unspec_based_function, (UNSPEC_SSHLLB, UNSPEC_USHLLB, -1))
FUNCTION (svshllt, unspec_based_function, (UNSPEC_SSHLLT, UNSPEC_USHLLT, -1))
FUNCTION (svshrnb, unspec_based_function, (UNSPEC_SHRNB, UNSPEC_SHRNB, -1))
FUNCTION (svshrnt, unspec_based_function, (UNSPEC_SHRNT, UNSPEC_SHRNT, -1))
FUNCTION (svsli, unspec_based_function, (UNSPEC_SLI, UNSPEC_SLI, -1))
FUNCTION (svsm4e, fixed_insn_function, (CODE_FOR_aarch64_sve2_sm4e))
FUNCTION (svsm4ekey, fixed_insn_function, (CODE_FOR_aarch64_sve2_sm4ekey))
FUNCTION (svsqadd, svsqadd_impl,)
FUNCTION (svsra, svsra_impl,)
FUNCTION (svsri, unspec_based_function, (UNSPEC_SRI, UNSPEC_SRI, -1))
FUNCTION (svst1dq, svst1xq_impl, (VNx1DImode))
FUNCTION (svst1q_scatter, svst1q_scatter_impl,)
FUNCTION (svst1wq, svst1xq_impl, (VNx1SImode))
FUNCTION (svst2q, svst234q_impl, (2, VNx2TImode))
FUNCTION (svst3q, svst234q_impl, (3, VNx3TImode))
FUNCTION (svst4q, svst234q_impl, (4, VNx4TImode))
FUNCTION (svstnt1_scatter, svstnt1_scatter_impl,)
FUNCTION (svstnt1b_scatter, svstnt1_scatter_truncate_impl, (QImode))
FUNCTION (svstnt1h_scatter, svstnt1_scatter_truncate_impl, (HImode))
FUNCTION (svstnt1w_scatter, svstnt1_scatter_truncate_impl, (SImode))
FUNCTION (svsubhnb, unspec_based_function, (UNSPEC_SUBHNB, UNSPEC_SUBHNB, -1))
FUNCTION (svsubhnt, unspec_based_function, (UNSPEC_SUBHNT, UNSPEC_SUBHNT, -1))
FUNCTION (svsublb, unspec_based_function, (UNSPEC_SSUBLB, UNSPEC_USUBLB, -1))
FUNCTION (svsublbt, unspec_based_function, (UNSPEC_SSUBLBT, -1, -1))
FUNCTION (svsublt, unspec_based_function, (UNSPEC_SSUBLT, UNSPEC_USUBLT, -1))
FUNCTION (svsubltb, unspec_based_function, (UNSPEC_SSUBLTB, -1, -1))
FUNCTION (svsubwb, unspec_based_function, (UNSPEC_SSUBWB, UNSPEC_USUBWB, -1))
FUNCTION (svsubwt, unspec_based_function, (UNSPEC_SSUBWT, UNSPEC_USUBWT, -1))
FUNCTION (svtbl2, svtbl2_impl,)
FUNCTION (svtblq, quiet<unspec_based_uncond_function>, (UNSPEC_TBLQ,
							UNSPEC_TBLQ,
							UNSPEC_TBLQ))
FUNCTION (svtbx, quiet<unspec_based_uncond_function>, (UNSPEC_TBX, UNSPEC_TBX,
						       UNSPEC_TBX))
FUNCTION (svtbxq, quiet<unspec_based_uncond_function>, (UNSPEC_TBXQ,
							UNSPEC_TBXQ,
							UNSPEC_TBXQ))
FUNCTION (svunpk, svunpk_impl,)
FUNCTION (svuqadd, svuqadd_impl,)
FUNCTION (svuzp, multireg_permute, (UNSPEC_UZP))
FUNCTION (svuzpq, multireg_permute, (UNSPEC_UZPQ))
FUNCTION (svuzpq1, svuzpq_impl, (0))
FUNCTION (svuzpq2, svuzpq_impl, (1))
FUNCTION (svwhilege, while_comparison, (UNSPEC_WHILEGE, UNSPEC_WHILEHS))
FUNCTION (svwhilegt, while_comparison, (UNSPEC_WHILEGT, UNSPEC_WHILEHI))
FUNCTION (svwhilerw, svwhilerw_svwhilewr_impl, (UNSPEC_WHILERW))
FUNCTION (svwhilewr, svwhilerw_svwhilewr_impl, (UNSPEC_WHILEWR))
FUNCTION (svxar, svxar_impl,)
FUNCTION (svzip, multireg_permute, (UNSPEC_ZIP))
FUNCTION (svzipq, multireg_permute, (UNSPEC_ZIPQ))
FUNCTION (svzipq1, svzipq_impl, (0))
FUNCTION (svzipq2, svzipq_impl, (1))

} /* end namespace aarch64_sve */
