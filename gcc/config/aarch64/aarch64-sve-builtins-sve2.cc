/* ACLE support for AArch64 SVE (__ARM_FEATURE_SVE2 intrinsics)
   Copyright (C) 2020 Free Software Foundation, Inc.

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

class svaba_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const OVERRIDE
  {
    rtx_code max_code = e.type_suffix (0).unsigned_p ? UMAX : SMAX;
    machine_mode mode = e.vector_mode (0);
    return e.use_exact_insn (code_for_aarch64_sve2_aba (max_code, mode));
  }
};

class svcdot_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const OVERRIDE
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
  expand (function_expander &e) const OVERRIDE
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    return e.use_exact_insn (code_for_aarch64_lane (unspec_cdot (rot),
						    e.vector_mode (0)));
  }
};

class svldnt1_gather_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const OVERRIDE
  {
    return CP_READ_MEMORY;
  }

  rtx
  expand (function_expander &e) const OVERRIDE
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
  CONSTEXPR svldnt1_gather_extend_impl (type_suffix_index memory_type)
    : extending_load (memory_type) {}

  rtx
  expand (function_expander &e) const OVERRIDE
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

  rtx
  expand (function_expander &e) const OVERRIDE
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
  CONSTEXPR svmovl_lb_impl (int unspec_for_sint, int unspec_for_uint,
			    int unspec_for_fp)
    : unspec_based_function_base (unspec_for_sint, unspec_for_uint,
				  unspec_for_fp)
  {}

  rtx
  expand (function_expander &e) const OVERRIDE
  {
    e.args.quick_push (const0_rtx);
    return e.map_to_unspecs (m_unspec_for_sint, m_unspec_for_uint,
			     m_unspec_for_fp);
  }
};

class svqcadd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const OVERRIDE
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
  expand (function_expander &e) const OVERRIDE
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
  expand (function_expander &e) const OVERRIDE
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
  fold (gimple_folder &f) const OVERRIDE
  {
    if (tree amount = uniform_integer_cst_p (gimple_call_arg (f.call, 2)))
      {
	if (wi::to_widest (amount) >= 0)
	  {
	    /* The rounding has no effect, and [SU]QSHL has immediate forms
	       that we can use for sensible shift amounts.  */
	    function_instance instance ("svqshl", functions::svqshl,
					shapes::binary_int_opt_n, MODE_n,
					f.type_suffix_ids, f.pred);
	    return f.redirect_call (instance);
	  }
	else
	  {
	    /* The saturation has no effect, and [SU]RSHL has immediate forms
	       that we can use for sensible shift amounts.  */
	    function_instance instance ("svrshl", functions::svrshl,
					shapes::binary_int_opt_n, MODE_n,
					f.type_suffix_ids, f.pred);
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
  fold (gimple_folder &f) const OVERRIDE
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
					f.type_suffix_ids, f.pred);
	    if (f.type_suffix (0).unsigned_p)
	      {
		instance.base_name = "svlsr";
		instance.base = functions::svlsr;
	      }
	    gcall *call = as_a <gcall *> (f.redirect_call (instance));
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
  fold (gimple_folder &f) const OVERRIDE
  {
    if (tree amount = uniform_integer_cst_p (gimple_call_arg (f.call, 2)))
      {
	if (wi::to_widest (amount) >= 0)
	  {
	    /* The rounding has no effect, and LSL has immediate forms
	       that we can use for sensible shift amounts.  */
	    function_instance instance ("svlsl", functions::svlsl,
					shapes::binary_uint_opt_n, MODE_n,
					f.type_suffix_ids, f.pred);
	    gcall *call = as_a <gcall *> (f.redirect_call (instance));
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
					f.type_suffix_ids, f.pred);
	    gcall *call = as_a <gcall *> (f.redirect_call (instance));
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
  expand (function_expander &e) const OVERRIDE
  {
    machine_mode mode = e.vector_mode (0);
    if (e.pred == PRED_x
	&& aarch64_sve_sqadd_sqsub_immediate_p (mode, e.args[2], false))
      return e.map_to_rtx_codes (UNKNOWN, US_PLUS, -1);
    return e.map_to_unspecs (-1, UNSPEC_USQADD, -1);
  }
};

class svsra_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const OVERRIDE
  {
    rtx_code shift_code = e.type_suffix (0).unsigned_p ? LSHIFTRT : ASHIFTRT;
    machine_mode mode = e.vector_mode (0);
    return e.use_exact_insn (code_for_aarch64_sve_add (shift_code, mode));
  }
};

class svstnt1_scatter_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const OVERRIDE
  {
    return CP_WRITE_MEMORY;
  }

  rtx
  expand (function_expander &e) const OVERRIDE
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
  CONSTEXPR svstnt1_scatter_truncate_impl (scalar_int_mode to_mode)
    : truncating_store (to_mode) {}

  rtx
  expand (function_expander &e) const OVERRIDE
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
  expand (function_expander &e) const OVERRIDE
  {
    return e.use_exact_insn (code_for_aarch64_sve2_tbl2 (e.vector_mode (0)));
  }
};

class svuqadd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const OVERRIDE
  {
    machine_mode mode = e.vector_mode (0);
    if (e.pred == PRED_x
	&& aarch64_sve_arith_immediate_p (mode, e.args[2], false))
      return e.use_unpred_insn (code_for_aarch64_sve_suqadd_const (mode));
    return e.map_to_unspecs (UNSPEC_SUQADD, -1, -1);
  }
};

/* Implements both svwhilerw and svwhilewr; the unspec parameter decides
   between them.  */
class svwhilerw_svwhilewr_impl : public full_width_access
{
public:
  CONSTEXPR svwhilerw_svwhilewr_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const OVERRIDE
  {
    for (unsigned int i = 0; i < 2; ++i)
      e.args[i] = e.convert_to_pmode (e.args[i]);
    return e.use_exact_insn (code_for_while (m_unspec, Pmode, e.gp_mode (0)));
  }

  int m_unspec;
};

} /* end anonymous namespace */

namespace aarch64_sve {

FUNCTION (svaba, svaba_impl,)
FUNCTION (svabalb, unspec_based_add_function, (UNSPEC_SABDLB,
					       UNSPEC_UABDLB, -1))
FUNCTION (svabalt, unspec_based_add_function, (UNSPEC_SABDLT,
					       UNSPEC_UABDLT, -1))
FUNCTION (svadclb, unspec_based_function, (-1, UNSPEC_ADCLB, -1))
FUNCTION (svadclt, unspec_based_function, (-1, UNSPEC_ADCLT, -1))
FUNCTION (svaddhnb, unspec_based_function, (UNSPEC_ADDHNB, UNSPEC_ADDHNB, -1))
FUNCTION (svaddhnt, unspec_based_function, (UNSPEC_ADDHNT, UNSPEC_ADDHNT, -1))
FUNCTION (svabdlb, unspec_based_function, (UNSPEC_SABDLB, UNSPEC_UABDLB, -1))
FUNCTION (svabdlt, unspec_based_function, (UNSPEC_SABDLT, UNSPEC_UABDLT, -1))
FUNCTION (svadalp, unspec_based_function, (UNSPEC_SADALP, UNSPEC_UADALP, -1))
FUNCTION (svaddlb, unspec_based_function, (UNSPEC_SADDLB, UNSPEC_UADDLB, -1))
FUNCTION (svaddlbt, unspec_based_function, (UNSPEC_SADDLBT, -1, -1))
FUNCTION (svaddlt, unspec_based_function, (UNSPEC_SADDLT, UNSPEC_UADDLT, -1))
FUNCTION (svaddwb, unspec_based_function, (UNSPEC_SADDWB, UNSPEC_UADDWB, -1))
FUNCTION (svaddwt, unspec_based_function, (UNSPEC_SADDWT, UNSPEC_UADDWT, -1))
FUNCTION (svaddp, unspec_based_pred_function, (UNSPEC_ADDP, UNSPEC_ADDP,
					       UNSPEC_FADDP))
FUNCTION (svaesd, fixed_insn_function, (CODE_FOR_aarch64_sve2_aesd))
FUNCTION (svaese, fixed_insn_function, (CODE_FOR_aarch64_sve2_aese))
FUNCTION (svaesimc, fixed_insn_function, (CODE_FOR_aarch64_sve2_aesimc))
FUNCTION (svaesmc, fixed_insn_function, (CODE_FOR_aarch64_sve2_aesmc))
FUNCTION (svbcax, CODE_FOR_MODE0 (aarch64_sve2_bcax),)
FUNCTION (svbdep, unspec_based_function, (UNSPEC_BDEP, UNSPEC_BDEP, -1))
FUNCTION (svbext, unspec_based_function, (UNSPEC_BEXT, UNSPEC_BEXT, -1))
FUNCTION (svbgrp, unspec_based_function, (UNSPEC_BGRP, UNSPEC_BGRP, -1))
FUNCTION (svbsl, CODE_FOR_MODE0 (aarch64_sve2_bsl),)
FUNCTION (svbsl1n, CODE_FOR_MODE0 (aarch64_sve2_bsl1n),)
FUNCTION (svbsl2n, CODE_FOR_MODE0 (aarch64_sve2_bsl2n),)
FUNCTION (svcdot, svcdot_impl,)
FUNCTION (svcdot_lane, svcdot_lane_impl,)
FUNCTION (svcvtlt, unspec_based_function, (-1, -1, UNSPEC_COND_FCVTLT))
FUNCTION (svcvtx, unspec_based_function, (-1, -1, UNSPEC_COND_FCVTX))
FUNCTION (svcvtxnt, CODE_FOR_MODE1 (aarch64_sve2_cvtxnt),)
FUNCTION (sveor3, CODE_FOR_MODE0 (aarch64_sve2_eor3),)
FUNCTION (sveorbt, unspec_based_function, (UNSPEC_EORBT, UNSPEC_EORBT, -1))
FUNCTION (sveortb, unspec_based_function, (UNSPEC_EORTB, UNSPEC_EORTB, -1))
FUNCTION (svhadd, unspec_based_function, (UNSPEC_SHADD, UNSPEC_UHADD, -1))
FUNCTION (svhsub, unspec_based_function, (UNSPEC_SHSUB, UNSPEC_UHSUB, -1))
FUNCTION (svhistcnt, CODE_FOR_MODE0 (aarch64_sve2_histcnt),)
FUNCTION (svhistseg, CODE_FOR_MODE0 (aarch64_sve2_histseg),)
FUNCTION (svhsubr, unspec_based_function_rotated, (UNSPEC_SHSUB,
						   UNSPEC_UHSUB, -1))
FUNCTION (svldnt1_gather, svldnt1_gather_impl,)
FUNCTION (svldnt1sb_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_s8))
FUNCTION (svldnt1sh_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_s16))
FUNCTION (svldnt1sw_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_s32))
FUNCTION (svldnt1ub_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_u8))
FUNCTION (svldnt1uh_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_u16))
FUNCTION (svldnt1uw_gather, svldnt1_gather_extend_impl, (TYPE_SUFFIX_u32))
FUNCTION (svlogb, unspec_based_function, (-1, -1, UNSPEC_COND_FLOGB))
FUNCTION (svmatch, svmatch_svnmatch_impl, (UNSPEC_MATCH))
FUNCTION (svmaxp, unspec_based_pred_function, (UNSPEC_SMAXP, UNSPEC_UMAXP,
					       UNSPEC_FMAXP))
FUNCTION (svmaxnmp, unspec_based_pred_function, (-1, -1, UNSPEC_FMAXNMP))
FUNCTION (svminp, unspec_based_pred_function, (UNSPEC_SMINP, UNSPEC_UMINP,
					       UNSPEC_FMINP))
FUNCTION (svminnmp, unspec_based_pred_function, (-1, -1, UNSPEC_FMINNMP))
FUNCTION (svmlalb, unspec_based_mla_function, (UNSPEC_SMULLB,
					       UNSPEC_UMULLB, UNSPEC_FMLALB))
FUNCTION (svmlalb_lane, unspec_based_mla_lane_function, (UNSPEC_SMULLB,
							 UNSPEC_UMULLB,
							 UNSPEC_FMLALB))
FUNCTION (svmlalt, unspec_based_mla_function, (UNSPEC_SMULLT,
					       UNSPEC_UMULLT, UNSPEC_FMLALT))
FUNCTION (svmlalt_lane, unspec_based_mla_lane_function, (UNSPEC_SMULLT,
							 UNSPEC_UMULLT,
							 UNSPEC_FMLALT))
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
FUNCTION (svpmul, CODE_FOR_MODE0 (aarch64_sve2_pmul),)
FUNCTION (svpmullb, unspec_based_function, (-1, UNSPEC_PMULLB, -1))
FUNCTION (svpmullb_pair, unspec_based_function, (-1, UNSPEC_PMULLB_PAIR, -1))
FUNCTION (svpmullt, unspec_based_function, (-1, UNSPEC_PMULLT, -1))
FUNCTION (svpmullt_pair, unspec_based_function, (-1, UNSPEC_PMULLT_PAIR, -1))
FUNCTION (svqabs, rtx_code_function, (SS_ABS, UNKNOWN, UNKNOWN))
FUNCTION (svqcadd, svqcadd_impl,)
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
FUNCTION (svqrdmulh, unspec_based_function, (UNSPEC_SQRDMULH, -1, -1))
FUNCTION (svqrdmulh_lane, unspec_based_lane_function, (UNSPEC_SQRDMULH,
						       -1, -1))
FUNCTION (svqrdmlah, unspec_based_function, (UNSPEC_SQRDMLAH, -1, -1))
FUNCTION (svqrdmlah_lane, unspec_based_lane_function, (UNSPEC_SQRDMLAH,
						       -1, -1))
FUNCTION (svqrdmlsh, unspec_based_function, (UNSPEC_SQRDMLSH, -1, -1))
FUNCTION (svqrdmlsh_lane, unspec_based_lane_function, (UNSPEC_SQRDMLSH,
						       -1, -1))
FUNCTION (svqrshl, svqrshl_impl,)
FUNCTION (svqrshrnb, unspec_based_function, (UNSPEC_SQRSHRNB,
					     UNSPEC_UQRSHRNB, -1))
FUNCTION (svqrshrnt, unspec_based_function, (UNSPEC_SQRSHRNT,
					     UNSPEC_UQRSHRNT, -1))
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
FUNCTION (svtbx, CODE_FOR_MODE0 (aarch64_sve2_tbx),)
FUNCTION (svuqadd, svuqadd_impl,)
FUNCTION (svwhilege, while_comparison, (UNSPEC_WHILEGE, UNSPEC_WHILEHS))
FUNCTION (svwhilegt, while_comparison, (UNSPEC_WHILEGT, UNSPEC_WHILEHI))
FUNCTION (svwhilerw, svwhilerw_svwhilewr_impl, (UNSPEC_WHILERW))
FUNCTION (svwhilewr, svwhilerw_svwhilewr_impl, (UNSPEC_WHILEWR))
FUNCTION (svxar, CODE_FOR_MODE0 (aarch64_sve2_xar),)

} /* end namespace aarch64_sve */
