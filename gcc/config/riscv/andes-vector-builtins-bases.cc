/* function_base implementation for Andes custom 'V' Extension for GNU compiler.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
   Contributed by Andes.

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
#include "andes-vector-builtins-bases.h"
#include "riscv-vector-builtins-bases.h"

using namespace riscv_vector;

namespace riscv_vector {

/* Implements Andes vfwcvt.  */
template <enum frm_op_type FRM_OP = NO_FRM>
class nds_vfncvtbf16_f : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_rounding_mode_operand_p () const override
  {
      return FRM_OP == HAS_FRM;
  }
  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_nds_vfncvt_bf16
			     (e.vector_mode ()));
  }
};

class nds_vfwcvtbf16_f : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_nds_vfwcvt_bf16 (e.vector_mode ()));
  }
};

/* Implements Andes vln8.v/vln8.v.  */
template <bool SIGN>
class nds_nibbleload : public function_base
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
    if (SIGN)
      return e.use_contiguous_load_insn (
	code_for_pred_intload_mov (SIGN_EXTEND, e.vector_mode ()));
    return e.use_contiguous_load_insn (
      code_for_pred_intload_mov (ZERO_EXTEND, e.vector_mode ()));
  }
};

template<int UNSPEC, enum frm_op_type FRM_OP = NO_FRM>
class nds_vfpmad : public function_base
{
public:
  bool has_rounding_mode_operand_p () const override
  {
    return FRM_OP == HAS_FRM;
  }
  bool may_require_frm_p () const override { return true; }

  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_pred_nds_vfpmad (UNSPEC,
						       e.vector_mode ()));
  }
};

/* Implements Andes vdot.  */
template<rtx_code EXTEND>
class nds_vd4dot : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_widen_ternop_insn
      (code_for_pred_nds_vd4dot (EXTEND, e.vector_mode ()));
  }
};

/* Implements vwmacc<su><su>.  */
class nds_vd4dotsu : public function_base
{
public:
  bool has_merge_operand_p () const override { return false; }

  rtx expand (function_expander &e) const override
  {
    return e.use_widen_ternop_insn
      (code_for_pred_nds_vd4dotsu (e.vector_mode ()));
  }
};

static CONSTEXPR const nds_vfwcvtbf16_f nds_vfwcvt_s_obj;
static CONSTEXPR const nds_vfncvtbf16_f<NO_FRM> nds_vfncvt_bf16_obj;
static CONSTEXPR const nds_vfncvtbf16_f<HAS_FRM> nds_vfncvt_bf16_frm_obj;
static CONSTEXPR const nds_nibbleload<true> nds_vln8_obj;
static CONSTEXPR const nds_nibbleload<false> nds_vlnu8_obj;
static CONSTEXPR const nds_vfpmad <UNSPEC_NDS_VFPMADT, NO_FRM> nds_vfpmadt_obj;
static CONSTEXPR const nds_vfpmad <UNSPEC_NDS_VFPMADB, NO_FRM> nds_vfpmadb_obj;
static CONSTEXPR const nds_vfpmad <UNSPEC_NDS_VFPMADT, HAS_FRM> nds_vfpmadt_frm_obj;
static CONSTEXPR const nds_vfpmad <UNSPEC_NDS_VFPMADB, HAS_FRM> nds_vfpmadb_frm_obj;
static CONSTEXPR const nds_vd4dot<SIGN_EXTEND> nds_vd4dots_obj;
static CONSTEXPR const nds_vd4dot<ZERO_EXTEND> nds_vd4dotu_obj;
static CONSTEXPR const nds_vd4dotsu nds_vd4dotsu_obj;

/* Declare the function base NAME, pointing it to an instance
   of class <NAME>_obj.  */
#define BASE(NAME) \
  namespace bases { const function_base *const NAME = &NAME##_obj; }

BASE (nds_vfwcvt_s)
BASE (nds_vfncvt_bf16)
BASE (nds_vfncvt_bf16_frm)
BASE (nds_vln8)
BASE (nds_vlnu8)
BASE (nds_vfpmadt)
BASE (nds_vfpmadb)
BASE (nds_vfpmadt_frm)
BASE (nds_vfpmadb_frm)
BASE (nds_vd4dots)
BASE (nds_vd4dotu)
BASE (nds_vd4dotsu)
} // end namespace riscv_vector
