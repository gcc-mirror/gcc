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

static CONSTEXPR const nds_vfwcvtbf16_f nds_vfwcvt_s_obj;
static CONSTEXPR const nds_vfncvtbf16_f<NO_FRM> nds_vfncvt_bf16_obj;
static CONSTEXPR const nds_vfncvtbf16_f<HAS_FRM> nds_vfncvt_bf16_frm_obj;

/* Declare the function base NAME, pointing it to an instance
   of class <NAME>_obj.  */
#define BASE(NAME) \
  namespace bases { const function_base *const NAME = &NAME##_obj; }

BASE (nds_vfwcvt_s)
BASE (nds_vfncvt_bf16)
BASE (nds_vfncvt_bf16_frm)

} // end namespace riscv_vector
