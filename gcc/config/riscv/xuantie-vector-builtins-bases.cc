/* function_base implementation for XuanTie custom 'V' Extension for GNU compiler.
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by Alibaba DAMO Academy XuanTie Team.

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
#include "xuantie-vector-builtins-bases.h"
#include "riscv-vector-builtins-bases.h"

using namespace riscv_vector;

namespace riscv_vector {

/* XXTVarith */
/* Implements xt.vgmulxor.vv/xt.vilo.vv/xt.vile.vv/xt.vcrcfoldr.vv
   xt.vcrcfoldn.vv.  */
template<int UNSPEC>
class xt_varith : public function_base
{
public:
  bool apply_mask_policy_p () const override { return false; }
  bool use_mask_predication_p () const override { return false; }
  bool has_merge_operand_p () const override
  {
    return (UNSPEC == UNSPEC_XT_VILE || UNSPEC == UNSPEC_XT_VILO);
  }
  rtx expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_xt_pred (UNSPEC, e.vector_mode ()));
  }
};

/* XXTVarith */
static constexpr const xt_varith<UNSPEC_XT_VGMULXOR> xt_vgmulxor_obj;
static constexpr const xt_varith<UNSPEC_XT_VILO> xt_vilo_obj;
static constexpr const xt_varith<UNSPEC_XT_VILE> xt_vile_obj;
static constexpr const xt_varith<UNSPEC_XT_VCRCFOLDR> xt_vcrcfoldr_obj;
static constexpr const xt_varith<UNSPEC_XT_VCRCFOLDN> xt_vcrcfoldn_obj;

/* Declare the function base NAME, pointing it to an instance
   of class <NAME>_obj.  */
#define BASE(NAME) \
  namespace bases { const function_base *const NAME = &NAME##_obj; }

/* XXTVarith */
BASE (xt_vgmulxor)
BASE (xt_vilo)
BASE (xt_vile)
BASE (xt_vcrcfoldr)
BASE (xt_vcrcfoldn)
} // end namespace riscv_vector
