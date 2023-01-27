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

/* Implements vle.v/vse.v/vlm.v/vsm.v/vlse.v/vsse.v codegen.  */
template <bool STORE_P, bool STRIDED_P = false>
class loadstore : public function_base
{
  unsigned int call_properties (const function_instance &) const override
  {
    if (STORE_P)
      return CP_WRITE_MEMORY;
    else
      return CP_READ_MEMORY;
  }

  bool can_be_overloaded_p (enum predication_type_index pred) const override
  {
    if (STORE_P)
      return true;
    return pred != PRED_TYPE_none && pred != PRED_TYPE_mu;
  }

  rtx expand (function_expander &e) const override
  {
    if (STORE_P)
      {
	if (STRIDED_P)
	  return e.use_contiguous_store_insn (
	    code_for_pred_strided_store (e.vector_mode ()));
	else
	  return e.use_contiguous_store_insn (
	    code_for_pred_store (e.vector_mode ()));
      }
    else
      {
	if (STRIDED_P)
	  return e.use_contiguous_load_insn (
	    code_for_pred_strided_load (e.vector_mode ()));
	else
	  return e.use_contiguous_load_insn (
	    code_for_pred_mov (e.vector_mode ()));
      }
  }
};

static CONSTEXPR const vsetvl<false> vsetvl_obj;
static CONSTEXPR const vsetvl<true> vsetvlmax_obj;
static CONSTEXPR const loadstore<false> vle_obj;
static CONSTEXPR const loadstore<true> vse_obj;
static CONSTEXPR const loadstore<false> vlm_obj;
static CONSTEXPR const loadstore<true> vsm_obj;
static CONSTEXPR const loadstore<false, true> vlse_obj;
static CONSTEXPR const loadstore<true, true> vsse_obj;

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

} // end namespace riscv_vector
