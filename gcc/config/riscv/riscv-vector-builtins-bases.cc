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
	  return e.use_exact_insn (
	    code_for_pred_indexed_load (unspec, e.vector_mode (),
					e.index_mode ()));
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
 * vadd/vsub/vrsub/vand/vor/vxor/vsll/vsra/vsrl/vmin/vmax/vminu/vmaxu/vdiv/vrem/vdivu/vremu/vsadd/vsaddu/vssub/vssubu.
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
	return e.use_exact_insn (code_for_pred_scalar (CODE, e.vector_mode ()));
      case OP_TYPE_vv:
	return e.use_exact_insn (code_for_pred (CODE, e.vector_mode ()));
      default:
	gcc_unreachable ();
      }
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
static CONSTEXPR const binop<MINUS> vrsub_obj;
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
static CONSTEXPR const binop<DIV> vdiv_obj;
static CONSTEXPR const binop<MOD> vrem_obj;
static CONSTEXPR const binop<UDIV> vdivu_obj;
static CONSTEXPR const binop<UMOD> vremu_obj;

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
BASE (vdiv)
BASE (vrem)
BASE (vdivu)
BASE (vremu)

} // end namespace riscv_vector
