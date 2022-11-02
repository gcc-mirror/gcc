/* Subroutines used for code generation for RISC-V 'V' Extension for GNU
   compiler. Copyright (C) 2022-2022 Free Software Foundation, Inc. Contributed
   by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "backend.h"
#include "rtl.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "alias.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "explow.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tm_p.h"
#include "target.h"
#include "expr.h"
#include "optabs.h"

using namespace riscv_vector;

namespace riscv_vector {

template <int MAX_OPERANDS> class insn_expander
{
public:
  insn_expander () : m_opno (0) {}
  void add_output_operand (rtx x, machine_mode mode)
  {
    create_output_operand (&m_ops[m_opno++], x, mode);
    gcc_assert (m_opno <= MAX_OPERANDS);
  }
  void add_input_operand (rtx x, machine_mode mode)
  {
    create_input_operand (&m_ops[m_opno++], x, mode);
    gcc_assert (m_opno <= MAX_OPERANDS);
  }
  void add_all_one_mask_operand (machine_mode mode)
  {
    add_input_operand (CONSTM1_RTX (mode), mode);
  }
  void add_vundef_operand (machine_mode mode)
  {
    add_input_operand (gen_rtx_UNSPEC (mode, gen_rtvec (1, const0_rtx),
				       UNSPEC_VUNDEF),
		       mode);
  }
  void add_policy_operand (enum tail_policy vta, enum mask_policy vma)
  {
    rtx tail_policy_rtx = vta == TAIL_UNDISTURBED ? const0_rtx : const1_rtx;
    rtx mask_policy_rtx = vma == MASK_UNDISTURBED ? const0_rtx : const1_rtx;
    add_input_operand (tail_policy_rtx, Pmode);
    add_input_operand (mask_policy_rtx, Pmode);
  }

  void expand (enum insn_code icode, bool temporary_volatile_p = false)
  {
    if (temporary_volatile_p)
      {
	temporary_volatile_ok v (true);
	expand_insn (icode, m_opno, m_ops);
      }
    else
      expand_insn (icode, m_opno, m_ops);
  }

private:
  int m_opno;
  expand_operand m_ops[MAX_OPERANDS];
};

/* Return true if X is a const_vector with all duplicate elements, which is in
   the range between MINVAL and MAXVAL.  */
bool
const_vec_all_same_in_range_p (rtx x, HOST_WIDE_INT minval,
			       HOST_WIDE_INT maxval)
{
  rtx elt;
  return (const_vec_duplicate_p (x, &elt) && CONST_INT_P (elt)
	  && IN_RANGE (INTVAL (elt), minval, maxval));
}

/* Emit an RVV unmask && vl mov from SRC to DEST.  */
static void
emit_pred_move (rtx dest, rtx src, rtx vl, machine_mode mask_mode)
{
  insn_expander<7> e;

  machine_mode mode = GET_MODE (dest);
  if (register_operand (src, mode) && register_operand (dest, mode))
    {
      emit_move_insn (dest, src);
      return;
    }

  e.add_output_operand (dest, mode);
  e.add_all_one_mask_operand (mask_mode);
  /* For load operation, we create undef operand.
     For store operation, we make it depend on the dest memory to
     avoid potential bugs.  */
  if (MEM_P (src))
    e.add_vundef_operand (mode);
  else
    e.add_input_operand (dest, mode);

  e.add_input_operand (src, mode);
  e.add_input_operand (vl, Pmode);

  e.add_policy_operand (TAIL_AGNOSTIC, MASK_AGNOSTIC);

  enum insn_code icode;
  icode = code_for_pred_mov (mode);
  e.expand (icode, true);
}

/* Expand a pre-RA RVV data move from SRC to DEST.
   It expands move for RVV fractional vector modes.  */
bool
legitimize_move (rtx dest, rtx src, machine_mode mask_mode)
{
  machine_mode mode = GET_MODE (dest);
  /* For whole registers load/store or register-register move,
     we don't need to specially handle them, just let them go
     through "*mov<mode>" and then use the codegen directly.  */
  if ((known_ge (GET_MODE_SIZE (mode), BYTES_PER_RISCV_VECTOR)
       && (GET_MODE_CLASS (mode) != MODE_VECTOR_BOOL))
      || (register_operand (src, mode) && register_operand (dest, mode)))
    {
      /* Need to force register if mem <- !reg.  */
      if (MEM_P (dest) && !REG_P (src))
	src = force_reg (mode, src);
      return false;
    }

  rtx vlmax = gen_reg_rtx (Pmode);
  unsigned int sew = GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
		       ? 8
		       : GET_MODE_BITSIZE (GET_MODE_INNER (mode));
  emit_insn (gen_vsetvl_no_side_effects (
    Pmode, vlmax, gen_rtx_REG (Pmode, 0), gen_int_mode (sew, Pmode),
    gen_int_mode ((unsigned int) mode, Pmode), const1_rtx, const1_rtx));

  if (!register_operand (src, mode) && !register_operand (dest, mode))
    {
      rtx tmp = gen_reg_rtx (mode);
      if (MEM_P (src))
	emit_pred_move (tmp, src, vlmax, mask_mode);
      else
	emit_move_insn (tmp, src);
      src = tmp;
    }
  emit_pred_move (dest, src, vlmax, mask_mode);
  return true;
}

} // namespace riscv_vector
