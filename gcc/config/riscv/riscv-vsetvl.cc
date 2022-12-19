/* VSETVL pass for RISC-V 'V' Extension for GNU compiler.
   Copyright(C) 2022-2022 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or(at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/*  This pass is to Set VL/VTYPE global status for RVV instructions
    that depend on VL and VTYPE registers by Lazy code motion (LCM).

    Strategy:

    -  Backward demanded info fusion within block.

    -  Lazy code motion (LCM) based demanded info backward propagation.

    -  RTL_SSA framework for def-use, PHI analysis.

    -  Lazy code motion (LCM) for global VL/VTYPE optimization.

    Assumption:

    -  Each avl operand is either an immediate (must be in range 0 ~ 31) or reg.

    This pass consists of 5 phases:

    -  Phase 1 - compute VL/VTYPE demanded information within each block
       by backward data-flow analysis.

    -  Phase 2 - Emit vsetvl instructions within each basic block according to
       demand, compute and save ANTLOC && AVLOC of each block.

    -  Phase 3 - Backward demanded info propagation and fusion across blocks.

    -  Phase 4 - Lazy code motion including: compute local properties,
       pre_edge_lcm and vsetvl insertion && delete edges for LCM results.

    -  Phase 5 - Cleanup AVL operand of RVV instruction since it will not be
       used any more and VL operand of VSETVL instruction if it is not used by
       any non-debug instructions.

    Implementation:

    -  The subroutine of optimize == 0 is simple_vsetvl.
       This function simplily vsetvl insertion for each RVV
       instruction. No optimization.

    -  The subroutine of optimize > 0 is lazy_vsetvl.
       This function optimize vsetvl insertion process by
       lazy code motion (LCM) layering on RTL_SSA.  */

#define IN_TARGET_CODE 1
#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "backend.h"
#include "rtl.h"
#include "target.h"
#include "tree-pass.h"
#include "df.h"
#include "rtl-ssa.h"
#include "cfgcleanup.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "insn-opinit.h"
#include "tm-constrs.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "predict.h"
#include "profile-count.h"
#include "riscv-vsetvl.h"

using namespace rtl_ssa;
using namespace riscv_vector;

DEBUG_FUNCTION void
debug (const vector_insn_info *info)
{
  info->dump (stderr);
}

DEBUG_FUNCTION void
debug (const vector_infos_manager *info)
{
  info->dump (stderr);
}

static bool
vlmax_avl_p (rtx x)
{
  return x && rtx_equal_p (x, RVV_VLMAX);
}

static bool
vlmax_avl_insn_p (rtx_insn *rinsn)
{
  return (INSN_CODE (rinsn) == CODE_FOR_vlmax_avlsi
	  || INSN_CODE (rinsn) == CODE_FOR_vlmax_avldi);
}

static bool
loop_basic_block_p (const basic_block cfg_bb)
{
  return JUMP_P (BB_END (cfg_bb)) && any_condjump_p (BB_END (cfg_bb));
}

/* Return true if it is an RVV instruction depends on VTYPE global
   status register.  */
static bool
has_vtype_op (rtx_insn *rinsn)
{
  return recog_memoized (rinsn) >= 0 && get_attr_has_vtype_op (rinsn);
}

/* Return true if it is an RVV instruction depends on VL global
   status register.  */
static bool
has_vl_op (rtx_insn *rinsn)
{
  return recog_memoized (rinsn) >= 0 && get_attr_has_vl_op (rinsn);
}

/* Is this a SEW value that can be encoded into the VTYPE format.  */
static bool
valid_sew_p (size_t sew)
{
  return exact_log2 (sew) && sew >= 8 && sew <= 64;
}

/* Return true if it is a vsetvl instruction.  */
static bool
vector_config_insn_p (rtx_insn *rinsn)
{
  return recog_memoized (rinsn) >= 0 && get_attr_type (rinsn) == TYPE_VSETVL;
}

/* Return true if it is vsetvldi or vsetvlsi.  */
static bool
vsetvl_insn_p (rtx_insn *rinsn)
{
  return (INSN_CODE (rinsn) == CODE_FOR_vsetvldi
	 || INSN_CODE (rinsn) == CODE_FOR_vsetvlsi);
}

/* Return true if INSN1 comes befeore INSN2 in the same block.  */
static bool
same_bb_and_before_p (const insn_info *insn1, const insn_info *insn2)
{
  return ((insn1->bb ()->index () == insn2->bb ()->index ())
	 && (*insn1 < *insn2));
}

/* Return true if INSN1 comes after or equal INSN2 in the same block.  */
static bool
same_bb_and_after_or_equal_p (const insn_info *insn1, const insn_info *insn2)
{
  return ((insn1->bb ()->index () == insn2->bb ()->index ())
	 && (*insn1 >= *insn2));
}

/* An "anticipatable occurrence" is one that is the first occurrence in the
   basic block, the operands are not modified in the basic block prior
   to the occurrence and the output is not used between the start of
   the block and the occurrence.  */
static bool
anticipatable_occurrence_p (const insn_info *insn, const vector_insn_info dem)
{
  /* The only possible operand we care of VSETVL is AVL.  */
  if (dem.has_avl_reg ())
    {
      /* The operands shoule not be modified in the basic block prior
	 to the occurrence.  */
      if (!vlmax_avl_p (dem.get_avl ()))
	{
	  set_info *set
	    = find_access (insn->uses (), REGNO (dem.get_avl ()))->def ();
	  /* If it's undefined, it's not anticipatable conservatively.  */
	  if (!set)
	    return false;
	  if (same_bb_and_before_p (set->insn (), insn))
	    return false;
	}
    }

  /* The output should not be used between the start of the block
     and the occurrence.  */
  if (vsetvl_insn_p (insn->rtl ()))
    {
      rtx dest = SET_DEST (XVECEXP (PATTERN (insn->rtl ()), 0, 0));
      for (insn_info *i = insn->prev_nondebug_insn (); i != nullptr;
	   i = i->prev_nondebug_insn ())
	if (find_access (i->uses (), REGNO (dest)))
	  return false;
    }

  return true;
}

/* An "available occurrence" is one that is the last occurrence in the
   basic block and the operands are not modified by following statements in
   the basic block [including this insn].  */
static bool
available_occurrence_p (const insn_info *insn, const vector_insn_info dem)
{
  /* The only possible operand we care of VSETVL is AVL.  */
  if (dem.has_avl_reg ())
    {
      /* The operands shoule not be modified in the basic block prior
	 to the occurrence.
	 e.g.
	    bb:
	      vsetvl hr3, hr2, ...
	      ...
	      vadd ... (vl=hr3)
      */
      if (!vlmax_avl_p (dem.get_avl ()))
	{
	  set_info *set
	    = find_access (insn->uses (), REGNO (dem.get_avl ()))->def ();
	  /* If it's undefined, it's not available conservatively.  */
	  if (!set)
	    return false;
	  if (same_bb_and_after_or_equal_p (set->insn (), insn))
	    return false;
	}
    }
  return true;
}

/* Return true if the branch probability is dominate.  */
static bool
dominate_probability_p (edge e)
{
  /* TODO: We simpily pick dominate probability >= 50%.
     However, it isn't always optimal. Consider cases
     like this:
       bb 0: 80% succs: bb 2, bb 3, bb 4, bb 5.
       bb 1: 20%

     Assume bb 1, bb 2, bb 3, bb 4, bb 5 are different
     one another, and bb 2, bb 3, bb 4, bb 5 are incompatible.

     ??? Maybe backward propagate bb 1 is better ???
     May need to support an accurate and reliable COST model
     in the future.  */
  return e->probability >= profile_probability::even ();
}

/* Return true if the block is worthwhile backward propagation.  */
static bool
backward_propagate_worthwhile_p (const basic_block cfg_bb,
				 const vector_block_info block_info)
{
  if (loop_basic_block_p (cfg_bb))
    {
      if (block_info.local_dem.compatible_p (block_info.reaching_out))
	return true;

      /* There is a obvious case that is not worthwhile and meaningless
	 to propagate the demand information:
			  local_dem
			     __________
			 ____|____     |
			|        |     |
			|________|     |
			     |_________|
			  reaching_out
	  Header is incompatible with reaching_out and the block is loop itself,
	  we don't backward propagete the local_dem since we can't avoid emit
	  vsetvl for the local_dem.  */
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, cfg_bb->succs)
	if (e->dest->index == cfg_bb->index)
	  return false;
    }

  return true;
}

/* Helper function to get VL operand.  */
static rtx
get_vl (rtx_insn *rinsn)
{
  if (has_vl_op (rinsn))
    {
      /* We only call get_vl for VLMAX use VTYPE instruction.
	 It's used to get the VL operand to emit VLMAX VSETVL instruction:
	 vsetvl a5,zero,e32,m1,ta,ma.  */
      gcc_assert (get_attr_avl_type (rinsn) == VLMAX);
      extract_insn_cached (rinsn);
      return recog_data.operand[get_attr_vl_op_idx (rinsn)];
    }
  return SET_DEST (XVECEXP (PATTERN (rinsn), 0, 0));
}

/* Helper function to get AVL operand.  */
static rtx
get_avl (rtx_insn *rinsn)
{
  if (vsetvl_insn_p (rinsn))
    return XVECEXP (SET_SRC (XVECEXP (PATTERN (rinsn), 0, 0)), 0, 0);

  if (!has_vl_op (rinsn))
    return NULL_RTX;
  if (get_attr_avl_type (rinsn) == VLMAX)
    return RVV_VLMAX;
  extract_insn_cached (rinsn);
  return recog_data.operand[get_attr_vl_op_idx (rinsn)];
}

static bool
can_backward_propagate_p (const function_info *ssa, const basic_block cfg_bb,
			  const vector_insn_info prop)
{
  insn_info *insn = prop.get_insn ();

  /* TODO: We don't backward propagate the explict VSETVL here
     since we will change vsetvl and vsetvlmax intrinsiscs into
     no side effects which can be optimized into optimzal location
     by GCC internal PASSes. We only need to support these backward
     propagation if vsetvl instrinsics have side effects.  */
  if (vsetvl_insn_p (insn->rtl ()))
    return false;

  gcc_assert (has_vtype_op (insn->rtl ()));
  rtx reg = NULL_RTX;

  /* Case 1: Don't need VL. Just let it backward propagate.  */
  if (!has_vl_op (insn->rtl ()))
    return true;
  else
    {
      /* Case 2: CONST_INT AVL, we don't need to check def.  */
      if (prop.has_avl_imm ())
	return true;
      else
	{
	  /* Case 3: REG AVL, we need to check the distance of def to make
	     sure we won't backward propagate over the def.  */
	  gcc_assert (prop.has_avl_reg ());
	  if (vlmax_avl_p (prop.get_avl ()))
	    /* Check VL operand for vsetvl vl,zero.  */
	    reg = get_vl (insn->rtl ());
	  else
	    /* Check AVL operand for vsetvl zero,avl.  */
	    reg = get_avl (insn->rtl ());
	}
    }

  def_info *def = find_access (insn->uses (), REGNO (reg))->def ();

  /* If the definition is in the current block, we can't propagate it
     acrocss blocks.  */
  if (def->bb ()->cfg_bb ()->index == insn->bb ()->cfg_bb ()->index)
    {
      set_info *set = safe_dyn_cast<set_info *> (def);

      /* True if it is a degenerate PHI that can be backward propagated.  */
      auto valid_degenerate_phi_p = [&] () {
	if (!set)
	  return false;

	phi_info *phi = safe_dyn_cast<phi_info *> (set);
	if (!phi)
	  return false;

	basic_block iter_bb;
	set_info *ultimate_def = look_through_degenerate_phi (set);
	const basic_block ultimate_bb = ultimate_def->bb ()->cfg_bb ();
	FOR_BB_BETWEEN (iter_bb, ultimate_bb, def->bb ()->cfg_bb (), next_bb)
	  if (iter_bb->index == cfg_bb->index)
	    return true;

	return false;
      };

      if (valid_degenerate_phi_p ())
	return true;

      /* TODO: Support non-degenerate PHI backward propagation later.  */
      return false;
    }

  /* If the definition block is the current block that we iterate, we
     can backward propagate it since we will insert or change VL/VTYPE
     info at the end of the current block we iterate.  */
  if (def->bb ()->cfg_bb ()->index == cfg_bb->index)
    return true;

  /* Make sure we don't backward propagete the VL/VTYPE info over the
     definition blocks.  */
  bool visited_p = false;
  for (const bb_info *bb : ssa->reverse_bbs ())
    {
      if (bb->cfg_bb ()->index == cfg_bb->index && visited_p)
	return false;
      if (bb->cfg_bb ()->index == def->bb ()->cfg_bb ()->index)
	visited_p = true;
    }

  return true;
}

/* Helper function to get SEW operand. We always have SEW value for
   all RVV instructions that have VTYPE OP.  */
static uint8_t
get_sew (rtx_insn *rinsn)
{
  return get_attr_sew (rinsn);
}

/* Helper function to get VLMUL operand. We always have VLMUL value for
   all RVV instructions that have VTYPE OP. */
static enum vlmul_type
get_vlmul (rtx_insn *rinsn)
{
  return (enum vlmul_type) get_attr_vlmul (rinsn);
}

/* Get default tail policy.  */
static bool
get_default_ta ()
{
  /* For the instruction that doesn't require TA, we still need a default value
     to emit vsetvl. We pick up the default value according to prefer policy. */
  return (bool) (get_prefer_tail_policy () & 0x1
		 || (get_prefer_tail_policy () >> 1 & 0x1));
}

/* Get default mask policy.  */
static bool
get_default_ma ()
{
  /* For the instruction that doesn't require MA, we still need a default value
     to emit vsetvl. We pick up the default value according to prefer policy. */
  return (bool) (get_prefer_mask_policy () & 0x1
		 || (get_prefer_mask_policy () >> 1 & 0x1));
}

/* Helper function to get TA operand.  */
static bool
tail_agnostic_p (rtx_insn *rinsn)
{
  /* If it doesn't have TA, we return agnostic by default.  */
  extract_insn_cached (rinsn);
  int ta = get_attr_ta (rinsn);
  return ta == INVALID_ATTRIBUTE ? get_default_ta () : IS_AGNOSTIC (ta);
}

/* Helper function to get MA operand.  */
static bool
mask_agnostic_p (rtx_insn *rinsn)
{
  /* If it doesn't have MA, we return agnostic by default.  */
  extract_insn_cached (rinsn);
  int ma = get_attr_ma (rinsn);
  return ma == INVALID_ATTRIBUTE ? get_default_ma () : IS_AGNOSTIC (ma);
}

/* Return true if FN has a vector instruction that use VL/VTYPE.  */
static bool
has_vector_insn (function *fn)
{
  basic_block cfg_bb;
  rtx_insn *rinsn;
  FOR_ALL_BB_FN (cfg_bb, fn)
    FOR_BB_INSNS (cfg_bb, rinsn)
      if (NONDEBUG_INSN_P (rinsn) && has_vtype_op (rinsn))
	return true;
  return false;
}

/* Emit vsetvl instruction.  */
static rtx
gen_vsetvl_pat (enum vsetvl_type insn_type, vl_vtype_info info, rtx vl)
{
  rtx avl = info.get_avl ();
  rtx sew = gen_int_mode (info.get_sew (), Pmode);
  rtx vlmul = gen_int_mode (info.get_vlmul (), Pmode);
  rtx ta = gen_int_mode (info.get_ta (), Pmode);
  rtx ma = gen_int_mode (info.get_ma (), Pmode);

  if (insn_type == VSETVL_NORMAL)
    {
      gcc_assert (vl != NULL_RTX);
      return gen_vsetvl (Pmode, vl, avl, sew, vlmul, ta, ma);
    }
  else if (insn_type == VSETVL_VTYPE_CHANGE_ONLY)
    return gen_vsetvl_vtype_change_only (sew, vlmul, ta, ma);
  else
    return gen_vsetvl_discard_result (Pmode, avl, sew, vlmul, ta, ma);
}

static rtx
gen_vsetvl_pat (rtx_insn *rinsn, const vector_insn_info info)
{
  rtx new_pat;
  if (vsetvl_insn_p (rinsn) || vlmax_avl_p (info.get_avl ()))
    {
      rtx dest = get_vl (rinsn);
      new_pat = gen_vsetvl_pat (VSETVL_NORMAL, info, dest);
    }
  else if (INSN_CODE (rinsn) == CODE_FOR_vsetvl_vtype_change_only)
    new_pat = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY, info, NULL_RTX);
  else
    new_pat = gen_vsetvl_pat (VSETVL_DISCARD_RESULT, info, NULL_RTX);
  return new_pat;
}

static void
emit_vsetvl_insn (enum vsetvl_type insn_type, enum emit_type emit_type,
		  vl_vtype_info info, rtx vl, rtx_insn *rinsn)
{
  rtx pat = gen_vsetvl_pat (insn_type, info, vl);
  if (dump_file)
    {
      fprintf (dump_file, "\nInsert vsetvl insn PATTERN:\n");
      print_rtl_single (dump_file, pat);
    }

  if (emit_type == EMIT_DIRECT)
    emit_insn (pat);
  else if (emit_type == EMIT_BEFORE)
    emit_insn_before (pat, rinsn);
  else
    emit_insn_after (pat, rinsn);
}

static void
eliminate_insn (rtx_insn *rinsn)
{
  if (dump_file)
    {
      fprintf (dump_file, "\nEliminate insn %d:\n", INSN_UID (rinsn));
      print_rtl_single (dump_file, rinsn);
    }
  if (in_sequence_p ())
    remove_insn (rinsn);
  else
    delete_insn (rinsn);
}

static void
insert_vsetvl (enum emit_type emit_type, rtx_insn *rinsn,
	       const vector_insn_info &info, const vector_insn_info &prev_info)
{
  /* Use X0, X0 form if the AVL is the same and the SEW+LMUL gives the same
     VLMAX.  */
  if (prev_info.valid_or_dirty_p () && !prev_info.unknown_p ()
      && info.same_avl_p (prev_info) && info.same_vlmax_p (prev_info))
    {
      emit_vsetvl_insn (VSETVL_VTYPE_CHANGE_ONLY, emit_type, info, NULL_RTX,
			rinsn);
      return;
    }

  if (info.has_avl_imm ())
    {
      emit_vsetvl_insn (VSETVL_DISCARD_RESULT, emit_type, info, NULL_RTX,
			rinsn);
      return;
    }

  if (info.has_avl_no_reg ())
    {
      /* We can only use x0, x0 if there's no chance of the vtype change causing
	 the previous vl to become invalid.  */
      if (prev_info.valid_or_dirty_p () && !prev_info.unknown_p ()
	  && info.same_vlmax_p (prev_info))
	{
	  emit_vsetvl_insn (VSETVL_VTYPE_CHANGE_ONLY, emit_type, info, NULL_RTX,
			    rinsn);
	  return;
	}
      /* Otherwise use an AVL of 0 to avoid depending on previous vl.  */
      vl_vtype_info new_info = info;
      new_info.set_avl_info (avl_info (const0_rtx, nullptr));
      emit_vsetvl_insn (VSETVL_DISCARD_RESULT, emit_type, new_info, NULL_RTX,
			rinsn);
      return;
    }

  /* Use X0 as the DestReg unless AVLReg is X0. We also need to change the
     opcode if the AVLReg is X0 as they have different register classes for
     the AVL operand.  */
  if (vlmax_avl_p (info.get_avl ()))
    {
      gcc_assert (has_vtype_op (rinsn) || vsetvl_insn_p (rinsn));
      rtx vl_op = get_vl (rinsn);
      gcc_assert (!vlmax_avl_p (vl_op));
      emit_vsetvl_insn (VSETVL_NORMAL, emit_type, info, vl_op, rinsn);
      return;
    }

  emit_vsetvl_insn (VSETVL_DISCARD_RESULT, emit_type, info, NULL_RTX, rinsn);

  if (dump_file)
    {
      fprintf (dump_file, "Update VL/VTYPE info, previous info=");
      prev_info.dump (dump_file);
    }
}

/* If X contains any LABEL_REF's, add REG_LABEL_OPERAND notes for them
   to INSN.  If such notes are added to an insn which references a
   CODE_LABEL, the LABEL_NUSES count is incremented.  We have to add
   that note, because the following loop optimization pass requires
   them.  */

/* ??? If there was a jump optimization pass after gcse and before loop,
   then we would not need to do this here, because jump would add the
   necessary REG_LABEL_OPERAND and REG_LABEL_TARGET notes.  */

static void
add_label_notes (rtx x, rtx_insn *insn)
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  if (code == LABEL_REF && !LABEL_REF_NONLOCAL_P (x))
    {
      /* This code used to ignore labels that referred to dispatch tables to
	 avoid flow generating (slightly) worse code.

	 We no longer ignore such label references (see LABEL_REF handling in
	 mark_jump_label for additional information).  */

      /* There's no reason for current users to emit jump-insns with
	 such a LABEL_REF, so we don't have to handle REG_LABEL_TARGET
	 notes.  */
      gcc_assert (!JUMP_P (insn));
      add_reg_note (insn, REG_LABEL_OPERAND, label_ref_label (x));

      if (LABEL_P (label_ref_label (x)))
	LABEL_NUSES (label_ref_label (x))++;

      return;
    }

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	add_label_notes (XEXP (x, i), insn);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  add_label_notes (XVECEXP (x, i, j), insn);
    }
}

/* Add EXPR to the end of basic block BB.

   This is used by both the PRE and code hoisting.  */

static void
insert_insn_end_basic_block (rtx_insn *rinsn, basic_block cfg_bb)
{
  rtx_insn *end_rinsn = BB_END (cfg_bb);
  rtx_insn *new_insn;
  rtx_insn *pat, *pat_end;

  pat = rinsn;
  gcc_assert (pat && INSN_P (pat));

  pat_end = pat;
  while (NEXT_INSN (pat_end) != NULL_RTX)
    pat_end = NEXT_INSN (pat_end);

  /* If the last end_rinsn is a jump, insert EXPR in front.  Similarly we need
     to take care of trapping instructions in presence of non-call exceptions.
   */

  if (JUMP_P (end_rinsn)
      || (NONJUMP_INSN_P (end_rinsn)
	  && (!single_succ_p (cfg_bb)
	      || single_succ_edge (cfg_bb)->flags & EDGE_ABNORMAL)))
    {
      /* FIXME: What if something in jump uses value set in new end_rinsn?  */
      new_insn = emit_insn_before_noloc (pat, end_rinsn, cfg_bb);
    }

  /* Likewise if the last end_rinsn is a call, as will happen in the presence
     of exception handling.  */
  else if (CALL_P (end_rinsn)
	   && (!single_succ_p (cfg_bb)
	       || single_succ_edge (cfg_bb)->flags & EDGE_ABNORMAL))
    {
      /* Keeping in mind targets with small register classes and parameters
	 in registers, we search backward and place the instructions before
	 the first parameter is loaded.  Do this for everyone for consistency
	 and a presumption that we'll get better code elsewhere as well.  */

      /* Since different machines initialize their parameter registers
	 in different orders, assume nothing.  Collect the set of all
	 parameter registers.  */
      end_rinsn = find_first_parameter_load (end_rinsn, BB_HEAD (cfg_bb));

      /* If we found all the parameter loads, then we want to insert
	 before the first parameter load.

	 If we did not find all the parameter loads, then we might have
	 stopped on the head of the block, which could be a CODE_LABEL.
	 If we inserted before the CODE_LABEL, then we would be putting
	 the end_rinsn in the wrong basic block.  In that case, put the
	 end_rinsn after the CODE_LABEL.  Also, respect NOTE_INSN_BASIC_BLOCK.
       */
      while (LABEL_P (end_rinsn) || NOTE_INSN_BASIC_BLOCK_P (end_rinsn))
	end_rinsn = NEXT_INSN (end_rinsn);

      new_insn = emit_insn_before_noloc (pat, end_rinsn, cfg_bb);
    }
  else
    new_insn = emit_insn_after_noloc (pat, end_rinsn, cfg_bb);

  while (1)
    {
      if (INSN_P (pat))
	add_label_notes (PATTERN (pat), new_insn);
      if (pat == pat_end)
	break;
      pat = NEXT_INSN (pat);
    }
}

/* Get VL/VTYPE information for INSN.  */
static vl_vtype_info
get_vl_vtype_info (const insn_info *insn)
{
  if (vector_config_insn_p (insn->rtl ()))
    gcc_assert (vsetvl_insn_p (insn->rtl ())
		&& "Can't handle X0, rs1 vsetvli yet");

  set_info *set = nullptr;
  rtx avl = ::get_avl (insn->rtl ());
  if (avl && REG_P (avl) && !vlmax_avl_p (avl))
    set = find_access (insn->uses (), REGNO (avl))->def ();

  uint8_t sew = get_sew (insn->rtl ());
  enum vlmul_type vlmul = get_vlmul (insn->rtl ());
  uint8_t ratio = get_attr_ratio (insn->rtl ());
  /* when get_attr_ratio is invalid, this kind of instructions
     doesn't care about ratio. However, we still need this value
     in demand info backward analysis.  */
  if (ratio == INVALID_ATTRIBUTE)
    ratio = calculate_ratio (sew, vlmul);
  bool ta = tail_agnostic_p (insn->rtl ());
  bool ma = mask_agnostic_p (insn->rtl ());

  /* If merge operand is undef value, we prefer agnostic.  */
  int merge_op_idx = get_attr_merge_op_idx (insn->rtl ());
  if (merge_op_idx != INVALID_ATTRIBUTE
      && satisfies_constraint_vu (recog_data.operand[merge_op_idx]))
    {
      ta = true;
      ma = true;
    }

  vl_vtype_info info (avl_info (avl, set), sew, vlmul, ratio, ta, ma);
  return info;
}

static void
change_insn (rtx_insn *rinsn, rtx new_pat)
{
  /* We don't apply change on RTL_SSA here since it's possible a
     new INSN we add in the PASS before which doesn't have RTL_SSA
     info yet.*/
  if (dump_file)
    {
      fprintf (dump_file, "\nChange PATTERN of insn %d from:\n",
	       INSN_UID (rinsn));
      print_rtl_single (dump_file, PATTERN (rinsn));
    }

  validate_change (rinsn, &PATTERN (rinsn), new_pat, true);

  if (dump_file)
    {
      fprintf (dump_file, "\nto:\n");
      print_rtl_single (dump_file, PATTERN (rinsn));
    }
}

static bool
change_insn (function_info *ssa, insn_change change, insn_info *insn,
	     rtx new_pat)
{
  rtx_insn *rinsn = insn->rtl ();
  auto attempt = ssa->new_change_attempt ();
  if (!restrict_movement (change))
    return false;

  if (dump_file)
    {
      fprintf (dump_file, "\nChange PATTERN of insn %d from:\n",
	       INSN_UID (rinsn));
      print_rtl_single (dump_file, PATTERN (rinsn));
      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "RTL_SSA info:\n");
	  pretty_printer pp;
	  pp.buffer->stream = dump_file;
	  insn->print_full (&pp);
	  pp_printf (&pp, "\n");
	  pp_flush (&pp);
	}
    }

  insn_change_watermark watermark;
  validate_change (rinsn, &PATTERN (rinsn), new_pat, true);

  /* These routines report failures themselves.  */
  if (!recog (attempt, change) || !change_is_worthwhile (change, false))
    return false;
  confirm_change_group ();
  ssa->change_insn (change);

  if (dump_file)
    {
      fprintf (dump_file, "\nto:\n");
      print_rtl_single (dump_file, PATTERN (rinsn));
      if (dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "RTL_SSA info:\n");
	  pretty_printer pp;
	  pp.buffer->stream = dump_file;
	  insn->print_full (&pp);
	  pp_printf (&pp, "\n");
	  pp_flush (&pp);
	}
    }
  return true;
}

avl_info::avl_info (rtx value_in, set_info *source_in)
  : m_value (value_in), m_source (source_in)
{}

avl_info &
avl_info::operator= (const avl_info &other)
{
  m_value = other.get_value ();
  m_source = other.get_source ();
  return *this;
}

bool
avl_info::operator== (const avl_info &other) const
{
  if (!m_value)
    return !other.get_value ();
  if (!other.get_value ())
    return false;

  /* It's safe to consider they are equal if their RTX value are
     strictly the same.  */
  if (m_value == other.get_value ())
    return true;

  if (GET_CODE (m_value) != GET_CODE (other.get_value ()))
    return false;

  /* Handle CONST_INT AVL.  */
  if (CONST_INT_P (m_value))
    return INTVAL (m_value) == INTVAL (other.get_value ());

  /* Handle VLMAX AVL.  */
  if (vlmax_avl_p (m_value))
    return vlmax_avl_p (other.get_value ());

  /* TODO: So far we only support VLMAX (AVL=zero) comparison,
     we will support non-VLMAX AVL in the future.  */
  return false;
}

bool
avl_info::operator!= (const avl_info &other) const
{
  return !(*this == other);
}

/* Initialize VL/VTYPE information.  */
vl_vtype_info::vl_vtype_info (avl_info avl_in, uint8_t sew_in,
			      enum vlmul_type vlmul_in, uint8_t ratio_in,
			      bool ta_in, bool ma_in)
  : m_avl (avl_in), m_sew (sew_in), m_vlmul (vlmul_in), m_ratio (ratio_in),
    m_ta (ta_in), m_ma (ma_in)
{
  gcc_assert (valid_sew_p (m_sew) && "Unexpected SEW");
}

bool
vl_vtype_info::operator== (const vl_vtype_info &other) const
{
  return m_avl == other.get_avl_info () && m_sew == other.get_sew ()
	 && m_vlmul == other.get_vlmul () && m_ta == other.get_ta ()
	 && m_ma == other.get_ma () && m_ratio == other.get_ratio ();
}

bool
vl_vtype_info::operator!= (const vl_vtype_info &other) const
{
  return !(*this == other);
}

bool
vl_vtype_info::has_non_zero_avl () const
{
  if (has_avl_imm ())
    return INTVAL (get_avl ()) > 0;
  if (has_avl_reg ())
    return vlmax_avl_p (get_avl ());
  return false;
}

bool
vl_vtype_info::same_avl_p (const vl_vtype_info &other) const
{
  return get_avl_info () == other.get_avl_info ();
}

bool
vl_vtype_info::same_vtype_p (const vl_vtype_info &other) const
{
  return get_sew () == other.get_sew () && get_vlmul () == other.get_vlmul ()
	 && get_ta () == other.get_ta () && get_ma () == other.get_ma ();
}

bool
vl_vtype_info::same_vlmax_p (const vl_vtype_info &other) const
{
  return get_ratio () == other.get_ratio ();
}

/* Compare the compatibility between Dem1 and Dem2.
   If Dem1 > Dem2, Dem1 has bigger compatibility then Dem2
   meaning Dem1 is easier be compatible with others than Dem2
   or Dem2 is stricter than Dem1.
   For example, Dem1 (demand SEW + LMUL) > Dem2 (demand RATIO).  */
bool
vector_insn_info::operator> (const vector_insn_info &other) const
{
  if (other.compatible_p (static_cast<const vl_vtype_info &> (*this))
      && !this->compatible_p (static_cast<const vl_vtype_info &> (other)))
    return true;
  return false;
}

bool
vector_insn_info::operator>= (const vector_insn_info &other) const
{
  if (*this > other)
    return true;

  if (*this == other)
    return true;

  if (!compatible_p (other))
    return false;

  if (!demand_p (DEMAND_AVL) && other.demand_p (DEMAND_AVL))
    return false;

  if (same_vlmax_p (other))
    {
      if (demand_p (DEMAND_RATIO) && !other.demand_p (DEMAND_RATIO)
	  && (get_sew () != other.get_sew ()
	      || get_vlmul () != other.get_vlmul ()))
	return false;

      if (get_sew () == other.get_sew () && get_vlmul () == other.get_vlmul ())
	{
	  if (demand_p (DEMAND_RATIO) && !other.demand_p (DEMAND_RATIO))
	    return false;
	}
    }

  if (demand_p (DEMAND_TAIL_POLICY) && !other.demand_p (DEMAND_TAIL_POLICY)
      && get_ta () != other.get_ta ())
    return false;

  if (demand_p (DEMAND_MASK_POLICY) && !other.demand_p (DEMAND_MASK_POLICY)
      && get_ma () != other.get_ma ())
    return false;

  return true;
}

bool
vector_insn_info::operator== (const vector_insn_info &other) const
{
  gcc_assert (!uninit_p () && !other.uninit_p ()
	      && "Uninitialization should not happen");

  /* Empty is only equal to another Empty.  */
  if (empty_p ())
    return other.empty_p ();
  if (other.empty_p ())
    return empty_p ();

  /* Unknown is only equal to another Unknown.  */
  if (unknown_p ())
    return other.unknown_p ();
  if (other.unknown_p ())
    return unknown_p ();

  for (size_t i = 0; i < NUM_DEMAND; i++)
    if (m_demands[i] != other.demand_p ((enum demand_type) i))
      return false;

  if (m_insn != other.get_insn ())
    return false;
  if (m_dirty_pat != other.get_dirty_pat ())
    return false;

  if (!same_avl_p (other))
    return false;

  /* If the full VTYPE is valid, check that it is the same.  */
  return same_vtype_p (other);
}

void
vector_insn_info::parse_insn (rtx_insn *rinsn)
{
  *this = vector_insn_info ();
  if (!NONDEBUG_INSN_P (rinsn))
    return;
  if (!has_vtype_op (rinsn))
    return;
  m_state = VALID;
  extract_insn_cached (rinsn);
  const rtx avl = recog_data.operand[get_attr_vl_op_idx (rinsn)];
  m_avl = avl_info (avl, nullptr);
  m_sew = ::get_sew (rinsn);
  m_vlmul = ::get_vlmul (rinsn);
  m_ta = tail_agnostic_p (rinsn);
  m_ma = mask_agnostic_p (rinsn);
}

void
vector_insn_info::parse_insn (insn_info *insn)
{
  *this = vector_insn_info ();

  /* Return if it is debug insn for the consistency with optimize == 0.  */
  if (insn->is_debug_insn ())
    return;

  /* We set it as unknown since we don't what will happen in CALL or ASM.  */
  if (insn->is_call () || insn->is_asm ())
    {
      set_unknown ();
      return;
    }

  /* If this is something that updates VL/VTYPE that we don't know about, set
     the state to unknown.  */
  if (!vector_config_insn_p (insn->rtl ())
      && (find_access (insn->defs (), VL_REGNUM)
	  || find_access (insn->defs (), VTYPE_REGNUM)))
    {
      set_unknown ();
      return;
    }

  if (!vector_config_insn_p (insn->rtl ()) && !has_vtype_op (insn->rtl ()))
    return;

  /* Warning: This function has to work on both the lowered (i.e. post
     emit_local_forward_vsetvls) and pre-lowering forms.  The main implication
     of this is that it can't use the value of a SEW, VL, or Policy operand as
     they might be stale after lowering.  */
  vl_vtype_info::operator= (get_vl_vtype_info (insn));
  m_insn = insn;
  m_state = VALID;
  if (vector_config_insn_p (insn->rtl ()))
    {
      m_demands[DEMAND_AVL] = true;
      m_demands[DEMAND_RATIO] = true;
      return;
    }

  if (has_vl_op (insn->rtl ()))
    m_demands[DEMAND_AVL] = true;

  if (get_attr_ratio (insn->rtl ()) != INVALID_ATTRIBUTE)
    m_demands[DEMAND_RATIO] = true;
  else
    {
      /* TODO: By default, if it doesn't demand RATIO, we set it
	 demand SEW && LMUL both. Some instructions may demand SEW
	 only and ignore LMUL, will fix it later.  */
      m_demands[DEMAND_SEW] = true;
      m_demands[DEMAND_LMUL] = true;
    }

  if (get_attr_ta (insn->rtl ()) != INVALID_ATTRIBUTE)
    m_demands[DEMAND_TAIL_POLICY] = true;
  if (get_attr_ma (insn->rtl ()) != INVALID_ATTRIBUTE)
    m_demands[DEMAND_MASK_POLICY] = true;
}

void
vector_insn_info::demand_vl_vtype ()
{
  m_state = VALID;
  m_demands[DEMAND_AVL] = true;
  m_demands[DEMAND_SEW] = true;
  m_demands[DEMAND_LMUL] = true;
  m_demands[DEMAND_TAIL_POLICY] = true;
  m_demands[DEMAND_MASK_POLICY] = true;
}

bool
vector_insn_info::compatible_p (const vector_insn_info &other) const
{
  gcc_assert (valid_or_dirty_p () && other.valid_or_dirty_p ()
	      && "Can't compare invalid demanded infos");

  /* Check SEW.  */
  if (demand_p (DEMAND_SEW) && other.demand_p (DEMAND_SEW)
      && get_sew () != other.get_sew ())
    return false;

  /* Check LMUL.  */
  if (demand_p (DEMAND_LMUL) && other.demand_p (DEMAND_LMUL)
      && get_vlmul () != other.get_vlmul ())
    return false;

  /* Check RATIO.  */
  if (demand_p (DEMAND_RATIO) && other.demand_p (DEMAND_RATIO)
      && get_ratio () != other.get_ratio ())
    return false;
  if (demand_p (DEMAND_RATIO) && (other.get_sew () || other.get_vlmul ())
      && get_ratio () != other.get_ratio ())
    return false;
  if (other.demand_p (DEMAND_RATIO) && (get_sew () || get_vlmul ())
      && get_ratio () != other.get_ratio ())
    return false;

  if (demand_p (DEMAND_TAIL_POLICY) && other.demand_p (DEMAND_TAIL_POLICY)
      && get_ta () != other.get_ta ())
    return false;
  if (demand_p (DEMAND_MASK_POLICY) && other.demand_p (DEMAND_MASK_POLICY)
      && get_ma () != other.get_ma ())
    return false;

  if (demand_p (DEMAND_AVL) && other.demand_p (DEMAND_AVL))
    return m_avl == other.get_avl_info ();

  return true;
}

bool
vector_insn_info::compatible_avl_p (const vl_vtype_info &other) const
{
  gcc_assert (valid_or_dirty_p () && "Can't compare invalid vl_vtype_info");
  gcc_assert (!unknown_p () && "Can't compare AVL in unknown state");
  if (!demand_p (DEMAND_AVL))
    return true;
  return get_avl_info () == other.get_avl_info ();
}

bool
vector_insn_info::compatible_vtype_p (const vl_vtype_info &other) const
{
  gcc_assert (valid_or_dirty_p () && "Can't compare invalid vl_vtype_info");
  gcc_assert (!unknown_p () && "Can't compare VTYPE in unknown state");
  if (demand_p (DEMAND_SEW) && m_sew != other.get_sew ())
    return false;
  if (demand_p (DEMAND_LMUL) && m_vlmul != other.get_vlmul ())
    return false;
  if (demand_p (DEMAND_RATIO) && m_ratio != other.get_ratio ())
    return false;
  if (demand_p (DEMAND_TAIL_POLICY) && m_ta != other.get_ta ())
    return false;
  if (demand_p (DEMAND_MASK_POLICY) && m_ma != other.get_ma ())
    return false;
  return true;
}

/* Determine whether the vector instructions requirements represented by
   Require are compatible with the previous vsetvli instruction represented
   by this.  INSN is the instruction whose requirements we're considering.  */
bool
vector_insn_info::compatible_p (const vl_vtype_info &curr_info) const
{
  gcc_assert (!uninit_p () && "Can't handle uninitialized info");
  if (empty_p ())
    return false;

  /* Nothing is compatible with Unknown.  */
  if (unknown_p ())
    return false;

  /* If the instruction doesn't need an AVLReg and the SEW matches, consider
     it compatible.  */
  if (!demand_p (DEMAND_AVL))
    if (m_sew == curr_info.get_sew ())
      return true;

  return compatible_avl_p (curr_info) && compatible_vtype_p (curr_info);
}

vector_insn_info
vector_insn_info::merge (const vector_insn_info &merge_info,
			 bool across_bb_p = false) const
{
  gcc_assert (this->compatible_p (merge_info)
	      && "Can't merge incompatible demanded infos");

  vector_insn_info new_info;
  new_info.demand_vl_vtype ();

  if (dirty_p ())
    {
      gcc_assert (across_bb_p);
      if (demand_p (DEMAND_AVL))
	new_info.set_insn (get_insn ());
      else
	new_info.set_insn (merge_info.get_insn ());
    }
  else
    {
      if (across_bb_p)
	new_info.set_insn (get_insn ());
      else
	new_info.set_insn (merge_info.get_insn ());
    }

  new_info.set_dirty_pat (merge_info.get_dirty_pat ());

  if (!demand_p (DEMAND_AVL) && !merge_info.demand_p (DEMAND_AVL))
    new_info.undemand (DEMAND_AVL);
  if (!demand_p (DEMAND_SEW) && !merge_info.demand_p (DEMAND_SEW))
    new_info.undemand (DEMAND_SEW);
  if (!demand_p (DEMAND_LMUL) && !merge_info.demand_p (DEMAND_LMUL))
    new_info.undemand (DEMAND_LMUL);

  if (!demand_p (DEMAND_TAIL_POLICY)
      && !merge_info.demand_p (DEMAND_TAIL_POLICY))
    new_info.undemand (DEMAND_TAIL_POLICY);
  if (!demand_p (DEMAND_MASK_POLICY)
      && !merge_info.demand_p (DEMAND_MASK_POLICY))
    new_info.undemand (DEMAND_MASK_POLICY);

  if (merge_info.demand_p (DEMAND_AVL))
    new_info.set_avl_info (merge_info.get_avl_info ());
  else if (demand_p (DEMAND_AVL))
    new_info.set_avl_info (get_avl_info ());

  if (merge_info.demand_p (DEMAND_SEW))
    new_info.set_sew (merge_info.get_sew ());
  else if (demand_p (DEMAND_SEW))
    new_info.set_sew (get_sew ());

  if (merge_info.demand_p (DEMAND_LMUL))
    new_info.set_vlmul (merge_info.get_vlmul ());
  else if (demand_p (DEMAND_LMUL))
    new_info.set_vlmul (get_vlmul ());

  if (!new_info.demand_p (DEMAND_SEW) && !new_info.demand_p (DEMAND_LMUL))
    {
      if (demand_p (DEMAND_RATIO) || merge_info.demand_p (DEMAND_RATIO))
	new_info.demand (DEMAND_RATIO);
      /* Even though we don't demand_p SEW && VLMUL in this case, we still
       * need them.  */
      if (merge_info.demand_p (DEMAND_RATIO))
	{
	  new_info.set_sew (merge_info.get_sew ());
	  new_info.set_vlmul (merge_info.get_vlmul ());
	  new_info.set_ratio (merge_info.get_ratio ());
	}
      else if (demand_p (DEMAND_RATIO))
	{
	  new_info.set_sew (get_sew ());
	  new_info.set_vlmul (get_vlmul ());
	  new_info.set_ratio (get_ratio ());
	}
    }
  else
    {
      /* when get_attr_ratio is invalid, this kind of instructions
	 doesn't care about ratio. However, we still need this value
	 in demand_p info backward analysis.  */
      new_info.set_ratio (
	calculate_ratio (new_info.get_sew (), new_info.get_vlmul ()));
    }

  if (merge_info.demand_p (DEMAND_TAIL_POLICY))
    new_info.set_ta (merge_info.get_ta ());
  else if (demand_p (DEMAND_TAIL_POLICY))
    new_info.set_ta (get_ta ());
  else
    new_info.set_ta (get_default_ta ());

  if (merge_info.demand_p (DEMAND_MASK_POLICY))
    new_info.set_ma (merge_info.get_ma ());
  else if (demand_p (DEMAND_MASK_POLICY))
    new_info.set_ma (get_ma ());
  else
    new_info.set_ma (get_default_ma ());

  return new_info;
}

void
vector_insn_info::dump (FILE *file) const
{
  fprintf (file, "[");
  if (uninit_p ())
    fprintf (file, "UNINITIALIZED,");
  else if (valid_p ())
    fprintf (file, "VALID,");
  else if (unknown_p ())
    fprintf (file, "UNKNOWN,");
  else if (empty_p ())
    fprintf (file, "EMPTY,");
  else
    fprintf (file, "DIRTY,");

  fprintf (file, "Demand field={%d(VL),", demand_p (DEMAND_AVL));
  fprintf (file, "%d(SEW),", demand_p (DEMAND_SEW));
  fprintf (file, "%d(LMUL),", demand_p (DEMAND_LMUL));
  fprintf (file, "%d(RATIO),", demand_p (DEMAND_RATIO));
  fprintf (file, "%d(TAIL_POLICY),", demand_p (DEMAND_TAIL_POLICY));
  fprintf (file, "%d(MASK_POLICY)}\n", demand_p (DEMAND_MASK_POLICY));

  fprintf (file, "AVL=");
  print_rtl_single (file, get_avl ());
  fprintf (file, "SEW=%d,", get_sew ());
  fprintf (file, "VLMUL=%d,", get_vlmul ());
  fprintf (file, "RATIO=%d,", get_ratio ());
  fprintf (file, "TAIL_POLICY=%d,", get_ta ());
  fprintf (file, "MASK_POLICY=%d", get_ma ());
  fprintf (file, "]\n");

  if (valid_p ())
    {
      if (get_insn ())
	{
	  fprintf (file, "RTL_SSA insn_info=");
	  pretty_printer pp;
	  pp.buffer->stream = file;
	  get_insn ()->print_full (&pp);
	  pp_printf (&pp, "\n");
	  pp_flush (&pp);
	}
      if (get_dirty_pat ())
	{
	  fprintf (file, "Dirty RTL Pattern=");
	  print_rtl_single (file, get_dirty_pat ());
	}
    }
}

vector_infos_manager::vector_infos_manager ()
{
  vector_edge_list = nullptr;
  vector_kill = nullptr;
  vector_del = nullptr;
  vector_insert = nullptr;
  vector_antic = nullptr;
  vector_transp = nullptr;
  vector_comp = nullptr;
  vector_avin = nullptr;
  vector_avout = nullptr;
  vector_insn_infos.safe_grow (get_max_uid ());
  vector_block_infos.safe_grow (last_basic_block_for_fn (cfun));
  if (!optimize)
    {
      basic_block cfg_bb;
      rtx_insn *rinsn;
      FOR_ALL_BB_FN (cfg_bb, cfun)
	{
	  vector_block_infos[cfg_bb->index].local_dem = vector_insn_info ();
	  vector_block_infos[cfg_bb->index].reaching_out = vector_insn_info ();
	  FOR_BB_INSNS (cfg_bb, rinsn)
	    vector_insn_infos[INSN_UID (rinsn)].parse_insn (rinsn);
	}
    }
  else
    {
      for (const bb_info *bb : crtl->ssa->bbs ())
	{
	  vector_block_infos[bb->index ()].local_dem = vector_insn_info ();
	  vector_block_infos[bb->index ()].reaching_out = vector_insn_info ();
	  for (insn_info *insn : bb->real_insns ())
	    vector_insn_infos[insn->uid ()].parse_insn (insn);
	}
    }
}

void
vector_infos_manager::create_expr (vector_insn_info &info)
{
  for (size_t i = 0; i < vector_exprs.length (); i++)
    if (*vector_exprs[i] == info)
      return;
  vector_exprs.safe_push (&info);
}

size_t
vector_infos_manager::get_expr_id (const vector_insn_info &info) const
{
  for (size_t i = 0; i < vector_exprs.length (); i++)
    if (*vector_exprs[i] == info)
      return i;
  gcc_unreachable ();
}

auto_vec<size_t>
vector_infos_manager::get_all_available_exprs (
  const vector_insn_info &info) const
{
  auto_vec<size_t> available_list;
  for (size_t i = 0; i < vector_exprs.length (); i++)
    if (info >= *vector_exprs[i])
      available_list.safe_push (i);
  return available_list;
}

bool
vector_infos_manager::all_same_ratio_p (sbitmap bitdata) const
{
  if (bitmap_empty_p (bitdata))
    return false;

  int ratio = -1;
  unsigned int bb_index;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (bitdata, 0, bb_index, sbi)
  {
    if (ratio == -1)
      ratio = vector_exprs[bb_index]->get_ratio ();
    else if (vector_exprs[bb_index]->get_ratio () != ratio)
      return false;
  }
  return true;
}

size_t
vector_infos_manager::expr_set_num (sbitmap bitdata) const
{
  size_t count = 0;
  for (size_t i = 0; i < vector_exprs.length (); i++)
    if (bitmap_bit_p (bitdata, i))
      count++;
  return count;
}

void
vector_infos_manager::release (void)
{
  if (!vector_insn_infos.is_empty ())
    vector_insn_infos.release ();
  if (!vector_block_infos.is_empty ())
    vector_block_infos.release ();
  if (!vector_exprs.is_empty ())
    vector_exprs.release ();

  if (optimize > 0)
    {
      /* Finished. Free up all the things we've allocated.  */
      free_edge_list (vector_edge_list);
      sbitmap_vector_free (vector_del);
      sbitmap_vector_free (vector_insert);
      sbitmap_vector_free (vector_kill);
      sbitmap_vector_free (vector_antic);
      sbitmap_vector_free (vector_transp);
      sbitmap_vector_free (vector_comp);
      sbitmap_vector_free (vector_avin);
      sbitmap_vector_free (vector_avout);
    }
}

void
vector_infos_manager::dump (FILE *file) const
{
  basic_block cfg_bb;
  rtx_insn *rinsn;

  fprintf (file, "\n");
  FOR_ALL_BB_FN (cfg_bb, cfun)
    {
      fprintf (file, "Local vector info of <bb %d>:\n", cfg_bb->index);
      fprintf (file, "<HEADER>=");
      vector_block_infos[cfg_bb->index].local_dem.dump (file);
      FOR_BB_INSNS (cfg_bb, rinsn)
	{
	  if (!NONDEBUG_INSN_P (rinsn) || !has_vtype_op (rinsn))
	    continue;
	  fprintf (file, "<insn %d>=", INSN_UID (rinsn));
	  const auto &info = vector_insn_infos[INSN_UID (rinsn)];
	  info.dump (file);
	}
      fprintf (file, "<FOOTER>=");
      vector_block_infos[cfg_bb->index].reaching_out.dump (file);
      fprintf (file, "\n\n");
    }

  fprintf (file, "\n");
  FOR_ALL_BB_FN (cfg_bb, cfun)
    {
      fprintf (file, "Local properties of <bb %d>:\n", cfg_bb->index);

      fprintf (file, "<ANTLOC>=");
      if (vector_antic == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_antic[cfg_bb->index]);

      fprintf (file, "<AVLOC>=");
      if (vector_comp == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_comp[cfg_bb->index]);

      fprintf (file, "<TRANSP>=");
      if (vector_transp == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_transp[cfg_bb->index]);

      fprintf (file, "<KILL>=");
      if (vector_kill == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_kill[cfg_bb->index]);
    }

  fprintf (file, "\n");
  FOR_ALL_BB_FN (cfg_bb, cfun)
    {
      fprintf (file, "Global LCM (Lazy code motion) result of <bb %d>:\n",
	       cfg_bb->index);

      fprintf (file, "<AVIN>=");
      if (vector_avin == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_avin[cfg_bb->index]);

      fprintf (file, "<AVOUT>=");
      if (vector_avout == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_avout[cfg_bb->index]);

      fprintf (file, "<DELETE>=");
      if (vector_del == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_del[cfg_bb->index]);
    }

  fprintf (file, "\nGlobal LCM (Lazy code motion) INSERT info:\n");
  for (size_t i = 0; i < vector_exprs.length (); i++)
    {
      for (int ed = 0; ed < NUM_EDGES (vector_edge_list); ed++)
	{
	  edge eg = INDEX_EDGE (vector_edge_list, ed);
	  if (bitmap_bit_p (vector_insert[ed], i))
	    fprintf (dump_file,
		     "INSERT edge %d from bb %d to bb %d for VSETVL "
		     "expr[%ld]\n",
		     ed, eg->src->index, eg->dest->index, i);
	}
    }
}

const pass_data pass_data_vsetvl = {
  RTL_PASS,	 /* type */
  "vsetvl",	 /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE,	 /* tv_id */
  0,		 /* properties_required */
  0,		 /* properties_provided */
  0,		 /* properties_destroyed */
  0,		 /* todo_flags_start */
  0,		 /* todo_flags_finish */
};

class pass_vsetvl : public rtl_opt_pass
{
private:
  class vector_infos_manager *m_vector_manager;

  void simple_vsetvl (void) const;
  void lazy_vsetvl (void);

  /* Phase 1.  */
  void compute_local_backward_infos (const bb_info *);

  /* Phase 2.  */
  bool need_vsetvl (const vector_insn_info &, const vector_insn_info &) const;
  void transfer_before (vector_insn_info &, insn_info *) const;
  void transfer_after (vector_insn_info &, insn_info *) const;
  void emit_local_forward_vsetvls (const bb_info *);

  /* Phase 3.  */
  void merge_successors (const basic_block, const basic_block);
  void compute_global_backward_infos (void);

  /* Phase 4.  */
  void prune_expressions (void);
  void compute_local_properties (void);
  bool can_refine_vsetvl_p (const basic_block, uint8_t) const;
  void refine_vsetvls (void) const;
  void cleanup_vsetvls (void);
  bool commit_vsetvls (void);
  void pre_vsetvl (void);

  /* Phase 5.  */
  void cleanup_insns (void) const;

  void init (void);
  void done (void);

public:
  pass_vsetvl (gcc::context *ctxt) : rtl_opt_pass (pass_data_vsetvl, ctxt) {}

  /* opt_pass methods: */
  virtual bool gate (function *) final override { return TARGET_VECTOR; }
  virtual unsigned int execute (function *) final override;
}; // class pass_vsetvl

/* Simple m_vsetvl_insert vsetvl for optimize == 0.  */
void
pass_vsetvl::simple_vsetvl (void) const
{
  if (dump_file)
    fprintf (dump_file,
	     "\nEntering Simple VSETVL PASS and Handling %d basic blocks for "
	     "function:%s\n",
	     n_basic_blocks_for_fn (cfun), function_name (cfun));

  basic_block cfg_bb;
  rtx_insn *rinsn;
  FOR_ALL_BB_FN (cfg_bb, cfun)
    {
      FOR_BB_INSNS (cfg_bb, rinsn)
	{
	  if (!NONDEBUG_INSN_P (rinsn))
	    continue;
	  if (has_vtype_op (rinsn))
	    {
	      const auto info
		= m_vector_manager->vector_insn_infos[INSN_UID (rinsn)];
	      emit_vsetvl_insn (VSETVL_DISCARD_RESULT, EMIT_BEFORE, info,
				NULL_RTX, rinsn);
	    }
	}
    }
}

/* Compute demanded information by backward data-flow analysis.  */
void
pass_vsetvl::compute_local_backward_infos (const bb_info *bb)
{
  vector_insn_info change;
  change.set_empty ();

  auto &block_info = m_vector_manager->vector_block_infos[bb->index ()];
  block_info.reaching_out = change;

  for (insn_info *insn : bb->reverse_real_nondebug_insns ())
    {
      auto &info = m_vector_manager->vector_insn_infos[insn->uid ()];

      if (info.uninit_p ())
	/* If it is uninitialized, propagate it directly.  */
	info = change;
      else if (info.unknown_p ())
	change = info;
      else
	{
	  gcc_assert (info.valid_p () && "Unexpected Invalid demanded info");
	  if (change.valid_p () && change.compatible_p (info))
	    info = change.merge (info);
	  change = info;
	}
    }

  block_info.local_dem = change;
  if (block_info.local_dem.empty_p ())
    block_info.reaching_out = block_info.local_dem;
}

/* Return true if a dem_info is required to transition from curr_info to
   require before INSN.  */
bool
pass_vsetvl::need_vsetvl (const vector_insn_info &require,
			  const vector_insn_info &curr_info) const
{
  if (!curr_info.valid_p () || curr_info.unknown_p () || curr_info.uninit_p ())
    return true;

  if (require.compatible_p (curr_info))
    return false;

  return true;
}

/* Given an incoming state reaching INSN, modifies that state so that it is
   minimally compatible with INSN.  The resulting state is guaranteed to be
   semantically legal for INSN, but may not be the state requested by INSN.  */
void
pass_vsetvl::transfer_before (vector_insn_info &info, insn_info *insn) const
{
  if (!has_vtype_op (insn->rtl ()))
    return;

  const vector_insn_info require
    = m_vector_manager->vector_insn_infos[insn->uid ()];
  if (info.valid_p () && !need_vsetvl (require, info))
    return;
  info = require;
}

/* Given a state with which we evaluated insn (see transfer_before above for why
   this might be different that the state insn requested), modify the state to
   reflect the changes insn might make.  */
void
pass_vsetvl::transfer_after (vector_insn_info &info, insn_info *insn) const
{
  if (vector_config_insn_p (insn->rtl ()))
    {
      info = m_vector_manager->vector_insn_infos[insn->uid ()];
      return;
    }

  /* TODO: Support fault first load info update VL in the future.  */

  /* If this is something that updates VL/VTYPE that we don't know about, set
     the state to unknown.  */
  if (insn->is_call () || insn->is_asm ()
      || find_access (insn->defs (), VL_REGNUM)
      || find_access (insn->defs (), VTYPE_REGNUM))
    info = vector_insn_info::get_unknown ();
}

/* Emit vsetvl within each block by forward data-flow analysis.  */
void
pass_vsetvl::emit_local_forward_vsetvls (const bb_info *bb)
{
  auto &block_info = m_vector_manager->vector_block_infos[bb->index ()];
  if (block_info.local_dem.empty_p ())
    return;

  vector_insn_info curr_info;
  for (insn_info *insn : bb->real_nondebug_insns ())
    {
      const vector_insn_info prev_info = curr_info;
      transfer_before (curr_info, insn);

      if (has_vtype_op (insn->rtl ()))
	{
	  if (static_cast<const vl_vtype_info &> (prev_info)
	      != static_cast<const vl_vtype_info &> (curr_info))
	    {
	      const auto require
		= m_vector_manager->vector_insn_infos[insn->uid ()];
	      if (!require.compatible_p (
		    static_cast<const vl_vtype_info &> (prev_info)))
		insert_vsetvl (EMIT_BEFORE, insn->rtl (), require, prev_info);
	    }
	}

      transfer_after (curr_info, insn);
    }

  block_info.reaching_out = curr_info;
}

/* Merge all successors of Father except child node.  */
void
pass_vsetvl::merge_successors (const basic_block father,
			       const basic_block child)
{
  edge e;
  edge_iterator ei;
  auto &father_info = m_vector_manager->vector_block_infos[father->index];
  gcc_assert (father_info.local_dem.dirty_p ()
	      || father_info.local_dem.empty_p ());
  gcc_assert (father_info.reaching_out.dirty_p ()
	      || father_info.reaching_out.empty_p ());

  FOR_EACH_EDGE (e, ei, father->succs)
    {
      const basic_block succ = e->dest;
      if (succ->index == child->index)
	continue;

      const auto succ_info
	= m_vector_manager->vector_block_infos[succ->index].local_dem;

      if (!succ_info.valid_p ())
	continue;

      vector_insn_info new_info;
      if (father_info.reaching_out.dirty_p ())
	{
	  if (!father_info.reaching_out.compatible_p (succ_info))
	    continue;

	  new_info = succ_info.merge (father_info.reaching_out, true);
	}
      else
	new_info = succ_info;

      new_info.set_dirty ();
      rtx new_pat = gen_vsetvl_pat (new_info.get_insn ()->rtl (), new_info);
      new_info.set_dirty_pat (new_pat);

      father_info.local_dem = new_info;
      father_info.reaching_out = new_info;
    }
}

/* Compute global backward demanded info.  */
void
pass_vsetvl::compute_global_backward_infos (void)
{
  /* We compute global infos by backward propagation.
     We want to have better performance in these following cases:

	1. for (size_t i = 0; i < n; i++) {
	     if (i != cond) {
	       vint8mf8_t v = *(vint8mf8_t*)(in + i + 100);
	       *(vint8mf8_t*)(out + i + 100) = v;
	     } else {
	       vbool1_t v = *(vbool1_t*)(in + i + 400);
	       *(vbool1_t*)(out + i + 400) = v;
	     }
	   }

	   Since we don't have any RVV instruction in the BEFORE blocks,
	   LCM fails to optimize such case. We want to backward propagate
	   them into empty blocks so that we could have better performance
	   in LCM.

	2. bb 0:
	     vsetvl e8,mf8 (demand RATIO)
	   bb 1:
	     vsetvl e32,mf2 (demand SEW and LMUL)
	   We backward propagate the first VSETVL into e32,mf2 so that we
	   could be able to eliminate the second VSETVL in LCM.  */

  for (const bb_info *bb : crtl->ssa->reverse_bbs ())
    {
      basic_block cfg_bb = bb->cfg_bb ();
      const auto &prop
	= m_vector_manager->vector_block_infos[cfg_bb->index].local_dem;

      /* If there is nothing to propagate, just skip it.  */
      if (!prop.valid_or_dirty_p ())
	continue;

      if (!backward_propagate_worthwhile_p (
	    cfg_bb, m_vector_manager->vector_block_infos[cfg_bb->index]))
	continue;

      edge e;
      edge_iterator ei;
      /* Backward propagate to each predecessor.  */
      FOR_EACH_EDGE (e, ei, cfg_bb->preds)
	{
	  rtx new_pat;
	  auto &block_info
	    = m_vector_manager->vector_block_infos[e->src->index];

	  /* We don't propagate through critical edges.  */
	  if (e->flags & EDGE_COMPLEX)
	    continue;
	  if (e->src->index == ENTRY_BLOCK_PTR_FOR_FN (cfun)->index)
	    continue;

	  if (block_info.reaching_out.unknown_p ())
	    continue;
	  else if (block_info.reaching_out.empty_p ())
	    {
	      if (!can_backward_propagate_p (crtl->ssa, e->src, prop))
		continue;

	      if (dominate_probability_p (e))
		{
		  rtx new_pat = gen_vsetvl_pat (prop.get_insn ()->rtl (), prop);

		  block_info.reaching_out = prop;
		  block_info.reaching_out.set_dirty ();
		  block_info.reaching_out.set_dirty_pat (new_pat);
		  block_info.local_dem = block_info.reaching_out;
		}

	      merge_successors (e->src, cfg_bb);
	    }
	  else if (block_info.reaching_out.dirty_p ())
	    {
	      /* DIRTY -> DIRTY or VALID -> DIRTY.  */
	      vector_insn_info new_info;

	      if (block_info.reaching_out.compatible_p (prop))
		{
		  if (block_info.reaching_out >= prop)
		    continue;
		  new_info = block_info.reaching_out.merge (prop, true);
		}
	      else
		{
		  if (dominate_probability_p (e))
		    new_info = prop;
		  else
		    continue;
		}

	      rtx new_pat
		= gen_vsetvl_pat (new_info.get_insn ()->rtl (), new_info);
	      new_info.set_dirty ();
	      new_info.set_dirty_pat (new_pat);
	      block_info.local_dem = new_info;
	      block_info.reaching_out = new_info;
	    }
	  else
	    {
	      /* We not only change the info during backward propagation,
		 but also change the VSETVL instruction.  */
	      gcc_assert (block_info.reaching_out.valid_p ());
	      if (!block_info.reaching_out.compatible_p (prop))
		continue;
	      if (block_info.reaching_out >= prop)
		continue;

	      vector_insn_info be_merged = block_info.reaching_out;
	      if (block_info.local_dem == block_info.reaching_out)
		be_merged = block_info.local_dem;
	      vector_insn_info new_info = be_merged.merge (prop, true);

	      rtx_insn *rinsn;
	      if (vector_config_insn_p (new_info.get_insn ()->rtl ()))
		{
		  rinsn = new_info.get_insn ()->rtl ();
		  gcc_assert (vsetvl_insn_p (rinsn)
			      && "Can't handle X0, rs1 vsetvli yet");
		}
	      else
		{
		  gcc_assert (has_vtype_op (new_info.get_insn ()->rtl ()));
		  rinsn = PREV_INSN (new_info.get_insn ()->rtl ());
		  gcc_assert (vector_config_insn_p (rinsn));
		}
	      new_pat = gen_vsetvl_pat (rinsn, new_info);
	      change_insn (rinsn, new_pat);
	      if (block_info.local_dem == block_info.reaching_out)
		block_info.local_dem = new_info;
	      block_info.reaching_out = new_info;
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\n\nDirty blocks list: ");
      for (size_t i = 0; i < m_vector_manager->vector_block_infos.length ();
	   i++)
	{
	  if (m_vector_manager->vector_block_infos[i].reaching_out.dirty_p ())
	    fprintf (dump_file, "%ld ", i);
	}
      fprintf (dump_file, "\n\n");
    }
}

/* Assemble the candidates expressions for LCM.  */
void
pass_vsetvl::prune_expressions (void)
{
  for (size_t i = 0; i < m_vector_manager->vector_block_infos.length (); i++)
    {
      if (m_vector_manager->vector_block_infos[i].local_dem.valid_or_dirty_p ())
	m_vector_manager->create_expr (
	  m_vector_manager->vector_block_infos[i].local_dem);
      if (m_vector_manager->vector_block_infos[i]
	    .reaching_out.valid_or_dirty_p ())
	m_vector_manager->create_expr (
	  m_vector_manager->vector_block_infos[i].reaching_out);
    }

  if (dump_file)
    {
      fprintf (dump_file, "\nThe total VSETVL expression num = %d\n",
	       m_vector_manager->vector_exprs.length ());
      fprintf (dump_file, "Expression List:\n");
      for (size_t i = 0; i < m_vector_manager->vector_exprs.length (); i++)
	{
	  fprintf (dump_file, "Expr[%ld]:\n", i);
	  m_vector_manager->vector_exprs[i]->dump (dump_file);
	  fprintf (dump_file, "\n");
	}
    }
}

void
pass_vsetvl::compute_local_properties (void)
{
  /* -  If T is locally available at the end of a block, then T' must be
	available at the end of the same block. Since some optimization has
	occurred earlier, T' might not be locally available, however, it must
	have been previously computed on all paths. As a formula, T at AVLOC(B)
	implies that T' at AVOUT(B).
	An "available occurrence" is one that is the last occurrence in the
	basic block and the operands are not modified by following statements in
	the basic block [including this insn].

     -  If T is locally anticipated at the beginning of a block, then either
	T', is locally anticipated or it is already available from previous
	blocks. As a formula, this means that T at ANTLOC(B) implies that T' at
	ANTLOC(B) at AVIN(B).
	An "anticipatable occurrence" is one that is the first occurrence in the
	basic block, the operands are not modified in the basic block prior
	to the occurrence and the output is not used between the start of
	the block and the occurrence.  */

  basic_block cfg_bb;
  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      int curr_bb_idx = cfg_bb->index;
      const auto local_dem
	= m_vector_manager->vector_block_infos[curr_bb_idx].local_dem;
      const auto reaching_out
	= m_vector_manager->vector_block_infos[curr_bb_idx].reaching_out;

      if (!local_dem.empty_p ())
	{
	  for (size_t i = 0; i < m_vector_manager->vector_exprs.length (); i++)
	    bitmap_clear_bit (m_vector_manager->vector_transp[curr_bb_idx], i);
	}

      if (local_dem.valid_or_dirty_p ())
	{
	  const insn_info *header_insn = local_dem.get_insn ();
	  size_t header_index = m_vector_manager->get_expr_id (local_dem);
	  if (anticipatable_occurrence_p (header_insn, local_dem))
	    bitmap_set_bit (m_vector_manager->vector_antic[curr_bb_idx],
			    header_index);
	}

      if (reaching_out.valid_or_dirty_p ())
	{
	  const insn_info *footer_insn = reaching_out.get_insn ();
	  size_t footer_index = m_vector_manager->get_expr_id (reaching_out);
	  if (available_occurrence_p (footer_insn, reaching_out))
	    bitmap_set_bit (m_vector_manager->vector_comp[curr_bb_idx],
			    footer_index);
	  auto_vec<size_t> available_list
	    = m_vector_manager->get_all_available_exprs (reaching_out);
	  for (size_t i = 0; i < available_list.length (); i++)
	    bitmap_set_bit (m_vector_manager->vector_comp[curr_bb_idx],
			    available_list[i]);
	}
    }

  /* Compute kill for each basic block using:

     ~(TRANSP | COMP)
  */

  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      bitmap_ior (m_vector_manager->vector_kill[cfg_bb->index],
		  m_vector_manager->vector_transp[cfg_bb->index],
		  m_vector_manager->vector_comp[cfg_bb->index]);
      bitmap_not (m_vector_manager->vector_kill[cfg_bb->index],
		  m_vector_manager->vector_kill[cfg_bb->index]);
    }

  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      edge e;
      edge_iterator ei;

      /* If the current block is the destination of an abnormal edge, we
	 kill all trapping (for PRE) and memory (for hoist) expressions
	 because we won't be able to properly place the instruction on
	 the edge.  So make them neither anticipatable nor transparent.
	 This is fairly conservative.

	 ??? For hoisting it may be necessary to check for set-and-jump
	 instructions here, not just for abnormal edges.  The general problem
	 is that when an expression cannot not be placed right at the end of
	 a basic block we should account for any side-effects of a subsequent
	 jump instructions that could clobber the expression.  It would
	 be best to implement this check along the lines of
	 should_hoist_expr_to_dom where the target block is already known
	 and, hence, there's no need to conservatively prune expressions on
	 "intermediate" set-and-jump instructions.  */
      FOR_EACH_EDGE (e, ei, cfg_bb->preds)
	if (e->flags & EDGE_COMPLEX)
	  {
	    bitmap_clear (m_vector_manager->vector_antic[cfg_bb->index]);
	    bitmap_clear (m_vector_manager->vector_transp[cfg_bb->index]);
	  }
    }
}

/* Return true if VSETVL in the block can be refined as vsetvl zero,zero.  */
bool
pass_vsetvl::can_refine_vsetvl_p (const basic_block cfg_bb, uint8_t ratio) const
{
  if (!m_vector_manager->all_same_ratio_p (
	m_vector_manager->vector_avin[cfg_bb->index]))
    return false;

  size_t expr_id
    = bitmap_first_set_bit (m_vector_manager->vector_avin[cfg_bb->index]);
  if (m_vector_manager->vector_exprs[expr_id]->get_ratio () != ratio)
    return false;

  edge e;
  edge_iterator ei;
  bool all_valid_p = true;
  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
    {
      if (bitmap_empty_p (m_vector_manager->vector_avout[e->src->index]))
	{
	  all_valid_p = false;
	  break;
	}
    }

  if (!all_valid_p)
    return false;
  return true;
}

/* Optimize athe case like this:

      bb 0:
	vsetvl 0 a5,zero,e8,mf8
	insn 0 (demand SEW + LMUL)
      bb 1:
	vsetvl 1 a5,zero,e16,mf4
	insn 1 (demand SEW + LMUL)

   In this case, we should be able to refine
   vsetvl 1 into vsetvl zero, zero according AVIN.  */
void
pass_vsetvl::refine_vsetvls (void) const
{
  basic_block cfg_bb;
  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      auto info = m_vector_manager->vector_block_infos[cfg_bb->index].local_dem;
      insn_info *insn = info.get_insn ();
      if (!info.valid_p ())
	continue;

      rtx_insn *rinsn = insn->rtl ();
      if (!can_refine_vsetvl_p (cfg_bb, info.get_ratio ()))
	continue;

      if (!vector_config_insn_p (rinsn))
	rinsn = PREV_INSN (rinsn);
      rtx new_pat = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY, info, NULL_RTX);
      change_insn (rinsn, new_pat);
    }
}

void
pass_vsetvl::cleanup_vsetvls ()
{
  basic_block cfg_bb;
  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      auto &info
	= m_vector_manager->vector_block_infos[cfg_bb->index].reaching_out;
      gcc_assert (m_vector_manager->expr_set_num (
		    m_vector_manager->vector_del[cfg_bb->index])
		  <= 1);
      for (size_t i = 0; i < m_vector_manager->vector_exprs.length (); i++)
	{
	  if (bitmap_bit_p (m_vector_manager->vector_del[cfg_bb->index], i))
	    {
	      if (info.dirty_p ())
		info.set_unknown ();
	      else
		{
		  insn_info *insn
		    = m_vector_manager->vector_exprs[i]->get_insn ();
		  gcc_assert (insn && insn->rtl ());
		  rtx_insn *rinsn;
		  if (vector_config_insn_p (insn->rtl ()))
		    rinsn = insn->rtl ();
		  else
		    {
		      gcc_assert (has_vtype_op (insn->rtl ()));
		      rinsn = PREV_INSN (insn->rtl ());
		      gcc_assert (
			vector_config_insn_p (PREV_INSN (insn->rtl ())));
		    }
		  eliminate_insn (rinsn);
		}
	    }
	}
    }
}

bool
pass_vsetvl::commit_vsetvls (void)
{
  bool need_commit = false;

  for (int ed = 0; ed < NUM_EDGES (m_vector_manager->vector_edge_list); ed++)
    {
      for (size_t i = 0; i < m_vector_manager->vector_exprs.length (); i++)
	{
	  edge eg = INDEX_EDGE (m_vector_manager->vector_edge_list, ed);
	  if (bitmap_bit_p (m_vector_manager->vector_insert[ed], i))
	    {
	      const vector_insn_info *require
		= m_vector_manager->vector_exprs[i];
	      gcc_assert (require->valid_or_dirty_p ());
	      rtl_profile_for_edge (eg);
	      start_sequence ();

	      insn_info *insn = require->get_insn ();
	      vector_insn_info prev_info = vector_insn_info ();
	      if (m_vector_manager->all_same_ratio_p (
		    m_vector_manager->vector_avout[eg->src->index]))
		{
		  size_t first = bitmap_first_set_bit (
		    m_vector_manager->vector_avout[eg->src->index]);
		  prev_info = *m_vector_manager->vector_exprs[first];
		}

	      insert_vsetvl (EMIT_DIRECT, insn->rtl (), *require, prev_info);
	      rtx_insn *rinsn = get_insns ();
	      end_sequence ();
	      default_rtl_profile ();

	      /* We should not get an abnormal edge here.  */
	      gcc_assert (!(eg->flags & EDGE_ABNORMAL));
	      need_commit = true;
	      insert_insn_on_edge (rinsn, eg);
	    }
	}
    }

  basic_block cfg_bb;
  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      const auto reaching_out
	= m_vector_manager->vector_block_infos[cfg_bb->index].reaching_out;
      if (!reaching_out.dirty_p ())
	continue;

      rtx new_pat = reaching_out.get_dirty_pat ();
      if (can_refine_vsetvl_p (cfg_bb, reaching_out.get_ratio ()))
	new_pat
	  = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY, reaching_out, NULL_RTX);

      start_sequence ();
      emit_insn (new_pat);
      rtx_insn *rinsn = get_insns ();
      end_sequence ();
      insert_insn_end_basic_block (rinsn, cfg_bb);
      if (dump_file)
	{
	  fprintf (dump_file,
		   "\nInsert vsetvl insn %d at the end of <bb %d>:\n",
		   INSN_UID (rinsn), cfg_bb->index);
	  print_rtl_single (dump_file, rinsn);
	}
    }

  return need_commit;
}

void
pass_vsetvl::pre_vsetvl (void)
{
  /* Compute entity list.  */
  prune_expressions ();

  /* Create the bitmap vectors.  */
  m_vector_manager->vector_antic
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    m_vector_manager->vector_exprs.length ());
  m_vector_manager->vector_transp
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    m_vector_manager->vector_exprs.length ());
  m_vector_manager->vector_comp
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    m_vector_manager->vector_exprs.length ());
  m_vector_manager->vector_avin
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    m_vector_manager->vector_exprs.length ());
  m_vector_manager->vector_avout
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    m_vector_manager->vector_exprs.length ());
  m_vector_manager->vector_kill
    = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
			    m_vector_manager->vector_exprs.length ());

  bitmap_vector_ones (m_vector_manager->vector_transp,
		      last_basic_block_for_fn (cfun));
  bitmap_vector_clear (m_vector_manager->vector_antic,
		       last_basic_block_for_fn (cfun));
  bitmap_vector_clear (m_vector_manager->vector_comp,
		       last_basic_block_for_fn (cfun));
  compute_local_properties ();
  m_vector_manager->vector_edge_list = pre_edge_lcm_avs (
    m_vector_manager->vector_exprs.length (), m_vector_manager->vector_transp,
    m_vector_manager->vector_comp, m_vector_manager->vector_antic,
    m_vector_manager->vector_kill, m_vector_manager->vector_avin,
    m_vector_manager->vector_avout, &m_vector_manager->vector_insert,
    &m_vector_manager->vector_del);

  /* We should dump the information before CFG is changed. Otherwise it will
     produce ICE (internal compiler error).  */
  if (dump_file)
    m_vector_manager->dump (dump_file);

  refine_vsetvls ();
  cleanup_vsetvls ();
  bool need_commit = commit_vsetvls ();
  if (need_commit)
    commit_edge_insertions ();
}

void
pass_vsetvl::cleanup_insns (void) const
{
  for (const bb_info *bb : crtl->ssa->bbs ())
    {
      for (insn_info *insn : bb->real_nondebug_insns ())
	{
	  rtx_insn *rinsn = insn->rtl ();

	  if (vlmax_avl_insn_p (rinsn))
	    {
	      eliminate_insn (rinsn);
	      continue;
	    }

	  /* Erase the AVL operand from the instruction.  */
	  if (!has_vl_op (rinsn) || !REG_P (get_vl (rinsn)))
	    continue;
	  rtx avl = get_vl (rinsn);
	  if (count_occurrences (PATTERN (rinsn), avl, true) == 1)
	    {
	      /* Get the list of uses for the new instruction.  */
	      auto attempt = crtl->ssa->new_change_attempt ();
	      insn_change change (insn);
	      /* Remove the use of the substituted value.  */
	      access_array_builder uses_builder (attempt);
	      uses_builder.reserve (insn->num_uses () - 1);
	      for (use_info *use : insn->uses ())
		if (use != find_access (insn->uses (), REGNO (avl)))
		  uses_builder.quick_push (use);
	      use_array new_uses = use_array (uses_builder.finish ());
	      change.new_uses = new_uses;
	      change.move_range = insn->ebb ()->insn_range ();
	      rtx pat = simplify_replace_rtx (PATTERN (rinsn), avl, const0_rtx);
	      gcc_assert (change_insn (crtl->ssa, change, insn, pat));
	    }
	}
    }
}

void
pass_vsetvl::init (void)
{
  if (optimize > 0)
    {
      /* Initialization of RTL_SSA.  */
      calculate_dominance_info (CDI_DOMINATORS);
      df_analyze ();
      crtl->ssa = new function_info (cfun);
    }

  m_vector_manager = new vector_infos_manager ();

  if (dump_file)
    {
      fprintf (dump_file, "\nPrologue: Initialize vector infos\n");
      m_vector_manager->dump (dump_file);
    }
}

void
pass_vsetvl::done (void)
{
  if (optimize > 0)
    {
      /* Finalization of RTL_SSA.  */
      free_dominance_info (CDI_DOMINATORS);
      if (crtl->ssa->perform_pending_updates ())
	cleanup_cfg (0);
      delete crtl->ssa;
      crtl->ssa = nullptr;
    }
  m_vector_manager->release ();
  delete m_vector_manager;
  m_vector_manager = nullptr;
}

/* Lazy vsetvl insertion for optimize > 0. */
void
pass_vsetvl::lazy_vsetvl (void)
{
  if (dump_file)
    fprintf (dump_file,
	     "\nEntering Lazy VSETVL PASS and Handling %d basic blocks for "
	     "function:%s\n",
	     n_basic_blocks_for_fn (cfun), function_name (cfun));

  /* Phase 1 - Compute the local dems within each block.
     The data-flow analysis within each block is backward analysis.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 1: Compute local backward vector infos\n");
  for (const bb_info *bb : crtl->ssa->bbs ())
    compute_local_backward_infos (bb);
  if (dump_file)
    m_vector_manager->dump (dump_file);

  /* Phase 2 - Emit vsetvl instructions within each basic block according to
     demand, compute and save ANTLOC && AVLOC of each block.  */
  if (dump_file)
    fprintf (dump_file,
	     "\nPhase 2: Emit vsetvl instruction within each block\n");
  for (const bb_info *bb : crtl->ssa->bbs ())
    emit_local_forward_vsetvls (bb);
  if (dump_file)
    m_vector_manager->dump (dump_file);

  /* Phase 3 - Propagate demanded info across blocks.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 3: Demands propagation across blocks\n");
  compute_global_backward_infos ();
  if (dump_file)
    m_vector_manager->dump (dump_file);

  /* Phase 4 - Lazy code motion.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 4: PRE vsetvl by Lazy code motion (LCM)\n");
  pre_vsetvl ();

  /* Phase 5 - Cleanup AVL && VL operand of RVV instruction.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 5: Cleanup AVL and VL operands\n");
  cleanup_insns ();
}

/* Main entry point for this pass.  */
unsigned int
pass_vsetvl::execute (function *)
{
  if (n_basic_blocks_for_fn (cfun) <= 0)
    return 0;

  /* The reason we have this since we didn't finish splitting yet
     when optimize == 0. In this case, we should conservatively
     split all instructions here to make sure we don't miss any
     RVV instruction.  */
  if (!optimize)
    split_all_insns ();

  /* Early return for there is no vector instructions.  */
  if (!has_vector_insn (cfun))
    return 0;

  init ();

  if (!optimize)
    simple_vsetvl ();
  else
    lazy_vsetvl ();

  done ();
  return 0;
}

rtl_opt_pass *
make_pass_vsetvl (gcc::context *ctxt)
{
  return new pass_vsetvl (ctxt);
}
