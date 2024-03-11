/* VSETVL pass for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
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

    -  Phase 3 - LCM Earliest-edge baseed VSETVL demand fusion.

    -  Phase 4 - Lazy code motion including: compute local properties,
       pre_edge_lcm and vsetvl insertion && delete edges for LCM results.

    -  Phase 5 - Cleanup AVL operand of RVV instruction since it will not be
       used any more and VL operand of VSETVL instruction if it is not used by
       any non-debug instructions.

    -  Phase 6 - DF based post VSETVL optimizations.

    Implementation:

    -  The subroutine of optimize == 0 is simple_vsetvl.
       This function simplily vsetvl insertion for each RVV
       instruction. No optimization.

    -  The subroutine of optimize > 0 is lazy_vsetvl.
       This function optimize vsetvl insertion process by
       lazy code motion (LCM) layering on RTL_SSA.

    -  get_avl (), get_insn (), get_avl_source ():

	1. get_insn () is the current instruction, find_access (get_insn
   ())->def is the same as get_avl_source () if get_insn () demand VL.
	2. If get_avl () is non-VLMAX REG, get_avl () == get_avl_source
   ()->regno ().
	3. get_avl_source ()->regno () is the REGNO that we backward propagate.
 */

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
#include "gcse.h"
#include "riscv-vsetvl.h"

using namespace rtl_ssa;
using namespace riscv_vector;

static CONSTEXPR const unsigned ALL_SEW[] = {8, 16, 32, 64};
static CONSTEXPR const vlmul_type ALL_LMUL[]
  = {LMUL_1, LMUL_2, LMUL_4, LMUL_8, LMUL_F8, LMUL_F4, LMUL_F2};

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

/* Return true if the block is a loop itself:
	  local_dem
	     __________
	 ____|____     |
	|        |     |
	|________|     |
	     |_________|
	  reaching_out
*/
static bool
loop_basic_block_p (const basic_block cfg_bb)
{
  if (JUMP_P (BB_END (cfg_bb)) && any_condjump_p (BB_END (cfg_bb)))
    {
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, cfg_bb->succs)
	if (e->dest->index == cfg_bb->index)
	  return true;
    }
  return false;
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

/* Return true if the instruction ignores VLMUL field of VTYPE.  */
static bool
ignore_vlmul_insn_p (rtx_insn *rinsn)
{
  return get_attr_type (rinsn) == TYPE_VIMOVVX
	 || get_attr_type (rinsn) == TYPE_VFMOVVF
	 || get_attr_type (rinsn) == TYPE_VIMOVXV
	 || get_attr_type (rinsn) == TYPE_VFMOVFV;
}

/* Return true if the instruction is scalar move instruction.  */
static bool
scalar_move_insn_p (rtx_insn *rinsn)
{
  return get_attr_type (rinsn) == TYPE_VIMOVXV
	 || get_attr_type (rinsn) == TYPE_VFMOVFV;
}

/* Return true if the instruction is fault first load instruction.  */
static bool
fault_first_load_p (rtx_insn *rinsn)
{
  return recog_memoized (rinsn) >= 0
	 && (get_attr_type (rinsn) == TYPE_VLDFF
	     || get_attr_type (rinsn) == TYPE_VLSEGDFF);
}

/* Return true if the instruction is read vl instruction.  */
static bool
read_vl_insn_p (rtx_insn *rinsn)
{
  return recog_memoized (rinsn) >= 0 && get_attr_type (rinsn) == TYPE_RDVL;
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
  if (!vector_config_insn_p (rinsn))
    return false;
  return (INSN_CODE (rinsn) == CODE_FOR_vsetvldi
	  || INSN_CODE (rinsn) == CODE_FOR_vsetvlsi);
}

/* Return true if it is vsetvl zero, rs1.  */
static bool
vsetvl_discard_result_insn_p (rtx_insn *rinsn)
{
  if (!vector_config_insn_p (rinsn))
    return false;
  return (INSN_CODE (rinsn) == CODE_FOR_vsetvl_discard_resultdi
	  || INSN_CODE (rinsn) == CODE_FOR_vsetvl_discard_resultsi);
}

/* Return true if it is vsetvl zero, zero.  */
static bool
vsetvl_vtype_change_only_p (rtx_insn *rinsn)
{
  if (!vector_config_insn_p (rinsn))
    return false;
  return (INSN_CODE (rinsn) == CODE_FOR_vsetvl_vtype_change_only);
}

static bool
after_or_same_p (const insn_info *insn1, const insn_info *insn2)
{
  return insn1->compare_with (insn2) >= 0;
}

static bool
real_insn_and_same_bb_p (const insn_info *insn, const bb_info *bb)
{
  return insn != nullptr && insn->is_real () && insn->bb () == bb;
}

static bool
before_p (const insn_info *insn1, const insn_info *insn2)
{
  return insn1->compare_with (insn2) < 0;
}

/* Helper function to get VL operand.  */
static rtx
get_vl (rtx_insn *rinsn)
{
  if (has_vl_op (rinsn))
    {
      extract_insn_cached (rinsn);
      return recog_data.operand[get_attr_vl_op_idx (rinsn)];
    }
  return SET_DEST (XVECEXP (PATTERN (rinsn), 0, 0));
}

/* An "anticipatable occurrence" is one that is the first occurrence in the
   basic block, the operands are not modified in the basic block prior
   to the occurrence and the output is not used between the start of
   the block and the occurrence.

   For VSETVL instruction, we have these following formats:
     1. vsetvl zero, rs1.
     2. vsetvl zero, imm.
     3. vsetvl rd, rs1.

   So base on these circumstances, a DEM is considered as a local anticipatable
   occurrence should satisfy these following conditions:

     1). rs1 (avl) are not modified in the basic block prior to the VSETVL.
     2). rd (vl) are not modified in the basic block prior to the VSETVL.
     3). rd (vl) is not used between the start of the block and the occurrence.

   Note: We don't need to check VL/VTYPE here since DEM is UNKNOWN if VL/VTYPE
	 is modified prior to the occurrence. This case is already considered as
	 a non-local anticipatable occurrence.
*/
static bool
anticipatable_occurrence_p (const bb_info *bb, const vector_insn_info dem)
{
  insn_info *insn = dem.get_insn ();
  /* The only possible operand we care of VSETVL is AVL.  */
  if (dem.has_avl_reg ())
    {
      /* rs1 (avl) are not modified in the basic block prior to the VSETVL.  */
      rtx avl = dem.get_avl_or_vl_reg ();
      if (dem.dirty_p ())
	{
	  gcc_assert (!vsetvl_insn_p (insn->rtl ()));

	  /* Earliest VSETVL will be inserted at the end of the block.  */
	  for (const insn_info *i : bb->real_nondebug_insns ())
	    {
	      /* rs1 (avl) are not modified in the basic block prior to the
		 VSETVL.  */
	      if (find_access (i->defs (), REGNO (avl)))
		return false;
	      if (vlmax_avl_p (dem.get_avl ()))
		{
		  /* rd (avl) is not used between the start of the block and
		     the occurrence. Note: Only for Dirty and VLMAX-avl.  */
		  if (find_access (i->uses (), REGNO (avl)))
		    return false;
		}
	    }

	  return true;
	}
      else if (!vlmax_avl_p (avl))
	{
	  set_info *set = dem.get_avl_source ();
	  /* If it's undefined, it's not anticipatable conservatively.  */
	  if (!set)
	    return false;
	  if (real_insn_and_same_bb_p (set->insn (), bb)
	      && before_p (set->insn (), insn))
	    return false;
	  for (insn_info *i = insn->prev_nondebug_insn ();
	       real_insn_and_same_bb_p (i, bb); i = i->prev_nondebug_insn ())
	    {
	      /* rs1 (avl) are not modified in the basic block prior to the
		 VSETVL.  */
	      if (find_access (i->defs (), REGNO (avl)))
		return false;
	    }
	}
    }

  /* rd (vl) is not used between the start of the block and the occurrence.  */
  if (vsetvl_insn_p (insn->rtl ()))
    {
      rtx dest = get_vl (insn->rtl ());
      for (insn_info *i = insn->prev_nondebug_insn ();
	   real_insn_and_same_bb_p (i, bb); i = i->prev_nondebug_insn ())
	{
	  /* rd (vl) is not used between the start of the block and the
	   * occurrence.  */
	  if (find_access (i->uses (), REGNO (dest)))
	    return false;
	  /* rd (vl) are not modified in the basic block prior to the VSETVL. */
	  if (find_access (i->defs (), REGNO (dest)))
	    return false;
	}
    }

  return true;
}

/* An "available occurrence" is one that is the last occurrence in the
   basic block and the operands are not modified by following statements in
   the basic block [including this insn].

   For VSETVL instruction, we have these following formats:
     1. vsetvl zero, rs1.
     2. vsetvl zero, imm.
     3. vsetvl rd, rs1.

   So base on these circumstances, a DEM is considered as a local available
   occurrence should satisfy these following conditions:

     1). rs1 (avl) are not modified by following statements in
	 the basic block.
     2). rd (vl) are not modified by following statements in
	 the basic block.

   Note: We don't need to check VL/VTYPE here since DEM is UNKNOWN if VL/VTYPE
	 is modified prior to the occurrence. This case is already considered as
	 a non-local available occurrence.
*/
static bool
available_occurrence_p (const bb_info *bb, const vector_insn_info dem)
{
  insn_info *insn = dem.get_insn ();
  /* The only possible operand we care of VSETVL is AVL.  */
  if (dem.has_avl_reg ())
    {
      if (!vlmax_avl_p (dem.get_avl ()))
	{
	  rtx dest = NULL_RTX;
	  insn_info *i = insn;
	  if (vsetvl_insn_p (insn->rtl ()))
	    {
	      dest = get_vl (insn->rtl ());
	      /* For user vsetvl a2, a2 instruction, we consider it as
		 available even though it modifies "a2".  */
	      i = i->next_nondebug_insn ();
	    }
	  for (; real_insn_and_same_bb_p (i, bb); i = i->next_nondebug_insn ())
	    {
	      if (read_vl_insn_p (i->rtl ()))
		continue;
	      /* rs1 (avl) are not modified by following statements in
		 the basic block.  */
	      if (find_access (i->defs (), REGNO (dem.get_avl ())))
		return false;
	      /* rd (vl) are not modified by following statements in
		 the basic block.  */
	      if (dest && find_access (i->defs (), REGNO (dest)))
		return false;
	    }
	}
    }
  return true;
}

static bool
insn_should_be_added_p (const insn_info *insn, unsigned int types)
{
  if (insn->is_real () && (types & REAL_SET))
    return true;
  if (insn->is_phi () && (types & PHI_SET))
    return true;
  if (insn->is_bb_head () && (types & BB_HEAD_SET))
    return true;
  if (insn->is_bb_end () && (types & BB_END_SET))
    return true;
  return false;
}

/* Recursively find all define instructions. The kind of instruction is
   specified by the DEF_TYPE.  */
static hash_set<set_info *>
get_all_sets (phi_info *phi, unsigned int types)
{
  hash_set<set_info *> insns;
  auto_vec<phi_info *> work_list;
  hash_set<phi_info *> visited_list;
  if (!phi)
    return hash_set<set_info *> ();
  work_list.safe_push (phi);

  while (!work_list.is_empty ())
    {
      phi_info *phi = work_list.pop ();
      visited_list.add (phi);
      for (use_info *use : phi->inputs ())
	{
	  def_info *def = use->def ();
	  set_info *set = safe_dyn_cast<set_info *> (def);
	  if (!set)
	    return hash_set<set_info *> ();

	  gcc_assert (!set->insn ()->is_debug_insn ());

	  if (insn_should_be_added_p (set->insn (), types))
	    insns.add (set);
	  if (set->insn ()->is_phi ())
	    {
	      phi_info *new_phi = as_a<phi_info *> (set);
	      if (!visited_list.contains (new_phi))
		work_list.safe_push (new_phi);
	    }
	}
    }
  return insns;
}

static hash_set<set_info *>
get_all_sets (set_info *set, bool /* get_real_inst */ real_p,
	      bool /*get_phi*/ phi_p, bool /* get_function_parameter*/ param_p)
{
  if (real_p && phi_p && param_p)
    return get_all_sets (safe_dyn_cast<phi_info *> (set),
			 REAL_SET | PHI_SET | BB_HEAD_SET | BB_END_SET);

  else if (real_p && param_p)
    return get_all_sets (safe_dyn_cast<phi_info *> (set),
			 REAL_SET | BB_HEAD_SET | BB_END_SET);

  else if (real_p)
    return get_all_sets (safe_dyn_cast<phi_info *> (set), REAL_SET);
  return hash_set<set_info *> ();
}

/* Helper function to get AVL operand.  */
static rtx
get_avl (rtx_insn *rinsn)
{
  if (vsetvl_insn_p (rinsn) || vsetvl_discard_result_insn_p (rinsn))
    return XVECEXP (SET_SRC (XVECEXP (PATTERN (rinsn), 0, 0)), 0, 0);

  if (!has_vl_op (rinsn))
    return NULL_RTX;
  if (get_attr_avl_type (rinsn) == VLMAX)
    return RVV_VLMAX;
  extract_insn_cached (rinsn);
  return recog_data.operand[get_attr_vl_op_idx (rinsn)];
}

static set_info *
get_same_bb_set (hash_set<set_info *> &sets, const basic_block cfg_bb)
{
  for (set_info *set : sets)
    if (set->bb ()->cfg_bb () == cfg_bb)
      return set;
  return nullptr;
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
gen_vsetvl_pat (enum vsetvl_type insn_type, const vl_vtype_info &info, rtx vl)
{
  rtx avl = info.get_avl ();
  /* if optimization == 0 and the instruction is vmv.x.s/vfmv.f.s,
     set the value of avl to (const_int 0) so that VSETVL PASS will
     insert vsetvl correctly.*/
  if (info.has_avl_no_reg ())
    avl = GEN_INT (0);
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
gen_vsetvl_pat (rtx_insn *rinsn, const vector_insn_info &info,
		rtx vl = NULL_RTX)
{
  rtx new_pat;
  vl_vtype_info new_info = info;
  if (info.get_insn () && info.get_insn ()->rtl ()
      && fault_first_load_p (info.get_insn ()->rtl ()))
    new_info.set_avl_info (
      avl_info (get_avl (info.get_insn ()->rtl ()), nullptr));
  if (vl)
    new_pat = gen_vsetvl_pat (VSETVL_NORMAL, new_info, vl);
  else
    {
      if (vsetvl_insn_p (rinsn))
	new_pat = gen_vsetvl_pat (VSETVL_NORMAL, new_info, get_vl (rinsn));
      else if (INSN_CODE (rinsn) == CODE_FOR_vsetvl_vtype_change_only)
	new_pat = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY, new_info, NULL_RTX);
      else
	new_pat = gen_vsetvl_pat (VSETVL_DISCARD_RESULT, new_info, NULL_RTX);
    }
  return new_pat;
}

static void
emit_vsetvl_insn (enum vsetvl_type insn_type, enum emit_type emit_type,
		  const vl_vtype_info &info, rtx vl, rtx_insn *rinsn)
{
  rtx pat = gen_vsetvl_pat (insn_type, info, vl);
  if (dump_file)
    {
      fprintf (dump_file, "\nInsert vsetvl insn PATTERN:\n");
      print_rtl_single (dump_file, pat);
      fprintf (dump_file, "\nfor insn:\n");
      print_rtl_single (dump_file, rinsn);
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

static vsetvl_type
insert_vsetvl (enum emit_type emit_type, rtx_insn *rinsn,
	       const vector_insn_info &info, const vector_insn_info &prev_info)
{
  /* Use X0, X0 form if the AVL is the same and the SEW+LMUL gives the same
     VLMAX.  */
  if (prev_info.valid_or_dirty_p () && !prev_info.unknown_p ()
      && info.compatible_avl_p (prev_info) && info.same_vlmax_p (prev_info))
    {
      emit_vsetvl_insn (VSETVL_VTYPE_CHANGE_ONLY, emit_type, info, NULL_RTX,
			rinsn);
      return VSETVL_VTYPE_CHANGE_ONLY;
    }

  if (info.has_avl_imm ())
    {
      emit_vsetvl_insn (VSETVL_DISCARD_RESULT, emit_type, info, NULL_RTX,
			rinsn);
      return VSETVL_DISCARD_RESULT;
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
	  return VSETVL_VTYPE_CHANGE_ONLY;
	}
      /* Otherwise use an AVL of 0 to avoid depending on previous vl.  */
      vl_vtype_info new_info = info;
      new_info.set_avl_info (avl_info (const0_rtx, nullptr));
      emit_vsetvl_insn (VSETVL_DISCARD_RESULT, emit_type, new_info, NULL_RTX,
			rinsn);
      return VSETVL_DISCARD_RESULT;
    }

  /* Use X0 as the DestReg unless AVLReg is X0. We also need to change the
     opcode if the AVLReg is X0 as they have different register classes for
     the AVL operand.  */
  if (vlmax_avl_p (info.get_avl ()))
    {
      gcc_assert (has_vtype_op (rinsn) || vsetvl_insn_p (rinsn));
      /* For user vsetvli a5, zero, we should use get_vl to get the VL
	 operand "a5".  */
      rtx vl_op = info.get_avl_or_vl_reg ();
      gcc_assert (!vlmax_avl_p (vl_op));
      emit_vsetvl_insn (VSETVL_NORMAL, emit_type, info, vl_op, rinsn);
      return VSETVL_NORMAL;
    }

  emit_vsetvl_insn (VSETVL_DISCARD_RESULT, emit_type, info, NULL_RTX, rinsn);

  if (dump_file)
    {
      fprintf (dump_file, "Update VL/VTYPE info, previous info=");
      prev_info.dump (dump_file);
    }
  return VSETVL_DISCARD_RESULT;
}

/* Get VL/VTYPE information for INSN.  */
static vl_vtype_info
get_vl_vtype_info (const insn_info *insn)
{
  set_info *set = nullptr;
  rtx avl = ::get_avl (insn->rtl ());
  if (avl && REG_P (avl))
    {
      if (vlmax_avl_p (avl) && has_vl_op (insn->rtl ()))
	set
	  = find_access (insn->uses (), REGNO (get_vl (insn->rtl ())))->def ();
      else if (!vlmax_avl_p (avl))
	set = find_access (insn->uses (), REGNO (avl))->def ();
      else
	set = nullptr;
    }

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

/* Change insn and Assert the change always happens.  */
static void
validate_change_or_fail (rtx object, rtx *loc, rtx new_rtx, bool in_group)
{
  bool change_p = validate_change (object, loc, new_rtx, in_group);
  gcc_assert (change_p);
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

  validate_change_or_fail (rinsn, &PATTERN (rinsn), new_pat, false);

  if (dump_file)
    {
      fprintf (dump_file, "\nto:\n");
      print_rtl_single (dump_file, PATTERN (rinsn));
    }
}

static const insn_info *
get_forward_read_vl_insn (const insn_info *insn)
{
  const bb_info *bb = insn->bb ();
  for (const insn_info *i = insn->next_nondebug_insn ();
       real_insn_and_same_bb_p (i, bb); i = i->next_nondebug_insn ())
    {
      if (find_access (i->defs (), VL_REGNUM))
	return nullptr;
      if (read_vl_insn_p (i->rtl ()))
	return i;
    }
  return nullptr;
}

static const insn_info *
get_backward_fault_first_load_insn (const insn_info *insn)
{
  const bb_info *bb = insn->bb ();
  for (const insn_info *i = insn->prev_nondebug_insn ();
       real_insn_and_same_bb_p (i, bb); i = i->prev_nondebug_insn ())
    {
      if (fault_first_load_p (i->rtl ()))
	return i;
      if (find_access (i->defs (), VL_REGNUM))
	return nullptr;
    }
  return nullptr;
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
    }

  insn_change_watermark watermark;
  validate_change_or_fail (rinsn, &PATTERN (rinsn), new_pat, true);

  /* These routines report failures themselves.  */
  if (!recog (attempt, change) || !change_is_worthwhile (change, false))
    return false;

  /* Fix bug:
      (insn 12 34 13 2 (set (reg:RVVM4DI 120 v24 [orig:134 _1 ] [134])
	(if_then_else:RVVM4DI (unspec:RVVMF8BI [
		    (const_vector:RVVMF8BI repeat [
			    (const_int 1 [0x1])
			])
		    (const_int 0 [0])
		    (const_int 2 [0x2]) repeated x2
		    (const_int 0 [0])
		    (reg:SI 66 vl)
		    (reg:SI 67 vtype)
		] UNSPEC_VPREDICATE)
	    (plus:RVVM4DI (reg/v:RVVM4DI 104 v8 [orig:137 op1 ] [137])
		(sign_extend:RVVM4DI (vec_duplicate:RVVM4SI (reg:SI 15 a5
    [140])))) (unspec:RVVM4DI [ (const_int 0 [0]) ] UNSPEC_VUNDEF)))
    "rvv.c":8:12 2784 {pred_single_widen_addsvnx8di_scalar} (expr_list:REG_EQUIV
    (mem/c:RVVM4DI (reg:DI 10 a0 [142]) [1 <retval>+0 S[64, 64] A128])
	(expr_list:REG_EQUAL (if_then_else:RVVM4DI (unspec:RVVMF8BI [
			(const_vector:RVVMF8BI repeat [
				(const_int 1 [0x1])
			    ])
			(reg/v:DI 13 a3 [orig:139 vl ] [139])
			(const_int 2 [0x2]) repeated x2
			(const_int 0 [0])
			(reg:SI 66 vl)
			(reg:SI 67 vtype)
		    ] UNSPEC_VPREDICATE)
		(plus:RVVM4DI (reg/v:RVVM4DI 104 v8 [orig:137 op1 ] [137])
		    (const_vector:RVVM4DI repeat [
			    (const_int 2730 [0xaaa])
			]))
		(unspec:RVVM4DI [
			(const_int 0 [0])
		    ] UNSPEC_VUNDEF))
	    (nil))))
    Here we want to remove use "a3". However, the REG_EQUAL/REG_EQUIV note use
    "a3" which made us fail in change_insn.  We reference to the
    'aarch64-cc-fusion.cc' and add this method.  */
  remove_reg_equal_equiv_notes (rinsn);
  confirm_change_group ();
  ssa->change_insn (change);

  if (dump_file)
    {
      fprintf (dump_file, "\nto:\n");
      print_rtl_single (dump_file, PATTERN (rinsn));
    }
  return true;
}

static void
change_vsetvl_insn (const insn_info *insn, const vector_insn_info &info,
		    rtx vl = NULL_RTX)
{
  rtx_insn *rinsn;
  if (vector_config_insn_p (insn->rtl ()))
    {
      rinsn = insn->rtl ();
      gcc_assert (vsetvl_insn_p (rinsn) && "Can't handle X0, rs1 vsetvli yet");
    }
  else
    {
      gcc_assert (has_vtype_op (insn->rtl ()));
      rinsn = PREV_INSN (insn->rtl ());
      gcc_assert (vector_config_insn_p (rinsn));
    }
  rtx new_pat = gen_vsetvl_pat (rinsn, info, vl);
  change_insn (rinsn, new_pat);
}

static bool
avl_source_has_vsetvl_p (set_info *avl_source)
{
  if (!avl_source)
    return false;
  if (!avl_source->insn ())
    return false;
  if (avl_source->insn ()->is_real ())
    return vsetvl_insn_p (avl_source->insn ()->rtl ());
  hash_set<set_info *> sets = get_all_sets (avl_source, true, false, true);
  for (const auto set : sets)
    {
      if (set->insn ()->is_real () && vsetvl_insn_p (set->insn ()->rtl ()))
	return true;
    }
  return false;
}

static bool
source_equal_p (insn_info *insn1, insn_info *insn2)
{
  if (!insn1 || !insn2)
    return false;
  rtx_insn *rinsn1 = insn1->rtl ();
  rtx_insn *rinsn2 = insn2->rtl ();
  if (!rinsn1 || !rinsn2)
    return false;
  rtx note1 = find_reg_equal_equiv_note (rinsn1);
  rtx note2 = find_reg_equal_equiv_note (rinsn2);
  rtx single_set1 = single_set (rinsn1);
  rtx single_set2 = single_set (rinsn2);
  if (read_vl_insn_p (rinsn1) && read_vl_insn_p (rinsn2))
    {
      const insn_info *load1 = get_backward_fault_first_load_insn (insn1);
      const insn_info *load2 = get_backward_fault_first_load_insn (insn2);
      return load1 && load2 && load1 == load2;
    }

  if (note1 && note2 && rtx_equal_p (note1, note2))
    return true;

  /* Since vsetvl instruction is not single SET.
     We handle this case specially here.  */
  if (vsetvl_insn_p (insn1->rtl ()) && vsetvl_insn_p (insn2->rtl ()))
    {
      /* For example:
	   vsetvl1 a6,a5,e32m1
	   RVV 1 (use a6 as AVL)
	   vsetvl2 a5,a5,e8mf4
	   RVV 2 (use a5 as AVL)
	 We consider AVL of RVV 1 and RVV 2 are same so that we can
	 gain more optimization opportunities.

	 Note: insn1_info.compatible_avl_p (insn2_info)
	 will make sure there is no instruction between vsetvl1 and vsetvl2
	 modify a5 since their def will be different if there is instruction
	 modify a5 and compatible_avl_p will return false.  */
      vector_insn_info insn1_info, insn2_info;
      insn1_info.parse_insn (insn1);
      insn2_info.parse_insn (insn2);

      /* To avoid dead loop, we don't optimize a vsetvli def has vsetvli
	 instructions which will complicate the situation.  */
      if (avl_source_has_vsetvl_p (insn1_info.get_avl_source ())
	  || avl_source_has_vsetvl_p (insn2_info.get_avl_source ()))
	return false;

      if (insn1_info.same_vlmax_p (insn2_info)
	  && insn1_info.compatible_avl_p (insn2_info))
	return true;
    }

  /* We only handle AVL is set by instructions with no side effects.  */
  if (!single_set1 || !single_set2)
    return false;
  if (!rtx_equal_p (SET_SRC (single_set1), SET_SRC (single_set2)))
    return false;
  /* RTL_SSA uses include REG_NOTE. Consider this following case:

     insn1 RTL:
	(insn 41 39 42 4 (set (reg:DI 26 s10 [orig:159 loop_len_46 ] [159])
	  (umin:DI (reg:DI 15 a5 [orig:201 _149 ] [201])
	    (reg:DI 14 a4 [276]))) 408 {*umindi3}
	(expr_list:REG_EQUAL (umin:DI (reg:DI 15 a5 [orig:201 _149 ] [201])
	    (const_int 2 [0x2]))
	(nil)))
     The RTL_SSA uses of this instruction has 2 uses:
	1. (reg:DI 15 a5 [orig:201 _149 ] [201]) - twice.
	2. (reg:DI 14 a4 [276]) - once.

     insn2 RTL:
	(insn 38 353 351 4 (set (reg:DI 27 s11 [orig:160 loop_len_47 ] [160])
	  (umin:DI (reg:DI 15 a5 [orig:199 _146 ] [199])
	    (reg:DI 14 a4 [276]))) 408 {*umindi3}
	(expr_list:REG_EQUAL (umin:DI (reg:DI 28 t3 [orig:200 ivtmp_147 ] [200])
	    (const_int 2 [0x2]))
	(nil)))
      The RTL_SSA uses of this instruction has 3 uses:
	1. (reg:DI 15 a5 [orig:199 _146 ] [199]) - once
	2. (reg:DI 14 a4 [276]) - once
	3. (reg:DI 28 t3 [orig:200 ivtmp_147 ] [200]) - once

      Return false when insn1->uses ().size () != insn2->uses ().size ()
  */
  if (insn1->uses ().size () != insn2->uses ().size ())
    return false;
  for (size_t i = 0; i < insn1->uses ().size (); i++)
    if (insn1->uses ()[i] != insn2->uses ()[i])
      return false;
  return true;
}

/* Helper function to get single same real RTL source.
   return NULL if it is not a single real RTL source.  */
static insn_info *
extract_single_source (set_info *set)
{
  if (!set)
    return nullptr;
  if (set->insn ()->is_real ())
    return set->insn ();
  if (!set->insn ()->is_phi ())
    return nullptr;
  hash_set<set_info *> sets = get_all_sets (set, true, false, true);

  insn_info *first_insn = (*sets.begin ())->insn ();
  if (first_insn->is_artificial ())
    return nullptr;
  for (const set_info *set : sets)
    {
      /* If there is a head or end insn, we conservative return
	 NULL so that VSETVL PASS will insert vsetvl directly.  */
      if (set->insn ()->is_artificial ())
	return nullptr;
      if (!source_equal_p (set->insn (), first_insn))
	return nullptr;
    }

  return first_insn;
}

static unsigned
calculate_sew (vlmul_type vlmul, unsigned int ratio)
{
  for (const unsigned sew : ALL_SEW)
    if (calculate_ratio (sew, vlmul) == ratio)
      return sew;
  return 0;
}

static vlmul_type
calculate_vlmul (unsigned int sew, unsigned int ratio)
{
  for (const vlmul_type vlmul : ALL_LMUL)
    if (calculate_ratio (sew, vlmul) == ratio)
      return vlmul;
  return LMUL_RESERVED;
}

static bool
incompatible_avl_p (const vector_insn_info &info1,
		    const vector_insn_info &info2)
{
  return !info1.compatible_avl_p (info2) && !info2.compatible_avl_p (info1);
}

static bool
different_sew_p (const vector_insn_info &info1, const vector_insn_info &info2)
{
  return info1.get_sew () != info2.get_sew ();
}

static bool
different_lmul_p (const vector_insn_info &info1, const vector_insn_info &info2)
{
  return info1.get_vlmul () != info2.get_vlmul ();
}

static bool
different_ratio_p (const vector_insn_info &info1, const vector_insn_info &info2)
{
  return info1.get_ratio () != info2.get_ratio ();
}

static bool
different_tail_policy_p (const vector_insn_info &info1,
			 const vector_insn_info &info2)
{
  return info1.get_ta () != info2.get_ta ();
}

static bool
different_mask_policy_p (const vector_insn_info &info1,
			 const vector_insn_info &info2)
{
  return info1.get_ma () != info2.get_ma ();
}

static bool
possible_zero_avl_p (const vector_insn_info &info1,
		     const vector_insn_info &info2)
{
  return !info1.has_non_zero_avl () || !info2.has_non_zero_avl ();
}

static bool
second_ratio_invalid_for_first_sew_p (const vector_insn_info &info1,
				      const vector_insn_info &info2)
{
  return calculate_vlmul (info1.get_sew (), info2.get_ratio ())
	 == LMUL_RESERVED;
}

static bool
second_ratio_invalid_for_first_lmul_p (const vector_insn_info &info1,
				       const vector_insn_info &info2)
{
  return calculate_sew (info1.get_vlmul (), info2.get_ratio ()) == 0;
}

static bool
float_insn_valid_sew_p (const vector_insn_info &info, unsigned int sew)
{
  if (info.get_insn () && info.get_insn ()->is_real ()
      && get_attr_type (info.get_insn ()->rtl ()) == TYPE_VFMOVFV)
    {
      if (sew == 16)
	return TARGET_VECTOR_ELEN_FP_16;
      else if (sew == 32)
	return TARGET_VECTOR_ELEN_FP_32;
      else if (sew == 64)
	return TARGET_VECTOR_ELEN_FP_64;
    }
  return true;
}

static bool
second_sew_less_than_first_sew_p (const vector_insn_info &info1,
				  const vector_insn_info &info2)
{
  return info2.get_sew () < info1.get_sew ()
	 || !float_insn_valid_sew_p (info1, info2.get_sew ());
}

static bool
first_sew_less_than_second_sew_p (const vector_insn_info &info1,
				  const vector_insn_info &info2)
{
  return info1.get_sew () < info2.get_sew ()
	 || !float_insn_valid_sew_p (info2, info1.get_sew ());
}

/* return 0 if LMUL1 == LMUL2.
   return -1 if LMUL1 < LMUL2.
   return 1 if LMUL1 > LMUL2.  */
static int
compare_lmul (vlmul_type vlmul1, vlmul_type vlmul2)
{
  if (vlmul1 == vlmul2)
    return 0;

  switch (vlmul1)
    {
    case LMUL_1:
      if (vlmul2 == LMUL_2 || vlmul2 == LMUL_4 || vlmul2 == LMUL_8)
	return 1;
      else
	return -1;
    case LMUL_2:
      if (vlmul2 == LMUL_4 || vlmul2 == LMUL_8)
	return 1;
      else
	return -1;
    case LMUL_4:
      if (vlmul2 == LMUL_8)
	return 1;
      else
	return -1;
    case LMUL_8:
      return -1;
    case LMUL_F2:
      if (vlmul2 == LMUL_1 || vlmul2 == LMUL_2 || vlmul2 == LMUL_4
	  || vlmul2 == LMUL_8)
	return 1;
      else
	return -1;
    case LMUL_F4:
      if (vlmul2 == LMUL_F2 || vlmul2 == LMUL_1 || vlmul2 == LMUL_2
	  || vlmul2 == LMUL_4 || vlmul2 == LMUL_8)
	return 1;
      else
	return -1;
    case LMUL_F8:
      return 0;
    default:
      gcc_unreachable ();
    }
}

static bool
second_lmul_less_than_first_lmul_p (const vector_insn_info &info1,
				    const vector_insn_info &info2)
{
  return compare_lmul (info2.get_vlmul (), info1.get_vlmul ()) == -1;
}

static bool
second_ratio_less_than_first_ratio_p (const vector_insn_info &info1,
				      const vector_insn_info &info2)
{
  return info2.get_ratio () < info1.get_ratio ();
}

static CONSTEXPR const demands_cond incompatible_conds[] = {
#define DEF_INCOMPATIBLE_COND(AVL1, SEW1, LMUL1, RATIO1, NONZERO_AVL1,         \
			      GE_SEW1, TAIL_POLICTY1, MASK_POLICY1, AVL2,      \
			      SEW2, LMUL2, RATIO2, NONZERO_AVL2, GE_SEW2,      \
			      TAIL_POLICTY2, MASK_POLICY2, COND)               \
  {{{AVL1, SEW1, LMUL1, RATIO1, NONZERO_AVL1, GE_SEW1, TAIL_POLICTY1,          \
     MASK_POLICY1},                                                            \
    {AVL2, SEW2, LMUL2, RATIO2, NONZERO_AVL2, GE_SEW2, TAIL_POLICTY2,          \
     MASK_POLICY2}},                                                           \
   COND},
#include "riscv-vsetvl.def"
};

static unsigned
greatest_sew (const vector_insn_info &info1, const vector_insn_info &info2)
{
  return std::max (info1.get_sew (), info2.get_sew ());
}

static unsigned
first_sew (const vector_insn_info &info1, const vector_insn_info &)
{
  return info1.get_sew ();
}

static unsigned
second_sew (const vector_insn_info &, const vector_insn_info &info2)
{
  return info2.get_sew ();
}

static vlmul_type
first_vlmul (const vector_insn_info &info1, const vector_insn_info &)
{
  return info1.get_vlmul ();
}

static vlmul_type
second_vlmul (const vector_insn_info &, const vector_insn_info &info2)
{
  return info2.get_vlmul ();
}

static unsigned
first_ratio (const vector_insn_info &info1, const vector_insn_info &)
{
  return info1.get_ratio ();
}

static unsigned
second_ratio (const vector_insn_info &, const vector_insn_info &info2)
{
  return info2.get_ratio ();
}

static vlmul_type
vlmul_for_first_sew_second_ratio (const vector_insn_info &info1,
				  const vector_insn_info &info2)
{
  return calculate_vlmul (info1.get_sew (), info2.get_ratio ());
}

static vlmul_type
vlmul_for_greatest_sew_second_ratio (const vector_insn_info &info1,
				     const vector_insn_info &info2)
{
  return calculate_vlmul (MAX (info1.get_sew (), info2.get_sew ()),
			  info2.get_ratio ());
}

static unsigned
ratio_for_second_sew_first_vlmul (const vector_insn_info &info1,
				  const vector_insn_info &info2)
{
  return calculate_ratio (info2.get_sew (), info1.get_vlmul ());
}

static CONSTEXPR const demands_fuse_rule fuse_rules[] = {
#define DEF_SEW_LMUL_FUSE_RULE(DEMAND_SEW1, DEMAND_LMUL1, DEMAND_RATIO1,       \
			       DEMAND_GE_SEW1, DEMAND_SEW2, DEMAND_LMUL2,      \
			       DEMAND_RATIO2, DEMAND_GE_SEW2, NEW_DEMAND_SEW,  \
			       NEW_DEMAND_LMUL, NEW_DEMAND_RATIO,              \
			       NEW_DEMAND_GE_SEW, NEW_SEW, NEW_VLMUL,          \
			       NEW_RATIO)                                      \
  {{{DEMAND_ANY, DEMAND_SEW1, DEMAND_LMUL1, DEMAND_RATIO1, DEMAND_ANY,         \
     DEMAND_GE_SEW1, DEMAND_ANY, DEMAND_ANY},                                  \
    {DEMAND_ANY, DEMAND_SEW2, DEMAND_LMUL2, DEMAND_RATIO2, DEMAND_ANY,         \
     DEMAND_GE_SEW2, DEMAND_ANY, DEMAND_ANY}},                                 \
   NEW_DEMAND_SEW,                                                             \
   NEW_DEMAND_LMUL,                                                            \
   NEW_DEMAND_RATIO,                                                           \
   NEW_DEMAND_GE_SEW,                                                          \
   NEW_SEW,                                                                    \
   NEW_VLMUL,                                                                  \
   NEW_RATIO},
#include "riscv-vsetvl.def"
};

static bool
always_unavailable (const vector_insn_info &, const vector_insn_info &)
{
  return true;
}

static bool
avl_unavailable_p (const vector_insn_info &info1, const vector_insn_info &info2)
{
  return !info2.compatible_avl_p (info1.get_avl_info ());
}

static bool
sew_unavailable_p (const vector_insn_info &info1, const vector_insn_info &info2)
{
  if (!info2.demand_p (DEMAND_LMUL) && !info2.demand_p (DEMAND_RATIO))
    {
      if (info2.demand_p (DEMAND_GE_SEW))
	return info1.get_sew () < info2.get_sew ();
      return info1.get_sew () != info2.get_sew ();
    }
  return true;
}

static bool
lmul_unavailable_p (const vector_insn_info &info1,
		    const vector_insn_info &info2)
{
  if (info1.get_vlmul () == info2.get_vlmul () && !info2.demand_p (DEMAND_SEW)
      && !info2.demand_p (DEMAND_RATIO))
    return false;
  return true;
}

static bool
ge_sew_unavailable_p (const vector_insn_info &info1,
		      const vector_insn_info &info2)
{
  if (!info2.demand_p (DEMAND_LMUL) && !info2.demand_p (DEMAND_RATIO)
      && info2.demand_p (DEMAND_GE_SEW))
    return info1.get_sew () < info2.get_sew ();
  return true;
}

static bool
ge_sew_lmul_unavailable_p (const vector_insn_info &info1,
			   const vector_insn_info &info2)
{
  if (!info2.demand_p (DEMAND_RATIO) && info2.demand_p (DEMAND_GE_SEW))
    return info1.get_sew () < info2.get_sew ();
  return true;
}

static bool
ge_sew_ratio_unavailable_p (const vector_insn_info &info1,
			    const vector_insn_info &info2)
{
  if (!info2.demand_p (DEMAND_LMUL))
    {
      if (info2.demand_p (DEMAND_GE_SEW))
	return info1.get_sew () < info2.get_sew ();
      /* Demand GE_SEW should be available for non-demand SEW.  */
      else if (!info2.demand_p (DEMAND_SEW))
	return false;
    }
  return true;
}

static CONSTEXPR const demands_cond unavailable_conds[] = {
#define DEF_UNAVAILABLE_COND(AVL1, SEW1, LMUL1, RATIO1, NONZERO_AVL1, GE_SEW1, \
			     TAIL_POLICTY1, MASK_POLICY1, AVL2, SEW2, LMUL2,   \
			     RATIO2, NONZERO_AVL2, GE_SEW2, TAIL_POLICTY2,     \
			     MASK_POLICY2, COND)                               \
  {{{AVL1, SEW1, LMUL1, RATIO1, NONZERO_AVL1, GE_SEW1, TAIL_POLICTY1,          \
     MASK_POLICY1},                                                            \
    {AVL2, SEW2, LMUL2, RATIO2, NONZERO_AVL2, GE_SEW2, TAIL_POLICTY2,          \
     MASK_POLICY2}},                                                           \
   COND},
#include "riscv-vsetvl.def"
};

static bool
same_sew_lmul_demand_p (const bool *dems1, const bool *dems2)
{
  return dems1[DEMAND_SEW] == dems2[DEMAND_SEW]
	 && dems1[DEMAND_LMUL] == dems2[DEMAND_LMUL]
	 && dems1[DEMAND_RATIO] == dems2[DEMAND_RATIO] && !dems1[DEMAND_GE_SEW]
	 && !dems2[DEMAND_GE_SEW];
}

static bool
propagate_avl_across_demands_p (const vector_insn_info &info1,
				const vector_insn_info &info2)
{
  if (info2.demand_p (DEMAND_AVL))
    {
      if (info2.demand_p (DEMAND_NONZERO_AVL))
	return info1.demand_p (DEMAND_AVL)
	       && !info1.demand_p (DEMAND_NONZERO_AVL) && info1.has_avl_reg ();
    }
  else
    return info1.demand_p (DEMAND_AVL) && info1.has_avl_reg ();
  return false;
}

static bool
reg_available_p (const insn_info *insn, const vector_insn_info &info)
{
  if (info.has_avl_reg () && !info.get_avl_source ())
    return false;
  insn_info *def_insn = info.get_avl_source ()->insn ();
  if (def_insn->bb () == insn->bb ())
    return before_p (def_insn, insn);
  else
    return dominated_by_p (CDI_DOMINATORS, insn->bb ()->cfg_bb (),
			   def_insn->bb ()->cfg_bb ());
}

/* Return true if the instruction support relaxed compatible check.  */
static bool
support_relaxed_compatible_p (const vector_insn_info &info1,
			      const vector_insn_info &info2)
{
  if (fault_first_load_p (info1.get_insn ()->rtl ())
      && info2.demand_p (DEMAND_AVL) && info2.has_avl_reg ()
      && info2.get_avl_source () && info2.get_avl_source ()->insn ()->is_phi ())
    {
      hash_set<set_info *> sets
	= get_all_sets (info2.get_avl_source (), true, false, false);
      for (set_info *set : sets)
	{
	  if (read_vl_insn_p (set->insn ()->rtl ()))
	    {
	      const insn_info *insn
		= get_backward_fault_first_load_insn (set->insn ());
	      if (insn == info1.get_insn ())
		return info2.compatible_vtype_p (info1);
	    }
	}
    }
  return false;
}

/* Count the number of REGNO in RINSN.  */
static int
count_regno_occurrences (rtx_insn *rinsn, unsigned int regno)
{
  int count = 0;
  extract_insn (rinsn);
  for (int i = 0; i < recog_data.n_operands; i++)
    if (refers_to_regno_p (regno, recog_data.operand[i]))
      count++;
  return count;
}

/* Return TRUE if the demands can be fused.  */
static bool
demands_can_be_fused_p (const vector_insn_info &be_fused,
			const vector_insn_info &to_fuse)
{
  return be_fused.compatible_p (to_fuse) && !be_fused.available_p (to_fuse);
}

/* Return true if we can fuse VSETVL demand info into predecessor of earliest
 * edge.  */
static bool
earliest_pred_can_be_fused_p (const bb_info *earliest_pred,
			      const vector_insn_info &earliest_info,
			      const vector_insn_info &expr, rtx *vlmax_vl)
{
  /* Backward VLMAX VL:
       bb 3:
	 vsetivli zero, 1 ... -> vsetvli t1, zero
	 vmv.s.x
       bb 5:
	 vsetvli t1, zero ... -> to be elided.
	 vlse16.v

       We should forward "t1".  */
  if (!earliest_info.has_avl_reg () && expr.has_avl_reg ())
    {
      rtx avl_or_vl_reg = expr.get_avl_or_vl_reg ();
      gcc_assert (avl_or_vl_reg);
      const insn_info *last_insn = earliest_info.get_insn ();
      /* To fuse demand on earlest edge, we make sure AVL/VL
	 didn't change from the consume insn to the predecessor
	 of the edge.  */
      for (insn_info *i = earliest_pred->end_insn ()->prev_nondebug_insn ();
	   real_insn_and_same_bb_p (i, earliest_pred)
	   && after_or_same_p (i, last_insn);
	   i = i->prev_nondebug_insn ())
	{
	  if (find_access (i->defs (), REGNO (avl_or_vl_reg)))
	    return false;
	  if (find_access (i->uses (), REGNO (avl_or_vl_reg)))
	    return false;
	}
      if (vlmax_vl && vlmax_avl_p (expr.get_avl ()))
	*vlmax_vl = avl_or_vl_reg;
    }

  return true;
}

/* Return true if the current VSETVL 1 is dominated by preceding VSETVL 2.

   VSETVL 2 dominates VSETVL 1 should satisfy this following check:

     - VSETVL 2 should have the RATIO (SEW/LMUL) with VSETVL 1.
     - VSETVL 2 is user vsetvl (vsetvl VL, AVL)
     - VSETVL 2 "VL" result is the "AVL" of VSETL1.  */
static bool
vsetvl_dominated_by_p (const basic_block cfg_bb,
		       const vector_insn_info &vsetvl1,
		       const vector_insn_info &vsetvl2, bool fuse_p)
{
  if (!vsetvl1.valid_or_dirty_p () || !vsetvl2.valid_or_dirty_p ())
    return false;
  if (!has_vl_op (vsetvl1.get_insn ()->rtl ())
      || !vsetvl_insn_p (vsetvl2.get_insn ()->rtl ()))
    return false;

  hash_set<set_info *> sets
    = get_all_sets (vsetvl1.get_avl_source (), true, false, false);
  set_info *set = get_same_bb_set (sets, cfg_bb);

  if (!vsetvl1.has_avl_reg () || vlmax_avl_p (vsetvl1.get_avl ())
      || !vsetvl2.same_vlmax_p (vsetvl1) || !set
      || set->insn () != vsetvl2.get_insn ())
    return false;

  if (fuse_p && vsetvl2.same_vtype_p (vsetvl1))
    return false;
  else if (!fuse_p && !vsetvl2.same_vtype_p (vsetvl1))
    return false;
  return true;
}

avl_info::avl_info (const avl_info &other)
{
  m_value = other.get_value ();
  m_source = other.get_source ();
}

avl_info::avl_info (rtx value_in, set_info *source_in)
  : m_value (value_in), m_source (source_in)
{}

bool
avl_info::single_source_equal_p (const avl_info &other) const
{
  set_info *set1 = m_source;
  set_info *set2 = other.get_source ();
  insn_info *insn1 = extract_single_source (set1);
  insn_info *insn2 = extract_single_source (set2);
  if (!insn1 || !insn2)
    return false;
  return source_equal_p (insn1, insn2);
}

bool
avl_info::multiple_source_equal_p (const avl_info &other) const
{
  /* When the def info is same in RTL_SSA namespace, it's safe
     to consider they are avl compatible.  */
  if (m_source == other.get_source ())
    return true;

  /* We only consider handle PHI node.  */
  if (!m_source->insn ()->is_phi () || !other.get_source ()->insn ()->is_phi ())
    return false;

  phi_info *phi1 = as_a<phi_info *> (m_source);
  phi_info *phi2 = as_a<phi_info *> (other.get_source ());

  if (phi1->is_degenerate () && phi2->is_degenerate ())
    {
      /* Degenerate PHI means the PHI node only have one input.  */

      /* If both PHI nodes have the same single input in use list.
	 We consider they are AVL compatible.  */
      if (phi1->input_value (0) == phi2->input_value (0))
	return true;
    }
  /* TODO: We can support more optimization cases in the future.  */
  return false;
}

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

  if (GET_CODE (m_value) != GET_CODE (other.get_value ()))
    return false;

  /* Handle CONST_INT AVL.  */
  if (CONST_INT_P (m_value))
    return INTVAL (m_value) == INTVAL (other.get_value ());

  /* Handle VLMAX AVL.  */
  if (vlmax_avl_p (m_value))
    return vlmax_avl_p (other.get_value ());
  if (vlmax_avl_p (other.get_value ()))
    return false;

  /* If any source is undef value, we think they are not equal.  */
  if (!m_source || !other.get_source ())
    return false;

  /* If both sources are single source (defined by a single real RTL)
     and their definitions are same.  */
  if (single_source_equal_p (other))
    return true;

  return multiple_source_equal_p (other);
}

bool
avl_info::operator!= (const avl_info &other) const
{
  return !(*this == other);
}

bool
avl_info::has_non_zero_avl () const
{
  if (has_avl_imm ())
    return INTVAL (get_value ()) > 0;
  if (has_avl_reg ())
    return vlmax_avl_p (get_value ());
  return false;
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
  return same_avl_p (other) && m_sew == other.get_sew ()
	 && m_vlmul == other.get_vlmul () && m_ta == other.get_ta ()
	 && m_ma == other.get_ma () && m_ratio == other.get_ratio ();
}

bool
vl_vtype_info::operator!= (const vl_vtype_info &other) const
{
  return !(*this == other);
}

bool
vl_vtype_info::same_avl_p (const vl_vtype_info &other) const
{
  /* We need to compare both RTL and SET. If both AVL are CONST_INT.
     For example, const_int 3 and const_int 4, we need to compare
     RTL. If both AVL are REG and their REGNO are same, we need to
     compare SET.  */
  return get_avl () == other.get_avl ()
	 && get_avl_source () == other.get_avl_source ();
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
vector_insn_info::operator>= (const vector_insn_info &other) const
{
  if (support_relaxed_compatible_p (*this, other))
    {
      unsigned array_size = sizeof (unavailable_conds) / sizeof (demands_cond);
      /* Bypass AVL unavailable cases.  */
      for (unsigned i = 2; i < array_size; i++)
	if (unavailable_conds[i].pair.match_cond_p (this->get_demands (),
						    other.get_demands ())
	    && unavailable_conds[i].incompatible_p (*this, other))
	  return false;
      return true;
    }

  if (!other.compatible_p (static_cast<const vl_vtype_info &> (*this)))
    return false;
  if (!this->compatible_p (static_cast<const vl_vtype_info &> (other)))
    return true;

  if (*this == other)
    return true;

  for (const auto &cond : unavailable_conds)
    if (cond.pair.match_cond_p (this->get_demands (), other.get_demands ())
	&& cond.incompatible_p (*this, other))
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

  /* We should consider different INSN demands as different
     expression.  Otherwise, we will be doing incorrect vsetvl
     elimination.  */
  if (m_insn != other.get_insn ())
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
  if (optimize == 0 && !has_vtype_op (rinsn))
    return;
  gcc_assert (!vsetvl_discard_result_insn_p (rinsn));
  m_state = VALID;
  extract_insn_cached (rinsn);
  rtx avl = ::get_avl (rinsn);
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
  if (!vector_config_insn_p (insn->rtl ()) && !has_vtype_op (insn->rtl ())
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
      if (!ignore_vlmul_insn_p (insn->rtl ()))
	m_demands[DEMAND_LMUL] = true;
    }

  if (get_attr_ta (insn->rtl ()) != INVALID_ATTRIBUTE)
    m_demands[DEMAND_TAIL_POLICY] = true;
  if (get_attr_ma (insn->rtl ()) != INVALID_ATTRIBUTE)
    m_demands[DEMAND_MASK_POLICY] = true;

  if (vector_config_insn_p (insn->rtl ()))
    return;

  if (scalar_move_insn_p (insn->rtl ()))
    {
      if (m_avl.has_non_zero_avl ())
	m_demands[DEMAND_NONZERO_AVL] = true;
      if (m_ta)
	m_demands[DEMAND_GE_SEW] = true;
    }

  if (!m_avl.has_avl_reg () || vlmax_avl_p (get_avl ()) || !m_avl.get_source ())
    return;
  if (!m_avl.get_source ()->insn ()->is_real ()
      && !m_avl.get_source ()->insn ()->is_phi ())
    return;

  insn_info *def_insn = extract_single_source (m_avl.get_source ());
  if (!def_insn || !vsetvl_insn_p (def_insn->rtl ()))
    return;

  vector_insn_info new_info;
  new_info.parse_insn (def_insn);
  if (!same_vlmax_p (new_info) && !scalar_move_insn_p (insn->rtl ()))
    return;

  if (new_info.has_avl ())
    {
      if (new_info.has_avl_imm ())
	set_avl_info (avl_info (new_info.get_avl (), nullptr));
      else
	{
	  if (vlmax_avl_p (new_info.get_avl ()))
	    set_avl_info (avl_info (new_info.get_avl (), get_avl_source ()));
	  else
	    {
	      /* Conservatively propagate non-VLMAX AVL of user vsetvl:
		 1. The user vsetvl should be same block with the rvv insn.
		 2. The user vsetvl is the only def insn of rvv insn.
		 3. The AVL is not modified between def-use chain.
		 4. The VL is only used by insn within EBB.
	       */
	      bool modified_p = false;
	      for (insn_info *i = def_insn->next_nondebug_insn ();
		   real_insn_and_same_bb_p (i, get_insn ()->bb ());
		   i = i->next_nondebug_insn ())
		{
		  /* Consider this following sequence:

		       insn 1: vsetvli a5,a3,e8,mf4,ta,mu
		       insn 2: vsetvli zero,a5,e32,m1,ta,ma
		       ...
		       vle32.v v1,0(a1)
		       vsetvli a2,zero,e32,m1,ta,ma
		       vadd.vv v1,v1,v1
		       vsetvli zero,a5,e32,m1,ta,ma
		       vse32.v v1,0(a0)
		       ...
		       insn 3: sub     a3,a3,a5
		       ...

		       We can local AVL propagate "a3" from insn 1 to insn 2
		       if no insns between insn 1 and insn 2 modify "a3 even
		       though insn 3 modifies "a3".
		       Otherwise, we can't perform local AVL propagation.

		       Early break if we reach the insn 2.  */
		  if (!before_p (i, insn))
		    break;
		  if (find_access (i->defs (), REGNO (new_info.get_avl ())))
		    {
		      modified_p = true;
		      break;
		    }
		}

	      bool has_live_out_use = false;
	      for (use_info *use : m_avl.get_source ()->all_uses ())
		{
		  if (use->is_live_out_use ())
		    {
		      has_live_out_use = true;
		      break;
		    }
		}
	      if (!modified_p && !has_live_out_use
		  && def_insn == m_avl.get_source ()->insn ()
		  && m_insn->bb () == def_insn->bb ())
		set_avl_info (new_info.get_avl_info ());
	    }
	}
    }

  if (scalar_move_insn_p (insn->rtl ()) && m_avl.has_non_zero_avl ())
    m_demands[DEMAND_NONZERO_AVL] = true;
}

bool
vector_insn_info::compatible_p (const vector_insn_info &other) const
{
  gcc_assert (valid_or_dirty_p () && other.valid_or_dirty_p ()
	      && "Can't compare invalid demanded infos");

  for (const auto &cond : incompatible_conds)
    if (cond.dual_incompatible_p (*this, other))
      return false;
  return true;
}

bool
vector_insn_info::skip_avl_compatible_p (const vector_insn_info &other) const
{
  gcc_assert (valid_or_dirty_p () && other.valid_or_dirty_p ()
	      && "Can't compare invalid demanded infos");
  unsigned array_size = sizeof (incompatible_conds) / sizeof (demands_cond);
  /* Bypass AVL incompatible cases.  */
  for (unsigned i = 1; i < array_size; i++)
    if (incompatible_conds[i].dual_incompatible_p (*this, other))
      return false;
  return true;
}

bool
vector_insn_info::compatible_avl_p (const vl_vtype_info &other) const
{
  gcc_assert (valid_or_dirty_p () && "Can't compare invalid vl_vtype_info");
  gcc_assert (!unknown_p () && "Can't compare AVL in unknown state");
  if (!demand_p (DEMAND_AVL))
    return true;
  if (demand_p (DEMAND_NONZERO_AVL) && other.has_non_zero_avl ())
    return true;
  return get_avl_info () == other.get_avl_info ();
}

bool
vector_insn_info::compatible_avl_p (const avl_info &other) const
{
  gcc_assert (valid_or_dirty_p () && "Can't compare invalid vl_vtype_info");
  gcc_assert (!unknown_p () && "Can't compare AVL in unknown state");
  gcc_assert (demand_p (DEMAND_AVL) && "Can't compare AVL undemand state");
  if (!demand_p (DEMAND_AVL))
    return true;
  if (demand_p (DEMAND_NONZERO_AVL) && other.has_non_zero_avl ())
    return true;
  return get_avl_info () == other;
}

bool
vector_insn_info::compatible_vtype_p (const vl_vtype_info &other) const
{
  gcc_assert (valid_or_dirty_p () && "Can't compare invalid vl_vtype_info");
  gcc_assert (!unknown_p () && "Can't compare VTYPE in unknown state");
  if (demand_p (DEMAND_SEW))
    {
      if (!demand_p (DEMAND_GE_SEW) && m_sew != other.get_sew ())
	return false;
      if (demand_p (DEMAND_GE_SEW) && m_sew > other.get_sew ())
	return false;
    }
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

bool
vector_insn_info::available_p (const vector_insn_info &other) const
{
  return *this >= other;
}

void
vector_insn_info::fuse_avl (const vector_insn_info &info1,
			    const vector_insn_info &info2)
{
  set_insn (info1.get_insn ());
  if (info1.demand_p (DEMAND_AVL))
    {
      if (info1.demand_p (DEMAND_NONZERO_AVL))
	{
	  if (info2.demand_p (DEMAND_AVL)
	      && !info2.demand_p (DEMAND_NONZERO_AVL))
	    {
	      set_avl_info (info2.get_avl_info ());
	      set_demand (DEMAND_AVL, true);
	      set_demand (DEMAND_NONZERO_AVL, false);
	      return;
	    }
	}
      set_avl_info (info1.get_avl_info ());
      set_demand (DEMAND_NONZERO_AVL, info1.demand_p (DEMAND_NONZERO_AVL));
    }
  else
    {
      set_avl_info (info2.get_avl_info ());
      set_demand (DEMAND_NONZERO_AVL, info2.demand_p (DEMAND_NONZERO_AVL));
    }
  set_demand (DEMAND_AVL,
	      info1.demand_p (DEMAND_AVL) || info2.demand_p (DEMAND_AVL));
}

void
vector_insn_info::fuse_sew_lmul (const vector_insn_info &info1,
				 const vector_insn_info &info2)
{
  /* We need to fuse sew && lmul according to demand info:

     1. GE_SEW.
     2. SEW.
     3. LMUL.
     4. RATIO.  */
  if (same_sew_lmul_demand_p (info1.get_demands (), info2.get_demands ()))
    {
      set_demand (DEMAND_SEW, info2.demand_p (DEMAND_SEW));
      set_demand (DEMAND_LMUL, info2.demand_p (DEMAND_LMUL));
      set_demand (DEMAND_RATIO, info2.demand_p (DEMAND_RATIO));
      set_demand (DEMAND_GE_SEW, info2.demand_p (DEMAND_GE_SEW));
      set_sew (info2.get_sew ());
      set_vlmul (info2.get_vlmul ());
      set_ratio (info2.get_ratio ());
      return;
    }
  for (const auto &rule : fuse_rules)
    {
      if (rule.pair.match_cond_p (info1.get_demands (), info2.get_demands ()))
	{
	  set_demand (DEMAND_SEW, rule.demand_sew_p);
	  set_demand (DEMAND_LMUL, rule.demand_lmul_p);
	  set_demand (DEMAND_RATIO, rule.demand_ratio_p);
	  set_demand (DEMAND_GE_SEW, rule.demand_ge_sew_p);
	  set_sew (rule.new_sew (info1, info2));
	  set_vlmul (rule.new_vlmul (info1, info2));
	  set_ratio (rule.new_ratio (info1, info2));
	  return;
	}
      if (rule.pair.match_cond_p (info2.get_demands (), info1.get_demands ()))
	{
	  set_demand (DEMAND_SEW, rule.demand_sew_p);
	  set_demand (DEMAND_LMUL, rule.demand_lmul_p);
	  set_demand (DEMAND_RATIO, rule.demand_ratio_p);
	  set_demand (DEMAND_GE_SEW, rule.demand_ge_sew_p);
	  set_sew (rule.new_sew (info2, info1));
	  set_vlmul (rule.new_vlmul (info2, info1));
	  set_ratio (rule.new_ratio (info2, info1));
	  return;
	}
    }
  gcc_unreachable ();
}

void
vector_insn_info::fuse_tail_policy (const vector_insn_info &info1,
				    const vector_insn_info &info2)
{
  if (info1.demand_p (DEMAND_TAIL_POLICY))
    {
      set_ta (info1.get_ta ());
      demand (DEMAND_TAIL_POLICY);
    }
  else if (info2.demand_p (DEMAND_TAIL_POLICY))
    {
      set_ta (info2.get_ta ());
      demand (DEMAND_TAIL_POLICY);
    }
  else
    set_ta (get_default_ta ());
}

void
vector_insn_info::fuse_mask_policy (const vector_insn_info &info1,
				    const vector_insn_info &info2)
{
  if (info1.demand_p (DEMAND_MASK_POLICY))
    {
      set_ma (info1.get_ma ());
      demand (DEMAND_MASK_POLICY);
    }
  else if (info2.demand_p (DEMAND_MASK_POLICY))
    {
      set_ma (info2.get_ma ());
      demand (DEMAND_MASK_POLICY);
    }
  else
    set_ma (get_default_ma ());
}

vector_insn_info
vector_insn_info::local_merge (const vector_insn_info &merge_info) const
{
  if (!vsetvl_insn_p (get_insn ()->rtl ()) && *this != merge_info)
    gcc_assert (this->compatible_p (merge_info)
		&& "Can't merge incompatible demanded infos");

  vector_insn_info new_info;
  new_info.set_valid ();
  /* For local backward data flow, we always update INSN && AVL as the
     latest INSN and AVL so that we can keep track status of each INSN.  */
  new_info.fuse_avl (merge_info, *this);
  new_info.fuse_sew_lmul (*this, merge_info);
  new_info.fuse_tail_policy (*this, merge_info);
  new_info.fuse_mask_policy (*this, merge_info);
  return new_info;
}

vector_insn_info
vector_insn_info::global_merge (const vector_insn_info &merge_info,
				unsigned int bb_index) const
{
  if (!vsetvl_insn_p (get_insn ()->rtl ()) && *this != merge_info)
    gcc_assert (this->compatible_p (merge_info)
		&& "Can't merge incompatible demanded infos");

  vector_insn_info new_info;
  new_info.set_valid ();

  /* For global data flow, we should keep original INSN and AVL if they
  valid since we should keep the life information of each block.

  For example:
    bb 0 -> bb 1.
  We should keep INSN && AVL of bb 1 since we will eventually emit
  vsetvl instruction according to INSN and AVL of bb 1.  */
  new_info.fuse_avl (*this, merge_info);
  /* Recompute the AVL source whose block index is equal to BB_INDEX.  */
  if (new_info.get_avl_source ()
      && new_info.get_avl_source ()->insn ()->is_phi ()
      && new_info.get_avl_source ()->bb ()->index () != bb_index)
    {
      hash_set<set_info *> sets
	= get_all_sets (new_info.get_avl_source (), true, true, true);
      new_info.set_avl_source (nullptr);
      bool can_find_set_p = false;
      set_info *first_set = nullptr;
      for (set_info *set : sets)
	{
	  if (!first_set)
	    first_set = set;
	  if (set->bb ()->index () == bb_index)
	    {
	      gcc_assert (!can_find_set_p);
	      new_info.set_avl_source (set);
	      can_find_set_p = true;
	    }
	}
      if (!can_find_set_p && sets.elements () == 1
	  && first_set->insn ()->is_real ())
	new_info.set_avl_source (first_set);
    }

  /* Make sure VLMAX AVL always has a set_info the get VL.  */
  if (vlmax_avl_p (new_info.get_avl ()))
    {
      if (this->get_avl_source ())
	new_info.set_avl_source (this->get_avl_source ());
      else
	{
	  gcc_assert (merge_info.get_avl_source ());
	  new_info.set_avl_source (merge_info.get_avl_source ());
	}
    }

  new_info.fuse_sew_lmul (*this, merge_info);
  new_info.fuse_tail_policy (*this, merge_info);
  new_info.fuse_mask_policy (*this, merge_info);
  return new_info;
}

/* Wrapper helps to return the AVL or VL operand for the
   vector_insn_info. Return AVL if the AVL is not VLMAX.
   Otherwise, return the VL operand.  */
rtx
vector_insn_info::get_avl_or_vl_reg (void) const
{
  gcc_assert (has_avl_reg ());
  if (!vlmax_avl_p (get_avl ()))
    return get_avl ();

  rtx_insn *rinsn = get_insn ()->rtl ();
  if (has_vl_op (rinsn) || vsetvl_insn_p (rinsn))
    {
      rtx vl = ::get_vl (rinsn);
      /* For VLMAX, we should make sure we get the
	 REG to emit 'vsetvl VL,zero' since the 'VL'
	 should be the REG according to RVV ISA.  */
      if (REG_P (vl))
	return vl;
    }

  /* We always has avl_source if it is VLMAX AVL.  */
  gcc_assert (get_avl_source ());
  return get_avl_reg_rtx ();
}

bool
vector_insn_info::update_fault_first_load_avl (insn_info *insn)
{
  // Update AVL to vl-output of the fault first load.
  const insn_info *read_vl = get_forward_read_vl_insn (insn);
  if (read_vl)
    {
      rtx vl = SET_DEST (PATTERN (read_vl->rtl ()));
      def_info *def = find_access (read_vl->defs (), REGNO (vl));
      set_info *set = safe_dyn_cast<set_info *> (def);
      set_avl_info (avl_info (vl, set));
      set_insn (insn);
      return true;
    }
  return false;
}

static const char *
vlmul_to_str (vlmul_type vlmul)
{
  switch (vlmul)
    {
    case LMUL_1:
      return "m1";
    case LMUL_2:
      return "m2";
    case LMUL_4:
      return "m4";
    case LMUL_8:
      return "m8";
    case LMUL_RESERVED:
      return "INVALID LMUL";
    case LMUL_F8:
      return "mf8";
    case LMUL_F4:
      return "mf4";
    case LMUL_F2:
      return "mf2";

    default:
      gcc_unreachable ();
    }
}

static const char *
policy_to_str (bool agnostic_p)
{
  return agnostic_p ? "agnostic" : "undisturbed";
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
  fprintf (file, "%d(DEMAND_NONZERO_AVL),", demand_p (DEMAND_NONZERO_AVL));
  fprintf (file, "%d(SEW),", demand_p (DEMAND_SEW));
  fprintf (file, "%d(DEMAND_GE_SEW),", demand_p (DEMAND_GE_SEW));
  fprintf (file, "%d(LMUL),", demand_p (DEMAND_LMUL));
  fprintf (file, "%d(RATIO),", demand_p (DEMAND_RATIO));
  fprintf (file, "%d(TAIL_POLICY),", demand_p (DEMAND_TAIL_POLICY));
  fprintf (file, "%d(MASK_POLICY)}\n", demand_p (DEMAND_MASK_POLICY));

  fprintf (file, "AVL=");
  print_rtl_single (file, get_avl ());
  fprintf (file, "SEW=%d,", get_sew ());
  fprintf (file, "VLMUL=%s,", vlmul_to_str (get_vlmul ()));
  fprintf (file, "RATIO=%d,", get_ratio ());
  fprintf (file, "TAIL_POLICY=%s,", policy_to_str (get_ta ()));
  fprintf (file, "MASK_POLICY=%s", policy_to_str (get_ma ()));
  fprintf (file, "]\n");

  if (valid_p ())
    {
      if (get_insn ())
	{
	  fprintf (file, "The real INSN=");
	  print_rtl_single (file, get_insn ()->rtl ());
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
  vector_antin = nullptr;
  vector_antout = nullptr;
  vector_earliest = nullptr;
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
	  vector_block_infos[bb->index ()].probability = profile_probability ();
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
    if (info.available_p (*vector_exprs[i]))
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

/* Return TRUE if the incoming vector configuration state
   to CFG_BB is compatible with the vector configuration
   state in CFG_BB, FALSE otherwise.  */
bool
vector_infos_manager::all_avail_in_compatible_p (const basic_block cfg_bb) const
{
  const auto &info = vector_block_infos[cfg_bb->index].local_dem;
  sbitmap avin = vector_avin[cfg_bb->index];
  unsigned int bb_index;
  sbitmap_iterator sbi;
  EXECUTE_IF_SET_IN_BITMAP (avin, 0, bb_index, sbi)
    {
      const auto &avin_info
	= static_cast<const vl_vtype_info &> (*vector_exprs[bb_index]);
      if (!info.compatible_p (avin_info))
	return false;
    }
  return true;
}

bool
vector_infos_manager::all_same_avl_p (const basic_block cfg_bb,
				      sbitmap bitdata) const
{
  if (bitmap_empty_p (bitdata))
    return false;

  const auto &block_info = vector_block_infos[cfg_bb->index];
  if (!block_info.local_dem.demand_p (DEMAND_AVL))
    return true;

  avl_info avl = block_info.local_dem.get_avl_info ();
  unsigned int bb_index;
  sbitmap_iterator sbi;

  EXECUTE_IF_SET_IN_BITMAP (bitdata, 0, bb_index, sbi)
    {
      if (vector_exprs[bb_index]->get_avl_info () != avl)
	return false;
    }
  return true;
}

bool
vector_infos_manager::earliest_fusion_worthwhile_p (
  const basic_block cfg_bb) const
{
  edge e;
  edge_iterator ei;
  profile_probability prob = profile_probability::uninitialized ();
  FOR_EACH_EDGE (e, ei, cfg_bb->succs)
    {
      if (prob == profile_probability::uninitialized ())
	prob = vector_block_infos[e->dest->index].probability;
      else if (prob == vector_block_infos[e->dest->index].probability)
	continue;
      else
	/* We pick the highest probability among those incompatible VSETVL
	   infos. When all incompatible VSTEVL infos have same probability, we
	   don't pick any of them.  */
	return true;
    }
  return false;
}

bool
vector_infos_manager::vsetvl_dominated_by_all_preds_p (
  const basic_block cfg_bb, const vector_insn_info &info) const
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
    {
      const auto &reaching_out = vector_block_infos[e->src->index].reaching_out;
      if (e->src->index == cfg_bb->index && reaching_out.compatible_p (info))
	continue;
      if (!vsetvl_dominated_by_p (e->src, info, reaching_out, false))
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

  gcc_assert (to_refine_vsetvls.is_empty ());
  gcc_assert (to_delete_vsetvls.is_empty ());
  if (optimize > 0)
    free_bitmap_vectors ();
}

void
vector_infos_manager::create_bitmap_vectors (void)
{
  /* Create the bitmap vectors.  */
  vector_antic = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				       vector_exprs.length ());
  vector_transp = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
					vector_exprs.length ());
  vector_comp = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				      vector_exprs.length ());
  vector_avin = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				      vector_exprs.length ());
  vector_avout = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				       vector_exprs.length ());
  vector_kill = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				      vector_exprs.length ());
  vector_antin = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
				       vector_exprs.length ());
  vector_antout = sbitmap_vector_alloc (last_basic_block_for_fn (cfun),
					vector_exprs.length ());

  bitmap_vector_ones (vector_transp, last_basic_block_for_fn (cfun));
  bitmap_vector_clear (vector_antic, last_basic_block_for_fn (cfun));
  bitmap_vector_clear (vector_comp, last_basic_block_for_fn (cfun));
  vector_edge_list = create_edge_list ();
  vector_earliest = sbitmap_vector_alloc (NUM_EDGES (vector_edge_list),
					  vector_exprs.length ());
}

void
vector_infos_manager::free_bitmap_vectors (void)
{
  /* Finished. Free up all the things we've allocated.  */
  free_edge_list (vector_edge_list);
  if (vector_del)
    sbitmap_vector_free (vector_del);
  if (vector_insert)
    sbitmap_vector_free (vector_insert);
  if (vector_kill)
    sbitmap_vector_free (vector_kill);
  if (vector_antic)
    sbitmap_vector_free (vector_antic);
  if (vector_transp)
    sbitmap_vector_free (vector_transp);
  if (vector_comp)
    sbitmap_vector_free (vector_comp);
  if (vector_avin)
    sbitmap_vector_free (vector_avin);
  if (vector_avout)
    sbitmap_vector_free (vector_avout);
  if (vector_antin)
    sbitmap_vector_free (vector_antin);
  if (vector_antout)
    sbitmap_vector_free (vector_antout);
  if (vector_earliest)
    sbitmap_vector_free (vector_earliest);

  vector_edge_list = nullptr;
  vector_kill = nullptr;
  vector_del = nullptr;
  vector_insert = nullptr;
  vector_antic = nullptr;
  vector_transp = nullptr;
  vector_comp = nullptr;
  vector_avin = nullptr;
  vector_avout = nullptr;
  vector_antin = nullptr;
  vector_antout = nullptr;
  vector_earliest = nullptr;
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
      fprintf (file, "<Probability>=");
      vector_block_infos[cfg_bb->index].probability.dump (file);
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

      fprintf (file, "<ANTIN>=");
      if (vector_antin == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_antin[cfg_bb->index]);

      fprintf (file, "<ANTOUT>=");
      if (vector_antout == nullptr)
	fprintf (file, "(nil)\n");
      else
	dump_bitmap_file (file, vector_antout[cfg_bb->index]);
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

  for (size_t i = 0; i < vector_exprs.length (); i++)
    {
      for (int ed = 0; ed < NUM_EDGES (vector_edge_list); ed++)
	{
	  edge eg = INDEX_EDGE (vector_edge_list, ed);
	  if (vector_insert)
	    {
	      if (bitmap_bit_p (vector_insert[ed], i))
		{
		  fprintf (file,
			   "\nGlobal LCM (Lazy code motion) INSERT info:\n");
		  fprintf (file,
			   "INSERT edge %d from <bb %d> to <bb %d> for VSETVL "
			   "expr[%ld]\n",
			   ed, eg->src->index, eg->dest->index, i);
		}
	    }
	  else
	    {
	      if (bitmap_bit_p (vector_earliest[ed], i))
		{
		  fprintf (file,
			   "\nGlobal LCM (Lazy code motion) EARLIEST info:\n");
		  fprintf (
		    file,
		    "EARLIEST edge %d from <bb %d> to <bb %d> for VSETVL "
		    "expr[%ld]\n",
		    ed, eg->src->index, eg->dest->index, i);
		}
	    }
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
  vector_infos_manager *m_vector_manager;

  const vector_insn_info &get_vector_info (const rtx_insn *) const;
  const vector_insn_info &get_vector_info (const insn_info *) const;
  const vector_block_info &get_block_info (const basic_block) const;
  const vector_block_info &get_block_info (const bb_info *) const;
  vector_block_info &get_block_info (const basic_block);
  vector_block_info &get_block_info (const bb_info *);
  void update_vector_info (const insn_info *, const vector_insn_info &);
  void update_block_info (int, profile_probability, const vector_insn_info &);

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
  bool earliest_fusion (void);
  void vsetvl_fusion (void);

  /* Phase 4.  */
  void prune_expressions (void);
  void compute_local_properties (void);
  bool can_refine_vsetvl_p (const basic_block, const vector_insn_info &) const;
  void refine_vsetvls (void) const;
  void cleanup_vsetvls (void);
  bool commit_vsetvls (void);
  void pre_vsetvl (void);

  /* Phase 5.  */
  rtx_insn *get_vsetvl_at_end (const bb_info *, vector_insn_info *) const;
  void local_eliminate_vsetvl_insn (const bb_info *) const;
  bool global_eliminate_vsetvl_insn (const bb_info *) const;
  void ssa_post_optimization (void) const;

  /* Phase 6.  */
  void df_post_optimization (void) const;

  void init (void);
  void done (void);
  void compute_probabilities (void);

public:
  pass_vsetvl (gcc::context *ctxt) : rtl_opt_pass (pass_data_vsetvl, ctxt) {}

  /* opt_pass methods: */
  virtual bool gate (function *) final override { return TARGET_VECTOR; }
  virtual unsigned int execute (function *) final override;
}; // class pass_vsetvl

const vector_insn_info &
pass_vsetvl::get_vector_info (const rtx_insn *i) const
{
  return m_vector_manager->vector_insn_infos[INSN_UID (i)];
}

const vector_insn_info &
pass_vsetvl::get_vector_info (const insn_info *i) const
{
  return m_vector_manager->vector_insn_infos[i->uid ()];
}

const vector_block_info &
pass_vsetvl::get_block_info (const basic_block bb) const
{
  return m_vector_manager->vector_block_infos[bb->index];
}

const vector_block_info &
pass_vsetvl::get_block_info (const bb_info *bb) const
{
  return m_vector_manager->vector_block_infos[bb->index ()];
}

vector_block_info &
pass_vsetvl::get_block_info (const basic_block bb)
{
  return m_vector_manager->vector_block_infos[bb->index];
}

vector_block_info &
pass_vsetvl::get_block_info (const bb_info *bb)
{
  return m_vector_manager->vector_block_infos[bb->index ()];
}

void
pass_vsetvl::update_vector_info (const insn_info *i,
				 const vector_insn_info &new_info)
{
  m_vector_manager->vector_insn_infos[i->uid ()] = new_info;
}

void
pass_vsetvl::update_block_info (int index, profile_probability prob,
				const vector_insn_info &new_info)
{
  m_vector_manager->vector_block_infos[index].probability = prob;
  if (m_vector_manager->vector_block_infos[index].local_dem
      == m_vector_manager->vector_block_infos[index].reaching_out)
    m_vector_manager->vector_block_infos[index].local_dem = new_info;
  m_vector_manager->vector_block_infos[index].reaching_out = new_info;
}

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
	      const auto info = get_vector_info (rinsn);
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
      auto &info = get_vector_info (insn);

      if (info.uninit_p ())
	/* If it is uninitialized, propagate it directly.  */
	update_vector_info (insn, change);
      else if (info.unknown_p ())
	change = info;
      else
	{
	  gcc_assert (info.valid_p () && "Unexpected Invalid demanded info");
	  if (change.valid_p ())
	    {
	      if (!(propagate_avl_across_demands_p (change, info)
		    && !reg_available_p (insn, change))
		  && change.compatible_p (info))
		{
		  update_vector_info (insn, change.local_merge (info));
		  /* Fix PR109399, we should update user vsetvl instruction
		     if there is a change in demand fusion.  */
		  if (vsetvl_insn_p (insn->rtl ()))
		    change_vsetvl_insn (insn, info);
		}
	    }
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

  if (require.compatible_p (static_cast<const vl_vtype_info &> (curr_info)))
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

  const vector_insn_info require = get_vector_info (insn);
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
      info = get_vector_info (insn);
      return;
    }

  if (fault_first_load_p (insn->rtl ())
      && info.update_fault_first_load_avl (insn))
    return;

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
      enum vsetvl_type type = NUM_VSETVL_TYPE;
      transfer_before (curr_info, insn);

      if (has_vtype_op (insn->rtl ()))
	{
	  if (static_cast<const vl_vtype_info &> (prev_info)
	      != static_cast<const vl_vtype_info &> (curr_info))
	    {
	      const auto require = get_vector_info (insn);
	      if (!require.compatible_p (
		    static_cast<const vl_vtype_info &> (prev_info)))
		type = insert_vsetvl (EMIT_BEFORE, insn->rtl (), require,
				      prev_info);
	    }
	}

      /* Fix the issue of following sequence:
	 vsetivli zero, 5
	 ....
	 vsetvli zero, zero
	 vmv.x.s (demand AVL = 8).
	 ....
	 incorrect: vsetvli zero, zero ===> Since the curr_info is AVL = 8.
	 correct: vsetivli zero, 8
	 vadd (demand AVL = 8).  */
      if (type == VSETVL_VTYPE_CHANGE_ONLY)
	{
	  /* Update the curr_info to be real correct AVL.  */
	  curr_info.set_avl_info (prev_info.get_avl_info ());
	}
      transfer_after (curr_info, insn);
    }

  block_info.reaching_out = curr_info;
}

/* Assemble the candidates expressions for LCM.  */
void
pass_vsetvl::prune_expressions (void)
{
  for (const bb_info *bb : crtl->ssa->bbs ())
    {
      if (m_vector_manager->vector_block_infos[bb->index ()]
	    .local_dem.valid_or_dirty_p ())
	m_vector_manager->create_expr (
	  m_vector_manager->vector_block_infos[bb->index ()].local_dem);
      if (m_vector_manager->vector_block_infos[bb->index ()]
	    .reaching_out.valid_or_dirty_p ())
	m_vector_manager->create_expr (
	  m_vector_manager->vector_block_infos[bb->index ()].reaching_out);
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

/* Compute the local properties of each recorded expression.

   Local properties are those that are defined by the block, irrespective of
   other blocks.

   An expression is transparent in a block if its operands are not modified
   in the block.

   An expression is computed (locally available) in a block if it is computed
   at least once and expression would contain the same value if the
   computation was moved to the end of the block.

   An expression is locally anticipatable in a block if it is computed at
   least once and expression would contain the same value if the computation
   was moved to the beginning of the block.  */
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
  for (const bb_info *bb : crtl->ssa->bbs ())
    {
      unsigned int curr_bb_idx = bb->index ();
      if (curr_bb_idx == ENTRY_BLOCK || curr_bb_idx == EXIT_BLOCK)
	continue;
      const auto local_dem
	= m_vector_manager->vector_block_infos[curr_bb_idx].local_dem;
      const auto reaching_out
	= m_vector_manager->vector_block_infos[curr_bb_idx].reaching_out;

      /* Compute transparent.  */
      for (size_t i = 0; i < m_vector_manager->vector_exprs.length (); i++)
	{
	  const auto *expr = m_vector_manager->vector_exprs[i];
	  if (local_dem.valid_or_dirty_p () || local_dem.unknown_p ())
	    bitmap_clear_bit (m_vector_manager->vector_transp[curr_bb_idx], i);
	  else if (expr->has_avl_reg ())
	    {
	      rtx reg = expr->get_avl_or_vl_reg ();
	      for (const insn_info *insn : bb->real_nondebug_insns ())
		{
		  if (find_access (insn->defs (), REGNO (reg)))
		    {
		      bitmap_clear_bit (
			m_vector_manager->vector_transp[curr_bb_idx], i);
		      break;
		    }
		  else if (vlmax_avl_p (expr->get_avl ())
			   && find_access (insn->uses (), REGNO (reg)))
		    {
		      bitmap_clear_bit (
			m_vector_manager->vector_transp[curr_bb_idx], i);
		      break;
		    }
		}
	    }
	}

      /* Compute anticipatable occurrences.  */
      if (local_dem.valid_or_dirty_p ())
	if (anticipatable_occurrence_p (bb, local_dem))
	  bitmap_set_bit (m_vector_manager->vector_antic[curr_bb_idx],
			  m_vector_manager->get_expr_id (local_dem));

      /* Compute available occurrences.  */
      if (reaching_out.valid_or_dirty_p ())
	{
	  auto_vec<size_t> available_list
	    = m_vector_manager->get_all_available_exprs (reaching_out);
	  for (size_t i = 0; i < available_list.length (); i++)
	    {
	      const vector_insn_info *expr
		= m_vector_manager->vector_exprs[available_list[i]];
	      if (available_occurrence_p (bb, *expr))
		bitmap_set_bit (m_vector_manager->vector_comp[curr_bb_idx],
				available_list[i]);
	    }
	}

      if (loop_basic_block_p (bb->cfg_bb ()) && local_dem.valid_or_dirty_p ()
	  && reaching_out.valid_or_dirty_p ()
	  && !local_dem.compatible_p (reaching_out))
	bitmap_clear_bit (m_vector_manager->vector_antic[curr_bb_idx],
			  m_vector_manager->get_expr_id (local_dem));
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

/* Fuse demand info for earliest edge.  */
bool
pass_vsetvl::earliest_fusion (void)
{
  bool changed_p = false;
  for (int ed = 0; ed < NUM_EDGES (m_vector_manager->vector_edge_list); ed++)
    {
      for (size_t i = 0; i < m_vector_manager->vector_exprs.length (); i++)
	{
	  auto &expr = *m_vector_manager->vector_exprs[i];
	  if (expr.empty_p ())
	    continue;
	  edge eg = INDEX_EDGE (m_vector_manager->vector_edge_list, ed);
	  /* If it is the edge that we never reach, skip its possible PRE
	     fusion conservatively.  */
	  if (eg->probability == profile_probability::never ())
	    break;
	  if (eg->src == ENTRY_BLOCK_PTR_FOR_FN (cfun)
	      || eg->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	    break;
	  if (bitmap_bit_p (m_vector_manager->vector_earliest[ed], i))
	    {
	      auto &src_block_info = get_block_info (eg->src);
	      auto &dest_block_info = get_block_info (eg->dest);
	      if (src_block_info.reaching_out.unknown_p ())
		break;

	      gcc_assert (!(eg->flags & EDGE_ABNORMAL));
	      vector_insn_info new_info = vector_insn_info ();
	      profile_probability prob = src_block_info.probability;
	      /* We don't fuse user vsetvl into EMPTY or
		 DIRTY (EMPTY but polluted) block for these
		 following reasons:

		- The user vsetvl instruction is configured as
		  no side effects that the previous passes
		  (GSCE, Loop-invariant, ..., etc)
		  should be able to do a good job on optimization
		  of user explicit vsetvls so we don't need to
		  PRE optimization (The user vsetvls should be
		  on the optimal local already before this pass)
		  again for user vsetvls in VSETVL PASS here
		  (Phase 3 && Phase 4).

		- Allowing user vsetvls be optimized in PRE
		  optimization here (Phase 3 && Phase 4) will
		  complicate the codes so much so we prefer user
		  vsetvls be optimized in post-optimization
		  (Phase 5 && Phase 6).  */
	      if (vsetvl_insn_p (expr.get_insn ()->rtl ()))
		{
		  if (src_block_info.reaching_out.empty_p ())
		    continue;
		  else if (src_block_info.reaching_out.dirty_p ()
			   && !src_block_info.reaching_out.compatible_p (expr))
		    {
		      new_info.set_empty ();
		      /* Update probability as uninitialized status so that
			 we won't try to fuse any demand info into such EMPTY
			 block any more.  */
		      prob = profile_probability::uninitialized ();
		      update_block_info (eg->src->index, prob, new_info);
		      continue;
		    }
		}

	      if (src_block_info.reaching_out.empty_p ())
		{
		  if (src_block_info.probability
		      == profile_probability::uninitialized ())
		    continue;
		  new_info = expr.global_merge (expr, eg->src->index);
		  new_info.set_dirty ();
		  prob = dest_block_info.probability;
		  update_block_info (eg->src->index, prob, new_info);
		  changed_p = true;
		}
	      else if (src_block_info.reaching_out.dirty_p ())
		{
		  /* DIRTY -> DIRTY or VALID -> DIRTY.  */
		  if (demands_can_be_fused_p (src_block_info.reaching_out,
					      expr))
		    {
		      new_info = src_block_info.reaching_out.global_merge (
			expr, eg->src->index);
		      new_info.set_dirty ();
		      prob += dest_block_info.probability;
		    }
		  else if (!src_block_info.reaching_out.compatible_p (expr)
			   && !m_vector_manager->earliest_fusion_worthwhile_p (
			     eg->src))
		    {
		      new_info.set_empty ();
		      prob = profile_probability::uninitialized ();
		    }
		  else if (!src_block_info.reaching_out.compatible_p (expr)
			   && dest_block_info.probability
				> src_block_info.probability)
		    {
		      new_info = expr;
		      new_info.set_dirty ();
		      prob = dest_block_info.probability;
		    }
		  else
		    continue;
		  update_block_info (eg->src->index, prob, new_info);
		  changed_p = true;
		}
	      else
		{
		  rtx vl = NULL_RTX;
		  if (vsetvl_insn_p (
			src_block_info.reaching_out.get_insn ()->rtl ())
		      && vsetvl_dominated_by_p (eg->src, expr,
						src_block_info.reaching_out,
						true))
		    ;
		  else if (!demands_can_be_fused_p (src_block_info.reaching_out,
						    expr))
		    continue;
		  else if (!earliest_pred_can_be_fused_p (
			     crtl->ssa->bb (eg->src),
			     src_block_info.reaching_out, expr, &vl))
		    continue;

		  vector_insn_info new_info
		    = src_block_info.reaching_out.global_merge (expr,
								eg->src->index);

		  prob = std::max (dest_block_info.probability,
				   src_block_info.probability);
		  change_vsetvl_insn (new_info.get_insn (), new_info, vl);
		  update_block_info (eg->src->index, prob, new_info);
		  changed_p = true;
		}
	    }
	}
    }
  return changed_p;
}

/* Fuse VSETVL demand info according LCM computed location.  */
void
pass_vsetvl::vsetvl_fusion (void)
{
  /* Fuse VSETVL demand info until VSETVL CFG fixed.  */
  bool changed_p = true;
  int fusion_no = 0;
  while (changed_p)
    {
      changed_p = false;
      fusion_no++;
      prune_expressions ();
      m_vector_manager->create_bitmap_vectors ();
      compute_local_properties ();
      /* Compute global availability.  */
      compute_available (m_vector_manager->vector_comp,
			 m_vector_manager->vector_kill,
			 m_vector_manager->vector_avout,
			 m_vector_manager->vector_avin);
      /* Compute global anticipatability.  */
      compute_antinout_edge (m_vector_manager->vector_antic,
			     m_vector_manager->vector_transp,
			     m_vector_manager->vector_antin,
			     m_vector_manager->vector_antout);
      /* Compute earliestness.  */
      compute_earliest (m_vector_manager->vector_edge_list,
			m_vector_manager->vector_exprs.length (),
			m_vector_manager->vector_antin,
			m_vector_manager->vector_antout,
			m_vector_manager->vector_avout,
			m_vector_manager->vector_kill,
			m_vector_manager->vector_earliest);
      changed_p |= earliest_fusion ();
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nEARLIEST fusion %d\n", fusion_no);
	  m_vector_manager->dump (dump_file);
	}
      m_vector_manager->free_bitmap_vectors ();
      if (!m_vector_manager->vector_exprs.is_empty ())
	m_vector_manager->vector_exprs.release ();
    }
}

/* Return true if VSETVL in the block can be refined as vsetvl zero,zero.  */
bool
pass_vsetvl::can_refine_vsetvl_p (const basic_block cfg_bb,
				  const vector_insn_info &info) const
{
  if (!m_vector_manager->all_same_ratio_p (
	m_vector_manager->vector_avin[cfg_bb->index]))
    return false;

  if (!m_vector_manager->all_same_avl_p (
	cfg_bb, m_vector_manager->vector_avin[cfg_bb->index]))
    return false;

  size_t expr_id
    = bitmap_first_set_bit (m_vector_manager->vector_avin[cfg_bb->index]);
  if (!m_vector_manager->vector_exprs[expr_id]->same_vlmax_p (info))
    return false;
  if (!m_vector_manager->vector_exprs[expr_id]->compatible_avl_p (info))
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
      auto info = get_block_info (cfg_bb).local_dem;
      insn_info *insn = info.get_insn ();
      if (!info.valid_p ())
	continue;

      rtx_insn *rinsn = insn->rtl ();
      if (!can_refine_vsetvl_p (cfg_bb, info))
	continue;

      /* We can't refine user vsetvl into vsetvl zero,zero since the dest
	 will be used by the following instructions.  */
      if (vector_config_insn_p (rinsn))
	{
	  m_vector_manager->to_refine_vsetvls.add (rinsn);
	  continue;
	}

      /* If all incoming edges to a block have a vector state that is compatbile
	 with the block. In such a case we need not emit a vsetvl in the current
	 block.  */

      gcc_assert (has_vtype_op (insn->rtl ()));
      rinsn = PREV_INSN (insn->rtl ());
      gcc_assert (vector_config_insn_p (PREV_INSN (insn->rtl ())));
      if (m_vector_manager->all_avail_in_compatible_p (cfg_bb))
	{
	  size_t id = m_vector_manager->get_expr_id (info);
	  if (bitmap_bit_p (m_vector_manager->vector_del[cfg_bb->index], id))
	    continue;
	  eliminate_insn (rinsn);
	}
      else
	{
	  rtx new_pat
	    = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY, info, NULL_RTX);
	  change_insn (rinsn, new_pat);
	}
    }
}

void
pass_vsetvl::cleanup_vsetvls ()
{
  basic_block cfg_bb;
  FOR_EACH_BB_FN (cfg_bb, cfun)
    {
      auto &info = get_block_info (cfg_bb).reaching_out;
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
		  const auto dem = get_block_info (cfg_bb).local_dem;
		  gcc_assert (dem == *m_vector_manager->vector_exprs[i]);
		  insn_info *insn = dem.get_insn ();
		  gcc_assert (insn && insn->rtl ());
		  rtx_insn *rinsn;
		  /* We can't eliminate user vsetvl since the dest will be used
		   * by the following instructions.  */
		  if (vector_config_insn_p (insn->rtl ()))
		    {
		      m_vector_manager->to_delete_vsetvls.add (insn->rtl ());
		      continue;
		    }

		  gcc_assert (has_vtype_op (insn->rtl ()));
		  rinsn = PREV_INSN (insn->rtl ());
		  gcc_assert (vector_config_insn_p (PREV_INSN (insn->rtl ())));
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
	      sbitmap bitdata = m_vector_manager->vector_avout[eg->src->index];
	      if (m_vector_manager->all_same_ratio_p (bitdata)
		  && m_vector_manager->all_same_avl_p (eg->dest, bitdata))
		{
		  size_t first = bitmap_first_set_bit (bitdata);
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

	      if (dump_file)
		{
		  fprintf (dump_file,
			   "\nInsert vsetvl insn %d at edge %d from <bb %d> to "
			   "<bb %d>:\n",
			   INSN_UID (rinsn), ed, eg->src->index,
			   eg->dest->index);
		  print_rtl_single (dump_file, rinsn);
		}
	    }
	}
    }

  for (const bb_info *bb : crtl->ssa->bbs ())
    {
      basic_block cfg_bb = bb->cfg_bb ();
      const auto reaching_out = get_block_info (cfg_bb).reaching_out;
      if (!reaching_out.dirty_p ())
	continue;

      rtx new_pat;
      if (!reaching_out.demand_p (DEMAND_AVL))
	{
	  vl_vtype_info new_info = reaching_out;
	  new_info.set_avl_info (avl_info (const0_rtx, nullptr));
	  new_pat = gen_vsetvl_pat (VSETVL_DISCARD_RESULT, new_info, NULL_RTX);
	}
      else if (can_refine_vsetvl_p (cfg_bb, reaching_out))
	new_pat
	  = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY, reaching_out, NULL_RTX);
      else if (vlmax_avl_p (reaching_out.get_avl ()))
	{
	  rtx vl = reaching_out.get_avl_or_vl_reg ();
	  new_pat = gen_vsetvl_pat (VSETVL_NORMAL, reaching_out, vl);
	}
      else
	new_pat
	  = gen_vsetvl_pat (VSETVL_DISCARD_RESULT, reaching_out, NULL_RTX);

      edge eg;
      edge_iterator eg_iterator;
      FOR_EACH_EDGE (eg, eg_iterator, cfg_bb->succs)
	{
	  /* We should not get an abnormal edge here.  */
	  gcc_assert (!(eg->flags & EDGE_ABNORMAL));
	  /* We failed to optimize this case in Phase 3 (earliest fusion):

		bb 2: vsetvl a5, a3 ...
		      goto bb 4
		bb 3: vsetvl a5, a2 ...
		      goto bb 4
		bb 4: vsetvli zero, a5 ---> Redundant, should be elided.

	     Since "a5" value can come from either bb 2 or bb 3, we can't make
	     it optimized in Phase 3 which will make phase 3 so complicated.
	     Now, we do post optimization here to elide the redundant VSETVL
	     insn in bb4.   */
	  if (m_vector_manager->vsetvl_dominated_by_all_preds_p (cfg_bb,
								 reaching_out))
	    continue;

	  start_sequence ();
	  emit_insn (copy_rtx (new_pat));
	  rtx_insn *rinsn = get_insns ();
	  end_sequence ();

	  insert_insn_on_edge (rinsn, eg);
	  need_commit = true;
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "\nInsert vsetvl insn %d from <bb %d> to <bb %d>:\n",
		       INSN_UID (rinsn), cfg_bb->index, eg->dest->index);
	      print_rtl_single (dump_file, rinsn);
	    }
	}
    }

  return need_commit;
}

void
pass_vsetvl::pre_vsetvl (void)
{
  /* Compute entity list.  */
  prune_expressions ();

  m_vector_manager->create_bitmap_vectors ();
  compute_local_properties ();
  m_vector_manager->vector_edge_list = pre_edge_lcm_avs (
    m_vector_manager->vector_exprs.length (), m_vector_manager->vector_transp,
    m_vector_manager->vector_comp, m_vector_manager->vector_antic,
    m_vector_manager->vector_kill, m_vector_manager->vector_avin,
    m_vector_manager->vector_avout, &m_vector_manager->vector_insert,
    &m_vector_manager->vector_del);

  /* We should dump the information before CFG is changed. Otherwise it will
     produce ICE (internal compiler error).  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    m_vector_manager->dump (dump_file);

  refine_vsetvls ();
  cleanup_vsetvls ();
  bool need_commit = commit_vsetvls ();
  if (need_commit)
    commit_edge_insertions ();
}

/* Some instruction can not be accessed in RTL_SSA when we don't re-init
   the new RTL_SSA framework but it is definetely at the END of the block.

  Here we optimize the VSETVL is hoisted by LCM:

   Before LCM:
     bb 1:
       vsetvli a5,a2,e32,m1,ta,mu
     bb 2:
       vsetvli zero,a5,e32,m1,ta,mu
       ...

   After LCM:
     bb 1:
       vsetvli a5,a2,e32,m1,ta,mu
       LCM INSERTED: vsetvli zero,a5,e32,m1,ta,mu --> eliminate
     bb 2:
       ...
   */
rtx_insn *
pass_vsetvl::get_vsetvl_at_end (const bb_info *bb, vector_insn_info *dem) const
{
  rtx_insn *end_vsetvl = BB_END (bb->cfg_bb ());
  if (end_vsetvl && NONDEBUG_INSN_P (end_vsetvl))
    {
      if (JUMP_P (end_vsetvl))
	end_vsetvl = PREV_INSN (end_vsetvl);

      if (NONDEBUG_INSN_P (end_vsetvl)
	  && vsetvl_discard_result_insn_p (end_vsetvl))
	{
	  /* Only handle single succ. here, multiple succ. is much
	     more complicated.  */
	  if (single_succ_p (bb->cfg_bb ()))
	    {
	      edge e = single_succ_edge (bb->cfg_bb ());
	      *dem = get_block_info (e->dest).local_dem;
	      return end_vsetvl;
	    }
	}
    }
  return nullptr;
}

/* This predicator should only used within same basic block.  */
static bool
local_avl_compatible_p (rtx avl1, rtx avl2)
{
  if (!REG_P (avl1) || !REG_P (avl2))
    return false;

  return REGNO (avl1) == REGNO (avl2);
}

/* Local user vsetvl optimizaiton:

     Case 1:
       vsetvl a5,a4,e8,mf8
       ...
       vsetvl zero,a5,e8,mf8 --> Eliminate directly.

     Case 2:
       vsetvl a5,a4,e8,mf8    --> vsetvl a5,a4,e32,mf2
       ...
       vsetvl zero,a5,e32,mf2 --> Eliminate directly.  */
void
pass_vsetvl::local_eliminate_vsetvl_insn (const bb_info *bb) const
{
  rtx_insn *prev_vsetvl = nullptr;
  rtx_insn *curr_vsetvl = nullptr;
  rtx vl_placeholder = RVV_VLMAX;
  rtx prev_avl = vl_placeholder;
  rtx curr_avl = vl_placeholder;
  vector_insn_info prev_dem;

  /* Instruction inserted by LCM is not appeared in RTL-SSA yet, try to
     found those instruciton.   */
  if (rtx_insn *end_vsetvl = get_vsetvl_at_end (bb, &prev_dem))
    {
      prev_avl = get_avl (end_vsetvl);
      prev_vsetvl = end_vsetvl;
    }

  bool skip_one = false;
  /* Backward propgate vsetvl info, drop the later one (prev_vsetvl) if it's
     compatible with current vsetvl (curr_avl), and merge the vtype and avl
     info. into current vsetvl.  */
  for (insn_info *insn : bb->reverse_real_nondebug_insns ())
    {
      rtx_insn *rinsn = insn->rtl ();
      const auto &curr_dem = get_vector_info (insn);
      bool need_invalidate = false;

      /* Skip if this insn already handled in last iteration.  */
      if (skip_one)
	{
	  skip_one = false;
	  continue;
	}

      if (vsetvl_insn_p (rinsn))
	{
	  curr_vsetvl = rinsn;
	  /* vsetvl are using vl rather than avl since it will try to merge
	     with other vsetvl_discard_result.

			v--- avl
	     vsetvl a5,a4,e8,mf8   # vsetvl
	     ...    ^--- vl
	     vsetvl zero,a5,e8,mf8 # vsetvl_discard_result
			 ^--- avl
	     */
	  curr_avl = get_vl (rinsn);
	  /* vsetvl is a cut point of local backward vsetvl elimination.  */
	  need_invalidate = true;
	}
      else if (has_vtype_op (rinsn) && NONDEBUG_INSN_P (PREV_INSN (rinsn))
	       && (vsetvl_discard_result_insn_p (PREV_INSN (rinsn))
		   || vsetvl_insn_p (PREV_INSN (rinsn))))
	{
	  curr_vsetvl = PREV_INSN (rinsn);

	  if (vsetvl_insn_p (PREV_INSN (rinsn)))
	    {
	      /* Need invalidate and skip if it's vsetvl.  */
	      need_invalidate = true;
	      /* vsetvl_discard_result_insn_p won't appeared in RTL-SSA,
	       * so only need to skip for vsetvl.  */
	      skip_one = true;
	    }

	  curr_avl = curr_dem.get_avl ();

	  /* Some instrucion like pred_extract_first<mode> don't reqruie avl, so
	     the avl is null, use vl_placeholder for unify the handling
	     logic. */
	  if (!curr_avl)
	    curr_avl = vl_placeholder;
	}
      else if (insn->is_call () || insn->is_asm ()
	       || find_access (insn->defs (), VL_REGNUM)
	       || find_access (insn->defs (), VTYPE_REGNUM)
	       || (REG_P (prev_avl)
		   && find_access (insn->defs (), REGNO (prev_avl))))
	{
	  /* Invalidate if this insn can't propagate vl, vtype or avl.  */
	  need_invalidate = true;
	  prev_dem = vector_insn_info ();
	}
      else
	/* Not interested instruction.  */
	continue;

      /* Local AVL compatibility checking is simpler than global, we only
	 need to check the REGNO is same.  */
      if (prev_dem.valid_or_dirty_p ()
	  && prev_dem.skip_avl_compatible_p (curr_dem)
	  && local_avl_compatible_p (prev_avl, curr_avl))
	{
	  /* curr_dem and prev_dem is compatible!  */
	  /* Update avl info since we need to make sure they are fully
	     compatible before merge.  */
	  prev_dem.set_avl_info (curr_dem.get_avl_info ());
	  /* Merge both and update into curr_vsetvl.  */
	  prev_dem = curr_dem.local_merge (prev_dem);
	  change_vsetvl_insn (curr_dem.get_insn (), prev_dem);
	  /* Then we can drop prev_vsetvl.  */
	  eliminate_insn (prev_vsetvl);
	}

      if (need_invalidate)
	{
	  prev_vsetvl = nullptr;
	  curr_vsetvl = nullptr;
	  prev_avl = vl_placeholder;
	  curr_avl = vl_placeholder;
	  prev_dem = vector_insn_info ();
	}
      else
	{
	  prev_vsetvl = curr_vsetvl;
	  prev_avl = curr_avl;
	  prev_dem = curr_dem;
	}
    }
}

/* Return the first vsetvl instruction in CFG_BB or NULL if
   none exists or if a user RVV instruction is enountered
   prior to any vsetvl.  */
static rtx_insn *
get_first_vsetvl_before_rvv_insns (basic_block cfg_bb,
				   enum vsetvl_type insn_type)
{
  gcc_assert (insn_type == VSETVL_DISCARD_RESULT
	      || insn_type == VSETVL_VTYPE_CHANGE_ONLY);
  rtx_insn *rinsn;
  FOR_BB_INSNS (cfg_bb, rinsn)
    {
      if (!NONDEBUG_INSN_P (rinsn))
	continue;
      /* If we don't find any inserted vsetvli before user RVV instructions,
	 we don't need to optimize the vsetvls in this block.  */
      if (has_vtype_op (rinsn) || vsetvl_insn_p (rinsn))
	return nullptr;

      if (insn_type == VSETVL_DISCARD_RESULT
	  && vsetvl_discard_result_insn_p (rinsn))
	return rinsn;
      if (insn_type == VSETVL_VTYPE_CHANGE_ONLY
	  && vsetvl_vtype_change_only_p (rinsn))
	return rinsn;
    }
  return nullptr;
}

/* Global user vsetvl optimizaiton:

     Case 1:
     bb 1:
       vsetvl a5,a4,e8,mf8
       ...
     bb 2:
       ...
       vsetvl zero,a5,e8,mf8 --> Eliminate directly.

     Case 2:
      bb 1:
       vsetvl a5,a4,e8,mf8    --> vsetvl a5,a4,e32,mf2
       ...
      bb 2:
       ...
       vsetvl zero,a5,e32,mf2 --> Eliminate directly.

     Case 3:
      bb 1:
       vsetvl a5,a4,e8,mf8    --> vsetvl a5,a4,e32,mf2
       ...
      bb 2:
       ...
       vsetvl a5,a4,e8,mf8    --> vsetvl a5,a4,e32,mf2
       goto bb 3
      bb 3:
       ...
       vsetvl zero,a5,e32,mf2 --> Eliminate directly.
*/
bool
pass_vsetvl::global_eliminate_vsetvl_insn (const bb_info *bb) const
{
  rtx_insn *vsetvl_rinsn = NULL;
  vector_insn_info dem = vector_insn_info ();
  const auto &block_info = get_block_info (bb);
  basic_block cfg_bb = bb->cfg_bb ();

  if (block_info.local_dem.valid_or_dirty_p ())
    {
      /* Optimize the local vsetvl.  */
      dem = block_info.local_dem;
      vsetvl_rinsn
	= get_first_vsetvl_before_rvv_insns (cfg_bb, VSETVL_DISCARD_RESULT);
    }
  if (!vsetvl_rinsn)
    /* Optimize the global vsetvl inserted by LCM.  */
    vsetvl_rinsn = get_vsetvl_at_end (bb, &dem);

  /* No need to optimize if block doesn't have vsetvl instructions.  */
  if (!dem.valid_or_dirty_p () || !vsetvl_rinsn || !dem.get_avl_source ()
      || !dem.has_avl_reg ())
    return false;

  /* Condition 1: Check it has preds.  */
  if (EDGE_COUNT (cfg_bb->preds) == 0)
    return false;

  /* If all preds has VL/VTYPE status setted by user vsetvls, and these
     user vsetvls are all skip_avl_compatible_p with the vsetvl in this
     block, we can eliminate this vsetvl instruction.  */
  sbitmap avin = m_vector_manager->vector_avin[cfg_bb->index];

  unsigned int bb_index;
  sbitmap_iterator sbi;
  rtx avl = dem.get_avl ();
  hash_set<set_info *> sets
    = get_all_sets (dem.get_avl_source (), true, false, false);
  /* Condition 2: All VL/VTYPE available in are all compatible.  */
  EXECUTE_IF_SET_IN_BITMAP (avin, 0, bb_index, sbi)
    {
      const auto &expr = m_vector_manager->vector_exprs[bb_index];
      const auto &insn = expr->get_insn ();
      def_info *def = find_access (insn->defs (), REGNO (avl));
      set_info *set = safe_dyn_cast<set_info *> (def);
      if (!vsetvl_insn_p (insn->rtl ()) || insn->bb () == bb
	  || !sets.contains (set))
	return false;
    }

  /* Condition 3: We don't do the global optimization for the block
     has a pred is entry block or exit block.  */
  /* Condition 4: All preds have available VL/VTYPE out.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
    {
      sbitmap avout = m_vector_manager->vector_avout[e->src->index];
      if (e->src == ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  || e->src == EXIT_BLOCK_PTR_FOR_FN (cfun)
	  || (unsigned int) e->src->index
	       >= m_vector_manager->vector_block_infos.length ()
	  || bitmap_empty_p (avout))
	return false;

      EXECUTE_IF_SET_IN_BITMAP (avout, 0, bb_index, sbi)
	{
	  const auto &expr = m_vector_manager->vector_exprs[bb_index];
	  const auto &insn = expr->get_insn ();
	  def_info *def = find_access (insn->defs (), REGNO (avl));
	  set_info *set = safe_dyn_cast<set_info *> (def);
	  if (!vsetvl_insn_p (insn->rtl ()) || insn->bb () == bb
	      || !sets.contains (set) || !expr->skip_avl_compatible_p (dem))
	    return false;
	}
    }

  /* Step1: Reshape the VL/VTYPE status to make sure everything compatible.  */
  auto_vec<basic_block> pred_cfg_bbs
    = get_dominated_by (CDI_POST_DOMINATORS, cfg_bb);
  FOR_EACH_EDGE (e, ei, cfg_bb->preds)
    {
      sbitmap avout = m_vector_manager->vector_avout[e->src->index];
      EXECUTE_IF_SET_IN_BITMAP (avout, 0, bb_index, sbi)
	{
	  vector_insn_info prev_dem = *m_vector_manager->vector_exprs[bb_index];
	  vector_insn_info curr_dem = dem;
	  insn_info *insn = prev_dem.get_insn ();
	  if (!pred_cfg_bbs.contains (insn->bb ()->cfg_bb ()))
	    continue;
	  /* Update avl info since we need to make sure they are fully
	     compatible before merge.  */
	  curr_dem.set_avl_info (prev_dem.get_avl_info ());
	  /* Merge both and update into curr_vsetvl.  */
	  prev_dem = curr_dem.local_merge (prev_dem);
	  change_vsetvl_insn (insn, prev_dem);
	}
    }

  /* Step2: eliminate the vsetvl instruction.  */
  eliminate_insn (vsetvl_rinsn);
  return true;
}

/* This function does the following post optimization base on RTL_SSA:

   1. Local user vsetvl optimizations.
   2. Global user vsetvl optimizations.
   3. AVL dependencies removal:
      Before VSETVL PASS, RVV instructions pattern is depending on AVL operand
      implicitly. Since we will emit VSETVL instruction and make RVV
      instructions depending on VL/VTYPE global status registers, we remove the
      such AVL operand in the RVV instructions pattern here in order to remove
      AVL dependencies when AVL operand is a register operand.

      Before the VSETVL PASS:
	li a5,32
	...
	vadd.vv (..., a5)
      After the VSETVL PASS:
	li a5,32
	vsetvli zero, a5, ...
	...
	vadd.vv (..., const_int 0).  */
void
pass_vsetvl::ssa_post_optimization (void) const
{
  for (const bb_info *bb : crtl->ssa->bbs ())
    {
      local_eliminate_vsetvl_insn (bb);
      bool changed_p = true;
      while (changed_p)
	{
	  changed_p = false;
	  changed_p |= global_eliminate_vsetvl_insn (bb);
	}
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
	  if (count_regno_occurrences (rinsn, REGNO (avl)) == 1)
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
	      rtx pat;
	      if (fault_first_load_p (rinsn))
		pat = simplify_replace_rtx (PATTERN (rinsn), avl, const0_rtx);
	      else
		{
		  rtx set = single_set (rinsn);
		  rtx src
		    = simplify_replace_rtx (SET_SRC (set), avl, const0_rtx);
		  pat = gen_rtx_SET (SET_DEST (set), src);
		}
	      bool ok = change_insn (crtl->ssa, change, insn, pat);
	      gcc_assert (ok);
	    }
	}
    }
}

/* Return true if the SET result is not used by any instructions.  */
static bool
has_no_uses (basic_block cfg_bb, rtx_insn *rinsn, int regno)
{
  /* Handle the following case that can not be detected in RTL_SSA.  */
  /* E.g.
	  li a5, 100
	  vsetvli a6, a5...
	  ...
	  vadd (use a6)

	The use of "a6" is removed from "vadd" but the information is
	not updated in RTL_SSA framework. We don't want to re-new
	a new RTL_SSA which is expensive, instead, we use data-flow
	analysis to check whether "a6" has no uses.  */
  if (bitmap_bit_p (df_get_live_out (cfg_bb), regno))
    return false;

  rtx_insn *iter;
  for (iter = NEXT_INSN (rinsn); iter && iter != NEXT_INSN (BB_END (cfg_bb));
       iter = NEXT_INSN (iter))
    if (df_find_use (iter, regno_reg_rtx[regno]))
      return false;

  return true;
}

/* This function does the following post optimization base on dataflow
   analysis:

   1. Change vsetvl rd, rs1 --> vsevl zero, rs1, if rd is not used by any
   nondebug instructions. Even though this PASS runs after RA and it doesn't
   help for reduce register pressure, it can help instructions scheduling since
   we remove the dependencies.

   2. Remove redundant user vsetvls base on outcome of Phase 4 (LCM) && Phase 5
   (AVL dependencies removal).  */
void
pass_vsetvl::df_post_optimization (void) const
{
  df_analyze ();
  hash_set<rtx_insn *> to_delete;
  basic_block cfg_bb;
  rtx_insn *rinsn;
  FOR_ALL_BB_FN (cfg_bb, cfun)
    {
      FOR_BB_INSNS (cfg_bb, rinsn)
	{
	  if (NONDEBUG_INSN_P (rinsn) && vsetvl_insn_p (rinsn))
	    {
	      rtx vl = get_vl (rinsn);
	      vector_insn_info info;
	      info.parse_insn (rinsn);
	      bool to_delete_p = m_vector_manager->to_delete_p (rinsn);
	      bool to_refine_p = m_vector_manager->to_refine_p (rinsn);
	      if (has_no_uses (cfg_bb, rinsn, REGNO (vl)))
		{
		  if (to_delete_p)
		    to_delete.add (rinsn);
		  else if (to_refine_p)
		    {
		      rtx new_pat = gen_vsetvl_pat (VSETVL_VTYPE_CHANGE_ONLY,
						    info, NULL_RTX);
		      validate_change_or_fail (rinsn, &PATTERN (rinsn), new_pat,
					       false);
		    }
		  else if (!vlmax_avl_p (info.get_avl ()))
		    {
		      rtx new_pat = gen_vsetvl_pat (VSETVL_DISCARD_RESULT, info,
						    NULL_RTX);
		      validate_change_or_fail (rinsn, &PATTERN (rinsn), new_pat,
					       false);
		    }
		}
	    }
	}
    }
  for (rtx_insn *rinsn : to_delete)
    eliminate_insn (rinsn);
}

void
pass_vsetvl::init (void)
{
  if (optimize > 0)
    {
      /* Initialization of RTL_SSA.  */
      calculate_dominance_info (CDI_DOMINATORS);
      calculate_dominance_info (CDI_POST_DOMINATORS);
      df_analyze ();
      crtl->ssa = new function_info (cfun);
    }

  m_vector_manager = new vector_infos_manager ();
  compute_probabilities ();

  if (dump_file && (dump_flags & TDF_DETAILS))
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
      free_dominance_info (CDI_POST_DOMINATORS);
      if (crtl->ssa->perform_pending_updates ())
	cleanup_cfg (0);
      delete crtl->ssa;
      crtl->ssa = nullptr;
    }
  m_vector_manager->release ();
  delete m_vector_manager;
  m_vector_manager = nullptr;
}

/* Compute probability for each block.  */
void
pass_vsetvl::compute_probabilities (void)
{
  /* Don't compute it in -O0 since we don't need it.  */
  if (!optimize)
    return;
  edge e;
  edge_iterator ei;

  for (const bb_info *bb : crtl->ssa->bbs ())
    {
      basic_block cfg_bb = bb->cfg_bb ();
      auto &curr_prob = get_block_info (cfg_bb).probability;

      /* GCC assume entry block (bb 0) are always so
	 executed so set its probability as "always".  */
      if (ENTRY_BLOCK_PTR_FOR_FN (cfun) == cfg_bb)
	curr_prob = profile_probability::always ();
      /* Exit block (bb 1) is the block we don't need to process.  */
      if (EXIT_BLOCK_PTR_FOR_FN (cfun) == cfg_bb)
	continue;

      gcc_assert (curr_prob.initialized_p ());
      FOR_EACH_EDGE (e, ei, cfg_bb->succs)
	{
	  auto &new_prob = get_block_info (e->dest).probability;
	  /* Normally, the edge probability should be initialized.
	     However, some special testing code which is written in
	     GIMPLE IR style force the edge probility uninitialized,
	     we conservatively set it as never so that it will not
	     affect PRE (Phase 3 && Phse 4).  */
	  if (!e->probability.initialized_p ())
	    new_prob = profile_probability::never ();
	  else if (!new_prob.initialized_p ())
	    new_prob = curr_prob * e->probability;
	  else if (new_prob == profile_probability::always ())
	    continue;
	  else
	    new_prob += curr_prob * e->probability;
	}
    }
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
  if (dump_file && (dump_flags & TDF_DETAILS))
    m_vector_manager->dump (dump_file);

  /* Phase 2 - Emit vsetvl instructions within each basic block according to
     demand, compute and save ANTLOC && AVLOC of each block.  */
  if (dump_file)
    fprintf (dump_file,
	     "\nPhase 2: Emit vsetvl instruction within each block\n");
  for (const bb_info *bb : crtl->ssa->bbs ())
    emit_local_forward_vsetvls (bb);
  if (dump_file && (dump_flags & TDF_DETAILS))
    m_vector_manager->dump (dump_file);

  /* Phase 3 - Propagate demanded info across blocks.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 3: Demands propagation across blocks\n");
  vsetvl_fusion ();

  /* Phase 4 - Lazy code motion.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 4: PRE vsetvl by Lazy code motion (LCM)\n");
  pre_vsetvl ();

  /* Phase 5 - Post optimization base on RTL_SSA.  */
  if (dump_file)
    fprintf (dump_file, "\nPhase 5: Post optimization base on RTL_SSA\n");
  ssa_post_optimization ();

  /* Phase 6 - Post optimization base on data-flow analysis.  */
  if (dump_file)
    fprintf (dump_file,
	     "\nPhase 6: Post optimization base on data-flow analysis\n");
  df_post_optimization ();
}

/* Main entry point for this pass.  */
unsigned int
pass_vsetvl::execute (function *)
{
  if (n_basic_blocks_for_fn (cfun) <= 0)
    return 0;

  /* The RVV instruction may change after split which is not a stable
     instruction. We need to split it here to avoid potential issue
     since the VSETVL PASS is insert before split PASS.  */
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
