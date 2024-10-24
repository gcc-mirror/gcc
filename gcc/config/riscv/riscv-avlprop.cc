/* AVL propagation pass for RISC-V 'V' Extension for GNU compiler.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

/* Pre-RA RTL_SSA-based pass propagates AVL for RVV instructions.
   A standalone AVL propagation pass is designed because:

     - Better code maintain:
       Current LCM-based VSETVL pass is so complicated that codes
       there will become even harder to maintain. A straight forward
       AVL propagation PASS is much easier to maintain.

     - Reduce scalar register pressure:
       A type of AVL propagation is we propagate AVL from NON-VLMAX
       instruction to VLMAX instruction.
       Note: VLMAX instruction should be ignore tail elements (TA)
       and the result should be used by the NON-VLMAX instruction.
       This optimization is mostly for auto-vectorization codes:

	  vsetvli r136, r137      --- SELECT_VL
	  vle8.v (use avl = r136) --- IFN_MASK_LEN_LOAD
	  vadd.vv (use VLMAX)     --- PLUS_EXPR
	  vse8.v (use avl = r136) --- IFN_MASK_LEN_STORE

	NO AVL propagation:

	  vsetvli a5, a4, ta
	  vle8.v v1
	  vsetvli t0, zero, ta
	  vadd.vv v2, v1, v1
	  vse8.v v2

	We can propagate the AVL to 'vadd.vv' since its result
	is consumed by a 'vse8.v' which has AVL = a5 and its
	tail elements are agnostic.

       We DON'T do this optimization on VSETVL pass since it is a
       post-RA pass that consumed 't0' already wheras a standalone
       pre-RA AVL propagation pass allows us elide the consumption
       of the pseudo register of 't0' then we can reduce scalar
       register pressure.

     - More AVL propagation opportunities:
       A pre-RA pass is more flexible for AVL REG def-use chain,
       thus we will get more potential AVL propagation as long as
       it doesn't increase the scalar register pressure.
*/

#define IN_TARGET_CODE 1
#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_MEMORY
#define INCLUDE_ARRAY

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
#include "insn-attr.h"
#include "tm-constrs.h"
#include "insn-opinit.h"

using namespace rtl_ssa;
using namespace riscv_vector;

enum avlprop_type
{
  /* VLMAX AVL and tail agnostic candidates.  */
  AVLPROP_VLMAX_TA,
  AVLPROP_NONE
};

/* dump helper functions */
static const char *
avlprop_type_to_str (enum avlprop_type type)
{
  switch (type)
    {
    case AVLPROP_VLMAX_TA:
      return "vlmax_ta";

    default:
      gcc_unreachable ();
    }
}

/* Return true if the AVL of the INSN can be propagated.  */
static bool
avl_can_be_propagated_p (rtx_insn *rinsn)
{
  /* We can't do AVL propagation when the instruction is potentially
     touching the element with i > AVL.  So, we don't do AVL propagation
     on these following situations:

       vgather:
	 - The index of "vrgather dest, source, index" may pick up the
	   element which has index >= AVL, so we can't strip the elements
	   that has index >= AVL of source register.
       vslide1down:
	 - The last element of vslide1down is AVL + 1 according to RVV ISA:
	   vstart <= i < vl-1    vd[i] = vs2[i+1] if v0.mask[i] enabled
	 - The last multiple elements of vslidedown can be the element
	   has index >= AVL according to RVV ISA:
	   0 <= i+OFFSET < VLMAX   src[i] = vs2[i+OFFSET]
	   vstart <= i < vl vd[i] = src[i] if v0.mask[i] enabled.
       vcompress:
	 - According to the ISA, the first vl elements of vector register
	   group vs2 should be extracted and packed for vcompress.  And the
	   highest element of vs2 vector may be touched by the mask.  For
	   example, given vlmax = 4 here.
	   v0 = 0b1000
	   v1 = {0x1, 0x2, 0x3, 0x4}
	   v2 = {0x5, 0x6, 0x7, 0x8}
	   vcompress v1, v2, v0 with avl = 4, v1 = {0x8, 0x2, 0x3, 0x4}.
	   vcompress v1, v2, v0 with avl = 2, v1 will be unchanged.
	   Thus, we cannot propagate avl of vcompress because it may has
	   semantics change to the result.  */
  return get_attr_type (rinsn) != TYPE_VGATHER
	 && get_attr_type (rinsn) != TYPE_VSLIDEDOWN
	 && get_attr_type (rinsn) != TYPE_VISLIDE1DOWN
	 && get_attr_type (rinsn) != TYPE_VFSLIDE1DOWN
	 && get_attr_type (rinsn) != TYPE_VCOMPRESS;
}

static bool
vlmax_ta_p (rtx_insn *rinsn)
{
  return vlmax_avl_type_p (rinsn) && tail_agnostic_p (rinsn);
}

static machine_mode
get_insn_vtype_mode (rtx_insn *rinsn)
{
  extract_insn_cached (rinsn);
  int mode_idx = get_attr_mode_idx (rinsn);
  gcc_assert (mode_idx != INVALID_ATTRIBUTE);
  return GET_MODE (recog_data.operand[mode_idx]);
}

/* Return new pattern for AVL propagation.
   Normally, we just replace AVL operand only for most
   of the instructions.  However, for instructions like
   fault load which use AVL TYPE twice in the pattern which
   will cause ICE in the later AVL TYPE change so we regenerate
   the whole pattern for such instructions.  */
static rtx
simplify_replace_avl (rtx_insn *rinsn, rtx new_avl)
{
  /* Replace AVL operand.  */
  extract_insn_cached (rinsn);
  rtx avl = recog_data.operand[get_attr_vl_op_idx (rinsn)];
  int count = count_regno_occurrences (rinsn, REGNO (avl));
  gcc_assert (count == 1);
  rtx new_pat = simplify_replace_rtx (PATTERN (rinsn), avl, new_avl);
  if (get_attr_type (rinsn) == TYPE_VLDFF
      || get_attr_type (rinsn) == TYPE_VLSEGDFF)
    new_pat
      = gen_pred_fault_load (recog_data.operand_mode[0], recog_data.operand[0],
			     recog_data.operand[1], recog_data.operand[2],
			     recog_data.operand[3], new_avl,
			     recog_data.operand[5], recog_data.operand[6],
			     get_avl_type_rtx (avl_type::NONVLMAX));
  else
    new_pat = simplify_replace_rtx (PATTERN (rinsn), avl, new_avl);
  return new_pat;
}

static void
simplify_replace_vlmax_avl (rtx_insn *rinsn, rtx new_avl)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPropagating AVL: ");
      print_rtl_single (dump_file, new_avl);
      fprintf (dump_file, "into: ");
      print_rtl_single (dump_file, rinsn);
    }
  rtx new_pat = simplify_replace_avl (rinsn, new_avl);
  validate_change_or_fail (rinsn, &PATTERN (rinsn), new_pat, false);

  /* Change AVL TYPE into NONVLMAX if it is VLMAX.  */
  if (vlmax_avl_type_p (rinsn))
    {
      int index = get_attr_avl_type_idx (rinsn);
      gcc_assert (index != INVALID_ATTRIBUTE);
      validate_change_or_fail (rinsn, recog_data.operand_loc[index],
			       get_avl_type_rtx (avl_type::NONVLMAX), false);
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Successfully to match this instruction: ");
      print_rtl_single (dump_file, rinsn);
    }
}

const pass_data pass_data_avlprop = {
  RTL_PASS,	 /* type */
  "avlprop",	 /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE,	 /* tv_id */
  0,		 /* properties_required */
  0,		 /* properties_provided */
  0,		 /* properties_destroyed */
  0,		 /* todo_flags_start */
  0,		 /* todo_flags_finish */
};

class pass_avlprop : public rtl_opt_pass
{
public:
  pass_avlprop (gcc::context *ctxt) : rtl_opt_pass (pass_data_avlprop, ctxt) {}

  /* opt_pass methods: */
  virtual bool gate (function *) final override
  {
    return TARGET_VECTOR && optimize > 0;
  }
  virtual unsigned int execute (function *) final override;

private:
  /* The AVL propagation instructions and corresponding preferred AVL.
     It will be updated during the analysis.  */
  hash_map<insn_info *, rtx> *m_avl_propagations;

  /* Potential feasible AVL propagation candidates.  */
  auto_vec<std::pair<enum avlprop_type, insn_info *>> m_candidates;

  rtx get_preferred_avl (const std::pair<enum avlprop_type, insn_info *>) const;
  rtx get_vlmax_ta_preferred_avl (insn_info *) const;
  rtx get_nonvlmax_avl (insn_info *) const;

  void avlprop_init (function *);
  void avlprop_done (void);
}; // class pass_avlprop

void
pass_avlprop::avlprop_init (function *fn)
{
  calculate_dominance_info (CDI_DOMINATORS);
  df_analyze ();
  crtl->ssa = new function_info (fn);
  m_avl_propagations = new hash_map<insn_info *, rtx>;
}

void
pass_avlprop::avlprop_done (void)
{
  free_dominance_info (CDI_DOMINATORS);
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);
  delete crtl->ssa;
  crtl->ssa = nullptr;
  delete m_avl_propagations;
  m_avl_propagations = NULL;
  if (!m_candidates.is_empty ())
    m_candidates.release ();
}

/* If we have a preferred AVL to propagate, return the AVL.
   Otherwise, return NULL_RTX as we don't need have any preferred
   AVL.  */

rtx
pass_avlprop::get_preferred_avl (
  const std::pair<enum avlprop_type, insn_info *> candidate) const
{
  switch (candidate.first)
    {
    case AVLPROP_VLMAX_TA:
      return get_vlmax_ta_preferred_avl (candidate.second);
    default:
      gcc_unreachable ();
    }
  return NULL_RTX;
}

/* This is a straight forward pattern ALWAYS in partial auto-vectorization:

     VL = SELECT_AVL (AVL, ...)
     V0 = MASK_LEN_LOAD (..., VL)
     V1 = MASK_LEN_LOAD (..., VL)
     V2 = V0 + V1 --- Missed LEN information.
     MASK_LEN_STORE (..., V2, VL)

   We prefer PLUS_EXPR (V0 + V1) instead of COND_LEN_ADD (V0, V1, dummy LEN)
   because:

     - Few code changes in Loop Vectorizer.
     - Reuse the current clean flow of partial vectorization, That is, apply
       predicate LEN or MASK into LOAD/STORE operations and other special
       arithmetic operations (e.d. DIV), then do the whole vector register
       operation if it DON'T affect the correctness.
       Such flow is used by all other targets like x86, sve, s390, ... etc.
     - PLUS_EXPR has better gimple optimizations than COND_LEN_ADD.

   We propagate AVL from NON-VLMAX to VLMAX for gimple IR like PLUS_EXPR which
   generates the VLMAX instruction due to missed LEN information. The later
   VSETVL PASS will elided the redundant vsetvls.
*/

rtx
pass_avlprop::get_vlmax_ta_preferred_avl (insn_info *insn) const
{
  if (!avl_can_be_propagated_p (insn->rtl ()))
    return NULL_RTX;
  int sew = get_sew (insn->rtl ());
  enum vlmul_type vlmul = get_vlmul (insn->rtl ());
  int ratio = calculate_ratio (sew, vlmul);

  rtx use_avl = NULL_RTX;
  for (def_info *def : insn->defs ())
    {
      if (!is_a<set_info *> (def) || def->is_mem ())
	return NULL_RTX;
      const auto *set = dyn_cast<set_info *> (def);

      /* FIXME: Stop AVL propagation if any USE is not a RVV real
	 instruction. It should be totally enough for vectorized codes since
	 they always locate at extended blocks.

	 TODO: We can extend PHI checking for intrinsic codes if it
	 necessary in the future.  */
      if (!set->is_local_to_ebb ())
	return NULL_RTX;

      for (use_info *use : set->nondebug_insn_uses ())
	{
	  insn_info *use_insn = use->insn ();
	  if (!use_insn->can_be_optimized () || use_insn->is_asm ()
	      || use_insn->is_call () || use_insn->has_volatile_refs ()
	      || use_insn->has_pre_post_modify ()
	      || !has_vl_op (use_insn->rtl ()))
	    return NULL_RTX;

	  /* We should only propagate non-VLMAX AVL into VLMAX insn when
	     such insn potential tail elements (after propagation) are
	     not used.  So, we should make sure the outcome of VLMAX insn
	     is not depend on.  */
	  extract_insn_cached (use_insn->rtl ());
	  int merge_op_idx = get_attr_merge_op_idx (use_insn->rtl ());
	  if (merge_op_idx != INVALID_ATTRIBUTE
	      && !satisfies_constraint_vu (recog_data.operand[merge_op_idx])
	      && refers_to_regno_p (set->regno (),
				    recog_data.operand[merge_op_idx])
	      && !tail_agnostic_p (use_insn->rtl ()))
	    return NULL_RTX;

	  int new_sew = get_sew (use_insn->rtl ());
	  enum vlmul_type new_vlmul = get_vlmul (use_insn->rtl ());
	  int new_ratio = calculate_ratio (new_sew, new_vlmul);
	  if (new_ratio != ratio)
	    return NULL_RTX;

	  rtx new_use_avl = get_nonvlmax_avl (use_insn);
	  if (!new_use_avl || SUBREG_P (new_use_avl))
	    return NULL_RTX;
	  if (REG_P (new_use_avl))
	    {
	      resource_info resource = full_register (REGNO (new_use_avl));
	      def_lookup dl = crtl->ssa->find_def (resource, use_insn);
	      if (dl.matching_set ())
		return NULL_RTX;
	      def_info *def1 = dl.prev_def (insn);
	      def_info *def2 = dl.prev_def (use_insn);
	      if (!def1 || !def2 || def1 != def2)
		return NULL_RTX;
	      /* For vectorized codes, we always use SELECT_VL/MIN_EXPR to
		 calculate the loop len at the header of the loop.
		 We only allow AVL propagation for real instruction for now.
		 TODO: We may enhance it for intrinsic codes if it is necessary.
	      */
	      if (!def1->insn ()->is_real ())
		return NULL_RTX;

	      /* FIXME: We only all AVL propagation within a block which should
		 be totally enough for vectorized codes.

		 TODO: We can enhance it here for intrinsic codes in the future
		 if it is necessary.  */
	      if (def1->insn ()->bb () != insn->bb ()
		  && !dominated_by_p (CDI_DOMINATORS, insn->bb ()->cfg_bb (),
				      def1->insn ()->bb ()->cfg_bb ()))
		return NULL_RTX;
	      if (def1->insn ()->bb () == insn->bb ()
		  && def1->insn ()->compare_with (insn) >= 0)
		return NULL_RTX;
	    }

	  if (!use_avl)
	    use_avl = new_use_avl;
	  else if (!rtx_equal_p (use_avl, new_use_avl))
	    return NULL_RTX;
	}
    }

  return use_avl;
}

/* Try to get the NONVLMAX AVL of the INSN.
   INSN can be either NON-VLMAX AVL itself or VLMAX AVL INSN
   before the PASS but has been propagated a NON-VLMAX AVL
   in the before round propagation.  */
rtx
pass_avlprop::get_nonvlmax_avl (insn_info *insn) const
{
  if (m_avl_propagations->get (insn))
    return (*m_avl_propagations->get (insn));
  else if (nonvlmax_avl_type_p (insn->rtl ()))
    {
      extract_insn_cached (insn->rtl ());
      return recog_data.operand[get_attr_vl_op_idx (insn->rtl ())];
    }

  return NULL_RTX;
}

/* Main entry point for this pass.  */
unsigned int
pass_avlprop::execute (function *fn)
{
  avlprop_init (fn);

  /* Iterate the whole function in reverse order (which could speed the
     convergence) to collect all potential candidates that could be AVL
     propagated.

     Note that: **NOT** all the candidates will be successfully AVL propagated.
  */
  for (bb_info *bb : crtl->ssa->reverse_bbs ())
    {
      for (insn_info *insn : bb->reverse_real_nondebug_insns ())
	{
	  /* We only forward AVL to the instruction that has AVL/VL operand
	     and can be optimized in RTL_SSA level.  */
	  if (!insn->can_be_optimized () || !has_vl_op (insn->rtl ()))
	    continue;

	  /* TODO: We only do AVL propagation for VLMAX AVL with tail
	     agnostic policy since we have missed-LEN information partial
	     autovectorization.  We could add more AVL propagation
	     for intrinsic codes in the future.  */
	  if (vlmax_ta_p (insn->rtl ()))
	    m_candidates.safe_push (std::make_pair (AVLPROP_VLMAX_TA, insn));
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nNumber of potential AVL propagations: %d\n",
	       m_candidates.length ());
      for (const auto &candidate : m_candidates)
	{
	  fprintf (dump_file, "\nAVL propagation type: %s\n",
		   avlprop_type_to_str (candidate.first));
	  print_rtl_single (dump_file, candidate.second->rtl ());
	}
    }

  /* Go through all the candidates looking for AVL that we could propagate. */
  bool change_p = true;
  while (change_p)
    {
      change_p = false;
      for (auto &candidate : m_candidates)
	{
	  rtx new_avl = get_preferred_avl (candidate);
	  if (new_avl)
	    {
	      gcc_assert (!vlmax_avl_p (new_avl));
	      auto &update
		= m_avl_propagations->get_or_insert (candidate.second);
	      change_p = !rtx_equal_p (update, new_avl);
	      update = new_avl;
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nNumber of successful AVL propagations: %d\n\n",
	     (int) m_avl_propagations->elements ());

  for (const auto prop : *m_avl_propagations)
    {
      rtx_insn *rinsn = prop.first->rtl ();
      simplify_replace_vlmax_avl (rinsn, prop.second);
    }

  if (rvv_vector_bits == RVV_VECTOR_BITS_ZVL)
    {
      /* Simplify VLMAX AVL into immediate AVL.
	 E.g. Simplify this following case:

	      vsetvl a5, zero, e32, m1
	      vadd.vv

	    into:

	      vsetvl zero, 4, e32, m1
	      vadd.vv
	 if GET_MODE_NUNITS (RVVM1SImode) == 4.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\nSimplifying VLMAX AVL into IMM AVL\n\n");
      for (auto &candidate : m_candidates)
	{
	  rtx_insn *rinsn = candidate.second->rtl ();
	  machine_mode vtype_mode = get_insn_vtype_mode (rinsn);
	  if (candidate.first == AVLPROP_VLMAX_TA
	      && !m_avl_propagations->get (candidate.second)
	      && imm_avl_p (vtype_mode))
	    {
	      rtx new_avl = gen_int_mode (GET_MODE_NUNITS (vtype_mode), Pmode);
	      simplify_replace_vlmax_avl (rinsn, new_avl);
	    }
	}
    }

  avlprop_done ();
  return 0;
}

rtl_opt_pass *
make_pass_avlprop (gcc::context *ctxt)
{
  return new pass_avlprop (ctxt);
}
