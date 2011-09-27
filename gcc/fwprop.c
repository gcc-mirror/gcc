/* RTL-based forward propagation pass for GNU compiler.
   Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Paolo Bonzini and Steven Bosscher.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"

#include "sparseset.h"
#include "timevar.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "flags.h"
#include "obstack.h"
#include "basic-block.h"
#include "output.h"
#include "df.h"
#include "target.h"
#include "cfgloop.h"
#include "tree-pass.h"
#include "domwalk.h"
#include "emit-rtl.h"


/* This pass does simple forward propagation and simplification when an
   operand of an insn can only come from a single def.  This pass uses
   df.c, so it is global.  However, we only do limited analysis of
   available expressions.

   1) The pass tries to propagate the source of the def into the use,
   and checks if the result is independent of the substituted value.
   For example, the high word of a (zero_extend:DI (reg:SI M)) is always
   zero, independent of the source register.

   In particular, we propagate constants into the use site.  Sometimes
   RTL expansion did not put the constant in the same insn on purpose,
   to satisfy a predicate, and the result will fail to be recognized;
   but this happens rarely and in this case we can still create a
   REG_EQUAL note.  For multi-word operations, this

      (set (subreg:SI (reg:DI 120) 0) (const_int 0))
      (set (subreg:SI (reg:DI 120) 4) (const_int -1))
      (set (subreg:SI (reg:DI 122) 0)
         (ior:SI (subreg:SI (reg:DI 119) 0) (subreg:SI (reg:DI 120) 0)))
      (set (subreg:SI (reg:DI 122) 4)
         (ior:SI (subreg:SI (reg:DI 119) 4) (subreg:SI (reg:DI 120) 4)))

   can be simplified to the much simpler

      (set (subreg:SI (reg:DI 122) 0) (subreg:SI (reg:DI 119)))
      (set (subreg:SI (reg:DI 122) 4) (const_int -1))

   This particular propagation is also effective at putting together
   complex addressing modes.  We are more aggressive inside MEMs, in
   that all definitions are propagated if the use is in a MEM; if the
   result is a valid memory address we check address_cost to decide
   whether the substitution is worthwhile.

   2) The pass propagates register copies.  This is not as effective as
   the copy propagation done by CSE's canon_reg, which works by walking
   the instruction chain, it can help the other transformations.

   We should consider removing this optimization, and instead reorder the
   RTL passes, because GCSE does this transformation too.  With some luck,
   the CSE pass at the end of rest_of_handle_gcse could also go away.

   3) The pass looks for paradoxical subregs that are actually unnecessary.
   Things like this:

     (set (reg:QI 120) (subreg:QI (reg:SI 118) 0))
     (set (reg:QI 121) (subreg:QI (reg:SI 119) 0))
     (set (reg:SI 122) (plus:SI (subreg:SI (reg:QI 120) 0)
                                (subreg:SI (reg:QI 121) 0)))

   are very common on machines that can only do word-sized operations.
   For each use of a paradoxical subreg (subreg:WIDER (reg:NARROW N) 0),
   if it has a single def and it is (subreg:NARROW (reg:WIDE M) 0),
   we can replace the paradoxical subreg with simply (reg:WIDE M).  The
   above will simplify this to

     (set (reg:QI 120) (subreg:QI (reg:SI 118) 0))
     (set (reg:QI 121) (subreg:QI (reg:SI 119) 0))
     (set (reg:SI 122) (plus:SI (reg:SI 118) (reg:SI 119)))

   where the first two insns are now dead.

   We used to use reaching definitions to find which uses have a
   single reaching definition (sounds obvious...), but this is too
   complex a problem in nasty testcases like PR33928.  Now we use the
   multiple definitions problem in df-problems.c.  The similarity
   between that problem and SSA form creation is taken further, in
   that fwprop does a dominator walk to create its chains; however,
   instead of creating a PHI function where multiple definitions meet
   I just punt and record only singleton use-def chains, which is
   all that is needed by fwprop.  */


static int num_changes;

DEF_VEC_P(df_ref);
DEF_VEC_ALLOC_P(df_ref,heap);
static VEC(df_ref,heap) *use_def_ref;
static VEC(df_ref,heap) *reg_defs;
static VEC(df_ref,heap) *reg_defs_stack;

/* The MD bitmaps are trimmed to include only live registers to cut
   memory usage on testcases like insn-recog.c.  Track live registers
   in the basic block and do not perform forward propagation if the
   destination is a dead pseudo occurring in a note.  */
static bitmap local_md;
static bitmap local_lr;

/* Return the only def in USE's use-def chain, or NULL if there is
   more than one def in the chain.  */

static inline df_ref
get_def_for_use (df_ref use)
{
  return VEC_index (df_ref, use_def_ref, DF_REF_ID (use));
}


/* Update the reg_defs vector with non-partial definitions in DEF_REC.
   TOP_FLAG says which artificials uses should be used, when DEF_REC
   is an artificial def vector.  LOCAL_MD is modified as after a
   df_md_simulate_* function; we do more or less the same processing
   done there, so we do not use those functions.  */

#define DF_MD_GEN_FLAGS \
	(DF_REF_PARTIAL | DF_REF_CONDITIONAL | DF_REF_MAY_CLOBBER)

static void
process_defs (df_ref *def_rec, int top_flag)
{
  df_ref def;
  while ((def = *def_rec++) != NULL)
    {
      df_ref curr_def = VEC_index (df_ref, reg_defs, DF_REF_REGNO (def));
      unsigned int dregno;

      if ((DF_REF_FLAGS (def) & DF_REF_AT_TOP) != top_flag)
	continue;

      dregno = DF_REF_REGNO (def);
      if (curr_def)
	VEC_safe_push (df_ref, heap, reg_defs_stack, curr_def);
      else
	{
	  /* Do not store anything if "transitioning" from NULL to NULL.  But
             otherwise, push a special entry on the stack to tell the
	     leave_block callback that the entry in reg_defs was NULL.  */
	  if (DF_REF_FLAGS (def) & DF_MD_GEN_FLAGS)
	    ;
	  else
	    VEC_safe_push (df_ref, heap, reg_defs_stack, def);
	}

      if (DF_REF_FLAGS (def) & DF_MD_GEN_FLAGS)
	{
	  bitmap_set_bit (local_md, dregno);
	  VEC_replace (df_ref, reg_defs, dregno, NULL);
	}
      else
	{
	  bitmap_clear_bit (local_md, dregno);
	  VEC_replace (df_ref, reg_defs, dregno, def);
	}
    }
}


/* Fill the use_def_ref vector with values for the uses in USE_REC,
   taking reaching definitions info from LOCAL_MD and REG_DEFS.
   TOP_FLAG says which artificials uses should be used, when USE_REC
   is an artificial use vector.  */

static void
process_uses (df_ref *use_rec, int top_flag)
{
  df_ref use;
  while ((use = *use_rec++) != NULL)
    if ((DF_REF_FLAGS (use) & DF_REF_AT_TOP) == top_flag)
      {
        unsigned int uregno = DF_REF_REGNO (use);
        if (VEC_index (df_ref, reg_defs, uregno)
	    && !bitmap_bit_p (local_md, uregno)
	    && bitmap_bit_p (local_lr, uregno))
	  VEC_replace (df_ref, use_def_ref, DF_REF_ID (use),
		       VEC_index (df_ref, reg_defs, uregno));
      }
}


static void
single_def_use_enter_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			    basic_block bb)
{
  int bb_index = bb->index;
  struct df_md_bb_info *md_bb_info = df_md_get_bb_info (bb_index);
  struct df_lr_bb_info *lr_bb_info = df_lr_get_bb_info (bb_index);
  rtx insn;

  bitmap_copy (local_md, &md_bb_info->in);
  bitmap_copy (local_lr, &lr_bb_info->in);

  /* Push a marker for the leave_block callback.  */
  VEC_safe_push (df_ref, heap, reg_defs_stack, NULL);

  process_uses (df_get_artificial_uses (bb_index), DF_REF_AT_TOP);
  process_defs (df_get_artificial_defs (bb_index), DF_REF_AT_TOP);

  /* We don't call df_simulate_initialize_forwards, as it may overestimate
     the live registers if there are unused artificial defs.  We prefer
     liveness to be underestimated.  */

  FOR_BB_INSNS (bb, insn)
    if (INSN_P (insn))
      {
        unsigned int uid = INSN_UID (insn);
        process_uses (DF_INSN_UID_USES (uid), 0);
        process_uses (DF_INSN_UID_EQ_USES (uid), 0);
        process_defs (DF_INSN_UID_DEFS (uid), 0);
	df_simulate_one_insn_forwards (bb, insn, local_lr);
      }

  process_uses (df_get_artificial_uses (bb_index), 0);
  process_defs (df_get_artificial_defs (bb_index), 0);
}

/* Pop the definitions created in this basic block when leaving its
   dominated parts.  */

static void
single_def_use_leave_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			    basic_block bb ATTRIBUTE_UNUSED)
{
  df_ref saved_def;
  while ((saved_def = VEC_pop (df_ref, reg_defs_stack)) != NULL)
    {
      unsigned int dregno = DF_REF_REGNO (saved_def);

      /* See also process_defs.  */
      if (saved_def == VEC_index (df_ref, reg_defs, dregno))
	VEC_replace (df_ref, reg_defs, dregno, NULL);
      else
	VEC_replace (df_ref, reg_defs, dregno, saved_def);
    }
}


/* Build a vector holding the reaching definitions of uses reached by a
   single dominating definition.  */

static void
build_single_def_use_links (void)
{
  struct dom_walk_data walk_data;

  /* We use the multiple definitions problem to compute our restricted
     use-def chains.  */
  df_set_flags (DF_EQ_NOTES);
  df_md_add_problem ();
  df_note_add_problem ();
  df_analyze ();
  df_maybe_reorganize_use_refs (DF_REF_ORDER_BY_INSN_WITH_NOTES);

  use_def_ref = VEC_alloc (df_ref, heap, DF_USES_TABLE_SIZE ());
  VEC_safe_grow_cleared (df_ref, heap, use_def_ref, DF_USES_TABLE_SIZE ());

  reg_defs = VEC_alloc (df_ref, heap, max_reg_num ());
  VEC_safe_grow_cleared (df_ref, heap, reg_defs, max_reg_num ());

  reg_defs_stack = VEC_alloc (df_ref, heap, n_basic_blocks * 10);
  local_md = BITMAP_ALLOC (NULL);
  local_lr = BITMAP_ALLOC (NULL);

  /* Walk the dominator tree looking for single reaching definitions
     dominating the uses.  This is similar to how SSA form is built.  */
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children = single_def_use_enter_block;
  walk_data.after_dom_children = single_def_use_leave_block;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);

  BITMAP_FREE (local_lr);
  BITMAP_FREE (local_md);
  VEC_free (df_ref, heap, reg_defs);
  VEC_free (df_ref, heap, reg_defs_stack);
}


/* Do not try to replace constant addresses or addresses of local and
   argument slots.  These MEM expressions are made only once and inserted
   in many instructions, as well as being used to control symbol table
   output.  It is not safe to clobber them.

   There are some uncommon cases where the address is already in a register
   for some reason, but we cannot take advantage of that because we have
   no easy way to unshare the MEM.  In addition, looking up all stack
   addresses is costly.  */

static bool
can_simplify_addr (rtx addr)
{
  rtx reg;

  if (CONSTANT_ADDRESS_P (addr))
    return false;

  if (GET_CODE (addr) == PLUS)
    reg = XEXP (addr, 0);
  else
    reg = addr;

  return (!REG_P (reg)
	  || (REGNO (reg) != FRAME_POINTER_REGNUM
	      && REGNO (reg) != HARD_FRAME_POINTER_REGNUM
	      && REGNO (reg) != ARG_POINTER_REGNUM));
}

/* Returns a canonical version of X for the address, from the point of view,
   that all multiplications are represented as MULT instead of the multiply
   by a power of 2 being represented as ASHIFT.

   Every ASHIFT we find has been made by simplify_gen_binary and was not
   there before, so it is not shared.  So we can do this in place.  */

static void
canonicalize_address (rtx x)
{
  for (;;)
    switch (GET_CODE (x))
      {
      case ASHIFT:
        if (CONST_INT_P (XEXP (x, 1))
            && INTVAL (XEXP (x, 1)) < GET_MODE_BITSIZE (GET_MODE (x))
            && INTVAL (XEXP (x, 1)) >= 0)
	  {
	    HOST_WIDE_INT shift = INTVAL (XEXP (x, 1));
	    PUT_CODE (x, MULT);
	    XEXP (x, 1) = gen_int_mode ((HOST_WIDE_INT) 1 << shift,
					GET_MODE (x));
	  }

	x = XEXP (x, 0);
        break;

      case PLUS:
        if (GET_CODE (XEXP (x, 0)) == PLUS
	    || GET_CODE (XEXP (x, 0)) == ASHIFT
	    || GET_CODE (XEXP (x, 0)) == CONST)
	  canonicalize_address (XEXP (x, 0));

	x = XEXP (x, 1);
        break;

      case CONST:
	x = XEXP (x, 0);
        break;

      default:
        return;
      }
}

/* OLD is a memory address.  Return whether it is good to use NEW instead,
   for a memory access in the given MODE.  */

static bool
should_replace_address (rtx old_rtx, rtx new_rtx, enum machine_mode mode,
			addr_space_t as, bool speed)
{
  int gain;

  if (rtx_equal_p (old_rtx, new_rtx)
      || !memory_address_addr_space_p (mode, new_rtx, as))
    return false;

  /* Copy propagation is always ok.  */
  if (REG_P (old_rtx) && REG_P (new_rtx))
    return true;

  /* Prefer the new address if it is less expensive.  */
  gain = (address_cost (old_rtx, mode, as, speed)
	  - address_cost (new_rtx, mode, as, speed));

  /* If the addresses have equivalent cost, prefer the new address
     if it has the highest `set_src_cost'.  That has the potential of
     eliminating the most insns without additional costs, and it
     is the same that cse.c used to do.  */
  if (gain == 0)
    gain = set_src_cost (new_rtx, speed) - set_src_cost (old_rtx, speed);

  return (gain > 0);
}


/* Flags for the last parameter of propagate_rtx_1.  */

enum {
  /* If PR_CAN_APPEAR is true, propagate_rtx_1 always returns true;
     if it is false, propagate_rtx_1 returns false if, for at least
     one occurrence OLD, it failed to collapse the result to a constant.
     For example, (mult:M (reg:M A) (minus:M (reg:M B) (reg:M A))) may
     collapse to zero if replacing (reg:M B) with (reg:M A).

     PR_CAN_APPEAR is disregarded inside MEMs: in that case,
     propagate_rtx_1 just tries to make cheaper and valid memory
     addresses.  */
  PR_CAN_APPEAR = 1,

  /* If PR_HANDLE_MEM is not set, propagate_rtx_1 won't attempt any replacement
     outside memory addresses.  This is needed because propagate_rtx_1 does
     not do any analysis on memory; thus it is very conservative and in general
     it will fail if non-read-only MEMs are found in the source expression.

     PR_HANDLE_MEM is set when the source of the propagation was not
     another MEM.  Then, it is safe not to treat non-read-only MEMs as
     ``opaque'' objects.  */
  PR_HANDLE_MEM = 2,

  /* Set when costs should be optimized for speed.  */
  PR_OPTIMIZE_FOR_SPEED = 4
};


/* Replace all occurrences of OLD in *PX with NEW and try to simplify the
   resulting expression.  Replace *PX with a new RTL expression if an
   occurrence of OLD was found.

   This is only a wrapper around simplify-rtx.c: do not add any pattern
   matching code here.  (The sole exception is the handling of LO_SUM, but
   that is because there is no simplify_gen_* function for LO_SUM).  */

static bool
propagate_rtx_1 (rtx *px, rtx old_rtx, rtx new_rtx, int flags)
{
  rtx x = *px, tem = NULL_RTX, op0, op1, op2;
  enum rtx_code code = GET_CODE (x);
  enum machine_mode mode = GET_MODE (x);
  enum machine_mode op_mode;
  bool can_appear = (flags & PR_CAN_APPEAR) != 0;
  bool valid_ops = true;

  if (!(flags & PR_HANDLE_MEM) && MEM_P (x) && !MEM_READONLY_P (x))
    {
      /* If unsafe, change MEMs to CLOBBERs or SCRATCHes (to preserve whether
	 they have side effects or not).  */
      *px = (side_effects_p (x)
	     ? gen_rtx_CLOBBER (GET_MODE (x), const0_rtx)
	     : gen_rtx_SCRATCH (GET_MODE (x)));
      return false;
    }

  /* If X is OLD_RTX, return NEW_RTX.  But not if replacing only within an
     address, and we are *not* inside one.  */
  if (x == old_rtx)
    {
      *px = new_rtx;
      return can_appear;
    }

  /* If this is an expression, try recursive substitution.  */
  switch (GET_RTX_CLASS (code))
    {
    case RTX_UNARY:
      op0 = XEXP (x, 0);
      op_mode = GET_MODE (op0);
      valid_ops &= propagate_rtx_1 (&op0, old_rtx, new_rtx, flags);
      if (op0 == XEXP (x, 0))
	return true;
      tem = simplify_gen_unary (code, mode, op0, op_mode);
      break;

    case RTX_BIN_ARITH:
    case RTX_COMM_ARITH:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      valid_ops &= propagate_rtx_1 (&op0, old_rtx, new_rtx, flags);
      valid_ops &= propagate_rtx_1 (&op1, old_rtx, new_rtx, flags);
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	return true;
      tem = simplify_gen_binary (code, mode, op0, op1);
      break;

    case RTX_COMPARE:
    case RTX_COMM_COMPARE:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      op_mode = GET_MODE (op0) != VOIDmode ? GET_MODE (op0) : GET_MODE (op1);
      valid_ops &= propagate_rtx_1 (&op0, old_rtx, new_rtx, flags);
      valid_ops &= propagate_rtx_1 (&op1, old_rtx, new_rtx, flags);
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	return true;
      tem = simplify_gen_relational (code, mode, op_mode, op0, op1);
      break;

    case RTX_TERNARY:
    case RTX_BITFIELD_OPS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);
      op2 = XEXP (x, 2);
      op_mode = GET_MODE (op0);
      valid_ops &= propagate_rtx_1 (&op0, old_rtx, new_rtx, flags);
      valid_ops &= propagate_rtx_1 (&op1, old_rtx, new_rtx, flags);
      valid_ops &= propagate_rtx_1 (&op2, old_rtx, new_rtx, flags);
      if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1) && op2 == XEXP (x, 2))
	return true;
      if (op_mode == VOIDmode)
	op_mode = GET_MODE (op0);
      tem = simplify_gen_ternary (code, mode, op_mode, op0, op1, op2);
      break;

    case RTX_EXTRA:
      /* The only case we try to handle is a SUBREG.  */
      if (code == SUBREG)
	{
          op0 = XEXP (x, 0);
	  valid_ops &= propagate_rtx_1 (&op0, old_rtx, new_rtx, flags);
          if (op0 == XEXP (x, 0))
	    return true;
	  tem = simplify_gen_subreg (mode, op0, GET_MODE (SUBREG_REG (x)),
				     SUBREG_BYTE (x));
	}
      break;

    case RTX_OBJ:
      if (code == MEM && x != new_rtx)
	{
	  rtx new_op0;
	  op0 = XEXP (x, 0);

	  /* There are some addresses that we cannot work on.  */
	  if (!can_simplify_addr (op0))
	    return true;

	  op0 = new_op0 = targetm.delegitimize_address (op0);
	  valid_ops &= propagate_rtx_1 (&new_op0, old_rtx, new_rtx,
					flags | PR_CAN_APPEAR);

	  /* Dismiss transformation that we do not want to carry on.  */
	  if (!valid_ops
	      || new_op0 == op0
	      || !(GET_MODE (new_op0) == GET_MODE (op0)
		   || GET_MODE (new_op0) == VOIDmode))
	    return true;

	  canonicalize_address (new_op0);

	  /* Copy propagations are always ok.  Otherwise check the costs.  */
	  if (!(REG_P (old_rtx) && REG_P (new_rtx))
	      && !should_replace_address (op0, new_op0, GET_MODE (x),
					  MEM_ADDR_SPACE (x),
	      			 	  flags & PR_OPTIMIZE_FOR_SPEED))
	    return true;

	  tem = replace_equiv_address_nv (x, new_op0);
	}

      else if (code == LO_SUM)
	{
          op0 = XEXP (x, 0);
          op1 = XEXP (x, 1);

	  /* The only simplification we do attempts to remove references to op0
	     or make it constant -- in both cases, op0's invalidity will not
	     make the result invalid.  */
	  propagate_rtx_1 (&op0, old_rtx, new_rtx, flags | PR_CAN_APPEAR);
	  valid_ops &= propagate_rtx_1 (&op1, old_rtx, new_rtx, flags);
          if (op0 == XEXP (x, 0) && op1 == XEXP (x, 1))
	    return true;

	  /* (lo_sum (high x) x) -> x  */
	  if (GET_CODE (op0) == HIGH && rtx_equal_p (XEXP (op0, 0), op1))
	    tem = op1;
	  else
	    tem = gen_rtx_LO_SUM (mode, op0, op1);

	  /* OP1 is likely not a legitimate address, otherwise there would have
	     been no LO_SUM.  We want it to disappear if it is invalid, return
	     false in that case.  */
	  return memory_address_p (mode, tem);
	}

      else if (code == REG)
	{
	  if (rtx_equal_p (x, old_rtx))
	    {
              *px = new_rtx;
              return can_appear;
	    }
	}
      break;

    default:
      break;
    }

  /* No change, no trouble.  */
  if (tem == NULL_RTX)
    return true;

  *px = tem;

  /* The replacement we made so far is valid, if all of the recursive
     replacements were valid, or we could simplify everything to
     a constant.  */
  return valid_ops || can_appear || CONSTANT_P (tem);
}


/* for_each_rtx traversal function that returns 1 if BODY points to
   a non-constant mem.  */

static int
varying_mem_p (rtx *body, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *body;
  return MEM_P (x) && !MEM_READONLY_P (x);
}


/* Replace all occurrences of OLD in X with NEW and try to simplify the
   resulting expression (in mode MODE).  Return a new expression if it is
   a constant, otherwise X.

   Simplifications where occurrences of NEW collapse to a constant are always
   accepted.  All simplifications are accepted if NEW is a pseudo too.
   Otherwise, we accept simplifications that have a lower or equal cost.  */

static rtx
propagate_rtx (rtx x, enum machine_mode mode, rtx old_rtx, rtx new_rtx,
	       bool speed)
{
  rtx tem;
  bool collapsed;
  int flags;

  if (REG_P (new_rtx) && REGNO (new_rtx) < FIRST_PSEUDO_REGISTER)
    return NULL_RTX;

  flags = 0;
  if (REG_P (new_rtx) || CONSTANT_P (new_rtx))
    flags |= PR_CAN_APPEAR;
  if (!for_each_rtx (&new_rtx, varying_mem_p, NULL))
    flags |= PR_HANDLE_MEM;

  if (speed)
    flags |= PR_OPTIMIZE_FOR_SPEED;

  tem = x;
  collapsed = propagate_rtx_1 (&tem, old_rtx, copy_rtx (new_rtx), flags);
  if (tem == x || !collapsed)
    return NULL_RTX;

  /* gen_lowpart_common will not be able to process VOIDmode entities other
     than CONST_INTs.  */
  if (GET_MODE (tem) == VOIDmode && !CONST_INT_P (tem))
    return NULL_RTX;

  if (GET_MODE (tem) == VOIDmode)
    tem = rtl_hooks.gen_lowpart_no_emit (mode, tem);
  else
    gcc_assert (GET_MODE (tem) == mode);

  return tem;
}




/* Return true if the register from reference REF is killed
   between FROM to (but not including) TO.  */

static bool
local_ref_killed_between_p (df_ref ref, rtx from, rtx to)
{
  rtx insn;

  for (insn = from; insn != to; insn = NEXT_INSN (insn))
    {
      df_ref *def_rec;
      if (!INSN_P (insn))
	continue;

      for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  if (DF_REF_REGNO (ref) == DF_REF_REGNO (def))
	    return true;
	}
    }
  return false;
}


/* Check if the given DEF is available in INSN.  This would require full
   computation of available expressions; we check only restricted conditions:
   - if DEF is the sole definition of its register, go ahead;
   - in the same basic block, we check for no definitions killing the
     definition of DEF_INSN;
   - if USE's basic block has DEF's basic block as the sole predecessor,
     we check if the definition is killed after DEF_INSN or before
     TARGET_INSN insn, in their respective basic blocks.  */
static bool
use_killed_between (df_ref use, rtx def_insn, rtx target_insn)
{
  basic_block def_bb = BLOCK_FOR_INSN (def_insn);
  basic_block target_bb = BLOCK_FOR_INSN (target_insn);
  int regno;
  df_ref def;

  /* We used to have a def reaching a use that is _before_ the def,
     with the def not dominating the use even though the use and def
     are in the same basic block, when a register may be used
     uninitialized in a loop.  This should not happen anymore since
     we do not use reaching definitions, but still we test for such
     cases and assume that DEF is not available.  */
  if (def_bb == target_bb
      ? DF_INSN_LUID (def_insn) >= DF_INSN_LUID (target_insn)
      : !dominated_by_p (CDI_DOMINATORS, target_bb, def_bb))
    return true;

  /* Check if the reg in USE has only one definition.  We already
     know that this definition reaches use, or we wouldn't be here.
     However, this is invalid for hard registers because if they are
     live at the beginning of the function it does not mean that we
     have an uninitialized access.  */
  regno = DF_REF_REGNO (use);
  def = DF_REG_DEF_CHAIN (regno);
  if (def
      && DF_REF_NEXT_REG (def) == NULL
      && regno >= FIRST_PSEUDO_REGISTER)
    return false;

  /* Check locally if we are in the same basic block.  */
  if (def_bb == target_bb)
    return local_ref_killed_between_p (use, def_insn, target_insn);

  /* Finally, if DEF_BB is the sole predecessor of TARGET_BB.  */
  if (single_pred_p (target_bb)
      && single_pred (target_bb) == def_bb)
    {
      df_ref x;

      /* See if USE is killed between DEF_INSN and the last insn in the
	 basic block containing DEF_INSN.  */
      x = df_bb_regno_last_def_find (def_bb, regno);
      if (x && DF_INSN_LUID (DF_REF_INSN (x)) >= DF_INSN_LUID (def_insn))
	return true;

      /* See if USE is killed between TARGET_INSN and the first insn in the
	 basic block containing TARGET_INSN.  */
      x = df_bb_regno_first_def_find (target_bb, regno);
      if (x && DF_INSN_LUID (DF_REF_INSN (x)) < DF_INSN_LUID (target_insn))
	return true;

      return false;
    }

  /* Otherwise assume the worst case.  */
  return true;
}


/* Check if all uses in DEF_INSN can be used in TARGET_INSN.  This
   would require full computation of available expressions;
   we check only restricted conditions, see use_killed_between.  */
static bool
all_uses_available_at (rtx def_insn, rtx target_insn)
{
  df_ref *use_rec;
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (def_insn);
  rtx def_set = single_set (def_insn);

  gcc_assert (def_set);

  /* If target_insn comes right after def_insn, which is very common
     for addresses, we can use a quicker test.  */
  if (NEXT_INSN (def_insn) == target_insn
      && REG_P (SET_DEST (def_set)))
    {
      rtx def_reg = SET_DEST (def_set);

      /* If the insn uses the reg that it defines, the substitution is
         invalid.  */
      for (use_rec = DF_INSN_INFO_USES (insn_info); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  if (rtx_equal_p (DF_REF_REG (use), def_reg))
	    return false;
	}
      for (use_rec = DF_INSN_INFO_EQ_USES (insn_info); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  if (rtx_equal_p (DF_REF_REG (use), def_reg))
	    return false;
	}
    }
  else
    {
      rtx def_reg = REG_P (SET_DEST (def_set)) ? SET_DEST (def_set) : NULL_RTX;

      /* Look at all the uses of DEF_INSN, and see if they are not
	 killed between DEF_INSN and TARGET_INSN.  */
      for (use_rec = DF_INSN_INFO_USES (insn_info); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  if (def_reg && rtx_equal_p (DF_REF_REG (use), def_reg))
	    return false;
	  if (use_killed_between (use, def_insn, target_insn))
	    return false;
	}
      for (use_rec = DF_INSN_INFO_EQ_USES (insn_info); *use_rec; use_rec++)
	{
	  df_ref use = *use_rec;
	  if (def_reg && rtx_equal_p (DF_REF_REG (use), def_reg))
	    return false;
	  if (use_killed_between (use, def_insn, target_insn))
	    return false;
	}
    }

  return true;
}


static df_ref *active_defs;
#ifdef ENABLE_CHECKING
static sparseset active_defs_check;
#endif

/* Fill the ACTIVE_DEFS array with the use->def link for the registers
   mentioned in USE_REC.  Register the valid entries in ACTIVE_DEFS_CHECK
   too, for checking purposes.  */

static void
register_active_defs (df_ref *use_rec)
{
  while (*use_rec)
    {
      df_ref use = *use_rec++;
      df_ref def = get_def_for_use (use);
      int regno = DF_REF_REGNO (use);

#ifdef ENABLE_CHECKING
      sparseset_set_bit (active_defs_check, regno);
#endif
      active_defs[regno] = def;
    }
}


/* Build the use->def links that we use to update the dataflow info
   for new uses.  Note that building the links is very cheap and if
   it were done earlier, they could be used to rule out invalid
   propagations (in addition to what is done in all_uses_available_at).
   I'm not doing this yet, though.  */

static void
update_df_init (rtx def_insn, rtx insn)
{
#ifdef ENABLE_CHECKING
  sparseset_clear (active_defs_check);
#endif
  register_active_defs (DF_INSN_USES (def_insn));
  register_active_defs (DF_INSN_USES (insn));
  register_active_defs (DF_INSN_EQ_USES (insn));
}


/* Update the USE_DEF_REF array for the given use, using the active definitions
   in the ACTIVE_DEFS array to match pseudos to their def. */

static inline void
update_uses (df_ref *use_rec)
{
  while (*use_rec)
    {
      df_ref use = *use_rec++;
      int regno = DF_REF_REGNO (use);

      /* Set up the use-def chain.  */
      if (DF_REF_ID (use) >= (int) VEC_length (df_ref, use_def_ref))
        VEC_safe_grow_cleared (df_ref, heap, use_def_ref,
                               DF_REF_ID (use) + 1);

#ifdef ENABLE_CHECKING
      gcc_assert (sparseset_bit_p (active_defs_check, regno));
#endif
      VEC_replace (df_ref, use_def_ref, DF_REF_ID (use), active_defs[regno]);
    }
}


/* Update the USE_DEF_REF array for the uses in INSN.  Only update note
   uses if NOTES_ONLY is true.  */

static void
update_df (rtx insn, rtx note)
{
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);

  if (note)
    {
      df_uses_create (&XEXP (note, 0), insn, DF_REF_IN_NOTE);
      df_notes_rescan (insn);
    }
  else
    {
      df_uses_create (&PATTERN (insn), insn, 0);
      df_insn_rescan (insn);
      update_uses (DF_INSN_INFO_USES (insn_info));
    }

  update_uses (DF_INSN_INFO_EQ_USES (insn_info));
}


/* Try substituting NEW into LOC, which originated from forward propagation
   of USE's value from DEF_INSN.  SET_REG_EQUAL says whether we are
   substituting the whole SET_SRC, so we can set a REG_EQUAL note if the
   new insn is not recognized.  Return whether the substitution was
   performed.  */

static bool
try_fwprop_subst (df_ref use, rtx *loc, rtx new_rtx, rtx def_insn, bool set_reg_equal)
{
  rtx insn = DF_REF_INSN (use);
  rtx set = single_set (insn);
  rtx note = NULL_RTX;
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));
  int old_cost = 0;
  bool ok;

  update_df_init (def_insn, insn);

  /* forward_propagate_subreg may be operating on an instruction with
     multiple sets.  If so, assume the cost of the new instruction is
     not greater than the old one.  */
  if (set)
    old_cost = set_src_cost (SET_SRC (set), speed);
  if (dump_file)
    {
      fprintf (dump_file, "\nIn insn %d, replacing\n ", INSN_UID (insn));
      print_inline_rtx (dump_file, *loc, 2);
      fprintf (dump_file, "\n with ");
      print_inline_rtx (dump_file, new_rtx, 2);
      fprintf (dump_file, "\n");
    }

  validate_unshare_change (insn, loc, new_rtx, true);
  if (!verify_changes (0))
    {
      if (dump_file)
	fprintf (dump_file, "Changes to insn %d not recognized\n",
		 INSN_UID (insn));
      ok = false;
    }

  else if (DF_REF_TYPE (use) == DF_REF_REG_USE
	   && set
	   && set_src_cost (SET_SRC (set), speed) > old_cost)
    {
      if (dump_file)
	fprintf (dump_file, "Changes to insn %d not profitable\n",
		 INSN_UID (insn));
      ok = false;
    }

  else
    {
      if (dump_file)
	fprintf (dump_file, "Changed insn %d\n", INSN_UID (insn));
      ok = true;
    }

  if (ok)
    {
      confirm_change_group ();
      num_changes++;
    }
  else
    {
      cancel_changes (0);

      /* Can also record a simplified value in a REG_EQUAL note,
	 making a new one if one does not already exist.  */
      if (set_reg_equal)
	{
	  if (dump_file)
	    fprintf (dump_file, " Setting REG_EQUAL note\n");

	  note = set_unique_reg_note (insn, REG_EQUAL, copy_rtx (new_rtx));
	}
    }

  if ((ok || note) && !CONSTANT_P (new_rtx))
    update_df (insn, note);

  return ok;
}

/* For the given single_set INSN, containing SRC known to be a
   ZERO_EXTEND or SIGN_EXTEND of a register, return true if INSN
   is redundant due to the register being set by a LOAD_EXTEND_OP
   load from memory.  */

static bool
free_load_extend (rtx src, rtx insn)
{
  rtx reg;
  df_ref *use_vec;
  df_ref use = 0, def;

  reg = XEXP (src, 0);
#ifdef LOAD_EXTEND_OP
  if (LOAD_EXTEND_OP (GET_MODE (reg)) != GET_CODE (src))
#endif
    return false;

  for (use_vec = DF_INSN_USES (insn); *use_vec; use_vec++)
    {
      use = *use_vec;

      if (!DF_REF_IS_ARTIFICIAL (use)
	  && DF_REF_TYPE (use) == DF_REF_REG_USE
	  && DF_REF_REG (use) == reg)
	break;
    }
  if (!use)
    return false;

  def = get_def_for_use (use);
  if (!def)
    return false;

  if (DF_REF_IS_ARTIFICIAL (def))
    return false;

  if (NONJUMP_INSN_P (DF_REF_INSN (def)))
    {
      rtx patt = PATTERN (DF_REF_INSN (def));

      if (GET_CODE (patt) == SET
	  && GET_CODE (SET_SRC (patt)) == MEM
	  && rtx_equal_p (SET_DEST (patt), reg))
	return true;
    }
  return false;
}

/* If USE is a subreg, see if it can be replaced by a pseudo.  */

static bool
forward_propagate_subreg (df_ref use, rtx def_insn, rtx def_set)
{
  rtx use_reg = DF_REF_REG (use);
  rtx use_insn, src;

  /* Only consider subregs... */
  enum machine_mode use_mode = GET_MODE (use_reg);
  if (GET_CODE (use_reg) != SUBREG
      || !REG_P (SET_DEST (def_set)))
    return false;

  /* If this is a paradoxical SUBREG...  */
  if (GET_MODE_SIZE (use_mode)
      > GET_MODE_SIZE (GET_MODE (SUBREG_REG (use_reg))))
    {
      /* If this is a paradoxical SUBREG, we have no idea what value the
	 extra bits would have.  However, if the operand is equivalent to
	 a SUBREG whose operand is the same as our mode, and all the modes
	 are within a word, we can just use the inner operand because
	 these SUBREGs just say how to treat the register.  */
      use_insn = DF_REF_INSN (use);
      src = SET_SRC (def_set);
      if (GET_CODE (src) == SUBREG
	  && REG_P (SUBREG_REG (src))
	  && REGNO (SUBREG_REG (src)) >= FIRST_PSEUDO_REGISTER
	  && GET_MODE (SUBREG_REG (src)) == use_mode
	  && subreg_lowpart_p (src)
	  && all_uses_available_at (def_insn, use_insn))
	return try_fwprop_subst (use, DF_REF_LOC (use), SUBREG_REG (src),
				 def_insn, false);
    }

  /* If this is a SUBREG of a ZERO_EXTEND or SIGN_EXTEND, and the SUBREG
     is the low part of the reg being extended then just use the inner
     operand.  Don't do this if the ZERO_EXTEND or SIGN_EXTEND insn will
     be removed due to it matching a LOAD_EXTEND_OP load from memory.  */
  else if (subreg_lowpart_p (use_reg))
    {
      use_insn = DF_REF_INSN (use);
      src = SET_SRC (def_set);
      if ((GET_CODE (src) == ZERO_EXTEND
	   || GET_CODE (src) == SIGN_EXTEND)
	  && REG_P (XEXP (src, 0))
	  && REGNO (XEXP (src, 0)) >= FIRST_PSEUDO_REGISTER
	  && GET_MODE (XEXP (src, 0)) == use_mode
	  && !free_load_extend (src, def_insn)
	  && all_uses_available_at (def_insn, use_insn))
	return try_fwprop_subst (use, DF_REF_LOC (use), XEXP (src, 0),
				 def_insn, false);
    }

  return false;
}

/* Try to replace USE with SRC (defined in DEF_INSN) in __asm.  */

static bool
forward_propagate_asm (df_ref use, rtx def_insn, rtx def_set, rtx reg)
{
  rtx use_insn = DF_REF_INSN (use), src, use_pat, asm_operands, new_rtx, *loc;
  int speed_p, i;
  df_ref *use_vec;

  gcc_assert ((DF_REF_FLAGS (use) & DF_REF_IN_NOTE) == 0);

  src = SET_SRC (def_set);
  use_pat = PATTERN (use_insn);

  /* In __asm don't replace if src might need more registers than
     reg, as that could increase register pressure on the __asm.  */
  use_vec = DF_INSN_USES (def_insn);
  if (use_vec[0] && use_vec[1])
    return false;

  update_df_init (def_insn, use_insn);
  speed_p = optimize_bb_for_speed_p (BLOCK_FOR_INSN (use_insn));
  asm_operands = NULL_RTX;
  switch (GET_CODE (use_pat))
    {
    case ASM_OPERANDS:
      asm_operands = use_pat;
      break;
    case SET:
      if (MEM_P (SET_DEST (use_pat)))
	{
	  loc = &SET_DEST (use_pat);
	  new_rtx = propagate_rtx (*loc, GET_MODE (*loc), reg, src, speed_p);
	  if (new_rtx)
	    validate_unshare_change (use_insn, loc, new_rtx, true);
	}
      asm_operands = SET_SRC (use_pat);
      break;
    case PARALLEL:
      for (i = 0; i < XVECLEN (use_pat, 0); i++)
	if (GET_CODE (XVECEXP (use_pat, 0, i)) == SET)
	  {
	    if (MEM_P (SET_DEST (XVECEXP (use_pat, 0, i))))
	      {
		loc = &SET_DEST (XVECEXP (use_pat, 0, i));
		new_rtx = propagate_rtx (*loc, GET_MODE (*loc), reg,
					 src, speed_p);
		if (new_rtx)
		  validate_unshare_change (use_insn, loc, new_rtx, true);
	      }
	    asm_operands = SET_SRC (XVECEXP (use_pat, 0, i));
	  }
	else if (GET_CODE (XVECEXP (use_pat, 0, i)) == ASM_OPERANDS)
	  asm_operands = XVECEXP (use_pat, 0, i);
      break;
    default:
      gcc_unreachable ();
    }

  gcc_assert (asm_operands && GET_CODE (asm_operands) == ASM_OPERANDS);
  for (i = 0; i < ASM_OPERANDS_INPUT_LENGTH (asm_operands); i++)
    {
      loc = &ASM_OPERANDS_INPUT (asm_operands, i);
      new_rtx = propagate_rtx (*loc, GET_MODE (*loc), reg, src, speed_p);
      if (new_rtx)
	validate_unshare_change (use_insn, loc, new_rtx, true);
    }

  if (num_changes_pending () == 0 || !apply_change_group ())
    return false;

  update_df (use_insn, NULL);
  num_changes++;
  return true;
}

/* Try to replace USE with SRC (defined in DEF_INSN) and simplify the
   result.  */

static bool
forward_propagate_and_simplify (df_ref use, rtx def_insn, rtx def_set)
{
  rtx use_insn = DF_REF_INSN (use);
  rtx use_set = single_set (use_insn);
  rtx src, reg, new_rtx, *loc;
  bool set_reg_equal;
  enum machine_mode mode;
  int asm_use = -1;

  if (INSN_CODE (use_insn) < 0)
    asm_use = asm_noperands (PATTERN (use_insn));

  if (!use_set && asm_use < 0 && !DEBUG_INSN_P (use_insn))
    return false;

  /* Do not propagate into PC, CC0, etc.  */
  if (use_set && GET_MODE (SET_DEST (use_set)) == VOIDmode)
    return false;

  /* If def and use are subreg, check if they match.  */
  reg = DF_REF_REG (use);
  if (GET_CODE (reg) == SUBREG && GET_CODE (SET_DEST (def_set)) == SUBREG)
    {
      if (SUBREG_BYTE (SET_DEST (def_set)) != SUBREG_BYTE (reg))
	return false;
    }
  /* Check if the def had a subreg, but the use has the whole reg.  */
  else if (REG_P (reg) && GET_CODE (SET_DEST (def_set)) == SUBREG)
    return false;
  /* Check if the use has a subreg, but the def had the whole reg.  Unlike the
     previous case, the optimization is possible and often useful indeed.  */
  else if (GET_CODE (reg) == SUBREG && REG_P (SET_DEST (def_set)))
    reg = SUBREG_REG (reg);

  /* Make sure that we can treat REG as having the same mode as the
     source of DEF_SET.  */
  if (GET_MODE (SET_DEST (def_set)) != GET_MODE (reg))
    return false;

  /* Check if the substitution is valid (last, because it's the most
     expensive check!).  */
  src = SET_SRC (def_set);
  if (!CONSTANT_P (src) && !all_uses_available_at (def_insn, use_insn))
    return false;

  /* Check if the def is loading something from the constant pool; in this
     case we would undo optimization such as compress_float_constant.
     Still, we can set a REG_EQUAL note.  */
  if (MEM_P (src) && MEM_READONLY_P (src))
    {
      rtx x = avoid_constant_pool_reference (src);
      if (x != src && use_set)
	{
          rtx note = find_reg_note (use_insn, REG_EQUAL, NULL_RTX);
	  rtx old_rtx = note ? XEXP (note, 0) : SET_SRC (use_set);
	  rtx new_rtx = simplify_replace_rtx (old_rtx, src, x);
	  if (old_rtx != new_rtx)
            set_unique_reg_note (use_insn, REG_EQUAL, copy_rtx (new_rtx));
	}
      return false;
    }

  if (asm_use >= 0)
    return forward_propagate_asm (use, def_insn, def_set, reg);

  /* Else try simplifying.  */

  if (DF_REF_TYPE (use) == DF_REF_REG_MEM_STORE)
    {
      loc = &SET_DEST (use_set);
      set_reg_equal = false;
    }
  else if (!use_set)
    {
      loc = &INSN_VAR_LOCATION_LOC (use_insn);
      set_reg_equal = false;
    }
  else
    {
      rtx note = find_reg_note (use_insn, REG_EQUAL, NULL_RTX);
      if (DF_REF_FLAGS (use) & DF_REF_IN_NOTE)
	loc = &XEXP (note, 0);
      else
	loc = &SET_SRC (use_set);

      /* Do not replace an existing REG_EQUAL note if the insn is not
	 recognized.  Either we're already replacing in the note, or we'll
	 separately try plugging the definition in the note and simplifying.
	 And only install a REQ_EQUAL note when the destination is a REG,
	 as the note would be invalid otherwise.  */
      set_reg_equal = (note == NULL_RTX && REG_P (SET_DEST (use_set)));
    }

  if (GET_MODE (*loc) == VOIDmode)
    mode = GET_MODE (SET_DEST (use_set));
  else
    mode = GET_MODE (*loc);

  new_rtx = propagate_rtx (*loc, mode, reg, src,
  			   optimize_bb_for_speed_p (BLOCK_FOR_INSN (use_insn)));

  if (!new_rtx)
    return false;

  return try_fwprop_subst (use, loc, new_rtx, def_insn, set_reg_equal);
}


/* Given a use USE of an insn, if it has a single reaching
   definition, try to forward propagate it into that insn.
   Return true if cfg cleanup will be needed.  */

static bool
forward_propagate_into (df_ref use)
{
  df_ref def;
  rtx def_insn, def_set, use_insn;
  rtx parent;

  if (DF_REF_FLAGS (use) & DF_REF_READ_WRITE)
    return false;
  if (DF_REF_IS_ARTIFICIAL (use))
    return false;

  /* Only consider uses that have a single definition.  */
  def = get_def_for_use (use);
  if (!def)
    return false;
  if (DF_REF_FLAGS (def) & DF_REF_READ_WRITE)
    return false;
  if (DF_REF_IS_ARTIFICIAL (def))
    return false;

  /* Do not propagate loop invariant definitions inside the loop.  */
  if (DF_REF_BB (def)->loop_father != DF_REF_BB (use)->loop_father)
    return false;

  /* Check if the use is still present in the insn!  */
  use_insn = DF_REF_INSN (use);
  if (DF_REF_FLAGS (use) & DF_REF_IN_NOTE)
    parent = find_reg_note (use_insn, REG_EQUAL, NULL_RTX);
  else
    parent = PATTERN (use_insn);

  if (!reg_mentioned_p (DF_REF_REG (use), parent))
    return false;

  def_insn = DF_REF_INSN (def);
  if (multiple_sets (def_insn))
    return false;
  def_set = single_set (def_insn);
  if (!def_set)
    return false;

  /* Only try one kind of propagation.  If two are possible, we'll
     do it on the following iterations.  */
  if (forward_propagate_and_simplify (use, def_insn, def_set)
      || forward_propagate_subreg (use, def_insn, def_set))
    {
      if (cfun->can_throw_non_call_exceptions
	  && find_reg_note (use_insn, REG_EH_REGION, NULL_RTX)
	  && purge_dead_edges (DF_REF_BB (use)))
	return true;
    }
  return false;
}


static void
fwprop_init (void)
{
  num_changes = 0;
  calculate_dominance_info (CDI_DOMINATORS);

  /* We do not always want to propagate into loops, so we have to find
     loops and be careful about them.  But we have to call flow_loops_find
     before df_analyze, because flow_loops_find may introduce new jump
     insns (sadly) if we are not working in cfglayout mode.  */
  loop_optimizer_init (0);

  build_single_def_use_links ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  active_defs = XNEWVEC (df_ref, max_reg_num ());
#ifdef ENABLE_CHECKING
  active_defs_check = sparseset_alloc (max_reg_num ());
#endif
}

static void
fwprop_done (void)
{
  loop_optimizer_finalize ();

  VEC_free (df_ref, heap, use_def_ref);
  free (active_defs);
#ifdef ENABLE_CHECKING
  sparseset_free (active_defs_check);
#endif

  free_dominance_info (CDI_DOMINATORS);
  cleanup_cfg (0);
  delete_trivially_dead_insns (get_insns (), max_reg_num ());

  if (dump_file)
    fprintf (dump_file,
	     "\nNumber of successful forward propagations: %d\n\n",
	     num_changes);
}


/* Main entry point.  */

static bool
gate_fwprop (void)
{
  return optimize > 0 && flag_forward_propagate;
}

static unsigned int
fwprop (void)
{
  unsigned i;
  bool need_cleanup = false;

  fwprop_init ();

  /* Go through all the uses.  df_uses_create will create new ones at the
     end, and we'll go through them as well.

     Do not forward propagate addresses into loops until after unrolling.
     CSE did so because it was able to fix its own mess, but we are not.  */

  for (i = 0; i < DF_USES_TABLE_SIZE (); i++)
    {
      df_ref use = DF_USES_GET (i);
      if (use)
	if (DF_REF_TYPE (use) == DF_REF_REG_USE
	    || DF_REF_BB (use)->loop_father == NULL
	    /* The outer most loop is not really a loop.  */
	    || loop_outer (DF_REF_BB (use)->loop_father) == NULL)
	  need_cleanup |= forward_propagate_into (use);
    }

  fwprop_done ();
  if (need_cleanup)
    cleanup_cfg (0);
  return 0;
}

struct rtl_opt_pass pass_rtl_fwprop =
{
 {
  RTL_PASS,
  "fwprop1",                            /* name */
  gate_fwprop,				/* gate */
  fwprop,				/* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_FWPROP,                            /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish
    | TODO_verify_flow
    | TODO_verify_rtl_sharing           /* todo_flags_finish */
 }
};

static unsigned int
fwprop_addr (void)
{
  unsigned i;
  bool need_cleanup = false;

  fwprop_init ();

  /* Go through all the uses.  df_uses_create will create new ones at the
     end, and we'll go through them as well.  */
  for (i = 0; i < DF_USES_TABLE_SIZE (); i++)
    {
      df_ref use = DF_USES_GET (i);
      if (use)
	if (DF_REF_TYPE (use) != DF_REF_REG_USE
	    && DF_REF_BB (use)->loop_father != NULL
	    /* The outer most loop is not really a loop.  */
	    && loop_outer (DF_REF_BB (use)->loop_father) != NULL)
	  need_cleanup |= forward_propagate_into (use);
    }

  fwprop_done ();

  if (need_cleanup)
    cleanup_cfg (0);
  return 0;
}

struct rtl_opt_pass pass_rtl_fwprop_addr =
{
 {
  RTL_PASS,
  "fwprop2",                            /* name */
  gate_fwprop,				/* gate */
  fwprop_addr,				/* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_FWPROP,                            /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish | TODO_verify_rtl_sharing  /* todo_flags_finish */
 }
};
