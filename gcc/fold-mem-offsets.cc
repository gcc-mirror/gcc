/* Late RTL pass to fold memory offsets.
   Copyright (C) 2023-2026 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "rtlanal.h"
#include "df.h"
#include "rtl-ssa.h"

#include "predict.h"
#include "cfgrtl.h"
#include "cfgcleanup.h"
#include "tree-pass.h"
#include "target.h"

#include "tm.h"
#include "tree.h"
#include "expr.h"
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "diagnostic-core.h"

/* This pass tries to optimize memory offset calculations by moving constants
   from add instructions to the memory instructions (loads / stores).
   For example it can transform code like this:

     add  t4, sp, 16
     add  t2, a6, t4
     shl  t3, t2, 1
     ld   a2, 0(t3)
     add  a2, 1
     sd   a2, 8(t2)

   into the following (one instruction less):

     add  t2, a6, sp
     shl  t3, t2, 1
     ld   a2, 32(t3)
     add  a2, 1
     sd   a2, 24(t2)

   Although the previous passes try to emit efficient offset calculations
   this pass is still beneficial because:

    - The mechanisms that optimize memory offsets usually work with specific
      patterns or have limitations.  This pass is designed to fold offsets
      through complex calculations that affect multiple memory operations
      and have partially overlapping calculations.

    - There are cases where add instructions are introduced in late rtl passes
      and the rest of the pipeline cannot eliminate them.  Arrays and structs
      allocated on the stack can result in unwanted add instructions that
      cannot be eliminated easily.

   The pass differentiates between the following instructions:

   - fold-mem-offset root insn: loads/stores where constants will be folded into
     the address offset.  E.g.:
       (set (mem:DI (plus:DI (reg:DI sp) (const_int 40))) (reg:DI ra))
   - fold-agnostic insns: instructions that may have an impact on the offset
     calculation, but that don't require any fixup when folding.  E.g.:
       (set (reg:DI a0) (ashift:DI (reg:DI s1) (const_int 1)))
   - fold insns: instructions that provide constants, which will be forwarded
     into the loads/stores as offset.  When folding, the constants will be
     set to zero.  E.g.:
       (set (reg:DI s0) (plus:DI (reg:DI sp) (const_int 8)))

   The pass utilizes the RTL SSA framework to get the data dependencies
   and operates in the following phases:

   - Phase 1: Iterate over all instructions to identify fold-mem-offset roots.
   - Phase 2: Walk back along the def-chain of fold-agnostic or fold insns.
	      When successful a new offset of the fold-mem-offset is calculated
	      and a vec of fold insns that need adjustments is created.
   - Phase 3: Drop all fold-mem-offset roots that won't accept the updated
	      offset.
   - Phase 4: Ensure that the defs of all fold insns are used only by
	      fold-mem-offsets insns (only needed if DEFs with multiple USEs
	      are handled, which is the default (see multi_use_mode below),
	      or forced on via -fexpensive-optimizations).
   - Phase 5: Update all fold-mem-offset roots and adjust the fold insns.

   When we walk the DEF-chain we have two choices of operations:

   - We only allow DEFs that have exactly one USE (in the instruction
     that we come from).  This greatly simplifies the problem, but also misses
     some cases.
   - We allow DEFs to have multiple USEs.  E.g. a single ADDI may define a
     value that is used by two LOADs.  In this case, we need to ensure that all
     USE-chains remain correct after we apply our transformation.  We do this
     by allowing only USEs that are part of any other fold-mem-offset chain in
     phase 4 above.  This mode is enabled by default, but is disabled for
     highly-connected CFGs (unless -fexpensive-optimizations forces it on).

   This pass should run before hard register propagation because it creates
   register moves that we expect to be eliminated.  */

using namespace rtl_ssa;

namespace {

/* Class that holds in FOLD_INSNS the instructions that if folded the offset
   of a memory instruction would increase by ADDED_OFFSET.  */
class fold_mem_info {
public:
  /* fold-mem-offset root details.  */
  insn_info *insn;
  rtx mem;
  rtx reg;
  HOST_WIDE_INT offset;
  /* Resulting offset if def-chain gets folded into fold-mem-offset root.  */
  HOST_WIDE_INT added_offset;

  /* Def-chain for offset.  */
  auto_vec<insn_info *> fold_agnostic_insns;
  auto_vec<insn_info *> fold_insns;

  fold_mem_info (insn_info *insn, rtx mem, rtx reg, HOST_WIDE_INT off)
    : insn (insn),
      mem (mem),
      reg (reg),
      offset (off),
      added_offset (0)
  {
  }
};

class change_info {
public:
  insn_change *change;
  /* Index specifying the order in RTL SSA's instruction changes.  */
  int change_index;

  change_info (insn_change *change)
    : change (change), change_index (0)
  {
  }

  change_info (insn_change *change, int index)
    : change (change), change_index (index)
  {
  }
};

/* Check if CHANGE exists in CHANGES.  */

static bool
change_in_vec_p (const auto_vec<change_info *> &changes,
		 const change_info &change)
{
  for (const change_info *other_change : changes)
    if (other_change->change->insn () == change.change->insn ())
      return true;

  return false;
}

/* Instruction changes that are going to be committed, keyed by the id of the
   fold group that they belong to (see fold_groups).  */
using changes_map_t
  = hash_map<int_hash<unsigned, -1U, -2U>, auto_vec<change_info *>>;

/* Zeroing the constant of a fold insn is only correct if every fold-mem-offset
   root that addresses memory through it is updated with the compensating
   offset.  Roots that share fold insns, directly or through another root,
   therefore form a group, which has to be committed, or cancelled, as one
   unit.  */

class fold_groups
{
public:
  /* Put the def-chain FOLD_INSNS in one group and return its id.  Insns that
     already belong to a group merge that group into the result, so chains
     sharing a fold insn end up in the same group.  */
  unsigned add_chain (const vec<insn_info *> &fold_insns)
  {
    unsigned group = 0;
    bool found = false;
    for (insn_info *insn : fold_insns)
      if (unsigned *seen = m_insn_group.get (insn))
	{
	  group = found ? merge (group, *seen) : find (*seen);
	  found = true;
	}

    if (!found)
      {
	group = m_parent.length ();
	m_parent.safe_push (group);
      }

    for (insn_info *insn : fold_insns)
      m_insn_group.put (insn, group);

    return group;
  }

  /* Changes to commit, per group.  */
  changes_map_t changes;

  /* Groups whose changes have been cancelled; no further changes may be
     attempted for them.  */
  auto_bitmap cancelled;

private:
  /* Resolve ID to the id of the group that it now belongs to.  */
  unsigned find (unsigned id)
  {
    while (m_parent[id] != id)
      id = m_parent[id];
    return id;
  }

  /* Merge the groups holding A and B and return the surviving id, which
     takes over the changes and the cancelled state of the absorbed group.  */
  unsigned merge (unsigned a, unsigned b)
  {
    a = find (a);
    b = find (b);
    if (a == b)
      return a;

    /* Canonicalize on the smaller id, so that the surviving key does not
       depend on the order in which the chains were processed.  */
    if (b < a)
      std::swap (a, b);
    m_parent[b] = a;

    /* Take A's entry first: get_or_insert can resize the map, which would
       invalidate a pointer to B's entry taken before it.  The groups were
       disjoint, so the lists cannot overlap.  */
    auto_vec<change_info *> &a_changes = changes.get_or_insert (a);
    if (auto_vec<change_info *> *b_changes = changes.get (b))
      {
	a_changes.safe_splice (*b_changes);
	changes.remove (b);
      }

    if (bitmap_clear_bit (cancelled, b))
      bitmap_set_bit (cancelled, a);

    return a;
  }

  /* Union-find parent of every group id.  */
  auto_vec<unsigned> m_parent;

  /* The group of each fold insn seen so far.  */
  hash_map<insn_info *, unsigned> m_insn_group;
};

/* Test if INSN is a memory load / store that can have an offset folded to it.
   Return true when INSN is such an instruction and return through MEM,
   REG and OFFSET the RTX that has a MEM code, the register that is
   used as a base address and the offset accordingly.  */
bool
get_fold_mem_offset_root (insn_info *insn, rtx *mem, rtx *reg,
			  HOST_WIDE_INT *offset)
{
  rtx set = single_set (insn->rtl ());
  if (set != NULL_RTX)
    {
      rtx src = SET_SRC (set);
      rtx dest = SET_DEST (set);

      /* Don't fold when we have unspec / volatile.  */
      if (GET_CODE (src) == UNSPEC
	  || GET_CODE (src) == UNSPEC_VOLATILE)
	return false;

      if (MEM_P (src))
	*mem = src;
      else if (MEM_P (dest))
	*mem = dest;
      else if ((GET_CODE (src) == SIGN_EXTEND
		|| GET_CODE (src) == ZERO_EXTEND)
	       && MEM_P (XEXP (src, 0)))
	*mem = XEXP (src, 0);
      else
	return false;
    }
  else
    return false;

  rtx mem_addr = XEXP (*mem, 0);
  if (REG_P (mem_addr))
    {
      *reg = mem_addr;
      *offset = 0;
    }
  else if (GET_CODE (mem_addr) == PLUS
	   && REG_P (XEXP (mem_addr, 0))
	   && CONST_INT_P (XEXP (mem_addr, 1)))
    {
      *reg = XEXP (mem_addr, 0);
      *offset = INTVAL (XEXP (mem_addr, 1));
    }
  else
    return false;

  return true;
}

/* Return true if DEF's register is conditionally redefined by a cond_exec
   before DEF is fully overwritten.  RTL-SSA models a cond_exec set as a plain
   def, missing the old value preserved on the predicate-false path, so such a
   redefinition hides a use of DEF and makes folding it unsafe.  */

static bool
def_shadowed_by_cond_exec_p (set_info *def)
{
  for (def_info *next = def->next_def (); next; next = next->next_def ())
    {
      insn_info *next_insn = next->insn ();
      if (next_insn->is_artificial ())
	return false;

      rtx_insn *next_rtl = next_insn->rtl ();
      if (GET_CODE (PATTERN (next_rtl)) == COND_EXEC)
	return true;

      /* A full set kills DEF; once dead it cannot reach a later cond_exec.
	 Keep scanning past defs that leave DEF partially live.  */
      if (simple_regno_set (PATTERN (next_rtl), def->regno ()))
	return false;
    }
  return false;
}

/* Get the single reaching definition of an instruction inside a BB.
   Return the definition or NULL if there's no definition with the desired
   criteria.  If SINGLE_USE is set to true the DEF must have exactly one
   USE resulting in a 1:1 DEF-USE relationship.  If set to false, then a
   1:n DEF-USE relationship is accepted and the caller must take care to
   ensure all USEs are safe folding.  */
static set_info *
get_single_def_in_bb (insn_info *insn, rtx reg, bool single_use)
{
  /* Get the use_info of the base register.  */
  for (use_info *use : insn->uses ())
    {
      /* Other USEs can be ignored and multiple equal USEs are fine.  */
      if (use->regno () != REGNO (reg))
	continue;

      /* Don't handle subregs for now.  */
      if (use->includes_subregs ())
	return NULL;

      /* Get the DEF of the register.  Bail on a cond_exec redefinition of
	 REG, which hides uses of DEF from RTL-SSA.  */
      set_info *def = use->def ();
      if (!def || def_shadowed_by_cond_exec_p (def))
	return NULL;

      /* Limit the amount of USEs of DEF to 1.  */
      if (single_use && !def->single_nondebug_use ())
	return NULL;

      /* Don't handle multiregs for now.  */
      if (def->includes_multiregs ())
	return NULL;

      /* Only consider uses whose definition comes from a real instruction
	 and has no notes attached.  */
      insn_info *def_insn = def->insn ();
      rtx_insn *def_rtl = def_insn->rtl ();
      if (def_insn->is_artificial ()
	  || find_reg_note (def_rtl, REG_EQUIV, NULL_RTX)
	  || find_reg_note (def_rtl, REG_EQUAL, NULL_RTX))
	return NULL;

      /* No parallel expressions or clobbers.  */
      if (def_insn->num_defs () != 1)
	return NULL;

      if (!NONJUMP_INSN_P (def_rtl) || RTX_FRAME_RELATED_P (def_rtl))
	return NULL;

      /* Check if the DEF is a SET of the expected form.  */
      rtx def_set = simple_regno_set (PATTERN (def_rtl), def->regno ());
      if (!def_set)
	return NULL;

      /* Ensure DEF and USE are in the same BB.  */
      if (def->bb () != insn->bb ())
	return NULL;

      return def;
    }

  return NULL;
}

static bool
fold_offsets (insn_info *insn, rtx reg, HOST_WIDE_INT *offset_out,
	      fold_mem_info *info, bool single_use);

/* Return the offset computed by fold_offsets, or 0 if the analysis fails.
   Used in fold_offsets_1 where failure means no constant contribution.  */
static HOST_WIDE_INT
fold_offsets_value (insn_info *insn, rtx reg, fold_mem_info *info,
		    bool single_use)
{
  HOST_WIDE_INT offset;
  fold_offsets (insn, reg, &offset, info, single_use);
  return offset;
}

/* Helper function for fold_offsets () that analyses the given INSN.

   For INSN with known pattern, we calculate the value of the propagated
   constant and store that in OFFSET_OUT.  Foldable INSNs are added to
   INFO->fold_insns and fold-agnostic INSNs are added to
   INFO->fold_agnostic_insns.  It is possible that some INSNs are added to
   both lists; when that happens the INSN is a fold insn.

   Returns true when the analysis succeeds.  Otherwise false.  */
static bool
fold_offsets_1 (insn_info *insn, HOST_WIDE_INT *offset_out,
		fold_mem_info *info, bool single_use)
{
  bool fold_agnostic = true;
  rtx_insn *insn_rtl = insn->rtl ();
  rtx set = single_set (insn_rtl);
  if (!set)
    return false;

  rtx src = SET_SRC (set);
  HOST_WIDE_INT offset = 0;

  switch (GET_CODE (src))
    {
    case PLUS:
      {
	/* Propagate through add.  */
	rtx arg1 = XEXP (src, 0);
	rtx arg2 = XEXP (src, 1);

	if (REG_P (arg1))
	  offset += fold_offsets_value (insn, arg1, info, single_use);
	else if (GET_CODE (arg1) == ASHIFT
		 && REG_P (XEXP (arg1, 0))
		 && CONST_INT_P (XEXP (arg1, 1)))
	  {
	    /* Handle R1 = (R2 << C) + ...  */
	    rtx reg = XEXP (arg1, 0);
	    rtx shamt = XEXP (arg1, 1);
	    HOST_WIDE_INT scale = HOST_WIDE_INT_1U << INTVAL (shamt);
	    offset += scale * fold_offsets_value (insn, reg, info, single_use);
	  }
	else if (GET_CODE (arg1) == PLUS
		 && REG_P (XEXP (arg1, 0))
		 && REG_P (XEXP (arg1, 1)))
	  {
	    /* Handle R1 = (R2 + R3) + ...  */
	    rtx reg1 = XEXP (arg1, 0);
	    rtx reg2 = XEXP (arg1, 1);
	    if (REGNO (reg1) != REGNO (reg2))
	      {
		offset += fold_offsets_value (insn, reg1, info, single_use);
		offset += fold_offsets_value (insn, reg2, info, single_use);
	      }
	    else
	      offset += 2 * fold_offsets_value (insn, reg1, info, single_use);
	  }
	else if (GET_CODE (arg1) == PLUS
		 && GET_CODE (XEXP (arg1, 0)) == ASHIFT
		 && REG_P (XEXP (XEXP (arg1, 0), 0))
		 && CONST_INT_P (XEXP (XEXP (arg1, 0), 1))
		 && REG_P (XEXP (arg1, 1)))
	  {
	    /* Handle R1 = ((R2 << C) + R3) + ...  */
	    rtx reg1 = XEXP (XEXP (arg1, 0), 0);
	    rtx shamt = XEXP (XEXP (arg1, 0), 1);
	    rtx reg2 = XEXP (arg1, 1);
	    HOST_WIDE_INT scale = HOST_WIDE_INT_1U << INTVAL (shamt);
	    if (REGNO (reg1) != REGNO (reg2))
	      {
		offset += scale
			 * fold_offsets_value (insn, reg1, info, single_use);
		offset += fold_offsets_value (insn, reg2, info, single_use);
	      }
	    else
	      offset += (scale + 1) * fold_offsets_value (insn, reg1, info,
						    single_use);
	  }
	else
	  return false;

	if (REG_P (arg2))
	  offset += fold_offsets_value (insn, arg2, info, single_use);
	else if (CONST_INT_P (arg2))
	  {
	    if (REG_P (arg1))
	      {
		offset += INTVAL (arg2);
		/* This is a R1 = R2 + C instruction, candidate for
		   folding.  */
		fold_agnostic = false;
	      }
	  }
	else
	  return false;

	/* Pattern recognized for folding.  */
	break;
      }
    case MINUS:
      {
	/* Propagate through minus.  */
	rtx arg1 = XEXP (src, 0);
	rtx arg2 = XEXP (src, 1);

	if (REG_P (arg1))
	  offset += fold_offsets_value (insn, arg1, info, single_use);
	else
	  return false;

	if (REG_P (arg2))
	  offset -= fold_offsets_value (insn, arg2, info, single_use);
	else if (CONST_INT_P (arg2))
	  {
	    if (REG_P (arg1))
	      {
		offset -= INTVAL (arg2);
		/* This is a R1 = R2 - C instruction, candidate for
		   folding.  */
		fold_agnostic = false;
	      }
	  }
	else
	  return false;

	/* Pattern recognized for folding.  */
	break;
      }
    case NEG:
      {
	/* Propagate through negation.  */
	rtx arg1 = XEXP (src, 0);
	if (REG_P (arg1))
	  offset = -fold_offsets_value (insn, arg1, info, single_use);
	else
	  return false;

	/* Pattern recognized for folding.  */
	break;
      }
    case MULT:
      {
	/* Propagate through multiply by constant.  */
	rtx arg1 = XEXP (src, 0);
	rtx arg2 = XEXP (src, 1);

	if (REG_P (arg1) && CONST_INT_P (arg2))
	  {
	    HOST_WIDE_INT scale = INTVAL (arg2);
	    offset = scale * fold_offsets_value (insn, arg1, info, single_use);
	  }
	else
	  return false;

	/* Pattern recognized for folding.  */
	break;
      }
    case ASHIFT:
      {
	/* Propagate through shift left by constant.  */
	rtx arg1 = XEXP (src, 0);
	rtx arg2 = XEXP (src, 1);

	if (REG_P (arg1) && CONST_INT_P (arg2))
	  {
	    HOST_WIDE_INT scale = (HOST_WIDE_INT_1U << INTVAL (arg2));
	    offset = scale * fold_offsets_value (insn, arg1, info, single_use);
	  }
	else
	  return false;

	/* Pattern recognized for folding.  */
	break;
      }
    case REG:
      {
	/* Propagate through register move.  */
	offset = fold_offsets_value (insn, src, info, single_use);

	/* Pattern recognized for folding.  */
	break;
      }
    case CONST_INT:
      {
	offset = INTVAL (src);
	/* R1 = C is candidate for folding.  */
	fold_agnostic = false;

	/* Pattern recognized for folding.  */
	break;
      }
    default:
      /* Cannot recognize.  */
      return false;
    }

  if (offset_out)
    *offset_out = offset;

  if (fold_agnostic)
    {
      if (!single_use)
	info->fold_agnostic_insns.safe_push (insn);
    }
  else if (!info->fold_insns.contains (insn))
    info->fold_insns.safe_push (insn);

  return true;
}

/* Returns true when all USEs of DEF (which defines REG) meet certain criteria
   to be foldable.  Otherwise false.  */
static bool
has_foldable_uses_p (set_info *def, rtx reg)
{
  /* We only fold through instructions that are transitively used as
     memory addresses and do not have other uses.  Use the same logic
     from offset calculation to visit instructions that can propagate
     offsets and keep track of them in CAN_FOLD_INSNS.  */
  for (use_info *use : def->nondebug_insn_uses ())
    {
      insn_info *use_insn = use->insn ();
      if (use_insn->is_artificial ())
	return false;

      /* Only handle an insn with a simple single set here.  */
      rtx_insn *use_rtl = use_insn->rtl ();
      if (!NONJUMP_INSN_P (use_rtl) || GET_CODE (PATTERN (use_rtl)) != SET)
	return false;

      /* Special case: A foldable memory store is not foldable if it
	 mentions DEST outside of the address calculation.  */
      rtx use_set = PATTERN (use_rtl);
      if (use_set && MEM_P (SET_DEST (use_set))
	  && reg_mentioned_p (reg, SET_SRC (use_set)))
	return false;

      if (use->bb () != def->bb ())
	return false;
    }

  return true;
}


/* Function that calculates the offset for INSN that would have to be added to
   all its USEs of REG.  Foldable INSNs are added to INFO->fold_insns and
   fold-agnostic INSNs are added to INFO->fold_agnostic_insns.
   It is possible that some INSNs are added to both lists; when that happens
   the INSN is a fold insn.

   On success, returns true and stores the offset in *OFFSET_OUT.  On failure,
   returns false and sets *OFFSET_OUT to 0.  */
static bool
fold_offsets (insn_info *insn, rtx reg, HOST_WIDE_INT *offset_out,
	      fold_mem_info *info, bool single_use = true)
{
  *offset_out = 0;

  /* We can only affect the values of GPR registers.  */
  unsigned int regno = REGNO (reg);
  if (fixed_regs[regno]
      || !TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], regno))
    return false;

  /* Get the DEF for REG in INSN.  */
  set_info *def = get_single_def_in_bb (insn, reg, single_use);
  if (!def)
    return false;

  insn_info *def_insn = def->insn ();
  rtx_insn *def_rtl = def_insn->rtl ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "For INSN: ");
      print_rtl_single (dump_file, insn->rtl ());
      fprintf (dump_file, "...found DEF: ");
      print_rtl_single (dump_file, def_rtl);
    }

  gcc_assert (REGNO (reg) == def->regno ());

  /* Check if all USEs of DEF are safe.  */
  if (!has_foldable_uses_p (def, reg))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "has_foldable_uses_p failed for: ");
	  print_rtl_single (dump_file, def_rtl);
	}
      return false;
    }

  /* Check if we know how to handle DEF.  */
  HOST_WIDE_INT offset;
  if (!fold_offsets_1 (def_insn, &offset, info, single_use))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "fold_offsets_1 failed for: ");
	  print_rtl_single (dump_file, def_rtl);
	}
      return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Instruction marked for propagation: ");
      print_rtl_single (dump_file, def_rtl);
    }

  *offset_out = offset;
  return true;
}

/* Check if any of the provided INSNs in INSN_LIST is not marked in the
   given bitmap.  Return true if at least one INSN is not in the bitmap and
   false otherwise.  */
static bool
insn_uses_not_in_bitmap (vec<insn_info *> *insn_list, bitmap bm)
{
  for (insn_info *insn : *insn_list)
    {
      gcc_assert (insn->num_defs () == 1);
      set_info *def = dyn_cast<set_info *>(insn->defs ()[0]);
      for (use_info *use : def->nondebug_insn_uses ())
	{
	  if (!bitmap_bit_p (bm, use->insn ()->uid ()))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Cannot ensure correct transformation as "
			   "INSN %u has a USE INSN %u that was not analysed.\n",
			   insn->uid (), use->insn ()->uid ());
		}

	      return true;
	    }
	}
    }

  return false;
}

/* Check if all USEs of all instructions have been analysed.
   If a fold_mem_info is found that has an unknown USE, then
   drop it from the list.  When this function returns all
   fold_mem_infos in the worklist reference instructions that
   have been analysed before and can therefore be committed.  */
static void
drop_unsafe_candidates (vec<fold_mem_info *> *worklist)
{
  bool changed;
  do
    {
      changed = false;

      /* First mark all analysed INSNs in a bitmap.  */
      auto_bitmap insn_closure;
      for (fold_mem_info *info : worklist)
	{
	  bitmap_set_bit (insn_closure, info->insn->uid ());
	  for (insn_info *insn : info->fold_agnostic_insns)
	    bitmap_set_bit (insn_closure, insn->uid ());
	  for (insn_info *insn : info->fold_insns)
	    bitmap_set_bit (insn_closure, insn->uid ());
	}

      /* Now check if all uses of fold_insns are marked.  */
      unsigned i;
      fold_mem_info *info;
      FOR_EACH_VEC_ELT (*worklist, i, info)
	{
	  if (insn_uses_not_in_bitmap (&info->fold_agnostic_insns, insn_closure)
	      || insn_uses_not_in_bitmap (&info->fold_insns, insn_closure))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file,
			   "Dropping fold-mem-offset root INSN %u.\n",
			   info->insn->uid ());
		}

	      /* Drop INFO from worklist and restart.  */
	      worklist->unordered_remove (i);
	      delete info;
	      changed = true;
	      break;
	    }
	}
    }
  while (changed);
}

/* If INSN is a root memory instruction that was affected by any folding
   then update its offset as necessary.  */
static rtx
do_commit_offset (fold_mem_info *info)
{
  rtx mem = info->mem;
  rtx reg = info->reg;
  HOST_WIDE_INT new_offset = info->offset + info->added_offset;

  if (info->added_offset == 0)
    return NULL_RTX;

  rtx new_mem = copy_rtx (mem);

  machine_mode mode = GET_MODE (XEXP (new_mem, 0));
  if (new_offset != 0)
    XEXP (new_mem, 0)
      = gen_rtx_PLUS (mode, reg, gen_int_mode (new_offset, mode));
  else
    XEXP (new_mem, 0) = reg;

  rtx new_insn = simplify_replace_rtx (info->insn->rtl (), mem, new_mem);

  return new_insn;
}

/* If INSN is a move / add instruction that was folded then replace its
   constant with zero.  Append the resulting insn_change to CHANGES for the
   caller to commit.  */
static rtx_insn *
do_commit_insn (insn_info *insn, auto_vec<change_info *> *changes)
{
  rtx_insn *insn_rtl = insn->rtl ();
  rtx_insn *new_insn_rtl = as_a<rtx_insn *> (copy_rtx (insn_rtl));

  /* If we deleted this INSNs before, then nothing left to do here.  */
  if (insn_rtl->deleted ())
    return NULL;

  rtx set = single_set (new_insn_rtl);
  rtx src = SET_SRC (set);

  /* Emit a move and let subsequent passes eliminate it if possible.  */
  if (CONST_INT_P (src))
    {
      /* Only change if necessary.  */
      if (INTVAL (src))
	{
	  /* INSN is R1 = C.  Set C to 0 because it was folded.  */
	  SET_SRC (set) = CONST0_RTX (GET_MODE (SET_SRC (set)));
	  change_info *change = new change_info (
				  new insn_change (insn),
				  num_validated_changes ());
	  changes->safe_push (change);

	  return new_insn_rtl;
	}
    }
  else
    {
      if (!BINARY_P (src))
	return NULL;

      rtx sec_src_op = XEXP (src, 1);

      /* Only change if necessary.  */
      if (INTVAL (sec_src_op))
	{
	  /* Mark self-assignments for deletion.  */
	  rtx dest = SET_DEST (set);
	  change_info *change = nullptr;
	  if (REGNO (dest) == REGNO (XEXP (src, 0)))
	    change = new change_info (
		       new insn_change (insn, insn_change::DELETE),
		       num_validated_changes ());
	  else
	    {
	      /* If INSN is R1 = R2 + C, C is folded to 0, so emit a mov
		 instead.  */
	      new_insn_rtl = gen_move_insn (SET_DEST (set), XEXP (src, 0));
	      change = new change_info (
			 new insn_change (insn), num_validated_changes ());
	    }

	  changes->safe_push (change);
	  return new_insn_rtl;
	}
    }

  return NULL;
}

/* Order two insn_change objects by the program order of their insns.  */

static bool
sort_changes (insn_change *a, insn_change *b)
{
  return a->insn ()->compare_with (b->insn ()) < 0;
}

/* Find and return the last definition of INSN.  */

static def_info *
get_last_def (insn_info *insn)
{
  for (def_info *def : insn->defs ())
    if (def->insn () == insn)
      return def;

  return NULL;
}

/* Move uses of DEF to the previous definition.  */

static void
move_uses_to_prev_def (def_info *def)
{
  auto set = dyn_cast<set_info *> (def);
  while (set->first_use ())
    {
      auto prev_set = dyn_cast<set_info *> (def->prev_def ());
      if (!prev_set)
	break;
      crtl->ssa->reparent_use (set->first_use (), prev_set);
    }
}

/* Cancel GROUP, lowering MIN_INDEX to CHANGE_INDEX so that cancel_changes
   drops everything the group has validated so far.  */
static void
cancel_changes_for_group (int change_index, bitmap cancelled_groups,
			  unsigned group, int *min_index)
{
  if (*min_index == -1 || change_index < *min_index)
    *min_index = change_index;
  bitmap_set_bit (cancelled_groups, group);
}

/* Store in GROUPS_TO_CANCEL the groups of CHANGES_MAP that have a change at
   or after CANCEL_MIN_INDEX, lowering it to the minimum index of each such
   group until reaching a fixed-point.  */

static void
find_groups_to_cancel (const changes_map_t &changes_map,
		       bitmap groups_to_cancel,
		       int *cancel_min_index)
{
  bool index_changed;
  do {
    index_changed = false;
    for (const auto &entry : changes_map)
      {
	int min_index = INT_MAX;
	bool cancelled_group = bitmap_bit_p (groups_to_cancel, entry.first);
	for (change_info *change : entry.second)
	  {
	    int change_index = change->change_index;
	    if (change_index < min_index)
	      min_index = change_index;

	    if (!cancelled_group && change_index >= *cancel_min_index)
	      {
		bitmap_set_bit (groups_to_cancel, entry.first);
		cancelled_group = true;
	      }
	  }

	if (cancelled_group && min_index < *cancel_min_index)
	  {
	    *cancel_min_index = min_index;
	    index_changed = true;
	    break;
	  }
      }
  }
  while (index_changed);
}

/* Free all change_info objects (and their inner insn_change) owned by
   CHANGES_INFO.  Used on early returns in update_insns before ownership
   is transferred to changes_map.  */

static void
free_changes_info (auto_vec<change_info *> &changes_info)
{
  for (change_info *ci : changes_info)
    {
      delete ci->change;
      delete ci;
    }
}

/* Update the memory offsets and constants in fold insns based on the analysis
   done in fold_mem_offsets_1.  ATTEMPT is the attempt object for the current
   changes, which are recorded in GROUPS.  */
static unsigned int
update_insns (fold_mem_info *info,
	      obstack_watermark *attempt,
	      fold_groups *groups,
	      int *cancel_min_index)
{
  insn_info *insn = info->insn;
  unsigned int stats_fold_count = 0;

  auto_vec<change_info *> changes_info;

  insn_change *change = new insn_change (insn);
  change_info *change_inf = new change_info (change, num_validated_changes ());
  int change_index = change_inf->change_index;
  changes_info.safe_push (change_inf);

  if (info->fold_insns.is_empty ())
    {
      free_changes_info (changes_info);
      return stats_fold_count;
    }

  unsigned group = groups->add_chain (info->fold_insns);
  auto_vec<change_info *> *prev_changes = groups->changes.get (group);

  /* Abort if changes for this group have been cancelled before.  */
  if (bitmap_bit_p (groups->cancelled, group))
    {
      cancel_changes_for_group (change_index, groups->cancelled, group,
				cancel_min_index);
      free_changes_info (changes_info);
      return stats_fold_count;
    }

  /* Keep a copy of insn_change elements only.  */
  auto_vec<insn_change *> changes (changes_info.length ());
  for (change_info *ci : changes_info)
    changes.quick_push (ci->change);

  auto ignore = ignore_changing_insns (changes);
  if (!rtl_ssa::restrict_movement (*change, ignore))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Restrict movement: Cannot update INSN %u.\n",
		 insn->uid ());
      cancel_changes_for_group (change_index, groups->cancelled, group,
				cancel_min_index);
      free_changes_info (changes_info);
      return stats_fold_count;
    }

  rtx new_insn = do_commit_offset (info);
  if (new_insn == NULL_RTX)
    {
      free_changes_info (changes_info);
      return stats_fold_count;
    }

  rtx_insn *insn_rtl = info->insn->rtl ();
  validate_change (insn_rtl, &PATTERN (insn_rtl), PATTERN (new_insn), 1);

  /* Check change validity and new instruction cost.  */
  if (!recog (*attempt, *change, ignore)
      || !changes_are_worthwhile (changes)
      || !crtl->ssa->verify_insn_changes (changes))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Recog/verify: Cannot update INSN %u.\n",
		 insn->uid ());
      cancel_changes_for_group (change_index, groups->cancelled, group,
				cancel_min_index);
      free_changes_info (changes_info);
      return stats_fold_count;
    }

  if (dump_file)
    fprintf (dump_file, "INSN %u: Memory offset changed from "
	 HOST_WIDE_INT_PRINT_DEC " to " HOST_WIDE_INT_PRINT_DEC ".\n",
	 insn->uid (), info->offset, info->offset + info->added_offset);

  while (!info->fold_insns.is_empty ())
    {
      insn_info *fold_insn = info->fold_insns.pop ();
      rtx_insn *fold_insn_rtl = fold_insn->rtl ();

      rtx_insn *new_fold_insn = do_commit_insn (fold_insn, &changes_info);
      if (!new_fold_insn)
	continue;

      change_info *last_change = changes_info.last ();
      changes.safe_push (last_change->change);

      std::sort (changes.begin (), changes.end (), sort_changes);

      auto ignore = ignore_changing_insns (changes);
      if (!rtl_ssa::restrict_movement (*last_change->change, ignore))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Restrict movement: Cannot update INSN %u.\n",
		     fold_insn->uid ());
	  cancel_changes_for_group (change_index, groups->cancelled, group,
				    cancel_min_index);
	  free_changes_info (changes_info);
	  return 0;
	}

      if (!changes_are_worthwhile (changes)
	  || !crtl->ssa->verify_insn_changes (changes))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Verify: Cannot update INSN %u.\n",
		     fold_insn->uid ());
	  cancel_changes_for_group (change_index, groups->cancelled, group,
				    cancel_min_index);
	  free_changes_info (changes_info);
	  return 0;
	}

      if (last_change->change->is_deletion ())
	{
	  /* Find last instruction's def.  */
	  def_info *insn_def = get_last_def (last_change->change->insn ());

	  /* Move uses of deleted instruction to the previous def.  */
	  move_uses_to_prev_def (insn_def);
	}
      else
	{
	  last_change->change_index = num_validated_changes ();
	  validate_change (fold_insn_rtl, &PATTERN (fold_insn_rtl),
			   PATTERN (new_fold_insn), 1);
	  if (!recog (*attempt, *last_change->change, ignore))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Recog: Cannot update INSN %u.\n",
			 fold_insn->uid ());
	      cancel_changes_for_group (change_index, groups->cancelled, group,
					cancel_min_index);
	      free_changes_info (changes_info);
	      return 0;
	    }
	}

      if (dump_file)
	{
	  const int last_change_uid = last_change->change->insn ()->uid ();
	  if (last_change->change->is_deletion ())
	    fprintf (dump_file, "INSN %u: Marked for deletion.\n",
		     last_change_uid);
	  else
	    fprintf (dump_file, "INSN %u: Constant set to zero.\n",
		     last_change_uid);
	}

      stats_fold_count++;
    }

  /* Add new changes to the group.  */
  if (prev_changes)
    {
      for (change_info *change : changes_info)
	if (!change_in_vec_p (*prev_changes, *change))
	  prev_changes->safe_push (change);
	else
	  {
	    delete change->change;
	    delete change;
	  }
    }
  else
    for (change_info *change : changes_info)
      {
	auto_vec<change_info *> &change_vect
	  = groups->changes.get_or_insert (group);

	if (!change_in_vec_p (change_vect, *change))
	  change_vect.safe_push (change);
	else
	  {
	    delete change->change;
	    delete change;
	  }
      }

  return stats_fold_count;
}

/* Helper function for fold_mem_offsets.  Fold memory offsets by analysing the
   DEF-USE chain.  If SINGLE_USE is true the DEFs will only have a single use,
   otherwise they can have multiple uses.  */
static unsigned int
fold_mem_offsets_1 (bool single_use)
{
  unsigned int stats_fold_count = 0;

  /* This collects the instruction changes into groups of interdependent
     def-chains, so that the change cancellation can be restricted to a single
     group if anything goes wrong.  */
  fold_groups groups;

  auto attempt = crtl->ssa->new_change_attempt ();
  insn_change_watermark watermark;

  int cancel_min_index = -1;

  /* Iterate over all nondebug INSNs get our candidates and fold them.  */
  auto_vec<fold_mem_info *> worklist;
  for (auto insn : iterate_safely (crtl->ssa->nondebug_insns ()))
    {
      if (!insn->is_real () || !insn->can_be_optimized ())
	continue;

      rtx mem, reg;
      HOST_WIDE_INT offset;
      if (!get_fold_mem_offset_root (insn, &mem, &reg, &offset))
	continue;

      fold_mem_info *info = new fold_mem_info (insn, mem, reg, offset);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Starting analysis from root: ");
	  print_rtl_single (dump_file, info->insn->rtl ());
	}

      /* Walk DEF-chain and collect info.fold_insns and the resulting
	 offset.  */
      if (!fold_offsets (info->insn, info->reg, &info->added_offset, info,
			 single_use)
	  || info->added_offset == 0)
	{
	  delete info;
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Found root offset delta: " HOST_WIDE_INT_PRINT_DEC "\n",
		 info->added_offset);

      if (single_use)
	{
	  stats_fold_count += update_insns (info, &attempt, &groups,
					    &cancel_min_index);
	  delete info;
	}
      else
	/* Append candidate.  */
	worklist.safe_push (info);
    }

  if (!single_use)
    {
      /* Now drop all fold_mem_infos, which contain INSNs that have unknown
	 USEs and are therefore not safe to change.  */
      drop_unsafe_candidates (&worklist);

      while (!worklist.is_empty ())
	{
	  fold_mem_info *info = worklist.pop ();
	  stats_fold_count += update_insns (info, &attempt, &groups,
					    &cancel_min_index);
	  delete info;
	}
    }

  /* In case that instructions have been cancelled, remove related
     instructions from the map and find the minimum index to use in
     cancel_changes.  */
  if (cancel_min_index != -1)
    {
      find_groups_to_cancel (groups.changes, groups.cancelled,
			     &cancel_min_index);

      bitmap_iterator bi;
      unsigned int key;
      EXECUTE_IF_SET_IN_BITMAP (groups.cancelled, 0, key, bi)
	{
	  auto_vec<change_info *> *changes = groups.changes.get (key);
	  if (changes)
	    {
	      for (change_info *change : *changes)
		{
		  if (dump_file)
		    fprintf (dump_file, "Change cancelled for insn %u.\n",
			     change->change->insn ()->uid ());
		  delete change->change;
		  delete change;
		}
	    }
	  groups.changes.remove (key);
	}

      cancel_changes (cancel_min_index);
    }

  if (cancel_min_index != 0)
    confirm_change_group ();

  /* change_insns wants the changes in program order, which also makes the
     result independent of the map's traversal order.  */
  auto_vec<insn_change *> live_changes;
  for (auto entry : groups.changes)
    for (change_info *ci : entry.second)
      if (ci->change->insn ()->has_been_deleted ())
	delete ci->change;
      else
	live_changes.safe_push (ci->change);

  std::sort (live_changes.begin (), live_changes.end (), sort_changes);
  crtl->ssa->change_insns (live_changes);

  for (insn_change *change : live_changes)
    delete change;

  /* Free the change_info wrappers for successful (non-cancelled) entries.
     Their inner insn_change has already been deleted above.  Cancelled
     entries were removed from the map and freed earlier.  */
  for (auto entry : groups.changes)
    for (change_info *ci : entry.second)
      delete ci;

  return stats_fold_count;
}

/* Main function of fold-mem-offsets pass.  */
static unsigned int
fold_mem_offsets (function *fn)
{
  bool multi_use_mode = true;

  /* Analysing every USE of a DEF (multi-use mode, below) is expensive on
     flow graphs which have a high connectivity and is unlikely to be
     particularly useful there, so gate it on the edge count.

     In normal circumstances a cfg should have about twice as many
     edges as blocks.  But we do not want to punish small functions
     which have a couple switch statements.  Rather than simply
     threshold the number of blocks, use something with a more
     graceful degradation.  */
  if (n_edges_for_fn (fn) > 20000 + n_basic_blocks_for_fn (fn) * 4)
    multi_use_mode = false;

  /* Initialize RTL SSA.  */
  calculate_dominance_info (CDI_DOMINATORS);
  df_analyze ();
  crtl->ssa = new rtl_ssa::function_info (cfun);

  /* The number of instructions that were simplified or eliminated.  */
  int stats_fold_count = 0;

  /* Fold mem offsets with DEFs that have a single USE.  */
  stats_fold_count += fold_mem_offsets_1 (true);

  /* Fold mem offsets with DEFs that have multiple USEs.  This expensive
     analysis is skipped for highly-connected CFGs, unless forced on with
     -fexpensive-optimizations.  As the latter is enabled by default at -O2,
     the skip only takes effect under -fno-expensive-optimizations.  */
  if (multi_use_mode || flag_expensive_optimizations)
    {
      if (dump_file)
	fprintf (dump_file, "Starting multi-use analysis\n");
      stats_fold_count += fold_mem_offsets_1 (false);
    }
  else
    warning (OPT_Wdisabled_optimization,
	     "fold-mem-offsets: disabling multi-use analysis for %d basic "
	     "blocks and %d edges/basic block",
	     n_basic_blocks_for_fn (fn),
	     n_edges_for_fn (fn) / n_basic_blocks_for_fn (fn));

  statistics_counter_event (cfun, "Number of folded instructions",
			    stats_fold_count);

  free_dominance_info (CDI_DOMINATORS);
  if (crtl->ssa->perform_pending_updates ())
    cleanup_cfg (0);

  delete crtl->ssa;
  crtl->ssa = nullptr;

  return 0;
}

const pass_data pass_data_fold_mem =
{
  RTL_PASS, /* type */
  "fold_mem_offsets", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_FOLD_MEM_OFFSETS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_fold_mem_offsets : public rtl_opt_pass
{
public:
  pass_fold_mem_offsets (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_fold_mem, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return flag_fold_mem_offsets && optimize >= 2;
    }

  unsigned int execute (function *fn) final override
    {
      return fold_mem_offsets (fn);
    }
}; // class pass_fold_mem_offsets

} // anon namespace

rtl_opt_pass *
make_pass_fold_mem_offsets (gcc::context *ctxt)
{
  return new pass_fold_mem_offsets (ctxt);
}
