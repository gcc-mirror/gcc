/* Late RTL pass to fold memory offsets.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "expr.h"
#include "backend.h"
#include "regs.h"
#include "target.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "predict.h"
#include "df.h"
#include "tree-pass.h"
#include "cfgrtl.h"

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

   This pass works on a basic block level and consists of 4 phases:

    - Phase 1 (Analysis): Find "foldable" instructions.
      Foldable instructions are those that we know how to propagate
      a constant addition through (add, shift, move, ...) and only have other
      foldable instructions for uses.  In that phase a DFS traversal on the
      definition tree is performed and foldable instructions are marked on
      a bitmap.  The add immediate instructions that are reachable in this
      DFS are candidates for folding since all the intermediate calculations
      affected by them are also foldable.

    - Phase 2 (Validity): Traverse and calculate the offsets that would result
      from folding the add immediate instructions.  Check whether the
      calculated offsets result in a valid instruction for the target.

    - Phase 3 (Commit offsets): Traverse again.  It is now known which folds
      are valid so at this point change the offsets in the memory instructions.

    - Phase 4 (Commit instruction deletions): Scan all instructions and delete
      or simplify (reduce to move) all add immediate instructions that were
      folded.

   This pass should run before hard register propagation because it creates
   register moves that we expect to be eliminated.  */

namespace {

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
  virtual bool gate (function *)
    {
      return flag_fold_mem_offsets && optimize >= 2;
    }

  virtual unsigned int execute (function *);
}; // class pass_fold_mem_offsets

/* Class that holds in FOLD_INSNS the instructions that if folded the offset
   of a memory instruction would increase by ADDED_OFFSET.  */
class fold_mem_info {
public:
  auto_bitmap fold_insns;
  HOST_WIDE_INT added_offset;
};

typedef hash_map<rtx_insn *, fold_mem_info *> fold_info_map;

/* Tracks which instructions can be reached through instructions that can
   propagate offsets for folding.  */
static bitmap_head can_fold_insns;

/* Marks instructions that are currently eligible for folding.  */
static bitmap_head candidate_fold_insns;

/* Tracks instructions that cannot be folded because it turned out that
   folding will result in creating an invalid memory instruction.
   An instruction can be in both CANDIDATE_FOLD_INSNS and CANNOT_FOLD_INSNS
   at the same time, in which case it is not legal to fold.  */
static bitmap_head cannot_fold_insns;

/* The number of instructions that were simplified or eliminated.  */
static int stats_fold_count;

/* Get the single reaching definition of an instruction inside a BB.
   The definition is desired for REG used in INSN.
   Return the definition insn or NULL if there's no definition with
   the desired criteria.  */
static rtx_insn *
get_single_def_in_bb (rtx_insn *insn, rtx reg)
{
  df_ref use;
  struct df_link *ref_chain, *ref_link;

  FOR_EACH_INSN_USE (use, insn)
    {
      if (GET_CODE (DF_REF_REG (use)) == SUBREG)
	return NULL;
      if (REGNO (DF_REF_REG (use)) == REGNO (reg))
	break;
    }

  if (!use)
    return NULL;

  ref_chain = DF_REF_CHAIN (use);

  if (!ref_chain)
    return NULL;

  for (ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting some definition for this instruction.  */
      if (ref_link->ref == NULL)
	return NULL;
      if (DF_REF_INSN_INFO (ref_link->ref) == NULL)
	return NULL;
      if (global_regs[REGNO (reg)]
	  && !set_of (reg, DF_REF_INSN (ref_link->ref)))
	return NULL;
    }

  if (ref_chain->next)
    return NULL;

  rtx_insn *def = DF_REF_INSN (ref_chain->ref);

  if (BLOCK_FOR_INSN (def) != BLOCK_FOR_INSN (insn))
    return NULL;

  if (DF_INSN_LUID (def) > DF_INSN_LUID (insn))
    return NULL;

  return def;
}

/* Get all uses of REG which is set in INSN.  Return the use list or NULL if a
   use is missing / irregular.  If SUCCESS is not NULL then set it to false if
   there are missing / irregular uses and true otherwise.  */
static df_link *
get_uses (rtx_insn *insn, rtx reg, bool *success)
{
  df_ref def;

  if (success)
    *success = false;

  FOR_EACH_INSN_DEF (def, insn)
    if (REGNO (DF_REF_REG (def)) == REGNO (reg))
      break;

  if (!def)
    return NULL;

  df_link *ref_chain = DF_REF_CHAIN (def);
  int insn_luid = DF_INSN_LUID (insn);
  basic_block insn_bb = BLOCK_FOR_INSN (insn);

  for (df_link *ref_link = ref_chain; ref_link; ref_link = ref_link->next)
    {
      /* Problem getting a use for this instruction.  */
      if (ref_link->ref == NULL)
	return NULL;
      if (DF_REF_CLASS (ref_link->ref) != DF_REF_REGULAR)
	return NULL;

      rtx_insn *use = DF_REF_INSN (ref_link->ref);
      if (DEBUG_INSN_P (use))
	continue;

      /* We do not handle REG_EQUIV/REG_EQ notes for now.  */
      if (DF_REF_FLAGS (ref_link->ref) & DF_REF_IN_NOTE)
	return NULL;
      if (BLOCK_FOR_INSN (use) != insn_bb)
	return NULL;
      /* Punt if use appears before def in the basic block.  See PR111601.  */
      if (DF_INSN_LUID (use) < insn_luid)
	return NULL;
    }

  if (success)
    *success = true;

  return ref_chain;
}

static HOST_WIDE_INT
fold_offsets (rtx_insn *insn, rtx reg, bool analyze, bitmap foldable_insns);

/*  Helper function for fold_offsets.

    If DO_RECURSION is false and ANALYZE is true this function returns true iff
    it understands the structure of INSN and knows how to propagate constants
    through it.  In this case OFFSET_OUT and FOLDABLE_INSNS are unused.

    If DO_RECURSION is true then it also calls fold_offsets for each recognized
    part of INSN with the appropriate arguments.

    If DO_RECURSION is true and ANALYZE is false then offset that would result
    from folding is computed and is returned through the pointer OFFSET_OUT.
    The instructions that can be folded are recorded in FOLDABLE_INSNS.  */
static bool
fold_offsets_1 (rtx_insn *insn, bool analyze, bool do_recursion,
		HOST_WIDE_INT *offset_out, bitmap foldable_insns)
{
  /* Doesn't make sense if both DO_RECURSION and ANALYZE are false.  */
  gcc_checking_assert (do_recursion || analyze);
  gcc_checking_assert (GET_CODE (PATTERN (insn)) == SET);

  rtx src = SET_SRC (PATTERN (insn));
  HOST_WIDE_INT offset = 0;

  switch (GET_CODE (src))
    {
    case PLUS:
      {
	/* Propagate through add.  */
	rtx arg1 = XEXP (src, 0);
	rtx arg2 = XEXP (src, 1);

	if (REG_P (arg1))
	  {
	    if (do_recursion)
	      offset += fold_offsets (insn, arg1, analyze, foldable_insns);
	  }
	else if (GET_CODE (arg1) == ASHIFT
		 && REG_P (XEXP (arg1, 0))
		 && CONST_INT_P (XEXP (arg1, 1)))
	  {
	    /* Handle R1 = (R2 << C) + ...  */
	    if (do_recursion)
	      {
		HOST_WIDE_INT scale
		  = (HOST_WIDE_INT_1U << INTVAL (XEXP (arg1, 1)));
		offset += scale * fold_offsets (insn, XEXP (arg1, 0), analyze,
						foldable_insns);
	      }
	  }
	else if (GET_CODE (arg1) == PLUS
		 && REG_P (XEXP (arg1, 0))
		 && REG_P (XEXP (arg1, 1)))
	  {
	    /* Handle R1 = (R2 + R3) + ...  */
	    if (do_recursion)
	      {
		offset += fold_offsets (insn, XEXP (arg1, 0), analyze,
					foldable_insns);
		offset += fold_offsets (insn, XEXP (arg1, 1), analyze,
					foldable_insns);
	      }
	  }
	else if (GET_CODE (arg1) == PLUS
		 && GET_CODE (XEXP (arg1, 0)) == ASHIFT
		 && REG_P (XEXP (XEXP (arg1, 0), 0))
		 && CONST_INT_P (XEXP (XEXP (arg1, 0), 1))
		 && REG_P (XEXP (arg1, 1)))
	  {
	    /* Handle R1 = ((R2 << C) + R3) + ...  */
	    if (do_recursion)
	      {
		HOST_WIDE_INT scale
		  = (HOST_WIDE_INT_1U << INTVAL (XEXP (XEXP (arg1, 0), 1)));
		offset += scale * fold_offsets (insn, XEXP (XEXP (arg1, 0), 0),
						analyze, foldable_insns);
		offset += fold_offsets (insn, XEXP (arg1, 1), analyze,
					foldable_insns);
	      }
	  }
	else
	  return false;

	if (REG_P (arg2))
	  {
	    if (do_recursion)
	      offset += fold_offsets (insn, arg2, analyze, foldable_insns);
	  }
	else if (CONST_INT_P (arg2))
	  {
	    if (REG_P (arg1))
	      {
		offset += INTVAL (arg2);
		/* This is a R1 = R2 + C instruction, candidate for folding.  */
		if (!analyze)
		  bitmap_set_bit (foldable_insns, INSN_UID (insn));
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
	  {
	    if (do_recursion)
	      offset += fold_offsets (insn, arg1, analyze, foldable_insns);
	  }
	else
	  return false;

	if (REG_P (arg2))
	  {
	    if (do_recursion)
	      offset -= fold_offsets (insn, arg2, analyze, foldable_insns);
	  }
	else if (CONST_INT_P (arg2))
	  {
	    if (REG_P (arg1))
	      {
		offset -= INTVAL (arg2);
		/* This is a R1 = R2 - C instruction, candidate for folding.  */
		if (!analyze)
		  bitmap_set_bit (foldable_insns, INSN_UID (insn));
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
	  {
	    if (do_recursion)
	      offset = -fold_offsets (insn, arg1, analyze, foldable_insns);
	  }
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
	    if (do_recursion)
	      {
		HOST_WIDE_INT scale = INTVAL (arg2);
		offset = scale * fold_offsets (insn, arg1, analyze,
					       foldable_insns);
	      }
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
	    if (do_recursion)
	      {
		HOST_WIDE_INT scale = (HOST_WIDE_INT_1U << INTVAL (arg2));
		offset = scale * fold_offsets (insn, arg1, analyze,
					       foldable_insns);
	      }
	  }
	else
	  return false;

	/* Pattern recognized for folding.  */
	break;
      }
    case REG:
      {
	/* Propagate through register move.  */
	if (do_recursion)
	  offset = fold_offsets (insn, src, analyze, foldable_insns);

	/* Pattern recognized for folding.  */
	break;
      }
    case CONST_INT:
      {
	offset = INTVAL (src);
	/* R1 = C is candidate for folding.  */
	if (!analyze)
	  bitmap_set_bit (foldable_insns, INSN_UID (insn));

	/* Pattern recognized for folding.  */
	break;
      }
    default:
      /* Cannot recognize.  */
      return false;
    }

    if (do_recursion && !analyze)
      *offset_out = offset;

    return true;
}

/* Function that computes the offset that would have to be added to all uses
   of REG if the instructions marked in FOLDABLE_INSNS were to be eliminated.

   If ANALYZE is true then mark in CAN_FOLD_INSNS which instructions
   transitively only affect other instructions found in CAN_FOLD_INSNS.
   If ANALYZE is false then compute the offset required for folding.  */
static HOST_WIDE_INT
fold_offsets (rtx_insn *insn, rtx reg, bool analyze, bitmap foldable_insns)
{
  rtx_insn *def = get_single_def_in_bb (insn, reg);

  if (!def || RTX_FRAME_RELATED_P (def) || GET_CODE (PATTERN (def)) != SET)
    return 0;

  rtx dest = SET_DEST (PATTERN (def));

  if (!REG_P (dest))
    return 0;

  /* We can only affect the values of GPR registers.  */
  unsigned int dest_regno = REGNO (dest);
  if (fixed_regs[dest_regno]
      || !TEST_HARD_REG_BIT (reg_class_contents[GENERAL_REGS], dest_regno))
    return 0;

  if (analyze)
    {
      /* Check if we know how to handle DEF.  */
      if (!fold_offsets_1 (def, true, false, NULL, NULL))
	return 0;

      /* We only fold through instructions that are transitively used as
	 memory addresses and do not have other uses.  Use the same logic
	 from offset calculation to visit instructions that can propagate
	 offsets and keep track of them in CAN_FOLD_INSNS.  */
      bool success;
      struct df_link *uses = get_uses (def, dest, &success), *ref_link;

      if (!success)
	return 0;

      for (ref_link = uses; ref_link; ref_link = ref_link->next)
	{
	  rtx_insn *use = DF_REF_INSN (ref_link->ref);

	  if (DEBUG_INSN_P (use))
	    continue;

	  /* Punt if the use is anything more complicated than a set
	     (clobber, use, etc).  */
	  if (!NONJUMP_INSN_P (use) || GET_CODE (PATTERN (use)) != SET)
	    return 0;

	  /* This use affects instructions outside of CAN_FOLD_INSNS.  */
	  if (!bitmap_bit_p (&can_fold_insns, INSN_UID (use)))
	    return 0;

	  rtx use_set = PATTERN (use);

	  /* Special case: A foldable memory store is not foldable if it
	     mentions DEST outside of the address calculation.  */
	  if (use_set && MEM_P (SET_DEST (use_set))
	      && reg_mentioned_p (dest, SET_SRC (use_set)))
	    return 0;
	}

      bitmap_set_bit (&can_fold_insns, INSN_UID (def));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Instruction marked for propagation: ");
	  print_rtl_single (dump_file, def);
	}
    }
  else
    {
      /* We cannot propagate through this instruction.  */
      if (!bitmap_bit_p (&can_fold_insns, INSN_UID (def)))
	return 0;
    }

  HOST_WIDE_INT offset = 0;
  bool recognized = fold_offsets_1 (def, analyze, true, &offset,
				    foldable_insns);

  if (!recognized)
    return 0;

  return offset;
}

/* Test if INSN is a memory load / store that can have an offset folded to it.
   Return true iff INSN is such an instruction and return through MEM_OUT,
   REG_OUT and OFFSET_OUT the RTX that has a MEM code, the register that is
   used as a base address and the offset accordingly.
   All of the out pointers may be NULL in which case they will be ignored.  */
bool
get_fold_mem_root (rtx_insn *insn, rtx *mem_out, rtx *reg_out,
		   HOST_WIDE_INT *offset_out)
{
  rtx set = single_set (insn);
  rtx mem = NULL_RTX;

  if (set != NULL_RTX)
    {
      rtx src = SET_SRC (set);
      rtx dest = SET_DEST (set);

      /* Don't fold when we have unspec / volatile.  */
      if (GET_CODE (src) == UNSPEC
	  || GET_CODE (src) == UNSPEC_VOLATILE
	  || GET_CODE (dest) == UNSPEC
	  || GET_CODE (dest) == UNSPEC_VOLATILE)
	return false;

      if (MEM_P (src))
	mem = src;
      else if (MEM_P (dest))
	mem = dest;
      else if ((GET_CODE (src) == SIGN_EXTEND
		|| GET_CODE (src) == ZERO_EXTEND)
	       && MEM_P (XEXP (src, 0)))
	mem = XEXP (src, 0);
    }

  if (mem == NULL_RTX)
    return false;

  rtx mem_addr = XEXP (mem, 0);
  rtx reg;
  HOST_WIDE_INT offset;

  if (REG_P (mem_addr))
    {
      reg = mem_addr;
      offset = 0;
    }
  else if (GET_CODE (mem_addr) == PLUS
	   && REG_P (XEXP (mem_addr, 0))
	   && CONST_INT_P (XEXP (mem_addr, 1)))
    {
      reg = XEXP (mem_addr, 0);
      offset = INTVAL (XEXP (mem_addr, 1));
    }
  else
    return false;

  if (mem_out)
    *mem_out = mem;
  if (reg_out)
    *reg_out = reg;
  if (offset_out)
    *offset_out = offset;

  return true;
}

/* If INSN is a root memory instruction then do a DFS traversal on its
   definitions and find folding candidates.  */
static void
do_analysis (rtx_insn *insn)
{
  rtx reg;
  if (!get_fold_mem_root (insn, NULL, &reg, NULL))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Starting analysis from root: ");
      print_rtl_single (dump_file, insn);
    }

  /* Analyse folding opportunities for this memory instruction.  */
  bitmap_set_bit (&can_fold_insns, INSN_UID (insn));
  fold_offsets (insn, reg, true, NULL);
}

static void
do_fold_info_calculation (rtx_insn *insn, fold_info_map *fold_info)
{
  rtx mem, reg;
  HOST_WIDE_INT cur_offset;
  if (!get_fold_mem_root (insn, &mem, &reg, &cur_offset))
    return;

  fold_mem_info *info = new fold_mem_info;
  info->added_offset = fold_offsets (insn, reg, false, info->fold_insns);

  fold_info->put (insn, info);
}

/* If INSN is a root memory instruction then compute a potentially new offset
   for it and test if the resulting instruction is valid.  */
static void
do_check_validity (rtx_insn *insn, fold_mem_info *info)
{
  rtx mem, reg;
  HOST_WIDE_INT cur_offset;
  if (!get_fold_mem_root (insn, &mem, &reg, &cur_offset))
    return;

  HOST_WIDE_INT new_offset = cur_offset + info->added_offset;

  /* Test if it is valid to change MEM's address offset to NEW_OFFSET.  */
  int icode = INSN_CODE (insn);
  INSN_CODE (insn) = -1;
  rtx mem_addr = XEXP (mem, 0);
  machine_mode mode = GET_MODE (mem_addr);
  if (new_offset != 0)
    XEXP (mem, 0) = gen_rtx_PLUS (mode, reg, gen_int_mode (new_offset, mode));
  else
    XEXP (mem, 0) = reg;

  bool illegal = insn_invalid_p (insn, false)
		 || !memory_address_addr_space_p (mode, XEXP (mem, 0),
						  MEM_ADDR_SPACE (mem));

  /* Restore the instruction.  */
  XEXP (mem, 0) = mem_addr;
  INSN_CODE (insn) = icode;

  if (illegal)
    bitmap_ior_into (&cannot_fold_insns, info->fold_insns);
  else
    bitmap_ior_into (&candidate_fold_insns, info->fold_insns);
}

static bool
compute_validity_closure (fold_info_map *fold_info)
{
  /* Let's say we have an arbitrary chain of foldable instructions xN = xN + C
     and memory operations rN that use xN as shown below.  If folding x1 in r1
     turns out to be invalid for whatever reason then it's also invalid to fold
     any of the other xN into any rN.  That means that we need the transitive
     closure of validity to determine whether we can fold a xN instruction.

     +--------------+    +-------------------+    +-------------------+
     | r1 = mem[x1] |    | r2 = mem[x1 + x2] |    | r3 = mem[x2 + x3] |   ...
     +--------------+    +-------------------+    +-------------------+
	    ^                ^       ^                ^       ^
	    |               /        |               /        |           ...
	    |              /         |              /         |
     +-------------+      /   +-------------+      /   +-------------+
     | x1 = x1 + 1 |-----+    | x2 = x2 + 1 |-----+    | x3 = x3 + 1 |--- ...
     +-------------+          +-------------+          +-------------+
	    ^                        ^                        ^
	    |                        |                        |
	   ...                      ...                      ...
  */

  /* In general three iterations should be enough for most cases, but allow up
     to five when -fexpensive-optimizations is used.  */
  int max_iters = 3 + 2 * flag_expensive_optimizations;
  for (int pass = 0; pass < max_iters; pass++)
    {
      bool made_changes = false;
      for (fold_info_map::iterator iter = fold_info->begin ();
	   iter != fold_info->end (); ++iter)
	{
	  fold_mem_info *info = (*iter).second;
	  if (bitmap_intersect_p (&cannot_fold_insns, info->fold_insns))
	    made_changes |= bitmap_ior_into (&cannot_fold_insns,
					     info->fold_insns);
	}

      if (!made_changes)
	return true;
    }

  return false;
}

/* If INSN is a root memory instruction that was affected by any folding
   then update its offset as necessary.  */
static void
do_commit_offset (rtx_insn *insn, fold_mem_info *info)
{
  rtx mem, reg;
  HOST_WIDE_INT cur_offset;
  if (!get_fold_mem_root (insn, &mem, &reg, &cur_offset))
    return;

  HOST_WIDE_INT new_offset = cur_offset + info->added_offset;

  if (new_offset == cur_offset)
    return;

  gcc_assert (!bitmap_empty_p (info->fold_insns));

  if (bitmap_intersect_p (&cannot_fold_insns, info->fold_insns))
    return;

  if (dump_file)
    {
      fprintf (dump_file, "Memory offset changed from "
	       HOST_WIDE_INT_PRINT_DEC " to " HOST_WIDE_INT_PRINT_DEC
	       " for instruction:\n", cur_offset, new_offset);
      print_rtl_single (dump_file, insn);
    }

  machine_mode mode = GET_MODE (XEXP (mem, 0));
  if (new_offset != 0)
    XEXP (mem, 0) = gen_rtx_PLUS (mode, reg, gen_int_mode (new_offset, mode));
  else
    XEXP (mem, 0) = reg;
  INSN_CODE (insn) = recog (PATTERN (insn), insn, 0);
  df_insn_rescan (insn);
}

/* If INSN is a move / add instruction that was folded then replace its
   constant part with zero.  */
static void
do_commit_insn (rtx_insn *insn)
{
  if (bitmap_bit_p (&candidate_fold_insns, INSN_UID (insn))
      && !bitmap_bit_p (&cannot_fold_insns, INSN_UID (insn)))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Instruction folded:");
	  print_rtl_single (dump_file, insn);
	}

      stats_fold_count++;

      rtx set = single_set (insn);
      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);

      /* Emit a move and let subsequent passes eliminate it if possible.  */
      if (GET_CODE (src) == CONST_INT)
	{
	  /* INSN is R1 = C.
	     Replace it with R1 = 0 because C was folded.  */
	  rtx mov_rtx
	    = gen_move_insn (dest, gen_int_mode (0, GET_MODE (dest)));
	  df_insn_rescan (emit_insn_after (mov_rtx, insn));
	}
      else
	{
	  /* INSN is R1 = R2 + C.
	     Replace it with R1 = R2 because C was folded.  */
	  rtx arg1 = XEXP (src, 0);

	  /* If the DEST == ARG1 then the move is a no-op.  */
	  if (REGNO (dest) != REGNO (arg1))
	    {
	      gcc_checking_assert (GET_MODE (dest) == GET_MODE (arg1));
	      rtx mov_rtx = gen_move_insn (dest, arg1);
	      df_insn_rescan (emit_insn_after (mov_rtx, insn));
	    }
	}

      /* Delete the original move / add instruction.  */
      delete_insn (insn);
    }
}

unsigned int
pass_fold_mem_offsets::execute (function *fn)
{
  df_set_flags (DF_EQ_NOTES + DF_RD_PRUNE_DEAD_DEFS + DF_DEFER_INSN_RESCAN);
  df_chain_add_problem (DF_UD_CHAIN + DF_DU_CHAIN);
  df_analyze ();

  bitmap_initialize (&can_fold_insns, NULL);
  bitmap_initialize (&candidate_fold_insns, NULL);
  bitmap_initialize (&cannot_fold_insns, NULL);

  stats_fold_count = 0;

  basic_block bb;
  rtx_insn *insn;
  FOR_ALL_BB_FN (bb, fn)
    {
      /* There is a conflict between this pass and RISCV's shorten-memrefs
	 pass.  For now disable folding if optimizing for size because
	 otherwise this cancels the effects of shorten-memrefs.  */
      if (optimize_bb_for_size_p (bb))
	continue;

      fold_info_map fold_info;

      bitmap_clear (&can_fold_insns);
      bitmap_clear (&candidate_fold_insns);
      bitmap_clear (&cannot_fold_insns);

      FOR_BB_INSNS (bb, insn)
	do_analysis (insn);

      FOR_BB_INSNS (bb, insn)
	do_fold_info_calculation (insn, &fold_info);

      FOR_BB_INSNS (bb, insn)
	if (fold_mem_info **info = fold_info.get (insn))
	  do_check_validity (insn, *info);

      if (compute_validity_closure (&fold_info))
	{
	  FOR_BB_INSNS (bb, insn)
	    if (fold_mem_info **info = fold_info.get (insn))
	      do_commit_offset (insn, *info);

	  FOR_BB_INSNS (bb, insn)
	    do_commit_insn (insn);
	}

      for (fold_info_map::iterator iter = fold_info.begin ();
	   iter != fold_info.end (); ++iter)
	delete (*iter).second;
    }

  statistics_counter_event (cfun, "Number of folded instructions",
			    stats_fold_count);

  bitmap_release (&can_fold_insns);
  bitmap_release (&candidate_fold_insns);
  bitmap_release (&cannot_fold_insns);

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_fold_mem_offsets (gcc::context *ctxt)
{
  return new pass_fold_mem_offsets (ctxt);
}
