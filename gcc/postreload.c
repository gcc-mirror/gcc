/* Perform simple optimizations to clean up the result of reload.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"

#include "cfgrtl.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "reload.h"
#include "cselib.h"
#include "tree-pass.h"
#include "dbgcnt.h"

#ifndef LOAD_EXTEND_OP
#define LOAD_EXTEND_OP(M) UNKNOWN
#endif

static int reload_cse_noop_set_p (rtx);
static bool reload_cse_simplify (rtx_insn *, rtx);
static void reload_cse_regs_1 (void);
static int reload_cse_simplify_set (rtx, rtx_insn *);
static int reload_cse_simplify_operands (rtx_insn *, rtx);

static void reload_combine (void);
static void reload_combine_note_use (rtx *, rtx_insn *, int, rtx);
static void reload_combine_note_store (rtx, const_rtx, void *);

static bool reload_cse_move2add (rtx_insn *);
static void move2add_note_store (rtx, const_rtx, void *);

/* Call cse / combine like post-reload optimization phases.
   FIRST is the first instruction.  */

static void
reload_cse_regs (rtx_insn *first ATTRIBUTE_UNUSED)
{
  bool moves_converted;
  reload_cse_regs_1 ();
  reload_combine ();
  moves_converted = reload_cse_move2add (first);
  if (flag_expensive_optimizations)
    {
      if (moves_converted)
	reload_combine ();
      reload_cse_regs_1 ();
    }
}

/* See whether a single set SET is a noop.  */
static int
reload_cse_noop_set_p (rtx set)
{
  if (cselib_reg_set_mode (SET_DEST (set)) != GET_MODE (SET_DEST (set)))
    return 0;

  return rtx_equal_for_cselib_p (SET_DEST (set), SET_SRC (set));
}

/* Try to simplify INSN.  Return true if the CFG may have changed.  */
static bool
reload_cse_simplify (rtx_insn *insn, rtx testreg)
{
  rtx body = PATTERN (insn);
  basic_block insn_bb = BLOCK_FOR_INSN (insn);
  unsigned insn_bb_succs = EDGE_COUNT (insn_bb->succs);

  /* If NO_FUNCTION_CSE has been set by the target, then we should not try
     to cse function calls.  */
  if (NO_FUNCTION_CSE && CALL_P (insn))
    return false;

  if (GET_CODE (body) == SET)
    {
      int count = 0;

      /* Simplify even if we may think it is a no-op.
         We may think a memory load of a value smaller than WORD_SIZE
         is redundant because we haven't taken into account possible
         implicit extension.  reload_cse_simplify_set() will bring
         this out, so it's safer to simplify before we delete.  */
      count += reload_cse_simplify_set (body, insn);

      if (!count && reload_cse_noop_set_p (body))
	{
	  if (check_for_inc_dec (insn))
	    delete_insn_and_edges (insn);
	  /* We're done with this insn.  */
	  goto done;
	}

      if (count > 0)
	apply_change_group ();
      else
	reload_cse_simplify_operands (insn, testreg);
    }
  else if (GET_CODE (body) == PARALLEL)
    {
      int i;
      int count = 0;
      rtx value = NULL_RTX;

      /* Registers mentioned in the clobber list for an asm cannot be reused
	 within the body of the asm.  Invalidate those registers now so that
	 we don't try to substitute values for them.  */
      if (asm_noperands (body) >= 0)
	{
	  for (i = XVECLEN (body, 0) - 1; i >= 0; --i)
	    {
	      rtx part = XVECEXP (body, 0, i);
	      if (GET_CODE (part) == CLOBBER && REG_P (XEXP (part, 0)))
		cselib_invalidate_rtx (XEXP (part, 0));
	    }
	}

      /* If every action in a PARALLEL is a noop, we can delete
	 the entire PARALLEL.  */
      for (i = XVECLEN (body, 0) - 1; i >= 0; --i)
	{
	  rtx part = XVECEXP (body, 0, i);
	  if (GET_CODE (part) == SET)
	    {
	      if (! reload_cse_noop_set_p (part))
		break;
	      if (REG_P (SET_DEST (part))
		  && REG_FUNCTION_VALUE_P (SET_DEST (part)))
		{
		  if (value)
		    break;
		  value = SET_DEST (part);
		}
	    }
	  else if (GET_CODE (part) != CLOBBER)
	    break;
	}

      if (i < 0)
	{
	  if (check_for_inc_dec (insn))
	    delete_insn_and_edges (insn);
	  /* We're done with this insn.  */
	  goto done;
	}

      /* It's not a no-op, but we can try to simplify it.  */
      for (i = XVECLEN (body, 0) - 1; i >= 0; --i)
	if (GET_CODE (XVECEXP (body, 0, i)) == SET)
	  count += reload_cse_simplify_set (XVECEXP (body, 0, i), insn);

      if (count > 0)
	apply_change_group ();
      else
	reload_cse_simplify_operands (insn, testreg);
    }

done:
  return (EDGE_COUNT (insn_bb->succs) != insn_bb_succs);
}

/* Do a very simple CSE pass over the hard registers.

   This function detects no-op moves where we happened to assign two
   different pseudo-registers to the same hard register, and then
   copied one to the other.  Reload will generate a useless
   instruction copying a register to itself.

   This function also detects cases where we load a value from memory
   into two different registers, and (if memory is more expensive than
   registers) changes it to simply copy the first register into the
   second register.

   Another optimization is performed that scans the operands of each
   instruction to see whether the value is already available in a
   hard register.  It then replaces the operand with the hard register
   if possible, much like an optional reload would.  */

static void
reload_cse_regs_1 (void)
{
  bool cfg_changed = false;
  basic_block bb;
  rtx_insn *insn;
  rtx testreg = gen_rtx_REG (word_mode, LAST_VIRTUAL_REGISTER + 1);

  cselib_init (CSELIB_RECORD_MEMORY);
  init_alias_analysis ();

  FOR_EACH_BB_FN (bb, cfun)
    FOR_BB_INSNS (bb, insn)
      {
	if (INSN_P (insn))
	  cfg_changed |= reload_cse_simplify (insn, testreg);

	cselib_process_insn (insn);
      }

  /* Clean up.  */
  end_alias_analysis ();
  cselib_finish ();
  if (cfg_changed)
    cleanup_cfg (0);
}

/* Try to simplify a single SET instruction.  SET is the set pattern.
   INSN is the instruction it came from.
   This function only handles one case: if we set a register to a value
   which is not a register, we try to find that value in some other register
   and change the set into a register copy.  */

static int
reload_cse_simplify_set (rtx set, rtx_insn *insn)
{
  int did_change = 0;
  int dreg;
  rtx src;
  reg_class_t dclass;
  int old_cost;
  cselib_val *val;
  struct elt_loc_list *l;
  enum rtx_code extend_op = UNKNOWN;
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));

  dreg = true_regnum (SET_DEST (set));
  if (dreg < 0)
    return 0;

  src = SET_SRC (set);
  if (side_effects_p (src) || true_regnum (src) >= 0)
    return 0;

  dclass = REGNO_REG_CLASS (dreg);

  /* When replacing a memory with a register, we need to honor assumptions
     that combine made wrt the contents of sign bits.  We'll do this by
     generating an extend instruction instead of a reg->reg copy.  Thus
     the destination must be a register that we can widen.  */
  if (MEM_P (src)
      && GET_MODE_BITSIZE (GET_MODE (src)) < BITS_PER_WORD
      && (extend_op = LOAD_EXTEND_OP (GET_MODE (src))) != UNKNOWN
      && !REG_P (SET_DEST (set)))
    return 0;

  val = cselib_lookup (src, GET_MODE (SET_DEST (set)), 0, VOIDmode);
  if (! val)
    return 0;

  /* If memory loads are cheaper than register copies, don't change them.  */
  if (MEM_P (src))
    old_cost = memory_move_cost (GET_MODE (src), dclass, true);
  else if (REG_P (src))
    old_cost = register_move_cost (GET_MODE (src),
				   REGNO_REG_CLASS (REGNO (src)), dclass);
  else
    old_cost = set_src_cost (src, GET_MODE (SET_DEST (set)), speed);

  for (l = val->locs; l; l = l->next)
    {
      rtx this_rtx = l->loc;
      int this_cost;

      if (CONSTANT_P (this_rtx) && ! references_value_p (this_rtx, 0))
	{
	  if (extend_op != UNKNOWN)
	    {
	      wide_int result;

	      if (!CONST_SCALAR_INT_P (this_rtx))
		continue;

	      switch (extend_op)
		{
		case ZERO_EXTEND:
		  result = wide_int::from (std::make_pair (this_rtx,
							   GET_MODE (src)),
					   BITS_PER_WORD, UNSIGNED);
		  break;
		case SIGN_EXTEND:
		  result = wide_int::from (std::make_pair (this_rtx,
							   GET_MODE (src)),
					   BITS_PER_WORD, SIGNED);
		  break;
		default:
		  gcc_unreachable ();
		}
	      this_rtx = immed_wide_int_const (result, word_mode);
	    }

	  this_cost = set_src_cost (this_rtx, GET_MODE (SET_DEST (set)), speed);
	}
      else if (REG_P (this_rtx))
	{
	  if (extend_op != UNKNOWN)
	    {
	      this_rtx = gen_rtx_fmt_e (extend_op, word_mode, this_rtx);
	      this_cost = set_src_cost (this_rtx, word_mode, speed);
	    }
	  else
	    this_cost = register_move_cost (GET_MODE (this_rtx),
					    REGNO_REG_CLASS (REGNO (this_rtx)),
					    dclass);
	}
      else
	continue;

      /* If equal costs, prefer registers over anything else.  That
	 tends to lead to smaller instructions on some machines.  */
      if (this_cost < old_cost
	  || (this_cost == old_cost
	      && REG_P (this_rtx)
	      && !REG_P (SET_SRC (set))))
	{
	  if (GET_MODE_BITSIZE (GET_MODE (SET_DEST (set))) < BITS_PER_WORD
	      && extend_op != UNKNOWN
#ifdef CANNOT_CHANGE_MODE_CLASS
	      && !CANNOT_CHANGE_MODE_CLASS (GET_MODE (SET_DEST (set)),
					    word_mode,
					    REGNO_REG_CLASS (REGNO (SET_DEST (set))))
#endif
	      )
	    {
	      rtx wide_dest = gen_rtx_REG (word_mode, REGNO (SET_DEST (set)));
	      ORIGINAL_REGNO (wide_dest) = ORIGINAL_REGNO (SET_DEST (set));
	      validate_change (insn, &SET_DEST (set), wide_dest, 1);
	    }

	  validate_unshare_change (insn, &SET_SRC (set), this_rtx, 1);
	  old_cost = this_cost, did_change = 1;
	}
    }

  return did_change;
}

/* Try to replace operands in INSN with equivalent values that are already
   in registers.  This can be viewed as optional reloading.

   For each non-register operand in the insn, see if any hard regs are
   known to be equivalent to that operand.  Record the alternatives which
   can accept these hard registers.  Among all alternatives, select the
   ones which are better or equal to the one currently matching, where
   "better" is in terms of '?' and '!' constraints.  Among the remaining
   alternatives, select the one which replaces most operands with
   hard registers.  */

static int
reload_cse_simplify_operands (rtx_insn *insn, rtx testreg)
{
  int i, j;

  /* For each operand, all registers that are equivalent to it.  */
  HARD_REG_SET equiv_regs[MAX_RECOG_OPERANDS];

  const char *constraints[MAX_RECOG_OPERANDS];

  /* Vector recording how bad an alternative is.  */
  int *alternative_reject;
  /* Vector recording how many registers can be introduced by choosing
     this alternative.  */
  int *alternative_nregs;
  /* Array of vectors recording, for each operand and each alternative,
     which hard register to substitute, or -1 if the operand should be
     left as it is.  */
  int *op_alt_regno[MAX_RECOG_OPERANDS];
  /* Array of alternatives, sorted in order of decreasing desirability.  */
  int *alternative_order;

  extract_constrain_insn (insn);

  if (recog_data.n_alternatives == 0 || recog_data.n_operands == 0)
    return 0;

  alternative_reject = XALLOCAVEC (int, recog_data.n_alternatives);
  alternative_nregs = XALLOCAVEC (int, recog_data.n_alternatives);
  alternative_order = XALLOCAVEC (int, recog_data.n_alternatives);
  memset (alternative_reject, 0, recog_data.n_alternatives * sizeof (int));
  memset (alternative_nregs, 0, recog_data.n_alternatives * sizeof (int));

  /* For each operand, find out which regs are equivalent.  */
  for (i = 0; i < recog_data.n_operands; i++)
    {
      cselib_val *v;
      struct elt_loc_list *l;
      rtx op;

      CLEAR_HARD_REG_SET (equiv_regs[i]);

      /* cselib blows up on CODE_LABELs.  Trying to fix that doesn't seem
	 right, so avoid the problem here.  Likewise if we have a constant
         and the insn pattern doesn't tell us the mode we need.  */
      if (LABEL_P (recog_data.operand[i])
	  || (CONSTANT_P (recog_data.operand[i])
	      && recog_data.operand_mode[i] == VOIDmode))
	continue;

      op = recog_data.operand[i];
      if (MEM_P (op)
	  && GET_MODE_BITSIZE (GET_MODE (op)) < BITS_PER_WORD
	  && LOAD_EXTEND_OP (GET_MODE (op)) != UNKNOWN)
	{
	  rtx set = single_set (insn);

	  /* We might have multiple sets, some of which do implicit
	     extension.  Punt on this for now.  */
	  if (! set)
	    continue;
	  /* If the destination is also a MEM or a STRICT_LOW_PART, no
	     extension applies.
	     Also, if there is an explicit extension, we don't have to
	     worry about an implicit one.  */
	  else if (MEM_P (SET_DEST (set))
		   || GET_CODE (SET_DEST (set)) == STRICT_LOW_PART
		   || GET_CODE (SET_SRC (set)) == ZERO_EXTEND
		   || GET_CODE (SET_SRC (set)) == SIGN_EXTEND)
	    ; /* Continue ordinary processing.  */
#ifdef CANNOT_CHANGE_MODE_CLASS
	  /* If the register cannot change mode to word_mode, it follows that
	     it cannot have been used in word_mode.  */
	  else if (REG_P (SET_DEST (set))
		   && CANNOT_CHANGE_MODE_CLASS (GET_MODE (SET_DEST (set)),
						word_mode,
						REGNO_REG_CLASS (REGNO (SET_DEST (set)))))
	    ; /* Continue ordinary processing.  */
#endif
	  /* If this is a straight load, make the extension explicit.  */
	  else if (REG_P (SET_DEST (set))
		   && recog_data.n_operands == 2
		   && SET_SRC (set) == op
		   && SET_DEST (set) == recog_data.operand[1-i])
	    {
	      validate_change (insn, recog_data.operand_loc[i],
			       gen_rtx_fmt_e (LOAD_EXTEND_OP (GET_MODE (op)),
					      word_mode, op),
			       1);
	      validate_change (insn, recog_data.operand_loc[1-i],
			       gen_rtx_REG (word_mode, REGNO (SET_DEST (set))),
			       1);
	      if (! apply_change_group ())
		return 0;
	      return reload_cse_simplify_operands (insn, testreg);
	    }
	  else
	    /* ??? There might be arithmetic operations with memory that are
	       safe to optimize, but is it worth the trouble?  */
	    continue;
	}

      if (side_effects_p (op))
	continue;
      v = cselib_lookup (op, recog_data.operand_mode[i], 0, VOIDmode);
      if (! v)
	continue;

      for (l = v->locs; l; l = l->next)
	if (REG_P (l->loc))
	  SET_HARD_REG_BIT (equiv_regs[i], REGNO (l->loc));
    }

  alternative_mask preferred = get_preferred_alternatives (insn);
  for (i = 0; i < recog_data.n_operands; i++)
    {
      machine_mode mode;
      int regno;
      const char *p;

      op_alt_regno[i] = XALLOCAVEC (int, recog_data.n_alternatives);
      for (j = 0; j < recog_data.n_alternatives; j++)
	op_alt_regno[i][j] = -1;

      p = constraints[i] = recog_data.constraints[i];
      mode = recog_data.operand_mode[i];

      /* Add the reject values for each alternative given by the constraints
	 for this operand.  */
      j = 0;
      while (*p != '\0')
	{
	  char c = *p++;
	  if (c == ',')
	    j++;
	  else if (c == '?')
	    alternative_reject[j] += 3;
	  else if (c == '!')
	    alternative_reject[j] += 300;
	}

      /* We won't change operands which are already registers.  We
	 also don't want to modify output operands.  */
      regno = true_regnum (recog_data.operand[i]);
      if (regno >= 0
	  || constraints[i][0] == '='
	  || constraints[i][0] == '+')
	continue;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  enum reg_class rclass = NO_REGS;

	  if (! TEST_HARD_REG_BIT (equiv_regs[i], regno))
	    continue;

	  set_mode_and_regno (testreg, mode, regno);

	  /* We found a register equal to this operand.  Now look for all
	     alternatives that can accept this register and have not been
	     assigned a register they can use yet.  */
	  j = 0;
	  p = constraints[i];
	  for (;;)
	    {
	      char c = *p;

	      switch (c)
		{
		case 'g':
		  rclass = reg_class_subunion[rclass][GENERAL_REGS];
		  break;

		default:
		  rclass
		    = (reg_class_subunion
		       [rclass]
		       [reg_class_for_constraint (lookup_constraint (p))]);
		  break;

		case ',': case '\0':
		  /* See if REGNO fits this alternative, and set it up as the
		     replacement register if we don't have one for this
		     alternative yet and the operand being replaced is not
		     a cheap CONST_INT.  */
		  if (op_alt_regno[i][j] == -1
		      && TEST_BIT (preferred, j)
		      && reg_fits_class_p (testreg, rclass, 0, mode)
		      && (!CONST_INT_P (recog_data.operand[i])
			  || (set_src_cost (recog_data.operand[i], mode,
					    optimize_bb_for_speed_p
					     (BLOCK_FOR_INSN (insn)))
			      > set_src_cost (testreg, mode,
					      optimize_bb_for_speed_p
					       (BLOCK_FOR_INSN (insn))))))
		    {
		      alternative_nregs[j]++;
		      op_alt_regno[i][j] = regno;
		    }
		  j++;
		  rclass = NO_REGS;
		  break;
		}
	      p += CONSTRAINT_LEN (c, p);

	      if (c == '\0')
		break;
	    }
	}
    }

  /* Record all alternatives which are better or equal to the currently
     matching one in the alternative_order array.  */
  for (i = j = 0; i < recog_data.n_alternatives; i++)
    if (alternative_reject[i] <= alternative_reject[which_alternative])
      alternative_order[j++] = i;
  recog_data.n_alternatives = j;

  /* Sort it.  Given a small number of alternatives, a dumb algorithm
     won't hurt too much.  */
  for (i = 0; i < recog_data.n_alternatives - 1; i++)
    {
      int best = i;
      int best_reject = alternative_reject[alternative_order[i]];
      int best_nregs = alternative_nregs[alternative_order[i]];

      for (j = i + 1; j < recog_data.n_alternatives; j++)
	{
	  int this_reject = alternative_reject[alternative_order[j]];
	  int this_nregs = alternative_nregs[alternative_order[j]];

	  if (this_reject < best_reject
	      || (this_reject == best_reject && this_nregs > best_nregs))
	    {
	      best = j;
	      best_reject = this_reject;
	      best_nregs = this_nregs;
	    }
	}

      std::swap (alternative_order[best], alternative_order[i]);
    }

  /* Substitute the operands as determined by op_alt_regno for the best
     alternative.  */
  j = alternative_order[0];

  for (i = 0; i < recog_data.n_operands; i++)
    {
      machine_mode mode = recog_data.operand_mode[i];
      if (op_alt_regno[i][j] == -1)
	continue;

      validate_change (insn, recog_data.operand_loc[i],
		       gen_rtx_REG (mode, op_alt_regno[i][j]), 1);
    }

  for (i = recog_data.n_dups - 1; i >= 0; i--)
    {
      int op = recog_data.dup_num[i];
      machine_mode mode = recog_data.operand_mode[op];

      if (op_alt_regno[op][j] == -1)
	continue;

      validate_change (insn, recog_data.dup_loc[i],
		       gen_rtx_REG (mode, op_alt_regno[op][j]), 1);
    }

  return apply_change_group ();
}

/* If reload couldn't use reg+reg+offset addressing, try to use reg+reg
   addressing now.
   This code might also be useful when reload gave up on reg+reg addressing
   because of clashes between the return register and INDEX_REG_CLASS.  */

/* The maximum number of uses of a register we can keep track of to
   replace them with reg+reg addressing.  */
#define RELOAD_COMBINE_MAX_USES 16

/* Describes a recorded use of a register.  */
struct reg_use
{
  /* The insn where a register has been used.  */
  rtx_insn *insn;
  /* Points to the memory reference enclosing the use, if any, NULL_RTX
     otherwise.  */
  rtx containing_mem;
  /* Location of the register within INSN.  */
  rtx *usep;
  /* The reverse uid of the insn.  */
  int ruid;
};

/* If the register is used in some unknown fashion, USE_INDEX is negative.
   If it is dead, USE_INDEX is RELOAD_COMBINE_MAX_USES, and STORE_RUID
   indicates where it is first set or clobbered.
   Otherwise, USE_INDEX is the index of the last encountered use of the
   register (which is first among these we have seen since we scan backwards).
   USE_RUID indicates the first encountered, i.e. last, of these uses.
   If ALL_OFFSETS_MATCH is true, all encountered uses were inside a PLUS
   with a constant offset; OFFSET contains this constant in that case.
   STORE_RUID is always meaningful if we only want to use a value in a
   register in a different place: it denotes the next insn in the insn
   stream (i.e. the last encountered) that sets or clobbers the register.
   REAL_STORE_RUID is similar, but clobbers are ignored when updating it.  */
static struct
  {
    struct reg_use reg_use[RELOAD_COMBINE_MAX_USES];
    rtx offset;
    int use_index;
    int store_ruid;
    int real_store_ruid;
    int use_ruid;
    bool all_offsets_match;
  } reg_state[FIRST_PSEUDO_REGISTER];

/* Reverse linear uid.  This is increased in reload_combine while scanning
   the instructions from last to first.  It is used to set last_label_ruid
   and the store_ruid / use_ruid fields in reg_state.  */
static int reload_combine_ruid;

/* The RUID of the last label we encountered in reload_combine.  */
static int last_label_ruid;

/* The RUID of the last jump we encountered in reload_combine.  */
static int last_jump_ruid;

/* The register numbers of the first and last index register.  A value of
   -1 in LAST_INDEX_REG indicates that we've previously computed these
   values and found no suitable index registers.  */
static int first_index_reg = -1;
static int last_index_reg;

#define LABEL_LIVE(LABEL) \
  (label_live[CODE_LABEL_NUMBER (LABEL) - min_labelno])

/* Subroutine of reload_combine_split_ruids, called to fix up a single
   ruid pointed to by *PRUID if it is higher than SPLIT_RUID.  */

static inline void
reload_combine_split_one_ruid (int *pruid, int split_ruid)
{
  if (*pruid > split_ruid)
    (*pruid)++;
}

/* Called when we insert a new insn in a position we've already passed in
   the scan.  Examine all our state, increasing all ruids that are higher
   than SPLIT_RUID by one in order to make room for a new insn.  */

static void
reload_combine_split_ruids (int split_ruid)
{
  unsigned i;

  reload_combine_split_one_ruid (&reload_combine_ruid, split_ruid);
  reload_combine_split_one_ruid (&last_label_ruid, split_ruid);
  reload_combine_split_one_ruid (&last_jump_ruid, split_ruid);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j, idx = reg_state[i].use_index;
      reload_combine_split_one_ruid (&reg_state[i].use_ruid, split_ruid);
      reload_combine_split_one_ruid (&reg_state[i].store_ruid, split_ruid);
      reload_combine_split_one_ruid (&reg_state[i].real_store_ruid,
				     split_ruid);
      if (idx < 0)
	continue;
      for (j = idx; j < RELOAD_COMBINE_MAX_USES; j++)
	{
	  reload_combine_split_one_ruid (&reg_state[i].reg_use[j].ruid,
					 split_ruid);
	}
    }
}

/* Called when we are about to rescan a previously encountered insn with
   reload_combine_note_use after modifying some part of it.  This clears all
   information about uses in that particular insn.  */

static void
reload_combine_purge_insn_uses (rtx_insn *insn)
{
  unsigned i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      int j, k, idx = reg_state[i].use_index;
      if (idx < 0)
	continue;
      j = k = RELOAD_COMBINE_MAX_USES;
      while (j-- > idx)
	{
	  if (reg_state[i].reg_use[j].insn != insn)
	    {
	      k--;
	      if (k != j)
		reg_state[i].reg_use[k] = reg_state[i].reg_use[j];
	    }
	}
      reg_state[i].use_index = k;
    }
}

/* Called when we need to forget about all uses of REGNO after an insn
   which is identified by RUID.  */

static void
reload_combine_purge_reg_uses_after_ruid (unsigned regno, int ruid)
{
  int j, k, idx = reg_state[regno].use_index;
  if (idx < 0)
    return;
  j = k = RELOAD_COMBINE_MAX_USES;
  while (j-- > idx)
    {
      if (reg_state[regno].reg_use[j].ruid >= ruid)
	{
	  k--;
	  if (k != j)
	    reg_state[regno].reg_use[k] = reg_state[regno].reg_use[j];
	}
    }
  reg_state[regno].use_index = k;
}

/* Find the use of REGNO with the ruid that is highest among those
   lower than RUID_LIMIT, and return it if it is the only use of this
   reg in the insn.  Return NULL otherwise.  */

static struct reg_use *
reload_combine_closest_single_use (unsigned regno, int ruid_limit)
{
  int i, best_ruid = 0;
  int use_idx = reg_state[regno].use_index;
  struct reg_use *retval;

  if (use_idx < 0)
    return NULL;
  retval = NULL;
  for (i = use_idx; i < RELOAD_COMBINE_MAX_USES; i++)
    {
      struct reg_use *use = reg_state[regno].reg_use + i; 
      int this_ruid = use->ruid;
      if (this_ruid >= ruid_limit)
	continue;
      if (this_ruid > best_ruid)
	{
	  best_ruid = this_ruid;
	  retval = use;
	}
      else if (this_ruid == best_ruid)
	retval = NULL;
    }
  if (last_label_ruid >= best_ruid)
    return NULL;
  return retval;
}

/* After we've moved an add insn, fix up any debug insns that occur
   between the old location of the add and the new location.  REG is
   the destination register of the add insn; REPLACEMENT is the
   SET_SRC of the add.  FROM and TO specify the range in which we
   should make this change on debug insns.  */

static void
fixup_debug_insns (rtx reg, rtx replacement, rtx_insn *from, rtx_insn *to)
{
  rtx_insn *insn;
  for (insn = from; insn != to; insn = NEXT_INSN (insn))
    {
      rtx t;

      if (!DEBUG_INSN_P (insn))
	continue;
      
      t = INSN_VAR_LOCATION_LOC (insn);
      t = simplify_replace_rtx (t, reg, replacement);
      validate_change (insn, &INSN_VAR_LOCATION_LOC (insn), t, 0);
    }
}

/* Subroutine of reload_combine_recognize_const_pattern.  Try to replace REG
   with SRC in the insn described by USE, taking costs into account.  Return
   true if we made the replacement.  */

static bool
try_replace_in_use (struct reg_use *use, rtx reg, rtx src)
{
  rtx_insn *use_insn = use->insn;
  rtx mem = use->containing_mem;
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (use_insn));

  if (mem != NULL_RTX)
    {
      addr_space_t as = MEM_ADDR_SPACE (mem);
      rtx oldaddr = XEXP (mem, 0);
      rtx newaddr = NULL_RTX;
      int old_cost = address_cost (oldaddr, GET_MODE (mem), as, speed);
      int new_cost;

      newaddr = simplify_replace_rtx (oldaddr, reg, src);
      if (memory_address_addr_space_p (GET_MODE (mem), newaddr, as))
	{
	  XEXP (mem, 0) = newaddr;
	  new_cost = address_cost (newaddr, GET_MODE (mem), as, speed);
	  XEXP (mem, 0) = oldaddr;
	  if (new_cost <= old_cost
	      && validate_change (use_insn,
				  &XEXP (mem, 0), newaddr, 0))
	    return true;
	}
    }
  else
    {
      rtx new_set = single_set (use_insn);
      if (new_set
	  && REG_P (SET_DEST (new_set))
	  && GET_CODE (SET_SRC (new_set)) == PLUS
	  && REG_P (XEXP (SET_SRC (new_set), 0))
	  && CONSTANT_P (XEXP (SET_SRC (new_set), 1)))
	{
	  rtx new_src;
	  machine_mode mode = GET_MODE (SET_DEST (new_set));
	  int old_cost = set_src_cost (SET_SRC (new_set), mode, speed);

	  gcc_assert (rtx_equal_p (XEXP (SET_SRC (new_set), 0), reg));
	  new_src = simplify_replace_rtx (SET_SRC (new_set), reg, src);

	  if (set_src_cost (new_src, mode, speed) <= old_cost
	      && validate_change (use_insn, &SET_SRC (new_set),
				  new_src, 0))
	    return true;
	}
    }
  return false;
}

/* Called by reload_combine when scanning INSN.  This function tries to detect
   patterns where a constant is added to a register, and the result is used
   in an address.
   Return true if no further processing is needed on INSN; false if it wasn't
   recognized and should be handled normally.  */

static bool
reload_combine_recognize_const_pattern (rtx_insn *insn)
{
  int from_ruid = reload_combine_ruid;
  rtx set, pat, reg, src, addreg;
  unsigned int regno;
  struct reg_use *use;
  bool must_move_add;
  rtx_insn *add_moved_after_insn = NULL;
  int add_moved_after_ruid = 0;
  int clobbered_regno = -1;

  set = single_set (insn);
  if (set == NULL_RTX)
    return false;

  reg = SET_DEST (set);
  src = SET_SRC (set);
  if (!REG_P (reg)
      || REG_NREGS (reg) != 1
      || GET_MODE (reg) != Pmode
      || reg == stack_pointer_rtx)
    return false;

  regno = REGNO (reg);

  /* We look for a REG1 = REG2 + CONSTANT insn, followed by either
     uses of REG1 inside an address, or inside another add insn.  If
     possible and profitable, merge the addition into subsequent
     uses.  */
  if (GET_CODE (src) != PLUS
      || !REG_P (XEXP (src, 0))
      || !CONSTANT_P (XEXP (src, 1)))
    return false;

  addreg = XEXP (src, 0);
  must_move_add = rtx_equal_p (reg, addreg);

  pat = PATTERN (insn);
  if (must_move_add && set != pat)
    {
      /* We have to be careful when moving the add; apart from the
	 single_set there may also be clobbers.  Recognize one special
	 case, that of one clobber alongside the set (likely a clobber
	 of the CC register).  */
      gcc_assert (GET_CODE (PATTERN (insn)) == PARALLEL);
      if (XVECLEN (pat, 0) != 2 || XVECEXP (pat, 0, 0) != set
	  || GET_CODE (XVECEXP (pat, 0, 1)) != CLOBBER
	  || !REG_P (XEXP (XVECEXP (pat, 0, 1), 0)))
	return false;
      clobbered_regno = REGNO (XEXP (XVECEXP (pat, 0, 1), 0));
    }

  do
    {
      use = reload_combine_closest_single_use (regno, from_ruid);

      if (use)
	/* Start the search for the next use from here.  */
	from_ruid = use->ruid;

      if (use && GET_MODE (*use->usep) == Pmode)
	{
	  bool delete_add = false;
	  rtx_insn *use_insn = use->insn;
	  int use_ruid = use->ruid;

	  /* Avoid moving the add insn past a jump.  */
	  if (must_move_add && use_ruid <= last_jump_ruid)
	    break;

	  /* If the add clobbers another hard reg in parallel, don't move
	     it past a real set of this hard reg.  */
	  if (must_move_add && clobbered_regno >= 0
	      && reg_state[clobbered_regno].real_store_ruid >= use_ruid)
	    break;

	  /* Do not separate cc0 setter and cc0 user on HAVE_cc0 targets.  */
	  if (HAVE_cc0 && must_move_add && sets_cc0_p (PATTERN (use_insn)))
	    break;

	  gcc_assert (reg_state[regno].store_ruid <= use_ruid);
	  /* Avoid moving a use of ADDREG past a point where it is stored.  */
	  if (reg_state[REGNO (addreg)].store_ruid > use_ruid)
	    break;

	  /* We also must not move the addition past an insn that sets
	     the same register, unless we can combine two add insns.  */
	  if (must_move_add && reg_state[regno].store_ruid == use_ruid)
	    {
	      if (use->containing_mem == NULL_RTX)
		delete_add = true;
	      else
		break;
	    }

	  if (try_replace_in_use (use, reg, src))
	    {
	      reload_combine_purge_insn_uses (use_insn);
	      reload_combine_note_use (&PATTERN (use_insn), use_insn,
				       use_ruid, NULL_RTX);

	      if (delete_add)
		{
		  fixup_debug_insns (reg, src, insn, use_insn);
		  delete_insn (insn);
		  return true;
		}
	      if (must_move_add)
		{
		  add_moved_after_insn = use_insn;
		  add_moved_after_ruid = use_ruid;
		}
	      continue;
	    }
	}
      /* If we get here, we couldn't handle this use.  */
      if (must_move_add)
	break;
    }
  while (use);

  if (!must_move_add || add_moved_after_insn == NULL_RTX)
    /* Process the add normally.  */
    return false;

  fixup_debug_insns (reg, src, insn, add_moved_after_insn);

  reorder_insns (insn, insn, add_moved_after_insn);
  reload_combine_purge_reg_uses_after_ruid (regno, add_moved_after_ruid);
  reload_combine_split_ruids (add_moved_after_ruid - 1);
  reload_combine_note_use (&PATTERN (insn), insn,
			   add_moved_after_ruid, NULL_RTX);
  reg_state[regno].store_ruid = add_moved_after_ruid;

  return true;
}

/* Called by reload_combine when scanning INSN.  Try to detect a pattern we
   can handle and improve.  Return true if no further processing is needed on
   INSN; false if it wasn't recognized and should be handled normally.  */

static bool
reload_combine_recognize_pattern (rtx_insn *insn)
{
  rtx set, reg, src;

  set = single_set (insn);
  if (set == NULL_RTX)
    return false;

  reg = SET_DEST (set);
  src = SET_SRC (set);
  if (!REG_P (reg) || REG_NREGS (reg) != 1)
    return false;

  unsigned int regno = REGNO (reg);
  machine_mode mode = GET_MODE (reg);

  if (reg_state[regno].use_index < 0
      || reg_state[regno].use_index >= RELOAD_COMBINE_MAX_USES)
    return false;

  for (int i = reg_state[regno].use_index;
       i < RELOAD_COMBINE_MAX_USES; i++)
    {
      struct reg_use *use = reg_state[regno].reg_use + i;
      if (GET_MODE (*use->usep) != mode)
	return false;
    }

  /* Look for (set (REGX) (CONST_INT))
     (set (REGX) (PLUS (REGX) (REGY)))
     ...
     ... (MEM (REGX)) ...
     and convert it to
     (set (REGZ) (CONST_INT))
     ...
     ... (MEM (PLUS (REGZ) (REGY)))... .

     First, check that we have (set (REGX) (PLUS (REGX) (REGY)))
     and that we know all uses of REGX before it dies.
     Also, explicitly check that REGX != REGY; our life information
     does not yet show whether REGY changes in this insn.  */

  if (GET_CODE (src) == PLUS
      && reg_state[regno].all_offsets_match
      && last_index_reg != -1
      && REG_P (XEXP (src, 1))
      && rtx_equal_p (XEXP (src, 0), reg)
      && !rtx_equal_p (XEXP (src, 1), reg)
      && last_label_ruid < reg_state[regno].use_ruid)
    {
      rtx base = XEXP (src, 1);
      rtx_insn *prev = prev_nonnote_nondebug_insn (insn);
      rtx prev_set = prev ? single_set (prev) : NULL_RTX;
      rtx index_reg = NULL_RTX;
      rtx reg_sum = NULL_RTX;
      int i;

      /* Now we need to set INDEX_REG to an index register (denoted as
	 REGZ in the illustration above) and REG_SUM to the expression
	 register+register that we want to use to substitute uses of REG
	 (typically in MEMs) with.  First check REG and BASE for being
	 index registers; we can use them even if they are not dead.  */
      if (TEST_HARD_REG_BIT (reg_class_contents[INDEX_REG_CLASS], regno)
	  || TEST_HARD_REG_BIT (reg_class_contents[INDEX_REG_CLASS],
				REGNO (base)))
	{
	  index_reg = reg;
	  reg_sum = src;
	}
      else
	{
	  /* Otherwise, look for a free index register.  Since we have
	     checked above that neither REG nor BASE are index registers,
	     if we find anything at all, it will be different from these
	     two registers.  */
	  for (i = first_index_reg; i <= last_index_reg; i++)
	    {
	      if (TEST_HARD_REG_BIT (reg_class_contents[INDEX_REG_CLASS], i)
		  && reg_state[i].use_index == RELOAD_COMBINE_MAX_USES
		  && reg_state[i].store_ruid <= reg_state[regno].use_ruid
		  && (call_used_regs[i] || df_regs_ever_live_p (i))
		  && (!frame_pointer_needed || i != HARD_FRAME_POINTER_REGNUM)
		  && !fixed_regs[i] && !global_regs[i]
		  && hard_regno_nregs[i][GET_MODE (reg)] == 1
		  && targetm.hard_regno_scratch_ok (i))
		{
		  index_reg = gen_rtx_REG (GET_MODE (reg), i);
		  reg_sum = gen_rtx_PLUS (GET_MODE (reg), index_reg, base);
		  break;
		}
	    }
	}

      /* Check that PREV_SET is indeed (set (REGX) (CONST_INT)) and that
	 (REGY), i.e. BASE, is not clobbered before the last use we'll
	 create.  */
      if (reg_sum
	  && prev_set
	  && CONST_INT_P (SET_SRC (prev_set))
	  && rtx_equal_p (SET_DEST (prev_set), reg)
	  && (reg_state[REGNO (base)].store_ruid
	      <= reg_state[regno].use_ruid))
	{
	  /* Change destination register and, if necessary, the constant
	     value in PREV, the constant loading instruction.  */
	  validate_change (prev, &SET_DEST (prev_set), index_reg, 1);
	  if (reg_state[regno].offset != const0_rtx)
	    validate_change (prev,
			     &SET_SRC (prev_set),
			     GEN_INT (INTVAL (SET_SRC (prev_set))
				      + INTVAL (reg_state[regno].offset)),
			     1);

	  /* Now for every use of REG that we have recorded, replace REG
	     with REG_SUM.  */
	  for (i = reg_state[regno].use_index;
	       i < RELOAD_COMBINE_MAX_USES; i++)
	    validate_unshare_change (reg_state[regno].reg_use[i].insn,
				     reg_state[regno].reg_use[i].usep,
				     /* Each change must have its own
					replacement.  */
				     reg_sum, 1);

	  if (apply_change_group ())
	    {
	      struct reg_use *lowest_ruid = NULL;

	      /* For every new use of REG_SUM, we have to record the use
		 of BASE therein, i.e. operand 1.  */
	      for (i = reg_state[regno].use_index;
		   i < RELOAD_COMBINE_MAX_USES; i++)
		{
		  struct reg_use *use = reg_state[regno].reg_use + i;
		  reload_combine_note_use (&XEXP (*use->usep, 1), use->insn,
					   use->ruid, use->containing_mem);
		  if (lowest_ruid == NULL || use->ruid < lowest_ruid->ruid)
		    lowest_ruid = use;
		}

	      fixup_debug_insns (reg, reg_sum, insn, lowest_ruid->insn);

	      /* Delete the reg-reg addition.  */
	      delete_insn (insn);

	      if (reg_state[regno].offset != const0_rtx
		  /* Previous REG_EQUIV / REG_EQUAL notes for PREV
		     are now invalid.  */
		  && remove_reg_equal_equiv_notes (prev))
		df_notes_rescan (prev);

	      reg_state[regno].use_index = RELOAD_COMBINE_MAX_USES;
	      return true;
	    }
	}
    }
  return false;
}

static void
reload_combine (void)
{
  rtx_insn *insn, *prev;
  basic_block bb;
  unsigned int r;
  int min_labelno, n_labels;
  HARD_REG_SET ever_live_at_start, *label_live;

  /* To avoid wasting too much time later searching for an index register,
     determine the minimum and maximum index register numbers.  */
  if (INDEX_REG_CLASS == NO_REGS)
    last_index_reg = -1;
  else if (first_index_reg == -1 && last_index_reg == 0)
    {
      for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	if (TEST_HARD_REG_BIT (reg_class_contents[INDEX_REG_CLASS], r))
	  {
	    if (first_index_reg == -1)
	      first_index_reg = r;

	    last_index_reg = r;
	  }

      /* If no index register is available, we can quit now.  Set LAST_INDEX_REG
	 to -1 so we'll know to quit early the next time we get here.  */
      if (first_index_reg == -1)
	{
	  last_index_reg = -1;
	  return;
	}
    }

  /* Set up LABEL_LIVE and EVER_LIVE_AT_START.  The register lifetime
     information is a bit fuzzy immediately after reload, but it's
     still good enough to determine which registers are live at a jump
     destination.  */
  min_labelno = get_first_label_num ();
  n_labels = max_label_num () - min_labelno;
  label_live = XNEWVEC (HARD_REG_SET, n_labels);
  CLEAR_HARD_REG_SET (ever_live_at_start);

  FOR_EACH_BB_REVERSE_FN (bb, cfun)
    {
      insn = BB_HEAD (bb);
      if (LABEL_P (insn))
	{
	  HARD_REG_SET live;
	  bitmap live_in = df_get_live_in (bb);

	  REG_SET_TO_HARD_REG_SET (live, live_in);
	  compute_use_by_pseudos (&live, live_in);
	  COPY_HARD_REG_SET (LABEL_LIVE (insn), live);
	  IOR_HARD_REG_SET (ever_live_at_start, live);
	}
    }

  /* Initialize last_label_ruid, reload_combine_ruid and reg_state.  */
  last_label_ruid = last_jump_ruid = reload_combine_ruid = 0;
  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
    {
      reg_state[r].store_ruid = 0;
      reg_state[r].real_store_ruid = 0;
      if (fixed_regs[r])
	reg_state[r].use_index = -1;
      else
	reg_state[r].use_index = RELOAD_COMBINE_MAX_USES;
    }

  for (insn = get_last_insn (); insn; insn = prev)
    {
      bool control_flow_insn;
      rtx note;

      prev = PREV_INSN (insn);

      /* We cannot do our optimization across labels.  Invalidating all the use
	 information we have would be costly, so we just note where the label
	 is and then later disable any optimization that would cross it.  */
      if (LABEL_P (insn))
	last_label_ruid = reload_combine_ruid;
      else if (BARRIER_P (insn))
	{
	  /* Crossing a barrier resets all the use information.  */
	  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	    if (! fixed_regs[r])
	      reg_state[r].use_index = RELOAD_COMBINE_MAX_USES;
	}
      else if (INSN_P (insn) && volatile_insn_p (PATTERN (insn)))
	/* Optimizations across insns being marked as volatile must be
	   prevented.  All the usage information is invalidated
	   here.  */
	for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	  if (! fixed_regs[r]
	      && reg_state[r].use_index != RELOAD_COMBINE_MAX_USES)
	    reg_state[r].use_index = -1;

      if (! NONDEBUG_INSN_P (insn))
	continue;

      reload_combine_ruid++;

      control_flow_insn = control_flow_insn_p (insn);
      if (control_flow_insn)
	last_jump_ruid = reload_combine_ruid;

      if (reload_combine_recognize_const_pattern (insn)
	  || reload_combine_recognize_pattern (insn))
	continue;

      note_stores (PATTERN (insn), reload_combine_note_store, NULL);

      if (CALL_P (insn))
	{
	  rtx link;
	  HARD_REG_SET used_regs;

	  get_call_reg_set_usage (insn, &used_regs, call_used_reg_set);

	  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	    if (TEST_HARD_REG_BIT (used_regs, r))
	      {
		reg_state[r].use_index = RELOAD_COMBINE_MAX_USES;
		reg_state[r].store_ruid = reload_combine_ruid;
	      }

	  for (link = CALL_INSN_FUNCTION_USAGE (insn); link;
	       link = XEXP (link, 1))
	    {
	      rtx setuse = XEXP (link, 0);
	      rtx usage_rtx = XEXP (setuse, 0);
	      if ((GET_CODE (setuse) == USE || GET_CODE (setuse) == CLOBBER)
		  && REG_P (usage_rtx))
	        {
		  unsigned int end_regno = END_REGNO (usage_rtx);
		  for (unsigned int i = REGNO (usage_rtx); i < end_regno; ++i)
		    if (GET_CODE (XEXP (link, 0)) == CLOBBER)
		      {
		        reg_state[i].use_index = RELOAD_COMBINE_MAX_USES;
		        reg_state[i].store_ruid = reload_combine_ruid;
		      }
		    else
		      reg_state[i].use_index = -1;
	         }
	     }
	}

      if (control_flow_insn && !ANY_RETURN_P (PATTERN (insn)))
	{
	  /* Non-spill registers might be used at the call destination in
	     some unknown fashion, so we have to mark the unknown use.  */
	  HARD_REG_SET *live;

	  if ((condjump_p (insn) || condjump_in_parallel_p (insn))
	      && JUMP_LABEL (insn))
	    {
	      if (ANY_RETURN_P (JUMP_LABEL (insn)))
		live = NULL;
	      else
		live = &LABEL_LIVE (JUMP_LABEL (insn));
	    }
	  else
	    live = &ever_live_at_start;

	  if (live)
	    for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	      if (TEST_HARD_REG_BIT (*live, r))
		reg_state[r].use_index = -1;
	}

      reload_combine_note_use (&PATTERN (insn), insn, reload_combine_ruid,
			       NULL_RTX);

      for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	{
	  if (REG_NOTE_KIND (note) == REG_INC && REG_P (XEXP (note, 0)))
	    {
	      int regno = REGNO (XEXP (note, 0));
	      reg_state[regno].store_ruid = reload_combine_ruid;
	      reg_state[regno].real_store_ruid = reload_combine_ruid;
	      reg_state[regno].use_index = -1;
	    }
	}
    }

  free (label_live);
}

/* Check if DST is a register or a subreg of a register; if it is,
   update store_ruid, real_store_ruid and use_index in the reg_state
   structure accordingly.  Called via note_stores from reload_combine.  */

static void
reload_combine_note_store (rtx dst, const_rtx set, void *data ATTRIBUTE_UNUSED)
{
  int regno = 0;
  int i;
  machine_mode mode = GET_MODE (dst);

  if (GET_CODE (dst) == SUBREG)
    {
      regno = subreg_regno_offset (REGNO (SUBREG_REG (dst)),
				   GET_MODE (SUBREG_REG (dst)),
				   SUBREG_BYTE (dst),
				   GET_MODE (dst));
      dst = SUBREG_REG (dst);
    }

  /* Some targets do argument pushes without adding REG_INC notes.  */

  if (MEM_P (dst))
    {
      dst = XEXP (dst, 0);
      if (GET_CODE (dst) == PRE_INC || GET_CODE (dst) == POST_INC
	  || GET_CODE (dst) == PRE_DEC || GET_CODE (dst) == POST_DEC
	  || GET_CODE (dst) == PRE_MODIFY || GET_CODE (dst) == POST_MODIFY)
	{
	  unsigned int end_regno = END_REGNO (XEXP (dst, 0));
	  for (unsigned int i = REGNO (XEXP (dst, 0)); i < end_regno; ++i)
	    {
	      /* We could probably do better, but for now mark the register
		 as used in an unknown fashion and set/clobbered at this
		 insn.  */
	      reg_state[i].use_index = -1;
	      reg_state[i].store_ruid = reload_combine_ruid;
	      reg_state[i].real_store_ruid = reload_combine_ruid;
	    }
	}
      else
        return;
    }

  if (!REG_P (dst))
    return;
  regno += REGNO (dst);

  /* note_stores might have stripped a STRICT_LOW_PART, so we have to be
     careful with registers / register parts that are not full words.
     Similarly for ZERO_EXTRACT.  */
  if (GET_CODE (SET_DEST (set)) == ZERO_EXTRACT
      || GET_CODE (SET_DEST (set)) == STRICT_LOW_PART)
    {
      for (i = hard_regno_nregs[regno][mode] - 1 + regno; i >= regno; i--)
	{
	  reg_state[i].use_index = -1;
	  reg_state[i].store_ruid = reload_combine_ruid;
	  reg_state[i].real_store_ruid = reload_combine_ruid;
	}
    }
  else
    {
      for (i = hard_regno_nregs[regno][mode] - 1 + regno; i >= regno; i--)
	{
	  reg_state[i].store_ruid = reload_combine_ruid;
	  if (GET_CODE (set) == SET)
	    reg_state[i].real_store_ruid = reload_combine_ruid;
	  reg_state[i].use_index = RELOAD_COMBINE_MAX_USES;
	}
    }
}

/* XP points to a piece of rtl that has to be checked for any uses of
   registers.
   *XP is the pattern of INSN, or a part of it.
   Called from reload_combine, and recursively by itself.  */
static void
reload_combine_note_use (rtx *xp, rtx_insn *insn, int ruid, rtx containing_mem)
{
  rtx x = *xp;
  enum rtx_code code = x->code;
  const char *fmt;
  int i, j;
  rtx offset = const0_rtx; /* For the REG case below.  */

  switch (code)
    {
    case SET:
      if (REG_P (SET_DEST (x)))
	{
	  reload_combine_note_use (&SET_SRC (x), insn, ruid, NULL_RTX);
	  return;
	}
      break;

    case USE:
      /* If this is the USE of a return value, we can't change it.  */
      if (REG_P (XEXP (x, 0)) && REG_FUNCTION_VALUE_P (XEXP (x, 0)))
	{
	  /* Mark the return register as used in an unknown fashion.  */
	  rtx reg = XEXP (x, 0);
	  unsigned int end_regno = END_REGNO (reg);
	  for (unsigned int regno = REGNO (reg); regno < end_regno; ++regno)
	    reg_state[regno].use_index = -1;
	  return;
	}
      break;

    case CLOBBER:
      if (REG_P (SET_DEST (x)))
	{
	  /* No spurious CLOBBERs of pseudo registers may remain.  */
	  gcc_assert (REGNO (SET_DEST (x)) < FIRST_PSEUDO_REGISTER);
	  return;
	}
      break;

    case PLUS:
      /* We are interested in (plus (reg) (const_int)) .  */
      if (!REG_P (XEXP (x, 0))
	  || !CONST_INT_P (XEXP (x, 1)))
	break;
      offset = XEXP (x, 1);
      x = XEXP (x, 0);
      /* Fall through.  */
    case REG:
      {
	int regno = REGNO (x);
	int use_index;
	int nregs;

	/* No spurious USEs of pseudo registers may remain.  */
	gcc_assert (regno < FIRST_PSEUDO_REGISTER);

	nregs = REG_NREGS (x);

	/* We can't substitute into multi-hard-reg uses.  */
	if (nregs > 1)
	  {
	    while (--nregs >= 0)
	      reg_state[regno + nregs].use_index = -1;
	    return;
	  }

	/* We may be called to update uses in previously seen insns.
	   Don't add uses beyond the last store we saw.  */
	if (ruid < reg_state[regno].store_ruid)
	  return;

	/* If this register is already used in some unknown fashion, we
	   can't do anything.
	   If we decrement the index from zero to -1, we can't store more
	   uses, so this register becomes used in an unknown fashion.  */
	use_index = --reg_state[regno].use_index;
	if (use_index < 0)
	  return;

	if (use_index == RELOAD_COMBINE_MAX_USES - 1)
	  {
	    /* This is the first use of this register we have seen since we
	       marked it as dead.  */
	    reg_state[regno].offset = offset;
	    reg_state[regno].all_offsets_match = true;
	    reg_state[regno].use_ruid = ruid;
	  }
	else
	  {
	    if (reg_state[regno].use_ruid > ruid)
	      reg_state[regno].use_ruid = ruid;

	    if (! rtx_equal_p (offset, reg_state[regno].offset))
	      reg_state[regno].all_offsets_match = false;
	  }

	reg_state[regno].reg_use[use_index].insn = insn;
	reg_state[regno].reg_use[use_index].ruid = ruid;
	reg_state[regno].reg_use[use_index].containing_mem = containing_mem;
	reg_state[regno].reg_use[use_index].usep = xp;
	return;
      }

    case MEM:
      containing_mem = x;
      break;

    default:
      break;
    }

  /* Recursively process the components of X.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	reload_combine_note_use (&XEXP (x, i), insn, ruid, containing_mem);
      else if (fmt[i] == 'E')
	{
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    reload_combine_note_use (&XVECEXP (x, i, j), insn, ruid,
				     containing_mem);
	}
    }
}

/* See if we can reduce the cost of a constant by replacing a move
   with an add.  We track situations in which a register is set to a
   constant or to a register plus a constant.  */
/* We cannot do our optimization across labels.  Invalidating all the
   information about register contents we have would be costly, so we
   use move2add_last_label_luid to note where the label is and then
   later disable any optimization that would cross it.
   reg_offset[n] / reg_base_reg[n] / reg_symbol_ref[n] / reg_mode[n]
   are only valid if reg_set_luid[n] is greater than
   move2add_last_label_luid.
   For a set that established a new (potential) base register with
   non-constant value, we use move2add_luid from the place where the
   setting insn is encountered; registers based off that base then
   get the same reg_set_luid.  Constants all get
   move2add_last_label_luid + 1 as their reg_set_luid.  */
static int reg_set_luid[FIRST_PSEUDO_REGISTER];

/* If reg_base_reg[n] is negative, register n has been set to
   reg_offset[n] or reg_symbol_ref[n] + reg_offset[n] in mode reg_mode[n].
   If reg_base_reg[n] is non-negative, register n has been set to the
   sum of reg_offset[n] and the value of register reg_base_reg[n]
   before reg_set_luid[n], calculated in mode reg_mode[n] .
   For multi-hard-register registers, all but the first one are
   recorded as BLKmode in reg_mode.  Setting reg_mode to VOIDmode
   marks it as invalid.  */
static HOST_WIDE_INT reg_offset[FIRST_PSEUDO_REGISTER];
static int reg_base_reg[FIRST_PSEUDO_REGISTER];
static rtx reg_symbol_ref[FIRST_PSEUDO_REGISTER];
static machine_mode reg_mode[FIRST_PSEUDO_REGISTER];

/* move2add_luid is linearly increased while scanning the instructions
   from first to last.  It is used to set reg_set_luid in
   reload_cse_move2add and move2add_note_store.  */
static int move2add_luid;

/* move2add_last_label_luid is set whenever a label is found.  Labels
   invalidate all previously collected reg_offset data.  */
static int move2add_last_label_luid;

/* ??? We don't know how zero / sign extension is handled, hence we
   can't go from a narrower to a wider mode.  */
#define MODES_OK_FOR_MOVE2ADD(OUTMODE, INMODE) \
  (GET_MODE_SIZE (OUTMODE) == GET_MODE_SIZE (INMODE) \
   || (GET_MODE_SIZE (OUTMODE) <= GET_MODE_SIZE (INMODE) \
       && TRULY_NOOP_TRUNCATION_MODES_P (OUTMODE, INMODE)))

/* Record that REG is being set to a value with the mode of REG.  */

static void
move2add_record_mode (rtx reg)
{
  int regno, nregs;
  machine_mode mode = GET_MODE (reg);

  if (GET_CODE (reg) == SUBREG)
    {
      regno = subreg_regno (reg);
      nregs = subreg_nregs (reg);
    }
  else if (REG_P (reg))
    {
      regno = REGNO (reg);
      nregs = REG_NREGS (reg);
    }
  else
    gcc_unreachable ();
  for (int i = nregs - 1; i > 0; i--)
    reg_mode[regno + i] = BLKmode;
  reg_mode[regno] = mode;
}

/* Record that REG is being set to the sum of SYM and OFF.  */

static void
move2add_record_sym_value (rtx reg, rtx sym, rtx off)
{
  int regno = REGNO (reg);

  move2add_record_mode (reg);
  reg_set_luid[regno] = move2add_luid;
  reg_base_reg[regno] = -1;
  reg_symbol_ref[regno] = sym;
  reg_offset[regno] = INTVAL (off);
}

/* Check if REGNO contains a valid value in MODE.  */

static bool
move2add_valid_value_p (int regno, machine_mode mode)
{
  if (reg_set_luid[regno] <= move2add_last_label_luid)
    return false;

  if (mode != reg_mode[regno])
    {
      if (!MODES_OK_FOR_MOVE2ADD (mode, reg_mode[regno]))
	return false;
      /* The value loaded into regno in reg_mode[regno] is also valid in
	 mode after truncation only if (REG:mode regno) is the lowpart of
	 (REG:reg_mode[regno] regno).  Now, for big endian, the starting
	 regno of the lowpart might be different.  */
      int s_off = subreg_lowpart_offset (mode, reg_mode[regno]);
      s_off = subreg_regno_offset (regno, reg_mode[regno], s_off, mode);
      if (s_off != 0)
	/* We could in principle adjust regno, check reg_mode[regno] to be
	   BLKmode, and return s_off to the caller (vs. -1 for failure),
	   but we currently have no callers that could make use of this
	   information.  */
	return false;
    }

  for (int i = hard_regno_nregs[regno][mode] - 1; i > 0; i--)
    if (reg_mode[regno + i] != BLKmode)
      return false;
  return true;
}

/* This function is called with INSN that sets REG to (SYM + OFF),
   while REG is known to already have value (SYM + offset).
   This function tries to change INSN into an add instruction
   (set (REG) (plus (REG) (OFF - offset))) using the known value.
   It also updates the information about REG's known value.
   Return true if we made a change.  */

static bool
move2add_use_add2_insn (rtx reg, rtx sym, rtx off, rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  rtx src = SET_SRC (pat);
  int regno = REGNO (reg);
  rtx new_src = gen_int_mode (UINTVAL (off) - reg_offset[regno],
			      GET_MODE (reg));
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));
  bool changed = false;

  /* (set (reg) (plus (reg) (const_int 0))) is not canonical;
     use (set (reg) (reg)) instead.
     We don't delete this insn, nor do we convert it into a
     note, to avoid losing register notes or the return
     value flag.  jump2 already knows how to get rid of
     no-op moves.  */
  if (new_src == const0_rtx)
    {
      /* If the constants are different, this is a
	 truncation, that, if turned into (set (reg)
	 (reg)), would be discarded.  Maybe we should
	 try a truncMN pattern?  */
      if (INTVAL (off) == reg_offset [regno])
	changed = validate_change (insn, &SET_SRC (pat), reg, 0);
    }
  else
    {
      struct full_rtx_costs oldcst, newcst;
      rtx tem = gen_rtx_PLUS (GET_MODE (reg), reg, new_src);

      get_full_set_rtx_cost (pat, &oldcst);
      SET_SRC (pat) = tem;
      get_full_set_rtx_cost (pat, &newcst);
      SET_SRC (pat) = src;

      if (costs_lt_p (&newcst, &oldcst, speed)
	  && have_add2_insn (reg, new_src))
	changed = validate_change (insn, &SET_SRC (pat), tem, 0);	
      else if (sym == NULL_RTX && GET_MODE (reg) != BImode)
	{
	  machine_mode narrow_mode;
	  for (narrow_mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	       narrow_mode != VOIDmode
		 && narrow_mode != GET_MODE (reg);
	       narrow_mode = GET_MODE_WIDER_MODE (narrow_mode))
	    {
	      if (have_insn_for (STRICT_LOW_PART, narrow_mode)
		  && ((reg_offset[regno] & ~GET_MODE_MASK (narrow_mode))
		      == (INTVAL (off) & ~GET_MODE_MASK (narrow_mode))))
		{
		  rtx narrow_reg = gen_lowpart_common (narrow_mode, reg);
		  rtx narrow_src = gen_int_mode (INTVAL (off),
						 narrow_mode);
		  rtx new_set
		    = gen_rtx_SET (gen_rtx_STRICT_LOW_PART (VOIDmode,
							    narrow_reg),
				   narrow_src);
		  get_full_set_rtx_cost (new_set, &newcst);
		  if (costs_lt_p (&newcst, &oldcst, speed))
		    {
		      changed = validate_change (insn, &PATTERN (insn),
						 new_set, 0);
		      if (changed)
			break;
		    }
		}
	    }
	}
    }
  move2add_record_sym_value (reg, sym, off);
  return changed;
}


/* This function is called with INSN that sets REG to (SYM + OFF),
   but REG doesn't have known value (SYM + offset).  This function
   tries to find another register which is known to already have
   value (SYM + offset) and change INSN into an add instruction
   (set (REG) (plus (the found register) (OFF - offset))) if such
   a register is found.  It also updates the information about
   REG's known value.
   Return true iff we made a change.  */

static bool
move2add_use_add3_insn (rtx reg, rtx sym, rtx off, rtx_insn *insn)
{
  rtx pat = PATTERN (insn);
  rtx src = SET_SRC (pat);
  int regno = REGNO (reg);
  int min_regno = 0;
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));
  int i;
  bool changed = false;
  struct full_rtx_costs oldcst, newcst, mincst;
  rtx plus_expr;

  init_costs_to_max (&mincst);
  get_full_set_rtx_cost (pat, &oldcst);

  plus_expr = gen_rtx_PLUS (GET_MODE (reg), reg, const0_rtx);
  SET_SRC (pat) = plus_expr;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (move2add_valid_value_p (i, GET_MODE (reg))
	&& reg_base_reg[i] < 0
	&& reg_symbol_ref[i] != NULL_RTX
	&& rtx_equal_p (sym, reg_symbol_ref[i]))
      {
	rtx new_src = gen_int_mode (UINTVAL (off) - reg_offset[i],
				    GET_MODE (reg));
	/* (set (reg) (plus (reg) (const_int 0))) is not canonical;
	   use (set (reg) (reg)) instead.
	   We don't delete this insn, nor do we convert it into a
	   note, to avoid losing register notes or the return
	   value flag.  jump2 already knows how to get rid of
	   no-op moves.  */
	if (new_src == const0_rtx)
	  {
	    init_costs_to_zero (&mincst);
	    min_regno = i;
	    break;
	  }
	else
	  {
	    XEXP (plus_expr, 1) = new_src;
	    get_full_set_rtx_cost (pat, &newcst);

	    if (costs_lt_p (&newcst, &mincst, speed))
	      {
		mincst = newcst;
		min_regno = i;
	      }
	  }
      }
  SET_SRC (pat) = src;

  if (costs_lt_p (&mincst, &oldcst, speed))
    {
      rtx tem;

      tem = gen_rtx_REG (GET_MODE (reg), min_regno);
      if (i != min_regno)
	{
	  rtx new_src = gen_int_mode (UINTVAL (off) - reg_offset[min_regno],
				      GET_MODE (reg));
	  tem = gen_rtx_PLUS (GET_MODE (reg), tem, new_src);
	}
      if (validate_change (insn, &SET_SRC (pat), tem, 0))
	changed = true;
    }
  reg_set_luid[regno] = move2add_luid;
  move2add_record_sym_value (reg, sym, off);
  return changed;
}

/* Convert move insns with constant inputs to additions if they are cheaper.
   Return true if any changes were made.  */
static bool
reload_cse_move2add (rtx_insn *first)
{
  int i;
  rtx_insn *insn;
  bool changed = false;

  for (i = FIRST_PSEUDO_REGISTER - 1; i >= 0; i--)
    {
      reg_set_luid[i] = 0;
      reg_offset[i] = 0;
      reg_base_reg[i] = 0;
      reg_symbol_ref[i] = NULL_RTX;
      reg_mode[i] = VOIDmode;
    }

  move2add_last_label_luid = 0;
  move2add_luid = 2;
  for (insn = first; insn; insn = NEXT_INSN (insn), move2add_luid++)
    {
      rtx pat, note;

      if (LABEL_P (insn))
	{
	  move2add_last_label_luid = move2add_luid;
	  /* We're going to increment move2add_luid twice after a
	     label, so that we can use move2add_last_label_luid + 1 as
	     the luid for constants.  */
	  move2add_luid++;
	  continue;
	}
      if (! INSN_P (insn))
	continue;
      pat = PATTERN (insn);
      /* For simplicity, we only perform this optimization on
	 straightforward SETs.  */
      if (GET_CODE (pat) == SET
	  && REG_P (SET_DEST (pat)))
	{
	  rtx reg = SET_DEST (pat);
	  int regno = REGNO (reg);
	  rtx src = SET_SRC (pat);

	  /* Check if we have valid information on the contents of this
	     register in the mode of REG.  */
	  if (move2add_valid_value_p (regno, GET_MODE (reg))
              && dbg_cnt (cse2_move2add))
	    {
	      /* Try to transform (set (REGX) (CONST_INT A))
				  ...
				  (set (REGX) (CONST_INT B))
		 to
				  (set (REGX) (CONST_INT A))
				  ...
				  (set (REGX) (plus (REGX) (CONST_INT B-A)))
		 or
				  (set (REGX) (CONST_INT A))
				  ...
				  (set (STRICT_LOW_PART (REGX)) (CONST_INT B))
	      */

	      if (CONST_INT_P (src)
		  && reg_base_reg[regno] < 0
		  && reg_symbol_ref[regno] == NULL_RTX)
		{
		  changed |= move2add_use_add2_insn (reg, NULL_RTX, src, insn);
		  continue;
		}

	      /* Try to transform (set (REGX) (REGY))
				  (set (REGX) (PLUS (REGX) (CONST_INT A)))
				  ...
				  (set (REGX) (REGY))
				  (set (REGX) (PLUS (REGX) (CONST_INT B)))
		 to
				  (set (REGX) (REGY))
				  (set (REGX) (PLUS (REGX) (CONST_INT A)))
				  ...
				  (set (REGX) (plus (REGX) (CONST_INT B-A)))  */
	      else if (REG_P (src)
		       && reg_set_luid[regno] == reg_set_luid[REGNO (src)]
		       && reg_base_reg[regno] == reg_base_reg[REGNO (src)]
		       && move2add_valid_value_p (REGNO (src), GET_MODE (reg)))
		{
		  rtx_insn *next = next_nonnote_nondebug_insn (insn);
		  rtx set = NULL_RTX;
		  if (next)
		    set = single_set (next);
		  if (set
		      && SET_DEST (set) == reg
		      && GET_CODE (SET_SRC (set)) == PLUS
		      && XEXP (SET_SRC (set), 0) == reg
		      && CONST_INT_P (XEXP (SET_SRC (set), 1)))
		    {
		      rtx src3 = XEXP (SET_SRC (set), 1);
		      unsigned HOST_WIDE_INT added_offset = UINTVAL (src3);
		      HOST_WIDE_INT base_offset = reg_offset[REGNO (src)];
		      HOST_WIDE_INT regno_offset = reg_offset[regno];
		      rtx new_src =
			gen_int_mode (added_offset
				      + base_offset
				      - regno_offset,
				      GET_MODE (reg));
		      bool success = false;
		      bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));

		      if (new_src == const0_rtx)
			/* See above why we create (set (reg) (reg)) here.  */
			success
			  = validate_change (next, &SET_SRC (set), reg, 0);
		      else
			{
			  rtx old_src = SET_SRC (set);
			  struct full_rtx_costs oldcst, newcst;
			  rtx tem = gen_rtx_PLUS (GET_MODE (reg), reg, new_src);

			  get_full_set_rtx_cost (set, &oldcst);
			  SET_SRC (set) = tem;
			  get_full_set_src_cost (tem, GET_MODE (reg), &newcst);
			  SET_SRC (set) = old_src;
			  costs_add_n_insns (&oldcst, 1);

			  if (costs_lt_p (&newcst, &oldcst, speed)
			      && have_add2_insn (reg, new_src))
			    {
			      rtx newpat = gen_rtx_SET (reg, tem);
			      success
				= validate_change (next, &PATTERN (next),
						   newpat, 0);
			    }
			}
		      if (success)
			delete_insn (insn);
		      changed |= success;
		      insn = next;
		      move2add_record_mode (reg);
		      reg_offset[regno]
			= trunc_int_for_mode (added_offset + base_offset,
					      GET_MODE (reg));
		      continue;
		    }
		}
	    }

	  /* Try to transform
	     (set (REGX) (CONST (PLUS (SYMBOL_REF) (CONST_INT A))))
	     ...
	     (set (REGY) (CONST (PLUS (SYMBOL_REF) (CONST_INT B))))
	     to
	     (set (REGX) (CONST (PLUS (SYMBOL_REF) (CONST_INT A))))
	     ...
	     (set (REGY) (CONST (PLUS (REGX) (CONST_INT B-A))))  */
	  if ((GET_CODE (src) == SYMBOL_REF
	       || (GET_CODE (src) == CONST
		   && GET_CODE (XEXP (src, 0)) == PLUS
		   && GET_CODE (XEXP (XEXP (src, 0), 0)) == SYMBOL_REF
		   && CONST_INT_P (XEXP (XEXP (src, 0), 1))))
	      && dbg_cnt (cse2_move2add))
	    {
	      rtx sym, off;

	      if (GET_CODE (src) == SYMBOL_REF)
		{
		  sym = src;
		  off = const0_rtx;
		}
	      else
		{
		  sym = XEXP (XEXP (src, 0), 0);
		  off = XEXP (XEXP (src, 0), 1);
		}

	      /* If the reg already contains the value which is sum of
		 sym and some constant value, we can use an add2 insn.  */
	      if (move2add_valid_value_p (regno, GET_MODE (reg))
		  && reg_base_reg[regno] < 0
		  && reg_symbol_ref[regno] != NULL_RTX
		  && rtx_equal_p (sym, reg_symbol_ref[regno]))
		changed |= move2add_use_add2_insn (reg, sym, off, insn);

	      /* Otherwise, we have to find a register whose value is sum
		 of sym and some constant value.  */
	      else
		changed |= move2add_use_add3_insn (reg, sym, off, insn);

	      continue;
	    }
	}

      for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	{
	  if (REG_NOTE_KIND (note) == REG_INC
	      && REG_P (XEXP (note, 0)))
	    {
	      /* Reset the information about this register.  */
	      int regno = REGNO (XEXP (note, 0));
	      if (regno < FIRST_PSEUDO_REGISTER)
		{
		  move2add_record_mode (XEXP (note, 0));
		  reg_mode[regno] = VOIDmode;
		}
	    }
	}
      note_stores (PATTERN (insn), move2add_note_store, insn);

      /* If INSN is a conditional branch, we try to extract an
	 implicit set out of it.  */
      if (any_condjump_p (insn))
	{
	  rtx cnd = fis_get_condition (insn);

	  if (cnd != NULL_RTX
	      && GET_CODE (cnd) == NE
	      && REG_P (XEXP (cnd, 0))
	      && !reg_set_p (XEXP (cnd, 0), insn)
	      /* The following two checks, which are also in
		 move2add_note_store, are intended to reduce the
		 number of calls to gen_rtx_SET to avoid memory
		 allocation if possible.  */
	      && SCALAR_INT_MODE_P (GET_MODE (XEXP (cnd, 0)))
	      && REG_NREGS (XEXP (cnd, 0)) == 1
	      && CONST_INT_P (XEXP (cnd, 1)))
	    {
	      rtx implicit_set =
		gen_rtx_SET (XEXP (cnd, 0), XEXP (cnd, 1));
	      move2add_note_store (SET_DEST (implicit_set), implicit_set, insn);
	    }
	}

      /* If this is a CALL_INSN, all call used registers are stored with
	 unknown values.  */
      if (CALL_P (insn))
	{
	  rtx link;

	  for (i = FIRST_PSEUDO_REGISTER - 1; i >= 0; i--)
	    {
	      if (call_used_regs[i])
		/* Reset the information about this register.  */
		reg_mode[i] = VOIDmode;
	    }

	  for (link = CALL_INSN_FUNCTION_USAGE (insn); link;
	       link = XEXP (link, 1))
	    {
	      rtx setuse = XEXP (link, 0);
	      rtx usage_rtx = XEXP (setuse, 0);
	      if (GET_CODE (setuse) == CLOBBER
		  && REG_P (usage_rtx))
	        {
		  unsigned int end_regno = END_REGNO (usage_rtx);
		  for (unsigned int r = REGNO (usage_rtx); r < end_regno; ++r)
		    /* Reset the information about this register.  */
		    reg_mode[r] = VOIDmode;
		}
	    }
	}
    }
  return changed;
}

/* SET is a SET or CLOBBER that sets DST.  DATA is the insn which
   contains SET.
   Update reg_set_luid, reg_offset and reg_base_reg accordingly.
   Called from reload_cse_move2add via note_stores.  */

static void
move2add_note_store (rtx dst, const_rtx set, void *data)
{
  rtx_insn *insn = (rtx_insn *) data;
  unsigned int regno = 0;
  machine_mode mode = GET_MODE (dst);

  /* Some targets do argument pushes without adding REG_INC notes.  */

  if (MEM_P (dst))
    {
      dst = XEXP (dst, 0);
      if (GET_CODE (dst) == PRE_INC || GET_CODE (dst) == POST_INC
	  || GET_CODE (dst) == PRE_DEC || GET_CODE (dst) == POST_DEC)
	reg_mode[REGNO (XEXP (dst, 0))] = VOIDmode;
      return;
    }

  if (GET_CODE (dst) == SUBREG)
    regno = subreg_regno (dst);
  else if (REG_P (dst))
    regno = REGNO (dst);
  else
    return;

  if (SCALAR_INT_MODE_P (mode)
      && GET_CODE (set) == SET)
    {
      rtx note, sym = NULL_RTX;
      rtx off;

      note = find_reg_equal_equiv_note (insn);
      if (note && GET_CODE (XEXP (note, 0)) == SYMBOL_REF)
	{
	  sym = XEXP (note, 0);
	  off = const0_rtx;
	}
      else if (note && GET_CODE (XEXP (note, 0)) == CONST
	       && GET_CODE (XEXP (XEXP (note, 0), 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (XEXP (note, 0), 0), 0)) == SYMBOL_REF
	       && CONST_INT_P (XEXP (XEXP (XEXP (note, 0), 0), 1)))
	{
	  sym = XEXP (XEXP (XEXP (note, 0), 0), 0);
	  off = XEXP (XEXP (XEXP (note, 0), 0), 1);
	}

      if (sym != NULL_RTX)
	{
	  move2add_record_sym_value (dst, sym, off);
	  return;
	}
    }

  if (SCALAR_INT_MODE_P (mode)
      && GET_CODE (set) == SET
      && GET_CODE (SET_DEST (set)) != ZERO_EXTRACT
      && GET_CODE (SET_DEST (set)) != STRICT_LOW_PART)
    {
      rtx src = SET_SRC (set);
      rtx base_reg;
      unsigned HOST_WIDE_INT offset;
      int base_regno;

      switch (GET_CODE (src))
	{
	case PLUS:
	  if (REG_P (XEXP (src, 0)))
	    {
	      base_reg = XEXP (src, 0);

	      if (CONST_INT_P (XEXP (src, 1)))
		offset = UINTVAL (XEXP (src, 1));
	      else if (REG_P (XEXP (src, 1))
		       && move2add_valid_value_p (REGNO (XEXP (src, 1)), mode))
		{
		  if (reg_base_reg[REGNO (XEXP (src, 1))] < 0
		      && reg_symbol_ref[REGNO (XEXP (src, 1))] == NULL_RTX)
		    offset = reg_offset[REGNO (XEXP (src, 1))];
		  /* Maybe the first register is known to be a
		     constant.  */
		  else if (move2add_valid_value_p (REGNO (base_reg), mode)
			   && reg_base_reg[REGNO (base_reg)] < 0
			   && reg_symbol_ref[REGNO (base_reg)] == NULL_RTX)
		    {
		      offset = reg_offset[REGNO (base_reg)];
		      base_reg = XEXP (src, 1);
		    }
		  else
		    goto invalidate;
		}
	      else
		goto invalidate;

	      break;
	    }

	  goto invalidate;

	case REG:
	  base_reg = src;
	  offset = 0;
	  break;

	case CONST_INT:
	  /* Start tracking the register as a constant.  */
	  reg_base_reg[regno] = -1;
	  reg_symbol_ref[regno] = NULL_RTX;
	  reg_offset[regno] = INTVAL (SET_SRC (set));
	  /* We assign the same luid to all registers set to constants.  */
	  reg_set_luid[regno] = move2add_last_label_luid + 1;
	  move2add_record_mode (dst);
	  return;

	default:
	  goto invalidate;
	}

      base_regno = REGNO (base_reg);
      /* If information about the base register is not valid, set it
	 up as a new base register, pretending its value is known
	 starting from the current insn.  */
      if (!move2add_valid_value_p (base_regno, mode))
	{
	  reg_base_reg[base_regno] = base_regno;
	  reg_symbol_ref[base_regno] = NULL_RTX;
	  reg_offset[base_regno] = 0;
	  reg_set_luid[base_regno] = move2add_luid;
	  gcc_assert (GET_MODE (base_reg) == mode);
	  move2add_record_mode (base_reg);
	}

      /* Copy base information from our base register.  */
      reg_set_luid[regno] = reg_set_luid[base_regno];
      reg_base_reg[regno] = reg_base_reg[base_regno];
      reg_symbol_ref[regno] = reg_symbol_ref[base_regno];

      /* Compute the sum of the offsets or constants.  */
      reg_offset[regno]
	= trunc_int_for_mode (offset + reg_offset[base_regno], mode);

      move2add_record_mode (dst);
    }
  else
    {
    invalidate:
      /* Invalidate the contents of the register.  */
      move2add_record_mode (dst);
      reg_mode[regno] = VOIDmode;
    }
}

namespace {

const pass_data pass_data_postreload_cse =
{
  RTL_PASS, /* type */
  "postreload", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_RELOAD_CSE_REGS, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_postreload_cse : public rtl_opt_pass
{
public:
  pass_postreload_cse (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_postreload_cse, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return (optimize > 0 && reload_completed); }

  virtual unsigned int execute (function *);

}; // class pass_postreload_cse

unsigned int
pass_postreload_cse::execute (function *fun)
{
  if (!dbg_cnt (postreload_cse))
    return 0;

  /* Do a very simple CSE pass over just the hard registers.  */
  reload_cse_regs (get_insns ());
  /* Reload_cse_regs can eliminate potentially-trapping MEMs.
     Remove any EH edges associated with them.  */
  if (fun->can_throw_non_call_exceptions
      && purge_all_dead_edges ())
    cleanup_cfg (0);

  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_postreload_cse (gcc::context *ctxt)
{
  return new pass_postreload_cse (ctxt);
}
