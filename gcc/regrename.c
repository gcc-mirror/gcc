/* Register renaming for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "regs.h"
#include "addresses.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "reload.h"
#include "output.h"
#include "function.h"
#include "recog.h"
#include "flags.h"
#include "toplev.h"
#include "obstack.h"
#include "timevar.h"
#include "tree-pass.h"
#include "df.h"

/* We keep linked lists of DU_HEAD structures, each of which describes
   a chain of occurrences of a reg.  */
struct du_head
{
  /* The next chain.  */
  struct du_head *next_chain;
  /* The first and last elements of this chain.  */
  struct du_chain *first, *last;
  /* Describes the register being tracked.  */
  unsigned regno, nregs;
  /* Nonzero if the chain crosses a call.  */
  unsigned int need_caller_save_reg:1;
  /* Nonzero if the chain is finished.  */
  unsigned int terminated:1;
};

/* This struct describes a single occurrence of a register.  */
struct du_chain
{
  /* Links to the next occurrence of the register.  */
  struct du_chain *next_use;

  /* The insn where the register appears.  */
  rtx insn;
  /* The location inside the insn.  */
  rtx *loc;
  /* The register class required by the insn at this location.  */
  ENUM_BITFIELD(reg_class) cl : 16;
};

enum scan_actions
{
  terminate_all_read,
  terminate_overlapping_read,
  terminate_write,
  terminate_dead,
  mark_read,
  mark_write,
  /* mark_access is for marking the destination regs in
     REG_FRAME_RELATED_EXPR notes (as if they were read) so that the
     note is updated properly.  */
  mark_access
};

static const char * const scan_actions_name[] =
{
  "terminate_all_read",
  "terminate_overlapping_read",
  "terminate_write",
  "terminate_dead",
  "mark_read",
  "mark_write",
  "mark_access"
};

static struct obstack rename_obstack;

static void do_replace (struct du_head *, int);
static void scan_rtx_reg (rtx, rtx *, enum reg_class,
			  enum scan_actions, enum op_type);
static void scan_rtx_address (rtx, rtx *, enum reg_class,
			      enum scan_actions, enum machine_mode);
static void scan_rtx (rtx, rtx *, enum reg_class, enum scan_actions,
		      enum op_type);
static struct du_head *build_def_use (basic_block);
static void dump_def_use_chain (struct du_head *);
static void note_sets (rtx, const_rtx, void *);
static void clear_dead_regs (HARD_REG_SET *, enum reg_note, rtx);
static void merge_overlapping_regs (basic_block, HARD_REG_SET *,
				    struct du_head *);

/* Called through note_stores.  Find sets of registers, and
   record them in *DATA (which is actually a HARD_REG_SET *).  */

static void
note_sets (rtx x, const_rtx set ATTRIBUTE_UNUSED, void *data)
{
  HARD_REG_SET *pset = (HARD_REG_SET *) data;

  if (GET_CODE (x) == SUBREG)
    x = SUBREG_REG (x);
  if (!REG_P (x))
    return;
  /* There must not be pseudos at this point.  */
  gcc_assert (HARD_REGISTER_P (x));
  add_to_hard_reg_set (pset, GET_MODE (x), REGNO (x));
}

/* Clear all registers from *PSET for which a note of kind KIND can be found
   in the list NOTES.  */

static void
clear_dead_regs (HARD_REG_SET *pset, enum reg_note kind, rtx notes)
{
  rtx note;
  for (note = notes; note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == kind && REG_P (XEXP (note, 0)))
      {
	rtx reg = XEXP (note, 0);
	/* There must not be pseudos at this point.  */
	gcc_assert (HARD_REGISTER_P (reg));
	remove_from_hard_reg_set (pset, GET_MODE (reg), REGNO (reg));
      }
}

/* For a def-use chain HEAD in basic block B, find which registers overlap
   its lifetime and set the corresponding bits in *PSET.  */

static void
merge_overlapping_regs (basic_block b, HARD_REG_SET *pset,
			struct du_head *head)
{
  struct du_chain *t;
  rtx insn;
  HARD_REG_SET live;
  df_ref *def_rec;

  REG_SET_TO_HARD_REG_SET (live, df_get_live_in (b));
  for (def_rec = df_get_artificial_defs (b->index); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_FLAGS (def) & DF_REF_AT_TOP)
	SET_HARD_REG_BIT (live, DF_REF_REGNO (def));
    }

  t = head->first;
  insn = BB_HEAD (b);
  while (t)
    {
      /* Search forward until the next reference to the register to be
	 renamed.  */
      while (insn != t->insn)
	{
	  if (INSN_P (insn))
	    {
	      clear_dead_regs (&live, REG_DEAD, REG_NOTES (insn));
	      note_stores (PATTERN (insn), note_sets, (void *) &live);
	      /* Only record currently live regs if we are inside the
		 reg's live range.  */
	      if (t != head->first)
		IOR_HARD_REG_SET (*pset, live);
	      clear_dead_regs (&live, REG_UNUSED, REG_NOTES (insn));
	    }
	  insn = NEXT_INSN (insn);
	}

      IOR_HARD_REG_SET (*pset, live);

      /* For the last reference, also merge in all registers set in the
	 same insn.
	 @@@ We only have take earlyclobbered sets into account.  */
      if (! t->next_use)
	note_stores (PATTERN (insn), note_sets, (void *) pset);

      t = t->next_use;
    }
}

/* Perform register renaming on the current function.  */

static unsigned int
regrename_optimize (void)
{
  int tick[FIRST_PSEUDO_REGISTER];
  int this_tick = 0;
  basic_block bb;
  char *first_obj;

  df_set_flags (DF_LR_RUN_DCE);
  df_note_add_problem ();
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  memset (tick, 0, sizeof tick);

  gcc_obstack_init (&rename_obstack);
  first_obj = XOBNEWVAR (&rename_obstack, char, 0);

  FOR_EACH_BB (bb)
    {
      struct du_head *all_chains = 0;
      HARD_REG_SET unavailable;
      HARD_REG_SET regs_seen;

      CLEAR_HARD_REG_SET (unavailable);

      if (dump_file)
	fprintf (dump_file, "\nBasic block %d:\n", bb->index);

      all_chains = build_def_use (bb);

      if (dump_file)
	dump_def_use_chain (all_chains);

      CLEAR_HARD_REG_SET (unavailable);
      /* Don't clobber traceback for noreturn functions.  */
      if (frame_pointer_needed)
	{
	  add_to_hard_reg_set (&unavailable, Pmode, FRAME_POINTER_REGNUM);
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	  add_to_hard_reg_set (&unavailable, Pmode, HARD_FRAME_POINTER_REGNUM);
#endif
	}

      CLEAR_HARD_REG_SET (regs_seen);
      while (all_chains)
	{
	  int new_reg, best_new_reg;
	  int n_uses;
	  struct du_head *this_head = all_chains;
	  struct du_chain *tmp;
	  HARD_REG_SET this_unavailable;
	  int reg = this_head->regno;
	  int i;

	  all_chains = this_head->next_chain;

	  best_new_reg = reg;

#if 0 /* This just disables optimization opportunities.  */
	  /* Only rename once we've seen the reg more than once.  */
	  if (! TEST_HARD_REG_BIT (regs_seen, reg))
	    {
	      SET_HARD_REG_BIT (regs_seen, reg);
	      continue;
	    }
#endif

	  if (fixed_regs[reg] || global_regs[reg]
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	      || (frame_pointer_needed && reg == HARD_FRAME_POINTER_REGNUM)
#else
	      || (frame_pointer_needed && reg == FRAME_POINTER_REGNUM)
#endif
	      )
	    continue;

	  COPY_HARD_REG_SET (this_unavailable, unavailable);

	  /* Count number of uses, and narrow the set of registers we can
	     use for renaming.  */
	  n_uses = 0;
	  for (tmp = this_head->first; tmp; tmp = tmp->next_use)
	    {
	      if (DEBUG_INSN_P (tmp->insn))
		continue;
	      n_uses++;
	      IOR_COMPL_HARD_REG_SET (this_unavailable,
				      reg_class_contents[tmp->cl]);
	    }

	  if (n_uses < 2)
	    continue;

	  if (this_head->need_caller_save_reg)
	    IOR_HARD_REG_SET (this_unavailable, call_used_reg_set);

	  merge_overlapping_regs (bb, &this_unavailable, this_head);

	  /* Now potential_regs is a reasonable approximation, let's
	     have a closer look at each register still in there.  */
	  for (new_reg = 0; new_reg < FIRST_PSEUDO_REGISTER; new_reg++)
	    {
	      enum machine_mode mode = GET_MODE (*this_head->first->loc);
	      int nregs = hard_regno_nregs[new_reg][mode];

	      for (i = nregs - 1; i >= 0; --i)
	        if (TEST_HARD_REG_BIT (this_unavailable, new_reg + i)
		    || fixed_regs[new_reg + i]
		    || global_regs[new_reg + i]
		    /* Can't use regs which aren't saved by the prologue.  */
		    || (! df_regs_ever_live_p (new_reg + i)
			&& ! call_used_regs[new_reg + i])
#ifdef LEAF_REGISTERS
		    /* We can't use a non-leaf register if we're in a
		       leaf function.  */
		    || (current_function_is_leaf
			&& !LEAF_REGISTERS[new_reg + i])
#endif
#ifdef HARD_REGNO_RENAME_OK
		    || ! HARD_REGNO_RENAME_OK (reg + i, new_reg + i)
#endif
		    )
		  break;
	      if (i >= 0)
		continue;

	      /* See whether it accepts all modes that occur in
		 definition and uses.  */
	      for (tmp = this_head->first; tmp; tmp = tmp->next_use)
		if ((! HARD_REGNO_MODE_OK (new_reg, GET_MODE (*tmp->loc))
		     && ! DEBUG_INSN_P (tmp->insn))
		    || (this_head->need_caller_save_reg
			&& ! (HARD_REGNO_CALL_PART_CLOBBERED
			      (reg, GET_MODE (*tmp->loc)))
			&& (HARD_REGNO_CALL_PART_CLOBBERED
			    (new_reg, GET_MODE (*tmp->loc)))))
		  break;
	      if (! tmp)
		{
		  if (tick[best_new_reg] > tick[new_reg])
		    best_new_reg = new_reg;
		}
	    }

	  if (dump_file)
	    {
	      fprintf (dump_file, "Register %s in insn %d",
		       reg_names[reg], INSN_UID (this_head->first->insn));
	      if (this_head->need_caller_save_reg)
		fprintf (dump_file, " crosses a call");
	    }

	  if (best_new_reg == reg)
	    {
	      tick[reg] = ++this_tick;
	      if (dump_file)
		fprintf (dump_file, "; no available better choice\n");
	      continue;
	    }

	  if (dump_file)
	    fprintf (dump_file, ", renamed as %s\n", reg_names[best_new_reg]);

	  do_replace (this_head, best_new_reg);
	  tick[best_new_reg] = ++this_tick;
	  df_set_regs_ever_live (best_new_reg, true);
	}

      obstack_free (&rename_obstack, first_obj);
    }

  obstack_free (&rename_obstack, NULL);

  if (dump_file)
    fputc ('\n', dump_file);

  return 0;
}

static void
do_replace (struct du_head *head, int reg)
{
  struct du_chain *chain;
  unsigned int base_regno = head->regno;

  gcc_assert (! DEBUG_INSN_P (head->first->insn));

  for (chain = head->first; chain; chain = chain->next_use)
    {
      unsigned int regno = ORIGINAL_REGNO (*chain->loc);
      struct reg_attrs *attr = REG_ATTRS (*chain->loc);
      int reg_ptr = REG_POINTER (*chain->loc);

      if (DEBUG_INSN_P (chain->insn) && REGNO (*chain->loc) != base_regno)
	INSN_VAR_LOCATION_LOC (chain->insn) = gen_rtx_UNKNOWN_VAR_LOC ();
      else
	{
	  rtx note;

	  *chain->loc = gen_raw_REG (GET_MODE (*chain->loc), reg);
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    ORIGINAL_REGNO (*chain->loc) = regno;
	  REG_ATTRS (*chain->loc) = attr;
	  REG_POINTER (*chain->loc) = reg_ptr;

	  for (note = REG_NOTES (chain->insn); note; note = XEXP (note, 1))
	    {
	      if (REG_NOTE_KIND (note) == REG_DEAD
		  || REG_NOTE_KIND (note) == REG_UNUSED)
		{
		  rtx reg = XEXP (note, 0);
		  gcc_assert (HARD_REGISTER_P (reg));

		  if (REGNO (reg) == base_regno)
		    XEXP (note, 0) = *chain->loc;
		}
	    }
	}

      df_insn_rescan (chain->insn);
    }
}


static struct du_head *open_chains;
static struct du_head *closed_chains;

static void
scan_rtx_reg (rtx insn, rtx *loc, enum reg_class cl, enum scan_actions action,
	      enum op_type type)
{
  struct du_head **p;
  rtx x = *loc;
  enum machine_mode mode = GET_MODE (x);
  unsigned this_regno = REGNO (x);
  unsigned this_nregs = hard_regno_nregs[this_regno][mode];

  if (action == mark_write)
    {
      if (type == OP_OUT)
	{
	  struct du_head *head = XOBNEW (&rename_obstack, struct du_head);
	  struct du_chain *this_du = XOBNEW (&rename_obstack, struct du_chain);
	  head->next_chain = open_chains;
	  open_chains = head;
	  head->first = head->last = this_du;
	  head->regno = this_regno;
	  head->nregs = this_nregs;
	  head->need_caller_save_reg = 0;
	  head->terminated = 0;

	  this_du->next_use = 0;
	  this_du->loc = loc;
	  this_du->insn = insn;
	  this_du->cl = cl;
	}
      return;
    }

  if ((type == OP_OUT) != (action == terminate_write || action == mark_access))
    return;

  for (p = &open_chains; *p;)
    {
      struct du_head *head = *p;

      /* Check if the chain has been terminated if it has then skip to
	 the next chain.

	 This can happen when we've already appended the location to
	 the chain in Step 3, but are trying to hide in-out operands
	 from terminate_write in Step 5.  */

      if (head->terminated)
	p = &head->next_chain;
      else
	{
	  int exact_match = (head->regno == this_regno
			     && head->nregs == this_nregs);

	  if (head->regno + head->nregs <= this_regno
	      || this_regno + this_nregs <= head->regno)
	    {
	      p = &head->next_chain;
	      continue;
	    }

	  if (action == mark_read || action == mark_access)
	    {
	      gcc_assert (exact_match || DEBUG_INSN_P (insn));

	      /* ??? Class NO_REGS can happen if the md file makes use of
		 EXTRA_CONSTRAINTS to match registers.  Which is arguably
		 wrong, but there we are.  Since we know not what this may
		 be replaced with, terminate the chain.  */
	      if (cl != NO_REGS)
		{
		  struct du_chain *this_du;
		  this_du = XOBNEW (&rename_obstack, struct du_chain);
		  this_du->next_use = 0;
		  this_du->loc = loc;
		  this_du->insn = insn;
		  this_du->cl = cl;
		  head->last->next_use = this_du;
		  head->last = this_du;
		  return;
		}
	    }

	  if (action != terminate_overlapping_read || ! exact_match)
	    {
	      struct du_head *next = head->next_chain;

	      /* Whether the terminated chain can be used for renaming
	         depends on the action and this being an exact match.
	         In either case, we remove this element from open_chains.  */

	      head->terminated = 1;
	      if ((action == terminate_dead || action == terminate_write)
		  && exact_match)
		{
		  head->next_chain = closed_chains;
		  closed_chains = head;
		  if (dump_file)
		    fprintf (dump_file,
			     "Closing chain %s at insn %d (%s)\n",
			     reg_names[head->regno], INSN_UID (insn),
			     scan_actions_name[(int) action]);
		}
	      else
		{
		  if (dump_file)
		    fprintf (dump_file,
			     "Discarding chain %s at insn %d (%s)\n",
			     reg_names[head->regno], INSN_UID (insn),
			     scan_actions_name[(int) action]);
		}
	      *p = next;
	    }
	  else
	    p = &head->next_chain;
	}
    }
}

/* Adapted from find_reloads_address_1.  CL is INDEX_REG_CLASS or
   BASE_REG_CLASS depending on how the register is being considered.  */

static void
scan_rtx_address (rtx insn, rtx *loc, enum reg_class cl,
		  enum scan_actions action, enum machine_mode mode)
{
  rtx x = *loc;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;
  int i, j;

  if (action == mark_write || action == mark_access)
    return;

  switch (code)
    {
    case PLUS:
      {
	rtx orig_op0 = XEXP (x, 0);
	rtx orig_op1 = XEXP (x, 1);
	RTX_CODE code0 = GET_CODE (orig_op0);
	RTX_CODE code1 = GET_CODE (orig_op1);
	rtx op0 = orig_op0;
	rtx op1 = orig_op1;
	rtx *locI = NULL;
	rtx *locB = NULL;
	enum rtx_code index_code = SCRATCH;

	if (GET_CODE (op0) == SUBREG)
	  {
	    op0 = SUBREG_REG (op0);
	    code0 = GET_CODE (op0);
	  }

	if (GET_CODE (op1) == SUBREG)
	  {
	    op1 = SUBREG_REG (op1);
	    code1 = GET_CODE (op1);
	  }

	if (code0 == MULT || code0 == SIGN_EXTEND || code0 == TRUNCATE
	    || code0 == ZERO_EXTEND || code1 == MEM)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (*locI);
	  }
	else if (code1 == MULT || code1 == SIGN_EXTEND || code1 == TRUNCATE
		 || code1 == ZERO_EXTEND || code0 == MEM)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (*locI);
	  }
	else if (code0 == CONST_INT || code0 == CONST
		 || code0 == SYMBOL_REF || code0 == LABEL_REF)
	  {
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (XEXP (x, 0));
	  }
	else if (code1 == CONST_INT || code1 == CONST
		 || code1 == SYMBOL_REF || code1 == LABEL_REF)
	  {
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (XEXP (x, 1));
	  }
	else if (code0 == REG && code1 == REG)
	  {
	    int index_op;
	    unsigned regno0 = REGNO (op0), regno1 = REGNO (op1);

	    if (REGNO_OK_FOR_INDEX_P (regno1)
		&& regno_ok_for_base_p (regno0, mode, PLUS, REG))
	      index_op = 1;
	    else if (REGNO_OK_FOR_INDEX_P (regno0)
		     && regno_ok_for_base_p (regno1, mode, PLUS, REG))
	      index_op = 0;
	    else if (regno_ok_for_base_p (regno0, mode, PLUS, REG)
		     || REGNO_OK_FOR_INDEX_P (regno1))
	      index_op = 1;
	    else if (regno_ok_for_base_p (regno1, mode, PLUS, REG))
	      index_op = 0;
	    else
	      index_op = 1;

	    locI = &XEXP (x, index_op);
	    locB = &XEXP (x, !index_op);
	    index_code = GET_CODE (*locI);
	  }
	else if (code0 == REG)
	  {
	    locI = &XEXP (x, 0);
	    locB = &XEXP (x, 1);
	    index_code = GET_CODE (*locI);
	  }
	else if (code1 == REG)
	  {
	    locI = &XEXP (x, 1);
	    locB = &XEXP (x, 0);
	    index_code = GET_CODE (*locI);
	  }

	if (locI)
	  scan_rtx_address (insn, locI, INDEX_REG_CLASS, action, mode);
	if (locB)
	  scan_rtx_address (insn, locB, base_reg_class (mode, PLUS, index_code),
			    action, mode);

	return;
      }

    case POST_INC:
    case POST_DEC:
    case POST_MODIFY:
    case PRE_INC:
    case PRE_DEC:
    case PRE_MODIFY:
#ifndef AUTO_INC_DEC
      /* If the target doesn't claim to handle autoinc, this must be
	 something special, like a stack push.  Kill this chain.  */
      action = terminate_all_read;
#endif
      break;

    case MEM:
      scan_rtx_address (insn, &XEXP (x, 0),
			base_reg_class (GET_MODE (x), MEM, SCRATCH), action,
			GET_MODE (x));
      return;

    case REG:
      scan_rtx_reg (insn, loc, cl, action, OP_IN);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx_address (insn, &XEXP (x, i), cl, action, mode);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx_address (insn, &XVECEXP (x, i, j), cl, action, mode);
    }
}

static void
scan_rtx (rtx insn, rtx *loc, enum reg_class cl, enum scan_actions action,
	  enum op_type type)
{
  const char *fmt;
  rtx x = *loc;
  enum rtx_code code = GET_CODE (x);
  int i, j;

  code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_FIXED:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case LABEL_REF:
    case CC0:
    case PC:
      return;

    case REG:
      scan_rtx_reg (insn, loc, cl, action, type);
      return;

    case MEM:
      scan_rtx_address (insn, &XEXP (x, 0),
			base_reg_class (GET_MODE (x), MEM, SCRATCH), action,
			GET_MODE (x));
      return;

    case SET:
      scan_rtx (insn, &SET_SRC (x), cl, action, OP_IN);
      scan_rtx (insn, &SET_DEST (x), cl, action,
		GET_CODE (PATTERN (insn)) == COND_EXEC ? OP_INOUT : OP_OUT);
      return;

    case STRICT_LOW_PART:
      scan_rtx (insn, &XEXP (x, 0), cl, action, OP_INOUT);
      return;

    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      scan_rtx (insn, &XEXP (x, 0), cl, action,
		type == OP_IN ? OP_IN : OP_INOUT);
      scan_rtx (insn, &XEXP (x, 1), cl, action, OP_IN);
      scan_rtx (insn, &XEXP (x, 2), cl, action, OP_IN);
      return;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
    case POST_MODIFY:
    case PRE_MODIFY:
      /* Should only happen inside MEM.  */
      gcc_unreachable ();

    case CLOBBER:
      scan_rtx (insn, &SET_DEST (x), cl, action,
		GET_CODE (PATTERN (insn)) == COND_EXEC ? OP_INOUT : OP_OUT);
      return;

    case EXPR_LIST:
      scan_rtx (insn, &XEXP (x, 0), cl, action, type);
      if (XEXP (x, 1))
	scan_rtx (insn, &XEXP (x, 1), cl, action, type);
      return;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	scan_rtx (insn, &XEXP (x, i), cl, action, type);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  scan_rtx (insn, &XVECEXP (x, i, j), cl, action, type);
    }
}

/* Hide operands of the current insn (of which there are N_OPS) by
   substituting cc0 for them.
   Previous values are stored in the OLD_OPERANDS and OLD_DUPS.
   If INOUT_ONLY is set, we only do this for OP_INOUT type operands.  */

static void
hide_operands (int n_ops, rtx *old_operands, rtx *old_dups,
	       bool inout_only)
{
  int i;
  for (i = 0; i < n_ops; i++)
    {
      old_operands[i] = recog_data.operand[i];
      /* Don't squash match_operator or match_parallel here, since
	 we don't know that all of the contained registers are
	 reachable by proper operands.  */
      if (recog_data.constraints[i][0] == '\0')
	continue;
      if (!inout_only || recog_data.operand_type[i] == OP_INOUT)
	*recog_data.operand_loc[i] = cc0_rtx;
    }
  for (i = 0; i < recog_data.n_dups; i++)
    {
      int opn = recog_data.dup_num[i];
      old_dups[i] = *recog_data.dup_loc[i];
      if (!inout_only || recog_data.operand_type[opn] == OP_INOUT)
	*recog_data.dup_loc[i] = cc0_rtx;
    }
}

/* Undo the substitution performed by hide_operands.  INSN is the insn we
   are processing; the arguments are the same as in hide_operands.  */

static void
restore_operands (rtx insn, int n_ops, rtx *old_operands, rtx *old_dups)
{
  int i;
  for (i = 0; i < recog_data.n_dups; i++)
    *recog_data.dup_loc[i] = old_dups[i];
  for (i = 0; i < n_ops; i++)
    *recog_data.operand_loc[i] = old_operands[i];
  if (recog_data.n_dups)
    df_insn_rescan (insn);
}

/* For each output operand of INSN, call scan_rtx to create a new
   open chain.  */

static void
record_out_operands (rtx insn)
{
  int n_ops = recog_data.n_operands;
  int alt = which_alternative;

  int i;

  for (i = 0; i < n_ops + recog_data.n_dups; i++)
    {
      int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
      rtx *loc = (i < n_ops
		  ? recog_data.operand_loc[opn]
		  : recog_data.dup_loc[i - n_ops]);
      rtx op = *loc;
      enum reg_class cl = recog_op_alt[opn][alt].cl;

      struct du_head *prev_open;

      if (recog_data.operand_type[opn] != OP_OUT)
	continue;

      prev_open = open_chains;
      scan_rtx (insn, loc, cl, mark_write, OP_OUT);

      /* ??? Many targets have output constraints on the SET_DEST
	 of a call insn, which is stupid, since these are certainly
	 ABI defined hard registers.  For these, and for asm operands
	 that originally referenced hard registers, we must record that
	 the chain cannot be renamed.  */
      if (CALL_P (insn)
	  || (asm_noperands (PATTERN (insn)) > 0
	      && REG_P (op)
	      && REGNO (op) == ORIGINAL_REGNO (op)))
	{
	  if (prev_open != open_chains)
	    open_chains->first->cl = NO_REGS;
	}
    }
}

/* Build def/use chain.  */

static struct du_head *
build_def_use (basic_block bb)
{
  rtx insn;

  open_chains = closed_chains = NULL;

  for (insn = BB_HEAD (bb); ; insn = NEXT_INSN (insn))
    {
      if (NONDEBUG_INSN_P (insn))
	{
	  int n_ops;
	  rtx note;
	  rtx old_operands[MAX_RECOG_OPERANDS];
	  rtx old_dups[MAX_DUP_OPERANDS];
	  int i, icode;
	  int alt;
	  int predicated;

	  /* Process the insn, determining its effect on the def-use
	     chains.  We perform the following steps with the register
	     references in the insn:
	     (1) Any read that overlaps an open chain, but doesn't exactly
	         match, causes that chain to be closed.  We can't deal
	         with overlaps yet.
	     (2) Any read outside an operand causes any chain it overlaps
	         with to be closed, since we can't replace it.
	     (3) Any read inside an operand is added if there's already
	         an open chain for it.
	     (4) For any REG_DEAD note we find, close open chains that
	         overlap it.
	     (5) For any write we find, close open chains that overlap it.
	     (6) For any write we find in an operand, make a new chain.
	     (7) For any REG_UNUSED, close any chains we just opened.  */

	  icode = recog_memoized (insn);
	  extract_insn (insn);
	  if (! constrain_operands (1))
	    fatal_insn_not_found (insn);
	  preprocess_constraints ();
	  alt = which_alternative;
	  n_ops = recog_data.n_operands;

	  /* Simplify the code below by rewriting things to reflect
	     matching constraints.  Also promote OP_OUT to OP_INOUT
	     in predicated instructions.  */

	  predicated = GET_CODE (PATTERN (insn)) == COND_EXEC;
	  for (i = 0; i < n_ops; ++i)
	    {
	      int matches = recog_op_alt[i][alt].matches;
	      if (matches >= 0)
		recog_op_alt[i][alt].cl = recog_op_alt[matches][alt].cl;
	      if (matches >= 0 || recog_op_alt[i][alt].matched >= 0
	          || (predicated && recog_data.operand_type[i] == OP_OUT))
		recog_data.operand_type[i] = OP_INOUT;
	    }

	  /* Step 1: Close chains for which we have overlapping reads.  */
	  for (i = 0; i < n_ops; i++)
	    scan_rtx (insn, recog_data.operand_loc[i],
		      NO_REGS, terminate_overlapping_read,
		      recog_data.operand_type[i]);

	  /* Step 2: Close chains for which we have reads outside operands.
	     We do this by munging all operands into CC0, and closing
	     everything remaining.  */

	  hide_operands (n_ops, old_operands, old_dups, false);
	  scan_rtx (insn, &PATTERN (insn), NO_REGS, terminate_all_read,
		    OP_IN);
	  restore_operands (insn, n_ops, old_operands, old_dups);

	  /* Step 2B: Can't rename function call argument registers.  */
	  if (CALL_P (insn) && CALL_INSN_FUNCTION_USAGE (insn))
	    scan_rtx (insn, &CALL_INSN_FUNCTION_USAGE (insn),
		      NO_REGS, terminate_all_read, OP_IN);

	  /* Step 2C: Can't rename asm operands that were originally
	     hard registers.  */
	  if (asm_noperands (PATTERN (insn)) > 0)
	    for (i = 0; i < n_ops; i++)
	      {
		rtx *loc = recog_data.operand_loc[i];
		rtx op = *loc;

		if (REG_P (op)
		    && REGNO (op) == ORIGINAL_REGNO (op)
		    && (recog_data.operand_type[i] == OP_IN
			|| recog_data.operand_type[i] == OP_INOUT))
		  scan_rtx (insn, loc, NO_REGS, terminate_all_read, OP_IN);
	      }

	  /* Step 3: Append to chains for reads inside operands.  */
	  for (i = 0; i < n_ops + recog_data.n_dups; i++)
	    {
	      int opn = i < n_ops ? i : recog_data.dup_num[i - n_ops];
	      rtx *loc = (i < n_ops
			  ? recog_data.operand_loc[opn]
			  : recog_data.dup_loc[i - n_ops]);
	      enum reg_class cl = recog_op_alt[opn][alt].cl;
	      enum op_type type = recog_data.operand_type[opn];

	      /* Don't scan match_operand here, since we've no reg class
		 information to pass down.  Any operands that we could
		 substitute in will be represented elsewhere.  */
	      if (recog_data.constraints[opn][0] == '\0')
		continue;

	      if (recog_op_alt[opn][alt].is_address)
		scan_rtx_address (insn, loc, cl, mark_read, VOIDmode);
	      else
		scan_rtx (insn, loc, cl, mark_read, type);
	    }

	  /* Step 3B: Record updates for regs in REG_INC notes, and
	     source regs in REG_FRAME_RELATED_EXPR notes.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_INC
		|| REG_NOTE_KIND (note) == REG_FRAME_RELATED_EXPR)
	      scan_rtx (insn, &XEXP (note, 0), ALL_REGS, mark_read,
			OP_INOUT);

	  /* Step 4: Close chains for registers that die here.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_DEAD)
	      scan_rtx (insn, &XEXP (note, 0), NO_REGS, terminate_dead,
			OP_IN);

	  /* Step 4B: If this is a call, any chain live at this point
	     requires a caller-saved reg.  */
	  if (CALL_P (insn))
	    {
	      struct du_head *p;
	      for (p = open_chains; p; p = p->next_chain)
		p->need_caller_save_reg = 1;
	    }

	  /* Step 5: Close open chains that overlap writes.  Similar to
	     step 2, we hide in-out operands, since we do not want to
	     close these chains.  */

	  hide_operands (n_ops, old_operands, old_dups, true);
	  scan_rtx (insn, &PATTERN (insn), NO_REGS, terminate_write, OP_IN);
	  restore_operands (insn, n_ops, old_operands, old_dups);

	  /* Step 6: Begin new chains for writes inside operands.  */
	  record_out_operands (insn);

	  /* Step 6B: Record destination regs in REG_FRAME_RELATED_EXPR
	     notes for update.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_FRAME_RELATED_EXPR)
	      scan_rtx (insn, &XEXP (note, 0), ALL_REGS, mark_access,
			OP_INOUT);

	  /* Step 7: Close chains for registers that were never
	     really used here.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_UNUSED)
	      scan_rtx (insn, &XEXP (note, 0), NO_REGS, terminate_dead,
			OP_IN);
	}
      else if (DEBUG_INSN_P (insn)
	       && !VAR_LOC_UNKNOWN_P (INSN_VAR_LOCATION_LOC (insn)))
	{
	  scan_rtx (insn, &INSN_VAR_LOCATION_LOC (insn),
		    ALL_REGS, mark_read, OP_IN);
	}
      if (insn == BB_END (bb))
	break;
    }

  /* Since we close every chain when we find a REG_DEAD note, anything that
     is still open lives past the basic block, so it can't be renamed.  */
  return closed_chains;
}

/* Dump all def/use chains in CHAINS to DUMP_FILE.  They are
   printed in reverse order as that's how we build them.  */

static void
dump_def_use_chain (struct du_head *head)
{
  while (head)
    {
      struct du_chain *this_du = head->first;
      fprintf (dump_file, "Register %s (%d):",
	       reg_names[head->regno], head->nregs);
      while (this_du)
	{
	  fprintf (dump_file, " %d [%s]", INSN_UID (this_du->insn),
		   reg_class_names[this_du->cl]);
	  this_du = this_du->next_use;
	}
      fprintf (dump_file, "\n");
      head = head->next_chain;
    }
}


static bool
gate_handle_regrename (void)
{
  return (optimize > 0 && (flag_rename_registers));
}

struct rtl_opt_pass pass_regrename =
{
 {
  RTL_PASS,
  "rnreg",                              /* name */
  gate_handle_regrename,                /* gate */
  regrename_optimize,                   /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_RENAME_REGISTERS,                  /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish | TODO_verify_rtl_sharing |
  TODO_dump_func                        /* todo_flags_finish */
 }
};

