/* Optimize by combining instructions for GNU compiler.
   Copyright (C) 1987, 1988, 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This module is essentially the "combiner" phase of the U. of Arizona
   Portable Optimizer, but redone to work on our list-structured
   representation for RTL instead of their string representation.

   The LOG_LINKS of each insn identify the most recent assignment
   to each REG used in the insn.  It is a list of previous insns,
   each of which contains a SET for a REG that is used in this insn
   and not used or set in between.  LOG_LINKs never cross basic blocks.
   They were set up by the preceding pass (lifetime analysis).

   We try to combine each pair of insns joined by a logical link.
   We also try to combine triples of insns A, B and C when
   C has a link back to B and B has a link back to A.

   LOG_LINKS does not have links for use of the CC0.  They don't
   need to, because the insn that sets the CC0 is always immediately
   before the insn that tests it.  So we always regard a branch
   insn as having a logical link to the preceding insn.  The same is true
   for an insn explicitly using CC0.

   We check (with use_crosses_set_p) to avoid combining in such a way
   as to move a computation to a place where its value would be different.

   Combination is done by mathematically substituting the previous
   insn(s) values for the regs they set into the expressions in
   the later insns that refer to these regs.  If the result is a valid insn
   for our target machine, according to the machine description,
   we install it, delete the earlier insns, and update the data flow
   information (LOG_LINKS and REG_NOTES) for what we did.

   There are a few exceptions where the dataflow information created by
   flow.c aren't completely updated:

   - reg_live_length is not updated
   - reg_n_refs is not adjusted in the rare case when a register is
     no longer required in a computation
   - there are extremely rare cases (see distribute_regnotes) when a
     REG_DEAD note is lost
   - a LOG_LINKS entry that refers to an insn with multiple SETs may be
     removed because there is no way to know which register it was 
     linking

   To simplify substitution, we combine only when the earlier insn(s)
   consist of only a single assignment.  To simplify updating afterward,
   we never combine when a subroutine call appears in the middle.

   Since we do not represent assignments to CC0 explicitly except when that
   is all an insn does, there is no LOG_LINKS entry in an insn that uses
   the condition code for the insn that set the condition code.
   Fortunately, these two insns must be consecutive.
   Therefore, every JUMP_INSN is taken to have an implicit logical link
   to the preceding insn.  This is not quite right, since non-jumps can
   also use the condition code; but in practice such insns would not
   combine anyway.  */

#include "config.h"
#include "gvarargs.h"
#include "rtl.h"
#include "flags.h"
#include "regs.h"
#include "expr.h"
#include "basic-block.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "insn-attr.h"
#include "recog.h"
#include "real.h"
#include <stdio.h>

/* It is not safe to use ordinary gen_lowpart in combine.
   Use gen_lowpart_for_combine instead.  See comments there.  */
#define gen_lowpart dont_use_gen_lowpart_you_dummy

/* Number of attempts to combine instructions in this function.  */

static int combine_attempts;

/* Number of attempts that got as far as substitution in this function.  */

static int combine_merges;

/* Number of instructions combined with added SETs in this function.  */

static int combine_extras;

/* Number of instructions combined in this function.  */

static int combine_successes;

/* Totals over entire compilation.  */

static int total_attempts, total_merges, total_extras, total_successes;

/* Vector mapping INSN_UIDs to cuids.
   The cuids are like uids but increase monotonically always.
   Combine always uses cuids so that it can compare them.
   But actually renumbering the uids, which we used to do,
   proves to be a bad idea because it makes it hard to compare
   the dumps produced by earlier passes with those from later passes.  */

static int *uid_cuid;

/* Get the cuid of an insn.  */

#define INSN_CUID(INSN) (uid_cuid[INSN_UID (INSN)])

/* Maximum register number, which is the size of the tables below.  */

static int combine_max_regno;

/* Record last point of death of (hard or pseudo) register n.  */

static rtx *reg_last_death;

/* Record last point of modification of (hard or pseudo) register n.  */

static rtx *reg_last_set;

/* Record the cuid of the last insn that invalidated memory
   (anything that writes memory, and subroutine calls, but not pushes).  */

static int mem_last_set;

/* Record the cuid of the last CALL_INSN
   so we can tell whether a potential combination crosses any calls.  */

static int last_call_cuid;

/* When `subst' is called, this is the insn that is being modified
   (by combining in a previous insn).  The PATTERN of this insn
   is still the old pattern partially modified and it should not be
   looked at, but this may be used to examine the successors of the insn
   to judge whether a simplification is valid.  */

static rtx subst_insn;

/* This is the lowest CUID that `subst' is currently dealing with.
   get_last_value will not return a value if the register was set at or
   after this CUID.  If not for this mechanism, we could get confused if
   I2 or I1 in try_combine were an insn that used the old value of a register
   to obtain a new value.  In that case, we might erroneously get the
   new value of the register when we wanted the old one.  */

static int subst_low_cuid;

/* This is the value of undobuf.num_undo when we started processing this 
   substitution.  This will prevent gen_rtx_combine from re-used a piece
   from the previous expression.  Doing so can produce circular rtl
   structures.  */

static int previous_num_undos;

/* The next group of arrays allows the recording of the last value assigned
   to (hard or pseudo) register n.  We use this information to see if a
   operation being processed is redundant given a prior operation performed
   on the register.  For example, an `and' with a constant is redundant if
   all the zero bits are already known to be turned off.

   We use an approach similar to that used by cse, but change it in the
   following ways:

   (1) We do not want to reinitialize at each label.
   (2) It is useful, but not critical, to know the actual value assigned
       to a register.  Often just its form is helpful.

   Therefore, we maintain the following arrays:

   reg_last_set_value		the last value assigned
   reg_last_set_label		records the value of label_tick when the
				register was assigned
   reg_last_set_table_tick	records the value of label_tick when a
				value using the register is assigned
   reg_last_set_invalid		set to non-zero when it is not valid
				to use the value of this register in some
				register's value

   To understand the usage of these tables, it is important to understand
   the distinction between the value in reg_last_set_value being valid
   and the register being validly contained in some other expression in the
   table.

   Entry I in reg_last_set_value is valid if it is non-zero, and either
   reg_n_sets[i] is 1 or reg_last_set_label[i] == label_tick.

   Register I may validly appear in any expression returned for the value
   of another register if reg_n_sets[i] is 1.  It may also appear in the
   value for register J if reg_last_set_label[i] < reg_last_set_label[j] or
   reg_last_set_invalid[j] is zero.

   If an expression is found in the table containing a register which may
   not validly appear in an expression, the register is replaced by
   something that won't match, (clobber (const_int 0)).

   reg_last_set_invalid[i] is set non-zero when register I is being assigned
   to and reg_last_set_table_tick[i] == label_tick.  */

/* Record last value assigned to (hard or pseudo) register n. */

static rtx *reg_last_set_value;

/* Record the value of label_tick when the value for register n is placed in
   reg_last_set_value[n].  */

static short *reg_last_set_label;

/* Record the value of label_tick when an expression involving register n
   is placed in reg_last_set_value. */

static short *reg_last_set_table_tick;

/* Set non-zero if references to register n in expressions should not be
   used.  */

static char *reg_last_set_invalid;

/* Incremented for each label. */

static short label_tick;

/* Some registers that are set more than once and used in more than one
   basic block are nevertheless always set in similar ways.  For example,
   a QImode register may be loaded from memory in two places on a machine
   where byte loads zero extend.

   We record in the following array what we know about the significant
   bits of a register, specifically which bits are known to be zero.

   If an entry is zero, it means that we don't know anything special.  */

static HOST_WIDE_INT *reg_significant;

/* Mode used to compute significance in reg_significant.  It is the largest
   integer mode that can fit in HOST_BITS_PER_WIDE_INT.  */

static enum machine_mode significant_mode;

/* Nonzero if we know that a register has some leading bits that are always
   equal to the sign bit.  */

static char *reg_sign_bit_copies;

/* Nonzero when reg_significant and reg_sign_bit_copies can be safely used.
   It is zero while computing them and after combine has completed.  This
   former test prevents propagating values based on previously set values,
   which can be incorrect if a variable is modified in a loop.  */

static int significant_valid;

/* Record one modification to rtl structure
   to be undone by storing old_contents into *where.
   is_int is 1 if the contents are an int.  */

struct undo
{
  int is_int;
  union {rtx rtx; int i;} old_contents;
  union {rtx *rtx; int *i;} where;
};

/* Record a bunch of changes to be undone, up to MAX_UNDO of them.
   num_undo says how many are currently recorded.

   storage is nonzero if we must undo the allocation of new storage.
   The value of storage is what to pass to obfree.

   other_insn is nonzero if we have modified some other insn in the process
   of working on subst_insn.  It must be verified too.  */

#define MAX_UNDO 50

struct undobuf
{
  int num_undo;
  char *storage;
  struct undo undo[MAX_UNDO];
  rtx other_insn;
};

static struct undobuf undobuf;

/* Substitute NEWVAL, an rtx expression, into INTO, a place in some
   insn.  The substitution can be undone by undo_all.  If INTO is already
   set to NEWVAL, do not record this change.  Because computing NEWVAL might
   also call SUBST, we have to compute it before we put anything into
   the undo table.  */

#define SUBST(INTO, NEWVAL)  \
 do { rtx _new = (NEWVAL);						\
      if (undobuf.num_undo < MAX_UNDO)					\
	{								\
	  undobuf.undo[undobuf.num_undo].is_int = 0;			\
	  undobuf.undo[undobuf.num_undo].where.rtx = &INTO;		\
	  undobuf.undo[undobuf.num_undo].old_contents.rtx = INTO;	\
	  INTO = _new;							\
	  if (undobuf.undo[undobuf.num_undo].old_contents.rtx != INTO)	\
	    undobuf.num_undo++; 					\
	}								\
    } while (0)

/* Similar to SUBST, but NEWVAL is an int.  INTO will normally be an XINT
   expression.
   Note that substitution for the value of a CONST_INT is not safe.  */

#define SUBST_INT(INTO, NEWVAL)  \
 do { if (undobuf.num_undo < MAX_UNDO)					\
{									\
	  undobuf.undo[undobuf.num_undo].is_int = 1;			\
	  undobuf.undo[undobuf.num_undo].where.i = (int *) &INTO;	\
	  undobuf.undo[undobuf.num_undo].old_contents.i = INTO;		\
	  INTO = NEWVAL;						\
	  if (undobuf.undo[undobuf.num_undo].old_contents.i != INTO)	\
	    undobuf.num_undo++;						\
	}								\
     } while (0)

/* Number of times the pseudo being substituted for
   was found and replaced.  */

static int n_occurrences;

static void set_significant ();
static void move_deaths ();
rtx remove_death ();
static void record_value_for_reg ();
static void record_dead_and_set_regs ();
static int use_crosses_set_p ();
static rtx try_combine ();
static rtx *find_split_point ();
static rtx subst ();
static void undo_all ();
static int reg_dead_at_p ();
static rtx expand_compound_operation ();
static rtx expand_field_assignment ();
static rtx make_extraction ();
static int get_pos_from_mask ();
static rtx force_to_mode ();
static rtx known_cond ();
static rtx make_field_assignment ();
static rtx make_compound_operation ();
static rtx apply_distributive_law ();
static rtx simplify_and_const_int ();
static unsigned HOST_WIDE_INT significant_bits ();
static int num_sign_bit_copies ();
static int merge_outer_ops ();
static rtx simplify_shift_const ();
static int recog_for_combine ();
static rtx gen_lowpart_for_combine ();
static rtx gen_rtx_combine ();
static rtx gen_binary ();
static rtx gen_unary ();
static enum rtx_code simplify_comparison ();
static int reversible_comparison_p ();
static int get_last_value_validate ();
static rtx get_last_value ();
static void distribute_notes ();
static void distribute_links ();

/* Main entry point for combiner.  F is the first insn of the function.
   NREGS is the first unused pseudo-reg number.  */

void
combine_instructions (f, nregs)
     rtx f;
     int nregs;
{
  register rtx insn, next, prev;
  register int i;
  register rtx links, nextlinks;

  combine_attempts = 0;
  combine_merges = 0;
  combine_extras = 0;
  combine_successes = 0;

  combine_max_regno = nregs;

  reg_last_death = (rtx *) alloca (nregs * sizeof (rtx));
  reg_last_set = (rtx *) alloca (nregs * sizeof (rtx));
  reg_last_set_value = (rtx *) alloca (nregs * sizeof (rtx));
  reg_last_set_table_tick = (short *) alloca (nregs * sizeof (short));
  reg_last_set_label = (short *) alloca (nregs * sizeof (short));
  reg_last_set_invalid = (char *) alloca (nregs * sizeof (char));
  reg_significant = (HOST_WIDE_INT *) alloca (nregs * sizeof (HOST_WIDE_INT));
  reg_sign_bit_copies = (char *) alloca (nregs * sizeof (char));

  bzero (reg_last_death, nregs * sizeof (rtx));
  bzero (reg_last_set, nregs * sizeof (rtx));
  bzero (reg_last_set_value, nregs * sizeof (rtx));
  bzero (reg_last_set_table_tick, nregs * sizeof (short));
  bzero (reg_last_set_invalid, nregs * sizeof (char));
  bzero (reg_significant, nregs * sizeof (HOST_WIDE_INT));
  bzero (reg_sign_bit_copies, nregs * sizeof (char));

  init_recog_no_volatile ();

  /* Compute maximum uid value so uid_cuid can be allocated.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    if (INSN_UID (insn) > i)
      i = INSN_UID (insn);

  uid_cuid = (int *) alloca ((i + 1) * sizeof (int));

  significant_mode = mode_for_size (HOST_BITS_PER_WIDE_INT, MODE_INT, 0);

  /* Don't use reg_significant when computing it.  This can cause problems
     when, for example, we have j <<= 1 in a loop.  */

  significant_valid = 0;

  /* Compute the mapping from uids to cuids.
     Cuids are numbers assigned to insns, like uids,
     except that cuids increase monotonically through the code. 

     Scan all SETs and see if we can deduce anything about what
     bits are significant for some registers.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      INSN_CUID (insn) = ++i;
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
	note_stores (PATTERN (insn), set_significant);
    }

  significant_valid = 1;

  /* Now scan all the insns in forward order.  */

  label_tick = 1;
  last_call_cuid = 0;
  mem_last_set = 0;

  for (insn = f; insn; insn = next ? next : NEXT_INSN (insn))
    {
      next = 0;

      if (GET_CODE (insn) == CODE_LABEL)
	label_tick++;

      else if (GET_CODE (insn) == INSN
	       || GET_CODE (insn) == CALL_INSN
	       || GET_CODE (insn) == JUMP_INSN)
	{
	  /* Try this insn with each insn it links back to.  */

	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    if ((next = try_combine (insn, XEXP (links, 0), NULL_RTX)) != 0)
	      goto retry;

	  /* Try each sequence of three linked insns ending with this one.  */

	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    for (nextlinks = LOG_LINKS (XEXP (links, 0)); nextlinks;
		 nextlinks = XEXP (nextlinks, 1))
	      if ((next = try_combine (insn, XEXP (links, 0),
				       XEXP (nextlinks, 0))) != 0)
		goto retry;

#ifdef HAVE_cc0
	  /* Try to combine a jump insn that uses CC0
	     with a preceding insn that sets CC0, and maybe with its
	     logical predecessor as well.
	     This is how we make decrement-and-branch insns.
	     We need this special code because data flow connections
	     via CC0 do not get entered in LOG_LINKS.  */

	  if (GET_CODE (insn) == JUMP_INSN
	      && (prev = prev_nonnote_insn (insn)) != 0
	      && GET_CODE (prev) == INSN
	      && sets_cc0_p (PATTERN (prev)))
	    {
	      if ((next = try_combine (insn, prev, NULL_RTX)) != 0)
		goto retry;

	      for (nextlinks = LOG_LINKS (prev); nextlinks;
		   nextlinks = XEXP (nextlinks, 1))
		if ((next = try_combine (insn, prev,
					 XEXP (nextlinks, 0))) != 0)
		  goto retry;
	    }

	  /* Do the same for an insn that explicitly references CC0.  */
	  if (GET_CODE (insn) == INSN
	      && (prev = prev_nonnote_insn (insn)) != 0
	      && GET_CODE (prev) == INSN
	      && sets_cc0_p (PATTERN (prev))
	      && GET_CODE (PATTERN (insn)) == SET
	      && reg_mentioned_p (cc0_rtx, SET_SRC (PATTERN (insn))))
	    {
	      if ((next = try_combine (insn, prev, NULL_RTX)) != 0)
		goto retry;

	      for (nextlinks = LOG_LINKS (prev); nextlinks;
		   nextlinks = XEXP (nextlinks, 1))
		if ((next = try_combine (insn, prev,
					 XEXP (nextlinks, 0))) != 0)
		  goto retry;
	    }

	  /* Finally, see if any of the insns that this insn links to
	     explicitly references CC0.  If so, try this insn, that insn,
	     and its predecessor if it sets CC0.  */
	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    if (GET_CODE (XEXP (links, 0)) == INSN
		&& GET_CODE (PATTERN (XEXP (links, 0))) == SET
		&& reg_mentioned_p (cc0_rtx, SET_SRC (PATTERN (XEXP (links, 0))))
		&& (prev = prev_nonnote_insn (XEXP (links, 0))) != 0
		&& GET_CODE (prev) == INSN
		&& sets_cc0_p (PATTERN (prev))
		&& (next = try_combine (insn, XEXP (links, 0), prev)) != 0)
	      goto retry;
#endif

	  /* Try combining an insn with two different insns whose results it
	     uses.  */
	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    for (nextlinks = XEXP (links, 1); nextlinks;
		 nextlinks = XEXP (nextlinks, 1))
	      if ((next = try_combine (insn, XEXP (links, 0),
				       XEXP (nextlinks, 0))) != 0)
		goto retry;

	  if (GET_CODE (insn) != NOTE)
	    record_dead_and_set_regs (insn);

	retry:
	  ;
	}
    }

  total_attempts += combine_attempts;
  total_merges += combine_merges;
  total_extras += combine_extras;
  total_successes += combine_successes;

  significant_valid = 0;
}

/* Called via note_stores.  If X is a pseudo that is used in more than
   one basic block, is narrower that HOST_BITS_PER_WIDE_INT, and is being
   set, record what bits are significant.  If we are clobbering X,
   ignore this "set" because the clobbered value won't be used. 

   If we are setting only a portion of X and we can't figure out what
   portion, assume all bits will be used since we don't know what will
   be happening.

   Similarly, set how many bits of X are known to be copies of the sign bit
   at all locations in the function.  This is the smallest number implied 
   by any set of X.  */

static void
set_significant (x, set)
     rtx x;
     rtx set;
{
  int num;

  if (GET_CODE (x) == REG
      && REGNO (x) >= FIRST_PSEUDO_REGISTER
      && reg_n_sets[REGNO (x)] > 1
      && reg_basic_block[REGNO (x)] < 0
      && GET_MODE_BITSIZE (GET_MODE (x)) <= HOST_BITS_PER_WIDE_INT)
    {
      if (GET_CODE (set) == CLOBBER)
	return;

      /* If this is a complex assignment, see if we can convert it into a
	 simple assignment.  */
      set = expand_field_assignment (set);
      if (SET_DEST (set) == x)
	{
	  reg_significant[REGNO (x)]
	    |= significant_bits (SET_SRC (set), significant_mode);
	  num = num_sign_bit_copies (SET_SRC (set), GET_MODE (x));
	  if (reg_sign_bit_copies[REGNO (x)] == 0
	      || reg_sign_bit_copies[REGNO (x)] > num)
	    reg_sign_bit_copies[REGNO (x)] = num;
	}
      else
	{
	  reg_significant[REGNO (x)] = GET_MODE_MASK (GET_MODE (x));
	  reg_sign_bit_copies[REGNO (x)] = 0;
	}
    }
}

/* See if INSN can be combined into I3.  PRED and SUCC are optionally
   insns that were previously combined into I3 or that will be combined
   into the merger of INSN and I3.

   Return 0 if the combination is not allowed for any reason.

   If the combination is allowed, *PDEST will be set to the single 
   destination of INSN and *PSRC to the single source, and this function
   will return 1.  */

static int
can_combine_p (insn, i3, pred, succ, pdest, psrc)
     rtx insn;
     rtx i3;
     rtx pred, succ;
     rtx *pdest, *psrc;
{
  int i;
  rtx set = 0, src, dest;
  rtx p, link;
  int all_adjacent = (succ ? (next_active_insn (insn) == succ
			      && next_active_insn (succ) == i3)
		      : next_active_insn (insn) == i3);

  /* Can combine only if previous insn is a SET of a REG, a SUBREG or CC0.
     or a PARALLEL consisting of such a SET and CLOBBERs. 

     If INSN has CLOBBER parallel parts, ignore them for our processing.
     By definition, these happen during the execution of the insn.  When it
     is merged with another insn, all bets are off.  If they are, in fact,
     needed and aren't also supplied in I3, they may be added by
     recog_for_combine.  Otherwise, it won't match. 

     We can also ignore a SET whose SET_DEST is mentioned in a REG_UNUSED
     note.

     Get the source and destination of INSN.  If more than one, can't 
     combine.  */
     
  if (GET_CODE (PATTERN (insn)) == SET)
    set = PATTERN (insn);
  else if (GET_CODE (PATTERN (insn)) == PARALLEL
	   && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
    {
      for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	{
	  rtx elt = XVECEXP (PATTERN (insn), 0, i);

	  switch (GET_CODE (elt))
	    {
	      /* We can ignore CLOBBERs.  */
	    case CLOBBER:
	      break;

	    case SET:
	      /* Ignore SETs whose result isn't used but not those that
		 have side-effects.  */
	      if (find_reg_note (insn, REG_UNUSED, SET_DEST (elt))
		  && ! side_effects_p (elt))
		break;

	      /* If we have already found a SET, this is a second one and
		 so we cannot combine with this insn.  */
	      if (set)
		return 0;

	      set = elt;
	      break;

	    default:
	      /* Anything else means we can't combine.  */
	      return 0;
	    }
	}

      if (set == 0
	  /* If SET_SRC is an ASM_OPERANDS we can't throw away these CLOBBERs,
	     so don't do anything with it.  */
	  || GET_CODE (SET_SRC (set)) == ASM_OPERANDS)
	return 0;
    }
  else
    return 0;

  if (set == 0)
    return 0;

  set = expand_field_assignment (set);
  src = SET_SRC (set), dest = SET_DEST (set);

  /* Don't eliminate a store in the stack pointer.  */
  if (dest == stack_pointer_rtx
      /* Don't install a subreg involving two modes not tieable.
	 It can worsen register allocation, and can even make invalid reload
	 insns, since the reg inside may need to be copied from in the
	 outside mode, and that may be invalid if it is an fp reg copied in
	 integer mode.  As a special exception, we can allow this if
	 I3 is simply copying DEST, a REG,  to CC0.  */
      || (GET_CODE (src) == SUBREG
	  && ! MODES_TIEABLE_P (GET_MODE (src), GET_MODE (SUBREG_REG (src)))
#ifdef HAVE_cc0
	  && ! (GET_CODE (i3) == INSN && GET_CODE (PATTERN (i3)) == SET
		&& SET_DEST (PATTERN (i3)) == cc0_rtx
		&& GET_CODE (dest) == REG && dest == SET_SRC (PATTERN (i3)))
#endif
	  )
      /* If we couldn't eliminate a field assignment, we can't combine.  */
      || GET_CODE (dest) == ZERO_EXTRACT || GET_CODE (dest) == STRICT_LOW_PART
      /* Don't combine with an insn that sets a register to itself if it has
	 a REG_EQUAL note.  This may be part of a REG_NO_CONFLICT sequence.  */
      || (rtx_equal_p (src, dest) && find_reg_note (insn, REG_EQUAL, NULL_RTX))
      /* Can't merge a function call.  */
      || GET_CODE (src) == CALL
      /* Don't substitute into an incremented register.  */
      || FIND_REG_INC_NOTE (i3, dest)
      || (succ && FIND_REG_INC_NOTE (succ, dest))
      /* Don't combine the end of a libcall into anything.  */
      || find_reg_note (insn, REG_RETVAL, NULL_RTX)
      /* Make sure that DEST is not used after SUCC but before I3.  */
      || (succ && ! all_adjacent
	  && reg_used_between_p (dest, succ, i3))
      /* Make sure that the value that is to be substituted for the register
	 does not use any registers whose values alter in between.  However,
	 If the insns are adjacent, a use can't cross a set even though we
	 think it might (this can happen for a sequence of insns each setting
	 the same destination; reg_last_set of that register might point to
	 a NOTE).  Also, don't move a volatile asm across any other insns.  */
      || (! all_adjacent
	  && (use_crosses_set_p (src, INSN_CUID (insn))
	      || (GET_CODE (src) == ASM_OPERANDS && MEM_VOLATILE_P (src))))
      /* If there is a REG_NO_CONFLICT note for DEST in I3 or SUCC, we get
	 better register allocation by not doing the combine.  */
      || find_reg_note (i3, REG_NO_CONFLICT, dest)
      || (succ && find_reg_note (succ, REG_NO_CONFLICT, dest))
      /* Don't combine across a CALL_INSN, because that would possibly
	 change whether the life span of some REGs crosses calls or not,
	 and it is a pain to update that information.
	 Exception: if source is a constant, moving it later can't hurt.
	 Accept that special case, because it helps -fforce-addr a lot.  */
      || (INSN_CUID (insn) < last_call_cuid && ! CONSTANT_P (src)))
    return 0;

  /* DEST must either be a REG or CC0.  */
  if (GET_CODE (dest) == REG)
    {
      /* If register alignment is being enforced for multi-word items in all
	 cases except for parameters, it is possible to have a register copy
	 insn referencing a hard register that is not allowed to contain the
	 mode being copied and which would not be valid as an operand of most
	 insns.  Eliminate this problem by not combining with such an insn.

	 Also, on some machines we don't want to extend the life of a hard
	 register.  */

      if (GET_CODE (src) == REG
	  && ((REGNO (dest) < FIRST_PSEUDO_REGISTER
	       && ! HARD_REGNO_MODE_OK (REGNO (dest), GET_MODE (dest)))
#ifdef SMALL_REGISTER_CLASSES
	      /* Don't extend the life of a hard register.  */
	      || REGNO (src) < FIRST_PSEUDO_REGISTER
#else
	      || (REGNO (src) < FIRST_PSEUDO_REGISTER
		  && ! HARD_REGNO_MODE_OK (REGNO (src), GET_MODE (src)))
#endif
	  ))
	return 0;
    }
  else if (GET_CODE (dest) != CC0)
    return 0;

  /* Don't substitute for a register intended as a clobberable operand.
     Similarly, don't substitute an expression containing a register that
     will be clobbered in I3.  */
  if (GET_CODE (PATTERN (i3)) == PARALLEL)
    for (i = XVECLEN (PATTERN (i3), 0) - 1; i >= 0; i--)
      if (GET_CODE (XVECEXP (PATTERN (i3), 0, i)) == CLOBBER
	  && (reg_overlap_mentioned_p (XEXP (XVECEXP (PATTERN (i3), 0, i), 0),
				       src)
	      || rtx_equal_p (XEXP (XVECEXP (PATTERN (i3), 0, i), 0), dest)))
	return 0;

  /* If INSN contains anything volatile, or is an `asm' (whether volatile
     or not), reject, unless nothing volatile comes between it and I3,
     with the exception of SUCC.  */

  if (GET_CODE (src) == ASM_OPERANDS || volatile_refs_p (src))
    for (p = NEXT_INSN (insn); p != i3; p = NEXT_INSN (p))
      if (GET_RTX_CLASS (GET_CODE (p)) == 'i'
	  && p != succ && volatile_refs_p (PATTERN (p)))
	return 0;

  /* If INSN or I2 contains an autoincrement or autodecrement,
     make sure that register is not used between there and I3,
     and not already used in I3 either.
     Also insist that I3 not be a jump; if it were one
     and the incremented register were spilled, we would lose.  */

#ifdef AUTO_INC_DEC
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_INC
	&& (GET_CODE (i3) == JUMP_INSN
	    || reg_used_between_p (XEXP (link, 0), insn, i3)
	    || reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (i3))))
      return 0;
#endif

#ifdef HAVE_cc0
  /* Don't combine an insn that follows a CC0-setting insn.
     An insn that uses CC0 must not be separated from the one that sets it.
     We do, however, allow I2 to follow a CC0-setting insn if that insn
     is passed as I1; in that case it will be deleted also.
     We also allow combining in this case if all the insns are adjacent
     because that would leave the two CC0 insns adjacent as well.
     It would be more logical to test whether CC0 occurs inside I1 or I2,
     but that would be much slower, and this ought to be equivalent.  */

  p = prev_nonnote_insn (insn);
  if (p && p != pred && GET_CODE (p) == INSN && sets_cc0_p (PATTERN (p))
      && ! all_adjacent)
    return 0;
#endif

  /* If we get here, we have passed all the tests and the combination is
     to be allowed.  */

  *pdest = dest;
  *psrc = src;

  return 1;
}

/* LOC is the location within I3 that contains its pattern or the component
   of a PARALLEL of the pattern.  We validate that it is valid for combining.

   One problem is if I3 modifies its output, as opposed to replacing it
   entirely, we can't allow the output to contain I2DEST or I1DEST as doing
   so would produce an insn that is not equivalent to the original insns.

   Consider:

         (set (reg:DI 101) (reg:DI 100))
	 (set (subreg:SI (reg:DI 101) 0) <foo>)

   This is NOT equivalent to:

         (parallel [(set (subreg:SI (reg:DI 100) 0) <foo>)
	 	    (set (reg:DI 101) (reg:DI 100))])

   Not only does this modify 100 (in which case it might still be valid
   if 100 were dead in I2), it sets 101 to the ORIGINAL value of 100. 

   We can also run into a problem if I2 sets a register that I1
   uses and I1 gets directly substituted into I3 (not via I2).  In that
   case, we would be getting the wrong value of I2DEST into I3, so we
   must reject the combination.  This case occurs when I2 and I1 both
   feed into I3, rather than when I1 feeds into I2, which feeds into I3.
   If I1_NOT_IN_SRC is non-zero, it means that finding I1 in the source
   of a SET must prevent combination from occurring.

   On machines where SMALL_REGISTER_CLASSES is defined, we don't combine
   if the destination of a SET is a hard register.

   Before doing the above check, we first try to expand a field assignment
   into a set of logical operations.

   If PI3_DEST_KILLED is non-zero, it is a pointer to a location in which
   we place a register that is both set and used within I3.  If more than one
   such register is detected, we fail.

   Return 1 if the combination is valid, zero otherwise.  */

static int
combinable_i3pat (i3, loc, i2dest, i1dest, i1_not_in_src, pi3dest_killed)
     rtx i3;
     rtx *loc;
     rtx i2dest;
     rtx i1dest;
     int i1_not_in_src;
     rtx *pi3dest_killed;
{
  rtx x = *loc;

  if (GET_CODE (x) == SET)
    {
      rtx set = expand_field_assignment (x);
      rtx dest = SET_DEST (set);
      rtx src = SET_SRC (set);
      rtx inner_dest = dest, inner_src = src;

      SUBST (*loc, set);

      while (GET_CODE (inner_dest) == STRICT_LOW_PART
	     || GET_CODE (inner_dest) == SUBREG
	     || GET_CODE (inner_dest) == ZERO_EXTRACT)
	inner_dest = XEXP (inner_dest, 0);

  /* We probably don't need this any more now that LIMIT_RELOAD_CLASS
     was added.  */
#if 0
      while (GET_CODE (inner_src) == STRICT_LOW_PART
	     || GET_CODE (inner_src) == SUBREG
	     || GET_CODE (inner_src) == ZERO_EXTRACT)
	inner_src = XEXP (inner_src, 0);

      /* If it is better that two different modes keep two different pseudos,
	 avoid combining them.  This avoids producing the following pattern
	 on a 386:
	  (set (subreg:SI (reg/v:QI 21) 0)
	       (lshiftrt:SI (reg/v:SI 20)
	           (const_int 24)))
	 If that were made, reload could not handle the pair of
	 reg 20/21, since it would try to get any GENERAL_REGS
	 but some of them don't handle QImode.  */

      if (rtx_equal_p (inner_src, i2dest)
	  && GET_CODE (inner_dest) == REG
	  && ! MODES_TIEABLE_P (GET_MODE (i2dest), GET_MODE (inner_dest)))
	return 0;
#endif

      /* Check for the case where I3 modifies its output, as
	 discussed above.  */
      if ((inner_dest != dest
	   && (reg_overlap_mentioned_p (i2dest, inner_dest)
	       || (i1dest && reg_overlap_mentioned_p (i1dest, inner_dest))))
	  /* This is the same test done in can_combine_p except that we
	     allow a hard register with SMALL_REGISTER_CLASSES if SRC is a
	     CALL operation.  */
	  || (GET_CODE (inner_dest) == REG
	      && REGNO (inner_dest) < FIRST_PSEUDO_REGISTER
#ifdef SMALL_REGISTER_CLASSES
	      && GET_CODE (src) != CALL
#else
	      && ! HARD_REGNO_MODE_OK (REGNO (inner_dest),
				       GET_MODE (inner_dest))
#endif
	      )

	  || (i1_not_in_src && reg_overlap_mentioned_p (i1dest, src)))
	return 0;

      /* If DEST is used in I3, it is being killed in this insn,
	 so record that for later.  */
      if (pi3dest_killed && GET_CODE (dest) == REG
	  && reg_referenced_p (dest, PATTERN (i3)))
	{
	  if (*pi3dest_killed)
	    return 0;

	  *pi3dest_killed = dest;
	}
    }

  else if (GET_CODE (x) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (x, 0); i++)
	if (! combinable_i3pat (i3, &XVECEXP (x, 0, i), i2dest, i1dest,
				i1_not_in_src, pi3dest_killed))
	  return 0;
    }

  return 1;
}

/* Try to combine the insns I1 and I2 into I3.
   Here I1 and I2 appear earlier than I3.
   I1 can be zero; then we combine just I2 into I3.
 
   It we are combining three insns and the resulting insn is not recognized,
   try splitting it into two insns.  If that happens, I2 and I3 are retained
   and I1 is pseudo-deleted by turning it into a NOTE.  Otherwise, I1 and I2
   are pseudo-deleted.

   If we created two insns, return I2; otherwise return I3.
   Return 0 if the combination does not work.  Then nothing is changed.  */

static rtx
try_combine (i3, i2, i1)
     register rtx i3, i2, i1;
{
  /* New patterns for I3 and I3, respectively.  */
  rtx newpat, newi2pat = 0;
  /* Indicates need to preserve SET in I1 or I2 in I3 if it is not dead.  */
  int added_sets_1, added_sets_2;
  /* Total number of SETs to put into I3.  */
  int total_sets;
  /* Nonzero is I2's body now appears in I3.  */
  int i2_is_used;
  /* INSN_CODEs for new I3, new I2, and user of condition code.  */
  int insn_code_number, i2_code_number, other_code_number;
  /* Contains I3 if the destination of I3 is used in its source, which means
     that the old life of I3 is being killed.  If that usage is placed into
     I2 and not in I3, a REG_DEAD note must be made.  */
  rtx i3dest_killed = 0;
  /* SET_DEST and SET_SRC of I2 and I1.  */
  rtx i2dest, i2src, i1dest = 0, i1src = 0;
  /* PATTERN (I2), or a copy of it in certain cases.  */
  rtx i2pat;
  /* Indicates if I2DEST or I1DEST is in I2SRC or I1_SRC.  */
  int i2dest_in_i2src, i1dest_in_i1src = 0, i2dest_in_i1src = 0;
  int i1_feeds_i3 = 0;
  /* Notes that must be added to REG_NOTES in I3 and I2.  */
  rtx new_i3_notes, new_i2_notes;

  int maxreg;
  rtx temp;
  register rtx link;
  int i;

  /* If any of I1, I2, and I3 isn't really an insn, we can't do anything.
     This can occur when flow deletes an insn that it has merged into an
     auto-increment address.  We also can't do anything if I3 has a
     REG_LIBCALL note since we don't want to disrupt the contiguity of a
     libcall.  */

  if (GET_RTX_CLASS (GET_CODE (i3)) != 'i'
      || GET_RTX_CLASS (GET_CODE (i2)) != 'i'
      || (i1 && GET_RTX_CLASS (GET_CODE (i1)) != 'i')
      || find_reg_note (i3, REG_LIBCALL, NULL_RTX))
    return 0;

  combine_attempts++;

  undobuf.num_undo = previous_num_undos = 0;
  undobuf.other_insn = 0;

  /* Save the current high-water-mark so we can free storage if we didn't
     accept this combination.  */
  undobuf.storage = (char *) oballoc (0);

  /* If I1 and I2 both feed I3, they can be in any order.  To simplify the
     code below, set I1 to be the earlier of the two insns.  */
  if (i1 && INSN_CUID (i1) > INSN_CUID (i2))
    temp = i1, i1 = i2, i2 = temp;

  /* First check for one important special-case that the code below will
     not handle.  Namely, the case where I1 is zero, I2 has multiple sets,
     and I3 is a SET whose SET_SRC is a SET_DEST in I2.  In that case,
     we may be able to replace that destination with the destination of I3.
     This occurs in the common code where we compute both a quotient and
     remainder into a structure, in which case we want to do the computation
     directly into the structure to avoid register-register copies.

     We make very conservative checks below and only try to handle the
     most common cases of this.  For example, we only handle the case
     where I2 and I3 are adjacent to avoid making difficult register
     usage tests.  */

  if (i1 == 0 && GET_CODE (i3) == INSN && GET_CODE (PATTERN (i3)) == SET
      && GET_CODE (SET_SRC (PATTERN (i3))) == REG
      && REGNO (SET_SRC (PATTERN (i3))) >= FIRST_PSEUDO_REGISTER
#ifdef SMALL_REGISTER_CLASSES
      && (GET_CODE (SET_DEST (PATTERN (i3))) != REG
	  || REGNO (SET_DEST (PATTERN (i3))) >= FIRST_PSEUDO_REGISTER)
#endif
      && find_reg_note (i3, REG_DEAD, SET_SRC (PATTERN (i3)))
      && GET_CODE (PATTERN (i2)) == PARALLEL
      && ! side_effects_p (SET_DEST (PATTERN (i3)))
      /* If the dest of I3 is a ZERO_EXTRACT or STRICT_LOW_PART, the code
	 below would need to check what is inside (and reg_overlap_mentioned_p
	 doesn't support those codes anyway).  Don't allow those destinations;
	 the resulting insn isn't likely to be recognized anyway.  */
      && GET_CODE (SET_DEST (PATTERN (i3))) != ZERO_EXTRACT
      && GET_CODE (SET_DEST (PATTERN (i3))) != STRICT_LOW_PART
      && ! reg_overlap_mentioned_p (SET_SRC (PATTERN (i3)),
				    SET_DEST (PATTERN (i3)))
      && next_real_insn (i2) == i3)
    {
      rtx p2 = PATTERN (i2);

      /* Make sure that the destination of I3,
	 which we are going to substitute into one output of I2,
	 is not used within another output of I2.  We must avoid making this:
	 (parallel [(set (mem (reg 69)) ...)
		    (set (reg 69) ...)])
	 which is not well-defined as to order of actions.
	 (Besides, reload can't handle output reloads for this.)

	 The problem can also happen if the dest of I3 is a memory ref,
	 if another dest in I2 is an indirect memory ref.  */
      for (i = 0; i < XVECLEN (p2, 0); i++)
	if (GET_CODE (XVECEXP (p2, 0, i)) == SET
	    && reg_overlap_mentioned_p (SET_DEST (PATTERN (i3)),
					SET_DEST (XVECEXP (p2, 0, i))))
	  break;

      if (i == XVECLEN (p2, 0))
	for (i = 0; i < XVECLEN (p2, 0); i++)
	  if (SET_DEST (XVECEXP (p2, 0, i)) == SET_SRC (PATTERN (i3)))
	    {
	      combine_merges++;

	      subst_insn = i3;
	      subst_low_cuid = INSN_CUID (i2);

	      added_sets_2 = 0;
	      i2dest = SET_SRC (PATTERN (i3));

	      /* Replace the dest in I2 with our dest and make the resulting
		 insn the new pattern for I3.  Then skip to where we
		 validate the pattern.  Everything was set up above.  */
	      SUBST (SET_DEST (XVECEXP (p2, 0, i)), 
		     SET_DEST (PATTERN (i3)));

	      newpat = p2;
	      goto validate_replacement;
	    }
    }

#ifndef HAVE_cc0
  /* If we have no I1 and I2 looks like:
	(parallel [(set (reg:CC X) (compare:CC OP (const_int 0)))
		   (set Y OP)])
     make up a dummy I1 that is
	(set Y OP)
     and change I2 to be
        (set (reg:CC X) (compare:CC Y (const_int 0)))

     (We can ignore any trailing CLOBBERs.)

     This undoes a previous combination and allows us to match a branch-and-
     decrement insn.  */

  if (i1 == 0 && GET_CODE (PATTERN (i2)) == PARALLEL
      && XVECLEN (PATTERN (i2), 0) >= 2
      && GET_CODE (XVECEXP (PATTERN (i2), 0, 0)) == SET
      && (GET_MODE_CLASS (GET_MODE (SET_DEST (XVECEXP (PATTERN (i2), 0, 0))))
	  == MODE_CC)
      && GET_CODE (SET_SRC (XVECEXP (PATTERN (i2), 0, 0))) == COMPARE
      && XEXP (SET_SRC (XVECEXP (PATTERN (i2), 0, 0)), 1) == const0_rtx
      && GET_CODE (XVECEXP (PATTERN (i2), 0, 1)) == SET
      && GET_CODE (SET_DEST (XVECEXP (PATTERN (i2), 0, 1))) == REG
      && rtx_equal_p (XEXP (SET_SRC (XVECEXP (PATTERN (i2), 0, 0)), 0),
		      SET_SRC (XVECEXP (PATTERN (i2), 0, 1))))
    {
      for (i =  XVECLEN (PATTERN (i2), 0) - 1; i >= 2; i--)
	if (GET_CODE (XVECEXP (PATTERN (i2), 0, i)) != CLOBBER)
	  break;

      if (i == 1)
	{
	  /* We make I1 with the same INSN_UID as I2.  This gives it
	     the same INSN_CUID for value tracking.  Our fake I1 will
	     never appear in the insn stream so giving it the same INSN_UID
	     as I2 will not cause a problem.  */

	  i1 = gen_rtx (INSN, VOIDmode, INSN_UID (i2), 0, i2,
			XVECEXP (PATTERN (i2), 0, 1), -1, 0, 0);

	  SUBST (PATTERN (i2), XVECEXP (PATTERN (i2), 0, 0));
	  SUBST (XEXP (SET_SRC (PATTERN (i2)), 0),
		 SET_DEST (PATTERN (i1)));
	}
    }
#endif

  /* Verify that I2 and I1 are valid for combining.  */
  if (! can_combine_p (i2, i3, i1, NULL_RTX, &i2dest, &i2src)
      || (i1 && ! can_combine_p (i1, i3, NULL_RTX, i2, &i1dest, &i1src)))
    {
      undo_all ();
      return 0;
    }

  /* Record whether I2DEST is used in I2SRC and similarly for the other
     cases.  Knowing this will help in register status updating below.  */
  i2dest_in_i2src = reg_overlap_mentioned_p (i2dest, i2src);
  i1dest_in_i1src = i1 && reg_overlap_mentioned_p (i1dest, i1src);
  i2dest_in_i1src = i1 && reg_overlap_mentioned_p (i2dest, i1src);

  /* See if I1 directly feeds into I3.  It does if I1DEST is not used
     in I2SRC.  */
  i1_feeds_i3 = i1 && ! reg_overlap_mentioned_p (i1dest, i2src);

  /* Ensure that I3's pattern can be the destination of combines.  */
  if (! combinable_i3pat (i3, &PATTERN (i3), i2dest, i1dest,
			  i1 && i2dest_in_i1src && i1_feeds_i3,
			  &i3dest_killed))
    {
      undo_all ();
      return 0;
    }

  /* If I3 has an inc, then give up if I1 or I2 uses the reg that is inc'd.
     We used to do this EXCEPT in one case: I3 has a post-inc in an
     output operand.  However, that exception can give rise to insns like
     	mov r3,(r3)+
     which is a famous insn on the PDP-11 where the value of r3 used as the
     source was model-dependent.  Avoid this sort of thing.  */

#if 0
  if (!(GET_CODE (PATTERN (i3)) == SET
	&& GET_CODE (SET_SRC (PATTERN (i3))) == REG
	&& GET_CODE (SET_DEST (PATTERN (i3))) == MEM
	&& (GET_CODE (XEXP (SET_DEST (PATTERN (i3)), 0)) == POST_INC
	    || GET_CODE (XEXP (SET_DEST (PATTERN (i3)), 0)) == POST_DEC)))
    /* It's not the exception.  */
#endif
#ifdef AUTO_INC_DEC
    for (link = REG_NOTES (i3); link; link = XEXP (link, 1))
      if (REG_NOTE_KIND (link) == REG_INC
	  && (reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (i2))
	      || (i1 != 0
		  && reg_overlap_mentioned_p (XEXP (link, 0), PATTERN (i1)))))
	{
	  undo_all ();
	  return 0;
	}
#endif

  /* See if the SETs in I1 or I2 need to be kept around in the merged
     instruction: whenever the value set there is still needed past I3.
     For the SETs in I2, this is easy: we see if I2DEST dies or is set in I3.

     For the SET in I1, we have two cases:  If I1 and I2 independently
     feed into I3, the set in I1 needs to be kept around if I1DEST dies
     or is set in I3.  Otherwise (if I1 feeds I2 which feeds I3), the set
     in I1 needs to be kept around unless I1DEST dies or is set in either
     I2 or I3.  We can distinguish these cases by seeing if I2SRC mentions
     I1DEST.  If so, we know I1 feeds into I2.  */

  added_sets_2 = ! dead_or_set_p (i3, i2dest);

  added_sets_1
    = i1 && ! (i1_feeds_i3 ? dead_or_set_p (i3, i1dest)
	       : (dead_or_set_p (i3, i1dest) || dead_or_set_p (i2, i1dest)));

  /* If the set in I2 needs to be kept around, we must make a copy of
     PATTERN (I2), so that when we substitute I1SRC for I1DEST in
     PATTERN (I2), we are only substituting for the original I1DEST, not into
     an already-substituted copy.  This also prevents making self-referential
     rtx.  If I2 is a PARALLEL, we just need the piece that assigns I2SRC to
     I2DEST.  */

  i2pat = (GET_CODE (PATTERN (i2)) == PARALLEL
	   ? gen_rtx (SET, VOIDmode, i2dest, i2src)
	   : PATTERN (i2));

  if (added_sets_2)
    i2pat = copy_rtx (i2pat);

  combine_merges++;

  /* Substitute in the latest insn for the regs set by the earlier ones.  */

  maxreg = max_reg_num ();

  subst_insn = i3;

  /* It is possible that the source of I2 or I1 may be performing an
     unneeded operation, such as a ZERO_EXTEND of something that is known
     to have the high part zero.  Handle that case by letting subst look at
     the innermost one of them.

     Another way to do this would be to have a function that tries to
     simplify a single insn instead of merging two or more insns.  We don't
     do this because of the potential of infinite loops and because
     of the potential extra memory required.  However, doing it the way
     we are is a bit of a kludge and doesn't catch all cases.

     But only do this if -fexpensive-optimizations since it slows things down
     and doesn't usually win.  */

  if (flag_expensive_optimizations)
    {
      /* Pass pc_rtx so no substitutions are done, just simplifications.
	 The cases that we are interested in here do not involve the few
	 cases were is_replaced is checked.  */
      if (i1)
	{
	  subst_low_cuid = INSN_CUID (i1);
	  i1src = subst (i1src, pc_rtx, pc_rtx, 0, 0);
	}
      else
	{
	  subst_low_cuid = INSN_CUID (i2);
	  i2src = subst (i2src, pc_rtx, pc_rtx, 0, 0);
	}

      previous_num_undos = undobuf.num_undo;
    }

#ifndef HAVE_cc0
  /* Many machines that don't use CC0 have insns that can both perform an
     arithmetic operation and set the condition code.  These operations will
     be represented as a PARALLEL with the first element of the vector
     being a COMPARE of an arithmetic operation with the constant zero.
     The second element of the vector will set some pseudo to the result
     of the same arithmetic operation.  If we simplify the COMPARE, we won't
     match such a pattern and so will generate an extra insn.   Here we test
     for this case, where both the comparison and the operation result are
     needed, and make the PARALLEL by just replacing I2DEST in I3SRC with
     I2SRC.  Later we will make the PARALLEL that contains I2.  */

  if (i1 == 0 && added_sets_2 && GET_CODE (PATTERN (i3)) == SET
      && GET_CODE (SET_SRC (PATTERN (i3))) == COMPARE
      && XEXP (SET_SRC (PATTERN (i3)), 1) == const0_rtx
      && rtx_equal_p (XEXP (SET_SRC (PATTERN (i3)), 0), i2dest))
    {
      rtx *cc_use;
      enum machine_mode compare_mode;

      newpat = PATTERN (i3);
      SUBST (XEXP (SET_SRC (newpat), 0), i2src);

      i2_is_used = 1;

#ifdef EXTRA_CC_MODES
      /* See if a COMPARE with the operand we substituted in should be done
	 with the mode that is currently being used.  If not, do the same
	 processing we do in `subst' for a SET; namely, if the destination
	 is used only once, try to replace it with a register of the proper
	 mode and also replace the COMPARE.  */
      if (undobuf.other_insn == 0
	  && (cc_use = find_single_use (SET_DEST (newpat), i3,
					&undobuf.other_insn))
	  && ((compare_mode = SELECT_CC_MODE (GET_CODE (*cc_use),
					      i2src, const0_rtx))
	      != GET_MODE (SET_DEST (newpat))))
	{
	  int regno = REGNO (SET_DEST (newpat));
	  rtx new_dest = gen_rtx (REG, compare_mode, regno);

	  if (regno < FIRST_PSEUDO_REGISTER
	      || (reg_n_sets[regno] == 1 && ! added_sets_2
		  && ! REG_USERVAR_P (SET_DEST (newpat))))
	    {
	      if (regno >= FIRST_PSEUDO_REGISTER)
		SUBST (regno_reg_rtx[regno], new_dest);

	      SUBST (SET_DEST (newpat), new_dest);
	      SUBST (XEXP (*cc_use, 0), new_dest);
	      SUBST (SET_SRC (newpat),
		     gen_rtx_combine (COMPARE, compare_mode,
				      i2src, const0_rtx));
	    }
	  else
	    undobuf.other_insn = 0;
	}
#endif	  
    }
  else
#endif
    {
      n_occurrences = 0;		/* `subst' counts here */

      /* If I1 feeds into I2 (not into I3) and I1DEST is in I1SRC, we
	 need to make a unique copy of I2SRC each time we substitute it
	 to avoid self-referential rtl.  */

      subst_low_cuid = INSN_CUID (i2);
      newpat = subst (PATTERN (i3), i2dest, i2src, 0,
		      ! i1_feeds_i3 && i1dest_in_i1src);
      previous_num_undos = undobuf.num_undo;

      /* Record whether i2's body now appears within i3's body.  */
      i2_is_used = n_occurrences;
    }

  /* If we already got a failure, don't try to do more.  Otherwise,
     try to substitute in I1 if we have it.  */

  if (i1 && GET_CODE (newpat) != CLOBBER)
    {
      /* Before we can do this substitution, we must redo the test done
	 above (see detailed comments there) that ensures  that I1DEST
	 isn't mentioned in any SETs in NEWPAT that are field assignments. */

      if (! combinable_i3pat (NULL_RTX, &newpat, i1dest, NULL_RTX,
			      0, NULL_PTR))
	{
	  undo_all ();
	  return 0;
	}

      n_occurrences = 0;
      subst_low_cuid = INSN_CUID (i1);
      newpat = subst (newpat, i1dest, i1src, 0, 0);
      previous_num_undos = undobuf.num_undo;
    }

  /* Fail if an autoincrement side-effect has been duplicated.  Be careful
     to count all the ways that I2SRC and I1SRC can be used.  */
  if ((FIND_REG_INC_NOTE (i2, NULL_RTX) != 0
       && i2_is_used + added_sets_2 > 1)
      || (i1 != 0 && FIND_REG_INC_NOTE (i1, NULL_RTX) != 0
	  && (n_occurrences + added_sets_1 + (added_sets_2 && ! i1_feeds_i3)
	      > 1))
      /* Fail if we tried to make a new register (we used to abort, but there's
	 really no reason to).  */
      || max_reg_num () != maxreg
      /* Fail if we couldn't do something and have a CLOBBER.  */
      || GET_CODE (newpat) == CLOBBER)
    {
      undo_all ();
      return 0;
    }

  /* If the actions of the earlier insns must be kept
     in addition to substituting them into the latest one,
     we must make a new PARALLEL for the latest insn
     to hold additional the SETs.  */

  if (added_sets_1 || added_sets_2)
    {
      combine_extras++;

      if (GET_CODE (newpat) == PARALLEL)
	{
	  rtvec old = XVEC (newpat, 0);
	  total_sets = XVECLEN (newpat, 0) + added_sets_1 + added_sets_2;
	  newpat = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (total_sets));
	  bcopy (&old->elem[0], &XVECEXP (newpat, 0, 0),
		 sizeof (old->elem[0]) * old->num_elem);
	}
      else
	{
	  rtx old = newpat;
	  total_sets = 1 + added_sets_1 + added_sets_2;
	  newpat = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (total_sets));
	  XVECEXP (newpat, 0, 0) = old;
	}

     if (added_sets_1)
       XVECEXP (newpat, 0, --total_sets)
	 = (GET_CODE (PATTERN (i1)) == PARALLEL
	    ? gen_rtx (SET, VOIDmode, i1dest, i1src) : PATTERN (i1));

     if (added_sets_2)
	{
	  /* If there is no I1, use I2's body as is.  We used to also not do
	     the subst call below if I2 was substituted into I3,
	     but that could lose a simplification.  */
	  if (i1 == 0)
	    XVECEXP (newpat, 0, --total_sets) = i2pat;
	  else
	    /* See comment where i2pat is assigned.  */
	    XVECEXP (newpat, 0, --total_sets)
	      = subst (i2pat, i1dest, i1src, 0, 0);
	}
    }

  /* We come here when we are replacing a destination in I2 with the
     destination of I3.  */
 validate_replacement:

  /* Is the result of combination a valid instruction?  */
  insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);

  /* If the result isn't valid, see if it is a PARALLEL of two SETs where
     the second SET's destination is a register that is unused.  In that case,
     we just need the first SET.   This can occur when simplifying a divmod
     insn.  We *must* test for this case here because the code below that
     splits two independent SETs doesn't handle this case correctly when it
     updates the register status.  Also check the case where the first
     SET's destination is unused.  That would not cause incorrect code, but
     does cause an unneeded insn to remain.  */

  if (insn_code_number < 0 && GET_CODE (newpat) == PARALLEL
      && XVECLEN (newpat, 0) == 2
      && GET_CODE (XVECEXP (newpat, 0, 0)) == SET
      && GET_CODE (XVECEXP (newpat, 0, 1)) == SET
      && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) == REG
      && find_reg_note (i3, REG_UNUSED, SET_DEST (XVECEXP (newpat, 0, 1)))
      && ! side_effects_p (SET_SRC (XVECEXP (newpat, 0, 1)))
      && asm_noperands (newpat) < 0)
    {
      newpat = XVECEXP (newpat, 0, 0);
      insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);
    }

  else if (insn_code_number < 0 && GET_CODE (newpat) == PARALLEL
	   && XVECLEN (newpat, 0) == 2
	   && GET_CODE (XVECEXP (newpat, 0, 0)) == SET
	   && GET_CODE (XVECEXP (newpat, 0, 1)) == SET
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 0))) == REG
	   && find_reg_note (i3, REG_UNUSED, SET_DEST (XVECEXP (newpat, 0, 0)))
	   && ! side_effects_p (SET_SRC (XVECEXP (newpat, 0, 0)))
	   && asm_noperands (newpat) < 0)
    {
      newpat = XVECEXP (newpat, 0, 1);
      insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);
    }

  /* See if this is an XOR.  If so, perhaps the problem is that the
     constant is out of range.  Replace it with a complemented XOR with
     a complemented constant; it might be in range.  */

  else if (insn_code_number < 0 && GET_CODE (newpat) == SET
	   && GET_CODE (SET_SRC (newpat)) == XOR
	   && GET_CODE (XEXP (SET_SRC (newpat), 1)) == CONST_INT
	   && ((temp = simplify_unary_operation (NOT,
						 GET_MODE (SET_SRC (newpat)),
						 XEXP (SET_SRC (newpat), 1),
						 GET_MODE (SET_SRC (newpat))))
	       != 0))
    {
      enum machine_mode i_mode = GET_MODE (SET_SRC (newpat));
      rtx pat
	= gen_rtx_combine (SET, VOIDmode, SET_DEST (newpat),
			   gen_unary (NOT, i_mode,
				      gen_binary (XOR, i_mode,
						  XEXP (SET_SRC (newpat), 0),
						  temp)));

      insn_code_number = recog_for_combine (&pat, i3, &new_i3_notes);
      if (insn_code_number >= 0)
	newpat = pat;
    }
							
  /* If we were combining three insns and the result is a simple SET
     with no ASM_OPERANDS that wasn't recognized, try to split it into two
     insns.  There are two ways to do this.  It can be split using a 
     machine-specific method (like when you have an addition of a large
     constant) or by combine in the function find_split_point.  */

  if (i1 && insn_code_number < 0 && GET_CODE (newpat) == SET
      && asm_noperands (newpat) < 0)
    {
      rtx m_split, *split;
      rtx ni2dest = i2dest;

      /* See if the MD file can split NEWPAT.  If it can't, see if letting it
	 use I2DEST as a scratch register will help.  In the latter case,
	 convert I2DEST to the mode of the source of NEWPAT if we can.  */

      m_split = split_insns (newpat, i3);

      /* We can only use I2DEST as a scratch reg if it doesn't overlap any
	 inputs of NEWPAT.  */

      /* ??? If I2DEST is not safe, and I1DEST exists, then it would be
	 possible to try that as a scratch reg.  This would require adding
	 more code to make it work though.  */

      if (m_split == 0 && ! reg_overlap_mentioned_p (ni2dest, newpat))
	{
	  /* If I2DEST is a hard register or the only use of a pseudo,
	     we can change its mode.  */
	  if (GET_MODE (SET_DEST (newpat)) != GET_MODE (i2dest)
	      && GET_MODE (SET_DEST (newpat)) != VOIDmode
	      && GET_CODE (i2dest) == REG
	      && (REGNO (i2dest) < FIRST_PSEUDO_REGISTER
		  || (reg_n_sets[REGNO (i2dest)] == 1 && ! added_sets_2
		      && ! REG_USERVAR_P (i2dest))))
	    ni2dest = gen_rtx (REG, GET_MODE (SET_DEST (newpat)),
			       REGNO (i2dest));

	  m_split = split_insns (gen_rtx (PARALLEL, VOIDmode,
					  gen_rtvec (2, newpat,
						     gen_rtx (CLOBBER,
							      VOIDmode,
							      ni2dest))),
				 i3);
	}

      if (m_split && GET_CODE (m_split) == SEQUENCE
	  && XVECLEN (m_split, 0) == 2
	  && (next_real_insn (i2) == i3
	      || ! use_crosses_set_p (PATTERN (XVECEXP (m_split, 0, 0)),
				      INSN_CUID (i2))))
	{
	  rtx i2set, i3set;
	  rtx newi3pat = PATTERN (XVECEXP (m_split, 0, 1));
	  newi2pat = PATTERN (XVECEXP (m_split, 0, 0));

	  i3set = single_set (XVECEXP (m_split, 0, 1));
	  i2set = single_set (XVECEXP (m_split, 0, 0));

	  /* In case we changed the mode of I2DEST, replace it in the
	     pseudo-register table here.  We can't do it above in case this
	     code doesn't get executed and we do a split the other way.  */

	  if (REGNO (i2dest) >= FIRST_PSEUDO_REGISTER)
	    SUBST (regno_reg_rtx[REGNO (i2dest)], ni2dest);

	  i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);

	  /* If I2 or I3 has multiple SETs, we won't know how to track
	     register status, so don't use these insns.  */

	  if (i2_code_number >= 0 && i2set && i3set)
	    insn_code_number = recog_for_combine (&newi3pat, i3,
						  &new_i3_notes);

	  if (insn_code_number >= 0)
	    newpat = newi3pat;

	  /* It is possible that both insns now set the destination of I3.
	     If so, we must show an extra use of it.  */

	  if (insn_code_number >= 0 && GET_CODE (SET_DEST (i3set)) == REG
	      && GET_CODE (SET_DEST (i2set)) == REG
	      && REGNO (SET_DEST (i3set)) == REGNO (SET_DEST (i2set)))
	    reg_n_sets[REGNO (SET_DEST (i2set))]++;
	}

      /* If we can split it and use I2DEST, go ahead and see if that
	 helps things be recognized.  Verify that none of the registers
	 are set between I2 and I3.  */
      if (insn_code_number < 0 && (split = find_split_point (&newpat, i3)) != 0
#ifdef HAVE_cc0
	  && GET_CODE (i2dest) == REG
#endif
	  /* We need I2DEST in the proper mode.  If it is a hard register
	     or the only use of a pseudo, we can change its mode.  */
	  && (GET_MODE (*split) == GET_MODE (i2dest)
	      || GET_MODE (*split) == VOIDmode
	      || REGNO (i2dest) < FIRST_PSEUDO_REGISTER
	      || (reg_n_sets[REGNO (i2dest)] == 1 && ! added_sets_2
		  && ! REG_USERVAR_P (i2dest)))
	  && (next_real_insn (i2) == i3
	      || ! use_crosses_set_p (*split, INSN_CUID (i2)))
	  /* We can't overwrite I2DEST if its value is still used by
	     NEWPAT.  */
	  && ! reg_referenced_p (i2dest, newpat))
	{
	  rtx newdest = i2dest;

	  /* Get NEWDEST as a register in the proper mode.  We have already
	     validated that we can do this.  */
	  if (GET_MODE (i2dest) != GET_MODE (*split)
	      && GET_MODE (*split) != VOIDmode)
	    {
	      newdest = gen_rtx (REG, GET_MODE (*split), REGNO (i2dest));

	      if (REGNO (i2dest) >= FIRST_PSEUDO_REGISTER)
		SUBST (regno_reg_rtx[REGNO (i2dest)], newdest);
	    }

	  /* If *SPLIT is a (mult FOO (const_int pow2)), convert it to
	     an ASHIFT.  This can occur if it was inside a PLUS and hence
	     appeared to be a memory address.  This is a kludge.  */
	  if (GET_CODE (*split) == MULT
	      && GET_CODE (XEXP (*split, 1)) == CONST_INT
	      && (i = exact_log2 (INTVAL (XEXP (*split, 1)))) >= 0)
	    SUBST (*split, gen_rtx_combine (ASHIFT, GET_MODE (*split),
					    XEXP (*split, 0), GEN_INT (i)));

#ifdef INSN_SCHEDULING
	  /* If *SPLIT is a paradoxical SUBREG, when we split it, it should
	     be written as a ZERO_EXTEND.  */
	  if (GET_CODE (*split) == SUBREG
	      && GET_CODE (SUBREG_REG (*split)) == MEM)
	    SUBST (*split, gen_rtx_combine (ZERO_EXTEND, GET_MODE (*split),
					    XEXP (*split, 0)));
#endif

	  newi2pat = gen_rtx_combine (SET, VOIDmode, newdest, *split);
	  SUBST (*split, newdest);
	  i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);
	  if (i2_code_number >= 0)
	    insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);
	}
    }

  /* Check for a case where we loaded from memory in a narrow mode and
     then sign extended it, but we need both registers.  In that case,
     we have a PARALLEL with both loads from the same memory location.
     We can split this into a load from memory followed by a register-register
     copy.  This saves at least one insn, more if register allocation can
     eliminate the copy.  */

  else if (i1 && insn_code_number < 0 && asm_noperands (newpat) < 0
	   && GET_CODE (newpat) == PARALLEL
	   && XVECLEN (newpat, 0) == 2
	   && GET_CODE (XVECEXP (newpat, 0, 0)) == SET
	   && GET_CODE (SET_SRC (XVECEXP (newpat, 0, 0))) == SIGN_EXTEND
	   && GET_CODE (XVECEXP (newpat, 0, 1)) == SET
	   && rtx_equal_p (SET_SRC (XVECEXP (newpat, 0, 1)),
			   XEXP (SET_SRC (XVECEXP (newpat, 0, 0)), 0))
	   && ! use_crosses_set_p (SET_SRC (XVECEXP (newpat, 0, 1)),
				   INSN_CUID (i2))
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != ZERO_EXTRACT
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != STRICT_LOW_PART
	   && ! reg_overlap_mentioned_p (SET_DEST (XVECEXP (newpat, 0, 1)),
					 SET_SRC (XVECEXP (newpat, 0, 1)))
	   && ! find_reg_note (i3, REG_UNUSED,
			       SET_DEST (XVECEXP (newpat, 0, 0))))
    {
      rtx ni2dest;

      newi2pat = XVECEXP (newpat, 0, 0);
      ni2dest = SET_DEST (XVECEXP (newpat, 0, 0));
      newpat = XVECEXP (newpat, 0, 1);
      SUBST (SET_SRC (newpat),
	     gen_lowpart_for_combine (GET_MODE (SET_SRC (newpat)), ni2dest));
      i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);
      if (i2_code_number >= 0)
	insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);

      if (insn_code_number >= 0)
	{
	  rtx insn;
	  rtx link;

	  /* If we will be able to accept this, we have made a change to the
	     destination of I3.  This can invalidate a LOG_LINKS pointing
	     to I3.  No other part of combine.c makes such a transformation.

	     The new I3 will have a destination that was previously the
	     destination of I1 or I2 and which was used in i2 or I3.  Call
	     distribute_links to make a LOG_LINK from the next use of
	     that destination.  */

	  PATTERN (i3) = newpat;
	  distribute_links (gen_rtx (INSN_LIST, VOIDmode, i3, NULL_RTX));

	  /* I3 now uses what used to be its destination and which is
	     now I2's destination.  That means we need a LOG_LINK from
	     I3 to I2.  But we used to have one, so we still will.

	     However, some later insn might be using I2's dest and have
	     a LOG_LINK pointing at I3.  We must remove this link.
	     The simplest way to remove the link is to point it at I1,
	     which we know will be a NOTE.  */

	  for (insn = NEXT_INSN (i3);
	       insn && GET_CODE (insn) != CODE_LABEL
	       && GET_CODE (PREV_INSN (insn)) != JUMP_INSN;
	       insn = NEXT_INSN (insn))
	    {
	      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
		  && reg_referenced_p (ni2dest, PATTERN (insn)))
		{
		  for (link = LOG_LINKS (insn); link;
		       link = XEXP (link, 1))
		    if (XEXP (link, 0) == i3)
		      XEXP (link, 0) = i1;

		  break;
		}
	    }
	}
    }
	    
  /* Similarly, check for a case where we have a PARALLEL of two independent
     SETs but we started with three insns.  In this case, we can do the sets
     as two separate insns.  This case occurs when some SET allows two
     other insns to combine, but the destination of that SET is still live.  */

  else if (i1 && insn_code_number < 0 && asm_noperands (newpat) < 0
	   && GET_CODE (newpat) == PARALLEL
	   && XVECLEN (newpat, 0) == 2
	   && GET_CODE (XVECEXP (newpat, 0, 0)) == SET
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 0))) != ZERO_EXTRACT
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 0))) != STRICT_LOW_PART
	   && GET_CODE (XVECEXP (newpat, 0, 1)) == SET
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != ZERO_EXTRACT
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != STRICT_LOW_PART
	   && ! use_crosses_set_p (SET_SRC (XVECEXP (newpat, 0, 1)),
				   INSN_CUID (i2))
	   /* Don't pass sets with (USE (MEM ...)) dests to the following.  */
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 1))) != USE
	   && GET_CODE (SET_DEST (XVECEXP (newpat, 0, 0))) != USE
	   && ! reg_referenced_p (SET_DEST (XVECEXP (newpat, 0, 1)),
				  XVECEXP (newpat, 0, 0))
	   && ! reg_referenced_p (SET_DEST (XVECEXP (newpat, 0, 0)),
				  XVECEXP (newpat, 0, 1)))
    {
      newi2pat = XVECEXP (newpat, 0, 1);
      newpat = XVECEXP (newpat, 0, 0);

      i2_code_number = recog_for_combine (&newi2pat, i2, &new_i2_notes);
      if (i2_code_number >= 0)
	insn_code_number = recog_for_combine (&newpat, i3, &new_i3_notes);
    }

  /* If it still isn't recognized, fail and change things back the way they
     were.  */
  if ((insn_code_number < 0
       /* Is the result a reasonable ASM_OPERANDS?  */
       && (! check_asm_operands (newpat) || added_sets_1 || added_sets_2)))
    {
      undo_all ();
      return 0;
    }

  /* If we had to change another insn, make sure it is valid also.  */
  if (undobuf.other_insn)
    {
      rtx other_notes = REG_NOTES (undobuf.other_insn);
      rtx other_pat = PATTERN (undobuf.other_insn);
      rtx new_other_notes;
      rtx note, next;

      other_code_number = recog_for_combine (&other_pat, undobuf.other_insn,
					     &new_other_notes);

      if (other_code_number < 0 && ! check_asm_operands (other_pat))
	{
	  undo_all ();
	  return 0;
	}

      PATTERN (undobuf.other_insn) = other_pat;

      /* If any of the notes in OTHER_INSN were REG_UNUSED, ensure that they
	 are still valid.  Then add any non-duplicate notes added by
	 recog_for_combine.  */
      for (note = REG_NOTES (undobuf.other_insn); note; note = next)
	{
	  next = XEXP (note, 1);

	  if (REG_NOTE_KIND (note) == REG_UNUSED
	      && ! reg_set_p (XEXP (note, 0), PATTERN (undobuf.other_insn)))
	    {
	      if (GET_CODE (XEXP (note, 0)) == REG)
		reg_n_deaths[REGNO (XEXP (note, 0))]--;

	      remove_note (undobuf.other_insn, note);
	    }
	}

      for (note = new_other_notes; note; note = XEXP (note, 1))
	if (GET_CODE (XEXP (note, 0)) == REG)
	  reg_n_deaths[REGNO (XEXP (note, 0))]++;

      distribute_notes (new_other_notes, undobuf.other_insn,
			undobuf.other_insn, NULL_RTX, NULL_RTX, NULL_RTX);
    }

  /* We now know that we can do this combination.  Merge the insns and 
     update the status of registers and LOG_LINKS.  */

  {
    rtx i3notes, i2notes, i1notes = 0;
    rtx i3links, i2links, i1links = 0;
    rtx midnotes = 0;
    int all_adjacent = (next_real_insn (i2) == i3
			&& (i1 == 0 || next_real_insn (i1) == i2));
    register int regno;
    /* Compute which registers we expect to eliminate.  */
    rtx elim_i2 = (newi2pat || i2dest_in_i2src || i2dest_in_i1src
		   ? 0 : i2dest);
    rtx elim_i1 = i1 == 0 || i1dest_in_i1src ? 0 : i1dest;

    /* Get the old REG_NOTES and LOG_LINKS from all our insns and
       clear them.  */
    i3notes = REG_NOTES (i3), i3links = LOG_LINKS (i3);
    i2notes = REG_NOTES (i2), i2links = LOG_LINKS (i2);
    if (i1)
      i1notes = REG_NOTES (i1), i1links = LOG_LINKS (i1);

    /* Ensure that we do not have something that should not be shared but
       occurs multiple times in the new insns.  Check this by first
       resetting all the `used' flags and then copying anything is shared.  */

    reset_used_flags (i3notes);
    reset_used_flags (i2notes);
    reset_used_flags (i1notes);
    reset_used_flags (newpat);
    reset_used_flags (newi2pat);
    if (undobuf.other_insn)
      reset_used_flags (PATTERN (undobuf.other_insn));

    i3notes = copy_rtx_if_shared (i3notes);
    i2notes = copy_rtx_if_shared (i2notes);
    i1notes = copy_rtx_if_shared (i1notes);
    newpat = copy_rtx_if_shared (newpat);
    newi2pat = copy_rtx_if_shared (newi2pat);
    if (undobuf.other_insn)
      reset_used_flags (PATTERN (undobuf.other_insn));

    INSN_CODE (i3) = insn_code_number;
    PATTERN (i3) = newpat;
    if (undobuf.other_insn)
      INSN_CODE (undobuf.other_insn) = other_code_number;

    /* We had one special case above where I2 had more than one set and
       we replaced a destination of one of those sets with the destination
       of I3.  In that case, we have to update LOG_LINKS of insns later
       in this basic block.  Note that this (expensive) case is rare.  */

    if (GET_CODE (PATTERN (i2)) == PARALLEL)
      for (i = 0; i < XVECLEN (PATTERN (i2), 0); i++)
	if (GET_CODE (SET_DEST (XVECEXP (PATTERN (i2), 0, i))) == REG
	    && SET_DEST (XVECEXP (PATTERN (i2), 0, i)) != i2dest
	    && ! find_reg_note (i2, REG_UNUSED,
				SET_DEST (XVECEXP (PATTERN (i2), 0, i))))
	  {
	    register rtx insn;

	    for (insn = NEXT_INSN (i2); insn; insn = NEXT_INSN (insn))
	      {
		if (insn != i3 && GET_RTX_CLASS (GET_CODE (insn)) == 'i')
		  for (link = LOG_LINKS (insn); link; link = XEXP (link, 1))
		    if (XEXP (link, 0) == i2)
		      XEXP (link, 0) = i3;

		if (GET_CODE (insn) == CODE_LABEL
		    || GET_CODE (insn) == JUMP_INSN)
		  break;
	      }
	  }

    LOG_LINKS (i3) = 0;
    REG_NOTES (i3) = 0;
    LOG_LINKS (i2) = 0;
    REG_NOTES (i2) = 0;

    if (newi2pat)
      {
	INSN_CODE (i2) = i2_code_number;
	PATTERN (i2) = newi2pat;
      }
    else
      {
	PUT_CODE (i2, NOTE);
	NOTE_LINE_NUMBER (i2) = NOTE_INSN_DELETED;
	NOTE_SOURCE_FILE (i2) = 0;
      }

    if (i1)
      {
	LOG_LINKS (i1) = 0;
	REG_NOTES (i1) = 0;
	PUT_CODE (i1, NOTE);
	NOTE_LINE_NUMBER (i1) = NOTE_INSN_DELETED;
	NOTE_SOURCE_FILE (i1) = 0;
      }

    /* Get death notes for everything that is now used in either I3 or
       I2 and used to die in a previous insn.  */

    move_deaths (newpat, i1 ? INSN_CUID (i1) : INSN_CUID (i2), i3, &midnotes);
    if (newi2pat)
      move_deaths (newi2pat, INSN_CUID (i1), i2, &midnotes);

    /* Distribute all the LOG_LINKS and REG_NOTES from I1, I2, and I3.  */
    if (i3notes)
      distribute_notes (i3notes, i3, i3, newi2pat ? i2 : NULL_RTX,
			elim_i2, elim_i1);
    if (i2notes)
      distribute_notes (i2notes, i2, i3, newi2pat ? i2 : NULL_RTX,
			elim_i2, elim_i1);
    if (i1notes)
      distribute_notes (i1notes, i1, i3, newi2pat ? i2 : NULL_RTX,
			elim_i2, elim_i1);
    if (midnotes)
      distribute_notes (midnotes, NULL_RTX, i3, newi2pat ? i2 : NULL_RTX,
			elim_i2, elim_i1);

    /* Distribute any notes added to I2 or I3 by recog_for_combine.  We
       know these are REG_UNUSED and want them to go to the desired insn,
       so we always pass it as i3.  We have not counted the notes in 
       reg_n_deaths yet, so we need to do so now.  */

    if (newi2pat && new_i2_notes)
      {
	for (temp = new_i2_notes; temp; temp = XEXP (temp, 1))
	  if (GET_CODE (XEXP (temp, 0)) == REG)
	    reg_n_deaths[REGNO (XEXP (temp, 0))]++;
	
	distribute_notes (new_i2_notes, i2, i2, NULL_RTX, NULL_RTX, NULL_RTX);
      }

    if (new_i3_notes)
      {
	for (temp = new_i3_notes; temp; temp = XEXP (temp, 1))
	  if (GET_CODE (XEXP (temp, 0)) == REG)
	    reg_n_deaths[REGNO (XEXP (temp, 0))]++;
	
	distribute_notes (new_i3_notes, i3, i3, NULL_RTX, NULL_RTX, NULL_RTX);
      }

    /* If I3DEST was used in I3SRC, it really died in I3.  We may need to
       put a REG_DEAD note for it somewhere.  Similarly for I2 and I1.
       Show an additional death due to the REG_DEAD note we make here.  If
       we discard it in distribute_notes, we will decrement it again.  */

    if (i3dest_killed)
      {
	if (GET_CODE (i3dest_killed) == REG)
	  reg_n_deaths[REGNO (i3dest_killed)]++;

	distribute_notes (gen_rtx (EXPR_LIST, REG_DEAD, i3dest_killed,
				   NULL_RTX),
			  NULL_RTX, i3, newi2pat ? i2 : NULL_RTX,
			  NULL_RTX, NULL_RTX);
      }

    /* For I2 and I1, we have to be careful.  If NEWI2PAT exists and sets
       I2DEST or I1DEST, the death must be somewhere before I2, not I3.  If
       we passed I3 in that case, it might delete I2.  */

    if (i2dest_in_i2src)
      {
	if (GET_CODE (i2dest) == REG)
	  reg_n_deaths[REGNO (i2dest)]++;

	if (newi2pat && reg_set_p (i2dest, newi2pat))
	  distribute_notes (gen_rtx (EXPR_LIST, REG_DEAD, i2dest, NULL_RTX),
			    NULL_RTX, i2, NULL_RTX, NULL_RTX, NULL_RTX);
	else
	  distribute_notes (gen_rtx (EXPR_LIST, REG_DEAD, i2dest, NULL_RTX),
			    NULL_RTX, i3, newi2pat ? i2 : NULL_RTX,
			    NULL_RTX, NULL_RTX);
      }

    if (i1dest_in_i1src)
      {
	if (GET_CODE (i1dest) == REG)
	  reg_n_deaths[REGNO (i1dest)]++;

	if (newi2pat && reg_set_p (i1dest, newi2pat))
	  distribute_notes (gen_rtx (EXPR_LIST, REG_DEAD, i1dest, NULL_RTX),
			    NULL_RTX, i2, NULL_RTX, NULL_RTX, NULL_RTX);
	else
	  distribute_notes (gen_rtx (EXPR_LIST, REG_DEAD, i1dest, NULL_RTX),
			    NULL_RTX, i3, newi2pat ? i2 : NULL_RTX,
			    NULL_RTX, NULL_RTX);
      }

    distribute_links (i3links);
    distribute_links (i2links);
    distribute_links (i1links);

    if (GET_CODE (i2dest) == REG)
      {
	rtx link;
	rtx i2_insn = 0, i2_val = 0, set;

	/* The insn that used to set this register doesn't exist, and
	   this life of the register may not exist either.  See if one of
	   I3's links points to an insn that sets I2DEST.  If it does, 
	   that is now the last known value for I2DEST. If we don't update
	   this and I2 set the register to a value that depended on its old
	   contents, we will get confused.  If this insn is used, thing
	   will be set correctly in combine_instructions.  */

	for (link = LOG_LINKS (i3); link; link = XEXP (link, 1))
	  if ((set = single_set (XEXP (link, 0))) != 0
	      && rtx_equal_p (i2dest, SET_DEST (set)))
	    i2_insn = XEXP (link, 0), i2_val = SET_SRC (set);

	record_value_for_reg (i2dest, i2_insn, i2_val);

	/* If the reg formerly set in I2 died only once and that was in I3,
	   zero its use count so it won't make `reload' do any work.  */
	if (! added_sets_2 && newi2pat == 0)
	  {
	    regno = REGNO (i2dest);
	    reg_n_sets[regno]--;
	    if (reg_n_sets[regno] == 0
		&& ! (basic_block_live_at_start[0][regno / REGSET_ELT_BITS]
		      & ((REGSET_ELT_TYPE) 1 << (regno % REGSET_ELT_BITS))))
	      reg_n_refs[regno] = 0;
	  }
      }

    if (i1 && GET_CODE (i1dest) == REG)
      {
	rtx link;
	rtx i1_insn = 0, i1_val = 0, set;

	for (link = LOG_LINKS (i3); link; link = XEXP (link, 1))
	  if ((set = single_set (XEXP (link, 0))) != 0
	      && rtx_equal_p (i1dest, SET_DEST (set)))
	    i1_insn = XEXP (link, 0), i1_val = SET_SRC (set);

	record_value_for_reg (i1dest, i1_insn, i1_val);

	regno = REGNO (i1dest);
	if (! added_sets_1)
	  {
	    reg_n_sets[regno]--;
	    if (reg_n_sets[regno] == 0
		&& ! (basic_block_live_at_start[0][regno / REGSET_ELT_BITS]
		      & ((REGSET_ELT_TYPE) 1 << (regno % REGSET_ELT_BITS))))
	      reg_n_refs[regno] = 0;
	  }
      }

    /* Update reg_significant et al for any changes that may have been made
       to this insn.  */

    note_stores (newpat, set_significant);
    if (newi2pat)
      note_stores (newi2pat, set_significant);

    /* If I3 is now an unconditional jump, ensure that it has a 
       BARRIER following it since it may have initially been a
       conditional jump.  It may also be the last nonnote insn.  */

    if ((GET_CODE (newpat) == RETURN || simplejump_p (i3))
	&& ((temp = next_nonnote_insn (i3)) == NULL_RTX
	    || GET_CODE (temp) != BARRIER))
      emit_barrier_after (i3);
  }

  combine_successes++;

  return newi2pat ? i2 : i3;
}

/* Undo all the modifications recorded in undobuf.  */

static void
undo_all ()
{
  register int i;
  if (undobuf.num_undo > MAX_UNDO)
    undobuf.num_undo = MAX_UNDO;
  for (i = undobuf.num_undo - 1; i >= 0; i--)
    {
      if (undobuf.undo[i].is_int)
	*undobuf.undo[i].where.i = undobuf.undo[i].old_contents.i;
      else
	*undobuf.undo[i].where.rtx = undobuf.undo[i].old_contents.rtx;
      
    }

  obfree (undobuf.storage);
  undobuf.num_undo = 0;
}

/* Find the innermost point within the rtx at LOC, possibly LOC itself,
   where we have an arithmetic expression and return that point.  LOC will
   be inside INSN.

   try_combine will call this function to see if an insn can be split into
   two insns.  */

static rtx *
find_split_point (loc, insn)
     rtx *loc;
     rtx insn;
{
  rtx x = *loc;
  enum rtx_code code = GET_CODE (x);
  rtx *split;
  int len = 0, pos, unsignedp;
  rtx inner;

  /* First special-case some codes.  */
  switch (code)
    {
    case SUBREG:
#ifdef INSN_SCHEDULING
      /* If we are making a paradoxical SUBREG invalid, it becomes a split
	 point.  */
      if (GET_CODE (SUBREG_REG (x)) == MEM)
	return loc;
#endif
      return find_split_point (&SUBREG_REG (x), insn);

    case MEM:
#ifdef HAVE_lo_sum
      /* If we have (mem (const ..)) or (mem (symbol_ref ...)), split it
	 using LO_SUM and HIGH.  */
      if (GET_CODE (XEXP (x, 0)) == CONST
	  || GET_CODE (XEXP (x, 0)) == SYMBOL_REF)
	{
	  SUBST (XEXP (x, 0),
		 gen_rtx_combine (LO_SUM, Pmode,
				  gen_rtx_combine (HIGH, Pmode, XEXP (x, 0)),
				  XEXP (x, 0)));
	  return &XEXP (XEXP (x, 0), 0);
	}
#endif

      /* If we have a PLUS whose second operand is a constant and the
	 address is not valid, perhaps will can split it up using
	 the machine-specific way to split large constants.  We use
	 the first psuedo-reg (one of the virtual regs) as a placeholder;
	 it will not remain in the result.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && ! memory_address_p (GET_MODE (x), XEXP (x, 0)))
	{
	  rtx reg = regno_reg_rtx[FIRST_PSEUDO_REGISTER];
	  rtx seq = split_insns (gen_rtx (SET, VOIDmode, reg, XEXP (x, 0)),
				 subst_insn);

	  /* This should have produced two insns, each of which sets our
	     placeholder.  If the source of the second is a valid address,
	     we can make put both sources together and make a split point
	     in the middle.  */

	  if (seq && XVECLEN (seq, 0) == 2
	      && GET_CODE (XVECEXP (seq, 0, 0)) == INSN
	      && GET_CODE (PATTERN (XVECEXP (seq, 0, 0))) == SET
	      && SET_DEST (PATTERN (XVECEXP (seq, 0, 0))) == reg
	      && ! reg_mentioned_p (reg,
				    SET_SRC (PATTERN (XVECEXP (seq, 0, 0))))
	      && GET_CODE (XVECEXP (seq, 0, 1)) == INSN
	      && GET_CODE (PATTERN (XVECEXP (seq, 0, 1))) == SET
	      && SET_DEST (PATTERN (XVECEXP (seq, 0, 1))) == reg
	      && memory_address_p (GET_MODE (x),
				   SET_SRC (PATTERN (XVECEXP (seq, 0, 1)))))
	    {
	      rtx src1 = SET_SRC (PATTERN (XVECEXP (seq, 0, 0)));
	      rtx src2 = SET_SRC (PATTERN (XVECEXP (seq, 0, 1)));

	      /* Replace the placeholder in SRC2 with SRC1.  If we can
		 find where in SRC2 it was placed, that can become our
		 split point and we can replace this address with SRC2.
		 Just try two obvious places.  */

	      src2 = replace_rtx (src2, reg, src1);
	      split = 0;
	      if (XEXP (src2, 0) == src1)
		split = &XEXP (src2, 0);
	      else if (GET_RTX_FORMAT (GET_CODE (XEXP (src2, 0)))[0] == 'e'
		       && XEXP (XEXP (src2, 0), 0) == src1)
		split = &XEXP (XEXP (src2, 0), 0);

	      if (split)
		{
		  SUBST (XEXP (x, 0), src2);
		  return split;
		}
	    }
	  
	  /* If that didn't work, perhaps the first operand is complex and
	     needs to be computed separately, so make a split point there.
	     This will occur on machines that just support REG + CONST
	     and have a constant moved through some previous computation.  */

	  else if (GET_RTX_CLASS (GET_CODE (XEXP (XEXP (x, 0), 0))) != 'o'
		   && ! (GET_CODE (XEXP (XEXP (x, 0), 0)) == SUBREG
			 && (GET_RTX_CLASS (GET_CODE (SUBREG_REG (XEXP (XEXP (x, 0), 0))))
			     == 'o')))
	    return &XEXP (XEXP (x, 0), 0);
	}
      break;

    case SET:
#ifdef HAVE_cc0
      /* If SET_DEST is CC0 and SET_SRC is not an operand, a COMPARE, or a
	 ZERO_EXTRACT, the most likely reason why this doesn't match is that
	 we need to put the operand into a register.  So split at that
	 point.  */

      if (SET_DEST (x) == cc0_rtx
	  && GET_CODE (SET_SRC (x)) != COMPARE
	  && GET_CODE (SET_SRC (x)) != ZERO_EXTRACT
	  && GET_RTX_CLASS (GET_CODE (SET_SRC (x))) != 'o'
	  && ! (GET_CODE (SET_SRC (x)) == SUBREG
		&& GET_RTX_CLASS (GET_CODE (SUBREG_REG (SET_SRC (x)))) == 'o'))
	return &SET_SRC (x);
#endif

      /* See if we can split SET_SRC as it stands.  */
      split = find_split_point (&SET_SRC (x), insn);
      if (split && split != &SET_SRC (x))
	return split;

      /* See if this is a bitfield assignment with everything constant.  If
	 so, this is an IOR of an AND, so split it into that.  */
      if (GET_CODE (SET_DEST (x)) == ZERO_EXTRACT
	  && (GET_MODE_BITSIZE (GET_MODE (XEXP (SET_DEST (x), 0)))
	      <= HOST_BITS_PER_WIDE_INT)
	  && GET_CODE (XEXP (SET_DEST (x), 1)) == CONST_INT
	  && GET_CODE (XEXP (SET_DEST (x), 2)) == CONST_INT
	  && GET_CODE (SET_SRC (x)) == CONST_INT
	  && ((INTVAL (XEXP (SET_DEST (x), 1))
	      + INTVAL (XEXP (SET_DEST (x), 2)))
	      <= GET_MODE_BITSIZE (GET_MODE (XEXP (SET_DEST (x), 0))))
	  && ! side_effects_p (XEXP (SET_DEST (x), 0)))
	{
	  int pos = INTVAL (XEXP (SET_DEST (x), 2));
	  int len = INTVAL (XEXP (SET_DEST (x), 1));
	  int src = INTVAL (SET_SRC (x));
	  rtx dest = XEXP (SET_DEST (x), 0);
	  enum machine_mode mode = GET_MODE (dest);
	  unsigned HOST_WIDE_INT mask = ((HOST_WIDE_INT) 1 << len) - 1;

#if BITS_BIG_ENDIAN
	  pos = GET_MODE_BITSIZE (mode) - len - pos;
#endif

	  if (src == mask)
	    SUBST (SET_SRC (x),
		   gen_binary (IOR, mode, dest, GEN_INT (src << pos)));
	  else
	    SUBST (SET_SRC (x),
		   gen_binary (IOR, mode,
			       gen_binary (AND, mode, dest, 
					   GEN_INT (~ (mask << pos)
						    & GET_MODE_MASK (mode))),
			       GEN_INT (src << pos)));

	  SUBST (SET_DEST (x), dest);

	  split = find_split_point (&SET_SRC (x), insn);
	  if (split && split != &SET_SRC (x))
	    return split;
	}

      /* Otherwise, see if this is an operation that we can split into two.
	 If so, try to split that.  */
      code = GET_CODE (SET_SRC (x));

      switch (code)
	{
	case AND:
	  /* If we are AND'ing with a large constant that is only a single
	     bit and the result is only being used in a context where we
	     need to know if it is zero or non-zero, replace it with a bit
	     extraction.  This will avoid the large constant, which might
	     have taken more than one insn to make.  If the constant were
	     not a valid argument to the AND but took only one insn to make,
	     this is no worse, but if it took more than one insn, it will
	     be better.  */

	  if (GET_CODE (XEXP (SET_SRC (x), 1)) == CONST_INT
	      && GET_CODE (XEXP (SET_SRC (x), 0)) == REG
	      && (pos = exact_log2 (INTVAL (XEXP (SET_SRC (x), 1)))) >= 7
	      && GET_CODE (SET_DEST (x)) == REG
	      && (split = find_single_use (SET_DEST (x), insn, NULL_PTR)) != 0
	      && (GET_CODE (*split) == EQ || GET_CODE (*split) == NE)
	      && XEXP (*split, 0) == SET_DEST (x)
	      && XEXP (*split, 1) == const0_rtx)
	    {
	      SUBST (SET_SRC (x),
		     make_extraction (GET_MODE (SET_DEST (x)),
				      XEXP (SET_SRC (x), 0),
				      pos, NULL_RTX, 1, 1, 0, 0));
	      return find_split_point (loc, insn);
	    }
	  break;

	case SIGN_EXTEND:
	  inner = XEXP (SET_SRC (x), 0);
	  pos = 0;
	  len = GET_MODE_BITSIZE (GET_MODE (inner));
	  unsignedp = 0;
	  break;

	case SIGN_EXTRACT:
	case ZERO_EXTRACT:
	  if (GET_CODE (XEXP (SET_SRC (x), 1)) == CONST_INT
	      && GET_CODE (XEXP (SET_SRC (x), 2)) == CONST_INT)
	    {
	      inner = XEXP (SET_SRC (x), 0);
	      len = INTVAL (XEXP (SET_SRC (x), 1));
	      pos = INTVAL (XEXP (SET_SRC (x), 2));

#if BITS_BIG_ENDIAN
	      pos = GET_MODE_BITSIZE (GET_MODE (inner)) - len - pos;
#endif
	      unsignedp = (code == ZERO_EXTRACT);
	    }
	  break;
	}

      if (len && pos >= 0 && pos + len <= GET_MODE_BITSIZE (GET_MODE (inner)))
	{
	  enum machine_mode mode = GET_MODE (SET_SRC (x));

	  /* For unsigned, we have a choice of a shift followed by an
	     AND or two shifts.  Use two shifts for field sizes where the
	     constant might be too large.  We assume here that we can
	     always at least get 8-bit constants in an AND insn, which is
	     true for every current RISC.  */

	  if (unsignedp && len <= 8)
	    {
	      SUBST (SET_SRC (x),
		     gen_rtx_combine
		     (AND, mode,
		      gen_rtx_combine (LSHIFTRT, mode,
				       gen_lowpart_for_combine (mode, inner),
				       GEN_INT (pos)),
		      GEN_INT (((HOST_WIDE_INT) 1 << len) - 1)));

	      split = find_split_point (&SET_SRC (x), insn);
	      if (split && split != &SET_SRC (x))
		return split;
	    }
	  else
	    {
	      SUBST (SET_SRC (x),
		     gen_rtx_combine
		     (unsignedp ? LSHIFTRT : ASHIFTRT, mode,
		      gen_rtx_combine (ASHIFT, mode,
				       gen_lowpart_for_combine (mode, inner),
				       GEN_INT (GET_MODE_BITSIZE (mode)
						- len - pos)),
		      GEN_INT (GET_MODE_BITSIZE (mode) - len)));

	      split = find_split_point (&SET_SRC (x), insn);
	      if (split && split != &SET_SRC (x))
		return split;
	    }
	}

      /* See if this is a simple operation with a constant as the second
	 operand.  It might be that this constant is out of range and hence
	 could be used as a split point.  */
      if ((GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == '2'
	   || GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == 'c'
	   || GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == '<')
	  && CONSTANT_P (XEXP (SET_SRC (x), 1))
	  && (GET_RTX_CLASS (GET_CODE (XEXP (SET_SRC (x), 0))) == 'o'
	      || (GET_CODE (XEXP (SET_SRC (x), 0)) == SUBREG
		  && (GET_RTX_CLASS (GET_CODE (SUBREG_REG (XEXP (SET_SRC (x), 0))))
		      == 'o'))))
	return &XEXP (SET_SRC (x), 1);

      /* Finally, see if this is a simple operation with its first operand
	 not in a register.  The operation might require this operand in a
	 register, so return it as a split point.  We can always do this
	 because if the first operand were another operation, we would have
	 already found it as a split point.  */
      if ((GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == '2'
	   || GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == 'c'
	   || GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == '<'
	   || GET_RTX_CLASS (GET_CODE (SET_SRC (x))) == '1')
	  && ! register_operand (XEXP (SET_SRC (x), 0), VOIDmode))
	return &XEXP (SET_SRC (x), 0);

      return 0;

    case AND:
    case IOR:
      /* We write NOR as (and (not A) (not B)), but if we don't have a NOR,
	 it is better to write this as (not (ior A B)) so we can split it.
	 Similarly for IOR.  */
      if (GET_CODE (XEXP (x, 0)) == NOT && GET_CODE (XEXP (x, 1)) == NOT)
	{
	  SUBST (*loc,
		 gen_rtx_combine (NOT, GET_MODE (x),
				  gen_rtx_combine (code == IOR ? AND : IOR,
						   GET_MODE (x),
						   XEXP (XEXP (x, 0), 0),
						   XEXP (XEXP (x, 1), 0))));
	  return find_split_point (loc, insn);
	}

      /* Many RISC machines have a large set of logical insns.  If the
	 second operand is a NOT, put it first so we will try to split the
	 other operand first.  */
      if (GET_CODE (XEXP (x, 1)) == NOT)
	{
	  rtx tem = XEXP (x, 0);
	  SUBST (XEXP (x, 0), XEXP (x, 1));
	  SUBST (XEXP (x, 1), tem);
	}
      break;
    }

  /* Otherwise, select our actions depending on our rtx class.  */
  switch (GET_RTX_CLASS (code))
    {
    case 'b':			/* This is ZERO_EXTRACT and SIGN_EXTRACT.  */
    case '3':
      split = find_split_point (&XEXP (x, 2), insn);
      if (split)
	return split;
      /* ... fall through ... */
    case '2':
    case 'c':
    case '<':
      split = find_split_point (&XEXP (x, 1), insn);
      if (split)
	return split;
      /* ... fall through ... */
    case '1':
      /* Some machines have (and (shift ...) ...) insns.  If X is not
	 an AND, but XEXP (X, 0) is, use it as our split point.  */
      if (GET_CODE (x) != AND && GET_CODE (XEXP (x, 0)) == AND)
	return &XEXP (x, 0);

      split = find_split_point (&XEXP (x, 0), insn);
      if (split)
	return split;
      return loc;
    }

  /* Otherwise, we don't have a split point.  */
  return 0;
}

/* Throughout X, replace FROM with TO, and return the result.
   The result is TO if X is FROM;
   otherwise the result is X, but its contents may have been modified.
   If they were modified, a record was made in undobuf so that
   undo_all will (among other things) return X to its original state.

   If the number of changes necessary is too much to record to undo,
   the excess changes are not made, so the result is invalid.
   The changes already made can still be undone.
   undobuf.num_undo is incremented for such changes, so by testing that
   the caller can tell whether the result is valid.

   `n_occurrences' is incremented each time FROM is replaced.
   
   IN_DEST is non-zero if we are processing the SET_DEST of a SET.

   UNIQUE_COPY is non-zero if each substitution must be unique.  We do this
   by copying if `n_occurrences' is non-zero.  */

static rtx
subst (x, from, to, in_dest, unique_copy)
     register rtx x, from, to;
     int in_dest;
     int unique_copy;
{
  register char *fmt;
  register int len, i;
  register enum rtx_code code = GET_CODE (x), orig_code = code;
  rtx temp;
  enum machine_mode mode = GET_MODE (x);
  enum machine_mode op0_mode = VOIDmode;
  rtx other_insn;
  rtx *cc_use;
  int n_restarts = 0;

/* FAKE_EXTEND_SAFE_P (MODE, FROM) is 1 if (subreg:MODE FROM 0) is a safe
   replacement for (zero_extend:MODE FROM) or (sign_extend:MODE FROM).
   If it is 0, that cannot be done.  We can now do this for any MEM
   because (SUBREG (MEM...)) is guaranteed to cause the MEM to be reloaded.
   If not for that, MEM's would very rarely be safe.  */

/* Reject MODEs bigger than a word, because we might not be able
   to reference a two-register group starting with an arbitrary register
   (and currently gen_lowpart might crash for a SUBREG).  */

#define FAKE_EXTEND_SAFE_P(MODE, FROM) \
  (GET_MODE_SIZE (MODE) <= UNITS_PER_WORD)

/* Two expressions are equal if they are identical copies of a shared
   RTX or if they are both registers with the same register number
   and mode.  */

#define COMBINE_RTX_EQUAL_P(X,Y)			\
  ((X) == (Y)						\
   || (GET_CODE (X) == REG && GET_CODE (Y) == REG	\
       && REGNO (X) == REGNO (Y) && GET_MODE (X) == GET_MODE (Y)))

  if (! in_dest && COMBINE_RTX_EQUAL_P (x, from))
    {
      n_occurrences++;
      return (unique_copy && n_occurrences > 1 ? copy_rtx (to) : to);
    }

  /* If X and FROM are the same register but different modes, they will
     not have been seen as equal above.  However, flow.c will make a 
     LOG_LINKS entry for that case.  If we do nothing, we will try to
     rerecognize our original insn and, when it succeeds, we will
     delete the feeding insn, which is incorrect.

     So force this insn not to match in this (rare) case.  */
  if (! in_dest && code == REG && GET_CODE (from) == REG
      && REGNO (x) == REGNO (from))
    return gen_rtx (CLOBBER, GET_MODE (x), const0_rtx);

  /* If this is an object, we are done unless it is a MEM or LO_SUM, both
     of which may contain things that can be combined.  */
  if (code != MEM && code != LO_SUM && GET_RTX_CLASS (code) == 'o')
    return x;

  /* It is possible to have a subexpression appear twice in the insn.
     Suppose that FROM is a register that appears within TO.
     Then, after that subexpression has been scanned once by `subst',
     the second time it is scanned, TO may be found.  If we were
     to scan TO here, we would find FROM within it and create a
     self-referent rtl structure which is completely wrong.  */
  if (COMBINE_RTX_EQUAL_P (x, to))
    return to;

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  /* We don't need to process a SET_DEST that is a register, CC0, or PC, so
     set up to skip this common case.  All other cases where we want to
     suppress replacing something inside a SET_SRC are handled via the
     IN_DEST operand.  */
  if (code == SET
      && (GET_CODE (SET_DEST (x)) == REG
        || GET_CODE (SET_DEST (x)) == CC0
        || GET_CODE (SET_DEST (x)) == PC))
    fmt = "ie";

  /* Get the mode of operand 0 in case X is now a SIGN_EXTEND of a constant. */
  if (fmt[0] == 'e')
    op0_mode = GET_MODE (XEXP (x, 0));

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      register rtx new;
	      if (COMBINE_RTX_EQUAL_P (XVECEXP (x, i, j), from))
		{
		  new = (unique_copy && n_occurrences ? copy_rtx (to) : to);
		  n_occurrences++;
		}
	      else
		{
		  new = subst (XVECEXP (x, i, j), from, to, 0, unique_copy);

		  /* If this substitution failed, this whole thing fails.  */
		  if (GET_CODE (new) == CLOBBER && XEXP (new, 0) == const0_rtx)
		    return new;
		}

	      SUBST (XVECEXP (x, i, j), new);
	    }
	}
      else if (fmt[i] == 'e')
	{
	  register rtx new;

	  if (COMBINE_RTX_EQUAL_P (XEXP (x, i), from))
	    {
	      new = (unique_copy && n_occurrences ? copy_rtx (to) : to);
	      n_occurrences++;
	    }
	  else
	    /* If we are in a SET_DEST, suppress most cases unless we
	       have gone inside a MEM, in which case we want to
	       simplify the address.  We assume here that things that
	       are actually part of the destination have their inner
	       parts in the first expression.  This is true for SUBREG, 
	       STRICT_LOW_PART, and ZERO_EXTRACT, which are the only
	       things aside from REG and MEM that should appear in a
	       SET_DEST.  */
	    new = subst (XEXP (x, i), from, to,
			 (((in_dest
			    && (code == SUBREG || code == STRICT_LOW_PART
				|| code == ZERO_EXTRACT))
			   || code == SET)
			  && i == 0), unique_copy);

	  /* If we found that we will have to reject this combination,
	     indicate that by returning the CLOBBER ourselves, rather than
	     an expression containing it.  This will speed things up as
	     well as prevent accidents where two CLOBBERs are considered
	     to be equal, thus producing an incorrect simplification.  */

	  if (GET_CODE (new) == CLOBBER && XEXP (new, 0) == const0_rtx)
	    return new;

	  SUBST (XEXP (x, i), new);
	}
    }

  /* We come back to here if we have replaced the expression with one of
     a different code and it is likely that further simplification will be
     possible.  */

 restart:

  /* If we have restarted more than 4 times, we are probably looping, so
     give up.  */
  if (++n_restarts > 4)
    return x;

  /* If we are restarting at all, it means that we no longer know the
     original mode of operand 0 (since we have probably changed the
     form of X).  */

  if (n_restarts > 1)
    op0_mode = VOIDmode;

  code = GET_CODE (x);

  /* If this is a commutative operation, put a constant last and a complex
     expression first.  We don't need to do this for comparisons here.  */
  if (GET_RTX_CLASS (code) == 'c'
      && ((CONSTANT_P (XEXP (x, 0)) && GET_CODE (XEXP (x, 1)) != CONST_INT)
	  || (GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == 'o'
	      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) != 'o')
	  || (GET_CODE (XEXP (x, 0)) == SUBREG
	      && GET_RTX_CLASS (GET_CODE (SUBREG_REG (XEXP (x, 0)))) == 'o'
	      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) != 'o')))
    {
      temp = XEXP (x, 0);
      SUBST (XEXP (x, 0), XEXP (x, 1));
      SUBST (XEXP (x, 1), temp);
    }

  /* If this is a PLUS, MINUS, or MULT, and the first operand is the
     sign extension of a PLUS with a constant, reverse the order of the sign
     extension and the addition. Note that this not the same as the original
     code, but overflow is undefined for signed values.  Also note that the
     PLUS will have been partially moved "inside" the sign-extension, so that
     the first operand of X will really look like:
         (ashiftrt (plus (ashift A C4) C5) C4).
     We convert this to
         (plus (ashiftrt (ashift A C4) C2) C4)
     and replace the first operand of X with that expression.  Later parts
     of this function may simplify the expression further.

     For example, if we start with (mult (sign_extend (plus A C1)) C2),
     we swap the SIGN_EXTEND and PLUS.  Later code will apply the
     distributive law to produce (plus (mult (sign_extend X) C1) C3).

     We do this to simplify address expressions.  */

  if ((code == PLUS || code == MINUS || code == MULT)
      && GET_CODE (XEXP (x, 0)) == ASHIFTRT
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == PLUS
      && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ASHIFT
      && GET_CODE (XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 1)) == CONST_INT
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 1) == XEXP (XEXP (x, 0), 1)
      && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
      && (temp = simplify_binary_operation (ASHIFTRT, mode,
					    XEXP (XEXP (XEXP (x, 0), 0), 1),
					    XEXP (XEXP (x, 0), 1))) != 0)
    {
      rtx new
	= simplify_shift_const (NULL_RTX, ASHIFT, mode,
				XEXP (XEXP (XEXP (XEXP (x, 0), 0), 0), 0),
				INTVAL (XEXP (XEXP (x, 0), 1)));

      new = simplify_shift_const (NULL_RTX, ASHIFTRT, mode, new,
				  INTVAL (XEXP (XEXP (x, 0), 1)));

      SUBST (XEXP (x, 0), gen_binary (PLUS, mode, new, temp));
    }

  /* If this is a simple operation applied to an IF_THEN_ELSE, try 
     applying it to the arms of the IF_THEN_ELSE.  This often simplifies
     things.  Don't deal with operations that change modes here.  */

  if ((GET_RTX_CLASS (code) == '2' || GET_RTX_CLASS (code) == 'c')
      && GET_CODE (XEXP (x, 0)) == IF_THEN_ELSE)
    {
      /* Don't do this by using SUBST inside X since we might be messing
	 up a shared expression.  */
      rtx cond = XEXP (XEXP (x, 0), 0);
      rtx t_arm = subst (gen_binary (code, mode, XEXP (XEXP (x, 0), 1),
				     XEXP (x, 1)),
			 pc_rtx, pc_rtx, 0, 0);
      rtx f_arm = subst (gen_binary (code, mode, XEXP (XEXP (x, 0), 2),
				     XEXP (x, 1)),
			 pc_rtx, pc_rtx, 0, 0);


      x = gen_rtx (IF_THEN_ELSE, mode, cond, t_arm, f_arm);
      goto restart;
    }

  else if (GET_RTX_CLASS (code) == '1'
	   && GET_CODE (XEXP (x, 0)) == IF_THEN_ELSE
	   && GET_MODE (XEXP (x, 0)) == mode)
    {
      rtx cond = XEXP (XEXP (x, 0), 0);
      rtx t_arm = subst (gen_unary (code, mode, XEXP (XEXP (x, 0), 1)),
			 pc_rtx, pc_rtx, 0, 0);
      rtx f_arm = subst (gen_unary (code, mode, XEXP (XEXP (x, 0), 2)),
			 pc_rtx, pc_rtx, 0, 0);

      x = gen_rtx_combine (IF_THEN_ELSE, mode, cond, t_arm, f_arm);
      goto restart;
    }

  /* Try to fold this expression in case we have constants that weren't
     present before.  */
  temp = 0;
  switch (GET_RTX_CLASS (code))
    {
    case '1':
      temp = simplify_unary_operation (code, mode, XEXP (x, 0), op0_mode);
      break;
    case '<':
      temp = simplify_relational_operation (code, op0_mode,
					    XEXP (x, 0), XEXP (x, 1));
#ifdef FLOAT_STORE_FLAG_VALUE
      if (temp != 0 && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	temp = ((temp == const0_rtx) ? CONST0_RTX (GET_MODE (x))
		: immed_real_const_1 (FLOAT_STORE_FLAG_VALUE, GET_MODE (x)));
#endif
      break;
    case 'c':
    case '2':
      temp = simplify_binary_operation (code, mode, XEXP (x, 0), XEXP (x, 1));
      break;
    case 'b':
    case '3':
      temp = simplify_ternary_operation (code, mode, op0_mode, XEXP (x, 0),
					 XEXP (x, 1), XEXP (x, 2));
      break;
    }

  if (temp)
    x = temp, code = GET_CODE (temp);

  /* First see if we can apply the inverse distributive law.  */
  if (code == PLUS || code == MINUS || code == IOR || code == XOR)
    {
      x = apply_distributive_law (x);
      code = GET_CODE (x);
    }

  /* If CODE is an associative operation not otherwise handled, see if we
     can associate some operands.  This can win if they are constants or
     if they are logically related (i.e. (a & b) & a.  */
  if ((code == PLUS || code == MINUS
       || code == MULT || code == AND || code == IOR || code == XOR
       || code == DIV || code == UDIV
       || code == SMAX || code == SMIN || code == UMAX || code == UMIN)
      && GET_MODE_CLASS (mode) == MODE_INT)
    {
      if (GET_CODE (XEXP (x, 0)) == code)
	{
	  rtx other = XEXP (XEXP (x, 0), 0);
	  rtx inner_op0 = XEXP (XEXP (x, 0), 1);
	  rtx inner_op1 = XEXP (x, 1);
	  rtx inner;
	  
	  /* Make sure we pass the constant operand if any as the second
	     one if this is a commutative operation.  */
	  if (CONSTANT_P (inner_op0) && GET_RTX_CLASS (code) == 'c')
	    {
	      rtx tem = inner_op0;
	      inner_op0 = inner_op1;
	      inner_op1 = tem;
	    }
	  inner = simplify_binary_operation (code == MINUS ? PLUS
					     : code == DIV ? MULT
					     : code == UDIV ? MULT
					     : code,
					     mode, inner_op0, inner_op1);

	  /* For commutative operations, try the other pair if that one
	     didn't simplify.  */
	  if (inner == 0 && GET_RTX_CLASS (code) == 'c')
	    {
	      other = XEXP (XEXP (x, 0), 1);
	      inner = simplify_binary_operation (code, mode,
						 XEXP (XEXP (x, 0), 0),
						 XEXP (x, 1));
	    }

	  if (inner)
	    {
	      x = gen_binary (code, mode, other, inner);
	      goto restart;
	    
	    }
	}
    }

  /* A little bit of algebraic simplification here.  */
  switch (code)
    {
    case MEM:
      /* Ensure that our address has any ASHIFTs converted to MULT in case
	 address-recognizing predicates are called later.  */
      temp = make_compound_operation (XEXP (x, 0), MEM);
      SUBST (XEXP (x, 0), temp);
      break;

    case SUBREG:
      /* (subreg:A (mem:B X) N) becomes a modified MEM unless the SUBREG
	 is paradoxical.  If we can't do that safely, then it becomes
	 something nonsensical so that this combination won't take place.  */

      if (GET_CODE (SUBREG_REG (x)) == MEM
	  && (GET_MODE_SIZE (mode)
	      <= GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))))
	{
	  rtx inner = SUBREG_REG (x);
	  int endian_offset = 0;
	  /* Don't change the mode of the MEM
	     if that would change the meaning of the address.  */
	  if (MEM_VOLATILE_P (SUBREG_REG (x))
	      || mode_dependent_address_p (XEXP (inner, 0)))
	    return gen_rtx (CLOBBER, mode, const0_rtx);

#if BYTES_BIG_ENDIAN
	  if (GET_MODE_SIZE (mode) < UNITS_PER_WORD)
	    endian_offset += UNITS_PER_WORD - GET_MODE_SIZE (mode);
	  if (GET_MODE_SIZE (GET_MODE (inner)) < UNITS_PER_WORD)
	    endian_offset -= UNITS_PER_WORD - GET_MODE_SIZE (GET_MODE (inner));
#endif
	  /* Note if the plus_constant doesn't make a valid address
	     then this combination won't be accepted.  */
	  x = gen_rtx (MEM, mode,
		       plus_constant (XEXP (inner, 0),
				      (SUBREG_WORD (x) * UNITS_PER_WORD
				       + endian_offset)));
	  MEM_VOLATILE_P (x) = MEM_VOLATILE_P (inner);
	  RTX_UNCHANGING_P (x) = RTX_UNCHANGING_P (inner);
	  MEM_IN_STRUCT_P (x) = MEM_IN_STRUCT_P (inner);
	  return x;
	}

      /* If we are in a SET_DEST, these other cases can't apply.  */
      if (in_dest)
	return x;

      /* Changing mode twice with SUBREG => just change it once,
	 or not at all if changing back to starting mode.  */
      if (GET_CODE (SUBREG_REG (x)) == SUBREG)
	{
	  if (mode == GET_MODE (SUBREG_REG (SUBREG_REG (x)))
	      && SUBREG_WORD (x) == 0 && SUBREG_WORD (SUBREG_REG (x)) == 0)
	    return SUBREG_REG (SUBREG_REG (x));

	  SUBST_INT (SUBREG_WORD (x),
		     SUBREG_WORD (x) + SUBREG_WORD (SUBREG_REG (x)));
	  SUBST (SUBREG_REG (x), SUBREG_REG (SUBREG_REG (x)));
	}

      /* SUBREG of a hard register => just change the register number
	 and/or mode.  If the hard register is not valid in that mode,
	 suppress this combination.  If the hard register is the stack,
	 frame, or argument pointer, leave this as a SUBREG.  */

      if (GET_CODE (SUBREG_REG (x)) == REG
	  && REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER
	  && REGNO (SUBREG_REG (x)) != FRAME_POINTER_REGNUM
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  && REGNO (SUBREG_REG (x)) != ARG_POINTER_REGNUM
#endif
	  && REGNO (SUBREG_REG (x)) != STACK_POINTER_REGNUM)
	{
	  if (HARD_REGNO_MODE_OK (REGNO (SUBREG_REG (x)) + SUBREG_WORD (x),
				  mode))
	    return gen_rtx (REG, mode,
			    REGNO (SUBREG_REG (x)) + SUBREG_WORD (x));
	  else
	    return gen_rtx (CLOBBER, mode, const0_rtx);
	}

      /* For a constant, try to pick up the part we want.  Handle a full
	 word and low-order part.  Only do this if we are narrowing
	 the constant; if it is being widened, we have no idea what
	 the extra bits will have been set to.  */

      if (CONSTANT_P (SUBREG_REG (x)) && op0_mode != VOIDmode
	  && GET_MODE_SIZE (mode) == UNITS_PER_WORD
	  && GET_MODE_SIZE (op0_mode) < UNITS_PER_WORD
	  && GET_MODE_CLASS (mode) == MODE_INT)
	{
	  temp = operand_subword (SUBREG_REG (x), SUBREG_WORD (x),
				  0, op0_mode);
	  if (temp)
	    return temp;
	}
	
      if (CONSTANT_P (SUBREG_REG (x)) && subreg_lowpart_p (x)
	  && GET_MODE_SIZE (mode) < GET_MODE_SIZE (op0_mode))
	return gen_lowpart_for_combine (mode, SUBREG_REG (x));

      /* If we are narrowing the object, we need to see if we can simplify
	 the expression for the object knowing that we only need the
	 low-order bits.  */

      if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))
	  && subreg_lowpart_p (x))
	return force_to_mode (SUBREG_REG (x), mode, GET_MODE_BITSIZE (mode),
			      NULL_RTX);
      break;

    case NOT:
      /* (not (plus X -1)) can become (neg X).  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && XEXP (XEXP (x, 0), 1) == constm1_rtx)
	{
	  x = gen_rtx_combine (NEG, mode, XEXP (XEXP (x, 0), 0));
	  goto restart;
	}

      /* Similarly, (not (neg X)) is (plus X -1).  */
      if (GET_CODE (XEXP (x, 0)) == NEG)
	{
	  x = gen_rtx_combine (PLUS, mode, XEXP (XEXP (x, 0), 0), constm1_rtx);
	  goto restart;
	}

      /* (not (xor X C)) for C constant is (xor X D) with D = ~ C.  */
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (temp = simplify_unary_operation (NOT, mode,
					       XEXP (XEXP (x, 0), 1),
					       mode)) != 0)
	{
	  SUBST (XEXP (XEXP (x, 0), 1), temp);
	  return XEXP (x, 0);
	}
	      
      /* (not (ashift 1 X)) is (rotate ~1 X).  We used to do this for operands
	 other than 1, but that is not valid.  We could do a similar
	 simplification for (not (lshiftrt C X)) where C is just the sign bit,
	 but this doesn't seem common enough to bother with.  */
      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && XEXP (XEXP (x, 0), 0) == const1_rtx)
	{
	  x = gen_rtx (ROTATE, mode, gen_unary (NOT, mode, const1_rtx),
		       XEXP (XEXP (x, 0), 1));
	  goto restart;
	}
					    
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && subreg_lowpart_p (XEXP (x, 0))
	  && (GET_MODE_SIZE (GET_MODE (XEXP (x, 0)))
	      < GET_MODE_SIZE (GET_MODE (SUBREG_REG (XEXP (x, 0)))))
	  && GET_CODE (SUBREG_REG (XEXP (x, 0))) == ASHIFT
	  && XEXP (SUBREG_REG (XEXP (x, 0)), 0) == const1_rtx)
	{
	  enum machine_mode inner_mode = GET_MODE (SUBREG_REG (XEXP (x, 0)));

	  x = gen_rtx (ROTATE, inner_mode,
		       gen_unary (NOT, inner_mode, const1_rtx),
		       XEXP (SUBREG_REG (XEXP (x, 0)), 1));
	  x = gen_lowpart_for_combine (mode, x);
	  goto restart;
	}
					    
#if STORE_FLAG_VALUE == -1
      /* (not (comparison foo bar)) can be done by reversing the comparison
	 code if valid.  */
      if (GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && reversible_comparison_p (XEXP (x, 0)))
	return gen_rtx_combine (reverse_condition (GET_CODE (XEXP (x, 0))),
				mode, XEXP (XEXP (x, 0), 0),
				XEXP (XEXP (x, 0), 1));
#endif

      /* Apply De Morgan's laws to reduce number of patterns for machines
 	 with negating logical insns (and-not, nand, etc.).  If result has
 	 only one NOT, put it first, since that is how the patterns are
 	 coded.  */

      if (GET_CODE (XEXP (x, 0)) == IOR || GET_CODE (XEXP (x, 0)) == AND)
 	{
 	 rtx in1 = XEXP (XEXP (x, 0), 0), in2 = XEXP (XEXP (x, 0), 1);

	 if (GET_CODE (in1) == NOT)
	   in1 = XEXP (in1, 0);
 	 else
	   in1 = gen_rtx_combine (NOT, GET_MODE (in1), in1);

	 if (GET_CODE (in2) == NOT)
	   in2 = XEXP (in2, 0);
 	 else if (GET_CODE (in2) == CONST_INT
		  && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	   in2 = GEN_INT (GET_MODE_MASK (mode) & ~ INTVAL (in2));
	 else
	   in2 = gen_rtx_combine (NOT, GET_MODE (in2), in2);

	 if (GET_CODE (in2) == NOT)
	   {
	     rtx tem = in2;
	     in2 = in1; in1 = tem;
	   }

	 x = gen_rtx_combine (GET_CODE (XEXP (x, 0)) == IOR ? AND : IOR,
			      mode, in1, in2);
	 goto restart;
       } 
      break;

    case NEG:
      /* (neg (plus X 1)) can become (not X).  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && XEXP (XEXP (x, 0), 1) == const1_rtx)
	{
	  x = gen_rtx_combine (NOT, mode, XEXP (XEXP (x, 0), 0));
	  goto restart;
	}

      /* Similarly, (neg (not X)) is (plus X 1).  */
      if (GET_CODE (XEXP (x, 0)) == NOT)
 	{
	  x = gen_rtx_combine (PLUS, mode, XEXP (XEXP (x, 0), 0), const1_rtx);
	  goto restart;
 	}

      /* (neg (minus X Y)) can become (minus Y X).  */
      if (GET_CODE (XEXP (x, 0)) == MINUS
	  && (GET_MODE_CLASS (mode) != MODE_FLOAT
	      /* x-y != -(y-x) with IEEE floating point. */
	      || TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT))
	{
	  x = gen_binary (MINUS, mode, XEXP (XEXP (x, 0), 1),
			  XEXP (XEXP (x, 0), 0));
	  goto restart;
	}

      /* (neg (xor A 1)) is (plus A -1) if A is known to be either 0 or 1. */
      if (GET_CODE (XEXP (x, 0)) == XOR && XEXP (XEXP (x, 0), 1) == const1_rtx
	  && significant_bits (XEXP (XEXP (x, 0), 0), mode) == 1)
	{
	  x = gen_binary (PLUS, mode, XEXP (XEXP (x, 0), 0), constm1_rtx);
	  goto restart;
	}

      /* NEG commutes with ASHIFT since it is multiplication.  Only do this
	 if we can then eliminate the NEG (e.g.,
	 if the operand is a constant).  */

      if (GET_CODE (XEXP (x, 0)) == ASHIFT)
	{
	  temp = simplify_unary_operation (NEG, mode,
					   XEXP (XEXP (x, 0), 0), mode);
	  if (temp)
	    {
	      SUBST (XEXP (XEXP (x, 0), 0), temp);
	      return XEXP (x, 0);
	    }
	}

      temp = expand_compound_operation (XEXP (x, 0));

      /* For C equal to the width of MODE minus 1, (neg (ashiftrt X C)) can be
 	 replaced by (lshiftrt X C).  This will convert
	 (neg (sign_extract X 1 Y)) to (zero_extract X 1 Y).  */

      if (GET_CODE (temp) == ASHIFTRT
	  && GET_CODE (XEXP (temp, 1)) == CONST_INT
	  && INTVAL (XEXP (temp, 1)) == GET_MODE_BITSIZE (mode) - 1)
	{
	  x = simplify_shift_const (temp, LSHIFTRT, mode, XEXP (temp, 0),
				    INTVAL (XEXP (temp, 1)));
	  goto restart;
	}

      /* If X has only a single bit significant, say, bit I, convert
	 (neg X) to (ashiftrt (ashift X C-I) C-I) where C is the bitsize of
	 MODE minus 1.  This will convert (neg (zero_extract X 1 Y)) to
	 (sign_extract X 1 Y).  But only do this if TEMP isn't a register
	 or a SUBREG of one since we'd be making the expression more
	 complex if it was just a register.  */

      if (GET_CODE (temp) != REG
	  && ! (GET_CODE (temp) == SUBREG
		&& GET_CODE (SUBREG_REG (temp)) == REG)
	  && (i = exact_log2 (significant_bits (temp, mode))) >= 0)
	{
	  rtx temp1 = simplify_shift_const
	    (NULL_RTX, ASHIFTRT, mode,
	     simplify_shift_const (NULL_RTX, ASHIFT, mode, temp,
				   GET_MODE_BITSIZE (mode) - 1 - i),
	     GET_MODE_BITSIZE (mode) - 1 - i);

	  /* If all we did was surround TEMP with the two shifts, we
	     haven't improved anything, so don't use it.  Otherwise,
	     we are better off with TEMP1.  */
	  if (GET_CODE (temp1) != ASHIFTRT
	      || GET_CODE (XEXP (temp1, 0)) != ASHIFT
	      || XEXP (XEXP (temp1, 0), 0) != temp)
	    {
	      x = temp1;
	      goto restart;
	    }
	}
      break;

    case FLOAT_TRUNCATE:
      /* (float_truncate:SF (float_extend:DF foo:SF)) = foo:SF.  */
      if (GET_CODE (XEXP (x, 0)) == FLOAT_EXTEND
	  && GET_MODE (XEXP (XEXP (x, 0), 0)) == mode)
 	return XEXP (XEXP (x, 0), 0);
      break;  

#ifdef HAVE_cc0
    case COMPARE:
      /* Convert (compare FOO (const_int 0)) to FOO unless we aren't
	 using cc0, in which case we want to leave it as a COMPARE
	 so we can distinguish it from a register-register-copy.  */
      if (XEXP (x, 1) == const0_rtx)
	return XEXP (x, 0);

      /* In IEEE floating point, x-0 is not the same as x.  */
      if ((TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
	   || GET_MODE_CLASS (GET_MODE (XEXP (x, 0))) == MODE_INT)
	  && XEXP (x, 1) == CONST0_RTX (GET_MODE (XEXP (x, 0))))
	return XEXP (x, 0);
      break;
#endif

    case CONST:
      /* (const (const X)) can become (const X).  Do it this way rather than
	 returning the inner CONST since CONST can be shared with a
	 REG_EQUAL note.  */
      if (GET_CODE (XEXP (x, 0)) == CONST)
	SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
      break;

#ifdef HAVE_lo_sum
    case LO_SUM:
      /* Convert (lo_sum (high FOO) FOO) to FOO.  This is necessary so we
	 can add in an offset.  find_split_point will split this address up
	 again if it doesn't match.  */
      if (GET_CODE (XEXP (x, 0)) == HIGH
	  && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1)))
	return XEXP (x, 1);
      break;
#endif

    case PLUS:
      /* If we have (plus (plus (A const) B)), associate it so that CONST is
	 outermost.  That's because that's the way indexed addresses are
	 supposed to appear.  This code used to check many more cases, but
	 they are now checked elsewhere.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && CONSTANT_ADDRESS_P (XEXP (XEXP (x, 0), 1)))
	return gen_binary (PLUS, mode,
			   gen_binary (PLUS, mode, XEXP (XEXP (x, 0), 0),
				       XEXP (x, 1)),
			   XEXP (XEXP (x, 0), 1));

      /* (plus (xor (and <foo> (const_int pow2 - 1)) <c>) <-c>)
	 when c is (const_int (pow2 + 1) / 2) is a sign extension of a
	 bit-field and can be replaced by either a sign_extend or a
	 sign_extract.  The `and' may be a zero_extend.  */
      if (GET_CODE (XEXP (x, 0)) == XOR
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) == - INTVAL (XEXP (XEXP (x, 0), 1))
	  && (i = exact_log2 (INTVAL (XEXP (XEXP (x, 0), 1)))) >= 0
	  && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	  && ((GET_CODE (XEXP (XEXP (x, 0), 0)) == AND
	       && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
	       && (INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))
		   == ((HOST_WIDE_INT) 1 << (i + 1)) - 1))
	      || (GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTEND
		  && (GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (XEXP (x, 0), 0), 0)))
		      == i + 1))))
	{
	  x = simplify_shift_const
	    (NULL_RTX, ASHIFTRT, mode,
	     simplify_shift_const (NULL_RTX, ASHIFT, mode,
				   XEXP (XEXP (XEXP (x, 0), 0), 0),
				   GET_MODE_BITSIZE (mode) - (i + 1)),
	     GET_MODE_BITSIZE (mode) - (i + 1));
	  goto restart;
	}

      /* If only the low-order bit of X is significant, (plus x -1)
	 can become (ashiftrt (ashift (xor x 1) C) C) where C is
	 the bitsize of the mode - 1.  This allows simplification of
	 "a = (b & 8) == 0;"  */
      if (XEXP (x, 1) == constm1_rtx
	  && GET_CODE (XEXP (x, 0)) != REG
	  && ! (GET_CODE (XEXP (x,0)) == SUBREG
		&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG)
	  && significant_bits (XEXP (x, 0), mode) == 1)
	{
	  x = simplify_shift_const
	    (NULL_RTX, ASHIFTRT, mode,
	     simplify_shift_const (NULL_RTX, ASHIFT, mode,
				   gen_rtx_combine (XOR, mode,
						    XEXP (x, 0), const1_rtx),
				   GET_MODE_BITSIZE (mode) - 1),
	     GET_MODE_BITSIZE (mode) - 1);
	  goto restart;
	}

      /* If we are adding two things that have no bits in common, convert
	 the addition into an IOR.  This will often be further simplified,
	 for example in cases like ((a & 1) + (a & 2)), which can
	 become a & 3.  */

      if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	  && (significant_bits (XEXP (x, 0), mode)
	      & significant_bits (XEXP (x, 1), mode)) == 0)
	{
	  x = gen_binary (IOR, mode, XEXP (x, 0), XEXP (x, 1));
	  goto restart;
	}
      break;

    case MINUS:
      /* (minus <foo> (and <foo> (const_int -pow2))) becomes
	 (and <foo> (const_int pow2-1))  */
      if (GET_CODE (XEXP (x, 1)) == AND
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  && exact_log2 (- INTVAL (XEXP (XEXP (x, 1), 1))) >= 0
	  && rtx_equal_p (XEXP (XEXP (x, 1), 0), XEXP (x, 0)))
	{
	  x = simplify_and_const_int (NULL_RTX, mode, XEXP (x, 0),
				      - INTVAL (XEXP (XEXP (x, 1), 1)) - 1);
	  goto restart;
	}
      break;

    case MULT:
      /* If we have (mult (plus A B) C), apply the distributive law and then
	 the inverse distributive law to see if things simplify.  This
	 occurs mostly in addresses, often when unrolling loops.  */

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  x = apply_distributive_law
	    (gen_binary (PLUS, mode,
			 gen_binary (MULT, mode,
				     XEXP (XEXP (x, 0), 0), XEXP (x, 1)),
			 gen_binary (MULT, mode,
				     XEXP (XEXP (x, 0), 1), XEXP (x, 1))));

	  if (GET_CODE (x) != MULT)
	    goto restart;
	}

      /* If this is multiplication by a power of two and its first operand is
	 a shift, treat the multiply as a shift to allow the shifts to
	 possibly combine.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (i = exact_log2 (INTVAL (XEXP (x, 1)))) >= 0
	  && (GET_CODE (XEXP (x, 0)) == ASHIFT
	      || GET_CODE (XEXP (x, 0)) == LSHIFTRT
	      || GET_CODE (XEXP (x, 0)) == ASHIFTRT
	      || GET_CODE (XEXP (x, 0)) == ROTATE
	      || GET_CODE (XEXP (x, 0)) == ROTATERT))
	{
	  x = simplify_shift_const (NULL_RTX, ASHIFT, mode, XEXP (x, 0), i);
	  goto restart;
	}

      /* Convert (mult (ashift (const_int 1) A) B) to (ashift B A).  */
      if (GET_CODE (XEXP (x, 0)) == ASHIFT
	  && XEXP (XEXP (x, 0), 0) == const1_rtx)
	return gen_rtx_combine (ASHIFT, mode, XEXP (x, 1),
				XEXP (XEXP (x, 0), 1));
      break;

    case UDIV:
      /* If this is a divide by a power of two, treat it as a shift if
	 its first operand is a shift.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (i = exact_log2 (INTVAL (XEXP (x, 1)))) >= 0
	  && (GET_CODE (XEXP (x, 0)) == ASHIFT
	      || GET_CODE (XEXP (x, 0)) == LSHIFTRT
	      || GET_CODE (XEXP (x, 0)) == ASHIFTRT
	      || GET_CODE (XEXP (x, 0)) == ROTATE
	      || GET_CODE (XEXP (x, 0)) == ROTATERT))
	{
	  x = simplify_shift_const (NULL_RTX, LSHIFTRT, mode, XEXP (x, 0), i);
	  goto restart;
	}
      break;

    case EQ:  case NE:
    case GT:  case GTU:  case GE:  case GEU:
    case LT:  case LTU:  case LE:  case LEU:
      /* If the first operand is a condition code, we can't do anything
	 with it.  */
      if (GET_CODE (XEXP (x, 0)) == COMPARE
	  || (GET_MODE_CLASS (GET_MODE (XEXP (x, 0))) != MODE_CC
#ifdef HAVE_cc0
	      && XEXP (x, 0) != cc0_rtx
#endif
	       ))
	{
	  rtx op0 = XEXP (x, 0);
	  rtx op1 = XEXP (x, 1);
	  enum rtx_code new_code;

	  if (GET_CODE (op0) == COMPARE)
	    op1 = XEXP (op0, 1), op0 = XEXP (op0, 0);

	  /* Simplify our comparison, if possible.  */
	  new_code = simplify_comparison (code, &op0, &op1);

#if STORE_FLAG_VALUE == 1
	  /* If STORE_FLAG_VALUE is 1, we can convert (ne x 0) to simply X
	     if only the low-order bit is significant in X (such as when
	     X is a ZERO_EXTRACT of one bit.  Similarly, we can convert
	     EQ to (xor X 1).  */
	  if (new_code == NE && GET_MODE_CLASS (mode) == MODE_INT
	      && op1 == const0_rtx
	      && significant_bits (op0, GET_MODE (op0)) == 1)
	    return gen_lowpart_for_combine (mode, op0);
	  else if (new_code == EQ && GET_MODE_CLASS (mode) == MODE_INT
		   && op1 == const0_rtx
		   && significant_bits (op0, GET_MODE (op0)) == 1)
	    return gen_rtx_combine (XOR, mode,
				    gen_lowpart_for_combine (mode, op0),
				    const1_rtx);
#endif

#if STORE_FLAG_VALUE == -1
	  /* If STORE_FLAG_VALUE is -1, we can convert (ne x 0)
	     to (neg x) if only the low-order bit of X is significant.
	     This converts (ne (zero_extract X 1 Y) 0) to
	     (sign_extract X 1 Y).  */
	  if (new_code == NE && GET_MODE_CLASS (mode) == MODE_INT
	      && op1 == const0_rtx
	      && significant_bits (op0, GET_MODE (op0)) == 1)
	    {
	      x = gen_rtx_combine (NEG, mode,
				   gen_lowpart_for_combine (mode, op0));
	      goto restart;
	    }
#endif

	  /* If STORE_FLAG_VALUE says to just test the sign bit and X has just
	     one significant bit, we can convert (ne x 0) to (ashift x c)
	     where C puts the bit in the sign bit.  Remove any AND with
	     STORE_FLAG_VALUE when we are done, since we are only going to
	     test the sign bit.  */
	  if (new_code == NE && GET_MODE_CLASS (mode) == MODE_INT
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	      && (STORE_FLAG_VALUE
		  == (HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (mode) - 1))
	      && op1 == const0_rtx
	      && mode == GET_MODE (op0)
	      && (i = exact_log2 (significant_bits (op0, GET_MODE (op0)))) >= 0)
	    {
	      x = simplify_shift_const (NULL_RTX, ASHIFT, mode, op0,
					GET_MODE_BITSIZE (mode) - 1 - i);
	      if (GET_CODE (x) == AND && XEXP (x, 1) == const_true_rtx)
		return XEXP (x, 0);
	      else
		return x;
	    }

	  /* If the code changed, return a whole new comparison.  */
	  if (new_code != code)
	    return gen_rtx_combine (new_code, mode, op0, op1);

	  /* Otherwise, keep this operation, but maybe change its operands.  
	     This also converts (ne (compare FOO BAR) 0) to (ne FOO BAR).  */
	  SUBST (XEXP (x, 0), op0);
	  SUBST (XEXP (x, 1), op1);
	}
      break;
	  
    case IF_THEN_ELSE:
      /* Sometimes we can simplify the arm of an IF_THEN_ELSE if a register
	 used in it is being compared against certain values.  Get the
	 true and false comparisons and see if that says anything about the
	 value of each arm.  */

      if (GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && reversible_comparison_p (XEXP (x, 0))
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG)
	{
	  HOST_WIDE_INT sig;
	  rtx from = XEXP (XEXP (x, 0), 0);
	  enum rtx_code true_code = GET_CODE (XEXP (x, 0));
	  enum rtx_code false_code = reverse_condition (true_code);
	  rtx true_val = XEXP (XEXP (x, 0), 1);
	  rtx false_val = true_val;
	  rtx true_arm = XEXP (x, 1);
	  rtx false_arm = XEXP (x, 2);
	  int swapped = 0;

	  /* If FALSE_CODE is EQ, swap the codes and arms.  */

	  if (false_code == EQ)
	    {
	      swapped = 1, true_code = EQ, false_code = NE;
	      true_arm = XEXP (x, 2), false_arm = XEXP (x, 1);
	    }

	  /* If we are comparing against zero and the expression being tested
	     has only a single significant bit, that is its value when it is 
	     not equal to zero.  Similarly if it is known to be -1 or 0.  */

	  if (true_code == EQ && true_val == const0_rtx
	      && exact_log2 (sig = significant_bits (from,
						     GET_MODE (from))) >= 0)
	    false_code = EQ, false_val = GEN_INT (sig);
	  else if (true_code == EQ && true_val == const0_rtx
		   && (num_sign_bit_copies (from, GET_MODE (from))
		       == GET_MODE_BITSIZE (GET_MODE (from))))
	    false_code = EQ, false_val = constm1_rtx;

	  /* Now simplify an arm if we know the value of the register
	     in the branch and it is used in the arm.  Be carefull due to
	     the potential of locally-shared RTL.  */

	  if (reg_mentioned_p (from, true_arm))
	    true_arm = subst (known_cond (copy_rtx (true_arm), true_code,
					  from, true_val),
			      pc_rtx, pc_rtx, 0, 0);
	  if (reg_mentioned_p (from, false_arm))
	    false_arm = subst (known_cond (copy_rtx (false_arm), false_code,
					   from, false_val),
			       pc_rtx, pc_rtx, 0, 0);

	  SUBST (XEXP (x, 1), swapped ? false_arm : true_arm);
	  SUBST (XEXP (x, 2), swapped ? true_arm : false_arm);
	}
      
      /* If we have (if_then_else FOO (pc) (label_ref BAR)) and FOO can be
	 reversed, do so to avoid needing two sets of patterns for
	 subtract-and-branch insns.  Similarly if we have a constant in that
	 position or if the third operand is the same as the first operand
	 of the comparison.  */

      if (GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && reversible_comparison_p (XEXP (x, 0))
	  && (XEXP (x, 1) == pc_rtx || GET_CODE (XEXP (x, 1)) == CONST_INT
	      || rtx_equal_p (XEXP (x, 2), XEXP (XEXP (x, 0), 0))))
	{
	  SUBST (XEXP (x, 0),
		 gen_binary (reverse_condition (GET_CODE (XEXP (x, 0))),
			     GET_MODE (XEXP (x, 0)),
			     XEXP (XEXP (x, 0), 0), XEXP (XEXP (x, 0), 1)));

	  temp = XEXP (x, 1);
	  SUBST (XEXP (x, 1), XEXP (x, 2));
	  SUBST (XEXP (x, 2), temp);
	}

      /* If the two arms are identical, we don't need the comparison.  */

      if (rtx_equal_p (XEXP (x, 1), XEXP (x, 2))
	  && ! side_effects_p (XEXP (x, 0)))
	return XEXP (x, 1);

      /* Look for cases where we have (abs x) or (neg (abs X)).  */

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_CODE (XEXP (x, 2)) == NEG
	  && rtx_equal_p (XEXP (x, 1), XEXP (XEXP (x, 2), 0))
	  && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && rtx_equal_p (XEXP (x, 1), XEXP (XEXP (x, 0), 0))
	  && ! side_effects_p (XEXP (x, 1)))
	switch (GET_CODE (XEXP (x, 0)))
	  {
	  case GT:
	  case GE:
	    x = gen_unary (ABS, mode, XEXP (x, 1));
	    goto restart;
	  case LT:
	  case LE:
	    x = gen_unary (NEG, mode, gen_unary (ABS, mode, XEXP (x, 1)));
	    goto restart;
	  }

      /* Look for MIN or MAX.  */

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1))
	  && rtx_equal_p (XEXP (XEXP (x, 0), 1), XEXP (x, 2))
	  && ! side_effects_p (XEXP (x, 0)))
	switch (GET_CODE (XEXP (x, 0)))
	  {
	  case GE:
	  case GT:
	    x = gen_binary (SMAX, mode, XEXP (x, 1), XEXP (x, 2));
	    goto restart;
	  case LE:
	  case LT:
	    x = gen_binary (SMIN, mode, XEXP (x, 1), XEXP (x, 2));
	    goto restart;
	  case GEU:
	  case GTU:
	    x = gen_binary (UMAX, mode, XEXP (x, 1), XEXP (x, 2));
	    goto restart;
	  case LEU:
	  case LTU:
	    x = gen_binary (UMIN, mode, XEXP (x, 1), XEXP (x, 2));
	    goto restart;
	  }

      /* If we have something like (if_then_else (ne A 0) (OP X C) X),
	 A is known to be either 0 or 1, and OP is an identity when its
	 second operand is zero, this can be done as (OP X (mult A C)).
	 Similarly if A is known to be 0 or -1 and also similarly if we have
	 a ZERO_EXTEND or SIGN_EXTEND as long as X is already extended (so
	 we don't destroy it).  */

      if (mode != VOIDmode
	  && (GET_CODE (XEXP (x, 0)) == EQ || GET_CODE (XEXP (x, 0)) == NE)
	  && XEXP (XEXP (x, 0), 1) == const0_rtx
	  && (significant_bits (XEXP (XEXP (x, 0), 0), mode) == 1
	      || (num_sign_bit_copies (XEXP (XEXP (x, 0), 0), mode)
		  == GET_MODE_BITSIZE (mode))))
	{
	  rtx nz = make_compound_operation (GET_CODE (XEXP (x, 0)) == NE
					    ? XEXP (x, 1) : XEXP (x, 2));
	  rtx z = GET_CODE (XEXP (x, 0)) == NE ? XEXP (x, 2) : XEXP (x, 1);
	  rtx dir = (significant_bits (XEXP (XEXP (x, 0), 0), mode) == 1
		     ? const1_rtx : constm1_rtx);
	  rtx c = 0;
	  enum machine_mode m = mode;
	  enum rtx_code op, extend_op = 0;

	  if ((GET_CODE (nz) == PLUS || GET_CODE (nz) == MINUS
	       || GET_CODE (nz) == IOR || GET_CODE (nz) == XOR
	       || GET_CODE (nz) == ASHIFT
	       || GET_CODE (nz) == LSHIFTRT || GET_CODE (nz) == ASHIFTRT)
	      && rtx_equal_p (XEXP (nz, 0), z))
	    c = XEXP (nz, 1), op = GET_CODE (nz);
	  else if (GET_CODE (nz) == SIGN_EXTEND
		   && (GET_CODE (XEXP (nz, 0)) == PLUS
		       || GET_CODE (XEXP (nz, 0)) == MINUS
		       || GET_CODE (XEXP (nz, 0)) == IOR
		       || GET_CODE (XEXP (nz, 0)) == XOR
		       || GET_CODE (XEXP (nz, 0)) == ASHIFT
		       || GET_CODE (XEXP (nz, 0)) == LSHIFTRT
		       || GET_CODE (XEXP (nz, 0)) == ASHIFTRT)
		   && GET_CODE (XEXP (XEXP (nz, 0), 0)) == SUBREG
		   && subreg_lowpart_p (XEXP (XEXP (nz, 0), 0))
		   && rtx_equal_p (SUBREG_REG (XEXP (XEXP (nz, 0), 0)), z)
		   && (num_sign_bit_copies (z, GET_MODE (z))
		       >= (GET_MODE_BITSIZE (mode)
			   - GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (nz, 0), 0))))))
	    {
	      c = XEXP (XEXP (nz, 0), 1);
	      op = GET_CODE (XEXP (nz, 0));
	      extend_op = SIGN_EXTEND;
	      m = GET_MODE (XEXP (nz, 0));
	    }
	  else if (GET_CODE (nz) == ZERO_EXTEND
		   && (GET_CODE (XEXP (nz, 0)) == PLUS
		       || GET_CODE (XEXP (nz, 0)) == MINUS
		       || GET_CODE (XEXP (nz, 0)) == IOR
		       || GET_CODE (XEXP (nz, 0)) == XOR
		       || GET_CODE (XEXP (nz, 0)) == ASHIFT
		       || GET_CODE (XEXP (nz, 0)) == LSHIFTRT
		       || GET_CODE (XEXP (nz, 0)) == ASHIFTRT)
		   && GET_CODE (XEXP (XEXP (nz, 0), 0)) == SUBREG
		   && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
		   && subreg_lowpart_p (XEXP (XEXP (nz, 0), 0))
		   && rtx_equal_p (SUBREG_REG (XEXP (XEXP (nz, 0), 0)), z)
		   && ((significant_bits (z, GET_MODE (z))
			& ~ GET_MODE_MASK (GET_MODE (XEXP (XEXP (nz, 0), 0))))
		       == 0))
	    {
	      c = XEXP (XEXP (nz, 0), 1);
	      op = GET_CODE (XEXP (nz, 0));
	      extend_op = ZERO_EXTEND;
	      m = GET_MODE (XEXP (nz, 0));
	    }

	  if (c && ! side_effects_p (c) && ! side_effects_p (z))
	    {
	      temp
		= gen_binary (MULT, m,
			      gen_lowpart_for_combine (m,
						       XEXP (XEXP (x, 0), 0)),
			      gen_binary (MULT, m, c, dir));

	      temp = gen_binary (op, m, gen_lowpart_for_combine (m, z), temp);

	      if (extend_op != 0)
		temp = gen_unary (extend_op, mode, temp);

	      return temp;
	    }
	}
      break;
	  
    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
    case ZERO_EXTEND:
    case SIGN_EXTEND:
      /* If we are processing SET_DEST, we are done. */
      if (in_dest)
	return x;

      x = expand_compound_operation (x);
      if (GET_CODE (x) != code)
	goto restart;
      break;

    case SET:
      /* (set (pc) (return)) gets written as (return).  */
      if (GET_CODE (SET_DEST (x)) == PC && GET_CODE (SET_SRC (x)) == RETURN)
	return SET_SRC (x);

      /* Convert this into a field assignment operation, if possible.  */
      x = make_field_assignment (x);

      /* If we are setting CC0 or if the source is a COMPARE, look for the
	 use of the comparison result and try to simplify it unless we already
	 have used undobuf.other_insn.  */
      if ((GET_CODE (SET_SRC (x)) == COMPARE
#ifdef HAVE_cc0
	   || SET_DEST (x) == cc0_rtx
#endif
	   )
	  && (cc_use = find_single_use (SET_DEST (x), subst_insn,
					&other_insn)) != 0
	  && (undobuf.other_insn == 0 || other_insn == undobuf.other_insn)
	  && GET_RTX_CLASS (GET_CODE (*cc_use)) == '<'
	  && XEXP (*cc_use, 0) == SET_DEST (x))
	{
	  enum rtx_code old_code = GET_CODE (*cc_use);
	  enum rtx_code new_code;
	  rtx op0, op1;
	  int other_changed = 0;
	  enum machine_mode compare_mode = GET_MODE (SET_DEST (x));

	  if (GET_CODE (SET_SRC (x)) == COMPARE)
	    op0 = XEXP (SET_SRC (x), 0), op1 = XEXP (SET_SRC (x), 1);
	  else
	    op0 = SET_SRC (x), op1 = const0_rtx;

	  /* Simplify our comparison, if possible.  */
	  new_code = simplify_comparison (old_code, &op0, &op1);

#if !defined (HAVE_cc0) && defined (EXTRA_CC_MODES)
	  /* If this machine has CC modes other than CCmode, check to see
	     if we need to use a different CC mode here.  */
	  compare_mode = SELECT_CC_MODE (new_code, op0, op1);

	  /* If the mode changed, we have to change SET_DEST, the mode
	     in the compare, and the mode in the place SET_DEST is used.
	     If SET_DEST is a hard register, just build new versions with
	     the proper mode.  If it is a pseudo, we lose unless it is only
	     time we set the pseudo, in which case we can safely change
	     its mode.  */
	  if (compare_mode != GET_MODE (SET_DEST (x)))
	    {
	      int regno = REGNO (SET_DEST (x));
	      rtx new_dest = gen_rtx (REG, compare_mode, regno);

	      if (regno < FIRST_PSEUDO_REGISTER
		  || (reg_n_sets[regno] == 1
		      && ! REG_USERVAR_P (SET_DEST (x))))
		{
		  if (regno >= FIRST_PSEUDO_REGISTER)
		    SUBST (regno_reg_rtx[regno], new_dest);

		  SUBST (SET_DEST (x), new_dest);
		  SUBST (XEXP (*cc_use, 0), new_dest);
		  other_changed = 1;
		}
	    }
#endif

	  /* If the code changed, we have to build a new comparison
	     in undobuf.other_insn.  */
	  if (new_code != old_code)
	    {
	      unsigned mask;

	      SUBST (*cc_use, gen_rtx_combine (new_code, GET_MODE (*cc_use),
					       SET_DEST (x), const0_rtx));

	      /* If the only change we made was to change an EQ into an
		 NE or vice versa, OP0 has only one significant bit,
		 and OP1 is zero, check if changing the user of the condition
		 code will produce a valid insn.  If it won't, we can keep
		 the original code in that insn by surrounding our operation
		 with an XOR.  */

	      if (((old_code == NE && new_code == EQ)
		   || (old_code == EQ && new_code == NE))
		  && ! other_changed && op1 == const0_rtx
		  && (GET_MODE_BITSIZE (GET_MODE (op0))
		      <= HOST_BITS_PER_WIDE_INT)
		  && (exact_log2 (mask = significant_bits (op0,
							   GET_MODE (op0)))
		      >= 0))
		{
		  rtx pat = PATTERN (other_insn), note = 0;

		  if ((recog_for_combine (&pat, undobuf.other_insn, &note) < 0
		       && ! check_asm_operands (pat)))
		    {
		      PUT_CODE (*cc_use, old_code);
		      other_insn = 0;

		      op0 = gen_binary (XOR, GET_MODE (op0), op0,
					GEN_INT (mask));
		    }
		}

	      other_changed = 1;
	    }

	  if (other_changed)
	    undobuf.other_insn = other_insn;

#ifdef HAVE_cc0
	  /* If we are now comparing against zero, change our source if
	     needed.  If we do not use cc0, we always have a COMPARE.  */
	  if (op1 == const0_rtx && SET_DEST (x) == cc0_rtx)
	    SUBST (SET_SRC (x), op0);
	  else
#endif

	  /* Otherwise, if we didn't previously have a COMPARE in the
	     correct mode, we need one.  */
	  if (GET_CODE (SET_SRC (x)) != COMPARE
	      || GET_MODE (SET_SRC (x)) != compare_mode)
	    SUBST (SET_SRC (x), gen_rtx_combine (COMPARE, compare_mode,
						 op0, op1));
	  else
	    {
	      /* Otherwise, update the COMPARE if needed.  */
	      SUBST (XEXP (SET_SRC (x), 0), op0);
	      SUBST (XEXP (SET_SRC (x), 1), op1);
	    }
	}
      else
	{
	  /* Get SET_SRC in a form where we have placed back any
	     compound expressions.  Then do the checks below.  */
	  temp = make_compound_operation (SET_SRC (x), SET);
	  SUBST (SET_SRC (x), temp);
	}

      /* If we have (set x (subreg:m1 (op:m2 ...) 0)) with OP being some
	 operation, and X being a REG or (subreg (reg)), we may be able to
	 convert this to (set (subreg:m2 x) (op)).

	 We can always do this if M1 is narrower than M2 because that
	 means that we only care about the low bits of the result.

	 However, on most machines (those with BYTE_LOADS_ZERO_EXTEND
	 and BYTES_LOADS_SIGN_EXTEND not defined), we cannot perform a
	 narrower operation that requested since the high-order bits will
	 be undefined.  On machine where BYTE_LOADS_*_EXTEND is defined,
	 however, this transformation is safe as long as M1 and M2 have
	 the same number of words.  */
 
      if (GET_CODE (SET_SRC (x)) == SUBREG
	  && subreg_lowpart_p (SET_SRC (x))
	  && GET_RTX_CLASS (GET_CODE (SUBREG_REG (SET_SRC (x)))) != 'o'
	  && (((GET_MODE_SIZE (GET_MODE (SET_SRC (x))) + (UNITS_PER_WORD - 1))
	       / UNITS_PER_WORD)
	      == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_SRC (x))))
		   + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD))
#if ! defined(BYTE_LOADS_ZERO_EXTEND) && ! defined (BYTE_LOADS_SIGN_EXTEND)
	  && (GET_MODE_SIZE (GET_MODE (SET_SRC (x)))
	      < GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_SRC (x)))))
#endif
	  && (GET_CODE (SET_DEST (x)) == REG
	      || (GET_CODE (SET_DEST (x)) == SUBREG
		  && GET_CODE (SUBREG_REG (SET_DEST (x))) == REG)))
	{
	  SUBST (SET_DEST (x),
		 gen_lowpart_for_combine (GET_MODE (SUBREG_REG (SET_SRC (x))),
					  SET_DEST (x)));
	  SUBST (SET_SRC (x), SUBREG_REG (SET_SRC (x)));
	}

#ifdef BYTE_LOADS_ZERO_EXTEND
      /* If we have (set FOO (subreg:M (mem:N BAR) 0)) with
	 M wider than N, this would require a paradoxical subreg.
	 Replace the subreg with a zero_extend to avoid the reload that
	 would otherwise be required. */
      if (GET_CODE (SET_SRC (x)) == SUBREG
	  && subreg_lowpart_p (SET_SRC (x))
	  && SUBREG_WORD (SET_SRC (x)) == 0
	  && (GET_MODE_SIZE (GET_MODE (SET_SRC (x)))
	      > GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_SRC (x)))))
	  && GET_CODE (SUBREG_REG (SET_SRC (x))) == MEM)
	SUBST (SET_SRC (x), gen_rtx_combine (ZERO_EXTEND,
					     GET_MODE (SET_SRC (x)),
					     XEXP (SET_SRC (x), 0)));
#endif

#ifndef HAVE_conditional_move

      /* If we don't have a conditional move, SET_SRC is an IF_THEN_ELSE,
	 and we are comparing an item known to be 0 or -1 against 0, use a
	 logical operation instead. Check for one of the arms being an IOR
	 of the other arm with some value.  We compute three terms to be
	 IOR'ed together.  In practice, at most two will be nonzero.  Then
	 we do the IOR's.  */

      if (GET_CODE (SET_DEST (x)) != PC
	  && GET_CODE (SET_SRC (x)) == IF_THEN_ELSE
	  && (GET_CODE (XEXP (SET_SRC (x), 0)) == EQ
	      || GET_CODE (XEXP (SET_SRC (x), 0)) == NE)
	  && XEXP (XEXP (SET_SRC (x), 0), 1) == const0_rtx
	  && (num_sign_bit_copies (XEXP (XEXP (SET_SRC (x), 0), 0),
				   GET_MODE (XEXP (XEXP (SET_SRC (x), 0), 0)))
	      == GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (SET_SRC (x), 0), 0))))
	  && ! side_effects_p (SET_SRC (x)))
	{
	  rtx true = (GET_CODE (XEXP (SET_SRC (x), 0)) == NE
		      ? XEXP (SET_SRC (x), 1) : XEXP (SET_SRC (x), 2));
	  rtx false = (GET_CODE (XEXP (SET_SRC (x), 0)) == NE
		       ? XEXP (SET_SRC (x), 2) : XEXP (SET_SRC (x), 1));
	  rtx term1 = const0_rtx, term2, term3;

	  if (GET_CODE (true) == IOR && rtx_equal_p (XEXP (true, 0), false))
	    term1 = false, true = XEXP (true, 1), false = const0_rtx;
	  else if (GET_CODE (true) == IOR
		   && rtx_equal_p (XEXP (true, 1), false))
	    term1 = false, true = XEXP (true, 0), false = const0_rtx;
	  else if (GET_CODE (false) == IOR
		   && rtx_equal_p (XEXP (false, 0), true))
	    term1 = true, false = XEXP (false, 1), true = const0_rtx;
	  else if (GET_CODE (false) == IOR
		   && rtx_equal_p (XEXP (false, 1), true))
	    term1 = true, false = XEXP (false, 0), true = const0_rtx;

	  term2 = gen_binary (AND, GET_MODE (SET_SRC (x)),
			      XEXP (XEXP (SET_SRC (x), 0), 0), true);
	  term3 = gen_binary (AND, GET_MODE (SET_SRC (x)),
			      gen_unary (NOT, GET_MODE (SET_SRC (x)),
					 XEXP (XEXP (SET_SRC (x), 0), 0)),
			      false);

	  SUBST (SET_SRC (x),
		 gen_binary (IOR, GET_MODE (SET_SRC (x)),
			     gen_binary (IOR, GET_MODE (SET_SRC (x)),
					 term1, term2),
			     term3));
	}
#endif
      break;

    case AND:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  x = simplify_and_const_int (x, mode, XEXP (x, 0),
				      INTVAL (XEXP (x, 1)));

	  /* If we have (ior (and (X C1) C2)) and the next restart would be
	     the last, simplify this by making C1 as small as possible
	     and then exit. */
	  if (n_restarts >= 3 && GET_CODE (x) == IOR
	      && GET_CODE (XEXP (x, 0)) == AND
	      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	      && GET_CODE (XEXP (x, 1)) == CONST_INT)
	    {
	      temp = gen_binary (AND, mode, XEXP (XEXP (x, 0), 0),
				 GEN_INT (INTVAL (XEXP (XEXP (x, 0), 1))
					  & ~ INTVAL (XEXP (x, 1))));
	      return gen_binary (IOR, mode, temp, XEXP (x, 1));
	    }

	  if (GET_CODE (x) != AND)
	    goto restart;
	}

      /* Convert (A | B) & A to A.  */
      if (GET_CODE (XEXP (x, 0)) == IOR
	  && (rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1))
	      || rtx_equal_p (XEXP (XEXP (x, 0), 1), XEXP (x, 1)))
	  && ! side_effects_p (XEXP (XEXP (x, 0), 0))
	  && ! side_effects_p (XEXP (XEXP (x, 0), 1)))
	return XEXP (x, 1);

      /* Convert (A ^ B) & A to A & (~ B) since the latter is often a single
	 insn (and may simplify more).  */
      else if (GET_CODE (XEXP (x, 0)) == XOR
	  && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1))
	  && ! side_effects_p (XEXP (x, 1)))
	{
	  x = gen_binary (AND, mode,
			  gen_unary (NOT, mode, XEXP (XEXP (x, 0), 1)),
			  XEXP (x, 1));
	  goto restart;
	}
      else if (GET_CODE (XEXP (x, 0)) == XOR
	       && rtx_equal_p (XEXP (XEXP (x, 0), 1), XEXP (x, 1))
	       && ! side_effects_p (XEXP (x, 1)))
	{
	  x = gen_binary (AND, mode,
			  gen_unary (NOT, mode, XEXP (XEXP (x, 0), 0)),
			  XEXP (x, 1));
	  goto restart;
	}

      /* Similarly for (~ (A ^ B)) & A.  */
      else if (GET_CODE (XEXP (x, 0)) == NOT
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == XOR
	       && rtx_equal_p (XEXP (XEXP (XEXP (x, 0), 0), 0), XEXP (x, 1))
	       && ! side_effects_p (XEXP (x, 1)))
	{
	  x = gen_binary (AND, mode, XEXP (XEXP (XEXP (x, 0), 0), 1),
			  XEXP (x, 1));
	  goto restart;
	}
      else if (GET_CODE (XEXP (x, 0)) == NOT
	       && GET_CODE (XEXP (XEXP (x, 0), 0)) == XOR
	       && rtx_equal_p (XEXP (XEXP (XEXP (x, 0), 0), 1), XEXP (x, 1))
	       && ! side_effects_p (XEXP (x, 1)))
	{
	  x = gen_binary (AND, mode, XEXP (XEXP (XEXP (x, 0), 0), 0),
			  XEXP (x, 1));
	  goto restart;
	}

      /* If we have (and A B) with A not an object but that is known to
	 be -1 or 0, this is equivalent to the expression
	 (if_then_else (ne A (const_int 0)) B (const_int 0))
	 We make this conversion because it may allow further
	 simplifications and then allow use of conditional move insns.
	 If the machine doesn't have condition moves, code in case SET
	 will convert the IF_THEN_ELSE back to the logical operation.
	 We build the IF_THEN_ELSE here in case further simplification
	 is possible (e.g., we can convert it to ABS).  */

      if (GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) != 'o'
	  && ! (GET_CODE (XEXP (x, 0)) == SUBREG
		&& GET_RTX_CLASS (GET_CODE (SUBREG_REG (XEXP (x, 0)))) == 'o')
	  && (num_sign_bit_copies (XEXP (x, 0), GET_MODE (XEXP (x, 0)))
	      == GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0)))))
	{
	  rtx op0 = XEXP (x, 0);
	  rtx op1 = const0_rtx;
	  enum rtx_code comp_code
	    = simplify_comparison (NE, &op0, &op1);

	  x =  gen_rtx_combine (IF_THEN_ELSE, mode,
				gen_binary (comp_code, VOIDmode, op0, op1),
				XEXP (x, 1), const0_rtx);
	  goto restart;
	}

      /* In the following group of tests (and those in case IOR below),
	 we start with some combination of logical operations and apply
	 the distributive law followed by the inverse distributive law.
	 Most of the time, this results in no change.  However, if some of
	 the operands are the same or inverses of each other, simplifications
	 will result.

	 For example, (and (ior A B) (not B)) can occur as the result of
	 expanding a bit field assignment.  When we apply the distributive
	 law to this, we get (ior (and (A (not B))) (and (B (not B)))),
	 which then simplifies to (and (A (not B))).  */

      /* If we have (and (ior A B) C), apply the distributive law and then
	 the inverse distributive law to see if things simplify.  */

      if (GET_CODE (XEXP (x, 0)) == IOR || GET_CODE (XEXP (x, 0)) == XOR)
	{
	  x = apply_distributive_law
	    (gen_binary (GET_CODE (XEXP (x, 0)), mode,
			 gen_binary (AND, mode,
				     XEXP (XEXP (x, 0), 0), XEXP (x, 1)),
			 gen_binary (AND, mode,
				     XEXP (XEXP (x, 0), 1), XEXP (x, 1))));
	  if (GET_CODE (x) != AND)
	    goto restart;
	}

      if (GET_CODE (XEXP (x, 1)) == IOR || GET_CODE (XEXP (x, 1)) == XOR)
	{
	  x = apply_distributive_law
	    (gen_binary (GET_CODE (XEXP (x, 1)), mode,
			 gen_binary (AND, mode,
				     XEXP (XEXP (x, 1), 0), XEXP (x, 0)),
			 gen_binary (AND, mode,
				     XEXP (XEXP (x, 1), 1), XEXP (x, 0))));
	  if (GET_CODE (x) != AND)
	    goto restart;
	}

      /* Similarly, taking advantage of the fact that
	 (and (not A) (xor B C)) == (xor (ior A B) (ior A C))  */

      if (GET_CODE (XEXP (x, 0)) == NOT && GET_CODE (XEXP (x, 1)) == XOR)
	{
	  x = apply_distributive_law
	    (gen_binary (XOR, mode,
			 gen_binary (IOR, mode, XEXP (XEXP (x, 0), 0),
				     XEXP (XEXP (x, 1), 0)),
			 gen_binary (IOR, mode, XEXP (XEXP (x, 0), 0),
				     XEXP (XEXP (x, 1), 1))));
	  if (GET_CODE (x) != AND)
	    goto restart;
	}
							    
      else if (GET_CODE (XEXP (x, 1)) == NOT && GET_CODE (XEXP (x, 0)) == XOR)
	{
	  x = apply_distributive_law
	    (gen_binary (XOR, mode,
			 gen_binary (IOR, mode, XEXP (XEXP (x, 1), 0),
				     XEXP (XEXP (x, 0), 0)),
			 gen_binary (IOR, mode, XEXP (XEXP (x, 1), 0),
				     XEXP (XEXP (x, 0), 1))));
	  if (GET_CODE (x) != AND)
	    goto restart;
	}
      break;

    case IOR:
      /* (ior A C) is C if all significant bits of A are on in C.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	  && (significant_bits (XEXP (x, 0), mode)
	      & ~ INTVAL (XEXP (x, 1))) == 0)
	return XEXP (x, 1);

      /* Convert (A & B) | A to A.  */
      if (GET_CODE (XEXP (x, 0)) == AND
	  && (rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1))
	      || rtx_equal_p (XEXP (XEXP (x, 0), 1), XEXP (x, 1)))
	  && ! side_effects_p (XEXP (XEXP (x, 0), 0))
	  && ! side_effects_p (XEXP (XEXP (x, 0), 1)))
	return XEXP (x, 1);

      /* If we have (ior (and A B) C), apply the distributive law and then
	 the inverse distributive law to see if things simplify.  */

      if (GET_CODE (XEXP (x, 0)) == AND)
	{
	  x = apply_distributive_law
	    (gen_binary (AND, mode,
			 gen_binary (IOR, mode,
				     XEXP (XEXP (x, 0), 0), XEXP (x, 1)),
			 gen_binary (IOR, mode,
				     XEXP (XEXP (x, 0), 1), XEXP (x, 1))));

	  if (GET_CODE (x) != IOR)
	    goto restart;
	}

      if (GET_CODE (XEXP (x, 1)) == AND)
	{
	  x = apply_distributive_law
	    (gen_binary (AND, mode,
			 gen_binary (IOR, mode,
				     XEXP (XEXP (x, 1), 0), XEXP (x, 0)),
			 gen_binary (IOR, mode,
				     XEXP (XEXP (x, 1), 1), XEXP (x, 0))));

	  if (GET_CODE (x) != IOR)
	    goto restart;
	}

      /* Convert (ior (ashift A CX) (lshiftrt A CY)) where CX+CY equals the
	 mode size to (rotate A CX).  */

      if (((GET_CODE (XEXP (x, 0)) == ASHIFT
	    && GET_CODE (XEXP (x, 1)) == LSHIFTRT)
	   || (GET_CODE (XEXP (x, 1)) == ASHIFT
	       && GET_CODE (XEXP (x, 0)) == LSHIFTRT))
	  && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (XEXP (x, 1), 0))
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  && (INTVAL (XEXP (XEXP (x, 0), 1)) + INTVAL (XEXP (XEXP (x, 1), 1))
	      == GET_MODE_BITSIZE (mode)))
	{
	  rtx shift_count;

	  if (GET_CODE (XEXP (x, 0)) == ASHIFT)
	    shift_count = XEXP (XEXP (x, 0), 1);
	  else
	    shift_count = XEXP (XEXP (x, 1), 1);
	  x = gen_rtx (ROTATE, mode, XEXP (XEXP (x, 0), 0), shift_count);
	  goto restart;
	}
      break;

    case XOR:
      /* Convert (XOR (NOT x) (NOT y)) to (XOR x y).
	 Also convert (XOR (NOT x) y) to (NOT (XOR x y)), similarly for
	 (NOT y).  */
      {
	int num_negated = 0;
	rtx in1 = XEXP (x, 0), in2 = XEXP (x, 1);

	if (GET_CODE (in1) == NOT)
	  num_negated++, in1 = XEXP (in1, 0);
	if (GET_CODE (in2) == NOT)
	  num_negated++, in2 = XEXP (in2, 0);

	if (num_negated == 2)
	  {
	    SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
	    SUBST (XEXP (x, 1), XEXP (XEXP (x, 1), 0));
	  }
	else if (num_negated == 1)
	  {
	    x =  gen_unary (NOT, mode,
			    gen_binary (XOR, mode, in1, in2));
	    goto restart;
	  }
      }

      /* Convert (xor (and A B) B) to (and (not A) B).  The latter may
	 correspond to a machine insn or result in further simplifications
	 if B is a constant.  */

      if (GET_CODE (XEXP (x, 0)) == AND
	  && rtx_equal_p (XEXP (XEXP (x, 0), 1), XEXP (x, 1))
	  && ! side_effects_p (XEXP (x, 1)))
	{
	  x = gen_binary (AND, mode,
			  gen_unary (NOT, mode, XEXP (XEXP (x, 0), 0)),
			  XEXP (x, 1));
	  goto restart;
	}
      else if (GET_CODE (XEXP (x, 0)) == AND
	       && rtx_equal_p (XEXP (XEXP (x, 0), 0), XEXP (x, 1))
	       && ! side_effects_p (XEXP (x, 1)))
	{
	  x = gen_binary (AND, mode,
			  gen_unary (NOT, mode, XEXP (XEXP (x, 0), 1)),
			  XEXP (x, 1));
	  goto restart;
	}


#if STORE_FLAG_VALUE == 1
      /* (xor (comparison foo bar) (const_int 1)) can become the reversed
	 comparison.  */
      if (XEXP (x, 1) == const1_rtx
	  && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && reversible_comparison_p (XEXP (x, 0)))
	return gen_rtx_combine (reverse_condition (GET_CODE (XEXP (x, 0))),
				mode, XEXP (XEXP (x, 0), 0),
				XEXP (XEXP (x, 0), 1));
#endif

      /* (xor (comparison foo bar) (const_int sign-bit))
	 when STORE_FLAG_VALUE is the sign bit.  */
      if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	  && (STORE_FLAG_VALUE
	      == (HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (mode) - 1))
	  && XEXP (x, 1) == const_true_rtx
	  && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
	  && reversible_comparison_p (XEXP (x, 0)))
	return gen_rtx_combine (reverse_condition (GET_CODE (XEXP (x, 0))),
				mode, XEXP (XEXP (x, 0), 0),
				XEXP (XEXP (x, 0), 1));
      break;

    case ABS:
      /* (abs (neg <foo>)) -> (abs <foo>) */
      if (GET_CODE (XEXP (x, 0)) == NEG)
	SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));

      /* If operand is something known to be positive, ignore the ABS.  */
      if (GET_CODE (XEXP (x, 0)) == FFS || GET_CODE (XEXP (x, 0)) == ABS
	  || ((GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0)))
	       <= HOST_BITS_PER_WIDE_INT)
	      && ((significant_bits (XEXP (x, 0), GET_MODE (XEXP (x, 0)))
		   & ((HOST_WIDE_INT) 1
		      << (GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0))) - 1)))
		  == 0)))
	return XEXP (x, 0);


      /* If operand is known to be only -1 or 0, convert ABS to NEG.  */
      if (num_sign_bit_copies (XEXP (x, 0), mode) == GET_MODE_BITSIZE (mode))
	{
	  x = gen_rtx_combine (NEG, mode, XEXP (x, 0));
	  goto restart;
	}
      break;

    case FFS:
      /* (ffs (*_extend <X>)) = (ffs <X>) */
      if (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	  || GET_CODE (XEXP (x, 0)) == ZERO_EXTEND)
	SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
      break;

    case FLOAT:
      /* (float (sign_extend <X>)) = (float <X>).  */
      if (GET_CODE (XEXP (x, 0)) == SIGN_EXTEND)
	SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
      break;

    case LSHIFT:
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
    case ROTATE:
    case ROTATERT:
      /* If this is a shift by a constant amount, simplify it.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  x = simplify_shift_const (x, code, mode, XEXP (x, 0), 
				    INTVAL (XEXP (x, 1)));
	  if (GET_CODE (x) != code)
	    goto restart;
	}

#ifdef SHIFT_COUNT_TRUNCATED
      else if (GET_CODE (XEXP (x, 1)) != REG)
	SUBST (XEXP (x, 1),
	       force_to_mode (XEXP (x, 1), GET_MODE (x),
			      exact_log2 (GET_MODE_BITSIZE (GET_MODE (x))),
			      NULL_RTX));
#endif

      break;
    }

  return x;
}

/* We consider ZERO_EXTRACT, SIGN_EXTRACT, and SIGN_EXTEND as "compound
   operations" because they can be replaced with two more basic operations.
   ZERO_EXTEND is also considered "compound" because it can be replaced with
   an AND operation, which is simpler, though only one operation.

   The function expand_compound_operation is called with an rtx expression
   and will convert it to the appropriate shifts and AND operations, 
   simplifying at each stage.

   The function make_compound_operation is called to convert an expression
   consisting of shifts and ANDs into the equivalent compound expression.
   It is the inverse of this function, loosely speaking.  */

static rtx
expand_compound_operation (x)
     rtx x;
{
  int pos = 0, len;
  int unsignedp = 0;
  int modewidth;
  rtx tem;

  switch (GET_CODE (x))
    {
    case ZERO_EXTEND:
      unsignedp = 1;
    case SIGN_EXTEND:
      /* We can't necessarily use a const_int for a multiword mode;
	 it depends on implicitly extending the value.
	 Since we don't know the right way to extend it,
	 we can't tell whether the implicit way is right.

	 Even for a mode that is no wider than a const_int,
	 we can't win, because we need to sign extend one of its bits through
	 the rest of it, and we don't know which bit.  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	return x;

      if (! FAKE_EXTEND_SAFE_P (GET_MODE (XEXP (x, 0)), XEXP (x, 0)))
	return x;

      len = GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0)));
      /* If the inner object has VOIDmode (the only way this can happen
	 is if it is a ASM_OPERANDS), we can't do anything since we don't
	 know how much masking to do.  */
      if (len == 0)
	return x;

      break;

    case ZERO_EXTRACT:
      unsignedp = 1;
    case SIGN_EXTRACT:
      /* If the operand is a CLOBBER, just return it.  */
      if (GET_CODE (XEXP (x, 0)) == CLOBBER)
	return XEXP (x, 0);

      if (GET_CODE (XEXP (x, 1)) != CONST_INT
	  || GET_CODE (XEXP (x, 2)) != CONST_INT
	  || GET_MODE (XEXP (x, 0)) == VOIDmode)
	return x;

      len = INTVAL (XEXP (x, 1));
      pos = INTVAL (XEXP (x, 2));

      /* If this goes outside the object being extracted, replace the object
	 with a (use (mem ...)) construct that only combine understands
	 and is used only for this purpose.  */
      if (len + pos > GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0))))
	SUBST (XEXP (x, 0), gen_rtx (USE, GET_MODE (x), XEXP (x, 0)));

#if BITS_BIG_ENDIAN
      pos = GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0))) - len - pos;
#endif
      break;

    default:
      return x;
    }

  /* If we reach here, we want to return a pair of shifts.  The inner
     shift is a left shift of BITSIZE - POS - LEN bits.  The outer
     shift is a right shift of BITSIZE - LEN bits.  It is arithmetic or
     logical depending on the value of UNSIGNEDP.

     If this was a ZERO_EXTEND or ZERO_EXTRACT, this pair of shifts will be
     converted into an AND of a shift.

     We must check for the case where the left shift would have a negative
     count.  This can happen in a case like (x >> 31) & 255 on machines
     that can't shift by a constant.  On those machines, we would first
     combine the shift with the AND to produce a variable-position 
     extraction.  Then the constant of 31 would be substituted in to produce
     a such a position.  */

  modewidth = GET_MODE_BITSIZE (GET_MODE (x));
  if (modewidth >= pos - len)
    tem = simplify_shift_const (NULL_RTX, unsignedp ? LSHIFTRT : ASHIFTRT,
				GET_MODE (x),
				simplify_shift_const (NULL_RTX, ASHIFT,
						      GET_MODE (x),
						      XEXP (x, 0),
						      modewidth - pos - len),
				modewidth - len);

  else if (unsignedp && len < HOST_BITS_PER_WIDE_INT)
    tem = simplify_and_const_int (NULL_RTX, GET_MODE (x),
				  simplify_shift_const (NULL_RTX, LSHIFTRT,
							GET_MODE (x),
							XEXP (x, 0), pos),
				  ((HOST_WIDE_INT) 1 << len) - 1);
  else
    /* Any other cases we can't handle.  */
    return x;
    

  /* If we couldn't do this for some reason, return the original
     expression.  */
  if (GET_CODE (tem) == CLOBBER)
    return x;

  return tem;
}

/* X is a SET which contains an assignment of one object into
   a part of another (such as a bit-field assignment, STRICT_LOW_PART,
   or certain SUBREGS). If possible, convert it into a series of
   logical operations.

   We half-heartedly support variable positions, but do not at all
   support variable lengths.  */

static rtx
expand_field_assignment (x)
     rtx x;
{
  rtx inner;
  rtx pos;			/* Always counts from low bit. */
  int len;
  rtx mask;
  enum machine_mode compute_mode;

  /* Loop until we find something we can't simplify.  */
  while (1)
    {
      if (GET_CODE (SET_DEST (x)) == STRICT_LOW_PART
	  && GET_CODE (XEXP (SET_DEST (x), 0)) == SUBREG)
	{
	  inner = SUBREG_REG (XEXP (SET_DEST (x), 0));
	  len = GET_MODE_BITSIZE (GET_MODE (XEXP (SET_DEST (x), 0)));
	  pos = const0_rtx;
	}
      else if (GET_CODE (SET_DEST (x)) == ZERO_EXTRACT
	       && GET_CODE (XEXP (SET_DEST (x), 1)) == CONST_INT)
	{
	  inner = XEXP (SET_DEST (x), 0);
	  len = INTVAL (XEXP (SET_DEST (x), 1));
	  pos = XEXP (SET_DEST (x), 2);

	  /* If the position is constant and spans the width of INNER,
	     surround INNER  with a USE to indicate this.  */
	  if (GET_CODE (pos) == CONST_INT
	      && INTVAL (pos) + len > GET_MODE_BITSIZE (GET_MODE (inner)))
	    inner = gen_rtx (USE, GET_MODE (SET_DEST (x)), inner);

#if BITS_BIG_ENDIAN
	  if (GET_CODE (pos) == CONST_INT)
	    pos = GEN_INT (GET_MODE_BITSIZE (GET_MODE (inner)) - len
			   - INTVAL (pos));
	  else if (GET_CODE (pos) == MINUS
		   && GET_CODE (XEXP (pos, 1)) == CONST_INT
		   && (INTVAL (XEXP (pos, 1))
		       == GET_MODE_BITSIZE (GET_MODE (inner)) - len))
	    /* If position is ADJUST - X, new position is X.  */
	    pos = XEXP (pos, 0);
	  else
	    pos = gen_binary (MINUS, GET_MODE (pos),
			      GEN_INT (GET_MODE_BITSIZE (GET_MODE (inner))
				       - len),
			      pos);
#endif
	}

      /* A SUBREG between two modes that occupy the same numbers of words
	 can be done by moving the SUBREG to the source.  */
      else if (GET_CODE (SET_DEST (x)) == SUBREG
	       && (((GET_MODE_SIZE (GET_MODE (SET_DEST (x)))
		     + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)
		   == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (SET_DEST (x))))
			+ (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)))
	{
	  x = gen_rtx (SET, VOIDmode, SUBREG_REG (SET_DEST (x)),
		       gen_lowpart_for_combine (GET_MODE (SUBREG_REG (SET_DEST (x))),
						SET_SRC (x)));
	  continue;
	}
      else
	break;

      while (GET_CODE (inner) == SUBREG && subreg_lowpart_p (inner))
	inner = SUBREG_REG (inner);

      compute_mode = GET_MODE (inner);

      /* Compute a mask of LEN bits, if we can do this on the host machine.  */
      if (len < HOST_BITS_PER_WIDE_INT)
	mask = GEN_INT (((HOST_WIDE_INT) 1 << len) - 1);
      else
	break;

      /* Now compute the equivalent expression.  Make a copy of INNER
	 for the SET_DEST in case it is a MEM into which we will substitute;
	 we don't want shared RTL in that case.  */
      x = gen_rtx (SET, VOIDmode, copy_rtx (inner),
		   gen_binary (IOR, compute_mode,
			       gen_binary (AND, compute_mode,
					   gen_unary (NOT, compute_mode,
						      gen_binary (ASHIFT,
								  compute_mode,
								  mask, pos)),
					   inner),
			       gen_binary (ASHIFT, compute_mode,
					   gen_binary (AND, compute_mode,
						       gen_lowpart_for_combine
						       (compute_mode,
							SET_SRC (x)),
						       mask),
					   pos)));
    }

  return x;
}

/* Return an RTX for a reference to LEN bits of INNER.  POS is the starting
   bit position (counted from the LSB) if >= 0; otherwise POS_RTX represents
   the starting bit position.

   INNER may be a USE.  This will occur when we started with a bitfield
   that went outside the boundary of the object in memory, which is
   allowed on most machines.  To isolate this case, we produce a USE
   whose mode is wide enough and surround the MEM with it.  The only
   code that understands the USE is this routine.  If it is not removed,
   it will cause the resulting insn not to match.

   UNSIGNEDP is non-zero for an unsigned reference and zero for a 
   signed reference.

   IN_DEST is non-zero if this is a reference in the destination of a
   SET.  This is used when a ZERO_ or SIGN_EXTRACT isn't needed.  If non-zero,
   a STRICT_LOW_PART will be used, if zero, ZERO_EXTEND or SIGN_EXTEND will
   be used.

   IN_COMPARE is non-zero if we are in a COMPARE.  This means that a
   ZERO_EXTRACT should be built even for bits starting at bit 0.

   MODE is the desired mode of the result (if IN_DEST == 0).  */

static rtx
make_extraction (mode, inner, pos, pos_rtx, len,
		 unsignedp, in_dest, in_compare)
     enum machine_mode mode;
     rtx inner;
     int pos;
     rtx pos_rtx;
     int len;
     int unsignedp;
     int in_dest, in_compare;
{
  /* This mode describes the size of the storage area
     to fetch the overall value from.  Within that, we
     ignore the POS lowest bits, etc.  */
  enum machine_mode is_mode = GET_MODE (inner);
  enum machine_mode inner_mode;
  enum machine_mode wanted_mem_mode = byte_mode;
  enum machine_mode pos_mode = word_mode;
  enum machine_mode extraction_mode = word_mode;
  enum machine_mode tmode = mode_for_size (len, MODE_INT, 1);
  int spans_byte = 0;
  rtx new = 0;

  /* Get some information about INNER and get the innermost object.  */
  if (GET_CODE (inner) == USE)
    /* (use:SI (mem:QI foo)) stands for (mem:SI foo).  */
    /* We don't need to adjust the position because we set up the USE
       to pretend that it was a full-word object.  */
    spans_byte = 1, inner = XEXP (inner, 0);
  else if (GET_CODE (inner) == SUBREG && subreg_lowpart_p (inner))
    {
      /* If going from (subreg:SI (mem:QI ...)) to (mem:QI ...),
	 consider just the QI as the memory to extract from.
	 The subreg adds or removes high bits; its mode is
	 irrelevant to the meaning of this extraction,
	 since POS and LEN count from the lsb.  */
      if (GET_CODE (SUBREG_REG (inner)) == MEM)
	is_mode = GET_MODE (SUBREG_REG (inner));
      inner = SUBREG_REG (inner);
    }

  inner_mode = GET_MODE (inner);

  if (pos_rtx && GET_CODE (pos_rtx) == CONST_INT)
    pos = INTVAL (pos_rtx);

  /* See if this can be done without an extraction.  We never can if the
     width of the field is not the same as that of some integer mode. For
     registers, we can only avoid the extraction if the position is at the
     low-order bit and this is either not in the destination or we have the
     appropriate STRICT_LOW_PART operation available.

     For MEM, we can avoid an extract if the field starts on an appropriate
     boundary and we can change the mode of the memory reference.  However,
     we cannot directly access the MEM if we have a USE and the underlying
     MEM is not TMODE.  This combination means that MEM was being used in a
     context where bits outside its mode were being referenced; that is only
     valid in bit-field insns.  */

  if (tmode != BLKmode
      && ! (spans_byte && inner_mode != tmode)
      && ((pos == 0 && GET_CODE (inner) != MEM
	   && (! in_dest
	       || (GET_CODE (inner) == REG
		   && (movstrict_optab->handlers[(int) tmode].insn_code
		       != CODE_FOR_nothing))))
	  || (GET_CODE (inner) == MEM && pos >= 0
	      && (pos
		  % (STRICT_ALIGNMENT ? GET_MODE_ALIGNMENT (tmode)
		     : BITS_PER_UNIT)) == 0
	      /* We can't do this if we are widening INNER_MODE (it
		 may not be aligned, for one thing).  */
	      && GET_MODE_BITSIZE (inner_mode) >= GET_MODE_BITSIZE (tmode)
	      && (inner_mode == tmode
		  || (! mode_dependent_address_p (XEXP (inner, 0))
		      && ! MEM_VOLATILE_P (inner))))))
    {
      /* If INNER is a MEM, make a new MEM that encompasses just the desired
	 field.  If the original and current mode are the same, we need not
	 adjust the offset.  Otherwise, we do if bytes big endian.  

	 If INNER is not a MEM, get a piece consisting of the just the field
	 of interest (in this case POS must be 0).  */

      if (GET_CODE (inner) == MEM)
	{
	  int offset;
	  /* POS counts from lsb, but make OFFSET count in memory order.  */
	  if (BYTES_BIG_ENDIAN)
	    offset = (GET_MODE_BITSIZE (is_mode) - len - pos) / BITS_PER_UNIT;
	  else
	    offset = pos / BITS_PER_UNIT;

	  new = gen_rtx (MEM, tmode, plus_constant (XEXP (inner, 0), offset));
	  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (inner);
	  MEM_VOLATILE_P (new) = MEM_VOLATILE_P (inner);
	  MEM_IN_STRUCT_P (new) = MEM_IN_STRUCT_P (inner);
	}
      else if (GET_CODE (inner) == REG)
	/* We can't call gen_lowpart_for_combine here since we always want
	   a SUBREG and it would sometimes return a new hard register.  */
	new = gen_rtx (SUBREG, tmode, inner,
		       (WORDS_BIG_ENDIAN
			&& GET_MODE_SIZE (inner_mode) > UNITS_PER_WORD
			? ((GET_MODE_SIZE (inner_mode) - GET_MODE_SIZE (tmode))
			   / UNITS_PER_WORD)
			: 0));
      else
	new = force_to_mode (inner, tmode, len, NULL_RTX);

      /* If this extraction is going into the destination of a SET, 
	 make a STRICT_LOW_PART unless we made a MEM.  */

      if (in_dest)
	return (GET_CODE (new) == MEM ? new
		: (GET_CODE (new) != SUBREG
		   ? gen_rtx (CLOBBER, tmode, const0_rtx)
		   : gen_rtx_combine (STRICT_LOW_PART, VOIDmode, new)));

      /* Otherwise, sign- or zero-extend unless we already are in the
	 proper mode.  */

      return (mode == tmode ? new
	      : gen_rtx_combine (unsignedp ? ZERO_EXTEND : SIGN_EXTEND,
				 mode, new));
    }

  /* Unless this is a COMPARE or we have a funny memory reference,
     don't do anything with zero-extending field extracts starting at
     the low-order bit since they are simple AND operations.  */
  if (pos == 0 && ! in_dest && ! in_compare && ! spans_byte && unsignedp)
    return 0;

  /* Get the mode to use should INNER be a MEM, the mode for the position,
     and the mode for the result.  */
#ifdef HAVE_insv
  if (in_dest)
    {
      wanted_mem_mode = insn_operand_mode[(int) CODE_FOR_insv][0];
      pos_mode = insn_operand_mode[(int) CODE_FOR_insv][2];
      extraction_mode = insn_operand_mode[(int) CODE_FOR_insv][3];
    }
#endif

#ifdef HAVE_extzv
  if (! in_dest && unsignedp)
    {
      wanted_mem_mode = insn_operand_mode[(int) CODE_FOR_extzv][1];
      pos_mode = insn_operand_mode[(int) CODE_FOR_extzv][3];
      extraction_mode = insn_operand_mode[(int) CODE_FOR_extzv][0];
    }
#endif

#ifdef HAVE_extv
  if (! in_dest && ! unsignedp)
    {
      wanted_mem_mode = insn_operand_mode[(int) CODE_FOR_extv][1];
      pos_mode = insn_operand_mode[(int) CODE_FOR_extv][3];
      extraction_mode = insn_operand_mode[(int) CODE_FOR_extv][0];
    }
#endif

  /* Never narrow an object, since that might not be safe.  */

  if (mode != VOIDmode
      && GET_MODE_SIZE (extraction_mode) < GET_MODE_SIZE (mode))
    extraction_mode = mode;

  if (pos_rtx && GET_MODE (pos_rtx) != VOIDmode
      && GET_MODE_SIZE (pos_mode) < GET_MODE_SIZE (GET_MODE (pos_rtx)))
    pos_mode = GET_MODE (pos_rtx);

  /* If this is not from memory or we have to change the mode of memory and
     cannot, the desired mode is EXTRACTION_MODE.  */
  if (GET_CODE (inner) != MEM
      || (inner_mode != wanted_mem_mode
	  && (mode_dependent_address_p (XEXP (inner, 0))
	      || MEM_VOLATILE_P (inner))))
    wanted_mem_mode = extraction_mode;

#if BITS_BIG_ENDIAN
  /* If position is constant, compute new position.  Otherwise, build
     subtraction.  */
  if (pos >= 0)
    pos = (MAX (GET_MODE_BITSIZE (is_mode), GET_MODE_BITSIZE (wanted_mem_mode))
	   - len - pos);
  else
    pos_rtx
      = gen_rtx_combine (MINUS, GET_MODE (pos_rtx),
			 GEN_INT (MAX (GET_MODE_BITSIZE (is_mode),
				       GET_MODE_BITSIZE (wanted_mem_mode))
				  - len),
			 pos_rtx);
#endif

  /* If INNER has a wider mode, make it smaller.  If this is a constant
     extract, try to adjust the byte to point to the byte containing
     the value.  */
  if (wanted_mem_mode != VOIDmode
      && GET_MODE_SIZE (wanted_mem_mode) < GET_MODE_SIZE (is_mode)
      && ((GET_CODE (inner) == MEM
	   && (inner_mode == wanted_mem_mode
	       || (! mode_dependent_address_p (XEXP (inner, 0))
		   && ! MEM_VOLATILE_P (inner))))))
    {
      int offset = 0;

      /* The computations below will be correct if the machine is big
	 endian in both bits and bytes or little endian in bits and bytes.
	 If it is mixed, we must adjust.  */
	     
#if BYTES_BIG_ENDIAN != BITS_BIG_ENDIAN
      if (! spans_byte && is_mode != wanted_mem_mode)
	offset = (GET_MODE_SIZE (is_mode)
		  - GET_MODE_SIZE (wanted_mem_mode) - offset);
#endif

      /* If bytes are big endian and we had a paradoxical SUBREG, we must
	 adjust OFFSET to compensate. */
#if BYTES_BIG_ENDIAN
      if (! spans_byte
	  && GET_MODE_SIZE (inner_mode) < GET_MODE_SIZE (is_mode))
	offset -= GET_MODE_SIZE (is_mode) - GET_MODE_SIZE (inner_mode);
#endif

      /* If this is a constant position, we can move to the desired byte.  */
      if (pos >= 0)
	{
	  offset += pos / BITS_PER_UNIT;
	  pos %= GET_MODE_BITSIZE (wanted_mem_mode);
	}

      if (offset != 0 || inner_mode != wanted_mem_mode)
	{
	  rtx newmem = gen_rtx (MEM, wanted_mem_mode,
				plus_constant (XEXP (inner, 0), offset));
	  RTX_UNCHANGING_P (newmem) = RTX_UNCHANGING_P (inner);
	  MEM_VOLATILE_P (newmem) = MEM_VOLATILE_P (inner);
	  MEM_IN_STRUCT_P (newmem) = MEM_IN_STRUCT_P (inner);
	  inner = newmem;
	}
    }

  /* If INNER is not memory, we can always get it into the proper mode. */
  else if (GET_CODE (inner) != MEM)
    inner = force_to_mode (inner, extraction_mode,
			   (pos < 0 ? GET_MODE_BITSIZE (extraction_mode)
			    : len + pos),
			   NULL_RTX);

  /* Adjust mode of POS_RTX, if needed.  If we want a wider mode, we
     have to zero extend.  Otherwise, we can just use a SUBREG.  */
  if (pos < 0
      && GET_MODE_SIZE (pos_mode) > GET_MODE_SIZE (GET_MODE (pos_rtx)))
    pos_rtx = gen_rtx_combine (ZERO_EXTEND, pos_mode, pos_rtx);
  else if (pos < 0
	   && GET_MODE_SIZE (pos_mode) < GET_MODE_SIZE (GET_MODE (pos_rtx)))
    pos_rtx = gen_lowpart_for_combine (pos_mode, pos_rtx);

  /* Make POS_RTX unless we already have it and it is correct.  */
  if (pos_rtx == 0 || (pos >= 0 && INTVAL (pos_rtx) != pos))
    pos_rtx = GEN_INT (pos);

  /* Make the required operation.  See if we can use existing rtx.  */
  new = gen_rtx_combine (unsignedp ? ZERO_EXTRACT : SIGN_EXTRACT,
			 extraction_mode, inner, GEN_INT (len), pos_rtx);
  if (! in_dest)
    new = gen_lowpart_for_combine (mode, new);

  return new;
}

/* Look at the expression rooted at X.  Look for expressions
   equivalent to ZERO_EXTRACT, SIGN_EXTRACT, ZERO_EXTEND, SIGN_EXTEND.
   Form these expressions.

   Return the new rtx, usually just X.

   Also, for machines like the Vax that don't have logical shift insns,
   try to convert logical to arithmetic shift operations in cases where
   they are equivalent.  This undoes the canonicalizations to logical
   shifts done elsewhere.

   We try, as much as possible, to re-use rtl expressions to save memory.

   IN_CODE says what kind of expression we are processing.  Normally, it is
   SET.  In a memory address (inside a MEM, PLUS or minus, the latter two
   being kludges), it is MEM.  When processing the arguments of a comparison
   or a COMPARE against zero, it is COMPARE.  */

static rtx
make_compound_operation (x, in_code)
     rtx x;
     enum rtx_code in_code;
{
  enum rtx_code code = GET_CODE (x);
  enum machine_mode mode = GET_MODE (x);
  int mode_width = GET_MODE_BITSIZE (mode);
  enum rtx_code next_code;
  int i, count;
  rtx new = 0;
  char *fmt;

  /* Select the code to be used in recursive calls.  Once we are inside an
     address, we stay there.  If we have a comparison, set to COMPARE,
     but once inside, go back to our default of SET.  */

  next_code = (code == MEM || code == PLUS || code == MINUS ? MEM
	       : ((code == COMPARE || GET_RTX_CLASS (code) == '<')
		  && XEXP (x, 1) == const0_rtx) ? COMPARE
	       : in_code == COMPARE ? SET : in_code);

  /* Process depending on the code of this operation.  If NEW is set
     non-zero, it will be returned.  */

  switch (code)
    {
    case ASHIFT:
    case LSHIFT:
      /* Convert shifts by constants into multiplications if inside
	 an address.  */
      if (in_code == MEM && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT
	  && INTVAL (XEXP (x, 1)) >= 0)
	new = gen_rtx_combine (MULT, mode, XEXP (x, 0),
			       GEN_INT ((HOST_WIDE_INT) 1
					<< INTVAL (XEXP (x, 1))));
      break;

    case AND:
      /* If the second operand is not a constant, we can't do anything
	 with it.  */
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	break;

      /* If the constant is a power of two minus one and the first operand
	 is a logical right shift, make an extraction.  */
      if (GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && (i = exact_log2 (INTVAL (XEXP (x, 1)) + 1)) >= 0)
	new = make_extraction (mode, XEXP (XEXP (x, 0), 0), -1,
			       XEXP (XEXP (x, 0), 1), i, 1,
			       0, in_code == COMPARE);

      /* Same as previous, but for (subreg (lshiftrt ...)) in first op.  */
      else if (GET_CODE (XEXP (x, 0)) == SUBREG
	       && subreg_lowpart_p (XEXP (x, 0))
	       && GET_CODE (SUBREG_REG (XEXP (x, 0))) == LSHIFTRT
	       && (i = exact_log2 (INTVAL (XEXP (x, 1)) + 1)) >= 0)
	new = make_extraction (GET_MODE (SUBREG_REG (XEXP (x, 0))),
			       XEXP (SUBREG_REG (XEXP (x, 0)), 0), -1,
			       XEXP (SUBREG_REG (XEXP (x, 0)), 1), i, 1,
			       0, in_code == COMPARE);


      /* If we are have (and (rotate X C) M) and C is larger than the number
	 of bits in M, this is an extraction.  */

      else if (GET_CODE (XEXP (x, 0)) == ROTATE
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	       && (i = exact_log2 (INTVAL (XEXP (x, 1)) + 1)) >= 0
	       && i <= INTVAL (XEXP (XEXP (x, 0), 1)))
	new = make_extraction (mode, XEXP (XEXP (x, 0), 0),
			       (GET_MODE_BITSIZE (mode)
				- INTVAL (XEXP (XEXP (x, 0), 1))),
			       NULL_RTX, i, 1, 0, in_code == COMPARE);

      /* On machines without logical shifts, if the operand of the AND is
	 a logical shift and our mask turns off all the propagated sign
	 bits, we can replace the logical shift with an arithmetic shift.  */
      else if (ashr_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing
	       && (lshr_optab->handlers[(int) mode].insn_code
		   == CODE_FOR_nothing)
	       && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	       && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	       && INTVAL (XEXP (XEXP (x, 0), 1)) >= 0
	       && INTVAL (XEXP (XEXP (x, 0), 1)) < HOST_BITS_PER_WIDE_INT
	       && mode_width <= HOST_BITS_PER_WIDE_INT)
	{
	  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (mode);

	  mask >>= INTVAL (XEXP (XEXP (x, 0), 1));
	  if ((INTVAL (XEXP (x, 1)) & ~mask) == 0)
	    SUBST (XEXP (x, 0),
		   gen_rtx_combine (ASHIFTRT, mode, XEXP (XEXP (x, 0), 0),
				    XEXP (XEXP (x, 0), 1)));
	}

      /* If the constant is one less than a power of two, this might be
	 representable by an extraction even if no shift is present.
	 If it doesn't end up being a ZERO_EXTEND, we will ignore it unless
	 we are in a COMPARE.  */
      else if ((i = exact_log2 (INTVAL (XEXP (x, 1)) + 1)) >= 0)
	new = make_extraction (mode, XEXP (x, 0), 0, NULL_RTX, i, 1,
			       0, in_code == COMPARE);

      /* If we are in a comparison and this is an AND with a power of two,
	 convert this into the appropriate bit extract.  */
      else if (in_code == COMPARE
	       && (i = exact_log2 (INTVAL (XEXP (x, 1)))) >= 0)
	new = make_extraction (mode, XEXP (x, 0), i, NULL_RTX, 1, 1, 0, 1);

      break;

    case LSHIFTRT:
      /* If the sign bit is known to be zero, replace this with an
	 arithmetic shift.  */
      if (ashr_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing
	  && lshr_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing
	  && mode_width <= HOST_BITS_PER_WIDE_INT
	  && (significant_bits (XEXP (x, 0), mode)
	      & (1 << (mode_width - 1))) == 0)
	{
	  new = gen_rtx_combine (ASHIFTRT, mode, XEXP (x, 0), XEXP (x, 1));
	  break;
	}

      /* ... fall through ... */

    case ASHIFTRT:
      /* If we have (ashiftrt (ashift foo C1) C2) with C2 >= C1,
	 this is a SIGN_EXTRACT.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (x, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= INTVAL (XEXP (XEXP (x, 0), 1)))
	new = make_extraction (mode, XEXP (XEXP (x, 0), 0),
			       (INTVAL (XEXP (x, 1))
				- INTVAL (XEXP (XEXP (x, 0), 1))),
			       NULL_RTX, mode_width - INTVAL (XEXP (x, 1)),
			       code == LSHIFTRT, 0, in_code == COMPARE);

      /* Similarly if we have (ashifrt (OP (ashift foo C1) C3) C2).  In these
	 cases, we are better off returning a SIGN_EXTEND of the operation.  */

      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && (GET_CODE (XEXP (x, 0)) == IOR || GET_CODE (XEXP (x, 0)) == AND
	      || GET_CODE (XEXP (x, 0)) == XOR
	      || GET_CODE (XEXP (x, 0)) == PLUS)
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))
	  && INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1)) < HOST_BITS_PER_WIDE_INT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (INTVAL (XEXP (XEXP (x, 0), 1))
	      & (((HOST_WIDE_INT) 1
		  << INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))) - 1)) == 0)
	{
	  HOST_WIDE_INT newop1
	    = (INTVAL (XEXP (XEXP (x, 0), 1))
	       >> INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1)));

	  new = make_extraction (mode,
				 gen_binary (GET_CODE (XEXP (x, 0)), mode,
					     XEXP (XEXP (XEXP (x, 0), 0), 0),
					     GEN_INT (newop1)),
				 (INTVAL (XEXP (x, 1))
				  - INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))),
				 NULL_RTX, mode_width - INTVAL (XEXP (x, 1)),
				 code == LSHIFTRT, 0, in_code == COMPARE);
	}

      /* Similarly for (ashiftrt (neg (ashift FOO C1)) C2).  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (x, 0)) == NEG
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1)))
	new = make_extraction (mode,
			       gen_unary (GET_CODE (XEXP (x, 0)), mode,
					  XEXP (XEXP (XEXP (x, 0), 0), 0)),
			       (INTVAL (XEXP (x, 1))
				- INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))),
			       NULL_RTX, mode_width - INTVAL (XEXP (x, 1)),
			       code == LSHIFTRT, 0, in_code == COMPARE);
      break;
    }

  if (new)
    {
      x = gen_lowpart_for_combine (mode, new);
      code = GET_CODE (x);
    }

  /* Now recursively process each operand of this operation.  */
  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    if (fmt[i] == 'e')
      {
	new = make_compound_operation (XEXP (x, i), next_code);
	SUBST (XEXP (x, i), new);
      }

  return x;
}

/* Given M see if it is a value that would select a field of bits
    within an item, but not the entire word.  Return -1 if not.
    Otherwise, return the starting position of the field, where 0 is the
    low-order bit.

   *PLEN is set to the length of the field.  */

static int
get_pos_from_mask (m, plen)
     unsigned HOST_WIDE_INT m;
     int *plen;
{
  /* Get the bit number of the first 1 bit from the right, -1 if none.  */
  int pos = exact_log2 (m & - m);

  if (pos < 0)
    return -1;

  /* Now shift off the low-order zero bits and see if we have a power of
     two minus 1.  */
  *plen = exact_log2 ((m >> pos) + 1);

  if (*plen <= 0)
    return -1;

  return pos;
}

/* Rewrite X so that it is an expression in MODE.  We only care about the
   low-order BITS bits so we can ignore AND operations that just clear
   higher-order bits.

   Also, if REG is non-zero and X is a register equal in value to REG, 
   replace X with REG.  */

static rtx
force_to_mode (x, mode, bits, reg)
     rtx x;
     enum machine_mode mode;
     int bits;
     rtx reg;
{
  enum rtx_code code = GET_CODE (x);
  enum machine_mode op_mode = mode;

  /* If X is narrower than MODE or if BITS is larger than the size of MODE,
     just get X in the proper mode.  */

  if (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (mode)
      || bits > GET_MODE_BITSIZE (mode))
    return gen_lowpart_for_combine (mode, x);

  switch (code)
    {
    case SIGN_EXTEND:
    case ZERO_EXTEND:
    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
      x = expand_compound_operation (x);
      if (GET_CODE (x) != code)
	return force_to_mode (x, mode, bits, reg);
      break;

    case REG:
      if (reg != 0 && (rtx_equal_p (get_last_value (reg), x)
		       || rtx_equal_p (reg, get_last_value (x))))
	x = reg;
      break;

    case CONST_INT:
      if (bits < HOST_BITS_PER_WIDE_INT)
	x = GEN_INT (INTVAL (x) & (((HOST_WIDE_INT) 1 << bits) - 1));
      return x;

    case SUBREG:
      /* Ignore low-order SUBREGs. */
      if (subreg_lowpart_p (x))
	return force_to_mode (SUBREG_REG (x), mode, bits, reg);
      break;

    case AND:
      /* If this is an AND with a constant.  Otherwise, we fall through to
	 do the general binary case.  */

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  HOST_WIDE_INT mask = INTVAL (XEXP (x, 1));
	  int len = exact_log2 (mask + 1);
	  rtx op = XEXP (x, 0);

	  /* If this is masking some low-order bits, we may be able to
	     impose a stricter constraint on what bits of the operand are
	     required.  */

	  op = force_to_mode (op, mode, len > 0 ? MIN (len, bits) : bits,
			      reg);

	  if (bits < HOST_BITS_PER_WIDE_INT)
	    mask &= ((HOST_WIDE_INT) 1 << bits) - 1;

	  /* If we have no AND in MODE, use the original mode for the
	     operation.  */

	  if (and_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	    op_mode = GET_MODE (x);

	  x = simplify_and_const_int (x, op_mode, op, mask);

	  /* If X is still an AND, see if it is an AND with a mask that
	     is just some low-order bits.  If so, and it is BITS wide (it
	     can't be wider), we don't need it.  */

	  if (GET_CODE (x) == AND && GET_CODE (XEXP (x, 1)) == CONST_INT
	      && bits < HOST_BITS_PER_WIDE_INT
	      && INTVAL (XEXP (x, 1)) == ((HOST_WIDE_INT) 1 << bits) - 1)
	    x = XEXP (x, 0);

	  break;
	}

      /* ... fall through ... */

    case PLUS:
    case MINUS:
    case MULT:
    case IOR:
    case XOR:
      /* For most binary operations, just propagate into the operation and
	 change the mode if we have an operation of that mode.  */

      if ((code == PLUS
	   && add_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	  || (code == MINUS
	      && sub_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	  || (code == MULT && (smul_optab->handlers[(int) mode].insn_code
			       == CODE_FOR_nothing))
	  || (code == AND
	      && and_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	  || (code == IOR
	      && ior_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	  || (code == XOR && (xor_optab->handlers[(int) mode].insn_code
			      == CODE_FOR_nothing)))
	op_mode = GET_MODE (x);

      x = gen_binary (code, op_mode,
		      gen_lowpart_for_combine (op_mode,
					       force_to_mode (XEXP (x, 0),
							      mode, bits,
							      reg)),
		      gen_lowpart_for_combine (op_mode,
					       force_to_mode (XEXP (x, 1),
							      mode, bits,
							      reg)));
      break;

    case ASHIFT:
    case LSHIFT:
      /* For left shifts, do the same, but just for the first operand.
	 If the shift count is a constant, we need even fewer bits of the
	 first operand.  */

      if (GET_CODE (XEXP (x, 1)) == CONST_INT && INTVAL (XEXP (x, 1)) < bits)
	bits -= INTVAL (XEXP (x, 1));

      if ((code == ASHIFT
	   && ashl_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	  || (code == LSHIFT && (lshl_optab->handlers[(int) mode].insn_code
				 == CODE_FOR_nothing)))
	op_mode = GET_MODE (x);

      x =  gen_binary (code, op_mode,
		       gen_lowpart_for_combine (op_mode,
						force_to_mode (XEXP (x, 0),
							       mode, bits,
							       reg)),
		       XEXP (x, 1));
      break;

    case LSHIFTRT:
      /* Here we can only do something if the shift count is a constant and
	 the count plus BITS is no larger than the width of MODE, we can do
	 the shift in MODE.  */

      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) + bits <= GET_MODE_BITSIZE (mode))
	{
	  rtx inner = force_to_mode (XEXP (x, 0), mode,
				     bits + INTVAL (XEXP (x, 1)), reg);

	  if (lshr_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	    op_mode = GET_MODE (x);

	  x = gen_binary (LSHIFTRT, op_mode,
			  gen_lowpart_for_combine (op_mode, inner),
			  XEXP (x, 1));
	}
      break;

    case ASHIFTRT:
      /* If this is a sign-extension operation that just affects bits
	 we don't care about, remove it.  */

      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) <= GET_MODE_BITSIZE (GET_MODE (x)) - bits
	  && GET_CODE (XEXP (x, 0)) == ASHIFT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && INTVAL (XEXP (XEXP (x, 0), 1)) == INTVAL (XEXP (x, 1)))
	return force_to_mode (XEXP (XEXP (x, 0), 0), mode, bits, reg);
      break;

    case NEG:
    case NOT:
      if ((code == NEG
	   && neg_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
	  || (code == NOT && (one_cmpl_optab->handlers[(int) mode].insn_code
			      == CODE_FOR_nothing)))
	op_mode = GET_MODE (x);

      /* Handle these similarly to the way we handle most binary operations. */
      x = gen_unary (code, op_mode,
		     gen_lowpart_for_combine (op_mode,
					      force_to_mode (XEXP (x, 0), mode,
							     bits, reg)));
      break;

    case IF_THEN_ELSE:
      /* We have no way of knowing if the IF_THEN_ELSE can itself be
	 written in a narrower mode.  We play it safe and do not do so.  */

      SUBST (XEXP (x, 1),
	     gen_lowpart_for_combine (GET_MODE (x),
				      force_to_mode (XEXP (x, 1), mode,
						     bits, reg)));
      SUBST (XEXP (x, 2),
	     gen_lowpart_for_combine (GET_MODE (x),
				      force_to_mode (XEXP (x, 2), mode,
						     bits, reg)));
      break;
    }

  /* Ensure we return a value of the proper mode.  */
  return gen_lowpart_for_combine (mode, x);
}

/* Return the value of expression X given the fact that condition COND
   is known to be true when applied to REG as its first operand and VAL
   as its second.  X is known to not be shared and so can be modified in
   place.

   We only handle the simplest cases, and specifically those cases that
   arise with IF_THEN_ELSE expressions.  */

static rtx
known_cond (x, cond, reg, val)
     rtx x;
     enum rtx_code cond;
     rtx reg, val;
{
  enum rtx_code code = GET_CODE (x);
  rtx new, temp;
  char *fmt;
  int i, j;

  if (side_effects_p (x))
    return x;

  if (cond == EQ && rtx_equal_p (x, reg))
    return val;

  /* If X is (abs REG) and we know something about REG's relationship
     with zero, we may be able to simplify this.  */

  if (code == ABS && rtx_equal_p (XEXP (x, 0), reg) && val == const0_rtx)
    switch (cond)
      {
      case GE:  case GT:  case EQ:
	return XEXP (x, 0);
      case LT:  case LE:
	return gen_unary (NEG, GET_MODE (XEXP (x, 0)), XEXP (x, 0));
      }

  /* The only other cases we handle are MIN, MAX, and comparisons if the
     operands are the same as REG and VAL.  */

  else if (GET_RTX_CLASS (code) == '<' || GET_RTX_CLASS (code) == 'c')
    {
      if (rtx_equal_p (XEXP (x, 0), val))
	cond = swap_condition (cond), temp = val, val = reg, reg = temp;

      if (rtx_equal_p (XEXP (x, 0), reg) && rtx_equal_p (XEXP (x, 1), val))
	{
	  if (GET_RTX_CLASS (code) == '<')
	    return (comparison_dominates_p (cond, code) ? const_true_rtx
		    : (comparison_dominates_p (cond,
					       reverse_condition (code))
		       ? const0_rtx : x));

	  else if (code == SMAX || code == SMIN
		   || code == UMIN || code == UMAX)
	    {
	      int unsignedp = (code == UMIN || code == UMAX);

	      if (code == SMAX || code == UMAX)
		cond = reverse_condition (cond);

	      switch (cond)
		{
		case GE:   case GT:
		  return unsignedp ? x : XEXP (x, 1);
		case LE:   case LT:
		  return unsignedp ? x : XEXP (x, 0);
		case GEU:  case GTU:
		  return unsignedp ? XEXP (x, 1) : x;
		case LEU:  case LTU:
		  return unsignedp ? XEXP (x, 0) : x;
		}
	    }
	}
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	SUBST (XEXP (x, i), known_cond (XEXP (x, i), cond, reg, val));
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  SUBST (XVECEXP (x, i, j), known_cond (XVECEXP (x, i, j),
						cond, reg, val));
    }

  return x;
}

/* See if X, a SET operation, can be rewritten as a bit-field assignment.
   Return that assignment if so.

   We only handle the most common cases.  */

static rtx
make_field_assignment (x)
     rtx x;
{
  rtx dest = SET_DEST (x);
  rtx src = SET_SRC (x);
  rtx ourdest;
  rtx assign;
  HOST_WIDE_INT c1;
  int pos, len;
  rtx other;
  enum machine_mode mode;

  /* If SRC was (and (not (ashift (const_int 1) POS)) DEST), this is
     a clear of a one-bit field.  We will have changed it to
     (and (rotate (const_int -2) POS) DEST), so check for that.  Also check
     for a SUBREG.  */

  if (GET_CODE (src) == AND && GET_CODE (XEXP (src, 0)) == ROTATE
      && GET_CODE (XEXP (XEXP (src, 0), 0)) == CONST_INT
      && INTVAL (XEXP (XEXP (src, 0), 0)) == -2
      && (rtx_equal_p (dest, XEXP (src, 1))
	  || rtx_equal_p (dest, get_last_value (XEXP (src, 1)))
	  || rtx_equal_p (get_last_value (dest), XEXP (src, 1))))
    {
      assign = make_extraction (VOIDmode, dest, -1, XEXP (XEXP (src, 0), 1),
				1, 1, 1, 0);
      return gen_rtx (SET, VOIDmode, assign, const0_rtx);
    }

  else if (GET_CODE (src) == AND && GET_CODE (XEXP (src, 0)) == SUBREG
	   && subreg_lowpart_p (XEXP (src, 0))
	   && (GET_MODE_SIZE (GET_MODE (XEXP (src, 0))) 
	       < GET_MODE_SIZE (GET_MODE (SUBREG_REG (XEXP (src, 0)))))
	   && GET_CODE (SUBREG_REG (XEXP (src, 0))) == ROTATE
	   && INTVAL (XEXP (SUBREG_REG (XEXP (src, 0)), 0)) == -2
	   && (rtx_equal_p (dest, XEXP (src, 1))
	       || rtx_equal_p (dest, get_last_value (XEXP (src, 1)))
	       || rtx_equal_p (get_last_value (dest), XEXP (src, 1))))
    {
      assign = make_extraction (VOIDmode, dest, -1,
				XEXP (SUBREG_REG (XEXP (src, 0)), 1),
				1, 1, 1, 0);
      return gen_rtx (SET, VOIDmode, assign, const0_rtx);
    }

  /* If SRC is (ior (ashift (const_int 1) POS DEST)), this is a set of a
     one-bit field.  */
  else if (GET_CODE (src) == IOR && GET_CODE (XEXP (src, 0)) == ASHIFT
	   && XEXP (XEXP (src, 0), 0) == const1_rtx
	   && (rtx_equal_p (dest, XEXP (src, 1))
	       || rtx_equal_p (dest, get_last_value (XEXP (src, 1)))
	       || rtx_equal_p (get_last_value (dest), XEXP (src, 1))))
    {
      assign = make_extraction (VOIDmode, dest, -1, XEXP (XEXP (src, 0), 1),
				1, 1, 1, 0);
      return gen_rtx (SET, VOIDmode, assign, const1_rtx);
    }

  /* The other case we handle is assignments into a constant-position
     field.  They look like (ior (and DEST C1) OTHER).  If C1 represents
     a mask that has all one bits except for a group of zero bits and
     OTHER is known to have zeros where C1 has ones, this is such an
     assignment.  Compute the position and length from C1.  Shift OTHER
     to the appropriate position, force it to the required mode, and
     make the extraction.  Check for the AND in both operands.  */

  if (GET_CODE (src) == IOR && GET_CODE (XEXP (src, 0)) == AND
      && GET_CODE (XEXP (XEXP (src, 0), 1)) == CONST_INT
      && (rtx_equal_p (XEXP (XEXP (src, 0), 0), dest)
	  || rtx_equal_p (XEXP (XEXP (src, 0), 0), get_last_value (dest))
	  || rtx_equal_p (get_last_value (XEXP (XEXP (src, 0), 1)), dest)))
    c1 = INTVAL (XEXP (XEXP (src, 0), 1)), other = XEXP (src, 1);
  else if (GET_CODE (src) == IOR && GET_CODE (XEXP (src, 1)) == AND
	   && GET_CODE (XEXP (XEXP (src, 1), 1)) == CONST_INT
	   && (rtx_equal_p (XEXP (XEXP (src, 1), 0), dest)
	       || rtx_equal_p (XEXP (XEXP (src, 1), 0), get_last_value (dest))
	       || rtx_equal_p (get_last_value (XEXP (XEXP (src, 1), 0)),
			       dest)))
    c1 = INTVAL (XEXP (XEXP (src, 1), 1)), other = XEXP (src, 0);
  else
    return x;

  pos = get_pos_from_mask (~c1, &len);
  if (pos < 0 || pos + len > GET_MODE_BITSIZE (GET_MODE (dest))
      || (GET_MODE_BITSIZE (GET_MODE (other)) <= HOST_BITS_PER_WIDE_INT
	  && (c1 & significant_bits (other, GET_MODE (other))) != 0))
    return x;

  assign = make_extraction (VOIDmode, dest, pos, NULL_RTX, len, 1, 1, 0);

  /* The mode to use for the source is the mode of the assignment, or of
     what is inside a possible STRICT_LOW_PART.  */
  mode = (GET_CODE (assign) == STRICT_LOW_PART 
	  ? GET_MODE (XEXP (assign, 0)) : GET_MODE (assign));

  /* Shift OTHER right POS places and make it the source, restricting it
     to the proper length and mode.  */

  src = force_to_mode (simplify_shift_const (NULL_RTX, LSHIFTRT,
					     GET_MODE (src), other, pos),
		       mode, len, dest);

  return gen_rtx_combine (SET, VOIDmode, assign, src);
}

/* See if X is of the form (+ (* a c) (* b c)) and convert to (* (+ a b) c)
   if so.  */

static rtx
apply_distributive_law (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  rtx lhs, rhs, other;
  rtx tem;
  enum rtx_code inner_code;

  /* The outer operation can only be one of the following:  */
  if (code != IOR && code != AND && code != XOR
      && code != PLUS && code != MINUS)
    return x;

  lhs = XEXP (x, 0), rhs = XEXP (x, 1);

  /* If either operand is a primitive we can't do anything, so get out fast. */
  if (GET_RTX_CLASS (GET_CODE (lhs)) == 'o'
      || GET_RTX_CLASS (GET_CODE (rhs)) == 'o')
    return x;

  lhs = expand_compound_operation (lhs);
  rhs = expand_compound_operation (rhs);
  inner_code = GET_CODE (lhs);
  if (inner_code != GET_CODE (rhs))
    return x;

  /* See if the inner and outer operations distribute.  */
  switch (inner_code)
    {
    case LSHIFTRT:
    case ASHIFTRT:
    case AND:
    case IOR:
      /* These all distribute except over PLUS.  */
      if (code == PLUS || code == MINUS)
	return x;
      break;

    case MULT:
      if (code != PLUS && code != MINUS)
	return x;
      break;

    case ASHIFT:
    case LSHIFT:
      /* These are also multiplies, so they distribute over everything.  */
      break;

    case SUBREG:
      /* Non-paradoxical SUBREGs distributes over all operations, provided
	 the inner modes and word numbers are the same, this is an extraction
	 of a low-order part, we don't convert an fp operation to int or
	 vice versa, and we would not be converting a single-word
	 operation into a multi-word operation.  The latter test is not
	 required, but it prevents generating unneeded multi-word operations.
	 Some of the previous tests are redundant given the latter test, but
	 are retained because they are required for correctness.

	 We produce the result slightly differently in this case.  */

      if (GET_MODE (SUBREG_REG (lhs)) != GET_MODE (SUBREG_REG (rhs))
	  || SUBREG_WORD (lhs) != SUBREG_WORD (rhs)
	  || ! subreg_lowpart_p (lhs)
	  || (GET_MODE_CLASS (GET_MODE (lhs))
	      != GET_MODE_CLASS (GET_MODE (SUBREG_REG (lhs))))
	  || (GET_MODE_SIZE (GET_MODE (lhs))
	      < GET_MODE_SIZE (GET_MODE (SUBREG_REG (lhs))))
	  || GET_MODE_SIZE (GET_MODE (SUBREG_REG (lhs))) > UNITS_PER_WORD)
	return x;

      tem = gen_binary (code, GET_MODE (SUBREG_REG (lhs)),
			SUBREG_REG (lhs), SUBREG_REG (rhs));
      return gen_lowpart_for_combine (GET_MODE (x), tem);

    default:
      return x;
    }

  /* Set LHS and RHS to the inner operands (A and B in the example
     above) and set OTHER to the common operand (C in the example).
     These is only one way to do this unless the inner operation is
     commutative.  */
  if (GET_RTX_CLASS (inner_code) == 'c'
      && rtx_equal_p (XEXP (lhs, 0), XEXP (rhs, 0)))
    other = XEXP (lhs, 0), lhs = XEXP (lhs, 1), rhs = XEXP (rhs, 1);
  else if (GET_RTX_CLASS (inner_code) == 'c'
	   && rtx_equal_p (XEXP (lhs, 0), XEXP (rhs, 1)))
    other = XEXP (lhs, 0), lhs = XEXP (lhs, 1), rhs = XEXP (rhs, 0);
  else if (GET_RTX_CLASS (inner_code) == 'c'
	   && rtx_equal_p (XEXP (lhs, 1), XEXP (rhs, 0)))
    other = XEXP (lhs, 1), lhs = XEXP (lhs, 0), rhs = XEXP (rhs, 1);
  else if (rtx_equal_p (XEXP (lhs, 1), XEXP (rhs, 1)))
    other = XEXP (lhs, 1), lhs = XEXP (lhs, 0), rhs = XEXP (rhs, 0);
  else
    return x;

  /* Form the new inner operation, seeing if it simplifies first.  */
  tem = gen_binary (code, GET_MODE (x), lhs, rhs);

  /* There is one exception to the general way of distributing:
     (a ^ b) | (a ^ c) -> (~a) & (b ^ c)  */
  if (code == XOR && inner_code == IOR)
    {
      inner_code = AND;
      other = gen_unary (NOT, GET_MODE (x), other);
    }

  /* We may be able to continuing distributing the result, so call
     ourselves recursively on the inner operation before forming the
     outer operation, which we return.  */
  return gen_binary (inner_code, GET_MODE (x),
		     apply_distributive_law (tem), other);
}

/* We have X, a logical `and' of VAROP with the constant CONSTOP, to be done
   in MODE.

   Return an equivalent form, if different from X.  Otherwise, return X.  If
   X is zero, we are to always construct the equivalent form.  */

static rtx
simplify_and_const_int (x, mode, varop, constop)
     rtx x;
     enum machine_mode mode;
     rtx varop;
     unsigned HOST_WIDE_INT constop;
{
  register enum machine_mode tmode;
  register rtx temp;
  unsigned HOST_WIDE_INT significant;

  /* There is a large class of optimizations based on the principle that
     some operations produce results where certain bits are known to be zero,
     and hence are not significant to the AND.  For example, if we have just
     done a left shift of one bit, the low-order bit is known to be zero and
     hence an AND with a mask of ~1 would not do anything.

     At the end of the following loop, we set:

     VAROP to be the item to be AND'ed with;
     CONSTOP to the constant value to AND it with.  */

  while (1)
    {
      /* If we ever encounter a mode wider than the host machine's widest
	 integer size, we can't compute the masks accurately, so give up.  */
      if (GET_MODE_BITSIZE (GET_MODE (varop)) > HOST_BITS_PER_WIDE_INT)
	break;

      /* Unless one of the cases below does a `continue',
	 a `break' will be executed to exit the loop.  */

      switch (GET_CODE (varop))
	{
	case CLOBBER:
	  /* If VAROP is a (clobber (const_int)), return it since we know
	     we are generating something that won't match. */
	  return varop;

#if ! BITS_BIG_ENDIAN
	case USE:
	  /* VAROP is a (use (mem ..)) that was made from a bit-field
	     extraction that spanned the boundary of the MEM.  If we are
	     now masking so it is within that boundary, we don't need the
	     USE any more.  */
	  if ((constop & ~ GET_MODE_MASK (GET_MODE (XEXP (varop, 0)))) == 0)
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  break;
#endif

	case SUBREG:
	  if (subreg_lowpart_p (varop)
	      /* We can ignore the effect this SUBREG if it narrows the mode
		 or, on machines where byte operations extend, if the
		 constant masks to zero all the bits the mode doesn't have.  */
	      && ((GET_MODE_SIZE (GET_MODE (varop))
		   < GET_MODE_SIZE (GET_MODE (SUBREG_REG (varop))))
#if defined(BYTE_LOADS_ZERO_EXTEND) || defined(BYTE_LOADS_SIGN_EXTEND)
		  || (0 == (constop
			    & GET_MODE_MASK (GET_MODE (varop))
			    & ~ GET_MODE_MASK (GET_MODE (SUBREG_REG (varop)))))
#endif
		  ))
	    {
	      varop = SUBREG_REG (varop);
	      continue;
	    }
	  break;

	case ZERO_EXTRACT:
	case SIGN_EXTRACT:
	case ZERO_EXTEND:
	case SIGN_EXTEND:
	  /* Try to expand these into a series of shifts and then work
	     with that result.  If we can't, for example, if the extract
	     isn't at a fixed position, give up.  */
	  temp = expand_compound_operation (varop);
	  if (temp != varop)
	    {
	      varop = temp;
	      continue;
	    }
	  break;

	case AND:
	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT)
	    {
	      constop &= INTVAL (XEXP (varop, 1));
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  break;

	case IOR:
	case XOR:
	  /* If VAROP is (ior (lshiftrt FOO C1) C2), try to commute the IOR and
	     LSHIFT so we end up with an (and (lshiftrt (ior ...) ...) ...)
	     operation which may be a bitfield extraction.  */

	  if (GET_CODE (XEXP (varop, 0)) == LSHIFTRT
	      && GET_CODE (XEXP (XEXP (varop, 0), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (varop, 0), 1)) >= 0
	      && INTVAL (XEXP (XEXP (varop, 0), 1)) < HOST_BITS_PER_WIDE_INT
	      && GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && (INTVAL (XEXP (varop, 1))
		  & ~ significant_bits (XEXP (varop, 0),
					GET_MODE (varop)) == 0))
	    {
	      temp = GEN_INT ((INTVAL (XEXP (varop, 1)) & constop)
			      << INTVAL (XEXP (XEXP (varop, 0), 1)));
	      temp = gen_binary (GET_CODE (varop), GET_MODE (varop),
				 XEXP (XEXP (varop, 0), 0), temp);
	      varop = gen_rtx_combine (LSHIFTRT, GET_MODE (varop),
				       temp, XEXP (varop, 1));
	      continue;
	    }

	  /* Apply the AND to both branches of the IOR or XOR, then try to
	     apply the distributive law.  This may eliminate operations 
	     if either branch can be simplified because of the AND.
	     It may also make some cases more complex, but those cases
	     probably won't match a pattern either with or without this.  */
	  return 
	    gen_lowpart_for_combine
	      (mode, apply_distributive_law
	       (gen_rtx_combine
		(GET_CODE (varop), GET_MODE (varop),
		 simplify_and_const_int (NULL_RTX, GET_MODE (varop),
					 XEXP (varop, 0), constop),
		 simplify_and_const_int (NULL_RTX, GET_MODE (varop),
					 XEXP (varop, 1), constop))));

	case NOT:
	  /* (and (not FOO)) is (and (xor FOO CONST_OP)) so if FOO is an
	     LSHIFTRT we can do the same as above.  */

	  if (GET_CODE (XEXP (varop, 0)) == LSHIFTRT
	      && GET_CODE (XEXP (XEXP (varop, 0), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (varop, 0), 1)) >= 0
	      && INTVAL (XEXP (XEXP (varop, 0), 1)) < HOST_BITS_PER_WIDE_INT)
	    {
	      temp = GEN_INT (constop << INTVAL (XEXP (XEXP (varop, 0), 1)));
	      temp = gen_binary (XOR, GET_MODE (varop),
				 XEXP (XEXP (varop, 0), 0), temp);
	      varop = gen_rtx_combine (LSHIFTRT, GET_MODE (varop),
				       temp, XEXP (XEXP (varop, 0), 1));
	      continue;
	    }
	  break;

	case ASHIFTRT:
	  /* If we are just looking for the sign bit, we don't need this
	     shift at all, even if it has a variable count.  */
	  if (constop == ((HOST_WIDE_INT) 1
			  << (GET_MODE_BITSIZE (GET_MODE (varop)) - 1)))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If this is a shift by a constant, get a mask that contains
	     those bits that are not copies of the sign bit.  We then have
	     two cases:  If CONSTOP only includes those bits, this can be
	     a logical shift, which may allow simplifications.  If CONSTOP
	     is a single-bit field not within those bits, we are requesting
	     a copy of the sign bit and hence can shift the sign bit to
	     the appropriate location.  */
	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && INTVAL (XEXP (varop, 1)) >= 0
	      && INTVAL (XEXP (varop, 1)) < HOST_BITS_PER_WIDE_INT)
	    {
	      int i = -1;

	      significant = GET_MODE_MASK (GET_MODE (varop));
	      significant >>= INTVAL (XEXP (varop, 1));

	      if ((constop & ~significant) == 0
		  || (i = exact_log2 (constop)) >= 0)
		{
		  varop = simplify_shift_const
		    (varop, LSHIFTRT, GET_MODE (varop), XEXP (varop, 0),
		     i < 0 ? INTVAL (XEXP (varop, 1))
		     : GET_MODE_BITSIZE (GET_MODE (varop)) - 1 - i);
		  if (GET_CODE (varop) != ASHIFTRT)
		    continue;
		}
	    }

	  /* If our mask is 1, convert this to a LSHIFTRT.  This can be done
	     even if the shift count isn't a constant.  */
	  if (constop == 1)
	    varop = gen_rtx_combine (LSHIFTRT, GET_MODE (varop),
				     XEXP (varop, 0), XEXP (varop, 1));
	  break;

	case NE:
	  /* (and (ne FOO 0) CONST) can be (and FOO CONST) if CONST is
	     included in STORE_FLAG_VALUE and FOO has no significant bits
	     not in CONST.  */
	  if ((constop & ~ STORE_FLAG_VALUE) == 0
	      && XEXP (varop, 0) == const0_rtx
	      && (significant_bits (XEXP (varop, 0), mode) & ~ constop) == 0)
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  break;

	case PLUS:
	  /* In (and (plus FOO C1) M), if M is a mask that just turns off
	     low-order bits (as in an alignment operation) and FOO is already
	     aligned to that boundary, we can convert remove this AND
	     and possibly the PLUS if it is now adding zero.  */
	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && exact_log2 (-constop) >= 0
	      && (significant_bits (XEXP (varop, 0), mode) & ~ constop) == 0)
	    {
	      varop = plus_constant (XEXP (varop, 0),
				     INTVAL (XEXP (varop, 1)) & constop);
	      constop = ~0;
	      break;
	    }

	  /* ... fall through ... */

	case MINUS:
	  /* In (and (plus (and FOO M1) BAR) M2), if M1 and M2 are one
	     less than powers of two and M2 is narrower than M1, we can
	     eliminate the inner AND.  This occurs when incrementing
	     bit fields.  */

	  if (GET_CODE (XEXP (varop, 0)) == ZERO_EXTRACT
	      || GET_CODE (XEXP (varop, 0)) == ZERO_EXTEND)
	    SUBST (XEXP (varop, 0),
		   expand_compound_operation (XEXP (varop, 0)));

	  if (GET_CODE (XEXP (varop, 0)) == AND
	      && GET_CODE (XEXP (XEXP (varop, 0), 1)) == CONST_INT
	      && exact_log2 (constop + 1) >= 0
	      && exact_log2 (INTVAL (XEXP (XEXP (varop, 0), 1)) + 1) >= 0
	      && (~ INTVAL (XEXP (XEXP (varop, 0), 1)) & constop) == 0)
	    SUBST (XEXP (varop, 0), XEXP (XEXP (varop, 0), 0));
	  break;
	}

      break;
    }

  /* If we have reached a constant, this whole thing is constant.  */
  if (GET_CODE (varop) == CONST_INT)
    return GEN_INT (constop & INTVAL (varop));

  /* See what bits are significant in VAROP.  */
  significant = significant_bits (varop, mode);

  /* Turn off all bits in the constant that are known to already be zero.
     Thus, if the AND isn't needed at all, we will have CONSTOP == SIGNIFICANT
     which is tested below.  */

  constop &= significant;

  /* If we don't have any bits left, return zero.  */
  if (constop == 0)
    return const0_rtx;

  /* Get VAROP in MODE.  Try to get a SUBREG if not.  Don't make a new SUBREG
     if we already had one (just check for the simplest cases).  */
  if (x && GET_CODE (XEXP (x, 0)) == SUBREG
      && GET_MODE (XEXP (x, 0)) == mode
      && SUBREG_REG (XEXP (x, 0)) == varop)
    varop = XEXP (x, 0);
  else
    varop = gen_lowpart_for_combine (mode, varop);

  /* If we can't make the SUBREG, try to return what we were given. */
  if (GET_CODE (varop) == CLOBBER)
    return x ? x : varop;

  /* If we are only masking insignificant bits, return VAROP.  */
  if (constop == significant)
    x = varop;

  /* Otherwise, return an AND.  See how much, if any, of X we can use.  */
  else if (x == 0 || GET_CODE (x) != AND || GET_MODE (x) != mode)
    x = gen_rtx_combine (AND, mode, varop, GEN_INT (constop));

  else
    {
      if (GET_CODE (XEXP (x, 1)) != CONST_INT
	  || INTVAL (XEXP (x, 1)) != constop)
	SUBST (XEXP (x, 1), GEN_INT (constop));

      SUBST (XEXP (x, 0), varop);
    }

  return x;
}

/* Given an expression, X, compute which bits in X can be non-zero.
   We don't care about bits outside of those defined in MODE.

   For most X this is simply GET_MODE_MASK (GET_MODE (MODE)), but if X is
   a shift, AND, or zero_extract, we can do better.  */

static unsigned HOST_WIDE_INT
significant_bits (x, mode)
     rtx x;
     enum machine_mode mode;
{
  unsigned HOST_WIDE_INT significant = GET_MODE_MASK (mode);
  unsigned HOST_WIDE_INT inner_sig;
  enum rtx_code code;
  int mode_width = GET_MODE_BITSIZE (mode);
  rtx tem;

  /* If X is wider than MODE, use its mode instead.  */
  if (GET_MODE_BITSIZE (GET_MODE (x)) > mode_width)
    {
      mode = GET_MODE (x);
      significant = GET_MODE_MASK (mode);
      mode_width = GET_MODE_BITSIZE (mode);
    }

  if (mode_width > HOST_BITS_PER_WIDE_INT)
    /* Our only callers in this case look for single bit values.  So
       just return the mode mask.  Those tests will then be false.  */
    return significant;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
#ifdef STACK_BOUNDARY
      /* If this is the stack pointer, we may know something about its
	 alignment.  If PUSH_ROUNDING is defined, it is possible for the
	 stack to be momentarily aligned only to that amount, so we pick
	 the least alignment.  */

      if (x == stack_pointer_rtx)
	{
	  int sp_alignment = STACK_BOUNDARY / BITS_PER_UNIT;

#ifdef PUSH_ROUNDING
	  sp_alignment = MIN (PUSH_ROUNDING (1), sp_alignment);
#endif

	  return significant & ~ (sp_alignment - 1);
	}
#endif

      /* If X is a register whose value we can find, use that value.  
	 Otherwise, use the previously-computed significant bits for this
	 register.  */

      tem = get_last_value (x);
      if (tem)
	return significant_bits (tem, mode);
      else if (significant_valid && reg_significant[REGNO (x)])
	return reg_significant[REGNO (x)] & significant;
      else
	return significant;

    case CONST_INT:
      return INTVAL (x);

#ifdef BYTE_LOADS_ZERO_EXTEND
    case MEM:
      /* In many, if not most, RISC machines, reading a byte from memory
	 zeros the rest of the register.  Noticing that fact saves a lot
	 of extra zero-extends.  */
      significant &= GET_MODE_MASK (GET_MODE (x));
      break;
#endif

#if STORE_FLAG_VALUE == 1
    case EQ:  case NE:
    case GT:  case GTU:
    case LT:  case LTU:
    case GE:  case GEU:
    case LE:  case LEU:

      if (GET_MODE_CLASS (mode) == MODE_INT)
	significant = 1;

      /* A comparison operation only sets the bits given by its mode.  The
	 rest are set undefined.  */
      if (GET_MODE_SIZE (GET_MODE (x)) < mode_width)
	significant |= (GET_MODE_MASK (mode) & ~ GET_MODE_MASK (GET_MODE (x)));
      break;
#endif

    case NEG:
      if (num_sign_bit_copies (XEXP (x, 0), GET_MODE (x))
	  == GET_MODE_BITSIZE (GET_MODE (x)))
	significant = 1;

      if (GET_MODE_SIZE (GET_MODE (x)) < mode_width)
	significant |= (GET_MODE_MASK (mode) & ~ GET_MODE_MASK (GET_MODE (x)));
      break;

    case ABS:
      if (num_sign_bit_copies (XEXP (x, 0), GET_MODE (x))
	  == GET_MODE_BITSIZE (GET_MODE (x)))
	significant = 1;
      break;

    case TRUNCATE:
      significant &= (significant_bits (XEXP (x, 0), mode)
		      & GET_MODE_MASK (mode));
      break;

    case ZERO_EXTEND:
      significant &= significant_bits (XEXP (x, 0), mode);
      if (GET_MODE (XEXP (x, 0)) != VOIDmode)
	significant &= GET_MODE_MASK (GET_MODE (XEXP (x, 0)));
      break;

    case SIGN_EXTEND:
      /* If the sign bit is known clear, this is the same as ZERO_EXTEND.
	 Otherwise, show all the bits in the outer mode but not the inner
	 may be non-zero.  */
      inner_sig = significant_bits (XEXP (x, 0), mode);
      if (GET_MODE (XEXP (x, 0)) != VOIDmode)
	{
	  inner_sig &= GET_MODE_MASK (GET_MODE (XEXP (x, 0)));
	  if (inner_sig &
	      (((HOST_WIDE_INT) 1
		<< (GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0))) - 1))))
	    inner_sig |= (GET_MODE_MASK (mode)
			  & ~ GET_MODE_MASK (GET_MODE (XEXP (x, 0))));
	}

      significant &= inner_sig;
      break;

    case AND:
      significant &= (significant_bits (XEXP (x, 0), mode)
		      & significant_bits (XEXP (x, 1), mode));
      break;

    case XOR:   case IOR:
    case UMIN:  case UMAX:  case SMIN:  case SMAX:
      significant &= (significant_bits (XEXP (x, 0), mode)
		      | significant_bits (XEXP (x, 1), mode));
      break;

    case PLUS:  case MINUS:
    case MULT:
    case DIV:   case UDIV:
    case MOD:   case UMOD:
      /* We can apply the rules of arithmetic to compute the number of
	 high- and low-order zero bits of these operations.  We start by
	 computing the width (position of the highest-order non-zero bit)
	 and the number of low-order zero bits for each value.  */
      {
	unsigned HOST_WIDE_INT sig0 = significant_bits (XEXP (x, 0), mode);
	unsigned HOST_WIDE_INT sig1 = significant_bits (XEXP (x, 1), mode);
	int width0 = floor_log2 (sig0) + 1;
	int width1 = floor_log2 (sig1) + 1;
	int low0 = floor_log2 (sig0 & -sig0);
	int low1 = floor_log2 (sig1 & -sig1);
	int op0_maybe_minusp = (sig0 & (1 << (mode_width - 1)));
	int op1_maybe_minusp = (sig1 & (1 << (mode_width - 1)));
	int result_width = mode_width;
	int result_low = 0;

	switch (code)
	  {
	  case PLUS:
	    result_width = MAX (width0, width1) + 1;
	    result_low = MIN (low0, low1);
	    break;
	  case MINUS:
	    result_low = MIN (low0, low1);
	    break;
	  case MULT:
	    result_width = width0 + width1;
	    result_low = low0 + low1;
	    break;
	  case DIV:
	    if (! op0_maybe_minusp && ! op1_maybe_minusp)
	      result_width = width0;
	    break;
	  case UDIV:
	    result_width = width0;
	    break;
	  case MOD:
	    if (! op0_maybe_minusp && ! op1_maybe_minusp)
	      result_width = MIN (width0, width1);
	    result_low = MIN (low0, low1);
	    break;
	  case UMOD:
	    result_width = MIN (width0, width1);
	    result_low = MIN (low0, low1);
	    break;
	  }

	if (result_width < mode_width)
	  significant &= ((HOST_WIDE_INT) 1 << result_width) - 1;

	if (result_low > 0)
	  significant &= ~ (((HOST_WIDE_INT) 1 << result_low) - 1);
      }
      break;

    case ZERO_EXTRACT:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT)
	significant &= ((HOST_WIDE_INT) 1 << INTVAL (XEXP (x, 1))) - 1;
      break;

    case SUBREG:
      /* If this is a SUBREG formed for a promoted variable that has
	 been zero-extended, we know that at least the high-order bits
	 are zero, though others might be too.  */

      if (SUBREG_PROMOTED_VAR_P (x) && SUBREG_PROMOTED_UNSIGNED_P (x))
	significant = (GET_MODE_MASK (GET_MODE (x))
		       & significant_bits (SUBREG_REG (x), GET_MODE (x)));

      /* If the inner mode is a single word for both the host and target
	 machines, we can compute this from which bits of the inner
	 object are known significant.  */
      if (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (x))) <= BITS_PER_WORD
	  && (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (x)))
	      <= HOST_BITS_PER_WIDE_INT))
	{
	  significant &= significant_bits (SUBREG_REG (x), mode);
#if ! defined(BYTE_LOADS_ZERO_EXTEND) && ! defined(BYTE_LOADS_SIGN_EXTEND)
	  /* On many CISC machines, accessing an object in a wider mode
	     causes the high-order bits to become undefined.  So they are
	     not known to be zero.  */
	  if (GET_MODE_SIZE (GET_MODE (x))
	      > GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	    significant |= (GET_MODE_MASK (GET_MODE (x))
			    & ~ GET_MODE_MASK (GET_MODE (SUBREG_REG (x))));
#endif
	}
      break;

    case ASHIFTRT:
    case LSHIFTRT:
    case ASHIFT:
    case LSHIFT:
    case ROTATE:
      /* The significant bits are in two classes: any bits within MODE
	 that aren't in GET_MODE (x) are always significant.  The rest of the
	 significant bits are those that are significant in the operand of
	 the shift when shifted the appropriate number of bits.  This
	 shows that high-order bits are cleared by the right shift and
	 low-order bits by left shifts.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0
	  && INTVAL (XEXP (x, 1)) < HOST_BITS_PER_WIDE_INT)
	{
	  enum machine_mode inner_mode = GET_MODE (x);
	  int width = GET_MODE_BITSIZE (inner_mode);
	  int count = INTVAL (XEXP (x, 1));
	  unsigned HOST_WIDE_INT mode_mask = GET_MODE_MASK (inner_mode);
	  unsigned HOST_WIDE_INT op_significant
	    = significant_bits (XEXP (x, 0), mode);
	  unsigned HOST_WIDE_INT inner = op_significant & mode_mask;
	  unsigned HOST_WIDE_INT outer = 0;

	  if (mode_width > width)
	    outer = (op_significant & significant & ~ mode_mask);

	  if (code == LSHIFTRT)
	    inner >>= count;
	  else if (code == ASHIFTRT)
	    {
	      inner >>= count;

	      /* If the sign bit was significant at before the shift, we
		 need to mark all the places it could have been copied to
		 by the shift significant.  */
	      if (inner & ((HOST_WIDE_INT) 1 << (width - 1 - count)))
		inner |= (((HOST_WIDE_INT) 1 << count) - 1) << (width - count);
	    }
	  else if (code == LSHIFT || code == ASHIFT)
	    inner <<= count;
	  else
	    inner = ((inner << (count % width)
		      | (inner >> (width - (count % width)))) & mode_mask);

	  significant &= (outer | inner);
	}
      break;

    case FFS:
      /* This is at most the number of bits in the mode.  */
      significant = ((HOST_WIDE_INT) 1 << (floor_log2 (mode_width) + 1)) - 1;
      break;

    case IF_THEN_ELSE:
      significant &= (significant_bits (XEXP (x, 1), mode)
		      | significant_bits (XEXP (x, 2), mode));
      break;
    }

  return significant;
}

/* Return the number of bits at the high-order end of X that are known to
   be equal to the sign bit.  This number will always be between 1 and
   the number of bits in the mode of X.  MODE is the mode to be used
   if X is VOIDmode.  */

static int
num_sign_bit_copies (x, mode)
     rtx x;
     enum machine_mode mode;
{
  enum rtx_code code = GET_CODE (x);
  int bitwidth;
  int num0, num1, result;
  unsigned HOST_WIDE_INT sig;
  rtx tem;

  /* If we weren't given a mode, use the mode of X.  If the mode is still
     VOIDmode, we don't know anything.  */

  if (mode == VOIDmode)
    mode = GET_MODE (x);

  if (mode == VOIDmode)
    return 1;

  bitwidth = GET_MODE_BITSIZE (mode);

  switch (code)
    {
    case REG:
      if (significant_valid && reg_sign_bit_copies[REGNO (x)] != 0)
	return reg_sign_bit_copies[REGNO (x)];

      tem =  get_last_value (x);
      if (tem != 0)
	return num_sign_bit_copies (tem, mode);
      break;

#ifdef BYTE_LOADS_SIGN_EXTEND
    case MEM:
      /* Some RISC machines sign-extend all loads of smaller than a word.  */
      return MAX (1, bitwidth - GET_MODE_BITSIZE (GET_MODE (x)) + 1);
#endif

    case CONST_INT:
      /* If the constant is negative, take its 1's complement and remask.
	 Then see how many zero bits we have.  */
      sig = INTVAL (x) & GET_MODE_MASK (mode);
      if (bitwidth <= HOST_BITS_PER_WIDE_INT
	  && (sig & ((HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	sig = (~ sig) & GET_MODE_MASK (mode);

      return (sig == 0 ? bitwidth : bitwidth - floor_log2 (sig) - 1);

    case SUBREG:
      /* If this is a SUBREG for a promoted object that is sign-extended
	 and we are looking at it in a wider mode, we know that at least the
	 high-order bits are known to be sign bit copies.  */

      if (SUBREG_PROMOTED_VAR_P (x) && ! SUBREG_PROMOTED_UNSIGNED_P (x))
	return (GET_MODE_BITSIZE (mode) - GET_MODE_BITSIZE (GET_MODE (x))
		+ num_sign_bit_copies (SUBREG_REG (x), GET_MODE (x)));

      /* For a smaller object, just ignore the high bits. */
      if (bitwidth <= GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (x))))
	{
	  num0 = num_sign_bit_copies (SUBREG_REG (x), VOIDmode);
	  return MAX (1, (num0
			  - (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (x)))
			     - bitwidth)));
	}

#if defined(BYTE_LOADS_ZERO_EXTEND) || defined(BYTE_LOADS_SIGN_EXTEND)
      /* For paradoxical SUBREGs, just look inside since, on machines with
	 one of these defined, we assume that operations are actually 
	 performed on the full register.  Note that we are passing MODE
	 to the recursive call, so the number of sign bit copies will
	 remain relative to that mode, not the inner mode.  */

      if (GET_MODE_SIZE (GET_MODE (x))
	  > GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	return num_sign_bit_copies (SUBREG_REG (x), mode);
#endif

      break;

    case SIGN_EXTRACT:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	return MAX (1, bitwidth - INTVAL (XEXP (x, 1)));
      break;

    case SIGN_EXTEND: 
      return (bitwidth - GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0)))
	      + num_sign_bit_copies (XEXP (x, 0), VOIDmode));

    case TRUNCATE:
      /* For a smaller object, just ignore the high bits. */
      num0 = num_sign_bit_copies (XEXP (x, 0), VOIDmode);
      return MAX (1, (num0 - (GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0)))
			      - bitwidth)));

    case NOT:
      return num_sign_bit_copies (XEXP (x, 0), mode);

    case ROTATE:       case ROTATERT:
      /* If we are rotating left by a number of bits less than the number
	 of sign bit copies, we can just subtract that amount from the
	 number.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) >= 0 && INTVAL (XEXP (x, 1)) < bitwidth)
	{
	  num0 = num_sign_bit_copies (XEXP (x, 0), mode);
	  return MAX (1, num0 - (code == ROTATE ? INTVAL (XEXP (x, 1))
				 : bitwidth - INTVAL (XEXP (x, 1))));
	}
      break;

    case NEG:
      /* In general, this subtracts one sign bit copy.  But if the value
	 is known to be positive, the number of sign bit copies is the
	 same as that of the input.  Finally, if the input has just one
	 significant bit, all the bits are copies of the sign bit.  */
      sig = significant_bits (XEXP (x, 0), mode);
      if (sig == 1)
	return bitwidth;

      num0 = num_sign_bit_copies (XEXP (x, 0), mode);
      if (num0 > 1
	  && bitwidth <= HOST_BITS_PER_WIDE_INT
	  && (((HOST_WIDE_INT) 1 << (bitwidth - 1)) & sig))
	num0--;

      return num0;

    case IOR:   case AND:   case XOR:
    case SMIN:  case SMAX:  case UMIN:  case UMAX:
      /* Logical operations will preserve the number of sign-bit copies.
	 MIN and MAX operations always return one of the operands.  */
      num0 = num_sign_bit_copies (XEXP (x, 0), mode);
      num1 = num_sign_bit_copies (XEXP (x, 1), mode);
      return MIN (num0, num1);

    case PLUS:  case MINUS:
      /* For addition and subtraction, we can have a 1-bit carry.  However,
	 if we are subtracting 1 from a positive number, there will not
	 be such a carry.  Furthermore, if the positive number is known to
	 be 0 or 1, we know the result is either -1 or 0.  */

      if (code == PLUS && XEXP (x, 1) == constm1_rtx
	  /* Don't do this if XEXP (x, 0) is a paradoxical subreg
	     because in principle we don't know what the high bits are.  */
	  && !(GET_CODE (XEXP (x, 0)) == SUBREG
	       && (GET_MODE_SIZE (GET_MODE (XEXP (XEXP (x, 0), 0)))
		   < GET_MODE_SIZE (GET_MODE (XEXP (x, 0))))))
	{
	  sig = significant_bits (XEXP (x, 0), mode);
	  if ((((HOST_WIDE_INT) 1 << (bitwidth - 1)) & sig) == 0)
	    return (sig == 1 || sig == 0 ? bitwidth
		    : bitwidth - floor_log2 (sig) - 1);
	}

      num0 = num_sign_bit_copies (XEXP (x, 0), mode);
      num1 = num_sign_bit_copies (XEXP (x, 1), mode);
      return MAX (1, MIN (num0, num1) - 1);
      
    case MULT:
      /* The number of bits of the product is the sum of the number of
	 bits of both terms.  However, unless one of the terms if known
	 to be positive, we must allow for an additional bit since negating
	 a negative number can remove one sign bit copy.  */

      num0 = num_sign_bit_copies (XEXP (x, 0), mode);
      num1 = num_sign_bit_copies (XEXP (x, 1), mode);

      result = bitwidth - (bitwidth - num0) - (bitwidth - num1);
      if (result > 0
	  && bitwidth <= HOST_BITS_PER_INT
	  && ((significant_bits (XEXP (x, 0), mode)
	       & ((HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	  && (significant_bits (XEXP (x, 1), mode)
	      & ((HOST_WIDE_INT) 1 << (bitwidth - 1)) != 0))
	result--;

      return MAX (1, result);

    case UDIV:
      /* The result must be <= the first operand.  */
      return num_sign_bit_copies (XEXP (x, 0), mode);

    case UMOD:
      /* The result must be <= the scond operand.  */
      return num_sign_bit_copies (XEXP (x, 1), mode);

    case DIV:
      /* Similar to unsigned division, except that we have to worry about
	 the case where the divisor is negative, in which case we have
	 to add 1.  */
      result = num_sign_bit_copies (XEXP (x, 0), mode);
      if (result > 1
	  && bitwidth <= HOST_BITS_PER_WIDE_INT
	  && (significant_bits (XEXP (x, 1), mode)
	      & ((HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	result --;

      return result;

    case MOD:
      result = num_sign_bit_copies (XEXP (x, 1), mode);
      if (result > 1
	  && bitwidth <= HOST_BITS_PER_WIDE_INT
	  && (significant_bits (XEXP (x, 1), mode)
	      & ((HOST_WIDE_INT) 1 << (bitwidth - 1))) != 0)
	result --;

      return result;

    case ASHIFTRT:
      /* Shifts by a constant add to the number of bits equal to the
	 sign bit.  */
      num0 = num_sign_bit_copies (XEXP (x, 0), mode);
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) > 0)
	num0 = MIN (bitwidth, num0 + INTVAL (XEXP (x, 1)));

      return num0;

    case ASHIFT:
    case LSHIFT:
      /* Left shifts destroy copies.  */
      if (GET_CODE (XEXP (x, 1)) != CONST_INT
	  || INTVAL (XEXP (x, 1)) < 0
	  || INTVAL (XEXP (x, 1)) >= bitwidth)
	return 1;

      num0 = num_sign_bit_copies (XEXP (x, 0), mode);
      return MAX (1, num0 - INTVAL (XEXP (x, 1)));

    case IF_THEN_ELSE:
      num0 = num_sign_bit_copies (XEXP (x, 1), mode);
      num1 = num_sign_bit_copies (XEXP (x, 2), mode);
      return MIN (num0, num1);

#if STORE_FLAG_VALUE == -1
    case EQ:  case NE:  case GE:  case GT:  case LE:  case LT:
    case GEU: case GTU: case LEU: case LTU:
      return bitwidth;
#endif
    }

  /* If we haven't been able to figure it out by one of the above rules,
     see if some of the high-order bits are known to be zero.  If so,
     count those bits and return one less than that amount.  If we can't
     safely compute the mask for this mode, always return BITWIDTH.  */

  if (bitwidth > HOST_BITS_PER_WIDE_INT)
    return 1;

  sig = significant_bits (x, mode);
  return sig == GET_MODE_MASK (mode) ? 1 : bitwidth - floor_log2 (sig) - 1;
}

/* Return the number of "extended" bits there are in X, when interpreted
   as a quantity in MODE whose signedness is indicated by UNSIGNEDP.  For
   unsigned quantities, this is the number of high-order zero bits.
   For signed quantities, this is the number of copies of the sign bit
   minus 1.  In both case, this function returns the number of "spare"
   bits.  For example, if two quantities for which this function returns
   at least 1 are added, the addition is known not to overflow.

   This function will always return 0 unless called during combine, which
   implies that it must be called from a define_split.  */

int
extended_count (x, mode, unsignedp)
     rtx x;
     enum machine_mode mode;
     int unsignedp;
{
  if (significant_valid == 0)
    return 0;

  return (unsignedp
	  ? (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	     && (GET_MODE_BITSIZE (mode) - 1
		 - floor_log2 (significant_bits (x, mode))))
	  : num_sign_bit_copies (x, mode) - 1);
}

/* This function is called from `simplify_shift_const' to merge two
   outer operations.  Specifically, we have already found that we need
   to perform operation *POP0 with constant *PCONST0 at the outermost
   position.  We would now like to also perform OP1 with constant CONST1
   (with *POP0 being done last).

   Return 1 if we can do the operation and update *POP0 and *PCONST0 with
   the resulting operation.  *PCOMP_P is set to 1 if we would need to 
   complement the innermost operand, otherwise it is unchanged.

   MODE is the mode in which the operation will be done.  No bits outside
   the width of this mode matter.  It is assumed that the width of this mode
   is smaller than or equal to HOST_BITS_PER_WIDE_INT.

   If *POP0 or OP1 are NIL, it means no operation is required.  Only NEG, PLUS,
   IOR, XOR, and AND are supported.  We may set *POP0 to SET if the proper
   result is simply *PCONST0.

   If the resulting operation cannot be expressed as one operation, we
   return 0 and do not change *POP0, *PCONST0, and *PCOMP_P.  */

static int
merge_outer_ops (pop0, pconst0, op1, const1, mode, pcomp_p)
     enum rtx_code *pop0;
     HOST_WIDE_INT *pconst0;
     enum rtx_code op1;
     HOST_WIDE_INT const1;
     enum machine_mode mode;
     int *pcomp_p;
{
  enum rtx_code op0 = *pop0;
  HOST_WIDE_INT const0 = *pconst0;

  const0 &= GET_MODE_MASK (mode);
  const1 &= GET_MODE_MASK (mode);

  /* If OP0 is an AND, clear unimportant bits in CONST1.  */
  if (op0 == AND)
    const1 &= const0;

  /* If OP0 or OP1 is NIL, this is easy.  Similarly if they are the same or
     if OP0 is SET.  */

  if (op1 == NIL || op0 == SET)
    return 1;

  else if (op0 == NIL)
    op0 = op1, const0 = const1;

  else if (op0 == op1)
    {
      switch (op0)
	{
	case AND:
	  const0 &= const1;
	  break;
	case IOR:
	  const0 |= const1;
	  break;
	case XOR:
	  const0 ^= const1;
	  break;
	case PLUS:
	  const0 += const1;
	  break;
	case NEG:
	  op0 = NIL;
	  break;
	}
    }

  /* Otherwise, if either is a PLUS or NEG, we can't do anything.  */
  else if (op0 == PLUS || op1 == PLUS || op0 == NEG || op1 == NEG)
    return 0;

  /* If the two constants aren't the same, we can't do anything.  The
     remaining six cases can all be done.  */
  else if (const0 != const1)
    return 0;

  else
    switch (op0)
      {
      case IOR:
	if (op1 == AND)
	  /* (a & b) | b == b */
	  op0 = SET;
	else /* op1 == XOR */
	  /* (a ^ b) | b == a | b */
	  ;
	break;

      case XOR:
	if (op1 == AND)
	  /* (a & b) ^ b == (~a) & b */
	  op0 = AND, *pcomp_p = 1;
	else /* op1 == IOR */
	  /* (a | b) ^ b == a & ~b */
	  op0 = AND, *pconst0 = ~ const0;
	break;

      case AND:
	if (op1 == IOR)
	  /* (a | b) & b == b */
	op0 = SET;
	else /* op1 == XOR */
	  /* (a ^ b) & b) == (~a) & b */
	  *pcomp_p = 1;
	break;
      }

  /* Check for NO-OP cases.  */
  const0 &= GET_MODE_MASK (mode);
  if (const0 == 0
      && (op0 == IOR || op0 == XOR || op0 == PLUS))
    op0 = NIL;
  else if (const0 == 0 && op0 == AND)
    op0 = SET;
  else if (const0 == GET_MODE_MASK (mode) && op0 == AND)
    op0 = NIL;

  *pop0 = op0;
  *pconst0 = const0;

  return 1;
}

/* Simplify a shift of VAROP by COUNT bits.  CODE says what kind of shift.
   The result of the shift is RESULT_MODE.  X, if non-zero, is an expression
   that we started with.

   The shift is normally computed in the widest mode we find in VAROP, as
   long as it isn't a different number of words than RESULT_MODE.  Exceptions
   are ASHIFTRT and ROTATE, which are always done in their original mode,  */

static rtx
simplify_shift_const (x, code, result_mode, varop, count)
     rtx x;
     enum rtx_code code;
     enum machine_mode result_mode;
     rtx varop;
     int count;
{
  enum rtx_code orig_code = code;
  int orig_count = count;
  enum machine_mode mode = result_mode;
  enum machine_mode shift_mode, tmode;
  int mode_words
    = (GET_MODE_SIZE (mode) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD;
  /* We form (outer_op (code varop count) (outer_const)).  */
  enum rtx_code outer_op = NIL;
  HOST_WIDE_INT outer_const;
  rtx const_rtx;
  int complement_p = 0;
  rtx new;

  /* If we were given an invalid count, don't do anything except exactly
     what was requested.  */

  if (count < 0 || count > GET_MODE_BITSIZE (mode))
    {
      if (x)
	return x;

      return gen_rtx (code, mode, varop, GEN_INT (count));
    }

  /* Unless one of the branches of the `if' in this loop does a `continue',
     we will `break' the loop after the `if'.  */

  while (count != 0)
    {
      /* If we have an operand of (clobber (const_int 0)), just return that
	 value.  */
      if (GET_CODE (varop) == CLOBBER)
	return varop;

      /* If we discovered we had to complement VAROP, leave.  Making a NOT
	 here would cause an infinite loop.  */
      if (complement_p)
	break;

      /* Convert ROTATETRT to ROTATE.  */
      if (code == ROTATERT)
	code = ROTATE, count = GET_MODE_BITSIZE (result_mode) - count;

      /* Canonicalize LSHIFT to ASHIFT.  */
      if (code == LSHIFT)
	code = ASHIFT;

      /* We need to determine what mode we will do the shift in.  If the
	 shift is a ASHIFTRT or ROTATE, we must always do it in the mode it
	 was originally done in.  Otherwise, we can do it in MODE, the widest
	 mode encountered. */
      shift_mode = (code == ASHIFTRT || code == ROTATE ? result_mode : mode);

      /* Handle cases where the count is greater than the size of the mode
	 minus 1.  For ASHIFT, use the size minus one as the count (this can
	 occur when simplifying (lshiftrt (ashiftrt ..))).  For rotates,
	 take the count modulo the size.  For other shifts, the result is
	 zero.

	 Since these shifts are being produced by the compiler by combining
	 multiple operations, each of which are defined, we know what the
	 result is supposed to be.  */
	 
      if (count > GET_MODE_BITSIZE (shift_mode) - 1)
	{
	  if (code == ASHIFTRT)
	    count = GET_MODE_BITSIZE (shift_mode) - 1;
	  else if (code == ROTATE || code == ROTATERT)
	    count %= GET_MODE_BITSIZE (shift_mode);
	  else
	    {
	      /* We can't simply return zero because there may be an
		 outer op.  */
	      varop = const0_rtx;
	      count = 0;
	      break;
	    }
	}

      /* Negative counts are invalid and should not have been made (a
	 programmer-specified negative count should have been handled
	 above). */
      else if (count < 0)
	abort ();

      /* An arithmetic right shift of a quantity known to be -1 or 0
	 is a no-op.  */
      if (code == ASHIFTRT
	  && (num_sign_bit_copies (varop, shift_mode)
	      == GET_MODE_BITSIZE (shift_mode)))
	{
	  count = 0;
	  break;
	}

      /* We simplify the tests below and elsewhere by converting
	 ASHIFTRT to LSHIFTRT if we know the sign bit is clear.
	 `make_compound_operation' will convert it to a ASHIFTRT for
	 those machines (such as Vax) that don't have a LSHIFTRT.  */
      if (GET_MODE_BITSIZE (shift_mode) <= HOST_BITS_PER_WIDE_INT
	  && code == ASHIFTRT
	  && ((significant_bits (varop, shift_mode)
	       & ((HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (shift_mode) - 1)))
	      == 0))
	code = LSHIFTRT;

      switch (GET_CODE (varop))
	{
	case SIGN_EXTEND:
	case ZERO_EXTEND:
	case SIGN_EXTRACT:
	case ZERO_EXTRACT:
	  new = expand_compound_operation (varop);
	  if (new != varop)
	    {
	      varop = new;
	      continue;
	    }
	  break;

	case MEM:
	  /* If we have (xshiftrt (mem ...) C) and C is MODE_WIDTH
	     minus the width of a smaller mode, we can do this with a
	     SIGN_EXTEND or ZERO_EXTEND from the narrower memory location.  */
	  if ((code == ASHIFTRT || code == LSHIFTRT)
	      && ! mode_dependent_address_p (XEXP (varop, 0))
	      && ! MEM_VOLATILE_P (varop)
	      && (tmode = mode_for_size (GET_MODE_BITSIZE (mode) - count,
					 MODE_INT, 1)) != BLKmode)
	    {
#if BYTES_BIG_ENDIAN
	      new = gen_rtx (MEM, tmode, XEXP (varop, 0));
#else
	      new = gen_rtx (MEM, tmode,
			     plus_constant (XEXP (varop, 0),
					    count / BITS_PER_UNIT));
	      RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (varop);
	      MEM_VOLATILE_P (new) = MEM_VOLATILE_P (varop);
	      MEM_IN_STRUCT_P (new) = MEM_IN_STRUCT_P (varop);
#endif
	      varop = gen_rtx_combine (code == ASHIFTRT ? SIGN_EXTEND
				       : ZERO_EXTEND, mode, new);
	      count = 0;
	      continue;
	    }
	  break;

	case USE:
	  /* Similar to the case above, except that we can only do this if
	     the resulting mode is the same as that of the underlying
	     MEM and adjust the address depending on the *bits* endianness
	     because of the way that bit-field extract insns are defined.  */
	  if ((code == ASHIFTRT || code == LSHIFTRT)
	      && (tmode = mode_for_size (GET_MODE_BITSIZE (mode) - count,
					 MODE_INT, 1)) != BLKmode
	      && tmode == GET_MODE (XEXP (varop, 0)))
	    {
#if BITS_BIG_ENDIAN
	      new = XEXP (varop, 0);
#else
	      new = copy_rtx (XEXP (varop, 0));
	      SUBST (XEXP (new, 0), 
		     plus_constant (XEXP (new, 0),
				    count / BITS_PER_UNIT));
#endif

	      varop = gen_rtx_combine (code == ASHIFTRT ? SIGN_EXTEND
				       : ZERO_EXTEND, mode, new);
	      count = 0;
	      continue;
	    }
	  break;

	case SUBREG:
	  /* If VAROP is a SUBREG, strip it as long as the inner operand has
	     the same number of words as what we've seen so far.  Then store
	     the widest mode in MODE.  */
	  if (subreg_lowpart_p (varop)
	      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (varop)))
		  > GET_MODE_SIZE (GET_MODE (varop)))
	      && (((GET_MODE_SIZE (GET_MODE (SUBREG_REG (varop)))
		    + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD)
		  == mode_words))
	    {
	      varop = SUBREG_REG (varop);
	      if (GET_MODE_SIZE (GET_MODE (varop)) > GET_MODE_SIZE (mode))
		mode = GET_MODE (varop);
	      continue;
	    }
	  break;

	case MULT:
	  /* Some machines use MULT instead of ASHIFT because MULT
	     is cheaper.  But it is still better on those machines to
	     merge two shifts into one.  */
	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && exact_log2 (INTVAL (XEXP (varop, 1))) >= 0)
	    {
	      varop = gen_binary (ASHIFT, GET_MODE (varop), XEXP (varop, 0),
				  GEN_INT (exact_log2 (INTVAL (XEXP (varop, 1)))));;
	      continue;
	    }
	  break;

	case UDIV:
	  /* Similar, for when divides are cheaper.  */
	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && exact_log2 (INTVAL (XEXP (varop, 1))) >= 0)
	    {
	      varop = gen_binary (LSHIFTRT, GET_MODE (varop), XEXP (varop, 0),
				  GEN_INT (exact_log2 (INTVAL (XEXP (varop, 1)))));
	      continue;
	    }
	  break;

	case ASHIFTRT:
	  /* If we are extracting just the sign bit of an arithmetic right 
	     shift, that shift is not needed.  */
	  if (code == LSHIFTRT && count == GET_MODE_BITSIZE (result_mode) - 1)
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* ... fall through ... */

	case LSHIFTRT:
	case ASHIFT:
	case LSHIFT:
	case ROTATE:
	  /* Here we have two nested shifts.  The result is usually the
	     AND of a new shift with a mask.  We compute the result below.  */
	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && INTVAL (XEXP (varop, 1)) >= 0
	      && INTVAL (XEXP (varop, 1)) < GET_MODE_BITSIZE (GET_MODE (varop))
	      && GET_MODE_BITSIZE (result_mode) <= HOST_BITS_PER_WIDE_INT
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	    {
	      enum rtx_code first_code = GET_CODE (varop);
	      int first_count = INTVAL (XEXP (varop, 1));
	      unsigned HOST_WIDE_INT mask;
	      rtx mask_rtx;
	      rtx inner;

	      if (first_code == LSHIFT)
		first_code = ASHIFT;

	      /* We have one common special case.  We can't do any merging if
		 the inner code is an ASHIFTRT of a smaller mode.  However, if
		 we have (ashift:M1 (subreg:M1 (ashiftrt:M2 FOO C1) 0) C2)
		 with C2 == GET_MODE_BITSIZE (M1) - GET_MODE_BITSIZE (M2),
		 we can convert it to
		 (ashiftrt:M1 (ashift:M1 (and:M1 (subreg:M1 FOO 0 C2) C3) C1).
		 This simplifies certain SIGN_EXTEND operations.  */
	      if (code == ASHIFT && first_code == ASHIFTRT
		  && (GET_MODE_BITSIZE (result_mode)
		      - GET_MODE_BITSIZE (GET_MODE (varop))) == count)
		{
		  /* C3 has the low-order C1 bits zero.  */
		  
		  mask = (GET_MODE_MASK (mode)
			  & ~ (((HOST_WIDE_INT) 1 << first_count) - 1));

		  varop = simplify_and_const_int (NULL_RTX, result_mode,
						  XEXP (varop, 0), mask);
		  varop = simplify_shift_const (NULL_RTX, ASHIFT, result_mode,
						varop, count);
		  count = first_count;
		  code = ASHIFTRT;
		  continue;
		}
	      
	      /* If this was (ashiftrt (ashift foo C1) C2) and FOO has more
		 than C1 high-order bits equal to the sign bit, we can convert
		 this to either an ASHIFT or a ASHIFTRT depending on the
		 two counts. 

		 We cannot do this if VAROP's mode is not SHIFT_MODE.  */

	      if (code == ASHIFTRT && first_code == ASHIFT
		  && GET_MODE (varop) == shift_mode
		  && (num_sign_bit_copies (XEXP (varop, 0), shift_mode)
		      > first_count))
		{
		  count -= first_count;
		  if (count < 0)
		    count = - count, code = ASHIFT;
		  varop = XEXP (varop, 0);
		  continue;
		}

	      /* There are some cases we can't do.  If CODE is ASHIFTRT,
		 we can only do this if FIRST_CODE is also ASHIFTRT.

		 We can't do the case when CODE is ROTATE and FIRST_CODE is
		 ASHIFTRT.

		 If the mode of this shift is not the mode of the outer shift,
		 we can't do this if either shift is ASHIFTRT or ROTATE.

		 Finally, we can't do any of these if the mode is too wide
		 unless the codes are the same.

		 Handle the case where the shift codes are the same
		 first.  */

	      if (code == first_code)
		{
		  if (GET_MODE (varop) != result_mode
		      && (code == ASHIFTRT || code == ROTATE))
		    break;

		  count += first_count;
		  varop = XEXP (varop, 0);
		  continue;
		}

	      if (code == ASHIFTRT
		  || (code == ROTATE && first_code == ASHIFTRT)
		  || GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT
		  || (GET_MODE (varop) != result_mode
		      && (first_code == ASHIFTRT || first_code == ROTATE
			  || code == ROTATE)))
		break;

	      /* To compute the mask to apply after the shift, shift the
		 significant bits of the inner shift the same way the 
		 outer shift will.  */

	      mask_rtx = GEN_INT (significant_bits (varop, GET_MODE (varop)));

	      mask_rtx
		= simplify_binary_operation (code, result_mode, mask_rtx,
					     GEN_INT (count));
				  
	      /* Give up if we can't compute an outer operation to use.  */
	      if (mask_rtx == 0
		  || GET_CODE (mask_rtx) != CONST_INT
		  || ! merge_outer_ops (&outer_op, &outer_const, AND,
					INTVAL (mask_rtx),
					result_mode, &complement_p))
		break;

	      /* If the shifts are in the same direction, we add the
		 counts.  Otherwise, we subtract them.  */
	      if ((code == ASHIFTRT || code == LSHIFTRT)
		  == (first_code == ASHIFTRT || first_code == LSHIFTRT))
		count += first_count;
	      else
		count -= first_count;

	      /* If COUNT is positive, the new shift is usually CODE, 
		 except for the two exceptions below, in which case it is
		 FIRST_CODE.  If the count is negative, FIRST_CODE should
		 always be used  */
	      if (count > 0
		  && ((first_code == ROTATE && code == ASHIFT)
		      || (first_code == ASHIFTRT && code == LSHIFTRT)))
		code = first_code;
	      else if (count < 0)
		code = first_code, count = - count;

	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If we have (A << B << C) for any shift, we can convert this to
	     (A << C << B).  This wins if A is a constant.  Only try this if
	     B is not a constant.  */

	  else if (GET_CODE (varop) == code
		   && GET_CODE (XEXP (varop, 1)) != CONST_INT
		   && 0 != (new
			    = simplify_binary_operation (code, mode,
							 XEXP (varop, 0),
							 GEN_INT (count))))
	    {
	      varop = gen_rtx_combine (code, mode, new, XEXP (varop, 1));
	      count = 0;
	      continue;
	    }
	  break;

	case NOT:
	  /* Make this fit the case below.  */
	  varop = gen_rtx_combine (XOR, mode, XEXP (varop, 0),
				   GEN_INT (GET_MODE_MASK (mode)));
	  continue;

	case IOR:
	case AND:
	case XOR:
	  /* If we have (xshiftrt (ior (plus X (const_int -1)) X) C)
	     with C the size of VAROP - 1 and the shift is logical if
	     STORE_FLAG_VALUE is 1 and arithmetic if STORE_FLAG_VALUE is -1,
	     we have an (le X 0) operation.   If we have an arithmetic shift
	     and STORE_FLAG_VALUE is 1 or we have a logical shift with
	     STORE_FLAG_VALUE of -1, we have a (neg (le X 0)) operation.  */

	  if (GET_CODE (varop) == IOR && GET_CODE (XEXP (varop, 0)) == PLUS
	      && XEXP (XEXP (varop, 0), 1) == constm1_rtx
	      && (STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	      && (code == LSHIFTRT || code == ASHIFTRT)
	      && count == GET_MODE_BITSIZE (GET_MODE (varop)) - 1
	      && rtx_equal_p (XEXP (XEXP (varop, 0), 0), XEXP (varop, 1)))
	    {
	      count = 0;
	      varop = gen_rtx_combine (LE, GET_MODE (varop), XEXP (varop, 1),
				       const0_rtx);

	      if (STORE_FLAG_VALUE == 1 ? code == ASHIFTRT : code == LSHIFTRT)
		varop = gen_rtx_combine (NEG, GET_MODE (varop), varop);

	      continue;
	    }

	  /* If we have (shift (logical)), move the logical to the outside
	     to allow it to possibly combine with another logical and the
	     shift to combine with another shift.  This also canonicalizes to
	     what a ZERO_EXTRACT looks like.  Also, some machines have
	     (and (shift)) insns.  */

	  if (GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && (new = simplify_binary_operation (code, result_mode,
						   XEXP (varop, 1),
						   GEN_INT (count))) != 0
	      && merge_outer_ops (&outer_op, &outer_const, GET_CODE (varop),
				  INTVAL (new), result_mode, &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If we can't do that, try to simplify the shift in each arm of the
	     logical expression, make a new logical expression, and apply
	     the inverse distributive law.  */
	  {
	    rtx lhs = simplify_shift_const (NULL_RTX, code, result_mode,
					    XEXP (varop, 0), count);
	    rtx rhs = simplify_shift_const (NULL_RTX, code, result_mode,
					    XEXP (varop, 1), count);

	    varop = gen_binary (GET_CODE (varop), result_mode, lhs, rhs);
	    varop = apply_distributive_law (varop);

	    count = 0;
	  }
	  break;

	case EQ:
	  /* convert (lshift (eq FOO 0) C) to (xor FOO 1) if STORE_FLAG_VALUE
	     says that the sign bit can be tested, FOO has mode MODE, C is
	     GET_MODE_BITSIZE (MODE) - 1, and FOO has only the low-order bit
	     significant.  */
	  if (code == LSHIFT
	      && XEXP (varop, 1) == const0_rtx
	      && GET_MODE (XEXP (varop, 0)) == result_mode
	      && count == GET_MODE_BITSIZE (result_mode) - 1
	      && GET_MODE_BITSIZE (result_mode) <= HOST_BITS_PER_WIDE_INT
	      && ((STORE_FLAG_VALUE
		   & ((HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (result_mode) - 1))))
	      && significant_bits (XEXP (varop, 0), result_mode) == 1
	      && merge_outer_ops (&outer_op, &outer_const, XOR,
				  (HOST_WIDE_INT) 1, result_mode,
				  &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      count = 0;
	      continue;
	    }
	  break;

	case NEG:
	  /* (lshiftrt (neg A) C) where A is either 0 or 1 and C is one less
	     than the number of bits in the mode is equivalent to A.  */
	  if (code == LSHIFTRT && count == GET_MODE_BITSIZE (result_mode) - 1
	      && significant_bits (XEXP (varop, 0), result_mode) == 1)
	    {
	      varop = XEXP (varop, 0);
	      count = 0;
	      continue;
	    }

	  /* NEG commutes with ASHIFT since it is multiplication.  Move the
	     NEG outside to allow shifts to combine.  */
	  if (code == ASHIFT
	      && merge_outer_ops (&outer_op, &outer_const, NEG,
				  (HOST_WIDE_INT) 0, result_mode,
				  &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  break;

	case PLUS:
	  /* (lshiftrt (plus A -1) C) where A is either 0 or 1 and C
	     is one less than the number of bits in the mode is
	     equivalent to (xor A 1).  */
	  if (code == LSHIFTRT && count == GET_MODE_BITSIZE (result_mode) - 1
	      && XEXP (varop, 1) == constm1_rtx
	      && significant_bits (XEXP (varop, 0), result_mode) == 1
	      && merge_outer_ops (&outer_op, &outer_const, XOR,
				  (HOST_WIDE_INT) 1, result_mode,
				  &complement_p))
	    {
	      count = 0;
	      varop = XEXP (varop, 0);
	      continue;
	    }

	  /* If we have (xshiftrt (plus FOO BAR) C), and the only bits
	     significant in BAR are those being shifted out and those
	     bits are known zero in FOO, we can replace the PLUS with FOO.
	     Similarly in the other operand order.  This code occurs when
	     we are computing the size of a variable-size array.  */

	  if ((code == ASHIFTRT || code == LSHIFTRT)
	      && count < HOST_BITS_PER_WIDE_INT
	      && significant_bits (XEXP (varop, 1), result_mode) >> count == 0
	      && (significant_bits (XEXP (varop, 1), result_mode)
		  & significant_bits (XEXP (varop, 0), result_mode)) == 0)
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  else if ((code == ASHIFTRT || code == LSHIFTRT)
		   && count < HOST_BITS_PER_WIDE_INT
		   && GET_MODE_BITSIZE (result_mode) <= HOST_BITS_PER_WIDE_INT
		   && 0 == (significant_bits (XEXP (varop, 0), result_mode)
			    >> count)
		   && 0 == (significant_bits (XEXP (varop, 0), result_mode)
			    & significant_bits (XEXP (varop, 1),
						 result_mode)))
	    {
	      varop = XEXP (varop, 1);
	      continue;
	    }

	  /* (ashift (plus foo C) N) is (plus (ashift foo N) C').  */
	  if (code == ASHIFT
	      && GET_CODE (XEXP (varop, 1)) == CONST_INT
	      && (new = simplify_binary_operation (ASHIFT, result_mode,
						   XEXP (varop, 1),
						   GEN_INT (count))) != 0
	      && merge_outer_ops (&outer_op, &outer_const, PLUS,
				  INTVAL (new), result_mode, &complement_p))
	    {
	      varop = XEXP (varop, 0);
	      continue;
	    }
	  break;

	case MINUS:
	  /* If we have (xshiftrt (minus (ashiftrt X C)) X) C)
	     with C the size of VAROP - 1 and the shift is logical if
	     STORE_FLAG_VALUE is 1 and arithmetic if STORE_FLAG_VALUE is -1,
	     we have a (gt X 0) operation.  If the shift is arithmetic with
	     STORE_FLAG_VALUE of 1 or logical with STORE_FLAG_VALUE == -1,
	     we have a (neg (gt X 0)) operation.  */

	  if (GET_CODE (XEXP (varop, 0)) == ASHIFTRT
	      && count == GET_MODE_BITSIZE (GET_MODE (varop)) - 1
	      && (STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	      && (code == LSHIFTRT || code == ASHIFTRT)
	      && GET_CODE (XEXP (XEXP (varop, 0), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (varop, 0), 1)) == count
	      && rtx_equal_p (XEXP (XEXP (varop, 0), 0), XEXP (varop, 1)))
	    {
	      count = 0;
	      varop = gen_rtx_combine (GT, GET_MODE (varop), XEXP (varop, 1),
				       const0_rtx);

	      if (STORE_FLAG_VALUE == 1 ? code == ASHIFTRT : code == LSHIFTRT)
		varop = gen_rtx_combine (NEG, GET_MODE (varop), varop);

	      continue;
	    }
	  break;
	}

      break;
    }

  /* We need to determine what mode to do the shift in.  If the shift is
     a ASHIFTRT or ROTATE, we must always do it in the mode it was originally
     done in.  Otherwise, we can do it in MODE, the widest mode encountered.
     The code we care about is that of the shift that will actually be done,
     not the shift that was originally requested.  */
  shift_mode = (code == ASHIFTRT || code == ROTATE ? result_mode : mode);

  /* We have now finished analyzing the shift.  The result should be
     a shift of type CODE with SHIFT_MODE shifting VAROP COUNT places.  If
     OUTER_OP is non-NIL, it is an operation that needs to be applied
     to the result of the shift.  OUTER_CONST is the relevant constant,
     but we must turn off all bits turned off in the shift.

     If we were passed a value for X, see if we can use any pieces of
     it.  If not, make new rtx.  */

  if (x && GET_RTX_CLASS (GET_CODE (x)) == '2'
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) == count)
    const_rtx = XEXP (x, 1);
  else
    const_rtx = GEN_INT (count);

  if (x && GET_CODE (XEXP (x, 0)) == SUBREG
      && GET_MODE (XEXP (x, 0)) == shift_mode
      && SUBREG_REG (XEXP (x, 0)) == varop)
    varop = XEXP (x, 0);
  else if (GET_MODE (varop) != shift_mode)
    varop = gen_lowpart_for_combine (shift_mode, varop);

  /* If we can't make the SUBREG, try to return what we were given. */
  if (GET_CODE (varop) == CLOBBER)
    return x ? x : varop;

  new = simplify_binary_operation (code, shift_mode, varop, const_rtx);
  if (new != 0)
    x = new;
  else
    {
      if (x == 0 || GET_CODE (x) != code || GET_MODE (x) != shift_mode)
	x = gen_rtx_combine (code, shift_mode, varop, const_rtx);

      SUBST (XEXP (x, 0), varop);
      SUBST (XEXP (x, 1), const_rtx);
    }

  /* If we were doing a LSHIFTRT in a wider mode than it was originally,
     turn off all the bits that the shift would have turned off.  */
  if (orig_code == LSHIFTRT && result_mode != shift_mode)
    x = simplify_and_const_int (NULL_RTX, shift_mode, x,
				GET_MODE_MASK (result_mode) >> orig_count);
      
  /* Do the remainder of the processing in RESULT_MODE.  */
  x = gen_lowpart_for_combine (result_mode, x);

  /* If COMPLEMENT_P is set, we have to complement X before doing the outer
     operation.  */
  if (complement_p)
    x = gen_unary (NOT, result_mode, x);

  if (outer_op != NIL)
    {
      if (GET_MODE_BITSIZE (result_mode) < HOST_BITS_PER_WIDE_INT)
	outer_const &= GET_MODE_MASK (result_mode);

      if (outer_op == AND)
	x = simplify_and_const_int (NULL_RTX, result_mode, x, outer_const);
      else if (outer_op == SET)
	/* This means that we have determined that the result is
	   equivalent to a constant.  This should be rare.  */
	x = GEN_INT (outer_const);
      else if (GET_RTX_CLASS (outer_op) == '1')
	x = gen_unary (outer_op, result_mode, x);
      else
	x = gen_binary (outer_op, result_mode, x, GEN_INT (outer_const));
    }

  return x;
}  

/* Like recog, but we receive the address of a pointer to a new pattern.
   We try to match the rtx that the pointer points to.
   If that fails, we may try to modify or replace the pattern,
   storing the replacement into the same pointer object.

   Modifications include deletion or addition of CLOBBERs.

   PNOTES is a pointer to a location where any REG_UNUSED notes added for
   the CLOBBERs are placed.

   The value is the final insn code from the pattern ultimately matched,
   or -1.  */

static int
recog_for_combine (pnewpat, insn, pnotes)
     rtx *pnewpat;
     rtx insn;
     rtx *pnotes;
{
  register rtx pat = *pnewpat;
  int insn_code_number;
  int num_clobbers_to_add = 0;
  int i;
  rtx notes = 0;

  /* Is the result of combination a valid instruction?  */
  insn_code_number = recog (pat, insn, &num_clobbers_to_add);

  /* If it isn't, there is the possibility that we previously had an insn
     that clobbered some register as a side effect, but the combined
     insn doesn't need to do that.  So try once more without the clobbers
     unless this represents an ASM insn.  */

  if (insn_code_number < 0 && ! check_asm_operands (pat)
      && GET_CODE (pat) == PARALLEL)
    {
      int pos;

      for (pos = 0, i = 0; i < XVECLEN (pat, 0); i++)
	if (GET_CODE (XVECEXP (pat, 0, i)) != CLOBBER)
	  {
	    if (i != pos)
	      SUBST (XVECEXP (pat, 0, pos), XVECEXP (pat, 0, i));
	    pos++;
	  }

      SUBST_INT (XVECLEN (pat, 0), pos);

      if (pos == 1)
	pat = XVECEXP (pat, 0, 0);

      insn_code_number = recog (pat, insn, &num_clobbers_to_add);
    }

  /* If we had any clobbers to add, make a new pattern than contains
     them.  Then check to make sure that all of them are dead.  */
  if (num_clobbers_to_add)
    {
      rtx newpat = gen_rtx (PARALLEL, VOIDmode,
			    gen_rtvec (GET_CODE (pat) == PARALLEL
				       ? XVECLEN (pat, 0) + num_clobbers_to_add
				       : num_clobbers_to_add + 1));

      if (GET_CODE (pat) == PARALLEL)
	for (i = 0; i < XVECLEN (pat, 0); i++)
	  XVECEXP (newpat, 0, i) = XVECEXP (pat, 0, i);
      else
	XVECEXP (newpat, 0, 0) = pat;

      add_clobbers (newpat, insn_code_number);

      for (i = XVECLEN (newpat, 0) - num_clobbers_to_add;
	   i < XVECLEN (newpat, 0); i++)
	{
	  if (GET_CODE (XEXP (XVECEXP (newpat, 0, i), 0)) == REG
	      && ! reg_dead_at_p (XEXP (XVECEXP (newpat, 0, i), 0), insn))
	    return -1;
	  notes = gen_rtx (EXPR_LIST, REG_UNUSED,
			   XEXP (XVECEXP (newpat, 0, i), 0), notes);
	}
      pat = newpat;
    }

  *pnewpat = pat;
  *pnotes = notes;

  return insn_code_number;
}

/* Like gen_lowpart but for use by combine.  In combine it is not possible
   to create any new pseudoregs.  However, it is safe to create
   invalid memory addresses, because combine will try to recognize
   them and all they will do is make the combine attempt fail.

   If for some reason this cannot do its job, an rtx
   (clobber (const_int 0)) is returned.
   An insn containing that will not be recognized.  */

#undef gen_lowpart

static rtx
gen_lowpart_for_combine (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  rtx result;

  if (GET_MODE (x) == mode)
    return x;

  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return gen_rtx (CLOBBER, GET_MODE (x), const0_rtx);

  /* X might be a paradoxical (subreg (mem)).  In that case, gen_lowpart
     won't know what to do.  So we will strip off the SUBREG here and
     process normally.  */
  if (GET_CODE (x) == SUBREG && GET_CODE (SUBREG_REG (x)) == MEM)
    {
      x = SUBREG_REG (x);
      if (GET_MODE (x) == mode)
	return x;
    }

  result = gen_lowpart_common (mode, x);
  if (result)
    return result;

  if (GET_CODE (x) == MEM)
    {
      register int offset = 0;
      rtx new;

      /* Refuse to work on a volatile memory ref or one with a mode-dependent
	 address.  */
      if (MEM_VOLATILE_P (x) || mode_dependent_address_p (XEXP (x, 0)))
	return gen_rtx (CLOBBER, GET_MODE (x), const0_rtx);

      /* If we want to refer to something bigger than the original memref,
	 generate a perverse subreg instead.  That will force a reload
	 of the original memref X.  */
      if (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (mode))
	return gen_rtx (SUBREG, mode, x, 0);

#if WORDS_BIG_ENDIAN
      offset = (MAX (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD)
		- MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD));
#endif
#if BYTES_BIG_ENDIAN
      /* Adjust the address so that the address-after-the-data
	 is unchanged.  */
      offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode))
		 - MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (x))));
#endif
      new = gen_rtx (MEM, mode, plus_constant (XEXP (x, 0), offset));
      RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (x);
      MEM_VOLATILE_P (new) = MEM_VOLATILE_P (x);
      MEM_IN_STRUCT_P (new) = MEM_IN_STRUCT_P (x);
      return new;
    }

  /* If X is a comparison operator, rewrite it in a new mode.  This
     probably won't match, but may allow further simplifications.  */
  else if (GET_RTX_CLASS (GET_CODE (x)) == '<')
    return gen_rtx_combine (GET_CODE (x), mode, XEXP (x, 0), XEXP (x, 1));

  /* If we couldn't simplify X any other way, just enclose it in a
     SUBREG.  Normally, this SUBREG won't match, but some patterns may
     include an explicit SUBREG or we may simplify it further in combine.  */
  else
    {
      int word = 0;

      if (WORDS_BIG_ENDIAN && GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	word = ((GET_MODE_SIZE (GET_MODE (x))
		 - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD))
		/ UNITS_PER_WORD);
      return gen_rtx (SUBREG, mode, x, word);
    }
}

/* Make an rtx expression.  This is a subset of gen_rtx and only supports
   expressions of 1, 2, or 3 operands, each of which are rtx expressions.

   If the identical expression was previously in the insn (in the undobuf),
   it will be returned.  Only if it is not found will a new expression
   be made.  */

/*VARARGS2*/
static rtx
gen_rtx_combine (va_alist)
     va_dcl
{
  va_list p;
  enum rtx_code code;
  enum machine_mode mode;
  int n_args;
  rtx args[3];
  int i, j;
  char *fmt;
  rtx rt;

  va_start (p);
  code = va_arg (p, enum rtx_code);
  mode = va_arg (p, enum machine_mode);
  n_args = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  if (n_args == 0 || n_args > 3)
    abort ();

  /* Get each arg and verify that it is supposed to be an expression.  */
  for (j = 0; j < n_args; j++)
    {
      if (*fmt++ != 'e')
	abort ();

      args[j] = va_arg (p, rtx);
    }

  /* See if this is in undobuf.  Be sure we don't use objects that came
     from another insn; this could produce circular rtl structures.  */

  for (i = previous_num_undos; i < undobuf.num_undo; i++)
    if (!undobuf.undo[i].is_int
	&& GET_CODE (undobuf.undo[i].old_contents.rtx) == code
	&& GET_MODE (undobuf.undo[i].old_contents.rtx) == mode)
      {
	for (j = 0; j < n_args; j++)
	  if (XEXP (undobuf.undo[i].old_contents.rtx, j) != args[j])
	    break;

	if (j == n_args)
	  return undobuf.undo[i].old_contents.rtx;
      }

  /* Otherwise make a new rtx.  We know we have 1, 2, or 3 args.
     Use rtx_alloc instead of gen_rtx because it's faster on RISC.  */
  rt = rtx_alloc (code);
  PUT_MODE (rt, mode);
  XEXP (rt, 0) = args[0];
  if (n_args > 1)
    {
      XEXP (rt, 1) = args[1];
      if (n_args > 2)
	XEXP (rt, 2) = args[2];
    }
  return rt;
}

/* These routines make binary and unary operations by first seeing if they
   fold; if not, a new expression is allocated.  */

static rtx
gen_binary (code, mode, op0, op1)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0, op1;
{
  rtx result;
  rtx tem;

  if (GET_RTX_CLASS (code) == 'c'
      && (GET_CODE (op0) == CONST_INT
	  || (CONSTANT_P (op0) && GET_CODE (op1) != CONST_INT)))
    tem = op0, op0 = op1, op1 = tem;

  if (GET_RTX_CLASS (code) == '<') 
    {
      enum machine_mode op_mode = GET_MODE (op0);
      if (op_mode == VOIDmode)
	op_mode = GET_MODE (op1);
      result = simplify_relational_operation (code, op_mode, op0, op1);
    }
  else
    result = simplify_binary_operation (code, mode, op0, op1);

  if (result)
    return result;

  /* Put complex operands first and constants second.  */
  if (GET_RTX_CLASS (code) == 'c'
      && ((CONSTANT_P (op0) && GET_CODE (op1) != CONST_INT)
	  || (GET_RTX_CLASS (GET_CODE (op0)) == 'o'
	      && GET_RTX_CLASS (GET_CODE (op1)) != 'o')
	  || (GET_CODE (op0) == SUBREG
	      && GET_RTX_CLASS (GET_CODE (SUBREG_REG (op0))) == 'o'
	      && GET_RTX_CLASS (GET_CODE (op1)) != 'o')))
    return gen_rtx_combine (code, mode, op1, op0);

  return gen_rtx_combine (code, mode, op0, op1);
}

static rtx
gen_unary (code, mode, op0)
     enum rtx_code code;
     enum machine_mode mode;
     rtx op0;
{
  rtx result = simplify_unary_operation (code, mode, op0, mode);

  if (result)
    return result;

  return gen_rtx_combine (code, mode, op0);
}

/* Simplify a comparison between *POP0 and *POP1 where CODE is the
   comparison code that will be tested.

   The result is a possibly different comparison code to use.  *POP0 and
   *POP1 may be updated.

   It is possible that we might detect that a comparison is either always
   true or always false.  However, we do not perform general constant
   folding in combine, so this knowledge isn't useful.  Such tautologies
   should have been detected earlier.  Hence we ignore all such cases.  */

static enum rtx_code
simplify_comparison (code, pop0, pop1)
     enum rtx_code code;
     rtx *pop0;
     rtx *pop1;
{
  rtx op0 = *pop0;
  rtx op1 = *pop1;
  rtx tem, tem1;
  int i;
  enum machine_mode mode, tmode;

  /* Try a few ways of applying the same transformation to both operands.  */
  while (1)
    {
      /* If both operands are the same constant shift, see if we can ignore the
	 shift.  We can if the shift is a rotate or if the bits shifted out of
	 this shift are not significant for either input and if the type of
	 comparison is compatible with the shift.  */
      if (GET_CODE (op0) == GET_CODE (op1)
	  && GET_MODE_BITSIZE (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT
	  && ((GET_CODE (op0) == ROTATE && (code == NE || code == EQ))
	      || ((GET_CODE (op0) == LSHIFTRT
		   || GET_CODE (op0) == ASHIFT || GET_CODE (op0) == LSHIFT)
		  && (code != GT && code != LT && code != GE && code != LE))
	      || (GET_CODE (op0) == ASHIFTRT
		  && (code != GTU && code != LTU
		      && code != GEU && code != GEU)))
	  && GET_CODE (XEXP (op0, 1)) == CONST_INT
	  && INTVAL (XEXP (op0, 1)) >= 0
	  && INTVAL (XEXP (op0, 1)) < HOST_BITS_PER_WIDE_INT
	  && XEXP (op0, 1) == XEXP (op1, 1))
	{
	  enum machine_mode mode = GET_MODE (op0);
	  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (mode);
	  int shift_count = INTVAL (XEXP (op0, 1));

	  if (GET_CODE (op0) == LSHIFTRT || GET_CODE (op0) == ASHIFTRT)
	    mask &= (mask >> shift_count) << shift_count;
	  else if (GET_CODE (op0) == ASHIFT || GET_CODE (op0) == LSHIFT)
	    mask = (mask & (mask << shift_count)) >> shift_count;

	  if ((significant_bits (XEXP (op0, 0), mode) & ~ mask) == 0
	      && (significant_bits (XEXP (op1, 0), mode) & ~ mask) == 0)
	    op0 = XEXP (op0, 0), op1 = XEXP (op1, 0);
	  else
	    break;
	}

      /* If both operands are AND's of a paradoxical SUBREG by constant, the
	 SUBREGs are of the same mode, and, in both cases, the AND would
	 be redundant if the comparison was done in the narrower mode,
	 do the comparison in the narrower mode (e.g., we are AND'ing with 1
	 and the operand's significant bits are 0xffffff01; in that case if
	 we only care about QImode, we don't need the AND).  This case occurs
	 if the output mode of an scc insn is not SImode and
	 STORE_FLAG_VALUE == 1 (e.g., the 386).  */

      else if  (GET_CODE (op0) == AND && GET_CODE (op1) == AND
		&& GET_CODE (XEXP (op0, 1)) == CONST_INT
		&& GET_CODE (XEXP (op1, 1)) == CONST_INT
		&& GET_CODE (XEXP (op0, 0)) == SUBREG
		&& GET_CODE (XEXP (op1, 0)) == SUBREG
		&& (GET_MODE_SIZE (GET_MODE (XEXP (op0, 0)))
		    > GET_MODE_SIZE (GET_MODE (SUBREG_REG (XEXP (op0, 0)))))
		&& (GET_MODE (SUBREG_REG (XEXP (op0, 0)))
		    == GET_MODE (SUBREG_REG (XEXP (op1, 0))))
		&& (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (XEXP (op0, 0))))
		    <= HOST_BITS_PER_WIDE_INT)
		&& (significant_bits (SUBREG_REG (XEXP (op0, 0)),
				      GET_MODE (SUBREG_REG (XEXP (op0, 0))))
		    & ~ INTVAL (XEXP (op0, 1))) == 0
		&& (significant_bits (SUBREG_REG (XEXP (op1, 0)),
				      GET_MODE (SUBREG_REG (XEXP (op1, 0))))
		    & ~ INTVAL (XEXP (op1, 1))) == 0)
	{
	  op0 = SUBREG_REG (XEXP (op0, 0));
	  op1 = SUBREG_REG (XEXP (op1, 0));

	  /* the resulting comparison is always unsigned since we masked off
	     the original sign bit. */
	  code = unsigned_condition (code);
	}
      else
	break;
    }
     
  /* If the first operand is a constant, swap the operands and adjust the
     comparison code appropriately.  */
  if (CONSTANT_P (op0))
    {
      tem = op0, op0 = op1, op1 = tem;
      code = swap_condition (code);
    }

  /* We now enter a loop during which we will try to simplify the comparison.
     For the most part, we only are concerned with comparisons with zero,
     but some things may really be comparisons with zero but not start
     out looking that way.  */

  while (GET_CODE (op1) == CONST_INT)
    {
      enum machine_mode mode = GET_MODE (op0);
      int mode_width = GET_MODE_BITSIZE (mode);
      unsigned HOST_WIDE_INT mask = GET_MODE_MASK (mode);
      int equality_comparison_p;
      int sign_bit_comparison_p;
      int unsigned_comparison_p;
      HOST_WIDE_INT const_op;

      /* We only want to handle integral modes.  This catches VOIDmode,
	 CCmode, and the floating-point modes.  An exception is that we
	 can handle VOIDmode if OP0 is a COMPARE or a comparison
	 operation.  */

      if (GET_MODE_CLASS (mode) != MODE_INT
	  && ! (mode == VOIDmode
		&& (GET_CODE (op0) == COMPARE
		    || GET_RTX_CLASS (GET_CODE (op0)) == '<')))
	break;

      /* Get the constant we are comparing against and turn off all bits
	 not on in our mode.  */
      const_op = INTVAL (op1);
      if (mode_width <= HOST_BITS_PER_WIDE_INT)
	const_op &= mask;

      /* If we are comparing against a constant power of two and the value
	 being compared has only that single significant bit (e.g., it was
	 `and'ed with that bit), we can replace this with a comparison
	 with zero.  */
      if (const_op
	  && (code == EQ || code == NE || code == GE || code == GEU
	      || code == LT || code == LTU)
	  && mode_width <= HOST_BITS_PER_WIDE_INT
	  && exact_log2 (const_op) >= 0
	  && significant_bits (op0, mode) == const_op)
	{
	  code = (code == EQ || code == GE || code == GEU ? NE : EQ);
	  op1 = const0_rtx, const_op = 0;
	}

      /* Similarly, if we are comparing a value known to be either -1 or
	 0 with -1, change it to the opposite comparison against zero.  */

      if (const_op == -1
	  && (code == EQ || code == NE || code == GT || code == LE
	      || code == GEU || code == LTU)
	  && num_sign_bit_copies (op0, mode) == mode_width)
	{
	  code = (code == EQ || code == LE || code == GEU ? NE : EQ);
	  op1 = const0_rtx, const_op = 0;
	}

      /* Do some canonicalizations based on the comparison code.  We prefer
	 comparisons against zero and then prefer equality comparisons.  
	 If we can reduce the size of a constant, we will do that too.  */

      switch (code)
	{
	case LT:
	  /* < C is equivalent to <= (C - 1) */
	  if (const_op > 0)
	    {
	      const_op -= 1;
	      op1 = GEN_INT (const_op);
	      code = LE;
	      /* ... fall through to LE case below.  */
	    }
	  else
	    break;

	case LE:
	  /* <= C is equivalent to < (C + 1); we do this for C < 0  */
	  if (const_op < 0)
	    {
	      const_op += 1;
	      op1 = GEN_INT (const_op);
	      code = LT;
	    }

	  /* If we are doing a <= 0 comparison on a value known to have
	     a zero sign bit, we can replace this with == 0.  */
	  else if (const_op == 0
		   && mode_width <= HOST_BITS_PER_WIDE_INT
		   && (significant_bits (op0, mode)
		       & ((HOST_WIDE_INT) 1 << (mode_width - 1))) == 0)
	    code = EQ;
	  break;

	case GE:
	  /* >= C is equivalent to > (C - 1). */
	  if (const_op > 0)
	    {
	      const_op -= 1;
	      op1 = GEN_INT (const_op);
	      code = GT;
	      /* ... fall through to GT below.  */
	    }
	  else
	    break;

	case GT:
	  /* > C is equivalent to >= (C + 1); we do this for C < 0*/
	  if (const_op < 0)
	    {
	      const_op += 1;
	      op1 = GEN_INT (const_op);
	      code = GE;
	    }

	  /* If we are doing a > 0 comparison on a value known to have
	     a zero sign bit, we can replace this with != 0.  */
	  else if (const_op == 0
		   && mode_width <= HOST_BITS_PER_WIDE_INT
		   && (significant_bits (op0, mode)
		       & ((HOST_WIDE_INT) 1 << (mode_width - 1))) == 0)
	    code = NE;
	  break;

	case LTU:
	  /* < C is equivalent to <= (C - 1).  */
	  if (const_op > 0)
	    {
	      const_op -= 1;
	      op1 = GEN_INT (const_op);
	      code = LEU;
	      /* ... fall through ... */
	    }

	  /* (unsigned) < 0x80000000 is equivalent to >= 0.  */
	  else if (const_op == (HOST_WIDE_INT) 1 << (mode_width - 1))
	    {
	      const_op = 0, op1 = const0_rtx;
	      code = GE;
	      break;
	    }
	  else
	    break;

	case LEU:
	  /* unsigned <= 0 is equivalent to == 0 */
	  if (const_op == 0)
	    code = EQ;

	  /* (unsigned) <= 0x7fffffff is equivalent to >= 0. */
	  else if (const_op == ((HOST_WIDE_INT) 1 << (mode_width - 1)) - 1)
	    {
	      const_op = 0, op1 = const0_rtx;
	      code = GE;
	    }
	  break;

	case GEU:
	  /* >= C is equivalent to < (C - 1).  */
	  if (const_op > 1)
	    {
	      const_op -= 1;
	      op1 = GEN_INT (const_op);
	      code = GTU;
	      /* ... fall through ... */
	    }

	  /* (unsigned) >= 0x80000000 is equivalent to < 0.  */
	  else if (const_op == (HOST_WIDE_INT) 1 << (mode_width - 1))
	    {
	      const_op = 0, op1 = const0_rtx;
	      code = LT;
	    }
	  else
	    break;

	case GTU:
	  /* unsigned > 0 is equivalent to != 0 */
	  if (const_op == 0)
	    code = NE;

	  /* (unsigned) > 0x7fffffff is equivalent to < 0.  */
	  else if (const_op == ((HOST_WIDE_INT) 1 << (mode_width - 1)) - 1)
	    {
	      const_op = 0, op1 = const0_rtx;
	      code = LT;
	    }
	  break;
	}

      /* Compute some predicates to simplify code below.  */

      equality_comparison_p = (code == EQ || code == NE);
      sign_bit_comparison_p = ((code == LT || code == GE) && const_op == 0);
      unsigned_comparison_p = (code == LTU || code == LEU || code == GTU
			       || code == LEU);

      /* Now try cases based on the opcode of OP0.  If none of the cases
	 does a "continue", we exit this loop immediately after the
	 switch.  */

      switch (GET_CODE (op0))
	{
	case ZERO_EXTRACT:
	  /* If we are extracting a single bit from a variable position in
	     a constant that has only a single bit set and are comparing it
	     with zero, we can convert this into an equality comparison 
	     between the position and the location of the single bit.  We can't
	     do this if bit endian and we don't have an extzv since we then
	     can't know what mode to use for the endianness adjustment.  */

#if ! BITS_BIG_ENDIAN || defined (HAVE_extzv)
	  if (GET_CODE (XEXP (op0, 0)) == CONST_INT
	      && XEXP (op0, 1) == const1_rtx
	      && equality_comparison_p && const_op == 0
	      && (i = exact_log2 (INTVAL (XEXP (op0, 0)))) >= 0)
	    {
#if BITS_BIG_ENDIAN
	      i = (GET_MODE_BITSIZE
		   (insn_operand_mode[(int) CODE_FOR_extzv][1]) - 1 - i);
#endif

	      op0 = XEXP (op0, 2);
	      op1 = GEN_INT (i);
	      const_op = i;

	      /* Result is nonzero iff shift count is equal to I.  */
	      code = reverse_condition (code);
	      continue;
	    }
#endif

	  /* ... fall through ... */

	case SIGN_EXTRACT:
	  tem = expand_compound_operation (op0);
	  if (tem != op0)
	    {
	      op0 = tem;
	      continue;
	    }
	  break;

	case NOT:
	  /* If testing for equality, we can take the NOT of the constant.  */
	  if (equality_comparison_p
	      && (tem = simplify_unary_operation (NOT, mode, op1, mode)) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* If just looking at the sign bit, reverse the sense of the
	     comparison.  */
	  if (sign_bit_comparison_p)
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == GE ? LT : GE);
	      continue;
	    }
	  break;

	case NEG:
	  /* If testing for equality, we can take the NEG of the constant.  */
	  if (equality_comparison_p
	      && (tem = simplify_unary_operation (NEG, mode, op1, mode)) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* The remaining cases only apply to comparisons with zero.  */
	  if (const_op != 0)
	    break;

	  /* When X is ABS or is known positive,
	     (neg X) is < 0 if and only if X != 0.  */

	  if (sign_bit_comparison_p
	      && (GET_CODE (XEXP (op0, 0)) == ABS
		  || (mode_width <= HOST_BITS_PER_WIDE_INT
		      && (significant_bits (XEXP (op0, 0), mode)
			  & ((HOST_WIDE_INT) 1 << (mode_width - 1))) == 0)))
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == LT ? NE : EQ);
	      continue;
	    }

	  /* If we have NEG of something that is the result of a
	     SIGN_EXTEND, SIGN_EXTRACT, or ASHIFTRT, we know that the
	     two high-order bits must be the same and hence that
	     "(-a) < 0" is equivalent to "a > 0".  Otherwise, we can't
	     do this.  */
	  if (GET_CODE (XEXP (op0, 0)) == SIGN_EXTEND
	      || (GET_CODE (XEXP (op0, 0)) == SIGN_EXTRACT
		  && GET_CODE (XEXP (XEXP (op0, 0), 1)) == CONST_INT
		  && (INTVAL (XEXP (XEXP (op0, 0), 1))
		      < GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (op0, 0), 0)))))
	      || (GET_CODE (XEXP (op0, 0)) == ASHIFTRT
		  && GET_CODE (XEXP (XEXP (op0, 0), 1)) == CONST_INT
		  && XEXP (XEXP (op0, 0), 1) != const0_rtx)
	      || ((tem = get_last_value (XEXP (op0, 0))) != 0
		  && (GET_CODE (tem) == SIGN_EXTEND
		      || (GET_CODE (tem) == SIGN_EXTRACT
			  && GET_CODE (XEXP (tem, 1)) == CONST_INT
			  && (INTVAL (XEXP (tem, 1))
			      < GET_MODE_BITSIZE (GET_MODE (XEXP (tem, 0)))))
		      || (GET_CODE (tem) == ASHIFTRT
			  && GET_CODE (XEXP (tem, 1)) == CONST_INT
			  && XEXP (tem, 1) != const0_rtx))))
	    {
	      op0 = XEXP (op0, 0);
	      code = swap_condition (code);
	      continue;
	    }
	  break;

	case ROTATE:
	  /* If we are testing equality and our count is a constant, we
	     can perform the inverse operation on our RHS.  */
	  if (equality_comparison_p && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && (tem = simplify_binary_operation (ROTATERT, mode,
						   op1, XEXP (op0, 1))) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* If we are doing a < 0 or >= 0 comparison, it means we are testing
	     a particular bit.  Convert it to an AND of a constant of that
	     bit.  This will be converted into a ZERO_EXTRACT.  */
	  if (const_op == 0 && sign_bit_comparison_p
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && mode_width <= HOST_BITS_PER_WIDE_INT)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode, XEXP (op0, 0),
					    ((HOST_WIDE_INT) 1
					     << (mode_width - 1
						 - INTVAL (XEXP (op0, 1)))));
	      code = (code == LT ? NE : EQ);
	      continue;
	    }

	  /* ... fall through ... */

	case ABS:
	  /* ABS is ignorable inside an equality comparison with zero.  */
	  if (const_op == 0 && equality_comparison_p)
	    {
	      op0 = XEXP (op0, 0);
	      continue;
	    }
	  break;
	  

	case SIGN_EXTEND:
	  /* Can simplify (compare (zero/sign_extend FOO) CONST)
	     to (compare FOO CONST) if CONST fits in FOO's mode and we 
	     are either testing inequality or have an unsigned comparison
	     with ZERO_EXTEND or a signed comparison with SIGN_EXTEND.  */
	  if (! unsigned_comparison_p
	      && (GET_MODE_BITSIZE (GET_MODE (XEXP (op0, 0)))
		  <= HOST_BITS_PER_WIDE_INT)
	      && ((unsigned HOST_WIDE_INT) const_op
		  < (((HOST_WIDE_INT) 1
		      << (GET_MODE_BITSIZE (GET_MODE (XEXP (op0, 0))) - 1)))))
	    {
	      op0 = XEXP (op0, 0);
	      continue;
	    }
	  break;

	case SUBREG:
	  /* Check for the case where we are comparing A - C1 with C2,
	     both constants are smaller than 1/2 the maxium positive
	     value in MODE, and the comparison is equality or unsigned.
	     In that case, if A is either zero-extended to MODE or has
	     sufficient sign bits so that the high-order bit in MODE
	     is a copy of the sign in the inner mode, we can prove that it is
	     safe to do the operation in the wider mode.  This simplifies
	     many range checks.  */

	  if (mode_width <= HOST_BITS_PER_WIDE_INT
	      && subreg_lowpart_p (op0)
	      && GET_CODE (SUBREG_REG (op0)) == PLUS
	      && GET_CODE (XEXP (SUBREG_REG (op0), 1)) == CONST_INT
	      && INTVAL (XEXP (SUBREG_REG (op0), 1)) < 0
	      && (- INTVAL (XEXP (SUBREG_REG (op0), 1))
		  < GET_MODE_MASK (mode) / 2)
	      && (unsigned) const_op < GET_MODE_MASK (mode) / 2
	      && (0 == (significant_bits (XEXP (SUBREG_REG (op0), 0),
					  GET_MODE (SUBREG_REG (op0)))
			& ~ GET_MODE_MASK (mode))
		  || (num_sign_bit_copies (XEXP (SUBREG_REG (op0), 0),
					   GET_MODE (SUBREG_REG (op0)))
		      > (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op0)))
			 - GET_MODE_BITSIZE (mode)))))
	    {
	      op0 = SUBREG_REG (op0);
	      continue;
	    }

	  /* If the inner mode is narrower and we are extracting the low part,
	     we can treat the SUBREG as if it were a ZERO_EXTEND.  */
	  if (subreg_lowpart_p (op0)
	      && GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op0))) < mode_width)
	    /* Fall through */ ;
	  else
	    break;

	  /* ... fall through ... */

	case ZERO_EXTEND:
	  if ((unsigned_comparison_p || equality_comparison_p)
	      && (GET_MODE_BITSIZE (GET_MODE (XEXP (op0, 0)))
		  <= HOST_BITS_PER_WIDE_INT)
	      && ((unsigned HOST_WIDE_INT) const_op
		  < GET_MODE_MASK (GET_MODE (XEXP (op0, 0)))))
	    {
	      op0 = XEXP (op0, 0);
	      continue;
	    }
	  break;

	case PLUS:
	  /* (eq (plus X C1) C2) -> (eq X (minus C2 C1)).  We can only do
	     this for equality comparisons due to pathological cases involving
	     overflows.  */
	  if (equality_comparison_p && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && (tem = simplify_binary_operation (MINUS, mode, op1,
						   XEXP (op0, 1))) != 0)
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }

	  /* (plus (abs X) (const_int -1)) is < 0 if and only if X == 0.  */
	  if (const_op == 0 && XEXP (op0, 1) == constm1_rtx
	      && GET_CODE (XEXP (op0, 0)) == ABS && sign_bit_comparison_p)
	    {
	      op0 = XEXP (XEXP (op0, 0), 0);
	      code = (code == LT ? EQ : NE);
	      continue;
	    }
	  break;

	case MINUS:
	  /* The sign bit of (minus (ashiftrt X C) X), where C is the number
	     of bits in X minus 1, is one iff X > 0.  */
	  if (sign_bit_comparison_p && GET_CODE (XEXP (op0, 0)) == ASHIFTRT
	      && GET_CODE (XEXP (XEXP (op0, 0), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (op0, 0), 1)) == mode_width - 1
	      && rtx_equal_p (XEXP (XEXP (op0, 0), 0), XEXP (op0, 1)))
	    {
	      op0 = XEXP (op0, 1);
	      code = (code == GE ? LE : GT);
	      continue;
	    }
	  break;

	case XOR:
	  /* (eq (xor A B) C) -> (eq A (xor B C)).  This is a simplification
	     if C is zero or B is a constant.  */
	  if (equality_comparison_p
	      && 0 != (tem = simplify_binary_operation (XOR, mode,
							XEXP (op0, 1), op1)))
	    {
	      op0 = XEXP (op0, 0);
	      op1 = tem;
	      continue;
	    }
	  break;

	case EQ:  case NE:
	case LT:  case LTU:  case LE:  case LEU:
	case GT:  case GTU:  case GE:  case GEU:
	  /* We can't do anything if OP0 is a condition code value, rather
	     than an actual data value.  */
	  if (const_op != 0
#ifdef HAVE_cc0
	      || XEXP (op0, 0) == cc0_rtx
#endif
	      || GET_MODE_CLASS (GET_MODE (XEXP (op0, 0))) == MODE_CC)
	    break;

	  /* Get the two operands being compared.  */
	  if (GET_CODE (XEXP (op0, 0)) == COMPARE)
	    tem = XEXP (XEXP (op0, 0), 0), tem1 = XEXP (XEXP (op0, 0), 1);
	  else
	    tem = XEXP (op0, 0), tem1 = XEXP (op0, 1);

	  /* Check for the cases where we simply want the result of the
	     earlier test or the opposite of that result.  */
	  if (code == NE
	      || (code == EQ && reversible_comparison_p (op0))
	      || (GET_MODE_BITSIZE (GET_MODE (op0)) <= HOST_BITS_PER_WIDE_INT
		  && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
		  && (STORE_FLAG_VALUE
		      & (((HOST_WIDE_INT) 1
			  << (GET_MODE_BITSIZE (GET_MODE (op0)) - 1))))
		  && (code == LT
		      || (code == GE && reversible_comparison_p (op0)))))
	    {
	      code = (code == LT || code == NE
		      ? GET_CODE (op0) : reverse_condition (GET_CODE (op0)));
	      op0 = tem, op1 = tem1;
	      continue;
	    }
	  break;

	case IOR:
	  /* The sign bit of (ior (plus X (const_int -1)) X) is non-zero
	     iff X <= 0.  */
	  if (sign_bit_comparison_p && GET_CODE (XEXP (op0, 0)) == PLUS
	      && XEXP (XEXP (op0, 0), 1) == constm1_rtx
	      && rtx_equal_p (XEXP (XEXP (op0, 0), 0), XEXP (op0, 1)))
	    {
	      op0 = XEXP (op0, 1);
	      code = (code == GE ? GT : LE);
	      continue;
	    }
	  break;

	case AND:
	  /* Convert (and (xshift 1 X) Y) to (and (lshiftrt Y X) 1).  This
	     will be converted to a ZERO_EXTRACT later.  */
	  if (const_op == 0 && equality_comparison_p
	      && (GET_CODE (XEXP (op0, 0)) == ASHIFT
		  || GET_CODE (XEXP (op0, 0)) == LSHIFT)
	      && XEXP (XEXP (op0, 0), 0) == const1_rtx)
	    {
	      op0 = simplify_and_const_int
		(op0, mode, gen_rtx_combine (LSHIFTRT, mode,
					     XEXP (op0, 1),
					     XEXP (XEXP (op0, 0), 1)),
		 (HOST_WIDE_INT) 1);
	      continue;
	    }

	  /* If we are comparing (and (lshiftrt X C1) C2) for equality with
	     zero and X is a comparison and C1 and C2 describe only bits set
	     in STORE_FLAG_VALUE, we can compare with X.  */
	  if (const_op == 0 && equality_comparison_p
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && GET_CODE (XEXP (op0, 0)) == LSHIFTRT
	      && GET_CODE (XEXP (XEXP (op0, 0), 1)) == CONST_INT
	      && INTVAL (XEXP (XEXP (op0, 0), 1)) >= 0
	      && INTVAL (XEXP (XEXP (op0, 0), 1)) < HOST_BITS_PER_WIDE_INT)
	    {
	      mask = ((INTVAL (XEXP (op0, 1)) & GET_MODE_MASK (mode))
		      << INTVAL (XEXP (XEXP (op0, 0), 1)));
	      if ((~ STORE_FLAG_VALUE & mask) == 0
		  && (GET_RTX_CLASS (GET_CODE (XEXP (XEXP (op0, 0), 0))) == '<'
		      || ((tem = get_last_value (XEXP (XEXP (op0, 0), 0))) != 0
			  && GET_RTX_CLASS (GET_CODE (tem)) == '<')))
		{
		  op0 = XEXP (XEXP (op0, 0), 0);
		  continue;
		}
	    }

	  /* If we are doing an equality comparison of an AND of a bit equal
	     to the sign bit, replace this with a LT or GE comparison of
	     the underlying value.  */
	  if (equality_comparison_p
	      && const_op == 0
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && ((INTVAL (XEXP (op0, 1)) & GET_MODE_MASK (mode))
		  == (HOST_WIDE_INT) 1 << (mode_width - 1)))
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == EQ ? GE : LT);
	      continue;
	    }

	  /* If this AND operation is really a ZERO_EXTEND from a narrower
	     mode, the constant fits within that mode, and this is either an
	     equality or unsigned comparison, try to do this comparison in
	     the narrower mode.  */
	  if ((equality_comparison_p || unsigned_comparison_p)
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && (i = exact_log2 ((INTVAL (XEXP (op0, 1))
				   & GET_MODE_MASK (mode))
				  + 1)) >= 0
	      && const_op >> i == 0
	      && (tmode = mode_for_size (i, MODE_INT, 1)) != BLKmode)
	    {
	      op0 = gen_lowpart_for_combine (tmode, XEXP (op0, 0));
	      continue;
	    }
	  break;

	case ASHIFT:
	case LSHIFT:
	  /* If we have (compare (xshift FOO N) (const_int C)) and
	     the high order N bits of FOO (N+1 if an inequality comparison)
	     are not significant, we can do this by comparing FOO with C
	     shifted right N bits so long as the low-order N bits of C are
	     zero.  */
	  if (GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && INTVAL (XEXP (op0, 1)) >= 0
	      && ((INTVAL (XEXP (op0, 1)) + ! equality_comparison_p)
		  < HOST_BITS_PER_WIDE_INT)
	      && ((const_op
		   &  ((HOST_WIDE_INT) 1 << INTVAL (XEXP (op0, 1))) - 1) == 0)
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && (significant_bits (XEXP (op0, 0), mode)
		  & ~ (mask >> (INTVAL (XEXP (op0, 1))
				+ ! equality_comparison_p))) == 0)
	    {
	      const_op >>= INTVAL (XEXP (op0, 1));
	      op1 = GEN_INT (const_op);
	      op0 = XEXP (op0, 0);
	      continue;
	    }

	  /* If we are doing a sign bit comparison, it means we are testing
	     a particular bit.  Convert it to the appropriate AND.  */
	  if (sign_bit_comparison_p && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && mode_width <= HOST_BITS_PER_WIDE_INT)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode, XEXP (op0, 0),
					    ((HOST_WIDE_INT) 1
					     << (mode_width - 1
						 - INTVAL (XEXP (op0, 1)))));
	      code = (code == LT ? NE : EQ);
	      continue;
	    }

	  /* If this an equality comparison with zero and we are shifting
	     the low bit to the sign bit, we can convert this to an AND of the
	     low-order bit.  */
	  if (const_op == 0 && equality_comparison_p
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && INTVAL (XEXP (op0, 1)) == mode_width - 1)
	    {
	      op0 = simplify_and_const_int (NULL_RTX, mode, XEXP (op0, 0),
					    (HOST_WIDE_INT) 1);
	      continue;
	    }
	  break;

	case ASHIFTRT:
	  /* If this is an equality comparison with zero, we can do this
	     as a logical shift, which might be much simpler.  */
	  if (equality_comparison_p && const_op == 0
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT)
	    {
	      op0 = simplify_shift_const (NULL_RTX, LSHIFTRT, mode,
					  XEXP (op0, 0),
					  INTVAL (XEXP (op0, 1)));
	      continue;
	    }

	  /* If OP0 is a sign extension and CODE is not an unsigned comparison,
	     do the comparison in a narrower mode.  */
	  if (! unsigned_comparison_p
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && GET_CODE (XEXP (op0, 0)) == ASHIFT
	      && XEXP (op0, 1) == XEXP (XEXP (op0, 0), 1)
	      && (tmode = mode_for_size (mode_width - INTVAL (XEXP (op0, 1)),
					 MODE_INT, 1)) != BLKmode
	      && ((unsigned HOST_WIDE_INT) const_op <= GET_MODE_MASK (tmode)
		  || ((unsigned HOST_WIDE_INT) - const_op
		      <= GET_MODE_MASK (tmode))))
	    {
	      op0 = gen_lowpart_for_combine (tmode, XEXP (XEXP (op0, 0), 0));
	      continue;
	    }

	  /* ... fall through ... */
	case LSHIFTRT:
	  /* If we have (compare (xshiftrt FOO N) (const_int C)) and
	     the low order N bits of FOO are not significant, we can do this
	     by comparing FOO with C shifted left N bits so long as no
	     overflow occurs.  */
	  if (GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && INTVAL (XEXP (op0, 1)) >= 0
	      && INTVAL (XEXP (op0, 1)) < HOST_BITS_PER_WIDE_INT
	      && mode_width <= HOST_BITS_PER_WIDE_INT
	      && (significant_bits (XEXP (op0, 0), mode)
		  & (((HOST_WIDE_INT) 1 << INTVAL (XEXP (op0, 1))) - 1)) == 0
	      && (const_op == 0
		  || (floor_log2 (const_op) + INTVAL (XEXP (op0, 1))
		      < mode_width)))
	    {
	      const_op <<= INTVAL (XEXP (op0, 1));
	      op1 = GEN_INT (const_op);
	      op0 = XEXP (op0, 0);
	      continue;
	    }

	  /* If we are using this shift to extract just the sign bit, we
	     can replace this with an LT or GE comparison.  */
	  if (const_op == 0
	      && (equality_comparison_p || sign_bit_comparison_p)
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT
	      && INTVAL (XEXP (op0, 1)) == mode_width - 1)
	    {
	      op0 = XEXP (op0, 0);
	      code = (code == NE || code == GT ? LT : GE);
	      continue;
	    }
	  break;
	}

      break;
    }

  /* Now make any compound operations involved in this comparison.  Then,
     check for an outmost SUBREG on OP0 that isn't doing anything or is
     paradoxical.  The latter case can only occur when it is known that the
     "extra" bits will be zero.  Therefore, it is safe to remove the SUBREG.
     We can never remove a SUBREG for a non-equality comparison because the
     sign bit is in a different place in the underlying object.  */

  op0 = make_compound_operation (op0, op1 == const0_rtx ? COMPARE : SET);
  op1 = make_compound_operation (op1, SET);

  if (GET_CODE (op0) == SUBREG && subreg_lowpart_p (op0)
      && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
      && (code == NE || code == EQ)
      && ((GET_MODE_SIZE (GET_MODE (op0))
	   > GET_MODE_SIZE (GET_MODE (SUBREG_REG (op0))))))
    {
      op0 = SUBREG_REG (op0);
      op1 = gen_lowpart_for_combine (GET_MODE (op0), op1);
    }

  else if (GET_CODE (op0) == SUBREG && subreg_lowpart_p (op0)
	   && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
	   && (code == NE || code == EQ)
	   && (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op0)))
	       <= HOST_BITS_PER_WIDE_INT)
	   && (significant_bits (SUBREG_REG (op0), GET_MODE (SUBREG_REG (op0)))
	       & ~ GET_MODE_MASK (GET_MODE (op0))) == 0
	   && (tem = gen_lowpart_for_combine (GET_MODE (SUBREG_REG (op0)),
					      op1),
	       (significant_bits (tem, GET_MODE (SUBREG_REG (op0)))
		& ~ GET_MODE_MASK (GET_MODE (op0))) == 0))
    op0 = SUBREG_REG (op0), op1 = tem;

  /* We now do the opposite procedure: Some machines don't have compare
     insns in all modes.  If OP0's mode is an integer mode smaller than a
     word and we can't do a compare in that mode, see if there is a larger
     mode for which we can do the compare.  There are a number of cases in
     which we can use the wider mode.  */

  mode = GET_MODE (op0);
  if (mode != VOIDmode && GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD
      && cmp_optab->handlers[(int) mode].insn_code == CODE_FOR_nothing)
    for (tmode = GET_MODE_WIDER_MODE (mode);
	 (tmode != VOIDmode
	  && GET_MODE_BITSIZE (tmode) <= HOST_BITS_PER_WIDE_INT);
	 tmode = GET_MODE_WIDER_MODE (tmode))
      if (cmp_optab->handlers[(int) tmode].insn_code != CODE_FOR_nothing)
	{
	  /* If the only significant bits in OP0 and OP1 are those in the
	     narrower mode and this is an equality or unsigned comparison,
	     we can use the wider mode.  Similarly for sign-extended
	     values and equality or signed comparisons.  */
	  if (((code == EQ || code == NE
		|| code == GEU || code == GTU || code == LEU || code == LTU)
	       && ((significant_bits (op0, tmode) & ~ GET_MODE_MASK (mode))
		   == 0)
	       && ((significant_bits (op1, tmode) & ~ GET_MODE_MASK (mode))
		   == 0))
	      || ((code == EQ || code == NE
		   || code == GE || code == GT || code == LE || code == LT)
		  && (num_sign_bit_copies (op0, tmode)
		      > GET_MODE_BITSIZE (tmode) - GET_MODE_BITSIZE (mode))
		  && (num_sign_bit_copies (op1, tmode)
		      > GET_MODE_BITSIZE (tmode) - GET_MODE_BITSIZE (mode))))
	    {
	      op0 = gen_lowpart_for_combine (tmode, op0);
	      op1 = gen_lowpart_for_combine (tmode, op1);
	      break;
	    }

	  /* If this is a test for negative, we can make an explicit
	     test of the sign bit.  */

	  if (op1 == const0_rtx && (code == LT || code == GE)
	      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	    {
	      op0 = gen_binary (AND, tmode,
				gen_lowpart_for_combine (tmode, op0),
				GEN_INT ((HOST_WIDE_INT) 1
					 << (GET_MODE_BITSIZE (mode) - 1)));
	      code = (code == LT) ? NE : EQ;
	      break;
	    }
	}

  *pop0 = op0;
  *pop1 = op1;

  return code;
}

/* Return 1 if we know that X, a comparison operation, is not operating
   on a floating-point value or is EQ or NE, meaning that we can safely
   reverse it.  */

static int
reversible_comparison_p (x)
     rtx x;
{
  if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT
      || GET_CODE (x) == NE || GET_CODE (x) == EQ)
    return 1;

  switch (GET_MODE_CLASS (GET_MODE (XEXP (x, 0))))
    {
    case MODE_INT:
      return 1;

    case MODE_CC:
      x = get_last_value (XEXP (x, 0));
      return (x && GET_CODE (x) == COMPARE
	      && GET_MODE_CLASS (GET_MODE (XEXP (x, 0))) == MODE_INT);
    }

  return 0;
}

/* Utility function for following routine.  Called when X is part of a value
   being stored into reg_last_set_value.  Sets reg_last_set_table_tick
   for each register mentioned.  Similar to mention_regs in cse.c  */

static void
update_table_tick (x)
     rtx x;
{
  register enum rtx_code code = GET_CODE (x);
  register char *fmt = GET_RTX_FORMAT (code);
  register int i;

  if (code == REG)
    {
      int regno = REGNO (x);
      int endregno = regno + (regno < FIRST_PSEUDO_REGISTER
			      ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);

      for (i = regno; i < endregno; i++)
	reg_last_set_table_tick[i] = label_tick;

      return;
    }
  
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    /* Note that we can't have an "E" in values stored; see
       get_last_value_validate.  */
    if (fmt[i] == 'e')
      update_table_tick (XEXP (x, i));
}

/* Record that REG is set to VALUE in insn INSN.  If VALUE is zero, we
   are saying that the register is clobbered and we no longer know its
   value.  If INSN is zero, don't update reg_last_set; this call is normally
   done with VALUE also zero to invalidate the register.  */

static void
record_value_for_reg (reg, insn, value)
     rtx reg;
     rtx insn;
     rtx value;
{
  int regno = REGNO (reg);
  int endregno = regno + (regno < FIRST_PSEUDO_REGISTER
			  ? HARD_REGNO_NREGS (regno, GET_MODE (reg)) : 1);
  int i;

  /* If VALUE contains REG and we have a previous value for REG, substitute
     the previous value.  */
  if (value && insn && reg_overlap_mentioned_p (reg, value))
    {
      rtx tem;

      /* Set things up so get_last_value is allowed to see anything set up to
	 our insn.  */
      subst_low_cuid = INSN_CUID (insn);
      tem = get_last_value (reg);      

      if (tem)
	value = replace_rtx (copy_rtx (value), reg, tem);
    }

  /* For each register modified, show we don't know its value, that
     its value has been updated, and that we don't know the location of
     the death of the register.  */
  for (i = regno; i < endregno; i ++)
    {
      if (insn)
	reg_last_set[i] = insn;
      reg_last_set_value[i] = 0;
      reg_last_death[i] = 0;
    }

  /* Mark registers that are being referenced in this value.  */
  if (value)
    update_table_tick (value);

  /* Now update the status of each register being set.
     If someone is using this register in this block, set this register
     to invalid since we will get confused between the two lives in this
     basic block.  This makes using this register always invalid.  In cse, we
     scan the table to invalidate all entries using this register, but this
     is too much work for us.  */

  for (i = regno; i < endregno; i++)
    {
      reg_last_set_label[i] = label_tick;
      if (value && reg_last_set_table_tick[i] == label_tick)
	reg_last_set_invalid[i] = 1;
      else
	reg_last_set_invalid[i] = 0;
    }

  /* The value being assigned might refer to X (like in "x++;").  In that
     case, we must replace it with (clobber (const_int 0)) to prevent
     infinite loops.  */
  if (value && ! get_last_value_validate (&value,
					  reg_last_set_label[regno], 0))
    {
      value = copy_rtx (value);
      if (! get_last_value_validate (&value, reg_last_set_label[regno], 1))
	value = 0;
    }

  /* For the main register being modified, update the value.  */
  reg_last_set_value[regno] = value;

}

/* Used for communication between the following two routines.  */
static rtx record_dead_insn;

/* Called via note_stores from record_dead_and_set_regs to handle one
   SET or CLOBBER in an insn.  */

static void
record_dead_and_set_regs_1 (dest, setter)
     rtx dest, setter;
{
  if (GET_CODE (dest) == REG)
    {
      /* If we are setting the whole register, we know its value.  Otherwise
	 show that we don't know the value.  We can handle SUBREG in
	 some cases.  */
      if (GET_CODE (setter) == SET && dest == SET_DEST (setter))
	record_value_for_reg (dest, record_dead_insn, SET_SRC (setter));
      else if (GET_CODE (setter) == SET
	       && GET_CODE (SET_DEST (setter)) == SUBREG
	       && SUBREG_REG (SET_DEST (setter)) == dest
	       && subreg_lowpart_p (SET_DEST (setter)))
	record_value_for_reg (dest, record_dead_insn,
			      gen_lowpart_for_combine (GET_MODE (dest),
						       SET_SRC (setter)));
      else
	record_value_for_reg (dest, record_dead_insn, NULL_RTX);
    }
  else if (GET_CODE (dest) == MEM
	   /* Ignore pushes, they clobber nothing.  */
	   && ! push_operand (dest, GET_MODE (dest)))
    mem_last_set = INSN_CUID (record_dead_insn);
}

/* Update the records of when each REG was most recently set or killed
   for the things done by INSN.  This is the last thing done in processing
   INSN in the combiner loop.

   We update reg_last_set, reg_last_set_value, reg_last_death, and also the
   similar information mem_last_set (which insn most recently modified memory)
   and last_call_cuid (which insn was the most recent subroutine call).  */

static void
record_dead_and_set_regs (insn)
     rtx insn;
{
  register rtx link;
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    {
      if (REG_NOTE_KIND (link) == REG_DEAD)
	reg_last_death[REGNO (XEXP (link, 0))] = insn;
      else if (REG_NOTE_KIND (link) == REG_INC)
	record_value_for_reg (XEXP (link, 0), insn, NULL_RTX);
    }

  if (GET_CODE (insn) == CALL_INSN)
    last_call_cuid = mem_last_set = INSN_CUID (insn);

  record_dead_insn = insn;
  note_stores (PATTERN (insn), record_dead_and_set_regs_1);
}

/* Utility routine for the following function.  Verify that all the registers
   mentioned in *LOC are valid when *LOC was part of a value set when
   label_tick == TICK.  Return 0 if some are not.

   If REPLACE is non-zero, replace the invalid reference with
   (clobber (const_int 0)) and return 1.  This replacement is useful because
   we often can get useful information about the form of a value (e.g., if
   it was produced by a shift that always produces -1 or 0) even though
   we don't know exactly what registers it was produced from.  */

static int
get_last_value_validate (loc, tick, replace)
     rtx *loc;
     int tick;
     int replace;
{
  rtx x = *loc;
  char *fmt = GET_RTX_FORMAT (GET_CODE (x));
  int len = GET_RTX_LENGTH (GET_CODE (x));
  int i;

  if (GET_CODE (x) == REG)
    {
      int regno = REGNO (x);
      int endregno = regno + (regno < FIRST_PSEUDO_REGISTER
			      ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);
      int j;

      for (j = regno; j < endregno; j++)
	if (reg_last_set_invalid[j]
	    /* If this is a pseudo-register that was only set once, it is
	       always valid.  */
	    || (! (regno >= FIRST_PSEUDO_REGISTER && reg_n_sets[regno] == 1)
		&& reg_last_set_label[j] > tick))
	  {
	    if (replace)
	      *loc = gen_rtx (CLOBBER, GET_MODE (x), const0_rtx);
	    return replace;
	  }

      return 1;
    }

  for (i = 0; i < len; i++)
    if ((fmt[i] == 'e'
	 && get_last_value_validate (&XEXP (x, i), tick, replace) == 0)
	/* Don't bother with these.  They shouldn't occur anyway.  */
	|| fmt[i] == 'E')
      return 0;

  /* If we haven't found a reason for it to be invalid, it is valid.  */
  return 1;
}

/* Get the last value assigned to X, if known.  Some registers
   in the value may be replaced with (clobber (const_int 0)) if their value
   is known longer known reliably.  */

static rtx
get_last_value (x)
     rtx x;
{
  int regno;
  rtx value;

  /* If this is a non-paradoxical SUBREG, get the value of its operand and
     then convert it to the desired mode.  If this is a paradoxical SUBREG,
     we cannot predict what values the "extra" bits might have. */
  if (GET_CODE (x) == SUBREG
      && subreg_lowpart_p (x)
      && (GET_MODE_SIZE (GET_MODE (x))
	  <= GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
      && (value = get_last_value (SUBREG_REG (x))) != 0)
    return gen_lowpart_for_combine (GET_MODE (x), value);

  if (GET_CODE (x) != REG)
    return 0;

  regno = REGNO (x);
  value = reg_last_set_value[regno];

  /* If we don't have a value or if it isn't for this basic block, return 0. */

  if (value == 0
      || (reg_n_sets[regno] != 1
	  && (reg_last_set_label[regno] != label_tick)))
    return 0;

  /* If the value was set in a later insn that the ones we are processing,
     we can't use it even if the register was only set once, but make a quick
     check to see if the previous insn set it to something.  This is commonly
     the case when the same pseudo is used by repeated insns.  */

  if (INSN_CUID (reg_last_set[regno]) >= subst_low_cuid)
    {
      rtx insn, set;

      for (insn = prev_nonnote_insn (subst_insn);
	   insn && INSN_CUID (insn) >= subst_low_cuid;
	   insn = prev_nonnote_insn (insn))
	;

      if (insn
	  && (set = single_set (insn)) != 0
	  && rtx_equal_p (SET_DEST (set), x))
	{
	  value = SET_SRC (set);

	  /* Make sure that VALUE doesn't reference X.  Replace any
	     expliit references with a CLOBBER.  If there are any remaining
	     references (rare), don't use the value.  */

	  if (reg_mentioned_p (x, value))
	    value = replace_rtx (copy_rtx (value), x,
				 gen_rtx (CLOBBER, GET_MODE (x), const0_rtx));

	  if (reg_overlap_mentioned_p (x, value))
	    return 0;
	}
      else
	return 0;
    }

  /* If the value has all its registers valid, return it.  */
  if (get_last_value_validate (&value, reg_last_set_label[regno], 0))
    return value;

  /* Otherwise, make a copy and replace any invalid register with
     (clobber (const_int 0)).  If that fails for some reason, return 0.  */

  value = copy_rtx (value);
  if (get_last_value_validate (&value, reg_last_set_label[regno], 1))
    return value;

  return 0;
}

/* Return nonzero if expression X refers to a REG or to memory
   that is set in an instruction more recent than FROM_CUID.  */

static int
use_crosses_set_p (x, from_cuid)
     register rtx x;
     int from_cuid;
{
  register char *fmt;
  register int i;
  register enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      register int regno = REGNO (x);
#ifdef PUSH_ROUNDING
      /* Don't allow uses of the stack pointer to be moved,
	 because we don't know whether the move crosses a push insn.  */
      if (regno == STACK_POINTER_REGNUM)
	return 1;
#endif
      return (reg_last_set[regno]
	      && INSN_CUID (reg_last_set[regno]) > from_cuid);
    }

  if (code == MEM && mem_last_set > from_cuid)
    return 1;

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (use_crosses_set_p (XVECEXP (x, i, j), from_cuid))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && use_crosses_set_p (XEXP (x, i), from_cuid))
	return 1;
    }
  return 0;
}

/* Define three variables used for communication between the following
   routines.  */

static int reg_dead_regno, reg_dead_endregno;
static int reg_dead_flag;

/* Function called via note_stores from reg_dead_at_p.

   If DEST is within [reg_dead_rengno, reg_dead_endregno), set 
   reg_dead_flag to 1 if X is a CLOBBER and to -1 it is a SET.  */

static void
reg_dead_at_p_1 (dest, x)
     rtx dest;
     rtx x;
{
  int regno, endregno;

  if (GET_CODE (dest) != REG)
    return;

  regno = REGNO (dest);
  endregno = regno + (regno < FIRST_PSEUDO_REGISTER 
		      ? HARD_REGNO_NREGS (regno, GET_MODE (dest)) : 1);

  if (reg_dead_endregno > regno && reg_dead_regno < endregno)
    reg_dead_flag = (GET_CODE (x) == CLOBBER) ? 1 : -1;
}

/* Return non-zero if REG is known to be dead at INSN.

   We scan backwards from INSN.  If we hit a REG_DEAD note or a CLOBBER
   referencing REG, it is dead.  If we hit a SET referencing REG, it is
   live.  Otherwise, see if it is live or dead at the start of the basic
   block we are in.  */

static int
reg_dead_at_p (reg, insn)
     rtx reg;
     rtx insn;
{
  int block, i;

  /* Set variables for reg_dead_at_p_1.  */
  reg_dead_regno = REGNO (reg);
  reg_dead_endregno = reg_dead_regno + (reg_dead_regno < FIRST_PSEUDO_REGISTER
					? HARD_REGNO_NREGS (reg_dead_regno,
							    GET_MODE (reg))
					: 1);

  reg_dead_flag = 0;

  /* Scan backwards until we find a REG_DEAD note, SET, CLOBBER, label, or
     beginning of function.  */
  for (; insn && GET_CODE (insn) != CODE_LABEL;
       insn = prev_nonnote_insn (insn))
    {
      note_stores (PATTERN (insn), reg_dead_at_p_1);
      if (reg_dead_flag)
	return reg_dead_flag == 1 ? 1 : 0;

      if (find_regno_note (insn, REG_DEAD, reg_dead_regno))
	return 1;
    }

  /* Get the basic block number that we were in.  */
  if (insn == 0)
    block = 0;
  else
    {
      for (block = 0; block < n_basic_blocks; block++)
	if (insn == basic_block_head[block])
	  break;

      if (block == n_basic_blocks)
	return 0;
    }

  for (i = reg_dead_regno; i < reg_dead_endregno; i++)
    if (basic_block_live_at_start[block][i / REGSET_ELT_BITS]
	& ((REGSET_ELT_TYPE) 1 << (i % REGSET_ELT_BITS)))
      return 0;

  return 1;
}

/* Remove register number REGNO from the dead registers list of INSN.

   Return the note used to record the death, if there was one.  */

rtx
remove_death (regno, insn)
     int regno;
     rtx insn;
{
  register rtx note = find_regno_note (insn, REG_DEAD, regno);

  if (note)
    {
      reg_n_deaths[regno]--;
      remove_note (insn, note);
    }

  return note;
}

/* For each register (hardware or pseudo) used within expression X, if its
   death is in an instruction with cuid between FROM_CUID (inclusive) and
   TO_INSN (exclusive), put a REG_DEAD note for that register in the
   list headed by PNOTES. 

   This is done when X is being merged by combination into TO_INSN.  These
   notes will then be distributed as needed.  */

static void
move_deaths (x, from_cuid, to_insn, pnotes)
     rtx x;
     int from_cuid;
     rtx to_insn;
     rtx *pnotes;
{
  register char *fmt;
  register int len, i;
  register enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      register int regno = REGNO (x);
      register rtx where_dead = reg_last_death[regno];

      if (where_dead && INSN_CUID (where_dead) >= from_cuid
	  && INSN_CUID (where_dead) < INSN_CUID (to_insn))
	{
	  rtx note = remove_death (regno, reg_last_death[regno]);

	  /* It is possible for the call above to return 0.  This can occur
	     when reg_last_death points to I2 or I1 that we combined with.
	     In that case make a new note.  */

	  if (note)
	    {
	      XEXP (note, 1) = *pnotes;
	      *pnotes = note;
	    }
	  else
	    *pnotes = gen_rtx (EXPR_LIST, REG_DEAD, x, *pnotes);

	  reg_n_deaths[regno]++;
	}

      return;
    }

  else if (GET_CODE (x) == SET)
    {
      rtx dest = SET_DEST (x);

      move_deaths (SET_SRC (x), from_cuid, to_insn, pnotes);

      /* In the case of a ZERO_EXTRACT, a STRICT_LOW_PART, or a SUBREG
	 that accesses one word of a multi-word item, some
	 piece of everything register in the expression is used by
	 this insn, so remove any old death.  */

      if (GET_CODE (dest) == ZERO_EXTRACT
	  || GET_CODE (dest) == STRICT_LOW_PART
	  || (GET_CODE (dest) == SUBREG
	      && (((GET_MODE_SIZE (GET_MODE (dest))
		    + UNITS_PER_WORD - 1) / UNITS_PER_WORD)
		  == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))
		       + UNITS_PER_WORD - 1) / UNITS_PER_WORD))))
	{
	  move_deaths (dest, from_cuid, to_insn, pnotes);
	  return;
	}

      /* If this is some other SUBREG, we know it replaces the entire
	 value, so use that as the destination.  */
      if (GET_CODE (dest) == SUBREG)
	dest = SUBREG_REG (dest);

      /* If this is a MEM, adjust deaths of anything used in the address.
	 For a REG (the only other possibility), the entire value is
	 being replaced so the old value is not used in this insn.  */

      if (GET_CODE (dest) == MEM)
	move_deaths (XEXP (dest, 0), from_cuid, to_insn, pnotes);
      return;
    }

  else if (GET_CODE (x) == CLOBBER)
    return;

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    move_deaths (XVECEXP (x, i, j), from_cuid, to_insn, pnotes);
	}
      else if (fmt[i] == 'e')
	move_deaths (XEXP (x, i), from_cuid, to_insn, pnotes);
    }
}

/* Return 1 if X is the target of a bit-field assignment in BODY, the
   pattern of an insn.  X must be a REG.  */

static int
reg_bitfield_target_p (x, body)
     rtx x;
     rtx body;
{
  int i;

  if (GET_CODE (body) == SET)
    {
      rtx dest = SET_DEST (body);
      rtx target;
      int regno, tregno, endregno, endtregno;

      if (GET_CODE (dest) == ZERO_EXTRACT)
	target = XEXP (dest, 0);
      else if (GET_CODE (dest) == STRICT_LOW_PART)
	target = SUBREG_REG (XEXP (dest, 0));
      else
	return 0;

      if (GET_CODE (target) == SUBREG)
	target = SUBREG_REG (target);

      if (GET_CODE (target) != REG)
	return 0;

      tregno = REGNO (target), regno = REGNO (x);
      if (tregno >= FIRST_PSEUDO_REGISTER || regno >= FIRST_PSEUDO_REGISTER)
	return target == x;

      endtregno = tregno + HARD_REGNO_NREGS (tregno, GET_MODE (target));
      endregno = regno + HARD_REGNO_NREGS (regno, GET_MODE (x));

      return endregno > tregno && regno < endtregno;
    }

  else if (GET_CODE (body) == PARALLEL)
    for (i = XVECLEN (body, 0) - 1; i >= 0; i--)
      if (reg_bitfield_target_p (x, XVECEXP (body, 0, i)))
	return 1;

  return 0;
}      

/* Given a chain of REG_NOTES originally from FROM_INSN, try to place them
   as appropriate.  I3 and I2 are the insns resulting from the combination
   insns including FROM (I2 may be zero).

   ELIM_I2 and ELIM_I1 are either zero or registers that we know will
   not need REG_DEAD notes because they are being substituted for.  This
   saves searching in the most common cases.

   Each note in the list is either ignored or placed on some insns, depending
   on the type of note.  */

static void
distribute_notes (notes, from_insn, i3, i2, elim_i2, elim_i1)
     rtx notes;
     rtx from_insn;
     rtx i3, i2;
     rtx elim_i2, elim_i1;
{
  rtx note, next_note;
  rtx tem;

  for (note = notes; note; note = next_note)
    {
      rtx place = 0, place2 = 0;

      /* If this NOTE references a pseudo register, ensure it references
	 the latest copy of that register.  */
      if (XEXP (note, 0) && GET_CODE (XEXP (note, 0)) == REG
	  && REGNO (XEXP (note, 0)) >= FIRST_PSEUDO_REGISTER)
	XEXP (note, 0) = regno_reg_rtx[REGNO (XEXP (note, 0))];

      next_note = XEXP (note, 1);
      switch (REG_NOTE_KIND (note))
	{
	case REG_UNUSED:
	  /* If this register is set or clobbered in I3, put the note there
	     unless there is one already.  */
	  if (reg_set_p (XEXP (note, 0), PATTERN (i3)))
	    {
	      if (! (GET_CODE (XEXP (note, 0)) == REG
		     ? find_regno_note (i3, REG_UNUSED, REGNO (XEXP (note, 0)))
		     : find_reg_note (i3, REG_UNUSED, XEXP (note, 0))))
		place = i3;
	    }
	  /* Otherwise, if this register is used by I3, then this register
	     now dies here, so we must put a REG_DEAD note here unless there
	     is one already.  */
	  else if (reg_referenced_p (XEXP (note, 0), PATTERN (i3))
		   && ! (GET_CODE (XEXP (note, 0)) == REG
			 ? find_regno_note (i3, REG_DEAD, REGNO (XEXP (note, 0)))
			 : find_reg_note (i3, REG_DEAD, XEXP (note, 0))))
	    {
	      PUT_REG_NOTE_KIND (note, REG_DEAD);
	      place = i3;
	    }
	  break;

	case REG_EQUAL:
	case REG_EQUIV:
	case REG_NONNEG:
	  /* These notes say something about results of an insn.  We can
	     only support them if they used to be on I3 in which case they
	     remain on I3.  Otherwise they are ignored.

	     If the note refers to an expression that is not a constant, we
	     must also ignore the note since we cannot tell whether the
	     equivalence is still true.  It might be possible to do
	     slightly better than this (we only have a problem if I2DEST
	     or I1DEST is present in the expression), but it doesn't
	     seem worth the trouble.  */

	  if (from_insn == i3
	      && (XEXP (note, 0) == 0 || CONSTANT_P (XEXP (note, 0))))
	    place = i3;
	  break;

	case REG_INC:
	case REG_NO_CONFLICT:
	case REG_LABEL:
	  /* These notes say something about how a register is used.  They must
	     be present on any use of the register in I2 or I3.  */
	  if (reg_mentioned_p (XEXP (note, 0), PATTERN (i3)))
	    place = i3;

	  if (i2 && reg_mentioned_p (XEXP (note, 0), PATTERN (i2)))
	    {
	      if (place)
		place2 = i2;
	      else
		place = i2;
	    }
	  break;

	case REG_WAS_0:
	  /* It is too much trouble to try to see if this note is still
	     correct in all situations.  It is better to simply delete it.  */
	  break;

	case REG_RETVAL:
	  /* If the insn previously containing this note still exists,
	     put it back where it was.  Otherwise move it to the previous
	     insn.  Adjust the corresponding REG_LIBCALL note.  */
	  if (GET_CODE (from_insn) != NOTE)
	    place = from_insn;
	  else
	    {
	      tem = find_reg_note (XEXP (note, 0), REG_LIBCALL, NULL_RTX);
	      place = prev_real_insn (from_insn);
	      if (tem && place)
		XEXP (tem, 0) = place;
	    }
	  break;

	case REG_LIBCALL:
	  /* This is handled similarly to REG_RETVAL.  */
	  if (GET_CODE (from_insn) != NOTE)
	    place = from_insn;
	  else
	    {
	      tem = find_reg_note (XEXP (note, 0), REG_RETVAL, NULL_RTX);
	      place = next_real_insn (from_insn);
	      if (tem && place)
		XEXP (tem, 0) = place;
	    }
	  break;

	case REG_DEAD:
	  /* If the register is used as an input in I3, it dies there.
	     Similarly for I2, if it is non-zero and adjacent to I3.

	     If the register is not used as an input in either I3 or I2
	     and it is not one of the registers we were supposed to eliminate,
	     there are two possibilities.  We might have a non-adjacent I2
	     or we might have somehow eliminated an additional register
	     from a computation.  For example, we might have had A & B where
	     we discover that B will always be zero.  In this case we will
	     eliminate the reference to A.

	     In both cases, we must search to see if we can find a previous
	     use of A and put the death note there.  */

	  if (reg_referenced_p (XEXP (note, 0), PATTERN (i3)))
	    place = i3;
	  else if (i2 != 0 && next_nonnote_insn (i2) == i3
		   && reg_referenced_p (XEXP (note, 0), PATTERN (i2)))
	    place = i2;

	  if (XEXP (note, 0) == elim_i2 || XEXP (note, 0) == elim_i1)
	    break;

	  /* If the register is used in both I2 and I3 and it dies in I3, 
	     we might have added another reference to it.  If reg_n_refs
	     was 2, bump it to 3.  This has to be correct since the 
	     register must have been set somewhere.  The reason this is
	     done is because local-alloc.c treats 2 references as a 
	     special case.  */

	  if (place == i3 && i2 != 0 && GET_CODE (XEXP (note, 0)) == REG
	      && reg_n_refs[REGNO (XEXP (note, 0))]== 2
	      && reg_referenced_p (XEXP (note, 0), PATTERN (i2)))
	    reg_n_refs[REGNO (XEXP (note, 0))] = 3;

	  if (place == 0)
	    for (tem = prev_nonnote_insn (i3);
		 tem && (GET_CODE (tem) == INSN
			 || GET_CODE (tem) == CALL_INSN);
		 tem = prev_nonnote_insn (tem))
	      {
		/* If the register is being set at TEM, see if that is all
		   TEM is doing.  If so, delete TEM.  Otherwise, make this
		   into a REG_UNUSED note instead.  */
		if (reg_set_p (XEXP (note, 0), PATTERN (tem)))
		  {
		    rtx set = single_set (tem);

		    /* Verify that it was the set, and not a clobber that
		       modified the register.  */

		    if (set != 0 && ! side_effects_p (SET_SRC (set))
			&& rtx_equal_p (XEXP (note, 0), SET_DEST (set)))
		      {
			/* Move the notes and links of TEM elsewhere.
			   This might delete other dead insns recursively. 
			   First set the pattern to something that won't use
			   any register.  */

			PATTERN (tem) = pc_rtx;

			distribute_notes (REG_NOTES (tem), tem, tem,
					  NULL_RTX, NULL_RTX, NULL_RTX);
			distribute_links (LOG_LINKS (tem));

			PUT_CODE (tem, NOTE);
			NOTE_LINE_NUMBER (tem) = NOTE_INSN_DELETED;
			NOTE_SOURCE_FILE (tem) = 0;
		      }
		    else
		      {
			PUT_REG_NOTE_KIND (note, REG_UNUSED);

			/*  If there isn't already a REG_UNUSED note, put one
			    here.  */
			if (! find_regno_note (tem, REG_UNUSED,
					       REGNO (XEXP (note, 0))))
			  place = tem;
			break;
		      }
		  }
		else if (reg_referenced_p (XEXP (note, 0), PATTERN (tem)))
		  {
		    place = tem;
		    break;
		  }
	      }

	  /* If the register is set or already dead at PLACE, we needn't do
	     anything with this note if it is still a REG_DEAD note.  

	     Note that we cannot use just `dead_or_set_p' here since we can
	     convert an assignment to a register into a bit-field assignment.
	     Therefore, we must also omit the note if the register is the 
	     target of a bitfield assignment.  */
	     
	  if (place && REG_NOTE_KIND (note) == REG_DEAD)
	    {
	      int regno = REGNO (XEXP (note, 0));

	      if (dead_or_set_p (place, XEXP (note, 0))
		  || reg_bitfield_target_p (XEXP (note, 0), PATTERN (place)))
		{
		  /* Unless the register previously died in PLACE, clear
		     reg_last_death.  [I no longer understand why this is
		     being done.] */
		  if (reg_last_death[regno] != place)
		    reg_last_death[regno] = 0;
		  place = 0;
		}
	      else
		reg_last_death[regno] = place;

	      /* If this is a death note for a hard reg that is occupying
		 multiple registers, ensure that we are still using all
		 parts of the object.  If we find a piece of the object
		 that is unused, we must add a USE for that piece before
		 PLACE and put the appropriate REG_DEAD note on it.

		 An alternative would be to put a REG_UNUSED for the pieces
		 on the insn that set the register, but that can't be done if
		 it is not in the same block.  It is simpler, though less
		 efficient, to add the USE insns.  */

	      if (place && regno < FIRST_PSEUDO_REGISTER
		  && HARD_REGNO_NREGS (regno, GET_MODE (XEXP (note, 0))) > 1)
		{
		  int endregno
		    = regno + HARD_REGNO_NREGS (regno,
						GET_MODE (XEXP (note, 0)));
		  int all_used = 1;
		  int i;

		  for (i = regno; i < endregno; i++)
		    if (! refers_to_regno_p (i, i + 1, PATTERN (place), 0))
		      {
			rtx piece = gen_rtx (REG, word_mode, i);
			rtx p;

			/* See if we already placed a USE note for this
			   register in front of PLACE.  */
			for (p = place;
			     GET_CODE (PREV_INSN (p)) == INSN
			     && GET_CODE (PATTERN (PREV_INSN (p))) == USE;
			     p = PREV_INSN (p))
			  if (rtx_equal_p (piece,
					   XEXP (PATTERN (PREV_INSN (p)), 0)))
			    {
			      p = 0;
			      break;
			    }

			if (p)
			  {
			    rtx use_insn
			      = emit_insn_before (gen_rtx (USE, VOIDmode,
							   piece),
						  p);
			    REG_NOTES (use_insn)
			      = gen_rtx (EXPR_LIST, REG_DEAD, piece,
					 REG_NOTES (use_insn));
			  }

			all_used = 0;
		      }

		  if (! all_used)
		    {
		      /* Put only REG_DEAD notes for pieces that are
			 still used and that are not already dead or set.  */

		      for (i = regno; i < endregno; i++)
			{
			  rtx piece = gen_rtx (REG, word_mode, i);

			  if (reg_referenced_p (piece, PATTERN (place))
			      && ! dead_or_set_p (place, piece)
			      && ! reg_bitfield_target_p (piece,
							  PATTERN (place)))
			    REG_NOTES (place) = gen_rtx (EXPR_LIST, REG_DEAD,
							 piece,
							 REG_NOTES (place));
			}

		      place = 0;
		    }
		}
	    }
	  break;

	default:
	  /* Any other notes should not be present at this point in the
	     compilation.  */
	  abort ();
	}

      if (place)
	{
	  XEXP (note, 1) = REG_NOTES (place);
	  REG_NOTES (place) = note;
	}
      else if ((REG_NOTE_KIND (note) == REG_DEAD
		|| REG_NOTE_KIND (note) == REG_UNUSED)
	       && GET_CODE (XEXP (note, 0)) == REG)
	reg_n_deaths[REGNO (XEXP (note, 0))]--;

      if (place2)
	{
	  if ((REG_NOTE_KIND (note) == REG_DEAD
	       || REG_NOTE_KIND (note) == REG_UNUSED)
	      && GET_CODE (XEXP (note, 0)) == REG)
	    reg_n_deaths[REGNO (XEXP (note, 0))]++;

	  REG_NOTES (place2) = gen_rtx (GET_CODE (note), REG_NOTE_KIND (note),
					XEXP (note, 0), REG_NOTES (place2));
	}
    }
}

/* Similarly to above, distribute the LOG_LINKS that used to be present on
   I3, I2, and I1 to new locations.  This is also called in one case to
   add a link pointing at I3 when I3's destination is changed.  */

static void
distribute_links (links)
     rtx links;
{
  rtx link, next_link;

  for (link = links; link; link = next_link)
    {
      rtx place = 0;
      rtx insn;
      rtx set, reg;

      next_link = XEXP (link, 1);

      /* If the insn that this link points to is a NOTE or isn't a single
	 set, ignore it.  In the latter case, it isn't clear what we
	 can do other than ignore the link, since we can't tell which 
	 register it was for.  Such links wouldn't be used by combine
	 anyway.

	 It is not possible for the destination of the target of the link to
	 have been changed by combine.  The only potential of this is if we
	 replace I3, I2, and I1 by I3 and I2.  But in that case the
	 destination of I2 also remains unchanged.  */

      if (GET_CODE (XEXP (link, 0)) == NOTE
	  || (set = single_set (XEXP (link, 0))) == 0)
	continue;

      reg = SET_DEST (set);
      while (GET_CODE (reg) == SUBREG || GET_CODE (reg) == ZERO_EXTRACT
	     || GET_CODE (reg) == SIGN_EXTRACT
	     || GET_CODE (reg) == STRICT_LOW_PART)
	reg = XEXP (reg, 0);

      /* A LOG_LINK is defined as being placed on the first insn that uses
	 a register and points to the insn that sets the register.  Start
	 searching at the next insn after the target of the link and stop
	 when we reach a set of the register or the end of the basic block.

	 Note that this correctly handles the link that used to point from
	 I3 to I2.  Also note that not much searching is typically done here
	 since most links don't point very far away.  */

      for (insn = NEXT_INSN (XEXP (link, 0));
	   (insn && GET_CODE (insn) != CODE_LABEL
	    && GET_CODE (PREV_INSN (insn)) != JUMP_INSN);
	   insn = NEXT_INSN (insn))
	if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	    && reg_overlap_mentioned_p (reg, PATTERN (insn)))
	  {
	    if (reg_referenced_p (reg, PATTERN (insn)))
	      place = insn;
	    break;
	  }

      /* If we found a place to put the link, place it there unless there
	 is already a link to the same insn as LINK at that point.  */

      if (place)
	{
	  rtx link2;

	  for (link2 = LOG_LINKS (place); link2; link2 = XEXP (link2, 1))
	    if (XEXP (link2, 0) == XEXP (link, 0))
	      break;

	  if (link2 == 0)
	    {
	      XEXP (link, 1) = LOG_LINKS (place);
	      LOG_LINKS (place) = link;
	    }
	}
    }
}

void
dump_combine_stats (file)
     FILE *file;
{
  fprintf
    (file,
     ";; Combiner statistics: %d attempts, %d substitutions (%d requiring new space),\n;; %d successes.\n\n",
     combine_attempts, combine_merges, combine_extras, combine_successes);
}

void
dump_combine_total_stats (file)
     FILE *file;
{
  fprintf
    (file,
     "\n;; Combiner totals: %d attempts, %d substitutions (%d requiring new space),\n;; %d successes.\n",
     total_attempts, total_merges, total_extras, total_successes);
}
