/* Allocate registers for pseudo-registers that span basic blocks.
   Copyright (C) 1987, 1988, 1991 Free Software Foundation, Inc.

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


#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "flags.h"
#include "basic-block.h"
#include "hard-reg-set.h"
#include "regs.h"
#include "insn-config.h"
#include "output.h"

/* This pass of the compiler performs global register allocation.
   It assigns hard register numbers to all the pseudo registers
   that were not handled in local_alloc.  Assignments are recorded
   in the vector reg_renumber, not by changing the rtl code.
   (Such changes are made by final).  The entry point is
   the function global_alloc.

   After allocation is complete, the reload pass is run as a subroutine
   of this pass, so that when a pseudo reg loses its hard reg due to
   spilling it is possible to make a second attempt to find a hard
   reg for it.  The reload pass is independent in other respects
   and it is run even when stupid register allocation is in use.

   1. count the pseudo-registers still needing allocation
   and assign allocation-numbers (allocnos) to them.
   Set up tables reg_allocno and allocno_reg to map 
   reg numbers to allocnos and vice versa.
   max_allocno gets the number of allocnos in use.

   2. Allocate a max_allocno by max_allocno conflict bit matrix and clear it.
   Allocate a max_allocno by FIRST_PSEUDO_REGISTER conflict matrix
   for conflicts between allocnos and explicit hard register use
   (which includes use of pseudo-registers allocated by local_alloc).

   3. for each basic block
    walk forward through the block, recording which
    unallocated registers and which hardware registers are live.
    Build the conflict matrix between the unallocated registers
    and another of unallocated registers versus hardware registers.
    Also record the preferred hardware registers
    for each unallocated one.

   4. Sort a table of the allocnos into order of
   desirability of the variables.

   5. Allocate the variables in that order; each if possible into
   a preferred register, else into another register.  */

/* Number of pseudo-registers still requiring allocation
   (not allocated by local_allocate).  */

static int max_allocno;

/* Indexed by (pseudo) reg number, gives the allocno, or -1
   for pseudo registers already allocated by local_allocate.  */

static int *reg_allocno;

/* Indexed by allocno, gives the reg number.  */

static int *allocno_reg;

/* A vector of the integers from 0 to max_allocno-1,
   sorted in the order of first-to-be-allocated first.  */

static int *allocno_order;

/* Indexed by an allocno, gives the number of consecutive
   hard registers needed by that pseudo reg.  */

static int *allocno_size;

/* Indexed by (pseudo) reg number, gives the number of another
   lower-numbered pseudo reg which can share a hard reg with this pseudo
   *even if the two pseudos would otherwise appear to conflict*.  */

static int *reg_may_share;

/* Define the number of bits in each element of `conflicts' and what
   type that element has.  We use the largest integer format on the
   host machine.  */

#define INT_BITS HOST_BITS_PER_WIDE_INT
#define INT_TYPE HOST_WIDE_INT

/* max_allocno by max_allocno array of bits,
   recording whether two allocno's conflict (can't go in the same
   hardware register).

   `conflicts' is not symmetric; a conflict between allocno's i and j
   is recorded either in element i,j or in element j,i.  */

static INT_TYPE *conflicts;

/* Number of ints require to hold max_allocno bits.
   This is the length of a row in `conflicts'.  */

static int allocno_row_words;

/* Two macros to test or store 1 in an element of `conflicts'.  */

#define CONFLICTP(I, J) \
 (conflicts[(I) * allocno_row_words + (J) / INT_BITS]	\
  & ((INT_TYPE) 1 << ((J) % INT_BITS)))

#define SET_CONFLICT(I, J) \
 (conflicts[(I) * allocno_row_words + (J) / INT_BITS]	\
  |= ((INT_TYPE) 1 << ((J) % INT_BITS)))

/* Set of hard regs currently live (during scan of all insns).  */

static HARD_REG_SET hard_regs_live;

/* Indexed by N, set of hard regs conflicting with allocno N.  */

static HARD_REG_SET *hard_reg_conflicts;

/* Indexed by N, set of hard regs preferred by allocno N.
   This is used to make allocnos go into regs that are copied to or from them,
   when possible, to reduce register shuffling.  */

static HARD_REG_SET *hard_reg_preferences;

/* Similar, but just counts register preferences made in simple copy
   operations, rather than arithmetic.  These are given priority because
   we can always eliminate an insn by using these, but using a register
   in the above list won't always eliminate an insn.  */

static HARD_REG_SET *hard_reg_copy_preferences;

/* Similar to hard_reg_preferences, but includes bits for subsequent
   registers when an allocno is multi-word.  The above variable is used for
   allocation while this is used to build reg_someone_prefers, below.  */

static HARD_REG_SET *hard_reg_full_preferences;

/* Indexed by N, set of hard registers that some later allocno has a
   preference for.  */

static HARD_REG_SET *regs_someone_prefers;

/* Set of registers that global-alloc isn't supposed to use.  */

static HARD_REG_SET no_global_alloc_regs;

/* Set of registers used so far.  */

static HARD_REG_SET regs_used_so_far;

/* Number of calls crossed by each allocno.  */

static int *allocno_calls_crossed;

/* Number of refs (weighted) to each allocno.  */

static int *allocno_n_refs;

/* Guess at live length of each allocno.
   This is actually the max of the live lengths of the regs.  */

static int *allocno_live_length;

/* Number of refs (weighted) to each hard reg, as used by local alloc.
   It is zero for a reg that contains global pseudos or is explicitly used.  */

static int local_reg_n_refs[FIRST_PSEUDO_REGISTER];

/* Guess at live length of each hard reg, as used by local alloc.
   This is actually the sum of the live lengths of the specific regs.  */

static int local_reg_live_length[FIRST_PSEUDO_REGISTER];

/* Test a bit in TABLE, a vector of HARD_REG_SETs,
   for vector element I, and hard register number J.  */

#define REGBITP(TABLE, I, J)     TEST_HARD_REG_BIT (TABLE[I], J)

/* Set to 1 a bit in a vector of HARD_REG_SETs.  Works like REGBITP.  */

#define SET_REGBIT(TABLE, I, J)  SET_HARD_REG_BIT (TABLE[I], J)

/* Bit mask for allocnos live at current point in the scan.  */

static INT_TYPE *allocnos_live;

/* Test, set or clear bit number I in allocnos_live,
   a bit vector indexed by allocno.  */

#define ALLOCNO_LIVE_P(I) \
  (allocnos_live[(I) / INT_BITS] & ((INT_TYPE) 1 << ((I) % INT_BITS)))

#define SET_ALLOCNO_LIVE(I) \
  (allocnos_live[(I) / INT_BITS] |= ((INT_TYPE) 1 << ((I) % INT_BITS)))

#define CLEAR_ALLOCNO_LIVE(I) \
  (allocnos_live[(I) / INT_BITS] &= ~((INT_TYPE) 1 << ((I) % INT_BITS)))

/* This is turned off because it doesn't work right for DImode.
   (And it is only used for DImode, so the other cases are worthless.)
   The problem is that it isn't true that there is NO possibility of conflict;
   only that there is no conflict if the two pseudos get the exact same regs.
   If they were allocated with a partial overlap, there would be a conflict.
   We can't safely turn off the conflict unless we have another way to
   prevent the partial overlap.

   Idea: change hard_reg_conflicts so that instead of recording which
   hard regs the allocno may not overlap, it records where the allocno
   may not start.  Change both where it is used and where it is updated.
   Then there is a way to record that (reg:DI 108) may start at 10
   but not at 9 or 11.  There is still the question of how to record
   this semi-conflict between two pseudos.  */
#if 0
/* Reg pairs for which conflict after the current insn
   is inhibited by a REG_NO_CONFLICT note.
   If the table gets full, we ignore any other notes--that is conservative.  */
#define NUM_NO_CONFLICT_PAIRS 4
/* Number of pairs in use in this insn.  */
int n_no_conflict_pairs;
static struct { int allocno1, allocno2;}
  no_conflict_pairs[NUM_NO_CONFLICT_PAIRS];
#endif /* 0 */

/* Record all regs that are set in any one insn.
   Communication from mark_reg_{store,clobber} and global_conflicts.  */

static rtx *regs_set;
static int n_regs_set;

/* All register that can be eliminated.  */

static HARD_REG_SET eliminable_regset;

static int allocno_compare ();
static void mark_reg_store ();
static void mark_reg_clobber ();
static void mark_reg_conflicts ();
static void mark_reg_live_nc ();
static void mark_reg_death ();
static void dump_conflicts ();
void dump_global_regs ();
static void find_reg ();
static void global_conflicts ();
static void expand_preferences ();
static void prune_preferences ();
static void record_conflicts ();
static void set_preference ();

/* Perform allocation of pseudo-registers not allocated by local_alloc.
   FILE is a file to output debugging information on,
   or zero if such output is not desired.

   Return value is nonzero if reload failed
   and we must not do any more for this function.  */

int
global_alloc (file)
     FILE *file;
{
#ifdef ELIMINABLE_REGS
  static struct {int from, to; } eliminables[] = ELIMINABLE_REGS;
#endif
  register int i;
  rtx x;

  max_allocno = 0;

  /* A machine may have certain hard registers that
     are safe to use only within a basic block.  */

  CLEAR_HARD_REG_SET (no_global_alloc_regs);
#ifdef OVERLAPPING_REGNO_P
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (OVERLAPPING_REGNO_P (i))
      SET_HARD_REG_BIT (no_global_alloc_regs, i);
#endif

  /* Build the regset of all eliminable registers and show we can't use those
     that we already know won't be eliminated.  */
#ifdef ELIMINABLE_REGS
  for (i = 0; i < sizeof eliminables / sizeof eliminables[0]; i++)
    {
      SET_HARD_REG_BIT (eliminable_regset, eliminables[i].from);

      if (! CAN_ELIMINATE (eliminables[i].from, eliminables[i].to)
	  || (eliminables[i].from == FRAME_POINTER_REGNUM
	      && (! flag_omit_frame_pointer || FRAME_POINTER_REQUIRED)))
	SET_HARD_REG_BIT (no_global_alloc_regs, eliminables[i].from);
    }
#else
  SET_HARD_REG_BIT (eliminable_regset, FRAME_POINTER_REGNUM);

  /* If we know we will definitely not be eliminating the frame pointer,
     don't allocate it.  */
  if (! flag_omit_frame_pointer || FRAME_POINTER_REQUIRED)
    SET_HARD_REG_BIT (no_global_alloc_regs, FRAME_POINTER_REGNUM);
#endif

  /* Track which registers have already been used.  Start with registers
     explicitly in the rtl, then registers allocated by local register
     allocation.  */

  CLEAR_HARD_REG_SET (regs_used_so_far);
#ifdef LEAF_REGISTERS
  /* If we are doing the leaf function optimization, and this is a leaf
     function, it means that the registers that take work to save are those
     that need a register window.  So prefer the ones that can be used in
     a leaf function.  */
  {
    char *cheap_regs;
    static char leaf_regs[] = LEAF_REGISTERS;

    if (only_leaf_regs_used () && leaf_function_p ())
      cheap_regs = leaf_regs;
    else
      cheap_regs = call_used_regs;
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (regs_ever_live[i] || cheap_regs[i])
	SET_HARD_REG_BIT (regs_used_so_far, i);
  }
#else
  /* We consider registers that do not have to be saved over calls as if
     they were already used since there is no cost in using them.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i] || call_used_regs[i])
      SET_HARD_REG_BIT (regs_used_so_far, i);
#endif

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] >= 0)
      SET_HARD_REG_BIT (regs_used_so_far, reg_renumber[i]);

  /* Establish mappings from register number to allocation number
     and vice versa.  In the process, count the allocnos.  */

  reg_allocno = (int *) alloca (max_regno * sizeof (int));

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    reg_allocno[i] = -1;

  /* Initialize the shared-hard-reg mapping
     from the list of pairs that may share.  */
  reg_may_share = (int *) alloca (max_regno * sizeof (int));
  bzero (reg_may_share, max_regno * sizeof (int));
  for (x = regs_may_share; x; x = XEXP (XEXP (x, 1), 1))
    {
      int r1 = REGNO (XEXP (x, 0));
      int r2 = REGNO (XEXP (XEXP (x, 1), 0));
      if (r1 > r2)
	reg_may_share[r1] = r2;
      else
	reg_may_share[r2] = r1;
    }

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    /* Note that reg_live_length[i] < 0 indicates a "constant" reg
       that we are supposed to refrain from putting in a hard reg.
       -2 means do make an allocno but don't allocate it.  */
    if (reg_n_refs[i] != 0 && reg_renumber[i] < 0 && reg_live_length[i] != -1
	/* Don't allocate pseudos that cross calls,
	   if this function receives a nonlocal goto.  */
	&& (! current_function_has_nonlocal_label
	    || reg_n_calls_crossed[i] == 0))
      {
	if (reg_may_share[i] && reg_allocno[reg_may_share[i]] >= 0)
	  reg_allocno[i] = reg_allocno[reg_may_share[i]];
	else
	  reg_allocno[i] = max_allocno++;
	if (reg_live_length[i] == 0)
	  abort ();
      }
    else
      reg_allocno[i] = -1;

  allocno_reg = (int *) alloca (max_allocno * sizeof (int));
  allocno_size = (int *) alloca (max_allocno * sizeof (int));
  allocno_calls_crossed = (int *) alloca (max_allocno * sizeof (int));
  allocno_n_refs = (int *) alloca (max_allocno * sizeof (int));
  allocno_live_length = (int *) alloca (max_allocno * sizeof (int));
  bzero (allocno_size, max_allocno * sizeof (int));
  bzero (allocno_calls_crossed, max_allocno * sizeof (int));
  bzero (allocno_n_refs, max_allocno * sizeof (int));
  bzero (allocno_live_length, max_allocno * sizeof (int));

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_allocno[i] >= 0)
      {
	int allocno = reg_allocno[i];
	allocno_reg[allocno] = i;
	allocno_size[allocno] = PSEUDO_REGNO_SIZE (i);
	allocno_calls_crossed[allocno] += reg_n_calls_crossed[i];
	allocno_n_refs[allocno] += reg_n_refs[i];
	if (allocno_live_length[allocno] < reg_live_length[i])
	  allocno_live_length[allocno] = reg_live_length[i];
      }

  /* Calculate amount of usage of each hard reg by pseudos
     allocated by local-alloc.  This is to see if we want to
     override it.  */
  bzero (local_reg_live_length, sizeof local_reg_live_length);
  bzero (local_reg_n_refs, sizeof local_reg_n_refs);
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_allocno[i] < 0 && reg_renumber[i] >= 0)
      {
	int regno = reg_renumber[i];
	int endregno = regno + HARD_REGNO_NREGS (regno, PSEUDO_REGNO_MODE (i));
	int j;

	for (j = regno; j < endregno; j++)
	  {
	    local_reg_n_refs[j] += reg_n_refs[i];
	    local_reg_live_length[j] += reg_live_length[i];
	  }
      }

  /* We can't override local-alloc for a reg used not just by local-alloc.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i])
      local_reg_n_refs[i] = 0;

  /* Allocate the space for the conflict and preference tables and
     initialize them.  */

  hard_reg_conflicts
    = (HARD_REG_SET *) alloca (max_allocno * sizeof (HARD_REG_SET));
  bzero (hard_reg_conflicts, max_allocno * sizeof (HARD_REG_SET));

  hard_reg_preferences
    = (HARD_REG_SET *) alloca (max_allocno * sizeof (HARD_REG_SET));
  bzero (hard_reg_preferences, max_allocno * sizeof (HARD_REG_SET));
  
  hard_reg_copy_preferences
    = (HARD_REG_SET *) alloca (max_allocno * sizeof (HARD_REG_SET));
  bzero (hard_reg_copy_preferences, max_allocno * sizeof (HARD_REG_SET));
  
  hard_reg_full_preferences
    = (HARD_REG_SET *) alloca (max_allocno * sizeof (HARD_REG_SET));
  bzero (hard_reg_full_preferences, max_allocno * sizeof (HARD_REG_SET));
  
  regs_someone_prefers
    = (HARD_REG_SET *) alloca (max_allocno * sizeof (HARD_REG_SET));
  bzero (regs_someone_prefers, max_allocno * sizeof (HARD_REG_SET));

  allocno_row_words = (max_allocno + INT_BITS - 1) / INT_BITS;

  conflicts = (INT_TYPE *) alloca (max_allocno * allocno_row_words
				   * sizeof (INT_TYPE));
  bzero (conflicts, max_allocno * allocno_row_words
	 * sizeof (INT_TYPE));

  allocnos_live = (INT_TYPE *) alloca (allocno_row_words * sizeof (INT_TYPE));

  /* If there is work to be done (at least one reg to allocate),
     perform global conflict analysis and allocate the regs.  */

  if (max_allocno > 0)
    {
      /* Scan all the insns and compute the conflicts among allocnos
	 and between allocnos and hard regs.  */

      global_conflicts ();

      /* Eliminate conflicts between pseudos and eliminable registers.  If
	 the register is not eliminated, the pseudo won't really be able to
	 live in the eliminable register, so the conflict doesn't matter.
	 If we do eliminate the register, the conflict will no longer exist.
	 So in either case, we can ignore the conflict.  Likewise for
	 preferences.  */

      for (i = 0; i < max_allocno; i++)
	{
	  AND_COMPL_HARD_REG_SET (hard_reg_conflicts[i], eliminable_regset);
	  AND_COMPL_HARD_REG_SET (hard_reg_copy_preferences[i],
				  eliminable_regset);
	  AND_COMPL_HARD_REG_SET (hard_reg_preferences[i], eliminable_regset);
	}

      /* Try to expand the preferences by merging them between allocnos.  */

      expand_preferences ();

      /* Determine the order to allocate the remaining pseudo registers.  */

      allocno_order = (int *) alloca (max_allocno * sizeof (int));
      for (i = 0; i < max_allocno; i++)
	allocno_order[i] = i;

      /* Default the size to 1, since allocno_compare uses it to divide by.
	 Also convert allocno_live_length of zero to -1.  A length of zero
	 can occur when all the registers for that allocno have reg_live_length
	 equal to -2.  In this case, we want to make an allocno, but not
	 allocate it.  So avoid the divide-by-zero and set it to a low
	 priority.  */

      for (i = 0; i < max_allocno; i++)
	{
	  if (allocno_size[i] == 0)
	    allocno_size[i] = 1;
	  if (allocno_live_length[i] == 0)
	    allocno_live_length[i] = -1;
	}

      qsort (allocno_order, max_allocno, sizeof (int), allocno_compare);
      
      prune_preferences ();

      if (file)
	dump_conflicts (file);

      /* Try allocating them, one by one, in that order,
	 except for parameters marked with reg_live_length[regno] == -2.  */

      for (i = 0; i < max_allocno; i++)
	if (reg_live_length[allocno_reg[allocno_order[i]]] >= 0)
	  {
	    /* If we have more than one register class,
	       first try allocating in the class that is cheapest
	       for this pseudo-reg.  If that fails, try any reg.  */
	    if (N_REG_CLASSES > 1)
	      {
		find_reg (allocno_order[i], HARD_CONST (0), 0, 0, 0);
		if (reg_renumber[allocno_reg[allocno_order[i]]] >= 0)
		  continue;
	      }
	    if (reg_alternate_class (allocno_reg[allocno_order[i]]) != NO_REGS)
	      find_reg (allocno_order[i], HARD_CONST (0), 1, 0, 0);
	  }
    }

  /* Do the reloads now while the allocno data still exist, so that we can
     try to assign new hard regs to any pseudo regs that are spilled.  */

#if 0 /* We need to eliminate regs even if there is no rtl code,
	 for the sake of debugging information.  */
  if (n_basic_blocks > 0)
#endif
    return reload (get_insns (), 1, file);
}

/* Sort predicate for ordering the allocnos.
   Returns -1 (1) if *v1 should be allocated before (after) *v2.  */

static int
allocno_compare (v1, v2)
     int *v1, *v2;
{
  /* Note that the quotient will never be bigger than
     the value of floor_log2 times the maximum number of
     times a register can occur in one insn (surely less than 100).
     Multiplying this by 10000 can't overflow.  */
  register int pri1
    = (((double) (floor_log2 (allocno_n_refs[*v1]) * allocno_n_refs[*v1])
	/ (allocno_live_length[*v1] * allocno_size[*v1]))
       * 10000);
  register int pri2
    = (((double) (floor_log2 (allocno_n_refs[*v2]) * allocno_n_refs[*v2])
	/ (allocno_live_length[*v2] * allocno_size[*v2]))
       * 10000);
  if (pri2 - pri1)
    return pri2 - pri1;

  /* If regs are equally good, sort by allocno,
     so that the results of qsort leave nothing to chance.  */
  return *v1 - *v2;
}

/* Scan the rtl code and record all conflicts and register preferences in the
   conflict matrices and preference tables.  */

static void
global_conflicts ()
{
  register int b, i;
  register rtx insn;
  short *block_start_allocnos;

  /* Make a vector that mark_reg_{store,clobber} will store in.  */
  regs_set = (rtx *) alloca (max_parallel * sizeof (rtx) * 2);

  block_start_allocnos = (short *) alloca (max_allocno * sizeof (short));

  for (b = 0; b < n_basic_blocks; b++)
    {
      bzero (allocnos_live, allocno_row_words * sizeof (INT_TYPE));

      /* Initialize table of registers currently live
	 to the state at the beginning of this basic block.
	 This also marks the conflicts among them.

	 For pseudo-regs, there is only one bit for each one
	 no matter how many hard regs it occupies.
	 This is ok; we know the size from PSEUDO_REGNO_SIZE.
	 For explicit hard regs, we cannot know the size that way
	 since one hard reg can be used with various sizes.
	 Therefore, we must require that all the hard regs
	 implicitly live as part of a multi-word hard reg
	 are explicitly marked in basic_block_live_at_start.  */

      {
	register int offset;
	REGSET_ELT_TYPE bit;
	register regset old = basic_block_live_at_start[b];
	int ax = 0;

#ifdef HARD_REG_SET
	hard_regs_live = old[0];
#else
	COPY_HARD_REG_SET (hard_regs_live, old);
#endif
	for (offset = 0, i = 0; offset < regset_size; offset++)
	  if (old[offset] == 0)
	    i += REGSET_ELT_BITS;
	  else
	    for (bit = 1; bit; bit <<= 1, i++)
	      {
		if (i >= max_regno)
		  break;
		if (old[offset] & bit)
		  {
		    register int a = reg_allocno[i];
		    if (a >= 0)
		      {
			SET_ALLOCNO_LIVE (a);
			block_start_allocnos[ax++] = a;
		      }
		    else if ((a = reg_renumber[i]) >= 0)
		      mark_reg_live_nc (a, PSEUDO_REGNO_MODE (i));
		  }
	      }

	/* Record that each allocno now live conflicts with each other
	   allocno now live, and with each hard reg now live.  */

	record_conflicts (block_start_allocnos, ax);
      }

      insn = basic_block_head[b];

      /* Scan the code of this basic block, noting which allocnos
	 and hard regs are born or die.  When one is born,
	 record a conflict with all others currently live.  */

      while (1)
	{
	  register RTX_CODE code = GET_CODE (insn);
	  register rtx link;

	  /* Make regs_set an empty set.  */

	  n_regs_set = 0;

	  if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	    {
	      int i = 0;

#if 0
	      for (link = REG_NOTES (insn);
		   link && i < NUM_NO_CONFLICT_PAIRS;
		   link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_NO_CONFLICT)
		  {
		    no_conflict_pairs[i].allocno1
		      = reg_allocno[REGNO (SET_DEST (PATTERN (insn)))];
		    no_conflict_pairs[i].allocno2
		      = reg_allocno[REGNO (XEXP (link, 0))];
		    i++;
		  }
#endif /* 0 */

	      /* Mark any registers clobbered by INSN as live,
		 so they conflict with the inputs.  */

	      note_stores (PATTERN (insn), mark_reg_clobber);

	      /* Mark any registers dead after INSN as dead now.  */

	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD)
		  mark_reg_death (XEXP (link, 0));

	      /* Mark any registers set in INSN as live,
		 and mark them as conflicting with all other live regs.
		 Clobbers are processed again, so they conflict with
		 the registers that are set.  */

	      note_stores (PATTERN (insn), mark_reg_store);

#ifdef AUTO_INC_DEC
	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_INC)
		  mark_reg_store (XEXP (link, 0), NULL_RTX);
#endif

	      /* If INSN has multiple outputs, then any reg that dies here
		 and is used inside of an output
		 must conflict with the other outputs.  */

	      if (GET_CODE (PATTERN (insn)) == PARALLEL && !single_set (insn))
		for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		  if (REG_NOTE_KIND (link) == REG_DEAD)
		    {
		      int used_in_output = 0;
		      int i;
		      rtx reg = XEXP (link, 0);

		      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
			{
			  rtx set = XVECEXP (PATTERN (insn), 0, i);
			  if (GET_CODE (set) == SET
			      && GET_CODE (SET_DEST (set)) != REG
			      && !rtx_equal_p (reg, SET_DEST (set))
			      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
			    used_in_output = 1;
			}
		      if (used_in_output)
			mark_reg_conflicts (reg);
		    }

	      /* Mark any registers set in INSN and then never used.  */

	      while (n_regs_set > 0)
		if (find_regno_note (insn, REG_UNUSED,
				     REGNO (regs_set[--n_regs_set])))
		  mark_reg_death (regs_set[n_regs_set]);
	    }

	  if (insn == basic_block_end[b])
	    break;
	  insn = NEXT_INSN (insn);
	}
    }
}
/* Expand the preference information by looking for cases where one allocno
   dies in an insn that sets an allocno.  If those two allocnos don't conflict,
   merge any preferences between those allocnos.  */

static void
expand_preferences ()
{
  rtx insn;
  rtx link;
  rtx set;

  /* We only try to handle the most common cases here.  Most of the cases
     where this wins are reg-reg copies.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i'
	&& (set = single_set (insn)) != 0
	&& GET_CODE (SET_DEST (set)) == REG
	&& reg_allocno[REGNO (SET_DEST (set))] >= 0)
      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	if (REG_NOTE_KIND (link) == REG_DEAD
	    && GET_CODE (XEXP (link, 0)) == REG
	    && reg_allocno[REGNO (XEXP (link, 0))] >= 0
	    && ! CONFLICTP (reg_allocno[REGNO (SET_DEST (set))],
			    reg_allocno[REGNO (XEXP (link, 0))])
	    && ! CONFLICTP (reg_allocno[REGNO (XEXP (link, 0))],
			    reg_allocno[REGNO (SET_DEST (set))]))
	  {
	    int a1 = reg_allocno[REGNO (SET_DEST (set))];
	    int a2 = reg_allocno[REGNO (XEXP (link, 0))];

	    if (XEXP (link, 0) == SET_SRC (set))
	      {
		IOR_HARD_REG_SET (hard_reg_copy_preferences[a1],
				  hard_reg_copy_preferences[a2]);
		IOR_HARD_REG_SET (hard_reg_copy_preferences[a2],
				  hard_reg_copy_preferences[a1]);
	      }

	    IOR_HARD_REG_SET (hard_reg_preferences[a1],
			      hard_reg_preferences[a2]);
	    IOR_HARD_REG_SET (hard_reg_preferences[a2],
			      hard_reg_preferences[a1]);
	    IOR_HARD_REG_SET (hard_reg_full_preferences[a1],
			      hard_reg_full_preferences[a2]);
	    IOR_HARD_REG_SET (hard_reg_full_preferences[a2],
			      hard_reg_full_preferences[a1]);
	  }
}

/* Prune the preferences for global registers to exclude registers that cannot
   be used.
   
   Compute `regs_someone_prefers', which is a bitmask of the hard registers
   that are preferred by conflicting registers of lower priority.  If possible,
   we will avoid using these registers.  */
   
static void
prune_preferences ()
{
  int i, j;
  int allocno;
  
  /* Scan least most important to most important.
     For each allocno, remove from preferences registers that cannot be used,
     either because of conflicts or register type.  Then compute all registers
     preferred by each lower-priority register that conflicts.  */

  for (i = max_allocno - 1; i >= 0; i--)
    {
      HARD_REG_SET temp;

      allocno = allocno_order[i];
      COPY_HARD_REG_SET (temp, hard_reg_conflicts[allocno]);

      if (allocno_calls_crossed[allocno] == 0)
	IOR_HARD_REG_SET (temp, fixed_reg_set);
      else
	IOR_HARD_REG_SET (temp,	call_used_reg_set);

      IOR_COMPL_HARD_REG_SET
	(temp,
	 reg_class_contents[(int) reg_preferred_class (allocno_reg[allocno])]);

      AND_COMPL_HARD_REG_SET (hard_reg_preferences[allocno], temp);
      AND_COMPL_HARD_REG_SET (hard_reg_copy_preferences[allocno], temp);
      AND_COMPL_HARD_REG_SET (hard_reg_full_preferences[allocno], temp);

      CLEAR_HARD_REG_SET (regs_someone_prefers[allocno]);

      /* Merge in the preferences of lower-priority registers (they have
	 already been pruned).  If we also prefer some of those registers,
	 don't exclude them unless we are of a smaller size (in which case
	 we want to give the lower-priority allocno the first chance for
	 these registers).  */
      for (j = i + 1; j < max_allocno; j++)
	if (CONFLICTP (allocno, allocno_order[j]))
	  {
	    COPY_HARD_REG_SET (temp,
			       hard_reg_full_preferences[allocno_order[j]]);
	    if (allocno_size[allocno_order[j]] <= allocno_size[allocno])
	      AND_COMPL_HARD_REG_SET (temp,
				      hard_reg_full_preferences[allocno]);
			       
	    IOR_HARD_REG_SET (regs_someone_prefers[allocno], temp);
	  }
    }
}

/* Assign a hard register to ALLOCNO; look for one that is the beginning
   of a long enough stretch of hard regs none of which conflicts with ALLOCNO.
   The registers marked in PREFREGS are tried first.

   LOSERS, if non-zero, is a HARD_REG_SET indicating registers that cannot
   be used for this allocation.

   If ALT_REGS_P is zero, consider only the preferred class of ALLOCNO's reg.
   Otherwise ignore that preferred class and use the alternate class.

   If ACCEPT_CALL_CLOBBERED is nonzero, accept a call-clobbered hard reg that
   will have to be saved and restored at calls.

   RETRYING is nonzero if this is called from retry_global_alloc.

   If we find one, record it in reg_renumber.
   If not, do nothing.  */

static void
find_reg (allocno, losers, alt_regs_p, accept_call_clobbered, retrying)
     int allocno;
     HARD_REG_SET losers;
     int alt_regs_p;
     int accept_call_clobbered;
     int retrying;
{
  register int i, best_reg, pass;
#ifdef HARD_REG_SET
  register		/* Declare it register if it's a scalar.  */
#endif
    HARD_REG_SET used, used1, used2;

  enum reg_class class = (alt_regs_p
			  ? reg_alternate_class (allocno_reg[allocno])
			  : reg_preferred_class (allocno_reg[allocno]));
  enum machine_mode mode = PSEUDO_REGNO_MODE (allocno_reg[allocno]);

  if (accept_call_clobbered)
    COPY_HARD_REG_SET (used1, call_fixed_reg_set);
  else if (allocno_calls_crossed[allocno] == 0)
    COPY_HARD_REG_SET (used1, fixed_reg_set);
  else
    COPY_HARD_REG_SET (used1, call_used_reg_set);

  /* Some registers should not be allocated in global-alloc.  */
  IOR_HARD_REG_SET (used1, no_global_alloc_regs);
  if (losers)
    IOR_HARD_REG_SET (used1, losers);

  IOR_COMPL_HARD_REG_SET (used1, reg_class_contents[(int) class]);
  COPY_HARD_REG_SET (used2, used1);

  IOR_HARD_REG_SET (used1, hard_reg_conflicts[allocno]);

  /* Try each hard reg to see if it fits.  Do this in two passes.
     In the first pass, skip registers that are preferred by some other pseudo
     to give it a better chance of getting one of those registers.  Only if
     we can't get a register when excluding those do we take one of them.
     However, we never allocate a register for the first time in pass 0.  */

  COPY_HARD_REG_SET (used, used1);
  IOR_COMPL_HARD_REG_SET (used, regs_used_so_far);
  IOR_HARD_REG_SET (used, regs_someone_prefers[allocno]);
  
  best_reg = -1;
  for (i = FIRST_PSEUDO_REGISTER, pass = 0;
       pass <= 1 && i >= FIRST_PSEUDO_REGISTER;
       pass++)
    {
      if (pass == 1)
	COPY_HARD_REG_SET (used, used1);
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
#ifdef REG_ALLOC_ORDER
	  int regno = reg_alloc_order[i];
#else
	  int regno = i;
#endif
	  if (! TEST_HARD_REG_BIT (used, regno)
	      && HARD_REGNO_MODE_OK (regno, mode))
	    {
	      register int j;
	      register int lim = regno + HARD_REGNO_NREGS (regno, mode);
	      for (j = regno + 1;
		   (j < lim
		    && ! TEST_HARD_REG_BIT (used, j));
		   j++);
	      if (j == lim)
		{
		  best_reg = regno;
		  break;
		}
#ifndef REG_ALLOC_ORDER
	      i = j;			/* Skip starting points we know will lose */
#endif
	    }
	  }
      }

  /* See if there is a preferred register with the same class as the register
     we allocated above.  Making this restriction prevents register
     preferencing from creating worse register allocation.

     Remove from the preferred registers and conflicting registers.  Note that
     additional conflicts may have been added after `prune_preferences' was
     called. 

     First do this for those register with copy preferences, then all
     preferred registers.  */

  AND_COMPL_HARD_REG_SET (hard_reg_copy_preferences[allocno], used);
  GO_IF_HARD_REG_SUBSET (hard_reg_copy_preferences[allocno],
			 reg_class_contents[(int) NO_REGS], no_copy_prefs);

  if (best_reg >= 0)
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (hard_reg_copy_preferences[allocno], i)
	    && HARD_REGNO_MODE_OK (i, mode)
	    && (REGNO_REG_CLASS (i) == REGNO_REG_CLASS (best_reg)
		|| reg_class_subset_p (REGNO_REG_CLASS (i),
				       REGNO_REG_CLASS (best_reg))
		|| reg_class_subset_p (REGNO_REG_CLASS (best_reg),
				       REGNO_REG_CLASS (i))))
	    {
	      register int j;
	      register int lim = i + HARD_REGNO_NREGS (i, mode);
	      for (j = i + 1;
		   (j < lim
		    && ! TEST_HARD_REG_BIT (used, j)
		    && (REGNO_REG_CLASS (j)
		    	== REGNO_REG_CLASS (best_reg + (j - i))
			|| reg_class_subset_p (REGNO_REG_CLASS (j),
					       REGNO_REG_CLASS (best_reg + (j - i)))
			|| reg_class_subset_p (REGNO_REG_CLASS (best_reg + (j - i)),
					       REGNO_REG_CLASS (j))));
		   j++);
	      if (j == lim)
		{
		  best_reg = i;
		  goto no_prefs;
		}
	    }
    }
 no_copy_prefs:

  AND_COMPL_HARD_REG_SET (hard_reg_preferences[allocno], used);
  GO_IF_HARD_REG_SUBSET (hard_reg_preferences[allocno],
			 reg_class_contents[(int) NO_REGS], no_prefs);

  if (best_reg >= 0)
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (hard_reg_preferences[allocno], i)
	    && HARD_REGNO_MODE_OK (i, mode)
	    && (REGNO_REG_CLASS (i) == REGNO_REG_CLASS (best_reg)
		|| reg_class_subset_p (REGNO_REG_CLASS (i),
				       REGNO_REG_CLASS (best_reg))
		|| reg_class_subset_p (REGNO_REG_CLASS (best_reg),
				       REGNO_REG_CLASS (i))))
	    {
	      register int j;
	      register int lim = i + HARD_REGNO_NREGS (i, mode);
	      for (j = i + 1;
		   (j < lim
		    && ! TEST_HARD_REG_BIT (used, j)
		    && (REGNO_REG_CLASS (j)
		    	== REGNO_REG_CLASS (best_reg + (j - i))
			|| reg_class_subset_p (REGNO_REG_CLASS (j),
					       REGNO_REG_CLASS (best_reg + (j - i)))
			|| reg_class_subset_p (REGNO_REG_CLASS (best_reg + (j - i)),
					       REGNO_REG_CLASS (j))));
		   j++);
	      if (j == lim)
		{
		  best_reg = i;
		  break;
		}
	    }
    }
 no_prefs:

  /* If we haven't succeeded yet, try with caller-saves.  */
  if (flag_caller_saves && best_reg < 0)
    {
      /* Did not find a register.  If it would be profitable to
	 allocate a call-clobbered register and save and restore it
	 around calls, do that.  */
      if (! accept_call_clobbered
	  && allocno_calls_crossed[allocno] != 0
	  && CALLER_SAVE_PROFITABLE (allocno_n_refs[allocno],
				     allocno_calls_crossed[allocno]))
	{
	  find_reg (allocno, losers, alt_regs_p, 1, retrying);
	  if (reg_renumber[allocno_reg[allocno]] >= 0)
	    {
	      caller_save_needed = 1;
	      return;
	    }
	}
    }

  /* If we haven't succeeded yet,
     see if some hard reg that conflicts with us
     was utilized poorly by local-alloc.
     If so, kick out the regs that were put there by local-alloc
     so we can use it instead.  */
  if (best_reg < 0 && !retrying
      /* Let's not bother with multi-reg allocnos.  */
      && allocno_size[allocno] == 1)
    {
      /* Count from the end, to find the least-used ones first.  */
      for (i = FIRST_PSEUDO_REGISTER - 1; i >= 0; i--)
	if (local_reg_n_refs[i] != 0
	    /* Don't use a reg no good for this pseudo.  */
	    && ! TEST_HARD_REG_BIT (used2, i)
	    && HARD_REGNO_MODE_OK (i, mode)
	    && ((double) local_reg_n_refs[i] / local_reg_live_length[i]
		< ((double) allocno_n_refs[allocno]
		   / allocno_live_length[allocno])))
	  {
	    /* Hard reg I was used less in total by local regs
	       than it would be used by this one allocno!  */
	    int k;
	    for (k = 0; k < max_regno; k++)
	      if (reg_renumber[k] >= 0)
		{
		  int regno = reg_renumber[k];
		  int endregno
		    = regno + HARD_REGNO_NREGS (regno, PSEUDO_REGNO_MODE (k));

		  if (i >= regno && i < endregno)
		    reg_renumber[k] = -1;
		}

	    best_reg = i;
	    break;
	  }
    }

  /* Did we find a register?  */

  if (best_reg >= 0)
    {
      register int lim, j;
      HARD_REG_SET this_reg;

      /* Yes.  Record it as the hard register of this pseudo-reg.  */
      reg_renumber[allocno_reg[allocno]] = best_reg;
      /* Also of any pseudo-regs that share with it.  */
      if (reg_may_share[allocno_reg[allocno]])
	for (j = FIRST_PSEUDO_REGISTER; j < max_regno; j++)
	  if (reg_allocno[j] == allocno)
	    reg_renumber[j] = best_reg;

      /* Make a set of the hard regs being allocated.  */
      CLEAR_HARD_REG_SET (this_reg);
      lim = best_reg + HARD_REGNO_NREGS (best_reg, mode);
      for (j = best_reg; j < lim; j++)
	{
	  SET_HARD_REG_BIT (this_reg, j);
	  SET_HARD_REG_BIT (regs_used_so_far, j);
	  /* This is no longer a reg used just by local regs.  */
	  local_reg_n_refs[j] = 0;
	}
      /* For each other pseudo-reg conflicting with this one,
	 mark it as conflicting with the hard regs this one occupies.  */
      lim = allocno;
      for (j = 0; j < max_allocno; j++)
	if (CONFLICTP (lim, j) || CONFLICTP (j, lim))
	  {
	    IOR_HARD_REG_SET (hard_reg_conflicts[j], this_reg);
	  }
    }
}

/* Called from `reload' to look for a hard reg to put pseudo reg REGNO in.
   Perhaps it had previously seemed not worth a hard reg,
   or perhaps its old hard reg has been commandeered for reloads.
   FORBIDDEN_REGS indicates certain hard regs that may not be used, even if
   they do not appear to be allocated.
   If FORBIDDEN_REGS is zero, no regs are forbidden.  */

void
retry_global_alloc (regno, forbidden_regs)
     int regno;
     HARD_REG_SET forbidden_regs;
{
  int allocno = reg_allocno[regno];
  if (allocno >= 0)
    {
      /* If we have more than one register class,
	 first try allocating in the class that is cheapest
	 for this pseudo-reg.  If that fails, try any reg.  */
      if (N_REG_CLASSES > 1)
	find_reg (allocno, forbidden_regs, 0, 0, 1);
      if (reg_renumber[regno] < 0
	  && reg_alternate_class (regno) != NO_REGS)
	find_reg (allocno, forbidden_regs, 1, 0, 1);

      /* If we found a register, modify the RTL for the register to
	 show the hard register, and mark that register live.  */
      if (reg_renumber[regno] >= 0)
	{
	  REGNO (regno_reg_rtx[regno]) = reg_renumber[regno];
	  mark_home_live (regno);
	}
    }
}

/* Record a conflict between register REGNO
   and everything currently live.
   REGNO must not be a pseudo reg that was allocated
   by local_alloc; such numbers must be translated through
   reg_renumber before calling here.  */

static void
record_one_conflict (regno)
     int regno;
{
  register int j;

  if (regno < FIRST_PSEUDO_REGISTER)
    /* When a hard register becomes live,
       record conflicts with live pseudo regs.  */
    for (j = 0; j < max_allocno; j++)
      {
	if (ALLOCNO_LIVE_P (j))
	  SET_HARD_REG_BIT (hard_reg_conflicts[j], regno);
      }
  else
    /* When a pseudo-register becomes live,
       record conflicts first with hard regs,
       then with other pseudo regs.  */
    {
      register int ialloc = reg_allocno[regno];
      register int ialloc_prod = ialloc * allocno_row_words;
      IOR_HARD_REG_SET (hard_reg_conflicts[ialloc], hard_regs_live);
      for (j = allocno_row_words - 1; j >= 0; j--)
	{
#if 0
	  int k;
	  for (k = 0; k < n_no_conflict_pairs; k++)
	    if (! ((j == no_conflict_pairs[k].allocno1
		    && ialloc == no_conflict_pairs[k].allocno2)
		   ||
		   (j == no_conflict_pairs[k].allocno2
		    && ialloc == no_conflict_pairs[k].allocno1)))
#endif /* 0 */
	      conflicts[ialloc_prod + j] |= allocnos_live[j];
	}
    }
}

/* Record all allocnos currently live as conflicting
   with each other and with all hard regs currently live.
   ALLOCNO_VEC is a vector of LEN allocnos, all allocnos that
   are currently live.  Their bits are also flagged in allocnos_live.  */

static void
record_conflicts (allocno_vec, len)
     register short *allocno_vec;
     register int len;
{
  register int allocno;
  register int j;
  register int ialloc_prod;

  while (--len >= 0)
    {
      allocno = allocno_vec[len];
      ialloc_prod = allocno * allocno_row_words;
      IOR_HARD_REG_SET (hard_reg_conflicts[allocno], hard_regs_live);
      for (j = allocno_row_words - 1; j >= 0; j--)
	conflicts[ialloc_prod + j] |= allocnos_live[j];
    }
}

/* Handle the case where REG is set by the insn being scanned,
   during the forward scan to accumulate conflicts.
   Store a 1 in regs_live or allocnos_live for this register, record how many
   consecutive hardware registers it actually needs,
   and record a conflict with all other registers already live.

   Note that even if REG does not remain alive after this insn,
   we must mark it here as live, to ensure a conflict between
   REG and any other regs set in this insn that really do live.
   This is because those other regs could be considered after this.

   REG might actually be something other than a register;
   if so, we do nothing.

   SETTER is 0 if this register was modified by an auto-increment (i.e.,
   a REG_INC note was found for it).

   CLOBBERs are processed here by calling mark_reg_clobber.  */ 

static void
mark_reg_store (orig_reg, setter)
     rtx orig_reg, setter;
{
  register int regno;
  register rtx reg = orig_reg;

  /* WORD is which word of a multi-register group is being stored.
     For the case where the store is actually into a SUBREG of REG.
     Except we don't use it; I believe the entire REG needs to be
     made live.  */
  int word = 0;

  if (GET_CODE (reg) == SUBREG)
    {
      word = SUBREG_WORD (reg);
      reg = SUBREG_REG (reg);
    }

  if (GET_CODE (reg) != REG)
    return;

  if (setter && GET_CODE (setter) == CLOBBER)
    {
      /* A clobber of a register should be processed here too.  */
      mark_reg_clobber (orig_reg, setter);
      return;
    }

  regs_set[n_regs_set++] = reg;

  if (setter)
    set_preference (reg, SET_SRC (setter));

  regno = REGNO (reg);

  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno] /* + word */;

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is or has a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_allocno[regno] >= 0)
	{
	  SET_ALLOCNO_LIVE (reg_allocno[regno]);
	  record_one_conflict (regno);
	}
    }
  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  else if (! fixed_regs[regno])
    {
      register int last = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (regno < last)
	{
	  record_one_conflict (regno);
	  SET_HARD_REG_BIT (hard_regs_live, regno);
	  regno++;
	}
    }
}

/* Like mark_reg_set except notice just CLOBBERs; ignore SETs.  */

static void
mark_reg_clobber (reg, setter)
     rtx reg, setter;
{
  register int regno;

  /* WORD is which word of a multi-register group is being stored.
     For the case where the store is actually into a SUBREG of REG.
     Except we don't use it; I believe the entire REG needs to be
     made live.  */
  int word = 0;

  if (GET_CODE (setter) != CLOBBER)
    return;

  if (GET_CODE (reg) == SUBREG)
    {
      word = SUBREG_WORD (reg);
      reg = SUBREG_REG (reg);
    }

  if (GET_CODE (reg) != REG)
    return;

  regs_set[n_regs_set++] = reg;

  regno = REGNO (reg);

  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno] /* + word */;

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is or has a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_allocno[regno] >= 0)
	{
	  SET_ALLOCNO_LIVE (reg_allocno[regno]);
	  record_one_conflict (regno);
	}
    }
  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  else if (! fixed_regs[regno])
    {
      register int last = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (regno < last)
	{
	  record_one_conflict (regno);
	  SET_HARD_REG_BIT (hard_regs_live, regno);
	  regno++;
	}
    }
}

/* Record that REG has conflicts with all the regs currently live.
   Do not mark REG itself as live.  */

static void
mark_reg_conflicts (reg)
     rtx reg;
{
  register int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (GET_CODE (reg) != REG)
    return;

  regno = REGNO (reg);

  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is or has a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_allocno[regno] >= 0)
	record_one_conflict (regno);
    }
  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  else if (! fixed_regs[regno])
    {
      register int last = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (regno < last)
	{
	  record_one_conflict (regno);
	  regno++;
	}
    }
}

/* Mark REG as being dead (following the insn being scanned now).
   Store a 0 in regs_live or allocnos_live for this register.  */

static void
mark_reg_death (reg)
     rtx reg;
{
  register int regno = REGNO (reg);

  /* For pseudo reg, see if it has been assigned a hardware reg.  */
  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_allocno[regno] >= 0)
	CLEAR_ALLOCNO_LIVE (reg_allocno[regno]);
    }
  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  else if (! fixed_regs[regno])
    {
      /* Pseudo regs already assigned hardware regs are treated
	 almost the same as explicit hardware regs.  */
      register int last = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (regno < last)
	{
	  CLEAR_HARD_REG_BIT (hard_regs_live, regno);
	  regno++;
	}
    }
}

/* Mark hard reg REGNO as currently live, assuming machine mode MODE
   for the value stored in it.  MODE determines how many consecutive
   registers are actually in use.  Do not record conflicts;
   it is assumed that the caller will do that.  */

static void
mark_reg_live_nc (regno, mode)
     register int regno;
     enum machine_mode mode;
{
  register int last = regno + HARD_REGNO_NREGS (regno, mode);
  while (regno < last)
    {
      SET_HARD_REG_BIT (hard_regs_live, regno);
      regno++;
    }
}

/* Try to set a preference for an allocno to a hard register.
   We are passed DEST and SRC which are the operands of a SET.  It is known
   that SRC is a register.  If SRC or the first operand of SRC is a register,
   try to set a preference.  If one of the two is a hard register and the other
   is a pseudo-register, mark the preference.
   
   Note that we are not as aggressive as local-alloc in trying to tie a
   pseudo-register to a hard register.  */

static void
set_preference (dest, src)
     rtx dest, src;
{
  int src_regno, dest_regno;
  /* Amount to add to the hard regno for SRC, or subtract from that for DEST,
     to compensate for subregs in SRC or DEST.  */
  int offset = 0;
  int i;
  int copy = 1;

  if (GET_RTX_FORMAT (GET_CODE (src))[0] == 'e')
    src = XEXP (src, 0), copy = 0;

  /* Get the reg number for both SRC and DEST.
     If neither is a reg, give up.  */

  if (GET_CODE (src) == REG)
    src_regno = REGNO (src);
  else if (GET_CODE (src) == SUBREG && GET_CODE (SUBREG_REG (src)) == REG)
    {
      src_regno = REGNO (SUBREG_REG (src));
      offset += SUBREG_WORD (src);
    }
  else
    return;

  if (GET_CODE (dest) == REG)
    dest_regno = REGNO (dest);
  else if (GET_CODE (dest) == SUBREG && GET_CODE (SUBREG_REG (dest)) == REG)
    {
      dest_regno = REGNO (SUBREG_REG (dest));
      offset -= SUBREG_WORD (dest);
    }
  else
    return;

  /* Convert either or both to hard reg numbers.  */

  if (reg_renumber[src_regno] >= 0)
    src_regno = reg_renumber[src_regno];

  if (reg_renumber[dest_regno] >= 0)
    dest_regno = reg_renumber[dest_regno];

  /* Now if one is a hard reg and the other is a global pseudo
     then give the other a preference.  */

  if (dest_regno < FIRST_PSEUDO_REGISTER && src_regno >= FIRST_PSEUDO_REGISTER
      && reg_allocno[src_regno] >= 0)
    {
      dest_regno -= offset;
      if (dest_regno >= 0 && dest_regno < FIRST_PSEUDO_REGISTER)
	{
	  if (copy)
	    SET_REGBIT (hard_reg_copy_preferences,
			reg_allocno[src_regno], dest_regno);

	  SET_REGBIT (hard_reg_preferences,
		      reg_allocno[src_regno], dest_regno);
	  for (i = dest_regno;
	       i < dest_regno + HARD_REGNO_NREGS (dest_regno, GET_MODE (dest));
	       i++)
	    SET_REGBIT (hard_reg_full_preferences, reg_allocno[src_regno], i);
	}
    }

  if (src_regno < FIRST_PSEUDO_REGISTER && dest_regno >= FIRST_PSEUDO_REGISTER
      && reg_allocno[dest_regno] >= 0)
    {
      src_regno += offset;
      if (src_regno >= 0 && src_regno < FIRST_PSEUDO_REGISTER)
	{
	  if (copy)
	    SET_REGBIT (hard_reg_copy_preferences,
			reg_allocno[dest_regno], src_regno);

	  SET_REGBIT (hard_reg_preferences,
		      reg_allocno[dest_regno], src_regno);
	  for (i = src_regno;
	       i < src_regno + HARD_REGNO_NREGS (src_regno, GET_MODE (src));
	       i++)
	    SET_REGBIT (hard_reg_full_preferences, reg_allocno[dest_regno], i);
	}
    }
}

/* Indicate that hard register number FROM was eliminated and replaced with
   an offset from hard register number TO.  The status of hard registers live
   at the start of a basic block is updated by replacing a use of FROM with
   a use of TO.  */

void
mark_elimination (from, to)
     int from, to;
{
  int i;

  for (i = 0; i < n_basic_blocks; i++)
    if ((basic_block_live_at_start[i][from / REGSET_ELT_BITS]
	 & ((REGSET_ELT_TYPE) 1 << (from % REGSET_ELT_BITS))) != 0)
      {
	basic_block_live_at_start[i][from / REGSET_ELT_BITS]
	  &= ~ ((REGSET_ELT_TYPE) 1 << (from % REGSET_ELT_BITS));
	basic_block_live_at_start[i][to / REGSET_ELT_BITS]
	  |= ((REGSET_ELT_TYPE) 1 << (to % REGSET_ELT_BITS));
      }
}

/* Print debugging trace information if -greg switch is given,
   showing the information on which the allocation decisions are based.  */

static void
dump_conflicts (file)
     FILE *file;
{
  register int i;
  register int has_preferences;
  fprintf (file, ";; %d regs to allocate:", max_allocno);
  for (i = 0; i < max_allocno; i++)
    {
      int j;
      fprintf (file, " %d", allocno_reg[allocno_order[i]]);
      for (j = 0; j < max_regno; j++)
	if (reg_allocno[j] == allocno_order[i]
	    && j != allocno_reg[allocno_order[i]])
	  fprintf (file, "+%d", j);
      if (allocno_size[allocno_order[i]] != 1)
	fprintf (file, " (%d)", allocno_size[allocno_order[i]]);
    }
  fprintf (file, "\n");

  for (i = 0; i < max_allocno; i++)
    {
      register int j;
      fprintf (file, ";; %d conflicts:", allocno_reg[i]);
      for (j = 0; j < max_allocno; j++)
	if (CONFLICTP (i, j) || CONFLICTP (j, i))
	  fprintf (file, " %d", allocno_reg[j]);
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (hard_reg_conflicts[i], j))
	  fprintf (file, " %d", j);
      fprintf (file, "\n");

      has_preferences = 0;
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (hard_reg_preferences[i], j))
	  has_preferences = 1;

      if (! has_preferences)
	continue;
      fprintf (file, ";; %d preferences:", allocno_reg[i]);
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (hard_reg_preferences[i], j))
	  fprintf (file, " %d", j);
      fprintf (file, "\n");
    }
  fprintf (file, "\n");
}

void
dump_global_regs (file)
     FILE *file;
{
  register int i, j;
  
  fprintf (file, ";; Register dispositions:\n");
  for (i = FIRST_PSEUDO_REGISTER, j = 0; i < max_regno; i++)
    if (reg_renumber[i] >= 0)
      {
	fprintf (file, "%d in %d  ", i, reg_renumber[i]);
        if (++j % 6 == 0)
	  fprintf (file, "\n");
      }

  fprintf (file, "\n\n;; Hard regs used: ");
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i])
      fprintf (file, " %d", i);
  fprintf (file, "\n\n");
}
