/* Allocate registers for pseudo-registers that span basic blocks.
   Copyright (C) 1987, 1988, 1991, 1994, 1996, 1997, 1998,
   1999, 2000, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.

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
#include "machmode.h"
#include "hard-reg-set.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "regs.h"
#include "function.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "output.h"
#include "toplev.h"
#include "tree-pass.h"
#include "timevar.h"
#include "df.h"
#include "vecprim.h"
#include "dbgcnt.h"

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

   1. Assign allocation-numbers (allocnos) to the pseudo-registers
   still needing allocations and to the pseudo-registers currently
   allocated by local-alloc which may be spilled by reload.
   Set up tables reg_allocno and allocno_reg to map
   reg numbers to allocnos and vice versa.
   max_allocno gets the number of allocnos in use.

   2. Allocate a max_allocno by max_allocno conflict bit matrix and clear it.
   Allocate a max_allocno by FIRST_PSEUDO_REGISTER conflict matrix
   for conflicts between allocnos and explicit hard register use
   (which includes use of pseudo-registers allocated by local_alloc).

   3. For each basic block
    walk forward through the block, recording which
    pseudo-registers and which hardware registers are live.
    Build the conflict matrix between the pseudo-registers
    and another of pseudo-registers versus hardware registers.
    Also record the preferred hardware registers
    for each pseudo-register.

   4. Sort a table of the allocnos into order of
   desirability of the variables.

   5. Allocate the variables in that order; each if possible into
   a preferred register, else into another register.  */

/* Number of pseudo-registers which are candidates for allocation.  */

static int max_allocno;

/* Indexed by (pseudo) reg number, gives the allocno, or -1
   for pseudo registers which are not to be allocated.  */

static int *reg_allocno;

struct allocno
{
  int reg;
  /* Gives the number of consecutive hard registers needed by that
     pseudo reg.  */
  int size;

  /* Number of calls crossed by each allocno.  */
  int calls_crossed;

  /* Number of calls that might throw crossed by each allocno.  */
  int throwing_calls_crossed;

  /* Number of refs to each allocno.  */
  int n_refs;

  /* Frequency of uses of each allocno.  */
  int freq;

  /* Guess at live length of each allocno.
     This is actually the max of the live lengths of the regs.  */
  int live_length;

  /* Set of hard regs conflicting with allocno N.  */

  HARD_REG_SET hard_reg_conflicts;

  /* Set of hard regs preferred by allocno N.
     This is used to make allocnos go into regs that are copied to or from them,
     when possible, to reduce register shuffling.  */

  HARD_REG_SET hard_reg_preferences;

  /* Similar, but just counts register preferences made in simple copy
     operations, rather than arithmetic.  These are given priority because
     we can always eliminate an insn by using these, but using a register
     in the above list won't always eliminate an insn.  */

  HARD_REG_SET hard_reg_copy_preferences;

  /* Similar to hard_reg_preferences, but includes bits for subsequent
     registers when an allocno is multi-word.  The above variable is used for
     allocation while this is used to build reg_someone_prefers, below.  */

  HARD_REG_SET hard_reg_full_preferences;

  /* Set of hard registers that some later allocno has a preference for.  */

  HARD_REG_SET regs_someone_prefers;

#ifdef STACK_REGS
  /* Set to true if allocno can't be allocated in the stack register.  */
  bool no_stack_reg;
#endif
};

static struct allocno *allocno;

/* A vector of the integers from 0 to max_allocno-1,
   sorted in the order of first-to-be-allocated first.  */

static int *allocno_order;

/* Define the number of bits in each element of `conflicts' and what
   type that element has.  We use the largest integer format on the
   host machine.  */

#define INT_BITS HOST_BITS_PER_WIDE_INT
#define INT_TYPE HOST_WIDE_INT

/* max_allocno by max_allocno array of bits,
   recording whether two allocno's conflict (can't go in the same
   hardware register).

   `conflicts' is symmetric after the call to mirror_conflicts.  */

static INT_TYPE *conflicts;

/* Number of ints required to hold max_allocno bits.
   This is the length of a row in `conflicts'.  */

static int allocno_row_words;

/* Two macros to test or store 1 in an element of `conflicts'.  */

#define CONFLICTP(I, J) \
 (conflicts[(I) * allocno_row_words + (unsigned) (J) / INT_BITS]	\
  & ((INT_TYPE) 1 << ((unsigned) (J) % INT_BITS)))

/* For any allocno set in ALLOCNO_SET, set ALLOCNO to that allocno,
   and execute CODE.  */
#define EXECUTE_IF_SET_IN_ALLOCNO_SET(ALLOCNO_SET, ALLOCNO, CODE)	\
do {									\
  int i_;								\
  int allocno_;								\
  INT_TYPE *p_ = (ALLOCNO_SET);						\
									\
  for (i_ = allocno_row_words - 1, allocno_ = 0; i_ >= 0;		\
       i_--, allocno_ += INT_BITS)					\
    {									\
      unsigned INT_TYPE word_ = (unsigned INT_TYPE) *p_++;		\
									\
      for ((ALLOCNO) = allocno_; word_; word_ >>= 1, (ALLOCNO)++)	\
	{								\
	  if (word_ & 1)						\
	    {CODE;}							\
	}								\
    }									\
} while (0)

/* Set of hard regs currently live (during scan of all insns).  */

static HARD_REG_SET hard_regs_live;

/* Set of registers that global-alloc isn't supposed to use.  */

static HARD_REG_SET no_global_alloc_regs;

/* Set of registers used so far.  */

static HARD_REG_SET regs_used_so_far;

/* Number of refs to each hard reg, as used by local alloc.
   It is zero for a reg that contains global pseudos or is explicitly used.  */

static int local_reg_n_refs[FIRST_PSEUDO_REGISTER];

/* Frequency of uses of given hard reg.  */
static int local_reg_freq[FIRST_PSEUDO_REGISTER];

/* Guess at live length of each hard reg, as used by local alloc.
   This is actually the sum of the live lengths of the specific regs.  */

static int local_reg_live_length[FIRST_PSEUDO_REGISTER];

/* Set to 1 a bit in a vector TABLE of HARD_REG_SETs, for vector
   element I, and hard register number J.  */

#define SET_REGBIT(TABLE, I, J)  SET_HARD_REG_BIT (allocno[I].TABLE, J)

/* Bit mask for allocnos live at current point in the scan.  */

static INT_TYPE *allocnos_live;

/* Test, set or clear bit number I in allocnos_live,
   a bit vector indexed by allocno.  */

#define SET_ALLOCNO_LIVE(I)				\
  (allocnos_live[(unsigned) (I) / INT_BITS]		\
     |= ((INT_TYPE) 1 << ((unsigned) (I) % INT_BITS)))

#define CLEAR_ALLOCNO_LIVE(I)				\
  (allocnos_live[(unsigned) (I) / INT_BITS]		\
     &= ~((INT_TYPE) 1 << ((unsigned) (I) % INT_BITS)))

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

static VEC(rtx, heap) *regs_set;


/* Return true if *LOC contains an asm.  */

static int
insn_contains_asm_1 (rtx *loc, void *data ATTRIBUTE_UNUSED)
{
  if ( !*loc)
    return 0;
  if (GET_CODE (*loc) == ASM_OPERANDS)
    return 1;
  return 0;
}


/* Return true if INSN contains an ASM.  */

static int
insn_contains_asm (rtx insn)
{
  return for_each_rtx (&insn, insn_contains_asm_1, NULL);
}


static void
compute_regs_asm_clobbered (char *regs_asm_clobbered)
{
  basic_block bb;

  memset (regs_asm_clobbered, 0, sizeof (char) * FIRST_PSEUDO_REGISTER);
  
  FOR_EACH_BB (bb)
    {
      rtx insn;
      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  struct df_ref **def_rec;
	  if (insn_contains_asm (insn))
	    for (def_rec = DF_INSN_DEFS (insn); *def_rec; def_rec++)
	      {
		struct df_ref *def = *def_rec;
		unsigned int dregno = DF_REF_REGNO (def);
		if (dregno < FIRST_PSEUDO_REGISTER)
		  {
		    unsigned int i;
		    enum machine_mode mode = GET_MODE (DF_REF_REAL_REG (def));
		    unsigned int end = dregno 
		      + hard_regno_nregs[dregno][mode] - 1;
		    for (i = dregno; i <= end; ++i)
		      regs_asm_clobbered[i] = 1;
		  }
	      }
	}
    }
}


/* All registers that can be eliminated.  */

static HARD_REG_SET eliminable_regset;

static int allocno_compare (const void *, const void *);
static void global_conflicts (void);
static void mirror_conflicts (void);
static void expand_preferences (void);
static void prune_preferences (void);
static void find_reg (int, HARD_REG_SET, int, int, int);
static void record_one_conflict (int);
static void record_conflicts (int *, int);
static void mark_reg_store (rtx, rtx, void *);
static void mark_reg_clobber (rtx, rtx, void *);
static void mark_reg_conflicts (rtx);
static void mark_reg_death (rtx);
static void set_preference (rtx, rtx);
static void dump_conflicts (FILE *);
static void reg_becomes_live (rtx, rtx, void *);
static void reg_dies (int, enum machine_mode, struct insn_chain *);




/* Look through the list of eliminable registers.  Set ELIM_SET to the
   set of registers which may be eliminated.  Set NO_GLOBAL_SET to the
   set of registers which may not be used across blocks.

   This will normally be called with ELIM_SET as the file static
   variable eliminable_regset, and NO_GLOBAL_SET as the file static
   variable NO_GLOBAL_ALLOC_REGS.  */

static void
compute_regsets (HARD_REG_SET *elim_set, 
                 HARD_REG_SET *no_global_set)
{

/* Like regs_ever_live, but 1 if a reg is set or clobbered from an asm.
   Unlike regs_ever_live, elements of this array corresponding to
   eliminable regs like the frame pointer are set if an asm sets them.  */
  char *regs_asm_clobbered = alloca (FIRST_PSEUDO_REGISTER * sizeof (char));

#ifdef ELIMINABLE_REGS
  static const struct {const int from, to; } eliminables[] = ELIMINABLE_REGS;
  size_t i;
#endif
  int need_fp
    = (! flag_omit_frame_pointer
       || (current_function_calls_alloca && EXIT_IGNORE_STACK)
       || FRAME_POINTER_REQUIRED);

  max_regno = max_reg_num ();
  compact_blocks ();

  max_allocno = 0;

  /* A machine may have certain hard registers that
     are safe to use only within a basic block.  */

  CLEAR_HARD_REG_SET (*no_global_set);
  CLEAR_HARD_REG_SET (*elim_set);

  compute_regs_asm_clobbered (regs_asm_clobbered);
  /* Build the regset of all eliminable registers and show we can't use those
     that we already know won't be eliminated.  */
#ifdef ELIMINABLE_REGS
  for (i = 0; i < ARRAY_SIZE (eliminables); i++)
    {
      bool cannot_elim
	= (! CAN_ELIMINATE (eliminables[i].from, eliminables[i].to)
	   || (eliminables[i].to == STACK_POINTER_REGNUM && need_fp));

      if (!regs_asm_clobbered[eliminables[i].from])
	{
	  SET_HARD_REG_BIT (*elim_set, eliminables[i].from);

	  if (cannot_elim)
	    SET_HARD_REG_BIT (*no_global_set, eliminables[i].from);
	}
      else if (cannot_elim)
	error ("%s cannot be used in asm here",
	       reg_names[eliminables[i].from]);
      else
	df_set_regs_ever_live (eliminables[i].from, true);
    }
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
  if (!regs_asm_clobbered[HARD_FRAME_POINTER_REGNUM])
    {
      SET_HARD_REG_BIT (*elim_set, HARD_FRAME_POINTER_REGNUM);
      if (need_fp)
	SET_HARD_REG_BIT (*no_global_set, HARD_FRAME_POINTER_REGNUM);
    }
  else if (need_fp)
    error ("%s cannot be used in asm here",
	   reg_names[HARD_FRAME_POINTER_REGNUM]);
  else
    df_set_regs_ever_live (HARD_FRAME_POINTER_REGNUM, true);
#endif

#else
  if (!regs_asm_clobbered[FRAME_POINTER_REGNUM])
    {
      SET_HARD_REG_BIT (*elim_set, FRAME_POINTER_REGNUM);
      if (need_fp)
	SET_HARD_REG_BIT (*no_global_set, FRAME_POINTER_REGNUM);
    }
  else if (need_fp)
    error ("%s cannot be used in asm here", reg_names[FRAME_POINTER_REGNUM]);
  else
    df_set_regs_ever_live (FRAME_POINTER_REGNUM, true);
#endif
}

/* Perform allocation of pseudo-registers not allocated by local_alloc.

   Return value is nonzero if reload failed
   and we must not do any more for this function.  */

static int
global_alloc (void)
{
  int retval;
  size_t i;

  compute_regsets (&eliminable_regset, &no_global_alloc_regs);

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
    const char *cheap_regs;
    const char *const leaf_regs = LEAF_REGISTERS;

    if (only_leaf_regs_used () && leaf_function_p ())
      cheap_regs = leaf_regs;
    else
      cheap_regs = call_used_regs;
    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
      if (df_regs_ever_live_p (i) || cheap_regs[i])
	SET_HARD_REG_BIT (regs_used_so_far, i);
  }
#else
  /* We consider registers that do not have to be saved over calls as if
     they were already used since there is no cost in using them.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (df_regs_ever_live_p (i) || call_used_regs[i])
      SET_HARD_REG_BIT (regs_used_so_far, i);
#endif

  for (i = FIRST_PSEUDO_REGISTER; i < (size_t) max_regno; i++)
    if (reg_renumber[i] >= 0)
      SET_HARD_REG_BIT (regs_used_so_far, reg_renumber[i]);

  /* Establish mappings from register number to allocation number
     and vice versa.  In the process, count the allocnos.  */

  reg_allocno = XNEWVEC (int, max_regno);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    reg_allocno[i] = -1;

  max_allocno = 0;
  for (i = FIRST_PSEUDO_REGISTER; i < (size_t) max_regno; i++)
    /* Note that reg_live_length[i] < 0 indicates a "constant" reg
       that we are supposed to refrain from putting in a hard reg.
       -2 means do make an allocno but don't allocate it.  */
    if (REG_N_REFS (i) != 0 && REG_LIVE_LENGTH (i) != -1
	/* Don't allocate pseudos that cross calls,
	   if this function receives a nonlocal goto.  */
	&& (! current_function_has_nonlocal_label
	    || REG_N_CALLS_CROSSED (i) == 0))
      {
	reg_allocno[i] = max_allocno++;
	gcc_assert (REG_LIVE_LENGTH (i));
      }
    else
      reg_allocno[i] = -1;

  allocno = XCNEWVEC (struct allocno, max_allocno);

  for (i = FIRST_PSEUDO_REGISTER; i < (size_t) max_regno; i++)
    if (reg_allocno[i] >= 0)
      {
	int num = reg_allocno[i];
	allocno[num].reg = i;
	allocno[num].size = PSEUDO_REGNO_SIZE (i);
	allocno[num].calls_crossed += REG_N_CALLS_CROSSED (i);
	allocno[num].throwing_calls_crossed
	  += REG_N_THROWING_CALLS_CROSSED (i);
	allocno[num].n_refs += REG_N_REFS (i);
	allocno[num].freq += REG_FREQ (i);
	if (allocno[num].live_length < REG_LIVE_LENGTH (i))
	  allocno[num].live_length = REG_LIVE_LENGTH (i);
      }

  /* Calculate amount of usage of each hard reg by pseudos
     allocated by local-alloc.  This is to see if we want to
     override it.  */
  memset (local_reg_live_length, 0, sizeof local_reg_live_length);
  memset (local_reg_n_refs, 0, sizeof local_reg_n_refs);
  memset (local_reg_freq, 0, sizeof local_reg_freq);
  for (i = FIRST_PSEUDO_REGISTER; i < (size_t) max_regno; i++)
    if (reg_renumber[i] >= 0)
      {
	int regno = reg_renumber[i];
	int endregno = end_hard_regno (PSEUDO_REGNO_MODE (i), regno);
	int j;

	for (j = regno; j < endregno; j++)
	  {
	    local_reg_n_refs[j] += REG_N_REFS (i);
	    local_reg_freq[j] += REG_FREQ (i);
	    local_reg_live_length[j] += REG_LIVE_LENGTH (i);
	  }
      }

  /* We can't override local-alloc for a reg used not just by local-alloc.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (df_regs_ever_live_p (i))
      local_reg_n_refs[i] = 0, local_reg_freq[i] = 0;

  if (dump_file)
    {
      for (i = FIRST_PSEUDO_REGISTER; i < (size_t) max_regno; i++)
	{
	  fprintf (dump_file, "%d REG_N_REFS=%d, REG_FREQ=%d, REG_LIVE_LENGTH=%d\n", 
		   (int)i, REG_N_REFS (i), REG_FREQ (i), REG_LIVE_LENGTH (i));
	}
      fprintf (dump_file, "regs_ever_live =");
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (df_regs_ever_live_p (i))
	  fprintf (dump_file, " %d", (int)i);
      fprintf (dump_file, "\n");
    }
  allocno_row_words = (max_allocno + INT_BITS - 1) / INT_BITS;

  /* We used to use alloca here, but the size of what it would try to
     allocate would occasionally cause it to exceed the stack limit and
     cause unpredictable core dumps.  Some examples were > 2Mb in size.  */
  conflicts = XCNEWVEC (INT_TYPE, max_allocno * allocno_row_words);

  allocnos_live = XNEWVEC (INT_TYPE, allocno_row_words);

  /* If there is work to be done (at least one reg to allocate),
     perform global conflict analysis and allocate the regs.  */

  if (max_allocno > 0)
    {
      /* Make a vector that mark_reg_{store,clobber} will store in.  */
      if (!regs_set)
	regs_set = VEC_alloc (rtx, heap, 10);

      /* Scan all the insns and compute the conflicts among allocnos
	 and between allocnos and hard regs.  */

      global_conflicts ();

      mirror_conflicts ();

      /* Eliminate conflicts between pseudos and eliminable registers.  If
	 the register is not eliminated, the pseudo won't really be able to
	 live in the eliminable register, so the conflict doesn't matter.
	 If we do eliminate the register, the conflict will no longer exist.
	 So in either case, we can ignore the conflict.  Likewise for
	 preferences.  */

      for (i = 0; i < (size_t) max_allocno; i++)
	{
	  AND_COMPL_HARD_REG_SET (allocno[i].hard_reg_conflicts,
				  eliminable_regset);
	  AND_COMPL_HARD_REG_SET (allocno[i].hard_reg_copy_preferences,
				  eliminable_regset);
	  AND_COMPL_HARD_REG_SET (allocno[i].hard_reg_preferences,
				  eliminable_regset);
	}

      /* Try to expand the preferences by merging them between allocnos.  */

      expand_preferences ();

      /* Determine the order to allocate the remaining pseudo registers.  */

      allocno_order = XNEWVEC (int, max_allocno);
      for (i = 0; i < (size_t) max_allocno; i++)
	allocno_order[i] = i;

      /* Default the size to 1, since allocno_compare uses it to divide by.
	 Also convert allocno_live_length of zero to -1.  A length of zero
	 can occur when all the registers for that allocno have reg_live_length
	 equal to -2.  In this case, we want to make an allocno, but not
	 allocate it.  So avoid the divide-by-zero and set it to a low
	 priority.  */

      for (i = 0; i < (size_t) max_allocno; i++)
	{
	  if (allocno[i].size == 0)
	    allocno[i].size = 1;
	  if (allocno[i].live_length == 0)
	    allocno[i].live_length = -1;
	}

      qsort (allocno_order, max_allocno, sizeof (int), allocno_compare);

      prune_preferences ();

      if (dump_file)
	dump_conflicts (dump_file);

      /* Try allocating them, one by one, in that order,
	 except for parameters marked with reg_live_length[regno] == -2.  */

      for (i = 0; i < (size_t) max_allocno; i++)
	if (reg_renumber[allocno[allocno_order[i]].reg] < 0
	    && REG_LIVE_LENGTH (allocno[allocno_order[i]].reg) >= 0)
	  {
            if (!dbg_cnt (global_alloc_at_reg))
              break;
	    /* If we have more than one register class,
	       first try allocating in the class that is cheapest
	       for this pseudo-reg.  If that fails, try any reg.  */
	    if (N_REG_CLASSES > 1)
	      {
		find_reg (allocno_order[i], 0, 0, 0, 0);
		if (reg_renumber[allocno[allocno_order[i]].reg] >= 0)
		  continue;
	      }
	    if (reg_alternate_class (allocno[allocno_order[i]].reg) != NO_REGS)
	      find_reg (allocno_order[i], 0, 1, 0, 0);
	  }

      free (allocno_order);
    }

  /* Do the reloads now while the allocno data still exists, so that we can
     try to assign new hard regs to any pseudo regs that are spilled.  */

#if 0 /* We need to eliminate regs even if there is no rtl code,
	 for the sake of debugging information.  */
  if (n_basic_blocks > NUM_FIXED_BLOCKS)
#endif
    {
      build_insn_chain (get_insns ());
      retval = reload (get_insns (), 1);
    }

  /* Clean up.  */
  free (reg_allocno);
  free (allocno);
  free (conflicts);
  free (allocnos_live);

  return retval;
}

/* Sort predicate for ordering the allocnos.
   Returns -1 (1) if *v1 should be allocated before (after) *v2.  */

static int
allocno_compare (const void *v1p, const void *v2p)
{
  int v1 = *(const int *)v1p, v2 = *(const int *)v2p;
  /* Note that the quotient will never be bigger than
     the value of floor_log2 times the maximum number of
     times a register can occur in one insn (surely less than 100)
     weighted by the frequency (maximally REG_FREQ_MAX).
     Multiplying this by 10000/REG_FREQ_MAX can't overflow.  */
  int pri1
    = (((double) (floor_log2 (allocno[v1].n_refs) * allocno[v1].freq)
	/ allocno[v1].live_length)
       * (10000 / REG_FREQ_MAX) * allocno[v1].size);
  int pri2
    = (((double) (floor_log2 (allocno[v2].n_refs) * allocno[v2].freq)
	/ allocno[v2].live_length)
       * (10000 / REG_FREQ_MAX) * allocno[v2].size);
  if (pri2 - pri1)
    return pri2 - pri1;

  /* If regs are equally good, sort by allocno,
     so that the results of qsort leave nothing to chance.  */
  return v1 - v2;
}

/* Scan the rtl code and record all conflicts and register preferences in the
   conflict matrices and preference tables.  */

static void
global_conflicts (void)
{
  unsigned i;
  basic_block b;
  rtx insn;
  int *block_start_allocnos;

  block_start_allocnos = XNEWVEC (int, max_allocno);

  FOR_EACH_BB (b)
    {
      memset (allocnos_live, 0, allocno_row_words * sizeof (INT_TYPE));

      /* Initialize table of registers currently live
	 to the state at the beginning of this basic block.
	 This also marks the conflicts among hard registers
	 and any allocnos that are live.

	 For pseudo-regs, there is only one bit for each one
	 no matter how many hard regs it occupies.
	 This is ok; we know the size from PSEUDO_REGNO_SIZE.
	 For explicit hard regs, we cannot know the size that way
	 since one hard reg can be used with various sizes.
	 Therefore, we must require that all the hard regs
	 implicitly live as part of a multi-word hard reg
	 be explicitly marked in basic_block_live_at_start.  */

      {
	int ax = 0;
	reg_set_iterator rsi;

	REG_SET_TO_HARD_REG_SET (hard_regs_live, DF_RA_LIVE_TOP (b));
	EXECUTE_IF_SET_IN_REG_SET (DF_RA_LIVE_TOP (b), FIRST_PSEUDO_REGISTER, i, rsi)
	  {
	    int a = reg_allocno[i];
	    if (a >= 0)
	      {
		SET_ALLOCNO_LIVE (a);
		block_start_allocnos[ax++] = a;
	      }
	    else if ((a = reg_renumber[i]) >= 0)
	      add_to_hard_reg_set (&hard_regs_live, PSEUDO_REGNO_MODE (i), a);
	  }

	/* Record that each allocno now live conflicts with each hard reg
	   now live.

	   It is not necessary to mark any conflicts between pseudos at
	   this point, even for pseudos which are live at the start of
	   the basic block.

	     Given two pseudos X and Y and any point in the CFG P.

	     On any path to point P where X and Y are live one of the
	     following conditions must be true:

		1. X is live at some instruction on the path that
		   evaluates Y.

		2. Y is live at some instruction on the path that
		   evaluates X.

		3. Either X or Y is not evaluated on the path to P
		   (i.e. it is used uninitialized) and thus the
		   conflict can be ignored.

	    In cases #1 and #2 the conflict will be recorded when we
	    scan the instruction that makes either X or Y become live.  */
	record_conflicts (block_start_allocnos, ax);

#ifdef EH_RETURN_DATA_REGNO
	if (bb_has_eh_pred (b))
	  {
	    unsigned int i;
	    
	    for (i = 0; ; ++i)
	      {
		unsigned int regno = EH_RETURN_DATA_REGNO (i);
		if (regno == INVALID_REGNUM)
		  break;
		record_one_conflict (regno);
	      }
	  }
#endif

	/* Pseudos can't go in stack regs at the start of a basic block that
	   is reached by an abnormal edge. Likewise for call clobbered regs,
	   because caller-save, fixup_abnormal_edges and possibly the table
	   driven EH machinery are not quite ready to handle such regs live
	   across such edges.  */
	{
	  edge e;
	  edge_iterator ei;

	  FOR_EACH_EDGE (e, ei, b->preds)
	    if (e->flags & EDGE_ABNORMAL)
	      break;

	  if (e != NULL)
	    {
#ifdef STACK_REGS
	      EXECUTE_IF_SET_IN_ALLOCNO_SET (allocnos_live, ax,
					     {
					       allocno[ax].no_stack_reg = 1;
					     });
	      for (ax = FIRST_STACK_REG; ax <= LAST_STACK_REG; ax++)
		record_one_conflict (ax);
#endif

	      /* No need to record conflicts for call clobbered regs if we have
		 nonlocal labels around, as we don't ever try to allocate such
		 regs in this case.  */
	      if (! current_function_has_nonlocal_label)
		for (ax = 0; ax < FIRST_PSEUDO_REGISTER; ax++)
		  if (call_used_regs [ax])
		    record_one_conflict (ax);
	    }
	}
      }

      insn = BB_HEAD (b);

      /* Scan the code of this basic block, noting which allocnos
	 and hard regs are born or die.  When one is born,
	 record a conflict with all others currently live.  */

      while (1)
	{
	  RTX_CODE code = GET_CODE (insn);
	  rtx link;

	  gcc_assert (VEC_empty (rtx, regs_set));
	  if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	    {
#if 0
	      int i = 0;
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

	      note_stores (PATTERN (insn), mark_reg_clobber, NULL);

#ifdef AUTO_INC_DEC
	      /* Auto-increment instructions clobber the base
		 register.  */
	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_INC)
		  mark_reg_store (XEXP (link, 0), NULL_RTX, NULL);
#endif
	      /* Mark any registers dead after INSN as dead now.  */

	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD)
		  mark_reg_death (XEXP (link, 0));

	      /* Mark any registers set in INSN as live,
		 and mark them as conflicting with all other live regs.
		 Clobbers are processed again, so they conflict with
		 the registers that are set.  */

	      note_stores (PATTERN (insn), mark_reg_store, NULL);

	      /* If INSN has multiple outputs, then any reg that dies here
		 and is used inside of an output
		 must conflict with the other outputs.

		 It is unsafe to use !single_set here since it will ignore an
		 unused output.  Just because an output is unused does not mean
		 the compiler can assume the side effect will not occur.
		 Consider if REG appears in the address of an output and we
		 reload the output.  If we allocate REG to the same hard
		 register as an unused output we could set the hard register
		 before the output reload insn.  */
	      if (GET_CODE (PATTERN (insn)) == PARALLEL && multiple_sets (insn))
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
			      && !REG_P (SET_DEST (set))
			      && !rtx_equal_p (reg, SET_DEST (set))
			      && reg_overlap_mentioned_p (reg, SET_DEST (set)))
			    used_in_output = 1;
			}
		      if (used_in_output)
			mark_reg_conflicts (reg);
		    }

	      /* Mark any registers set in INSN and then never used.  */

	      while (!VEC_empty (rtx, regs_set))
		{
		  rtx reg = VEC_pop (rtx, regs_set);
		  rtx note = find_regno_note (insn, REG_UNUSED,
					      REGNO (reg));
		  if (note)
		    mark_reg_death (XEXP (note, 0));
		}
	    }

	  if (insn == BB_END (b))
	    break;
	  insn = NEXT_INSN (insn);
	}
    }

  /* Clean up.  */
  free (block_start_allocnos);
}

/* Expand the preference information by looking for cases where one allocno
   dies in an insn that sets an allocno.  If those two allocnos don't conflict,
   merge any preferences between those allocnos.  */

static void
expand_preferences (void)
{
  rtx insn;
  rtx link;
  rtx set;

  /* We only try to handle the most common cases here.  Most of the cases
     where this wins are reg-reg copies.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& (set = single_set (insn)) != 0
	&& REG_P (SET_DEST (set))
	&& reg_allocno[REGNO (SET_DEST (set))] >= 0)
      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	if (REG_NOTE_KIND (link) == REG_DEAD
	    && REG_P (XEXP (link, 0))
	    && reg_allocno[REGNO (XEXP (link, 0))] >= 0
	    && ! CONFLICTP (reg_allocno[REGNO (SET_DEST (set))],
			    reg_allocno[REGNO (XEXP (link, 0))]))
	  {
	    int a1 = reg_allocno[REGNO (SET_DEST (set))];
	    int a2 = reg_allocno[REGNO (XEXP (link, 0))];

	    if (XEXP (link, 0) == SET_SRC (set))
	      {
		IOR_HARD_REG_SET (allocno[a1].hard_reg_copy_preferences,
				  allocno[a2].hard_reg_copy_preferences);
		IOR_HARD_REG_SET (allocno[a2].hard_reg_copy_preferences,
				  allocno[a1].hard_reg_copy_preferences);
	      }

	    IOR_HARD_REG_SET (allocno[a1].hard_reg_preferences,
			      allocno[a2].hard_reg_preferences);
	    IOR_HARD_REG_SET (allocno[a2].hard_reg_preferences,
			      allocno[a1].hard_reg_preferences);
	    IOR_HARD_REG_SET (allocno[a1].hard_reg_full_preferences,
			      allocno[a2].hard_reg_full_preferences);
	    IOR_HARD_REG_SET (allocno[a2].hard_reg_full_preferences,
			      allocno[a1].hard_reg_full_preferences);
	  }
}

/* Prune the preferences for global registers to exclude registers that cannot
   be used.

   Compute `regs_someone_prefers', which is a bitmask of the hard registers
   that are preferred by conflicting registers of lower priority.  If possible,
   we will avoid using these registers.  */

static void
prune_preferences (void)
{
  int i;
  int num;
  int *allocno_to_order = XNEWVEC (int, max_allocno);

  /* Scan least most important to most important.
     For each allocno, remove from preferences registers that cannot be used,
     either because of conflicts or register type.  Then compute all registers
     preferred by each lower-priority register that conflicts.  */

  for (i = max_allocno - 1; i >= 0; i--)
    {
      HARD_REG_SET temp;

      num = allocno_order[i];
      allocno_to_order[num] = i;
      COPY_HARD_REG_SET (temp, allocno[num].hard_reg_conflicts);

      if (allocno[num].calls_crossed == 0)
	IOR_HARD_REG_SET (temp, fixed_reg_set);
      else
	IOR_HARD_REG_SET (temp,	call_used_reg_set);

      IOR_COMPL_HARD_REG_SET
	(temp,
	 reg_class_contents[(int) reg_preferred_class (allocno[num].reg)]);

      AND_COMPL_HARD_REG_SET (allocno[num].hard_reg_preferences, temp);
      AND_COMPL_HARD_REG_SET (allocno[num].hard_reg_copy_preferences, temp);
      AND_COMPL_HARD_REG_SET (allocno[num].hard_reg_full_preferences, temp);
    }

  for (i = max_allocno - 1; i >= 0; i--)
    {
      /* Merge in the preferences of lower-priority registers (they have
	 already been pruned).  If we also prefer some of those registers,
	 don't exclude them unless we are of a smaller size (in which case
	 we want to give the lower-priority allocno the first chance for
	 these registers).  */
      HARD_REG_SET temp, temp2;
      int allocno2;

      num = allocno_order[i];

      CLEAR_HARD_REG_SET (temp);
      CLEAR_HARD_REG_SET (temp2);

      EXECUTE_IF_SET_IN_ALLOCNO_SET (conflicts + num * allocno_row_words,
				     allocno2,
	{
	  if (allocno_to_order[allocno2] > i)
	    {
	      if (allocno[allocno2].size <= allocno[num].size)
		IOR_HARD_REG_SET (temp,
				  allocno[allocno2].hard_reg_full_preferences);
	      else
		IOR_HARD_REG_SET (temp2,
				  allocno[allocno2].hard_reg_full_preferences);
	    }
	});

      AND_COMPL_HARD_REG_SET (temp, allocno[num].hard_reg_full_preferences);
      IOR_HARD_REG_SET (temp, temp2);
      COPY_HARD_REG_SET (allocno[num].regs_someone_prefers, temp);
    }
  free (allocno_to_order);
}

/* Assign a hard register to allocno NUM; look for one that is the beginning
   of a long enough stretch of hard regs none of which conflicts with ALLOCNO.
   The registers marked in PREFREGS are tried first.

   LOSERS, if nonzero, is a HARD_REG_SET indicating registers that cannot
   be used for this allocation.

   If ALT_REGS_P is zero, consider only the preferred class of ALLOCNO's reg.
   Otherwise ignore that preferred class and use the alternate class.

   If ACCEPT_CALL_CLOBBERED is nonzero, accept a call-clobbered hard reg that
   will have to be saved and restored at calls.

   RETRYING is nonzero if this is called from retry_global_alloc.

   If we find one, record it in reg_renumber.
   If not, do nothing.  */

static void
find_reg (int num, HARD_REG_SET losers, int alt_regs_p, int accept_call_clobbered, int retrying)
{
  int i, best_reg, pass;
  HARD_REG_SET used, used1, used2;

  enum reg_class class = (alt_regs_p
			  ? reg_alternate_class (allocno[num].reg)
			  : reg_preferred_class (allocno[num].reg));
  enum machine_mode mode = PSEUDO_REGNO_MODE (allocno[num].reg);

  if (accept_call_clobbered)
    COPY_HARD_REG_SET (used1, call_fixed_reg_set);
  else if (allocno[num].calls_crossed == 0)
    COPY_HARD_REG_SET (used1, fixed_reg_set);
  else
    COPY_HARD_REG_SET (used1, call_used_reg_set);

  /* Some registers should not be allocated in global-alloc.  */
  IOR_HARD_REG_SET (used1, no_global_alloc_regs);
  if (losers)
    IOR_HARD_REG_SET (used1, losers);

  IOR_COMPL_HARD_REG_SET (used1, reg_class_contents[(int) class]);
  COPY_HARD_REG_SET (used2, used1);

  IOR_HARD_REG_SET (used1, allocno[num].hard_reg_conflicts);

#ifdef CANNOT_CHANGE_MODE_CLASS
  cannot_change_mode_set_regs (&used1, mode, allocno[num].reg);
#endif

  /* Try each hard reg to see if it fits.  Do this in two passes.
     In the first pass, skip registers that are preferred by some other pseudo
     to give it a better chance of getting one of those registers.  Only if
     we can't get a register when excluding those do we take one of them.
     However, we never allocate a register for the first time in pass 0.  */

  COPY_HARD_REG_SET (used, used1);
  IOR_COMPL_HARD_REG_SET (used, regs_used_so_far);
  IOR_HARD_REG_SET (used, allocno[num].regs_someone_prefers);

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
	      && HARD_REGNO_MODE_OK (regno, mode)
	      && (allocno[num].calls_crossed == 0
		  || accept_call_clobbered
		  || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode)))
	    {
	      int j;
	      int lim = end_hard_regno (mode, regno);
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

  AND_COMPL_HARD_REG_SET (allocno[num].hard_reg_copy_preferences, used);
  if (!hard_reg_set_empty_p (allocno[num].hard_reg_copy_preferences)
      && best_reg >= 0)
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (allocno[num].hard_reg_copy_preferences, i)
	    && HARD_REGNO_MODE_OK (i, mode)
	    && (allocno[num].calls_crossed == 0
		|| accept_call_clobbered
		|| ! HARD_REGNO_CALL_PART_CLOBBERED (i, mode))
	    && (REGNO_REG_CLASS (i) == REGNO_REG_CLASS (best_reg)
		|| reg_class_subset_p (REGNO_REG_CLASS (i),
				       REGNO_REG_CLASS (best_reg))
		|| reg_class_subset_p (REGNO_REG_CLASS (best_reg),
				       REGNO_REG_CLASS (i))))
	    {
	      int j;
	      int lim = end_hard_regno (mode, i);
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

  AND_COMPL_HARD_REG_SET (allocno[num].hard_reg_preferences, used);
  if (!hard_reg_set_empty_p (allocno[num].hard_reg_preferences)
      && best_reg >= 0)
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (TEST_HARD_REG_BIT (allocno[num].hard_reg_preferences, i)
	    && HARD_REGNO_MODE_OK (i, mode)
	    && (allocno[num].calls_crossed == 0
		|| accept_call_clobbered
		|| ! HARD_REGNO_CALL_PART_CLOBBERED (i, mode))
	    && (REGNO_REG_CLASS (i) == REGNO_REG_CLASS (best_reg)
		|| reg_class_subset_p (REGNO_REG_CLASS (i),
				       REGNO_REG_CLASS (best_reg))
		|| reg_class_subset_p (REGNO_REG_CLASS (best_reg),
				       REGNO_REG_CLASS (i))))
	    {
	      int j;
	      int lim = end_hard_regno (mode, i);
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

  /* If we haven't succeeded yet, try with caller-saves.
     We need not check to see if the current function has nonlocal
     labels because we don't put any pseudos that are live over calls in
     registers in that case.  */

  if (flag_caller_saves && best_reg < 0)
    {
      /* Did not find a register.  If it would be profitable to
	 allocate a call-clobbered register and save and restore it
	 around calls, do that.  Don't do this if it crosses any calls
	 that might throw.  */
      if (! accept_call_clobbered
	  && allocno[num].calls_crossed != 0
	  && allocno[num].throwing_calls_crossed == 0
	  && CALLER_SAVE_PROFITABLE (allocno[num].n_refs,
				     allocno[num].calls_crossed))
	{
	  HARD_REG_SET new_losers;
	  if (! losers)
	    CLEAR_HARD_REG_SET (new_losers);
	  else
	    COPY_HARD_REG_SET (new_losers, losers);

	  IOR_HARD_REG_SET(new_losers, losing_caller_save_reg_set);
	  find_reg (num, new_losers, alt_regs_p, 1, retrying);
	  if (reg_renumber[allocno[num].reg] >= 0)
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
      && allocno[num].size == 1
      && REG_BASIC_BLOCK (allocno[num].reg) == REG_BLOCK_GLOBAL)
    {
      /* Count from the end, to find the least-used ones first.  */
      for (i = FIRST_PSEUDO_REGISTER - 1; i >= 0; i--)
	{
#ifdef REG_ALLOC_ORDER
	  int regno = reg_alloc_order[i];
#else
	  int regno = i;
#endif

	  if (local_reg_n_refs[regno] != 0
	      /* Don't use a reg no good for this pseudo.  */
	      && ! TEST_HARD_REG_BIT (used2, regno)
	      && HARD_REGNO_MODE_OK (regno, mode)
	      /* The code below assumes that we need only a single
		 register, but the check of allocno[num].size above
		 was not enough.  Sometimes we need more than one
		 register for a single-word value.  */
	      && hard_regno_nregs[regno][mode] == 1
	      && (allocno[num].calls_crossed == 0
		  || accept_call_clobbered
		  || ! HARD_REGNO_CALL_PART_CLOBBERED (regno, mode))
#ifdef CANNOT_CHANGE_MODE_CLASS
	      && ! invalid_mode_change_p (regno, REGNO_REG_CLASS (regno),
					  mode)
#endif
#ifdef STACK_REGS
	     && (!allocno[num].no_stack_reg
		 || regno < FIRST_STACK_REG || regno > LAST_STACK_REG)
#endif
	      )
	    {
	      /* We explicitly evaluate the divide results into temporary
		 variables so as to avoid excess precision problems that occur
		 on an i386-unknown-sysv4.2 (unixware) host.  */

	      double tmp1 = ((double) local_reg_freq[regno] * local_reg_n_refs[regno]
			    / local_reg_live_length[regno]);
	      double tmp2 = ((double) allocno[num].freq * allocno[num].n_refs
			     / allocno[num].live_length);

	      if (tmp1 < tmp2)
		{
		  /* Hard reg REGNO was used less in total by local regs
		     than it would be used by this one allocno!  */
		  int k;
		  if (dump_file)
		    {
		      fprintf (dump_file, "Regno %d better for global %d, ",
		      	       regno, allocno[num].reg);
		      fprintf (dump_file, "fr:%d, ll:%d, nr:%d ",
			       allocno[num].freq, allocno[num].live_length,
			       allocno[num].n_refs);
		      fprintf (dump_file, "(was: fr:%d, ll:%d, nr:%d)\n",
			       local_reg_freq[regno],
			       local_reg_live_length[regno],
			       local_reg_n_refs[regno]);
		    }

		  for (k = 0; k < max_regno; k++)
		    if (reg_renumber[k] >= 0)
		      {
			int r = reg_renumber[k];
			int endregno
			  = end_hard_regno (PSEUDO_REGNO_MODE (k), r);

			if (regno >= r && regno < endregno)
			  {
			    if (dump_file)
			      fprintf (dump_file,
				       "Local Reg %d now on stack\n", k);
			    reg_renumber[k] = -1;
			  }
		      }

		  best_reg = regno;
		  break;
		}
	    }
	}
    }

  /* Did we find a register?  */

  if (best_reg >= 0)
    {
      int lim, j;
      HARD_REG_SET this_reg;

      /* Yes.  Record it as the hard register of this pseudo-reg.  */
      reg_renumber[allocno[num].reg] = best_reg;

      /* Make a set of the hard regs being allocated.  */
      CLEAR_HARD_REG_SET (this_reg);
      lim = end_hard_regno (mode, best_reg);
      for (j = best_reg; j < lim; j++)
	{
	  SET_HARD_REG_BIT (this_reg, j);
	  SET_HARD_REG_BIT (regs_used_so_far, j);
	  /* This is no longer a reg used just by local regs.  */
	  local_reg_n_refs[j] = 0;
	  local_reg_freq[j] = 0;
	}
      /* For each other pseudo-reg conflicting with this one,
	 mark it as conflicting with the hard regs this one occupies.  */
      lim = num;
      EXECUTE_IF_SET_IN_ALLOCNO_SET (conflicts + lim * allocno_row_words, j,
	{
	  IOR_HARD_REG_SET (allocno[j].hard_reg_conflicts, this_reg);
	});
    }
}

/* Called from `reload' to look for a hard reg to put pseudo reg REGNO in.
   Perhaps it had previously seemed not worth a hard reg,
   or perhaps its old hard reg has been commandeered for reloads.
   FORBIDDEN_REGS indicates certain hard regs that may not be used, even if
   they do not appear to be allocated.
   If FORBIDDEN_REGS is zero, no regs are forbidden.  */

void
retry_global_alloc (int regno, HARD_REG_SET forbidden_regs)
{
  int alloc_no = reg_allocno[regno];
  if (alloc_no >= 0)
    {
      /* If we have more than one register class,
	 first try allocating in the class that is cheapest
	 for this pseudo-reg.  If that fails, try any reg.  */
      if (N_REG_CLASSES > 1)
	find_reg (alloc_no, forbidden_regs, 0, 0, 1);
      if (reg_renumber[regno] < 0
	  && reg_alternate_class (regno) != NO_REGS)
	find_reg (alloc_no, forbidden_regs, 1, 0, 1);

      /* If we found a register, modify the RTL for the register to
	 show the hard register, and mark that register live.  */
      if (reg_renumber[regno] >= 0)
	{
	  SET_REGNO (regno_reg_rtx[regno], reg_renumber[regno]);
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
record_one_conflict (int regno)
{
  int j;

  if (regno < FIRST_PSEUDO_REGISTER)
    /* When a hard register becomes live,
       record conflicts with live pseudo regs.  */
    EXECUTE_IF_SET_IN_ALLOCNO_SET (allocnos_live, j,
      {
	SET_HARD_REG_BIT (allocno[j].hard_reg_conflicts, regno);
      });
  else
    /* When a pseudo-register becomes live,
       record conflicts first with hard regs,
       then with other pseudo regs.  */
    {
      int ialloc = reg_allocno[regno];
      int ialloc_prod = ialloc * allocno_row_words;

      IOR_HARD_REG_SET (allocno[ialloc].hard_reg_conflicts, hard_regs_live);
      for (j = allocno_row_words - 1; j >= 0; j--)
	conflicts[ialloc_prod + j] |= allocnos_live[j];
    }
}

/* Record all allocnos currently live as conflicting
   with all hard regs currently live.

   ALLOCNO_VEC is a vector of LEN allocnos, all allocnos that
   are currently live.  Their bits are also flagged in allocnos_live.  */

static void
record_conflicts (int *allocno_vec, int len)
{
  while (--len >= 0)
    IOR_HARD_REG_SET (allocno[allocno_vec[len]].hard_reg_conflicts,
                      hard_regs_live);
}

/* If CONFLICTP (i, j) is true, make sure CONFLICTP (j, i) is also true.  */
static void
mirror_conflicts (void)
{
  int i, j;
  int rw = allocno_row_words;
  int rwb = rw * INT_BITS;
  INT_TYPE *p = conflicts;
  INT_TYPE *q0 = conflicts, *q1, *q2;
  unsigned INT_TYPE mask;

  for (i = max_allocno - 1, mask = 1; i >= 0; i--, mask <<= 1)
    {
      if (! mask)
	{
	  mask = 1;
	  q0++;
	}
      for (j = allocno_row_words - 1, q1 = q0; j >= 0; j--, q1 += rwb)
	{
	  unsigned INT_TYPE word;

	  for (word = (unsigned INT_TYPE) *p++, q2 = q1; word;
	       word >>= 1, q2 += rw)
	    {
	      if (word & 1)
		*q2 |= mask;
	    }
	}
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
   a REG_INC note was found for it).  */

static void
mark_reg_store (rtx reg, rtx setter, void *data ATTRIBUTE_UNUSED)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return;

  VEC_safe_push (rtx, heap, regs_set, reg);

  if (setter && GET_CODE (setter) != CLOBBER)
    set_preference (reg, SET_SRC (setter));

  regno = REGNO (reg);

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

  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (regno < FIRST_PSEUDO_REGISTER && ! fixed_regs[regno])
    {
      int last = end_hard_regno (GET_MODE (reg), regno);
      while (regno < last)
	{
	  record_one_conflict (regno);
	  SET_HARD_REG_BIT (hard_regs_live, regno);
	  regno++;
	}
    }
}

/* Like mark_reg_store except notice just CLOBBERs; ignore SETs.  */

static void
mark_reg_clobber (rtx reg, rtx setter, void *data)
{
  if (GET_CODE (setter) == CLOBBER)
    mark_reg_store (reg, setter, data);
}

/* Record that REG has conflicts with all the regs currently live.
   Do not mark REG itself as live.  */

static void
mark_reg_conflicts (rtx reg)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return;

  regno = REGNO (reg);

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is or has a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_allocno[regno] >= 0)
	record_one_conflict (regno);
    }

  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (regno < FIRST_PSEUDO_REGISTER && ! fixed_regs[regno])
    {
      int last = end_hard_regno (GET_MODE (reg), regno);
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
mark_reg_death (rtx reg)
{
  int regno = REGNO (reg);

  /* Either this is one of the max_allocno pseudo regs not allocated,
     or it is a hardware reg.  First handle the pseudo-regs.  */
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (reg_allocno[regno] >= 0)
	CLEAR_ALLOCNO_LIVE (reg_allocno[regno]);
    }

  /* For pseudo reg, see if it has been assigned a hardware reg.  */
  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (regno < FIRST_PSEUDO_REGISTER && ! fixed_regs[regno])
    /* Pseudo regs already assigned hardware regs are treated
       almost the same as explicit hardware regs.  */
    remove_from_hard_reg_set (&hard_regs_live, GET_MODE (reg), regno);
}

/* Try to set a preference for an allocno to a hard register.
   We are passed DEST and SRC which are the operands of a SET.  It is known
   that SRC is a register.  If SRC or the first operand of SRC is a register,
   try to set a preference.  If one of the two is a hard register and the other
   is a pseudo-register, mark the preference.

   Note that we are not as aggressive as local-alloc in trying to tie a
   pseudo-register to a hard register.  */

static void
set_preference (rtx dest, rtx src)
{
  unsigned int src_regno, dest_regno, end_regno;
  /* Amount to add to the hard regno for SRC, or subtract from that for DEST,
     to compensate for subregs in SRC or DEST.  */
  int offset = 0;
  unsigned int i;
  int copy = 1;

  if (GET_RTX_FORMAT (GET_CODE (src))[0] == 'e')
    src = XEXP (src, 0), copy = 0;

  /* Get the reg number for both SRC and DEST.
     If neither is a reg, give up.  */

  if (REG_P (src))
    src_regno = REGNO (src);
  else if (GET_CODE (src) == SUBREG && REG_P (SUBREG_REG (src)))
    {
      src_regno = REGNO (SUBREG_REG (src));

      if (REGNO (SUBREG_REG (src)) < FIRST_PSEUDO_REGISTER)
	offset += subreg_regno_offset (REGNO (SUBREG_REG (src)),
				       GET_MODE (SUBREG_REG (src)),
				       SUBREG_BYTE (src),
				       GET_MODE (src));
      else
	offset += (SUBREG_BYTE (src)
		   / REGMODE_NATURAL_SIZE (GET_MODE (src)));
    }
  else
    return;

  if (REG_P (dest))
    dest_regno = REGNO (dest);
  else if (GET_CODE (dest) == SUBREG && REG_P (SUBREG_REG (dest)))
    {
      dest_regno = REGNO (SUBREG_REG (dest));

      if (REGNO (SUBREG_REG (dest)) < FIRST_PSEUDO_REGISTER)
	offset -= subreg_regno_offset (REGNO (SUBREG_REG (dest)),
				       GET_MODE (SUBREG_REG (dest)),
				       SUBREG_BYTE (dest),
				       GET_MODE (dest));
      else
	offset -= (SUBREG_BYTE (dest)
		   / REGMODE_NATURAL_SIZE (GET_MODE (dest)));
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
      if (dest_regno < FIRST_PSEUDO_REGISTER)
	{
	  if (copy)
	    SET_REGBIT (hard_reg_copy_preferences,
			reg_allocno[src_regno], dest_regno);

	  SET_REGBIT (hard_reg_preferences,
		      reg_allocno[src_regno], dest_regno);
	  end_regno = end_hard_regno (GET_MODE (dest), dest_regno);
	  for (i = dest_regno; i < end_regno; i++)
	    SET_REGBIT (hard_reg_full_preferences, reg_allocno[src_regno], i);
	}
    }

  if (src_regno < FIRST_PSEUDO_REGISTER && dest_regno >= FIRST_PSEUDO_REGISTER
      && reg_allocno[dest_regno] >= 0)
    {
      src_regno += offset;
      if (src_regno < FIRST_PSEUDO_REGISTER)
	{
	  if (copy)
	    SET_REGBIT (hard_reg_copy_preferences,
			reg_allocno[dest_regno], src_regno);

	  SET_REGBIT (hard_reg_preferences,
		      reg_allocno[dest_regno], src_regno);
	  end_regno = end_hard_regno (GET_MODE (src), src_regno);
	  for (i = src_regno; i < end_regno; i++)
	    SET_REGBIT (hard_reg_full_preferences, reg_allocno[dest_regno], i);
	}
    }
}

/* Indicate that hard register number FROM was eliminated and replaced with
   an offset from hard register number TO.  The status of hard registers live
   at the start of a basic block is updated by replacing a use of FROM with
   a use of TO.  */

void
mark_elimination (int from, int to)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      regset r = DF_RA_LIVE_IN (bb);
      if (REGNO_REG_SET_P (r, from))
	{
	  CLEAR_REGNO_REG_SET (r, from);
	  SET_REGNO_REG_SET (r, to);
	}
    }
}

/* Used for communication between the following functions.  Holds the
   current life information.  */
static regset live_relevant_regs;

/* Record in live_relevant_regs and REGS_SET that register REG became live.
   This is called via note_stores.  */
static void
reg_becomes_live (rtx reg, rtx setter ATTRIBUTE_UNUSED, void *regs_set)
{
  int regno;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      int nregs = hard_regno_nregs[regno][GET_MODE (reg)];
      while (nregs-- > 0)
	{
	  if (GET_CODE (setter) == CLOBBER)
	    CLEAR_REGNO_REG_SET (live_relevant_regs, regno);
	  else
	    SET_REGNO_REG_SET (live_relevant_regs, regno);

	  if (!fixed_regs[regno])
	    SET_REGNO_REG_SET ((regset) regs_set, regno);
	  regno++;
	}
    }
  else if (reg_renumber[regno] >= 0)
    {
      if (GET_CODE (setter) == CLOBBER)
	CLEAR_REGNO_REG_SET (live_relevant_regs, regno);
      else
	SET_REGNO_REG_SET (live_relevant_regs, regno);
      SET_REGNO_REG_SET ((regset) regs_set, regno);
    }
}

/* Record in live_relevant_regs that register REGNO died.  */
static void
reg_dies (int regno, enum machine_mode mode, struct insn_chain *chain)
{
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      int nregs = hard_regno_nregs[regno][mode];
      while (nregs-- > 0)
	{
	  CLEAR_REGNO_REG_SET (live_relevant_regs, regno);
	  if (! fixed_regs[regno])
	    SET_REGNO_REG_SET (&chain->dead_or_set, regno);
	  regno++;
	}
    }
  else
    {
      CLEAR_REGNO_REG_SET (live_relevant_regs, regno);
      if (reg_renumber[regno] >= 0)
	SET_REGNO_REG_SET (&chain->dead_or_set, regno);
    }
}

/* Walk the insns of the current function and build reload_insn_chain,
   and record register life information.  */
void
build_insn_chain (rtx first)
{
  struct insn_chain **p = &reload_insn_chain;
  struct insn_chain *prev = 0;
  basic_block b = ENTRY_BLOCK_PTR->next_bb;

  live_relevant_regs = ALLOC_REG_SET (&reg_obstack);

  for (; first; first = NEXT_INSN (first))
    {
      struct insn_chain *c;

      if (first == BB_HEAD (b))
	{
	  unsigned i;
	  bitmap_iterator bi;

	  CLEAR_REG_SET (live_relevant_regs);

	  EXECUTE_IF_SET_IN_BITMAP (df_get_live_top (b), 0, i, bi)
	    {
	      if (i < FIRST_PSEUDO_REGISTER
		  ? ! TEST_HARD_REG_BIT (eliminable_regset, i)
		  : reg_renumber[i] >= 0)
		SET_REGNO_REG_SET (live_relevant_regs, i);
	    }
	}

      if (!NOTE_P (first) && !BARRIER_P (first))
	{
	  c = new_insn_chain ();
	  c->prev = prev;
	  prev = c;
	  *p = c;
	  p = &c->next;
	  c->insn = first;
	  c->block = b->index;

	  if (INSN_P (first))
	    {
	      rtx link;

	      /* Mark the death of everything that dies in this instruction.  */

	      for (link = REG_NOTES (first); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD
		    && REG_P (XEXP (link, 0)))
		  reg_dies (REGNO (XEXP (link, 0)), GET_MODE (XEXP (link, 0)),
			    c);

	      COPY_REG_SET (&c->live_throughout, live_relevant_regs);

	      /* Mark everything born in this instruction as live.  */

	      note_stores (PATTERN (first), reg_becomes_live,
			   &c->dead_or_set);
	    }
	  else
	    COPY_REG_SET (&c->live_throughout, live_relevant_regs);

	  if (INSN_P (first))
	    {
	      rtx link;

	      /* Mark anything that is set in this insn and then unused as dying.  */

	      for (link = REG_NOTES (first); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_UNUSED
		    && REG_P (XEXP (link, 0)))
		  reg_dies (REGNO (XEXP (link, 0)), GET_MODE (XEXP (link, 0)),
			    c);
	    }
	}

      if (first == BB_END (b))
	b = b->next_bb;

      /* Stop after we pass the end of the last basic block.  Verify that
	 no real insns are after the end of the last basic block.

	 We may want to reorganize the loop somewhat since this test should
	 always be the right exit test.  Allow an ADDR_VEC or ADDR_DIF_VEC if
	 the previous real insn is a JUMP_INSN.  */
      if (b == EXIT_BLOCK_PTR)
	{
#ifdef ENABLE_CHECKING
	  for (first = NEXT_INSN (first); first; first = NEXT_INSN (first))
	    gcc_assert (!INSN_P (first)
			|| GET_CODE (PATTERN (first)) == USE
			|| ((GET_CODE (PATTERN (first)) == ADDR_VEC
			     || GET_CODE (PATTERN (first)) == ADDR_DIFF_VEC)
			    && prev_real_insn (first) != 0
			    && JUMP_P (prev_real_insn (first))));
#endif
	  break;
	}
    }
  FREE_REG_SET (live_relevant_regs);
  *p = 0;
}

/* Print debugging trace information if -dg switch is given,
   showing the information on which the allocation decisions are based.  */

static void
dump_conflicts (FILE *file)
{
  int i;
  int has_preferences;
  int nregs;
  nregs = 0;
  for (i = 0; i < max_allocno; i++)
    {
      if (reg_renumber[allocno[allocno_order[i]].reg] >= 0)
	continue;
      nregs++;
    }
  fprintf (file, ";; %d regs to allocate:", nregs);
  for (i = 0; i < max_allocno; i++)
    {
      int j;
      if (reg_renumber[allocno[allocno_order[i]].reg] >= 0)
	continue;
      fprintf (file, " %d", allocno[allocno_order[i]].reg);
      for (j = 0; j < max_regno; j++)
	if (reg_allocno[j] == allocno_order[i]
	    && j != allocno[allocno_order[i]].reg)
	  fprintf (file, "+%d", j);
      if (allocno[allocno_order[i]].size != 1)
	fprintf (file, " (%d)", allocno[allocno_order[i]].size);
    }
  fprintf (file, "\n");

  for (i = 0; i < max_allocno; i++)
    {
      int j;
      fprintf (file, ";; %d conflicts:", allocno[i].reg);
      for (j = 0; j < max_allocno; j++)
	if (CONFLICTP (j, i))
	  fprintf (file, " %d", allocno[j].reg);
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (allocno[i].hard_reg_conflicts, j))
	  fprintf (file, " %d", j);
      fprintf (file, "\n");

      has_preferences = 0;
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (allocno[i].hard_reg_preferences, j))
	  has_preferences = 1;

      if (! has_preferences)
	continue;
      fprintf (file, ";; %d preferences:", allocno[i].reg);
      for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	if (TEST_HARD_REG_BIT (allocno[i].hard_reg_preferences, j))
	  fprintf (file, " %d", j);
      fprintf (file, "\n");
    }
  fprintf (file, "\n");
}

void
dump_global_regs (FILE *file)
{
  int i, j;

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
    if (df_regs_ever_live_p (i))
      fprintf (file, " %d", i);
  fprintf (file, "\n\n");
}

/* Run old register allocator.  Return TRUE if we must exit
   rest_of_compilation upon return.  */
static unsigned int
rest_of_handle_global_alloc (void)
{
  bool failure;

  /* If optimizing, allocate remaining pseudo-regs.  Do the reload
     pass fixing up any insns that are invalid.  */
  if (optimize && dbg_cnt (global_alloc_at_func))
    failure = global_alloc ();
  else
    {
      compute_regsets (&eliminable_regset, &no_global_alloc_regs);
      build_insn_chain (get_insns ());
      df_set_flags (DF_NO_INSN_RESCAN);
      failure = reload (get_insns (), 0);
    }

  if (dump_enabled_p (pass_global_alloc.static_pass_number))
    {
      timevar_push (TV_DUMP);
      dump_global_regs (dump_file);
      timevar_pop (TV_DUMP);
    }

  /* FIXME: This appears on the surface to be wrong thing to be doing.
     So much of the compiler is designed to check reload_completed to
     see if it is running after reload that seems doomed to failure.
     We should be returning a value that says that we have found
     errors so that nothing but the cleanup passes are run
     afterwards.  */
  gcc_assert (reload_completed || failure);
  reload_completed = !failure;

  /* The world has changed so much that at this point we might as well
     just rescan everything.  Not that df_rescan_all_insns is not
     going to help here because it does not touch the artificial uses
     and defs.  */
  df_finish_pass ();
  if (optimize > 1)
    df_live_add_problem ();
  df_scan_alloc (NULL);
  df_scan_blocks ();

  if (optimize)
    df_analyze ();

  regstat_free_n_sets_and_refs ();
  regstat_free_ri ();
  return 0;
}

struct tree_opt_pass pass_global_alloc =
{
  "greg",                               /* name */
  NULL,                                 /* gate */
  rest_of_handle_global_alloc,          /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_GLOBAL_ALLOC,                      /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func |
  TODO_ggc_collect,                     /* todo_flags_finish */
  'g'                                   /* letter */
};

