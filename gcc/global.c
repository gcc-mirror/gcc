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
#include "ra.h"

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

   2. Allocate a max_allocno by max_allocno compressed triangular conflict
   bit matrix (a triangular bit matrix with portions removed for which we
   can guarantee there are no conflicts, example: two local pseudos that
   live in different basic blocks) and clear it.  This is called "conflict".
   Note that for triangular bit matrices, there are two possible equations
   for computing the bit number for two allocnos: LOW and HIGH (LOW < HIGH):

     1) BITNUM = f(HIGH) + LOW, where
       f(HIGH) = (HIGH * (HIGH - 1)) / 2

     2) BITNUM = f(LOW) + HIGH, where
       f(LOW) = LOW * (max_allocno - LOW) + (LOW * (LOW - 1)) / 2 - LOW - 1

   We use the second (and less common) equation as this gives us better
   cache locality for local allocnos that are live within the same basic
   block.  Also note that f(HIGH) and f(LOW) can be precalculated for all
   values of HIGH and LOW, so all that is necessary to compute the bit
   number for two allocnos LOW and HIGH is a load followed by an addition.

   Allocate a max_allocno by FIRST_PSEUDO_REGISTER conflict matrix for
   conflicts between allocnos and explicit hard register use (which
   includes use of pseudo-registers allocated by local_alloc).  This
   is the hard_reg_conflicts inside each allocno.

   3. For each basic block, walk backward through the block, recording
   which pseudo-registers and which hardware registers are live.
   Build the conflict matrix between the pseudo-registers and another of
   pseudo-registers versus hardware registers.

   4. For each basic block, walk backward through the block, recording
   the preferred hardware registers for each pseudo-register.

   5. Sort a table of the allocnos into order of desirability of the variables.

   6. Allocate the variables in that order; each if possible into
   a preferred register, else into another register.  */

/* A vector of the integers from 0 to max_allocno-1,
   sorted in the order of first-to-be-allocated first.  */

static int *allocno_order;

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

static int regno_compare (const void *, const void *);
static int allocno_compare (const void *, const void *);
static void expand_preferences (void);
static void prune_preferences (void);
static void set_preferences (void);
static void find_reg (int, HARD_REG_SET, int, int, int);
static void dump_conflicts (FILE *);
static void build_insn_chain (void);


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
  int max_blk;
  int *num_allocnos_per_blk;

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

  /* Initially fill the reg_allocno array with regno's...  */
  max_blk = 0;
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
	int blk = regno_basic_block (i);
	reg_allocno[max_allocno++] = i;
	if (blk > max_blk)
	  max_blk = blk;
	gcc_assert (REG_LIVE_LENGTH (i));
      }

  allocno = XCNEWVEC (struct allocno, max_allocno);
  partial_bitnum = XNEWVEC (HOST_WIDE_INT, max_allocno);
  num_allocnos_per_blk = XCNEWVEC (int, max_blk + 1);

  /* ...so we can sort them in the order we want them to receive
     their allocnos.  */
  qsort (reg_allocno, max_allocno, sizeof (int), regno_compare);

  for (i = 0; i < (size_t) max_allocno; i++)
    {
      int regno = reg_allocno[i];
      int blk = regno_basic_block (regno);
      num_allocnos_per_blk[blk]++;
      allocno[i].reg = regno;
      allocno[i].size = PSEUDO_REGNO_SIZE (regno);
      allocno[i].calls_crossed += REG_N_CALLS_CROSSED (regno);
      allocno[i].freq_calls_crossed += REG_FREQ_CALLS_CROSSED (regno);
      allocno[i].throwing_calls_crossed
	+= REG_N_THROWING_CALLS_CROSSED (regno);
      allocno[i].n_refs += REG_N_REFS (regno);
      allocno[i].freq += REG_FREQ (regno);
      if (allocno[i].live_length < REG_LIVE_LENGTH (regno))
	allocno[i].live_length = REG_LIVE_LENGTH (regno);
    }

  /* The "global" block must contain all allocnos.  */
  num_allocnos_per_blk[0] = max_allocno;

  /* Now reinitialize the reg_allocno array in terms of the
     optimized regno to allocno mapping we created above.  */
  for (i = 0; i < (size_t) max_regno; i++)
    reg_allocno[i] = -1;

  max_bitnum = 0;
  for (i = 0; i < (size_t) max_allocno; i++)
    {
      int regno = allocno[i].reg;
      int blk = regno_basic_block (regno);
      int row_size = --num_allocnos_per_blk[blk];
      reg_allocno[regno] = (int) i;
      partial_bitnum[i] = (row_size > 0) ? max_bitnum - ((int) i + 1) : -1;
      max_bitnum += row_size;
    }

#ifdef ENABLE_CHECKING
  gcc_assert (max_bitnum <=
	      (((HOST_WIDE_INT) max_allocno *
		((HOST_WIDE_INT) max_allocno - 1)) / 2));
#endif

  if (dump_file)
    {
      HOST_WIDE_INT num_bits, num_bytes, actual_bytes;

      fprintf (dump_file, "## max_blk:     %d\n", max_blk);
      fprintf (dump_file, "## max_regno:   %d\n", max_regno);
      fprintf (dump_file, "## max_allocno: %d\n", max_allocno);

      num_bits = max_bitnum;
      num_bytes = CEIL (num_bits, 8);
      actual_bytes = num_bytes;
      fprintf (dump_file, "## Compressed triangular bitmatrix size: ");
      fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC " bits, ", num_bits);
      fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC " bytes\n", num_bytes);

      num_bits = ((HOST_WIDE_INT) max_allocno *
		  ((HOST_WIDE_INT) max_allocno - 1)) / 2;
      num_bytes = CEIL (num_bits, 8);
      fprintf (dump_file, "## Standard triangular bitmatrix size:   ");
      fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC " bits, ", num_bits);
      fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC " bytes ", num_bytes);
      if (num_bytes > 0)
	fprintf (dump_file, "[%.2f%%]\n",
		 100.0 * ((double) actual_bytes / (double) num_bytes));
      else
	fprintf (dump_file, "[--%%]\n");

      num_bits = (HOST_WIDE_INT) max_allocno * (HOST_WIDE_INT) max_allocno;
      num_bytes = CEIL (num_bits, 8);
      fprintf (dump_file, "## Square bitmatrix size:                ");
      fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC " bits, ", num_bits);
      fprintf (dump_file, HOST_WIDE_INT_PRINT_DEC " bytes ", num_bytes);
      if (num_bytes > 0)
	fprintf (dump_file, "[%.2f%%]\n",
		 100.0 * ((double) actual_bytes / (double) num_bytes));
      else
	fprintf (dump_file, "[--%%]\n");
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

  conflicts = NULL;
  adjacency = NULL;
  adjacency_pool = NULL;

  /* If there is work to be done (at least one reg to allocate),
     perform global conflict analysis and allocate the regs.  */

  if (max_allocno > 0)
    {
      /* We used to use alloca here, but the size of what it would try to
	 allocate would occasionally cause it to exceed the stack limit and
	 cause unpredictable core dumps.  Some examples were > 2Mb in size.  */
      conflicts = XCNEWVEC (HOST_WIDEST_FAST_INT,
			    CEIL(max_bitnum, HOST_BITS_PER_WIDEST_FAST_INT));

      adjacency = XCNEWVEC (adjacency_t *, max_allocno);
      adjacency_pool = create_alloc_pool ("global_alloc adjacency list pool",
					  sizeof (adjacency_t), 1024);

      /* Scan all the insns and compute the conflicts among allocnos
	 and between allocnos and hard regs.  */

      global_conflicts ();

      /* There is just too much going on in the register allocators to
	 keep things up to date.  At the end we have to rescan anyway
	 because things change when the reload_completed flag is set.  
	 So we just turn off scanning and we will rescan by hand.  

	 However, we needed to do the rescanning before this point to
	 get the new insns scanned inserted by local_alloc scanned for
	 global_conflicts.  */
      df_set_flags (DF_NO_INSN_RESCAN);

      /* Eliminate conflicts between pseudos and eliminable registers.  If
	 the register is not eliminated, the pseudo won't really be able to
	 live in the eliminable register, so the conflict doesn't matter.
	 If we do eliminate the register, the conflict will no longer exist.
	 So in either case, we can ignore the conflict.  Likewise for
	 preferences.  */

      set_preferences ();

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
      free (conflicts);
    }

  /* Do the reloads now while the allocno data still exists, so that we can
     try to assign new hard regs to any pseudo regs that are spilled.  */

#if 0 /* We need to eliminate regs even if there is no rtl code,
	 for the sake of debugging information.  */
  if (n_basic_blocks > NUM_FIXED_BLOCKS)
#endif
    {
      build_insn_chain ();
      retval = reload (get_insns (), 1);
    }

  /* Clean up.  */
  free (reg_allocno);
  free (num_allocnos_per_blk);
  free (partial_bitnum);
  free (allocno);
  if (adjacency != NULL)
    {
      free_alloc_pool (adjacency_pool);
      free (adjacency);
    }

  return retval;
}

/* Sort predicate for ordering the regnos.  We want the regno to allocno
   mapping to have the property that all "global" regnos (ie, regnos that
   are referenced in more than one basic block) have smaller allocno values
   than "local" regnos (ie, regnos referenced in only one basic block).
   In addition, for two basic blocks "i" and "j" with i < j, all regnos
   local to basic block i should have smaller allocno values than regnos
   local to basic block j.
   Returns -1 (1) if *v1p should be allocated before (after) *v2p.  */

static int
regno_compare (const void *v1p, const void *v2p)
{
  int regno1 = *(const int *)v1p;
  int regno2 = *(const int *)v2p;
  int blk1 = REG_BASIC_BLOCK (regno1);
  int blk2 = REG_BASIC_BLOCK (regno2);

  /* Prefer lower numbered basic blocks.  Note that global and unknown
     blocks have negative values, giving them high precedence.  */
  if (blk1 - blk2)
    return blk1 - blk2;

  /* If both regs are referenced from the same block, sort by regno.  */
  return regno1 - regno2;
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
	    && ! conflict_p (reg_allocno[REGNO (SET_DEST (set))],
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

/* Helper function for set_preferences.  */
static void
set_preferences_1 (rtx reg, const_rtx setter, void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return;

  gcc_assert (setter);
  if (GET_CODE (setter) != CLOBBER)
    set_preference (reg, SET_SRC (setter));
}

/* Scan all of the insns and initialize the preferences.  */

static void 
set_preferences (void)
{
  basic_block bb;
  rtx insn;
  FOR_EACH_BB (bb)
    FOR_BB_INSNS_REVERSE (bb, insn)
      {
	if (!INSN_P (insn))
	  continue;	

	note_stores (PATTERN (insn), set_preferences_1, NULL);
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
      adjacency_iter ai;

      num = allocno_order[i];

      CLEAR_HARD_REG_SET (temp);
      CLEAR_HARD_REG_SET (temp2);

      FOR_EACH_CONFLICT (num, allocno2, ai)
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
	}

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

#ifdef EH_RETURN_DATA_REGNO
  if (allocno[num].no_eh_reg)
    {
      unsigned int j;
      for (j = 0; ; ++j)
	{
	  unsigned int regno = EH_RETURN_DATA_REGNO (j);
	  if (regno == INVALID_REGNUM)
	    break;
	  SET_HARD_REG_BIT (used1, regno);
	}
    }
#endif

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
	  && CALLER_SAVE_PROFITABLE (optimize_size ? allocno[num].n_refs : allocno[num].freq,
				     optimize_size ? allocno[num].calls_crossed
				     : allocno[num].freq_calls_crossed))
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
      adjacency_iter ai;

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
      FOR_EACH_CONFLICT (num, j, ai)
	{
	  IOR_HARD_REG_SET (allocno[j].hard_reg_conflicts, this_reg);
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
      regset r = DF_LIVE_IN (bb);
      if (REGNO_REG_SET_P (r, from))
	{
	  CLEAR_REGNO_REG_SET (r, from);
	  SET_REGNO_REG_SET (r, to);
	}
    }
}

/* Print chain C to FILE.  */

static void
print_insn_chain (FILE *file, struct insn_chain *c)
{
  fprintf (file, "insn=%d, ", INSN_UID(c->insn));
  bitmap_print (file, &c->live_throughout, "live_throughout: ", ", ");
  bitmap_print (file, &c->dead_or_set, "dead_or_set: ", "\n");
}


/* Print all reload_insn_chains to FILE.  */

static void
print_insn_chains (FILE *file)
{
  struct insn_chain *c;
  for (c = reload_insn_chain; c ; c = c->next)
    print_insn_chain (file, c);
}


/* Walk the insns of the current function and build reload_insn_chain,
   and record register life information.  */

static void
build_insn_chain (void)
{
  unsigned int i;
  struct insn_chain **p = &reload_insn_chain;
  basic_block bb;
  struct insn_chain *c = NULL;
  struct insn_chain *next = NULL;
  bitmap live_relevant_regs = BITMAP_ALLOC (NULL);
  bitmap elim_regset = BITMAP_ALLOC (NULL);
  /* live_subregs is a vector used to keep accurate information about
     which hardregs are live in multiword pseudos.  live_subregs and
     live_subregs_used are indexed by pseudo number.  The live_subreg
     entry for a particular pseudo is only used if the corresponding
     element is non zero in live_subregs_used.  The value in
     live_subregs_used is number of bytes that the pseudo can
     occupy.  */
  sbitmap *live_subregs = XCNEWVEC (sbitmap, max_regno);
  int *live_subregs_used = XNEWVEC (int, max_regno);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (eliminable_regset, i))
      bitmap_set_bit (elim_regset, i);

  FOR_EACH_BB_REVERSE (bb)
    {
      bitmap_iterator bi;
      rtx insn;
      
      CLEAR_REG_SET (live_relevant_regs);
      memset (live_subregs_used, 0, max_regno * sizeof (int));
      
      EXECUTE_IF_SET_IN_BITMAP (df_get_live_out (bb), 0, i, bi)
	{
	  if (i >= FIRST_PSEUDO_REGISTER)
	    break;
	  bitmap_set_bit (live_relevant_regs, i);
	}

      EXECUTE_IF_SET_IN_BITMAP (df_get_live_out (bb), FIRST_PSEUDO_REGISTER, i, bi)
	{
	  if (reg_renumber[i] >= 0)
	    bitmap_set_bit (live_relevant_regs, i);
	}

      FOR_BB_INSNS_REVERSE (bb, insn)
	{
	  if (!NOTE_P (insn) && !BARRIER_P (insn))
	    {
	      unsigned int uid = INSN_UID (insn);
	      struct df_ref **def_rec;
	      struct df_ref **use_rec;

	      c = new_insn_chain ();
	      c->next = next;
	      next = c;
	      *p = c;
	      p = &c->prev;
	      
	      c->insn = insn;
	      c->block = bb->index;

	      if (INSN_P (insn))
		for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
		  {
		    struct df_ref *def = *def_rec;
		    unsigned int regno = DF_REF_REGNO (def);
		    
		    /* Ignore may clobbers because these are generated
		       from calls. However, every other kind of def is
		       added to dead_or_set.  */
		    if (!DF_REF_FLAGS_IS_SET (def, DF_REF_MAY_CLOBBER))
		      {
			if (regno < FIRST_PSEUDO_REGISTER)
			  {
			    if (!fixed_regs[regno])
			      bitmap_set_bit (&c->dead_or_set, regno);
			  }
			else if (reg_renumber[regno] >= 0)
			  bitmap_set_bit (&c->dead_or_set, regno);
		      }

		    if ((regno < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)
			&& (!DF_REF_FLAGS_IS_SET (def, DF_REF_CONDITIONAL)))
		      {
			rtx reg = DF_REF_REG (def);

			/* We can model subregs, but not if they are
			   wrapped in ZERO_EXTRACTS.  */
			if (GET_CODE (reg) == SUBREG
			    && !DF_REF_FLAGS_IS_SET (def, DF_REF_EXTRACT))
			  {
			    unsigned int start = SUBREG_BYTE (reg);
			    unsigned int last = start 
			      + GET_MODE_SIZE (GET_MODE (reg));

			    ra_init_live_subregs (bitmap_bit_p (live_relevant_regs, 
								regno), 
						  live_subregs, 
						  live_subregs_used,
						  regno, reg);

			    if (!DF_REF_FLAGS_IS_SET
				(def, DF_REF_STRICT_LOWER_PART))
			      {
				/* Expand the range to cover entire words.
				   Bytes added here are "don't care".  */
				start = start / UNITS_PER_WORD * UNITS_PER_WORD;
				last = ((last + UNITS_PER_WORD - 1)
					/ UNITS_PER_WORD * UNITS_PER_WORD);
			      }

			    /* Ignore the paradoxical bits.  */
			    if ((int)last > live_subregs_used[regno])
			      last = live_subregs_used[regno];

			    while (start < last)
			      {
				RESET_BIT (live_subregs[regno], start);
				start++;
			      }
			    
			    if (sbitmap_empty_p (live_subregs[regno]))
			      {
				live_subregs_used[regno] = 0;
				bitmap_clear_bit (live_relevant_regs, regno);
			      }
			    else
			      /* Set live_relevant_regs here because
				 that bit has to be true to get us to
				 look at the live_subregs fields.  */
			      bitmap_set_bit (live_relevant_regs, regno);
			  }
			else
			  {
			    /* DF_REF_PARTIAL is generated for
			       subregs, STRICT_LOW_PART, and
			       ZERO_EXTRACT.  We handle the subreg
			       case above so here we have to keep from
			       modeling the def as a killing def.  */
			    if (!DF_REF_FLAGS_IS_SET (def, DF_REF_PARTIAL))
			      {
				bitmap_clear_bit (live_relevant_regs, regno);
				live_subregs_used[regno] = 0;
			      }
			  }
		      }
		  }
	  
	      bitmap_and_compl_into (live_relevant_regs, elim_regset);
	      bitmap_copy (&c->live_throughout, live_relevant_regs);

	      if (INSN_P (insn))
		for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
		  {
		    struct df_ref *use = *use_rec;
		    unsigned int regno = DF_REF_REGNO (use);
		    rtx reg = DF_REF_REG (use);
		    
		    /* DF_REF_READ_WRITE on a use means that this use
		       is fabricated from a def that is a partial set
		       to a multiword reg.  Here, we only model the
		       subreg case that is not wrapped in ZERO_EXTRACT
		       precisely so we do not need to look at the
		       fabricated use. */
		    if (DF_REF_FLAGS_IS_SET (use, DF_REF_READ_WRITE) 
			&& !DF_REF_FLAGS_IS_SET (use, DF_REF_EXTRACT) 
			&& DF_REF_FLAGS_IS_SET (use, DF_REF_SUBREG))
		      continue;
		    
		    /* Add the last use of each var to dead_or_set.  */
		    if (!bitmap_bit_p (live_relevant_regs, regno))
		      {
			if (regno < FIRST_PSEUDO_REGISTER)
			  {
			    if (!fixed_regs[regno])
			      bitmap_set_bit (&c->dead_or_set, regno);
			  }
			else if (reg_renumber[regno] >= 0)
			  bitmap_set_bit (&c->dead_or_set, regno);
		      }
		    
		    if (regno < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)
		      {
			if (GET_CODE (reg) == SUBREG
			    && !DF_REF_FLAGS_IS_SET (use, DF_REF_EXTRACT)) 
			  {
			    unsigned int start = SUBREG_BYTE (reg);
			    unsigned int last = start 
			      + GET_MODE_SIZE (GET_MODE (reg));
			    
			    ra_init_live_subregs (bitmap_bit_p (live_relevant_regs, 
								regno), 
						  live_subregs, 
						  live_subregs_used,
						  regno, reg);
			    
			    /* Ignore the paradoxical bits.  */
			    if ((int)last > live_subregs_used[regno])
			      last = live_subregs_used[regno];

			    while (start < last)
			      {
				SET_BIT (live_subregs[regno], start);
				start++;
			      }
			  }
			else
			  /* Resetting the live_subregs_used is
			     effectively saying do not use the subregs
			     because we are reading the whole
			     pseudo.  */
			  live_subregs_used[regno] = 0;
			bitmap_set_bit (live_relevant_regs, regno);
		      }
		  }
	    }
	}

      /* FIXME!! The following code is a disaster.  Reload needs to see the
	 labels and jump tables that are just hanging out in between
	 the basic blocks.  See pr33676.  */
      insn = BB_HEAD (bb);
      
      /* Skip over the barriers and cruft.  */
      while (insn && (BARRIER_P (insn) || NOTE_P (insn) 
		      || BLOCK_FOR_INSN (insn) == bb))
	insn = PREV_INSN (insn);
      
      /* While we add anything except barriers and notes, the focus is
	 to get the labels and jump tables into the
	 reload_insn_chain.  */
      while (insn)
	{
	  if (!NOTE_P (insn) && !BARRIER_P (insn))
	    {
	      if (BLOCK_FOR_INSN (insn))
		break;
	      
	      c = new_insn_chain ();
	      c->next = next;
	      next = c;
	      *p = c;
	      p = &c->prev;
	      
	      /* The block makes no sense here, but it is what the old
		 code did.  */
	      c->block = bb->index;
	      c->insn = insn;
	      bitmap_copy (&c->live_throughout, live_relevant_regs);
	    }	  
	  insn = PREV_INSN (insn);
	}
    }

  for (i = 0; i < (unsigned int) max_regno; i++)
    if (live_subregs[i])
      free (live_subregs[i]);

  reload_insn_chain = c;
  *p = NULL;

  free (live_subregs);
  free (live_subregs_used);
  BITMAP_FREE (live_relevant_regs);
  BITMAP_FREE (elim_regset);

  if (dump_file)
    print_insn_chains (dump_file);
}

/* Print debugging trace information if -dg switch is given,
   showing the information on which the allocation decisions are based.  */

static void
dump_conflicts (FILE *file)
{
  int i;
  int regno;
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
  for (regno = 0; regno < max_regno; regno++)
    if ((i = reg_allocno[regno]) >= 0)
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

  for (regno = 0; regno < max_regno; regno++)
    if ((i = reg_allocno[regno]) >= 0)
      {
	int j;
	adjacency_iter ai;
	fprintf (file, ";; %d conflicts:", allocno[i].reg);
	FOR_EACH_CONFLICT (i, j, ai)
	  {
	    fprintf (file, " %d", allocno[j].reg);
	  }
	for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	  if (TEST_HARD_REG_BIT (allocno[i].hard_reg_conflicts, j)
	      && !fixed_regs[j])
	    fprintf (file, " %d", j);
	fprintf (file, "\n");

	has_preferences = 0;
	for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	  if (TEST_HARD_REG_BIT (allocno[i].hard_reg_preferences, j))
	    has_preferences = 1;

	if (!has_preferences)
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
      /* There is just too much going on in the register allocators to
	 keep things up to date.  At the end we have to rescan anyway
	 because things change when the reload_completed flag is set.  
	 So we just turn off scanning and we will rescan by hand.  */
      df_set_flags (DF_NO_INSN_RESCAN);
      compute_regsets (&eliminable_regset, &no_global_alloc_regs);
      build_insn_chain ();
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
     just rescan everything.  Note that df_rescan_all_insns is not
     going to help here because it does not touch the artificial uses
     and defs.  */
  df_finish_pass (true);
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
  TODO_dump_func | TODO_verify_rtl_sharing
  | TODO_ggc_collect,                   /* todo_flags_finish */
  'g'                                   /* letter */
};

