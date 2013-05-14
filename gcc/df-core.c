/* Allocation for dataflow support routines.
   Copyright (C) 1999-2013 Free Software Foundation, Inc.
   Originally contributed by Michael P. Hayes
             (m.hayes@elec.canterbury.ac.nz, mhayes@redhat.com)
   Major rewrite contributed by Danny Berlin (dberlin@dberlin.org)
             and Kenneth Zadeck (zadeck@naturalbridge.com).

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

/*
OVERVIEW:

The files in this collection (df*.c,df.h) provide a general framework
for solving dataflow problems.  The global dataflow is performed using
a good implementation of iterative dataflow analysis.

The file df-problems.c provides problem instance for the most common
dataflow problems: reaching defs, upward exposed uses, live variables,
uninitialized variables, def-use chains, and use-def chains.  However,
the interface allows other dataflow problems to be defined as well.

Dataflow analysis is available in most of the rtl backend (the parts
between pass_df_initialize and pass_df_finish).  It is quite likely
that these boundaries will be expanded in the future.  The only
requirement is that there be a correct control flow graph.

There are three variations of the live variable problem that are
available whenever dataflow is available.  The LR problem finds the
areas that can reach a use of a variable, the UR problems finds the
areas that can be reached from a definition of a variable.  The LIVE
problem finds the intersection of these two areas.

There are several optional problems.  These can be enabled when they
are needed and disabled when they are not needed.

Dataflow problems are generally solved in three layers.  The bottom
layer is called scanning where a data structure is built for each rtl
insn that describes the set of defs and uses of that insn.  Scanning
is generally kept up to date, i.e. as the insns changes, the scanned
version of that insn changes also.  There are various mechanisms for
making this happen and are described in the INCREMENTAL SCANNING
section.

In the middle layer, basic blocks are scanned to produce transfer
functions which describe the effects of that block on the global
dataflow solution.  The transfer functions are only rebuilt if the
some instruction within the block has changed.

The top layer is the dataflow solution itself.  The dataflow solution
is computed by using an efficient iterative solver and the transfer
functions.  The dataflow solution must be recomputed whenever the
control changes or if one of the transfer function changes.


USAGE:

Here is an example of using the dataflow routines.

      df_[chain,live,note,rd]_add_problem (flags);

      df_set_blocks (blocks);

      df_analyze ();

      df_dump (stderr);

      df_finish_pass (false);

DF_[chain,live,note,rd]_ADD_PROBLEM adds a problem, defined by an
instance to struct df_problem, to the set of problems solved in this
instance of df.  All calls to add a problem for a given instance of df
must occur before the first call to DF_ANALYZE.

Problems can be dependent on other problems.  For instance, solving
def-use or use-def chains is dependent on solving reaching
definitions. As long as these dependencies are listed in the problem
definition, the order of adding the problems is not material.
Otherwise, the problems will be solved in the order of calls to
df_add_problem.  Note that it is not necessary to have a problem.  In
that case, df will just be used to do the scanning.



DF_SET_BLOCKS is an optional call used to define a region of the
function on which the analysis will be performed.  The normal case is
to analyze the entire function and no call to df_set_blocks is made.
DF_SET_BLOCKS only effects the blocks that are effected when computing
the transfer functions and final solution.  The insn level information
is always kept up to date.

When a subset is given, the analysis behaves as if the function only
contains those blocks and any edges that occur directly between the
blocks in the set.  Care should be taken to call df_set_blocks right
before the call to analyze in order to eliminate the possibility that
optimizations that reorder blocks invalidate the bitvector.

DF_ANALYZE causes all of the defined problems to be (re)solved.  When
DF_ANALYZE is completes, the IN and OUT sets for each basic block
contain the computer information.  The DF_*_BB_INFO macros can be used
to access these bitvectors.  All deferred rescannings are down before
the transfer functions are recomputed.

DF_DUMP can then be called to dump the information produce to some
file.  This calls DF_DUMP_START, to print the information that is not
basic block specific, and then calls DF_DUMP_TOP and DF_DUMP_BOTTOM
for each block to print the basic specific information.  These parts
can all be called separately as part of a larger dump function.


DF_FINISH_PASS causes df_remove_problem to be called on all of the
optional problems.  It also causes any insns whose scanning has been
deferred to be rescanned as well as clears all of the changeable flags.
Setting the pass manager TODO_df_finish flag causes this function to
be run.  However, the pass manager will call df_finish_pass AFTER the
pass dumping has been done, so if you want to see the results of the
optional problems in the pass dumps, use the TODO flag rather than
calling the function yourself.

INCREMENTAL SCANNING

There are four ways of doing the incremental scanning:

1) Immediate rescanning - Calls to df_insn_rescan, df_notes_rescan,
   df_bb_delete, df_insn_change_bb have been added to most of
   the low level service functions that maintain the cfg and change
   rtl.  Calling and of these routines many cause some number of insns
   to be rescanned.

   For most modern rtl passes, this is certainly the easiest way to
   manage rescanning the insns.  This technique also has the advantage
   that the scanning information is always correct and can be relied
   upon even after changes have been made to the instructions.  This
   technique is contra indicated in several cases:

   a) If def-use chains OR use-def chains (but not both) are built,
      using this is SIMPLY WRONG.  The problem is that when a ref is
      deleted that is the target of an edge, there is not enough
      information to efficiently find the source of the edge and
      delete the edge.  This leaves a dangling reference that may
      cause problems.

   b) If def-use chains AND use-def chains are built, this may
      produce unexpected results.  The problem is that the incremental
      scanning of an insn does not know how to repair the chains that
      point into an insn when the insn changes.  So the incremental
      scanning just deletes the chains that enter and exit the insn
      being changed.  The dangling reference issue in (a) is not a
      problem here, but if the pass is depending on the chains being
      maintained after insns have been modified, this technique will
      not do the correct thing.

   c) If the pass modifies insns several times, this incremental
      updating may be expensive.

   d) If the pass modifies all of the insns, as does register
      allocation, it is simply better to rescan the entire function.

2) Deferred rescanning - Calls to df_insn_rescan, df_notes_rescan, and
   df_insn_delete do not immediately change the insn but instead make
   a note that the insn needs to be rescanned.  The next call to
   df_analyze, df_finish_pass, or df_process_deferred_rescans will
   cause all of the pending rescans to be processed.

   This is the technique of choice if either 1a, 1b, or 1c are issues
   in the pass.  In the case of 1a or 1b, a call to df_finish_pass
   (either manually or via TODO_df_finish) should be made before the
   next call to df_analyze or df_process_deferred_rescans.

   This mode is also used by a few passes that still rely on note_uses,
   note_stores and for_each_rtx instead of using the DF data.  This
   can be said to fall under case 1c.

   To enable this mode, call df_set_flags (DF_DEFER_INSN_RESCAN).
   (This mode can be cleared by calling df_clear_flags
   (DF_DEFER_INSN_RESCAN) but this does not cause the deferred insns to
   be rescanned.

3) Total rescanning - In this mode the rescanning is disabled.
   Only when insns are deleted is the df information associated with
   it also deleted.  At the end of the pass, a call must be made to
   df_insn_rescan_all.  This method is used by the register allocator
   since it generally changes each insn multiple times (once for each ref)
   and does not need to make use of the updated scanning information.

4) Do it yourself - In this mechanism, the pass updates the insns
   itself using the low level df primitives.  Currently no pass does
   this, but it has the advantage that it is quite efficient given
   that the pass generally has exact knowledge of what it is changing.

DATA STRUCTURES

Scanning produces a `struct df_ref' data structure (ref) is allocated
for every register reference (def or use) and this records the insn
and bb the ref is found within.  The refs are linked together in
chains of uses and defs for each insn and for each register.  Each ref
also has a chain field that links all the use refs for a def or all
the def refs for a use.  This is used to create use-def or def-use
chains.

Different optimizations have different needs.  Ultimately, only
register allocation and schedulers should be using the bitmaps
produced for the live register and uninitialized register problems.
The rest of the backend should be upgraded to using and maintaining
the linked information such as def use or use def chains.


PHILOSOPHY:

While incremental bitmaps are not worthwhile to maintain, incremental
chains may be perfectly reasonable.  The fastest way to build chains
from scratch or after significant modifications is to build reaching
definitions (RD) and build the chains from this.

However, general algorithms for maintaining use-def or def-use chains
are not practical.  The amount of work to recompute the chain any
chain after an arbitrary change is large.  However, with a modest
amount of work it is generally possible to have the application that
uses the chains keep them up to date.  The high level knowledge of
what is really happening is essential to crafting efficient
incremental algorithms.

As for the bit vector problems, there is no interface to give a set of
blocks over with to resolve the iteration.  In general, restarting a
dataflow iteration is difficult and expensive.  Again, the best way to
keep the dataflow information up to data (if this is really what is
needed) it to formulate a problem specific solution.

There are fine grained calls for creating and deleting references from
instructions in df-scan.c.  However, these are not currently connected
to the engine that resolves the dataflow equations.


DATA STRUCTURES:

The basic object is a DF_REF (reference) and this may either be a
DEF (definition) or a USE of a register.

These are linked into a variety of lists; namely reg-def, reg-use,
insn-def, insn-use, def-use, and use-def lists.  For example, the
reg-def lists contain all the locations that define a given register
while the insn-use lists contain all the locations that use a
register.

Note that the reg-def and reg-use chains are generally short for
pseudos and long for the hard registers.

ACCESSING INSNS:

1) The df insn information is kept in an array of DF_INSN_INFO objects.
   The array is indexed by insn uid, and every DF_REF points to the
   DF_INSN_INFO object of the insn that contains the reference.

2) Each insn has three sets of refs, which are linked into one of three
   lists: The insn's defs list (accessed by the DF_INSN_INFO_DEFS,
   DF_INSN_DEFS, or DF_INSN_UID_DEFS macros), the insn's uses list
   (accessed by the DF_INSN_INFO_USES, DF_INSN_USES, or
   DF_INSN_UID_USES macros) or the insn's eq_uses list (accessed by the
   DF_INSN_INFO_EQ_USES, DF_INSN_EQ_USES or DF_INSN_UID_EQ_USES macros).
   The latter list are the list of references in REG_EQUAL or REG_EQUIV
   notes.  These macros produce a ref (or NULL), the rest of the list
   can be obtained by traversal of the NEXT_REF field (accessed by the
   DF_REF_NEXT_REF macro.)  There is no significance to the ordering of
   the uses or refs in an instruction.

3) Each insn has a logical uid field (LUID) which is stored in the
   DF_INSN_INFO object for the insn.  The LUID field is accessed by
   the DF_INSN_INFO_LUID, DF_INSN_LUID, and DF_INSN_UID_LUID macros.
   When properly set, the LUID is an integer that numbers each insn in
   the basic block, in order from the start of the block.
   The numbers are only correct after a call to df_analyze.  They will
   rot after insns are added deleted or moved round.

ACCESSING REFS:

There are 4 ways to obtain access to refs:

1) References are divided into two categories, REAL and ARTIFICIAL.

   REAL refs are associated with instructions.

   ARTIFICIAL refs are associated with basic blocks.  The heads of
   these lists can be accessed by calling df_get_artificial_defs or
   df_get_artificial_uses for the particular basic block.

   Artificial defs and uses occur both at the beginning and ends of blocks.

     For blocks that area at the destination of eh edges, the
     artificial uses and defs occur at the beginning.  The defs relate
     to the registers specified in EH_RETURN_DATA_REGNO and the uses
     relate to the registers specified in ED_USES.  Logically these
     defs and uses should really occur along the eh edge, but there is
     no convenient way to do this.  Artificial edges that occur at the
     beginning of the block have the DF_REF_AT_TOP flag set.

     Artificial uses occur at the end of all blocks.  These arise from
     the hard registers that are always live, such as the stack
     register and are put there to keep the code from forgetting about
     them.

     Artificial defs occur at the end of the entry block.  These arise
     from registers that are live at entry to the function.

2) There are three types of refs: defs, uses and eq_uses.  (Eq_uses are
   uses that appear inside a REG_EQUAL or REG_EQUIV note.)

   All of the eq_uses, uses and defs associated with each pseudo or
   hard register may be linked in a bidirectional chain.  These are
   called reg-use or reg_def chains.  If the changeable flag
   DF_EQ_NOTES is set when the chains are built, the eq_uses will be
   treated like uses.  If it is not set they are ignored.

   The first use, eq_use or def for a register can be obtained using
   the DF_REG_USE_CHAIN, DF_REG_EQ_USE_CHAIN or DF_REG_DEF_CHAIN
   macros.  Subsequent uses for the same regno can be obtained by
   following the next_reg field of the ref.  The number of elements in
   each of the chains can be found by using the DF_REG_USE_COUNT,
   DF_REG_EQ_USE_COUNT or DF_REG_DEF_COUNT macros.

   In previous versions of this code, these chains were ordered.  It
   has not been practical to continue this practice.

3) If def-use or use-def chains are built, these can be traversed to
   get to other refs.  If the flag DF_EQ_NOTES has been set, the chains
   include the eq_uses.  Otherwise these are ignored when building the
   chains.

4) An array of all of the uses (and an array of all of the defs) can
   be built.  These arrays are indexed by the value in the id
   structure.  These arrays are only lazily kept up to date, and that
   process can be expensive.  To have these arrays built, call
   df_reorganize_defs or df_reorganize_uses.  If the flag DF_EQ_NOTES
   has been set the array will contain the eq_uses.  Otherwise these
   are ignored when building the array and assigning the ids.  Note
   that the values in the id field of a ref may change across calls to
   df_analyze or df_reorganize_defs or df_reorganize_uses.

   If the only use of this array is to find all of the refs, it is
   better to traverse all of the registers and then traverse all of
   reg-use or reg-def chains.

NOTES:

Embedded addressing side-effects, such as POST_INC or PRE_INC, generate
both a use and a def.  These are both marked read/write to show that they
are dependent. For example, (set (reg 40) (mem (post_inc (reg 42))))
will generate a use of reg 42 followed by a def of reg 42 (both marked
read/write).  Similarly, (set (reg 40) (mem (pre_dec (reg 41))))
generates a use of reg 41 then a def of reg 41 (both marked read/write),
even though reg 41 is decremented before it is used for the memory
address in this second example.

A set to a REG inside a ZERO_EXTRACT, or a set to a non-paradoxical SUBREG
for which the number of word_mode units covered by the outer mode is
smaller than that covered by the inner mode, invokes a read-modify-write
operation.  We generate both a use and a def and again mark them
read/write.

Paradoxical subreg writes do not leave a trace of the old content, so they
are write-only operations.
*/


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tm_p.h"
#include "insn-config.h"
#include "recog.h"
#include "function.h"
#include "regs.h"
#include "alloc-pool.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "df.h"
#include "tree-pass.h"
#include "params.h"

static void *df_get_bb_info (struct dataflow *, unsigned int);
static void df_set_bb_info (struct dataflow *, unsigned int, void *);
static void df_clear_bb_info (struct dataflow *, unsigned int);
#ifdef DF_DEBUG_CFG
static void df_set_clean_cfg (void);
#endif

/* The obstack on which regsets are allocated.  */
struct bitmap_obstack reg_obstack;

/* An obstack for bitmap not related to specific dataflow problems.
   This obstack should e.g. be used for bitmaps with a short life time
   such as temporary bitmaps.  */

bitmap_obstack df_bitmap_obstack;


/*----------------------------------------------------------------------------
  Functions to create, destroy and manipulate an instance of df.
----------------------------------------------------------------------------*/

struct df_d *df;

/* Add PROBLEM (and any dependent problems) to the DF instance.  */

void
df_add_problem (struct df_problem *problem)
{
  struct dataflow *dflow;
  int i;

  /* First try to add the dependent problem. */
  if (problem->dependent_problem)
    df_add_problem (problem->dependent_problem);

  /* Check to see if this problem has already been defined.  If it
     has, just return that instance, if not, add it to the end of the
     vector.  */
  dflow = df->problems_by_index[problem->id];
  if (dflow)
    return;

  /* Make a new one and add it to the end.  */
  dflow = XCNEW (struct dataflow);
  dflow->problem = problem;
  dflow->computed = false;
  dflow->solutions_dirty = true;
  df->problems_by_index[dflow->problem->id] = dflow;

  /* Keep the defined problems ordered by index.  This solves the
     problem that RI will use the information from UREC if UREC has
     been defined, or from LIVE if LIVE is defined and otherwise LR.
     However for this to work, the computation of RI must be pushed
     after which ever of those problems is defined, but we do not
     require any of those except for LR to have actually been
     defined.  */
  df->num_problems_defined++;
  for (i = df->num_problems_defined - 2; i >= 0; i--)
    {
      if (problem->id < df->problems_in_order[i]->problem->id)
	df->problems_in_order[i+1] = df->problems_in_order[i];
      else
	{
	  df->problems_in_order[i+1] = dflow;
	  return;
	}
    }
  df->problems_in_order[0] = dflow;
}


/* Set the MASK flags in the DFLOW problem.  The old flags are
   returned.  If a flag is not allowed to be changed this will fail if
   checking is enabled.  */
int
df_set_flags (int changeable_flags)
{
  int old_flags = df->changeable_flags;
  df->changeable_flags |= changeable_flags;
  return old_flags;
}


/* Clear the MASK flags in the DFLOW problem.  The old flags are
   returned.  If a flag is not allowed to be changed this will fail if
   checking is enabled.  */
int
df_clear_flags (int changeable_flags)
{
  int old_flags = df->changeable_flags;
  df->changeable_flags &= ~changeable_flags;
  return old_flags;
}


/* Set the blocks that are to be considered for analysis.  If this is
   not called or is called with null, the entire function in
   analyzed.  */

void
df_set_blocks (bitmap blocks)
{
  if (blocks)
    {
      if (dump_file)
	bitmap_print (dump_file, blocks, "setting blocks to analyze ", "\n");
      if (df->blocks_to_analyze)
	{
	  /* This block is called to change the focus from one subset
	     to another.  */
	  int p;
	  bitmap_head diff;
	  bitmap_initialize (&diff, &df_bitmap_obstack);
	  bitmap_and_compl (&diff, df->blocks_to_analyze, blocks);
	  for (p = 0; p < df->num_problems_defined; p++)
	    {
	      struct dataflow *dflow = df->problems_in_order[p];
	      if (dflow->optional_p && dflow->problem->reset_fun)
		dflow->problem->reset_fun (df->blocks_to_analyze);
	      else if (dflow->problem->free_blocks_on_set_blocks)
		{
		  bitmap_iterator bi;
		  unsigned int bb_index;

		  EXECUTE_IF_SET_IN_BITMAP (&diff, 0, bb_index, bi)
		    {
		      basic_block bb = BASIC_BLOCK (bb_index);
		      if (bb)
			{
			  void *bb_info = df_get_bb_info (dflow, bb_index);
			  dflow->problem->free_bb_fun (bb, bb_info);
			  df_clear_bb_info (dflow, bb_index);
			}
		    }
		}
	    }

	   bitmap_clear (&diff);
	}
      else
	{
	  /* This block of code is executed to change the focus from
	     the entire function to a subset.  */
	  bitmap_head blocks_to_reset;
	  bool initialized = false;
	  int p;
	  for (p = 0; p < df->num_problems_defined; p++)
	    {
	      struct dataflow *dflow = df->problems_in_order[p];
	      if (dflow->optional_p && dflow->problem->reset_fun)
		{
		  if (!initialized)
		    {
		      basic_block bb;
		      bitmap_initialize (&blocks_to_reset, &df_bitmap_obstack);
		      FOR_ALL_BB(bb)
			{
			  bitmap_set_bit (&blocks_to_reset, bb->index);
			}
		    }
		  dflow->problem->reset_fun (&blocks_to_reset);
		}
	    }
	  if (initialized)
	    bitmap_clear (&blocks_to_reset);

	  df->blocks_to_analyze = BITMAP_ALLOC (&df_bitmap_obstack);
	}
      bitmap_copy (df->blocks_to_analyze, blocks);
      df->analyze_subset = true;
    }
  else
    {
      /* This block is executed to reset the focus to the entire
	 function.  */
      if (dump_file)
	fprintf (dump_file, "clearing blocks_to_analyze\n");
      if (df->blocks_to_analyze)
	{
	  BITMAP_FREE (df->blocks_to_analyze);
	  df->blocks_to_analyze = NULL;
	}
      df->analyze_subset = false;
    }

  /* Setting the blocks causes the refs to be unorganized since only
     the refs in the blocks are seen.  */
  df_maybe_reorganize_def_refs (DF_REF_ORDER_NO_TABLE);
  df_maybe_reorganize_use_refs (DF_REF_ORDER_NO_TABLE);
  df_mark_solutions_dirty ();
}


/* Delete a DFLOW problem (and any problems that depend on this
   problem).  */

void
df_remove_problem (struct dataflow *dflow)
{
  struct df_problem *problem;
  int i;

  if (!dflow)
    return;

  problem = dflow->problem;
  gcc_assert (problem->remove_problem_fun);

  /* Delete any problems that depended on this problem first.  */
  for (i = 0; i < df->num_problems_defined; i++)
    if (df->problems_in_order[i]->problem->dependent_problem == problem)
      df_remove_problem (df->problems_in_order[i]);

  /* Now remove this problem.  */
  for (i = 0; i < df->num_problems_defined; i++)
    if (df->problems_in_order[i] == dflow)
      {
	int j;
	for (j = i + 1; j < df->num_problems_defined; j++)
	  df->problems_in_order[j-1] = df->problems_in_order[j];
	df->problems_in_order[j-1] = NULL;
	df->num_problems_defined--;
	break;
      }

  (problem->remove_problem_fun) ();
  df->problems_by_index[problem->id] = NULL;
}


/* Remove all of the problems that are not permanent.  Scanning, LR
   and (at -O2 or higher) LIVE are permanent, the rest are removable.
   Also clear all of the changeable_flags.  */

void
df_finish_pass (bool verify ATTRIBUTE_UNUSED)
{
  int i;
  int removed = 0;

#ifdef ENABLE_DF_CHECKING
  int saved_flags;
#endif

  if (!df)
    return;

  df_maybe_reorganize_def_refs (DF_REF_ORDER_NO_TABLE);
  df_maybe_reorganize_use_refs (DF_REF_ORDER_NO_TABLE);

#ifdef ENABLE_DF_CHECKING
  saved_flags = df->changeable_flags;
#endif

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      struct df_problem *problem = dflow->problem;

      if (dflow->optional_p)
	{
	  gcc_assert (problem->remove_problem_fun);
	  (problem->remove_problem_fun) ();
	  df->problems_in_order[i] = NULL;
	  df->problems_by_index[problem->id] = NULL;
	  removed++;
	}
    }
  df->num_problems_defined -= removed;

  /* Clear all of the flags.  */
  df->changeable_flags = 0;
  df_process_deferred_rescans ();

  /* Set the focus back to the whole function.  */
  if (df->blocks_to_analyze)
    {
      BITMAP_FREE (df->blocks_to_analyze);
      df->blocks_to_analyze = NULL;
      df_mark_solutions_dirty ();
      df->analyze_subset = false;
    }

#ifdef ENABLE_DF_CHECKING
  /* Verification will fail in DF_NO_INSN_RESCAN.  */
  if (!(saved_flags & DF_NO_INSN_RESCAN))
    {
      df_lr_verify_transfer_functions ();
      if (df_live)
	df_live_verify_transfer_functions ();
    }

#ifdef DF_DEBUG_CFG
  df_set_clean_cfg ();
#endif
#endif

#ifdef ENABLE_CHECKING
  if (verify)
    df->changeable_flags |= DF_VERIFY_SCHEDULED;
#endif
}


/* Set up the dataflow instance for the entire back end.  */

static unsigned int
rest_of_handle_df_initialize (void)
{
  gcc_assert (!df);
  df = XCNEW (struct df_d);
  df->changeable_flags = 0;

  bitmap_obstack_initialize (&df_bitmap_obstack);

  /* Set this to a conservative value.  Stack_ptr_mod will compute it
     correctly later.  */
  crtl->sp_is_unchanging = 0;

  df_scan_add_problem ();
  df_scan_alloc (NULL);

  /* These three problems are permanent.  */
  df_lr_add_problem ();
  if (optimize > 1)
    df_live_add_problem ();

  df->postorder = XNEWVEC (int, last_basic_block);
  df->postorder_inverted = XNEWVEC (int, last_basic_block);
  df->n_blocks = post_order_compute (df->postorder, true, true);
  df->n_blocks_inverted = inverted_post_order_compute (df->postorder_inverted);
  gcc_assert (df->n_blocks == df->n_blocks_inverted);

  df->hard_regs_live_count = XCNEWVEC (unsigned int, FIRST_PSEUDO_REGISTER);

  df_hard_reg_init ();
  /* After reload, some ports add certain bits to regs_ever_live so
     this cannot be reset.  */
  df_compute_regs_ever_live (true);
  df_scan_blocks ();
  df_compute_regs_ever_live (false);
  return 0;
}


static bool
gate_opt (void)
{
  return optimize > 0;
}


struct rtl_opt_pass pass_df_initialize_opt =
{
 {
  RTL_PASS,
  "dfinit",                             /* name */
  OPTGROUP_NONE,                        /* optinfo_flags */
  gate_opt,                             /* gate */
  rest_of_handle_df_initialize,         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DF_SCAN,                           /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};


static bool
gate_no_opt (void)
{
  return optimize == 0;
}


struct rtl_opt_pass pass_df_initialize_no_opt =
{
 {
  RTL_PASS,
  "no-opt dfinit",                      /* name */
  OPTGROUP_NONE,                        /* optinfo_flags */
  gate_no_opt,                          /* gate */
  rest_of_handle_df_initialize,         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_DF_SCAN,                           /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};


/* Free all the dataflow info and the DF structure.  This should be
   called from the df_finish macro which also NULLs the parm.  */

static unsigned int
rest_of_handle_df_finish (void)
{
  int i;

  gcc_assert (df);

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      dflow->problem->free_fun ();
    }

  free (df->postorder);
  free (df->postorder_inverted);
  free (df->hard_regs_live_count);
  free (df);
  df = NULL;

  bitmap_obstack_release (&df_bitmap_obstack);
  return 0;
}


struct rtl_opt_pass pass_df_finish =
{
 {
  RTL_PASS,
  "dfinish",                            /* name */
  OPTGROUP_NONE,                        /* optinfo_flags */
  NULL,					/* gate */
  rest_of_handle_df_finish,             /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};





/*----------------------------------------------------------------------------
   The general data flow analysis engine.
----------------------------------------------------------------------------*/

/* Return time BB when it was visited for last time.  */
#define BB_LAST_CHANGE_AGE(bb) ((ptrdiff_t)(bb)->aux)

/* Helper function for df_worklist_dataflow.
   Propagate the dataflow forward.
   Given a BB_INDEX, do the dataflow propagation
   and set bits on for successors in PENDING
   if the out set of the dataflow has changed.

   AGE specify time when BB was visited last time.
   AGE of 0 means we are visiting for first time and need to
   compute transfer function to initialize datastructures.
   Otherwise we re-do transfer function only if something change
   while computing confluence functions.
   We need to compute confluence only of basic block that are younger
   then last visit of the BB.

   Return true if BB info has changed.  This is always the case
   in the first visit.  */

static bool
df_worklist_propagate_forward (struct dataflow *dataflow,
                               unsigned bb_index,
                               unsigned *bbindex_to_postorder,
                               bitmap pending,
                               sbitmap considered,
			       ptrdiff_t age)
{
  edge e;
  edge_iterator ei;
  basic_block bb = BASIC_BLOCK (bb_index);
  bool changed = !age;

  /*  Calculate <conf_op> of incoming edges.  */
  if (EDGE_COUNT (bb->preds) > 0)
    FOR_EACH_EDGE (e, ei, bb->preds)
      {
        if (age <= BB_LAST_CHANGE_AGE (e->src)
	    && bitmap_bit_p (considered, e->src->index))
          changed |= dataflow->problem->con_fun_n (e);
      }
  else if (dataflow->problem->con_fun_0)
    dataflow->problem->con_fun_0 (bb);

  if (changed
      && dataflow->problem->trans_fun (bb_index))
    {
      /* The out set of this block has changed.
         Propagate to the outgoing blocks.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
          unsigned ob_index = e->dest->index;

          if (bitmap_bit_p (considered, ob_index))
            bitmap_set_bit (pending, bbindex_to_postorder[ob_index]);
        }
      return true;
    }
  return false;
}


/* Helper function for df_worklist_dataflow.
   Propagate the dataflow backward.  */

static bool
df_worklist_propagate_backward (struct dataflow *dataflow,
                                unsigned bb_index,
                                unsigned *bbindex_to_postorder,
                                bitmap pending,
                                sbitmap considered,
			        ptrdiff_t age)
{
  edge e;
  edge_iterator ei;
  basic_block bb = BASIC_BLOCK (bb_index);
  bool changed = !age;

  /*  Calculate <conf_op> of incoming edges.  */
  if (EDGE_COUNT (bb->succs) > 0)
    FOR_EACH_EDGE (e, ei, bb->succs)
      {
        if (age <= BB_LAST_CHANGE_AGE (e->dest)
	    && bitmap_bit_p (considered, e->dest->index))
          changed |= dataflow->problem->con_fun_n (e);
      }
  else if (dataflow->problem->con_fun_0)
    dataflow->problem->con_fun_0 (bb);

  if (changed
      && dataflow->problem->trans_fun (bb_index))
    {
      /* The out set of this block has changed.
         Propagate to the outgoing blocks.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
          unsigned ob_index = e->src->index;

          if (bitmap_bit_p (considered, ob_index))
            bitmap_set_bit (pending, bbindex_to_postorder[ob_index]);
        }
      return true;
    }
  return false;
}

/* Main dataflow solver loop.

   DATAFLOW is problem we are solving, PENDING is worklist of basic blocks we
   need to visit.
   BLOCK_IN_POSTORDER is array of size N_BLOCKS specifying postorder in BBs and
   BBINDEX_TO_POSTORDER is array mapping back BB->index to postorder possition.
   PENDING will be freed.

   The worklists are bitmaps indexed by postorder positions.  

   The function implements standard algorithm for dataflow solving with two
   worklists (we are processing WORKLIST and storing new BBs to visit in
   PENDING).

   As an optimization we maintain ages when BB was changed (stored in bb->aux)
   and when it was last visited (stored in last_visit_age).  This avoids need
   to re-do confluence function for edges to basic blocks whose source
   did not change since destination was visited last time.  */

static void
df_worklist_dataflow_doublequeue (struct dataflow *dataflow,
			  	  bitmap pending,
                                  sbitmap considered,
                                  int *blocks_in_postorder,
				  unsigned *bbindex_to_postorder,
				  int n_blocks)
{
  enum df_flow_dir dir = dataflow->problem->dir;
  int dcount = 0;
  bitmap worklist = BITMAP_ALLOC (&df_bitmap_obstack);
  int age = 0;
  bool changed;
  vec<int> last_visit_age = vNULL;
  int prev_age;
  basic_block bb;
  int i;

  last_visit_age.safe_grow_cleared (n_blocks);

  /* Double-queueing. Worklist is for the current iteration,
     and pending is for the next. */
  while (!bitmap_empty_p (pending))
    {
      bitmap_iterator bi;
      unsigned int index;

      /* Swap pending and worklist. */
      bitmap temp = worklist;
      worklist = pending;
      pending = temp;

      EXECUTE_IF_SET_IN_BITMAP (worklist, 0, index, bi)
	{
	  unsigned bb_index;
	  dcount++;

	  bitmap_clear_bit (pending, index);
	  bb_index = blocks_in_postorder[index];
	  bb = BASIC_BLOCK (bb_index);
	  prev_age = last_visit_age[index];
	  if (dir == DF_FORWARD)
	    changed = df_worklist_propagate_forward (dataflow, bb_index,
						     bbindex_to_postorder,
						     pending, considered,
						     prev_age);
	  else
	    changed = df_worklist_propagate_backward (dataflow, bb_index,
						      bbindex_to_postorder,
						      pending, considered,
						      prev_age);
	  last_visit_age[index] = ++age;
	  if (changed)
	    bb->aux = (void *)(ptrdiff_t)age;
	}
      bitmap_clear (worklist);
    }
  for (i = 0; i < n_blocks; i++)
    BASIC_BLOCK (blocks_in_postorder[i])->aux = NULL;

  BITMAP_FREE (worklist);
  BITMAP_FREE (pending);
  last_visit_age.release ();

  /* Dump statistics. */
  if (dump_file)
    fprintf (dump_file, "df_worklist_dataflow_doublequeue:"
	     "n_basic_blocks %d n_edges %d"
	     " count %d (%5.2g)\n",
	     n_basic_blocks, n_edges,
	     dcount, dcount / (float)n_basic_blocks);
}

/* Worklist-based dataflow solver. It uses sbitmap as a worklist,
   with "n"-th bit representing the n-th block in the reverse-postorder order.
   The solver is a double-queue algorithm similar to the "double stack" solver
   from Cooper, Harvey and Kennedy, "Iterative data-flow analysis, Revisited".
   The only significant difference is that the worklist in this implementation
   is always sorted in RPO of the CFG visiting direction.  */

void
df_worklist_dataflow (struct dataflow *dataflow,
                      bitmap blocks_to_consider,
                      int *blocks_in_postorder,
                      int n_blocks)
{
  bitmap pending = BITMAP_ALLOC (&df_bitmap_obstack);
  sbitmap considered = sbitmap_alloc (last_basic_block);
  bitmap_iterator bi;
  unsigned int *bbindex_to_postorder;
  int i;
  unsigned int index;
  enum df_flow_dir dir = dataflow->problem->dir;

  gcc_assert (dir != DF_NONE);

  /* BBINDEX_TO_POSTORDER maps the bb->index to the reverse postorder.  */
  bbindex_to_postorder = XNEWVEC (unsigned int, last_basic_block);

  /* Initialize the array to an out-of-bound value.  */
  for (i = 0; i < last_basic_block; i++)
    bbindex_to_postorder[i] = last_basic_block;

  /* Initialize the considered map.  */
  bitmap_clear (considered);
  EXECUTE_IF_SET_IN_BITMAP (blocks_to_consider, 0, index, bi)
    {
      bitmap_set_bit (considered, index);
    }

  /* Initialize the mapping of block index to postorder.  */
  for (i = 0; i < n_blocks; i++)
    {
      bbindex_to_postorder[blocks_in_postorder[i]] = i;
      /* Add all blocks to the worklist.  */
      bitmap_set_bit (pending, i);
    }

  /* Initialize the problem. */
  if (dataflow->problem->init_fun)
    dataflow->problem->init_fun (blocks_to_consider);

  /* Solve it.  */
  df_worklist_dataflow_doublequeue (dataflow, pending, considered,
				    blocks_in_postorder,
				    bbindex_to_postorder,
				    n_blocks);
  sbitmap_free (considered);
  free (bbindex_to_postorder);
}


/* Remove the entries not in BLOCKS from the LIST of length LEN, preserving
   the order of the remaining entries.  Returns the length of the resulting
   list.  */

static unsigned
df_prune_to_subcfg (int list[], unsigned len, bitmap blocks)
{
  unsigned act, last;

  for (act = 0, last = 0; act < len; act++)
    if (bitmap_bit_p (blocks, list[act]))
      list[last++] = list[act];

  return last;
}


/* Execute dataflow analysis on a single dataflow problem.

   BLOCKS_TO_CONSIDER are the blocks whose solution can either be
   examined or will be computed.  For calls from DF_ANALYZE, this is
   the set of blocks that has been passed to DF_SET_BLOCKS.
*/

void
df_analyze_problem (struct dataflow *dflow,
		    bitmap blocks_to_consider,
		    int *postorder, int n_blocks)
{
  timevar_push (dflow->problem->tv_id);

  /* (Re)Allocate the datastructures necessary to solve the problem.  */
  if (dflow->problem->alloc_fun)
    dflow->problem->alloc_fun (blocks_to_consider);

#ifdef ENABLE_DF_CHECKING
  if (dflow->problem->verify_start_fun)
    dflow->problem->verify_start_fun ();
#endif

  /* Set up the problem and compute the local information.  */
  if (dflow->problem->local_compute_fun)
    dflow->problem->local_compute_fun (blocks_to_consider);

  /* Solve the equations.  */
  if (dflow->problem->dataflow_fun)
    dflow->problem->dataflow_fun (dflow, blocks_to_consider,
				  postorder, n_blocks);

  /* Massage the solution.  */
  if (dflow->problem->finalize_fun)
    dflow->problem->finalize_fun (blocks_to_consider);

#ifdef ENABLE_DF_CHECKING
  if (dflow->problem->verify_end_fun)
    dflow->problem->verify_end_fun ();
#endif

  timevar_pop (dflow->problem->tv_id);

  dflow->computed = true;
}


/* Analyze dataflow info for the basic blocks specified by the bitmap
   BLOCKS, or for the whole CFG if BLOCKS is zero.  */

void
df_analyze (void)
{
  bitmap current_all_blocks = BITMAP_ALLOC (&df_bitmap_obstack);
  bool everything;
  int i;

  free (df->postorder);
  free (df->postorder_inverted);
  df->postorder = XNEWVEC (int, last_basic_block);
  df->postorder_inverted = XNEWVEC (int, last_basic_block);
  df->n_blocks = post_order_compute (df->postorder, true, true);
  df->n_blocks_inverted = inverted_post_order_compute (df->postorder_inverted);

  /* These should be the same.  */
  gcc_assert (df->n_blocks == df->n_blocks_inverted);

  /* We need to do this before the df_verify_all because this is
     not kept incrementally up to date.  */
  df_compute_regs_ever_live (false);
  df_process_deferred_rescans ();

  if (dump_file)
    fprintf (dump_file, "df_analyze called\n");

#ifndef ENABLE_DF_CHECKING
  if (df->changeable_flags & DF_VERIFY_SCHEDULED)
#endif
    df_verify ();

  for (i = 0; i < df->n_blocks; i++)
    bitmap_set_bit (current_all_blocks, df->postorder[i]);

#ifdef ENABLE_CHECKING
  /* Verify that POSTORDER_INVERTED only contains blocks reachable from
     the ENTRY block.  */
  for (i = 0; i < df->n_blocks_inverted; i++)
    gcc_assert (bitmap_bit_p (current_all_blocks, df->postorder_inverted[i]));
#endif

  /* Make sure that we have pruned any unreachable blocks from these
     sets.  */
  if (df->analyze_subset)
    {
      everything = false;
      bitmap_and_into (df->blocks_to_analyze, current_all_blocks);
      df->n_blocks = df_prune_to_subcfg (df->postorder,
					 df->n_blocks, df->blocks_to_analyze);
      df->n_blocks_inverted = df_prune_to_subcfg (df->postorder_inverted,
			                          df->n_blocks_inverted,
                                                  df->blocks_to_analyze);
      BITMAP_FREE (current_all_blocks);
    }
  else
    {
      everything = true;
      df->blocks_to_analyze = current_all_blocks;
      current_all_blocks = NULL;
    }

  /* Skip over the DF_SCAN problem. */
  for (i = 1; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->solutions_dirty)
        {
          if (dflow->problem->dir == DF_FORWARD)
            df_analyze_problem (dflow,
                                df->blocks_to_analyze,
                                df->postorder_inverted,
                                df->n_blocks_inverted);
          else
            df_analyze_problem (dflow,
                                df->blocks_to_analyze,
                                df->postorder,
                                df->n_blocks);
        }
    }

  if (everything)
    {
      BITMAP_FREE (df->blocks_to_analyze);
      df->blocks_to_analyze = NULL;
    }

#ifdef DF_DEBUG_CFG
  df_set_clean_cfg ();
#endif
}


/* Return the number of basic blocks from the last call to df_analyze.  */

int
df_get_n_blocks (enum df_flow_dir dir)
{
  gcc_assert (dir != DF_NONE);

  if (dir == DF_FORWARD)
    {
      gcc_assert (df->postorder_inverted);
      return df->n_blocks_inverted;
    }

  gcc_assert (df->postorder);
  return df->n_blocks;
}


/* Return a pointer to the array of basic blocks in the reverse postorder.
   Depending on the direction of the dataflow problem,
   it returns either the usual reverse postorder array
   or the reverse postorder of inverted traversal. */
int *
df_get_postorder (enum df_flow_dir dir)
{
  gcc_assert (dir != DF_NONE);

  if (dir == DF_FORWARD)
    {
      gcc_assert (df->postorder_inverted);
      return df->postorder_inverted;
    }
  gcc_assert (df->postorder);
  return df->postorder;
}

static struct df_problem user_problem;
static struct dataflow user_dflow;

/* Interface for calling iterative dataflow with user defined
   confluence and transfer functions.  All that is necessary is to
   supply DIR, a direction, CONF_FUN_0, a confluence function for
   blocks with no logical preds (or NULL), CONF_FUN_N, the normal
   confluence function, TRANS_FUN, the basic block transfer function,
   and BLOCKS, the set of blocks to examine, POSTORDER the blocks in
   postorder, and N_BLOCKS, the number of blocks in POSTORDER. */

void
df_simple_dataflow (enum df_flow_dir dir,
		    df_init_function init_fun,
		    df_confluence_function_0 con_fun_0,
		    df_confluence_function_n con_fun_n,
		    df_transfer_function trans_fun,
		    bitmap blocks, int * postorder, int n_blocks)
{
  memset (&user_problem, 0, sizeof (struct df_problem));
  user_problem.dir = dir;
  user_problem.init_fun = init_fun;
  user_problem.con_fun_0 = con_fun_0;
  user_problem.con_fun_n = con_fun_n;
  user_problem.trans_fun = trans_fun;
  user_dflow.problem = &user_problem;
  df_worklist_dataflow (&user_dflow, blocks, postorder, n_blocks);
}



/*----------------------------------------------------------------------------
   Functions to support limited incremental change.
----------------------------------------------------------------------------*/


/* Get basic block info.  */

static void *
df_get_bb_info (struct dataflow *dflow, unsigned int index)
{
  if (dflow->block_info == NULL)
    return NULL;
  if (index >= dflow->block_info_size)
    return NULL;
  return (void *)((char *)dflow->block_info
		  + index * dflow->problem->block_info_elt_size);
}


/* Set basic block info.  */

static void
df_set_bb_info (struct dataflow *dflow, unsigned int index,
		void *bb_info)
{
  gcc_assert (dflow->block_info);
  memcpy ((char *)dflow->block_info
	  + index * dflow->problem->block_info_elt_size,
	  bb_info, dflow->problem->block_info_elt_size);
}


/* Clear basic block info.  */

static void
df_clear_bb_info (struct dataflow *dflow, unsigned int index)
{
  gcc_assert (dflow->block_info);
  gcc_assert (dflow->block_info_size > index);
  memset ((char *)dflow->block_info
	  + index * dflow->problem->block_info_elt_size,
	  0, dflow->problem->block_info_elt_size);
}


/* Mark the solutions as being out of date.  */

void
df_mark_solutions_dirty (void)
{
  if (df)
    {
      int p;
      for (p = 1; p < df->num_problems_defined; p++)
	df->problems_in_order[p]->solutions_dirty = true;
    }
}


/* Return true if BB needs it's transfer functions recomputed.  */

bool
df_get_bb_dirty (basic_block bb)
{
  return bitmap_bit_p ((df_live
			? df_live : df_lr)->out_of_date_transfer_functions,
		       bb->index);
}


/* Mark BB as needing it's transfer functions as being out of
   date.  */

void
df_set_bb_dirty (basic_block bb)
{
  bb->flags |= BB_MODIFIED;
  if (df)
    {
      int p;
      for (p = 1; p < df->num_problems_defined; p++)
	{
	  struct dataflow *dflow = df->problems_in_order[p];
	  if (dflow->out_of_date_transfer_functions)
	    bitmap_set_bit (dflow->out_of_date_transfer_functions, bb->index);
	}
      df_mark_solutions_dirty ();
    }
}


/* Grow the bb_info array.  */

void
df_grow_bb_info (struct dataflow *dflow)
{
  unsigned int new_size = last_basic_block + 1;
  if (dflow->block_info_size < new_size)
    {
      new_size += new_size / 4;
      dflow->block_info
         = (void *)XRESIZEVEC (char, (char *)dflow->block_info,
			       new_size
			       * dflow->problem->block_info_elt_size);
      memset ((char *)dflow->block_info
	      + dflow->block_info_size
	      * dflow->problem->block_info_elt_size,
	      0,
	      (new_size - dflow->block_info_size)
	      * dflow->problem->block_info_elt_size);
      dflow->block_info_size = new_size;
    }
}


/* Clear the dirty bits.  This is called from places that delete
   blocks.  */
static void
df_clear_bb_dirty (basic_block bb)
{
  int p;
  for (p = 1; p < df->num_problems_defined; p++)
    {
      struct dataflow *dflow = df->problems_in_order[p];
      if (dflow->out_of_date_transfer_functions)
	bitmap_clear_bit (dflow->out_of_date_transfer_functions, bb->index);
    }
}

/* Called from the rtl_compact_blocks to reorganize the problems basic
   block info.  */

void
df_compact_blocks (void)
{
  int i, p;
  basic_block bb;
  void *problem_temps;
  bitmap_head tmp;

  bitmap_initialize (&tmp, &df_bitmap_obstack);
  for (p = 0; p < df->num_problems_defined; p++)
    {
      struct dataflow *dflow = df->problems_in_order[p];

      /* Need to reorganize the out_of_date_transfer_functions for the
	 dflow problem.  */
      if (dflow->out_of_date_transfer_functions)
	{
	  bitmap_copy (&tmp, dflow->out_of_date_transfer_functions);
	  bitmap_clear (dflow->out_of_date_transfer_functions);
	  if (bitmap_bit_p (&tmp, ENTRY_BLOCK))
	    bitmap_set_bit (dflow->out_of_date_transfer_functions, ENTRY_BLOCK);
	  if (bitmap_bit_p (&tmp, EXIT_BLOCK))
	    bitmap_set_bit (dflow->out_of_date_transfer_functions, EXIT_BLOCK);

	  i = NUM_FIXED_BLOCKS;
	  FOR_EACH_BB (bb)
	    {
	      if (bitmap_bit_p (&tmp, bb->index))
		bitmap_set_bit (dflow->out_of_date_transfer_functions, i);
	      i++;
	    }
	}

      /* Now shuffle the block info for the problem.  */
      if (dflow->problem->free_bb_fun)
	{
	  int size = last_basic_block * dflow->problem->block_info_elt_size;
	  problem_temps = XNEWVAR (char, size);
	  df_grow_bb_info (dflow);
	  memcpy (problem_temps, dflow->block_info, size);

	  /* Copy the bb info from the problem tmps to the proper
	     place in the block_info vector.  Null out the copied
	     item.  The entry and exit blocks never move.  */
	  i = NUM_FIXED_BLOCKS;
	  FOR_EACH_BB (bb)
	    {
	      df_set_bb_info (dflow, i,
			      (char *)problem_temps
			      + bb->index * dflow->problem->block_info_elt_size);
	      i++;
	    }
	  memset ((char *)dflow->block_info
		  + i * dflow->problem->block_info_elt_size, 0,
		  (last_basic_block - i)
		  * dflow->problem->block_info_elt_size);
	  free (problem_temps);
	}
    }

  /* Shuffle the bits in the basic_block indexed arrays.  */

  if (df->blocks_to_analyze)
    {
      if (bitmap_bit_p (&tmp, ENTRY_BLOCK))
	bitmap_set_bit (df->blocks_to_analyze, ENTRY_BLOCK);
      if (bitmap_bit_p (&tmp, EXIT_BLOCK))
	bitmap_set_bit (df->blocks_to_analyze, EXIT_BLOCK);
      bitmap_copy (&tmp, df->blocks_to_analyze);
      bitmap_clear (df->blocks_to_analyze);
      i = NUM_FIXED_BLOCKS;
      FOR_EACH_BB (bb)
	{
	  if (bitmap_bit_p (&tmp, bb->index))
	    bitmap_set_bit (df->blocks_to_analyze, i);
	  i++;
	}
    }

  bitmap_clear (&tmp);

  i = NUM_FIXED_BLOCKS;
  FOR_EACH_BB (bb)
    {
      SET_BASIC_BLOCK (i, bb);
      bb->index = i;
      i++;
    }

  gcc_assert (i == n_basic_blocks);

  for (; i < last_basic_block; i++)
    SET_BASIC_BLOCK (i, NULL);

#ifdef DF_DEBUG_CFG
  if (!df_lr->solutions_dirty)
    df_set_clean_cfg ();
#endif
}


/* Shove NEW_BLOCK in at OLD_INDEX.  Called from ifcvt to hack a
   block.  There is no excuse for people to do this kind of thing.  */

void
df_bb_replace (int old_index, basic_block new_block)
{
  int new_block_index = new_block->index;
  int p;

  if (dump_file)
    fprintf (dump_file, "shoving block %d into %d\n", new_block_index, old_index);

  gcc_assert (df);
  gcc_assert (BASIC_BLOCK (old_index) == NULL);

  for (p = 0; p < df->num_problems_defined; p++)
    {
      struct dataflow *dflow = df->problems_in_order[p];
      if (dflow->block_info)
	{
	  df_grow_bb_info (dflow);
	  df_set_bb_info (dflow, old_index,
			  df_get_bb_info (dflow, new_block_index));
	}
    }

  df_clear_bb_dirty (new_block);
  SET_BASIC_BLOCK (old_index, new_block);
  new_block->index = old_index;
  df_set_bb_dirty (BASIC_BLOCK (old_index));
  SET_BASIC_BLOCK (new_block_index, NULL);
}


/* Free all of the per basic block dataflow from all of the problems.
   This is typically called before a basic block is deleted and the
   problem will be reanalyzed.  */

void
df_bb_delete (int bb_index)
{
  basic_block bb = BASIC_BLOCK (bb_index);
  int i;

  if (!df)
    return;

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->problem->free_bb_fun)
	{
	  void *bb_info = df_get_bb_info (dflow, bb_index);
	  if (bb_info)
	    {
	      dflow->problem->free_bb_fun (bb, bb_info);
	      df_clear_bb_info (dflow, bb_index);
	    }
	}
    }
  df_clear_bb_dirty (bb);
  df_mark_solutions_dirty ();
}


/* Verify that there is a place for everything and everything is in
   its place.  This is too expensive to run after every pass in the
   mainline.  However this is an excellent debugging tool if the
   dataflow information is not being updated properly.  You can just
   sprinkle calls in until you find the place that is changing an
   underlying structure without calling the proper updating
   routine.  */

void
df_verify (void)
{
  df_scan_verify ();
#ifdef ENABLE_DF_CHECKING
  df_lr_verify_transfer_functions ();
  if (df_live)
    df_live_verify_transfer_functions ();
#endif
}

#ifdef DF_DEBUG_CFG

/* Compute an array of ints that describes the cfg.  This can be used
   to discover places where the cfg is modified by the appropriate
   calls have not been made to the keep df informed.  The internals of
   this are unexciting, the key is that two instances of this can be
   compared to see if any changes have been made to the cfg.  */

static int *
df_compute_cfg_image (void)
{
  basic_block bb;
  int size = 2 + (2 * n_basic_blocks);
  int i;
  int * map;

  FOR_ALL_BB (bb)
    {
      size += EDGE_COUNT (bb->succs);
    }

  map = XNEWVEC (int, size);
  map[0] = size;
  i = 1;
  FOR_ALL_BB (bb)
    {
      edge_iterator ei;
      edge e;

      map[i++] = bb->index;
      FOR_EACH_EDGE (e, ei, bb->succs)
	map[i++] = e->dest->index;
      map[i++] = -1;
    }
  map[i] = -1;
  return map;
}

static int *saved_cfg = NULL;


/* This function compares the saved version of the cfg with the
   current cfg and aborts if the two are identical.  The function
   silently returns if the cfg has been marked as dirty or the two are
   the same.  */

void
df_check_cfg_clean (void)
{
  int *new_map;

  if (!df)
    return;

  if (df_lr->solutions_dirty)
    return;

  if (saved_cfg == NULL)
    return;

  new_map = df_compute_cfg_image ();
  gcc_assert (memcmp (saved_cfg, new_map, saved_cfg[0] * sizeof (int)) == 0);
  free (new_map);
}


/* This function builds a cfg fingerprint and squirrels it away in
   saved_cfg.  */

static void
df_set_clean_cfg (void)
{
  free (saved_cfg);
  saved_cfg = df_compute_cfg_image ();
}

#endif /* DF_DEBUG_CFG  */
/*----------------------------------------------------------------------------
   PUBLIC INTERFACES TO QUERY INFORMATION.
----------------------------------------------------------------------------*/


/* Return first def of REGNO within BB.  */

df_ref
df_bb_regno_first_def_find (basic_block bb, unsigned int regno)
{
  rtx insn;
  df_ref *def_rec;
  unsigned int uid;

  FOR_BB_INSNS (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      uid = INSN_UID (insn);
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  if (DF_REF_REGNO (def) == regno)
	    return def;
	}
    }
  return NULL;
}


/* Return last def of REGNO within BB.  */

df_ref
df_bb_regno_last_def_find (basic_block bb, unsigned int regno)
{
  rtx insn;
  df_ref *def_rec;
  unsigned int uid;

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      uid = INSN_UID (insn);
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  df_ref def = *def_rec;
	  if (DF_REF_REGNO (def) == regno)
	    return def;
	}
    }

  return NULL;
}

/* Finds the reference corresponding to the definition of REG in INSN.
   DF is the dataflow object.  */

df_ref
df_find_def (rtx insn, rtx reg)
{
  unsigned int uid;
  df_ref *def_rec;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  gcc_assert (REG_P (reg));

  uid = INSN_UID (insn);
  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
    {
      df_ref def = *def_rec;
      if (DF_REF_REGNO (def) == REGNO (reg))
	return def;
    }

  return NULL;
}


/* Return true if REG is defined in INSN, zero otherwise.  */

bool
df_reg_defined (rtx insn, rtx reg)
{
  return df_find_def (insn, reg) != NULL;
}


/* Finds the reference corresponding to the use of REG in INSN.
   DF is the dataflow object.  */

df_ref
df_find_use (rtx insn, rtx reg)
{
  unsigned int uid;
  df_ref *use_rec;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  gcc_assert (REG_P (reg));

  uid = INSN_UID (insn);
  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
    {
      df_ref use = *use_rec;
      if (DF_REF_REGNO (use) == REGNO (reg))
	return use;
    }
  if (df->changeable_flags & DF_EQ_NOTES)
    for (use_rec = DF_INSN_UID_EQ_USES (uid); *use_rec; use_rec++)
      {
	df_ref use = *use_rec;
	if (DF_REF_REGNO (use) == REGNO (reg))
	  return use;
      }
  return NULL;
}


/* Return true if REG is referenced in INSN, zero otherwise.  */

bool
df_reg_used (rtx insn, rtx reg)
{
  return df_find_use (insn, reg) != NULL;
}


/*----------------------------------------------------------------------------
   Debugging and printing functions.
----------------------------------------------------------------------------*/

/* Write information about registers and basic blocks into FILE.
   This is part of making a debugging dump.  */

void
dump_regset (regset r, FILE *outf)
{
  unsigned i;
  reg_set_iterator rsi;

  if (r == NULL)
    {
      fputs (" (nil)", outf);
      return;
    }

  EXECUTE_IF_SET_IN_REG_SET (r, 0, i, rsi)
    {
      fprintf (outf, " %d", i);
      if (i < FIRST_PSEUDO_REGISTER)
	fprintf (outf, " [%s]",
		 reg_names[i]);
    }
}

/* Print a human-readable representation of R on the standard error
   stream.  This function is designed to be used from within the
   debugger.  */
extern void debug_regset (regset);
DEBUG_FUNCTION void
debug_regset (regset r)
{
  dump_regset (r, stderr);
  putc ('\n', stderr);
}

/* Write information about registers and basic blocks into FILE.
   This is part of making a debugging dump.  */

void
df_print_regset (FILE *file, bitmap r)
{
  unsigned int i;
  bitmap_iterator bi;

  if (r == NULL)
    fputs (" (nil)", file);
  else
    {
      EXECUTE_IF_SET_IN_BITMAP (r, 0, i, bi)
	{
	  fprintf (file, " %d", i);
	  if (i < FIRST_PSEUDO_REGISTER)
	    fprintf (file, " [%s]", reg_names[i]);
	}
    }
  fprintf (file, "\n");
}


/* Write information about registers and basic blocks into FILE.  The
   bitmap is in the form used by df_byte_lr.  This is part of making a
   debugging dump.  */

void
df_print_word_regset (FILE *file, bitmap r)
{
  unsigned int max_reg = max_reg_num ();

  if (r == NULL)
    fputs (" (nil)", file);
  else
    {
      unsigned int i;
      for (i = FIRST_PSEUDO_REGISTER; i < max_reg; i++)
	{
	  bool found = (bitmap_bit_p (r, 2 * i)
			|| bitmap_bit_p (r, 2 * i + 1));
	  if (found)
	    {
	      int word;
	      const char * sep = "";
	      fprintf (file, " %d", i);
	      fprintf (file, "(");
	      for (word = 0; word < 2; word++)
		if (bitmap_bit_p (r, 2 * i + word))
		  {
		    fprintf (file, "%s%d", sep, word);
		    sep = ", ";
		  }
	      fprintf (file, ")");
	    }
	}
    }
  fprintf (file, "\n");
}


/* Dump dataflow info.  */

void
df_dump (FILE *file)
{
  basic_block bb;
  df_dump_start (file);

  FOR_ALL_BB (bb)
    {
      df_print_bb_index (bb, file);
      df_dump_top (bb, file);
      df_dump_bottom (bb, file);
    }

  fprintf (file, "\n");
}


/* Dump dataflow info for df->blocks_to_analyze.  */

void
df_dump_region (FILE *file)
{
  if (df->blocks_to_analyze)
    {
      bitmap_iterator bi;
      unsigned int bb_index;

      fprintf (file, "\n\nstarting region dump\n");
      df_dump_start (file);

      EXECUTE_IF_SET_IN_BITMAP (df->blocks_to_analyze, 0, bb_index, bi)
	{
	  basic_block bb = BASIC_BLOCK (bb_index);
	  dump_bb (file, bb, 0, TDF_DETAILS);
	}
      fprintf (file, "\n");
    }
  else
    df_dump (file);
}


/* Dump the introductory information for each problem defined.  */

void
df_dump_start (FILE *file)
{
  int i;

  if (!df || !file)
    return;

  fprintf (file, "\n\n%s\n", current_function_name ());
  fprintf (file, "\nDataflow summary:\n");
  if (df->blocks_to_analyze)
    fprintf (file, "def_info->table_size = %d, use_info->table_size = %d\n",
	     DF_DEFS_TABLE_SIZE (), DF_USES_TABLE_SIZE ());

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->computed)
	{
	  df_dump_problem_function fun = dflow->problem->dump_start_fun;
	  if (fun)
	    fun(file);
	}
    }
}


/* Dump the top or bottom of the block information for BB.  */
static void
df_dump_bb_problem_data (basic_block bb, FILE *file, bool top)
{
  int i;

  if (!df || !file)
    return;

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->computed)
	{
	  df_dump_bb_problem_function bbfun;

	  if (top)
	    bbfun = dflow->problem->dump_top_fun;
	  else
	    bbfun = dflow->problem->dump_bottom_fun;

	  if (bbfun)
	    bbfun (bb, file);
	}
    }
}

/* Dump the top of the block information for BB.  */

void
df_dump_top (basic_block bb, FILE *file)
{
  df_dump_bb_problem_data (bb, file, /*top=*/true);
}

/* Dump the bottom of the block information for BB.  */

void
df_dump_bottom (basic_block bb, FILE *file)
{
  df_dump_bb_problem_data (bb, file, /*top=*/false);
}


/* Dump information about INSN just before or after dumping INSN itself.  */
static void
df_dump_insn_problem_data (const_rtx insn, FILE *file, bool top)
{
  int i;

  if (!df || !file)
    return;

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->computed)
	{
	  df_dump_insn_problem_function insnfun;

	  if (top)
	    insnfun = dflow->problem->dump_insn_top_fun;
	  else
	    insnfun = dflow->problem->dump_insn_bottom_fun;

	  if (insnfun)
	    insnfun (insn, file);
	}
    }
}

/* Dump information about INSN before dumping INSN itself.  */

void
df_dump_insn_top (const_rtx insn, FILE *file)
{
  df_dump_insn_problem_data (insn,  file, /*top=*/true);
}

/* Dump information about INSN after dumping INSN itself.  */

void
df_dump_insn_bottom (const_rtx insn, FILE *file)
{
  df_dump_insn_problem_data (insn,  file, /*top=*/false);
}


static void
df_ref_dump (df_ref ref, FILE *file)
{
  fprintf (file, "%c%d(%d)",
	   DF_REF_REG_DEF_P (ref)
	   ? 'd'
	   : (DF_REF_FLAGS (ref) & DF_REF_IN_NOTE) ? 'e' : 'u',
	   DF_REF_ID (ref),
	   DF_REF_REGNO (ref));
}

void
df_refs_chain_dump (df_ref *ref_rec, bool follow_chain, FILE *file)
{
  fprintf (file, "{ ");
  while (*ref_rec)
    {
      df_ref ref = *ref_rec;
      df_ref_dump (ref, file);
      if (follow_chain)
	df_chain_dump (DF_REF_CHAIN (ref), file);
      ref_rec++;
    }
  fprintf (file, "}");
}


/* Dump either a ref-def or reg-use chain.  */

void
df_regs_chain_dump (df_ref ref,  FILE *file)
{
  fprintf (file, "{ ");
  while (ref)
    {
      df_ref_dump (ref, file);
      ref = DF_REF_NEXT_REG (ref);
    }
  fprintf (file, "}");
}


static void
df_mws_dump (struct df_mw_hardreg **mws, FILE *file)
{
  while (*mws)
    {
      fprintf (file, "mw %c r[%d..%d]\n",
	       (DF_MWS_REG_DEF_P (*mws)) ? 'd' : 'u',
	       (*mws)->start_regno, (*mws)->end_regno);
      mws++;
    }
}


static void
df_insn_uid_debug (unsigned int uid,
		   bool follow_chain, FILE *file)
{
  fprintf (file, "insn %d luid %d",
	   uid, DF_INSN_UID_LUID (uid));

  if (DF_INSN_UID_DEFS (uid))
    {
      fprintf (file, " defs ");
      df_refs_chain_dump (DF_INSN_UID_DEFS (uid), follow_chain, file);
    }

  if (DF_INSN_UID_USES (uid))
    {
      fprintf (file, " uses ");
      df_refs_chain_dump (DF_INSN_UID_USES (uid), follow_chain, file);
    }

  if (DF_INSN_UID_EQ_USES (uid))
    {
      fprintf (file, " eq uses ");
      df_refs_chain_dump (DF_INSN_UID_EQ_USES (uid), follow_chain, file);
    }

  if (DF_INSN_UID_MWS (uid))
    {
      fprintf (file, " mws ");
      df_mws_dump (DF_INSN_UID_MWS (uid), file);
    }
  fprintf (file, "\n");
}


DEBUG_FUNCTION void
df_insn_debug (rtx insn, bool follow_chain, FILE *file)
{
  df_insn_uid_debug (INSN_UID (insn), follow_chain, file);
}

DEBUG_FUNCTION void
df_insn_debug_regno (rtx insn, FILE *file)
{
  struct df_insn_info *insn_info = DF_INSN_INFO_GET (insn);

  fprintf (file, "insn %d bb %d luid %d defs ",
	   INSN_UID (insn), BLOCK_FOR_INSN (insn)->index,
	   DF_INSN_INFO_LUID (insn_info));
  df_refs_chain_dump (DF_INSN_INFO_DEFS (insn_info), false, file);

  fprintf (file, " uses ");
  df_refs_chain_dump (DF_INSN_INFO_USES (insn_info), false, file);

  fprintf (file, " eq_uses ");
  df_refs_chain_dump (DF_INSN_INFO_EQ_USES (insn_info), false, file);
  fprintf (file, "\n");
}

DEBUG_FUNCTION void
df_regno_debug (unsigned int regno, FILE *file)
{
  fprintf (file, "reg %d defs ", regno);
  df_regs_chain_dump (DF_REG_DEF_CHAIN (regno), file);
  fprintf (file, " uses ");
  df_regs_chain_dump (DF_REG_USE_CHAIN (regno), file);
  fprintf (file, " eq_uses ");
  df_regs_chain_dump (DF_REG_EQ_USE_CHAIN (regno), file);
  fprintf (file, "\n");
}


DEBUG_FUNCTION void
df_ref_debug (df_ref ref, FILE *file)
{
  fprintf (file, "%c%d ",
	   DF_REF_REG_DEF_P (ref) ? 'd' : 'u',
	   DF_REF_ID (ref));
  fprintf (file, "reg %d bb %d insn %d flag %#x type %#x ",
	   DF_REF_REGNO (ref),
	   DF_REF_BBNO (ref),
	   DF_REF_IS_ARTIFICIAL (ref) ? -1 : DF_REF_INSN_UID (ref),
	   DF_REF_FLAGS (ref),
	   DF_REF_TYPE (ref));
  if (DF_REF_LOC (ref))
    {
      if (flag_dump_noaddr)
	fprintf (file, "loc #(#) chain ");
      else
	fprintf (file, "loc %p(%p) chain ", (void *)DF_REF_LOC (ref),
		 (void *)*DF_REF_LOC (ref));
    }
  else
    fprintf (file, "chain ");
  df_chain_dump (DF_REF_CHAIN (ref), file);
  fprintf (file, "\n");
}

/* Functions for debugging from GDB.  */

DEBUG_FUNCTION void
debug_df_insn (rtx insn)
{
  df_insn_debug (insn, true, stderr);
  debug_rtx (insn);
}


DEBUG_FUNCTION void
debug_df_reg (rtx reg)
{
  df_regno_debug (REGNO (reg), stderr);
}


DEBUG_FUNCTION void
debug_df_regno (unsigned int regno)
{
  df_regno_debug (regno, stderr);
}


DEBUG_FUNCTION void
debug_df_ref (df_ref ref)
{
  df_ref_debug (ref, stderr);
}


DEBUG_FUNCTION void
debug_df_defno (unsigned int defno)
{
  df_ref_debug (DF_DEFS_GET (defno), stderr);
}


DEBUG_FUNCTION void
debug_df_useno (unsigned int defno)
{
  df_ref_debug (DF_USES_GET (defno), stderr);
}


DEBUG_FUNCTION void
debug_df_chain (struct df_link *link)
{
  df_chain_dump (link, stderr);
  fputc ('\n', stderr);
}
