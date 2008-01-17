/* Allocation for dataflow support routines.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
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
areas tha can be reached from a definition of a variable.  The LIVE
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
functions which describe the effects of that block on the a global
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

   e) If the pass uses either non-standard or ancient techniques to
      modify insns, automatic detection of the insns that need to be
      rescanned may be impractical.  Cse and regrename fall into this
      category.

2) Deferred rescanning - Calls to df_insn_rescan, df_notes_rescan, and
   df_insn_delete do not immediately change the insn but instead make
   a note that the insn needs to be rescanned.  The next call to
   df_analyze, df_finish_pass, or df_process_deferred_rescans will
   cause all of the pending rescans to be processed.

   This is the technique of choice if either 1a, 1b, or 1c are issues
   in the pass.  In the case of 1a or 1b, a call to df_remove_problem
   (df_chain) should be made before the next call to df_analyze or
   df_process_deferred_rescans.

   To enable this mode, call df_set_flags (DF_DEFER_INSN_RESCAN).
   (This mode can be cleared by calling df_clear_flags
   (DF_DEFER_INSN_RESCAN) but this does not cause the deferred insns to
   be rescanned.

   3) Total rescanning - In this mode the rescanning is disabled.
   However, the df information associated with deleted insn is delete
   at the time the insn is deleted.  At the end of the pass, a call
   must be made to df_insn_rescan_all.  This method is used by the
   register allocator since it generally changes each insn multiple
   times (once for each ref) and does not need to make use of the
   updated scanning information.

   It is also currently used by two older passes (cse, and regrename)
   which change insns in hard to track ways.  It is hoped that this
   will be fixed soon since this it is expensive to rescan all of the
   insns when only a small number of them have really changed.

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

1) The df insn information is kept in the insns array.  This array is
   indexed by insn uid.  

2) Each insn has three sets of refs: They are linked into one of three
   lists: the insn's defs list (accessed by the DF_INSN_DEFS or
   DF_INSN_UID_DEFS macros), the insn's uses list (accessed by the
   DF_INSN_USES or DF_INSN_UID_USES macros) or the insn's eq_uses list
   (accessed by the DF_INSN_EQ_USES or DF_INSN_UID_EQ_USES macros).
   The latter list are the list of references in REG_EQUAL or
   REG_EQUIV notes.  These macros produce a ref (or NULL), the rest of
   the list can be obtained by traversal of the NEXT_REF field
   (accessed by the DF_REF_NEXT_REF macro.)  There is no significance
   to the ordering of the uses or refs in an instruction.

3) Each insn has a logical uid field (LUID).  When properly set, this
   is an integer that numbers each insn in the basic block, in order from
   the start of the block.  The numbers are only correct after a call to
   df_analyse.  They will rot after insns are added deleted or moved
   around.

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
smaller than that covered by the inner mode, invokes a read-modify-write.
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
#include "output.h"
#include "alloc-pool.h"
#include "flags.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "timevar.h"
#include "df.h"
#include "tree-pass.h"
#include "params.h"

static void *df_get_bb_info (struct dataflow *, unsigned int);
static void df_set_bb_info (struct dataflow *, unsigned int, void *);
#ifdef DF_DEBUG_CFG
static void df_set_clean_cfg (void);
#endif

/* An obstack for bitmap not related to specific dataflow problems.
   This obstack should e.g. be used for bitmaps with a short life time
   such as temporary bitmaps.  */

bitmap_obstack df_bitmap_obstack;


/*----------------------------------------------------------------------------
  Functions to create, destroy and manipulate an instance of df.
----------------------------------------------------------------------------*/

struct df *df;

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
enum df_changeable_flags
df_set_flags (enum df_changeable_flags changeable_flags)
{
  enum df_changeable_flags old_flags = df->changeable_flags;
  df->changeable_flags |= changeable_flags;
  return old_flags;
}


/* Clear the MASK flags in the DFLOW problem.  The old flags are
   returned.  If a flag is not allowed to be changed this will fail if
   checking is enabled.  */
enum df_changeable_flags
df_clear_flags (enum df_changeable_flags changeable_flags)
{
  enum df_changeable_flags old_flags = df->changeable_flags;
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
	  bitmap diff = BITMAP_ALLOC (&df_bitmap_obstack);
	  bitmap_and_compl (diff, df->blocks_to_analyze, blocks);
	  for (p = 0; p < df->num_problems_defined; p++)
	    {
	      struct dataflow *dflow = df->problems_in_order[p];
	      if (dflow->optional_p && dflow->problem->reset_fun)
		dflow->problem->reset_fun (df->blocks_to_analyze);
	      else if (dflow->problem->free_blocks_on_set_blocks)
		{
		  bitmap_iterator bi;
		  unsigned int bb_index;
		  
		  EXECUTE_IF_SET_IN_BITMAP (diff, 0, bb_index, bi)
		    {
		      basic_block bb = BASIC_BLOCK (bb_index);
		      if (bb)
			{
			  void *bb_info = df_get_bb_info (dflow, bb_index);
			  if (bb_info)
			    {
			      dflow->problem->free_bb_fun (bb, bb_info);
			      df_set_bb_info (dflow, bb_index, NULL);
			    }
			}
		    }
		}
	    }

	  BITMAP_FREE (diff);
	}
      else
	{
	  /* This block of code is executed to change the focus from
	     the entire function to a subset.  */
	  bitmap blocks_to_reset = NULL;
	  int p;
	  for (p = 0; p < df->num_problems_defined; p++)
	    {
	      struct dataflow *dflow = df->problems_in_order[p];
	      if (dflow->optional_p && dflow->problem->reset_fun)
		{
		  if (!blocks_to_reset)
		    {
		      basic_block bb;
		      blocks_to_reset =
			BITMAP_ALLOC (&df_bitmap_obstack);
		      FOR_ALL_BB(bb)
			{
			  bitmap_set_bit (blocks_to_reset, bb->index); 
			}
		    }
		  dflow->problem->reset_fun (blocks_to_reset);
		}
	    }
	  if (blocks_to_reset)
	    BITMAP_FREE (blocks_to_reset);

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
	df->problems_in_order[j] = NULL;
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
  enum df_changeable_flags saved_flags;
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
  df = XCNEW (struct df);
  df->changeable_flags = 0;

  bitmap_obstack_initialize (&df_bitmap_obstack);

  /* Set this to a conservative value.  Stack_ptr_mod will compute it
     correctly later.  */
  current_function_sp_is_unchanging = 0;

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

  df->hard_regs_live_count = XNEWVEC (unsigned int, FIRST_PSEUDO_REGISTER);
  memset (df->hard_regs_live_count, 0, 
	  sizeof (unsigned int) * FIRST_PSEUDO_REGISTER);

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


struct tree_opt_pass pass_df_initialize_opt =
{
  "dfinit",                             /* name */
  gate_opt,                             /* gate */
  rest_of_handle_df_initialize,         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  0,                                    /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0,                                    /* todo_flags_finish */
  'z'                                   /* letter */
};


static bool
gate_no_opt (void)
{
  return optimize == 0;
}


struct tree_opt_pass pass_df_initialize_no_opt =
{
  "dfinit",                             /* name */
  gate_no_opt,                          /* gate */
  rest_of_handle_df_initialize,         /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  0,                                    /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0,                                    /* todo_flags_finish */
  'z'                                   /* letter */
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

  if (df->postorder)
    free (df->postorder);
  if (df->postorder_inverted)
    free (df->postorder_inverted);
  free (df->hard_regs_live_count);
  free (df);
  df = NULL;

  bitmap_obstack_release (&df_bitmap_obstack);
  return 0;
}


struct tree_opt_pass pass_df_finish =
{
  "dfinish",                            /* name */
  NULL,					/* gate */
  rest_of_handle_df_finish,             /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  0,                                    /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0,                                    /* todo_flags_finish */
  'z'                                   /* letter */
};





/*----------------------------------------------------------------------------
   The general data flow analysis engine.
----------------------------------------------------------------------------*/


/* Helper function for df_worklist_dataflow.
   Propagate the dataflow forward. 
   Given a BB_INDEX, do the dataflow propagation
   and set bits on for successors in PENDING
   if the out set of the dataflow has changed. */

static void
df_worklist_propagate_forward (struct dataflow *dataflow,
                               unsigned bb_index,
                               unsigned *bbindex_to_postorder,
                               bitmap pending,
                               sbitmap considered)
{
  edge e;
  edge_iterator ei;
  basic_block bb = BASIC_BLOCK (bb_index);

  /*  Calculate <conf_op> of incoming edges.  */
  if (EDGE_COUNT (bb->preds) > 0)
    FOR_EACH_EDGE (e, ei, bb->preds)
      {								
        if (TEST_BIT (considered, e->src->index))		
          dataflow->problem->con_fun_n (e);
      }								
  else if (dataflow->problem->con_fun_0)
    dataflow->problem->con_fun_0 (bb);

  if (dataflow->problem->trans_fun (bb_index))
    {
      /* The out set of this block has changed. 
         Propagate to the outgoing blocks.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
          unsigned ob_index = e->dest->index;

          if (TEST_BIT (considered, ob_index))
            bitmap_set_bit (pending, bbindex_to_postorder[ob_index]);
        }
    }
}


/* Helper function for df_worklist_dataflow.
   Propagate the dataflow backward.  */

static void
df_worklist_propagate_backward (struct dataflow *dataflow,
                                unsigned bb_index,
                                unsigned *bbindex_to_postorder,
                                bitmap pending,
                                sbitmap considered)
{
  edge e;
  edge_iterator ei;
  basic_block bb = BASIC_BLOCK (bb_index);

  /*  Calculate <conf_op> of incoming edges.  */
  if (EDGE_COUNT (bb->succs) > 0)
    FOR_EACH_EDGE (e, ei, bb->succs)
      {								
        if (TEST_BIT (considered, e->dest->index))		
          dataflow->problem->con_fun_n (e);
      }								
  else if (dataflow->problem->con_fun_0)
    dataflow->problem->con_fun_0 (bb);

  if (dataflow->problem->trans_fun (bb_index))
    {
      /* The out set of this block has changed. 
         Propagate to the outgoing blocks.  */
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
          unsigned ob_index = e->src->index;

          if (TEST_BIT (considered, ob_index))
            bitmap_set_bit (pending, bbindex_to_postorder[ob_index]);
        }
    }
}



/* This will free "pending". */
static void 
df_worklist_dataflow_overeager (struct dataflow *dataflow,
				bitmap pending,
                                sbitmap considered,
                                int *blocks_in_postorder,
				unsigned *bbindex_to_postorder)
{
  enum df_flow_dir dir = dataflow->problem->dir;
  int count = 0;

  while (!bitmap_empty_p (pending))
    {
      unsigned bb_index;
      int index;
      count++;

      index = bitmap_first_set_bit (pending);
      bitmap_clear_bit (pending, index);

      bb_index = blocks_in_postorder[index];

      if (dir == DF_FORWARD)
	df_worklist_propagate_forward (dataflow, bb_index,
				       bbindex_to_postorder,
				       pending, considered);
      else 
	df_worklist_propagate_backward (dataflow, bb_index,
					bbindex_to_postorder,
					pending, considered);
    }

  BITMAP_FREE (pending);

  /* Dump statistics. */
  if (dump_file)
    fprintf (dump_file, "df_worklist_dataflow_overeager:"
	     "n_basic_blocks %d n_edges %d"
	     " count %d (%5.2g)\n",
	     n_basic_blocks, n_edges,
	     count, count / (float)n_basic_blocks);
}

static void 
df_worklist_dataflow_doublequeue (struct dataflow *dataflow,
			  	  bitmap pending,
                                  sbitmap considered,
                                  int *blocks_in_postorder,
				  unsigned *bbindex_to_postorder)
{
  enum df_flow_dir dir = dataflow->problem->dir;
  int dcount = 0;
  bitmap worklist = BITMAP_ALLOC (&df_bitmap_obstack);

  /* Double-queueing. Worklist is for the current iteration,
     and pending is for the next. */
  while (!bitmap_empty_p (pending))
    {
      /* Swap pending and worklist. */
      bitmap temp = worklist;
      worklist = pending;
      pending = temp;

      do
	{
	  int index;
	  unsigned bb_index;
	  dcount++;

	  index = bitmap_first_set_bit (worklist);
	  bitmap_clear_bit (worklist, index);

	  bb_index = blocks_in_postorder[index];

	  if (dir == DF_FORWARD)
	    df_worklist_propagate_forward (dataflow, bb_index,
					   bbindex_to_postorder,
					   pending, considered);
	  else 
	    df_worklist_propagate_backward (dataflow, bb_index,
					    bbindex_to_postorder,
					    pending, considered);
	}
      while (!bitmap_empty_p (worklist));
    }

  BITMAP_FREE (worklist);
  BITMAP_FREE (pending);

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
   This is so-called over-eager algorithm where it propagates
   changes on demand. This algorithm may visit blocks more than
   iterative method if there are deeply nested loops. 
   Worklist algorithm works better than iterative algorithm
   for CFGs with no nested loops.
   In practice, the measurement shows worklist algorithm beats 
   iterative algorithm by some margin overall.  
   Note that this is slightly different from the traditional textbook worklist solver,
   in that the worklist is effectively sorted by the reverse postorder.
   For CFGs with no nested loops, this is optimal. 
   
   The overeager algorithm while works well for typical inputs,
   it could degenerate into excessive iterations given CFGs with high loop nests
   and unstructured loops. To cap the excessive iteration on such case,
   we switch to double-queueing when the original algorithm seems to 
   get into such.
   */

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
  bbindex_to_postorder =
    (unsigned int *)xmalloc (last_basic_block * sizeof (unsigned int));

  /* Initialize the array to an out-of-bound value.  */
  for (i = 0; i < last_basic_block; i++)
    bbindex_to_postorder[i] = last_basic_block;

  /* Initialize the considered map.  */
  sbitmap_zero (considered);
  EXECUTE_IF_SET_IN_BITMAP (blocks_to_consider, 0, index, bi)
    {
      SET_BIT (considered, index);
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

  /* Solve it. Determine the solving algorithm
     based on a simple heuristic. */
  if (n_edges > PARAM_VALUE (PARAM_DF_DOUBLE_QUEUE_THRESHOLD_FACTOR)
      * n_basic_blocks)
    {
      /* High average connectivity, meaning dense graph
         with more likely deep nested loops
	 or unstructured loops. */
      df_worklist_dataflow_doublequeue (dataflow, pending, considered,
					blocks_in_postorder,
					bbindex_to_postorder);
    }
  else 
    {
      /* Most inputs fall into this case
        with relatively flat or structured CFG. */
      df_worklist_dataflow_overeager (dataflow, pending, considered,
				      blocks_in_postorder,
				      bbindex_to_postorder);
    }

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

#ifdef ENABLE_DF_CHECKING
  if (dflow->problem->verify_start_fun)
    dflow->problem->verify_start_fun ();
#endif

  /* (Re)Allocate the datastructures necessary to solve the problem.  */ 
  if (dflow->problem->alloc_fun)
    dflow->problem->alloc_fun (blocks_to_consider);

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
  
  if (df->postorder)
    free (df->postorder);
  if (df->postorder_inverted)
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
  return (struct df_scan_bb_info *) dflow->block_info[index];
}


/* Set basic block info.  */

static void
df_set_bb_info (struct dataflow *dflow, unsigned int index, 
		void *bb_info)
{
  gcc_assert (dflow->block_info);
  dflow->block_info[index] = bb_info;
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
  if (df && df_live)
    return bitmap_bit_p (df_live->out_of_date_transfer_functions, bb->index);
  else 
    return false;
}


/* Mark BB as needing it's transfer functions as being out of
   date.  */

void 
df_set_bb_dirty (basic_block bb)
{
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
  void **problem_temps;
  int size = last_basic_block * sizeof (void *);
  bitmap tmp = BITMAP_ALLOC (&df_bitmap_obstack);
  problem_temps = xmalloc (size);

  for (p = 0; p < df->num_problems_defined; p++)
    {
      struct dataflow *dflow = df->problems_in_order[p];

      /* Need to reorganize the out_of_date_transfer_functions for the
	 dflow problem.  */
      if (dflow->out_of_date_transfer_functions)
	{
	  bitmap_copy (tmp, dflow->out_of_date_transfer_functions);
	  bitmap_clear (dflow->out_of_date_transfer_functions);
	  if (bitmap_bit_p (tmp, ENTRY_BLOCK))
	    bitmap_set_bit (dflow->out_of_date_transfer_functions, ENTRY_BLOCK);
	  if (bitmap_bit_p (tmp, EXIT_BLOCK))
	    bitmap_set_bit (dflow->out_of_date_transfer_functions, EXIT_BLOCK);

	  i = NUM_FIXED_BLOCKS;
	  FOR_EACH_BB (bb) 
	    {
	      if (bitmap_bit_p (tmp, bb->index))
		bitmap_set_bit (dflow->out_of_date_transfer_functions, i);
	      i++;
	    }
	}

      /* Now shuffle the block info for the problem.  */
      if (dflow->problem->free_bb_fun)
	{
	  df_grow_bb_info (dflow);
	  memcpy (problem_temps, dflow->block_info, size);

	  /* Copy the bb info from the problem tmps to the proper
	     place in the block_info vector.  Null out the copied
	     item.  The entry and exit blocks never move.  */
	  i = NUM_FIXED_BLOCKS;
	  FOR_EACH_BB (bb) 
	    {
	      df_set_bb_info (dflow, i, problem_temps[bb->index]);
	      problem_temps[bb->index] = NULL;
	      i++;
	    }
	  memset (dflow->block_info + i, 0, 
		  (last_basic_block - i) *sizeof (void *));

	  /* Free any block infos that were not copied (and NULLed).
	     These are from orphaned blocks.  */
	  for (i = NUM_FIXED_BLOCKS; i < last_basic_block; i++)
	    {
	      basic_block bb = BASIC_BLOCK (i); 
	      if (problem_temps[i] && bb)
		dflow->problem->free_bb_fun
		  (bb, problem_temps[i]);
	    }
	}
    }

  /* Shuffle the bits in the basic_block indexed arrays.  */

  if (df->blocks_to_analyze)
    {
      if (bitmap_bit_p (tmp, ENTRY_BLOCK))
	bitmap_set_bit (df->blocks_to_analyze, ENTRY_BLOCK);
      if (bitmap_bit_p (tmp, EXIT_BLOCK))
	bitmap_set_bit (df->blocks_to_analyze, EXIT_BLOCK);
      bitmap_copy (tmp, df->blocks_to_analyze);
      bitmap_clear (df->blocks_to_analyze);
      i = NUM_FIXED_BLOCKS;
      FOR_EACH_BB (bb) 
	{
	  if (bitmap_bit_p (tmp, bb->index))
	    bitmap_set_bit (df->blocks_to_analyze, i);
	  i++;
	}
    }

  BITMAP_FREE (tmp);

  free (problem_temps);

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
	  gcc_assert (df_get_bb_info (dflow, old_index) == NULL);
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
	      df_set_bb_info (dflow, bb_index, NULL);
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
  if (saved_cfg)
    free (saved_cfg);
  saved_cfg = df_compute_cfg_image ();
}

#endif /* DF_DEBUG_CFG  */
/*----------------------------------------------------------------------------
   PUBLIC INTERFACES TO QUERY INFORMATION.
----------------------------------------------------------------------------*/


/* Return first def of REGNO within BB.  */

struct df_ref *
df_bb_regno_first_def_find (basic_block bb, unsigned int regno)
{
  rtx insn;
  struct df_ref **def_rec;
  unsigned int uid;

  FOR_BB_INSNS (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      uid = INSN_UID (insn);
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  struct df_ref *def = *def_rec;
	  if (DF_REF_REGNO (def) == regno)
	    return def;
	}
    }
  return NULL;
}


/* Return last def of REGNO within BB.  */

struct df_ref *
df_bb_regno_last_def_find (basic_block bb, unsigned int regno)
{
  rtx insn;
  struct df_ref **def_rec;
  unsigned int uid;

  FOR_BB_INSNS_REVERSE (bb, insn)
    {
      if (!INSN_P (insn))
	continue;

      uid = INSN_UID (insn);
      for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	{
	  struct df_ref *def = *def_rec;
	  if (DF_REF_REGNO (def) == regno)
	    return def;
	}
    }

  return NULL;
}

/* Finds the reference corresponding to the definition of REG in INSN.
   DF is the dataflow object.  */

struct df_ref *
df_find_def (rtx insn, rtx reg)
{
  unsigned int uid;
  struct df_ref **def_rec;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  gcc_assert (REG_P (reg));

  uid = INSN_UID (insn);
  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
    {
      struct df_ref *def = *def_rec;
      if (rtx_equal_p (DF_REF_REAL_REG (def), reg))
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
  
struct df_ref *
df_find_use (rtx insn, rtx reg)
{
  unsigned int uid;
  struct df_ref **use_rec;

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);
  gcc_assert (REG_P (reg));

  uid = INSN_UID (insn);
  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
    {
      struct df_ref *use = *use_rec;
      if (rtx_equal_p (DF_REF_REAL_REG (use), reg))
	return use;
    } 
  if (df->changeable_flags & DF_EQ_NOTES)
    for (use_rec = DF_INSN_UID_EQ_USES (uid); *use_rec; use_rec++)
      {
	struct df_ref *use = *use_rec;
	if (rtx_equal_p (DF_REF_REAL_REG (use), reg))
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
	  
	  df_print_bb_index (bb, file);
	  df_dump_top (bb, file);
	  df_dump_bottom (bb, file);
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


/* Dump the top of the block information for BB.  */ 

void
df_dump_top (basic_block bb, FILE *file)
{
  int i;

  if (!df || !file)
    return;

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->computed)
	{
	  df_dump_bb_problem_function bbfun = dflow->problem->dump_top_fun;
	  if (bbfun)
	    bbfun (bb, file); 
	}
    }
}


/* Dump the bottom of the block information for BB.  */ 

void
df_dump_bottom (basic_block bb, FILE *file)
{
  int i;

  if (!df || !file)
    return;

  for (i = 0; i < df->num_problems_defined; i++)
    {
      struct dataflow *dflow = df->problems_in_order[i];
      if (dflow->computed)
	{
	  df_dump_bb_problem_function bbfun = dflow->problem->dump_bottom_fun;
	  if (bbfun)
	    bbfun (bb, file); 
	}
    }
}


void
df_refs_chain_dump (struct df_ref **ref_rec, bool follow_chain, FILE *file)
{
  fprintf (file, "{ ");
  while (*ref_rec)
    {
      struct df_ref *ref = *ref_rec;
      fprintf (file, "%c%d(%d)",
	       DF_REF_REG_DEF_P (ref) ? 'd' : (DF_REF_FLAGS (ref) & DF_REF_IN_NOTE) ? 'e' : 'u',
	       DF_REF_ID (ref),
	       DF_REF_REGNO (ref));
      if (follow_chain)
	df_chain_dump (DF_REF_CHAIN (ref), file);
      ref_rec++;
    }
  fprintf (file, "}");
}


/* Dump either a ref-def or reg-use chain.  */

void
df_regs_chain_dump (struct df_ref *ref,  FILE *file)
{
  fprintf (file, "{ ");
  while (ref)
    {
      fprintf (file, "%c%d(%d) ",
	       DF_REF_REG_DEF_P (ref) ? 'd' : 'u',
	       DF_REF_ID (ref),
	       DF_REF_REGNO (ref));
      ref = ref->next_reg;
    }
  fprintf (file, "}");
}


static void
df_mws_dump (struct df_mw_hardreg **mws, FILE *file)
{
  while (*mws)
    {
      fprintf (file, "mw %c r[%d..%d]\n", 
	       ((*mws)->type == DF_REF_REG_DEF) ? 'd' : 'u',
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


void
df_insn_debug (rtx insn, bool follow_chain, FILE *file)
{
  df_insn_uid_debug (INSN_UID (insn), follow_chain, file);
}

void
df_insn_debug_regno (rtx insn, FILE *file)
{
  unsigned int uid = INSN_UID(insn);

  fprintf (file, "insn %d bb %d luid %d defs ",
	   uid, BLOCK_FOR_INSN (insn)->index, DF_INSN_LUID (insn));
  df_refs_chain_dump (DF_INSN_UID_DEFS (uid), false, file);
    
  fprintf (file, " uses ");
  df_refs_chain_dump (DF_INSN_UID_USES (uid), false, file);

  fprintf (file, " eq_uses ");
  df_refs_chain_dump (DF_INSN_UID_EQ_USES (uid), false, file);
  fprintf (file, "\n");
}

void
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


void
df_ref_debug (struct df_ref *ref, FILE *file)
{
  fprintf (file, "%c%d ",
	   DF_REF_REG_DEF_P (ref) ? 'd' : 'u',
	   DF_REF_ID (ref));
  fprintf (file, "reg %d bb %d insn %d flag 0x%x type 0x%x ",
	   DF_REF_REGNO (ref),
	   DF_REF_BBNO (ref),
	   DF_REF_INSN (ref) ? INSN_UID (DF_REF_INSN (ref)) : -1,
	   DF_REF_FLAGS (ref),
	   DF_REF_TYPE (ref));
  if (DF_REF_LOC (ref))
    fprintf (file, "loc %p(%p) chain ", (void *)DF_REF_LOC (ref), (void *)*DF_REF_LOC (ref));
  else
    fprintf (file, "chain ");
  df_chain_dump (DF_REF_CHAIN (ref), file);
  fprintf (file, "\n");
}

/* Functions for debugging from GDB.  */

void
debug_df_insn (rtx insn)
{
  df_insn_debug (insn, true, stderr);
  debug_rtx (insn);
}


void
debug_df_reg (rtx reg)
{
  df_regno_debug (REGNO (reg), stderr);
}


void
debug_df_regno (unsigned int regno)
{
  df_regno_debug (regno, stderr);
}


void
debug_df_ref (struct df_ref *ref)
{
  df_ref_debug (ref, stderr);
}


void
debug_df_defno (unsigned int defno)
{
  df_ref_debug (DF_DEFS_GET (defno), stderr);
}


void
debug_df_useno (unsigned int defno)
{
  df_ref_debug (DF_USES_GET (defno), stderr);
}


void
debug_df_chain (struct df_link *link)
{
  df_chain_dump (link, stderr);
  fputc ('\n', stderr);
}
