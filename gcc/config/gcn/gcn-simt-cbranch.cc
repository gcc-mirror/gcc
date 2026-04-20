/* AMD GCN "simt-cbranch" pass implementation.

   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by BayLibre.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* Transform conditional branches to support SIMT masks.  In SIMT mode,
   the program must execute both the true and false branches of a
   conditional, suitably masked.  There are short-circuit branches for the
   case where the condition is false for all threads, which serve as both
   an optimization, and to make it clear to subsequent compiler passes that
   the instructions should be treated as conditional (i.e. not eliminating
   common expressions that will be masked differently).

   For example:

       A
       if cond goto T else F
       | \
       T  F
       | /
       Z

   Becomes linear with short-cut branches:

       A
       true_mask = cond
       false_mask = exec & ~cond
       if (true_mask != 0)
     / |
     | exec = true_mask
     | T (masked)
     \ |
       if (false_mask != 0)
     / |
     | exec = false_mask
     | F (masked)
     \ |
       exec = true_mask | false_mask
       Z

  All the blocks in the function are sorted into a single, linear sequence
  in which blocks that depend on other blocks are placed in the correct order,
  but different code paths may be interleaved.  A scoring system is used to try
  to keep related blocks together (probably better for register pressure?),
  without actually requiring it.

  If there are circular dependencies (i.e. mark_dfs_back_edges emits
  EDGE_DFS_BACK attributes) then they are treated as a loop.  All blocks within
  the loop must be executed before blocks that leave the loop.  (This might
  cause problems where there are early abort scenarios, but other threads loop
  infinitely?) A loop may have multiple exit paths, all of which must be
  executed (once all threads have stopped looping), so on each iteration the
  exit conditions are accumulated, effectively transferring bits from EXEC into
  one of the exit masks.

  After both true and false branches have been executed, the masks are combined
  using OR, rather than restoring from a saved value, because this method is
  combinable across arbitrary numbers of nested or overlapping conditionals
  and loops.

  The function entry mask is not explicitly saved and restored, but as the
  condition masks are merged back together, at each logical joint point, the
  final mask should naturally return to the starting value before the function
  exit point.  The only exception is where some paths exit via "noreturn"
  functions.  In this case it is assumed that something like an "abort" is
  being called -- the SIMT paradigm is broken -- so we just make sure these are
  called before the regular return.  If the "noreturn" actually does return
  then the results are undefined.

  At no point should EXEC ever be allowed to be completely zero.  If this
  were to happen then scalar values that happen to have been assigned to
  VGPRs would be broken, and the program may never be able to recover EXEC
  (among other UB).

  All functions are converted to use SIMT conditionals, regardless of whether
  they will be used for SIMT (it must assume that they might be called that
  way).  If a function is called in single-thread mode (EXEC == 1) then the
  mask operations should function as booleans, the short-cut branches will end
  up mimicking the original branch structure somewhat closely, and a trace of
  actual working instructions should end up looking very similar to what it
  would have done without the transformations (in theory).
  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree-pass.h"
#include "rtl.h"
#include "cfghooks.h"
#include "diagnostic-core.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "tree.h"
#include "expr.h"
#include "explow.h"
#include "print-rtl.h"

const pass_data pass_data_gcn_simt_cbranch =
{
  RTL_PASS, // type
  "simt_cbranch", // name
  OPTGROUP_NONE, // optinfo_flags
  TV_SIMT, // tv_id
  0, // properties_required
  0, // properties_provided
  0, // properties_destroyed
  0, // todo_flags_start
  TODO_df_verify | TODO_df_finish, // todo_flags_finish
};

class pass_gcn_simt_cbranch : public rtl_opt_pass
{
public:
  pass_gcn_simt_cbranch (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_gcn_simt_cbranch, ctxt)
  {}

  // opt_pass methods:
  virtual bool gate (function *);
  virtual unsigned int execute (function *);
};

rtl_opt_pass *
make_pass_gcn_simt_cbranch (gcc::context *ctxt)
{
  return new pass_gcn_simt_cbranch (ctxt);
}

bool
pass_gcn_simt_cbranch::gate (function *)
{
  /* This pass is required for correctness whenever -msimt is enabled.  */
  return flag_simt_mode;
}

/* Reject any function whose CFG contains an EDGE_COMPLEX edge (abnormal,
   abnormal call, EH, or preserve).  These model control flow that this pass
   does not attempt to linearize: exceptions, __builtin_setjmp/longjmp and
   nested-function non-local goto (which share the same abnormal call
   edges), transactional memory, and sibcalls.  GCN supports none of these,
   so in practice this should only trigger on testcases that exercise the
   construct directly.  */

static bool
reject_unsupported_edges (function *fn)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    for (edge e : bb->succs)
      if (e->flags & EDGE_COMPLEX)
	{
	  sorry ("exceptions, %<setjmp%>/%<longjmp%>, non-local gotos, "
		 "transactional memory, and sibling calls are not "
		 "supported with %<-msimt%>");
	  return true;
	}
  return false;
}

/* Transform the CFG in order to simplify later analysis and transformation.

  Insert some fake edges to prevent abort/exit calls breaking dominance.
  Simply looping back on itself is enough to prevent all the post-dominators
  becoming EXIT_BLOCK.  The EDGE_FAKE will be ignored during analysis and
  removed during transformation.

  Entry         Entry
  |\            |\,-----.
  | Abort   >   | Abort |
  |             | `-----'
  Exit          Exit

  This will not handle proper sibcalls, but GCN doesn't have machine
  description patterns for those in any case.
*/

static void
work_around_noreturn_blocks (function *fn)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      if (EDGE_COUNT (bb->succs) == 0)
	{
	  /* Select a destination block that will preserve the dominators
	     information, and allow post-dominators to choose something
	     more useful than EXIT_BLOCK.  */
	  basic_block dest = bb;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Inserting fake back edge bb %d -> bb %d\n",
		     bb->index, dest->index);
	  make_edge (bb, dest, EDGE_FAKE | EDGE_FALLTHRU);
	}
    }
}

/* Transform the CFG in order to simplify later analysis and transformation.

   Whenever a block is both a fork and a join, split the block in two.

   A   B        A   B
    \ /          \ /
     C       >    C'   <- new
    / \           |
   X   Y          C
                 / \
                X   Y
*/

static void
separate_forks_from_joins (function *fn)
{
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      if (EDGE_COUNT (bb->preds) > 1 && EDGE_COUNT (bb->succs) > 1)
	{
	  /* The split point is somewhat arbitrary, but it needs to be
	     after the initial label and note instructions.  */
	  rtx_insn *insn;
	  FOR_BB_INSNS (bb, insn)
	    {
	      if (!LABEL_P (insn) && !NOTE_P (insn))
		break;
	    }

	  split_block (bb, PREV_INSN (insn));
	}
    }
}

/* These are helper types for simplify_join_blocks.  */

struct fork_join {
  basic_block fork, join;
  vec<int> edges_to_redirect;

  fork_join ()
  {
    edges_to_redirect.create (16);
  }
  /* Don't destruct the vec to avoid double free.
     The garbage collector will get the left over vectors.  */
};

static int
fj_cmp (const void *A, const void *B)
{
  int lenA = (*(const struct fork_join *const *)A)->edges_to_redirect.length ();
  int lenB = (*(const struct fork_join *const *)B)->edges_to_redirect.length ();

  return (lenA == lenB ? 0
	  : lenA > lenB ? 1
	  : -1);
}

// FIXME: encapsulate first_new_bb
static int first_new_bb = 0;
static bool
callback_join_edges (edge e, void *data)
{
  auto active_fj = (struct fork_join *)data;

  while (e->src->index >= first_new_bb
	 || e->src->index == active_fj->join->index)
    {
      for (unsigned i = 0; i < EDGE_COUNT (e->src->preds); i++)
	if (!(EDGE_PRED (e->src, i)->flags & (EDGE_DFS_BACK | EDGE_FAKE)))
	  {
	    /* Select the first precessor that is not a fake or back edge.  */
	    e = EDGE_PRED (e->src, i);
	    break;
	  }
    }
  return active_fj->edges_to_redirect.contains (e->src->index);
}

/* Transform the CFG in order to simplify later analysis and transformation.

   For each conditional branch, ensure that the dominators and
   post-dominators are not shared with other conditional regions.
   That is, ensure that multiple forks do not share a join, by inserting
   additional empty basic blocks.

   A           A
   |\          |\
   B C         B C
   | |\     >  | |\
   | E F       | E F
   |/,-'       | |/
   Z           | Y    <- new
               |/
               Z

   We scan first and modify after so as not to break dominance info, but
   also because the new blocks need to be created properly nested.  */

static void
simplify_join_blocks (function *fn)
{
  if (dump_file)
    fprintf (dump_file, "\nSimplifying shared join blocks...\n");

  calculate_dominance_info (CDI_DOMINATORS);
  calculate_dominance_info (CDI_POST_DOMINATORS);

  auto_vec<struct fork_join> worklist;

  /* Find the join blocks that are shared by multiple forks.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      /* Blocks without multiple successors can't be conditional.  */
      if (EDGE_COUNT (bb->succs) < 2)
	continue;

      basic_block join_bb = get_immediate_dominator (CDI_POST_DOMINATORS, bb);
      if (EDGE_COUNT (join_bb->preds) < 2)
	/* Short-cut the trivial case.  */
	continue;

      struct fork_join fj;
      fj.fork = bb;
      fj.join = join_bb;

      /* Make a list of all the edges that come from bb.  */
      for (edge e : join_bb->preds)
	if (e->src == bb
	    || dominated_by_p (CDI_DOMINATORS, e->src, bb))
	  fj.edges_to_redirect.safe_push (e->src->index);

      /* Ignore cases where all (or no, or one) edges come from bb.  */
      if (fj.edges_to_redirect.length () > 1
	  && fj.edges_to_redirect.length () != EDGE_COUNT (join_bb->preds))
	{
	  worklist.safe_push (fj);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "bb %d joins too many forks;"
		     " splitting edges originating at bb %d\n",
		     fj.join->index, fj.fork->index);
	}
    }

  /* make_forwarder_block does not seem to leave the dominator info in a
     usable state.  */
  free_dominance_info (CDI_POST_DOMINATORS);
  free_dominance_info (CDI_DOMINATORS);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "%d joins to fix up\n\n", worklist.length ());

  if (worklist.is_empty ())
    return;

  /* Sort the worklist such that the smallest sets of joins come first.
     By construction, these are the ones that feed into the larger sets,
     so must come first.  */
  auto_vec<struct fork_join *, 8> sorted_worklist (worklist.length ());
  for (auto& fj : worklist)
    sorted_worklist.quick_push (&fj);
  sorted_worklist.qsort (fj_cmp);

  /* Insert new join blocks such that each one corresponds to one fork.
     As we insert the blocks the pointers in worklist become obsolete,
     so we track a simple set of mappings to the new correct value.  */
  first_new_bb = last_basic_block_for_fn (fn);
  basic_block *bb_mappings = (basic_block *) alloca (sizeof (basic_block)
						     * first_new_bb);
  memset (bb_mappings, 0, sizeof (basic_block) * first_new_bb);
  for (auto fj : sorted_worklist)
    {
      int orig_bb_id = fj->join->index;
      basic_block join_bb = (bb_mappings[orig_bb_id]
			     ? bb_mappings[orig_bb_id]
			     : fj->join);

      edge newedge = make_forwarder_block (join_bb, callback_join_edges, fj);
      bb_mappings[orig_bb_id] = newedge->dest;
    }

  /* The transformations might have exposed some more cases, so repeat.  */
  simplify_join_blocks (fn);
}

/* Helpers for unify_joining_back_edges.  */

static bool
callback_back_edges (edge e, void *)
{
  return e->flags & EDGE_DFS_BACK;
}

static bool
callback_forward_edges (edge e, void *)
{
  return !(e->flags & EDGE_DFS_BACK);
}

/* Transform the CFG in order to simplify later analysis and transformation.

   For each block that receives multiple loop back edges, create a new block
   that joins those before the back edge.

   | .---.       |,---.
   |/,-. |       A    |
   A   | |       |\   |
   |`--' |    >  B |  |
   B     |       |\|  |
   |`----'       | C--'  <- new
   Z             Z

   Do the same for the non-back edges, so that there's a clear dominator.

   A B         A B
   |/,-.       |/
   L   |   >   C    <- new
   |`--'       |,-.
   Z           L  |
               |`-'
               Z
*/

static void
unify_joining_back_edges (function *fn)
{
  if (dump_file)
    fprintf (dump_file, "\nUnifying shared loop back blocks...\n");

  /* Find the join blocks that are shared by multiple back edges.  */
  mark_dfs_back_edges (fn);
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      int total_incoming_edges = EDGE_COUNT (bb->preds);
      int incoming_back_edges = 0;

      for (edge e : bb->preds)
	if (e->flags & EDGE_DFS_BACK)
	  incoming_back_edges++;

      if (incoming_back_edges > 1)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "bb %d joins %d back-edges;"
		     " inserting a forwarder block\n",
		     bb->index, incoming_back_edges);

	  edge newloopback = make_forwarder_block (bb, callback_back_edges,
						   NULL);

	  /* Fix up the edge flags so we can use them in the analysis.
	     Note: the back edges shown in the dot graph dumps are
	     calculated independently of what we store here.  */
	  newloopback->flags |= EDGE_DFS_BACK;
	  for (edge e : bb->preds)
	    e->flags &= ~EDGE_DFS_BACK;
	}

      if ((total_incoming_edges - incoming_back_edges) > 1)
	make_forwarder_block (bb, callback_forward_edges, NULL);
    }
}

/* Transform the CFG in order to simplify later analysis and transformation.

   For each block that has two outgoing edges, ensure that none of the
   successors have multiple incoming edges.  Insert additional empty blocks to
   give the separation.

   A   B       A   B
   |\ /        |\  |
   C D      >  C X |  <- new
   |/          | |/
   Z           | D
               |/
               Z
*/

static void
separate_forking_blocks (function *fn)
{
  if (dump_file)
    fprintf (dump_file, "\nSeparating successive conditionals...\n");

  /* Find the join blocks that are shared by multiple back edges.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      if (EDGE_COUNT (bb->succs) < 2)
	continue;

      for (edge e : bb->succs)
	if (EDGE_COUNT (e->dest->preds) > 1)
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file,
		       "edge bb %d -> bb %d connects a fork to a join directly;"
		       " inserting a separator block\n", e->src->index,
		       e->dest->index);

	    split_edge (e);
	  }
    }
}

/* Scan and record a region of basic blocks reachable from START
   without passing through the blocks present in LIMITS.

   All other fields are derived from START and LIMITS.

   BLOCKS includes all blocks within the region, including any of the
   blocks from LIMITS if they actually are reachable, but does not include
   blocks from which there is no path to any of the LIMITS.

   REACHABLE includes all the blocks reachable from START, all the way to
   the function exit block, without passing through LIMITS. This is a
   superset of BLOCKS.

   ANCESTORS includes all the blocks that have a path to LIMITS without
   passing through START. This will be a superset of BLOCKS.  It is only
   computed when DO_REVERSE_SCAN is set.

   PARENTS includes all blocks that are not themselves within the region
   described by BLOCKS, but have edges coming into the region, excluding
   parents of blocks (only) in LIMITS, or of START itself. This is a subset
   of ANCESTORS.

   EXITS includes all blocks that are not themselves within the region
   described by BLOCKS, but are alternative successors of blocks within
   BLOCKS.  This is a subset of REACHABLE. It is only computed when
   DO_REVERSE_SCAN is set.  */

class bbregion {
public:
  basic_block start;
  bitmap_head limits;

  bitmap_head blocks;
  bitmap_head reachable;
  bitmap_head ancestors;
  bitmap_head parents;
  bitmap_head exits;

  /* Empty constructor for deferred initialization via init.  */
  bbregion ()
    : start(NULL), do_reverse_scan(false)
  {
    bitmap_initialize (&blocks, NULL);
    bitmap_initialize (&reachable, NULL);
    bitmap_initialize (&ancestors, NULL);
    bitmap_initialize (&parents, NULL);
    bitmap_initialize (&exits, NULL);
    bitmap_initialize (&limits, NULL);
  }

  /* Construct and initialize in one step.  */
  bbregion (basic_block start, const_bitmap limits, function *fn)
    : bbregion()
  {
    init (start, limits, fn);
  }

  /* For deferred init pattern only.  */
  void init (basic_block start, const_bitmap limits, function *fn)
  {
    this->start = start;
    bitmap_copy (&this->limits, limits);

    scan_region (fn);
  }

  ~bbregion ()
  {
    bitmap_clear (&blocks);
    bitmap_clear (&reachable);
    bitmap_clear (&ancestors);
    bitmap_clear (&parents);
    bitmap_clear (&exits);
    bitmap_clear (&limits);
  }

  void dump (FILE *fd, const char *indent) const
  {
    fprintf (fd, "%sbbregion: bb %d -> ", indent, start->index);
    dump_bitmap (fd, &limits);
    fprintf (fd, "%s  reachable: ", indent);
    dump_bitmap (fd, &reachable);
    fprintf (fd, "%s  ancestors: ", indent);
    dump_bitmap (fd, &ancestors);
    fprintf (fd, "%s  blocks:    ", indent);
    dump_bitmap (fd, &blocks);
    fprintf (fd, "%s  parents:   ", indent);
    dump_bitmap (fd, &parents);
    fprintf (fd, "%s  exits:     ", indent);
    dump_bitmap (fd, &exits);
  }

protected:
  /* Set this to control the scan mode.  Loops need the scan both forward
     and backward to fully determine the bitmaps, but that's expensive
     and not needed for conditionals, so we can turn it off here.  */
  bool do_reverse_scan;

  /* Initiate the CFG scans and populate the bitmaps.  */
  void scan_region (function *fn)
  {
    /* Walking the region forward finds all reachable blocks, including
       blocks reachable from loop exits and such like.  This is sufficient
       when "limit" includes the post-dominator of the region.  */
    scan_forward (start);

    if (!do_reverse_scan)
      {
	bitmap_copy (&blocks, &reachable);
	return;
      }

    /* Walking the region backwards finds all the ancestors of the blocks
       within the region, which allows us to eliminate blocks that are
       reachable from the start point, but do not themselves have a path
       to the "limit" blocks.  This is most useful for loop regions that
       have early exit paths.  */
    bitmap_iterator bi;
    unsigned num;
    EXECUTE_IF_SET_IN_BITMAP (&limits, 0, num, bi)
      scan_reverse (BASIC_BLOCK_FOR_FN (fn, num));

    /* The intersection of the two scans gives the blocks inside the
       region.  */
    bitmap_and (&blocks, &reachable, &ancestors);

    if (bitmap_empty_p (&blocks) && !bitmap_empty_p (&reachable))
      {
	/* The reachable blocks did not include any of the limits.
	   This happens for early function exits (e.g. "abort").  */
	bitmap_copy (&blocks, &reachable);
	return;
      }

    /* Remove "parent" bits set by the forwards scan, if the reverse scan
       showed that they're not actually parents.  */
    int delayed_clear = -1;
    EXECUTE_IF_SET_IN_BITMAP (&parents, 0, num, bi)
      {
	if (delayed_clear >= 0)
	  {
	    bitmap_clear_bit (&parents, delayed_clear);
	    delayed_clear = -1;
	  }

	bool found = false;
	for (edge e : BASIC_BLOCK_FOR_FN (fn, num)->succs)
	  if (bitmap_bit_p (&blocks, e->dest->index))
	    {
	      found = true;
	      break;
	    }
	if (!found)
	  delayed_clear = num;
      }
    if (delayed_clear >= 0)
      bitmap_clear_bit (&parents, delayed_clear);
  }

  /* Recursive graph walk from BB to any of LIMITS, filling in REACHABLE
     and PARENTS.  REACHABLE serves as the visited list.  */
  void scan_forward (basic_block bb)
  {
    if (bitmap_bit_p (&reachable, bb->index))
      return;

    bitmap_set_bit (&reachable, bb->index);
    bitmap_clear_bit (&parents, bb->index);

    if (bitmap_bit_p (&limits, bb->index))
      return;

    if (bb != start)
      for (edge e : bb->preds)
	if (!(e->flags & EDGE_FAKE)
	    && !bitmap_bit_p (&reachable, e->src->index))
	  bitmap_set_bit (&parents, e->src->index);

    for (edge e : bb->succs)
      if (!(e->flags & EDGE_FAKE))
	scan_forward (e->dest);
  }

  /* Recursive graph walk from BB to START, filling in ANCESTORS and EXITS.
     ANCESTORS serves as the visited list.  */
  void scan_reverse (basic_block bb)
  {
    if (bitmap_bit_p (&ancestors, bb->index))
      return;

    bitmap_set_bit (&ancestors, bb->index);
    bitmap_clear_bit (&exits, bb->index);

    if (!bitmap_bit_p (&limits, bb->index))
      for (edge e : bb->succs)
	if (!(e->flags & EDGE_FAKE)
	    && !bitmap_bit_p (&ancestors, e->dest->index))
	  bitmap_set_bit (&exits, e->dest->index);

    if (bb == start)
      return;

    for (edge e : bb->preds)
      if (!(e->flags & EDGE_FAKE))
	scan_reverse (e->src);
  }
};

/* A subclass for bbregions that represents loops.

   START is the loop-head block, that has the incoming back-edge.
   LIMITS is the block at the start of the back-edge. */

class loop_region : public bbregion
{
public:
  loop_region (basic_block start, function *fn)
    : bbregion()
  {
    caller = NULL;
    for (edge e : start->preds)
      {
	if (e->flags & EDGE_FAKE)
	  continue;

	if (e->flags & EDGE_DFS_BACK)
	  {
	    /* unify_joining_back_edges should have ensured there's only one
	       incoming back-edge.  */
	    gcc_assert (this->start == NULL);

	    auto_bitmap limits;
	    bitmap_set_bit (limits, e->src->index);

	    /* Configure the scan to exclude exit paths.  */
	    do_reverse_scan = true;
	    init (start, limits, fn);
	  }
	else
	  {
	    /* unify_joining_back_edges should have ensured there's only one
	       incoming regular edge.  */
	    gcc_assert (caller == NULL);
	    caller = e->src;
	  }
      }
  }

  /* The parent block that enters the loop but is not in the loop.  */
  basic_block caller;

  /* Each exit point has its own pseudoreg with its own mask.  */
  hash_map<int_hash<int, -1, -2>, rtx> exit_conditions;

  void dump (FILE *fd, const char *indent) const
  {
    fprintf (fd, "%sloop_region: bb %d -> ", indent, start->index);
    dump_bitmap (fd, &limits);
    fprintf (fd, "%s  caller: bb %d\n", indent, caller->index);
    for (auto exit : exit_conditions)
      {
	fprintf (fd, "\n%s  exit via bb %d: ", indent, exit.first);
	dump_value_slim (fd, exit.second, 1);
      }
    fprintf (fd, "\n ");
    ((const bbregion *) this)->dump (fd, indent);
  }
};

/* A subclass for bbregions that represent fork/branch/join conditionals.

   START is the fork block.
   LIMITS contains the post-dominator block.
   JOIN is also the post-dominator block.  */

class cond_region : public bbregion
{
public:
  cond_region (basic_block fork, function *fn)
  {
    join = get_immediate_dominator (CDI_POST_DOMINATORS, fork);

    auto_bitmap limits;
    bitmap_set_bit (limits, join->index);
    init (fork, limits, fn);

    false_branch_condvar = NULL;
    true_branch_condvar = NULL;
    false_is_loop_exit = false;
    true_is_loop_exit = false;
  }

  /* The join block.  (Use bbregion::start for the fork block.)  */
  basic_block join;

  /* The mask variables that will be set for the transformed CFG.  */
  rtx false_branch_condvar;
  rtx true_branch_condvar;
  bool false_is_loop_exit;
  bool true_is_loop_exit;

  void dump (FILE *fd, const char *indent) const
  {
    fprintf (fd, "%scond_region: fork bb %d -> join bb %d\n", indent,
	     start->index, join->index);
    ((const bbregion *) this)->dump (fd, indent);
  }
};

DEBUG_FUNCTION void
debug (const bbregion *obj)
{
  obj->dump (stderr, "");
}

DEBUG_FUNCTION void
debug (const bbregion obj)
{
  debug (&obj);
}

DEBUG_FUNCTION void
debug (const cond_region *obj)
{
  obj->dump (stderr, "");
}

DEBUG_FUNCTION void
debug (const cond_region obj)
{
  debug (&obj);
}

DEBUG_FUNCTION void
debug (const loop_region *obj)
{
  obj->dump (stderr, "");
}

DEBUG_FUNCTION void
debug (const loop_region obj)
{
  debug (&obj);
}

/* This class holds all the pass state that would otherwise be global or
   need passing around.  There's only one instance per pass invocation.  */

class simt_cbranch_state {
public:
  function *fn;

  /* All the loops in the function.
     LOOPHEADS and LOOPCALLERS are indices into LOOPS.  */
  auto_vec<loop_region *> loops;
  hash_map<int_hash<int, -1, -2>, loop_region *> loopheads;
  hash_map<int_hash<int, -1, -2>, loop_region *> loopcallers;

  /* All the conditions in the function, indexed by fork and joins blocks.
     FORKS and JOINS are indices into CONDITIONALS.  */
  auto_vec<cond_region *> conditionals;
  hash_map<int_hash<int, -1, -2>, cond_region *> forks;
  hash_map<int_hash<int, -1, -2>, cond_region *> joins;

  /* Computed block ordering and required condition masks for each.  */
  auto_vec<int> output_bb_seq;        /* Sorted by order-of-output.  */
  auto_vec<rtx> output_bb_condmasks;  /* Indexed by BB ID.  */

  /* The data needed to plan each short-cut branch.  */
  struct short_cut_plan
    {
      basic_block src, dest;        /* Blocks for the new edge.  */
      rtx_insn *setexec_insn;       /* The place to split the SRC block.  */
      hash_set<rtx> skipped_cvars;  /* Condition variables to be handled.  */
    };
  auto_vec<short_cut_plan> cuts;

  simt_cbranch_state (function *fn)
  : fn(fn)
  {
    output_bb_seq.reserve (last_basic_block_for_fn (fn));
    output_bb_condmasks.safe_grow_cleared (last_basic_block_for_fn (fn));
  }

  loop_region *check_overlaps (bbregion *conditional);
  void scan_cfg ();
  void determine_output_bb_sequence ();
  void transform_cfg ();
  void plan_short_cut_branches ();
  void insert_short_cut_branches ();
  void handle_simt_enter_exit ();

  void execute ()
  {
    calculate_dominance_info (CDI_POST_DOMINATORS);
    mark_dfs_back_edges (fn);

    scan_cfg ();
    determine_output_bb_sequence ();
    transform_cfg ();
    plan_short_cut_branches ();
    insert_short_cut_branches ();
    handle_simt_enter_exit ();

    if (dump_file && (dump_flags & TDF_DETAILS))
      fprintf (dump_file, "\n");

    free_dominance_info (CDI_POST_DOMINATORS);
  }
};

/* Returns the outermost loop that overlaps CONDITIONAL.
   The function also serves to add potentially useful information to the
   TDF_DETAILS dumps.  */

loop_region *
simt_cbranch_state::check_overlaps (bbregion *conditional)
{
  loop_region *result = NULL;

  for (auto loop : loops)
    {
      auto_bitmap intersection;
      bitmap_and (intersection, &loop->blocks, &conditional->blocks);
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && bitmap_equal_p (&conditional->blocks, intersection))
	fprintf (dump_file, "    Condition is within loop at %d\n",
		 loop->start->index);
      else if (dump_file && (dump_flags & TDF_DETAILS)
	       && !bitmap_empty_p (intersection))
	fprintf (dump_file,
		 "    Condition partially overlaps loop at %d\n",
		 loop->start->index);

      if (bitmap_bit_p (&loop->exits, conditional->start->index))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "    Branch is loop exit\n");

	  /* Return the outermost loop if this exits multiple loops.  */
	  if (!result
	      || !bitmap_bit_p (&result->blocks, loop->start->index))
	    result = loop;
	}
    }

  return result;
}

/* Return true if all of the CFG direct dependencies (predecessors) of BB
   are marked in DONE.  */

static bool
block_dependencies_done_p (basic_block bb, bitmap done)
{
  for (edge e : bb->preds)
    if (!(e->flags & EDGE_FAKE)
	&& !(e->flags & EDGE_DFS_BACK)
	&& !bitmap_bit_p (done, e->src->index))
    return false;
  return true;
}

/* Like emit_insn_before/after, but inserts the instruction at the head of
   a block, after the special insns.  */

static rtx_insn *
emit_insn_at_bb_head (rtx x, basic_block bb)
{
  rtx_insn *insn;
  FOR_BB_INSNS (bb, insn)
    {
      if (NONDEBUG_INSN_P (insn))
	return emit_insn_before (x, insn);
    }
  return emit_insn_after (x, BB_END (bb));
}

/* Like FALLTHRU_EDGE, but does not assume the block has a conditional.  */

static edge
fallthru_edge (basic_block bb)
{
  switch (EDGE_COUNT (bb->succs))
    {
    case 1:
      return EDGE_SUCC (bb, 0);
    case 2:
      return FALLTHRU_EDGE (bb);
    default:
      gcc_unreachable ();
    }
  return NULL;
}

/* Scan the CFG and build the LOOPS and CONDITIONALS tables.  */

void
simt_cbranch_state::scan_cfg ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nScanning block dependencies...\n");

  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      for (edge e : bb->preds)
	{
	  if (e->flags & EDGE_DFS_BACK
	      && !(e->flags & EDGE_FAKE))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  scanning loop...\n");

	      loop_region *loop = new loop_region (bb, fn);
	      loops.safe_push (loop);
	      loopheads.put (loop->start->index, loop);
	      loopcallers.put (loop->caller->index, loop);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		loop->dump (dump_file, "    ");
	    }
	}

      if (EDGE_COUNT (bb->succs) == 2)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  scanning conditional...\n");

	  cond_region *region = new cond_region (bb, fn);
	  conditionals.safe_push (region);
	  forks.put (region->start->index, region);
	  joins.put (region->join->index, region);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    region->dump (dump_file, "    ");
	}
    }
}

/* Determine a suitable linear SIMT ordering for the entire CFG.
   Populates the OUTPUT_BB_SEQ and OUTPUT_BB_CONDMASKS tables.

   These values are used to score each block in the CFG.  The score is used
   when selecting a block from the deferred list.  The order will have some
   impact on optimization (register pressure, and block coalescing, perhaps),
   but these values should not affect correctness (although special scores are
   also used to keep the function exit last).  */

#define SCORE_INLOOP 8
#define SCORE_NOEXIT 4
#define SCORE_CONDITIONAL 1

void
simt_cbranch_state::determine_output_bb_sequence ()
{
  auto_vec<basic_block> deferred;
  auto_bitmap visited;
  auto_vec<int> scores (last_basic_block_for_fn (fn));
  scores.quick_grow_cleared (last_basic_block_for_fn (fn));

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nScoring BB priorities...\n");

  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  bb %d:", bb->index);

      int score = 0;

      /* Score priority based on nesting depth.  Loops are
	 weighted higher than conditionals.  */
      for (auto loop : loops)
	if (bitmap_bit_p (&loop->blocks, bb->index))
	  {
	    score += SCORE_INLOOP;

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, " +loop");
	  }
      for (auto c : conditionals)
	{
	  if (bitmap_bit_p (&c->blocks, bb->index))
	    {
	      score += SCORE_CONDITIONAL;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, " +if");
	    }
	}
      if (fallthru_edge (bb)->dest->index == EXIT_BLOCK
	  && NONDEBUG_INSN_P (BB_END (bb))
	  && GET_CODE (PATTERN (BB_END (bb))) == USE)

	{
	  score -= 88888;  /* Highly disparage epilogue blocks.  */

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " -epilogue");
	}

      scores[bb->index] = score;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " -> %d\n", score);
    }
  scores[EXIT_BLOCK] = -99999;  /* Very highly disparage the EXIT_BLOCK.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  bb %d: -exit -> %d\n", EXIT_BLOCK,
	     scores[EXIT_BLOCK]);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nDetermining an execution sequence ...\n");
  bb = ENTRY_BLOCK_PTR_FOR_FN (fn);
  while (1)
    {
      if (bb
	  && ((scores[bb->index] < 0 && !deferred.is_empty ())
	      || !block_dependencies_done_p (bb, visited)))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "bb %d has unmet dependencies;"
		     " defer and select another...\n",
		     bb->index);

	  deferred.safe_push (bb);
	  bb = NULL;
	}

      if (bb && bitmap_bit_p (visited, bb->index))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "bb %d already visited; select another...\n",
		     bb->index);

	  bb = NULL;
	}

      if (!bb)
	{
	  int candidate_score = -9999999;
	  bb = NULL;
	  for (auto deferred_bb : deferred)
	    if (!bitmap_bit_p (visited, deferred_bb->index)
		&& block_dependencies_done_p (deferred_bb, visited)
		&& scores[deferred_bb->index] > candidate_score)
	      {
		candidate_score = scores[deferred_bb->index];
		bb = deferred_bb;
	      }

	  if (bb && dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Available blocks:");
	      for (auto db : deferred)
		{
		  if (bitmap_bit_p (visited, db->index))
		    ;
		  else if (!block_dependencies_done_p (db, visited))
		    fprintf (dump_file, " bb %d (unmet deps)", db->index);
		  else
		    fprintf (dump_file, " bb %d (score:%d)", db->index,
			     scores[db->index]);
		}
	      fprintf (dump_file, "\n => Select bb %d\n", bb->index);
	    }
	}
      if (!bb)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "No blocks remain; finished.\n");

	  break;
	}

      /* The next BB is now selected.  */
      output_bb_seq.quick_push (bb->index);
      bitmap_set_bit (visited, bb->index);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "==========> BB %d is next block\n", bb->index);

      switch (EDGE_COUNT (bb->succs))
	{
	case 0:
	  bb = NULL;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Block has no successor\n");
	  break;
	case 1:
	  bb = EDGE_SUCC (bb, 0)->dest;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Fall through to natural successor\n");
	  break;
	case 2:
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Fork point; need to select a branch...\n");

	      cond_region &all = **forks.get (bb->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "    fork/join region %d => %d ....\n",
			   bb->index, all.join->index);
		  all.dump (dump_file, "      ");
		  check_overlaps (&all);
		}

	      auto_bitmap limits;
	      bitmap_set_bit (limits, all.start->index);
	      bitmap_set_bit (limits, all.join->index);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "    false_branch region %d => %d ....\n",
			 fallthru_edge (bb)->dest->index, all.join->index);
	      bbregion false_branch (fallthru_edge (bb)->dest, limits, fn);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		false_branch.dump (dump_file, "      ");
	      loop_region *false_branch_exits_loop = check_overlaps (&false_branch);
	      all.false_is_loop_exit = (false_branch_exits_loop != NULL);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "    true_branch region %d => %d ....\n",
			 BRANCH_EDGE (bb)->dest->index, all.join->index);
	      bbregion true_branch (BRANCH_EDGE (bb)->dest, limits, fn);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		true_branch.dump (dump_file, "      ");
	      loop_region *true_branch_exits_loop = check_overlaps (&true_branch);
	      all.true_is_loop_exit = (true_branch_exits_loop != NULL);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  auto_bitmap intersection;
		  bitmap_and (intersection, &false_branch.blocks, &true_branch.blocks);
		  fprintf (dump_file, "    overlap: %d blocks\n",
			   bitmap_count_bits (intersection));
		}

	      all.false_branch_condvar = gen_reg_rtx (DImode);
	      all.true_branch_condvar = gen_reg_rtx (DImode);

	      if (false_branch_exits_loop)
		false_branch_exits_loop->exit_conditions.put (bb->index,
							      all.false_branch_condvar);
	      if (true_branch_exits_loop)
		true_branch_exits_loop->exit_conditions.put (bb->index,
							     all.true_branch_condvar);

	      if (false_branch_exits_loop)
		{
		  deferred.safe_push (false_branch.start);
		  if (!bitmap_bit_p (&false_branch.reachable, all.join->index))
		    scores[false_branch.start->index] += SCORE_NOEXIT;

		  /* What was the branch will now be the fall-through.  */
		  bb = true_branch.start;

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "Use true branch as fallthrough; defer BB %d\n",
			     false_branch.start->index);
		}
	      else
		{
		  deferred.safe_push (true_branch.start);
		  if (!bitmap_bit_p (&true_branch.reachable, all.join->index))
		    scores[true_branch.start->index] += SCORE_NOEXIT;

		  /* Continue to use the same fall-through as before.  */
		  bb = false_branch.start;

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "Use false branch as fallthrough; defer BB %d\n",
			     true_branch.start->index);
		}
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  /* Calculate the EXEC masks for each block in the new chain.  */
  for (auto num : output_bb_seq)
    {
      bb = BASIC_BLOCK_FOR_FN (fn, num);

      if (bb->index == ENTRY_BLOCK)
	output_bb_condmasks[bb->index] = gen_rtx_REG (DImode, EXEC_REG);
      else
	{
	  loop_region **lp = loopheads.get (bb->index);

	  if (lp)
	    /* The loop condition variable will have been initialized
	       in the "caller" block, and updated each iteration.  */
	    output_bb_condmasks[bb->index] = gen_rtx_REG (DImode, EXEC_REG);
	  else
	    {
	      for (edge e : bb->preds)
		{
		  if (e->flags & EDGE_FAKE)
		    continue;

		  if (e->flags & EDGE_DFS_BACK)
		    /* The condition_out from the other edge will already
		       have the loop condition initializer.  */
		    continue;

		  cond_region **c = forks.get (e->src->index);
		  rtx newcond;

		  if (c)
		    newcond = ((e->flags & EDGE_FALLTHRU)
			       ? (*c)->false_branch_condvar
			       : (*c)->true_branch_condvar);
		  else
		    newcond = output_bb_condmasks[e->src->index];
		  if (output_bb_condmasks[bb->index] == newcond)
		    ;
		  else if (!output_bb_condmasks[bb->index])
		    output_bb_condmasks[bb->index] = newcond;
		  else
		    {
		      while (GET_CODE (newcond) == IOR)
			{
			  rtx reg = XEXP (newcond, 0);
			  gcc_assert (REG_P (reg));
			  output_bb_condmasks[bb->index] =
			    gen_rtx_IOR (DImode, reg,
					 output_bb_condmasks[bb->index]);
			  newcond = XEXP (newcond, 1);
			}
		      gcc_assert (REG_P (newcond));
		      output_bb_condmasks[bb->index] =
			gen_rtx_IOR (DImode, newcond,
				     output_bb_condmasks[bb->index]);
		    }
		}
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "\n\nFinal ordering and masks:\n");
      for (int num : output_bb_seq)
	{
	  fprintf (dump_file, "BB %d\n", num);

	  fprintf (dump_file, "  condition in: ");
	  dump_value_slim (dump_file, output_bb_condmasks[num], 1);
	  fprintf (dump_file, "\n");

	  loop_region **lp = loopcallers.get (num);
	  if (lp)
	    {
	      loop_region *loop = *lp;
	      for (auto condvar : loop->exit_conditions)
		{
		  fprintf (dump_file, "  init exit condvar: ");
		  dump_value_slim (dump_file, condvar.second, 1);
		  fprintf (dump_file, " (for loop at bb %d, exit at bb %d)\n",
			   loop->start->index, condvar.first);
		}
	    }

	  cond_region **cp = forks.get (num);
	  if (cp)
	    {
	      cond_region *c = *cp;
	      fprintf (dump_file, "  write condition var (false branch): ");
	      dump_value_slim (dump_file, c->false_branch_condvar, 1);
	      if (c->false_is_loop_exit)
		fprintf (dump_file, " (loop exit condition)");
	      fprintf (dump_file, "\n");
	      fprintf (dump_file, "  write condition var (true branch):  ");
	      dump_value_slim (dump_file, c->true_branch_condvar, 1);
	      if (c->true_is_loop_exit)
		fprintf (dump_file, " (loop exit condition)");
	      fprintf (dump_file, "\n");
	    }
	}
    }
}

/* Do the SIMT CFG transformations, according to OUTPUT_BB_SEQ.

   At the end, the CFG has been linearized, the old branch instructions
   have been removed, the mask handling insns have been inserted, and
   the CUTS table has been initialized, ready for plan_short_cut_branches.  */

void
simt_cbranch_state::transform_cfg ()
{
  if (dump_file)
    fprintf (dump_file, "\n\nTransforming the CFG....\n");

  /* Build a linear sequence of blocks, keeping only back-edges from
     the existing CFG.  */
  basic_block bb, prev_bb = BASIC_BLOCK_FOR_FN (fn, 0);
  for (int num : output_bb_seq)
    {
      if (num == 0)
	continue;

      bb = BASIC_BLOCK_FOR_FN (fn, num);
      edge e;
      edge_iterator ei;
      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
	{
	  /* Back edges are for loops and remain unchanged.  */
	  if (e->flags & (EDGE_DFS_BACK | EDGE_FAKE))
	    {
	      ei_next (&ei);
	      continue;
	    }

	  /* Delete the edge.  */
	  remove_edge (e);
	}

      if (/* Don't overwrite the flags if the edge already exists.  */
	  !find_edge (prev_bb, bb))
	make_edge (prev_bb, bb, EDGE_FALLTHRU);

      prev_bb = bb;
    }

  /* Remove EDGE_FALLTHRU flags from any loop-back edges that were previously
     unconditional; all should now be branches off a single flow.  */
  FOR_EACH_BB_FN (bb, fn)
    {
      for (edge e : bb->succs)
	if (e->flags & (EDGE_DFS_BACK | EDGE_FAKE))
	  e->flags &= ~EDGE_FALLTHRU;
    }

  /* This indicates that a BB writes to a condition variable, which will
     force the next BB to reset EXEC on entry.  */
  auto_bitmap exec_invalidated;

  /* Replace redundant cjump insns with the condition variables.  */
  for (auto c : conditionals)
    {
      auto cjump = as_a <rtx_jump_insn*> (BB_END (c->start));
      rtx oldcondop = XEXP (SET_SRC (pc_set (cjump)), 0);
      rtx oldcondreg = XEXP (oldcondop, 0);
      rtx oldcondval = XEXP (oldcondop, 1);

      if (!REG_P (oldcondreg))
	oldcondreg = force_reg (GET_MODE (oldcondreg), XEXP (oldcondop, 0));

      /* gcn.md defines cjump relative to zero, although the ISA
	 doesn't actually require that.  If that ever changes, the
	 assumptions here are invalid.  */
      gcc_checking_assert (CONST_INT_P (oldcondval)
			   && INTVAL (oldcondval) == 0);
      gcc_checking_assert (GET_CODE (oldcondop) == NE
			   || GET_CODE (oldcondop) == EQ);

      start_sequence ();
      rtx execreg = gen_rtx_REG (DImode, EXEC_REG);

      /* TODO: Consider tracking BImode and DImode conditions separately?
	       Keeping BImode values as BImode might be better in some ways,
	       but it will need special handling throughout.  BImode can
	       only happen when the write is known to be unconditional, right?
	       If so this might be a useful microoptimization.  */
      if (GET_MODE (oldcondreg) == BImode)
	{
	  rtx direg = gen_reg_rtx (DImode);
	  emit_insn (gen_cselectdi (direg, oldcondreg, execreg, const0_rtx));
	  oldcondreg = direg;
	}

      /* FIXME This whole sequence can be optimized using s_andn2 including
	       the SCC output for the branch that likely follows.  */
      rtx inversemask = gen_reg_rtx (DImode);
      emit_insn (gen_one_cmpldi2 (inversemask, oldcondreg));
      emit_insn (gen_anddi3 (inversemask, inversemask, execreg));

      /* The bits in oldcondreg are correct for NE comparisons, and inverted
	 for EQ comparisons.  Swap the masks here, accordingly.  */
      rtx mask = oldcondreg;
      if (GET_CODE (oldcondop) == EQ)
	{
	  mask = inversemask;
	  inversemask = oldcondreg;
	}

      /* Branch exit conditions are cumulative as they should be executed
	 once after the loop has finished looping.
	 Regular condition variables are overwritten each time they are
	 encountered.  */
      if (c->true_is_loop_exit)
	emit_insn (gen_iordi3 (c->true_branch_condvar,
			       c->true_branch_condvar, mask));
      else
	emit_move_insn (c->true_branch_condvar, mask);

      if (c->false_is_loop_exit)
	emit_insn (gen_iordi3 (c->false_branch_condvar,
			       c->false_branch_condvar, inversemask));
      else
	emit_move_insn (c->false_branch_condvar, inversemask);

      emit_insn_before (end_sequence (), cjump);
      delete_insn (cjump);

      bitmap_set_bit (exec_invalidated, c->start->index);
    }

  /* Insert the loop exit condition initializers.  */
  for (auto loop : loops)
    {
      start_sequence ();
      emit_insn (gen_exec_barrier ());

      /* Initialize the cumulative exit masks for each exit point.  */
      for (auto condvar : loop->exit_conditions)
	emit_move_insn (condvar.second, const0_rtx);

      emit_insn (gen_exec_barrier ());
      emit_insn_after (end_sequence (), BB_END (loop->caller));

      bitmap_set_bit (exec_invalidated, loop->caller->index);
    }

  /* Delete empty BBs.  These come from when we simplified the dominator
     relationships tree.  */
  basic_block next_bb = NULL;
  for (bb = fn->cfg->x_entry_block_ptr->next_bb;
       bb->index != EXIT_BLOCK;
       bb = next_bb)
    {
      next_bb = bb->next_bb;

      /* Is the block empty?  */
      if (!NOTE_INSN_BASIC_BLOCK_P (BB_END (bb)))
	continue;

      /* Is this a block that will have a branch written?  */
      if (EDGE_COUNT (bb->preds) != 1
	  || EDGE_COUNT (bb->succs) != 1)
	continue;

      if (dump_file)
	fprintf (dump_file, "Cleaning up empty bb %d\n", bb->index);

      /* The block can be deleted.  */
      edge e_incoming = EDGE_PRED (bb, 0);
      edge e_outgoing = EDGE_SUCC (bb, 0);
      redirect_edge_succ (e_incoming, e_outgoing->dest);
      e_incoming->flags = e_outgoing->flags;

      delete_basic_block (bb);
    }

  /* Ensure that there's a block to set the exit mask.
     (Unless the function has an infinite loop, or otherwise no path
     to EXIT_BLOCK.)  */
  bb = BASIC_BLOCK_FOR_FN (fn, EXIT_BLOCK);
  if (EDGE_COUNT (bb->preds) > 0
      && (output_bb_condmasks[bb->index]
	  != output_bb_condmasks[EDGE_PRED (bb, 0)->src->index]))
    {
      basic_block newbb = split_edge (EDGE_PRED (bb, 0));
      if (output_bb_condmasks.length () < (unsigned) newbb->index + 1)
        output_bb_condmasks.safe_grow_cleared (newbb->index + 1);
      output_bb_condmasks[newbb->index] = output_bb_condmasks[bb->index];
    }

  /* Insert the output_bb_condmasks variables.  */
  FOR_EACH_BB_FN (bb, fn)
    {
      gcc_assert (output_bb_condmasks[bb->index]);

      if (REG_P (output_bb_condmasks[bb->index])
	  && REGNO (output_bb_condmasks[bb->index]) == EXEC_REG)
	continue;

      bool cond_already_set = true;
      for (edge e : bb->preds)
	{
	  if (bitmap_bit_p (exec_invalidated, e->src->index)
	      || (output_bb_condmasks[e->src->index]
		  != output_bb_condmasks[bb->index]))
	    cond_already_set = false;
	}

      if (cond_already_set)
	continue;

      short_cut_plan cut;
      cut.src = bb;

      start_sequence ();
      emit_insn (gen_exec_barrier ());
      /* TODO memoize the force_operand??  */
      rtx mask = force_operand (output_bb_condmasks[bb->index], NULL);
      cut.setexec_insn = emit_insn (gen_setexec (mask));
      emit_insn (gen_exec_barrier ());
      emit_insn_at_bb_head (end_sequence (), bb);

      cuts.safe_push (cut);
    }
}

/* Plan the short-cut branches.

   The CUTS table has been initialized already, with the SRC block and
   SETEXEC_INSN fields, in transform_cfg (because it was convenient to do so).
   Populate the rest of the fields now.  */

void
simt_cbranch_state::plan_short_cut_branches ()
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nPlanning short-cut branches ...\n");

  /* Plan the short-cut edges, for when all bits in the execution mask are
     zero.  This shorter code-path is an optimization, but it also functions
     as an inhibitor for later compiler passes that aren't aware that the
     blocks are masked and try to "optimize" apparently duplicate code.

     We already identified the starting points, but we need to find the
     end points.  In the simplest case, in which the start BB uses a different
     output_bb_condmasks to the only successor BB and there are no writes to
     condition variables, we simply jump to the successor BB.

     If the successor BB uses the same condition, or a subset-condition, then
     we can skip both BBs, as so on.

     If the short-cut jump would skip writing to a condition variable then
     we know that that condition would also be zero.  */
  for (auto& cut : cuts)
    {
      basic_block bb = cut.src;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
		   "Considering short-cut branch starting before bb %d\n"
		   "  cvars:",
		   bb->index);
	}

      /* In the case that the short-cut branch is taken, all the components
	 of the output_bb_condmasks mask must have been zero.
	 Make a note of this now so we can track it later.
         output_bb_condmasks looks like (ior reg (ior reg reg))  */
      hash_set<rtx> known_zero;
      rtx x = output_bb_condmasks[bb->index];
      while (x)
	{
	  rtx reg = x;
	  if (GET_CODE (x) == IOR)
	    {
	      reg = XEXP (x, 0);
	      x = XEXP (x, 1);
	    }
	  else
	    x = NULL;

	  gcc_assert (REG_P (reg));
	  known_zero.add (reg);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " ");
	      dump_value_slim (dump_file, reg, 1);
	    }
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\n");

      /* Walk the fallthrough blocks starting at cut.src to determine how
	 skipping it will affect future decisions, and to see how many blocks
	 we can safely short-cut, in the case the mask is all-zeros.  */
      while (1)
	{
	  /* We can't short-cut to the function exit, and loop back edges
	     should already have been taken care of, so we're done walking.  */
	  if (EDGE_COUNT (bb->succs) == 0)
	    {
	      cut.dest = cut.src;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  Reached end of function;"
			 " should never need short cut\n");
	      break;
	    }
	  if (fallthru_edge (bb)->flags & (EDGE_FAKE | EDGE_DFS_BACK))
	    {
	      cut.dest = bb;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  No onward route; short cut ends at bb %d\n",
			 bb->index);
	      break;
	    }
	  basic_block next_bb = fallthru_edge (bb)->dest;

	  /* Skipping a block that writes to a condition mask that is then
	     used later would be an error.  However, writing to a mask when
	     no lanes are active is guaranteed to write zero, so we just
	     ensure that this happens on both main branch and short-cut branch.
	     If it ends up redundant the write should be optimized away in
	     a later pass.  */
	  cond_region **c = forks.get (bb->index);
	  if (c)
	    {
	      if (!(*c)->true_is_loop_exit)
		{
		  cut.skipped_cvars.add ((*c)->true_branch_condvar);
		  known_zero.add ((*c)->true_branch_condvar);
		}
	      if (!(*c)->false_is_loop_exit)
		{
		  cut.skipped_cvars.add ((*c)->false_branch_condvar);
		  known_zero.add ((*c)->false_branch_condvar);
		}

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "  Bypassed cvars: ");
		  dump_value_slim (dump_file, (*c)->true_branch_condvar, 1);
		  fprintf (dump_file, " ");
		  dump_value_slim (dump_file, (*c)->false_branch_condvar, 1);
		  fprintf (dump_file, "\n");
		}
	    }

	  loop_region **l = loopcallers.get (bb->index);
	  if (l)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Bypassed loop exit cvars: ");
	      for (auto condvar : (*l)->exit_conditions)
		{
		  cut.skipped_cvars.add (condvar.second);
		  known_zero.add (condvar.second);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, " ");
		      dump_value_slim (dump_file, condvar.second, 1);
		    }
		}
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "\n");
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Check conditions for next bb %d\n",
		       next_bb->index);
	    }

	  /* Check if the condition mask on the next block will also be zero,
	     or not known.  If zero we will loop and short-cut that block
	     also.
	     output_bb_condmasks looks like (ior reg (ior reg reg))  */
	  bool confirmed_zero = true;
	  x = output_bb_condmasks[next_bb->index];
	  while (x)
	    {
	      rtx reg = x;
	      if (GET_CODE (x) == IOR)
		{
		  reg = XEXP (x, 0);
		  x = XEXP (x, 1);
		}
	      else
		x = NULL;

	      gcc_assert (REG_P (reg));
	      if (REGNO (reg) != EXEC_REG
		  && !known_zero.contains (reg))
		{
		  confirmed_zero = false;

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "    Possible non-zero cvar: ");
		      dump_value_slim (dump_file, reg, 1);
		      fprintf (dump_file, "\n");
		    }
		  break;
		}

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "    Cvar would be zero: ");
		  dump_value_slim (dump_file, reg, 1);
		  fprintf (dump_file, "\n");
		}
	    }
	  if (!confirmed_zero)
	    {
	      cut.dest = next_bb;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  -> short-cut ends at bb %d\n",
			 next_bb->index);
	      break;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  -> short-cut can continue past bb %d\n",
		     next_bb->index);

	  bb = next_bb;
	}
    }
}

/* Insert the short-cut branches planning in CUTS, and clean up the CFG. */

void
simt_cbranch_state::insert_short_cut_branches ()
{
  if (dump_file)
    fprintf (dump_file, "\nInserting short-cut branches ...\n");

  /* Insert the short-cut edges.  */
  for (auto& cut : cuts)
    {
      if (cut.src == cut.dest)
	continue;

      rtx condvar = SET_SRC (XVECEXP (PATTERN (cut.setexec_insn), 0, 0));

      /* Insert the short-cut route into the CFG.  */
      split_block (cut.src, PREV_INSN (cut.setexec_insn));
      edge new_shortcut = make_edge (cut.src, cut.dest, 0);

      /* Add the short-cut branch insn.
	 It would be fewer instructions to use EXECZ, but once the mask
	 has become zero it might be difficult to handle scalars safely.  */
      rtx shortcut_label = block_label (cut.dest);
      start_sequence ();

      emit_insn (gen_cbranchdi4 (gen_rtx_EQ (DImode, NULL, NULL),
				 condvar, const0_rtx, shortcut_label));
      emit_insn_after (end_sequence (), BB_END (cut.src));
      JUMP_LABEL (BB_END (cut.src)) = shortcut_label;

      /* Add any mask management to the short-cut branch.  */
      if (!cut.skipped_cvars.is_empty ())
	{
	  basic_block shortcut_block = split_edge (new_shortcut);
	  start_sequence ();
	  for (rtx x : cut.skipped_cvars)
	    emit_move_insn (x, const0_rtx);
	  emit_insn (gen_exec_barrier ());
	  emit_insn_after (end_sequence (), BB_END (shortcut_block));
	}
    }

  /* Remove now-redundant fallthru edges at the loop-back point, if needed.  */
  for (auto loop : loops)
    {
      basic_block final_bb = NULL;
      edge back_edge = NULL;

      for (auto e : loop->start->preds)
	if (e->flags & EDGE_DFS_BACK)
	  {
	    back_edge = e;
	    final_bb = e->src;
	    break;
	  }
      if (!final_bb)
	/* All edges were back-edges.  */
	continue;

      rtx_insn *final_insn = BB_END (final_bb);
      gcc_checking_assert (!any_condjump_p (final_insn));

      if (EDGE_COUNT (final_bb->succs) == 2)
	{
	  edge other_edge = fallthru_edge (final_bb);
	  if (other_edge == back_edge)
	    other_edge = BRANCH_EDGE (final_bb);

	  remove_edge (other_edge);
	}
      back_edge->flags &= ~EDGE_FALLTHRU;

      if (!JUMP_P (final_insn))
	{
	  emit_jump_insn_after (gen_jump (block_label (loop->start)),
				final_insn);
	  emit_barrier_after_bb (final_bb);
	}
    }

  /* Remove now-redundant edges from no-exit blocks.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      /* Find blocks that have a fake edge; these are the no-exit blocks.  */
      for (edge e : bb->succs)
	if (e->flags & EDGE_FAKE)
	  {
	    /* Delete both the fallthru and the fake edge.  */
	    if (EDGE_COUNT (bb->succs) > 1)
	      remove_edge (EDGE_SUCC (bb, 1));
	    remove_edge (EDGE_SUCC (bb, 0));
	    break;
	  }
    }
}

/* If the function has SIMT_ENTER or SIMT_EXIT statements, ensure that
   the condition masks are converted from single-thread to full SIMT-mode
   at the enter transition point, and vice-versa at the exit point.

   E.g. a comparison in single-thread mode will have set the condition
        variable to 0x1, and the SIMT_ENTER statement must broadcast that
        to all the threads by changing the mask to 0xffffffff.

   At the exit point, we absolutely do not want to retain any mask bits
   greater than 0x1, but it's less clear how to reduce the mask to one bit.
   If the mask is all ones then it's clear that the new 1-bit mask should
   be also one, but if only some lanes are enabled then how should it reduce?
   For now, this implementation just keeps "1" if any threads has "1".  */

void
simt_cbranch_state::handle_simt_enter_exit ()
{
  if (dump_file)
    fprintf (dump_file, "\nHandling simt_enter/exit insns ...\n");

  basic_block bb;
  FOR_EACH_BB_FN (bb, fn)
    {
      rtx_insn *insn;
      FOR_BB_INSNS (bb, insn)
	{
	  if (!NONDEBUG_INSN_P (insn)
	      || (INSN_CODE (insn) != CODE_FOR_omp_simt_enter
		  && INSN_CODE (insn) != CODE_FOR_omp_simt_exit))
	    continue;

	  rtx newval = (INSN_CODE (insn) == CODE_FOR_omp_simt_enter
			? constm1_rtx : const1_rtx);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Found %s at bb %d insn %d\n",
		     get_insn_name (INSN_CODE (insn)), bb->index,
		     INSN_UID (insn));

	  for (auto c : conditionals)
	    {
	      start_sequence ();

	      rtx scc1 = gen_reg_rtx (BImode);
	      emit_insn (gen_cstoredi4 (scc1, gen_rtx_NE (BImode, 0, 0),
					c->true_branch_condvar, const0_rtx));
	      emit_insn (gen_cselectdi (c->true_branch_condvar, scc1,
					newval, const0_rtx));

	      rtx scc2 = gen_reg_rtx (BImode);
	      emit_insn (gen_cstoredi4 (scc2, gen_rtx_NE (BImode, 0, 0),
					c->false_branch_condvar, const0_rtx));
	      emit_insn (gen_cselectdi (c->false_branch_condvar, scc2,
					newval, const0_rtx));

	      emit_insn_after (end_sequence (), insn);
	    }
	}
    }
}

unsigned int
pass_gcn_simt_cbranch::execute (function *fn)
{
  if (reject_unsupported_edges (fn))
    return 0;

  /* Insert some extra blocks and edges to make the dependencies clearer.  */
  work_around_noreturn_blocks (fn);
  separate_forks_from_joins (fn);
  unify_joining_back_edges (fn);
  simplify_join_blocks (fn);
  separate_forking_blocks (fn);

  /* Do the real work of the pass.  */
  simt_cbranch_state pass_state (fn);
  pass_state.execute ();

  return 0;
}
