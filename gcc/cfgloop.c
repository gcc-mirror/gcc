/* Natural loop discovery code for GNU compiler.
   Copyright (C) 2000-2020 Free Software Foundation, Inc.

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "gimple-ssa.h"
#include "diagnostic-core.h"
#include "cfganal.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "dumpfile.h"

static void flow_loops_cfg_dump (FILE *);

/* Dump loop related CFG information.  */

static void
flow_loops_cfg_dump (FILE *file)
{
  basic_block bb;

  if (!file)
    return;

  FOR_EACH_BB_FN (bb, cfun)
    {
      edge succ;
      edge_iterator ei;

      fprintf (file, ";; %d succs { ", bb->index);
      FOR_EACH_EDGE (succ, ei, bb->succs)
	fprintf (file, "%d ", succ->dest->index);
      fprintf (file, "}\n");
    }
}

/* Return nonzero if the nodes of LOOP are a subset of OUTER.  */

bool
flow_loop_nested_p (const class loop *outer, const class loop *loop)
{
  unsigned odepth = loop_depth (outer);

  return (loop_depth (loop) > odepth
	  && (*loop->superloops)[odepth] == outer);
}

/* Returns the loop such that LOOP is nested DEPTH (indexed from zero)
   loops within LOOP.  */

class loop *
superloop_at_depth (class loop *loop, unsigned depth)
{
  unsigned ldepth = loop_depth (loop);

  gcc_assert (depth <= ldepth);

  if (depth == ldepth)
    return loop;

  return (*loop->superloops)[depth];
}

/* Returns the list of the latch edges of LOOP.  */

static vec<edge> 
get_loop_latch_edges (const class loop *loop)
{
  edge_iterator ei;
  edge e;
  vec<edge> ret = vNULL;

  FOR_EACH_EDGE (e, ei, loop->header->preds)
    {
      if (dominated_by_p (CDI_DOMINATORS, e->src, loop->header))
	ret.safe_push (e);
    }

  return ret;
}

/* Dump the loop information specified by LOOP to the stream FILE
   using auxiliary dump callback function LOOP_DUMP_AUX if non null.  */

void
flow_loop_dump (const class loop *loop, FILE *file,
		void (*loop_dump_aux) (const class loop *, FILE *, int),
		int verbose)
{
  basic_block *bbs;
  unsigned i;
  vec<edge> latches;
  edge e;

  if (! loop || ! loop->header)
    return;

  fprintf (file, ";;\n;; Loop %d\n", loop->num);

  fprintf (file, ";;  header %d, ", loop->header->index);
  if (loop->latch)
    fprintf (file, "latch %d\n", loop->latch->index);
  else
    {
      fprintf (file, "multiple latches:");
      latches = get_loop_latch_edges (loop);
      FOR_EACH_VEC_ELT (latches, i, e)
	fprintf (file, " %d", e->src->index);
      latches.release ();
      fprintf (file, "\n");
    }

  fprintf (file, ";;  depth %d, outer %ld\n",
	   loop_depth (loop), (long) (loop_outer (loop)
				      ? loop_outer (loop)->num : -1));

  if (loop->latch)
    {
      bool read_profile_p;
      gcov_type nit = expected_loop_iterations_unbounded (loop, &read_profile_p);
      if (read_profile_p && !loop->any_estimate)
	fprintf (file, ";;  profile-based iteration count: %" PRIu64 "\n",
		 (uint64_t) nit);
    }

  fprintf (file, ";;  nodes:");
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    fprintf (file, " %d", bbs[i]->index);
  free (bbs);
  fprintf (file, "\n");

  if (loop_dump_aux)
    loop_dump_aux (loop, file, verbose);
}

/* Dump the loop information about loops to the stream FILE,
   using auxiliary dump callback function LOOP_DUMP_AUX if non null.  */

void
flow_loops_dump (FILE *file, void (*loop_dump_aux) (const class loop *, FILE *, int), int verbose)
{
  class loop *loop;

  if (!current_loops || ! file)
    return;

  fprintf (file, ";; %d loops found\n", number_of_loops (cfun));

  FOR_EACH_LOOP (loop, LI_INCLUDE_ROOT)
    {
      flow_loop_dump (loop, file, loop_dump_aux, verbose);
    }

  if (verbose)
    flow_loops_cfg_dump (file);
}

/* Free data allocated for LOOP.  */

void
flow_loop_free (class loop *loop)
{
  struct loop_exit *exit, *next;

  vec_free (loop->superloops);

  /* Break the list of the loop exit records.  They will be freed when the
     corresponding edge is rescanned or removed, and this avoids
     accessing the (already released) head of the list stored in the
     loop structure.  */
  for (exit = loop->exits->next; exit != loop->exits; exit = next)
    {
      next = exit->next;
      exit->next = exit;
      exit->prev = exit;
    }

  ggc_free (loop->exits);
  ggc_free (loop);
}

/* Free all the memory allocated for LOOPS.  */

void
flow_loops_free (struct loops *loops)
{
  if (loops->larray)
    {
      unsigned i;
      loop_p loop;

      /* Free the loop descriptors.  */
      FOR_EACH_VEC_SAFE_ELT (loops->larray, i, loop)
	{
	  if (!loop)
	    continue;

	  flow_loop_free (loop);
	}

      vec_free (loops->larray);
    }
}

/* Find the nodes contained within the LOOP with header HEADER.
   Return the number of nodes within the loop.  */

int
flow_loop_nodes_find (basic_block header, class loop *loop)
{
  vec<basic_block> stack = vNULL;
  int num_nodes = 1;
  edge latch;
  edge_iterator latch_ei;

  header->loop_father = loop;

  FOR_EACH_EDGE (latch, latch_ei, loop->header->preds)
    {
      if (latch->src->loop_father == loop
	  || !dominated_by_p (CDI_DOMINATORS, latch->src, loop->header))
	continue;

      num_nodes++;
      stack.safe_push (latch->src);
      latch->src->loop_father = loop;

      while (!stack.is_empty ())
	{
	  basic_block node;
	  edge e;
	  edge_iterator ei;

	  node = stack.pop ();

	  FOR_EACH_EDGE (e, ei, node->preds)
	    {
	      basic_block ancestor = e->src;

	      if (ancestor->loop_father != loop)
		{
		  ancestor->loop_father = loop;
		  num_nodes++;
		  stack.safe_push (ancestor);
		}
	    }
	}
    }
  stack.release ();

  return num_nodes;
}

/* Records the vector of superloops of the loop LOOP, whose immediate
   superloop is FATHER.  */

static void
establish_preds (class loop *loop, class loop *father)
{
  loop_p ploop;
  unsigned depth = loop_depth (father) + 1;
  unsigned i;

  loop->superloops = 0;
  vec_alloc (loop->superloops, depth);
  FOR_EACH_VEC_SAFE_ELT (father->superloops, i, ploop)
    loop->superloops->quick_push (ploop);
  loop->superloops->quick_push (father);

  for (ploop = loop->inner; ploop; ploop = ploop->next)
    establish_preds (ploop, loop);
}

/* Add LOOP to the loop hierarchy tree where FATHER is father of the
   added loop.  If LOOP has some children, take care of that their
   pred field will be initialized correctly.  If AFTER is non-null
   then it's expected it's a pointer into FATHERs inner sibling
   list and LOOP is added behind AFTER, otherwise it's added in front
   of FATHERs siblings.  */

void
flow_loop_tree_node_add (class loop *father, class loop *loop,
			 class loop *after)
{
  if (after)
    {
      loop->next = after->next;
      after->next = loop;
    }
  else
    {
      loop->next = father->inner;
      father->inner = loop;
    }

  establish_preds (loop, father);
}

/* Remove LOOP from the loop hierarchy tree.  */

void
flow_loop_tree_node_remove (class loop *loop)
{
  class loop *prev, *father;

  father = loop_outer (loop);

  /* Remove loop from the list of sons.  */
  if (father->inner == loop)
    father->inner = loop->next;
  else
    {
      for (prev = father->inner; prev->next != loop; prev = prev->next)
	continue;
      prev->next = loop->next;
    }

  loop->superloops = NULL;
}

/* Allocates and returns new loop structure.  */

class loop *
alloc_loop (void)
{
  class loop *loop = ggc_cleared_alloc<class loop> ();

  loop->exits = ggc_cleared_alloc<loop_exit> ();
  loop->exits->next = loop->exits->prev = loop->exits;
  loop->can_be_parallel = false;
  loop->constraints = 0;
  loop->nb_iterations_upper_bound = 0;
  loop->nb_iterations_likely_upper_bound = 0;
  loop->nb_iterations_estimate = 0;
  return loop;
}

/* Initializes loops structure LOOPS, reserving place for NUM_LOOPS loops
   (including the root of the loop tree).  */

void
init_loops_structure (struct function *fn,
		      struct loops *loops, unsigned num_loops)
{
  class loop *root;

  memset (loops, 0, sizeof *loops);
  vec_alloc (loops->larray, num_loops);

  /* Dummy loop containing whole function.  */
  root = alloc_loop ();
  root->num_nodes = n_basic_blocks_for_fn (fn);
  root->latch = EXIT_BLOCK_PTR_FOR_FN (fn);
  root->header = ENTRY_BLOCK_PTR_FOR_FN (fn);
  ENTRY_BLOCK_PTR_FOR_FN (fn)->loop_father = root;
  EXIT_BLOCK_PTR_FOR_FN (fn)->loop_father = root;

  loops->larray->quick_push (root);
  loops->tree_root = root;
}

/* Returns whether HEADER is a loop header.  */

bool
bb_loop_header_p (basic_block header)
{
  edge_iterator ei;
  edge e;

  /* If we have an abnormal predecessor, do not consider the
     loop (not worth the problems).  */
  if (bb_has_abnormal_pred (header))
    return false;

  /* Look for back edges where a predecessor is dominated
     by this block.  A natural loop has a single entry
     node (header) that dominates all the nodes in the
     loop.  It also has single back edge to the header
     from a latch node.  */
  FOR_EACH_EDGE (e, ei, header->preds)
    {
      basic_block latch = e->src;
      if (latch != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	  && dominated_by_p (CDI_DOMINATORS, latch, header))
	return true;
    }

  return false;
}

/* Find all the natural loops in the function and save in LOOPS structure and
   recalculate loop_father information in basic block structures.
   If LOOPS is non-NULL then the loop structures for already recorded loops
   will be re-used and their number will not change.  We assume that no
   stale loops exist in LOOPS.
   When LOOPS is NULL it is allocated and re-built from scratch.
   Return the built LOOPS structure.  */

struct loops *
flow_loops_find (struct loops *loops)
{
  bool from_scratch = (loops == NULL);
  int *rc_order;
  int b;
  unsigned i;

  /* Ensure that the dominators are computed.  */
  calculate_dominance_info (CDI_DOMINATORS);

  if (!loops)
    {
      loops = ggc_cleared_alloc<struct loops> ();
      init_loops_structure (cfun, loops, 1);
    }

  /* Ensure that loop exits were released.  */
  gcc_assert (loops->exits == NULL);

  /* Taking care of this degenerate case makes the rest of
     this code simpler.  */
  if (n_basic_blocks_for_fn (cfun) == NUM_FIXED_BLOCKS)
    return loops;

  /* The root loop node contains all basic-blocks.  */
  loops->tree_root->num_nodes = n_basic_blocks_for_fn (cfun);

  /* Compute depth first search order of the CFG so that outer
     natural loops will be found before inner natural loops.  */
  rc_order = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  pre_and_rev_post_order_compute (NULL, rc_order, false);

  /* Gather all loop headers in reverse completion order and allocate
     loop structures for loops that are not already present.  */
  auto_vec<loop_p> larray (loops->larray->length ());
  for (b = 0; b < n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS; b++)
    {
      basic_block header = BASIC_BLOCK_FOR_FN (cfun, rc_order[b]);
      if (bb_loop_header_p (header))
	{
	  class loop *loop;

	  /* The current active loop tree has valid loop-fathers for
	     header blocks.  */
	  if (!from_scratch
	      && header->loop_father->header == header)
	    {
	      loop = header->loop_father;
	      /* If we found an existing loop remove it from the
		 loop tree.  It is going to be inserted again
		 below.  */
	      flow_loop_tree_node_remove (loop);
	    }
	  else
	    {
	      /* Otherwise allocate a new loop structure for the loop.  */
	      loop = alloc_loop ();
	      /* ???  We could re-use unused loop slots here.  */
	      loop->num = loops->larray->length ();
	      vec_safe_push (loops->larray, loop);
	      loop->header = header;

	      if (!from_scratch
		  && dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "flow_loops_find: discovered new "
			 "loop %d with header %d\n",
			 loop->num, header->index);
	    }
	  /* Reset latch, we recompute it below.  */
	  loop->latch = NULL;
	  larray.safe_push (loop);
	}

      /* Make blocks part of the loop root node at start.  */
      header->loop_father = loops->tree_root;
    }

  free (rc_order);

  /* Now iterate over the loops found, insert them into the loop tree
     and assign basic-block ownership.  */
  for (i = 0; i < larray.length (); ++i)
    {
      class loop *loop = larray[i];
      basic_block header = loop->header;
      edge_iterator ei;
      edge e;

      flow_loop_tree_node_add (header->loop_father, loop);
      loop->num_nodes = flow_loop_nodes_find (loop->header, loop);

      /* Look for the latch for this header block, if it has just a
	 single one.  */
      FOR_EACH_EDGE (e, ei, header->preds)
	{
	  basic_block latch = e->src;

	  if (flow_bb_inside_loop_p (loop, latch))
	    {
	      if (loop->latch != NULL)
		{
		  /* More than one latch edge.  */
		  loop->latch = NULL;
		  break;
		}
	      loop->latch = latch;
	    }
	}
    }

  return loops;
}

/* qsort helper for sort_sibling_loops.  */

static int *sort_sibling_loops_cmp_rpo;
static int
sort_sibling_loops_cmp (const void *la_, const void *lb_)
{
  const class loop *la = *(const class loop * const *)la_;
  const class loop *lb = *(const class loop * const *)lb_;
  return (sort_sibling_loops_cmp_rpo[la->header->index]
	  - sort_sibling_loops_cmp_rpo[lb->header->index]);
}

/* Sort sibling loops in RPO order.  */

void
sort_sibling_loops (function *fn)
{
  /* Match flow_loops_find in the order we sort sibling loops.  */
  sort_sibling_loops_cmp_rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));
  int *rc_order = XNEWVEC (int, n_basic_blocks_for_fn (cfun));
  pre_and_rev_post_order_compute_fn (fn, NULL, rc_order, false);
  for (int i = 0; i < n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS; ++i)
    sort_sibling_loops_cmp_rpo[rc_order[i]] = i;
  free (rc_order);

  auto_vec<loop_p, 3> siblings;
  loop_p loop;
  FOR_EACH_LOOP_FN (fn, loop, LI_INCLUDE_ROOT)
    if (loop->inner && loop->inner->next)
      {
	loop_p sibling = loop->inner;
	do
	  {
	    siblings.safe_push (sibling);
	    sibling = sibling->next;
	  }
	while (sibling);
	siblings.qsort (sort_sibling_loops_cmp);
	loop_p *siblingp = &loop->inner;
	for (unsigned i = 0; i < siblings.length (); ++i)
	  {
	    *siblingp = siblings[i];
	    siblingp = &(*siblingp)->next;
	  }
	*siblingp = NULL;
	siblings.truncate (0);
      }

  free (sort_sibling_loops_cmp_rpo);
  sort_sibling_loops_cmp_rpo = NULL;
}

/* Ratio of frequencies of edges so that one of more latch edges is
   considered to belong to inner loop with same header.  */
#define HEAVY_EDGE_RATIO 8

/* Minimum number of samples for that we apply
   find_subloop_latch_edge_by_profile heuristics.  */
#define HEAVY_EDGE_MIN_SAMPLES 10

/* If the profile info is available, finds an edge in LATCHES that much more
   frequent than the remaining edges.  Returns such an edge, or NULL if we do
   not find one.

   We do not use guessed profile here, only the measured one.  The guessed
   profile is usually too flat and unreliable for this (and it is mostly based
   on the loop structure of the program, so it does not make much sense to
   derive the loop structure from it).  */

static edge
find_subloop_latch_edge_by_profile (vec<edge> latches)
{
  unsigned i;
  edge e, me = NULL;
  profile_count mcount = profile_count::zero (), tcount = profile_count::zero ();

  FOR_EACH_VEC_ELT (latches, i, e)
    {
      if (e->count ()> mcount)
	{
	  me = e;
	  mcount = e->count();
	}
      tcount += e->count();
    }

  if (!tcount.initialized_p () || !(tcount.ipa () > HEAVY_EDGE_MIN_SAMPLES)
      || (tcount - mcount).apply_scale (HEAVY_EDGE_RATIO, 1) > tcount)
    return NULL;

  if (dump_file)
    fprintf (dump_file,
	     "Found latch edge %d -> %d using profile information.\n",
	     me->src->index, me->dest->index);
  return me;
}

/* Among LATCHES, guesses a latch edge of LOOP corresponding to subloop, based
   on the structure of induction variables.  Returns this edge, or NULL if we
   do not find any.

   We are quite conservative, and look just for an obvious simple innermost
   loop (which is the case where we would lose the most performance by not
   disambiguating the loop).  More precisely, we look for the following
   situation: The source of the chosen latch edge dominates sources of all
   the other latch edges.  Additionally, the header does not contain a phi node
   such that the argument from the chosen edge is equal to the argument from
   another edge.  */

static edge
find_subloop_latch_edge_by_ivs (class loop *loop ATTRIBUTE_UNUSED, vec<edge> latches)
{
  edge e, latch = latches[0];
  unsigned i;
  gphi *phi;
  gphi_iterator psi;
  tree lop;
  basic_block bb;

  /* Find the candidate for the latch edge.  */
  for (i = 1; latches.iterate (i, &e); i++)
    if (dominated_by_p (CDI_DOMINATORS, latch->src, e->src))
      latch = e;

  /* Verify that it dominates all the latch edges.  */
  FOR_EACH_VEC_ELT (latches, i, e)
    if (!dominated_by_p (CDI_DOMINATORS, e->src, latch->src))
      return NULL;

  /* Check for a phi node that would deny that this is a latch edge of
     a subloop.  */
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();
      lop = PHI_ARG_DEF_FROM_EDGE (phi, latch);

      /* Ignore the values that are not changed inside the subloop.  */
      if (TREE_CODE (lop) != SSA_NAME
	  || SSA_NAME_DEF_STMT (lop) == phi)
	continue;
      bb = gimple_bb (SSA_NAME_DEF_STMT (lop));
      if (!bb || !flow_bb_inside_loop_p (loop, bb))
	continue;

      FOR_EACH_VEC_ELT (latches, i, e)
	if (e != latch
	    && PHI_ARG_DEF_FROM_EDGE (phi, e) == lop)
	  return NULL;
    }

  if (dump_file)
    fprintf (dump_file,
	     "Found latch edge %d -> %d using iv structure.\n",
	     latch->src->index, latch->dest->index);
  return latch;
}

/* If we can determine that one of the several latch edges of LOOP behaves
   as a latch edge of a separate subloop, returns this edge.  Otherwise
   returns NULL.  */

static edge
find_subloop_latch_edge (class loop *loop)
{
  vec<edge> latches = get_loop_latch_edges (loop);
  edge latch = NULL;

  if (latches.length () > 1)
    {
      latch = find_subloop_latch_edge_by_profile (latches);

      if (!latch
	  /* We consider ivs to guess the latch edge only in SSA.  Perhaps we
	     should use cfghook for this, but it is hard to imagine it would
	     be useful elsewhere.  */
	  && current_ir_type () == IR_GIMPLE)
	latch = find_subloop_latch_edge_by_ivs (loop, latches);
    }

  latches.release ();
  return latch;
}

/* Callback for make_forwarder_block.  Returns true if the edge E is marked
   in the set MFB_REIS_SET.  */

static hash_set<edge> *mfb_reis_set;
static bool
mfb_redirect_edges_in_set (edge e)
{
  return mfb_reis_set->contains (e);
}

/* Creates a subloop of LOOP with latch edge LATCH.  */

static void
form_subloop (class loop *loop, edge latch)
{
  edge_iterator ei;
  edge e, new_entry;
  class loop *new_loop;

  mfb_reis_set = new hash_set<edge>;
  FOR_EACH_EDGE (e, ei, loop->header->preds)
    {
      if (e != latch)
	mfb_reis_set->add (e);
    }
  new_entry = make_forwarder_block (loop->header, mfb_redirect_edges_in_set,
				    NULL);
  delete mfb_reis_set;

  loop->header = new_entry->src;

  /* Find the blocks and subloops that belong to the new loop, and add it to
     the appropriate place in the loop tree.  */
  new_loop = alloc_loop ();
  new_loop->header = new_entry->dest;
  new_loop->latch = latch->src;
  add_loop (new_loop, loop);
}

/* Make all the latch edges of LOOP to go to a single forwarder block --
   a new latch of LOOP.  */

static void
merge_latch_edges (class loop *loop)
{
  vec<edge> latches = get_loop_latch_edges (loop);
  edge latch, e;
  unsigned i;

  gcc_assert (latches.length () > 0);

  if (latches.length () == 1)
    loop->latch = latches[0]->src;
  else
    {
      if (dump_file)
	fprintf (dump_file, "Merged latch edges of loop %d\n", loop->num);

      mfb_reis_set = new hash_set<edge>;
      FOR_EACH_VEC_ELT (latches, i, e)
	mfb_reis_set->add (e);
      latch = make_forwarder_block (loop->header, mfb_redirect_edges_in_set,
				    NULL);
      delete mfb_reis_set;

      loop->header = latch->dest;
      loop->latch = latch->src;
    }

  latches.release ();
}

/* LOOP may have several latch edges.  Transform it into (possibly several)
   loops with single latch edge.  */

static void
disambiguate_multiple_latches (class loop *loop)
{
  edge e;

  /* We eliminate the multiple latches by splitting the header to the forwarder
     block F and the rest R, and redirecting the edges.  There are two cases:

     1) If there is a latch edge E that corresponds to a subloop (we guess
        that based on profile -- if it is taken much more often than the
	remaining edges; and on trees, using the information about induction
	variables of the loops), we redirect E to R, all the remaining edges to
	F, then rescan the loops and try again for the outer loop.
     2) If there is no such edge, we redirect all latch edges to F, and the
        entry edges to R, thus making F the single latch of the loop.  */

  if (dump_file)
    fprintf (dump_file, "Disambiguating loop %d with multiple latches\n",
	     loop->num);

  /* During latch merging, we may need to redirect the entry edges to a new
     block.  This would cause problems if the entry edge was the one from the
     entry block.  To avoid having to handle this case specially, split
     such entry edge.  */
  e = find_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun), loop->header);
  if (e)
    split_edge (e);

  while (1)
    {
      e = find_subloop_latch_edge (loop);
      if (!e)
	break;

      form_subloop (loop, e);
    }

  merge_latch_edges (loop);
}

/* Split loops with multiple latch edges.  */

void
disambiguate_loops_with_multiple_latches (void)
{
  class loop *loop;

  FOR_EACH_LOOP (loop, 0)
    {
      if (!loop->latch)
	disambiguate_multiple_latches (loop);
    }
}

/* Return nonzero if basic block BB belongs to LOOP.  */
bool
flow_bb_inside_loop_p (const class loop *loop, const_basic_block bb)
{
  class loop *source_loop;

  if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return 0;

  source_loop = bb->loop_father;
  return loop == source_loop || flow_loop_nested_p (loop, source_loop);
}

/* Enumeration predicate for get_loop_body_with_size.  */
static bool
glb_enum_p (const_basic_block bb, const void *glb_loop)
{
  const class loop *const loop = (const class loop *) glb_loop;
  return (bb != loop->header
	  && dominated_by_p (CDI_DOMINATORS, bb, loop->header));
}

/* Gets basic blocks of a LOOP.  Header is the 0-th block, rest is in dfs
   order against direction of edges from latch.  Specially, if
   header != latch, latch is the 1-st block.  LOOP cannot be the fake
   loop tree root, and its size must be at most MAX_SIZE.  The blocks
   in the LOOP body are stored to BODY, and the size of the LOOP is
   returned.  */

unsigned
get_loop_body_with_size (const class loop *loop, basic_block *body,
			 unsigned max_size)
{
  return dfs_enumerate_from (loop->header, 1, glb_enum_p,
			     body, max_size, loop);
}

/* Gets basic blocks of a LOOP.  Header is the 0-th block, rest is in dfs
   order against direction of edges from latch.  Specially, if
   header != latch, latch is the 1-st block.  */

basic_block *
get_loop_body (const class loop *loop)
{
  basic_block *body, bb;
  unsigned tv = 0;

  gcc_assert (loop->num_nodes);

  body = XNEWVEC (basic_block, loop->num_nodes);

  if (loop->latch == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      /* There may be blocks unreachable from EXIT_BLOCK, hence we need to
	 special-case the fake loop that contains the whole function.  */
      gcc_assert (loop->num_nodes == (unsigned) n_basic_blocks_for_fn (cfun));
      body[tv++] = loop->header;
      body[tv++] = EXIT_BLOCK_PTR_FOR_FN (cfun);
      FOR_EACH_BB_FN (bb, cfun)
	body[tv++] = bb;
    }
  else
    tv = get_loop_body_with_size (loop, body, loop->num_nodes);

  gcc_assert (tv == loop->num_nodes);
  return body;
}

/* Fills dominance descendants inside LOOP of the basic block BB into
   array TOVISIT from index *TV.  */

static void
fill_sons_in_loop (const class loop *loop, basic_block bb,
		   basic_block *tovisit, int *tv)
{
  basic_block son, postpone = NULL;

  tovisit[(*tv)++] = bb;
  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    {
      if (!flow_bb_inside_loop_p (loop, son))
	continue;

      if (dominated_by_p (CDI_DOMINATORS, loop->latch, son))
	{
	  postpone = son;
	  continue;
	}
      fill_sons_in_loop (loop, son, tovisit, tv);
    }

  if (postpone)
    fill_sons_in_loop (loop, postpone, tovisit, tv);
}

/* Gets body of a LOOP (that must be different from the outermost loop)
   sorted by dominance relation.  Additionally, if a basic block s dominates
   the latch, then only blocks dominated by s are be after it.  */

basic_block *
get_loop_body_in_dom_order (const class loop *loop)
{
  basic_block *tovisit;
  int tv;

  gcc_assert (loop->num_nodes);

  tovisit = XNEWVEC (basic_block, loop->num_nodes);

  gcc_assert (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun));

  tv = 0;
  fill_sons_in_loop (loop, loop->header, tovisit, &tv);

  gcc_assert (tv == (int) loop->num_nodes);

  return tovisit;
}

/* Gets body of a LOOP sorted via provided BB_COMPARATOR.  */

basic_block *
get_loop_body_in_custom_order (const class loop *loop,
			       int (*bb_comparator) (const void *, const void *))
{
  basic_block *bbs = get_loop_body (loop);

  qsort (bbs, loop->num_nodes, sizeof (basic_block), bb_comparator);

  return bbs;
}

/* Same as above, but use gcc_sort_r instead of qsort.  */

basic_block *
get_loop_body_in_custom_order (const class loop *loop, void *data,
			       int (*bb_comparator) (const void *, const void *, void *))
{
  basic_block *bbs = get_loop_body (loop);

  gcc_sort_r (bbs, loop->num_nodes, sizeof (basic_block), bb_comparator, data);

  return bbs;
}

/* Get body of a LOOP in breadth first sort order.  */

basic_block *
get_loop_body_in_bfs_order (const class loop *loop)
{
  basic_block *blocks;
  basic_block bb;
  unsigned int i = 1;
  unsigned int vc = 0;

  gcc_assert (loop->num_nodes);
  gcc_assert (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun));

  blocks = XNEWVEC (basic_block, loop->num_nodes);
  auto_bitmap visited;
  blocks[0] = loop->header;
  bitmap_set_bit (visited, loop->header->index);
  while (i < loop->num_nodes)
    {
      edge e;
      edge_iterator ei;
      gcc_assert (i > vc);
      bb = blocks[vc++];

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (flow_bb_inside_loop_p (loop, e->dest))
	    {
	      /* This bb is now visited.  */
	      if (bitmap_set_bit (visited, e->dest->index))
		blocks[i++] = e->dest;
	    }
	}
    }

  return blocks;
}

/* Hash function for struct loop_exit.  */

hashval_t
loop_exit_hasher::hash (loop_exit *exit)
{
  return htab_hash_pointer (exit->e);
}

/* Equality function for struct loop_exit.  Compares with edge.  */

bool
loop_exit_hasher::equal (loop_exit *exit, edge e)
{
  return exit->e == e;
}

/* Frees the list of loop exit descriptions EX.  */

void
loop_exit_hasher::remove (loop_exit *exit)
{
  loop_exit *next;
  for (; exit; exit = next)
    {
      next = exit->next_e;

      exit->next->prev = exit->prev;
      exit->prev->next = exit->next;

      ggc_free (exit);
    }
}

/* Returns the list of records for E as an exit of a loop.  */

static struct loop_exit *
get_exit_descriptions (edge e)
{
  return current_loops->exits->find_with_hash (e, htab_hash_pointer (e));
}

/* Updates the lists of loop exits in that E appears.
   If REMOVED is true, E is being removed, and we
   just remove it from the lists of exits.
   If NEW_EDGE is true and E is not a loop exit, we
   do not try to remove it from loop exit lists.  */

void
rescan_loop_exit (edge e, bool new_edge, bool removed)
{
  struct loop_exit *exits = NULL, *exit;
  class loop *aloop, *cloop;

  if (!loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    return;

  if (!removed
      && e->src->loop_father != NULL
      && e->dest->loop_father != NULL
      && !flow_bb_inside_loop_p (e->src->loop_father, e->dest))
    {
      cloop = find_common_loop (e->src->loop_father, e->dest->loop_father);
      for (aloop = e->src->loop_father;
	   aloop != cloop;
	   aloop = loop_outer (aloop))
	{
	  exit = ggc_alloc<loop_exit> ();
	  exit->e = e;

	  exit->next = aloop->exits->next;
	  exit->prev = aloop->exits;
	  exit->next->prev = exit;
	  exit->prev->next = exit;

	  exit->next_e = exits;
	  exits = exit;
	}
    }

  if (!exits && new_edge)
    return;

  loop_exit **slot
    = current_loops->exits->find_slot_with_hash (e, htab_hash_pointer (e),
						 exits ? INSERT : NO_INSERT);
  if (!slot)
    return;

  if (exits)
    {
      if (*slot)
	loop_exit_hasher::remove (*slot);
      *slot = exits;
    }
  else
    current_loops->exits->clear_slot (slot);
}

/* For each loop, record list of exit edges, and start maintaining these
   lists.  */

void
record_loop_exits (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;

  if (!current_loops)
    return;

  if (loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    return;
  loops_state_set (LOOPS_HAVE_RECORDED_EXITS);

  gcc_assert (current_loops->exits == NULL);
  current_loops->exits
    = hash_table<loop_exit_hasher>::create_ggc (2 * number_of_loops (cfun));

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  rescan_loop_exit (e, true, false);
	}
    }
}

/* Dumps information about the exit in *SLOT to FILE.
   Callback for htab_traverse.  */

int
dump_recorded_exit (loop_exit **slot, FILE *file)
{
  struct loop_exit *exit = *slot;
  unsigned n = 0;
  edge e = exit->e;

  for (; exit != NULL; exit = exit->next_e)
    n++;

  fprintf (file, "Edge %d->%d exits %u loops\n",
	   e->src->index, e->dest->index, n);

  return 1;
}

/* Dumps the recorded exits of loops to FILE.  */

extern void dump_recorded_exits (FILE *);
void
dump_recorded_exits (FILE *file)
{
  if (!current_loops->exits)
    return;
  current_loops->exits->traverse<FILE *, dump_recorded_exit> (file);
}

/* Releases lists of loop exits.  */

void
release_recorded_exits (function *fn)
{
  gcc_assert (loops_state_satisfies_p (fn, LOOPS_HAVE_RECORDED_EXITS));
  loops_for_fn (fn)->exits->empty ();
  loops_for_fn (fn)->exits = NULL;
  loops_state_clear (fn, LOOPS_HAVE_RECORDED_EXITS);
}

/* Returns the list of the exit edges of a LOOP.  */

vec<edge> 
get_loop_exit_edges (const class loop *loop, basic_block *body)
{
  vec<edge> edges = vNULL;
  edge e;
  unsigned i;
  edge_iterator ei;
  struct loop_exit *exit;

  gcc_assert (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun));

  /* If we maintain the lists of exits, use them.  Otherwise we must
     scan the body of the loop.  */
  if (loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    {
      for (exit = loop->exits->next; exit->e; exit = exit->next)
	edges.safe_push (exit->e);
    }
  else
    {
      bool body_from_caller = true;
      if (!body)
	{
	  body = get_loop_body (loop);
	  body_from_caller = false;
	}
      for (i = 0; i < loop->num_nodes; i++)
	FOR_EACH_EDGE (e, ei, body[i]->succs)
	  {
	    if (!flow_bb_inside_loop_p (loop, e->dest))
	      edges.safe_push (e);
	  }
      if (!body_from_caller)
	free (body);
    }

  return edges;
}

/* Counts the number of conditional branches inside LOOP.  */

unsigned
num_loop_branches (const class loop *loop)
{
  unsigned i, n;
  basic_block * body;

  gcc_assert (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun));

  body = get_loop_body (loop);
  n = 0;
  for (i = 0; i < loop->num_nodes; i++)
    if (EDGE_COUNT (body[i]->succs) >= 2)
      n++;
  free (body);

  return n;
}

/* Adds basic block BB to LOOP.  */
void
add_bb_to_loop (basic_block bb, class loop *loop)
{
  unsigned i;
  loop_p ploop;
  edge_iterator ei;
  edge e;

  gcc_assert (bb->loop_father == NULL);
  bb->loop_father = loop;
  loop->num_nodes++;
  FOR_EACH_VEC_SAFE_ELT (loop->superloops, i, ploop)
    ploop->num_nodes++;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      rescan_loop_exit (e, true, false);
    }
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      rescan_loop_exit (e, true, false);
    }
}

/* Remove basic block BB from loops.  */
void
remove_bb_from_loops (basic_block bb)
{
  unsigned i;
  class loop *loop = bb->loop_father;
  loop_p ploop;
  edge_iterator ei;
  edge e;

  gcc_assert (loop != NULL);
  loop->num_nodes--;
  FOR_EACH_VEC_SAFE_ELT (loop->superloops, i, ploop)
    ploop->num_nodes--;
  bb->loop_father = NULL;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      rescan_loop_exit (e, false, true);
    }
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      rescan_loop_exit (e, false, true);
    }
}

/* Finds nearest common ancestor in loop tree for given loops.  */
class loop *
find_common_loop (class loop *loop_s, class loop *loop_d)
{
  unsigned sdepth, ddepth;

  if (!loop_s) return loop_d;
  if (!loop_d) return loop_s;

  sdepth = loop_depth (loop_s);
  ddepth = loop_depth (loop_d);

  if (sdepth < ddepth)
    loop_d = (*loop_d->superloops)[sdepth];
  else if (sdepth > ddepth)
    loop_s = (*loop_s->superloops)[ddepth];

  while (loop_s != loop_d)
    {
      loop_s = loop_outer (loop_s);
      loop_d = loop_outer (loop_d);
    }
  return loop_s;
}

/* Removes LOOP from structures and frees its data.  */

void
delete_loop (class loop *loop)
{
  /* Remove the loop from structure.  */
  flow_loop_tree_node_remove (loop);

  /* Remove loop from loops array.  */
  (*current_loops->larray)[loop->num] = NULL;

  /* Free loop data.  */
  flow_loop_free (loop);
}

/* Cancels the LOOP; it must be innermost one.  */

static void
cancel_loop (class loop *loop)
{
  basic_block *bbs;
  unsigned i;
  class loop *outer = loop_outer (loop);

  gcc_assert (!loop->inner);

  /* Move blocks up one level (they should be removed as soon as possible).  */
  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
    bbs[i]->loop_father = outer;

  free (bbs);
  delete_loop (loop);
}

/* Cancels LOOP and all its subloops.  */
void
cancel_loop_tree (class loop *loop)
{
  while (loop->inner)
    cancel_loop_tree (loop->inner);
  cancel_loop (loop);
}

/* Disable warnings about missing quoting in GCC diagnostics for
   the verification errors.  Their format strings don't follow GCC
   diagnostic conventions and the calls are ultimately followed by
   a deliberate ICE triggered by a failed assertion.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

/* Checks that information about loops is correct
     -- sizes of loops are all right
     -- results of get_loop_body really belong to the loop
     -- loop header have just single entry edge and single latch edge
     -- loop latches have only single successor that is header of their loop
     -- irreducible loops are correctly marked
     -- the cached loop depth and loop father of each bb is correct
  */
DEBUG_FUNCTION void
verify_loop_structure (void)
{
  unsigned *sizes, i, j;
  basic_block bb, *bbs;
  class loop *loop;
  int err = 0;
  edge e;
  unsigned num = number_of_loops (cfun);
  struct loop_exit *exit, *mexit;
  bool dom_available = dom_info_available_p (CDI_DOMINATORS);

  if (loops_state_satisfies_p (LOOPS_NEED_FIXUP))
    {
      error ("loop verification on loop tree that needs fixup");
      err = 1;
    }

  /* We need up-to-date dominators, compute or verify them.  */
  if (!dom_available)
    calculate_dominance_info (CDI_DOMINATORS);
  else
    verify_dominators (CDI_DOMINATORS);

  /* Check the loop tree root.  */
  if (current_loops->tree_root->header != ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || current_loops->tree_root->latch != EXIT_BLOCK_PTR_FOR_FN (cfun)
      || (current_loops->tree_root->num_nodes
	  != (unsigned) n_basic_blocks_for_fn (cfun)))
    {
      error ("corrupt loop tree root");
      err = 1;
    }

  /* Check the headers.  */
  FOR_EACH_BB_FN (bb, cfun)
    if (bb_loop_header_p (bb))
      {
	if (bb->loop_father->header == NULL)
	  {
	    error ("loop with header %d marked for removal", bb->index);
	    err = 1;
	  }
	else if (bb->loop_father->header != bb)
	  {
	    error ("loop with header %d not in loop tree", bb->index);
	    err = 1;
	  }
      }
    else if (bb->loop_father->header == bb)
      {
	error ("non-loop with header %d not marked for removal", bb->index);
	err = 1;
      }

  /* Check the recorded loop father and sizes of loops.  */
  auto_sbitmap visited (last_basic_block_for_fn (cfun));
  bitmap_clear (visited);
  bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun));
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      unsigned n;

      if (loop->header == NULL)
	{
	  error ("removed loop %d in loop tree", loop->num);
	  err = 1;
	  continue;
	}

      n = get_loop_body_with_size (loop, bbs, n_basic_blocks_for_fn (cfun));
      if (loop->num_nodes != n)
	{
	  error ("size of loop %d should be %d, not %d",
		 loop->num, n, loop->num_nodes);
	  err = 1;
	}

      for (j = 0; j < n; j++)
	{
	  bb = bbs[j];

	  if (!flow_bb_inside_loop_p (loop, bb))
	    {
	      error ("bb %d does not belong to loop %d",
		     bb->index, loop->num);
	      err = 1;
	    }

	  /* Ignore this block if it is in an inner loop.  */
	  if (bitmap_bit_p (visited, bb->index))
	    continue;
	  bitmap_set_bit (visited, bb->index);

	  if (bb->loop_father != loop)
	    {
	      error ("bb %d has father loop %d, should be loop %d",
		     bb->index, bb->loop_father->num, loop->num);
	      err = 1;
	    }
	}
    }
  free (bbs);

  /* Check headers and latches.  */
  FOR_EACH_LOOP (loop, 0)
    {
      i = loop->num;
      if (loop->header == NULL)
	continue;
      if (!bb_loop_header_p (loop->header))
	{
	  error ("loop %d%'s header is not a loop header", i);
	  err = 1;
	}
      if (loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS)
	  && EDGE_COUNT (loop->header->preds) != 2)
	{
	  error ("loop %d%'s header does not have exactly 2 entries", i);
	  err = 1;
	}
      if (loop->latch)
	{
	  if (!find_edge (loop->latch, loop->header))
	    {
	      error ("loop %d%'s latch does not have an edge to its header", i);
	      err = 1;
	    }
	  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, loop->header))
	    {
	      error ("loop %d%'s latch is not dominated by its header", i);
	      err = 1;
	    }
	}
      if (loops_state_satisfies_p (LOOPS_HAVE_SIMPLE_LATCHES))
	{
	  if (!single_succ_p (loop->latch))
	    {
	      error ("loop %d%'s latch does not have exactly 1 successor", i);
	      err = 1;
	    }
	  if (single_succ (loop->latch) != loop->header)
	    {
	      error ("loop %d%'s latch does not have header as successor", i);
	      err = 1;
	    }
	  if (loop->latch->loop_father != loop)
	    {
	      error ("loop %d%'s latch does not belong directly to it", i);
	      err = 1;
	    }
	}
      if (loop->header->loop_father != loop)
	{
	  error ("loop %d%'s header does not belong directly to it", i);
	  err = 1;
	}
      if (loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS)
	  && (loop_latch_edge (loop)->flags & EDGE_IRREDUCIBLE_LOOP))
	{
	  error ("loop %d%'s latch is marked as part of irreducible region", i);
	  err = 1;
	}
    }

  /* Check irreducible loops.  */
  if (loops_state_satisfies_p (LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS))
    {
      auto_edge_flag saved_irr_mask (cfun);
      /* Record old info.  */
      auto_sbitmap irreds (last_basic_block_for_fn (cfun));
      FOR_EACH_BB_FN (bb, cfun)
	{
	  edge_iterator ei;
	  if (bb->flags & BB_IRREDUCIBLE_LOOP)
	    bitmap_set_bit (irreds, bb->index);
	  else
	    bitmap_clear_bit (irreds, bb->index);
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_IRREDUCIBLE_LOOP)
	      e->flags |= saved_irr_mask;
	}

      /* Recount it.  */
      mark_irreducible_loops ();

      /* Compare.  */
      FOR_EACH_BB_FN (bb, cfun)
	{
	  edge_iterator ei;

	  if ((bb->flags & BB_IRREDUCIBLE_LOOP)
	      && !bitmap_bit_p (irreds, bb->index))
	    {
	      error ("basic block %d should be marked irreducible", bb->index);
	      err = 1;
	    }
	  else if (!(bb->flags & BB_IRREDUCIBLE_LOOP)
	      && bitmap_bit_p (irreds, bb->index))
	    {
	      error ("basic block %d should not be marked irreducible", bb->index);
	      err = 1;
	    }
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if ((e->flags & EDGE_IRREDUCIBLE_LOOP)
		  && !(e->flags & saved_irr_mask))
		{
		  error ("edge from %d to %d should be marked irreducible",
			 e->src->index, e->dest->index);
		  err = 1;
		}
	      else if (!(e->flags & EDGE_IRREDUCIBLE_LOOP)
		       && (e->flags & saved_irr_mask))
		{
		  error ("edge from %d to %d should not be marked irreducible",
			 e->src->index, e->dest->index);
		  err = 1;
		}
	      e->flags &= ~saved_irr_mask;
	    }
	}
    }

  /* Check the recorded loop exits.  */
  FOR_EACH_LOOP (loop, 0)
    {
      if (!loop->exits || loop->exits->e != NULL)
	{
	  error ("corrupted head of the exits list of loop %d",
		 loop->num);
	  err = 1;
	}
      else
	{
	  /* Check that the list forms a cycle, and all elements except
	     for the head are nonnull.  */
	  for (mexit = loop->exits, exit = mexit->next, i = 0;
	       exit->e && exit != mexit;
	       exit = exit->next)
	    {
	      if (i++ & 1)
		mexit = mexit->next;
	    }

	  if (exit != loop->exits)
	    {
	      error ("corrupted exits list of loop %d", loop->num);
	      err = 1;
	    }
	}

      if (!loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
	{
	  if (loop->exits->next != loop->exits)
	    {
	      error ("nonempty exits list of loop %d, but exits are not recorded",
		     loop->num);
	      err = 1;
	    }
	}
    }

  if (loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    {
      unsigned n_exits = 0, eloops;

      sizes = XCNEWVEC (unsigned, num);
      memset (sizes, 0, sizeof (unsigned) * num);
      FOR_EACH_BB_FN (bb, cfun)
	{
	  edge_iterator ei;
	  if (bb->loop_father == current_loops->tree_root)
	    continue;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (flow_bb_inside_loop_p (bb->loop_father, e->dest))
		continue;

	      n_exits++;
	      exit = get_exit_descriptions (e);
	      if (!exit)
		{
		  error ("exit %d->%d not recorded",
			 e->src->index, e->dest->index);
		  err = 1;
		}
	      eloops = 0;
	      for (; exit; exit = exit->next_e)
		eloops++;

	      for (loop = bb->loop_father;
		   loop != e->dest->loop_father
		   /* When a loop exit is also an entry edge which
		      can happen when avoiding CFG manipulations
		      then the last loop exited is the outer loop
		      of the loop entered.  */
		   && loop != loop_outer (e->dest->loop_father);
		   loop = loop_outer (loop))
		{
		  eloops--;
		  sizes[loop->num]++;
		}

	      if (eloops != 0)
		{
		  error ("wrong list of exited loops for edge %d->%d",
			 e->src->index, e->dest->index);
		  err = 1;
		}
	    }
	}

      if (n_exits != current_loops->exits->elements ())
	{
	  error ("too many loop exits recorded");
	  err = 1;
	}

      FOR_EACH_LOOP (loop, 0)
	{
	  eloops = 0;
	  for (exit = loop->exits->next; exit->e; exit = exit->next)
	    eloops++;
	  if (eloops != sizes[loop->num])
	    {
	      error ("%d exits recorded for loop %d (having %d exits)",
		     eloops, loop->num, sizes[loop->num]);
	      err = 1;
	    }
	}

      free (sizes);
    }

  gcc_assert (!err);

  if (!dom_available)
    free_dominance_info (CDI_DOMINATORS);
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

/* Returns latch edge of LOOP.  */
edge
loop_latch_edge (const class loop *loop)
{
  return find_edge (loop->latch, loop->header);
}

/* Returns preheader edge of LOOP.  */
edge
loop_preheader_edge (const class loop *loop)
{
  edge e;
  edge_iterator ei;

  gcc_assert (loops_state_satisfies_p (LOOPS_HAVE_PREHEADERS)
	      && ! loops_state_satisfies_p (LOOPS_MAY_HAVE_MULTIPLE_LATCHES));

  FOR_EACH_EDGE (e, ei, loop->header->preds)
    if (e->src != loop->latch)
      break;

  if (! e)
    {
      gcc_assert (! loop_outer (loop));
      return single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
    }

  return e;
}

/* Returns true if E is an exit of LOOP.  */

bool
loop_exit_edge_p (const class loop *loop, const_edge e)
{
  return (flow_bb_inside_loop_p (loop, e->src)
	  && !flow_bb_inside_loop_p (loop, e->dest));
}

/* Returns the single exit edge of LOOP, or NULL if LOOP has either no exit
   or more than one exit.  If loops do not have the exits recorded, NULL
   is returned always.  */

edge
single_exit (const class loop *loop)
{
  struct loop_exit *exit = loop->exits->next;

  if (!loops_state_satisfies_p (LOOPS_HAVE_RECORDED_EXITS))
    return NULL;

  if (exit->e && exit->next == loop->exits)
    return exit->e;
  else
    return NULL;
}

/* Returns true when BB has an incoming edge exiting LOOP.  */

bool
loop_exits_to_bb_p (class loop *loop, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (loop_exit_edge_p (loop, e))
      return true;

  return false;
}

/* Returns true when BB has an outgoing edge exiting LOOP.  */

bool
loop_exits_from_bb_p (class loop *loop, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (loop_exit_edge_p (loop, e))
      return true;

  return false;
}

/* Return location corresponding to the loop control condition if possible.  */

dump_user_location_t
get_loop_location (class loop *loop)
{
  rtx_insn *insn = NULL;
  class niter_desc *desc = NULL;
  edge exit;

  /* For a for or while loop, we would like to return the location
     of the for or while statement, if possible.  To do this, look
     for the branch guarding the loop back-edge.  */

  /* If this is a simple loop with an in_edge, then the loop control
     branch is typically at the end of its source.  */
  desc = get_simple_loop_desc (loop);
  if (desc->in_edge)
    {
      FOR_BB_INSNS_REVERSE (desc->in_edge->src, insn)
        {
          if (INSN_P (insn) && INSN_HAS_LOCATION (insn))
            return insn;
        }
    }
  /* If loop has a single exit, then the loop control branch
     must be at the end of its source.  */
  if ((exit = single_exit (loop)))
    {
      FOR_BB_INSNS_REVERSE (exit->src, insn)
        {
          if (INSN_P (insn) && INSN_HAS_LOCATION (insn))
            return insn;
        }
    }
  /* Next check the latch, to see if it is non-empty.  */
  FOR_BB_INSNS_REVERSE (loop->latch, insn)
    {
      if (INSN_P (insn) && INSN_HAS_LOCATION (insn))
        return insn;
    }
  /* Finally, if none of the above identifies the loop control branch,
     return the first location in the loop header.  */
  FOR_BB_INSNS (loop->header, insn)
    {
      if (INSN_P (insn) && INSN_HAS_LOCATION (insn))
        return insn;
    }
  /* If all else fails, simply return the current function location.  */
  return dump_user_location_t::from_function_decl (current_function_decl);
}

/* Records that every statement in LOOP is executed I_BOUND times.
   REALISTIC is true if I_BOUND is expected to be close to the real number
   of iterations.  UPPER is true if we are sure the loop iterates at most
   I_BOUND times.  */

void
record_niter_bound (class loop *loop, const widest_int &i_bound,
		    bool realistic, bool upper)
{
  /* Update the bounds only when there is no previous estimation, or when the
     current estimation is smaller.  */
  if (upper
      && (!loop->any_upper_bound
	  || wi::ltu_p (i_bound, loop->nb_iterations_upper_bound)))
    {
      loop->any_upper_bound = true;
      loop->nb_iterations_upper_bound = i_bound;
      if (!loop->any_likely_upper_bound)
	{
	  loop->any_likely_upper_bound = true;
	  loop->nb_iterations_likely_upper_bound = i_bound;
	}
    }
  if (realistic
      && (!loop->any_estimate
	  || wi::ltu_p (i_bound, loop->nb_iterations_estimate)))
    {
      loop->any_estimate = true;
      loop->nb_iterations_estimate = i_bound;
    }
  if (!realistic
      && (!loop->any_likely_upper_bound
          || wi::ltu_p (i_bound, loop->nb_iterations_likely_upper_bound)))
    {
      loop->any_likely_upper_bound = true;
      loop->nb_iterations_likely_upper_bound = i_bound;
    }

  /* If an upper bound is smaller than the realistic estimate of the
     number of iterations, use the upper bound instead.  */
  if (loop->any_upper_bound
      && loop->any_estimate
      && wi::ltu_p (loop->nb_iterations_upper_bound,
		    loop->nb_iterations_estimate))
    loop->nb_iterations_estimate = loop->nb_iterations_upper_bound;
  if (loop->any_upper_bound
      && loop->any_likely_upper_bound
      && wi::ltu_p (loop->nb_iterations_upper_bound,
		    loop->nb_iterations_likely_upper_bound))
    loop->nb_iterations_likely_upper_bound = loop->nb_iterations_upper_bound;
}

/* Similar to get_estimated_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
get_estimated_loop_iterations_int (class loop *loop)
{
  widest_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!get_estimated_loop_iterations (loop, &nit))
    return -1;

  if (!wi::fits_shwi_p (nit))
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Returns an upper bound on the number of executions of statements
   in the LOOP.  For statements before the loop exit, this exceeds
   the number of execution of the latch by one.  */

HOST_WIDE_INT
max_stmt_executions_int (class loop *loop)
{
  HOST_WIDE_INT nit = get_max_loop_iterations_int (loop);
  HOST_WIDE_INT snit;

  if (nit == -1)
    return -1;

  snit = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) nit + 1);

  /* If the computation overflows, return -1.  */
  return snit < 0 ? -1 : snit;
}

/* Returns an likely upper bound on the number of executions of statements
   in the LOOP.  For statements before the loop exit, this exceeds
   the number of execution of the latch by one.  */

HOST_WIDE_INT
likely_max_stmt_executions_int (class loop *loop)
{
  HOST_WIDE_INT nit = get_likely_max_loop_iterations_int (loop);
  HOST_WIDE_INT snit;

  if (nit == -1)
    return -1;

  snit = (HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) nit + 1);

  /* If the computation overflows, return -1.  */
  return snit < 0 ? -1 : snit;
}

/* Sets NIT to the estimated number of executions of the latch of the
   LOOP.  If we have no reliable estimate, the function returns false, otherwise
   returns true.  */

bool
get_estimated_loop_iterations (class loop *loop, widest_int *nit)
{
  /* Even if the bound is not recorded, possibly we can derrive one from
     profile.  */
  if (!loop->any_estimate)
    {
      if (loop->header->count.reliable_p ())
	{
          *nit = gcov_type_to_wide_int
		   (expected_loop_iterations_unbounded (loop) + 1);
	  return true;
	}
      return false;
    }

  *nit = loop->nb_iterations_estimate;
  return true;
}

/* Sets NIT to an upper bound for the maximum number of executions of the
   latch of the LOOP.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
get_max_loop_iterations (const class loop *loop, widest_int *nit)
{
  if (!loop->any_upper_bound)
    return false;

  *nit = loop->nb_iterations_upper_bound;
  return true;
}

/* Similar to get_max_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
get_max_loop_iterations_int (const class loop *loop)
{
  widest_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!get_max_loop_iterations (loop, &nit))
    return -1;

  if (!wi::fits_shwi_p (nit))
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Sets NIT to an upper bound for the maximum number of executions of the
   latch of the LOOP.  If we have no reliable estimate, the function returns
   false, otherwise returns true.  */

bool
get_likely_max_loop_iterations (class loop *loop, widest_int *nit)
{
  if (!loop->any_likely_upper_bound)
    return false;

  *nit = loop->nb_iterations_likely_upper_bound;
  return true;
}

/* Similar to get_max_loop_iterations, but returns the estimate only
   if it fits to HOST_WIDE_INT.  If this is not the case, or the estimate
   on the number of iterations of LOOP could not be derived, returns -1.  */

HOST_WIDE_INT
get_likely_max_loop_iterations_int (class loop *loop)
{
  widest_int nit;
  HOST_WIDE_INT hwi_nit;

  if (!get_likely_max_loop_iterations (loop, &nit))
    return -1;

  if (!wi::fits_shwi_p (nit))
    return -1;
  hwi_nit = nit.to_shwi ();

  return hwi_nit < 0 ? -1 : hwi_nit;
}

/* Returns the loop depth of the loop BB belongs to.  */

int
bb_loop_depth (const_basic_block bb)
{
  return bb->loop_father ? loop_depth (bb->loop_father) : 0;
}

/* Marks LOOP for removal and sets LOOPS_NEED_FIXUP.  */

void
mark_loop_for_removal (loop_p loop)
{
  if (loop->header == NULL)
    return;
  loop->former_header = loop->header;
  loop->header = NULL;
  loop->latch = NULL;
  loops_state_set (LOOPS_NEED_FIXUP);
}
