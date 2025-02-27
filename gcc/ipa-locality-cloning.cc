/* Code locality based function cloning.
   Copyright The GNU Toolchain Authors

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

/* This file implements cloning required to improve partitioning of the
   callgraph for locality considerations.

   Partitioning for improving code locality.
   This pass aims to place frequently executed callchains closer together in
   memory to improve performance through improved locality.  If any frequent
   callchains cannot be placed together because they are already placed
   elsewhere, local function clones are created and all callers near to the
   clones are redirected to use this copy.

   Locality code placement is done in 2 parts.
   1. IPA pass to be executed after ipa-inline and before ipa-pure-const.
      Execute stage prepares the plan to place all nodes into partitions.
   2. WPA Partition stage actually implements the plan.

   Brief overview of the IPA pass:
   1. Create and sort callchains.  If PGO is available, use real profile
   counts.  Otherwise, use a set of heuristics to sort the callchains.
   2. Create a partition plan for the callchains, processing them in the sorted
      order.
      1. If a function is unpartitioned, place it in the current partition.
      2. If a function is already placed in a partition away from current
	 partition as part of another callchain:
	 Create a local clone in current partition, if cloning criteria is
	 satisfied.
      3. Redirect any new caller to a local clone if one exists.
   Partition size is param controlled to fine tune per program behavior.  */

#include "config.h"
#define INCLUDE_ALGORITHM
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "tree.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "symtab-thunks.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "symtab-clones.h"
#include "ipa-locality-cloning.h"

/* Locality partitions, assigns nodes to partitions.  These are used later in
   WPA partitioning.  */
vec<locality_partition> locality_partitions;

/* Map from original node to its latest clone.  Gets overwritten whenever a new
   clone is created from the same node.  */
hash_map<cgraph_node *, cgraph_node *> node_to_clone;
/* Map from clone to its original node.  */
hash_map<cgraph_node *, cgraph_node *> clone_to_node;

/* Data structure to hold static heuristics and orders for cgraph_nodes.  */
struct locality_order
{
  cgraph_node *node;
  sreal order;
  locality_order (cgraph_node *node, sreal order) : node (node), order (order)
  {}
};

/* Return true if NODE is already in some partition.  */
static inline bool
node_partitioned_p (cgraph_node *node)
{
  return node->aux;
}

/* Add symbol NODE to partition PART.  */
static void
add_node_to_partition (locality_partition part, cgraph_node *node)
{
  struct cgraph_edge *e;
  if (node_partitioned_p (node))
    return;

  part->nodes.safe_push (node);
  node->aux = (void *) (uintptr_t) (part->part_id);

  if (!node->alias && node->get_partitioning_class () == SYMBOL_PARTITION)
    part->insns += ipa_size_summaries->get (node)->size;

  /* Add all inline clones and callees that are duplicated.  */
  for (e = node->callees; e; e = e->next_callee)
    if (!e->inline_failed)
      add_node_to_partition (part, e->callee);
    /* omp declare_variant_alt or transparent_alias with definition or linker
       discardable (non-local comdat but not forced and not
       used by non-LTO).  */
    else if (e->callee->get_partitioning_class () == SYMBOL_DUPLICATE)
      add_node_to_partition (part, e->callee);

  /* Add all thunks associated with the function.  */
  for (e = node->callers; e; e = e->next_caller)
    if (e->caller->thunk && !e->caller->inlined_to)
      add_node_to_partition (part, e->caller);

  /* Add all aliases associated with the symbol.  */
  struct ipa_ref *ref;
  FOR_EACH_ALIAS (node, ref)
    if (!ref->referring->transparent_alias)
      {
	cgraph_node *referring = dyn_cast<cgraph_node *> (ref->referring);
	/* Only add function aliases.
	   Varpool refs are added later in LTO partitioning pass.  */
	if (referring)
	  add_node_to_partition (part, referring);
      }
    else
      {
	struct ipa_ref *ref2;
	/* We do not need to add transparent aliases if they are not used.
	   However we must add aliases of transparent aliases if they exist.  */
	FOR_EACH_ALIAS (ref->referring, ref2)
	  {
	    /* Nested transparent aliases are not permitted.  */
	    gcc_checking_assert (!ref2->referring->transparent_alias);
	    cgraph_node *referring = dyn_cast<cgraph_node *> (ref2->referring);
	    if (referring)
	      add_node_to_partition (part, referring);
	  }
      }
}

/* Return TRUE if NODE is in PARTITION.  */
static bool
node_in_partition_p (locality_partition partition, cgraph_node *node)
{
  return ((uintptr_t) (partition->part_id) == (uintptr_t) (node->aux));
}

/* Helper function for qsort; to break ties.  */
static int
compare_node_uids (cgraph_node *n1, cgraph_node *n2)
{
  int res = n1->get_uid () - n2->get_uid ();
  gcc_assert (res != 0);
  return res > 0 ? 1 : -1;
}

/* Helper function for qsort; sort nodes by order.  */
static int
static_profile_cmp (const void *pa, const void *pb)
{
  const locality_order *a = *static_cast<const locality_order *const *> (pa);
  const locality_order *b = *static_cast<const locality_order *const *> (pb);
  /* Ascending order.  */
  if (b->order < a->order)
    return 1;
  if (b->order > a->order)
    return -1;
  return compare_node_uids (a->node, b->node);
}

/* Helper function for qsort; sort nodes by profile count.  */
static int
compare_edge_profile_counts (const void *pa, const void *pb)
{
  const locality_order *a = *static_cast<const locality_order *const *> (pa);
  const locality_order *b = *static_cast<const locality_order *const *> (pb);

  profile_count cnt1 = a->node->count.ipa ();
  profile_count cnt2 = b->node->count.ipa ();
  if (!cnt1.compatible_p (cnt2))
    return static_profile_cmp (pa, pb);

  if (cnt1 < cnt2)
    return 1;
  if (cnt1 > cnt2)
    return -1;
  return static_profile_cmp (pa, pb);
}

/* Create and return a new partition and increment NPARTITIONS.  */

static locality_partition
create_partition (int &npartitions)
{
  locality_partition part = XCNEW (struct locality_partition_def);
  npartitions++;
  part->part_id = npartitions;
  part->nodes.create (1);
  part->insns = 0;
  locality_partitions.safe_push (part);
  return part;
}

/* Structure for holding profile count information of callers of a node.  */
struct profile_stats
{
  /* Sum of non-recursive call counts.  */
  profile_count nonrec_count;

  /* Sum of recursive call counts.  */
  profile_count rec_count;

  /* If non-NULL, this node is the target of alias or thunk and calls from this
     should be count in rec_count.  */
  cgraph_node *target;
};

/* Initialize fields of STATS.  */
static inline void
init_profile_stats (profile_stats *stats, cgraph_node *target = NULL)
{
  stats->nonrec_count = profile_count::zero ();
  stats->rec_count = profile_count::zero ();
  stats->target = target;
}

/* Helper function of to accumulate call counts.  */
static bool
accumulate_profile_counts_after_cloning (cgraph_node *node, void *data)
{
  struct profile_stats *stats = (struct profile_stats *) data;
  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    {
      if (!e->count.initialized_p ())
	continue;

      if (e->caller == stats->target)
	stats->rec_count += e->count.ipa ();
      else
	stats->nonrec_count += e->count.ipa ();
    }
  return false;
}

/* NEW_NODE is a previously created clone of ORIG_NODE already present in
   current partition.  EDGES contains newly redirected edges to NEW_NODE.
   Adjust profile information for both nodes and the edge.  */

static void
adjust_profile_info_for_non_self_rec_edges (auto_vec<cgraph_edge *> &edges,
					    cgraph_node *new_node,
					    cgraph_node *orig_node)
{
  profile_count orig_node_count = orig_node->count.ipa ();
  profile_count edge_count = profile_count::zero ();
  profile_count final_new_count = profile_count::zero ();
  profile_count final_orig_count = profile_count::zero ();

  for (unsigned i = 0; i < edges.length (); ++i)
    if (edges[i]->count.initialized_p ())
      edge_count += edges[i]->count.ipa ();

  final_orig_count = orig_node_count - edge_count;

  /* NEW_NODE->count was adjusted for other callers when the clone was
     first created.  Just add the new edge count.  */
  final_new_count = new_node->count + edge_count;

  final_new_count = orig_node_count.combine_with_ipa_count (final_new_count);
  orig_node->count = final_orig_count;
  new_node->count = final_new_count;

    if (dump_file)
    {
      fprintf (dump_file, "Adjusting profile information for %s\n",
	       new_node->dump_asm_name ());
      fprintf (dump_file, "\tOriginal node %s\n", orig_node->dump_asm_name ());
      fprintf (dump_file, "\tOriginal count: ");
      orig_node_count.dump (dump_file);
      fprintf (dump_file, "\n\tAdjusted original count to: ");
      final_orig_count.dump (dump_file);
      fprintf (dump_file, "\n\tAdjusted clone count to: ");
      final_new_count.dump (dump_file);
      fprintf (dump_file, "\n");
    }

  /* Scale all callee edges according to adjusted counts.  */
  profile_count orig_node_count_copy = orig_node_count;
  profile_count::adjust_for_ipa_scaling (&final_new_count,
					 &orig_node_count_copy);
  for (cgraph_edge *cs = new_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_new_count, orig_node_count_copy);
  for (cgraph_edge *cs = new_node->indirect_calls; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_new_count, orig_node_count_copy);

  profile_count::adjust_for_ipa_scaling (&final_orig_count, &orig_node_count);
  for (cgraph_edge *cs = orig_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_orig_count, orig_node_count);
  for (cgraph_edge *cs = orig_node->indirect_calls; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_orig_count, orig_node_count);
}

/* Adjust profile counts of NEW_NODE and ORIG_NODE, where NEW_NODE is a clone
   of OLD_NODE.
   Assumes that all eligible edges from current partition so far are redirected
   to NEW_NODE and recursive edges are adjusted.  */

static void
adjust_profile_info (cgraph_node *new_node, cgraph_node *orig_node)
{
  /* If all calls to NEW_NODE are non-recursive, subtract corresponding count
     from ORIG_NODE and assign to NEW_NODE, any unexpected remainder stays with
     ORIG_NODE.
     Recursive calls if present, likely contribute to majority of count;
     scale according to redirected callers' count.  */

  profile_count orig_node_count = orig_node->count.ipa ();
  profile_stats new_stats, orig_stats;

  init_profile_stats (&new_stats);
  init_profile_stats (&orig_stats);

  new_node->call_for_symbol_thunks_and_aliases
    (accumulate_profile_counts_after_cloning, &new_stats, false);
  orig_node->call_for_symbol_thunks_and_aliases
    (accumulate_profile_counts_after_cloning, &orig_stats, false);

  profile_count orig_nonrec_count = orig_stats.nonrec_count;
  profile_count orig_rec_count = orig_stats.rec_count;
  profile_count new_nonrec_count = new_stats.nonrec_count;
  profile_count new_rec_count = new_stats.rec_count;

  profile_count final_new_count = new_nonrec_count;
  profile_count final_orig_count = profile_count::zero ();

  /* All calls to NEW_NODE are non-recursive or recursive calls have
     zero count.  */
  if (!new_rec_count.nonzero_p ())
    final_orig_count = orig_node_count - new_nonrec_count;
  else
    {
      /* If ORIG_NODE is externally visible, indirect calls or calls from
	 another part of the code may contribute to the count.
	 update_profiling_info () from ipa-cp.cc pretends to have an extra
	 caller to represent the extra counts.  */
      if (!orig_node->local)
	{
	  profile_count pretend_count = (orig_node_count - new_nonrec_count -
					 orig_nonrec_count - orig_rec_count);
	  orig_nonrec_count += pretend_count;
	}

      /* Remaining rec_count is assigned in proportion to clone's non-recursive
	 count.  */
      profile_count rec_count = orig_node_count - new_nonrec_count
				- orig_nonrec_count;
      profile_count new_rec_scaled
	= rec_count.apply_scale (new_nonrec_count,
				 new_nonrec_count + orig_nonrec_count);
      final_new_count += new_rec_scaled;
      final_orig_count = orig_node_count - final_new_count;
    }

  final_new_count = orig_node_count.combine_with_ipa_count (final_new_count);
  new_node->count = final_new_count;
  orig_node->count = final_orig_count;

  if (dump_file)
    {
      fprintf (dump_file, "Adjusting profile information for %s\n",
	       new_node->dump_asm_name ());
      fprintf (dump_file, "\tOriginal node %s\n", orig_node->dump_asm_name ());
      fprintf (dump_file, "\tOriginal count: ");
      orig_node_count.dump (dump_file);
      fprintf (dump_file, "\n\tAdjusted original count to: ");
      final_orig_count.dump (dump_file);
      fprintf (dump_file, "\n\tAdjusted clone count to: ");
      final_new_count.dump (dump_file);
      fprintf (dump_file, "\n");
    }

  /* Scale all callee edges according to adjusted counts.  */
  profile_count orig_node_count_copy = orig_node_count;
  profile_count::adjust_for_ipa_scaling (&final_new_count,
					 &orig_node_count_copy);
  for (cgraph_edge *cs = new_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_new_count, orig_node_count_copy);
  for (cgraph_edge *cs = new_node->indirect_calls; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_new_count, orig_node_count_copy);

  profile_count::adjust_for_ipa_scaling (&final_orig_count, &orig_node_count);
  for (cgraph_edge *cs = orig_node->callees; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_orig_count, orig_node_count);
  for (cgraph_edge *cs = orig_node->indirect_calls; cs; cs = cs->next_callee)
    cs->count = cs->count.apply_scale (final_orig_count, orig_node_count);
}

/* Return true if EDGE can be safely redirected to another callee.  */
static inline bool
edge_redirectable_p (cgraph_edge *edge, lto_locality_cloning_model cm)
{
  if (cm == LTO_LOCALITY_NON_INTERPOSABLE_CLONING)
    {
      /* Interposability may change on edge basis.  */
      enum availability avail;
      avail = edge->callee->get_availability (edge->caller);
      if (avail <= AVAIL_INTERPOSABLE)
	return false;
    }
  return true;
}

/* Create a locality clone of CNODE and redirect all callers present in
   PARTITION.
   Create a clone dpending on whether CNODE itself is a clone or not.  */

static cgraph_node *
create_locality_clone (cgraph_node *cnode,
		       locality_partition partition, int &cl_num,
		       lto_locality_cloning_model cm)
{
  cgraph_node *cl_node = NULL;
  vec<cgraph_edge *> redirect_callers = vNULL;
  /* All callers of cnode in current partition are redirected.  */
  struct cgraph_edge *edge;
  for (edge = cnode->callers; edge; edge = edge->next_caller)
    {
      struct cgraph_node *caller = edge->caller;
      if (node_in_partition_p (partition, caller) && caller->definition
	  && caller != cnode && edge_redirectable_p (edge, cm))
	redirect_callers.safe_push (edge);
    }

  const char *suffix = "locality_clone";

  tree old_decl = cnode->decl;
  tree new_decl = copy_node (old_decl);

  /* Generate a new name for the new version. */
  const char *name = IDENTIFIER_POINTER (DECL_NAME (old_decl));
  DECL_NAME (new_decl) = clone_function_name (name, suffix, cl_num);
  SET_DECL_ASSEMBLER_NAME (new_decl,
			   clone_function_name (old_decl, suffix, cl_num));
  cl_num++;
  if (dump_file)
    fprintf (dump_file, "\tNew name %s\n",
	     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (new_decl)));

  cl_node = cnode->create_clone (new_decl, cnode->count /*profile_count*/,
				 false /*update_original*/, redirect_callers,
				 false /*call_duplication_hook*/,
				 NULL /*new_inlined_to*/,
				 NULL /*param_adjustments*/, suffix);

  set_new_clone_decl_and_node_flags (cl_node);

  if (cnode->ipa_transforms_to_apply.exists ())
    cl_node->ipa_transforms_to_apply
      = cnode->ipa_transforms_to_apply.copy ();

  if (dump_file)
    {
      fprintf (dump_file, "Cloned Node: %s %s\n", cnode->dump_asm_name (),
	       cl_node->dump_asm_name ());

      for (edge = cl_node->callers; edge; edge = edge->next_caller)
	fprintf (dump_file, "Redirected callers: %s\n",
		 edge->caller->dump_asm_name ());

      for (edge = cl_node->callees; edge; edge = edge->next_callee)
	fprintf (dump_file, "Callees of clone: %s %d\n",
		 edge->callee->dump_asm_name (), edge->frequency ());
    }
  return cl_node;
}

/* Redirect recursive edges of CLONE to correctly point to CLONE.  As part of
   cloning process, all callee edges of a node are just duplicated but not
   redirected.  Therefore, these edges still call to original of CLONE.

   For non-inlined CLONEs, NEW_CALLEE == CLONE and ORIG_CALLEE is CLONE's
   original node.

   For inlined node, self recursion to CLONE's original same as non-inlined,
   additionally, calls to CLONE->inlined_to are also recursive:
   NEW_CALLEE == CLONE->inlined_into and
   ORIG_CALLEE == original node of CLONE->inlined_into.  */

static void
adjust_recursive_callees (cgraph_node *clone, cgraph_node *new_callee,
			  cgraph_node *orig_callee)
{
  cgraph_node *alias = NULL;
  for (cgraph_edge *e = clone->callees; e; e = e->next_callee)
    {
      if (!e->inline_failed)
	continue;

      /* Only self-cycle or local alias are handled.  */
      cgraph_node *callee = e->callee;
      if (callee == orig_callee)
	{
	  cgraph_node **cl = node_to_clone.get (orig_callee);
	  gcc_assert (cl && *cl == new_callee);
	  e->redirect_callee_duplicating_thunks (new_callee);
	  if (dump_file)
	    fprintf (dump_file, "recursive call from %s to %s orig %s\n",
		     e->caller->dump_asm_name (), e->callee->dump_asm_name (),
		     callee->dump_asm_name ());
	}
      else if (callee->alias
	       && e->callee->ultimate_alias_target () == orig_callee)
	{
	  if (!alias)
	    {
	      alias = dyn_cast<cgraph_node *> (
		new_callee->noninterposable_alias ());
	    }
	  e->redirect_callee_duplicating_thunks (alias);
	  if (dump_file)
	    fprintf (dump_file, "recursive call from %s to %s orig %s\n",
		     e->caller->dump_asm_name (), e->callee->dump_asm_name (),
		     callee->dump_asm_name ());
	}
    }
  new_callee->expand_all_artificial_thunks ();
  if (alias)
    alias->expand_all_artificial_thunks ();
}

/* Create clones for CALLER's inlined callees, ORIG_INLINED_TO is the original
   node from clone_as_needed () such that new_inlined_to is a clone of it.  */

static void
inline_clones (cgraph_node *caller, cgraph_node *orig_inlined_to)
{
  struct cgraph_edge *edge;
  for (edge = caller->callees; edge; edge = edge->next_callee)
    {
      struct cgraph_node *callee = edge->callee;
      if (edge->inline_failed)
	continue;

      if (callee->inlined_to != orig_inlined_to)
	continue;

      struct cgraph_node *new_inlined_to, *cl;
      if (caller->inlined_to)
	new_inlined_to = caller->inlined_to;
      else
	new_inlined_to = caller;

      cl = callee->create_clone (callee->decl,
				 edge->count /*profile_count*/,
				 true /*update_original*/,
				 vNULL /*redirect_callers*/,
				 false /*call_duplication_hook*/,
				 new_inlined_to /*new_inlined_to*/,
				 NULL /*param_adjustments*/,
				 "locality_clone" /*suffix*/);
      edge->redirect_callee (cl);

      node_to_clone.put (callee, cl);
      clone_to_node.put (cl, callee);

      if (callee->thunk)
	{
	  thunk_info *info = thunk_info::get (callee);
	  *thunk_info::get_create (cl) = *info;
	}

      adjust_recursive_callees (cl, new_inlined_to, orig_inlined_to);
      adjust_recursive_callees (cl, cl, callee);
      if (dump_file)
	{
	  fprintf (dump_file, "Inline cloned\n");
	  cl->dump (dump_file);
	}

      /* Recursively inline till end of this callchain.  */
      inline_clones (cl, orig_inlined_to);
    }
}

/* Clone EDGE->CALLEE if it or a clone of it is not already in PARTITION.
   Redirect all callers of EDGE->CALLEE that are in PARTITION, not just the
   EDGE.  If a clone is already present in PARTITION, redirect all edges from
   EDGE->CALLER to EDGE->CALLEE.  This is because we only visit one edge per
   caller to callee and redirect for all others from there.

   If cloning, also recursively clone inlined functions till the end of the
   callchain because inlined clones have 1-1 exclusive copy and edge from
   caller to inlined node.

   There are 2 flows possible:
   1. Only redirect
      1.1. cnode is already in current partition - cnode mustn't be a
      locality_clone -> nothing to do
      1.2. A clone of cnode is in current partition - find out if it's the
      correct clone for edge - must be a locality_clone but the exact same
      kind as callee i.e. orig or cp/sra clone, if yes, redirect, else go to #2
      1.3. Cnode/a clone of cnode is in current partition but caller is inlined
   2. Clone and redirect
      2.1. cnode is original node
      2.2. cnode itself is a clone
      Clone inlines
   Flavors of edges:
   1. Normal -> orig nodes, locality clones or cp/sra clones
   2. Recursive -> direct recursion
   3. Alias -> recursion via aliasing or as a result of IPA code duplication
   4. Inline -> shouldn't be included in callchain.  */

static cgraph_node *
clone_node_as_needed (cgraph_edge *edge, locality_partition partition,
		      int &cl_num, lto_locality_cloning_model cm)
{
  /* suitable_for_locality_cloning_p () currently prohibits cloning aliases due
     to potential versioning and materialization issues.  Could be enabled in
     the future.  suitable_for_locality_cloning_p () also checks for
     interposability for CNODE but not for edge redirection.  */
  struct cgraph_node *cnode = edge->callee;
  struct cgraph_node *caller = edge->caller;

  /* If clone of cnode is already in the partition
     Get latest clone of cnode.  If current partition has cloned cnode, that
     clone should be returned.  Otherwise, clone from previous partition is
     returned
     Original node and its clone shouldn't co-exist in current partition

     This is required if callee is partitioned via another edge before caller
     was, and we are now visiting caller->callee edge

     1) a -> b ==> a -> bc1; b was cloned say via d -> bc1, a is orig
     2) ac1 -> b ==> ac1 -> bc1; b was cloned and a was just cloned
     3) a -> bc1 and bc2 present, mustn't happen, b was cloned and a was
	redirected without being partitioned first.
	Why will we do this again - multiple edges and something's wrong in
	partition_callchain ()
     4) ac1 -> bc1 ==> ac1 -> bc2; a was cloned and we already got (1) in some
	other partition
     5) ac1 -> bc1 but no clone present in this PARTITION.  Create from b, not
	from bc1?
     6) a -> b; a -> bc0; create new clone, no clone present
     7) ac0 -> b; ac0 -> bc0 same as (6)
     8) a -> bc0 and no clone present, mustn't happen, same as (3)

     Redirect when bc1 is present and:
     a -> b or ac -> b or ac -> bc0  */

  cgraph_node *orig_cnode = cnode;
  cgraph_node **o_cnode = clone_to_node.get (cnode);
  if (o_cnode)
    orig_cnode = *o_cnode;

  cgraph_node **cnode_cl = node_to_clone.get (orig_cnode);

  if (cnode_cl && node_in_partition_p (partition, *cnode_cl))
    {
      if (node_in_partition_p (partition, caller))
	{
	  bool clone_p = false;
	  auto_vec<cgraph_edge *> redirected_edges;
	  for (cgraph_edge *ec = caller->callees; ec; ec = ec->next_callee)
	    if (ec->callee == cnode && edge_redirectable_p (ec, cm))
	      {
		ec->redirect_callee_duplicating_thunks (*cnode_cl);
		clone_p = true;
		redirected_edges.safe_push (ec);
		if (dump_file)
		  {
		    fprintf (dump_file, "clone present %s %s redirecting %s\n",
			     cnode->dump_asm_name (),
			     (*cnode_cl)->dump_asm_name (),
			     caller->dump_asm_name ());
		  }
	      }
	  if (clone_p)
	    {
	      (*cnode_cl)->expand_all_artificial_thunks ();
	      adjust_profile_info_for_non_self_rec_edges (redirected_edges,
							  *cnode_cl, cnode);
	      return NULL;
	    }
	}
    }

  /* Create a new clone for a -> b, ac -> b.
     For ac -> bc, should be done on bc or b?
     bc could be from b_cp/b_sra or b.  */

  if (orig_cnode != cnode)
    {
      if (dump_file)
	fprintf (dump_file, "Clone of clone %s %s\n", cnode->dump_asm_name (),
		 orig_cnode->dump_asm_name ());
      return NULL;
    }

  struct cgraph_node *cloned_node
    = create_locality_clone (cnode, partition, cl_num, cm);

  gcc_assert (cloned_node);
  if (!cloned_node)
    return NULL;

  node_to_clone.put (cnode, cloned_node);
  clone_to_node.put (cloned_node, cnode);

  adjust_recursive_callees (cloned_node, cloned_node, cnode);
  symtab->call_cgraph_duplication_hooks (cnode, cloned_node);

  adjust_profile_info (cloned_node, cnode);
  /* Inline clones are created iff their inlined_to == CNODE.  */
  inline_clones (cloned_node, cnode);

  return cloned_node;
}

/* Accumulate frequency of all edges from EDGE->caller to EDGE->callee.  */

static sreal
accumulate_incoming_edge_frequency (cgraph_edge *edge)
{
  sreal count = 0;
  struct cgraph_edge *e;
  for (e = edge->callee->callers; e; e = e->next_caller)
    {
      /* Make a local decision about all edges for EDGE->caller but not the
	 other nodes already in the partition.  Their edges will be visited
	 later or may have been visited before and not fit the
	 cut-off criteria.  */
      if (e->caller == edge->caller)
	count += e->sreal_frequency ();
    }
  return count;
}

/* Determine if EDGE->CALLEE is suitable for cloning.  It is assummed that the
   callee is not an inlined node.  */

static bool
suitable_for_locality_cloning_p (cgraph_edge *edge,
				 lto_locality_cloning_model cm)
{
  cgraph_node *node = edge->callee;
  if (!node->versionable)
    return false;

  /* Out-of-line locality clones of ipcp or sra clones will be created in this
     pass after IPA inline is run.  A locality clone has the same function
     body and the same updated signature as the ipcp/sra clone.
     This fails or asserts based on how the clone is created:
     1. If param_adjustments and tree_map are not recorded for locality clone:
	clone materialization (tree_function_versioning ()) fails when
	updating signature and remapping calls because clone_of (ipcp/sra
	clone) and locality clone differ in param information.
     2. If param_adjustments and tree_map are provided: asserts are triggered
	in fnsummary duplication because IPA inline resets some summaries.

     One inelegant solution is to provide param_adjustments and tree_map, and
     then set clone_of to ipcp/sra clone's clone_of.  However, this sometimes
     results in segmentation fault when the compiled program is run.
     Disabling clone of clones altogether for now with an aim to resolve this
     is future.  */
  if (node->clone_of)
    return false;

  if (node->alias)
    return false;

  if (edge->recursive_p ())
    return false;

  if (!node->definition)
    return false;

  /* Don't clone NODE if IPA count of NODE or EDGE is zero.  */
  if (!node->count.ipa ().nonzero_p () || !edge->count.ipa ().nonzero_p ())
    return false;

  if (cm == LTO_LOCALITY_NON_INTERPOSABLE_CLONING)
    {
      /* Interposability may change on edge basis.  */
      enum availability avail;
      edge->callee->ultimate_alias_target (&avail, edge->caller);
      if (avail <= AVAIL_INTERPOSABLE)
	return false;
    }

  return true;
}

/* Map from caller to all callees already visited for partitioning.  */
hash_map<cgraph_node *, auto_vec<cgraph_node *> > caller_to_callees;

/* Partition EDGE->CALLEE into PARTITION or clone if already partitioned and
   satisfies cloning criteria such as CLONING_MODEL, REAL_FREQ and SIZE
   cut-offs and CLONE_FURTHER_P set by previous caller.  */

/* callgraph can have multiple caller to callee edges for multiple callsites
   For the first such edge, we make decisions about cutoffs and cloning because
   we redirect ALL callsites to cloned callee, not just one of them.  */

static void
partition_callchain (cgraph_edge *edge, locality_partition partition,
		     bool clone_further_p,
		     lto_locality_cloning_model cloning_model,
		     double freq_cutoff, int size, int &cl_num)
{
  /* Aliases are added in the same partition as their targets.
     Aliases are not cloned and their callees are not processed separately.  */
  cgraph_node *node = edge->callee->ultimate_alias_target ();
  cgraph_node *caller = edge->caller;
  cgraph_node *caller_node = node, *cl_node = NULL;

  /* Already visited the caller to callee edges.  */
  auto_vec<cgraph_node *> &callees = caller_to_callees.get_or_insert (caller);
  if (std::find (callees.begin (), callees.end (), node) != callees.end ())
    return;

  callees.safe_push (node);

  if (node->get_partitioning_class () == SYMBOL_PARTITION)
    {
      if (!node_partitioned_p (node))
	{
	  add_node_to_partition (partition, node);
	  if (dump_file)
	    fprintf (dump_file, "Partitioned node: %s\n",
		     node->dump_asm_name ());
	}
      else if (cloning_model >= LTO_LOCALITY_NON_INTERPOSABLE_CLONING
	       && !node_in_partition_p (partition, node))
	{
	  /* Non-inlined node, or alias, already partitioned
	     If cut-off, don't clone callees but partition unpartitioned
	     callees.
	     size is node + inlined nodes.  */
	  if (clone_further_p)
	    {
	      if (!node->alias)
		if (ipa_size_summaries->get (node)->size >= size)
		  clone_further_p = false;

	      if (freq_cutoff != 0.0)
		{
		  sreal acc_freq = accumulate_incoming_edge_frequency (edge);
		  if (acc_freq.to_double () < freq_cutoff)
		    clone_further_p = false;
		}
	    }

	  if (!suitable_for_locality_cloning_p (edge, cloning_model))
	    clone_further_p = false;

	  if (clone_further_p)
	    {
	      /* Try to clone NODE and its inline chain.  */
	      if (dump_file)
		fprintf (dump_file, "Cloning node: %s\n",
			 node->dump_asm_name ());
	      cl_node = clone_node_as_needed (edge, partition, cl_num,
					      cloning_model);
	      if (cl_node)
		{
		  add_node_to_partition (partition, cl_node);
		  caller_node = cl_node;
		}
	      else
		caller_node = NULL;
	    }
	}
    }
  else if (!node->inlined_to)
    return;

  if (caller_node)
    for (cgraph_edge *e = caller_node->callees; e; e = e->next_callee)
      partition_callchain (e, partition, clone_further_p, cloning_model,
			   freq_cutoff, size, cl_num);
}

/* Determine whether NODE is an entrypoint to a callchain.  */

static bool
is_entry_node_p (cgraph_node *node)
{
  /* node->inlined_to is returned as SYMBOL_DUPLICATE.  */
  if (node->get_partitioning_class () != SYMBOL_PARTITION)
    return false;

  if (!node->callers)
    return true;

  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    {
      if (! e->recursive_p ())
	return false;
    }
  if (node->alias
      && !is_entry_node_p (node->ultimate_alias_target ()))
    return false;
  return true;
}

/* Determine order of all external nodes if PGO profile is available.
   Store the order in ORDER.  */

static bool
locality_determine_ipa_order (auto_vec<locality_order *> *order)
{
  struct cgraph_node *node;
  auto_vec<locality_order *> non_comparable_nodes;
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->get_partitioning_class () == SYMBOL_PARTITION)
      {
	if (node->no_reorder)
	  {
	    if (dump_file)
	      fprintf (dump_file, "no reorder %s\n", node->dump_asm_name ());
	    return false;
	  }
	else if (is_entry_node_p (node))
	  {
	    profile_count pcnt = node->count.ipa ();
	    if (!pcnt.initialized_p () || !pcnt.ipa_p ())
	      {
		sreal cnt = 0;
		locality_order *lo = new locality_order (node, cnt);
		non_comparable_nodes.safe_push (lo);
		continue;
	      }
	    sreal count = 0;
	    struct cgraph_edge *edge;
	    for (edge = node->callees; edge; edge = edge->next_callee)
	      {
		/* For PGO, frequency is not used in
		   compare_edge_profile_counts (), it's used only as part of
		   static profile order.  */
		sreal freq = edge->sreal_frequency ();
		count += freq;
	      }
	    locality_order *cl = new locality_order (node, count);
	    order->safe_push (cl);
	  }
      }
  order->qsort (compare_edge_profile_counts);
  for (auto el : non_comparable_nodes)
    order->safe_push (el);
  return true;
}

/* Determine order of all external nodes if only static profile is available.
   Store the order in ORDER.  */

static bool
locality_determine_static_order (auto_vec<locality_order *> *order)
{
  struct cgraph_node *node;
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->get_partitioning_class () == SYMBOL_PARTITION)
      {
	if (node->no_reorder)
	  {
	    if (dump_file)
	      fprintf (dump_file, "no reorder %s\n", node->dump_asm_name ());
	    return false;
	  }
	else if (is_entry_node_p (node))
	  {
	    sreal count = 0;
	    struct cgraph_edge *edge;
	    for (edge = node->callees; edge; edge = edge->next_callee)
	      {
		sreal freq = edge->sreal_frequency ();
		count += freq;
	      }
	    locality_order *cl = new locality_order (node, count);
	    order->safe_push (cl);
	  }
      }
  order->qsort (static_profile_cmp);
  return true;
}

/* Partitioning for code locality.
   1. Create and sort callchains.  If PGO is available, use real profile
   counts.  Otherwise, use a set of heuristics to sort the callchains.
   2. Partition the external nodes and their callchains in the determined order
      2.1. If !partition, partition, else try and clone if it satisfies cloning
      criteria.
   3. Partition all other unpartitioned nodes.  */

static void
locality_partition_and_clone (int max_locality_partition_size,
			      lto_locality_cloning_model cloning_model,
			      int freq_denominator, int size)
{
  locality_partition partition;
  int npartitions = 0;

  auto_vec<locality_order *> order;
  auto_vec<varpool_node *> varpool_order;
  struct cgraph_node *node;
  bool order_p;

  int cl_num = 0;

  double real_freq = 0.0;
  if (freq_denominator > 0)
    real_freq = 1.0 / (double) freq_denominator;

  cgraph_node *n = symtab->first_defined_function ();
  if (n && n->count.ipa_p ())
    order_p = locality_determine_ipa_order (&order);
  else
    order_p = locality_determine_static_order (&order);
  if (!order_p)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Locality partition: falling back to balanced"
			      "model\n");
	}

      return;
    }

  int64_t partition_size
    = max_locality_partition_size
	? max_locality_partition_size : param_max_partition_size;
  partition = create_partition (npartitions);

  for (unsigned i = 0; i < order.length (); i++)
    {
      node = order[i]->node;
      if (node_partitioned_p (node))
	continue;

      if (partition->insns > partition_size)
	partition = create_partition (npartitions);
      if (dump_file)
	fprintf (dump_file, "Partition id: %d\n", partition->part_id);

      add_node_to_partition (partition, node);
      if (dump_file)
	fprintf (dump_file, "Ordered Node: %s\n", node->dump_asm_name ());

      for (cgraph_edge *edge = node->callees; edge; edge = edge->next_callee)
	{
	  /* Recursively partition the callchain of edge->callee.  */
	  partition_callchain (edge, partition, true, cloning_model, real_freq,
			       size, cl_num);
	}
    }

  for (unsigned i = 0; i < order.length (); i++)
    delete order[i];
  order = vNULL;
}

/* Entry point to locality-clone pass.  */
static int
lc_execute (void)
{
  symtab_node *node;
  FOR_EACH_SYMBOL (node)
    node->aux = NULL;

  locality_partition_and_clone (param_max_locality_partition_size,
				flag_lto_locality_cloning,
				param_lto_locality_frequency,
				param_lto_locality_size);

  FOR_EACH_SYMBOL (node)
    node->aux = NULL;
  return 0;
}

namespace {

const pass_data pass_data_ipa_locality_clone = {
  IPA_PASS,				      /* type */
  "locality-clone",			      /* name */
  OPTGROUP_NONE,			      /* optinfo_flags */
  TV_IPA_LC,				      /* tv_id */
  0,					      /* properties_required */
  0,					      /* properties_provided */
  0,					      /* properties_destroyed */
  0,					      /* todo_flags_start */
  (TODO_dump_symtab | TODO_remove_functions), /* todo_flags_finish */
};

class pass_ipa_locality_cloning : public ipa_opt_pass_d
{
public:
  pass_ipa_locality_cloning (gcc::context *ctxt)
    : ipa_opt_pass_d (pass_data_ipa_locality_clone, ctxt,
		      NULL, /* generate_summary */
		      NULL, /* write_summary */
		      NULL, /* read_summary */
		      NULL, /* write_optimization_summary */
		      NULL, /* read_optimization_summary */
		      NULL, /* stmt_fixup */
		      0,    /* function_transform_todo_flags_start */
		      NULL, /* function_transform */
		      NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return (flag_wpa && flag_ipa_reorder_for_locality);
  }

  virtual unsigned int execute (function *) { return lc_execute (); }

}; // class pass_ipa_locality_cloning

} // namespace

ipa_opt_pass_d *
make_pass_ipa_locality_cloning (gcc::context *ctxt)
{
  return new pass_ipa_locality_cloning (ctxt);
}
