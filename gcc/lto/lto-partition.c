/* LTO partitioning logic routines.
   Copyright 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

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
#include "toplev.h"
#include "tree.h"
#include "tm.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "timevar.h"
#include "params.h"
#include "ipa-inline.h"
#include "ipa-utils.h"
#include "lto-partition.h"

VEC(ltrans_partition, heap) *ltrans_partitions;

static void add_cgraph_node_to_partition (ltrans_partition part, struct cgraph_node *node);
static void add_varpool_node_to_partition (ltrans_partition part, struct varpool_node *vnode);

/* Create new partition with name NAME.  */
static ltrans_partition
new_partition (const char *name)
{
  ltrans_partition part = XCNEW (struct ltrans_partition_def);
  part->cgraph_set = cgraph_node_set_new ();
  part->varpool_set = varpool_node_set_new ();
  part->name = name;
  part->insns = 0;
  VEC_safe_push (ltrans_partition, heap, ltrans_partitions, part);
  return part;
}

/* Free memory used by ltrans datastructures.  */
void
free_ltrans_partitions (void)
{
  unsigned int idx;
  ltrans_partition part;
  for (idx = 0; VEC_iterate (ltrans_partition, ltrans_partitions, idx, part); idx++)
    {
      free_cgraph_node_set (part->cgraph_set);
      free (part);
    }
  VEC_free (ltrans_partition, heap, ltrans_partitions);
}

/* See all references that go to comdat objects and bring them into partition too.
   Also see all aliases of the newly added entry and bring them, too.  */
static void
add_references_to_partition (ltrans_partition part, struct ipa_ref_list *refs)
{
  int i;
  struct ipa_ref *ref;
  for (i = 0; ipa_ref_list_reference_iterate (refs, i, ref); i++)
    {
      if (symtab_function_p (ref->referred)
	  && (DECL_COMDAT (cgraph_function_node (ipa_ref_node (ref),
			   NULL)->symbol.decl)
	      || (ref->use == IPA_REF_ALIAS
		  && lookup_attribute
		       ("weakref", DECL_ATTRIBUTES (ipa_ref_node (ref)->symbol.decl))))
	  && !cgraph_node_in_set_p (ipa_ref_node (ref), part->cgraph_set))
	add_cgraph_node_to_partition (part, ipa_ref_node (ref));
      else
        if (symtab_variable_p (ref->referred)
	    && (DECL_COMDAT (ipa_ref_varpool_node (ref)->symbol.decl)
		|| DECL_EXTERNAL (ipa_ref_varpool_node (ref)->symbol.decl)
	        || (ref->use == IPA_REF_ALIAS
		    && lookup_attribute
		         ("weakref",
			  DECL_ATTRIBUTES (ipa_ref_varpool_node (ref)->symbol.decl))))
	    && !varpool_node_in_set_p (ipa_ref_varpool_node (ref),
				       part->varpool_set))
	  add_varpool_node_to_partition (part, ipa_ref_varpool_node (ref));
    }
  for (i = 0; ipa_ref_list_referring_iterate (refs, i, ref); i++)
    {
      if (symtab_function_p (ref->referring)
	  && ref->use == IPA_REF_ALIAS
	  && !cgraph_node_in_set_p (ipa_ref_referring_node (ref),
				    part->cgraph_set)
	  && !lookup_attribute ("weakref",
				DECL_ATTRIBUTES
				  (ipa_ref_referring_node (ref)->symbol.decl)))
	add_cgraph_node_to_partition (part, ipa_ref_referring_node (ref));
      else
        if (symtab_variable_p (ref->referring)
	    && ref->use == IPA_REF_ALIAS
	    && !varpool_node_in_set_p (ipa_ref_referring_varpool_node (ref),
				       part->varpool_set)
	    && !lookup_attribute ("weakref",
				  DECL_ATTRIBUTES
				    (ipa_ref_referring_varpool_node (ref)->symbol.decl)))
	  add_varpool_node_to_partition (part,
					 ipa_ref_referring_varpool_node (ref));
    }
}

/* Worker for add_cgraph_node_to_partition.  */

static bool
add_cgraph_node_to_partition_1 (struct cgraph_node *node, void *data)
{
  ltrans_partition part = (ltrans_partition) data;

  /* non-COMDAT aliases of COMDAT functions needs to be output just once.  */
  if (!DECL_COMDAT (node->symbol.decl)
      && !node->global.inlined_to
      && node->symbol.aux)
    {
      gcc_assert (node->thunk.thunk_p || node->alias);
      return false;
    }

  if (node->symbol.aux)
    {
      node->symbol.in_other_partition = 1;
      if (cgraph_dump_file)
        fprintf (cgraph_dump_file, "Node %s/%i now used in multiple partitions\n",
		 cgraph_node_name (node), node->uid);
    }
  node->symbol.aux = (void *)((size_t)node->symbol.aux + 1);
  cgraph_node_set_add (part->cgraph_set, node);
  return false;
}

/* Add NODE to partition as well as the inline callees and referred comdats into partition PART. */

static void
add_cgraph_node_to_partition (ltrans_partition part, struct cgraph_node *node)
{
  struct cgraph_edge *e;
  cgraph_node_set_iterator csi;
  struct cgraph_node *n;

  /* If NODE is already there, we have nothing to do.  */
  csi = cgraph_node_set_find (part->cgraph_set, node);
  if (!csi_end_p (csi))
    return;

  cgraph_for_node_thunks_and_aliases (node, add_cgraph_node_to_partition_1, part, true);

  part->insns += inline_summary (node)->self_size;


  cgraph_node_set_add (part->cgraph_set, node);

  for (e = node->callees; e; e = e->next_callee)
    if ((!e->inline_failed
	 || DECL_COMDAT (cgraph_function_node (e->callee, NULL)->symbol.decl))
	&& !cgraph_node_in_set_p (e->callee, part->cgraph_set))
      add_cgraph_node_to_partition (part, e->callee);

  /* The only way to assemble non-weakref alias is to add the aliased object into
     the unit.  */
  add_references_to_partition (part, &node->symbol.ref_list);
  n = cgraph_function_node (node, NULL);
  if (n != node
      && !lookup_attribute ("weakref",
			    DECL_ATTRIBUTES (node->symbol.decl)))
    add_cgraph_node_to_partition (part, n);

  if (node->symbol.same_comdat_group)
    for (n = cgraph (node->symbol.same_comdat_group);
	 n != node; n = cgraph (n->symbol.same_comdat_group))
      add_cgraph_node_to_partition (part, n);
}

/* Add VNODE to partition as well as comdat references partition PART. */

static void
add_varpool_node_to_partition (ltrans_partition part, struct varpool_node *vnode)
{
  varpool_node_set_iterator vsi;
  struct varpool_node *v;

  /* If NODE is already there, we have nothing to do.  */
  vsi = varpool_node_set_find (part->varpool_set, vnode);
  if (!vsi_end_p (vsi))
    return;

  varpool_node_set_add (part->varpool_set, vnode);

  if (vnode->symbol.aux)
    {
      vnode->symbol.in_other_partition = 1;
      if (cgraph_dump_file)
        fprintf (cgraph_dump_file, "Varpool node %s now used in multiple partitions\n",
		 varpool_node_name (vnode));
    }
  vnode->symbol.aux = (void *)((size_t)vnode->symbol.aux + 1);

  /* The only way to assemble non-weakref alias is to add the aliased object into
     the unit.  */
  v = varpool_variable_node (vnode, NULL);
  if (v != vnode
      && !lookup_attribute ("weakref",
			    DECL_ATTRIBUTES (vnode->symbol.decl)))
    add_varpool_node_to_partition (part, v);

  add_references_to_partition (part, &vnode->symbol.ref_list);

  if (vnode->symbol.same_comdat_group
      && !varpool_node_in_set_p (varpool (vnode->symbol.same_comdat_group),
				 part->varpool_set))
    add_varpool_node_to_partition (part, varpool (vnode->symbol.same_comdat_group));
}

/* Undo all additions until number of cgraph nodes in PARITION is N_CGRAPH_NODES
   and number of varpool nodes is N_VARPOOL_NODES.  */

static void
undo_partition (ltrans_partition partition, unsigned int n_cgraph_nodes,
		unsigned int n_varpool_nodes)
{
  while (VEC_length (cgraph_node_ptr, partition->cgraph_set->nodes) >
	 n_cgraph_nodes)
    {
      struct cgraph_node *node = VEC_index (cgraph_node_ptr,
					    partition->cgraph_set->nodes,
					    n_cgraph_nodes);
      partition->insns -= inline_summary (node)->self_size;
      cgraph_node_set_remove (partition->cgraph_set, node);
      node->symbol.aux = (void *)((size_t)node->symbol.aux - 1);
    }
  while (VEC_length (varpool_node_ptr, partition->varpool_set->nodes) >
	 n_varpool_nodes)
    {
      struct varpool_node *node = VEC_index (varpool_node_ptr,
					     partition->varpool_set->nodes,
					     n_varpool_nodes);
      varpool_node_set_remove (partition->varpool_set, node);
      node->symbol.aux = (void *)((size_t)node->symbol.aux - 1);
    }
}

/* Return true if NODE should be partitioned.
   This means that partitioning algorithm should put NODE into one of partitions.
   This apply to most functions with bodies.  Functions that are not partitions
   are put into every unit needing them.  This is the case of i.e. COMDATs.  */

static bool
partition_cgraph_node_p (struct cgraph_node *node)
{
  /* We will get proper partition based on function they are inlined to.  */
  if (node->global.inlined_to)
    return false;
  /* Nodes without a body do not need partitioning.  */
  if (!node->analyzed)
    return false;
  /* Extern inlines and comdat are always only in partitions they are needed.  */
  if (DECL_EXTERNAL (node->symbol.decl)
      || (DECL_COMDAT (node->symbol.decl)
	  && !node->symbol.force_output
	  && !symtab_used_from_object_file_p ((symtab_node) node)))
    return false;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (node->symbol.decl)))
    return false;
  return true;
}

/* Return true if VNODE should be partitioned. 
   This means that partitioning algorithm should put VNODE into one of partitions. */

static bool
partition_varpool_node_p (struct varpool_node *vnode)
{
  if (vnode->alias || !vnode->analyzed)
    return false;
  /* Constant pool and comdat are always only in partitions they are needed.  */
  if (DECL_IN_CONSTANT_POOL (vnode->symbol.decl)
      || DECL_EXTERNAL (vnode->symbol.decl)
      || (DECL_COMDAT (vnode->symbol.decl)
	  && !vnode->symbol.force_output
	  && !symtab_used_from_object_file_p ((symtab_node) vnode)))
    return false;
  if (lookup_attribute ("weakref", DECL_ATTRIBUTES (vnode->symbol.decl)))
    return false;
  return true;
}

/* Group cgrah nodes by input files.  This is used mainly for testing
   right now.  */

void
lto_1_to_1_map (void)
{
  struct cgraph_node *node;
  struct varpool_node *vnode;
  struct lto_file_decl_data *file_data;
  struct pointer_map_t *pmap;
  ltrans_partition partition;
  void **slot;
  int npartitions = 0;

  timevar_push (TV_WHOPR_WPA);

  pmap = pointer_map_create ();

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      if (!partition_cgraph_node_p (node)
	  || node->symbol.aux)
	continue;

      file_data = node->symbol.lto_file_data;

      if (file_data)
	{
          slot = pointer_map_contains (pmap, file_data);
          if (slot)
	    partition = (ltrans_partition) *slot;
	  else
	    {
	      partition = new_partition (file_data->file_name);
	      slot = pointer_map_insert (pmap, file_data);
	      *slot = partition;
	      npartitions++;
	    }
	}
      else if (!file_data
	       && VEC_length (ltrans_partition, ltrans_partitions))
	partition = VEC_index (ltrans_partition, ltrans_partitions, 0);
      else
	{
	  partition = new_partition ("");
	  slot = pointer_map_insert (pmap, NULL);
	  *slot = partition;
	  npartitions++;
	}

      add_cgraph_node_to_partition (partition, node);
    }

  FOR_EACH_VARIABLE (vnode)
    {
      if (!partition_varpool_node_p (vnode)
	  || vnode->symbol.aux)
	continue;
      file_data = vnode->symbol.lto_file_data;
      slot = pointer_map_contains (pmap, file_data);
      if (slot)
	partition = (ltrans_partition) *slot;
      else
	{
	  partition = new_partition (file_data->file_name);
	  slot = pointer_map_insert (pmap, file_data);
	  *slot = partition;
	  npartitions++;
	}

      add_varpool_node_to_partition (partition, vnode);
    }
  FOR_EACH_FUNCTION (node)
    node->symbol.aux = NULL;
  FOR_EACH_VARIABLE (vnode)
    vnode->symbol.aux = NULL;

  /* If the cgraph is empty, create one cgraph node set so that there is still
     an output file for any variables that need to be exported in a DSO.  */
  if (!npartitions)
    new_partition ("empty");

  pointer_map_destroy (pmap);

  timevar_pop (TV_WHOPR_WPA);

  lto_stats.num_cgraph_partitions += VEC_length (ltrans_partition, 
						 ltrans_partitions);
}

/* Helper function for qsort; sort nodes by order.  */
static int
node_cmp (const void *pa, const void *pb)
{
  const struct cgraph_node *a = *(const struct cgraph_node * const *) pa;
  const struct cgraph_node *b = *(const struct cgraph_node * const *) pb;
  return b->symbol.order - a->symbol.order;
}

/* Helper function for qsort; sort nodes by order.  */
static int
varpool_node_cmp (const void *pa, const void *pb)
{
  const struct varpool_node *a = *(const struct varpool_node * const *) pa;
  const struct varpool_node *b = *(const struct varpool_node * const *) pb;
  return b->symbol.order - a->symbol.order;
}

/* Group cgraph nodes into equally-sized partitions.

   The partitioning algorithm is simple: nodes are taken in predefined order.
   The order corresponds to the order we want functions to have in the final
   output.  In the future this will be given by function reordering pass, but
   at the moment we use the topological order, which is a good approximation.

   The goal is to partition this linear order into intervals (partitions) so
   that all the partitions have approximately the same size and the number of
   callgraph or IPA reference edges crossing boundaries is minimal.

   This is a lot faster (O(n) in size of callgraph) than algorithms doing
   priority-based graph clustering that are generally O(n^2) and, since
   WHOPR is designed to make things go well across partitions, it leads
   to good results.

   We compute the expected size of a partition as:

     max (total_size / lto_partitions, min_partition_size)

   We use dynamic expected size of partition so small programs are partitioned
   into enough partitions to allow use of multiple CPUs, while large programs
   are not partitioned too much.  Creating too many partitions significantly
   increases the streaming overhead.

   In the future, we would like to bound the maximal size of partitions so as
   to prevent the LTRANS stage from consuming too much memory.  At the moment,
   however, the WPA stage is the most memory intensive for large benchmarks,
   since too many types and declarations are read into memory.

   The function implements a simple greedy algorithm.  Nodes are being added
   to the current partition until after 3/4 of the expected partition size is
   reached.  Past this threshold, we keep track of boundary size (number of
   edges going to other partitions) and continue adding functions until after
   the current partition has grown to twice the expected partition size.  Then
   the process is undone to the point where the minimal ratio of boundary size
   and in-partition calls was reached.  */

void
lto_balanced_map (void)
{
  int n_nodes = 0;
  int n_varpool_nodes = 0, varpool_pos = 0;
  struct cgraph_node **postorder =
    XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  struct cgraph_node **order = XNEWVEC (struct cgraph_node *, cgraph_max_uid);
  struct varpool_node **varpool_order = NULL;
  int i, postorder_len;
  struct cgraph_node *node;
  int total_size = 0, best_total_size = 0;
  int partition_size;
  ltrans_partition partition;
  unsigned int last_visited_cgraph_node = 0, last_visited_varpool_node = 0;
  struct varpool_node *vnode;
  int cost = 0, internal = 0;
  int best_n_nodes = 0, best_n_varpool_nodes = 0, best_i = 0, best_cost =
    INT_MAX, best_internal = 0;
  int npartitions;
  int current_order = -1;

  FOR_EACH_VARIABLE (vnode)
    gcc_assert (!vnode->symbol.aux);
  /* Until we have better ordering facility, use toplogical order.
     Include only nodes we will partition and compute estimate of program
     size.  Note that since nodes that are not partitioned might be put into
     multiple partitions, this is just an estimate of real size.  This is why
     we keep partition_size updated after every partition is finalized.  */
  postorder_len = ipa_reverse_postorder (postorder);
    
  for (i = 0; i < postorder_len; i++)
    {
      node = postorder[i];
      if (partition_cgraph_node_p (node))
	{
	  order[n_nodes++] = node;
          total_size += inline_summary (node)->size;
	}
    }
  free (postorder);

  if (!flag_toplevel_reorder)
    {
      qsort (order, n_nodes, sizeof (struct cgraph_node *), node_cmp);

      FOR_EACH_VARIABLE (vnode)
	if (partition_varpool_node_p (vnode))
	  n_varpool_nodes++;
      varpool_order = XNEWVEC (struct varpool_node *, n_varpool_nodes);

      n_varpool_nodes = 0;
      FOR_EACH_VARIABLE (vnode)
	if (partition_varpool_node_p (vnode))
	  varpool_order[n_varpool_nodes++] = vnode;
      qsort (varpool_order, n_varpool_nodes, sizeof (struct varpool_node *),
	     varpool_node_cmp);
    }

  /* Compute partition size and create the first partition.  */
  partition_size = total_size / PARAM_VALUE (PARAM_LTO_PARTITIONS);
  if (partition_size < PARAM_VALUE (MIN_PARTITION_SIZE))
    partition_size = PARAM_VALUE (MIN_PARTITION_SIZE);
  npartitions = 1;
  partition = new_partition ("");
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Total unit size: %i, partition size: %i\n",
	     total_size, partition_size);

  for (i = 0; i < n_nodes; i++)
    {
      if (order[i]->symbol.aux)
	continue;

      current_order = order[i]->symbol.order;

      if (!flag_toplevel_reorder)
	while (varpool_pos < n_varpool_nodes
	       && varpool_order[varpool_pos]->symbol.order < current_order)
	  {
	    if (!varpool_order[varpool_pos]->symbol.aux)
	      add_varpool_node_to_partition (partition, varpool_order[varpool_pos]);
	    varpool_pos++;
	  }

      add_cgraph_node_to_partition (partition, order[i]);
      total_size -= inline_summary (order[i])->size;
	  

      /* Once we added a new node to the partition, we also want to add
         all referenced variables unless they was already added into some
         earlier partition.
	 add_cgraph_node_to_partition adds possibly multiple nodes and
	 variables that are needed to satisfy needs of ORDER[i].
         We remember last visited cgraph and varpool node from last iteration
         of outer loop that allows us to process every new addition. 

	 At the same time we compute size of the boundary into COST.  Every
         callgraph or IPA reference edge leaving the partition contributes into
         COST.  Every edge inside partition was earlier computed as one leaving
	 it and thus we need to subtract it from COST.  */
      while (last_visited_cgraph_node <
	     VEC_length (cgraph_node_ptr, partition->cgraph_set->nodes)
	     || last_visited_varpool_node < VEC_length (varpool_node_ptr,
							partition->varpool_set->
							nodes))
	{
	  struct ipa_ref_list *refs;
	  int j;
	  struct ipa_ref *ref;
	  bool cgraph_p = false;

	  if (last_visited_cgraph_node <
	      VEC_length (cgraph_node_ptr, partition->cgraph_set->nodes))
	    {
	      struct cgraph_edge *edge;

	      cgraph_p = true;
	      node = VEC_index (cgraph_node_ptr, partition->cgraph_set->nodes,
				last_visited_cgraph_node);
	      refs = &node->symbol.ref_list;

	      last_visited_cgraph_node++;

	      gcc_assert (node->analyzed);

	      /* Compute boundary cost of callgraph edges.  */
	      for (edge = node->callees; edge; edge = edge->next_callee)
		if (edge->callee->analyzed)
		  {
		    int edge_cost = edge->frequency;
		    cgraph_node_set_iterator csi;

		    if (!edge_cost)
		      edge_cost = 1;
		    gcc_assert (edge_cost > 0);
		    csi = cgraph_node_set_find (partition->cgraph_set, edge->callee);
		    if (!csi_end_p (csi)
		        && csi.index < last_visited_cgraph_node - 1)
		      cost -= edge_cost, internal+= edge_cost;
		    else
		      cost += edge_cost;
		  }
	      for (edge = node->callers; edge; edge = edge->next_caller)
		{
		  int edge_cost = edge->frequency;
		  cgraph_node_set_iterator csi;

		  gcc_assert (edge->caller->analyzed);
		  if (!edge_cost)
		    edge_cost = 1;
		  gcc_assert (edge_cost > 0);
		  csi = cgraph_node_set_find (partition->cgraph_set, edge->caller);
		  if (!csi_end_p (csi)
		      && csi.index < last_visited_cgraph_node)
		    cost -= edge_cost;
		  else
		    cost += edge_cost;
		}
	    }
	  else
	    {
	      refs =
		&VEC_index (varpool_node_ptr, partition->varpool_set->nodes,
			    last_visited_varpool_node)->symbol.ref_list;
	      last_visited_varpool_node++;
	    }

	  /* Compute boundary cost of IPA REF edges and at the same time look into
	     variables referenced from current partition and try to add them.  */
	  for (j = 0; ipa_ref_list_reference_iterate (refs, j, ref); j++)
	    if (symtab_variable_p (ref->referred))
	      {
		varpool_node_set_iterator vsi;

		vnode = ipa_ref_varpool_node (ref);
		if (!vnode->finalized)
		  continue;
		if (!vnode->symbol.aux && flag_toplevel_reorder
		    && partition_varpool_node_p (vnode))
		  add_varpool_node_to_partition (partition, vnode);
		vsi = varpool_node_set_find (partition->varpool_set, vnode);
		if (!vsi_end_p (vsi)
		    && vsi.index < last_visited_varpool_node - !cgraph_p)
		  cost--, internal++;
		else
		  cost++;
	      }
	    else
	      {
		cgraph_node_set_iterator csi;

		node = ipa_ref_node (ref);
		if (!node->analyzed)
		  continue;
		csi = cgraph_node_set_find (partition->cgraph_set, node);
		if (!csi_end_p (csi)
		    && csi.index < last_visited_cgraph_node - cgraph_p)
		  cost--, internal++;
		else
		  cost++;
	      }
	  for (j = 0; ipa_ref_list_referring_iterate (refs, j, ref); j++)
	    if (symtab_variable_p (ref->referring))
	      {
		varpool_node_set_iterator vsi;

		vnode = ipa_ref_referring_varpool_node (ref);
		gcc_assert (vnode->finalized);
		if (!vnode->symbol.aux && flag_toplevel_reorder
		    && partition_varpool_node_p (vnode))
		  add_varpool_node_to_partition (partition, vnode);
		vsi = varpool_node_set_find (partition->varpool_set, vnode);
		if (!vsi_end_p (vsi)
		    && vsi.index < last_visited_varpool_node)
		  cost--;
		else
		  cost++;
	      }
	    else
	      {
		cgraph_node_set_iterator csi;

		node = ipa_ref_referring_node (ref);
		gcc_assert (node->analyzed);
		csi = cgraph_node_set_find (partition->cgraph_set, node);
		if (!csi_end_p (csi)
		    && csi.index < last_visited_cgraph_node)
		  cost--;
		else
		  cost++;
	      }
	}

      /* If the partition is large enough, start looking for smallest boundary cost.  */
      if (partition->insns < partition_size * 3 / 4
	  || best_cost == INT_MAX
	  || ((!cost 
	       || (best_internal * (HOST_WIDE_INT) cost
		   > (internal * (HOST_WIDE_INT)best_cost)))
  	      && partition->insns < partition_size * 5 / 4))
	{
	  best_cost = cost;
	  best_internal = internal;
	  best_i = i;
	  best_n_nodes = VEC_length (cgraph_node_ptr,
				     partition->cgraph_set->nodes);
	  best_n_varpool_nodes = VEC_length (varpool_node_ptr,
					     partition->varpool_set->nodes);
	  best_total_size = total_size;
	}
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file, "Step %i: added %s/%i, size %i, cost %i/%i best %i/%i, step %i\n", i,
		 cgraph_node_name (order[i]), order[i]->uid, partition->insns, cost, internal,
		 best_cost, best_internal, best_i);
      /* Partition is too large, unwind into step when best cost was reached and
	 start new partition.  */
      if (partition->insns > 2 * partition_size)
	{
	  if (best_i != i)
	    {
	      if (cgraph_dump_file)
		fprintf (cgraph_dump_file, "Unwinding %i insertions to step %i\n",
			 i - best_i, best_i);
	      undo_partition (partition, best_n_nodes, best_n_varpool_nodes);
	    }
	  i = best_i;
 	  /* When we are finished, avoid creating empty partition.  */
	  while (i < n_nodes - 1 && order[i + 1]->symbol.aux)
	    i++;
	  if (i == n_nodes - 1)
	    break;
	  partition = new_partition ("");
	  last_visited_cgraph_node = 0;
	  last_visited_varpool_node = 0;
	  total_size = best_total_size;
	  cost = 0;

	  if (cgraph_dump_file)
	    fprintf (cgraph_dump_file, "New partition\n");
	  best_n_nodes = 0;
	  best_n_varpool_nodes = 0;
	  best_cost = INT_MAX;

	  /* Since the size of partitions is just approximate, update the size after
	     we finished current one.  */
	  if (npartitions < PARAM_VALUE (PARAM_LTO_PARTITIONS))
	    partition_size = total_size
	      / (PARAM_VALUE (PARAM_LTO_PARTITIONS) - npartitions);
	  else
	    partition_size = INT_MAX;

	  if (partition_size < PARAM_VALUE (MIN_PARTITION_SIZE))
	    partition_size = PARAM_VALUE (MIN_PARTITION_SIZE);
	  npartitions ++;
	}
    }

  /* Varables that are not reachable from the code go into last partition.  */
  if (flag_toplevel_reorder)
    {
      FOR_EACH_VARIABLE (vnode)
        if (partition_varpool_node_p (vnode) && !vnode->symbol.aux)
	  add_varpool_node_to_partition (partition, vnode);
    }
  else
    {
      while (varpool_pos < n_varpool_nodes)
	{
	  if (!varpool_order[varpool_pos]->symbol.aux)
	    add_varpool_node_to_partition (partition, varpool_order[varpool_pos]);
	  varpool_pos++;
	}
      free (varpool_order);
    }
  free (order);
}

/* Promote variable VNODE to be static.  */

static bool
promote_var (struct varpool_node *vnode)
{
  if (TREE_PUBLIC (vnode->symbol.decl) || DECL_EXTERNAL (vnode->symbol.decl))
    return false;
  gcc_assert (flag_wpa);
  TREE_PUBLIC (vnode->symbol.decl) = 1;
  DECL_VISIBILITY (vnode->symbol.decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (vnode->symbol.decl) = true;
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	    "Promoting var as hidden: %s\n", varpool_node_name (vnode));
  return true;
}

/* Promote function NODE to be static.  */

static bool
promote_fn (struct cgraph_node *node)
{
  gcc_assert (flag_wpa);
  if (TREE_PUBLIC (node->symbol.decl) || DECL_EXTERNAL (node->symbol.decl))
    return false;
  TREE_PUBLIC (node->symbol.decl) = 1;
  DECL_VISIBILITY (node->symbol.decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (node->symbol.decl) = true;
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file,
	     "Promoting function as hidden: %s/%i\n",
	     cgraph_node_name (node), node->uid);
  return true;
}

/* Find out all static decls that need to be promoted to global because
   of cross file sharing.  This function must be run in the WPA mode after
   all inlinees are added.  */

void
lto_promote_cross_file_statics (void)
{
  struct varpool_node *vnode;
  unsigned i, n_sets;
  cgraph_node_set set;
  varpool_node_set vset;
  cgraph_node_set_iterator csi;
  varpool_node_set_iterator vsi;
  VEC(varpool_node_ptr, heap) *promoted_initializers = NULL;
  struct pointer_set_t *inserted = pointer_set_create ();

  gcc_assert (flag_wpa);

  n_sets = VEC_length (ltrans_partition, ltrans_partitions);
  for (i = 0; i < n_sets; i++)
    {
      ltrans_partition part
	= VEC_index (ltrans_partition, ltrans_partitions, i);
      set = part->cgraph_set;
      vset = part->varpool_set;

      /* If node called or referred to from other partition, it needs to be
	 globalized.  */
      for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
	{
	  struct cgraph_node *node = csi_node (csi);
	  if (node->symbol.externally_visible)
	    continue;
	  if (node->global.inlined_to)
	    continue;
	  if ((!DECL_EXTERNAL (node->symbol.decl)
	       && !DECL_COMDAT (node->symbol.decl))
	      && (referenced_from_other_partition_p (&node->symbol.ref_list, set, vset)
		  || reachable_from_other_partition_p (node, set)))
	    promote_fn (node);
	}
      for (vsi = vsi_start (vset); !vsi_end_p (vsi); vsi_next (&vsi))
	{
	  vnode = vsi_node (vsi);
	  /* Constant pool references use internal labels and thus can not
	     be made global.  It is sensible to keep those ltrans local to
	     allow better optimization.  */
	  if (!DECL_IN_CONSTANT_POOL (vnode->symbol.decl)
	      && !DECL_EXTERNAL (vnode->symbol.decl)
	      && !DECL_COMDAT (vnode->symbol.decl)
	      && !vnode->symbol.externally_visible && vnode->analyzed
	      && referenced_from_other_partition_p (&vnode->symbol.ref_list,
						    set, vset))
	    promote_var (vnode);
	}

      /* We export the initializer of a read-only var into each partition
	 referencing the var.  Folding might take declarations from the
	 initializer and use them, so everything referenced from the
	 initializer can be accessed from this partition after folding.

	 This means that we need to promote all variables and functions
	 referenced from all initializers of read-only vars referenced
	 from this partition that are not in this partition.  This needs
	 to be done recursively.  */
      FOR_EACH_VARIABLE (vnode)
	if (const_value_known_p (vnode->symbol.decl)
	    && DECL_INITIAL (vnode->symbol.decl)
	    && !varpool_node_in_set_p (vnode, vset)
	    && referenced_from_this_partition_p (&vnode->symbol.ref_list, set, vset)
	    && !pointer_set_insert (inserted, vnode))
	VEC_safe_push (varpool_node_ptr, heap, promoted_initializers, vnode);

      while (!VEC_empty (varpool_node_ptr, promoted_initializers))
	{
	  int i;
	  struct ipa_ref *ref;

	  vnode = VEC_pop (varpool_node_ptr, promoted_initializers);
	  for (i = 0;
	       ipa_ref_list_reference_iterate (&vnode->symbol.ref_list, i, ref);
	       i++)
	    {
	      if (symtab_function_p (ref->referred))
		{
		  struct cgraph_node *n = ipa_ref_node (ref);
		  gcc_assert (!n->global.inlined_to);
		  if (!n->symbol.externally_visible
		      && !cgraph_node_in_set_p (n, set))
		    promote_fn (n);
		}
	      else
		{
		  struct varpool_node *v = ipa_ref_varpool_node (ref);
		  if (varpool_node_in_set_p (v, vset))
		    continue;

		  /* Constant pool references use internal labels and thus
		     cannot be made global.  It is sensible to keep those
		     ltrans local to allow better optimization.
		     Similarly we ship external vars initializers into
		     every ltrans unit possibly referring to it.  */
		  if (DECL_IN_CONSTANT_POOL (v->symbol.decl)
		      || DECL_EXTERNAL (v->symbol.decl))
		    {
		      if (!pointer_set_insert (inserted, vnode))
			VEC_safe_push (varpool_node_ptr, heap,
				       promoted_initializers, v);
		    }
		  else if (!v->symbol.externally_visible && v->analyzed)
		    {
		      if (promote_var (v)
			  && DECL_INITIAL (v->symbol.decl)
			  && const_value_known_p (v->symbol.decl)
			  && !pointer_set_insert (inserted, vnode))
			VEC_safe_push (varpool_node_ptr, heap,
				       promoted_initializers, v);
		    }
		}
	    }
	}
    }
  pointer_set_destroy (inserted);
}
