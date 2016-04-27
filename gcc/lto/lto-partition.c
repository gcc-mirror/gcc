/* LTO partitioning logic routines.
   Copyright (C) 2009-2016 Free Software Foundation, Inc.

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
#include "target.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "stringpool.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "params.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-inline.h"
#include "lto-partition.h"

vec<ltrans_partition> ltrans_partitions;

static void add_symbol_to_partition (ltrans_partition part, symtab_node *node);


/* Create new partition with name NAME.  */

static ltrans_partition
new_partition (const char *name)
{
  ltrans_partition part = XCNEW (struct ltrans_partition_def);
  part->encoder = lto_symtab_encoder_new (false);
  part->name = name;
  part->insns = 0;
  part->symbols = 0;
  ltrans_partitions.safe_push (part);
  return part;
}

/* Free memory used by ltrans datastructures.  */

void
free_ltrans_partitions (void)
{
  unsigned int idx;
  ltrans_partition part;
  for (idx = 0; ltrans_partitions.iterate (idx, &part); idx++)
    {
      if (part->initializers_visited)
	delete part->initializers_visited;
      /* Symtab encoder is freed after streaming.  */
      free (part);
    }
  ltrans_partitions.release ();
}

/* Return true if symbol is already in some partition.  */

static inline bool
symbol_partitioned_p (symtab_node *node)
{
  return node->aux;
}

/* Add references into the partition.  */
static void
add_references_to_partition (ltrans_partition part, symtab_node *node)
{
  int i;
  struct ipa_ref *ref = NULL;

  /* Add all duplicated references to the partition.  */
  for (i = 0; node->iterate_reference (i, ref); i++)
    if (ref->referred->get_partitioning_class () == SYMBOL_DUPLICATE)
      add_symbol_to_partition (part, ref->referred);
    /* References to a readonly variable may be constant foled into its value.
       Recursively look into the initializers of the constant variable and add
       references, too.  */
    else if (is_a <varpool_node *> (ref->referred)
	     && (dyn_cast <varpool_node *> (ref->referred)
		 ->ctor_useable_for_folding_p ()
		 || POINTER_BOUNDS_P (ref->referred->decl))
	     && !lto_symtab_encoder_in_partition_p (part->encoder, ref->referred))
      {
	if (!part->initializers_visited)
	  part->initializers_visited = new hash_set<symtab_node *>;
	if (!part->initializers_visited->add (ref->referred))
	  add_references_to_partition (part, ref->referred);
      }
}

/* Helper function for add_symbol_to_partition doing the actual dirty work
   of adding NODE to PART.  */

static bool
add_symbol_to_partition_1 (ltrans_partition part, symtab_node *node)
{
  enum symbol_partitioning_class c = node->get_partitioning_class ();
  struct ipa_ref *ref;
  symtab_node *node1;

  /* If NODE is already there, we have nothing to do.  */
  if (lto_symtab_encoder_in_partition_p (part->encoder, node))
    return true;

  /* non-duplicated aliases or tunks of a duplicated symbol needs to be output
     just once.

     Be lax about comdats; they may or may not be duplicated and we may
     end up in need to duplicate keyed comdat because it has unkeyed alias.  */
  if (c == SYMBOL_PARTITION && !DECL_COMDAT (node->decl)
      && symbol_partitioned_p (node))
    return false;

  /* Be sure that we never try to duplicate partitioned symbol
     or add external symbol.  */
  gcc_assert (c != SYMBOL_EXTERNAL
	      && (c == SYMBOL_DUPLICATE || !symbol_partitioned_p (node)));

  part->symbols++;

  lto_set_symtab_encoder_in_partition (part->encoder, node);

  if (symbol_partitioned_p (node))
    {
      node->in_other_partition = 1;
      if (symtab->dump_file)
	fprintf (symtab->dump_file,
		 "Symbol node %s now used in multiple partitions\n",
		 node->name ());
    }
  node->aux = (void *)((size_t)node->aux + 1);

  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
    {
      struct cgraph_edge *e;
      if (!node->alias)
        part->insns += inline_summaries->get (cnode)->self_size;

      /* Add all inline clones and callees that are duplicated.  */
      for (e = cnode->callees; e; e = e->next_callee)
	if (!e->inline_failed)
	  add_symbol_to_partition_1 (part, e->callee);
	else if (e->callee->get_partitioning_class () == SYMBOL_DUPLICATE)
	  add_symbol_to_partition (part, e->callee);

      /* Add all thunks associated with the function.  */
      for (e = cnode->callers; e; e = e->next_caller)
	if (e->caller->thunk.thunk_p)
	  add_symbol_to_partition_1 (part, e->caller);

      /* Instrumented version is actually the same function.
	 Therefore put it into the same partition.  */
      if (cnode->instrumented_version)
	add_symbol_to_partition_1 (part, cnode->instrumented_version);
    }

  add_references_to_partition (part, node);

  /* Add all aliases associated with the symbol.  */

  FOR_EACH_ALIAS (node, ref)
    if (!ref->referring->transparent_alias)
      add_symbol_to_partition_1 (part, ref->referring);
    else
      {
	struct ipa_ref *ref2;
	/* We do not need to add transparent aliases if they are not used.
	   However we must add aliases of transparent aliases if they exist.  */
	FOR_EACH_ALIAS (ref->referring, ref2)
	  {
	    /* Nested transparent aliases are not permitted.  */
	    gcc_checking_assert (!ref2->referring->transparent_alias);
	    add_symbol_to_partition_1 (part, ref2->referring);
	  }
      }

  /* Ensure that SAME_COMDAT_GROUP lists all allways added in a group.  */
  if (node->same_comdat_group)
    for (node1 = node->same_comdat_group;
	 node1 != node; node1 = node1->same_comdat_group)
      if (!node->alias)
	{
	  bool added = add_symbol_to_partition_1 (part, node1);
	  gcc_assert (added);
	}
  return true;
}

/* If symbol NODE is really part of other symbol's definition (i.e. it is
   internal label, thunk, alias or so), return the outer symbol. 
   When add_symbol_to_partition_1 is called on the outer symbol it must
   eventually add NODE, too.  */
static symtab_node *
contained_in_symbol (symtab_node *node)
{
  /* There is no need to consider transparent aliases to be part of the
     definition: they are only useful insite the partition they are output
     and thus we will always see an explicit reference to it.  */
  if (node->transparent_alias)
    return node;
  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
    {
      cnode = cnode->function_symbol ();
      if (cnode->global.inlined_to)
	cnode = cnode->global.inlined_to;
      return cnode;
    }
  else if (varpool_node *vnode = dyn_cast <varpool_node *> (node))
    return vnode->ultimate_alias_target ();
  return node;
}

/* Add symbol NODE to partition.  When definition of NODE is part
   of other symbol definition, add the other symbol, too.  */

static void
add_symbol_to_partition (ltrans_partition part, symtab_node *node)
{
  symtab_node *node1;

  /* Verify that we do not try to duplicate something that can not be.  */
  gcc_checking_assert (node->get_partitioning_class () == SYMBOL_DUPLICATE
		       || !symbol_partitioned_p (node));

  while ((node1 = contained_in_symbol (node)) != node)
    node = node1;

  /* If we have duplicated symbol contained in something we can not duplicate,
     we are very badly screwed.  The other way is possible, so we do not
     assert this in add_symbol_to_partition_1. 

     Be lax about comdats; they may or may not be duplicated and we may
     end up in need to duplicate keyed comdat because it has unkeyed alias.  */

  gcc_assert (node->get_partitioning_class () == SYMBOL_DUPLICATE
	      || DECL_COMDAT (node->decl)
	      || !symbol_partitioned_p (node));

  add_symbol_to_partition_1 (part, node);
}

/* Undo all additions until number of cgraph nodes in PARITION is N_CGRAPH_NODES
   and number of varpool nodes is N_VARPOOL_NODES.  */

static void
undo_partition (ltrans_partition partition, unsigned int n_nodes)
{
  while (lto_symtab_encoder_size (partition->encoder) > (int)n_nodes)
    {
      symtab_node *node = lto_symtab_encoder_deref (partition->encoder,
						   n_nodes);
      partition->symbols--;
      cgraph_node *cnode;

      /* After UNDO we no longer know what was visited.  */
      if (partition->initializers_visited)
	delete partition->initializers_visited;
      partition->initializers_visited = NULL;

      if (!node->alias && (cnode = dyn_cast <cgraph_node *> (node)))
        partition->insns -= inline_summaries->get (cnode)->self_size;
      lto_symtab_encoder_delete_node (partition->encoder, node);
      node->aux = (void *)((size_t)node->aux - 1);
    }
}

/* Group cgrah nodes by input files.  This is used mainly for testing
   right now.  */

void
lto_1_to_1_map (void)
{
  symtab_node *node;
  struct lto_file_decl_data *file_data;
  hash_map<lto_file_decl_data *, ltrans_partition> pmap;
  ltrans_partition partition;
  int npartitions = 0;

  FOR_EACH_SYMBOL (node)
    {
      if (node->get_partitioning_class () != SYMBOL_PARTITION
	  || symbol_partitioned_p (node))
	continue;

      file_data = node->lto_file_data;

      if (file_data)
	{
          ltrans_partition *slot = &pmap.get_or_insert (file_data);
          if (*slot)
	    partition = *slot;
	  else
	    {
	      partition = new_partition (file_data->file_name);
	      *slot = partition;
	      npartitions++;
	    }
	}
      else if (!file_data && ltrans_partitions.length ())
	partition = ltrans_partitions[0];
      else
	{
	  partition = new_partition ("");
	  pmap.put (NULL, partition);
	  npartitions++;
	}

      add_symbol_to_partition (partition, node);
    }

  /* If the cgraph is empty, create one cgraph node set so that there is still
     an output file for any variables that need to be exported in a DSO.  */
  if (!npartitions)
    new_partition ("empty");

}

/* Maximal partitioning.  Put every new symbol into new partition if possible.  */

void
lto_max_map (void)
{
  symtab_node *node;
  ltrans_partition partition;
  int npartitions = 0;

  FOR_EACH_SYMBOL (node)
    {
      if (node->get_partitioning_class () != SYMBOL_PARTITION
	  || symbol_partitioned_p (node))
	continue;
      partition = new_partition (node->asm_name ());
      add_symbol_to_partition (partition, node);
      npartitions++;
    }
  if (!npartitions)
    new_partition ("empty");
}

/* Helper function for qsort; sort nodes by order. noreorder functions must have
   been removed earlier.  */
static int
node_cmp (const void *pa, const void *pb)
{
  const struct cgraph_node *a = *(const struct cgraph_node * const *) pa;
  const struct cgraph_node *b = *(const struct cgraph_node * const *) pb;

  /* Profile reorder flag enables function reordering based on first execution
     of a function. All functions with profile are placed in ascending
     order at the beginning.  */

  if (flag_profile_reorder_functions)
  {
    /* Functions with time profile are sorted in ascending order.  */
    if (a->tp_first_run && b->tp_first_run)
      return a->tp_first_run != b->tp_first_run
	? a->tp_first_run - b->tp_first_run
        : a->order - b->order;

    /* Functions with time profile are sorted before the functions
       that do not have the profile.  */
    if (a->tp_first_run || b->tp_first_run)
      return b->tp_first_run - a->tp_first_run;
  }

  return b->order - a->order;
}

/* Helper function for qsort; sort nodes by order.  */
static int
varpool_node_cmp (const void *pa, const void *pb)
{
  const symtab_node *a = *static_cast<const symtab_node * const *> (pa);
  const symtab_node *b = *static_cast<const symtab_node * const *> (pb);
  return b->order - a->order;
}

/* Add all symtab nodes from NEXT_NODE to PARTITION in order.  */

static void
add_sorted_nodes (vec<symtab_node *> &next_nodes, ltrans_partition partition)
{
  unsigned i;
  symtab_node *node;

  next_nodes.qsort (varpool_node_cmp);
  FOR_EACH_VEC_ELT (next_nodes, i, node)
    if (!symbol_partitioned_p (node))
      add_symbol_to_partition (partition, node);
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
lto_balanced_map (int n_lto_partitions, int max_partition_size)
{
  int n_nodes = 0;
  int n_varpool_nodes = 0, varpool_pos = 0, best_varpool_pos = 0;
  struct cgraph_node **order = XNEWVEC (cgraph_node *, symtab->cgraph_max_uid);
  auto_vec<cgraph_node *> noreorder;
  auto_vec<varpool_node *> varpool_order;
  int i;
  struct cgraph_node *node;
  int original_total_size, total_size = 0, best_total_size = 0;
  int partition_size;
  ltrans_partition partition;
  int last_visited_node = 0;
  varpool_node *vnode;
  int cost = 0, internal = 0;
  int best_n_nodes = 0, best_i = 0, best_cost =
    INT_MAX, best_internal = 0;
  int npartitions;
  int current_order = -1;
  int noreorder_pos = 0;

  FOR_EACH_VARIABLE (vnode)
    gcc_assert (!vnode->aux);
    
  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->get_partitioning_class () == SYMBOL_PARTITION)
      {
	if (node->no_reorder)
	  noreorder.safe_push (node);
	else
	  order[n_nodes++] = node;
	if (!node->alias)
	  total_size += inline_summaries->get (node)->size;
      }

  original_total_size = total_size;

  /* Streaming works best when the source units do not cross partition
     boundaries much.  This is because importing function from a source
     unit tends to import a lot of global trees defined there.  We should
     get better about minimizing the function bounday, but until that
     things works smoother if we order in source order.  */
  qsort (order, n_nodes, sizeof (struct cgraph_node *), node_cmp);
  noreorder.qsort (node_cmp);

  if (symtab->dump_file)
    {
      for(i = 0; i < n_nodes; i++)
	fprintf (symtab->dump_file, "Balanced map symbol order:%s:%u\n",
		 order[i]->name (), order[i]->tp_first_run);
      for(i = 0; i < (int)noreorder.length(); i++)
	fprintf (symtab->dump_file, "Balanced map symbol no_reorder:%s:%u\n",
		 noreorder[i]->name (), noreorder[i]->tp_first_run);
    }

  /* Collect all variables that should not be reordered.  */
  FOR_EACH_VARIABLE (vnode)
    if (vnode->get_partitioning_class () == SYMBOL_PARTITION
	&& (!flag_toplevel_reorder || vnode->no_reorder))
      varpool_order.safe_push (vnode);
  n_varpool_nodes = varpool_order.length ();
  varpool_order.qsort (varpool_node_cmp);

  /* Compute partition size and create the first partition.  */
  if (PARAM_VALUE (MIN_PARTITION_SIZE) > max_partition_size)
    fatal_error (input_location, "min partition size cannot be greater than max partition size");

  partition_size = total_size / n_lto_partitions;
  if (partition_size < PARAM_VALUE (MIN_PARTITION_SIZE))
    partition_size = PARAM_VALUE (MIN_PARTITION_SIZE);
  npartitions = 1;
  partition = new_partition ("");
  if (symtab->dump_file)
    fprintf (symtab->dump_file, "Total unit size: %i, partition size: %i\n",
	     total_size, partition_size);

  auto_vec<symtab_node *> next_nodes;

  for (i = 0; i < n_nodes; i++)
    {
      if (symbol_partitioned_p (order[i]))
	continue;

      current_order = order[i]->order;

      /* Output noreorder and varpool in program order first.  */
      next_nodes.truncate (0);
      while (varpool_pos < n_varpool_nodes
	     && varpool_order[varpool_pos]->order < current_order)
	next_nodes.safe_push (varpool_order[varpool_pos++]);
      while (noreorder_pos < (int)noreorder.length ()
	     && noreorder[noreorder_pos]->order < current_order)
	{
	  if (!noreorder[noreorder_pos]->alias)
	    total_size -= inline_summaries->get (noreorder[noreorder_pos])->size;
	  next_nodes.safe_push (noreorder[noreorder_pos++]);
	}
      add_sorted_nodes (next_nodes, partition);

      add_symbol_to_partition (partition, order[i]);
      if (!order[i]->alias)
        total_size -= inline_summaries->get (order[i])->size;
	  

      /* Once we added a new node to the partition, we also want to add
         all referenced variables unless they was already added into some
         earlier partition.
	 add_symbol_to_partition adds possibly multiple nodes and
	 variables that are needed to satisfy needs of ORDER[i].
         We remember last visited cgraph and varpool node from last iteration
         of outer loop that allows us to process every new addition. 

	 At the same time we compute size of the boundary into COST.  Every
         callgraph or IPA reference edge leaving the partition contributes into
         COST.  Every edge inside partition was earlier computed as one leaving
	 it and thus we need to subtract it from COST.  */
      while (last_visited_node < lto_symtab_encoder_size (partition->encoder))
	{
	  symtab_node *refs_node;
	  int j;
	  struct ipa_ref *ref = NULL;
	  symtab_node *snode = lto_symtab_encoder_deref (partition->encoder,
							last_visited_node);

	  if (cgraph_node *node = dyn_cast <cgraph_node *> (snode))
	    {
	      struct cgraph_edge *edge;

	      refs_node = node;

	      last_visited_node++;

	      gcc_assert (node->definition || node->weakref);

	      /* Compute boundary cost of callgraph edges.  */
	      for (edge = node->callees; edge; edge = edge->next_callee)
		if (edge->callee->definition)
		  {
		    int edge_cost = edge->frequency;
		    int index;

		    if (!edge_cost)
		      edge_cost = 1;
		    gcc_assert (edge_cost > 0);
		    index = lto_symtab_encoder_lookup (partition->encoder,
						       edge->callee);
		    if (index != LCC_NOT_FOUND
		        && index < last_visited_node - 1)
		      cost -= edge_cost, internal += edge_cost;
		    else
		      cost += edge_cost;
		  }
	      for (edge = node->callers; edge; edge = edge->next_caller)
		{
		  int edge_cost = edge->frequency;
		  int index;

		  gcc_assert (edge->caller->definition);
		  if (!edge_cost)
		    edge_cost = 1;
		  gcc_assert (edge_cost > 0);
		  index = lto_symtab_encoder_lookup (partition->encoder,
						     edge->caller);
		  if (index != LCC_NOT_FOUND
		      && index < last_visited_node - 1)
		    cost -= edge_cost;
		  else
		    cost += edge_cost;
		}
	    }
	  else
	    {
	      refs_node = snode;
	      last_visited_node++;
	    }

	  /* Compute boundary cost of IPA REF edges and at the same time look into
	     variables referenced from current partition and try to add them.  */
	  for (j = 0; refs_node->iterate_reference (j, ref); j++)
	    if (is_a <varpool_node *> (ref->referred))
	      {
		int index;

		vnode = dyn_cast <varpool_node *> (ref->referred);
		if (!vnode->definition)
		  continue;
		if (!symbol_partitioned_p (vnode) && flag_toplevel_reorder
		    && !vnode->no_reorder
		    && vnode->get_partitioning_class () == SYMBOL_PARTITION)
		  add_symbol_to_partition (partition, vnode);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   vnode);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--, internal++;
		else
		  cost++;
	      }
	    else
	      {
		int index;

		node = dyn_cast <cgraph_node *> (ref->referred);
		if (!node->definition)
		  continue;
		index = lto_symtab_encoder_lookup (partition->encoder,
						   node);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--, internal++;
		else
		  cost++;
	      }
	  for (j = 0; refs_node->iterate_referring (j, ref); j++)
	    if (is_a <varpool_node *> (ref->referring))
	      {
		int index;

		vnode = dyn_cast <varpool_node *> (ref->referring);
		gcc_assert (vnode->definition);
		/* It is better to couple variables with their users, because it allows them
		   to be removed.  Coupling with objects they refer to only helps to reduce
		   number of symbols promoted to hidden.  */
		if (!symbol_partitioned_p (vnode) && flag_toplevel_reorder
		    && !vnode->no_reorder
		    && !vnode->can_remove_if_no_refs_p ()
		    && vnode->get_partitioning_class () == SYMBOL_PARTITION)
		  add_symbol_to_partition (partition, vnode);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   vnode);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
		  cost--;
		else
		  cost++;
	      }
	    else
	      {
		int index;

		node = dyn_cast <cgraph_node *> (ref->referring);
		gcc_assert (node->definition);
		index = lto_symtab_encoder_lookup (partition->encoder,
						   node);
		if (index != LCC_NOT_FOUND
		    && index < last_visited_node - 1)
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
	  best_n_nodes = lto_symtab_encoder_size (partition->encoder);
	  best_total_size = total_size;
	  best_varpool_pos = varpool_pos;
	}
      if (symtab->dump_file)
	fprintf (symtab->dump_file, "Step %i: added %s/%i, size %i, cost %i/%i "
		 "best %i/%i, step %i\n", i,
		 order[i]->name (), order[i]->order,
		 partition->insns, cost, internal,
		 best_cost, best_internal, best_i);
      /* Partition is too large, unwind into step when best cost was reached and
	 start new partition.  */
      if (partition->insns > 2 * partition_size
	  || partition->insns > max_partition_size)
	{
	  if (best_i != i)
	    {
	      if (symtab->dump_file)
		fprintf (symtab->dump_file, "Unwinding %i insertions to step %i\n",
			 i - best_i, best_i);
	      undo_partition (partition, best_n_nodes);
	      varpool_pos = best_varpool_pos;
	    }
	  i = best_i;
 	  /* When we are finished, avoid creating empty partition.  */
	  while (i < n_nodes - 1 && symbol_partitioned_p (order[i + 1]))
	    i++;
	  if (i == n_nodes - 1)
	    break;
	  partition = new_partition ("");
	  last_visited_node = 0;
	  total_size = best_total_size;
	  cost = 0;

	  if (symtab->dump_file)
	    fprintf (symtab->dump_file, "New partition\n");
	  best_n_nodes = 0;
	  best_cost = INT_MAX;

	  /* Since the size of partitions is just approximate, update the size after
	     we finished current one.  */
	  if (npartitions < n_lto_partitions)
	    partition_size = total_size / (n_lto_partitions - npartitions);
	  else
	    partition_size = INT_MAX;

	  if (partition_size < PARAM_VALUE (MIN_PARTITION_SIZE))
	    partition_size = PARAM_VALUE (MIN_PARTITION_SIZE);
	  npartitions ++;
	}
    }

  next_nodes.truncate (0);

  /* Varables that are not reachable from the code go into last partition.  */
  if (flag_toplevel_reorder)
    {
      FOR_EACH_VARIABLE (vnode)
	if (vnode->get_partitioning_class () == SYMBOL_PARTITION
	    && !symbol_partitioned_p (vnode)
	    && !vnode->no_reorder)
	  next_nodes.safe_push (vnode);
    }

  /* Output remaining ordered symbols.  */
  while (varpool_pos < n_varpool_nodes)
    next_nodes.safe_push (varpool_order[varpool_pos++]);
  while (noreorder_pos < (int)noreorder.length ())
    next_nodes.safe_push (noreorder[noreorder_pos++]);
  add_sorted_nodes (next_nodes, partition);

  free (order);

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "\nPartition sizes:\n");
      unsigned partitions = ltrans_partitions.length ();

      for (unsigned i = 0; i < partitions ; i++)
	{
	  ltrans_partition p = ltrans_partitions[i];
	  fprintf (symtab->dump_file, "partition %d contains %d (%2.2f%%)"
		   " symbols and %d (%2.2f%%) insns\n", i, p->symbols,
		   100.0 * p->symbols / n_nodes, p->insns,
		   100.0 * p->insns / original_total_size);
	}

      fprintf (symtab->dump_file, "\n");
    }
}

/* Return true if we must not change the name of the NODE.  The name as
   extracted from the corresponding decl should be passed in NAME.  */

static bool
must_not_rename (symtab_node *node, const char *name)
{
  /* Our renaming machinery do not handle more than one change of assembler name.
     We should not need more than one anyway.  */
  if (node->lto_file_data
      && lto_get_decl_name_mapping (node->lto_file_data, name) != name)
    {
      if (symtab->dump_file)
	fprintf (symtab->dump_file,
		 "Not privatizing symbol name: %s. It privatized already.\n",
		 name);
      return true;
    }
  /* Avoid mangling of already mangled clones. 
     ???  should have a flag whether a symbol has a 'private' name already,
     since we produce some symbols like that i.e. for global constructors
     that are not really clones.  */
  if (node->unique_name)
    {
      if (symtab->dump_file)
	fprintf (symtab->dump_file,
		 "Not privatizing symbol name: %s. Has unique name.\n",
		 name);
      return true;
    }
  return false;
}

/* If we are an offload compiler, we may have to rewrite symbols to be
   valid on this target.  Return either PTR or a modified version of it.  */

static const char *
maybe_rewrite_identifier (const char *ptr)
{
#if defined ACCEL_COMPILER && (defined NO_DOT_IN_LABEL || defined NO_DOLLAR_IN_LABEL)
#ifndef NO_DOT_IN_LABEL
  char valid = '.';
  const char reject[] = "$";
#elif !defined NO_DOLLAR_IN_LABEL
  char valid = '$';
  const char reject[] = ".";
#else
  char valid = '_';
  const char reject[] = ".$";
#endif

  char *copy = NULL;
  const char *match = ptr;
  for (;;)
    {
      size_t off = strcspn (match, reject);
      if (match[off] == '\0')
	break;
      if (copy == NULL)
	{
	  copy = xstrdup (ptr);
	  match = copy;
	}
      copy[off] = valid;
    }
  return match;
#else
  return ptr;
#endif
}

/* Ensure that the symbol in NODE is valid for the target, and if not,
   rewrite it.  */

static void
validize_symbol_for_target (symtab_node *node)
{
  tree decl = node->decl;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (must_not_rename (node, name))
    return;

  const char *name2 = maybe_rewrite_identifier (name);
  if (name2 != name)
    {
      symtab->change_decl_assembler_name (decl, get_identifier (name2));
      if (node->lto_file_data)
	lto_record_renamed_decl (node->lto_file_data, name,
				 IDENTIFIER_POINTER
				 (DECL_ASSEMBLER_NAME (decl)));
    }
}

/* Helper for privatize_symbol_name.  Mangle NODE symbol name
   represented by DECL.  */

static bool
privatize_symbol_name_1 (symtab_node *node, tree decl)
{
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  if (must_not_rename (node, name))
    return false;

  name = maybe_rewrite_identifier (name);
  symtab->change_decl_assembler_name (decl,
				      clone_function_name_1 (name,
							     "lto_priv"));

  if (node->lto_file_data)
    lto_record_renamed_decl (node->lto_file_data, name,
			     IDENTIFIER_POINTER
			     (DECL_ASSEMBLER_NAME (decl)));

  if (symtab->dump_file)
    fprintf (symtab->dump_file,
	     "Privatizing symbol name: %s -> %s\n",
	     name, IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));

  return true;
}

/* Mangle NODE symbol name into a local name.
   This is necessary to do
   1) if two or more static vars of same assembler name
      are merged into single ltrans unit.
   2) if previously static var was promoted hidden to avoid possible conflict
      with symbols defined out of the LTO world.  */

static bool
privatize_symbol_name (symtab_node *node)
{
  if (!privatize_symbol_name_1 (node, node->decl))
    return false;

  /* We could change name which is a target of transparent alias
     chain of instrumented function name.  Fix alias chain if so  .*/
  if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
    {
      tree iname = NULL_TREE;
      if (cnode->instrumentation_clone)
	{
	  /* If we want to privatize instrumentation clone
	     then we also need to privatize original function.  */
	  if (cnode->instrumented_version)
	    privatize_symbol_name (cnode->instrumented_version);
	  else
	    privatize_symbol_name_1 (cnode, cnode->orig_decl);
	  iname = DECL_ASSEMBLER_NAME (cnode->decl);
	  TREE_CHAIN (iname) = DECL_ASSEMBLER_NAME (cnode->orig_decl);
	}
      else if (cnode->instrumented_version
	       && cnode->instrumented_version->orig_decl == cnode->decl)
	{
	  iname = DECL_ASSEMBLER_NAME (cnode->instrumented_version->decl);
	  TREE_CHAIN (iname) = DECL_ASSEMBLER_NAME (cnode->decl);
	}
    }

  return true;
}

/* Promote variable VNODE to be static.  */

static void
promote_symbol (symtab_node *node)
{
  /* We already promoted ... */
  if (DECL_VISIBILITY (node->decl) == VISIBILITY_HIDDEN
      && DECL_VISIBILITY_SPECIFIED (node->decl)
      && TREE_PUBLIC (node->decl))
    {
      validize_symbol_for_target (node);
      return;
    }

  gcc_checking_assert (!TREE_PUBLIC (node->decl)
		       && !DECL_EXTERNAL (node->decl));
  /* Be sure that newly public symbol does not conflict with anything already
     defined by the non-LTO part.  */
  privatize_symbol_name (node);
  TREE_PUBLIC (node->decl) = 1;
  DECL_VISIBILITY (node->decl) = VISIBILITY_HIDDEN;
  DECL_VISIBILITY_SPECIFIED (node->decl) = true;
  ipa_ref *ref;

  /* Promoting a symbol also promotes all trasparent aliases with exception
     of weakref where the visibility flags are always wrong and set to 
     !PUBLIC.  */
  for (unsigned i = 0; node->iterate_direct_aliases (i, ref); i++)
    {
      struct symtab_node *alias = ref->referring;
      if (alias->transparent_alias && !alias->weakref)
	{
	  TREE_PUBLIC (alias->decl) = 1;
	  DECL_VISIBILITY (alias->decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (alias->decl) = true;
	}
      gcc_assert (!alias->weakref || TREE_PUBLIC (alias->decl));
    }

  if (symtab->dump_file)
    fprintf (symtab->dump_file,
	    "Promoting as hidden: %s\n", node->name ());
}

/* Return true if NODE needs named section even if it won't land in the partition
   symbol table.
   FIXME: we should really not use named sections for inline clones and master
   clones.  */

static bool
may_need_named_section_p (lto_symtab_encoder_t encoder, symtab_node *node)
{
  struct cgraph_node *cnode = dyn_cast <cgraph_node *> (node);
  if (!cnode)
    return false;
  if (node->real_symbol_p ())
    return false;
  return (!encoder
	  || (lto_symtab_encoder_lookup (encoder, node) != LCC_NOT_FOUND
              && lto_symtab_encoder_encode_body_p (encoder,
				                   cnode)));
}

/* If NODE represents a static variable.  See if there are other variables
   of the same name in partition ENCODER (or in whole compilation unit if
   ENCODER is NULL) and if so, mangle the statics.  Always mangle all
   conflicting statics, so we reduce changes of silently miscompiling
   asm statements referring to them by symbol name.  */

static void
rename_statics (lto_symtab_encoder_t encoder, symtab_node *node)
{
  tree decl = node->decl;
  symtab_node *s;
  tree name = DECL_ASSEMBLER_NAME (decl);

  /* See if this is static symbol. */
  if (((node->externally_visible && !node->weakref)
      /* FIXME: externally_visible is somewhat illogically not set for
	 external symbols (i.e. those not defined).  Remove this test
	 once this is fixed.  */
        || DECL_EXTERNAL (node->decl)
	|| !node->real_symbol_p ())
       && !may_need_named_section_p (encoder, node))
    return;

  /* Now walk symbols sharing the same name and see if there are any conflicts.
     (all types of symbols counts here, since we can not have static of the
     same name as external or public symbol.)  */
  for (s = symtab_node::get_for_asmname (name);
       s; s = s->next_sharing_asm_name)
    if ((s->real_symbol_p () || may_need_named_section_p (encoder, s))
	&& s->decl != node->decl
	&& (!encoder
	    || lto_symtab_encoder_lookup (encoder, s) != LCC_NOT_FOUND))
       break;

  /* OK, no confict, so we have nothing to do.  */
  if (!s)
    return;

  if (symtab->dump_file)
    fprintf (symtab->dump_file,
	    "Renaming statics with asm name: %s\n", node->name ());

  /* Assign every symbol in the set that shares the same ASM name an unique
     mangled name.  */
  for (s = symtab_node::get_for_asmname (name); s;)
    if ((!s->externally_visible || s->weakref)
	/* Transparent aliases having same name as target are renamed at a
	   time their target gets new name.  Transparent aliases that use
	   separate assembler name require the name to be unique.  */
	&& (!s->transparent_alias || !s->definition || s->weakref
	    || !symbol_table::assembler_names_equal_p
		 (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (s->decl)),
		  IDENTIFIER_POINTER
		    (DECL_ASSEMBLER_NAME (s->get_alias_target()->decl))))
	&& ((s->real_symbol_p ()
             && !DECL_EXTERNAL (s->decl)
	     && !TREE_PUBLIC (s->decl))
 	    || may_need_named_section_p (encoder, s))
	&& (!encoder
	    || lto_symtab_encoder_lookup (encoder, s) != LCC_NOT_FOUND))
      {
        if (privatize_symbol_name (s))
	  /* Re-start from beginning since we do not know how many symbols changed a name.  */
	  s = symtab_node::get_for_asmname (name);
        else s = s->next_sharing_asm_name;
      }
    else s = s->next_sharing_asm_name;
}

/* Find out all static decls that need to be promoted to global because
   of cross file sharing.  This function must be run in the WPA mode after
   all inlinees are added.  */

void
lto_promote_cross_file_statics (void)
{
  unsigned i, n_sets;

  gcc_assert (flag_wpa);

  lto_stream_offload_p = false;
  select_what_to_stream ();

  /* First compute boundaries.  */
  n_sets = ltrans_partitions.length ();
  for (i = 0; i < n_sets; i++)
    {
      ltrans_partition part
	= ltrans_partitions[i];
      part->encoder = compute_ltrans_boundary (part->encoder);
    }

  /* Look at boundaries and promote symbols as needed.  */
  for (i = 0; i < n_sets; i++)
    {
      lto_symtab_encoder_iterator lsei;
      lto_symtab_encoder_t encoder = ltrans_partitions[i]->encoder;

      for (lsei = lsei_start (encoder); !lsei_end_p (lsei);
	   lsei_next (&lsei))
        {
          symtab_node *node = lsei_node (lsei);

	  /* If symbol is static, rename it if its assembler name clash with
	     anything else in this unit.  */
	  rename_statics (encoder, node);

	  /* No need to promote if symbol already is externally visible ... */
	  if (node->externally_visible
 	      /* ... or if it is part of current partition ... */
	      || lto_symtab_encoder_in_partition_p (encoder, node)
	      /* ... or if we do not partition it. This mean that it will
		 appear in every partition refernecing it.  */
	      || node->get_partitioning_class () != SYMBOL_PARTITION)
	    {
	      validize_symbol_for_target (node);
	      continue;
	    }

          promote_symbol (node);
        }
    }
}

/* Rename statics in the whole unit in the case that 
   we do -flto-partition=none.  */

void
lto_promote_statics_nonwpa (void)
{
  symtab_node *node;
  FOR_EACH_SYMBOL (node)
    {
      rename_statics (NULL, node);
      validize_symbol_for_target (node);
    }
}
