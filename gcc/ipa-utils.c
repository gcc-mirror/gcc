/* Utilities for ipa analysis.
   Copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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
#include "tree.h"
#include "gimple.h"
#include "tree-inline.h"
#include "dumpfile.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "splay-tree.h"
#include "ggc.h"
#include "ipa-utils.h"
#include "ipa-reference.h"
#include "flags.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "lto-streamer.h"
#include "ipa-inline.h"

/* Debugging function for postorder and inorder code. NOTE is a string
   that is printed before the nodes are printed.  ORDER is an array of
   cgraph_nodes that has COUNT useful nodes in it.  */

void
ipa_print_order (FILE* out,
		 const char * note,
		 struct cgraph_node** order,
		 int count)
{
  int i;
  fprintf (out, "\n\n ordered call graph: %s\n", note);

  for (i = count - 1; i >= 0; i--)
    dump_cgraph_node (dump_file, order[i]);
  fprintf (out, "\n");
  fflush (out);
}


struct searchc_env {
  struct cgraph_node **stack;
  int stack_size;
  struct cgraph_node **result;
  int order_pos;
  splay_tree nodes_marked_new;
  bool reduce;
  bool allow_overwritable;
  int count;
};

/* This is an implementation of Tarjan's strongly connected region
   finder as reprinted in Aho Hopcraft and Ullman's The Design and
   Analysis of Computer Programs (1975) pages 192-193.  This version
   has been customized for cgraph_nodes.  The env parameter is because
   it is recursive and there are no nested functions here.  This
   function should only be called from itself or
   ipa_reduced_postorder.  ENV is a stack env and would be
   unnecessary if C had nested functions.  V is the node to start
   searching from.  */

static void
searchc (struct searchc_env* env, struct cgraph_node *v,
	 bool (*ignore_edge) (struct cgraph_edge *))
{
  struct cgraph_edge *edge;
  struct ipa_dfs_info *v_info = (struct ipa_dfs_info *) v->aux;

  /* mark node as old */
  v_info->new_node = false;
  splay_tree_remove (env->nodes_marked_new, v->uid);

  v_info->dfn_number = env->count;
  v_info->low_link = env->count;
  env->count++;
  env->stack[(env->stack_size)++] = v;
  v_info->on_stack = true;

  for (edge = v->callees; edge; edge = edge->next_callee)
    {
      struct ipa_dfs_info * w_info;
      enum availability avail;
      struct cgraph_node *w = cgraph_function_or_thunk_node (edge->callee, &avail);

      if (!w || (ignore_edge && ignore_edge (edge)))
        continue;

      if (w->aux
	  && (avail > AVAIL_OVERWRITABLE
	      || (env->allow_overwritable && avail == AVAIL_OVERWRITABLE)))
	{
	  w_info = (struct ipa_dfs_info *) w->aux;
	  if (w_info->new_node)
	    {
	      searchc (env, w, ignore_edge);
	      v_info->low_link =
		(v_info->low_link < w_info->low_link) ?
		v_info->low_link : w_info->low_link;
	    }
	  else
	    if ((w_info->dfn_number < v_info->dfn_number)
		&& (w_info->on_stack))
	      v_info->low_link =
		(w_info->dfn_number < v_info->low_link) ?
		w_info->dfn_number : v_info->low_link;
	}
    }


  if (v_info->low_link == v_info->dfn_number)
    {
      struct cgraph_node *last = NULL;
      struct cgraph_node *x;
      struct ipa_dfs_info *x_info;
      do {
	x = env->stack[--(env->stack_size)];
	x_info = (struct ipa_dfs_info *) x->aux;
	x_info->on_stack = false;
	x_info->scc_no = v_info->dfn_number;

	if (env->reduce)
	  {
	    x_info->next_cycle = last;
	    last = x;
	  }
	else
	  env->result[env->order_pos++] = x;
      }
      while (v != x);
      if (env->reduce)
	env->result[env->order_pos++] = v;
    }
}

/* Topsort the call graph by caller relation.  Put the result in ORDER.

   The REDUCE flag is true if you want the cycles reduced to single nodes.
   You can use ipa_get_nodes_in_cycle to obtain a vector containing all real
   call graph nodes in a reduced node.

   Set ALLOW_OVERWRITABLE if nodes with such availability should be included.
   IGNORE_EDGE, if non-NULL is a hook that may make some edges insignificant
   for the topological sort.   */

int
ipa_reduced_postorder (struct cgraph_node **order,
		       bool reduce, bool allow_overwritable,
		       bool (*ignore_edge) (struct cgraph_edge *))
{
  struct cgraph_node *node;
  struct searchc_env env;
  splay_tree_node result;
  env.stack = XCNEWVEC (struct cgraph_node *, cgraph_n_nodes);
  env.stack_size = 0;
  env.result = order;
  env.order_pos = 0;
  env.nodes_marked_new = splay_tree_new (splay_tree_compare_ints, 0, 0);
  env.count = 1;
  env.reduce = reduce;
  env.allow_overwritable = allow_overwritable;

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      enum availability avail = cgraph_function_body_availability (node);

      if (avail > AVAIL_OVERWRITABLE
	  || (allow_overwritable
	      && (avail == AVAIL_OVERWRITABLE)))
	{
	  /* Reuse the info if it is already there.  */
	  struct ipa_dfs_info *info = (struct ipa_dfs_info *) node->aux;
	  if (!info)
	    info = XCNEW (struct ipa_dfs_info);
	  info->new_node = true;
	  info->on_stack = false;
	  info->next_cycle = NULL;
	  node->aux = info;

	  splay_tree_insert (env.nodes_marked_new,
			     (splay_tree_key)node->uid,
			     (splay_tree_value)node);
	}
      else
	node->aux = NULL;
    }
  result = splay_tree_min (env.nodes_marked_new);
  while (result)
    {
      node = (struct cgraph_node *)result->value;
      searchc (&env, node, ignore_edge);
      result = splay_tree_min (env.nodes_marked_new);
    }
  splay_tree_delete (env.nodes_marked_new);
  free (env.stack);

  return env.order_pos;
}

/* Deallocate all ipa_dfs_info structures pointed to by the aux pointer of call
   graph nodes.  */

void
ipa_free_postorder_info (void)
{
  struct cgraph_node *node;
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      /* Get rid of the aux information.  */
      if (node->aux)
	{
	  free (node->aux);
	  node->aux = NULL;
	}
    }
}

/* Get the set of nodes for the cycle in the reduced call graph starting
   from NODE.  */

vec<cgraph_node_ptr> 
ipa_get_nodes_in_cycle (struct cgraph_node *node)
{
  vec<cgraph_node_ptr> v = vNULL;
  struct ipa_dfs_info *node_dfs_info;
  while (node)
    {
      v.safe_push (node);
      node_dfs_info = (struct ipa_dfs_info *) node->aux;
      node = node_dfs_info->next_cycle;
    }
  return v;
}

/* Return true iff the CS is an edge within a strongly connected component as
   computed by ipa_reduced_postorder.  */

bool
ipa_edge_within_scc (struct cgraph_edge *cs)
{
  struct ipa_dfs_info *caller_dfs = (struct ipa_dfs_info *) cs->caller->aux;
  struct ipa_dfs_info *callee_dfs;
  struct cgraph_node *callee = cgraph_function_node (cs->callee, NULL);

  callee_dfs = (struct ipa_dfs_info *) callee->aux;
  return (caller_dfs
	  && callee_dfs
	  && caller_dfs->scc_no == callee_dfs->scc_no);
}

struct postorder_stack
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;
  int ref;
};

/* Fill array order with all nodes with output flag set in the reverse
   topological order.  Return the number of elements in the array.
   FIXME: While walking, consider aliases, too.  */

int
ipa_reverse_postorder (struct cgraph_node **order)
{
  struct cgraph_node *node, *node2;
  int stack_size = 0;
  int order_pos = 0;
  struct cgraph_edge *edge;
  int pass;
  struct ipa_ref *ref;

  struct postorder_stack *stack =
    XCNEWVEC (struct postorder_stack, cgraph_n_nodes);

  /* We have to deal with cycles nicely, so use a depth first traversal
     output algorithm.  Ignore the fact that some functions won't need
     to be output and put them into order as well, so we get dependencies
     right through inline functions.  */
  FOR_EACH_FUNCTION (node)
    node->aux = NULL;
  for (pass = 0; pass < 2; pass++)
    FOR_EACH_FUNCTION (node)
      if (!node->aux
	  && (pass
	      || (!node->address_taken
		  && !node->global.inlined_to
		  && !node->alias && !node->thunk.thunk_p
		  && !cgraph_only_called_directly_p (node))))
	{
	  stack_size = 0;
          stack[stack_size].node = node;
	  stack[stack_size].edge = node->callers;
	  stack[stack_size].ref = 0;
	  node->aux = (void *)(size_t)1;
	  while (stack_size >= 0)
	    {
	      while (true)
		{
		  node2 = NULL;
		  while (stack[stack_size].edge && !node2)
		    {
		      edge = stack[stack_size].edge;
		      node2 = edge->caller;
		      stack[stack_size].edge = edge->next_caller;
		      /* Break possible cycles involving always-inline
			 functions by ignoring edges from always-inline
			 functions to non-always-inline functions.  */
		      if (DECL_DISREGARD_INLINE_LIMITS (edge->caller->decl)
			  && !DECL_DISREGARD_INLINE_LIMITS
			    (cgraph_function_node (edge->callee, NULL)->decl))
			node2 = NULL;
		    }
		  for (;ipa_ref_list_referring_iterate (&stack[stack_size].node->ref_list,
						       stack[stack_size].ref,
						       ref) && !node2;
		       stack[stack_size].ref++)
		    {
		      if (ref->use == IPA_REF_ALIAS)
			node2 = ipa_ref_referring_node (ref);
		    }
		  if (!node2)
		    break;
		  if (!node2->aux)
		    {
		      stack[++stack_size].node = node2;
		      stack[stack_size].edge = node2->callers;
		      stack[stack_size].ref = 0;
		      node2->aux = (void *)(size_t)1;
		    }
		}
	      order[order_pos++] = stack[stack_size--].node;
	    }
	}
  free (stack);
  FOR_EACH_FUNCTION (node)
    node->aux = NULL;
  return order_pos;
}



/* Given a memory reference T, will return the variable at the bottom
   of the access.  Unlike get_base_address, this will recurse through
   INDIRECT_REFS.  */

tree
get_base_var (tree t)
{
  while (!SSA_VAR_P (t)
	 && (!CONSTANT_CLASS_P (t))
	 && TREE_CODE (t) != LABEL_DECL
	 && TREE_CODE (t) != FUNCTION_DECL
	 && TREE_CODE (t) != CONST_DECL
	 && TREE_CODE (t) != CONSTRUCTOR)
    {
      t = TREE_OPERAND (t, 0);
    }
  return t;
}


/* Create a new cgraph node set.  */

cgraph_node_set
cgraph_node_set_new (void)
{
  cgraph_node_set new_node_set;

  new_node_set = XCNEW (struct cgraph_node_set_def);
  new_node_set->map = pointer_map_create ();
  new_node_set->nodes.create (0);
  return new_node_set;
}


/* Add cgraph_node NODE to cgraph_node_set SET.  */

void
cgraph_node_set_add (cgraph_node_set set, struct cgraph_node *node)
{
  void **slot;

  slot = pointer_map_insert (set->map, node);

  if (*slot)
    {
      int index = (size_t) *slot - 1;
      gcc_checking_assert ((set->nodes[index]
		           == node));
      return;
    }

  *slot = (void *)(size_t) (set->nodes.length () + 1);

  /* Insert into node vector.  */
  set->nodes.safe_push (node);
}


/* Remove cgraph_node NODE from cgraph_node_set SET.  */

void
cgraph_node_set_remove (cgraph_node_set set, struct cgraph_node *node)
{
  void **slot, **last_slot;
  int index;
  struct cgraph_node *last_node;

  slot = pointer_map_contains (set->map, node);
  if (slot == NULL || !*slot)
    return;

  index = (size_t) *slot - 1;
  gcc_checking_assert (set->nodes[index]
	      	       == node);

  /* Remove from vector. We do this by swapping node with the last element
     of the vector.  */
  last_node = set->nodes.pop ();
  if (last_node != node)
    {
      last_slot = pointer_map_contains (set->map, last_node);
      gcc_checking_assert (last_slot && *last_slot);
      *last_slot = (void *)(size_t) (index + 1);

      /* Move the last element to the original spot of NODE.  */
      set->nodes[index] = last_node;
    }

  /* Remove element from hash table.  */
  *slot = NULL;
}


/* Find NODE in SET and return an iterator to it if found.  A null iterator
   is returned if NODE is not in SET.  */

cgraph_node_set_iterator
cgraph_node_set_find (cgraph_node_set set, struct cgraph_node *node)
{
  void **slot;
  cgraph_node_set_iterator csi;

  slot = pointer_map_contains (set->map, node);
  if (slot == NULL || !*slot)
    csi.index = (unsigned) ~0;
  else
    csi.index = (size_t)*slot - 1;
  csi.set = set;

  return csi;
}


/* Dump content of SET to file F.  */

void
dump_cgraph_node_set (FILE *f, cgraph_node_set set)
{
  cgraph_node_set_iterator iter;

  for (iter = csi_start (set); !csi_end_p (iter); csi_next (&iter))
    {
      struct cgraph_node *node = csi_node (iter);
      fprintf (f, " %s/%i", node->name (), node->order);
    }
  fprintf (f, "\n");
}


/* Dump content of SET to stderr.  */

DEBUG_FUNCTION void
debug_cgraph_node_set (cgraph_node_set set)
{
  dump_cgraph_node_set (stderr, set);
}


/* Free varpool node set.  */

void
free_cgraph_node_set (cgraph_node_set set)
{
  set->nodes.release ();
  pointer_map_destroy (set->map);
  free (set);
}


/* Create a new varpool node set.  */

varpool_node_set
varpool_node_set_new (void)
{
  varpool_node_set new_node_set;

  new_node_set = XCNEW (struct varpool_node_set_def);
  new_node_set->map = pointer_map_create ();
  new_node_set->nodes.create (0);
  return new_node_set;
}


/* Add varpool_node NODE to varpool_node_set SET.  */

void
varpool_node_set_add (varpool_node_set set, struct varpool_node *node)
{
  void **slot;

  slot = pointer_map_insert (set->map, node);

  if (*slot)
    {
      int index = (size_t) *slot - 1;
      gcc_checking_assert ((set->nodes[index]
		           == node));
      return;
    }

  *slot = (void *)(size_t) (set->nodes.length () + 1);

  /* Insert into node vector.  */
  set->nodes.safe_push (node);
}


/* Remove varpool_node NODE from varpool_node_set SET.  */

void
varpool_node_set_remove (varpool_node_set set, struct varpool_node *node)
{
  void **slot, **last_slot;
  int index;
  struct varpool_node *last_node;

  slot = pointer_map_contains (set->map, node);
  if (slot == NULL || !*slot)
    return;

  index = (size_t) *slot - 1;
  gcc_checking_assert (set->nodes[index]
	      	       == node);

  /* Remove from vector. We do this by swapping node with the last element
     of the vector.  */
  last_node = set->nodes.pop ();
  if (last_node != node)
    {
      last_slot = pointer_map_contains (set->map, last_node);
      gcc_checking_assert (last_slot && *last_slot);
      *last_slot = (void *)(size_t) (index + 1);

      /* Move the last element to the original spot of NODE.  */
      set->nodes[index] = last_node;
    }

  /* Remove element from hash table.  */
  *slot = NULL;
}


/* Find NODE in SET and return an iterator to it if found.  A null iterator
   is returned if NODE is not in SET.  */

varpool_node_set_iterator
varpool_node_set_find (varpool_node_set set, struct varpool_node *node)
{
  void **slot;
  varpool_node_set_iterator vsi;

  slot = pointer_map_contains (set->map, node);
  if (slot == NULL || !*slot)
    vsi.index = (unsigned) ~0;
  else
    vsi.index = (size_t)*slot - 1;
  vsi.set = set;

  return vsi;
}


/* Dump content of SET to file F.  */

void
dump_varpool_node_set (FILE *f, varpool_node_set set)
{
  varpool_node_set_iterator iter;

  for (iter = vsi_start (set); !vsi_end_p (iter); vsi_next (&iter))
    {
      struct varpool_node *node = vsi_node (iter);
      fprintf (f, " %s", node->name ());
    }
  fprintf (f, "\n");
}


/* Free varpool node set.  */

void
free_varpool_node_set (varpool_node_set set)
{
  set->nodes.release ();
  pointer_map_destroy (set->map);
  free (set);
}


/* Dump content of SET to stderr.  */

DEBUG_FUNCTION void
debug_varpool_node_set (varpool_node_set set)
{
  dump_varpool_node_set (stderr, set);
}


/* SRC and DST are going to be merged.  Take SRC's profile and merge it into
   DST so it is not going to be lost.  Destroy SRC's body on the way.  */

void
ipa_merge_profiles (struct cgraph_node *dst,
		    struct cgraph_node *src)
{
  tree oldsrcdecl = src->decl;
  struct function *srccfun, *dstcfun;
  bool match = true;

  if (!src->definition
      || !dst->definition)
    return;
  if (src->frequency < dst->frequency)
    src->frequency = dst->frequency;
  if (!dst->count)
    return;
  if (cgraph_dump_file)
    {
      fprintf (cgraph_dump_file, "Merging profiles of %s/%i to %s/%i\n",
	       xstrdup (src->name ()), src->order,
	       xstrdup (dst->name ()), dst->order);
    }
  dst->count += src->count;

  /* This is ugly.  We need to get both function bodies into memory.
     If declaration is merged, we need to duplicate it to be able
     to load body that is being replaced.  This makes symbol table
     temporarily inconsistent.  */
  if (src->decl == dst->decl)
    {
      void **slot;
      struct lto_in_decl_state temp;
      struct lto_in_decl_state *state;

      /* We are going to move the decl, we want to remove its file decl data.
	 and link these with the new decl. */
      temp.fn_decl = src->decl;
      slot = htab_find_slot (src->lto_file_data->function_decl_states,
			     &temp, NO_INSERT);
      state = (lto_in_decl_state *)*slot;
      htab_clear_slot (src->lto_file_data->function_decl_states, slot);
      gcc_assert (state);

      /* Duplicate the decl and be sure it does not link into body of DST.  */
      src->decl = copy_node (src->decl);
      DECL_STRUCT_FUNCTION (src->decl) = NULL;
      DECL_ARGUMENTS (src->decl) = NULL;
      DECL_INITIAL (src->decl) = NULL;
      DECL_RESULT (src->decl) = NULL;

      /* Associate the decl state with new declaration, so LTO streamer
 	 can look it up.  */
      state->fn_decl = src->decl;
      slot = htab_find_slot (src->lto_file_data->function_decl_states,
			     state, INSERT);
      gcc_assert (!*slot);
      *slot = state;
    }
  cgraph_get_body (src);
  cgraph_get_body (dst);
  srccfun = DECL_STRUCT_FUNCTION (src->decl);
  dstcfun = DECL_STRUCT_FUNCTION (dst->decl);
  if (n_basic_blocks_for_function (srccfun)
      != n_basic_blocks_for_function (dstcfun))
    {
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file,
		 "Giving up; number of basic block mismatch.\n");
      match = false;
    }
  else if (last_basic_block_for_function (srccfun)
	   != last_basic_block_for_function (dstcfun))
    {
      if (cgraph_dump_file)
	fprintf (cgraph_dump_file,
		 "Giving up; last block mismatch.\n");
      match = false;
    }
  else 
    {
      basic_block srcbb, dstbb;

      FOR_ALL_BB_FN (srcbb, srccfun)
	{
	  unsigned int i;

	  dstbb = BASIC_BLOCK_FOR_FUNCTION (dstcfun, srcbb->index);
	  if (dstbb == NULL)
	    {
	      if (cgraph_dump_file)
		fprintf (cgraph_dump_file,
			 "No matching block for bb %i.\n",
			 srcbb->index);
	      match = false;
	      break;
	    }
	  if (EDGE_COUNT (srcbb->succs) != EDGE_COUNT (dstbb->succs))
	    {
	      if (cgraph_dump_file)
		fprintf (cgraph_dump_file,
			 "Edge count mistmatch for bb %i.\n",
			 srcbb->index);
	      match = false;
	      break;
	    }
	  for (i = 0; i < EDGE_COUNT (srcbb->succs); i++)
	    {
	      edge srce = EDGE_SUCC (srcbb, i);
	      edge dste = EDGE_SUCC (dstbb, i);
	      if (srce->dest->index != dste->dest->index)
		{
		  if (cgraph_dump_file)
		    fprintf (cgraph_dump_file,
			     "Succ edge mistmatch for bb %i.\n",
			     srce->dest->index);
		  match = false;
		  break;
		}
	    }
	}
    }
  if (match)
    {
      struct cgraph_edge *e;
      basic_block srcbb, dstbb;

      /* TODO: merge also statement histograms.  */
      FOR_ALL_BB_FN (srcbb, srccfun)
	{
	  unsigned int i;

	  dstbb = BASIC_BLOCK_FOR_FUNCTION (dstcfun, srcbb->index);
	  dstbb->count += srcbb->count;
	  for (i = 0; i < EDGE_COUNT (srcbb->succs); i++)
	    {
	      edge srce = EDGE_SUCC (srcbb, i);
	      edge dste = EDGE_SUCC (dstbb, i);
	      dste->count += srce->count;
	    }
	}
      push_cfun (dstcfun);
      counts_to_freqs ();
      compute_function_frequency ();
      pop_cfun ();
      for (e = dst->callees; e; e = e->next_callee)
	{
	  gcc_assert (!e->speculative);
	  e->count = gimple_bb (e->call_stmt)->count;
	  e->frequency = compute_call_stmt_bb_frequency
			     (dst->decl,
			      gimple_bb (e->call_stmt));
	}
      for (e = dst->indirect_calls; e; e = e->next_callee)
	{
	  gcc_assert (!e->speculative);
	  e->count = gimple_bb (e->call_stmt)->count;
	  e->frequency = compute_call_stmt_bb_frequency
			     (dst->decl,
			      gimple_bb (e->call_stmt));
	}
      cgraph_release_function_body (src);
      inline_update_overall_summary (dst);
    }
  /* TODO: if there is no match, we can scale up.  */
  src->decl = oldsrcdecl;
}

/* Return true if call to DEST is known to be self-recusive call withing FUNC.   */

bool
recursive_call_p (tree func, tree dest)
{
  struct cgraph_node *dest_node = cgraph_get_create_node (dest);
  struct cgraph_node *cnode = cgraph_get_create_node (func);

  return symtab_semantically_equivalent_p (dest_node,
					   cnode);
}
