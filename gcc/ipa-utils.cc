/* Utilities for ipa analysis.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "dumpfile.h"
#include "splay-tree.h"
#include "ipa-utils.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "tree-ssa-loop-niter.h"
#include "calls.h"
#include "cfgloop.h"
#include "cfganal.h"

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
    order[i]->dump (out);
  fprintf (out, "\n");
  fflush (out);
}


struct searchc_env {
  struct cgraph_node **stack;
  struct cgraph_node **result;
  int stack_size;
  int order_pos;
  splay_tree nodes_marked_new;
  bool reduce;
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
  splay_tree_remove (env->nodes_marked_new, v->get_uid ());

  v_info->dfn_number = env->count;
  v_info->low_link = env->count;
  env->count++;
  env->stack[(env->stack_size)++] = v;
  v_info->on_stack = true;

  for (edge = v->callees; edge; edge = edge->next_callee)
    {
      struct ipa_dfs_info * w_info;
      enum availability avail;
      struct cgraph_node *w = edge->callee->ultimate_alias_target (&avail);

      if (!w || (ignore_edge && ignore_edge (edge)))
        continue;

      if (w->aux
	  && (avail >= AVAIL_INTERPOSABLE))
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
		       bool reduce,
		       bool (*ignore_edge) (struct cgraph_edge *))
{
  struct cgraph_node *node;
  struct searchc_env env;
  splay_tree_node result;
  env.stack = XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  env.stack_size = 0;
  env.result = order;
  env.order_pos = 0;
  env.nodes_marked_new = splay_tree_new (splay_tree_compare_ints, 0, 0);
  env.count = 1;
  env.reduce = reduce;

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      enum availability avail = node->get_availability ();

      if (avail > AVAIL_INTERPOSABLE
	  || avail == AVAIL_INTERPOSABLE)
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
			     (splay_tree_key)node->get_uid (),
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

vec<cgraph_node *>
ipa_get_nodes_in_cycle (struct cgraph_node *node)
{
  vec<cgraph_node *> v = vNULL;
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
  struct cgraph_node *callee = cs->callee->function_symbol ();

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
  struct ipa_ref *ref = NULL;

  struct postorder_stack *stack =
    XCNEWVEC (struct postorder_stack, symtab->cgraph_count);

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
		  && !node->inlined_to
		  && !node->alias && !node->thunk
		  && !node->only_called_directly_p ())))
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
		    }
		  for (; stack[stack_size].node->iterate_referring (
						       stack[stack_size].ref,
						       ref) && !node2;
		       stack[stack_size].ref++)
		    {
		      if (ref->use == IPA_REF_ALIAS)
			node2 = dyn_cast <cgraph_node *> (ref->referring);
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

/* Scale function of calls in NODE by ratio ORIG_COUNT/NODE->count.  */

void
scale_ipa_profile_for_fn (struct cgraph_node *node, profile_count orig_count)
{
  profile_count to = node->count;
  profile_count::adjust_for_ipa_scaling (&to, &orig_count);
  struct cgraph_edge *e;
  
  for (e = node->callees; e; e = e->next_callee)
    e->count = e->count.apply_scale (to, orig_count);
  for (e = node->indirect_calls; e; e = e->next_callee)
    e->count = e->count.apply_scale (to, orig_count);
}

/* SRC and DST are going to be merged.  Take SRC's profile and merge it into
   DST so it is not going to be lost.  Possibly destroy SRC's body on the way
   unless PRESERVE_BODY is set.  */

void
ipa_merge_profiles (struct cgraph_node *dst,
		    struct cgraph_node *src,
		    bool preserve_body)
{
  tree oldsrcdecl = src->decl;
  struct function *srccfun, *dstcfun;
  bool match = true;
  bool copy_counts = false;

  if (!src->definition
      || !dst->definition)
    return;

  if (src->frequency < dst->frequency)
    src->frequency = dst->frequency;

  /* Time profiles are merged.  */
  if (dst->tp_first_run > src->tp_first_run && src->tp_first_run)
    dst->tp_first_run = src->tp_first_run;

  if (src->profile_id && !dst->profile_id)
    dst->profile_id = src->profile_id;

  /* Merging zero profile to dst is no-op.  */
  if (src->count.ipa () == profile_count::zero ())
    return;

  /* FIXME when we merge in unknown profile, we ought to set counts as
     unsafe.  */
  if (!src->count.initialized_p ()
      || !(src->count.ipa () == src->count))
    return;
  profile_count orig_count = dst->count;

  /* Either sum the profiles if both are IPA and not global0, or
     pick more informative one (that is nonzero IPA if other is
     uninitialized, guessed or global0).   */

  if ((dst->count.ipa ().nonzero_p ()
       || src->count.ipa ().nonzero_p ())
      && dst->count.ipa ().initialized_p ()
      && src->count.ipa ().initialized_p ())
    dst->count = dst->count.ipa () + src->count.ipa ();
  else if (dst->count.ipa ().initialized_p ())
    ;
  else if (src->count.ipa ().initialized_p ())
    {
      copy_counts = true;
      dst->count = src->count.ipa ();
    }

  /* If no updating needed return early.  */
  if (dst->count == orig_count)
    return;

  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "Merging profiles of %s count:",
	       src->dump_name ());
      src->count.dump (symtab->dump_file);
      fprintf (symtab->dump_file, " to %s count:",
	       dst->dump_name ());
      orig_count.dump (symtab->dump_file);
      fprintf (symtab->dump_file, " resulting count:");
      dst->count.dump (symtab->dump_file);
      fprintf (symtab->dump_file, "\n");
    }

  /* First handle functions with no gimple body.  */
  if (dst->thunk || dst->alias
      || src->thunk || src->alias)
    {
      scale_ipa_profile_for_fn (dst, orig_count);
      return;
    }

  /* This is ugly.  We need to get both function bodies into memory.
     If declaration is merged, we need to duplicate it to be able
     to load body that is being replaced.  This makes symbol table
     temporarily inconsistent.  */
  if (src->decl == dst->decl)
    {
      struct lto_in_decl_state temp;
      struct lto_in_decl_state *state;

      /* We are going to move the decl, we want to remove its file decl data.
	 and link these with the new decl. */
      temp.fn_decl = src->decl;
      lto_in_decl_state **slot
	= src->lto_file_data->function_decl_states->find_slot (&temp,
							       NO_INSERT);
      state = *slot;
      src->lto_file_data->function_decl_states->clear_slot (slot);
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
      slot
	= src->lto_file_data->function_decl_states->find_slot (state, INSERT);
      gcc_assert (!*slot);
      *slot = state;
    }
  src->get_untransformed_body ();
  dst->get_untransformed_body ();
  srccfun = DECL_STRUCT_FUNCTION (src->decl);
  dstcfun = DECL_STRUCT_FUNCTION (dst->decl);
  if (n_basic_blocks_for_fn (srccfun)
      != n_basic_blocks_for_fn (dstcfun))
    {
      if (symtab->dump_file)
	fprintf (symtab->dump_file,
		 "Giving up; number of basic block mismatch.\n");
      match = false;
    }
  else if (last_basic_block_for_fn (srccfun)
	   != last_basic_block_for_fn (dstcfun))
    {
      if (symtab->dump_file)
	fprintf (symtab->dump_file,
		 "Giving up; last block mismatch.\n");
      match = false;
    }
  else 
    {
      basic_block srcbb, dstbb;
      struct cgraph_edge *e, *e2;

      for (e = dst->callees, e2 = src->callees; e && e2 && match;
	   e2 = e2->next_callee, e = e->next_callee)
	{
	  if (gimple_bb (e->call_stmt)->index
	      != gimple_bb (e2->call_stmt)->index)
	    {
	      if (symtab->dump_file)
		fprintf (symtab->dump_file,
			 "Giving up; call stmt mismatch.\n");
	      match = false;
	    }
	}
      if (e || e2)
	{
	  if (symtab->dump_file)
	    fprintf (symtab->dump_file,
		     "Giving up; number of calls differs.\n");
	  match = false;
	}
      for (e = dst->indirect_calls, e2 = src->indirect_calls; e && e2 && match;
	   e2 = e2->next_callee, e = e->next_callee)
	{
	  if (gimple_bb (e->call_stmt)->index
	      != gimple_bb (e2->call_stmt)->index)
	    {
	      if (symtab->dump_file)
		fprintf (symtab->dump_file,
			 "Giving up; indirect call stmt mismatch.\n");
	      match = false;
	    }
	}
      if (e || e2)
	{
	  if (symtab->dump_file)
	    fprintf (symtab->dump_file,
		     "Giving up; number of indirect calls differs.\n");
	  match=false;
	}

      if (match)
	FOR_ALL_BB_FN (srcbb, srccfun)
	  {
	    unsigned int i;

	    dstbb = BASIC_BLOCK_FOR_FN (dstcfun, srcbb->index);
	    if (dstbb == NULL)
	      {
		if (symtab->dump_file)
		  fprintf (symtab->dump_file,
			   "No matching block for bb %i.\n",
			   srcbb->index);
		match = false;
		break;
	      }
	    if (EDGE_COUNT (srcbb->succs) != EDGE_COUNT (dstbb->succs))
	      {
		if (symtab->dump_file)
		  fprintf (symtab->dump_file,
			   "Edge count mismatch for bb %i.\n",
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
		    if (symtab->dump_file)
		      fprintf (symtab->dump_file,
			       "Succ edge mismatch for bb %i.\n",
			       srce->dest->index);
		    match = false;
		    break;
		  }
	      }
	  }
    }
  if (match)
    {
      struct cgraph_edge *e, *e2;
      basic_block srcbb, dstbb;

      /* Function and global profile may be out of sync.  First scale it same
	 way as fixup_cfg would.  */
      profile_count srcnum = src->count;
      profile_count srcden = ENTRY_BLOCK_PTR_FOR_FN (srccfun)->count;
      bool srcscale = srcnum.initialized_p () && !(srcnum == srcden);
      profile_count dstnum = orig_count;
      profile_count dstden = ENTRY_BLOCK_PTR_FOR_FN (dstcfun)->count;
      bool dstscale = !copy_counts
		      && dstnum.initialized_p () && !(dstnum == dstden);

      /* TODO: merge also statement histograms.  */
      FOR_ALL_BB_FN (srcbb, srccfun)
	{
	  unsigned int i;

	  dstbb = BASIC_BLOCK_FOR_FN (dstcfun, srcbb->index);

	  profile_count srccount = srcbb->count;
	  if (srcscale)
	    srccount = srccount.apply_scale (srcnum, srcden);
	  if (dstscale)
	    dstbb->count = dstbb->count.apply_scale (dstnum, dstden);

	  if (copy_counts)
	    {
	      dstbb->count = srccount;
	      for (i = 0; i < EDGE_COUNT (srcbb->succs); i++)
		{
		  edge srce = EDGE_SUCC (srcbb, i);
		  edge dste = EDGE_SUCC (dstbb, i);
		  if (srce->probability.initialized_p ())
		    dste->probability = srce->probability;
		}
	    }	
	  else 
	    {
	      for (i = 0; i < EDGE_COUNT (srcbb->succs); i++)
		{
		  edge srce = EDGE_SUCC (srcbb, i);
		  edge dste = EDGE_SUCC (dstbb, i);
		  profile_count sum =
		    dstbb->count.ipa () + srccount.ipa ();
		  if (sum.nonzero_p ())
		    dste->probability =
		      dste->probability * dstbb->count.ipa ().probability_in
						   (sum)
		      + srce->probability * srcbb->count.ipa ().probability_in
						   (sum);
		}
	      dstbb->count = dstbb->count.ipa () + srccount.ipa ();
	    }
	}
      push_cfun (dstcfun);
      update_max_bb_count ();
      compute_function_frequency ();
      pop_cfun ();
      for (e = dst->callees; e; e = e->next_callee)
	{
	  if (e->speculative)
	    continue;
	  e->count = gimple_bb (e->call_stmt)->count;
	}
      for (e = dst->indirect_calls, e2 = src->indirect_calls; e;
	   e2 = (e2 ? e2->next_callee : NULL), e = e->next_callee)
	{
	  if (!e->speculative && !e2->speculative)
	    {
	      /* FIXME: we need to also merge ipa-profile histograms
		 because with LTO merging happens from lto-symtab before
		 these are converted to indirect edges.  */
	      e->count = gimple_bb (e->call_stmt)->count;
	      continue;
	    }

	  /* When copying just remove all speuclations on dst and then copy
	     one from src.  */
	  if (copy_counts)
	    {
	      while (e->speculative)
		cgraph_edge::resolve_speculation (e, NULL);
	      e->count = gimple_bb (e->call_stmt)->count;
	      if (e2->speculative)
		{
		  for (cgraph_edge *e3 = e2->first_speculative_call_target ();
		       e3;
		       e3 = e3->next_speculative_call_target ())
		    {
		      cgraph_edge *ns;
		      ns = e->make_speculative
			 (dyn_cast <cgraph_node *>
			    (e3->speculative_call_target_ref ()->referred),
			     e3->count, e3->speculative_id);
		      /* Target may differ from ref (for example it may be
			 redirected to local alias.  */
		      ns->redirect_callee (e3->callee);
		    }
		}
	      continue;
	    }

	  /* Iterate all speculations in SRC, see if corresponding ones exist
	     int DST and if so, sum the counts.  Otherwise create new
	     speculation.  */
	  int max_spec = 0;
	  for (cgraph_edge *e3 = e->first_speculative_call_target ();
	       e3;
	       e3 = e3->next_speculative_call_target ())
	    if (e3->speculative_id > max_spec)
	      max_spec = e3->speculative_id;
	  for (cgraph_edge *e3 = e2->first_speculative_call_target ();
	       e3;
	       e3 = e3->next_speculative_call_target ())
	    {
	      cgraph_edge *te
		 = e->speculative_call_for_target
			 (dyn_cast <cgraph_node *>
			    (e3->speculative_call_target_ref ()->referred));
	      if (te)
		te->count = te->count + e3->count;
	      else
		{
		  e->count = e->count + e3->count;
		  cgraph_edge *ns;
		  ns = e->make_speculative
			 (dyn_cast <cgraph_node *>
			    (e3->speculative_call_target_ref ()
			     ->referred),
			  e3->count,
			  e3->speculative_id + max_spec + 1);
		  /* Target may differ from ref (for example it may be
		     redirected to local alias.  */
		  ns->redirect_callee (e3->callee);
		}
	    }
	}
      if (!preserve_body)
        src->release_body ();
      /* Update summary.  */
      compute_fn_summary (dst, 0);
    }
  /* We can't update CFG profile, but we can scale IPA profile. CFG
     will be scaled according to dst->count after IPA passes.  */
  else
    scale_ipa_profile_for_fn (dst, orig_count);
  src->decl = oldsrcdecl;
}

/* Return true if call to DEST is known to be self-recusive
   call withing FUNC.  */

bool
recursive_call_p (tree func, tree dest)
{
  struct cgraph_node *dest_node = cgraph_node::get_create (dest);
  struct cgraph_node *cnode = cgraph_node::get_create (func);
  ipa_ref *alias;
  enum availability avail;

  gcc_assert (!cnode->alias);
  if (cnode != dest_node->ultimate_alias_target (&avail))
    return false;
  if (avail >= AVAIL_AVAILABLE)
    return true;
  if (!dest_node->semantically_equivalent_p (cnode))
    return false;
  /* If there is only one way to call the fuction or we know all of them
     are semantically equivalent, we still can consider call recursive.  */
  FOR_EACH_ALIAS (cnode, alias)
    if (!dest_node->semantically_equivalent_p (alias->referring))
      return false;
  return true;
}

/* Return true if stmt may terminate execution of function.
   If assume_return_or_eh we can further assume that the function ends
   either by retrn statement or EH (no trapping or infinite loops).  */

bool
stmt_may_terminate_function_p (function *fun, gimple *stmt, bool assume_return_or_eh)
{
  if (stmt_can_throw_external (fun, stmt))
    return true;
  if (assume_return_or_eh)
    return false;
  gasm *astmt = dyn_cast <gasm *> (stmt);
  if (astmt && gimple_asm_volatile_p (astmt))
    return true;
  if (gimple_could_trap_p (stmt))
    return true;
  if (gcall *call = dyn_cast <gcall *> (stmt))
    {
      int flags = gimple_call_flags (call);
      if (flags & (ECF_PURE | ECF_CONST) && ! (flags & ECF_LOOPING_CONST_OR_PURE))
	return false;
      modref_summary *s = get_modref_function_summary (call, NULL);
      if (s && !s->side_effects)
	return false;
      return true;
    }
  return false;
}

/* Return bitmap of all basic blocks whose first statements are known to
   execute on every invocation of the function.

   If assume_return_or_eh we can further assume that the function ends
   either by retrn statement or EH (no trapping or infinite loops).
   This is useful when sumarizing function in passes like ipa-modref.
 
   Seeing assume_return_or_eh to false is used to prove that given
   statmeent will be executed even if the function gets into infinite
   loop or trap.  */
bitmap
find_always_executed_bbs (function *fun, bool assume_return_or_eh)
{
  auto_vec<basic_block, 20> stack;
  auto_vec<basic_block, 20> terminating_bbs;
  hash_set<basic_block> visited;
  hash_set<basic_block> terminating_bbs_set;
  edge e;
  edge_iterator ei;
  int flags = flags_from_decl_or_type (fun->decl);
  /* PUre and const functions always return.  */
  assume_return_or_eh |= (flags & (ECF_CONST|ECF_PURE)) && !(flags & ECF_LOOPING_CONST_OR_PURE);
  if (!assume_return_or_eh)
    mark_dfs_back_edges (fun);

  /* First walk all BBs reachable from entry stopping on statements that may
     terminate execution.  Everything past this statement is not going to be executed
     each invocation.  */
  stack.safe_push (ENTRY_BLOCK_PTR_FOR_FN (fun));
  while (!stack.is_empty ())
    {
      basic_block bb = stack.pop ();
      bool found = false, found_exit = false;
      if (bb->index == EXIT_BLOCK)
	continue;
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->dest == EXIT_BLOCK_PTR_FOR_FN (fun))
	    {
	      found_exit = true;
	      break;
	    }
	  /* Watch for infinite loops.  */
	  if (!found
	      && !assume_return_or_eh && (e->flags & EDGE_DFS_BACK))
	    {
	      if (!dom_info_available_p (CDI_DOMINATORS))
		calculate_dominance_info (CDI_DOMINATORS);
	      /* If this is not a loop latch edge it is an irreducible region.
		 Assume that it is infinite.
		 TODO: with C++ forced progression we can still walk the
		 irreducible region and see if it contains any side effects.
		 Similarly for loops.  -ffinite-loops does not really imply
		 this since we allow inlining across -ffinite-loops bondary
		 and thus it can be used only as a loop flag.  */
	      if (e->dest->loop_father->header != e->dest
		  || !dominated_by_p (CDI_DOMINATORS, bb, e->dest))
		found = true;
	      else if (!finite_loop_p (e->dest->loop_father))
		found = true;
	    }
	}
      if (!assume_return_or_eh
	  && (EDGE_COUNT (bb->succs) == 0 || (bb->flags & BB_IRREDUCIBLE_LOOP)))
	found = true;
      for (gimple_stmt_iterator si = gsi_start_nondebug_after_labels_bb (bb);
	   !gsi_end_p (si) && !found; gsi_next_nondebug (&si))
	if (stmt_may_terminate_function_p (fun, gsi_stmt (si), assume_return_or_eh))
	  {
	    found = true;
	    break;
	  }
      if (found)
	{
	  visited.add (EXIT_BLOCK_PTR_FOR_FN (fun));
	  if (!found_exit)
	    {
	      terminating_bbs.safe_push (bb);
	      terminating_bbs_set.add (bb);
	    }
	}
      else
	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (!visited.add (e->dest))
	    stack.safe_push (e->dest);
    }

  /* Next walk from exit block and find all articulations in the CFG.
     Add all terminating basic blocks as "fake" predecessors of the
     exit block.  */

  bitmap ret = BITMAP_ALLOC (NULL);
  /* A degenerated case when there is no path to exit.  */
  if (!visited.contains (EXIT_BLOCK_PTR_FOR_FN (fun)))
    {
      bitmap_set_bit (ret,
		      single_succ_edge
		        (ENTRY_BLOCK_PTR_FOR_FN (fun))->dest->index);
      return ret;
    }

  struct astate
  {
    unsigned int dfs_preorder;
    unsigned int dfs_postorder;

    unsigned int low, high;
  };

  struct worklist
  {
    basic_block bb;
    astate *cstate;
  };

  struct obstack state_obstack;
  gcc_obstack_init (&state_obstack);
  hash_map<basic_block, astate *> state;
  auto_vec<worklist, 32> worklist_vec;
  unsigned int next_dfs_num = 1;

  /* Always executed blocks are blocks that are on every path from entry to exit.
     We proceed in two steps.  First we do backward DFS walk (so we know that entry
     is always reached) and record preorder and postorder visiting times.

     In second step we proceed in postorder and for every block A we compute
     minimal preorder (A.low) and maximal postorder (A.high) of block reachable
     from the BBs in DFS subtree of A.  If A is always executed there are no
     edges out of this subtree.  This can be tested by checking that A.low == A.preorder
     and B.high == A.postorder.
    
     This is first step. Do backward DFS walk and record preorder, postorder
     and predecessor info.  Initialize stack in postorder.  */
  worklist we = {EXIT_BLOCK_PTR_FOR_FN (fun), NULL};
  worklist_vec.safe_push (we);
  while (!worklist_vec.is_empty ())
    {
      worklist &w = worklist_vec.last ();
      basic_block bb = w.bb;
      astate *cstate = w.cstate;

      if (!cstate)
	{
	  astate **slot = &state.get_or_insert (bb);

	  cstate = *slot;
	  /* Already processed by DFS?  */
	  if (cstate)
	    {
	      worklist_vec.pop ();
	      continue;
	    }
	  /* DFS is visiting BB for first time.  */
	  *slot = cstate = XOBNEW (&state_obstack, struct astate);
	  cstate->low = cstate->high = cstate->dfs_preorder = next_dfs_num++;
	  w.cstate = cstate;
	  /* Exit block is special; process all fake edges we identified.  */
	  if (bb == EXIT_BLOCK_PTR_FOR_FN (fun))
	    for (basic_block bb2 : terminating_bbs)
	      {
		worklist we = {bb2, NULL};
		worklist_vec.safe_push (we);
	      }
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (visited.contains (e->src))
	      {
		worklist we = {e->src, NULL};
		worklist_vec.safe_push (we);
	      }
	  /* Keep BB on worklist so we process it last time.  */
	  continue;
	}
      /* We are finished with processing reachable BBs, see if we have articulation.  */
      worklist_vec.pop ();
      cstate->high = cstate->dfs_postorder = next_dfs_num++;
      stack.safe_push (bb);
    }
  /* This is the final postorder walk.  Determine low and high values and mark
     always executed blocks.  */
  for (basic_block bb : stack)
    {
      astate *cstate = *state.get (bb);
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  astate **cstate2 = state.get (e->src);
	  /* We skip walking part of CFG reached only after first edge to exit.
	     No BB reachable from the skipped part is always executed */
	  if (!cstate2)
	    {
	      if (e->src != ENTRY_BLOCK_PTR_FOR_FN (fun))
		cstate->low = 0;
	      continue;
	    }
	  cstate->low = MIN (cstate->low, (*cstate2)->low);
	  cstate->high = MAX (cstate->high, (*cstate2)->high);
	}
      if (dump_file && (dump_flags & TDF_DETAILS) && bb != EXIT_BLOCK_PTR_FOR_FN (fun))
	fprintf (dump_file, "BB %i %s preorder %i posorder %i low %i high %i\n",
		 bb->index, terminating_bbs_set.contains (bb) ? "(terminating)": "",
		 cstate->dfs_preorder, cstate->dfs_postorder, cstate->low, cstate->high);
      if (cstate->low == cstate->dfs_preorder && cstate->high == cstate->dfs_postorder
	  && bb != EXIT_BLOCK_PTR_FOR_FN (fun))
	bitmap_set_bit (ret, bb->index);
      if (terminating_bbs_set.contains (bb))
	cstate->low = 0;
      else
	FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    astate **cstate2 = state.get (e->dest);
	    if (!cstate2)
	      continue;
	    cstate->low = MIN (cstate->low, (*cstate2)->low);
	    cstate->high = MAX (cstate->high, (*cstate2)->high);
	  }
      }
  obstack_free (&state_obstack, NULL);
  if (dump_file)
    {
      fprintf (dump_file, "Always executed bbbs %s: ",
	       assume_return_or_eh ? "(assuming return or EH)": "");
      bitmap_print (dump_file, ret, "", "\n");
    }

  return ret;
}
