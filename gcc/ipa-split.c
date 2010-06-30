/* Function splitting pass
   Copyright (C) 2010
   Free Software Foundation, Inc.
   Contributed by Jan Hubicka  <jh@suse.cz>

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

/* The purpose of this pass is to split function bodies to improve
   inlining.  I.e. for function of the form:

   func (...)
     {
       if (cheap_test)
	 something_small
       else
	 something_big
     }

   Produce:

   func.part (...)
     {
	something_big
     }

   func (...)
     {
       if (cheap_test)
	 something_small
       else
	 func.part (...);
     }

   When func becomes inlinable and when cheap_test is often true, inlining func,
   but not fund.part leads to performance imrovement similar as inlining
   original func while the code size growth is smaller.

   The pass is organized in three stages:
   1) Collect local info about basic block into BB_INFO structure and
      compute function body estimated size and time.
   2) Via DFS walk find all possible basic blocks where we can split
      and chose best one.
   3) If split point is found, split at the specified BB by creating a clone
      and updating function to call it.  

   The decisions what functions to split are in execute_split_functions
   and consider_split.  

   There are several possible future improvements for this pass including:

   1) Splitting to break up large functions
   2) Splitting to reduce stack frame usage
   3) Allow split part of function to use values computed in the header part.
      The values needs to be passed to split function, perhaps via same
      interface as for nested functions or as argument.
   4) Support for simple rematerialization.  I.e. when split part use
      value computed in header from function parameter in very cheap way, we
      can just recompute it.
   5) Support splitting of nested functions.
   6) Support non-SSA arguments.  
   7) There is nothing preventing us from producing multiple parts of single function
      when needed or splitting also the parts.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "target.h"
#include "cgraph.h"
#include "ipa-prop.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "flags.h"
#include "timevar.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "tree-inline.h"
#include "fibheap.h"
#include "params.h"
#include "gimple-pretty-print.h"

/* Per basic block info.  */

typedef struct
{
  unsigned int size;
  unsigned int time;
} bb_info;
DEF_VEC_O(bb_info);
DEF_VEC_ALLOC_O(bb_info,heap);

static VEC(bb_info, heap) *bb_info_vec;

/* Description of split point.  */

struct split_point
{
  /* Size of the partitions.  */
  unsigned int header_time, header_size, split_time, split_size;

  /* SSA names that need to be passed into spit funciton.  */
  bitmap ssa_names_to_pass;

  /* Basic block where we split (that will become entry point of new function.  */
  basic_block entry_bb;

  /* Basic blocks we are splitting away.  */
  bitmap split_bbs;
};

/* Best split point found.  */

struct split_point best_split_point;

/* Callback for walk_stmt_load_store_addr_ops.  If T is non-ssa automatic
   variable, check it if it is present in bitmap passed via DATA.  */

static bool
test_nonssa_use (gimple stmt ATTRIBUTE_UNUSED, tree t,
	         void *data ATTRIBUTE_UNUSED)
{
  t = get_base_address (t);

  if (t && !is_gimple_reg (t)
      && ((TREE_CODE (t) == VAR_DECL
	  && auto_var_in_fn_p (t, current_function_decl))
	  || (TREE_CODE (t) == RESULT_DECL)
	  || (TREE_CODE (t) == PARM_DECL)))
    return bitmap_bit_p ((bitmap)data, DECL_UID (t));
  return false;
}

/* Dump split point CURRENT.  */

static void
dump_split_point (FILE * file, struct split_point *current)
{
  fprintf (file,
	   "Split point at BB %i header time:%i header size: %i"
	   " split time: %i split size: %i\n  bbs: ",
	   current->entry_bb->index, current->header_time,
	   current->header_size, current->split_time, current->split_size);
  dump_bitmap (file, current->split_bbs);
  fprintf (file, "  SSA names to pass: ");
  dump_bitmap (file, current->ssa_names_to_pass);
}

/* We found an split_point CURRENT.  NON_SSA_VARS is bitmap of all non ssa
   variables used and RETURN_BB is return basic block.
   See if we can split function here.  */

static void
consider_split (struct split_point *current, bitmap non_ssa_vars,
		basic_block return_bb)
{
  tree parm;
  unsigned int num_args = 0;
  unsigned int call_overhead;
  edge e;
  edge_iterator ei;
  gimple_stmt_iterator bsi;
  unsigned int i;
  int incomming_freq = 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_split_point (dump_file, current);

  FOR_EACH_EDGE (e, ei, current->entry_bb->preds)
    if (!bitmap_bit_p (current->split_bbs, e->src->index))
      incomming_freq += EDGE_FREQUENCY (e);

  /* Do not split when we would end up calling function anyway.  */
  if (incomming_freq
      >= (ENTRY_BLOCK_PTR->frequency
	  * PARAM_VALUE (PARAM_PARTIAL_INLINING_ENTRY_PROBABILITY) / 100))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: incomming frequency is too large.\n");
      return;
    }

  if (!current->header_size)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  Refused: header empty\n");
      gcc_unreachable ();
      return;
    }

  /* Verify that PHI args on entry are either virutal or all their operands
     incomming from header are the same.  */
  for (bsi = gsi_start_phis (current->entry_bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);
      tree val = NULL;

      if (!is_gimple_reg (gimple_phi_result (stmt)))
	continue;
      for (i = 0; i < gimple_phi_num_args (stmt); i++)
	{
	  edge e = gimple_phi_arg_edge (stmt, i);
	  if (!bitmap_bit_p (current->split_bbs, e->src->index))
	    {
	      tree edge_val = gimple_phi_arg_def (stmt, i);
	      if (val && edge_val != val)
	        {
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "  Refused: entry BB has PHI with multiple variants\n");
		  return;
	        }
	      val = edge_val;
	    }
	}
    }


  /* See what argument we will pass to the split function and compute
     call overhead.  */
  call_overhead = eni_size_weights.call_cost;
  for (parm = DECL_ARGUMENTS (current_function_decl); parm;
       parm = TREE_CHAIN (parm))
    {
      if (!is_gimple_reg (parm))
	{
	  if (bitmap_bit_p (non_ssa_vars, DECL_UID (parm)))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  Refused: need to pass non-ssa param values\n");
	      return;
	    }
	}
      else if (gimple_default_def (cfun, parm)
	       && bitmap_bit_p (current->ssa_names_to_pass,
				SSA_NAME_VERSION (gimple_default_def
						  (cfun, parm))))
	{
	  if (!VOID_TYPE_P (TREE_TYPE (parm)))
	    call_overhead += estimate_move_cost (TREE_TYPE (parm));
	  num_args++;
	}
    }
  if (!VOID_TYPE_P (TREE_TYPE (current_function_decl)))
    call_overhead += estimate_move_cost (TREE_TYPE (current_function_decl));

  if (current->split_size <= call_overhead)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: split size is smaller than call overhead\n");
      return;
    }
  if (current->header_size + call_overhead
      >= (unsigned int)(DECL_DECLARED_INLINE_P (current_function_decl)
			? MAX_INLINE_INSNS_SINGLE
			: MAX_INLINE_INSNS_AUTO))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: header size is too large for inline candidate\n");
      return;
    }

  /* FIXME: we currently can pass only SSA function parameters to the split
     arguments.  Once parm_adjustment infrastructure is supported by cloning,
     we can pass more than that.  */
  if (num_args != bitmap_count_bits (current->ssa_names_to_pass))
    {
      
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: need to pass non-param values\n");
      return;
    }

  /* When there are non-ssa vars used in the split region, see if they
     are used in the header region.  If so, reject the split.
     FIXME: we can use nested function support to access both.  */
  if (!bitmap_empty_p (non_ssa_vars))
    {
      basic_block bb;
      FOR_EACH_BB (bb)
	{
	  gimple_stmt_iterator bsi;
	  if (!bitmap_bit_p (current->split_bbs, bb->index))
	    continue;
	  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	    {
	      if (is_gimple_debug (gsi_stmt (bsi)))
		continue;
	      if (walk_stmt_load_store_addr_ops
		  (gsi_stmt (bsi), non_ssa_vars, test_nonssa_use,
		   test_nonssa_use, test_nonssa_use))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "  Refused: split part has non-ssa uses\n");
		  return;
		}
	    }
	  for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	    {
	      if (walk_stmt_load_store_addr_ops
		  (gsi_stmt (bsi), non_ssa_vars, test_nonssa_use,
		   test_nonssa_use, test_nonssa_use))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "  Refused: split part has non-ssa uses\n");
		  return;
		}
	    }
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (e->dest != return_bb)
		continue;
	      for (bsi = gsi_start_phis (return_bb); !gsi_end_p (bsi);
		   gsi_next (&bsi))
		{
		  gimple stmt = gsi_stmt (bsi);
		  tree op = gimple_phi_arg_def (stmt, e->dest_idx);

		  if (!is_gimple_reg (gimple_phi_result (stmt)))
		    continue;
		  if (TREE_CODE (op) != SSA_NAME
		      && test_nonssa_use (stmt, op, non_ssa_vars))
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file,
				 "  Refused: split part has non-ssa uses\n");
		      return;
		    }
		}
	    }
	  }
      return;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Accepted!\n");

  /* At the moment chose split point with lowest frequency and that leaves
     out smallest size of header.
     In future we might re-consider this heuristics.  */
  if (!best_split_point.split_bbs
      || best_split_point.entry_bb->frequency > current->entry_bb->frequency
      || (best_split_point.entry_bb->frequency == current->entry_bb->frequency
	  && best_split_point.split_size < current->split_size))
	
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  New best split point!\n");
      if (best_split_point.ssa_names_to_pass)
	{
	  BITMAP_FREE (best_split_point.ssa_names_to_pass);
	  BITMAP_FREE (best_split_point.split_bbs);
	}
      best_split_point = *current;
      best_split_point.ssa_names_to_pass = BITMAP_ALLOC (NULL);
      bitmap_copy (best_split_point.ssa_names_to_pass,
		   current->ssa_names_to_pass);
      best_split_point.split_bbs = BITMAP_ALLOC (NULL);
      bitmap_copy (best_split_point.split_bbs, current->split_bbs);
    }
}

/* Return basic block containing RETURN statement, or EXIT_BLOCK_PTR if none
   found. 
   When there are multiple RETURN statement, chose one with return value,
   since that one is more likely shared by multiple code paths.
   TODO: We might support multiple return blocks.  */

static basic_block
find_return_bb (void)
{
  edge e;
  edge_iterator ei;
  basic_block return_bb = EXIT_BLOCK_PTR;

  if (EDGE_COUNT (EXIT_BLOCK_PTR->preds) == 1)
    FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR->preds)
      {
	gimple_stmt_iterator bsi;
	bool found_return = false;
	tree retval = NULL_TREE;

	for (bsi = gsi_start_bb (e->src); !gsi_end_p (bsi); gsi_next (&bsi))
	  if (gimple_code (gsi_stmt (bsi)) != GIMPLE_RETURN
	      && gimple_code (gsi_stmt (bsi)) != GIMPLE_LABEL
	      && !is_gimple_debug (gsi_stmt (bsi)))
	    break;
	  else if (gimple_code (gsi_stmt (bsi)) == GIMPLE_RETURN)
	    {
	      found_return = true;
	      retval = gimple_return_retval (gsi_stmt (bsi));
	    }
	if (gsi_end_p (bsi) && found_return)
	  {
	    if (retval)
	      return e->src;
	    else
	      return_bb = e->src;
	  }
      }
  return return_bb;
}

/* Callback for walk_stmt_load_store_addr_ops.  If T is non-ssa automatic
   variable, mark it as used in bitmap passed via DATA. 
   Return true when access to T prevents splitting the function.  */

static bool
mark_nonssa_use (gimple stmt ATTRIBUTE_UNUSED, tree t,
	         void *data ATTRIBUTE_UNUSED)
{
  t = get_base_address (t);

  if (!t || is_gimple_reg (t))
    return false;

  /* At present we can't pass non-SSA arguments to split function.
     FIXME: this can be relaxed by passing references to arguments.  */
  if (TREE_CODE (t) == PARM_DECL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Can not split use of non-ssa function parameter.\n");
      return true;
    }

  if ((TREE_CODE (t) == VAR_DECL && auto_var_in_fn_p (t, current_function_decl))
      || (TREE_CODE (t) == RESULT_DECL))
    bitmap_set_bit ((bitmap)data, DECL_UID (t));
  return false;
}

/* Compute local properties of basic block BB we collect when looking for
   split points.  We look for ssa defs and store them in SET_SSA_NAMES,
   for ssa uses and store them in USED_SSA_NAMES and for any non-SSA automatic
   vars stored in NON_SSA_VARS.

   When BB has edge to RETURN_BB, collect uses in RETURN_BB too.  

   Return false when BB contains something that prevents it from being put into
   split function.  */

static bool
visit_bb (basic_block bb, basic_block return_bb,
	  bitmap set_ssa_names, bitmap used_ssa_names,
	  bitmap non_ssa_vars)
{
  gimple_stmt_iterator bsi;
  edge e;
  edge_iterator ei;
  bool can_split = true;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);
      tree op;
      ssa_op_iter iter;
      tree decl;

      if (is_gimple_debug (stmt))
	continue;

      /* FIXME: We can split regions containing EH.  We can not however
	 split RESX, EH_DISPATCH and EH_POINTER referring to same region
	 into different partitions.  This would require tracking of
	 EH regions and checking in consider_split_point if they 
	 are not used elsewhere.  */
      if (gimple_code (stmt) == GIMPLE_RESX
	  && stmt_can_throw_external (stmt))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Can not split external resx.\n");
	  can_split = false;
	}
      if (gimple_code (stmt) == GIMPLE_EH_DISPATCH)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Can not split eh dispatch.\n");
	  can_split = false;
	}

      /* Check builtins that prevent splitting.  */
      if (gimple_code (stmt) == GIMPLE_CALL
	  && (decl = gimple_call_fndecl (stmt)) != NULL_TREE
	  && DECL_BUILT_IN (decl)
	  && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (decl))
	  {
	  /* FIXME: once we will allow passing non-parm values to split part,
	     we need to be sure to handle correct builtin_stack_save and
	     builtin_stack_restore.  At the moment we are safe; there is no
	     way to store builtin_stack_save result in non-SSA variable
	     since all calls to those are compiler generated.  */
	  case BUILT_IN_APPLY:
	  case BUILT_IN_VA_START:
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Can not split builtin_apply and va_start.\n");
	    can_split = false;
	    break;
	  case BUILT_IN_EH_POINTER:
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Can not split builtin_eh_pointer.\n");
	    can_split = false;
	    break;
	  default:
	    break;
	  }

      FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_DEF)
	bitmap_set_bit (set_ssa_names, SSA_NAME_VERSION (op));
      FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
	bitmap_set_bit (used_ssa_names, SSA_NAME_VERSION (op));
      can_split &= !walk_stmt_load_store_addr_ops (stmt, non_ssa_vars,
						   mark_nonssa_use,
						   mark_nonssa_use,
						   mark_nonssa_use);
    }
  for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gimple stmt = gsi_stmt (bsi);
      unsigned int i;

      if (is_gimple_debug (stmt))
	continue;
      if (!is_gimple_reg (gimple_phi_result (stmt)))
	continue;
      bitmap_set_bit (set_ssa_names,
		      SSA_NAME_VERSION (gimple_phi_result (stmt)));
      for (i = 0; i < gimple_phi_num_args (stmt); i++)
	{
	  tree op = gimple_phi_arg_def (stmt, i);
	  if (TREE_CODE (op) == SSA_NAME)
	    bitmap_set_bit (used_ssa_names, SSA_NAME_VERSION (op));
	}
      can_split &= !walk_stmt_load_store_addr_ops (stmt, non_ssa_vars,
						   mark_nonssa_use,
						   mark_nonssa_use,
						   mark_nonssa_use);
    }
  /* Record also uses comming from PHI operand in return BB.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->dest == return_bb)
      {
	bool found_phi = false;
	for (bsi = gsi_start_phis (return_bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  {
	    gimple stmt = gsi_stmt (bsi);
	    tree op = gimple_phi_arg_def (stmt, e->dest_idx);

	    if (is_gimple_debug (stmt))
	      continue;
	    if (!is_gimple_reg (gimple_phi_result (stmt)))
	      continue;
	    found_phi = true;
	    if (TREE_CODE (op) == SSA_NAME)
	      bitmap_set_bit (used_ssa_names, SSA_NAME_VERSION (op));
	    else
	      can_split &= !mark_nonssa_use (stmt, op, non_ssa_vars);
	  }
	if (!gsi_end_p (gsi_last_bb (return_bb)))
	  {
	    ssa_op_iter iter;
	    gimple stmt = gsi_stmt (gsi_last_bb (return_bb));
	    tree op;
	    if (!found_phi)
	      FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
		bitmap_set_bit (used_ssa_names, SSA_NAME_VERSION (op));
	    can_split &= !walk_stmt_load_store_addr_ops (stmt, non_ssa_vars,
							 mark_nonssa_use,
							 mark_nonssa_use,
							 mark_nonssa_use);
	  }
      }
  return can_split;
}

/* Stack entry for recursive DFS walk in find_split_point.  */

typedef struct
{
  /* Basic block we are examining.  */
  basic_block bb;

  /* SSA names set and used by the BB and all BBs reachable
     from it via DFS walk.  */
  bitmap set_ssa_names, used_ssa_names;
  bitmap non_ssa_vars;

  /* All BBS visited from this BB via DFS walk.  */
  bitmap bbs_visited;

  /* Last examined edge in DFS walk.  Since we walk unoriented graph,
     the value is up to sum of incomming and outgoing edges of BB.  */
  unsigned int edge_num;

  /* Stack entry index of earliest BB reachable from current BB
     or any BB visited later in DFS valk.  */
  int earliest;

  /* Overall time and size of all BBs reached from this BB in DFS walk.  */
  int overall_time, overall_size;

  /* When false we can not split on this BB.  */
  bool can_split;
} stack_entry;
DEF_VEC_O(stack_entry);
DEF_VEC_ALLOC_O(stack_entry,heap);


/* Find all articulations and call consider_split on them.
   OVERALL_TIME and OVERALL_SIZE is time and size of the function.

   We perform basic algorithm for finding an articulation in a graph
   created from CFG by considering it to be an unoriented graph.

   The articulation is discovered via DFS walk. We collect earliest
   basic block on stack that is reachable via backward edge.  Articulation
   is any basic block such that there is no backward edge bypassing it.
   To reduce stack usage we maintain heap allocated stack in STACK vector.
   AUX pointer of BB is set to index it appears in the stack or -1 once
   it is visited and popped off the stack.

   The algorithm finds articulation after visiting the whole component
   reachable by it.  This makes it convenient to collect information about
   the component used by consider_split.  */

static void
find_split_points (int overall_time, int overall_size)
{
  stack_entry first;
  VEC(stack_entry, heap) *stack = NULL;
  basic_block bb;
  basic_block return_bb = find_return_bb ();
  struct split_point current;

  current.header_time = overall_time;
  current.header_size = overall_size;
  current.split_time = 0;
  current.split_size = 0;
  current.ssa_names_to_pass = BITMAP_ALLOC (NULL);

  first.bb = ENTRY_BLOCK_PTR;
  first.edge_num = 0;
  first.overall_time = 0;
  first.overall_size = 0;
  first.earliest = INT_MAX;
  first.set_ssa_names = 0;
  first.used_ssa_names = 0;
  first.bbs_visited = 0;
  VEC_safe_push (stack_entry, heap, stack, &first);
  ENTRY_BLOCK_PTR->aux = (void *)(intptr_t)-1;

  while (!VEC_empty (stack_entry, stack))
    {
      stack_entry *entry = VEC_last (stack_entry, stack);

      /* We are walking an acyclic graph, so edge_num counts
	 succ and pred edges together.  However when considering
         articulation, we want to have processed everything reachable
	 from articulation but nothing that reaches into it.  */
      if (entry->edge_num == EDGE_COUNT (entry->bb->succs)
	  && entry->bb != ENTRY_BLOCK_PTR)
	{
	  int pos = VEC_length (stack_entry, stack);
	  entry->can_split &= visit_bb (entry->bb, return_bb,
					entry->set_ssa_names,
					entry->used_ssa_names,
					entry->non_ssa_vars);
	  if (pos <= entry->earliest && !entry->can_split
	      && dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "found articulation at bb %i but can not split\n",
		     entry->bb->index);
	  if (pos <= entry->earliest && entry->can_split)
	     {
	       if (dump_file && (dump_flags & TDF_DETAILS))
		 fprintf (dump_file, "found articulation at bb %i\n",
			  entry->bb->index);
	       current.entry_bb = entry->bb;
	       current.ssa_names_to_pass = BITMAP_ALLOC (NULL);
	       bitmap_and_compl (current.ssa_names_to_pass,
				 entry->used_ssa_names, entry->set_ssa_names);
	       current.header_time = overall_time - entry->overall_time;
	       current.header_size = overall_size - entry->overall_size;
	       current.split_time = entry->overall_time;
	       current.split_size = entry->overall_size;
	       current.split_bbs = entry->bbs_visited;
	       consider_split (&current, entry->non_ssa_vars, return_bb);
	       BITMAP_FREE (current.ssa_names_to_pass);
	     }
	}
      /* Do actual DFS walk.  */
      if (entry->edge_num
	  < (EDGE_COUNT (entry->bb->succs)
	     + EDGE_COUNT (entry->bb->preds)))
	{
          edge e;
	  basic_block dest;
	  if (entry->edge_num < EDGE_COUNT (entry->bb->succs))
	    {
	      e = EDGE_SUCC (entry->bb, entry->edge_num);
	      dest = e->dest;
	    }
	  else
	    {
	      e = EDGE_PRED (entry->bb, entry->edge_num
			     - EDGE_COUNT (entry->bb->succs));
	      dest = e->src;
	    }

	  entry->edge_num++;

	  /* New BB to visit, push it to the stack.  */
	  if (dest != return_bb && dest != EXIT_BLOCK_PTR
	      && !dest->aux)
	    {
	      stack_entry new_entry;

	      new_entry.bb = dest;
	      new_entry.edge_num = 0;
	      new_entry.overall_time
		 = VEC_index (bb_info, bb_info_vec, dest->index)->time;
	      new_entry.overall_size
		 = VEC_index (bb_info, bb_info_vec, dest->index)->size;
	      new_entry.earliest = INT_MAX;
	      new_entry.set_ssa_names = BITMAP_ALLOC (NULL);
	      new_entry.used_ssa_names = BITMAP_ALLOC (NULL);
	      new_entry.bbs_visited = BITMAP_ALLOC (NULL);
	      new_entry.non_ssa_vars = BITMAP_ALLOC (NULL);
	      new_entry.can_split = true;
	      bitmap_set_bit (new_entry.bbs_visited, dest->index);
	      VEC_safe_push (stack_entry, heap, stack, &new_entry);
	      dest->aux = (void *)(intptr_t)VEC_length (stack_entry, stack);
	    }
	  /* Back edge found, record the earliest point.  */
	  else if ((intptr_t)dest->aux > 0
		   && (intptr_t)dest->aux < entry->earliest)
	    entry->earliest = (intptr_t)dest->aux;
	}
      /* We are done with examing the edges. pop off the value from stack and
	 merge stuff we cummulate during the walk.  */
      else if (entry->bb != ENTRY_BLOCK_PTR)
	{
	  stack_entry *prev = VEC_index (stack_entry, stack,
					 VEC_length (stack_entry, stack) - 2);

	  entry->bb->aux = (void *)(intptr_t)-1;
	  prev->can_split &= entry->can_split;
	  if (prev->set_ssa_names)
	    {
	      bitmap_ior_into (prev->set_ssa_names, entry->set_ssa_names);
	      bitmap_ior_into (prev->used_ssa_names, entry->used_ssa_names);
	      bitmap_ior_into (prev->bbs_visited, entry->bbs_visited);
	      bitmap_ior_into (prev->non_ssa_vars, entry->non_ssa_vars);
	    }
	  if (prev->earliest > entry->earliest)
	    prev->earliest = entry->earliest;
	  prev->overall_time += entry->overall_time;
	  prev->overall_size += entry->overall_size;
	  BITMAP_FREE (entry->set_ssa_names);
	  BITMAP_FREE (entry->used_ssa_names);
	  BITMAP_FREE (entry->bbs_visited);
	  BITMAP_FREE (entry->non_ssa_vars);
	  VEC_pop (stack_entry, stack);
	}
      else
        VEC_pop (stack_entry, stack);
    }
  ENTRY_BLOCK_PTR->aux = NULL;
  FOR_EACH_BB (bb)
    bb->aux = NULL;
  BITMAP_FREE (current.ssa_names_to_pass);
}

/* Split function at SPLIT_POINT.  */

static void
split_function (struct split_point *split_point)
{
  VEC (tree, heap) *args_to_pass = NULL;
  bitmap args_to_skip = BITMAP_ALLOC (NULL);
  tree parm;
  int num = 0;
  struct cgraph_node *node;
  basic_block return_bb = find_return_bb ();
  basic_block call_bb;
  gimple_stmt_iterator gsi;
  gimple call;
  edge e;
  edge_iterator ei;
  tree retval = NULL, real_retval = NULL;
  bool split_part_return_p = false;
  gimple last_stmt = NULL;

  if (dump_file)
    {
      fprintf (dump_file, "\n\nSplitting function at:\n");
      dump_split_point (dump_file, split_point);
    }

  /* Collect the parameters of new function and args_to_skip bitmap.  */
  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm; parm = TREE_CHAIN (parm), num++)
    if (!is_gimple_reg (parm)
	|| !gimple_default_def (cfun, parm)
	|| !bitmap_bit_p (split_point->ssa_names_to_pass,
			  SSA_NAME_VERSION (gimple_default_def (cfun, parm))))
      bitmap_set_bit (args_to_skip, num);
    else
      VEC_safe_push (tree, heap, args_to_pass, gimple_default_def (cfun, parm));

  /* See if the split function will return.  */
  FOR_EACH_EDGE (e, ei, return_bb->preds)
    if (bitmap_bit_p (split_point->split_bbs, e->src->index))
      break;
  if (e)
    split_part_return_p = true;

  /* If we return, we will need the return block.  */
  if (return_bb != EXIT_BLOCK_PTR && split_part_return_p)
    bitmap_set_bit (split_point->split_bbs, return_bb->index);

  /* Now create the actual clone.  */
  rebuild_cgraph_edges ();
  node = cgraph_function_versioning (cgraph_node (current_function_decl),
				     NULL, NULL,
				     args_to_skip,
				     split_point->split_bbs,
				     split_point->entry_bb, "_part");
  /* For usual cloning it is enough to clear builtin only when signature
     changes.  For partial inlining we however can not expect the part
     of builtin implementation to have same semantic as the whole.  */
  if (DECL_BUILT_IN (node->decl))
    {
      DECL_BUILT_IN_CLASS (node->decl) = NOT_BUILT_IN;
      DECL_FUNCTION_CODE (node->decl) = (enum built_in_function) 0;
    }
  cgraph_node_remove_callees (cgraph_node (current_function_decl));
  if (!split_part_return_p)
    TREE_THIS_VOLATILE (node->decl) = 1;
  if (dump_file)
    dump_function_to_file (node->decl, dump_file, dump_flags);

  /* Create the basic block we place call into.  It is the entry basic block
     split after last label.  */
  call_bb = split_point->entry_bb;
  for (gsi = gsi_start_bb (call_bb); !gsi_end_p (gsi);)
    if (gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL)
      {
	last_stmt = gsi_stmt (gsi);
	gsi_next (&gsi);
      }
    else
      break;
  e = split_block (split_point->entry_bb, last_stmt);
  remove_edge (e);

  /* Produce the call statement.  */
  gsi = gsi_last_bb (call_bb);
  call = gimple_build_call_vec (node->decl, args_to_pass);
  gimple_set_block (call, DECL_INITIAL (current_function_decl));

  /* Update return value.  This is bit tricky.  When we do not return,
     do nothing.  When we return we might need to update return_bb
     or produce a new return statement.  */
  if (!split_part_return_p)
    gsi_insert_after (&gsi, call, GSI_NEW_STMT);
  else
    {
      e = make_edge (call_bb, return_bb,
		     return_bb == EXIT_BLOCK_PTR ? 0 : EDGE_FALLTHRU);
      e->count = call_bb->count;
      e->probability = REG_BR_PROB_BASE;
      if (return_bb != EXIT_BLOCK_PTR)
	{
	  gimple return_stmt = gsi_stmt (gsi_last_bb (return_bb));
	  gcc_assert (gimple_code (return_stmt) == GIMPLE_RETURN);

	  if ((real_retval = retval = gimple_return_retval (return_stmt))
	      && !is_gimple_min_invariant (retval)
	      && (TREE_CODE (retval) != SSA_NAME
		  || !SSA_NAME_IS_DEFAULT_DEF (retval)))
	    {
	      gimple_stmt_iterator psi;

	      /* See if there is PHI definind return value.  */
	      for (psi = gsi_start_phis (return_bb);
		   !gsi_end_p (psi); gsi_next (&psi))
		if (is_gimple_reg (gimple_phi_result (gsi_stmt (psi))))
		  break;

	      /* When we have PHI, update PHI.  When there is no PHI,
		 update the return statement itself.  */
	      if (TREE_CODE (retval) == SSA_NAME)
		{
		  retval = make_ssa_name (SSA_NAME_VAR (retval), call);
		  if (TREE_CODE (retval) == SSA_NAME
		      && !gsi_end_p (psi))
		    add_phi_arg (gsi_stmt (psi), retval, e, UNKNOWN_LOCATION);
		  else if (TREE_CODE (retval) == SSA_NAME)
		    {
		      gimple_return_set_retval (return_stmt, retval);
		      update_stmt (return_stmt);
		    }
		}
	      gimple_call_set_lhs (call, retval);
	    }
          gsi_insert_after (&gsi, call, GSI_NEW_STMT);
	}
      else
	{
	  gimple ret;
	  if (!VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))))
	    {
	      retval = DECL_RESULT (current_function_decl);

	      /* We use temporary register to hold value when aggregate_value_p
		 is false.  Similarly for DECL_BY_REFERENCE we must avoid extra
		 copy.  */
	      if (!aggregate_value_p (retval, TREE_TYPE (current_function_decl))
		  && !DECL_BY_REFERENCE (retval))
		retval = create_tmp_reg (TREE_TYPE (retval), NULL);
	      if (is_gimple_reg (retval))
		retval = make_ssa_name (retval, call);
	      gimple_call_set_lhs (call, retval);
	    }
          gsi_insert_after (&gsi, call, GSI_NEW_STMT);
	  ret = gimple_build_return (retval);
	  gsi_insert_after (&gsi, ret, GSI_NEW_STMT);
	}
    }
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  compute_inline_parameters (node);
}

/* Execute function splitting pass.  */

static unsigned int
execute_split_functions (void)
{
  gimple_stmt_iterator bsi;
  basic_block bb;
  int overall_time = 0, overall_size = 0;
  int todo = 0;
  struct cgraph_node *node = cgraph_node (current_function_decl);

  if (flags_from_decl_or_type (current_function_decl) & ECF_NORETURN)
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: noreturn function.\n");
      return 0;
    }
  if (MAIN_NAME_P (DECL_NAME (current_function_decl)))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: main function.\n");
      return 0;
    }
  /* This can be relaxed; function might become inlinable after splitting
     away the uninlinable part.  */
  if (!node->local.inlinable)
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: not inlinable.\n");
      return 0;
    }
  if (node->local.disregard_inline_limits)
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: disregading inline limits.\n");
      return 0;
    }
  /* This can be relaxed; most of versioning tests actually prevents
     a duplication.  */
  if (!tree_versionable_function_p (current_function_decl))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: not versionable.\n");
      return 0;
    }
  /* FIXME: we could support this.  */
  if (DECL_STRUCT_FUNCTION (current_function_decl)->static_chain_decl)
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: nested function.\n");
      return 0;
    }
  /* FIXME: Should be easy to support.  */
  if (DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: returns value by reference.\n");
      return 0;
    }

  /* See if it makes sense to try to split.
     It makes sense to split if we inline, that is if we have direct calls to
     handle or direct calls are possibly going to appear as result of indirect
     inlining or LTO.
     Note that we are not completely conservative about disqualifying functions
     called once.  It is possible that the caller is called more then once and
     then inlining would still benefit.  */
  if ((!node->callers || !node->callers->next_caller)
      && !node->address_taken
      && ((!flag_lto && !flag_whopr) || !node->local.externally_visible))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: not called directly "
		 "or called once.\n");
      return 0;
    }

  /* FIXME: We can actually split if splitting reduces call overhead.  */
  if (!flag_inline_small_functions
      && !DECL_DECLARED_INLINE_P (current_function_decl))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: not autoinlining and function"
		 " is not inline.\n");
      return 0;
    }

  /* Compute local info about basic blocks and determine function size/time.  */
  VEC_safe_grow_cleared (bb_info, heap, bb_info_vec, last_basic_block + 1);
  memset (&best_split_point, 0, sizeof (best_split_point));
  FOR_EACH_BB (bb)
    {
      int time = 0;
      int size = 0;
      int freq = compute_call_stmt_bb_frequency (current_function_decl, bb);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Basic block %i\n", bb->index);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  int this_time, this_size;
	  gimple stmt = gsi_stmt (bsi);

	  this_size = estimate_num_insns (stmt, &eni_size_weights);
	  this_time = estimate_num_insns (stmt, &eni_time_weights) * freq;
	  size += this_size;
	  time += this_time;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  freq:%6i size:%3i time:%3i ",
		       freq, this_size, this_time);
	      print_gimple_stmt (dump_file, stmt, 0, 0);
	    }
	}
      overall_time += time;
      overall_size += size;
      VEC_index (bb_info, bb_info_vec, bb->index)->time = time;
      VEC_index (bb_info, bb_info_vec, bb->index)->size = size;
    }
  find_split_points (overall_time, overall_size);
  if (best_split_point.split_bbs)
    {
      split_function (&best_split_point);
      BITMAP_FREE (best_split_point.ssa_names_to_pass);
      BITMAP_FREE (best_split_point.split_bbs);
      todo = TODO_update_ssa | TODO_cleanup_cfg;
    }
  VEC_free (bb_info, heap, bb_info_vec);
  bb_info_vec = NULL;
  return todo;
}

static bool
gate_split_functions (void)
{
  return flag_partial_inlining;
}

struct gimple_opt_pass pass_split_functions =
{
 {
  GIMPLE_PASS,
  "fnsplit",				/* name */
  gate_split_functions,			/* gate */
  execute_split_functions,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_FNSPLIT,			/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func			/* todo_flags_finish */
 }
};
