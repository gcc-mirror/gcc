/* Function splitting pass
   Copyright (C) 2010-2018 Free Software Foundation, Inc.
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
   but not fund.part leads to performance improvement similar as inlining
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "cfganal.h"
#include "calls.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-inline.h"
#include "params.h"
#include "gimple-pretty-print.h"
#include "ipa-fnsummary.h"
#include "cfgloop.h"

/* Per basic block info.  */

struct split_bb_info
{
  unsigned int size;
  sreal time;
};

static vec<split_bb_info> bb_info_vec;

/* Description of split point.  */

struct split_point
{
  /* Size of the partitions.  */
  sreal header_time, split_time;
  unsigned int header_size, split_size;

  /* SSA names that need to be passed into spit function.  */
  bitmap ssa_names_to_pass;

  /* Basic block where we split (that will become entry point of new function.  */
  basic_block entry_bb;

  /* Count for entering the split part.
     This is not count of the entry_bb because it may be in loop.  */
  profile_count count;

  /* Basic blocks we are splitting away.  */
  bitmap split_bbs;

  /* True when return value is computed on split part and thus it needs
     to be returned.  */
  bool split_part_set_retval;
};

/* Best split point found.  */

struct split_point best_split_point;

/* Set of basic blocks that are not allowed to dominate a split point.  */

static bitmap forbidden_dominators;

static tree find_retval (basic_block return_bb);

/* Callback for walk_stmt_load_store_addr_ops.  If T is non-SSA automatic
   variable, check it if it is present in bitmap passed via DATA.  */

static bool
test_nonssa_use (gimple *, tree t, tree, void *data)
{
  t = get_base_address (t);

  if (!t || is_gimple_reg (t))
    return false;

  if (TREE_CODE (t) == PARM_DECL
      || (VAR_P (t)
	  && auto_var_in_fn_p (t, current_function_decl))
      || TREE_CODE (t) == RESULT_DECL
	 /* Normal labels are part of CFG and will be handled gratefuly.
	    Forced labels however can be used directly by statements and
	    need to stay in one partition along with their uses.  */
      || (TREE_CODE (t) == LABEL_DECL
	  && FORCED_LABEL (t)))
    return bitmap_bit_p ((bitmap)data, DECL_UID (t));

  /* For DECL_BY_REFERENCE, the return value is actually a pointer.  We want
     to pretend that the value pointed to is actual result decl.  */
  if ((TREE_CODE (t) == MEM_REF || INDIRECT_REF_P (t))
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
      && SSA_NAME_VAR (TREE_OPERAND (t, 0))
      && TREE_CODE (SSA_NAME_VAR (TREE_OPERAND (t, 0))) == RESULT_DECL
      && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
    return
      bitmap_bit_p ((bitmap)data,
		    DECL_UID (DECL_RESULT (current_function_decl)));

  return false;
}

/* Dump split point CURRENT.  */

static void
dump_split_point (FILE * file, struct split_point *current)
{
  fprintf (file,
	   "Split point at BB %i\n"
	   "  header time: %f header size: %i\n"
	   "  split time: %f split size: %i\n  bbs: ",
	   current->entry_bb->index, current->header_time.to_double (),
	   current->header_size, current->split_time.to_double (),
	   current->split_size);
  dump_bitmap (file, current->split_bbs);
  fprintf (file, "  SSA names to pass: ");
  dump_bitmap (file, current->ssa_names_to_pass);
}

/* Look for all BBs in header that might lead to the split part and verify
   that they are not defining any non-SSA var used by the split part.
   Parameters are the same as for consider_split.  */

static bool
verify_non_ssa_vars (struct split_point *current, bitmap non_ssa_vars,
		     basic_block return_bb)
{
  bitmap seen = BITMAP_ALLOC (NULL);
  vec<basic_block> worklist = vNULL;
  edge e;
  edge_iterator ei;
  bool ok = true;
  basic_block bb;

  FOR_EACH_EDGE (e, ei, current->entry_bb->preds)
    if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	&& !bitmap_bit_p (current->split_bbs, e->src->index))
      {
        worklist.safe_push (e->src);
	bitmap_set_bit (seen, e->src->index);
      }

  while (!worklist.is_empty ())
    {
      bb = worklist.pop ();
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	    && bitmap_set_bit (seen, e->src->index))
	  {
	    gcc_checking_assert (!bitmap_bit_p (current->split_bbs,
					        e->src->index));
	    worklist.safe_push (e->src);
	  }
      for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	{
	  gimple *stmt = gsi_stmt (bsi);
	  if (is_gimple_debug (stmt))
	    continue;
	  if (walk_stmt_load_store_addr_ops
	      (stmt, non_ssa_vars, test_nonssa_use, test_nonssa_use,
	       test_nonssa_use))
	    {
	      ok = false;
	      goto done;
	    }
	  if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	    if (test_nonssa_use (stmt, gimple_label_label (label_stmt),
				 NULL_TREE, non_ssa_vars))
	      {
		ok = false;
		goto done;
	      }
	}
      for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
	   gsi_next (&bsi))
	{
	  if (walk_stmt_load_store_addr_ops
	      (gsi_stmt (bsi), non_ssa_vars, test_nonssa_use, test_nonssa_use,
	       test_nonssa_use))
	    {
	      ok = false;
	      goto done;
	    }
	}
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->dest != return_bb)
	    continue;
	  for (gphi_iterator bsi = gsi_start_phis (return_bb);
	       !gsi_end_p (bsi);
	       gsi_next (&bsi))
	    {
	      gphi *stmt = bsi.phi ();
	      tree op = gimple_phi_arg_def (stmt, e->dest_idx);

	      if (virtual_operand_p (gimple_phi_result (stmt)))
		continue;
	      if (TREE_CODE (op) != SSA_NAME
		  && test_nonssa_use (stmt, op, op, non_ssa_vars))
		{
		  ok = false;
		  goto done;
		}
	    }
	}
    }

  /* Verify that the rest of function does not define any label
     used by the split part.  */
  FOR_EACH_BB_FN (bb, cfun)
    if (!bitmap_bit_p (current->split_bbs, bb->index)
	&& !bitmap_bit_p (seen, bb->index))
      {
        gimple_stmt_iterator bsi;
        for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	  if (glabel *label_stmt = dyn_cast <glabel *> (gsi_stmt (bsi)))
	    {
	      if (test_nonssa_use (label_stmt,
				   gimple_label_label (label_stmt),
				   NULL_TREE, non_ssa_vars))
		{
		  ok = false;
		  goto done;
		}
	    }
	  else
	    break;
      }
    
done:
  BITMAP_FREE (seen);
  worklist.release ();
  return ok;
}

/* If STMT is a call, check the callee against a list of forbidden
   predicate functions.  If a match is found, look for uses of the
   call result in condition statements that compare against zero.
   For each such use, find the block targeted by the condition
   statement for the nonzero result, and set the bit for this block
   in the forbidden dominators bitmap.  The purpose of this is to avoid
   selecting a split point where we are likely to lose the chance
   to optimize away an unused function call.  */

static void
check_forbidden_calls (gimple *stmt)
{
  imm_use_iterator use_iter;
  use_operand_p use_p;
  tree lhs;

  /* At the moment, __builtin_constant_p is the only forbidden
     predicate function call (see PR49642).  */
  if (!gimple_call_builtin_p (stmt, BUILT_IN_CONSTANT_P))
    return;

  lhs = gimple_call_lhs (stmt);

  if (!lhs || TREE_CODE (lhs) != SSA_NAME)
    return;

  FOR_EACH_IMM_USE_FAST (use_p, use_iter, lhs)
    {
      tree op1;
      basic_block use_bb, forbidden_bb;
      enum tree_code code;
      edge true_edge, false_edge;
      gcond *use_stmt;

      use_stmt = dyn_cast <gcond *> (USE_STMT (use_p));
      if (!use_stmt)
	continue;

      /* Assuming canonical form for GIMPLE_COND here, with constant
	 in second position.  */
      op1 = gimple_cond_rhs (use_stmt);
      code = gimple_cond_code (use_stmt);
      use_bb = gimple_bb (use_stmt);

      extract_true_false_edges_from_block (use_bb, &true_edge, &false_edge);

      /* We're only interested in comparisons that distinguish
	 unambiguously from zero.  */
      if (!integer_zerop (op1) || code == LE_EXPR || code == GE_EXPR)
	continue;

      if (code == EQ_EXPR)
	forbidden_bb = false_edge->dest;
      else
	forbidden_bb = true_edge->dest;

      bitmap_set_bit (forbidden_dominators, forbidden_bb->index);
    }
}

/* If BB is dominated by any block in the forbidden dominators set,
   return TRUE; else FALSE.  */

static bool
dominated_by_forbidden (basic_block bb)
{
  unsigned dom_bb;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (forbidden_dominators, 1, dom_bb, bi)
    {
      if (dominated_by_p (CDI_DOMINATORS, bb,
			  BASIC_BLOCK_FOR_FN (cfun, dom_bb)))
	return true;
    }

  return false;
}

/* For give split point CURRENT and return block RETURN_BB return 1
   if ssa name VAL is set by split part and 0 otherwise.  */
static bool
split_part_set_ssa_name_p (tree val, struct split_point *current,
			   basic_block return_bb)
{
  if (TREE_CODE (val) != SSA_NAME)
    return false;

  return (!SSA_NAME_IS_DEFAULT_DEF (val)
	  && (bitmap_bit_p (current->split_bbs,
			    gimple_bb (SSA_NAME_DEF_STMT (val))->index)
	      || gimple_bb (SSA_NAME_DEF_STMT (val)) == return_bb));
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
  gphi_iterator bsi;
  unsigned int i;
  tree retval;
  bool back_edge = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_split_point (dump_file, current);

  current->count = profile_count::zero ();
  FOR_EACH_EDGE (e, ei, current->entry_bb->preds)
    {
      if (e->flags & EDGE_DFS_BACK)
	back_edge = true;
      if (!bitmap_bit_p (current->split_bbs, e->src->index))
	current->count += e->count ();
    }

  /* Do not split when we would end up calling function anyway.
     Compares are three state, use !(...<...) to also give up when outcome
     is unknown.  */
  if (!(current->count
       < (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.apply_scale
	   (PARAM_VALUE (PARAM_PARTIAL_INLINING_ENTRY_PROBABILITY), 100))))
    {
      /* When profile is guessed, we can not expect it to give us
	 realistic estimate on likelyness of function taking the
	 complex path.  As a special case, when tail of the function is
	 a loop, enable splitting since inlining code skipping the loop
	 is likely noticeable win.  */
      if (back_edge
	  && profile_status_for_fn (cfun) != PROFILE_READ
	  && current->count
		 < ENTRY_BLOCK_PTR_FOR_FN (cfun)->count)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "  Split before loop, accepting despite low counts");
	      current->count.dump (dump_file);
	      fprintf (dump_file, " ");
	      ENTRY_BLOCK_PTR_FOR_FN (cfun)->count.dump (dump_file);
	    }
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "  Refused: incoming frequency is too large.\n");
	  return;
	}
    }

  if (!current->header_size)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  Refused: header empty\n");
      return;
    }

  /* Verify that PHI args on entry are either virtual or all their operands
     incoming from header are the same.  */
  for (bsi = gsi_start_phis (current->entry_bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      gphi *stmt = bsi.phi ();
      tree val = NULL;

      if (virtual_operand_p (gimple_phi_result (stmt)))
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
       parm = DECL_CHAIN (parm))
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
      else
	{
	  tree ddef = ssa_default_def (cfun, parm);
	  if (ddef
	      && bitmap_bit_p (current->ssa_names_to_pass,
			       SSA_NAME_VERSION (ddef)))
	    {
	      if (!VOID_TYPE_P (TREE_TYPE (parm)))
		call_overhead += estimate_move_cost (TREE_TYPE (parm), false);
	      num_args++;
	    }
	}
    }
  if (!VOID_TYPE_P (TREE_TYPE (current_function_decl)))
    call_overhead += estimate_move_cost (TREE_TYPE (current_function_decl),
					 false);

  if (current->split_size <= call_overhead)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: split size is smaller than call overhead\n");
      return;
    }
  /* FIXME: The logic here is not very precise, because inliner does use
     inline predicates to reduce function body size.  We add 10 to anticipate
     that.  Next stage1 we should try to be more meaningful here.  */
  if (current->header_size + call_overhead
      >= (unsigned int)(DECL_DECLARED_INLINE_P (current_function_decl)
			? MAX_INLINE_INSNS_SINGLE
			: MAX_INLINE_INSNS_AUTO) + 10)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: header size is too large for inline candidate\n");
      return;
    }

  /* Splitting functions brings the target out of comdat group; this will
     lead to code duplication if the function is reused by other unit.
     Limit this duplication.  This is consistent with limit in tree-sra.c  
     FIXME: with LTO we ought to be able to do better!  */
  if (DECL_ONE_ONLY (current_function_decl)
      && current->split_size >= (unsigned int) MAX_INLINE_INSNS_AUTO + 10)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: function is COMDAT and tail is too large\n");
      return;
    }
  /* For comdat functions also reject very small tails; those will likely get
     inlined back and we do not want to risk the duplication overhead.
     FIXME: with LTO we ought to be able to do better!  */
  if (DECL_ONE_ONLY (current_function_decl)
      && current->split_size
	 <= (unsigned int) PARAM_VALUE (PARAM_EARLY_INLINING_INSNS) / 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: function is COMDAT and tail is too small\n");
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
  if (!bitmap_empty_p (non_ssa_vars)
      && !verify_non_ssa_vars (current, non_ssa_vars, return_bb))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: split part has non-ssa uses\n");
      return;
    }

  /* If the split point is dominated by a forbidden block, reject
     the split.  */
  if (!bitmap_empty_p (forbidden_dominators)
      && dominated_by_forbidden (current->entry_bb))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "  Refused: split point dominated by forbidden block\n");
      return;
    }

  /* See if retval used by return bb is computed by header or split part.
     When it is computed by split part, we need to produce return statement
     in the split part and add code to header to pass it around.

     This is bit tricky to test:
       1) When there is no return_bb or no return value, we always pass
          value around.
       2) Invariants are always computed by caller.
       3) For SSA we need to look if defining statement is in header or split part
       4) For non-SSA we need to look where the var is computed. */
  retval = find_retval (return_bb);
  if (!retval)
    {
      /* If there is a return_bb with no return value in function returning
	 value by reference, also make the split part return void, otherwise
	 we expansion would try to create a non-POD temporary, which is
	 invalid.  */
      if (return_bb != EXIT_BLOCK_PTR_FOR_FN (cfun)
	  && DECL_RESULT (current_function_decl)
	  && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
	current->split_part_set_retval = false;
      else
	current->split_part_set_retval = true;
    }
  else if (is_gimple_min_invariant (retval))
    current->split_part_set_retval = false;
  /* Special case is value returned by reference we record as if it was non-ssa
     set to result_decl.  */
  else if (TREE_CODE (retval) == SSA_NAME
	   && SSA_NAME_VAR (retval)
	   && TREE_CODE (SSA_NAME_VAR (retval)) == RESULT_DECL
	   && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
    current->split_part_set_retval
       = bitmap_bit_p (non_ssa_vars, DECL_UID (SSA_NAME_VAR (retval)));
  else if (TREE_CODE (retval) == SSA_NAME)
    current->split_part_set_retval
      = split_part_set_ssa_name_p (retval, current, return_bb);
  else if (TREE_CODE (retval) == PARM_DECL)
    current->split_part_set_retval = false;
  else if (VAR_P (retval)
	   || TREE_CODE (retval) == RESULT_DECL)
    current->split_part_set_retval
      = bitmap_bit_p (non_ssa_vars, DECL_UID (retval));
  else
    current->split_part_set_retval = true;

  /* split_function fixes up at most one PHI non-virtual PHI node in return_bb,
     for the return value.  If there are other PHIs, give up.  */
  if (return_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      gphi_iterator psi;

      for (psi = gsi_start_phis (return_bb); !gsi_end_p (psi); gsi_next (&psi))
	if (!virtual_operand_p (gimple_phi_result (psi.phi ()))
	    && !(retval
		 && current->split_part_set_retval
		 && TREE_CODE (retval) == SSA_NAME
		 && !DECL_BY_REFERENCE (DECL_RESULT (current_function_decl))
		 && SSA_NAME_DEF_STMT (retval) == psi.phi ()))
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file,
		       "  Refused: return bb has extra PHIs\n");
	    return;
	  }
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  Accepted!\n");

  /* At the moment chose split point with lowest count and that leaves
     out smallest size of header.
     In future we might re-consider this heuristics.  */
  if (!best_split_point.split_bbs
      || best_split_point.count
	 > current->count
      || (best_split_point.count == current->count 
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

/* Return basic block containing RETURN statement.  We allow basic blocks
   of the form:
   <retval> = tmp_var;
   return <retval>
   but return_bb can not be more complex than this (except for
   -fsanitize=thread we allow TSAN_FUNC_EXIT () internal call in there).
   If nothing is found, return the exit block.

   When there are multiple RETURN statement, chose one with return value,
   since that one is more likely shared by multiple code paths.

   Return BB is special, because for function splitting it is the only
   basic block that is duplicated in between header and split part of the
   function.

   TODO: We might support multiple return blocks.  */

static basic_block
find_return_bb (void)
{
  edge e;
  basic_block return_bb = EXIT_BLOCK_PTR_FOR_FN (cfun);
  gimple_stmt_iterator bsi;
  bool found_return = false;
  tree retval = NULL_TREE;

  if (!single_pred_p (EXIT_BLOCK_PTR_FOR_FN (cfun)))
    return return_bb;

  e = single_pred_edge (EXIT_BLOCK_PTR_FOR_FN (cfun));
  for (bsi = gsi_last_bb (e->src); !gsi_end_p (bsi); gsi_prev (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);
      if (gimple_code (stmt) == GIMPLE_LABEL
	  || is_gimple_debug (stmt)
	  || gimple_clobber_p (stmt))
	;
      else if (gimple_code (stmt) == GIMPLE_ASSIGN
	       && found_return
	       && gimple_assign_single_p (stmt)
	       && (auto_var_in_fn_p (gimple_assign_rhs1 (stmt),
				     current_function_decl)
		   || is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
	       && retval == gimple_assign_lhs (stmt))
	;
      else if (greturn *return_stmt = dyn_cast <greturn *> (stmt))
	{
	  found_return = true;
	  retval = gimple_return_retval (return_stmt);
	}
      /* For -fsanitize=thread, allow also TSAN_FUNC_EXIT () in the return
	 bb.  */
      else if ((flag_sanitize & SANITIZE_THREAD)
	       && gimple_call_internal_p (stmt, IFN_TSAN_FUNC_EXIT))
	;
      else
	break;
    }
  if (gsi_end_p (bsi) && found_return)
    return_bb = e->src;

  return return_bb;
}

/* Given return basic block RETURN_BB, see where return value is really
   stored.  */
static tree
find_retval (basic_block return_bb)
{
  gimple_stmt_iterator bsi;
  for (bsi = gsi_start_bb (return_bb); !gsi_end_p (bsi); gsi_next (&bsi))
    if (greturn *return_stmt = dyn_cast <greturn *> (gsi_stmt (bsi)))
      return gimple_return_retval (return_stmt);
    else if (gimple_code (gsi_stmt (bsi)) == GIMPLE_ASSIGN
	     && !gimple_clobber_p (gsi_stmt (bsi)))
      return gimple_assign_rhs1 (gsi_stmt (bsi));
  return NULL;
}

/* Callback for walk_stmt_load_store_addr_ops.  If T is non-SSA automatic
   variable, mark it as used in bitmap passed via DATA.
   Return true when access to T prevents splitting the function.  */

static bool
mark_nonssa_use (gimple *, tree t, tree, void *data)
{
  t = get_base_address (t);

  if (!t || is_gimple_reg (t))
    return false;

  /* At present we can't pass non-SSA arguments to split function.
     FIXME: this can be relaxed by passing references to arguments.  */
  if (TREE_CODE (t) == PARM_DECL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Cannot split: use of non-ssa function parameter.\n");
      return true;
    }

  if ((VAR_P (t) && auto_var_in_fn_p (t, current_function_decl))
      || TREE_CODE (t) == RESULT_DECL
      || (TREE_CODE (t) == LABEL_DECL && FORCED_LABEL (t)))
    bitmap_set_bit ((bitmap)data, DECL_UID (t));

  /* For DECL_BY_REFERENCE, the return value is actually a pointer.  We want
     to pretend that the value pointed to is actual result decl.  */
  if ((TREE_CODE (t) == MEM_REF || INDIRECT_REF_P (t))
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
      && SSA_NAME_VAR (TREE_OPERAND (t, 0))
      && TREE_CODE (SSA_NAME_VAR (TREE_OPERAND (t, 0))) == RESULT_DECL
      && DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
    return
      bitmap_bit_p ((bitmap)data,
		    DECL_UID (DECL_RESULT (current_function_decl)));

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
  edge e;
  edge_iterator ei;
  bool can_split = true;

  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    {
      gimple *stmt = gsi_stmt (bsi);
      tree op;
      ssa_op_iter iter;
      tree decl;

      if (is_gimple_debug (stmt))
	continue;

      if (gimple_clobber_p (stmt))
	continue;

      /* FIXME: We can split regions containing EH.  We can not however
	 split RESX, EH_DISPATCH and EH_POINTER referring to same region
	 into different partitions.  This would require tracking of
	 EH regions and checking in consider_split_point if they 
	 are not used elsewhere.  */
      if (gimple_code (stmt) == GIMPLE_RESX)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Cannot split: resx.\n");
	  can_split = false;
	}
      if (gimple_code (stmt) == GIMPLE_EH_DISPATCH)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Cannot split: eh dispatch.\n");
	  can_split = false;
	}

      /* Check builtins that prevent splitting.  */
      if (gimple_code (stmt) == GIMPLE_CALL
	  && (decl = gimple_call_fndecl (stmt)) != NULL_TREE
	  && fndecl_built_in_p (decl, BUILT_IN_NORMAL))
	switch (DECL_FUNCTION_CODE (decl))
	  {
	  /* FIXME: once we will allow passing non-parm values to split part,
	     we need to be sure to handle correct builtin_stack_save and
	     builtin_stack_restore.  At the moment we are safe; there is no
	     way to store builtin_stack_save result in non-SSA variable
	     since all calls to those are compiler generated.  */
	  case BUILT_IN_APPLY:
	  case BUILT_IN_APPLY_ARGS:
	  case BUILT_IN_VA_START:
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file,
		       "Cannot split: builtin_apply and va_start.\n");
	    can_split = false;
	    break;
	  case BUILT_IN_EH_POINTER:
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Cannot split: builtin_eh_pointer.\n");
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
  for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi);
       gsi_next (&bsi))
    {
      gphi *stmt = bsi.phi ();
      unsigned int i;

      if (virtual_operand_p (gimple_phi_result (stmt)))
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
  /* Record also uses coming from PHI operand in return BB.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->dest == return_bb)
      {
	for (gphi_iterator bsi = gsi_start_phis (return_bb);
	     !gsi_end_p (bsi);
	     gsi_next (&bsi))
	  {
	    gphi *stmt = bsi.phi ();
	    tree op = gimple_phi_arg_def (stmt, e->dest_idx);

	    if (virtual_operand_p (gimple_phi_result (stmt)))
	      continue;
	    if (TREE_CODE (op) == SSA_NAME)
	      bitmap_set_bit (used_ssa_names, SSA_NAME_VERSION (op));
	    else
	      can_split &= !mark_nonssa_use (stmt, op, op, non_ssa_vars);
	  }
      }
  return can_split;
}

/* Stack entry for recursive DFS walk in find_split_point.  */

struct stack_entry
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
     the value is up to sum of incoming and outgoing edges of BB.  */
  unsigned int edge_num;

  /* Stack entry index of earliest BB reachable from current BB
     or any BB visited later in DFS walk.  */
  int earliest;

  /* Overall time and size of all BBs reached from this BB in DFS walk.  */
  sreal overall_time;
  int overall_size;

  /* When false we can not split on this BB.  */
  bool can_split;
};


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
find_split_points (basic_block return_bb, sreal overall_time, int overall_size)
{
  stack_entry first;
  vec<stack_entry> stack = vNULL;
  basic_block bb;
  struct split_point current;

  current.header_time = overall_time;
  current.header_size = overall_size;
  current.split_time = 0;
  current.split_size = 0;
  current.ssa_names_to_pass = BITMAP_ALLOC (NULL);

  first.bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  first.edge_num = 0;
  first.overall_time = 0;
  first.overall_size = 0;
  first.earliest = INT_MAX;
  first.set_ssa_names = 0;
  first.used_ssa_names = 0;
  first.non_ssa_vars = 0;
  first.bbs_visited = 0;
  first.can_split = false;
  stack.safe_push (first);
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->aux = (void *)(intptr_t)-1;

  while (!stack.is_empty ())
    {
      stack_entry *entry = &stack.last ();

      /* We are walking an acyclic graph, so edge_num counts
	 succ and pred edges together.  However when considering
         articulation, we want to have processed everything reachable
	 from articulation but nothing that reaches into it.  */
      if (entry->edge_num == EDGE_COUNT (entry->bb->succs)
	  && entry->bb != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  int pos = stack.length ();
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
	  if (dest != return_bb && dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	      && !dest->aux)
	    {
	      stack_entry new_entry;

	      new_entry.bb = dest;
	      new_entry.edge_num = 0;
	      new_entry.overall_time
		 = bb_info_vec[dest->index].time;
	      new_entry.overall_size
		 = bb_info_vec[dest->index].size;
	      new_entry.earliest = INT_MAX;
	      new_entry.set_ssa_names = BITMAP_ALLOC (NULL);
	      new_entry.used_ssa_names = BITMAP_ALLOC (NULL);
	      new_entry.bbs_visited = BITMAP_ALLOC (NULL);
	      new_entry.non_ssa_vars = BITMAP_ALLOC (NULL);
	      new_entry.can_split = true;
	      bitmap_set_bit (new_entry.bbs_visited, dest->index);
	      stack.safe_push (new_entry);
	      dest->aux = (void *)(intptr_t)stack.length ();
	    }
	  /* Back edge found, record the earliest point.  */
	  else if ((intptr_t)dest->aux > 0
		   && (intptr_t)dest->aux < entry->earliest)
	    entry->earliest = (intptr_t)dest->aux;
	}
      /* We are done with examining the edges.  Pop off the value from stack
	 and merge stuff we accumulate during the walk.  */
      else if (entry->bb != ENTRY_BLOCK_PTR_FOR_FN (cfun))
	{
	  stack_entry *prev = &stack[stack.length () - 2];

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
	  stack.pop ();
	}
      else
        stack.pop ();
    }
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->aux = NULL;
  FOR_EACH_BB_FN (bb, cfun)
    bb->aux = NULL;
  stack.release ();
  BITMAP_FREE (current.ssa_names_to_pass);
}

/* Split function at SPLIT_POINT.  */

static void
split_function (basic_block return_bb, struct split_point *split_point,
		bool add_tsan_func_exit)
{
  vec<tree> args_to_pass = vNULL;
  bitmap args_to_skip;
  tree parm;
  int num = 0;
  cgraph_node *node, *cur_node = cgraph_node::get (current_function_decl);
  basic_block call_bb;
  gcall *call, *tsan_func_exit_call = NULL;
  edge e;
  edge_iterator ei;
  tree retval = NULL, real_retval = NULL;
  gimple *last_stmt = NULL;
  unsigned int i;
  tree arg, ddef;

  if (dump_file)
    {
      fprintf (dump_file, "\n\nSplitting function at:\n");
      dump_split_point (dump_file, split_point);
    }

  if (cur_node->local.can_change_signature)
    args_to_skip = BITMAP_ALLOC (NULL);
  else
    args_to_skip = NULL;

  /* Collect the parameters of new function and args_to_skip bitmap.  */
  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm; parm = DECL_CHAIN (parm), num++)
    if (args_to_skip
	&& (!is_gimple_reg (parm)
	    || (ddef = ssa_default_def (cfun, parm)) == NULL_TREE
	    || !bitmap_bit_p (split_point->ssa_names_to_pass,
			      SSA_NAME_VERSION (ddef))))
      bitmap_set_bit (args_to_skip, num);
    else
      {
	/* This parm might not have been used up to now, but is going to be
	   used, hence register it.  */
	if (is_gimple_reg (parm))
	  arg = get_or_create_ssa_default_def (cfun, parm);
	else
	  arg = parm;

	if (!useless_type_conversion_p (DECL_ARG_TYPE (parm), TREE_TYPE (arg)))
	  arg = fold_convert (DECL_ARG_TYPE (parm), arg);
	args_to_pass.safe_push (arg);
      }

  /* See if the split function will return.  */
  bool split_part_return_p = false;
  FOR_EACH_EDGE (e, ei, return_bb->preds)
    {
      if (bitmap_bit_p (split_point->split_bbs, e->src->index))
	split_part_return_p = true;
    }

  /* Add return block to what will become the split function.
     We do not return; no return block is needed.  */
  if (!split_part_return_p)
    ;
  /* We have no return block, so nothing is needed.  */
  else if (return_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    ;
  /* When we do not want to return value, we need to construct
     new return block with empty return statement.
     FIXME: Once we are able to change return type, we should change function
     to return void instead of just outputting function with undefined return
     value.  For structures this affects quality of codegen.  */
  else if ((retval = find_retval (return_bb))
	   && !split_point->split_part_set_retval)
    {
      bool redirected = true;
      basic_block new_return_bb = create_basic_block (NULL, 0, return_bb);
      gimple_stmt_iterator gsi = gsi_start_bb (new_return_bb);
      gsi_insert_after (&gsi, gimple_build_return (NULL), GSI_NEW_STMT);
      new_return_bb->count = profile_count::zero ();
      while (redirected)
	{
	  redirected = false;
	  FOR_EACH_EDGE (e, ei, return_bb->preds)
	    if (bitmap_bit_p (split_point->split_bbs, e->src->index))
	      {
		new_return_bb->count += e->count ();
		redirect_edge_and_branch (e, new_return_bb);
		redirected = true;
		break;
	      }
	}
      e = make_single_succ_edge (new_return_bb, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
      add_bb_to_loop (new_return_bb, current_loops->tree_root);
      bitmap_set_bit (split_point->split_bbs, new_return_bb->index);
    }
  /* When we pass around the value, use existing return block.  */
  else
    bitmap_set_bit (split_point->split_bbs, return_bb->index);

  /* If RETURN_BB has virtual operand PHIs, they must be removed and the
     virtual operand marked for renaming as we change the CFG in a way that
     tree-inline is not able to compensate for.

     Note this can happen whether or not we have a return value.  If we have
     a return value, then RETURN_BB may have PHIs for real operands too.  */
  if (return_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      bool phi_p = false;
      for (gphi_iterator gsi = gsi_start_phis (return_bb);
	   !gsi_end_p (gsi);)
	{
	  gphi *stmt = gsi.phi ();
	  if (!virtual_operand_p (gimple_phi_result (stmt)))
	    {
	      gsi_next (&gsi);
	      continue;
	    }
	  mark_virtual_phi_result_for_renaming (stmt);
	  remove_phi_node (&gsi, true);
	  phi_p = true;
	}
      /* In reality we have to rename the reaching definition of the
	 virtual operand at return_bb as we will eventually release it
	 when we remove the code region we outlined.
	 So we have to rename all immediate virtual uses of that region
	 if we didn't see a PHI definition yet.  */
      /* ???  In real reality we want to set the reaching vdef of the
         entry of the SESE region as the vuse of the call and the reaching
	 vdef of the exit of the SESE region as the vdef of the call.  */
      if (!phi_p)
	for (gimple_stmt_iterator gsi = gsi_start_bb (return_bb);
	     !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  {
	    gimple *stmt = gsi_stmt (gsi);
	    if (gimple_vuse (stmt))
	      {
		gimple_set_vuse (stmt, NULL_TREE);
		update_stmt (stmt);
	      }
	    if (gimple_vdef (stmt))
	      break;
	  }
    }

  /* Now create the actual clone.  */
  cgraph_edge::rebuild_edges ();
  node = cur_node->create_version_clone_with_body
    (vNULL, NULL, args_to_skip,
     !split_part_return_p || !split_point->split_part_set_retval,
     split_point->split_bbs, split_point->entry_bb, "part");

  node->split_part = true;

  if (cur_node->same_comdat_group)
    {
      /* TODO: call is versionable if we make sure that all
	 callers are inside of a comdat group.  */
      cur_node->calls_comdat_local = 1;
      node->add_to_same_comdat_group (cur_node);
    }


  /* Let's take a time profile for splitted function.  */
  node->tp_first_run = cur_node->tp_first_run + 1;

  /* For usual cloning it is enough to clear builtin only when signature
     changes.  For partial inlining we however can not expect the part
     of builtin implementation to have same semantic as the whole.  */
  if (fndecl_built_in_p (node->decl))
    {
      DECL_BUILT_IN_CLASS (node->decl) = NOT_BUILT_IN;
      DECL_FUNCTION_CODE (node->decl) = (enum built_in_function) 0;
    }

  /* If return_bb contains any clobbers that refer to SSA_NAMEs
     set in the split part, remove them.  Also reset debug stmts that
     refer to SSA_NAMEs set in the split part.  */
  if (return_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      gimple_stmt_iterator gsi = gsi_start_bb (return_bb);
      while (!gsi_end_p (gsi))
	{
	  tree op;
	  ssa_op_iter iter;
	  gimple *stmt = gsi_stmt (gsi);
	  bool remove = false;
	  if (gimple_clobber_p (stmt) || is_gimple_debug (stmt))
	    FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
	      {
		basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (op));
		if (op != retval
		    && bb
		    && bb != return_bb
		    && bitmap_bit_p (split_point->split_bbs, bb->index))
		  {
		    if (is_gimple_debug (stmt))
		      {
			gimple_debug_bind_reset_value (stmt);
			update_stmt (stmt);
		      }
		    else
		      remove = true;
		    break;
		  }
	      }
	  if (remove)
	    gsi_remove (&gsi, true);
	  else
	    gsi_next (&gsi);
	}
    }

  /* If the original function is declared inline, there is no point in issuing
     a warning for the non-inlinable part.  */
  DECL_NO_INLINE_WARNING_P (node->decl) = 1;
  cur_node->remove_callees ();
  cur_node->remove_all_references ();
  if (!split_part_return_p)
    TREE_THIS_VOLATILE (node->decl) = 1;
  if (dump_file)
    dump_function_to_file (node->decl, dump_file, dump_flags);

  /* Create the basic block we place call into.  It is the entry basic block
     split after last label.  */
  call_bb = split_point->entry_bb;
  for (gimple_stmt_iterator gsi = gsi_start_bb (call_bb); !gsi_end_p (gsi);)
    if (gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL)
      {
	last_stmt = gsi_stmt (gsi);
	gsi_next (&gsi);
      }
    else
      break;
  call_bb->count = split_point->count;
  e = split_block (split_point->entry_bb, last_stmt);
  remove_edge (e);

  /* Produce the call statement.  */
  gimple_stmt_iterator gsi = gsi_last_bb (call_bb);
  FOR_EACH_VEC_ELT (args_to_pass, i, arg)
    if (!is_gimple_val (arg))
      {
	arg = force_gimple_operand_gsi (&gsi, arg, true, NULL_TREE,
					false, GSI_CONTINUE_LINKING);
	args_to_pass[i] = arg;
      }
  call = gimple_build_call_vec (node->decl, args_to_pass);
  gimple_set_block (call, DECL_INITIAL (current_function_decl));
  args_to_pass.release ();

  /* For optimized away parameters, add on the caller side
     before the call
     DEBUG D#X => parm_Y(D)
     stmts and associate D#X with parm in decl_debug_args_lookup
     vector to say for debug info that if parameter parm had been passed,
     it would have value parm_Y(D).  */
  if (args_to_skip)
    {
      vec<tree, va_gc> **debug_args = NULL;
      unsigned i = 0, len = 0;
      if (MAY_HAVE_DEBUG_BIND_STMTS)
	{
	  debug_args = decl_debug_args_lookup (node->decl);
	  if (debug_args)
	    len = vec_safe_length (*debug_args);
	}
      for (parm = DECL_ARGUMENTS (current_function_decl), num = 0;
	   parm; parm = DECL_CHAIN (parm), num++)
	if (bitmap_bit_p (args_to_skip, num) && is_gimple_reg (parm))
	  {
	    tree ddecl;
	    gimple *def_temp;

	    /* This needs to be done even without
	       MAY_HAVE_DEBUG_BIND_STMTS, otherwise if it didn't exist
	       before, we'd end up with different SSA_NAME_VERSIONs
	       between -g and -g0.  */
	    arg = get_or_create_ssa_default_def (cfun, parm);
	    if (!MAY_HAVE_DEBUG_BIND_STMTS || debug_args == NULL)
	      continue;

	    while (i < len && (**debug_args)[i] != DECL_ORIGIN (parm))
	      i += 2;
	    if (i >= len)
	      continue;
	    ddecl = (**debug_args)[i + 1];
	    def_temp
	      = gimple_build_debug_bind (ddecl, unshare_expr (arg), call);
	    gsi_insert_after (&gsi, def_temp, GSI_NEW_STMT);
	  }
    }

  /* We avoid address being taken on any variable used by split part,
     so return slot optimization is always possible.  Moreover this is
     required to make DECL_BY_REFERENCE work.  */
  if (aggregate_value_p (DECL_RESULT (current_function_decl),
			 TREE_TYPE (current_function_decl))
      && (!is_gimple_reg_type (TREE_TYPE (DECL_RESULT (current_function_decl)))
	  || DECL_BY_REFERENCE (DECL_RESULT (current_function_decl))))
    gimple_call_set_return_slot_opt (call, true);

  if (add_tsan_func_exit)
    tsan_func_exit_call = gimple_build_call_internal (IFN_TSAN_FUNC_EXIT, 0);

  /* Update return value.  This is bit tricky.  When we do not return,
     do nothing.  When we return we might need to update return_bb
     or produce a new return statement.  */
  if (!split_part_return_p)
    {
      gsi_insert_after (&gsi, call, GSI_NEW_STMT);
      if (tsan_func_exit_call)
	gsi_insert_after (&gsi, tsan_func_exit_call, GSI_NEW_STMT);
    }
  else
    {
      e = make_single_succ_edge (call_bb, return_bb,
				 return_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
				 ? 0 : EDGE_FALLTHRU);

      /* If there is return basic block, see what value we need to store
         return value into and put call just before it.  */
      if (return_bb != EXIT_BLOCK_PTR_FOR_FN (cfun))
	{
	  real_retval = retval;
	  if (real_retval && split_point->split_part_set_retval)
	    {
	      gphi_iterator psi;

	      /* See if we need new SSA_NAME for the result.
		 When DECL_BY_REFERENCE is true, retval is actually pointer to
		 return value and it is constant in whole function.  */
	      if (TREE_CODE (retval) == SSA_NAME
		  && !DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
		{
		  retval = copy_ssa_name (retval, call);

		  /* See if there is PHI defining return value.  */
		  for (psi = gsi_start_phis (return_bb);
		       !gsi_end_p (psi); gsi_next (&psi))
		    if (!virtual_operand_p (gimple_phi_result (psi.phi ())))
		      break;

		  /* When there is PHI, just update its value.  */
		  if (TREE_CODE (retval) == SSA_NAME
		      && !gsi_end_p (psi))
		    add_phi_arg (psi.phi (), retval, e, UNKNOWN_LOCATION);
		  /* Otherwise update the return BB itself.
		     find_return_bb allows at most one assignment to return value,
		     so update first statement.  */
		  else
		    {
		      gimple_stmt_iterator bsi;
		      for (bsi = gsi_start_bb (return_bb); !gsi_end_p (bsi);
			   gsi_next (&bsi))
			if (greturn *return_stmt
			      = dyn_cast <greturn *> (gsi_stmt (bsi)))
			  {
			    gimple_return_set_retval (return_stmt, retval);
			    break;
			  }
			else if (gimple_code (gsi_stmt (bsi)) == GIMPLE_ASSIGN
				 && !gimple_clobber_p (gsi_stmt (bsi)))
			  {
			    gimple_assign_set_rhs1 (gsi_stmt (bsi), retval);
			    break;
			  }
		      update_stmt (gsi_stmt (bsi));
		      /* Also adjust clobbers and debug stmts in return_bb.  */
		      for (bsi = gsi_start_bb (return_bb); !gsi_end_p (bsi);
			   gsi_next (&bsi))
			{
			  gimple *stmt = gsi_stmt (bsi);
			  if (gimple_clobber_p (stmt)
			      || is_gimple_debug (stmt))
			    {
			      ssa_op_iter iter;
			      use_operand_p use_p;
			      bool update = false;
			      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter,
							SSA_OP_USE)
				if (USE_FROM_PTR (use_p) == real_retval)
				  {
				    SET_USE (use_p, retval);
				    update = true;
				  }
			      if (update)
				update_stmt (stmt);
			    }
			}
		    }
		}
	      if (DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
		{
		  gimple_call_set_lhs (call, build_simple_mem_ref (retval));
		  gsi_insert_after (&gsi, call, GSI_NEW_STMT);
		}
	      else
		{
		  tree restype;
		  restype = TREE_TYPE (DECL_RESULT (current_function_decl));
		  gsi_insert_after (&gsi, call, GSI_NEW_STMT);
		  if (!useless_type_conversion_p (TREE_TYPE (retval), restype))
		    {
		      gimple *cpy;
		      tree tem = create_tmp_reg (restype);
		      tem = make_ssa_name (tem, call);
		      cpy = gimple_build_assign (retval, NOP_EXPR, tem);
		      gsi_insert_after (&gsi, cpy, GSI_NEW_STMT);
		      retval = tem;
		    }
		  gimple_call_set_lhs (call, retval);
		  update_stmt (call);
		}
	    }
	  else
	    gsi_insert_after (&gsi, call, GSI_NEW_STMT);
	  if (tsan_func_exit_call)
	    gsi_insert_after (&gsi, tsan_func_exit_call, GSI_NEW_STMT);
	}
      /* We don't use return block (there is either no return in function or
	 multiple of them).  So create new basic block with return statement.
	 */
      else
	{
	  greturn *ret;
	  if (split_point->split_part_set_retval
	      && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl))))
	    {
	      retval = DECL_RESULT (current_function_decl);

	      /* We use temporary register to hold value when aggregate_value_p
		 is false.  Similarly for DECL_BY_REFERENCE we must avoid extra
		 copy.  */
	      if (!aggregate_value_p (retval, TREE_TYPE (current_function_decl))
		  && !DECL_BY_REFERENCE (retval))
		retval = create_tmp_reg (TREE_TYPE (retval));
	      if (is_gimple_reg (retval))
		{
		  /* When returning by reference, there is only one SSA name
		     assigned to RESULT_DECL (that is pointer to return value).
		     Look it up or create new one if it is missing.  */
		  if (DECL_BY_REFERENCE (retval))
		    retval = get_or_create_ssa_default_def (cfun, retval);
		  /* Otherwise produce new SSA name for return value.  */
		  else
		    retval = make_ssa_name (retval, call);
		}
	      if (DECL_BY_REFERENCE (DECL_RESULT (current_function_decl)))
	        gimple_call_set_lhs (call, build_simple_mem_ref (retval));
	      else
	        gimple_call_set_lhs (call, retval);
	      gsi_insert_after (&gsi, call, GSI_NEW_STMT);
	    }
	  else
	    {
	      gsi_insert_after (&gsi, call, GSI_NEW_STMT);
	      if (retval
		  && is_gimple_reg_type (TREE_TYPE (retval))
		  && !is_gimple_val (retval))
		{
		  gassign *g
		    = gimple_build_assign (make_ssa_name (TREE_TYPE (retval)),
					   retval);
		  retval = gimple_assign_lhs (g);
		  gsi_insert_after (&gsi, g, GSI_NEW_STMT);
		}
	    }
	  if (tsan_func_exit_call)
	    gsi_insert_after (&gsi, tsan_func_exit_call, GSI_NEW_STMT);
	  ret = gimple_build_return (retval);
	  gsi_insert_after (&gsi, ret, GSI_NEW_STMT);
	}
    }
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
  compute_fn_summary (node, true);
}

/* Execute function splitting pass.  */

static unsigned int
execute_split_functions (void)
{
  gimple_stmt_iterator bsi;
  basic_block bb;
  sreal overall_time = 0;
  int overall_size = 0;
  int todo = 0;
  struct cgraph_node *node = cgraph_node::get (current_function_decl);

  if (flags_from_decl_or_type (current_function_decl)
      & (ECF_NORETURN|ECF_MALLOC))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: noreturn/malloc function.\n");
      return 0;
    }
  if (MAIN_NAME_P (DECL_NAME (current_function_decl)))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: main function.\n");
      return 0;
    }
  if (node->frequency == NODE_FREQUENCY_UNLIKELY_EXECUTED)
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: function is unlikely executed.\n");
      return 0;
    }
  /* This can be relaxed; function might become inlinable after splitting
     away the uninlinable part.  */
  if (ipa_fn_summaries
      && ipa_fn_summaries->get (node)
      && !ipa_fn_summaries->get (node)->inlinable)
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: not inlinable.\n");
      return 0;
    }
  if (DECL_DISREGARD_INLINE_LIMITS (node->decl))
    {
      if (dump_file)
	fprintf (dump_file, "Not splitting: disregarding inline limits.\n");
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

  /* See if it makes sense to try to split.
     It makes sense to split if we inline, that is if we have direct calls to
     handle or direct calls are possibly going to appear as result of indirect
     inlining or LTO.  Also handle -fprofile-generate as LTO to allow non-LTO
     training for LTO -fprofile-use build.

     Note that we are not completely conservative about disqualifying functions
     called once.  It is possible that the caller is called more then once and
     then inlining would still benefit.  */
  if ((!node->callers
       /* Local functions called once will be completely inlined most of time.  */
       || (!node->callers->next_caller && node->local.local))
      && !node->address_taken
      && !node->has_aliases_p ()
      && (!flag_lto || !node->externally_visible))
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

  /* We enforce splitting after loop headers when profile info is not
     available.  */
  if (profile_status_for_fn (cfun) != PROFILE_READ)
    mark_dfs_back_edges ();

  /* Initialize bitmap to track forbidden calls.  */
  forbidden_dominators = BITMAP_ALLOC (NULL);
  calculate_dominance_info (CDI_DOMINATORS);

  /* Compute local info about basic blocks and determine function size/time.  */
  bb_info_vec.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  best_split_point.split_bbs = NULL;
  basic_block return_bb = find_return_bb ();
  int tsan_exit_found = -1;
  FOR_EACH_BB_FN (bb, cfun)
    {
      sreal time = 0;
      int size = 0;
      sreal freq = bb->count.to_sreal_scale
			 (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Basic block %i\n", bb->index);

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  sreal this_time;
	  int this_size;
	  gimple *stmt = gsi_stmt (bsi);

	  this_size = estimate_num_insns (stmt, &eni_size_weights);
	  this_time = (sreal)estimate_num_insns (stmt, &eni_time_weights)
			 * freq;
	  size += this_size;
	  time += this_time;
	  check_forbidden_calls (stmt);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  freq:%4.2f size:%3i time:%4.2f ",
		       freq.to_double (), this_size, this_time.to_double ());
	      print_gimple_stmt (dump_file, stmt, 0);
	    }

	  if ((flag_sanitize & SANITIZE_THREAD)
	      && gimple_call_internal_p (stmt, IFN_TSAN_FUNC_EXIT))
	    {
	      /* We handle TSAN_FUNC_EXIT for splitting either in the
		 return_bb, or in its immediate predecessors.  */
	      if ((bb != return_bb && !find_edge (bb, return_bb))
		  || (tsan_exit_found != -1
		      && tsan_exit_found != (bb != return_bb)))
		{
		  if (dump_file)
		    fprintf (dump_file, "Not splitting: TSAN_FUNC_EXIT"
			     " in unexpected basic block.\n");
		  BITMAP_FREE (forbidden_dominators);
		  bb_info_vec.release ();
		  return 0;
		}
	      tsan_exit_found = bb != return_bb;
	    }
	}
      overall_time += time;
      overall_size += size;
      bb_info_vec[bb->index].time = time;
      bb_info_vec[bb->index].size = size;
    }
  find_split_points (return_bb, overall_time, overall_size);
  if (best_split_point.split_bbs)
    {
      split_function (return_bb, &best_split_point, tsan_exit_found == 1);
      BITMAP_FREE (best_split_point.ssa_names_to_pass);
      BITMAP_FREE (best_split_point.split_bbs);
      todo = TODO_update_ssa | TODO_cleanup_cfg;
    }
  BITMAP_FREE (forbidden_dominators);
  bb_info_vec.release ();
  return todo;
}

namespace {

const pass_data pass_data_split_functions =
{
  GIMPLE_PASS, /* type */
  "fnsplit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_FNSPLIT, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_split_functions : public gimple_opt_pass
{
public:
  pass_split_functions (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_split_functions, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
    {
      return execute_split_functions ();
    }

}; // class pass_split_functions

bool
pass_split_functions::gate (function *)
{
  /* When doing profile feedback, we want to execute the pass after profiling
     is read.  So disable one in early optimization.  */
  return (flag_partial_inlining
	  && !profile_arc_flag && !flag_branch_probabilities);
}

} // anon namespace

gimple_opt_pass *
make_pass_split_functions (gcc::context *ctxt)
{
  return new pass_split_functions (ctxt);
}

/* Execute function splitting pass.  */

static unsigned int
execute_feedback_split_functions (void)
{
  unsigned int retval = execute_split_functions ();
  if (retval)
    retval |= TODO_rebuild_cgraph_edges;
  return retval;
}

namespace {

const pass_data pass_data_feedback_split_functions =
{
  GIMPLE_PASS, /* type */
  "feedback_fnsplit", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_FNSPLIT, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_feedback_split_functions : public gimple_opt_pass
{
public:
  pass_feedback_split_functions (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_feedback_split_functions, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *)
    {
      return execute_feedback_split_functions ();
    }

}; // class pass_feedback_split_functions

bool
pass_feedback_split_functions::gate (function *)
{
  /* We don't need to split when profiling at all, we are producing
     lousy code anyway.  */
  return (flag_partial_inlining
	  && flag_branch_probabilities);
}

} // anon namespace

gimple_opt_pass *
make_pass_feedback_split_functions (gcc::context *ctxt)
{
  return new pass_feedback_split_functions (ctxt);
}
