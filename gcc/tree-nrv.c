/* Language independent return value optimizations
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "function.h"
#include "basic-block.h"
#include "expr.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "langhooks.h"

/* This file implements return value optimizations for functions which
   return aggregate types.

   Basically this pass searches the function for return statements which
   return a local aggregate.  When converted to RTL such statements will
   generate a copy from the local aggregate to final return value destination
   mandated by the target's ABI.

   That copy can often be avoided by directly constructing the return value
   into the final destination mandated by the target's ABI.

   This is basically a generic equivalent to the C++ front-end's 
   Named Return Value optimization.  */

struct nrv_data
{
  /* This is the temporary (a VAR_DECL) which appears in all of
     this function's RETURN_EXPR statements.  */
  tree var;

  /* This is the function's RESULT_DECL.  We will replace all occurrences
     of VAR with RESULT_DECL when we apply this optimization.  */
  tree result;
};

static tree finalize_nrv_r (tree *, int *, void *);

/* Callback for the tree walker.

   If TP refers to a RETURN_EXPR, then set the expression being returned
   to nrv_data->result.

   If TP refers to nrv_data->var, then replace nrv_data->var with
   nrv_data->result.

   If we reach a node where we know all the subtrees are uninteresting,
   then set *WALK_SUBTREES to zero.  */

static tree
finalize_nrv_r (tree *tp, int *walk_subtrees, void *data)
{
  struct nrv_data *dp = (struct nrv_data *)data;

  /* No need to walk into types.  */
  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  /* If this is a RETURN_EXPR, set the expression being returned to RESULT.  */
  else if (TREE_CODE (*tp) == RETURN_EXPR)
    TREE_OPERAND (*tp, 0) = dp->result;

  /* Otherwise replace all occurrences of VAR with RESULT.  */
  else if (*tp == dp->var)
    *tp = dp->result;

  /* Keep iterating.  */
  return NULL_TREE;
}

/* Main entry point for return value optimizations.

   If this function always returns the same local variable, and that
   local variable is an aggregate type, then replace the variable with
   the function's DECL_RESULT.

   This is the equivalent of the C++ named return value optimization
   applied to optimized trees in a language independent form.  If we
   ever encounter languages which prevent this kind of optimization,
   then we could either have the languages register the optimization or
   we could change the gating function to check the current language.  */
   
static void
tree_nrv (void)
{
  tree result = DECL_RESULT (current_function_decl);
  tree result_type = TREE_TYPE (result);
  tree found = NULL;
  basic_block bb;
  struct nrv_data data;

  /* If this function does not return an aggregate type in memory, then
     there is nothing to do.  */
  if (!aggregate_value_p (result, current_function_decl))
    return;

  /* Look through each block for suitable return expressions.   RETURN_EXPRs
     end basic blocks, so we only have to look at the last statement in
     each block.  That makes this very fast.  */
  FOR_EACH_BB (bb)
    {
      tree stmt = last_stmt (bb);

      if (stmt && TREE_CODE (stmt) == RETURN_EXPR)
	{
	  tree ret_expr = TREE_OPERAND (stmt, 0);

	  /* This probably should not happen, but just to be safe do
	     not perform NRV optimizations if only some of the return
	     statement return a value.  */
	  if (!ret_expr
	      || TREE_CODE (ret_expr) != MODIFY_EXPR
	      || TREE_CODE (TREE_OPERAND (ret_expr, 0)) != RESULT_DECL)
	    return;

	  /* Now verify that this return statement uses the same value
	     as any previously encountered return statement.  */
	  if (found != NULL)
	    {
	      /* If we found a return statement using a different variable
		 than previous return statements, then we can not perform
		 NRV optimizations.  */
	      if (found != TREE_OPERAND (ret_expr, 1))
		return;
	    }
	  else
	    found = TREE_OPERAND (ret_expr, 1);

	  /* The returned value must be a local automatic variable of the
	     same type and alignment as the function's result.  */
	  if (TREE_CODE (found) != VAR_DECL
	      || TREE_THIS_VOLATILE (found)
	      || DECL_CONTEXT (found) != current_function_decl
	      || TREE_STATIC (found)
	      || TREE_ADDRESSABLE (found)
	      || DECL_ALIGN (found) > DECL_ALIGN (result)
              || !lang_hooks.types_compatible_p (TREE_TYPE (found), 
 		 result_type))
	    return;
	}
    }

  if (!found)
    return;

  /* If dumping details, then note once and only the NRV replacement.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "NRV Replaced: ");
      print_generic_expr (dump_file, found, dump_flags);
      fprintf (dump_file, "  with: ");
      print_generic_expr (dump_file, result, dump_flags);
      fprintf (dump_file, "\n");
    }

  /* At this point we know that all the return statements return the
     same local which has suitable attributes for NRV.   Copy debugging
     information from FOUND to RESULT.  */
  DECL_NAME (result) = DECL_NAME (found);
  DECL_SOURCE_LOCATION (result) = DECL_SOURCE_LOCATION (found);
  DECL_ABSTRACT_ORIGIN (result) = DECL_ABSTRACT_ORIGIN (found);
  TREE_ADDRESSABLE (result) = TREE_ADDRESSABLE (found);

  /* Now walk through the function changing all references to VAR to be
     RESULT.  */
  data.var = found;
  data.result = result;
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	walk_tree (bsi_stmt_ptr (bsi), finalize_nrv_r, &data, 0);
    }

  /* FOUND is no longer used.  Ensure it gets removed.  */
  var_ann (found)->used = 0;
}

struct tree_opt_pass pass_nrv = 
{
  "nrv",				/* name */
  NULL,					/* gate */
  tree_nrv,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_NRV,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_ggc_collect,			/* todo_flags_finish */
  0					/* letter */
};
