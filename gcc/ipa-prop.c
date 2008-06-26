/* Interprocedural analyses.
   Copyright (C) 2005, 2007 Free Software Foundation, Inc.

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
#include "tree.h"
#include "langhooks.h"
#include "ggc.h"
#include "target.h"
#include "cgraph.h"
#include "ipa-prop.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "flags.h"
#include "timevar.h"

/* Initialize worklist to contain all functions.  */
struct ipa_func_list *
ipa_init_func_list (void)
{
  struct cgraph_node *node;
  struct ipa_func_list * wl;

  wl = NULL;
  for (node = cgraph_nodes; node; node = node->next)
    ipa_push_func_to_list (&wl, node);

  return wl;
}

/* Add cgraph node MT to the worklist. Set worklist element WL
   to point to MT.  */
void
ipa_push_func_to_list (struct ipa_func_list **wl, struct cgraph_node *mt)
{
  struct ipa_func_list *temp;

  temp = XCNEW (struct ipa_func_list);
  temp->node = mt;
  temp->next = *wl;
  *wl = temp;
}

/* Remove a function from the worklist. WL points to the first
   element in the list, which is removed.  */
struct cgraph_node *
ipa_pop_func_from_list (struct ipa_func_list ** wl)
{
  struct ipa_func_list *first;
  struct cgraph_node *return_func;

  first = *wl;
  *wl = (*wl)->next;
  return_func = first->node;
  free (first);
  return return_func;
}

/* Return index of the formal whose tree is ptree in function which corresponds
   to info.  */
static int
ipa_get_param_decl_index (struct ipa_node_params *info, tree ptree)
{
  int i, count;

  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    if (ipa_get_ith_param(info, i) == ptree)
      return i;

  return -1;
}

/* Insert the formal trees to the param_decls array in function MT.  */
void
ipa_create_param_decls_array (struct cgraph_node *mt)
{
  tree fndecl;
  tree fnargs;
  tree parm;
  int param_num;
  struct ipa_node_params *info = IPA_NODE_REF (mt);

  info->param_decls = XCNEWVEC (tree, ipa_get_param_count (info));
  fndecl = mt->decl;
  fnargs = DECL_ARGUMENTS (fndecl);
  param_num = 0;
  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    {
      info->param_decls[param_num] = parm;
      param_num++;
    }
}

/* Count number of formals in MT. Insert the result to the 
   ipa_node_params.  */
void
ipa_count_formal_params (struct cgraph_node *mt)
{
  tree fndecl;
  tree fnargs;
  tree parm;
  int param_num;

  fndecl = mt->decl;
  fnargs = DECL_ARGUMENTS (fndecl);
  param_num = 0;
  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    param_num++;
  ipa_set_param_count (IPA_NODE_REF (mt), param_num);
}

/* Check STMT to detect whether a formal is modified within MT, the appropriate
   entry is updated in the modified_flags array of ipa_node_params (associated
   with MT).  */
static void
ipa_check_stmt_modifications (struct cgraph_node *mt, tree stmt)
{
  int index, j;
  tree parm_decl;
  struct ipa_node_params *info;

  switch (TREE_CODE (stmt))
    {
    case GIMPLE_MODIFY_STMT:
	  if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) == PARM_DECL)
	{
	  info = IPA_NODE_REF (mt);
	  parm_decl = GIMPLE_STMT_OPERAND (stmt, 0);
	  index = ipa_get_param_decl_index (info, parm_decl);
	  if (index >= 0)
	    info->modified_flags[index] = true;
	}
      break;
    case ASM_EXPR:
      /* Asm code could modify any of the parameters.  */
      info = IPA_NODE_REF (mt);
      for (j = 0; j < ipa_get_param_count (IPA_NODE_REF (mt)); j++)
	info->modified_flags[j] = true;
      break;
    default:
      break;
    }
}

/* The modify computation driver for MT. Compute which formal arguments 
   of function MT are locally modified.  Formals may be modified in MT
   if their address is taken, or if
   they appear on the left hand side of an assignment.  */
void
ipa_detect_param_modifications (struct cgraph_node *mt)
{
  tree decl;
  tree body;
  int j, count;
  basic_block bb;
  struct function *func;
  block_stmt_iterator bsi;
  tree stmt, parm_tree;
  struct ipa_node_params *info = IPA_NODE_REF (mt);

  if (ipa_get_param_count (info) == 0)
    return;

  count = ipa_get_param_count (info);
  info->modified_flags = XCNEWVEC (bool, count);
  decl = mt->decl;
  /* ??? Handle pending sizes case. Set all parameters 
     of the function to be modified.  */

  if (DECL_UNINLINABLE (decl))
    {
      for (j = 0; j < count; j++)
	info->modified_flags[j] = true;

      return;
    }
  /* Formals whose address is taken are considered modified.  */
  for (j = 0; j < count; j++)
    {
      parm_tree = ipa_get_ith_param (info, j);
      if (!is_gimple_reg (parm_tree) 
	  && TREE_ADDRESSABLE (parm_tree))
	info->modified_flags[j] = true;
    }
  body = DECL_SAVED_TREE (decl);
  if (body != NULL)
    {
      func = DECL_STRUCT_FUNCTION (decl);
      FOR_EACH_BB_FN (bb, func)
      {
	for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	  {
	    stmt = bsi_stmt (bsi);
	    ipa_check_stmt_modifications (mt, stmt);
	  }
      }
    }
}

/* Count number of arguments callsite CS has and store it in 
   ipa_edge_args structure corresponding to this callsite.  */
void
ipa_count_arguments (struct cgraph_edge *cs)
{
  tree call_tree;
  int arg_num;

  call_tree = get_call_expr_in (cs->call_stmt);
  gcc_assert (TREE_CODE (call_tree) == CALL_EXPR);
  arg_num = call_expr_nargs (call_tree);
  ipa_set_cs_argument_count (IPA_EDGE_REF (cs), arg_num);
}

/* Compute jump function for all arguments of callsite CS 
   and insert the information in the jump_functions array
   in the ipa_edge_args corresponding to this callsite.  */
void
ipa_compute_jump_functions (struct cgraph_edge *cs)
{
  tree call_tree;
  tree arg, cst_decl;
  int arg_num;
  struct cgraph_node *mt;
  tree parm_decl;
  struct function *curr_cfun;
  call_expr_arg_iterator iter;
  struct ipa_edge_args *args = IPA_EDGE_REF (cs);

  if (ipa_get_cs_argument_count (args) == 0)
    return;
  args->jump_functions = XCNEWVEC (struct ipa_jump_func,
				   ipa_get_cs_argument_count (args));
  call_tree = get_call_expr_in (cs->call_stmt);
  gcc_assert (TREE_CODE (call_tree) == CALL_EXPR);
  arg_num = 0;

  FOR_EACH_CALL_EXPR_ARG (arg, iter, call_tree)
    {
      /* If the formal parameter was passed as argument, we store 
         IPA_PASS_THROUGH and its index in the caller as the jump function
         of this argument.  */
      if ((TREE_CODE (arg) == SSA_NAME
	   && TREE_CODE (SSA_NAME_VAR (arg)) == PARM_DECL)
	  || TREE_CODE (arg) == PARM_DECL)
	{
	  struct ipa_node_params *info;
	  int index;

	  mt = cs->caller;
	  info = IPA_NODE_REF (mt);
	  parm_decl = TREE_CODE (arg) == PARM_DECL ? arg : SSA_NAME_VAR (arg);
          
	  index = ipa_get_param_decl_index (info, parm_decl);
	  if (TREE_CODE (arg) == SSA_NAME && IS_VALID_JUMP_FUNC_INDEX (index))
	    {
	      curr_cfun = DECL_STRUCT_FUNCTION (mt->decl);
	      if (!gimple_default_def (curr_cfun, parm_decl) 
	          || gimple_default_def (curr_cfun, parm_decl) != arg)
		info->modified_flags[index] = true;
	    }
	  if (!IS_VALID_JUMP_FUNC_INDEX (index) || info->modified_flags[index])
	    args->jump_functions[arg_num].type = IPA_UNKNOWN;
	  else
	    {
	      args->jump_functions[arg_num].type = IPA_PASS_THROUGH;
	      args->jump_functions[arg_num].value.formal_id = index;
	    }
	}
      /* If a constant value was passed as argument, 
         we store IPA_CONST and its value as the jump function
         of this argument.  */
      else if (TREE_CODE (arg) == INTEGER_CST
	       || TREE_CODE (arg) == REAL_CST
	       || TREE_CODE (arg) == FIXED_CST)
	{
	  args->jump_functions[arg_num].type = IPA_CONST;
	  args->jump_functions[arg_num].value.constant = arg;
	}
      /* This is for the case of Fortran. If the address of a const_decl 
         was passed as argument then we store
         IPA_CONST_REF/IPA_CONST_REF and the constant
         value as the jump function corresponding to this argument.  */
      else if (TREE_CODE (arg) == ADDR_EXPR
	       && TREE_CODE (TREE_OPERAND (arg, 0)) == CONST_DECL)
	{
	  cst_decl = TREE_OPERAND (arg, 0);
	  if (TREE_CODE (DECL_INITIAL (cst_decl)) == INTEGER_CST
	      || TREE_CODE (DECL_INITIAL (cst_decl)) == REAL_CST
	      || TREE_CODE (DECL_INITIAL (cst_decl)) == FIXED_CST)
	    {
	      args->jump_functions[arg_num].type = IPA_CONST_REF;
	      args->jump_functions[arg_num].value.constant = cst_decl;
	    }
	}
      else
	args->jump_functions[arg_num].type = IPA_UNKNOWN;
      arg_num++;
    }
}

/* Allocate and initialize ipa_node_params structure for the given cgraph
   node.  */
void
ipa_create_node_params (struct cgraph_node *node)
{
  node->aux = xcalloc (1, sizeof (struct ipa_node_params));
}

/* Allocate and initialize ipa_node_params structure for all
   nodes in callgraph.  */
void
ipa_create_all_node_params (void)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    ipa_create_node_params (node);
}

/* Allocate and initialize ipa_edge structure.  */
void
ipa_create_all_edge_args (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    for (cs = node->callees; cs; cs = cs->next_callee)
      cs->aux = xcalloc (1, sizeof (struct ipa_edge_args));
}

/* Free ipa_edge structure.  */
void
ipa_free_all_edge_args (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    for (cs = node->callees; cs; cs = cs->next_callee)
      if (cs->aux)
	{
	  if (IPA_EDGE_REF (cs)->jump_functions)
	    free (IPA_EDGE_REF (cs)->jump_functions);
	  free (cs->aux);
	  cs->aux = NULL;
	}
}

/* Free ipa data structures of ipa_node_params and ipa_edge_args.  */
void
ipa_free_all_node_params (void)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    {
      if (node->aux == NULL)
	continue;
      if (IPA_NODE_REF (node)->ipcp_lattices)
	free (IPA_NODE_REF (node)->ipcp_lattices);
      if (IPA_NODE_REF (node)->param_decls)
	free (IPA_NODE_REF (node)->param_decls);
      if (IPA_NODE_REF (node)->modified_flags)
	free (IPA_NODE_REF (node)->modified_flags);
      free (node->aux);
      node->aux = NULL;
    }
}

/* Print ipa_tree_map data structures of all functions in the
   callgraph to F.  */
void
ipa_print_all_tree_maps (FILE * f)
{
  int i, count;
  tree temp;
  struct cgraph_node *node;

  fprintf (f, "\nPARAM TREE MAP PRINT\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info = IPA_NODE_REF (node);
      fprintf (f, "function  %s Trees :: \n", cgraph_node_name (node));
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  temp = ipa_get_ith_param (info, i);
	  if (TREE_CODE (temp) == PARM_DECL)
	    fprintf (f, "  param [%d] : %s\n", i,
		     (*lang_hooks.decl_printable_name) (temp, 2));
	}

    }
}

/* Print modified_flags data structures of all functions in the
   callgraph to F.  */
void
ipa_print_all_params_modified (FILE * f)
{
  int i, count;
  bool temp;
  struct cgraph_node *node;

  fprintf (f, "\nMODIFY PRINT\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info = IPA_NODE_REF (node);
      fprintf (f, "function  %s :: \n", cgraph_node_name (node));
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  temp = info->modified_flags[i];
	  if (temp)
	    fprintf (f, " param [%d] true \n", i);
	  else
	    fprintf (f, " param [%d] false \n", i);
	}
    }
}

