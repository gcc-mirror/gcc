/* Interprocedural analyses.
   Copyright (C) 2005, 2007, 2008, 2009 Free Software Foundation, Inc.

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
#include "tree-inline.h"
#include "flags.h"
#include "timevar.h"
#include "flags.h"
#include "diagnostic.h"
#include "lto-streamer.h"

/* Vector where the parameter infos are actually stored. */
VEC (ipa_node_params_t, heap) *ipa_node_params_vector;
/* Vector where the parameter infos are actually stored. */
VEC (ipa_edge_args_t, gc) *ipa_edge_args_vector;

/* Holders of ipa cgraph hooks: */
static struct cgraph_edge_hook_list *edge_removal_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;
static struct cgraph_2edge_hook_list *edge_duplication_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;

/* Add cgraph NODE described by INFO to the worklist WL regardless of whether
   it is in one or not.  It should almost never be used directly, as opposed to
   ipa_push_func_to_list.  */

void
ipa_push_func_to_list_1 (struct ipa_func_list **wl,
			 struct cgraph_node *node,
			 struct ipa_node_params *info)
{
  struct ipa_func_list *temp;

  info->node_enqueued = 1;
  temp = XCNEW (struct ipa_func_list);
  temp->node = node;
  temp->next = *wl;
  *wl = temp;
}

/* Initialize worklist to contain all functions.  */

struct ipa_func_list *
ipa_init_func_list (void)
{
  struct cgraph_node *node;
  struct ipa_func_list * wl;

  wl = NULL;
  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      {
	struct ipa_node_params *info = IPA_NODE_REF (node);
	/* Unreachable nodes should have been eliminated before ipcp and
	   inlining.  */
	gcc_assert (node->needed || node->reachable);
	ipa_push_func_to_list_1 (&wl, node, info);
      }

  return wl;
}

/* Remove a function from the worklist WL and return it.  */

struct cgraph_node *
ipa_pop_func_from_list (struct ipa_func_list **wl)
{
  struct ipa_node_params *info;
  struct ipa_func_list *first;
  struct cgraph_node *node;

  first = *wl;
  *wl = (*wl)->next;
  node = first->node;
  free (first);

  info = IPA_NODE_REF (node);
  info->node_enqueued = 0;
  return node;
}

/* Return index of the formal whose tree is PTREE in function which corresponds
   to INFO.  */

static int
ipa_get_param_decl_index (struct ipa_node_params *info, tree ptree)
{
  int i, count;

  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    if (ipa_get_param(info, i) == ptree)
      return i;

  return -1;
}

/* Populate the param_decl field in parameter descriptors of INFO that
   corresponds to NODE.  */

static void
ipa_populate_param_decls (struct cgraph_node *node,
			  struct ipa_node_params *info)
{
  tree fndecl;
  tree fnargs;
  tree parm;
  int param_num;

  fndecl = node->decl;
  fnargs = DECL_ARGUMENTS (fndecl);
  param_num = 0;
  for (parm = fnargs; parm; parm = TREE_CHAIN (parm))
    {
      info->params[param_num].decl = parm;
      param_num++;
    }
}

/* Return how many formal parameters FNDECL has.  */

static inline int
count_formal_params_1 (tree fndecl)
{
  tree parm;
  int count = 0;

  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = TREE_CHAIN (parm))
    count++;

  return count;
}

/* Count number of formal parameters in NOTE. Store the result to the
   appropriate field of INFO.  */

static void
ipa_count_formal_params (struct cgraph_node *node,
			 struct ipa_node_params *info)
{
  int param_num;

  param_num = count_formal_params_1 (node->decl);
  ipa_set_param_count (info, param_num);
}

/* Initialize the ipa_node_params structure associated with NODE by counting
   the function parameters, creating the descriptors and populating their
   param_decls.  */

void
ipa_initialize_node_params (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (!info->params)
    {
      ipa_count_formal_params (node, info);
      info->params = XCNEWVEC (struct ipa_param_descriptor,
				    ipa_get_param_count (info));
      ipa_populate_param_decls (node, info);
    }
}

/* Callback of walk_stmt_load_store_addr_ops for the visit_store and visit_addr
   parameters.  If OP is a parameter declaration, mark it as modified in the
   info structure passed in DATA.  */

static bool
visit_store_addr_for_mod_analysis (gimple stmt ATTRIBUTE_UNUSED,
				   tree op, void *data)
{
  struct ipa_node_params *info = (struct ipa_node_params *) data;

  if (TREE_CODE (op) == PARM_DECL)
    {
      int index = ipa_get_param_decl_index (info, op);
      gcc_assert (index >= 0);
      info->params[index].modified = true;
    }

  return false;
}

/* Compute which formal parameters of function associated with NODE are locally
   modified or their address is taken.  Note that this does not apply on
   parameters with SSA names but those can and should be analyzed
   differently.  */

void
ipa_detect_param_modifications (struct cgraph_node *node)
{
  tree decl = node->decl;
  basic_block bb;
  struct function *func;
  gimple_stmt_iterator gsi;
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (ipa_get_param_count (info) == 0 || info->modification_analysis_done)
    return;

  func = DECL_STRUCT_FUNCTION (decl);
  FOR_EACH_BB_FN (bb, func)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      walk_stmt_load_store_addr_ops (gsi_stmt (gsi), info, NULL,
				     visit_store_addr_for_mod_analysis,
				     visit_store_addr_for_mod_analysis);

  info->modification_analysis_done = 1;
}

/* Count number of arguments callsite CS has and store it in
   ipa_edge_args structure corresponding to this callsite.  */

void
ipa_count_arguments (struct cgraph_edge *cs)
{
  gimple stmt;
  int arg_num;

  stmt = cs->call_stmt;
  gcc_assert (is_gimple_call (stmt));
  arg_num = gimple_call_num_args (stmt);
  if (VEC_length (ipa_edge_args_t, ipa_edge_args_vector)
      <= (unsigned) cgraph_edge_max_uid)
    VEC_safe_grow_cleared (ipa_edge_args_t, gc,
			   ipa_edge_args_vector, cgraph_edge_max_uid + 1);
  ipa_set_cs_argument_count (IPA_EDGE_REF (cs), arg_num);
}

/* Print the jump functions of all arguments on all call graph edges going from
   NODE to file F.  */

void
ipa_print_node_jump_functions (FILE *f, struct cgraph_node *node)
{
  int i, count;
  struct cgraph_edge *cs;
  struct ipa_jump_func *jump_func;
  enum jump_func_type type;

  fprintf (f, "  Jump functions of caller  %s:\n", cgraph_node_name (node));
  for (cs = node->callees; cs; cs = cs->next_callee)
    {
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	continue;

      fprintf (f, "    callsite  %s ", cgraph_node_name (node));
      fprintf (f, "-> %s :: \n", cgraph_node_name (cs->callee));

      count = ipa_get_cs_argument_count (IPA_EDGE_REF (cs));
      for (i = 0; i < count; i++)
	{
	  jump_func = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
	  type = jump_func->type;

	  fprintf (f, "       param %d: ", i);
	  if (type == IPA_JF_UNKNOWN)
	    fprintf (f, "UNKNOWN\n");
	  else if (type == IPA_JF_CONST)
 	    {
	      tree val = jump_func->value.constant;
	      fprintf (f, "CONST: ");
	      print_generic_expr (f, val, 0);
	      fprintf (f, "\n");
	    }
	  else if (type == IPA_JF_CONST_MEMBER_PTR)
	    {
	      fprintf (f, "CONST MEMBER PTR: ");
	      print_generic_expr (f, jump_func->value.member_cst.pfn, 0);
	      fprintf (f, ", ");
	      print_generic_expr (f, jump_func->value.member_cst.delta, 0);
	      fprintf (f, "\n");
	    }
	  else if (type == IPA_JF_PASS_THROUGH)
 	    {
	      fprintf (f, "PASS THROUGH: ");
	      fprintf (f, "%d, op %s ",
		       jump_func->value.pass_through.formal_id,
		       tree_code_name[(int)
				      jump_func->value.pass_through.operation]);
	      if (jump_func->value.pass_through.operation != NOP_EXPR)
		print_generic_expr (dump_file,
				    jump_func->value.pass_through.operand, 0);
	      fprintf (dump_file, "\n");
 	    }
	  else if (type == IPA_JF_ANCESTOR)
	    {
	      fprintf (f, "ANCESTOR: ");
	      fprintf (f, "%d, offset "HOST_WIDE_INT_PRINT_DEC"\n",
		       jump_func->value.ancestor.formal_id,
		       jump_func->value.ancestor.offset);
	    }
	}
    }
}

/* Print ipa_jump_func data structures of all nodes in the call graph to F.  */

void
ipa_print_all_jump_functions (FILE *f)
{
  struct cgraph_node *node;

  fprintf (f, "\nJump functions:\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      ipa_print_node_jump_functions (f, node);
    }
}

/* Determine whether passing ssa name NAME constitutes a polynomial
   pass-through function or getting an address of an acestor and if so, write
   such a jump function to JFUNC.  INFO describes the caller.  */

static void
compute_complex_pass_through (struct ipa_node_params *info,
			      struct ipa_jump_func *jfunc,
			      tree name)
{
  HOST_WIDE_INT offset, size, max_size;
  tree op1, op2, type;
  int index;
  gimple stmt = SSA_NAME_DEF_STMT (name);

  if (!is_gimple_assign (stmt))
    return;
  op1 = gimple_assign_rhs1 (stmt);
  op2 = gimple_assign_rhs2 (stmt);

  if (op2)
    {
      if (TREE_CODE (op1) != SSA_NAME
	  || !SSA_NAME_IS_DEFAULT_DEF (op1)
	  || (TREE_CODE_CLASS (gimple_expr_code (stmt)) != tcc_comparison
	      && !useless_type_conversion_p (TREE_TYPE (name),
					     TREE_TYPE (op1)))
	  || !is_gimple_ip_invariant (op2))
	return;

      index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op1));
      if (index >= 0)
	{
	  jfunc->type = IPA_JF_PASS_THROUGH;
	  jfunc->value.pass_through.formal_id = index;
	  jfunc->value.pass_through.operation = gimple_assign_rhs_code (stmt);
	  jfunc->value.pass_through.operand = op2;
	}
      return;
    }

  if (TREE_CODE (op1) != ADDR_EXPR)
    return;
  op1 = TREE_OPERAND (op1, 0);
  type = TREE_TYPE (op1);

  op1 = get_ref_base_and_extent (op1, &offset, &size, &max_size);
  if (TREE_CODE (op1) != INDIRECT_REF
      /* If this is a varying address, punt.  */
      || max_size == -1
      || max_size != size)
    return;
  op1 = TREE_OPERAND (op1, 0);
  if (TREE_CODE (op1) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (op1))
    return;

  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op1));
  if (index >= 0)
    {
      jfunc->type = IPA_JF_ANCESTOR;
      jfunc->value.ancestor.formal_id = index;
      jfunc->value.ancestor.offset = offset;
      jfunc->value.ancestor.type = type;
    }
}


/* Determine the jump functions of scalar arguments.  Scalar means SSA names
   and constants of a number of selected types.  INFO is the ipa_node_params
   structure associated with the caller, FUNCTIONS is a pointer to an array of
   jump function structures associated with CALL which is the call statement
   being examined.*/

static void
compute_scalar_jump_functions (struct ipa_node_params *info,
			       struct ipa_jump_func *functions,
			       gimple call)
{
  tree arg;
  unsigned num = 0;

  for (num = 0; num < gimple_call_num_args (call); num++)
    {
      arg = gimple_call_arg (call, num);

      if (is_gimple_ip_invariant (arg))
	{
	  functions[num].type = IPA_JF_CONST;
	  functions[num].value.constant = arg;
	}
      else if (TREE_CODE (arg) == SSA_NAME)
	{
	  if (SSA_NAME_IS_DEFAULT_DEF (arg))
	    {
	      int index = ipa_get_param_decl_index (info, SSA_NAME_VAR (arg));

	      if (index >= 0)
		{
		  functions[num].type = IPA_JF_PASS_THROUGH;
		  functions[num].value.pass_through.formal_id = index;
		  functions[num].value.pass_through.operation = NOP_EXPR;
		}
	    }
	  else
	    compute_complex_pass_through (info, &functions[num], arg);
	}
    }
}

/* Inspect the given TYPE and return true iff it has the same structure (the
   same number of fields of the same types) as a C++ member pointer.  If
   METHOD_PTR and DELTA are non-NULL, store the trees representing the
   corresponding fields there.  */

static bool
type_like_member_ptr_p (tree type, tree *method_ptr, tree *delta)
{
  tree fld;

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  fld = TYPE_FIELDS (type);
  if (!fld || !POINTER_TYPE_P (TREE_TYPE (fld))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (fld))) != METHOD_TYPE)
    return false;

  if (method_ptr)
    *method_ptr = fld;

  fld = TREE_CHAIN (fld);
  if (!fld || INTEGRAL_TYPE_P (fld))
    return false;
  if (delta)
    *delta = fld;

  if (TREE_CHAIN (fld))
    return false;

  return true;
}

/* Go through arguments of the CALL and for every one that looks like a member
   pointer, check whether it can be safely declared pass-through and if so,
   mark that to the corresponding item of jump FUNCTIONS.  Return true iff
   there are non-pass-through member pointers within the arguments.  INFO
   describes formal parameters of the caller.  */

static bool
compute_pass_through_member_ptrs (struct ipa_node_params *info,
				  struct ipa_jump_func *functions,
				  gimple call)
{
  bool undecided_members = false;
  unsigned num;
  tree arg;

  for (num = 0; num < gimple_call_num_args (call); num++)
    {
      arg = gimple_call_arg (call, num);

      if (type_like_member_ptr_p (TREE_TYPE (arg), NULL, NULL))
	{
	  if (TREE_CODE (arg) == PARM_DECL)
	    {
	      int index = ipa_get_param_decl_index (info, arg);

	      gcc_assert (index >=0);
	      if (!ipa_is_param_modified (info, index))
		{
		  functions[num].type = IPA_JF_PASS_THROUGH;
		  functions[num].value.pass_through.formal_id = index;
		  functions[num].value.pass_through.operation = NOP_EXPR;
		}
	      else
		undecided_members = true;
	    }
	  else
	    undecided_members = true;
	}
    }

  return undecided_members;
}

/* Simple function filling in a member pointer constant jump function (with PFN
   and DELTA as the constant value) into JFUNC.  */

static void
fill_member_ptr_cst_jump_function (struct ipa_jump_func *jfunc,
				   tree pfn, tree delta)
{
  jfunc->type = IPA_JF_CONST_MEMBER_PTR;
  jfunc->value.member_cst.pfn = pfn;
  jfunc->value.member_cst.delta = delta;
}

/* If RHS is an SSA_NAMe and it is defined by a simple copy assign statement,
   return the rhs of its defining statement.  */

static inline tree
get_ssa_def_if_simple_copy (tree rhs)
{
  while (TREE_CODE (rhs) == SSA_NAME && !SSA_NAME_IS_DEFAULT_DEF (rhs))
    {
      gimple def_stmt = SSA_NAME_DEF_STMT (rhs);

      if (gimple_assign_single_p (def_stmt))
	rhs = gimple_assign_rhs1 (def_stmt);
      else
	break;
    }
  return rhs;
}

/* Traverse statements from CALL backwards, scanning whether the argument ARG
   which is a member pointer is filled in with constant values.  If it is, fill
   the jump function JFUNC in appropriately.  METHOD_FIELD and DELTA_FIELD are
   fields of the record type of the member pointer.  To give an example, we
   look for a pattern looking like the following:

     D.2515.__pfn ={v} printStuff;
     D.2515.__delta ={v} 0;
     i_1 = doprinting (D.2515);  */

static void
determine_cst_member_ptr (gimple call, tree arg, tree method_field,
			  tree delta_field, struct ipa_jump_func *jfunc)
{
  gimple_stmt_iterator gsi;
  tree method = NULL_TREE;
  tree delta = NULL_TREE;

  gsi = gsi_for_stmt (call);

  gsi_prev (&gsi);
  for (; !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      tree lhs, rhs, fld;

      if (!gimple_assign_single_p (stmt))
	return;

      lhs = gimple_assign_lhs (stmt);
      rhs = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (lhs) != COMPONENT_REF
	  || TREE_OPERAND (lhs, 0) != arg)
	continue;

      fld = TREE_OPERAND (lhs, 1);
      if (!method && fld == method_field)
	{
	  rhs = get_ssa_def_if_simple_copy (rhs);
	  if (TREE_CODE (rhs) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (rhs, 0)) == FUNCTION_DECL
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (rhs, 0))) == METHOD_TYPE)
	    {
	      method = TREE_OPERAND (rhs, 0);
	      if (delta)
		{
		  fill_member_ptr_cst_jump_function (jfunc, rhs, delta);
		  return;
		}
	    }
	  else
	    return;
	}

      if (!delta && fld == delta_field)
	{
	  rhs = get_ssa_def_if_simple_copy (rhs);
	  if (TREE_CODE (rhs) == INTEGER_CST)
	    {
	      delta = rhs;
	      if (method)
		{
		  fill_member_ptr_cst_jump_function (jfunc, rhs, delta);
		  return;
		}
	    }
	  else
	    return;
	}
    }

  return;
}

/* Go through the arguments of the CALL and for every member pointer within
   tries determine whether it is a constant.  If it is, create a corresponding
   constant jump function in FUNCTIONS which is an array of jump functions
   associated with the call.  */

static void
compute_cst_member_ptr_arguments (struct ipa_jump_func *functions,
				  gimple call)
{
  unsigned num;
  tree arg, method_field, delta_field;

  for (num = 0; num < gimple_call_num_args (call); num++)
    {
      arg = gimple_call_arg (call, num);

      if (functions[num].type == IPA_JF_UNKNOWN
	  && type_like_member_ptr_p (TREE_TYPE (arg), &method_field,
				     &delta_field))
	determine_cst_member_ptr (call, arg, method_field, delta_field,
				  &functions[num]);
    }
}

/* Compute jump function for all arguments of callsite CS and insert the
   information in the jump_functions array in the ipa_edge_args corresponding
   to this callsite.  */

void
ipa_compute_jump_functions (struct cgraph_edge *cs)
{
  struct ipa_node_params *info = IPA_NODE_REF (cs->caller);
  struct ipa_edge_args *arguments = IPA_EDGE_REF (cs);
  gimple call;

  if (ipa_get_cs_argument_count (arguments) == 0 || arguments->jump_functions)
    return;
  arguments->jump_functions = GGC_CNEWVEC (struct ipa_jump_func,
					   ipa_get_cs_argument_count (arguments));

  call = cs->call_stmt;
  gcc_assert (is_gimple_call (call));

  /* We will deal with constants and SSA scalars first:  */
  compute_scalar_jump_functions (info, arguments->jump_functions, call);

  /* Let's check whether there are any potential member pointers and if so,
     whether we can determine their functions as pass_through.  */
  if (!compute_pass_through_member_ptrs (info, arguments->jump_functions, call))
    return;

  /* Finally, let's check whether we actually pass a new constant member
     pointer here...  */
  compute_cst_member_ptr_arguments (arguments->jump_functions, call);
}

/* If RHS looks like a rhs of a statement loading pfn from a member
   pointer formal parameter, return the parameter, otherwise return
   NULL.  If USE_DELTA, then we look for a use of the delta field
   rather than the pfn.  */

static tree
ipa_get_member_ptr_load_param (tree rhs, bool use_delta)
{
  tree rec, fld;
  tree ptr_field;
  tree delta_field;

  if (TREE_CODE (rhs) != COMPONENT_REF)
    return NULL_TREE;

  rec = TREE_OPERAND (rhs, 0);
  if (TREE_CODE (rec) != PARM_DECL
      || !type_like_member_ptr_p (TREE_TYPE (rec), &ptr_field, &delta_field))
    return NULL_TREE;

  fld = TREE_OPERAND (rhs, 1);
  if (use_delta ? (fld == delta_field) : (fld == ptr_field))
    return rec;
  else
    return NULL_TREE;
}

/* If STMT looks like a statement loading a value from a member pointer formal
   parameter, this function returns that parameter.  */

static tree
ipa_get_stmt_member_ptr_load_param (gimple stmt, bool use_delta)
{
  tree rhs;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  rhs = gimple_assign_rhs1 (stmt);
  return ipa_get_member_ptr_load_param (rhs, use_delta);
}

/* Returns true iff T is an SSA_NAME defined by a statement.  */

static bool
ipa_is_ssa_with_stmt_def (tree t)
{
  if (TREE_CODE (t) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (t))
    return true;
  else
    return false;
}

/* Creates a new note describing a call to a parameter number FORMAL_ID and
   attaches it to the linked list of INFO.  It also sets the called flag of the
   parameter.  STMT is the corresponding call statement.  */

static void
ipa_note_param_call (struct ipa_node_params *info, int formal_id,
		     gimple stmt)
{
  struct ipa_param_call_note *note;
  basic_block bb = gimple_bb (stmt);

  note = XCNEW (struct ipa_param_call_note);
  note->formal_id = formal_id;
  note->stmt = stmt;
  note->lto_stmt_uid = gimple_uid (stmt);
  note->count = bb->count;
  note->frequency = compute_call_stmt_bb_frequency (current_function_decl, bb);
  note->loop_nest = bb->loop_depth;

  note->next = info->param_calls;
  info->param_calls = note;

  return;
}

/* Analyze the CALL and examine uses of formal parameters of the caller
   (described by INFO).  Currently it checks whether the call calls a pointer
   that is a formal parameter and if so, the parameter is marked with the
   called flag and a note describing the call is created.  This is very simple
   for ordinary pointers represented in SSA but not-so-nice when it comes to
   member pointers.  The ugly part of this function does nothing more than
   tries to match the pattern of such a call.  An example of such a pattern is
   the gimple dump below, the call is on the last line:

     <bb 2>:
       f$__delta_5 = f.__delta;
       f$__pfn_24 = f.__pfn;
       D.2496_3 = (int) f$__pfn_24;
       D.2497_4 = D.2496_3 & 1;
       if (D.2497_4 != 0)
         goto <bb 3>;
       else
         goto <bb 4>;

     <bb 3>:
       D.2500_7 = (unsigned int) f$__delta_5;
       D.2501_8 = &S + D.2500_7;
       D.2502_9 = (int (*__vtbl_ptr_type) (void) * *) D.2501_8;
       D.2503_10 = *D.2502_9;
       D.2504_12 = f$__pfn_24 + -1;
       D.2505_13 = (unsigned int) D.2504_12;
       D.2506_14 = D.2503_10 + D.2505_13;
       D.2507_15 = *D.2506_14;
       iftmp.11_16 = (String:: *) D.2507_15;

     <bb 4>:
       # iftmp.11_1 = PHI <iftmp.11_16(3), f$__pfn_24(2)>
       D.2500_19 = (unsigned int) f$__delta_5;
       D.2508_20 = &S + D.2500_19;
       D.2493_21 = iftmp.11_1 (D.2508_20, 4);

   Such patterns are results of simple calls to a member pointer:

     int doprinting (int (MyString::* f)(int) const)
     {
       MyString S ("somestring");

       return (S.*f)(4);
     }
*/

static void
ipa_analyze_call_uses (struct ipa_node_params *info, gimple call)
{
  tree target = gimple_call_fn (call);
  gimple def;
  tree var;
  tree n1, n2;
  gimple d1, d2;
  tree rec, rec2, cond;
  gimple branch;
  int index;
  basic_block bb, virt_bb, join;

  if (TREE_CODE (target) != SSA_NAME)
    return;

  var = SSA_NAME_VAR (target);
  if (SSA_NAME_IS_DEFAULT_DEF (target))
    {
      /* assuming TREE_CODE (var) == PARM_DECL */
      index = ipa_get_param_decl_index (info, var);
      if (index >= 0)
	ipa_note_param_call (info, index, call);
      return;
    }

  /* Now we need to try to match the complex pattern of calling a member
     pointer. */

  if (!POINTER_TYPE_P (TREE_TYPE (target))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (target))) != METHOD_TYPE)
    return;

  def = SSA_NAME_DEF_STMT (target);
  if (gimple_code (def) != GIMPLE_PHI)
    return;

  if (gimple_phi_num_args (def) != 2)
    return;

  /* First, we need to check whether one of these is a load from a member
     pointer that is a parameter to this function. */
  n1 = PHI_ARG_DEF (def, 0);
  n2 = PHI_ARG_DEF (def, 1);
  if (!ipa_is_ssa_with_stmt_def (n1) || !ipa_is_ssa_with_stmt_def (n2))
    return;
  d1 = SSA_NAME_DEF_STMT (n1);
  d2 = SSA_NAME_DEF_STMT (n2);

  if ((rec = ipa_get_stmt_member_ptr_load_param (d1, false)))
    {
      if (ipa_get_stmt_member_ptr_load_param (d2, false))
	return;

      bb = gimple_bb (d1);
      virt_bb = gimple_bb (d2);
    }
  else if ((rec = ipa_get_stmt_member_ptr_load_param (d2, false)))
    {
      bb = gimple_bb (d2);
      virt_bb = gimple_bb (d1);
    }
  else
    return;

  /* Second, we need to check that the basic blocks are laid out in the way
     corresponding to the pattern. */

  join = gimple_bb (def);
  if (!single_pred_p (virt_bb) || !single_succ_p (virt_bb)
      || single_pred (virt_bb) != bb
      || single_succ (virt_bb) != join)
    return;

  /* Third, let's see that the branching is done depending on the least
     significant bit of the pfn. */

  branch = last_stmt (bb);
  if (gimple_code (branch) != GIMPLE_COND)
    return;

  if (gimple_cond_code (branch) != NE_EXPR
      || !integer_zerop (gimple_cond_rhs (branch)))
    return;

  cond = gimple_cond_lhs (branch);
  if (!ipa_is_ssa_with_stmt_def (cond))
    return;

  def = SSA_NAME_DEF_STMT (cond);
  if (!is_gimple_assign (def)
      || gimple_assign_rhs_code (def) != BIT_AND_EXPR
      || !integer_onep (gimple_assign_rhs2 (def)))
    return;

  cond = gimple_assign_rhs1 (def);
  if (!ipa_is_ssa_with_stmt_def (cond))
    return;

  def = SSA_NAME_DEF_STMT (cond);

  if (is_gimple_assign (def)
      && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def)))
    {
      cond = gimple_assign_rhs1 (def);
      if (!ipa_is_ssa_with_stmt_def (cond))
	return;
      def = SSA_NAME_DEF_STMT (cond);
    }

  rec2 = ipa_get_stmt_member_ptr_load_param (def,
					     (TARGET_PTRMEMFUNC_VBIT_LOCATION
					      == ptrmemfunc_vbit_in_delta));

  if (rec != rec2)
    return;

  index = ipa_get_param_decl_index (info, rec);
  if (index >= 0 && !ipa_is_param_modified (info, index))
    ipa_note_param_call (info, index, call);

  return;
}

/* Analyze the statement STMT with respect to formal parameters (described in
   INFO) and their uses.  Currently it only checks whether formal parameters
   are called.  */

static void
ipa_analyze_stmt_uses (struct ipa_node_params *info, gimple stmt)
{
  if (is_gimple_call (stmt))
    ipa_analyze_call_uses (info, stmt);
}

/* Scan the function body of NODE and inspect the uses of formal parameters.
   Store the findings in various structures of the associated ipa_node_params
   structure, such as parameter flags, notes etc.  */

void
ipa_analyze_params_uses (struct cgraph_node *node)
{
  tree decl = node->decl;
  basic_block bb;
  struct function *func;
  gimple_stmt_iterator gsi;
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (ipa_get_param_count (info) == 0 || info->uses_analysis_done)
    return;

  func = DECL_STRUCT_FUNCTION (decl);
  FOR_EACH_BB_FN (bb, func)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  ipa_analyze_stmt_uses (info, stmt);
	}
    }

  info->uses_analysis_done = 1;
}

/* Update the jump functions associated with call graph edge E when the call
   graph edge CS is being inlined, assuming that E->caller is already (possibly
   indirectly) inlined into CS->callee and that E has not been inlined.

   We keep pass through functions only if they do not contain any operation.
   This is sufficient for inlining and greately simplifies things.  */

static void
update_jump_functions_after_inlining (struct cgraph_edge *cs,
				      struct cgraph_edge *e)
{
  struct ipa_edge_args *top = IPA_EDGE_REF (cs);
  struct ipa_edge_args *args = IPA_EDGE_REF (e);
  int count = ipa_get_cs_argument_count (args);
  int i;

  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *src, *dst = ipa_get_ith_jump_func (args, i);

      if (dst->type == IPA_JF_ANCESTOR)
	{
	  dst->type = IPA_JF_UNKNOWN;
	  continue;
	}

      if (dst->type != IPA_JF_PASS_THROUGH)
	continue;

      /* We must check range due to calls with variable number of arguments and
	 we cannot combine jump functions with operations.  */
      if (dst->value.pass_through.operation != NOP_EXPR
	  || (dst->value.pass_through.formal_id
	      >= ipa_get_cs_argument_count (top)))
	{
	  dst->type = IPA_JF_UNKNOWN;
	  continue;
	}

      src = ipa_get_ith_jump_func (top, dst->value.pass_through.formal_id);
      *dst = *src;
    }
}

/* Print out a debug message to file F that we have discovered that an indirect
   call described by NT is in fact a call of a known constant function described
   by JFUNC.  NODE is the node where the call is.  */

static void
print_edge_addition_message (FILE *f, struct ipa_param_call_note *nt,
			     struct ipa_jump_func *jfunc,
			     struct cgraph_node *node)
{
  fprintf (f, "ipa-prop: Discovered an indirect call to a known target (");
  if (jfunc->type == IPA_JF_CONST_MEMBER_PTR)
    {
      print_node_brief (f, "", jfunc->value.member_cst.pfn, 0);
      print_node_brief (f, ", ", jfunc->value.member_cst.delta, 0);
    }
  else
    print_node_brief(f, "", jfunc->value.constant, 0);

  fprintf (f, ") in %s: ", cgraph_node_name (node));
  print_gimple_stmt (f, nt->stmt, 2, TDF_SLIM);
}

/* Update the param called notes associated with NODE when CS is being inlined,
   assuming NODE is (potentially indirectly) inlined into CS->callee.
   Moreover, if the callee is discovered to be constant, create a new cgraph
   edge for it.  Newly discovered indirect edges will be added to *NEW_EDGES,
   unless NEW_EDGES is NULL.  Return true iff a new edge(s) were created.  */

static bool
update_call_notes_after_inlining (struct cgraph_edge *cs,
				  struct cgraph_node *node,
				  VEC (cgraph_edge_p, heap) **new_edges)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  struct ipa_edge_args *top = IPA_EDGE_REF (cs);
  struct ipa_param_call_note *nt;
  bool res = false;

  for (nt = info->param_calls; nt; nt = nt->next)
    {
      struct ipa_jump_func *jfunc;

      if (nt->processed)
	continue;

      /* We must check range due to calls with variable number of arguments:  */
      if (nt->formal_id >= ipa_get_cs_argument_count (top))
	{
	  nt->processed = true;
	  continue;
	}

      jfunc = ipa_get_ith_jump_func (top, nt->formal_id);
      if (jfunc->type == IPA_JF_PASS_THROUGH
	  && jfunc->value.pass_through.operation == NOP_EXPR)
	nt->formal_id = jfunc->value.pass_through.formal_id;
      else if (jfunc->type == IPA_JF_CONST
	       || jfunc->type == IPA_JF_CONST_MEMBER_PTR)
	{
	  struct cgraph_node *callee;
	  struct cgraph_edge *new_indirect_edge;
	  tree decl;

	  nt->processed = true;
	  if (jfunc->type == IPA_JF_CONST_MEMBER_PTR)
	    decl = jfunc->value.member_cst.pfn;
	  else
	    decl = jfunc->value.constant;

	  if (TREE_CODE (decl) != ADDR_EXPR)
	    continue;
	  decl = TREE_OPERAND (decl, 0);

	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    continue;
	  callee = cgraph_node (decl);
	  if (!callee || !callee->local.inlinable)
	    continue;

	  res = true;
	  if (dump_file)
	    print_edge_addition_message (dump_file, nt, jfunc, node);

	  new_indirect_edge = cgraph_create_edge (node, callee, nt->stmt,
						  nt->count, nt->frequency,
						  nt->loop_nest);
	  new_indirect_edge->lto_stmt_uid = nt->lto_stmt_uid;
	  new_indirect_edge->indirect_call = 1;
	  ipa_check_create_edge_args ();
	  if (new_edges)
	    VEC_safe_push (cgraph_edge_p, heap, *new_edges, new_indirect_edge);
	  top = IPA_EDGE_REF (cs);
	}
      else
	{
	  /* Ancestor jum functions and pass theoughs with operations should
	     not be used on parameters that then get called.  */
	  gcc_assert (jfunc->type == IPA_JF_UNKNOWN);
	  nt->processed = true;
	}
    }
  return res;
}

/* Recursively traverse subtree of NODE (including node) made of inlined
   cgraph_edges when CS has been inlined and invoke
   update_call_notes_after_inlining on all nodes and
   update_jump_functions_after_inlining on all non-inlined edges that lead out
   of this subtree.  Newly discovered indirect edges will be added to
   *NEW_EDGES, unless NEW_EDGES is NULL.  Return true iff a new edge(s) were
   created.  */

static bool
propagate_info_to_inlined_callees (struct cgraph_edge *cs,
				   struct cgraph_node *node,
				   VEC (cgraph_edge_p, heap) **new_edges)
{
  struct cgraph_edge *e;
  bool res;

  res = update_call_notes_after_inlining (cs, node, new_edges);

  for (e = node->callees; e; e = e->next_callee)
    if (!e->inline_failed)
      res |= propagate_info_to_inlined_callees (cs, e->callee, new_edges);
    else
      update_jump_functions_after_inlining (cs, e);

  return res;
}

/* Update jump functions and call note functions on inlining the call site CS.
   CS is expected to lead to a node already cloned by
   cgraph_clone_inline_nodes.  Newly discovered indirect edges will be added to
   *NEW_EDGES, unless NEW_EDGES is NULL.  Return true iff a new edge(s) were +
   created.  */

bool
ipa_propagate_indirect_call_infos (struct cgraph_edge *cs,
				   VEC (cgraph_edge_p, heap) **new_edges)
{
  /* FIXME lto: We do not stream out indirect call information.  */
  if (flag_wpa)
    return false;

  /* Do nothing if the preparation phase has not been carried out yet
     (i.e. during early inlining).  */
  if (!ipa_node_params_vector)
    return false;
  gcc_assert (ipa_edge_args_vector);

  return propagate_info_to_inlined_callees (cs, cs->callee, new_edges);
}

/* Frees all dynamically allocated structures that the argument info points
   to.  */

void
ipa_free_edge_args_substructures (struct ipa_edge_args *args)
{
  if (args->jump_functions)
    ggc_free (args->jump_functions);

  memset (args, 0, sizeof (*args));
}

/* Free all ipa_edge structures.  */

void
ipa_free_all_edge_args (void)
{
  int i;
  struct ipa_edge_args *args;

  for (i = 0;
       VEC_iterate (ipa_edge_args_t, ipa_edge_args_vector, i, args);
       i++)
    ipa_free_edge_args_substructures (args);

  VEC_free (ipa_edge_args_t, gc, ipa_edge_args_vector);
  ipa_edge_args_vector = NULL;
}

/* Frees all dynamically allocated structures that the param info points
   to.  */

void
ipa_free_node_params_substructures (struct ipa_node_params *info)
{
  if (info->params)
    free (info->params);

  while (info->param_calls)
    {
      struct ipa_param_call_note *note = info->param_calls;
      info->param_calls = note->next;
      free (note);
    }

  memset (info, 0, sizeof (*info));
}

/* Free all ipa_node_params structures.  */

void
ipa_free_all_node_params (void)
{
  int i;
  struct ipa_node_params *info;

  for (i = 0;
       VEC_iterate (ipa_node_params_t, ipa_node_params_vector, i, info);
       i++)
    ipa_free_node_params_substructures (info);

  VEC_free (ipa_node_params_t, heap, ipa_node_params_vector);
  ipa_node_params_vector = NULL;
}

/* Hook that is called by cgraph.c when an edge is removed.  */

static void
ipa_edge_removal_hook (struct cgraph_edge *cs, void *data ATTRIBUTE_UNUSED)
{
  /* During IPA-CP updating we can be called on not-yet analyze clones.  */
  if (VEC_length (ipa_edge_args_t, ipa_edge_args_vector)
      <= (unsigned)cs->uid)
    return;
  ipa_free_edge_args_substructures (IPA_EDGE_REF (cs));
}

/* Hook that is called by cgraph.c when a node is removed.  */

static void
ipa_node_removal_hook (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  ipa_free_node_params_substructures (IPA_NODE_REF (node));
}

/* Helper function to duplicate an array of size N that is at SRC and store a
   pointer to it to DST.  Nothing is done if SRC is NULL.  */

static void *
duplicate_array (void *src, size_t n)
{
  void *p;

  if (!src)
    return NULL;

  p = xmalloc (n);
  memcpy (p, src, n);
  return p;
}

/* Like duplicate_array byt in GGC memory.  */

static void *
duplicate_ggc_array (void *src, size_t n)
{
  void *p;

  if (!src)
    return NULL;

  p = ggc_alloc (n);
  memcpy (p, src, n);
  return p;
}

/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
ipa_edge_duplication_hook (struct cgraph_edge *src, struct cgraph_edge *dst,
			   __attribute__((unused)) void *data)
{
  struct ipa_edge_args *old_args, *new_args;
  int arg_count;

  ipa_check_create_edge_args ();

  old_args = IPA_EDGE_REF (src);
  new_args = IPA_EDGE_REF (dst);

  arg_count = ipa_get_cs_argument_count (old_args);
  ipa_set_cs_argument_count (new_args, arg_count);
  new_args->jump_functions = (struct ipa_jump_func *)
    duplicate_ggc_array (old_args->jump_functions,
		         sizeof (struct ipa_jump_func) * arg_count);
}

/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
ipa_node_duplication_hook (struct cgraph_node *src, struct cgraph_node *dst,
			   __attribute__((unused)) void *data)
{
  struct ipa_node_params *old_info, *new_info;
  struct ipa_param_call_note *note;
  int param_count;

  ipa_check_create_node_params ();
  old_info = IPA_NODE_REF (src);
  new_info = IPA_NODE_REF (dst);
  param_count = ipa_get_param_count (old_info);

  ipa_set_param_count (new_info, param_count);
  new_info->params = (struct ipa_param_descriptor *)
    duplicate_array (old_info->params,
		     sizeof (struct ipa_param_descriptor) * param_count);
  new_info->ipcp_orig_node = old_info->ipcp_orig_node;
  new_info->count_scale = old_info->count_scale;

  for (note = old_info->param_calls; note; note = note->next)
    {
      struct ipa_param_call_note *nn;

      nn = (struct ipa_param_call_note *)
	xcalloc (1, sizeof (struct ipa_param_call_note));
      memcpy (nn, note, sizeof (struct ipa_param_call_note));
      nn->next = new_info->param_calls;
      new_info->param_calls = nn;
    }
}

/* Register our cgraph hooks if they are not already there.  */

void
ipa_register_cgraph_hooks (void)
{
  if (!edge_removal_hook_holder)
    edge_removal_hook_holder =
      cgraph_add_edge_removal_hook (&ipa_edge_removal_hook, NULL);
  if (!node_removal_hook_holder)
    node_removal_hook_holder =
      cgraph_add_node_removal_hook (&ipa_node_removal_hook, NULL);
  if (!edge_duplication_hook_holder)
    edge_duplication_hook_holder =
      cgraph_add_edge_duplication_hook (&ipa_edge_duplication_hook, NULL);
  if (!node_duplication_hook_holder)
    node_duplication_hook_holder =
      cgraph_add_node_duplication_hook (&ipa_node_duplication_hook, NULL);
}

/* Unregister our cgraph hooks if they are not already there.  */

static void
ipa_unregister_cgraph_hooks (void)
{
  cgraph_remove_edge_removal_hook (edge_removal_hook_holder);
  edge_removal_hook_holder = NULL;
  cgraph_remove_node_removal_hook (node_removal_hook_holder);
  node_removal_hook_holder = NULL;
  cgraph_remove_edge_duplication_hook (edge_duplication_hook_holder);
  edge_duplication_hook_holder = NULL;
  cgraph_remove_node_duplication_hook (node_duplication_hook_holder);
  node_duplication_hook_holder = NULL;
}

/* Free all ipa_node_params and all ipa_edge_args structures if they are no
   longer needed after ipa-cp.  */

void
free_all_ipa_structures_after_ipa_cp (void)
{
  if (!flag_indirect_inlining)
    {
      ipa_free_all_edge_args ();
      ipa_free_all_node_params ();
      ipa_unregister_cgraph_hooks ();
    }
}

/* Free all ipa_node_params and all ipa_edge_args structures if they are no
   longer needed after indirect inlining.  */

void
free_all_ipa_structures_after_iinln (void)
{
  ipa_free_all_edge_args ();
  ipa_free_all_node_params ();
  ipa_unregister_cgraph_hooks ();
}

/* Print ipa_tree_map data structures of all functions in the
   callgraph to F.  */

void
ipa_print_node_params (FILE * f, struct cgraph_node *node)
{
  int i, count;
  tree temp;
  struct ipa_node_params *info;

  if (!node->analyzed)
    return;
  info = IPA_NODE_REF (node);
  fprintf (f, "  function  %s Trees :: \n", cgraph_node_name (node));
  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    {
      temp = ipa_get_param (info, i);
      if (TREE_CODE (temp) == PARM_DECL)
	fprintf (f, "    param %d : %s", i,
                 (DECL_NAME (temp)
                  ? (*lang_hooks.decl_printable_name) (temp, 2)
                  : "(unnamed)"));
      if (ipa_is_param_modified (info, i))
	fprintf (f, " modified");
      fprintf (f, "\n");
    }
}

/* Print ipa_tree_map data structures of all functions in the
   callgraph to F.  */

void
ipa_print_all_params (FILE * f)
{
  struct cgraph_node *node;

  fprintf (f, "\nFunction parameters:\n");
  for (node = cgraph_nodes; node; node = node->next)
    ipa_print_node_params (f, node);
}

/* Return a heap allocated vector containing formal parameters of FNDECL.  */

VEC(tree, heap) *
ipa_get_vector_of_formal_parms (tree fndecl)
{
  VEC(tree, heap) *args;
  int count;
  tree parm;

  count = count_formal_params_1 (fndecl);
  args = VEC_alloc (tree, heap, count);
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = TREE_CHAIN (parm))
    VEC_quick_push (tree, args, parm);

  return args;
}

/* Return a heap allocated vector containing types of formal parameters of
   function type FNTYPE.  */

static inline VEC(tree, heap) *
get_vector_of_formal_parm_types (tree fntype)
{
  VEC(tree, heap) *types;
  int count = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    count++;

  types = VEC_alloc (tree, heap, count);
  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    VEC_quick_push (tree, types, TREE_VALUE (t));

  return types;
}

/* Modify the function declaration FNDECL and its type according to the plan in
   ADJUSTMENTS.  It also sets base fields of individual adjustments structures
   to reflect the actual parameters being modified which are determined by the
   base_index field.  */

void
ipa_modify_formal_parameters (tree fndecl, ipa_parm_adjustment_vec adjustments,
			      const char *synth_parm_prefix)
{
  VEC(tree, heap) *oparms, *otypes;
  tree orig_type, new_type = NULL;
  tree old_arg_types, t, new_arg_types = NULL;
  tree parm, *link = &DECL_ARGUMENTS (fndecl);
  int i, len = VEC_length (ipa_parm_adjustment_t, adjustments);
  tree new_reversed = NULL;
  bool care_for_types, last_parm_void;

  if (!synth_parm_prefix)
    synth_parm_prefix = "SYNTH";

  oparms = ipa_get_vector_of_formal_parms (fndecl);
  orig_type = TREE_TYPE (fndecl);
  old_arg_types = TYPE_ARG_TYPES (orig_type);

  /* The following test is an ugly hack, some functions simply don't have any
     arguments in their type.  This is probably a bug but well... */
  care_for_types = (old_arg_types != NULL_TREE);
  if (care_for_types)
    {
      last_parm_void = (TREE_VALUE (tree_last (old_arg_types))
			== void_type_node);
      otypes = get_vector_of_formal_parm_types (orig_type);
      if (last_parm_void)
	gcc_assert (VEC_length (tree, oparms) + 1 == VEC_length (tree, otypes));
      else
	gcc_assert (VEC_length (tree, oparms) == VEC_length (tree, otypes));
    }
  else
    {
      last_parm_void = false;
      otypes = NULL;
    }

  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      gcc_assert (link);

      adj = VEC_index (ipa_parm_adjustment_t, adjustments, i);
      parm = VEC_index (tree, oparms, adj->base_index);
      adj->base = parm;

      if (adj->copy_param)
	{
	  if (care_for_types)
	    new_arg_types = tree_cons (NULL_TREE, VEC_index (tree, otypes,
							     adj->base_index),
				       new_arg_types);
	  *link = parm;
	  link = &TREE_CHAIN (parm);
	}
      else if (!adj->remove_param)
	{
	  tree new_parm;
	  tree ptype;

	  if (adj->by_ref)
	    ptype = build_pointer_type (adj->type);
	  else
	    ptype = adj->type;

	  if (care_for_types)
	    new_arg_types = tree_cons (NULL_TREE, ptype, new_arg_types);

	  new_parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE,
				 ptype);
	  DECL_NAME (new_parm) = create_tmp_var_name (synth_parm_prefix);

	  DECL_ARTIFICIAL (new_parm) = 1;
	  DECL_ARG_TYPE (new_parm) = ptype;
	  DECL_CONTEXT (new_parm) = fndecl;
	  TREE_USED (new_parm) = 1;
	  DECL_IGNORED_P (new_parm) = 1;
	  layout_decl (new_parm, 0);

	  add_referenced_var (new_parm);
	  mark_sym_for_renaming (new_parm);
	  adj->base = parm;
	  adj->reduction = new_parm;

	  *link = new_parm;

	  link = &TREE_CHAIN (new_parm);
	}
    }

  *link = NULL_TREE;

  if (care_for_types)
    {
      new_reversed = nreverse (new_arg_types);
      if (last_parm_void)
	{
	  if (new_reversed)
	    TREE_CHAIN (new_arg_types) = void_list_node;
	  else
	    new_reversed = void_list_node;
	}
    }

  /* Use copy_node to preserve as much as possible from original type
     (debug info, attribute lists etc.)
     Exception is METHOD_TYPEs must have THIS argument.
     When we are asked to remove it, we need to build new FUNCTION_TYPE
     instead.  */
  if (TREE_CODE (orig_type) != METHOD_TYPE
       || (VEC_index (ipa_parm_adjustment_t, adjustments, 0)->copy_param
	 && VEC_index (ipa_parm_adjustment_t, adjustments, 0)->base_index == 0))
    {
      new_type = copy_node (orig_type);
      TYPE_ARG_TYPES (new_type) = new_reversed;
    }
  else
    {
      new_type
        = build_distinct_type_copy (build_function_type (TREE_TYPE (orig_type),
							 new_reversed));
      TYPE_CONTEXT (new_type) = TYPE_CONTEXT (orig_type);
      DECL_VINDEX (fndecl) = NULL_TREE;
    }

  /* This is a new type, not a copy of an old type.  Need to reassociate
     variants.  We can handle everything except the main variant lazily.  */
  t = TYPE_MAIN_VARIANT (orig_type);
  if (orig_type != t)
    {
      TYPE_MAIN_VARIANT (new_type) = t;
      TYPE_NEXT_VARIANT (new_type) = TYPE_NEXT_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = new_type;
    }
  else
    {
      TYPE_MAIN_VARIANT (new_type) = new_type;
      TYPE_NEXT_VARIANT (new_type) = NULL;
    }

  TREE_TYPE (fndecl) = new_type;
  if (otypes)
    VEC_free (tree, heap, otypes);
  VEC_free (tree, heap, oparms);
}

/* Modify actual arguments of a function call CS as indicated in ADJUSTMENTS.
   If this is a directly recursive call, CS must be NULL.  Otherwise it must
   contain the corresponding call graph edge.  */

void
ipa_modify_call_arguments (struct cgraph_edge *cs, gimple stmt,
			   ipa_parm_adjustment_vec adjustments)
{
  VEC(tree, heap) *vargs;
  gimple new_stmt;
  gimple_stmt_iterator gsi;
  tree callee_decl;
  int i, len;

  len = VEC_length (ipa_parm_adjustment_t, adjustments);
  vargs = VEC_alloc (tree, heap, len);

  gsi = gsi_for_stmt (stmt);
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;

      adj = VEC_index (ipa_parm_adjustment_t, adjustments, i);

      if (adj->copy_param)
	{
	  tree arg = gimple_call_arg (stmt, adj->base_index);

	  VEC_quick_push (tree, vargs, arg);
	}
      else if (!adj->remove_param)
	{
	  tree expr, orig_expr;
	  bool allow_ptr, repl_found;

	  orig_expr = expr = gimple_call_arg (stmt, adj->base_index);
	  if (TREE_CODE (expr) == ADDR_EXPR)
	    {
	      allow_ptr = false;
	      expr = TREE_OPERAND (expr, 0);
	    }
	  else
	    allow_ptr = true;

	  repl_found = build_ref_for_offset (&expr, TREE_TYPE (expr),
					     adj->offset, adj->type,
					     allow_ptr);
	  if (repl_found)
	    {
	      if (adj->by_ref)
		expr = build_fold_addr_expr (expr);
	    }
	  else
	    {
	      tree ptrtype = build_pointer_type (adj->type);
	      expr = orig_expr;
	      if (!POINTER_TYPE_P (TREE_TYPE (expr)))
		expr = build_fold_addr_expr (expr);
	      if (!useless_type_conversion_p (ptrtype, TREE_TYPE (expr)))
		expr = fold_convert (ptrtype, expr);
	      expr = fold_build2 (POINTER_PLUS_EXPR, ptrtype, expr,
				  build_int_cst (size_type_node,
						 adj->offset / BITS_PER_UNIT));
	      if (!adj->by_ref)
		expr = fold_build1 (INDIRECT_REF, adj->type, expr);
	    }
	  expr = force_gimple_operand_gsi (&gsi, expr,
					   adj->by_ref
					   || is_gimple_reg_type (adj->type),
					   NULL, true, GSI_SAME_STMT);
	  VEC_quick_push (tree, vargs, expr);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "replacing stmt:");
      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, 0);
    }

  callee_decl = !cs ? gimple_call_fndecl (stmt) : cs->callee->decl;
  new_stmt = gimple_build_call_vec (callee_decl, vargs);
  VEC_free (tree, heap, vargs);
  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_block (new_stmt, gimple_block (stmt));
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "with stmt:");
      print_gimple_stmt (dump_file, new_stmt, 0, 0);
      fprintf (dump_file, "\n");
    }
  gsi_replace (&gsi, new_stmt, true);
  if (cs)
    cgraph_set_call_stmt (cs, new_stmt);
  update_ssa (TODO_update_ssa);
  free_dominance_info (CDI_DOMINATORS);
}

/* Return true iff BASE_INDEX is in ADJUSTMENTS more than once.  */

static bool
index_in_adjustments_multiple_times_p (int base_index,
				       ipa_parm_adjustment_vec adjustments)
{
  int i, len = VEC_length (ipa_parm_adjustment_t, adjustments);
  bool one = false;

  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      adj = VEC_index (ipa_parm_adjustment_t, adjustments, i);

      if (adj->base_index == base_index)
	{
	  if (one)
	    return true;
	  else
	    one = true;
	}
    }
  return false;
}


/* Return adjustments that should have the same effect on function parameters
   and call arguments as if they were first changed according to adjustments in
   INNER and then by adjustments in OUTER.  */

ipa_parm_adjustment_vec
ipa_combine_adjustments (ipa_parm_adjustment_vec inner,
			 ipa_parm_adjustment_vec outer)
{
  int i, outlen = VEC_length (ipa_parm_adjustment_t, outer);
  int inlen = VEC_length (ipa_parm_adjustment_t, inner);
  int removals = 0;
  ipa_parm_adjustment_vec adjustments, tmp;

  tmp = VEC_alloc (ipa_parm_adjustment_t, heap, inlen);
  for (i = 0; i < inlen; i++)
    {
      struct ipa_parm_adjustment *n;
      n = VEC_index (ipa_parm_adjustment_t, inner, i);

      if (n->remove_param)
	removals++;
      else
	VEC_quick_push (ipa_parm_adjustment_t, tmp, n);
    }

  adjustments = VEC_alloc (ipa_parm_adjustment_t, heap, outlen + removals);
  for (i = 0; i < outlen; i++)
    {
      struct ipa_parm_adjustment *r;
      struct ipa_parm_adjustment *out = VEC_index (ipa_parm_adjustment_t,
						   outer, i);
      struct ipa_parm_adjustment *in = VEC_index (ipa_parm_adjustment_t, tmp,
						  out->base_index);

      gcc_assert (!in->remove_param);
      if (out->remove_param)
	{
	  if (!index_in_adjustments_multiple_times_p (in->base_index, tmp))
	    {
	      r = VEC_quick_push (ipa_parm_adjustment_t, adjustments, NULL);
	      memset (r, 0, sizeof (*r));
	      r->remove_param = true;
	    }
	  continue;
	}

      r = VEC_quick_push (ipa_parm_adjustment_t, adjustments, NULL);
      memset (r, 0, sizeof (*r));
      r->base_index = in->base_index;
      r->type = out->type;

      /* FIXME:  Create nonlocal value too.  */

      if (in->copy_param && out->copy_param)
	r->copy_param = true;
      else if (in->copy_param)
	r->offset = out->offset;
      else if (out->copy_param)
	r->offset = in->offset;
      else
	r->offset = in->offset + out->offset;
    }

  for (i = 0; i < inlen; i++)
    {
      struct ipa_parm_adjustment *n = VEC_index (ipa_parm_adjustment_t,
						 inner, i);

      if (n->remove_param)
	VEC_quick_push (ipa_parm_adjustment_t, adjustments, n);
    }

  VEC_free (ipa_parm_adjustment_t, heap, tmp);
  return adjustments;
}

/* Dump the adjustments in the vector ADJUSTMENTS to dump_file in a human
   friendly way, assuming they are meant to be applied to FNDECL.  */

void
ipa_dump_param_adjustments (FILE *file, ipa_parm_adjustment_vec adjustments,
			    tree fndecl)
{
  int i, len = VEC_length (ipa_parm_adjustment_t, adjustments);
  bool first = true;
  VEC(tree, heap) *parms = ipa_get_vector_of_formal_parms (fndecl);

  fprintf (file, "IPA param adjustments: ");
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      adj = VEC_index (ipa_parm_adjustment_t, adjustments, i);

      if (!first)
	fprintf (file, "                 ");
      else
	first = false;

      fprintf (file, "%i. base_index: %i - ", i, adj->base_index);
      print_generic_expr (file, VEC_index (tree, parms, adj->base_index), 0);
      if (adj->base)
	{
	  fprintf (file, ", base: ");
	  print_generic_expr (file, adj->base, 0);
	}
      if (adj->reduction)
	{
	  fprintf (file, ", reduction: ");
	  print_generic_expr (file, adj->reduction, 0);
	}
      if (adj->new_ssa_base)
	{
	  fprintf (file, ", new_ssa_base: ");
	  print_generic_expr (file, adj->new_ssa_base, 0);
	}

      if (adj->copy_param)
	fprintf (file, ", copy_param");
      else if (adj->remove_param)
	fprintf (file, ", remove_param");
      else
	fprintf (file, ", offset %li", (long) adj->offset);
      if (adj->by_ref)
	fprintf (file, ", by_ref");
      print_node_brief (file, ", type: ", adj->type, 0);
      fprintf (file, "\n");
    }
  VEC_free (tree, heap, parms);
}

/* Stream out jump function JUMP_FUNC to OB.  */

static void
ipa_write_jump_function (struct output_block *ob,
			 struct ipa_jump_func *jump_func)
{
  lto_output_uleb128_stream (ob->main_stream,
			     jump_func->type);

  switch (jump_func->type)
    {
    case IPA_JF_UNKNOWN:
      break;
    case IPA_JF_CONST:
      lto_output_tree (ob, jump_func->value.constant, true);
      break;
    case IPA_JF_PASS_THROUGH:
      lto_output_tree (ob, jump_func->value.pass_through.operand, true);
      lto_output_uleb128_stream (ob->main_stream,
				 jump_func->value.pass_through.formal_id);
      lto_output_uleb128_stream (ob->main_stream,
				 jump_func->value.pass_through.operation);
      break;
    case IPA_JF_ANCESTOR:
      lto_output_uleb128_stream (ob->main_stream,
				 jump_func->value.ancestor.offset);
      lto_output_tree (ob, jump_func->value.ancestor.type, true);
      lto_output_uleb128_stream (ob->main_stream,
				 jump_func->value.ancestor.formal_id);
      break;
    case IPA_JF_CONST_MEMBER_PTR:
      lto_output_tree (ob, jump_func->value.member_cst.pfn, true);
      lto_output_tree (ob, jump_func->value.member_cst.delta, false);
      break;
    }
}

/* Read in jump function JUMP_FUNC from IB.  */

static void
ipa_read_jump_function (struct lto_input_block *ib,
			struct ipa_jump_func *jump_func,
			struct data_in *data_in)
{
  jump_func->type = (enum jump_func_type) lto_input_uleb128 (ib);

  switch (jump_func->type)
    {
    case IPA_JF_UNKNOWN:
      break;
    case IPA_JF_CONST:
      jump_func->value.constant = lto_input_tree (ib, data_in);
      break;
    case IPA_JF_PASS_THROUGH:
      jump_func->value.pass_through.operand = lto_input_tree (ib, data_in);
      jump_func->value.pass_through.formal_id = lto_input_uleb128 (ib);
      jump_func->value.pass_through.operation = (enum tree_code) lto_input_uleb128 (ib);
      break;
    case IPA_JF_ANCESTOR:
      jump_func->value.ancestor.offset = lto_input_uleb128 (ib);
      jump_func->value.ancestor.type = lto_input_tree (ib, data_in);
      jump_func->value.ancestor.formal_id = lto_input_uleb128 (ib);
      break;
    case IPA_JF_CONST_MEMBER_PTR:
      jump_func->value.member_cst.pfn = lto_input_tree (ib, data_in);
      jump_func->value.member_cst.delta = lto_input_tree (ib, data_in);
      break;
    }
}

/* Stream out a parameter call note.  */

static void
ipa_write_param_call_note (struct output_block *ob,
			   struct ipa_param_call_note *note)
{
  gcc_assert (!note->processed);
  lto_output_uleb128_stream (ob->main_stream, gimple_uid (note->stmt));
  lto_output_sleb128_stream (ob->main_stream, note->formal_id);
  lto_output_sleb128_stream (ob->main_stream, note->count);
  lto_output_sleb128_stream (ob->main_stream, note->frequency);
  lto_output_sleb128_stream (ob->main_stream, note->loop_nest);
}

/* Read in a parameter call note.  */

static void
ipa_read_param_call_note (struct lto_input_block *ib,
			  struct ipa_node_params *info)

{
  struct ipa_param_call_note *note = XCNEW (struct ipa_param_call_note);

  note->lto_stmt_uid = (unsigned int) lto_input_uleb128 (ib);
  note->formal_id = (int) lto_input_sleb128 (ib);
  note->count = (gcov_type) lto_input_sleb128 (ib);
  note->frequency = (int) lto_input_sleb128 (ib);
  note->loop_nest = (int) lto_input_sleb128 (ib);

  note->next = info->param_calls;
  info->param_calls = note;
}


/* Stream out NODE info to OB.  */

static void
ipa_write_node_info (struct output_block *ob, struct cgraph_node *node)
{
  int node_ref;
  lto_cgraph_encoder_t encoder;
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int j;
  struct cgraph_edge *e;
  struct bitpack_d *bp;
  int note_count = 0;
  struct ipa_param_call_note *note;

  encoder = ob->decl_state->cgraph_node_encoder;
  node_ref = lto_cgraph_encoder_encode (encoder, node);
  lto_output_uleb128_stream (ob->main_stream, node_ref);

  bp = bitpack_create ();
  bp_pack_value (bp, info->called_with_var_arguments, 1);
  bp_pack_value (bp, info->uses_analysis_done, 1);
  gcc_assert (info->modification_analysis_done
	      || ipa_get_param_count (info) == 0);
  gcc_assert (!info->node_enqueued);
  gcc_assert (!info->ipcp_orig_node);
  for (j = 0; j < ipa_get_param_count (info); j++)
    bp_pack_value (bp, info->params[j].modified, 1);
  lto_output_bitpack (ob->main_stream, bp);
  bitpack_delete (bp);
  for (e = node->callees; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);

      lto_output_uleb128_stream (ob->main_stream,
				 ipa_get_cs_argument_count (args));
      for (j = 0; j < ipa_get_cs_argument_count (args); j++)
	ipa_write_jump_function (ob, ipa_get_ith_jump_func (args, j));
    }

  for (note = info->param_calls; note; note = note->next)
    note_count++;
  lto_output_uleb128_stream (ob->main_stream, note_count);
  for (note = info->param_calls; note; note = note->next)
    ipa_write_param_call_note (ob, note);
}

/* Srtream in NODE info from IB.  */

static void
ipa_read_node_info (struct lto_input_block *ib, struct cgraph_node *node,
		    struct data_in *data_in)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int k;
  struct cgraph_edge *e;
  struct bitpack_d *bp;
  int i, note_count;

  ipa_initialize_node_params (node);

  bp = lto_input_bitpack (ib);
  info->called_with_var_arguments = bp_unpack_value (bp, 1);
  info->uses_analysis_done = bp_unpack_value (bp, 1);
  if (ipa_get_param_count (info) != 0)
    {
      info->modification_analysis_done = true;
      info->uses_analysis_done = true;
    }
  info->node_enqueued = false;
  for (k = 0; k < ipa_get_param_count (info); k++)
    info->params[k].modified = bp_unpack_value (bp, 1);
  bitpack_delete (bp);
  for (e = node->callees; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int count = lto_input_uleb128 (ib);

      ipa_set_cs_argument_count (args, count);
      if (!count)
	continue;

      args->jump_functions = GGC_CNEWVEC (struct ipa_jump_func,
				          ipa_get_cs_argument_count (args));
      for (k = 0; k < ipa_get_cs_argument_count (args); k++)
	ipa_read_jump_function (ib, ipa_get_ith_jump_func (args, k), data_in);
    }

  note_count = lto_input_uleb128 (ib);
  for (i = 0; i < note_count; i++)
    ipa_read_param_call_note (ib, info);
}

/* Write jump functions for nodes in SET.  */

void
ipa_prop_write_jump_functions (cgraph_node_set set)
{
  struct cgraph_node *node;
  struct output_block *ob = create_output_block (LTO_section_jump_functions);
  unsigned int count = 0;
  cgraph_node_set_iterator csi;

  ob->cgraph_node = NULL;

  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (node->analyzed && IPA_NODE_REF (node) != NULL)
	count++;
    }

  lto_output_uleb128_stream (ob->main_stream, count);

  /* Process all of the functions.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (node->analyzed && IPA_NODE_REF (node) != NULL)
        ipa_write_node_info (ob, node);
    }
  lto_output_1_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);
}

/* Read section in file FILE_DATA of length LEN with data DATA.  */

static void
ipa_prop_read_section (struct lto_file_decl_data *file_data, const char *data,
		       size_t len)
{
  const struct lto_function_header *header =
    (const struct lto_function_header *) data;
  const int32_t cfg_offset = sizeof (struct lto_function_header);
  const int32_t main_offset = cfg_offset + header->cfg_size;
  const int32_t string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  struct lto_input_block ib_main;
  unsigned int i;
  unsigned int count;

  LTO_INIT_INPUT_BLOCK (ib_main, (const char *) data + main_offset, 0,
			header->main_size);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, NULL);
  count = lto_input_uleb128 (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_cgraph_encoder_t encoder;

      index = lto_input_uleb128 (&ib_main);
      encoder = file_data->cgraph_node_encoder;
      node = lto_cgraph_encoder_deref (encoder, index);
      ipa_read_node_info (&ib_main, node, data_in);
    }
  lto_free_section_data (file_data, LTO_section_jump_functions, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Read ipcp jump functions.  */

void
ipa_prop_read_jump_functions (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  ipa_register_cgraph_hooks ();

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data = lto_get_section_data (file_data, LTO_section_jump_functions, NULL, &len);

      if (data)
        ipa_prop_read_section (file_data, data, len);
    }
}

/* After merging units, we can get mismatch in argument counts.
   Also decl merging might've rendered parameter lists obsolette.
   Also compute called_with_variable_arg info.  */

void
ipa_update_after_lto_read (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *cs;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      ipa_initialize_node_params (node);

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      for (cs = node->callees; cs; cs = cs->next_callee)
	{
	  if (ipa_get_cs_argument_count (IPA_EDGE_REF (cs))
	      != ipa_get_param_count (IPA_NODE_REF (cs->callee)))
	    ipa_set_called_with_variable_arg (IPA_NODE_REF (cs->callee));
	}
}

/* Walk param call notes of NODE and set their call statements given the uid
   stored in each note and STMTS which is an array of statements indexed by the
   uid.  */

void
lto_ipa_fixup_call_notes (struct cgraph_node *node, gimple *stmts)
{
  struct ipa_node_params *info;
  struct ipa_param_call_note *note;

  ipa_check_create_node_params ();
  info = IPA_NODE_REF (node);
  note = info->param_calls;
  /* If there are no notes or they have already been fixed up (the same fixup
     is called for both inlining and ipa-cp), there's nothing to do. */
  if (!note || note->stmt)
    return;

  do
    {
      note->stmt = stmts[note->lto_stmt_uid];
      note = note->next;
    }
  while (note);
}
