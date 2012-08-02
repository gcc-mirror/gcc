/* Interprocedural analyses.
   Copyright (C) 2005, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.

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
#include "gimple.h"
#include "flags.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "tree-streamer.h"


/* Intermediate information about a parameter that is only useful during the
   run of ipa_analyze_node and is not kept afterwards.  */

struct param_analysis_info
{
  bool modified;
  bitmap visited_statements;
};

/* Vector where the parameter infos are actually stored. */
VEC (ipa_node_params_t, heap) *ipa_node_params_vector;
/* Vector where the parameter infos are actually stored. */
VEC (ipa_edge_args_t, gc) *ipa_edge_args_vector;

/* Holders of ipa cgraph hooks: */
static struct cgraph_edge_hook_list *edge_removal_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;
static struct cgraph_2edge_hook_list *edge_duplication_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *function_insertion_hook_holder;

/* Return index of the formal whose tree is PTREE in function which corresponds
   to INFO.  */

int
ipa_get_param_decl_index (struct ipa_node_params *info, tree ptree)
{
  int i, count;

  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    if (ipa_get_param (info, i) == ptree)
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

  fndecl = node->symbol.decl;
  fnargs = DECL_ARGUMENTS (fndecl);
  param_num = 0;
  for (parm = fnargs; parm; parm = DECL_CHAIN (parm))
    {
      VEC_index (ipa_param_descriptor_t,
		 info->descriptors, param_num)->decl = parm;
      param_num++;
    }
}

/* Return how many formal parameters FNDECL has.  */

static inline int
count_formal_params (tree fndecl)
{
  tree parm;
  int count = 0;

  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    count++;

  return count;
}

/* Initialize the ipa_node_params structure associated with NODE by counting
   the function parameters, creating the descriptors and populating their
   param_decls.  */

void
ipa_initialize_node_params (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (!info->descriptors)
    {
      int param_count;

      param_count = count_formal_params (node->symbol.decl);
      if (param_count)
	{
	  VEC_safe_grow_cleared (ipa_param_descriptor_t, heap,
				 info->descriptors, param_count);
	  ipa_populate_param_decls (node, info);
	}
    }
}

/* Print the jump functions associated with call graph edge CS to file F.  */

static void
ipa_print_node_jump_functions_for_edge (FILE *f, struct cgraph_edge *cs)
{
  int i, count;

  count = ipa_get_cs_argument_count (IPA_EDGE_REF (cs));
  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *jump_func;
      enum jump_func_type type;

      jump_func = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
      type = jump_func->type;

      fprintf (f, "       param %d: ", i);
      if (type == IPA_JF_UNKNOWN)
	fprintf (f, "UNKNOWN\n");
      else if (type == IPA_JF_KNOWN_TYPE)
	{
	  fprintf (f, "KNOWN TYPE: base  ");
	  print_generic_expr (f, jump_func->value.known_type.base_type, 0);
	  fprintf (f, ", offset "HOST_WIDE_INT_PRINT_DEC", component ",
		   jump_func->value.known_type.offset);
	  print_generic_expr (f, jump_func->value.known_type.component_type, 0);
	  fprintf (f, "\n");
	}
      else if (type == IPA_JF_CONST)
	{
	  tree val = jump_func->value.constant;
	  fprintf (f, "CONST: ");
	  print_generic_expr (f, val, 0);
	  if (TREE_CODE (val) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (val, 0)) == CONST_DECL)
	    {
	      fprintf (f, " -> ");
	      print_generic_expr (f, DECL_INITIAL (TREE_OPERAND (val, 0)),
				  0);
	    }
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
	    print_generic_expr (f,
				jump_func->value.pass_through.operand, 0);
	  fprintf (f, "\n");
	}
      else if (type == IPA_JF_ANCESTOR)
	{
	  fprintf (f, "ANCESTOR: ");
	  fprintf (f, "%d, offset "HOST_WIDE_INT_PRINT_DEC", ",
		   jump_func->value.ancestor.formal_id,
		   jump_func->value.ancestor.offset);
	  print_generic_expr (f, jump_func->value.ancestor.type, 0);
	  fprintf (f, "\n");
	}
    }
}


/* Print the jump functions of all arguments on all call graph edges going from
   NODE to file F.  */

void
ipa_print_node_jump_functions (FILE *f, struct cgraph_node *node)
{
  struct cgraph_edge *cs;
  int i;

  fprintf (f, "  Jump functions of caller  %s:\n", cgraph_node_name (node));
  for (cs = node->callees; cs; cs = cs->next_callee)
    {
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	continue;

      fprintf (f, "    callsite  %s/%i -> %s/%i : \n",
	       xstrdup (cgraph_node_name (node)), node->uid,
	       xstrdup (cgraph_node_name (cs->callee)), cs->callee->uid);
      ipa_print_node_jump_functions_for_edge (f, cs);
    }

  for (cs = node->indirect_calls, i = 0; cs; cs = cs->next_callee, i++)
    {
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	continue;

      if (cs->call_stmt)
	{
	  fprintf (f, "    indirect callsite %d for stmt ", i);
	  print_gimple_stmt (f, cs->call_stmt, 0, TDF_SLIM);
	}
      else
	fprintf (f, "    indirect callsite %d :\n", i);
      ipa_print_node_jump_functions_for_edge (f, cs);

    }
}

/* Print ipa_jump_func data structures of all nodes in the call graph to F.  */

void
ipa_print_all_jump_functions (FILE *f)
{
  struct cgraph_node *node;

  fprintf (f, "\nJump functions:\n");
  FOR_EACH_FUNCTION (node)
    {
      ipa_print_node_jump_functions (f, node);
    }
}

/* Set JFUNC to be a known type jump function.  */

static void
ipa_set_jf_known_type (struct ipa_jump_func *jfunc, HOST_WIDE_INT offset,
		       tree base_type, tree component_type)
{
  jfunc->type = IPA_JF_KNOWN_TYPE;
  jfunc->value.known_type.offset = offset,
  jfunc->value.known_type.base_type = base_type;
  jfunc->value.known_type.component_type = component_type;
}

/* Set JFUNC to be a constant jmp function.  */

static void
ipa_set_jf_constant (struct ipa_jump_func *jfunc, tree constant)
{
  jfunc->type = IPA_JF_CONST;
  jfunc->value.constant = constant;
}

/* Set JFUNC to be a simple pass-through jump function.  */
static void
ipa_set_jf_simple_pass_through (struct ipa_jump_func *jfunc, int formal_id)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = NULL_TREE;
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = NOP_EXPR;
}

/* Set JFUNC to be an arithmetic pass through jump function.  */

static void
ipa_set_jf_arith_pass_through (struct ipa_jump_func *jfunc, int formal_id,
			       tree operand, enum tree_code operation)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = operand;
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = operation;
}

/* Set JFUNC to be an ancestor jump function.  */

static void
ipa_set_ancestor_jf (struct ipa_jump_func *jfunc, HOST_WIDE_INT offset,
		     tree type, int formal_id)
{
  jfunc->type = IPA_JF_ANCESTOR;
  jfunc->value.ancestor.formal_id = formal_id;
  jfunc->value.ancestor.offset = offset;
  jfunc->value.ancestor.type = type;
}

/* Simple function filling in a member pointer constant jump function (with PFN
   and DELTA as the constant value) into JFUNC.  */

static void
ipa_set_jf_member_ptr_cst (struct ipa_jump_func *jfunc,
			   tree pfn, tree delta)
{
  jfunc->type = IPA_JF_CONST_MEMBER_PTR;
  jfunc->value.member_cst.pfn = pfn;
  jfunc->value.member_cst.delta = delta;
}

/* Structure to be passed in between detect_type_change and
   check_stmt_for_type_change.  */

struct type_change_info
{
  /* Offset into the object where there is the virtual method pointer we are
     looking for.  */
  HOST_WIDE_INT offset;
  /* The declaration or SSA_NAME pointer of the base that we are checking for
     type change.  */
  tree object;
  /* If we actually can tell the type that the object has changed to, it is
     stored in this field.  Otherwise it remains NULL_TREE.  */
  tree known_current_type;
  /* Set to true if dynamic type change has been detected.  */
  bool type_maybe_changed;
  /* Set to true if multiple types have been encountered.  known_current_type
     must be disregarded in that case.  */
  bool multiple_types_encountered;
};

/* Return true if STMT can modify a virtual method table pointer.

   This function makes special assumptions about both constructors and
   destructors which are all the functions that are allowed to alter the VMT
   pointers.  It assumes that destructors begin with assignment into all VMT
   pointers and that constructors essentially look in the following way:

   1) The very first thing they do is that they call constructors of ancestor
   sub-objects that have them.

   2) Then VMT pointers of this and all its ancestors is set to new values
   corresponding to the type corresponding to the constructor.

   3) Only afterwards, other stuff such as constructor of member sub-objects
   and the code written by the user is run.  Only this may include calling
   virtual functions, directly or indirectly.

   There is no way to call a constructor of an ancestor sub-object in any
   other way.

   This means that we do not have to care whether constructors get the correct
   type information because they will always change it (in fact, if we define
   the type to be given by the VMT pointer, it is undefined).

   The most important fact to derive from the above is that if, for some
   statement in the section 3, we try to detect whether the dynamic type has
   changed, we can safely ignore all calls as we examine the function body
   backwards until we reach statements in section 2 because these calls cannot
   be ancestor constructors or destructors (if the input is not bogus) and so
   do not change the dynamic type (this holds true only for automatically
   allocated objects but at the moment we devirtualize only these).  We then
   must detect that statements in section 2 change the dynamic type and can try
   to derive the new type.  That is enough and we can stop, we will never see
   the calls into constructors of sub-objects in this code.  Therefore we can
   safely ignore all call statements that we traverse.
  */

static bool
stmt_may_be_vtbl_ptr_store (gimple stmt)
{
  if (is_gimple_call (stmt))
    return false;
  else if (is_gimple_assign (stmt))
    {
      tree lhs = gimple_assign_lhs (stmt);

      if (!AGGREGATE_TYPE_P (TREE_TYPE (lhs)))
	{
	  if (flag_strict_aliasing
	      && !POINTER_TYPE_P (TREE_TYPE (lhs)))
	    return false;

	  if (TREE_CODE (lhs) == COMPONENT_REF
	      && !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
	    return false;
	  /* In the future we might want to use get_base_ref_and_offset to find
	     if there is a field corresponding to the offset and if so, proceed
	     almost like if it was a component ref.  */
	}
    }
  return true;
}

/* If STMT can be proved to be an assignment to the virtual method table
   pointer of ANALYZED_OBJ and the type associated with the new table
   identified, return the type.  Otherwise return NULL_TREE.  */

static tree
extr_type_from_vtbl_ptr_store (gimple stmt, struct type_change_info *tci)
{
  HOST_WIDE_INT offset, size, max_size;
  tree lhs, rhs, base;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (lhs) != COMPONENT_REF
      || !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1))
      || TREE_CODE (rhs) != ADDR_EXPR)
    return NULL_TREE;
  rhs = get_base_address (TREE_OPERAND (rhs, 0));
  if (!rhs
      || TREE_CODE (rhs) != VAR_DECL
      || !DECL_VIRTUAL_P (rhs))
    return NULL_TREE;

  base = get_ref_base_and_extent (lhs, &offset, &size, &max_size);
  if (offset != tci->offset
      || size != POINTER_SIZE
      || max_size != POINTER_SIZE)
    return NULL_TREE;
  if (TREE_CODE (base) == MEM_REF)
    {
      if (TREE_CODE (tci->object) != MEM_REF
	  || TREE_OPERAND (tci->object, 0) != TREE_OPERAND (base, 0)
	  || !tree_int_cst_equal (TREE_OPERAND (tci->object, 1),
				  TREE_OPERAND (base, 1)))
	return NULL_TREE;
    }
  else if (tci->object != base)
    return NULL_TREE;

  return DECL_CONTEXT (rhs);
}

/* Callback of walk_aliased_vdefs and a helper function for
   detect_type_change to check whether a particular statement may modify
   the virtual table pointer, and if possible also determine the new type of
   the (sub-)object.  It stores its result into DATA, which points to a
   type_change_info structure.  */

static bool
check_stmt_for_type_change (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef, void *data)
{
  gimple stmt = SSA_NAME_DEF_STMT (vdef);
  struct type_change_info *tci = (struct type_change_info *) data;

  if (stmt_may_be_vtbl_ptr_store (stmt))
    {
      tree type;
      type = extr_type_from_vtbl_ptr_store (stmt, tci);
      if (tci->type_maybe_changed
	  && type != tci->known_current_type)
	tci->multiple_types_encountered = true;
      tci->known_current_type = type;
      tci->type_maybe_changed = true;
      return true;
    }
  else
    return false;
}



/* Like detect_type_change but with extra argument COMP_TYPE which will become
   the component type part of new JFUNC of dynamic type change is detected and
   the new base type is identified.  */

static bool
detect_type_change_1 (tree arg, tree base, tree comp_type, gimple call,
		      struct ipa_jump_func *jfunc, HOST_WIDE_INT offset)
{
  struct type_change_info tci;
  ao_ref ao;

  gcc_checking_assert (DECL_P (arg)
		       || TREE_CODE (arg) == MEM_REF
		       || handled_component_p (arg));
  /* Const calls cannot call virtual methods through VMT and so type changes do
     not matter.  */
  if (!flag_devirtualize || !gimple_vuse (call))
    return false;

  ao_ref_init (&ao, arg);
  ao.base = base;
  ao.offset = offset;
  ao.size = POINTER_SIZE;
  ao.max_size = ao.size;

  tci.offset = offset;
  tci.object = get_base_address (arg);
  tci.known_current_type = NULL_TREE;
  tci.type_maybe_changed = false;
  tci.multiple_types_encountered = false;

  walk_aliased_vdefs (&ao, gimple_vuse (call), check_stmt_for_type_change,
		      &tci, NULL);
  if (!tci.type_maybe_changed)
    return false;

  if (!tci.known_current_type
      || tci.multiple_types_encountered
      || offset != 0)
    jfunc->type = IPA_JF_UNKNOWN;
  else
    ipa_set_jf_known_type (jfunc, 0, tci.known_current_type, comp_type);

  return true;
}

/* Detect whether the dynamic type of ARG has changed (before callsite CALL) by
   looking for assignments to its virtual table pointer.  If it is, return true
   and fill in the jump function JFUNC with relevant type information or set it
   to unknown.  ARG is the object itself (not a pointer to it, unless
   dereferenced).  BASE is the base of the memory access as returned by
   get_ref_base_and_extent, as is the offset.  */

static bool
detect_type_change (tree arg, tree base, gimple call,
		    struct ipa_jump_func *jfunc, HOST_WIDE_INT offset)
{
  return detect_type_change_1 (arg, base, TREE_TYPE (arg), call, jfunc, offset);
}

/* Like detect_type_change but ARG is supposed to be a non-dereferenced pointer
   SSA name (its dereference will become the base and the offset is assumed to
   be zero).  */

static bool
detect_type_change_ssa (tree arg, gimple call, struct ipa_jump_func *jfunc)
{
  tree comp_type;

  gcc_checking_assert (TREE_CODE (arg) == SSA_NAME);
  if (!flag_devirtualize
      || !POINTER_TYPE_P (TREE_TYPE (arg))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (arg))) != RECORD_TYPE)
    return false;

  comp_type = TREE_TYPE (TREE_TYPE (arg));
  arg = build2 (MEM_REF, ptr_type_node, arg,
		build_int_cst (ptr_type_node, 0));

  return detect_type_change_1 (arg, arg, comp_type, call, jfunc, 0);
}

/* Callback of walk_aliased_vdefs.  Flags that it has been invoked to the
   boolean variable pointed to by DATA.  */

static bool
mark_modified (ao_ref *ao ATTRIBUTE_UNUSED, tree vdef ATTRIBUTE_UNUSED,
		     void *data)
{
  bool *b = (bool *) data;
  *b = true;
  return true;
}

/* Return true if the formal parameter PARM might have been modified in this
   function before reaching the statement STMT.  PARM_AINFO is a pointer to a
   structure containing temporary information about PARM.  */

static bool
is_parm_modified_before_stmt (struct param_analysis_info *parm_ainfo,
			      gimple stmt, tree parm)
{
  bool modified = false;
  ao_ref refd;

  if (parm_ainfo->modified)
    return true;

  gcc_checking_assert (gimple_vuse (stmt) != NULL_TREE);
  ao_ref_init (&refd, parm);
  walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified,
		      &modified, &parm_ainfo->visited_statements);
  if (modified)
    {
      parm_ainfo->modified = true;
      return true;
    }
  return false;
}

/* If STMT is an assignment that loads a value from an parameter declaration,
   return the index of the parameter in ipa_node_params which has not been
   modified.  Otherwise return -1.  */

static int
load_from_unmodified_param (struct ipa_node_params *info,
			    struct param_analysis_info *parms_ainfo,
			    gimple stmt)
{
  int index;
  tree op1;

  if (!gimple_assign_single_p (stmt))
    return -1;

  op1 = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (op1) != PARM_DECL)
    return -1;

  index = ipa_get_param_decl_index (info, op1);
  if (index < 0
      || is_parm_modified_before_stmt (&parms_ainfo[index], stmt, op1))
    return -1;

  return index;
}

/* Given that an actual argument is an SSA_NAME (given in NAME) and is a result
   of an assignment statement STMT, try to determine whether we are actually
   handling any of the following cases and construct an appropriate jump
   function into JFUNC if so:

   1) The passed value is loaded from a formal parameter which is not a gimple
   register (most probably because it is addressable, the value has to be
   scalar) and we can guarantee the value has not changed.  This case can
   therefore be described by a simple pass-through jump function.  For example:

      foo (int a)
      {
        int a.0;

        a.0_2 = a;
        bar (a.0_2);

   2) The passed value can be described by a simple arithmetic pass-through
   jump function. E.g.

      foo (int a)
      {
        int D.2064;

        D.2064_4 = a.1(D) + 4;
        bar (D.2064_4);

   This case can also occur in combination of the previous one, e.g.:

      foo (int a, int z)
      {
        int a.0;
        int D.2064;

	a.0_3 = a;
	D.2064_4 = a.0_3 + 4;
	foo (D.2064_4);

   3) The passed value is an address of an object within another one (which
   also passed by reference).  Such situations are described by an ancestor
   jump function and describe situations such as:

     B::foo() (struct B * const this)
     {
       struct A * D.1845;

       D.1845_2 = &this_1(D)->D.1748;
       A::bar (D.1845_2);

   INFO is the structure describing individual parameters access different
   stages of IPA optimizations.  PARMS_AINFO contains the information that is
   only needed for intraprocedural analysis.  */

static void
compute_complex_assign_jump_func (struct ipa_node_params *info,
				  struct param_analysis_info *parms_ainfo,
				  struct ipa_jump_func *jfunc,
				  gimple call, gimple stmt, tree name)
{
  HOST_WIDE_INT offset, size, max_size;
  tree op1, tc_ssa, base, ssa;
  int index;

  op1 = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (op1) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (op1))
	index = ipa_get_param_decl_index (info, SSA_NAME_VAR (op1));
      else
	index = load_from_unmodified_param (info, parms_ainfo,
					    SSA_NAME_DEF_STMT (op1));
      tc_ssa = op1;
    }
  else
    {
      index = load_from_unmodified_param (info, parms_ainfo, stmt);
      tc_ssa = gimple_assign_lhs (stmt);
    }

  if (index >= 0)
    {
      tree op2 = gimple_assign_rhs2 (stmt);

      if (op2)
	{
	  if (!is_gimple_ip_invariant (op2)
	      || (TREE_CODE_CLASS (gimple_expr_code (stmt)) != tcc_comparison
		  && !useless_type_conversion_p (TREE_TYPE (name),
						 TREE_TYPE (op1))))
	    return;

	  ipa_set_jf_arith_pass_through (jfunc, index, op2,
					 gimple_assign_rhs_code (stmt));
	}
      else if (gimple_assign_single_p (stmt)
	       && !detect_type_change_ssa (tc_ssa, call, jfunc))
	ipa_set_jf_simple_pass_through (jfunc, index);
      return;
    }

  if (TREE_CODE (op1) != ADDR_EXPR)
    return;
  op1 = TREE_OPERAND (op1, 0);
  if (TREE_CODE (TREE_TYPE (op1)) != RECORD_TYPE)
    return;
  base = get_ref_base_and_extent (op1, &offset, &size, &max_size);
  if (TREE_CODE (base) != MEM_REF
      /* If this is a varying address, punt.  */
      || max_size == -1
      || max_size != size)
    return;
  offset += mem_ref_offset (base).low * BITS_PER_UNIT;
  ssa = TREE_OPERAND (base, 0);
  if (TREE_CODE (ssa) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (ssa)
      || offset < 0)
    return;

  /* Dynamic types are changed only in constructors and destructors and  */
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (ssa));
  if (index >= 0
      && !detect_type_change (op1, base, call, jfunc, offset))
    ipa_set_ancestor_jf (jfunc, offset, TREE_TYPE (op1), index);
}

/* Extract the base, offset and MEM_REF expression from a statement ASSIGN if
   it looks like:

   iftmp.1_3 = &obj_2(D)->D.1762;

   The base of the MEM_REF must be a default definition SSA NAME of a
   parameter.  Return NULL_TREE if it looks otherwise.  If case of success, the
   whole MEM_REF expression is returned and the offset calculated from any
   handled components and the MEM_REF itself is stored into *OFFSET.  The whole
   RHS stripped off the ADDR_EXPR is stored into *OBJ_P.  */

static tree
get_ancestor_addr_info (gimple assign, tree *obj_p, HOST_WIDE_INT *offset)
{
  HOST_WIDE_INT size, max_size;
  tree expr, parm, obj;

  if (!gimple_assign_single_p (assign))
    return NULL_TREE;
  expr = gimple_assign_rhs1 (assign);

  if (TREE_CODE (expr) != ADDR_EXPR)
    return NULL_TREE;
  expr = TREE_OPERAND (expr, 0);
  obj = expr;
  expr = get_ref_base_and_extent (expr, offset, &size, &max_size);

  if (TREE_CODE (expr) != MEM_REF
      /* If this is a varying address, punt.  */
      || max_size == -1
      || max_size != size
      || *offset < 0)
    return NULL_TREE;
  parm = TREE_OPERAND (expr, 0);
  if (TREE_CODE (parm) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (parm)
      || TREE_CODE (SSA_NAME_VAR (parm)) != PARM_DECL)
    return NULL_TREE;

  *offset += mem_ref_offset (expr).low * BITS_PER_UNIT;
  *obj_p = obj;
  return expr;
}


/* Given that an actual argument is an SSA_NAME that is a result of a phi
   statement PHI, try to find out whether NAME is in fact a
   multiple-inheritance typecast from a descendant into an ancestor of a formal
   parameter and thus can be described by an ancestor jump function and if so,
   write the appropriate function into JFUNC.

   Essentially we want to match the following pattern:

     if (obj_2(D) != 0B)
       goto <bb 3>;
     else
       goto <bb 4>;

   <bb 3>:
     iftmp.1_3 = &obj_2(D)->D.1762;

   <bb 4>:
     # iftmp.1_1 = PHI <iftmp.1_3(3), 0B(2)>
     D.1879_6 = middleman_1 (iftmp.1_1, i_5(D));
     return D.1879_6;  */

static void
compute_complex_ancestor_jump_func (struct ipa_node_params *info,
				    struct ipa_jump_func *jfunc,
				    gimple call, gimple phi)
{
  HOST_WIDE_INT offset;
  gimple assign, cond;
  basic_block phi_bb, assign_bb, cond_bb;
  tree tmp, parm, expr, obj;
  int index, i;

  if (gimple_phi_num_args (phi) != 2)
    return;

  if (integer_zerop (PHI_ARG_DEF (phi, 1)))
    tmp = PHI_ARG_DEF (phi, 0);
  else if (integer_zerop (PHI_ARG_DEF (phi, 0)))
    tmp = PHI_ARG_DEF (phi, 1);
  else
    return;
  if (TREE_CODE (tmp) != SSA_NAME
      || SSA_NAME_IS_DEFAULT_DEF (tmp)
      || !POINTER_TYPE_P (TREE_TYPE (tmp))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (tmp))) != RECORD_TYPE)
    return;

  assign = SSA_NAME_DEF_STMT (tmp);
  assign_bb = gimple_bb (assign);
  if (!single_pred_p (assign_bb))
    return;
  expr = get_ancestor_addr_info (assign, &obj, &offset);
  if (!expr)
    return;
  parm = TREE_OPERAND (expr, 0);
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (parm));
  gcc_assert (index >= 0);

  cond_bb = single_pred (assign_bb);
  cond = last_stmt (cond_bb);
  if (!cond
      || gimple_code (cond) != GIMPLE_COND
      || gimple_cond_code (cond) != NE_EXPR
      || gimple_cond_lhs (cond) != parm
      || !integer_zerop (gimple_cond_rhs (cond)))
    return;

  phi_bb = gimple_bb (phi);
  for (i = 0; i < 2; i++)
    {
      basic_block pred = EDGE_PRED (phi_bb, i)->src;
      if (pred != assign_bb && pred != cond_bb)
	return;
    }

  if (!detect_type_change (obj, expr, call, jfunc, offset))
    ipa_set_ancestor_jf (jfunc, offset, TREE_TYPE (obj), index);
}

/* Given OP which is passed as an actual argument to a called function,
   determine if it is possible to construct a KNOWN_TYPE jump function for it
   and if so, create one and store it to JFUNC.  */

static void
compute_known_type_jump_func (tree op, struct ipa_jump_func *jfunc,
			      gimple call)
{
  HOST_WIDE_INT offset, size, max_size;
  tree base;

  if (!flag_devirtualize
      || TREE_CODE (op) != ADDR_EXPR
      || TREE_CODE (TREE_TYPE (TREE_TYPE (op))) != RECORD_TYPE)
    return;

  op = TREE_OPERAND (op, 0);
  base = get_ref_base_and_extent (op, &offset, &size, &max_size);
  if (!DECL_P (base)
      || max_size == -1
      || max_size != size
      || TREE_CODE (TREE_TYPE (base)) != RECORD_TYPE
      || is_global_var (base))
    return;

  if (!TYPE_BINFO (TREE_TYPE (base))
      || detect_type_change (op, base, call, jfunc, offset))
    return;

  ipa_set_jf_known_type (jfunc, offset, TREE_TYPE (base), TREE_TYPE (op));
}


/* Determine the jump functions of scalar arguments.  Scalar means SSA names
   and constants of a number of selected types.  INFO is the ipa_node_params
   structure associated with the caller, PARMS_AINFO describes state of
   analysis with respect to individual formal parameters.  ARGS is the
   ipa_edge_args structure describing the callsite CALL which is the call
   statement being examined.*/

static void
compute_scalar_jump_functions (struct ipa_node_params *info,
			       struct param_analysis_info *parms_ainfo,
			       struct ipa_edge_args *args,
			       gimple call)
{
  tree arg;
  unsigned num = 0;

  for (num = 0; num < gimple_call_num_args (call); num++)
    {
      struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, num);
      arg = gimple_call_arg (call, num);

      if (is_gimple_ip_invariant (arg))
	ipa_set_jf_constant (jfunc, arg);
      else if (TREE_CODE (arg) == SSA_NAME)
	{
	  if (SSA_NAME_IS_DEFAULT_DEF (arg))
	    {
	      int index = ipa_get_param_decl_index (info, SSA_NAME_VAR (arg));

	      if (index >= 0
		  && !detect_type_change_ssa (arg, call, jfunc))
		ipa_set_jf_simple_pass_through (jfunc, index);
	    }
	  else
	    {
	      gimple stmt = SSA_NAME_DEF_STMT (arg);
	      if (is_gimple_assign (stmt))
		compute_complex_assign_jump_func (info, parms_ainfo, jfunc,
						  call, stmt, arg);
	      else if (gimple_code (stmt) == GIMPLE_PHI)
		compute_complex_ancestor_jump_func (info, jfunc, call, stmt);
	    }
	}
      else
	compute_known_type_jump_func (arg, jfunc, call);
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

  fld = DECL_CHAIN (fld);
  if (!fld || INTEGRAL_TYPE_P (fld))
    return false;
  if (delta)
    *delta = fld;

  if (DECL_CHAIN (fld))
    return false;

  return true;
}

/* Go through arguments of the CALL and for every one that looks like a member
   pointer, check whether it can be safely declared pass-through and if so,
   mark that to the corresponding item of jump FUNCTIONS.  Return true iff
   there are non-pass-through member pointers within the arguments.  INFO
   describes formal parameters of the caller.  PARMS_INFO is a pointer to a
   vector containing intermediate information about each formal parameter.  */

static bool
compute_pass_through_member_ptrs (struct ipa_node_params *info,
				  struct param_analysis_info *parms_ainfo,
				  struct ipa_edge_args *args,
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
	      if (!is_parm_modified_before_stmt (&parms_ainfo[index], call,
						 arg))
		{
		  struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args,
								       num);
		  ipa_set_jf_simple_pass_through (jfunc, index);
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

/* If RHS is an SSA_NAME and it is defined by a simple copy assign statement,
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

      if (!stmt_may_clobber_ref_p (stmt, arg))
	continue;
      if (!gimple_assign_single_p (stmt))
	return;

      lhs = gimple_assign_lhs (stmt);
      rhs = gimple_assign_rhs1 (stmt);

      if (TREE_CODE (lhs) != COMPONENT_REF
	  || TREE_OPERAND (lhs, 0) != arg)
	return;

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
		  ipa_set_jf_member_ptr_cst (jfunc, rhs, delta);
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
		  ipa_set_jf_member_ptr_cst (jfunc, rhs, delta);
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
compute_cst_member_ptr_arguments (struct ipa_edge_args *args,
				  gimple call)
{
  unsigned num;
  tree arg, method_field, delta_field;

  for (num = 0; num < gimple_call_num_args (call); num++)
    {
      struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, num);
      arg = gimple_call_arg (call, num);

      if (jfunc->type == IPA_JF_UNKNOWN
	  && type_like_member_ptr_p (TREE_TYPE (arg), &method_field,
				     &delta_field))
	determine_cst_member_ptr (call, arg, method_field, delta_field, jfunc);
    }
}

/* Compute jump function for all arguments of callsite CS and insert the
   information in the jump_functions array in the ipa_edge_args corresponding
   to this callsite.  */

static void
ipa_compute_jump_functions_for_edge (struct param_analysis_info *parms_ainfo,
				     struct cgraph_edge *cs)
{
  struct ipa_node_params *info = IPA_NODE_REF (cs->caller);
  struct ipa_edge_args *args = IPA_EDGE_REF (cs);
  gimple call = cs->call_stmt;
  int arg_num = gimple_call_num_args (call);

  if (arg_num == 0 || args->jump_functions)
    return;
  VEC_safe_grow_cleared (ipa_jump_func_t, gc, args->jump_functions, arg_num);

  /* We will deal with constants and SSA scalars first:  */
  compute_scalar_jump_functions (info, parms_ainfo, args, call);

  /* Let's check whether there are any potential member pointers and if so,
     whether we can determine their functions as pass_through.  */
  if (!compute_pass_through_member_ptrs (info, parms_ainfo, args, call))
    return;

  /* Finally, let's check whether we actually pass a new constant member
     pointer here...  */
  compute_cst_member_ptr_arguments (args, call);
}

/* Compute jump functions for all edges - both direct and indirect - outgoing
   from NODE.  Also count the actual arguments in the process.  */

static void
ipa_compute_jump_functions (struct cgraph_node *node,
			    struct param_analysis_info *parms_ainfo)
{
  struct cgraph_edge *cs;

  for (cs = node->callees; cs; cs = cs->next_callee)
    {
      struct cgraph_node *callee = cgraph_function_or_thunk_node (cs->callee,
								  NULL);
      /* We do not need to bother analyzing calls to unknown
	 functions unless they may become known during lto/whopr.  */
      if (!callee->analyzed && !flag_lto)
	continue;
      ipa_compute_jump_functions_for_edge (parms_ainfo, cs);
    }

  for (cs = node->indirect_calls; cs; cs = cs->next_callee)
    ipa_compute_jump_functions_for_edge (parms_ainfo, cs);
}

/* If RHS looks like a rhs of a statement loading pfn from a member
   pointer formal parameter, return the parameter, otherwise return
   NULL.  If USE_DELTA, then we look for a use of the delta field
   rather than the pfn.  */

static tree
ipa_get_member_ptr_load_param (tree rhs, bool use_delta)
{
  tree rec, ref_field, ref_offset, fld, fld_offset, ptr_field, delta_field;

  if (TREE_CODE (rhs) == COMPONENT_REF)
    {
      ref_field = TREE_OPERAND (rhs, 1);
      rhs = TREE_OPERAND (rhs, 0);
    }
  else
    ref_field = NULL_TREE;
  if (TREE_CODE (rhs) != MEM_REF)
    return NULL_TREE;
  rec = TREE_OPERAND (rhs, 0);
  if (TREE_CODE (rec) != ADDR_EXPR)
    return NULL_TREE;
  rec = TREE_OPERAND (rec, 0);
  if (TREE_CODE (rec) != PARM_DECL
      || !type_like_member_ptr_p (TREE_TYPE (rec), &ptr_field, &delta_field))
    return NULL_TREE;

  ref_offset = TREE_OPERAND (rhs, 1);

  if (ref_field)
    {
      if (integer_nonzerop (ref_offset))
	return NULL_TREE;

      if (use_delta)
	fld = delta_field;
      else
	fld = ptr_field;

      return ref_field == fld ? rec : NULL_TREE;
    }

  if (use_delta)
    fld_offset = byte_position (delta_field);
  else
    fld_offset = byte_position (ptr_field);

  return tree_int_cst_equal (ref_offset, fld_offset) ? rec : NULL_TREE;
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

/* Find the indirect call graph edge corresponding to STMT and mark it as a
   call to a parameter number PARAM_INDEX.  NODE is the caller.  Return the
   indirect call graph edge.  */

static struct cgraph_edge *
ipa_note_param_call (struct cgraph_node *node, int param_index, gimple stmt)
{
  struct cgraph_edge *cs;

  cs = cgraph_edge (node, stmt);
  cs->indirect_info->param_index = param_index;
  cs->indirect_info->anc_offset = 0;
  cs->indirect_info->polymorphic = 0;
  return cs;
}

/* Analyze the CALL and examine uses of formal parameters of the caller NODE
   (described by INFO).  PARMS_AINFO is a pointer to a vector containing
   intermediate information about each formal parameter.  Currently it checks
   whether the call calls a pointer that is a formal parameter and if so, the
   parameter is marked with the called flag and an indirect call graph edge
   describing the call is created.  This is very simple for ordinary pointers
   represented in SSA but not-so-nice when it comes to member pointers.  The
   ugly part of this function does nothing more than trying to match the
   pattern of such a call.  An example of such a pattern is the gimple dump
   below, the call is on the last line:

     <bb 2>:
       f$__delta_5 = f.__delta;
       f$__pfn_24 = f.__pfn;

   or
     <bb 2>:
       f$__delta_5 = MEM[(struct  *)&f];
       f$__pfn_24 = MEM[(struct  *)&f + 4B];

   and a few lines below:

     <bb 5>
       D.2496_3 = (int) f$__pfn_24;
       D.2497_4 = D.2496_3 & 1;
       if (D.2497_4 != 0)
         goto <bb 3>;
       else
         goto <bb 4>;

     <bb 6>:
       D.2500_7 = (unsigned int) f$__delta_5;
       D.2501_8 = &S + D.2500_7;
       D.2502_9 = (int (*__vtbl_ptr_type) (void) * *) D.2501_8;
       D.2503_10 = *D.2502_9;
       D.2504_12 = f$__pfn_24 + -1;
       D.2505_13 = (unsigned int) D.2504_12;
       D.2506_14 = D.2503_10 + D.2505_13;
       D.2507_15 = *D.2506_14;
       iftmp.11_16 = (String:: *) D.2507_15;

     <bb 7>:
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
ipa_analyze_indirect_call_uses (struct cgraph_node *node,
				struct ipa_node_params *info,
				struct param_analysis_info *parms_ainfo,
				gimple call, tree target)
{
  gimple def;
  tree n1, n2;
  gimple d1, d2;
  tree rec, rec2, cond;
  gimple branch;
  int index;
  basic_block bb, virt_bb, join;

  if (SSA_NAME_IS_DEFAULT_DEF (target))
    {
      tree var = SSA_NAME_VAR (target);
      index = ipa_get_param_decl_index (info, var);
      if (index >= 0)
	ipa_note_param_call (node, index, call);
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

  join = gimple_bb (def);
  if ((rec = ipa_get_stmt_member_ptr_load_param (d1, false)))
    {
      if (ipa_get_stmt_member_ptr_load_param (d2, false))
	return;

      bb = EDGE_PRED (join, 0)->src;
      virt_bb = gimple_bb (d2);
    }
  else if ((rec = ipa_get_stmt_member_ptr_load_param (d2, false)))
    {
      bb = EDGE_PRED (join, 1)->src;
      virt_bb = gimple_bb (d1);
    }
  else
    return;

  /* Second, we need to check that the basic blocks are laid out in the way
     corresponding to the pattern. */

  if (!single_pred_p (virt_bb) || !single_succ_p (virt_bb)
      || single_pred (virt_bb) != bb
      || single_succ (virt_bb) != join)
    return;

  /* Third, let's see that the branching is done depending on the least
     significant bit of the pfn. */

  branch = last_stmt (bb);
  if (!branch || gimple_code (branch) != GIMPLE_COND)
    return;

  if ((gimple_cond_code (branch) != NE_EXPR
       && gimple_cond_code (branch) != EQ_EXPR)
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
  if (index >= 0 && !is_parm_modified_before_stmt (&parms_ainfo[index],
						   call, rec))
    ipa_note_param_call (node, index, call);

  return;
}

/* Analyze a CALL to an OBJ_TYPE_REF which is passed in TARGET and if the
   object referenced in the expression is a formal parameter of the caller
   (described by INFO), create a call note for the statement. */

static void
ipa_analyze_virtual_call_uses (struct cgraph_node *node,
			       struct ipa_node_params *info, gimple call,
			       tree target)
{
  struct cgraph_edge *cs;
  struct cgraph_indirect_call_info *ii;
  struct ipa_jump_func jfunc;
  tree obj = OBJ_TYPE_REF_OBJECT (target);
  int index;
  HOST_WIDE_INT anc_offset;

  if (!flag_devirtualize)
    return;

  if (TREE_CODE (obj) != SSA_NAME)
    return;

  if (SSA_NAME_IS_DEFAULT_DEF (obj))
    {
      if (TREE_CODE (SSA_NAME_VAR (obj)) != PARM_DECL)
	return;

      anc_offset = 0;
      index = ipa_get_param_decl_index (info, SSA_NAME_VAR (obj));
      gcc_assert (index >= 0);
      if (detect_type_change_ssa (obj, call, &jfunc))
	return;
    }
  else
    {
      gimple stmt = SSA_NAME_DEF_STMT (obj);
      tree expr;

      expr = get_ancestor_addr_info (stmt, &obj, &anc_offset);
      if (!expr)
	return;
      index = ipa_get_param_decl_index (info,
					SSA_NAME_VAR (TREE_OPERAND (expr, 0)));
      gcc_assert (index >= 0);
      if (detect_type_change (obj, expr, call, &jfunc, anc_offset))
	return;
    }

  cs = ipa_note_param_call (node, index, call);
  ii = cs->indirect_info;
  ii->anc_offset = anc_offset;
  ii->otr_token = tree_low_cst (OBJ_TYPE_REF_TOKEN (target), 1);
  ii->otr_type = TREE_TYPE (TREE_TYPE (OBJ_TYPE_REF_OBJECT (target)));
  ii->polymorphic = 1;
}

/* Analyze a call statement CALL whether and how it utilizes formal parameters
   of the caller (described by INFO).  PARMS_AINFO is a pointer to a vector
   containing intermediate information about each formal parameter.  */

static void
ipa_analyze_call_uses (struct cgraph_node *node,
		       struct ipa_node_params *info,
		       struct param_analysis_info *parms_ainfo, gimple call)
{
  tree target = gimple_call_fn (call);

  if (!target)
    return;
  if (TREE_CODE (target) == SSA_NAME)
    ipa_analyze_indirect_call_uses (node, info, parms_ainfo, call, target);
  else if (TREE_CODE (target) == OBJ_TYPE_REF)
    ipa_analyze_virtual_call_uses (node, info, call, target);
}


/* Analyze the call statement STMT with respect to formal parameters (described
   in INFO) of caller given by NODE.  Currently it only checks whether formal
   parameters are called.  PARMS_AINFO is a pointer to a vector containing
   intermediate information about each formal parameter.  */

static void
ipa_analyze_stmt_uses (struct cgraph_node *node, struct ipa_node_params *info,
		       struct param_analysis_info *parms_ainfo, gimple stmt)
{
  if (is_gimple_call (stmt))
    ipa_analyze_call_uses (node, info, parms_ainfo, stmt);
}

/* Callback of walk_stmt_load_store_addr_ops for the visit_load.
   If OP is a parameter declaration, mark it as used in the info structure
   passed in DATA.  */

static bool
visit_ref_for_mod_analysis (gimple stmt ATTRIBUTE_UNUSED,
			     tree op, void *data)
{
  struct ipa_node_params *info = (struct ipa_node_params *) data;

  op = get_base_address (op);
  if (op
      && TREE_CODE (op) == PARM_DECL)
    {
      int index = ipa_get_param_decl_index (info, op);
      gcc_assert (index >= 0);
      ipa_set_param_used (info, index, true);
    }

  return false;
}

/* Scan the function body of NODE and inspect the uses of formal parameters.
   Store the findings in various structures of the associated ipa_node_params
   structure, such as parameter flags, notes etc.  PARMS_AINFO is a pointer to a
   vector containing intermediate information about each formal parameter.   */

static void
ipa_analyze_params_uses (struct cgraph_node *node,
			 struct param_analysis_info *parms_ainfo)
{
  tree decl = node->symbol.decl;
  basic_block bb;
  struct function *func;
  gimple_stmt_iterator gsi;
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i;

  if (ipa_get_param_count (info) == 0 || info->uses_analysis_done)
    return;

  for (i = 0; i < ipa_get_param_count (info); i++)
    {
      tree parm = ipa_get_param (info, i);
      tree ddef;
      /* For SSA regs see if parameter is used.  For non-SSA we compute
	 the flag during modification analysis.  */
      if (is_gimple_reg (parm)
	  && (ddef = ssa_default_def (DECL_STRUCT_FUNCTION (node->symbol.decl),
				      parm)) != NULL_TREE
	  && !has_zero_uses (ddef))
	ipa_set_param_used (info, i, true);
    }

  func = DECL_STRUCT_FUNCTION (decl);
  FOR_EACH_BB_FN (bb, func)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);

	  if (is_gimple_debug (stmt))
	    continue;

	  ipa_analyze_stmt_uses (node, info, parms_ainfo, stmt);
	  walk_stmt_load_store_addr_ops (stmt, info,
					 visit_ref_for_mod_analysis,
					 visit_ref_for_mod_analysis,
					 visit_ref_for_mod_analysis);
	}
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	walk_stmt_load_store_addr_ops (gsi_stmt (gsi), info,
				       visit_ref_for_mod_analysis,
				       visit_ref_for_mod_analysis,
				       visit_ref_for_mod_analysis);
    }

  info->uses_analysis_done = 1;
}

/* Initialize the array describing properties of of formal parameters
   of NODE, analyze their uses and compute jump functions associated
   with actual arguments of calls from within NODE.  */

void
ipa_analyze_node (struct cgraph_node *node)
{
  struct ipa_node_params *info;
  struct param_analysis_info *parms_ainfo;
  int i, param_count;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  info = IPA_NODE_REF (node);
  push_cfun (DECL_STRUCT_FUNCTION (node->symbol.decl));
  current_function_decl = node->symbol.decl;
  ipa_initialize_node_params (node);

  param_count = ipa_get_param_count (info);
  parms_ainfo = XALLOCAVEC (struct param_analysis_info, param_count);
  memset (parms_ainfo, 0, sizeof (struct param_analysis_info) * param_count);

  ipa_analyze_params_uses (node, parms_ainfo);
  ipa_compute_jump_functions (node, parms_ainfo);

  for (i = 0; i < param_count; i++)
    if (parms_ainfo[i].visited_statements)
      BITMAP_FREE (parms_ainfo[i].visited_statements);

  current_function_decl = NULL;
  pop_cfun ();
}


/* Update the jump function DST when the call graph edge corresponding to SRC is
   is being inlined, knowing that DST is of type ancestor and src of known
   type.  */

static void
combine_known_type_and_ancestor_jfs (struct ipa_jump_func *src,
				     struct ipa_jump_func *dst)
{
  HOST_WIDE_INT combined_offset;
  tree combined_type;

  combined_offset = ipa_get_jf_known_type_offset (src)
    + ipa_get_jf_ancestor_offset (dst);
  combined_type = ipa_get_jf_ancestor_type (dst);

  ipa_set_jf_known_type (dst, combined_offset,
			 ipa_get_jf_known_type_base_type (src),
			 combined_type);
}

/* Update the jump functions associated with call graph edge E when the call
   graph edge CS is being inlined, assuming that E->caller is already (possibly
   indirectly) inlined into CS->callee and that E has not been inlined.  */

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
      struct ipa_jump_func *dst = ipa_get_ith_jump_func (args, i);

      if (dst->type == IPA_JF_ANCESTOR)
	{
	  struct ipa_jump_func *src;

	  /* Variable number of arguments can cause havoc if we try to access
	     one that does not exist in the inlined edge.  So make sure we
	     don't.  */
	  if (dst->value.ancestor.formal_id >= ipa_get_cs_argument_count (top))
	    {
	      dst->type = IPA_JF_UNKNOWN;
	      continue;
	    }

	  src = ipa_get_ith_jump_func (top, dst->value.ancestor.formal_id);
	  if (src->type == IPA_JF_KNOWN_TYPE)
	    combine_known_type_and_ancestor_jfs (src, dst);
	  else if (src->type == IPA_JF_PASS_THROUGH
		   && src->value.pass_through.operation == NOP_EXPR)
	    dst->value.ancestor.formal_id = src->value.pass_through.formal_id;
	  else if (src->type == IPA_JF_ANCESTOR)
	    {
	      dst->value.ancestor.formal_id = src->value.ancestor.formal_id;
	      dst->value.ancestor.offset += src->value.ancestor.offset;
	    }
	  else
	    dst->type = IPA_JF_UNKNOWN;
	}
      else if (dst->type == IPA_JF_PASS_THROUGH)
	{
	  struct ipa_jump_func *src;
	  /* We must check range due to calls with variable number of arguments
	     and we cannot combine jump functions with operations.  */
	  if (dst->value.pass_through.operation == NOP_EXPR
	      && (dst->value.pass_through.formal_id
		  < ipa_get_cs_argument_count (top)))
	    {
	      src = ipa_get_ith_jump_func (top,
					   dst->value.pass_through.formal_id);
	      *dst = *src;
	    }
	  else
	    dst->type = IPA_JF_UNKNOWN;
	}
    }
}

/* If TARGET is an addr_expr of a function declaration, make it the destination
   of an indirect edge IE and return the edge.  Otherwise, return NULL.  */

struct cgraph_edge *
ipa_make_edge_direct_to_target (struct cgraph_edge *ie, tree target)
{
  struct cgraph_node *callee;

  if (TREE_CODE (target) == ADDR_EXPR)
    target = TREE_OPERAND (target, 0);
  if (TREE_CODE (target) != FUNCTION_DECL)
    return NULL;
  callee = cgraph_get_node (target);
  if (!callee)
    return NULL;
  ipa_check_create_node_params ();

  /* We can not make edges to inline clones.  It is bug that someone removed
     the cgraph node too early.  */
  gcc_assert (!callee->global.inlined_to);

  cgraph_make_edge_direct (ie, callee);
  if (dump_file)
    {
      fprintf (dump_file, "ipa-prop: Discovered %s call to a known target "
	       "(%s/%i -> %s/%i), for stmt ",
	       ie->indirect_info->polymorphic ? "a virtual" : "an indirect",
	       xstrdup (cgraph_node_name (ie->caller)), ie->caller->uid,
	       xstrdup (cgraph_node_name (ie->callee)), ie->callee->uid);
      if (ie->call_stmt)
	print_gimple_stmt (dump_file, ie->call_stmt, 2, TDF_SLIM);
      else
	fprintf (dump_file, "with uid %i\n", ie->lto_stmt_uid);
    }
  callee = cgraph_function_or_thunk_node (callee, NULL);

  return ie;
}

/* Try to find a destination for indirect edge IE that corresponds to a simple
   call or a call of a member function pointer and where the destination is a
   pointer formal parameter described by jump function JFUNC.  If it can be
   determined, return the newly direct edge, otherwise return NULL.  */

static struct cgraph_edge *
try_make_edge_direct_simple_call (struct cgraph_edge *ie,
				  struct ipa_jump_func *jfunc)
{
  tree target;

  if (jfunc->type == IPA_JF_CONST)
    target = ipa_get_jf_constant (jfunc);
  else if (jfunc->type == IPA_JF_CONST_MEMBER_PTR)
    target = ipa_get_jf_member_ptr_pfn (jfunc);
  else
    return NULL;

  return ipa_make_edge_direct_to_target (ie, target);
}

/* Try to find a destination for indirect edge IE that corresponds to a
   virtual call based on a formal parameter which is described by jump
   function JFUNC and if it can be determined, make it direct and return the
   direct edge.  Otherwise, return NULL.  */

static struct cgraph_edge *
try_make_edge_direct_virtual_call (struct cgraph_edge *ie,
				   struct ipa_jump_func *jfunc)
{
  tree binfo, target;

  if (jfunc->type != IPA_JF_KNOWN_TYPE)
    return NULL;

  binfo = TYPE_BINFO (ipa_get_jf_known_type_base_type (jfunc));
  gcc_checking_assert (binfo);
  binfo = get_binfo_at_offset (binfo, ipa_get_jf_known_type_offset (jfunc)
			       + ie->indirect_info->anc_offset,
			       ie->indirect_info->otr_type);
  if (binfo)
    target = gimple_get_virt_method_for_binfo (ie->indirect_info->otr_token,
					       binfo);
  else
    return NULL;

  if (target)
    return ipa_make_edge_direct_to_target (ie, target);
  else
    return NULL;
}

/* Update the param called notes associated with NODE when CS is being inlined,
   assuming NODE is (potentially indirectly) inlined into CS->callee.
   Moreover, if the callee is discovered to be constant, create a new cgraph
   edge for it.  Newly discovered indirect edges will be added to *NEW_EDGES,
   unless NEW_EDGES is NULL.  Return true iff a new edge(s) were created.  */

static bool
update_indirect_edges_after_inlining (struct cgraph_edge *cs,
				      struct cgraph_node *node,
				      VEC (cgraph_edge_p, heap) **new_edges)
{
  struct ipa_edge_args *top;
  struct cgraph_edge *ie, *next_ie, *new_direct_edge;
  bool res = false;

  ipa_check_create_edge_args ();
  top = IPA_EDGE_REF (cs);

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      struct cgraph_indirect_call_info *ici = ie->indirect_info;
      struct ipa_jump_func *jfunc;

      next_ie = ie->next_callee;

      if (ici->param_index == -1)
	continue;

      /* We must check range due to calls with variable number of arguments:  */
      if (ici->param_index >= ipa_get_cs_argument_count (top))
	{
	  ici->param_index = -1;
	  continue;
	}

      jfunc = ipa_get_ith_jump_func (top, ici->param_index);
      if (jfunc->type == IPA_JF_PASS_THROUGH
	  && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
	ici->param_index = ipa_get_jf_pass_through_formal_id (jfunc);
      else if (jfunc->type == IPA_JF_ANCESTOR)
	{
 	  ici->param_index = ipa_get_jf_ancestor_formal_id (jfunc);
 	  ici->anc_offset += ipa_get_jf_ancestor_offset (jfunc);
	}
      else
	/* Either we can find a destination for this edge now or never. */
	ici->param_index = -1;

      if (!flag_indirect_inlining)
	continue;

      if (ici->polymorphic)
	new_direct_edge = try_make_edge_direct_virtual_call (ie, jfunc);
      else
	new_direct_edge = try_make_edge_direct_simple_call (ie, jfunc);

      if (new_direct_edge)
	{
	  new_direct_edge->indirect_inlining_edge = 1;
	  if (new_direct_edge->call_stmt)
	    new_direct_edge->call_stmt_cannot_inline_p
	      = !gimple_check_call_matching_types (new_direct_edge->call_stmt,
						   new_direct_edge->callee->symbol.decl);
	  if (new_edges)
	    {
	      VEC_safe_push (cgraph_edge_p, heap, *new_edges,
			     new_direct_edge);
	      top = IPA_EDGE_REF (cs);
	      res = true;
	    }
	}
    }

  return res;
}

/* Recursively traverse subtree of NODE (including node) made of inlined
   cgraph_edges when CS has been inlined and invoke
   update_indirect_edges_after_inlining on all nodes and
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

  res = update_indirect_edges_after_inlining (cs, node, new_edges);

  for (e = node->callees; e; e = e->next_callee)
    if (!e->inline_failed)
      res |= propagate_info_to_inlined_callees (cs, e->callee, new_edges);
    else
      update_jump_functions_after_inlining (cs, e);
  for (e = node->indirect_calls; e; e = e->next_callee)
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
  bool changed;
  /* Do nothing if the preparation phase has not been carried out yet
     (i.e. during early inlining).  */
  if (!ipa_node_params_vector)
    return false;
  gcc_assert (ipa_edge_args_vector);

  changed = propagate_info_to_inlined_callees (cs, cs->callee, new_edges);

  /* We do not keep jump functions of inlined edges up to date. Better to free
     them so we do not access them accidentally.  */
  ipa_free_edge_args_substructures (IPA_EDGE_REF (cs));
  return changed;
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

  FOR_EACH_VEC_ELT (ipa_edge_args_t, ipa_edge_args_vector, i, args)
    ipa_free_edge_args_substructures (args);

  VEC_free (ipa_edge_args_t, gc, ipa_edge_args_vector);
  ipa_edge_args_vector = NULL;
}

/* Frees all dynamically allocated structures that the param info points
   to.  */

void
ipa_free_node_params_substructures (struct ipa_node_params *info)
{
  VEC_free (ipa_param_descriptor_t, heap, info->descriptors);
  free (info->lattices);
  /* Lattice values and their sources are deallocated with their alocation
     pool.  */
  VEC_free (tree, heap, info->known_vals);
  memset (info, 0, sizeof (*info));
}

/* Free all ipa_node_params structures.  */

void
ipa_free_all_node_params (void)
{
  int i;
  struct ipa_node_params *info;

  FOR_EACH_VEC_ELT (ipa_node_params_t, ipa_node_params_vector, i, info)
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
  /* During IPA-CP updating we can be called on not-yet analyze clones.  */
  if (VEC_length (ipa_node_params_t, ipa_node_params_vector)
      <= (unsigned)node->uid)
    return;
  ipa_free_node_params_substructures (IPA_NODE_REF (node));
}

/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
ipa_edge_duplication_hook (struct cgraph_edge *src, struct cgraph_edge *dst,
			   __attribute__((unused)) void *data)
{
  struct ipa_edge_args *old_args, *new_args;

  ipa_check_create_edge_args ();

  old_args = IPA_EDGE_REF (src);
  new_args = IPA_EDGE_REF (dst);

  new_args->jump_functions = VEC_copy (ipa_jump_func_t, gc,
				       old_args->jump_functions);
}

/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
ipa_node_duplication_hook (struct cgraph_node *src, struct cgraph_node *dst,
			   ATTRIBUTE_UNUSED void *data)
{
  struct ipa_node_params *old_info, *new_info;

  ipa_check_create_node_params ();
  old_info = IPA_NODE_REF (src);
  new_info = IPA_NODE_REF (dst);

  new_info->descriptors = VEC_copy (ipa_param_descriptor_t, heap,
				    old_info->descriptors);
  new_info->lattices = NULL;
  new_info->ipcp_orig_node = old_info->ipcp_orig_node;

  new_info->uses_analysis_done = old_info->uses_analysis_done;
  new_info->node_enqueued = old_info->node_enqueued;
}


/* Analyze newly added function into callgraph.  */

static void
ipa_add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  ipa_analyze_node (node);
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
  function_insertion_hook_holder =
      cgraph_add_function_insertion_hook (&ipa_add_new_function, NULL);
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
  cgraph_remove_function_insertion_hook (function_insertion_hook_holder);
  function_insertion_hook_holder = NULL;
}

/* Free all ipa_node_params and all ipa_edge_args structures if they are no
   longer needed after ipa-cp.  */

void
ipa_free_all_structures_after_ipa_cp (void)
{
  if (!optimize)
    {
      ipa_free_all_edge_args ();
      ipa_free_all_node_params ();
      free_alloc_pool (ipcp_sources_pool);
      free_alloc_pool (ipcp_values_pool);
      ipa_unregister_cgraph_hooks ();
    }
}

/* Free all ipa_node_params and all ipa_edge_args structures if they are no
   longer needed after indirect inlining.  */

void
ipa_free_all_structures_after_iinln (void)
{
  ipa_free_all_edge_args ();
  ipa_free_all_node_params ();
  ipa_unregister_cgraph_hooks ();
  if (ipcp_sources_pool)
    free_alloc_pool (ipcp_sources_pool);
  if (ipcp_values_pool)
    free_alloc_pool (ipcp_values_pool);
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
  fprintf (f, "  function  %s parameter descriptors:\n",
	   cgraph_node_name (node));
  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    {
      temp = ipa_get_param (info, i);
      if (TREE_CODE (temp) == PARM_DECL)
	fprintf (f, "    param %d : %s", i,
                 (DECL_NAME (temp)
                  ? (*lang_hooks.decl_printable_name) (temp, 2)
                  : "(unnamed)"));
      if (ipa_is_param_used (info, i))
	fprintf (f, " used");
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
  FOR_EACH_FUNCTION (node)
    ipa_print_node_params (f, node);
}

/* Return a heap allocated vector containing formal parameters of FNDECL.  */

VEC(tree, heap) *
ipa_get_vector_of_formal_parms (tree fndecl)
{
  VEC(tree, heap) *args;
  int count;
  tree parm;

  count = count_formal_params (fndecl);
  args = VEC_alloc (tree, heap, count);
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
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
	  link = &DECL_CHAIN (parm);
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

	  adj->base = parm;
	  adj->reduction = new_parm;

	  *link = new_parm;

	  link = &DECL_CHAIN (new_parm);
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
      new_type = build_distinct_type_copy (orig_type);
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

  /* When signature changes, we need to clear builtin info.  */
  if (DECL_BUILT_IN (fndecl))
    {
      DECL_BUILT_IN_CLASS (fndecl) = NOT_BUILT_IN;
      DECL_FUNCTION_CODE (fndecl) = (enum built_in_function) 0;
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
  DECL_VIRTUAL_P (fndecl) = 0;
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
  VEC(tree, gc) **debug_args = NULL;
  gimple new_stmt;
  gimple_stmt_iterator gsi;
  tree callee_decl;
  int i, len;

  len = VEC_length (ipa_parm_adjustment_t, adjustments);
  vargs = VEC_alloc (tree, heap, len);
  callee_decl = !cs ? gimple_call_fndecl (stmt) : cs->callee->symbol.decl;

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
	  tree expr, base, off;
	  location_t loc;

	  /* We create a new parameter out of the value of the old one, we can
	     do the following kind of transformations:

	     - A scalar passed by reference is converted to a scalar passed by
               value.  (adj->by_ref is false and the type of the original
               actual argument is a pointer to a scalar).

             - A part of an aggregate is passed instead of the whole aggregate.
               The part can be passed either by value or by reference, this is
               determined by value of adj->by_ref.  Moreover, the code below
               handles both situations when the original aggregate is passed by
               value (its type is not a pointer) and when it is passed by
               reference (it is a pointer to an aggregate).

	     When the new argument is passed by reference (adj->by_ref is true)
	     it must be a part of an aggregate and therefore we form it by
	     simply taking the address of a reference inside the original
	     aggregate.  */

	  gcc_checking_assert (adj->offset % BITS_PER_UNIT == 0);
	  base = gimple_call_arg (stmt, adj->base_index);
	  loc = EXPR_LOCATION (base);

	  if (TREE_CODE (base) != ADDR_EXPR
	      && POINTER_TYPE_P (TREE_TYPE (base)))
	    off = build_int_cst (adj->alias_ptr_type,
				 adj->offset / BITS_PER_UNIT);
	  else
	    {
	      HOST_WIDE_INT base_offset;
	      tree prev_base;

	      if (TREE_CODE (base) == ADDR_EXPR)
		base = TREE_OPERAND (base, 0);
	      prev_base = base;
	      base = get_addr_base_and_unit_offset (base, &base_offset);
	      /* Aggregate arguments can have non-invariant addresses.  */
	      if (!base)
		{
		  base = build_fold_addr_expr (prev_base);
		  off = build_int_cst (adj->alias_ptr_type,
				       adj->offset / BITS_PER_UNIT);
		}
	      else if (TREE_CODE (base) == MEM_REF)
		{
		  off = build_int_cst (adj->alias_ptr_type,
				       base_offset
				       + adj->offset / BITS_PER_UNIT);
		  off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1),
					 off);
		  base = TREE_OPERAND (base, 0);
		}
	      else
		{
		  off = build_int_cst (adj->alias_ptr_type,
				       base_offset
				       + adj->offset / BITS_PER_UNIT);
		  base = build_fold_addr_expr (base);
		}
	    }

	  if (!adj->by_ref)
	    {
	      tree type = adj->type;
	      unsigned int align;
	      unsigned HOST_WIDE_INT misalign;

	      get_pointer_alignment_1 (base, &align, &misalign);
	      misalign += (double_int_sext (tree_to_double_int (off),
					    TYPE_PRECISION (TREE_TYPE (off))).low
			   * BITS_PER_UNIT);
	      misalign = misalign & (align - 1);
	      if (misalign != 0)
		align = (misalign & -misalign);
	      if (align < TYPE_ALIGN (type))
		type = build_aligned_type (type, align);
	      expr = fold_build2_loc (loc, MEM_REF, type, base, off);
	    }
	  else
	    {
	      expr = fold_build2_loc (loc, MEM_REF, adj->type, base, off);
	      expr = build_fold_addr_expr (expr);
	    }

	  expr = force_gimple_operand_gsi (&gsi, expr,
					   adj->by_ref
					   || is_gimple_reg_type (adj->type),
					   NULL, true, GSI_SAME_STMT);
	  VEC_quick_push (tree, vargs, expr);
	}
      if (!adj->copy_param && MAY_HAVE_DEBUG_STMTS)
	{
	  unsigned int ix;
	  tree ddecl = NULL_TREE, origin = DECL_ORIGIN (adj->base), arg;
	  gimple def_temp;

	  arg = gimple_call_arg (stmt, adj->base_index);
	  if (!useless_type_conversion_p (TREE_TYPE (origin), TREE_TYPE (arg)))
	    {
	      if (!fold_convertible_p (TREE_TYPE (origin), arg))
		continue;
	      arg = fold_convert_loc (gimple_location (stmt),
				      TREE_TYPE (origin), arg);
	    }
	  if (debug_args == NULL)
	    debug_args = decl_debug_args_insert (callee_decl);
	  for (ix = 0; VEC_iterate (tree, *debug_args, ix, ddecl); ix += 2)
	    if (ddecl == origin)
	      {
		ddecl = VEC_index (tree, *debug_args, ix + 1);
		break;
	      }
	  if (ddecl == NULL)
	    {
	      ddecl = make_node (DEBUG_EXPR_DECL);
	      DECL_ARTIFICIAL (ddecl) = 1;
	      TREE_TYPE (ddecl) = TREE_TYPE (origin);
	      DECL_MODE (ddecl) = DECL_MODE (origin);

	      VEC_safe_push (tree, gc, *debug_args, origin);
	      VEC_safe_push (tree, gc, *debug_args, ddecl);
	    }
	  def_temp = gimple_build_debug_bind (ddecl, unshare_expr (arg),
					      stmt);
	  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "replacing stmt:");
      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, 0);
    }

  new_stmt = gimple_build_call_vec (callee_decl, vargs);
  VEC_free (tree, heap, vargs);
  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_block (new_stmt, gimple_block (stmt));
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));
  gimple_call_copy_flags (new_stmt, stmt);

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
  streamer_write_uhwi (ob, jump_func->type);

  switch (jump_func->type)
    {
    case IPA_JF_UNKNOWN:
      break;
    case IPA_JF_KNOWN_TYPE:
      streamer_write_uhwi (ob, jump_func->value.known_type.offset);
      stream_write_tree (ob, jump_func->value.known_type.base_type, true);
      stream_write_tree (ob, jump_func->value.known_type.component_type, true);
      break;
    case IPA_JF_CONST:
      stream_write_tree (ob, jump_func->value.constant, true);
      break;
    case IPA_JF_PASS_THROUGH:
      stream_write_tree (ob, jump_func->value.pass_through.operand, true);
      streamer_write_uhwi (ob, jump_func->value.pass_through.formal_id);
      streamer_write_uhwi (ob, jump_func->value.pass_through.operation);
      break;
    case IPA_JF_ANCESTOR:
      streamer_write_uhwi (ob, jump_func->value.ancestor.offset);
      stream_write_tree (ob, jump_func->value.ancestor.type, true);
      streamer_write_uhwi (ob, jump_func->value.ancestor.formal_id);
      break;
    case IPA_JF_CONST_MEMBER_PTR:
      stream_write_tree (ob, jump_func->value.member_cst.pfn, true);
      stream_write_tree (ob, jump_func->value.member_cst.delta, false);
      break;
    }
}

/* Read in jump function JUMP_FUNC from IB.  */

static void
ipa_read_jump_function (struct lto_input_block *ib,
			struct ipa_jump_func *jump_func,
			struct data_in *data_in)
{
  jump_func->type = (enum jump_func_type) streamer_read_uhwi (ib);

  switch (jump_func->type)
    {
    case IPA_JF_UNKNOWN:
      break;
    case IPA_JF_KNOWN_TYPE:
      jump_func->value.known_type.offset = streamer_read_uhwi (ib);
      jump_func->value.known_type.base_type = stream_read_tree (ib, data_in);
      jump_func->value.known_type.component_type = stream_read_tree (ib,
								     data_in);
      break;
    case IPA_JF_CONST:
      jump_func->value.constant = stream_read_tree (ib, data_in);
      break;
    case IPA_JF_PASS_THROUGH:
      jump_func->value.pass_through.operand = stream_read_tree (ib, data_in);
      jump_func->value.pass_through.formal_id = streamer_read_uhwi (ib);
      jump_func->value.pass_through.operation
	= (enum tree_code) streamer_read_uhwi (ib);
      break;
    case IPA_JF_ANCESTOR:
      jump_func->value.ancestor.offset = streamer_read_uhwi (ib);
      jump_func->value.ancestor.type = stream_read_tree (ib, data_in);
      jump_func->value.ancestor.formal_id = streamer_read_uhwi (ib);
      break;
    case IPA_JF_CONST_MEMBER_PTR:
      jump_func->value.member_cst.pfn = stream_read_tree (ib, data_in);
      jump_func->value.member_cst.delta = stream_read_tree (ib, data_in);
      break;
    }
}

/* Stream out parts of cgraph_indirect_call_info corresponding to CS that are
   relevant to indirect inlining to OB.  */

static void
ipa_write_indirect_edge_info (struct output_block *ob,
			      struct cgraph_edge *cs)
{
  struct cgraph_indirect_call_info *ii = cs->indirect_info;
  struct bitpack_d bp;

  streamer_write_hwi (ob, ii->param_index);
  streamer_write_hwi (ob, ii->anc_offset);
  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, ii->polymorphic, 1);
  streamer_write_bitpack (&bp);

  if (ii->polymorphic)
    {
      streamer_write_hwi (ob, ii->otr_token);
      stream_write_tree (ob, ii->otr_type, true);
    }
}

/* Read in parts of cgraph_indirect_call_info corresponding to CS that are
   relevant to indirect inlining from IB.  */

static void
ipa_read_indirect_edge_info (struct lto_input_block *ib,
			     struct data_in *data_in ATTRIBUTE_UNUSED,
			     struct cgraph_edge *cs)
{
  struct cgraph_indirect_call_info *ii = cs->indirect_info;
  struct bitpack_d bp;

  ii->param_index = (int) streamer_read_hwi (ib);
  ii->anc_offset = (HOST_WIDE_INT) streamer_read_hwi (ib);
  bp = streamer_read_bitpack (ib);
  ii->polymorphic = bp_unpack_value (&bp, 1);
  if (ii->polymorphic)
    {
      ii->otr_token = (HOST_WIDE_INT) streamer_read_hwi (ib);
      ii->otr_type = stream_read_tree (ib, data_in);
    }
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
  struct bitpack_d bp;

  encoder = ob->decl_state->cgraph_node_encoder;
  node_ref = lto_cgraph_encoder_encode (encoder, node);
  streamer_write_uhwi (ob, node_ref);

  bp = bitpack_create (ob->main_stream);
  gcc_assert (info->uses_analysis_done
	      || ipa_get_param_count (info) == 0);
  gcc_assert (!info->node_enqueued);
  gcc_assert (!info->ipcp_orig_node);
  for (j = 0; j < ipa_get_param_count (info); j++)
    bp_pack_value (&bp, ipa_is_param_used (info, j), 1);
  streamer_write_bitpack (&bp);
  for (e = node->callees; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);

      streamer_write_uhwi (ob, ipa_get_cs_argument_count (args));
      for (j = 0; j < ipa_get_cs_argument_count (args); j++)
	ipa_write_jump_function (ob, ipa_get_ith_jump_func (args, j));
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);

      streamer_write_uhwi (ob, ipa_get_cs_argument_count (args));
      for (j = 0; j < ipa_get_cs_argument_count (args); j++)
	ipa_write_jump_function (ob, ipa_get_ith_jump_func (args, j));
      ipa_write_indirect_edge_info (ob, e);
    }
}

/* Stream in NODE info from IB.  */

static void
ipa_read_node_info (struct lto_input_block *ib, struct cgraph_node *node,
		    struct data_in *data_in)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int k;
  struct cgraph_edge *e;
  struct bitpack_d bp;

  ipa_initialize_node_params (node);

  bp = streamer_read_bitpack (ib);
  if (ipa_get_param_count (info) != 0)
    info->uses_analysis_done = true;
  info->node_enqueued = false;
  for (k = 0; k < ipa_get_param_count (info); k++)
    ipa_set_param_used (info, k, bp_unpack_value (&bp, 1));
  for (e = node->callees; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int count = streamer_read_uhwi (ib);

      if (!count)
	continue;
      VEC_safe_grow_cleared (ipa_jump_func_t, gc, args->jump_functions, count);

      for (k = 0; k < ipa_get_cs_argument_count (args); k++)
	ipa_read_jump_function (ib, ipa_get_ith_jump_func (args, k), data_in);
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int count = streamer_read_uhwi (ib);

      if (count)
	{
	  VEC_safe_grow_cleared (ipa_jump_func_t, gc, args->jump_functions,
				 count);
          for (k = 0; k < ipa_get_cs_argument_count (args); k++)
	    ipa_read_jump_function (ib, ipa_get_ith_jump_func (args, k),
				    data_in);
	}
      ipa_read_indirect_edge_info (ib, data_in, e);
    }
}

/* Write jump functions for nodes in SET.  */

void
ipa_prop_write_jump_functions (cgraph_node_set set)
{
  struct cgraph_node *node;
  struct output_block *ob;
  unsigned int count = 0;
  cgraph_node_set_iterator csi;

  if (!ipa_node_params_vector)
    return;

  ob = create_output_block (LTO_section_jump_functions);
  ob->cgraph_node = NULL;
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (cgraph_function_with_gimple_body_p (node)
	  && IPA_NODE_REF (node) != NULL)
	count++;
    }

  streamer_write_uhwi (ob, count);

  /* Process all of the functions.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (cgraph_function_with_gimple_body_p (node)
	  && IPA_NODE_REF (node) != NULL)
        ipa_write_node_info (ob, node);
    }
  streamer_write_char_stream (ob->main_stream, 0);
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
  const int cfg_offset = sizeof (struct lto_function_header);
  const int main_offset = cfg_offset + header->cfg_size;
  const int string_offset = main_offset + header->main_size;
  struct data_in *data_in;
  struct lto_input_block ib_main;
  unsigned int i;
  unsigned int count;

  LTO_INIT_INPUT_BLOCK (ib_main, (const char *) data + main_offset, 0,
			header->main_size);

  data_in =
    lto_data_in_create (file_data, (const char *) data + string_offset,
			header->string_size, NULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_cgraph_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->cgraph_node_encoder;
      node = lto_cgraph_encoder_deref (encoder, index);
      gcc_assert (node->analyzed);
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
   Also decl merging might've rendered parameter lists obsolete.
   Also compute called_with_variable_arg info.  */

void
ipa_update_after_lto_read (void)
{
  struct cgraph_node *node;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();

  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->analyzed)
      ipa_initialize_node_params (node);
}
