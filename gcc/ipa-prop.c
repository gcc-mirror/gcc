/* Interprocedural analyses.
   Copyright (C) 2005-2014 Free Software Foundation, Inc.

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
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "expr.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "langhooks.h"
#include "target.h"
#include "ipa-prop.h"
#include "bitmap.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-pass.h"
#include "tree-inline.h"
#include "ipa-inline.h"
#include "flags.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "tree-streamer.h"
#include "params.h"
#include "ipa-utils.h"
#include "stringpool.h"
#include "tree-ssanames.h"

/* Intermediate information about a parameter that is only useful during the
   run of ipa_analyze_node and is not kept afterwards.  */

struct param_analysis_info
{
  bool parm_modified, ref_modified, pt_modified;
  bitmap parm_visited_statements, pt_visited_statements;
};

/* Vector where the parameter infos are actually stored. */
vec<ipa_node_params> ipa_node_params_vector;
/* Vector of known aggregate values in cloned nodes.  */
vec<ipa_agg_replacement_value_p, va_gc> *ipa_node_agg_replacements;
/* Vector where the parameter infos are actually stored. */
vec<ipa_edge_args, va_gc> *ipa_edge_args_vector;

/* Holders of ipa cgraph hooks: */
static struct cgraph_edge_hook_list *edge_removal_hook_holder;
static struct cgraph_node_hook_list *node_removal_hook_holder;
static struct cgraph_2edge_hook_list *edge_duplication_hook_holder;
static struct cgraph_2node_hook_list *node_duplication_hook_holder;
static struct cgraph_node_hook_list *function_insertion_hook_holder;

/* Description of a reference to an IPA constant.  */
struct ipa_cst_ref_desc
{
  /* Edge that corresponds to the statement which took the reference.  */
  struct cgraph_edge *cs;
  /* Linked list of duplicates created when call graph edges are cloned.  */
  struct ipa_cst_ref_desc *next_duplicate;
  /* Number of references in IPA structures, IPA_UNDESCRIBED_USE if the value
     if out of control.  */
  int refcount;
};

/* Allocation pool for reference descriptions.  */

static alloc_pool ipa_refdesc_pool;

/* Return true if DECL_FUNCTION_SPECIFIC_OPTIMIZATION of the decl associated
   with NODE should prevent us from analyzing it for the purposes of IPA-CP.  */

static bool
ipa_func_spec_opts_forbid_analysis_p (struct cgraph_node *node)
{
  tree fs_opts = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (node->decl);
  struct cl_optimization *os;

  if (!fs_opts)
    return false;
  os = TREE_OPTIMIZATION (fs_opts);
  return !os->x_optimize || !os->x_flag_ipa_cp;
}

/* Return index of the formal whose tree is PTREE in function which corresponds
   to INFO.  */

static int
ipa_get_param_decl_index_1 (vec<ipa_param_descriptor> descriptors, tree ptree)
{
  int i, count;

  count = descriptors.length ();
  for (i = 0; i < count; i++)
    if (descriptors[i].decl == ptree)
      return i;

  return -1;
}

/* Return index of the formal whose tree is PTREE in function which corresponds
   to INFO.  */

int
ipa_get_param_decl_index (struct ipa_node_params *info, tree ptree)
{
  return ipa_get_param_decl_index_1 (info->descriptors, ptree);
}

/* Populate the param_decl field in parameter DESCRIPTORS that correspond to
   NODE.  */

static void
ipa_populate_param_decls (struct cgraph_node *node,
			  vec<ipa_param_descriptor> &descriptors)
{
  tree fndecl;
  tree fnargs;
  tree parm;
  int param_num;

  fndecl = node->decl;
  gcc_assert (gimple_has_body_p (fndecl));
  fnargs = DECL_ARGUMENTS (fndecl);
  param_num = 0;
  for (parm = fnargs; parm; parm = DECL_CHAIN (parm))
    {
      descriptors[param_num].decl = parm;
      descriptors[param_num].move_cost = estimate_move_cost (TREE_TYPE (parm));
      param_num++;
    }
}

/* Return how many formal parameters FNDECL has.  */

static inline int
count_formal_params (tree fndecl)
{
  tree parm;
  int count = 0;
  gcc_assert (gimple_has_body_p (fndecl));

  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    count++;

  return count;
}

/* Return the declaration of Ith formal parameter of the function corresponding
   to INFO.  Note there is no setter function as this array is built just once
   using ipa_initialize_node_params. */

void
ipa_dump_param (FILE *file, struct ipa_node_params *info, int i)
{
  fprintf (file, "param #%i", i);
  if (info->descriptors[i].decl)
    {
      fprintf (file, " ");
      print_generic_expr (file, info->descriptors[i].decl, 0);
    }
}

/* Initialize the ipa_node_params structure associated with NODE 
   to hold PARAM_COUNT parameters.  */

void
ipa_alloc_node_params (struct cgraph_node *node, int param_count)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (!info->descriptors.exists () && param_count)
    info->descriptors.safe_grow_cleared (param_count);
}

/* Initialize the ipa_node_params structure associated with NODE by counting
   the function parameters, creating the descriptors and populating their
   param_decls.  */

void
ipa_initialize_node_params (struct cgraph_node *node)
{
  struct ipa_node_params *info = IPA_NODE_REF (node);

  if (!info->descriptors.exists ())
    {
      ipa_alloc_node_params (node, count_formal_params (node->decl));
      ipa_populate_param_decls (node, info->descriptors);
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
	  tree val = jump_func->value.constant.value;
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
      else if (type == IPA_JF_PASS_THROUGH)
	{
	  fprintf (f, "PASS THROUGH: ");
	  fprintf (f, "%d, op %s",
		   jump_func->value.pass_through.formal_id,
		   get_tree_code_name(jump_func->value.pass_through.operation));
	  if (jump_func->value.pass_through.operation != NOP_EXPR)
	    {
	      fprintf (f, " ");
	      print_generic_expr (f,
				  jump_func->value.pass_through.operand, 0);
	    }
	  if (jump_func->value.pass_through.agg_preserved)
	    fprintf (f, ", agg_preserved");
	  if (jump_func->value.pass_through.type_preserved)
	    fprintf (f, ", type_preserved");
	  fprintf (f, "\n");
	}
      else if (type == IPA_JF_ANCESTOR)
	{
	  fprintf (f, "ANCESTOR: ");
	  fprintf (f, "%d, offset "HOST_WIDE_INT_PRINT_DEC", ",
		   jump_func->value.ancestor.formal_id,
		   jump_func->value.ancestor.offset);
	  print_generic_expr (f, jump_func->value.ancestor.type, 0);
	  if (jump_func->value.ancestor.agg_preserved)
	    fprintf (f, ", agg_preserved");
	  if (jump_func->value.ancestor.type_preserved)
	    fprintf (f, ", type_preserved");
	  fprintf (f, "\n");
	}

      if (jump_func->agg.items)
	{
	  struct ipa_agg_jf_item *item;
	  int j;

	  fprintf (f, "         Aggregate passed by %s:\n",
		   jump_func->agg.by_ref ? "reference" : "value");
	  FOR_EACH_VEC_SAFE_ELT (jump_func->agg.items, j, item)
	    {
	      fprintf (f, "           offset: " HOST_WIDE_INT_PRINT_DEC ", ",
		       item->offset);
	      if (TYPE_P (item->value))
		fprintf (f, "clobber of " HOST_WIDE_INT_PRINT_DEC " bits",
			 tree_to_uhwi (TYPE_SIZE (item->value)));
	      else
		{
		  fprintf (f, "cst: ");
		  print_generic_expr (f, item->value, 0);
		}
	      fprintf (f, "\n");
	    }
	}
    }
}


/* Print the jump functions of all arguments on all call graph edges going from
   NODE to file F.  */

void
ipa_print_node_jump_functions (FILE *f, struct cgraph_node *node)
{
  struct cgraph_edge *cs;

  fprintf (f, "  Jump functions of caller  %s/%i:\n", node->name (),
	   node->order);
  for (cs = node->callees; cs; cs = cs->next_callee)
    {
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	continue;

      fprintf (f, "    callsite  %s/%i -> %s/%i : \n",
	       xstrdup (node->name ()), node->order,
	       xstrdup (cs->callee->name ()),
	       cs->callee->order);
      ipa_print_node_jump_functions_for_edge (f, cs);
    }

  for (cs = node->indirect_calls; cs; cs = cs->next_callee)
    {
      struct cgraph_indirect_call_info *ii;
      if (!ipa_edge_args_info_available_for_edge_p (cs))
	continue;

      ii = cs->indirect_info;
      if (ii->agg_contents)
	fprintf (f, "    indirect %s callsite, calling param %i, "
		 "offset " HOST_WIDE_INT_PRINT_DEC ", %s",
		 ii->member_ptr ? "member ptr" : "aggregate",
		 ii->param_index, ii->offset,
		 ii->by_ref ? "by reference" : "by_value");
      else
	fprintf (f, "    indirect %s callsite, calling param %i, "
		 "offset " HOST_WIDE_INT_PRINT_DEC,
		 ii->polymorphic ? "polymorphic" : "simple", ii->param_index,
		 ii->offset);

      if (cs->call_stmt)
	{
	  fprintf (f, ", for stmt ");
	  print_gimple_stmt (f, cs->call_stmt, 0, TDF_SLIM);
	}
      else
	fprintf (f, "\n");
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
  gcc_assert (TREE_CODE (component_type) == RECORD_TYPE
	      && TYPE_BINFO (component_type));
  if (!flag_devirtualize)
    return;
  gcc_assert (BINFO_VTABLE (TYPE_BINFO (component_type)));
  jfunc->type = IPA_JF_KNOWN_TYPE;
  jfunc->value.known_type.offset = offset,
  jfunc->value.known_type.base_type = base_type;
  jfunc->value.known_type.component_type = component_type;
  gcc_assert (component_type);
}

/* Set JFUNC to be a copy of another jmp (to be used by jump function
   combination code).  The two functions will share their rdesc.  */

static void
ipa_set_jf_cst_copy (struct ipa_jump_func *dst,
		     struct ipa_jump_func *src)

{
  gcc_checking_assert (src->type == IPA_JF_CONST);
  dst->type = IPA_JF_CONST;
  dst->value.constant = src->value.constant;
}

/* Set JFUNC to be a constant jmp function.  */

static void
ipa_set_jf_constant (struct ipa_jump_func *jfunc, tree constant,
		     struct cgraph_edge *cs)
{
  constant = unshare_expr (constant);
  if (constant && EXPR_P (constant))
    SET_EXPR_LOCATION (constant, UNKNOWN_LOCATION);
  jfunc->type = IPA_JF_CONST;
  jfunc->value.constant.value = unshare_expr_without_location (constant);

  if (TREE_CODE (constant) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (constant, 0)) == FUNCTION_DECL)
    {
      struct ipa_cst_ref_desc *rdesc;
      if (!ipa_refdesc_pool)
	ipa_refdesc_pool = create_alloc_pool ("IPA-PROP ref descriptions",
					sizeof (struct ipa_cst_ref_desc), 32);

      rdesc = (struct ipa_cst_ref_desc *) pool_alloc (ipa_refdesc_pool);
      rdesc->cs = cs;
      rdesc->next_duplicate = NULL;
      rdesc->refcount = 1;
      jfunc->value.constant.rdesc = rdesc;
    }
  else
    jfunc->value.constant.rdesc = NULL;
}

/* Set JFUNC to be a simple pass-through jump function.  */
static void
ipa_set_jf_simple_pass_through (struct ipa_jump_func *jfunc, int formal_id,
				bool agg_preserved, bool type_preserved)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = NULL_TREE;
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = NOP_EXPR;
  jfunc->value.pass_through.agg_preserved = agg_preserved;
  jfunc->value.pass_through.type_preserved = type_preserved;
}

/* Set JFUNC to be an arithmetic pass through jump function.  */

static void
ipa_set_jf_arith_pass_through (struct ipa_jump_func *jfunc, int formal_id,
			       tree operand, enum tree_code operation)
{
  jfunc->type = IPA_JF_PASS_THROUGH;
  jfunc->value.pass_through.operand = unshare_expr_without_location (operand);
  jfunc->value.pass_through.formal_id = formal_id;
  jfunc->value.pass_through.operation = operation;
  jfunc->value.pass_through.agg_preserved = false;
  jfunc->value.pass_through.type_preserved = false;
}

/* Set JFUNC to be an ancestor jump function.  */

static void
ipa_set_ancestor_jf (struct ipa_jump_func *jfunc, HOST_WIDE_INT offset,
		     tree type, int formal_id, bool agg_preserved,
		     bool type_preserved)
{
  if (!flag_devirtualize)
    type_preserved = false;
  gcc_assert (!type_preserved
	      || (TREE_CODE (type) == RECORD_TYPE
		  && TYPE_BINFO (type)
		  && BINFO_VTABLE (TYPE_BINFO (type))));
  jfunc->type = IPA_JF_ANCESTOR;
  jfunc->value.ancestor.formal_id = formal_id;
  jfunc->value.ancestor.offset = offset;
  jfunc->value.ancestor.type = type_preserved ? type : NULL;
  jfunc->value.ancestor.agg_preserved = agg_preserved;
  jfunc->value.ancestor.type_preserved = type_preserved;
}

/* Extract the acual BINFO being described by JFUNC which must be a known type
   jump function.  */

tree
ipa_binfo_from_known_type_jfunc (struct ipa_jump_func *jfunc)
{
  tree base_binfo = TYPE_BINFO (jfunc->value.known_type.base_type);
  if (!base_binfo)
    return NULL_TREE;
  return get_binfo_at_offset (base_binfo,
			      jfunc->value.known_type.offset,
			      jfunc->value.known_type.component_type);
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
  /* TODO: Skip clobbers, doing so triggers problem in PR60306.  */
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
  tree lhs, rhs, base, binfo;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  lhs = gimple_assign_lhs (stmt);
  rhs = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (lhs) != COMPONENT_REF
      || !DECL_VIRTUAL_P (TREE_OPERAND (lhs, 1)))
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

  binfo = vtable_pointer_value_to_binfo (rhs);

  /* FIXME: vtable_pointer_value_to_binfo may return BINFO of a
     base of outer type.  In this case we would need to either
     work on binfos or translate it back to outer type and offset.
     KNOWN_TYPE jump functions are not ready for that, yet.  */
  if (!binfo || TYPE_BINFO (BINFO_TYPE (binfo)) != binfo)
   return NULL;

  return BINFO_TYPE (binfo);
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



/* Detect whether the dynamic type of ARG of COMP_TYPE has changed (before
   callsite CALL) by looking for assignments to its virtual table pointer.  If
   it is, return true and fill in the jump function JFUNC with relevant type
   information or set it to unknown.  ARG is the object itself (not a pointer
   to it, unless dereferenced).  BASE is the base of the memory access as
   returned by get_ref_base_and_extent, as is the offset.  */

static bool
detect_type_change (tree arg, tree base, tree comp_type, gimple call,
		    struct ipa_jump_func *jfunc, HOST_WIDE_INT offset)
{
  struct type_change_info tci;
  ao_ref ao;

  gcc_checking_assert (DECL_P (arg)
		       || TREE_CODE (arg) == MEM_REF
		       || handled_component_p (arg));
  /* Const calls cannot call virtual methods through VMT and so type changes do
     not matter.  */
  if (!flag_devirtualize || !gimple_vuse (call)
      /* Be sure expected_type is polymorphic.  */
      || !comp_type
      || TREE_CODE (comp_type) != RECORD_TYPE
      || !TYPE_BINFO (comp_type)
      || !BINFO_VTABLE (TYPE_BINFO (comp_type)))
    return true;

  /* C++ methods are not allowed to change THIS pointer unless they
     are constructors or destructors.  */
  if (TREE_CODE	(base) == MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (base, 0))
      && TREE_CODE (SSA_NAME_VAR (TREE_OPERAND (base, 0))) == PARM_DECL
      && TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE
      && !DECL_CXX_CONSTRUCTOR_P (current_function_decl)
      && !DECL_CXX_DESTRUCTOR_P (current_function_decl)
      && (SSA_NAME_VAR (TREE_OPERAND (base, 0))
	  == DECL_ARGUMENTS (current_function_decl)))
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

/* Like detect_type_change but ARG is supposed to be a non-dereferenced pointer
   SSA name (its dereference will become the base and the offset is assumed to
   be zero).  */

static bool
detect_type_change_ssa (tree arg, tree comp_type,
			gimple call, struct ipa_jump_func *jfunc)
{
  gcc_checking_assert (TREE_CODE (arg) == SSA_NAME);
  if (!flag_devirtualize
      || !POINTER_TYPE_P (TREE_TYPE (arg)))
    return false;

  arg = build2 (MEM_REF, ptr_type_node, arg,
		build_int_cst (ptr_type_node, 0));

  return detect_type_change (arg, arg, comp_type, call, jfunc, 0);
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

/* Return true if a load from a formal parameter PARM_LOAD is known to retrieve
   a value known not to be modified in this function before reaching the
   statement STMT.  PARM_AINFO is a pointer to a structure containing temporary
   information about the parameter.  */

static bool
parm_preserved_before_stmt_p (struct param_analysis_info *parm_ainfo,
			       gimple stmt, tree parm_load)
{
  bool modified = false;
  bitmap *visited_stmts;
  ao_ref refd;

  if (parm_ainfo && parm_ainfo->parm_modified)
    return false;

  gcc_checking_assert (gimple_vuse (stmt) != NULL_TREE);
  ao_ref_init (&refd, parm_load);
  /* We can cache visited statements only when parm_ainfo is available and when
     we are looking at a naked load of the whole parameter.  */
  if (!parm_ainfo || TREE_CODE (parm_load) != PARM_DECL)
    visited_stmts = NULL;
  else
    visited_stmts = &parm_ainfo->parm_visited_statements;
  walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified, &modified,
		      visited_stmts);
  if (parm_ainfo && modified)
    parm_ainfo->parm_modified = true;
  return !modified;
}

/* If STMT is an assignment that loads a value from an parameter declaration,
   return the index of the parameter in ipa_node_params which has not been
   modified.  Otherwise return -1.  */

static int
load_from_unmodified_param (vec<ipa_param_descriptor> descriptors,
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

  index = ipa_get_param_decl_index_1 (descriptors, op1);
  if (index < 0
      || !parm_preserved_before_stmt_p (parms_ainfo ? &parms_ainfo[index]
					: NULL, stmt, op1))
    return -1;

  return index;
}

/* Return true if memory reference REF loads data that are known to be
   unmodified in this function before reaching statement STMT.  PARM_AINFO, if
   non-NULL, is a pointer to a structure containing temporary information about
   PARM.  */

static bool
parm_ref_data_preserved_p (struct param_analysis_info *parm_ainfo,
			      gimple stmt, tree ref)
{
  bool modified = false;
  ao_ref refd;

  gcc_checking_assert (gimple_vuse (stmt));
  if (parm_ainfo && parm_ainfo->ref_modified)
    return false;

  ao_ref_init (&refd, ref);
  walk_aliased_vdefs (&refd, gimple_vuse (stmt), mark_modified, &modified,
		      NULL);
  if (parm_ainfo && modified)
    parm_ainfo->ref_modified = true;
  return !modified;
}

/* Return true if the data pointed to by PARM is known to be unmodified in this
   function before reaching call statement CALL into which it is passed.
   PARM_AINFO is a pointer to a structure containing temporary information
   about PARM.  */

static bool
parm_ref_data_pass_through_p (struct param_analysis_info *parm_ainfo,
			       gimple call, tree parm)
{
  bool modified = false;
  ao_ref refd;

  /* It's unnecessary to calculate anything about memory contnets for a const
     function because it is not goin to use it.  But do not cache the result
     either.  Also, no such calculations for non-pointers.  */
  if (!gimple_vuse (call)
      || !POINTER_TYPE_P (TREE_TYPE (parm)))
    return false;

  if (parm_ainfo->pt_modified)
    return false;

  ao_ref_init_from_ptr_and_size (&refd, parm, NULL_TREE);
  walk_aliased_vdefs (&refd, gimple_vuse (call), mark_modified, &modified,
		      parm_ainfo ? &parm_ainfo->pt_visited_statements : NULL);
  if (modified)
    parm_ainfo->pt_modified = true;
  return !modified;
}

/* Return true if we can prove that OP is a memory reference loading unmodified
   data from an aggregate passed as a parameter and if the aggregate is passed
   by reference, that the alias type of the load corresponds to the type of the
   formal parameter (so that we can rely on this type for TBAA in callers).
   INFO and PARMS_AINFO describe parameters of the current function (but the
   latter can be NULL), STMT is the load statement.  If function returns true,
   *INDEX_P, *OFFSET_P and *BY_REF is filled with the parameter index, offset
   within the aggregate and whether it is a load from a value passed by
   reference respectively.  */

static bool
ipa_load_from_parm_agg_1 (vec<ipa_param_descriptor> descriptors,
			  struct param_analysis_info *parms_ainfo, gimple stmt,
			  tree op, int *index_p, HOST_WIDE_INT *offset_p,
			  HOST_WIDE_INT *size_p, bool *by_ref_p)
{
  int index;
  HOST_WIDE_INT size, max_size;
  tree base = get_ref_base_and_extent (op, offset_p, &size, &max_size);

  if (max_size == -1 || max_size != size || *offset_p < 0)
    return false;

  if (DECL_P (base))
    {
      int index = ipa_get_param_decl_index_1 (descriptors, base);
      if (index >= 0
	  && parm_preserved_before_stmt_p (parms_ainfo ? &parms_ainfo[index]
					   : NULL, stmt, op))
	{
	  *index_p = index;
	  *by_ref_p = false;
	  if (size_p)
	    *size_p = size;
	  return true;
	}
      return false;
    }

  if (TREE_CODE (base) != MEM_REF
	   || TREE_CODE (TREE_OPERAND (base, 0)) != SSA_NAME
	   || !integer_zerop (TREE_OPERAND (base, 1)))
    return false;

  if (SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (base, 0)))
    {
      tree parm = SSA_NAME_VAR (TREE_OPERAND (base, 0));
      index = ipa_get_param_decl_index_1 (descriptors, parm);
    }
  else
    {
      /* This branch catches situations where a pointer parameter is not a
	 gimple register, for example:

	 void hip7(S*) (struct S * p)
	 {
	 void (*<T2e4>) (struct S *) D.1867;
	 struct S * p.1;

	 <bb 2>:
	 p.1_1 = p;
	 D.1867_2 = p.1_1->f;
	 D.1867_2 ();
	 gdp = &p;
      */

      gimple def = SSA_NAME_DEF_STMT (TREE_OPERAND (base, 0));
      index = load_from_unmodified_param (descriptors, parms_ainfo, def);
    }

  if (index >= 0
      && parm_ref_data_preserved_p (parms_ainfo ? &parms_ainfo[index] : NULL,
				    stmt, op))
    {
      *index_p = index;
      *by_ref_p = true;
      if (size_p)
	*size_p = size;
      return true;
    }
  return false;
}

/* Just like the previous function, just without the param_analysis_info
   pointer, for users outside of this file.  */

bool
ipa_load_from_parm_agg (struct ipa_node_params *info, gimple stmt,
			tree op, int *index_p, HOST_WIDE_INT *offset_p,
			bool *by_ref_p)
{
  return ipa_load_from_parm_agg_1 (info->descriptors, NULL, stmt, op, index_p,
				   offset_p, NULL, by_ref_p);
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
				  gimple call, gimple stmt, tree name,
				  tree param_type)
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
	index = load_from_unmodified_param (info->descriptors, parms_ainfo,
					    SSA_NAME_DEF_STMT (op1));
      tc_ssa = op1;
    }
  else
    {
      index = load_from_unmodified_param (info->descriptors, parms_ainfo, stmt);
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
      else if (gimple_assign_single_p (stmt))
	{
	  bool agg_p = parm_ref_data_pass_through_p (&parms_ainfo[index],
						     call, tc_ssa);
	  bool type_p = false;

	  if (param_type && POINTER_TYPE_P (param_type))
	    type_p = !detect_type_change_ssa (tc_ssa, TREE_TYPE (param_type),
					      call, jfunc);
	  if (type_p || jfunc->type == IPA_JF_UNKNOWN)
	    ipa_set_jf_simple_pass_through (jfunc, index, agg_p, type_p);
	}
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
  offset += mem_ref_offset (base).to_short_addr () * BITS_PER_UNIT;
  ssa = TREE_OPERAND (base, 0);
  if (TREE_CODE (ssa) != SSA_NAME
      || !SSA_NAME_IS_DEFAULT_DEF (ssa)
      || offset < 0)
    return;

  /* Dynamic types are changed in constructors and destructors.  */
  index = ipa_get_param_decl_index (info, SSA_NAME_VAR (ssa));
  if (index >= 0 && param_type && POINTER_TYPE_P (param_type))
    {
      bool type_p = !detect_type_change (op1, base, TREE_TYPE (param_type),
					 call, jfunc, offset);
      if (type_p || jfunc->type == IPA_JF_UNKNOWN)
	ipa_set_ancestor_jf (jfunc, offset,
			     type_p ? TREE_TYPE (param_type) : NULL, index,
			     parm_ref_data_pass_through_p (&parms_ainfo[index],
							   call, ssa), type_p);
    }
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

  *offset += mem_ref_offset (expr).to_short_addr () * BITS_PER_UNIT;
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
				    struct param_analysis_info *parms_ainfo,
				    struct ipa_jump_func *jfunc,
				    gimple call, gimple phi, tree param_type)
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
  if (index < 0)
    return;

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

  bool type_p = false;
  if (param_type && POINTER_TYPE_P (param_type))
    type_p = !detect_type_change (obj, expr, TREE_TYPE (param_type),
				  call, jfunc, offset);
  if (type_p || jfunc->type == IPA_JF_UNKNOWN)
    ipa_set_ancestor_jf (jfunc, offset, type_p ? TREE_TYPE (param_type) : NULL, index,
			 parm_ref_data_pass_through_p (&parms_ainfo[index],
						       call, parm), type_p);
}

/* Given OP which is passed as an actual argument to a called function,
   determine if it is possible to construct a KNOWN_TYPE jump function for it
   and if so, create one and store it to JFUNC.
   EXPECTED_TYPE represents a type the argument should be in  */

static void
compute_known_type_jump_func (tree op, struct ipa_jump_func *jfunc,
			      gimple call, tree expected_type)
{
  HOST_WIDE_INT offset, size, max_size;
  tree base;

  if (!flag_devirtualize
      || TREE_CODE (op) != ADDR_EXPR
      || TREE_CODE (TREE_TYPE (TREE_TYPE (op))) != RECORD_TYPE
      /* Be sure expected_type is polymorphic.  */
      || !expected_type
      || TREE_CODE (expected_type) != RECORD_TYPE
      || !TYPE_BINFO (expected_type)
      || !BINFO_VTABLE (TYPE_BINFO (expected_type)))
    return;

  op = TREE_OPERAND (op, 0);
  base = get_ref_base_and_extent (op, &offset, &size, &max_size);
  if (!DECL_P (base)
      || max_size == -1
      || max_size != size
      || TREE_CODE (TREE_TYPE (base)) != RECORD_TYPE
      || is_global_var (base))
    return;

  if (detect_type_change (op, base, expected_type, call, jfunc, offset))
    return;

  ipa_set_jf_known_type (jfunc, offset, TREE_TYPE (base),
			 expected_type);
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
      || TREE_CODE (TREE_TYPE (TREE_TYPE (fld))) != METHOD_TYPE
      || !tree_fits_uhwi_p (DECL_FIELD_OFFSET (fld)))
    return false;

  if (method_ptr)
    *method_ptr = fld;

  fld = DECL_CHAIN (fld);
  if (!fld || INTEGRAL_TYPE_P (fld)
      || !tree_fits_uhwi_p (DECL_FIELD_OFFSET (fld)))
    return false;
  if (delta)
    *delta = fld;

  if (DECL_CHAIN (fld))
    return false;

  return true;
}

/* If RHS is an SSA_NAME and it is defined by a simple copy assign statement,
   return the rhs of its defining statement.  Otherwise return RHS as it
   is.  */

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

/* Simple linked list, describing known contents of an aggregate beforere
   call.  */

struct ipa_known_agg_contents_list
{
  /* Offset and size of the described part of the aggregate.  */
  HOST_WIDE_INT offset, size;
  /* Known constant value or NULL if the contents is known to be unknown.  */
  tree constant;
  /* Pointer to the next structure in the list.  */
  struct ipa_known_agg_contents_list *next;
};

/* Traverse statements from CALL backwards, scanning whether an aggregate given
   in ARG is filled in with constant values.  ARG can either be an aggregate
   expression or a pointer to an aggregate.  ARG_TYPE is the type of the aggregate.
   JFUNC is the jump function into which the constants are subsequently stored.  */

static void
determine_known_aggregate_parts (gimple call, tree arg, tree arg_type,
				 struct ipa_jump_func *jfunc)
{
  struct ipa_known_agg_contents_list *list = NULL;
  int item_count = 0, const_count = 0;
  HOST_WIDE_INT arg_offset, arg_size;
  gimple_stmt_iterator gsi;
  tree arg_base;
  bool check_ref, by_ref;
  ao_ref r;

  /* The function operates in three stages.  First, we prepare check_ref, r,
     arg_base and arg_offset based on what is actually passed as an actual
     argument.  */

  if (POINTER_TYPE_P (arg_type))
    {
      by_ref = true;
      if (TREE_CODE (arg) == SSA_NAME)
	{
	  tree type_size;
          if (!tree_fits_uhwi_p (TYPE_SIZE (TREE_TYPE (arg_type))))
            return;
	  check_ref = true;
	  arg_base = arg;
	  arg_offset = 0;
	  type_size = TYPE_SIZE (TREE_TYPE (arg_type));
	  arg_size = tree_to_uhwi (type_size);
	  ao_ref_init_from_ptr_and_size (&r, arg_base, NULL_TREE);
	}
      else if (TREE_CODE (arg) == ADDR_EXPR)
	{
	  HOST_WIDE_INT arg_max_size;

	  arg = TREE_OPERAND (arg, 0);
	  arg_base = get_ref_base_and_extent (arg, &arg_offset, &arg_size,
					  &arg_max_size);
	  if (arg_max_size == -1
	      || arg_max_size != arg_size
	      || arg_offset < 0)
	    return;
	  if (DECL_P (arg_base))
	    {
	      tree size;
	      check_ref = false;
	      size = build_int_cst (integer_type_node, arg_size);
	      ao_ref_init_from_ptr_and_size (&r, arg_base, size);
	    }
	  else
	    return;
	}
      else
	return;
    }
  else
    {
      HOST_WIDE_INT arg_max_size;

      gcc_checking_assert (AGGREGATE_TYPE_P (TREE_TYPE (arg)));

      by_ref = false;
      check_ref = false;
      arg_base = get_ref_base_and_extent (arg, &arg_offset, &arg_size,
					  &arg_max_size);
      if (arg_max_size == -1
	  || arg_max_size != arg_size
	  || arg_offset < 0)
	return;

      ao_ref_init (&r, arg);
    }

  /* Second stage walks back the BB, looks at individual statements and as long
     as it is confident of how the statements affect contents of the
     aggregates, it builds a sorted linked list of ipa_agg_jf_list structures
     describing it.  */
  gsi = gsi_for_stmt (call);
  gsi_prev (&gsi);
  for (; !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      struct ipa_known_agg_contents_list *n, **p;
      gimple stmt = gsi_stmt (gsi);
      HOST_WIDE_INT lhs_offset, lhs_size, lhs_max_size;
      tree lhs, rhs, lhs_base;
      bool partial_overlap;

      if (!stmt_may_clobber_ref_p_1 (stmt, &r))
	continue;
      if (!gimple_assign_single_p (stmt))
	break;

      lhs = gimple_assign_lhs (stmt);
      rhs = gimple_assign_rhs1 (stmt);
      if (!is_gimple_reg_type (TREE_TYPE (rhs))
	  || TREE_CODE (lhs) == BIT_FIELD_REF
	  || contains_bitfld_component_ref_p (lhs))
	break;

      lhs_base = get_ref_base_and_extent (lhs, &lhs_offset, &lhs_size,
					  &lhs_max_size);
      if (lhs_max_size == -1
	  || lhs_max_size != lhs_size
	  || (lhs_offset < arg_offset
	      && lhs_offset + lhs_size > arg_offset)
	  || (lhs_offset < arg_offset + arg_size
	      && lhs_offset + lhs_size > arg_offset + arg_size))
	break;

      if (check_ref)
	{
	  if (TREE_CODE (lhs_base) != MEM_REF
	      || TREE_OPERAND (lhs_base, 0) != arg_base
	      || !integer_zerop (TREE_OPERAND (lhs_base, 1)))
	    break;
	}
      else if (lhs_base != arg_base)
	{
	  if (DECL_P (lhs_base))
	    continue;
	  else
	    break;
	}

      if (lhs_offset + lhs_size < arg_offset
	  || lhs_offset >= (arg_offset + arg_size))
	continue;

      partial_overlap = false;
      p = &list;
      while (*p && (*p)->offset < lhs_offset)
	{
	  if ((*p)->offset + (*p)->size > lhs_offset)
	    {
	      partial_overlap = true;
	      break;
	    }
	  p = &(*p)->next;
	}
      if (partial_overlap)
	break;
      if (*p && (*p)->offset < lhs_offset + lhs_size)
	{
	  if ((*p)->offset == lhs_offset && (*p)->size == lhs_size)
	    /* We already know this value is subsequently overwritten with
	       something else.  */
	    continue;
	  else
	    /* Otherwise this is a partial overlap which we cannot
	       represent.  */
	    break;
	}

      rhs = get_ssa_def_if_simple_copy (rhs);
      n = XALLOCA (struct ipa_known_agg_contents_list);
      n->size = lhs_size;
      n->offset = lhs_offset;
      if (is_gimple_ip_invariant (rhs))
	{
	  n->constant = rhs;
	  const_count++;
	}
      else
	n->constant = NULL_TREE;
      n->next = *p;
      *p = n;

      item_count++;
      if (const_count == PARAM_VALUE (PARAM_IPA_MAX_AGG_ITEMS)
	  || item_count == 2 * PARAM_VALUE (PARAM_IPA_MAX_AGG_ITEMS))
	break;
    }

  /* Third stage just goes over the list and creates an appropriate vector of
     ipa_agg_jf_item structures out of it, of sourse only if there are
     any known constants to begin with.  */

  if (const_count)
    {
      jfunc->agg.by_ref = by_ref;
      vec_alloc (jfunc->agg.items, const_count);
      while (list)
	{
	  if (list->constant)
	    {
	      struct ipa_agg_jf_item item;
	      item.offset = list->offset - arg_offset;
	      gcc_assert ((item.offset % BITS_PER_UNIT) == 0);
	      item.value = unshare_expr_without_location (list->constant);
	      jfunc->agg.items->quick_push (item);
	    }
	  list = list->next;
	}
    }
}

static tree
ipa_get_callee_param_type (struct cgraph_edge *e, int i)
{
  int n;
  tree type = (e->callee
	       ? TREE_TYPE (e->callee->decl)
	       : gimple_call_fntype (e->call_stmt));
  tree t = TYPE_ARG_TYPES (type);

  for (n = 0; n < i; n++)
    {
      if (!t)
        break;
      t = TREE_CHAIN (t);
    }
  if (t)
    return TREE_VALUE (t);
  if (!e->callee)
    return NULL;
  t = DECL_ARGUMENTS (e->callee->decl);
  for (n = 0; n < i; n++)
    {
      if (!t)
	return NULL;
      t = TREE_CHAIN (t);
    }
  if (t)
    return TREE_TYPE (t);
  return NULL;
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
  int n, arg_num = gimple_call_num_args (call);

  if (arg_num == 0 || args->jump_functions)
    return;
  vec_safe_grow_cleared (args->jump_functions, arg_num);

  if (gimple_call_internal_p (call))
    return;
  if (ipa_func_spec_opts_forbid_analysis_p (cs->caller))
    return;

  for (n = 0; n < arg_num; n++)
    {
      struct ipa_jump_func *jfunc = ipa_get_ith_jump_func (args, n);
      tree arg = gimple_call_arg (call, n);
      tree param_type = ipa_get_callee_param_type (cs, n);

      if (is_gimple_ip_invariant (arg))
	ipa_set_jf_constant (jfunc, arg, cs);
      else if (!is_gimple_reg_type (TREE_TYPE (arg))
	       && TREE_CODE (arg) == PARM_DECL)
	{
	  int index = ipa_get_param_decl_index (info, arg);

	  gcc_assert (index >=0);
	  /* Aggregate passed by value, check for pass-through, otherwise we
	     will attempt to fill in aggregate contents later in this
	     for cycle.  */
	  if (parm_preserved_before_stmt_p (&parms_ainfo[index], call, arg))
	    {
	      ipa_set_jf_simple_pass_through (jfunc, index, false, false);
	      continue;
	    }
	}
      else if (TREE_CODE (arg) == SSA_NAME)
	{
	  if (SSA_NAME_IS_DEFAULT_DEF (arg))
	    {
	      int index = ipa_get_param_decl_index (info, SSA_NAME_VAR (arg));
	      if (index >= 0)
		{
		  bool agg_p, type_p;
		  agg_p = parm_ref_data_pass_through_p (&parms_ainfo[index],
							call, arg);
		  if (param_type && POINTER_TYPE_P (param_type))
		    type_p = !detect_type_change_ssa (arg, TREE_TYPE (param_type),
						      call, jfunc);
		  else
		    type_p = false;
		  if (type_p || jfunc->type == IPA_JF_UNKNOWN)
		    ipa_set_jf_simple_pass_through (jfunc, index, agg_p,
						    type_p);
		}
	    }
	  else
	    {
	      gimple stmt = SSA_NAME_DEF_STMT (arg);
	      if (is_gimple_assign (stmt))
		compute_complex_assign_jump_func (info, parms_ainfo, jfunc,
						  call, stmt, arg, param_type);
	      else if (gimple_code (stmt) == GIMPLE_PHI)
		compute_complex_ancestor_jump_func (info, parms_ainfo, jfunc,
						    call, stmt, param_type);
	    }
	}
      else
	compute_known_type_jump_func (arg, jfunc, call,
				      param_type
				      && POINTER_TYPE_P (param_type)
				      ? TREE_TYPE (param_type)
				      : NULL);

      /* If ARG is pointer, we can not use its type to determine the type of aggregate
	 passed (because type conversions are ignored in gimple).  Usually we can
	 safely get type from function declaration, but in case of K&R prototypes or
	 variadic functions we can try our luck with type of the pointer passed.
	 TODO: Since we look for actual initialization of the memory object, we may better
	 work out the type based on the memory stores we find.  */
      if (!param_type)
	param_type = TREE_TYPE (arg);

      if ((jfunc->type != IPA_JF_PASS_THROUGH
	      || !ipa_get_jf_pass_through_agg_preserved (jfunc))
	  && (jfunc->type != IPA_JF_ANCESTOR
	      || !ipa_get_jf_ancestor_agg_preserved (jfunc))
	  && (AGGREGATE_TYPE_P (TREE_TYPE (arg))
	      || POINTER_TYPE_P (param_type)))
	determine_known_aggregate_parts (call, arg, param_type, jfunc);
    }
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
      if (!callee->definition && !flag_lto)
	continue;
      ipa_compute_jump_functions_for_edge (parms_ainfo, cs);
    }

  for (cs = node->indirect_calls; cs; cs = cs->next_callee)
    ipa_compute_jump_functions_for_edge (parms_ainfo, cs);
}

/* If STMT looks like a statement loading a value from a member pointer formal
   parameter, return that parameter and store the offset of the field to
   *OFFSET_P, if it is non-NULL.  Otherwise return NULL (but *OFFSET_P still
   might be clobbered).  If USE_DELTA, then we look for a use of the delta
   field rather than the pfn.  */

static tree
ipa_get_stmt_member_ptr_load_param (gimple stmt, bool use_delta,
				    HOST_WIDE_INT *offset_p)
{
  tree rhs, rec, ref_field, ref_offset, fld, ptr_field, delta_field;

  if (!gimple_assign_single_p (stmt))
    return NULL_TREE;

  rhs = gimple_assign_rhs1 (stmt);
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

  if (use_delta)
    fld = delta_field;
  else
    fld = ptr_field;
  if (offset_p)
    *offset_p = int_bit_position (fld);

  if (ref_field)
    {
      if (integer_nonzerop (ref_offset))
	return NULL_TREE;
      return ref_field == fld ? rec : NULL_TREE;
    }
  else
    return tree_int_cst_equal (byte_position (fld), ref_offset) ? rec
      : NULL_TREE;
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
  cs->indirect_info->agg_contents = 0;
  cs->indirect_info->member_ptr = 0;
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

   Moreover, the function also looks for called pointers loaded from aggregates
   passed by value or reference.  */

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
  HOST_WIDE_INT offset;
  bool by_ref;

  if (SSA_NAME_IS_DEFAULT_DEF (target))
    {
      tree var = SSA_NAME_VAR (target);
      index = ipa_get_param_decl_index (info, var);
      if (index >= 0)
	ipa_note_param_call (node, index, call);
      return;
    }

  def = SSA_NAME_DEF_STMT (target);
  if (gimple_assign_single_p (def)
      && ipa_load_from_parm_agg_1 (info->descriptors, parms_ainfo, def,
				   gimple_assign_rhs1 (def), &index, &offset,
				   NULL, &by_ref))
    {
      struct cgraph_edge *cs = ipa_note_param_call (node, index, call);
      if (cs->indirect_info->offset != offset)
	cs->indirect_info->outer_type = NULL;
      cs->indirect_info->offset = offset;
      cs->indirect_info->agg_contents = 1;
      cs->indirect_info->by_ref = by_ref;
      return;
    }

  /* Now we need to try to match the complex pattern of calling a member
     pointer. */
  if (gimple_code (def) != GIMPLE_PHI
      || gimple_phi_num_args (def) != 2
      || !POINTER_TYPE_P (TREE_TYPE (target))
      || TREE_CODE (TREE_TYPE (TREE_TYPE (target))) != METHOD_TYPE)
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
  if ((rec = ipa_get_stmt_member_ptr_load_param (d1, false, &offset)))
    {
      if (ipa_get_stmt_member_ptr_load_param (d2, false, NULL))
	return;

      bb = EDGE_PRED (join, 0)->src;
      virt_bb = gimple_bb (d2);
    }
  else if ((rec = ipa_get_stmt_member_ptr_load_param (d2, false, &offset)))
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
					      == ptrmemfunc_vbit_in_delta),
					     NULL);
  if (rec != rec2)
    return;

  index = ipa_get_param_decl_index (info, rec);
  if (index >= 0
      && parm_preserved_before_stmt_p (&parms_ainfo[index], call, rec))
    {
      struct cgraph_edge *cs = ipa_note_param_call (node, index, call);
      if (cs->indirect_info->offset != offset)
	cs->indirect_info->outer_type = NULL;
      cs->indirect_info->offset = offset;
      cs->indirect_info->agg_contents = 1;
      cs->indirect_info->member_ptr = 1;
    }

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
      if (detect_type_change_ssa (obj, obj_type_ref_class (target),
				  call, &jfunc))
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
      if (detect_type_change (obj, expr, obj_type_ref_class (target),
			      call, &jfunc, anc_offset))
	return;
    }

  cs = ipa_note_param_call (node, index, call);
  ii = cs->indirect_info;
  ii->offset = anc_offset;
  ii->otr_token = tree_to_uhwi (OBJ_TYPE_REF_TOKEN (target));
  ii->otr_type = obj_type_ref_class (target);
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
  struct cgraph_edge *cs;

  if (!target
      || (TREE_CODE (target) != SSA_NAME
          && !virtual_method_call_p (target)))
    return;

  /* If we previously turned the call into a direct call, there is
     no need to analyze.  */
  cs = cgraph_edge (node, call);
  if (cs && !cs->indirect_unknown_callee)
    return;
  if (TREE_CODE (target) == SSA_NAME)
    ipa_analyze_indirect_call_uses (node, info, parms_ainfo, call, target);
  else if (virtual_method_call_p (target))
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
visit_ref_for_mod_analysis (gimple, tree op, tree, void *data)
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
  tree decl = node->decl;
  basic_block bb;
  struct function *func;
  gimple_stmt_iterator gsi;
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int i;

  if (ipa_get_param_count (info) == 0 || info->uses_analysis_done)
    return;

  info->uses_analysis_done = 1;
  if (ipa_func_spec_opts_forbid_analysis_p (node))
    {
      for (i = 0; i < ipa_get_param_count (info); i++)
	{
	  ipa_set_param_used (info, i, true);
	  ipa_set_controlled_uses (info, i, IPA_UNDESCRIBED_USE);
	}
      return;
    }

  for (i = 0; i < ipa_get_param_count (info); i++)
    {
      tree parm = ipa_get_param (info, i);
      int controlled_uses = 0;

      /* For SSA regs see if parameter is used.  For non-SSA we compute
	 the flag during modification analysis.  */
      if (is_gimple_reg (parm))
	{
	  tree ddef = ssa_default_def (DECL_STRUCT_FUNCTION (node->decl),
				       parm);
	  if (ddef && !has_zero_uses (ddef))
	    {
	      imm_use_iterator imm_iter;
	      use_operand_p use_p;

	      ipa_set_param_used (info, i, true);
	      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, ddef)
		if (!is_gimple_call (USE_STMT (use_p)))
		  {
		    if (!is_gimple_debug (USE_STMT (use_p)))
		      {
			controlled_uses = IPA_UNDESCRIBED_USE;
			break;
		      }
		  }
		else
		  controlled_uses++;
	    }
	  else
	    controlled_uses = 0;
	}
      else
	controlled_uses = IPA_UNDESCRIBED_USE;
      ipa_set_controlled_uses (info, i, controlled_uses);
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
}

/* Free stuff in PARMS_AINFO, assume there are PARAM_COUNT parameters.  */

static void
free_parms_ainfo (struct param_analysis_info *parms_ainfo, int param_count)
{
  int i;

  for (i = 0; i < param_count; i++)
    {
      if (parms_ainfo[i].parm_visited_statements)
	BITMAP_FREE (parms_ainfo[i].parm_visited_statements);
      if (parms_ainfo[i].pt_visited_statements)
	BITMAP_FREE (parms_ainfo[i].pt_visited_statements);
    }
}

/* Initialize the array describing properties of of formal parameters
   of NODE, analyze their uses and compute jump functions associated
   with actual arguments of calls from within NODE.  */

void
ipa_analyze_node (struct cgraph_node *node)
{
  struct ipa_node_params *info;
  struct param_analysis_info *parms_ainfo;
  int param_count;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  info = IPA_NODE_REF (node);
  push_cfun (DECL_STRUCT_FUNCTION (node->decl));
  ipa_initialize_node_params (node);

  param_count = ipa_get_param_count (info);
  parms_ainfo = XALLOCAVEC (struct param_analysis_info, param_count);
  memset (parms_ainfo, 0, sizeof (struct param_analysis_info) * param_count);

  ipa_analyze_params_uses (node, parms_ainfo);
  ipa_compute_jump_functions (node, parms_ainfo);

  free_parms_ainfo (parms_ainfo, param_count);
  pop_cfun ();
}

/* Given a statement CALL which must be a GIMPLE_CALL calling an OBJ_TYPE_REF
   attempt a type-based devirtualization.  If successful, return the
   target function declaration, otherwise return NULL.  */

tree
ipa_intraprocedural_devirtualization (gimple call)
{
  tree binfo, token, fndecl;
  struct ipa_jump_func jfunc;
  tree otr = gimple_call_fn (call);

  jfunc.type = IPA_JF_UNKNOWN;
  compute_known_type_jump_func (OBJ_TYPE_REF_OBJECT (otr), &jfunc,
				call, obj_type_ref_class (otr));
  if (jfunc.type != IPA_JF_KNOWN_TYPE)
    return NULL_TREE;
  binfo = ipa_binfo_from_known_type_jfunc (&jfunc);
  if (!binfo)
    return NULL_TREE;
  token = OBJ_TYPE_REF_TOKEN (otr);
  fndecl = gimple_get_virt_method_for_binfo (tree_to_uhwi (token),
					     binfo);
#ifdef ENABLE_CHECKING
  if (fndecl)
    gcc_assert (possible_polymorphic_call_target_p
		  (otr, cgraph_get_node (fndecl)));
#endif
  return fndecl;
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

  if (!ipa_get_jf_ancestor_type_preserved (dst))
    {
      dst->type = IPA_JF_UNKNOWN;
      return;
    }

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
	  int dst_fid = dst->value.ancestor.formal_id;

	  /* Variable number of arguments can cause havoc if we try to access
	     one that does not exist in the inlined edge.  So make sure we
	     don't.  */
	  if (dst_fid >= ipa_get_cs_argument_count (top))
	    {
	      dst->type = IPA_JF_UNKNOWN;
	      continue;
	    }

	  src = ipa_get_ith_jump_func (top, dst_fid);

	  if (src->agg.items
	      && (dst->value.ancestor.agg_preserved || !src->agg.by_ref))
	    {
	      struct ipa_agg_jf_item *item;
	      int j;

	      /* Currently we do not produce clobber aggregate jump functions,
		 replace with merging when we do.  */
	      gcc_assert (!dst->agg.items);

	      dst->agg.items = vec_safe_copy (src->agg.items);
	      dst->agg.by_ref = src->agg.by_ref;
	      FOR_EACH_VEC_SAFE_ELT (dst->agg.items, j, item)
		item->offset -= dst->value.ancestor.offset;
	    }

	  if (src->type == IPA_JF_KNOWN_TYPE)
	    combine_known_type_and_ancestor_jfs (src, dst);
	  else if (src->type == IPA_JF_PASS_THROUGH
		   && src->value.pass_through.operation == NOP_EXPR)
	    {
	      dst->value.ancestor.formal_id = src->value.pass_through.formal_id;
	      dst->value.ancestor.agg_preserved &=
		src->value.pass_through.agg_preserved;
	      dst->value.ancestor.type_preserved &=
		src->value.pass_through.type_preserved;
	    }
	  else if (src->type == IPA_JF_ANCESTOR)
	    {
	      dst->value.ancestor.formal_id = src->value.ancestor.formal_id;
	      dst->value.ancestor.offset += src->value.ancestor.offset;
	      dst->value.ancestor.agg_preserved &=
		src->value.ancestor.agg_preserved;
	      dst->value.ancestor.type_preserved &=
		src->value.ancestor.type_preserved;
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
	      int dst_fid = dst->value.pass_through.formal_id;
	      src = ipa_get_ith_jump_func (top, dst_fid);
	      bool dst_agg_p = ipa_get_jf_pass_through_agg_preserved (dst);

	      switch (src->type)
		{
		case IPA_JF_UNKNOWN:
		  dst->type = IPA_JF_UNKNOWN;
		  break;
		case IPA_JF_KNOWN_TYPE:
		  if (ipa_get_jf_pass_through_type_preserved (dst))
		    ipa_set_jf_known_type (dst,
					   ipa_get_jf_known_type_offset (src),
					   ipa_get_jf_known_type_base_type (src),
					   ipa_get_jf_known_type_component_type (src));
		  else
		    dst->type = IPA_JF_UNKNOWN;
		  break;
		case IPA_JF_CONST:
		  ipa_set_jf_cst_copy (dst, src);
		  break;

		case IPA_JF_PASS_THROUGH:
		  {
		    int formal_id = ipa_get_jf_pass_through_formal_id (src);
		    enum tree_code operation;
		    operation = ipa_get_jf_pass_through_operation (src);

		    if (operation == NOP_EXPR)
		      {
			bool agg_p, type_p;
			agg_p = dst_agg_p
			  && ipa_get_jf_pass_through_agg_preserved (src);
			type_p = ipa_get_jf_pass_through_type_preserved (src)
			  && ipa_get_jf_pass_through_type_preserved (dst);
			ipa_set_jf_simple_pass_through (dst, formal_id,
							agg_p, type_p);
		      }
		    else
		      {
			tree operand = ipa_get_jf_pass_through_operand (src);
			ipa_set_jf_arith_pass_through (dst, formal_id, operand,
						       operation);
		      }
		    break;
		  }
		case IPA_JF_ANCESTOR:
		  {
		    bool agg_p, type_p;
		    agg_p = dst_agg_p
		      && ipa_get_jf_ancestor_agg_preserved (src);
		    type_p = ipa_get_jf_ancestor_type_preserved (src)
		      && ipa_get_jf_pass_through_type_preserved (dst);
		    ipa_set_ancestor_jf (dst,
					 ipa_get_jf_ancestor_offset (src),
					 ipa_get_jf_ancestor_type (src),
					 ipa_get_jf_ancestor_formal_id (src),
					 agg_p, type_p);
		    break;
		  }
		default:
		  gcc_unreachable ();
		}

	      if (src->agg.items
		  && (dst_agg_p || !src->agg.by_ref))
		{
		  /* Currently we do not produce clobber aggregate jump
		     functions, replace with merging when we do.  */
		  gcc_assert (!dst->agg.items);

		  dst->agg.by_ref = src->agg.by_ref;
		  dst->agg.items = vec_safe_copy (src->agg.items);
		}
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
  struct inline_edge_summary *es = inline_edge_summary (ie);
  bool unreachable = false;

  if (TREE_CODE (target) == ADDR_EXPR)
    target = TREE_OPERAND (target, 0);
  if (TREE_CODE (target) != FUNCTION_DECL)
    {
      target = canonicalize_constructor_val (target, NULL);
      if (!target || TREE_CODE (target) != FUNCTION_DECL)
	{
	  if (ie->indirect_info->member_ptr)
	    /* Member pointer call that goes through a VMT lookup.  */
	    return NULL;

	  if (dump_file)
	    fprintf (dump_file, "ipa-prop: Discovered direct call to non-function"
				" in %s/%i, making it unreachable.\n",
		     ie->caller->name (), ie->caller->order);
	  target = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
	  callee = cgraph_get_create_node (target);
	  unreachable = true;
	}
      else
	callee = cgraph_get_node (target);
    }
  else
    callee = cgraph_get_node (target);

  /* Because may-edges are not explicitely represented and vtable may be external,
     we may create the first reference to the object in the unit.  */
  if (!callee || callee->global.inlined_to)
    {

      /* We are better to ensure we can refer to it.
	 In the case of static functions we are out of luck, since we already	
	 removed its body.  In the case of public functions we may or may
	 not introduce the reference.  */
      if (!canonicalize_constructor_val (target, NULL)
	  || !TREE_PUBLIC (target))
	{
	  if (dump_file)
	    fprintf (dump_file, "ipa-prop: Discovered call to a known target "
		     "(%s/%i -> %s/%i) but can not refer to it. Giving up.\n",
		     xstrdup (ie->caller->name ()),
		     ie->caller->order,
		     xstrdup (ie->callee->name ()),
		     ie->callee->order);
	  return NULL;
	}
      callee = cgraph_get_create_node (target);
    }
  ipa_check_create_node_params ();

  /* We can not make edges to inline clones.  It is bug that someone removed
     the cgraph node too early.  */
  gcc_assert (!callee->global.inlined_to);

  if (dump_file && !unreachable)
    {
      fprintf (dump_file, "ipa-prop: Discovered %s call to a known target "
	       "(%s/%i -> %s/%i), for stmt ",
	       ie->indirect_info->polymorphic ? "a virtual" : "an indirect",
	       xstrdup (ie->caller->name ()),
	       ie->caller->order,
	       xstrdup (callee->name ()),
	       callee->order);
      if (ie->call_stmt)
	print_gimple_stmt (dump_file, ie->call_stmt, 2, TDF_SLIM);
      else
	fprintf (dump_file, "with uid %i\n", ie->lto_stmt_uid);
     }
  ie = cgraph_make_edge_direct (ie, callee);
  es = inline_edge_summary (ie);
  es->call_stmt_size -= (eni_size_weights.indirect_call_cost
			 - eni_size_weights.call_cost);
  es->call_stmt_time -= (eni_time_weights.indirect_call_cost
			 - eni_time_weights.call_cost);

  return ie;
}

/* Retrieve value from aggregate jump function AGG for the given OFFSET or
   return NULL if there is not any.  BY_REF specifies whether the value has to
   be passed by reference or by value.  */

tree
ipa_find_agg_cst_for_param (struct ipa_agg_jump_function *agg,
			    HOST_WIDE_INT offset, bool by_ref)
{
  struct ipa_agg_jf_item *item;
  int i;

  if (by_ref != agg->by_ref)
    return NULL;

  FOR_EACH_VEC_SAFE_ELT (agg->items, i, item)
    if (item->offset == offset)
      {
	/* Currently we do not have clobber values, return NULL for them once
	   we do.  */
	gcc_checking_assert (is_gimple_ip_invariant (item->value));
	return item->value;
      }
  return NULL;
}

/* Remove a reference to SYMBOL from the list of references of a node given by
   reference description RDESC.  Return true if the reference has been
   successfully found and removed.  */

static bool
remove_described_reference (symtab_node *symbol, struct ipa_cst_ref_desc *rdesc)
{
  struct ipa_ref *to_del;
  struct cgraph_edge *origin;

  origin = rdesc->cs;
  if (!origin)
    return false;
  to_del = ipa_find_reference (origin->caller, symbol,
			       origin->call_stmt, origin->lto_stmt_uid);
  if (!to_del)
    return false;

  ipa_remove_reference (to_del);
  if (dump_file)
    fprintf (dump_file, "ipa-prop: Removed a reference from %s/%i to %s.\n",
	     xstrdup (origin->caller->name ()),
	     origin->caller->order, xstrdup (symbol->name ()));
  return true;
}

/* If JFUNC has a reference description with refcount different from
   IPA_UNDESCRIBED_USE, return the reference description, otherwise return
   NULL.  JFUNC must be a constant jump function.  */

static struct ipa_cst_ref_desc *
jfunc_rdesc_usable (struct ipa_jump_func *jfunc)
{
  struct ipa_cst_ref_desc *rdesc = ipa_get_jf_constant_rdesc (jfunc);
  if (rdesc && rdesc->refcount != IPA_UNDESCRIBED_USE)
    return rdesc;
  else
    return NULL;
}

/* If the value of constant jump function JFUNC is an address of a function
   declaration, return the associated call graph node.  Otherwise return
   NULL.  */

static cgraph_node *
cgraph_node_for_jfunc (struct ipa_jump_func *jfunc)
{
  gcc_checking_assert (jfunc->type == IPA_JF_CONST);
  tree cst = ipa_get_jf_constant (jfunc);
  if (TREE_CODE (cst) != ADDR_EXPR
      || TREE_CODE (TREE_OPERAND (cst, 0)) != FUNCTION_DECL)
    return NULL;

  return cgraph_get_node (TREE_OPERAND (cst, 0));
}


/* If JFUNC is a constant jump function with a usable rdesc, decrement its
   refcount and if it hits zero, remove reference to SYMBOL from the caller of
   the edge specified in the rdesc.  Return false if either the symbol or the
   reference could not be found, otherwise return true.  */

static bool
try_decrement_rdesc_refcount (struct ipa_jump_func *jfunc)
{
  struct ipa_cst_ref_desc *rdesc;
  if (jfunc->type == IPA_JF_CONST
      && (rdesc = jfunc_rdesc_usable (jfunc))
      && --rdesc->refcount == 0)
    {
      symtab_node *symbol = cgraph_node_for_jfunc (jfunc);
      if (!symbol)
	return false;

      return remove_described_reference (symbol, rdesc);
    }
  return true;
}

/* Try to find a destination for indirect edge IE that corresponds to a simple
   call or a call of a member function pointer and where the destination is a
   pointer formal parameter described by jump function JFUNC.  If it can be
   determined, return the newly direct edge, otherwise return NULL.
   NEW_ROOT_INFO is the node info that JFUNC lattices are relative to.  */

static struct cgraph_edge *
try_make_edge_direct_simple_call (struct cgraph_edge *ie,
				  struct ipa_jump_func *jfunc,
				  struct ipa_node_params *new_root_info)
{
  struct cgraph_edge *cs;
  tree target;
  bool agg_contents = ie->indirect_info->agg_contents;

  if (ie->indirect_info->agg_contents)
    target = ipa_find_agg_cst_for_param (&jfunc->agg,
					 ie->indirect_info->offset,
					 ie->indirect_info->by_ref);
  else
    target = ipa_value_from_jfunc (new_root_info, jfunc);
  if (!target)
    return NULL;
  cs = ipa_make_edge_direct_to_target (ie, target);

  if (cs && !agg_contents)
    {
      bool ok;
      gcc_checking_assert (cs->callee
			   && (cs != ie
			       || jfunc->type != IPA_JF_CONST
			       || !cgraph_node_for_jfunc (jfunc)
			       || cs->callee == cgraph_node_for_jfunc (jfunc)));
      ok = try_decrement_rdesc_refcount (jfunc);
      gcc_checking_assert (ok);
    }

  return cs;
}

/* Try to find a destination for indirect edge IE that corresponds to a virtual
   call based on a formal parameter which is described by jump function JFUNC
   and if it can be determined, make it direct and return the direct edge.
   Otherwise, return NULL.  NEW_ROOT_INFO is the node info that JFUNC lattices
   are relative to.  */

static struct cgraph_edge *
try_make_edge_direct_virtual_call (struct cgraph_edge *ie,
				   struct ipa_jump_func *jfunc,
				   struct ipa_node_params *new_root_info)
{
  tree binfo, target;

  if (!flag_devirtualize)
    return NULL;

  /* First try to do lookup via known virtual table pointer value.  */
  if (!ie->indirect_info->by_ref)
    {
      tree vtable;
      unsigned HOST_WIDE_INT offset;
      tree t = ipa_find_agg_cst_for_param (&jfunc->agg,
					   ie->indirect_info->offset,
					   true);
      if (t && vtable_pointer_value_to_vtable (t, &vtable, &offset))
	{
	  target = gimple_get_virt_method_for_vtable (ie->indirect_info->otr_token,
						      vtable, offset);
	  if (target)
	    {
	      if ((TREE_CODE (TREE_TYPE (target)) == FUNCTION_TYPE
		   && DECL_FUNCTION_CODE (target) == BUILT_IN_UNREACHABLE)
		  || !possible_polymorphic_call_target_p
		       (ie, cgraph_get_node (target)))
		{
		  if (dump_file)
		    fprintf (dump_file,
			     "Type inconsident devirtualization: %s/%i->%s\n",
			     ie->caller->name (), ie->caller->order,
			     IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (target)));
		  target = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
		  cgraph_get_create_node (target);
		}
	      return ipa_make_edge_direct_to_target (ie, target);
	    }
	}
    }

  binfo = ipa_value_from_jfunc (new_root_info, jfunc);

  if (!binfo)
    return NULL;

  if (TREE_CODE (binfo) != TREE_BINFO)
    {
      ipa_polymorphic_call_context context;
      vec <cgraph_node *>targets;
      bool final;

      if (!get_polymorphic_call_info_from_invariant
	     (&context, binfo, ie->indirect_info->otr_type,
	      ie->indirect_info->offset))
	return NULL;
      targets = possible_polymorphic_call_targets
		 (ie->indirect_info->otr_type,
		  ie->indirect_info->otr_token,
		  context, &final);
      if (!final || targets.length () > 1)
	return NULL;
      if (targets.length () == 1)
	target = targets[0]->decl;
      else
	{
          target = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
	  cgraph_get_create_node (target);
	}
    }
  else
    {
      binfo = get_binfo_at_offset (binfo, ie->indirect_info->offset,
				   ie->indirect_info->otr_type);
      if (binfo)
	target = gimple_get_virt_method_for_binfo (ie->indirect_info->otr_token,
						   binfo);
      else
	return NULL;
    }

  if (target)
    {
#ifdef ENABLE_CHECKING
      gcc_assert (possible_polymorphic_call_target_p
	 (ie, cgraph_get_node (target)));
#endif
      return ipa_make_edge_direct_to_target (ie, target);
    }
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
				      vec<cgraph_edge_p> *new_edges)
{
  struct ipa_edge_args *top;
  struct cgraph_edge *ie, *next_ie, *new_direct_edge;
  struct ipa_node_params *new_root_info;
  bool res = false;

  ipa_check_create_edge_args ();
  top = IPA_EDGE_REF (cs);
  new_root_info = IPA_NODE_REF (cs->caller->global.inlined_to
				? cs->caller->global.inlined_to
				: cs->caller);

  for (ie = node->indirect_calls; ie; ie = next_ie)
    {
      struct cgraph_indirect_call_info *ici = ie->indirect_info;
      struct ipa_jump_func *jfunc;
      int param_index;

      next_ie = ie->next_callee;

      if (ici->param_index == -1)
	continue;

      /* We must check range due to calls with variable number of arguments:  */
      if (ici->param_index >= ipa_get_cs_argument_count (top))
	{
	  ici->param_index = -1;
	  continue;
	}

      param_index = ici->param_index;
      jfunc = ipa_get_ith_jump_func (top, param_index);

      if (!flag_indirect_inlining)
	new_direct_edge = NULL;
      else if (ici->polymorphic)
	new_direct_edge = try_make_edge_direct_virtual_call (ie, jfunc,
							     new_root_info);
      else
	new_direct_edge = try_make_edge_direct_simple_call (ie, jfunc,
							    new_root_info);
      /* If speculation was removed, then we need to do nothing.  */
      if (new_direct_edge && new_direct_edge != ie)
	{
	  new_direct_edge->indirect_inlining_edge = 1;
	  top = IPA_EDGE_REF (cs);
	  res = true;
	}
      else if (new_direct_edge)
	{
	  new_direct_edge->indirect_inlining_edge = 1;
	  if (new_direct_edge->call_stmt)
	    new_direct_edge->call_stmt_cannot_inline_p
	      = !gimple_check_call_matching_types (
		  new_direct_edge->call_stmt,
		  new_direct_edge->callee->decl, false);
	  if (new_edges)
	    {
	      new_edges->safe_push (new_direct_edge);
	      res = true;
	    }
	  top = IPA_EDGE_REF (cs);
	}
      else if (jfunc->type == IPA_JF_PASS_THROUGH
	       && ipa_get_jf_pass_through_operation (jfunc) == NOP_EXPR)
	{
	  if (ici->agg_contents
	      && !ipa_get_jf_pass_through_agg_preserved (jfunc))
	    ici->param_index = -1;
	  else
	    ici->param_index = ipa_get_jf_pass_through_formal_id (jfunc);
	}
      else if (jfunc->type == IPA_JF_ANCESTOR)
	{
	  if (ici->agg_contents
	      && !ipa_get_jf_ancestor_agg_preserved (jfunc))
	    ici->param_index = -1;
	  else
	    {
	      ici->param_index = ipa_get_jf_ancestor_formal_id (jfunc);
	      if (ipa_get_jf_ancestor_offset (jfunc))
	        ici->outer_type = NULL;
	      ici->offset += ipa_get_jf_ancestor_offset (jfunc);
	    }
	}
      else
	/* Either we can find a destination for this edge now or never. */
	ici->param_index = -1;
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
				   vec<cgraph_edge_p> *new_edges)
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

/* Combine two controlled uses counts as done during inlining.  */

static int
combine_controlled_uses_counters (int c, int d)
{
  if (c == IPA_UNDESCRIBED_USE || d == IPA_UNDESCRIBED_USE)
    return IPA_UNDESCRIBED_USE;
  else
    return c + d - 1;
}

/* Propagate number of controlled users from CS->caleee to the new root of the
   tree of inlined nodes.  */

static void
propagate_controlled_uses (struct cgraph_edge *cs)
{
  struct ipa_edge_args *args = IPA_EDGE_REF (cs);
  struct cgraph_node *new_root = cs->caller->global.inlined_to
    ? cs->caller->global.inlined_to : cs->caller;
  struct ipa_node_params *new_root_info = IPA_NODE_REF (new_root);
  struct ipa_node_params *old_root_info = IPA_NODE_REF (cs->callee);
  int count, i;

  count = MIN (ipa_get_cs_argument_count (args),
	       ipa_get_param_count (old_root_info));
  for (i = 0; i < count; i++)
    {
      struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);
      struct ipa_cst_ref_desc *rdesc;

      if (jf->type == IPA_JF_PASS_THROUGH)
	{
	  int src_idx, c, d;
	  src_idx = ipa_get_jf_pass_through_formal_id (jf);
	  c = ipa_get_controlled_uses (new_root_info, src_idx);
	  d = ipa_get_controlled_uses (old_root_info, i);

	  gcc_checking_assert (ipa_get_jf_pass_through_operation (jf)
			       == NOP_EXPR || c == IPA_UNDESCRIBED_USE);
	  c = combine_controlled_uses_counters (c, d);
	  ipa_set_controlled_uses (new_root_info, src_idx, c);
	  if (c == 0 && new_root_info->ipcp_orig_node)
	    {
	      struct cgraph_node *n;
	      struct ipa_ref *ref;
	      tree t = new_root_info->known_vals[src_idx];

	      if (t && TREE_CODE (t) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
		  && (n = cgraph_get_node (TREE_OPERAND (t, 0)))
		  && (ref = ipa_find_reference (new_root,
						n, NULL, 0)))
		{
		  if (dump_file)
		    fprintf (dump_file, "ipa-prop: Removing cloning-created "
			     "reference from %s/%i to %s/%i.\n",
			     xstrdup (new_root->name ()),
			     new_root->order,
			     xstrdup (n->name ()), n->order);
		  ipa_remove_reference (ref);
		}
	    }
	}
      else if (jf->type == IPA_JF_CONST
	       && (rdesc = jfunc_rdesc_usable (jf)))
	{
	  int d = ipa_get_controlled_uses (old_root_info, i);
	  int c = rdesc->refcount;
	  rdesc->refcount = combine_controlled_uses_counters (c, d);
	  if (rdesc->refcount == 0)
	    {
	      tree cst = ipa_get_jf_constant (jf);
	      struct cgraph_node *n;
	      gcc_checking_assert (TREE_CODE (cst) == ADDR_EXPR
				   && TREE_CODE (TREE_OPERAND (cst, 0))
				   == FUNCTION_DECL);
	      n = cgraph_get_node (TREE_OPERAND (cst, 0));
	      if (n)
		{
		  struct cgraph_node *clone;
		  bool ok;
		  ok = remove_described_reference (n, rdesc);
		  gcc_checking_assert (ok);

		  clone = cs->caller;
		  while (clone->global.inlined_to
			 && clone != rdesc->cs->caller
			 && IPA_NODE_REF (clone)->ipcp_orig_node)
		    {
		      struct ipa_ref *ref;
		      ref = ipa_find_reference (clone,
						n, NULL, 0);
		      if (ref)
			{
			  if (dump_file)
			    fprintf (dump_file, "ipa-prop: Removing "
				     "cloning-created reference "
				     "from %s/%i to %s/%i.\n",
				     xstrdup (clone->name ()),
				     clone->order,
				     xstrdup (n->name ()),
				     n->order);
			  ipa_remove_reference (ref);
			}
		      clone = clone->callers->caller;
		    }
		}
	    }
	}
    }

  for (i = ipa_get_param_count (old_root_info);
       i < ipa_get_cs_argument_count (args);
       i++)
    {
      struct ipa_jump_func *jf = ipa_get_ith_jump_func (args, i);

      if (jf->type == IPA_JF_CONST)
	{
	  struct ipa_cst_ref_desc *rdesc = jfunc_rdesc_usable (jf);
	  if (rdesc)
	    rdesc->refcount = IPA_UNDESCRIBED_USE;
	}
      else if (jf->type == IPA_JF_PASS_THROUGH)
	ipa_set_controlled_uses (new_root_info,
				 jf->value.pass_through.formal_id,
				 IPA_UNDESCRIBED_USE);
    }
}

/* Update jump functions and call note functions on inlining the call site CS.
   CS is expected to lead to a node already cloned by
   cgraph_clone_inline_nodes.  Newly discovered indirect edges will be added to
   *NEW_EDGES, unless NEW_EDGES is NULL.  Return true iff a new edge(s) were +
   created.  */

bool
ipa_propagate_indirect_call_infos (struct cgraph_edge *cs,
				   vec<cgraph_edge_p> *new_edges)
{
  bool changed;
  /* Do nothing if the preparation phase has not been carried out yet
     (i.e. during early inlining).  */
  if (!ipa_node_params_vector.exists ())
    return false;
  gcc_assert (ipa_edge_args_vector);

  propagate_controlled_uses (cs);
  changed = propagate_info_to_inlined_callees (cs, cs->callee, new_edges);

  return changed;
}

/* Frees all dynamically allocated structures that the argument info points
   to.  */

void
ipa_free_edge_args_substructures (struct ipa_edge_args *args)
{
  vec_free (args->jump_functions);
  memset (args, 0, sizeof (*args));
}

/* Free all ipa_edge structures.  */

void
ipa_free_all_edge_args (void)
{
  int i;
  struct ipa_edge_args *args;

  if (!ipa_edge_args_vector)
    return;

  FOR_EACH_VEC_ELT (*ipa_edge_args_vector, i, args)
    ipa_free_edge_args_substructures (args);

  vec_free (ipa_edge_args_vector);
}

/* Frees all dynamically allocated structures that the param info points
   to.  */

void
ipa_free_node_params_substructures (struct ipa_node_params *info)
{
  info->descriptors.release ();
  free (info->lattices);
  /* Lattice values and their sources are deallocated with their alocation
     pool.  */
  info->known_vals.release ();
  memset (info, 0, sizeof (*info));
}

/* Free all ipa_node_params structures.  */

void
ipa_free_all_node_params (void)
{
  int i;
  struct ipa_node_params *info;

  FOR_EACH_VEC_ELT (ipa_node_params_vector, i, info)
    ipa_free_node_params_substructures (info);

  ipa_node_params_vector.release ();
}

/* Set the aggregate replacements of NODE to be AGGVALS.  */

void
ipa_set_node_agg_value_chain (struct cgraph_node *node,
			      struct ipa_agg_replacement_value *aggvals)
{
  if (vec_safe_length (ipa_node_agg_replacements) <= (unsigned) cgraph_max_uid)
    vec_safe_grow_cleared (ipa_node_agg_replacements, cgraph_max_uid + 1);

  (*ipa_node_agg_replacements)[node->uid] = aggvals;
}

/* Hook that is called by cgraph.c when an edge is removed.  */

static void
ipa_edge_removal_hook (struct cgraph_edge *cs, void *data ATTRIBUTE_UNUSED)
{
  struct ipa_edge_args *args;

  /* During IPA-CP updating we can be called on not-yet analyzed clones.  */
  if (vec_safe_length (ipa_edge_args_vector) <= (unsigned)cs->uid)
    return;

  args = IPA_EDGE_REF (cs);
  if (args->jump_functions)
    {
      struct ipa_jump_func *jf;
      int i;
      FOR_EACH_VEC_ELT (*args->jump_functions, i, jf)
	{
	  struct ipa_cst_ref_desc *rdesc;
	  try_decrement_rdesc_refcount (jf);
	  if (jf->type == IPA_JF_CONST
	      && (rdesc = ipa_get_jf_constant_rdesc (jf))
	      && rdesc->cs == cs)
	    rdesc->cs = NULL;
	}
    }

  ipa_free_edge_args_substructures (IPA_EDGE_REF (cs));
}

/* Hook that is called by cgraph.c when a node is removed.  */

static void
ipa_node_removal_hook (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  /* During IPA-CP updating we can be called on not-yet analyze clones.  */
  if (ipa_node_params_vector.length () > (unsigned)node->uid)
    ipa_free_node_params_substructures (IPA_NODE_REF (node));
  if (vec_safe_length (ipa_node_agg_replacements) > (unsigned)node->uid)
    (*ipa_node_agg_replacements)[(unsigned)node->uid] = NULL;
}

/* Hook that is called by cgraph.c when an edge is duplicated.  */

static void
ipa_edge_duplication_hook (struct cgraph_edge *src, struct cgraph_edge *dst,
			   __attribute__((unused)) void *data)
{
  struct ipa_edge_args *old_args, *new_args;
  unsigned int i;

  ipa_check_create_edge_args ();

  old_args = IPA_EDGE_REF (src);
  new_args = IPA_EDGE_REF (dst);

  new_args->jump_functions = vec_safe_copy (old_args->jump_functions);

  for (i = 0; i < vec_safe_length (old_args->jump_functions); i++)
    {
      struct ipa_jump_func *src_jf = ipa_get_ith_jump_func (old_args, i);
      struct ipa_jump_func *dst_jf = ipa_get_ith_jump_func (new_args, i);

      dst_jf->agg.items = vec_safe_copy (dst_jf->agg.items);

      if (src_jf->type == IPA_JF_CONST)
	{
	  struct ipa_cst_ref_desc *src_rdesc = jfunc_rdesc_usable (src_jf);

	  if (!src_rdesc)
	    dst_jf->value.constant.rdesc = NULL;
	  else if (src->caller == dst->caller)
	    {
	      struct ipa_ref *ref;
	      symtab_node *n = cgraph_node_for_jfunc (src_jf);
	      gcc_checking_assert (n);
	      ref = ipa_find_reference (src->caller, n,
					src->call_stmt, src->lto_stmt_uid);
	      gcc_checking_assert (ref);
	      ipa_clone_ref (ref, dst->caller, ref->stmt);

	      gcc_checking_assert (ipa_refdesc_pool);
	      struct ipa_cst_ref_desc *dst_rdesc
		= (struct ipa_cst_ref_desc *) pool_alloc (ipa_refdesc_pool);
	      dst_rdesc->cs = dst;
	      dst_rdesc->refcount = src_rdesc->refcount;
	      dst_rdesc->next_duplicate = NULL;
	      dst_jf->value.constant.rdesc = dst_rdesc;
	    }
	  else if (src_rdesc->cs == src)
	    {
	      struct ipa_cst_ref_desc *dst_rdesc;
	      gcc_checking_assert (ipa_refdesc_pool);
	      dst_rdesc
		= (struct ipa_cst_ref_desc *) pool_alloc (ipa_refdesc_pool);
	      dst_rdesc->cs = dst;
	      dst_rdesc->refcount = src_rdesc->refcount;
	      dst_rdesc->next_duplicate = src_rdesc->next_duplicate;
	      src_rdesc->next_duplicate = dst_rdesc;
	      dst_jf->value.constant.rdesc = dst_rdesc;
	    }
	  else
	    {
	      struct ipa_cst_ref_desc *dst_rdesc;
	      /* This can happen during inlining, when a JFUNC can refer to a
		 reference taken in a function up in the tree of inline clones.
		 We need to find the duplicate that refers to our tree of
		 inline clones.  */

	      gcc_assert (dst->caller->global.inlined_to);
	      for (dst_rdesc = src_rdesc->next_duplicate;
		   dst_rdesc;
		   dst_rdesc = dst_rdesc->next_duplicate)
		{
		  struct cgraph_node *top;
		  top = dst_rdesc->cs->caller->global.inlined_to
		    ? dst_rdesc->cs->caller->global.inlined_to
		    : dst_rdesc->cs->caller;
		  if (dst->caller->global.inlined_to == top)
		    break;
		}
	      gcc_assert (dst_rdesc);
	      dst_jf->value.constant.rdesc = dst_rdesc;
	    }
	}
    }
}

/* Hook that is called by cgraph.c when a node is duplicated.  */

static void
ipa_node_duplication_hook (struct cgraph_node *src, struct cgraph_node *dst,
			   ATTRIBUTE_UNUSED void *data)
{
  struct ipa_node_params *old_info, *new_info;
  struct ipa_agg_replacement_value *old_av, *new_av;

  ipa_check_create_node_params ();
  old_info = IPA_NODE_REF (src);
  new_info = IPA_NODE_REF (dst);

  new_info->descriptors = old_info->descriptors.copy ();
  new_info->lattices = NULL;
  new_info->ipcp_orig_node = old_info->ipcp_orig_node;

  new_info->uses_analysis_done = old_info->uses_analysis_done;
  new_info->node_enqueued = old_info->node_enqueued;

  old_av = ipa_get_agg_replacements_for_node (src);
  if (!old_av)
    return;

  new_av = NULL;
  while (old_av)
    {
      struct ipa_agg_replacement_value *v;

      v = ggc_alloc_ipa_agg_replacement_value ();
      memcpy (v, old_av, sizeof (*v));
      v->next = new_av;
      new_av = v;
      old_av = old_av->next;
    }
  ipa_set_node_agg_value_chain (dst, new_av);
}


/* Analyze newly added function into callgraph.  */

static void
ipa_add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (cgraph_function_with_gimple_body_p (node))
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
      free_alloc_pool (ipcp_agg_lattice_pool);
      ipa_unregister_cgraph_hooks ();
      if (ipa_refdesc_pool)
	free_alloc_pool (ipa_refdesc_pool);
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
  if (ipcp_agg_lattice_pool)
    free_alloc_pool (ipcp_agg_lattice_pool);
  if (ipa_refdesc_pool)
    free_alloc_pool (ipa_refdesc_pool);
}

/* Print ipa_tree_map data structures of all functions in the
   callgraph to F.  */

void
ipa_print_node_params (FILE *f, struct cgraph_node *node)
{
  int i, count;
  struct ipa_node_params *info;

  if (!node->definition)
    return;
  info = IPA_NODE_REF (node);
  fprintf (f, "  function  %s/%i parameter descriptors:\n",
	   node->name (), node->order);
  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    {
      int c;

      fprintf (f, "    ");
      ipa_dump_param (f, info, i);
      if (ipa_is_param_used (info, i))
	fprintf (f, " used");
      c = ipa_get_controlled_uses (info, i);
      if (c == IPA_UNDESCRIBED_USE)
	fprintf (f, " undescribed_use");
      else
	fprintf (f, "  controlled_uses=%i", c);
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

vec<tree> 
ipa_get_vector_of_formal_parms (tree fndecl)
{
  vec<tree> args;
  int count;
  tree parm;

  gcc_assert (!flag_wpa);
  count = count_formal_params (fndecl);
  args.create (count);
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    args.quick_push (parm);

  return args;
}

/* Return a heap allocated vector containing types of formal parameters of
   function type FNTYPE.  */

vec<tree>
ipa_get_vector_of_formal_parm_types (tree fntype)
{
  vec<tree> types;
  int count = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    count++;

  types.create (count);
  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    types.quick_push (TREE_VALUE (t));

  return types;
}

/* Modify the function declaration FNDECL and its type according to the plan in
   ADJUSTMENTS.  It also sets base fields of individual adjustments structures
   to reflect the actual parameters being modified which are determined by the
   base_index field.  */

void
ipa_modify_formal_parameters (tree fndecl, ipa_parm_adjustment_vec adjustments)
{
  vec<tree> oparms = ipa_get_vector_of_formal_parms (fndecl);
  tree orig_type = TREE_TYPE (fndecl);
  tree old_arg_types = TYPE_ARG_TYPES (orig_type);

  /* The following test is an ugly hack, some functions simply don't have any
     arguments in their type.  This is probably a bug but well... */
  bool care_for_types = (old_arg_types != NULL_TREE);
  bool last_parm_void;
  vec<tree> otypes;
  if (care_for_types)
    {
      last_parm_void = (TREE_VALUE (tree_last (old_arg_types))
			== void_type_node);
      otypes = ipa_get_vector_of_formal_parm_types (orig_type);
      if (last_parm_void)
	gcc_assert (oparms.length () + 1 == otypes.length ());
      else
	gcc_assert (oparms.length () == otypes.length ());
    }
  else
    {
      last_parm_void = false;
      otypes.create (0);
    }

  int len = adjustments.length ();
  tree *link = &DECL_ARGUMENTS (fndecl);
  tree new_arg_types = NULL;
  for (int i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      gcc_assert (link);

      adj = &adjustments[i];
      tree parm;
      if (adj->op == IPA_PARM_OP_NEW)
	parm = NULL;
      else
	parm = oparms[adj->base_index];
      adj->base = parm;

      if (adj->op == IPA_PARM_OP_COPY)
	{
	  if (care_for_types)
	    new_arg_types = tree_cons (NULL_TREE, otypes[adj->base_index],
				       new_arg_types);
	  *link = parm;
	  link = &DECL_CHAIN (parm);
	}
      else if (adj->op != IPA_PARM_OP_REMOVE)
	{
	  tree new_parm;
	  tree ptype;

	  if (adj->by_ref)
	    ptype = build_pointer_type (adj->type);
	  else
	    {
	      ptype = adj->type;
	      if (is_gimple_reg_type (ptype))
		{
		  unsigned malign = GET_MODE_ALIGNMENT (TYPE_MODE (ptype));
		  if (TYPE_ALIGN (ptype) < malign)
		    ptype = build_aligned_type (ptype, malign);
		}
	    }

	  if (care_for_types)
	    new_arg_types = tree_cons (NULL_TREE, ptype, new_arg_types);

	  new_parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE,
				 ptype);
	  const char *prefix = adj->arg_prefix ? adj->arg_prefix : "SYNTH";
	  DECL_NAME (new_parm) = create_tmp_var_name (prefix);
	  DECL_ARTIFICIAL (new_parm) = 1;
	  DECL_ARG_TYPE (new_parm) = ptype;
	  DECL_CONTEXT (new_parm) = fndecl;
	  TREE_USED (new_parm) = 1;
	  DECL_IGNORED_P (new_parm) = 1;
	  layout_decl (new_parm, 0);

	  if (adj->op == IPA_PARM_OP_NEW)
	    adj->base = NULL;
	  else
	    adj->base = parm;
	  adj->new_decl = new_parm;

	  *link = new_parm;
	  link = &DECL_CHAIN (new_parm);
	}
    }

  *link = NULL_TREE;

  tree new_reversed = NULL;
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
  tree new_type = NULL;
  if (TREE_CODE (orig_type) != METHOD_TYPE
       || (adjustments[0].op == IPA_PARM_OP_COPY
	  && adjustments[0].base_index == 0))
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
  tree t = TYPE_MAIN_VARIANT (orig_type);
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
  otypes.release ();
  oparms.release ();
}

/* Modify actual arguments of a function call CS as indicated in ADJUSTMENTS.
   If this is a directly recursive call, CS must be NULL.  Otherwise it must
   contain the corresponding call graph edge.  */

void
ipa_modify_call_arguments (struct cgraph_edge *cs, gimple stmt,
			   ipa_parm_adjustment_vec adjustments)
{
  struct cgraph_node *current_node = cgraph_get_node (current_function_decl);
  vec<tree> vargs;
  vec<tree, va_gc> **debug_args = NULL;
  gimple new_stmt;
  gimple_stmt_iterator gsi, prev_gsi;
  tree callee_decl;
  int i, len;

  len = adjustments.length ();
  vargs.create (len);
  callee_decl = !cs ? gimple_call_fndecl (stmt) : cs->callee->decl;
  ipa_remove_stmt_references (current_node, stmt);

  gsi = gsi_for_stmt (stmt);
  prev_gsi = gsi;
  gsi_prev (&prev_gsi);
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;

      adj = &adjustments[i];

      if (adj->op == IPA_PARM_OP_COPY)
	{
	  tree arg = gimple_call_arg (stmt, adj->base_index);

	  vargs.quick_push (arg);
	}
      else if (adj->op != IPA_PARM_OP_REMOVE)
	{
	  tree expr, base, off;
	  location_t loc;
	  unsigned int deref_align = 0;
	  bool deref_base = false;

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
	  loc = DECL_P (base) ? DECL_SOURCE_LOCATION (base)
			      : EXPR_LOCATION (base);

	  if (TREE_CODE (base) != ADDR_EXPR
	      && POINTER_TYPE_P (TREE_TYPE (base)))
	    off = build_int_cst (adj->alias_ptr_type,
				 adj->offset / BITS_PER_UNIT);
	  else
	    {
	      HOST_WIDE_INT base_offset;
	      tree prev_base;
	      bool addrof;

	      if (TREE_CODE (base) == ADDR_EXPR)
		{
		  base = TREE_OPERAND (base, 0);
		  addrof = true;
		}
	      else
		addrof = false;
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
		  if (!addrof)
		    {
		      deref_base = true;
		      deref_align = TYPE_ALIGN (TREE_TYPE (base));
		    }
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

	      if (deref_base)
		{
		  align = deref_align;
		  misalign = 0;
		}
	      else
		{
		  get_pointer_alignment_1 (base, &align, &misalign);
		  if (TYPE_ALIGN (type) > align)
		    align = TYPE_ALIGN (type);
		}
	      misalign += (offset_int::from (off, SIGNED).to_short_addr ()
			   * BITS_PER_UNIT);
	      misalign = misalign & (align - 1);
	      if (misalign != 0)
		align = (misalign & -misalign);
	      if (align < TYPE_ALIGN (type))
		type = build_aligned_type (type, align);
	      base = force_gimple_operand_gsi (&gsi, base,
					       true, NULL, true, GSI_SAME_STMT);
	      expr = fold_build2_loc (loc, MEM_REF, type, base, off);
	      /* If expr is not a valid gimple call argument emit
	         a load into a temporary.  */
	      if (is_gimple_reg_type (TREE_TYPE (expr)))
		{
		  gimple tem = gimple_build_assign (NULL_TREE, expr);
		  if (gimple_in_ssa_p (cfun))
		    {
		      gimple_set_vuse (tem, gimple_vuse (stmt));
		      expr = make_ssa_name (TREE_TYPE (expr), tem);
		    }
		  else
		    expr = create_tmp_reg (TREE_TYPE (expr), NULL);
		  gimple_assign_set_lhs (tem, expr);
		  gsi_insert_before (&gsi, tem, GSI_SAME_STMT);
		}
	    }
	  else
	    {
	      expr = fold_build2_loc (loc, MEM_REF, adj->type, base, off);
	      expr = build_fold_addr_expr (expr);
	      expr = force_gimple_operand_gsi (&gsi, expr,
					       true, NULL, true, GSI_SAME_STMT);
	    }
	  vargs.quick_push (expr);
	}
      if (adj->op != IPA_PARM_OP_COPY && MAY_HAVE_DEBUG_STMTS)
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
	  for (ix = 0; vec_safe_iterate (*debug_args, ix, &ddecl); ix += 2)
	    if (ddecl == origin)
	      {
		ddecl = (**debug_args)[ix + 1];
		break;
	      }
	  if (ddecl == NULL)
	    {
	      ddecl = make_node (DEBUG_EXPR_DECL);
	      DECL_ARTIFICIAL (ddecl) = 1;
	      TREE_TYPE (ddecl) = TREE_TYPE (origin);
	      DECL_MODE (ddecl) = DECL_MODE (origin);

	      vec_safe_push (*debug_args, origin);
	      vec_safe_push (*debug_args, ddecl);
	    }
	  def_temp = gimple_build_debug_bind (ddecl, unshare_expr (arg), stmt);
	  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "replacing stmt:");
      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, 0);
    }

  new_stmt = gimple_build_call_vec (callee_decl, vargs);
  vargs.release ();
  if (gimple_call_lhs (stmt))
    gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

  gimple_set_block (new_stmt, gimple_block (stmt));
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  if (gimple_in_ssa_p (cfun))
    {
      gimple_set_vuse (new_stmt, gimple_vuse (stmt));
      if (gimple_vdef (stmt))
	{
	  gimple_set_vdef (new_stmt, gimple_vdef (stmt));
	  SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "with stmt:");
      print_gimple_stmt (dump_file, new_stmt, 0, 0);
      fprintf (dump_file, "\n");
    }
  gsi_replace (&gsi, new_stmt, true);
  if (cs)
    cgraph_set_call_stmt (cs, new_stmt);
  do
    {
      ipa_record_stmt_references (current_node, gsi_stmt (gsi));
      gsi_prev (&gsi);
    }
  while (gsi_stmt (gsi) != gsi_stmt (prev_gsi));
}

/* If the expression *EXPR should be replaced by a reduction of a parameter, do
   so.  ADJUSTMENTS is a pointer to a vector of adjustments.  CONVERT
   specifies whether the function should care about type incompatibility the
   current and new expressions.  If it is false, the function will leave
   incompatibility issues to the caller.  Return true iff the expression
   was modified. */

bool
ipa_modify_expr (tree *expr, bool convert,
		 ipa_parm_adjustment_vec adjustments)
{
  struct ipa_parm_adjustment *cand
    = ipa_get_adjustment_candidate (&expr, &convert, adjustments, false);
  if (!cand)
    return false;

  tree src;
  if (cand->by_ref)
    src = build_simple_mem_ref (cand->new_decl);
  else
    src = cand->new_decl;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "About to replace expr ");
      print_generic_expr (dump_file, *expr, 0);
      fprintf (dump_file, " with ");
      print_generic_expr (dump_file, src, 0);
      fprintf (dump_file, "\n");
    }

  if (convert && !useless_type_conversion_p (TREE_TYPE (*expr), cand->type))
    {
      tree vce = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (*expr), src);
      *expr = vce;
    }
  else
    *expr = src;
  return true;
}

/* If T is an SSA_NAME, return NULL if it is not a default def or
   return its base variable if it is.  If IGNORE_DEFAULT_DEF is true,
   the base variable is always returned, regardless if it is a default
   def.  Return T if it is not an SSA_NAME.  */

static tree
get_ssa_base_param (tree t, bool ignore_default_def)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      if (ignore_default_def || SSA_NAME_IS_DEFAULT_DEF (t))
	return SSA_NAME_VAR (t);
      else
	return NULL_TREE;
    }
  return t;
}

/* Given an expression, return an adjustment entry specifying the
   transformation to be done on EXPR.  If no suitable adjustment entry
   was found, returns NULL.

   If IGNORE_DEFAULT_DEF is set, consider SSA_NAMEs which are not a
   default def, otherwise bail on them.

   If CONVERT is non-NULL, this function will set *CONVERT if the
   expression provided is a component reference.  ADJUSTMENTS is the
   adjustments vector.  */

ipa_parm_adjustment *
ipa_get_adjustment_candidate (tree **expr, bool *convert,
			      ipa_parm_adjustment_vec adjustments,
			      bool ignore_default_def)
{
  if (TREE_CODE (**expr) == BIT_FIELD_REF
      || TREE_CODE (**expr) == IMAGPART_EXPR
      || TREE_CODE (**expr) == REALPART_EXPR)
    {
      *expr = &TREE_OPERAND (**expr, 0);
      if (convert)
	*convert = true;
    }

  HOST_WIDE_INT offset, size, max_size;
  tree base = get_ref_base_and_extent (**expr, &offset, &size, &max_size);
  if (!base || size == -1 || max_size == -1)
    return NULL;

  if (TREE_CODE (base) == MEM_REF)
    {
      offset += mem_ref_offset (base).to_short_addr () * BITS_PER_UNIT;
      base = TREE_OPERAND (base, 0);
    }

  base = get_ssa_base_param (base, ignore_default_def);
  if (!base || TREE_CODE (base) != PARM_DECL)
    return NULL;

  struct ipa_parm_adjustment *cand = NULL;
  unsigned int len = adjustments.length ();
  for (unsigned i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj = &adjustments[i];

      if (adj->base == base
	  && (adj->offset == offset || adj->op == IPA_PARM_OP_REMOVE))
	{
	  cand = adj;
	  break;
	}
    }

  if (!cand || cand->op == IPA_PARM_OP_COPY || cand->op == IPA_PARM_OP_REMOVE)
    return NULL;
  return cand;
}

/* Return true iff BASE_INDEX is in ADJUSTMENTS more than once.  */

static bool
index_in_adjustments_multiple_times_p (int base_index,
				       ipa_parm_adjustment_vec adjustments)
{
  int i, len = adjustments.length ();
  bool one = false;

  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      adj = &adjustments[i];

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
  int i, outlen = outer.length ();
  int inlen = inner.length ();
  int removals = 0;
  ipa_parm_adjustment_vec adjustments, tmp;

  tmp.create (inlen);
  for (i = 0; i < inlen; i++)
    {
      struct ipa_parm_adjustment *n;
      n = &inner[i];

      if (n->op == IPA_PARM_OP_REMOVE)
	removals++;
      else
	{
	  /* FIXME: Handling of new arguments are not implemented yet.  */
	  gcc_assert (n->op != IPA_PARM_OP_NEW);
	  tmp.quick_push (*n);
	}
    }

  adjustments.create (outlen + removals);
  for (i = 0; i < outlen; i++)
    {
      struct ipa_parm_adjustment r;
      struct ipa_parm_adjustment *out = &outer[i];
      struct ipa_parm_adjustment *in = &tmp[out->base_index];

      memset (&r, 0, sizeof (r));
      gcc_assert (in->op != IPA_PARM_OP_REMOVE);
      if (out->op == IPA_PARM_OP_REMOVE)
	{
	  if (!index_in_adjustments_multiple_times_p (in->base_index, tmp))
	    {
	      r.op = IPA_PARM_OP_REMOVE;
	      adjustments.quick_push (r);
	    }
	  continue;
	}
      else
	{
	  /* FIXME: Handling of new arguments are not implemented yet.  */
	  gcc_assert (out->op != IPA_PARM_OP_NEW);
	}

      r.base_index = in->base_index;
      r.type = out->type;

      /* FIXME:  Create nonlocal value too.  */

      if (in->op == IPA_PARM_OP_COPY && out->op == IPA_PARM_OP_COPY)
	r.op = IPA_PARM_OP_COPY;
      else if (in->op == IPA_PARM_OP_COPY)
	r.offset = out->offset;
      else if (out->op == IPA_PARM_OP_COPY)
	r.offset = in->offset;
      else
	r.offset = in->offset + out->offset;
      adjustments.quick_push (r);
    }

  for (i = 0; i < inlen; i++)
    {
      struct ipa_parm_adjustment *n = &inner[i];

      if (n->op == IPA_PARM_OP_REMOVE)
	adjustments.quick_push (*n);
    }

  tmp.release ();
  return adjustments;
}

/* Dump the adjustments in the vector ADJUSTMENTS to dump_file in a human
   friendly way, assuming they are meant to be applied to FNDECL.  */

void
ipa_dump_param_adjustments (FILE *file, ipa_parm_adjustment_vec adjustments,
			    tree fndecl)
{
  int i, len = adjustments.length ();
  bool first = true;
  vec<tree> parms = ipa_get_vector_of_formal_parms (fndecl);

  fprintf (file, "IPA param adjustments: ");
  for (i = 0; i < len; i++)
    {
      struct ipa_parm_adjustment *adj;
      adj = &adjustments[i];

      if (!first)
	fprintf (file, "                 ");
      else
	first = false;

      fprintf (file, "%i. base_index: %i - ", i, adj->base_index);
      print_generic_expr (file, parms[adj->base_index], 0);
      if (adj->base)
	{
	  fprintf (file, ", base: ");
	  print_generic_expr (file, adj->base, 0);
	}
      if (adj->new_decl)
	{
	  fprintf (file, ", new_decl: ");
	  print_generic_expr (file, adj->new_decl, 0);
	}
      if (adj->new_ssa_base)
	{
	  fprintf (file, ", new_ssa_base: ");
	  print_generic_expr (file, adj->new_ssa_base, 0);
	}

      if (adj->op == IPA_PARM_OP_COPY)
	fprintf (file, ", copy_param");
      else if (adj->op == IPA_PARM_OP_REMOVE)
	fprintf (file, ", remove_param");
      else
	fprintf (file, ", offset %li", (long) adj->offset);
      if (adj->by_ref)
	fprintf (file, ", by_ref");
      print_node_brief (file, ", type: ", adj->type, 0);
      fprintf (file, "\n");
    }
  parms.release ();
}

/* Dump the AV linked list.  */

void
ipa_dump_agg_replacement_values (FILE *f, struct ipa_agg_replacement_value *av)
{
  bool comma = false;
  fprintf (f, "     Aggregate replacements:");
  for (; av; av = av->next)
    {
      fprintf (f, "%s %i[" HOST_WIDE_INT_PRINT_DEC "]=", comma ? "," : "",
	       av->index, av->offset);
      print_generic_expr (f, av->value, 0);
      comma = true;
    }
  fprintf (f, "\n");
}

/* Stream out jump function JUMP_FUNC to OB.  */

static void
ipa_write_jump_function (struct output_block *ob,
			 struct ipa_jump_func *jump_func)
{
  struct ipa_agg_jf_item *item;
  struct bitpack_d bp;
  int i, count;

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
      gcc_assert (
	  EXPR_LOCATION (jump_func->value.constant.value) == UNKNOWN_LOCATION);
      stream_write_tree (ob, jump_func->value.constant.value, true);
      break;
    case IPA_JF_PASS_THROUGH:
      streamer_write_uhwi (ob, jump_func->value.pass_through.operation);
      if (jump_func->value.pass_through.operation == NOP_EXPR)
	{
	  streamer_write_uhwi (ob, jump_func->value.pass_through.formal_id);
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, jump_func->value.pass_through.agg_preserved, 1);
	  bp_pack_value (&bp, jump_func->value.pass_through.type_preserved, 1);
	  streamer_write_bitpack (&bp);
	}
      else
	{
	  stream_write_tree (ob, jump_func->value.pass_through.operand, true);
	  streamer_write_uhwi (ob, jump_func->value.pass_through.formal_id);
	}
      break;
    case IPA_JF_ANCESTOR:
      streamer_write_uhwi (ob, jump_func->value.ancestor.offset);
      stream_write_tree (ob, jump_func->value.ancestor.type, true);
      streamer_write_uhwi (ob, jump_func->value.ancestor.formal_id);
      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, jump_func->value.ancestor.agg_preserved, 1);
      bp_pack_value (&bp, jump_func->value.ancestor.type_preserved, 1);
      streamer_write_bitpack (&bp);
      break;
    }

  count = vec_safe_length (jump_func->agg.items);
  streamer_write_uhwi (ob, count);
  if (count)
    {
      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, jump_func->agg.by_ref, 1);
      streamer_write_bitpack (&bp);
    }

  FOR_EACH_VEC_SAFE_ELT (jump_func->agg.items, i, item)
    {
      streamer_write_uhwi (ob, item->offset);
      stream_write_tree (ob, item->value, true);
    }
}

/* Read in jump function JUMP_FUNC from IB.  */

static void
ipa_read_jump_function (struct lto_input_block *ib,
			struct ipa_jump_func *jump_func,
			struct cgraph_edge *cs,
			struct data_in *data_in)
{
  enum jump_func_type jftype;
  enum tree_code operation;
  int i, count;

  jftype = (enum jump_func_type) streamer_read_uhwi (ib);
  switch (jftype)
    {
    case IPA_JF_UNKNOWN:
      jump_func->type = IPA_JF_UNKNOWN;
      break;
    case IPA_JF_KNOWN_TYPE:
      {
	HOST_WIDE_INT offset = streamer_read_uhwi (ib);
	tree base_type = stream_read_tree (ib, data_in);
	tree component_type = stream_read_tree (ib, data_in);

	ipa_set_jf_known_type (jump_func, offset, base_type, component_type);
	break;
      }
    case IPA_JF_CONST:
      ipa_set_jf_constant (jump_func, stream_read_tree (ib, data_in), cs);
      break;
    case IPA_JF_PASS_THROUGH:
      operation = (enum tree_code) streamer_read_uhwi (ib);
      if (operation == NOP_EXPR)
	{
	  int formal_id =  streamer_read_uhwi (ib);
	  struct bitpack_d bp = streamer_read_bitpack (ib);
	  bool agg_preserved = bp_unpack_value (&bp, 1);
	  bool type_preserved = bp_unpack_value (&bp, 1);
	  ipa_set_jf_simple_pass_through (jump_func, formal_id, agg_preserved,
					  type_preserved);
	}
      else
	{
	  tree operand = stream_read_tree (ib, data_in);
	  int formal_id =  streamer_read_uhwi (ib);
	  ipa_set_jf_arith_pass_through (jump_func, formal_id, operand,
					 operation);
	}
      break;
    case IPA_JF_ANCESTOR:
      {
	HOST_WIDE_INT offset = streamer_read_uhwi (ib);
	tree type = stream_read_tree (ib, data_in);
	int formal_id = streamer_read_uhwi (ib);
	struct bitpack_d bp = streamer_read_bitpack (ib);
	bool agg_preserved = bp_unpack_value (&bp, 1);
	bool type_preserved = bp_unpack_value (&bp, 1);

	ipa_set_ancestor_jf (jump_func, offset, type, formal_id, agg_preserved,
			     type_preserved);
	break;
      }
    }

  count = streamer_read_uhwi (ib);
  vec_alloc (jump_func->agg.items, count);
  if (count)
    {
      struct bitpack_d bp = streamer_read_bitpack (ib);
      jump_func->agg.by_ref = bp_unpack_value (&bp, 1);
    }
  for (i = 0; i < count; i++)
    {
      struct ipa_agg_jf_item item;
      item.offset = streamer_read_uhwi (ib);
      item.value = stream_read_tree (ib, data_in);
      jump_func->agg.items->quick_push (item);
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
  streamer_write_hwi (ob, ii->offset);
  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, ii->polymorphic, 1);
  bp_pack_value (&bp, ii->agg_contents, 1);
  bp_pack_value (&bp, ii->member_ptr, 1);
  bp_pack_value (&bp, ii->by_ref, 1);
  bp_pack_value (&bp, ii->maybe_in_construction, 1);
  bp_pack_value (&bp, ii->maybe_derived_type, 1);
  streamer_write_bitpack (&bp);

  if (ii->polymorphic)
    {
      streamer_write_hwi (ob, ii->otr_token);
      stream_write_tree (ob, ii->otr_type, true);
      stream_write_tree (ob, ii->outer_type, true);
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
  ii->offset = (HOST_WIDE_INT) streamer_read_hwi (ib);
  bp = streamer_read_bitpack (ib);
  ii->polymorphic = bp_unpack_value (&bp, 1);
  ii->agg_contents = bp_unpack_value (&bp, 1);
  ii->member_ptr = bp_unpack_value (&bp, 1);
  ii->by_ref = bp_unpack_value (&bp, 1);
  ii->maybe_in_construction = bp_unpack_value (&bp, 1);
  ii->maybe_derived_type = bp_unpack_value (&bp, 1);
  if (ii->polymorphic)
    {
      ii->otr_token = (HOST_WIDE_INT) streamer_read_hwi (ib);
      ii->otr_type = stream_read_tree (ib, data_in);
      ii->outer_type = stream_read_tree (ib, data_in);
    }
}

/* Stream out NODE info to OB.  */

static void
ipa_write_node_info (struct output_block *ob, struct cgraph_node *node)
{
  int node_ref;
  lto_symtab_encoder_t encoder;
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int j;
  struct cgraph_edge *e;
  struct bitpack_d bp;

  encoder = ob->decl_state->symtab_node_encoder;
  node_ref = lto_symtab_encoder_encode (encoder, node);
  streamer_write_uhwi (ob, node_ref);

  streamer_write_uhwi (ob, ipa_get_param_count (info));
  for (j = 0; j < ipa_get_param_count (info); j++)
    streamer_write_uhwi (ob, ipa_get_param_move_cost (info, j));
  bp = bitpack_create (ob->main_stream);
  gcc_assert (info->uses_analysis_done
	      || ipa_get_param_count (info) == 0);
  gcc_assert (!info->node_enqueued);
  gcc_assert (!info->ipcp_orig_node);
  for (j = 0; j < ipa_get_param_count (info); j++)
    bp_pack_value (&bp, ipa_is_param_used (info, j), 1);
  streamer_write_bitpack (&bp);
  for (j = 0; j < ipa_get_param_count (info); j++)
    streamer_write_hwi (ob, ipa_get_controlled_uses (info, j));
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

  ipa_alloc_node_params (node, streamer_read_uhwi (ib));

  for (k = 0; k < ipa_get_param_count (info); k++)
    info->descriptors[k].move_cost = streamer_read_uhwi (ib);
    
  bp = streamer_read_bitpack (ib);
  if (ipa_get_param_count (info) != 0)
    info->uses_analysis_done = true;
  info->node_enqueued = false;
  for (k = 0; k < ipa_get_param_count (info); k++)
    ipa_set_param_used (info, k, bp_unpack_value (&bp, 1));
  for (k = 0; k < ipa_get_param_count (info); k++)
    ipa_set_controlled_uses (info, k, streamer_read_hwi (ib));
  for (e = node->callees; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int count = streamer_read_uhwi (ib);

      if (!count)
	continue;
      vec_safe_grow_cleared (args->jump_functions, count);

      for (k = 0; k < ipa_get_cs_argument_count (args); k++)
	ipa_read_jump_function (ib, ipa_get_ith_jump_func (args, k), e,
				data_in);
    }
  for (e = node->indirect_calls; e; e = e->next_callee)
    {
      struct ipa_edge_args *args = IPA_EDGE_REF (e);
      int count = streamer_read_uhwi (ib);

      if (count)
	{
	  vec_safe_grow_cleared (args->jump_functions, count);
          for (k = 0; k < ipa_get_cs_argument_count (args); k++)
	    ipa_read_jump_function (ib, ipa_get_ith_jump_func (args, k), e,
				    data_in);
	}
      ipa_read_indirect_edge_info (ib, data_in, e);
    }
}

/* Write jump functions for nodes in SET.  */

void
ipa_prop_write_jump_functions (void)
{
  struct cgraph_node *node;
  struct output_block *ob;
  unsigned int count = 0;
  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder;


  if (!ipa_node_params_vector.exists ())
    return;

  ob = create_output_block (LTO_section_jump_functions);
  encoder = ob->decl_state->symtab_node_encoder;
  ob->cgraph_node = NULL;
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      if (cgraph_function_with_gimple_body_p (node)
	  && IPA_NODE_REF (node) != NULL)
	count++;
    }

  streamer_write_uhwi (ob, count);

  /* Process all of the functions.  */
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
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
			header->string_size, vNULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_symtab_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->symtab_node_encoder;
      node = cgraph (lto_symtab_encoder_deref (encoder, index));
      gcc_assert (node->definition);
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
  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
}

void
write_agg_replacement_chain (struct output_block *ob, struct cgraph_node *node)
{
  int node_ref;
  unsigned int count = 0;
  lto_symtab_encoder_t encoder;
  struct ipa_agg_replacement_value *aggvals, *av;

  aggvals = ipa_get_agg_replacements_for_node (node);
  encoder = ob->decl_state->symtab_node_encoder;
  node_ref = lto_symtab_encoder_encode (encoder, node);
  streamer_write_uhwi (ob, node_ref);

  for (av = aggvals; av; av = av->next)
    count++;
  streamer_write_uhwi (ob, count);

  for (av = aggvals; av; av = av->next)
    {
      struct bitpack_d bp;

      streamer_write_uhwi (ob, av->offset);
      streamer_write_uhwi (ob, av->index);
      stream_write_tree (ob, av->value, true);

      bp = bitpack_create (ob->main_stream);
      bp_pack_value (&bp, av->by_ref, 1);
      streamer_write_bitpack (&bp);
    }
}

/* Stream in the aggregate value replacement chain for NODE from IB.  */

static void
read_agg_replacement_chain (struct lto_input_block *ib,
			    struct cgraph_node *node,
			    struct data_in *data_in)
{
  struct ipa_agg_replacement_value *aggvals = NULL;
  unsigned int count, i;

  count = streamer_read_uhwi (ib);
  for (i = 0; i <count; i++)
    {
      struct ipa_agg_replacement_value *av;
      struct bitpack_d bp;

      av = ggc_alloc_ipa_agg_replacement_value ();
      av->offset = streamer_read_uhwi (ib);
      av->index = streamer_read_uhwi (ib);
      av->value = stream_read_tree (ib, data_in);
      bp = streamer_read_bitpack (ib);
      av->by_ref = bp_unpack_value (&bp, 1);
      av->next = aggvals;
      aggvals = av;
    }
  ipa_set_node_agg_value_chain (node, aggvals);
}

/* Write all aggregate replacement for nodes in set.  */

void
ipa_prop_write_all_agg_replacement (void)
{
  struct cgraph_node *node;
  struct output_block *ob;
  unsigned int count = 0;
  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder;

  if (!ipa_node_agg_replacements)
    return;

  ob = create_output_block (LTO_section_ipcp_transform);
  encoder = ob->decl_state->symtab_node_encoder;
  ob->cgraph_node = NULL;
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      if (cgraph_function_with_gimple_body_p (node)
	  && ipa_get_agg_replacements_for_node (node) != NULL)
	count++;
    }

  streamer_write_uhwi (ob, count);

  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      if (cgraph_function_with_gimple_body_p (node)
	  && ipa_get_agg_replacements_for_node (node) != NULL)
	write_agg_replacement_chain (ob, node);
    }
  streamer_write_char_stream (ob->main_stream, 0);
  produce_asm (ob, NULL);
  destroy_output_block (ob);
}

/* Read replacements section in file FILE_DATA of length LEN with data
   DATA.  */

static void
read_replacements_section (struct lto_file_decl_data *file_data,
			   const char *data,
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

  data_in = lto_data_in_create (file_data, (const char *) data + string_offset,
				header->string_size, vNULL);
  count = streamer_read_uhwi (&ib_main);

  for (i = 0; i < count; i++)
    {
      unsigned int index;
      struct cgraph_node *node;
      lto_symtab_encoder_t encoder;

      index = streamer_read_uhwi (&ib_main);
      encoder = file_data->symtab_node_encoder;
      node = cgraph (lto_symtab_encoder_deref (encoder, index));
      gcc_assert (node->definition);
      read_agg_replacement_chain (&ib_main, node, data_in);
    }
  lto_free_section_data (file_data, LTO_section_jump_functions, NULL, data,
			 len);
  lto_data_in_delete (data_in);
}

/* Read IPA-CP aggregate replacements.  */

void
ipa_prop_read_all_agg_replacement (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  while ((file_data = file_data_vec[j++]))
    {
      size_t len;
      const char *data = lto_get_section_data (file_data,
					       LTO_section_ipcp_transform,
					       NULL, &len);
      if (data)
        read_replacements_section (file_data, data, len);
    }
}

/* Adjust the aggregate replacements in AGGVAL to reflect parameters skipped in
   NODE.  */

static void
adjust_agg_replacement_values (struct cgraph_node *node,
			       struct ipa_agg_replacement_value *aggval)
{
  struct ipa_agg_replacement_value *v;
  int i, c = 0, d = 0, *adj;

  if (!node->clone.combined_args_to_skip)
    return;

  for (v = aggval; v; v = v->next)
    {
      gcc_assert (v->index >= 0);
      if (c < v->index)
	c = v->index;
    }
  c++;

  adj = XALLOCAVEC (int, c);
  for (i = 0; i < c; i++)
    if (bitmap_bit_p (node->clone.combined_args_to_skip, i))
      {
	adj[i] = -1;
	d++;
      }
    else
      adj[i] = i - d;

  for (v = aggval; v; v = v->next)
    v->index = adj[v->index];
}


/* Function body transformation phase.  */

unsigned int
ipcp_transform_function (struct cgraph_node *node)
{
  vec<ipa_param_descriptor> descriptors = vNULL;
  struct param_analysis_info *parms_ainfo;
  struct ipa_agg_replacement_value *aggval;
  gimple_stmt_iterator gsi;
  basic_block bb;
  int param_count;
  bool cfg_changed = false, something_changed = false;

  gcc_checking_assert (cfun);
  gcc_checking_assert (current_function_decl);

  if (dump_file)
    fprintf (dump_file, "Modification phase of node %s/%i\n",
	     node->name (), node->order);

  aggval = ipa_get_agg_replacements_for_node (node);
  if (!aggval)
      return 0;
  param_count = count_formal_params (node->decl);
  if (param_count == 0)
    return 0;
  adjust_agg_replacement_values (node, aggval);
  if (dump_file)
    ipa_dump_agg_replacement_values (dump_file, aggval);
  parms_ainfo = XALLOCAVEC (struct param_analysis_info, param_count);
  memset (parms_ainfo, 0, sizeof (struct param_analysis_info) * param_count);
  descriptors.safe_grow_cleared (param_count);
  ipa_populate_param_decls (node, descriptors);

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	struct ipa_agg_replacement_value *v;
	gimple stmt = gsi_stmt (gsi);
	tree rhs, val, t;
	HOST_WIDE_INT offset, size;
	int index;
	bool by_ref, vce;

	if (!gimple_assign_load_p (stmt))
	  continue;
	rhs = gimple_assign_rhs1 (stmt);
	if (!is_gimple_reg_type (TREE_TYPE (rhs)))
	  continue;

	vce = false;
	t = rhs;
	while (handled_component_p (t))
	  {
	    /* V_C_E can do things like convert an array of integers to one
               bigger integer and similar things we do not handle below.  */
            if (TREE_CODE (rhs) == VIEW_CONVERT_EXPR)
	      {
		vce = true;
		break;
	      }
	    t = TREE_OPERAND (t, 0);
	  }
	if (vce)
	  continue;

	if (!ipa_load_from_parm_agg_1 (descriptors, parms_ainfo, stmt,
				       rhs, &index, &offset, &size, &by_ref))
	  continue;
	for (v = aggval; v; v = v->next)
	  if (v->index == index
	      && v->offset == offset)
	    break;
	if (!v
	    || v->by_ref != by_ref
	    || tree_to_shwi (TYPE_SIZE (TREE_TYPE (v->value))) != size)
	  continue;

	gcc_checking_assert (is_gimple_ip_invariant (v->value));
	if (!useless_type_conversion_p (TREE_TYPE (rhs), TREE_TYPE (v->value)))
	  {
	    if (fold_convertible_p (TREE_TYPE (rhs), v->value))
	      val = fold_build1 (NOP_EXPR, TREE_TYPE (rhs), v->value);
	    else if (TYPE_SIZE (TREE_TYPE (rhs))
		     == TYPE_SIZE (TREE_TYPE (v->value)))
	      val = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (rhs), v->value);
	    else
	      {
		if (dump_file)
		  {
		    fprintf (dump_file, "    const ");
		    print_generic_expr (dump_file, v->value, 0);
		    fprintf (dump_file, "  can't be converted to type of ");
		    print_generic_expr (dump_file, rhs, 0);
		    fprintf (dump_file, "\n");
		  }
		continue;
	      }
	  }
	else
	  val = v->value;

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "Modifying stmt:\n  ");
	    print_gimple_stmt (dump_file, stmt, 0, 0);
	  }
	gimple_assign_set_rhs_from_tree (&gsi, val);
	update_stmt (stmt);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file, "into:\n  ");
	    print_gimple_stmt (dump_file, stmt, 0, 0);
	    fprintf (dump_file, "\n");
	  }

	something_changed = true;
	if (maybe_clean_eh_stmt (stmt)
	    && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
	  cfg_changed = true;
      }

  (*ipa_node_agg_replacements)[node->uid] = NULL;
  free_parms_ainfo (parms_ainfo, param_count);
  descriptors.release ();

  if (!something_changed)
    return 0;
  else if (cfg_changed)
    return TODO_update_ssa_only_virtuals | TODO_cleanup_cfg;
  else
    return TODO_update_ssa_only_virtuals;
}
