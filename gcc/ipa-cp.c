/* Interprocedural constant propagation
   Copyright (C) 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Razya Ladelsky <RAZYA@il.ibm.com>
   
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

/* Interprocedural constant propagation.  The aim of interprocedural constant
   propagation (IPCP) is to find which function's argument has the same
   constant value in each invocation throughout the whole program. For example,
   consider the following program:

   int g (int y)
   {
     printf ("value is %d",y);
   }
   
   int f (int x)
   {
     g (x);
   }

   int h (int y)
   {
     g (y);
   }

   void main (void)
   {
     f (3);
     h (3);
   }
   
   
   The IPCP algorithm will find that g's formal argument y is always called
   with the value 3.

   The algorithm used is based on "Interprocedural Constant Propagation", by
   Challahan David, Keith D Cooper, Ken Kennedy, Linda Torczon, Comp86, pg
   152-161
   
   The optimization is divided into three stages:

   First stage - intraprocedural analysis
   =======================================
   This phase computes jump_function and modification flags.
   
   A jump function for a callsite represents the values passed as an actual
   arguments of a given callsite. There are three types of values:
   Pass through - the caller's formal parameter is passed as an actual argument.
   Constant - a constant is passed as an actual argument.
   Unknown - neither of the above.
   
   The jump function info, ipa_jump_func, is stored in ipa_edge_args
   structure (defined in ipa_prop.h and pointed to by cgraph_node->aux)
   modified_flags are defined in ipa_node_params structure
   (defined in ipa_prop.h and pointed to by cgraph_edge->aux).
   
   -ipcp_init_stage() is the first stage driver.

   Second stage - interprocedural analysis
   ========================================
   This phase does the interprocedural constant propagation.
   It computes lattices for all formal parameters in the program
   and their value that may be:
   TOP - unknown.
   BOTTOM - non constant.
   CONSTANT - constant value.
   
   Lattice describing a formal parameter p will have a constant value if all
   callsites invoking this function have the same constant value passed to p.
   
   The lattices are stored in ipcp_lattice which is itself in ipa_node_params
   structure (defined in ipa_prop.h and pointed to by cgraph_edge->aux).

   -ipcp_iterate_stage() is the second stage driver.

   Third phase - transformation of function code
   ============================================
   Propagates the constant-valued formals into the function.
   For each function whose parameters are constants, we create its clone.

   Then we process the clone in two ways:
   1. We insert an assignment statement 'parameter = const' at the beginning
      of the cloned function.
   2. For read-only parameters that do not live in memory, we replace all their
      uses with the constant.

   We also need to modify some callsites to call the cloned functions instead
   of the original ones.  For a callsite passing an argument found to be a
   constant by IPCP, there are two different cases to handle:
   1. A constant is passed as an argument.  In this case the callsite in the
      should be redirected to call the cloned callee.
   2. A parameter (of the caller) passed as an argument (pass through
      argument).  In such cases both the caller and the callee have clones and
      only the callsite in the cloned caller is redirected to call to the
      cloned callee.

   This update is done in two steps: First all cloned functions are created
   during a traversal of the call graph, during which all callsites are
   redirected to call the cloned function.  Then the callsites are traversed
   and many calls redirected back to fit the description above.

   -ipcp_insert_stage() is the third phase driver.
   
*/

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

/* Get the original node field of ipa_node_params associated with node NODE.  */
static inline struct cgraph_node *
ipcp_get_orig_node (struct cgraph_node *node)
{
  return IPA_NODE_REF (node)->ipcp_orig_node;
}

/* Return true if NODE describes a cloned/versioned function.  */
static inline bool
ipcp_node_is_clone (struct cgraph_node *node)
{
  return (ipcp_get_orig_node (node) != NULL);
}

/* Create ipa_node_params and its data structures for NEW_NODE.  Set ORIG_NODE
   as the ipcp_orig_node field in ipa_node_params.  */
static void
ipcp_init_cloned_node (struct cgraph_node *orig_node,
		       struct cgraph_node *new_node)
{
  ipa_check_create_node_params ();
  IPA_NODE_REF (new_node)->ipcp_orig_node = orig_node;
  ipa_count_formal_params (new_node);
  ipa_create_param_decls_array (new_node);
}

/* Perform intraprocedrual analysis needed for ipcp.  */
static void
ipcp_analyze_node (struct cgraph_node *node)
{
  /* Unreachable nodes should have been eliminated before ipcp.  */
  gcc_assert (node->needed || node->reachable);

  ipa_count_formal_params (node);
  ipa_create_param_decls_array (node);
  ipa_detect_param_modifications (node);
}

/* Recompute all local information since node might've got new
   direct calls after clonning.  */
static void
ipcp_update_cloned_node (struct cgraph_node *new_node)
{
  /* We might've introduced new direct calls.  */
  push_cfun (DECL_STRUCT_FUNCTION (new_node->decl));
  current_function_decl = new_node->decl;
  rebuild_cgraph_edges ();

  /* Indirect inlinng rely on fact that we've already analyzed
     the body..  */
  if (flag_indirect_inlining)
    {
      struct cgraph_edge *cs;

      ipcp_analyze_node (new_node);

      for (cs = new_node->callees; cs; cs = cs->next_callee)
	{
	  ipa_count_arguments (cs);
	  ipa_compute_jump_functions (cs);
	}
    }
  pop_cfun ();
  current_function_decl = NULL;
}

/* Return scale for NODE.  */
static inline gcov_type
ipcp_get_node_scale (struct cgraph_node *node)
{
  return IPA_NODE_REF (node)->count_scale;
}

/* Set COUNT as scale for NODE.  */
static inline void
ipcp_set_node_scale (struct cgraph_node *node, gcov_type count)
{
  IPA_NODE_REF (node)->count_scale = count;
}

/* Return whether LAT is a constant lattice.  */
static inline bool
ipcp_lat_is_const (struct ipcp_lattice *lat)
{
  if (lat->type == IPA_CONST_VALUE)
    return true;
  else
    return false;
}

/* Return whether LAT is a constant lattice that ipa-cp can actually insert
   into the code (i.e. constants excluding member pointers and pointers).  */
static inline bool
ipcp_lat_is_insertable (struct ipcp_lattice *lat)
{
  return lat->type == IPA_CONST_VALUE;
}

/* Return true if LAT1 and LAT2 are equal.  */
static inline bool
ipcp_lats_are_equal (struct ipcp_lattice *lat1, struct ipcp_lattice *lat2)
{
  gcc_assert (ipcp_lat_is_const (lat1) && ipcp_lat_is_const (lat2));
  if (lat1->type != lat2->type)
    return false;

  if (operand_equal_p (lat1->constant, lat2->constant, 0))
    return true;

  return false;
}

/* Compute Meet arithmetics:
   Meet (IPA_BOTTOM, x) = IPA_BOTTOM
   Meet (IPA_TOP,x) = x
   Meet (const_a,const_b) = IPA_BOTTOM,  if const_a != const_b.
   MEET (const_a,const_b) = const_a, if const_a == const_b.*/
static void
ipa_lattice_meet (struct ipcp_lattice *res, struct ipcp_lattice *lat1,
		  struct ipcp_lattice *lat2)
{
  if (lat1->type == IPA_BOTTOM || lat2->type == IPA_BOTTOM)
    {
      res->type = IPA_BOTTOM;
      return;
    }
  if (lat1->type == IPA_TOP)
    {
      res->type = lat2->type;
      res->constant = lat2->constant;
      return;
    }
  if (lat2->type == IPA_TOP)
    {
      res->type = lat1->type;
      res->constant = lat1->constant;
      return;
    }
  if (!ipcp_lats_are_equal (lat1, lat2))
    {
      res->type = IPA_BOTTOM;
      return;
    }
  res->type = lat1->type;
  res->constant = lat1->constant;
}

/* Return the lattice corresponding to the Ith formal parameter of the function
   described by INFO.  */
static inline struct ipcp_lattice *
ipcp_get_ith_lattice (struct ipa_node_params *info, int i)
{
  return &(info->ipcp_lattices[i]);
}

/* Given the jump function JFUNC, compute the lattice LAT that describes the
   value coming down the callsite. INFO describes the caller node so that
   pass-through jump functions can be evaluated.  */
static void
ipcp_lattice_from_jfunc (struct ipa_node_params *info, struct ipcp_lattice *lat,
			 struct ipa_jump_func *jfunc)
{
  if (jfunc->type == IPA_CONST)
    {
      lat->type = IPA_CONST_VALUE;
      lat->constant = jfunc->value.constant;
    }
  else if (jfunc->type == IPA_PASS_THROUGH)
    {
      struct ipcp_lattice *caller_lat;

      caller_lat = ipcp_get_ith_lattice (info, jfunc->value.formal_id);
      lat->type = caller_lat->type;
      lat->constant = caller_lat->constant;
    }
  else
    lat->type = IPA_BOTTOM;
}

/* True when OLD_LAT and NEW_LAT values are not the same.  */

static bool
ipcp_lattice_changed (struct ipcp_lattice *old_lat,
		      struct ipcp_lattice *new_lat)
{
  if (old_lat->type == new_lat->type)
    {
      if (!ipcp_lat_is_const (old_lat))
	return false;
      if (ipcp_lats_are_equal (old_lat, new_lat))
	return false;
    }
  return true;
}

/* Print all ipcp_lattices of all functions to F.  */
static void
ipcp_print_all_lattices (FILE * f)
{
  struct cgraph_node *node;
  int i, count;

  fprintf (f, "\nLATTICE PRINT\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info;

      if (!node->analyzed)
	continue;
      info = IPA_NODE_REF (node);
      fprintf (f, "Printing lattices %s:\n", cgraph_node_name (node));
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipcp_get_ith_lattice (info, i);

	  fprintf (f, " param [%d]: ", i);
	  if (lat->type == IPA_CONST_VALUE)
	    {
	      fprintf (f, "type is CONST ");
	      print_generic_expr (f, lat->constant, 0);
	      fprintf (f, "\n");
	    }
	  else if (lat->type == IPA_TOP)
	    fprintf (f, "type is TOP\n");
	  else
	    fprintf (f, "type is BOTTOM\n");
	}
    }
}

/* Initialize ipcp_lattices array.  The lattices corresponding to supported
   types (integers, real types and Fortran constants defined as const_decls)
   are initialized to IPA_TOP, the rest of them to IPA_BOTTOM.  */
static void
ipcp_initialize_node_lattices (struct cgraph_node *node)
{
  int i;
  struct ipa_node_params *info = IPA_NODE_REF (node);

  info->ipcp_lattices = XCNEWVEC (struct ipcp_lattice,
				  ipa_get_param_count (info));
  
  /* When cloning is allowed, we can assume that externally visible functions
     are not called.  We will compensate this by cloning later.  */
  if (flag_ipa_cp_clone || !node->needed)
    for (i = 0; i < ipa_get_param_count (info) ; i++)
      ipcp_get_ith_lattice (info, i)->type = IPA_TOP;
  else
    for (i = 0; i < ipa_get_param_count (info) ; i++)
      ipcp_get_ith_lattice (info, i)->type = IPA_BOTTOM;
}

/* build INTEGER_CST tree with type TREE_TYPE and value according to LAT.
   Return the tree.  */
static tree
build_const_val (struct ipcp_lattice *lat, tree tree_type)
{
  tree val;

  gcc_assert (ipcp_lat_is_const (lat));
  val = lat->constant;

  if (!useless_type_conversion_p (tree_type, TREE_TYPE (val)))
    {
      if (fold_convertible_p (tree_type, val))
	return fold_build1 (NOP_EXPR, tree_type, val);
      else
	return fold_build1 (VIEW_CONVERT_EXPR, tree_type, val);
    }
  return val;
}

/* Compute the proper scale for NODE.  It is the ratio between the number of
   direct calls (represented on the incoming cgraph_edges) and sum of all
   invocations of NODE (represented as count in cgraph_node).  */
static void
ipcp_compute_node_scale (struct cgraph_node *node)
{
  gcov_type sum;
  struct cgraph_edge *cs;

  sum = 0;
  /* Compute sum of all counts of callers. */
  for (cs = node->callers; cs != NULL; cs = cs->next_caller)
    sum += cs->count;
  if (node->count == 0)
    ipcp_set_node_scale (node, 0);
  else
    ipcp_set_node_scale (node, sum * REG_BR_PROB_BASE / node->count);
}

/* Initialization and computation of IPCP data structures.  This is the initial
   intraprocedural analysis of functions, which gathers information to be
   propagated later on.  */
static void
ipcp_init_stage (void)
{
  struct cgraph_node *node;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      {
        ipcp_analyze_node (node);
        ipcp_initialize_node_lattices (node);
        ipcp_compute_node_scale (node);
      }
  for (node = cgraph_nodes; node; node = node->next)
    {
      if (!node->analyzed)
	continue;
      /* building jump functions  */
      for (cs = node->callees; cs; cs = cs->next_callee)
	{
	  if (!cs->callee->analyzed)
	    continue;
	  ipa_count_arguments (cs);
	  if (ipa_get_cs_argument_count (IPA_EDGE_REF (cs))
	      != ipa_get_param_count (IPA_NODE_REF (cs->callee)))
	    {
	      /* Handle cases of functions with 
	         a variable number of parameters.  */
	      ipa_set_called_with_variable_arg (IPA_NODE_REF (cs->callee));
	      if (flag_indirect_inlining)
	        ipa_compute_jump_functions (cs);
	    }
	  else
	    ipa_compute_jump_functions (cs);
	}
    }
}

/* Return true if there are some formal parameters whose value is IPA_TOP (in
   the whole compilation unit).  Change their values to IPA_BOTTOM, since they
   most probably get their values from outside of this compilation unit.  */
static bool
ipcp_change_tops_to_bottom (void)
{
  int i, count;
  struct cgraph_node *node;
  bool prop_again;

  prop_again = false;
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info = IPA_NODE_REF (node);
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipcp_get_ith_lattice (info, i);
	  if (lat->type == IPA_TOP)
	    {
	      prop_again = true;
	      lat->type = IPA_BOTTOM;
	    }
	}
    }
  return prop_again;
}

/* Interprocedural analysis. The algorithm propagates constants from the
   caller's parameters to the callee's arguments.  */
static void
ipcp_propagate_stage (void)
{
  int i;
  struct ipcp_lattice inc_lat = { IPA_BOTTOM, NULL };
  struct ipcp_lattice new_lat = { IPA_BOTTOM, NULL };
  struct ipcp_lattice *dest_lat;
  struct cgraph_edge *cs;
  struct ipa_jump_func *jump_func;
  struct ipa_func_list *wl;
  int count;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  /* Initialize worklist to contain all functions.  */
  wl = ipa_init_func_list ();
  while (wl)
    {
      struct cgraph_node *node = ipa_pop_func_from_list (&wl);
      struct ipa_node_params *info = IPA_NODE_REF (node);

      for (cs = node->callees; cs; cs = cs->next_callee)
	{
	  struct ipa_node_params *callee_info = IPA_NODE_REF (cs->callee);
	  struct ipa_edge_args *args = IPA_EDGE_REF (cs);

	  if (ipa_is_called_with_var_arguments (callee_info))
	    continue;

	  count = ipa_get_cs_argument_count (args);
	  for (i = 0; i < count; i++)
	    {
	      jump_func = ipa_get_ith_jump_func (args, i);
	      ipcp_lattice_from_jfunc (info, &inc_lat, jump_func);
	      dest_lat = ipcp_get_ith_lattice (callee_info, i);
	      ipa_lattice_meet (&new_lat, &inc_lat, dest_lat);
	      if (ipcp_lattice_changed (&new_lat, dest_lat))
		{
		  dest_lat->type = new_lat.type;
		  dest_lat->constant = new_lat.constant;
		  ipa_push_func_to_list (&wl, cs->callee);
		}
	    }
	}
    }
}

/* Call the constant propagation algorithm and re-call it if necessary
   (if there are undetermined values left).  */
static void
ipcp_iterate_stage (void)
{
  ipcp_propagate_stage ();
  if (ipcp_change_tops_to_bottom ())
    /* Some lattices have changed from IPA_TOP to IPA_BOTTOM.
       This change should be propagated.  */
    ipcp_propagate_stage ();
}

/* Check conditions to forbid constant insertion to function described by
   NODE.  */
static inline bool
ipcp_node_not_modifiable_p (struct cgraph_node *node)
{
  /* ??? Handle pending sizes case.  */
  if (DECL_UNINLINABLE (node->decl))
    return true;
  return false;
}

/* Print count scale data structures.  */
static void
ipcp_function_scale_print (FILE * f)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    {
      if (!node->analyzed)
	continue;
      fprintf (f, "printing scale for %s: ", cgraph_node_name (node));
      fprintf (f, "value is  " HOST_WIDE_INT_PRINT_DEC
	       "  \n", (HOST_WIDE_INT) ipcp_get_node_scale (node));
    }
}

/* Print counts of all cgraph nodes.  */
static void
ipcp_print_func_profile_counts (FILE * f)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    {
      fprintf (f, "function %s: ", cgraph_node_name (node));
      fprintf (f, "count is  " HOST_WIDE_INT_PRINT_DEC
	       "  \n", (HOST_WIDE_INT) node->count);
    }
}

/* Print counts of all cgraph edges.  */
static void
ipcp_print_call_profile_counts (FILE * f)
{
  struct cgraph_node *node;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    {
      for (cs = node->callees; cs; cs = cs->next_callee)
	{
	  fprintf (f, "%s -> %s ", cgraph_node_name (cs->caller),
		   cgraph_node_name (cs->callee));
	  fprintf (f, "count is  " HOST_WIDE_INT_PRINT_DEC "  \n",
		   (HOST_WIDE_INT) cs->count);
	}
    }
}

/* Print all counts and probabilities of cfg edges of all functions.  */
static void
ipcp_print_edge_profiles (FILE * f)
{
  struct cgraph_node *node;
  basic_block bb;
  edge_iterator ei;
  edge e;

  for (node = cgraph_nodes; node; node = node->next)
    {
      fprintf (f, "function %s: \n", cgraph_node_name (node));
      if (node->analyzed)
	{
	  bb =
	    ENTRY_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION (node->decl));
	  fprintf (f, "ENTRY: ");
	  fprintf (f, " " HOST_WIDE_INT_PRINT_DEC
		   " %d\n", (HOST_WIDE_INT) bb->count, bb->frequency);

	  if (bb->succs)
	    FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (e->dest ==
		  EXIT_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION
					       (node->decl)))
		fprintf (f, "edge ENTRY -> EXIT,  Count");
	      else
		fprintf (f, "edge ENTRY -> %d,  Count", e->dest->index);
	      fprintf (f, " " HOST_WIDE_INT_PRINT_DEC
		       " Prob %d\n", (HOST_WIDE_INT) e->count,
		       e->probability);
	    }
	  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
	  {
	    fprintf (f, "bb[%d]: ", bb->index);
	    fprintf (f, " " HOST_WIDE_INT_PRINT_DEC
		     " %d\n", (HOST_WIDE_INT) bb->count, bb->frequency);
	    FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (e->dest ==
		  EXIT_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION
					       (node->decl)))
		fprintf (f, "edge %d -> EXIT,  Count", e->src->index);
	      else
		fprintf (f, "edge %d -> %d,  Count", e->src->index,
			 e->dest->index);
	      fprintf (f, " " HOST_WIDE_INT_PRINT_DEC " Prob %d\n",
		       (HOST_WIDE_INT) e->count, e->probability);
	    }
	  }
	}
    }
}

/* Print counts and frequencies for all basic blocks of all functions.  */
static void
ipcp_print_bb_profiles (FILE * f)
{
  basic_block bb;
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    {
      fprintf (f, "function %s: \n", cgraph_node_name (node));
      if (node->analyzed)
	{
	  bb =
	    ENTRY_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION (node->decl));
	  fprintf (f, "ENTRY: Count");
	  fprintf (f, " " HOST_WIDE_INT_PRINT_DEC
		   " Frequency  %d\n", (HOST_WIDE_INT) bb->count,
		   bb->frequency);

	  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
	  {
	    fprintf (f, "bb[%d]: Count", bb->index);
	    fprintf (f, " " HOST_WIDE_INT_PRINT_DEC
		     " Frequency %d\n", (HOST_WIDE_INT) bb->count,
		     bb->frequency);
	  }
	  bb =
	    EXIT_BLOCK_PTR_FOR_FUNCTION (DECL_STRUCT_FUNCTION (node->decl));
	  fprintf (f, "EXIT: Count");
	  fprintf (f, " " HOST_WIDE_INT_PRINT_DEC
		   " Frequency %d\n", (HOST_WIDE_INT) bb->count,
		   bb->frequency);

	}
    }
}

/* Print all IPCP data structures to F.  */
static void
ipcp_print_all_structures (FILE * f)
{
  ipcp_print_all_lattices (f);
  ipcp_function_scale_print (f);
  ipa_print_all_tree_maps (f);
  ipa_print_all_param_flags (f);
  ipa_print_all_jump_functions (f);
}

/* Print profile info for all functions.  */
static void
ipcp_print_profile_data (FILE * f)
{
  fprintf (f, "\nNODE COUNTS :\n");
  ipcp_print_func_profile_counts (f);
  fprintf (f, "\nCS COUNTS stage:\n");
  ipcp_print_call_profile_counts (f);
  fprintf (f, "\nBB COUNTS and FREQUENCIES :\n");
  ipcp_print_bb_profiles (f);
  fprintf (f, "\nCFG EDGES COUNTS and PROBABILITIES :\n");
  ipcp_print_edge_profiles (f);
}

/* Build and initialize ipa_replace_map struct according to LAT. This struct is
   processed by versioning, which operates according to the flags set.
   PARM_TREE is the formal parameter found to be constant.  LAT represents the
   constant.  */
static struct ipa_replace_map *
ipcp_create_replace_map (tree parm_tree, struct ipcp_lattice *lat)
{
  struct ipa_replace_map *replace_map;
  tree const_val;

  replace_map = XCNEW (struct ipa_replace_map);
  if (dump_file)
    fprintf (dump_file, "replacing param with const\n");
  const_val = build_const_val (lat, TREE_TYPE (parm_tree));
  replace_map->old_tree = parm_tree;
  replace_map->new_tree = const_val;
  replace_map->replace_p = true;
  replace_map->ref_p = false;

  return replace_map;
}

/* Return true if this callsite should be redirected to the original callee
   (instead of the cloned one).  */
static bool
ipcp_need_redirect_p (struct cgraph_edge *cs)
{
  struct ipa_node_params *orig_callee_info;
  int i, count;
  struct ipa_jump_func *jump_func;
  struct cgraph_node *node = cs->callee, *orig;

  if ((orig = ipcp_get_orig_node (node)) != NULL)
    node = orig;
  if (ipcp_get_orig_node (cs->caller))
    return false;

  orig_callee_info = IPA_NODE_REF (node);
  count = ipa_get_param_count (orig_callee_info);
  for (i = 0; i < count; i++)
    {
      struct ipcp_lattice *lat = ipcp_get_ith_lattice (orig_callee_info, i);
      if (ipcp_lat_is_const (lat))
	{
	  jump_func = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
	  if (jump_func->type != IPA_CONST)
	    return true;
	}
    }

  return false;
}

/* Fix the callsites and the call graph after function cloning was done.  */
static void
ipcp_update_callgraph (void)
{
  struct cgraph_node *node, *orig_callee;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    {
      /* want to fix only original nodes  */
      if (!node->analyzed || ipcp_node_is_clone (node))
	continue;
      for (cs = node->callees; cs; cs = cs->next_callee)
	if (ipcp_node_is_clone (cs->callee))
	  {
	    /* Callee is a cloned node  */
	    orig_callee = ipcp_get_orig_node (cs->callee);
	    if (ipcp_need_redirect_p (cs))
	      {
		cgraph_redirect_edge_callee (cs, orig_callee);
		gimple_call_set_fndecl (cs->call_stmt, orig_callee->decl);
	      }
	  }
    }
}

/* Update all cfg basic blocks in NODE according to SCALE.  */
static void
ipcp_update_bb_counts (struct cgraph_node *node, gcov_type scale)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
    bb->count = bb->count * scale / REG_BR_PROB_BASE;
}

/* Update all cfg edges in NODE according to SCALE.  */
static void
ipcp_update_edges_counts (struct cgraph_node *node, gcov_type scale)
{
  basic_block bb;
  edge_iterator ei;
  edge e;

  FOR_ALL_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
    FOR_EACH_EDGE (e, ei, bb->succs)
    e->count = e->count * scale / REG_BR_PROB_BASE;
}

/* Update profiling info for versioned functions and the functions they were
   versioned from.  */
static void
ipcp_update_profiling (void)
{
  struct cgraph_node *node, *orig_node;
  gcov_type scale, scale_complement;
  struct cgraph_edge *cs;

  for (node = cgraph_nodes; node; node = node->next)
    {
      if (ipcp_node_is_clone (node))
	{
	  orig_node = ipcp_get_orig_node (node);
	  scale = ipcp_get_node_scale (orig_node);
	  node->count = orig_node->count * scale / REG_BR_PROB_BASE;
	  scale_complement = REG_BR_PROB_BASE - scale;
	  orig_node->count =
	    orig_node->count * scale_complement / REG_BR_PROB_BASE;
	  for (cs = node->callees; cs; cs = cs->next_callee)
	    cs->count = cs->count * scale / REG_BR_PROB_BASE;
	  for (cs = orig_node->callees; cs; cs = cs->next_callee)
	    cs->count = cs->count * scale_complement / REG_BR_PROB_BASE;
	  ipcp_update_bb_counts (node, scale);
	  ipcp_update_bb_counts (orig_node, scale_complement);
	  ipcp_update_edges_counts (node, scale);
	  ipcp_update_edges_counts (orig_node, scale_complement);
	}
    }
}

/* Maximal count found in program.  */
static gcov_type max_count;
bitmap dead_nodes;

/* Return true if original clone needs to be preserved.  */
static bool
ipcp_need_original_clone_p (struct cgraph_node *node)
{
  struct cgraph_edge *e;

  if (node->needed)
    return true;
  for (e = node->callers; e; e = e->next_caller)
    if (!bitmap_bit_p (dead_nodes, e->caller->uid)
        && ipcp_need_redirect_p (e))
      return true;

  return false;
}

/* Estimate cost of cloning NODE.  */
static long
ipcp_estimate_cloning_cost (struct cgraph_node *node)
{
  int freq_sum = 1;
  gcov_type count_sum = 1;
  struct cgraph_edge *e;
  int cost;

  /* When we don't need original clone; we should always propagate.  */
  if (!ipcp_need_original_clone_p (node))
    {
      if (dump_file)
	fprintf (dump_file, "Function %s can be fully propagated\n",
		 cgraph_node_name (node));
      return 0;
    }

  for (e = node->callers; e; e = e->next_caller)
    if (!bitmap_bit_p (dead_nodes, e->caller->uid)
        && !ipcp_need_redirect_p (e))
      {
	count_sum += e->count;
	freq_sum += e->frequency + 1;
      }

  cost = node->local.inline_summary.self_insns * 1000;
  if (max_count)
    cost /= count_sum * 1000 / max_count + 1;
  else
    cost /= freq_sum * 1000 / REG_BR_PROB_BASE + 1;
  if (dump_file)
    fprintf (dump_file, "Cost of versioning %s is %i, (size: %i, freq: %i)\n",
             cgraph_node_name (node), cost, node->local.inline_summary.self_insns,
	     freq_sum);
  return cost + 1;
}

/* Return number of live constant parameters.  */
static int
ipcp_const_param_count (struct cgraph_node *node)
{
  int const_param = 0;
  struct ipa_node_params *info = IPA_NODE_REF (node);
  int count = ipa_get_param_count (info);
  int i;

  for (i = 0; i < count; i++)
    {
      struct ipcp_lattice *lat = ipcp_get_ith_lattice (info, i);
      tree parm_tree = ipa_get_ith_param (info, i);
      if (ipcp_lat_is_insertable (lat)
	  /* Do not count obviously unused arguments.  */
	  && (!is_gimple_reg (parm_tree)
	      || gimple_default_def (DECL_STRUCT_FUNCTION (node->decl),
				     parm_tree)))
	const_param++;
    }
  return const_param;
}

/* Propagate the constant parameters found by ipcp_iterate_stage()
   to the function's code.  */
static void
ipcp_insert_stage (void)
{
  struct cgraph_node *node, *node1 = NULL;
  int i;
  VEC (cgraph_edge_p, heap) * redirect_callers;
  varray_type replace_trees;
  struct cgraph_edge *cs;
  int node_callers, count;
  tree parm_tree;
  struct ipa_replace_map *replace_param;
  fibheap_t heap;
  long overall_insns = 0, new_insns = 0;
  long max_new_insns;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();

  dead_nodes = BITMAP_ALLOC (NULL);

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      {
	if (node->count > max_count)
	  max_count = node->count;
	overall_insns += node->local.inline_summary.self_insns;
      }

  max_new_insns = overall_insns;
  if (max_new_insns < PARAM_VALUE (PARAM_LARGE_UNIT_INSNS))
    max_new_insns = PARAM_VALUE (PARAM_LARGE_UNIT_INSNS);
  max_new_insns = max_new_insns * PARAM_VALUE (PARAM_IPCP_UNIT_GROWTH) / 100 + 1;

  /* First collect all functions we proved to have constant arguments to heap.  */
  heap = fibheap_new ();
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info;
      /* Propagation of the constant is forbidden in certain conditions.  */
      if (!node->analyzed || ipcp_node_not_modifiable_p (node))
	  continue;
      info = IPA_NODE_REF (node);
      if (ipa_is_called_with_var_arguments (info))
	continue;
      if (ipcp_const_param_count (node))
	node->aux = fibheap_insert (heap, ipcp_estimate_cloning_cost (node), node);
     }

  /* Now clone in priority order until code size growth limits are met or
     heap is emptied.  */
  while (!fibheap_empty (heap))
    {
      struct ipa_node_params *info;
      int growth = 0;

      node = (struct cgraph_node *)fibheap_extract_min (heap);
      node->aux = NULL;
      if (dump_file)
	fprintf (dump_file, "considering function %s\n",
		 cgraph_node_name (node));

      if (ipcp_need_original_clone_p (node))
        growth = node->local.inline_summary.self_insns;
      else
	bitmap_set_bit (dead_nodes, node->uid);

      if (new_insns + growth > max_new_insns)
	break;
      if (growth
          && (optimize_size
	      || (DECL_STRUCT_FUNCTION (node->decl)
	          ->function_frequency == FUNCTION_FREQUENCY_UNLIKELY_EXECUTED)))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not versioning, cold code would grow");
	  continue;
	}

      new_insns += growth;

      info = IPA_NODE_REF (node);
      count = ipa_get_param_count (info);

      VARRAY_GENERIC_PTR_INIT (replace_trees, ipcp_const_param_count (node),
				"replace_trees");
      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipcp_get_ith_lattice (info, i);
	  parm_tree = ipa_get_ith_param (info, i);

	  if (lat->type == IPA_CONST_VALUE
	      /* Do not count obviously unused arguments.  */
	      && (!is_gimple_reg (parm_tree)
		  || gimple_default_def (DECL_STRUCT_FUNCTION (node->decl),
					 parm_tree)))
	    {
	      replace_param =
		ipcp_create_replace_map (parm_tree, lat);
	      VARRAY_PUSH_GENERIC_PTR (replace_trees, replace_param);
	    }
	}

      /* Compute how many callers node has.  */
      node_callers = 0;
      for (cs = node->callers; cs != NULL; cs = cs->next_caller)
	node_callers++;
      redirect_callers = VEC_alloc (cgraph_edge_p, heap, node_callers);
      for (cs = node->callers; cs != NULL; cs = cs->next_caller)
	VEC_quick_push (cgraph_edge_p, redirect_callers, cs);

      /* Redirecting all the callers of the node to the
         new versioned node.  */
      node1 =
	cgraph_function_versioning (node, redirect_callers, replace_trees);
      VEC_free (cgraph_edge_p, heap, redirect_callers);
      VARRAY_CLEAR (replace_trees);
      if (node1 == NULL)
	continue;
      if (dump_file)
	fprintf (dump_file, "versioned function %s with growth %i, overall %i\n",
		 cgraph_node_name (node), (int)growth, (int)new_insns);
      ipcp_init_cloned_node (node, node1);

      /* We've possibly introduced direct calls.  */
      ipcp_update_cloned_node (node1);

      if (dump_file)
	dump_function_to_file (node1->decl, dump_file, dump_flags);

      for (cs = node->callees; cs; cs = cs->next_callee)
        if (cs->callee->aux)
	  {
	    fibheap_delete_node (heap, (fibnode_t) cs->callee->aux);
	    cs->callee->aux = fibheap_insert (heap,
	    				      ipcp_estimate_cloning_cost (cs->callee),
					      cs->callee);
	  }
    }

  while (!fibheap_empty (heap))
    {
      if (dump_file)
	fprintf (dump_file, "skipping function %s\n",
		 cgraph_node_name (node));
      node = (struct cgraph_node *) fibheap_extract_min (heap);
      node->aux = NULL;
    }
  fibheap_delete (heap);
  BITMAP_FREE (dead_nodes);
  ipcp_update_callgraph ();
  ipcp_update_profiling ();
}

/* The IPCP driver.  */
static unsigned int
ipcp_driver (void)
{
  /* 2. Do the interprocedural propagation.  */
  ipcp_iterate_stage ();
  if (dump_file)
    {
      fprintf (dump_file, "\nIPA structures after propagation:\n");
      ipcp_print_all_structures (dump_file);
      fprintf (dump_file, "\nProfiling info before insert stage:\n");
      ipcp_print_profile_data (dump_file);
    }
  /* 3. Insert the constants found to the functions.  */
  ipcp_insert_stage ();
  if (dump_file)
    {
      fprintf (dump_file, "\nProfiling info after insert stage:\n");
      ipcp_print_profile_data (dump_file);
    }
  /* Free all IPCP structures.  */
  free_all_ipa_structures_after_ipa_cp ();
  if (dump_file)
    fprintf (dump_file, "\nIPA constant propagation end\n");
  cgraph_remove_unreachable_nodes (true, NULL);
  return 0;
}

/* Note function body size.  */
static void
ipcp_generate_summary (void)
{
  if (dump_file)
    fprintf (dump_file, "\nIPA constant propagation start:\n");
  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  ipa_register_cgraph_hooks ();
  /* 1. Call the init stage to initialize 
     the ipa_node_params and ipa_edge_args structures.  */
  ipcp_init_stage ();
  if (dump_file)
    {
      fprintf (dump_file, "\nIPA structures before propagation:\n");
      ipcp_print_all_structures (dump_file);
    }
}

/* Gate for IPCP optimization.  */
static bool
cgraph_gate_cp (void)
{
  return flag_ipa_cp;
}

struct ipa_opt_pass pass_ipa_cp = 
{
 {
  IPA_PASS,
  "cp",				/* name */
  cgraph_gate_cp,		/* gate */
  ipcp_driver,			/* execute */
  NULL,				/* sub */
  NULL,				/* next */
  0,				/* static_pass_number */
  TV_IPA_CONSTANT_PROP,		/* tv_id */
  0,				/* properties_required */
  PROP_trees,			/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_cgraph | TODO_dump_func	/* todo_flags_finish */
 },
 ipcp_generate_summary,			/* generate_summary */
 NULL,					/* write_summary */
 NULL,					/* read_summary */
 NULL,					/* function_read_summary */
 0,					/* TODOs */
 NULL,					/* function_transform */
 NULL,					/* variable_transform */
};
