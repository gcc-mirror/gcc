/* Interprocedural constant propagation
   Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
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

/* Number of functions identified as candidates for cloning. When not cloning
   we can simplify iterate stage not forcing it to go through the decision
   on what is profitable and what not.  */
static int n_cloning_candidates;

/* Maximal count found in program.  */
static gcov_type max_count;

/* Cgraph nodes that has been completely replaced by cloning during iterate
 * stage and will be removed after ipcp is finished.  */
static bitmap dead_nodes;

static void ipcp_print_profile_data (FILE *);
static void ipcp_function_scale_print (FILE *);

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
  ipa_initialize_node_params (new_node);
  IPA_NODE_REF (new_node)->ipcp_orig_node = orig_node;
}

/* Perform intraprocedrual analysis needed for ipcp.  */
static void
ipcp_analyze_node (struct cgraph_node *node)
{
  /* Unreachable nodes should have been eliminated before ipcp.  */
  gcc_assert (node->needed || node->reachable);

  ipa_initialize_node_params (node);
  ipa_detect_param_modifications (node);
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
ipcp_get_lattice (struct ipa_node_params *info, int i)
{
  return &(info->params[i].ipcp_lattice);
}

/* Given the jump function JFUNC, compute the lattice LAT that describes the
   value coming down the callsite. INFO describes the caller node so that
   pass-through jump functions can be evaluated.  */
static void
ipcp_lattice_from_jfunc (struct ipa_node_params *info, struct ipcp_lattice *lat,
			 struct ipa_jump_func *jfunc)
{
  if (jfunc->type == IPA_JF_CONST)
    {
      lat->type = IPA_CONST_VALUE;
      lat->constant = jfunc->value.constant;
    }
  else if (jfunc->type == IPA_JF_PASS_THROUGH)
    {
      struct ipcp_lattice *caller_lat;

      caller_lat = ipcp_get_lattice (info, jfunc->value.formal_id);
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

  fprintf (f, "\nLattice:\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info;

      if (!node->analyzed)
	continue;
      info = IPA_NODE_REF (node);
      fprintf (f, "  Node: %s:\n", cgraph_node_name (node));
      count = ipa_get_param_count (info);
      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipcp_get_lattice (info, i);

	  fprintf (f, "    param [%d]: ", i);
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

/* Return true if ipcp algorithms would allow cloning NODE.  */

static bool
ipcp_versionable_function_p (struct cgraph_node *node)
{
  tree decl = node->decl;
  basic_block bb;

  /* There are a number of generic reasons functions cannot be versioned.  */
  if (!tree_versionable_function_p (decl))
    return false;

  /* Removing arguments doesn't work if the function takes varargs.  */
  if (DECL_STRUCT_FUNCTION (decl)->stdarg)
    return false;

  /* Removing arguments doesn't work if we use __builtin_apply_args.  */
  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (decl))
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  const_gimple stmt = gsi_stmt (gsi);
	  tree t;

	  if (!is_gimple_call (stmt))
	    continue;
	  t = gimple_call_fndecl (stmt);
	  if (t == NULL_TREE)
	    continue;
	  if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (t) == BUILT_IN_APPLY_ARGS)
	    return false;
	}
    }

  return true;
}

/* Return true if this NODE is viable candidate for cloning.  */
static bool
ipcp_cloning_candidate_p (struct cgraph_node *node)
{
  int n_calls = 0;
  int n_hot_calls = 0;
  gcov_type direct_call_sum = 0;
  struct cgraph_edge *e;

  /* We never clone functions that are not visible from outside.
     FIXME: in future we should clone such functions when they are called with
     different constants, but current ipcp implementation is not good on this.
     */
  if (!node->needed || !node->analyzed)
    return false;

  if (cgraph_function_body_availability (node) <= AVAIL_OVERWRITABLE)
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; body is overwrittable.\n",
 	         cgraph_node_name (node));
      return false;
    }
  if (!ipcp_versionable_function_p (node))
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; body is not versionable.\n",
 	         cgraph_node_name (node));
      return false;
    }
  for (e = node->callers; e; e = e->next_caller)
    {
      direct_call_sum += e->count;
      n_calls ++;
      if (cgraph_maybe_hot_edge_p (e))
	n_hot_calls ++;
    }
  
  if (!n_calls)
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; no direct calls.\n",
 	         cgraph_node_name (node));
      return false;
    }
  if (node->local.inline_summary.self_size < n_calls)
    {
      if (dump_file)
        fprintf (dump_file, "Considering %s for cloning; code would shrink.\n",
 	         cgraph_node_name (node));
      return true;
    }  

  if (!flag_ipa_cp_clone)
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; -fipa-cp-clone disabled.\n",
 	         cgraph_node_name (node));
      return false;
    }

  if (!optimize_function_for_speed_p (DECL_STRUCT_FUNCTION (node->decl)))
    {
      if (dump_file)
        fprintf (dump_file, "Not considering %s for cloning; optimizing it for size.\n",
 	         cgraph_node_name (node));
      return false;
    }

  /* When profile is available and function is hot, propagate into it even if
     calls seems cold; constant propagation can improve function's speed
     significandly.  */
  if (max_count)
    {
      if (direct_call_sum > node->count * 90 / 100)
	{
	  if (dump_file)
	    fprintf (dump_file, "Considering %s for cloning; usually called directly.\n",
		     cgraph_node_name (node));
	  return true;
        }
    }
  if (!n_hot_calls)
    {
      if (dump_file)
	fprintf (dump_file, "Not considering %s for cloning; no hot calls.\n",
		 cgraph_node_name (node));
      return false;
    }
  if (dump_file)
    fprintf (dump_file, "Considering %s for cloning.\n",
	     cgraph_node_name (node));
  return true;
}

/* Initialize ipcp_lattices array.  The lattices corresponding to supported
   types (integers, real types and Fortran constants defined as const_decls)
   are initialized to IPA_TOP, the rest of them to IPA_BOTTOM.  */
static void
ipcp_initialize_node_lattices (struct cgraph_node *node)
{
  int i;
  struct ipa_node_params *info = IPA_NODE_REF (node);
  enum ipa_lattice_type type;

  if (ipa_is_called_with_var_arguments (info))
    type = IPA_BOTTOM;
  else if (!node->needed)
    type = IPA_TOP;
  /* When cloning is allowed, we can assume that externally visible functions
     are not called.  We will compensate this by cloning later.  */
  else if (ipcp_cloning_candidate_p (node))
    type = IPA_TOP, n_cloning_candidates ++;
  else
    type = IPA_BOTTOM;

  for (i = 0; i < ipa_get_param_count (info) ; i++)
    ipcp_get_lattice (info, i)->type = type;
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
      ipcp_analyze_node (node);
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
	  struct ipcp_lattice *lat = ipcp_get_lattice (info, i);
	  if (lat->type == IPA_TOP)
	    {
	      prop_again = true;
	      if (dump_file)
		{
		  fprintf (dump_file, "Forcing param ");
		  print_generic_expr (dump_file, ipa_get_param (info, i), 0);
		  fprintf (dump_file, " of node %s to bottom.\n",
			   cgraph_node_name (node));
		}
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
	      dest_lat = ipcp_get_lattice (callee_info, i);
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
  struct cgraph_node *node;
  n_cloning_candidates = 0;

  if (dump_file)
    fprintf (dump_file, "\nIPA iterate stage:\n\n");
  for (node = cgraph_nodes; node; node = node->next)
    {
      ipcp_initialize_node_lattices (node);
      ipcp_compute_node_scale (node);
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      ipcp_print_all_lattices (dump_file);
      ipcp_function_scale_print (dump_file);
    }

  ipcp_propagate_stage ();
  if (ipcp_change_tops_to_bottom ())
    /* Some lattices have changed from IPA_TOP to IPA_BOTTOM.
       This change should be propagated.  */
    {
      gcc_assert (n_cloning_candidates);
      ipcp_propagate_stage ();
    }
  if (dump_file)
    {
      fprintf (dump_file, "\nIPA lattices after propagation:\n");
      ipcp_print_all_lattices (dump_file);
      if (dump_flags & TDF_DETAILS)
        ipcp_print_profile_data (dump_file);
    }
}

/* Check conditions to forbid constant insertion to function described by
   NODE.  */
static inline bool
ipcp_node_modifiable_p (struct cgraph_node *node)
{
  /* Once we will be able to do in-place replacement, we can be more
     lax here.  */
  return ipcp_versionable_function_p (node);
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

/* Print profile info for all functions.  */
static void
ipcp_print_profile_data (FILE * f)
{
  fprintf (f, "\nNODE COUNTS :\n");
  ipcp_print_func_profile_counts (f);
  fprintf (f, "\nCS COUNTS stage:\n");
  ipcp_print_call_profile_counts (f);
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

  replace_map = GGC_NEW (struct ipa_replace_map);
  const_val = build_const_val (lat, TREE_TYPE (parm_tree));
  if (dump_file)
    {
      fprintf (dump_file, "  replacing param ");
      print_generic_expr (dump_file, parm_tree, 0);
      fprintf (dump_file, " with const ");
      print_generic_expr (dump_file, const_val, 0);
      fprintf (dump_file, "\n");
    }
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

  if (!n_cloning_candidates)
    return false;

  if ((orig = ipcp_get_orig_node (node)) != NULL)
    node = orig;
  if (ipcp_get_orig_node (cs->caller))
    return false;

  orig_callee_info = IPA_NODE_REF (node);
  count = ipa_get_param_count (orig_callee_info);
  for (i = 0; i < count; i++)
    {
      struct ipcp_lattice *lat = ipcp_get_lattice (orig_callee_info, i);
      if (ipcp_lat_is_const (lat))
	{
	  jump_func = ipa_get_ith_jump_func (IPA_EDGE_REF (cs), i);
	  if (jump_func->type != IPA_JF_CONST)
	    return true;
	}
    }

  return false;
}

/* Fix the callsites and the call graph after function cloning was done.  */
static void
ipcp_update_callgraph (void)
{
  struct cgraph_node *node;

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed && ipcp_node_is_clone (node))
      {
	bitmap args_to_skip = BITMAP_ALLOC (NULL);
	struct cgraph_node *orig_node = ipcp_get_orig_node (node);
        struct ipa_node_params *info = IPA_NODE_REF (orig_node);
        int i, count = ipa_get_param_count (info);
        struct cgraph_edge *cs, *next;

	for (i = 0; i < count; i++)
	  {
	    struct ipcp_lattice *lat = ipcp_get_lattice (info, i);
	    tree parm_tree = ipa_get_param (info, i);

	    /* We can proactively remove obviously unused arguments.  */
	    if (is_gimple_reg (parm_tree)
		&& !gimple_default_def (DECL_STRUCT_FUNCTION (orig_node->decl),
					parm_tree))
	      {
		bitmap_set_bit (args_to_skip, i);
		continue;
	      }

	    if (lat->type == IPA_CONST_VALUE)
	      bitmap_set_bit (args_to_skip, i);
	  }
	for (cs = node->callers; cs; cs = next)
	  {
	    next = cs->next_caller;
	    if (!ipcp_node_is_clone (cs->caller) && ipcp_need_redirect_p (cs))
	      cgraph_redirect_edge_callee (cs, orig_node);
	  }
      }
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
	}
    }
}

/* If NODE was cloned, how much would program grow? */
static long
ipcp_estimate_growth (struct cgraph_node *node)
{
  struct cgraph_edge *cs;
  int redirectable_node_callers = 0;
  int removable_args = 0;
  bool need_original = node->needed;
  struct ipa_node_params *info;
  int i, count;
  int growth;

  for (cs = node->callers; cs != NULL; cs = cs->next_caller)
    if (cs->caller == node || !ipcp_need_redirect_p (cs))
      redirectable_node_callers++;
    else
      need_original = true;

  /* If we will be able to fully replace orignal node, we never increase
     program size.  */
  if (!need_original)
    return 0;

  info = IPA_NODE_REF (node);
  count = ipa_get_param_count (info);
  for (i = 0; i < count; i++)
    {
      struct ipcp_lattice *lat = ipcp_get_lattice (info, i);
      tree parm_tree = ipa_get_param (info, i);

      /* We can proactively remove obviously unused arguments.  */
      if (is_gimple_reg (parm_tree)
	  && !gimple_default_def (DECL_STRUCT_FUNCTION (node->decl),
				  parm_tree))
	removable_args++;

      if (lat->type == IPA_CONST_VALUE)
	removable_args++;
    }

  /* We make just very simple estimate of savings for removal of operand from
     call site.  Precise cost is dificult to get, as our size metric counts
     constants and moves as free.  Generally we are looking for cases that
     small function is called very many times.  */
  growth = node->local.inline_summary.self_size
  	   - removable_args * redirectable_node_callers;
  if (growth < 0)
    return 0;
  return growth;
}


/* Estimate cost of cloning NODE.  */
static long
ipcp_estimate_cloning_cost (struct cgraph_node *node)
{
  int freq_sum = 1;
  gcov_type count_sum = 1;
  struct cgraph_edge *e;
  int cost;

  cost = ipcp_estimate_growth (node) * 1000;
  if (!cost)
    {
      if (dump_file)
        fprintf (dump_file, "Versioning of %s will save code size\n",
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

  if (max_count)
    cost /= count_sum * 1000 / max_count + 1;
  else
    cost /= freq_sum * 1000 / REG_BR_PROB_BASE + 1;
  if (dump_file)
    fprintf (dump_file, "Cost of versioning %s is %i, (size: %i, freq: %i)\n",
             cgraph_node_name (node), cost, node->local.inline_summary.self_size,
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
      struct ipcp_lattice *lat = ipcp_get_lattice (info, i);
      tree parm_tree = ipa_get_param (info, i);
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
  VEC (ipa_replace_map_p,gc)* replace_trees;
  int node_callers, count;
  tree parm_tree;
  struct ipa_replace_map *replace_param;
  fibheap_t heap;
  long overall_size = 0, new_size = 0;
  long max_new_size;

  ipa_check_create_node_params ();
  ipa_check_create_edge_args ();
  if (dump_file)
    fprintf (dump_file, "\nIPA insert stage:\n\n");

  dead_nodes = BITMAP_ALLOC (NULL);

  for (node = cgraph_nodes; node; node = node->next)
    if (node->analyzed)
      {
	if (node->count > max_count)
	  max_count = node->count;
	overall_size += node->local.inline_summary.self_size;
      }

  max_new_size = overall_size;
  if (max_new_size < PARAM_VALUE (PARAM_LARGE_UNIT_INSNS))
    max_new_size = PARAM_VALUE (PARAM_LARGE_UNIT_INSNS);
  max_new_size = max_new_size * PARAM_VALUE (PARAM_IPCP_UNIT_GROWTH) / 100 + 1;

  /* First collect all functions we proved to have constant arguments to heap.  */
  heap = fibheap_new ();
  for (node = cgraph_nodes; node; node = node->next)
    {
      struct ipa_node_params *info;
      /* Propagation of the constant is forbidden in certain conditions.  */
      if (!node->analyzed || !ipcp_node_modifiable_p (node))
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
      bitmap args_to_skip;
      struct cgraph_edge *cs;

      node = (struct cgraph_node *)fibheap_extract_min (heap);
      node->aux = NULL;
      if (dump_file)
	fprintf (dump_file, "considering function %s\n",
		 cgraph_node_name (node));

      growth = ipcp_estimate_growth (node);

      if (new_size + growth > max_new_size)
	break;
      if (growth
	  && optimize_function_for_size_p (DECL_STRUCT_FUNCTION (node->decl)))
	{
	  if (dump_file)
	    fprintf (dump_file, "Not versioning, cold code would grow");
	  continue;
	}

      new_size += growth;

      /* Look if original function becomes dead after clonning.  */
      for (cs = node->callers; cs != NULL; cs = cs->next_caller)
	if (cs->caller == node || ipcp_need_redirect_p (cs))
	  break;
      if (!cs && !node->needed)
	bitmap_set_bit (dead_nodes, node->uid);

      info = IPA_NODE_REF (node);
      count = ipa_get_param_count (info);

      replace_trees = VEC_alloc (ipa_replace_map_p, gc, 1);
      args_to_skip = BITMAP_GGC_ALLOC ();
      for (i = 0; i < count; i++)
	{
	  struct ipcp_lattice *lat = ipcp_get_lattice (info, i);
	  parm_tree = ipa_get_param (info, i);

	  /* We can proactively remove obviously unused arguments.  */
	  if (is_gimple_reg (parm_tree)
	      && !gimple_default_def (DECL_STRUCT_FUNCTION (node->decl),
				      parm_tree))
	    {
	      bitmap_set_bit (args_to_skip, i);
	      continue;
	    }

	  if (lat->type == IPA_CONST_VALUE)
	    {
	      replace_param =
		ipcp_create_replace_map (parm_tree, lat);
	      VEC_safe_push (ipa_replace_map_p, gc, replace_trees, replace_param);
	      bitmap_set_bit (args_to_skip, i);
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
	cgraph_create_virtual_clone (node, redirect_callers, replace_trees,
				     args_to_skip);
      args_to_skip = NULL;
      VEC_free (cgraph_edge_p, heap, redirect_callers);
      replace_trees = NULL;

      if (node1 == NULL)
	continue;
      if (dump_file)
	fprintf (dump_file, "versioned function %s with growth %i, overall %i\n",
		 cgraph_node_name (node), (int)growth, (int)new_size);
      ipcp_init_cloned_node (node, node1);

      /* TODO: We can use indirect inlning info to produce new calls.  */

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
  cgraph_remove_unreachable_nodes (true,dump_file);
  if (dump_file)
    {
      fprintf (dump_file, "\nIPA structures before propagation:\n");
      if (dump_flags & TDF_DETAILS)
        ipa_print_all_params (dump_file);
      ipa_print_all_jump_functions (dump_file);
    }
  /* 2. Do the interprocedural propagation.  */
  ipcp_iterate_stage ();
  /* 3. Insert the constants found to the functions.  */
  ipcp_insert_stage ();
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nProfiling info after insert stage:\n");
      ipcp_print_profile_data (dump_file);
    }
  /* Free all IPCP structures.  */
  free_all_ipa_structures_after_ipa_cp ();
  if (dump_file)
    fprintf (dump_file, "\nIPA constant propagation end\n");
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
}

/* Gate for IPCP optimization.  */
static bool
cgraph_gate_cp (void)
{
  return flag_ipa_cp;
}

struct ipa_opt_pass_d pass_ipa_cp =
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
  0,				/* properties_provided */
  0,				/* properties_destroyed */
  0,				/* todo_flags_start */
  TODO_dump_cgraph | TODO_dump_func |
  TODO_remove_functions /* todo_flags_finish */
 },
 ipcp_generate_summary,			/* generate_summary */
 NULL,					/* write_summary */
 NULL,					/* read_summary */
 NULL,					/* function_read_summary */
 0,					/* TODOs */
 NULL,					/* function_transform */
 NULL,					/* variable_transform */
};
