/* Support for simple predicate analysis.

   Copyright (C) 2001-2021 Free Software Foundation, Inc.
   Contributed by Xinliang David Li <davidxl@google.com>
   Generalized by Martin Sebor <msebor@redhat.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "cfghooks.h"
#include "attribs.h"
#include "builtins.h"
#include "calls.h"
#include "value-query.h"

#include "gimple-predicate-analysis.h"

#define DEBUG_PREDICATE_ANALYZER 1

/* Find the immediate postdominator of the specified basic block BB.  */

static inline basic_block
find_pdom (basic_block bb)
{
  basic_block exit_bb = EXIT_BLOCK_PTR_FOR_FN (cfun);
  if (bb == exit_bb)
    return exit_bb;

  if (basic_block pdom = get_immediate_dominator (CDI_POST_DOMINATORS, bb))
    return pdom;

  return exit_bb;
}

/* Find the immediate dominator of the specified basic block BB.  */

static inline basic_block
find_dom (basic_block bb)
{
  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  if (bb == entry_bb)
    return entry_bb;

  if (basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb))
    return dom;

  return entry_bb;
}

/* Return true if BB1 is postdominating BB2 and BB1 is not a loop exit
   bb.  The loop exit bb check is simple and does not cover all cases.  */

static bool
is_non_loop_exit_postdominating (basic_block bb1, basic_block bb2)
{
  if (!dominated_by_p (CDI_POST_DOMINATORS, bb2, bb1))
    return false;

  if (single_pred_p (bb1) && !single_succ_p (bb2))
    return false;

  return true;
}

/* Find BB's closest postdominator that is its control equivalent (i.e.,
   that's controlled by the same predicate).  */

static inline basic_block
find_control_equiv_block (basic_block bb)
{
  basic_block pdom = find_pdom (bb);

  /* Skip the postdominating bb that is also a loop exit.  */
  if (!is_non_loop_exit_postdominating (pdom, bb))
    return NULL;

  /* If the postdominator is dominated by BB, return it.  */
  if (dominated_by_p (CDI_DOMINATORS, pdom, bb))
    return pdom;

  return NULL;
}

/* Return true if X1 is the negation of X2.  */

static inline bool
pred_neg_p (const pred_info &x1, const pred_info &x2)
{
  if (!operand_equal_p (x1.pred_lhs, x2.pred_lhs, 0)
      || !operand_equal_p (x1.pred_rhs, x2.pred_rhs, 0))
    return false;

  tree_code c1 = x1.cond_code, c2;
  if (x1.invert == x2.invert)
    c2 = invert_tree_comparison (x2.cond_code, false);
  else
    c2 = x2.cond_code;

  return c1 == c2;
}

/* Return whether the condition (VAL CMPC BOUNDARY) is true.  */

static bool
is_value_included_in (tree val, tree boundary, tree_code cmpc)
{
  /* Only handle integer constant here.  */
  if (TREE_CODE (val) != INTEGER_CST || TREE_CODE (boundary) != INTEGER_CST)
    return true;

  bool inverted = false;
  if (cmpc == GE_EXPR || cmpc == GT_EXPR || cmpc == NE_EXPR)
    {
      cmpc = invert_tree_comparison (cmpc, false);
      inverted = true;
    }

  bool result;
  if (cmpc == EQ_EXPR)
    result = tree_int_cst_equal (val, boundary);
  else if (cmpc == LT_EXPR)
    result = tree_int_cst_lt (val, boundary);
  else
    {
      gcc_assert (cmpc == LE_EXPR);
      result = tree_int_cst_le (val, boundary);
    }

  if (inverted)
    result ^= 1;

  return result;
}

/* Format the vector of edges EV as a string.  */

static std::string
format_edge_vec (const vec<edge> &ev)
{
  std::string str;

  unsigned n = ev.length ();
  for (unsigned i = 0; i < n; ++i)
    {
      char es[32];
      const_edge e = ev[i];
      sprintf (es, "%u", e->src->index);
      str += es;
      if (i + 1 < n)
	str += " -> ";
    }
  return str;
}

/* Format the first N elements of the array of vector of edges EVA as
   a string.  */

static std::string
format_edge_vecs (const vec<edge> eva[], unsigned n)
{
  std::string str;

  for (unsigned i = 0; i != n; ++i)
    {
      str += '{';
      str += format_edge_vec (eva[i]);
      str += '}';
      if (i + 1 < n)
	str += ", ";
    }
  return str;
}

/* Dump a single pred_info to DUMP_FILE.  */

static void
dump_pred_info (const pred_info &pred)
{
  if (pred.invert)
    fprintf (dump_file, "NOT (");
  print_generic_expr (dump_file, pred.pred_lhs);
  fprintf (dump_file, " %s ", op_symbol_code (pred.cond_code));
  print_generic_expr (dump_file, pred.pred_rhs);
  if (pred.invert)
    fputc (')', dump_file);
}

/* Dump a pred_chain to DUMP_FILE.  */

static void
dump_pred_chain (const pred_chain &chain)
{
  unsigned np = chain.length ();
  if (np > 1)
    fprintf (dump_file, "AND (");

  for (unsigned j = 0; j < np; j++)
    {
      dump_pred_info (chain[j]);
      if (j < np - 1)
	fprintf (dump_file, ", ");
      else if (j > 0)
	fputc (')', dump_file);
    }
}

/* Dump the predicate chain PREDS for STMT, prefixed by MSG.  */

static void
dump_predicates (gimple *stmt, const pred_chain_union &preds, const char *msg)
{
  fprintf (dump_file, "%s", msg);
  if (stmt)
    {
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "is guarded by:\n");
    }

  unsigned np = preds.length ();
  if (np > 1)
    fprintf (dump_file, "OR (");
  for (unsigned i = 0; i < np; i++)
    {
      dump_pred_chain (preds[i]);
      if (i < np - 1)
	fprintf (dump_file, ", ");
      else if (i > 0)
	fputc (')', dump_file);
    }
  fputc ('\n', dump_file);
}

/* Dump the first NCHAINS elements of the DEP_CHAINS array into DUMP_FILE.  */

static void
dump_dep_chains (const auto_vec<edge> dep_chains[], unsigned nchains)
{
  if (!dump_file)
    return;

  for (unsigned i = 0; i != nchains; ++i)
    {
      const auto_vec<edge> &v = dep_chains[i];
      unsigned n = v.length ();
      for (unsigned j = 0; j != n; ++j)
	{
	  fprintf (dump_file, "%u", v[j]->src->index);
	  if (j + 1 < n)
	    fprintf (dump_file, " -> ");
	}
      fputc ('\n', dump_file);
    }
}

/* Return the 'normalized' conditional code with operand swapping
   and condition inversion controlled by SWAP_COND and INVERT.  */

static tree_code
get_cmp_code (tree_code orig_cmp_code, bool swap_cond, bool invert)
{
  tree_code tc = orig_cmp_code;

  if (swap_cond)
    tc = swap_tree_comparison (orig_cmp_code);
  if (invert)
    tc = invert_tree_comparison (tc, false);

  switch (tc)
    {
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      break;
    default:
      return ERROR_MARK;
    }
  return tc;
}

/* Return true if PRED is common among all predicate chains in PREDS
   (and therefore can be factored out).  */

static bool
find_matching_predicate_in_rest_chains (const pred_info &pred,
					const pred_chain_union &preds)
{
  /* Trival case.  */
  if (preds.length () == 1)
    return true;

  for (unsigned i = 1; i < preds.length (); i++)
    {
      bool found = false;
      const pred_chain &chain = preds[i];
      unsigned n = chain.length ();
      for (unsigned j = 0; j < n; j++)
	{
	  const pred_info &pred2 = chain[j];
	  /* Can relax the condition comparison to not use address
	     comparison.  However, the most common case is that
	     multiple control dependent paths share a common path
	     prefix, so address comparison should be ok.  */
	  if (operand_equal_p (pred2.pred_lhs, pred.pred_lhs, 0)
	      && operand_equal_p (pred2.pred_rhs, pred.pred_rhs, 0)
	      && pred2.invert == pred.invert)
	    {
	      found = true;
	      break;
	    }
	}
      if (!found)
	return false;
    }
  return true;
}

/* Find a predicate to examine against paths of interest.  If there
   is no predicate of the "FLAG_VAR CMP CONST" form, try to find one
   of that's the form "FLAG_VAR CMP FLAG_VAR" with value range info.
   PHI is the phi node whose incoming (interesting) paths need to be
   examined.  On success, return the comparison code, set defintion
   gimple of FLAG_DEF and BOUNDARY_CST.  Otherwise return ERROR_MARK.  */

static tree_code
find_var_cmp_const (pred_chain_union preds, gphi *phi, gimple **flag_def,
		    tree *boundary_cst)
{
  tree_code vrinfo_code = ERROR_MARK;
  gimple *vrinfo_def = NULL;
  tree vrinfo_cst = NULL;

  gcc_assert (preds.length () > 0);
  pred_chain chain = preds[0];
  for (unsigned i = 0; i < chain.length (); i++)
    {
      bool use_vrinfo_p = false;
      const pred_info &pred = chain[i];
      tree cond_lhs = pred.pred_lhs;
      tree cond_rhs = pred.pred_rhs;
      if (cond_lhs == NULL_TREE || cond_rhs == NULL_TREE)
	continue;

      tree_code code = get_cmp_code (pred.cond_code, false, pred.invert);
      if (code == ERROR_MARK)
	continue;

      /* Convert to the canonical form SSA_NAME CMP CONSTANT.  */
      if (TREE_CODE (cond_lhs) == SSA_NAME
	  && is_gimple_constant (cond_rhs))
	;
      else if (TREE_CODE (cond_rhs) == SSA_NAME
	       && is_gimple_constant (cond_lhs))
	{
	  std::swap (cond_lhs, cond_rhs);
	  if ((code = get_cmp_code (code, true, false)) == ERROR_MARK)
	    continue;
	}
      /* Check if we can take advantage of FLAG_VAR COMP FLAG_VAR predicate
	 with value range info.  Note only first of such case is handled.  */
      else if (vrinfo_code == ERROR_MARK
	       && TREE_CODE (cond_lhs) == SSA_NAME
	       && TREE_CODE (cond_rhs) == SSA_NAME)
	{
	  gimple* lhs_def = SSA_NAME_DEF_STMT (cond_lhs);
	  if (!lhs_def || gimple_code (lhs_def) != GIMPLE_PHI
	      || gimple_bb (lhs_def) != gimple_bb (phi))
	    {
	      std::swap (cond_lhs, cond_rhs);
	      if ((code = get_cmp_code (code, true, false)) == ERROR_MARK)
		continue;
	    }

	  /* Check value range info of rhs, do following transforms:
	       flag_var < [min, max]  ->  flag_var < max
	       flag_var > [min, max]  ->  flag_var > min

	     We can also transform LE_EXPR/GE_EXPR to LT_EXPR/GT_EXPR:
	       flag_var <= [min, max] ->  flag_var < [min, max+1]
	       flag_var >= [min, max] ->  flag_var > [min-1, max]
	     if no overflow/wrap.  */
	  tree type = TREE_TYPE (cond_lhs);
	  value_range r;
	  if (!INTEGRAL_TYPE_P (type)
	      || !get_range_query (cfun)->range_of_expr (r, cond_rhs)
	      || r.kind () != VR_RANGE)
	    continue;

	  wide_int min = r.lower_bound ();
	  wide_int max = r.upper_bound ();
	  if (code == LE_EXPR
	      && max != wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type)))
	    {
	      code = LT_EXPR;
	      max = max + 1;
	    }
	  if (code == GE_EXPR
	      && min != wi::min_value (TYPE_PRECISION (type), TYPE_SIGN (type)))
	    {
	      code = GT_EXPR;
	      min = min - 1;
	    }
	  if (code == LT_EXPR)
	    cond_rhs = wide_int_to_tree (type, max);
	  else if (code == GT_EXPR)
	    cond_rhs = wide_int_to_tree (type, min);
	  else
	    continue;

	  use_vrinfo_p = true;
	}
      else
	continue;

      if ((*flag_def = SSA_NAME_DEF_STMT (cond_lhs)) == NULL)
	continue;

      if (gimple_code (*flag_def) != GIMPLE_PHI
	  || gimple_bb (*flag_def) != gimple_bb (phi)
	  || !find_matching_predicate_in_rest_chains (pred, preds))
	continue;

      /* Return if any "flag_var comp const" predicate is found.  */
      if (!use_vrinfo_p)
	{
	  *boundary_cst = cond_rhs;
	  return code;
	}
      /* Record if any "flag_var comp flag_var[vinfo]" predicate is found.  */
      else if (vrinfo_code == ERROR_MARK)
	{
	  vrinfo_code = code;
	  vrinfo_def = *flag_def;
	  vrinfo_cst = cond_rhs;
	}
    }
  /* Return the "flag_var cmp flag_var[vinfo]" predicate we found.  */
  if (vrinfo_code != ERROR_MARK)
    {
      *flag_def = vrinfo_def;
      *boundary_cst = vrinfo_cst;
    }
  return vrinfo_code;
}

/* Return true if all interesting opnds are pruned, false otherwise.
   PHI is the phi node with interesting operands, OPNDS is the bitmap
   of the interesting operand positions, FLAG_DEF is the statement
   defining the flag guarding the use of the PHI output, BOUNDARY_CST
   is the const value used in the predicate associated with the flag,
   CMP_CODE is the comparison code used in the predicate, VISITED_PHIS
   is the pointer set of phis visited, and VISITED_FLAG_PHIS is
   the pointer to the pointer set of flag definitions that are also
   phis.

   Example scenario:

   BB1:
     flag_1 = phi <0, 1>			// (1)
     var_1  = phi <undef, some_val>


   BB2:
     flag_2 = phi <0,   flag_1, flag_1>		// (2)
     var_2  = phi <undef, var_1, var_1>
     if (flag_2 == 1)
       goto BB3;

   BB3:
     use of var_2				// (3)

   Because some flag arg in (1) is not constant, if we do not look into
   the flag phis recursively, it is conservatively treated as unknown and
   var_1 is thought to flow into use at (3).  Since var_1 is potentially
   uninitialized a false warning will be emitted.
   Checking recursively into (1), the compiler can find out that only
   some_val (which is defined) can flow into (3) which is OK.  */

static bool
prune_phi_opnds (gphi *phi, unsigned opnds, gphi *flag_def,
		 tree boundary_cst, tree_code cmp_code,
		 predicate::func_t &eval,
		 hash_set<gphi *> *visited_phis,
		 bitmap *visited_flag_phis)
{
  /* The Boolean predicate guarding the PHI definition.  Initialized
     lazily from PHI in the first call to is_use_guarded() and cached
     for subsequent iterations.  */
  predicate def_preds (eval);

  unsigned n = MIN (eval.max_phi_args, gimple_phi_num_args (flag_def));
  for (unsigned i = 0; i < n; i++)
    {
      if (!MASK_TEST_BIT (opnds, i))
	continue;

      tree flag_arg = gimple_phi_arg_def (flag_def, i);
      if (!is_gimple_constant (flag_arg))
	{
	  if (TREE_CODE (flag_arg) != SSA_NAME)
	    return false;

	  gphi *flag_arg_def = dyn_cast<gphi *> (SSA_NAME_DEF_STMT (flag_arg));
	  if (!flag_arg_def)
	    return false;

	  tree phi_arg = gimple_phi_arg_def (phi, i);
	  if (TREE_CODE (phi_arg) != SSA_NAME)
	    return false;

	  gphi *phi_arg_def = dyn_cast<gphi *> (SSA_NAME_DEF_STMT (phi_arg));
	  if (!phi_arg_def)
	    return false;

	  if (gimple_bb (phi_arg_def) != gimple_bb (flag_arg_def))
	    return false;

	  if (!*visited_flag_phis)
	    *visited_flag_phis = BITMAP_ALLOC (NULL);

	  tree phi_result = gimple_phi_result (flag_arg_def);
	  if (bitmap_bit_p (*visited_flag_phis, SSA_NAME_VERSION (phi_result)))
	    return false;

	  bitmap_set_bit (*visited_flag_phis, SSA_NAME_VERSION (phi_result));

	  /* Now recursively try to prune the interesting phi args.  */
	  unsigned opnds_arg_phi = eval.phi_arg_set (phi_arg_def);
	  if (!prune_phi_opnds (phi_arg_def, opnds_arg_phi, flag_arg_def,
				boundary_cst, cmp_code, eval, visited_phis,
				visited_flag_phis))
	    return false;

	  bitmap_clear_bit (*visited_flag_phis, SSA_NAME_VERSION (phi_result));
	  continue;
	}

      /* Now check if the constant is in the guarded range.  */
      if (is_value_included_in (flag_arg, boundary_cst, cmp_code))
	{
	  /* Now that we know that this undefined edge is not pruned.
	     If the operand is defined by another phi, we can further
	     prune the incoming edges of that phi by checking
	     the predicates of this operands.  */

	  tree opnd = gimple_phi_arg_def (phi, i);
	  gimple *opnd_def = SSA_NAME_DEF_STMT (opnd);
	  if (gphi *opnd_def_phi = dyn_cast <gphi *> (opnd_def))
	    {
	      unsigned opnds2 = eval.phi_arg_set (opnd_def_phi);
	      if (!MASK_EMPTY (opnds2))
		{
		  edge opnd_edge = gimple_phi_arg_edge (phi, i);
		  if (def_preds.is_use_guarded (phi, opnd_edge->src,
						opnd_def_phi, opnds2,
						visited_phis))
		    return false;
		}
	    }
	  else
	    return false;
	}
    }

  return true;
}

/* Recursively compute the set PHI's incoming edges with "uninteresting"
   operands of a phi chain, i.e., those for which EVAL returns false.
   CD_ROOT is the control dependence root from which edges are collected
   up the CFG nodes that it's dominated by.  *EDGES holds the result, and
   VISITED is used for detecting cycles.  */

static void
collect_phi_def_edges (gphi *phi, basic_block cd_root, auto_vec<edge> *edges,
		       predicate::func_t &eval, hash_set<gimple *> *visited)
{
  if (visited->elements () == 0
      && DEBUG_PREDICATE_ANALYZER
      && dump_file)
    {
      fprintf (dump_file, "%s for cd_root %u and ",
	       __func__, cd_root->index);
      print_gimple_stmt (dump_file, phi, 0);

    }

  if (visited->add (phi))
    return;

  unsigned n = gimple_phi_num_args (phi);
  for (unsigned i = 0; i < n; i++)
    {
      edge opnd_edge = gimple_phi_arg_edge (phi, i);
      tree opnd = gimple_phi_arg_def (phi, i);

      if (TREE_CODE (opnd) == SSA_NAME)
	{
	  gimple *def = SSA_NAME_DEF_STMT (opnd);

	  if (gimple_code (def) == GIMPLE_PHI
	      && dominated_by_p (CDI_DOMINATORS, gimple_bb (def), cd_root))
	    collect_phi_def_edges (as_a<gphi *> (def), cd_root, edges, eval,
				   visited);
	  else if (!eval (opnd))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file,
			   "\tFound def edge %i -> %i for cd_root %i "
			   "and operand %u of: ",
			   opnd_edge->src->index, opnd_edge->dest->index,
			   cd_root->index, i);
		  print_gimple_stmt (dump_file, phi, 0);
		}
	      edges->safe_push (opnd_edge);
	    }
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file,
		       "\tFound def edge %i -> %i for cd_root %i "
		       "and operand %u of: ",
		       opnd_edge->src->index, opnd_edge->dest->index,
		       cd_root->index, i);
	      print_gimple_stmt (dump_file, phi, 0);
	    }

	  if (!eval (opnd))
	    edges->safe_push (opnd_edge);
	}
    }
}

/* Return an expression corresponding to the predicate PRED.  */

static tree
build_pred_expr (const pred_info &pred)
{
  tree_code cond_code = pred.cond_code;
  tree lhs = pred.pred_lhs;
  tree rhs = pred.pred_rhs;

  if (pred.invert)
    cond_code = invert_tree_comparison (cond_code, false);

  return build2 (cond_code, TREE_TYPE (lhs), lhs, rhs);
}

/* Return an expression corresponding to PREDS.  */

static tree
build_pred_expr (const pred_chain_union &preds, bool invert = false)
{
  tree_code code = invert ? TRUTH_AND_EXPR : TRUTH_OR_EXPR;
  tree_code subcode = invert ? TRUTH_OR_EXPR : TRUTH_AND_EXPR;

  tree expr = NULL_TREE;
  for (unsigned i = 0; i != preds.length (); ++i)
    {
      tree subexpr = NULL_TREE;
      for (unsigned j = 0; j != preds[i].length (); ++j)
       {
         const pred_info &pi = preds[i][j];
         tree cond = build_pred_expr (pi);
	 if (invert)
	   cond = invert_truthvalue (cond);
         subexpr = subexpr ? build2 (subcode, boolean_type_node,
                                     subexpr, cond) : cond;
       }
      if (expr)
       expr = build2 (code, boolean_type_node, expr, subexpr);
      else
       expr = subexpr;
    }

  return expr;
}

/* Return a bitset of all PHI arguments or zero if there are too many.  */

unsigned
predicate::func_t::phi_arg_set (gphi *phi)
{
  unsigned n = gimple_phi_num_args (phi);

  if (max_phi_args < n)
    return 0;

  /* Set the least significant N bits.  */
  return (1U << n) - 1;
}

/* Determine if the predicate set of the use does not overlap with that
   of the interesting paths.  The most common senario of guarded use is
   in Example 1:
     Example 1:
	   if (some_cond)
	   {
	      x = ...;   // set x to valid
	      flag = true;
	   }

	    ... some code ...

	   if (flag)
	      use (x);   // use when x is valid

     The real world examples are usually more complicated, but similar
     and usually result from inlining:

	 bool init_func (int * x)
	 {
	     if (some_cond)
		return false;
	     *x  =  ...;   // set *x to valid
	     return true;
	 }

	 void foo (..)
	 {
	     int x;

	     if (!init_func (&x))
		return;

	     .. some_code ...
	     use (x);      // use when x is valid
	 }

     Another possible use scenario is in the following trivial example:

     Example 2:
	  if (n > 0)
	     x = 1;
	  ...
	  if (n > 0)
	    {
	      if (m < 2)
		 ... = x;
	    }

     Predicate analysis needs to compute the composite predicate:

       1) 'x' use predicate: (n > 0) .AND. (m < 2)
       2) 'x' default value  (non-def) predicate: .NOT. (n > 0)
       (the predicate chain for phi operand defs can be computed
       starting from a bb that is control equivalent to the phi's
       bb and is dominating the operand def.)

       and check overlapping:
	  (n > 0) .AND. (m < 2) .AND. (.NOT. (n > 0))
	<==> false

     This implementation provides a framework that can handle different
     scenarios.  (Note that many simple cases are handled properly without
     the predicate analysis if jump threading eliminates the merge point
     thus makes path-sensitive analysis unnecessary.)

     PHI is the phi node whose incoming (undefined) paths need to be
     pruned, and OPNDS is the bitmap holding interesting operand
     positions.  VISITED is the pointer set of phi stmts being
     checked.  */

bool
predicate::overlap (gphi *phi, unsigned opnds, hash_set<gphi *> *visited)
{
  gimple *flag_def = NULL;
  tree boundary_cst = NULL_TREE;
  bitmap visited_flag_phis = NULL;

  /* Find within the common prefix of multiple predicate chains
     a predicate that is a comparison of a flag variable against
     a constant.  */
  tree_code cmp_code = find_var_cmp_const (m_preds, phi, &flag_def,
					   &boundary_cst);
  if (cmp_code == ERROR_MARK)
    return true;

  /* Now check all the uninit incoming edges have a constant flag
     value that is in conflict with the use guard/predicate.  */
  gphi *phi_def = as_a<gphi *> (flag_def);
  bool all_pruned = prune_phi_opnds (phi, opnds, phi_def, boundary_cst,
				     cmp_code, m_eval, visited,
				     &visited_flag_phis);

  if (visited_flag_phis)
    BITMAP_FREE (visited_flag_phis);

  return !all_pruned;
}

/* Return true if two predicates PRED1 and X2 are equivalent.  Assume
   the expressions have already properly re-associated.  */

static inline bool
pred_equal_p (const pred_info &pred1, const pred_info &pred2)
{
  if (!operand_equal_p (pred1.pred_lhs, pred2.pred_lhs, 0)
      || !operand_equal_p (pred1.pred_rhs, pred2.pred_rhs, 0))
    return false;

  tree_code c1 = pred1.cond_code, c2;
  if (pred1.invert != pred2.invert
      && TREE_CODE_CLASS (pred2.cond_code) == tcc_comparison)
    c2 = invert_tree_comparison (pred2.cond_code, false);
  else
    c2 = pred2.cond_code;

  return c1 == c2;
}

/* Return true if PRED tests inequality (i.e., X != Y).  */

static inline bool
is_neq_relop_p (const pred_info &pred)
{

  return ((pred.cond_code == NE_EXPR && !pred.invert)
	  || (pred.cond_code == EQ_EXPR && pred.invert));
}

/* Returns true if PRED is of the form X != 0.  */

static inline bool
is_neq_zero_form_p (const pred_info &pred)
{
  if (!is_neq_relop_p (pred) || !integer_zerop (pred.pred_rhs)
      || TREE_CODE (pred.pred_lhs) != SSA_NAME)
    return false;
  return true;
}

/* Return true if PRED is equivalent to X != 0.  */

static inline bool
pred_expr_equal_p (const pred_info &pred, tree expr)
{
  if (!is_neq_zero_form_p (pred))
    return false;

  return operand_equal_p (pred.pred_lhs, expr, 0);
}

/* Return true if VAL satisfies (x CMPC BOUNDARY) predicate.  CMPC can
   be either one of the range comparison codes ({GE,LT,EQ,NE}_EXPR and
   the like), or BIT_AND_EXPR.  EXACT_P is only meaningful for the latter.
   Modify the question from VAL & BOUNDARY != 0 to VAL & BOUNDARY == VAL.
   For other values of CMPC, EXACT_P is ignored.  */

static bool
value_sat_pred_p (tree val, tree boundary, tree_code cmpc,
		  bool exact_p = false)
{
  if (cmpc != BIT_AND_EXPR)
    return is_value_included_in (val, boundary, cmpc);

  wide_int andw = wi::to_wide (val) & wi::to_wide (boundary);
  if (exact_p)
    return andw == wi::to_wide (val);

  return andw.to_uhwi ();
}

/* Return true if the domain of single predicate expression PRED1
   is a subset of that of PRED2, and false if it cannot be proved.  */

static bool
subset_of (const pred_info &pred1, const pred_info &pred2)
{
  if (pred_equal_p (pred1, pred2))
    return true;

  if ((TREE_CODE (pred1.pred_rhs) != INTEGER_CST)
      || (TREE_CODE (pred2.pred_rhs) != INTEGER_CST))
    return false;

  if (!operand_equal_p (pred1.pred_lhs, pred2.pred_lhs, 0))
    return false;

  tree_code code1 = pred1.cond_code;
  if (pred1.invert)
    code1 = invert_tree_comparison (code1, false);
  tree_code code2 = pred2.cond_code;
  if (pred2.invert)
    code2 = invert_tree_comparison (code2, false);

  if (code2 == NE_EXPR && code1 == NE_EXPR)
    return false;

  if (code2 == NE_EXPR)
    return !value_sat_pred_p (pred2.pred_rhs, pred1.pred_rhs, code1);

  if (code1 == EQ_EXPR)
    return value_sat_pred_p (pred1.pred_rhs, pred2.pred_rhs, code2);

  if (code1 == code2)
    return value_sat_pred_p (pred1.pred_rhs, pred2.pred_rhs, code2,
			     code1 == BIT_AND_EXPR);

  return false;
}

/* Return true if the domain of CHAIN1 is a subset of that of CHAIN2.
   Return false if it cannot be proven so.  */

static bool
subset_of (const pred_chain &chain1, const pred_chain &chain2)
{
  unsigned np1 = chain1.length ();
  unsigned np2 = chain2.length ();
  for (unsigned i2 = 0; i2 < np2; i2++)
    {
      bool found = false;
      const pred_info &info2 = chain2[i2];
      for (unsigned i1 = 0; i1 < np1; i1++)
	{
	  const pred_info &info1 = chain1[i1];
	  if (subset_of (info1, info2))
	    {
	      found = true;
	      break;
	    }
	}
      if (!found)
	return false;
    }
  return true;
}

/* Return true if the domain defined by the predicate chain PREDS is
   a subset of the domain of *THIS.  Return false if PREDS's domain
   is not a subset of any of the sub-domains of *THIS (corresponding
   to each individual chains in it), even though it may be still be
   a subset of whole domain of *THIS which is the union (ORed) of all
   its subdomains.  In other words, the result is conservative.  */

bool
predicate::includes (const pred_chain &chain) const
{
  for (unsigned i = 0; i < m_preds.length (); i++)
    if (subset_of (chain, m_preds[i]))
      return true;

  return false;
}

/* Return true if the domain defined by *THIS is a superset of PREDS's
   domain.
   Avoid building generic trees (and rely on the folding capability
   of the compiler), and instead perform brute force comparison of
   individual predicate chains (this won't be a computationally costly
   since the chains are pretty short).  Returning false does not
   necessarily mean *THIS is not a superset of *PREDS, only that
   it need not be since the analysis cannot prove it.  */

bool
predicate::superset_of (const predicate &preds) const
{
  for (unsigned i = 0; i < preds.m_preds.length (); i++)
    if (!includes (preds.m_preds[i]))
      return false;

  return true;
}

/* Create a predicate of the form OP != 0 and push it the work list CHAIN.  */

static void
push_to_worklist (tree op, pred_chain *chain, hash_set<tree> *mark_set)
{
  if (mark_set->contains (op))
    return;
  mark_set->add (op);

  pred_info arg_pred;
  arg_pred.pred_lhs = op;
  arg_pred.pred_rhs = integer_zero_node;
  arg_pred.cond_code = NE_EXPR;
  arg_pred.invert = false;
  chain->safe_push (arg_pred);
}

/* Return a pred_info for a gimple assignment CMP_ASSIGN with comparison
   rhs.  */

static pred_info
get_pred_info_from_cmp (const gimple *cmp_assign)
{
  pred_info pred;
  pred.pred_lhs = gimple_assign_rhs1 (cmp_assign);
  pred.pred_rhs = gimple_assign_rhs2 (cmp_assign);
  pred.cond_code = gimple_assign_rhs_code (cmp_assign);
  pred.invert = false;
  return pred;
}

/* If PHI is a degenerate phi with all operands having the same value (relop)
   update *PRED to that value and return true.  Otherwise return false.  */

static bool
is_degenerate_phi (gimple *phi, pred_info *pred)
{
  tree op0 = gimple_phi_arg_def (phi, 0);

  if (TREE_CODE (op0) != SSA_NAME)
    return false;

  gimple *def0 = SSA_NAME_DEF_STMT (op0);
  if (gimple_code (def0) != GIMPLE_ASSIGN)
    return false;

  if (TREE_CODE_CLASS (gimple_assign_rhs_code (def0)) != tcc_comparison)
    return false;

  pred_info pred0 = get_pred_info_from_cmp (def0);

  unsigned n = gimple_phi_num_args (phi);
  for (unsigned i = 1; i < n; ++i)
    {
      tree op = gimple_phi_arg_def (phi, i);
      if (TREE_CODE (op) != SSA_NAME)
	return false;

      gimple *def = SSA_NAME_DEF_STMT (op);
      if (gimple_code (def) != GIMPLE_ASSIGN)
	return false;

      if (TREE_CODE_CLASS (gimple_assign_rhs_code (def)) != tcc_comparison)
	return false;

      pred_info pred = get_pred_info_from_cmp (def);
      if (!pred_equal_p (pred, pred0))
	return false;
    }

  *pred = pred0;
  return true;
}

/* Recursively compute the control dependence chains (paths of edges)
   from the dependent basic block, DEP_BB, up to the dominating basic
   block, DOM_BB (the head node of a chain should be dominated by it),
   storing them in the CD_CHAINS array.
   CUR_CD_CHAIN is the current chain being computed.
   *NUM_CHAINS is total number of chains in the CD_CHAINS array.
   *NUM_CALLS is the number of recursive calls to control unbounded
   recursion.
   Return true if the information is successfully computed, false if
   there is no control dependence or not computed.  */

static bool
compute_control_dep_chain (basic_block dom_bb, const_basic_block dep_bb,
			   vec<edge> cd_chains[], unsigned *num_chains,
			   vec<edge> &cur_cd_chain, unsigned *num_calls,
			   unsigned depth = 0)
{
  if (*num_calls > (unsigned)param_uninit_control_dep_attempts)
    {
      if (dump_file)
	fprintf (dump_file, "param_uninit_control_dep_attempts exceeded: %u\n",
		 *num_calls);
      return false;
    }
  ++*num_calls;

  /* FIXME: Use a set instead.  */
  unsigned cur_chain_len = cur_cd_chain.length ();
  if (cur_chain_len > MAX_CHAIN_LEN)
    {
      if (dump_file)
	fprintf (dump_file, "MAX_CHAIN_LEN exceeded: %u\n", cur_chain_len);

      return false;
    }

  if (cur_chain_len > 5)
    {
      if (dump_file)
	fprintf (dump_file, "chain length exceeds 5: %u\n", cur_chain_len);
    }

  for (unsigned i = 0; i < cur_chain_len; i++)
    {
      edge e = cur_cd_chain[i];
      /* Cycle detected.  */
      if (e->src == dom_bb)
	{
	  if (dump_file)
	    fprintf (dump_file, "cycle detected\n");
	  return false;
	}
    }

  if (DEBUG_PREDICATE_ANALYZER && dump_file)
    fprintf (dump_file,
	     "%*s%s (dom_bb = %u, dep_bb = %u, cd_chains = { %s }, ...)\n",
	     depth, "", __func__, dom_bb->index, dep_bb->index,
	     format_edge_vecs (cd_chains, *num_chains).c_str ());

  bool found_cd_chain = false;

  /* Iterate over DOM_BB's successors.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, dom_bb->succs)
    {
      int post_dom_check = 0;
      if (e->flags & (EDGE_FAKE | EDGE_ABNORMAL))
	continue;

      basic_block cd_bb = e->dest;
      cur_cd_chain.safe_push (e);
      while (!is_non_loop_exit_postdominating (cd_bb, dom_bb))
	{
	  if (cd_bb == dep_bb)
	    {
	      /* Found a direct control dependence.  */
	      if (*num_chains < MAX_NUM_CHAINS)
		{
		  cd_chains[*num_chains] = cur_cd_chain.copy ();
		  (*num_chains)++;
		}
	      found_cd_chain = true;
	      /* Check path from next edge.  */
	      break;
	    }

	  /* Check if DEP_BB is indirectly control-dependent on DOM_BB.  */
	  if (compute_control_dep_chain (cd_bb, dep_bb, cd_chains,
					 num_chains, cur_cd_chain,
					 num_calls, depth + 1))
	    {
	      found_cd_chain = true;
	      break;
	    }

	  cd_bb = find_pdom (cd_bb);
	  post_dom_check++;
	  if (cd_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	      || post_dom_check > MAX_POSTDOM_CHECK)
	    break;
	}
      cur_cd_chain.pop ();
      gcc_assert (cur_cd_chain.length () == cur_chain_len);
    }

  gcc_assert (cur_cd_chain.length () == cur_chain_len);
  return found_cd_chain;
}

/* Return true if PRED can be invalidated by any predicate in GUARD.  */

static bool
can_be_invalidated_p (const pred_info &pred, const pred_chain &guard)
{
  if (dump_file && dump_flags & TDF_DETAILS)
    {
      fprintf (dump_file, "Testing if predicate: ");
      dump_pred_info (pred);
      fprintf (dump_file, "\n...can be invalidated by a USE guard of: ");
      dump_pred_chain (guard);
      fputc ('\n', dump_file);
    }

  unsigned n = guard.length ();
  for (unsigned i = 0; i < n; ++i)
    {
      if (pred_neg_p (pred, guard[i]))
	{
	  if (dump_file && dump_flags & TDF_DETAILS)
	    {
	      fprintf (dump_file, "  Predicate invalidated by: ");
	      dump_pred_info (guard[i]);
	      fputc ('\n', dump_file);
	    }
	  return true;
	}
    }

  return false;
}

/* Return true if all predicates in PREDS are invalidated by GUARD being
   true.  */

static bool
can_be_invalidated_p (const pred_chain_union &preds, const pred_chain &guard)
{
  if (preds.is_empty ())
    return false;

  if (dump_file && dump_flags & TDF_DETAILS)
    dump_predicates (NULL, preds,
		     "Testing if anything here can be invalidated: ");

  for (unsigned i = 0; i < preds.length (); ++i)
    {
      const pred_chain &chain = preds[i];
      for (unsigned j = 0; j < chain.length (); ++j)
	if (can_be_invalidated_p (chain[j], guard))
	  return true;

      /* If we were unable to invalidate any predicate in C, then there
	 is a viable path from entry to the PHI where the PHI takes
	 an interesting value and continues to a use of the PHI.  */
      return false;
    }
  return true;
}

/* Return true if none of the PHI arguments in OPNDS is used given
   the use guards in *THIS that guard the PHI's use.  */

bool
predicate::use_cannot_happen (gphi *phi, unsigned opnds)
{
  if (!m_eval.phi_arg_set (phi))
    return false;

  /* PHI_USE_GUARDS are OR'ed together.  If we have more than one
     possible guard, there's no way of knowing which guard was true.
     Since we need to be absolutely sure that the uninitialized
     operands will be invalidated, bail.  */
  const pred_chain_union &phi_use_guards = m_preds;
  if (phi_use_guards.length () != 1)
    return false;

  const pred_chain &use_guard = phi_use_guards[0];

  /* Look for the control dependencies of all the interesting operands
     and build guard predicates describing them.  */
  unsigned n = gimple_phi_num_args (phi);
  for (unsigned i = 0; i < n; ++i)
    {
      if (!MASK_TEST_BIT (opnds, i))
	continue;

      edge e = gimple_phi_arg_edge (phi, i);
      auto_vec<edge> dep_chains[MAX_NUM_CHAINS];
      auto_vec<edge, MAX_CHAIN_LEN + 1> cur_chain;
      unsigned num_chains = 0;
      unsigned num_calls = 0;

      /* Build the control dependency chain for the PHI argument...  */
      if (!compute_control_dep_chain (ENTRY_BLOCK_PTR_FOR_FN (cfun),
				      e->src, dep_chains, &num_chains,
				      cur_chain, &num_calls))
	return false;

      if (DEBUG_PREDICATE_ANALYZER && dump_file)
	{
	  fprintf (dump_file, "predicate::use_cannot_happen (...) "
		   "dep_chains for arg %u:\n\t", i);
	  dump_dep_chains (dep_chains, num_chains);
	}

      /* ...and convert it into a set of predicates guarding its
	 definition.  */
      predicate def_preds (m_eval);
      def_preds.init_from_control_deps (dep_chains, num_chains);
      if (def_preds.is_empty ())
	/* If there's no predicate there's no basis to rule the use out.  */
	return false;

      def_preds.simplify ();
      def_preds.normalize ();

      /* Can the guard for this PHI argument be negated by the one
	 guarding the PHI use?  */
      if (!can_be_invalidated_p (def_preds.chain (), use_guard))
	return false;
    }

  return true;
}

/* Implemented simplifications:

   1) ((x IOR y) != 0) AND (x != 0) is equivalent to (x != 0);
   2) (X AND Y) OR (!X AND Y) is equivalent to Y;
   3) X OR (!X AND Y) is equivalent to (X OR Y);
   4) ((x IAND y) != 0) || (x != 0 AND y != 0)) is equivalent to
      (x != 0 AND y != 0)
   5) (X AND Y) OR (!X AND Z) OR (!Y AND Z) is equivalent to
      (X AND Y) OR Z

   PREDS is the predicate chains, and N is the number of chains.  */

/* Implement rule 1 above.  PREDS is the AND predicate to simplify
   in place.  */

static void
simplify_1 (pred_chain &chain)
{
  bool simplified = false;
  pred_chain s_chain = vNULL;

  unsigned n = chain.length ();
  for (unsigned i = 0; i < n; i++)
    {
      pred_info &a_pred = chain[i];

      if (!a_pred.pred_lhs
	  || !is_neq_zero_form_p (a_pred))
	continue;

      gimple *def_stmt = SSA_NAME_DEF_STMT (a_pred.pred_lhs);
      if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
	continue;

      if (gimple_assign_rhs_code (def_stmt) != BIT_IOR_EXPR)
	continue;

      for (unsigned j = 0; j < n; j++)
	{
	  const pred_info &b_pred = chain[j];

	  if (!b_pred.pred_lhs
	      || !is_neq_zero_form_p (b_pred))
	    continue;

	  if (pred_expr_equal_p (b_pred, gimple_assign_rhs1 (def_stmt))
	      || pred_expr_equal_p (b_pred, gimple_assign_rhs2 (def_stmt)))
	    {
	      /* Mark A_PRED for removal from PREDS.  */
	      a_pred.pred_lhs = NULL;
	      a_pred.pred_rhs = NULL;
	      simplified = true;
	      break;
	    }
	}
    }

  if (!simplified)
    return;

  /* Remove predicates marked above.  */
  for (unsigned i = 0; i < n; i++)
    {
      pred_info &a_pred = chain[i];
      if (!a_pred.pred_lhs)
	continue;
      s_chain.safe_push (a_pred);
    }

  chain.release ();
  chain = s_chain;
}

/* Implements rule 2 for the OR predicate PREDS:

   2) (X AND Y) OR (!X AND Y) is equivalent to Y.  */

bool
predicate::simplify_2 ()
{
  bool simplified = false;

  /* (X AND Y) OR (!X AND Y) is equivalent to Y.
     (X AND Y) OR (X AND !Y) is equivalent to X.  */

  unsigned n = m_preds.length ();
  for (unsigned i = 0; i < n; i++)
    {
      pred_chain &a_chain = m_preds[i];
      if (a_chain.length () != 2)
	continue;

      /* Create copies since the chain may be released below before
	 the copy is added to the other chain.  */
      const pred_info x = a_chain[0];
      const pred_info y = a_chain[1];

      for (unsigned j = 0; j < n; j++)
	{
	  if (j == i)
	    continue;

	  pred_chain &b_chain = m_preds[j];
	  if (b_chain.length () != 2)
	    continue;

	  const pred_info &x2 = b_chain[0];
	  const pred_info &y2 = b_chain[1];

	  if (pred_equal_p (x, x2) && pred_neg_p (y, y2))
	    {
	      /* Kill a_chain.  */
	      b_chain.release ();
	      a_chain.release ();
	      b_chain.safe_push (x);
	      simplified = true;
	      break;
	    }
	  if (pred_neg_p (x, x2) && pred_equal_p (y, y2))
	    {
	      /* Kill a_chain.  */
	      a_chain.release ();
	      b_chain.release ();
	      b_chain.safe_push (y);
	      simplified = true;
	      break;
	    }
	}
    }
  /* Now clean up the chain.  */
  if (simplified)
    {
      pred_chain_union s_preds = vNULL;
      for (unsigned i = 0; i < n; i++)
	{
	  if (m_preds[i].is_empty ())
	    continue;
	  s_preds.safe_push (m_preds[i]);
	}
      m_preds.release ();
      m_preds = s_preds;
      s_preds = vNULL;
    }

  return simplified;
}

/* Implement rule 3 for the OR predicate PREDS:

   3) x OR (!x AND y) is equivalent to x OR y.  */

bool
predicate::simplify_3 ()
{
  /* Now iteratively simplify X OR (!X AND Z ..)
       into X OR (Z ...).  */

  unsigned n = m_preds.length ();
  if (n < 2)
    return false;

  bool simplified = false;
  for (unsigned i = 0; i < n; i++)
    {
      const pred_chain &a_chain = m_preds[i];

      if (a_chain.length () != 1)
	continue;

      const pred_info &x = a_chain[0];
      for (unsigned j = 0; j < n; j++)
	{
	  if (j == i)
	    continue;

	  pred_chain b_chain = m_preds[j];
	  if (b_chain.length () < 2)
	    continue;

	  for (unsigned k = 0; k < b_chain.length (); k++)
	    {
	      const pred_info &x2 = b_chain[k];
	      if (pred_neg_p (x, x2))
		{
		  b_chain.unordered_remove (k);
		  simplified = true;
		  break;
		}
	    }
	}
    }
  return simplified;
}

/* Implement rule 4 for the OR predicate PREDS:

   2) ((x AND y) != 0) OR (x != 0 AND y != 0) is equivalent to
       (x != 0 ANd y != 0).   */

bool
predicate::simplify_4 ()
{
  bool simplified = false;
  pred_chain_union s_preds = vNULL;

  unsigned n = m_preds.length ();
  for (unsigned i = 0; i < n; i++)
    {
      pred_chain a_chain = m_preds[i];
      if (a_chain.length () != 1)
	continue;

      const pred_info &z = a_chain[0];
      if (!is_neq_zero_form_p (z))
	continue;

      gimple *def_stmt = SSA_NAME_DEF_STMT (z.pred_lhs);
      if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
	continue;

      if (gimple_assign_rhs_code (def_stmt) != BIT_AND_EXPR)
	continue;

      for (unsigned j = 0; j < n; j++)
	{
	  if (j == i)
	    continue;

	  pred_chain b_chain = m_preds[j];
	  if (b_chain.length () != 2)
	    continue;

	  const pred_info &x2 = b_chain[0];
	  const pred_info &y2 = b_chain[1];
	  if (!is_neq_zero_form_p (x2) || !is_neq_zero_form_p (y2))
	    continue;

	  if ((pred_expr_equal_p (x2, gimple_assign_rhs1 (def_stmt))
	       && pred_expr_equal_p (y2, gimple_assign_rhs2 (def_stmt)))
	      || (pred_expr_equal_p (x2, gimple_assign_rhs2 (def_stmt))
		  && pred_expr_equal_p (y2, gimple_assign_rhs1 (def_stmt))))
	    {
	      /* Kill a_chain.  */
	      a_chain.release ();
	      simplified = true;
	      break;
	    }
	}
    }
  /* Now clean up the chain.  */
  if (simplified)
    {
      for (unsigned i = 0; i < n; i++)
	{
	  if (m_preds[i].is_empty ())
	    continue;
	  s_preds.safe_push (m_preds[i]);
	}

      m_preds.release ();
      m_preds = s_preds;
      s_preds = vNULL;
    }

  return simplified;
}

/* Simplify predicates in *THIS.  */

void
predicate::simplify (gimple *use_or_def, bool is_use)
{
  if (dump_file && dump_flags & TDF_DETAILS)
    {
      fprintf (dump_file, "Before simplication ");
      dump (use_or_def, is_use ? "[USE]:\n" : "[DEF]:\n");
    }

  unsigned n = m_preds.length ();
  for (unsigned i = 0; i < n; i++)
    ::simplify_1 (m_preds[i]);

  if (n < 2)
    return;

  bool changed;
  do
    {
      changed = false;
      if (simplify_2 ())
	changed = true;

      if (simplify_3 ())
	changed = true;

      if (simplify_4 ())
	changed = true;
    }
  while (changed);
}

/* Attempt to normalize predicate chains by following UD chains by
   building up a big tree of either IOR operations or AND operations,
   and converting the IOR tree into a pred_chain_union or the BIT_AND
   tree into a pred_chain.
   Example:

  _3 = _2 RELOP1 _1;
  _6 = _5 RELOP2 _4;
  _9 = _8 RELOP3 _7;
  _10 = _3 | _6;
  _12 = _9 | _0;
  _t = _10 | _12;

  then _t != 0 will be normalized into a pred_chain_union

   (_2 RELOP1 _1) OR (_5 RELOP2 _4) OR (_8 RELOP3 _7) OR (_0 != 0)

   Similarly given:

  _3 = _2 RELOP1 _1;
  _6 = _5 RELOP2 _4;
  _9 = _8 RELOP3 _7;
  _10 = _3 & _6;
  _12 = _9 & _0;

  then _t != 0 will be normalized into a pred_chain:
  (_2 RELOP1 _1) AND (_5 RELOP2 _4) AND (_8 RELOP3 _7) AND (_0 != 0)
  */

/* Store a PRED in *THIS.  */

void
predicate::push_pred (const pred_info &pred)
{
  pred_chain chain = vNULL;
  chain.safe_push (pred);
  m_preds.safe_push (chain);
}

/* Dump predicates in *THIS for STMT prepended by MSG.  */

void
predicate::dump (gimple *stmt, const char *msg) const
{
  fprintf (dump_file, "%s", msg);
  if (stmt)
    {
      fputc ('\t', dump_file);
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "  is conditional on:\n");
    }

  unsigned np = m_preds.length ();
  if (np == 0)
    {
      fprintf (dump_file, "\t(empty)\n");
      return;
    }

  {
    tree expr = build_pred_expr (m_preds);
    char *str = print_generic_expr_to_str (expr);
    fprintf (dump_file, "\t%s (expanded)\n", str);
    free (str);
  }

  if (np > 1)
    fprintf (dump_file, "\tOR (");
  else
    fputc ('\t', dump_file);
  for (unsigned i = 0; i < np; i++)
    {
      dump_pred_chain (m_preds[i]);
      if (i < np - 1)
	fprintf (dump_file, ", ");
      else if (i > 0)
	fputc (')', dump_file);
    }
  fputc ('\n', dump_file);
}

/* Initialize *THIS with the predicates of the control dependence chains
   between the basic block DEF_BB that defines a variable of interst and
   USE_BB that uses the variable, respectively.  */

predicate::predicate (basic_block def_bb, basic_block use_bb, func_t &eval)
  : m_preds (vNULL), m_eval (eval)
{
  /* Set CD_ROOT to the basic block closest to USE_BB that is the control
     equivalent of (is guarded by the same predicate as) DEF_BB that also
     dominates USE_BB.  */
  basic_block cd_root = def_bb;
  while (dominated_by_p (CDI_DOMINATORS, use_bb, cd_root))
    {
      /* Find CD_ROOT's closest postdominator that's its control
	 equivalent.  */
      if (basic_block bb = find_control_equiv_block (cd_root))
	if (dominated_by_p (CDI_DOMINATORS, use_bb, bb))
	  {
	    cd_root = bb;
	    continue;
	  }

      break;
    }

  /* Set DEP_CHAINS to the set of edges between CD_ROOT and USE_BB.
     Each DEP_CHAINS element is a series of edges whose conditions
     are logical conjunctions.  Together, the DEP_CHAINS vector is
     used below to initialize an OR expression of the conjunctions.  */
  unsigned num_calls = 0;
  unsigned num_chains = 0;
  auto_vec<edge> dep_chains[MAX_NUM_CHAINS];
  auto_vec<edge, MAX_CHAIN_LEN + 1> cur_chain;

  compute_control_dep_chain (cd_root, use_bb, dep_chains, &num_chains,
			     cur_chain, &num_calls);

  if (DEBUG_PREDICATE_ANALYZER && dump_file)
    {
      fprintf (dump_file, "predicate::predicate (def_bb = %u, use_bb = %u, func_t) "
	       "initialized from %u dep_chains:\n\t",
	       def_bb->index, use_bb->index, num_chains);
      dump_dep_chains (dep_chains, num_chains);
    }

  /* From the set of edges computed above initialize *THIS as the OR
     condition under which the definition in DEF_BB is used in USE_BB.
     Each OR subexpression is represented by one element of DEP_CHAINS,
     where each element consists of a series of AND subexpressions.  */
  init_from_control_deps (dep_chains, num_chains);
}

/* Release resources in *THIS.  */

predicate::~predicate ()
{
  unsigned n = m_preds.length ();
  for (unsigned i = 0; i != n; ++i)
    m_preds[i].release ();
  m_preds.release ();
}

/* Copy-assign RHS to *THIS.  */

predicate&
predicate::operator= (const predicate &rhs)
{
  if (this == &rhs)
    return *this;

  /* FIXME: Make this a compile-time constraint?  */
  gcc_assert (&m_eval == &rhs.m_eval);

  unsigned n = m_preds.length ();
  for (unsigned i = 0; i != n; ++i)
    m_preds[i].release ();
  m_preds.release ();

  n = rhs.m_preds.length ();
  for (unsigned i = 0; i != n; ++i)
    {
      const pred_chain &chain = rhs.m_preds[i];
      m_preds.safe_push (chain.copy ());
    }

  return *this;
}

/* For each use edge of PHI, compute all control dependence chains
   and convert those to the composite predicates in M_PREDS.
   Return true if a nonempty predicate has been obtained.  */

bool
predicate::init_from_phi_def (gphi *phi)
{
  gcc_assert (is_empty ());

  basic_block phi_bb = gimple_bb (phi);
  /* Find the closest dominating bb to be the control dependence root.  */
  basic_block cd_root = find_dom (phi_bb);
  if (!cd_root)
    return false;

  /* Set DEF_EDGES to the edges to the PHI from the bb's that provide
     definitions of each of the PHI operands for which M_EVAL is false.  */
  auto_vec<edge> def_edges;
  hash_set<gimple *> visited_phis;
  collect_phi_def_edges (phi, cd_root, &def_edges, m_eval, &visited_phis);

  unsigned nedges = def_edges.length ();
  if (nedges == 0)
    return false;

  unsigned num_chains = 0;
  auto_vec<edge> dep_chains[MAX_NUM_CHAINS];
  auto_vec<edge, MAX_CHAIN_LEN + 1> cur_chain;
  for (unsigned i = 0; i < nedges; i++)
    {
      edge e = def_edges[i];
      unsigned num_calls = 0;
      unsigned prev_nc = num_chains;
      compute_control_dep_chain (cd_root, e->src, dep_chains,
				 &num_chains, cur_chain, &num_calls);

      /* Update the newly added chains with the phi operand edge.  */
      if (EDGE_COUNT (e->src->succs) > 1)
	{
	  if (prev_nc == num_chains && num_chains < MAX_NUM_CHAINS)
	    dep_chains[num_chains++] = vNULL;
	  for (unsigned j = prev_nc; j < num_chains; j++)
	    dep_chains[j].safe_push (e);
	}
    }

  /* Convert control dependence chains to the predicate in *THIS under
     which the PHI operands are defined to values for which M_EVAL is
     false.  */
  init_from_control_deps (dep_chains, num_chains);
  return !is_empty ();
}

/* Compute the predicates that guard the use USE_STMT and check if
   the incoming paths that have an empty (or possibly empty) definition
   can be pruned.  Return true if it can be determined that the use of
   PHI's def in USE_STMT is guarded by a predicate set that does not
   overlap with the predicate sets of all runtime paths that do not
   have a definition.

   Return false if the use is not guarded or if it cannot be determined.
   USE_BB is the bb of the use (for phi operand use, the bb is not the bb
   of the phi stmt, but the source bb of the operand edge).

   OPNDS is a bitmap with a bit set for each PHI operand of interest.

   THIS->M_PREDS contains the (memoized) defining predicate chains of
   a PHI.  If THIS->M_PREDS is empty, the PHI's defining predicate
   chains are computed and stored into THIS->M_PREDS as needed.

   VISITED_PHIS is a pointer set of phis being visited.  */

bool
predicate::is_use_guarded (gimple *use_stmt, basic_block use_bb,
			   gphi *phi, unsigned opnds,
			   hash_set<gphi *> *visited)
{
  if (visited->add (phi))
    return false;

  /* The basic block where the PHI is defined.  */
  basic_block def_bb = gimple_bb (phi);

  /* Try to build the predicate expression under which the PHI flows
     into its use.  This will be empty if the PHI is defined and used
     in the same bb.  */
  predicate use_preds (def_bb, use_bb, m_eval);

  if (is_non_loop_exit_postdominating (use_bb, def_bb))
    {
      if (is_empty ())
	{
	  /* Lazily initialize *THIS from the PHI and build its use
	     expression.  */
	  init_from_phi_def (phi);
	  m_use_expr = build_pred_expr (use_preds.m_preds);
	}

      /* The use is not guarded.  */
      return false;
    }

  if (use_preds.is_empty ())
    return false;

  /* Try to prune the dead incoming phi edges.  */
  if (!use_preds.overlap (phi, opnds, visited))
    {
      if (DEBUG_PREDICATE_ANALYZER && dump_file)
	fputs ("found predicate overlap\n", dump_file);

      return true;
    }

  /* We might be able to prove that if the control dependencies for OPNDS
     are true, the control dependencies for USE_STMT can never be true.  */
  if (use_preds.use_cannot_happen (phi, opnds))
    return true;

  if (is_empty ())
    {
      /* Lazily initialize *THIS from PHI.  */
      if (!init_from_phi_def (phi))
	{
	  m_use_expr = build_pred_expr (use_preds.m_preds);
	  return false;
	}

      simplify (phi);
      normalize (phi);
    }

  use_preds.simplify (use_stmt, /*is_use=*/true);
  use_preds.normalize (use_stmt, /*is_use=*/true);

  /* Return true if the predicate guarding the valid definition (i.e.,
     *THIS) is a superset of the predicate guarding the use (i.e.,
     USE_PREDS).  */
  if (superset_of (use_preds))
    return true;

  m_use_expr = build_pred_expr (use_preds.m_preds);

  return false;
}

/* Public interface to the above. */

bool
predicate::is_use_guarded (gimple *stmt, basic_block use_bb, gphi *phi,
			   unsigned opnds)
{
  hash_set<gphi *> visited;
  return is_use_guarded (stmt, use_bb, phi, opnds, &visited);
}

/* Normalize predicate PRED:
   1) if PRED can no longer be normalized, append it to *THIS.
   2) otherwise if PRED is of the form x != 0, follow x's definition
      and put normalized predicates into WORK_LIST.  */

void
predicate::normalize (pred_chain *norm_chain,
		      pred_info pred,
		      tree_code and_or_code,
		      pred_chain *work_list,
		      hash_set<tree> *mark_set)
{
  if (!is_neq_zero_form_p (pred))
    {
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (pred);
      else
	norm_chain->safe_push (pred);
      return;
    }

  gimple *def_stmt = SSA_NAME_DEF_STMT (pred.pred_lhs);

  if (gimple_code (def_stmt) == GIMPLE_PHI
      && is_degenerate_phi (def_stmt, &pred))
    /* PRED has been modified above.  */
    work_list->safe_push (pred);
  else if (gimple_code (def_stmt) == GIMPLE_PHI && and_or_code == BIT_IOR_EXPR)
    {
      unsigned n = gimple_phi_num_args (def_stmt);

      /* Punt for a nonzero constant.  The predicate should be one guarding
	 the phi edge.  */
      for (unsigned i = 0; i < n; ++i)
	{
	  tree op = gimple_phi_arg_def (def_stmt, i);
	  if (TREE_CODE (op) == INTEGER_CST && !integer_zerop (op))
	    {
	      push_pred (pred);
	      return;
	    }
	}

      for (unsigned i = 0; i < n; ++i)
	{
	  tree op = gimple_phi_arg_def (def_stmt, i);
	  if (integer_zerop (op))
	    continue;

	  push_to_worklist (op, work_list, mark_set);
	}
    }
  else if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
    {
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (pred);
      else
	norm_chain->safe_push (pred);
    }
  else if (gimple_assign_rhs_code (def_stmt) == and_or_code)
    {
      /* Avoid splitting up bit manipulations like x & 3 or y | 1.  */
      if (is_gimple_min_invariant (gimple_assign_rhs2 (def_stmt)))
	{
	  /* But treat x & 3 as a condition.  */
	  if (and_or_code == BIT_AND_EXPR)
	    {
	      pred_info n_pred;
	      n_pred.pred_lhs = gimple_assign_rhs1 (def_stmt);
	      n_pred.pred_rhs = gimple_assign_rhs2 (def_stmt);
	      n_pred.cond_code = and_or_code;
	      n_pred.invert = false;
	      norm_chain->safe_push (n_pred);
	    }
	}
      else
	{
	  push_to_worklist (gimple_assign_rhs1 (def_stmt), work_list, mark_set);
	  push_to_worklist (gimple_assign_rhs2 (def_stmt), work_list, mark_set);
	}
    }
  else if (TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt))
	   == tcc_comparison)
    {
      pred_info n_pred = get_pred_info_from_cmp (def_stmt);
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (n_pred);
      else
	norm_chain->safe_push (n_pred);
    }
  else
    {
      if (and_or_code == BIT_IOR_EXPR)
	push_pred (pred);
      else
	norm_chain->safe_push (pred);
    }
}

/* Normalize PRED and store the normalized predicates in THIS->M_PREDS.  */

void
predicate::normalize (const pred_info &pred)
{
  if (!is_neq_zero_form_p (pred))
    {
      push_pred (pred);
      return;
    }

  tree_code and_or_code = ERROR_MARK;

  gimple *def_stmt = SSA_NAME_DEF_STMT (pred.pred_lhs);
  if (gimple_code (def_stmt) == GIMPLE_ASSIGN)
    and_or_code = gimple_assign_rhs_code (def_stmt);
  if (and_or_code != BIT_IOR_EXPR && and_or_code != BIT_AND_EXPR)
    {
      if (TREE_CODE_CLASS (and_or_code) == tcc_comparison)
	{
	  pred_info n_pred = get_pred_info_from_cmp (def_stmt);
	  push_pred (n_pred);
	}
      else
	push_pred (pred);
      return;
    }


  pred_chain norm_chain = vNULL;
  pred_chain work_list = vNULL;
  work_list.safe_push (pred);
  hash_set<tree> mark_set;

  while (!work_list.is_empty ())
    {
      pred_info a_pred = work_list.pop ();
      normalize (&norm_chain, a_pred, and_or_code, &work_list, &mark_set);
    }

  if (and_or_code == BIT_AND_EXPR)
    m_preds.safe_push (norm_chain);

  work_list.release ();
}

/* Normalize a single predicate PRED_CHAIN and append it to *THIS.  */

void
predicate::normalize (const pred_chain &chain)
{
  pred_chain work_list = vNULL;
  hash_set<tree> mark_set;
  for (unsigned i = 0; i < chain.length (); i++)
    {
      work_list.safe_push (chain[i]);
      mark_set.add (chain[i].pred_lhs);
    }

  /* Normalized chain of predicates built up below.  */
  pred_chain norm_chain = vNULL;
  while (!work_list.is_empty ())
    {
      pred_info pi = work_list.pop ();
      predicate pred (m_eval);
      /* The predicate object is not modified here, only NORM_CHAIN and
	 WORK_LIST are appended to.  */
      pred.normalize (&norm_chain, pi, BIT_AND_EXPR, &work_list, &mark_set);
    }

  m_preds.safe_push (norm_chain);
  work_list.release ();
}

/* Normalize predicate chains in THIS.  */

void
predicate::normalize (gimple *use_or_def, bool is_use)
{
  if (dump_file && dump_flags & TDF_DETAILS)
    {
      fprintf (dump_file, "Before normalization ");
      dump (use_or_def, is_use ? "[USE]:\n" : "[DEF]:\n");
    }

  predicate norm_preds (m_eval);
  for (unsigned i = 0; i < m_preds.length (); i++)
    {
      if (m_preds[i].length () != 1)
	norm_preds.normalize (m_preds[i]);
      else
	norm_preds.normalize (m_preds[i][0]);
    }

  *this = norm_preds;

  if (dump_file)
    {
      fprintf (dump_file, "After normalization ");
      dump (use_or_def, is_use ? "[USE]:\n" : "[DEF]:\n");
    }
}

/* Add a predicate for the condition or logical assignment STMT to CHAIN.
   Expand SSA_NAME into constituent subexpressions.  Invert the result
   if INVERT is true.  Return true if the predicate has been added.  */

static bool
add_pred (pred_chain *chain, gimple *stmt, bool invert)
{
  if (gimple_code (stmt) == GIMPLE_COND)
    {
      tree lhs = gimple_cond_lhs (stmt);
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  gimple *def = SSA_NAME_DEF_STMT (lhs);
	  if (is_gimple_assign (def)
	      && add_pred (chain, def, invert))
	    return true;
	}

      pred_info pred;
      pred.pred_lhs = lhs;
      pred.pred_rhs = gimple_cond_rhs (stmt);
      pred.cond_code = gimple_cond_code (stmt);
      pred.invert = invert;
      chain->safe_push (pred);
      return true;
    }

  if (!is_gimple_assign (stmt))
    return false;

  if (gimple_assign_single_p (stmt))
    // FIXME: handle this?
    return false;

  if (TREE_TYPE (gimple_assign_lhs (stmt)) != boolean_type_node)
    return false;

  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree_code code = gimple_assign_rhs_code (stmt);
  if (code == BIT_AND_EXPR)
    {
      if (TREE_CODE (rhs1) == SSA_NAME
	  && add_pred (chain, SSA_NAME_DEF_STMT (rhs1), invert)
	  && TREE_CODE (rhs2) == SSA_NAME
	  /* FIXME: Need to handle failure below! */
	  && add_pred (chain, SSA_NAME_DEF_STMT (rhs2), invert))
	return true;
    }
  else if (TREE_CODE_CLASS (code) != tcc_comparison)
    return false;

  pred_info pred;
  pred.pred_lhs = rhs1;
  pred.pred_rhs = rhs2;
  pred.cond_code = code;
  pred.invert = invert;
  chain->safe_push (pred);
  return true;
}

/* Convert the chains of control dependence edges into a set of predicates.
   A control dependence chain is represented by a vector edges.  DEP_CHAINS
   points to an array of NUM_CHAINS dependence chains. One edge in
   a dependence chain is mapped to predicate expression represented by
   pred_info type.  One dependence chain is converted to a composite
   predicate that is the result of AND operation of pred_info mapped to
   each edge.  A composite predicate is represented by a vector of
   pred_info.  Sets M_PREDS to the resulting composite predicates.  */

void
predicate::init_from_control_deps (const vec<edge> *dep_chains,
				   unsigned num_chains)
{
  gcc_assert (is_empty ());

  bool has_valid_pred = false;
  if (num_chains == 0)
    return;

  if (num_chains >= MAX_NUM_CHAINS)
    {
      if (dump_file)
	fprintf (dump_file, "MAX_NUM_CHAINS exceeded: %u\n", num_chains);
      return;
    }

  /* Convert the control dependency chain into a set of predicates.  */
  m_preds.reserve (num_chains);

  for (unsigned i = 0; i < num_chains; i++)
    {
      /* One path through the CFG represents a logical conjunction
	 of the predicates.  */
      const vec<edge> &path = dep_chains[i];

      has_valid_pred = false;
      /* The chain of predicates guarding the definition along this path.  */
      pred_chain t_chain{ };
      for (unsigned j = 0; j < path.length (); j++)
	{
	  edge e = path[j];
	  basic_block guard_bb = e->src;
	  /* Ignore empty forwarder blocks.  */
	  if (empty_block_p (guard_bb) && single_succ_p (guard_bb))
	    continue;

	  /* An empty basic block here is likely a PHI, and is not one
	     of the cases we handle below.  */
	  gimple_stmt_iterator gsi = gsi_last_bb (guard_bb);
	  if (gsi_end_p (gsi))
	    {
	      has_valid_pred = false;
	      break;
	    }
	  /* Get the conditional controlling the bb exit edge.  */
	  gimple *cond_stmt = gsi_stmt (gsi);
	  if (is_gimple_call (cond_stmt) && EDGE_COUNT (e->src->succs) >= 2)
	    /* Ignore EH edge.  Can add assertion on the other edge's flag.  */
	    continue;
	  /* Skip if there is essentially one succesor.  */
	  if (EDGE_COUNT (e->src->succs) == 2)
	    {
	      edge e1;
	      edge_iterator ei1;
	      bool skip = false;

	      FOR_EACH_EDGE (e1, ei1, e->src->succs)
		{
		  if (EDGE_COUNT (e1->dest->succs) == 0)
		    {
		      skip = true;
		      break;
		    }
		}
	      if (skip)
		continue;
	    }
	  if (gimple_code (cond_stmt) == GIMPLE_COND)
	    {
	      /* The true edge corresponds to the uninteresting condition.
		 Add the negated predicate(s) for the edge to record
		 the interesting condition.  */
	      pred_info one_pred;
	      one_pred.pred_lhs = gimple_cond_lhs (cond_stmt);
	      one_pred.pred_rhs = gimple_cond_rhs (cond_stmt);
	      one_pred.cond_code = gimple_cond_code (cond_stmt);
	      one_pred.invert = !!(e->flags & EDGE_FALSE_VALUE);

	      t_chain.safe_push (one_pred);

	      if (DEBUG_PREDICATE_ANALYZER && dump_file)
		{
		  fprintf (dump_file, "one_pred = ");
		  dump_pred_info (one_pred);
		  fputc ('\n', dump_file);
		}

	      has_valid_pred = true;
	    }
	  else if (gswitch *gs = dyn_cast<gswitch *> (cond_stmt))
	    {
	      /* Avoid quadratic behavior.  */
	      if (gimple_switch_num_labels (gs) > MAX_SWITCH_CASES)
		{
		  has_valid_pred = false;
		  break;
		}
	      /* Find the case label.  */
	      tree l = NULL_TREE;
	      unsigned idx;
	      for (idx = 0; idx < gimple_switch_num_labels (gs); ++idx)
		{
		  tree tl = gimple_switch_label (gs, idx);
		  if (e->dest == label_to_block (cfun, CASE_LABEL (tl)))
		    {
		      if (!l)
			l = tl;
		      else
			{
			  l = NULL_TREE;
			  break;
			}
		    }
		}
	      /* If more than one label reaches this block or the case
		 label doesn't have a single value (like the default one)
		 fail.  */
	      if (!l
		  || !CASE_LOW (l)
		  || (CASE_HIGH (l)
		      && !operand_equal_p (CASE_LOW (l), CASE_HIGH (l), 0)))
		{
		  has_valid_pred = false;
		  break;
		}

	      pred_info one_pred;
	      one_pred.pred_lhs = gimple_switch_index (gs);
	      one_pred.pred_rhs = CASE_LOW (l);
	      one_pred.cond_code = EQ_EXPR;
	      one_pred.invert = false;
	      t_chain.safe_push (one_pred);
	      has_valid_pred = true;
	    }
	  else
	    {
	      /* Disabled.  See PR 90994.
		 has_valid_pred = false;  */
	      break;
	    }
	}

      if (!has_valid_pred)
	break;
      else
	m_preds.safe_push (t_chain);
    }

  if (DEBUG_PREDICATE_ANALYZER && dump_file)
    {
      fprintf (dump_file, "init_from_control_deps {%s}:\n",
	       format_edge_vecs (dep_chains, num_chains).c_str ());
      dump (NULL, "");
    }

  if (has_valid_pred)
    gcc_assert (m_preds.length () != 0);
  else
    /* Clear M_PREDS to indicate failure.  */
    m_preds.release ();
}

/* Return the predicate expression guarding the definition of
   the interesting variable.  When INVERT is set, return the logical
   NOT of the predicate.  */

tree
predicate::def_expr (bool invert /* = false */) const
{
  /* The predicate is stored in an inverted form.  */
  return build_pred_expr (m_preds, !invert);
}

/* Return the predicate expression guarding the use of the interesting
   variable or null if the use predicate hasn't been determined yet.  */

tree
predicate::use_expr () const
{
  return m_use_expr;
}

/* Return a logical AND expression with the (optionally inverted) predicate
   expression guarding the definition of the interesting variable and one
   guarding its use.  Return null if the use predicate hasn't yet been
   determined.  */

tree
predicate::expr (bool invert /* = false */) const
{
  if (!m_use_expr)
    return NULL_TREE;

  tree expr = build_pred_expr (m_preds, !invert);
  return build2 (TRUTH_AND_EXPR, boolean_type_node, expr, m_use_expr);
}
