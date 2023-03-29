/* SSA Dominator optimizations for trees
   Copyright (C) 2001-2023 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "cfganal.h"
#include "cfgloop.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "tree-inline.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "domwalk.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-threadupdate.h"
#include "tree-ssa-scopedtables.h"
#include "tree-ssa-threadedge.h"
#include "tree-ssa-dom.h"
#include "gimplify.h"
#include "tree-cfgcleanup.h"
#include "dbgcnt.h"
#include "alloc-pool.h"
#include "tree-vrp.h"
#include "vr-values.h"
#include "gimple-range.h"
#include "gimple-range-path.h"
#include "alias.h"

/* This file implements optimizations on the dominator tree.  */

/* Structure for recording edge equivalences.

   Computing and storing the edge equivalences instead of creating
   them on-demand can save significant amounts of time, particularly
   for pathological cases involving switch statements.

   These structures live for a single iteration of the dominator
   optimizer in the edge's AUX field.  At the end of an iteration we
   free each of these structures.  */
class edge_info
{
 public:
  typedef std::pair <tree, tree> equiv_pair;
  edge_info (edge);
  ~edge_info ();

  /* Record a simple LHS = RHS equivalence.  This may trigger
     calls to derive_equivalences.  */
  void record_simple_equiv (tree, tree);

  /* If traversing this edge creates simple equivalences, we store
     them as LHS/RHS pairs within this vector.  */
  vec<equiv_pair> simple_equivalences;

  /* Traversing an edge may also indicate one or more particular conditions
     are true or false.  */
  vec<cond_equivalence> cond_equivalences;

 private:
  /* Derive equivalences by walking the use-def chains.  */
  void derive_equivalences (tree, tree, int);
};

/* Track whether or not we have changed the control flow graph.  */
static bool cfg_altered;

/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;
static vec<gimple *> need_noreturn_fixup;

/* Statistics for dominator optimizations.  */
struct opt_stats_d
{
  long num_stmts;
  long num_exprs_considered;
  long num_re;
  long num_const_prop;
  long num_copy_prop;
};

static struct opt_stats_d opt_stats;

/* Local functions.  */
static void record_equality (tree, tree, class const_and_copies *);
static void record_equivalences_from_phis (basic_block);
static void record_equivalences_from_incoming_edge (basic_block,
						    class const_and_copies *,
						    class avail_exprs_stack *,
						    bitmap blocks_on_stack);
static void eliminate_redundant_computations (gimple_stmt_iterator *,
					      class const_and_copies *,
					      class avail_exprs_stack *);
static void record_equivalences_from_stmt (gimple *, int,
					   class avail_exprs_stack *);
static void dump_dominator_optimization_stats (FILE *file,
					       hash_table<expr_elt_hasher> *);
static void record_temporary_equivalences (edge, class const_and_copies *,
					   class avail_exprs_stack *, bitmap);

/* Constructor for EDGE_INFO.  An EDGE_INFO instance is always
   associated with an edge E.  */

edge_info::edge_info (edge e)
{
  /* Free the old one associated with E, if it exists and
     associate our new object with E.  */
  free_dom_edge_info (e);
  e->aux = this;

  /* And initialize the embedded vectors.  */
  simple_equivalences = vNULL;
  cond_equivalences = vNULL;
}

/* Destructor just needs to release the vectors.  */

edge_info::~edge_info (void)
{
  this->cond_equivalences.release ();
  this->simple_equivalences.release ();
}

/* NAME is known to have the value VALUE, which must be a constant.

   Walk through its use-def chain to see if there are other equivalences
   we might be able to derive.

   RECURSION_LIMIT controls how far back we recurse through the use-def
   chains.  */

void
edge_info::derive_equivalences (tree name, tree value, int recursion_limit)
{
  if (TREE_CODE (name) != SSA_NAME || TREE_CODE (value) != INTEGER_CST)
    return;

  /* This records the equivalence for the toplevel object.  Do
     this before checking the recursion limit.  */
  simple_equivalences.safe_push (equiv_pair (name, value));

  /* Limit how far up the use-def chains we are willing to walk.  */
  if (recursion_limit == 0)
    return;

  /* We can walk up the use-def chains to potentially find more
     equivalences.  */
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);
  if (is_gimple_assign (def_stmt))
    {
      enum tree_code code = gimple_assign_rhs_code (def_stmt);
      switch (code)
	{
	/* If the result of an OR is zero, then its operands are, too.  */
	case BIT_IOR_EXPR:
	  if (integer_zerop (value))
	    {
	      tree rhs1 = gimple_assign_rhs1 (def_stmt);
	      tree rhs2 = gimple_assign_rhs2 (def_stmt);

	      value = build_zero_cst (TREE_TYPE (rhs1));
	      derive_equivalences (rhs1, value, recursion_limit - 1);
	      value = build_zero_cst (TREE_TYPE (rhs2));
	      derive_equivalences (rhs2, value, recursion_limit - 1);
	    }
	  break;

	/* If the result of an AND is nonzero, then its operands are, too.  */
	case BIT_AND_EXPR:
	  if (!integer_zerop (value))
	    {
	      tree rhs1 = gimple_assign_rhs1 (def_stmt);
	      tree rhs2 = gimple_assign_rhs2 (def_stmt);

	      /* If either operand has a boolean range, then we
		 know its value must be one, otherwise we just know it
		 is nonzero.  The former is clearly useful, I haven't
		 seen cases where the latter is helpful yet.  */
	      if (TREE_CODE (rhs1) == SSA_NAME)
		{
		  if (ssa_name_has_boolean_range (rhs1))
		    {
		      value = build_one_cst (TREE_TYPE (rhs1));
		      derive_equivalences (rhs1, value, recursion_limit - 1);
		    }
		}
	      if (TREE_CODE (rhs2) == SSA_NAME)
		{
		  if (ssa_name_has_boolean_range (rhs2))
		    {
		      value = build_one_cst (TREE_TYPE (rhs2));
		      derive_equivalences (rhs2, value, recursion_limit - 1);
		    }
		}
	    }
	  break;

	/* If LHS is an SSA_NAME and RHS is a constant integer and LHS was
	   set via a widening type conversion, then we may be able to record
	   additional equivalences.  */
	CASE_CONVERT:
	  {
	    tree rhs = gimple_assign_rhs1 (def_stmt);
	    tree rhs_type = TREE_TYPE (rhs);
	    if (INTEGRAL_TYPE_P (rhs_type)
		&& (TYPE_PRECISION (TREE_TYPE (name))
		    >= TYPE_PRECISION (rhs_type))
		&& int_fits_type_p (value, rhs_type))
	      derive_equivalences (rhs,
				   fold_convert (rhs_type, value),
				   recursion_limit - 1);
	    break;
	  }

	/* We can invert the operation of these codes trivially if
	   one of the RHS operands is a constant to produce a known
	   value for the other RHS operand.  */
	case POINTER_PLUS_EXPR:
	case PLUS_EXPR:
	  {
	    tree rhs1 = gimple_assign_rhs1 (def_stmt);
	    tree rhs2 = gimple_assign_rhs2 (def_stmt);

	    /* If either argument is a constant, then we can compute
	       a constant value for the nonconstant argument.  */
	    if (TREE_CODE (rhs1) == INTEGER_CST
		&& TREE_CODE (rhs2) == SSA_NAME)
	      derive_equivalences (rhs2,
				   fold_binary (MINUS_EXPR, TREE_TYPE (rhs1),
						value, rhs1),
				   recursion_limit - 1);
	    else if (TREE_CODE (rhs2) == INTEGER_CST
		     && TREE_CODE (rhs1) == SSA_NAME)
	      derive_equivalences (rhs1,
				   fold_binary (MINUS_EXPR, TREE_TYPE (rhs1),
						value, rhs2),
				   recursion_limit - 1);
	    break;
	  }

	/* If one of the operands is a constant, then we can compute
	   the value of the other operand.  If both operands are
	   SSA_NAMEs, then they must be equal if the result is zero.  */
	case MINUS_EXPR:
	  {
	    tree rhs1 = gimple_assign_rhs1 (def_stmt);
	    tree rhs2 = gimple_assign_rhs2 (def_stmt);

	    /* If either argument is a constant, then we can compute
	       a constant value for the nonconstant argument.  */
	    if (TREE_CODE (rhs1) == INTEGER_CST
		&& TREE_CODE (rhs2) == SSA_NAME)
	      derive_equivalences (rhs2,
				   fold_binary (MINUS_EXPR, TREE_TYPE (rhs1),
						rhs1, value),
				   recursion_limit - 1);
	    else if (TREE_CODE (rhs2) == INTEGER_CST
		     && TREE_CODE (rhs1) == SSA_NAME)
	      derive_equivalences (rhs1,
				   fold_binary (PLUS_EXPR, TREE_TYPE (rhs1),
						value, rhs2),
				   recursion_limit - 1);
	    else if (integer_zerop (value))
	      {
		tree cond = build2 (EQ_EXPR, boolean_type_node,
				    gimple_assign_rhs1 (def_stmt),
				    gimple_assign_rhs2 (def_stmt));
		tree inverted = invert_truthvalue (cond);
		record_conditions (&this->cond_equivalences, cond, inverted);
	      }
	    break;
	  }

	case EQ_EXPR:
	case NE_EXPR:
	  {
	    if ((code == EQ_EXPR && integer_onep (value))
		|| (code == NE_EXPR && integer_zerop (value)))
	      {
		tree rhs1 = gimple_assign_rhs1 (def_stmt);
		tree rhs2 = gimple_assign_rhs2 (def_stmt);

		/* If either argument is a constant, then record the
		   other argument as being the same as that constant.

		   If neither operand is a constant, then we have a
		   conditional name == name equivalence.  */
		if (TREE_CODE (rhs1) == INTEGER_CST)
		  derive_equivalences (rhs2, rhs1, recursion_limit - 1);
		else if (TREE_CODE (rhs2) == INTEGER_CST)
		  derive_equivalences (rhs1, rhs2, recursion_limit - 1);
	      }
	    else
	      {
		tree cond = build2 (code, boolean_type_node,
				    gimple_assign_rhs1 (def_stmt),
				    gimple_assign_rhs2 (def_stmt));
		tree inverted = invert_truthvalue (cond);
		if (integer_zerop (value))
		  std::swap (cond, inverted);
		record_conditions (&this->cond_equivalences, cond, inverted);
	      }
	    break;
	  }

	/* For BIT_NOT and NEGATE, we can just apply the operation to the
	   VALUE to get the new equivalence.  It will always be a constant
	   so we can recurse.  */
	case BIT_NOT_EXPR:
	case NEGATE_EXPR:
	  {
	    tree rhs = gimple_assign_rhs1 (def_stmt);
	    tree res;
	    /* If this is a NOT and the operand has a boolean range, then we
	       know its value must be zero or one.  We are not supposed to
	       have a BIT_NOT_EXPR for boolean types with precision > 1 in
	       the general case, see e.g. the handling of TRUTH_NOT_EXPR in
	       the gimplifier, but it can be generated by match.pd out of
	       a BIT_XOR_EXPR wrapped in a BIT_AND_EXPR.  Now the handling
	       of BIT_AND_EXPR above already forces a specific semantics for
	       boolean types with precision > 1 so we must do the same here,
	       otherwise we could change the semantics of TRUTH_NOT_EXPR for
	       boolean types with precision > 1.  */
	    if (code == BIT_NOT_EXPR
		&& TREE_CODE (rhs) == SSA_NAME
		&& ssa_name_has_boolean_range (rhs))
	      {
		if ((TREE_INT_CST_LOW (value) & 1) == 0)
		  res = build_one_cst (TREE_TYPE (rhs));
		else
		  res = build_zero_cst (TREE_TYPE (rhs));
	      }
	    else
	      res = fold_build1 (code, TREE_TYPE (rhs), value);
	    derive_equivalences (rhs, res, recursion_limit - 1);
	    break;
	  }

	default:
	  {
	    if (TREE_CODE_CLASS (code) == tcc_comparison)
	      {
		tree cond = build2 (code, boolean_type_node,
				    gimple_assign_rhs1 (def_stmt),
				    gimple_assign_rhs2 (def_stmt));
		tree inverted = invert_truthvalue (cond);
		if (integer_zerop (value))
		  std::swap (cond, inverted);
		record_conditions (&this->cond_equivalences, cond, inverted);
		break;
	      }
	    break;
	  }
	}
    }
}

void
edge_info::record_simple_equiv (tree lhs, tree rhs)
{
  /* If the RHS is a constant, then we may be able to derive
     further equivalences.  Else just record the name = name
     equivalence.  */
  if (TREE_CODE (rhs) == INTEGER_CST)
    derive_equivalences (lhs, rhs, 4);
  else
    simple_equivalences.safe_push (equiv_pair (lhs, rhs));
}

/* Free the edge_info data attached to E, if it exists and
   clear e->aux.  */

void
free_dom_edge_info (edge e)
{
  class edge_info *edge_info = (class edge_info *)e->aux;

  if (edge_info)
    delete edge_info;
  e->aux = NULL;
}

/* Free all EDGE_INFO structures associated with edges in the CFG.
   If a particular edge can be threaded, copy the redirection
   target from the EDGE_INFO structure into the edge's AUX field
   as required by code to update the CFG and SSA graph for
   jump threading.  */

static void
free_all_edge_infos (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	free_dom_edge_info (e);
    }
}

/* Return TRUE if BB has precisely two preds, one of which
   is a backedge from a forwarder block where the forwarder
   block is a direct successor of BB.  Being a forwarder
   block, it has no side effects other than transfer of
   control.  Otherwise return FALSE.  */

static bool
single_block_loop_p (basic_block bb)
{
  /* Two preds.  */
  if (EDGE_COUNT (bb->preds) != 2)
    return false;

  /* One and only one of the edges must be marked with
     EDGE_DFS_BACK.  */
  basic_block pred = NULL;
  unsigned int count = 0;
  if (EDGE_PRED (bb, 0)->flags & EDGE_DFS_BACK)
    {
      pred = EDGE_PRED (bb, 0)->src;
      count++;
    }
  if (EDGE_PRED (bb, 1)->flags & EDGE_DFS_BACK)
    {
      pred = EDGE_PRED (bb, 1)->src;
      count++;
    }

  if (count != 1)
    return false;

  /* Now examine PRED.  It should have a single predecessor which
     is BB and a single successor that is also BB.  */
  if (EDGE_COUNT (pred->preds) != 1
      || EDGE_COUNT (pred->succs) != 1
      || EDGE_PRED (pred, 0)->src != bb
      || EDGE_SUCC (pred, 0)->dest != bb)
    return false;

  /* This looks good from a CFG standpoint.  Now look at the guts
     of PRED.  Basically we want to verify there are no PHI nodes
     and no real statements.  */
  if (! gimple_seq_empty_p (phi_nodes (pred)))
    return false;

  gimple_stmt_iterator gsi;
  for (gsi = gsi_last_bb (pred); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      switch (gimple_code (stmt))
	{
	  case GIMPLE_LABEL:
	    if (DECL_NONLOCAL (gimple_label_label (as_a <glabel *> (stmt))))
	      return false;
	    break;

	  case GIMPLE_DEBUG:
	    break;

	  default:
	    return false;
	}
    }

  return true;
}

/* We have finished optimizing BB, record any information implied by
   taking a specific outgoing edge from BB.  */

static void
record_edge_info (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  class edge_info *edge_info;

  /* Free all the outgoing edge info data associated with
     BB's outgoing edges.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    free_dom_edge_info (e);

  if (! gsi_end_p (gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      location_t loc = gimple_location (stmt);

      if (gimple_code (stmt) == GIMPLE_SWITCH)
	{
	  gswitch *switch_stmt = as_a <gswitch *> (stmt);
	  tree index = gimple_switch_index (switch_stmt);

	  if (TREE_CODE (index) == SSA_NAME)
	    {
	      int i;
              int n_labels = gimple_switch_num_labels (switch_stmt);
	      tree *info = XCNEWVEC (tree, last_basic_block_for_fn (cfun));

	      for (i = 0; i < n_labels; i++)
		{
		  tree label = gimple_switch_label (switch_stmt, i);
		  basic_block target_bb
		    = label_to_block (cfun, CASE_LABEL (label));
		  if (CASE_HIGH (label)
		      || !CASE_LOW (label)
		      || info[target_bb->index])
		    info[target_bb->index] = error_mark_node;
		  else
		    info[target_bb->index] = label;
		}

	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  basic_block target_bb = e->dest;
		  tree label = info[target_bb->index];

		  if (label != NULL && label != error_mark_node)
		    {
		      tree x = fold_convert_loc (loc, TREE_TYPE (index),
						 CASE_LOW (label));
		      edge_info = new class edge_info (e);
		      edge_info->record_simple_equiv (index, x);
		    }
		}
	      free (info);
	    }
	}

      /* A COND_EXPR may create equivalences too.  */
      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  edge true_edge;
	  edge false_edge;

          tree op0 = gimple_cond_lhs (stmt);
          tree op1 = gimple_cond_rhs (stmt);
          enum tree_code code = gimple_cond_code (stmt);

	  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

          /* Special case comparing booleans against a constant as we
             know the value of OP0 on both arms of the branch.  i.e., we
             can record an equivalence for OP0 rather than COND. 

	     However, don't do this if the constant isn't zero or one.
	     Such conditionals will get optimized more thoroughly during
	     the domwalk.  */
	  if ((code == EQ_EXPR || code == NE_EXPR)
	      && TREE_CODE (op0) == SSA_NAME
	      && ssa_name_has_boolean_range (op0)
	      && is_gimple_min_invariant (op1)
	      && (integer_zerop (op1) || integer_onep (op1)))
            {
	      tree true_val = constant_boolean_node (true, TREE_TYPE (op0));
	      tree false_val = constant_boolean_node (false, TREE_TYPE (op0));

              if (code == EQ_EXPR)
                {
		  edge_info = new class edge_info (true_edge);
		  edge_info->record_simple_equiv (op0,
						  (integer_zerop (op1)
						   ? false_val : true_val));
		  edge_info = new class edge_info (false_edge);
		  edge_info->record_simple_equiv (op0,
						  (integer_zerop (op1)
						   ? true_val : false_val));
                }
              else
                {
		  edge_info = new class edge_info (true_edge);
		  edge_info->record_simple_equiv (op0,
						  (integer_zerop (op1)
						   ? true_val : false_val));
		  edge_info = new class edge_info (false_edge);
		  edge_info->record_simple_equiv (op0,
						  (integer_zerop (op1)
						   ? false_val : true_val));
                }
            }
	  /* This can show up in the IL as a result of copy propagation
	     it will eventually be canonicalized, but we have to cope
	     with this case within the pass.  */
          else if (is_gimple_min_invariant (op0)
                   && TREE_CODE (op1) == SSA_NAME)
            {
              tree cond = build2 (code, boolean_type_node, op0, op1);
              tree inverted = invert_truthvalue_loc (loc, cond);
	      bool can_infer_simple_equiv
		= !(HONOR_SIGNED_ZEROS (op0) && real_maybe_zerop (op0))
		  && !DECIMAL_FLOAT_MODE_P (element_mode (TREE_TYPE (op0)));
	      class edge_info *edge_info;

	      edge_info = new class edge_info (true_edge);
              record_conditions (&edge_info->cond_equivalences, cond, inverted);

              if (can_infer_simple_equiv && code == EQ_EXPR)
		edge_info->record_simple_equiv (op1, op0);

	      edge_info = new class edge_info (false_edge);
              record_conditions (&edge_info->cond_equivalences, inverted, cond);

              if (can_infer_simple_equiv && TREE_CODE (inverted) == EQ_EXPR)
		edge_info->record_simple_equiv (op1, op0);
            }

          else if (TREE_CODE (op0) == SSA_NAME
                   && (TREE_CODE (op1) == SSA_NAME
                       || is_gimple_min_invariant (op1)))
            {
              tree cond = build2 (code, boolean_type_node, op0, op1);
              tree inverted = invert_truthvalue_loc (loc, cond);
	      bool can_infer_simple_equiv
		= !(HONOR_SIGNED_ZEROS (op1) && real_maybe_zerop (op1))
		  && !DECIMAL_FLOAT_MODE_P (element_mode (TREE_TYPE (op1)));
	      class edge_info *edge_info;

	      edge_info = new class edge_info (true_edge);
              record_conditions (&edge_info->cond_equivalences, cond, inverted);

              if (can_infer_simple_equiv && code == EQ_EXPR)
		edge_info->record_simple_equiv (op0, op1);

	      edge_info = new class edge_info (false_edge);
              record_conditions (&edge_info->cond_equivalences, inverted, cond);

              if (can_infer_simple_equiv && TREE_CODE (inverted) == EQ_EXPR)
		edge_info->record_simple_equiv (op0, op1);
            }

	  /* If this block is a single block loop, then we may be able to
	     record some equivalences on the loop's exit edge.  */
	  if (single_block_loop_p (bb))
	    {
	      /* We know it's a single block loop.  Now look at the loop
		 exit condition.  What we're looking for is whether or not
		 the exit condition is loop invariant which we can detect
		 by checking if all the SSA_NAMEs referenced are defined
		 outside the loop.  */
	      if ((TREE_CODE (op0) != SSA_NAME
		   || gimple_bb (SSA_NAME_DEF_STMT (op0)) != bb)
		  && (TREE_CODE (op1) != SSA_NAME
		      || gimple_bb (SSA_NAME_DEF_STMT (op1)) != bb))
		{
		  /* At this point we know the exit condition is loop
		     invariant.  The only way to get out of the loop is
		     if it never traverses the backedge to begin with.  This
		     implies that any PHI nodes create equivalances that we
		     can attach to the loop exit edge.  */
		  bool alternative
		    = (EDGE_PRED (bb, 0)->flags & EDGE_DFS_BACK) ? 1 : 0;

		  gphi_iterator gsi;
		  for (gsi = gsi_start_phis (bb);
		       !gsi_end_p (gsi);
		       gsi_next (&gsi))
		    {
		      /* Now get the EDGE_INFO class so we can append
			 it to our list.  We want the successor edge
			 where the destination is not the source of
			 an incoming edge.  */
		      gphi *phi = gsi.phi ();
		      tree src = PHI_ARG_DEF (phi, alternative);
		      tree dst = PHI_RESULT (phi);

		      /* If the other alternative is the same as the result,
			 then this is a degenerate and can be ignored.  */
		      if (dst == PHI_ARG_DEF (phi, !alternative))
			continue;

		      if (EDGE_SUCC (bb, 0)->dest
			  != EDGE_PRED (bb, !alternative)->src)
			edge_info = (class edge_info *)EDGE_SUCC (bb, 0)->aux;
		      else
			edge_info = (class edge_info *)EDGE_SUCC (bb, 1)->aux;

		      /* Note that since this processing is done independently
			 of other edge equivalency processing, we may not
			 have an EDGE_INFO structure set up yet.  */
		      if (edge_info == NULL)
			edge_info = new class edge_info (false_edge);
		      edge_info->record_simple_equiv (dst, src);
		    }
		}
	    }
        }
    }
}

class dom_jt_state : public jt_state
{
public:
  dom_jt_state (const_and_copies *copies, avail_exprs_stack *avails)
    : m_copies (copies), m_avails (avails)
  {
    bitmap_tree_view (m_blocks_on_stack);
  }
  void push (edge e) override
  {
    m_copies->push_marker ();
    m_avails->push_marker ();
    jt_state::push (e);
  }
  void pop () override
  {
    m_copies->pop_to_marker ();
    m_avails->pop_to_marker ();
    jt_state::pop ();
  }
  void register_equivs_edge (edge e) override
  {
    record_temporary_equivalences (e, m_copies, m_avails, m_blocks_on_stack);
  }
  void register_equiv (tree dest, tree src, bool update) override;
  bitmap get_blocks_on_stack () { return m_blocks_on_stack; }
private:
  const_and_copies *m_copies;
  avail_exprs_stack *m_avails;
  /* Set of blocks on the stack, to be used for medium-fast
     dominance queries in back_propagate_equivalences.  */
  auto_bitmap m_blocks_on_stack;
};

void
dom_jt_state::register_equiv (tree dest, tree src, bool)
{
  m_copies->record_const_or_copy (dest, src);
}

class dom_jt_simplifier : public hybrid_jt_simplifier
{
public:
  dom_jt_simplifier (avail_exprs_stack *avails, gimple_ranger *ranger,
		     path_range_query *query)
    : hybrid_jt_simplifier (ranger, query), m_avails (avails) { }

private:
  tree simplify (gimple *, gimple *, basic_block, jt_state *) override;
  avail_exprs_stack *m_avails;
};

tree
dom_jt_simplifier::simplify (gimple *stmt, gimple *within_stmt,
			     basic_block bb, jt_state *state)
{
  /* First see if the conditional is in the hash table.  */
  tree cached_lhs =  m_avails->lookup_avail_expr (stmt, false, true);
  if (cached_lhs)
    return cached_lhs;

  /* Otherwise call the ranger if possible.  */
  if (state)
    return hybrid_jt_simplifier::simplify (stmt, within_stmt, bb, state);

  return NULL;
}

class dom_opt_dom_walker : public dom_walker
{
public:
  dom_opt_dom_walker (cdi_direction direction,
		      jump_threader *threader,
		      dom_jt_state *state,
		      gimple_ranger *ranger,
		      const_and_copies *const_and_copies,
		      avail_exprs_stack *avail_exprs_stack)
    : dom_walker (direction, REACHABLE_BLOCKS)
    {
      m_ranger = ranger;
      m_state = state;
      m_dummy_cond = gimple_build_cond (NE_EXPR, integer_zero_node,
					integer_zero_node, NULL, NULL);
      m_const_and_copies = const_and_copies;
      m_avail_exprs_stack = avail_exprs_stack;
      m_threader = threader;
    }

  edge before_dom_children (basic_block) final override;
  void after_dom_children (basic_block) final override;

private:

  /* Unwindable equivalences, both const/copy and expression varieties.  */
  class const_and_copies *m_const_and_copies;
  class avail_exprs_stack *m_avail_exprs_stack;

  /* Dummy condition to avoid creating lots of throw away statements.  */
  gcond *m_dummy_cond;

  /* Optimize a single statement within a basic block using the
     various tables mantained by DOM.  Returns the taken edge if
     the statement is a conditional with a statically determined
     value.  */
  edge optimize_stmt (basic_block, gimple_stmt_iterator *, bool *);

  void set_global_ranges_from_unreachable_edges (basic_block);

  void test_for_singularity (gimple *, avail_exprs_stack *);
  edge fold_cond (gcond *cond);

  jump_threader *m_threader;
  gimple_ranger *m_ranger;
  dom_jt_state *m_state;
};

/* Jump threading, redundancy elimination and const/copy propagation.

   This pass may expose new symbols that need to be renamed into SSA.  For
   every new symbol exposed, its corresponding bit will be set in
   VARS_TO_RENAME.  */

namespace {

const pass_data pass_data_dominator =
{
  GIMPLE_PASS, /* type */
  "dom", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SSA_DOMINATOR_OPTS, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_dominator : public gimple_opt_pass
{
public:
  pass_dominator (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_dominator, ctxt),
      may_peel_loop_headers_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_dominator (m_ctxt); }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      may_peel_loop_headers_p = param;
    }
  bool gate (function *) final override { return flag_tree_dom != 0; }
  unsigned int execute (function *) final override;

 private:
  /* This flag is used to prevent loops from being peeled repeatedly in jump
     threading; it will be removed once we preserve loop structures throughout
     the compilation -- we will be able to mark the affected loops directly in
     jump threading, and avoid peeling them next time.  */
  bool may_peel_loop_headers_p;
}; // class pass_dominator

unsigned int
pass_dominator::execute (function *fun)
{
  memset (&opt_stats, 0, sizeof (opt_stats));

  /* Create our hash tables.  */
  hash_table<expr_elt_hasher> *avail_exprs
    = new hash_table<expr_elt_hasher> (1024);
  class avail_exprs_stack *avail_exprs_stack
    = new class avail_exprs_stack (avail_exprs);
  class const_and_copies *const_and_copies = new class const_and_copies ();
  need_eh_cleanup = BITMAP_ALLOC (NULL);
  need_noreturn_fixup.create (0);

  calculate_dominance_info (CDI_DOMINATORS);
  cfg_altered = false;

  /* We need to know loop structures in order to avoid destroying them
     in jump threading.  Note that we still can e.g. thread through loop
     headers to an exit edge, or through loop header to the loop body, assuming
     that we update the loop info.

     TODO: We don't need to set LOOPS_HAVE_PREHEADERS generally, but due
     to several overly conservative bail-outs in jump threading, case
     gcc.dg/tree-ssa/pr21417.c can't be threaded if loop preheader is
     missing.  We should improve jump threading in future then
     LOOPS_HAVE_PREHEADERS won't be needed here.  */
  loop_optimizer_init (LOOPS_HAVE_PREHEADERS | LOOPS_HAVE_SIMPLE_LATCHES
		       | LOOPS_HAVE_MARKED_IRREDUCIBLE_REGIONS);

  /* We need accurate information regarding back edges in the CFG
     for jump threading; this may include back edges that are not part of
     a single loop.  */
  mark_dfs_back_edges ();

  /* We want to create the edge info structures before the dominator walk
     so that they'll be in place for the jump threader, particularly when
     threading through a join block.

     The conditions will be lazily updated with global equivalences as
     we reach them during the dominator walk.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    record_edge_info (bb);

  /* Recursively walk the dominator tree optimizing statements.  */
  gimple_ranger *ranger = enable_ranger (fun);
  path_range_query path_query (*ranger);
  dom_jt_simplifier simplifier (avail_exprs_stack, ranger, &path_query);
  dom_jt_state state (const_and_copies, avail_exprs_stack);
  jump_threader threader (&simplifier, &state);
  dom_opt_dom_walker walker (CDI_DOMINATORS,
			     &threader,
			     &state,
			     ranger,
			     const_and_copies,
			     avail_exprs_stack);
  walker.walk (fun->cfg->x_entry_block_ptr);

  ranger->export_global_ranges ();
  disable_ranger (fun);

  /* Look for blocks where we cleared EDGE_EXECUTABLE on an outgoing
     edge.  When found, remove jump threads which contain any outgoing
     edge from the affected block.  */
  if (cfg_altered)
    {
      FOR_EACH_BB_FN (bb, fun)
	{
	  edge_iterator ei;
	  edge e;

	  /* First see if there are any edges without EDGE_EXECUTABLE
	     set.  */
	  bool found = false;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if ((e->flags & EDGE_EXECUTABLE) == 0)
		{
		  found = true;
		  break;
		}
	    }

	  /* If there were any such edges found, then remove jump threads
	     containing any edge leaving BB.  */
	  if (found)
	    FOR_EACH_EDGE (e, ei, bb->succs)
	      threader.remove_jump_threads_including (e);
	}
    }

  {
    gimple_stmt_iterator gsi;
    basic_block bb;
    FOR_EACH_BB_FN (bb, fun)
      {
	for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	  update_stmt_if_modified (gsi_stmt (gsi));
      }
  }

  /* If we exposed any new variables, go ahead and put them into
     SSA form now, before we handle jump threading.  This simplifies
     interactions between rewriting of _DECL nodes into SSA form
     and rewriting SSA_NAME nodes into SSA form after block
     duplication and CFG manipulation.  */
  update_ssa (TODO_update_ssa);

  free_all_edge_infos ();

  /* Thread jumps, creating duplicate blocks as needed.  */
  cfg_altered |= threader.thread_through_all_blocks (may_peel_loop_headers_p);

  if (cfg_altered)
    free_dominance_info (CDI_DOMINATORS);

  /* Removal of statements may make some EH edges dead.  Purge
     such edges from the CFG as needed.  */
  if (!bitmap_empty_p (need_eh_cleanup))
    {
      unsigned i;
      bitmap_iterator bi;

      /* Jump threading may have created forwarder blocks from blocks
	 needing EH cleanup; the new successor of these blocks, which
	 has inherited from the original block, needs the cleanup.
	 Don't clear bits in the bitmap, as that can break the bitmap
	 iterator.  */
      EXECUTE_IF_SET_IN_BITMAP (need_eh_cleanup, 0, i, bi)
	{
	  basic_block bb = BASIC_BLOCK_FOR_FN (fun, i);
	  if (bb == NULL)
	    continue;
	  while (single_succ_p (bb)
		 && (single_succ_edge (bb)->flags
		     & (EDGE_EH|EDGE_DFS_BACK)) == 0)
	    bb = single_succ (bb);
	  if (bb == EXIT_BLOCK_PTR_FOR_FN (fun))
	    continue;
	  if ((unsigned) bb->index != i)
	    bitmap_set_bit (need_eh_cleanup, bb->index);
	}

      gimple_purge_all_dead_eh_edges (need_eh_cleanup);
      bitmap_clear (need_eh_cleanup);
    }

  /* Fixup stmts that became noreturn calls.  This may require splitting
     blocks and thus isn't possible during the dominator walk or before
     jump threading finished.  Do this in reverse order so we don't
     inadvertedly remove a stmt we want to fixup by visiting a dominating
     now noreturn call first.  */
  while (!need_noreturn_fixup.is_empty ())
    {
      gimple *stmt = need_noreturn_fixup.pop ();
      if (dump_file && dump_flags & TDF_DETAILS)
	{
	  fprintf (dump_file, "Fixing up noreturn call ");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "\n");
	}
      fixup_noreturn_call (stmt);
    }

  statistics_counter_event (fun, "Redundant expressions eliminated",
			    opt_stats.num_re);
  statistics_counter_event (fun, "Constants propagated",
			    opt_stats.num_const_prop);
  statistics_counter_event (fun, "Copies propagated",
			    opt_stats.num_copy_prop);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    dump_dominator_optimization_stats (dump_file, avail_exprs);

  loop_optimizer_finalize ();

  /* Delete our main hashtable.  */
  delete avail_exprs;
  avail_exprs = NULL;

  /* Free asserted bitmaps and stacks.  */
  BITMAP_FREE (need_eh_cleanup);
  need_noreturn_fixup.release ();
  delete avail_exprs_stack;
  delete const_and_copies;

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_dominator (gcc::context *ctxt)
{
  return new pass_dominator (ctxt);
}

/* Valueize hook for gimple_fold_stmt_to_constant_1.  */

static tree
dom_valueize (tree t)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      tree tem = SSA_NAME_VALUE (t);
      if (tem)
	return tem;
    }
  return t;
}

/* We have just found an equivalence for LHS on an edge E.
   Look backwards to other uses of LHS and see if we can derive
   additional equivalences that are valid on edge E.  */
static void
back_propagate_equivalences (tree lhs, edge e,
			     class const_and_copies *const_and_copies,
			     bitmap domby)
{
  use_operand_p use_p;
  imm_use_iterator iter;
  basic_block dest = e->dest;
  bool domok = (dom_info_state (CDI_DOMINATORS) == DOM_OK);

  /* Iterate over the uses of LHS to see if any dominate E->dest.
     If so, they may create useful equivalences too.

     ???  If the code gets re-organized to a worklist to catch more
     indirect opportunities and it is made to handle PHIs then this
     should only consider use_stmts in basic-blocks we have already visited.  */
  FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
    {
      gimple *use_stmt = USE_STMT (use_p);

      /* Often the use is in DEST, which we trivially know we can't use.
	 This is cheaper than the dominator set tests below.  */
      if (dest == gimple_bb (use_stmt))
	continue;

      /* Filter out statements that can never produce a useful
	 equivalence.  */
      tree lhs2 = gimple_get_lhs (use_stmt);
      if (!lhs2 || TREE_CODE (lhs2) != SSA_NAME)
	continue;

      if (domok)
	{
	  if (!dominated_by_p (CDI_DOMINATORS, dest, gimple_bb (use_stmt)))
	    continue;
	}
      else
	{
	  /* We can use the set of BBs on the stack from a domwalk
	     for a medium fast way to query dominance.  Profiling
	     has shown non-fast query dominance tests here can be fairly
	     expensive.  */
	  /* This tests if USE_STMT does not dominate DEST.  */
	  if (!bitmap_bit_p (domby, gimple_bb (use_stmt)->index))
	    continue;
	}

      /* At this point USE_STMT dominates DEST and may result in a
	 useful equivalence.  Try to simplify its RHS to a constant
	 or SSA_NAME.  */
      tree res = gimple_fold_stmt_to_constant_1 (use_stmt, dom_valueize,
						 no_follow_ssa_edges);
      if (res && (TREE_CODE (res) == SSA_NAME || is_gimple_min_invariant (res)))
	record_equality (lhs2, res, const_and_copies);
    }
}

/* Record into CONST_AND_COPIES and AVAIL_EXPRS_STACK any equivalences implied
   by traversing edge E (which are cached in E->aux).

   Callers are responsible for managing the unwinding markers.  */
static void
record_temporary_equivalences (edge e,
			       class const_and_copies *const_and_copies,
			       class avail_exprs_stack *avail_exprs_stack,
			       bitmap blocks_on_stack)
{
  int i;
  class edge_info *edge_info = (class edge_info *) e->aux;

  /* If we have info associated with this edge, record it into
     our equivalence tables.  */
  if (edge_info)
    {
      cond_equivalence *eq;
      /* If we have 0 = COND or 1 = COND equivalences, record them
	 into our expression hash tables.  */
      for (i = 0; edge_info->cond_equivalences.iterate (i, &eq); ++i)
	avail_exprs_stack->record_cond (eq);

      edge_info::equiv_pair *seq;
      for (i = 0; edge_info->simple_equivalences.iterate (i, &seq); ++i)
	{
	  tree lhs = seq->first;
	  if (!lhs || TREE_CODE (lhs) != SSA_NAME)
	    continue;

	  /* Record the simple NAME = VALUE equivalence.  */
	  tree rhs = seq->second;

	  /* If this is a SSA_NAME = SSA_NAME equivalence and one operand is
	     cheaper to compute than the other, then set up the equivalence
	     such that we replace the expensive one with the cheap one.

	     If they are the same cost to compute, then do not record
	     anything.  */
	  if (TREE_CODE (lhs) == SSA_NAME && TREE_CODE (rhs) == SSA_NAME)
	    {
	      gimple *rhs_def = SSA_NAME_DEF_STMT (rhs);
	      int rhs_cost = estimate_num_insns (rhs_def, &eni_size_weights);

	      gimple *lhs_def = SSA_NAME_DEF_STMT (lhs);
	      int lhs_cost = estimate_num_insns (lhs_def, &eni_size_weights);

	      if (rhs_cost > lhs_cost)
	        record_equality (rhs, lhs, const_and_copies);
	      else if (rhs_cost < lhs_cost)
	        record_equality (lhs, rhs, const_and_copies);
	    }
	  else
	    record_equality (lhs, rhs, const_and_copies);


	  /* Any equivalence found for LHS may result in additional
	     equivalences for other uses of LHS that we have already
	     processed.  */
	  back_propagate_equivalences (lhs, e, const_and_copies,
				       blocks_on_stack);
	}
    }
}

/* PHI nodes can create equivalences too.

   Ignoring any alternatives which are the same as the result, if
   all the alternatives are equal, then the PHI node creates an
   equivalence.  */

static void
record_equivalences_from_phis (basic_block bb)
{
  gphi_iterator gsi;

  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
    {
      gphi *phi = gsi.phi ();

      /* We might eliminate the PHI, so advance GSI now.  */
      gsi_next (&gsi);

      tree lhs = gimple_phi_result (phi);
      tree rhs = NULL;
      size_t i;

      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree t = gimple_phi_arg_def (phi, i);

	  /* Ignore alternatives which are the same as our LHS.  Since
	     LHS is a PHI_RESULT, it is known to be a SSA_NAME, so we
	     can simply compare pointers.  */
	  if (lhs == t)
	    continue;

	  /* If the associated edge is not marked as executable, then it
	     can be ignored.  */
	  if ((gimple_phi_arg_edge (phi, i)->flags & EDGE_EXECUTABLE) == 0)
	    continue;

	  t = dom_valueize (t);

	  /* If T is an SSA_NAME and its associated edge is a backedge,
	     then quit as we cannot utilize this equivalence.  */
	  if (TREE_CODE (t) == SSA_NAME
	      && (gimple_phi_arg_edge (phi, i)->flags & EDGE_DFS_BACK))
	    break;

	  /* If we have not processed an alternative yet, then set
	     RHS to this alternative.  */
	  if (rhs == NULL)
	    rhs = t;
	  /* If we have processed an alternative (stored in RHS), then
	     see if it is equal to this one.  If it isn't, then stop
	     the search.  */
	  else if (! operand_equal_for_phi_arg_p (rhs, t))
	    break;
	}

      /* If we had no interesting alternatives, then all the RHS alternatives
	 must have been the same as LHS.  */
      if (!rhs)
	rhs = lhs;

      /* If we managed to iterate through each PHI alternative without
	 breaking out of the loop, then we have a PHI which may create
	 a useful equivalence.  We do not need to record unwind data for
	 this, since this is a true assignment and not an equivalence
	 inferred from a comparison.  All uses of this ssa name are dominated
	 by this assignment, so unwinding just costs time and space.  */
      if (i == gimple_phi_num_args (phi))
	{
	  if (may_propagate_copy (lhs, rhs))
	    set_ssa_name_value (lhs, rhs);
	  else if (virtual_operand_p (lhs))
	    {
	      gimple *use_stmt;
	      imm_use_iterator iter;
	      use_operand_p use_p;
	      /* For virtual operands we have to propagate into all uses as
	         otherwise we will create overlapping life-ranges.  */
	      FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	        FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	          SET_USE (use_p, rhs);
	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	        SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs) = 1;
	      gimple_stmt_iterator tmp_gsi = gsi_for_stmt (phi);
	      remove_phi_node (&tmp_gsi, true);
	    }
	}
    }
}

/* Return true if all uses of NAME are dominated by STMT or feed STMT
   via a chain of single immediate uses.  */

static bool
all_uses_feed_or_dominated_by_stmt (tree name, gimple *stmt)
{
  use_operand_p use_p, use2_p;
  imm_use_iterator iter;
  basic_block stmt_bb = gimple_bb (stmt);

  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      gimple *use_stmt = USE_STMT (use_p), *use_stmt2;
      if (use_stmt == stmt
	  || is_gimple_debug (use_stmt)
	  || (gimple_bb (use_stmt) != stmt_bb
	      && dominated_by_p (CDI_DOMINATORS,
				 gimple_bb (use_stmt), stmt_bb)))
	continue;
      while (use_stmt != stmt
	     && is_gimple_assign (use_stmt)
	     && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
	     && single_imm_use (gimple_assign_lhs (use_stmt),
				&use2_p, &use_stmt2))
	use_stmt = use_stmt2;
      if (use_stmt != stmt)
	return false;
    }
  return true;
}

/* Set global ranges that can be determined from the C->M edge:

   <bb C>:
   ...
   if (something)
     goto <bb N>;
   else
     goto <bb M>;
   <bb N>:
   __builtin_unreachable ();
   <bb M>:
*/

void
dom_opt_dom_walker::set_global_ranges_from_unreachable_edges (basic_block bb)
{
  edge pred_e = single_pred_edge_ignoring_loop_edges (bb, false);
  if (!pred_e)
    return;

  gimple *stmt = last_stmt (pred_e->src);
  if (!stmt
      || gimple_code (stmt) != GIMPLE_COND
      || !assert_unreachable_fallthru_edge_p (pred_e))
    return;

  tree name;
  gori_compute &gori = m_ranger->gori ();
  FOR_EACH_GORI_EXPORT_NAME (gori, pred_e->src, name)
    if (all_uses_feed_or_dominated_by_stmt (name, stmt)
	// The condition must post-dominate the definition point.
	&& (SSA_NAME_IS_DEFAULT_DEF (name)
	    || (gimple_bb (SSA_NAME_DEF_STMT (name))
		== pred_e->src)))
      {
	Value_Range r (TREE_TYPE (name));

	if (m_ranger->range_on_edge (r, pred_e, name)
	    && !r.varying_p ()
	    && !r.undefined_p ())
	  {
	    set_range_info (name, r);
	    maybe_set_nonzero_bits (pred_e, name);
	  }
      }
}

/* Record any equivalences created by the incoming edge to BB into
   CONST_AND_COPIES and AVAIL_EXPRS_STACK.  If BB has more than one
   incoming edge, then no equivalence is created.  */

static void
record_equivalences_from_incoming_edge (basic_block bb,
    class const_and_copies *const_and_copies,
    class avail_exprs_stack *avail_exprs_stack,
    bitmap blocks_on_stack)
{
  edge e;
  basic_block parent;

  /* If our parent block ended with a control statement, then we may be
     able to record some equivalences based on which outgoing edge from
     the parent was followed.  */
  parent = get_immediate_dominator (CDI_DOMINATORS, bb);

  e = single_pred_edge_ignoring_loop_edges (bb, true);

  /* If we had a single incoming edge from our parent block, then enter
     any data associated with the edge into our tables.  */
  if (e && e->src == parent)
    record_temporary_equivalences (e, const_and_copies, avail_exprs_stack,
				   blocks_on_stack);
}

/* Dump statistics for the hash table HTAB.  */

static void
htab_statistics (FILE *file, const hash_table<expr_elt_hasher> &htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab.size (),
	   (long) htab.elements (),
	   htab.collisions ());
}

/* Dump SSA statistics on FILE.  */

static void
dump_dominator_optimization_stats (FILE *file,
				   hash_table<expr_elt_hasher> *avail_exprs)
{
  fprintf (file, "Total number of statements:                   %6ld\n\n",
	   opt_stats.num_stmts);
  fprintf (file, "Exprs considered for dominator optimizations: %6ld\n",
           opt_stats.num_exprs_considered);

  fprintf (file, "\nHash table statistics:\n");

  fprintf (file, "    avail_exprs: ");
  htab_statistics (file, *avail_exprs);
}


/* Similarly, but assume that X and Y are the two operands of an EQ_EXPR.
   This constrains the cases in which we may treat this as assignment.  */

static void
record_equality (tree x, tree y, class const_and_copies *const_and_copies)
{
  tree prev_x = NULL, prev_y = NULL;

  if (tree_swap_operands_p (x, y))
    std::swap (x, y);

  /* Most of the time tree_swap_operands_p does what we want.  But there
     are cases where we know one operand is better for copy propagation than
     the other.  Given no other code cares about ordering of equality
     comparison operators for that purpose, we just handle the special cases
     here.  */
  if (TREE_CODE (x) == SSA_NAME && TREE_CODE (y) == SSA_NAME)
    {
      /* If one operand is a single use operand, then make it
	 X.  This will preserve its single use properly and if this
	 conditional is eliminated, the computation of X can be
	 eliminated as well.  */
      if (has_single_use (y) && ! has_single_use (x))
	std::swap (x, y);
    }
  if (TREE_CODE (x) == SSA_NAME)
    prev_x = SSA_NAME_VALUE (x);
  if (TREE_CODE (y) == SSA_NAME)
    prev_y = SSA_NAME_VALUE (y);

  /* If one of the previous values is invariant, or invariant in more loops
     (by depth), then use that.
     Otherwise it doesn't matter which value we choose, just so
     long as we canonicalize on one value.  */
  if (is_gimple_min_invariant (y))
    ;
  else if (is_gimple_min_invariant (x))
    prev_x = x, x = y, y = prev_x, prev_x = prev_y;
  else if (prev_x && is_gimple_min_invariant (prev_x))
    x = y, y = prev_x, prev_x = prev_y;
  else if (prev_y)
    y = prev_y;

  /* After the swapping, we must have one SSA_NAME.  */
  if (TREE_CODE (x) != SSA_NAME)
    return;

  /* For IEEE, -0.0 == 0.0, so we don't necessarily know the sign of a
     variable compared against zero.  If we're honoring signed zeros,
     then we cannot record this value unless we know that the value is
     nonzero.  */
  if (HONOR_SIGNED_ZEROS (x)
      && (TREE_CODE (y) != REAL_CST
	  || real_equal (&dconst0, &TREE_REAL_CST (y))))
    return;

  const_and_copies->record_const_or_copy (x, y, prev_x);
}

/* Returns true when STMT is a simple iv increment.  It detects the
   following situation:

   i_1 = phi (..., i_k)
   [...]
   i_j = i_{j-1}  for each j : 2 <= j <= k-1
   [...]
   i_k = i_{k-1} +/- ...  */

bool
simple_iv_increment_p (gimple *stmt)
{
  enum tree_code code;
  tree lhs, preinc;
  gimple *phi;
  size_t i;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (stmt);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (code != PLUS_EXPR
      && code != MINUS_EXPR
      && code != POINTER_PLUS_EXPR)
    return false;

  preinc = gimple_assign_rhs1 (stmt);
  if (TREE_CODE (preinc) != SSA_NAME)
    return false;

  phi = SSA_NAME_DEF_STMT (preinc);
  while (gimple_code (phi) != GIMPLE_PHI)
    {
      /* Follow trivial copies, but not the DEF used in a back edge,
	 so that we don't prevent coalescing.  */
      if (!gimple_assign_ssa_name_copy_p (phi))
	return false;
      preinc = gimple_assign_rhs1 (phi);
      phi = SSA_NAME_DEF_STMT (preinc);
    }

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    if (gimple_phi_arg_def (phi, i) == lhs)
      return true;

  return false;
}

/* Propagate know values from SSA_NAME_VALUE into the PHI nodes of the
   successors of BB.  */

static void
cprop_into_successor_phis (basic_block bb,
			   class const_and_copies *const_and_copies)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      int indx;
      gphi_iterator gsi;

      /* If this is an abnormal edge, then we do not want to copy propagate
	 into the PHI alternative associated with this edge.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      gsi = gsi_start_phis (e->dest);
      if (gsi_end_p (gsi))
	continue;

      /* We may have an equivalence associated with this edge.  While
	 we cannot propagate it into non-dominated blocks, we can
	 propagate them into PHIs in non-dominated blocks.  */

      /* Push the unwind marker so we can reset the const and copies
	 table back to its original state after processing this edge.  */
      const_and_copies->push_marker ();

      /* Extract and record any simple NAME = VALUE equivalences.

	 Don't bother with [01] = COND equivalences, they're not useful
	 here.  */
      class edge_info *edge_info = (class edge_info *) e->aux;

      if (edge_info)
	{
	  edge_info::equiv_pair *seq;
	  for (int i = 0; edge_info->simple_equivalences.iterate (i, &seq); ++i)
	    {
	      tree lhs = seq->first;
	      tree rhs = seq->second;

	      if (lhs && TREE_CODE (lhs) == SSA_NAME)
		const_and_copies->record_const_or_copy (lhs, rhs);
	    }

	}

      indx = e->dest_idx;
      for ( ; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  tree new_val;
	  use_operand_p orig_p;
	  tree orig_val;
          gphi *phi = gsi.phi ();

	  /* The alternative may be associated with a constant, so verify
	     it is an SSA_NAME before doing anything with it.  */
	  orig_p = gimple_phi_arg_imm_use_ptr (phi, indx);
	  orig_val = get_use_from_ptr (orig_p);
	  if (TREE_CODE (orig_val) != SSA_NAME)
	    continue;

	  /* If we have *ORIG_P in our constant/copy table, then replace
	     ORIG_P with its value in our constant/copy table.  */
	  new_val = SSA_NAME_VALUE (orig_val);
	  if (new_val
	      && new_val != orig_val
	      && may_propagate_copy (orig_val, new_val))
	    propagate_value (orig_p, new_val);
	}

      const_and_copies->pop_to_marker ();
    }
}

edge
dom_opt_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nOptimizing block #%d\n\n", bb->index);

  /* Push a marker on the stacks of local information so that we know how
     far to unwind when we finalize this block.  */
  m_avail_exprs_stack->push_marker ();
  m_const_and_copies->push_marker ();
  bitmap_set_bit (m_state->get_blocks_on_stack (), bb->index);

  record_equivalences_from_incoming_edge (bb, m_const_and_copies,
					  m_avail_exprs_stack,
					  m_state->get_blocks_on_stack ());
  set_global_ranges_from_unreachable_edges (bb);

  /* PHI nodes can create equivalences too.  */
  record_equivalences_from_phis (bb);

  /* Create equivalences from redundant PHIs.  PHIs are only truly
     redundant when they exist in the same block, so push another
     marker and unwind right afterwards.  */
  m_avail_exprs_stack->push_marker ();
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    eliminate_redundant_computations (&gsi, m_const_and_copies,
				      m_avail_exprs_stack);
  m_avail_exprs_stack->pop_to_marker ();

  edge taken_edge = NULL;
  /* Initialize visited flag ahead of us, it has undefined state on
     pass entry.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    gimple_set_visited (gsi_stmt (gsi), false);
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
    {
      /* Do not optimize a stmt twice, substitution might end up with
         _3 = _3 which is not valid.  */
      if (gimple_visited_p (gsi_stmt (gsi)))
	{
	  gsi_next (&gsi);
	  continue;
	}

      bool removed_p = false;
      taken_edge = this->optimize_stmt (bb, &gsi, &removed_p);
      if (!removed_p)
	gimple_set_visited (gsi_stmt (gsi), true);

      /* Go back and visit stmts inserted by folding after substituting
	 into the stmt at gsi.  */
      if (gsi_end_p (gsi))
	{
	  gcc_checking_assert (removed_p);
	  gsi = gsi_last_bb (bb);
	  while (!gsi_end_p (gsi) && !gimple_visited_p (gsi_stmt (gsi)))
	    gsi_prev (&gsi);
	}
      else
	{
	  do
	    {
	      gsi_prev (&gsi);
	    }
	  while (!gsi_end_p (gsi) && !gimple_visited_p (gsi_stmt (gsi)));
	}
      if (gsi_end_p (gsi))
	gsi = gsi_start_bb (bb);
      else
	gsi_next (&gsi);
    }

  /* Now prepare to process dominated blocks.  */
  record_edge_info (bb);
  cprop_into_successor_phis (bb, m_const_and_copies);
  if (taken_edge && !dbg_cnt (dom_unreachable_edges))
    return NULL;

  return taken_edge;
}

/* We have finished processing the dominator children of BB, perform
   any finalization actions in preparation for leaving this node in
   the dominator tree.  */

void
dom_opt_dom_walker::after_dom_children (basic_block bb)
{
  m_threader->thread_outgoing_edges (bb);
  bitmap_clear_bit (m_state->get_blocks_on_stack (), bb->index);
  m_avail_exprs_stack->pop_to_marker ();
  m_const_and_copies->pop_to_marker ();
}

/* Search for redundant computations in STMT.  If any are found, then
   replace them with the variable holding the result of the computation.

   If safe, record this expression into AVAIL_EXPRS_STACK and
   CONST_AND_COPIES.  */

static void
eliminate_redundant_computations (gimple_stmt_iterator* gsi,
				  class const_and_copies *const_and_copies,
				  class avail_exprs_stack *avail_exprs_stack)
{
  tree expr_type;
  tree cached_lhs;
  tree def;
  bool insert = true;
  bool assigns_var_p = false;

  gimple *stmt = gsi_stmt (*gsi);

  if (gimple_code (stmt) == GIMPLE_PHI)
    def = gimple_phi_result (stmt);
  else
    def = gimple_get_lhs (stmt);

  /* Certain expressions on the RHS can be optimized away, but cannot
     themselves be entered into the hash tables.  */
  if (! def
      || TREE_CODE (def) != SSA_NAME
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def)
      || gimple_vdef (stmt)
      /* Do not record equivalences for increments of ivs.  This would create
	 overlapping live ranges for a very questionable gain.  */
      || simple_iv_increment_p (stmt))
    insert = false;

  /* Check if the expression has been computed before.  */
  cached_lhs = avail_exprs_stack->lookup_avail_expr (stmt, insert, true);

  opt_stats.num_exprs_considered++;

  /* Get the type of the expression we are trying to optimize.  */
  if (is_gimple_assign (stmt))
    {
      expr_type = TREE_TYPE (gimple_assign_lhs (stmt));
      assigns_var_p = true;
    }
  else if (gimple_code (stmt) == GIMPLE_COND)
    expr_type = boolean_type_node;
  else if (is_gimple_call (stmt))
    {
      gcc_assert (gimple_call_lhs (stmt));
      expr_type = TREE_TYPE (gimple_call_lhs (stmt));
      assigns_var_p = true;
    }
  else if (gswitch *swtch_stmt = dyn_cast <gswitch *> (stmt))
    expr_type = TREE_TYPE (gimple_switch_index (swtch_stmt));
  else if (gimple_code (stmt) == GIMPLE_PHI)
    /* We can't propagate into a phi, so the logic below doesn't apply.
       Instead record an equivalence between the cached LHS and the
       PHI result of this statement, provided they are in the same block.
       This should be sufficient to kill the redundant phi.  */
    {
      if (def && cached_lhs)
	const_and_copies->record_const_or_copy (def, cached_lhs);
      return;
    }
  else
    gcc_unreachable ();

  if (!cached_lhs)
    return;

  /* It is safe to ignore types here since we have already done
     type checking in the hashing and equality routines.  In fact
     type checking here merely gets in the way of constant
     propagation.  Also, make sure that it is safe to propagate
     CACHED_LHS into the expression in STMT.  */
  if ((TREE_CODE (cached_lhs) != SSA_NAME
       && (assigns_var_p
           || useless_type_conversion_p (expr_type, TREE_TYPE (cached_lhs))))
      || may_propagate_copy_into_stmt (stmt, cached_lhs))
  {
      gcc_checking_assert (TREE_CODE (cached_lhs) == SSA_NAME
			   || is_gimple_min_invariant (cached_lhs));

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced redundant expr '");
	  print_gimple_expr (dump_file, stmt, 0, dump_flags);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, cached_lhs, dump_flags);
          fprintf (dump_file, "'\n");
	}

      opt_stats.num_re++;

      if (assigns_var_p
	  && !useless_type_conversion_p (expr_type, TREE_TYPE (cached_lhs)))
	cached_lhs = fold_convert (expr_type, cached_lhs);

      propagate_tree_value_into_stmt (gsi, cached_lhs);

      /* Since it is always necessary to mark the result as modified,
         perhaps we should move this into propagate_tree_value_into_stmt
         itself.  */
      gimple_set_modified (gsi_stmt (*gsi), true);
  }
}

/* STMT, a GIMPLE_ASSIGN, may create certain equivalences, in either
   the available expressions table or the const_and_copies table.
   Detect and record those equivalences into AVAIL_EXPRS_STACK. 

   We handle only very simple copy equivalences here.  The heavy
   lifing is done by eliminate_redundant_computations.  */

static void
record_equivalences_from_stmt (gimple *stmt, int may_optimize_p,
			       class avail_exprs_stack *avail_exprs_stack)
{
  tree lhs;
  enum tree_code lhs_code;

  gcc_assert (is_gimple_assign (stmt));

  lhs = gimple_assign_lhs (stmt);
  lhs_code = TREE_CODE (lhs);

  if (lhs_code == SSA_NAME
      && gimple_assign_single_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);

      /* If the RHS of the assignment is a constant or another variable that
	 may be propagated, register it in the CONST_AND_COPIES table.  We
	 do not need to record unwind data for this, since this is a true
	 assignment and not an equivalence inferred from a comparison.  All
	 uses of this ssa name are dominated by this assignment, so unwinding
	 just costs time and space.  */
      if (may_optimize_p
	  && (TREE_CODE (rhs) == SSA_NAME
	      || is_gimple_min_invariant (rhs)))
	{
	  rhs = dom_valueize (rhs);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "==== ASGN ");
	      print_generic_expr (dump_file, lhs);
	      fprintf (dump_file, " = ");
	      print_generic_expr (dump_file, rhs);
	      fprintf (dump_file, "\n");
	    }

	  set_ssa_name_value (lhs, rhs);
	}
    }

  /* Make sure we can propagate &x + CST.  */
  if (lhs_code == SSA_NAME
      && gimple_assign_rhs_code (stmt) == POINTER_PLUS_EXPR
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == ADDR_EXPR
      && TREE_CODE (gimple_assign_rhs2 (stmt)) == INTEGER_CST)
    {
      tree op0 = gimple_assign_rhs1 (stmt);
      tree op1 = gimple_assign_rhs2 (stmt);
      tree new_rhs
	= build1 (ADDR_EXPR, TREE_TYPE (op0),
		  fold_build2 (MEM_REF, TREE_TYPE (TREE_TYPE (op0)),
			       unshare_expr (op0), fold_convert (ptr_type_node,
								 op1)));
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "==== ASGN ");
	  print_generic_expr (dump_file, lhs);
	  fprintf (dump_file, " = ");
	  print_generic_expr (dump_file, new_rhs);
	  fprintf (dump_file, "\n");
	}

      set_ssa_name_value (lhs, new_rhs);
    }

  /* A memory store, even an aliased store, creates a useful
     equivalence.  By exchanging the LHS and RHS, creating suitable
     vops and recording the result in the available expression table,
     we may be able to expose more redundant loads.  */
  if (!gimple_has_volatile_ops (stmt)
      && gimple_references_memory_p (stmt)
      && gimple_assign_single_p (stmt)
      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
      && !is_gimple_reg (lhs))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      gassign *new_stmt;

      /* Build a new statement with the RHS and LHS exchanged.  */
      if (TREE_CODE (rhs) == SSA_NAME)
        {
          /* NOTE tuples.  The call to gimple_build_assign below replaced
             a call to build_gimple_modify_stmt, which did not set the
             SSA_NAME_DEF_STMT on the LHS of the assignment.  Doing so
             may cause an SSA validation failure, as the LHS may be a
             default-initialized name and should have no definition.  I'm
             a bit dubious of this, as the artificial statement that we
             generate here may in fact be ill-formed, but it is simply
             used as an internal device in this pass, and never becomes
             part of the CFG.  */
	  gimple *defstmt = SSA_NAME_DEF_STMT (rhs);
          new_stmt = gimple_build_assign (rhs, lhs);
          SSA_NAME_DEF_STMT (rhs) = defstmt;
        }
      else
        new_stmt = gimple_build_assign (rhs, lhs);

      gimple_set_vuse (new_stmt, gimple_vdef (stmt));

      /* Finally enter the statement into the available expression
	 table.  */
      avail_exprs_stack->lookup_avail_expr (new_stmt, true, true);
    }
}

/* Replace *OP_P in STMT with any known equivalent value for *OP_P from
   CONST_AND_COPIES.  */

static void
cprop_operand (gimple *stmt, use_operand_p op_p, range_query *query)
{
  tree val;
  tree op = USE_FROM_PTR (op_p);

  /* If the operand has a known constant value or it is known to be a
     copy of some other variable, use the value or copy stored in
     CONST_AND_COPIES.  */
  val = SSA_NAME_VALUE (op);
  if (!val)
    {
      Value_Range r (TREE_TYPE (op));
      tree single;
      if (query->range_of_expr (r, op, stmt) && r.singleton_p (&single))
	val = single;
    }

  if (val && val != op)
    {
      /* Do not replace hard register operands in asm statements.  */
      if (gimple_code (stmt) == GIMPLE_ASM
	  && !may_propagate_copy_into_asm (op))
	return;

      /* Certain operands are not allowed to be copy propagated due
	 to their interaction with exception handling and some GCC
	 extensions.  */
      if (!may_propagate_copy (op, val))
	return;

      /* Do not propagate copies into BIVs.
         See PR23821 and PR62217 for how this can disturb IV and
	 number of iteration analysis.  */
      if (TREE_CODE (val) != INTEGER_CST)
	{
	  gimple *def = SSA_NAME_DEF_STMT (op);
	  if (gimple_code (def) == GIMPLE_PHI
	      && gimple_bb (def)->loop_father->header == gimple_bb (def))
	    return;
	}

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, op, dump_flags);
	  fprintf (dump_file, "' with %s '",
		   (TREE_CODE (val) != SSA_NAME ? "constant" : "variable"));
	  print_generic_expr (dump_file, val, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      if (TREE_CODE (val) != SSA_NAME)
	opt_stats.num_const_prop++;
      else
	opt_stats.num_copy_prop++;

      propagate_value (op_p, val);

      /* And note that we modified this statement.  This is now
	 safe, even if we changed virtual operands since we will
	 rescan the statement and rewrite its operands again.  */
      gimple_set_modified (stmt, true);
    }
}

/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).

   Propagate values from CONST_AND_COPIES into the uses, vuses and
   vdef_ops of STMT.  */

static void
cprop_into_stmt (gimple *stmt, range_query *query)
{
  use_operand_p op_p;
  ssa_op_iter iter;
  tree last_copy_propagated_op = NULL;

  FOR_EACH_SSA_USE_OPERAND (op_p, stmt, iter, SSA_OP_USE)
    {
      tree old_op = USE_FROM_PTR (op_p);

      /* If we have A = B and B = A in the copy propagation tables
	 (due to an equality comparison), avoid substituting B for A
	 then A for B in the trivially discovered cases.   This allows
	 optimization of statements were A and B appear as input
	 operands.  */
      if (old_op != last_copy_propagated_op)
	{
	  cprop_operand (stmt, op_p, query);

	  tree new_op = USE_FROM_PTR (op_p);
	  if (new_op != old_op && TREE_CODE (new_op) == SSA_NAME)
	    last_copy_propagated_op = new_op;
	}
    }
}

/* If STMT contains a relational test, try to convert it into an
   equality test if there is only a single value which can ever
   make the test true.

   For example, if the expression hash table contains:

    TRUE = (i <= 1)

   And we have a test within statement of i >= 1, then we can safely
   rewrite the test as i == 1 since there only a single value where
   the test is true.

   This is similar to code in VRP.  */

void
dom_opt_dom_walker::test_for_singularity (gimple *stmt,
					  avail_exprs_stack *avail_exprs_stack)
{
  /* We want to support gimple conditionals as well as assignments
     where the RHS contains a conditional.  */
  if (is_gimple_assign (stmt) || gimple_code (stmt) == GIMPLE_COND)
    {
      enum tree_code code = ERROR_MARK;
      tree lhs, rhs;

      /* Extract the condition of interest from both forms we support.  */
      if (is_gimple_assign (stmt))
	{
	  code = gimple_assign_rhs_code (stmt);
	  lhs = gimple_assign_rhs1 (stmt);
	  rhs = gimple_assign_rhs2 (stmt);
	}
      else if (gimple_code (stmt) == GIMPLE_COND)
	{
	  code = gimple_cond_code (as_a <gcond *> (stmt));
	  lhs = gimple_cond_lhs (as_a <gcond *> (stmt));
	  rhs = gimple_cond_rhs (as_a <gcond *> (stmt));
	}

      /* We're looking for a relational test using LE/GE.  Also note we can
	 canonicalize LT/GT tests against constants into LE/GT tests.  */
      if (code == LE_EXPR || code == GE_EXPR
	  || ((code == LT_EXPR || code == GT_EXPR)
	       && TREE_CODE (rhs) == INTEGER_CST))
	{
	  /* For LT_EXPR and GT_EXPR, canonicalize to LE_EXPR and GE_EXPR.  */
	  if (code == LT_EXPR)
	    rhs = fold_build2 (MINUS_EXPR, TREE_TYPE (rhs),
			       rhs, build_int_cst (TREE_TYPE (rhs), 1));

	  if (code == GT_EXPR)
	    rhs = fold_build2 (PLUS_EXPR, TREE_TYPE (rhs),
			       rhs, build_int_cst (TREE_TYPE (rhs), 1));

	  /* Determine the code we want to check for in the hash table.  */
	  enum tree_code test_code;
	  if (code == GE_EXPR || code == GT_EXPR)
	    test_code = LE_EXPR;
	  else
	    test_code = GE_EXPR;

	  /* Update the dummy statement so we can query the hash tables.  */
	  gimple_cond_set_code (m_dummy_cond, test_code);
	  gimple_cond_set_lhs (m_dummy_cond, lhs);
	  gimple_cond_set_rhs (m_dummy_cond, rhs);
	  tree cached_lhs
	    = avail_exprs_stack->lookup_avail_expr (m_dummy_cond,
						    false, false);

	  /* If the lookup returned 1 (true), then the expression we
	     queried was in the hash table.  As a result there is only
	     one value that makes the original conditional true.  Update
	     STMT accordingly.  */
	  if (cached_lhs && integer_onep (cached_lhs))
	    {
	      if (is_gimple_assign (stmt))
		{
		  gimple_assign_set_rhs_code (stmt, EQ_EXPR);
		  gimple_assign_set_rhs2 (stmt, rhs);
		  gimple_set_modified (stmt, true);
		}
	      else
		{
		  gimple_set_modified (stmt, true);
		  gimple_cond_set_code (as_a <gcond *> (stmt), EQ_EXPR);
		  gimple_cond_set_rhs (as_a <gcond *> (stmt), rhs);
		  gimple_set_modified (stmt, true);
		}
	    }
	}
    }
}

/* If STMT is a comparison of two uniform vectors reduce it to a comparison
   of scalar objects, otherwise leave STMT unchanged.  */

static void
reduce_vector_comparison_to_scalar_comparison (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_COND)
    {
      tree lhs = gimple_cond_lhs (stmt);
      tree rhs = gimple_cond_rhs (stmt);

      /* We may have a vector comparison where both arms are uniform
	 vectors.  If so, we can simplify the vector comparison down
	 to a scalar comparison.  */
      if (TREE_CODE (TREE_TYPE (lhs)) == VECTOR_TYPE
	  && TREE_CODE (TREE_TYPE (rhs)) == VECTOR_TYPE)
	{
	  /* If either operand is an SSA_NAME, then look back to its
	     defining statement to try and get at a suitable source.  */
	  if (TREE_CODE (rhs) == SSA_NAME)
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (rhs);
	      if (gimple_assign_single_p (def_stmt))
		rhs = gimple_assign_rhs1 (def_stmt);
	    }

	  if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (lhs);
	      if (gimple_assign_single_p (def_stmt))
		lhs = gimple_assign_rhs1 (def_stmt);
	    }

	  /* Now see if they are both uniform vectors and if so replace
	     the vector comparison with a scalar comparison.  */
	  tree rhs_elem = rhs ? uniform_vector_p (rhs) : NULL_TREE;
	  tree lhs_elem = lhs ? uniform_vector_p (lhs) : NULL_TREE;
	  if (rhs_elem && lhs_elem)
	    {
	      if (dump_file && dump_flags & TDF_DETAILS)
		{
		  fprintf (dump_file, "Reducing vector comparison: ");
		  print_gimple_stmt (dump_file, stmt, 0);
		}

	      gimple_cond_set_rhs (as_a <gcond *>(stmt), rhs_elem);
	      gimple_cond_set_lhs (as_a <gcond *>(stmt), lhs_elem);
	      gimple_set_modified (stmt, true);

	      if (dump_file && dump_flags & TDF_DETAILS)
		{
		  fprintf (dump_file, "To scalar equivalent: ");
		  print_gimple_stmt (dump_file, stmt, 0);
		  fprintf (dump_file, "\n");
		}
	    }
	}
    }
}

/* If possible, rewrite the conditional as TRUE or FALSE, and return
   the taken edge.  Otherwise, return NULL.  */

edge
dom_opt_dom_walker::fold_cond (gcond *cond)
{
  simplify_using_ranges simplify (m_ranger);
  if (simplify.fold_cond (cond))
    {
      basic_block bb = gimple_bb (cond);
      if (gimple_cond_true_p (cond))
	return find_taken_edge (bb, boolean_true_node);
      if (gimple_cond_false_p (cond))
	return find_taken_edge (bb, boolean_false_node);
    }
  return NULL;
}

/* Optimize the statement in block BB pointed to by iterator SI.

   We try to perform some simplistic global redundancy elimination and
   constant propagation:

   1- To detect global redundancy, we keep track of expressions that have
      been computed in this block and its dominators.  If we find that the
      same expression is computed more than once, we eliminate repeated
      computations by using the target of the first one.

   2- Constant values and copy assignments.  This is used to do very
      simplistic constant and copy propagation.  When a constant or copy
      assignment is found, we map the value on the RHS of the assignment to
      the variable in the LHS in the CONST_AND_COPIES table.

   3- Very simple redundant store elimination is performed.

   4- We can simplify a condition to a constant or from a relational
      condition to an equality condition.  */

edge
dom_opt_dom_walker::optimize_stmt (basic_block bb, gimple_stmt_iterator *si,
				   bool *removed_p)
{
  gimple *stmt, *old_stmt;
  bool may_optimize_p;
  bool modified_p = false;
  bool was_noreturn;
  edge retval = NULL;

  old_stmt = stmt = gsi_stmt (*si);
  was_noreturn = is_gimple_call (stmt) && gimple_call_noreturn_p (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Optimizing statement ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  /* STMT may be a comparison of uniform vectors that we can simplify
     down to a comparison of scalars.  Do that transformation first
     so that all the scalar optimizations from here onward apply.  */
  reduce_vector_comparison_to_scalar_comparison (stmt);

  update_stmt_if_modified (stmt);
  opt_stats.num_stmts++;

  /* Const/copy propagate into USES, VUSES and the RHS of VDEFs.  */
  cprop_into_stmt (stmt, m_ranger);

  /* If the statement has been modified with constant replacements,
     fold its RHS before checking for redundant computations.  */
  if (gimple_modified_p (stmt))
    {
      tree rhs = NULL;

      /* Try to fold the statement making sure that STMT is kept
	 up to date.  */
      if (fold_stmt (si))
	{
	  stmt = gsi_stmt (*si);
	  gimple_set_modified (stmt, true);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Folded to: ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	}

      /* We only need to consider cases that can yield a gimple operand.  */
      if (gimple_assign_single_p (stmt))
        rhs = gimple_assign_rhs1 (stmt);
      else if (gimple_code (stmt) == GIMPLE_GOTO)
        rhs = gimple_goto_dest (stmt);
      else if (gswitch *swtch_stmt = dyn_cast <gswitch *> (stmt))
        /* This should never be an ADDR_EXPR.  */
        rhs = gimple_switch_index (swtch_stmt);

      if (rhs && TREE_CODE (rhs) == ADDR_EXPR)
        recompute_tree_invariant_for_addr_expr (rhs);

      /* Indicate that maybe_clean_or_replace_eh_stmt needs to be called,
	 even if fold_stmt updated the stmt already and thus cleared
	 gimple_modified_p flag on it.  */
      modified_p = true;
    }

  /* Check for redundant computations.  Do this optimization only
     for assignments that have no volatile ops and conditionals.  */
  may_optimize_p = (!gimple_has_side_effects (stmt)
                    && (is_gimple_assign (stmt)
                        || (is_gimple_call (stmt)
                            && gimple_call_lhs (stmt) != NULL_TREE)
                        || gimple_code (stmt) == GIMPLE_COND
                        || gimple_code (stmt) == GIMPLE_SWITCH));

  if (may_optimize_p)
    {
      if (gimple_code (stmt) == GIMPLE_CALL)
	{
	  /* Resolve __builtin_constant_p.  If it hasn't been
	     folded to integer_one_node by now, it's fairly
	     certain that the value simply isn't constant.  */
	  tree callee = gimple_call_fndecl (stmt);
	  if (callee
	      && fndecl_built_in_p (callee, BUILT_IN_CONSTANT_P))
	    {
	      propagate_tree_value_into_stmt (si, integer_zero_node);
	      stmt = gsi_stmt (*si);
	    }
	}

      if (gimple_code (stmt) == GIMPLE_COND)
	{
	  tree lhs = gimple_cond_lhs (stmt);
	  tree rhs = gimple_cond_rhs (stmt);

	  /* If the LHS has a range [0..1] and the RHS has a range ~[0..1],
	     then this conditional is computable at compile time.  We can just
	     shove either 0 or 1 into the LHS, mark the statement as modified
	     and all the right things will just happen below.

	     Note this would apply to any case where LHS has a range
	     narrower than its type implies and RHS is outside that
	     narrower range.  Future work.  */
	  if (TREE_CODE (lhs) == SSA_NAME
	      && ssa_name_has_boolean_range (lhs)
	      && TREE_CODE (rhs) == INTEGER_CST
	      && ! (integer_zerop (rhs) || integer_onep (rhs)))
	    {
	      gimple_cond_set_lhs (as_a <gcond *> (stmt),
				   fold_convert (TREE_TYPE (lhs),
						 integer_zero_node));
	      gimple_set_modified (stmt, true);
	    }
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      /* Exploiting EVRP data is not yet fully integrated into DOM
		 but we need to do something for this case to avoid regressing
		 udr4.f90 and new1.C which have unexecutable blocks with
		 undefined behavior that get diagnosed if they're left in the
		 IL because we've attached range information to new
		 SSA_NAMES.  */
	      update_stmt_if_modified (stmt);
	      edge taken_edge = fold_cond (as_a <gcond *> (stmt));
	      if (taken_edge)
		{
		  gimple_set_modified (stmt, true);
		  update_stmt (stmt);
		  cfg_altered = true;
		  return taken_edge;
		}
	    }
	}

      update_stmt_if_modified (stmt);
      eliminate_redundant_computations (si, m_const_and_copies,
					m_avail_exprs_stack);
      stmt = gsi_stmt (*si);

      /* Perform simple redundant store elimination.  */
      if (gimple_assign_single_p (stmt)
	  && TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  tree rhs = gimple_assign_rhs1 (stmt);
	  tree cached_lhs;
	  gassign *new_stmt;
	  rhs = dom_valueize (rhs);
	  /* Build a new statement with the RHS and LHS exchanged.  */
	  if (TREE_CODE (rhs) == SSA_NAME)
	    {
	      gimple *defstmt = SSA_NAME_DEF_STMT (rhs);
	      new_stmt = gimple_build_assign (rhs, lhs);
	      SSA_NAME_DEF_STMT (rhs) = defstmt;
	    }
	  else
	    new_stmt = gimple_build_assign (rhs, lhs);
	  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
	  expr_hash_elt *elt = NULL;
	  cached_lhs = m_avail_exprs_stack->lookup_avail_expr (new_stmt, false,
							       false, &elt);
	  if (cached_lhs
	      && operand_equal_p (rhs, cached_lhs, 0)
	      && refs_same_for_tbaa_p (elt->expr ()->kind == EXPR_SINGLE
				       ? elt->expr ()->ops.single.rhs
				       : NULL_TREE, lhs))
	    {
	      basic_block bb = gimple_bb (stmt);
	      unlink_stmt_vdef (stmt);
	      if (gsi_remove (si, true))
		{
		  bitmap_set_bit (need_eh_cleanup, bb->index);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  Flagged to clear EH edges.\n");
		}
	      release_defs (stmt);
	      *removed_p = true;
	      return retval;
	    }
	}

      /* If this statement was not redundant, we may still be able to simplify
	 it, which may in turn allow other part of DOM or other passes to do
	 a better job.  */
      test_for_singularity (stmt, m_avail_exprs_stack);
    }

  /* Record any additional equivalences created by this statement.  */
  if (is_gimple_assign (stmt))
    record_equivalences_from_stmt (stmt, may_optimize_p, m_avail_exprs_stack);

  /* If STMT is a COND_EXPR or SWITCH_EXPR and it was modified, then we may
     know where it goes.  */
  if (gimple_modified_p (stmt) || modified_p)
    {
      tree val = NULL;

      if (gimple_code (stmt) == GIMPLE_COND)
        val = fold_binary_loc (gimple_location (stmt),
			       gimple_cond_code (stmt), boolean_type_node,
			       gimple_cond_lhs (stmt),
			       gimple_cond_rhs (stmt));
      else if (gswitch *swtch_stmt = dyn_cast <gswitch *> (stmt))
	val = gimple_switch_index (swtch_stmt);

      if (val && TREE_CODE (val) == INTEGER_CST)
	{
	  retval = find_taken_edge (bb, val);
	  if (retval)
	    {
	      /* Fix the condition to be either true or false.  */
	      if (gimple_code (stmt) == GIMPLE_COND)
		{
		  if (integer_zerop (val))
		    gimple_cond_make_false (as_a <gcond *> (stmt));
		  else if (integer_onep (val))
		    gimple_cond_make_true (as_a <gcond *> (stmt));
		  else
		    gcc_unreachable ();

		  gimple_set_modified (stmt, true);
		}

	      /* Further simplifications may be possible.  */
	      cfg_altered = true;
	    }
	}

      update_stmt_if_modified (stmt);

      /* If we simplified a statement in such a way as to be shown that it
	 cannot trap, update the eh information and the cfg to match.  */
      if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	{
	  bitmap_set_bit (need_eh_cleanup, bb->index);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Flagged to clear EH edges.\n");
	}

      if (!was_noreturn
	  && is_gimple_call (stmt) && gimple_call_noreturn_p (stmt))
	need_noreturn_fixup.safe_push (stmt);
    }
  return retval;
}
