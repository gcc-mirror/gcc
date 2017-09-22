/* If-conversion for vectorizer.
   Copyright (C) 2004-2017 Free Software Foundation, Inc.
   Contributed by Devang Patel <dpatel@apple.com>

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

/* This pass implements a tree level if-conversion of loops.  Its
   initial goal is to help the vectorizer to vectorize loops with
   conditions.

   A short description of if-conversion:

     o Decide if a loop is if-convertible or not.
     o Walk all loop basic blocks in breadth first order (BFS order).
       o Remove conditional statements (at the end of basic block)
         and propagate condition into destination basic blocks'
	 predicate list.
       o Replace modify expression with conditional modify expression
         using current basic block's condition.
     o Merge all basic blocks
       o Replace phi nodes with conditional modify expr
       o Merge all basic blocks into header

     Sample transformation:

     INPUT
     -----

     # i_23 = PHI <0(0), i_18(10)>;
     <L0>:;
     j_15 = A[i_23];
     if (j_15 > 41) goto <L1>; else goto <L17>;

     <L17>:;
     goto <bb 3> (<L3>);

     <L1>:;

     # iftmp.2_4 = PHI <0(8), 42(2)>;
     <L3>:;
     A[i_23] = iftmp.2_4;
     i_18 = i_23 + 1;
     if (i_18 <= 15) goto <L19>; else goto <L18>;

     <L19>:;
     goto <bb 1> (<L0>);

     <L18>:;

     OUTPUT
     ------

     # i_23 = PHI <0(0), i_18(10)>;
     <L0>:;
     j_15 = A[i_23];

     <L3>:;
     iftmp.2_4 = j_15 > 41 ? 42 : 0;
     A[i_23] = iftmp.2_4;
     i_18 = i_23 + 1;
     if (i_18 <= 15) goto <L19>; else goto <L18>;

     <L19>:;
     goto <bb 1> (<L0>);

     <L18>:;
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs-query.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-address.h"
#include "dbgcnt.h"
#include "tree-hash-traits.h"
#include "varasm.h"
#include "builtins.h"
#include "params.h"
#include "cfganal.h"

/* Only handle PHIs with no more arguments unless we are asked to by
   simd pragma.  */
#define MAX_PHI_ARG_NUM \
  ((unsigned) PARAM_VALUE (PARAM_MAX_TREE_IF_CONVERSION_PHI_ARGS))

/* Indicate if new load/store that needs to be predicated is introduced
   during if conversion.  */
static bool any_pred_load_store;

/* Indicate if there are any complicated PHIs that need to be handled in
   if-conversion.  Complicated PHI has more than two arguments and can't
   be degenerated to two arguments PHI.  See more information in comment
   before phi_convertible_by_degenerating_args.  */
static bool any_complicated_phi;

/* Hash for struct innermost_loop_behavior.  It depends on the user to
   free the memory.  */

struct innermost_loop_behavior_hash : nofree_ptr_hash <innermost_loop_behavior>
{
  static inline hashval_t hash (const value_type &);
  static inline bool equal (const value_type &,
			    const compare_type &);
};

inline hashval_t
innermost_loop_behavior_hash::hash (const value_type &e)
{
  hashval_t hash;

  hash = iterative_hash_expr (e->base_address, 0);
  hash = iterative_hash_expr (e->offset, hash);
  hash = iterative_hash_expr (e->init, hash);
  return iterative_hash_expr (e->step, hash);
}

inline bool
innermost_loop_behavior_hash::equal (const value_type &e1,
				     const compare_type &e2)
{
  if ((e1->base_address && !e2->base_address)
      || (!e1->base_address && e2->base_address)
      || (!e1->offset && e2->offset)
      || (e1->offset && !e2->offset)
      || (!e1->init && e2->init)
      || (e1->init && !e2->init)
      || (!e1->step && e2->step)
      || (e1->step && !e2->step))
    return false;

  if (e1->base_address && e2->base_address
      && !operand_equal_p (e1->base_address, e2->base_address, 0))
    return false;
  if (e1->offset && e2->offset
      && !operand_equal_p (e1->offset, e2->offset, 0))
    return false;
  if (e1->init && e2->init
      && !operand_equal_p (e1->init, e2->init, 0))
    return false;
  if (e1->step && e2->step
      && !operand_equal_p (e1->step, e2->step, 0))
    return false;

  return true;
}

/* List of basic blocks in if-conversion-suitable order.  */
static basic_block *ifc_bbs;

/* Hash table to store <DR's innermost loop behavior, DR> pairs.  */
static hash_map<innermost_loop_behavior_hash,
		data_reference_p> *innermost_DR_map;

/* Hash table to store <base reference, DR> pairs.  */
static hash_map<tree_operand_hash, data_reference_p> *baseref_DR_map;

/* Structure used to predicate basic blocks.  This is attached to the
   ->aux field of the BBs in the loop to be if-converted.  */
struct bb_predicate {

  /* The condition under which this basic block is executed.  */
  tree predicate;

  /* PREDICATE is gimplified, and the sequence of statements is
     recorded here, in order to avoid the duplication of computations
     that occur in previous conditions.  See PR44483.  */
  gimple_seq predicate_gimplified_stmts;
};

/* Returns true when the basic block BB has a predicate.  */

static inline bool
bb_has_predicate (basic_block bb)
{
  return bb->aux != NULL;
}

/* Returns the gimplified predicate for basic block BB.  */

static inline tree
bb_predicate (basic_block bb)
{
  return ((struct bb_predicate *) bb->aux)->predicate;
}

/* Sets the gimplified predicate COND for basic block BB.  */

static inline void
set_bb_predicate (basic_block bb, tree cond)
{
  gcc_assert ((TREE_CODE (cond) == TRUTH_NOT_EXPR
	       && is_gimple_condexpr (TREE_OPERAND (cond, 0)))
	      || is_gimple_condexpr (cond));
  ((struct bb_predicate *) bb->aux)->predicate = cond;
}

/* Returns the sequence of statements of the gimplification of the
   predicate for basic block BB.  */

static inline gimple_seq
bb_predicate_gimplified_stmts (basic_block bb)
{
  return ((struct bb_predicate *) bb->aux)->predicate_gimplified_stmts;
}

/* Sets the sequence of statements STMTS of the gimplification of the
   predicate for basic block BB.  */

static inline void
set_bb_predicate_gimplified_stmts (basic_block bb, gimple_seq stmts)
{
  ((struct bb_predicate *) bb->aux)->predicate_gimplified_stmts = stmts;
}

/* Adds the sequence of statements STMTS to the sequence of statements
   of the predicate for basic block BB.  */

static inline void
add_bb_predicate_gimplified_stmts (basic_block bb, gimple_seq stmts)
{
  gimple_seq_add_seq_without_update
    (&(((struct bb_predicate *) bb->aux)->predicate_gimplified_stmts), stmts);
}

/* Initializes to TRUE the predicate of basic block BB.  */

static inline void
init_bb_predicate (basic_block bb)
{
  bb->aux = XNEW (struct bb_predicate);
  set_bb_predicate_gimplified_stmts (bb, NULL);
  set_bb_predicate (bb, boolean_true_node);
}

/* Release the SSA_NAMEs associated with the predicate of basic block BB,
   but don't actually free it.  */

static inline void
release_bb_predicate (basic_block bb)
{
  gimple_seq stmts = bb_predicate_gimplified_stmts (bb);
  if (stmts)
    {
      if (flag_checking)
	for (gimple_stmt_iterator i = gsi_start (stmts);
	     !gsi_end_p (i); gsi_next (&i))
	  gcc_assert (! gimple_use_ops (gsi_stmt (i)));

      set_bb_predicate_gimplified_stmts (bb, NULL);
    }
}

/* Free the predicate of basic block BB.  */

static inline void
free_bb_predicate (basic_block bb)
{
  if (!bb_has_predicate (bb))
    return;

  release_bb_predicate (bb);
  free (bb->aux);
  bb->aux = NULL;
}

/* Reinitialize predicate of BB with the true predicate.  */

static inline void
reset_bb_predicate (basic_block bb)
{
  if (!bb_has_predicate (bb))
    init_bb_predicate (bb);
  else
    {
      release_bb_predicate (bb);
      set_bb_predicate (bb, boolean_true_node);
    }
}

/* Returns a new SSA_NAME of type TYPE that is assigned the value of
   the expression EXPR.  Inserts the statement created for this
   computation before GSI and leaves the iterator GSI at the same
   statement.  */

static tree
ifc_temp_var (tree type, tree expr, gimple_stmt_iterator *gsi)
{
  tree new_name = make_temp_ssa_name (type, NULL, "_ifc_");
  gimple *stmt = gimple_build_assign (new_name, expr);
  gimple_set_vuse (stmt, gimple_vuse (gsi_stmt (*gsi)));
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  return new_name;
}

/* Return true when COND is a false predicate.  */

static inline bool
is_false_predicate (tree cond)
{
  return (cond != NULL_TREE
	  && (cond == boolean_false_node
	      || integer_zerop (cond)));
}

/* Return true when COND is a true predicate.  */

static inline bool
is_true_predicate (tree cond)
{
  return (cond == NULL_TREE
	  || cond == boolean_true_node
	  || integer_onep (cond));
}

/* Returns true when BB has a predicate that is not trivial: true or
   NULL_TREE.  */

static inline bool
is_predicated (basic_block bb)
{
  return !is_true_predicate (bb_predicate (bb));
}

/* Parses the predicate COND and returns its comparison code and
   operands OP0 and OP1.  */

static enum tree_code
parse_predicate (tree cond, tree *op0, tree *op1)
{
  gimple *s;

  if (TREE_CODE (cond) == SSA_NAME
      && is_gimple_assign (s = SSA_NAME_DEF_STMT (cond)))
    {
      if (TREE_CODE_CLASS (gimple_assign_rhs_code (s)) == tcc_comparison)
	{
	  *op0 = gimple_assign_rhs1 (s);
	  *op1 = gimple_assign_rhs2 (s);
	  return gimple_assign_rhs_code (s);
	}

      else if (gimple_assign_rhs_code (s) == TRUTH_NOT_EXPR)
	{
	  tree op = gimple_assign_rhs1 (s);
	  tree type = TREE_TYPE (op);
	  enum tree_code code = parse_predicate (op, op0, op1);

	  return code == ERROR_MARK ? ERROR_MARK
	    : invert_tree_comparison (code, HONOR_NANS (type));
	}

      return ERROR_MARK;
    }

  if (COMPARISON_CLASS_P (cond))
    {
      *op0 = TREE_OPERAND (cond, 0);
      *op1 = TREE_OPERAND (cond, 1);
      return TREE_CODE (cond);
    }

  return ERROR_MARK;
}

/* Returns the fold of predicate C1 OR C2 at location LOC.  */

static tree
fold_or_predicates (location_t loc, tree c1, tree c2)
{
  tree op1a, op1b, op2a, op2b;
  enum tree_code code1 = parse_predicate (c1, &op1a, &op1b);
  enum tree_code code2 = parse_predicate (c2, &op2a, &op2b);

  if (code1 != ERROR_MARK && code2 != ERROR_MARK)
    {
      tree t = maybe_fold_or_comparisons (code1, op1a, op1b,
					  code2, op2a, op2b);
      if (t)
	return t;
    }

  return fold_build2_loc (loc, TRUTH_OR_EXPR, boolean_type_node, c1, c2);
}

/* Returns either a COND_EXPR or the folded expression if the folded
   expression is a MIN_EXPR, a MAX_EXPR, an ABS_EXPR,
   a constant or a SSA_NAME. */

static tree
fold_build_cond_expr (tree type, tree cond, tree rhs, tree lhs)
{
  tree rhs1, lhs1, cond_expr;

  /* If COND is comparison r != 0 and r has boolean type, convert COND
     to SSA_NAME to accept by vect bool pattern.  */
  if (TREE_CODE (cond) == NE_EXPR)
    {
      tree op0 = TREE_OPERAND (cond, 0);
      tree op1 = TREE_OPERAND (cond, 1);
      if (TREE_CODE (op0) == SSA_NAME
	  && TREE_CODE (TREE_TYPE (op0)) == BOOLEAN_TYPE
	  && (integer_zerop (op1)))
	cond = op0;
    }
  cond_expr = fold_ternary (COND_EXPR, type, cond, rhs, lhs);

  if (cond_expr == NULL_TREE)
    return build3 (COND_EXPR, type, cond, rhs, lhs);

  STRIP_USELESS_TYPE_CONVERSION (cond_expr);

  if (is_gimple_val (cond_expr))
    return cond_expr;

  if (TREE_CODE (cond_expr) == ABS_EXPR)
    {
      rhs1 = TREE_OPERAND (cond_expr, 1);
      STRIP_USELESS_TYPE_CONVERSION (rhs1);
      if (is_gimple_val (rhs1))
	return build1 (ABS_EXPR, type, rhs1);
    }

  if (TREE_CODE (cond_expr) == MIN_EXPR
      || TREE_CODE (cond_expr) == MAX_EXPR)
    {
      lhs1 = TREE_OPERAND (cond_expr, 0);
      STRIP_USELESS_TYPE_CONVERSION (lhs1);
      rhs1 = TREE_OPERAND (cond_expr, 1);
      STRIP_USELESS_TYPE_CONVERSION (rhs1);
      if (is_gimple_val (rhs1) && is_gimple_val (lhs1))
	return build2 (TREE_CODE (cond_expr), type, lhs1, rhs1);
    }
  return build3 (COND_EXPR, type, cond, rhs, lhs);
}

/* Add condition NC to the predicate list of basic block BB.  LOOP is
   the loop to be if-converted. Use predicate of cd-equivalent block
   for join bb if it exists: we call basic blocks bb1 and bb2 
   cd-equivalent if they are executed under the same condition.  */

static inline void
add_to_predicate_list (struct loop *loop, basic_block bb, tree nc)
{
  tree bc, *tp;
  basic_block dom_bb;

  if (is_true_predicate (nc))
    return;

  /* If dominance tells us this basic block is always executed,
     don't record any predicates for it.  */
  if (dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
    return;

  dom_bb = get_immediate_dominator (CDI_DOMINATORS, bb);
  /* We use notion of cd equivalence to get simpler predicate for
     join block, e.g. if join block has 2 predecessors with predicates
     p1 & p2 and p1 & !p2, we'd like to get p1 for it instead of
     p1 & p2 | p1 & !p2.  */
  if (dom_bb != loop->header
      && get_immediate_dominator (CDI_POST_DOMINATORS, dom_bb) == bb)
    {
      gcc_assert (flow_bb_inside_loop_p (loop, dom_bb));
      bc = bb_predicate (dom_bb);
      if (!is_true_predicate (bc))
	set_bb_predicate (bb, bc);
      else
	gcc_assert (is_true_predicate (bb_predicate (bb)));
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Use predicate of bb#%d for bb#%d\n",
		 dom_bb->index, bb->index);
      return;
    }

  if (!is_predicated (bb))
    bc = nc;
  else
    {
      bc = bb_predicate (bb);
      bc = fold_or_predicates (EXPR_LOCATION (bc), nc, bc);
      if (is_true_predicate (bc))
	{
	  reset_bb_predicate (bb);
	  return;
	}
    }

  /* Allow a TRUTH_NOT_EXPR around the main predicate.  */
  if (TREE_CODE (bc) == TRUTH_NOT_EXPR)
    tp = &TREE_OPERAND (bc, 0);
  else
    tp = &bc;
  if (!is_gimple_condexpr (*tp))
    {
      gimple_seq stmts;
      *tp = force_gimple_operand_1 (*tp, &stmts, is_gimple_condexpr, NULL_TREE);
      add_bb_predicate_gimplified_stmts (bb, stmts);
    }
  set_bb_predicate (bb, bc);
}

/* Add the condition COND to the previous condition PREV_COND, and add
   this to the predicate list of the destination of edge E.  LOOP is
   the loop to be if-converted.  */

static void
add_to_dst_predicate_list (struct loop *loop, edge e,
			   tree prev_cond, tree cond)
{
  if (!flow_bb_inside_loop_p (loop, e->dest))
    return;

  if (!is_true_predicate (prev_cond))
    cond = fold_build2 (TRUTH_AND_EXPR, boolean_type_node,
			prev_cond, cond);

  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, e->dest))
    add_to_predicate_list (loop, e->dest, cond);
}

/* Return true if one of the successor edges of BB exits LOOP.  */

static bool
bb_with_exit_edge_p (struct loop *loop, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (loop_exit_edge_p (loop, e))
      return true;

  return false;
}

/* Given PHI which has more than two arguments, this function checks if
   it's if-convertible by degenerating its arguments.  Specifically, if
   below two conditions are satisfied:

     1) Number of PHI arguments with different values equals to 2 and one
	argument has the only occurrence.
     2) The edge corresponding to the unique argument isn't critical edge.

   Such PHI can be handled as PHIs have only two arguments.  For example,
   below PHI:

     res = PHI <A_1(e1), A_1(e2), A_2(e3)>;

   can be transformed into:

     res = (predicate of e3) ? A_2 : A_1;

   Return TRUE if it is the case, FALSE otherwise.  */

static bool
phi_convertible_by_degenerating_args (gphi *phi)
{
  edge e;
  tree arg, t1 = NULL, t2 = NULL;
  unsigned int i, i1 = 0, i2 = 0, n1 = 0, n2 = 0;
  unsigned int num_args = gimple_phi_num_args (phi);

  gcc_assert (num_args > 2);

  for (i = 0; i < num_args; i++)
    {
      arg = gimple_phi_arg_def (phi, i);
      if (t1 == NULL || operand_equal_p (t1, arg, 0))
	{
	  n1++;
	  i1 = i;
	  t1 = arg;
	}
      else if (t2 == NULL || operand_equal_p (t2, arg, 0))
	{
	  n2++;
	  i2 = i;
	  t2 = arg;
	}
      else
	return false;
    }

  if (n1 != 1 && n2 != 1)
    return false;

  /* Check if the edge corresponding to the unique arg is critical.  */
  e = gimple_phi_arg_edge (phi, (n1 == 1) ? i1 : i2);
  if (EDGE_COUNT (e->src->succs) > 1)
    return false;

  return true;
}

/* Return true when PHI is if-convertible.  PHI is part of loop LOOP
   and it belongs to basic block BB.  Note at this point, it is sure
   that PHI is if-convertible.  This function updates global variable
   ANY_COMPLICATED_PHI if PHI is complicated.  */

static bool
if_convertible_phi_p (struct loop *loop, basic_block bb, gphi *phi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "-------------------------\n");
      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
    }

  if (bb != loop->header
      && gimple_phi_num_args (phi) > 2
      && !phi_convertible_by_degenerating_args (phi))
    any_complicated_phi = true;

  return true;
}

/* Records the status of a data reference.  This struct is attached to
   each DR->aux field.  */

struct ifc_dr {
  bool rw_unconditionally;
  bool w_unconditionally;
  bool written_at_least_once;

  tree rw_predicate;
  tree w_predicate;
  tree base_w_predicate;
};

#define IFC_DR(DR) ((struct ifc_dr *) (DR)->aux)
#define DR_BASE_W_UNCONDITIONALLY(DR) (IFC_DR (DR)->written_at_least_once)
#define DR_RW_UNCONDITIONALLY(DR) (IFC_DR (DR)->rw_unconditionally)
#define DR_W_UNCONDITIONALLY(DR) (IFC_DR (DR)->w_unconditionally)

/* Iterates over DR's and stores refs, DR and base refs, DR pairs in
   HASH tables.  While storing them in HASH table, it checks if the
   reference is unconditionally read or written and stores that as a flag
   information.  For base reference it checks if it is written atlest once
   unconditionally and stores it as flag information along with DR.
   In other words for every data reference A in STMT there exist other
   accesses to a data reference with the same base with predicates that
   add up (OR-up) to the true predicate: this ensures that the data
   reference A is touched (read or written) on every iteration of the
   if-converted loop.  */
static void
hash_memrefs_baserefs_and_store_DRs_read_written_info (data_reference_p a)
{

  data_reference_p *master_dr, *base_master_dr;
  tree base_ref = DR_BASE_OBJECT (a);
  innermost_loop_behavior *innermost = &DR_INNERMOST (a);
  tree ca = bb_predicate (gimple_bb (DR_STMT (a)));
  bool exist1, exist2;

  master_dr = &innermost_DR_map->get_or_insert (innermost, &exist1);
  if (!exist1)
    *master_dr = a;

  if (DR_IS_WRITE (a))
    {
      IFC_DR (*master_dr)->w_predicate
	= fold_or_predicates (UNKNOWN_LOCATION, ca,
			      IFC_DR (*master_dr)->w_predicate);
      if (is_true_predicate (IFC_DR (*master_dr)->w_predicate))
	DR_W_UNCONDITIONALLY (*master_dr) = true;
    }
  IFC_DR (*master_dr)->rw_predicate
    = fold_or_predicates (UNKNOWN_LOCATION, ca,
			  IFC_DR (*master_dr)->rw_predicate);
  if (is_true_predicate (IFC_DR (*master_dr)->rw_predicate))
    DR_RW_UNCONDITIONALLY (*master_dr) = true;

  if (DR_IS_WRITE (a))
    {
      base_master_dr = &baseref_DR_map->get_or_insert (base_ref, &exist2);
      if (!exist2)
	*base_master_dr = a;
      IFC_DR (*base_master_dr)->base_w_predicate
	= fold_or_predicates (UNKNOWN_LOCATION, ca,
			      IFC_DR (*base_master_dr)->base_w_predicate);
      if (is_true_predicate (IFC_DR (*base_master_dr)->base_w_predicate))
	DR_BASE_W_UNCONDITIONALLY (*base_master_dr) = true;
    }
}

/* Return TRUE if can prove the index IDX of an array reference REF is
   within array bound.  Return false otherwise.  */

static bool
idx_within_array_bound (tree ref, tree *idx, void *dta)
{
  bool overflow;
  widest_int niter, valid_niter, delta, wi_step;
  tree ev, init, step;
  tree low, high;
  struct loop *loop = (struct loop*) dta;

  /* Only support within-bound access for array references.  */
  if (TREE_CODE (ref) != ARRAY_REF)
    return false;

  /* For arrays at the end of the structure, we are not guaranteed that they
     do not really extend over their declared size.  However, for arrays of
     size greater than one, this is unlikely to be intended.  */
  if (array_at_struct_end_p (ref))
    return false;

  ev = analyze_scalar_evolution (loop, *idx);
  ev = instantiate_parameters (loop, ev);
  init = initial_condition (ev);
  step = evolution_part_in_loop_num (ev, loop->num);

  if (!init || TREE_CODE (init) != INTEGER_CST
      || (step && TREE_CODE (step) != INTEGER_CST))
    return false;

  low = array_ref_low_bound (ref);
  high = array_ref_up_bound (ref);

  /* The case of nonconstant bounds could be handled, but it would be
     complicated.  */
  if (TREE_CODE (low) != INTEGER_CST
      || !high || TREE_CODE (high) != INTEGER_CST)
    return false;

  /* Check if the intial idx is within bound.  */
  if (wi::to_widest (init) < wi::to_widest (low)
      || wi::to_widest (init) > wi::to_widest (high))
    return false;

  /* The idx is always within bound.  */
  if (!step || integer_zerop (step))
    return true;

  if (!max_loop_iterations (loop, &niter))
    return false;

  if (wi::to_widest (step) < 0)
    {
      delta = wi::to_widest (init) - wi::to_widest (low);
      wi_step = -wi::to_widest (step);
    }
  else
    {
      delta = wi::to_widest (high) - wi::to_widest (init);
      wi_step = wi::to_widest (step);
    }

  valid_niter = wi::div_floor (delta, wi_step, SIGNED, &overflow);
  /* The iteration space of idx is within array bound.  */
  if (!overflow && niter <= valid_niter)
    return true;

  return false;
}

/* Return TRUE if ref is a within bound array reference.  */

static bool
ref_within_array_bound (gimple *stmt, tree ref)
{
  struct loop *loop = loop_containing_stmt (stmt);

  gcc_assert (loop != NULL);
  return for_each_index (&ref, idx_within_array_bound, loop);
}


/* Given a memory reference expression T, return TRUE if base object
   it refers to is writable.  The base object of a memory reference
   is the main object being referenced, which is returned by function
   get_base_address.  */

static bool
base_object_writable (tree ref)
{
  tree base_tree = get_base_address (ref);

  return (base_tree
	  && DECL_P (base_tree)
	  && decl_binds_to_current_def_p (base_tree)
	  && !TREE_READONLY (base_tree));
}

/* Return true when the memory references of STMT won't trap in the
   if-converted code.  There are two things that we have to check for:

   - writes to memory occur to writable memory: if-conversion of
   memory writes transforms the conditional memory writes into
   unconditional writes, i.e. "if (cond) A[i] = foo" is transformed
   into "A[i] = cond ? foo : A[i]", and as the write to memory may not
   be executed at all in the original code, it may be a readonly
   memory.  To check that A is not const-qualified, we check that
   there exists at least an unconditional write to A in the current
   function.

   - reads or writes to memory are valid memory accesses for every
   iteration.  To check that the memory accesses are correctly formed
   and that we are allowed to read and write in these locations, we
   check that the memory accesses to be if-converted occur at every
   iteration unconditionally.

   Returns true for the memory reference in STMT, same memory reference
   is read or written unconditionally atleast once and the base memory
   reference is written unconditionally once.  This is to check reference
   will not write fault.  Also retuns true if the memory reference is
   unconditionally read once then we are conditionally writing to memory
   which is defined as read and write and is bound to the definition
   we are seeing.  */
static bool
ifcvt_memrefs_wont_trap (gimple *stmt, vec<data_reference_p> drs)
{
  data_reference_p *master_dr, *base_master_dr;
  data_reference_p a = drs[gimple_uid (stmt) - 1];

  tree base = DR_BASE_OBJECT (a);
  innermost_loop_behavior *innermost = &DR_INNERMOST (a);

  gcc_assert (DR_STMT (a) == stmt);
  gcc_assert (DR_BASE_ADDRESS (a) || DR_OFFSET (a)
              || DR_INIT (a) || DR_STEP (a));

  master_dr = innermost_DR_map->get (innermost);
  gcc_assert (master_dr != NULL);

  base_master_dr = baseref_DR_map->get (base);

  /* If a is unconditionally written to it doesn't trap.  */
  if (DR_W_UNCONDITIONALLY (*master_dr))
    return true;

  /* If a is unconditionally accessed then ...

     Even a is conditional access, we can treat it as an unconditional
     one if it's an array reference and all its index are within array
     bound.  */
  if (DR_RW_UNCONDITIONALLY (*master_dr)
      || ref_within_array_bound (stmt, DR_REF (a)))
    {
      /* an unconditional read won't trap.  */
      if (DR_IS_READ (a))
	return true;

      /* an unconditionaly write won't trap if the base is written
         to unconditionally.  */
      if (base_master_dr
	  && DR_BASE_W_UNCONDITIONALLY (*base_master_dr))
	return PARAM_VALUE (PARAM_ALLOW_STORE_DATA_RACES);
      /* or the base is known to be not readonly.  */
      else if (base_object_writable (DR_REF (a)))
	return PARAM_VALUE (PARAM_ALLOW_STORE_DATA_RACES);
    }

  return false;
}

/* Return true if STMT could be converted into a masked load or store
   (conditional load or store based on a mask computed from bb predicate).  */

static bool
ifcvt_can_use_mask_load_store (gimple *stmt)
{
  tree lhs, ref;
  machine_mode mode;
  basic_block bb = gimple_bb (stmt);
  bool is_load;

  if (!(flag_tree_loop_vectorize || bb->loop_father->force_vectorize)
      || bb->loop_father->dont_vectorize
      || !gimple_assign_single_p (stmt)
      || gimple_has_volatile_ops (stmt))
    return false;

  /* Check whether this is a load or store.  */
  lhs = gimple_assign_lhs (stmt);
  if (gimple_store_p (stmt))
    {
      if (!is_gimple_val (gimple_assign_rhs1 (stmt)))
	return false;
      is_load = false;
      ref = lhs;
    }
  else if (gimple_assign_load_p (stmt))
    {
      is_load = true;
      ref = gimple_assign_rhs1 (stmt);
    }
  else
    return false;

  if (may_be_nonaddressable_p (ref))
    return false;

  /* Mask should be integer mode of the same size as the load/store
     mode.  */
  mode = TYPE_MODE (TREE_TYPE (lhs));
  if (!int_mode_for_mode (mode).exists () || VECTOR_MODE_P (mode))
    return false;

  if (can_vec_mask_load_store_p (mode, VOIDmode, is_load))
    return true;

  return false;
}

/* Return true when STMT is if-convertible.

   GIMPLE_ASSIGN statement is not if-convertible if,
   - it is not movable,
   - it could trap,
   - LHS is not var decl.  */

static bool
if_convertible_gimple_assign_stmt_p (gimple *stmt,
				     vec<data_reference_p> refs)
{
  tree lhs = gimple_assign_lhs (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "-------------------------\n");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  if (!is_gimple_reg_type (TREE_TYPE (lhs)))
    return false;

  /* Some of these constrains might be too conservative.  */
  if (stmt_ends_bb_p (stmt)
      || gimple_has_volatile_ops (stmt)
      || (TREE_CODE (lhs) == SSA_NAME
          && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
      || gimple_has_side_effects (stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "stmt not suitable for ifcvt\n");
      return false;
    }

  /* tree-into-ssa.c uses GF_PLF_1, so avoid it, because
     in between if_convertible_loop_p and combine_blocks
     we can perform loop versioning.  */
  gimple_set_plf (stmt, GF_PLF_2, false);

  if ((! gimple_vuse (stmt)
       || gimple_could_trap_p_1 (stmt, false, false)
       || ! ifcvt_memrefs_wont_trap (stmt, refs))
      && gimple_could_trap_p (stmt))
    {
      if (ifcvt_can_use_mask_load_store (stmt))
	{
	  gimple_set_plf (stmt, GF_PLF_2, true);
	  any_pred_load_store = true;
	  return true;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "tree could trap...\n");
      return false;
    }

  /* When if-converting stores force versioning, likewise if we
     ended up generating store data races.  */
  if (gimple_vdef (stmt))
    any_pred_load_store = true;

  return true;
}

/* Return true when STMT is if-convertible.

   A statement is if-convertible if:
   - it is an if-convertible GIMPLE_ASSIGN,
   - it is a GIMPLE_LABEL or a GIMPLE_COND,
   - it is builtins call.  */

static bool
if_convertible_stmt_p (gimple *stmt, vec<data_reference_p> refs)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_LABEL:
    case GIMPLE_DEBUG:
    case GIMPLE_COND:
      return true;

    case GIMPLE_ASSIGN:
      return if_convertible_gimple_assign_stmt_p (stmt, refs);

    case GIMPLE_CALL:
      {
	tree fndecl = gimple_call_fndecl (stmt);
	if (fndecl)
	  {
	    int flags = gimple_call_flags (stmt);
	    if ((flags & ECF_CONST)
		&& !(flags & ECF_LOOPING_CONST_OR_PURE)
		/* We can only vectorize some builtins at the moment,
		   so restrict if-conversion to those.  */
		&& DECL_BUILT_IN (fndecl))
	      return true;
	  }
	return false;
      }

    default:
      /* Don't know what to do with 'em so don't do anything.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "don't know what to do\n");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      return false;
    }

  return true;
}

/* Assumes that BB has more than 1 predecessors.
   Returns false if at least one successor is not on critical edge
   and true otherwise.  */

static inline bool
all_preds_critical_p (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (EDGE_COUNT (e->src->succs) == 1)
      return false;
  return true;
}

/* Returns true if at least one successor in on critical edge.  */
static inline bool
has_pred_critical_p (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (EDGE_COUNT (e->src->succs) > 1)
      return true;
  return false;
}

/* Return true when BB is if-convertible.  This routine does not check
   basic block's statements and phis.

   A basic block is not if-convertible if:
   - it is non-empty and it is after the exit block (in BFS order),
   - it is after the exit block but before the latch,
   - its edges are not normal.

   EXIT_BB is the basic block containing the exit of the LOOP.  BB is
   inside LOOP.  */

static bool
if_convertible_bb_p (struct loop *loop, basic_block bb, basic_block exit_bb)
{
  edge e;
  edge_iterator ei;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "----------[%d]-------------\n", bb->index);

  if (EDGE_COUNT (bb->succs) > 2)
    return false;

  if (exit_bb)
    {
      if (bb != loop->latch)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "basic block after exit bb but before latch\n");
	  return false;
	}
      else if (!empty_block_p (bb))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "non empty basic block after exit bb\n");
	  return false;
	}
      else if (bb == loop->latch
	       && bb != exit_bb
	       && !dominated_by_p (CDI_DOMINATORS, bb, exit_bb))
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "latch is not dominated by exit_block\n");
	    return false;
	  }
    }

  /* Be less adventurous and handle only normal edges.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->flags & (EDGE_EH | EDGE_ABNORMAL | EDGE_IRREDUCIBLE_LOOP))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file, "Difficult to handle edges\n");
	return false;
      }

  return true;
}

/* Return true when all predecessor blocks of BB are visited.  The
   VISITED bitmap keeps track of the visited blocks.  */

static bool
pred_blocks_visited_p (basic_block bb, bitmap *visited)
{
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!bitmap_bit_p (*visited, e->src->index))
      return false;

  return true;
}

/* Get body of a LOOP in suitable order for if-conversion.  It is
   caller's responsibility to deallocate basic block list.
   If-conversion suitable order is, breadth first sort (BFS) order
   with an additional constraint: select a block only if all its
   predecessors are already selected.  */

static basic_block *
get_loop_body_in_if_conv_order (const struct loop *loop)
{
  basic_block *blocks, *blocks_in_bfs_order;
  basic_block bb;
  bitmap visited;
  unsigned int index = 0;
  unsigned int visited_count = 0;

  gcc_assert (loop->num_nodes);
  gcc_assert (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun));

  blocks = XCNEWVEC (basic_block, loop->num_nodes);
  visited = BITMAP_ALLOC (NULL);

  blocks_in_bfs_order = get_loop_body_in_bfs_order (loop);

  index = 0;
  while (index < loop->num_nodes)
    {
      bb = blocks_in_bfs_order [index];

      if (bb->flags & BB_IRREDUCIBLE_LOOP)
	{
	  free (blocks_in_bfs_order);
	  BITMAP_FREE (visited);
	  free (blocks);
	  return NULL;
	}

      if (!bitmap_bit_p (visited, bb->index))
	{
	  if (pred_blocks_visited_p (bb, &visited)
	      || bb == loop->header)
	    {
	      /* This block is now visited.  */
	      bitmap_set_bit (visited, bb->index);
	      blocks[visited_count++] = bb;
	    }
	}

      index++;

      if (index == loop->num_nodes
	  && visited_count != loop->num_nodes)
	/* Not done yet.  */
	index = 0;
    }
  free (blocks_in_bfs_order);
  BITMAP_FREE (visited);
  return blocks;
}

/* Returns true when the analysis of the predicates for all the basic
   blocks in LOOP succeeded.

   predicate_bbs first allocates the predicates of the basic blocks.
   These fields are then initialized with the tree expressions
   representing the predicates under which a basic block is executed
   in the LOOP.  As the loop->header is executed at each iteration, it
   has the "true" predicate.  Other statements executed under a
   condition are predicated with that condition, for example

   | if (x)
   |   S1;
   | else
   |   S2;

   S1 will be predicated with "x", and
   S2 will be predicated with "!x".  */

static void
predicate_bbs (loop_p loop)
{
  unsigned int i;

  for (i = 0; i < loop->num_nodes; i++)
    init_bb_predicate (ifc_bbs[i]);

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      tree cond;
      gimple *stmt;

      /* The loop latch and loop exit block are always executed and
	 have no extra conditions to be processed: skip them.  */
      if (bb == loop->latch
	  || bb_with_exit_edge_p (loop, bb))
	{
	  reset_bb_predicate (bb);
	  continue;
	}

      cond = bb_predicate (bb);
      stmt = last_stmt (bb);
      if (stmt && gimple_code (stmt) == GIMPLE_COND)
	{
	  tree c2;
	  edge true_edge, false_edge;
	  location_t loc = gimple_location (stmt);
	  tree c = build2_loc (loc, gimple_cond_code (stmt),
				    boolean_type_node,
				    gimple_cond_lhs (stmt),
				    gimple_cond_rhs (stmt));

	  /* Add new condition into destination's predicate list.  */
	  extract_true_false_edges_from_block (gimple_bb (stmt),
					       &true_edge, &false_edge);

	  /* If C is true, then TRUE_EDGE is taken.  */
	  add_to_dst_predicate_list (loop, true_edge, unshare_expr (cond),
				     unshare_expr (c));

	  /* If C is false, then FALSE_EDGE is taken.  */
	  c2 = build1_loc (loc, TRUTH_NOT_EXPR, boolean_type_node,
			   unshare_expr (c));
	  add_to_dst_predicate_list (loop, false_edge,
				     unshare_expr (cond), c2);

	  cond = NULL_TREE;
	}

      /* If current bb has only one successor, then consider it as an
	 unconditional goto.  */
      if (single_succ_p (bb))
	{
	  basic_block bb_n = single_succ (bb);

	  /* The successor bb inherits the predicate of its
	     predecessor.  If there is no predicate in the predecessor
	     bb, then consider the successor bb as always executed.  */
	  if (cond == NULL_TREE)
	    cond = boolean_true_node;

	  add_to_predicate_list (loop, bb_n, cond);
	}
    }

  /* The loop header is always executed.  */
  reset_bb_predicate (loop->header);
  gcc_assert (bb_predicate_gimplified_stmts (loop->header) == NULL
	      && bb_predicate_gimplified_stmts (loop->latch) == NULL);
}

/* Build region by adding loop pre-header and post-header blocks.  */

static vec<basic_block>
build_region (struct loop *loop)
{
  vec<basic_block> region = vNULL;
  basic_block exit_bb = NULL;

  gcc_assert (ifc_bbs);
  /* The first element is loop pre-header.  */
  region.safe_push (loop_preheader_edge (loop)->src);

  for (unsigned int i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      region.safe_push (bb);
      /* Find loop postheader.  */
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (loop_exit_edge_p (loop, e))
	  {
	      exit_bb = e->dest;
	      break;
	  }
    }
  /* The last element is loop post-header.  */
  gcc_assert (exit_bb);
  region.safe_push (exit_bb);
  return region;
}

/* Return true when LOOP is if-convertible.  This is a helper function
   for if_convertible_loop_p.  REFS and DDRS are initialized and freed
   in if_convertible_loop_p.  */

static bool
if_convertible_loop_p_1 (struct loop *loop, vec<data_reference_p> *refs)
{
  unsigned int i;
  basic_block exit_bb = NULL;
  vec<basic_block> region;

  if (find_data_references_in_loop (loop, refs) == chrec_dont_know)
    return false;

  calculate_dominance_info (CDI_DOMINATORS);

  /* Allow statements that can be handled during if-conversion.  */
  ifc_bbs = get_loop_body_in_if_conv_order (loop);
  if (!ifc_bbs)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Irreducible loop\n");
      return false;
    }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];

      if (!if_convertible_bb_p (loop, bb, exit_bb))
	return false;

      if (bb_with_exit_edge_p (loop, bb))
	exit_bb = bb;
    }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	switch (gimple_code (gsi_stmt (gsi)))
	  {
	  case GIMPLE_LABEL:
	  case GIMPLE_ASSIGN:
	  case GIMPLE_CALL:
	  case GIMPLE_DEBUG:
	  case GIMPLE_COND:
	    gimple_set_uid (gsi_stmt (gsi), 0);
	    break;
	  default:
	    return false;
	  }
    }

  data_reference_p dr;

  innermost_DR_map
	  = new hash_map<innermost_loop_behavior_hash, data_reference_p>;
  baseref_DR_map = new hash_map<tree_operand_hash, data_reference_p>;

  /* Compute post-dominator tree locally.  */
  region = build_region (loop);
  calculate_dominance_info_for_region (CDI_POST_DOMINATORS, region);

  predicate_bbs (loop);

  /* Free post-dominator tree since it is not used after predication.  */
  free_dominance_info_for_region (cfun, CDI_POST_DOMINATORS, region);
  region.release ();

  for (i = 0; refs->iterate (i, &dr); i++)
    {
      tree ref = DR_REF (dr);

      dr->aux = XNEW (struct ifc_dr);
      DR_BASE_W_UNCONDITIONALLY (dr) = false;
      DR_RW_UNCONDITIONALLY (dr) = false;
      DR_W_UNCONDITIONALLY (dr) = false;
      IFC_DR (dr)->rw_predicate = boolean_false_node;
      IFC_DR (dr)->w_predicate = boolean_false_node;
      IFC_DR (dr)->base_w_predicate = boolean_false_node;
      if (gimple_uid (DR_STMT (dr)) == 0)
	gimple_set_uid (DR_STMT (dr), i + 1);

      /* If DR doesn't have innermost loop behavior or it's a compound
         memory reference, we synthesize its innermost loop behavior
         for hashing.  */
      if (TREE_CODE (ref) == COMPONENT_REF
          || TREE_CODE (ref) == IMAGPART_EXPR
          || TREE_CODE (ref) == REALPART_EXPR
          || !(DR_BASE_ADDRESS (dr) || DR_OFFSET (dr)
	       || DR_INIT (dr) || DR_STEP (dr)))
        {
          while (TREE_CODE (ref) == COMPONENT_REF
	         || TREE_CODE (ref) == IMAGPART_EXPR
	         || TREE_CODE (ref) == REALPART_EXPR)
	    ref = TREE_OPERAND (ref, 0);

	  memset (&DR_INNERMOST (dr), 0, sizeof (DR_INNERMOST (dr)));
	  DR_BASE_ADDRESS (dr) = ref;
        }
      hash_memrefs_baserefs_and_store_DRs_read_written_info (dr);
    }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      gimple_stmt_iterator itr;

      /* Check the if-convertibility of statements in predicated BBs.  */
      if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
	for (itr = gsi_start_bb (bb); !gsi_end_p (itr); gsi_next (&itr))
	  if (!if_convertible_stmt_p (gsi_stmt (itr), *refs))
	    return false;
    }

  /* Checking PHIs needs to be done after stmts, as the fact whether there
     are any masked loads or stores affects the tests.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      gphi_iterator itr;

      for (itr = gsi_start_phis (bb); !gsi_end_p (itr); gsi_next (&itr))
	if (!if_convertible_phi_p (loop, bb, itr.phi ()))
	  return false;
    }

  if (dump_file)
    fprintf (dump_file, "Applying if-conversion\n");

  return true;
}

/* Return true when LOOP is if-convertible.
   LOOP is if-convertible if:
   - it is innermost,
   - it has two or more basic blocks,
   - it has only one exit,
   - loop header is not the exit edge,
   - if its basic blocks and phi nodes are if convertible.  */

static bool
if_convertible_loop_p (struct loop *loop)
{
  edge e;
  edge_iterator ei;
  bool res = false;
  vec<data_reference_p> refs;

  /* Handle only innermost loop.  */
  if (!loop || loop->inner)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "not innermost loop\n");
      return false;
    }

  /* If only one block, no need for if-conversion.  */
  if (loop->num_nodes <= 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "less than 2 basic blocks\n");
      return false;
    }

  /* More than one loop exit is too much to handle.  */
  if (!single_exit (loop))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "multiple exits\n");
      return false;
    }

  /* If one of the loop header's edge is an exit edge then do not
     apply if-conversion.  */
  FOR_EACH_EDGE (e, ei, loop->header->succs)
    if (loop_exit_edge_p (loop, e))
      return false;

  refs.create (5);
  res = if_convertible_loop_p_1 (loop, &refs);

  data_reference_p dr;
  unsigned int i;
  for (i = 0; refs.iterate (i, &dr); i++)
    free (dr->aux);

  free_data_refs (refs);

  delete innermost_DR_map;
  innermost_DR_map = NULL;

  delete baseref_DR_map;
  baseref_DR_map = NULL;

  return res;
}

/* Returns true if def-stmt for phi argument ARG is simple increment/decrement
   which is in predicated basic block.
   In fact, the following PHI pattern is searching:
      loop-header:
	reduc_1 = PHI <..., reduc_2>
      ...
	if (...)
	  reduc_3 = ...
	reduc_2 = PHI <reduc_1, reduc_3>

   ARG_0 and ARG_1 are correspondent PHI arguments.
   REDUC, OP0 and OP1 contain reduction stmt and its operands.
   EXTENDED is true if PHI has > 2 arguments.  */

static bool
is_cond_scalar_reduction (gimple *phi, gimple **reduc, tree arg_0, tree arg_1,
			  tree *op0, tree *op1, bool extended)
{
  tree lhs, r_op1, r_op2;
  gimple *stmt;
  gimple *header_phi = NULL;
  enum tree_code reduction_op;
  basic_block bb = gimple_bb (phi);
  struct loop *loop = bb->loop_father;
  edge latch_e = loop_latch_edge (loop);
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  edge e;
  edge_iterator ei;
  bool result = false;
  if (TREE_CODE (arg_0) != SSA_NAME || TREE_CODE (arg_1) != SSA_NAME)
    return false;

  if (!extended && gimple_code (SSA_NAME_DEF_STMT (arg_0)) == GIMPLE_PHI)
    {
      lhs = arg_1;
      header_phi = SSA_NAME_DEF_STMT (arg_0);
      stmt = SSA_NAME_DEF_STMT (arg_1);
    }
  else if (gimple_code (SSA_NAME_DEF_STMT (arg_1)) == GIMPLE_PHI)
    {
      lhs = arg_0;
      header_phi = SSA_NAME_DEF_STMT (arg_1);
      stmt = SSA_NAME_DEF_STMT (arg_0);
    }
  else
    return false;
  if (gimple_bb (header_phi) != loop->header)
    return false;

  if (PHI_ARG_DEF_FROM_EDGE (header_phi, latch_e) != PHI_RESULT (phi))
    return false;

  if (gimple_code (stmt) != GIMPLE_ASSIGN
      || gimple_has_volatile_ops (stmt))
    return false;

  if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
    return false;

  if (!is_predicated (gimple_bb (stmt)))
    return false;

  /* Check that stmt-block is predecessor of phi-block.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
    if (e->dest == bb)
      {
	result = true;
	break;
      }
  if (!result)
    return false;

  if (!has_single_use (lhs))
    return false;

  reduction_op = gimple_assign_rhs_code (stmt);
  if (reduction_op != PLUS_EXPR && reduction_op != MINUS_EXPR)
    return false;
  r_op1 = gimple_assign_rhs1 (stmt);
  r_op2 = gimple_assign_rhs2 (stmt);

  /* Make R_OP1 to hold reduction variable.  */
  if (r_op2 == PHI_RESULT (header_phi)
      && reduction_op == PLUS_EXPR)
    std::swap (r_op1, r_op2);
  else if (r_op1 != PHI_RESULT (header_phi))
    return false;

  /* Check that R_OP1 is used in reduction stmt or in PHI only.  */
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, r_op1)
    {
      gimple *use_stmt = USE_STMT (use_p);
      if (is_gimple_debug (use_stmt))
	continue;
      if (use_stmt == stmt)
	continue;
      if (gimple_code (use_stmt) != GIMPLE_PHI)
	return false;
    }

  *op0 = r_op1; *op1 = r_op2;
  *reduc = stmt;
  return true;
}

/* Converts conditional scalar reduction into unconditional form, e.g.
     bb_4
       if (_5 != 0) goto bb_5 else goto bb_6
     end_bb_4
     bb_5
       res_6 = res_13 + 1;
     end_bb_5
     bb_6
       # res_2 = PHI <res_13(4), res_6(5)>
     end_bb_6

   will be converted into sequence
    _ifc__1 = _5 != 0 ? 1 : 0;
    res_2 = res_13 + _ifc__1;
  Argument SWAP tells that arguments of conditional expression should be
  swapped.
  Returns rhs of resulting PHI assignment.  */

static tree
convert_scalar_cond_reduction (gimple *reduc, gimple_stmt_iterator *gsi,
			       tree cond, tree op0, tree op1, bool swap)
{
  gimple_stmt_iterator stmt_it;
  gimple *new_assign;
  tree rhs;
  tree rhs1 = gimple_assign_rhs1 (reduc);
  tree tmp = make_temp_ssa_name (TREE_TYPE (rhs1), NULL, "_ifc_");
  tree c;
  tree zero = build_zero_cst (TREE_TYPE (rhs1));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Found cond scalar reduction.\n");
      print_gimple_stmt (dump_file, reduc, 0, TDF_SLIM);
    }

  /* Build cond expression using COND and constant operand
     of reduction rhs.  */
  c = fold_build_cond_expr (TREE_TYPE (rhs1),
			    unshare_expr (cond),
			    swap ? zero : op1,
			    swap ? op1 : zero);

  /* Create assignment stmt and insert it at GSI.  */
  new_assign = gimple_build_assign (tmp, c);
  gsi_insert_before (gsi, new_assign, GSI_SAME_STMT);
  /* Build rhs for unconditional increment/decrement.  */
  rhs = fold_build2 (gimple_assign_rhs_code (reduc),
		     TREE_TYPE (rhs1), op0, tmp);

  /* Delete original reduction stmt.  */
  stmt_it = gsi_for_stmt (reduc);
  gsi_remove (&stmt_it, true);
  release_defs (reduc);
  return rhs;
}

/* Produce condition for all occurrences of ARG in PHI node.  */

static tree
gen_phi_arg_condition (gphi *phi, vec<int> *occur,
		       gimple_stmt_iterator *gsi)
{
  int len;
  int i;
  tree cond = NULL_TREE;
  tree c;
  edge e;

  len = occur->length ();
  gcc_assert (len > 0);
  for (i = 0; i < len; i++)
    {
      e = gimple_phi_arg_edge (phi, (*occur)[i]);
      c = bb_predicate (e->src);
      if (is_true_predicate (c))
	{
	  cond = c;
	  break;
	}
      c = force_gimple_operand_gsi_1 (gsi, unshare_expr (c),
				      is_gimple_condexpr, NULL_TREE,
				      true, GSI_SAME_STMT);
      if (cond != NULL_TREE)
	{
	  /* Must build OR expression.  */
	  cond = fold_or_predicates (EXPR_LOCATION (c), c, cond);
	  cond = force_gimple_operand_gsi_1 (gsi, unshare_expr (cond),
					     is_gimple_condexpr, NULL_TREE,
					     true, GSI_SAME_STMT);
	}
      else
	cond = c;
    }
  gcc_assert (cond != NULL_TREE);
  return cond;
}

/* Local valueization callback that follows all-use SSA edges.  */

static tree
ifcvt_follow_ssa_use_edges (tree val)
{
  return val;
}

/* Replace a scalar PHI node with a COND_EXPR using COND as condition.
   This routine can handle PHI nodes with more than two arguments.

   For example,
     S1: A = PHI <x1(1), x2(5)>
   is converted into,
     S2: A = cond ? x1 : x2;

   The generated code is inserted at GSI that points to the top of
   basic block's statement list.
   If PHI node has more than two arguments a chain of conditional
   expression is produced.  */


static void
predicate_scalar_phi (gphi *phi, gimple_stmt_iterator *gsi)
{
  gimple *new_stmt = NULL, *reduc;
  tree rhs, res, arg0, arg1, op0, op1, scev;
  tree cond;
  unsigned int index0;
  unsigned int max, args_len;
  edge e;
  basic_block bb;
  unsigned int i;

  res = gimple_phi_result (phi);
  if (virtual_operand_p (res))
    return;

  if ((rhs = degenerate_phi_result (phi))
      || ((scev = analyze_scalar_evolution (gimple_bb (phi)->loop_father,
					    res))
	  && !chrec_contains_undetermined (scev)
	  && scev != res
	  && (rhs = gimple_phi_arg_def (phi, 0))))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Degenerate phi!\n");
	  print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	}
      new_stmt = gimple_build_assign (res, rhs);
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
      update_stmt (new_stmt);
      return;
    }

  bb = gimple_bb (phi);
  if (EDGE_COUNT (bb->preds) == 2)
    {
      /* Predicate ordinary PHI node with 2 arguments.  */
      edge first_edge, second_edge;
      basic_block true_bb;
      first_edge = EDGE_PRED (bb, 0);
      second_edge = EDGE_PRED (bb, 1);
      cond = bb_predicate (first_edge->src);
      if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
	std::swap (first_edge, second_edge);
      if (EDGE_COUNT (first_edge->src->succs) > 1)
	{
	  cond = bb_predicate (second_edge->src);
	  if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
	    cond = TREE_OPERAND (cond, 0);
	  else
	    first_edge = second_edge;
	}
      else
	cond = bb_predicate (first_edge->src);
      /* Gimplify the condition to a valid cond-expr conditonal operand.  */
      cond = force_gimple_operand_gsi_1 (gsi, unshare_expr (cond),
					 is_gimple_condexpr, NULL_TREE,
					 true, GSI_SAME_STMT);
      true_bb = first_edge->src;
      if (EDGE_PRED (bb, 1)->src == true_bb)
	{
	  arg0 = gimple_phi_arg_def (phi, 1);
	  arg1 = gimple_phi_arg_def (phi, 0);
	}
      else
	{
	  arg0 = gimple_phi_arg_def (phi, 0);
	  arg1 = gimple_phi_arg_def (phi, 1);
	}
      if (is_cond_scalar_reduction (phi, &reduc, arg0, arg1,
				    &op0, &op1, false))
	/* Convert reduction stmt into vectorizable form.  */
	rhs = convert_scalar_cond_reduction (reduc, gsi, cond, op0, op1,
					     true_bb != gimple_bb (reduc));
      else
	/* Build new RHS using selected condition and arguments.  */
	rhs = fold_build_cond_expr (TREE_TYPE (res), unshare_expr (cond),
				    arg0, arg1);
      new_stmt = gimple_build_assign (res, rhs);
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
      gimple_stmt_iterator new_gsi = gsi_for_stmt (new_stmt);
      if (fold_stmt (&new_gsi, ifcvt_follow_ssa_use_edges))
	{
	  new_stmt = gsi_stmt (new_gsi);
	  update_stmt (new_stmt);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "new phi replacement stmt\n");
	  print_gimple_stmt (dump_file, new_stmt, 0, TDF_SLIM);
	}
      return;
    }

  /* Create hashmap for PHI node which contain vector of argument indexes
     having the same value.  */
  bool swap = false;
  hash_map<tree_operand_hash, auto_vec<int> > phi_arg_map;
  unsigned int num_args = gimple_phi_num_args (phi);
  int max_ind = -1;
  /* Vector of different PHI argument values.  */
  auto_vec<tree> args (num_args);

  /* Compute phi_arg_map.  */
  for (i = 0; i < num_args; i++)
    {
      tree arg;

      arg = gimple_phi_arg_def (phi, i);
      if (!phi_arg_map.get (arg))
	args.quick_push (arg);
      phi_arg_map.get_or_insert (arg).safe_push (i);
    }

  /* Determine element with max number of occurrences.  */
  max_ind = -1;
  max = 1;
  args_len = args.length ();
  for (i = 0; i < args_len; i++)
    {
      unsigned int len;
      if ((len = phi_arg_map.get (args[i])->length ()) > max)
	{
	  max_ind = (int) i;
	  max = len;
	}
    }

  /* Put element with max number of occurences to the end of ARGS.  */
  if (max_ind != -1 && max_ind +1 != (int) args_len)
    std::swap (args[args_len - 1], args[max_ind]);

  /* Handle one special case when number of arguments with different values
     is equal 2 and one argument has the only occurrence.  Such PHI can be
     handled as if would have only 2 arguments.  */
  if (args_len == 2 && phi_arg_map.get (args[0])->length () == 1)
    {
      vec<int> *indexes;
      indexes = phi_arg_map.get (args[0]);
      index0 = (*indexes)[0];
      arg0 = args[0];
      arg1 = args[1];
      e = gimple_phi_arg_edge (phi, index0);
      cond = bb_predicate (e->src);
      if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
	{
	  swap = true;
	  cond = TREE_OPERAND (cond, 0);
	}
      /* Gimplify the condition to a valid cond-expr conditonal operand.  */
      cond = force_gimple_operand_gsi_1 (gsi, unshare_expr (cond),
					 is_gimple_condexpr, NULL_TREE,
					 true, GSI_SAME_STMT);
      if (!(is_cond_scalar_reduction (phi, &reduc, arg0 , arg1,
				      &op0, &op1, true)))
	rhs = fold_build_cond_expr (TREE_TYPE (res), unshare_expr (cond),
				    swap? arg1 : arg0,
				    swap? arg0 : arg1);
      else
	/* Convert reduction stmt into vectorizable form.  */
	rhs = convert_scalar_cond_reduction (reduc, gsi, cond, op0, op1,
					     swap);
      new_stmt = gimple_build_assign (res, rhs);
      gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
      update_stmt (new_stmt);
    }
  else
    {
      /* Common case.  */
      vec<int> *indexes;
      tree type = TREE_TYPE (gimple_phi_result (phi));
      tree lhs;
      arg1 = args[1];
      for (i = 0; i < args_len; i++)
	{
	  arg0 = args[i];
	  indexes = phi_arg_map.get (args[i]);
	  if (i != args_len - 1)
	    lhs = make_temp_ssa_name (type, NULL, "_ifc_");
	  else
	    lhs = res;
	  cond = gen_phi_arg_condition (phi, indexes, gsi);
	  rhs = fold_build_cond_expr (type, unshare_expr (cond),
				      arg0, arg1);
	  new_stmt = gimple_build_assign (lhs, rhs);
	  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
	  update_stmt (new_stmt);
	  arg1 = lhs;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "new extended phi replacement stmt\n");
      print_gimple_stmt (dump_file, new_stmt, 0, TDF_SLIM);
    }
}

/* Replaces in LOOP all the scalar phi nodes other than those in the
   LOOP->header block with conditional modify expressions.  */

static void
predicate_all_scalar_phis (struct loop *loop)
{
  basic_block bb;
  unsigned int orig_loop_num_nodes = loop->num_nodes;
  unsigned int i;

  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gphi *phi;
      gimple_stmt_iterator gsi;
      gphi_iterator phi_gsi;
      bb = ifc_bbs[i];

      if (bb == loop->header)
	continue;

      phi_gsi = gsi_start_phis (bb);
      if (gsi_end_p (phi_gsi))
	continue;

      gsi = gsi_after_labels (bb);
      while (!gsi_end_p (phi_gsi))
	{
	  phi = phi_gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    gsi_next (&phi_gsi);
	  else
	    {
	      predicate_scalar_phi (phi, &gsi);
	      remove_phi_node (&phi_gsi, false);
	    }
	}
    }
}

/* Insert in each basic block of LOOP the statements produced by the
   gimplification of the predicates.  */

static void
insert_gimplified_predicates (loop_p loop)
{
  unsigned int i;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      gimple_seq stmts;
      if (!is_predicated (bb))
	gcc_assert (bb_predicate_gimplified_stmts (bb) == NULL);
      if (!is_predicated (bb))
	{
	  /* Do not insert statements for a basic block that is not
	     predicated.  Also make sure that the predicate of the
	     basic block is set to true.  */
	  reset_bb_predicate (bb);
	  continue;
	}

      stmts = bb_predicate_gimplified_stmts (bb);
      if (stmts)
	{
	  if (any_pred_load_store)
	    {
	      /* Insert the predicate of the BB just after the label,
		 as the if-conversion of memory writes will use this
		 predicate.  */
	      gimple_stmt_iterator gsi = gsi_after_labels (bb);
	      gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
	    }
	  else
	    {
	      /* Insert the predicate of the BB at the end of the BB
		 as this would reduce the register pressure: the only
		 use of this predicate will be in successor BBs.  */
	      gimple_stmt_iterator gsi = gsi_last_bb (bb);

	      if (gsi_end_p (gsi)
		  || stmt_ends_bb_p (gsi_stmt (gsi)))
		gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);
	      else
		gsi_insert_seq_after (&gsi, stmts, GSI_SAME_STMT);
	    }

	  /* Once the sequence is code generated, set it to NULL.  */
	  set_bb_predicate_gimplified_stmts (bb, NULL);
	}
    }
}

/* Helper function for predicate_mem_writes. Returns index of existent
   mask if it was created for given SIZE and -1 otherwise.  */

static int
mask_exists (int size, vec<int> vec)
{
  unsigned int ix;
  int v;
  FOR_EACH_VEC_ELT (vec, ix, v)
    if (v == size)
      return (int) ix;
  return -1;
}

/* Predicate each write to memory in LOOP.

   This function transforms control flow constructs containing memory
   writes of the form:

   | for (i = 0; i < N; i++)
   |   if (cond)
   |     A[i] = expr;

   into the following form that does not contain control flow:

   | for (i = 0; i < N; i++)
   |   A[i] = cond ? expr : A[i];

   The original CFG looks like this:

   | bb_0
   |   i = 0
   | end_bb_0
   |
   | bb_1
   |   if (i < N) goto bb_5 else goto bb_2
   | end_bb_1
   |
   | bb_2
   |   cond = some_computation;
   |   if (cond) goto bb_3 else goto bb_4
   | end_bb_2
   |
   | bb_3
   |   A[i] = expr;
   |   goto bb_4
   | end_bb_3
   |
   | bb_4
   |   goto bb_1
   | end_bb_4

   insert_gimplified_predicates inserts the computation of the COND
   expression at the beginning of the destination basic block:

   | bb_0
   |   i = 0
   | end_bb_0
   |
   | bb_1
   |   if (i < N) goto bb_5 else goto bb_2
   | end_bb_1
   |
   | bb_2
   |   cond = some_computation;
   |   if (cond) goto bb_3 else goto bb_4
   | end_bb_2
   |
   | bb_3
   |   cond = some_computation;
   |   A[i] = expr;
   |   goto bb_4
   | end_bb_3
   |
   | bb_4
   |   goto bb_1
   | end_bb_4

   predicate_mem_writes is then predicating the memory write as follows:

   | bb_0
   |   i = 0
   | end_bb_0
   |
   | bb_1
   |   if (i < N) goto bb_5 else goto bb_2
   | end_bb_1
   |
   | bb_2
   |   if (cond) goto bb_3 else goto bb_4
   | end_bb_2
   |
   | bb_3
   |   cond = some_computation;
   |   A[i] = cond ? expr : A[i];
   |   goto bb_4
   | end_bb_3
   |
   | bb_4
   |   goto bb_1
   | end_bb_4

   and finally combine_blocks removes the basic block boundaries making
   the loop vectorizable:

   | bb_0
   |   i = 0
   |   if (i < N) goto bb_5 else goto bb_1
   | end_bb_0
   |
   | bb_1
   |   cond = some_computation;
   |   A[i] = cond ? expr : A[i];
   |   if (i < N) goto bb_5 else goto bb_4
   | end_bb_1
   |
   | bb_4
   |   goto bb_1
   | end_bb_4
*/

static void
predicate_mem_writes (loop_p loop)
{
  unsigned int i, orig_loop_num_nodes = loop->num_nodes;
  auto_vec<int, 1> vect_sizes;
  auto_vec<tree, 1> vect_masks;

  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gimple_stmt_iterator gsi;
      basic_block bb = ifc_bbs[i];
      tree cond = bb_predicate (bb);
      bool swap;
      gimple *stmt;
      int index;

      if (is_true_predicate (cond))
	continue;

      swap = false;
      if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
	{
	  swap = true;
	  cond = TREE_OPERAND (cond, 0);
	}

      vect_sizes.truncate (0);
      vect_masks.truncate (0);

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  if (!gimple_assign_single_p (stmt = gsi_stmt (gsi)))
	    ;
	  else if (is_false_predicate (cond))
	    {
	      unlink_stmt_vdef (stmt);
	      gsi_remove (&gsi, true);
	      release_defs (stmt);
	      continue;
	    }
	  else if (gimple_plf (stmt, GF_PLF_2))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs = gimple_assign_rhs1 (stmt);
	      tree ref, addr, ptr, mask;
	      gcall *new_stmt;
	      gimple_seq stmts = NULL;
	      int bitsize = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (lhs)));
	      ref = TREE_CODE (lhs) == SSA_NAME ? rhs : lhs;
	      mark_addressable (ref);
	      addr = force_gimple_operand_gsi (&gsi, build_fold_addr_expr (ref),
					       true, NULL_TREE, true,
					       GSI_SAME_STMT);
	      if (!vect_sizes.is_empty ()
		  && (index = mask_exists (bitsize, vect_sizes)) != -1)
		/* Use created mask.  */
		mask = vect_masks[index];
	      else
		{
		  if (COMPARISON_CLASS_P (cond))
		    mask = gimple_build (&stmts, TREE_CODE (cond),
					 boolean_type_node,
					 TREE_OPERAND (cond, 0),
					 TREE_OPERAND (cond, 1));
		  else
		    {
		      gcc_assert (TREE_CODE (cond) == SSA_NAME);
		      mask = cond;
		    }

		  if (swap)
		    {
		      tree true_val
			= constant_boolean_node (true, TREE_TYPE (mask));
		      mask = gimple_build (&stmts, BIT_XOR_EXPR,
					   TREE_TYPE (mask), mask, true_val);
		    }
		  gsi_insert_seq_before (&gsi, stmts, GSI_SAME_STMT);

		  mask = ifc_temp_var (TREE_TYPE (mask), mask, &gsi);
		  /* Save mask and its size for further use.  */
		  vect_sizes.safe_push (bitsize);
		  vect_masks.safe_push (mask);
		}
	      ptr = build_int_cst (reference_alias_ptr_type (ref),
				   get_object_alignment (ref));
	      /* Copy points-to info if possible.  */
	      if (TREE_CODE (addr) == SSA_NAME && !SSA_NAME_PTR_INFO (addr))
		copy_ref_info (build2 (MEM_REF, TREE_TYPE (ref), addr, ptr),
			       ref);
	      if (TREE_CODE (lhs) == SSA_NAME)
		{
		  new_stmt
		    = gimple_build_call_internal (IFN_MASK_LOAD, 3, addr,
						  ptr, mask);
		  gimple_call_set_lhs (new_stmt, lhs);
		  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
		}
	      else
		{
		  new_stmt
		    = gimple_build_call_internal (IFN_MASK_STORE, 4, addr, ptr,
						  mask, rhs);
		  gimple_set_vuse (new_stmt, gimple_vuse (stmt));
		  gimple_set_vdef (new_stmt, gimple_vdef (stmt));
		  SSA_NAME_DEF_STMT (gimple_vdef (new_stmt)) = new_stmt;
		}
	      gimple_call_set_nothrow (new_stmt, true);

	      gsi_replace (&gsi, new_stmt, true);
	    }
	  else if (gimple_vdef (stmt))
	    {
	      tree lhs = gimple_assign_lhs (stmt);
	      tree rhs = gimple_assign_rhs1 (stmt);
	      tree type = TREE_TYPE (lhs);

	      lhs = ifc_temp_var (type, unshare_expr (lhs), &gsi);
	      rhs = ifc_temp_var (type, unshare_expr (rhs), &gsi);
	      if (swap)
		std::swap (lhs, rhs);
	      cond = force_gimple_operand_gsi_1 (&gsi, unshare_expr (cond),
						 is_gimple_condexpr, NULL_TREE,
						 true, GSI_SAME_STMT);
	      rhs = fold_build_cond_expr (type, unshare_expr (cond), rhs, lhs);
	      gimple_assign_set_rhs1 (stmt, ifc_temp_var (type, rhs, &gsi));
	      update_stmt (stmt);
	    }
	  gsi_next (&gsi);
	}
    }
}

/* Remove all GIMPLE_CONDs and GIMPLE_LABELs of all the basic blocks
   other than the exit and latch of the LOOP.  Also resets the
   GIMPLE_DEBUG information.  */

static void
remove_conditions_and_labels (loop_p loop)
{
  gimple_stmt_iterator gsi;
  unsigned int i;

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];

      if (bb_with_exit_edge_p (loop, bb)
        || bb == loop->latch)
      continue;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); )
	switch (gimple_code (gsi_stmt (gsi)))
	  {
	  case GIMPLE_COND:
	  case GIMPLE_LABEL:
	    gsi_remove (&gsi, true);
	    break;

	  case GIMPLE_DEBUG:
	    /* ??? Should there be conditional GIMPLE_DEBUG_BINDs?  */
	    if (gimple_debug_bind_p (gsi_stmt (gsi)))
	      {
		gimple_debug_bind_reset_value (gsi_stmt (gsi));
		update_stmt (gsi_stmt (gsi));
	      }
	    gsi_next (&gsi);
	    break;

	  default:
	    gsi_next (&gsi);
	  }
    }
}

/* Combine all the basic blocks from LOOP into one or two super basic
   blocks.  Replace PHI nodes with conditional modify expressions.  */

static void
combine_blocks (struct loop *loop)
{
  basic_block bb, exit_bb, merge_target_bb;
  unsigned int orig_loop_num_nodes = loop->num_nodes;
  unsigned int i;
  edge e;
  edge_iterator ei;

  remove_conditions_and_labels (loop);
  insert_gimplified_predicates (loop);
  predicate_all_scalar_phis (loop);

  if (any_pred_load_store)
    predicate_mem_writes (loop);

  /* Merge basic blocks: first remove all the edges in the loop,
     except for those from the exit block.  */
  exit_bb = NULL;
  bool *predicated = XNEWVEC (bool, orig_loop_num_nodes);
  for (i = 0; i < orig_loop_num_nodes; i++)
    {
      bb = ifc_bbs[i];
      predicated[i] = !is_true_predicate (bb_predicate (bb));
      free_bb_predicate (bb);
      if (bb_with_exit_edge_p (loop, bb))
	{
	  gcc_assert (exit_bb == NULL);
	  exit_bb = bb;
	}
    }
  gcc_assert (exit_bb != loop->latch);

  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      bb = ifc_bbs[i];

      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei));)
	{
	  if (e->src == exit_bb)
	    ei_next (&ei);
	  else
	    remove_edge (e);
	}
    }

  if (exit_bb != NULL)
    {
      if (exit_bb != loop->header)
	{
	  /* Connect this node to loop header.  */
	  make_single_succ_edge (loop->header, exit_bb, EDGE_FALLTHRU);
	  set_immediate_dominator (CDI_DOMINATORS, exit_bb, loop->header);
	}

      /* Redirect non-exit edges to loop->latch.  */
      FOR_EACH_EDGE (e, ei, exit_bb->succs)
	{
	  if (!loop_exit_edge_p (loop, e))
	    redirect_edge_and_branch (e, loop->latch);
	}
      set_immediate_dominator (CDI_DOMINATORS, loop->latch, exit_bb);
    }
  else
    {
      /* If the loop does not have an exit, reconnect header and latch.  */
      make_edge (loop->header, loop->latch, EDGE_FALLTHRU);
      set_immediate_dominator (CDI_DOMINATORS, loop->latch, loop->header);
    }

  merge_target_bb = loop->header;

  /* Get at the virtual def valid for uses starting at the first block
     we merge into the header.  Without a virtual PHI the loop has the
     same virtual use on all stmts.  */
  gphi *vphi = get_virtual_phi (loop->header);
  tree last_vdef = NULL_TREE;
  if (vphi)
    {
      last_vdef = gimple_phi_result (vphi);
      for (gimple_stmt_iterator gsi = gsi_start_bb (loop->header);
	   ! gsi_end_p (gsi); gsi_next (&gsi))
	if (gimple_vdef (gsi_stmt (gsi)))
	  last_vdef = gimple_vdef (gsi_stmt (gsi));
    }
  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gimple_stmt_iterator gsi;
      gimple_stmt_iterator last;

      bb = ifc_bbs[i];

      if (bb == exit_bb || bb == loop->latch)
	continue;

      /* We release virtual PHIs late because we have to propagate them
         out using the current VUSE.  The def might be the one used
	 after the loop.  */
      vphi = get_virtual_phi (bb);
      if (vphi)
	{
	  imm_use_iterator iter;
	  use_operand_p use_p;
	  gimple *use_stmt;
	  FOR_EACH_IMM_USE_STMT (use_stmt, iter, gimple_phi_result (vphi))
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		SET_USE (use_p, last_vdef);
	    }
	  gsi = gsi_for_stmt (vphi); 
	  remove_phi_node (&gsi, true);
	}

      /* Make stmts member of loop->header and clear range info from all stmts
	 in BB which is now no longer executed conditional on a predicate we
	 could have derived it from.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_bb (stmt, merge_target_bb);
	  /* Update virtual operands.  */
	  if (last_vdef)
	    {
	      use_operand_p use_p = ssa_vuse_operand (stmt);
	      if (use_p
		  && USE_FROM_PTR (use_p) != last_vdef)
		SET_USE (use_p, last_vdef);
	      if (gimple_vdef (stmt))
		last_vdef = gimple_vdef (stmt);
	    }
	  if (predicated[i])
	    {
	      ssa_op_iter i;
	      tree op;
	      FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_DEF)
		reset_flow_sensitive_info (op);
	    }
	}

      /* Update stmt list.  */
      last = gsi_last_bb (merge_target_bb);
      gsi_insert_seq_after_without_update (&last, bb_seq (bb), GSI_NEW_STMT);
      set_bb_seq (bb, NULL);

      delete_basic_block (bb);
    }

  /* If possible, merge loop header to the block with the exit edge.
     This reduces the number of basic blocks to two, to please the
     vectorizer that handles only loops with two nodes.  */
  if (exit_bb
      && exit_bb != loop->header)
    {
      /* We release virtual PHIs late because we have to propagate them
         out using the current VUSE.  The def might be the one used
	 after the loop.  */
      vphi = get_virtual_phi (exit_bb);
      if (vphi)
	{
	  imm_use_iterator iter;
	  use_operand_p use_p;
	  gimple *use_stmt;
	  FOR_EACH_IMM_USE_STMT (use_stmt, iter, gimple_phi_result (vphi))
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		SET_USE (use_p, last_vdef);
	    }
	  gimple_stmt_iterator gsi = gsi_for_stmt (vphi); 
	  remove_phi_node (&gsi, true);
	}

      if (can_merge_blocks_p (loop->header, exit_bb))
	merge_blocks (loop->header, exit_bb);
    }

  free (ifc_bbs);
  ifc_bbs = NULL;
  free (predicated);
}

/* Version LOOP before if-converting it; the original loop
   will be if-converted, the new copy of the loop will not,
   and the LOOP_VECTORIZED internal call will be guarding which
   loop to execute.  The vectorizer pass will fold this
   internal call into either true or false. 

   Note that this function intentionally invalidates profile.  Both edges
   out of LOOP_VECTORIZED must have 100% probability so the profile remains
   consistent after the condition is folded in the vectorizer.  */

static struct loop *
version_loop_for_if_conversion (struct loop *loop)
{
  basic_block cond_bb;
  tree cond = make_ssa_name (boolean_type_node);
  struct loop *new_loop;
  gimple *g;
  gimple_stmt_iterator gsi;
  unsigned int save_length;

  g = gimple_build_call_internal (IFN_LOOP_VECTORIZED, 2,
				  build_int_cst (integer_type_node, loop->num),
				  integer_zero_node);
  gimple_call_set_lhs (g, cond);

  /* Save BB->aux around loop_version as that uses the same field.  */
  save_length = loop->inner ? loop->inner->num_nodes : loop->num_nodes;
  void **saved_preds = XALLOCAVEC (void *, save_length);
  for (unsigned i = 0; i < save_length; i++)
    saved_preds[i] = ifc_bbs[i]->aux;

  initialize_original_copy_tables ();
  /* At this point we invalidate porfile confistency until IFN_LOOP_VECTORIZED
     is re-merged in the vectorizer.  */
  new_loop = loop_version (loop, cond, &cond_bb,
			   profile_probability::always (),
			   profile_probability::always (),
			   profile_probability::always (),
			   profile_probability::always (), true);
  free_original_copy_tables ();

  for (unsigned i = 0; i < save_length; i++)
    ifc_bbs[i]->aux = saved_preds[i];

  if (new_loop == NULL)
    return NULL;

  new_loop->dont_vectorize = true;
  new_loop->force_vectorize = false;
  gsi = gsi_last_bb (cond_bb);
  gimple_call_set_arg (g, 1, build_int_cst (integer_type_node, new_loop->num));
  gsi_insert_before (&gsi, g, GSI_SAME_STMT);
  update_ssa (TODO_update_ssa);
  return new_loop;
}

/* Return true when LOOP satisfies the follow conditions that will
   allow it to be recognized by the vectorizer for outer-loop
   vectorization:
    - The loop is not the root node of the loop tree.
    - The loop has exactly one inner loop.
    - The loop has a single exit.
    - The loop header has a single successor, which is the inner
      loop header.
    - Each of the inner and outer loop latches have a single
      predecessor.
    - The loop exit block has a single predecessor, which is the
      inner loop's exit block.  */

static bool
versionable_outer_loop_p (struct loop *loop)
{
  if (!loop_outer (loop)
      || loop->dont_vectorize
      || !loop->inner
      || loop->inner->next
      || !single_exit (loop)
      || !single_succ_p (loop->header)
      || single_succ (loop->header) != loop->inner->header
      || !single_pred_p (loop->latch)
      || !single_pred_p (loop->inner->latch))
    return false;

  basic_block outer_exit = single_pred (loop->latch);
  basic_block inner_exit = single_pred (loop->inner->latch);

  if (!single_pred_p (outer_exit) || single_pred (outer_exit) != inner_exit)
    return false;

  if (dump_file)
    fprintf (dump_file, "Found vectorizable outer loop for versioning\n");

  return true;
}

/* Performs splitting of critical edges.  Skip splitting and return false
   if LOOP will not be converted because:

     - LOOP is not well formed.
     - LOOP has PHI with more than MAX_PHI_ARG_NUM arguments.

   Last restriction is valid only if AGGRESSIVE_IF_CONV is false.  */

static bool
ifcvt_split_critical_edges (struct loop *loop, bool aggressive_if_conv)
{
  basic_block *body;
  basic_block bb;
  unsigned int num = loop->num_nodes;
  unsigned int i;
  gimple *stmt;
  edge e;
  edge_iterator ei;
  auto_vec<edge> critical_edges;

  /* Loop is not well formed.  */
  if (num <= 2 || loop->inner || !single_exit (loop))
    return false;

  body = get_loop_body (loop);
  for (i = 0; i < num; i++)
    {
      bb = body[i];
      if (!aggressive_if_conv
	  && phi_nodes (bb)
	  && EDGE_COUNT (bb->preds) > MAX_PHI_ARG_NUM)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "BB %d has complicated PHI with more than %u args.\n",
		     bb->index, MAX_PHI_ARG_NUM);

	  free (body);
	  return false;
	}
      if (bb == loop->latch || bb_with_exit_edge_p (loop, bb))
	continue;

      stmt = last_stmt (bb);
      /* Skip basic blocks not ending with conditional branch.  */
      if (!stmt || gimple_code (stmt) != GIMPLE_COND)
	continue;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (EDGE_CRITICAL_P (e) && e->dest->loop_father == loop)
	  critical_edges.safe_push (e);
    }
  free (body);

  while (critical_edges.length () > 0)
    {
      e = critical_edges.pop ();
      /* Don't split if bb can be predicated along non-critical edge.  */
      if (EDGE_COUNT (e->dest->preds) > 2 || all_preds_critical_p (e->dest))
	split_edge (e);
    }

  return true;
}

/* Delete redundant statements produced by predication which prevents
   loop vectorization.  */

static void
ifcvt_local_dce (basic_block bb)
{
  gimple *stmt;
  gimple *stmt1;
  gimple *phi;
  gimple_stmt_iterator gsi;
  auto_vec<gimple *> worklist;
  enum gimple_code code;
  use_operand_p use_p;
  imm_use_iterator imm_iter;

  worklist.create (64);
  /* Consider all phi as live statements.  */
  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      phi = gsi_stmt (gsi);
      gimple_set_plf (phi, GF_PLF_2, true);
      worklist.safe_push (phi);
    }
  /* Consider load/store statements, CALL and COND as live.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (gimple_store_p (stmt)
	  || gimple_assign_load_p (stmt)
	  || is_gimple_debug (stmt))
	{
	  gimple_set_plf (stmt, GF_PLF_2, true);
	  worklist.safe_push (stmt);
	  continue;
	}
      code = gimple_code (stmt);
      if (code == GIMPLE_COND || code == GIMPLE_CALL)
	{
	  gimple_set_plf (stmt, GF_PLF_2, true);
	  worklist.safe_push (stmt);
	  continue;
	}
      gimple_set_plf (stmt, GF_PLF_2, false);

      if (code == GIMPLE_ASSIGN)
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
	    {
	      stmt1 = USE_STMT (use_p);
	      if (gimple_bb (stmt1) != bb)
		{
		  gimple_set_plf (stmt, GF_PLF_2, true);
		  worklist.safe_push (stmt);
		  break;
		}
	    }
	}
    }
  /* Propagate liveness through arguments of live stmt.  */
  while (worklist.length () > 0)
    {
      ssa_op_iter iter;
      use_operand_p use_p;
      tree use;

      stmt = worklist.pop ();
      FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
	{
	  use = USE_FROM_PTR (use_p);
	  if (TREE_CODE (use) != SSA_NAME)
	    continue;
	  stmt1 = SSA_NAME_DEF_STMT (use);
	  if (gimple_bb (stmt1) != bb
	      || gimple_plf (stmt1, GF_PLF_2))
	    continue;
	  gimple_set_plf (stmt1, GF_PLF_2, true);
	  worklist.safe_push (stmt1);
	}
    }
  /* Delete dead statements.  */
  gsi = gsi_start_bb (bb);
  while (!gsi_end_p (gsi))
    {
      stmt = gsi_stmt (gsi);
      if (gimple_plf (stmt, GF_PLF_2))
	{
	  gsi_next (&gsi);
	  continue;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Delete dead stmt in bb#%d\n", bb->index);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      gsi_remove (&gsi, true);
      release_defs (stmt);
    }
}

/* If-convert LOOP when it is legal.  For the moment this pass has no
   profitability analysis.  Returns non-zero todo flags when something
   changed.  */

unsigned int
tree_if_conversion (struct loop *loop)
{
  unsigned int todo = 0;
  bool aggressive_if_conv;
  struct loop *rloop;

 again:
  rloop = NULL;
  ifc_bbs = NULL;
  any_pred_load_store = false;
  any_complicated_phi = false;

  /* Apply more aggressive if-conversion when loop or its outer loop were
     marked with simd pragma.  When that's the case, we try to if-convert
     loop containing PHIs with more than MAX_PHI_ARG_NUM arguments.  */
  aggressive_if_conv = loop->force_vectorize;
  if (!aggressive_if_conv)
    {
      struct loop *outer_loop = loop_outer (loop);
      if (outer_loop && outer_loop->force_vectorize)
	aggressive_if_conv = true;
    }

  if (!ifcvt_split_critical_edges (loop, aggressive_if_conv))
    goto cleanup;

  if (!if_convertible_loop_p (loop)
      || !dbg_cnt (if_conversion_tree))
    goto cleanup;

  if ((any_pred_load_store || any_complicated_phi)
      && ((!flag_tree_loop_vectorize && !loop->force_vectorize)
	  || loop->dont_vectorize))
    goto cleanup;

  /* Since we have no cost model, always version loops unless the user
     specified -ftree-loop-if-convert or unless versioning is required.
     Either version this loop, or if the pattern is right for outer-loop
     vectorization, version the outer loop.  In the latter case we will
     still if-convert the original inner loop.  */
  if (any_pred_load_store
      || any_complicated_phi
      || flag_tree_loop_if_convert != 1)
    {
      struct loop *vloop
	= (versionable_outer_loop_p (loop_outer (loop))
	   ? loop_outer (loop) : loop);
      struct loop *nloop = version_loop_for_if_conversion (vloop);
      if (nloop == NULL)
	goto cleanup;
      if (vloop != loop)
	{
	  /* If versionable_outer_loop_p decided to version the
	     outer loop, version also the inner loop of the non-vectorized
	     loop copy.  So we transform:
	      loop1
		loop2
	     into:
	      if (LOOP_VECTORIZED (1, 3))
		{
		  loop1
		    loop2
		}
	      else
		loop3 (copy of loop1)
		  if (LOOP_VECTORIZED (4, 5))
		    loop4 (copy of loop2)
		  else
		    loop5 (copy of loop4)  */
	  gcc_assert (nloop->inner && nloop->inner->next == NULL);
	  rloop = nloop->inner;
	}
    }

  /* Now all statements are if-convertible.  Combine all the basic
     blocks into one huge basic block doing the if-conversion
     on-the-fly.  */
  combine_blocks (loop);

  /* Delete dead predicate computations.  */
  ifcvt_local_dce (loop->header);

  todo |= TODO_cleanup_cfg;

 cleanup:
  if (ifc_bbs)
    {
      unsigned int i;

      for (i = 0; i < loop->num_nodes; i++)
	free_bb_predicate (ifc_bbs[i]);

      free (ifc_bbs);
      ifc_bbs = NULL;
    }
  if (rloop != NULL)
    {
      loop = rloop;
      goto again;
    }

  return todo;
}

/* Tree if-conversion pass management.  */

namespace {

const pass_data pass_data_if_conversion =
{
  GIMPLE_PASS, /* type */
  "ifcvt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_LOOP_IFCVT, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_if_conversion : public gimple_opt_pass
{
public:
  pass_if_conversion (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_if_conversion, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *);

}; // class pass_if_conversion

bool
pass_if_conversion::gate (function *fun)
{
  return (((flag_tree_loop_vectorize || fun->has_force_vectorize_loops)
	   && flag_tree_loop_if_convert != 0)
	  || flag_tree_loop_if_convert == 1);
}

unsigned int
pass_if_conversion::execute (function *fun)
{
  struct loop *loop;
  unsigned todo = 0;

  if (number_of_loops (fun) <= 1)
    return 0;

  FOR_EACH_LOOP (loop, 0)
    if (flag_tree_loop_if_convert == 1
	|| ((flag_tree_loop_vectorize || loop->force_vectorize)
	    && !loop->dont_vectorize))
      todo |= tree_if_conversion (loop);

  if (flag_checking)
    {
      basic_block bb;
      FOR_EACH_BB_FN (bb, fun)
	gcc_assert (!bb->aux);
    }

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_if_conversion (gcc::context *ctxt)
{
  return new pass_if_conversion (ctxt);
}
