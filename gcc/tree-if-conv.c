/* If-conversion for vectorizer.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "gimple.h"
#include "gimple-ssa.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssanames.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "dbgcnt.h"

/* List of basic blocks in if-conversion-suitable order.  */
static basic_block *ifc_bbs;

/* Structure used to predicate basic blocks.  This is attached to the
   ->aux field of the BBs in the loop to be if-converted.  */
typedef struct bb_predicate_s {

  /* The condition under which this basic block is executed.  */
  tree predicate;

  /* PREDICATE is gimplified, and the sequence of statements is
     recorded here, in order to avoid the duplication of computations
     that occur in previous conditions.  See PR44483.  */
  gimple_seq predicate_gimplified_stmts;
} *bb_predicate_p;

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
  return ((bb_predicate_p) bb->aux)->predicate;
}

/* Sets the gimplified predicate COND for basic block BB.  */

static inline void
set_bb_predicate (basic_block bb, tree cond)
{
  gcc_assert ((TREE_CODE (cond) == TRUTH_NOT_EXPR
	       && is_gimple_condexpr (TREE_OPERAND (cond, 0)))
	      || is_gimple_condexpr (cond));
  ((bb_predicate_p) bb->aux)->predicate = cond;
}

/* Returns the sequence of statements of the gimplification of the
   predicate for basic block BB.  */

static inline gimple_seq
bb_predicate_gimplified_stmts (basic_block bb)
{
  return ((bb_predicate_p) bb->aux)->predicate_gimplified_stmts;
}

/* Sets the sequence of statements STMTS of the gimplification of the
   predicate for basic block BB.  */

static inline void
set_bb_predicate_gimplified_stmts (basic_block bb, gimple_seq stmts)
{
  ((bb_predicate_p) bb->aux)->predicate_gimplified_stmts = stmts;
}

/* Adds the sequence of statements STMTS to the sequence of statements
   of the predicate for basic block BB.  */

static inline void
add_bb_predicate_gimplified_stmts (basic_block bb, gimple_seq stmts)
{
  gimple_seq_add_seq
    (&(((bb_predicate_p) bb->aux)->predicate_gimplified_stmts), stmts);
}

/* Initializes to TRUE the predicate of basic block BB.  */

static inline void
init_bb_predicate (basic_block bb)
{
  bb->aux = XNEW (struct bb_predicate_s);
  set_bb_predicate_gimplified_stmts (bb, NULL);
  set_bb_predicate (bb, boolean_true_node);
}

/* Free the predicate of basic block BB.  */

static inline void
free_bb_predicate (basic_block bb)
{
  gimple_seq stmts;

  if (!bb_has_predicate (bb))
    return;

  /* Release the SSA_NAMEs created for the gimplification of the
     predicate.  */
  stmts = bb_predicate_gimplified_stmts (bb);
  if (stmts)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start (stmts); !gsi_end_p (i); gsi_next (&i))
	free_stmt_operands (gsi_stmt (i));
    }

  free (bb->aux);
  bb->aux = NULL;
}

/* Free the predicate of BB and reinitialize it with the true
   predicate.  */

static inline void
reset_bb_predicate (basic_block bb)
{
  free_bb_predicate (bb);
  init_bb_predicate (bb);
}

/* Returns a new SSA_NAME of type TYPE that is assigned the value of
   the expression EXPR.  Inserts the statement created for this
   computation before GSI and leaves the iterator GSI at the same
   statement.  */

static tree
ifc_temp_var (tree type, tree expr, gimple_stmt_iterator *gsi)
{
  tree new_name = make_temp_ssa_name (type, NULL, "_ifc_");
  gimple stmt = gimple_build_assign (new_name, expr);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  return new_name;
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
  gimple s;

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
	    : invert_tree_comparison (code, HONOR_NANS (TYPE_MODE (type)));
	}

      return ERROR_MARK;
    }

  if (TREE_CODE_CLASS (TREE_CODE (cond)) == tcc_comparison)
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

/* Returns true if N is either a constant or a SSA_NAME.  */

static bool
constant_or_ssa_name (tree n)
{
  switch (TREE_CODE (n))
    {
      case SSA_NAME:
      case INTEGER_CST:
      case REAL_CST:
      case COMPLEX_CST:
      case VECTOR_CST:
	return true;
      default:
	return false;
    }
}

/* Returns either a COND_EXPR or the folded expression if the folded
   expression is a MIN_EXPR, a MAX_EXPR, an ABS_EXPR,
   a constant or a SSA_NAME. */

static tree
fold_build_cond_expr (tree type, tree cond, tree rhs, tree lhs)
{
  tree rhs1, lhs1, cond_expr;
  cond_expr = fold_ternary (COND_EXPR, type, cond,
			    rhs, lhs);

  if (cond_expr == NULL_TREE)
    return build3 (COND_EXPR, type, cond, rhs, lhs);

  STRIP_USELESS_TYPE_CONVERSION (cond_expr);

  if (constant_or_ssa_name (cond_expr))
    return cond_expr;

  if (TREE_CODE (cond_expr) == ABS_EXPR)
    {
      rhs1 = TREE_OPERAND (cond_expr, 1);
      STRIP_USELESS_TYPE_CONVERSION (rhs1);
      if (constant_or_ssa_name (rhs1))
	return build1 (ABS_EXPR, type, rhs1);
    }

  if (TREE_CODE (cond_expr) == MIN_EXPR
      || TREE_CODE (cond_expr) == MAX_EXPR)
    {
      lhs1 = TREE_OPERAND (cond_expr, 0);
      STRIP_USELESS_TYPE_CONVERSION (lhs1);
      rhs1 = TREE_OPERAND (cond_expr, 1);
      STRIP_USELESS_TYPE_CONVERSION (rhs1);
      if (constant_or_ssa_name (rhs1)
	  && constant_or_ssa_name (lhs1))
	return build2 (TREE_CODE (cond_expr), type, lhs1, rhs1);
    }
  return build3 (COND_EXPR, type, cond, rhs, lhs);
}

/* Add condition NC to the predicate list of basic block BB.  */

static inline void
add_to_predicate_list (basic_block bb, tree nc)
{
  tree bc, *tp;

  if (is_true_predicate (nc))
    return;

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

  add_to_predicate_list (e->dest, cond);
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

/* Return true when PHI is if-convertible.  PHI is part of loop LOOP
   and it belongs to basic block BB.

   PHI is not if-convertible if:
   - it has more than 2 arguments.

   When the flag_tree_loop_if_convert_stores is not set, PHI is not
   if-convertible if:
   - a virtual PHI is immediately used in another PHI node,
   - there is a virtual PHI in a BB other than the loop->header.  */

static bool
if_convertible_phi_p (struct loop *loop, basic_block bb, gimple phi)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "-------------------------\n");
      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
    }

  if (bb != loop->header && gimple_phi_num_args (phi) != 2)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "More than two phi node args.\n");
      return false;
    }

  if (flag_tree_loop_if_convert_stores)
    return true;

  /* When the flag_tree_loop_if_convert_stores is not set, check
     that there are no memory writes in the branches of the loop to be
     if-converted.  */
  if (virtual_operand_p (gimple_phi_result (phi)))
    {
      imm_use_iterator imm_iter;
      use_operand_p use_p;

      if (bb != loop->header)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Virtual phi not on loop->header.\n");
	  return false;
	}

      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, gimple_phi_result (phi))
	{
	  if (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Difficult to handle this virtual phi.\n");
	      return false;
	    }
	}
    }

  return true;
}

/* Records the status of a data reference.  This struct is attached to
   each DR->aux field.  */

struct ifc_dr {
  /* -1 when not initialized, 0 when false, 1 when true.  */
  int written_at_least_once;

  /* -1 when not initialized, 0 when false, 1 when true.  */
  int rw_unconditionally;
};

#define IFC_DR(DR) ((struct ifc_dr *) (DR)->aux)
#define DR_WRITTEN_AT_LEAST_ONCE(DR) (IFC_DR (DR)->written_at_least_once)
#define DR_RW_UNCONDITIONALLY(DR) (IFC_DR (DR)->rw_unconditionally)

/* Returns true when the memory references of STMT are read or written
   unconditionally.  In other words, this function returns true when
   for every data reference A in STMT there exist other accesses to
   a data reference with the same base with predicates that add up (OR-up) to
   the true predicate: this ensures that the data reference A is touched
   (read or written) on every iteration of the if-converted loop.  */

static bool
memrefs_read_or_written_unconditionally (gimple stmt,
					 vec<data_reference_p> drs)
{
  int i, j;
  data_reference_p a, b;
  tree ca = bb_predicate (gimple_bb (stmt));

  for (i = 0; drs.iterate (i, &a); i++)
    if (DR_STMT (a) == stmt)
      {
	bool found = false;
	int x = DR_RW_UNCONDITIONALLY (a);

	if (x == 0)
	  return false;

	if (x == 1)
	  continue;

	for (j = 0; drs.iterate (j, &b); j++)
          {
            tree ref_base_a = DR_REF (a);
            tree ref_base_b = DR_REF (b);

            if (DR_STMT (b) == stmt)
              continue;

            while (TREE_CODE (ref_base_a) == COMPONENT_REF
                   || TREE_CODE (ref_base_a) == IMAGPART_EXPR
                   || TREE_CODE (ref_base_a) == REALPART_EXPR)
              ref_base_a = TREE_OPERAND (ref_base_a, 0);

            while (TREE_CODE (ref_base_b) == COMPONENT_REF
                   || TREE_CODE (ref_base_b) == IMAGPART_EXPR
                   || TREE_CODE (ref_base_b) == REALPART_EXPR)
              ref_base_b = TREE_OPERAND (ref_base_b, 0);

  	    if (!operand_equal_p (ref_base_a, ref_base_b, 0))
	      {
	        tree cb = bb_predicate (gimple_bb (DR_STMT (b)));

	        if (DR_RW_UNCONDITIONALLY (b) == 1
		    || is_true_predicate (cb)
		    || is_true_predicate (ca
                        = fold_or_predicates (EXPR_LOCATION (cb), ca, cb)))
		  {
		    DR_RW_UNCONDITIONALLY (a) = 1;
  		    DR_RW_UNCONDITIONALLY (b) = 1;
		    found = true;
		    break;
		  }
               }
	    }

	if (!found)
	  {
	    DR_RW_UNCONDITIONALLY (a) = 0;
	    return false;
	  }
      }

  return true;
}

/* Returns true when the memory references of STMT are unconditionally
   written.  In other words, this function returns true when for every
   data reference A written in STMT, there exist other writes to the
   same data reference with predicates that add up (OR-up) to the true
   predicate: this ensures that the data reference A is written on
   every iteration of the if-converted loop.  */

static bool
write_memrefs_written_at_least_once (gimple stmt,
				     vec<data_reference_p> drs)
{
  int i, j;
  data_reference_p a, b;
  tree ca = bb_predicate (gimple_bb (stmt));

  for (i = 0; drs.iterate (i, &a); i++)
    if (DR_STMT (a) == stmt
	&& DR_IS_WRITE (a))
      {
	bool found = false;
	int x = DR_WRITTEN_AT_LEAST_ONCE (a);

	if (x == 0)
	  return false;

	if (x == 1)
	  continue;

	for (j = 0; drs.iterate (j, &b); j++)
	  if (DR_STMT (b) != stmt
	      && DR_IS_WRITE (b)
	      && same_data_refs_base_objects (a, b))
	    {
	      tree cb = bb_predicate (gimple_bb (DR_STMT (b)));

	      if (DR_WRITTEN_AT_LEAST_ONCE (b) == 1
		  || is_true_predicate (cb)
		  || is_true_predicate (ca = fold_or_predicates (EXPR_LOCATION (cb),
								 ca, cb)))
		{
		  DR_WRITTEN_AT_LEAST_ONCE (a) = 1;
		  DR_WRITTEN_AT_LEAST_ONCE (b) = 1;
		  found = true;
		  break;
		}
	    }

	if (!found)
	  {
	    DR_WRITTEN_AT_LEAST_ONCE (a) = 0;
	    return false;
	  }
      }

  return true;
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
   iteration unconditionally.  */

static bool
ifcvt_memrefs_wont_trap (gimple stmt, vec<data_reference_p> refs)
{
  return write_memrefs_written_at_least_once (stmt, refs)
    && memrefs_read_or_written_unconditionally (stmt, refs);
}

/* Wrapper around gimple_could_trap_p refined for the needs of the
   if-conversion.  Try to prove that the memory accesses of STMT could
   not trap in the innermost loop containing STMT.  */

static bool
ifcvt_could_trap_p (gimple stmt, vec<data_reference_p> refs)
{
  if (gimple_vuse (stmt)
      && !gimple_could_trap_p_1 (stmt, false, false)
      && ifcvt_memrefs_wont_trap (stmt, refs))
    return false;

  return gimple_could_trap_p (stmt);
}

/* Return true when STMT is if-convertible.

   GIMPLE_ASSIGN statement is not if-convertible if,
   - it is not movable,
   - it could trap,
   - LHS is not var decl.  */

static bool
if_convertible_gimple_assign_stmt_p (gimple stmt,
				     vec<data_reference_p> refs)
{
  tree lhs = gimple_assign_lhs (stmt);
  basic_block bb;

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

  if (flag_tree_loop_if_convert_stores)
    {
      if (ifcvt_could_trap_p (stmt, refs))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "tree could trap...\n");
	  return false;
	}
      return true;
    }

  if (gimple_assign_rhs_could_trap_p (stmt))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "tree could trap...\n");
      return false;
    }

  bb = gimple_bb (stmt);

  if (TREE_CODE (lhs) != SSA_NAME
      && bb != bb->loop_father->header
      && !bb_with_exit_edge_p (bb->loop_father, bb))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "LHS is not var\n");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      return false;
    }

  return true;
}

/* Return true when STMT is if-convertible.

   A statement is if-convertible if:
   - it is an if-convertible GIMPLE_ASSIGN,
   - it is a GIMPLE_LABEL or a GIMPLE_COND.  */

static bool
if_convertible_stmt_p (gimple stmt, vec<data_reference_p> refs)
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
      break;
    }

  return true;
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

  if (EDGE_COUNT (bb->preds) > 2
      || EDGE_COUNT (bb->succs) > 2)
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

  /* At least one incoming edge has to be non-critical as otherwise edge
     predicates are not equal to basic-block predicates of the edge
     source.  */
  if (EDGE_COUNT (bb->preds) > 1
      && bb != loop->header)
    {
      bool found = false;
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (EDGE_COUNT (e->src->succs) == 1)
	  found = true;
      if (!found)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "only critical predecessors\n");
	  return false;
	}
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
  gcc_assert (loop->latch != EXIT_BLOCK_PTR);

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

static bool
predicate_bbs (loop_p loop)
{
  unsigned int i;

  for (i = 0; i < loop->num_nodes; i++)
    init_bb_predicate (ifc_bbs[i]);

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      tree cond;
      gimple_stmt_iterator itr;

      /* The loop latch is always executed and has no extra conditions
	 to be processed: skip it.  */
      if (bb == loop->latch)
	{
	  reset_bb_predicate (loop->latch);
	  continue;
	}

      cond = bb_predicate (bb);

      for (itr = gsi_start_bb (bb); !gsi_end_p (itr); gsi_next (&itr))
	{
	  gimple stmt = gsi_stmt (itr);

	  switch (gimple_code (stmt))
	    {
	    case GIMPLE_LABEL:
	    case GIMPLE_ASSIGN:
	    case GIMPLE_CALL:
	    case GIMPLE_DEBUG:
	      break;

	    case GIMPLE_COND:
	      {
		tree c2;
		edge true_edge, false_edge;
		location_t loc = gimple_location (stmt);
		tree c = fold_build2_loc (loc, gimple_cond_code (stmt),
					  boolean_type_node,
					  gimple_cond_lhs (stmt),
					  gimple_cond_rhs (stmt));

		/* Add new condition into destination's predicate list.  */
		extract_true_false_edges_from_block (gimple_bb (stmt),
						     &true_edge, &false_edge);

		/* If C is true, then TRUE_EDGE is taken.  */
		add_to_dst_predicate_list (loop, true_edge,
					   unshare_expr (cond),
					   unshare_expr (c));

		/* If C is false, then FALSE_EDGE is taken.  */
		c2 = build1_loc (loc, TRUTH_NOT_EXPR,
				 boolean_type_node, unshare_expr (c));
		add_to_dst_predicate_list (loop, false_edge,
					   unshare_expr (cond), c2);

		cond = NULL_TREE;
		break;
	      }

	    default:
	      /* Not handled yet in if-conversion.  */
	      return false;
	    }
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

	  add_to_predicate_list (bb_n, cond);
	}
    }

  /* The loop header is always executed.  */
  reset_bb_predicate (loop->header);
  gcc_assert (bb_predicate_gimplified_stmts (loop->header) == NULL
	      && bb_predicate_gimplified_stmts (loop->latch) == NULL);

  return true;
}

/* Return true when LOOP is if-convertible.  This is a helper function
   for if_convertible_loop_p.  REFS and DDRS are initialized and freed
   in if_convertible_loop_p.  */

static bool
if_convertible_loop_p_1 (struct loop *loop,
			 vec<loop_p> *loop_nest,
			 vec<data_reference_p> *refs,
			 vec<ddr_p> *ddrs)
{
  bool res;
  unsigned int i;
  basic_block exit_bb = NULL;

  /* Don't if-convert the loop when the data dependences cannot be
     computed: the loop won't be vectorized in that case.  */
  res = compute_data_dependences_for_loop (loop, true, loop_nest, refs, ddrs);
  if (!res)
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

  res = predicate_bbs (loop);
  if (!res)
    return false;

  if (flag_tree_loop_if_convert_stores)
    {
      data_reference_p dr;

      for (i = 0; refs->iterate (i, &dr); i++)
	{
	  dr->aux = XNEW (struct ifc_dr);
	  DR_WRITTEN_AT_LEAST_ONCE (dr) = -1;
	  DR_RW_UNCONDITIONALLY (dr) = -1;
	}
    }

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = ifc_bbs[i];
      gimple_stmt_iterator itr;

      for (itr = gsi_start_phis (bb); !gsi_end_p (itr); gsi_next (&itr))
	if (!if_convertible_phi_p (loop, bb, gsi_stmt (itr)))
	  return false;

      /* Check the if-convertibility of statements in predicated BBs.  */
      if (is_predicated (bb))
	for (itr = gsi_start_bb (bb); !gsi_end_p (itr); gsi_next (&itr))
	  if (!if_convertible_stmt_p (gsi_stmt (itr), *refs))
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
  vec<ddr_p> ddrs;

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
  ddrs.create (25);
  stack_vec<loop_p, 3> loop_nest;
  res = if_convertible_loop_p_1 (loop, &loop_nest, &refs, &ddrs);

  if (flag_tree_loop_if_convert_stores)
    {
      data_reference_p dr;
      unsigned int i;

      for (i = 0; refs.iterate (i, &dr); i++)
	free (dr->aux);
    }

  loop_nest.release ();
  free_data_refs (refs);
  free_dependence_relations (ddrs);
  return res;
}

/* Basic block BB has two predecessors.  Using predecessor's bb
   predicate, set an appropriate condition COND for the PHI node
   replacement.  Return the true block whose phi arguments are
   selected when cond is true.  LOOP is the loop containing the
   if-converted region, GSI is the place to insert the code for the
   if-conversion.  */

static basic_block
find_phi_replacement_condition (basic_block bb, tree *cond,
				gimple_stmt_iterator *gsi)
{
  edge first_edge, second_edge;
  tree tmp_cond;

  gcc_assert (EDGE_COUNT (bb->preds) == 2);
  first_edge = EDGE_PRED (bb, 0);
  second_edge = EDGE_PRED (bb, 1);

  /* Prefer an edge with a not negated predicate.
     ???  That's a very weak cost model.  */
  tmp_cond = bb_predicate (first_edge->src);
  gcc_assert (tmp_cond);
  if (TREE_CODE (tmp_cond) == TRUTH_NOT_EXPR)
    {
      edge tmp_edge;

      tmp_edge = first_edge;
      first_edge = second_edge;
      second_edge = tmp_edge;
    }

  /* Check if the edge we take the condition from is not critical.
     We know that at least one non-critical edge exists.  */
  if (EDGE_COUNT (first_edge->src->succs) > 1)
    {
      *cond = bb_predicate (second_edge->src);

      if (TREE_CODE (*cond) == TRUTH_NOT_EXPR)
	*cond = TREE_OPERAND (*cond, 0);
      else
	/* Select non loop header bb.  */
	first_edge = second_edge;
    }
  else
    *cond = bb_predicate (first_edge->src);

  /* Gimplify the condition to a valid cond-expr conditonal operand.  */
  *cond = force_gimple_operand_gsi_1 (gsi, unshare_expr (*cond),
				      is_gimple_condexpr, NULL_TREE,
				      true, GSI_SAME_STMT);

  return first_edge->src;
}

/* Replace a scalar PHI node with a COND_EXPR using COND as condition.
   This routine does not handle PHI nodes with more than two
   arguments.

   For example,
     S1: A = PHI <x1(1), x2(5)>
   is converted into,
     S2: A = cond ? x1 : x2;

   The generated code is inserted at GSI that points to the top of
   basic block's statement list.  When COND is true, phi arg from
   TRUE_BB is selected.  */

static void
predicate_scalar_phi (gimple phi, tree cond,
		      basic_block true_bb,
		      gimple_stmt_iterator *gsi)
{
  gimple new_stmt;
  basic_block bb;
  tree rhs, res, arg, scev;

  gcc_assert (gimple_code (phi) == GIMPLE_PHI
	      && gimple_phi_num_args (phi) == 2);

  res = gimple_phi_result (phi);
  /* Do not handle virtual phi nodes.  */
  if (virtual_operand_p (res))
    return;

  bb = gimple_bb (phi);

  if ((arg = degenerate_phi_result (phi))
      || ((scev = analyze_scalar_evolution (gimple_bb (phi)->loop_father,
					    res))
	  && !chrec_contains_undetermined (scev)
	  && scev != res
	  && (arg = gimple_phi_arg_def (phi, 0))))
    rhs = arg;
  else
    {
      tree arg_0, arg_1;
      /* Use condition that is not TRUTH_NOT_EXPR in conditional modify expr.  */
      if (EDGE_PRED (bb, 1)->src == true_bb)
	{
	  arg_0 = gimple_phi_arg_def (phi, 1);
	  arg_1 = gimple_phi_arg_def (phi, 0);
	}
      else
	{
	  arg_0 = gimple_phi_arg_def (phi, 0);
	  arg_1 = gimple_phi_arg_def (phi, 1);
	}

      /* Build new RHS using selected condition and arguments.  */
      rhs = fold_build_cond_expr (TREE_TYPE (res), unshare_expr (cond),
				  arg_0, arg_1);
    }

  new_stmt = gimple_build_assign (res, rhs);
  SSA_NAME_DEF_STMT (gimple_phi_result (phi)) = new_stmt;
  gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
  update_stmt (new_stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "new phi replacement stmt\n");
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
      gimple phi;
      tree cond = NULL_TREE;
      gimple_stmt_iterator gsi, phi_gsi;
      basic_block true_bb = NULL;
      bb = ifc_bbs[i];

      if (bb == loop->header)
	continue;

      phi_gsi = gsi_start_phis (bb);
      if (gsi_end_p (phi_gsi))
	continue;

      /* BB has two predecessors.  Using predecessor's aux field, set
	 appropriate condition for the PHI node replacement.  */
      gsi = gsi_after_labels (bb);
      true_bb = find_phi_replacement_condition (bb, &cond, &gsi);

      while (!gsi_end_p (phi_gsi))
	{
	  phi = gsi_stmt (phi_gsi);
	  predicate_scalar_phi (phi, cond, true_bb, &gsi);
	  release_phi_node (phi);
	  gsi_next (&phi_gsi);
	}

      set_phi_nodes (bb, NULL);
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
	  if (flag_tree_loop_if_convert_stores)
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

  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gimple_stmt_iterator gsi;
      basic_block bb = ifc_bbs[i];
      tree cond = bb_predicate (bb);
      bool swap;
      gimple stmt;

      if (is_true_predicate (cond))
	continue;

      swap = false;
      if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
	{
	  swap = true;
	  cond = TREE_OPERAND (cond, 0);
	}

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	if ((stmt = gsi_stmt (gsi))
	    && gimple_assign_single_p (stmt)
	    && gimple_vdef (stmt))
	  {
	    tree lhs = gimple_assign_lhs (stmt);
	    tree rhs = gimple_assign_rhs1 (stmt);
	    tree type = TREE_TYPE (lhs);

	    lhs = ifc_temp_var (type, unshare_expr (lhs), &gsi);
	    rhs = ifc_temp_var (type, unshare_expr (rhs), &gsi);
	    if (swap)
	      {
		tree tem = lhs;
		lhs = rhs;
		rhs = tem;
	      }
	    cond = force_gimple_operand_gsi_1 (&gsi, unshare_expr (cond),
					       is_gimple_condexpr, NULL_TREE,
					       true, GSI_SAME_STMT);
	    rhs = fold_build_cond_expr (type, unshare_expr (cond), rhs, lhs);
	    gimple_assign_set_rhs1 (stmt, ifc_temp_var (type, rhs, &gsi));
	    update_stmt (stmt);
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

  if (flag_tree_loop_if_convert_stores)
    predicate_mem_writes (loop);

  /* Merge basic blocks: first remove all the edges in the loop,
     except for those from the exit block.  */
  exit_bb = NULL;
  for (i = 0; i < orig_loop_num_nodes; i++)
    {
      bb = ifc_bbs[i];
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
	  make_edge (loop->header, exit_bb, EDGE_FALLTHRU);
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
  for (i = 1; i < orig_loop_num_nodes; i++)
    {
      gimple_stmt_iterator gsi;
      gimple_stmt_iterator last;

      bb = ifc_bbs[i];

      if (bb == exit_bb || bb == loop->latch)
	continue;

      /* Make stmts member of loop->header.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	gimple_set_bb (gsi_stmt (gsi), merge_target_bb);

      /* Update stmt list.  */
      last = gsi_last_bb (merge_target_bb);
      gsi_insert_seq_after (&last, bb_seq (bb), GSI_NEW_STMT);
      set_bb_seq (bb, NULL);

      delete_basic_block (bb);
    }

  /* If possible, merge loop header to the block with the exit edge.
     This reduces the number of basic blocks to two, to please the
     vectorizer that handles only loops with two nodes.  */
  if (exit_bb
      && exit_bb != loop->header
      && can_merge_blocks_p (loop->header, exit_bb))
    merge_blocks (loop->header, exit_bb);

  free (ifc_bbs);
  ifc_bbs = NULL;
}

/* If-convert LOOP when it is legal.  For the moment this pass has no
   profitability analysis.  Returns true when something changed.  */

static bool
tree_if_conversion (struct loop *loop)
{
  bool changed = false;
  ifc_bbs = NULL;

  if (!if_convertible_loop_p (loop)
      || !dbg_cnt (if_conversion_tree))
    goto cleanup;

  /* Now all statements are if-convertible.  Combine all the basic
     blocks into one huge basic block doing the if-conversion
     on-the-fly.  */
  combine_blocks (loop);

  if (flag_tree_loop_if_convert_stores)
    mark_virtual_operands_for_renaming (cfun);

  changed = true;

 cleanup:
  if (ifc_bbs)
    {
      unsigned int i;

      for (i = 0; i < loop->num_nodes; i++)
	free_bb_predicate (ifc_bbs[i]);

      free (ifc_bbs);
      ifc_bbs = NULL;
    }

  return changed;
}

/* Tree if-conversion pass management.  */

static unsigned int
main_tree_if_conversion (void)
{
  loop_iterator li;
  struct loop *loop;
  bool changed = false;
  unsigned todo = 0;

  if (number_of_loops (cfun) <= 1)
    return 0;

  FOR_EACH_LOOP (li, loop, 0)
    if (flag_tree_loop_if_convert == 1
	|| flag_tree_loop_if_convert_stores == 1
	|| flag_tree_loop_vectorize
	|| loop->force_vect)
    changed |= tree_if_conversion (loop);

  if (changed)
    todo |= TODO_cleanup_cfg;

  if (changed && flag_tree_loop_if_convert_stores)
    todo |= TODO_update_ssa_only_virtuals;

#ifdef ENABLE_CHECKING
  {
    basic_block bb;
    FOR_EACH_BB (bb)
      gcc_assert (!bb->aux);
  }
#endif

  return todo;
}

/* Returns true when the if-conversion pass is enabled.  */

static bool
gate_tree_if_conversion (void)
{
  return (((flag_tree_loop_vectorize || cfun->has_force_vect_loops)
	   && flag_tree_loop_if_convert != 0)
	  || flag_tree_loop_if_convert == 1
	  || flag_tree_loop_if_convert_stores == 1);
}

namespace {

const pass_data pass_data_if_conversion =
{
  GIMPLE_PASS, /* type */
  "ifcvt", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_stmts | TODO_verify_flow
    | TODO_verify_ssa ), /* todo_flags_finish */
};

class pass_if_conversion : public gimple_opt_pass
{
public:
  pass_if_conversion (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_if_conversion, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_tree_if_conversion (); }
  unsigned int execute () { return main_tree_if_conversion (); }

}; // class pass_if_conversion

} // anon namespace

gimple_opt_pass *
make_pass_if_conversion (gcc::context *ctxt)
{
  return new pass_if_conversion (ctxt);
}
