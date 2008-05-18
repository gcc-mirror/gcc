/* Dead code elimination pass for the GNU compiler.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   Contributed by Ben Elliston <bje@redhat.com>
   and Andrew MacLeod <amacleod@redhat.com>
   Adapted to use control dependence by Steven Bosscher, SUSE Labs.
 
This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Dead code elimination.

   References:

     Building an Optimizing Compiler,
     Robert Morgan, Butterworth-Heinemann, 1998, Section 8.9.

     Advanced Compiler Design and Implementation,
     Steven Muchnick, Morgan Kaufmann, 1997, Section 18.10.

   Dead-code elimination is the removal of statements which have no
   impact on the program's output.  "Dead statements" have no impact
   on the program's output, while "necessary statements" may have
   impact on the output.

   The algorithm consists of three phases:
   1. Marking as necessary all statements known to be necessary,
      e.g. most function calls, writing a value to memory, etc;
   2. Propagating necessary statements, e.g., the statements
      giving values to operands in necessary statements; and
   3. Removing dead statements.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "diagnostic.h"
#include "toplev.h"
/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "obstack.h"
#include "basic-block.h"

#include "tree.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "flags.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"

static struct stmt_stats
{
  int total;
  int total_phis;
  int removed;
  int removed_phis;
} stats;


static VEC (tree, heap) *worklist;
static VEC (tree, heap) *cond_dead_built_in_calls;

/* Vector indicating an SSA name has already been processed and marked
   as necessary.  */
static sbitmap processed;

/* Vector indicating that last_stmt if a basic block has already been
   marked as necessary.  */
static sbitmap last_stmt_necessary;

/* Before we can determine whether a control branch is dead, we need to
   compute which blocks are control dependent on which edges.

   We expect each block to be control dependent on very few edges so we
   use a bitmap for each block recording its edges.  An array holds the
   bitmap.  The Ith bit in the bitmap is set if that block is dependent
   on the Ith edge.  */
static bitmap *control_dependence_map;

/* Vector indicating that a basic block has already had all the edges
   processed that it is control dependent on.  */
static sbitmap visited_control_parents;

/* TRUE if this pass alters the CFG (by removing control statements).
   FALSE otherwise.

   If this pass alters the CFG, then it will arrange for the dominators
   to be recomputed.  */
static bool cfg_altered;

/* Execute code that follows the macro for each edge (given number
   EDGE_NUMBER within the CODE) for which the block with index N is
   control dependent.  */
#define EXECUTE_IF_CONTROL_DEPENDENT(BI, N, EDGE_NUMBER)	\
  EXECUTE_IF_SET_IN_BITMAP (control_dependence_map[(N)], 0,	\
			    (EDGE_NUMBER), (BI))


/* Indicate block BB is control dependent on an edge with index EDGE_INDEX.  */
static inline void
set_control_dependence_map_bit (basic_block bb, int edge_index)
{
  if (bb == ENTRY_BLOCK_PTR)
    return;
  gcc_assert (bb != EXIT_BLOCK_PTR);
  bitmap_set_bit (control_dependence_map[bb->index], edge_index);
}

/* Clear all control dependences for block BB.  */
static inline void
clear_control_dependence_bitmap (basic_block bb)
{
  bitmap_clear (control_dependence_map[bb->index]);
}


/* Find the immediate postdominator PDOM of the specified basic block BLOCK.
   This function is necessary because some blocks have negative numbers.  */

static inline basic_block
find_pdom (basic_block block)
{
  gcc_assert (block != ENTRY_BLOCK_PTR);

  if (block == EXIT_BLOCK_PTR)
    return EXIT_BLOCK_PTR;
  else
    {
      basic_block bb = get_immediate_dominator (CDI_POST_DOMINATORS, block);
      if (! bb)
	return EXIT_BLOCK_PTR;
      return bb;
    }
}


/* Determine all blocks' control dependences on the given edge with edge_list
   EL index EDGE_INDEX, ala Morgan, Section 3.6.  */

static void
find_control_dependence (struct edge_list *el, int edge_index)
{
  basic_block current_block;
  basic_block ending_block;

  gcc_assert (INDEX_EDGE_PRED_BB (el, edge_index) != EXIT_BLOCK_PTR);

  if (INDEX_EDGE_PRED_BB (el, edge_index) == ENTRY_BLOCK_PTR)
    ending_block = single_succ (ENTRY_BLOCK_PTR);
  else
    ending_block = find_pdom (INDEX_EDGE_PRED_BB (el, edge_index));

  for (current_block = INDEX_EDGE_SUCC_BB (el, edge_index);
       current_block != ending_block && current_block != EXIT_BLOCK_PTR;
       current_block = find_pdom (current_block))
    {
      edge e = INDEX_EDGE (el, edge_index);

      /* For abnormal edges, we don't make current_block control
	 dependent because instructions that throw are always necessary
	 anyway.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      set_control_dependence_map_bit (current_block, edge_index);
    }
}


/* Record all blocks' control dependences on all edges in the edge
   list EL, ala Morgan, Section 3.6.  */

static void
find_all_control_dependences (struct edge_list *el)
{
  int i;

  for (i = 0; i < NUM_EDGES (el); ++i)
    find_control_dependence (el, i);
}


#define NECESSARY(stmt)		stmt->base.asm_written_flag

/*  Conditional dead call elimination 

   Some builtin functions can set errno on error conditions, but they
   are otherwise pure. If the result of a call to such a function is 
   not used, the compiler can still not eliminate the call without 
   powerful interprocedural analysis to prove that the errno is not
   checked. However, if the conditions under which the error occurs
   are known, compiler can conditionally dead code eliminate the calls
   by shrink-wrapping the semi-dead calls into the error condition:
      
        built_in_call(args) 
          ==>
        if (error_cond(args))
             built_in_call(args)

    An actual simple exampl is :
         log (x);   // Mostly dead call
     ==>
         if (x < 0)
             log(x);
     With this change, call to log(x) is effectively eliminated, as
     in majority of the cases, log won't be called with x out of 
     range. The branch is totally predicatible, so the branch cost
     is low.  Such optimization improves the performance of  
     an important application in a big search company. 
   
   Note that library functions are not supposed to clear errno to zero without
   error.
   
   In this implementation, only 'pow' and 'log' are handled. ('sin' and 'cos' 
   seem to be wrongly handled by gcc -- they are treated as not touching errno
   which is not correct.)
   
   The condition wrapping the builtin call is conservatively set to avoid too
   aggressive (wrong) shrink wrapping. The optimization is called conditional
   dead call elimination because the call is eliminated under the condition 
   that the input arguments would not lead to domain or range error (for 
   instance when x <= 0 for a log(x) call), however the chances that the error
   condition is hit is very low (those builtin calls which are conditionally 
   dead are usually part of the C++ abstraction penalty exposed after 
   inlining).  */


/* A helper method to help select calls to pow that are suitable for
   conditional DCE transformation. Returns true if the pow call is 
   a candidate.*/

static bool
check_pow (tree pow_call)
{
  tree base, expn;
  enum tree_code bc, ec;

  gcc_assert (TREE_CODE (pow_call) == CALL_EXPR);
  if (call_expr_nargs (pow_call) != 2)
    return false;

  base = CALL_EXPR_ARG (pow_call, 0);
  expn = CALL_EXPR_ARG (pow_call, 1);

  bc = TREE_CODE (base);
  ec = TREE_CODE (expn);
 
  gcc_assert (TREE_CODE_CLASS (bc) != tcc_constant 
             || bc == REAL_CST);
  gcc_assert (TREE_CODE_CLASS (ec) != tcc_constant 
             || ec == REAL_CST);

  /* Folding candidates are not interesting.  */
  if (ec == REAL_CST && bc == REAL_CST)
    return false;

  if (bc == REAL_CST)
   {
     /* Only handle a fixed range of constant.  */
     REAL_VALUE_TYPE mv;
     REAL_VALUE_TYPE bcv = TREE_REAL_CST (base);
     if (REAL_VALUES_EQUAL (bcv, dconst1))
       return false;
     if (REAL_VALUES_LESS (bcv, dconst1))
       return false;
     real_from_integer (&mv, VOIDmode,256,0,1);
     if (REAL_VALUES_LESS (mv, bcv))
       return false;
     return true;
   }
  else if (bc == SSA_NAME)
   {
     tree def, nm, rhs, rhs0, var, typ;
     int sz;

     def = SSA_NAME_DEF_STMT (base);
     if (TREE_CODE (def) != GIMPLE_MODIFY_STMT)
       return false;

     nm = GIMPLE_STMT_OPERAND (def,0);
     gcc_assert (TREE_CODE (nm) == SSA_NAME);
     if (nm != base) 
       return false;

     rhs = GIMPLE_STMT_OPERAND (def,1);
     
     if (TREE_CODE (rhs) != FLOAT_EXPR)
       return false;
     rhs0 = TREE_OPERAND (rhs,0);

     if (TREE_CODE (rhs0) != SSA_NAME)
       return false;

     var = SSA_NAME_VAR (rhs0);
     if (TREE_CODE (var) != VAR_DECL &&
         TREE_CODE (var) != PARM_DECL)
       return false;

     typ = TREE_TYPE (var);
     if (TREE_CODE (typ) != INTEGER_TYPE)
       return false;
     sz = int_size_in_bytes (typ);
     if (sz == -1 || sz > INT_TYPE_SIZE) 
       return false;

     return true;
   }
  else
    return false;
}

/* A helper function to help select candidate calls to log that are 
   suitable for conditional DCE. Returns true if the log call is a
    candidate. */

static bool
check_log (tree log_call)
{
  tree arg_typ;
  gcc_assert (TREE_CODE (log_call) == CALL_EXPR);
  if (call_expr_nargs (log_call) != 1)
    return false;

  arg_typ = TREE_TYPE (CALL_EXPR_ARG (log_call, 0));
  if (!is_gimple_reg_type (arg_typ))
    return false;
  return true;
}


/* A helper function to determine if a builtin function call is a 
   candidate for conditional DCE. Returns true if the builtin call
   is a candidate. */

static bool
is_unnecessary_except_errno_call (tree call)
{
  tree fn;
  bool is_unnecessary_except_errno = false;
  enum built_in_function fnc;

  if (!flag_tree_builtin_dce) 
    return false;

  gcc_assert (call && TREE_CODE (call) == CALL_EXPR);

  fn = get_callee_fndecl (call);
  if (!fn || !DECL_BUILT_IN (fn)) 
    return false;

  fnc = DECL_FUNCTION_CODE (fn);
  switch (fnc)
    {
    CASE_FLT_FN (BUILT_IN_POW): 
      if (check_pow (call))
        is_unnecessary_except_errno = true;
      break;

    CASE_FLT_FN (BUILT_IN_LOG):
      if (check_log (call))
        is_unnecessary_except_errno = true;
      break;
    default : 
      is_unnecessary_except_errno = false;
      break;
   }

  return is_unnecessary_except_errno;
}


/* If STMT is not already marked necessary, mark it, and add it to the
   worklist if ADD_TO_WORKLIST is true.  */
static inline void
mark_stmt_necessary (tree stmt, bool add_to_worklist)
{
  gcc_assert (stmt);
  gcc_assert (!DECL_P (stmt));

  if (NECESSARY (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Marking useful stmt: ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  NECESSARY (stmt) = 1;
  if (add_to_worklist)
    VEC_safe_push (tree, heap, worklist, stmt);
}


/* Mark the statement defining operand OP as necessary.  */

static inline void
mark_operand_necessary (tree op)
{
  tree stmt;
  int ver;

  gcc_assert (op);

  ver = SSA_NAME_VERSION (op);
  if (TEST_BIT (processed, ver))
    return;
  SET_BIT (processed, ver);

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (stmt);

  if (NECESSARY (stmt) || IS_EMPTY_STMT (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
   {
     fprintf (dump_file, " Marked as necessary: ");
     print_generic_stmt (dump_file, stmt, TDF_SLIM);
     fprintf (dump_file, "\n");
   }

  NECESSARY (stmt) = 1;
  VEC_safe_push (tree, heap, worklist, stmt);
}


/* Mark STMT as necessary if it obviously is.  Add it to the worklist if
   it can make other statements necessary.

   If AGGRESSIVE is false, control statements are conservatively marked as
   necessary.  */

static void
mark_stmt_if_obviously_necessary (tree stmt, bool aggressive)
{
  stmt_ann_t ann;
  tree op;

  /* With non-call exceptions, we have to assume that all statements could
     throw.  If a statement may throw, it is inherently necessary.  */
  if (flag_non_call_exceptions
      && tree_could_throw_p (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  /* Statements that are implicitly live.  Most function calls, asm and return
     statements are required.  Labels and BIND_EXPR nodes are kept because
     they are control flow, and we have no way of knowing whether they can be
     removed.  DCE can eliminate all the other statements in a block, and CFG
     can then remove the block and labels.  */
  switch (TREE_CODE (stmt))
    {
    case PREDICT_EXPR:
    case LABEL_EXPR:
    case CASE_LABEL_EXPR:
      mark_stmt_necessary (stmt, false);
      return;

    case ASM_EXPR:
    case RESX_EXPR:
    case RETURN_EXPR:
    case CHANGE_DYNAMIC_TYPE_EXPR:
      mark_stmt_necessary (stmt, true);
      return;

    case CALL_EXPR:
      /* Most, but not all function calls are required.  Function calls that
	 produce no result and have no side effects (i.e. const pure
	 functions) are unnecessary.  */
      if (TREE_SIDE_EFFECTS (stmt)) 
        mark_stmt_necessary (stmt, true);
      
      return;

    case GIMPLE_MODIFY_STMT:
      op = get_call_expr_in (stmt);
      if (op && TREE_SIDE_EFFECTS (op))
	{
          mark_stmt_necessary (stmt, true);
	  return;
	}

      /* These values are mildly magic bits of the EH runtime.  We can't
	 see the entire lifetime of these values until landing pads are
	 generated.  */
      if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) == EXC_PTR_EXPR
	  || TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) == FILTER_EXPR)
	{
	  mark_stmt_necessary (stmt, true);
	  return;
	}
      break;

    case GOTO_EXPR:
      gcc_assert (!simple_goto_p (stmt));
      mark_stmt_necessary (stmt, true);
      return;

    case COND_EXPR:
      gcc_assert (EDGE_COUNT (bb_for_stmt (stmt)->succs) == 2);
      /* Fall through.  */

    case SWITCH_EXPR:
      if (! aggressive)
	mark_stmt_necessary (stmt, true);
      break;

    default:
      break;
    }

  ann = stmt_ann (stmt);

  /* If the statement has volatile operands, it needs to be preserved.
     Same for statements that can alter control flow in unpredictable
     ways.  */
  if (ann->has_volatile_ops || is_ctrl_altering_stmt (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  if (is_hidden_global_store (stmt))
    {
      mark_stmt_necessary (stmt, true);
      return;
    }

  return;
}


/* Make corresponding control dependent edges necessary.  We only
   have to do this once for each basic block, so we clear the bitmap
   after we're done.  */
static void
mark_control_dependent_edges_necessary (basic_block bb, struct edge_list *el)
{
  bitmap_iterator bi;
  unsigned edge_number;

  gcc_assert (bb != EXIT_BLOCK_PTR);

  if (bb == ENTRY_BLOCK_PTR)
    return;

  EXECUTE_IF_CONTROL_DEPENDENT (bi, bb->index, edge_number)
    {
      tree t;
      basic_block cd_bb = INDEX_EDGE_PRED_BB (el, edge_number);

      if (TEST_BIT (last_stmt_necessary, cd_bb->index))
	continue;
      SET_BIT (last_stmt_necessary, cd_bb->index);

      t = last_stmt (cd_bb);
      if (t && is_ctrl_stmt (t))
	mark_stmt_necessary (t, true);
    }
}


/* Find obviously necessary statements.  These are things like most function
   calls, and stores to file level variables.

   If EL is NULL, control statements are conservatively marked as
   necessary.  Otherwise it contains the list of edges used by control
   dependence analysis.  */

static void
find_obviously_necessary_stmts (struct edge_list *el)
{
  basic_block bb;
  block_stmt_iterator i;
  edge e;

  FOR_EACH_BB (bb)
    {
      tree phi;

      /* PHI nodes are never inherently necessary.  */
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	NECESSARY (phi) = 0;

      /* Check all statements in the block.  */
      for (i = bsi_start (bb); ! bsi_end_p (i); bsi_next (&i))
	{
	  tree stmt = bsi_stmt (i);
	  NECESSARY (stmt) = 0;
	  mark_stmt_if_obviously_necessary (stmt, el != NULL);
	}
    }

  if (el)
    {
      /* Prevent the loops from being removed.  We must keep the infinite loops,
	 and we currently do not have a means to recognize the finite ones.  */
      FOR_EACH_BB (bb)
	{
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_DFS_BACK)
	      mark_control_dependent_edges_necessary (e->dest, el);
	}
    }
}


/* Propagate necessity using the operands of necessary statements.
   Process the uses on each statement in the worklist, and add all
   feeding statements which contribute to the calculation of this
   value to the worklist. 

   In conservative mode, EL is NULL.  */

static void
propagate_necessity (struct edge_list *el)
{
  tree stmt;
  bool aggressive = (el ? true : false); 

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nProcessing worklist:\n");

  while (VEC_length (tree, worklist) > 0)
    {
      /* Take STMT from worklist.  */
      stmt = VEC_pop (tree, worklist);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "processing: ");
	  print_generic_stmt (dump_file, stmt, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      if (aggressive)
	{
	  /* Mark the last statements of the basic blocks that the block
	     containing STMT is control dependent on, but only if we haven't
	     already done so.  */
	  basic_block bb = bb_for_stmt (stmt);
	  if (bb != ENTRY_BLOCK_PTR
	      && ! TEST_BIT (visited_control_parents, bb->index))
	    {
	      SET_BIT (visited_control_parents, bb->index);
	      mark_control_dependent_edges_necessary (bb, el);
	    }
	}

      if (TREE_CODE (stmt) == PHI_NODE)
	{
	  /* PHI nodes are somewhat special in that each PHI alternative has
	     data and control dependencies.  All the statements feeding the
	     PHI node's arguments are always necessary.  In aggressive mode,
	     we also consider the control dependent edges leading to the
	     predecessor block associated with each PHI alternative as
	     necessary.  */
	  int k;

	  for (k = 0; k < PHI_NUM_ARGS (stmt); k++)
            {
	      tree arg = PHI_ARG_DEF (stmt, k);
	      if (TREE_CODE (arg) == SSA_NAME)
		mark_operand_necessary (arg);
	    }

	  if (aggressive)
	    {
	      for (k = 0; k < PHI_NUM_ARGS (stmt); k++)
		{
		  basic_block arg_bb = PHI_ARG_EDGE (stmt, k)->src;
		  if (arg_bb != ENTRY_BLOCK_PTR
		      && ! TEST_BIT (visited_control_parents, arg_bb->index))
		    {
		      SET_BIT (visited_control_parents, arg_bb->index);
		      mark_control_dependent_edges_necessary (arg_bb, el);
		    }
		}
	    }
	}
      else
	{
	  /* Propagate through the operands.  Examine all the USE, VUSE and
	     VDEF operands in this statement.  Mark all the statements 
	     which feed this statement's uses as necessary.  The
	     operands of VDEF expressions are also needed as they
	     represent potential definitions that may reach this
	     statement (VDEF operands allow us to follow def-def
	     links).  */
	  ssa_op_iter iter;
	  tree use;

	  FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_ALL_USES)
	    mark_operand_necessary (use);
	}
    }
}

/* Method to generate conditional statements for guarding condionally
   dead calls to pow. One or more statements can be generated for 
   each logical condition. Statement groups of different conditions
   are separated by a NULL tree and they are stored in the VEC 
   conds. The number of logical conditions are stored in *nconds. */
static void
gen_conditions_for_pow (tree pow_call, enum built_in_function fnc, 
                        VEC (tree, heap)* conds, unsigned * nconds)
{
  tree base, expn;
  enum tree_code bc, ec;
  gcc_assert (TREE_CODE (pow_call) == CALL_EXPR);
  gcc_assert (call_expr_nargs (pow_call) == 2);
  gcc_assert (fnc == BUILT_IN_POW);

  *nconds = 0;

  base = CALL_EXPR_ARG (pow_call, 0);
  expn = CALL_EXPR_ARG (pow_call, 1);

  bc = TREE_CODE (base);
  ec = TREE_CODE (expn);
  
  gcc_assert (TREE_CODE_CLASS (bc) != tcc_constant ||
             bc == REAL_CST);
  gcc_assert (TREE_CODE_CLASS (ec) != tcc_constant ||
             ec == REAL_CST);

  if (bc == REAL_CST)
   {
     tree float_typ, max_exp_real_cst;
     tree temp, tempn, tempc, tempcn, stmt1, stmt2, stmt3;
     REAL_VALUE_TYPE mv;

     /* See candidate selection in check_pow. 
        Since the candidates have a given range
        of base values, the guard code generated
        for such calls: pow(const,y) are simple:
           if ( y > max_y )
               pow(const, y);
        max_y can be computed separately for each
        const base, but in this implemetation, we
        choose to compute it using the max base
        in the allowed range.  */

     REAL_VALUE_TYPE bcv = TREE_REAL_CST (base);
     gcc_assert (!REAL_VALUES_EQUAL (bcv, dconst1));
     gcc_assert (!REAL_VALUES_LESS (bcv, dconst1));
     real_from_integer (&mv, VOIDmode,256,0,1),
     gcc_assert (!REAL_VALUES_LESS (mv, bcv));
     float_typ = TREE_TYPE (expn); 

     max_exp_real_cst = build_real (float_typ, mv);
     temp = create_tmp_var (float_typ, "DCE_COND");
     stmt1 = build_gimple_modify_stmt (temp, expn);
     tempn = make_ssa_name (temp, stmt1);
     GIMPLE_STMT_OPERAND (stmt1, 0) = tempn;

     tempc = create_tmp_var (boolean_type_node, "DCE_COND_TEST");
     stmt2 = build_gimple_modify_stmt (tempc, 
                                       build2 (GT_EXPR, 
                                               boolean_type_node, 
                                               tempn, max_exp_real_cst));
     tempcn = make_ssa_name (tempc, stmt2);
     GIMPLE_STMT_OPERAND (stmt2, 0) = tempcn;

     stmt3 = build3 (COND_EXPR, void_type_node,
                     tempcn, NULL_TREE, NULL_TREE);
     VEC_safe_push (tree, heap, conds, stmt1);
     VEC_safe_push (tree, heap, conds, stmt2);
     VEC_safe_push (tree, heap, conds, stmt3);
     (*nconds)++;

   }
  else if (bc == SSA_NAME)
   {
     tree def, nm, rhs, rhs0, var, int_typ, float_typ;
     tree max_exp_cst, max_exp_real_cst;
     tree temp1, temp1n, temp2, temp2n, temp2c, temp2cn; 
     tree cst0, stmt1, stmt2, stmt3;
     int sz, max_exp;

     /* Generate error condition code for pow calls with
        non constant base value. The candidates selected
        have their base argument value converted from
        integer (see check_pow) value (1,2,4 bytes), and
        the max exp value is computed based on the size
        of the integer type.  The code below first does
        sanity check and then does code generation.  */

     def = SSA_NAME_DEF_STMT (base);
     gcc_assert (TREE_CODE (def) == GIMPLE_MODIFY_STMT);

     nm = GIMPLE_STMT_OPERAND (def,0);
     gcc_assert (TREE_CODE (nm) == SSA_NAME);
     gcc_assert (nm == base); 

     rhs = GIMPLE_STMT_OPERAND (def,1);
     
     gcc_assert (TREE_CODE (rhs) == FLOAT_EXPR);
     rhs0 = TREE_OPERAND (rhs,0);
     gcc_assert (TREE_CODE (rhs0) == SSA_NAME);

     var = SSA_NAME_VAR (rhs0);
     gcc_assert (TREE_CODE (var) == VAR_DECL 
                 || TREE_CODE (var) == PARM_DECL);
     
     int_typ = TREE_TYPE (var);
     gcc_assert (TREE_CODE (int_typ) == INTEGER_TYPE);

     sz = int_size_in_bytes (int_typ);
     gcc_assert (sz > 0 && sz <= INT_TYPE_SIZE) ;


     float_typ = TREE_TYPE (SSA_NAME_VAR (expn)); 
     if (sz == 1)
       max_exp = 128;
     else if (sz == 2)
       max_exp = 64;
     else 
      {
        gcc_assert (sz == 4);
        max_exp = 32;
      }
     max_exp_cst = build_int_cst (integer_type_node, max_exp);
     max_exp_real_cst = build_real_from_int_cst (float_typ, max_exp_cst);
     
     /* For pow ((dobule)x,y), generate the following conditions:
      cond 1:
        temp1 = x;
        if (temp1 <= 0)

      cond 2:
        temp2 = y;
        if (temp2 > max_exp_real_cst)  */

     temp2 = create_tmp_var (float_typ, "DCE_COND2");
     stmt1 = build_gimple_modify_stmt (temp2, expn);
     temp2n = make_ssa_name (temp2, stmt1);
     GIMPLE_STMT_OPERAND (stmt1,0) = temp2n;

     temp2c = create_tmp_var (boolean_type_node, "DCE_COND2_TEST");
     stmt2 = build_gimple_modify_stmt (temp2c, 
                                       build2 (GT_EXPR, 
                                               boolean_type_node, 
                                               temp2n, max_exp_real_cst));
     temp2cn = make_ssa_name (temp2c, stmt2);
     GIMPLE_STMT_OPERAND (stmt2, 0) = temp2cn;

     stmt3 = build3 (COND_EXPR, void_type_node,
                     temp2cn, NULL_TREE, NULL_TREE);
     VEC_safe_push (tree, heap, conds, stmt1);
     VEC_safe_push (tree, heap, conds, stmt2);
     VEC_safe_push (tree, heap, conds, stmt3);
     (*nconds)++;

     /* Now a seperator*/
     VEC_safe_push (tree, heap, conds, NULL);

     temp1 = create_tmp_var (int_typ, "DCE_COND1");
     cst0 = build_int_cst (int_typ, 0);
     stmt1 = build_gimple_modify_stmt (temp1, rhs0);
     temp1n = make_ssa_name (temp1, stmt1);
     GIMPLE_STMT_OPERAND (stmt1,0) = temp1n;
     stmt2 = build3 (COND_EXPR, void_type_node,
                     build2 (LE_EXPR, boolean_type_node, temp1n, cst0),
                     NULL_TREE, NULL_TREE);

     VEC_safe_push (tree, heap, conds, stmt1);
     VEC_safe_push (tree, heap, conds, stmt2);
     (*nconds)++;

   }
  else
    gcc_unreachable ();
}

/* The method to generate error condition guard code for log(x)
   calls. */
static void
gen_conditions_for_log (tree call, enum built_in_function fnc, 
                        VEC (tree, heap)* conds, unsigned * nconds)
{
  tree arg, cst0, temp, tempn, tempc, tempcn, stmt1, stmt2, stmt3;
  gcc_assert (TREE_CODE (call) == CALL_EXPR);
  gcc_assert (fnc == BUILT_IN_LOG || fnc == BUILT_IN_LOGF || fnc == BUILT_IN_LOGL);

  *nconds = 0;

  /* for log(x), 
   Generate condition

   temp = x
   if (x <= 0)
  */
  arg = CALL_EXPR_ARG (call, 0);
  cst0 = build_real (TREE_TYPE (arg), dconst0);
  temp = create_tmp_var (TREE_TYPE (arg), "DCE_COND");
  stmt1 = build_gimple_modify_stmt (temp, arg);
  tempn = make_ssa_name (temp, stmt1);
  GIMPLE_STMT_OPERAND (stmt1,0) = tempn;

  tempc = create_tmp_var (boolean_type_node, "DCE_COND_TEST");
  stmt2 = build_gimple_modify_stmt (tempc, 
                                    build2 (LE_EXPR, 
                                            boolean_type_node, 
                                            tempn, cst0));
  tempcn = make_ssa_name (tempc, stmt2);
  GIMPLE_STMT_OPERAND (stmt2, 0) = tempcn;

  stmt3 = build3 (COND_EXPR, void_type_node, tempcn,
                       NULL_TREE, NULL_TREE);

  VEC_safe_push (tree, heap, conds, stmt1);
  VEC_safe_push (tree, heap, conds, stmt2);
  VEC_safe_push (tree, heap, conds, stmt3);
  (*nconds)++;

}


/*  The method to generate shrink wrap conditions for partially 
    a dead builtin call whose return value is not used anywhere,
    but has to be kept live due to potential error condition.  

    BI_CALL:  the builtin call
    CONDS  :  the vector of statements for condition code
    NCODES :  the pointer to the number of logical conditions, 
              which may be different from the length of CONDS
              vector. Statements belonging to different logical
              condition are separated by NULL tree in the vector
*/

static void
gen_shrink_wrap_conditions (tree bi_call, VEC (tree, heap)* conds, unsigned int * nconds)
{
  tree call, fn;
  enum built_in_function fnc; 
  gcc_assert (nconds && conds);
  gcc_assert (VEC_length(tree, conds) == 0);
  gcc_assert (TREE_CODE (bi_call) == GIMPLE_MODIFY_STMT 
              || TREE_CODE (bi_call) == CALL_EXPR);

  call = bi_call;
  if (TREE_CODE (call) == GIMPLE_MODIFY_STMT)
    call = get_call_expr_in (bi_call);

  fn = get_callee_fndecl (call);
  gcc_assert (fn && DECL_BUILT_IN (fn)); 
  fnc = DECL_FUNCTION_CODE (fn);
  *nconds = 0;

  switch (fnc)
    {
     case BUILT_IN_POW:
       gen_conditions_for_pow (call, fnc, conds, nconds);
       break;
     case BUILT_IN_LOG:
     case BUILT_IN_LOGF:
     case BUILT_IN_LOGL:
       gen_conditions_for_log (call, fnc, conds, nconds);
       break;
     default : 
       gcc_unreachable();
     break;
   }

  gcc_assert (*nconds);
  return;
}


/* Propability of the branch (to the call) is taken. */
#define ERR_PROB 0.01

/*  The method to shrink wrap a partially  dead builtin call 
    whose return value is not used anywhere, but has to be kept 
    live due to potential error condition.  */
static void
shrink_wrap_one_built_in_call (tree bi_call)
{
  block_stmt_iterator bi_call_bsi, join_tgt_bsi;
  basic_block bi_call_bb, join_tgt_bb, guard_bb, guard_bb0;
  edge join_tgt_in_edge_from_call, join_tgt_in_edge_fall_thru;
  tree join_tgt_label_decl, join_tgt_label;
  edge bi_call_in_edge0, guard_bb_in_edge;
  VEC (tree, heap) *conds;
  unsigned tn_cond_stmts, nconds;
  unsigned ci;
  tree cond_expr = NULL;
  tree cond_expr_start;
  tree bi_call_label_decl;
  tree bi_call_label;

  conds = VEC_alloc (tree, heap, 10);
  gen_shrink_wrap_conditions (bi_call, conds, &nconds);

  gcc_assert (nconds > 0);
  /* Make sure no reallocation happens. */
  gcc_assert (VEC_length (tree, conds) <= 10);
  gcc_assert (VEC_length (tree, conds) >= nconds);
  bi_call_bb = bb_for_stmt (bi_call);

  /* Now find the join target bb -- split 
     bi_call_bb if needed */
  bi_call_bsi = bsi_for_stmt (bi_call);

  gcc_assert (!bsi_end_p (bi_call_bsi));
  join_tgt_in_edge_from_call = split_block (bi_call_bb, bi_call);
  bi_call_bsi = bsi_for_stmt (bi_call);

  join_tgt_bb = join_tgt_in_edge_from_call->dest;
  join_tgt_label_decl = create_artificial_label ();
  join_tgt_label = build1 (LABEL_EXPR, void_type_node, join_tgt_label_decl);
  join_tgt_bsi = bsi_start (join_tgt_bb);
  bsi_insert_before (&join_tgt_bsi, join_tgt_label, BSI_SAME_STMT);

  /* Now it is time to insert the first condtional expression 
     into bi_call_bb and split this bb so that bi_call is 
     shrink-wrapped*/
  tn_cond_stmts = VEC_length (tree, conds);
  cond_expr = NULL;
  cond_expr_start = VEC_index (tree, conds,0);
  for (ci = 0; ci < tn_cond_stmts; ci++)
   {
     tree c = VEC_index (tree, conds, ci);
     gcc_assert ( c || ci != 0 );
     if (!c) 
       break;
     bsi_insert_before (&bi_call_bsi, c, BSI_SAME_STMT);
     cond_expr = c;
   }
  nconds --;
  ci ++;
  gcc_assert (cond_expr && TREE_CODE (cond_expr) == COND_EXPR);

  /* Now the label*/
  bi_call_label_decl = create_artificial_label ();
  bi_call_label = build1 (LABEL_EXPR, void_type_node, bi_call_label_decl);
  bsi_insert_before (&bi_call_bsi, bi_call_label, BSI_SAME_STMT);

  bi_call_in_edge0 = split_block (bi_call_bb, cond_expr);
  bi_call_in_edge0->flags &= ~EDGE_FALLTHRU;
  bi_call_in_edge0->flags |= EDGE_TRUE_VALUE;
  guard_bb0 = bi_call_bb;
  bi_call_bb = bi_call_in_edge0->dest;
  join_tgt_in_edge_fall_thru = make_edge (guard_bb0, join_tgt_bb, EDGE_FALSE_VALUE);

  bi_call_in_edge0->probability = REG_BR_PROB_BASE*ERR_PROB;
  join_tgt_in_edge_fall_thru->probability = 
      REG_BR_PROB_BASE - bi_call_in_edge0->probability;

  /* Code generation for the rest of the conditions */
  guard_bb = guard_bb0;
  for (; nconds > 0; )
   {
     unsigned ci0;
     edge bi_call_in_edge; 
     block_stmt_iterator guard_bsi = bsi_for_stmt (cond_expr_start);
     ci0 = ci;
     cond_expr_start = VEC_index (tree, conds, ci0);
     for (; ci < tn_cond_stmts; ci++)
      {
        tree c = VEC_index (tree, conds, ci);
        gcc_assert ( c || ci != ci0 );
        if (!c) 
          break;
        bsi_insert_before (&guard_bsi, c, BSI_SAME_STMT);
        cond_expr = c;
      }
     nconds --;
     ci ++;
     gcc_assert (cond_expr && TREE_CODE (cond_expr) == COND_EXPR);
     guard_bb_in_edge = split_block (guard_bb, cond_expr);
     guard_bb_in_edge->flags &= ~EDGE_FALLTHRU;
     guard_bb_in_edge->flags |= EDGE_FALSE_VALUE;

     bi_call_in_edge = make_edge (guard_bb, bi_call_bb, EDGE_TRUE_VALUE);

     bi_call_in_edge->probability = REG_BR_PROB_BASE * ERR_PROB;
     guard_bb_in_edge->probability = 
         REG_BR_PROB_BASE - bi_call_in_edge->probability;

   }

  VEC_free (tree, heap, conds);
  if (dump_file && (dump_flags & TDF_DETAILS))
   {
     location_t loc;
     loc = EXPR_LOCATION (bi_call);
     inform (
         "%Hfunction call is shrink-wrapped into error conditions.",
         &loc);
   }
}

/*  The top level method for conditional dead code shrink 
    wrapping transformation.  */

static bool
shrink_wrap_conditional_dead_built_in_calls (void)
{
  unsigned i = 0;
  unsigned n = VEC_length (tree, cond_dead_built_in_calls);
  if (n == 0) return false;

  for (; i < n ; i++)
   {
     tree bi_call = VEC_index (tree,  cond_dead_built_in_calls, i);
     shrink_wrap_one_built_in_call (bi_call);
   }

  cfg_altered = true;

  return true;
}

/* Remove dead PHI nodes from block BB.  */

static bool
remove_dead_phis (basic_block bb)
{
  tree prev, phi;
  bool something_changed = false;

  prev = NULL_TREE;
  phi = phi_nodes (bb);
  while (phi)
    {
      stats.total_phis++;

      if (! NECESSARY (phi))
	{
	  tree next = PHI_CHAIN (phi);

	  something_changed = true;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Deleting : ");
	      print_generic_stmt (dump_file, phi, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }

	  remove_phi_node (phi, prev, true);
	  stats.removed_phis++;
	  phi = next;
	}
      else
	{
	  prev = phi;
	  phi = PHI_CHAIN (phi);
	}
    }
  return something_changed;
}


/* Remove dead statement pointed to by iterator I.  Receives the basic block BB
   containing I so that we don't have to look it up.  */

static void
remove_dead_stmt (block_stmt_iterator *i, basic_block bb)
{
  tree t = bsi_stmt (*i);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Deleting : ");
      print_generic_stmt (dump_file, t, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  stats.removed++;

  /* If we have determined that a conditional branch statement contributes
     nothing to the program, then we not only remove it, but we also change
     the flow graph so that the current block will simply fall-thru to its
     immediate post-dominator.  The blocks we are circumventing will be
     removed by cleanup_tree_cfg if this change in the flow graph makes them
     unreachable.  */
  if (is_ctrl_stmt (t))
    {
      basic_block post_dom_bb;

      /* The post dominance info has to be up-to-date.  */
      gcc_assert (dom_info_state (CDI_POST_DOMINATORS) == DOM_OK);
      /* Get the immediate post dominator of bb.  */
      post_dom_bb = get_immediate_dominator (CDI_POST_DOMINATORS, bb);

      /* There are three particularly problematical cases.

	 1. Blocks that do not have an immediate post dominator.  This
	    can happen with infinite loops.

	 2. Blocks that are only post dominated by the exit block.  These
	    can also happen for infinite loops as we create fake edges
	    in the dominator tree.

	 3. If the post dominator has PHI nodes we may be able to compute
	    the right PHI args for them.

	 In each of these cases we must remove the control statement
	 as it may reference SSA_NAMEs which are going to be removed and
	 we remove all but one outgoing edge from the block.  */
      if (! post_dom_bb
	  || post_dom_bb == EXIT_BLOCK_PTR
	  || phi_nodes (post_dom_bb))
	;
      else
	{
	  /* Redirect the first edge out of BB to reach POST_DOM_BB.  */
	  redirect_edge_and_branch (EDGE_SUCC (bb, 0), post_dom_bb);
	  PENDING_STMT (EDGE_SUCC (bb, 0)) = NULL;

	  /* It is not sufficient to set cfg_altered below during edge
	     removal, in case BB has two successors and one of them
	     is POST_DOM_BB.  */
	  cfg_altered = true;
	}
      EDGE_SUCC (bb, 0)->probability = REG_BR_PROB_BASE;
      EDGE_SUCC (bb, 0)->count = bb->count;

      /* The edge is no longer associated with a conditional, so it does
	 not have TRUE/FALSE flags.  */
      EDGE_SUCC (bb, 0)->flags &= ~(EDGE_TRUE_VALUE | EDGE_FALSE_VALUE);

      /* The lone outgoing edge from BB will be a fallthru edge.  */
      EDGE_SUCC (bb, 0)->flags |= EDGE_FALLTHRU;

      /* Remove the remaining the outgoing edges.  */
      while (!single_succ_p (bb))
	{
	  /* FIXME.  When we remove the edge, we modify the CFG, which
	     in turn modifies the dominator and post-dominator tree.
	     Is it safe to postpone recomputing the dominator and
	     post-dominator tree until the end of this pass given that
	     the post-dominators are used above?  */
	  cfg_altered = true;
          remove_edge (EDGE_SUCC (bb, 1));
	}
    }
  
  bsi_remove (i, true);  
  release_defs (t); 
}


/* Eliminate unnecessary statements. Any instruction not marked as necessary
   contributes nothing to the program, and can be deleted.  */

static bool
eliminate_unnecessary_stmts (void)
{
  bool something_changed = false;
  basic_block bb;
  block_stmt_iterator i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nEliminating unnecessary statements:\n");

  clear_special_calls ();
  FOR_EACH_BB (bb)
    {
      /* Remove dead PHI nodes.  */
      something_changed |= remove_dead_phis (bb);
    }

  FOR_EACH_BB (bb)
    {
      /* Remove dead statements.  */
      for (i = bsi_start (bb); ! bsi_end_p (i) ; )
	{
	  tree t = bsi_stmt (i);

	  stats.total++;

	  /* If `i' is not necessary then remove it.  */
	  if (! NECESSARY (t))
	    {
	      remove_dead_stmt (&i, bb);
	      something_changed = true;
	    }
	  else
	    {
	      tree call = get_call_expr_in (t);
	      if (call)
		{
		  tree name;

		  /* When LHS of var = call (); is dead, simplify it into
		     call (); saving one operand.  */
		  if (TREE_CODE (t) == GIMPLE_MODIFY_STMT
		      && (TREE_CODE ((name = GIMPLE_STMT_OPERAND (t, 0)))
			  == SSA_NAME)
		      && !TEST_BIT (processed, SSA_NAME_VERSION (name)))
		    {
		      tree oldlhs = GIMPLE_STMT_OPERAND (t, 0);
		      something_changed = true;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  fprintf (dump_file, "Deleting LHS of call: ");
			  print_generic_stmt (dump_file, t, TDF_SLIM);
			  fprintf (dump_file, "\n");
			}

                      if (is_unnecessary_except_errno_call (call))
                       {
                         if (dump_file && (dump_flags & TDF_DETAILS))
			  {
                            fprintf (dump_file, "Found conditional dead call: ");
                            print_generic_stmt (dump_file, t, TDF_SLIM);
                            fprintf (dump_file, "\n");
                          }
                         VEC_safe_push (tree, heap, cond_dead_built_in_calls, call);
                       }

		      push_stmt_changes (bsi_stmt_ptr (i));
		      TREE_BLOCK (call) = TREE_BLOCK (t);
		      bsi_replace (&i, call, false);
		      maybe_clean_or_replace_eh_stmt (t, call);
		      mark_symbols_for_renaming (call);
		      pop_stmt_changes (bsi_stmt_ptr (i));
		      release_ssa_name (oldlhs);


		    }
		  notice_special_calls (call);
		}
	      bsi_next (&i);
	    }
	}
    }

  something_changed |= 
      shrink_wrap_conditional_dead_built_in_calls ();

  return something_changed;
}


/* Print out removed statement statistics.  */

static void
print_stats (void)
{
  if (dump_file && (dump_flags & (TDF_STATS|TDF_DETAILS)))
    {
      float percg;

      percg = ((float) stats.removed / (float) stats.total) * 100;
      fprintf (dump_file, "Removed %d of %d statements (%d%%)\n",
	       stats.removed, stats.total, (int) percg);

      if (stats.total_phis == 0)
	percg = 0;
      else
	percg = ((float) stats.removed_phis / (float) stats.total_phis) * 100;

      fprintf (dump_file, "Removed %d of %d PHI nodes (%d%%)\n",
	       stats.removed_phis, stats.total_phis, (int) percg);
    }
}

/* Initialization for this pass.  Set up the used data structures.  */

static void
tree_dce_init (bool aggressive)
{
  memset ((void *) &stats, 0, sizeof (stats));

  if (aggressive)
    {
      int i;

      control_dependence_map = XNEWVEC (bitmap, last_basic_block);
      for (i = 0; i < last_basic_block; ++i)
	control_dependence_map[i] = BITMAP_ALLOC (NULL);

      last_stmt_necessary = sbitmap_alloc (last_basic_block);
      sbitmap_zero (last_stmt_necessary);
    }

  processed = sbitmap_alloc (num_ssa_names + 1);
  sbitmap_zero (processed);

  worklist = VEC_alloc (tree, heap, 64);
  cond_dead_built_in_calls = VEC_alloc (tree, heap,64);
  cfg_altered = false;
  
}

/* Cleanup after this pass.  */

static void
tree_dce_done (bool aggressive)
{
  if (aggressive)
    {
      int i;

      for (i = 0; i < last_basic_block; ++i)
	BITMAP_FREE (control_dependence_map[i]);
      free (control_dependence_map);

      sbitmap_free (visited_control_parents);
      sbitmap_free (last_stmt_necessary);
    }

  sbitmap_free (processed);

  VEC_free (tree, heap, worklist);
  VEC_free (tree, heap, cond_dead_built_in_calls);
}


/* Main routine to eliminate dead code.

   AGGRESSIVE controls the aggressiveness of the algorithm.
   In conservative mode, we ignore control dependence and simply declare
   all but the most trivially dead branches necessary.  This mode is fast.
   In aggressive mode, control dependences are taken into account, which
   results in more dead code elimination, but at the cost of some time.

   FIXME: Aggressive mode before PRE doesn't work currently because
	  the dominance info is not invalidated after DCE1.  This is
	  not an issue right now because we only run aggressive DCE
	  as the last tree SSA pass, but keep this in mind when you
	  start experimenting with pass ordering.  */

static unsigned int
perform_tree_ssa_dce (bool aggressive)
{
  struct edge_list *el = NULL;
  bool something_changed = 0;

  tree_dce_init (aggressive);

  if (aggressive)
    {
      /* Compute control dependence.  */
      timevar_push (TV_CONTROL_DEPENDENCES);
      calculate_dominance_info (CDI_POST_DOMINATORS);
      el = create_edge_list ();
      find_all_control_dependences (el);
      timevar_pop (TV_CONTROL_DEPENDENCES);

      visited_control_parents = sbitmap_alloc (last_basic_block);
      sbitmap_zero (visited_control_parents);

      mark_dfs_back_edges ();
    }

  find_obviously_necessary_stmts (el);

  propagate_necessity (el);
  
  something_changed |= eliminate_unnecessary_stmts ();
  something_changed |= cfg_altered;

  /* We do not update postdominators, so free them unconditionally.  */
  free_dominance_info (CDI_POST_DOMINATORS);

  /* If we removed paths in the CFG, then we need to update
     dominators as well.  I haven't investigated the possibility
     of incrementally updating dominators.  */
  if (cfg_altered)
    free_dominance_info (CDI_DOMINATORS);

  /* Debugging dumps.  */
  if (dump_file)
    print_stats ();

  tree_dce_done (aggressive);

  free_edge_list (el);

  if (something_changed)
    return (TODO_update_ssa | TODO_cleanup_cfg | TODO_ggc_collect 
	    | TODO_remove_unused_locals);
  else
    return 0;
}

/* Pass entry points.  */
static unsigned int
tree_ssa_dce (void)
{
  return perform_tree_ssa_dce (/*aggressive=*/false);
}

static unsigned int
tree_ssa_dce_loop (void)
{
  unsigned int todo;
  todo = perform_tree_ssa_dce (/*aggressive=*/false);
  if (todo)
    {
      free_numbers_of_iterations_estimates ();
      scev_reset ();
    }
  return todo;
}

static unsigned int
tree_ssa_cd_dce (void)
{
  return perform_tree_ssa_dce (/*aggressive=*/optimize >= 2);
}

static bool
gate_dce (void)
{
  return flag_tree_dce != 0;
}

struct gimple_opt_pass pass_dce =
{
 {
  GIMPLE_PASS,
  "dce",				/* name */
  gate_dce,				/* gate */
  tree_ssa_dce,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_DCE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_dce_loop =
{
 {
  GIMPLE_PASS,
  "dceloop",				/* name */
  gate_dce,				/* gate */
  tree_ssa_dce_loop,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_DCE,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
 }
};

struct gimple_opt_pass pass_cd_dce =
{
 {
  GIMPLE_PASS,
  "cddce",				/* name */
  gate_dce,				/* gate */
  tree_ssa_cd_dce,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CD_DCE,			/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa
  | TODO_verify_flow			/* todo_flags_finish */
 }
};
