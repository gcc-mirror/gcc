/* Conditional constant propagation pass for the GNU compiler.
   Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Adapted from original RTL SSA-CCP by Daniel Berlin <dberlin@dberlin.org>
   Adapted to GIMPLE trees by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.
   
GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.
   
GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.
   
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* Conditional constant propagation.

   References:

     Constant propagation with conditional branches,
     Wegman and Zadeck, ACM TOPLAS 13(2):181-210.

     Building an Optimizing Compiler,
     Robert Morgan, Butterworth-Heinemann, 1998, Section 8.9.

     Advanced Compiler Design and Implementation,
     Steven Muchnick, Morgan Kaufmann, 1997, Section 12.6  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "langhooks.h"

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"

#include "diagnostic.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-dump.h"
#include "tree-pass.h"
#include "timevar.h"
#include "expr.h"
#include "flags.h"


/* Possible lattice values.  */
typedef enum
{
  UNINITIALIZED = 0,
  UNDEFINED,
  CONSTANT,
  VARYING
} latticevalue;

/* Use the TREE_VISITED bitflag to mark statements and PHI nodes that have
   been deemed VARYING and shouldn't be simulated again.  */
#define DONT_SIMULATE_AGAIN(T)	TREE_VISITED (T)

/* Main structure for CCP.  Contains the lattice value and, if it's a
    constant, the constant value.  */
typedef struct
{
  latticevalue lattice_val;
  tree const_val;
} value;

/* A bitmap to keep track of executable blocks in the CFG.  */
static sbitmap executable_blocks;

/* Array of control flow edges on the worklist.  */
static GTY(()) varray_type cfg_blocks = NULL;

static unsigned int cfg_blocks_num = 0;
static int cfg_blocks_tail;
static int cfg_blocks_head;

static sbitmap bb_in_list;

/* This is used to track the current value of each variable.  */
static value *value_vector;

/* Worklist of SSA edges which will need reexamination as their definition
   has changed.  SSA edges are def-use edges in the SSA web.  For each
   edge, we store the definition statement or PHI node D.  The destination
   nodes that need to be visited are accessed using immediate_uses
   (D).  */
static GTY(()) varray_type ssa_edges;

/* Identical to SSA_EDGES.  For performance reasons, the list of SSA
   edges is split into two.  One contains all SSA edges who need to be
   reexamined because their lattice value changed to varying (this
   worklist), and the other contains all other SSA edges to be
   reexamined (ssa_edges).
   
   Since most values in the program are varying, the ideal situation
   is to move them to that lattice value as quickly as possible.
   Thus, it doesn't make sense to process any other type of lattice
   value until all varying values are propagated fully, which is one
   thing using the varying worklist achieves.  In addition, if you
   don't use a separate worklist for varying edges, you end up with
   situations where lattice values move from
   undefined->constant->varying instead of undefined->varying.
*/
static GTY(()) varray_type varying_ssa_edges;


static void initialize (void);
static void finalize (void);
static void visit_phi_node (tree);
static tree ccp_fold (tree);
static value cp_lattice_meet (value, value);
static void visit_stmt (tree);
static void visit_cond_stmt (tree);
static void visit_assignment (tree);
static void add_var_to_ssa_edges_worklist (tree, value);
static void add_outgoing_control_edges (basic_block);
static void add_control_edge (edge);
static void def_to_varying (tree);
static void set_lattice_value (tree, value);
static void simulate_block (basic_block);
static void simulate_stmt (tree);
static void substitute_and_fold (void);
static value evaluate_stmt (tree);
static void dump_lattice_value (FILE *, const char *, value);
static bool replace_uses_in (tree, bool *);
static latticevalue likely_value (tree);
static tree get_rhs (tree);
static void set_rhs (tree *, tree);
static value *get_value (tree);
static value get_default_value (tree);
static tree ccp_fold_builtin (tree, tree);
static bool get_strlen (tree, tree *, bitmap);
static inline bool cfg_blocks_empty_p (void);
static void cfg_blocks_add (basic_block);
static basic_block cfg_blocks_get (void);
static bool need_imm_uses_for (tree var);

/* Process an SSA edge worklist.  WORKLIST is the SSA edge worklist to
   drain. This pops statements off the given WORKLIST and processes
   them until there are no more statements on WORKLIST.  */

static void
process_ssa_edge_worklist (varray_type *worklist)
{
  /* Drain the entire worklist.  */
  while (VARRAY_ACTIVE_SIZE (*worklist) > 0)
    {
      /* Pull the statement to simulate off the worklist.  */
      tree stmt = VARRAY_TOP_TREE (*worklist);
      stmt_ann_t ann = stmt_ann (stmt);
      VARRAY_POP (*worklist);
      
      /* visit_stmt can "cancel" reevaluation of some statements.
	 If it does, then in_ccp_worklist will be zero.  */
      if (ann->in_ccp_worklist)
	{
	  ann->in_ccp_worklist = 0;
	  simulate_stmt (stmt);
	}
    } 
}
 
/* Main entry point for SSA Conditional Constant Propagation.  FNDECL is
   the declaration for the function to optimize.
   
   On exit, VARS_TO_RENAME will contain the symbols that have been exposed by
   the propagation of ADDR_EXPR expressions into pointer dereferences and need
   to be renamed into SSA.

   PHASE indicates which dump file from the DUMP_FILES array to use when
   dumping debugging information.  */

static void
tree_ssa_ccp (void)
{
  initialize ();

  /* Iterate until the worklists are empty.  */
  while (!cfg_blocks_empty_p () 
	 || VARRAY_ACTIVE_SIZE (ssa_edges) > 0
	 || VARRAY_ACTIVE_SIZE (varying_ssa_edges) > 0)
    {
      if (!cfg_blocks_empty_p ())
	{
	  /* Pull the next block to simulate off the worklist.  */
	  basic_block dest_block = cfg_blocks_get ();
	  simulate_block (dest_block);
	}

      /* In order to move things to varying as quickly as
         possible,process the VARYING_SSA_EDGES worklist first.  */
      process_ssa_edge_worklist (&varying_ssa_edges);

      /* Now process the SSA_EDGES worklist.  */
      process_ssa_edge_worklist (&ssa_edges);
    }

  /* Now perform substitutions based on the known constant values.  */
  substitute_and_fold ();

  /* Now cleanup any unreachable code.  */
  cleanup_tree_cfg ();

  /* Free allocated memory.  */
  finalize ();

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_referenced_vars (dump_file);
      fprintf (dump_file, "\n\n");
    }
}

static bool
gate_ccp (void)
{
  return flag_tree_ccp != 0;
}

struct tree_opt_pass pass_ccp = 
{
  "ccp",				/* name */
  gate_ccp,				/* gate */
  tree_ssa_ccp,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_CCP,				/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa
    | TODO_verify_stmts			/* todo_flags_finish */
};


/* Get the constant value associated with variable VAR.  */

static value *
get_value (tree var)
{
  value *val;

#if defined ENABLE_CHECKING
  if (TREE_CODE (var) != SSA_NAME)
    abort ();
#endif

  val = &value_vector[SSA_NAME_VERSION (var)];
  if (val->lattice_val == UNINITIALIZED)
    *val = get_default_value (var);

  return val;
}


/* Simulate the execution of BLOCK.  Evaluate the statement associated
   with each variable reference inside the block.  */

static void
simulate_block (basic_block block)
{
  tree phi;

  /* There is nothing to do for the exit block.  */
  if (block == EXIT_BLOCK_PTR)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nSimulating block %d\n", block->index);

  /* Always simulate PHI nodes, even if we have simulated this block
     before.  */
  for (phi = phi_nodes (block); phi; phi = PHI_CHAIN (phi))
    visit_phi_node (phi);

  /* If this is the first time we've simulated this block, then we
     must simulate each of its statements.  */
  if (!TEST_BIT (executable_blocks, block->index))
    {
      block_stmt_iterator j;
      unsigned int normal_edge_count;
      edge e, normal_edge;

      /* Note that we have simulated this block.  */
      SET_BIT (executable_blocks, block->index);

      for (j = bsi_start (block); !bsi_end_p (j); bsi_next (&j))
	visit_stmt (bsi_stmt (j));

      /* We can not predict when abnormal edges will be executed, so
	 once a block is considered executable, we consider any
	 outgoing abnormal edges as executable.

	 At the same time, if this block has only one successor that is
	 reached by non-abnormal edges, then add that successor to the
	 worklist.  */
      normal_edge_count = 0;
      normal_edge = NULL;
      for (e = block->succ; e; e = e->succ_next)
        {
	  if (e->flags & EDGE_ABNORMAL)
	    {
	      add_control_edge (e);
	    }
	  else
	    {
	      normal_edge_count++;
	      normal_edge = e;
	    }
        }

        if (normal_edge_count == 1)
	  add_control_edge (normal_edge);
    }
}


/* Follow the def-use edges for statement DEF_STMT and simulate all the
   statements reached by it.  */

static void
simulate_stmt (tree use_stmt)
{
  basic_block use_bb = bb_for_stmt (use_stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nSimulating statement (from ssa_edges): ");
      print_generic_stmt (dump_file, use_stmt, dump_flags);
    }

  if (TREE_CODE (use_stmt) == PHI_NODE)
    {
      /* PHI nodes are always visited, regardless of whether or not the
         destination block is executable.  */
      visit_phi_node (use_stmt);
    }
  else if (TEST_BIT (executable_blocks, use_bb->index))
    {
      /* Otherwise, visit the statement containing the use reached by
         DEF, only if the destination block is marked executable.  */
      visit_stmt (use_stmt);
    }
}


/* Perform final substitution and folding.  After this pass the program
   should still be in SSA form.  */

static void
substitute_and_fold (void)
{
  basic_block bb;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file,
	     "\nSubstituing constants and folding statements\n\n");

  /* Substitute constants in every statement of every basic block.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;
      tree phi;

      /* Propagate our known constants into PHI nodes.  */
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  int i;

	  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
	    {
	      value *new_val;
	      use_operand_p orig_p = PHI_ARG_DEF_PTR (phi, i);
	      tree orig = USE_FROM_PTR (orig_p);

	      if (! SSA_VAR_P (orig))
		break;

	      new_val = get_value (orig);
	      if (new_val->lattice_val == CONSTANT
		  && may_propagate_copy (orig, new_val->const_val))
		SET_USE (orig_p, new_val->const_val);
	    }
	}

      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
	{
          bool replaced_address;
	  tree stmt = bsi_stmt (i);

	  /* Skip statements that have been folded already.  */
	  if (stmt_modified_p (stmt) || !is_exec_stmt (stmt))
	    continue;

	  /* Replace the statement with its folded version and mark it
	     folded.  */
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Line %d: replaced ", get_lineno (stmt));
	      print_generic_stmt (dump_file, stmt, TDF_SLIM);
	    }

	  if (replace_uses_in (stmt, &replaced_address))
	    {
	      bool changed = fold_stmt (bsi_stmt_ptr (i));
	      stmt = bsi_stmt(i);
	      modify_stmt (stmt);
	      /* If we folded a builtin function, we'll likely
		 need to rename VDEFs.  */
	      if (replaced_address || changed)
		{
		  mark_new_vars_to_rename (stmt, vars_to_rename);
		  if (maybe_clean_eh_stmt (stmt))
		    tree_purge_dead_eh_edges (bb);
		}
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, " with ");
	      print_generic_stmt (dump_file, stmt, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	}
    }
}


/* Loop through the PHI_NODE's parameters for BLOCK and compare their
   lattice values to determine PHI_NODE's lattice value.  The value of a
   PHI node is determined calling cp_lattice_meet() with all the arguments
   of the PHI node that are incoming via executable edges.  */

static void
visit_phi_node (tree phi)
{
  bool short_circuit = 0;
  value phi_val, *curr_val;
  int i;

  /* If the PHI node has already been deemed to be VARYING, don't simulate
     it again.  */
  if (DONT_SIMULATE_AGAIN (phi))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_generic_expr (dump_file, phi, dump_flags);
    }

  curr_val = get_value (PHI_RESULT (phi));
  switch (curr_val->lattice_val)
    {
    case VARYING:
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\n   Shortcircuit. Default of VARYING.");
      short_circuit = 1;
      break;

    case CONSTANT:
      phi_val = *curr_val;
      break;

    case UNDEFINED:
    case UNINITIALIZED:
      phi_val.lattice_val = UNDEFINED;
      phi_val.const_val = NULL_TREE;
      break;

    default:
      abort ();
    }

  /* If the variable is volatile or the variable is never referenced in a
     real operand, then consider the PHI node VARYING.  */
  if (short_circuit || TREE_THIS_VOLATILE (SSA_NAME_VAR (PHI_RESULT (phi))))
    {
      phi_val.lattice_val = VARYING;
      phi_val.const_val = NULL;
    }
  else
    for (i = 0; i < PHI_NUM_ARGS (phi); i++)
      {
	/* Compute the meet operator over all the PHI arguments.  */
	edge e = PHI_ARG_EDGE (phi, i);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    fprintf (dump_file,
		     "\n    Argument #%d (%d -> %d %sexecutable)\n",
		     i, e->src->index, e->dest->index,
		     (e->flags & EDGE_EXECUTABLE) ? "" : "not ");
	  }

	/* If the incoming edge is executable, Compute the meet operator for
	   the existing value of the PHI node and the current PHI argument.  */
	if (e->flags & EDGE_EXECUTABLE)
	  {
	    tree rdef = PHI_ARG_DEF (phi, i);
	    value *rdef_val, val;

	    if (is_gimple_min_invariant (rdef))
	      {
		val.lattice_val = CONSTANT;
		val.const_val = rdef;
		rdef_val = &val;
	      }
	    else
	      rdef_val = get_value (rdef);

	    phi_val = cp_lattice_meet (phi_val, *rdef_val);

	    if (dump_file && (dump_flags & TDF_DETAILS))
	      {
		fprintf (dump_file, "\t");
		print_generic_expr (dump_file, rdef, dump_flags);
		dump_lattice_value (dump_file, "\tValue: ", *rdef_val);
		fprintf (dump_file, "\n");
	      }

	    if (phi_val.lattice_val == VARYING)
	      break;
	  }
      }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_lattice_value (dump_file, "\n    PHI node value: ", phi_val);
      fprintf (dump_file, "\n\n");
    }

  set_lattice_value (PHI_RESULT (phi), phi_val);
  if (phi_val.lattice_val == VARYING)
    DONT_SIMULATE_AGAIN (phi) = 1;
}


/* Compute the meet operator between VAL1 and VAL2:

   		any M UNDEFINED = any
		any M VARYING	= VARYING
		Ci  M Cj	= Ci		if (i == j)
		Ci  M Cj	= VARYING	if (i != j)  */
static value
cp_lattice_meet (value val1, value val2)
{
  value result;

  /* any M UNDEFINED = any.  */
  if (val1.lattice_val == UNDEFINED)
    return val2;
  else if (val2.lattice_val == UNDEFINED)
    return val1;

  /* any M VARYING = VARYING.  */
  if (val1.lattice_val == VARYING || val2.lattice_val == VARYING)
    {
      result.lattice_val = VARYING;
      result.const_val = NULL_TREE;
      return result;
    }

  /* Ci M Cj = Ci	if (i == j)
     Ci M Cj = VARYING	if (i != j)  */
  if (simple_cst_equal (val1.const_val, val2.const_val) == 1)
    {
      result.lattice_val = CONSTANT;
      result.const_val = val1.const_val;
    }
  else
    {
      result.lattice_val = VARYING;
      result.const_val = NULL_TREE;
    }

  return result;
}


/* Evaluate statement STMT.  If the statement produces an output value and
   its evaluation changes the lattice value of its output, do the following:

   - If the statement is an assignment, add all the SSA edges starting at
     this definition.

   - If the statement is a conditional branch:
   	. If the statement evaluates to non-constant, add all edges to
	  worklist.
	. If the statement is constant, add the edge executed as the
	  result of the branch.  */

static void
visit_stmt (tree stmt)
{
  size_t i;
  stmt_ann_t ann;
  def_optype defs;
  v_may_def_optype v_may_defs;
  v_must_def_optype v_must_defs;

  /* If the statement has already been deemed to be VARYING, don't simulate
     it again.  */
  if (DONT_SIMULATE_AGAIN (stmt))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting statement: ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  ann = stmt_ann (stmt);

  /* If this statement is already in the worklist then "cancel" it.  The
     reevaluation implied by the worklist entry will produce the same
     value we generate here and thus reevaluating it again from the
     worklist is pointless.  */
  if (ann->in_ccp_worklist)
    ann->in_ccp_worklist = 0;

  /* Now examine the statement.  If the statement is an assignment that
     produces a single output value, evaluate its RHS to see if the lattice
     value of its output has changed.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME)
    visit_assignment (stmt);

  /* Definitions made by statements other than assignments to SSA_NAMEs
     represent unknown modifications to their outputs.  Mark them VARYING.  */
  else if (NUM_DEFS (defs = DEF_OPS (ann)) != 0)
    {
      DONT_SIMULATE_AGAIN (stmt) = 1;
      for (i = 0; i < NUM_DEFS (defs); i++)
	{
	  tree def = DEF_OP (defs, i);
	  def_to_varying (def);
	}
    }

  /* If STMT is a conditional branch, see if we can determine which branch
     will be taken.  */
  else if (TREE_CODE (stmt) == COND_EXPR || TREE_CODE (stmt) == SWITCH_EXPR)
    visit_cond_stmt (stmt);

  /* Any other kind of statement is not interesting for constant
     propagation and, therefore, not worth simulating.  */
  else
    {
      DONT_SIMULATE_AGAIN (stmt) = 1;

      /* If STMT is a computed goto, then mark all the output edges
	 executable.  */
      if (computed_goto_p (stmt))
	add_outgoing_control_edges (bb_for_stmt (stmt));
    }

  /* Mark all V_MAY_DEF operands VARYING.  */
  v_may_defs = V_MAY_DEF_OPS (ann);
  for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
    def_to_varying (V_MAY_DEF_RESULT (v_may_defs, i));
    
  /* Mark all V_MUST_DEF operands VARYING.  */
  v_must_defs = V_MUST_DEF_OPS (ann);
  for (i = 0; i < NUM_V_MUST_DEFS (v_must_defs); i++)
    def_to_varying (V_MUST_DEF_OP (v_must_defs, i));
}


/* Visit the assignment statement STMT.  Set the value of its LHS to the
   value computed by the RHS.  */

static void
visit_assignment (tree stmt)
{
  value val;
  tree lhs, rhs;

  lhs = TREE_OPERAND (stmt, 0);
  rhs = TREE_OPERAND (stmt, 1);

  if (TREE_THIS_VOLATILE (SSA_NAME_VAR (lhs)))
    {
      /* Volatile variables are always VARYING.  */
      val.lattice_val = VARYING;
      val.const_val = NULL_TREE;
    }
  else if (TREE_CODE (rhs) == SSA_NAME)
    {
      /* For a simple copy operation, we copy the lattice values.  */
      value *nval = get_value (rhs);
      val = *nval;
    }
  else
    {
      /* Evaluate the statement.  */
      val = evaluate_stmt (stmt);
    }

  /* FIXME: Hack.  If this was a definition of a bitfield, we need to widen
     the constant value into the type of the destination variable.  This
     should not be necessary if GCC represented bitfields properly.  */
  {
    tree lhs = TREE_OPERAND (stmt, 0);
    if (val.lattice_val == CONSTANT
	&& TREE_CODE (lhs) == COMPONENT_REF
	&& DECL_BIT_FIELD (TREE_OPERAND (lhs, 1)))
      {
	tree w = widen_bitfield (val.const_val, TREE_OPERAND (lhs, 1), lhs);

	if (w && is_gimple_min_invariant (w))
	  val.const_val = w;
	else
	  {
	    val.lattice_val = VARYING;
	    val.const_val = NULL;
	  }
      }
  }

  /* Set the lattice value of the statement's output.  */
  set_lattice_value (lhs, val);
  if (val.lattice_val == VARYING)
    DONT_SIMULATE_AGAIN (stmt) = 1;
}


/* Visit the conditional statement STMT.  If it evaluates to a constant value,
   mark outgoing edges appropriately.  */

static void
visit_cond_stmt (tree stmt)
{
  edge e;
  value val;
  basic_block block;

  block = bb_for_stmt (stmt);
  val = evaluate_stmt (stmt);

  /* Find which edge out of the conditional block will be taken and add it
     to the worklist.  If no single edge can be determined statically, add
     all outgoing edges from BLOCK.  */
  e = find_taken_edge (block, val.const_val);
  if (e)
    add_control_edge (e);
  else
    {
      DONT_SIMULATE_AGAIN (stmt) = 1;
      add_outgoing_control_edges (block);
    }
}


/* Add all the edges coming out of BB to the control flow worklist.  */

static void
add_outgoing_control_edges (basic_block bb)
{
  edge e;

  for (e = bb->succ; e; e = e->succ_next)
    add_control_edge (e);
}


/* Add edge E to the control flow worklist.  */

static void
add_control_edge (edge e)
{
  basic_block bb = e->dest;
  if (bb == EXIT_BLOCK_PTR)
    return;

  /* If the edge had already been executed, skip it.  */
  if (e->flags & EDGE_EXECUTABLE)
      return;

  e->flags |= EDGE_EXECUTABLE;

  /* If the block is already in the list, we're done.  */
  if (TEST_BIT (bb_in_list, bb->index))
    return;

  cfg_blocks_add (bb);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Adding Destination of edge (%d -> %d) to worklist\n\n",
	     e->src->index, e->dest->index);
}


/* CCP specific front-end to the non-destructive constant folding routines.

   Attempt to simplify the RHS of STMT knowing that one or more
   operands are constants.

   If simplification is possible, return the simplified RHS,
   otherwise return the original RHS.  */

static tree
ccp_fold (tree stmt)
{
  tree rhs = get_rhs (stmt);
  enum tree_code code = TREE_CODE (rhs);
  int kind = TREE_CODE_CLASS (code);
  tree retval = NULL_TREE;

  /* If the RHS is just a variable, then that variable must now have
     a constant value that we can return directly.  */
  if (TREE_CODE (rhs) == SSA_NAME)
    return get_value (rhs)->const_val;

  /* Unary operators.  Note that we know the single operand must
     be a constant.  So this should almost always return a
     simplified RHS.  */
  if (kind == '1')
    {
      /* Handle unary operators which can appear in GIMPLE form.  */
      tree op0 = TREE_OPERAND (rhs, 0);

      /* Simplify the operand down to a constant.  */
      if (TREE_CODE (op0) == SSA_NAME)
	{
	  value *val = get_value (op0);
	  if (val->lattice_val == CONSTANT)
	    op0 = get_value (op0)->const_val;
	}

      retval = nondestructive_fold_unary_to_constant (code,
		     				      TREE_TYPE (rhs),
						      op0);

      /* If we folded, but did not create an invariant, then we can not
	 use this expression.  */
      if (retval && ! is_gimple_min_invariant (retval))
	return NULL;

      /* If we could not fold the expression, but the arguments are all
         constants and gimple values, then build and return the new
	 expression. 

	 In some cases the new expression is still something we can
	 use as a replacement for an argument.  This happens with
	 NOP conversions of types for example.

	 In other cases the new expression can not be used as a
	 replacement for an argument (as it would create non-gimple
	 code).  But the new expression can still be used to derive
	 other constants.  */
      if (! retval && is_gimple_min_invariant (op0))
	return build1 (code, TREE_TYPE (rhs), op0);
    }

  /* Binary and comparison operators.  We know one or both of the
     operands are constants.  */
  else if (kind == '2'
           || kind == '<'
           || code == TRUTH_AND_EXPR
           || code == TRUTH_OR_EXPR
           || code == TRUTH_XOR_EXPR)
    {
      /* Handle binary and comparison operators that can appear in
         GIMPLE form.  */
      tree op0 = TREE_OPERAND (rhs, 0);
      tree op1 = TREE_OPERAND (rhs, 1);

      /* Simplify the operands down to constants when appropriate.  */
      if (TREE_CODE (op0) == SSA_NAME)
	{
	  value *val = get_value (op0);
	  if (val->lattice_val == CONSTANT)
	    op0 = val->const_val;
	}

      if (TREE_CODE (op1) == SSA_NAME)
	{
	  value *val = get_value (op1);
	  if (val->lattice_val == CONSTANT)
	    op1 = val->const_val;
	}

      retval = nondestructive_fold_binary_to_constant (code,
		     				       TREE_TYPE (rhs),
						       op0, op1);

      /* If we folded, but did not create an invariant, then we can not
	 use this expression.  */
      if (retval && ! is_gimple_min_invariant (retval))
	return NULL;
      
      /* If we could not fold the expression, but the arguments are all
         constants and gimple values, then build and return the new
	 expression. 

	 In some cases the new expression is still something we can
	 use as a replacement for an argument.  This happens with
	 NOP conversions of types for example.

	 In other cases the new expression can not be used as a
	 replacement for an argument (as it would create non-gimple
	 code).  But the new expression can still be used to derive
	 other constants.  */
      if (! retval
	  && is_gimple_min_invariant (op0)
	  && is_gimple_min_invariant (op1))
	return build (code, TREE_TYPE (rhs), op0, op1);
    }

  /* We may be able to fold away calls to builtin functions if their
     arguments are constants.  */
  else if (code == CALL_EXPR
	   && TREE_CODE (TREE_OPERAND (rhs, 0)) == ADDR_EXPR
	   && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (rhs, 0), 0))
	       == FUNCTION_DECL)
	   && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (rhs, 0), 0)))
    {
      use_optype uses = STMT_USE_OPS (stmt);
      if (NUM_USES (uses) != 0)
	{
	  tree *orig;
	  size_t i;

	  /* Preserve the original values of every operand.  */
	  orig = xmalloc (sizeof (tree) * NUM_USES (uses));
	  for (i = 0; i < NUM_USES (uses); i++)
	    orig[i] = USE_OP (uses, i);

	  /* Substitute operands with their values and try to fold.  */
	  replace_uses_in (stmt, NULL);
	  retval = fold_builtin (rhs);

	  /* Restore operands to their original form.  */
	  for (i = 0; i < NUM_USES (uses); i++)
	    SET_USE_OP (uses, i, orig[i]);
	  free (orig);
	}
    }
  else
    return rhs;

  /* If we got a simplified form, see if we need to convert its type.  */
  if (retval)
    {
      if (TREE_TYPE (retval) != TREE_TYPE (rhs))
	retval = fold_convert (TREE_TYPE (rhs), retval);

      if (TREE_TYPE (retval) == TREE_TYPE (rhs))
	return retval;
    }

  /* No simplification was possible.  */
  return rhs;
}


/* Evaluate statement STMT.  */

static value
evaluate_stmt (tree stmt)
{
  value val;
  tree simplified;
  latticevalue likelyvalue = likely_value (stmt);

  /* If the statement is likely to have a CONSTANT result, then try
     to fold the statement to determine the constant value.  */
  if (likelyvalue == CONSTANT)
    simplified = ccp_fold (stmt);
  /* If the statement is likely to have a VARYING result, then do not
     bother folding the statement.  */
  else if (likelyvalue == VARYING)
    simplified = get_rhs (stmt);
  /* Otherwise the statement is likely to have an UNDEFINED value and
     there will be nothing to do.  */
  else
    simplified = NULL_TREE;

  if (simplified && is_gimple_min_invariant (simplified))
    {
      /* The statement produced a constant value.  */
      val.lattice_val = CONSTANT;
      val.const_val = simplified;
    }
  else
    {
      /* The statement produced a nonconstant value.  If the statement
         had undefined operands, then the result of the statement should
	 be undefined.  Else the result of the statement is VARYING.  */
      val.lattice_val = (likelyvalue == UNDEFINED ? UNDEFINED : VARYING);
      val.const_val = NULL_TREE;
    }

  return val;
}


/* Debugging dumps.  */

static void
dump_lattice_value (FILE *outf, const char *prefix, value val)
{
  switch (val.lattice_val)
    {
    case UNDEFINED:
      fprintf (outf, "%sUNDEFINED", prefix);
      break;
    case VARYING:
      fprintf (outf, "%sVARYING", prefix);
      break;
    case CONSTANT:
      fprintf (outf, "%sCONSTANT ", prefix);
      print_generic_expr (outf, val.const_val, dump_flags);
      break;
    default:
      abort ();
    }
}

/* Given a constant value VAL for bitfield FIELD, and a destination
   variable VAR, return VAL appropriately widened to fit into VAR.  If
   FIELD is wider than HOST_WIDE_INT, NULL is returned.  */

tree
widen_bitfield (tree val, tree field, tree var)
{
  unsigned HOST_WIDE_INT var_size, field_size;
  tree wide_val;
  unsigned HOST_WIDE_INT mask;
  unsigned int i;

  /* We can only do this if the size of the type and field and VAL are
     all constants representable in HOST_WIDE_INT.  */
  if (!host_integerp (TYPE_SIZE (TREE_TYPE (var)), 1)
      || !host_integerp (DECL_SIZE (field), 1)
      || !host_integerp (val, 0))
    return NULL_TREE;

  var_size = tree_low_cst (TYPE_SIZE (TREE_TYPE (var)), 1);
  field_size = tree_low_cst (DECL_SIZE (field), 1);

  /* Give up if either the bitfield or the variable are too wide.  */
  if (field_size > HOST_BITS_PER_WIDE_INT || var_size > HOST_BITS_PER_WIDE_INT)
    return NULL_TREE;

#if defined ENABLE_CHECKING
  if (var_size < field_size)
    abort ();
#endif

  /* If the sign bit of the value is not set or the field's type is unsigned,
     just mask off the high order bits of the value.  */
  if (DECL_UNSIGNED (field)
      || !(tree_low_cst (val, 0) & (((HOST_WIDE_INT)1) << (field_size - 1))))
    {
      /* Zero extension.  Build a mask with the lower 'field_size' bits
	 set and a BIT_AND_EXPR node to clear the high order bits of
	 the value.  */
      for (i = 0, mask = 0; i < field_size; i++)
	mask |= ((HOST_WIDE_INT) 1) << i;

      wide_val = build (BIT_AND_EXPR, TREE_TYPE (var), val, 
			fold_convert (TREE_TYPE (var), build_int_2 (mask, 0)));
    }
  else
    {
      /* Sign extension.  Create a mask with the upper 'field_size'
	 bits set and a BIT_IOR_EXPR to set the high order bits of the
	 value.  */
      for (i = 0, mask = 0; i < (var_size - field_size); i++)
	mask |= ((HOST_WIDE_INT) 1) << (var_size - i - 1);

      wide_val = build (BIT_IOR_EXPR, TREE_TYPE (var), val,
			fold_convert (TREE_TYPE (var), build_int_2 (mask, 0)));
    }

  return fold (wide_val);
}


/* Function indicating whether we ought to include information for 'var'
   when calculating immediate uses.  */

static bool
need_imm_uses_for (tree var)
{
  return get_value (var)->lattice_val != VARYING;
}


/* Initialize local data structures and worklists for CCP.  */

static void
initialize (void)
{
  edge e;
  basic_block bb;
  sbitmap virtual_var;

  /* Worklists of SSA edges.  */
  VARRAY_TREE_INIT (ssa_edges, 20, "ssa_edges");
  VARRAY_TREE_INIT (varying_ssa_edges, 20, "varying_ssa_edges");

  executable_blocks = sbitmap_alloc (last_basic_block);
  sbitmap_zero (executable_blocks);

  bb_in_list = sbitmap_alloc (last_basic_block);
  sbitmap_zero (bb_in_list);

  value_vector = (value *) xmalloc (num_ssa_names * sizeof (value));
  memset (value_vector, 0, num_ssa_names * sizeof (value));

  /* 1 if ssa variable is used in a virtual variable context.  */
  virtual_var = sbitmap_alloc (num_ssa_names);
  sbitmap_zero (virtual_var);

  /* Initialize default values and simulation flags for PHI nodes, statements 
     and edges.  */
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;
      tree stmt;
      stmt_ann_t ann;
      def_optype defs;
      v_may_def_optype v_may_defs;
      v_must_def_optype v_must_defs;
      size_t x;
      int vary;

      /* Get the default value for each definition.  */
      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
        {
	  vary = 0;
	  stmt = bsi_stmt (i);
	  get_stmt_operands (stmt);
	  ann = stmt_ann (stmt);
	  defs = DEF_OPS (ann);
	  for (x = 0; x < NUM_DEFS (defs); x++)
	    {
	      tree def = DEF_OP (defs, x);
	      if (get_value (def)->lattice_val == VARYING)
		vary = 1;
	    }
	  DONT_SIMULATE_AGAIN (stmt) = vary;

	  /* Mark all V_MAY_DEF operands VARYING.  */
	  v_may_defs = V_MAY_DEF_OPS (ann);
	  for (x = 0; x < NUM_V_MAY_DEFS (v_may_defs); x++)
	    {
	      tree res = V_MAY_DEF_RESULT (v_may_defs, x);
	      get_value (res)->lattice_val = VARYING;
	      SET_BIT (virtual_var, SSA_NAME_VERSION (res));
	    }
	    
	  /* Mark all V_MUST_DEF operands VARYING.  */
	  v_must_defs = V_MUST_DEF_OPS (ann);
	  for (x = 0; x < NUM_V_MUST_DEFS (v_must_defs); x++)
	    {
	      tree v_must_def = V_MUST_DEF_OP (v_must_defs, x);
	      get_value (v_must_def)->lattice_val = VARYING;
	      SET_BIT (virtual_var, SSA_NAME_VERSION (v_must_def));
	    }
	}

      for (e = bb->succ; e; e = e->succ_next)
	e->flags &= ~EDGE_EXECUTABLE;
    }

  /* Now process PHI nodes.  */
  FOR_EACH_BB (bb)
    {
      tree phi, var;
      int x;
      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
        {
	  value *val;
	  val = get_value (PHI_RESULT (phi));
	  if (val->lattice_val != VARYING)
	    {
	      for (x = 0; x < PHI_NUM_ARGS (phi); x++)
	        {
		  var = PHI_ARG_DEF (phi, x);
		  /* If one argument is virtual, the result is virtual, and
		     therefore varying.  */
		  if (TREE_CODE (var) == SSA_NAME)
		    {
		      if (TEST_BIT (virtual_var, SSA_NAME_VERSION (var)))
		        {
			  val->lattice_val = VARYING;
			  SET_BIT (virtual_var, 
				   SSA_NAME_VERSION (PHI_RESULT (phi)));
			  break;
			}
		    }
	}
	    }
	  DONT_SIMULATE_AGAIN (phi) = ((val->lattice_val == VARYING) ? 1 : 0);
	}
    }

  sbitmap_free (virtual_var);
  /* Compute immediate uses for variables we care about.  */
  compute_immediate_uses (TDFA_USE_OPS, need_imm_uses_for);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_immediate_uses (dump_file);

  VARRAY_BB_INIT (cfg_blocks, 20, "cfg_blocks");

  /* Seed the algorithm by adding the successors of the entry block to the
     edge worklist.  */
  for (e = ENTRY_BLOCK_PTR->succ; e; e = e->succ_next)
    {
      if (e->dest != EXIT_BLOCK_PTR)
        {
	  e->flags |= EDGE_EXECUTABLE;
	  cfg_blocks_add (e->dest);
	}
    }
}


/* Free allocated storage.  */

static void
finalize (void)
{
  ssa_edges = NULL;
  varying_ssa_edges = NULL;
  cfg_blocks = NULL;
  free (value_vector);
  sbitmap_free (bb_in_list);
  sbitmap_free (executable_blocks);
  free_df ();
}

/* Is the block worklist empty.  */

static inline bool
cfg_blocks_empty_p (void)
{
  return (cfg_blocks_num == 0);
}

/* Add a basic block to the worklist.  */

static void 
cfg_blocks_add (basic_block bb)
{
   if (bb == ENTRY_BLOCK_PTR || bb == EXIT_BLOCK_PTR)
     return;

   if (TEST_BIT (bb_in_list, bb->index))
     return;

    if (cfg_blocks_empty_p ())
      {
	cfg_blocks_tail = cfg_blocks_head = 0;
	cfg_blocks_num = 1;
      }
    else
      {
	cfg_blocks_num++;
	if (cfg_blocks_num > VARRAY_SIZE (cfg_blocks))
	  {
	    /* We have to grow the array now.  Adjust to queue to occupy the
	       full space of the original array.  */
	    cfg_blocks_tail = VARRAY_SIZE (cfg_blocks);
	    cfg_blocks_head = 0;
	    VARRAY_GROW (cfg_blocks, 2 * VARRAY_SIZE (cfg_blocks));
	  }
	else
	  cfg_blocks_tail = (cfg_blocks_tail + 1) % VARRAY_SIZE (cfg_blocks);
      }
    VARRAY_BB (cfg_blocks, cfg_blocks_tail) = bb;
    SET_BIT (bb_in_list, bb->index);
}

/* Remove a block from the worklist.  */

static basic_block
cfg_blocks_get (void)
{
  basic_block bb;

  bb = VARRAY_BB (cfg_blocks, cfg_blocks_head);

#ifdef ENABLE_CHECKING
  if (cfg_blocks_empty_p () || !bb)
    abort ();
#endif

  cfg_blocks_head = (cfg_blocks_head + 1) % VARRAY_SIZE (cfg_blocks);
  --cfg_blocks_num;
  RESET_BIT (bb_in_list, bb->index);

  return bb;
}

/* We have just defined a new value for VAR.  Add all immediate uses
   of VAR to the ssa_edges or varying_ssa_edges worklist.  */
static void
add_var_to_ssa_edges_worklist (tree var, value val)
{
  tree stmt = SSA_NAME_DEF_STMT (var);
  dataflow_t df = get_immediate_uses (stmt);
  int num_uses = num_immediate_uses (df);
  int i;

  for (i = 0; i < num_uses; i++)
    {
      tree use = immediate_use (df, i);

      if (!DONT_SIMULATE_AGAIN (use))
	{
	  stmt_ann_t ann = stmt_ann (use);
	  if (ann->in_ccp_worklist == 0)
	    {
	      ann->in_ccp_worklist = 1;
	      if (val.lattice_val == VARYING)
		VARRAY_PUSH_TREE (varying_ssa_edges, use);
	      else
		VARRAY_PUSH_TREE (ssa_edges, use);
	    }
	}
    }
}

/* Set the lattice value for the variable VAR to VARYING.  */

static void
def_to_varying (tree var)
{
  value val;
  val.lattice_val = VARYING;
  val.const_val = NULL_TREE;
  set_lattice_value (var, val);
}

/* Set the lattice value for variable VAR to VAL.  */

static void
set_lattice_value (tree var, value val)
{
  value *old = get_value (var);

#ifdef ENABLE_CHECKING
  if (val.lattice_val == UNDEFINED)
    {
      /* CONSTANT->UNDEFINED is never a valid state transition.  */
      if (old->lattice_val == CONSTANT)
	abort ();

      /* VARYING->UNDEFINED is generally not a valid state transition,
	 except for values which are initialized to VARYING.  */
      if (old->lattice_val == VARYING
	  && get_default_value (var).lattice_val != VARYING)
	abort ();
    }
  else if (val.lattice_val == CONSTANT)
    {
      /* VARYING -> CONSTANT is an invalid state transition, except
	 for objects which start off in a VARYING state.  */
      if (old->lattice_val == VARYING
	  && get_default_value (var).lattice_val != VARYING)
	abort ();
    }
#endif

  /* If the constant for VAR has changed, then this VAR is really varying.  */
  if (old->lattice_val == CONSTANT && val.lattice_val == CONSTANT
      && !simple_cst_equal (old->const_val, val.const_val))
    {
      val.lattice_val = VARYING;
      val.const_val = NULL_TREE;
    }

  if (old->lattice_val != val.lattice_val)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  dump_lattice_value (dump_file,
			      "Lattice value changed to ", val);
	  fprintf (dump_file, ".  Adding definition to SSA edges.\n");
	}

      add_var_to_ssa_edges_worklist (var, val);
      *old = val;
    }
}

/* Replace USE references in statement STMT with their immediate reaching
   definition.  Return true if at least one reference was replaced.  If
   REPLACED_ADDRESSES_P is given, it will be set to true if an address
   constant was replaced.  */

static bool
replace_uses_in (tree stmt, bool *replaced_addresses_p)
{
  bool replaced = false;
  use_optype uses;
  size_t i;

  if (replaced_addresses_p)
    *replaced_addresses_p = false;

  get_stmt_operands (stmt);

  uses = STMT_USE_OPS (stmt);
  for (i = 0; i < NUM_USES (uses); i++)
    {
      use_operand_p use = USE_OP_PTR (uses, i);
      value *val = get_value (USE_FROM_PTR (use));

      if (val->lattice_val == CONSTANT)
	{
	  SET_USE (use, val->const_val);
	  replaced = true;
	  if (POINTER_TYPE_P (TREE_TYPE (USE_FROM_PTR (use))) 
	      && replaced_addresses_p)
	    *replaced_addresses_p = true;
	}
    }

  return replaced;
}

/* Return the likely latticevalue for STMT.

   If STMT has no operands, then return CONSTANT.

   Else if any operands of STMT are undefined, then return UNDEFINED.

   Else if any operands of STMT are constants, then return CONSTANT.

   Else return VARYING.  */

static latticevalue
likely_value (tree stmt)
{
  use_optype uses;
  size_t i;
  int found_constant = 0;
  stmt_ann_t ann;

  /* If the statement makes aliased loads or has volatile operands, it
     won't fold to a constant value.  */
  ann = stmt_ann (stmt);
  if (ann->makes_aliased_loads || ann->has_volatile_ops)
    return VARYING;

  /* A CALL_EXPR is assumed to be varying.  This may be overly conservative,
     in the presence of const and pure calls.  */
  if (get_call_expr_in (stmt) != NULL_TREE)
    return VARYING;

  get_stmt_operands (stmt);

  uses = USE_OPS (ann);
  for (i = 0; i < NUM_USES (uses); i++)
    {
      tree use = USE_OP (uses, i);
      value *val = get_value (use);

      if (val->lattice_val == UNDEFINED)
	return UNDEFINED;

      if (val->lattice_val == CONSTANT)
	found_constant = 1;
    }

  return ((found_constant || !uses) ? CONSTANT : VARYING);
}

/* A subroutine of fold_stmt_r.  Attempts to fold *(A+O) to A[X].
   BASE is an array type.  OFFSET is a byte displacement.  ORIG_TYPE
   is the desired result type.  */

static tree
maybe_fold_offset_to_array_ref (tree base, tree offset, tree orig_type)
{
  tree min_idx, idx, elt_offset = integer_zero_node;
  tree array_type, elt_type, elt_size;

  /* If BASE is an ARRAY_REF, we can pick up another offset (this time
     measured in units of the size of elements type) from that ARRAY_REF).
     We can't do anything if either is variable.

     The case we handle here is *(&A[N]+O).  */
  if (TREE_CODE (base) == ARRAY_REF)
    {
      tree low_bound = array_ref_low_bound (base);

      elt_offset = TREE_OPERAND (base, 1);
      if (TREE_CODE (low_bound) != INTEGER_CST
	  || TREE_CODE (elt_offset) != INTEGER_CST)
	return NULL_TREE;

      elt_offset = int_const_binop (MINUS_EXPR, elt_offset, low_bound, 0);
      base = TREE_OPERAND (base, 0);
    }

  /* Ignore stupid user tricks of indexing non-array variables.  */
  array_type = TREE_TYPE (base);
  if (TREE_CODE (array_type) != ARRAY_TYPE)
    return NULL_TREE;
  elt_type = TREE_TYPE (array_type);
  if (!lang_hooks.types_compatible_p (orig_type, elt_type))
    return NULL_TREE;
	
  /* If OFFSET and ELT_OFFSET are zero, we don't care about the size of the
     element type (so we can use the alignment if it's not constant).
     Otherwise, compute the offset as an index by using a division.  If the
     division isn't exact, then don't do anything.  */
  elt_size = TYPE_SIZE_UNIT (elt_type);
  if (integer_zerop (offset))
    {
      if (TREE_CODE (elt_size) != INTEGER_CST)
	elt_size = size_int (TYPE_ALIGN (elt_type));

      idx = integer_zero_node;
    }
  else
    {
      unsigned HOST_WIDE_INT lquo, lrem;
      HOST_WIDE_INT hquo, hrem;

      if (TREE_CODE (elt_size) != INTEGER_CST
	  || div_and_round_double (TRUNC_DIV_EXPR, 1,
				   TREE_INT_CST_LOW (offset),
				   TREE_INT_CST_HIGH (offset),
				   TREE_INT_CST_LOW (elt_size),
				   TREE_INT_CST_HIGH (elt_size),
				   &lquo, &hquo, &lrem, &hrem)
	  || lrem || hrem)
	return NULL_TREE;

      idx = build_int_2_wide (lquo, hquo);
    }

  /* Assume the low bound is zero.  If there is a domain type, get the
     low bound, if any, convert the index into that type, and add the
     low bound.  */
  min_idx = integer_zero_node;
  if (TYPE_DOMAIN (array_type))
    {
      if (TYPE_MIN_VALUE (TYPE_DOMAIN (array_type)))
	min_idx = TYPE_MIN_VALUE (TYPE_DOMAIN (array_type));
      else
	min_idx = fold_convert (TYPE_DOMAIN (array_type), min_idx);

      if (TREE_CODE (min_idx) != INTEGER_CST)
	return NULL_TREE;

      idx = fold_convert (TYPE_DOMAIN (array_type), idx);
      elt_offset = fold_convert (TYPE_DOMAIN (array_type), elt_offset);
    }

  if (!integer_zerop (min_idx))
    idx = int_const_binop (PLUS_EXPR, idx, min_idx, 0);
  if (!integer_zerop (elt_offset))
    idx = int_const_binop (PLUS_EXPR, idx, elt_offset, 0);

  return build (ARRAY_REF, orig_type, base, idx, min_idx,
		size_int (tree_low_cst (elt_size, 1)
			  / (TYPE_ALIGN (elt_type) / BITS_PER_UNIT)));
}

/* A subroutine of fold_stmt_r.  Attempts to fold *(S+O) to S.X.
   BASE is a record type.  OFFSET is a byte displacement.  ORIG_TYPE
   is the desired result type.  */
/* ??? This doesn't handle class inheritance.  */

static tree
maybe_fold_offset_to_component_ref (tree record_type, tree base, tree offset,
				    tree orig_type, bool base_is_ptr)
{
  tree f, t, field_type, tail_array_field;

  if (TREE_CODE (record_type) != RECORD_TYPE
      && TREE_CODE (record_type) != UNION_TYPE
      && TREE_CODE (record_type) != QUAL_UNION_TYPE)
    return NULL_TREE;

  /* Short-circuit silly cases.  */
  if (lang_hooks.types_compatible_p (record_type, orig_type))
    return NULL_TREE;

  tail_array_field = NULL_TREE;
  for (f = TYPE_FIELDS (record_type); f ; f = TREE_CHAIN (f))
    {
      int cmp;

      if (TREE_CODE (f) != FIELD_DECL)
	continue;
      if (DECL_BIT_FIELD (f))
	continue;
      if (TREE_CODE (DECL_FIELD_OFFSET (f)) != INTEGER_CST)
	continue;

      /* ??? Java creates "interesting" fields for representing base classes.
	 They have no name, and have no context.  With no context, we get into
	 trouble with nonoverlapping_component_refs_p.  Skip them.  */
      if (!DECL_FIELD_CONTEXT (f))
	continue;

      /* The previous array field isn't at the end.  */
      tail_array_field = NULL_TREE;

      /* Check to see if this offset overlaps with the field.  */
      cmp = tree_int_cst_compare (DECL_FIELD_OFFSET (f), offset);
      if (cmp > 0)
	continue;

      field_type = TREE_TYPE (f);
      if (cmp < 0)
	{
	  /* Don't care about offsets into the middle of scalars.  */
	  if (!AGGREGATE_TYPE_P (field_type))
	    continue;

	  /* Check for array at the end of the struct.  This is often
	     used as for flexible array members.  We should be able to
	     turn this into an array access anyway.  */
	  if (TREE_CODE (field_type) == ARRAY_TYPE)
	    tail_array_field = f;

	  /* Check the end of the field against the offset.  */
	  if (!DECL_SIZE_UNIT (f)
	      || TREE_CODE (DECL_SIZE_UNIT (f)) != INTEGER_CST)
	    continue;
	  t = int_const_binop (MINUS_EXPR, offset, DECL_FIELD_OFFSET (f), 1);
	  if (!tree_int_cst_lt (t, DECL_SIZE_UNIT (f)))
	    continue;

	  /* If we matched, then set offset to the displacement into
	     this field.  */
	  offset = t;
	}

      /* Here we exactly match the offset being checked.  If the types match,
	 then we can return that field.  */
      else if (lang_hooks.types_compatible_p (orig_type, field_type))
	{
	  if (base_is_ptr)
	    base = build1 (INDIRECT_REF, record_type, base);
	  t = build (COMPONENT_REF, field_type, base, f, NULL_TREE);
	  return t;
	}

      /* Don't care about type-punning of scalars.  */
      else if (!AGGREGATE_TYPE_P (field_type))
	return NULL_TREE;

      goto found;
    }

  if (!tail_array_field)
    return NULL_TREE;

  f = tail_array_field;
  field_type = TREE_TYPE (f);

 found:
  /* If we get here, we've got an aggregate field, and a possibly 
     nonzero offset into them.  Recurse and hope for a valid match.  */
  if (base_is_ptr)
    base = build1 (INDIRECT_REF, record_type, base);
  base = build (COMPONENT_REF, field_type, base, f, NULL_TREE);

  t = maybe_fold_offset_to_array_ref (base, offset, orig_type);
  if (t)
    return t;
  return maybe_fold_offset_to_component_ref (field_type, base, offset,
					     orig_type, false);
}

/* A subroutine of fold_stmt_r.  Attempt to simplify *(BASE+OFFSET).
   Return the simplified expression, or NULL if nothing could be done.  */

static tree
maybe_fold_stmt_indirect (tree expr, tree base, tree offset)
{
  tree t;

  /* We may well have constructed a double-nested PLUS_EXPR via multiple
     substitutions.  Fold that down to one.  Remove NON_LVALUE_EXPRs that
     are sometimes added.  */
  base = fold (base);
  STRIP_NOPS (base);
  TREE_OPERAND (expr, 0) = base;

  /* One possibility is that the address reduces to a string constant.  */
  t = fold_read_from_constant_string (expr);
  if (t)
    return t;

  /* Add in any offset from a PLUS_EXPR.  */
  if (TREE_CODE (base) == PLUS_EXPR)
    {
      tree offset2;

      offset2 = TREE_OPERAND (base, 1);
      if (TREE_CODE (offset2) != INTEGER_CST)
	return NULL_TREE;
      base = TREE_OPERAND (base, 0);

      offset = int_const_binop (PLUS_EXPR, offset, offset2, 1);
    }

  if (TREE_CODE (base) == ADDR_EXPR)
    {
      /* Strip the ADDR_EXPR.  */
      base = TREE_OPERAND (base, 0);

      /* Try folding *(&B+O) to B[X].  */
      t = maybe_fold_offset_to_array_ref (base, offset, TREE_TYPE (expr));
      if (t)
	return t;

      /* Try folding *(&B+O) to B.X.  */
      t = maybe_fold_offset_to_component_ref (TREE_TYPE (base), base, offset,
					      TREE_TYPE (expr), false);
      if (t)
	return t;

      /* Fold *&B to B.  We can only do this if EXPR is the same type
	 as BASE.  We can't do this if EXPR is the element type of an array
	 and BASE is the array.  */
      if (integer_zerop (offset)
	  && lang_hooks.types_compatible_p (TREE_TYPE (base),
					    TREE_TYPE (expr)))
	return base;
    }
  else
    {
      /* We can get here for out-of-range string constant accesses, 
	 such as "_"[3].  Bail out of the entire substitution search
	 and arrange for the entire statement to be replaced by a
	 call to __builtin_trap.  In all likelyhood this will all be
	 constant-folded away, but in the meantime we can't leave with
	 something that get_expr_operands can't understand.  */

      t = base;
      STRIP_NOPS (t);
      if (TREE_CODE (t) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	{
	  /* FIXME: Except that this causes problems elsewhere with dead
	     code not being deleted, and we abort in the rtl expanders 
	     because we failed to remove some ssa_name.  In the meantime,
	     just return zero.  */
	  /* FIXME2: This condition should be signaled by
	     fold_read_from_constant_string directly, rather than 
	     re-checking for it here.  */
	  return integer_zero_node;
	}

      /* Try folding *(B+O) to B->X.  Still an improvement.  */
      if (POINTER_TYPE_P (TREE_TYPE (base)))
	{
          t = maybe_fold_offset_to_component_ref (TREE_TYPE (TREE_TYPE (base)),
						  base, offset,
						  TREE_TYPE (expr), true);
	  if (t)
	    return t;
	}
    }

  /* Otherwise we had an offset that we could not simplify.  */
  return NULL_TREE;
}

/* A subroutine of fold_stmt_r.  EXPR is a PLUS_EXPR.

   A quaint feature extant in our address arithmetic is that there
   can be hidden type changes here.  The type of the result need
   not be the same as the type of the input pointer.

   What we're after here is an expression of the form
	(T *)(&array + const)
   where the cast doesn't actually exist, but is implicit in the
   type of the PLUS_EXPR.  We'd like to turn this into
	&array[x]
   which may be able to propagate further.  */

static tree
maybe_fold_stmt_addition (tree expr)
{
  tree op0 = TREE_OPERAND (expr, 0);
  tree op1 = TREE_OPERAND (expr, 1);
  tree ptr_type = TREE_TYPE (expr);
  tree ptd_type;
  tree t;
  bool subtract = (TREE_CODE (expr) == MINUS_EXPR);

  /* We're only interested in pointer arithmetic.  */
  if (!POINTER_TYPE_P (ptr_type))
    return NULL_TREE;
  /* Canonicalize the integral operand to op1.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (op0)))
    {
      if (subtract)
	return NULL_TREE;
      t = op0, op0 = op1, op1 = t;
    }
  /* It had better be a constant.  */
  if (TREE_CODE (op1) != INTEGER_CST)
    return NULL_TREE;
  /* The first operand should be an ADDR_EXPR.  */
  if (TREE_CODE (op0) != ADDR_EXPR)
    return NULL_TREE;
  op0 = TREE_OPERAND (op0, 0);

  /* If the first operand is an ARRAY_REF, expand it so that we can fold
     the offset into it.  */
  while (TREE_CODE (op0) == ARRAY_REF)
    {
      tree array_obj = TREE_OPERAND (op0, 0);
      tree array_idx = TREE_OPERAND (op0, 1);
      tree elt_type = TREE_TYPE (op0);
      tree elt_size = TYPE_SIZE_UNIT (elt_type);
      tree min_idx;

      if (TREE_CODE (array_idx) != INTEGER_CST)
	break;
      if (TREE_CODE (elt_size) != INTEGER_CST)
	break;

      /* Un-bias the index by the min index of the array type.  */
      min_idx = TYPE_DOMAIN (TREE_TYPE (array_obj));
      if (min_idx)
	{
	  min_idx = TYPE_MIN_VALUE (min_idx);
	  if (min_idx)
	    {
	      if (TREE_CODE (min_idx) != INTEGER_CST)
		break;

	      array_idx = convert (TREE_TYPE (min_idx), array_idx);
	      if (!integer_zerop (min_idx))
		array_idx = int_const_binop (MINUS_EXPR, array_idx,
					     min_idx, 0);
	    }
	}

      /* Convert the index to a byte offset.  */
      array_idx = convert (sizetype, array_idx);
      array_idx = int_const_binop (MULT_EXPR, array_idx, elt_size, 0);

      /* Update the operands for the next round, or for folding.  */
      /* If we're manipulating unsigned types, then folding into negative
	 values can produce incorrect results.  Particularly if the type
	 is smaller than the width of the pointer.  */
      if (subtract
	  && TYPE_UNSIGNED (TREE_TYPE (op1))
	  && tree_int_cst_lt (array_idx, op1))
	return NULL;
      op1 = int_const_binop (subtract ? MINUS_EXPR : PLUS_EXPR,
			     array_idx, op1, 0);
      subtract = false;
      op0 = array_obj;
    }

  /* If we weren't able to fold the subtraction into another array reference,
     canonicalize the integer for passing to the array and component ref
     simplification functions.  */
  if (subtract)
    {
      if (TYPE_UNSIGNED (TREE_TYPE (op1)))
	return NULL;
      op1 = fold (build1 (NEGATE_EXPR, TREE_TYPE (op1), op1));
      /* ??? In theory fold should always produce another integer.  */
      if (TREE_CODE (op1) != INTEGER_CST)
	return NULL;
    }

  ptd_type = TREE_TYPE (ptr_type);

  /* At which point we can try some of the same things as for indirects.  */
  t = maybe_fold_offset_to_array_ref (op0, op1, ptd_type);
  if (!t)
    t = maybe_fold_offset_to_component_ref (TREE_TYPE (op0), op0, op1,
					    ptd_type, false);
  if (t)
    t = build1 (ADDR_EXPR, ptr_type, t);

  return t;
}

/* Subroutine of fold_stmt called via walk_tree.  We perform several
   simplifications of EXPR_P, mostly having to do with pointer arithmetic.  */

static tree
fold_stmt_r (tree *expr_p, int *walk_subtrees, void *data)
{
  bool *changed_p = data;
  tree expr = *expr_p, t;

  /* ??? It'd be nice if walk_tree had a pre-order option.  */
  switch (TREE_CODE (expr))
    {
    case INDIRECT_REF:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
	return t;
      *walk_subtrees = 0;

      t = maybe_fold_stmt_indirect (expr, TREE_OPERAND (expr, 0),
				    integer_zero_node);
      break;

      /* ??? Could handle ARRAY_REF here, as a variant of INDIRECT_REF.
	 We'd only want to bother decomposing an existing ARRAY_REF if
	 the base array is found to have another offset contained within.
	 Otherwise we'd be wasting time.  */

    case ADDR_EXPR:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
	return t;
      *walk_subtrees = 0;

      /* Set TREE_INVARIANT properly so that the value is properly
	 considered constant, and so gets propagated as expected.  */
      if (*changed_p)
        recompute_tree_invarant_for_addr_expr (expr);
      return NULL_TREE;

    case PLUS_EXPR:
    case MINUS_EXPR:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
	return t;
      t = walk_tree (&TREE_OPERAND (expr, 1), fold_stmt_r, data, NULL);
      if (t)
	return t;
      *walk_subtrees = 0;

      t = maybe_fold_stmt_addition (expr);
      break;

    case COMPONENT_REF:
      t = walk_tree (&TREE_OPERAND (expr, 0), fold_stmt_r, data, NULL);
      if (t)
        return t;
      *walk_subtrees = 0;

      /* Make sure the FIELD_DECL is actually a field in the type on
         the lhs.  In cases with IMA it is possible that it came
         from another, equivalent type at this point.  We have
	 already checked the equivalence in this case.
	 Match on type plus offset, to allow for unnamed fields.
	 We won't necessarily get the corresponding field for
	 unions; this is believed to be harmless.  */

      if ((current_file_decl && TREE_CHAIN (current_file_decl))
        && (DECL_FIELD_CONTEXT (TREE_OPERAND (expr, 1)) !=
            TREE_TYPE (TREE_OPERAND (expr, 0))))
        {
          tree f;
          tree orig_field = TREE_OPERAND (expr, 1);
          tree orig_type = TREE_TYPE (orig_field);
          for (f = TYPE_FIELDS (TREE_TYPE (TREE_OPERAND (expr, 0)));
              f; f = TREE_CHAIN (f))
            {
              if (lang_hooks.types_compatible_p (TREE_TYPE (f), orig_type)
                  && tree_int_cst_compare (DECL_FIELD_BIT_OFFSET (f),
                                          DECL_FIELD_BIT_OFFSET (orig_field))
                      == 0
                  && tree_int_cst_compare (DECL_FIELD_OFFSET (f),
                                          DECL_FIELD_OFFSET (orig_field))
                      == 0)
                {
                  TREE_OPERAND (expr, 1) = f;
                  break;
                }
            }
        /* Fall through is an error; it will be detected in tree-sra.  */
        }
      break;

    default:
      return NULL_TREE;
    }

  if (t)
    {
      *expr_p = t;
      *changed_p = true;
    }

  return NULL_TREE;
}

/* Fold the statement pointed by STMT_P.  In some cases, this function may
   replace the whole statement with a new one.  Returns true iff folding
   makes any changes.  */

bool
fold_stmt (tree *stmt_p)
{
  tree rhs, result, stmt;
  bool changed = false;

  stmt = *stmt_p;

  /* If we replaced constants and the statement makes pointer dereferences,
     then we may need to fold instances of *&VAR into VAR, etc.  */
  if (walk_tree (stmt_p, fold_stmt_r, &changed, NULL))
    {
      *stmt_p
	= build_function_call_expr (implicit_built_in_decls[BUILT_IN_TRAP],
				    NULL);
      return true;
    }

  rhs = get_rhs (stmt);
  if (!rhs)
    return changed;
  result = NULL_TREE;

  if (TREE_CODE (rhs) == CALL_EXPR)
    {
      tree callee;

      /* Check for builtins that CCP can handle using information not
	 available in the generic fold routines.  */
      callee = get_callee_fndecl (rhs);
      if (callee && DECL_BUILT_IN (callee))
	result = ccp_fold_builtin (stmt, rhs);
      else
	{
	  /* Check for resolvable OBJ_TYPE_REF.  The only sorts we can resolve
	     here are when we've propagated the address of a decl into the
	     object slot.  */
	  /* ??? Should perhaps do this in fold proper.  However, doing it
	     there requires that we create a new CALL_EXPR, and that requires
	     copying EH region info to the new node.  Easier to just do it
	     here where we can just smash the call operand.  */
	  callee = TREE_OPERAND (rhs, 0);
	  if (TREE_CODE (callee) == OBJ_TYPE_REF
	      && lang_hooks.fold_obj_type_ref
	      && TREE_CODE (OBJ_TYPE_REF_OBJECT (callee)) == ADDR_EXPR
	      && DECL_P (TREE_OPERAND (OBJ_TYPE_REF_OBJECT (callee), 0)))
	    {
	      tree t;

	      t = TREE_TYPE (TREE_OPERAND (OBJ_TYPE_REF_OBJECT (callee), 0));
	      t = lang_hooks.fold_obj_type_ref (callee, t);
	      if (t)
		{
		  TREE_OPERAND (rhs, 0) = t;
		  changed = true;
		}
	    }
	}
    }

  /* If we couldn't fold the RHS, hand over to the generic fold routines.  */
  if (result == NULL_TREE)
    result = fold (rhs);

  /* Strip away useless type conversions.  Both the NON_LVALUE_EXPR that
     may have been added by fold, and "useless" type conversions that might
     now be apparent due to propagation.  */
  STRIP_MAIN_TYPE_NOPS (result);
  STRIP_USELESS_TYPE_CONVERSION (result);

  if (result != rhs)
    {
      changed = true;
      set_rhs (stmt_p, result);
    }

  return changed;
}

/* Get the main expression from statement STMT.  */

static tree
get_rhs (tree stmt)
{
  enum tree_code code = TREE_CODE (stmt);

  if (code == MODIFY_EXPR)
    return TREE_OPERAND (stmt, 1);
  if (code == COND_EXPR)
    return COND_EXPR_COND (stmt);
  else if (code == SWITCH_EXPR)
    return SWITCH_COND (stmt);
  else if (code == RETURN_EXPR)
    {
      if (!TREE_OPERAND (stmt, 0))
	return NULL_TREE;
      if (TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
	return TREE_OPERAND (TREE_OPERAND (stmt, 0), 1);
      else
	return TREE_OPERAND (stmt, 0);
    }
  else if (code == GOTO_EXPR)
    return GOTO_DESTINATION (stmt);
  else if (code == LABEL_EXPR)
    return LABEL_EXPR_LABEL (stmt);
  else
    return stmt;
}


/* Set the main expression of *STMT_P to EXPR.  */

static void
set_rhs (tree *stmt_p, tree expr)
{
  tree stmt = *stmt_p;
  enum tree_code code = TREE_CODE (stmt);

  if (code == MODIFY_EXPR)
    TREE_OPERAND (stmt, 1) = expr;
  else if (code == COND_EXPR)
    COND_EXPR_COND (stmt) = expr;
  else if (code == SWITCH_EXPR)
    SWITCH_COND (stmt) = expr;
  else if (code == RETURN_EXPR)
    {
      if (TREE_OPERAND (stmt, 0)
	  && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
	TREE_OPERAND (TREE_OPERAND (stmt, 0), 1) = expr;
      else
	TREE_OPERAND (stmt, 0) = expr;
    }
  else if (code == GOTO_EXPR)
    GOTO_DESTINATION (stmt) = expr;
  else if (code == LABEL_EXPR)
    LABEL_EXPR_LABEL (stmt) = expr;
  else
    {
      /* Replace the whole statement with EXPR.  If EXPR has no side
	 effects, then replace *STMT_P with an empty statement.  */
      stmt_ann_t ann = stmt_ann (stmt);
      *stmt_p = TREE_SIDE_EFFECTS (expr) ? expr : build_empty_stmt ();
      (*stmt_p)->common.ann = (tree_ann_t) ann;

      if (TREE_SIDE_EFFECTS (expr))
	{
	  def_optype defs;
	  v_may_def_optype v_may_defs;
	  v_must_def_optype v_must_defs;
	  size_t i;

	  /* Fix all the SSA_NAMEs created by *STMT_P to point to its new
	     replacement.  */
	  defs = DEF_OPS (ann);
	  for (i = 0; i < NUM_DEFS (defs); i++)
	    {
	      tree var = DEF_OP (defs, i);
	      if (TREE_CODE (var) == SSA_NAME)
		SSA_NAME_DEF_STMT (var) = *stmt_p;
	    }

	  v_may_defs = V_MAY_DEF_OPS (ann);
	  for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
	    {
	      tree var = V_MAY_DEF_RESULT (v_may_defs, i);
	      if (TREE_CODE (var) == SSA_NAME)
		SSA_NAME_DEF_STMT (var) = *stmt_p;
	    }
	    
	  v_must_defs = V_MUST_DEF_OPS (ann);
	  for (i = 0; i < NUM_V_MUST_DEFS (v_must_defs); i++)
	    {
	      tree var = V_MUST_DEF_OP (v_must_defs, i);
	      if (TREE_CODE (var) == SSA_NAME)
		SSA_NAME_DEF_STMT (var) = *stmt_p;
	    }
	}
    }
}


/* Return a default value for variable VAR using the following rules:

   1- Global and static variables are considered VARYING, unless they are
      declared const.

   2- Function arguments are considered VARYING.

   3- Any other value is considered UNDEFINED.  This is useful when
      considering PHI nodes.  PHI arguments that are undefined do not
      change the constant value of the PHI node, which allows for more
      constants to be propagated.  */

static value
get_default_value (tree var)
{
  value val;
  tree sym;

  if (TREE_CODE (var) == SSA_NAME)
    sym = SSA_NAME_VAR (var);
  else
    {
#ifdef ENABLE_CHECKING
      if (!DECL_P (var))
	abort ();
#endif
      sym = var;
    }

  val.lattice_val = UNDEFINED;
  val.const_val = NULL_TREE;

  if (TREE_CODE (sym) == PARM_DECL || TREE_THIS_VOLATILE (sym))
    {
      /* Function arguments and volatile variables are considered VARYING.  */
      val.lattice_val = VARYING;
    }
  else if (decl_function_context (sym) != current_function_decl
           || TREE_STATIC (sym))
    {
      /* Globals and static variables are considered VARYING, unless they
	 are declared 'const'.  */
      val.lattice_val = VARYING;

      if (TREE_READONLY (sym)
	  && DECL_INITIAL (sym)
	  && is_gimple_min_invariant (DECL_INITIAL (sym)))
	{
	  val.lattice_val = CONSTANT;
	  val.const_val = DECL_INITIAL (sym);
	}
    }
  else
    {
      enum tree_code code;
      tree stmt = SSA_NAME_DEF_STMT (var);

      if (!IS_EMPTY_STMT (stmt))
        {
	  code = TREE_CODE (stmt);
	  if (code != MODIFY_EXPR && code != PHI_NODE)
	    val.lattice_val = VARYING;
	}
    }

  return val;
}


/* Fold builtin call FN in statement STMT.  If it cannot be folded into a
   constant, return NULL_TREE.  Otherwise, return its constant value.  */

static tree
ccp_fold_builtin (tree stmt, tree fn)
{
  tree result, strlen_val[2];
  tree arglist = TREE_OPERAND (fn, 1), a;
  tree callee = get_callee_fndecl (fn);
  bitmap visited;
  int strlen_arg, i;

  /* Ignore MD builtins.  */
  if (DECL_BUILT_IN_CLASS (callee) == BUILT_IN_MD)
    return NULL_TREE;

  /* First try the generic builtin folder.  If that succeeds, return the
     result directly.  */
  result = fold_builtin (fn);
  if (result)
    return result;

  /* If the builtin could not be folded, and it has no argument list,
     we're done.  */
  if (!arglist)
    return NULL_TREE;

  /* Limit the work only for builtins we know how to simplify.  */
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
    case BUILT_IN_FPUTS:
    case BUILT_IN_FPUTS_UNLOCKED:
      strlen_arg = 1;
      break;
    case BUILT_IN_STRCPY:
    case BUILT_IN_STRNCPY:
      strlen_arg = 2;
      break;
    default:
      return NULL_TREE;
    }

  /* Try to use the dataflow information gathered by the CCP process.  */
  visited = BITMAP_XMALLOC ();

  memset (strlen_val, 0, sizeof (strlen_val));
  for (i = 0, a = arglist;
       strlen_arg;
       i++, strlen_arg >>= 1, a = TREE_CHAIN (a))
    if (strlen_arg & 1)
      {
	bitmap_clear (visited);
	if (!get_strlen (TREE_VALUE (a), &strlen_val[i], visited))
	  strlen_val[i] = NULL_TREE;
      }

  BITMAP_XFREE (visited);

  /* FIXME.  All this code looks dangerous in the sense that it might
     create non-gimple expressions.  */
  switch (DECL_FUNCTION_CODE (callee))
    {
    case BUILT_IN_STRLEN:
      /* Convert from the internal "sizetype" type to "size_t".  */
      if (strlen_val[0]
	  && size_type_node)
	{
	  tree new = convert (size_type_node, strlen_val[0]);

	  /* If the result is not a valid gimple value, or not a cast
	     of a valid gimple value, then we can not use the result.  */
	  if (is_gimple_val (new)
	      || (is_gimple_cast (new)
		  && is_gimple_val (TREE_OPERAND (new, 0))))
	    return new;
	  else
	    return NULL_TREE;
	}
      return strlen_val[0];
    case BUILT_IN_STRCPY:
      if (strlen_val[1]
	  && is_gimple_val (strlen_val[1]))
      return simplify_builtin_strcpy (arglist, strlen_val[1]);
    case BUILT_IN_STRNCPY:
      if (strlen_val[1]
	  && is_gimple_val (strlen_val[1]))
      return simplify_builtin_strncpy (arglist, strlen_val[1]);
    case BUILT_IN_FPUTS:
      return simplify_builtin_fputs (arglist,
				     TREE_CODE (stmt) != MODIFY_EXPR, 0,
				     strlen_val[0]);
    case BUILT_IN_FPUTS_UNLOCKED:
      return simplify_builtin_fputs (arglist,
				     TREE_CODE (stmt) != MODIFY_EXPR, 1,
				     strlen_val[0]);

    default:
      abort ();
    }

  return NULL_TREE;
}


/* Return the string length of ARG in LENGTH.  If ARG is an SSA name variable,
   follow its use-def chains.  If LENGTH is not NULL and its value is not
   equal to the length we determine, or if we are unable to determine the
   length, return false.  VISITED is a bitmap of visited variables.  */

static bool
get_strlen (tree arg, tree *length, bitmap visited)
{
  tree var, def_stmt, val;
  
  if (TREE_CODE (arg) != SSA_NAME)
    {
      val = c_strlen (arg, 1);
      if (!val)
	return false;

      if (*length && simple_cst_equal (val, *length) != 1)
	return false;

      *length = val;
      return true;
    }

  /* If we were already here, break the infinite cycle.  */
  if (bitmap_bit_p (visited, SSA_NAME_VERSION (arg)))
    return true;
  bitmap_set_bit (visited, SSA_NAME_VERSION (arg));

  var = arg;
  def_stmt = SSA_NAME_DEF_STMT (var);

  switch (TREE_CODE (def_stmt))
    {
      case MODIFY_EXPR:
	{
	  tree len, rhs;
	  
	  /* The RHS of the statement defining VAR must either have a
	     constant length or come from another SSA_NAME with a constant
	     length.  */
	  rhs = TREE_OPERAND (def_stmt, 1);
	  STRIP_NOPS (rhs);
	  if (TREE_CODE (rhs) == SSA_NAME)
	    return get_strlen (rhs, length, visited);

	  /* See if the RHS is a constant length.  */
	  len = c_strlen (rhs, 1);
	  if (len)
	    {
	      if (*length && simple_cst_equal (len, *length) != 1)
		return false;

	      *length = len;
	      return true;
	    }

	  break;
	}

      case PHI_NODE:
	{
	  /* All the arguments of the PHI node must have the same constant
	     length.  */
	  int i;

	  for (i = 0; i < PHI_NUM_ARGS (def_stmt); i++)
	    {
	      tree arg = PHI_ARG_DEF (def_stmt, i);

	      /* If this PHI has itself as an argument, we cannot
		 determine the string length of this argument.  However,
		 if we can find a constant string length for the other
		 PHI args then we can still be sure that this is a
		 constant string length.  So be optimistic and just
		 continue with the next argument.  */
	      if (arg == PHI_RESULT (def_stmt))
		continue;

	      if (!get_strlen (arg, length, visited))
		return false;
	    }

	  return true;
	}

      default:
	break;
    }


  return false;
}


/* A simple pass that attempts to fold all builtin functions.  This pass
   is run after we've propagated as many constants as we can.  */

static void
execute_fold_all_builtins (void)
{
  basic_block bb;
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator i;
      for (i = bsi_start (bb); !bsi_end_p (i); bsi_next (&i))
	{
	  tree *stmtp = bsi_stmt_ptr (i);
	  tree call = get_rhs (*stmtp);
	  tree callee, result;

	  if (!call || TREE_CODE (call) != CALL_EXPR)
	    continue;
	  callee = get_callee_fndecl (call);
	  if (!callee || DECL_BUILT_IN_CLASS (callee) != BUILT_IN_NORMAL)
	    continue;

	  result = ccp_fold_builtin (*stmtp, call);
	  if (!result)
	    switch (DECL_FUNCTION_CODE (callee))
	      {
	      case BUILT_IN_CONSTANT_P:
		/* Resolve __builtin_constant_p.  If it hasn't been
		   folded to integer_one_node by now, it's fairly
		   certain that the value simply isn't constant.  */
		result = integer_zero_node;
		break;

	      default:
		continue;
	      }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Simplified\n  ");
	      print_generic_stmt (dump_file, *stmtp, dump_flags);
	    }

	  set_rhs (stmtp, result);
	  modify_stmt (*stmtp);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "to\n  ");
	      print_generic_stmt (dump_file, *stmtp, dump_flags);
	      fprintf (dump_file, "\n");
	    }
	}
    }
}

struct tree_opt_pass pass_fold_builtins = 
{
  "fab",				/* name */
  NULL,					/* gate */
  execute_fold_all_builtins,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  0,					/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_ssa	/* todo_flags_finish */
};


#include "gt-tree-ssa-ccp.h"
