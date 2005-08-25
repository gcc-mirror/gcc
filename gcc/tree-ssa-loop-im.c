/* Loop invariant motion.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.
   
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "domwalk.h"
#include "params.h"
#include "tree-pass.h"
#include "flags.h"

/* TODO:  Support for predicated code motion.  I.e.

   while (1)
     {
       if (cond)
	 {
	   a = inv;
	   something;
	 }
     }

   Where COND and INV are is invariants, but evaluating INV may trap or be
   invalid from some other reason if !COND.  This may be transformed to

   if (cond)
     a = inv;
   while (1)
     {
       if (cond)
	 something;
     }  */

/* A type for the list of statements that have to be moved in order to be able
   to hoist an invariant computation.  */

struct depend
{
  tree stmt;
  struct depend *next;
};

/* The auxiliary data kept for each statement.  */

struct lim_aux_data
{
  struct loop *max_loop;	/* The outermost loop in that the statement
				   is invariant.  */

  struct loop *tgt_loop;	/* The loop out of that we want to move the
				   invariant.  */

  struct loop *always_executed_in;
				/* The outermost loop for that we are sure
				   the statement is executed if the loop
				   is entered.  */

  bool sm_done;			/* True iff the store motion for a memory
				   reference in the statement has already
				   been executed.  */

  unsigned cost;		/* Cost of the computation performed by the
				   statement.  */

  struct depend *depends;	/* List of statements that must be also hoisted
				   out of the loop when this statement is
				   hoisted; i.e. those that define the operands
				   of the statement and are inside of the
				   MAX_LOOP loop.  */
};

#define LIM_DATA(STMT) (TREE_CODE (STMT) == PHI_NODE \
			? NULL \
			: (struct lim_aux_data *) (stmt_ann (STMT)->common.aux))

/* Description of a memory reference for store motion.  */

struct mem_ref
{
  tree *ref;			/* The reference itself.  */
  tree stmt;			/* The statement in that it occurs.  */
  struct mem_ref *next;		/* Next use in the chain.  */
};

/* Minimum cost of an expensive expression.  */
#define LIM_EXPENSIVE ((unsigned) PARAM_VALUE (PARAM_LIM_EXPENSIVE))

/* The outermost loop for that execution of the header guarantees that the
   block will be executed.  */
#define ALWAYS_EXECUTED_IN(BB) ((struct loop *) (BB)->aux)

static unsigned max_stmt_uid;	/* Maximal uid of a statement.  Uids to phi
				   nodes are assigned using the versions of
				   ssa names they define.  */

/* Returns uid of statement STMT.  */

static unsigned
get_stmt_uid (tree stmt)
{
  if (TREE_CODE (stmt) == PHI_NODE)
    return SSA_NAME_VERSION (PHI_RESULT (stmt)) + max_stmt_uid;

  return stmt_ann (stmt)->uid;
}

/* Calls CBCK for each index in memory reference ADDR_P.  There are two
   kinds situations handled; in each of these cases, the memory reference
   and DATA are passed to the callback:
   
   Access to an array: ARRAY_{RANGE_}REF (base, index).  In this case we also
   pass the pointer to the index to the callback.

   Pointer dereference: INDIRECT_REF (addr).  In this case we also pass the
   pointer to addr to the callback.
   
   If the callback returns false, the whole search stops and false is returned.
   Otherwise the function returns true after traversing through the whole
   reference *ADDR_P.  */

bool
for_each_index (tree *addr_p, bool (*cbck) (tree, tree *, void *), void *data)
{
  tree *nxt, *idx;

  for (; ; addr_p = nxt)
    {
      switch (TREE_CODE (*addr_p))
	{
	case SSA_NAME:
	  return cbck (*addr_p, addr_p, data);

	case MISALIGNED_INDIRECT_REF:
	case ALIGN_INDIRECT_REF:
	case INDIRECT_REF:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  return cbck (*addr_p, nxt, data);

	case BIT_FIELD_REF:
	case VIEW_CONVERT_EXPR:
	case ARRAY_RANGE_REF:
	case REALPART_EXPR:
	case IMAGPART_EXPR:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  break;

	case COMPONENT_REF:
	  /* If the component has varying offset, it behaves like index
	     as well.  */
	  idx = &TREE_OPERAND (*addr_p, 2);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;

	  nxt = &TREE_OPERAND (*addr_p, 0);
	  break;

	case ARRAY_REF:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  if (!cbck (*addr_p, &TREE_OPERAND (*addr_p, 1), data))
	    return false;
	  break;

	case VAR_DECL:
	case PARM_DECL:
	case STRING_CST:
	case RESULT_DECL:
	case VECTOR_CST:
	case COMPLEX_CST:
	case INTEGER_CST:
	case REAL_CST:
	  return true;

	default:
    	  gcc_unreachable ();
	}
    }
}

/* If it is possible to hoist the statement STMT unconditionally,
   returns MOVE_POSSIBLE.
   If it is possible to hoist the statement STMT, but we must avoid making
   it executed if it would not be executed in the original program (e.g.
   because it may trap), return MOVE_PRESERVE_EXECUTION.
   Otherwise return MOVE_IMPOSSIBLE.  */

enum move_pos
movement_possibility (tree stmt)
{
  tree lhs, rhs;

  if (flag_unswitch_loops
      && TREE_CODE (stmt) == COND_EXPR)
    {
      /* If we perform unswitching, force the operands of the invariant
	 condition to be moved out of the loop.  */
      get_stmt_operands (stmt);

      return MOVE_POSSIBLE;
    }

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return MOVE_IMPOSSIBLE;

  if (stmt_ends_bb_p (stmt))
    return MOVE_IMPOSSIBLE;

  get_stmt_operands (stmt);

  if (stmt_ann (stmt)->has_volatile_ops)
    return MOVE_IMPOSSIBLE;

  lhs = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (lhs) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return MOVE_IMPOSSIBLE;

  rhs = TREE_OPERAND (stmt, 1);

  if (TREE_SIDE_EFFECTS (rhs))
    return MOVE_IMPOSSIBLE;

  if (TREE_CODE (lhs) != SSA_NAME
      || tree_could_trap_p (rhs))
    return MOVE_PRESERVE_EXECUTION;

  if (get_call_expr_in (stmt))
    {
      /* While pure or const call is guaranteed to have no side effects, we
	 cannot move it arbitrarily.  Consider code like

	 char *s = something ();

	 while (1)
	   {
	     if (s)
	       t = strlen (s);
	     else
	       t = 0;
	   }

	 Here the strlen call cannot be moved out of the loop, even though
	 s is invariant.  In addition to possibly creating a call with
	 invalid arguments, moving out a function call that is not executed
	 may cause performance regressions in case the call is costly and
	 not executed at all.  */
      return MOVE_PRESERVE_EXECUTION;
    }
  return MOVE_POSSIBLE;
}

/* Suppose that operand DEF is used inside the LOOP.  Returns the outermost
   loop to that we could move the expression using DEF if it did not have
   other operands, i.e. the outermost loop enclosing LOOP in that the value
   of DEF is invariant.  */

static struct loop *
outermost_invariant_loop (tree def, struct loop *loop)
{
  tree def_stmt;
  basic_block def_bb;
  struct loop *max_loop;

  if (TREE_CODE (def) != SSA_NAME)
    return superloop_at_depth (loop, 1);

  def_stmt = SSA_NAME_DEF_STMT (def);
  def_bb = bb_for_stmt (def_stmt);
  if (!def_bb)
    return superloop_at_depth (loop, 1);

  max_loop = find_common_loop (loop, def_bb->loop_father);

  if (LIM_DATA (def_stmt) && LIM_DATA (def_stmt)->max_loop)
    max_loop = find_common_loop (max_loop,
				 LIM_DATA (def_stmt)->max_loop->outer);
  if (max_loop == loop)
    return NULL;
  max_loop = superloop_at_depth (loop, max_loop->depth + 1);

  return max_loop;
}

/* Returns the outermost superloop of LOOP in that the expression EXPR is
   invariant.  */

static struct loop *
outermost_invariant_loop_expr (tree expr, struct loop *loop)
{
  enum tree_code_class class = TREE_CODE_CLASS (TREE_CODE (expr));
  unsigned i, nops;
  struct loop *max_loop = superloop_at_depth (loop, 1), *aloop;

  if (TREE_CODE (expr) == SSA_NAME
      || TREE_CODE (expr) == INTEGER_CST
      || is_gimple_min_invariant (expr))
    return outermost_invariant_loop (expr, loop);

  if (class != tcc_unary
      && class != tcc_binary
      && class != tcc_expression
      && class != tcc_comparison)
    return NULL;

  nops = TREE_CODE_LENGTH (TREE_CODE (expr));
  for (i = 0; i < nops; i++)
    {
      aloop = outermost_invariant_loop_expr (TREE_OPERAND (expr, i), loop);
      if (!aloop)
	return NULL;

      if (flow_loop_nested_p (max_loop, aloop))
	max_loop = aloop;
    }

  return max_loop;
}

/* DATA is a structure containing information associated with a statement
   inside LOOP.  DEF is one of the operands of this statement.
   
   Find the outermost loop enclosing LOOP in that value of DEF is invariant
   and record this in DATA->max_loop field.  If DEF itself is defined inside
   this loop as well (i.e. we need to hoist it out of the loop if we want
   to hoist the statement represented by DATA), record the statement in that
   DEF is defined to the DATA->depends list.  Additionally if ADD_COST is true,
   add the cost of the computation of DEF to the DATA->cost.
   
   If DEF is not invariant in LOOP, return false.  Otherwise return TRUE.  */

static bool
add_dependency (tree def, struct lim_aux_data *data, struct loop *loop,
		bool add_cost)
{
  tree def_stmt = SSA_NAME_DEF_STMT (def);
  basic_block def_bb = bb_for_stmt (def_stmt);
  struct loop *max_loop;
  struct depend *dep;

  if (!def_bb)
    return true;

  max_loop = outermost_invariant_loop (def, loop);
  if (!max_loop)
    return false;

  if (flow_loop_nested_p (data->max_loop, max_loop))
    data->max_loop = max_loop;

  if (!LIM_DATA (def_stmt))
    return true;

  if (add_cost
      /* Only add the cost if the statement defining DEF is inside LOOP,
	 i.e. if it is likely that by moving the invariants dependent
	 on it, we will be able to avoid creating a new register for
	 it (since it will be only used in these dependent invariants).  */
      && def_bb->loop_father == loop)
    data->cost += LIM_DATA (def_stmt)->cost;

  dep = xmalloc (sizeof (struct depend));
  dep->stmt = def_stmt;
  dep->next = data->depends;
  data->depends = dep;

  return true;
}

/* Returns an estimate for a cost of statement STMT.  TODO -- the values here
   are just ad-hoc constants.  The estimates should be based on target-specific
   values.  */

static unsigned
stmt_cost (tree stmt)
{
  tree lhs, rhs;
  unsigned cost = 1;

  /* Always try to create possibilities for unswitching.  */
  if (TREE_CODE (stmt) == COND_EXPR)
    return LIM_EXPENSIVE;

  lhs = TREE_OPERAND (stmt, 0);
  rhs = TREE_OPERAND (stmt, 1);

  /* Hoisting memory references out should almost surely be a win.  */
  if (stmt_references_memory_p (stmt))
    cost += 20;

  switch (TREE_CODE (rhs))
    {
    case CALL_EXPR:
      /* We should be hoisting calls if possible.  */

      /* Unless the call is a builtin_constant_p; this always folds to a
	 constant, so moving it is useless.  */
      rhs = get_callee_fndecl (rhs);
      if (DECL_BUILT_IN_CLASS (rhs) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (rhs) == BUILT_IN_CONSTANT_P)
	return 0;

      cost += 20;
      break;

    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      /* Division and multiplication are usually expensive.  */
      cost += 20;
      break;

    default:
      break;
    }

  return cost;
}

/* Determine the outermost loop to that it is possible to hoist a statement
   STMT and store it to LIM_DATA (STMT)->max_loop.  To do this we determine
   the outermost loop in that the value computed by STMT is invariant.
   If MUST_PRESERVE_EXEC is true, additionally choose such a loop that
   we preserve the fact whether STMT is executed.  It also fills other related
   information to LIM_DATA (STMT).
   
   The function returns false if STMT cannot be hoisted outside of the loop it
   is defined in, and true otherwise.  */

static bool
determine_max_movement (tree stmt, bool must_preserve_exec)
{
  basic_block bb = bb_for_stmt (stmt);
  struct loop *loop = bb->loop_father;
  struct loop *level;
  struct lim_aux_data *lim_data = LIM_DATA (stmt);
  tree val;
  ssa_op_iter iter;
  
  if (must_preserve_exec)
    level = ALWAYS_EXECUTED_IN (bb);
  else
    level = superloop_at_depth (loop, 1);
  lim_data->max_loop = level;

  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_USE)
    if (!add_dependency (val, lim_data, loop, true))
      return false;

  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_VIRTUAL_USES | SSA_OP_VIRTUAL_KILLS)
    if (!add_dependency (val, lim_data, loop, false))
      return false;

  lim_data->cost += stmt_cost (stmt);

  return true;
}

/* Suppose that some statement in ORIG_LOOP is hoisted to the loop LEVEL,
   and that one of the operands of this statement is computed by STMT.
   Ensure that STMT (together with all the statements that define its
   operands) is hoisted at least out of the loop LEVEL.  */

static void
set_level (tree stmt, struct loop *orig_loop, struct loop *level)
{
  struct loop *stmt_loop = bb_for_stmt (stmt)->loop_father;
  struct depend *dep;

  stmt_loop = find_common_loop (orig_loop, stmt_loop);
  if (LIM_DATA (stmt) && LIM_DATA (stmt)->tgt_loop)
    stmt_loop = find_common_loop (stmt_loop,
				  LIM_DATA (stmt)->tgt_loop->outer);
  if (flow_loop_nested_p (stmt_loop, level))
    return;

  gcc_assert (LIM_DATA (stmt));
  gcc_assert (level == LIM_DATA (stmt)->max_loop
	      || flow_loop_nested_p (LIM_DATA (stmt)->max_loop, level));

  LIM_DATA (stmt)->tgt_loop = level;
  for (dep = LIM_DATA (stmt)->depends; dep; dep = dep->next)
    set_level (dep->stmt, orig_loop, level);
}

/* Determines an outermost loop from that we want to hoist the statement STMT.
   For now we chose the outermost possible loop.  TODO -- use profiling
   information to set it more sanely.  */

static void
set_profitable_level (tree stmt)
{
  set_level (stmt, bb_for_stmt (stmt)->loop_father, LIM_DATA (stmt)->max_loop);
}

/* Returns true if STMT is not a pure call.  */

static bool
nonpure_call_p (tree stmt)
{
  tree call = get_call_expr_in (stmt);

  if (!call)
    return false;

  return TREE_SIDE_EFFECTS (call) != 0;
}

/* Releases the memory occupied by DATA.  */

static void
free_lim_aux_data (struct lim_aux_data *data)
{
  struct depend *dep, *next;

  for (dep = data->depends; dep; dep = next)
    {
      next = dep->next;
      free (dep);
    }
  free (data);
}

/* Determine the outermost loops in that statements in basic block BB are
   invariant, and record them to the LIM_DATA associated with the statements.
   Callback for walk_dominator_tree.  */

static void
determine_invariantness_stmt (struct dom_walk_data *dw_data ATTRIBUTE_UNUSED,
			      basic_block bb)
{
  enum move_pos pos;
  block_stmt_iterator bsi;
  tree stmt;
  bool maybe_never = ALWAYS_EXECUTED_IN (bb) == NULL;
  struct loop *outermost = ALWAYS_EXECUTED_IN (bb);

  if (!bb->loop_father->outer)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Basic block %d (loop %d -- depth %d):\n\n",
	     bb->index, bb->loop_father->num, bb->loop_father->depth);

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      stmt = bsi_stmt (bsi);

      pos = movement_possibility (stmt);
      if (pos == MOVE_IMPOSSIBLE)
	{
	  if (nonpure_call_p (stmt))
	    {
	      maybe_never = true;
	      outermost = NULL;
	    }
	  continue;
	}

      stmt_ann (stmt)->common.aux = xcalloc (1, sizeof (struct lim_aux_data));
      LIM_DATA (stmt)->always_executed_in = outermost;

      if (maybe_never && pos == MOVE_PRESERVE_EXECUTION)
	continue;

      if (!determine_max_movement (stmt, pos == MOVE_PRESERVE_EXECUTION))
	{
	  LIM_DATA (stmt)->max_loop = NULL;
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  print_generic_stmt_indented (dump_file, stmt, 0, 2);
	  fprintf (dump_file, "  invariant up to level %d, cost %d.\n\n",
		   LIM_DATA (stmt)->max_loop->depth,
		   LIM_DATA (stmt)->cost);
	}

      if (LIM_DATA (stmt)->cost >= LIM_EXPENSIVE)
	set_profitable_level (stmt);
    }
}

/* For each statement determines the outermost loop in that it is invariant,
   statements on whose motion it depends and the cost of the computation.
   This information is stored to the LIM_DATA structure associated with
   each statement.  */

static void
determine_invariantness (void)
{
  struct dom_walk_data walk_data;

  memset (&walk_data, 0, sizeof (struct dom_walk_data));
  walk_data.before_dom_children_before_stmts = determine_invariantness_stmt;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);
}

/* Commits edge insertions and updates loop structures.  */

void
loop_commit_inserts (void)
{
  unsigned old_last_basic_block, i;
  basic_block bb;

  old_last_basic_block = last_basic_block;
  bsi_commit_edge_inserts ();
  for (i = old_last_basic_block; i < (unsigned) last_basic_block; i++)
    {
      bb = BASIC_BLOCK (i);
      add_bb_to_loop (bb,
		      find_common_loop (EDGE_SUCC (bb, 0)->dest->loop_father,
					EDGE_PRED (bb, 0)->src->loop_father));
    }
}

/* Hoist the statements in basic block BB out of the loops prescribed by
   data stored in LIM_DATA structures associated with each statement.  Callback
   for walk_dominator_tree.  */

static void
move_computations_stmt (struct dom_walk_data *dw_data ATTRIBUTE_UNUSED,
			basic_block bb)
{
  struct loop *level;
  block_stmt_iterator bsi;
  tree stmt;
  unsigned cost = 0;

  if (!bb->loop_father->outer)
    return;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); )
    {
      stmt = bsi_stmt (bsi);

      if (!LIM_DATA (stmt))
	{
	  bsi_next (&bsi);
	  continue;
	}

      cost = LIM_DATA (stmt)->cost;
      level = LIM_DATA (stmt)->tgt_loop;
      free_lim_aux_data (LIM_DATA (stmt));
      stmt_ann (stmt)->common.aux = NULL;

      if (!level)
	{
	  bsi_next (&bsi);
	  continue;
	}

      /* We do not really want to move conditionals out of the loop; we just
	 placed it here to force its operands to be moved if necessary.  */
      if (TREE_CODE (stmt) == COND_EXPR)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Moving statement\n");
	  print_generic_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "(cost %u) out of loop %d.\n\n",
		   cost, level->num);
	}
      bsi_insert_on_edge (loop_preheader_edge (level), stmt);
      bsi_remove (&bsi);
    }
}

/* Hoist the statements out of the loops prescribed by data stored in
   LIM_DATA structures associated with each statement.*/

static void
move_computations (void)
{
  struct dom_walk_data walk_data;

  memset (&walk_data, 0, sizeof (struct dom_walk_data));
  walk_data.before_dom_children_before_stmts = move_computations_stmt;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);

  loop_commit_inserts ();
  rewrite_into_ssa (false);
  if (!bitmap_empty_p (vars_to_rename))
    {
      /* The rewrite of ssa names may cause violation of loop closed ssa
	 form invariants.  TODO -- avoid these rewrites completely.
	 Information in virtual phi nodes is sufficient for it.  */
      rewrite_into_loop_closed_ssa ();
    }
  bitmap_clear (vars_to_rename);
}

/* Checks whether the statement defining variable *INDEX can be hoisted
   out of the loop passed in DATA.  Callback for for_each_index.  */

static bool
may_move_till (tree ref, tree *index, void *data)
{
  struct loop *loop = data, *max_loop;

  /* If REF is an array reference, check also that the step and the lower
     bound is invariant in LOOP.  */
  if (TREE_CODE (ref) == ARRAY_REF)
    {
      tree step = array_ref_element_size (ref);
      tree lbound = array_ref_low_bound (ref);

      max_loop = outermost_invariant_loop_expr (step, loop);
      if (!max_loop)
	return false;

      max_loop = outermost_invariant_loop_expr (lbound, loop);
      if (!max_loop)
	return false;
    }

  max_loop = outermost_invariant_loop (*index, loop);
  if (!max_loop)
    return false;

  return true;
}

/* Forces statements defining (invariant) SSA names in expression EXPR to be
   moved out of the LOOP.  ORIG_LOOP is the loop in that EXPR is used.  */

static void
force_move_till_expr (tree expr, struct loop *orig_loop, struct loop *loop)
{
  enum tree_code_class class = TREE_CODE_CLASS (TREE_CODE (expr));
  unsigned i, nops;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      tree stmt = SSA_NAME_DEF_STMT (expr);
      if (IS_EMPTY_STMT (stmt))
	return;

      set_level (stmt, orig_loop, loop);
      return;
    }

  if (class != tcc_unary
      && class != tcc_binary
      && class != tcc_expression
      && class != tcc_comparison)
    return;

  nops = TREE_CODE_LENGTH (TREE_CODE (expr));
  for (i = 0; i < nops; i++)
    force_move_till_expr (TREE_OPERAND (expr, i), orig_loop, loop);
}

/* Forces statement defining invariants in REF (and *INDEX) to be moved out of
   the LOOP.  The reference REF is used in the loop ORIG_LOOP.  Callback for
   for_each_index.  */

struct fmt_data
{
  struct loop *loop;
  struct loop *orig_loop;
};

static bool
force_move_till (tree ref, tree *index, void *data)
{
  tree stmt;
  struct fmt_data *fmt_data = data;

  if (TREE_CODE (ref) == ARRAY_REF)
    {
      tree step = array_ref_element_size (ref);
      tree lbound = array_ref_low_bound (ref);

      force_move_till_expr (step, fmt_data->orig_loop, fmt_data->loop);
      force_move_till_expr (lbound, fmt_data->orig_loop, fmt_data->loop);
    }

  if (TREE_CODE (*index) != SSA_NAME)
    return true;

  stmt = SSA_NAME_DEF_STMT (*index);
  if (IS_EMPTY_STMT (stmt))
    return true;

  set_level (stmt, fmt_data->orig_loop, fmt_data->loop);

  return true;
}

/* Records memory reference *REF (that occurs in statement STMT)
   to the list MEM_REFS.  */

static void
record_mem_ref (struct mem_ref **mem_refs, tree stmt, tree *ref)
{
  struct mem_ref *aref = xmalloc (sizeof (struct mem_ref));

  aref->stmt = stmt;
  aref->ref = ref;

  aref->next = *mem_refs;
  *mem_refs = aref;
}

/* Releases list of memory references MEM_REFS.  */

static void
free_mem_refs (struct mem_ref *mem_refs)
{
  struct mem_ref *act;

  while (mem_refs)
    {
      act = mem_refs;
      mem_refs = mem_refs->next;
      free (act);
    }
}

/* If VAR is defined in LOOP and the statement it is defined in does not belong
   to the set SEEN, add the statement to QUEUE of length IN_QUEUE and
   to the set SEEN.  */

static void
maybe_queue_var (tree var, struct loop *loop,
		 sbitmap seen, tree *queue, unsigned *in_queue)
{
  tree stmt = SSA_NAME_DEF_STMT (var);
  basic_block def_bb = bb_for_stmt (stmt);
	      
  if (!def_bb
      || !flow_bb_inside_loop_p (loop, def_bb)
      || TEST_BIT (seen, get_stmt_uid (stmt)))
    return;
	  
  SET_BIT (seen, get_stmt_uid (stmt));
  queue[(*in_queue)++] = stmt;
}

/* If COMMON_REF is NULL, set COMMON_REF to *OP and return true.
   Otherwise return true if the memory reference *OP is equal to COMMON_REF.
   Record the reference OP to list MEM_REFS.  STMT is the statement in that
   the reference occurs.  */

struct sra_data
{
  struct mem_ref **mem_refs;
  tree common_ref;
  tree stmt;
};

static bool
fem_single_reachable_address (tree *op, void *data)
{
  struct sra_data *sra_data = data;

  if (sra_data->common_ref
      && !operand_equal_p (*op, sra_data->common_ref, 0))
    return false;
  sra_data->common_ref = *op;

  record_mem_ref (sra_data->mem_refs, sra_data->stmt, op);
  return true;
}

/* Runs CALLBACK for each operand of STMT that is a memory reference.  DATA
   is passed to the CALLBACK as well.  The traversal stops if CALLBACK
   returns false, for_each_memref then returns false as well.  Otherwise
   for_each_memref returns true.  */

static bool
for_each_memref (tree stmt, bool (*callback)(tree *, void *), void *data)
{
  tree *op;

  if (TREE_CODE (stmt) == RETURN_EXPR)
    stmt = TREE_OPERAND (stmt, 1);

  if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      op = &TREE_OPERAND (stmt, 0);
      if (TREE_CODE (*op) != SSA_NAME
	  && !callback (op, data))
	return false;

      op = &TREE_OPERAND (stmt, 1);
      if (TREE_CODE (*op) != SSA_NAME
	  && is_gimple_lvalue (*op)
	  && !callback (op, data))
	return false;

      stmt = TREE_OPERAND (stmt, 1);
    }

  if (TREE_CODE (stmt) == WITH_SIZE_EXPR)
    stmt = TREE_OPERAND (stmt, 0);

  if (TREE_CODE (stmt) == CALL_EXPR)
    {
      tree args;

      for (args = TREE_OPERAND (stmt, 1); args; args = TREE_CHAIN (args))
	{
	  op = &TREE_VALUE (args);

	  if (TREE_CODE (*op) != SSA_NAME
	      && is_gimple_lvalue (*op)
	      && !callback (op, data))
	    return false;
	}
    }

  return true;
}

/* Determine whether all memory references inside the LOOP that correspond
   to virtual ssa names defined in statement STMT are equal.
   If so, store the list of the references to MEM_REFS, and return one
   of them.  Otherwise store NULL to MEM_REFS and return NULL_TREE.
   *SEEN_CALL_STMT is set to true if the virtual operands suggest
   that the reference might be clobbered by a call inside the LOOP.  */

static tree
single_reachable_address (struct loop *loop, tree stmt,
			  struct mem_ref **mem_refs,
			  bool *seen_call_stmt)
{
  unsigned max_uid = max_stmt_uid + num_ssa_names;
  tree *queue = xmalloc (sizeof (tree) * max_uid);
  sbitmap seen = sbitmap_alloc (max_uid);
  unsigned in_queue = 1;
  dataflow_t df;
  unsigned i, n;
  struct sra_data sra_data;
  tree call;
  tree val;
  ssa_op_iter iter;

  sbitmap_zero (seen);

  *mem_refs = NULL;
  sra_data.mem_refs = mem_refs;
  sra_data.common_ref = NULL_TREE;

  queue[0] = stmt;
  SET_BIT (seen, get_stmt_uid (stmt));
  *seen_call_stmt = false;

  while (in_queue)
    {
      stmt = queue[--in_queue];
      sra_data.stmt = stmt;

      if (LIM_DATA (stmt)
	  && LIM_DATA (stmt)->sm_done)
	goto fail;

      switch (TREE_CODE (stmt))
	{
	case MODIFY_EXPR:
	case CALL_EXPR:
	case RETURN_EXPR:
	  if (!for_each_memref (stmt, fem_single_reachable_address,
				&sra_data))
	    goto fail;

	  /* If this is a function that may depend on the memory location,
	     record the fact.  We cannot directly refuse call clobbered
	     operands here, since sra_data.common_ref did not have
	     to be set yet.  */
	  call = get_call_expr_in (stmt);
	  if (call
	      && !(call_expr_flags (call) & ECF_CONST))
	    *seen_call_stmt = true;

	  /* Traverse also definitions of the VUSES (there may be other
	     distinct from the one we used to get to this statement).  */
	  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_VIRTUAL_USES)
	    maybe_queue_var (val, loop, seen, queue, &in_queue);

	  break;

	case PHI_NODE:
	  for (i = 0; i < (unsigned) PHI_NUM_ARGS (stmt); i++)
	    if (TREE_CODE (PHI_ARG_DEF (stmt, i)) == SSA_NAME)
	      maybe_queue_var (PHI_ARG_DEF (stmt, i), loop,
		               seen, queue, &in_queue);
	  break;

	default:
	  goto fail;
	}

      /* Find uses of virtual names.  */
      df = get_immediate_uses (stmt);
      n = num_immediate_uses (df);

      for (i = 0; i < n; i++)
	{
	  stmt = immediate_use (df, i);

	  if (!flow_bb_inside_loop_p (loop, bb_for_stmt (stmt)))
	    continue;

	  if (TEST_BIT (seen, get_stmt_uid (stmt)))
	    continue;
	  SET_BIT (seen, get_stmt_uid (stmt));

	  queue[in_queue++] = stmt;
	}
    }

  free (queue);
  sbitmap_free (seen);

  return sra_data.common_ref;

fail:
  free_mem_refs (*mem_refs);
  *mem_refs = NULL;
  free (queue);
  sbitmap_free (seen);

  return NULL;
}

/* Rewrites memory references in list MEM_REFS by variable TMP_VAR.  */

static void
rewrite_mem_refs (tree tmp_var, struct mem_ref *mem_refs)
{
  tree var;
  ssa_op_iter iter;

  for (; mem_refs; mem_refs = mem_refs->next)
    {
      FOR_EACH_SSA_TREE_OPERAND (var, mem_refs->stmt, iter, SSA_OP_ALL_VIRTUALS)
	{
	  var = SSA_NAME_VAR (var);
	  bitmap_set_bit (vars_to_rename, var_ann (var)->uid);
	}

      *mem_refs->ref = tmp_var;
      modify_stmt (mem_refs->stmt);
    }
}

/* Records request for store motion of memory reference REF from LOOP.
   MEM_REFS is the list of occurrences of the reference REF inside LOOP;
   these references are rewritten by a new temporary variable.
   Exits from the LOOP are stored in EXITS, there are N_EXITS of them.
   The initialization of the temporary variable is put to the preheader
   of the loop, and assignments to the reference from the temporary variable
   are emitted to exits.  */

static void
schedule_sm (struct loop *loop, edge *exits, unsigned n_exits, tree ref,
	     struct mem_ref *mem_refs)
{
  struct mem_ref *aref;
  tree tmp_var;
  unsigned i;
  tree load, store;
  struct fmt_data fmt_data;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Executing store motion of ");
      print_generic_expr (dump_file, ref, 0);
      fprintf (dump_file, " from loop %d\n", loop->num);
    }

  tmp_var = make_rename_temp (TREE_TYPE (ref), "lsm_tmp");

  fmt_data.loop = loop;
  fmt_data.orig_loop = loop;
  for_each_index (&ref, force_move_till, &fmt_data);

  rewrite_mem_refs (tmp_var, mem_refs);
  for (aref = mem_refs; aref; aref = aref->next)
    if (LIM_DATA (aref->stmt))
      LIM_DATA (aref->stmt)->sm_done = true;

  /* Emit the load & stores.  */
  load = build (MODIFY_EXPR, void_type_node, tmp_var, ref);
  get_stmt_ann (load)->common.aux = xcalloc (1, sizeof (struct lim_aux_data));
  LIM_DATA (load)->max_loop = loop;
  LIM_DATA (load)->tgt_loop = loop;

  /* Put this into the latch, so that we are sure it will be processed after
     all dependencies.  */
  bsi_insert_on_edge (loop_latch_edge (loop), load);

  for (i = 0; i < n_exits; i++)
    {
      store = build (MODIFY_EXPR, void_type_node,
		     unshare_expr (ref), tmp_var);
      bsi_insert_on_edge (exits[i], store);
    }
}

/* Returns true if REF may be clobbered by calls.  */

static bool
is_call_clobbered_ref (tree ref)
{
  tree base;

  base = get_base_address (ref);
  if (!base)
    return true;

  if (DECL_P (base))
    return is_call_clobbered (base);

  if (INDIRECT_REF_P (base))
    {
      /* Check whether the alias tags associated with the pointer
	 are call clobbered.  */
      tree ptr = TREE_OPERAND (base, 0);
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);
      tree nmt = (pi) ? pi->name_mem_tag : NULL_TREE;
      tree tmt = var_ann (SSA_NAME_VAR (ptr))->type_mem_tag;

      if ((nmt && is_call_clobbered (nmt))
	  || (tmt && is_call_clobbered (tmt)))
	return true;

      return false;
    }

  gcc_unreachable ();
}

/* Determine whether all memory references inside LOOP corresponding to the
   virtual ssa name REG are equal to each other, and whether the address of
   this common reference can be hoisted outside of the loop.  If this is true,
   prepare the statements that load the value of the memory reference to a
   temporary variable in the loop preheader, store it back on the loop exits,
   and replace all the references inside LOOP by this temporary variable.
   LOOP has N_EXITS stored in EXITS.  */

static void
determine_lsm_reg (struct loop *loop, edge *exits, unsigned n_exits, tree reg)
{
  tree ref;
  struct mem_ref *mem_refs, *aref;
  struct loop *must_exec;
  bool sees_call;
  
  if (is_gimple_reg (reg))
    return;
  
  ref = single_reachable_address (loop, SSA_NAME_DEF_STMT (reg), &mem_refs,
				  &sees_call);
  if (!ref)
    return;

  /* If we cannot create a ssa name for the result, give up.  */
  if (!is_gimple_reg_type (TREE_TYPE (ref))
      || TREE_THIS_VOLATILE (ref))
    goto fail;

  /* If there is a call that may use the location, give up as well.  */
  if (sees_call
      && is_call_clobbered_ref (ref))
    goto fail;

  if (!for_each_index (&ref, may_move_till, loop))
    goto fail;

  if (tree_could_trap_p (ref))
    {
      /* If the memory access is unsafe (i.e. it might trap), ensure that some
	 of the statements in that it occurs is always executed when the loop
	 is entered.  This way we know that by moving the load from the
	 reference out of the loop we will not cause the error that would not
	 occur otherwise.

	 TODO -- in fact we would like to check for anticipability of the
	 reference, i.e. that on each path from loop entry to loop exit at
	 least one of the statements containing the memory reference is
	 executed.  */

      for (aref = mem_refs; aref; aref = aref->next)
	{
	  if (!LIM_DATA (aref->stmt))
	    continue;

	  must_exec = LIM_DATA (aref->stmt)->always_executed_in;
	  if (!must_exec)
	    continue;

	  if (must_exec == loop
	      || flow_loop_nested_p (must_exec, loop))
	    break;
	}

      if (!aref)
	goto fail;
    }

  schedule_sm (loop, exits, n_exits, ref, mem_refs);

fail: ;
  free_mem_refs (mem_refs);
}

/* Checks whether LOOP (with N_EXITS exits stored in EXITS array) is suitable
   for a store motion optimization (i.e. whether we can insert statement
   on its exits).  */

static bool
loop_suitable_for_sm (struct loop *loop ATTRIBUTE_UNUSED, edge *exits,
		      unsigned n_exits)
{
  unsigned i;

  for (i = 0; i < n_exits; i++)
    if (exits[i]->flags & EDGE_ABNORMAL)
      return false;

  return true;
}

/* Try to perform store motion for all memory references modified inside
   LOOP.  */

static void
determine_lsm_loop (struct loop *loop)
{
  tree phi;
  unsigned n_exits;
  edge *exits = get_loop_exit_edges (loop, &n_exits);

  if (!loop_suitable_for_sm (loop, exits, n_exits))
    {
      free (exits);
      return;
    }

  for (phi = phi_nodes (loop->header); phi; phi = PHI_CHAIN (phi))
    determine_lsm_reg (loop, exits, n_exits, PHI_RESULT (phi));

  free (exits);
}

/* Try to perform store motion for all memory references modified inside
   any of LOOPS.  */

static void
determine_lsm (struct loops *loops)
{
  struct loop *loop;
  basic_block bb;

  if (!loops->tree_root->inner)
    return;

  /* Create a UID for each statement in the function.  Ordering of the
     UIDs is not important for this pass.  */
  max_stmt_uid = 0;
  FOR_EACH_BB (bb)
    {
      block_stmt_iterator bsi;

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	stmt_ann (bsi_stmt (bsi))->uid = max_stmt_uid++;
    }

  compute_immediate_uses (TDFA_USE_VOPS, NULL);

  /* Pass the loops from the outermost.  For each virtual operand loop phi node
     check whether all the references inside the loop correspond to a single
     address, and if so, move them.  */

  loop = loops->tree_root->inner;
  while (1)
    {
      determine_lsm_loop (loop);

      if (loop->inner)
	{
	  loop = loop->inner;
	  continue;
	}
      while (!loop->next)
	{
	  loop = loop->outer;
	  if (loop == loops->tree_root)
	    {
	      free_df ();
	      loop_commit_inserts ();
	      return;
	    }
	}
      loop = loop->next;
    }
}

/* Fills ALWAYS_EXECUTED_IN information for basic blocks of LOOP, i.e.
   for each such basic block bb records the outermost loop for that execution
   of its header implies execution of bb.  CONTAINS_CALL is the bitmap of
   blocks that contain a nonpure call.  */

static void
fill_always_executed_in (struct loop *loop, sbitmap contains_call)
{
  basic_block bb = NULL, *bbs, last = NULL;
  unsigned i;
  edge e;
  struct loop *inn_loop = loop;

  if (!loop->header->aux)
    {
      bbs = get_loop_body_in_dom_order (loop);

      for (i = 0; i < loop->num_nodes; i++)
	{
	  edge_iterator ei;
	  bb = bbs[i];

	  if (dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
	    last = bb;

	  if (TEST_BIT (contains_call, bb->index))
	    break;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!flow_bb_inside_loop_p (loop, e->dest))
	      break;
	  if (e)
	    break;

	  /* A loop might be infinite (TODO use simple loop analysis
	     to disprove this if possible).  */
	  if (bb->flags & BB_IRREDUCIBLE_LOOP)
	    break;

	  if (!flow_bb_inside_loop_p (inn_loop, bb))
	    break;

	  if (bb->loop_father->header == bb)
	    {
	      if (!dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
		break;

	      /* In a loop that is always entered we may proceed anyway.
		 But record that we entered it and stop once we leave it.  */
	      inn_loop = bb->loop_father;
	    }
	}

      while (1)
	{
	  last->aux = loop;
	  if (last == loop->header)
	    break;
	  last = get_immediate_dominator (CDI_DOMINATORS, last);
	}

      free (bbs);
    }

  for (loop = loop->inner; loop; loop = loop->next)
    fill_always_executed_in (loop, contains_call);
}

/* Compute the global information needed by the loop invariant motion pass.
   LOOPS is the loop tree.  */

static void
tree_ssa_lim_initialize (struct loops *loops)
{
  sbitmap contains_call = sbitmap_alloc (last_basic_block);
  block_stmt_iterator bsi;
  struct loop *loop;
  basic_block bb;

  sbitmap_zero (contains_call);
  FOR_EACH_BB (bb)
    {
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  if (nonpure_call_p (bsi_stmt (bsi)))
	    break;
	}

      if (!bsi_end_p (bsi))
	SET_BIT (contains_call, bb->index);
    }

  for (loop = loops->tree_root->inner; loop; loop = loop->next)
    fill_always_executed_in (loop, contains_call);

  sbitmap_free (contains_call);
}

/* Cleans up after the invariant motion pass.  */

static void
tree_ssa_lim_finalize (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      bb->aux = NULL;
    }
}

/* Moves invariants from LOOPS.  Only "expensive" invariants are moved out --
   i.e. those that are likely to be win regardless of the register pressure.  */

void
tree_ssa_lim (struct loops *loops)
{
  tree_ssa_lim_initialize (loops);

  /* For each statement determine the outermost loop in that it is
     invariant and cost for computing the invariant.  */
  determine_invariantness ();

  /* For each memory reference determine whether it is possible to hoist it
     out of the loop.  Force the necessary invariants to be moved out of the
     loops as well.  */
  determine_lsm (loops);

  /* Move the expressions that are expensive enough.  */
  move_computations ();

  tree_ssa_lim_finalize ();
}
