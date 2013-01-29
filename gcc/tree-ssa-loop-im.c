/* Loop invariant motion.
   Copyright (C) 2003-2013 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "domwalk.h"
#include "params.h"
#include "tree-pass.h"
#include "flags.h"
#include "hashtab.h"
#include "tree-affine.h"
#include "pointer-set.h"
#include "tree-ssa-propagate.h"

/* TODO:  Support for predicated code motion.  I.e.

   while (1)
     {
       if (cond)
	 {
	   a = inv;
	   something;
	 }
     }

   Where COND and INV are invariants, but evaluating INV may trap or be
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
  gimple stmt;
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

  unsigned cost;		/* Cost of the computation performed by the
				   statement.  */

  struct depend *depends;	/* List of statements that must be also hoisted
				   out of the loop when this statement is
				   hoisted; i.e. those that define the operands
				   of the statement and are inside of the
				   MAX_LOOP loop.  */
};

/* Maps statements to their lim_aux_data.  */

static struct pointer_map_t *lim_aux_data_map;

/* Description of a memory reference location.  */

typedef struct mem_ref_loc
{
  tree *ref;			/* The reference itself.  */
  gimple stmt;			/* The statement in that it occurs.  */
} *mem_ref_loc_p;


/* The list of memory reference locations in a loop.  */

typedef struct mem_ref_locs
{
  vec<mem_ref_loc_p> locs;
} *mem_ref_locs_p;


/* Description of a memory reference.  */

typedef struct mem_ref
{
  tree mem;			/* The memory itself.  */
  unsigned id;			/* ID assigned to the memory reference
				   (its index in memory_accesses.refs_list)  */
  hashval_t hash;		/* Its hash value.  */
  bitmap stored;		/* The set of loops in that this memory location
				   is stored to.  */
  vec<mem_ref_locs_p> accesses_in_loop;
				/* The locations of the accesses.  Vector
				   indexed by the loop number.  */

  /* The following sets are computed on demand.  We keep both set and
     its complement, so that we know whether the information was
     already computed or not.  */
  bitmap indep_loop;		/* The set of loops in that the memory
				   reference is independent, meaning:
				   If it is stored in the loop, this store
				     is independent on all other loads and
				     stores.
				   If it is only loaded, then it is independent
				     on all stores in the loop.  */
  bitmap dep_loop;		/* The complement of INDEP_LOOP.  */

  bitmap indep_ref;		/* The set of memory references on that
				   this reference is independent.  */
  bitmap dep_ref;		/* The complement of INDEP_REF.  */
} *mem_ref_p;




/* Description of memory accesses in loops.  */

static struct
{
  /* The hash table of memory references accessed in loops.  */
  htab_t refs;

  /* The list of memory references.  */
  vec<mem_ref_p> refs_list;

  /* The set of memory references accessed in each loop.  */
  vec<bitmap> refs_in_loop;

  /* The set of memory references accessed in each loop, including
     subloops.  */
  vec<bitmap> all_refs_in_loop;

  /* The set of memory references stored in each loop, including
     subloops.  */
  vec<bitmap> all_refs_stored_in_loop;

  /* Cache for expanding memory addresses.  */
  struct pointer_map_t *ttae_cache;
} memory_accesses;

/* Obstack for the bitmaps in the above data structures.  */
static bitmap_obstack lim_bitmap_obstack;

static bool ref_indep_loop_p (struct loop *, mem_ref_p);

/* Minimum cost of an expensive expression.  */
#define LIM_EXPENSIVE ((unsigned) PARAM_VALUE (PARAM_LIM_EXPENSIVE))

/* The outermost loop for which execution of the header guarantees that the
   block will be executed.  */
#define ALWAYS_EXECUTED_IN(BB) ((struct loop *) (BB)->aux)
#define SET_ALWAYS_EXECUTED_IN(BB, VAL) ((BB)->aux = (void *) (VAL))

/* Whether the reference was analyzable.  */
#define MEM_ANALYZABLE(REF) ((REF)->mem != error_mark_node)

static struct lim_aux_data *
init_lim_data (gimple stmt)
{
  void **p = pointer_map_insert (lim_aux_data_map, stmt);

  *p = XCNEW (struct lim_aux_data);
  return (struct lim_aux_data *) *p;
}

static struct lim_aux_data *
get_lim_data (gimple stmt)
{
  void **p = pointer_map_contains (lim_aux_data_map, stmt);
  if (!p)
    return NULL;

  return (struct lim_aux_data *) *p;
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

static void
clear_lim_data (gimple stmt)
{
  void **p = pointer_map_contains (lim_aux_data_map, stmt);
  if (!p)
    return;

  free_lim_aux_data ((struct lim_aux_data *) *p);
  *p = NULL;
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

	case MEM_REF:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  return cbck (*addr_p, nxt, data);

	case BIT_FIELD_REF:
	case VIEW_CONVERT_EXPR:
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
	case ARRAY_RANGE_REF:
	  nxt = &TREE_OPERAND (*addr_p, 0);
	  if (!cbck (*addr_p, &TREE_OPERAND (*addr_p, 1), data))
	    return false;
	  break;

	case VAR_DECL:
	case PARM_DECL:
	case CONST_DECL:
	case STRING_CST:
	case RESULT_DECL:
	case VECTOR_CST:
	case COMPLEX_CST:
	case INTEGER_CST:
	case REAL_CST:
	case FIXED_CST:
	case CONSTRUCTOR:
	  return true;

	case ADDR_EXPR:
	  gcc_assert (is_gimple_min_invariant (*addr_p));
	  return true;

	case TARGET_MEM_REF:
	  idx = &TMR_BASE (*addr_p);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;
	  idx = &TMR_INDEX (*addr_p);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;
	  idx = &TMR_INDEX2 (*addr_p);
	  if (*idx
	      && !cbck (*addr_p, idx, data))
	    return false;
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
movement_possibility (gimple stmt)
{
  tree lhs;
  enum move_pos ret = MOVE_POSSIBLE;

  if (flag_unswitch_loops
      && gimple_code (stmt) == GIMPLE_COND)
    {
      /* If we perform unswitching, force the operands of the invariant
	 condition to be moved out of the loop.  */
      return MOVE_POSSIBLE;
    }

  if (gimple_code (stmt) == GIMPLE_PHI
      && gimple_phi_num_args (stmt) <= 2
      && !virtual_operand_p (gimple_phi_result (stmt))
      && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (stmt)))
    return MOVE_POSSIBLE;

  if (gimple_get_lhs (stmt) == NULL_TREE)
    return MOVE_IMPOSSIBLE;

  if (gimple_vdef (stmt))
    return MOVE_IMPOSSIBLE;

  if (stmt_ends_bb_p (stmt)
      || gimple_has_volatile_ops (stmt)
      || gimple_has_side_effects (stmt)
      || stmt_could_throw_p (stmt))
    return MOVE_IMPOSSIBLE;

  if (is_gimple_call (stmt))
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
      ret = MOVE_PRESERVE_EXECUTION;
      lhs = gimple_call_lhs (stmt);
    }
  else if (is_gimple_assign (stmt))
    lhs = gimple_assign_lhs (stmt);
  else
    return MOVE_IMPOSSIBLE;

  if (TREE_CODE (lhs) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return MOVE_IMPOSSIBLE;

  if (TREE_CODE (lhs) != SSA_NAME
      || gimple_could_trap_p (stmt))
    return MOVE_PRESERVE_EXECUTION;

  /* Non local loads in a transaction cannot be hoisted out.  Well,
     unless the load happens on every path out of the loop, but we
     don't take this into account yet.  */
  if (flag_tm
      && gimple_in_transaction (stmt)
      && gimple_assign_single_p (stmt))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      if (DECL_P (rhs) && is_global_var (rhs))
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Cannot hoist conditional load of ");
	      print_generic_expr (dump_file, rhs, TDF_SLIM);
	      fprintf (dump_file, " because it is in a transaction.\n");
	    }
	  return MOVE_IMPOSSIBLE;
	}
    }

  return ret;
}

/* Suppose that operand DEF is used inside the LOOP.  Returns the outermost
   loop to that we could move the expression using DEF if it did not have
   other operands, i.e. the outermost loop enclosing LOOP in that the value
   of DEF is invariant.  */

static struct loop *
outermost_invariant_loop (tree def, struct loop *loop)
{
  gimple def_stmt;
  basic_block def_bb;
  struct loop *max_loop;
  struct lim_aux_data *lim_data;

  if (!def)
    return superloop_at_depth (loop, 1);

  if (TREE_CODE (def) != SSA_NAME)
    {
      gcc_assert (is_gimple_min_invariant (def));
      return superloop_at_depth (loop, 1);
    }

  def_stmt = SSA_NAME_DEF_STMT (def);
  def_bb = gimple_bb (def_stmt);
  if (!def_bb)
    return superloop_at_depth (loop, 1);

  max_loop = find_common_loop (loop, def_bb->loop_father);

  lim_data = get_lim_data (def_stmt);
  if (lim_data != NULL && lim_data->max_loop != NULL)
    max_loop = find_common_loop (max_loop,
				 loop_outer (lim_data->max_loop));
  if (max_loop == loop)
    return NULL;
  max_loop = superloop_at_depth (loop, loop_depth (max_loop) + 1);

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
  gimple def_stmt = SSA_NAME_DEF_STMT (def);
  basic_block def_bb = gimple_bb (def_stmt);
  struct loop *max_loop;
  struct depend *dep;
  struct lim_aux_data *def_data;

  if (!def_bb)
    return true;

  max_loop = outermost_invariant_loop (def, loop);
  if (!max_loop)
    return false;

  if (flow_loop_nested_p (data->max_loop, max_loop))
    data->max_loop = max_loop;

  def_data = get_lim_data (def_stmt);
  if (!def_data)
    return true;

  if (add_cost
      /* Only add the cost if the statement defining DEF is inside LOOP,
	 i.e. if it is likely that by moving the invariants dependent
	 on it, we will be able to avoid creating a new register for
	 it (since it will be only used in these dependent invariants).  */
      && def_bb->loop_father == loop)
    data->cost += def_data->cost;

  dep = XNEW (struct depend);
  dep->stmt = def_stmt;
  dep->next = data->depends;
  data->depends = dep;

  return true;
}

/* Returns an estimate for a cost of statement STMT.  The values here
   are just ad-hoc constants, similar to costs for inlining.  */

static unsigned
stmt_cost (gimple stmt)
{
  /* Always try to create possibilities for unswitching.  */
  if (gimple_code (stmt) == GIMPLE_COND
      || gimple_code (stmt) == GIMPLE_PHI)
    return LIM_EXPENSIVE;

  /* We should be hoisting calls if possible.  */
  if (is_gimple_call (stmt))
    {
      tree fndecl;

      /* Unless the call is a builtin_constant_p; this always folds to a
	 constant, so moving it is useless.  */
      fndecl = gimple_call_fndecl (stmt);
      if (fndecl
	  && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
	  && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CONSTANT_P)
	return 0;

      return LIM_EXPENSIVE;
    }

  /* Hoisting memory references out should almost surely be a win.  */
  if (gimple_references_memory_p (stmt))
    return LIM_EXPENSIVE;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return 1;

  switch (gimple_assign_rhs_code (stmt))
    {
    case MULT_EXPR:
    case WIDEN_MULT_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case DOT_PROD_EXPR:
    case FMA_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
    case RDIV_EXPR:
      /* Division and multiplication are usually expensive.  */
      return LIM_EXPENSIVE;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case WIDEN_LSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* Shifts and rotates are usually expensive.  */
      return LIM_EXPENSIVE;

    case CONSTRUCTOR:
      /* Make vector construction cost proportional to the number
         of elements.  */
      return CONSTRUCTOR_NELTS (gimple_assign_rhs1 (stmt));

    case SSA_NAME:
    case PAREN_EXPR:
      /* Whether or not something is wrapped inside a PAREN_EXPR
         should not change move cost.  Nor should an intermediate
	 unpropagated SSA name copy.  */
      return 0;

    default:
      return 1;
    }
}

/* Finds the outermost loop between OUTER and LOOP in that the memory reference
   REF is independent.  If REF is not independent in LOOP, NULL is returned
   instead.  */

static struct loop *
outermost_indep_loop (struct loop *outer, struct loop *loop, mem_ref_p ref)
{
  struct loop *aloop;

  if (bitmap_bit_p (ref->stored, loop->num))
    return NULL;

  for (aloop = outer;
       aloop != loop;
       aloop = superloop_at_depth (loop, loop_depth (aloop) + 1))
    if (!bitmap_bit_p (ref->stored, aloop->num)
	&& ref_indep_loop_p (aloop, ref))
      return aloop;

  if (ref_indep_loop_p (loop, ref))
    return loop;
  else
    return NULL;
}

/* If there is a simple load or store to a memory reference in STMT, returns
   the location of the memory reference, and sets IS_STORE according to whether
   it is a store or load.  Otherwise, returns NULL.  */

static tree *
simple_mem_ref_in_stmt (gimple stmt, bool *is_store)
{
  tree *lhs, *rhs;

  /* Recognize SSA_NAME = MEM and MEM = (SSA_NAME | invariant) patterns.  */
  if (!gimple_assign_single_p (stmt))
    return NULL;

  lhs = gimple_assign_lhs_ptr (stmt);
  rhs = gimple_assign_rhs1_ptr (stmt);

  if (TREE_CODE (*lhs) == SSA_NAME && gimple_vuse (stmt))
    {
      *is_store = false;
      return rhs;
    }
  else if (gimple_vdef (stmt)
	   && (TREE_CODE (*rhs) == SSA_NAME || is_gimple_min_invariant (*rhs)))
    {
      *is_store = true;
      return lhs;
    }
  else
    return NULL;
}

/* Returns the memory reference contained in STMT.  */

static mem_ref_p
mem_ref_in_stmt (gimple stmt)
{
  bool store;
  tree *mem = simple_mem_ref_in_stmt (stmt, &store);
  hashval_t hash;
  mem_ref_p ref;

  if (!mem)
    return NULL;
  gcc_assert (!store);

  hash = iterative_hash_expr (*mem, 0);
  ref = (mem_ref_p) htab_find_with_hash (memory_accesses.refs, *mem, hash);

  gcc_assert (ref != NULL);
  return ref;
}

/* From a controlling predicate in DOM determine the arguments from
   the PHI node PHI that are chosen if the predicate evaluates to
   true and false and store them to *TRUE_ARG_P and *FALSE_ARG_P if
   they are non-NULL.  Returns true if the arguments can be determined,
   else return false.  */

static bool
extract_true_false_args_from_phi (basic_block dom, gimple phi,
				  tree *true_arg_p, tree *false_arg_p)
{
  basic_block bb = gimple_bb (phi);
  edge true_edge, false_edge, tem;
  tree arg0 = NULL_TREE, arg1 = NULL_TREE;

  /* We have to verify that one edge into the PHI node is dominated
     by the true edge of the predicate block and the other edge
     dominated by the false edge.  This ensures that the PHI argument
     we are going to take is completely determined by the path we
     take from the predicate block.
     We can only use BB dominance checks below if the destination of
     the true/false edges are dominated by their edge, thus only
     have a single predecessor.  */
  extract_true_false_edges_from_block (dom, &true_edge, &false_edge);
  tem = EDGE_PRED (bb, 0);
  if (tem == true_edge
      || (single_pred_p (true_edge->dest)
	  && (tem->src == true_edge->dest
	      || dominated_by_p (CDI_DOMINATORS,
				 tem->src, true_edge->dest))))
    arg0 = PHI_ARG_DEF (phi, tem->dest_idx);
  else if (tem == false_edge
	   || (single_pred_p (false_edge->dest)
	       && (tem->src == false_edge->dest
		   || dominated_by_p (CDI_DOMINATORS,
				      tem->src, false_edge->dest))))
    arg1 = PHI_ARG_DEF (phi, tem->dest_idx);
  else
    return false;
  tem = EDGE_PRED (bb, 1);
  if (tem == true_edge
      || (single_pred_p (true_edge->dest)
	  && (tem->src == true_edge->dest
	      || dominated_by_p (CDI_DOMINATORS,
				 tem->src, true_edge->dest))))
    arg0 = PHI_ARG_DEF (phi, tem->dest_idx);
  else if (tem == false_edge
	   || (single_pred_p (false_edge->dest)
	       && (tem->src == false_edge->dest
		   || dominated_by_p (CDI_DOMINATORS,
				      tem->src, false_edge->dest))))
    arg1 = PHI_ARG_DEF (phi, tem->dest_idx);
  else
    return false;
  if (!arg0 || !arg1)
    return false;

  if (true_arg_p)
    *true_arg_p = arg0;
  if (false_arg_p)
    *false_arg_p = arg1;

  return true;
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
determine_max_movement (gimple stmt, bool must_preserve_exec)
{
  basic_block bb = gimple_bb (stmt);
  struct loop *loop = bb->loop_father;
  struct loop *level;
  struct lim_aux_data *lim_data = get_lim_data (stmt);
  tree val;
  ssa_op_iter iter;

  if (must_preserve_exec)
    level = ALWAYS_EXECUTED_IN (bb);
  else
    level = superloop_at_depth (loop, 1);
  lim_data->max_loop = level;

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      use_operand_p use_p;
      unsigned min_cost = UINT_MAX;
      unsigned total_cost = 0;
      struct lim_aux_data *def_data;

      /* We will end up promoting dependencies to be unconditionally
	 evaluated.  For this reason the PHI cost (and thus the
	 cost we remove from the loop by doing the invariant motion)
	 is that of the cheapest PHI argument dependency chain.  */
      FOR_EACH_PHI_ARG (use_p, stmt, iter, SSA_OP_USE)
	{
	  val = USE_FROM_PTR (use_p);
	  if (TREE_CODE (val) != SSA_NAME)
	    continue;
	  if (!add_dependency (val, lim_data, loop, false))
	    return false;
	  def_data = get_lim_data (SSA_NAME_DEF_STMT (val));
	  if (def_data)
	    {
	      min_cost = MIN (min_cost, def_data->cost);
	      total_cost += def_data->cost;
	    }
	}

      lim_data->cost += min_cost;

      if (gimple_phi_num_args (stmt) > 1)
	{
	  basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb);
	  gimple cond;
	  if (gsi_end_p (gsi_last_bb (dom)))
	    return false;
	  cond = gsi_stmt (gsi_last_bb (dom));
	  if (gimple_code (cond) != GIMPLE_COND)
	    return false;
	  /* Verify that this is an extended form of a diamond and
	     the PHI arguments are completely controlled by the
	     predicate in DOM.  */
	  if (!extract_true_false_args_from_phi (dom, stmt, NULL, NULL))
	    return false;

	  /* Fold in dependencies and cost of the condition.  */
	  FOR_EACH_SSA_TREE_OPERAND (val, cond, iter, SSA_OP_USE)
	    {
	      if (!add_dependency (val, lim_data, loop, false))
		return false;
	      def_data = get_lim_data (SSA_NAME_DEF_STMT (val));
	      if (def_data)
		total_cost += def_data->cost;
	    }

	  /* We want to avoid unconditionally executing very expensive
	     operations.  As costs for our dependencies cannot be
	     negative just claim we are not invariand for this case.
	     We also are not sure whether the control-flow inside the
	     loop will vanish.  */
	  if (total_cost - min_cost >= 2 * LIM_EXPENSIVE
	      && !(min_cost != 0
		   && total_cost / min_cost <= 2))
	    return false;

	  /* Assume that the control-flow in the loop will vanish.
	     ???  We should verify this and not artificially increase
	     the cost if that is not the case.  */
	  lim_data->cost += stmt_cost (stmt);
	}

      return true;
    }
  else
    FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_USE)
      if (!add_dependency (val, lim_data, loop, true))
	return false;

  if (gimple_vuse (stmt))
    {
      mem_ref_p ref = mem_ref_in_stmt (stmt);

      if (ref)
	{
	  lim_data->max_loop
		  = outermost_indep_loop (lim_data->max_loop, loop, ref);
	  if (!lim_data->max_loop)
	    return false;
	}
      else
	{
	  if ((val = gimple_vuse (stmt)) != NULL_TREE)
	    {
	      if (!add_dependency (val, lim_data, loop, false))
		return false;
	    }
	}
    }

  lim_data->cost += stmt_cost (stmt);

  return true;
}

/* Suppose that some statement in ORIG_LOOP is hoisted to the loop LEVEL,
   and that one of the operands of this statement is computed by STMT.
   Ensure that STMT (together with all the statements that define its
   operands) is hoisted at least out of the loop LEVEL.  */

static void
set_level (gimple stmt, struct loop *orig_loop, struct loop *level)
{
  struct loop *stmt_loop = gimple_bb (stmt)->loop_father;
  struct depend *dep;
  struct lim_aux_data *lim_data;

  stmt_loop = find_common_loop (orig_loop, stmt_loop);
  lim_data = get_lim_data (stmt);
  if (lim_data != NULL && lim_data->tgt_loop != NULL)
    stmt_loop = find_common_loop (stmt_loop,
				  loop_outer (lim_data->tgt_loop));
  if (flow_loop_nested_p (stmt_loop, level))
    return;

  gcc_assert (level == lim_data->max_loop
	      || flow_loop_nested_p (lim_data->max_loop, level));

  lim_data->tgt_loop = level;
  for (dep = lim_data->depends; dep; dep = dep->next)
    set_level (dep->stmt, orig_loop, level);
}

/* Determines an outermost loop from that we want to hoist the statement STMT.
   For now we chose the outermost possible loop.  TODO -- use profiling
   information to set it more sanely.  */

static void
set_profitable_level (gimple stmt)
{
  set_level (stmt, gimple_bb (stmt)->loop_father, get_lim_data (stmt)->max_loop);
}

/* Returns true if STMT is a call that has side effects.  */

static bool
nonpure_call_p (gimple stmt)
{
  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  return gimple_has_side_effects (stmt);
}

/* Rewrite a/b to a*(1/b).  Return the invariant stmt to process.  */

static gimple
rewrite_reciprocal (gimple_stmt_iterator *bsi)
{
  gimple stmt, stmt1, stmt2;
  tree name, lhs, type;
  tree real_one;
  gimple_stmt_iterator gsi;

  stmt = gsi_stmt (*bsi);
  lhs = gimple_assign_lhs (stmt);
  type = TREE_TYPE (lhs);

  real_one = build_one_cst (type);

  name = make_temp_ssa_name (type, NULL, "reciptmp");
  stmt1 = gimple_build_assign_with_ops (RDIV_EXPR, name, real_one,
					gimple_assign_rhs2 (stmt));

  stmt2 = gimple_build_assign_with_ops (MULT_EXPR, lhs, name,
					gimple_assign_rhs1 (stmt));

  /* Replace division stmt with reciprocal and multiply stmts.
     The multiply stmt is not invariant, so update iterator
     and avoid rescanning.  */
  gsi = *bsi;
  gsi_insert_before (bsi, stmt1, GSI_NEW_STMT);
  gsi_replace (&gsi, stmt2, true);

  /* Continue processing with invariant reciprocal statement.  */
  return stmt1;
}

/* Check if the pattern at *BSI is a bittest of the form
   (A >> B) & 1 != 0 and in this case rewrite it to A & (1 << B) != 0.  */

static gimple
rewrite_bittest (gimple_stmt_iterator *bsi)
{
  gimple stmt, use_stmt, stmt1, stmt2;
  tree lhs, name, t, a, b;
  use_operand_p use;

  stmt = gsi_stmt (*bsi);
  lhs = gimple_assign_lhs (stmt);

  /* Verify that the single use of lhs is a comparison against zero.  */
  if (TREE_CODE (lhs) != SSA_NAME
      || !single_imm_use (lhs, &use, &use_stmt)
      || gimple_code (use_stmt) != GIMPLE_COND)
    return stmt;
  if (gimple_cond_lhs (use_stmt) != lhs
      || (gimple_cond_code (use_stmt) != NE_EXPR
	  && gimple_cond_code (use_stmt) != EQ_EXPR)
      || !integer_zerop (gimple_cond_rhs (use_stmt)))
    return stmt;

  /* Get at the operands of the shift.  The rhs is TMP1 & 1.  */
  stmt1 = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
  if (gimple_code (stmt1) != GIMPLE_ASSIGN)
    return stmt;

  /* There is a conversion in between possibly inserted by fold.  */
  if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt1)))
    {
      t = gimple_assign_rhs1 (stmt1);
      if (TREE_CODE (t) != SSA_NAME
	  || !has_single_use (t))
	return stmt;
      stmt1 = SSA_NAME_DEF_STMT (t);
      if (gimple_code (stmt1) != GIMPLE_ASSIGN)
	return stmt;
    }

  /* Verify that B is loop invariant but A is not.  Verify that with
     all the stmt walking we are still in the same loop.  */
  if (gimple_assign_rhs_code (stmt1) != RSHIFT_EXPR
      || loop_containing_stmt (stmt1) != loop_containing_stmt (stmt))
    return stmt;

  a = gimple_assign_rhs1 (stmt1);
  b = gimple_assign_rhs2 (stmt1);

  if (outermost_invariant_loop (b, loop_containing_stmt (stmt1)) != NULL
      && outermost_invariant_loop (a, loop_containing_stmt (stmt1)) == NULL)
    {
      gimple_stmt_iterator rsi;

      /* 1 << B */
      t = fold_build2 (LSHIFT_EXPR, TREE_TYPE (a),
		       build_int_cst (TREE_TYPE (a), 1), b);
      name = make_temp_ssa_name (TREE_TYPE (a), NULL, "shifttmp");
      stmt1 = gimple_build_assign (name, t);

      /* A & (1 << B) */
      t = fold_build2 (BIT_AND_EXPR, TREE_TYPE (a), a, name);
      name = make_temp_ssa_name (TREE_TYPE (a), NULL, "shifttmp");
      stmt2 = gimple_build_assign (name, t);

      /* Replace the SSA_NAME we compare against zero.  Adjust
	 the type of zero accordingly.  */
      SET_USE (use, name);
      gimple_cond_set_rhs (use_stmt, build_int_cst_type (TREE_TYPE (name), 0));

      /* Don't use gsi_replace here, none of the new assignments sets
	 the variable originally set in stmt.  Move bsi to stmt1, and
	 then remove the original stmt, so that we get a chance to
	 retain debug info for it.  */
      rsi = *bsi;
      gsi_insert_before (bsi, stmt1, GSI_NEW_STMT);
      gsi_insert_before (&rsi, stmt2, GSI_SAME_STMT);
      gsi_remove (&rsi, true);

      return stmt1;
    }

  return stmt;
}


/* Determine the outermost loops in that statements in basic block BB are
   invariant, and record them to the LIM_DATA associated with the statements.
   Callback for walk_dominator_tree.  */

static void
determine_invariantness_stmt (struct dom_walk_data *dw_data ATTRIBUTE_UNUSED,
			      basic_block bb)
{
  enum move_pos pos;
  gimple_stmt_iterator bsi;
  gimple stmt;
  bool maybe_never = ALWAYS_EXECUTED_IN (bb) == NULL;
  struct loop *outermost = ALWAYS_EXECUTED_IN (bb);
  struct lim_aux_data *lim_data;

  if (!loop_outer (bb->loop_father))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Basic block %d (loop %d -- depth %d):\n\n",
	     bb->index, bb->loop_father->num, loop_depth (bb->loop_father));

  /* Look at PHI nodes, but only if there is at most two.
     ???  We could relax this further by post-processing the inserted
     code and transforming adjacent cond-exprs with the same predicate
     to control flow again.  */
  bsi = gsi_start_phis (bb);
  if (!gsi_end_p (bsi)
      && ((gsi_next (&bsi), gsi_end_p (bsi))
	  || (gsi_next (&bsi), gsi_end_p (bsi))))
    for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
      {
	stmt = gsi_stmt (bsi);

	pos = movement_possibility (stmt);
	if (pos == MOVE_IMPOSSIBLE)
	  continue;

	lim_data = init_lim_data (stmt);
	lim_data->always_executed_in = outermost;

	if (!determine_max_movement (stmt, false))
	  {
	    lim_data->max_loop = NULL;
	    continue;
	  }

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    print_gimple_stmt (dump_file, stmt, 2, 0);
	    fprintf (dump_file, "  invariant up to level %d, cost %d.\n\n",
		     loop_depth (lim_data->max_loop),
		     lim_data->cost);
	  }

	if (lim_data->cost >= LIM_EXPENSIVE)
	  set_profitable_level (stmt);
      }

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      stmt = gsi_stmt (bsi);

      pos = movement_possibility (stmt);
      if (pos == MOVE_IMPOSSIBLE)
	{
	  if (nonpure_call_p (stmt))
	    {
	      maybe_never = true;
	      outermost = NULL;
	    }
	  /* Make sure to note always_executed_in for stores to make
	     store-motion work.  */
	  else if (stmt_makes_single_store (stmt))
	    {
	      struct lim_aux_data *lim_data = init_lim_data (stmt);
	      lim_data->always_executed_in = outermost;
	    }
	  continue;
	}

      if (is_gimple_assign (stmt)
	  && (get_gimple_rhs_class (gimple_assign_rhs_code (stmt))
	      == GIMPLE_BINARY_RHS))
	{
	  tree op0 = gimple_assign_rhs1 (stmt);
	  tree op1 = gimple_assign_rhs2 (stmt);
	  struct loop *ol1 = outermost_invariant_loop (op1,
					loop_containing_stmt (stmt));

	  /* If divisor is invariant, convert a/b to a*(1/b), allowing reciprocal
	     to be hoisted out of loop, saving expensive divide.  */
	  if (pos == MOVE_POSSIBLE
	      && gimple_assign_rhs_code (stmt) == RDIV_EXPR
	      && flag_unsafe_math_optimizations
	      && !flag_trapping_math
	      && ol1 != NULL
	      && outermost_invariant_loop (op0, ol1) == NULL)
	    stmt = rewrite_reciprocal (&bsi);

	  /* If the shift count is invariant, convert (A >> B) & 1 to
	     A & (1 << B) allowing the bit mask to be hoisted out of the loop
	     saving an expensive shift.  */
	  if (pos == MOVE_POSSIBLE
	      && gimple_assign_rhs_code (stmt) == BIT_AND_EXPR
	      && integer_onep (op1)
	      && TREE_CODE (op0) == SSA_NAME
	      && has_single_use (op0))
	    stmt = rewrite_bittest (&bsi);
	}

      lim_data = init_lim_data (stmt);
      lim_data->always_executed_in = outermost;

      if (maybe_never && pos == MOVE_PRESERVE_EXECUTION)
	continue;

      if (!determine_max_movement (stmt, pos == MOVE_PRESERVE_EXECUTION))
	{
	  lim_data->max_loop = NULL;
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  print_gimple_stmt (dump_file, stmt, 2, 0);
	  fprintf (dump_file, "  invariant up to level %d, cost %d.\n\n",
		   loop_depth (lim_data->max_loop),
		   lim_data->cost);
	}

      if (lim_data->cost >= LIM_EXPENSIVE)
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
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.before_dom_children = determine_invariantness_stmt;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);
}

/* Hoist the statements in basic block BB out of the loops prescribed by
   data stored in LIM_DATA structures associated with each statement.  Callback
   for walk_dominator_tree.  */

static void
move_computations_stmt (struct dom_walk_data *dw_data,
			basic_block bb)
{
  struct loop *level;
  gimple_stmt_iterator bsi;
  gimple stmt;
  unsigned cost = 0;
  struct lim_aux_data *lim_data;

  if (!loop_outer (bb->loop_father))
    return;

  for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); )
    {
      gimple new_stmt;
      stmt = gsi_stmt (bsi);

      lim_data = get_lim_data (stmt);
      if (lim_data == NULL)
	{
	  gsi_next (&bsi);
	  continue;
	}

      cost = lim_data->cost;
      level = lim_data->tgt_loop;
      clear_lim_data (stmt);

      if (!level)
	{
	  gsi_next (&bsi);
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Moving PHI node\n");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	  fprintf (dump_file, "(cost %u) out of loop %d.\n\n",
		   cost, level->num);
	}

      if (gimple_phi_num_args (stmt) == 1)
	{
	  tree arg = PHI_ARG_DEF (stmt, 0);
	  new_stmt = gimple_build_assign_with_ops (TREE_CODE (arg),
						   gimple_phi_result (stmt),
						   arg, NULL_TREE);
	  SSA_NAME_DEF_STMT (gimple_phi_result (stmt)) = new_stmt;
	}
      else
	{
	  basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb);
	  gimple cond = gsi_stmt (gsi_last_bb (dom));
	  tree arg0 = NULL_TREE, arg1 = NULL_TREE, t;
	  /* Get the PHI arguments corresponding to the true and false
	     edges of COND.  */
	  extract_true_false_args_from_phi (dom, stmt, &arg0, &arg1);
	  gcc_assert (arg0 && arg1);
	  t = build2 (gimple_cond_code (cond), boolean_type_node,
		      gimple_cond_lhs (cond), gimple_cond_rhs (cond));
	  new_stmt = gimple_build_assign_with_ops (COND_EXPR,
						   gimple_phi_result (stmt),
						   t, arg0, arg1);
	  SSA_NAME_DEF_STMT (gimple_phi_result (stmt)) = new_stmt;
	  *((unsigned int *)(dw_data->global_data)) |= TODO_cleanup_cfg;
	}
      gsi_insert_on_edge (loop_preheader_edge (level), new_stmt);
      remove_phi_node (&bsi, false);
    }

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); )
    {
      edge e;

      stmt = gsi_stmt (bsi);

      lim_data = get_lim_data (stmt);
      if (lim_data == NULL)
	{
	  gsi_next (&bsi);
	  continue;
	}

      cost = lim_data->cost;
      level = lim_data->tgt_loop;
      clear_lim_data (stmt);

      if (!level)
	{
	  gsi_next (&bsi);
	  continue;
	}

      /* We do not really want to move conditionals out of the loop; we just
	 placed it here to force its operands to be moved if necessary.  */
      if (gimple_code (stmt) == GIMPLE_COND)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Moving statement\n");
	  print_gimple_stmt (dump_file, stmt, 0, 0);
	  fprintf (dump_file, "(cost %u) out of loop %d.\n\n",
		   cost, level->num);
	}

      e = loop_preheader_edge (level);
      gcc_assert (!gimple_vdef (stmt));
      if (gimple_vuse (stmt))
	{
	  /* The new VUSE is the one from the virtual PHI in the loop
	     header or the one already present.  */
	  gimple_stmt_iterator gsi2;
	  for (gsi2 = gsi_start_phis (e->dest);
	       !gsi_end_p (gsi2); gsi_next (&gsi2))
	    {
	      gimple phi = gsi_stmt (gsi2);
	      if (virtual_operand_p (gimple_phi_result (phi)))
		{
		  gimple_set_vuse (stmt, PHI_ARG_DEF_FROM_EDGE (phi, e));
		  break;
		}
	    }
	}
      gsi_remove (&bsi, false);
      gsi_insert_on_edge (e, stmt);
    }
}

/* Hoist the statements out of the loops prescribed by data stored in
   LIM_DATA structures associated with each statement.*/

static unsigned int
move_computations (void)
{
  struct dom_walk_data walk_data;
  unsigned int todo = 0;

  memset (&walk_data, 0, sizeof (struct dom_walk_data));
  walk_data.global_data = &todo;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.before_dom_children = move_computations_stmt;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);

  gsi_commit_edge_inserts ();
  if (need_ssa_update_p (cfun))
    rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);

  return todo;
}

/* Checks whether the statement defining variable *INDEX can be hoisted
   out of the loop passed in DATA.  Callback for for_each_index.  */

static bool
may_move_till (tree ref, tree *index, void *data)
{
  struct loop *loop = (struct loop *) data, *max_loop;

  /* If REF is an array reference, check also that the step and the lower
     bound is invariant in LOOP.  */
  if (TREE_CODE (ref) == ARRAY_REF)
    {
      tree step = TREE_OPERAND (ref, 3);
      tree lbound = TREE_OPERAND (ref, 2);

      max_loop = outermost_invariant_loop (step, loop);
      if (!max_loop)
	return false;

      max_loop = outermost_invariant_loop (lbound, loop);
      if (!max_loop)
	return false;
    }

  max_loop = outermost_invariant_loop (*index, loop);
  if (!max_loop)
    return false;

  return true;
}

/* If OP is SSA NAME, force the statement that defines it to be
   moved out of the LOOP.  ORIG_LOOP is the loop in that EXPR is used.  */

static void
force_move_till_op (tree op, struct loop *orig_loop, struct loop *loop)
{
  gimple stmt;

  if (!op
      || is_gimple_min_invariant (op))
    return;

  gcc_assert (TREE_CODE (op) == SSA_NAME);

  stmt = SSA_NAME_DEF_STMT (op);
  if (gimple_nop_p (stmt))
    return;

  set_level (stmt, orig_loop, loop);
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
  struct fmt_data *fmt_data = (struct fmt_data *) data;

  if (TREE_CODE (ref) == ARRAY_REF)
    {
      tree step = TREE_OPERAND (ref, 3);
      tree lbound = TREE_OPERAND (ref, 2);

      force_move_till_op (step, fmt_data->orig_loop, fmt_data->loop);
      force_move_till_op (lbound, fmt_data->orig_loop, fmt_data->loop);
    }

  force_move_till_op (*index, fmt_data->orig_loop, fmt_data->loop);

  return true;
}

/* A hash function for struct mem_ref object OBJ.  */

static hashval_t
memref_hash (const void *obj)
{
  const struct mem_ref *const mem = (const struct mem_ref *) obj;

  return mem->hash;
}

/* An equality function for struct mem_ref object OBJ1 with
   memory reference OBJ2.  */

static int
memref_eq (const void *obj1, const void *obj2)
{
  const struct mem_ref *const mem1 = (const struct mem_ref *) obj1;

  return operand_equal_p (mem1->mem, (const_tree) obj2, 0);
}

/* Releases list of memory reference locations ACCS.  */

static void
free_mem_ref_locs (mem_ref_locs_p accs)
{
  unsigned i;
  mem_ref_loc_p loc;

  if (!accs)
    return;

  FOR_EACH_VEC_ELT (accs->locs, i, loc)
    free (loc);
  accs->locs.release ();
  free (accs);
}

/* A function to free the mem_ref object OBJ.  */

static void
memref_free (struct mem_ref *mem)
{
  unsigned i;
  mem_ref_locs_p accs;

  FOR_EACH_VEC_ELT (mem->accesses_in_loop, i, accs)
    free_mem_ref_locs (accs);
  mem->accesses_in_loop.release ();

  free (mem);
}

/* Allocates and returns a memory reference description for MEM whose hash
   value is HASH and id is ID.  */

static mem_ref_p
mem_ref_alloc (tree mem, unsigned hash, unsigned id)
{
  mem_ref_p ref = XNEW (struct mem_ref);
  ref->mem = mem;
  ref->id = id;
  ref->hash = hash;
  ref->stored = BITMAP_ALLOC (&lim_bitmap_obstack);
  ref->indep_loop = BITMAP_ALLOC (&lim_bitmap_obstack);
  ref->dep_loop = BITMAP_ALLOC (&lim_bitmap_obstack);
  ref->indep_ref = BITMAP_ALLOC (&lim_bitmap_obstack);
  ref->dep_ref = BITMAP_ALLOC (&lim_bitmap_obstack);
  ref->accesses_in_loop.create (0);

  return ref;
}

/* Allocates and returns the new list of locations.  */

static mem_ref_locs_p
mem_ref_locs_alloc (void)
{
  mem_ref_locs_p accs = XNEW (struct mem_ref_locs);
  accs->locs.create (0);
  return accs;
}

/* Records memory reference location *LOC in LOOP to the memory reference
   description REF.  The reference occurs in statement STMT.  */

static void
record_mem_ref_loc (mem_ref_p ref, struct loop *loop, gimple stmt, tree *loc)
{
  mem_ref_loc_p aref = XNEW (struct mem_ref_loc);
  mem_ref_locs_p accs;
  bitmap ril = memory_accesses.refs_in_loop[loop->num];

  if (ref->accesses_in_loop.length ()
      <= (unsigned) loop->num)
    ref->accesses_in_loop.safe_grow_cleared (loop->num + 1);
  accs = ref->accesses_in_loop[loop->num];
  if (!accs)
    {
      accs = mem_ref_locs_alloc ();
      ref->accesses_in_loop[loop->num] = accs;
    }

  aref->stmt = stmt;
  aref->ref = loc;

  accs->locs.safe_push (aref);
  bitmap_set_bit (ril, ref->id);
}

/* Marks reference REF as stored in LOOP.  */

static void
mark_ref_stored (mem_ref_p ref, struct loop *loop)
{
  for (;
       loop != current_loops->tree_root
       && !bitmap_bit_p (ref->stored, loop->num);
       loop = loop_outer (loop))
    bitmap_set_bit (ref->stored, loop->num);
}

/* Gathers memory references in statement STMT in LOOP, storing the
   information about them in the memory_accesses structure.  Marks
   the vops accessed through unrecognized statements there as
   well.  */

static void
gather_mem_refs_stmt (struct loop *loop, gimple stmt)
{
  tree *mem = NULL;
  hashval_t hash;
  PTR *slot;
  mem_ref_p ref;
  bool is_stored;
  unsigned id;

  if (!gimple_vuse (stmt))
    return;

  mem = simple_mem_ref_in_stmt (stmt, &is_stored);
  if (!mem)
    {
      id = memory_accesses.refs_list.length ();
      ref = mem_ref_alloc (error_mark_node, 0, id);
      memory_accesses.refs_list.safe_push (ref);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Unanalyzed memory reference %u: ", id);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      if (gimple_vdef (stmt))
	mark_ref_stored (ref, loop);
      record_mem_ref_loc (ref, loop, stmt, mem);
      return;
    }

  hash = iterative_hash_expr (*mem, 0);
  slot = htab_find_slot_with_hash (memory_accesses.refs, *mem, hash, INSERT);

  if (*slot)
    {
      ref = (mem_ref_p) *slot;
      id = ref->id;
    }
  else
    {
      id = memory_accesses.refs_list.length ();
      ref = mem_ref_alloc (*mem, hash, id);
      memory_accesses.refs_list.safe_push (ref);
      *slot = ref;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Memory reference %u: ", id);
	  print_generic_expr (dump_file, ref->mem, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
    }

  if (is_stored)
    mark_ref_stored (ref, loop);

  record_mem_ref_loc (ref, loop, stmt, mem);
  return;
}

/* Gathers memory references in loops.  */

static void
gather_mem_refs_in_loops (void)
{
  gimple_stmt_iterator bsi;
  basic_block bb;
  struct loop *loop;
  loop_iterator li;
  bitmap lrefs, alrefs, alrefso;

  FOR_EACH_BB (bb)
    {
      loop = bb->loop_father;
      if (loop == current_loops->tree_root)
	continue;

      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	gather_mem_refs_stmt (loop, gsi_stmt (bsi));
    }

  /* Propagate the information about accessed memory references up
     the loop hierarchy.  */
  FOR_EACH_LOOP (li, loop, LI_FROM_INNERMOST)
    {
      lrefs = memory_accesses.refs_in_loop[loop->num];
      alrefs = memory_accesses.all_refs_in_loop[loop->num];
      bitmap_ior_into (alrefs, lrefs);

      if (loop_outer (loop) == current_loops->tree_root)
	continue;

      alrefso = memory_accesses.all_refs_in_loop[loop_outer (loop)->num];
      bitmap_ior_into (alrefso, alrefs);
    }
}

/* Create a mapping from virtual operands to references that touch them
   in LOOP.  */

static void
create_vop_ref_mapping_loop (struct loop *loop)
{
  bitmap refs = memory_accesses.refs_in_loop[loop->num];
  struct loop *sloop;
  bitmap_iterator bi;
  unsigned i;
  mem_ref_p ref;

  EXECUTE_IF_SET_IN_BITMAP (refs, 0, i, bi)
    {
      ref = memory_accesses.refs_list[i];
      for (sloop = loop; sloop != current_loops->tree_root;
	   sloop = loop_outer (sloop))
	if (bitmap_bit_p (ref->stored, loop->num))
	  {
	    bitmap refs_stored
	      = memory_accesses.all_refs_stored_in_loop[sloop->num];
	    bitmap_set_bit (refs_stored, ref->id);
	  }
    }
}

/* For each non-clobbered virtual operand and each loop, record the memory
   references in this loop that touch the operand.  */

static void
create_vop_ref_mapping (void)
{
  loop_iterator li;
  struct loop *loop;

  FOR_EACH_LOOP (li, loop, 0)
    {
      create_vop_ref_mapping_loop (loop);
    }
}

/* Gathers information about memory accesses in the loops.  */

static void
analyze_memory_references (void)
{
  unsigned i;
  bitmap empty;

  memory_accesses.refs = htab_create (100, memref_hash, memref_eq, NULL);
  memory_accesses.refs_list.create (0);
  memory_accesses.refs_in_loop.create (number_of_loops ());
  memory_accesses.all_refs_in_loop.create (number_of_loops ());
  memory_accesses.all_refs_stored_in_loop.create (number_of_loops ());

  for (i = 0; i < number_of_loops (); i++)
    {
      empty = BITMAP_ALLOC (&lim_bitmap_obstack);
      memory_accesses.refs_in_loop.quick_push (empty);
      empty = BITMAP_ALLOC (&lim_bitmap_obstack);
      memory_accesses.all_refs_in_loop.quick_push (empty);
      empty = BITMAP_ALLOC (&lim_bitmap_obstack);
      memory_accesses.all_refs_stored_in_loop.quick_push (empty);
    }

  memory_accesses.ttae_cache = NULL;

  gather_mem_refs_in_loops ();
  create_vop_ref_mapping ();
}

/* Returns true if MEM1 and MEM2 may alias.  TTAE_CACHE is used as a cache in
   tree_to_aff_combination_expand.  */

static bool
mem_refs_may_alias_p (tree mem1, tree mem2, struct pointer_map_t **ttae_cache)
{
  /* Perform BASE + OFFSET analysis -- if MEM1 and MEM2 are based on the same
     object and their offset differ in such a way that the locations cannot
     overlap, then they cannot alias.  */
  double_int size1, size2;
  aff_tree off1, off2;

  /* Perform basic offset and type-based disambiguation.  */
  if (!refs_may_alias_p (mem1, mem2))
    return false;

  /* The expansion of addresses may be a bit expensive, thus we only do
     the check at -O2 and higher optimization levels.  */
  if (optimize < 2)
    return true;

  get_inner_reference_aff (mem1, &off1, &size1);
  get_inner_reference_aff (mem2, &off2, &size2);
  aff_combination_expand (&off1, ttae_cache);
  aff_combination_expand (&off2, ttae_cache);
  aff_combination_scale (&off1, double_int_minus_one);
  aff_combination_add (&off2, &off1);

  if (aff_comb_cannot_overlap_p (&off2, size1, size2))
    return false;

  return true;
}

/* Rewrites location LOC by TMP_VAR.  */

static void
rewrite_mem_ref_loc (mem_ref_loc_p loc, tree tmp_var)
{
  *loc->ref = tmp_var;
  update_stmt (loc->stmt);
}

/* Adds all locations of REF in LOOP and its subloops to LOCS.  */

static void
get_all_locs_in_loop (struct loop *loop, mem_ref_p ref,
		      vec<mem_ref_loc_p> *locs)
{
  mem_ref_locs_p accs;
  unsigned i;
  mem_ref_loc_p loc;
  bitmap refs = memory_accesses.all_refs_in_loop[loop->num];
  struct loop *subloop;

  if (!bitmap_bit_p (refs, ref->id))
    return;

  if (ref->accesses_in_loop.length ()
      > (unsigned) loop->num)
    {
      accs = ref->accesses_in_loop[loop->num];
      if (accs)
	{
	  FOR_EACH_VEC_ELT (accs->locs, i, loc)
	    locs->safe_push (loc);
	}
    }

  for (subloop = loop->inner; subloop != NULL; subloop = subloop->next)
    get_all_locs_in_loop (subloop, ref, locs);
}

/* Rewrites all references to REF in LOOP by variable TMP_VAR.  */

static void
rewrite_mem_refs (struct loop *loop, mem_ref_p ref, tree tmp_var)
{
  unsigned i;
  mem_ref_loc_p loc;
  vec<mem_ref_loc_p> locs = vNULL;

  get_all_locs_in_loop (loop, ref, &locs);
  FOR_EACH_VEC_ELT (locs, i, loc)
    rewrite_mem_ref_loc (loc, tmp_var);
  locs.release ();
}

/* The name and the length of the currently generated variable
   for lsm.  */
#define MAX_LSM_NAME_LENGTH 40
static char lsm_tmp_name[MAX_LSM_NAME_LENGTH + 1];
static int lsm_tmp_name_length;

/* Adds S to lsm_tmp_name.  */

static void
lsm_tmp_name_add (const char *s)
{
  int l = strlen (s) + lsm_tmp_name_length;
  if (l > MAX_LSM_NAME_LENGTH)
    return;

  strcpy (lsm_tmp_name + lsm_tmp_name_length, s);
  lsm_tmp_name_length = l;
}

/* Stores the name for temporary variable that replaces REF to
   lsm_tmp_name.  */

static void
gen_lsm_tmp_name (tree ref)
{
  const char *name;

  switch (TREE_CODE (ref))
    {
    case MEM_REF:
    case TARGET_MEM_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_");
      break;

    case ADDR_EXPR:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      break;

    case BIT_FIELD_REF:
    case VIEW_CONVERT_EXPR:
    case ARRAY_RANGE_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      break;

    case REALPART_EXPR:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_RE");
      break;

    case IMAGPART_EXPR:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_IM");
      break;

    case COMPONENT_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_");
      name = get_name (TREE_OPERAND (ref, 1));
      if (!name)
	name = "F";
      lsm_tmp_name_add (name);
      break;

    case ARRAY_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_I");
      break;

    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
      name = get_name (ref);
      if (!name)
	name = "D";
      lsm_tmp_name_add (name);
      break;

    case STRING_CST:
      lsm_tmp_name_add ("S");
      break;

    case RESULT_DECL:
      lsm_tmp_name_add ("R");
      break;

    case INTEGER_CST:
      /* Nothing.  */
      break;

    default:
      gcc_unreachable ();
    }
}

/* Determines name for temporary variable that replaces REF.
   The name is accumulated into the lsm_tmp_name variable.
   N is added to the name of the temporary.  */

char *
get_lsm_tmp_name (tree ref, unsigned n)
{
  char ns[2];

  lsm_tmp_name_length = 0;
  gen_lsm_tmp_name (ref);
  lsm_tmp_name_add ("_lsm");
  if (n < 10)
    {
      ns[0] = '0' + n;
      ns[1] = 0;
      lsm_tmp_name_add (ns);
    }
  return lsm_tmp_name;
}

struct prev_flag_edges {
  /* Edge to insert new flag comparison code.  */
  edge append_cond_position;

  /* Edge for fall through from previous flag comparison.  */
  edge last_cond_fallthru;
};

/* Helper function for execute_sm.  Emit code to store TMP_VAR into
   MEM along edge EX.

   The store is only done if MEM has changed.  We do this so no
   changes to MEM occur on code paths that did not originally store
   into it.

   The common case for execute_sm will transform:

     for (...) {
       if (foo)
         stuff;
       else
         MEM = TMP_VAR;
     }

   into:

     lsm = MEM;
     for (...) {
       if (foo)
         stuff;
       else
         lsm = TMP_VAR;
     }
     MEM = lsm;

  This function will generate:

     lsm = MEM;

     lsm_flag = false;
     ...
     for (...) {
       if (foo)
         stuff;
       else {
         lsm = TMP_VAR;
         lsm_flag = true;
       }
     }
     if (lsm_flag)	<--
       MEM = lsm;	<--
*/

static void
execute_sm_if_changed (edge ex, tree mem, tree tmp_var, tree flag)
{
  basic_block new_bb, then_bb, old_dest;
  bool loop_has_only_one_exit;
  edge then_old_edge, orig_ex = ex;
  gimple_stmt_iterator gsi;
  gimple stmt;
  struct prev_flag_edges *prev_edges = (struct prev_flag_edges *) ex->aux;

  /* ?? Insert store after previous store if applicable.  See note
     below.  */
  if (prev_edges)
    ex = prev_edges->append_cond_position;

  loop_has_only_one_exit = single_pred_p (ex->dest);

  if (loop_has_only_one_exit)
    ex = split_block_after_labels (ex->dest);

  old_dest = ex->dest;
  new_bb = split_edge (ex);
  then_bb = create_empty_bb (new_bb);
  if (current_loops && new_bb->loop_father)
    add_bb_to_loop (then_bb, new_bb->loop_father);

  gsi = gsi_start_bb (new_bb);
  stmt = gimple_build_cond (NE_EXPR, flag, boolean_false_node,
			    NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

  gsi = gsi_start_bb (then_bb);
  /* Insert actual store.  */
  stmt = gimple_build_assign (unshare_expr (mem), tmp_var);
  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

  make_edge (new_bb, then_bb, EDGE_TRUE_VALUE);
  make_edge (new_bb, old_dest, EDGE_FALSE_VALUE);
  then_old_edge = make_edge (then_bb, old_dest, EDGE_FALLTHRU);

  set_immediate_dominator (CDI_DOMINATORS, then_bb, new_bb);

  if (prev_edges)
    {
      basic_block prevbb = prev_edges->last_cond_fallthru->src;
      redirect_edge_succ (prev_edges->last_cond_fallthru, new_bb);
      set_immediate_dominator (CDI_DOMINATORS, new_bb, prevbb);
      set_immediate_dominator (CDI_DOMINATORS, old_dest,
			       recompute_dominator (CDI_DOMINATORS, old_dest));
    }

  /* ?? Because stores may alias, they must happen in the exact
     sequence they originally happened.  Save the position right after
     the (_lsm) store we just created so we can continue appending after
     it and maintain the original order.  */
  {
    struct prev_flag_edges *p;

    if (orig_ex->aux)
      orig_ex->aux = NULL;
    alloc_aux_for_edge (orig_ex, sizeof (struct prev_flag_edges));
    p = (struct prev_flag_edges *) orig_ex->aux;
    p->append_cond_position = then_old_edge;
    p->last_cond_fallthru = find_edge (new_bb, old_dest);
    orig_ex->aux = (void *) p;
  }

  if (!loop_has_only_one_exit)
    for (gsi = gsi_start_phis (old_dest); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple phi = gsi_stmt (gsi);
	unsigned i;

	for (i = 0; i < gimple_phi_num_args (phi); i++)
	  if (gimple_phi_arg_edge (phi, i)->src == new_bb)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      add_phi_arg (phi, arg, then_old_edge, UNKNOWN_LOCATION);
	      update_stmt (phi);
	    }
      }
  /* Remove the original fall through edge.  This was the
     single_succ_edge (new_bb).  */
  EDGE_SUCC (new_bb, 0)->flags &= ~EDGE_FALLTHRU;
}

/* Helper function for execute_sm.  On every location where REF is
   set, set an appropriate flag indicating the store.  */

static tree
execute_sm_if_changed_flag_set (struct loop *loop, mem_ref_p ref)
{
  unsigned i;
  mem_ref_loc_p loc;
  tree flag;
  vec<mem_ref_loc_p> locs = vNULL;
  char *str = get_lsm_tmp_name (ref->mem, ~0);

  lsm_tmp_name_add ("_flag");
  flag = create_tmp_reg (boolean_type_node, str);
  get_all_locs_in_loop (loop, ref, &locs);
  FOR_EACH_VEC_ELT (locs, i, loc)
    {
      gimple_stmt_iterator gsi;
      gimple stmt;

      /* Only set the flag for writes.  */
      if (is_gimple_assign (loc->stmt)
	  && gimple_assign_lhs_ptr (loc->stmt) == loc->ref)
	{
	  gsi = gsi_for_stmt (loc->stmt);
	  stmt = gimple_build_assign (flag, boolean_true_node);
	  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);
	}
    }
  locs.release ();
  return flag;
}

/* Executes store motion of memory reference REF from LOOP.
   Exits from the LOOP are stored in EXITS.  The initialization of the
   temporary variable is put to the preheader of the loop, and assignments
   to the reference from the temporary variable are emitted to exits.  */

static void
execute_sm (struct loop *loop, vec<edge> exits, mem_ref_p ref)
{
  tree tmp_var, store_flag;
  unsigned i;
  gimple load;
  struct fmt_data fmt_data;
  edge ex, latch_edge;
  struct lim_aux_data *lim_data;
  bool multi_threaded_model_p = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Executing store motion of ");
      print_generic_expr (dump_file, ref->mem, 0);
      fprintf (dump_file, " from loop %d\n", loop->num);
    }

  tmp_var = create_tmp_reg (TREE_TYPE (ref->mem),
			      get_lsm_tmp_name (ref->mem, ~0));

  fmt_data.loop = loop;
  fmt_data.orig_loop = loop;
  for_each_index (&ref->mem, force_move_till, &fmt_data);

  if (block_in_transaction (loop_preheader_edge (loop)->src)
      || !PARAM_VALUE (PARAM_ALLOW_STORE_DATA_RACES))
    multi_threaded_model_p = true;

  if (multi_threaded_model_p)
    store_flag = execute_sm_if_changed_flag_set (loop, ref);

  rewrite_mem_refs (loop, ref, tmp_var);

  /* Emit the load code into the latch, so that we are sure it will
     be processed after all dependencies.  */
  latch_edge = loop_latch_edge (loop);

  /* FIXME/TODO: For the multi-threaded variant, we could avoid this
     load altogether, since the store is predicated by a flag.  We
     could, do the load only if it was originally in the loop.  */
  load = gimple_build_assign (tmp_var, unshare_expr (ref->mem));
  lim_data = init_lim_data (load);
  lim_data->max_loop = loop;
  lim_data->tgt_loop = loop;
  gsi_insert_on_edge (latch_edge, load);

  if (multi_threaded_model_p)
    {
      load = gimple_build_assign (store_flag, boolean_false_node);
      lim_data = init_lim_data (load);
      lim_data->max_loop = loop;
      lim_data->tgt_loop = loop;
      gsi_insert_on_edge (latch_edge, load);
    }

  /* Sink the store to every exit from the loop.  */
  FOR_EACH_VEC_ELT (exits, i, ex)
    if (!multi_threaded_model_p)
      {
	gimple store;
	store = gimple_build_assign (unshare_expr (ref->mem), tmp_var);
	gsi_insert_on_edge (ex, store);
      }
    else
      execute_sm_if_changed (ex, ref->mem, tmp_var, store_flag);
}

/* Hoists memory references MEM_REFS out of LOOP.  EXITS is the list of exit
   edges of the LOOP.  */

static void
hoist_memory_references (struct loop *loop, bitmap mem_refs,
			 vec<edge> exits)
{
  mem_ref_p ref;
  unsigned  i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (mem_refs, 0, i, bi)
    {
      ref = memory_accesses.refs_list[i];
      execute_sm (loop, exits, ref);
    }
}

/* Returns true if REF is always accessed in LOOP.  If STORED_P is true
   make sure REF is always stored to in LOOP.  */

static bool
ref_always_accessed_p (struct loop *loop, mem_ref_p ref, bool stored_p)
{
  vec<mem_ref_loc_p> locs = vNULL;
  unsigned i;
  mem_ref_loc_p loc;
  bool ret = false;
  struct loop *must_exec;
  tree base;

  base = get_base_address (ref->mem);
  if (INDIRECT_REF_P (base)
      || TREE_CODE (base) == MEM_REF)
    base = TREE_OPERAND (base, 0);

  get_all_locs_in_loop (loop, ref, &locs);
  FOR_EACH_VEC_ELT (locs, i, loc)
    {
      if (!get_lim_data (loc->stmt))
	continue;

      /* If we require an always executed store make sure the statement
         stores to the reference.  */
      if (stored_p)
	{
	  tree lhs;
	  if (!gimple_get_lhs (loc->stmt))
	    continue;
	  lhs = get_base_address (gimple_get_lhs (loc->stmt));
	  if (!lhs)
	    continue;
	  if (INDIRECT_REF_P (lhs)
	      || TREE_CODE (lhs) == MEM_REF)
	    lhs = TREE_OPERAND (lhs, 0);
	  if (lhs != base)
	    continue;
	}

      must_exec = get_lim_data (loc->stmt)->always_executed_in;
      if (!must_exec)
	continue;

      if (must_exec == loop
	  || flow_loop_nested_p (must_exec, loop))
	{
	  ret = true;
	  break;
	}
    }
  locs.release ();

  return ret;
}

/* Returns true if REF1 and REF2 are independent.  */

static bool
refs_independent_p (mem_ref_p ref1, mem_ref_p ref2)
{
  if (ref1 == ref2
      || bitmap_bit_p (ref1->indep_ref, ref2->id))
    return true;
  if (bitmap_bit_p (ref1->dep_ref, ref2->id))
    return false;
  if (!MEM_ANALYZABLE (ref1)
      || !MEM_ANALYZABLE (ref2))
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Querying dependency of refs %u and %u: ",
	     ref1->id, ref2->id);

  if (mem_refs_may_alias_p (ref1->mem, ref2->mem,
			    &memory_accesses.ttae_cache))
    {
      bitmap_set_bit (ref1->dep_ref, ref2->id);
      bitmap_set_bit (ref2->dep_ref, ref1->id);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "dependent.\n");
      return false;
    }
  else
    {
      bitmap_set_bit (ref1->indep_ref, ref2->id);
      bitmap_set_bit (ref2->indep_ref, ref1->id);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "independent.\n");
      return true;
    }
}

/* Records the information whether REF is independent in LOOP (according
   to INDEP).  */

static void
record_indep_loop (struct loop *loop, mem_ref_p ref, bool indep)
{
  if (indep)
    bitmap_set_bit (ref->indep_loop, loop->num);
  else
    bitmap_set_bit (ref->dep_loop, loop->num);
}

/* Returns true if REF is independent on all other memory references in
   LOOP.  */

static bool
ref_indep_loop_p_1 (struct loop *loop, mem_ref_p ref)
{
  bitmap refs_to_check;
  unsigned i;
  bitmap_iterator bi;
  bool ret = true, stored = bitmap_bit_p (ref->stored, loop->num);
  mem_ref_p aref;

  if (stored)
    refs_to_check = memory_accesses.all_refs_in_loop[loop->num];
  else
    refs_to_check = memory_accesses.all_refs_stored_in_loop[loop->num];

  EXECUTE_IF_SET_IN_BITMAP (refs_to_check, 0, i, bi)
    {
      aref = memory_accesses.refs_list[i];
      if (!MEM_ANALYZABLE (aref)
	  || !refs_independent_p (ref, aref))
	{
	  ret = false;
	  record_indep_loop (loop, aref, false);
	  break;
	}
    }

  return ret;
}

/* Returns true if REF is independent on all other memory references in
   LOOP.  Wrapper over ref_indep_loop_p_1, caching its results.  */

static bool
ref_indep_loop_p (struct loop *loop, mem_ref_p ref)
{
  bool ret;

  if (bitmap_bit_p (ref->indep_loop, loop->num))
    return true;
  if (bitmap_bit_p (ref->dep_loop, loop->num))
    return false;

  ret = ref_indep_loop_p_1 (loop, ref);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Querying dependencies of ref %u in loop %d: %s\n",
	     ref->id, loop->num, ret ? "independent" : "dependent");

  record_indep_loop (loop, ref, ret);

  return ret;
}

/* Returns true if we can perform store motion of REF from LOOP.  */

static bool
can_sm_ref_p (struct loop *loop, mem_ref_p ref)
{
  tree base;

  /* Can't hoist unanalyzable refs.  */
  if (!MEM_ANALYZABLE (ref))
    return false;

  /* Unless the reference is stored in the loop, there is nothing to do.  */
  if (!bitmap_bit_p (ref->stored, loop->num))
    return false;

  /* It should be movable.  */
  if (!is_gimple_reg_type (TREE_TYPE (ref->mem))
      || TREE_THIS_VOLATILE (ref->mem)
      || !for_each_index (&ref->mem, may_move_till, loop))
    return false;

  /* If it can throw fail, we do not properly update EH info.  */
  if (tree_could_throw_p (ref->mem))
    return false;

  /* If it can trap, it must be always executed in LOOP.
     Readonly memory locations may trap when storing to them, but
     tree_could_trap_p is a predicate for rvalues, so check that
     explicitly.  */
  base = get_base_address (ref->mem);
  if ((tree_could_trap_p (ref->mem)
       || (DECL_P (base) && TREE_READONLY (base)))
      && !ref_always_accessed_p (loop, ref, true))
    return false;

  /* And it must be independent on all other memory references
     in LOOP.  */
  if (!ref_indep_loop_p (loop, ref))
    return false;

  return true;
}

/* Marks the references in LOOP for that store motion should be performed
   in REFS_TO_SM.  SM_EXECUTED is the set of references for that store
   motion was performed in one of the outer loops.  */

static void
find_refs_for_sm (struct loop *loop, bitmap sm_executed, bitmap refs_to_sm)
{
  bitmap refs = memory_accesses.all_refs_in_loop[loop->num];
  unsigned i;
  bitmap_iterator bi;
  mem_ref_p ref;

  EXECUTE_IF_AND_COMPL_IN_BITMAP (refs, sm_executed, 0, i, bi)
    {
      ref = memory_accesses.refs_list[i];
      if (can_sm_ref_p (loop, ref))
	bitmap_set_bit (refs_to_sm, i);
    }
}

/* Checks whether LOOP (with exits stored in EXITS array) is suitable
   for a store motion optimization (i.e. whether we can insert statement
   on its exits).  */

static bool
loop_suitable_for_sm (struct loop *loop ATTRIBUTE_UNUSED,
		      vec<edge> exits)
{
  unsigned i;
  edge ex;

  FOR_EACH_VEC_ELT (exits, i, ex)
    if (ex->flags & (EDGE_ABNORMAL | EDGE_EH))
      return false;

  return true;
}

/* Try to perform store motion for all memory references modified inside
   LOOP.  SM_EXECUTED is the bitmap of the memory references for that
   store motion was executed in one of the outer loops.  */

static void
store_motion_loop (struct loop *loop, bitmap sm_executed)
{
  vec<edge> exits = get_loop_exit_edges (loop);
  struct loop *subloop;
  bitmap sm_in_loop = BITMAP_ALLOC (NULL);

  if (loop_suitable_for_sm (loop, exits))
    {
      find_refs_for_sm (loop, sm_executed, sm_in_loop);
      hoist_memory_references (loop, sm_in_loop, exits);
    }
  exits.release ();

  bitmap_ior_into (sm_executed, sm_in_loop);
  for (subloop = loop->inner; subloop != NULL; subloop = subloop->next)
    store_motion_loop (subloop, sm_executed);
  bitmap_and_compl_into (sm_executed, sm_in_loop);
  BITMAP_FREE (sm_in_loop);
}

/* Try to perform store motion for all memory references modified inside
   loops.  */

static void
store_motion (void)
{
  struct loop *loop;
  bitmap sm_executed = BITMAP_ALLOC (NULL);

  for (loop = current_loops->tree_root->inner; loop != NULL; loop = loop->next)
    store_motion_loop (loop, sm_executed);

  BITMAP_FREE (sm_executed);
  gsi_commit_edge_inserts ();
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

  if (ALWAYS_EXECUTED_IN (loop->header) == NULL)
    {
      bbs = get_loop_body_in_dom_order (loop);

      for (i = 0; i < loop->num_nodes; i++)
	{
	  edge_iterator ei;
	  bb = bbs[i];

	  if (dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
	    last = bb;

	  if (bitmap_bit_p (contains_call, bb->index))
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
	  SET_ALWAYS_EXECUTED_IN (last, loop);
	  if (last == loop->header)
	    break;
	  last = get_immediate_dominator (CDI_DOMINATORS, last);
	}

      free (bbs);
    }

  for (loop = loop->inner; loop; loop = loop->next)
    fill_always_executed_in (loop, contains_call);
}

/* Compute the global information needed by the loop invariant motion pass.  */

static void
tree_ssa_lim_initialize (void)
{
  sbitmap contains_call = sbitmap_alloc (last_basic_block);
  gimple_stmt_iterator bsi;
  struct loop *loop;
  basic_block bb;

  bitmap_obstack_initialize (&lim_bitmap_obstack);

  bitmap_clear (contains_call);
  FOR_EACH_BB (bb)
    {
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  if (nonpure_call_p (gsi_stmt (bsi)))
	    break;
	}

      if (!gsi_end_p (bsi))
	bitmap_set_bit (contains_call, bb->index);
    }

  for (loop = current_loops->tree_root->inner; loop; loop = loop->next)
    fill_always_executed_in (loop, contains_call);

  sbitmap_free (contains_call);

  lim_aux_data_map = pointer_map_create ();

  if (flag_tm)
    compute_transaction_bits ();

  alloc_aux_for_edges (0);
}

/* Cleans up after the invariant motion pass.  */

static void
tree_ssa_lim_finalize (void)
{
  basic_block bb;
  unsigned i;
  mem_ref_p ref;

  free_aux_for_edges ();

  FOR_EACH_BB (bb)
    SET_ALWAYS_EXECUTED_IN (bb, NULL);

  bitmap_obstack_release (&lim_bitmap_obstack);
  pointer_map_destroy (lim_aux_data_map);

  htab_delete (memory_accesses.refs);

  FOR_EACH_VEC_ELT (memory_accesses.refs_list, i, ref)
    memref_free (ref);
  memory_accesses.refs_list.release ();

  memory_accesses.refs_in_loop.release ();
  memory_accesses.all_refs_in_loop.release ();
  memory_accesses.all_refs_stored_in_loop.release ();

  if (memory_accesses.ttae_cache)
    free_affine_expand_cache (&memory_accesses.ttae_cache);
}

/* Moves invariants from loops.  Only "expensive" invariants are moved out --
   i.e. those that are likely to be win regardless of the register pressure.  */

unsigned int
tree_ssa_lim (void)
{
  unsigned int todo;

  tree_ssa_lim_initialize ();

  /* Gathers information about memory accesses in the loops.  */
  analyze_memory_references ();

  /* For each statement determine the outermost loop in that it is
     invariant and cost for computing the invariant.  */
  determine_invariantness ();

  /* Execute store motion.  Force the necessary invariants to be moved
     out of the loops as well.  */
  store_motion ();

  /* Move the expressions that are expensive enough.  */
  todo = move_computations ();

  tree_ssa_lim_finalize ();

  return todo;
}
