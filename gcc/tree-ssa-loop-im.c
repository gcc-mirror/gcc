/* Loop invariant motion.
   Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   
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
#include "real.h"
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

/* Description of a memory reference location.  */

typedef struct mem_ref_loc
{
  tree *ref;			/* The reference itself.  */
  tree stmt;			/* The statement in that it occurs.  */
} *mem_ref_loc_p;

DEF_VEC_P(mem_ref_loc_p);
DEF_VEC_ALLOC_P(mem_ref_loc_p, heap);

/* The list of memory reference locations in a loop.  */

typedef struct mem_ref_locs
{
  VEC (mem_ref_loc_p, heap) *locs;
} *mem_ref_locs_p;

DEF_VEC_P(mem_ref_locs_p);
DEF_VEC_ALLOC_P(mem_ref_locs_p, heap);

/* Description of a memory reference.  */

typedef struct mem_ref
{
  tree mem;			/* The memory itself.  */
  unsigned id;			/* ID assigned to the memory reference
				   (its index in memory_accesses.refs_list)  */
  hashval_t hash;		/* Its hash value.  */
  bitmap stored;		/* The set of loops in that this memory locatio
				   is stored to.  */
  VEC (mem_ref_locs_p, heap) *accesses_in_loop;
				/* The locations of the accesses.  Vector
				   indexed by the loop number.  */
  bitmap vops;			/* Vops corresponding to this memory
				   location.  */

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
  bitmap dep_ref;		/* The complement of DEP_REF.  */
} *mem_ref_p;

DEF_VEC_P(mem_ref_p);
DEF_VEC_ALLOC_P(mem_ref_p, heap);

DEF_VEC_P(bitmap);
DEF_VEC_ALLOC_P(bitmap, heap);

DEF_VEC_P(htab_t);
DEF_VEC_ALLOC_P(htab_t, heap);

/* Description of memory accesses in loops.  */

static struct
{
  /* The hash table of memory references accessed in loops.  */
  htab_t refs;

  /* The list of memory references.  */
  VEC (mem_ref_p, heap) *refs_list;

  /* The set of memory references accessed in each loop.  */
  VEC (bitmap, heap) *refs_in_loop;

  /* The set of memory references accessed in each loop, including
     subloops.  */
  VEC (bitmap, heap) *all_refs_in_loop;

  /* The set of virtual operands clobbered in a given loop.  */
  VEC (bitmap, heap) *clobbered_vops;

  /* Map from the pair (loop, virtual operand) to the set of refs that
     touch the virtual operand in the loop.  */
  VEC (htab_t, heap) *vop_ref_map;

  /* Cache for expanding memory addresses.  */
  struct pointer_map_t *ttae_cache;
} memory_accesses;

static bool ref_indep_loop_p (struct loop *, mem_ref_p);

/* Minimum cost of an expensive expression.  */
#define LIM_EXPENSIVE ((unsigned) PARAM_VALUE (PARAM_LIM_EXPENSIVE))

/* The outermost loop for that execution of the header guarantees that the
   block will be executed.  */
#define ALWAYS_EXECUTED_IN(BB) ((struct loop *) (BB)->aux)

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
      return MOVE_POSSIBLE;
    }

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return MOVE_IMPOSSIBLE;

  if (!ZERO_SSA_OPERANDS (stmt, SSA_OP_VIRTUAL_DEFS))
    return MOVE_IMPOSSIBLE;

  if (stmt_ends_bb_p (stmt))
    return MOVE_IMPOSSIBLE;

  if (stmt_ann (stmt)->has_volatile_ops)
    return MOVE_IMPOSSIBLE;

  lhs = GIMPLE_STMT_OPERAND (stmt, 0);
  if (TREE_CODE (lhs) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return MOVE_IMPOSSIBLE;

  rhs = GIMPLE_STMT_OPERAND (stmt, 1);

  if (TREE_SIDE_EFFECTS (rhs)
      || tree_could_throw_p (rhs))
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
				 loop_outer (LIM_DATA (def_stmt)->max_loop));
  if (max_loop == loop)
    return NULL;
  max_loop = superloop_at_depth (loop, loop_depth (max_loop) + 1);

  return max_loop;
}

/* Returns the outermost superloop of LOOP in that the expression EXPR is
   invariant.  */

static struct loop *
outermost_invariant_loop_expr (tree expr, struct loop *loop)
{
  enum tree_code_class codeclass = TREE_CODE_CLASS (TREE_CODE (expr));
  unsigned i, nops;
  struct loop *max_loop = superloop_at_depth (loop, 1), *aloop;

  if (TREE_CODE (expr) == SSA_NAME
      || TREE_CODE (expr) == INTEGER_CST
      || is_gimple_min_invariant (expr))
    return outermost_invariant_loop (expr, loop);

  if (codeclass != tcc_unary
      && codeclass != tcc_binary
      && codeclass != tcc_expression
      && codeclass != tcc_vl_exp
      && codeclass != tcc_comparison)
    return NULL;

  nops = TREE_OPERAND_LENGTH (expr);
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

  dep = XNEW (struct depend);
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
  tree rhs;
  unsigned cost = 1;

  /* Always try to create possibilities for unswitching.  */
  if (TREE_CODE (stmt) == COND_EXPR)
    return LIM_EXPENSIVE;

  rhs = GENERIC_TREE_OPERAND (stmt, 1);

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
    case RDIV_EXPR:
      /* Division and multiplication are usually expensive.  */
      cost += 20;
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      cost += 20;
      break;

    default:
      break;
    }

  return cost;
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
   the location of the memory reference, and sets IS_STORE accoring to whether
   it is a store or load.  Otherwise, returns NULL.  */

static tree *
simple_mem_ref_in_stmt (tree stmt, bool *is_store)
{
  tree *lhs, *rhs;

  /* Recognize MEM = (SSA_NAME | invariant) and SSA_NAME = MEM patterns.  */
  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return NULL;

  lhs = &GIMPLE_STMT_OPERAND (stmt, 0);
  rhs = &GIMPLE_STMT_OPERAND (stmt, 1);

  if (TREE_CODE (*lhs) == SSA_NAME)
    {
      if (!is_gimple_addressable (*rhs))
	return NULL;

      *is_store = false;
      return rhs;
    }
  else if (TREE_CODE (*rhs) == SSA_NAME
	   || is_gimple_min_invariant (*rhs))
    {
      *is_store = true;
      return lhs;
    }
  else
    return NULL;
}

/* Returns the memory reference contained in STMT.  */

static mem_ref_p
mem_ref_in_stmt (tree stmt)
{
  bool store;
  tree *mem = simple_mem_ref_in_stmt (stmt, &store);
  hashval_t hash;
  mem_ref_p ref;

  if (!mem)
    return NULL;
  gcc_assert (!store);

  hash = iterative_hash_expr (*mem, 0);
  ref = htab_find_with_hash (memory_accesses.refs, *mem, hash);

  gcc_assert (ref != NULL);
  return ref;
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

  if (!ZERO_SSA_OPERANDS (stmt, SSA_OP_VIRTUAL_USES))
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
	  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_VIRTUAL_USES)
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
set_level (tree stmt, struct loop *orig_loop, struct loop *level)
{
  struct loop *stmt_loop = bb_for_stmt (stmt)->loop_father;
  struct depend *dep;

  stmt_loop = find_common_loop (orig_loop, stmt_loop);
  if (LIM_DATA (stmt) && LIM_DATA (stmt)->tgt_loop)
    stmt_loop = find_common_loop (stmt_loop,
				  loop_outer (LIM_DATA (stmt)->tgt_loop));
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

/* Rewrite a/b to a*(1/b).  Return the invariant stmt to process.  */

static tree
rewrite_reciprocal (block_stmt_iterator *bsi)
{
  tree stmt, lhs, rhs, stmt1, stmt2, var, name, tmp;

  stmt = bsi_stmt (*bsi);
  lhs = GENERIC_TREE_OPERAND (stmt, 0);
  rhs = GENERIC_TREE_OPERAND (stmt, 1);

  /* stmt must be GIMPLE_MODIFY_STMT.  */
  var = create_tmp_var (TREE_TYPE (rhs), "reciptmp");
  add_referenced_var (var);

  tmp = build2 (RDIV_EXPR, TREE_TYPE (rhs),
		build_real (TREE_TYPE (rhs), dconst1),
		TREE_OPERAND (rhs, 1));
  stmt1 = build_gimple_modify_stmt (var, tmp);
  name = make_ssa_name (var, stmt1);
  GIMPLE_STMT_OPERAND (stmt1, 0) = name;
  tmp = build2 (MULT_EXPR, TREE_TYPE (rhs),
		name, TREE_OPERAND (rhs, 0));
  stmt2 = build_gimple_modify_stmt (lhs, tmp);

  /* Replace division stmt with reciprocal and multiply stmts.
     The multiply stmt is not invariant, so update iterator
     and avoid rescanning.  */
  bsi_replace (bsi, stmt1, true);
  bsi_insert_after (bsi, stmt2, BSI_NEW_STMT);
  SSA_NAME_DEF_STMT (lhs) = stmt2;

  /* Continue processing with invariant reciprocal statement.  */
  return stmt1;
}

/* Check if the pattern at *BSI is a bittest of the form
   (A >> B) & 1 != 0 and in this case rewrite it to A & (1 << B) != 0.  */

static tree
rewrite_bittest (block_stmt_iterator *bsi)
{
  tree stmt, lhs, rhs, var, name, use_stmt, stmt1, stmt2, t;
  use_operand_p use;

  stmt = bsi_stmt (*bsi);
  lhs = GENERIC_TREE_OPERAND (stmt, 0);
  rhs = GENERIC_TREE_OPERAND (stmt, 1);

  /* Verify that the single use of lhs is a comparison against zero.  */
  if (TREE_CODE (lhs) != SSA_NAME
      || !single_imm_use (lhs, &use, &use_stmt)
      || TREE_CODE (use_stmt) != COND_EXPR)
    return stmt;
  t = COND_EXPR_COND (use_stmt);
  if (TREE_OPERAND (t, 0) != lhs
      || (TREE_CODE (t) != NE_EXPR
	  && TREE_CODE (t) != EQ_EXPR)
      || !integer_zerop (TREE_OPERAND (t, 1)))
    return stmt;

  /* Get at the operands of the shift.  The rhs is TMP1 & 1.  */
  stmt1 = SSA_NAME_DEF_STMT (TREE_OPERAND (rhs, 0));
  if (TREE_CODE (stmt1) != GIMPLE_MODIFY_STMT)
    return stmt;

  /* There is a conversion in between possibly inserted by fold.  */
  t = GIMPLE_STMT_OPERAND (stmt1, 1);
  if (CONVERT_EXPR_P (t))
    {
      t = TREE_OPERAND (t, 0);
      if (TREE_CODE (t) != SSA_NAME
	  || !has_single_use (t))
	return stmt;
      stmt1 = SSA_NAME_DEF_STMT (t);
      if (TREE_CODE (stmt1) != GIMPLE_MODIFY_STMT)
	return stmt;
      t = GIMPLE_STMT_OPERAND (stmt1, 1);
    }

  /* Verify that B is loop invariant but A is not.  Verify that with
     all the stmt walking we are still in the same loop.  */
  if (TREE_CODE (t) == RSHIFT_EXPR
      && loop_containing_stmt (stmt1) == loop_containing_stmt (stmt)
      && outermost_invariant_loop_expr (TREE_OPERAND (t, 1),
                                        loop_containing_stmt (stmt1)) != NULL
      && outermost_invariant_loop_expr (TREE_OPERAND (t, 0),
                                        loop_containing_stmt (stmt1)) == NULL)
    {
      tree a = TREE_OPERAND (t, 0);
      tree b = TREE_OPERAND (t, 1);

      /* 1 << B */
      var = create_tmp_var (TREE_TYPE (a), "shifttmp");
      add_referenced_var (var);
      t = fold_build2 (LSHIFT_EXPR, TREE_TYPE (a),
		       build_int_cst (TREE_TYPE (a), 1), b);
      stmt1 = build_gimple_modify_stmt (var, t);
      name = make_ssa_name (var, stmt1);
      GIMPLE_STMT_OPERAND (stmt1, 0) = name;

      /* A & (1 << B) */
      t = fold_build2 (BIT_AND_EXPR, TREE_TYPE (a), a, name);
      stmt2 = build_gimple_modify_stmt (var, t);
      name = make_ssa_name (var, stmt2);
      GIMPLE_STMT_OPERAND (stmt2, 0) = name;

      /* Replace the SSA_NAME we compare against zero.  Adjust
	 the type of zero accordingly.  */
      SET_USE (use, name);
      TREE_OPERAND (COND_EXPR_COND (use_stmt), 1)
	= build_int_cst_type (TREE_TYPE (name), 0);

      bsi_insert_before (bsi, stmt1, BSI_SAME_STMT);
      bsi_replace (bsi, stmt2, true);

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
  block_stmt_iterator bsi;
  tree stmt, rhs;
  bool maybe_never = ALWAYS_EXECUTED_IN (bb) == NULL;
  struct loop *outermost = ALWAYS_EXECUTED_IN (bb);

  if (!loop_outer (bb->loop_father))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Basic block %d (loop %d -- depth %d):\n\n",
	     bb->index, bb->loop_father->num, loop_depth (bb->loop_father));

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
	  /* Make sure to note always_executed_in for stores to make
	     store-motion work.  */
	  else if (stmt_makes_single_store (stmt))
	    {
	      stmt_ann (stmt)->common.aux
		= xcalloc (1, sizeof (struct lim_aux_data));
	      LIM_DATA (stmt)->always_executed_in = outermost;
	    }
	  continue;
	}

      if (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT)
	{
	  rhs = GIMPLE_STMT_OPERAND (stmt, 1);

	  /* If divisor is invariant, convert a/b to a*(1/b), allowing reciprocal
	     to be hoisted out of loop, saving expensive divide.  */
	  if (pos == MOVE_POSSIBLE
	      && TREE_CODE (rhs) == RDIV_EXPR
	      && flag_unsafe_math_optimizations
	      && !flag_trapping_math
	      && outermost_invariant_loop_expr (TREE_OPERAND (rhs, 1),
						loop_containing_stmt (stmt)) != NULL
	      && outermost_invariant_loop_expr (rhs,
						loop_containing_stmt (stmt)) == NULL)
	    stmt = rewrite_reciprocal (&bsi);

	  /* If the shift count is invariant, convert (A >> B) & 1 to
	     A & (1 << B) allowing the bit mask to be hoisted out of the loop
	     saving an expensive shift.  */
	  if (pos == MOVE_POSSIBLE
	      && TREE_CODE (rhs) == BIT_AND_EXPR
	      && integer_onep (TREE_OPERAND (rhs, 1))
	      && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME
	      && has_single_use (TREE_OPERAND (rhs, 0)))
	    stmt = rewrite_bittest (&bsi);
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
		   loop_depth (LIM_DATA (stmt)->max_loop),
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
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.before_dom_children_before_stmts = determine_invariantness_stmt;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);
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

  if (!loop_outer (bb->loop_father))
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

      mark_virtual_ops_for_renaming (stmt);
      bsi_insert_on_edge (loop_preheader_edge (level), stmt);
      bsi_remove (&bsi, false);
    }
}

/* Hoist the statements out of the loops prescribed by data stored in
   LIM_DATA structures associated with each statement.*/

static void
move_computations (void)
{
  struct dom_walk_data walk_data;

  memset (&walk_data, 0, sizeof (struct dom_walk_data));
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.before_dom_children_before_stmts = move_computations_stmt;

  init_walk_dominator_tree (&walk_data);
  walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);
  fini_walk_dominator_tree (&walk_data);

  bsi_commit_edge_inserts ();
  if (need_ssa_update_p ())
    rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
}

/* Checks whether the statement defining variable *INDEX can be hoisted
   out of the loop passed in DATA.  Callback for for_each_index.  */

static bool
may_move_till (tree ref, tree *index, void *data)
{
  struct loop *loop = (struct loop*) data, *max_loop;

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
  enum tree_code_class codeclass = TREE_CODE_CLASS (TREE_CODE (expr));
  unsigned i, nops;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      tree stmt = SSA_NAME_DEF_STMT (expr);
      if (IS_EMPTY_STMT (stmt))
	return;

      set_level (stmt, orig_loop, loop);
      return;
    }

  if (codeclass != tcc_unary
      && codeclass != tcc_binary
      && codeclass != tcc_expression
      && codeclass != tcc_vl_exp
      && codeclass != tcc_comparison)
    return;

  nops = TREE_OPERAND_LENGTH (expr);
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
  struct fmt_data *fmt_data = (struct fmt_data *) data;

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

/* A hash function for struct mem_ref object OBJ.  */

static hashval_t
memref_hash (const void *obj)
{
  const struct mem_ref *mem = obj;

  return mem->hash;
}

/* An equality function for struct mem_ref object OBJ1 with
   memory reference OBJ2.  */

static int
memref_eq (const void *obj1, const void *obj2)
{
  const struct mem_ref *mem1 = obj1;

  return operand_equal_p (mem1->mem, (tree) obj2, 0);
}

/* Releases list of memory reference locations ACCS.  */

static void
free_mem_ref_locs (mem_ref_locs_p accs)
{
  unsigned i;
  mem_ref_loc_p loc;

  if (!accs)
    return;

  for (i = 0; VEC_iterate (mem_ref_loc_p, accs->locs, i, loc); i++)
    free (loc);
  VEC_free (mem_ref_loc_p, heap, accs->locs);
  free (accs);
}

/* A function to free the mem_ref object OBJ.  */

static void
memref_free (void *obj)
{
  struct mem_ref *mem = obj;
  unsigned i;
  mem_ref_locs_p accs;

  BITMAP_FREE (mem->stored);
  BITMAP_FREE (mem->indep_loop);
  BITMAP_FREE (mem->dep_loop);
  BITMAP_FREE (mem->indep_ref);
  BITMAP_FREE (mem->dep_ref);

  for (i = 0; VEC_iterate (mem_ref_locs_p, mem->accesses_in_loop, i, accs); i++)
    free_mem_ref_locs (accs);
  VEC_free (mem_ref_locs_p, heap, mem->accesses_in_loop);

  BITMAP_FREE (mem->vops);
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
  ref->stored = BITMAP_ALLOC (NULL);
  ref->indep_loop = BITMAP_ALLOC (NULL);
  ref->dep_loop = BITMAP_ALLOC (NULL);
  ref->indep_ref = BITMAP_ALLOC (NULL);
  ref->dep_ref = BITMAP_ALLOC (NULL);
  ref->accesses_in_loop = NULL;
  ref->vops = BITMAP_ALLOC (NULL);

  return ref;
}

/* Allocates and returns the new list of locations.  */

static mem_ref_locs_p
mem_ref_locs_alloc (void)
{
  mem_ref_locs_p accs = XNEW (struct mem_ref_locs);
  accs->locs = NULL;
  return accs;
}

/* Records memory reference location *LOC in LOOP to the memory reference
   description REF.  The reference occurs in statement STMT.  */

static void
record_mem_ref_loc (mem_ref_p ref, struct loop *loop, tree stmt, tree *loc)
{
  mem_ref_loc_p aref = XNEW (struct mem_ref_loc);
  mem_ref_locs_p accs;
  bitmap ril = VEC_index (bitmap, memory_accesses.refs_in_loop, loop->num);

  if (VEC_length (mem_ref_locs_p, ref->accesses_in_loop)
      <= (unsigned) loop->num)
    VEC_safe_grow_cleared (mem_ref_locs_p, heap, ref->accesses_in_loop,
			   loop->num + 1);
  accs = VEC_index (mem_ref_locs_p, ref->accesses_in_loop, loop->num);
  if (!accs)
    {
      accs = mem_ref_locs_alloc ();
      VEC_replace (mem_ref_locs_p, ref->accesses_in_loop, loop->num, accs);
    }

  aref->stmt = stmt;
  aref->ref = loc;

  VEC_safe_push (mem_ref_loc_p, heap, accs->locs, aref);
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
gather_mem_refs_stmt (struct loop *loop, tree stmt)
{
  tree *mem = NULL;
  hashval_t hash;
  PTR *slot;
  mem_ref_p ref;
  ssa_op_iter oi;
  tree vname;
  bool is_stored;
  bitmap clvops;
  unsigned id;

  if (ZERO_SSA_OPERANDS (stmt, SSA_OP_ALL_VIRTUALS))
    return;

  mem = simple_mem_ref_in_stmt (stmt, &is_stored);
  if (!mem)
    goto fail;

  hash = iterative_hash_expr (*mem, 0);
  slot = htab_find_slot_with_hash (memory_accesses.refs, *mem, hash, INSERT);

  if (*slot)
    {
      ref = *slot;
      id = ref->id;
    }
  else
    {
      id = VEC_length (mem_ref_p, memory_accesses.refs_list);
      ref = mem_ref_alloc (*mem, hash, id);
      VEC_safe_push (mem_ref_p, heap, memory_accesses.refs_list, ref);
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

  FOR_EACH_SSA_TREE_OPERAND (vname, stmt, oi, SSA_OP_VIRTUAL_USES)
    bitmap_set_bit (ref->vops, DECL_UID (SSA_NAME_VAR (vname)));
  record_mem_ref_loc (ref, loop, stmt, mem);
  return;

fail:
  clvops = VEC_index (bitmap, memory_accesses.clobbered_vops, loop->num);
  FOR_EACH_SSA_TREE_OPERAND (vname, stmt, oi, SSA_OP_VIRTUAL_USES)
    bitmap_set_bit (clvops, DECL_UID (SSA_NAME_VAR (vname)));
}

/* Gathers memory references in loops.  */

static void
gather_mem_refs_in_loops (void)
{
  block_stmt_iterator bsi;
  basic_block bb;
  struct loop *loop;
  loop_iterator li;
  bitmap clvo, clvi;
  bitmap lrefs, alrefs, alrefso;

  FOR_EACH_BB (bb)
    {
      loop = bb->loop_father;
      if (loop == current_loops->tree_root)
	continue;

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	gather_mem_refs_stmt (loop, bsi_stmt (bsi));
    }

  /* Propagate the information about clobbered vops and accessed memory
     references up the loop hierarchy.  */
  FOR_EACH_LOOP (li, loop, LI_FROM_INNERMOST)
    {
      lrefs = VEC_index (bitmap, memory_accesses.refs_in_loop, loop->num);
      alrefs = VEC_index (bitmap, memory_accesses.all_refs_in_loop, loop->num);
      bitmap_ior_into (alrefs, lrefs);

      if (loop_outer (loop) == current_loops->tree_root)
	continue;

      clvi = VEC_index (bitmap, memory_accesses.clobbered_vops, loop->num);
      clvo = VEC_index (bitmap, memory_accesses.clobbered_vops,
			loop_outer (loop)->num);
      bitmap_ior_into (clvo, clvi);

      alrefso = VEC_index (bitmap, memory_accesses.all_refs_in_loop,
			   loop_outer (loop)->num);
      bitmap_ior_into (alrefso, alrefs);
    }
}

/* Element of the hash table that maps vops to memory references.  */

struct vop_to_refs_elt
{
  /* DECL_UID of the vop.  */
  unsigned uid;

  /* List of the all references.  */
  bitmap refs_all;

  /* List of stored references.  */
  bitmap refs_stored;
};

/* A hash function for struct vop_to_refs_elt object OBJ.  */

static hashval_t
vtoe_hash (const void *obj)
{
  const struct vop_to_refs_elt *vtoe = obj;

  return vtoe->uid;
}

/* An equality function for struct vop_to_refs_elt object OBJ1 with
   uid of a vop OBJ2.  */

static int
vtoe_eq (const void *obj1, const void *obj2)
{
  const struct vop_to_refs_elt *vtoe = obj1;
  const unsigned *uid = obj2;

  return vtoe->uid == *uid;
}

/* A function to free the struct vop_to_refs_elt object.  */

static void
vtoe_free (void *obj)
{
  struct vop_to_refs_elt *vtoe = obj;

  BITMAP_FREE (vtoe->refs_all);
  BITMAP_FREE (vtoe->refs_stored);
  free (vtoe);
}

/* Records REF to hashtable VOP_TO_REFS for the index VOP.  STORED is true
   if the reference REF is stored.  */

static void
record_vop_access (htab_t vop_to_refs, unsigned vop, unsigned ref, bool stored)
{
  void **slot = htab_find_slot_with_hash (vop_to_refs, &vop, vop, INSERT);
  struct vop_to_refs_elt *vtoe;

  if (!*slot)
    {
      vtoe = XNEW (struct vop_to_refs_elt);
      vtoe->uid = vop;
      vtoe->refs_all = BITMAP_ALLOC (NULL);
      vtoe->refs_stored = BITMAP_ALLOC (NULL);
      *slot = vtoe;
    }
  else
    vtoe = *slot;

  bitmap_set_bit (vtoe->refs_all, ref);
  if (stored)
    bitmap_set_bit (vtoe->refs_stored, ref);
}

/* Returns the set of references that access VOP according to the table
   VOP_TO_REFS.  */

static bitmap
get_vop_accesses (htab_t vop_to_refs, unsigned vop)
{
  struct vop_to_refs_elt *vtoe = htab_find_with_hash (vop_to_refs, &vop, vop);
  return vtoe->refs_all;
}

/* Returns the set of stores that access VOP according to the table
   VOP_TO_REFS.  */

static bitmap
get_vop_stores (htab_t vop_to_refs, unsigned vop)
{
  struct vop_to_refs_elt *vtoe = htab_find_with_hash (vop_to_refs, &vop, vop);
  return vtoe->refs_stored;
}

/* Adds REF to mapping from virtual operands to references in LOOP.  */

static void
add_vop_ref_mapping (struct loop *loop, mem_ref_p ref)
{
  htab_t map = VEC_index (htab_t, memory_accesses.vop_ref_map, loop->num);
  bool stored = bitmap_bit_p (ref->stored, loop->num);
  bitmap clobbers = VEC_index (bitmap, memory_accesses.clobbered_vops,
			       loop->num);
  bitmap_iterator bi;
  unsigned vop;

  EXECUTE_IF_AND_COMPL_IN_BITMAP (ref->vops, clobbers, 0, vop, bi)
    {
      record_vop_access (map, vop, ref->id, stored);
    }
}

/* Create a mapping from virtual operands to references that touch them
   in LOOP.  */

static void
create_vop_ref_mapping_loop (struct loop *loop)
{
  bitmap refs = VEC_index (bitmap, memory_accesses.refs_in_loop, loop->num);
  struct loop *sloop;
  bitmap_iterator bi;
  unsigned i;
  mem_ref_p ref;

  EXECUTE_IF_SET_IN_BITMAP (refs, 0, i, bi)
    {
      ref = VEC_index (mem_ref_p, memory_accesses.refs_list, i);
      for (sloop = loop; sloop != current_loops->tree_root; sloop = loop_outer (sloop))
	add_vop_ref_mapping (sloop, ref);
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
  htab_t hempty;

  memory_accesses.refs
	  = htab_create (100, memref_hash, memref_eq, memref_free);
  memory_accesses.refs_list = NULL;
  memory_accesses.refs_in_loop = VEC_alloc (bitmap, heap,
					    number_of_loops ());
  memory_accesses.all_refs_in_loop = VEC_alloc (bitmap, heap,
						number_of_loops ());
  memory_accesses.clobbered_vops = VEC_alloc (bitmap, heap,
					      number_of_loops ());
  memory_accesses.vop_ref_map = VEC_alloc (htab_t, heap,
					   number_of_loops ());

  for (i = 0; i < number_of_loops (); i++)
    {
      empty = BITMAP_ALLOC (NULL);
      VEC_quick_push (bitmap, memory_accesses.refs_in_loop, empty);
      empty = BITMAP_ALLOC (NULL);
      VEC_quick_push (bitmap, memory_accesses.all_refs_in_loop, empty);
      empty = BITMAP_ALLOC (NULL);
      VEC_quick_push (bitmap, memory_accesses.clobbered_vops, empty);
      hempty = htab_create (10, vtoe_hash, vtoe_eq, vtoe_free);
      VEC_quick_push (htab_t, memory_accesses.vop_ref_map, hempty);
    }

  memory_accesses.ttae_cache = NULL;

  gather_mem_refs_in_loops ();
  create_vop_ref_mapping ();
}

/* Returns true if a region of size SIZE1 at position 0 and a region of
   size SIZE2 at position DIFF cannot overlap.  */

static bool
cannot_overlap_p (aff_tree *diff, double_int size1, double_int size2)
{
  double_int d, bound;

  /* Unless the difference is a constant, we fail.  */
  if (diff->n != 0)
    return false;

  d = diff->offset;
  if (double_int_negative_p (d))
    {
      /* The second object is before the first one, we succeed if the last
	 element of the second object is before the start of the first one.  */
      bound = double_int_add (d, double_int_add (size2, double_int_minus_one));
      return double_int_negative_p (bound);
    }
  else
    {
      /* We succeed if the second object starts after the first one ends.  */
      return double_int_scmp (size1, d) <= 0;
    }
}

/* Returns true if MEM1 and MEM2 may alias.  TTAE_CACHE is used as a cache in
   tree_to_aff_combination_expand.  */

static bool
mem_refs_may_alias_p (tree mem1, tree mem2, struct pointer_map_t **ttae_cache)
{
  /* Perform BASE + OFFSET analysis -- if MEM1 and MEM2 are based on the same
     object and their offset differ in such a way that the locations cannot
     overlap, then they cannot alias.  */
  aff_tree off1, off2;
  double_int size1, size2;
  tree base1, base2;

  /* If MEM1 and MEM2 are based on different variables, they cannot alias.  */
  base1 = get_base_address (mem1);
  base2 = get_base_address (mem2);

  if (base1
      && !INDIRECT_REF_P (base1)
      && base2
      && !INDIRECT_REF_P (base2)
      && !operand_equal_p (base1, base2, 0))
    return false;

  /* With strict aliasing, it is impossible to access a scalar variable through
     anything but a pointer dereference or through a union (gcc extension).  */
  if (flag_strict_aliasing)
    {
      if (!INDIRECT_REF_P (mem1)
	  && base1
	  && TREE_CODE (TREE_TYPE (base1)) != UNION_TYPE
	  && SSA_VAR_P (mem2)
	  && !AGGREGATE_TYPE_P (TREE_TYPE (mem2)))
	return false;
      if (!INDIRECT_REF_P (mem2)
	  && base2
	  && TREE_CODE (TREE_TYPE (base2)) != UNION_TYPE
	  && SSA_VAR_P (mem1)
	  && !AGGREGATE_TYPE_P (TREE_TYPE (mem1)))
	return false;
      if (!alias_sets_conflict_p (get_alias_set (mem1), get_alias_set (mem2)))
	return false;
    }

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

  if (cannot_overlap_p (&off2, size1, size2))
    return false;

  return true;
}

/* Rewrites location LOC by TMP_VAR.  */

static void
rewrite_mem_ref_loc (mem_ref_loc_p loc, tree tmp_var)
{
  mark_virtual_ops_for_renaming (loc->stmt);
  *loc->ref = tmp_var;
  update_stmt (loc->stmt);
}

/* Adds all locations of REF in LOOP and its subloops to LOCS.  */

static void
get_all_locs_in_loop (struct loop *loop, mem_ref_p ref,
		      VEC (mem_ref_loc_p, heap) **locs)
{
  mem_ref_locs_p accs;
  unsigned i;
  mem_ref_loc_p loc;
  bitmap refs = VEC_index (bitmap, memory_accesses.all_refs_in_loop,
			   loop->num);
  struct loop *subloop;

  if (!bitmap_bit_p (refs, ref->id))
    return;

  if (VEC_length (mem_ref_locs_p, ref->accesses_in_loop)
      > (unsigned) loop->num)
    {
      accs = VEC_index (mem_ref_locs_p, ref->accesses_in_loop, loop->num);
      if (accs)
	{
	  for (i = 0; VEC_iterate (mem_ref_loc_p, accs->locs, i, loc); i++)
	    VEC_safe_push (mem_ref_loc_p, heap, *locs, loc);
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
  VEC (mem_ref_loc_p, heap) *locs = NULL;

  get_all_locs_in_loop (loop, ref, &locs);
  for (i = 0; VEC_iterate (mem_ref_loc_p, locs, i, loc); i++)
    rewrite_mem_ref_loc (loc, tmp_var);
  VEC_free (mem_ref_loc_p, heap, locs);
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
    case MISALIGNED_INDIRECT_REF:
    case ALIGN_INDIRECT_REF:
    case INDIRECT_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_");
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
      lsm_tmp_name_add ("_");
      lsm_tmp_name_add (name);

    case ARRAY_REF:
      gen_lsm_tmp_name (TREE_OPERAND (ref, 0));
      lsm_tmp_name_add ("_I");
      break;

    case SSA_NAME:
      ref = SSA_NAME_VAR (ref);
      /* Fallthru.  */

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

/* Executes store motion of memory reference REF from LOOP.
   Exits from the LOOP are stored in EXITS.  The initialization of the
   temporary variable is put to the preheader of the loop, and assignments
   to the reference from the temporary variable are emitted to exits.  */

static void
execute_sm (struct loop *loop, VEC (edge, heap) *exits, mem_ref_p ref)
{
  tree tmp_var;
  unsigned i;
  tree load, store;
  struct fmt_data fmt_data;
  edge ex;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Executing store motion of ");
      print_generic_expr (dump_file, ref->mem, 0);
      fprintf (dump_file, " from loop %d\n", loop->num);
    }

  tmp_var = make_rename_temp (TREE_TYPE (ref->mem),
			      get_lsm_tmp_name (ref->mem, ~0));

  fmt_data.loop = loop;
  fmt_data.orig_loop = loop;
  for_each_index (&ref->mem, force_move_till, &fmt_data);

  rewrite_mem_refs (loop, ref, tmp_var);

  /* Emit the load & stores.  */
  load = build_gimple_modify_stmt (tmp_var, unshare_expr (ref->mem));
  get_stmt_ann (load)->common.aux = xcalloc (1, sizeof (struct lim_aux_data));
  LIM_DATA (load)->max_loop = loop;
  LIM_DATA (load)->tgt_loop = loop;

  /* Put this into the latch, so that we are sure it will be processed after
     all dependencies.  */
  bsi_insert_on_edge (loop_latch_edge (loop), load);

  for (i = 0; VEC_iterate (edge, exits, i, ex); i++)
    {
      store = build_gimple_modify_stmt (unshare_expr (ref->mem), tmp_var);
      bsi_insert_on_edge (ex, store);
    }
}

/* Hoists memory references MEM_REFS out of LOOP.  EXITS is the list of exit
   edges of the LOOP.  */

static void
hoist_memory_references (struct loop *loop, bitmap mem_refs,
			 VEC (edge, heap) *exits)
{
  mem_ref_p ref;
  unsigned  i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (mem_refs, 0, i, bi)
    {
      ref = VEC_index (mem_ref_p, memory_accesses.refs_list, i);
      execute_sm (loop, exits, ref);
    }
}

/* Returns true if REF is always accessed in LOOP.  */

static bool
ref_always_accessed_p (struct loop *loop, mem_ref_p ref)
{
  VEC (mem_ref_loc_p, heap) *locs = NULL;
  unsigned i;
  mem_ref_loc_p loc;
  bool ret = false;
  struct loop *must_exec;

  get_all_locs_in_loop (loop, ref, &locs);
  for (i = 0; VEC_iterate (mem_ref_loc_p, locs, i, loc); i++)
    {
      if (!LIM_DATA (loc->stmt))
	continue;

      must_exec = LIM_DATA (loc->stmt)->always_executed_in;
      if (!must_exec)
	continue;

      if (must_exec == loop
	  || flow_loop_nested_p (must_exec, loop))
	{
	  ret = true;
	  break;
	}
    }
  VEC_free (mem_ref_loc_p, heap, locs);

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
  bitmap clobbers, refs_to_check, refs;
  unsigned i;
  bitmap_iterator bi;
  bool ret = true, stored = bitmap_bit_p (ref->stored, loop->num);
  htab_t map;
  mem_ref_p aref;

  /* If the reference is clobbered, it is not independent.  */
  clobbers = VEC_index (bitmap, memory_accesses.clobbered_vops, loop->num);
  if (bitmap_intersect_p (ref->vops, clobbers))
    return false;

  refs_to_check = BITMAP_ALLOC (NULL);

  map = VEC_index (htab_t, memory_accesses.vop_ref_map, loop->num);
  EXECUTE_IF_AND_COMPL_IN_BITMAP (ref->vops, clobbers, 0, i, bi)
    {
      if (stored)
	refs = get_vop_accesses (map, i);
      else
	refs = get_vop_stores (map, i);

      bitmap_ior_into (refs_to_check, refs);
    }

  EXECUTE_IF_SET_IN_BITMAP (refs_to_check, 0, i, bi)
    {
      aref = VEC_index (mem_ref_p, memory_accesses.refs_list, i);
      if (!refs_independent_p (ref, aref))
	{
	  ret = false;
	  record_indep_loop (loop, aref, false);
	  break;
	}
    }

  BITMAP_FREE (refs_to_check);
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
  /* Unless the reference is stored in the loop, there is nothing to do.  */
  if (!bitmap_bit_p (ref->stored, loop->num))
    return false;

  /* It should be movable.  */
  if (!is_gimple_reg_type (TREE_TYPE (ref->mem))
      || TREE_THIS_VOLATILE (ref->mem)
      || !for_each_index (&ref->mem, may_move_till, loop))
    return false;

  /* If it can trap, it must be always executed in LOOP.  */
  if (tree_could_trap_p (ref->mem)
      && !ref_always_accessed_p (loop, ref))
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
  bitmap refs = VEC_index (bitmap, memory_accesses.all_refs_in_loop,
			   loop->num);
  unsigned i;
  bitmap_iterator bi;
  mem_ref_p ref;

  EXECUTE_IF_AND_COMPL_IN_BITMAP (refs, sm_executed, 0, i, bi)
    {
      ref = VEC_index (mem_ref_p, memory_accesses.refs_list, i);
      if (can_sm_ref_p (loop, ref))
	bitmap_set_bit (refs_to_sm, i);
    }
}

/* Checks whether LOOP (with exits stored in EXITS array) is suitable
   for a store motion optimization (i.e. whether we can insert statement
   on its exits).  */

static bool
loop_suitable_for_sm (struct loop *loop ATTRIBUTE_UNUSED,
		      VEC (edge, heap) *exits)
{
  unsigned i;
  edge ex;

  for (i = 0; VEC_iterate (edge, exits, i, ex); i++)
    if (ex->flags & EDGE_ABNORMAL)
      return false;

  return true;
}

/* Try to perform store motion for all memory references modified inside
   LOOP.  SM_EXECUTED is the bitmap of the memory references for that
   store motion was executed in one of the outer loops.  */

static void
store_motion_loop (struct loop *loop, bitmap sm_executed)
{
  VEC (edge, heap) *exits = get_loop_exit_edges (loop);
  struct loop *subloop;
  bitmap sm_in_loop = BITMAP_ALLOC (NULL);

  if (loop_suitable_for_sm (loop, exits))
    {
      find_refs_for_sm (loop, sm_executed, sm_in_loop);
      hoist_memory_references (loop, sm_in_loop, exits);
    }
  VEC_free (edge, heap, exits);

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
  bsi_commit_edge_inserts ();
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

/* Compute the global information needed by the loop invariant motion pass.  */

static void
tree_ssa_lim_initialize (void)
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

  for (loop = current_loops->tree_root->inner; loop; loop = loop->next)
    fill_always_executed_in (loop, contains_call);

  sbitmap_free (contains_call);
}

/* Cleans up after the invariant motion pass.  */

static void
tree_ssa_lim_finalize (void)
{
  basic_block bb;
  unsigned i;
  bitmap b;
  htab_t h;

  FOR_EACH_BB (bb)
    {
      bb->aux = NULL;
    }

  VEC_free (mem_ref_p, heap, memory_accesses.refs_list);
  htab_delete (memory_accesses.refs);

  for (i = 0; VEC_iterate (bitmap, memory_accesses.refs_in_loop, i, b); i++)
    BITMAP_FREE (b);
  VEC_free (bitmap, heap, memory_accesses.refs_in_loop);

  for (i = 0; VEC_iterate (bitmap, memory_accesses.all_refs_in_loop, i, b); i++)
    BITMAP_FREE (b);
  VEC_free (bitmap, heap, memory_accesses.all_refs_in_loop);

  for (i = 0; VEC_iterate (bitmap, memory_accesses.clobbered_vops, i, b); i++)
    BITMAP_FREE (b);
  VEC_free (bitmap, heap, memory_accesses.clobbered_vops);

  for (i = 0; VEC_iterate (htab_t, memory_accesses.vop_ref_map, i, h); i++)
    htab_delete (h);
  VEC_free (htab_t, heap, memory_accesses.vop_ref_map);

  if (memory_accesses.ttae_cache)
    pointer_map_destroy (memory_accesses.ttae_cache);
}

/* Moves invariants from loops.  Only "expensive" invariants are moved out --
   i.e. those that are likely to be win regardless of the register pressure.  */

void
tree_ssa_lim (void)
{
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
  move_computations ();

  tree_ssa_lim_finalize ();
}
