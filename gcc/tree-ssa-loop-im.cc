/* Loop invariant motion.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "cfganal.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "cfgloop.h"
#include "tree-affine.h"
#include "tree-ssa-propagate.h"
#include "trans-mem.h"
#include "gimple-fold.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-niter.h"
#include "alias.h"
#include "builtins.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "dbgcnt.h"
#include "insn-codes.h"
#include "optabs-tree.h"

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

/* The auxiliary data kept for each statement.  */

struct lim_aux_data
{
  class loop *max_loop;	/* The outermost loop in that the statement
				   is invariant.  */

  class loop *tgt_loop;	/* The loop out of that we want to move the
				   invariant.  */

  class loop *always_executed_in;
				/* The outermost loop for that we are sure
				   the statement is executed if the loop
				   is entered.  */

  unsigned cost;		/* Cost of the computation performed by the
				   statement.  */

  unsigned ref;			/* The simple_mem_ref in this stmt or 0.  */

  vec<gimple *> depends;	/* Vector of statements that must be also
				   hoisted out of the loop when this statement
				   is hoisted; i.e. those that define the
				   operands of the statement and are inside of
				   the MAX_LOOP loop.  */
};

/* Maps statements to their lim_aux_data.  */

static hash_map<gimple *, lim_aux_data *> *lim_aux_data_map;

/* Description of a memory reference location.  */

struct mem_ref_loc
{
  tree *ref;			/* The reference itself.  */
  gimple *stmt;			/* The statement in that it occurs.  */
};


/* Description of a memory reference.  */

class im_mem_ref
{
public:
  unsigned id : 30;		/* ID assigned to the memory reference
				   (its index in memory_accesses.refs_list)  */
  unsigned ref_canonical : 1;   /* Whether mem.ref was canonicalized.  */
  unsigned ref_decomposed : 1;  /* Whether the ref was hashed from mem.  */
  hashval_t hash;		/* Its hash value.  */

  /* The memory access itself and associated caching of alias-oracle
     query meta-data.  We are using mem.ref == error_mark_node for the
     case the reference is represented by its single access stmt
     in accesses_in_loop[0].  */
  ao_ref mem;

  bitmap stored;		/* The set of loops in that this memory location
				   is stored to.  */
  bitmap loaded;		/* The set of loops in that this memory location
				   is loaded from.  */
  vec<mem_ref_loc>		accesses_in_loop;
				/* The locations of the accesses.  */

  /* The following set is computed on demand.  */
  bitmap_head dep_loop;		/* The set of loops in that the memory
				   reference is {in,}dependent in
				   different modes.  */
};

/* We use six bits per loop in the ref->dep_loop bitmap to record
   the dep_kind x dep_state combinations.  */

enum dep_kind { lim_raw, sm_war, sm_waw };
enum dep_state { dep_unknown, dep_independent, dep_dependent };

/* coldest outermost loop for given loop.  */
vec<class loop *> coldest_outermost_loop;
/* hotter outer loop nearest to given loop.  */
vec<class loop *> hotter_than_inner_loop;

/* Populate the loop dependence cache of REF for LOOP, KIND with STATE.  */

static void
record_loop_dependence (class loop *loop, im_mem_ref *ref,
			dep_kind kind, dep_state state)
{
  gcc_assert (state != dep_unknown);
  unsigned bit = 6 * loop->num + kind * 2 + state == dep_dependent ? 1 : 0;
  bitmap_set_bit (&ref->dep_loop, bit);
}

/* Query the loop dependence cache of REF for LOOP, KIND.  */

static dep_state
query_loop_dependence (class loop *loop, im_mem_ref *ref, dep_kind kind)
{
  unsigned first_bit = 6 * loop->num + kind * 2;
  if (bitmap_bit_p (&ref->dep_loop, first_bit))
    return dep_independent;
  else if (bitmap_bit_p (&ref->dep_loop, first_bit + 1))
    return dep_dependent;
  return dep_unknown;
}

/* Mem_ref hashtable helpers.  */

struct mem_ref_hasher : nofree_ptr_hash <im_mem_ref>
{
  typedef ao_ref *compare_type;
  static inline hashval_t hash (const im_mem_ref *);
  static inline bool equal (const im_mem_ref *, const ao_ref *);
};

/* A hash function for class im_mem_ref object OBJ.  */

inline hashval_t
mem_ref_hasher::hash (const im_mem_ref *mem)
{
  return mem->hash;
}

/* An equality function for class im_mem_ref object MEM1 with
   memory reference OBJ2.  */

inline bool
mem_ref_hasher::equal (const im_mem_ref *mem1, const ao_ref *obj2)
{
  if (obj2->max_size_known_p ())
    return (mem1->ref_decomposed
	    && ((TREE_CODE (mem1->mem.base) == MEM_REF
		 && TREE_CODE (obj2->base) == MEM_REF
		 && operand_equal_p (TREE_OPERAND (mem1->mem.base, 0),
				     TREE_OPERAND (obj2->base, 0), 0)
		 && known_eq (mem_ref_offset (mem1->mem.base) * BITS_PER_UNIT + mem1->mem.offset,
			      mem_ref_offset (obj2->base) * BITS_PER_UNIT + obj2->offset))
		|| (operand_equal_p (mem1->mem.base, obj2->base, 0)
		    && known_eq (mem1->mem.offset, obj2->offset)))
	    && known_eq (mem1->mem.size, obj2->size)
	    && known_eq (mem1->mem.max_size, obj2->max_size)
	    && mem1->mem.volatile_p == obj2->volatile_p
	    && (mem1->mem.ref_alias_set == obj2->ref_alias_set
		/* We are not canonicalizing alias-sets but for the
		   special-case we didn't canonicalize yet and the
		   incoming ref is a alias-set zero MEM we pick
		   the correct one already.  */
		|| (!mem1->ref_canonical
		    && (TREE_CODE (obj2->ref) == MEM_REF
			|| TREE_CODE (obj2->ref) == TARGET_MEM_REF)
		    && obj2->ref_alias_set == 0)
		/* Likewise if there's a canonical ref with alias-set zero.  */
		|| (mem1->ref_canonical && mem1->mem.ref_alias_set == 0))
	    && types_compatible_p (TREE_TYPE (mem1->mem.ref),
				   TREE_TYPE (obj2->ref)));
  else
    return operand_equal_p (mem1->mem.ref, obj2->ref, 0);
}


/* Description of memory accesses in loops.  */

static struct
{
  /* The hash table of memory references accessed in loops.  */
  hash_table<mem_ref_hasher> *refs;

  /* The list of memory references.  */
  vec<im_mem_ref *> refs_list;

  /* The set of memory references accessed in each loop.  */
  vec<bitmap_head> refs_loaded_in_loop;

  /* The set of memory references stored in each loop.  */
  vec<bitmap_head> refs_stored_in_loop;

  /* The set of memory references stored in each loop, including subloops .  */
  vec<bitmap_head> all_refs_stored_in_loop;

  /* Cache for expanding memory addresses.  */
  hash_map<tree, name_expansion *> *ttae_cache;
} memory_accesses;

/* Obstack for the bitmaps in the above data structures.  */
static bitmap_obstack lim_bitmap_obstack;
static obstack mem_ref_obstack;

static bool ref_indep_loop_p (class loop *, im_mem_ref *, dep_kind);
static bool ref_always_accessed_p (class loop *, im_mem_ref *, bool);
static bool refs_independent_p (im_mem_ref *, im_mem_ref *, bool = true);

/* Minimum cost of an expensive expression.  */
#define LIM_EXPENSIVE ((unsigned) param_lim_expensive)

/* The outermost loop for which execution of the header guarantees that the
   block will be executed.  */
#define ALWAYS_EXECUTED_IN(BB) ((class loop *) (BB)->aux)
#define SET_ALWAYS_EXECUTED_IN(BB, VAL) ((BB)->aux = (void *) (VAL))

/* ID of the shared unanalyzable mem.  */
#define UNANALYZABLE_MEM_ID 0

/* Whether the reference was analyzable.  */
#define MEM_ANALYZABLE(REF) ((REF)->id != UNANALYZABLE_MEM_ID)

static struct lim_aux_data *
init_lim_data (gimple *stmt)
{
  lim_aux_data *p = XCNEW (struct lim_aux_data);
  lim_aux_data_map->put (stmt, p);

  return p;
}

static struct lim_aux_data *
get_lim_data (gimple *stmt)
{
  lim_aux_data **p = lim_aux_data_map->get (stmt);
  if (!p)
    return NULL;

  return *p;
}

/* Releases the memory occupied by DATA.  */

static void
free_lim_aux_data (struct lim_aux_data *data)
{
  data->depends.release ();
  free (data);
}

static void
clear_lim_data (gimple *stmt)
{
  lim_aux_data **p = lim_aux_data_map->get (stmt);
  if (!p)
    return;

  free_lim_aux_data (*p);
  *p = NULL;
}


/* The possibilities of statement movement.  */
enum move_pos
  {
    MOVE_IMPOSSIBLE,		/* No movement -- side effect expression.  */
    MOVE_PRESERVE_EXECUTION,	/* Must not cause the non-executed statement
				   become executed -- memory accesses, ... */
    MOVE_POSSIBLE		/* Unlimited movement.  */
  };


/* If it is possible to hoist the statement STMT unconditionally,
   returns MOVE_POSSIBLE.
   If it is possible to hoist the statement STMT, but we must avoid making
   it executed if it would not be executed in the original program (e.g.
   because it may trap), return MOVE_PRESERVE_EXECUTION.
   Otherwise return MOVE_IMPOSSIBLE.  */

static enum move_pos
movement_possibility_1 (gimple *stmt)
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
      || stmt_could_throw_p (cfun, stmt))
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

  if (is_gimple_assign (stmt))
    {
      auto code = gimple_assign_rhs_code (stmt);
      tree type = TREE_TYPE (gimple_assign_rhs1 (stmt));
      /* For shifts and rotates and possibly out-of-bound shift operands
	 we currently cannot rewrite them into something unconditionally
	 well-defined.  */
      if ((code == LSHIFT_EXPR
	   || code == RSHIFT_EXPR
	   || code == LROTATE_EXPR
	   || code == RROTATE_EXPR)
	  && (TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST
	      /* We cannot use ranges at 'stmt' here.  */
	      || wi::ltu_p (wi::to_wide (gimple_assign_rhs2 (stmt)),
			    element_precision (type))))
	ret = MOVE_PRESERVE_EXECUTION;
    }

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

static enum move_pos
movement_possibility (gimple *stmt)
{
  enum move_pos pos = movement_possibility_1 (stmt);
  if (pos == MOVE_POSSIBLE)
    {
      use_operand_p use_p;
      ssa_op_iter ssa_iter;
      FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, ssa_iter, SSA_OP_USE)
	if (TREE_CODE (USE_FROM_PTR (use_p)) == SSA_NAME
	    && ssa_name_maybe_undef_p (USE_FROM_PTR (use_p)))
	  return MOVE_PRESERVE_EXECUTION;
    }
  return pos;
}


/* Compare the profile count inequality of bb and loop's preheader, it is
   three-state as stated in profile-count.h, FALSE is returned if inequality
   cannot be decided.  */
bool
bb_colder_than_loop_preheader (basic_block bb, class loop *loop)
{
  gcc_assert (bb && loop);
  return bb->count < loop_preheader_edge (loop)->src->count;
}

/* Check coldest loop between OUTERMOST_LOOP and LOOP by comparing profile
   count.
  It does three steps check:
  1) Check whether CURR_BB is cold in it's own loop_father, if it is cold, just
  return NULL which means it should not be moved out at all;
  2) CURR_BB is NOT cold, check if pre-computed COLDEST_LOOP is outside of
  OUTERMOST_LOOP, if it is inside of OUTERMOST_LOOP, return the COLDEST_LOOP;
  3) If COLDEST_LOOP is outside of OUTERMOST_LOOP, check whether there is a
  hotter loop between OUTERMOST_LOOP and loop in pre-computed
  HOTTER_THAN_INNER_LOOP, return it's nested inner loop, otherwise return
  OUTERMOST_LOOP.
  At last, the coldest_loop is inside of OUTERMOST_LOOP, just return it as
  the hoist target.  */

static class loop *
get_coldest_out_loop (class loop *outermost_loop, class loop *loop,
		      basic_block curr_bb)
{
  gcc_assert (outermost_loop == loop
	      || flow_loop_nested_p (outermost_loop, loop));

  /* If bb_colder_than_loop_preheader returns false due to three-state
    comparision, OUTERMOST_LOOP is returned finally to preserve the behavior.
    Otherwise, return the coldest loop between OUTERMOST_LOOP and LOOP.  */
  if (curr_bb && bb_colder_than_loop_preheader (curr_bb, loop))
    return NULL;

  class loop *coldest_loop = coldest_outermost_loop[loop->num];
  if (loop_depth (coldest_loop) < loop_depth (outermost_loop))
    {
      class loop *hotter_loop = hotter_than_inner_loop[loop->num];
      if (!hotter_loop
	  || loop_depth (hotter_loop) < loop_depth (outermost_loop))
	return outermost_loop;

      /*  hotter_loop is between OUTERMOST_LOOP and LOOP like:
	[loop tree root, ..., coldest_loop, ..., outermost_loop, ...,
	hotter_loop, second_coldest_loop, ..., loop]
	return second_coldest_loop to be the hoist target.  */
      class loop *aloop;
      for (aloop = hotter_loop->inner; aloop; aloop = aloop->next)
	if (aloop == loop || flow_loop_nested_p (aloop, loop))
	  return aloop;
    }
  return coldest_loop;
}

/* Suppose that operand DEF is used inside the LOOP.  Returns the outermost
   loop to that we could move the expression using DEF if it did not have
   other operands, i.e. the outermost loop enclosing LOOP in that the value
   of DEF is invariant.  */

static class loop *
outermost_invariant_loop (tree def, class loop *loop)
{
  gimple *def_stmt;
  basic_block def_bb;
  class loop *max_loop;
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
add_dependency (tree def, struct lim_aux_data *data, class loop *loop,
		bool add_cost)
{
  gimple *def_stmt = SSA_NAME_DEF_STMT (def);
  basic_block def_bb = gimple_bb (def_stmt);
  class loop *max_loop;
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

  data->depends.safe_push (def_stmt);

  return true;
}

/* Returns an estimate for a cost of statement STMT.  The values here
   are just ad-hoc constants, similar to costs for inlining.  */

static unsigned
stmt_cost (gimple *stmt)
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
      if (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_CONSTANT_P))
	return 0;

      return LIM_EXPENSIVE;
    }

  /* Hoisting memory references out should almost surely be a win.  */
  if (gimple_references_memory_p (stmt))
    return LIM_EXPENSIVE;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return 1;

  enum tree_code code = gimple_assign_rhs_code (stmt);
  switch (code)
    {
    case MULT_EXPR:
    case WIDEN_MULT_EXPR:
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case DOT_PROD_EXPR:
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

    case COND_EXPR:
    case VEC_COND_EXPR:
      /* Conditionals are expensive.  */
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
      /* Comparisons are usually expensive.  */
      if (TREE_CODE_CLASS (code) == tcc_comparison)
	return LIM_EXPENSIVE;
      return 1;
    }
}

/* Finds the outermost loop between OUTER and LOOP in that the memory reference
   REF is independent.  If REF is not independent in LOOP, NULL is returned
   instead.  */

static class loop *
outermost_indep_loop (class loop *outer, class loop *loop, im_mem_ref *ref)
{
  class loop *aloop;

  if (ref->stored && bitmap_bit_p (ref->stored, loop->num))
    return NULL;

  for (aloop = outer;
       aloop != loop;
       aloop = superloop_at_depth (loop, loop_depth (aloop) + 1))
    if ((!ref->stored || !bitmap_bit_p (ref->stored, aloop->num))
	&& ref_indep_loop_p (aloop, ref, lim_raw))
      return aloop;

  if (ref_indep_loop_p (loop, ref, lim_raw))
    return loop;
  else
    return NULL;
}

/* If there is a simple load or store to a memory reference in STMT, returns
   the location of the memory reference, and sets IS_STORE according to whether
   it is a store or load.  Otherwise, returns NULL.  */

static tree *
simple_mem_ref_in_stmt (gimple *stmt, bool *is_store)
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

/* From a controlling predicate in DOM determine the arguments from
   the PHI node PHI that are chosen if the predicate evaluates to
   true and false and store them to *TRUE_ARG_P and *FALSE_ARG_P if
   they are non-NULL.  Returns true if the arguments can be determined,
   else return false.  */

static bool
extract_true_false_args_from_phi (basic_block dom, gphi *phi,
				  tree *true_arg_p, tree *false_arg_p)
{
  edge te, fe;
  if (! extract_true_false_controlled_edges (dom, gimple_bb (phi),
					     &te, &fe))
    return false;

  if (true_arg_p)
    *true_arg_p = PHI_ARG_DEF (phi, te->dest_idx);
  if (false_arg_p)
    *false_arg_p = PHI_ARG_DEF (phi, fe->dest_idx);

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
determine_max_movement (gimple *stmt, bool must_preserve_exec)
{
  basic_block bb = gimple_bb (stmt);
  class loop *loop = bb->loop_father;
  class loop *level;
  struct lim_aux_data *lim_data = get_lim_data (stmt);
  tree val;
  ssa_op_iter iter;

  if (must_preserve_exec)
    level = ALWAYS_EXECUTED_IN (bb);
  else
    level = superloop_at_depth (loop, 1);
  lim_data->max_loop = get_coldest_out_loop (level, loop, bb);
  if (!lim_data->max_loop)
    return false;

  if (gphi *phi = dyn_cast <gphi *> (stmt))
    {
      use_operand_p use_p;
      unsigned min_cost = UINT_MAX;
      unsigned total_cost = 0;
      struct lim_aux_data *def_data;

      /* We will end up promoting dependencies to be unconditionally
	 evaluated.  For this reason the PHI cost (and thus the
	 cost we remove from the loop by doing the invariant motion)
	 is that of the cheapest PHI argument dependency chain.  */
      FOR_EACH_PHI_ARG (use_p, phi, iter, SSA_OP_USE)
	{
	  val = USE_FROM_PTR (use_p);

	  if (TREE_CODE (val) != SSA_NAME)
	    {
	      /* Assign const 1 to constants.  */
	      min_cost = MIN (min_cost, 1);
	      total_cost += 1;
	      continue;
	    }
	  if (!add_dependency (val, lim_data, loop, false))
	    return false;

	  gimple *def_stmt = SSA_NAME_DEF_STMT (val);
	  if (gimple_bb (def_stmt)
	      && gimple_bb (def_stmt)->loop_father == loop)
	    {
	      def_data = get_lim_data (def_stmt);
	      if (def_data)
		{
		  min_cost = MIN (min_cost, def_data->cost);
		  total_cost += def_data->cost;
		}
	    }
	}

      min_cost = MIN (min_cost, total_cost);
      lim_data->cost += min_cost;

      if (gimple_phi_num_args (phi) > 1)
	{
	  basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb);
	  gimple *cond;
	  if (gsi_end_p (gsi_last_bb (dom)))
	    return false;
	  cond = gsi_stmt (gsi_last_bb (dom));
	  if (gimple_code (cond) != GIMPLE_COND)
	    return false;
	  /* Verify that this is an extended form of a diamond and
	     the PHI arguments are completely controlled by the
	     predicate in DOM.  */
	  if (!extract_true_false_args_from_phi (dom, phi, NULL, NULL))
	    return false;

	/* Check if one of the depedent statement is a vector compare whether
	   the target supports it,  otherwise it's invalid to hoist it out of
	   the gcond it belonged to.  */
	if (VECTOR_TYPE_P (TREE_TYPE (gimple_cond_lhs (cond))))
	  {
	    tree type = TREE_TYPE (gimple_cond_lhs (cond));
	    auto code = gimple_cond_code (cond);
	    if (!target_supports_op_p (type, code, optab_vector))
	      return false;
	  }

	  /* Fold in dependencies and cost of the condition.  */
	  FOR_EACH_SSA_TREE_OPERAND (val, cond, iter, SSA_OP_USE)
	    {
	      if (!add_dependency (val, lim_data, loop, false))
		return false;
	      def_data = get_lim_data (SSA_NAME_DEF_STMT (val));
	      if (def_data)
		lim_data->cost += def_data->cost;
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

  /* A stmt that receives abnormal edges cannot be hoisted.  */
  if (is_a <gcall *> (stmt)
      && (gimple_call_flags (stmt) & ECF_RETURNS_TWICE))
    return false;

  FOR_EACH_SSA_TREE_OPERAND (val, stmt, iter, SSA_OP_USE)
    if (!add_dependency (val, lim_data, loop, true))
      return false;

  if (gimple_vuse (stmt))
    {
      im_mem_ref *ref
	= lim_data ? memory_accesses.refs_list[lim_data->ref] : NULL;
      if (ref
	  && MEM_ANALYZABLE (ref))
	{
	  lim_data->max_loop = outermost_indep_loop (lim_data->max_loop,
						     loop, ref);
	  if (!lim_data->max_loop)
	    return false;
	}
      else if (! add_dependency (gimple_vuse (stmt), lim_data, loop, false))
	return false;
    }

  lim_data->cost += stmt_cost (stmt);

  return true;
}

/* Suppose that some statement in ORIG_LOOP is hoisted to the loop LEVEL,
   and that one of the operands of this statement is computed by STMT.
   Ensure that STMT (together with all the statements that define its
   operands) is hoisted at least out of the loop LEVEL.  */

static void
set_level (gimple *stmt, class loop *orig_loop, class loop *level)
{
  class loop *stmt_loop = gimple_bb (stmt)->loop_father;
  struct lim_aux_data *lim_data;
  gimple *dep_stmt;
  unsigned i;

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
  FOR_EACH_VEC_ELT (lim_data->depends, i, dep_stmt)
    set_level (dep_stmt, orig_loop, level);
}

/* Determines an outermost loop from that we want to hoist the statement STMT.
   For now we chose the outermost possible loop.  TODO -- use profiling
   information to set it more sanely.  */

static void
set_profitable_level (gimple *stmt)
{
  set_level (stmt, gimple_bb (stmt)->loop_father, get_lim_data (stmt)->max_loop);
}

/* Returns true if STMT is a call that has side effects.  */

static bool
nonpure_call_p (gimple *stmt)
{
  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  return gimple_has_side_effects (stmt);
}

/* Rewrite a/b to a*(1/b).  Return the invariant stmt to process.  */

static gimple *
rewrite_reciprocal (gimple_stmt_iterator *bsi)
{
  gassign *stmt, *stmt1, *stmt2;
  tree name, lhs, type;
  tree real_one;
  gimple_stmt_iterator gsi;

  stmt = as_a <gassign *> (gsi_stmt (*bsi));
  lhs = gimple_assign_lhs (stmt);
  type = TREE_TYPE (lhs);

  real_one = build_one_cst (type);

  name = make_temp_ssa_name (type, NULL, "reciptmp");
  stmt1 = gimple_build_assign (name, RDIV_EXPR, real_one,
			       gimple_assign_rhs2 (stmt));
  stmt2 = gimple_build_assign (lhs, MULT_EXPR, name,
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

static gimple *
rewrite_bittest (gimple_stmt_iterator *bsi)
{
  gassign *stmt;
  gimple *stmt1;
  gassign *stmt2;
  gimple *use_stmt;
  gcond *cond_stmt;
  tree lhs, name, t, a, b;
  use_operand_p use;

  stmt = as_a <gassign *> (gsi_stmt (*bsi));
  lhs = gimple_assign_lhs (stmt);

  /* Verify that the single use of lhs is a comparison against zero.  */
  if (TREE_CODE (lhs) != SSA_NAME
      || !single_imm_use (lhs, &use, &use_stmt))
    return stmt;
  cond_stmt = dyn_cast <gcond *> (use_stmt);
  if (!cond_stmt)
    return stmt;
  if (gimple_cond_lhs (cond_stmt) != lhs
      || (gimple_cond_code (cond_stmt) != NE_EXPR
	  && gimple_cond_code (cond_stmt) != EQ_EXPR)
      || !integer_zerop (gimple_cond_rhs (cond_stmt)))
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
      gimple_cond_set_rhs (cond_stmt,
			   build_int_cst_type (TREE_TYPE (name),
					       0));

      /* Don't use gsi_replace here, none of the new assignments sets
	 the variable originally set in stmt.  Move bsi to stmt1, and
	 then remove the original stmt, so that we get a chance to
	 retain debug info for it.  */
      rsi = *bsi;
      gsi_insert_before (bsi, stmt1, GSI_NEW_STMT);
      gsi_insert_before (&rsi, stmt2, GSI_SAME_STMT);
      gimple *to_release = gsi_stmt (rsi);
      gsi_remove (&rsi, true);
      release_defs (to_release);

      return stmt1;
    }

  return stmt;
}

/* Determine the outermost loops in that statements in basic block BB are
   invariant, and record them to the LIM_DATA associated with the
   statements.  */

static void
compute_invariantness (basic_block bb)
{
  enum move_pos pos;
  gimple_stmt_iterator bsi;
  gimple *stmt;
  bool maybe_never = ALWAYS_EXECUTED_IN (bb) == NULL;
  class loop *outermost = ALWAYS_EXECUTED_IN (bb);
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

	lim_data = get_lim_data (stmt);
	if (! lim_data)
	  lim_data = init_lim_data (stmt);
	lim_data->always_executed_in = outermost;

	if (!determine_max_movement (stmt, false))
	  {
	    lim_data->max_loop = NULL;
	    continue;
	  }

	if (dump_file && (dump_flags & TDF_DETAILS))
	  {
	    print_gimple_stmt (dump_file, stmt, 2);
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
	      struct lim_aux_data *lim_data = get_lim_data (stmt);
	      if (! lim_data)
		lim_data = init_lim_data (stmt);
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
	  class loop *ol1 = outermost_invariant_loop (op1,
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

      lim_data = get_lim_data (stmt);
      if (! lim_data)
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
	  print_gimple_stmt (dump_file, stmt, 2);
	  fprintf (dump_file, "  invariant up to level %d, cost %d.\n\n",
		   loop_depth (lim_data->max_loop),
		   lim_data->cost);
	}

      if (lim_data->cost >= LIM_EXPENSIVE)
	set_profitable_level (stmt);
    }
}

/* Hoist the statements in basic block BB out of the loops prescribed by
   data stored in LIM_DATA structures associated with each statement.  Callback
   for walk_dominator_tree.  */

unsigned int
move_computations_worker (basic_block bb)
{
  class loop *level;
  unsigned cost = 0;
  struct lim_aux_data *lim_data;
  unsigned int todo = 0;

  if (!loop_outer (bb->loop_father))
    return todo;

  for (gphi_iterator bsi = gsi_start_phis (bb); !gsi_end_p (bsi); )
    {
      gassign *new_stmt;
      gphi *stmt = bsi.phi ();

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
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "(cost %u) out of loop %d.\n\n",
		   cost, level->num);
	}

      if (gimple_phi_num_args (stmt) == 1)
	{
	  tree arg = PHI_ARG_DEF (stmt, 0);
	  new_stmt = gimple_build_assign (gimple_phi_result (stmt),
					  TREE_CODE (arg), arg);
	}
      else
	{
	  basic_block dom = get_immediate_dominator (CDI_DOMINATORS, bb);
	  gimple *cond = gsi_stmt (gsi_last_bb (dom));
	  tree arg0 = NULL_TREE, arg1 = NULL_TREE, t;
	  /* Get the PHI arguments corresponding to the true and false
	     edges of COND.  */
	  extract_true_false_args_from_phi (dom, stmt, &arg0, &arg1);
	  gcc_assert (arg0 && arg1);
	  /* For `bool_val != 0`, reuse bool_val. */
	  if (gimple_cond_code (cond) == NE_EXPR
	      && integer_zerop (gimple_cond_rhs (cond))
	      && types_compatible_p (TREE_TYPE (gimple_cond_lhs (cond)),
				     boolean_type_node))
	    {
	      t = gimple_cond_lhs (cond);
	    }
	  else
	    {
	      t = make_ssa_name (boolean_type_node);
	      new_stmt = gimple_build_assign (t, gimple_cond_code (cond),
					      gimple_cond_lhs (cond),
					      gimple_cond_rhs (cond));
	      gsi_insert_on_edge (loop_preheader_edge (level), new_stmt);
	    }
	  new_stmt = gimple_build_assign (gimple_phi_result (stmt),
					  COND_EXPR, t, arg0, arg1);
	  todo |= TODO_cleanup_cfg;
	}
      if (!ALWAYS_EXECUTED_IN (bb)
	  || (ALWAYS_EXECUTED_IN (bb) != level
	      && !flow_loop_nested_p (ALWAYS_EXECUTED_IN (bb), level)))
	reset_flow_sensitive_info (gimple_assign_lhs (new_stmt));
      gsi_insert_on_edge (loop_preheader_edge (level), new_stmt);
      remove_phi_node (&bsi, false);
    }

  for (gimple_stmt_iterator bsi = gsi_start_bb (bb); !gsi_end_p (bsi); )
    {
      edge e;

      gimple *stmt = gsi_stmt (bsi);

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
	{
	  gsi_next (&bsi);
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Moving statement\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	  fprintf (dump_file, "(cost %u) out of loop %d.\n\n",
		   cost, level->num);
	}

      e = loop_preheader_edge (level);
      gcc_assert (!gimple_vdef (stmt));
      if (gimple_vuse (stmt))
	{
	  /* The new VUSE is the one from the virtual PHI in the loop
	     header or the one already present.  */
	  gphi_iterator gsi2;
	  for (gsi2 = gsi_start_phis (e->dest);
	       !gsi_end_p (gsi2); gsi_next (&gsi2))
	    {
	      gphi *phi = gsi2.phi ();
	      if (virtual_operand_p (gimple_phi_result (phi)))
		{
		  SET_USE (gimple_vuse_op (stmt),
			   PHI_ARG_DEF_FROM_EDGE (phi, e));
		  break;
		}
	    }
	}
      gsi_remove (&bsi, false);
      if (gimple_has_lhs (stmt)
	  && TREE_CODE (gimple_get_lhs (stmt)) == SSA_NAME
	  && (!ALWAYS_EXECUTED_IN (bb)
	      || !(ALWAYS_EXECUTED_IN (bb) == level
		   || flow_loop_nested_p (ALWAYS_EXECUTED_IN (bb), level))))
	reset_flow_sensitive_info (gimple_get_lhs (stmt));
      /* In case this is a stmt that is not unconditionally executed
         when the target loop header is executed and the stmt may
	 invoke undefined integer or pointer overflow rewrite it to
	 unsigned arithmetic.  */
      if (is_gimple_assign (stmt)
	  && INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_lhs (stmt)))
	  && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (gimple_assign_lhs (stmt)))
	  && arith_code_with_undefined_signed_overflow
	       (gimple_assign_rhs_code (stmt))
	  && (!ALWAYS_EXECUTED_IN (bb)
	      || !(ALWAYS_EXECUTED_IN (bb) == level
		   || flow_loop_nested_p (ALWAYS_EXECUTED_IN (bb), level))))
	gsi_insert_seq_on_edge (e, rewrite_to_defined_overflow (stmt));
      else
	gsi_insert_on_edge (e, stmt);
    }

  return todo;
}

/* Checks whether the statement defining variable *INDEX can be hoisted
   out of the loop passed in DATA.  Callback for for_each_index.  */

static bool
may_move_till (tree ref, tree *index, void *data)
{
  class loop *loop = (class loop *) data, *max_loop;

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
force_move_till_op (tree op, class loop *orig_loop, class loop *loop)
{
  gimple *stmt;

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
  class loop *loop;
  class loop *orig_loop;
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

/* A function to free the mem_ref object OBJ.  */

static void
memref_free (class im_mem_ref *mem)
{
  mem->accesses_in_loop.release ();
}

/* Allocates and returns a memory reference description for MEM whose hash
   value is HASH and id is ID.  */

static im_mem_ref *
mem_ref_alloc (ao_ref *mem, unsigned hash, unsigned id)
{
  im_mem_ref *ref = XOBNEW (&mem_ref_obstack, class im_mem_ref);
  if (mem)
    ref->mem = *mem;
  else
    ao_ref_init (&ref->mem, error_mark_node);
  ref->id = id;
  ref->ref_canonical = false;
  ref->ref_decomposed = false;
  ref->hash = hash;
  ref->stored = NULL;
  ref->loaded = NULL;
  bitmap_initialize (&ref->dep_loop, &lim_bitmap_obstack);
  ref->accesses_in_loop.create (1);

  return ref;
}

/* Records memory reference location *LOC in LOOP to the memory reference
   description REF.  The reference occurs in statement STMT.  */

static void
record_mem_ref_loc (im_mem_ref *ref, gimple *stmt, tree *loc)
{
  mem_ref_loc aref;
  aref.stmt = stmt;
  aref.ref = loc;
  ref->accesses_in_loop.safe_push (aref);
}

/* Set the LOOP bit in REF stored bitmap and allocate that if
   necessary.  Return whether a bit was changed.  */

static bool
set_ref_stored_in_loop (im_mem_ref *ref, class loop *loop)
{
  if (!ref->stored)
    ref->stored = BITMAP_ALLOC (&lim_bitmap_obstack);
  return bitmap_set_bit (ref->stored, loop->num);
}

/* Marks reference REF as stored in LOOP.  */

static void
mark_ref_stored (im_mem_ref *ref, class loop *loop)
{
  while (loop != current_loops->tree_root
	 && set_ref_stored_in_loop (ref, loop))
    loop = loop_outer (loop);
}

/* Set the LOOP bit in REF loaded bitmap and allocate that if
   necessary.  Return whether a bit was changed.  */

static bool
set_ref_loaded_in_loop (im_mem_ref *ref, class loop *loop)
{
  if (!ref->loaded)
    ref->loaded = BITMAP_ALLOC (&lim_bitmap_obstack);
  return bitmap_set_bit (ref->loaded, loop->num);
}

/* Marks reference REF as loaded in LOOP.  */

static void
mark_ref_loaded (im_mem_ref *ref, class loop *loop)
{
  while (loop != current_loops->tree_root
	 && set_ref_loaded_in_loop (ref, loop))
    loop = loop_outer (loop);
}

/* Gathers memory references in statement STMT in LOOP, storing the
   information about them in the memory_accesses structure.  Marks
   the vops accessed through unrecognized statements there as
   well.  */

static void
gather_mem_refs_stmt (class loop *loop, gimple *stmt)
{
  tree *mem = NULL;
  hashval_t hash;
  im_mem_ref **slot;
  im_mem_ref *ref;
  bool is_stored;
  unsigned id;

  if (!gimple_vuse (stmt))
    return;

  mem = simple_mem_ref_in_stmt (stmt, &is_stored);
  if (!mem && is_gimple_assign (stmt))
    {
      /* For aggregate copies record distinct references but use them
	 only for disambiguation purposes.  */
      id = memory_accesses.refs_list.length ();
      ref = mem_ref_alloc (NULL, 0, id);
      memory_accesses.refs_list.safe_push (ref);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Unhandled memory reference %u: ", id);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      record_mem_ref_loc (ref, stmt, mem);
      is_stored = gimple_vdef (stmt);
    }
  else if (!mem)
    {
      /* We use the shared mem_ref for all unanalyzable refs.  */
      id = UNANALYZABLE_MEM_ID;
      ref = memory_accesses.refs_list[id];
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Unanalyzed memory reference %u: ", id);
	  print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	}
      is_stored = gimple_vdef (stmt);
    }
  else
    {
      /* We are looking for equal refs that might differ in structure
         such as a.b vs. MEM[&a + 4].  So we key off the ao_ref but
	 make sure we can canonicalize the ref in the hashtable if
	 non-operand_equal_p refs are found.  For the lookup we mark
	 the case we want strict equality with aor.max_size == -1.  */
      ao_ref aor;
      ao_ref_init (&aor, *mem);
      ao_ref_base (&aor);
      ao_ref_alias_set (&aor);
      HOST_WIDE_INT offset, size, max_size;
      poly_int64 saved_maxsize = aor.max_size, mem_off;
      tree mem_base;
      bool ref_decomposed;
      if (aor.max_size_known_p ()
	  && aor.offset.is_constant (&offset)
	  && aor.size.is_constant (&size)
	  && aor.max_size.is_constant (&max_size)
	  && size == max_size
	  && (size % BITS_PER_UNIT) == 0
	  /* We're canonicalizing to a MEM where TYPE_SIZE specifies the
	     size.  Make sure this is consistent with the extraction.  */
	  && poly_int_tree_p (TYPE_SIZE (TREE_TYPE (*mem)))
	  && known_eq (wi::to_poly_offset (TYPE_SIZE (TREE_TYPE (*mem))),
		       aor.size)
	  && (mem_base = get_addr_base_and_unit_offset (aor.ref, &mem_off)))
	{
	  ref_decomposed = true;
	  tree base = ao_ref_base (&aor);
	  poly_int64 moffset;
	  HOST_WIDE_INT mcoffset;
	  if (TREE_CODE (base) == MEM_REF
	      && (mem_ref_offset (base) * BITS_PER_UNIT + offset).to_shwi (&moffset)
	      && moffset.is_constant (&mcoffset))
	    {
	      hash = iterative_hash_expr (TREE_OPERAND (base, 0), 0);
	      hash = iterative_hash_host_wide_int (mcoffset, hash);
	    }
	  else
	    {
	      hash = iterative_hash_expr (base, 0);
	      hash = iterative_hash_host_wide_int (offset, hash);
	    }
	  hash = iterative_hash_host_wide_int (size, hash);
	}
      else
	{
	  ref_decomposed = false;
	  hash = iterative_hash_expr (aor.ref, 0);
	  aor.max_size = -1;
	}
      slot = memory_accesses.refs->find_slot_with_hash (&aor, hash, INSERT);
      aor.max_size = saved_maxsize;
      if (*slot)
	{
	  if (!(*slot)->ref_canonical
	      && !operand_equal_p (*mem, (*slot)->mem.ref, 0))
	    {
	      /* If we didn't yet canonicalize the hashtable ref (which
	         we'll end up using for code insertion) and hit a second
		 equal ref that is not structurally equivalent create
		 a canonical ref which is a bare MEM_REF.  */
	      if (TREE_CODE (*mem) == MEM_REF
		  || TREE_CODE (*mem) == TARGET_MEM_REF)
		{
		  (*slot)->mem.ref = *mem;
		  (*slot)->mem.base_alias_set = ao_ref_base_alias_set (&aor);
		}
	      else
		{
		  tree ref_alias_type = reference_alias_ptr_type (*mem);
		  unsigned int ref_align = get_object_alignment (*mem);
		  tree ref_type = TREE_TYPE (*mem);
		  tree tmp = build1 (ADDR_EXPR, ptr_type_node,
				     unshare_expr (mem_base));
		  if (TYPE_ALIGN (ref_type) != ref_align)
		    ref_type = build_aligned_type (ref_type, ref_align);
		  tree new_ref
		    = fold_build2 (MEM_REF, ref_type, tmp,
				   build_int_cst (ref_alias_type, mem_off));
		  if ((*slot)->mem.volatile_p)
		    TREE_THIS_VOLATILE (new_ref) = 1;
		  (*slot)->mem.ref = new_ref;
		  /* Make sure the recorded base and offset are consistent
		     with the newly built ref.  */
		  if (TREE_CODE (TREE_OPERAND (new_ref, 0)) == ADDR_EXPR)
		    ;
		  else
		    {
		      (*slot)->mem.base = new_ref;
		      (*slot)->mem.offset = 0;
		    }
		  gcc_checking_assert (TREE_CODE ((*slot)->mem.ref) == MEM_REF
				       && is_gimple_mem_ref_addr
				            (TREE_OPERAND ((*slot)->mem.ref,
							   0)));
		  (*slot)->mem.base_alias_set = (*slot)->mem.ref_alias_set;
		}
	      (*slot)->ref_canonical = true;
	    }
	  ref = *slot;
	  id = ref->id;
	}
      else
	{
	  id = memory_accesses.refs_list.length ();
	  ref = mem_ref_alloc (&aor, hash, id);
	  ref->ref_decomposed = ref_decomposed;
	  memory_accesses.refs_list.safe_push (ref);
	  *slot = ref;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Memory reference %u: ", id);
	      print_generic_expr (dump_file, ref->mem.ref, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	}

      record_mem_ref_loc (ref, stmt, mem);
    }
  if (is_stored)
    {
      bitmap_set_bit (&memory_accesses.refs_stored_in_loop[loop->num], ref->id);
      mark_ref_stored (ref, loop);
    }
  /* A not simple memory op is also a read when it is a write.  */
  if (!is_stored || id == UNANALYZABLE_MEM_ID
      || ref->mem.ref == error_mark_node)
    {
      bitmap_set_bit (&memory_accesses.refs_loaded_in_loop[loop->num], ref->id);
      mark_ref_loaded (ref, loop);
    }
  init_lim_data (stmt)->ref = ref->id;
  return;
}

static unsigned *bb_loop_postorder;

/* qsort sort function to sort blocks after their loop fathers postorder.  */

static int
sort_bbs_in_loop_postorder_cmp (const void *bb1_, const void *bb2_,
				void *bb_loop_postorder_)
{
  unsigned *bb_loop_postorder = (unsigned *)bb_loop_postorder_;
  basic_block bb1 = *(const basic_block *)bb1_;
  basic_block bb2 = *(const basic_block *)bb2_;
  class loop *loop1 = bb1->loop_father;
  class loop *loop2 = bb2->loop_father;
  if (loop1->num == loop2->num)
    return bb1->index - bb2->index;
  return bb_loop_postorder[loop1->num] < bb_loop_postorder[loop2->num] ? -1 : 1;
}

/* qsort sort function to sort ref locs after their loop fathers postorder.  */

static int
sort_locs_in_loop_postorder_cmp (const void *loc1_, const void *loc2_,
				 void *bb_loop_postorder_)
{
  unsigned *bb_loop_postorder = (unsigned *)bb_loop_postorder_;
  const mem_ref_loc *loc1 = (const mem_ref_loc *)loc1_;
  const mem_ref_loc *loc2 = (const mem_ref_loc *)loc2_;
  class loop *loop1 = gimple_bb (loc1->stmt)->loop_father;
  class loop *loop2 = gimple_bb (loc2->stmt)->loop_father;
  if (loop1->num == loop2->num)
    return 0;
  return bb_loop_postorder[loop1->num] < bb_loop_postorder[loop2->num] ? -1 : 1;
}

/* Gathers memory references in loops.  */

static void
analyze_memory_references (bool store_motion)
{
  gimple_stmt_iterator bsi;
  basic_block bb, *bbs;
  class loop *outer;
  unsigned i, n;

  /* Collect all basic-blocks in loops and sort them after their
     loops postorder.  */
  i = 0;
  bbs = XNEWVEC (basic_block, n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS);
  FOR_EACH_BB_FN (bb, cfun)
    if (bb->loop_father != current_loops->tree_root)
      bbs[i++] = bb;
  n = i;
  gcc_sort_r (bbs, n, sizeof (basic_block), sort_bbs_in_loop_postorder_cmp,
	      bb_loop_postorder);

  /* Visit blocks in loop postorder and assign mem-ref IDs in that order.
     That results in better locality for all the bitmaps.  It also
     automatically sorts the location list of gathered memory references
     after their loop postorder number allowing to binary-search it.  */
  for (i = 0; i < n; ++i)
    {
      basic_block bb = bbs[i];
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
        gather_mem_refs_stmt (bb->loop_father, gsi_stmt (bsi));
    }

  /* Verify the list of gathered memory references is sorted after their
     loop postorder number.  */
  if (flag_checking)
    {
      im_mem_ref *ref;
      FOR_EACH_VEC_ELT (memory_accesses.refs_list, i, ref)
	for (unsigned j = 1; j < ref->accesses_in_loop.length (); ++j)
	  gcc_assert (sort_locs_in_loop_postorder_cmp
			(&ref->accesses_in_loop[j-1], &ref->accesses_in_loop[j],
			 bb_loop_postorder) <= 0);
    }

  free (bbs);

  if (!store_motion)
    return;

  /* Propagate the information about accessed memory references up
     the loop hierarchy.  */
  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    {
      /* Finalize the overall touched references (including subloops).  */
      bitmap_ior_into (&memory_accesses.all_refs_stored_in_loop[loop->num],
		       &memory_accesses.refs_stored_in_loop[loop->num]);

      /* Propagate the information about accessed memory references up
	 the loop hierarchy.  */
      outer = loop_outer (loop);
      if (outer == current_loops->tree_root)
	continue;

      bitmap_ior_into (&memory_accesses.all_refs_stored_in_loop[outer->num],
		       &memory_accesses.all_refs_stored_in_loop[loop->num]);
    }
}

/* Returns true if MEM1 and MEM2 may alias.  TTAE_CACHE is used as a cache in
   tree_to_aff_combination_expand.  */

static bool
mem_refs_may_alias_p (im_mem_ref *mem1, im_mem_ref *mem2,
		      hash_map<tree, name_expansion *> **ttae_cache,
		      bool tbaa_p)
{
  gcc_checking_assert (mem1->mem.ref != error_mark_node
		       && mem2->mem.ref != error_mark_node);

  /* Perform BASE + OFFSET analysis -- if MEM1 and MEM2 are based on the same
     object and their offset differ in such a way that the locations cannot
     overlap, then they cannot alias.  */
  poly_widest_int size1, size2;
  aff_tree off1, off2;

  /* Perform basic offset and type-based disambiguation.  */
  if (!refs_may_alias_p_1 (&mem1->mem, &mem2->mem, tbaa_p))
    return false;

  /* The expansion of addresses may be a bit expensive, thus we only do
     the check at -O2 and higher optimization levels.  */
  if (optimize < 2)
    return true;

  get_inner_reference_aff (mem1->mem.ref, &off1, &size1);
  get_inner_reference_aff (mem2->mem.ref, &off2, &size2);
  aff_combination_expand (&off1, ttae_cache);
  aff_combination_expand (&off2, ttae_cache);
  aff_combination_scale (&off1, -1);
  aff_combination_add (&off2, &off1);

  if (aff_comb_cannot_overlap_p (&off2, size1, size2))
    return false;

  return true;
}

/* Compare function for bsearch searching for reference locations
   in a loop.  */

static int
find_ref_loc_in_loop_cmp (const void *loop_, const void *loc_,
			  void *bb_loop_postorder_)
{
  unsigned *bb_loop_postorder = (unsigned *)bb_loop_postorder_;
  class loop *loop = (class loop *)const_cast<void *>(loop_);
  mem_ref_loc *loc = (mem_ref_loc *)const_cast<void *>(loc_);
  class loop *loc_loop = gimple_bb (loc->stmt)->loop_father;
  if (loop->num  == loc_loop->num
      || flow_loop_nested_p (loop, loc_loop))
    return 0;
  return (bb_loop_postorder[loop->num] < bb_loop_postorder[loc_loop->num]
	  ? -1 : 1);
}

/* Iterates over all locations of REF in LOOP and its subloops calling
   fn.operator() with the location as argument.  When that operator
   returns true the iteration is stopped and true is returned.
   Otherwise false is returned.  */

template <typename FN>
static bool
for_all_locs_in_loop (class loop *loop, im_mem_ref *ref, FN fn)
{
  unsigned i;
  mem_ref_loc *loc;

  /* Search for the cluster of locs in the accesses_in_loop vector
     which is sorted after postorder index of the loop father.  */
  loc = ref->accesses_in_loop.bsearch (loop, find_ref_loc_in_loop_cmp,
				       bb_loop_postorder);
  if (!loc)
    return false;

  /* We have found one location inside loop or its sub-loops.  Iterate
     both forward and backward to cover the whole cluster.  */
  i = loc - ref->accesses_in_loop.address ();
  while (i > 0)
    {
      --i;
      mem_ref_loc *l = &ref->accesses_in_loop[i];
      if (!flow_bb_inside_loop_p (loop, gimple_bb (l->stmt)))
	break;
      if (fn (l))
	return true;
    }
  for (i = loc - ref->accesses_in_loop.address ();
       i < ref->accesses_in_loop.length (); ++i)
    {
      mem_ref_loc *l = &ref->accesses_in_loop[i];
      if (!flow_bb_inside_loop_p (loop, gimple_bb (l->stmt)))
	break;
      if (fn (l))
	return true;
    }

  return false;
}

/* Rewrites location LOC by TMP_VAR.  */

class rewrite_mem_ref_loc
{
public:
  rewrite_mem_ref_loc (tree tmp_var_) : tmp_var (tmp_var_) {}
  bool operator () (mem_ref_loc *loc);
  tree tmp_var;
};

bool
rewrite_mem_ref_loc::operator () (mem_ref_loc *loc)
{
  *loc->ref = tmp_var;
  update_stmt (loc->stmt);
  return false;
}

/* Rewrites all references to REF in LOOP by variable TMP_VAR.  */

static void
rewrite_mem_refs (class loop *loop, im_mem_ref *ref, tree tmp_var)
{
  for_all_locs_in_loop (loop, ref, rewrite_mem_ref_loc (tmp_var));
}

/* Stores the first reference location in LOCP.  */

class first_mem_ref_loc_1
{
public:
  first_mem_ref_loc_1 (mem_ref_loc **locp_) : locp (locp_) {}
  bool operator () (mem_ref_loc *loc);
  mem_ref_loc **locp;
};

bool
first_mem_ref_loc_1::operator () (mem_ref_loc *loc)
{
  *locp = loc;
  return true;
}

/* Returns the first reference location to REF in LOOP.  */

static mem_ref_loc *
first_mem_ref_loc (class loop *loop, im_mem_ref *ref)
{
  mem_ref_loc *locp = NULL;
  for_all_locs_in_loop (loop, ref, first_mem_ref_loc_1 (&locp));
  return locp;
}

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
       MEM = lsm;	<-- (X)

  In case MEM and TMP_VAR are NULL the function will return the then
  block so the caller can insert (X) and other related stmts.
*/

static basic_block
execute_sm_if_changed (edge ex, tree mem, tree tmp_var, tree flag,
		       edge preheader, hash_set <basic_block> *flag_bbs,
		       edge &append_cond_position, edge &last_cond_fallthru)
{
  basic_block new_bb, then_bb, old_dest;
  bool loop_has_only_one_exit;
  edge then_old_edge;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  bool irr = ex->flags & EDGE_IRREDUCIBLE_LOOP;

  profile_count count_sum = profile_count::zero ();
  int nbbs = 0, ncount = 0;
  profile_probability flag_probability = profile_probability::uninitialized ();

  /* Flag is set in FLAG_BBS. Determine probability that flag will be true
     at loop exit.

     This code may look fancy, but it cannot update profile very realistically
     because we do not know the probability that flag will be true at given
     loop exit.

     We look for two interesting extremes
       - when exit is dominated by block setting the flag, we know it will
         always be true.  This is a common case.
       - when all blocks setting the flag have very low frequency we know
         it will likely be false.
     In all other cases we default to 2/3 for flag being true.  */

  for (hash_set<basic_block>::iterator it = flag_bbs->begin ();
       it != flag_bbs->end (); ++it)
    {
       if ((*it)->count.initialized_p ())
         count_sum += (*it)->count, ncount ++;
       if (dominated_by_p (CDI_DOMINATORS, ex->src, *it))
	 flag_probability = profile_probability::always ();
       nbbs++;
    }

  profile_probability cap
	  = profile_probability::guessed_always ().apply_scale (2, 3);

  if (flag_probability.initialized_p ())
    ;
  else if (ncount == nbbs
	   && preheader->count () >= count_sum && preheader->count ().nonzero_p ())
    {
      flag_probability = count_sum.probability_in (preheader->count ());
      if (flag_probability > cap)
	flag_probability = cap;
    }

  if (!flag_probability.initialized_p ())
    flag_probability = cap;

  /* ?? Insert store after previous store if applicable.  See note
     below.  */
  if (append_cond_position)
    ex = append_cond_position;

  loop_has_only_one_exit = single_pred_p (ex->dest);

  if (loop_has_only_one_exit)
    ex = split_block_after_labels (ex->dest);
  else
    {
      for (gphi_iterator gpi = gsi_start_phis (ex->dest);
	   !gsi_end_p (gpi); gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;

	  /* When the destination has a non-virtual PHI node with multiple
	     predecessors make sure we preserve the PHI structure by
	     forcing a forwarder block so that hoisting of that PHI will
	     still work.  */
	  split_edge (ex);
	  break;
	}
    }

  old_dest = ex->dest;
  new_bb = split_edge (ex);
  if (append_cond_position)
    new_bb->count += last_cond_fallthru->count ();
  then_bb = create_empty_bb (new_bb);
  then_bb->count = new_bb->count.apply_probability (flag_probability);
  if (irr)
    then_bb->flags = BB_IRREDUCIBLE_LOOP;
  add_bb_to_loop (then_bb, new_bb->loop_father);

  gsi = gsi_start_bb (new_bb);
  stmt = gimple_build_cond (NE_EXPR, flag, boolean_false_node,
			    NULL_TREE, NULL_TREE);
  gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);

  /* Insert actual store.  */
  if (mem)
    {
      gsi = gsi_start_bb (then_bb);
      stmt = gimple_build_assign (unshare_expr (mem), tmp_var);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);
    }

  edge e1 = single_succ_edge (new_bb);
  edge e2 = make_edge (new_bb, then_bb,
	               EDGE_TRUE_VALUE | (irr ? EDGE_IRREDUCIBLE_LOOP : 0));
  e2->probability = flag_probability;

  e1->flags |= EDGE_FALSE_VALUE | (irr ? EDGE_IRREDUCIBLE_LOOP : 0);
  e1->flags &= ~EDGE_FALLTHRU;

  e1->probability = flag_probability.invert ();

  then_old_edge = make_single_succ_edge (then_bb, old_dest,
			     EDGE_FALLTHRU | (irr ? EDGE_IRREDUCIBLE_LOOP : 0));

  set_immediate_dominator (CDI_DOMINATORS, then_bb, new_bb);

  if (append_cond_position)
    {
      basic_block prevbb = last_cond_fallthru->src;
      redirect_edge_succ (last_cond_fallthru, new_bb);
      set_immediate_dominator (CDI_DOMINATORS, new_bb, prevbb);
      set_immediate_dominator (CDI_DOMINATORS, old_dest,
			       recompute_dominator (CDI_DOMINATORS, old_dest));
    }

  /* ?? Because stores may alias, they must happen in the exact
     sequence they originally happened.  Save the position right after
     the (_lsm) store we just created so we can continue appending after
     it and maintain the original order.  */
  append_cond_position = then_old_edge;
  last_cond_fallthru = find_edge (new_bb, old_dest);

  if (!loop_has_only_one_exit)
    for (gphi_iterator gpi = gsi_start_phis (old_dest);
	 !gsi_end_p (gpi); gsi_next (&gpi))
      {
	gphi *phi = gpi.phi ();
	unsigned i;

	for (i = 0; i < gimple_phi_num_args (phi); i++)
	  if (gimple_phi_arg_edge (phi, i)->src == new_bb)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      add_phi_arg (phi, arg, then_old_edge, UNKNOWN_LOCATION);
	      update_stmt (phi);
	    }
      }

  return then_bb;
}

/* When REF is set on the location, set flag indicating the store.  */

class sm_set_flag_if_changed
{
public:
  sm_set_flag_if_changed (tree flag_, hash_set <basic_block> *bbs_)
	 : flag (flag_), bbs (bbs_) {}
  bool operator () (mem_ref_loc *loc);
  tree flag;
  hash_set <basic_block> *bbs;
};

bool
sm_set_flag_if_changed::operator () (mem_ref_loc *loc)
{
  /* Only set the flag for writes.  */
  if (is_gimple_assign (loc->stmt)
      && gimple_assign_lhs_ptr (loc->stmt) == loc->ref)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (loc->stmt);
      gimple *stmt = gimple_build_assign (flag, boolean_true_node);
      gsi_insert_after (&gsi, stmt, GSI_CONTINUE_LINKING);
      bbs->add (gimple_bb (stmt));
    }
  return false;
}

/* Helper function for execute_sm.  On every location where REF is
   set, set an appropriate flag indicating the store.  */

static tree
execute_sm_if_changed_flag_set (class loop *loop, im_mem_ref *ref,
				hash_set <basic_block> *bbs)
{
  tree flag;
  char *str = get_lsm_tmp_name (ref->mem.ref, ~0, "_flag");
  flag = create_tmp_reg (boolean_type_node, str);
  for_all_locs_in_loop (loop, ref, sm_set_flag_if_changed (flag, bbs));
  return flag;
}

struct sm_aux
{
  tree tmp_var;
  tree store_flag;
  hash_set <basic_block> flag_bbs;
};

/* Executes store motion of memory reference REF from LOOP.
   Exits from the LOOP are stored in EXITS.  The initialization of the
   temporary variable is put to the preheader of the loop, and assignments
   to the reference from the temporary variable are emitted to exits.  */

static sm_aux *
execute_sm (class loop *loop, im_mem_ref *ref,
	    hash_map<im_mem_ref *, sm_aux *> &aux_map, bool maybe_mt,
	    bool use_other_flag_var)
{
  gassign *load;
  struct fmt_data fmt_data;
  struct lim_aux_data *lim_data;
  bool multi_threaded_model_p = false;
  gimple_stmt_iterator gsi;
  sm_aux *aux = new sm_aux;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Executing store motion of ");
      print_generic_expr (dump_file, ref->mem.ref);
      fprintf (dump_file, " from loop %d\n", loop->num);
    }

  aux->tmp_var = create_tmp_reg (TREE_TYPE (ref->mem.ref),
				 get_lsm_tmp_name (ref->mem.ref, ~0));

  fmt_data.loop = loop;
  fmt_data.orig_loop = loop;
  for_each_index (&ref->mem.ref, force_move_till, &fmt_data);

  bool always_stored = ref_always_accessed_p (loop, ref, true);
  if (maybe_mt
      && (bb_in_transaction (loop_preheader_edge (loop)->src)
	  || (ref_can_have_store_data_races (ref->mem.ref) && ! always_stored)))
    multi_threaded_model_p = true;

  if (multi_threaded_model_p && !use_other_flag_var)
    aux->store_flag
      = execute_sm_if_changed_flag_set (loop, ref, &aux->flag_bbs);
  else
    aux->store_flag = NULL_TREE;

  /* Remember variable setup.  */
  aux_map.put (ref, aux);

  rewrite_mem_refs (loop, ref, aux->tmp_var);

  /* Emit the load code on a random exit edge or into the latch if
     the loop does not exit, so that we are sure it will be processed
     by move_computations after all dependencies.  */
  gsi = gsi_for_stmt (first_mem_ref_loc (loop, ref)->stmt);

  /* Avoid doing a load if there was no load of the ref in the loop.
     Esp. when the ref is not always stored we cannot optimize it
     away later.  But when it is not always stored we must use a conditional
     store then.  */
  if ((!always_stored && !multi_threaded_model_p)
      || (ref->loaded && bitmap_bit_p (ref->loaded, loop->num)))
    load = gimple_build_assign (aux->tmp_var, unshare_expr (ref->mem.ref));
  else
    {
      /* If not emitting a load mark the uninitialized state on the
	 loop entry as not to be warned for.  */
      tree uninit = create_tmp_reg (TREE_TYPE (aux->tmp_var));
      suppress_warning (uninit, OPT_Wuninitialized);
      load = gimple_build_assign (aux->tmp_var, uninit);
    }
  lim_data = init_lim_data (load);
  lim_data->max_loop = loop;
  lim_data->tgt_loop = loop;
  gsi_insert_before (&gsi, load, GSI_SAME_STMT);

  if (aux->store_flag)
    {
      load = gimple_build_assign (aux->store_flag, boolean_false_node);
      lim_data = init_lim_data (load);
      lim_data->max_loop = loop;
      lim_data->tgt_loop = loop;
      gsi_insert_before (&gsi, load, GSI_SAME_STMT);
    }

  return aux;
}

/* sm_ord is used for ordinary stores we can retain order with respect
       to other stores
   sm_unord is used for conditional executed stores which need to be
       able to execute in arbitrary order with respect to other stores
   sm_other is used for stores we do not try to apply store motion to.  */
enum sm_kind { sm_ord, sm_unord, sm_other };
struct seq_entry
{
  seq_entry () = default;
  seq_entry (unsigned f, sm_kind k, tree fr = NULL)
    : first (f), second (k), from (fr) {}
  unsigned first;
  sm_kind second;
  tree from;
};

static void
execute_sm_exit (class loop *loop, edge ex, vec<seq_entry> &seq,
		 hash_map<im_mem_ref *, sm_aux *> &aux_map, sm_kind kind,
		 edge &append_cond_position, edge &last_cond_fallthru,
		 bitmap clobbers_to_prune)
{
  /* Sink the stores to exit from the loop.  */
  for (unsigned i = seq.length (); i > 0; --i)
    {
      im_mem_ref *ref = memory_accesses.refs_list[seq[i-1].first];
      if (seq[i-1].second == sm_other)
	{
	  gcc_assert (kind == sm_ord && seq[i-1].from != NULL_TREE);
	  gassign *store;
	  if (ref->mem.ref == error_mark_node)
	    {
	      tree lhs = gimple_assign_lhs (ref->accesses_in_loop[0].stmt);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Re-issueing dependent ");
		  print_generic_expr (dump_file, unshare_expr (seq[i-1].from));
		  fprintf (dump_file, " of ");
		  print_generic_expr (dump_file, lhs);
		  fprintf (dump_file, " from loop %d on exit %d -> %d\n",
			   loop->num, ex->src->index, ex->dest->index);
		}
	      store = gimple_build_assign (unshare_expr (lhs),
					   unshare_expr (seq[i-1].from));
	      bitmap_set_bit (clobbers_to_prune, seq[i-1].first);
	    }
	  else
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Re-issueing dependent store of ");
		  print_generic_expr (dump_file, ref->mem.ref);
		  fprintf (dump_file, " from loop %d on exit %d -> %d\n",
			   loop->num, ex->src->index, ex->dest->index);
		}
	      store = gimple_build_assign (unshare_expr (ref->mem.ref),
					   seq[i-1].from);
	    }
	  gsi_insert_on_edge (ex, store);
	}
      else
	{
	  sm_aux *aux = *aux_map.get (ref);
	  if (!aux->store_flag || kind == sm_ord)
	    {
	      gassign *store;
	      store = gimple_build_assign (unshare_expr (ref->mem.ref),
					   aux->tmp_var);
	      gsi_insert_on_edge (ex, store);
	    }
	  else
	    execute_sm_if_changed (ex, ref->mem.ref, aux->tmp_var,
				   aux->store_flag,
				   loop_preheader_edge (loop), &aux->flag_bbs,
				   append_cond_position, last_cond_fallthru);
	}
    }
}

/* Push the SM candidate at index PTR in the sequence SEQ down until
   we hit the next SM candidate.  Return true if that went OK and
   false if we could not disambiguate agains another unrelated ref.
   Update *AT to the index where the candidate now resides.  */

static bool
sm_seq_push_down (vec<seq_entry> &seq, unsigned ptr, unsigned *at)
{
  *at = ptr;
  for (; ptr > 0; --ptr)
    {
      seq_entry &new_cand = seq[ptr];
      seq_entry &against = seq[ptr-1];
      if (against.second == sm_ord
	  || (against.second == sm_other && against.from != NULL_TREE))
	/* Found the tail of the sequence.  */
	break;
      /* We may not ignore self-dependences here.  */
      if (new_cand.first == against.first
	  /* ???  We could actually handle clobbers here, but not easily
	     with LIMs dependence analysis.  */
	  || (memory_accesses.refs_list[new_cand.first]->mem.ref
	      == error_mark_node)
	  || (memory_accesses.refs_list[against.first]->mem.ref
	      == error_mark_node)
	  || !refs_independent_p (memory_accesses.refs_list[new_cand.first],
				  memory_accesses.refs_list[against.first],
				  false))
	/* ???  Prune new_cand from the list of refs to apply SM to.  */
	return false;
      std::swap (new_cand, against);
      *at = ptr - 1;
    }
  return true;
}

/* Computes the sequence of stores from candidates in REFS_NOT_IN_SEQ to SEQ
   walking backwards from VDEF (or the end of BB if VDEF is NULL).  */

static int
sm_seq_valid_bb (class loop *loop, basic_block bb, tree vdef,
		 vec<seq_entry> &seq, bitmap refs_not_in_seq,
		 bitmap refs_not_supported, bool forked,
		 bitmap fully_visited)
{
  if (!vdef)
    for (gimple_stmt_iterator gsi = gsi_last_bb (bb); !gsi_end_p (gsi);
	 gsi_prev (&gsi))
      {
	vdef = gimple_vdef (gsi_stmt (gsi));
	if (vdef)
	  break;
      }
  if (!vdef)
    {
      gphi *vphi = get_virtual_phi (bb);
      if (vphi)
	vdef = gimple_phi_result (vphi);
    }
  if (!vdef)
    {
      if (single_pred_p (bb))
	/* This handles the perfect nest case.  */
	return sm_seq_valid_bb (loop, single_pred (bb), vdef,
				seq, refs_not_in_seq, refs_not_supported,
				forked, fully_visited);
      return 0;
    }
  do
    {
      gimple *def = SSA_NAME_DEF_STMT (vdef);
      if (gimple_bb (def) != bb)
	{
	  /* If we forked by processing a PHI do not allow our walk to
	     merge again until we handle that robustly.  */
	  if (forked)
	    {
	      /* Mark refs_not_in_seq as unsupported.  */
	      bitmap_ior_into (refs_not_supported, refs_not_in_seq);
	      return 1;
	    }
	  /* Otherwise it doesn't really matter if we end up in different
	     BBs.  */
	  bb = gimple_bb (def);
	}
      if (gphi *phi = dyn_cast <gphi *> (def))
	{
	  /* Handle CFG merges.  Until we handle forks (gimple_bb (def) != bb)
	     this is still linear.
	     Eventually we want to cache intermediate results per BB
	     (but we can't easily cache for different exits?).  */
	  /* Stop at PHIs with possible backedges.  */
	  if (bb == bb->loop_father->header
	      || bb->flags & BB_IRREDUCIBLE_LOOP)
	    {
	      /* Mark refs_not_in_seq as unsupported.  */
	      bitmap_ior_into (refs_not_supported, refs_not_in_seq);
	      return 1;
	    }
	  if (gimple_phi_num_args (phi) == 1)
	    return sm_seq_valid_bb (loop, gimple_phi_arg_edge (phi, 0)->src,
				    gimple_phi_arg_def (phi, 0), seq,
				    refs_not_in_seq, refs_not_supported,
				    false, fully_visited);
	  if (bitmap_bit_p (fully_visited,
			    SSA_NAME_VERSION (gimple_phi_result (phi))))
	    return 1;
	  auto_vec<seq_entry> first_edge_seq;
	  auto_bitmap tem_refs_not_in_seq (&lim_bitmap_obstack);
	  int eret;
	  bitmap_copy (tem_refs_not_in_seq, refs_not_in_seq);
	  eret = sm_seq_valid_bb (loop, gimple_phi_arg_edge (phi, 0)->src,
				  gimple_phi_arg_def (phi, 0),
				  first_edge_seq,
				  tem_refs_not_in_seq, refs_not_supported,
				  true, fully_visited);
	  if (eret != 1)
	    return -1;
	  /* Simplify our lives by pruning the sequence of !sm_ord.  */
	  while (!first_edge_seq.is_empty ()
		 && first_edge_seq.last ().second != sm_ord)
	    first_edge_seq.pop ();
	  for (unsigned int i = 1; i < gimple_phi_num_args (phi); ++i)
	    {
	      tree vuse = gimple_phi_arg_def (phi, i);
	      edge e = gimple_phi_arg_edge (phi, i);
	      auto_vec<seq_entry> edge_seq;
	      bitmap_and_compl (tem_refs_not_in_seq,
				refs_not_in_seq, refs_not_supported);
	      /* If we've marked all refs we search for as unsupported
		 we can stop processing and use the sequence as before
		 the PHI.  */
	      if (bitmap_empty_p (tem_refs_not_in_seq))
		return 1;
	      eret = sm_seq_valid_bb (loop, e->src, vuse, edge_seq,
				      tem_refs_not_in_seq, refs_not_supported,
				      true, fully_visited);
	      if (eret != 1)
		return -1;
	      /* Simplify our lives by pruning the sequence of !sm_ord.  */
	      while (!edge_seq.is_empty ()
		     && edge_seq.last ().second != sm_ord)
		edge_seq.pop ();
	      unsigned min_len = MIN(first_edge_seq.length (),
				     edge_seq.length ());
	      /* Incrementally merge seqs into first_edge_seq.  */
	      int first_uneq = -1;
	      auto_vec<seq_entry, 2> extra_refs;
	      for (unsigned int i = 0; i < min_len; ++i)
		{
		  /* ???  We can more intelligently merge when we face different
		     order by additional sinking operations in one sequence.
		     For now we simply mark them as to be processed by the
		     not order-preserving SM code.  */
		  if (first_edge_seq[i].first != edge_seq[i].first)
		    {
		      if (first_edge_seq[i].second == sm_ord)
			bitmap_set_bit (refs_not_supported,
					first_edge_seq[i].first);
		      if (edge_seq[i].second == sm_ord)
			bitmap_set_bit (refs_not_supported, edge_seq[i].first);
		      first_edge_seq[i].second = sm_other;
		      first_edge_seq[i].from = NULL_TREE;
		      /* Record the dropped refs for later processing.  */
		      if (first_uneq == -1)
			first_uneq = i;
		      extra_refs.safe_push (seq_entry (edge_seq[i].first,
						       sm_other, NULL_TREE));
		    }
		  /* sm_other prevails.  */
		  else if (first_edge_seq[i].second != edge_seq[i].second)
		    {
		      /* Make sure the ref is marked as not supported.  */
		      bitmap_set_bit (refs_not_supported,
				      first_edge_seq[i].first);
		      first_edge_seq[i].second = sm_other;
		      first_edge_seq[i].from = NULL_TREE;
		    }
		  else if (first_edge_seq[i].second == sm_other
			   && first_edge_seq[i].from != NULL_TREE
			   && (edge_seq[i].from == NULL_TREE
			       || !operand_equal_p (first_edge_seq[i].from,
						    edge_seq[i].from, 0)))
		    first_edge_seq[i].from = NULL_TREE;
		}
	      /* Any excess elements become sm_other since they are now
		 coonditionally executed.  */
	      if (first_edge_seq.length () > edge_seq.length ())
		{
		  for (unsigned i = edge_seq.length ();
		       i < first_edge_seq.length (); ++i)
		    {
		      if (first_edge_seq[i].second == sm_ord)
			bitmap_set_bit (refs_not_supported,
					first_edge_seq[i].first);
		      first_edge_seq[i].second = sm_other;
		    }
		}
	      else if (edge_seq.length () > first_edge_seq.length ())
		{
		  if (first_uneq == -1)
		    first_uneq = first_edge_seq.length ();
		  for (unsigned i = first_edge_seq.length ();
		       i < edge_seq.length (); ++i)
		    {
		      if (edge_seq[i].second == sm_ord)
			bitmap_set_bit (refs_not_supported, edge_seq[i].first);
		      extra_refs.safe_push (seq_entry (edge_seq[i].first,
						       sm_other, NULL_TREE));
		    }
		}
	      /* Put unmerged refs at first_uneq to force dependence checking
		 on them.  */
	      if (first_uneq != -1)
		{
		  /* Missing ordered_splice_at.  */
		  if ((unsigned)first_uneq == first_edge_seq.length ())
		    first_edge_seq.safe_splice (extra_refs);
		  else
		    {
		      unsigned fes_length = first_edge_seq.length ();
		      first_edge_seq.safe_grow (fes_length
						+ extra_refs.length ());
		      memmove (&first_edge_seq[first_uneq + extra_refs.length ()],
			       &first_edge_seq[first_uneq],
			       (fes_length - first_uneq) * sizeof (seq_entry));
		      memcpy (&first_edge_seq[first_uneq],
			      extra_refs.address (),
			      extra_refs.length () * sizeof (seq_entry));
		    }
		}
	    }
	  /* Use the sequence from the first edge and push SMs down.  */
	  for (unsigned i = 0; i < first_edge_seq.length (); ++i)
	    {
	      unsigned id = first_edge_seq[i].first;
	      seq.safe_push (first_edge_seq[i]);
	      unsigned new_idx;
	      if ((first_edge_seq[i].second == sm_ord
		   || (first_edge_seq[i].second == sm_other
		       && first_edge_seq[i].from != NULL_TREE))
		  && !sm_seq_push_down (seq, seq.length () - 1, &new_idx))
		{
		  if (first_edge_seq[i].second == sm_ord)
		    bitmap_set_bit (refs_not_supported, id);
		  /* Mark it sm_other.  */
		  seq[new_idx].second = sm_other;
		  seq[new_idx].from = NULL_TREE;
		}
	    }
	  bitmap_set_bit (fully_visited,
			  SSA_NAME_VERSION (gimple_phi_result (phi)));
	  return 1;
	}
      lim_aux_data *data = get_lim_data (def);
      im_mem_ref *ref = memory_accesses.refs_list[data->ref];
      if (data->ref == UNANALYZABLE_MEM_ID)
	return -1;
      /* Stop at memory references which we can't move.  */
      else if ((ref->mem.ref == error_mark_node
		/* We can move end-of-storage/object down.  */
		&& !gimple_clobber_p (ref->accesses_in_loop[0].stmt,
				      CLOBBER_STORAGE_END)
		&& !gimple_clobber_p (ref->accesses_in_loop[0].stmt,
				      CLOBBER_OBJECT_END))
	       || TREE_THIS_VOLATILE (ref->mem.ref))
	{
	  /* Mark refs_not_in_seq as unsupported.  */
	  bitmap_ior_into (refs_not_supported, refs_not_in_seq);
	  return 1;
	}
      /* One of the stores we want to apply SM to and we've not yet seen.  */
      else if (bitmap_clear_bit (refs_not_in_seq, data->ref))
	{
	  seq.safe_push (seq_entry (data->ref, sm_ord));

	  /* 1) push it down the queue until a SMed
	     and not ignored ref is reached, skipping all not SMed refs
	     and ignored refs via non-TBAA disambiguation.  */
	  unsigned new_idx;
	  if (!sm_seq_push_down (seq, seq.length () - 1, &new_idx)
	      /* If that fails but we did not fork yet continue, we'll see
		 to re-materialize all of the stores in the sequence then.
		 Further stores will only be pushed up to this one.  */
	      && forked)
	    {
	      bitmap_set_bit (refs_not_supported, data->ref);
	      /* Mark it sm_other.  */
	      seq[new_idx].second = sm_other;
	    }

	  /* 2) check whether we've seen all refs we want to SM and if so
	     declare success for the active exit  */
	  if (bitmap_empty_p (refs_not_in_seq))
	    return 1;
	}
      else
	/* Another store not part of the final sequence.  Simply push it.  */
	seq.safe_push (seq_entry (data->ref, sm_other,
				  gimple_assign_rhs1 (def)));

      vdef = gimple_vuse (def);
    }
  while (1);
}

/* Hoists memory references MEM_REFS out of LOOP.  EXITS is the list of exit
   edges of the LOOP.  */

static void
hoist_memory_references (class loop *loop, bitmap mem_refs,
			 const vec<edge> &exits)
{
  im_mem_ref *ref;
  unsigned  i;
  bitmap_iterator bi;

  /* There's a special case we can use ordered re-materialization for
     conditionally excuted stores which is when all stores in the loop
     happen in the same basic-block.  In that case we know we'll reach
     all stores and thus can simply process that BB and emit a single
     conditional block of ordered materializations.  See PR102436.  */
  basic_block single_store_bb = NULL;
  EXECUTE_IF_SET_IN_BITMAP (&memory_accesses.all_refs_stored_in_loop[loop->num],
			    0, i, bi)
    {
      bool fail = false;
      ref = memory_accesses.refs_list[i];
      for (auto loc : ref->accesses_in_loop)
	if (!gimple_vdef (loc.stmt))
	  ;
	else if (!single_store_bb)
	  {
	    single_store_bb = gimple_bb (loc.stmt);
	    bool conditional = false;
	    for (edge e : exits)
	      if (!dominated_by_p (CDI_DOMINATORS, e->src, single_store_bb))
		{
		  /* Conditional as seen from e.  */
		  conditional = true;
		  break;
		}
	    if (!conditional)
	      {
		fail = true;
		break;
	      }
	  }
	else if (single_store_bb != gimple_bb (loc.stmt))
	  {
	    fail = true;
	    break;
	  }
      if (fail)
	{
	  single_store_bb = NULL;
	  break;
	}
    }
  if (single_store_bb)
    {
      /* Analyze the single block with stores.  */
      auto_bitmap fully_visited;
      auto_bitmap refs_not_supported;
      auto_bitmap refs_not_in_seq;
      auto_vec<seq_entry> seq;
      bitmap_copy (refs_not_in_seq, mem_refs);
      int res = sm_seq_valid_bb (loop, single_store_bb, NULL_TREE,
				 seq, refs_not_in_seq, refs_not_supported,
				 false, fully_visited);
      if (res != 1)
	{
	  /* Unhandled refs can still fail this.  */
	  bitmap_clear (mem_refs);
	  return;
	}

      /* We cannot handle sm_other since we neither remember the
	 stored location nor the value at the point we execute them.  */
      for (unsigned i = 0; i < seq.length (); ++i)
	{
	  unsigned new_i;
	  if (seq[i].second == sm_other
	      && seq[i].from != NULL_TREE)
	    seq[i].from = NULL_TREE;
	  else if ((seq[i].second == sm_ord
		    || (seq[i].second == sm_other
			&& seq[i].from != NULL_TREE))
		   && !sm_seq_push_down (seq, i, &new_i))
	    {
	      bitmap_set_bit (refs_not_supported, seq[new_i].first);
	      seq[new_i].second = sm_other;
	      seq[new_i].from = NULL_TREE;
	    }
	}
      bitmap_and_compl_into (mem_refs, refs_not_supported);
      if (bitmap_empty_p (mem_refs))
	return;

      /* Prune seq.  */
      while (seq.last ().second == sm_other
	     && seq.last ().from == NULL_TREE)
	seq.pop ();

      hash_map<im_mem_ref *, sm_aux *> aux_map;

      /* Execute SM but delay the store materialization for ordered
	 sequences on exit.  Remember a created flag var and make
	 sure to re-use it.  */
      sm_aux *flag_var_aux = nullptr;
      EXECUTE_IF_SET_IN_BITMAP (mem_refs, 0, i, bi)
	{
	  ref = memory_accesses.refs_list[i];
	  sm_aux *aux = execute_sm (loop, ref, aux_map, true,
				    flag_var_aux != nullptr);
	  if (aux->store_flag)
	    flag_var_aux = aux;
	}

      /* Materialize ordered store sequences on exits.  */
      edge e;
      auto_bitmap clobbers_to_prune;
      FOR_EACH_VEC_ELT (exits, i, e)
	{
	  edge append_cond_position = NULL;
	  edge last_cond_fallthru = NULL;
	  edge insert_e = e;
	  /* Construct the single flag variable control flow and insert
	     the ordered seq of stores in the then block.  With
	     -fstore-data-races we can do the stores unconditionally.  */
	  if (flag_var_aux)
	    insert_e
	      = single_pred_edge
		  (execute_sm_if_changed (e, NULL_TREE, NULL_TREE,
					  flag_var_aux->store_flag,
					  loop_preheader_edge (loop),
					  &flag_var_aux->flag_bbs,
					  append_cond_position,
					  last_cond_fallthru));
	  execute_sm_exit (loop, insert_e, seq, aux_map, sm_ord,
			   append_cond_position, last_cond_fallthru,
			   clobbers_to_prune);
	  gsi_commit_one_edge_insert (insert_e, NULL);
	}

      /* Remove clobbers inside the loop we re-materialized on exits.  */
      EXECUTE_IF_SET_IN_BITMAP (clobbers_to_prune, 0, i, bi)
	{
	  gimple *stmt = memory_accesses.refs_list[i]->accesses_in_loop[0].stmt;
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  unlink_stmt_vdef (stmt);
	  release_defs (stmt);
	  gimple_set_vdef (stmt, NULL_TREE);
	  gsi_remove (&gsi, true);
	}

      for (hash_map<im_mem_ref *, sm_aux *>::iterator iter = aux_map.begin ();
	   iter != aux_map.end (); ++iter)
	delete (*iter).second;

      return;
    }

  /* To address PR57359 before actually applying store-motion check
     the candidates found for validity with regards to reordering
     relative to other stores which we until here disambiguated using
     TBAA which isn't valid.
     What matters is the order of the last stores to the mem_refs
     with respect to the other stores of the loop at the point of the
     loop exits.  */

  /* For each exit compute the store order, pruning from mem_refs
     on the fly.  */
  /* The complexity of this is at least
     O(number of exits * number of SM refs) but more approaching
     O(number of exits * number of SM refs * number of stores).  */
  /* ???  Somehow do this in a single sweep over the loop body.  */
  auto_vec<std::pair<edge, vec<seq_entry> > > sms;
  auto_bitmap refs_not_supported (&lim_bitmap_obstack);
  edge e;
  FOR_EACH_VEC_ELT (exits, i, e)
    {
      vec<seq_entry> seq;
      seq.create (4);
      auto_bitmap refs_not_in_seq (&lim_bitmap_obstack);
      bitmap_and_compl (refs_not_in_seq, mem_refs, refs_not_supported);
      if (bitmap_empty_p (refs_not_in_seq))
	{
	  seq.release ();
	  break;
	}
      auto_bitmap fully_visited;
      int res = sm_seq_valid_bb (loop, e->src, NULL_TREE,
				 seq, refs_not_in_seq,
				 refs_not_supported, false,
				 fully_visited);
      if (res != 1)
	{
	  bitmap_copy (refs_not_supported, mem_refs);
	  seq.release ();
	  break;
	}
      sms.safe_push (std::make_pair (e, seq));
    }

  /* Prune pruned mem_refs from earlier processed exits.  */
  bool changed = !bitmap_empty_p (refs_not_supported);
  while (changed)
    {
      changed = false;
      std::pair<edge, vec<seq_entry> > *seq;
      FOR_EACH_VEC_ELT (sms, i, seq)
	{
	  bool need_to_push = false;
	  for (unsigned i = 0; i < seq->second.length (); ++i)
	    {
	      sm_kind kind = seq->second[i].second;
	      if (kind == sm_other && seq->second[i].from == NULL_TREE)
		break;
	      unsigned id = seq->second[i].first;
	      unsigned new_idx;
	      if (kind == sm_ord
		  && bitmap_bit_p (refs_not_supported, id))
		{
		  seq->second[i].second = sm_other;
		  gcc_assert (seq->second[i].from == NULL_TREE);
		  need_to_push = true;
		}
	      else if (need_to_push
		       && !sm_seq_push_down (seq->second, i, &new_idx))
		{
		  /* We need to push down both sm_ord and sm_other
		     but for the latter we need to disqualify all
		     following refs.  */
		  if (kind == sm_ord)
		    {
		      if (bitmap_set_bit (refs_not_supported, id))
			changed = true;
		      seq->second[new_idx].second = sm_other;
		    }
		  else
		    {
		      for (unsigned j = seq->second.length () - 1;
			   j > new_idx; --j)
			if (seq->second[j].second == sm_ord
			    && bitmap_set_bit (refs_not_supported,
					       seq->second[j].first))
			  changed = true;
		      seq->second.truncate (new_idx);
		      break;
		    }
		}
	    }
	}
    }
  std::pair<edge, vec<seq_entry> > *seq;
  FOR_EACH_VEC_ELT (sms, i, seq)
    {
      /* Prune sm_other from the end.  */
      while (!seq->second.is_empty ()
	     && seq->second.last ().second == sm_other)
	seq->second.pop ();
      /* Prune duplicates from the start.  */
      auto_bitmap seen (&lim_bitmap_obstack);
      unsigned j, k;
      for (j = k = 0; j < seq->second.length (); ++j)
	if (bitmap_set_bit (seen, seq->second[j].first))
	  {
	    if (k != j)
	      seq->second[k] = seq->second[j];
	    ++k;
	  }
      seq->second.truncate (k);
      /* And verify.  */
      seq_entry *e;
      FOR_EACH_VEC_ELT (seq->second, j, e)
	gcc_assert (e->second == sm_ord
		    || (e->second == sm_other && e->from != NULL_TREE));
    }

  /* Verify dependence for refs we cannot handle with the order preserving
     code (refs_not_supported) or prune them from mem_refs.  */
  auto_vec<seq_entry> unord_refs;
  EXECUTE_IF_SET_IN_BITMAP (refs_not_supported, 0, i, bi)
    {
      ref = memory_accesses.refs_list[i];
      if (!ref_indep_loop_p (loop, ref, sm_waw))
	bitmap_clear_bit (mem_refs, i);
      /* We've now verified store order for ref with respect to all other
	 stores in the loop does not matter.  */
      else
	unord_refs.safe_push (seq_entry (i, sm_unord));
    }

  hash_map<im_mem_ref *, sm_aux *> aux_map;

  /* Execute SM but delay the store materialization for ordered
     sequences on exit.  */
  EXECUTE_IF_SET_IN_BITMAP (mem_refs, 0, i, bi)
    {
      ref = memory_accesses.refs_list[i];
      execute_sm (loop, ref, aux_map, bitmap_bit_p (refs_not_supported, i),
		  false);
    }

  /* Materialize ordered store sequences on exits.  */
  auto_bitmap clobbers_to_prune;
  FOR_EACH_VEC_ELT (exits, i, e)
    {
      edge append_cond_position = NULL;
      edge last_cond_fallthru = NULL;
      if (i < sms.length ())
	{
	  gcc_assert (sms[i].first == e);
	  execute_sm_exit (loop, e, sms[i].second, aux_map, sm_ord,
			   append_cond_position, last_cond_fallthru,
			   clobbers_to_prune);
	  sms[i].second.release ();
	}
      if (!unord_refs.is_empty ())
	execute_sm_exit (loop, e, unord_refs, aux_map, sm_unord,
			 append_cond_position, last_cond_fallthru,
			 clobbers_to_prune);
      /* Commit edge inserts here to preserve the order of stores
	 when an exit exits multiple loops.  */
      gsi_commit_one_edge_insert (e, NULL);
    }

  /* Remove clobbers inside the loop we re-materialized on exits.  */
  EXECUTE_IF_SET_IN_BITMAP (clobbers_to_prune, 0, i, bi)
    {
      gimple *stmt = memory_accesses.refs_list[i]->accesses_in_loop[0].stmt;
      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      unlink_stmt_vdef (stmt);
      release_defs (stmt);
      gimple_set_vdef (stmt, NULL_TREE);
      gsi_remove (&gsi, true);
    }

  for (hash_map<im_mem_ref *, sm_aux *>::iterator iter = aux_map.begin ();
       iter != aux_map.end (); ++iter)
    delete (*iter).second;
}

class ref_always_accessed
{
public:
  ref_always_accessed (class loop *loop_, bool stored_p_)
      : loop (loop_), stored_p (stored_p_) {}
  bool operator () (mem_ref_loc *loc);
  class loop *loop;
  bool stored_p;
};

bool
ref_always_accessed::operator () (mem_ref_loc *loc)
{
  class loop *must_exec;

  struct lim_aux_data *lim_data = get_lim_data (loc->stmt);
  if (!lim_data)
    return false;

  /* If we require an always executed store make sure the statement
     is a store.  */
  if (stored_p)
    {
      tree lhs = gimple_get_lhs (loc->stmt);
      if (!lhs
	  || !(DECL_P (lhs) || REFERENCE_CLASS_P (lhs)))
	return false;
    }

  must_exec = lim_data->always_executed_in;
  if (!must_exec)
    return false;

  if (must_exec == loop
      || flow_loop_nested_p (must_exec, loop))
    return true;

  return false;
}

/* Returns true if REF is always accessed in LOOP.  If STORED_P is true
   make sure REF is always stored to in LOOP.  */

static bool
ref_always_accessed_p (class loop *loop, im_mem_ref *ref, bool stored_p)
{
  return for_all_locs_in_loop (loop, ref,
			       ref_always_accessed (loop, stored_p));
}

/* Returns true if REF1 and REF2 are independent.  */

static bool
refs_independent_p (im_mem_ref *ref1, im_mem_ref *ref2, bool tbaa_p)
{
  if (ref1 == ref2)
    return true;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Querying dependency of refs %u and %u: ",
	     ref1->id, ref2->id);

  if (mem_refs_may_alias_p (ref1, ref2, &memory_accesses.ttae_cache, tbaa_p))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "dependent.\n");
      return false;
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "independent.\n");
      return true;
    }
}

/* Returns true if REF is independent on all other accessess in LOOP.
   KIND specifies the kind of dependence to consider.
     lim_raw assumes REF is not stored in LOOP and disambiguates RAW
	     dependences so if true REF can be hoisted out of LOOP
     sm_war disambiguates a store REF against all other loads to see
	    whether the store can be sunk across loads out of LOOP
     sm_waw disambiguates a store REF against all other stores to see
	    whether the store can be sunk across stores out of LOOP.  */

static bool
ref_indep_loop_p (class loop *loop, im_mem_ref *ref, dep_kind kind)
{
  bool indep_p = true;
  bitmap refs_to_check;

  if (kind == sm_war)
    refs_to_check = &memory_accesses.refs_loaded_in_loop[loop->num];
  else
    refs_to_check = &memory_accesses.refs_stored_in_loop[loop->num];

  if (bitmap_bit_p (refs_to_check, UNANALYZABLE_MEM_ID)
      || ref->mem.ref == error_mark_node)
    indep_p = false;
  else
    {
      /* tri-state, { unknown, independent, dependent }  */
      dep_state state = query_loop_dependence (loop, ref, kind);
      if (state != dep_unknown)
	return state == dep_independent ? true : false;

      class loop *inner = loop->inner;
      while (inner)
	{
	  if (!ref_indep_loop_p (inner, ref, kind))
	    {
	      indep_p = false;
	      break;
	    }
	  inner = inner->next;
	}

      if (indep_p)
	{
	  unsigned i;
	  bitmap_iterator bi;
	  EXECUTE_IF_SET_IN_BITMAP (refs_to_check, 0, i, bi)
	    {
	      im_mem_ref *aref = memory_accesses.refs_list[i];
	      if (aref->mem.ref == error_mark_node)
		{
		  gimple *stmt = aref->accesses_in_loop[0].stmt;
		  if ((kind == sm_war
		       && ref_maybe_used_by_stmt_p (stmt, &ref->mem,
						    kind != sm_waw))
		      || stmt_may_clobber_ref_p_1 (stmt, &ref->mem,
						   kind != sm_waw))
		    {
		      indep_p = false;
		      break;
		    }
		}
	      else if (!refs_independent_p (ref, aref, kind != sm_waw))
		{
		  indep_p = false;
		  break;
		}
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Querying %s dependencies of ref %u in loop %d: %s\n",
	     kind == lim_raw ? "RAW" : (kind == sm_war ? "SM WAR" : "SM WAW"),
	     ref->id, loop->num, indep_p ? "independent" : "dependent");

  /* Record the computed result in the cache.  */
  record_loop_dependence (loop, ref, kind,
			  indep_p ? dep_independent : dep_dependent);

  return indep_p;
}

class ref_in_loop_hot_body
{
public:
  ref_in_loop_hot_body (class loop *loop_) : l (loop_) {}
  bool operator () (mem_ref_loc *loc);
  class loop *l;
};

/* Check the coldest loop between loop L and innermost loop.  If there is one
   cold loop between L and INNER_LOOP, store motion can be performed, otherwise
   no cold loop means no store motion.  get_coldest_out_loop also handles cases
   when l is inner_loop.  */
bool
ref_in_loop_hot_body::operator () (mem_ref_loc *loc)
{
  basic_block curr_bb = gimple_bb (loc->stmt);
  class loop *inner_loop = curr_bb->loop_father;
  return get_coldest_out_loop (l, inner_loop, curr_bb);
}


/* Returns true if we can perform store motion of REF from LOOP.  */

static bool
can_sm_ref_p (class loop *loop, im_mem_ref *ref)
{
  tree base;

  /* Can't hoist unanalyzable refs.  */
  if (!MEM_ANALYZABLE (ref))
    return false;

  /* Can't hoist/sink aggregate copies.  */
  if (ref->mem.ref == error_mark_node)
    return false;

  /* It should be movable.  */
  if (!is_gimple_reg_type (TREE_TYPE (ref->mem.ref))
      || TREE_THIS_VOLATILE (ref->mem.ref)
      || !for_each_index (&ref->mem.ref, may_move_till, loop))
    return false;

  /* If it can throw fail, we do not properly update EH info.  */
  if (tree_could_throw_p (ref->mem.ref))
    return false;

  /* If it can trap, it must be always executed in LOOP.
     Readonly memory locations may trap when storing to them, but
     tree_could_trap_p is a predicate for rvalues, so check that
     explicitly.  */
  base = get_base_address (ref->mem.ref);
  if ((tree_could_trap_p (ref->mem.ref)
       || (DECL_P (base) && TREE_READONLY (base)))
      /* ???  We can at least use false here, allowing loads?  We
	 are forcing conditional stores if the ref is not always
	 stored to later anyway.  So this would only guard
	 the load we need to emit.  Thus when the ref is not
	 loaded we can elide this completely?  */
      && !ref_always_accessed_p (loop, ref, true))
    return false;

  /* Verify all loads of ref can be hoisted.  */
  if (ref->loaded
      && bitmap_bit_p (ref->loaded, loop->num)
      && !ref_indep_loop_p (loop, ref, lim_raw))
    return false;

  /* Verify the candidate can be disambiguated against all loads,
     that is, we can elide all in-loop stores.  Disambiguation
     against stores is done later when we cannot guarantee preserving
     the order of stores.  */
  if (!ref_indep_loop_p (loop, ref, sm_war))
    return false;

  /* Verify whether the candidate is hot for LOOP.  Only do store motion if the
    candidate's profile count is hot.  Statement in cold BB shouldn't be moved
    out of it's loop_father.  */
  if (!for_all_locs_in_loop (loop, ref, ref_in_loop_hot_body (loop)))
    return false;

  return true;
}

/* Marks the references in LOOP for that store motion should be performed
   in REFS_TO_SM.  SM_EXECUTED is the set of references for that store
   motion was performed in one of the outer loops.  */

static void
find_refs_for_sm (class loop *loop, bitmap sm_executed, bitmap refs_to_sm)
{
  bitmap refs = &memory_accesses.all_refs_stored_in_loop[loop->num];
  unsigned i;
  bitmap_iterator bi;
  im_mem_ref *ref;

  EXECUTE_IF_AND_COMPL_IN_BITMAP (refs, sm_executed, 0, i, bi)
    {
      ref = memory_accesses.refs_list[i];
      if (can_sm_ref_p (loop, ref) && dbg_cnt (lim))
	bitmap_set_bit (refs_to_sm, i);
    }
}

/* Checks whether LOOP (with exits stored in EXITS array) is suitable
   for a store motion optimization (i.e. whether we can insert statement
   on its exits).  */

static bool
loop_suitable_for_sm (class loop *loop ATTRIBUTE_UNUSED,
		      const vec<edge> &exits)
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
store_motion_loop (class loop *loop, bitmap sm_executed)
{
  auto_vec<edge> exits = get_loop_exit_edges (loop);
  class loop *subloop;
  bitmap sm_in_loop = BITMAP_ALLOC (&lim_bitmap_obstack);

  if (loop_suitable_for_sm (loop, exits))
    {
      find_refs_for_sm (loop, sm_executed, sm_in_loop);
      if (!bitmap_empty_p (sm_in_loop))
	hoist_memory_references (loop, sm_in_loop, exits);
    }

  bitmap_ior_into (sm_executed, sm_in_loop);
  for (subloop = loop->inner; subloop != NULL; subloop = subloop->next)
    store_motion_loop (subloop, sm_executed);
  bitmap_and_compl_into (sm_executed, sm_in_loop);
  BITMAP_FREE (sm_in_loop);
}

/* Try to perform store motion for all memory references modified inside
   loops.  */

static void
do_store_motion (void)
{
  class loop *loop;
  bitmap sm_executed = BITMAP_ALLOC (&lim_bitmap_obstack);

  for (loop = current_loops->tree_root->inner; loop != NULL; loop = loop->next)
    store_motion_loop (loop, sm_executed);

  BITMAP_FREE (sm_executed);
}

/* Fills ALWAYS_EXECUTED_IN information for basic blocks of LOOP, i.e.
   for each such basic block bb records the outermost loop for that execution
   of its header implies execution of bb.  CONTAINS_CALL is the bitmap of
   blocks that contain a nonpure call.  */

static void
fill_always_executed_in_1 (class loop *loop, sbitmap contains_call)
{
  basic_block bb = NULL, last = NULL;
  edge e;
  class loop *inn_loop = loop;

  if (ALWAYS_EXECUTED_IN (loop->header) == NULL)
    {
      auto_vec<basic_block, 64> worklist;
      worklist.reserve_exact (loop->num_nodes);
      worklist.quick_push (loop->header);
      do
	{
	  edge_iterator ei;
	  bb = worklist.pop ();

	  if (!flow_bb_inside_loop_p (inn_loop, bb))
	    {
	      /* When we are leaving a possibly infinite inner loop
		 we have to stop processing.  */
	      if (!finite_loop_p (inn_loop))
		break;
	      /* If the loop was finite we can continue with processing
		 the loop we exited to.  */
	      inn_loop = bb->loop_father;
	    }

	  if (dominated_by_p (CDI_DOMINATORS, loop->latch, bb))
	    last = bb;

	  if (bitmap_bit_p (contains_call, bb->index))
	    break;

	  /* If LOOP exits from this BB stop processing.  */
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (!flow_bb_inside_loop_p (loop, e->dest))
	      break;
	  if (e)
	    break;

	  /* A loop might be infinite (TODO use simple loop analysis
	     to disprove this if possible).  */
	  if (bb->flags & BB_IRREDUCIBLE_LOOP)
	    break;

	  if (bb->loop_father->header == bb)
	    /* Record that we enter into a subloop since it might not
	       be finite.  */
	    /* ???  Entering into a not always executed subloop makes
	       fill_always_executed_in quadratic in loop depth since
	       we walk those loops N times.  This is not a problem
	       in practice though, see PR102253 for a worst-case testcase.  */
	    inn_loop = bb->loop_father;

	  /* Walk the body of LOOP sorted by dominance relation.  Additionally,
	     if a basic block S dominates the latch, then only blocks dominated
	     by S are after it.
	     This is get_loop_body_in_dom_order using a worklist algorithm and
	     stopping once we are no longer interested in visiting further
	     blocks.  */
	  unsigned old_len = worklist.length ();
	  unsigned postpone = 0;
	  for (basic_block son = first_dom_son (CDI_DOMINATORS, bb);
	       son;
	       son = next_dom_son (CDI_DOMINATORS, son))
	    {
	      if (!flow_bb_inside_loop_p (loop, son))
		continue;
	      if (dominated_by_p (CDI_DOMINATORS, loop->latch, son))
		postpone = worklist.length ();
	      worklist.quick_push (son);
	    }
	  if (postpone)
	    /* Postponing the block that dominates the latch means
	       processing it last and thus putting it earliest in the
	       worklist.  */
	    std::swap (worklist[old_len], worklist[postpone]);
	}
      while (!worklist.is_empty ());

      while (1)
	{
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "BB %d is always executed in loop %d\n",
			 last->index, loop->num);
	  SET_ALWAYS_EXECUTED_IN (last, loop);
	  if (last == loop->header)
	    break;
	  last = get_immediate_dominator (CDI_DOMINATORS, last);
	}
    }

  for (loop = loop->inner; loop; loop = loop->next)
    fill_always_executed_in_1 (loop, contains_call);
}

/* Fills ALWAYS_EXECUTED_IN information for basic blocks, i.e.
   for each such basic block bb records the outermost loop for that execution
   of its header implies execution of bb.  */

static void
fill_always_executed_in (void)
{
  basic_block bb;
  class loop *loop;

  auto_sbitmap contains_call (last_basic_block_for_fn (cfun));
  bitmap_clear (contains_call);
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  if (nonpure_call_p (gsi_stmt (gsi)))
	    break;
	}

      if (!gsi_end_p (gsi))
	bitmap_set_bit (contains_call, bb->index);
    }

  for (loop = current_loops->tree_root->inner; loop; loop = loop->next)
    fill_always_executed_in_1 (loop, contains_call);
}

/* Find the coldest loop preheader for LOOP, also find the nearest hotter loop
   to LOOP.  Then recursively iterate each inner loop.  */

void
fill_coldest_and_hotter_out_loop (class loop *coldest_loop,
				  class loop *hotter_loop, class loop *loop)
{
  if (bb_colder_than_loop_preheader (loop_preheader_edge (loop)->src,
				     coldest_loop))
    coldest_loop = loop;

  coldest_outermost_loop[loop->num] = coldest_loop;

  hotter_than_inner_loop[loop->num] = NULL;
  class loop *outer_loop = loop_outer (loop);
  if (hotter_loop
      && bb_colder_than_loop_preheader (loop_preheader_edge (loop)->src,
					hotter_loop))
    hotter_than_inner_loop[loop->num] = hotter_loop;

  if (outer_loop && outer_loop != current_loops->tree_root
      && bb_colder_than_loop_preheader (loop_preheader_edge (loop)->src,
					outer_loop))
    hotter_than_inner_loop[loop->num] = outer_loop;

  if (dump_enabled_p ())
    {
      dump_printf (MSG_NOTE, "loop %d's coldest_outermost_loop is %d, ",
		   loop->num, coldest_loop->num);
      if (hotter_than_inner_loop[loop->num])
	dump_printf (MSG_NOTE, "hotter_than_inner_loop is %d\n",
		     hotter_than_inner_loop[loop->num]->num);
      else
	dump_printf (MSG_NOTE, "hotter_than_inner_loop is NULL\n");
    }

  class loop *inner_loop;
  for (inner_loop = loop->inner; inner_loop; inner_loop = inner_loop->next)
    fill_coldest_and_hotter_out_loop (coldest_loop,
				      hotter_than_inner_loop[loop->num],
				      inner_loop);
}

/* Compute the global information needed by the loop invariant motion pass.  */

static void
tree_ssa_lim_initialize (bool store_motion)
{
  unsigned i;

  bitmap_obstack_initialize (&lim_bitmap_obstack);
  gcc_obstack_init (&mem_ref_obstack);
  lim_aux_data_map = new hash_map<gimple *, lim_aux_data *>;

  if (flag_tm)
    compute_transaction_bits ();

  memory_accesses.refs = new hash_table<mem_ref_hasher> (100);
  memory_accesses.refs_list.create (100);
  /* Allocate a special, unanalyzable mem-ref with ID zero.  */
  memory_accesses.refs_list.quick_push
    (mem_ref_alloc (NULL, 0, UNANALYZABLE_MEM_ID));

  memory_accesses.refs_loaded_in_loop.create (number_of_loops (cfun));
  memory_accesses.refs_loaded_in_loop.quick_grow_cleared (number_of_loops (cfun));
  memory_accesses.refs_stored_in_loop.create (number_of_loops (cfun));
  memory_accesses.refs_stored_in_loop.quick_grow_cleared (number_of_loops (cfun));
  if (store_motion)
    {
      memory_accesses.all_refs_stored_in_loop.create (number_of_loops (cfun));
      memory_accesses.all_refs_stored_in_loop.quick_grow_cleared
						      (number_of_loops (cfun));
    }

  for (i = 0; i < number_of_loops (cfun); i++)
    {
      bitmap_initialize (&memory_accesses.refs_loaded_in_loop[i],
			 &lim_bitmap_obstack);
      bitmap_initialize (&memory_accesses.refs_stored_in_loop[i],
			 &lim_bitmap_obstack);
      if (store_motion)
	bitmap_initialize (&memory_accesses.all_refs_stored_in_loop[i],
			   &lim_bitmap_obstack);
    }

  memory_accesses.ttae_cache = NULL;

  /* Initialize bb_loop_postorder with a mapping from loop->num to
     its postorder index.  */
  i = 0;
  bb_loop_postorder = XNEWVEC (unsigned, number_of_loops (cfun));
  for (auto loop : loops_list (cfun, LI_FROM_INNERMOST))
    bb_loop_postorder[loop->num] = i++;
}

/* Cleans up after the invariant motion pass.  */

static void
tree_ssa_lim_finalize (void)
{
  basic_block bb;
  unsigned i;
  im_mem_ref *ref;

  FOR_EACH_BB_FN (bb, cfun)
    SET_ALWAYS_EXECUTED_IN (bb, NULL);

  bitmap_obstack_release (&lim_bitmap_obstack);
  delete lim_aux_data_map;

  delete memory_accesses.refs;
  memory_accesses.refs = NULL;

  FOR_EACH_VEC_ELT (memory_accesses.refs_list, i, ref)
    memref_free (ref);
  memory_accesses.refs_list.release ();
  obstack_free (&mem_ref_obstack, NULL);

  memory_accesses.refs_loaded_in_loop.release ();
  memory_accesses.refs_stored_in_loop.release ();
  memory_accesses.all_refs_stored_in_loop.release ();

  if (memory_accesses.ttae_cache)
    free_affine_expand_cache (&memory_accesses.ttae_cache);

  free (bb_loop_postorder);

  coldest_outermost_loop.release ();
  hotter_than_inner_loop.release ();
}

/* Moves invariants from loops.  Only "expensive" invariants are moved out --
   i.e. those that are likely to be win regardless of the register pressure.
   Only perform store motion if STORE_MOTION is true.  */

unsigned int
loop_invariant_motion_in_fun (function *fun, bool store_motion)
{
  unsigned int todo = 0;

  tree_ssa_lim_initialize (store_motion);

  mark_ssa_maybe_undefs ();

  /* Gathers information about memory accesses in the loops.  */
  analyze_memory_references (store_motion);

  /* Fills ALWAYS_EXECUTED_IN information for basic blocks.  */
  fill_always_executed_in ();

  /* Pre-compute coldest outermost loop and nearest hotter loop of each loop.
   */
  class loop *loop;
  coldest_outermost_loop.create (number_of_loops (cfun));
  coldest_outermost_loop.safe_grow_cleared (number_of_loops (cfun));
  hotter_than_inner_loop.create (number_of_loops (cfun));
  hotter_than_inner_loop.safe_grow_cleared (number_of_loops (cfun));
  for (loop = current_loops->tree_root->inner; loop != NULL; loop = loop->next)
    fill_coldest_and_hotter_out_loop (loop, NULL, loop);

  int *rpo = XNEWVEC (int, last_basic_block_for_fn (fun));
  int n = pre_and_rev_post_order_compute_fn (fun, NULL, rpo, false);

  /* For each statement determine the outermost loop in that it is
     invariant and cost for computing the invariant.  */
  for (int i = 0; i < n; ++i)
    compute_invariantness (BASIC_BLOCK_FOR_FN (fun, rpo[i]));

  /* Execute store motion.  Force the necessary invariants to be moved
     out of the loops as well.  */
  if (store_motion)
    do_store_motion ();

  free (rpo);
  rpo = XNEWVEC (int, last_basic_block_for_fn (fun));
  n = pre_and_rev_post_order_compute_fn (fun, NULL, rpo, false);

  /* Move the expressions that are expensive enough.  */
  for (int i = 0; i < n; ++i)
    todo |= move_computations_worker (BASIC_BLOCK_FOR_FN (fun, rpo[i]));

  free (rpo);

  gsi_commit_edge_inserts ();
  if (need_ssa_update_p (fun))
    rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);

  tree_ssa_lim_finalize ();

  return todo;
}

/* Loop invariant motion pass.  */

namespace {

const pass_data pass_data_lim =
{
  GIMPLE_PASS, /* type */
  "lim", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LIM, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lim : public gimple_opt_pass
{
public:
  pass_lim (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lim, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_lim (m_ctxt); }
  bool gate (function *) final override { return flag_tree_loop_im != 0; }
  unsigned int execute (function *) final override;

}; // class pass_lim

unsigned int
pass_lim::execute (function *fun)
{
  bool in_loop_pipeline = scev_initialized_p ();
  if (!in_loop_pipeline)
    loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);

  if (number_of_loops (fun) <= 1)
    return 0;
  unsigned int todo = loop_invariant_motion_in_fun (fun, flag_move_loop_stores);

  if (!in_loop_pipeline)
    loop_optimizer_finalize ();
  else
    scev_reset ();
  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_lim (gcc::context *ctxt)
{
  return new pass_lim (ctxt);
}


