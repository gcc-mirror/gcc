/* Induction variable optimizations.
   Copyright (C) 2003-2016 Free Software Foundation, Inc.

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

/* This pass tries to find the optimal set of induction variables for the loop.
   It optimizes just the basic linear induction variables (although adding
   support for other types should not be too hard).  It includes the
   optimizations commonly known as strength reduction, induction variable
   coalescing and induction variable elimination.  It does it in the
   following steps:

   1) The interesting uses of induction variables are found.  This includes

      -- uses of induction variables in non-linear expressions
      -- addresses of arrays
      -- comparisons of induction variables

      Note the interesting uses are categorized and handled in group.
      Generally, address type uses are grouped together if their iv bases
      are different in constant offset.

   2) Candidates for the induction variables are found.  This includes

      -- old induction variables
      -- the variables defined by expressions derived from the "interesting
	 groups/uses" above

   3) The optimal (w.r. to a cost function) set of variables is chosen.  The
      cost function assigns a cost to sets of induction variables and consists
      of three parts:

      -- The group/use costs.  Each of the interesting groups/uses chooses
	 the best induction variable in the set and adds its cost to the sum.
	 The cost reflects the time spent on modifying the induction variables
	 value to be usable for the given purpose (adding base and offset for
	 arrays, etc.).
      -- The variable costs.  Each of the variables has a cost assigned that
	 reflects the costs associated with incrementing the value of the
	 variable.  The original variables are somewhat preferred.
      -- The set cost.  Depending on the size of the set, extra cost may be
	 added to reflect register pressure.

      All the costs are defined in a machine-specific way, using the target
      hooks and machine descriptions to determine them.

   4) The trees are transformed to use the new variables, the dead code is
      removed.

   All of this is done loop by loop.  Doing it globally is theoretically
   possible, it might give a better performance and it might enable us
   to decide costs more precisely, but getting all the interactions right
   would be complicated.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "tm_p.h"
#include "ssa.h"
#include "expmed.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "explow.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "params.h"
#include "tree-affine.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-address.h"
#include "builtins.h"
#include "tree-vectorizer.h"

/* FIXME: Expressions are expanded to RTL in this pass to determine the
   cost of different addressing modes.  This should be moved to a TBD
   interface between the GIMPLE and RTL worlds.  */

/* The infinite cost.  */
#define INFTY 10000000

#define AVG_LOOP_NITER(LOOP) 5

/* Returns the expected number of loop iterations for LOOP.
   The average trip count is computed from profile data if it
   exists. */

static inline HOST_WIDE_INT
avg_loop_niter (struct loop *loop)
{
  HOST_WIDE_INT niter = estimated_stmt_executions_int (loop);
  if (niter == -1)
    {
      niter = max_stmt_executions_int (loop);
      if (niter == -1 || niter > AVG_LOOP_NITER (loop))
        return AVG_LOOP_NITER (loop);
    }

  return niter;
}

struct iv_use;

/* Representation of the induction variable.  */
struct iv
{
  tree base;		/* Initial value of the iv.  */
  tree base_object;	/* A memory object to that the induction variable points.  */
  tree step;		/* Step of the iv (constant only).  */
  tree ssa_name;	/* The ssa name with the value.  */
  struct iv_use *nonlin_use;	/* The identifier in the use if it is the case.  */
  bool biv_p;		/* Is it a biv?  */
  bool no_overflow;	/* True if the iv doesn't overflow.  */
  bool have_address_use;/* For biv, indicate if it's used in any address
			   type use.  */
};

/* Per-ssa version information (induction variable descriptions, etc.).  */
struct version_info
{
  tree name;		/* The ssa name.  */
  struct iv *iv;	/* Induction variable description.  */
  bool has_nonlin_use;	/* For a loop-level invariant, whether it is used in
			   an expression that is not an induction variable.  */
  bool preserve_biv;	/* For the original biv, whether to preserve it.  */
  unsigned inv_id;	/* Id of an invariant.  */
};

/* Types of uses.  */
enum use_type
{
  USE_NONLINEAR_EXPR,	/* Use in a nonlinear expression.  */
  USE_ADDRESS,		/* Use in an address.  */
  USE_COMPARE		/* Use is a compare.  */
};

/* Cost of a computation.  */
struct comp_cost
{
  int cost;		/* The runtime cost.  */
  unsigned complexity;	/* The estimate of the complexity of the code for
			   the computation (in no concrete units --
			   complexity field should be larger for more
			   complex expressions and addressing modes).  */
  int scratch;		/* Scratch used during cost computation.  */
};

static const comp_cost no_cost = {0, 0, 0};
static const comp_cost infinite_cost = {INFTY, INFTY, INFTY};

/* The candidate - cost pair.  */
struct cost_pair
{
  struct iv_cand *cand;	/* The candidate.  */
  comp_cost cost;	/* The cost.  */
  bitmap depends_on;	/* The list of invariants that have to be
			   preserved.  */
  tree value;		/* For final value elimination, the expression for
			   the final value of the iv.  For iv elimination,
			   the new bound to compare with.  */
  enum tree_code comp;	/* For iv elimination, the comparison.  */
  int inv_expr_id;      /* Loop invariant expression id.  */
};

/* Use.  */
struct iv_use
{
  unsigned id;		/* The id of the use.  */
  unsigned group_id;	/* The group id the use belongs to.  */
  enum use_type type;	/* Type of the use.  */
  struct iv *iv;	/* The induction variable it is based on.  */
  gimple *stmt;		/* Statement in that it occurs.  */
  tree *op_p;		/* The place where it occurs.  */

  tree addr_base;	/* Base address with const offset stripped.  */
  unsigned HOST_WIDE_INT addr_offset;
			/* Const offset stripped from base address.  */
};

/* Group of uses.  */
struct iv_group
{
  /* The id of the group.  */
  unsigned id;
  /* Uses of the group are of the same type.  */
  enum use_type type;
  /* The set of "related" IV candidates, plus the important ones.  */
  bitmap related_cands;
  /* Number of IV candidates in the cost_map.  */
  unsigned n_map_members;
  /* The costs wrto the iv candidates.  */
  struct cost_pair *cost_map;
  /* The selected candidate for the group.  */
  struct iv_cand *selected;
  /* Uses in the group.  */
  vec<struct iv_use *> vuses;
};

/* The position where the iv is computed.  */
enum iv_position
{
  IP_NORMAL,		/* At the end, just before the exit condition.  */
  IP_END,		/* At the end of the latch block.  */
  IP_BEFORE_USE,	/* Immediately before a specific use.  */
  IP_AFTER_USE,		/* Immediately after a specific use.  */
  IP_ORIGINAL		/* The original biv.  */
};

/* The induction variable candidate.  */
struct iv_cand
{
  unsigned id;		/* The number of the candidate.  */
  bool important;	/* Whether this is an "important" candidate, i.e. such
			   that it should be considered by all uses.  */
  ENUM_BITFIELD(iv_position) pos : 8;	/* Where it is computed.  */
  gimple *incremented_at;/* For original biv, the statement where it is
			   incremented.  */
  tree var_before;	/* The variable used for it before increment.  */
  tree var_after;	/* The variable used for it after increment.  */
  struct iv *iv;	/* The value of the candidate.  NULL for
			   "pseudocandidate" used to indicate the possibility
			   to replace the final value of an iv by direct
			   computation of the value.  */
  unsigned cost;	/* Cost of the candidate.  */
  unsigned cost_step;	/* Cost of the candidate's increment operation.  */
  struct iv_use *ainc_use; /* For IP_{BEFORE,AFTER}_USE candidates, the place
			      where it is incremented.  */
  bitmap depends_on;	/* The list of invariants that are used in step of the
			   biv.  */
  struct iv *orig_iv;	/* The original iv if this cand is added from biv with
			   smaller type.  */
};

/* Hashtable entry for common candidate derived from iv uses.  */
struct iv_common_cand
{
  tree base;
  tree step;
  /* IV uses from which this common candidate is derived.  */
  auto_vec<struct iv_use *> uses;
  hashval_t hash;
};

/* Hashtable helpers.  */

struct iv_common_cand_hasher : delete_ptr_hash <iv_common_cand>
{
  static inline hashval_t hash (const iv_common_cand *);
  static inline bool equal (const iv_common_cand *, const iv_common_cand *);
};

/* Hash function for possible common candidates.  */

inline hashval_t
iv_common_cand_hasher::hash (const iv_common_cand *ccand)
{
  return ccand->hash;
}

/* Hash table equality function for common candidates.  */

inline bool
iv_common_cand_hasher::equal (const iv_common_cand *ccand1,
			      const iv_common_cand *ccand2)
{
  return (ccand1->hash == ccand2->hash
	  && operand_equal_p (ccand1->base, ccand2->base, 0)
	  && operand_equal_p (ccand1->step, ccand2->step, 0)
	  && (TYPE_PRECISION (TREE_TYPE (ccand1->base))
	      == TYPE_PRECISION (TREE_TYPE (ccand2->base))));
}

/* Loop invariant expression hashtable entry.  */
struct iv_inv_expr_ent
{
  tree expr;
  int id;
  hashval_t hash;
};

/* Hashtable helpers.  */

struct iv_inv_expr_hasher : free_ptr_hash <iv_inv_expr_ent>
{
  static inline hashval_t hash (const iv_inv_expr_ent *);
  static inline bool equal (const iv_inv_expr_ent *, const iv_inv_expr_ent *);
};

/* Hash function for loop invariant expressions.  */

inline hashval_t
iv_inv_expr_hasher::hash (const iv_inv_expr_ent *expr)
{
  return expr->hash;
}

/* Hash table equality function for expressions.  */

inline bool
iv_inv_expr_hasher::equal (const iv_inv_expr_ent *expr1,
			   const iv_inv_expr_ent *expr2)
{
  return expr1->hash == expr2->hash
	 && operand_equal_p (expr1->expr, expr2->expr, 0);
}

struct ivopts_data
{
  /* The currently optimized loop.  */
  struct loop *current_loop;
  source_location loop_loc;

  /* Numbers of iterations for all exits of the current loop.  */
  hash_map<edge, tree_niter_desc *> *niters;

  /* Number of registers used in it.  */
  unsigned regs_used;

  /* The size of version_info array allocated.  */
  unsigned version_info_size;

  /* The array of information for the ssa names.  */
  struct version_info *version_info;

  /* The hashtable of loop invariant expressions created
     by ivopt.  */
  hash_table<iv_inv_expr_hasher> *inv_expr_tab;

  /* Loop invariant expression id.  */
  int inv_expr_id;

  /* The bitmap of indices in version_info whose value was changed.  */
  bitmap relevant;

  /* The uses of induction variables.  */
  vec<iv_group *> vgroups;

  /* The candidates.  */
  vec<iv_cand *> vcands;

  /* A bitmap of important candidates.  */
  bitmap important_candidates;

  /* Cache used by tree_to_aff_combination_expand.  */
  hash_map<tree, name_expansion *> *name_expansion_cache;

  /* The hashtable of common candidates derived from iv uses.  */
  hash_table<iv_common_cand_hasher> *iv_common_cand_tab;

  /* The common candidates.  */
  vec<iv_common_cand *> iv_common_cands;

  /* The maximum invariant id.  */
  unsigned max_inv_id;

  /* Number of no_overflow BIVs which are not used in memory address.  */
  unsigned bivs_not_used_in_addr;

  /* Obstack for iv structure.  */
  struct obstack iv_obstack;

  /* Whether to consider just related and important candidates when replacing a
     use.  */
  bool consider_all_candidates;

  /* Are we optimizing for speed?  */
  bool speed;

  /* Whether the loop body includes any function calls.  */
  bool body_includes_call;

  /* Whether the loop body can only be exited via single exit.  */
  bool loop_single_exit_p;
};

/* An assignment of iv candidates to uses.  */

struct iv_ca
{
  /* The number of uses covered by the assignment.  */
  unsigned upto;

  /* Number of uses that cannot be expressed by the candidates in the set.  */
  unsigned bad_groups;

  /* Candidate assigned to a use, together with the related costs.  */
  struct cost_pair **cand_for_group;

  /* Number of times each candidate is used.  */
  unsigned *n_cand_uses;

  /* The candidates used.  */
  bitmap cands;

  /* The number of candidates in the set.  */
  unsigned n_cands;

  /* Total number of registers needed.  */
  unsigned n_regs;

  /* Total cost of expressing uses.  */
  comp_cost cand_use_cost;

  /* Total cost of candidates.  */
  unsigned cand_cost;

  /* Number of times each invariant is used.  */
  unsigned *n_invariant_uses;

  /* The array holding the number of uses of each loop
     invariant expressions created by ivopt.  */
  unsigned *used_inv_expr;

  /* The number of created loop invariants.  */
  unsigned num_used_inv_expr;

  /* Total cost of the assignment.  */
  comp_cost cost;
};

/* Difference of two iv candidate assignments.  */

struct iv_ca_delta
{
  /* Changed group.  */
  struct iv_group *group;

  /* An old assignment (for rollback purposes).  */
  struct cost_pair *old_cp;

  /* A new assignment.  */
  struct cost_pair *new_cp;

  /* Next change in the list.  */
  struct iv_ca_delta *next;
};

/* Bound on number of candidates below that all candidates are considered.  */

#define CONSIDER_ALL_CANDIDATES_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_CONSIDER_ALL_CANDIDATES_BOUND))

/* If there are more iv occurrences, we just give up (it is quite unlikely that
   optimizing such a loop would help, and it would take ages).  */

#define MAX_CONSIDERED_GROUPS \
  ((unsigned) PARAM_VALUE (PARAM_IV_MAX_CONSIDERED_USES))

/* If there are at most this number of ivs in the set, try removing unnecessary
   ivs from the set always.  */

#define ALWAYS_PRUNE_CAND_SET_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_ALWAYS_PRUNE_CAND_SET_BOUND))

/* The list of trees for that the decl_rtl field must be reset is stored
   here.  */

static vec<tree> decl_rtl_to_reset;

static comp_cost force_expr_to_var_cost (tree, bool);

/* The single loop exit if it dominates the latch, NULL otherwise.  */

edge
single_dom_exit (struct loop *loop)
{
  edge exit = single_exit (loop);

  if (!exit)
    return NULL;

  if (!just_once_each_iteration_p (loop, exit->src))
    return NULL;

  return exit;
}

/* Dumps information about the induction variable IV to FILE.  Don't dump
   variable's name if DUMP_NAME is FALSE.  The information is dumped with
   preceding spaces indicated by INDENT_LEVEL.  */

void
dump_iv (FILE *file, struct iv *iv, bool dump_name, unsigned indent_level)
{
  const char *p;
  const char spaces[9] = {' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '\0'};

  if (indent_level > 4)
    indent_level = 4;
  p = spaces + 8 - (indent_level << 1);

  fprintf (file, "%sIV struct:\n", p);
  if (iv->ssa_name && dump_name)
    {
      fprintf (file, "%s  SSA_NAME:\t", p);
      print_generic_expr (file, iv->ssa_name, TDF_SLIM);
      fprintf (file, "\n");
    }

  fprintf (file, "%s  Type:\t", p);
  print_generic_expr (file, TREE_TYPE (iv->base), TDF_SLIM);
  fprintf (file, "\n");

  fprintf (file, "%s  Base:\t", p);
  print_generic_expr (file, iv->base, TDF_SLIM);
  fprintf (file, "\n");

  fprintf (file, "%s  Step:\t", p);
  print_generic_expr (file, iv->step, TDF_SLIM);
  fprintf (file, "\n");

  if (iv->base_object)
    {
      fprintf (file, "%s  Object:\t", p);
      print_generic_expr (file, iv->base_object, TDF_SLIM);
      fprintf (file, "\n");
    }

  fprintf (file, "%s  Biv:\t%c\n", p, iv->biv_p ? 'Y' : 'N');

  fprintf (file, "%s  Overflowness wrto loop niter:\t%s\n",
	   p, iv->no_overflow ? "No-overflow" : "Overflow");
}

/* Dumps information about the USE to FILE.  */

void
dump_use (FILE *file, struct iv_use *use)
{
  fprintf (file, "  Use %d.%d:\n", use->group_id, use->id);
  fprintf (file, "    At stmt:\t");
  print_gimple_stmt (file, use->stmt, 0, 0);
  fprintf (file, "    At pos:\t");
  if (use->op_p)
    print_generic_expr (file, *use->op_p, TDF_SLIM);
  fprintf (file, "\n");
  dump_iv (file, use->iv, false, 2);
}

/* Dumps information about the uses to FILE.  */

void
dump_groups (FILE *file, struct ivopts_data *data)
{
  unsigned i, j;
  struct iv_group *group;

  for (i = 0; i < data->vgroups.length (); i++)
    {
      group = data->vgroups[i];
      fprintf (file, "Group %d:\n", group->id);
      if (group->type == USE_NONLINEAR_EXPR)
	fprintf (file, "  Type:\tGENERIC\n");
      else if (group->type == USE_ADDRESS)
	fprintf (file, "  Type:\tADDRESS\n");
      else
	{
	  gcc_assert (group->type == USE_COMPARE);
	  fprintf (file, "  Type:\tCOMPARE\n");
	}
      for (j = 0; j < group->vuses.length (); j++)
	dump_use (file, group->vuses[j]);
    }
}

/* Dumps information about induction variable candidate CAND to FILE.  */

void
dump_cand (FILE *file, struct iv_cand *cand)
{
  struct iv *iv = cand->iv;

  fprintf (file, "Candidate %d:\n", cand->id);
  if (cand->depends_on)
    {
      fprintf (file, "  Depend on: ");
      dump_bitmap (file, cand->depends_on);
    }

  if (cand->var_before)
    {
      fprintf (file, "  Var befor: ");
      print_generic_expr (file, cand->var_before, TDF_SLIM);
      fprintf (file, "\n");
    }
  if (cand->var_after)
    {
      fprintf (file, "  Var after: ");
      print_generic_expr (file, cand->var_after, TDF_SLIM);
      fprintf (file, "\n");
    }

  switch (cand->pos)
    {
    case IP_NORMAL:
      fprintf (file, "  Incr POS: before exit test\n");
      break;

    case IP_BEFORE_USE:
      fprintf (file, "  Incr POS: before use %d\n", cand->ainc_use->id);
      break;

    case IP_AFTER_USE:
      fprintf (file, "  Incr POS: after use %d\n", cand->ainc_use->id);
      break;

    case IP_END:
      fprintf (file, "  Incr POS: at end\n");
      break;

    case IP_ORIGINAL:
      fprintf (file, "  Incr POS: orig biv\n");
      break;
    }

  dump_iv (file, iv, false, 1);
}

/* Returns the info for ssa version VER.  */

static inline struct version_info *
ver_info (struct ivopts_data *data, unsigned ver)
{
  return data->version_info + ver;
}

/* Returns the info for ssa name NAME.  */

static inline struct version_info *
name_info (struct ivopts_data *data, tree name)
{
  return ver_info (data, SSA_NAME_VERSION (name));
}

/* Returns true if STMT is after the place where the IP_NORMAL ivs will be
   emitted in LOOP.  */

static bool
stmt_after_ip_normal_pos (struct loop *loop, gimple *stmt)
{
  basic_block bb = ip_normal_pos (loop), sbb = gimple_bb (stmt);

  gcc_assert (bb);

  if (sbb == loop->latch)
    return true;

  if (sbb != bb)
    return false;

  return stmt == last_stmt (bb);
}

/* Returns true if STMT if after the place where the original induction
   variable CAND is incremented.  If TRUE_IF_EQUAL is set, we return true
   if the positions are identical.  */

static bool
stmt_after_inc_pos (struct iv_cand *cand, gimple *stmt, bool true_if_equal)
{
  basic_block cand_bb = gimple_bb (cand->incremented_at);
  basic_block stmt_bb = gimple_bb (stmt);

  if (!dominated_by_p (CDI_DOMINATORS, stmt_bb, cand_bb))
    return false;

  if (stmt_bb != cand_bb)
    return true;

  if (true_if_equal
      && gimple_uid (stmt) == gimple_uid (cand->incremented_at))
    return true;
  return gimple_uid (stmt) > gimple_uid (cand->incremented_at);
}

/* Returns true if STMT if after the place where the induction variable
   CAND is incremented in LOOP.  */

static bool
stmt_after_increment (struct loop *loop, struct iv_cand *cand, gimple *stmt)
{
  switch (cand->pos)
    {
    case IP_END:
      return false;

    case IP_NORMAL:
      return stmt_after_ip_normal_pos (loop, stmt);

    case IP_ORIGINAL:
    case IP_AFTER_USE:
      return stmt_after_inc_pos (cand, stmt, false);

    case IP_BEFORE_USE:
      return stmt_after_inc_pos (cand, stmt, true);

    default:
      gcc_unreachable ();
    }
}

/* Returns true if EXP is a ssa name that occurs in an abnormal phi node.  */

static bool
abnormal_ssa_name_p (tree exp)
{
  if (!exp)
    return false;

  if (TREE_CODE (exp) != SSA_NAME)
    return false;

  return SSA_NAME_OCCURS_IN_ABNORMAL_PHI (exp) != 0;
}

/* Returns false if BASE or INDEX contains a ssa name that occurs in an
   abnormal phi node.  Callback for for_each_index.  */

static bool
idx_contains_abnormal_ssa_name_p (tree base, tree *index,
				  void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      if (abnormal_ssa_name_p (TREE_OPERAND (base, 2)))
	return false;
      if (abnormal_ssa_name_p (TREE_OPERAND (base, 3)))
	return false;
    }

  return !abnormal_ssa_name_p (*index);
}

/* Returns true if EXPR contains a ssa name that occurs in an
   abnormal phi node.  */

bool
contains_abnormal_ssa_name_p (tree expr)
{
  enum tree_code code;
  enum tree_code_class codeclass;

  if (!expr)
    return false;

  code = TREE_CODE (expr);
  codeclass = TREE_CODE_CLASS (code);

  if (code == SSA_NAME)
    return SSA_NAME_OCCURS_IN_ABNORMAL_PHI (expr) != 0;

  if (code == INTEGER_CST
      || is_gimple_min_invariant (expr))
    return false;

  if (code == ADDR_EXPR)
    return !for_each_index (&TREE_OPERAND (expr, 0),
			    idx_contains_abnormal_ssa_name_p,
			    NULL);

  if (code == COND_EXPR)
    return contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 0))
      || contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 1))
      || contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 2));

  switch (codeclass)
    {
    case tcc_binary:
    case tcc_comparison:
      if (contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 1)))
	return true;

      /* Fallthru.  */
    case tcc_unary:
      if (contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 0)))
	return true;

      break;

    default:
      gcc_unreachable ();
    }

  return false;
}

/*  Returns the structure describing number of iterations determined from
    EXIT of DATA->current_loop, or NULL if something goes wrong.  */

static struct tree_niter_desc *
niter_for_exit (struct ivopts_data *data, edge exit)
{
  struct tree_niter_desc *desc;
  tree_niter_desc **slot;

  if (!data->niters)
    {
      data->niters = new hash_map<edge, tree_niter_desc *>;
      slot = NULL;
    }
  else
    slot = data->niters->get (exit);

  if (!slot)
    {
      /* Try to determine number of iterations.  We cannot safely work with ssa
         names that appear in phi nodes on abnormal edges, so that we do not
         create overlapping life ranges for them (PR 27283).  */
      desc = XNEW (struct tree_niter_desc);
      if (!number_of_iterations_exit (data->current_loop,
				      exit, desc, true)
     	  || contains_abnormal_ssa_name_p (desc->niter))
	{
	  XDELETE (desc);
	  desc = NULL;
	}
      data->niters->put (exit, desc);
    }
  else
    desc = *slot;

  return desc;
}

/* Returns the structure describing number of iterations determined from
   single dominating exit of DATA->current_loop, or NULL if something
   goes wrong.  */

static struct tree_niter_desc *
niter_for_single_dom_exit (struct ivopts_data *data)
{
  edge exit = single_dom_exit (data->current_loop);

  if (!exit)
    return NULL;

  return niter_for_exit (data, exit);
}

/* Initializes data structures used by the iv optimization pass, stored
   in DATA.  */

static void
tree_ssa_iv_optimize_init (struct ivopts_data *data)
{
  data->version_info_size = 2 * num_ssa_names;
  data->version_info = XCNEWVEC (struct version_info, data->version_info_size);
  data->relevant = BITMAP_ALLOC (NULL);
  data->important_candidates = BITMAP_ALLOC (NULL);
  data->max_inv_id = 0;
  data->niters = NULL;
  data->vgroups.create (20);
  data->vcands.create (20);
  data->inv_expr_tab = new hash_table<iv_inv_expr_hasher> (10);
  data->inv_expr_id = 0;
  data->name_expansion_cache = NULL;
  data->iv_common_cand_tab = new hash_table<iv_common_cand_hasher> (10);
  data->iv_common_cands.create (20);
  decl_rtl_to_reset.create (20);
  gcc_obstack_init (&data->iv_obstack);
}

/* Returns a memory object to that EXPR points.  In case we are able to
   determine that it does not point to any such object, NULL is returned.  */

static tree
determine_base_object (tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  tree base, obj;

  /* If this is a pointer casted to any type, we need to determine
     the base object for the pointer; so handle conversions before
     throwing away non-pointer expressions.  */
  if (CONVERT_EXPR_P (expr))
    return determine_base_object (TREE_OPERAND (expr, 0));

  if (!POINTER_TYPE_P (TREE_TYPE (expr)))
    return NULL_TREE;

  switch (code)
    {
    case INTEGER_CST:
      return NULL_TREE;

    case ADDR_EXPR:
      obj = TREE_OPERAND (expr, 0);
      base = get_base_address (obj);

      if (!base)
	return expr;

      if (TREE_CODE (base) == MEM_REF)
	return determine_base_object (TREE_OPERAND (base, 0));

      return fold_convert (ptr_type_node,
		           build_fold_addr_expr (base));

    case POINTER_PLUS_EXPR:
      return determine_base_object (TREE_OPERAND (expr, 0));

    case PLUS_EXPR:
    case MINUS_EXPR:
      /* Pointer addition is done solely using POINTER_PLUS_EXPR.  */
      gcc_unreachable ();

    default:
      return fold_convert (ptr_type_node, expr);
    }
}

/* Return true if address expression with non-DECL_P operand appears
   in EXPR.  */

static bool
contain_complex_addr_expr (tree expr)
{
  bool res = false;

  STRIP_NOPS (expr);
  switch (TREE_CODE (expr))
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
      res |= contain_complex_addr_expr (TREE_OPERAND (expr, 0));
      res |= contain_complex_addr_expr (TREE_OPERAND (expr, 1));
      break;

    case ADDR_EXPR:
      return (!DECL_P (TREE_OPERAND (expr, 0)));

    default:
      return false;
    }

  return res;
}

/* Allocates an induction variable with given initial value BASE and step STEP
   for loop LOOP.  NO_OVERFLOW implies the iv doesn't overflow.  */

static struct iv *
alloc_iv (struct ivopts_data *data, tree base, tree step,
	  bool no_overflow = false)
{
  tree expr = base;
  struct iv *iv = (struct iv*) obstack_alloc (&data->iv_obstack,
					      sizeof (struct iv));
  gcc_assert (step != NULL_TREE);

  /* Lower address expression in base except ones with DECL_P as operand.
     By doing this:
       1) More accurate cost can be computed for address expressions;
       2) Duplicate candidates won't be created for bases in different
          forms, like &a[0] and &a.  */
  STRIP_NOPS (expr);
  if ((TREE_CODE (expr) == ADDR_EXPR && !DECL_P (TREE_OPERAND (expr, 0)))
      || contain_complex_addr_expr (expr))
    {
      aff_tree comb;
      tree_to_aff_combination (expr, TREE_TYPE (base), &comb);
      base = fold_convert (TREE_TYPE (base), aff_combination_to_tree (&comb));
    }

  iv->base = base;
  iv->base_object = determine_base_object (base);
  iv->step = step;
  iv->biv_p = false;
  iv->nonlin_use = NULL;
  iv->ssa_name = NULL_TREE;
  iv->no_overflow = no_overflow;
  iv->have_address_use = false;

  return iv;
}

/* Sets STEP and BASE for induction variable IV.  NO_OVERFLOW implies the IV
   doesn't overflow.  */

static void
set_iv (struct ivopts_data *data, tree iv, tree base, tree step,
	bool no_overflow)
{
  struct version_info *info = name_info (data, iv);

  gcc_assert (!info->iv);

  bitmap_set_bit (data->relevant, SSA_NAME_VERSION (iv));
  info->iv = alloc_iv (data, base, step, no_overflow);
  info->iv->ssa_name = iv;
}

/* Finds induction variable declaration for VAR.  */

static struct iv *
get_iv (struct ivopts_data *data, tree var)
{
  basic_block bb;
  tree type = TREE_TYPE (var);

  if (!POINTER_TYPE_P (type)
      && !INTEGRAL_TYPE_P (type))
    return NULL;

  if (!name_info (data, var)->iv)
    {
      bb = gimple_bb (SSA_NAME_DEF_STMT (var));

      if (!bb
	  || !flow_bb_inside_loop_p (data->current_loop, bb))
	set_iv (data, var, var, build_int_cst (type, 0), true);
    }

  return name_info (data, var)->iv;
}

/* Return the first non-invariant ssa var found in EXPR.  */

static tree
extract_single_var_from_expr (tree expr)
{
  int i, n;
  tree tmp;
  enum tree_code code;

  if (!expr || is_gimple_min_invariant (expr))
    return NULL;

  code = TREE_CODE (expr);
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
    {
      n = TREE_OPERAND_LENGTH (expr);
      for (i = 0; i < n; i++)
	{
	  tmp = extract_single_var_from_expr (TREE_OPERAND (expr, i));

	  if (tmp)
	    return tmp;
	}
    }
  return (TREE_CODE (expr) == SSA_NAME) ? expr : NULL;
}

/* Finds basic ivs.  */

static bool
find_bivs (struct ivopts_data *data)
{
  gphi *phi;
  affine_iv iv;
  tree step, type, base, stop;
  bool found = false;
  struct loop *loop = data->current_loop;
  gphi_iterator psi;

  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();

      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
	continue;

      if (virtual_operand_p (PHI_RESULT (phi)))
	continue;

      if (!simple_iv (loop, loop, PHI_RESULT (phi), &iv, true))
	continue;

      if (integer_zerop (iv.step))
	continue;

      step = iv.step;
      base = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
      /* Stop expanding iv base at the first ssa var referred by iv step.
	 Ideally we should stop at any ssa var, because that's expensive
	 and unusual to happen, we just do it on the first one.

	 See PR64705 for the rationale.  */
      stop = extract_single_var_from_expr (step);
      base = expand_simple_operations (base, stop);
      if (contains_abnormal_ssa_name_p (base)
	  || contains_abnormal_ssa_name_p (step))
	continue;

      type = TREE_TYPE (PHI_RESULT (phi));
      base = fold_convert (type, base);
      if (step)
	{
	  if (POINTER_TYPE_P (type))
	    step = convert_to_ptrofftype (step);
	  else
	    step = fold_convert (type, step);
	}

      set_iv (data, PHI_RESULT (phi), base, step, iv.no_overflow);
      found = true;
    }

  return found;
}

/* Marks basic ivs.  */

static void
mark_bivs (struct ivopts_data *data)
{
  gphi *phi;
  gimple *def;
  tree var;
  struct iv *iv, *incr_iv;
  struct loop *loop = data->current_loop;
  basic_block incr_bb;
  gphi_iterator psi;

  data->bivs_not_used_in_addr = 0;
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();

      iv = get_iv (data, PHI_RESULT (phi));
      if (!iv)
	continue;

      var = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
      def = SSA_NAME_DEF_STMT (var);
      /* Don't mark iv peeled from other one as biv.  */
      if (def
	  && gimple_code (def) == GIMPLE_PHI
	  && gimple_bb (def) == loop->header)
	continue;

      incr_iv = get_iv (data, var);
      if (!incr_iv)
	continue;

      /* If the increment is in the subloop, ignore it.  */
      incr_bb = gimple_bb (SSA_NAME_DEF_STMT (var));
      if (incr_bb->loop_father != data->current_loop
	  || (incr_bb->flags & BB_IRREDUCIBLE_LOOP))
	continue;

      iv->biv_p = true;
      incr_iv->biv_p = true;
      if (iv->no_overflow)
	data->bivs_not_used_in_addr++;
      if (incr_iv->no_overflow)
	data->bivs_not_used_in_addr++;
    }
}

/* Checks whether STMT defines a linear induction variable and stores its
   parameters to IV.  */

static bool
find_givs_in_stmt_scev (struct ivopts_data *data, gimple *stmt, affine_iv *iv)
{
  tree lhs, stop;
  struct loop *loop = data->current_loop;

  iv->base = NULL_TREE;
  iv->step = NULL_TREE;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs = gimple_assign_lhs (stmt);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  if (!simple_iv (loop, loop_containing_stmt (stmt), lhs, iv, true))
    return false;

  /* Stop expanding iv base at the first ssa var referred by iv step.
     Ideally we should stop at any ssa var, because that's expensive
     and unusual to happen, we just do it on the first one.

     See PR64705 for the rationale.  */
  stop = extract_single_var_from_expr (iv->step);
  iv->base = expand_simple_operations (iv->base, stop);
  if (contains_abnormal_ssa_name_p (iv->base)
      || contains_abnormal_ssa_name_p (iv->step))
    return false;

  /* If STMT could throw, then do not consider STMT as defining a GIV.
     While this will suppress optimizations, we can not safely delete this
     GIV and associated statements, even if it appears it is not used.  */
  if (stmt_could_throw_p (stmt))
    return false;

  return true;
}

/* Finds general ivs in statement STMT.  */

static void
find_givs_in_stmt (struct ivopts_data *data, gimple *stmt)
{
  affine_iv iv;

  if (!find_givs_in_stmt_scev (data, stmt, &iv))
    return;

  set_iv (data, gimple_assign_lhs (stmt), iv.base, iv.step, iv.no_overflow);
}

/* Finds general ivs in basic block BB.  */

static void
find_givs_in_bb (struct ivopts_data *data, basic_block bb)
{
  gimple_stmt_iterator bsi;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    find_givs_in_stmt (data, gsi_stmt (bsi));
}

/* Finds general ivs.  */

static void
find_givs (struct ivopts_data *data)
{
  struct loop *loop = data->current_loop;
  basic_block *body = get_loop_body_in_dom_order (loop);
  unsigned i;

  for (i = 0; i < loop->num_nodes; i++)
    find_givs_in_bb (data, body[i]);
  free (body);
}

/* For each ssa name defined in LOOP determines whether it is an induction
   variable and if so, its initial value and step.  */

static bool
find_induction_variables (struct ivopts_data *data)
{
  unsigned i;
  bitmap_iterator bi;

  if (!find_bivs (data))
    return false;

  find_givs (data);
  mark_bivs (data);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      struct tree_niter_desc *niter = niter_for_single_dom_exit (data);

      if (niter)
	{
	  fprintf (dump_file, "  number of iterations ");
	  print_generic_expr (dump_file, niter->niter, TDF_SLIM);
	  if (!integer_zerop (niter->may_be_zero))
	    {
	      fprintf (dump_file, "; zero if ");
	      print_generic_expr (dump_file, niter->may_be_zero, TDF_SLIM);
	    }
	  fprintf (dump_file, "\n");
	};

      fprintf (dump_file, "\n<Induction Vars>:\n");
      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
	{
	  struct version_info *info = ver_info (data, i);
	  if (info->iv && info->iv->step && !integer_zerop (info->iv->step))
	    dump_iv (dump_file, ver_info (data, i)->iv, true, 0);
	}
    }

  return true;
}

/* Records a use of TYPE at *USE_P in STMT whose value is IV in GROUP.
   For address type use, ADDR_BASE is the stripped IV base, ADDR_OFFSET
   is the const offset stripped from IV base; for other types use, both
   are zero by default.  */

static struct iv_use *
record_use (struct iv_group *group, tree *use_p, struct iv *iv,
	    gimple *stmt, enum use_type type, tree addr_base,
	    unsigned HOST_WIDE_INT addr_offset)
{
  struct iv_use *use = XCNEW (struct iv_use);

  use->id = group->vuses.length ();
  use->group_id = group->id;
  use->type = type;
  use->iv = iv;
  use->stmt = stmt;
  use->op_p = use_p;
  use->addr_base = addr_base;
  use->addr_offset = addr_offset;

  group->vuses.safe_push (use);
  return use;
}

/* Checks whether OP is a loop-level invariant and if so, records it.
   NONLINEAR_USE is true if the invariant is used in a way we do not
   handle specially.  */

static void
record_invariant (struct ivopts_data *data, tree op, bool nonlinear_use)
{
  basic_block bb;
  struct version_info *info;

  if (TREE_CODE (op) != SSA_NAME
      || virtual_operand_p (op))
    return;

  bb = gimple_bb (SSA_NAME_DEF_STMT (op));
  if (bb
      && flow_bb_inside_loop_p (data->current_loop, bb))
    return;

  info = name_info (data, op);
  info->name = op;
  info->has_nonlin_use |= nonlinear_use;
  if (!info->inv_id)
    info->inv_id = ++data->max_inv_id;
  bitmap_set_bit (data->relevant, SSA_NAME_VERSION (op));
}

static tree
strip_offset (tree expr, unsigned HOST_WIDE_INT *offset);

/* Record a group of TYPE.  */

static struct iv_group *
record_group (struct ivopts_data *data, enum use_type type)
{
  struct iv_group *group = XCNEW (struct iv_group);

  group->id = data->vgroups.length ();
  group->type = type;
  group->related_cands = BITMAP_ALLOC (NULL);
  group->vuses.create (1);

  data->vgroups.safe_push (group);
  return group;
}

/* Record a use of TYPE at *USE_P in STMT whose value is IV in a group.
   New group will be created if there is no existing group for the use.  */

static struct iv_use *
record_group_use (struct ivopts_data *data, tree *use_p,
		  struct iv *iv, gimple *stmt, enum use_type type)
{
  tree addr_base = NULL;
  struct iv_group *group = NULL;
  unsigned HOST_WIDE_INT addr_offset = 0;

  /* Record non address type use in a new group.  */
  if (type == USE_ADDRESS && iv->base_object)
    {
      unsigned int i;

      addr_base = strip_offset (iv->base, &addr_offset);
      for (i = 0; i < data->vgroups.length (); i++)
	{
	  struct iv_use *use;

	  group = data->vgroups[i];
	  use = group->vuses[0];
	  if (use->type != USE_ADDRESS || !use->iv->base_object)
	    continue;

	  /* Check if it has the same stripped base and step.  */
	  if (operand_equal_p (iv->base_object, use->iv->base_object, 0)
	      && operand_equal_p (iv->step, use->iv->step, 0)
	      && operand_equal_p (addr_base, use->addr_base, 0))
	    break;
	}
      if (i == data->vgroups.length ())
	group = NULL;
    }

  if (!group)
    group = record_group (data, type);

  return record_use (group, use_p, iv, stmt, type, addr_base, addr_offset);
}

/* Checks whether the use OP is interesting and if so, records it.  */

static struct iv_use *
find_interesting_uses_op (struct ivopts_data *data, tree op)
{
  struct iv *iv;
  gimple *stmt;
  struct iv_use *use;

  if (TREE_CODE (op) != SSA_NAME)
    return NULL;

  iv = get_iv (data, op);
  if (!iv)
    return NULL;

  if (iv->nonlin_use)
    {
      gcc_assert (iv->nonlin_use->type == USE_NONLINEAR_EXPR);
      return iv->nonlin_use;
    }

  if (integer_zerop (iv->step))
    {
      record_invariant (data, op, true);
      return NULL;
    }

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (gimple_code (stmt) == GIMPLE_PHI || is_gimple_assign (stmt));

  use = record_group_use (data, NULL, iv, stmt, USE_NONLINEAR_EXPR);
  iv->nonlin_use = use;
  return use;
}

/* Given a condition in statement STMT, checks whether it is a compare
   of an induction variable and an invariant.  If this is the case,
   CONTROL_VAR is set to location of the iv, BOUND to the location of
   the invariant, IV_VAR and IV_BOUND are set to the corresponding
   induction variable descriptions, and true is returned.  If this is not
   the case, CONTROL_VAR and BOUND are set to the arguments of the
   condition and false is returned.  */

static bool
extract_cond_operands (struct ivopts_data *data, gimple *stmt,
		       tree **control_var, tree **bound,
		       struct iv **iv_var, struct iv **iv_bound)
{
  /* The objects returned when COND has constant operands.  */
  static struct iv const_iv;
  static tree zero;
  tree *op0 = &zero, *op1 = &zero;
  struct iv *iv0 = &const_iv, *iv1 = &const_iv;
  bool ret = false;

  if (gimple_code (stmt) == GIMPLE_COND)
    {
      gcond *cond_stmt = as_a <gcond *> (stmt);
      op0 = gimple_cond_lhs_ptr (cond_stmt);
      op1 = gimple_cond_rhs_ptr (cond_stmt);
    }
  else
    {
      op0 = gimple_assign_rhs1_ptr (stmt);
      op1 = gimple_assign_rhs2_ptr (stmt);
    }

  zero = integer_zero_node;
  const_iv.step = integer_zero_node;

  if (TREE_CODE (*op0) == SSA_NAME)
    iv0 = get_iv (data, *op0);
  if (TREE_CODE (*op1) == SSA_NAME)
    iv1 = get_iv (data, *op1);

  /* Exactly one of the compared values must be an iv, and the other one must
     be an invariant.  */
  if (!iv0 || !iv1)
    goto end;

  if (integer_zerop (iv0->step))
    {
      /* Control variable may be on the other side.  */
      std::swap (op0, op1);
      std::swap (iv0, iv1);
    }
  ret = !integer_zerop (iv0->step) && integer_zerop (iv1->step);

end:
  if (control_var)
    *control_var = op0;
  if (iv_var)
    *iv_var = iv0;
  if (bound)
    *bound = op1;
  if (iv_bound)
    *iv_bound = iv1;

  return ret;
}

/* Checks whether the condition in STMT is interesting and if so,
   records it.  */

static void
find_interesting_uses_cond (struct ivopts_data *data, gimple *stmt)
{
  tree *var_p, *bound_p;
  struct iv *var_iv;

  if (!extract_cond_operands (data, stmt, &var_p, &bound_p, &var_iv, NULL))
    {
      find_interesting_uses_op (data, *var_p);
      find_interesting_uses_op (data, *bound_p);
      return;
    }

  record_group_use (data, NULL, var_iv, stmt, USE_COMPARE);
}

/* Returns the outermost loop EXPR is obviously invariant in
   relative to the loop LOOP, i.e. if all its operands are defined
   outside of the returned loop.  Returns NULL if EXPR is not
   even obviously invariant in LOOP.  */

struct loop *
outermost_invariant_loop_for_expr (struct loop *loop, tree expr)
{
  basic_block def_bb;
  unsigned i, len;

  if (is_gimple_min_invariant (expr))
    return current_loops->tree_root;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      def_bb = gimple_bb (SSA_NAME_DEF_STMT (expr));
      if (def_bb)
	{
	  if (flow_bb_inside_loop_p (loop, def_bb))
	    return NULL;
	  return superloop_at_depth (loop,
				     loop_depth (def_bb->loop_father) + 1);
	}

      return current_loops->tree_root;
    }

  if (!EXPR_P (expr))
    return NULL;

  unsigned maxdepth = 0;
  len = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < len; i++)
    {
      struct loop *ivloop;
      if (!TREE_OPERAND (expr, i))
	continue;

      ivloop = outermost_invariant_loop_for_expr (loop, TREE_OPERAND (expr, i));
      if (!ivloop)
	return NULL;
      maxdepth = MAX (maxdepth, loop_depth (ivloop));
    }

  return superloop_at_depth (loop, maxdepth);
}

/* Returns true if expression EXPR is obviously invariant in LOOP,
   i.e. if all its operands are defined outside of the LOOP.  LOOP
   should not be the function body.  */

bool
expr_invariant_in_loop_p (struct loop *loop, tree expr)
{
  basic_block def_bb;
  unsigned i, len;

  gcc_assert (loop_depth (loop) > 0);

  if (is_gimple_min_invariant (expr))
    return true;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      def_bb = gimple_bb (SSA_NAME_DEF_STMT (expr));
      if (def_bb
	  && flow_bb_inside_loop_p (loop, def_bb))
	return false;

      return true;
    }

  if (!EXPR_P (expr))
    return false;

  len = TREE_OPERAND_LENGTH (expr);
  for (i = 0; i < len; i++)
    if (TREE_OPERAND (expr, i)
	&& !expr_invariant_in_loop_p (loop, TREE_OPERAND (expr, i)))
      return false;

  return true;
}

/* Given expression EXPR which computes inductive values with respect
   to loop recorded in DATA, this function returns biv from which EXPR
   is derived by tracing definition chains of ssa variables in EXPR.  */

static struct iv*
find_deriving_biv_for_expr (struct ivopts_data *data, tree expr)
{
  struct iv *iv;
  unsigned i, n;
  tree e2, e1;
  enum tree_code code;
  gimple *stmt;

  if (expr == NULL_TREE)
    return NULL;

  if (is_gimple_min_invariant (expr))
    return NULL;

  code = TREE_CODE (expr);
  if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
    {
      n = TREE_OPERAND_LENGTH (expr);
      for (i = 0; i < n; i++)
	{
	  iv = find_deriving_biv_for_expr (data, TREE_OPERAND (expr, i));
	  if (iv)
	    return iv;
	}
    }

  /* Stop if it's not ssa name.  */
  if (code != SSA_NAME)
    return NULL;

  iv = get_iv (data, expr);
  if (!iv || integer_zerop (iv->step))
    return NULL;
  else if (iv->biv_p)
    return iv;

  stmt = SSA_NAME_DEF_STMT (expr);
  if (gphi *phi = dyn_cast <gphi *> (stmt))
    {
      ssa_op_iter iter;
      use_operand_p use_p;

      if (virtual_operand_p (gimple_phi_result (phi)))
	return NULL;

      FOR_EACH_PHI_ARG (use_p, phi, iter, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  iv = find_deriving_biv_for_expr (data, use);
	  if (iv)
	    return iv;
	}
      return NULL;
    }
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return NULL;

  e1 = gimple_assign_rhs1 (stmt);
  code = gimple_assign_rhs_code (stmt);
  if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS)
    return find_deriving_biv_for_expr (data, e1);

  switch (code)
    {
    case MULT_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case POINTER_PLUS_EXPR:
      /* Increments, decrements and multiplications by a constant
	 are simple.  */
      e2 = gimple_assign_rhs2 (stmt);
      iv = find_deriving_biv_for_expr (data, e2);
      if (iv)
	return iv;

      /* Fallthru.  */
    CASE_CONVERT:
      /* Casts are simple.  */
      return find_deriving_biv_for_expr (data, e1);

    default:
      break;
    }

  return NULL;
}

/* Record BIV, its predecessor and successor that they are used in
   address type uses.  */

static void
record_biv_for_address_use (struct ivopts_data *data, struct iv *biv)
{
  unsigned i;
  tree type, base_1, base_2;
  bitmap_iterator bi;

  if (!biv || !biv->biv_p || integer_zerop (biv->step)
      || biv->have_address_use || !biv->no_overflow)
    return;

  type = TREE_TYPE (biv->base);
  if (!INTEGRAL_TYPE_P (type))
    return;

  biv->have_address_use = true;
  data->bivs_not_used_in_addr--;
  base_1 = fold_build2 (PLUS_EXPR, type, biv->base, biv->step);
  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
    {
      struct iv *iv = ver_info (data, i)->iv;

      if (!iv || !iv->biv_p || integer_zerop (iv->step)
	  || iv->have_address_use || !iv->no_overflow)
	continue;

      if (type != TREE_TYPE (iv->base)
	  || !INTEGRAL_TYPE_P (TREE_TYPE (iv->base)))
	continue;

      if (!operand_equal_p (biv->step, iv->step, 0))
	continue;

      base_2 = fold_build2 (PLUS_EXPR, type, iv->base, iv->step);
      if (operand_equal_p (base_1, iv->base, 0)
	  || operand_equal_p (base_2, biv->base, 0))
	{
	  iv->have_address_use = true;
	  data->bivs_not_used_in_addr--;
	}
    }
}

/* Cumulates the steps of indices into DATA and replaces their values with the
   initial ones.  Returns false when the value of the index cannot be determined.
   Callback for for_each_index.  */

struct ifs_ivopts_data
{
  struct ivopts_data *ivopts_data;
  gimple *stmt;
  tree step;
};

static bool
idx_find_step (tree base, tree *idx, void *data)
{
  struct ifs_ivopts_data *dta = (struct ifs_ivopts_data *) data;
  struct iv *iv;
  bool use_overflow_semantics = false;
  tree step, iv_base, iv_step, lbound, off;
  struct loop *loop = dta->ivopts_data->current_loop;

  /* If base is a component ref, require that the offset of the reference
     be invariant.  */
  if (TREE_CODE (base) == COMPONENT_REF)
    {
      off = component_ref_field_offset (base);
      return expr_invariant_in_loop_p (loop, off);
    }

  /* If base is array, first check whether we will be able to move the
     reference out of the loop (in order to take its address in strength
     reduction).  In order for this to work we need both lower bound
     and step to be loop invariants.  */
  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      /* Moreover, for a range, the size needs to be invariant as well.  */
      if (TREE_CODE (base) == ARRAY_RANGE_REF
	  && !expr_invariant_in_loop_p (loop, TYPE_SIZE (TREE_TYPE (base))))
	return false;

      step = array_ref_element_size (base);
      lbound = array_ref_low_bound (base);

      if (!expr_invariant_in_loop_p (loop, step)
	  || !expr_invariant_in_loop_p (loop, lbound))
	return false;
    }

  if (TREE_CODE (*idx) != SSA_NAME)
    return true;

  iv = get_iv (dta->ivopts_data, *idx);
  if (!iv)
    return false;

  /* XXX  We produce for a base of *D42 with iv->base being &x[0]
	  *&x[0], which is not folded and does not trigger the
	  ARRAY_REF path below.  */
  *idx = iv->base;

  if (integer_zerop (iv->step))
    return true;

  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      step = array_ref_element_size (base);

      /* We only handle addresses whose step is an integer constant.  */
      if (TREE_CODE (step) != INTEGER_CST)
	return false;
    }
  else
    /* The step for pointer arithmetics already is 1 byte.  */
    step = size_one_node;

  iv_base = iv->base;
  iv_step = iv->step;
  if (iv->no_overflow && nowrap_type_p (TREE_TYPE (iv_step)))
    use_overflow_semantics = true;

  if (!convert_affine_scev (dta->ivopts_data->current_loop,
			    sizetype, &iv_base, &iv_step, dta->stmt,
			    use_overflow_semantics))
    {
      /* The index might wrap.  */
      return false;
    }

  step = fold_build2 (MULT_EXPR, sizetype, step, iv_step);
  dta->step = fold_build2 (PLUS_EXPR, sizetype, dta->step, step);

  if (dta->ivopts_data->bivs_not_used_in_addr)
    {
      if (!iv->biv_p)
	iv = find_deriving_biv_for_expr (dta->ivopts_data, iv->ssa_name);

      record_biv_for_address_use (dta->ivopts_data, iv);
    }
  return true;
}

/* Records use in index IDX.  Callback for for_each_index.  Ivopts data
   object is passed to it in DATA.  */

static bool
idx_record_use (tree base, tree *idx,
		void *vdata)
{
  struct ivopts_data *data = (struct ivopts_data *) vdata;
  find_interesting_uses_op (data, *idx);
  if (TREE_CODE (base) == ARRAY_REF || TREE_CODE (base) == ARRAY_RANGE_REF)
    {
      find_interesting_uses_op (data, array_ref_element_size (base));
      find_interesting_uses_op (data, array_ref_low_bound (base));
    }
  return true;
}

/* If we can prove that TOP = cst * BOT for some constant cst,
   store cst to MUL and return true.  Otherwise return false.
   The returned value is always sign-extended, regardless of the
   signedness of TOP and BOT.  */

static bool
constant_multiple_of (tree top, tree bot, widest_int *mul)
{
  tree mby;
  enum tree_code code;
  unsigned precision = TYPE_PRECISION (TREE_TYPE (top));
  widest_int res, p0, p1;

  STRIP_NOPS (top);
  STRIP_NOPS (bot);

  if (operand_equal_p (top, bot, 0))
    {
      *mul = 1;
      return true;
    }

  code = TREE_CODE (top);
  switch (code)
    {
    case MULT_EXPR:
      mby = TREE_OPERAND (top, 1);
      if (TREE_CODE (mby) != INTEGER_CST)
	return false;

      if (!constant_multiple_of (TREE_OPERAND (top, 0), bot, &res))
	return false;

      *mul = wi::sext (res * wi::to_widest (mby), precision);
      return true;

    case PLUS_EXPR:
    case MINUS_EXPR:
      if (!constant_multiple_of (TREE_OPERAND (top, 0), bot, &p0)
	  || !constant_multiple_of (TREE_OPERAND (top, 1), bot, &p1))
	return false;

      if (code == MINUS_EXPR)
	p1 = -p1;
      *mul = wi::sext (p0 + p1, precision);
      return true;

    case INTEGER_CST:
      if (TREE_CODE (bot) != INTEGER_CST)
	return false;

      p0 = widest_int::from (top, SIGNED);
      p1 = widest_int::from (bot, SIGNED);
      if (p1 == 0)
	return false;
      *mul = wi::sext (wi::divmod_trunc (p0, p1, SIGNED, &res), precision);
      return res == 0;

    default:
      return false;
    }
}

/* Return true if memory reference REF with step STEP may be unaligned.  */

static bool
may_be_unaligned_p (tree ref, tree step)
{
  /* TARGET_MEM_REFs are translated directly to valid MEMs on the target,
     thus they are not misaligned.  */
  if (TREE_CODE (ref) == TARGET_MEM_REF)
    return false;

  unsigned int align = TYPE_ALIGN (TREE_TYPE (ref));
  if (GET_MODE_ALIGNMENT (TYPE_MODE (TREE_TYPE (ref))) > align)
    align = GET_MODE_ALIGNMENT (TYPE_MODE (TREE_TYPE (ref)));

  unsigned HOST_WIDE_INT bitpos;
  unsigned int ref_align;
  get_object_alignment_1 (ref, &ref_align, &bitpos);
  if (ref_align < align
      || (bitpos % align) != 0
      || (bitpos % BITS_PER_UNIT) != 0)
    return true;

  unsigned int trailing_zeros = tree_ctz (step);
  if (trailing_zeros < HOST_BITS_PER_INT
      && (1U << trailing_zeros) * BITS_PER_UNIT < align)
    return true;

  return false;
}

/* Return true if EXPR may be non-addressable.   */

bool
may_be_nonaddressable_p (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case TARGET_MEM_REF:
      /* TARGET_MEM_REFs are translated directly to valid MEMs on the
	 target, thus they are always addressable.  */
      return false;

    case MEM_REF:
      /* Likewise for MEM_REFs, modulo the storage order.  */
      return REF_REVERSE_STORAGE_ORDER (expr);

    case BIT_FIELD_REF:
      if (REF_REVERSE_STORAGE_ORDER (expr))
	return true;
      return may_be_nonaddressable_p (TREE_OPERAND (expr, 0));

    case COMPONENT_REF:
      if (TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return true;
      return DECL_NONADDRESSABLE_P (TREE_OPERAND (expr, 1))
	     || may_be_nonaddressable_p (TREE_OPERAND (expr, 0));

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return true;
      return may_be_nonaddressable_p (TREE_OPERAND (expr, 0));

    case VIEW_CONVERT_EXPR:
      /* This kind of view-conversions may wrap non-addressable objects
	 and make them look addressable.  After some processing the
	 non-addressability may be uncovered again, causing ADDR_EXPRs
	 of inappropriate objects to be built.  */
      if (is_gimple_reg (TREE_OPERAND (expr, 0))
	  || !is_gimple_addressable (TREE_OPERAND (expr, 0)))
	return true;
      return may_be_nonaddressable_p (TREE_OPERAND (expr, 0));

    CASE_CONVERT:
      return true;

    default:
      break;
    }

  return false;
}

/* Finds addresses in *OP_P inside STMT.  */

static void
find_interesting_uses_address (struct ivopts_data *data, gimple *stmt,
			       tree *op_p)
{
  tree base = *op_p, step = size_zero_node;
  struct iv *civ;
  struct ifs_ivopts_data ifs_ivopts_data;

  /* Do not play with volatile memory references.  A bit too conservative,
     perhaps, but safe.  */
  if (gimple_has_volatile_ops (stmt))
    goto fail;

  /* Ignore bitfields for now.  Not really something terribly complicated
     to handle.  TODO.  */
  if (TREE_CODE (base) == BIT_FIELD_REF)
    goto fail;

  base = unshare_expr (base);

  if (TREE_CODE (base) == TARGET_MEM_REF)
    {
      tree type = build_pointer_type (TREE_TYPE (base));
      tree astep;

      if (TMR_BASE (base)
	  && TREE_CODE (TMR_BASE (base)) == SSA_NAME)
	{
	  civ = get_iv (data, TMR_BASE (base));
	  if (!civ)
	    goto fail;

	  TMR_BASE (base) = civ->base;
	  step = civ->step;
	}
      if (TMR_INDEX2 (base)
	  && TREE_CODE (TMR_INDEX2 (base)) == SSA_NAME)
	{
	  civ = get_iv (data, TMR_INDEX2 (base));
	  if (!civ)
	    goto fail;

	  TMR_INDEX2 (base) = civ->base;
	  step = civ->step;
	}
      if (TMR_INDEX (base)
	  && TREE_CODE (TMR_INDEX (base)) == SSA_NAME)
	{
	  civ = get_iv (data, TMR_INDEX (base));
	  if (!civ)
	    goto fail;

	  TMR_INDEX (base) = civ->base;
	  astep = civ->step;

	  if (astep)
	    {
	      if (TMR_STEP (base))
		astep = fold_build2 (MULT_EXPR, type, TMR_STEP (base), astep);

	      step = fold_build2 (PLUS_EXPR, type, step, astep);
	    }
	}

      if (integer_zerop (step))
	goto fail;
      base = tree_mem_ref_addr (type, base);
    }
  else
    {
      ifs_ivopts_data.ivopts_data = data;
      ifs_ivopts_data.stmt = stmt;
      ifs_ivopts_data.step = size_zero_node;
      if (!for_each_index (&base, idx_find_step, &ifs_ivopts_data)
	  || integer_zerop (ifs_ivopts_data.step))
	goto fail;
      step = ifs_ivopts_data.step;

      /* Check that the base expression is addressable.  This needs
	 to be done after substituting bases of IVs into it.  */
      if (may_be_nonaddressable_p (base))
	goto fail;

      /* Moreover, on strict alignment platforms, check that it is
	 sufficiently aligned.  */
      if (STRICT_ALIGNMENT && may_be_unaligned_p (base, step))
	goto fail;

      base = build_fold_addr_expr (base);

      /* Substituting bases of IVs into the base expression might
	 have caused folding opportunities.  */
      if (TREE_CODE (base) == ADDR_EXPR)
	{
	  tree *ref = &TREE_OPERAND (base, 0);
	  while (handled_component_p (*ref))
	    ref = &TREE_OPERAND (*ref, 0);
	  if (TREE_CODE (*ref) == MEM_REF)
	    {
	      tree tem = fold_binary (MEM_REF, TREE_TYPE (*ref),
				      TREE_OPERAND (*ref, 0),
				      TREE_OPERAND (*ref, 1));
	      if (tem)
		*ref = tem;
	    }
	}
    }

  civ = alloc_iv (data, base, step);
  record_group_use (data, op_p, civ, stmt, USE_ADDRESS);
  return;

fail:
  for_each_index (op_p, idx_record_use, data);
}

/* Finds and records invariants used in STMT.  */

static void
find_invariants_stmt (struct ivopts_data *data, gimple *stmt)
{
  ssa_op_iter iter;
  use_operand_p use_p;
  tree op;

  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
    {
      op = USE_FROM_PTR (use_p);
      record_invariant (data, op, false);
    }
}

/* Finds interesting uses of induction variables in the statement STMT.  */

static void
find_interesting_uses_stmt (struct ivopts_data *data, gimple *stmt)
{
  struct iv *iv;
  tree op, *lhs, *rhs;
  ssa_op_iter iter;
  use_operand_p use_p;
  enum tree_code code;

  find_invariants_stmt (data, stmt);

  if (gimple_code (stmt) == GIMPLE_COND)
    {
      find_interesting_uses_cond (data, stmt);
      return;
    }

  if (is_gimple_assign (stmt))
    {
      lhs = gimple_assign_lhs_ptr (stmt);
      rhs = gimple_assign_rhs1_ptr (stmt);

      if (TREE_CODE (*lhs) == SSA_NAME)
	{
	  /* If the statement defines an induction variable, the uses are not
	     interesting by themselves.  */

	  iv = get_iv (data, *lhs);

	  if (iv && !integer_zerop (iv->step))
	    return;
	}

      code = gimple_assign_rhs_code (stmt);
      if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS
	  && (REFERENCE_CLASS_P (*rhs)
	      || is_gimple_val (*rhs)))
	{
	  if (REFERENCE_CLASS_P (*rhs))
	    find_interesting_uses_address (data, stmt, rhs);
	  else
	    find_interesting_uses_op (data, *rhs);

	  if (REFERENCE_CLASS_P (*lhs))
	    find_interesting_uses_address (data, stmt, lhs);
	  return;
	}
      else if (TREE_CODE_CLASS (code) == tcc_comparison)
	{
	  find_interesting_uses_cond (data, stmt);
	  return;
	}

      /* TODO -- we should also handle address uses of type

	 memory = call (whatever);

	 and

	 call (memory).  */
    }

  if (gimple_code (stmt) == GIMPLE_PHI
      && gimple_bb (stmt) == data->current_loop->header)
    {
      iv = get_iv (data, PHI_RESULT (stmt));

      if (iv && !integer_zerop (iv->step))
	return;
    }

  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
    {
      op = USE_FROM_PTR (use_p);

      if (TREE_CODE (op) != SSA_NAME)
	continue;

      iv = get_iv (data, op);
      if (!iv)
	continue;

      find_interesting_uses_op (data, op);
    }
}

/* Finds interesting uses of induction variables outside of loops
   on loop exit edge EXIT.  */

static void
find_interesting_uses_outside (struct ivopts_data *data, edge exit)
{
  gphi *phi;
  gphi_iterator psi;
  tree def;

  for (psi = gsi_start_phis (exit->dest); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();
      def = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      if (!virtual_operand_p (def))
        find_interesting_uses_op (data, def);
    }
}

/* Compute maximum offset of [base + offset] addressing mode
   for memory reference represented by USE.  */

static HOST_WIDE_INT
compute_max_addr_offset (struct iv_use *use)
{
  int width;
  rtx reg, addr;
  HOST_WIDE_INT i, off;
  unsigned list_index, num;
  addr_space_t as;
  machine_mode mem_mode, addr_mode;
  static vec<HOST_WIDE_INT> max_offset_list;

  as = TYPE_ADDR_SPACE (TREE_TYPE (use->iv->base));
  mem_mode = TYPE_MODE (TREE_TYPE (*use->op_p));

  num = max_offset_list.length ();
  list_index = (unsigned) as * MAX_MACHINE_MODE + (unsigned) mem_mode;
  if (list_index >= num)
    {
      max_offset_list.safe_grow (list_index + MAX_MACHINE_MODE);
      for (; num < max_offset_list.length (); num++)
	max_offset_list[num] = -1;
    }

  off = max_offset_list[list_index];
  if (off != -1)
    return off;

  addr_mode = targetm.addr_space.address_mode (as);
  reg = gen_raw_REG (addr_mode, LAST_VIRTUAL_REGISTER + 1);
  addr = gen_rtx_fmt_ee (PLUS, addr_mode, reg, NULL_RTX);

  width = GET_MODE_BITSIZE (addr_mode) - 1;
  if (width > (HOST_BITS_PER_WIDE_INT - 1))
    width = HOST_BITS_PER_WIDE_INT - 1;

  for (i = width; i > 0; i--)
    {
      off = ((unsigned HOST_WIDE_INT) 1 << i) - 1;
      XEXP (addr, 1) = gen_int_mode (off, addr_mode);
      if (memory_address_addr_space_p (mem_mode, addr, as))
	break;

      /* For some strict-alignment targets, the offset must be naturally
	 aligned.  Try an aligned offset if mem_mode is not QImode.  */
      off = ((unsigned HOST_WIDE_INT) 1 << i);
      if (off > GET_MODE_SIZE (mem_mode) && mem_mode != QImode)
	{
	  off -= GET_MODE_SIZE (mem_mode);
	  XEXP (addr, 1) = gen_int_mode (off, addr_mode);
	  if (memory_address_addr_space_p (mem_mode, addr, as))
	    break;
	}
    }
  if (i == 0)
    off = 0;

  max_offset_list[list_index] = off;
  return off;
}

/* Comparison function to sort group in ascending order of addr_offset.  */

static int
group_compare_offset (const void *a, const void *b)
{
  const struct iv_use *const *u1 = (const struct iv_use *const *) a;
  const struct iv_use *const *u2 = (const struct iv_use *const *) b;

  if ((*u1)->addr_offset != (*u2)->addr_offset)
    return (*u1)->addr_offset < (*u2)->addr_offset ? -1 : 1;
  else
    return 0;
}

/* Check if small groups should be split.  Return true if no group
   contains more than two uses with distinct addr_offsets.  Return
   false otherwise.  We want to split such groups because:

     1) Small groups don't have much benefit and may interfer with
	general candidate selection.
     2) Size for problem with only small groups is usually small and
	general algorithm can handle it well.

   TODO -- Above claim may not hold when we want to merge memory
   accesses with conseuctive addresses.  */

static bool
split_small_address_groups_p (struct ivopts_data *data)
{
  unsigned int i, j, distinct = 1;
  struct iv_use *pre;
  struct iv_group *group;

  for (i = 0; i < data->vgroups.length (); i++)
    {
      group = data->vgroups[i];
      if (group->vuses.length () == 1)
	continue;

      gcc_assert (group->type == USE_ADDRESS);
      if (group->vuses.length () == 2)
	{
	  if (group->vuses[0]->addr_offset > group->vuses[1]->addr_offset)
	    std::swap (group->vuses[0], group->vuses[1]);
	}
      else
	group->vuses.qsort (group_compare_offset);

      if (distinct > 2)
	continue;

      distinct = 1;
      for (pre = group->vuses[0], j = 1; j < group->vuses.length (); j++)
	{
	  if (group->vuses[j]->addr_offset != pre->addr_offset)
	    {
	      pre = group->vuses[j];
	      distinct++;
	    }

	  if (distinct > 2)
	    break;
	}
    }

  return (distinct <= 2);
}

/* For each group of address type uses, this function further groups
   these uses according to the maximum offset supported by target's
   [base + offset] addressing mode.  */

static void
split_address_groups (struct ivopts_data *data)
{
  unsigned int i, j;
  HOST_WIDE_INT max_offset = -1;

  /* Reset max offset to split all small groups.  */
  if (split_small_address_groups_p (data))
    max_offset = 0;

  for (i = 0; i < data->vgroups.length (); i++)
    {
      struct iv_group *group = data->vgroups[i];
      struct iv_use *use = group->vuses[0];

      use->id = 0;
      use->group_id = group->id;
      if (group->vuses.length () == 1)
	continue;

      if (max_offset != 0)
	max_offset = compute_max_addr_offset (use);

      for (j = 1; j < group->vuses.length (); j++)
	{
	  struct iv_use *next = group->vuses[j];

	  /* Only uses with offset that can fit in offset part against
	     the first use can be grouped together.  */
	  if (next->addr_offset - use->addr_offset
	      > (unsigned HOST_WIDE_INT) max_offset)
	    break;

	  next->id = j;
	  next->group_id = group->id;
	}
      /* Split group.  */
      if (j < group->vuses.length ())
	{
	  struct iv_group *new_group = record_group (data, group->type);
	  new_group->vuses.safe_splice (group->vuses);
	  new_group->vuses.block_remove (0, j);
	  group->vuses.truncate (j);
	}
    }
}

/* Finds uses of the induction variables that are interesting.  */

static void
find_interesting_uses (struct ivopts_data *data)
{
  basic_block bb;
  gimple_stmt_iterator bsi;
  basic_block *body = get_loop_body (data->current_loop);
  unsigned i;
  edge e;

  for (i = 0; i < data->current_loop->num_nodes; i++)
    {
      edge_iterator ei;
      bb = body[i];

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && !flow_bb_inside_loop_p (data->current_loop, e->dest))
	  find_interesting_uses_outside (data, e);

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	find_interesting_uses_stmt (data, gsi_stmt (bsi));
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	if (!is_gimple_debug (gsi_stmt (bsi)))
	  find_interesting_uses_stmt (data, gsi_stmt (bsi));
    }

  split_address_groups (data);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      bitmap_iterator bi;

      fprintf (dump_file, "\n<Invariant Vars>:\n");
      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
	{
	  struct version_info *info = ver_info (data, i);
	  if (info->inv_id)
	    {
	      fprintf (dump_file, "Inv %d:\t", info->inv_id);
	      print_generic_expr (dump_file, info->name, TDF_SLIM);
	      fprintf (dump_file, "%s\n",
		       info->has_nonlin_use ? "" : "\t(eliminable)");
	    }
	}

      fprintf (dump_file, "\n<IV Groups>:\n");
      dump_groups (dump_file, data);
      fprintf (dump_file, "\n");
    }

  free (body);
}

/* Strips constant offsets from EXPR and stores them to OFFSET.  If INSIDE_ADDR
   is true, assume we are inside an address.  If TOP_COMPREF is true, assume
   we are at the top-level of the processed address.  */

static tree
strip_offset_1 (tree expr, bool inside_addr, bool top_compref,
		HOST_WIDE_INT *offset)
{
  tree op0 = NULL_TREE, op1 = NULL_TREE, tmp, step;
  enum tree_code code;
  tree type, orig_type = TREE_TYPE (expr);
  HOST_WIDE_INT off0, off1, st;
  tree orig_expr = expr;

  STRIP_NOPS (expr);

  type = TREE_TYPE (expr);
  code = TREE_CODE (expr);
  *offset = 0;

  switch (code)
    {
    case INTEGER_CST:
      if (!cst_and_fits_in_hwi (expr)
	  || integer_zerop (expr))
	return orig_expr;

      *offset = int_cst_value (expr);
      return build_int_cst (orig_type, 0);

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);

      op0 = strip_offset_1 (op0, false, false, &off0);
      op1 = strip_offset_1 (op1, false, false, &off1);

      *offset = (code == MINUS_EXPR ? off0 - off1 : off0 + off1);
      if (op0 == TREE_OPERAND (expr, 0)
	  && op1 == TREE_OPERAND (expr, 1))
	return orig_expr;

      if (integer_zerop (op1))
	expr = op0;
      else if (integer_zerop (op0))
	{
	  if (code == MINUS_EXPR)
	    expr = fold_build1 (NEGATE_EXPR, type, op1);
	  else
	    expr = op1;
	}
      else
	expr = fold_build2 (code, type, op0, op1);

      return fold_convert (orig_type, expr);

    case MULT_EXPR:
      op1 = TREE_OPERAND (expr, 1);
      if (!cst_and_fits_in_hwi (op1))
	return orig_expr;

      op0 = TREE_OPERAND (expr, 0);
      op0 = strip_offset_1 (op0, false, false, &off0);
      if (op0 == TREE_OPERAND (expr, 0))
	return orig_expr;

      *offset = off0 * int_cst_value (op1);
      if (integer_zerop (op0))
	expr = op0;
      else
	expr = fold_build2 (MULT_EXPR, type, op0, op1);

      return fold_convert (orig_type, expr);

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (!inside_addr)
	return orig_expr;

      step = array_ref_element_size (expr);
      if (!cst_and_fits_in_hwi (step))
	break;

      st = int_cst_value (step);
      op1 = TREE_OPERAND (expr, 1);
      op1 = strip_offset_1 (op1, false, false, &off1);
      *offset = off1 * st;

      if (top_compref
	  && integer_zerop (op1))
	{
	  /* Strip the component reference completely.  */
	  op0 = TREE_OPERAND (expr, 0);
	  op0 = strip_offset_1 (op0, inside_addr, top_compref, &off0);
	  *offset += off0;
	  return op0;
	}
      break;

    case COMPONENT_REF:
      {
	tree field;

	if (!inside_addr)
	  return orig_expr;

	tmp = component_ref_field_offset (expr);
	field = TREE_OPERAND (expr, 1);
	if (top_compref
	    && cst_and_fits_in_hwi (tmp)
	    && cst_and_fits_in_hwi (DECL_FIELD_BIT_OFFSET (field)))
	  {
	    HOST_WIDE_INT boffset, abs_off;

	    /* Strip the component reference completely.  */
	    op0 = TREE_OPERAND (expr, 0);
	    op0 = strip_offset_1 (op0, inside_addr, top_compref, &off0);
	    boffset = int_cst_value (DECL_FIELD_BIT_OFFSET (field));
	    abs_off = abs_hwi (boffset) / BITS_PER_UNIT;
	    if (boffset < 0)
	      abs_off = -abs_off;

	    *offset = off0 + int_cst_value (tmp) + abs_off;
	    return op0;
	  }
      }
      break;

    case ADDR_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op0 = strip_offset_1 (op0, true, true, &off0);
      *offset += off0;

      if (op0 == TREE_OPERAND (expr, 0))
	return orig_expr;

      expr = build_fold_addr_expr (op0);
      return fold_convert (orig_type, expr);

    case MEM_REF:
      /* ???  Offset operand?  */
      inside_addr = false;
      break;

    default:
      return orig_expr;
    }

  /* Default handling of expressions for that we want to recurse into
     the first operand.  */
  op0 = TREE_OPERAND (expr, 0);
  op0 = strip_offset_1 (op0, inside_addr, false, &off0);
  *offset += off0;

  if (op0 == TREE_OPERAND (expr, 0)
      && (!op1 || op1 == TREE_OPERAND (expr, 1)))
    return orig_expr;

  expr = copy_node (expr);
  TREE_OPERAND (expr, 0) = op0;
  if (op1)
    TREE_OPERAND (expr, 1) = op1;

  /* Inside address, we might strip the top level component references,
     thus changing type of the expression.  Handling of ADDR_EXPR
     will fix that.  */
  expr = fold_convert (orig_type, expr);

  return expr;
}

/* Strips constant offsets from EXPR and stores them to OFFSET.  */

static tree
strip_offset (tree expr, unsigned HOST_WIDE_INT *offset)
{
  HOST_WIDE_INT off;
  tree core = strip_offset_1 (expr, false, false, &off);
  *offset = off;
  return core;
}

/* Returns variant of TYPE that can be used as base for different uses.
   We return unsigned type with the same precision, which avoids problems
   with overflows.  */

static tree
generic_type_for (tree type)
{
  if (POINTER_TYPE_P (type))
    return unsigned_type_for (type);

  if (TYPE_UNSIGNED (type))
    return type;

  return unsigned_type_for (type);
}

/* Records invariants in *EXPR_P.  Callback for walk_tree.  DATA contains
   the bitmap to that we should store it.  */

static struct ivopts_data *fd_ivopts_data;
static tree
find_depends (tree *expr_p, int *ws ATTRIBUTE_UNUSED, void *data)
{
  bitmap *depends_on = (bitmap *) data;
  struct version_info *info;

  if (TREE_CODE (*expr_p) != SSA_NAME)
    return NULL_TREE;
  info = name_info (fd_ivopts_data, *expr_p);

  if (!info->inv_id || info->has_nonlin_use)
    return NULL_TREE;

  if (!*depends_on)
    *depends_on = BITMAP_ALLOC (NULL);
  bitmap_set_bit (*depends_on, info->inv_id);

  return NULL_TREE;
}

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  If both BASE and STEP are NULL, we add a pseudocandidate for the
   replacement of the final value of the iv by a direct computation.  */

static struct iv_cand *
add_candidate_1 (struct ivopts_data *data,
		 tree base, tree step, bool important, enum iv_position pos,
		 struct iv_use *use, gimple *incremented_at,
		 struct iv *orig_iv = NULL)
{
  unsigned i;
  struct iv_cand *cand = NULL;
  tree type, orig_type;

  gcc_assert (base && step);

  /* -fkeep-gc-roots-live means that we have to keep a real pointer
     live, but the ivopts code may replace a real pointer with one
     pointing before or after the memory block that is then adjusted
     into the memory block during the loop.  FIXME: It would likely be
     better to actually force the pointer live and still use ivopts;
     for example, it would be enough to write the pointer into memory
     and keep it there until after the loop.  */
  if (flag_keep_gc_roots_live && POINTER_TYPE_P (TREE_TYPE (base)))
    return NULL;

  /* For non-original variables, make sure their values are computed in a type
     that does not invoke undefined behavior on overflows (since in general,
     we cannot prove that these induction variables are non-wrapping).  */
  if (pos != IP_ORIGINAL)
    {
      orig_type = TREE_TYPE (base);
      type = generic_type_for (orig_type);
      if (type != orig_type)
	{
	  base = fold_convert (type, base);
	  step = fold_convert (type, step);
	}
    }

  for (i = 0; i < data->vcands.length (); i++)
    {
      cand = data->vcands[i];

      if (cand->pos != pos)
	continue;

      if (cand->incremented_at != incremented_at
	  || ((pos == IP_AFTER_USE || pos == IP_BEFORE_USE)
	      && cand->ainc_use != use))
	continue;

      if (operand_equal_p (base, cand->iv->base, 0)
	  && operand_equal_p (step, cand->iv->step, 0)
          && (TYPE_PRECISION (TREE_TYPE (base))
              == TYPE_PRECISION (TREE_TYPE (cand->iv->base))))
	break;
    }

  if (i == data->vcands.length ())
    {
      cand = XCNEW (struct iv_cand);
      cand->id = i;
      cand->iv = alloc_iv (data, base, step);
      cand->pos = pos;
      if (pos != IP_ORIGINAL)
	{
	  cand->var_before = create_tmp_var_raw (TREE_TYPE (base), "ivtmp");
	  cand->var_after = cand->var_before;
	}
      cand->important = important;
      cand->incremented_at = incremented_at;
      data->vcands.safe_push (cand);

      if (TREE_CODE (step) != INTEGER_CST)
	{
	  fd_ivopts_data = data;
	  walk_tree (&step, find_depends, &cand->depends_on, NULL);
	}

      if (pos == IP_AFTER_USE || pos == IP_BEFORE_USE)
	cand->ainc_use = use;
      else
	cand->ainc_use = NULL;

      cand->orig_iv = orig_iv;
      if (dump_file && (dump_flags & TDF_DETAILS))
	dump_cand (dump_file, cand);
    }

  cand->important |= important;

  /* Relate candidate to the group for which it is added.  */
  if (use)
    bitmap_set_bit (data->vgroups[use->group_id]->related_cands, i);

  return cand;
}

/* Returns true if incrementing the induction variable at the end of the LOOP
   is allowed.

   The purpose is to avoid splitting latch edge with a biv increment, thus
   creating a jump, possibly confusing other optimization passes and leaving
   less freedom to scheduler.  So we allow IP_END_POS only if IP_NORMAL_POS
   is not available (so we do not have a better alternative), or if the latch
   edge is already nonempty.  */

static bool
allow_ip_end_pos_p (struct loop *loop)
{
  if (!ip_normal_pos (loop))
    return true;

  if (!empty_block_p (ip_end_pos (loop)))
    return true;

  return false;
}

/* If possible, adds autoincrement candidates BASE + STEP * i based on use USE.
   Important field is set to IMPORTANT.  */

static void
add_autoinc_candidates (struct ivopts_data *data, tree base, tree step,
			bool important, struct iv_use *use)
{
  basic_block use_bb = gimple_bb (use->stmt);
  machine_mode mem_mode;
  unsigned HOST_WIDE_INT cstepi;

  /* If we insert the increment in any position other than the standard
     ones, we must ensure that it is incremented once per iteration.
     It must not be in an inner nested loop, or one side of an if
     statement.  */
  if (use_bb->loop_father != data->current_loop
      || !dominated_by_p (CDI_DOMINATORS, data->current_loop->latch, use_bb)
      || stmt_could_throw_p (use->stmt)
      || !cst_and_fits_in_hwi (step))
    return;

  cstepi = int_cst_value (step);

  mem_mode = TYPE_MODE (TREE_TYPE (*use->op_p));
  if (((USE_LOAD_PRE_INCREMENT (mem_mode)
	|| USE_STORE_PRE_INCREMENT (mem_mode))
       && GET_MODE_SIZE (mem_mode) == cstepi)
      || ((USE_LOAD_PRE_DECREMENT (mem_mode)
	   || USE_STORE_PRE_DECREMENT (mem_mode))
	  && GET_MODE_SIZE (mem_mode) == -cstepi))
    {
      enum tree_code code = MINUS_EXPR;
      tree new_base;
      tree new_step = step;

      if (POINTER_TYPE_P (TREE_TYPE (base)))
	{
	  new_step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);
	  code = POINTER_PLUS_EXPR;
	}
      else
	new_step = fold_convert (TREE_TYPE (base), new_step);
      new_base = fold_build2 (code, TREE_TYPE (base), base, new_step);
      add_candidate_1 (data, new_base, step, important, IP_BEFORE_USE, use,
		       use->stmt);
    }
  if (((USE_LOAD_POST_INCREMENT (mem_mode)
	|| USE_STORE_POST_INCREMENT (mem_mode))
       && GET_MODE_SIZE (mem_mode) == cstepi)
      || ((USE_LOAD_POST_DECREMENT (mem_mode)
	   || USE_STORE_POST_DECREMENT (mem_mode))
	  && GET_MODE_SIZE (mem_mode) == -cstepi))
    {
      add_candidate_1 (data, base, step, important, IP_AFTER_USE, use,
		       use->stmt);
    }
}

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  The candidate computation is scheduled before exit condition and at
   the end of loop.  */

static void
add_candidate (struct ivopts_data *data,
	       tree base, tree step, bool important, struct iv_use *use,
	       struct iv *orig_iv = NULL)
{
  if (ip_normal_pos (data->current_loop))
    add_candidate_1 (data, base, step, important,
		     IP_NORMAL, use, NULL, orig_iv);
  if (ip_end_pos (data->current_loop)
      && allow_ip_end_pos_p (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_END, use, NULL, orig_iv);
}

/* Adds standard iv candidates.  */

static void
add_standard_iv_candidates (struct ivopts_data *data)
{
  add_candidate (data, integer_zero_node, integer_one_node, true, NULL);

  /* The same for a double-integer type if it is still fast enough.  */
  if (TYPE_PRECISION
        (long_integer_type_node) > TYPE_PRECISION (integer_type_node)
      && TYPE_PRECISION (long_integer_type_node) <= BITS_PER_WORD)
    add_candidate (data, build_int_cst (long_integer_type_node, 0),
		   build_int_cst (long_integer_type_node, 1), true, NULL);

  /* The same for a double-integer type if it is still fast enough.  */
  if (TYPE_PRECISION
        (long_long_integer_type_node) > TYPE_PRECISION (long_integer_type_node)
      && TYPE_PRECISION (long_long_integer_type_node) <= BITS_PER_WORD)
    add_candidate (data, build_int_cst (long_long_integer_type_node, 0),
		   build_int_cst (long_long_integer_type_node, 1), true, NULL);
}


/* Adds candidates bases on the old induction variable IV.  */

static void
add_iv_candidate_for_biv (struct ivopts_data *data, struct iv *iv)
{
  gimple *phi;
  tree def;
  struct iv_cand *cand;

  /* Check if this biv is used in address type use.  */
  if (iv->no_overflow  && iv->have_address_use
      && INTEGRAL_TYPE_P (TREE_TYPE (iv->base))
      && TYPE_PRECISION (TREE_TYPE (iv->base)) < TYPE_PRECISION (sizetype))
    {
      tree base = fold_convert (sizetype, iv->base);
      tree step = fold_convert (sizetype, iv->step);

      /* Add iv cand of same precision as index part in TARGET_MEM_REF.  */
      add_candidate (data, base, step, true, NULL, iv);
      /* Add iv cand of the original type only if it has nonlinear use.  */
      if (iv->nonlin_use)
	add_candidate (data, iv->base, iv->step, true, NULL);
    }
  else
    add_candidate (data, iv->base, iv->step, true, NULL);

  /* The same, but with initial value zero.  */
  if (POINTER_TYPE_P (TREE_TYPE (iv->base)))
    add_candidate (data, size_int (0), iv->step, true, NULL);
  else
    add_candidate (data, build_int_cst (TREE_TYPE (iv->base), 0),
		   iv->step, true, NULL);

  phi = SSA_NAME_DEF_STMT (iv->ssa_name);
  if (gimple_code (phi) == GIMPLE_PHI)
    {
      /* Additionally record the possibility of leaving the original iv
	 untouched.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (data->current_loop));
      /* Don't add candidate if it's from another PHI node because
	 it's an affine iv appearing in the form of PEELED_CHREC.  */
      phi = SSA_NAME_DEF_STMT (def);
      if (gimple_code (phi) != GIMPLE_PHI)
	{
	  cand = add_candidate_1 (data,
				  iv->base, iv->step, true, IP_ORIGINAL, NULL,
				  SSA_NAME_DEF_STMT (def));
	  if (cand)
	    {
	      cand->var_before = iv->ssa_name;
	      cand->var_after = def;
	    }
	}
      else
	gcc_assert (gimple_bb (phi) == data->current_loop->header);
    }
}

/* Adds candidates based on the old induction variables.  */

static void
add_iv_candidate_for_bivs (struct ivopts_data *data)
{
  unsigned i;
  struct iv *iv;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
    {
      iv = ver_info (data, i)->iv;
      if (iv && iv->biv_p && !integer_zerop (iv->step))
	add_iv_candidate_for_biv (data, iv);
    }
}

/* Record common candidate {BASE, STEP} derived from USE in hashtable.  */

static void
record_common_cand (struct ivopts_data *data, tree base,
		    tree step, struct iv_use *use)
{
  struct iv_common_cand ent;
  struct iv_common_cand **slot;

  ent.base = base;
  ent.step = step;
  ent.hash = iterative_hash_expr (base, 0);
  ent.hash = iterative_hash_expr (step, ent.hash);

  slot = data->iv_common_cand_tab->find_slot (&ent, INSERT);
  if (*slot == NULL)
    {
      *slot = new iv_common_cand ();
      (*slot)->base = base;
      (*slot)->step = step;
      (*slot)->uses.create (8);
      (*slot)->hash = ent.hash;
      data->iv_common_cands.safe_push ((*slot));
    }

  gcc_assert (use != NULL);
  (*slot)->uses.safe_push (use);
  return;
}

/* Comparison function used to sort common candidates.  */

static int
common_cand_cmp (const void *p1, const void *p2)
{
  unsigned n1, n2;
  const struct iv_common_cand *const *const ccand1
    = (const struct iv_common_cand *const *)p1;
  const struct iv_common_cand *const *const ccand2
    = (const struct iv_common_cand *const *)p2;

  n1 = (*ccand1)->uses.length ();
  n2 = (*ccand2)->uses.length ();
  return n2 - n1;
}

/* Adds IV candidates based on common candidated recorded.  */

static void
add_iv_candidate_derived_from_uses (struct ivopts_data *data)
{
  unsigned i, j;
  struct iv_cand *cand_1, *cand_2;

  data->iv_common_cands.qsort (common_cand_cmp);
  for (i = 0; i < data->iv_common_cands.length (); i++)
    {
      struct iv_common_cand *ptr = data->iv_common_cands[i];

      /* Only add IV candidate if it's derived from multiple uses.  */
      if (ptr->uses.length () <= 1)
	break;

      cand_1 = NULL;
      cand_2 = NULL;
      if (ip_normal_pos (data->current_loop))
	cand_1 = add_candidate_1 (data, ptr->base, ptr->step,
				  false, IP_NORMAL, NULL, NULL);

      if (ip_end_pos (data->current_loop)
	  && allow_ip_end_pos_p (data->current_loop))
	cand_2 = add_candidate_1 (data, ptr->base, ptr->step,
				  false, IP_END, NULL, NULL);

      /* Bind deriving uses and the new candidates.  */
      for (j = 0; j < ptr->uses.length (); j++)
	{
	  struct iv_group *group = data->vgroups[ptr->uses[j]->group_id];
	  if (cand_1)
	    bitmap_set_bit (group->related_cands, cand_1->id);
	  if (cand_2)
	    bitmap_set_bit (group->related_cands, cand_2->id);
	}
    }

  /* Release data since it is useless from this point.  */
  data->iv_common_cand_tab->empty ();
  data->iv_common_cands.truncate (0);
}

/* Adds candidates based on the value of USE's iv.  */

static void
add_iv_candidate_for_use (struct ivopts_data *data, struct iv_use *use)
{
  unsigned HOST_WIDE_INT offset;
  tree base;
  tree basetype;
  struct iv *iv = use->iv;

  add_candidate (data, iv->base, iv->step, false, use);

  /* Record common candidate for use in case it can be shared by others.  */
  record_common_cand (data, iv->base, iv->step, use);

  /* Record common candidate with initial value zero.  */
  basetype = TREE_TYPE (iv->base);
  if (POINTER_TYPE_P (basetype))
    basetype = sizetype;
  record_common_cand (data, build_int_cst (basetype, 0), iv->step, use);

  /* Record common candidate with constant offset stripped in base.
     Like the use itself, we also add candidate directly for it.  */
  base = strip_offset (iv->base, &offset);
  if (offset || base != iv->base)
    {
      record_common_cand (data, base, iv->step, use);
      add_candidate (data, base, iv->step, false, use);
    }

  /* Record common candidate with base_object removed in base.  */
  if (iv->base_object != NULL)
    {
      unsigned i;
      aff_tree aff_base;
      tree step, base_object = iv->base_object;

      base = iv->base;
      step = iv->step;
      STRIP_NOPS (base);
      STRIP_NOPS (step);
      STRIP_NOPS (base_object);
      tree_to_aff_combination (base, TREE_TYPE (base), &aff_base);
      for (i = 0; i < aff_base.n; i++)
	{
	  if (aff_base.elts[i].coef != 1)
	    continue;

	  if (operand_equal_p (aff_base.elts[i].val, base_object, 0))
	    break;
	}
      if (i < aff_base.n)
	{
	  aff_combination_remove_elt (&aff_base, i);
	  base = aff_combination_to_tree (&aff_base);
	  basetype = TREE_TYPE (base);
	  if (POINTER_TYPE_P (basetype))
	    basetype = sizetype;

	  step = fold_convert (basetype, step);
	  record_common_cand (data, base, step, use);
	  /* Also record common candidate with offset stripped.  */
	  base = strip_offset (base, &offset);
	  if (offset)
	    record_common_cand (data, base, step, use);
	}
    }

  /* At last, add auto-incremental candidates.  Make such variables
     important since other iv uses with same base object may be based
     on it.  */
  if (use != NULL && use->type == USE_ADDRESS)
    add_autoinc_candidates (data, iv->base, iv->step, true, use);
}

/* Adds candidates based on the uses.  */

static void
add_iv_candidate_for_groups (struct ivopts_data *data)
{
  unsigned i;

  /* Only add candidate for the first use in group.  */
  for (i = 0; i < data->vgroups.length (); i++)
    {
      struct iv_group *group = data->vgroups[i];

      gcc_assert (group->vuses[0] != NULL);
      add_iv_candidate_for_use (data, group->vuses[0]);
    }
  add_iv_candidate_derived_from_uses (data);
}

/* Record important candidates and add them to related_cands bitmaps.  */

static void
record_important_candidates (struct ivopts_data *data)
{
  unsigned i;
  struct iv_group *group;

  for (i = 0; i < data->vcands.length (); i++)
    {
      struct iv_cand *cand = data->vcands[i];

      if (cand->important)
	bitmap_set_bit (data->important_candidates, i);
    }

  data->consider_all_candidates = (data->vcands.length ()
				   <= CONSIDER_ALL_CANDIDATES_BOUND);

  /* Add important candidates to groups' related_cands bitmaps.  */
  for (i = 0; i < data->vgroups.length (); i++)
    {
      group = data->vgroups[i];
      bitmap_ior_into (group->related_cands, data->important_candidates);
    }
}

/* Allocates the data structure mapping the (use, candidate) pairs to costs.
   If consider_all_candidates is true, we use a two-dimensional array, otherwise
   we allocate a simple list to every use.  */

static void
alloc_use_cost_map (struct ivopts_data *data)
{
  unsigned i, size, s;

  for (i = 0; i < data->vgroups.length (); i++)
    {
      struct iv_group *group = data->vgroups[i];

      if (data->consider_all_candidates)
	size = data->vcands.length ();
      else
	{
	  s = bitmap_count_bits (group->related_cands);

	  /* Round up to the power of two, so that moduling by it is fast.  */
	  size = s ? (1 << ceil_log2 (s)) : 1;
	}

      group->n_map_members = size;
      group->cost_map = XCNEWVEC (struct cost_pair, size);
    }
}

/* Returns description of computation cost of expression whose runtime
   cost is RUNTIME and complexity corresponds to COMPLEXITY.  */

static comp_cost
new_cost (unsigned runtime, unsigned complexity)
{
  comp_cost cost;

  cost.cost = runtime;
  cost.complexity = complexity;

  return cost;
}

/* Returns true if COST is infinite.  */

static bool
infinite_cost_p (comp_cost cost)
{
  return cost.cost == INFTY;
}

/* Adds costs COST1 and COST2.  */

static comp_cost
add_costs (comp_cost cost1, comp_cost cost2)
{
  if (infinite_cost_p (cost1) || infinite_cost_p (cost2))
    return infinite_cost;

  cost1.cost += cost2.cost;
  cost1.complexity += cost2.complexity;

  return cost1;
}
/* Subtracts costs COST1 and COST2.  */

static comp_cost
sub_costs (comp_cost cost1, comp_cost cost2)
{
  cost1.cost -= cost2.cost;
  cost1.complexity -= cost2.complexity;

  return cost1;
}

/* Returns a negative number if COST1 < COST2, a positive number if
   COST1 > COST2, and 0 if COST1 = COST2.  */

static int
compare_costs (comp_cost cost1, comp_cost cost2)
{
  if (cost1.cost == cost2.cost)
    return cost1.complexity - cost2.complexity;

  return cost1.cost - cost2.cost;
}

/* Sets cost of (GROUP, CAND) pair to COST and record that it depends
   on invariants DEPENDS_ON and that the value used in expressing it
   is VALUE, and in case of iv elimination the comparison operator is COMP.  */

static void
set_group_iv_cost (struct ivopts_data *data,
		   struct iv_group *group, struct iv_cand *cand,
		   comp_cost cost, bitmap depends_on, tree value,
		   enum tree_code comp, int inv_expr_id)
{
  unsigned i, s;

  if (infinite_cost_p (cost))
    {
      BITMAP_FREE (depends_on);
      return;
    }

  if (data->consider_all_candidates)
    {
      group->cost_map[cand->id].cand = cand;
      group->cost_map[cand->id].cost = cost;
      group->cost_map[cand->id].depends_on = depends_on;
      group->cost_map[cand->id].value = value;
      group->cost_map[cand->id].comp = comp;
      group->cost_map[cand->id].inv_expr_id = inv_expr_id;
      return;
    }

  /* n_map_members is a power of two, so this computes modulo.  */
  s = cand->id & (group->n_map_members - 1);
  for (i = s; i < group->n_map_members; i++)
    if (!group->cost_map[i].cand)
      goto found;
  for (i = 0; i < s; i++)
    if (!group->cost_map[i].cand)
      goto found;

  gcc_unreachable ();

found:
  group->cost_map[i].cand = cand;
  group->cost_map[i].cost = cost;
  group->cost_map[i].depends_on = depends_on;
  group->cost_map[i].value = value;
  group->cost_map[i].comp = comp;
  group->cost_map[i].inv_expr_id = inv_expr_id;
}

/* Gets cost of (GROUP, CAND) pair.  */

static struct cost_pair *
get_group_iv_cost (struct ivopts_data *data, struct iv_group *group,
		   struct iv_cand *cand)
{
  unsigned i, s;
  struct cost_pair *ret;

  if (!cand)
    return NULL;

  if (data->consider_all_candidates)
    {
      ret = group->cost_map + cand->id;
      if (!ret->cand)
	return NULL;

      return ret;
    }

  /* n_map_members is a power of two, so this computes modulo.  */
  s = cand->id & (group->n_map_members - 1);
  for (i = s; i < group->n_map_members; i++)
    if (group->cost_map[i].cand == cand)
      return group->cost_map + i;
    else if (group->cost_map[i].cand == NULL)
      return NULL;
  for (i = 0; i < s; i++)
    if (group->cost_map[i].cand == cand)
      return group->cost_map + i;
    else if (group->cost_map[i].cand == NULL)
      return NULL;

  return NULL;
}

/* Produce DECL_RTL for object obj so it looks like it is stored in memory.  */
static rtx
produce_memory_decl_rtl (tree obj, int *regno)
{
  addr_space_t as = TYPE_ADDR_SPACE (TREE_TYPE (obj));
  machine_mode address_mode = targetm.addr_space.address_mode (as);
  rtx x;

  gcc_assert (obj);
  if (TREE_STATIC (obj) || DECL_EXTERNAL (obj))
    {
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (obj));
      x = gen_rtx_SYMBOL_REF (address_mode, name);
      SET_SYMBOL_REF_DECL (x, obj);
      x = gen_rtx_MEM (DECL_MODE (obj), x);
      set_mem_addr_space (x, as);
      targetm.encode_section_info (obj, x, true);
    }
  else
    {
      x = gen_raw_REG (address_mode, (*regno)++);
      x = gen_rtx_MEM (DECL_MODE (obj), x);
      set_mem_addr_space (x, as);
    }

  return x;
}

/* Prepares decl_rtl for variables referred in *EXPR_P.  Callback for
   walk_tree.  DATA contains the actual fake register number.  */

static tree
prepare_decl_rtl (tree *expr_p, int *ws, void *data)
{
  tree obj = NULL_TREE;
  rtx x = NULL_RTX;
  int *regno = (int *) data;

  switch (TREE_CODE (*expr_p))
    {
    case ADDR_EXPR:
      for (expr_p = &TREE_OPERAND (*expr_p, 0);
	   handled_component_p (*expr_p);
	   expr_p = &TREE_OPERAND (*expr_p, 0))
	continue;
      obj = *expr_p;
      if (DECL_P (obj) && HAS_RTL_P (obj) && !DECL_RTL_SET_P (obj))
        x = produce_memory_decl_rtl (obj, regno);
      break;

    case SSA_NAME:
      *ws = 0;
      obj = SSA_NAME_VAR (*expr_p);
      /* Defer handling of anonymous SSA_NAMEs to the expander.  */
      if (!obj)
	return NULL_TREE;
      if (!DECL_RTL_SET_P (obj))
	x = gen_raw_REG (DECL_MODE (obj), (*regno)++);
      break;

    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      *ws = 0;
      obj = *expr_p;

      if (DECL_RTL_SET_P (obj))
	break;

      if (DECL_MODE (obj) == BLKmode)
	x = produce_memory_decl_rtl (obj, regno);
      else
	x = gen_raw_REG (DECL_MODE (obj), (*regno)++);

      break;

    default:
      break;
    }

  if (x)
    {
      decl_rtl_to_reset.safe_push (obj);
      SET_DECL_RTL (obj, x);
    }

  return NULL_TREE;
}

/* Determines cost of the computation of EXPR.  */

static unsigned
computation_cost (tree expr, bool speed)
{
  rtx_insn *seq;
  rtx rslt;
  tree type = TREE_TYPE (expr);
  unsigned cost;
  /* Avoid using hard regs in ways which may be unsupported.  */
  int regno = LAST_VIRTUAL_REGISTER + 1;
  struct cgraph_node *node = cgraph_node::get (current_function_decl);
  enum node_frequency real_frequency = node->frequency;

  node->frequency = NODE_FREQUENCY_NORMAL;
  crtl->maybe_hot_insn_p = speed;
  walk_tree (&expr, prepare_decl_rtl, &regno, NULL);
  start_sequence ();
  rslt = expand_expr (expr, NULL_RTX, TYPE_MODE (type), EXPAND_NORMAL);
  seq = get_insns ();
  end_sequence ();
  default_rtl_profile ();
  node->frequency = real_frequency;

  cost = seq_cost (seq, speed);
  if (MEM_P (rslt))
    cost += address_cost (XEXP (rslt, 0), TYPE_MODE (type),
			  TYPE_ADDR_SPACE (type), speed);
  else if (!REG_P (rslt))
    cost += set_src_cost (rslt, TYPE_MODE (type), speed);

  return cost;
}

/* Returns variable containing the value of candidate CAND at statement AT.  */

static tree
var_at_stmt (struct loop *loop, struct iv_cand *cand, gimple *stmt)
{
  if (stmt_after_increment (loop, cand, stmt))
    return cand->var_after;
  else
    return cand->var_before;
}

/* If A is (TYPE) BA and B is (TYPE) BB, and the types of BA and BB have the
   same precision that is at least as wide as the precision of TYPE, stores
   BA to A and BB to B, and returns the type of BA.  Otherwise, returns the
   type of A and B.  */

static tree
determine_common_wider_type (tree *a, tree *b)
{
  tree wider_type = NULL;
  tree suba, subb;
  tree atype = TREE_TYPE (*a);

  if (CONVERT_EXPR_P (*a))
    {
      suba = TREE_OPERAND (*a, 0);
      wider_type = TREE_TYPE (suba);
      if (TYPE_PRECISION (wider_type) < TYPE_PRECISION (atype))
	return atype;
    }
  else
    return atype;

  if (CONVERT_EXPR_P (*b))
    {
      subb = TREE_OPERAND (*b, 0);
      if (TYPE_PRECISION (wider_type) != TYPE_PRECISION (TREE_TYPE (subb)))
	return atype;
    }
  else
    return atype;

  *a = suba;
  *b = subb;
  return wider_type;
}

/* Determines the expression by that USE is expressed from induction variable
   CAND at statement AT in LOOP.  The expression is stored in a decomposed
   form into AFF.  Returns false if USE cannot be expressed using CAND.  */

static bool
get_computation_aff (struct loop *loop,
		     struct iv_use *use, struct iv_cand *cand, gimple *at,
		     struct aff_tree *aff)
{
  tree ubase = use->iv->base;
  tree ustep = use->iv->step;
  tree cbase = cand->iv->base;
  tree cstep = cand->iv->step, cstep_common;
  tree utype = TREE_TYPE (ubase), ctype = TREE_TYPE (cbase);
  tree common_type, var;
  tree uutype;
  aff_tree cbase_aff, var_aff;
  widest_int rat;

  if (TYPE_PRECISION (utype) > TYPE_PRECISION (ctype))
    {
      /* We do not have a precision to express the values of use.  */
      return false;
    }

  var = var_at_stmt (loop, cand, at);
  uutype = unsigned_type_for (utype);

  /* If the conversion is not noop, perform it.  */
  if (TYPE_PRECISION (utype) < TYPE_PRECISION (ctype))
    {
      if (cand->orig_iv != NULL && CONVERT_EXPR_P (cbase)
	  && (CONVERT_EXPR_P (cstep) || TREE_CODE (cstep) == INTEGER_CST))
	{
	  tree inner_base, inner_step, inner_type;
	  inner_base = TREE_OPERAND (cbase, 0);
	  if (CONVERT_EXPR_P (cstep))
	    inner_step = TREE_OPERAND (cstep, 0);
	  else
	    inner_step = cstep;

	  inner_type = TREE_TYPE (inner_base);
	  /* If candidate is added from a biv whose type is smaller than
	     ctype, we know both candidate and the biv won't overflow.
	     In this case, it's safe to skip the convertion in candidate.
	     As an example, (unsigned short)((unsigned long)A) equals to
	     (unsigned short)A, if A has a type no larger than short.  */
	  if (TYPE_PRECISION (inner_type) <= TYPE_PRECISION (uutype))
	    {
	      cbase = inner_base;
	      cstep = inner_step;
	    }
	}
      cstep = fold_convert (uutype, cstep);
      cbase = fold_convert (uutype, cbase);
      var = fold_convert (uutype, var);
    }

  /* Ratio is 1 when computing the value of biv cand by itself.
     We can't rely on constant_multiple_of in this case because the
     use is created after the original biv is selected.  The call
     could fail because of inconsistent fold behavior.  See PR68021
     for more information.  */
  if (cand->pos == IP_ORIGINAL && cand->incremented_at == use->stmt)
    {
      gcc_assert (is_gimple_assign (use->stmt));
      gcc_assert (use->iv->ssa_name == cand->var_after);
      gcc_assert (gimple_assign_lhs (use->stmt) == cand->var_after);
      rat = 1;
    }
  else if (!constant_multiple_of (ustep, cstep, &rat))
    return false;

  /* In case both UBASE and CBASE are shortened to UUTYPE from some common
     type, we achieve better folding by computing their difference in this
     wider type, and cast the result to UUTYPE.  We do not need to worry about
     overflows, as all the arithmetics will in the end be performed in UUTYPE
     anyway.  */
  common_type = determine_common_wider_type (&ubase, &cbase);

  /* use = ubase - ratio * cbase + ratio * var.  */
  tree_to_aff_combination (ubase, common_type, aff);
  tree_to_aff_combination (cbase, common_type, &cbase_aff);
  tree_to_aff_combination (var, uutype, &var_aff);

  /* We need to shift the value if we are after the increment.  */
  if (stmt_after_increment (loop, cand, at))
    {
      aff_tree cstep_aff;

      if (common_type != uutype)
	cstep_common = fold_convert (common_type, cstep);
      else
	cstep_common = cstep;

      tree_to_aff_combination (cstep_common, common_type, &cstep_aff);
      aff_combination_add (&cbase_aff, &cstep_aff);
    }

  aff_combination_scale (&cbase_aff, -rat);
  aff_combination_add (aff, &cbase_aff);
  if (common_type != uutype)
    aff_combination_convert (aff, uutype);

  aff_combination_scale (&var_aff, rat);
  aff_combination_add (aff, &var_aff);

  return true;
}

/* Return the type of USE.  */

static tree
get_use_type (struct iv_use *use)
{
  tree base_type = TREE_TYPE (use->iv->base);
  tree type;

  if (use->type == USE_ADDRESS)
    {
      /* The base_type may be a void pointer.  Create a pointer type based on
	 the mem_ref instead.  */
      type = build_pointer_type (TREE_TYPE (*use->op_p));
      gcc_assert (TYPE_ADDR_SPACE (TREE_TYPE (type))
		  == TYPE_ADDR_SPACE (TREE_TYPE (base_type)));
    }
  else
    type = base_type;

  return type;
}

/* Determines the expression by that USE is expressed from induction variable
   CAND at statement AT in LOOP.  The computation is unshared.  */

static tree
get_computation_at (struct loop *loop,
		    struct iv_use *use, struct iv_cand *cand, gimple *at)
{
  aff_tree aff;
  tree type = get_use_type (use);

  if (!get_computation_aff (loop, use, cand, at, &aff))
    return NULL_TREE;
  unshare_aff_combination (&aff);
  return fold_convert (type, aff_combination_to_tree (&aff));
}

/* Determines the expression by that USE is expressed from induction variable
   CAND in LOOP.  The computation is unshared.  */

static tree
get_computation (struct loop *loop, struct iv_use *use, struct iv_cand *cand)
{
  return get_computation_at (loop, use, cand, use->stmt);
}

/* Adjust the cost COST for being in loop setup rather than loop body.
   If we're optimizing for space, the loop setup overhead is constant;
   if we're optimizing for speed, amortize it over the per-iteration cost.  */
static unsigned
adjust_setup_cost (struct ivopts_data *data, unsigned cost)
{
  if (cost == INFTY)
    return cost;
  else if (optimize_loop_for_speed_p (data->current_loop))
    return cost / avg_loop_niter (data->current_loop);
  else
    return cost;
}

/* Returns true if multiplying by RATIO is allowed in an address.  Test the
   validity for a memory reference accessing memory of mode MODE in
   address space AS.  */


bool
multiplier_allowed_in_address_p (HOST_WIDE_INT ratio, machine_mode mode,
				 addr_space_t as)
{
#define MAX_RATIO 128
  unsigned int data_index = (int) as * MAX_MACHINE_MODE + (int) mode;
  static vec<sbitmap> valid_mult_list;
  sbitmap valid_mult;

  if (data_index >= valid_mult_list.length ())
    valid_mult_list.safe_grow_cleared (data_index + 1);

  valid_mult = valid_mult_list[data_index];
  if (!valid_mult)
    {
      machine_mode address_mode = targetm.addr_space.address_mode (as);
      rtx reg1 = gen_raw_REG (address_mode, LAST_VIRTUAL_REGISTER + 1);
      rtx reg2 = gen_raw_REG (address_mode, LAST_VIRTUAL_REGISTER + 2);
      rtx addr, scaled;
      HOST_WIDE_INT i;

      valid_mult = sbitmap_alloc (2 * MAX_RATIO + 1);
      bitmap_clear (valid_mult);
      scaled = gen_rtx_fmt_ee (MULT, address_mode, reg1, NULL_RTX);
      addr = gen_rtx_fmt_ee (PLUS, address_mode, scaled, reg2);
      for (i = -MAX_RATIO; i <= MAX_RATIO; i++)
	{
	  XEXP (scaled, 1) = gen_int_mode (i, address_mode);
	  if (memory_address_addr_space_p (mode, addr, as)
	      || memory_address_addr_space_p (mode, scaled, as))
	    bitmap_set_bit (valid_mult, i + MAX_RATIO);
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  allowed multipliers:");
	  for (i = -MAX_RATIO; i <= MAX_RATIO; i++)
	    if (bitmap_bit_p (valid_mult, i + MAX_RATIO))
	      fprintf (dump_file, " %d", (int) i);
	  fprintf (dump_file, "\n");
	  fprintf (dump_file, "\n");
	}

      valid_mult_list[data_index] = valid_mult;
    }

  if (ratio > MAX_RATIO || ratio < -MAX_RATIO)
    return false;

  return bitmap_bit_p (valid_mult, ratio + MAX_RATIO);
}

/* Returns cost of address in shape symbol + var + OFFSET + RATIO * index.
   If SYMBOL_PRESENT is false, symbol is omitted.  If VAR_PRESENT is false,
   variable is omitted.  Compute the cost for a memory reference that accesses
   a memory location of mode MEM_MODE in address space AS.

   MAY_AUTOINC is set to true if the autoincrement (increasing index by
   size of MEM_MODE / RATIO) is available.  To make this determination, we
   look at the size of the increment to be made, which is given in CSTEP.
   CSTEP may be zero if the step is unknown.
   STMT_AFTER_INC is true iff the statement we're looking at is after the
   increment of the original biv.

   TODO -- there must be some better way.  This all is quite crude.  */

enum ainc_type
{
  AINC_PRE_INC,		/* Pre increment.  */
  AINC_PRE_DEC,		/* Pre decrement.  */
  AINC_POST_INC,	/* Post increment.  */
  AINC_POST_DEC,	/* Post decrement.  */
  AINC_NONE		/* Also the number of auto increment types.  */
};

struct address_cost_data
{
  HOST_WIDE_INT min_offset, max_offset;
  unsigned costs[2][2][2][2];
  unsigned ainc_costs[AINC_NONE];
};


static comp_cost
get_address_cost (bool symbol_present, bool var_present,
		  unsigned HOST_WIDE_INT offset, HOST_WIDE_INT ratio,
		  HOST_WIDE_INT cstep, machine_mode mem_mode,
		  addr_space_t as, bool speed,
		  bool stmt_after_inc, bool *may_autoinc)
{
  machine_mode address_mode = targetm.addr_space.address_mode (as);
  static vec<address_cost_data *> address_cost_data_list;
  unsigned int data_index = (int) as * MAX_MACHINE_MODE + (int) mem_mode;
  address_cost_data *data;
  static bool has_preinc[MAX_MACHINE_MODE], has_postinc[MAX_MACHINE_MODE];
  static bool has_predec[MAX_MACHINE_MODE], has_postdec[MAX_MACHINE_MODE];
  unsigned cost, acost, complexity;
  enum ainc_type autoinc_type;
  bool offset_p, ratio_p, autoinc;
  HOST_WIDE_INT s_offset, autoinc_offset, msize;
  unsigned HOST_WIDE_INT mask;
  unsigned bits;

  if (data_index >= address_cost_data_list.length ())
    address_cost_data_list.safe_grow_cleared (data_index + 1);

  data = address_cost_data_list[data_index];
  if (!data)
    {
      HOST_WIDE_INT i;
      HOST_WIDE_INT rat, off = 0;
      int old_cse_not_expected, width;
      unsigned sym_p, var_p, off_p, rat_p, add_c;
      rtx_insn *seq;
      rtx addr, base;
      rtx reg0, reg1;

      data = (address_cost_data *) xcalloc (1, sizeof (*data));

      reg1 = gen_raw_REG (address_mode, LAST_VIRTUAL_REGISTER + 1);

      width = GET_MODE_BITSIZE (address_mode) - 1;
      if (width > (HOST_BITS_PER_WIDE_INT - 1))
	width = HOST_BITS_PER_WIDE_INT - 1;
      addr = gen_rtx_fmt_ee (PLUS, address_mode, reg1, NULL_RTX);

      for (i = width; i >= 0; i--)
	{
	  off = -((unsigned HOST_WIDE_INT) 1 << i);
	  XEXP (addr, 1) = gen_int_mode (off, address_mode);
	  if (memory_address_addr_space_p (mem_mode, addr, as))
	    break;
	}
      data->min_offset = (i == -1? 0 : off);

      for (i = width; i >= 0; i--)
	{
	  off = ((unsigned HOST_WIDE_INT) 1 << i) - 1;
	  XEXP (addr, 1) = gen_int_mode (off, address_mode);
	  if (memory_address_addr_space_p (mem_mode, addr, as))
	    break;
	  /* For some strict-alignment targets, the offset must be naturally
	     aligned.  Try an aligned offset if mem_mode is not QImode.  */
	  off = mem_mode != QImode
		? ((unsigned HOST_WIDE_INT) 1 << i)
		    - GET_MODE_SIZE (mem_mode)
		: 0;
	  if (off > 0)
	    {
	      XEXP (addr, 1) = gen_int_mode (off, address_mode);
	      if (memory_address_addr_space_p (mem_mode, addr, as))
		break;
	    }
	}
      if (i == -1)
        off = 0;
      data->max_offset = off;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "get_address_cost:\n");
	  fprintf (dump_file, "  min offset %s " HOST_WIDE_INT_PRINT_DEC "\n",
		   GET_MODE_NAME (mem_mode),
		   data->min_offset);
	  fprintf (dump_file, "  max offset %s " HOST_WIDE_INT_PRINT_DEC "\n",
		   GET_MODE_NAME (mem_mode),
		   data->max_offset);
	}

      rat = 1;
      for (i = 2; i <= MAX_RATIO; i++)
	if (multiplier_allowed_in_address_p (i, mem_mode, as))
	  {
	    rat = i;
	    break;
	  }

      /* Compute the cost of various addressing modes.  */
      acost = 0;
      reg0 = gen_raw_REG (address_mode, LAST_VIRTUAL_REGISTER + 1);
      reg1 = gen_raw_REG (address_mode, LAST_VIRTUAL_REGISTER + 2);

      if (USE_LOAD_PRE_DECREMENT (mem_mode)
	  || USE_STORE_PRE_DECREMENT (mem_mode))
	{
	  addr = gen_rtx_PRE_DEC (address_mode, reg0);
	  has_predec[mem_mode]
	    = memory_address_addr_space_p (mem_mode, addr, as);

	  if (has_predec[mem_mode])
	    data->ainc_costs[AINC_PRE_DEC]
	      = address_cost (addr, mem_mode, as, speed);
	}
      if (USE_LOAD_POST_DECREMENT (mem_mode)
	  || USE_STORE_POST_DECREMENT (mem_mode))
	{
	  addr = gen_rtx_POST_DEC (address_mode, reg0);
	  has_postdec[mem_mode]
	    = memory_address_addr_space_p (mem_mode, addr, as);

	  if (has_postdec[mem_mode])
	    data->ainc_costs[AINC_POST_DEC]
	      = address_cost (addr, mem_mode, as, speed);
	}
      if (USE_LOAD_PRE_INCREMENT (mem_mode)
	  || USE_STORE_PRE_DECREMENT (mem_mode))
	{
	  addr = gen_rtx_PRE_INC (address_mode, reg0);
	  has_preinc[mem_mode]
	    = memory_address_addr_space_p (mem_mode, addr, as);

	  if (has_preinc[mem_mode])
	    data->ainc_costs[AINC_PRE_INC]
	      = address_cost (addr, mem_mode, as, speed);
	}
      if (USE_LOAD_POST_INCREMENT (mem_mode)
	  || USE_STORE_POST_INCREMENT (mem_mode))
	{
	  addr = gen_rtx_POST_INC (address_mode, reg0);
	  has_postinc[mem_mode]
	    = memory_address_addr_space_p (mem_mode, addr, as);

	  if (has_postinc[mem_mode])
	    data->ainc_costs[AINC_POST_INC]
	      = address_cost (addr, mem_mode, as, speed);
	}
      for (i = 0; i < 16; i++)
	{
	  sym_p = i & 1;
	  var_p = (i >> 1) & 1;
	  off_p = (i >> 2) & 1;
	  rat_p = (i >> 3) & 1;

	  addr = reg0;
	  if (rat_p)
	    addr = gen_rtx_fmt_ee (MULT, address_mode, addr,
				   gen_int_mode (rat, address_mode));

	  if (var_p)
	    addr = gen_rtx_fmt_ee (PLUS, address_mode, addr, reg1);

	  if (sym_p)
	    {
	      base = gen_rtx_SYMBOL_REF (address_mode, ggc_strdup (""));
	      /* ??? We can run into trouble with some backends by presenting
		 it with symbols which haven't been properly passed through
		 targetm.encode_section_info.  By setting the local bit, we
		 enhance the probability of things working.  */
	      SYMBOL_REF_FLAGS (base) = SYMBOL_FLAG_LOCAL;

	      if (off_p)
		base = gen_rtx_fmt_e (CONST, address_mode,
				      gen_rtx_fmt_ee
					(PLUS, address_mode, base,
					 gen_int_mode (off, address_mode)));
	    }
	  else if (off_p)
	    base = gen_int_mode (off, address_mode);
	  else
	    base = NULL_RTX;

	  if (base)
	    addr = gen_rtx_fmt_ee (PLUS, address_mode, addr, base);

	  start_sequence ();
	  /* To avoid splitting addressing modes, pretend that no cse will
	     follow.  */
	  old_cse_not_expected = cse_not_expected;
	  cse_not_expected = true;
	  addr = memory_address_addr_space (mem_mode, addr, as);
	  cse_not_expected = old_cse_not_expected;
	  seq = get_insns ();
	  end_sequence ();

	  acost = seq_cost (seq, speed);
	  acost += address_cost (addr, mem_mode, as, speed);

	  if (!acost)
	    acost = 1;
	  data->costs[sym_p][var_p][off_p][rat_p] = acost;
	}

      /* On some targets, it is quite expensive to load symbol to a register,
	 which makes addresses that contain symbols look much more expensive.
	 However, the symbol will have to be loaded in any case before the
	 loop (and quite likely we have it in register already), so it does not
	 make much sense to penalize them too heavily.  So make some final
         tweaks for the SYMBOL_PRESENT modes:

         If VAR_PRESENT is false, and the mode obtained by changing symbol to
	 var is cheaper, use this mode with small penalty.
	 If VAR_PRESENT is true, try whether the mode with
	 SYMBOL_PRESENT = false is cheaper even with cost of addition, and
	 if this is the case, use it.  */
      add_c = add_cost (speed, address_mode);
      for (i = 0; i < 8; i++)
	{
	  var_p = i & 1;
	  off_p = (i >> 1) & 1;
	  rat_p = (i >> 2) & 1;

	  acost = data->costs[0][1][off_p][rat_p] + 1;
	  if (var_p)
	    acost += add_c;

	  if (acost < data->costs[1][var_p][off_p][rat_p])
	    data->costs[1][var_p][off_p][rat_p] = acost;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "<Address Costs>:\n");

	  for (i = 0; i < 16; i++)
	    {
	      sym_p = i & 1;
	      var_p = (i >> 1) & 1;
	      off_p = (i >> 2) & 1;
	      rat_p = (i >> 3) & 1;

	      fprintf (dump_file, "  ");
	      if (sym_p)
		fprintf (dump_file, "sym + ");
	      if (var_p)
		fprintf (dump_file, "var + ");
	      if (off_p)
		fprintf (dump_file, "cst + ");
	      if (rat_p)
		fprintf (dump_file, "rat * ");

	      acost = data->costs[sym_p][var_p][off_p][rat_p];
	      fprintf (dump_file, "index costs %d\n", acost);
	    }
	  if (has_predec[mem_mode] || has_postdec[mem_mode]
	      || has_preinc[mem_mode] || has_postinc[mem_mode])
	    fprintf (dump_file, "  May include autoinc/dec\n");
	  fprintf (dump_file, "\n");
	}

      address_cost_data_list[data_index] = data;
    }

  bits = GET_MODE_BITSIZE (address_mode);
  mask = ~(~(unsigned HOST_WIDE_INT) 0 << (bits - 1) << 1);
  offset &= mask;
  if ((offset >> (bits - 1) & 1))
    offset |= ~mask;
  s_offset = offset;

  autoinc = false;
  autoinc_type = AINC_NONE;
  msize = GET_MODE_SIZE (mem_mode);
  autoinc_offset = offset;
  if (stmt_after_inc)
    autoinc_offset += ratio * cstep;
  if (symbol_present || var_present || ratio != 1)
    autoinc = false;
  else
    {
      if (has_postinc[mem_mode] && autoinc_offset == 0
	  && msize == cstep)
	autoinc_type = AINC_POST_INC;
      else if (has_postdec[mem_mode] && autoinc_offset == 0
	       && msize == -cstep)
	autoinc_type = AINC_POST_DEC;
      else if (has_preinc[mem_mode] && autoinc_offset == msize
	       && msize == cstep)
	autoinc_type = AINC_PRE_INC;
      else if (has_predec[mem_mode] && autoinc_offset == -msize
	       && msize == -cstep)
	autoinc_type = AINC_PRE_DEC;

      if (autoinc_type != AINC_NONE)
	autoinc = true;
    }

  cost = 0;
  offset_p = (s_offset != 0
	      && data->min_offset <= s_offset
	      && s_offset <= data->max_offset);
  ratio_p = (ratio != 1
	     && multiplier_allowed_in_address_p (ratio, mem_mode, as));

  if (ratio != 1 && !ratio_p)
    cost += mult_by_coeff_cost (ratio, address_mode, speed);

  if (s_offset && !offset_p && !symbol_present)
    cost += add_cost (speed, address_mode);

  if (may_autoinc)
    *may_autoinc = autoinc;
  if (autoinc)
    acost = data->ainc_costs[autoinc_type];
  else
    acost = data->costs[symbol_present][var_present][offset_p][ratio_p];
  complexity = (symbol_present != 0) + (var_present != 0) + offset_p + ratio_p;
  return new_cost (cost + acost, complexity);
}

 /* Calculate the SPEED or size cost of shiftadd EXPR in MODE.  MULT is the
    EXPR operand holding the shift.  COST0 and COST1 are the costs for
    calculating the operands of EXPR.  Returns true if successful, and returns
    the cost in COST.  */

static bool
get_shiftadd_cost (tree expr, machine_mode mode, comp_cost cost0,
                   comp_cost cost1, tree mult, bool speed, comp_cost *cost)
{
  comp_cost res;
  tree op1 = TREE_OPERAND (expr, 1);
  tree cst = TREE_OPERAND (mult, 1);
  tree multop = TREE_OPERAND (mult, 0);
  int m = exact_log2 (int_cst_value (cst));
  int maxm = MIN (BITS_PER_WORD, GET_MODE_BITSIZE (mode));
  int as_cost, sa_cost;
  bool mult_in_op1;

  if (!(m >= 0 && m < maxm))
    return false;

  STRIP_NOPS (op1);
  mult_in_op1 = operand_equal_p (op1, mult, 0);

  as_cost = add_cost (speed, mode) + shift_cost (speed, mode, m);

  /* If the target has a cheap shift-and-add or shift-and-sub instruction,
     use that in preference to a shift insn followed by an add insn.  */
  sa_cost = (TREE_CODE (expr) != MINUS_EXPR
             ? shiftadd_cost (speed, mode, m)
             : (mult_in_op1
                ? shiftsub1_cost (speed, mode, m)
                : shiftsub0_cost (speed, mode, m)));

  res = new_cost (MIN (as_cost, sa_cost), 0);
  res = add_costs (res, mult_in_op1 ? cost0 : cost1);

  STRIP_NOPS (multop);
  if (!is_gimple_val (multop))
    res = add_costs (res, force_expr_to_var_cost (multop, speed));

  *cost = res;
  return true;
}

/* Estimates cost of forcing expression EXPR into a variable.  */

static comp_cost
force_expr_to_var_cost (tree expr, bool speed)
{
  static bool costs_initialized = false;
  static unsigned integer_cost [2];
  static unsigned symbol_cost [2];
  static unsigned address_cost [2];
  tree op0, op1;
  comp_cost cost0, cost1, cost;
  machine_mode mode;

  if (!costs_initialized)
    {
      tree type = build_pointer_type (integer_type_node);
      tree var, addr;
      rtx x;
      int i;

      var = create_tmp_var_raw (integer_type_node, "test_var");
      TREE_STATIC (var) = 1;
      x = produce_memory_decl_rtl (var, NULL);
      SET_DECL_RTL (var, x);

      addr = build1 (ADDR_EXPR, type, var);


      for (i = 0; i < 2; i++)
	{
	  integer_cost[i] = computation_cost (build_int_cst (integer_type_node,
							     2000), i);

	  symbol_cost[i] = computation_cost (addr, i) + 1;

	  address_cost[i]
	    = computation_cost (fold_build_pointer_plus_hwi (addr, 2000), i) + 1;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "force_expr_to_var_cost %s costs:\n", i ? "speed" : "size");
	      fprintf (dump_file, "  integer %d\n", (int) integer_cost[i]);
	      fprintf (dump_file, "  symbol %d\n", (int) symbol_cost[i]);
	      fprintf (dump_file, "  address %d\n", (int) address_cost[i]);
	      fprintf (dump_file, "  other %d\n", (int) target_spill_cost[i]);
	      fprintf (dump_file, "\n");
	    }
	}

      costs_initialized = true;
    }

  STRIP_NOPS (expr);

  if (SSA_VAR_P (expr))
    return no_cost;

  if (is_gimple_min_invariant (expr))
    {
      if (TREE_CODE (expr) == INTEGER_CST)
	return new_cost (integer_cost [speed], 0);

      if (TREE_CODE (expr) == ADDR_EXPR)
	{
	  tree obj = TREE_OPERAND (expr, 0);

	  if (TREE_CODE (obj) == VAR_DECL
	      || TREE_CODE (obj) == PARM_DECL
	      || TREE_CODE (obj) == RESULT_DECL)
	    return new_cost (symbol_cost [speed], 0);
	}

      return new_cost (address_cost [speed], 0);
    }

  switch (TREE_CODE (expr))
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);
      STRIP_NOPS (op0);
      STRIP_NOPS (op1);
      break;

    CASE_CONVERT:
    case NEGATE_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      STRIP_NOPS (op0);
      op1 = NULL_TREE;
      break;

    default:
      /* Just an arbitrary value, FIXME.  */
      return new_cost (target_spill_cost[speed], 0);
    }

  if (op0 == NULL_TREE
      || TREE_CODE (op0) == SSA_NAME || CONSTANT_CLASS_P (op0))
    cost0 = no_cost;
  else
    cost0 = force_expr_to_var_cost (op0, speed);

  if (op1 == NULL_TREE
      || TREE_CODE (op1) == SSA_NAME || CONSTANT_CLASS_P (op1))
    cost1 = no_cost;
  else
    cost1 = force_expr_to_var_cost (op1, speed);

  mode = TYPE_MODE (TREE_TYPE (expr));
  switch (TREE_CODE (expr))
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case NEGATE_EXPR:
      cost = new_cost (add_cost (speed, mode), 0);
      if (TREE_CODE (expr) != NEGATE_EXPR)
        {
          tree mult = NULL_TREE;
          comp_cost sa_cost;
          if (TREE_CODE (op1) == MULT_EXPR)
            mult = op1;
          else if (TREE_CODE (op0) == MULT_EXPR)
            mult = op0;

          if (mult != NULL_TREE
              && cst_and_fits_in_hwi (TREE_OPERAND (mult, 1))
              && get_shiftadd_cost (expr, mode, cost0, cost1, mult,
                                    speed, &sa_cost))
            return sa_cost;
        }
      break;

    CASE_CONVERT:
      {
	tree inner_mode, outer_mode;
	outer_mode = TREE_TYPE (expr);
	inner_mode = TREE_TYPE (op0);
	cost = new_cost (convert_cost (TYPE_MODE (outer_mode),
				       TYPE_MODE (inner_mode), speed), 0);
      }
      break;

    case MULT_EXPR:
      if (cst_and_fits_in_hwi (op0))
	cost = new_cost (mult_by_coeff_cost (int_cst_value (op0),
					     mode, speed), 0);
      else if (cst_and_fits_in_hwi (op1))
	cost = new_cost (mult_by_coeff_cost (int_cst_value (op1),
					     mode, speed), 0);
      else
	return new_cost (target_spill_cost [speed], 0);
      break;

    default:
      gcc_unreachable ();
    }

  cost = add_costs (cost, cost0);
  cost = add_costs (cost, cost1);

  /* Bound the cost by target_spill_cost.  The parts of complicated
     computations often are either loop invariant or at least can
     be shared between several iv uses, so letting this grow without
     limits would not give reasonable results.  */
  if (cost.cost > (int) target_spill_cost [speed])
    cost.cost = target_spill_cost [speed];

  return cost;
}

/* Estimates cost of forcing EXPR into a variable.  DEPENDS_ON is a set of the
   invariants the computation depends on.  */

static comp_cost
force_var_cost (struct ivopts_data *data,
		tree expr, bitmap *depends_on)
{
  if (depends_on)
    {
      fd_ivopts_data = data;
      walk_tree (&expr, find_depends, depends_on, NULL);
    }

  return force_expr_to_var_cost (expr, data->speed);
}

/* Estimates cost of expressing address ADDR  as var + symbol + offset.  The
   value of offset is added to OFFSET, SYMBOL_PRESENT and VAR_PRESENT are set
   to false if the corresponding part is missing.  DEPENDS_ON is a set of the
   invariants the computation depends on.  */

static comp_cost
split_address_cost (struct ivopts_data *data,
		    tree addr, bool *symbol_present, bool *var_present,
		    unsigned HOST_WIDE_INT *offset, bitmap *depends_on)
{
  tree core;
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  tree toffset;
  machine_mode mode;
  int unsignedp, reversep, volatilep;

  core = get_inner_reference (addr, &bitsize, &bitpos, &toffset, &mode,
			      &unsignedp, &reversep, &volatilep, false);

  if (toffset != 0
      || bitpos % BITS_PER_UNIT != 0
      || reversep
      || TREE_CODE (core) != VAR_DECL)
    {
      *symbol_present = false;
      *var_present = true;
      fd_ivopts_data = data;
      if (depends_on)
	walk_tree (&addr, find_depends, depends_on, NULL);

      return new_cost (target_spill_cost[data->speed], 0);
    }

  *offset += bitpos / BITS_PER_UNIT;
  if (TREE_STATIC (core)
      || DECL_EXTERNAL (core))
    {
      *symbol_present = true;
      *var_present = false;
      return no_cost;
    }

  *symbol_present = false;
  *var_present = true;
  return no_cost;
}

/* Estimates cost of expressing difference of addresses E1 - E2 as
   var + symbol + offset.  The value of offset is added to OFFSET,
   SYMBOL_PRESENT and VAR_PRESENT are set to false if the corresponding
   part is missing.  DEPENDS_ON is a set of the invariants the computation
   depends on.  */

static comp_cost
ptr_difference_cost (struct ivopts_data *data,
		     tree e1, tree e2, bool *symbol_present, bool *var_present,
		     unsigned HOST_WIDE_INT *offset, bitmap *depends_on)
{
  HOST_WIDE_INT diff = 0;
  aff_tree aff_e1, aff_e2;
  tree type;

  gcc_assert (TREE_CODE (e1) == ADDR_EXPR);

  if (ptr_difference_const (e1, e2, &diff))
    {
      *offset += diff;
      *symbol_present = false;
      *var_present = false;
      return no_cost;
    }

  if (integer_zerop (e2))
    return split_address_cost (data, TREE_OPERAND (e1, 0),
			       symbol_present, var_present, offset, depends_on);

  *symbol_present = false;
  *var_present = true;

  type = signed_type_for (TREE_TYPE (e1));
  tree_to_aff_combination (e1, type, &aff_e1);
  tree_to_aff_combination (e2, type, &aff_e2);
  aff_combination_scale (&aff_e2, -1);
  aff_combination_add (&aff_e1, &aff_e2);

  return force_var_cost (data, aff_combination_to_tree (&aff_e1), depends_on);
}

/* Estimates cost of expressing difference E1 - E2 as
   var + symbol + offset.  The value of offset is added to OFFSET,
   SYMBOL_PRESENT and VAR_PRESENT are set to false if the corresponding
   part is missing.  DEPENDS_ON is a set of the invariants the computation
   depends on.  */

static comp_cost
difference_cost (struct ivopts_data *data,
		 tree e1, tree e2, bool *symbol_present, bool *var_present,
		 unsigned HOST_WIDE_INT *offset, bitmap *depends_on)
{
  machine_mode mode = TYPE_MODE (TREE_TYPE (e1));
  unsigned HOST_WIDE_INT off1, off2;
  aff_tree aff_e1, aff_e2;
  tree type;

  e1 = strip_offset (e1, &off1);
  e2 = strip_offset (e2, &off2);
  *offset += off1 - off2;

  STRIP_NOPS (e1);
  STRIP_NOPS (e2);

  if (TREE_CODE (e1) == ADDR_EXPR)
    return ptr_difference_cost (data, e1, e2, symbol_present, var_present,
				offset, depends_on);
  *symbol_present = false;

  if (operand_equal_p (e1, e2, 0))
    {
      *var_present = false;
      return no_cost;
    }

  *var_present = true;

  if (integer_zerop (e2))
    return force_var_cost (data, e1, depends_on);

  if (integer_zerop (e1))
    {
      comp_cost cost = force_var_cost (data, e2, depends_on);
      cost.cost += mult_by_coeff_cost (-1, mode, data->speed);
      return cost;
    }

  type = signed_type_for (TREE_TYPE (e1));
  tree_to_aff_combination (e1, type, &aff_e1);
  tree_to_aff_combination (e2, type, &aff_e2);
  aff_combination_scale (&aff_e2, -1);
  aff_combination_add (&aff_e1, &aff_e2);

  return force_var_cost (data, aff_combination_to_tree (&aff_e1), depends_on);
}

/* Returns true if AFF1 and AFF2 are identical.  */

static bool
compare_aff_trees (aff_tree *aff1, aff_tree *aff2)
{
  unsigned i;

  if (aff1->n != aff2->n)
    return false;

  for (i = 0; i < aff1->n; i++)
    {
      if (aff1->elts[i].coef != aff2->elts[i].coef)
        return false;

      if (!operand_equal_p (aff1->elts[i].val, aff2->elts[i].val, 0))
        return false;
    }
  return true;
}

/* Stores EXPR in DATA->inv_expr_tab, and assigns it an inv_expr_id.  */

static int
get_expr_id (struct ivopts_data *data, tree expr)
{
  struct iv_inv_expr_ent ent;
  struct iv_inv_expr_ent **slot;

  ent.expr = expr;
  ent.hash = iterative_hash_expr (expr, 0);
  slot = data->inv_expr_tab->find_slot (&ent, INSERT);
  if (*slot)
    return (*slot)->id;

  *slot = XNEW (struct iv_inv_expr_ent);
  (*slot)->expr = expr;
  (*slot)->hash = ent.hash;
  (*slot)->id = data->inv_expr_id++;
  return (*slot)->id;
}

/* Returns the pseudo expr id if expression UBASE - RATIO * CBASE
   requires a new compiler generated temporary.  Returns -1 otherwise.
   ADDRESS_P is a flag indicating if the expression is for address
   computation.  */

static int
get_loop_invariant_expr_id (struct ivopts_data *data, tree ubase,
                            tree cbase, HOST_WIDE_INT ratio,
                            bool address_p)
{
  aff_tree ubase_aff, cbase_aff;
  tree expr, ub, cb;

  STRIP_NOPS (ubase);
  STRIP_NOPS (cbase);
  ub = ubase;
  cb = cbase;

  if ((TREE_CODE (ubase) == INTEGER_CST)
      && (TREE_CODE (cbase) == INTEGER_CST))
    return -1;

  /* Strips the constant part. */
  if (TREE_CODE (ubase) == PLUS_EXPR
      || TREE_CODE (ubase) == MINUS_EXPR
      || TREE_CODE (ubase) == POINTER_PLUS_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (ubase, 1)) == INTEGER_CST)
        ubase = TREE_OPERAND (ubase, 0);
    }

  /* Strips the constant part. */
  if (TREE_CODE (cbase) == PLUS_EXPR
      || TREE_CODE (cbase) == MINUS_EXPR
      || TREE_CODE (cbase) == POINTER_PLUS_EXPR)
    {
      if (TREE_CODE (TREE_OPERAND (cbase, 1)) == INTEGER_CST)
        cbase = TREE_OPERAND (cbase, 0);
    }

  if (address_p)
    {
      if (((TREE_CODE (ubase) == SSA_NAME)
           || (TREE_CODE (ubase) == ADDR_EXPR
               && is_gimple_min_invariant (ubase)))
          && (TREE_CODE (cbase) == INTEGER_CST))
        return -1;

      if (((TREE_CODE (cbase) == SSA_NAME)
           || (TREE_CODE (cbase) == ADDR_EXPR
               && is_gimple_min_invariant (cbase)))
          && (TREE_CODE (ubase) == INTEGER_CST))
        return -1;
    }

  if (ratio == 1)
    {
      if (operand_equal_p (ubase, cbase, 0))
        return -1;

      if (TREE_CODE (ubase) == ADDR_EXPR
          && TREE_CODE (cbase) == ADDR_EXPR)
        {
          tree usym, csym;

          usym = TREE_OPERAND (ubase, 0);
          csym = TREE_OPERAND (cbase, 0);
          if (TREE_CODE (usym) == ARRAY_REF)
            {
              tree ind = TREE_OPERAND (usym, 1);
              if (TREE_CODE (ind) == INTEGER_CST
                  && tree_fits_shwi_p (ind)
                  && tree_to_shwi (ind) == 0)
                usym = TREE_OPERAND (usym, 0);
            }
          if (TREE_CODE (csym) == ARRAY_REF)
            {
              tree ind = TREE_OPERAND (csym, 1);
              if (TREE_CODE (ind) == INTEGER_CST
                  && tree_fits_shwi_p (ind)
                  && tree_to_shwi (ind) == 0)
                csym = TREE_OPERAND (csym, 0);
            }
          if (operand_equal_p (usym, csym, 0))
            return -1;
        }
      /* Now do more complex comparison  */
      tree_to_aff_combination (ubase, TREE_TYPE (ubase), &ubase_aff);
      tree_to_aff_combination (cbase, TREE_TYPE (cbase), &cbase_aff);
      if (compare_aff_trees (&ubase_aff, &cbase_aff))
        return -1;
    }

  tree_to_aff_combination (ub, TREE_TYPE (ub), &ubase_aff);
  tree_to_aff_combination (cb, TREE_TYPE (cb), &cbase_aff);

  aff_combination_scale (&cbase_aff, -1 * ratio);
  aff_combination_add (&ubase_aff, &cbase_aff);
  expr = aff_combination_to_tree (&ubase_aff);
  return get_expr_id (data, expr);
}



/* Determines the cost of the computation by that USE is expressed
   from induction variable CAND.  If ADDRESS_P is true, we just need
   to create an address from it, otherwise we want to get it into
   register.  A set of invariants we depend on is stored in
   DEPENDS_ON.  AT is the statement at that the value is computed.
   If CAN_AUTOINC is nonnull, use it to record whether autoinc
   addressing is likely.  */

static comp_cost
get_computation_cost_at (struct ivopts_data *data,
			 struct iv_use *use, struct iv_cand *cand,
			 bool address_p, bitmap *depends_on, gimple *at,
			 bool *can_autoinc,
                         int *inv_expr_id)
{
  tree ubase = use->iv->base, ustep = use->iv->step;
  tree cbase, cstep;
  tree utype = TREE_TYPE (ubase), ctype;
  unsigned HOST_WIDE_INT cstepi, offset = 0;
  HOST_WIDE_INT ratio, aratio;
  bool var_present, symbol_present, stmt_is_after_inc;
  comp_cost cost;
  widest_int rat;
  bool speed = optimize_bb_for_speed_p (gimple_bb (at));
  machine_mode mem_mode = (address_p
				? TYPE_MODE (TREE_TYPE (*use->op_p))
				: VOIDmode);

  if (depends_on)
    *depends_on = NULL;

  /* Only consider real candidates.  */
  if (!cand->iv)
    return infinite_cost;

  cbase = cand->iv->base;
  cstep = cand->iv->step;
  ctype = TREE_TYPE (cbase);

  if (TYPE_PRECISION (utype) > TYPE_PRECISION (ctype))
    {
      /* We do not have a precision to express the values of use.  */
      return infinite_cost;
    }

  if (address_p
      || (use->iv->base_object
	  && cand->iv->base_object
	  && POINTER_TYPE_P (TREE_TYPE (use->iv->base_object))
	  && POINTER_TYPE_P (TREE_TYPE (cand->iv->base_object))))
    {
      /* Do not try to express address of an object with computation based
	 on address of a different object.  This may cause problems in rtl
	 level alias analysis (that does not expect this to be happening,
	 as this is illegal in C), and would be unlikely to be useful
	 anyway.  */
      if (use->iv->base_object
	  && cand->iv->base_object
	  && !operand_equal_p (use->iv->base_object, cand->iv->base_object, 0))
	return infinite_cost;
    }

  if (TYPE_PRECISION (utype) < TYPE_PRECISION (ctype))
    {
      /* TODO -- add direct handling of this case.  */
      goto fallback;
    }

  /* CSTEPI is removed from the offset in case statement is after the
     increment.  If the step is not constant, we use zero instead.
     This is a bit imprecise (there is the extra addition), but
     redundancy elimination is likely to transform the code so that
     it uses value of the variable before increment anyway,
     so it is not that much unrealistic.  */
  if (cst_and_fits_in_hwi (cstep))
    cstepi = int_cst_value (cstep);
  else
    cstepi = 0;

  if (!constant_multiple_of (ustep, cstep, &rat))
    return infinite_cost;

  if (wi::fits_shwi_p (rat))
    ratio = rat.to_shwi ();
  else
    return infinite_cost;

  STRIP_NOPS (cbase);
  ctype = TREE_TYPE (cbase);

  stmt_is_after_inc = stmt_after_increment (data->current_loop, cand, at);

  /* use = ubase + ratio * (var - cbase).  If either cbase is a constant
     or ratio == 1, it is better to handle this like

     ubase - ratio * cbase + ratio * var

     (also holds in the case ratio == -1, TODO.  */

  if (cst_and_fits_in_hwi (cbase))
    {
      offset = - ratio * (unsigned HOST_WIDE_INT) int_cst_value (cbase);
      cost = difference_cost (data,
			      ubase, build_int_cst (utype, 0),
			      &symbol_present, &var_present, &offset,
			      depends_on);
      cost.cost /= avg_loop_niter (data->current_loop);
    }
  else if (ratio == 1)
    {
      tree real_cbase = cbase;

      /* Check to see if any adjustment is needed.  */
      if (cstepi == 0 && stmt_is_after_inc)
        {
          aff_tree real_cbase_aff;
          aff_tree cstep_aff;

          tree_to_aff_combination (cbase, TREE_TYPE (real_cbase),
                                   &real_cbase_aff);
          tree_to_aff_combination (cstep, TREE_TYPE (cstep), &cstep_aff);

          aff_combination_add (&real_cbase_aff, &cstep_aff);
          real_cbase = aff_combination_to_tree (&real_cbase_aff);
        }

      cost = difference_cost (data,
			      ubase, real_cbase,
			      &symbol_present, &var_present, &offset,
			      depends_on);
      cost.cost /= avg_loop_niter (data->current_loop);
    }
  else if (address_p
	   && !POINTER_TYPE_P (ctype)
	   && multiplier_allowed_in_address_p
		(ratio, mem_mode,
			TYPE_ADDR_SPACE (TREE_TYPE (utype))))
    {
      tree real_cbase = cbase;

      if (cstepi == 0 && stmt_is_after_inc)
	{
	  if (POINTER_TYPE_P (ctype))
	    real_cbase = fold_build2 (POINTER_PLUS_EXPR, ctype, cbase, cstep);
	  else
	    real_cbase = fold_build2 (PLUS_EXPR, ctype, cbase, cstep);
	}
      real_cbase = fold_build2 (MULT_EXPR, ctype, real_cbase,
				build_int_cst (ctype, ratio));
      cost = difference_cost (data,
			      ubase, real_cbase,
			      &symbol_present, &var_present, &offset,
			      depends_on);
      cost.cost /= avg_loop_niter (data->current_loop);
    }
  else
    {
      cost = force_var_cost (data, cbase, depends_on);
      cost = add_costs (cost,
			difference_cost (data,
					 ubase, build_int_cst (utype, 0),
					 &symbol_present, &var_present,
					 &offset, depends_on));
      cost.cost /= avg_loop_niter (data->current_loop);
      cost.cost += add_cost (data->speed, TYPE_MODE (ctype));
    }

  /* Record setup cost in scrach field.  */
  cost.scratch = cost.cost;

  if (inv_expr_id && depends_on && *depends_on)
    {
      *inv_expr_id =
          get_loop_invariant_expr_id (data, ubase, cbase, ratio, address_p);
      /* Clear depends on.  */
      if (*inv_expr_id != -1)
        bitmap_clear (*depends_on);
    }

  /* If we are after the increment, the value of the candidate is higher by
     one iteration.  */
  if (stmt_is_after_inc)
    offset -= ratio * cstepi;

  /* Now the computation is in shape symbol + var1 + const + ratio * var2.
     (symbol/var1/const parts may be omitted).  If we are looking for an
     address, find the cost of addressing this.  */
  if (address_p)
    return add_costs (cost,
		      get_address_cost (symbol_present, var_present,
					offset, ratio, cstepi,
					mem_mode,
					TYPE_ADDR_SPACE (TREE_TYPE (utype)),
					speed, stmt_is_after_inc,
					can_autoinc));

  /* Otherwise estimate the costs for computing the expression.  */
  if (!symbol_present && !var_present && !offset)
    {
      if (ratio != 1)
	cost.cost += mult_by_coeff_cost (ratio, TYPE_MODE (ctype), speed);
      return cost;
    }

  /* Symbol + offset should be compile-time computable so consider that they
      are added once to the variable, if present.  */
  if (var_present && (symbol_present || offset))
    cost.cost += adjust_setup_cost (data,
				    add_cost (speed, TYPE_MODE (ctype)));

  /* Having offset does not affect runtime cost in case it is added to
     symbol, but it increases complexity.  */
  if (offset)
    cost.complexity++;

  cost.cost += add_cost (speed, TYPE_MODE (ctype));

  aratio = ratio > 0 ? ratio : -ratio;
  if (aratio != 1)
    cost.cost += mult_by_coeff_cost (aratio, TYPE_MODE (ctype), speed);
  return cost;

fallback:
  if (can_autoinc)
    *can_autoinc = false;

  {
    /* Just get the expression, expand it and measure the cost.  */
    tree comp = get_computation_at (data->current_loop, use, cand, at);

    if (!comp)
      return infinite_cost;

    if (address_p)
      comp = build_simple_mem_ref (comp);

    cost = new_cost (computation_cost (comp, speed), 0);
    cost.scratch = 0;
    return cost;
  }
}

/* Determines the cost of the computation by that USE is expressed
   from induction variable CAND.  If ADDRESS_P is true, we just need
   to create an address from it, otherwise we want to get it into
   register.  A set of invariants we depend on is stored in
   DEPENDS_ON.  If CAN_AUTOINC is nonnull, use it to record whether
   autoinc addressing is likely.  */

static comp_cost
get_computation_cost (struct ivopts_data *data,
		      struct iv_use *use, struct iv_cand *cand,
		      bool address_p, bitmap *depends_on,
                      bool *can_autoinc, int *inv_expr_id)
{
  return get_computation_cost_at (data,
				  use, cand, address_p, depends_on, use->stmt,
				  can_autoinc, inv_expr_id);
}

/* Determines cost of computing the use in GROUP with CAND in a generic
   expression.  */

static bool
determine_group_iv_cost_generic (struct ivopts_data *data,
				 struct iv_group *group, struct iv_cand *cand)
{
  comp_cost cost;
  int inv_expr_id = -1;
  bitmap depends_on = NULL;
  struct iv_use *use = group->vuses[0];

  /* The simple case first -- if we need to express value of the preserved
     original biv, the cost is 0.  This also prevents us from counting the
     cost of increment twice -- once at this use and once in the cost of
     the candidate.  */
  if (cand->pos == IP_ORIGINAL && cand->incremented_at == use->stmt)
    cost = no_cost;
  else
    cost = get_computation_cost (data, use, cand, false,
				 &depends_on, NULL, &inv_expr_id);

  set_group_iv_cost (data, group, cand, cost, depends_on,
		     NULL_TREE, ERROR_MARK, inv_expr_id);
  return !infinite_cost_p (cost);
}

/* Determines cost of computing uses in GROUP with CAND in addresses.  */

static bool
determine_group_iv_cost_address (struct ivopts_data *data,
				 struct iv_group *group, struct iv_cand *cand)
{
  unsigned i;
  bitmap depends_on;
  bool can_autoinc, first = true;
  int inv_expr_id = -1;
  struct iv_use *use = group->vuses[0];
  comp_cost sum_cost = no_cost, cost;

  cost = get_computation_cost (data, use, cand, true,
			       &depends_on, &can_autoinc, &inv_expr_id);

  sum_cost = cost;
  if (!infinite_cost_p (sum_cost) && cand->ainc_use == use)
    {
      if (can_autoinc)
	sum_cost.cost -= cand->cost_step;
      /* If we generated the candidate solely for exploiting autoincrement
	 opportunities, and it turns out it can't be used, set the cost to
	 infinity to make sure we ignore it.  */
      else if (cand->pos == IP_AFTER_USE || cand->pos == IP_BEFORE_USE)
	sum_cost = infinite_cost;
    }

  /* Uses in a group can share setup code, so only add setup cost once.  */
  cost.cost -= cost.scratch;
  /* Compute and add costs for rest uses of this group.  */
  for (i = 1; i < group->vuses.length () && !infinite_cost_p (sum_cost); i++)
    {
      struct iv_use *next = group->vuses[i];

      /* Compute cost for the first use with different offset to the main
	 use and add it afterwards.  Costs for these uses could be quite
	 different.  Given below uses in a group:
	   use 0  : {base + A + offset_0, step}
	   use 0.1: {base + A + offset_0, step}
	   use 0.2: {base + A + offset_1, step}
	   use 0.3: {base + A + offset_2, step}
	 when we need to compute costs with candidate:
	   cand 1 : {base + B + offset_0, step}

	 The first use with different offset is use 0.2, its cost is larger
	 than cost of use 0/0.1 because we need to compute:
	   A - B + offset_1 - offset_0
	   rather than:
	   A - B.  */
      if (first && next->addr_offset != use->addr_offset)
	{
	  first = false;
	  cost = get_computation_cost (data, next, cand, true,
				       NULL, &can_autoinc, NULL);
	  /* Remove setup cost.  */
	  if (!infinite_cost_p (cost))
	    cost.cost -= cost.scratch;
	}
      sum_cost = add_costs (sum_cost, cost);
    }
  set_group_iv_cost (data, group, cand, sum_cost, depends_on,
		     NULL_TREE, ERROR_MARK, inv_expr_id);

  return !infinite_cost_p (sum_cost);
}

/* Computes value of candidate CAND at position AT in iteration NITER, and
   stores it to VAL.  */

static void
cand_value_at (struct loop *loop, struct iv_cand *cand, gimple *at, tree niter,
	       aff_tree *val)
{
  aff_tree step, delta, nit;
  struct iv *iv = cand->iv;
  tree type = TREE_TYPE (iv->base);
  tree steptype = type;
  if (POINTER_TYPE_P (type))
    steptype = sizetype;
  steptype = unsigned_type_for (type);

  tree_to_aff_combination (iv->step, TREE_TYPE (iv->step), &step);
  aff_combination_convert (&step, steptype);
  tree_to_aff_combination (niter, TREE_TYPE (niter), &nit);
  aff_combination_convert (&nit, steptype);
  aff_combination_mult (&nit, &step, &delta);
  if (stmt_after_increment (loop, cand, at))
    aff_combination_add (&delta, &step);

  tree_to_aff_combination (iv->base, type, val);
  if (!POINTER_TYPE_P (type))
    aff_combination_convert (val, steptype);
  aff_combination_add (val, &delta);
}

/* Returns period of induction variable iv.  */

static tree
iv_period (struct iv *iv)
{
  tree step = iv->step, period, type;
  tree pow2div;

  gcc_assert (step && TREE_CODE (step) == INTEGER_CST);

  type = unsigned_type_for (TREE_TYPE (step));
  /* Period of the iv is lcm (step, type_range)/step -1,
     i.e., N*type_range/step - 1. Since type range is power
     of two, N == (step >> num_of_ending_zeros_binary (step),
     so the final result is

       (type_range >> num_of_ending_zeros_binary (step)) - 1

  */
  pow2div = num_ending_zeros (step);

  period = build_low_bits_mask (type,
                                (TYPE_PRECISION (type)
                                 - tree_to_uhwi (pow2div)));

  return period;
}

/* Returns the comparison operator used when eliminating the iv USE.  */

static enum tree_code
iv_elimination_compare (struct ivopts_data *data, struct iv_use *use)
{
  struct loop *loop = data->current_loop;
  basic_block ex_bb;
  edge exit;

  ex_bb = gimple_bb (use->stmt);
  exit = EDGE_SUCC (ex_bb, 0);
  if (flow_bb_inside_loop_p (loop, exit->dest))
    exit = EDGE_SUCC (ex_bb, 1);

  return (exit->flags & EDGE_TRUE_VALUE ? EQ_EXPR : NE_EXPR);
}

/* Returns true if we can prove that BASE - OFFSET does not overflow.  For now,
   we only detect the situation that BASE = SOMETHING + OFFSET, where the
   calculation is performed in non-wrapping type.

   TODO: More generally, we could test for the situation that
	 BASE = SOMETHING + OFFSET' and OFFSET is between OFFSET' and zero.
	 This would require knowing the sign of OFFSET.  */

static bool
difference_cannot_overflow_p (struct ivopts_data *data, tree base, tree offset)
{
  enum tree_code code;
  tree e1, e2;
  aff_tree aff_e1, aff_e2, aff_offset;

  if (!nowrap_type_p (TREE_TYPE (base)))
    return false;

  base = expand_simple_operations (base);

  if (TREE_CODE (base) == SSA_NAME)
    {
      gimple *stmt = SSA_NAME_DEF_STMT (base);

      if (gimple_code (stmt) != GIMPLE_ASSIGN)
	return false;

      code = gimple_assign_rhs_code (stmt);
      if (get_gimple_rhs_class (code) != GIMPLE_BINARY_RHS)
	return false;

      e1 = gimple_assign_rhs1 (stmt);
      e2 = gimple_assign_rhs2 (stmt);
    }
  else
    {
      code = TREE_CODE (base);
      if (get_gimple_rhs_class (code) != GIMPLE_BINARY_RHS)
	return false;
      e1 = TREE_OPERAND (base, 0);
      e2 = TREE_OPERAND (base, 1);
    }

  /* Use affine expansion as deeper inspection to prove the equality.  */
  tree_to_aff_combination_expand (e2, TREE_TYPE (e2),
				  &aff_e2, &data->name_expansion_cache);
  tree_to_aff_combination_expand (offset, TREE_TYPE (offset),
				  &aff_offset, &data->name_expansion_cache);
  aff_combination_scale (&aff_offset, -1);
  switch (code)
    {
    case PLUS_EXPR:
      aff_combination_add (&aff_e2, &aff_offset);
      if (aff_combination_zero_p (&aff_e2))
	return true;

      tree_to_aff_combination_expand (e1, TREE_TYPE (e1),
				      &aff_e1, &data->name_expansion_cache);
      aff_combination_add (&aff_e1, &aff_offset);
      return aff_combination_zero_p (&aff_e1);

    case POINTER_PLUS_EXPR:
      aff_combination_add (&aff_e2, &aff_offset);
      return aff_combination_zero_p (&aff_e2);

    default:
      return false;
    }
}

/* Tries to replace loop exit by one formulated in terms of a LT_EXPR
   comparison with CAND.  NITER describes the number of iterations of
   the loops.  If successful, the comparison in COMP_P is altered accordingly.

   We aim to handle the following situation:

   sometype *base, *p;
   int a, b, i;

   i = a;
   p = p_0 = base + a;

   do
     {
       bla (*p);
       p++;
       i++;
     }
   while (i < b);

   Here, the number of iterations of the loop is (a + 1 > b) ? 0 : b - a - 1.
   We aim to optimize this to

   p = p_0 = base + a;
   do
     {
       bla (*p);
       p++;
     }
   while (p < p_0 - a + b);

   This preserves the correctness, since the pointer arithmetics does not
   overflow.  More precisely:

   1) if a + 1 <= b, then p_0 - a + b is the final value of p, hence there is no
      overflow in computing it or the values of p.
   2) if a + 1 > b, then we need to verify that the expression p_0 - a does not
      overflow.  To prove this, we use the fact that p_0 = base + a.  */

static bool
iv_elimination_compare_lt (struct ivopts_data *data,
                           struct iv_cand *cand, enum tree_code *comp_p,
			   struct tree_niter_desc *niter)
{
  tree cand_type, a, b, mbz, nit_type = TREE_TYPE (niter->niter), offset;
  struct aff_tree nit, tmpa, tmpb;
  enum tree_code comp;
  HOST_WIDE_INT step;

  /* We need to know that the candidate induction variable does not overflow.
     While more complex analysis may be used to prove this, for now just
     check that the variable appears in the original program and that it
     is computed in a type that guarantees no overflows.  */
  cand_type = TREE_TYPE (cand->iv->base);
  if (cand->pos != IP_ORIGINAL || !nowrap_type_p (cand_type))
    return false;

  /* Make sure that the loop iterates till the loop bound is hit, as otherwise
     the calculation of the BOUND could overflow, making the comparison
     invalid.  */
  if (!data->loop_single_exit_p)
    return false;

  /* We need to be able to decide whether candidate is increasing or decreasing
     in order to choose the right comparison operator.  */
  if (!cst_and_fits_in_hwi (cand->iv->step))
    return false;
  step = int_cst_value (cand->iv->step);

  /* Check that the number of iterations matches the expected pattern:
     a + 1 > b ? 0 : b - a - 1.  */
  mbz = niter->may_be_zero;
  if (TREE_CODE (mbz) == GT_EXPR)
    {
      /* Handle a + 1 > b.  */
      tree op0 = TREE_OPERAND (mbz, 0);
      if (TREE_CODE (op0) == PLUS_EXPR && integer_onep (TREE_OPERAND (op0, 1)))
	{
	  a = TREE_OPERAND (op0, 0);
	  b = TREE_OPERAND (mbz, 1);
	}
      else
	return false;
    }
  else if (TREE_CODE (mbz) == LT_EXPR)
    {
      tree op1 = TREE_OPERAND (mbz, 1);

      /* Handle b < a + 1.  */
      if (TREE_CODE (op1) == PLUS_EXPR && integer_onep (TREE_OPERAND (op1, 1)))
        {
          a = TREE_OPERAND (op1, 0);
          b = TREE_OPERAND (mbz, 0);
        }
      else
	return false;
    }
  else
    return false;

  /* Expected number of iterations is B - A - 1.  Check that it matches
     the actual number, i.e., that B - A - NITER = 1.  */
  tree_to_aff_combination (niter->niter, nit_type, &nit);
  tree_to_aff_combination (fold_convert (nit_type, a), nit_type, &tmpa);
  tree_to_aff_combination (fold_convert (nit_type, b), nit_type, &tmpb);
  aff_combination_scale (&nit, -1);
  aff_combination_scale (&tmpa, -1);
  aff_combination_add (&tmpb, &tmpa);
  aff_combination_add (&tmpb, &nit);
  if (tmpb.n != 0 || tmpb.offset != 1)
    return false;

  /* Finally, check that CAND->IV->BASE - CAND->IV->STEP * A does not
     overflow.  */
  offset = fold_build2 (MULT_EXPR, TREE_TYPE (cand->iv->step),
			cand->iv->step,
			fold_convert (TREE_TYPE (cand->iv->step), a));
  if (!difference_cannot_overflow_p (data, cand->iv->base, offset))
    return false;

  /* Determine the new comparison operator.  */
  comp = step < 0 ? GT_EXPR : LT_EXPR;
  if (*comp_p == NE_EXPR)
    *comp_p = comp;
  else if (*comp_p == EQ_EXPR)
    *comp_p = invert_tree_comparison (comp, false);
  else
    gcc_unreachable ();

  return true;
}

/* Check whether it is possible to express the condition in USE by comparison
   of candidate CAND.  If so, store the value compared with to BOUND, and the
   comparison operator to COMP.  */

static bool
may_eliminate_iv (struct ivopts_data *data,
		  struct iv_use *use, struct iv_cand *cand, tree *bound,
		  enum tree_code *comp)
{
  basic_block ex_bb;
  edge exit;
  tree period;
  struct loop *loop = data->current_loop;
  aff_tree bnd;
  struct tree_niter_desc *desc = NULL;

  if (TREE_CODE (cand->iv->step) != INTEGER_CST)
    return false;

  /* For now works only for exits that dominate the loop latch.
     TODO: extend to other conditions inside loop body.  */
  ex_bb = gimple_bb (use->stmt);
  if (use->stmt != last_stmt (ex_bb)
      || gimple_code (use->stmt) != GIMPLE_COND
      || !dominated_by_p (CDI_DOMINATORS, loop->latch, ex_bb))
    return false;

  exit = EDGE_SUCC (ex_bb, 0);
  if (flow_bb_inside_loop_p (loop, exit->dest))
    exit = EDGE_SUCC (ex_bb, 1);
  if (flow_bb_inside_loop_p (loop, exit->dest))
    return false;

  desc = niter_for_exit (data, exit);
  if (!desc)
    return false;

  /* Determine whether we can use the variable to test the exit condition.
     This is the case iff the period of the induction variable is greater
     than the number of iterations for which the exit condition is true.  */
  period = iv_period (cand->iv);

  /* If the number of iterations is constant, compare against it directly.  */
  if (TREE_CODE (desc->niter) == INTEGER_CST)
    {
      /* See cand_value_at.  */
      if (stmt_after_increment (loop, cand, use->stmt))
        {
          if (!tree_int_cst_lt (desc->niter, period))
            return false;
        }
      else
        {
          if (tree_int_cst_lt (period, desc->niter))
            return false;
        }
    }

  /* If not, and if this is the only possible exit of the loop, see whether
     we can get a conservative estimate on the number of iterations of the
     entire loop and compare against that instead.  */
  else
    {
      widest_int period_value, max_niter;

      max_niter = desc->max;
      if (stmt_after_increment (loop, cand, use->stmt))
        max_niter += 1;
      period_value = wi::to_widest (period);
      if (wi::gtu_p (max_niter, period_value))
        {
          /* See if we can take advantage of inferred loop bound information.  */
          if (data->loop_single_exit_p)
            {
              if (!max_loop_iterations (loop, &max_niter))
                return false;
              /* The loop bound is already adjusted by adding 1.  */
              if (wi::gtu_p (max_niter, period_value))
                return false;
            }
          else
            return false;
        }
    }

  cand_value_at (loop, cand, use->stmt, desc->niter, &bnd);

  *bound = fold_convert (TREE_TYPE (cand->iv->base),
			 aff_combination_to_tree (&bnd));
  *comp = iv_elimination_compare (data, use);

  /* It is unlikely that computing the number of iterations using division
     would be more profitable than keeping the original induction variable.  */
  if (expression_expensive_p (*bound))
    return false;

  /* Sometimes, it is possible to handle the situation that the number of
     iterations may be zero unless additional assumtions by using <
     instead of != in the exit condition.

     TODO: we could also calculate the value MAY_BE_ZERO ? 0 : NITER and
	   base the exit condition on it.  However, that is often too
	   expensive.  */
  if (!integer_zerop (desc->may_be_zero))
    return iv_elimination_compare_lt (data, cand, comp, desc);

  return true;
}

 /* Calculates the cost of BOUND, if it is a PARM_DECL.  A PARM_DECL must
    be copied, if it is used in the loop body and DATA->body_includes_call.  */

static int
parm_decl_cost (struct ivopts_data *data, tree bound)
{
  tree sbound = bound;
  STRIP_NOPS (sbound);

  if (TREE_CODE (sbound) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (sbound)
      && TREE_CODE (SSA_NAME_VAR (sbound)) == PARM_DECL
      && data->body_includes_call)
    return COSTS_N_INSNS (1);

  return 0;
}

/* Determines cost of computing the use in GROUP with CAND in a condition.  */

static bool
determine_group_iv_cost_cond (struct ivopts_data *data,
			      struct iv_group *group, struct iv_cand *cand)
{
  tree bound = NULL_TREE;
  struct iv *cmp_iv;
  bitmap depends_on_elim = NULL, depends_on_express = NULL, depends_on;
  comp_cost elim_cost, express_cost, cost, bound_cost;
  bool ok;
  int elim_inv_expr_id = -1, express_inv_expr_id = -1, inv_expr_id;
  tree *control_var, *bound_cst;
  enum tree_code comp = ERROR_MARK;
  struct iv_use *use = group->vuses[0];

  gcc_assert (cand->iv);

  /* Try iv elimination.  */
  if (may_eliminate_iv (data, use, cand, &bound, &comp))
    {
      elim_cost = force_var_cost (data, bound, &depends_on_elim);
      if (elim_cost.cost == 0)
        elim_cost.cost = parm_decl_cost (data, bound);
      else if (TREE_CODE (bound) == INTEGER_CST)
        elim_cost.cost = 0;
      /* If we replace a loop condition 'i < n' with 'p < base + n',
	 depends_on_elim will have 'base' and 'n' set, which implies
	 that both 'base' and 'n' will be live during the loop.	 More likely,
	 'base + n' will be loop invariant, resulting in only one live value
	 during the loop.  So in that case we clear depends_on_elim and set
        elim_inv_expr_id instead.  */
      if (depends_on_elim && bitmap_count_bits (depends_on_elim) > 1)
	{
	  elim_inv_expr_id = get_expr_id (data, bound);
	  bitmap_clear (depends_on_elim);
	}
      /* The bound is a loop invariant, so it will be only computed
	 once.  */
      elim_cost.cost = adjust_setup_cost (data, elim_cost.cost);
    }
  else
    elim_cost = infinite_cost;

  /* Try expressing the original giv.  If it is compared with an invariant,
     note that we cannot get rid of it.  */
  ok = extract_cond_operands (data, use->stmt, &control_var, &bound_cst,
			      NULL, &cmp_iv);
  gcc_assert (ok);

  /* When the condition is a comparison of the candidate IV against
     zero, prefer this IV.

     TODO: The constant that we're subtracting from the cost should
     be target-dependent.  This information should be added to the
     target costs for each backend.  */
  if (!infinite_cost_p (elim_cost) /* Do not try to decrease infinite! */
      && integer_zerop (*bound_cst)
      && (operand_equal_p (*control_var, cand->var_after, 0)
	  || operand_equal_p (*control_var, cand->var_before, 0)))
    elim_cost.cost -= 1;

  express_cost = get_computation_cost (data, use, cand, false,
				       &depends_on_express, NULL,
                                       &express_inv_expr_id);
  fd_ivopts_data = data;
  walk_tree (&cmp_iv->base, find_depends, &depends_on_express, NULL);

  /* Count the cost of the original bound as well.  */
  bound_cost = force_var_cost (data, *bound_cst, NULL);
  if (bound_cost.cost == 0)
    bound_cost.cost = parm_decl_cost (data, *bound_cst);
  else if (TREE_CODE (*bound_cst) == INTEGER_CST)
    bound_cost.cost = 0;
  express_cost.cost += bound_cost.cost;

  /* Choose the better approach, preferring the eliminated IV. */
  if (compare_costs (elim_cost, express_cost) <= 0)
    {
      cost = elim_cost;
      depends_on = depends_on_elim;
      depends_on_elim = NULL;
      inv_expr_id = elim_inv_expr_id;
    }
  else
    {
      cost = express_cost;
      depends_on = depends_on_express;
      depends_on_express = NULL;
      bound = NULL_TREE;
      comp = ERROR_MARK;
      inv_expr_id = express_inv_expr_id;
    }

  set_group_iv_cost (data, group, cand, cost,
		     depends_on, bound, comp, inv_expr_id);

  if (depends_on_elim)
    BITMAP_FREE (depends_on_elim);
  if (depends_on_express)
    BITMAP_FREE (depends_on_express);

  return !infinite_cost_p (cost);
}

/* Determines cost of computing uses in GROUP with CAND.  Returns false
   if USE cannot be represented with CAND.  */

static bool
determine_group_iv_cost (struct ivopts_data *data,
			 struct iv_group *group, struct iv_cand *cand)
{
  switch (group->type)
    {
    case USE_NONLINEAR_EXPR:
      return determine_group_iv_cost_generic (data, group, cand);

    case USE_ADDRESS:
      return determine_group_iv_cost_address (data, group, cand);

    case USE_COMPARE:
      return determine_group_iv_cost_cond (data, group, cand);

    default:
      gcc_unreachable ();
    }
}

/* Return true if get_computation_cost indicates that autoincrement is
   a possibility for the pair of USE and CAND, false otherwise.  */

static bool
autoinc_possible_for_pair (struct ivopts_data *data, struct iv_use *use,
			   struct iv_cand *cand)
{
  bitmap depends_on;
  bool can_autoinc;
  comp_cost cost;

  if (use->type != USE_ADDRESS)
    return false;

  cost = get_computation_cost (data, use, cand, true, &depends_on,
			       &can_autoinc, NULL);

  BITMAP_FREE (depends_on);

  return !infinite_cost_p (cost) && can_autoinc;
}

/* Examine IP_ORIGINAL candidates to see if they are incremented next to a
   use that allows autoincrement, and set their AINC_USE if possible.  */

static void
set_autoinc_for_original_candidates (struct ivopts_data *data)
{
  unsigned i, j;

  for (i = 0; i < data->vcands.length (); i++)
    {
      struct iv_cand *cand = data->vcands[i];
      struct iv_use *closest_before = NULL;
      struct iv_use *closest_after = NULL;
      if (cand->pos != IP_ORIGINAL)
	continue;

      for (j = 0; j < data->vgroups.length (); j++)
	{
	  struct iv_group *group = data->vgroups[j];
	  struct iv_use *use = group->vuses[0];
	  unsigned uid = gimple_uid (use->stmt);

	  if (gimple_bb (use->stmt) != gimple_bb (cand->incremented_at))
	    continue;

	  if (uid < gimple_uid (cand->incremented_at)
	      && (closest_before == NULL
		  || uid > gimple_uid (closest_before->stmt)))
	    closest_before = use;

	  if (uid > gimple_uid (cand->incremented_at)
	      && (closest_after == NULL
		  || uid < gimple_uid (closest_after->stmt)))
	    closest_after = use;
	}

      if (closest_before != NULL
	  && autoinc_possible_for_pair (data, closest_before, cand))
	cand->ainc_use = closest_before;
      else if (closest_after != NULL
	       && autoinc_possible_for_pair (data, closest_after, cand))
	cand->ainc_use = closest_after;
    }
}

/* Finds the candidates for the induction variables.  */

static void
find_iv_candidates (struct ivopts_data *data)
{
  /* Add commonly used ivs.  */
  add_standard_iv_candidates (data);

  /* Add old induction variables.  */
  add_iv_candidate_for_bivs (data);

  /* Add induction variables derived from uses.  */
  add_iv_candidate_for_groups (data);

  set_autoinc_for_original_candidates (data);

  /* Record the important candidates.  */
  record_important_candidates (data);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      unsigned i;

      fprintf (dump_file, "\n<Important Candidates>:\t");
      for (i = 0; i < data->vcands.length (); i++)
	if (data->vcands[i]->important)
	  fprintf (dump_file, " %d,", data->vcands[i]->id);
      fprintf (dump_file, "\n");

      fprintf (dump_file, "\n<Group, Cand> Related:\n");
      for (i = 0; i < data->vgroups.length (); i++)
	{
	  struct iv_group *group = data->vgroups[i];

	  if (group->related_cands)
	    {
	      fprintf (dump_file, "  Group %d:\t", group->id);
	      dump_bitmap (dump_file, group->related_cands);
	    }
	}
      fprintf (dump_file, "\n");
    }
}

/* Determines costs of computing use of iv with an iv candidate.  */

static void
determine_group_iv_costs (struct ivopts_data *data)
{
  unsigned i, j;
  struct iv_cand *cand;
  struct iv_group *group;
  bitmap to_clear = BITMAP_ALLOC (NULL);

  alloc_use_cost_map (data);

  for (i = 0; i < data->vgroups.length (); i++)
    {
      group = data->vgroups[i];

      if (data->consider_all_candidates)
	{
	  for (j = 0; j < data->vcands.length (); j++)
	    {
	      cand = data->vcands[j];
	      determine_group_iv_cost (data, group, cand);
	    }
	}
      else
	{
	  bitmap_iterator bi;

	  EXECUTE_IF_SET_IN_BITMAP (group->related_cands, 0, j, bi)
	    {
	      cand = data->vcands[j];
	      if (!determine_group_iv_cost (data, group, cand))
		bitmap_set_bit (to_clear, j);
	    }

	  /* Remove the candidates for that the cost is infinite from
	     the list of related candidates.  */
	  bitmap_and_compl_into (group->related_cands, to_clear);
	  bitmap_clear (to_clear);
	}
    }

  BITMAP_FREE (to_clear);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "<Group-candidate Costs>:\n");

      for (i = 0; i < data->vgroups.length (); i++)
	{
	  group = data->vgroups[i];

	  fprintf (dump_file, "Group %d:\n", i);
	  fprintf (dump_file, "  cand\tcost\tcompl.\tdepends on\n");
	  for (j = 0; j < group->n_map_members; j++)
	    {
	      if (!group->cost_map[j].cand
		  || infinite_cost_p (group->cost_map[j].cost))
		continue;

	      fprintf (dump_file, "  %d\t%d\t%d\t",
		       group->cost_map[j].cand->id,
		       group->cost_map[j].cost.cost,
		       group->cost_map[j].cost.complexity);
	      if (group->cost_map[j].depends_on)
		bitmap_print (dump_file,
			      group->cost_map[j].depends_on, "","");
	      if (group->cost_map[j].inv_expr_id != -1)
		fprintf (dump_file, " inv_expr:%d",
			 group->cost_map[j].inv_expr_id);
	      fprintf (dump_file, "\n");
	    }

	  fprintf (dump_file, "\n");
	}
      fprintf (dump_file, "\n");
    }
}

/* Determines cost of the candidate CAND.  */

static void
determine_iv_cost (struct ivopts_data *data, struct iv_cand *cand)
{
  comp_cost cost_base;
  unsigned cost, cost_step;
  tree base;

  if (!cand->iv)
    {
      cand->cost = 0;
      return;
    }

  /* There are two costs associated with the candidate -- its increment
     and its initialization.  The second is almost negligible for any loop
     that rolls enough, so we take it just very little into account.  */

  base = cand->iv->base;
  cost_base = force_var_cost (data, base, NULL);
  /* It will be exceptional that the iv register happens to be initialized with
     the proper value at no cost.  In general, there will at least be a regcopy
     or a const set.  */
  if (cost_base.cost == 0)
    cost_base.cost = COSTS_N_INSNS (1);
  cost_step = add_cost (data->speed, TYPE_MODE (TREE_TYPE (base)));

  cost = cost_step + adjust_setup_cost (data, cost_base.cost);

  /* Prefer the original ivs unless we may gain something by replacing it.
     The reason is to make debugging simpler; so this is not relevant for
     artificial ivs created by other optimization passes.  */
  if (cand->pos != IP_ORIGINAL
      || !SSA_NAME_VAR (cand->var_before)
      || DECL_ARTIFICIAL (SSA_NAME_VAR (cand->var_before)))
    cost++;

  /* Prefer not to insert statements into latch unless there are some
     already (so that we do not create unnecessary jumps).  */
  if (cand->pos == IP_END
      && empty_block_p (ip_end_pos (data->current_loop)))
    cost++;

  cand->cost = cost;
  cand->cost_step = cost_step;
}

/* Determines costs of computation of the candidates.  */

static void
determine_iv_costs (struct ivopts_data *data)
{
  unsigned i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "<Candidate Costs>:\n");
      fprintf (dump_file, "  cand\tcost\n");
    }

  for (i = 0; i < data->vcands.length (); i++)
    {
      struct iv_cand *cand = data->vcands[i];

      determine_iv_cost (data, cand);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "  %d\t%d\n", i, cand->cost);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n");
}

/* Calculates cost for having SIZE induction variables.  */

static unsigned
ivopts_global_cost_for_size (struct ivopts_data *data, unsigned size)
{
  /* We add size to the cost, so that we prefer eliminating ivs
     if possible.  */
  return size + estimate_reg_pressure_cost (size, data->regs_used, data->speed,
					    data->body_includes_call);
}

/* For each size of the induction variable set determine the penalty.  */

static void
determine_set_costs (struct ivopts_data *data)
{
  unsigned j, n;
  gphi *phi;
  gphi_iterator psi;
  tree op;
  struct loop *loop = data->current_loop;
  bitmap_iterator bi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "<Global Costs>:\n");
      fprintf (dump_file, "  target_avail_regs %d\n", target_avail_regs);
      fprintf (dump_file, "  target_clobbered_regs %d\n", target_clobbered_regs);
      fprintf (dump_file, "  target_reg_cost %d\n", target_reg_cost[data->speed]);
      fprintf (dump_file, "  target_spill_cost %d\n", target_spill_cost[data->speed]);
    }

  n = 0;
  for (psi = gsi_start_phis (loop->header); !gsi_end_p (psi); gsi_next (&psi))
    {
      phi = psi.phi ();
      op = PHI_RESULT (phi);

      if (virtual_operand_p (op))
	continue;

      if (get_iv (data, op))
	continue;

      n++;
    }

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, j, bi)
    {
      struct version_info *info = ver_info (data, j);

      if (info->inv_id && info->has_nonlin_use)
	n++;
    }

  data->regs_used = n;
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "  regs_used %d\n", n);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  cost for size:\n");
      fprintf (dump_file, "  ivs\tcost\n");
      for (j = 0; j <= 2 * target_avail_regs; j++)
	fprintf (dump_file, "  %d\t%d\n", j,
		 ivopts_global_cost_for_size (data, j));
      fprintf (dump_file, "\n");
    }
}

/* Returns true if A is a cheaper cost pair than B.  */

static bool
cheaper_cost_pair (struct cost_pair *a, struct cost_pair *b)
{
  int cmp;

  if (!a)
    return false;

  if (!b)
    return true;

  cmp = compare_costs (a->cost, b->cost);
  if (cmp < 0)
    return true;

  if (cmp > 0)
    return false;

  /* In case the costs are the same, prefer the cheaper candidate.  */
  if (a->cand->cost < b->cand->cost)
    return true;

  return false;
}


/* Returns candidate by that USE is expressed in IVS.  */

static struct cost_pair *
iv_ca_cand_for_group (struct iv_ca *ivs, struct iv_group *group)
{
  return ivs->cand_for_group[group->id];
}

/* Computes the cost field of IVS structure.  */

static void
iv_ca_recount_cost (struct ivopts_data *data, struct iv_ca *ivs)
{
  comp_cost cost = ivs->cand_use_cost;

  cost.cost += ivs->cand_cost;

  cost.cost += ivopts_global_cost_for_size (data,
                                            ivs->n_regs + ivs->num_used_inv_expr);

  ivs->cost = cost;
}

/* Remove invariants in set INVS to set IVS.  */

static void
iv_ca_set_remove_invariants (struct iv_ca *ivs, bitmap invs)
{
  bitmap_iterator bi;
  unsigned iid;

  if (!invs)
    return;

  EXECUTE_IF_SET_IN_BITMAP (invs, 0, iid, bi)
    {
      ivs->n_invariant_uses[iid]--;
      if (ivs->n_invariant_uses[iid] == 0)
        ivs->n_regs--;
    }
}

/* Set USE not to be expressed by any candidate in IVS.  */

static void
iv_ca_set_no_cp (struct ivopts_data *data, struct iv_ca *ivs,
		 struct iv_group *group)
{
  unsigned gid = group->id, cid;
  struct cost_pair *cp;

  cp = ivs->cand_for_group[gid];
  if (!cp)
    return;
  cid = cp->cand->id;

  ivs->bad_groups++;
  ivs->cand_for_group[gid] = NULL;
  ivs->n_cand_uses[cid]--;

  if (ivs->n_cand_uses[cid] == 0)
    {
      bitmap_clear_bit (ivs->cands, cid);
      /* Do not count the pseudocandidates.  */
      if (cp->cand->iv)
	ivs->n_regs--;
      ivs->n_cands--;
      ivs->cand_cost -= cp->cand->cost;

      iv_ca_set_remove_invariants (ivs, cp->cand->depends_on);
    }

  ivs->cand_use_cost = sub_costs (ivs->cand_use_cost, cp->cost);

  iv_ca_set_remove_invariants (ivs, cp->depends_on);

  if (cp->inv_expr_id != -1)
    {
      ivs->used_inv_expr[cp->inv_expr_id]--;
      if (ivs->used_inv_expr[cp->inv_expr_id] == 0)
        ivs->num_used_inv_expr--;
    }
  iv_ca_recount_cost (data, ivs);
}

/* Add invariants in set INVS to set IVS.  */

static void
iv_ca_set_add_invariants (struct iv_ca *ivs, bitmap invs)
{
  bitmap_iterator bi;
  unsigned iid;

  if (!invs)
    return;

  EXECUTE_IF_SET_IN_BITMAP (invs, 0, iid, bi)
    {
      ivs->n_invariant_uses[iid]++;
      if (ivs->n_invariant_uses[iid] == 1)
        ivs->n_regs++;
    }
}

/* Set cost pair for GROUP in set IVS to CP.  */

static void
iv_ca_set_cp (struct ivopts_data *data, struct iv_ca *ivs,
	      struct iv_group *group, struct cost_pair *cp)
{
  unsigned gid = group->id, cid;

  if (ivs->cand_for_group[gid] == cp)
    return;

  if (ivs->cand_for_group[gid])
    iv_ca_set_no_cp (data, ivs, group);

  if (cp)
    {
      cid = cp->cand->id;

      ivs->bad_groups--;
      ivs->cand_for_group[gid] = cp;
      ivs->n_cand_uses[cid]++;
      if (ivs->n_cand_uses[cid] == 1)
	{
	  bitmap_set_bit (ivs->cands, cid);
	  /* Do not count the pseudocandidates.  */
	  if (cp->cand->iv)
	    ivs->n_regs++;
	  ivs->n_cands++;
	  ivs->cand_cost += cp->cand->cost;

	  iv_ca_set_add_invariants (ivs, cp->cand->depends_on);
	}

      ivs->cand_use_cost = add_costs (ivs->cand_use_cost, cp->cost);
      iv_ca_set_add_invariants (ivs, cp->depends_on);

      if (cp->inv_expr_id != -1)
        {
          ivs->used_inv_expr[cp->inv_expr_id]++;
          if (ivs->used_inv_expr[cp->inv_expr_id] == 1)
            ivs->num_used_inv_expr++;
        }
      iv_ca_recount_cost (data, ivs);
    }
}

/* Extend set IVS by expressing USE by some of the candidates in it
   if possible.  Consider all important candidates if candidates in
   set IVS don't give any result.  */

static void
iv_ca_add_group (struct ivopts_data *data, struct iv_ca *ivs,
	       struct iv_group *group)
{
  struct cost_pair *best_cp = NULL, *cp;
  bitmap_iterator bi;
  unsigned i;
  struct iv_cand *cand;

  gcc_assert (ivs->upto >= group->id);
  ivs->upto++;
  ivs->bad_groups++;

  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, i, bi)
    {
      cand = data->vcands[i];
      cp = get_group_iv_cost (data, group, cand);
      if (cheaper_cost_pair (cp, best_cp))
	best_cp = cp;
    }

  if (best_cp == NULL)
    {
      EXECUTE_IF_SET_IN_BITMAP (data->important_candidates, 0, i, bi)
	{
	  cand = data->vcands[i];
	  cp = get_group_iv_cost (data, group, cand);
	  if (cheaper_cost_pair (cp, best_cp))
	    best_cp = cp;
	}
    }

  iv_ca_set_cp (data, ivs, group, best_cp);
}

/* Get cost for assignment IVS.  */

static comp_cost
iv_ca_cost (struct iv_ca *ivs)
{
  /* This was a conditional expression but it triggered a bug in
     Sun C 5.5.  */
  if (ivs->bad_groups)
    return infinite_cost;
  else
    return ivs->cost;
}

/* Returns true if all dependences of CP are among invariants in IVS.  */

static bool
iv_ca_has_deps (struct iv_ca *ivs, struct cost_pair *cp)
{
  unsigned i;
  bitmap_iterator bi;

  if (!cp->depends_on)
    return true;

  EXECUTE_IF_SET_IN_BITMAP (cp->depends_on, 0, i, bi)
    {
      if (ivs->n_invariant_uses[i] == 0)
	return false;
    }

  return true;
}

/* Creates change of expressing GROUP by NEW_CP instead of OLD_CP and chains
   it before NEXT.  */

static struct iv_ca_delta *
iv_ca_delta_add (struct iv_group *group, struct cost_pair *old_cp,
		 struct cost_pair *new_cp, struct iv_ca_delta *next)
{
  struct iv_ca_delta *change = XNEW (struct iv_ca_delta);

  change->group = group;
  change->old_cp = old_cp;
  change->new_cp = new_cp;
  change->next = next;

  return change;
}

/* Joins two lists of changes L1 and L2.  Destructive -- old lists
   are rewritten.  */

static struct iv_ca_delta *
iv_ca_delta_join (struct iv_ca_delta *l1, struct iv_ca_delta *l2)
{
  struct iv_ca_delta *last;

  if (!l2)
    return l1;

  if (!l1)
    return l2;

  for (last = l1; last->next; last = last->next)
    continue;
  last->next = l2;

  return l1;
}

/* Reverse the list of changes DELTA, forming the inverse to it.  */

static struct iv_ca_delta *
iv_ca_delta_reverse (struct iv_ca_delta *delta)
{
  struct iv_ca_delta *act, *next, *prev = NULL;

  for (act = delta; act; act = next)
    {
      next = act->next;
      act->next = prev;
      prev = act;

      std::swap (act->old_cp, act->new_cp);
    }

  return prev;
}

/* Commit changes in DELTA to IVS.  If FORWARD is false, the changes are
   reverted instead.  */

static void
iv_ca_delta_commit (struct ivopts_data *data, struct iv_ca *ivs,
		    struct iv_ca_delta *delta, bool forward)
{
  struct cost_pair *from, *to;
  struct iv_ca_delta *act;

  if (!forward)
    delta = iv_ca_delta_reverse (delta);

  for (act = delta; act; act = act->next)
    {
      from = act->old_cp;
      to = act->new_cp;
      gcc_assert (iv_ca_cand_for_group (ivs, act->group) == from);
      iv_ca_set_cp (data, ivs, act->group, to);
    }

  if (!forward)
    iv_ca_delta_reverse (delta);
}

/* Returns true if CAND is used in IVS.  */

static bool
iv_ca_cand_used_p (struct iv_ca *ivs, struct iv_cand *cand)
{
  return ivs->n_cand_uses[cand->id] > 0;
}

/* Returns number of induction variable candidates in the set IVS.  */

static unsigned
iv_ca_n_cands (struct iv_ca *ivs)
{
  return ivs->n_cands;
}

/* Free the list of changes DELTA.  */

static void
iv_ca_delta_free (struct iv_ca_delta **delta)
{
  struct iv_ca_delta *act, *next;

  for (act = *delta; act; act = next)
    {
      next = act->next;
      free (act);
    }

  *delta = NULL;
}

/* Allocates new iv candidates assignment.  */

static struct iv_ca *
iv_ca_new (struct ivopts_data *data)
{
  struct iv_ca *nw = XNEW (struct iv_ca);

  nw->upto = 0;
  nw->bad_groups = 0;
  nw->cand_for_group = XCNEWVEC (struct cost_pair *,
				 data->vgroups.length ());
  nw->n_cand_uses = XCNEWVEC (unsigned, data->vcands.length ());
  nw->cands = BITMAP_ALLOC (NULL);
  nw->n_cands = 0;
  nw->n_regs = 0;
  nw->cand_use_cost = no_cost;
  nw->cand_cost = 0;
  nw->n_invariant_uses = XCNEWVEC (unsigned, data->max_inv_id + 1);
  nw->cost = no_cost;
  nw->used_inv_expr = XCNEWVEC (unsigned, data->inv_expr_id + 1);
  nw->num_used_inv_expr = 0;

  return nw;
}

/* Free memory occupied by the set IVS.  */

static void
iv_ca_free (struct iv_ca **ivs)
{
  free ((*ivs)->cand_for_group);
  free ((*ivs)->n_cand_uses);
  BITMAP_FREE ((*ivs)->cands);
  free ((*ivs)->n_invariant_uses);
  free ((*ivs)->used_inv_expr);
  free (*ivs);
  *ivs = NULL;
}

/* Dumps IVS to FILE.  */

static void
iv_ca_dump (struct ivopts_data *data, FILE *file, struct iv_ca *ivs)
{
  const char *pref = "  invariants ";
  unsigned i;
  comp_cost cost = iv_ca_cost (ivs);

  fprintf (file, "  cost: %d (complexity %d)\n", cost.cost, cost.complexity);
  fprintf (file, "  cand_cost: %d\n  cand_group_cost: %d (complexity %d)\n",
           ivs->cand_cost, ivs->cand_use_cost.cost, ivs->cand_use_cost.complexity);
  bitmap_print (file, ivs->cands, "  candidates: ","\n");

  for (i = 0; i < ivs->upto; i++)
    {
      struct iv_group *group = data->vgroups[i];
      struct cost_pair *cp = iv_ca_cand_for_group (ivs, group);
      if (cp)
	fprintf (file, "   group:%d --> iv_cand:%d, cost=(%d,%d)\n",
		 group->id, cp->cand->id, cp->cost.cost, cp->cost.complexity);
      else
	fprintf (file, "   group:%d --> ??\n", group->id);
    }

  for (i = 1; i <= data->max_inv_id; i++)
    if (ivs->n_invariant_uses[i])
      {
	fprintf (file, "%s%d", pref, i);
	pref = ", ";
      }
  fprintf (file, "\n\n");
}

/* Try changing candidate in IVS to CAND for each use.  Return cost of the
   new set, and store differences in DELTA.  Number of induction variables
   in the new set is stored to N_IVS. MIN_NCAND is a flag. When it is true
   the function will try to find a solution with mimimal iv candidates.  */

static comp_cost
iv_ca_extend (struct ivopts_data *data, struct iv_ca *ivs,
	      struct iv_cand *cand, struct iv_ca_delta **delta,
	      unsigned *n_ivs, bool min_ncand)
{
  unsigned i;
  comp_cost cost;
  struct iv_group *group;
  struct cost_pair *old_cp, *new_cp;

  *delta = NULL;
  for (i = 0; i < ivs->upto; i++)
    {
      group = data->vgroups[i];
      old_cp = iv_ca_cand_for_group (ivs, group);

      if (old_cp
	  && old_cp->cand == cand)
	continue;

      new_cp = get_group_iv_cost (data, group, cand);
      if (!new_cp)
	continue;

      if (!min_ncand && !iv_ca_has_deps (ivs, new_cp))
	continue;

      if (!min_ncand && !cheaper_cost_pair (new_cp, old_cp))
        continue;

      *delta = iv_ca_delta_add (group, old_cp, new_cp, *delta);
    }

  iv_ca_delta_commit (data, ivs, *delta, true);
  cost = iv_ca_cost (ivs);
  if (n_ivs)
    *n_ivs = iv_ca_n_cands (ivs);
  iv_ca_delta_commit (data, ivs, *delta, false);

  return cost;
}

/* Try narrowing set IVS by removing CAND.  Return the cost of
   the new set and store the differences in DELTA.  START is
   the candidate with which we start narrowing.  */

static comp_cost
iv_ca_narrow (struct ivopts_data *data, struct iv_ca *ivs,
	      struct iv_cand *cand, struct iv_cand *start,
	      struct iv_ca_delta **delta)
{
  unsigned i, ci;
  struct iv_group *group;
  struct cost_pair *old_cp, *new_cp, *cp;
  bitmap_iterator bi;
  struct iv_cand *cnd;
  comp_cost cost, best_cost, acost;

  *delta = NULL;
  for (i = 0; i < data->vgroups.length (); i++)
    {
      group = data->vgroups[i];

      old_cp = iv_ca_cand_for_group (ivs, group);
      if (old_cp->cand != cand)
	continue;

      best_cost = iv_ca_cost (ivs);
      /* Start narrowing with START.  */
      new_cp = get_group_iv_cost (data, group, start);

      if (data->consider_all_candidates)
	{
	  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, ci, bi)
	    {
	      if (ci == cand->id || (start && ci == start->id))
		continue;

	      cnd = data->vcands[ci];

	      cp = get_group_iv_cost (data, group, cnd);
	      if (!cp)
		continue;

	      iv_ca_set_cp (data, ivs, group, cp);
	      acost = iv_ca_cost (ivs);

	      if (compare_costs (acost, best_cost) < 0)
		{
		  best_cost = acost;
		  new_cp = cp;
		}
	    }
	}
      else
	{
	  EXECUTE_IF_AND_IN_BITMAP (group->related_cands, ivs->cands, 0, ci, bi)
	    {
	      if (ci == cand->id || (start && ci == start->id))
		continue;

	      cnd = data->vcands[ci];

	      cp = get_group_iv_cost (data, group, cnd);
	      if (!cp)
		continue;

	      iv_ca_set_cp (data, ivs, group, cp);
	      acost = iv_ca_cost (ivs);

	      if (compare_costs (acost, best_cost) < 0)
		{
		  best_cost = acost;
		  new_cp = cp;
		}
	    }
	}
      /* Restore to old cp for use.  */
      iv_ca_set_cp (data, ivs, group, old_cp);

      if (!new_cp)
	{
	  iv_ca_delta_free (delta);
	  return infinite_cost;
	}

      *delta = iv_ca_delta_add (group, old_cp, new_cp, *delta);
    }

  iv_ca_delta_commit (data, ivs, *delta, true);
  cost = iv_ca_cost (ivs);
  iv_ca_delta_commit (data, ivs, *delta, false);

  return cost;
}

/* Try optimizing the set of candidates IVS by removing candidates different
   from to EXCEPT_CAND from it.  Return cost of the new set, and store
   differences in DELTA.  */

static comp_cost
iv_ca_prune (struct ivopts_data *data, struct iv_ca *ivs,
	     struct iv_cand *except_cand, struct iv_ca_delta **delta)
{
  bitmap_iterator bi;
  struct iv_ca_delta *act_delta, *best_delta;
  unsigned i;
  comp_cost best_cost, acost;
  struct iv_cand *cand;

  best_delta = NULL;
  best_cost = iv_ca_cost (ivs);

  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, i, bi)
    {
      cand = data->vcands[i];

      if (cand == except_cand)
	continue;

      acost = iv_ca_narrow (data, ivs, cand, except_cand, &act_delta);

      if (compare_costs (acost, best_cost) < 0)
	{
	  best_cost = acost;
	  iv_ca_delta_free (&best_delta);
	  best_delta = act_delta;
	}
      else
	iv_ca_delta_free (&act_delta);
    }

  if (!best_delta)
    {
      *delta = NULL;
      return best_cost;
    }

  /* Recurse to possibly remove other unnecessary ivs.  */
  iv_ca_delta_commit (data, ivs, best_delta, true);
  best_cost = iv_ca_prune (data, ivs, except_cand, delta);
  iv_ca_delta_commit (data, ivs, best_delta, false);
  *delta = iv_ca_delta_join (best_delta, *delta);
  return best_cost;
}

/* Check if CAND_IDX is a candidate other than OLD_CAND and has
   cheaper local cost for GROUP than BEST_CP.  Return pointer to
   the corresponding cost_pair, otherwise just return BEST_CP.  */

static struct cost_pair*
cheaper_cost_with_cand (struct ivopts_data *data, struct iv_group *group,
			unsigned int cand_idx, struct iv_cand *old_cand,
			struct cost_pair *best_cp)
{
  struct iv_cand *cand;
  struct cost_pair *cp;

  gcc_assert (old_cand != NULL && best_cp != NULL);
  if (cand_idx == old_cand->id)
    return best_cp;

  cand = data->vcands[cand_idx];
  cp = get_group_iv_cost (data, group, cand);
  if (cp != NULL && cheaper_cost_pair (cp, best_cp))
    return cp;

  return best_cp;
}

/* Try breaking local optimal fixed-point for IVS by replacing candidates
   which are used by more than one iv uses.  For each of those candidates,
   this function tries to represent iv uses under that candidate using
   other ones with lower local cost, then tries to prune the new set.
   If the new set has lower cost, It returns the new cost after recording
   candidate replacement in list DELTA.  */

static comp_cost
iv_ca_replace (struct ivopts_data *data, struct iv_ca *ivs,
	       struct iv_ca_delta **delta)
{
  bitmap_iterator bi, bj;
  unsigned int i, j, k;
  struct iv_cand *cand;
  comp_cost orig_cost, acost;
  struct iv_ca_delta *act_delta, *tmp_delta;
  struct cost_pair *old_cp, *best_cp = NULL;

  *delta = NULL;
  orig_cost = iv_ca_cost (ivs);

  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, i, bi)
    {
      if (ivs->n_cand_uses[i] == 1
	  || ivs->n_cand_uses[i] > ALWAYS_PRUNE_CAND_SET_BOUND)
	continue;

      cand = data->vcands[i];

      act_delta = NULL;
      /*  Represent uses under current candidate using other ones with
	  lower local cost.  */
      for (j = 0; j < ivs->upto; j++)
	{
	  struct iv_group *group = data->vgroups[j];
	  old_cp = iv_ca_cand_for_group (ivs, group);

	  if (old_cp->cand != cand)
	    continue;

	  best_cp = old_cp;
	  if (data->consider_all_candidates)
	    for (k = 0; k < data->vcands.length (); k++)
	      best_cp = cheaper_cost_with_cand (data, group, k,
						old_cp->cand, best_cp);
	  else
	    EXECUTE_IF_SET_IN_BITMAP (group->related_cands, 0, k, bj)
	      best_cp = cheaper_cost_with_cand (data, group, k,
						old_cp->cand, best_cp);

	  if (best_cp == old_cp)
	    continue;

	  act_delta = iv_ca_delta_add (group, old_cp, best_cp, act_delta);
	}
      /* No need for further prune.  */
      if (!act_delta)
	continue;

      /* Prune the new candidate set.  */
      iv_ca_delta_commit (data, ivs, act_delta, true);
      acost = iv_ca_prune (data, ivs, NULL, &tmp_delta);
      iv_ca_delta_commit (data, ivs, act_delta, false);
      act_delta = iv_ca_delta_join (act_delta, tmp_delta);

      if (compare_costs (acost, orig_cost) < 0)
	{
	  *delta = act_delta;
	  return acost;
	}
      else
	iv_ca_delta_free (&act_delta);
    }

  return orig_cost;
}

/* Tries to extend the sets IVS in the best possible way in order to
   express the GROUP.  If ORIGINALP is true, prefer candidates from
   the original set of IVs, otherwise favor important candidates not
   based on any memory object.  */

static bool
try_add_cand_for (struct ivopts_data *data, struct iv_ca *ivs,
		  struct iv_group *group, bool originalp)
{
  comp_cost best_cost, act_cost;
  unsigned i;
  bitmap_iterator bi;
  struct iv_cand *cand;
  struct iv_ca_delta *best_delta = NULL, *act_delta;
  struct cost_pair *cp;

  iv_ca_add_group (data, ivs, group);
  best_cost = iv_ca_cost (ivs);
  cp = iv_ca_cand_for_group (ivs, group);
  if (cp)
    {
      best_delta = iv_ca_delta_add (group, NULL, cp, NULL);
      iv_ca_set_no_cp (data, ivs, group);
    }

  /* If ORIGINALP is true, try to find the original IV for the use.  Otherwise
     first try important candidates not based on any memory object.  Only if
     this fails, try the specific ones.  Rationale -- in loops with many
     variables the best choice often is to use just one generic biv.  If we
     added here many ivs specific to the uses, the optimization algorithm later
     would be likely to get stuck in a local minimum, thus causing us to create
     too many ivs.  The approach from few ivs to more seems more likely to be
     successful -- starting from few ivs, replacing an expensive use by a
     specific iv should always be a win.  */
  EXECUTE_IF_SET_IN_BITMAP (group->related_cands, 0, i, bi)
    {
      cand = data->vcands[i];

      if (originalp && cand->pos !=IP_ORIGINAL)
	continue;

      if (!originalp && cand->iv->base_object != NULL_TREE)
	continue;

      if (iv_ca_cand_used_p (ivs, cand))
        continue;

      cp = get_group_iv_cost (data, group, cand);
      if (!cp)
	continue;

      iv_ca_set_cp (data, ivs, group, cp);
      act_cost = iv_ca_extend (data, ivs, cand, &act_delta, NULL,
                               true);
      iv_ca_set_no_cp (data, ivs, group);
      act_delta = iv_ca_delta_add (group, NULL, cp, act_delta);

      if (compare_costs (act_cost, best_cost) < 0)
	{
	  best_cost = act_cost;

	  iv_ca_delta_free (&best_delta);
	  best_delta = act_delta;
	}
      else
	iv_ca_delta_free (&act_delta);
    }

  if (infinite_cost_p (best_cost))
    {
      for (i = 0; i < group->n_map_members; i++)
	{
	  cp = group->cost_map + i;
	  cand = cp->cand;
	  if (!cand)
	    continue;

	  /* Already tried this.  */
	  if (cand->important)
	    {
	      if (originalp && cand->pos == IP_ORIGINAL)
		continue;
	      if (!originalp && cand->iv->base_object == NULL_TREE)
		continue;
	    }

	  if (iv_ca_cand_used_p (ivs, cand))
	    continue;

	  act_delta = NULL;
	  iv_ca_set_cp (data, ivs, group, cp);
	  act_cost = iv_ca_extend (data, ivs, cand, &act_delta, NULL, true);
	  iv_ca_set_no_cp (data, ivs, group);
	  act_delta = iv_ca_delta_add (group,
				       iv_ca_cand_for_group (ivs, group),
				       cp, act_delta);

	  if (compare_costs (act_cost, best_cost) < 0)
	    {
	      best_cost = act_cost;

	      if (best_delta)
		iv_ca_delta_free (&best_delta);
	      best_delta = act_delta;
	    }
	  else
	    iv_ca_delta_free (&act_delta);
	}
    }

  iv_ca_delta_commit (data, ivs, best_delta, true);
  iv_ca_delta_free (&best_delta);

  return !infinite_cost_p (best_cost);
}

/* Finds an initial assignment of candidates to uses.  */

static struct iv_ca *
get_initial_solution (struct ivopts_data *data, bool originalp)
{
  unsigned i;
  struct iv_ca *ivs = iv_ca_new (data);

  for (i = 0; i < data->vgroups.length (); i++)
    if (!try_add_cand_for (data, ivs, data->vgroups[i], originalp))
      {
	iv_ca_free (&ivs);
	return NULL;
      }

  return ivs;
}

/* Tries to improve set of induction variables IVS.  TRY_REPLACE_P
   points to a bool variable, this function tries to break local
   optimal fixed-point by replacing candidates in IVS if it's true.  */

static bool
try_improve_iv_set (struct ivopts_data *data,
		    struct iv_ca *ivs, bool *try_replace_p)
{
  unsigned i, n_ivs;
  comp_cost acost, best_cost = iv_ca_cost (ivs);
  struct iv_ca_delta *best_delta = NULL, *act_delta, *tmp_delta;
  struct iv_cand *cand;

  /* Try extending the set of induction variables by one.  */
  for (i = 0; i < data->vcands.length (); i++)
    {
      cand = data->vcands[i];

      if (iv_ca_cand_used_p (ivs, cand))
	continue;

      acost = iv_ca_extend (data, ivs, cand, &act_delta, &n_ivs, false);
      if (!act_delta)
	continue;

      /* If we successfully added the candidate and the set is small enough,
	 try optimizing it by removing other candidates.  */
      if (n_ivs <= ALWAYS_PRUNE_CAND_SET_BOUND)
      	{
	  iv_ca_delta_commit (data, ivs, act_delta, true);
	  acost = iv_ca_prune (data, ivs, cand, &tmp_delta);
	  iv_ca_delta_commit (data, ivs, act_delta, false);
	  act_delta = iv_ca_delta_join (act_delta, tmp_delta);
	}

      if (compare_costs (acost, best_cost) < 0)
	{
	  best_cost = acost;
	  iv_ca_delta_free (&best_delta);
	  best_delta = act_delta;
	}
      else
	iv_ca_delta_free (&act_delta);
    }

  if (!best_delta)
    {
      /* Try removing the candidates from the set instead.  */
      best_cost = iv_ca_prune (data, ivs, NULL, &best_delta);

      if (!best_delta && *try_replace_p)
	{
	  *try_replace_p = false;
	  /* So far candidate selecting algorithm tends to choose fewer IVs
	     so that it can handle cases in which loops have many variables
	     but the best choice is often to use only one general biv.  One
	     weakness is it can't handle opposite cases, in which different
	     candidates should be chosen with respect to each use.  To solve
	     the problem, we replace candidates in a manner described by the
	     comments of iv_ca_replace, thus give general algorithm a chance
	     to break local optimal fixed-point in these cases.  */
	  best_cost = iv_ca_replace (data, ivs, &best_delta);
	}

      if (!best_delta)
	return false;
    }

  iv_ca_delta_commit (data, ivs, best_delta, true);
  gcc_assert (compare_costs (best_cost, iv_ca_cost (ivs)) == 0);
  iv_ca_delta_free (&best_delta);
  return true;
}

/* Attempts to find the optimal set of induction variables.  We do simple
   greedy heuristic -- we try to replace at most one candidate in the selected
   solution and remove the unused ivs while this improves the cost.  */

static struct iv_ca *
find_optimal_iv_set_1 (struct ivopts_data *data, bool originalp)
{
  struct iv_ca *set;
  bool try_replace_p = true;

  /* Get the initial solution.  */
  set = get_initial_solution (data, originalp);
  if (!set)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Unable to substitute for ivs, failed.\n");
      return NULL;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Initial set of candidates:\n");
      iv_ca_dump (data, dump_file, set);
    }

  while (try_improve_iv_set (data, set, &try_replace_p))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Improved to:\n");
	  iv_ca_dump (data, dump_file, set);
	}
    }

  return set;
}

static struct iv_ca *
find_optimal_iv_set (struct ivopts_data *data)
{
  unsigned i;
  comp_cost cost, origcost;
  struct iv_ca *set, *origset;

  /* Determine the cost based on a strategy that starts with original IVs,
     and try again using a strategy that prefers candidates not based
     on any IVs.  */
  origset = find_optimal_iv_set_1 (data, true);
  set = find_optimal_iv_set_1 (data, false);

  if (!origset && !set)
    return NULL;

  origcost = origset ? iv_ca_cost (origset) : infinite_cost;
  cost = set ? iv_ca_cost (set) : infinite_cost;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Original cost %d (complexity %d)\n\n",
	       origcost.cost, origcost.complexity);
      fprintf (dump_file, "Final cost %d (complexity %d)\n\n",
	       cost.cost, cost.complexity);
    }

  /* Choose the one with the best cost.  */
  if (compare_costs (origcost, cost) <= 0)
    {
      if (set)
	iv_ca_free (&set);
      set = origset;
    }
  else if (origset)
    iv_ca_free (&origset);

  for (i = 0; i < data->vgroups.length (); i++)
    {
      struct iv_group *group = data->vgroups[i];
      group->selected = iv_ca_cand_for_group (set, group)->cand;
    }

  return set;
}

/* Creates a new induction variable corresponding to CAND.  */

static void
create_new_iv (struct ivopts_data *data, struct iv_cand *cand)
{
  gimple_stmt_iterator incr_pos;
  tree base;
  struct iv_use *use;
  struct iv_group *group;
  bool after = false;

  if (!cand->iv)
    return;

  switch (cand->pos)
    {
    case IP_NORMAL:
      incr_pos = gsi_last_bb (ip_normal_pos (data->current_loop));
      break;

    case IP_END:
      incr_pos = gsi_last_bb (ip_end_pos (data->current_loop));
      after = true;
      break;

    case IP_AFTER_USE:
      after = true;
      /* fall through */
    case IP_BEFORE_USE:
      incr_pos = gsi_for_stmt (cand->incremented_at);
      break;

    case IP_ORIGINAL:
      /* Mark that the iv is preserved.  */
      name_info (data, cand->var_before)->preserve_biv = true;
      name_info (data, cand->var_after)->preserve_biv = true;

      /* Rewrite the increment so that it uses var_before directly.  */
      use = find_interesting_uses_op (data, cand->var_after);
      group = data->vgroups[use->group_id];
      group->selected = cand;
      return;
    }

  gimple_add_tmp_var (cand->var_before);

  base = unshare_expr (cand->iv->base);

  create_iv (base, unshare_expr (cand->iv->step),
	     cand->var_before, data->current_loop,
	     &incr_pos, after, &cand->var_before, &cand->var_after);
}

/* Creates new induction variables described in SET.  */

static void
create_new_ivs (struct ivopts_data *data, struct iv_ca *set)
{
  unsigned i;
  struct iv_cand *cand;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (set->cands, 0, i, bi)
    {
      cand = data->vcands[i];
      create_new_iv (data, cand);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Selected IV set for loop %d",
	       data->current_loop->num);
      if (data->loop_loc != UNKNOWN_LOCATION)
	fprintf (dump_file, " at %s:%d", LOCATION_FILE (data->loop_loc),
		 LOCATION_LINE (data->loop_loc));
      fprintf (dump_file, ", %lu IVs:\n", bitmap_count_bits (set->cands));
      EXECUTE_IF_SET_IN_BITMAP (set->cands, 0, i, bi)
        {
          cand = data->vcands[i];
          dump_cand (dump_file, cand);
        }
      fprintf (dump_file, "\n");
    }
}

/* Rewrites USE (definition of iv used in a nonlinear expression)
   using candidate CAND.  */

static void
rewrite_use_nonlinear_expr (struct ivopts_data *data,
			    struct iv_use *use, struct iv_cand *cand)
{
  tree comp;
  tree op, tgt;
  gassign *ass;
  gimple_stmt_iterator bsi;

  /* An important special case -- if we are asked to express value of
     the original iv by itself, just exit; there is no need to
     introduce a new computation (that might also need casting the
     variable to unsigned and back).  */
  if (cand->pos == IP_ORIGINAL
      && cand->incremented_at == use->stmt)
    {
      enum tree_code stmt_code;

      gcc_assert (is_gimple_assign (use->stmt));
      gcc_assert (gimple_assign_lhs (use->stmt) == cand->var_after);

      /* Check whether we may leave the computation unchanged.
	 This is the case only if it does not rely on other
	 computations in the loop -- otherwise, the computation
	 we rely upon may be removed in remove_unused_ivs,
	 thus leading to ICE.  */
      stmt_code = gimple_assign_rhs_code (use->stmt);
      if (stmt_code == PLUS_EXPR
	  || stmt_code == MINUS_EXPR
	  || stmt_code == POINTER_PLUS_EXPR)
	{
	  if (gimple_assign_rhs1 (use->stmt) == cand->var_before)
	    op = gimple_assign_rhs2 (use->stmt);
	  else if (gimple_assign_rhs2 (use->stmt) == cand->var_before)
	    op = gimple_assign_rhs1 (use->stmt);
	  else
	    op = NULL_TREE;
	}
      else
	op = NULL_TREE;

      if (op && expr_invariant_in_loop_p (data->current_loop, op))
	return;
    }

  comp = get_computation (data->current_loop, use, cand);
  gcc_assert (comp != NULL_TREE);

  switch (gimple_code (use->stmt))
    {
    case GIMPLE_PHI:
      tgt = PHI_RESULT (use->stmt);

      /* If we should keep the biv, do not replace it.  */
      if (name_info (data, tgt)->preserve_biv)
	return;

      bsi = gsi_after_labels (gimple_bb (use->stmt));
      break;

    case GIMPLE_ASSIGN:
      tgt = gimple_assign_lhs (use->stmt);
      bsi = gsi_for_stmt (use->stmt);
      break;

    default:
      gcc_unreachable ();
    }

  if (!valid_gimple_rhs_p (comp)
      || (gimple_code (use->stmt) != GIMPLE_PHI
	  /* We can't allow re-allocating the stmt as it might be pointed
	     to still.  */
	  && (get_gimple_rhs_num_ops (TREE_CODE (comp))
	      >= gimple_num_ops (gsi_stmt (bsi)))))
    {
      comp = force_gimple_operand_gsi (&bsi, comp, true, NULL_TREE,
				       true, GSI_SAME_STMT);
      if (POINTER_TYPE_P (TREE_TYPE (tgt)))
	{
	  duplicate_ssa_name_ptr_info (comp, SSA_NAME_PTR_INFO (tgt));
	  /* As this isn't a plain copy we have to reset alignment
	     information.  */
	  if (SSA_NAME_PTR_INFO (comp))
	    mark_ptr_info_alignment_unknown (SSA_NAME_PTR_INFO (comp));
	}
    }

  if (gimple_code (use->stmt) == GIMPLE_PHI)
    {
      ass = gimple_build_assign (tgt, comp);
      gsi_insert_before (&bsi, ass, GSI_SAME_STMT);

      bsi = gsi_for_stmt (use->stmt);
      remove_phi_node (&bsi, false);
    }
  else
    {
      gimple_assign_set_rhs_from_tree (&bsi, comp);
      use->stmt = gsi_stmt (bsi);
    }
}

/* Performs a peephole optimization to reorder the iv update statement with
   a mem ref to enable instruction combining in later phases. The mem ref uses
   the iv value before the update, so the reordering transformation requires
   adjustment of the offset. CAND is the selected IV_CAND.

   Example:

   t = MEM_REF (base, iv1, 8, 16);  // base, index, stride, offset
   iv2 = iv1 + 1;

   if (t < val)      (1)
     goto L;
   goto Head;


   directly propagating t over to (1) will introduce overlapping live range
   thus increase register pressure. This peephole transform it into:


   iv2 = iv1 + 1;
   t = MEM_REF (base, iv2, 8, 8);
   if (t < val)
     goto L;
   goto Head;
*/

static void
adjust_iv_update_pos (struct iv_cand *cand, struct iv_use *use)
{
  tree var_after;
  gimple *iv_update, *stmt;
  basic_block bb;
  gimple_stmt_iterator gsi, gsi_iv;

  if (cand->pos != IP_NORMAL)
    return;

  var_after = cand->var_after;
  iv_update = SSA_NAME_DEF_STMT (var_after);

  bb = gimple_bb (iv_update);
  gsi = gsi_last_nondebug_bb (bb);
  stmt = gsi_stmt (gsi);

  /* Only handle conditional statement for now.  */
  if (gimple_code (stmt) != GIMPLE_COND)
    return;

  gsi_prev_nondebug (&gsi);
  stmt = gsi_stmt (gsi);
  if (stmt != iv_update)
    return;

  gsi_prev_nondebug (&gsi);
  if (gsi_end_p (gsi))
    return;

  stmt = gsi_stmt (gsi);
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  if (stmt != use->stmt)
    return;

  if (TREE_CODE (gimple_assign_lhs (stmt)) != SSA_NAME)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Reordering \n");
      print_gimple_stmt (dump_file, iv_update, 0, 0);
      print_gimple_stmt (dump_file, use->stmt, 0, 0);
      fprintf (dump_file, "\n");
    }

  gsi = gsi_for_stmt (use->stmt);
  gsi_iv = gsi_for_stmt (iv_update);
  gsi_move_before (&gsi_iv, &gsi);

  cand->pos = IP_BEFORE_USE;
  cand->incremented_at = use->stmt;
}

/* Rewrites USE (address that is an iv) using candidate CAND.  */

static void
rewrite_use_address (struct ivopts_data *data,
		     struct iv_use *use, struct iv_cand *cand)
{
  aff_tree aff;
  gimple_stmt_iterator bsi = gsi_for_stmt (use->stmt);
  tree base_hint = NULL_TREE;
  tree ref, iv;
  bool ok;

  adjust_iv_update_pos (cand, use);
  ok = get_computation_aff (data->current_loop, use, cand, use->stmt, &aff);
  gcc_assert (ok);
  unshare_aff_combination (&aff);

  /* To avoid undefined overflow problems, all IV candidates use unsigned
     integer types.  The drawback is that this makes it impossible for
     create_mem_ref to distinguish an IV that is based on a memory object
     from one that represents simply an offset.

     To work around this problem, we pass a hint to create_mem_ref that
     indicates which variable (if any) in aff is an IV based on a memory
     object.  Note that we only consider the candidate.  If this is not
     based on an object, the base of the reference is in some subexpression
     of the use -- but these will use pointer types, so they are recognized
     by the create_mem_ref heuristics anyway.  */
  if (cand->iv->base_object)
    base_hint = var_at_stmt (data->current_loop, cand, use->stmt);

  iv = var_at_stmt (data->current_loop, cand, use->stmt);
  ref = create_mem_ref (&bsi, TREE_TYPE (*use->op_p), &aff,
			reference_alias_ptr_type (*use->op_p),
			iv, base_hint, data->speed);
  copy_ref_info (ref, *use->op_p);
  *use->op_p = ref;
}

/* Rewrites USE (the condition such that one of the arguments is an iv) using
   candidate CAND.  */

static void
rewrite_use_compare (struct ivopts_data *data,
		     struct iv_use *use, struct iv_cand *cand)
{
  tree comp, *var_p, op, bound;
  gimple_stmt_iterator bsi = gsi_for_stmt (use->stmt);
  enum tree_code compare;
  struct iv_group *group = data->vgroups[use->group_id];
  struct cost_pair *cp = get_group_iv_cost (data, group, cand);
  bool ok;

  bound = cp->value;
  if (bound)
    {
      tree var = var_at_stmt (data->current_loop, cand, use->stmt);
      tree var_type = TREE_TYPE (var);
      gimple_seq stmts;

      if (dump_file && (dump_flags & TDF_DETAILS))
        {
          fprintf (dump_file, "Replacing exit test: ");
          print_gimple_stmt (dump_file, use->stmt, 0, TDF_SLIM);
        }
      compare = cp->comp;
      bound = unshare_expr (fold_convert (var_type, bound));
      op = force_gimple_operand (bound, &stmts, true, NULL_TREE);
      if (stmts)
	gsi_insert_seq_on_edge_immediate (
		loop_preheader_edge (data->current_loop),
		stmts);

      gcond *cond_stmt = as_a <gcond *> (use->stmt);
      gimple_cond_set_lhs (cond_stmt, var);
      gimple_cond_set_code (cond_stmt, compare);
      gimple_cond_set_rhs (cond_stmt, op);
      return;
    }

  /* The induction variable elimination failed; just express the original
     giv.  */
  comp = get_computation (data->current_loop, use, cand);
  gcc_assert (comp != NULL_TREE);

  ok = extract_cond_operands (data, use->stmt, &var_p, NULL, NULL, NULL);
  gcc_assert (ok);

  *var_p = force_gimple_operand_gsi (&bsi, comp, true, SSA_NAME_VAR (*var_p),
				     true, GSI_SAME_STMT);
}

/* Rewrite the groups using the selected induction variables.  */

static void
rewrite_groups (struct ivopts_data *data)
{
  unsigned i, j;

  for (i = 0; i < data->vgroups.length (); i++)
    {
      struct iv_group *group = data->vgroups[i];
      struct iv_cand *cand = group->selected;

      gcc_assert (cand);

      if (group->type == USE_NONLINEAR_EXPR)
	{
	  for (j = 0; j < group->vuses.length (); j++)
	    {
	      rewrite_use_nonlinear_expr (data, group->vuses[j], cand);
	      update_stmt (group->vuses[j]->stmt);
	    }
	}
      else if (group->type == USE_ADDRESS)
	{
	  for (j = 0; j < group->vuses.length (); j++)
	    {
	      rewrite_use_address (data, group->vuses[j], cand);
	      update_stmt (group->vuses[j]->stmt);
	    }
	}
      else
	{
	  gcc_assert (group->type == USE_COMPARE);

	  for (j = 0; j < group->vuses.length (); j++)
	    {
	      rewrite_use_compare (data, group->vuses[j], cand);
	      update_stmt (group->vuses[j]->stmt);
	    }
	}
    }
}

/* Removes the ivs that are not used after rewriting.  */

static void
remove_unused_ivs (struct ivopts_data *data)
{
  unsigned j;
  bitmap_iterator bi;
  bitmap toremove = BITMAP_ALLOC (NULL);

  /* Figure out an order in which to release SSA DEFs so that we don't
     release something that we'd have to propagate into a debug stmt
     afterwards.  */
  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, j, bi)
    {
      struct version_info *info;

      info = ver_info (data, j);
      if (info->iv
	  && !integer_zerop (info->iv->step)
	  && !info->inv_id
	  && !info->iv->nonlin_use
	  && !info->preserve_biv)
	{
	  bitmap_set_bit (toremove, SSA_NAME_VERSION (info->iv->ssa_name));

	  tree def = info->iv->ssa_name;

	  if (MAY_HAVE_DEBUG_STMTS && SSA_NAME_DEF_STMT (def))
	    {
	      imm_use_iterator imm_iter;
	      use_operand_p use_p;
	      gimple *stmt;
	      int count = 0;

	      FOR_EACH_IMM_USE_STMT (stmt, imm_iter, def)
		{
		  if (!gimple_debug_bind_p (stmt))
		    continue;

		  /* We just want to determine whether to do nothing
		     (count == 0), to substitute the computed
		     expression into a single use of the SSA DEF by
		     itself (count == 1), or to use a debug temp
		     because the SSA DEF is used multiple times or as
		     part of a larger expression (count > 1). */
		  count++;
		  if (gimple_debug_bind_get_value (stmt) != def)
		    count++;

		  if (count > 1)
		    BREAK_FROM_IMM_USE_STMT (imm_iter);
		}

	      if (!count)
		continue;

	      struct iv_use dummy_use;
	      struct iv_cand *best_cand = NULL, *cand;
	      unsigned i, best_pref = 0, cand_pref;

	      memset (&dummy_use, 0, sizeof (dummy_use));
	      dummy_use.iv = info->iv;
	      for (i = 0; i < data->vgroups.length () && i < 64; i++)
		{
		  cand = data->vgroups[i]->selected;
		  if (cand == best_cand)
		    continue;
		  cand_pref = operand_equal_p (cand->iv->step,
					       info->iv->step, 0)
		    ? 4 : 0;
		  cand_pref
		    += TYPE_MODE (TREE_TYPE (cand->iv->base))
		    == TYPE_MODE (TREE_TYPE (info->iv->base))
		    ? 2 : 0;
		  cand_pref
		    += TREE_CODE (cand->iv->base) == INTEGER_CST
		    ? 1 : 0;
		  if (best_cand == NULL || best_pref < cand_pref)
		    {
		      best_cand = cand;
		      best_pref = cand_pref;
		    }
		}

	      if (!best_cand)
		continue;

	      tree comp = get_computation_at (data->current_loop,
					      &dummy_use, best_cand,
					      SSA_NAME_DEF_STMT (def));
	      if (!comp)
		continue;

	      if (count > 1)
		{
		  tree vexpr = make_node (DEBUG_EXPR_DECL);
		  DECL_ARTIFICIAL (vexpr) = 1;
		  TREE_TYPE (vexpr) = TREE_TYPE (comp);
		  if (SSA_NAME_VAR (def))
		    DECL_MODE (vexpr) = DECL_MODE (SSA_NAME_VAR (def));
		  else
		    DECL_MODE (vexpr) = TYPE_MODE (TREE_TYPE (vexpr));
		  gdebug *def_temp
		    = gimple_build_debug_bind (vexpr, comp, NULL);
		  gimple_stmt_iterator gsi;

		  if (gimple_code (SSA_NAME_DEF_STMT (def)) == GIMPLE_PHI)
		    gsi = gsi_after_labels (gimple_bb
					    (SSA_NAME_DEF_STMT (def)));
		  else
		    gsi = gsi_for_stmt (SSA_NAME_DEF_STMT (def));

		  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
		  comp = vexpr;
		}

	      FOR_EACH_IMM_USE_STMT (stmt, imm_iter, def)
		{
		  if (!gimple_debug_bind_p (stmt))
		    continue;

		  FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		    SET_USE (use_p, comp);

		  update_stmt (stmt);
		}
	    }
	}
    }

  release_defs_bitset (toremove);

  BITMAP_FREE (toremove);
}

/* Frees memory occupied by struct tree_niter_desc in *VALUE. Callback
   for hash_map::traverse.  */

bool
free_tree_niter_desc (edge const &, tree_niter_desc *const &value, void *)
{
  free (value);
  return true;
}

/* Frees data allocated by the optimization of a single loop.  */

static void
free_loop_data (struct ivopts_data *data)
{
  unsigned i, j;
  bitmap_iterator bi;
  tree obj;

  if (data->niters)
    {
      data->niters->traverse<void *, free_tree_niter_desc> (NULL);
      delete data->niters;
      data->niters = NULL;
    }

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
    {
      struct version_info *info;

      info = ver_info (data, i);
      info->iv = NULL;
      info->has_nonlin_use = false;
      info->preserve_biv = false;
      info->inv_id = 0;
    }
  bitmap_clear (data->relevant);
  bitmap_clear (data->important_candidates);

  for (i = 0; i < data->vgroups.length (); i++)
    {
      struct iv_group *group = data->vgroups[i];

      for (j = 0; j < group->vuses.length (); j++)
	free (group->vuses[j]);
      group->vuses.release ();

      BITMAP_FREE (group->related_cands);
      for (j = 0; j < group->n_map_members; j++)
	if (group->cost_map[j].depends_on)
	  BITMAP_FREE (group->cost_map[j].depends_on);

      free (group->cost_map);
      free (group);
    }
  data->vgroups.truncate (0);

  for (i = 0; i < data->vcands.length (); i++)
    {
      struct iv_cand *cand = data->vcands[i];

      if (cand->depends_on)
	BITMAP_FREE (cand->depends_on);
      free (cand);
    }
  data->vcands.truncate (0);

  if (data->version_info_size < num_ssa_names)
    {
      data->version_info_size = 2 * num_ssa_names;
      free (data->version_info);
      data->version_info = XCNEWVEC (struct version_info, data->version_info_size);
    }

  data->max_inv_id = 0;

  FOR_EACH_VEC_ELT (decl_rtl_to_reset, i, obj)
    SET_DECL_RTL (obj, NULL_RTX);

  decl_rtl_to_reset.truncate (0);

  data->inv_expr_tab->empty ();
  data->inv_expr_id = 0;

  data->iv_common_cand_tab->empty ();
  data->iv_common_cands.truncate (0);
}

/* Finalizes data structures used by the iv optimization pass.  LOOPS is the
   loop tree.  */

static void
tree_ssa_iv_optimize_finalize (struct ivopts_data *data)
{
  free_loop_data (data);
  free (data->version_info);
  BITMAP_FREE (data->relevant);
  BITMAP_FREE (data->important_candidates);

  decl_rtl_to_reset.release ();
  data->vgroups.release ();
  data->vcands.release ();
  delete data->inv_expr_tab;
  data->inv_expr_tab = NULL;
  free_affine_expand_cache (&data->name_expansion_cache);
  delete data->iv_common_cand_tab;
  data->iv_common_cand_tab = NULL;
  data->iv_common_cands.release ();
  obstack_free (&data->iv_obstack, NULL);
}

/* Returns true if the loop body BODY includes any function calls.  */

static bool
loop_body_includes_call (basic_block *body, unsigned num_nodes)
{
  gimple_stmt_iterator gsi;
  unsigned i;

  for (i = 0; i < num_nodes; i++)
    for (gsi = gsi_start_bb (body[i]); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_call (stmt)
	    && !is_inexpensive_builtin (gimple_call_fndecl (stmt)))
	  return true;
      }
  return false;
}

/* Optimizes the LOOP.  Returns true if anything changed.  */

static bool
tree_ssa_iv_optimize_loop (struct ivopts_data *data, struct loop *loop)
{
  bool changed = false;
  struct iv_ca *iv_ca;
  edge exit = single_dom_exit (loop);
  basic_block *body;

  gcc_assert (!data->niters);
  data->current_loop = loop;
  data->loop_loc = find_loop_location (loop);
  data->speed = optimize_loop_for_speed_p (loop);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Processing loop %d", loop->num);
      if (data->loop_loc != UNKNOWN_LOCATION)
	fprintf (dump_file, " at %s:%d", LOCATION_FILE (data->loop_loc),
		 LOCATION_LINE (data->loop_loc));
      fprintf (dump_file, "\n");

      if (exit)
	{
	  fprintf (dump_file, "  single exit %d -> %d, exit condition ",
		   exit->src->index, exit->dest->index);
	  print_gimple_stmt (dump_file, last_stmt (exit->src), 0, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      fprintf (dump_file, "\n");
    }

  body = get_loop_body (loop);
  data->body_includes_call = loop_body_includes_call (body, loop->num_nodes);
  renumber_gimple_stmt_uids_in_blocks (body, loop->num_nodes);
  free (body);

  data->loop_single_exit_p = exit != NULL && loop_only_exit_p (loop, exit);

  /* For each ssa name determines whether it behaves as an induction variable
     in some loop.  */
  if (!find_induction_variables (data))
    goto finish;

  /* Finds interesting uses (item 1).  */
  find_interesting_uses (data);
  if (data->vgroups.length () > MAX_CONSIDERED_GROUPS)
    goto finish;

  /* Finds candidates for the induction variables (item 2).  */
  find_iv_candidates (data);

  /* Calculates the costs (item 3, part 1).  */
  determine_iv_costs (data);
  determine_group_iv_costs (data);
  determine_set_costs (data);

  /* Find the optimal set of induction variables (item 3, part 2).  */
  iv_ca = find_optimal_iv_set (data);
  if (!iv_ca)
    goto finish;
  changed = true;

  /* Create the new induction variables (item 4, part 1).  */
  create_new_ivs (data, iv_ca);
  iv_ca_free (&iv_ca);

  /* Rewrite the uses (item 4, part 2).  */
  rewrite_groups (data);

  /* Remove the ivs that are unused after rewriting.  */
  remove_unused_ivs (data);

  /* We have changed the structure of induction variables; it might happen
     that definitions in the scev database refer to some of them that were
     eliminated.  */
  scev_reset ();

finish:
  free_loop_data (data);

  return changed;
}

/* Main entry point.  Optimizes induction variables in loops.  */

void
tree_ssa_iv_optimize (void)
{
  struct loop *loop;
  struct ivopts_data data;

  tree_ssa_iv_optimize_init (&data);

  /* Optimize the loops starting with the innermost ones.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	flow_loop_dump (loop, dump_file, NULL, 1);

      tree_ssa_iv_optimize_loop (&data, loop);
    }

  tree_ssa_iv_optimize_finalize (&data);
}
