/* Induction variable optimizations.
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

   2) Candidates for the induction variables are found.  This includes

      -- old induction variables
      -- the variables defined by expressions derived from the "interesting
	 uses" above

   3) The optimal (w.r. to a cost function) set of variables is chosen.  The
      cost function assigns a cost to sets of induction variables and consists
      of three parts:

      -- The use costs.  Each of the interesting uses chooses the best induction
	 variable in the set and adds its cost to the sum.  The cost reflects
	 the time spent on modifying the induction variables value to be usable
	 for the given purpose (adding base and offset for arrays, etc.).
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
#include "varray.h"
#include "expr.h"
#include "tree-pass.h"
#include "ggc.h"
#include "insn-config.h"
#include "recog.h"
#include "hashtab.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "cfgloop.h"
#include "params.h"
#include "langhooks.h"

/* The infinite cost.  */
#define INFTY 10000000

/* The expected number of loop iterations.  TODO -- use profiling instead of
   this.  */
#define AVG_LOOP_NITER(LOOP) 5


/* Representation of the induction variable.  */
struct iv
{
  tree base;		/* Initial value of the iv.  */
  tree base_object;	/* A memory object to that the induction variable points.  */
  tree step;		/* Step of the iv (constant only).  */
  tree ssa_name;	/* The ssa name with the value.  */
  bool biv_p;		/* Is it a biv?  */
  bool have_use_for;	/* Do we already have a use for it?  */
  unsigned use_id;	/* The identifier in the use if it is the case.  */
};

/* Per-ssa version information (induction variable descriptions, etc.).  */
struct version_info
{
  tree name;		/* The ssa name.  */
  struct iv *iv;	/* Induction variable description.  */
  bool has_nonlin_use;	/* For a loop-level invariant, whether it is used in
			   an expression that is not an induction variable.  */
  unsigned inv_id;	/* Id of an invariant.  */
  bool preserve_biv;	/* For the original biv, whether to preserve it.  */
};

/* Information attached to loop.  */
struct loop_data
{
  unsigned regs_used;	/* Number of registers used.  */
};

/* Types of uses.  */
enum use_type
{
  USE_NONLINEAR_EXPR,	/* Use in a nonlinear expression.  */
  USE_OUTER,		/* The induction variable is used outside the loop.  */
  USE_ADDRESS,		/* Use in an address.  */
  USE_COMPARE		/* Use is a compare.  */
};

/* The candidate - cost pair.  */
struct cost_pair
{
  struct iv_cand *cand;	/* The candidate.  */
  unsigned cost;	/* The cost.  */
  bitmap depends_on;	/* The list of invariants that have to be
			   preserved.  */
};

/* Use.  */
struct iv_use
{
  unsigned id;		/* The id of the use.  */
  enum use_type type;	/* Type of the use.  */
  struct iv *iv;	/* The induction variable it is based on.  */
  tree stmt;		/* Statement in that it occurs.  */
  tree *op_p;		/* The place where it occurs.  */
  bitmap related_cands;	/* The set of "related" iv candidates, plus the common
			   important ones.  */

  unsigned n_map_members; /* Number of candidates in the cost_map list.  */
  struct cost_pair *cost_map;
			/* The costs wrto the iv candidates.  */

  struct iv_cand *selected;
			/* The selected candidate.  */
};

/* The position where the iv is computed.  */
enum iv_position
{
  IP_NORMAL,		/* At the end, just before the exit condition.  */
  IP_END,		/* At the end of the latch block.  */
  IP_ORIGINAL		/* The original biv.  */
};

/* The induction variable candidate.  */
struct iv_cand
{
  unsigned id;		/* The number of the candidate.  */
  bool important;	/* Whether this is an "important" candidate, i.e. such
			   that it should be considered by all uses.  */
  enum iv_position pos;	/* Where it is computed.  */
  tree incremented_at;	/* For original biv, the statement where it is
			   incremented.  */
  tree var_before;	/* The variable used for it before increment.  */
  tree var_after;	/* The variable used for it after increment.  */
  struct iv *iv;	/* The value of the candidate.  NULL for
			   "pseudocandidate" used to indicate the possibility
			   to replace the final value of an iv by direct
			   computation of the value.  */
  unsigned cost;	/* Cost of the candidate.  */
};

/* The data used by the induction variable optimizations.  */

struct ivopts_data
{
  /* The currently optimized loop.  */
  struct loop *current_loop;

  /* Numbers of iterations for all exits of the current loop.  */
  htab_t niters;

  /* The size of version_info array allocated.  */
  unsigned version_info_size;

  /* The array of information for the ssa names.  */
  struct version_info *version_info;

  /* The bitmap of indices in version_info whose value was changed.  */
  bitmap relevant;

  /* The maximum invariant id.  */
  unsigned max_inv_id;

  /* The uses of induction variables.  */
  varray_type iv_uses;

  /* The candidates.  */
  varray_type iv_candidates;

  /* A bitmap of important candidates.  */
  bitmap important_candidates;

  /* Whether to consider just related and important candidates when replacing a
     use.  */
  bool consider_all_candidates;
};

/* An assignment of iv candidates to uses.  */

struct iv_ca
{
  /* The number of uses covered by the assignment.  */
  unsigned upto;

  /* Number of uses that cannot be expressed by the candidates in the set.  */
  unsigned bad_uses;

  /* Candidate assigned to a use, together with the related costs.  */
  struct cost_pair **cand_for_use;

  /* Number of times each candidate is used.  */
  unsigned *n_cand_uses;

  /* The candidates used.  */
  bitmap cands;

  /* The number of candidates in the set.  */
  unsigned n_cands;

  /* Total number of registers needed.  */
  unsigned n_regs;

  /* Total cost of expressing uses.  */
  unsigned cand_use_cost;

  /* Total cost of candidates.  */
  unsigned cand_cost;

  /* Number of times each invariant is used.  */
  unsigned *n_invariant_uses;

  /* Total cost of the assignment.  */
  unsigned cost;
};

/* Difference of two iv candidate assignments.  */

struct iv_ca_delta
{
  /* Changed use.  */
  struct iv_use *use;

  /* An old assignment (for rollback purposes).  */
  struct cost_pair *old_cp;

  /* A new assignment.  */
  struct cost_pair *new_cp;

  /* Next change in the list.  */
  struct iv_ca_delta *next_change;
};

/* Bound on number of candidates below that all candidates are considered.  */

#define CONSIDER_ALL_CANDIDATES_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_CONSIDER_ALL_CANDIDATES_BOUND))

/* If there are more iv occurrences, we just give up (it is quite unlikely that
   optimizing such a loop would help, and it would take ages).  */

#define MAX_CONSIDERED_USES \
  ((unsigned) PARAM_VALUE (PARAM_IV_MAX_CONSIDERED_USES))

/* If there are at most this number of ivs in the set, try removing unnecessary
   ivs from the set always.  */

#define ALWAYS_PRUNE_CAND_SET_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_ALWAYS_PRUNE_CAND_SET_BOUND))

/* The list of trees for that the decl_rtl field must be reset is stored
   here.  */

static varray_type decl_rtl_to_reset;

/* Number of uses recorded in DATA.  */

static inline unsigned
n_iv_uses (struct ivopts_data *data)
{
  return VARRAY_ACTIVE_SIZE (data->iv_uses);
}

/* Ith use recorded in DATA.  */

static inline struct iv_use *
iv_use (struct ivopts_data *data, unsigned i)
{
  return VARRAY_GENERIC_PTR_NOGC (data->iv_uses, i);
}

/* Number of candidates recorded in DATA.  */

static inline unsigned
n_iv_cands (struct ivopts_data *data)
{
  return VARRAY_ACTIVE_SIZE (data->iv_candidates);
}

/* Ith candidate recorded in DATA.  */

static inline struct iv_cand *
iv_cand (struct ivopts_data *data, unsigned i)
{
  return VARRAY_GENERIC_PTR_NOGC (data->iv_candidates, i);
}

/* The data for LOOP.  */

static inline struct loop_data *
loop_data (struct loop *loop)
{
  return loop->aux;
}

/* The single loop exit if it dominates the latch, NULL otherwise.  */

static edge
single_dom_exit (struct loop *loop)
{
  edge exit = loop->single_exit;

  if (!exit)
    return NULL;

  if (!just_once_each_iteration_p (loop, exit->src))
    return NULL;

  return exit;
}

/* Dumps information about the induction variable IV to FILE.  */

extern void dump_iv (FILE *, struct iv *);
void
dump_iv (FILE *file, struct iv *iv)
{
  if (iv->ssa_name)
    {
      fprintf (file, "ssa name ");
      print_generic_expr (file, iv->ssa_name, TDF_SLIM);
      fprintf (file, "\n");
    }

  fprintf (file, "  type ");
  print_generic_expr (file, TREE_TYPE (iv->base), TDF_SLIM);
  fprintf (file, "\n");

  if (iv->step)
    {
      fprintf (file, "  base ");
      print_generic_expr (file, iv->base, TDF_SLIM);
      fprintf (file, "\n");

      fprintf (file, "  step ");
      print_generic_expr (file, iv->step, TDF_SLIM);
      fprintf (file, "\n");
    }
  else
    {
      fprintf (file, "  invariant ");
      print_generic_expr (file, iv->base, TDF_SLIM);
      fprintf (file, "\n");
    }

  if (iv->base_object)
    {
      fprintf (file, "  base object ");
      print_generic_expr (file, iv->base_object, TDF_SLIM);
      fprintf (file, "\n");
    }

  if (iv->biv_p)
    fprintf (file, "  is a biv\n");
}

/* Dumps information about the USE to FILE.  */

extern void dump_use (FILE *, struct iv_use *);
void
dump_use (FILE *file, struct iv_use *use)
{
  fprintf (file, "use %d\n", use->id);

  switch (use->type)
    {
    case USE_NONLINEAR_EXPR:
      fprintf (file, "  generic\n");
      break;

    case USE_OUTER:
      fprintf (file, "  outside\n");
      break;

    case USE_ADDRESS:
      fprintf (file, "  address\n");
      break;

    case USE_COMPARE:
      fprintf (file, "  compare\n");
      break;

    default:
      gcc_unreachable ();
    }

  fprintf (file, "  in statement ");
  print_generic_expr (file, use->stmt, TDF_SLIM);
  fprintf (file, "\n");

  fprintf (file, "  at position ");
  if (use->op_p)
    print_generic_expr (file, *use->op_p, TDF_SLIM);
  fprintf (file, "\n");

  dump_iv (file, use->iv);

  if (use->related_cands)
    {
      fprintf (file, "  related candidates ");
      dump_bitmap (file, use->related_cands);
    }
}

/* Dumps information about the uses to FILE.  */

extern void dump_uses (FILE *, struct ivopts_data *);
void
dump_uses (FILE *file, struct ivopts_data *data)
{
  unsigned i;
  struct iv_use *use;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);

      dump_use (file, use);
      fprintf (file, "\n");
    }
}

/* Dumps information about induction variable candidate CAND to FILE.  */

extern void dump_cand (FILE *, struct iv_cand *);
void
dump_cand (FILE *file, struct iv_cand *cand)
{
  struct iv *iv = cand->iv;

  fprintf (file, "candidate %d%s\n",
	   cand->id, cand->important ? " (important)" : "");

  if (!iv)
    {
      fprintf (file, "  final value replacement\n");
      return;
    }

  switch (cand->pos)
    {
    case IP_NORMAL:
      fprintf (file, "  incremented before exit test\n");
      break;

    case IP_END:
      fprintf (file, "  incremented at end\n");
      break;

    case IP_ORIGINAL:
      fprintf (file, "  original biv\n");
      break;
    }

  dump_iv (file, iv);
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

/* Checks whether there exists number X such that X * B = A, counting modulo
   2^BITS.  */

static bool
divide (unsigned bits, unsigned HOST_WIDE_INT a, unsigned HOST_WIDE_INT b,
	HOST_WIDE_INT *x)
{
  unsigned HOST_WIDE_INT mask = ~(~(unsigned HOST_WIDE_INT) 0 << (bits - 1) << 1);
  unsigned HOST_WIDE_INT inv, ex, val;
  unsigned i;

  a &= mask;
  b &= mask;

  /* First divide the whole equation by 2 as long as possible.  */
  while (!(a & 1) && !(b & 1))
    {
      a >>= 1;
      b >>= 1;
      bits--;
      mask >>= 1;
    }

  if (!(b & 1))
    {
      /* If b is still even, a is odd and there is no such x.  */
      return false;
    }

  /* Find the inverse of b.  We compute it as
     b^(2^(bits - 1) - 1) (mod 2^bits).  */
  inv = 1;
  ex = b;
  for (i = 0; i < bits - 1; i++)
    {
      inv = (inv * ex) & mask;
      ex = (ex * ex) & mask;
    }

  val = (a * inv) & mask;

  gcc_assert (((val * b) & mask) == a);

  if ((val >> (bits - 1)) & 1)
    val |= ~mask;

  *x = val;

  return true;
}

/* Returns true if STMT is after the place where the IP_NORMAL ivs will be
   emitted in LOOP.  */

static bool
stmt_after_ip_normal_pos (struct loop *loop, tree stmt)
{
  basic_block bb = ip_normal_pos (loop), sbb = bb_for_stmt (stmt);

  gcc_assert (bb);

  if (sbb == loop->latch)
    return true;

  if (sbb != bb)
    return false;

  return stmt == last_stmt (bb);
}

/* Returns true if STMT if after the place where the original induction
   variable CAND is incremented.  */

static bool
stmt_after_ip_original_pos (struct iv_cand *cand, tree stmt)
{
  basic_block cand_bb = bb_for_stmt (cand->incremented_at);
  basic_block stmt_bb = bb_for_stmt (stmt);
  block_stmt_iterator bsi;

  if (!dominated_by_p (CDI_DOMINATORS, stmt_bb, cand_bb))
    return false;

  if (stmt_bb != cand_bb)
    return true;

  /* Scan the block from the end, since the original ivs are usually
     incremented at the end of the loop body.  */
  for (bsi = bsi_last (stmt_bb); ; bsi_prev (&bsi))
    {
      if (bsi_stmt (bsi) == cand->incremented_at)
	return false;
      if (bsi_stmt (bsi) == stmt)
	return true;
    }
}

/* Returns true if STMT if after the place where the induction variable
   CAND is incremented in LOOP.  */

static bool
stmt_after_increment (struct loop *loop, struct iv_cand *cand, tree stmt)
{
  switch (cand->pos)
    {
    case IP_END:
      return false;

    case IP_NORMAL:
      return stmt_after_ip_normal_pos (loop, stmt);

    case IP_ORIGINAL:
      return stmt_after_ip_original_pos (cand, stmt);

    default:
      gcc_unreachable ();
    }
}

/* Element of the table in that we cache the numbers of iterations obtained
   from exits of the loop.  */

struct nfe_cache_elt
{
  /* The edge for that the number of iterations is cached.  */
  edge exit;

  /* True if the # of iterations was successfully determined.  */
  bool valid_p;

  /* Description of # of iterations.  */
  struct tree_niter_desc niter;
};

/* Hash function for nfe_cache_elt E.  */

static hashval_t
nfe_hash (const void *e)
{
  const struct nfe_cache_elt *elt = e;

  return htab_hash_pointer (elt->exit);
}

/* Equality function for nfe_cache_elt E1 and edge E2.  */

static int
nfe_eq (const void *e1, const void *e2)
{
  const struct nfe_cache_elt *elt1 = e1;

  return elt1->exit == e2;
}

/*  Returns structure describing number of iterations determined from
    EXIT of DATA->current_loop, or NULL if something goes wrong.  */

static struct tree_niter_desc *
niter_for_exit (struct ivopts_data *data, edge exit)
{
  struct nfe_cache_elt *nfe_desc;
  PTR *slot;

  slot = htab_find_slot_with_hash (data->niters, exit,
				   htab_hash_pointer (exit),
				   INSERT);

  if (!*slot)
    {
      nfe_desc = xmalloc (sizeof (struct nfe_cache_elt));
      nfe_desc->exit = exit;
      nfe_desc->valid_p = number_of_iterations_exit (data->current_loop,
						     exit, &nfe_desc->niter);
      *slot = nfe_desc;
    }
  else
    nfe_desc = *slot;

  if (!nfe_desc->valid_p)
    return NULL;

  return &nfe_desc->niter;
}

/* Returns structure describing number of iterations determined from
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
   in DATA.  LOOPS is the loop tree.  */

static void
tree_ssa_iv_optimize_init (struct loops *loops, struct ivopts_data *data)
{
  unsigned i;

  data->version_info_size = 2 * num_ssa_names;
  data->version_info = xcalloc (data->version_info_size,
				sizeof (struct version_info));
  data->relevant = BITMAP_ALLOC (NULL);
  data->important_candidates = BITMAP_ALLOC (NULL);
  data->max_inv_id = 0;
  data->niters = htab_create (10, nfe_hash, nfe_eq, free);

  for (i = 1; i < loops->num; i++)
    if (loops->parray[i])
      loops->parray[i]->aux = xcalloc (1, sizeof (struct loop_data));

  VARRAY_GENERIC_PTR_NOGC_INIT (data->iv_uses, 20, "iv_uses");
  VARRAY_GENERIC_PTR_NOGC_INIT (data->iv_candidates, 20, "iv_candidates");
  VARRAY_GENERIC_PTR_NOGC_INIT (decl_rtl_to_reset, 20, "decl_rtl_to_reset");
}

/* Returns a memory object to that EXPR points.  In case we are able to
   determine that it does not point to any such object, NULL is returned.  */

static tree
determine_base_object (tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  tree base, obj, op0, op1;

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

      if (TREE_CODE (base) == INDIRECT_REF)
	return determine_base_object (TREE_OPERAND (base, 0));

      return fold (build1 (ADDR_EXPR, ptr_type_node, base));

    case PLUS_EXPR:
    case MINUS_EXPR:
      op0 = determine_base_object (TREE_OPERAND (expr, 0));
      op1 = determine_base_object (TREE_OPERAND (expr, 1));
      
      if (!op1)
	return op0;

      if (!op0)
	return (code == PLUS_EXPR
		? op1
		: fold (build1 (NEGATE_EXPR, ptr_type_node, op1)));

      return fold (build (code, ptr_type_node, op0, op1));

    case NOP_EXPR:
    case CONVERT_EXPR:
      return determine_base_object (TREE_OPERAND (expr, 0));

    default:
      return fold_convert (ptr_type_node, expr);
    }
}

/* Allocates an induction variable with given initial value BASE and step STEP
   for loop LOOP.  */

static struct iv *
alloc_iv (tree base, tree step)
{
  struct iv *iv = xcalloc (1, sizeof (struct iv));

  if (step && integer_zerop (step))
    step = NULL_TREE;

  iv->base = base;
  iv->base_object = determine_base_object (base);
  iv->step = step;
  iv->biv_p = false;
  iv->have_use_for = false;
  iv->use_id = 0;
  iv->ssa_name = NULL_TREE;

  return iv;
}

/* Sets STEP and BASE for induction variable IV.  */

static void
set_iv (struct ivopts_data *data, tree iv, tree base, tree step)
{
  struct version_info *info = name_info (data, iv);

  gcc_assert (!info->iv);

  bitmap_set_bit (data->relevant, SSA_NAME_VERSION (iv));
  info->iv = alloc_iv (base, step);
  info->iv->ssa_name = iv;
}

/* Finds induction variable declaration for VAR.  */

static struct iv *
get_iv (struct ivopts_data *data, tree var)
{
  basic_block bb;
  
  if (!name_info (data, var)->iv)
    {
      bb = bb_for_stmt (SSA_NAME_DEF_STMT (var));

      if (!bb
	  || !flow_bb_inside_loop_p (data->current_loop, bb))
	set_iv (data, var, var, NULL_TREE);
    }

  return name_info (data, var)->iv;
}

/* Determines the step of a biv defined in PHI.  */

static tree
determine_biv_step (tree phi)
{
  struct loop *loop = bb_for_stmt (phi)->loop_father;
  tree name = PHI_RESULT (phi), base, step;
  tree type = TREE_TYPE (name);

  if (!is_gimple_reg (name))
    return NULL_TREE;

  if (!simple_iv (loop, phi, name, &base, &step))
    return NULL_TREE;

  if (!step)
    return build_int_cst (type, 0);

  return step;
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
  if (TREE_CODE (base) == ARRAY_REF)
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

static bool
contains_abnormal_ssa_name_p (tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  enum tree_code_class class = TREE_CODE_CLASS (code);
    
  if (code == SSA_NAME)
    return SSA_NAME_OCCURS_IN_ABNORMAL_PHI (expr) != 0;

  if (code == INTEGER_CST
      || is_gimple_min_invariant (expr))
    return false;

  if (code == ADDR_EXPR)
    return !for_each_index (&TREE_OPERAND (expr, 0),
			    idx_contains_abnormal_ssa_name_p,
			    NULL);

  switch (class)
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

/* Finds basic ivs.  */

static bool
find_bivs (struct ivopts_data *data)
{
  tree phi, step, type, base;
  bool found = false;
  struct loop *loop = data->current_loop;

  for (phi = phi_nodes (loop->header); phi; phi = PHI_CHAIN (phi))
    {
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
	continue;

      step = determine_biv_step (phi);

      if (!step)
	continue;
      if (cst_and_fits_in_hwi (step)
	  && int_cst_value (step) == 0)
	continue;

      base = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (loop));
      if (contains_abnormal_ssa_name_p (base))
	continue;

      type = TREE_TYPE (PHI_RESULT (phi));
      base = fold_convert (type, base);
      step = fold_convert (type, step);

      /* FIXME: We do not handle induction variables whose step does
	 not satisfy cst_and_fits_in_hwi.  */
      if (!cst_and_fits_in_hwi (step))
	continue;

      set_iv (data, PHI_RESULT (phi), base, step);
      found = true;
    }

  return found;
}

/* Marks basic ivs.  */

static void
mark_bivs (struct ivopts_data *data)
{
  tree phi, var;
  struct iv *iv, *incr_iv;
  struct loop *loop = data->current_loop;
  basic_block incr_bb;

  for (phi = phi_nodes (loop->header); phi; phi = PHI_CHAIN (phi))
    {
      iv = get_iv (data, PHI_RESULT (phi));
      if (!iv)
	continue;

      var = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (loop));
      incr_iv = get_iv (data, var);
      if (!incr_iv)
	continue;

      /* If the increment is in the subloop, ignore it.  */
      incr_bb = bb_for_stmt (SSA_NAME_DEF_STMT (var));
      if (incr_bb->loop_father != data->current_loop
	  || (incr_bb->flags & BB_IRREDUCIBLE_LOOP))
	continue;

      iv->biv_p = true;
      incr_iv->biv_p = true;
    }
}

/* Checks whether STMT defines a linear induction variable and stores its
   parameters to BASE and STEP.  */

static bool
find_givs_in_stmt_scev (struct ivopts_data *data, tree stmt,
			tree *base, tree *step)
{
  tree lhs;
  struct loop *loop = data->current_loop;

  *base = NULL_TREE;
  *step = NULL_TREE;

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return false;

  lhs = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  if (!simple_iv (loop, stmt, TREE_OPERAND (stmt, 1), base, step))
    return false;

  /* FIXME: We do not handle induction variables whose step does
     not satisfy cst_and_fits_in_hwi.  */
  if (!zero_p (*step)
      && !cst_and_fits_in_hwi (*step))
    return false;

  if (contains_abnormal_ssa_name_p (*base))
    return false;

  return true;
}

/* Finds general ivs in statement STMT.  */

static void
find_givs_in_stmt (struct ivopts_data *data, tree stmt)
{
  tree base, step;

  if (!find_givs_in_stmt_scev (data, stmt, &base, &step))
    return;

  set_iv (data, TREE_OPERAND (stmt, 0), base, step);
}

/* Finds general ivs in basic block BB.  */

static void
find_givs_in_bb (struct ivopts_data *data, basic_block bb)
{
  block_stmt_iterator bsi;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    find_givs_in_stmt (data, bsi_stmt (bsi));
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
      struct tree_niter_desc *niter;

      niter = niter_for_single_dom_exit (data);

      if (niter)
	{
	  fprintf (dump_file, "  number of iterations ");
	  print_generic_expr (dump_file, niter->niter, TDF_SLIM);
	  fprintf (dump_file, "\n");

    	  fprintf (dump_file, "  may be zero if ");
    	  print_generic_expr (dump_file, niter->may_be_zero, TDF_SLIM);
    	  fprintf (dump_file, "\n");
    	  fprintf (dump_file, "\n");
    	};
 
      fprintf (dump_file, "Induction variables:\n\n");

      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
	{
	  if (ver_info (data, i)->iv)
	    dump_iv (dump_file, ver_info (data, i)->iv);
	}
    }

  return true;
}

/* Records a use of type USE_TYPE at *USE_P in STMT whose value is IV.  */

static struct iv_use *
record_use (struct ivopts_data *data, tree *use_p, struct iv *iv,
	    tree stmt, enum use_type use_type)
{
  struct iv_use *use = xcalloc (1, sizeof (struct iv_use));

  use->id = n_iv_uses (data);
  use->type = use_type;
  use->iv = iv;
  use->stmt = stmt;
  use->op_p = use_p;
  use->related_cands = BITMAP_ALLOC (NULL);

  /* To avoid showing ssa name in the dumps, if it was not reset by the
     caller.  */
  iv->ssa_name = NULL_TREE;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_use (dump_file, use);

  VARRAY_PUSH_GENERIC_PTR_NOGC (data->iv_uses, use);

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
      || !is_gimple_reg (op))
    return;

  bb = bb_for_stmt (SSA_NAME_DEF_STMT (op));
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

/* Checks whether the use OP is interesting and if so, records it
   as TYPE.  */

static struct iv_use *
find_interesting_uses_outer_or_nonlin (struct ivopts_data *data, tree op,
				       enum use_type type)
{
  struct iv *iv;
  struct iv *civ;
  tree stmt;
  struct iv_use *use;

  if (TREE_CODE (op) != SSA_NAME)
    return NULL;

  iv = get_iv (data, op);
  if (!iv)
    return NULL;
  
  if (iv->have_use_for)
    {
      use = iv_use (data, iv->use_id);

      gcc_assert (use->type == USE_NONLINEAR_EXPR
		  || use->type == USE_OUTER);

      if (type == USE_NONLINEAR_EXPR)
	use->type = USE_NONLINEAR_EXPR;
      return use;
    }

  if (zero_p (iv->step))
    {
      record_invariant (data, op, true);
      return NULL;
    }
  iv->have_use_for = true;

  civ = xmalloc (sizeof (struct iv));
  *civ = *iv;

  stmt = SSA_NAME_DEF_STMT (op);
  gcc_assert (TREE_CODE (stmt) == PHI_NODE
	      || TREE_CODE (stmt) == MODIFY_EXPR);

  use = record_use (data, NULL, civ, stmt, type);
  iv->use_id = use->id;

  return use;
}

/* Checks whether the use OP is interesting and if so, records it.  */

static struct iv_use *
find_interesting_uses_op (struct ivopts_data *data, tree op)
{
  return find_interesting_uses_outer_or_nonlin (data, op, USE_NONLINEAR_EXPR);
}

/* Records a definition of induction variable OP that is used outside of the
   loop.  */

static struct iv_use *
find_interesting_uses_outer (struct ivopts_data *data, tree op)
{
  return find_interesting_uses_outer_or_nonlin (data, op, USE_OUTER);
}

/* Checks whether the condition *COND_P in STMT is interesting
   and if so, records it.  */

static void
find_interesting_uses_cond (struct ivopts_data *data, tree stmt, tree *cond_p)
{
  tree *op0_p;
  tree *op1_p;
  struct iv *iv0 = NULL, *iv1 = NULL, *civ;
  struct iv const_iv;
  tree zero = integer_zero_node;

  const_iv.step = NULL_TREE;

  if (integer_zerop (*cond_p)
      || integer_nonzerop (*cond_p))
    return;

  if (TREE_CODE (*cond_p) == SSA_NAME)
    {
      op0_p = cond_p;
      op1_p = &zero;
    }
  else
    {
      op0_p = &TREE_OPERAND (*cond_p, 0);
      op1_p = &TREE_OPERAND (*cond_p, 1);
    }

  if (TREE_CODE (*op0_p) == SSA_NAME)
    iv0 = get_iv (data, *op0_p);
  else
    iv0 = &const_iv;

  if (TREE_CODE (*op1_p) == SSA_NAME)
    iv1 = get_iv (data, *op1_p);
  else
    iv1 = &const_iv;

  if (/* When comparing with non-invariant value, we may not do any senseful
	 induction variable elimination.  */
      (!iv0 || !iv1)
      /* Eliminating condition based on two ivs would be nontrivial.
	 ??? TODO -- it is not really important to handle this case.  */
      || (!zero_p (iv0->step) && !zero_p (iv1->step)))
    {
      find_interesting_uses_op (data, *op0_p);
      find_interesting_uses_op (data, *op1_p);
      return;
    }

  if (zero_p (iv0->step) && zero_p (iv1->step))
    {
      /* If both are invariants, this is a work for unswitching.  */
      return;
    }

  civ = xmalloc (sizeof (struct iv));
  *civ = zero_p (iv0->step) ? *iv1: *iv0;
  record_use (data, cond_p, civ, stmt, USE_COMPARE);
}

/* Returns true if expression EXPR is obviously invariant in LOOP,
   i.e. if all its operands are defined outside of the LOOP.  */

bool
expr_invariant_in_loop_p (struct loop *loop, tree expr)
{
  basic_block def_bb;
  unsigned i, len;

  if (is_gimple_min_invariant (expr))
    return true;

  if (TREE_CODE (expr) == SSA_NAME)
    {
      def_bb = bb_for_stmt (SSA_NAME_DEF_STMT (expr));
      if (def_bb
	  && flow_bb_inside_loop_p (loop, def_bb))
	return false;

      return true;
    }

  if (!EXPR_P (expr))
    return false;

  len = TREE_CODE_LENGTH (TREE_CODE (expr));
  for (i = 0; i < len; i++)
    if (!expr_invariant_in_loop_p (loop, TREE_OPERAND (expr, i)))
      return false;

  return true;
}

/* Cumulates the steps of indices into DATA and replaces their values with the
   initial ones.  Returns false when the value of the index cannot be determined.
   Callback for for_each_index.  */

struct ifs_ivopts_data
{
  struct ivopts_data *ivopts_data;
  tree stmt;
  tree *step_p;
};

static bool
idx_find_step (tree base, tree *idx, void *data)
{
  struct ifs_ivopts_data *dta = data;
  struct iv *iv;
  tree step, type, iv_type, iv_step, lbound, off;
  struct loop *loop = dta->ivopts_data->current_loop;

  if (TREE_CODE (base) == MISALIGNED_INDIRECT_REF
      || TREE_CODE (base) == ALIGN_INDIRECT_REF)
    return false;

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
  if (TREE_CODE (base) == ARRAY_REF)
    {
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

  *idx = iv->base;

  if (!iv->step)
    return true;

  iv_type = TREE_TYPE (iv->base);
  type = build_pointer_type (TREE_TYPE (base));
  if (TREE_CODE (base) == ARRAY_REF)
    {
      step = array_ref_element_size (base);

      /* We only handle addresses whose step is an integer constant.  */
      if (TREE_CODE (step) != INTEGER_CST)
	return false;
    }
  else
    /* The step for pointer arithmetics already is 1 byte.  */
    step = build_int_cst (type, 1);

  if (TYPE_PRECISION (iv_type) < TYPE_PRECISION (type))
    iv_step = can_count_iv_in_wider_type (dta->ivopts_data->current_loop,
					  type, iv->base, iv->step, dta->stmt);
  else
    iv_step = fold_convert (iv_type, iv->step);

  if (!iv_step)
    {
      /* The index might wrap.  */
      return false;
    }

  step = fold_binary_to_constant (MULT_EXPR, type, step, iv_step);

  if (!*dta->step_p)
    *dta->step_p = step;
  else
    *dta->step_p = fold_binary_to_constant (PLUS_EXPR, type,
					    *dta->step_p, step);

  return true;
}

/* Records use in index IDX.  Callback for for_each_index.  Ivopts data
   object is passed to it in DATA.  */

static bool
idx_record_use (tree base, tree *idx,
		void *data)
{
  find_interesting_uses_op (data, *idx);
  if (TREE_CODE (base) == ARRAY_REF)
    {
      find_interesting_uses_op (data, array_ref_element_size (base));
      find_interesting_uses_op (data, array_ref_low_bound (base));
    }
  return true;
}

/* Returns true if memory reference REF may be unaligned.  */

static bool
may_be_unaligned_p (tree ref)
{
  tree base;
  tree base_type;
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  tree toffset;
  enum machine_mode mode;
  int unsignedp, volatilep;
  unsigned base_align;

  /* The test below is basically copy of what expr.c:normal_inner_ref
     does to check whether the object must be loaded by parts when
     STRICT_ALIGNMENT is true.  */
  base = get_inner_reference (ref, &bitsize, &bitpos, &toffset, &mode,
			      &unsignedp, &volatilep, true);
  base_type = TREE_TYPE (base);
  base_align = TYPE_ALIGN (base_type);

  if (mode != BLKmode
      && (base_align < GET_MODE_ALIGNMENT (mode)
	  || bitpos % GET_MODE_ALIGNMENT (mode) != 0
	  || bitpos % BITS_PER_UNIT != 0))
    return true;

  return false;
}

/* Finds addresses in *OP_P inside STMT.  */

static void
find_interesting_uses_address (struct ivopts_data *data, tree stmt, tree *op_p)
{
  tree base = unshare_expr (*op_p), step = NULL;
  struct iv *civ;
  struct ifs_ivopts_data ifs_ivopts_data;

  /* Do not play with volatile memory references.  A bit too conservative,
     perhaps, but safe.  */
  if (stmt_ann (stmt)->has_volatile_ops)
    goto fail;

  /* Ignore bitfields for now.  Not really something terribly complicated
     to handle.  TODO.  */
  if (TREE_CODE (base) == COMPONENT_REF
      && DECL_NONADDRESSABLE_P (TREE_OPERAND (base, 1)))
    goto fail;

  if (STRICT_ALIGNMENT
      && may_be_unaligned_p (base))
    goto fail;

  ifs_ivopts_data.ivopts_data = data;
  ifs_ivopts_data.stmt = stmt;
  ifs_ivopts_data.step_p = &step;
  if (!for_each_index (&base, idx_find_step, &ifs_ivopts_data)
      || zero_p (step))
    goto fail;

  gcc_assert (TREE_CODE (base) != ALIGN_INDIRECT_REF);
  gcc_assert (TREE_CODE (base) != MISALIGNED_INDIRECT_REF);

  if (TREE_CODE (base) == INDIRECT_REF)
    base = TREE_OPERAND (base, 0);
  else
    base = build_addr (base);

  civ = alloc_iv (base, step);
  record_use (data, op_p, civ, stmt, USE_ADDRESS);
  return;

fail:
  for_each_index (op_p, idx_record_use, data);
}

/* Finds and records invariants used in STMT.  */

static void
find_invariants_stmt (struct ivopts_data *data, tree stmt)
{
  use_optype uses = NULL;
  unsigned i, n;
  tree op;

  if (TREE_CODE (stmt) == PHI_NODE)
    n = PHI_NUM_ARGS (stmt);
  else
    {
      get_stmt_operands (stmt);
      uses = STMT_USE_OPS (stmt);
      n = NUM_USES (uses);
    }

  for (i = 0; i < n; i++)
    {
      if (TREE_CODE (stmt) == PHI_NODE)
	op = PHI_ARG_DEF (stmt, i);
      else
	op = USE_OP (uses, i);

      record_invariant (data, op, false);
    }
}

/* Finds interesting uses of induction variables in the statement STMT.  */

static void
find_interesting_uses_stmt (struct ivopts_data *data, tree stmt)
{
  struct iv *iv;
  tree op, lhs, rhs;
  use_optype uses = NULL;
  unsigned i, n;

  find_invariants_stmt (data, stmt);

  if (TREE_CODE (stmt) == COND_EXPR)
    {
      find_interesting_uses_cond (data, stmt, &COND_EXPR_COND (stmt));
      return;
    }

  if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      lhs = TREE_OPERAND (stmt, 0);
      rhs = TREE_OPERAND (stmt, 1);

      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  /* If the statement defines an induction variable, the uses are not
	     interesting by themselves.  */

	  iv = get_iv (data, lhs);

	  if (iv && !zero_p (iv->step))
	    return;
	}

      switch (TREE_CODE_CLASS (TREE_CODE (rhs)))
	{
	case tcc_comparison:
	  find_interesting_uses_cond (data, stmt, &TREE_OPERAND (stmt, 1));
	  return;

	case tcc_reference:
	  find_interesting_uses_address (data, stmt, &TREE_OPERAND (stmt, 1));
	  if (REFERENCE_CLASS_P (lhs))
	    find_interesting_uses_address (data, stmt, &TREE_OPERAND (stmt, 0));
	  return;

	default: ;
	}

      if (REFERENCE_CLASS_P (lhs)
	  && is_gimple_val (rhs))
	{
	  find_interesting_uses_address (data, stmt, &TREE_OPERAND (stmt, 0));
	  find_interesting_uses_op (data, rhs);
	  return;
	}

      /* TODO -- we should also handle address uses of type

	 memory = call (whatever);

	 and

	 call (memory).  */
    }

  if (TREE_CODE (stmt) == PHI_NODE
      && bb_for_stmt (stmt) == data->current_loop->header)
    {
      lhs = PHI_RESULT (stmt);
      iv = get_iv (data, lhs);

      if (iv && !zero_p (iv->step))
	return;
    }

  if (TREE_CODE (stmt) == PHI_NODE)
    n = PHI_NUM_ARGS (stmt);
  else
    {
      uses = STMT_USE_OPS (stmt);
      n = NUM_USES (uses);
    }

  for (i = 0; i < n; i++)
    {
      if (TREE_CODE (stmt) == PHI_NODE)
	op = PHI_ARG_DEF (stmt, i);
      else
	op = USE_OP (uses, i);

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
  tree phi, def;

  for (phi = phi_nodes (exit->dest); phi; phi = PHI_CHAIN (phi))
    {
      def = PHI_ARG_DEF_FROM_EDGE (phi, exit);
      find_interesting_uses_outer (data, def);
    }
}

/* Finds uses of the induction variables that are interesting.  */

static void
find_interesting_uses (struct ivopts_data *data)
{
  basic_block bb;
  block_stmt_iterator bsi;
  tree phi;
  basic_block *body = get_loop_body (data->current_loop);
  unsigned i;
  struct version_info *info;
  edge e;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Uses:\n\n");

  for (i = 0; i < data->current_loop->num_nodes; i++)
    {
      edge_iterator ei;
      bb = body[i];

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR
	    && !flow_bb_inside_loop_p (data->current_loop, e->dest))
	  find_interesting_uses_outside (data, e);

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	find_interesting_uses_stmt (data, phi);
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	find_interesting_uses_stmt (data, bsi_stmt (bsi));
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      bitmap_iterator bi;

      fprintf (dump_file, "\n");

      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
	{
	  info = ver_info (data, i);
	  if (info->inv_id)
	    {
	      fprintf (dump_file, "  ");
	      print_generic_expr (dump_file, info->name, TDF_SLIM);
	      fprintf (dump_file, " is invariant (%d)%s\n",
		       info->inv_id, info->has_nonlin_use ? "" : ", eliminable");
	    }
	}

      fprintf (dump_file, "\n");
    }

  free (body);
}

/* Strips constant offsets from EXPR and stores them to OFFSET.  If INSIDE_ADDR
   is true, assume we are inside an address.  */

static tree
strip_offset (tree expr, bool inside_addr, unsigned HOST_WIDE_INT *offset)
{
  tree op0 = NULL_TREE, op1 = NULL_TREE, step;
  enum tree_code code;
  tree type, orig_type = TREE_TYPE (expr);
  unsigned HOST_WIDE_INT off0, off1, st;
  tree orig_expr = expr;

  STRIP_NOPS (expr);
  type = TREE_TYPE (expr);
  code = TREE_CODE (expr);
  *offset = 0;

  switch (code)
    {
    case INTEGER_CST:
      if (!cst_and_fits_in_hwi (expr)
	  || zero_p (expr))
	return orig_expr;

      *offset = int_cst_value (expr);
      return build_int_cst_type (orig_type, 0);

    case PLUS_EXPR:
    case MINUS_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);

      op0 = strip_offset (op0, false, &off0);
      op1 = strip_offset (op1, false, &off1);

      *offset = (code == PLUS_EXPR ? off0 + off1 : off0 - off1);
      if (op0 == TREE_OPERAND (expr, 0)
	  && op1 == TREE_OPERAND (expr, 1))
	return orig_expr;

      if (zero_p (op1))
	expr = op0;
      else if (zero_p (op0))
	{
	  if (code == PLUS_EXPR)
	    expr = op1;
	  else
	    expr = build1 (NEGATE_EXPR, type, op1);
	}
      else
	expr = build2 (code, type, op0, op1);

      return fold_convert (orig_type, expr);

    case ARRAY_REF:
      if (!inside_addr)
	return orig_expr;

      step = array_ref_element_size (expr);
      if (!cst_and_fits_in_hwi (step))
	break;

      st = int_cst_value (step);
      op1 = TREE_OPERAND (expr, 1);
      op1 = strip_offset (op1, false, &off1);
      *offset = off1 * st;
      break;

    case COMPONENT_REF:
      if (!inside_addr)
	return orig_expr;
      break;

    case ADDR_EXPR:
      inside_addr = true;
      break;

    default:
      return orig_expr;
    }

  /* Default handling of expressions for that we want to recurse into
     the first operand.  */
  op0 = TREE_OPERAND (expr, 0);
  op0 = strip_offset (op0, inside_addr, &off0);
  *offset += off0;

  if (op0 == TREE_OPERAND (expr, 0)
      && (!op1 || op1 == TREE_OPERAND (expr, 1)))
    return orig_expr;

  expr = copy_node (expr);
  TREE_OPERAND (expr, 0) = op0;
  if (op1)
    TREE_OPERAND (expr, 1) = op1;

  return fold_convert (orig_type, expr);
}

/* Returns variant of TYPE that can be used as base for different uses.
   For integer types, we return unsigned variant of the type, which
   avoids problems with overflows.  For pointer types, we return void *.  */

static tree
generic_type_for (tree type)
{
  if (POINTER_TYPE_P (type))
    return ptr_type_node;

  if (TYPE_UNSIGNED (type))
    return type;

  return unsigned_type_for (type);
}

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  If both BASE and STEP are NULL, we add a pseudocandidate for the
   replacement of the final value of the iv by a direct computation.  */

static struct iv_cand *
add_candidate_1 (struct ivopts_data *data,
		 tree base, tree step, bool important, enum iv_position pos,
		 struct iv_use *use, tree incremented_at)
{
  unsigned i;
  struct iv_cand *cand = NULL;
  tree type, orig_type;
  
  if (base)
    {
      orig_type = TREE_TYPE (base);
      type = generic_type_for (orig_type);
      if (type != orig_type)
	{
	  base = fold_convert (type, base);
	  if (step)
	    step = fold_convert (type, step);
	}
    }

  for (i = 0; i < n_iv_cands (data); i++)
    {
      cand = iv_cand (data, i);

      if (cand->pos != pos)
	continue;

      if (cand->incremented_at != incremented_at)
	continue;

      if (!cand->iv)
	{
	  if (!base && !step)
	    break;

	  continue;
	}

      if (!base && !step)
	continue;

      if (!operand_equal_p (base, cand->iv->base, 0))
	continue;

      if (zero_p (cand->iv->step))
	{
	  if (zero_p (step))
	    break;
	}
      else
	{
	  if (step && operand_equal_p (step, cand->iv->step, 0))
	    break;
	}
    }

  if (i == n_iv_cands (data))
    {
      cand = xcalloc (1, sizeof (struct iv_cand));
      cand->id = i;

      if (!base && !step)
	cand->iv = NULL;
      else
	cand->iv = alloc_iv (base, step);

      cand->pos = pos;
      if (pos != IP_ORIGINAL && cand->iv)
	{
	  cand->var_before = create_tmp_var_raw (TREE_TYPE (base), "ivtmp");
	  cand->var_after = cand->var_before;
	}
      cand->important = important;
      cand->incremented_at = incremented_at;
      VARRAY_PUSH_GENERIC_PTR_NOGC (data->iv_candidates, cand);

      if (dump_file && (dump_flags & TDF_DETAILS))
	dump_cand (dump_file, cand);
    }

  if (important && !cand->important)
    {
      cand->important = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Candidate %d is important\n", cand->id);
    }

  if (use)
    {
      bitmap_set_bit (use->related_cands, i);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Candidate %d is related to use %d\n",
		 cand->id, use->id);
    }

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

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  The candidate computation is scheduled on all available positions.  */

static void
add_candidate (struct ivopts_data *data, 
	       tree base, tree step, bool important, struct iv_use *use)
{
  if (ip_normal_pos (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_NORMAL, use, NULL_TREE);
  if (ip_end_pos (data->current_loop)
      && allow_ip_end_pos_p (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_END, use, NULL_TREE);
}

/* Add a standard "0 + 1 * iteration" iv candidate for a
   type with SIZE bits.  */

static void
add_standard_iv_candidates_for_size (struct ivopts_data *data,
				     unsigned int size)
{
  tree type = lang_hooks.types.type_for_size (size, true);
  add_candidate (data, build_int_cst (type, 0), build_int_cst (type, 1),
		 true, NULL);
}

/* Adds standard iv candidates.  */

static void
add_standard_iv_candidates (struct ivopts_data *data)
{
  add_standard_iv_candidates_for_size (data, INT_TYPE_SIZE);

  /* The same for a double-integer type if it is still fast enough.  */
  if (BITS_PER_WORD >= INT_TYPE_SIZE * 2)
    add_standard_iv_candidates_for_size (data, INT_TYPE_SIZE * 2);
}


/* Adds candidates bases on the old induction variable IV.  */

static void
add_old_iv_candidates (struct ivopts_data *data, struct iv *iv)
{
  tree phi, def;
  struct iv_cand *cand;

  add_candidate (data, iv->base, iv->step, true, NULL);

  /* The same, but with initial value zero.  */
  add_candidate (data,
		 build_int_cst (TREE_TYPE (iv->base), 0),
		 iv->step, true, NULL);

  phi = SSA_NAME_DEF_STMT (iv->ssa_name);
  if (TREE_CODE (phi) == PHI_NODE)
    {
      /* Additionally record the possibility of leaving the original iv
	 untouched.  */
      def = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (data->current_loop));
      cand = add_candidate_1 (data,
			      iv->base, iv->step, true, IP_ORIGINAL, NULL,
			      SSA_NAME_DEF_STMT (def));
      cand->var_before = iv->ssa_name;
      cand->var_after = def;
    }
}

/* Adds candidates based on the old induction variables.  */

static void
add_old_ivs_candidates (struct ivopts_data *data)
{
  unsigned i;
  struct iv *iv;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
    {
      iv = ver_info (data, i)->iv;
      if (iv && iv->biv_p && !zero_p (iv->step))
	add_old_iv_candidates (data, iv);
    }
}

/* Adds candidates based on the value of the induction variable IV and USE.  */

static void
add_iv_value_candidates (struct ivopts_data *data,
			 struct iv *iv, struct iv_use *use)
{
  add_candidate (data, iv->base, iv->step, false, use);

  /* The same, but with initial value zero.  */
  add_candidate (data, build_int_cst (TREE_TYPE (iv->base), 0),
		 iv->step, false, use);
}

/* Adds candidates based on the address IV and USE.  */

static void
add_address_candidates (struct ivopts_data *data,
			struct iv *iv, struct iv_use *use)
{
  tree base, abase;
  unsigned HOST_WIDE_INT offset;

  /* First, the trivial choices.  */
  add_iv_value_candidates (data, iv, use);

  /* Second, try removing the COMPONENT_REFs.  */
  if (TREE_CODE (iv->base) == ADDR_EXPR)
    {
      base = TREE_OPERAND (iv->base, 0);
      while (TREE_CODE (base) == COMPONENT_REF
	     || (TREE_CODE (base) == ARRAY_REF
		 && TREE_CODE (TREE_OPERAND (base, 1)) == INTEGER_CST))
	base = TREE_OPERAND (base, 0);

      if (base != TREE_OPERAND (iv->base, 0))
	{ 
	  gcc_assert (TREE_CODE (base) != ALIGN_INDIRECT_REF);
	  gcc_assert (TREE_CODE (base) != MISALIGNED_INDIRECT_REF);

	  if (TREE_CODE (base) == INDIRECT_REF)
	    base = TREE_OPERAND (base, 0);
	  else
	    base = build_addr (base);
	  add_candidate (data, base, iv->step, false, use);
	}
    }

  /* Third, try removing the constant offset.  */
  abase = iv->base;
  base = strip_offset (abase, false, &offset);
  if (offset)
    add_candidate (data, base, iv->step, false, use);
}

/* Possibly adds pseudocandidate for replacing the final value of USE by
   a direct computation.  */

static void
add_iv_outer_candidates (struct ivopts_data *data, struct iv_use *use)
{
  struct tree_niter_desc *niter;

  /* We must know where we exit the loop and how many times does it roll.  */
  niter = niter_for_single_dom_exit (data);
  if (!niter
      || !zero_p (niter->may_be_zero))
    return;

  add_candidate_1 (data, NULL, NULL, false, IP_NORMAL, use, NULL_TREE);
}

/* Adds candidates based on the uses.  */

static void
add_derived_ivs_candidates (struct ivopts_data *data)
{
  unsigned i;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);

      if (!use)
	continue;

      switch (use->type)
	{
	case USE_NONLINEAR_EXPR:
	case USE_COMPARE:
	  /* Just add the ivs based on the value of the iv used here.  */
	  add_iv_value_candidates (data, use->iv, use);
	  break;

	case USE_OUTER:
	  add_iv_value_candidates (data, use->iv, use);

	  /* Additionally, add the pseudocandidate for the possibility to
	     replace the final value by a direct computation.  */
	  add_iv_outer_candidates (data, use);
	  break;

	case USE_ADDRESS:
	  add_address_candidates (data, use->iv, use);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Record important candidates and add them to related_cands bitmaps
   if needed.  */

static void
record_important_candidates (struct ivopts_data *data)
{
  unsigned i;
  struct iv_use *use;

  for (i = 0; i < n_iv_cands (data); i++)
    {
      struct iv_cand *cand = iv_cand (data, i);

      if (cand->important)
	bitmap_set_bit (data->important_candidates, i);
    }

  data->consider_all_candidates = (n_iv_cands (data)
				   <= CONSIDER_ALL_CANDIDATES_BOUND);

  if (data->consider_all_candidates)
    {
      /* We will not need "related_cands" bitmaps in this case,
	 so release them to decrease peak memory consumption.  */
      for (i = 0; i < n_iv_uses (data); i++)
	{
	  use = iv_use (data, i);
	  BITMAP_FREE (use->related_cands);
	}
    }
  else
    {
      /* Add important candidates to the related_cands bitmaps.  */
      for (i = 0; i < n_iv_uses (data); i++)
	bitmap_ior_into (iv_use (data, i)->related_cands,
			 data->important_candidates);
    }
}

/* Finds the candidates for the induction variables.  */

static void
find_iv_candidates (struct ivopts_data *data)
{
  /* Add commonly used ivs.  */
  add_standard_iv_candidates (data);

  /* Add old induction variables.  */
  add_old_ivs_candidates (data);

  /* Add induction variables derived from uses.  */
  add_derived_ivs_candidates (data);

  /* Record the important candidates.  */
  record_important_candidates (data);
}

/* Allocates the data structure mapping the (use, candidate) pairs to costs.
   If consider_all_candidates is true, we use a two-dimensional array, otherwise
   we allocate a simple list to every use.  */

static void
alloc_use_cost_map (struct ivopts_data *data)
{
  unsigned i, size, s, j;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);
      bitmap_iterator bi;

      if (data->consider_all_candidates)
	size = n_iv_cands (data);
      else
	{
	  s = 0;
	  EXECUTE_IF_SET_IN_BITMAP (use->related_cands, 0, j, bi)
	    {
	      s++;
	    }

	  /* Round up to the power of two, so that moduling by it is fast.  */
	  for (size = 1; size < s; size <<= 1)
	    continue;
	}

      use->n_map_members = size;
      use->cost_map = xcalloc (size, sizeof (struct cost_pair));
    }
}

/* Sets cost of (USE, CANDIDATE) pair to COST and record that it depends
   on invariants DEPENDS_ON.  */

static void
set_use_iv_cost (struct ivopts_data *data,
		 struct iv_use *use, struct iv_cand *cand, unsigned cost,
		 bitmap depends_on)
{
  unsigned i, s;

  if (cost == INFTY)
    {
      BITMAP_FREE (depends_on);
      return;
    }

  if (data->consider_all_candidates)
    {
      use->cost_map[cand->id].cand = cand;
      use->cost_map[cand->id].cost = cost;
      use->cost_map[cand->id].depends_on = depends_on;
      return;
    }

  /* n_map_members is a power of two, so this computes modulo.  */
  s = cand->id & (use->n_map_members - 1);
  for (i = s; i < use->n_map_members; i++)
    if (!use->cost_map[i].cand)
      goto found;
  for (i = 0; i < s; i++)
    if (!use->cost_map[i].cand)
      goto found;

  gcc_unreachable ();

found:
  use->cost_map[i].cand = cand;
  use->cost_map[i].cost = cost;
  use->cost_map[i].depends_on = depends_on;
}

/* Gets cost of (USE, CANDIDATE) pair.  */

static struct cost_pair *
get_use_iv_cost (struct ivopts_data *data, struct iv_use *use,
		 struct iv_cand *cand)
{
  unsigned i, s;
  struct cost_pair *ret;

  if (!cand)
    return NULL;

  if (data->consider_all_candidates)
    {
      ret = use->cost_map + cand->id;
      if (!ret->cand)
	return NULL;

      return ret;
    }
      
  /* n_map_members is a power of two, so this computes modulo.  */
  s = cand->id & (use->n_map_members - 1);
  for (i = s; i < use->n_map_members; i++)
    if (use->cost_map[i].cand == cand)
      return use->cost_map + i;

  for (i = 0; i < s; i++)
    if (use->cost_map[i].cand == cand)
      return use->cost_map + i;

  return NULL;
}

/* Returns estimate on cost of computing SEQ.  */

static unsigned
seq_cost (rtx seq)
{
  unsigned cost = 0;
  rtx set;

  for (; seq; seq = NEXT_INSN (seq))
    {
      set = single_set (seq);
      if (set)
	cost += rtx_cost (set, SET);
      else
	cost++;
    }

  return cost;
}

/* Produce DECL_RTL for object obj so it looks like it is stored in memory.  */
static rtx
produce_memory_decl_rtl (tree obj, int *regno)
{
  rtx x;
  if (!obj)
    abort ();
  if (TREE_STATIC (obj) || DECL_EXTERNAL (obj))
    {
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (obj));
      x = gen_rtx_SYMBOL_REF (Pmode, name);
    }
  else
    x = gen_raw_REG (Pmode, (*regno)++);

  return gen_rtx_MEM (DECL_MODE (obj), x);
}

/* Prepares decl_rtl for variables referred in *EXPR_P.  Callback for
   walk_tree.  DATA contains the actual fake register number.  */

static tree
prepare_decl_rtl (tree *expr_p, int *ws, void *data)
{
  tree obj = NULL_TREE;
  rtx x = NULL_RTX;
  int *regno = data;

  switch (TREE_CODE (*expr_p))
    {
    case ADDR_EXPR:
      for (expr_p = &TREE_OPERAND (*expr_p, 0);
	   handled_component_p (*expr_p);
	   expr_p = &TREE_OPERAND (*expr_p, 0))
	continue;
      obj = *expr_p;
      if (DECL_P (obj))
        x = produce_memory_decl_rtl (obj, regno);
      break;

    case SSA_NAME:
      *ws = 0;
      obj = SSA_NAME_VAR (*expr_p);
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
      VARRAY_PUSH_GENERIC_PTR_NOGC (decl_rtl_to_reset, obj);
      SET_DECL_RTL (obj, x);
    }

  return NULL_TREE;
}

/* Determines cost of the computation of EXPR.  */

static unsigned
computation_cost (tree expr)
{
  rtx seq, rslt;
  tree type = TREE_TYPE (expr);
  unsigned cost;
  /* Avoid using hard regs in ways which may be unsupported.  */
  int regno = LAST_VIRTUAL_REGISTER + 1;

  walk_tree (&expr, prepare_decl_rtl, &regno, NULL);
  start_sequence ();
  rslt = expand_expr (expr, NULL_RTX, TYPE_MODE (type), EXPAND_NORMAL);
  seq = get_insns ();
  end_sequence ();

  cost = seq_cost (seq);
  if (GET_CODE (rslt) == MEM)
    cost += address_cost (XEXP (rslt, 0), TYPE_MODE (type));

  return cost;
}

/* Returns variable containing the value of candidate CAND at statement AT.  */

static tree
var_at_stmt (struct loop *loop, struct iv_cand *cand, tree stmt)
{
  if (stmt_after_increment (loop, cand, stmt))
    return cand->var_after;
  else
    return cand->var_before;
}

/* Determines the expression by that USE is expressed from induction variable
   CAND at statement AT in LOOP.  */

static tree
get_computation_at (struct loop *loop,
		    struct iv_use *use, struct iv_cand *cand, tree at)
{
  tree ubase = use->iv->base;
  tree ustep = use->iv->step;
  tree cbase = cand->iv->base;
  tree cstep = cand->iv->step;
  tree utype = TREE_TYPE (ubase), ctype = TREE_TYPE (cbase);
  tree uutype;
  tree expr, delta;
  tree ratio;
  unsigned HOST_WIDE_INT ustepi, cstepi;
  HOST_WIDE_INT ratioi;

  if (TYPE_PRECISION (utype) > TYPE_PRECISION (ctype))
    {
      /* We do not have a precision to express the values of use.  */
      return NULL_TREE;
    }

  expr = var_at_stmt (loop, cand, at);

  if (TREE_TYPE (expr) != ctype)
    {
      /* This may happen with the original ivs.  */
      expr = fold_convert (ctype, expr);
    }

  if (TYPE_UNSIGNED (utype))
    uutype = utype;
  else
    {
      uutype = unsigned_type_for (utype);
      ubase = fold_convert (uutype, ubase);
      ustep = fold_convert (uutype, ustep);
    }

  if (uutype != ctype)
    {
      expr = fold_convert (uutype, expr);
      cbase = fold_convert (uutype, cbase);
      cstep = fold_convert (uutype, cstep);
    }

  if (!cst_and_fits_in_hwi (cstep)
      || !cst_and_fits_in_hwi (ustep))
    return NULL_TREE;

  ustepi = int_cst_value (ustep);
  cstepi = int_cst_value (cstep);

  if (!divide (TYPE_PRECISION (uutype), ustepi, cstepi, &ratioi))
    {
      /* TODO maybe consider case when ustep divides cstep and the ratio is
	 a power of 2 (so that the division is fast to execute)?  We would
	 need to be much more careful with overflows etc. then.  */
      return NULL_TREE;
    }

  /* We may need to shift the value if we are after the increment.  */
  if (stmt_after_increment (loop, cand, at))
    cbase = fold (build2 (PLUS_EXPR, uutype, cbase, cstep));

  /* use = ubase - ratio * cbase + ratio * var.

     In general case ubase + ratio * (var - cbase) could be better (one less
     multiplication), but often it is possible to eliminate redundant parts
     of computations from (ubase - ratio * cbase) term, and if it does not
     happen, fold is able to apply the distributive law to obtain this form
     anyway.  */

  if (ratioi == 1)
    {
      delta = fold (build2 (MINUS_EXPR, uutype, ubase, cbase));
      expr = fold (build2 (PLUS_EXPR, uutype, expr, delta));
    }
  else if (ratioi == -1)
    {
      delta = fold (build2 (PLUS_EXPR, uutype, ubase, cbase));
      expr = fold (build2 (MINUS_EXPR, uutype, delta, expr));
    }
  else
    {
      ratio = build_int_cst_type (uutype, ratioi);
      delta = fold (build2 (MULT_EXPR, uutype, ratio, cbase));
      delta = fold (build2 (MINUS_EXPR, uutype, ubase, delta));
      expr = fold (build2 (MULT_EXPR, uutype, ratio, expr));
      expr = fold (build2 (PLUS_EXPR, uutype, delta, expr));
    }

  return fold_convert (utype, expr);
}

/* Determines the expression by that USE is expressed from induction variable
   CAND in LOOP.  */

static tree
get_computation (struct loop *loop, struct iv_use *use, struct iv_cand *cand)
{
  return get_computation_at (loop, use, cand, use->stmt);
}

/* Returns cost of addition in MODE.  */

static unsigned
add_cost (enum machine_mode mode)
{
  static unsigned costs[NUM_MACHINE_MODES];
  rtx seq;
  unsigned cost;

  if (costs[mode])
    return costs[mode];

  start_sequence ();
  force_operand (gen_rtx_fmt_ee (PLUS, mode,
				 gen_raw_REG (mode, FIRST_PSEUDO_REGISTER),
				 gen_raw_REG (mode, FIRST_PSEUDO_REGISTER + 1)),
		 NULL_RTX);
  seq = get_insns ();
  end_sequence ();

  cost = seq_cost (seq);
  if (!cost)
    cost = 1;

  costs[mode] = cost;
      
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Addition in %s costs %d\n",
	     GET_MODE_NAME (mode), cost);
  return cost;
}

/* Entry in a hashtable of already known costs for multiplication.  */
struct mbc_entry
{
  HOST_WIDE_INT cst;		/* The constant to multiply by.  */
  enum machine_mode mode;	/* In mode.  */
  unsigned cost;		/* The cost.  */
};

/* Counts hash value for the ENTRY.  */

static hashval_t
mbc_entry_hash (const void *entry)
{
  const struct mbc_entry *e = entry;

  return 57 * (hashval_t) e->mode + (hashval_t) (e->cst % 877);
}

/* Compares the hash table entries ENTRY1 and ENTRY2.  */

static int
mbc_entry_eq (const void *entry1, const void *entry2)
{
  const struct mbc_entry *e1 = entry1;
  const struct mbc_entry *e2 = entry2;

  return (e1->mode == e2->mode
	  && e1->cst == e2->cst);
}

/* Returns cost of multiplication by constant CST in MODE.  */

static unsigned
multiply_by_cost (HOST_WIDE_INT cst, enum machine_mode mode)
{
  static htab_t costs;
  struct mbc_entry **cached, act;
  rtx seq;
  unsigned cost;

  if (!costs)
    costs = htab_create (100, mbc_entry_hash, mbc_entry_eq, free);

  act.mode = mode;
  act.cst = cst;
  cached = (struct mbc_entry **) htab_find_slot (costs, &act, INSERT);
  if (*cached)
    return (*cached)->cost;

  *cached = xmalloc (sizeof (struct mbc_entry));
  (*cached)->mode = mode;
  (*cached)->cst = cst;

  start_sequence ();
  expand_mult (mode, gen_raw_REG (mode, FIRST_PSEUDO_REGISTER), GEN_INT (cst),
	       NULL_RTX, 0);
  seq = get_insns ();
  end_sequence ();
  
  cost = seq_cost (seq);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Multiplication by %d in %s costs %d\n",
	     (int) cst, GET_MODE_NAME (mode), cost);

  (*cached)->cost = cost;

  return cost;
}

/* Returns cost of address in shape symbol + var + OFFSET + RATIO * index.
   If SYMBOL_PRESENT is false, symbol is omitted.  If VAR_PRESENT is false,
   variable is omitted.  The created memory accesses MODE.
   
   TODO -- there must be some better way.  This all is quite crude.  */

static unsigned
get_address_cost (bool symbol_present, bool var_present,
		  unsigned HOST_WIDE_INT offset, HOST_WIDE_INT ratio)
{
#define MAX_RATIO 128
  static sbitmap valid_mult;
  static HOST_WIDE_INT rat, off;
  static HOST_WIDE_INT min_offset, max_offset;
  static unsigned costs[2][2][2][2];
  unsigned cost, acost;
  rtx seq, addr, base;
  bool offset_p, ratio_p;
  rtx reg1;
  HOST_WIDE_INT s_offset;
  unsigned HOST_WIDE_INT mask;
  unsigned bits;

  if (!valid_mult)
    {
      HOST_WIDE_INT i;

      reg1 = gen_raw_REG (Pmode, FIRST_PSEUDO_REGISTER);

      addr = gen_rtx_fmt_ee (PLUS, Pmode, reg1, NULL_RTX);
      for (i = 1; i <= 1 << 20; i <<= 1)
	{
	  XEXP (addr, 1) = GEN_INT (i);
	  if (!memory_address_p (Pmode, addr))
	    break;
	}
      max_offset = i >> 1;
      off = max_offset;

      for (i = 1; i <= 1 << 20; i <<= 1)
	{
	  XEXP (addr, 1) = GEN_INT (-i);
	  if (!memory_address_p (Pmode, addr))
	    break;
	}
      min_offset = -(i >> 1);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "get_address_cost:\n");
	  fprintf (dump_file, "  min offset %d\n", (int) min_offset);
	  fprintf (dump_file, "  max offset %d\n", (int) max_offset);
	}

      valid_mult = sbitmap_alloc (2 * MAX_RATIO + 1);
      sbitmap_zero (valid_mult);
      rat = 1;
      addr = gen_rtx_fmt_ee (MULT, Pmode, reg1, NULL_RTX);
      for (i = -MAX_RATIO; i <= MAX_RATIO; i++)
	{
	  XEXP (addr, 1) = GEN_INT (i);
	  if (memory_address_p (Pmode, addr))
	    {
	      SET_BIT (valid_mult, i + MAX_RATIO);
	      rat = i;
	    }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  allowed multipliers:");
	  for (i = -MAX_RATIO; i <= MAX_RATIO; i++)
	    if (TEST_BIT (valid_mult, i + MAX_RATIO))
	      fprintf (dump_file, " %d", (int) i);
	  fprintf (dump_file, "\n");
	  fprintf (dump_file, "\n");
	}
    }

  bits = GET_MODE_BITSIZE (Pmode);
  mask = ~(~(unsigned HOST_WIDE_INT) 0 << (bits - 1) << 1);
  offset &= mask;
  if ((offset >> (bits - 1) & 1))
    offset |= ~mask;
  s_offset = offset;

  cost = 0;
  offset_p = (s_offset != 0
	      && min_offset <= s_offset && s_offset <= max_offset);
  ratio_p = (ratio != 1
	     && -MAX_RATIO <= ratio && ratio <= MAX_RATIO
	     && TEST_BIT (valid_mult, ratio + MAX_RATIO));

  if (ratio != 1 && !ratio_p)
    cost += multiply_by_cost (ratio, Pmode);

  if (s_offset && !offset_p && !symbol_present)
    {
      cost += add_cost (Pmode);
      var_present = true;
    }

  acost = costs[symbol_present][var_present][offset_p][ratio_p];
  if (!acost)
    {
      acost = 0;
      
      addr = gen_raw_REG (Pmode, FIRST_PSEUDO_REGISTER);
      reg1 = gen_raw_REG (Pmode, FIRST_PSEUDO_REGISTER + 1);
      if (ratio_p)
	addr = gen_rtx_fmt_ee (MULT, Pmode, addr, GEN_INT (rat));

      if (var_present)
	addr = gen_rtx_fmt_ee (PLUS, Pmode, addr, reg1);

      if (symbol_present)
	{
	  base = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (""));
	  if (offset_p)
	    base = gen_rtx_fmt_e (CONST, Pmode,
				  gen_rtx_fmt_ee (PLUS, Pmode,
						  base,
						  GEN_INT (off)));
	}
      else if (offset_p)
	base = GEN_INT (off);
      else
	base = NULL_RTX;
    
      if (base)
	addr = gen_rtx_fmt_ee (PLUS, Pmode, addr, base);
  
      start_sequence ();
      addr = memory_address (Pmode, addr);
      seq = get_insns ();
      end_sequence ();
  
      acost = seq_cost (seq);
      acost += address_cost (addr, Pmode);

      if (!acost)
	acost = 1;
      costs[symbol_present][var_present][offset_p][ratio_p] = acost;
    }

  return cost + acost;
}

/* Records invariants in *EXPR_P.  Callback for walk_tree.  DATA contains
   the bitmap to that we should store it.  */

static struct ivopts_data *fd_ivopts_data;
static tree
find_depends (tree *expr_p, int *ws ATTRIBUTE_UNUSED, void *data)
{
  bitmap *depends_on = data;
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

/* Estimates cost of forcing EXPR into a variable.  DEPENDS_ON is a set of the
   invariants the computation depends on.  */

static unsigned
force_var_cost (struct ivopts_data *data,
		tree expr, bitmap *depends_on)
{
  static bool costs_initialized = false;
  static unsigned integer_cost;
  static unsigned symbol_cost;
  static unsigned address_cost;
  tree op0, op1;
  unsigned cost0, cost1, cost;
  enum machine_mode mode;

  if (!costs_initialized)
    {
      tree var = create_tmp_var_raw (integer_type_node, "test_var");
      rtx x = gen_rtx_MEM (DECL_MODE (var),
			   gen_rtx_SYMBOL_REF (Pmode, "test_var"));
      tree addr;
      tree type = build_pointer_type (integer_type_node);

      integer_cost = computation_cost (build_int_cst_type (integer_type_node,
							   2000));

      SET_DECL_RTL (var, x);
      TREE_STATIC (var) = 1;
      addr = build1 (ADDR_EXPR, type, var);
      symbol_cost = computation_cost (addr) + 1;

      address_cost
	= computation_cost (build2 (PLUS_EXPR, type,
				    addr,
				    build_int_cst_type (type, 2000))) + 1;
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "force_var_cost:\n");
	  fprintf (dump_file, "  integer %d\n", (int) integer_cost);
	  fprintf (dump_file, "  symbol %d\n", (int) symbol_cost);
	  fprintf (dump_file, "  address %d\n", (int) address_cost);
	  fprintf (dump_file, "  other %d\n", (int) target_spill_cost);
	  fprintf (dump_file, "\n");
	}

      costs_initialized = true;
    }

  STRIP_NOPS (expr);

  if (depends_on)
    {
      fd_ivopts_data = data;
      walk_tree (&expr, find_depends, depends_on, NULL);
    }

  if (SSA_VAR_P (expr))
    return 0;

  if (TREE_INVARIANT (expr))
    {
      if (TREE_CODE (expr) == INTEGER_CST)
	return integer_cost;

      if (TREE_CODE (expr) == ADDR_EXPR)
	{
	  tree obj = TREE_OPERAND (expr, 0);

	  if (TREE_CODE (obj) == VAR_DECL
	      || TREE_CODE (obj) == PARM_DECL
	      || TREE_CODE (obj) == RESULT_DECL)
	    return symbol_cost;
	}

      return address_cost;
    }

  switch (TREE_CODE (expr))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      op0 = TREE_OPERAND (expr, 0);
      op1 = TREE_OPERAND (expr, 1);
      STRIP_NOPS (op0);
      STRIP_NOPS (op1);

      if (is_gimple_val (op0))
	cost0 = 0;
      else
	cost0 = force_var_cost (data, op0, NULL);

      if (is_gimple_val (op1))
	cost1 = 0;
      else
	cost1 = force_var_cost (data, op1, NULL);

      break;

    default:
      /* Just an arbitrary value, FIXME.  */
      return target_spill_cost;
    }

  mode = TYPE_MODE (TREE_TYPE (expr));
  switch (TREE_CODE (expr))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      cost = add_cost (mode);
      break;

    case MULT_EXPR:
      if (cst_and_fits_in_hwi (op0))
	cost = multiply_by_cost (int_cst_value (op0), mode);
      else if (cst_and_fits_in_hwi (op1))
	cost = multiply_by_cost (int_cst_value (op1), mode);
      else
	return target_spill_cost;
      break;

    default:
      gcc_unreachable ();
    }

  cost += cost0;
  cost += cost1;

  /* Bound the cost by target_spill_cost.  The parts of complicated
     computations often are either loop invariant or at least can
     be shared between several iv uses, so letting this grow without
     limits would not give reasonable results.  */
  return cost < target_spill_cost ? cost : target_spill_cost;
}

/* Estimates cost of expressing address ADDR  as var + symbol + offset.  The
   value of offset is added to OFFSET, SYMBOL_PRESENT and VAR_PRESENT are set
   to false if the corresponding part is missing.  DEPENDS_ON is a set of the
   invariants the computation depends on.  */

static unsigned
split_address_cost (struct ivopts_data *data,
		    tree addr, bool *symbol_present, bool *var_present,
		    unsigned HOST_WIDE_INT *offset, bitmap *depends_on)
{
  tree core;
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  tree toffset;
  enum machine_mode mode;
  int unsignedp, volatilep;
  
  core = get_inner_reference (addr, &bitsize, &bitpos, &toffset, &mode,
			      &unsignedp, &volatilep, false);

  if (toffset != 0
      || bitpos % BITS_PER_UNIT != 0
      || TREE_CODE (core) != VAR_DECL)
    {
      *symbol_present = false;
      *var_present = true;
      fd_ivopts_data = data;
      walk_tree (&addr, find_depends, depends_on, NULL);
      return target_spill_cost;
    }

  *offset += bitpos / BITS_PER_UNIT;
  if (TREE_STATIC (core)
      || DECL_EXTERNAL (core))
    {
      *symbol_present = true;
      *var_present = false;
      return 0;
    }
      
  *symbol_present = false;
  *var_present = true;
  return 0;
}

/* Estimates cost of expressing difference of addresses E1 - E2 as
   var + symbol + offset.  The value of offset is added to OFFSET,
   SYMBOL_PRESENT and VAR_PRESENT are set to false if the corresponding
   part is missing.  DEPENDS_ON is a set of the invariants the computation
   depends on.  */

static unsigned
ptr_difference_cost (struct ivopts_data *data,
		     tree e1, tree e2, bool *symbol_present, bool *var_present,
		     unsigned HOST_WIDE_INT *offset, bitmap *depends_on)
{
  HOST_WIDE_INT diff = 0;
  unsigned cost;

  gcc_assert (TREE_CODE (e1) == ADDR_EXPR);

  if (ptr_difference_const (e1, e2, &diff))
    {
      *offset += diff;
      *symbol_present = false;
      *var_present = false;
      return 0;
    }

  if (e2 == integer_zero_node)
    return split_address_cost (data, TREE_OPERAND (e1, 0),
			       symbol_present, var_present, offset, depends_on);

  *symbol_present = false;
  *var_present = true;
  
  cost = force_var_cost (data, e1, depends_on);
  cost += force_var_cost (data, e2, depends_on);
  cost += add_cost (Pmode);

  return cost;
}

/* Estimates cost of expressing difference E1 - E2 as
   var + symbol + offset.  The value of offset is added to OFFSET,
   SYMBOL_PRESENT and VAR_PRESENT are set to false if the corresponding
   part is missing.  DEPENDS_ON is a set of the invariants the computation
   depends on.  */

static unsigned
difference_cost (struct ivopts_data *data,
		 tree e1, tree e2, bool *symbol_present, bool *var_present,
		 unsigned HOST_WIDE_INT *offset, bitmap *depends_on)
{
  unsigned cost;
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (e1));
  unsigned HOST_WIDE_INT off1, off2;

  e1 = strip_offset (e1, false, &off1);
  e2 = strip_offset (e2, false, &off2);
  *offset += off1 - off2;

  STRIP_NOPS (e1);
  STRIP_NOPS (e2);

  if (TREE_CODE (e1) == ADDR_EXPR)
    return ptr_difference_cost (data, e1, e2, symbol_present, var_present, offset,
				depends_on);
  *symbol_present = false;

  if (operand_equal_p (e1, e2, 0))
    {
      *var_present = false;
      return 0;
    }
  *var_present = true;
  if (zero_p (e2))
    return force_var_cost (data, e1, depends_on);

  if (zero_p (e1))
    {
      cost = force_var_cost (data, e2, depends_on);
      cost += multiply_by_cost (-1, mode);

      return cost;
    }

  cost = force_var_cost (data, e1, depends_on);
  cost += force_var_cost (data, e2, depends_on);
  cost += add_cost (mode);

  return cost;
}

/* Determines the cost of the computation by that USE is expressed
   from induction variable CAND.  If ADDRESS_P is true, we just need
   to create an address from it, otherwise we want to get it into
   register.  A set of invariants we depend on is stored in
   DEPENDS_ON.  AT is the statement at that the value is computed.  */

static unsigned
get_computation_cost_at (struct ivopts_data *data,
			 struct iv_use *use, struct iv_cand *cand,
			 bool address_p, bitmap *depends_on, tree at)
{
  tree ubase = use->iv->base, ustep = use->iv->step;
  tree cbase, cstep;
  tree utype = TREE_TYPE (ubase), ctype;
  unsigned HOST_WIDE_INT ustepi, cstepi, offset = 0;
  HOST_WIDE_INT ratio, aratio;
  bool var_present, symbol_present;
  unsigned cost = 0, n_sums;

  *depends_on = NULL;

  /* Only consider real candidates.  */
  if (!cand->iv)
    return INFTY;

  cbase = cand->iv->base;
  cstep = cand->iv->step;
  ctype = TREE_TYPE (cbase);

  if (TYPE_PRECISION (utype) > TYPE_PRECISION (ctype))
    {
      /* We do not have a precision to express the values of use.  */
      return INFTY;
    }

  if (address_p)
    {
      /* Do not try to express address of an object with computation based
	 on address of a different object.  This may cause problems in rtl
	 level alias analysis (that does not expect this to be happening,
	 as this is illegal in C), and would be unlikely to be useful
	 anyway.  */
      if (use->iv->base_object
	  && cand->iv->base_object
	  && !operand_equal_p (use->iv->base_object, cand->iv->base_object, 0))
	return INFTY;
    }

  if (!cst_and_fits_in_hwi (ustep)
      || !cst_and_fits_in_hwi (cstep))
    return INFTY;

  if (TREE_CODE (ubase) == INTEGER_CST
      && !cst_and_fits_in_hwi (ubase))
    goto fallback;

  if (TREE_CODE (cbase) == INTEGER_CST
      && !cst_and_fits_in_hwi (cbase))
    goto fallback;
    
  ustepi = int_cst_value (ustep);
  cstepi = int_cst_value (cstep);

  if (TYPE_PRECISION (utype) != TYPE_PRECISION (ctype))
    {
      /* TODO -- add direct handling of this case.  */
      goto fallback;
    }

  if (!divide (TYPE_PRECISION (utype), ustepi, cstepi, &ratio))
    return INFTY;

  /* use = ubase + ratio * (var - cbase).  If either cbase is a constant
     or ratio == 1, it is better to handle this like
     
     ubase - ratio * cbase + ratio * var
     
     (also holds in the case ratio == -1, TODO.  */

  if (TREE_CODE (cbase) == INTEGER_CST)
    {
      offset = - ratio * int_cst_value (cbase); 
      cost += difference_cost (data,
			       ubase, integer_zero_node,
			       &symbol_present, &var_present, &offset,
			       depends_on);
    }
  else if (ratio == 1)
    {
      cost += difference_cost (data,
			       ubase, cbase,
			       &symbol_present, &var_present, &offset,
			       depends_on);
    }
  else
    {
      cost += force_var_cost (data, cbase, depends_on);
      cost += add_cost (TYPE_MODE (ctype));
      cost += difference_cost (data,
			       ubase, integer_zero_node,
			       &symbol_present, &var_present, &offset,
			       depends_on);
    }

  /* If we are after the increment, the value of the candidate is higher by
     one iteration.  */
  if (stmt_after_increment (data->current_loop, cand, at))
    offset -= ratio * cstepi;

  /* Now the computation is in shape symbol + var1 + const + ratio * var2.
     (symbol/var/const parts may be omitted).  If we are looking for an address,
     find the cost of addressing this.  */
  if (address_p)
    return cost + get_address_cost (symbol_present, var_present, offset, ratio);

  /* Otherwise estimate the costs for computing the expression.  */
  aratio = ratio > 0 ? ratio : -ratio;
  if (!symbol_present && !var_present && !offset)
    {
      if (ratio != 1)
	cost += multiply_by_cost (ratio, TYPE_MODE (ctype));

      return cost;
    }

  if (aratio != 1)
    cost += multiply_by_cost (aratio, TYPE_MODE (ctype));

  n_sums = 1;
  if (var_present
      /* Symbol + offset should be compile-time computable.  */
      && (symbol_present || offset))
    n_sums++;

  return cost + n_sums * add_cost (TYPE_MODE (ctype));

fallback:
  {
    /* Just get the expression, expand it and measure the cost.  */
    tree comp = get_computation_at (data->current_loop, use, cand, at);

    if (!comp)
      return INFTY;

    if (address_p)
      comp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (comp)), comp);

    return computation_cost (comp);
  }
}

/* Determines the cost of the computation by that USE is expressed
   from induction variable CAND.  If ADDRESS_P is true, we just need
   to create an address from it, otherwise we want to get it into
   register.  A set of invariants we depend on is stored in
   DEPENDS_ON.  */

static unsigned
get_computation_cost (struct ivopts_data *data,
		      struct iv_use *use, struct iv_cand *cand,
		      bool address_p, bitmap *depends_on)
{
  return get_computation_cost_at (data,
				  use, cand, address_p, depends_on, use->stmt);
}

/* Determines cost of basing replacement of USE on CAND in a generic
   expression.  */

static bool
determine_use_iv_cost_generic (struct ivopts_data *data,
			       struct iv_use *use, struct iv_cand *cand)
{
  bitmap depends_on;
  unsigned cost;

  /* The simple case first -- if we need to express value of the preserved
     original biv, the cost is 0.  This also prevents us from counting the
     cost of increment twice -- once at this use and once in the cost of
     the candidate.  */
  if (cand->pos == IP_ORIGINAL
      && cand->incremented_at == use->stmt)
    {
      set_use_iv_cost (data, use, cand, 0, NULL);
      return true;
    }

  cost = get_computation_cost (data, use, cand, false, &depends_on);
  set_use_iv_cost (data, use, cand, cost, depends_on);

  return cost != INFTY;
}

/* Determines cost of basing replacement of USE on CAND in an address.  */

static bool
determine_use_iv_cost_address (struct ivopts_data *data,
			       struct iv_use *use, struct iv_cand *cand)
{
  bitmap depends_on;
  unsigned cost = get_computation_cost (data, use, cand, true, &depends_on);

  set_use_iv_cost (data, use, cand, cost, depends_on);

  return cost != INFTY;
}

/* Computes value of induction variable IV in iteration NITER.  */

static tree
iv_value (struct iv *iv, tree niter)
{
  tree val;
  tree type = TREE_TYPE (iv->base);

  niter = fold_convert (type, niter);
  val = fold (build2 (MULT_EXPR, type, iv->step, niter));

  return fold (build2 (PLUS_EXPR, type, iv->base, val));
}

/* Computes value of candidate CAND at position AT in iteration NITER.  */

static tree
cand_value_at (struct loop *loop, struct iv_cand *cand, tree at, tree niter)
{
  tree val = iv_value (cand->iv, niter);
  tree type = TREE_TYPE (cand->iv->base);

  if (stmt_after_increment (loop, cand, at))
    val = fold (build2 (PLUS_EXPR, type, val, cand->iv->step));

  return val;
}

/* Returns period of induction variable iv.  */

static tree
iv_period (struct iv *iv)
{
  tree step = iv->step, period, type;
  tree pow2div;

  gcc_assert (step && TREE_CODE (step) == INTEGER_CST);

  /* Period of the iv is gcd (step, type range).  Since type range is power
     of two, it suffices to determine the maximum power of two that divides
     step.  */
  pow2div = num_ending_zeros (step);
  type = unsigned_type_for (TREE_TYPE (step));

  period = build_low_bits_mask (type,
				(TYPE_PRECISION (type)
				 - tree_low_cst (pow2div, 1)));

  return period;
}

/* Check whether it is possible to express the condition in USE by comparison
   of candidate CAND.  If so, store the comparison code to COMPARE and the
   value compared with to BOUND.  */

static bool
may_eliminate_iv (struct ivopts_data *data,
		  struct iv_use *use, struct iv_cand *cand,
		  enum tree_code *compare, tree *bound)
{
  basic_block ex_bb;
  edge exit;
  struct tree_niter_desc *niter;
  tree nit, nit_type;
  tree wider_type, period, per_type;
  struct loop *loop = data->current_loop;
  
  /* For now works only for exits that dominate the loop latch.  TODO -- extend
     for other conditions inside loop body.  */
  ex_bb = bb_for_stmt (use->stmt);
  if (use->stmt != last_stmt (ex_bb)
      || TREE_CODE (use->stmt) != COND_EXPR)
    return false;
  if (!dominated_by_p (CDI_DOMINATORS, loop->latch, ex_bb))
    return false;

  exit = EDGE_SUCC (ex_bb, 0);
  if (flow_bb_inside_loop_p (loop, exit->dest))
    exit = EDGE_SUCC (ex_bb, 1);
  if (flow_bb_inside_loop_p (loop, exit->dest))
    return false;

  niter = niter_for_exit (data, exit);
  if (!niter
      || !zero_p (niter->may_be_zero))
    return false;

  nit = niter->niter;
  nit_type = TREE_TYPE (nit);

  /* Determine whether we may use the variable to test whether niter iterations
     elapsed.  This is the case iff the period of the induction variable is
     greater than the number of iterations.  */
  period = iv_period (cand->iv);
  if (!period)
    return false;
  per_type = TREE_TYPE (period);

  wider_type = TREE_TYPE (period);
  if (TYPE_PRECISION (nit_type) < TYPE_PRECISION (per_type))
    wider_type = per_type;
  else
    wider_type = nit_type;

  if (!integer_nonzerop (fold (build2 (GE_EXPR, boolean_type_node,
				       fold_convert (wider_type, period),
				       fold_convert (wider_type, nit)))))
    return false;

  if (exit->flags & EDGE_TRUE_VALUE)
    *compare = EQ_EXPR;
  else
    *compare = NE_EXPR;

  *bound = cand_value_at (loop, cand, use->stmt, nit);
  return true;
}

/* Determines cost of basing replacement of USE on CAND in a condition.  */

static bool
determine_use_iv_cost_condition (struct ivopts_data *data,
				 struct iv_use *use, struct iv_cand *cand)
{
  tree bound;
  enum tree_code compare;

  /* Only consider real candidates.  */
  if (!cand->iv)
    {
      set_use_iv_cost (data, use, cand, INFTY, NULL);
      return false;
    }

  if (may_eliminate_iv (data, use, cand, &compare, &bound))
    {
      bitmap depends_on = NULL;
      unsigned cost = force_var_cost (data, bound, &depends_on);

      set_use_iv_cost (data, use, cand, cost, depends_on);
      return cost != INFTY;
    }

  /* The induction variable elimination failed; just express the original
     giv.  If it is compared with an invariant, note that we cannot get
     rid of it.  */
  if (TREE_CODE (*use->op_p) == SSA_NAME)
    record_invariant (data, *use->op_p, true);
  else
    {
      record_invariant (data, TREE_OPERAND (*use->op_p, 0), true);
      record_invariant (data, TREE_OPERAND (*use->op_p, 1), true);
    }

  return determine_use_iv_cost_generic (data, use, cand);
}

/* Checks whether it is possible to replace the final value of USE by
   a direct computation.  If so, the formula is stored to *VALUE.  */

static bool
may_replace_final_value (struct ivopts_data *data, struct iv_use *use,
			 tree *value)
{
  struct loop *loop = data->current_loop;
  edge exit;
  struct tree_niter_desc *niter;

  exit = single_dom_exit (loop);
  if (!exit)
    return false;

  gcc_assert (dominated_by_p (CDI_DOMINATORS, exit->src,
			      bb_for_stmt (use->stmt)));

  niter = niter_for_single_dom_exit (data);
  if (!niter
      || !zero_p (niter->may_be_zero))
    return false;

  *value = iv_value (use->iv, niter->niter);

  return true;
}

/* Determines cost of replacing final value of USE using CAND.  */

static bool
determine_use_iv_cost_outer (struct ivopts_data *data,
			     struct iv_use *use, struct iv_cand *cand)
{
  bitmap depends_on;
  unsigned cost;
  edge exit;
  tree value;
  struct loop *loop = data->current_loop;

  /* The simple case first -- if we need to express value of the preserved
     original biv, the cost is 0.  This also prevents us from counting the
     cost of increment twice -- once at this use and once in the cost of
     the candidate.  */
  if (cand->pos == IP_ORIGINAL
      && cand->incremented_at == use->stmt)
    {
      set_use_iv_cost (data, use, cand, 0, NULL);
      return true;
    }

  if (!cand->iv)
    {
      if (!may_replace_final_value (data, use, &value))
	{
	  set_use_iv_cost (data, use, cand, INFTY, NULL);
	  return false;
	}

      depends_on = NULL;
      cost = force_var_cost (data, value, &depends_on);

      cost /= AVG_LOOP_NITER (loop);

      set_use_iv_cost (data, use, cand, cost, depends_on);
      return cost != INFTY;
    }

  exit = single_dom_exit (loop);
  if (exit)
    {
      /* If there is just a single exit, we may use value of the candidate
	 after we take it to determine the value of use.  */
      cost = get_computation_cost_at (data, use, cand, false, &depends_on,
				      last_stmt (exit->src));
      if (cost != INFTY)
	cost /= AVG_LOOP_NITER (loop);
    }
  else
    {
      /* Otherwise we just need to compute the iv.  */
      cost = get_computation_cost (data, use, cand, false, &depends_on);
    }
				   
  set_use_iv_cost (data, use, cand, cost, depends_on);

  return cost != INFTY;
}

/* Determines cost of basing replacement of USE on CAND.  Returns false
   if USE cannot be based on CAND.  */

static bool
determine_use_iv_cost (struct ivopts_data *data,
		       struct iv_use *use, struct iv_cand *cand)
{
  switch (use->type)
    {
    case USE_NONLINEAR_EXPR:
      return determine_use_iv_cost_generic (data, use, cand);

    case USE_OUTER:
      return determine_use_iv_cost_outer (data, use, cand);

    case USE_ADDRESS:
      return determine_use_iv_cost_address (data, use, cand);

    case USE_COMPARE:
      return determine_use_iv_cost_condition (data, use, cand);

    default:
      gcc_unreachable ();
    }
}

/* Determines costs of basing the use of the iv on an iv candidate.  */

static void
determine_use_iv_costs (struct ivopts_data *data)
{
  unsigned i, j;
  struct iv_use *use;
  struct iv_cand *cand;
  bitmap to_clear = BITMAP_ALLOC (NULL);

  alloc_use_cost_map (data);

  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);

      if (data->consider_all_candidates)
	{
	  for (j = 0; j < n_iv_cands (data); j++)
	    {
	      cand = iv_cand (data, j);
	      determine_use_iv_cost (data, use, cand);
	    }
	}
      else
	{
	  bitmap_iterator bi;

	  EXECUTE_IF_SET_IN_BITMAP (use->related_cands, 0, j, bi)
	    {
	      cand = iv_cand (data, j);
	      if (!determine_use_iv_cost (data, use, cand))
		bitmap_set_bit (to_clear, j);
	    }

	  /* Remove the candidates for that the cost is infinite from
	     the list of related candidates.  */
	  bitmap_and_compl_into (use->related_cands, to_clear);
	  bitmap_clear (to_clear);
	}
    }

  BITMAP_FREE (to_clear);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Use-candidate costs:\n");

      for (i = 0; i < n_iv_uses (data); i++)
	{
	  use = iv_use (data, i);

	  fprintf (dump_file, "Use %d:\n", i);
	  fprintf (dump_file, "  cand\tcost\tdepends on\n");
	  for (j = 0; j < use->n_map_members; j++)
	    {
	      if (!use->cost_map[j].cand
		  || use->cost_map[j].cost == INFTY)
		continue;

	      fprintf (dump_file, "  %d\t%d\t",
		       use->cost_map[j].cand->id,
		       use->cost_map[j].cost);
	      if (use->cost_map[j].depends_on)
		bitmap_print (dump_file,
			      use->cost_map[j].depends_on, "","");
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
  unsigned cost_base, cost_step;
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
  cost_step = add_cost (TYPE_MODE (TREE_TYPE (base)));

  cand->cost = cost_step + cost_base / AVG_LOOP_NITER (current_loop);

  /* Prefer the original iv unless we may gain something by replacing it.  */
  if (cand->pos == IP_ORIGINAL)
    cand->cost--;
  
  /* Prefer not to insert statements into latch unless there are some
     already (so that we do not create unnecessary jumps).  */
  if (cand->pos == IP_END
      && empty_block_p (ip_end_pos (data->current_loop)))
    cand->cost++;
}

/* Determines costs of computation of the candidates.  */

static void
determine_iv_costs (struct ivopts_data *data)
{
  unsigned i;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Candidate costs:\n");
      fprintf (dump_file, "  cand\tcost\n");
    }

  for (i = 0; i < n_iv_cands (data); i++)
    {
      struct iv_cand *cand = iv_cand (data, i);

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
  return global_cost_for_size (size,
			       loop_data (data->current_loop)->regs_used,
			       n_iv_uses (data));
}

/* For each size of the induction variable set determine the penalty.  */

static void
determine_set_costs (struct ivopts_data *data)
{
  unsigned j, n;
  tree phi, op;
  struct loop *loop = data->current_loop;
  bitmap_iterator bi;

  /* We use the following model (definitely improvable, especially the
     cost function -- TODO):

     We estimate the number of registers available (using MD data), name it A.

     We estimate the number of registers used by the loop, name it U.  This
     number is obtained as the number of loop phi nodes (not counting virtual
     registers and bivs) + the number of variables from outside of the loop.

     We set a reserve R (free regs that are used for temporary computations,
     etc.).  For now the reserve is a constant 3.

     Let I be the number of induction variables.
     
     -- if U + I + R <= A, the cost is I * SMALL_COST (just not to encourage
	make a lot of ivs without a reason).
     -- if A - R < U + I <= A, the cost is I * PRES_COST
     -- if U + I > A, the cost is I * PRES_COST and
        number of uses * SPILL_COST * (U + I - A) / (U + I) is added.  */

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Global costs:\n");
      fprintf (dump_file, "  target_avail_regs %d\n", target_avail_regs);
      fprintf (dump_file, "  target_small_cost %d\n", target_small_cost);
      fprintf (dump_file, "  target_pres_cost %d\n", target_pres_cost);
      fprintf (dump_file, "  target_spill_cost %d\n", target_spill_cost);
    }

  n = 0;
  for (phi = phi_nodes (loop->header); phi; phi = PHI_CHAIN (phi))
    {
      op = PHI_RESULT (phi);

      if (!is_gimple_reg (op))
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

  loop_data (loop)->regs_used = n;
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
  if (!a)
    return false;

  if (!b)
    return true;

  if (a->cost < b->cost)
    return true;

  if (a->cost > b->cost)
    return false;

  /* In case the costs are the same, prefer the cheaper candidate.  */
  if (a->cand->cost < b->cand->cost)
    return true;

  return false;
}

/* Computes the cost field of IVS structure.  */

static void
iv_ca_recount_cost (struct ivopts_data *data, struct iv_ca *ivs)
{
  unsigned cost = 0;

  cost += ivs->cand_use_cost;
  cost += ivs->cand_cost;
  cost += ivopts_global_cost_for_size (data, ivs->n_regs);

  ivs->cost = cost;
}

/* Set USE not to be expressed by any candidate in IVS.  */

static void
iv_ca_set_no_cp (struct ivopts_data *data, struct iv_ca *ivs,
		 struct iv_use *use)
{
  unsigned uid = use->id, cid, iid;
  bitmap deps;
  struct cost_pair *cp;
  bitmap_iterator bi;

  cp = ivs->cand_for_use[uid];
  if (!cp)
    return;
  cid = cp->cand->id;

  ivs->bad_uses++;
  ivs->cand_for_use[uid] = NULL;
  ivs->n_cand_uses[cid]--;

  if (ivs->n_cand_uses[cid] == 0)
    {
      bitmap_clear_bit (ivs->cands, cid);
      /* Do not count the pseudocandidates.  */
      if (cp->cand->iv)
	ivs->n_regs--;
      ivs->n_cands--;
      ivs->cand_cost -= cp->cand->cost;
    }

  ivs->cand_use_cost -= cp->cost;

  deps = cp->depends_on;

  if (deps)
    {
      EXECUTE_IF_SET_IN_BITMAP (deps, 0, iid, bi)
	{
	  ivs->n_invariant_uses[iid]--;
	  if (ivs->n_invariant_uses[iid] == 0)
	    ivs->n_regs--;
	}
    }

  iv_ca_recount_cost (data, ivs);
}

/* Set cost pair for USE in set IVS to CP.  */

static void
iv_ca_set_cp (struct ivopts_data *data, struct iv_ca *ivs,
	      struct iv_use *use, struct cost_pair *cp)
{
  unsigned uid = use->id, cid, iid;
  bitmap deps;
  bitmap_iterator bi;

  if (ivs->cand_for_use[uid] == cp)
    return;

  if (ivs->cand_for_use[uid])
    iv_ca_set_no_cp (data, ivs, use);

  if (cp)
    {
      cid = cp->cand->id;

      ivs->bad_uses--;
      ivs->cand_for_use[uid] = cp;
      ivs->n_cand_uses[cid]++;
      if (ivs->n_cand_uses[cid] == 1)
	{
	  bitmap_set_bit (ivs->cands, cid);
	  /* Do not count the pseudocandidates.  */
	  if (cp->cand->iv)
	    ivs->n_regs++;
	  ivs->n_cands++;
	  ivs->cand_cost += cp->cand->cost;
	}

      ivs->cand_use_cost += cp->cost;

      deps = cp->depends_on;

      if (deps)
	{
	  EXECUTE_IF_SET_IN_BITMAP (deps, 0, iid, bi)
	    {
	      ivs->n_invariant_uses[iid]++;
	      if (ivs->n_invariant_uses[iid] == 1)
		ivs->n_regs++;
	    }
	}

      iv_ca_recount_cost (data, ivs);
    }
}

/* Extend set IVS by expressing USE by some of the candidates in it
   if possible.  */

static void
iv_ca_add_use (struct ivopts_data *data, struct iv_ca *ivs,
	       struct iv_use *use)
{
  struct cost_pair *best_cp = NULL, *cp;
  bitmap_iterator bi;
  unsigned i;

  gcc_assert (ivs->upto >= use->id);

  if (ivs->upto == use->id)
    {
      ivs->upto++;
      ivs->bad_uses++;
    }

  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, i, bi)
    {
      cp = get_use_iv_cost (data, use, iv_cand (data, i));

      if (cheaper_cost_pair (cp, best_cp))
	best_cp = cp;
    }

  iv_ca_set_cp (data, ivs, use, best_cp);
}

/* Get cost for assignment IVS.  */

static unsigned
iv_ca_cost (struct iv_ca *ivs)
{
  return (ivs->bad_uses ? INFTY : ivs->cost);
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

/* Creates change of expressing USE by NEW_CP instead of OLD_CP and chains
   it before NEXT_CHANGE.  */

static struct iv_ca_delta *
iv_ca_delta_add (struct iv_use *use, struct cost_pair *old_cp,
		 struct cost_pair *new_cp, struct iv_ca_delta *next_change)
{
  struct iv_ca_delta *change = xmalloc (sizeof (struct iv_ca_delta));

  change->use = use;
  change->old_cp = old_cp;
  change->new_cp = new_cp;
  change->next_change = next_change;

  return change;
}

/* Joins two lists of changes L1 and L2.  Destructive -- old lists
   are rewritten.   */

static struct iv_ca_delta *
iv_ca_delta_join (struct iv_ca_delta *l1, struct iv_ca_delta *l2)
{
  struct iv_ca_delta *last;

  if (!l2)
    return l1;

  if (!l1)
    return l2;

  for (last = l1; last->next_change; last = last->next_change)
    continue;
  last->next_change = l2;

  return l1;
}

/* Returns candidate by that USE is expressed in IVS.  */

static struct cost_pair *
iv_ca_cand_for_use (struct iv_ca *ivs, struct iv_use *use)
{
  return ivs->cand_for_use[use->id];
}

/* Reverse the list of changes DELTA, forming the inverse to it.  */

static struct iv_ca_delta *
iv_ca_delta_reverse (struct iv_ca_delta *delta)
{
  struct iv_ca_delta *act, *next, *prev = NULL;
  struct cost_pair *tmp;

  for (act = delta; act; act = next)
    {
      next = act->next_change;
      act->next_change = prev;
      prev = act;

      tmp = act->old_cp;
      act->old_cp = act->new_cp;
      act->new_cp = tmp;
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

  for (act = delta; act; act = act->next_change)
    {
      from = act->old_cp;
      to = act->new_cp;
      gcc_assert (iv_ca_cand_for_use (ivs, act->use) == from);
      iv_ca_set_cp (data, ivs, act->use, to);
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
      next = act->next_change;
      free (act);
    }

  *delta = NULL;
}

/* Allocates new iv candidates assignment.  */

static struct iv_ca *
iv_ca_new (struct ivopts_data *data)
{
  struct iv_ca *nw = xmalloc (sizeof (struct iv_ca));

  nw->upto = 0;
  nw->bad_uses = 0;
  nw->cand_for_use = xcalloc (n_iv_uses (data), sizeof (struct cost_pair *));
  nw->n_cand_uses = xcalloc (n_iv_cands (data), sizeof (unsigned));
  nw->cands = BITMAP_ALLOC (NULL);
  nw->n_cands = 0;
  nw->n_regs = 0;
  nw->cand_use_cost = 0;
  nw->cand_cost = 0;
  nw->n_invariant_uses = xcalloc (data->max_inv_id + 1, sizeof (unsigned));
  nw->cost = 0;

  return nw;
}

/* Free memory occupied by the set IVS.  */

static void
iv_ca_free (struct iv_ca **ivs)
{
  free ((*ivs)->cand_for_use);
  free ((*ivs)->n_cand_uses);
  BITMAP_FREE ((*ivs)->cands);
  free ((*ivs)->n_invariant_uses);
  free (*ivs);
  *ivs = NULL;
}

/* Dumps IVS to FILE.  */

static void
iv_ca_dump (struct ivopts_data *data, FILE *file, struct iv_ca *ivs)
{
  const char *pref = "  invariants ";
  unsigned i;

  fprintf (file, "  cost %d\n", iv_ca_cost (ivs));
  bitmap_print (file, ivs->cands, "  candidates ","\n");

  for (i = 1; i <= data->max_inv_id; i++)
    if (ivs->n_invariant_uses[i])
      {
	fprintf (file, "%s%d", pref, i);
	pref = ", ";
      }
  fprintf (file, "\n");
}

/* Try changing candidate in IVS to CAND for each use.  Return cost of the
   new set, and store differences in DELTA.  Number of induction variables
   in the new set is stored to N_IVS.  */

static unsigned
iv_ca_extend (struct ivopts_data *data, struct iv_ca *ivs,
	      struct iv_cand *cand, struct iv_ca_delta **delta,
	      unsigned *n_ivs)
{
  unsigned i, cost;
  struct iv_use *use;
  struct cost_pair *old_cp, *new_cp;

  *delta = NULL;
  for (i = 0; i < ivs->upto; i++)
    {
      use = iv_use (data, i);
      old_cp = iv_ca_cand_for_use (ivs, use);

      if (old_cp
	  && old_cp->cand == cand)
	continue;

      new_cp = get_use_iv_cost (data, use, cand);
      if (!new_cp)
	continue;

      if (!iv_ca_has_deps (ivs, new_cp))
	continue;
      
      if (!cheaper_cost_pair (new_cp, old_cp))
	continue;

      *delta = iv_ca_delta_add (use, old_cp, new_cp, *delta);
    }

  iv_ca_delta_commit (data, ivs, *delta, true);
  cost = iv_ca_cost (ivs);
  if (n_ivs)
    *n_ivs = iv_ca_n_cands (ivs);
  iv_ca_delta_commit (data, ivs, *delta, false);

  return cost;
}

/* Try narrowing set IVS by removing CAND.  Return the cost of
   the new set and store the differences in DELTA.  */

static unsigned
iv_ca_narrow (struct ivopts_data *data, struct iv_ca *ivs,
	      struct iv_cand *cand, struct iv_ca_delta **delta)
{
  unsigned i, ci;
  struct iv_use *use;
  struct cost_pair *old_cp, *new_cp, *cp;
  bitmap_iterator bi;
  struct iv_cand *cnd;
  unsigned cost;

  *delta = NULL;
  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);

      old_cp = iv_ca_cand_for_use (ivs, use);
      if (old_cp->cand != cand)
	continue;

      new_cp = NULL;

      if (data->consider_all_candidates)
	{
	  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, ci, bi)
	    {
	      if (ci == cand->id)
		continue;

	      cnd = iv_cand (data, ci);

	      cp = get_use_iv_cost (data, use, cnd);
	      if (!cp)
		continue;
	      if (!iv_ca_has_deps (ivs, cp))
		continue;
      
	      if (!cheaper_cost_pair (cp, new_cp))
		continue;

	      new_cp = cp;
	    }
	}
      else
	{
	  EXECUTE_IF_AND_IN_BITMAP (use->related_cands, ivs->cands, 0, ci, bi)
	    {
	      if (ci == cand->id)
		continue;

	      cnd = iv_cand (data, ci);

	      cp = get_use_iv_cost (data, use, cnd);
	      if (!cp)
		continue;
	      if (!iv_ca_has_deps (ivs, cp))
		continue;
      
	      if (!cheaper_cost_pair (cp, new_cp))
		continue;

	      new_cp = cp;
	    }
	}

      if (!new_cp)
	{
	  iv_ca_delta_free (delta);
	  return INFTY;
	}

      *delta = iv_ca_delta_add (use, old_cp, new_cp, *delta);
    }

  iv_ca_delta_commit (data, ivs, *delta, true);
  cost = iv_ca_cost (ivs);
  iv_ca_delta_commit (data, ivs, *delta, false);

  return cost;
}

/* Try optimizing the set of candidates IVS by removing candidates different
   from to EXCEPT_CAND from it.  Return cost of the new set, and store
   differences in DELTA.  */

static unsigned
iv_ca_prune (struct ivopts_data *data, struct iv_ca *ivs,
	     struct iv_cand *except_cand, struct iv_ca_delta **delta)
{
  bitmap_iterator bi;
  struct iv_ca_delta *act_delta, *best_delta;
  unsigned i, best_cost, acost;
  struct iv_cand *cand;

  best_delta = NULL;
  best_cost = iv_ca_cost (ivs);

  EXECUTE_IF_SET_IN_BITMAP (ivs->cands, 0, i, bi)
    {
      cand = iv_cand (data, i);

      if (cand == except_cand)
	continue;

      acost = iv_ca_narrow (data, ivs, cand, &act_delta);

      if (acost < best_cost)
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

/* Tries to extend the sets IVS in the best possible way in order
   to express the USE.  */

static bool
try_add_cand_for (struct ivopts_data *data, struct iv_ca *ivs,
		  struct iv_use *use)
{
  unsigned best_cost, act_cost;
  unsigned i;
  bitmap_iterator bi;
  struct iv_cand *cand;
  struct iv_ca_delta *best_delta = NULL, *act_delta;
  struct cost_pair *cp;

  iv_ca_add_use (data, ivs, use);
  best_cost = iv_ca_cost (ivs);

  cp = iv_ca_cand_for_use (ivs, use);
  if (cp)
    {
      best_delta = iv_ca_delta_add (use, NULL, cp, NULL);
      iv_ca_set_no_cp (data, ivs, use);
    }

  /* First try important candidates.  Only if it fails, try the specific ones.
     Rationale -- in loops with many variables the best choice often is to use
     just one generic biv.  If we added here many ivs specific to the uses,
     the optimization algorithm later would be likely to get stuck in a local
     minimum, thus causing us to create too many ivs.  The approach from
     few ivs to more seems more likely to be successful -- starting from few
     ivs, replacing an expensive use by a specific iv should always be a
     win.  */
  EXECUTE_IF_SET_IN_BITMAP (data->important_candidates, 0, i, bi)
    {
      cand = iv_cand (data, i);

      if (iv_ca_cand_used_p (ivs, cand))
	continue;

      cp = get_use_iv_cost (data, use, cand);
      if (!cp)
	continue;

      iv_ca_set_cp (data, ivs, use, cp);
      act_cost = iv_ca_extend (data, ivs, cand, &act_delta, NULL);
      iv_ca_set_no_cp (data, ivs, use);
      act_delta = iv_ca_delta_add (use, NULL, cp, act_delta);

      if (act_cost < best_cost)
	{
	  best_cost = act_cost;

	  iv_ca_delta_free (&best_delta);
	  best_delta = act_delta;
	}
      else
	iv_ca_delta_free (&act_delta);
    }

  if (best_cost == INFTY)
    {
      for (i = 0; i < use->n_map_members; i++)
	{
	  cp = use->cost_map + i;
	  cand = cp->cand;
	  if (!cand)
	    continue;

	  /* Already tried this.  */
	  if (cand->important)
	    continue;
      
	  if (iv_ca_cand_used_p (ivs, cand))
	    continue;

	  act_delta = NULL;
	  iv_ca_set_cp (data, ivs, use, cp);
	  act_cost = iv_ca_extend (data, ivs, cand, &act_delta, NULL);
	  iv_ca_set_no_cp (data, ivs, use);
	  act_delta = iv_ca_delta_add (use, iv_ca_cand_for_use (ivs, use),
				       cp, act_delta);

	  if (act_cost < best_cost)
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

  return (best_cost != INFTY);
}

/* Finds an initial assignment of candidates to uses.  */

static struct iv_ca *
get_initial_solution (struct ivopts_data *data)
{
  struct iv_ca *ivs = iv_ca_new (data);
  unsigned i;

  for (i = 0; i < n_iv_uses (data); i++)
    if (!try_add_cand_for (data, ivs, iv_use (data, i)))
      {
	iv_ca_free (&ivs);
	return NULL;
      }

  return ivs;
}

/* Tries to improve set of induction variables IVS.  */

static bool
try_improve_iv_set (struct ivopts_data *data, struct iv_ca *ivs)
{
  unsigned i, acost, best_cost = iv_ca_cost (ivs), n_ivs;
  struct iv_ca_delta *best_delta = NULL, *act_delta, *tmp_delta;
  struct iv_cand *cand;

  /* Try extending the set of induction variables by one.  */
  for (i = 0; i < n_iv_cands (data); i++)
    {
      cand = iv_cand (data, i);
      
      if (iv_ca_cand_used_p (ivs, cand))
	continue;

      acost = iv_ca_extend (data, ivs, cand, &act_delta, &n_ivs);
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

      if (acost < best_cost)
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

      /* Nothing more we can do.  */
      if (!best_delta)
	return false;
    }

  iv_ca_delta_commit (data, ivs, best_delta, true);
  gcc_assert (best_cost == iv_ca_cost (ivs));
  iv_ca_delta_free (&best_delta);
  return true;
}

/* Attempts to find the optimal set of induction variables.  We do simple
   greedy heuristic -- we try to replace at most one candidate in the selected
   solution and remove the unused ivs while this improves the cost.  */

static struct iv_ca *
find_optimal_iv_set (struct ivopts_data *data)
{
  unsigned i;
  struct iv_ca *set;
  struct iv_use *use;

  /* Get the initial solution.  */
  set = get_initial_solution (data);
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

  while (try_improve_iv_set (data, set))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Improved to:\n");
	  iv_ca_dump (data, dump_file, set);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Final cost %d\n\n", iv_ca_cost (set));

  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);
      use->selected = iv_ca_cand_for_use (set, use)->cand;
    }

  return set;
}

/* Creates a new induction variable corresponding to CAND.  */

static void
create_new_iv (struct ivopts_data *data, struct iv_cand *cand)
{
  block_stmt_iterator incr_pos;
  tree base;
  bool after = false;

  if (!cand->iv)
    return;

  switch (cand->pos)
    {
    case IP_NORMAL:
      incr_pos = bsi_last (ip_normal_pos (data->current_loop));
      break;

    case IP_END:
      incr_pos = bsi_last (ip_end_pos (data->current_loop));
      after = true;
      break;

    case IP_ORIGINAL:
      /* Mark that the iv is preserved.  */
      name_info (data, cand->var_before)->preserve_biv = true;
      name_info (data, cand->var_after)->preserve_biv = true;

      /* Rewrite the increment so that it uses var_before directly.  */
      find_interesting_uses_op (data, cand->var_after)->selected = cand;
      
      return;
    }
 
  gimple_add_tmp_var (cand->var_before);
  add_referenced_tmp_var (cand->var_before);

  base = unshare_expr (cand->iv->base);

  create_iv (base, cand->iv->step, cand->var_before, data->current_loop,
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
      cand = iv_cand (data, i);
      create_new_iv (data, cand);
    }
}

/* Removes statement STMT (real or a phi node).  If INCLUDING_DEFINED_NAME
   is true, remove also the ssa name defined by the statement.  */

static void
remove_statement (tree stmt, bool including_defined_name)
{
  if (TREE_CODE (stmt) == PHI_NODE)
    {
      if (!including_defined_name)
	{
	  /* Prevent the ssa name defined by the statement from being removed.  */
	  SET_PHI_RESULT (stmt, NULL);
	}
      remove_phi_node (stmt, NULL_TREE, bb_for_stmt (stmt));
    }
  else
    {
      block_stmt_iterator bsi = bsi_for_stmt (stmt);

      bsi_remove (&bsi);
    }
}

/* Rewrites USE (definition of iv used in a nonlinear expression)
   using candidate CAND.  */

static void
rewrite_use_nonlinear_expr (struct ivopts_data *data,
			    struct iv_use *use, struct iv_cand *cand)
{
  tree comp;
  tree op, stmts, tgt, ass;
  block_stmt_iterator bsi, pbsi;

  /* An important special case -- if we are asked to express value of
     the original iv by itself, just exit; there is no need to
     introduce a new computation (that might also need casting the
     variable to unsigned and back).  */
  if (cand->pos == IP_ORIGINAL
      && TREE_CODE (use->stmt) == MODIFY_EXPR
      && TREE_OPERAND (use->stmt, 0) == cand->var_after)
    {
      op = TREE_OPERAND (use->stmt, 1);

      /* Be a bit careful.  In case variable is expressed in some
	 complicated way, rewrite it so that we may get rid of this
	 complicated expression.  */
      if ((TREE_CODE (op) == PLUS_EXPR
	   || TREE_CODE (op) == MINUS_EXPR)
	  && TREE_OPERAND (op, 0) == cand->var_before
	  && TREE_CODE (TREE_OPERAND (op, 1)) == INTEGER_CST)
	return;
    }

  comp = unshare_expr (get_computation (data->current_loop,
					use, cand));
  switch (TREE_CODE (use->stmt))
    {
    case PHI_NODE:
      tgt = PHI_RESULT (use->stmt);

      /* If we should keep the biv, do not replace it.  */
      if (name_info (data, tgt)->preserve_biv)
	return;

      pbsi = bsi = bsi_start (bb_for_stmt (use->stmt));
      while (!bsi_end_p (pbsi)
	     && TREE_CODE (bsi_stmt (pbsi)) == LABEL_EXPR)
	{
	  bsi = pbsi;
	  bsi_next (&pbsi);
	}
      break;

    case MODIFY_EXPR:
      tgt = TREE_OPERAND (use->stmt, 0);
      bsi = bsi_for_stmt (use->stmt);
      break;

    default:
      gcc_unreachable ();
    }

  op = force_gimple_operand (comp, &stmts, false, SSA_NAME_VAR (tgt));

  if (TREE_CODE (use->stmt) == PHI_NODE)
    {
      if (stmts)
	bsi_insert_after (&bsi, stmts, BSI_CONTINUE_LINKING);
      ass = build2 (MODIFY_EXPR, TREE_TYPE (tgt), tgt, op);
      bsi_insert_after (&bsi, ass, BSI_NEW_STMT);
      remove_statement (use->stmt, false);
      SSA_NAME_DEF_STMT (tgt) = ass;
    }
  else
    {
      if (stmts)
	bsi_insert_before (&bsi, stmts, BSI_SAME_STMT);
      TREE_OPERAND (use->stmt, 1) = op;
    }
}

/* Replaces ssa name in index IDX by its basic variable.  Callback for
   for_each_index.  */

static bool
idx_remove_ssa_names (tree base, tree *idx,
		      void *data ATTRIBUTE_UNUSED)
{
  tree *op;

  if (TREE_CODE (*idx) == SSA_NAME)
    *idx = SSA_NAME_VAR (*idx);

  if (TREE_CODE (base) == ARRAY_REF)
    {
      op = &TREE_OPERAND (base, 2);
      if (*op
	  && TREE_CODE (*op) == SSA_NAME)
	*op = SSA_NAME_VAR (*op);
      op = &TREE_OPERAND (base, 3);
      if (*op
	  && TREE_CODE (*op) == SSA_NAME)
	*op = SSA_NAME_VAR (*op);
    }

  return true;
}

/* Unshares REF and replaces ssa names inside it by their basic variables.  */

static tree
unshare_and_remove_ssa_names (tree ref)
{
  ref = unshare_expr (ref);
  for_each_index (&ref, idx_remove_ssa_names, NULL);

  return ref;
}

/* Rewrites base of memory access OP with expression WITH in statement
   pointed to by BSI.  */

static void
rewrite_address_base (block_stmt_iterator *bsi, tree *op, tree with)
{
  tree bvar, var, new_var, new_name, copy, name;
  tree orig;

  var = bvar = get_base_address (*op);

  if (!var || TREE_CODE (with) != SSA_NAME)
    goto do_rewrite;

  gcc_assert (TREE_CODE (var) != ALIGN_INDIRECT_REF);
  gcc_assert (TREE_CODE (var) != MISALIGNED_INDIRECT_REF);
  if (TREE_CODE (var) == INDIRECT_REF)
    var = TREE_OPERAND (var, 0);
  if (TREE_CODE (var) == SSA_NAME)
    {
      name = var;
      var = SSA_NAME_VAR (var);
    }
  else if (DECL_P (var))
    name = NULL_TREE;
  else
    goto do_rewrite;
    
  if (var_ann (var)->type_mem_tag)
    var = var_ann (var)->type_mem_tag;

  /* We need to add a memory tag for the variable.  But we do not want
     to add it to the temporary used for the computations, since this leads
     to problems in redundancy elimination when there are common parts
     in two computations referring to the different arrays.  So we copy
     the variable to a new temporary.  */
  copy = build2 (MODIFY_EXPR, void_type_node, NULL_TREE, with);
  if (name)
    new_name = duplicate_ssa_name (name, copy);
  else
    {
      new_var = create_tmp_var (TREE_TYPE (with), "ruatmp");
      add_referenced_tmp_var (new_var);
      var_ann (new_var)->type_mem_tag = var;
      new_name = make_ssa_name (new_var, copy);
    }
  TREE_OPERAND (copy, 0) = new_name;
  bsi_insert_before (bsi, copy, BSI_SAME_STMT);
  with = new_name;

do_rewrite:

  orig = NULL_TREE;
  gcc_assert (TREE_CODE (*op) != ALIGN_INDIRECT_REF);
  gcc_assert (TREE_CODE (*op) != MISALIGNED_INDIRECT_REF);

  if (TREE_CODE (*op) == INDIRECT_REF)
    orig = REF_ORIGINAL (*op);
  if (!orig)
    orig = unshare_and_remove_ssa_names (*op);

  *op = build1 (INDIRECT_REF, TREE_TYPE (*op), with);

  /* Record the original reference, for purposes of alias analysis.  */
  REF_ORIGINAL (*op) = orig;
}

/* Rewrites USE (address that is an iv) using candidate CAND.  */

static void
rewrite_use_address (struct ivopts_data *data,
		     struct iv_use *use, struct iv_cand *cand)
{
  tree comp = unshare_expr (get_computation (data->current_loop,
					     use, cand));
  block_stmt_iterator bsi = bsi_for_stmt (use->stmt);
  tree stmts;
  tree op = force_gimple_operand (comp, &stmts, true, NULL_TREE);

  if (stmts)
    bsi_insert_before (&bsi, stmts, BSI_SAME_STMT);

  rewrite_address_base (&bsi, use->op_p, op);
}

/* Rewrites USE (the condition such that one of the arguments is an iv) using
   candidate CAND.  */

static void
rewrite_use_compare (struct ivopts_data *data,
		     struct iv_use *use, struct iv_cand *cand)
{
  tree comp;
  tree *op_p, cond, op, stmts, bound;
  block_stmt_iterator bsi = bsi_for_stmt (use->stmt);
  enum tree_code compare;
  
  if (may_eliminate_iv (data, use, cand, &compare, &bound))
    {
      tree var = var_at_stmt (data->current_loop, cand, use->stmt);
      tree var_type = TREE_TYPE (var);

      bound = fold_convert (var_type, bound);
      op = force_gimple_operand (unshare_expr (bound), &stmts,
				 true, NULL_TREE);

      if (stmts)
	bsi_insert_before (&bsi, stmts, BSI_SAME_STMT);

      *use->op_p = build2 (compare, boolean_type_node, var, op);
      modify_stmt (use->stmt);
      return;
    }

  /* The induction variable elimination failed; just express the original
     giv.  */
  comp = unshare_expr (get_computation (data->current_loop, use, cand));

  cond = *use->op_p;
  op_p = &TREE_OPERAND (cond, 0);
  if (TREE_CODE (*op_p) != SSA_NAME
      || zero_p (get_iv (data, *op_p)->step))
    op_p = &TREE_OPERAND (cond, 1);

  op = force_gimple_operand (comp, &stmts, true, SSA_NAME_VAR (*op_p));
  if (stmts)
    bsi_insert_before (&bsi, stmts, BSI_SAME_STMT);

  *op_p = op;
}

/* Ensure that operand *OP_P may be used at the end of EXIT without
   violating loop closed ssa form.  */

static void
protect_loop_closed_ssa_form_use (edge exit, use_operand_p op_p)
{
  basic_block def_bb;
  struct loop *def_loop;
  tree phi, use;

  use = USE_FROM_PTR (op_p);
  if (TREE_CODE (use) != SSA_NAME)
    return;

  def_bb = bb_for_stmt (SSA_NAME_DEF_STMT (use));
  if (!def_bb)
    return;

  def_loop = def_bb->loop_father;
  if (flow_bb_inside_loop_p (def_loop, exit->dest))
    return;

  /* Try finding a phi node that copies the value out of the loop.  */
  for (phi = phi_nodes (exit->dest); phi; phi = PHI_CHAIN (phi))
    if (PHI_ARG_DEF_FROM_EDGE (phi, exit) == use)
      break;

  if (!phi)
    {
      /* Create such a phi node.  */
      tree new_name = duplicate_ssa_name (use, NULL);

      phi = create_phi_node (new_name, exit->dest);
      SSA_NAME_DEF_STMT (new_name) = phi;
      add_phi_arg (phi, use, exit);
    }

  SET_USE (op_p, PHI_RESULT (phi));
}

/* Ensure that operands of STMT may be used at the end of EXIT without
   violating loop closed ssa form.  */

static void
protect_loop_closed_ssa_form (edge exit, tree stmt)
{
  use_optype uses;
  vuse_optype vuses;
  v_may_def_optype v_may_defs;
  unsigned i;

  get_stmt_operands (stmt);

  uses = STMT_USE_OPS (stmt);
  for (i = 0; i < NUM_USES (uses); i++)
    protect_loop_closed_ssa_form_use (exit, USE_OP_PTR (uses, i));

  vuses = STMT_VUSE_OPS (stmt);
  for (i = 0; i < NUM_VUSES (vuses); i++)
    protect_loop_closed_ssa_form_use (exit, VUSE_OP_PTR (vuses, i));

  v_may_defs = STMT_V_MAY_DEF_OPS (stmt);
  for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
    protect_loop_closed_ssa_form_use (exit, V_MAY_DEF_OP_PTR (v_may_defs, i));
}

/* STMTS compute a value of a phi argument OP on EXIT of a loop.  Arrange things
   so that they are emitted on the correct place, and so that the loop closed
   ssa form is preserved.  */

static void
compute_phi_arg_on_exit (edge exit, tree stmts, tree op)
{
  tree_stmt_iterator tsi;
  block_stmt_iterator bsi;
  tree phi, stmt, def, next;

  if (EDGE_COUNT (exit->dest->preds) > 1)
    split_loop_exit_edge (exit);

  if (TREE_CODE (stmts) == STATEMENT_LIST)
    {
      for (tsi = tsi_start (stmts); !tsi_end_p (tsi); tsi_next (&tsi))
	protect_loop_closed_ssa_form (exit, tsi_stmt (tsi));
    }
  else
    protect_loop_closed_ssa_form (exit, stmts);

  /* Ensure there is label in exit->dest, so that we can
     insert after it.  */
  tree_block_label (exit->dest);
  bsi = bsi_after_labels (exit->dest);
  bsi_insert_after (&bsi, stmts, BSI_CONTINUE_LINKING);

  if (!op)
    return;

  for (phi = phi_nodes (exit->dest); phi; phi = next)
    {
      next = PHI_CHAIN (phi);

      if (PHI_ARG_DEF_FROM_EDGE (phi, exit) == op)
	{
	  def = PHI_RESULT (phi);
	  remove_statement (phi, false);
	  stmt = build2 (MODIFY_EXPR, TREE_TYPE (op),
			def, op);
	  SSA_NAME_DEF_STMT (def) = stmt;
	  bsi_insert_after (&bsi, stmt, BSI_CONTINUE_LINKING);
	}
    }
}

/* Rewrites the final value of USE (that is only needed outside of the loop)
   using candidate CAND.  */

static void
rewrite_use_outer (struct ivopts_data *data,
		   struct iv_use *use, struct iv_cand *cand)
{
  edge exit;
  tree value, op, stmts, tgt;
  tree phi;

  switch (TREE_CODE (use->stmt))
    {
    case PHI_NODE:
      tgt = PHI_RESULT (use->stmt);
      break;
    case MODIFY_EXPR:
      tgt = TREE_OPERAND (use->stmt, 0);
      break;
    default:
      gcc_unreachable ();
    }

  exit = single_dom_exit (data->current_loop);

  if (exit)
    {
      if (!cand->iv)
	{
	  bool ok = may_replace_final_value (data, use, &value);
	  gcc_assert (ok);
	}
      else
	value = get_computation_at (data->current_loop,
				    use, cand, last_stmt (exit->src));

      value = unshare_expr (value);
      op = force_gimple_operand (value, &stmts, true, SSA_NAME_VAR (tgt));
	  
      /* If we will preserve the iv anyway and we would need to perform
	 some computation to replace the final value, do nothing.  */
      if (stmts && name_info (data, tgt)->preserve_biv)
	return;

      for (phi = phi_nodes (exit->dest); phi; phi = PHI_CHAIN (phi))
	{
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, exit);

	  if (USE_FROM_PTR (use_p) == tgt)
	    SET_USE (use_p, op);
	}

      if (stmts)
	compute_phi_arg_on_exit (exit, stmts, op);

      /* Enable removal of the statement.  We cannot remove it directly,
	 since we may still need the aliasing information attached to the
	 ssa name defined by it.  */
      name_info (data, tgt)->iv->have_use_for = false;
      return;
    }

  /* If the variable is going to be preserved anyway, there is nothing to
     do.  */
  if (name_info (data, tgt)->preserve_biv)
    return;

  /* Otherwise we just need to compute the iv.  */
  rewrite_use_nonlinear_expr (data, use, cand);
}

/* Rewrites USE using candidate CAND.  */

static void
rewrite_use (struct ivopts_data *data,
	     struct iv_use *use, struct iv_cand *cand)
{
  switch (use->type)
    {
      case USE_NONLINEAR_EXPR:
	rewrite_use_nonlinear_expr (data, use, cand);
	break;

      case USE_OUTER:
	rewrite_use_outer (data, use, cand);
	break;

      case USE_ADDRESS:
	rewrite_use_address (data, use, cand);
	break;

      case USE_COMPARE:
	rewrite_use_compare (data, use, cand);
	break;

      default:
	gcc_unreachable ();
    }
  modify_stmt (use->stmt);
}

/* Rewrite the uses using the selected induction variables.  */

static void
rewrite_uses (struct ivopts_data *data)
{
  unsigned i;
  struct iv_cand *cand;
  struct iv_use *use;

  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);
      cand = use->selected;
      gcc_assert (cand);

      rewrite_use (data, use, cand);
    }
}

/* Removes the ivs that are not used after rewriting.  */

static void
remove_unused_ivs (struct ivopts_data *data)
{
  unsigned j;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, j, bi)
    {
      struct version_info *info;

      info = ver_info (data, j);
      if (info->iv
	  && !zero_p (info->iv->step)
	  && !info->inv_id
	  && !info->iv->have_use_for
	  && !info->preserve_biv)
	remove_statement (SSA_NAME_DEF_STMT (info->iv->ssa_name), true);
    }
}

/* Frees data allocated by the optimization of a single loop.  */

static void
free_loop_data (struct ivopts_data *data)
{
  unsigned i, j;
  bitmap_iterator bi;

  htab_empty (data->niters);

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i, bi)
    {
      struct version_info *info;

      info = ver_info (data, i);
      if (info->iv)
	free (info->iv);
      info->iv = NULL;
      info->has_nonlin_use = false;
      info->preserve_biv = false;
      info->inv_id = 0;
    }
  bitmap_clear (data->relevant);
  bitmap_clear (data->important_candidates);

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);

      free (use->iv);
      BITMAP_FREE (use->related_cands);
      for (j = 0; j < use->n_map_members; j++)
	if (use->cost_map[j].depends_on)
	  BITMAP_FREE (use->cost_map[j].depends_on);
      free (use->cost_map);
      free (use);
    }
  VARRAY_POP_ALL (data->iv_uses);

  for (i = 0; i < n_iv_cands (data); i++)
    {
      struct iv_cand *cand = iv_cand (data, i);

      if (cand->iv)
	free (cand->iv);
      free (cand);
    }
  VARRAY_POP_ALL (data->iv_candidates);

  if (data->version_info_size < num_ssa_names)
    {
      data->version_info_size = 2 * num_ssa_names;
      free (data->version_info);
      data->version_info = xcalloc (data->version_info_size,
				    sizeof (struct version_info));
    }

  data->max_inv_id = 0;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (decl_rtl_to_reset); i++)
    {
      tree obj = VARRAY_GENERIC_PTR_NOGC (decl_rtl_to_reset, i);

      SET_DECL_RTL (obj, NULL_RTX);
    }
  VARRAY_POP_ALL (decl_rtl_to_reset);
}

/* Finalizes data structures used by the iv optimization pass.  LOOPS is the
   loop tree.  */

static void
tree_ssa_iv_optimize_finalize (struct loops *loops, struct ivopts_data *data)
{
  unsigned i;

  for (i = 1; i < loops->num; i++)
    if (loops->parray[i])
      {
	free (loops->parray[i]->aux);
	loops->parray[i]->aux = NULL;
      }

  free_loop_data (data);
  free (data->version_info);
  BITMAP_FREE (data->relevant);
  BITMAP_FREE (data->important_candidates);
  htab_delete (data->niters);

  VARRAY_FREE (decl_rtl_to_reset);
  VARRAY_FREE (data->iv_uses);
  VARRAY_FREE (data->iv_candidates);
}

/* Optimizes the LOOP.  Returns true if anything changed.  */

static bool
tree_ssa_iv_optimize_loop (struct ivopts_data *data, struct loop *loop)
{
  bool changed = false;
  struct iv_ca *iv_ca;
  edge exit;

  data->current_loop = loop;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Processing loop %d\n", loop->num);
      
      exit = single_dom_exit (loop);
      if (exit)
	{
	  fprintf (dump_file, "  single exit %d -> %d, exit condition ",
		   exit->src->index, exit->dest->index);
	  print_generic_expr (dump_file, last_stmt (exit->src), TDF_SLIM);
	  fprintf (dump_file, "\n");
	}

      fprintf (dump_file, "\n");
    }

  /* For each ssa name determines whether it behaves as an induction variable
     in some loop.  */
  if (!find_induction_variables (data))
    goto finish;

  /* Finds interesting uses (item 1).  */
  find_interesting_uses (data);
  if (n_iv_uses (data) > MAX_CONSIDERED_USES)
    goto finish;

  /* Finds candidates for the induction variables (item 2).  */
  find_iv_candidates (data);

  /* Calculates the costs (item 3, part 1).  */
  determine_use_iv_costs (data);
  determine_iv_costs (data);
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
  rewrite_uses (data);

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

/* Main entry point.  Optimizes induction variables in LOOPS.  */

void
tree_ssa_iv_optimize (struct loops *loops)
{
  struct loop *loop;
  struct ivopts_data data;

  tree_ssa_iv_optimize_init (loops, &data);

  /* Optimize the loops starting with the innermost ones.  */
  loop = loops->tree_root;
  while (loop->inner)
    loop = loop->inner;

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
  verify_stmts ();
#endif

  /* Scan the loops, inner ones first.  */
  while (loop != loops->tree_root)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	flow_loop_dump (loop, dump_file, NULL, 1);

      tree_ssa_iv_optimize_loop (&data, loop);

      if (loop->next)
	{
	  loop = loop->next;
	  while (loop->inner)
	    loop = loop->inner;
	}
      else
	loop = loop->outer;
    }

#ifdef ENABLE_CHECKING
  verify_loop_closed_ssa ();
  verify_stmts ();
#endif

  tree_ssa_iv_optimize_finalize (loops, &data);
}
