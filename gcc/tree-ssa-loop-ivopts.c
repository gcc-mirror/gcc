/* Induction variable optimizations.
   Copyright (C) 2003 Free Software Foundation, Inc.
   
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

/* The infinite cost.  */
#define INFTY 10000000

/* The expected number of loop iterations.  TODO -- use profiling instead of
   this.  */
#define AVG_LOOP_NITER(LOOP) 5

/* Just to shorten the ugly names.  */
#define EXEC_BINARY nondestructive_fold_binary_to_constant
#define EXEC_UNARY nondestructive_fold_unary_to_constant

/* Representation of the induction variable.  */
struct iv
{
  tree base;		/* Initial value of the iv.  */
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
  struct tree_niter_desc niter;
			/* Number of iterations.  */

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
  bitmap related_cands;	/* The set of "related" iv candidates.  */

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

  /* Whether to consider just related and important candidates when replacing a
     use.  */
  bool consider_all_candidates;
};

/* Bound on number of candidates below that all candidates are considered.  */

#define CONSIDER_ALL_CANDIDATES_BOUND \
  ((unsigned) PARAM_VALUE (PARAM_IV_CONSIDER_ALL_CANDIDATES_BOUND))

/* If there are more iv occurences, we just give up (it is quite unlikely that
   optimizing such a loop would help, and it would take ages).  */

#define MAX_CONSIDERED_USES \
  ((unsigned) PARAM_VALUE (PARAM_IV_MAX_CONSIDERED_USES))

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
  fprintf (file, "ssa name ");
  print_generic_expr (file, iv->ssa_name, TDF_SLIM);
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

  if (iv->biv_p)
    fprintf (file, "  is a biv\n");
}

/* Dumps information about the USE to FILE.  */

extern void dump_use (FILE *, struct iv_use *);
void
dump_use (FILE *file, struct iv_use *use)
{
  struct iv *iv = use->iv;

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
      abort ();
    }

   fprintf (file, "  in statement ");
   print_generic_expr (file, use->stmt, TDF_SLIM);
   fprintf (file, "\n");

   fprintf (file, "  at position ");
   if (use->op_p)
     print_generic_expr (file, *use->op_p, TDF_SLIM);
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

   fprintf (file, "  related candidates ");
   dump_bitmap (file, use->related_cands);
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

  if (((val * b) & mask) != a)
    abort ();

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

  if (!bb)
    abort ();

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
      abort ();
    }
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
  data->relevant = BITMAP_XMALLOC ();
  data->max_inv_id = 0;

  for (i = 1; i < loops->num; i++)
    if (loops->parray[i])
      loops->parray[i]->aux = xcalloc (1, sizeof (struct loop_data));

  VARRAY_GENERIC_PTR_NOGC_INIT (data->iv_uses, 20, "iv_uses");
  VARRAY_GENERIC_PTR_NOGC_INIT (data->iv_candidates, 20, "iv_candidates");
  VARRAY_GENERIC_PTR_NOGC_INIT (decl_rtl_to_reset, 20, "decl_rtl_to_reset");
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

  if (info->iv)
    abort ();

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
    return fold_convert (type, integer_zero_node);

  return step;
}

/* Returns false if INDEX is a ssa name that occurs in an
   abnormal phi node.  Callback for for_each_index.  */

static bool
idx_contains_abnormal_ssa_name_p (tree base ATTRIBUTE_UNUSED, tree *index,
				  void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*index) != SSA_NAME)
    return true;

  return SSA_NAME_OCCURS_IN_ABNORMAL_PHI (*index) == 0;
}

/* Returns true if EXPR contains a ssa name that occurs in an
   abnormal phi node.  */

static bool
contains_abnormal_ssa_name_p (tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  char class = TREE_CODE_CLASS (code);
    
  if (code == SSA_NAME)
    return SSA_NAME_OCCURS_IN_ABNORMAL_PHI (expr) != 0;

  if (code == INTEGER_CST
      || is_gimple_min_invariant (expr))
    return false;

  if (code == ADDR_EXPR)
    return !for_each_index (&TREE_OPERAND (expr, 1),
			    idx_contains_abnormal_ssa_name_p,
			    NULL);

  switch (class)
    {
    case '2':
      if (contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 1)))
	return true;

      /* Fallthru.  */
    case '1':
      if (contains_abnormal_ssa_name_p (TREE_OPERAND (expr, 0)))
	return true;

      break;

    default:
      abort ();
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

  for (phi = phi_nodes (loop->header); phi; phi = TREE_CHAIN (phi))
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

  for (phi = phi_nodes (loop->header); phi; phi = TREE_CHAIN (phi))
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

/* Determine the number of iterations of the current loop.  */

static void
determine_number_of_iterations (struct ivopts_data *data)
{
  struct loop *loop = data->current_loop;
  edge exit = single_dom_exit (loop);

  if (!exit)
    return;

  number_of_iterations_exit (loop, exit, &loop_data (loop)->niter);
}

/* For each ssa name defined in LOOP determines whether it is an induction
   variable and if so, its initial value and step.  */

static bool
find_induction_variables (struct ivopts_data *data)
{
  unsigned i;
  struct loop *loop = data->current_loop;

  if (!find_bivs (data))
    return false;

  find_givs (data);
  mark_bivs (data);
  determine_number_of_iterations (data);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (loop_data (loop)->niter.niter)
	{
	  fprintf (dump_file, "  number of iterations ");
	  print_generic_expr (dump_file, loop_data (loop)->niter.niter,
			      TDF_SLIM);
	  fprintf (dump_file, "\n");

    	  fprintf (dump_file, "  may be zero if ");
    	  print_generic_expr (dump_file, loop_data (loop)->niter.may_be_zero,
    			      TDF_SLIM);
    	  fprintf (dump_file, "\n");

    	  fprintf (dump_file, "  bogus unless ");
    	  print_generic_expr (dump_file, loop_data (loop)->niter.assumptions,
    			      TDF_SLIM);
    	  fprintf (dump_file, "\n");
    	  fprintf (dump_file, "\n");
    	};
 
      fprintf (dump_file, "Induction variables:\n\n");

      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i,
	{
	  if (ver_info (data, i)->iv)
	    dump_iv (dump_file, ver_info (data, i)->iv);
	});
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
  use->related_cands = BITMAP_XMALLOC ();

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

      if (use->type != USE_NONLINEAR_EXPR
	  && use->type != USE_OUTER)
	abort ();

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
  if (TREE_CODE (stmt) != PHI_NODE
      && TREE_CODE (stmt) != MODIFY_EXPR)
    abort ();

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
  tree step, type, iv_type, iv_step;
  
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
    step = array_ref_element_size (base);
  else
    {
      /* The step for pointer arithmetics already is 1 byte.  */
      step = fold_convert (type, integer_one_node);
    }

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

  step = EXEC_BINARY (MULT_EXPR, type, step, iv_step);

  if (!*dta->step_p)
    *dta->step_p = step;
  else
    *dta->step_p = EXEC_BINARY (PLUS_EXPR, type, *dta->step_p, step);

  return true;
}

/* Records use in index IDX.  Callback for for_each_index.  Ivopts data
   object is passed to it in DATA.  */

static bool
idx_record_use (tree base ATTRIBUTE_UNUSED, tree *idx,
		void *data)
{
  find_interesting_uses_op (data, *idx);
  return true;
}

/* Finds addresses in *OP_P inside STMT.  */

static void
find_interesting_uses_address (struct ivopts_data *data, tree stmt, tree *op_p)
{
  tree base = unshare_expr (*op_p), step = NULL;
  struct iv *civ;
  struct ifs_ivopts_data ifs_ivopts_data;

  /* Ignore bitfields for now.  Not really something terribly complicated
     to handle.  TODO.  */
  if (TREE_CODE (base) == COMPONENT_REF
      && DECL_NONADDRESSABLE_P (TREE_OPERAND (base, 1)))
    goto fail;

  ifs_ivopts_data.ivopts_data = data;
  ifs_ivopts_data.stmt = stmt;
  ifs_ivopts_data.step_p = &step;
  if (!for_each_index (&base, idx_find_step, &ifs_ivopts_data)
      || zero_p (step))
    goto fail;

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
	case '<':
	  find_interesting_uses_cond (data, stmt, &TREE_OPERAND (stmt, 1));
	  return;

	case 'r':
	  find_interesting_uses_address (data, stmt, &TREE_OPERAND (stmt, 1));
	  if (TREE_CODE_CLASS (TREE_CODE (lhs)) == 'r')
	    find_interesting_uses_address (data, stmt, &TREE_OPERAND (stmt, 0));
	  return;

	default: ;
	}

      if (TREE_CODE_CLASS (TREE_CODE (lhs)) == 'r')
	{
	  find_interesting_uses_address (data, stmt, &TREE_OPERAND (stmt, 0));
	  find_interesting_uses_op (data, rhs);
	  return;
	}
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

  for (phi = phi_nodes (exit->dest); phi; phi = TREE_CHAIN (phi))
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
      bb = body[i];

      for (e = bb->succ; e; e = e->succ_next)
	if (e->dest != EXIT_BLOCK_PTR
	    && !flow_bb_inside_loop_p (data->current_loop, e->dest))
	  find_interesting_uses_outside (data, e);

      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
	find_interesting_uses_stmt (data, phi);
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	find_interesting_uses_stmt (data, bsi_stmt (bsi));
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\n");

      EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i,
	{
	  info = ver_info (data, i);
	  if (info->inv_id)
	    {
	      fprintf (dump_file, "  ");
	      print_generic_expr (dump_file, info->name, TDF_SLIM);
	      fprintf (dump_file, " is invariant (%d)%s\n",
		       info->inv_id, info->has_nonlin_use ? "" : ", eliminable");
	    }
	});

      fprintf (dump_file, "\n");
    }

  free (body);
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
  tree type;
  
  if (base)
    {
      type = TREE_TYPE (base);
      if (!TYPE_UNSIGNED (type))
	{
	  type = unsigned_type_for (type);
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

/* Adds a candidate BASE + STEP * i.  Important field is set to IMPORTANT and
   position to POS.  If USE is not NULL, the candidate is set as related to
   it.  The candidate computation is scheduled on all available positions.  */

static void
add_candidate (struct ivopts_data *data, 
	       tree base, tree step, bool important, struct iv_use *use)
{
  if (ip_normal_pos (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_NORMAL, use, NULL_TREE);
  if (ip_end_pos (data->current_loop))
    add_candidate_1 (data, base, step, important, IP_END, use, NULL_TREE);
}

/* Adds standard iv candidates.  */

static void
add_standard_iv_candidates (struct ivopts_data *data)
{
  /* Add 0 + 1 * iteration candidate.  */
  add_candidate (data,
		 fold_convert (unsigned_type_node, integer_zero_node),
      		 fold_convert (unsigned_type_node, integer_one_node),
		 true, NULL);

  /* The same for a long type.  */
  add_candidate (data,
		 fold_convert (long_unsigned_type_node, integer_zero_node),
		 fold_convert (long_unsigned_type_node, integer_one_node),
		 true, NULL);
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
		 fold_convert (TREE_TYPE (iv->base), integer_zero_node),
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

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i,
    {
      iv = ver_info (data, i)->iv;
      if (iv && iv->biv_p && !zero_p (iv->step))
	add_old_iv_candidates (data, iv);
    });
}

/* Adds candidates based on the value of the induction variable IV and USE.  */

static void
add_iv_value_candidates (struct ivopts_data *data,
			 struct iv *iv, struct iv_use *use)
{
  add_candidate (data, iv->base, iv->step, false, use);

  /* The same, but with initial value zero.  */
  add_candidate (data,
		 fold_convert (TREE_TYPE (iv->base), integer_zero_node),
		 iv->step, false, use);
}

/* Adds candidates based on the address IV and USE.  */

static void
add_address_candidates (struct ivopts_data *data,
			struct iv *iv, struct iv_use *use)
{
  tree base, abase, tmp, *act;

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
	  if (TREE_CODE (base) == INDIRECT_REF)
	    base = TREE_OPERAND (base, 0);
	  else
	    base = build_addr (base);
	  add_candidate (data, base, iv->step, false, use);
	}
    }

  /* Third, try removing the constant offset.  */
  abase = iv->base;
  while (TREE_CODE (abase) == PLUS_EXPR
	 && TREE_CODE (TREE_OPERAND (abase, 1)) != INTEGER_CST)
    abase = TREE_OPERAND (abase, 0);
  /* We found the offset, so make the copy of the non-shared part and
     remove it.  */
  if (TREE_CODE (abase) == PLUS_EXPR)
    {
      tmp = iv->base;
      act = &base;

      for (tmp = iv->base; tmp != abase; tmp = TREE_OPERAND (tmp, 0))
	{
	  *act = build2 (PLUS_EXPR, TREE_TYPE (tmp),
			 NULL_TREE, TREE_OPERAND (tmp, 1));
	  act = &TREE_OPERAND (*act, 0);
	}
      *act = TREE_OPERAND (tmp, 0);

      add_candidate (data, base, iv->step, false, use);
    }
}

/* Possibly adds pseudocandidate for replacing the final value of USE by
   a direct computation.  */

static void
add_iv_outer_candidates (struct ivopts_data *data, struct iv_use *use)
{
  struct tree_niter_desc *niter;
  struct loop *loop = data->current_loop;

  /* We must know where we exit the loop and how many times does it roll.  */
  if (!single_dom_exit (loop))
    return;

  niter = &loop_data (loop)->niter;
  if (!niter->niter
      || !operand_equal_p (niter->assumptions, boolean_true_node, 0)
      || !operand_equal_p (niter->may_be_zero, boolean_false_node, 0))
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
	  abort ();
	}
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
}

/* Allocates the data structure mapping the (use, candidate) pairs to costs.
   If consider_all_candidates is true, we use a two-dimensional array, otherwise
   we allocate a simple list to every use.  */

static void
alloc_use_cost_map (struct ivopts_data *data)
{
  unsigned i, n_imp = 0, size, j;

  if (!data->consider_all_candidates)
    {
      for (i = 0; i < n_iv_cands (data); i++)
	{
	  struct iv_cand *cand = iv_cand (data, i);
	  if (cand->important)
	    n_imp++;
	}
    }

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);

      if (data->consider_all_candidates)
	{
	  size = n_iv_cands (data);
	  use->n_map_members = size;
	}
      else
	{
	  size = n_imp;
	  EXECUTE_IF_SET_IN_BITMAP (use->related_cands, 0, j, size++);
	  use->n_map_members = 0;
	}

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
  if (cost == INFTY
      && depends_on)
    {
      BITMAP_XFREE (depends_on);
      depends_on = NULL;
    }

  if (data->consider_all_candidates)
    {
      use->cost_map[cand->id].cand = cand;
      use->cost_map[cand->id].cost = cost;
      use->cost_map[cand->id].depends_on = depends_on;
      return;
    }

  if (cost == INFTY)
    return;

  use->cost_map[use->n_map_members].cand = cand;
  use->cost_map[use->n_map_members].cost = cost;
  use->cost_map[use->n_map_members].depends_on = depends_on;
  use->n_map_members++;
}

/* Gets cost of (USE, CANDIDATE) pair.  Stores the bitmap of dependencies to
   DEPENDS_ON.  */

static unsigned
get_use_iv_cost (struct ivopts_data *data,
		 struct iv_use *use, struct iv_cand *cand, bitmap *depends_on)
{
  unsigned i;

  if (!cand)
    return INFTY;

  if (data->consider_all_candidates)
    i = cand->id;
  else
    {
      for (i = 0; i < use->n_map_members; i++)
	if (use->cost_map[i].cand == cand)
	  break;

      if (i == use->n_map_members)
	return INFTY;
    }

  if (depends_on)
    *depends_on = use->cost_map[i].depends_on;
  return use->cost_map[i].cost;
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
	{
	  if (TREE_STATIC (obj)
	      || DECL_EXTERNAL (obj))
	    {
	      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (obj));
	      x = gen_rtx_SYMBOL_REF (Pmode, name);
	    }
	  else
	    x = gen_raw_REG (Pmode, (*regno)++);

	  x = gen_rtx_MEM (DECL_MODE (obj), x);
	}
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
  int regno = 0;

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
  tree ubase = use->iv->base, ustep = use->iv->step;
  tree cbase = cand->iv->base, cstep = cand->iv->step;
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

  /* use = ubase + ratio * (var - cbase).  If either cbase is a constant
     or |ratio| == 1, it is better to handle this like
     
     ubase - ratio * cbase + ratio * var.  */

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
  else if (TREE_CODE (cbase) == INTEGER_CST)
    {
      ratio = build_int_cst_type (uutype, ratioi);
      delta = fold (build2 (MULT_EXPR, uutype, ratio, cbase));
      delta = fold (build2 (MINUS_EXPR, uutype, ubase, delta));
      expr = fold (build2 (MULT_EXPR, uutype, ratio, expr));
      expr = fold (build2 (PLUS_EXPR, uutype, delta, expr));
    }
  else
    {
      expr = fold (build2 (MINUS_EXPR, uutype, expr, cbase));
      ratio = build_int_cst_type (uutype, ratioi);
      expr = fold (build2 (MULT_EXPR, uutype, ratio, expr));
      expr = fold (build2 (PLUS_EXPR, uutype, ubase, expr));
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

/* Strips constant offsets from EXPR and adds them to OFFSET.  */

static void
strip_offset (tree *expr, unsigned HOST_WIDE_INT *offset)
{
  tree op0, op1;
  enum tree_code code;
  
  while (1)
    {
      if (cst_and_fits_in_hwi (*expr))
	{
	  *offset += int_cst_value (*expr);
	  *expr = integer_zero_node;
	  return;
	}

      code = TREE_CODE (*expr);
     
      if (code != PLUS_EXPR && code != MINUS_EXPR)
	return;

      op0 = TREE_OPERAND (*expr, 0);
      op1 = TREE_OPERAND (*expr, 1);

      if (cst_and_fits_in_hwi (op1))
	{
	  if (code == PLUS_EXPR)
	    *offset += int_cst_value (op1);
	  else
	    *offset -= int_cst_value (op1);

	  *expr = op0;
	  continue;
	}

      if (code != PLUS_EXPR)
	return;

      if (!cst_and_fits_in_hwi (op0))
	return;

      *offset += int_cst_value (op0);
      *expr = op1;
    }
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
  offset_p = (min_offset <= s_offset && s_offset <= max_offset);
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

      if (symbol_present)
	{
	  base = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (""));
	  if (offset_p)
	    base = gen_rtx_fmt_e (CONST, Pmode,
				  gen_rtx_fmt_ee (PLUS, Pmode,
						  base,
						  GEN_INT (off)));
	  if (var_present)
	    base = gen_rtx_fmt_ee (PLUS, Pmode, reg1, base);
	}

      else if (var_present)
	{
	  base = reg1;
	  if (offset_p)
	    base = gen_rtx_fmt_ee (PLUS, Pmode, base, GEN_INT (off));
	}
      else if (offset_p)
	base = GEN_INT (off);
      else
	base = NULL_RTX;
    
      if (base)
	addr = gen_rtx_fmt_ee (PLUS, Pmode, base, addr);
  
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
    *depends_on = BITMAP_XMALLOC ();
  bitmap_set_bit (*depends_on, info->inv_id);

  return NULL_TREE;
}

/* Estimates cost of forcing EXPR into variable.  DEPENDS_ON is a set of the
   invariants the computation depends on.  */

static unsigned
force_var_cost (struct ivopts_data *data,
		tree expr, bitmap *depends_on)
{
  static bool costs_initialized = false;
  static unsigned integer_cost;
  static unsigned symbol_cost;
  static unsigned address_cost;

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

  /* Just an arbitrary value, FIXME.  */
  return target_spill_cost;
}

/* Peels a single layer of ADDR.  If DIFF is not NULL, do it only if the
   offset is constant and add the offset to DIFF.  */

static tree
peel_address (tree addr, unsigned HOST_WIDE_INT *diff)
{
  tree off, size;
  HOST_WIDE_INT bit_offset;

  switch (TREE_CODE (addr))
    {
    case SSA_NAME:
    case INDIRECT_REF:
    case BIT_FIELD_REF:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case STRING_CST:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      return NULL_TREE;

    case COMPONENT_REF:
      off = DECL_FIELD_BIT_OFFSET (TREE_OPERAND (addr, 1));
      bit_offset = TREE_INT_CST_LOW (off);

      if (bit_offset % BITS_PER_UNIT)
	abort ();
      
      if (diff)
	*diff += bit_offset / BITS_PER_UNIT;

      return TREE_OPERAND (addr, 0);

    case ARRAY_REF:
      off = TREE_OPERAND (addr, 1);

      if (diff)
	{
	  if (!cst_and_fits_in_hwi (off))
	    return NULL_TREE;

	  size = TYPE_SIZE_UNIT (TREE_TYPE (addr));
	  if (!cst_and_fits_in_hwi (size))
	    return NULL_TREE;

	  *diff += TREE_INT_CST_LOW (off) * TREE_INT_CST_LOW (size);
	}

      return TREE_OPERAND (addr, 0);

    default:
      abort ();
    }
}

/* Checks whether E1 and E2 have constant difference, and if they do,
   store it in *DIFF.  */

static bool
ptr_difference_const (tree e1, tree e2, unsigned HOST_WIDE_INT *diff)
{
  int d1 = 0, d2 = 0;
  tree x;
  unsigned HOST_WIDE_INT delta1 = 0, delta2 = 0;

  /* Find depths of E1 and E2.  */
  for (x = e1; x; x = peel_address (x, NULL))
    d1++;
  for (x = e2; x; x = peel_address (x, NULL))
    d2++;

  for (; e1 && d1 > d2; e1 = peel_address (e1, &delta1))
    d1--;
  for (; e2 && d2 > d1; e2 = peel_address (e2, &delta2))
    d2--;

  while (e1 && e2 && !operand_equal_p (e1, e2, 0))
    {
      e1 = peel_address (e1, &delta1);
      e2 = peel_address (e2, &delta1);
    }

  if (!e1 || !e2)
    return false;

  *diff = delta1 - delta2;
  return true;
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
  tree core = addr;

  while (core
	 && TREE_CODE (core) != VAR_DECL)
    core = peel_address (core, offset);

  if (!core)
    {
      *symbol_present = false;
      *var_present = true;
      fd_ivopts_data = data;
      walk_tree (&addr, find_depends, depends_on, NULL);
      return target_spill_cost;
    }  
	  
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
  unsigned HOST_WIDE_INT diff = 0;
  unsigned cost;

  if (TREE_CODE (e1) != ADDR_EXPR)
    abort ();

  if (TREE_CODE (e2) == ADDR_EXPR
      && ptr_difference_const (TREE_OPERAND (e1, 0),
			       TREE_OPERAND (e2, 0), &diff))
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

  strip_offset (&e1, offset);
  *offset = -*offset;
  strip_offset (&e2, offset);
  *offset = -*offset;

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
    return get_address_cost (symbol_present, var_present, offset, ratio);

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

static void
determine_use_iv_cost_generic (struct ivopts_data *data,
			       struct iv_use *use, struct iv_cand *cand)
{
  bitmap depends_on;
  unsigned cost = get_computation_cost (data, use, cand, false, &depends_on);

  set_use_iv_cost (data, use, cand, cost, depends_on);
}

/* Determines cost of basing replacement of USE on CAND in an address.  */

static void
determine_use_iv_cost_address (struct ivopts_data *data,
			       struct iv_use *use, struct iv_cand *cand)
{
  bitmap depends_on;
  unsigned cost = get_computation_cost (data, use, cand, true, &depends_on);

  set_use_iv_cost (data, use, cand, cost, depends_on);
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

/* Check whether it is possible to express the condition in USE by comparison
   of candidate CAND.  If so, store the comparison code to COMPARE and the
   value compared with to BOUND.  */

static bool
may_eliminate_iv (struct loop *loop,
		  struct iv_use *use, struct iv_cand *cand,
		  enum tree_code *compare, tree *bound)
{
  edge exit;
  struct tree_niter_desc *niter, new_niter;
  tree wider_type, type, base;

  /* For now just very primitive -- we work just for the single exit condition,
     and are quite conservative about the possible overflows.  TODO -- both of
     these can be improved.  */
  exit = single_dom_exit (loop);
  if (!exit)
    return false;
  if (use->stmt != last_stmt (exit->src))
    return false;

  niter = &loop_data (loop)->niter;
  if (!niter->niter
      || !integer_nonzerop (niter->assumptions)
      || !integer_zerop (niter->may_be_zero))
    return false;

  if (exit->flags & EDGE_TRUE_VALUE)
    *compare = EQ_EXPR;
  else
    *compare = NE_EXPR;

  *bound = cand_value_at (loop, cand, use->stmt, niter->niter);

  /* Let us check there is not some problem with overflows, by checking that
     the number of iterations is unchanged.  */
  base = cand->iv->base;
  type = TREE_TYPE (base);
  if (stmt_after_increment (loop, cand, use->stmt))
    base = fold (build2 (PLUS_EXPR, type, base, cand->iv->step));

  new_niter.niter = NULL_TREE;
  number_of_iterations_cond (TREE_TYPE (cand->iv->base), base,
			     cand->iv->step, NE_EXPR, *bound, NULL_TREE,
			     &new_niter);
  if (!new_niter.niter
      || !integer_nonzerop (new_niter.assumptions)
      || !integer_zerop (new_niter.may_be_zero))
    return false;

  wider_type = TREE_TYPE (new_niter.niter);
  if (TYPE_PRECISION (wider_type) < TYPE_PRECISION (TREE_TYPE (niter->niter)))
    wider_type = TREE_TYPE (niter->niter);
  if (!operand_equal_p (fold_convert (wider_type, niter->niter),
			fold_convert (wider_type, new_niter.niter), 0))
    return false;

  return true;
}

/* Determines cost of basing replacement of USE on CAND in a condition.  */

static void
determine_use_iv_cost_condition (struct ivopts_data *data,
				 struct iv_use *use, struct iv_cand *cand)
{
  tree bound;
  enum tree_code compare;

  /* Only consider real candidates.  */
  if (!cand->iv)
    {
      set_use_iv_cost (data, use, cand, INFTY, NULL);
      return;
    }

  if (may_eliminate_iv (data->current_loop, use, cand, &compare, &bound))
    {
      bitmap depends_on = NULL;
      unsigned cost = force_var_cost (data, bound, &depends_on);

      set_use_iv_cost (data, use, cand, cost, depends_on);
      return;
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

  determine_use_iv_cost_generic (data, use, cand);
}

/* Checks whether it is possible to replace the final value of USE by
   a direct computation.  If so, the formula is stored to *VALUE.  */

static bool
may_replace_final_value (struct loop *loop, struct iv_use *use, tree *value)
{
  edge exit;
  struct tree_niter_desc *niter;

  exit = single_dom_exit (loop);
  if (!exit)
    return false;

  if (!dominated_by_p (CDI_DOMINATORS, exit->src,
		       bb_for_stmt (use->stmt)))
    abort ();

  niter = &loop_data (loop)->niter;
  if (!niter->niter
      || !operand_equal_p (niter->assumptions, boolean_true_node, 0)
      || !operand_equal_p (niter->may_be_zero, boolean_false_node, 0))
    return false;

  *value = iv_value (use->iv, niter->niter);

  return true;
}

/* Determines cost of replacing final value of USE using CAND.  */

static void
determine_use_iv_cost_outer (struct ivopts_data *data,
			     struct iv_use *use, struct iv_cand *cand)
{
  bitmap depends_on;
  unsigned cost;
  edge exit;
  tree value;
  struct loop *loop = data->current_loop;
	  
  if (!cand->iv)
    {
      if (!may_replace_final_value (loop, use, &value))
	{
	  set_use_iv_cost (data, use, cand, INFTY, NULL);
	  return;
	}

      depends_on = NULL;
      cost = force_var_cost (data, value, &depends_on);

      cost /= AVG_LOOP_NITER (loop);

      set_use_iv_cost (data, use, cand, cost, depends_on);
      return;
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
}

/* Determines cost of basing replacement of USE on CAND.  */

static void
determine_use_iv_cost (struct ivopts_data *data,
		       struct iv_use *use, struct iv_cand *cand)
{
  switch (use->type)
    {
    case USE_NONLINEAR_EXPR:
      determine_use_iv_cost_generic (data, use, cand);
      break;

    case USE_OUTER:
      determine_use_iv_cost_outer (data, use, cand);
      break;

    case USE_ADDRESS:
      determine_use_iv_cost_address (data, use, cand);
      break;

    case USE_COMPARE:
      determine_use_iv_cost_condition (data, use, cand);
      break;

    default:
      abort ();
    }
}

/* Determines costs of basing the use of the iv on an iv candidate.  */

static void
determine_use_iv_costs (struct ivopts_data *data)
{
  unsigned i, j;
  struct iv_use *use;
  struct iv_cand *cand;

  data->consider_all_candidates = (n_iv_cands (data)
				   <= CONSIDER_ALL_CANDIDATES_BOUND);

  alloc_use_cost_map (data);

  if (!data->consider_all_candidates)
    {
      /* Add the important candidate entries.  */
      for (j = 0; j < n_iv_cands (data); j++)
	{
	  cand = iv_cand (data, j);
	  if (!cand->important)
	    continue;
	  for (i = 0; i < n_iv_uses (data); i++)
	    {
	      use = iv_use (data, i);
	      determine_use_iv_cost (data, use, cand);
	    }
	}
    }

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
	  EXECUTE_IF_SET_IN_BITMAP (use->related_cands, 0, j,
	    {
	      cand = iv_cand (data, j);
	      if (!cand->important)
	        determine_use_iv_cost (data, use, cand);
	    });
	}
    }

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
  tree base, last;
  basic_block bb;

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
  if (cand->pos == IP_END)
    {
      bb = ip_end_pos (data->current_loop);
      last = last_stmt (bb);

      if (!last
	  || TREE_CODE (last) == LABEL_EXPR)
	cand->cost++;
    }
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
  for (phi = phi_nodes (loop->header); phi; phi = TREE_CHAIN (phi))
    {
      op = PHI_RESULT (phi);

      if (!is_gimple_reg (op))
	continue;

      if (get_iv (data, op))
	continue;

      n++;
    }

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, j,
    {
      struct version_info *info = ver_info (data, j);

      if (info->inv_id && info->has_nonlin_use)
	n++;
    });

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

/* Finds a best candidate for USE and stores it to CAND.  The candidates are
   taken from the set SOL and they may depend on invariants in the set INV.
   The really used candidate and invariants are noted to USED_IVS and
   USED_INV.  */

static unsigned
find_best_candidate (struct ivopts_data *data,
		     struct iv_use *use, bitmap sol, bitmap inv,
		     bitmap used_ivs, bitmap used_inv, struct iv_cand **cand)
{
  unsigned c, d;
  unsigned best_cost = INFTY, cost;
  struct iv_cand *cnd = NULL, *acnd;
  bitmap depends_on = NULL, asol;

  if (data->consider_all_candidates)
    asol = sol;
  else
    {
      asol = BITMAP_XMALLOC ();
      bitmap_a_and_b (asol, sol, use->related_cands);
    }

  EXECUTE_IF_SET_IN_BITMAP (asol, 0, c,
    {
      acnd = iv_cand (data, c);
      cost = get_use_iv_cost (data, use, acnd, &depends_on);

      if (cost == INFTY)
	goto next_cand;
      if (cost > best_cost)
	goto next_cand;
      if (cost == best_cost)
	{
	  /* Prefer the cheaper iv.  */
	  if (acnd->cost >= cnd->cost)
	    goto next_cand;
	}

      if (depends_on)
	{
	  EXECUTE_IF_AND_COMPL_IN_BITMAP (depends_on, inv, 0, d,
					  goto next_cand);
	  if (used_inv)
	    bitmap_a_or_b (used_inv, used_inv, depends_on);
	}

      cnd = acnd;
      best_cost = cost;
next_cand: ;
    });

  if (cnd && used_ivs)
    bitmap_set_bit (used_ivs, cnd->id);

  if (cand)
    *cand = cnd;

  if (!data->consider_all_candidates)
    BITMAP_XFREE (asol);

  return best_cost;
}

/* Returns cost of set of ivs SOL + invariants INV.  Removes unnecessary
   induction variable candidates and invariants from the sets.  Only
   uses 0 .. MAX_USE - 1 are taken into account.  */

static unsigned
set_cost_up_to (struct ivopts_data *data, bitmap sol, bitmap inv,
		unsigned max_use)
{
  unsigned i;
  unsigned cost = 0, size = 0, acost;
  struct iv_use *use;
  struct iv_cand *cand;
  bitmap used_ivs = BITMAP_XMALLOC (), used_inv = BITMAP_XMALLOC ();

  for (i = 0; i < max_use; i++)
    {
      use = iv_use (data, i);
      acost = find_best_candidate (data, use, sol, inv,
				   used_ivs, used_inv, NULL);
      if (acost == INFTY)
	{
	  BITMAP_XFREE (used_ivs);
	  BITMAP_XFREE (used_inv);
	  return INFTY;
	}
      cost += acost;
    }

  EXECUTE_IF_SET_IN_BITMAP (used_ivs, 0, i,
    {
      cand = iv_cand (data, i);

      /* Do not count the pseudocandidates.  */
      if (cand->iv)
	size++;

      cost += cand->cost;
    });
  EXECUTE_IF_SET_IN_BITMAP (used_inv, 0, i, size++);
  cost += ivopts_global_cost_for_size (data, size);

  bitmap_copy (sol, used_ivs);
  bitmap_copy (inv, used_inv);

  BITMAP_XFREE (used_ivs);
  BITMAP_XFREE (used_inv);

  return cost;
}

/* Computes cost of set of ivs SOL + invariants INV.  Removes unnecessary
   induction variable candidates and invariants from the sets.  */

static unsigned
set_cost (struct ivopts_data *data, bitmap sol, bitmap inv)
{
  return set_cost_up_to (data, sol, inv, n_iv_uses (data));
}

/* Tries to extend the sets IVS and INV in the best possible way in order
   to express the USE.  */

static bool
try_add_cand_for (struct ivopts_data *data, bitmap ivs, bitmap inv,
		  struct iv_use *use)
{
  unsigned best_cost = set_cost_up_to (data, ivs, inv, use->id + 1), act_cost;
  bitmap best_ivs = BITMAP_XMALLOC ();
  bitmap best_inv = BITMAP_XMALLOC ();
  bitmap act_ivs = BITMAP_XMALLOC ();
  bitmap act_inv = BITMAP_XMALLOC ();
  unsigned i;
  struct cost_pair *cp;

  bitmap_copy (best_ivs, ivs);
  bitmap_copy (best_inv, inv);

  for (i = 0; i < use->n_map_members; i++)
    {
      cp = use->cost_map + i;
      if (cp->cost == INFTY)
	continue;

      bitmap_copy (act_ivs, ivs);
      bitmap_set_bit (act_ivs, cp->cand->id);
      if (cp->depends_on)
	bitmap_a_or_b (act_inv, inv, cp->depends_on);
      else
	bitmap_copy (act_inv, inv);
      act_cost = set_cost_up_to (data, act_ivs, act_inv, use->id + 1);

      if (act_cost < best_cost)
	{
	  best_cost = act_cost;
	  bitmap_copy (best_ivs, act_ivs);
	  bitmap_copy (best_inv, act_inv);
	}
    }

  bitmap_copy (ivs, best_ivs);
  bitmap_copy (inv, best_inv);

  BITMAP_XFREE (best_ivs);
  BITMAP_XFREE (best_inv);
  BITMAP_XFREE (act_ivs);
  BITMAP_XFREE (act_inv);

  return (best_cost != INFTY);
}

/* Finds an initial set of IVS and invariants INV.  We do this by simply
   choosing the best candidate for each use.  */

static unsigned
get_initial_solution (struct ivopts_data *data, bitmap ivs, bitmap inv)
{
  unsigned i;

  for (i = 0; i < n_iv_uses (data); i++)
    if (!try_add_cand_for (data, ivs, inv, iv_use (data, i)))
      return INFTY;

  return set_cost (data, ivs, inv);
}

/* Tries to improve set of induction variables IVS and invariants INV to get
   it better than COST.  */

static bool
try_improve_iv_set (struct ivopts_data *data,
		    bitmap ivs, bitmap inv, unsigned *cost)
{
  unsigned i, acost;
  bitmap new_ivs = BITMAP_XMALLOC (), new_inv = BITMAP_XMALLOC ();
  bitmap best_new_ivs = NULL, best_new_inv = NULL;

  /* Try altering the set of induction variables by one.  */
  for (i = 0; i < n_iv_cands (data); i++)
    {
      bitmap_copy (new_ivs, ivs);
      bitmap_copy (new_inv, inv);

      if (bitmap_bit_p (ivs, i))
	bitmap_clear_bit (new_ivs, i);
      else
	bitmap_set_bit (new_ivs, i);

      acost = set_cost (data, new_ivs, new_inv);
      if (acost >= *cost)
	continue;

      if (!best_new_ivs)
	{
	  best_new_ivs = BITMAP_XMALLOC ();
	  best_new_inv = BITMAP_XMALLOC ();
	}

      *cost = acost;
      bitmap_copy (best_new_ivs, new_ivs);
      bitmap_copy (best_new_inv, new_inv);
    }

  /* Ditto for invariants.  */
  for (i = 1; i <= data->max_inv_id; i++)
    {
      if (ver_info (data, i)->has_nonlin_use)
	continue;

      bitmap_copy (new_ivs, ivs);
      bitmap_copy (new_inv, inv);

      if (bitmap_bit_p (inv, i))
	bitmap_clear_bit (new_inv, i);
      else
	bitmap_set_bit (new_inv, i);

      acost = set_cost (data, new_ivs, new_inv);
      if (acost >= *cost)
	continue;

      if (!best_new_ivs)
	{
	  best_new_ivs = BITMAP_XMALLOC ();
	  best_new_inv = BITMAP_XMALLOC ();
	}

      *cost = acost;
      bitmap_copy (best_new_ivs, new_ivs);
      bitmap_copy (best_new_inv, new_inv);
    }

  BITMAP_XFREE (new_ivs);
  BITMAP_XFREE (new_inv);

  if (!best_new_ivs)
    return false;

  bitmap_copy (ivs, best_new_ivs);
  bitmap_copy (inv, best_new_inv);
  BITMAP_XFREE (best_new_ivs);
  BITMAP_XFREE (best_new_inv);
  return true;
}

/* Attempts to find the optimal set of induction variables.  We do simple
   greedy heuristic -- we try to replace at most one candidate in the selected
   solution and remove the unused ivs while this improves the cost.  */

static bitmap
find_optimal_iv_set (struct ivopts_data *data)
{
  unsigned cost, i;
  bitmap set = BITMAP_XMALLOC ();
  bitmap inv = BITMAP_XMALLOC ();
  struct iv_use *use;

  /* Set the upper bound.  */
  cost = get_initial_solution (data, set, inv);
  if (cost == INFTY)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Unable to substitute for ivs, failed.\n");
      BITMAP_XFREE (inv);
      BITMAP_XFREE (set);
      return NULL;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Initial set of candidates (cost %d): ", cost);
      bitmap_print (dump_file, set, "", "");
      fprintf (dump_file, " invariants ");
      bitmap_print (dump_file, inv, "", "");
      fprintf (dump_file, "\n");
    }

  while (try_improve_iv_set (data, set, inv, &cost))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Improved to (cost %d): ", cost);
	  bitmap_print (dump_file, set, "", "");
	  fprintf (dump_file, " invariants ");
	  bitmap_print (dump_file, inv, "", "");
	  fprintf (dump_file, "\n");
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Final cost %d\n\n", cost);

  for (i = 0; i < n_iv_uses (data); i++)
    {
      use = iv_use (data, i);
      find_best_candidate (data, use, set, inv, NULL, NULL, &use->selected);
    }

  BITMAP_XFREE (inv);

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
create_new_ivs (struct ivopts_data *data, bitmap set)
{
  unsigned i;
  struct iv_cand *cand;

  EXECUTE_IF_SET_IN_BITMAP (set, 0, i,
    {
      cand = iv_cand (data, i);
      create_new_iv (data, cand);
    });
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
      block_stmt_iterator bsi = stmt_for_bsi (stmt);

      bsi_remove (&bsi);
    }
}

/* Rewrites USE (definition of iv used in a nonlinear expression)
   using candidate CAND.  */

static void
rewrite_use_nonlinear_expr (struct ivopts_data *data,
			    struct iv_use *use, struct iv_cand *cand)
{
  tree comp = unshare_expr (get_computation (data->current_loop,
					     use, cand));
  tree op, stmts, tgt, ass;
  block_stmt_iterator bsi, pbsi;
 
  if (TREE_CODE (use->stmt) == PHI_NODE)
    {
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
    }
  else if (TREE_CODE (use->stmt) == MODIFY_EXPR)
    {
      tgt = TREE_OPERAND (use->stmt, 0);
      bsi = stmt_for_bsi (use->stmt);
    }
  else
    abort ();

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
idx_remove_ssa_names (tree base ATTRIBUTE_UNUSED, tree *idx,
		      void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*idx) == SSA_NAME)
    *idx = SSA_NAME_VAR (*idx);
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
  tree var = get_base_address (*op), new_var, new_name, copy, name;
  tree orig;

  if (!var || TREE_CODE (with) != SSA_NAME)
    goto do_rewrite;

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
  block_stmt_iterator bsi = stmt_for_bsi (use->stmt);
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
  block_stmt_iterator bsi = stmt_for_bsi (use->stmt);
  enum tree_code compare;
  
  if (may_eliminate_iv (data->current_loop,
			use, cand, &compare, &bound))
    {
      op = force_gimple_operand (unshare_expr (bound), &stmts,
				 true, NULL_TREE);

      if (stmts)
	bsi_insert_before (&bsi, stmts, BSI_SAME_STMT);

      *use->op_p = build2 (compare, boolean_type_node,
			  var_at_stmt (data->current_loop,
				       cand, use->stmt), op);
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
  for (phi = phi_nodes (exit->dest); phi; phi = TREE_CHAIN (phi))
    if (PHI_ARG_DEF_FROM_EDGE (phi, exit) == use)
      break;

  if (!phi)
    {
      /* Create such a phi node.  */
      tree new_name = duplicate_ssa_name (use, NULL);

      phi = create_phi_node (new_name, exit->dest);
      SSA_NAME_DEF_STMT (new_name) = phi;
      add_phi_arg (&phi, use, exit);
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

  if (exit->dest->pred->pred_next)
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
      next = TREE_CHAIN (phi);

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

  if (TREE_CODE (use->stmt) == PHI_NODE)
    tgt = PHI_RESULT (use->stmt);
  else if (TREE_CODE (use->stmt) == MODIFY_EXPR)
    tgt = TREE_OPERAND (use->stmt, 0);
  else
    abort ();
  exit = single_dom_exit (data->current_loop);

  if (exit)
    {
      if (!cand->iv)
	{
	  if (!may_replace_final_value (data->current_loop, use, &value))
	    abort ();
	}
      else
	value = get_computation_at (data->current_loop,
				    use, cand, last_stmt (exit->src));

      op = force_gimple_operand (value, &stmts, true, SSA_NAME_VAR (tgt));
	  
      /* If we will preserve the iv anyway and we would need to perform
	 some computation to replace the final value, do nothing.  */
      if (stmts && name_info (data, tgt)->preserve_biv)
	return;

      for (phi = phi_nodes (exit->dest); phi; phi = TREE_CHAIN (phi))
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
	abort ();
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
      if (!cand)
	abort ();

      rewrite_use (data, use, cand);
    }
}

/* Removes the ivs that are not used after rewriting.  */

static void
remove_unused_ivs (struct ivopts_data *data)
{
  unsigned j;

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, j,
    {
      struct version_info *info;

      info = ver_info (data, j);
      if (info->iv
	  && !zero_p (info->iv->step)
	  && !info->inv_id
	  && !info->iv->have_use_for
	  && !info->preserve_biv)
	remove_statement (SSA_NAME_DEF_STMT (info->iv->ssa_name), true);
    });
}

/* Frees data allocated by the optimization of a single loop.  */

static void
free_loop_data (struct ivopts_data *data)
{
  unsigned i, j;

  EXECUTE_IF_SET_IN_BITMAP (data->relevant, 0, i,
    {
      struct version_info *info;

      info = ver_info (data, i);
      if (info->iv)
	free (info->iv);
      info->iv = NULL;
      info->has_nonlin_use = false;
      info->preserve_biv = false;
      info->inv_id = 0;
    });
  bitmap_clear (data->relevant);

  for (i = 0; i < n_iv_uses (data); i++)
    {
      struct iv_use *use = iv_use (data, i);

      free (use->iv);
      BITMAP_XFREE (use->related_cands);
      for (j = 0; j < use->n_map_members; j++)
	if (use->cost_map[j].depends_on)
	  BITMAP_XFREE (use->cost_map[j].depends_on);
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
  BITMAP_XFREE (data->relevant);

  VARRAY_FREE (decl_rtl_to_reset);
  VARRAY_FREE (data->iv_uses);
  VARRAY_FREE (data->iv_candidates);
}

/* Optimizes the LOOP.  Returns true if anything changed.  */

static bool
tree_ssa_iv_optimize_loop (struct ivopts_data *data, struct loop *loop)
{
  bool changed = false;
  bitmap iv_set;
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
  iv_set = find_optimal_iv_set (data);
  if (!iv_set)
    goto finish;
  changed = true;

  /* Create the new induction variables (item 4, part 1).  */
  create_new_ivs (data, iv_set);
  
  /* Rewrite the uses (item 4, part 2).  */
  rewrite_uses (data);

  /* Remove the ivs that are unused after rewriting.  */
  remove_unused_ivs (data);

  loop_commit_inserts ();

  BITMAP_XFREE (iv_set);

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
#endif

  /* Scan the loops, inner ones first.  */
  while (loop != loops->tree_root)
    {
      if (tree_ssa_iv_optimize_loop (&data, loop))
	{
#ifdef ENABLE_CHECKING
	  verify_loop_closed_ssa ();
#endif
	}

      if (loop->next)
	{
	  loop = loop->next;
	  while (loop->inner)
	    loop = loop->inner;
	}
      else
	loop = loop->outer;
    }

  tree_ssa_iv_optimize_finalize (loops, &data);
}
