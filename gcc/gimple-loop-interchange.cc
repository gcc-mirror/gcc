/* Loop interchange.
   Copyright (C) 2017-2018 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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
#include "is-a.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-ssa.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-dce.h"
#include "tree-data-ref.h"
#include "tree-vectorizer.h"

/* This pass performs loop interchange: for example, the loop nest

   for (int j = 0; j < N; j++)
     for (int k = 0; k < N; k++)
       for (int i = 0; i < N; i++)
	 c[i][j] = c[i][j] + a[i][k]*b[k][j];

   is transformed to

   for (int i = 0; i < N; i++)
     for (int j = 0; j < N; j++)
       for (int k = 0; k < N; k++)
	 c[i][j] = c[i][j] + a[i][k]*b[k][j];

   This pass implements loop interchange in the following steps:

     1) Find perfect loop nest for each innermost loop and compute data
	dependence relations for it.  For above example, loop nest is
	<loop_j, loop_k, loop_i>.
     2) From innermost to outermost loop, this pass tries to interchange
	each loop pair.  For above case, it firstly tries to interchange
	<loop_k, loop_i> and loop nest becomes <loop_j, loop_i, loop_k>.
	Then it tries to interchange <loop_j, loop_i> and loop nest becomes
	<loop_i, loop_j, loop_k>.  The overall effect is to move innermost
	loop to the outermost position.  For loop pair <loop_i, loop_j>
	to be interchanged, we:
     3) Check if data dependence relations are valid for loop interchange.
     4) Check if both loops can be interchanged in terms of transformation.
     5) Check if interchanging the two loops is profitable.
     6) Interchange the two loops by mapping induction variables.

   This pass also handles reductions in loop nest.  So far we only support
   simple reduction of inner loop and double reduction of the loop nest.  */

/* Maximum number of stmts in each loop that should be interchanged.  */
#define MAX_NUM_STMT    (PARAM_VALUE (PARAM_LOOP_INTERCHANGE_MAX_NUM_STMTS))
/* Maximum number of data references in loop nest.  */
#define MAX_DATAREFS    (PARAM_VALUE (PARAM_LOOP_MAX_DATAREFS_FOR_DATADEPS))

/* Comparison ratio of access stride between inner/outer loops to be
   interchanged.  This is the minimum stride ratio for loop interchange
   to be profitable.  */
#define OUTER_STRIDE_RATIO  (PARAM_VALUE (PARAM_LOOP_INTERCHANGE_STRIDE_RATIO))
/* The same as above, but we require higher ratio for interchanging the
   innermost two loops.  */
#define INNER_STRIDE_RATIO  ((OUTER_STRIDE_RATIO) + 1)

/* Comparison ratio of stmt cost between inner/outer loops.  Loops won't
   be interchanged if outer loop has too many stmts.  */
#define STMT_COST_RATIO     (3)

/* Vector of strides that DR accesses in each level loop of a loop nest.  */
#define DR_ACCESS_STRIDE(dr) ((vec<tree> *) dr->aux)

/* Structure recording loop induction variable.  */
typedef struct induction
{
  /* IV itself.  */
  tree var;
  /* IV's initializing value, which is the init arg of the IV PHI node.  */
  tree init_val;
  /* IV's initializing expr, which is (the expanded result of) init_val.  */
  tree init_expr;
  /* IV's step.  */
  tree step;
} *induction_p;

/* Enum type for loop reduction variable.  */
enum reduction_type
{
  UNKNOWN_RTYPE = 0,
  SIMPLE_RTYPE,
  DOUBLE_RTYPE
};

/* Structure recording loop reduction variable.  */
typedef struct reduction
{
  /* Reduction itself.  */
  tree var;
  /* PHI node defining reduction variable.  */
  gphi *phi;
  /* Init and next variables of the reduction.  */
  tree init;
  tree next;
  /* Lcssa PHI node if reduction is used outside of its definition loop.  */
  gphi *lcssa_phi;
  /* Stmts defining init and next.  */
  gimple *producer;
  gimple *consumer;
  /* If init is loaded from memory, this is the loading memory reference.  */
  tree init_ref;
  /* If reduction is finally stored to memory, this is the stored memory
     reference.  */
  tree fini_ref;
  enum reduction_type type;
} *reduction_p;


/* Dump reduction RE.  */

static void
dump_reduction (reduction_p re)
{
  if (re->type == SIMPLE_RTYPE)
    fprintf (dump_file, "  Simple reduction:  ");
  else if (re->type == DOUBLE_RTYPE)
    fprintf (dump_file, "  Double reduction:  ");
  else
    fprintf (dump_file, "  Unknown reduction:  ");

  print_gimple_stmt (dump_file, re->phi, 0);
}

/* Dump LOOP's induction IV.  */
static void
dump_induction (struct loop *loop, induction_p iv)
{
  fprintf (dump_file, "  Induction:  ");
  print_generic_expr (dump_file, iv->var, TDF_SLIM);
  fprintf (dump_file, " = {");
  print_generic_expr (dump_file, iv->init_expr, TDF_SLIM);
  fprintf (dump_file, ", ");
  print_generic_expr (dump_file, iv->step, TDF_SLIM);
  fprintf (dump_file, "}_%d\n", loop->num);
}

/* Loop candidate for interchange.  */

struct loop_cand
{
  loop_cand (struct loop *, struct loop *);
  ~loop_cand ();

  reduction_p find_reduction_by_stmt (gimple *);
  void classify_simple_reduction (reduction_p);
  bool analyze_iloop_reduction_var (tree);
  bool analyze_oloop_reduction_var (loop_cand *, tree);
  bool analyze_induction_var (tree, tree);
  bool analyze_carried_vars (loop_cand *);
  bool analyze_lcssa_phis (void);
  bool can_interchange_p (loop_cand *);
  void undo_simple_reduction (reduction_p, bitmap);

  /* The loop itself.  */
  struct loop *m_loop;
  /* The outer loop for interchange.  It equals to loop if this loop cand
     itself represents the outer loop.  */
  struct loop *m_outer;
  /* Vector of induction variables in loop.  */
  vec<induction_p> m_inductions;
  /* Vector of reduction variables in loop.  */
  vec<reduction_p> m_reductions;
  /* Lcssa PHI nodes of this loop.  */
  vec<gphi *> m_lcssa_nodes;
  /* Single exit edge of this loop.  */
  edge m_exit;
  /* Basic blocks of this loop.  */
  basic_block *m_bbs;
  /* Number of stmts of this loop.  Inner loops' stmts are not included.  */
  int m_num_stmts;
  /* Number of constant initialized simple reduction.  */
  int m_const_init_reduc;
};

/* Constructor.  */

loop_cand::loop_cand (struct loop *loop, struct loop *outer)
  : m_loop (loop), m_outer (outer), m_exit (single_exit (loop)),
    m_bbs (get_loop_body (loop)), m_num_stmts (0), m_const_init_reduc (0)
{
    m_inductions.create (3);
    m_reductions.create (3);
    m_lcssa_nodes.create (3);
}

/* Destructor.  */

loop_cand::~loop_cand ()
{
  induction_p iv;
  for (unsigned i = 0; m_inductions.iterate (i, &iv); ++i)
    free (iv);

  reduction_p re;
  for (unsigned i = 0; m_reductions.iterate (i, &re); ++i)
    free (re);

  m_inductions.release ();
  m_reductions.release ();
  m_lcssa_nodes.release ();
  free (m_bbs);
}

/* Return single use stmt of VAR in LOOP, otherwise return NULL.  */

static gimple *
single_use_in_loop (tree var, struct loop *loop)
{
  gimple *stmt, *res = NULL;
  use_operand_p use_p;
  imm_use_iterator iterator;

  FOR_EACH_IMM_USE_FAST (use_p, iterator, var)
    {
      stmt = USE_STMT (use_p);
      if (is_gimple_debug (stmt))
	continue;

      if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	continue;

      if (res)
	return NULL;

      res = stmt;
    }
  return res;
}

/* Return true if E is unsupported in loop interchange, i.e, E is a complex
   edge or part of irreducible loop.  */

static inline bool
unsupported_edge (edge e)
{
  return (e->flags & (EDGE_COMPLEX | EDGE_IRREDUCIBLE_LOOP));
}

/* Return the reduction if STMT is one of its lcssa PHI, producer or consumer
   stmt.  */

reduction_p
loop_cand::find_reduction_by_stmt (gimple *stmt)
{
  gphi *phi = dyn_cast <gphi *> (stmt);
  reduction_p re;

  for (unsigned i = 0; m_reductions.iterate (i, &re); ++i)
    if ((phi != NULL && phi == re->lcssa_phi)
	|| (stmt == re->producer || stmt == re->consumer))
      return re;

  return NULL;
}

/* Return true if current loop_cand be interchanged.  ILOOP is not NULL if
   current loop_cand is outer loop in loop nest.  */

bool
loop_cand::can_interchange_p (loop_cand *iloop)
{
  /* For now we only support at most one reduction.  */
  unsigned allowed_reduction_num = 1;

  /* Only support reduction if the loop nest to be interchanged is the
     innermostin two loops.  */
  if ((iloop == NULL && m_loop->inner != NULL)
       || (iloop != NULL && iloop->m_loop->inner != NULL))
    allowed_reduction_num = 0;

  if (m_reductions.length () > allowed_reduction_num
      || (m_reductions.length () == 1
	  && m_reductions[0]->type == UNKNOWN_RTYPE))
    return false;

  /* Only support lcssa PHI node which is for reduction.  */
  if (m_lcssa_nodes.length () > allowed_reduction_num)
    return false;

  /* Check if basic block has any unsupported operation.  Note basic blocks
     of inner loops are not checked here.  */
  for (unsigned i = 0; i < m_loop->num_nodes; i++)
    {
      basic_block bb = m_bbs[i];
      gphi_iterator psi;
      gimple_stmt_iterator gsi;

      /* Skip basic blocks of inner loops.  */
      if (bb->loop_father != m_loop)
       continue;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt))
	    continue;

	  if (gimple_has_side_effects (stmt))
	    return false;

	  m_num_stmts++;
	  if (gcall *call = dyn_cast <gcall *> (stmt))
	    {
	      /* In basic block of outer loop, the call should be cheap since
		 it will be moved to inner loop.  */
	      if (iloop != NULL
		  && !gimple_inexpensive_call_p (call))
		return false;
	      continue;
	    }

	  if (!iloop || !gimple_vuse (stmt))
	    continue;

	  /* Support stmt accessing memory in outer loop only if it is for
	     inner loop's reduction.  */
	  if (iloop->find_reduction_by_stmt (stmt))
	    continue;

	  tree lhs;
	  /* Support loop invariant memory reference if it's only used once by
	     inner loop.  */
	  /* ???  How's this checking for invariantness?  */
	  if (gimple_assign_single_p (stmt)
	      && (lhs = gimple_assign_lhs (stmt)) != NULL_TREE
	      && TREE_CODE (lhs) == SSA_NAME
	      && single_use_in_loop (lhs, iloop->m_loop))
	    continue;

	  return false;
	}
      /* Check if loop has too many stmts.  */
      if (m_num_stmts > MAX_NUM_STMT)
	return false;

      /* Allow PHI nodes in any basic block of inner loop, PHI nodes in outer
	 loop's header, or PHI nodes in dest bb of inner loop's exit edge.  */
      if (!iloop || bb == m_loop->header
	  || bb == iloop->m_exit->dest)
	continue;

      /* Don't allow any other PHI nodes.  */
      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
	if (!virtual_operand_p (PHI_RESULT (psi.phi ())))
	  return false;
    }

  return true;
}

/* Programmers and optimizers (like loop store motion) may optimize code:

     for (int i = 0; i < N; i++)
       for (int j = 0; j < N; j++)
	 a[i] += b[j][i] * c[j][i];

   into reduction:

     for (int i = 0; i < N; i++)
       {
	 // producer.  Note sum can be intitialized to a constant.
	 int sum = a[i];
	 for (int j = 0; j < N; j++)
	   {
	     sum += b[j][i] * c[j][i];
	   }
	 // consumer.
	 a[i] = sum;
       }

   The result code can't be interchanged without undoing the optimization.
   This function classifies this kind reduction and records information so
   that we can undo the store motion during interchange.  */

void
loop_cand::classify_simple_reduction (reduction_p re)
{
  gimple *producer, *consumer;

  /* Check init variable of reduction and how it is initialized.  */
  if (TREE_CODE (re->init) == SSA_NAME)
    {
      producer = SSA_NAME_DEF_STMT (re->init);
      re->producer = producer;
      basic_block bb = gimple_bb (producer);
      if (!bb || bb->loop_father != m_outer)
	return;

      if (!gimple_assign_load_p (producer))
	return;

      re->init_ref = gimple_assign_rhs1 (producer);
    }
  else if (CONSTANT_CLASS_P (re->init))
    m_const_init_reduc++;
  else
    return;

  /* Check how reduction variable is used.  */
  consumer = single_use_in_loop (PHI_RESULT (re->lcssa_phi), m_outer);
  if (!consumer
      || !gimple_store_p (consumer))
    return;

  re->fini_ref = gimple_get_lhs (consumer);
  re->consumer = consumer;

  /* Simple reduction with constant initializer.  */
  if (!re->init_ref)
    {
      gcc_assert (CONSTANT_CLASS_P (re->init));
      re->init_ref = unshare_expr (re->fini_ref);
    }

  /* Require memory references in producer and consumer are the same so
     that we can undo reduction during interchange.  */
  if (re->init_ref && !operand_equal_p (re->init_ref, re->fini_ref, 0))
    return;

  re->type = SIMPLE_RTYPE;
}

/* Analyze reduction variable VAR for inner loop of the loop nest to be
   interchanged.  Return true if analysis succeeds.  */

bool
loop_cand::analyze_iloop_reduction_var (tree var)
{
  gphi *phi = as_a <gphi *> (SSA_NAME_DEF_STMT (var));
  gphi *lcssa_phi = NULL, *use_phi;
  tree init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (m_loop));
  tree next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (m_loop));
  reduction_p re;
  gimple *stmt, *next_def, *single_use = NULL;
  use_operand_p use_p;
  imm_use_iterator iterator;

  if (TREE_CODE (next) != SSA_NAME)
    return false;

  next_def = SSA_NAME_DEF_STMT (next);
  basic_block bb = gimple_bb (next_def);
  if (!bb || !flow_bb_inside_loop_p (m_loop, bb))
    return false;

  /* In restricted reduction, the var is (and must be) used in defining
     the updated var.  The process can be depicted as below:

		var ;; = PHI<init, next>
		 |
		 |
		 v
      +---------------------+
      | reduction operators | <-- other operands
      +---------------------+
		 |
		 |
		 v
		next

     In terms loop interchange, we don't change how NEXT is computed based
     on VAR and OTHER OPERANDS.  In case of double reduction in loop nest
     to be interchanged, we don't changed it at all.  In the case of simple
     reduction in inner loop, we only make change how VAR/NEXT is loaded or
     stored.  With these conditions, we can relax restrictions on reduction
     in a way that reduction operation is seen as black box.  In general,
     we can ignore reassociation of reduction operator; we can handle fake
     reductions in which VAR is not even used to compute NEXT.  */
  if (! single_imm_use (var, &use_p, &single_use)
      || ! flow_bb_inside_loop_p (m_loop, gimple_bb (single_use)))
    return false;

  /* Check the reduction operation.  We require a left-associative operation.
     For FP math we also need to be allowed to associate operations.  */
  if (gassign *ass = dyn_cast <gassign *> (single_use))
    {
      enum tree_code code = gimple_assign_rhs_code (ass);
      if (! (associative_tree_code (code)
	     || (code == MINUS_EXPR
		 && use_p->use == gimple_assign_rhs1_ptr (ass)))
	  || (FLOAT_TYPE_P (TREE_TYPE (var))
	      && ! flag_associative_math))
	return false;
    }
  else
    return false;

  /* Handle and verify a series of stmts feeding the reduction op.  */
  if (single_use != next_def
      && !check_reduction_path (dump_user_location_t (), m_loop, phi, next,
				gimple_assign_rhs_code (single_use)))
    return false;

  /* Only support cases in which INIT is used in inner loop.  */
  if (TREE_CODE (init) == SSA_NAME)
    FOR_EACH_IMM_USE_FAST (use_p, iterator, init)
      {
	stmt = USE_STMT (use_p);
	if (is_gimple_debug (stmt))
	  continue;

	if (!flow_bb_inside_loop_p (m_loop, gimple_bb (stmt)))
	  return false;
      }

  FOR_EACH_IMM_USE_FAST (use_p, iterator, next)
    {
      stmt = USE_STMT (use_p);
      if (is_gimple_debug (stmt))
	continue;

      /* Or else it's used in PHI itself.  */
      use_phi = dyn_cast <gphi *> (stmt);
      if (use_phi == phi)
	continue;

      if (use_phi != NULL
	  && lcssa_phi == NULL
	  && gimple_bb (stmt) == m_exit->dest
	  && PHI_ARG_DEF_FROM_EDGE (use_phi, m_exit) == next)
	lcssa_phi = use_phi;
      else
	return false;
    }
  if (!lcssa_phi)
    return false;

  re = XCNEW (struct reduction);
  re->var = var;
  re->init = init;
  re->next = next;
  re->phi = phi;
  re->lcssa_phi = lcssa_phi;

  classify_simple_reduction (re);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_reduction (re);

  m_reductions.safe_push (re);
  return true;
}

/* Analyze reduction variable VAR for outer loop of the loop nest to be
   interchanged.  ILOOP is not NULL and points to inner loop.  For the
   moment, we only support double reduction for outer loop, like:

     for (int i = 0; i < n; i++)
       {
	 int sum = 0;

	 for (int j = 0; j < n; j++)    // outer loop
	   for (int k = 0; k < n; k++)  // inner loop
	     sum += a[i][k]*b[k][j];

	 s[i] = sum;
       }

   Note the innermost two loops are the loop nest to be interchanged.
   Return true if analysis succeeds.  */

bool
loop_cand::analyze_oloop_reduction_var (loop_cand *iloop, tree var)
{
  gphi *phi = as_a <gphi *> (SSA_NAME_DEF_STMT (var));
  gphi *lcssa_phi = NULL, *use_phi;
  tree init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (m_loop));
  tree next = PHI_ARG_DEF_FROM_EDGE (phi, loop_latch_edge (m_loop));
  reduction_p re;
  gimple *stmt, *next_def;
  use_operand_p use_p;
  imm_use_iterator iterator;

  if (TREE_CODE (next) != SSA_NAME)
    return false;

  next_def = SSA_NAME_DEF_STMT (next);
  basic_block bb = gimple_bb (next_def);
  if (!bb || !flow_bb_inside_loop_p (m_loop, bb))
    return false;

  /* Find inner loop's simple reduction that uses var as initializer.  */
  reduction_p inner_re = NULL;
  for (unsigned i = 0; iloop->m_reductions.iterate (i, &inner_re); ++i)
    if (inner_re->init == var || operand_equal_p (inner_re->init, var, 0))
      break;

  if (inner_re == NULL
      || inner_re->type != UNKNOWN_RTYPE
      || inner_re->producer != phi)
    return false;

  /* In case of double reduction, outer loop's reduction should be updated
     by inner loop's simple reduction.  */
  if (next_def != inner_re->lcssa_phi)
    return false;

  /* Outer loop's reduction should only be used to initialize inner loop's
     simple reduction.  */
  if (! single_imm_use (var, &use_p, &stmt)
      || stmt != inner_re->phi)
    return false;

  /* Check this reduction is correctly used outside of loop via lcssa phi.  */
  FOR_EACH_IMM_USE_FAST (use_p, iterator, next)
    {
      stmt = USE_STMT (use_p);
      if (is_gimple_debug (stmt))
	continue;

      /* Or else it's used in PHI itself.  */
      use_phi = dyn_cast <gphi *> (stmt);
      if (use_phi == phi)
	continue;

      if (lcssa_phi == NULL
	  && use_phi != NULL
	  && gimple_bb (stmt) == m_exit->dest
	  && PHI_ARG_DEF_FROM_EDGE (use_phi, m_exit) == next)
	lcssa_phi = use_phi;
      else
	return false;
    }
  if (!lcssa_phi)
    return false;

  re = XCNEW (struct reduction);
  re->var = var;
  re->init = init;
  re->next = next;
  re->phi = phi;
  re->lcssa_phi = lcssa_phi;
  re->type = DOUBLE_RTYPE;
  inner_re->type = DOUBLE_RTYPE;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_reduction (re);

  m_reductions.safe_push (re);
  return true;
}

/* Return true if VAR is induction variable of current loop whose scev is
   specified by CHREC.  */

bool
loop_cand::analyze_induction_var (tree var, tree chrec)
{
  gphi *phi = as_a <gphi *> (SSA_NAME_DEF_STMT (var));
  tree init = PHI_ARG_DEF_FROM_EDGE (phi, loop_preheader_edge (m_loop));

  /* Var is loop invariant, though it's unlikely to happen.  */
  if (tree_does_not_contain_chrecs (chrec))
    {
      struct induction *iv = XCNEW (struct induction);
      iv->var = var;
      iv->init_val = init;
      iv->init_expr = chrec;
      iv->step = build_int_cst (TREE_TYPE (chrec), 0);
      m_inductions.safe_push (iv);
      return true;
    }

  if (TREE_CODE (chrec) != POLYNOMIAL_CHREC
      || CHREC_VARIABLE (chrec) != (unsigned) m_loop->num
      || tree_contains_chrecs (CHREC_LEFT (chrec), NULL)
      || tree_contains_chrecs (CHREC_RIGHT (chrec), NULL))
    return false;

  struct induction *iv = XCNEW (struct induction);
  iv->var = var;
  iv->init_val = init;
  iv->init_expr = CHREC_LEFT (chrec);
  iv->step = CHREC_RIGHT (chrec);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_induction (m_loop, iv);

  m_inductions.safe_push (iv);
  return true;
}

/* Return true if all loop carried variables defined in loop header can
   be successfully analyzed.  */

bool
loop_cand::analyze_carried_vars (loop_cand *iloop)
{
  edge e = loop_preheader_edge (m_outer);
  gphi_iterator gsi;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nLoop(%d) carried vars:\n", m_loop->num);

  for (gsi = gsi_start_phis (m_loop->header); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      tree var = PHI_RESULT (phi);
      if (virtual_operand_p (var))
	continue;

      tree chrec = analyze_scalar_evolution (m_loop, var);
      chrec = instantiate_scev (e, m_loop, chrec);

      /* Analyze var as reduction variable.  */
      if (chrec_contains_undetermined (chrec)
	  || chrec_contains_symbols_defined_in_loop (chrec, m_outer->num))
	{
	  if (iloop && !analyze_oloop_reduction_var (iloop, var))
	    return false;
	  if (!iloop && !analyze_iloop_reduction_var (var))
	    return false;
	}
      /* Analyze var as induction variable.  */
      else if (!analyze_induction_var (var, chrec))
	return false;
    }

  return true;
}

/* Return TRUE if loop closed PHI nodes can be analyzed successfully.  */

bool
loop_cand::analyze_lcssa_phis (void)
{
  gphi_iterator gsi;
  for (gsi = gsi_start_phis (m_exit->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      if (virtual_operand_p (PHI_RESULT (phi)))
	continue;

      /* TODO: We only support lcssa phi for reduction for now.  */
      if (!find_reduction_by_stmt (phi))
	return false;
    }

  return true;
}

/* CONSUMER is a stmt in BB storing reduction result into memory object.
   When the reduction is intialized from constant value, we need to add
   a stmt loading from the memory object to target basic block in inner
   loop during undoing the reduction.  Problem is that memory reference
   may use ssa variables not dominating the target basic block.  This
   function finds all stmts on which CONSUMER depends in basic block BB,
   records and returns them via STMTS.  */

static void
find_deps_in_bb_for_stmt (gimple_seq *stmts, basic_block bb, gimple *consumer)
{
  auto_vec<gimple *, 4> worklist;
  use_operand_p use_p;
  ssa_op_iter iter;
  gimple *stmt, *def_stmt;
  gimple_stmt_iterator gsi;

  /* First clear flag for stmts in bb.  */
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    gimple_set_plf (gsi_stmt (gsi), GF_PLF_1, false);

  /* DFS search all depended stmts in bb and mark flag for these stmts.  */
  worklist.safe_push (consumer);
  while (!worklist.is_empty ())
    {
      stmt = worklist.pop ();
      FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
	{
	  def_stmt = SSA_NAME_DEF_STMT (USE_FROM_PTR (use_p));

	  if (is_a <gphi *> (def_stmt)
	      || gimple_bb (def_stmt) != bb
	      || gimple_plf (def_stmt, GF_PLF_1))
	    continue;

	  worklist.safe_push (def_stmt);
	}
      gimple_set_plf (stmt, GF_PLF_1, true);
    }
  for (gsi = gsi_start_nondebug_bb (bb);
       !gsi_end_p (gsi) && (stmt = gsi_stmt (gsi)) != consumer;)
    {
      /* Move dep stmts to sequence STMTS.  */
      if (gimple_plf (stmt, GF_PLF_1))
	{
	  gsi_remove (&gsi, false);
	  gimple_seq_add_stmt_without_update (stmts, stmt);
	}
      else
	gsi_next_nondebug (&gsi);
    }
}

/* User can write, optimizers can generate simple reduction RE for inner
   loop.  In order to make interchange valid, we have to undo reduction by
   moving producer and consumer stmts into the inner loop.  For example,
   below code:

     init = MEM_REF[idx];		//producer
     loop:
       var = phi<init, next>
       next = var op ...
     reduc_sum = phi<next>
     MEM_REF[idx] = reduc_sum		//consumer

   is transformed into:

     loop:
       new_var = MEM_REF[idx];		//producer after moving
       next = new_var op ...
       MEM_REF[idx] = next;		//consumer after moving

   Note if the reduction variable is initialized to constant, like:

     var = phi<0.0, next>

   we compute new_var as below:

     loop:
       tmp = MEM_REF[idx];
       new_var = !first_iteration ? tmp : 0.0;

   so that the initial const is used in the first iteration of loop.  Also
   record ssa variables for dead code elimination in DCE_SEEDS.  */

void
loop_cand::undo_simple_reduction (reduction_p re, bitmap dce_seeds)
{
  gimple *stmt;
  gimple_stmt_iterator from, to = gsi_after_labels (m_loop->header);
  gimple_seq stmts = NULL;
  tree new_var;

  /* Prepare the initialization stmts and insert it to inner loop.  */
  if (re->producer != NULL)
    {
      gimple_set_vuse (re->producer, NULL_TREE);
      from = gsi_for_stmt (re->producer);
      gsi_remove (&from, false);
      gimple_seq_add_stmt_without_update (&stmts, re->producer);
      new_var = re->init;
    }
  else
    {
      /* Find all stmts on which expression "MEM_REF[idx]" depends.  */
      find_deps_in_bb_for_stmt (&stmts, gimple_bb (re->consumer), re->consumer);
      /* Because we generate new stmt loading from the MEM_REF to TMP.  */
      tree cond, tmp = copy_ssa_name (re->var);
      stmt = gimple_build_assign (tmp, re->init_ref);
      gimple_seq_add_stmt_without_update (&stmts, stmt);

      /* Init new_var to MEM_REF or CONST depending on if it is the first
	 iteration.  */
      induction_p iv = m_inductions[0];
      cond = fold_build2 (NE_EXPR, boolean_type_node, iv->var, iv->init_val);
      new_var = copy_ssa_name (re->var);
      stmt = gimple_build_assign (new_var, COND_EXPR, cond, tmp, re->init);
      gimple_seq_add_stmt_without_update (&stmts, stmt);
    }
  gsi_insert_seq_before (&to, stmts, GSI_SAME_STMT);

  /* Replace all uses of reduction var with new variable.  */
  use_operand_p use_p;
  imm_use_iterator iterator;
  FOR_EACH_IMM_USE_STMT (stmt, iterator, re->var)
    {
      FOR_EACH_IMM_USE_ON_STMT (use_p, iterator)
	SET_USE (use_p, new_var);

      update_stmt (stmt);
    }

  /* Move consumer stmt into inner loop, just after reduction next's def.  */
  unlink_stmt_vdef (re->consumer);
  release_ssa_name (gimple_vdef (re->consumer));
  gimple_set_vdef (re->consumer, NULL_TREE);
  gimple_set_vuse (re->consumer, NULL_TREE);
  gimple_assign_set_rhs1 (re->consumer, re->next);
  from = gsi_for_stmt (re->consumer);
  to = gsi_for_stmt (SSA_NAME_DEF_STMT (re->next));
  gsi_move_after (&from, &to);

  /* Mark the reduction variables for DCE.  */
  bitmap_set_bit (dce_seeds, SSA_NAME_VERSION (re->var));
  bitmap_set_bit (dce_seeds, SSA_NAME_VERSION (PHI_RESULT (re->lcssa_phi)));
}

/* Free DATAREFS and its auxiliary memory.  */

static void
free_data_refs_with_aux (vec<data_reference_p> datarefs)
{
  data_reference_p dr;
  for (unsigned i = 0; datarefs.iterate (i, &dr); ++i)
    if (dr->aux != NULL)
      {
	DR_ACCESS_STRIDE (dr)->release ();
	delete (vec<tree> *) dr->aux;
      }

  free_data_refs (datarefs);
}

/* Class for loop interchange transformation.  */

class tree_loop_interchange
{
public:
  tree_loop_interchange (vec<struct loop *> loop_nest)
    : m_loop_nest (loop_nest), m_niters_iv_var (NULL_TREE),
      m_dce_seeds (BITMAP_ALLOC (NULL)) { }
  ~tree_loop_interchange () { BITMAP_FREE (m_dce_seeds); }
  bool interchange (vec<data_reference_p>, vec<ddr_p>);

private:
  void update_data_info (unsigned, unsigned, vec<data_reference_p>, vec<ddr_p>);
  bool valid_data_dependences (unsigned, unsigned, vec<ddr_p>);
  void interchange_loops (loop_cand &, loop_cand &);
  void map_inductions_to_loop (loop_cand &, loop_cand &);
  void move_code_to_inner_loop (struct loop *, struct loop *, basic_block *);

  /* The whole loop nest in which interchange is ongoing.  */
  vec<struct loop *> m_loop_nest;
  /* We create new IV which is only used in loop's exit condition check.
     In case of 3-level loop nest interchange, when we interchange the
     innermost two loops, new IV created in the middle level loop does
     not need to be preserved in interchanging the outermost two loops
     later.  We record the IV so that it can be skipped.  */
  tree m_niters_iv_var;
  /* Bitmap of seed variables for dead code elimination after interchange.  */
  bitmap m_dce_seeds;
};

/* Update data refs' access stride and dependence information after loop
   interchange.  I_IDX/O_IDX gives indices of interchanged loops in loop
   nest.  DATAREFS are data references.  DDRS are data dependences.  */

void
tree_loop_interchange::update_data_info (unsigned i_idx, unsigned o_idx,
					 vec<data_reference_p> datarefs,
					 vec<ddr_p> ddrs)
{
  struct data_reference *dr;
  struct data_dependence_relation *ddr;

  /* Update strides of data references.  */
  for (unsigned i = 0; datarefs.iterate (i, &dr); ++i)
    {
      vec<tree> *stride = DR_ACCESS_STRIDE (dr);
      gcc_assert (stride->length () > i_idx);
      std::swap ((*stride)[i_idx], (*stride)[o_idx]);
    }
  /* Update data dependences.  */
  for (unsigned i = 0; ddrs.iterate (i, &ddr); ++i)
    if (DDR_ARE_DEPENDENT (ddr) != chrec_known)
      {
        for (unsigned j = 0; j < DDR_NUM_DIST_VECTS (ddr); ++j)
	  {
	    lambda_vector dist_vect = DDR_DIST_VECT (ddr, j);
	    std::swap (dist_vect[i_idx], dist_vect[o_idx]);
	  }
      }
}

/* Check data dependence relations, return TRUE if it's valid to interchange
   two loops specified by I_IDX/O_IDX.  Theoretically, interchanging the two
   loops is valid only if dist vector, after interchanging, doesn't have '>'
   as the leftmost non-'=' direction.  Practically, this function have been
   conservative here by not checking some valid cases.  */

bool
tree_loop_interchange::valid_data_dependences (unsigned i_idx, unsigned o_idx,
					       vec<ddr_p> ddrs)
{
  struct data_dependence_relation *ddr;

  for (unsigned i = 0; ddrs.iterate (i, &ddr); ++i)
    {
      /* Skip no-dependence case.  */
      if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
	continue;

      for (unsigned j = 0; j < DDR_NUM_DIST_VECTS (ddr); ++j)
	{
	  lambda_vector dist_vect = DDR_DIST_VECT (ddr, j);
	  unsigned level = dependence_level (dist_vect, m_loop_nest.length ());

	  /* If there is no carried dependence.  */
	  if (level == 0)
	    continue;

	  level --;

	  /* If dependence is not carried by any loop in between the two
	     loops [oloop, iloop] to interchange.  */
	  if (level < o_idx || level > i_idx)
	    continue;

	  /* Be conservative, skip case if either direction at i_idx/o_idx
	     levels is not '=' or '<'.  */
	  if (dist_vect[i_idx] < 0 || dist_vect[o_idx] < 0)
	    return false;
	}
    }

  return true;
}

/* Interchange two loops specified by ILOOP and OLOOP.  */

void
tree_loop_interchange::interchange_loops (loop_cand &iloop, loop_cand &oloop)
{
  reduction_p re;
  gimple_stmt_iterator gsi;
  tree i_niters, o_niters, var_after;

  /* Undo inner loop's simple reduction.  */
  for (unsigned i = 0; iloop.m_reductions.iterate (i, &re); ++i)
    if (re->type != DOUBLE_RTYPE)
      {
	if (re->producer)
	  reset_debug_uses (re->producer);

	iloop.undo_simple_reduction (re, m_dce_seeds);
      }

  /* Only need to reset debug uses for double reduction.  */
  for (unsigned i = 0; oloop.m_reductions.iterate (i, &re); ++i)
    {
      gcc_assert (re->type == DOUBLE_RTYPE);
      reset_debug_uses (SSA_NAME_DEF_STMT (re->var));
      reset_debug_uses (SSA_NAME_DEF_STMT (re->next));
    }

  /* Prepare niters for both loops.  */
  struct loop *loop_nest = m_loop_nest[0];
  edge instantiate_below = loop_preheader_edge (loop_nest);
  gsi = gsi_last_bb (loop_preheader_edge (loop_nest)->src);
  i_niters = number_of_latch_executions (iloop.m_loop);
  i_niters = analyze_scalar_evolution (loop_outer (iloop.m_loop), i_niters);
  i_niters = instantiate_scev (instantiate_below, loop_outer (iloop.m_loop),
			       i_niters);
  i_niters = force_gimple_operand_gsi (&gsi, unshare_expr (i_niters), true,
				       NULL_TREE, false, GSI_CONTINUE_LINKING);
  o_niters = number_of_latch_executions (oloop.m_loop);
  if (oloop.m_loop != loop_nest)
    {
      o_niters = analyze_scalar_evolution (loop_outer (oloop.m_loop), o_niters);
      o_niters = instantiate_scev (instantiate_below, loop_outer (oloop.m_loop),
				   o_niters);
    }
  o_niters = force_gimple_operand_gsi (&gsi, unshare_expr (o_niters), true,
				       NULL_TREE, false, GSI_CONTINUE_LINKING);

  /* Move src's code to tgt loop.  This is necessary when src is the outer
     loop and tgt is the inner loop.  */
  move_code_to_inner_loop (oloop.m_loop, iloop.m_loop, oloop.m_bbs);

  /* Map outer loop's IV to inner loop, and vice versa.  */
  map_inductions_to_loop (oloop, iloop);
  map_inductions_to_loop (iloop, oloop);

  /* Create canonical IV for both loops.  Note canonical IV for outer/inner
     loop is actually from inner/outer loop.  Also we record the new IV
     created for the outer loop so that it can be skipped in later loop
     interchange.  */
  create_canonical_iv (oloop.m_loop, oloop.m_exit,
		       i_niters, &m_niters_iv_var, &var_after);
  bitmap_set_bit (m_dce_seeds, SSA_NAME_VERSION (var_after));
  create_canonical_iv (iloop.m_loop, iloop.m_exit,
		       o_niters, NULL, &var_after);
  bitmap_set_bit (m_dce_seeds, SSA_NAME_VERSION (var_after));

  /* Scrap niters estimation of interchanged loops.  */
  iloop.m_loop->any_upper_bound = false;
  iloop.m_loop->any_likely_upper_bound = false;
  free_numbers_of_iterations_estimates (iloop.m_loop);
  oloop.m_loop->any_upper_bound = false;
  oloop.m_loop->any_likely_upper_bound = false;
  free_numbers_of_iterations_estimates (oloop.m_loop);

  /* Clear all cached scev information.  This is expensive but shouldn't be
     a problem given we interchange in very limited times.  */
  scev_reset_htab ();

  /* ???  The association between the loop data structure and the
     CFG changed, so what was loop N at the source level is now
     loop M.  We should think of retaining the association or breaking
     it fully by creating a new loop instead of re-using the "wrong" one.  */
}

/* Map induction variables of SRC loop to TGT loop.  The function firstly
   creates the same IV of SRC loop in TGT loop, then deletes the original
   IV and re-initialize it using the newly created IV.  For example, loop
   nest:

     for (i = 0; i < N; i++)
       for (j = 0; j < M; j++)
	 {
	   //use of i;
	   //use of j;
	 }

   will be transformed into:

     for (jj = 0; jj < M; jj++)
       for (ii = 0; ii < N; ii++)
	 {
	   //use of ii;
	   //use of jj;
	 }

   after loop interchange.  */

void
tree_loop_interchange::map_inductions_to_loop (loop_cand &src, loop_cand &tgt)
{
  induction_p iv;
  edge e = tgt.m_exit;
  gimple_stmt_iterator incr_pos = gsi_last_bb (e->src), gsi;

  /* Map source loop's IV to target loop.  */
  for (unsigned i = 0; src.m_inductions.iterate (i, &iv); ++i)
    {
      gimple *use_stmt, *stmt = SSA_NAME_DEF_STMT (iv->var);
      gcc_assert (is_a <gphi *> (stmt));

      use_operand_p use_p;
      /* Only map original IV to target loop.  */
      if (m_niters_iv_var != iv->var)
	{
	  /* Map the IV by creating the same one in target loop.  */
	  tree var_before, var_after;
	  tree base = unshare_expr (iv->init_expr);
	  tree step = unshare_expr (iv->step);
	  create_iv (base, step, SSA_NAME_VAR (iv->var),
		     tgt.m_loop, &incr_pos, false, &var_before, &var_after);
	  bitmap_set_bit (m_dce_seeds, SSA_NAME_VERSION (var_before));
	  bitmap_set_bit (m_dce_seeds, SSA_NAME_VERSION (var_after));

	  /* Replace uses of the original IV var with newly created IV var.  */
	  imm_use_iterator imm_iter;
	  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, iv->var)
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
		SET_USE (use_p, var_before);

	      update_stmt (use_stmt);
	    }
	}

      /* Mark all uses for DCE.  */
      ssa_op_iter op_iter;
      FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, op_iter, SSA_OP_USE)
	{
	  tree use = USE_FROM_PTR (use_p);
	  if (TREE_CODE (use) == SSA_NAME
	      && ! SSA_NAME_IS_DEFAULT_DEF (use))
	    bitmap_set_bit (m_dce_seeds, SSA_NAME_VERSION (use));
	}

      /* Delete definition of the original IV in the source loop.  */
      gsi = gsi_for_stmt (stmt);
      remove_phi_node (&gsi, true);
    }
}

/* Move stmts of outer loop to inner loop.  */

void
tree_loop_interchange::move_code_to_inner_loop (struct loop *outer,
						struct loop *inner,
						basic_block *outer_bbs)
{
  basic_block oloop_exit_bb = single_exit (outer)->src;
  gimple_stmt_iterator gsi, to;

  for (unsigned i = 0; i < outer->num_nodes; i++)
    {
      basic_block bb = outer_bbs[i];

      /* Skip basic blocks of inner loop.  */
      if (flow_bb_inside_loop_p (inner, bb))
	continue;

      /* Move code from header/latch to header/latch.  */
      if (bb == outer->header)
	to = gsi_after_labels (inner->header);
      else if (bb == outer->latch)
	to = gsi_after_labels (inner->latch);
      else
	/* Otherwise, simply move to exit->src.  */
	to = gsi_last_bb (single_exit (inner)->src);

      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (oloop_exit_bb == bb
	      && stmt == gsi_stmt (gsi_last_bb (oloop_exit_bb)))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  if (gimple_vuse (stmt))
	    gimple_set_vuse (stmt, NULL_TREE);
	  if (gimple_vdef (stmt))
	    {
	      unlink_stmt_vdef (stmt);
	      release_ssa_name (gimple_vdef (stmt));
	      gimple_set_vdef (stmt, NULL_TREE);
	    }

	  reset_debug_uses (stmt);
	  gsi_move_before (&gsi, &to);
	}
    }
}

/* Given data reference DR in LOOP_NEST, the function computes DR's access
   stride at each level of loop from innermost LOOP to outer.  On success,
   it saves access stride at each level loop in a vector which is pointed
   by DR->aux.  For example:

     int arr[100][100][100];
     for (i = 0; i < 100; i++)       ;(DR->aux)strides[0] = 40000
       for (j = 100; j > 0; j--)     ;(DR->aux)strides[1] = 400
	 for (k = 0; k < 100; k++)   ;(DR->aux)strides[2] = 4
	   arr[i][j - 1][k] = 0;  */

static void
compute_access_stride (struct loop *loop_nest, struct loop *loop,
		       data_reference_p dr)
{
  vec<tree> *strides = new vec<tree> ();
  basic_block bb = gimple_bb (DR_STMT (dr));

  while (!flow_bb_inside_loop_p (loop, bb))
    {
      strides->safe_push (build_int_cst (sizetype, 0));
      loop = loop_outer (loop);
    }
  gcc_assert (loop == bb->loop_father);

  tree ref = DR_REF (dr);
  if (TREE_CODE (ref) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (ref, 1)))
    {
      /* We can't take address of bitfields.  If the bitfield is at constant
	 offset from the start of the struct, just use address of the
	 struct, for analysis of the strides that shouldn't matter.  */
      if (!TREE_OPERAND (ref, 2)
	  || TREE_CODE (TREE_OPERAND (ref, 2)) == INTEGER_CST)
	ref = TREE_OPERAND (ref, 0);
      /* Otherwise, if we have a bit field representative, use that.  */
      else if (DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (ref, 1))
	       != NULL_TREE)
	{
	  tree repr = DECL_BIT_FIELD_REPRESENTATIVE (TREE_OPERAND (ref, 1));
	  ref = build3 (COMPONENT_REF, TREE_TYPE (repr), TREE_OPERAND (ref, 0),
			repr, TREE_OPERAND (ref, 2));
	}
      /* Otherwise punt.  */
      else
	{
	  dr->aux = strides;
	  return;
	}
    }
  tree scev_base = build_fold_addr_expr (ref);
  tree scev = analyze_scalar_evolution (loop, scev_base);
  scev = instantiate_scev (loop_preheader_edge (loop_nest), loop, scev);
  if (! chrec_contains_undetermined (scev))
    {
      tree sl = scev;
      struct loop *expected = loop;
      while (TREE_CODE (sl) == POLYNOMIAL_CHREC)
	{
	  struct loop *sl_loop = get_chrec_loop (sl);
	  while (sl_loop != expected)
	    {
	      strides->safe_push (size_int (0));
	      expected = loop_outer (expected);
	    }
	  strides->safe_push (CHREC_RIGHT (sl));
	  sl = CHREC_LEFT (sl);
	  expected = loop_outer (expected);
	}
      if (! tree_contains_chrecs (sl, NULL))
	while (expected != loop_outer (loop_nest))
	  {
	    strides->safe_push (size_int (0));
	    expected = loop_outer (expected);
	  }
    }

  dr->aux = strides;
}

/* Given loop nest LOOP_NEST with innermost LOOP, the function computes
   access strides with respect to each level loop for all data refs in
   DATAREFS from inner loop to outer loop.  On success, it returns the
   outermost loop that access strides can be computed successfully for
   all data references.  If access strides cannot be computed at least
   for two levels of loop for any data reference, it returns NULL.  */

static struct loop *
compute_access_strides (struct loop *loop_nest, struct loop *loop,
			vec<data_reference_p> datarefs)
{
  unsigned i, j, num_loops = (unsigned) -1;
  data_reference_p dr;
  vec<tree> *stride;

  for (i = 0; datarefs.iterate (i, &dr); ++i)
    {
      compute_access_stride (loop_nest, loop, dr);
      stride = DR_ACCESS_STRIDE (dr);
      if (stride->length () < num_loops)
	{
	  num_loops = stride->length ();
	  if (num_loops < 2)
	    return NULL;
	}
    }

  for (i = 0; datarefs.iterate (i, &dr); ++i)
    {
      stride = DR_ACCESS_STRIDE (dr);
      if (stride->length () > num_loops)
	stride->truncate (num_loops);

      for (j = 0; j < (num_loops >> 1); ++j)
	std::swap ((*stride)[j], (*stride)[num_loops - j - 1]);
    }

  loop = superloop_at_depth (loop, loop_depth (loop) + 1 - num_loops);
  gcc_assert (loop_nest == loop || flow_loop_nested_p (loop_nest, loop));
  return loop;
}

/* Prune access strides for data references in DATAREFS by removing strides
   of loops that isn't in current LOOP_NEST.  */

static void
prune_access_strides_not_in_loop (struct loop *loop_nest,
				  struct loop *innermost,
				  vec<data_reference_p> datarefs)
{
  data_reference_p dr;
  unsigned num_loops = loop_depth (innermost) - loop_depth (loop_nest) + 1;
  gcc_assert (num_loops > 1);

  /* Block remove strides of loops that is not in current loop nest.  */
  for (unsigned i = 0; datarefs.iterate (i, &dr); ++i)
    {
      vec<tree> *stride = DR_ACCESS_STRIDE (dr);
      if (stride->length () > num_loops)
	stride->block_remove (0, stride->length () - num_loops);
    }
}

/* Dump access strides for all DATAREFS.  */

static void
dump_access_strides (vec<data_reference_p> datarefs)
{
  data_reference_p dr;
  fprintf (dump_file, "Access Strides for DRs:\n");
  for (unsigned i = 0; datarefs.iterate (i, &dr); ++i)
    {
      fprintf (dump_file, "  ");
      print_generic_expr (dump_file, DR_REF (dr), TDF_SLIM);
      fprintf (dump_file, ":\t\t<");

      vec<tree> *stride = DR_ACCESS_STRIDE (dr);
      unsigned num_loops = stride->length ();
      for (unsigned j = 0; j < num_loops; ++j)
	{
	  print_generic_expr (dump_file, (*stride)[j], TDF_SLIM);
	  fprintf (dump_file, "%s", (j < num_loops - 1) ? ",\t" : ">\n");
	}
    }
}

/* Return true if it's profitable to interchange two loops whose index
   in whole loop nest vector are I_IDX/O_IDX respectively.  The function
   computes and compares three types information from all DATAREFS:
     1) Access stride for loop I_IDX and O_IDX.
     2) Number of invariant memory references with respect to I_IDX before
	and after loop interchange.
     3) Flags indicating if all memory references access sequential memory
	in ILOOP, before and after loop interchange.
   If INNMOST_LOOP_P is true, the two loops for interchanging are the two
   innermost loops in loop nest.  This function also dumps information if
   DUMP_INFO_P is true.  */

static bool
should_interchange_loops (unsigned i_idx, unsigned o_idx,
			  vec<data_reference_p> datarefs,
			  unsigned i_stmt_cost, unsigned o_stmt_cost,
			  bool innermost_loops_p, bool dump_info_p = true)
{
  unsigned HOST_WIDE_INT ratio;
  unsigned i, j, num_old_inv_drs = 0, num_new_inv_drs = 0;
  struct data_reference *dr;
  bool all_seq_dr_before_p = true, all_seq_dr_after_p = true;
  widest_int iloop_strides = 0, oloop_strides = 0;
  unsigned num_unresolved_drs = 0;
  unsigned num_resolved_ok_drs = 0;
  unsigned num_resolved_not_ok_drs = 0;

  if (dump_info_p && dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nData ref strides:\n\tmem_ref:\t\tiloop\toloop\n");

  for (i = 0; datarefs.iterate (i, &dr); ++i)
    {
      vec<tree> *stride = DR_ACCESS_STRIDE (dr);
      tree iloop_stride = (*stride)[i_idx], oloop_stride = (*stride)[o_idx];

      bool subloop_stride_p = false;
      /* Data ref can't be invariant or sequential access at current loop if
	 its address changes with respect to any subloops.  */
      for (j = i_idx + 1; j < stride->length (); ++j)
	if (!integer_zerop ((*stride)[j]))
	  {
	    subloop_stride_p = true;
	    break;
	  }

      if (integer_zerop (iloop_stride))
	{
	  if (!subloop_stride_p)
	    num_old_inv_drs++;
	}
      if (integer_zerop (oloop_stride))
	{
	  if (!subloop_stride_p)
	    num_new_inv_drs++;
	}

      if (TREE_CODE (iloop_stride) == INTEGER_CST
	  && TREE_CODE (oloop_stride) == INTEGER_CST)
	{
	  iloop_strides = wi::add (iloop_strides, wi::to_widest (iloop_stride));
	  oloop_strides = wi::add (oloop_strides, wi::to_widest (oloop_stride));
	}
      else if (multiple_of_p (TREE_TYPE (iloop_stride),
			      iloop_stride, oloop_stride))
	num_resolved_ok_drs++;
      else if (multiple_of_p (TREE_TYPE (iloop_stride),
			      oloop_stride, iloop_stride))
	num_resolved_not_ok_drs++;
      else
	num_unresolved_drs++;

      /* Data ref can't be sequential access if its address changes in sub
	 loop.  */
      if (subloop_stride_p)
	{
	  all_seq_dr_before_p = false;
	  all_seq_dr_after_p = false;
	  continue;
	}
      /* Track if all data references are sequential accesses before/after loop
	 interchange.  Note invariant is considered sequential here.  */
      tree access_size = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr)));
      if (all_seq_dr_before_p
	  && ! (integer_zerop (iloop_stride)
		|| operand_equal_p (access_size, iloop_stride, 0)))
	all_seq_dr_before_p = false;
      if (all_seq_dr_after_p
	  && ! (integer_zerop (oloop_stride)
		|| operand_equal_p (access_size, oloop_stride, 0)))
	all_seq_dr_after_p = false;
    }

  if (dump_info_p && dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\toverall:\t\t");
      print_decu (iloop_strides, dump_file);
      fprintf (dump_file, "\t");
      print_decu (oloop_strides, dump_file);
      fprintf (dump_file, "\n");

      fprintf (dump_file, "Invariant data ref: before(%d), after(%d)\n",
	       num_old_inv_drs, num_new_inv_drs);
      fprintf (dump_file, "All consecutive stride: before(%s), after(%s)\n",
	       all_seq_dr_before_p ? "true" : "false",
	       all_seq_dr_after_p ? "true" : "false");
      fprintf (dump_file, "OK to interchage with variable strides: %d\n",
	       num_resolved_ok_drs);
      fprintf (dump_file, "Not OK to interchage with variable strides: %d\n",
	       num_resolved_not_ok_drs);
      fprintf (dump_file, "Variable strides we cannot decide: %d\n",
	       num_unresolved_drs);
      fprintf (dump_file, "Stmt cost of inner loop: %d\n", i_stmt_cost);
      fprintf (dump_file, "Stmt cost of outer loop: %d\n", o_stmt_cost);
    }

  if (num_unresolved_drs != 0 || num_resolved_not_ok_drs != 0)
    return false;

  /* Stmts of outer loop will be moved to inner loop.  If there are two many
     such stmts, it could make inner loop costly.  Here we compare stmt cost
     between outer and inner loops.  */
  if (i_stmt_cost && o_stmt_cost
      && num_old_inv_drs + o_stmt_cost > num_new_inv_drs
      && o_stmt_cost * STMT_COST_RATIO > i_stmt_cost)
    return false;

  /* We use different stride comparison ratio for interchanging innermost
     two loops or not.  The idea is to be conservative in interchange for
     the innermost loops.  */
  ratio = innermost_loops_p ? INNER_STRIDE_RATIO : OUTER_STRIDE_RATIO;
  /* Do interchange if it gives better data locality behavior.  */
  if (wi::gtu_p (iloop_strides, wi::mul (oloop_strides, ratio)))
    return true;
  if (wi::gtu_p (iloop_strides, oloop_strides))
    {
      /* Or it creates more invariant memory references.  */
      if ((!all_seq_dr_before_p || all_seq_dr_after_p)
	  && num_new_inv_drs > num_old_inv_drs)
	return true;
      /* Or it makes all memory references sequential.  */
      if (num_new_inv_drs >= num_old_inv_drs
	  && !all_seq_dr_before_p && all_seq_dr_after_p)
	return true;
    }

  return false;
}

/* Try to interchange inner loop of a loop nest to outer level.  */

bool
tree_loop_interchange::interchange (vec<data_reference_p> datarefs,
				    vec<ddr_p> ddrs)
{
  dump_user_location_t loc = find_loop_location (m_loop_nest[0]);
  bool changed_p = false;
  /* In each iteration we try to interchange I-th loop with (I+1)-th loop.
     The overall effect is to push inner loop to outermost level in whole
     loop nest.  */
  for (unsigned i = m_loop_nest.length (); i > 1; --i)
    {
      unsigned i_idx = i - 1, o_idx = i - 2;

      /* Check validity for loop interchange.  */
      if (!valid_data_dependences (i_idx, o_idx, ddrs))
	break;

      loop_cand iloop (m_loop_nest[i_idx], m_loop_nest[o_idx]);
      loop_cand oloop (m_loop_nest[o_idx], m_loop_nest[o_idx]);

      /* Check if we can do transformation for loop interchange.  */
      if (!iloop.analyze_carried_vars (NULL)
	  || !iloop.analyze_lcssa_phis ()
	  || !oloop.analyze_carried_vars (&iloop)
	  || !oloop.analyze_lcssa_phis ()
	  || !iloop.can_interchange_p (NULL)
	  || !oloop.can_interchange_p (&iloop))
	break;

      /* Outer loop's stmts will be moved to inner loop during interchange.
	 If there are many of them, it may make inner loop very costly.  We
	 need to check number of outer loop's stmts in profit cost model of
	 interchange.  */
      int stmt_cost = oloop.m_num_stmts;
      /* Count out the exit checking stmt of outer loop.  */
      stmt_cost --;
      /* Count out IV's increasing stmt, IVOPTs takes care if it.  */
      stmt_cost -= oloop.m_inductions.length ();
      /* Count in the additional load and cond_expr stmts caused by inner
	 loop's constant initialized reduction.  */
      stmt_cost += iloop.m_const_init_reduc * 2;
      if (stmt_cost < 0)
	stmt_cost = 0;

      /* Check profitability for loop interchange.  */
      if (should_interchange_loops (i_idx, o_idx, datarefs,
				    (unsigned) iloop.m_num_stmts,
				    (unsigned) stmt_cost,
				    iloop.m_loop->inner == NULL))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Loop_pair<outer:%d, inner:%d> is interchanged\n\n",
		     oloop.m_loop->num, iloop.m_loop->num);

	  changed_p = true;
	  interchange_loops (iloop, oloop);
	  /* No need to update if there is no further loop interchange.  */
	  if (o_idx > 0)
	    update_data_info (i_idx, o_idx, datarefs, ddrs);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Loop_pair<outer:%d, inner:%d> is not interchanged\n\n",
		     oloop.m_loop->num, iloop.m_loop->num);
	}
    }
  simple_dce_from_worklist (m_dce_seeds);

  if (changed_p)
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, loc,
		     "loops interchanged in loop nest\n");

  return changed_p;
}


/* Loop interchange pass.  */

namespace {

const pass_data pass_data_linterchange =
{
  GIMPLE_PASS, /* type */
  "linterchange", /* name */
  OPTGROUP_LOOP, /* optinfo_flags */
  TV_LINTERCHANGE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_linterchange : public gimple_opt_pass
{
public:
  pass_linterchange (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_linterchange, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_linterchange (m_ctxt); }
  virtual bool gate (function *) { return flag_loop_interchange; }
  virtual unsigned int execute (function *);

}; // class pass_linterchange


/* Return true if LOOP has proper form for interchange.  We check three
   conditions in the function:
     1) In general, a loop can be interchanged only if it doesn't have
	basic blocks other than header, exit and latch besides possible
	inner loop nest.  This basically restricts loop interchange to
	below form loop nests:

          header<---+
            |       |
            v       |
        INNER_LOOP  |
            |       |
            v       |
          exit--->latch

     2) Data reference in basic block that executes in different times
	than loop head/exit is not allowed.
     3) Record the innermost outer loop that doesn't form rectangle loop
	nest with LOOP.  */

static bool
proper_loop_form_for_interchange (struct loop *loop, struct loop **min_outer)
{
  edge e0, e1, exit;

  /* Don't interchange if loop has unsupported information for the moment.  */
  if (loop->safelen > 0
      || loop->constraints != 0
      || loop->can_be_parallel
      || loop->dont_vectorize
      || loop->force_vectorize
      || loop->in_oacc_kernels_region
      || loop->orig_loop_num != 0
      || loop->simduid != NULL_TREE)
    return false;

  /* Don't interchange if outer loop has basic block other than header, exit
     and latch.  */
  if (loop->inner != NULL
      && loop->num_nodes != loop->inner->num_nodes + 3)
    return false;

  if ((exit = single_dom_exit (loop)) == NULL)
    return false;

  /* Check control flow on loop header/exit blocks.  */
  if (loop->header == exit->src
      && (EDGE_COUNT (loop->header->preds) != 2
	  || EDGE_COUNT (loop->header->succs) != 2))
    return false;
  else if (loop->header != exit->src
	   && (EDGE_COUNT (loop->header->preds) != 2
	       || !single_succ_p (loop->header)
	       || unsupported_edge (single_succ_edge (loop->header))
	       || EDGE_COUNT (exit->src->succs) != 2
	       || !single_pred_p (exit->src)
	       || unsupported_edge (single_pred_edge (exit->src))))
    return false;

  e0 = EDGE_PRED (loop->header, 0);
  e1 = EDGE_PRED (loop->header, 1);
  if (unsupported_edge (e0) || unsupported_edge (e1)
      || (e0->src != loop->latch && e1->src != loop->latch)
      || (e0->src->loop_father == loop && e1->src->loop_father == loop))
    return false;

  e0 = EDGE_SUCC (exit->src, 0);
  e1 = EDGE_SUCC (exit->src, 1);
  if (unsupported_edge (e0) || unsupported_edge (e1)
      || (e0->dest != loop->latch && e1->dest != loop->latch)
      || (e0->dest->loop_father == loop && e1->dest->loop_father == loop))
    return false;

  /* Don't interchange if any reference is in basic block that doesn't
     dominate exit block.  */
  basic_block *bbs = get_loop_body (loop);
  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];

      if (bb->loop_father != loop
	  || bb == loop->header || bb == exit->src
	  || dominated_by_p (CDI_DOMINATORS, exit->src, bb))
	continue;

      for (gimple_stmt_iterator gsi = gsi_start_nondebug_bb (bb);
	   !gsi_end_p (gsi); gsi_next_nondebug (&gsi))
	if (gimple_vuse (gsi_stmt (gsi)))
	  {
	    free (bbs);
	    return false;
	  }
    }
  free (bbs);

  tree niters = number_of_latch_executions (loop);
  niters = analyze_scalar_evolution (loop_outer (loop), niters);
  if (!niters || chrec_contains_undetermined (niters))
    return false;

  /* Record the innermost outer loop that doesn't form rectangle loop nest.  */
  for (loop_p loop2 = loop_outer (loop);
       loop2 && flow_loop_nested_p (*min_outer, loop2);
       loop2 = loop_outer (loop2))
    {
      niters = instantiate_scev (loop_preheader_edge (loop2),
				 loop_outer (loop), niters);
      if (!evolution_function_is_invariant_p (niters, loop2->num))
	{
	  *min_outer = loop2;
	  break;
	}
    }
  return true;
}

/* Return true if any two adjacent loops in loop nest [INNERMOST, LOOP_NEST]
   should be interchanged by looking into all DATAREFS.  */

static bool
should_interchange_loop_nest (struct loop *loop_nest, struct loop *innermost,
			      vec<data_reference_p> datarefs)
{
  unsigned idx = loop_depth (innermost) - loop_depth (loop_nest);
  gcc_assert (idx > 0);

  /* Check if any two adjacent loops should be interchanged.  */
  for (struct loop *loop = innermost;
       loop != loop_nest; loop = loop_outer (loop), idx--)
    if (should_interchange_loops (idx, idx - 1, datarefs, 0, 0,
				  loop == innermost, false))
      return true;

  return false;
}

/* Given loop nest LOOP_NEST and data references DATAREFS, compute data
   dependences for loop interchange and store it in DDRS.  Note we compute
   dependences directly rather than call generic interface so that we can
   return on unknown dependence instantly.  */

static bool
tree_loop_interchange_compute_ddrs (vec<loop_p> loop_nest,
				    vec<data_reference_p> datarefs,
				    vec<ddr_p> *ddrs)
{
  struct data_reference *a, *b;
  struct loop *innermost = loop_nest.last ();

  for (unsigned i = 0; datarefs.iterate (i, &a); ++i)
    {
      bool a_outer_p = gimple_bb (DR_STMT (a))->loop_father != innermost;
      for (unsigned j = i + 1; datarefs.iterate (j, &b); ++j)
	if (DR_IS_WRITE (a) || DR_IS_WRITE (b))
	  {
	    bool b_outer_p = gimple_bb (DR_STMT (b))->loop_father != innermost;
	    /* Don't support multiple write references in outer loop.  */
	    if (a_outer_p && b_outer_p && DR_IS_WRITE (a) && DR_IS_WRITE (b))
	      return false;

	    ddr_p ddr = initialize_data_dependence_relation (a, b, loop_nest);
	    ddrs->safe_push (ddr);
	    compute_affine_dependence (ddr, loop_nest[0]);

	    /* Give up if ddr is unknown dependence or classic direct vector
	       is not available.  */
	    if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know
		|| (DDR_ARE_DEPENDENT (ddr) == NULL_TREE
		    && DDR_NUM_DIR_VECTS (ddr) == 0))
	      return false;

	    /* If either data references is in outer loop of nest, we require
	       no dependence here because the data reference need to be moved
	       into inner loop during interchange.  */
	    if (a_outer_p && b_outer_p
		&& operand_equal_p (DR_REF (a), DR_REF (b), 0))
	      continue;
	    if (DDR_ARE_DEPENDENT (ddr) != chrec_known
		&& (a_outer_p || b_outer_p))
	      return false;
	}
    }

  return true;
}

/* Prune DATAREFS by removing any data reference not inside of LOOP.  */

static inline void
prune_datarefs_not_in_loop (struct loop *loop, vec<data_reference_p> datarefs)
{
  unsigned i, j;
  struct data_reference *dr;

  for (i = 0, j = 0; datarefs.iterate (i, &dr); ++i)
    {
      if (flow_bb_inside_loop_p (loop, gimple_bb (DR_STMT (dr))))
	datarefs[j++] = dr;
      else
	{
	  if (dr->aux)
	    {
	      DR_ACCESS_STRIDE (dr)->release ();
	      delete (vec<tree> *) dr->aux;
	    }
	  free_data_ref (dr);
	}
    }
  datarefs.truncate (j);
}

/* Find and store data references in DATAREFS for LOOP nest.  If there's
   difficult data reference in a basic block, we shrink the loop nest to
   inner loop of that basic block's father loop.  On success, return the
   outer loop of the result loop nest.  */

static struct loop *
prepare_data_references (struct loop *loop, vec<data_reference_p> *datarefs)
{
  struct loop *loop_nest = loop;
  vec<data_reference_p> *bb_refs;
  basic_block bb, *bbs = get_loop_body_in_dom_order (loop);

  for (unsigned i = 0; i < loop->num_nodes; i++)
    bbs[i]->aux = NULL;

  /* Find data references for all basic blocks.  Shrink loop nest on difficult
     data reference.  */
  for (unsigned i = 0; loop_nest && i < loop->num_nodes; ++i)
    {
      bb = bbs[i];
      if (!flow_bb_inside_loop_p (loop_nest, bb))
	continue;

      bb_refs = new vec<data_reference_p> ();
      if (find_data_references_in_bb (loop, bb, bb_refs) == chrec_dont_know)
        {
	  loop_nest = bb->loop_father->inner;
	  if (loop_nest && !loop_nest->inner)
	    loop_nest = NULL;

	  free_data_refs (*bb_refs);
          delete bb_refs;
        }
      else if (bb_refs->is_empty ())
	delete bb_refs;
      else
	bb->aux = bb_refs;
    }

  /* Collect all data references in loop nest.  */
  for (unsigned i = 0; i < loop->num_nodes; i++)
    {
      bb = bbs[i];
      if (!bb->aux)
	continue;

      bb_refs = (vec<data_reference_p> *) bb->aux;
      if (loop_nest && flow_bb_inside_loop_p (loop_nest, bb))
	datarefs->safe_splice (*bb_refs);
      else
	free_data_refs (*bb_refs);

      delete bb_refs;
      bb->aux = NULL;
    }
  free (bbs);

  return loop_nest;
}

/* Given innermost LOOP, return true if perfect loop nest can be found and
   data dependences can be computed.  If succeed, record the perfect loop
   nest in LOOP_NEST; record all data references in DATAREFS and record all
   data dependence relations in DDRS.

   We do support a restricted form of imperfect loop nest, i.e, loop nest
   with load/store in outer loop initializing/finalizing simple reduction
   of the innermost loop.  For such outer loop reference, we require that
   it has no dependence with others sinve it will be moved to inner loop
   in interchange.  */

static bool
prepare_perfect_loop_nest (struct loop *loop, vec<loop_p> *loop_nest,
			   vec<data_reference_p> *datarefs, vec<ddr_p> *ddrs)
{
  struct loop *start_loop = NULL, *innermost = loop;
  struct loop *outermost = loops_for_fn (cfun)->tree_root;

  /* Find loop nest from the innermost loop.  The outermost is the innermost
     outer*/
  while (loop->num != 0 && loop->inner == start_loop
	 && flow_loop_nested_p (outermost, loop))
    {
      if (!proper_loop_form_for_interchange (loop, &outermost))
	break;

      start_loop = loop;
      /* If this loop has sibling loop, the father loop won't be in perfect
	 loop nest.  */
      if (loop->next != NULL)
	break;

      loop = loop_outer (loop);
    }
  if (!start_loop || !start_loop->inner)
    return false;

  /* Prepare the data reference vector for the loop nest, pruning outer
     loops we cannot handle.  */
  start_loop = prepare_data_references (start_loop, datarefs);
  if (!start_loop
      /* Check if there is no data reference.  */
      || datarefs->is_empty ()
      /* Check if there are too many of data references.  */
      || (int) datarefs->length () > MAX_DATAREFS)
    return false;

  /* Compute access strides for all data references, pruning outer
     loops we cannot analyze refs in.  */
  start_loop = compute_access_strides (start_loop, innermost, *datarefs);
  if (!start_loop)
    return false;

  /* Check if any interchange is profitable in the loop nest.  */
  if (!should_interchange_loop_nest (start_loop, innermost, *datarefs))
    return false;

  /* Check if data dependences can be computed for loop nest starting from
     start_loop.  */
  loop = start_loop;
  do {
    loop_nest->truncate (0);

    if (loop != start_loop)
      prune_datarefs_not_in_loop (start_loop, *datarefs);

    if (find_loop_nest (start_loop, loop_nest)
	&& tree_loop_interchange_compute_ddrs (*loop_nest, *datarefs, ddrs))
      {
	if (dump_file && (dump_flags & TDF_DETAILS))
	  fprintf (dump_file,
		   "\nConsider loop interchange for loop_nest<%d - %d>\n",
		   start_loop->num, innermost->num);

	if (loop != start_loop)
	  prune_access_strides_not_in_loop (start_loop, innermost, *datarefs);

	if (dump_file && (dump_flags & TDF_DETAILS))
	  dump_access_strides (*datarefs);

	return true;
      }

    free_dependence_relations (*ddrs);
    *ddrs = vNULL;
    /* Try to compute data dependences with the outermost loop stripped.  */
    loop = start_loop;
    start_loop = start_loop->inner;
  } while (start_loop && start_loop->inner);

  return false;
}

/* Main entry for loop interchange pass.  */

unsigned int
pass_linterchange::execute (function *fun)
{
  if (number_of_loops (fun) <= 2)
    return 0;

  bool changed_p = false;
  struct loop *loop;
  FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
    {
      vec<loop_p> loop_nest = vNULL;
      vec<data_reference_p> datarefs = vNULL;
      vec<ddr_p> ddrs = vNULL;
      if (prepare_perfect_loop_nest (loop, &loop_nest, &datarefs, &ddrs))
	{
	  tree_loop_interchange loop_interchange (loop_nest);
	  changed_p |= loop_interchange.interchange (datarefs, ddrs);
	}
      free_dependence_relations (ddrs);
      free_data_refs_with_aux (datarefs);
      loop_nest.release ();
    }

  return changed_p ? (TODO_update_ssa_only_virtuals) : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_linterchange (gcc::context *ctxt)
{
  return new pass_linterchange (ctxt);
}
